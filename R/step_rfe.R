# Tutorial
# https://www.tidymodels.org/learn/develop/recipes/

# step_percentile() calls recipes::add_step()
#  └──> recipes::add_step() calls step_percentile_new()
#   └──> step_percentile_new() calls recipes::step()

step_rfe <- function(
    recipe, 
    ...,
    outcome = NULL,
    role = "predictor", 
    trained = FALSE, 
    #options = list(probs = (0:100)/100, names = TRUE),
    skip = FALSE,
    id = rand_id("rfe"),
    exclude = NULL
) {
  
  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures 
  ##  the values and also checks to make sure that they are not empty.  
  terms <- ellipse_check(...) 
  
  add_step(
    recipe, # obrigatorio
    step_rfe_new( # obrigatorio
      terms = terms, # obrigatorio
      trained = trained, # obrigatorio
      outcome = outcome,
      role = role, 
      exclude = exclude,
      #options = options,
      skip = skip,
      id = id
    )
  )
}

step_rfe_new <- 
  function(terms, role, trained, outcome, skip, id, exclude) {
      
    step(
      subclass = "rfe", 
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      #options = options,
      skip = skip,
      id = id,
      exclude = exclude
    )
  }

get_profile <- function(x, y){
  
  set.seed(1)
  
  ctrl_rfe <- caret::rfeControl(functions = caret::rfFuncs, 
                         method = "LOOCV", verbose = FALSE)
  
  rfeProfile <- caret::rfe(x = x,
                    y = y, 
                    sizes = c(10, 15, 20, 25), 
                    rfeControl = ctrl_rfe)
  
  print(length(rfeProfile$optVariables))
  
  if(length(rfeProfile$optVariables) > 25){
    selectedVars <- rfeProfile$variables
    bestVar <- rfeProfile$control$functions$selectVar(selectedVars, 25)
    print("Greater than 25")
    
  } else {
    bestVar <- rfeProfile$optVariables
    print("Less than or equal to 25")
    
  }
  
  
  tibble(x %>% select(all_of(bestVar)), y)
  
  
}


prep.step_rfe <- function(x, training, info = NULL, ...) {
  
  # translate the terms arguments
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  #y_name <- y_name[1]
  
  
  if (length(x_names) > 0) {
    # fit initial model
    X <- training[, x_names]
    y <- training[[y_name]]
    
    selected_df <- get_profile(X, y)
    selected_vars <- colnames(selected_df %>% select(-y))
    print(length(selected_vars))
    
    exclude <- colnames(X)[!colnames(X) %in% selected_vars]
    exclude
    
  } else {
    exclude <- character()
  }
  
  step_rfe_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    exclude = exclude,
    #scores = res,
    skip = x$skip,
    id = x$id
  )
}



bake.step_rfe <- function(object, new_data, ...) {
  
  
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  
  as_tibble(new_data)
  
  
}





