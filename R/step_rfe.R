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
  
  ctrl_rfe <- rfeControl(functions = rfFuncs, 
                         method = "LOOCV", verbose = FALSE)
  
  rfeProfile <- rfe(x = x,
                    y = y, 
                    sizes = c(25), 
                    rfeControl = ctrl_rfe)
  
  #rfeProfile
  #rfeProfile$optVariables
  
  selectedVars <- rfeProfile$variables
  bestVar <- rfeProfile$control$functions$selectVar(selectedVars, 25)
  
  tibble(x %>% select(all_of(bestVar)), y)
  
  
}


prep.step_rfe <- function(x, training, info = NULL, ...) {
  
  # translate the terms arguments
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  print(x_names)
  print(y_name)
  #y_name <- y_name[1]
  
  
  if (length(x_names) > 0) {
    # fit initial model
    X <- training[, x_names]
    y <- training[[y_name]]
    
    selected_df <- get_profile(X, y)
    selected_vars <- colnames(selected_df %>% select(-y))
    
    exclude <- colnames(X)[!colnames(X) %in% selected_vars]
    print(exclude)
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


#get_profile(biomass_tr %>% select(-outcome), 
 #           biomass_tr$outcome)


library(caret)

bake.step_rfe <- function(object, new_data, ...) {
  
  
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  
  as_tibble(new_data)
  
  
}





