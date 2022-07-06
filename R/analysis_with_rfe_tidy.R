# Ler dados brutos

# Read data ----
sub_with_features <- readRDS("cache/dataset.rds")


source("R/step_rfe.R")

library(tidymodels)

set.seed(1)

# Split into training and test sets
mood_split <- rsample::initial_split(data = sub_with_features,
                                     strata = "outcome",
                                     prop = 0.7)

# Create the objects

mood_train <- rsample::training(mood_split)
mood_test <- rsample::testing(mood_split)


# Check grouped outcome distribution by past episode
#mood_train |> 
 # dplyr::group_by(past_episode) |> 
  #dplyr::count(outcome)

# Create k-fold cv object
folds <- rsample::vfold_cv(data = mood_train,
                           v = 5,
                           repeats = 5,
                           strata = "outcome")

folds

# Create the tune grid
log_grid <- dials::grid_regular(
  dials::penalty(),
  levels = 10
)

# Specify logistic regression model (LASSO)
log_spec <- parsnip::logistic_reg(mixture = 1, penalty = tune::tune()) |> 
  parsnip::set_engine("glmnet") |> 
  parsnip::set_mode("classification")

# Create recipe for LASSO model
log_rec <- recipes::recipe(outcome ~ ., data = mood_train) |> 
  #recipes::update_role(rec, new_role = "id") |> 
  #recipes::update_role(past_episode, new_role = "episode") |> 
  recipes::step_normalize(recipes::all_numeric_predictors()) |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  #recipes::step_impute_median(recipes::all_numeric_predictors()) |> 
  #recipes::step_impute_mode(recipes::all_nominal_predictors()) |> 
  recipes::step_impute_bag(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors()) |> 
  recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE) |>
  themis::step_rose(outcome)
#|>
  #step_rfe_lm(all_predictors(), outcome = "outcome")
  #recipeselectors::step_select_roc(recipes::all_predictors(),
  #                                 outcome = "outcome",
  #                                 top_p = 10)


# Specify random forest model
rf_spec <- parsnip::rand_forest(trees = 1000,
                                min_n = tune::tune(),
                                mtry = tune::tune()) |> 
  parsnip::set_engine("ranger", importance = "impurity") |> 
  parsnip::set_mode("classification")

# Create recipe for the random forest model
rf_rec <- recipes::recipe(outcome ~ ., data = mood_train) |> 
  #recipes::update_role(rec, new_role = "id") |> 
  #recipes::update_role(past_episode, new_role = "episode") |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  #recipes::step_impute_median(recipes::all_numeric_predictors()) |> 
  #recipes::step_impute_mode(recipes::all_nominal_predictors()) |> 
  recipes::step_impute_bag(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors()) |>
  themis::step_rose(outcome) |>
  step_rfe(all_predictors(), outcome = "outcome")
  #recipeselectors::step_select_forests(recipes::all_predictors(),
  #                                     trees = 1000,
  #                                     mtry = round(sqrt(ncol(mood_train |> 
  #                                                         dplyr::select(
  #                                                           -rec,
  #                                                           -outcome,
  #                                                           -past_episode
  #                                                         )))),
  #                                     min_n = 0.1,
  #                                     outcome = "outcome",
  #                                     top_p = 10)
  
# Check how preprocessing for the logistic regression model goes
log_processed <- log_rec |> 
  recipes::prep() |> 
  recipes::bake(new_data = NULL)

log_processed


# Check how preprocessing for the random forests model goes
rf_processed <- rf_rec |> 
  recipes::prep() |> 
  recipes::bake(new_data = NULL)


# Create workflow for LASSO regression
log_wf <- workflows::workflow() |> 
  workflows::add_recipe(log_rec) |> 
  workflows::add_model(log_spec)

# Create random forest workflow
rf_wf <- workflows::workflow() |> 
  workflows::add_recipe(rf_rec) |> 
  workflows::add_model(rf_spec)

# Fit LASSO resamples
log_rs <- tune::tune_grid(
  object = log_wf,
  resamples = folds,
  grid = log_grid,
  control = tune::control_grid(
    save_pred = TRUE,
    allow_par = TRUE,
    verbose = TRUE,
    save_workflow = TRUE
  ),
  metrics = yardstick::metric_set(yardstick::roc_auc,
                                  yardstick::bal_accuracy,
                                  yardstick::ppv)
)

# Fit RF resamples
rf_rs <- tune::tune_grid(
  object = rf_wf,
  resamples = folds,
  grid = 10,
  control = tune::control_grid(
    save_pred = TRUE,
    allow_par = TRUE,
    verbose = TRUE,
    save_workflow = TRUE
  ),
  metrics = yardstick::metric_set(yardstick::roc_auc,
                                  yardstick::bal_accuracy,
                                  yardstick::ppv)
)


save.image("analysis_with_rfe_tidy_060722.RData")

# Check best RF models based on AUC
rf_rs |> 
  tune::show_best(metric = "roc_auc",
                  n = 10)

# Check best LASSO models based on AUC
log_rs |> 
  tune::show_best(metric = "roc_auc",
                  n = 10)

log_best <- tune::select_best(log_rs, "roc_auc")
rf_best <- tune::select_best(rf_rs, "roc_auc")

dt_spec <- parsnip::decision_tree(cost_complexity = tune::tune(),
                                  tree_depth = tune::tune(),
                                  min_n = tune::tune()) |> 
  parsnip::set_engine("rpart") |> 
  parsnip::set_mode("classification")

dt_wf <- workflows::workflow() |> 
  workflows::add_model(dt_spec) |> 
  workflows::add_recipe(rf_rec)

dt_rs <- tune::tune_grid(
  object = dt_wf,
  resamples = folds,
  grid = 15,
  control = tune::control_grid(
    save_pred = TRUE,
    allow_par = TRUE,
    verbose = TRUE,
    save_workflow = TRUE
  ),
  metrics = yardstick::metric_set(yardstick::roc_auc,
                                  yardstick::bal_accuracy,
                                  yardstick::ppv)
)

dt_rs |> 
  tune::show_best()
