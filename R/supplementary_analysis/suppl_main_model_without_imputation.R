# Ler dados brutos

library(tidymodels)
library(caret)

table(sub_with_features$outcome)



# Read data ----
sub_with_features <- readRDS("cache/dataset.rds")
dim(sub_with_features)


source("R/step_rfe.R")
source("R/functions.R")

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
  recipes::step_normalize(recipes::all_numeric_predictors()) |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0) |> 
  #recipes::step_impute_bag(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors()) |> 
  themis::step_rose(outcome, seed = 1) |>
  recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE)


# Specify random forest model
rf_spec <- parsnip::rand_forest(trees = 1000,
                                min_n = tune::tune(),
                                mtry = tune::tune()) |> 
  parsnip::set_engine("ranger", importance = "impurity") |> 
  parsnip::set_mode("classification")

# Create recipe for the random forest model
rf_rec <- recipes::recipe(outcome ~ ., data = mood_train) |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0) |> 
  #recipes::step_impute_bag(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors()) |>
  themis::step_rose(outcome, seed = 1) |>
  step_rfe(all_predictors(), outcome = "outcome")




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
set.seed(1)

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




# Check best RF models based on AUC
rf_rs |> 
  tune::show_best(metric = "roc_auc",
                  n = 10)




# Check best LASSO models based on AUC
log_rs |> 
  tune::show_best(metric = "roc_auc",
                  n = 10)



# Test AUC ----

# Random forests 
best_model_rf <- tune::select_best(rf_rs, "roc_auc")


final_wf_rf <- 
  rf_wf %>% 
  finalize_workflow(best_model_rf)

final_fit_rf <- 
  final_wf_rf %>%
  last_fit(mood_split) 


roc_ci_rf <- calculateROC(final_fit_rf)
roc_ci_rf


final_result_rf <- final_fit_rf %>% collect_metrics()
final_result_rf

test_auc_rf <- round(final_result_rf$.estimate[2], 3)
test_auc_rf

test_auc_str_rf <- paste0("Test AUC: \n", 
                       test_auc_rf,
                       " (", 
                       roc_ci_rf[2], 
                       " - ", 
                       roc_ci_rf[3],
                       ")")
test_auc_str_rf


# LASSO
best_model_ls <- tune::select_best(log_rs, "roc_auc")


final_wf_ls <- 
  log_wf %>% 
  finalize_workflow(best_model_ls)

final_fit_ls <- 
  final_wf_ls %>%
  last_fit(mood_split) 


roc_ci_ls <- calculateROC(final_fit_ls)
roc_ci_ls


final_result_ls <- final_fit_ls %>% collect_metrics()
final_result_ls

test_auc_ls <- round(final_result_ls$.estimate[2], 3)
test_auc_ls

test_auc_str_ls <- paste0("Test AUC: \n", 
                          test_auc_ls,
                          " (", 
                          roc_ci_ls[2], 
                          " - ", 
                          roc_ci_ls[3],
                          ")")
test_auc_str_ls


#load("suppl_without_imputation.RData")
save.image("suppl_without_imputation_04092022.RData")

