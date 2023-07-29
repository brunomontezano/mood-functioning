# Ler dados brutos

library(tidymodels)

# Read data ----
sub_with_features <- readRDS("cache/dataset.rds")
sub_with_features$rec <- 1:nrow(sub_with_features)

sub_with_features <- sub_with_features %>%
  relocate(rec, .before = outcome)

dim(sub_with_features)

source("R/step_rfe.R")

library(tidymodels)

set.seed(1)

# Split into training and test sets
mood_split <- rsample::initial_split(data = sub_with_features,
                                     strata = "outcome",
                                     prop = 0.5)

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

folds2 <- rsample::vfold_cv(data = mood_train,
                           v = 10,
                           repeats = 5,
                           strata = "outcome")

folds2


# Specify random forest model
rf_spec <- parsnip::rand_forest(trees = 1000,
                                min_n = tune::tune(),
                                mtry = tune::tune()) |> 
  parsnip::set_engine("ranger", importance = "impurity") |> 
  parsnip::set_mode("classification")

# Create recipe for the random forest model
rf_rec <- recipes::recipe(outcome ~ ., data = mood_train) |> 
  recipes::update_role(rec, new_role = "id") |>
  recipes::step_nzv(recipes::all_predictors()) |>
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  recipes::step_impute_bag(recipes::all_predictors()) |>
  themis::step_rose(outcome, seed = 1) |>
  step_rfe(all_predictors(), outcome = "outcome")
  

# Check how preprocessing for the random forests model goes
rf_processed <- rf_rec |> 
  recipes::prep() |> 
  recipes::bake(new_data = NULL)


# Create random forest workflow
rf_wf <- workflows::workflow() |> 
  workflows::add_recipe(rf_rec) |> 
  workflows::add_model(rf_spec)


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

set.seed(1)

rf_rs2 <- tune::tune_grid(
  object = rf_wf,
  resamples = folds2,
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

rf_rs2 |> 
  tune::show_best(metric = "roc_auc",
                  n = 10)


save.image("suppl_main_model_5050.RData")

