# Ler dados brutos

library(tidymodels)
library(caret)



# Read data ----
sub_with_features <- readRDS("cache/dataset.rds")
dim(sub_with_features)

source("R/functions.R")
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


# Specify random forest model
rf_spec <- parsnip::rand_forest(trees = 1000,
                                min_n = tune::tune(),
                                mtry = tune::tune()) |> 
  parsnip::set_engine("ranger", importance = "impurity") |> 
  parsnip::set_mode("classification")

# Create recipe for the random forest model
rf_rec <- recipes::recipe(outcome ~ ., data = mood_train) |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  recipes::step_impute_bag(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors()) |>
  themis::step_rose(outcome, seed = 1) 
  #step_rfe(all_predictors(), outcome = "outcome", skip = TRUE)



# Check how preprocessing for the random forests model goes

rf_processed <- rf_rec |> 
  recipes::prep() |> 
  recipes::bake(new_data = sub_with_features)


# Get imputed dataset
imputed_rec_with_nzv <- recipes::recipe(outcome ~ ., data = mood_train) |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  recipes::step_impute_bag(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors())

imputed_prepped_with_nzv <- recipes::prep(imputed_rec_with_nzv, mood_train) 

imputed_baked_with_nzv <- recipes::bake(imputed_prepped_with_nzv, new_data = sub_with_features)

dim(imputed_baked_with_nzv)
colnames(imputed_baked_with_nzv)

# Get imputed dataset without nzv
imputed_rec <- recipes::recipe(outcome ~ ., data = mood_train) |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  recipes::step_impute_bag(recipes::all_predictors())

imputed_prepped <- recipes::prep(imputed_rec, mood_train) 
imputed_baked <- recipes::bake(imputed_prepped, new_data = sub_with_features)

colnames(imputed_baked)

dim(imputed_baked)
dim(sub_with_features)


dim(sub_with_features)
dim(imputed_baked)
dim(imputed_baked_with_nzv)

missing_vars <- colnames(sub_with_features)[!colnames(sub_with_features) %in% colnames(imputed_baked)]
nzv_vars <- colnames(sub_with_features)[!colnames(sub_with_features) %in% colnames(imputed_baked_with_nzv)]
nzv_vars <- nzv_vars[!nzv_vars %in% missing_vars]

length(missing_vars)
length(nzv_vars)

missing_vars
nzv_vars

sub_with_features |> select(all_of(nzv_vars)) |> summary()




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



# Check best RF models based on AUC
rf_rs |> 
  tune::show_best(metric = "roc_auc",
                  n = 10)

best_model <- tune::select_best(rf_rs, "roc_auc")


final_wf <- 
  rf_wf %>% 
  finalize_workflow(best_model)



# Final fit --------------------------------------------------------------------

final_fit <- 
  final_wf %>%
  last_fit(mood_split) 


roc_ci <- calculateROC(final_fit)
roc_ci


final_result <- final_fit %>% collect_metrics()
final_result

test_auc <- round(final_result$.estimate[2], 2)
test_auc

test_auc_str <- paste0("Test AUC: \n", 
                       test_auc,
                       " (", 
                       roc_ci[2], 
                       " - ", 
                       roc_ci[3],
                       ")")
test_auc_str


#load("session/suppl_main_model_without_rfe_23072022.RData")
#save.image("session/suppl_main_model_without_rfe_23072022.RData")
save.image("session/suppl_main_model_without_rfe_04092022.RData")

