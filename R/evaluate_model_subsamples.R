# Test set with current episode ----
library(tibble)
library(tidymodels)
source("R/step_rfe.R")


final_model <- readRDS(file = "cache/main_final_model.rds")
current_test <- readRDS(file = "cache/dataset_with_current_episode.rds")
other_test <- readRDS(file = "cache/dataset_with_other_disorders.rds")
healthy_test <- readRDS(file = "cache/dataset_healthy.rds")

nrow(current_test)
nrow(other_test)
nrow(healthy_test)


calculateROC2 <- function(new_test_set, my_model) {
  
  preds <- predict(my_model, new_test_set, type = "prob")
  preds
  
  pred_df <- data.frame(outcome = relevel(new_test_set$outcome, ref = "No"), 
                        .pred_class = preds$.pred_Yes)
  
  
  refs <- pred_df$outcome
  preds <- pred_df$.pred_class
  
  roc1 <- pROC::roc(refs, preds)
  ci_auc <- round(as.numeric(pROC::ci.auc(roc1)), 2)
  
  auc_ci <- data.frame(
    AUC = ci_auc[2],
    lowerCI = ci_auc[1],
    upperCI = ci_auc[3]
  )
  
  auc_ci
  
}


test_auc_current <- calculateROC2(current_test, final_model)
test_auc_other_disorders <- calculateROC2(other_test, final_model)
test_auc_healthy <- calculateROC2(healthy_test, final_model)

test_auc_current
test_auc_other_disorders
test_auc_healthy

preds_current <- predict(final_model, current_test, type = "prob")

preds_other_disorders <- predict(final_model, other_test, type = "prob")

preds_healthy <- predict(final_model, healthy_test, type = "prob")

summary(preds_current$.pred_Yes)
summary(preds_other_disorders$.pred_Yes)
summary(preds_healthy$.pred_Yes)

hist(preds_current$.pred_Yes)
hist(preds_other_disorders$.pred_Yes)
hist(preds_healthy$.pred_Yes)

save.image("session/evaluate_model_subsamples.R")
