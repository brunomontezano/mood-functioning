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

table(current_test$outcome)
table(other_test$outcome)
table(healthy_test$outcome)



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


# Load function ----

get_perf_by_cutoff2 <- function(cutoff, mdl, new_data){
  
  pred_probs <- predict(mdl, new_data, type = "prob")
  
  preds <- factor(ifelse(pred_probs$.pred_Yes >= cutoff, "Yes", "No"),
                  levels = c("No", "Yes"))
  
  
  
  pred_df <- data.frame(outcome = relevel(new_data$outcome, ref = "No"), 
                        .pred_class = relevel(preds, ref = "No"))
  
  print(list(
    levels(pred_df$outcome),
    levels(pred_df$.pred_class),
    table(pred_df$outcome)))
  
  
  data.frame(cutoff = cutoff,
             bal_accuracy = yardstick::bal_accuracy(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate,
             sens = yardstick::sensitivity(pred_df, truth = outcome, estimate = .pred_class, event_level = "second" )$.estimate,
             spe = yardstick::specificity(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate,
             ppv = yardstick::ppv(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate,
             npv = yardstick::npv(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate)
  
  
}



# Performance current episode (follow) ----

table(current_test$outcome)

perf_cutoff_res_current <- map(seq(0.1, 0.9, by = 0.1), 
                       mdl = final_model,
                       new_data = current_test,
                       get_perf_by_cutoff2) %>% 
  bind_rows() %>% round(2)


perf_cutoff_res_current


# Performance participants with other disorders ----

perf_cutoff_res_other <- map(seq(0.1, 0.9, by = 0.1), 
                               mdl = final_model,
                               new_data = other_test,
                               get_perf_by_cutoff2) %>% 
  bind_rows() %>% round(2)


perf_cutoff_res_other


perf_cutoff_res_healthy <- map(seq(0.1, 0.9, by = 0.1), 
                               mdl = final_model,
                               new_data = healthy_test,
                               get_perf_by_cutoff2) %>% 
  bind_rows() %>% round(2)


perf_cutoff_res_healthy


#write.csv(perf_cutoff_res_current, file = "performance_by_cutoff_current.csv")
#write.csv(perf_cutoff_res_other, file = "performance_by_cutoff_other_disorders.csv")
#write.csv(perf_cutoff_res_healthy, file = "performance_by_cutoff_healthy.csv")



#load("session/evaluate_model_subsamples.RData")

#save.image("session/evaluate_model_subsamples.RData")
