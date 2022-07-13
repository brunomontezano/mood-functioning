# Evaluation of the best models

#load("analysis_with_rfe_tidy_060722.RData")

load("analysis_with_rfe_tidy_090722b.RData")

source("R/functions.R")

aucs_train

set.seed(1)



#log_best <- tune::select_best(log_rs, "roc_auc")
rf_best <- tune::select_best(rf_rs, "roc_auc")


# Coeficients LASSO ----

log_coefs <- log_wf %>% 
  finalize_workflow(log_best) %>%
  fit(mood_train) %>%
  extract_fit_parsnip() %>%
  tidy()


log_coefs %>% filter(estimate != 0) %>% arrange(estimate) %>% View()

# Best workflow and best model ----

best_wf <- rf_wf
best_model <- rf_best


# Final workflow ---------------------------------------------------------------

final_wf <- 
  best_wf %>% 
  finalize_workflow(best_model)



# Final fit --------------------------------------------------------------------

final_wf

final_fit <- 
  final_wf %>%
  last_fit(mood_split) 


final_fit %>%
  collect_metrics()

final_fit %>% 
  collect_predictions() %>% 
  conf_mat(truth = outcome, estimate = .pred_class)



# Performance metrics ----

perf_cutoff_res <- map(seq(0.1, 0.9, by = 0.1), mdl = final_fit, get_perf_by_cutoff) %>% bind_rows()
perf_cutoff_res <- round(perf_cutoff_res, 2)
perf_cutoff_res


#write.csv(perf_cutoff_res, "rf_perf_cutoff_res.csv")

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


final_fit %>%
  collect_predictions() %>% 
  roc_curve(event_level = "second", truth = outcome, .pred_Yes)


# Plot ROC 

final_fit %>%
  collect_predictions() %>% 
  roc_curve(event_level = "second", truth = outcome, .pred_Yes) %>% 
  ggplot(
    aes(
      x = 1 - specificity, 
      y = sensitivity
    )
  ) + # plot with 2 ROC curves for each model
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, size = 0.4) +
  scale_color_manual(values = c("#48466D", "#3D84A8")) +
  coord_fixed() +
  cowplot::theme_cowplot() +
  annotate(geom = "text", 
           x = 0.75, 
           y = 0.25,
           size = 5,
           label = test_auc_str, 
           color = "#333333")


final_fit %>%
  collect_predictions()


final_model <- extract_workflow(final_fit)
final_model

final_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip(num_features = 25)





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
test_auc_other_disorders <- calculateROC2(other_set, final_model)
test_auc_healthy <- calculateROC2(healthy_test, final_model)

test_auc_current
test_auc_other_disorders
test_auc_healthy

preds_current <- predict(final_model, current_test, type = "prob")

preds_other_disorders <- predict(final_model, other_set, type = "prob")

preds_healthy <- predict(final_model, healthy_test, type = "prob")

summary(preds_current$.pred_Yes)
summary(preds_other_disorders$.pred_Yes)
summary(preds_healthy$.pred_Yes)
