#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Elaboração de funções definidas pelo usuário


# Calculate ROC

calculateROC <- function(my_model) {
  
  df <- my_model %>% collect_predictions()
  
  refs <- df$outcome
  preds <- df$.pred_Yes
  
  roc1 <- pROC::roc(refs, preds)
  ci_auc <- round(as.numeric(pROC::ci.auc(roc1)), 2)
  
  auc_ci <- data.frame(
    AUC = ci_auc[2],
    lowerCI = ci_auc[1],
    upperCI = ci_auc[3]
  )
  
  auc_ci
  
}


get_perf_by_cutoff <- function(cutoff, mdl){
  
  df <- mdl %>% collect_predictions()
  
  obs <- df$outcome
  preds <- ifelse(df$.pred_Yes >= cutoff, "Yes", "No")
  
  pred_df <- data.frame(outcome = factor(obs, levels = c("No", "Yes")), 
                        .pred_class = factor(preds, levels = c("No", "Yes")))
  
  
  data.frame(cutoff = cutoff,
             bal_accuracy = yardstick::bal_accuracy(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate,
             sens = yardstick::sensitivity(pred_df, truth = outcome, estimate = .pred_class, event_level = "second" )$.estimate,
             spe = yardstick::specificity(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate,
             ppv = yardstick::ppv(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate,
             npv = yardstick::npv(pred_df, truth = outcome, estimate = .pred_class, event_level = "second")$.estimate)
  
  
}



