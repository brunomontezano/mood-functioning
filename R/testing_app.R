# Testing web app
#load("session/evaluate_model_subsamples.RData")


library(dplyr)
library(tidymodels)
model_vars <- final_model %>% 
  extract_fit_parsnip() %>% 
  vip::vip(num_features = 25)

w <- which(preds_current$.pred_Yes < 0.5)

selected_examples <- current_test[w, ] %>% 
  select(model_vars$data$Variable)


df <- data.frame(selected_examples, .pred_Yes = preds_current$.pred_Yes[w])

df <- df %>% select(abep3_t1, estano_t1, religdic_t1, 
                   escol_t1, pais_doencapsi, medpsi, 
                   internacao_vida, trabatu_t1, trat_t1, 
                   interr_t1, pais_faleceu, aldtenta_t1, 
                   distat_t1, agoraat_t1, fobsoa_t1, fobsoa_t1, 
                   panico_lifetime, srq3_t1, somabdi_t1, somasrq_t1, 
                   abuso_emocional, abuso_fisico, abuso_sexual, 
                   neg_emocional, neg_fisica,any_ilicit_drug,
                   .pred_Yes
)

data.frame(colnames(df), exemplo1 = as.data.frame(df[2, ]))

t(as.data.frame(df[2, ]))
           