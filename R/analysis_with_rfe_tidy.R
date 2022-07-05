set.seed(1)
# Ler dados brutos
# Read raw data
raw <- haven::read_spss("data/coorte-t1-t2-24-08-17.sav")

source("R/step_rfe.R")

library(tidymodels)

# Filter to get desired sample
subsample <- raw |>
  dplyr::filter(
    # Participou do follow-up
    # Participated in follow-up
    perdas == 1 &
      # Não possui episódio depressivo atual
      # Do not have current depressive episode
      miniA08AT_t2 == 0 &
      # Não possui episódio depressivo atípico atual
      # Do not have current atypical depressive episode
      miniA08ATPA_t2 == 0 &
      # Nem ep. depressivo por condição médica atual
      # Do not have current depressive episode from medical conditions
      miniA09AT_t2 == 0 &
      # Nem ep. depressivo induzido por substância atual
      # Do not have current depressive episode induced from substance
      miniA10AT_t2 == 0 &
      # Nem ep. depressivo melancólico atual
      # Do not have current melancholic depressive episode
      miniA15b_t2 == 0 &
      # Nem distimia atual
      # Do not have current dysthymia
      miniBA05e_t2 == 0 &
      # Nem hipomania atual
      # Do not have current hypomanic episode
      miniD06AT_t2 == 0 &
      # Nem mania atual
      # Do not have current manic episode
      miniD07AT_t2 == 0 &
      # Possui história de humor:
      # Em ordem, ep. depressivo passado, ep. depressivo atípico passado,
      # ep. depressivo por condição médica passado e ep. depressivo
      # induzido por substância passado, hipomania passada, mania passada
      # Participant has mood episode history:
      # In order: past depressive episode, past atypical depressive episode,
      # past depressive episode from medical conditions, past depressive
      # episode from substance use, past hypomanic episode, past manic
      # episode
      (
        miniA08PS_t2 == 1 | miniA08ATPP_t2 == 1 | miniA09PS_t2 == 1 |
          miniA10PS_t2 == 1 | miniD06PS_t2 == 1 | miniD07PS_t2 == 1
      )
  )

subsample_with_outcome <- subsample |>
  # Transformar labels do SPSS em fator
  # Transform SPSS labels in factor levels
  haven::as_factor() |>
  # Limpar nomes de variáveis
  # Improve variable names
  janitor::clean_names() |>
  # Criar desfecho de disfuncionalidade
  # Create outcome based on FAST score
  dplyr::mutate(outcome = relevel(as.factor(
    dplyr::if_else(fast_sum_t2 > 11, "Yes", "No")
  ), ref = "No"))

# Check how the outcome is distributed
# Looks like we have 89 subjects with impairment
# 193 without significant impairment
# And one subject with missing value in the outcome

subsample_with_outcome |>
  dplyr::count(outcome)

subsample_with_outcome <- subsample_with_outcome |>
  dplyr::filter(!is.na(outcome))

# Add variable with follow-up past mood episodes
subsample_with_outcome <- subsample_with_outcome |>
  dplyr::mutate(
    past_depressed = dplyr::if_else(
      mini_a08ps_t2 == "sim" | mini_a08atpp_t2 == "sim" |
        mini_a09ps_t2 == "sim" | mini_a10ps_t2 == "sim",
      "Yes",
      "No"
    ),
    past_hypo_or_mania = dplyr::if_else(mini_d06ps_t2 == 1 |
                                        mini_d07ps_t2 == 1,
                                        "Yes", "No"),
    past_episode = dplyr::case_when(
      past_depressed == "Yes" &
        past_hypo_or_mania == "Yes" ~ "Both episodes",
      past_depressed == "No" &
        past_hypo_or_mania == "Yes" ~ "Hypomanic or manic",
      past_depressed == "Yes" &
        past_hypo_or_mania == "No" ~ "Depressive",
    )
  )


# How the past episodes are distributed in the follow-up
subsample_with_outcome |>
  dplyr::count(past_episode)

# What about grouped the outcome grouped by these episodes?
subsample_with_outcome |>
  dplyr::group_by(past_episode) |>
  dplyr::count(outcome)

# Select variables and process some
sub_with_features <- subsample_with_outcome |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"),
                              \(x) as.numeric(x))) |> 
  dplyr::rowwise() |>
  dplyr::mutate(
    abuso_emocional = sum(c(
      ctq03_t2, ctq08_t2, ctq14_t2, ctq18_t2, ctq25_t2
    )),
    abuso_fisico = sum(c(
      ctq09_t2, ctq11_t2, ctq12_t2, ctq15_t2, ctq17_t2
    )),
    abuso_sexual = sum(c(
      ctq20_t2, ctq21_t2, ctq23_t2, ctq24_t2, ctq27_t2
    )),
    neg_emocional = sum(c(
      ctq05_t2, ctq07_t2, ctq13_t2, ctq19_t2, ctq28_t2
    )),
    neg_fisica = sum(c(
      ctq01_t2, ctq02_t2, ctq04_t2, ctq06_t2, ctq26_t2
    ))
  ) |>
  dplyr::ungroup() |>
  #dplyr::mutate(
  #  abuso_emocional = as.factor(dplyr::if_else(abuso_emocional >= 10,
  #                                             "Yes", "No")),
  #  abuso_fisico = as.factor(dplyr::if_else(abuso_fisico >= 8, "Yes", "No")),
  #  abuso_sexual = as.factor(dplyr::if_else(abuso_sexual >= 8, "Yes", "No")),
  #  neg_emocional = as.factor(dplyr::if_else(neg_emocional >= 15,
  #                                           "Yes", "No")),
  #  neg_fisica = as.factor(dplyr::if_else(neg_fisica >= 8, "Yes", "No"))
  #) |> 
  dplyr::mutate(
    religiaodic_t1 = as.factor(dplyr::if_else(religiaodic_t1 == 1, "sim", "não")),
    forcsex_t1 = as.factor(dplyr::if_else(
      forcsex_t1 == "sim",
      "sim",
      dplyr::if_else(forcsex_t1 == "não", "não", NA_character_)
    )),
    vezgravi_t1 = as.factor(dplyr::if_else(
      vezgravi_t1 == "nenhuma",
      "não",
      dplyr::if_else(vezgravi_t1 %in% c("1 vez", "2 vezes ou mais"),
                     "sim", NA_character_)
    )),
    interr_t1 = as.factor(dplyr::if_else(interr_t1 == "sim",
                                      "sim",
                                      "não",
                                      missing = "não"))
  ) |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("hcl3"),
                              \(x) as.factor(
                                dplyr::case_when(x %in% c("0", "não") ~ "não",
                                                 x == "sim" ~ "sim",
                                                 TRUE ~ "não")
                              ))) |> 
  dplyr::mutate(dplyr::across(
    c(
      tabaco2_t1,
      alcool2_t1,
      maconha2_t1,
      cocaina2_t1,
      crack2_t1,
      anfeta2_t1,
      inalante2_t1,
      sedativos2_t1,
      alucinog2_t1,
      opioides2_t1,
      outrasub2_t1,
      ilicitas2_t1
    ),
    \(x) as.factor(dplyr::if_else(x == "sem uso / uso ocasional",
                                  "não", "sim"))
  )) |> 
  dplyr::select(
    rec,
    outcome,
    past_episode,
    edmat_t1,
    edmmel_t1,
    edmps_t1,
    distat_t1,
    maniahipo_t1,
    pansfo_t1,
    pancfo_t1,
    agoraat_t1,
    fobsoa_t1,
    tocat_t1,
    teptat_t1,
    tagat_t1,
    idade_t1_2,
    sexo_t1,
    parceiro_t1,
    trabatu_t1,
    trabdin_t1,
    estano_t1,
    abep3_t1,
    dplyr::starts_with("bdi"),
    dplyr::starts_with("qlusou"),
    alcool2_t1,
    tabaco2_t1,
    maconha2_t1,
    alucinog2_t1,
    opioides2_t1,
    cocaina2_t1,
    crack2_t1,
    anfeta2_t1,
    inalante2_t1,
    sedativos2_t1,
    alucinog2_t1,
    outrasub2_t1,
    ilicitas2_t1,
    hospner_t1,
    cons_t1,
    trat_t1,
    forcsex_t1,
    e5panvi_t1,
    e7sint_t1,
    dplyr::starts_with("bsi"),
    dplyr::starts_with("srq"),
    dplyr::starts_with("hcl3"),
    dplyr::starts_with("abuso"),
    dplyr::starts_with("neg_"),
    religiaodic_t1,
    medic_t1,
    idadrog_t1,
    anorat_t1,
    buliat_t1,
    sexobb_t1,
    algmata_t1,
    aldtenta_t1,
    interr_t1,
    prfalec_t1,
    apisepa_t1,
    apoio_t1,
    rsat_t1,
    cpele_dic_t1,
    vezgravi_t1,
    totbsi_t1,
    totbdi_t1,
    somasrq20_t1,
    srq20dic_t1
  ) |> 
  dplyr::mutate(
    edmat = as.factor(dplyr::if_else(
      edmat_t1 == "sim" | edmmel_t1 == "sim",
    "sim", "não")),
    talim = as.factor(dplyr::if_else(
      anorat_t1 == "sim" | buliat_t1 == "sim",
      "sim", "não"
    )),
    dplyr::across(dplyr::starts_with("bdi"),
                  \(x) as.factor(dplyr::case_when(
                    x %in% 1:3 ~ "sim",
                    x == 0 ~ "não"
                  ))),
    .keep = "unused"
  )



sub_with_features$maniahipo_t1 <- forcats::fct_recode(
  as.factor(
    sub_with_features$maniahipo_t1
    ),
  nao = "0",
  sim = "1")


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
                           repeats = 10,
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
  recipes::update_role(rec, new_role = "id") |> 
  #recipes::update_role(past_episode, new_role = "episode") |> 
  recipes::step_normalize(recipes::all_numeric_predictors()) |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  recipes::step_impute_median(recipes::all_numeric_predictors()) |> 
  recipes::step_impute_mode(recipes::all_nominal_predictors()) |> 
  recipes::step_nzv(recipes::all_predictors()) |> 
  recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE) |>
  step_rfe(all_predictors(), outcome = "outcome")
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
  recipes::update_role(rec, new_role = "id") |> 
  #recipes::update_role(past_episode, new_role = "episode") |> 
  recipes::step_filter_missing(recipes::all_predictors(), threshold = 0.1) |> 
  recipes::step_impute_median(recipes::all_numeric_predictors()) |> 
  recipes::step_impute_mode(recipes::all_nominal_predictors()) |> 
  recipes::step_nzv(recipes::all_predictors()) |>
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

# Check how preprocessing for the logistic regression model goes
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
