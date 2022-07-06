#' Author: Bruno Braga Montezano
#' Subject: Predição de prejuízo funcional em sujeitos com transtornos de humor

# R version 4.1 or superior should be installed

# Load libraries ---------------------------------------------------------------

library(magrittr)
library(dplyr)
library(haven)
library(purrr)
library(gridExtra)

# Import -----------------------------------------------------------------------

raw <- haven::read_sav("data/coorte-t1-t2-24-08-17.sav")



# Define who has current disorder ----------------------------------------------
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
  #haven::as_factor() |>
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
  mutate_at(vars(mini_a08ps_t2, mini_a08atpp_t2, 
                 mini_a09ps_t2, mini_a10ps_t2,
                 mini_d06ps_t2, mini_d07ps_t2), haven::as_factor) |>
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



# Tidy -------------------------------------------------------------------------

subsample_with_outcome2 <- subsample_with_outcome %>%
  janitor::clean_names() %>%
  mutate_at(vars(
    a20medicpsi_t2, a30interp_t2, mini_m11b4_t2, mini_m13b2_t2,
    mini_e06_t2, somabdi_t1, pancfo_t1
  ), ~ as.numeric(as.character(.x)))



# Criar variáveis e recodificá-las ---------------------------------------------
subsample_with_outcome3 <- subsample_with_outcome2 %>% 
  dplyr::mutate_at(vars(assist02a_t2, assist03a_t2, assist04a_t2, assist05a_t2,
                   assist06a_t2, assist07a_t2, assist02b_t2, assist03b_t2, assist04b_t2, assist05b_t2,
                     assist06b_t2, assist07b_t2, assist02c_t2, assist03c_t2, assist04c_t2, assist05c_t2,
                     assist06c_t2, assist07c_t2, assist02d1_t2, assist03d1_t2, assist04d1_t2, assist05d1_t2,
                     assist06d1_t2, assist07d1_t2, assist02e_t2, assist03e_t2, assist04e_t2, assist05e_t2,
                     assist06e_t2, assist07e_t2, assist02f_t2, assist03f_t2, assist04f_t2, assist05f_t2,
                     assist06f_t2, assist07f_t2, assist02g_t2, assist03g_t2, assist04g_t2, assist05g_t2,
                     assist06g_t2, assist07g_t2, assist02h_t2, assist03h_t2, assist04h_t2, assist05h_t2,
                     assist06h_t2, assist07h_t2, assist02i_t2, assist03i_t2, assist04i_t2, assist05i_t2,
                     assist06i_t2, assist07i_t2), as.numeric) # Create ASSIST risk scores

subsample_with_outcome3 %>% select(assist02a_t2, assist03a_t2, assist04a_t2, assist05a_t2,
                        assist06a_t2, assist07a_t2, assist02b_t2, assist03b_t2, assist04b_t2, assist05b_t2,
                        assist06b_t2, assist07b_t2, assist02c_t2, assist03c_t2, assist04c_t2, assist05c_t2,
                        assist06c_t2, assist07c_t2, assist02d1_t2, assist03d1_t2, assist04d1_t2, assist05d1_t2,
                        assist06d1_t2, assist07d1_t2, assist02e_t2, assist03e_t2, assist04e_t2, assist05e_t2,
                        assist06e_t2, assist07e_t2, assist02f_t2, assist03f_t2, assist04f_t2, assist05f_t2,
                        assist06f_t2, assist07f_t2, assist02g_t2, assist03g_t2, assist04g_t2, assist05g_t2,
                        assist06g_t2, assist07g_t2, assist02h_t2, assist03h_t2, assist04h_t2, assist05h_t2,
                        assist06h_t2, assist07h_t2, assist02i_t2, assist03i_t2, assist04i_t2, assist05i_t2,
                        assist06i_t2, assist07i_t2) %>% dplyr::rowwise() %>% dplyr::transmute(tabaco = sum(assist02a_t2, assist03a_t2, assist04a_t2, assist05a_t2,
                                                                                  assist06a_t2, assist07a_t2))


ds <- subsample_with_outcome3 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(hcl_total = sum(c(
    hcl31_t1, hcl32_t1, hcl33_t1,
    hcl34_t1, hcl35_t1, hcl36_t1,
    hcl37_t1, hcl38_t1, hcl39_t1,
    hcl310_t1, hcl311_t1, hcl312_t1,
    hcl313_t1, hcl314_t1, hcl315_t1,
    hcl316_t1, hcl317_t1, hcl318_t1,
    hcl319_t1, hcl320_t1, hcl321_t1,
    hcl322_t1, hcl323_t1, hcl324_t1,
    hcl325_t1, hcl326_t1, hcl327_t1,
    hcl328_t1, hcl329_t1, hcl330_t1,
    hcl331_t1, hcl332_t1
  ))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), ~ dplyr::na_if(.x, 9))) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), as.numeric)) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), ~ .x + 1)) %>%
  #dplyr::mutate(dplyr::across(c(
  #  "ctq05_t2", "ctq07_t2", "ctq13_t2", "ctq19_t2",
  #  "ctq28_t2", "ctq02_t2", "ctq26_t2"
  # ), ~ 6 - .x)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    abuso_emocional = sum(c(ctq03_t2, ctq08_t2, ctq14_t2, ctq18_t2, ctq25_t2)),
    abuso_fisico = sum(c(ctq09_t2, ctq11_t2, ctq12_t2, ctq15_t2, ctq17_t2)),
    abuso_sexual = sum(c(ctq20_t2, ctq21_t2, ctq23_t2, ctq24_t2, ctq27_t2)),
    neg_emocional = sum(c(ctq05_t2, ctq07_t2, ctq13_t2, ctq19_t2, ctq28_t2)),
    neg_fisica = sum(c(ctq01_t2, ctq02_t2, ctq04_t2, ctq06_t2, ctq26_t2)),
    ctq_total = sum(c(ctq01_t2, ctq02_t2, ctq03_t2, ctq04_t2, ctq05_t2, ctq06_t2))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    briga_t1 = dplyr::case_when(
      agress_t1 == 1 | is.na(agress_t1) ~ 0,
      agress_t1 == 2 | agress_t1 == 3 ~ 1
    ),
    pais_faleceu = dplyr::case_when(fmae_t1 == 1 | fpai_t1 == 1 ~ 1, TRUE ~ 0),
    familiar_tb = dplyr::case_when(mini_a13_t2 == 1 ~ 1, TRUE ~ 0)
  ) %>%
  dplyr::mutate(dplyr::across(
    c("bsi1_t1", "bsi2_t1", "bsi4_t1", "bsi5_t1"),
    \(x) dplyr::case_when(
      x == 0 ~ 0,
      x == 1 ~ 1,
      x == 2 ~ 1
    )
  )) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("assist01"), \(x) dplyr::case_when(
    x == 0 | x == 9 | is.na(x) ~ 0,
    x == 1 ~ 1
  ))) %>%
  dplyr::mutate(
    pais_internados = dplyr::case_when(b04interna1_t2 == 1 |
                                         b11interna2_t2 == 1 ~ 1, TRUE ~ 0),
    pais_tentativa = dplyr::case_when(b06tentsu1_t2 == 1 | b13tentsu2_t2 == 1 ~ 1, TRUE ~ 0),
    pais_medicacao = dplyr::case_when(b03med1_t2 == 1 | b10med2_t2 == 1 ~ 1, TRUE ~ 0),
    pais_doencapsi = dplyr::case_when(b01famil1_t2 == 1 | b08famil2_t2 == 1 ~ 1, TRUE ~ 0),
    irmaos_doencapsi = dplyr::case_when(b15famil3_t2 == 1 ~ 1, TRUE ~ 0),
    medpsi = dplyr::case_when(a20medicpsi_t2 == 1 ~ 1, TRUE ~ 0),
    internacao_vida = dplyr::case_when(a30interp_t2 == 1 ~ 1, TRUE ~ 0),
    transtorno_psicotico = dplyr::case_when(mini_m11b4_t2 == 1 | mini_m13a2_t2 == 1 |
                                              mini_m13b2_t2 == 1 ~ 1, TRUE ~ 0),
    panico_lifetime = dplyr::case_when(mini_e06_t2 == 1 ~ 1, TRUE ~ 0),
    bdi_severo = dplyr::case_when(somabdi_t1 >= 29 ~ 1, TRUE ~ 0),
    panico_atual = dplyr::case_when(pancfo_t1 == 1 | pansfo_t1 == 1 ~ 1, TRUE ~ 0)
  ) %>% dplyr::rowwise() %>% dplyr::mutate( # Create ASSIST risk scores
    tabaco = sum(assist02a_t2 + assist03a_t2 + assist04a_t2 + assist05a_t2 +
                   assist06a_t2 + assist07a_t2),
    alcool = sum(assist02b_t2 + assist03b_t2 + assist04b_t2 + assist05b_t2 +
                   assist06b_t2 + assist07b_t2),
    maconha = sum(assist02c_t2 + assist03c_t2 + assist04c_t2 + assist05c_t2 +
                    assist06c_t2 + assist07c_t2),
    cocaina = sum(assist02d1_t2 + assist03d1_t2 + assist04d1_t2 + assist05d1_t2 +
                    assist06d1_t2 + assist07d1_t2),
    anfetamina = sum(assist02e_t2 + assist03e_t2 + assist04e_t2 + assist05e_t2 +
                       assist06e_t2 + assist07e_t2),
    inalantes = sum(assist02f_t2 + assist03f_t2 + assist04f_t2 + assist05f_t2 +
                      assist06f_t2 + assist07f_t2),
    sedativos = sum(assist02g_t2 + assist03g_t2 + assist04g_t2 + assist05g_t2 +
                      assist06g_t2 + assist07g_t2),
    alucinogenos = sum(assist02h_t2 + assist03h_t2 + assist04h_t2 + assist05h_t2 +
                         assist06h_t2 + assist07h_t2),
    opioides = sum(assist02i_t2 + assist03i_t2 + assist04i_t2 + assist05i_t2 +
                     assist06i_t2 + assist07i_t2)
  ) %>% 
  dplyr::select( outcome,
    # Sociodemográficas e clínicas
    cpele_dic_t1, sexo_t1, idade_t1_2, abep3_t1, estano_t1, religdic_t1,
    idtrab_t1, escol_t1, pais_internados, pais_tentativa, pais_medicacao,
    pais_doencapsi, irmaos_doencapsi, medpsi, internacao_vida,
    trabdin_t1, trabatu_t1, apoio_t1, apisepa_t1, grupeli_t1,
    trat_t1, interr_t1, idadrog_t1, forcsex_t1, parceiro_t1,
    briga_t1, pais_faleceu, aldtenta_t1, algmata_t1, familiar_tb,
    # uso de substancias
    tabaco, alcool, maconha, cocaina, anfetamina, inalantes, sedativos, alucinogenos, opioides,
    # dplyr::starts_with("assist") & dplyr::ends_with("t2"),
    # transtornos psiquiátricos
    edmat_t1, edmmel_t1, distat_t1, maniahipo_t1, agoraat_t1, panico_atual,
    fobsoa_t1, tocat_t1, teptat_t1, tagat_t1, panico_lifetime, transtorno_psicotico,
    # Itens da SRQ
    srq3_t1, srq11_t1, srq17_t1,
    # Escores totais de instrumentos
    somabdi_t1, somasrq_t1, hcl_total,
    # Domínios da CTQ
    abuso_emocional, abuso_fisico, abuso_sexual, neg_emocional, neg_fisica,
    # Itens da BSI
    bsi1_t1, bsi2_t1, bsi4_t1, bsi5_t1
  ) %>%
  dplyr::mutate(dplyr::across(c(
    everything(), -idade_t1_2, -idadrog_t1,
    -somabdi_t1, -somasrq_t1, -hcl_total, -abuso_emocional, -abuso_fisico, -abuso_sexual,
    -neg_emocional, -neg_fisica, -idtrab_t1
  ), ~ factor(.x))) %>%
  dplyr::mutate(dplyr::across(c(interr_t1, trabatu_t1), ~ tidyr::replace_na(.x, as.factor(0))))


ds <- ds %>% dplyr::mutate(dplyr::across(c(tabaco, maconha, cocaina,
                  anfetamina, inalantes, sedativos, alucinogenos, opioides, alcool), ~as.numeric(as.character(.x))))



ds <- ds %>% dplyr::mutate(dplyr::across(c(alcool), ~as.factor(ifelse(.x >= 11, "moderate_risk", "low_risk")))) %>% 
  mutate(dplyr::across(c(tabaco, maconha, cocaina, 
                         anfetamina, inalantes, sedativos, 
                         alucinogenos, opioides), 
                       ~as.factor(ifelse(.x >= 4, "moderate_risk", "low_risk"))))

drugs_df <- ds %>% dplyr::select(tabaco, maconha, cocaina,
                                 anfetamina, inalantes, sedativos, alucinogenos, opioides, alcool)

ds$any_ilicit_drug <- apply(ds %>% select(maconha, cocaina, 
                    anfetamina, inalantes, sedativos, 
                    alucinogenos, opioides), 1, function(x){any(x == "moderate_risk")})

ds$any_ilicit_drug <- as.factor(ifelse(ds$any_ilicit_drug, "Yes", "No"))


ds$alcool_ou_tabaco <- apply(ds %>% select(alcool, tabaco), 1, function(x){any(x == "moderate_risk")})
ds$alcool_ou_tabaco <- as.factor(ifelse(ds$alcool_ou_tabaco, "Yes", "No"))

ds <- ds %>% select(-c(maconha, cocaina, 
                    anfetamina, inalantes, sedativos, 
                    alucinogenos, opioides, alcool, tabaco))




# Rotular a variável de desfecho -----------------------------------------------

summary(ds)


dt2 <- ds %>% mutate_if(is.factor, function(x){levels(x) <- make.names(levels(x)); x})
head(dt2)
str(dt2)

dt2$escol_t1 <- relevel(dt2$escol_t1, ref = "X1")
dt2$abep3_t1 <- relevel(dt2$abep3_t1, ref = "X1")
dt2$pais_medicacao <- relevel(dt2$pais_medicacao, ref = "X0")
dt2$pais_doencapsi <- relevel(dt2$pais_doencapsi, ref = "X0")
dt2$medpsi <- relevel(dt2$medpsi, ref = "X0")
dt2$trabdin_t1 <- relevel(dt2$trabdin_t1, ref = "X0")
dt2$apoio_t1 <- relevel(dt2$apoio_t1, ref = "X0")
dt2$trat_t1 <- relevel(dt2$trat_t1, ref = "X0")
dt2$interr_t1 <- relevel(dt2$interr_t1, ref = "X0")
dt2$aldtenta_t1 <- relevel(dt2$aldtenta_t1, ref = "X0")
dt2$familiar_tb <- relevel(dt2$familiar_tb, ref = "X0")
dt2$maniahipo_t1 <- relevel(dt2$maniahipo_t1, ref = "X0")
dt2$tagat_t1 <- relevel(dt2$tagat_t1, ref = "X0")
dt2$srq11_t1 <- relevel(dt2$srq11_t1, ref = "X0")

ds <- dt2 |> dplyr::filter(!is.na(outcome))


# Dar uma olhada nos dados
dplyr::glimpse(ds)

dim(ds)

# Checar variáveis categóricas
ds %>%
  purrr::keep(is.factor) %>%
  summary()

# Checar variáveis numéricas
ds %>%
  purrr::keep(is.numeric) %>%
  summary()

# Fixing idadrog_t1
#cond_idadrog <- ifelse(ds$any_ilicit_drug == "No" & ds$alcool_ou_tabaco == "No", TRUE, FALSE)
#ds$idadrog_t1[cond_idadrog] <- 0


# Export data ------------------------------------------------------------------
#load("sessions/preprocessing.RData")
save.image("session/preprocessing.RData")

saveRDS(ds,
        file = "cache/dataset.rds")

write.csv(ds, file = "cache/ds_with_missings.csv")
