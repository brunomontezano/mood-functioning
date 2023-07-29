# R version 4.1 or superior should be installed

# Load libraries ---------------------------------------------------------------

library(magrittr)
library(dplyr)
library(haven)
library(purrr)
library(gridExtra)

# Import -----------------------------------------------------------------------

raw <- haven::read_sav("data/coorte-t1-t2-24-08-17.sav")
raw$ID <- 1:nrow(raw)


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


subsample_current <- raw |>
  dplyr::filter(
    # Participou do follow-up
    # Participated in follow-up
    perdas == 1 & (
      # Não possui episódio depressivo atual
      # Do not have current depressive episode
      miniA08AT_t2 == 1 |
        # Não possui episódio depressivo atípico atual
        # Do not have current atypical depressive episode
        miniA08ATPA_t2 == 1 |
        # Nem ep. depressivo por condição médica atual
        # Do not have current depressive episode from medical conditions
        miniA09AT_t2 == 1 |
        # Nem ep. depressivo induzido por substância atual
        # Do not have current depressive episode induced from substance
        miniA10AT_t2 == 1 |
        # Nem ep. depressivo melancólico atual
        # Do not have current melancholic depressive episode
        miniA15b_t2 == 1 |
        # Nem distimia atual
        # Do not have current dysthymia
        miniBA05e_t2 == 1 |
        # Nem hipomania atual
        # Do not have current hypomanic episode
        miniD06AT_t2 == 1 |
        # Nem mania atual
        # Do not have current manic episode
        miniD07AT_t2 == 1 )
    #&
    # Possui história de humor:
    # Em ordem, ep. depressivo passado, ep. depressivo atípico passado,
    # ep. depressivo por condição médica passado e ep. depressivo
    # induzido por substância passado, hipomania passada, mania passada
    # Participant has mood episode history:
    # In order: past depressive episode, past atypical depressive episode,
    # past depressive episode from medical conditions, past depressive
    # episode from substance use, past hypomanic episode, past manic
    # episode
    #(
    #  miniA08PS_t2 == 1 | miniA08ATPP_t2 == 1 | miniA09PS_t2 == 1 |
    #    miniA10PS_t2 == 1 | miniD06PS_t2 == 1 | miniD07PS_t2 == 1
    #)
  )



# Filter to get desired sample
subsample_other_disorders <- raw |>
  
  # Limpar nomes de variáveis
  # Improve variable names
  janitor::clean_names() |>
  
  dplyr::filter(
    # Participou do follow-up
    # Participated in follow-up
    perdas == 1 & (
      teptat_t1 == 1 |
        tocat_t1 == 1 |
        tagat_t1 == 1 |
        pancfo_t1 == 1 |
        pansfo_t1 == 1 |
        agoraat_t1 == 1 |
        fobsoa_t1 == 1
    ) & (
      edmat_t1 == 0 &
        edmmel_t1 == 0 &
        maniahipo_t1 == 0
    ) & (
      mini_a08ps_t2    == 0 & 
        mini_a08atpp_t2  == 0 & 
        mini_a09ps_t2    == 0 & 
        mini_a10ps_t2    == 0 &
        mini_d06ps_t2    == 0 &
        mini_d07ps_t2    == 0 
      
    )
  )



# Filter to get desired sample
subsample_healthy <- raw |>
  
  # Limpar nomes de variáveis
  # Improve variable names
  janitor::clean_names() |>
  
  dplyr::filter(
    # Participou do follow-up
    # Participated in follow-up
    perdas == 1 & (
      teptat_t1  == 0 &
        tocat_t1   == 0 &
        tagat_t1   == 0 &
        pancfo_t1  == 0 &
        pansfo_t1  == 0 &
        agoraat_t1 == 0 &
        fobsoa_t1  == 0
    ) & (
      edmat_t1     == 0 &
        edmmel_t1    == 0 &
        maniahipo_t1 == 0
    ) & (
      mini_a08ps_t2    == 0 & 
        mini_a08atpp_t2  == 0 & 
        mini_a09ps_t2    == 0 & 
        mini_a10ps_t2    == 0 &
        mini_d06ps_t2    == 0 &
        mini_d07ps_t2    == 0 
      
    )
  )


subsamples_ids <- list(
  original = subsample$ID,
  current = subsample_current$ID,
  other_disorders = subsample_other_disorders$id,
  healthy = subsample_healthy$id)

reduce(subsamples_ids,
       dplyr::intersect)


#devtools::install_github("yanlinlin82/ggvenn")

library(ggvenn)
ggvenn(
  subsamples_ids, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
  stroke_size = 0.5, set_name_size = 4
)



# sobreposicao entre, current e other, healthy e current
