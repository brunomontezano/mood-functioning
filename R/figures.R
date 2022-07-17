load("session/analysis_with_rfe_tidy_090722b.RData")

# Check AUC
auc_testset <- final_model |> 
  predict(mood_test, type = "prob") |> 
  dplyr::bind_cols(mood_test |> dplyr::select(outcome)) |> 
  dplyr::mutate(.pred_class = dplyr::if_else(.pred_Yes > 0.5, "Yes", "No")) |> 
  yardstick::roc_auc(outcome, .pred_No)

# Check AUC confidence intervals
auc_ci <- final_model |> 
  predict(mood_test, type = "prob") |> 
  dplyr::bind_cols(mood_test |> dplyr::select(outcome)) |> 
  dplyr::mutate(.pred_class = dplyr::if_else(.pred_Yes > 0.5, "Yes", "No")) |> 
  pROC::roc(outcome, .pred_No) |> 
  pROC::ci.auc()

# Create plot text
test_auc_str <- paste0("Test AUC: \n", 
                       round(auc_ci[2], 3),
                       " (", 
                       round(auc_ci[1], 3), 
                       " - ", 
                       format(round(auc_ci[3], 3), nsmall = 3),
                       ")")

plot_roc <- final_model |> 
  predict(mood_test, type = "prob") |> 
  dplyr::bind_cols(mood_test |> dplyr::select(outcome)) |> 
  dplyr::mutate(.pred_class = dplyr::if_else(.pred_Yes > 0.5, "Yes", "No")) |> 
  yardstick::roc_curve(outcome, .pred_No) |> 
  ggplot2::autoplot() +
  ggplot2::labs(x = "1 - Specificity", y = "Sensitivity") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.x.bottom = ggplot2::element_text(size = 12, hjust = 1)
  ) +
  ggplot2::geom_abline(slope = 1, intercept = 0, size = 0.5) +
  ggplot2::annotate(geom = "text", 
           x = 0.65, 
           y = 0.25,
           size = 5,
           label = test_auc_str, 
           color = "#000000")

plot_roc$layers[[1]]$aes_params$colour <- "darkmagenta"
plot_roc$layers[[1]]$aes_params$size <- 1.5

plot_roc

labels <- c(
  "Knows someone who tried suicide",
  "Psychiatric hospitalization (lifetime)",
  "SRQ item 3",
  "Parental psychiatric disease",
  "Psychiatric medication use",
  "Currently working",
  "Has a religion",
  "Current dysthymia",
  "Current agoraphobia",
  "Currently studying",
  "Lifetime panic disorder",
  "Psychological or psychiatric treatment",
  "Current social phobia",
  "Education",
  "Socioeconomic status",
  "Parents passed away",
  "Illicit drug use",
  "BDI score",
  "Emotional neglect",
  "Interrupted treatment",
  "Physical abuse",
  "Emotional abuse",
  "Physical neglect",
  "SRQ score",
  "Sexual abuse"
)

imp_plot <- final_model |> 
  hardhat::extract_fit_engine() |> 
  vip::vip(num_features = 25) +
  ggplot2::scale_x_discrete(labels = labels) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.x.bottom = ggplot2::element_text(size = 12, hjust = 1)
  )

imp_plot$layers[[1]]$aes_params$fill <- "firebrick2"

imp_plot

plotar_var <- function(var) {
  final_model |>
    hardhat::extract_fit_engine() |>
    pdp::partial(train = rf_processed,
                 pred.var = var,
                 plot = TRUE,
                 alpha = 0.5,
                 smooth = TRUE,
                 plot.engine = "ggplot2")
}

plotar_var("panico_lifetime")

abuso_sexual <- plotar_var("abuso_sexual") +
  ggplot2::labs(y = "", x = "Sexual abuse") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(size = 11),
    axis.text.x.bottom = ggplot2::element_text(size = 11, hjust = 1),
    panel.border = ggplot2::element_blank()
  )

somasrq <- plotar_var("somasrq_t1") +
  ggplot2::labs(y = "", x = "SRQ score") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(size = 11),
    axis.text.x.bottom = ggplot2::element_text(size = 11, hjust = 1),
    panel.border = ggplot2::element_blank()
  )

neg_fisica <- plotar_var("neg_fisica") +
  ggplot2::labs(y = "", x = "Physical neglect") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(size = 11),
    axis.text.x.bottom = ggplot2::element_text(size = 11, hjust = 1),
    panel.border = ggplot2::element_blank()
  )

abuso_emocional <- plotar_var("abuso_emocional") +
  ggplot2::labs(y = "", x = "Emotional abuse") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(size = 11),
    axis.text.x.bottom = ggplot2::element_text(size = 11, hjust = 1),
    panel.border = ggplot2::element_blank()
  )

abuso_fisico <- plotar_var("abuso_fisico") +
  ggplot2::labs(y = "", x = "Physical abuse") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(size = 11),
    axis.text.x.bottom = ggplot2::element_text(size = 11, hjust = 1),
    panel.border = ggplot2::element_blank()
  )

library(patchwork)

(abuso_sexual + somasrq) / (neg_fisica + abuso_emocional) / abuso_fisico

grid::grid.draw(grid::textGrob(label = "Predicted outcome",
                               x = 0.02,
                               rot = 90,
                               gp = grid::gpar(col = "black", fontsize = 14)))

