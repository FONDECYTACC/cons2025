# Combined threshold-metric figure (clean / Epidemiology style)
#
# Single figure stacking both outcomes:
#   A = Readmission, B = Death  (each: model rows x metric columns)
# Horizon legend is collected once at the bottom. Each outcome keeps its OWN
# x-axis, because the threshold grids differ (readmission 5-35% vs death 1-7%),
# so they cannot share a common scale.
#
# Requires plot_threshold_metrics_clean() to be sourced first.
#
# Usage:
#   source(file.path(project_root, "cons/_alt_scripts/plot_threshold_metrics_clean.R"))
#   source(file.path(project_root, "cons/_alt_scripts/plot_threshold_metrics_clean_combined.R"))
#   fig <- plot_threshold_metrics_clean_combined(
#     summary_long = threshold_out_bestperf12$summary_long,
#     panel_models = c("SHAP primary", "SHAP implemented"),
#     panel_titles = c("best_perf", "best_perf2"))
#   print(fig$plot)

`%||%` <- function(x, y) if (is.null(x)) y else x

#' @inheritParams plot_threshold_metrics_clean
#' @param outcome_titles optional named vector mapping outcome -> display title.
#' @return list(plot = <patchwork>, outcomes = list(combined = list(plot = ...)),
#'              parts = <per-outcome plots>). The `outcomes` slot makes it
#'         directly usable by save_threshold_metric_figures().
plot_threshold_metrics_clean_combined <- function(
    summary_long,
    horizons     = c(6, 12, 36, 60),
    panel_models = c("Full PH updated2", "SHAP primary"),
    panel_titles = c("Full PH", "SHAP primary"),
    metrics      = c("NPV", "PPV", "Sens", "Spec"),
    outcomes     = c("Readmission", "Death"),
    base_family  = "Times New Roman",
    uncertainty  = c("ribbon", "errorbar", "none"),
    outcome_titles = NULL) {

  if (!exists("plot_threshold_metrics_clean", mode = "function"))
    stop("Source plot_threshold_metrics_clean.R before this script.", call. = FALSE)
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package `patchwork` is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package `ggplot2` is required.", call. = FALSE)

  uncertainty    <- match.arg(uncertainty)
  outcome_titles <- outcome_titles %||% setNames(outcomes, outcomes)

  base <- plot_threshold_metrics_clean(
    summary_long = summary_long, horizons = horizons,
    panel_models = panel_models, panel_titles = panel_titles,
    metrics = metrics, outcomes = outcomes,
    base_family = base_family, uncertainty = uncertainty,
    save_files = FALSE)

  # keep, in requested order, only outcomes that produced a plot
  have <- outcomes[vapply(outcomes,
                          function(o) !is.null(base$outcomes[[o]]$plot),
                          logical(1))]
  if (!length(have)) stop("No outcome plots were produced.", call. = FALSE)

  subplots <- lapply(seq_along(have), function(i) {
    o <- have[i]
    p <- base$outcomes[[o]]$plot +
      ggplot2::labs(title = outcome_titles[[o]] %||% o) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0))
    if (i < length(have))                     # x-axis title only on the last row
      p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    p
  })

  combined <- patchwork::wrap_plots(subplots, ncol = 1, guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(
      legend.position      = "bottom",
      legend.justification = "center",
      plot.tag             = ggplot2::element_text(face = "bold", size = 14,
                                                   family = base_family))

  list(
    plot     = combined,
    outcomes = list(combined = list(plot = combined)),  # for save_*_figures()
    parts    = base$outcomes
  )
}
