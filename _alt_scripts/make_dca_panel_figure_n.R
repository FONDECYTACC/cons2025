# =============================================================================
# make_dca_panel_figure_n.R
#
# N-row DCA panel figure (any number of arms, not fixed at 2 like the
# shared A/B figures), built standalone for the 2026-07-15 review session's
# 3-arm readmission comparison (net risk, joint CIF x updated2 death, joint
# CIF x rule2 death). Does NOT modify make_dca_panel_figure() /
# make_dca_panel_figure_std() / _abc() (dca_from_results_boot_for_metrics.R
# / make_dca_panel_figure_std.R), which are hardcoded to 2 rows (A/B) and
# used elsewhere in the pipeline (see point 1's dispatcher decision, same
# "don't touch the shared function" reasoning applied here).
#
# More customizable than the 2-row versions on purpose (explicit request,
# 2026-07-15): accepts an arbitrary-length NAMED list of `adca_out$summary`
# data.frames (one per arm/row, from run_dca_full_summary()'s output), free
# row labels, a raw-vs-standardized net-benefit switch, and the same
# lang/font/limits controls as make_dca_panel_figure_std().
#
# Usage:
#   dca_3arm &lt;- run_dca_full_summary(
#     list(net_risk = results_boot_val_bp1, cif_bp1 = inj_A, cif_bp2 = inj_B),
#     horizons = DCA_HORIZONS)
#   make_dca_panel_figure_n(
#     list(net_risk = dca_3arm$net_risk$summary,
#          cif_bp1  = dca_3arm$cif_bp1$summary,
#          cif_bp2  = dca_3arm$cif_bp2$summary),
#     outcome = "readmission", horizons = c(6, 12, 36, 60),
#     row_labels = c(net_risk = "Net risk (1-S(t))",
#                    cif_bp1  = "Joint CIF, death = updated2",
#                    cif_bp2  = "Joint CIF, death = rule2"))
# =============================================================================

make_dca_panel_figure_n <- function(
    summaries,                        # named list of adca_out$summary data.frames, one per row
    outcome         = c("readmission", "death"),
    horizons        = c(6, 12, 36, 60),
    standardized    = TRUE,           # TRUE: standardized_nb_mean; FALSE: raw net_benefit_mean
    row_labels      = NULL,           # named vector keyed by names(summaries); default = names(summaries)
    x_limits        = NULL,           # NULL = ggplot's own data-driven range
    y_limits        = NULL,           # NULL: c(-0.2, 1.03) if standardized, else data-driven
    lang            = c("en", "es"),
    tnr             = "Times New Roman",
    x_lab           = NULL,
    y_lab           = NULL,
    strategy_labels = NULL,           # named vector keyed by Model / Treat all / Treat none
    emit_caption    = FALSE
) {
  outcome <- match.arg(outcome)
  lang <- match.arg(lang)
  stopifnot(is.list(summaries), length(summaries) >= 1L, !is.null(names(summaries)))
  if (is.null(row_labels)) row_labels <- stats::setNames(names(summaries), names(summaries))
  stopifnot(all(names(summaries) %in% names(row_labels)))

  nb_col <- if (isTRUE(standardized)) "standardized_nb_mean" else "net_benefit_mean"
  lo_col <- if (isTRUE(standardized)) "standardized_nb_q025" else "net_benefit_q025"
  hi_col <- if (isTRUE(standardized)) "standardized_nb_q975" else "net_benefit_q975"

  strategy_col <- NULL
  rows <- lapply(names(summaries), function(nm) {
    d <- summaries[[nm]]
    if (is.null(strategy_col)) {
      strategy_col <<- if ("strategy" %in% names(d)) "strategy" else "Strategy"
    }
    d <- d[d$risk == outcome & d$horizon %in% horizons, , drop = FALSE]
    d$.row <- row_labels[[nm]]
    d$.nb <- d[[nb_col]]
    d$.lo <- if (lo_col %in% names(d)) d[[lo_col]] else NA_real_
    d$.hi <- if (hi_col %in% names(d)) d[[hi_col]] else NA_real_
    d
  })
  plot_df <- do.call(rbind, rows)
  stopifnot(nrow(plot_df) > 0L)
  plot_df$.row <- factor(plot_df$.row, levels = unname(row_labels[names(summaries)]))
  month_word <- if (identical(lang, "es")) "meses" else "months"
  horizon_levels <- sprintf("%s %s", sort(unique(horizons)), month_word)
  plot_df$horizon_lab <- factor(sprintf("%s %s", plot_df$horizon, month_word), levels = horizon_levels)

  default_strategy_labels <- if (identical(lang, "es")) {
    c(Model = "Modelo", `Treat all` = "Tratar a todos", `Treat none` = "Tratar a ninguno")
  } else {
    c(Model = "Model", `Treat all` = "Treat all", `Treat none` = "Treat none")
  }
  if (is.null(strategy_labels)) strategy_labels <- default_strategy_labels
  known <- names(strategy_labels)[names(strategy_labels) %in% unique(plot_df[[strategy_col]])]
  if (length(known)) plot_df[[strategy_col]] <- factor(
    ifelse(plot_df[[strategy_col]] %in% known, strategy_labels[plot_df[[strategy_col]]], plot_df[[strategy_col]])
  )

  if (is.null(x_lab)) x_lab <- if (identical(lang, "es")) "Umbral de riesgo" else "Risk threshold"
  if (is.null(y_lab)) {
    y_lab <- if (isTRUE(standardized)) {
      if (identical(lang, "es")) "Beneficio neto estandarizado" else "Standardized net benefit"
    } else {
      if (identical(lang, "es")) "Beneficio neto" else "Net benefit"
    }
  }
  if (is.null(y_limits)) y_limits <- if (isTRUE(standardized)) c(-0.2, 1.03) else NULL

  has_band <- all(is.finite(plot_df$.lo)) && all(is.finite(plot_df$.hi))
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = threshold, y = .nb, color = .data[[strategy_col]]))
  if (has_band) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .lo, ymax = .hi, fill = .data[[strategy_col]]),
      alpha = 0.15, color = NA
    )
  }
  p <- p +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::facet_grid(rows = ggplot2::vars(.row), cols = ggplot2::vars(horizon_lab)) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits) +
    ggplot2::labs(x = x_lab, y = y_lab, color = NULL, fill = NULL) +
    ggplot2::theme_bw(base_family = tnr) +
    ggplot2::theme(legend.position = "bottom", strip.background = ggplot2::element_rect(fill = "grey95"))

  if (isTRUE(emit_caption)) {
    attr(p, "caption") <- sprintf(
      if (identical(lang, "es")) "Curvas de decisión (%s) por brazo de riesgo predicho: %s."
      else "Decision curves (%s) by predicted-risk arm: %s.",
      outcome, paste(names(row_labels), collapse = ", ")
    )
  }
  p
}
