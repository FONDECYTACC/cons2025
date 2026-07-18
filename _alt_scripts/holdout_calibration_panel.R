# =============================================================================
# holdout_calibration_panel.R
#
# Table and N-row figure helpers for comparing calibration across the
# readmission arms built in point 3 (net risk, joint CIF x updated2, joint
# CIF x rule2) alongside the two death models. Consumes the output shape
# already produced by calibrate_death_holdout() / .holdout_calibrate_readmit_
# from_raw() (both in validate_holdout_metrics.R / holdout_cif_cache.R):
# a list with $pooled_summary (one row per horizon: ici_mean, ece_mean,
# eo_mean, mean_pred, observed, ...) and $calibration_curves (one row per
# horizon x decile bin: mean_predicted, observed, observed_lower/upper).
#
# Does not modify any existing calibration plotting code in the notebook
# (cal_curve_plot(), .make_cal_panel(), assemble_cal_curves()) -- those stay
# 2-row (readmission + one death model at a time); this adds a standalone
# N-row alternative, same "new function, don't touch the shared one"
# decision already applied to the C-index dispatcher and the DCA panel.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Combined ICI/ECE/E:O table across an arbitrary number of calibration
#    objects. Mirrors the notebook's existing show_cal() helper, generalized
#    to N models instead of a hardcoded 3 (readmit/death_bp1/death_bp2).
# -----------------------------------------------------------------------------
.holdout_calibration_summary_table <- function(cal_list, cols = c(
    "time_months", "mean_pred", "observed", "ici_mean", "ici_p025", "ici_p975",
    "ece_mean", "eo_mean")) {
  stopifnot(is.list(cal_list), !is.null(names(cal_list)))
  rows <- lapply(names(cal_list), function(nm) {
    d <- cal_list[[nm]]$pooled_summary
    use_cols <- intersect(cols, names(d))
    out <- d[, use_cols, drop = FALSE]
    out$model <- nm
    out
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[, c("model", setdiff(names(out), "model"))]
}

# -----------------------------------------------------------------------------
# 2. N-row calibration curve figure: predicted vs observed, faceted by
#    horizon (columns) and arm (rows), with the observed-side ribbon
#    (observed_lower/upper from cross-imputation spread) and a 45-degree
#    reference line.
# -----------------------------------------------------------------------------
.holdout_calibration_curve_plot_n <- function(
    cal_list,
    row_labels   = NULL,          # named vector keyed by names(cal_list); default = names(cal_list)
    horizons     = NULL,          # NULL = all horizons present in the data
    lang         = c("en", "es"),
    tnr          = "Times New Roman",
    x_lab        = NULL,
    y_lab        = NULL
) {
  lang <- match.arg(lang)
  stopifnot(is.list(cal_list), length(cal_list) >= 1L, !is.null(names(cal_list)))
  if (is.null(row_labels)) row_labels <- stats::setNames(names(cal_list), names(cal_list))
  stopifnot(all(names(cal_list) %in% names(row_labels)))

  rows <- lapply(names(cal_list), function(nm) {
    d <- cal_list[[nm]]$calibration_curves
    stopifnot(!is.null(d))
    if (!is.null(horizons)) d <- d[d$time_months %in% horizons, , drop = FALSE]
    d$.row <- row_labels[[nm]]
    d
  })
  plot_df <- do.call(rbind, rows)
  stopifnot(nrow(plot_df) > 0L)
  plot_df$.row <- factor(plot_df$.row, levels = unname(row_labels[names(cal_list)]))
  month_word <- if (identical(lang, "es")) "meses" else "months"
  hz <- sort(unique(plot_df$time_months))
  plot_df$horizon_lab <- factor(sprintf("%s %s", plot_df$time_months, month_word),
                                sprintf("%s %s", hz, month_word))

  if (is.null(x_lab)) x_lab <- if (identical(lang, "es")) "Riesgo predicho medio" else "Mean predicted risk"
  if (is.null(y_lab)) y_lab <- if (identical(lang, "es")) "Riesgo observado" else "Observed risk"

  has_band <- all(c("observed_lower", "observed_upper") %in% names(plot_df)) &&
    all(is.finite(plot_df$observed_lower)) && all(is.finite(plot_df$observed_upper))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = mean_predicted, y = observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50")
  if (has_band) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = observed_lower, ymax = observed_upper), width = 0, alpha = 0.5
    )
  }
  p <- p +
    ggplot2::geom_point(ggplot2::aes(size = n_patients), alpha = 0.8) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::facet_grid(rows = ggplot2::vars(.row), cols = ggplot2::vars(horizon_lab)) +
    ggplot2::labs(x = x_lab, y = y_lab, size = if (identical(lang, "es")) "n pacientes" else "n patients") +
    ggplot2::theme_bw(base_family = tnr) +
    ggplot2::theme(legend.position = "bottom", strip.background = ggplot2::element_rect(fill = "grey95"))
  p
}
