# =============================================================================
# cal_panel_epi_functions.R
#
# Epidemiology-style calibration panel helpers: `create_cal_published()` (one
# horizon: calibration curve + marginal density strips on the right and bottom)
# and `.make_cal_panel()` (a 4-horizon row built from it).
#
# PROVENANCE (2026-07-16): these two functions previously lived ONLY inside the
# `holdout-cal-fig-functions` cell of `cons/prediction23_converted_mod.ipynb`.
# That cell was lost while editing the notebook, which broke every figure cell
# that calls `.make_cal_panel()` ("could not find function"). The code below is
# a VERBATIM copy of that cell's definitions (read from the notebook earlier the
# same session), moved here so a notebook edit can no longer delete them: the
# notebook now just source()s this file.
#
# NOTE: the lost cell ALSO did two other things that are NOT reproduced here and
# may need restoring separately in the notebook:
#   1. `tnr <- "Times New Roman"` (a global; other cells define it too, and this
#      file guards for it below, so it is usually harmless).
#   2. `source(<project_root>/cons/_alt_scripts/cal_fig_improved.R)`, which
#      provides `assemble_cal_curves()` used by the `holdout-cal-fig-improved`
#      cell. If that cell errors with "could not find function
#      assemble_cal_curves", add that source() line back.
#
# Depends: ggplot2, patchwork, scales, dplyr.
# =============================================================================

# `create_cal_published()` reads the font family from a global `tnr`, exactly as
# it did inside the notebook. Guard it so sourcing this file standalone (or in a
# fresh session where the defining cell has not run) does not fail.
if (!exists("tnr", inherits = TRUE) || !is.character(tnr) || length(tnr) != 1L) {
  tnr <- "Times New Roman"
}

create_cal_published <- function(data, time_title, color = "#2166AC",
                                 x_lim = c(0, 0.4), x_by = 0.1, y_lim = x_lim, y_by = x_by,
                                 show_x_label = FALSE, show_y_label = FALSE) {
  fixed_margin <- ggplot2::margin(5, 2, 5, 5)
  .label_fmt <- function(lim) scales::number_format(accuracy =
    ifelse(diff(lim) <= 0.02, 0.001, ifelse(diff(lim) <= 0.2, 0.01, 0.1)))
  main <- ggplot2::ggplot(data, ggplot2::aes(mean_predicted, observed)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = observed_lower, ymax = observed_upper), alpha = 0.3, fill = color) +
    ggplot2::geom_line(color = color, linewidth = 0.9) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_lower, ymax = observed_upper),
                           width = 0.02 * diff(x_lim), color = color, alpha = 0.6, linewidth = 0.5) +
    ggplot2::geom_point(size = 1, color = color, fill = "white", shape = 21, stroke = 0.8) +
    ggplot2::scale_x_continuous(limits = x_lim, expand = c(0, 0),
                                breaks = seq(x_lim[1], x_lim[2], x_by), labels = .label_fmt(x_lim)) +
    ggplot2::scale_y_continuous(limits = y_lim, expand = c(0, 0),
                                breaks = seq(y_lim[1], y_lim[2], y_by), labels = .label_fmt(y_lim)) +
    ggplot2::labs(x = "Predicted probability", y = "Observed proportion", subtitle = time_title) +
    ggplot2::theme_bw(base_size = 15, base_family = tnr) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 13, color = "black"),
      axis.title = ggplot2::element_text(size = 13, face = "bold"),
      axis.title.x = ggplot2::element_text(color = if (show_x_label) "black" else NA, size = 13, face = "bold"),
      axis.title.y = ggplot2::element_text(color = if (show_y_label) "black" else NA, size = 13, face = "bold"),
      plot.margin = fixed_margin)
  right_dens <- ggplot2::ggplot(data, ggplot2::aes(observed)) +
    ggplot2::geom_density(fill = color, alpha = 0.3, color = NA) +
    ggplot2::scale_x_continuous(limits = y_lim, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::coord_flip() + ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 5, 0))
  bottom_dens <- ggplot2::ggplot(data, ggplot2::aes(mean_predicted)) +
    ggplot2::geom_density(fill = color, alpha = 0.3, color = NA) +
    ggplot2::scale_x_continuous(limits = x_lim, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(0, 2, 0, 5))
  design <- c(patchwork::area(1, 1, 10, 10), patchwork::area(1, 11, 10, 11), patchwork::area(11, 1, 11, 10))
  main + right_dens + bottom_dens + patchwork::plot_layout(design = design)
}

.make_cal_panel <- function(cal_obj, color, x_lim, x_by, y_lim = x_lim, y_by = x_by) {
  curves <- dplyr::mutate(cal_obj$calibration_curves, time_label = paste0(time_months, " months"))
  tls <- paste0(sort(unique(curves$time_months)), " months")
  plots <- lapply(seq_along(tls), function(i)
    create_cal_published(dplyr::filter(curves, time_label == tls[i]), tls[i], color,
      x_lim, x_by, y_lim, y_by, show_x_label = (i == ceiling(length(tls) / 2)), show_y_label = (i == 1)))
  patchwork::wrap_plots(plots, ncol = 4)
}
