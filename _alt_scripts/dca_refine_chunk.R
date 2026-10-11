#| label: dca-refine-annex
#| fig-width: 7.2
#| fig-height: 7.6
#| warning: false
#| message: false
# ============================================================================
# DCA PUBLICATION FIGURE, REFINEMENT PASS (annex to
# `holdout-dca-publication-figure-en`).
# It rebuilds the figure from the SAVED BOOTSTRAP RESULTS
# (pred23_holdout_dca_bootstrap_all_models.rds, about 112 KB), so nothing from
# the held-out pipeline has to run again. It deliberately does NOT reuse
# Figure19_DCA_bootstrap_patients_plot.rds: that object is an already-assembled
# cowplot grid, and panel spacing, panel tags and legend geometry cannot be
# retuned once a grid has been built. Re-deriving from the 112 KB table takes
# seconds and is what makes every knob below actually settable.
#
# The four refinements, point by point, each with its own knob:
#   (1) less horizontal space between the horizon panels
#   (2) A and B bolder, and closer to the panel vertically
#   (3) larger legend text
#   (4) the vertical dead space between legend and panels removed, which is
#       what pays for (3)
#
# Requires in kernel: nothing. project_root and data_out are used when present.
# Place AFTER `holdout-dca-publication-figure-en`. Writes its own files with a
# _refined suffix, so the original Figure19 exports are never overwritten.
# ============================================================================
stopifnot(requireNamespace("ggplot2", quietly = TRUE), requireNamespace("dplyr", quietly = TRUE),
          requireNamespace("patchwork", quietly = TRUE), requireNamespace("cowplot", quietly = TRUE),
          requireNamespace("scales", quietly = TRUE), requireNamespace("grid", quietly = TRUE))
# ---- (1) horizontal space between the four horizon panels -------------------
# Was 8 pt in the original theme. This is the single biggest source of wasted
# width, because it is applied three times per row.
DCA_R1_PANEL_SPACING_PT <- 3
# ---- (2) panel letters A and B ------------------------------------------------
# SIZE in pt. TOP_MARGIN_PT is the band the tag lives in: patchwork draws the
# tag in the plot's top margin, so SHRINKING that margin is what brings the
# letter closer to the panel. Keep it at or above the tag size or the letter
# starts overlapping the panel strip.
DCA_R2_TAG_SIZE <- 13
DCA_R2_TOP_MARGIN_PT <- 13
# ---- (3) and (4) legend --------------------------------------------------------
# The original legend text was 5.6 pt, well under the 8 pt floor LWW applies to
# printed artwork. KEY_H is the height of each legend key; the labels carry
# embedded line breaks, so it must stay tall enough for a three-line entry.
# REL_H is the legend band as a fraction of the figure: raising it while
# zeroing the surrounding margins is what converts dead space into type size.
# NROW matters more than any other legend knob: seven entries on ONE row cannot
# exceed about 5.6 pt inside 7.2 in without the end entries being clipped, which
# is why the original was set that small. Two rows roughly doubles the width per
# entry and is what makes a readable size possible at all.
DCA_R34_LEGEND_NROW <- 2
DCA_R34_LEGEND_TEXT_SIZE <- 7.6
DCA_R34_LEGEND_KEY_H <- 20
DCA_R34_LEGEND_KEY_W <- 16
DCA_R34_LEGEND_REL_H <- 0.17
DCA_R34_BODY_BOTTOM_MARGIN_PT <- 0
# ---- Data: kernel first, then the saved bootstrap table -----------------------
if (!exists("dca_boot_all", inherits = TRUE)) {
  .dca_root <- if (exists("project_root", inherits = TRUE)) project_root else getwd()
  .dca_cand <- c(file.path(.dca_root, "data", "20241015_out", "pred23_holdout_dca_bootstrap_all_models.rds"),
                 file.path(.dca_root, "data", "20241015_out", "pred23_holdout_validation_2026_07_17.rds"))
  .dca_path <- .dca_cand[file.exists(.dca_cand)][1]
  stopifnot("could not find the RDS with the dense DCA results" = !is.na(.dca_path))
  .dca_loaded <- readRDS(.dca_path)
  dca_boot_all <- if (is.data.frame(.dca_loaded)) .dca_loaded
    else if (is.list(.dca_loaded) && is.data.frame(.dca_loaded$results)) .dca_loaded$results
    else if (is.list(.dca_loaded) && is.data.frame(.dca_loaded$dca_boot_all)) .dca_loaded$dca_boot_all
    else stop("the selected RDS does not contain a recognizable dense DCA table", call. = FALSE)
  message("dca-refine: loaded the bootstrap results from ", basename(.dca_path),
          " (", round(file.info(.dca_path)$size / 1024), " KB); the held-out pipeline was not needed.")
}
.dca_required <- c("model_id", "model_label", "outcome", "horizon", "threshold", "in_focus",
                   "event_risk", "net_cases_per1000", "net_cases_per1000_lo", "net_cases_per1000_hi", "B_valid")
stopifnot(all(.dca_required %in% names(dca_boot_all)), all(dca_boot_all$B_valid == 1000L))
# ---- Keys, labels and aesthetics, identical to the original cell ---------------
.dca_labels <- c(death_all = "Mortality: all\npredictors", death_shap = "Mortality: SHAP-\ninformed",
                 read_netrisk = "Readmission:\nNet risk",
                 read_all = "Readmission:\nCumulative incidence,\nall predictors",
                 read_shap = "Readmission:\nCumulative incidence,\nSHAP-informed",
                 treat_all = "Reference:\nTreat all", treat_none = "Reference:\nTreat none")
.dca_model_keys <- c("death_all", "death_shap", "read_netrisk", "read_all", "read_shap")
.dca_legend_keys <- c(.dca_model_keys, "treat_all", "treat_none")
.dca_key_map <- c("Mortality Full PH" = "death_all", "Mortality SHAP-informed" = "death_shap",
                  "Readmission net risk 1-S(t)" = "read_netrisk",
                  "Readmission CIF with Full PH mortality" = "read_all",
                  "Readmission CIF with SHAP-informed mortality" = "read_shap")
.dca_colors <- c(death_all = "#24455E", death_shap = "#8A5A2B", read_netrisk = "#8A8A8A",
                 read_all = "#202020", read_shap = "#626262")
.dca_fills <- c(death_all = "#9FB9CA", death_shap = "#D5B287", read_netrisk = "#D9D9D9",
                read_all = "#D9D9D9", read_shap = "#D9D9D9")
.dca_ltys <- c(death_all = "solid", death_shap = "longdash", read_netrisk = "dotted",
               read_all = "solid", read_shap = "longdash")
.dca_leg_colors <- c(.dca_colors, treat_all = "#333333", treat_none = "#777777")
.dca_leg_ltys <- c(.dca_ltys, treat_all = "dotdash", treat_none = "solid")
.dca_pdata <- dca_boot_all |>
  dplyr::filter(in_focus) |>
  dplyr::mutate(model_key = unname(.dca_key_map[model_label]),
                horizon_label = factor(paste0(horizon, " months"),
                                       levels = paste0(c(6, 12, 36, 60), " months")))
stopifnot("an original label is not registered in the key map" = !anyNA(.dca_pdata$model_key))
.dca_pdata$model_key <- factor(.dca_pdata$model_key, levels = .dca_model_keys)
.dca_envelope <- .dca_pdata |>
  dplyr::filter(outcome == "readmission") |>
  dplyr::group_by(horizon_label, threshold) |>
  dplyr::summarise(lower = min(net_cases_per1000_lo), upper = max(net_cases_per1000_hi), .groups = "drop")
.dca_ref_specs <- .dca_pdata |>
  dplyr::group_by(outcome, horizon_label) |>
  dplyr::summarise(event_risk = dplyr::first(event_risk), threshold_min = min(threshold),
                   threshold_max = max(threshold), .groups = "drop")
.dca_treat_all <- do.call(rbind, lapply(seq_len(nrow(.dca_ref_specs)), function(i) {
  spec <- .dca_ref_specs[i, , drop = FALSE]
  tg <- seq(spec$threshold_min, spec$threshold_max, length.out = 201L)
  data.frame(outcome = spec$outcome, horizon_label = spec$horizon_label, threshold = tg,
             net_cases_per1000 = 1000 * (spec$event_risk - tg) / (1 - tg))
})) |>
  dplyr::filter(net_cases_per1000 >= 0)
.dca_pct <- function(x) {
  pct <- 100 * x
  ifelse(abs(pct - round(pct)) < 1e-8, paste0(sprintf("%.0f", pct), "%"), paste0(sprintf("%.1f", pct), "%"))
}
# ---- Theme carrying knobs (1) and (2) -----------------------------------------
.dca_theme_r <- ggplot2::theme_classic(base_size = 8.5, base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 9.5, face = "bold", hjust = 0),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 8.2, face = "bold"),
    axis.title = ggplot2::element_text(size = 8.5),
    axis.text = ggplot2::element_text(size = 7.4, color = "black"),
    axis.line = ggplot2::element_line(linewidth = 0.35, color = "black"),
    axis.ticks = ggplot2::element_line(linewidth = 0.35, color = "black"),
    panel.spacing = grid::unit(DCA_R1_PANEL_SPACING_PT, "pt"),
    legend.position = "none",
    plot.margin = ggplot2::margin(DCA_R2_TOP_MARGIN_PT, 5, 2, 5))
.dca_panel <- function(outcome_value) {
  pd <- dplyr::filter(.dca_pdata, outcome == outcome_value)
  pref <- dplyr::filter(.dca_treat_all, outcome == outcome_value) |>
    dplyr::mutate(reference_key = factor("treat_all", levels = .dca_legend_keys))
  pnone <- data.frame(yintercept = 0, reference_key = factor("treat_none", levels = .dca_legend_keys))
  p <- ggplot2::ggplot(pd, ggplot2::aes(x = threshold, y = net_cases_per1000,
                                        color = model_key, linetype = model_key, group = model_key)) +
    ggplot2::geom_hline(data = pnone, ggplot2::aes(yintercept = yintercept, color = reference_key,
                                                   linetype = reference_key),
                        inherit.aes = FALSE, linewidth = 0.35) +
    ggplot2::geom_line(data = pref, ggplot2::aes(x = threshold, y = net_cases_per1000,
                                                 color = reference_key, linetype = reference_key, group = 1),
                       inherit.aes = FALSE, linewidth = 0.4)
  if (identical(outcome_value, "readmission")) {
    p <- p + ggplot2::geom_ribbon(data = .dca_envelope,
                                  ggplot2::aes(x = threshold, ymin = lower, ymax = upper, group = 1),
                                  inherit.aes = FALSE, fill = "#CFCFCF", color = "#A8A8A8",
                                  linewidth = 0.20, alpha = 0.55)
  } else {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = net_cases_per1000_lo, ymax = net_cases_per1000_hi,
                                               fill = model_key, group = model_key),
                                  color = NA, alpha = 0.30, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = .dca_fills, limits = .dca_model_keys, breaks = .dca_model_keys,
                                 labels = unname(.dca_labels[.dca_model_keys]), drop = FALSE, name = NULL)
  }
  p +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::facet_wrap(~horizon_label, nrow = 1, scales = "free") +
    ggplot2::scale_color_manual(values = .dca_leg_colors, limits = .dca_legend_keys, breaks = .dca_legend_keys,
                                labels = unname(.dca_labels[.dca_legend_keys]), drop = FALSE, name = NULL) +
    ggplot2::scale_linetype_manual(values = .dca_leg_ltys, limits = .dca_legend_keys, breaks = .dca_legend_keys,
                                   labels = unname(.dca_labels[.dca_legend_keys]), drop = FALSE, name = NULL) +
    ggplot2::scale_x_continuous(labels = .dca_pct, breaks = scales::breaks_pretty(n = 4),
                                expand = ggplot2::expansion(mult = c(0.01, 0.03))) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ",", decimal.mark = "."),
                                breaks = scales::breaks_pretty(n = 5),
                                expand = ggplot2::expansion(mult = c(0.04, 0.08))) +
    ggplot2::labs(x = "Probability threshold", y = "Net benefit per 1,000") +
    .dca_theme_r +
    ggplot2::theme(plot.title = ggplot2::element_blank())
}
# ---- Legend carrying knobs (3) and (4) ----------------------------------------
.dca_leg_data <- data.frame(x = 0, xend = 1, y = seq_along(.dca_legend_keys),
                            yend = seq_along(.dca_legend_keys),
                            legend_key = factor(.dca_legend_keys, levels = .dca_legend_keys))
.dca_leg_plot <- ggplot2::ggplot(.dca_leg_data) +
  ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
                                     color = legend_key, linetype = legend_key), linewidth = 0.75) +
  ggplot2::scale_color_manual(values = .dca_leg_colors, limits = .dca_legend_keys, breaks = .dca_legend_keys,
                              labels = unname(.dca_labels[.dca_legend_keys]), drop = FALSE, name = NULL) +
  ggplot2::scale_linetype_manual(values = .dca_leg_ltys, limits = .dca_legend_keys, breaks = .dca_legend_keys,
                                 labels = unname(.dca_labels[.dca_legend_keys]), drop = FALSE, name = NULL) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = DCA_R34_LEGEND_NROW, byrow = TRUE, order = 1),
                  linetype = ggplot2::guide_legend(nrow = DCA_R34_LEGEND_NROW, byrow = TRUE, order = 1)) +
  ggplot2::theme_void(base_family = "sans") +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.box.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.position = "bottom", legend.direction = "horizontal",
    legend.text = ggplot2::element_text(size = DCA_R34_LEGEND_TEXT_SIZE, color = "black", lineheight = 0.88),
    legend.key = ggplot2::element_rect(fill = "white", color = NA),
    legend.key.width = grid::unit(DCA_R34_LEGEND_KEY_W, "pt"),
    legend.key.height = grid::unit(DCA_R34_LEGEND_KEY_H, "pt"),
    legend.spacing.x = grid::unit(0.3, "pt"),
    legend.box.spacing = grid::unit(0, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    plot.margin = ggplot2::margin(0, 0, 0, 0))
.dca_leg_grob <- cowplot::get_plot_component(.dca_leg_plot, "guide-box-bottom", return_all = TRUE)
# ---- Assemble ------------------------------------------------------------------
.dca_p_readmit <- .dca_panel("readmission") + ggplot2::theme(axis.title.x = ggplot2::element_blank())
.dca_p_death <- .dca_panel("death") +
  ggplot2::theme(plot.margin = ggplot2::margin(DCA_R2_TOP_MARGIN_PT, 5, DCA_R34_BODY_BOTTOM_MARGIN_PT, 5))
dca_figure_refined_body <- (.dca_p_readmit / .dca_p_death) +
  patchwork::plot_layout(heights = c(1, 1)) +
  patchwork::plot_annotation(
    tag_levels = "A",
    theme = ggplot2::theme(plot.tag = ggplot2::element_text(family = "sans", face = "bold",
                                                            size = DCA_R2_TAG_SIZE)))
dca_figure_refined <- cowplot::plot_grid(dca_figure_refined_body, .dca_leg_grob, ncol = 1,
                                         rel_heights = c(1, DCA_R34_LEGEND_REL_H)) +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))
print(dca_figure_refined)
# ---- Export, never overwriting the original Figure19 files ---------------------
.dca_fig_dir <- if (exists("project_root", inherits = TRUE)) file.path(project_root, "cons", "_figs") else file.path(getwd(), "cons", "_figs")
dir.create(.dca_fig_dir, recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(file.path(.dca_fig_dir, "Figure19_DCA_bootstrap_patients_refined.pdf"),
                dca_figure_refined, device = grDevices::cairo_pdf,
                width = 7.2, height = 7.6, units = "in", bg = "white")
ggplot2::ggsave(file.path(.dca_fig_dir, "Figure19_DCA_bootstrap_patients_refined_600dpi.tiff"),
                dca_figure_refined, device = ragg::agg_tiff,
                width = 7.2, height = 7.6, units = "in", dpi = 600,
                compression = "lzw", background = "white")
ggplot2::ggsave(file.path(.dca_fig_dir, "Figure19_DCA_bootstrap_patients_refined.png"),
                dca_figure_refined, device = ragg::agg_png,
                width = 7.2, height = 7.6, units = "in", dpi = 300, background = "white")
message("dca-refine: wrote the refined DCA figure to ", .dca_fig_dir)
