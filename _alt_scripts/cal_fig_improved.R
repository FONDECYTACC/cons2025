# =====================================================================================
# cal_fig_improved.R
# Held-out calibration CURVE figure for Article 2 (prediction23), parametrized.
#
# Companion to the summary-indices figure (plot_calibration_indices_epi): THIS one shows
# the predicted-vs-observed curve within bins at each landmark horizon (6, 12, 36, 60
# months), with the calibration indices (ICI, ECE, E:O) annotated per panel.
#
# What this rewrite fixes vs the inline notebook version:
#   1. Language: lang = "en" | "es" switches axis titles, the horizon word
#      (months / meses), and the decimal mark (comma for Spanish). Every label can also
#      be overridden directly (x_lab, y_lab, month_word).
#   2. "Collapsed" layout: per-figure axis titles are turned OFF in the assembler and a
#      SINGLE shared y title and x title are added once, so the square panels get room.
#      assemble_cal_curves() also defaults to a ~4:3 canvas (the natural aspect of a
#      4-column x 3-row grid of square panels) instead of the too-tall 28x24.
#   3. tnr (font) is an argument, not a global; annotation size is tunable; panel spacing
#      and number formatting are cleaned up.
#
# Depends: ggplot2, patchwork, dplyr, scales, grid (base). Times New Roman must be
# available to the graphics device (otherwise it falls back silently).
# =====================================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# -------------------------------------------------------------------------------------
# Single-outcome calibration curve (one row, faceted by horizon).
# -------------------------------------------------------------------------------------
cal_fig_improved <- function(cal_obj,
                             ttl        = NULL,
                             color      = "#2166AC",
                             x_max      = NULL,
                             tnr        = "Times New Roman",
                             lang       = c("en", "es"),
                             x_lab      = NULL,
                             y_lab      = NULL,
                             month_word = NULL,
                             show_x_lab = TRUE,
                             show_y_lab = TRUE,
                             annotate   = TRUE,
                             annot_size = 2.9,
                             n_breaks   = 4L) {

  lang <- match.arg(lang)
  cur  <- cal_obj$calibration_curves
  ps   <- cal_obj$pooled_summary
  if (is.null(x_max)) x_max <- max(cur$observed_upper, cur$mean_predicted, na.rm = TRUE) * 1.05

  # ---- language-aware labels (any can be overridden by the caller) -------------------
  if (is.null(month_word)) month_word <- if (lang == "es") "meses" else "months"
  if (is.null(x_lab))      x_lab <- if (lang == "es") "Probabilidad predicha" else "Predicted probability"
  if (is.null(y_lab))      y_lab <- if (lang == "es") "Proporción observada (AJ / KM)"
                                    else                "Observed proportion (AJ / KM)"

  lv <- paste0(sort(unique(cur$time_months)), " ", month_word)
  cur$facet <- factor(paste0(cur$time_months, " ", month_word), levels = lv)

  # ---- per-panel index annotation (comma decimals in Spanish) ------------------------
  dec  <- if (lang == "es") "," else "."
  fmtn <- function(x, d) formatC(x, format = "f", digits = d, decimal.mark = dec)
  ann  <- data.frame(
    facet = factor(paste0(ps$time_months, " ", month_word), levels = lv),
    lab   = sprintf("ICI %s\nECE %s\nE:O %s",
                    fmtn(ps$ici_mean, 3), fmtn(ps$ece_mean, 3), fmtn(ps$eo_mean, 2)),
    stringsAsFactors = FALSE
  )

  num_fmt <- scales::label_number(decimal.mark = dec, big.mark = "",
                                  accuracy = if (x_max <= 0.2) 0.01 else 0.1)

  g <- ggplot2::ggplot(cur, ggplot2::aes(mean_predicted, observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "grey40", linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = observed_lower, ymax = observed_upper),
                         alpha = 0.25, fill = color) +
    ggplot2::geom_line(color = color, linewidth = 0.8) +
    ggplot2::geom_point(shape = 21, fill = "white", color = color, size = 1.6, stroke = 0.7)

  if (isTRUE(annotate)) {
    g <- g + ggplot2::geom_text(data = ann, ggplot2::aes(0, x_max, label = lab),
                                hjust = 0, vjust = 1, size = annot_size,
                                family = tnr, lineheight = 0.95, inherit.aes = FALSE)
  }

  g <- g +
    ggplot2::facet_wrap(~ facet, nrow = 1) +
    ggplot2::coord_equal(xlim = c(0, x_max), ylim = c(0, x_max), clip = "off") +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n_breaks), labels = num_fmt) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n_breaks), labels = num_fmt) +
    ggplot2::labs(x = x_lab, y = y_lab, title = ttl) +
    ggplot2::theme_bw(base_size = 12, base_family = tnr) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing    = ggplot2::unit(0.7, "lines"),
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey80"),
      strip.text       = ggplot2::element_text(face = "bold"),
      axis.title       = ggplot2::element_text(face = "bold", size = 13))

  if (!show_y_lab) g <- g + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  if (!show_x_lab) g <- g + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  g
}

# -------------------------------------------------------------------------------------
# Stack several outcome rows with ONE shared x title and ONE shared y title.
# `rows` is a list of lists, each: list(cal=<cal_obj>, color=, x_max=, ttl=).
# Returns the patchwork object and (optionally) saves it.
# -------------------------------------------------------------------------------------
assemble_cal_curves <- function(rows,
                                tnr        = "Times New Roman",
                                lang       = c("en", "es"),
                                x_lab      = NULL,
                                y_lab      = NULL,
                                file       = NULL,
                                width      = 28,
                                height     = 21,
                                dpi        = 600,
                                annot_size = 2.9,
                                tag_levels = "A") {

  lang <- match.arg(lang)
  if (is.null(x_lab)) x_lab <- if (lang == "es") "Probabilidad predicha" else "Predicted probability"
  if (is.null(y_lab)) y_lab <- if (lang == "es") "Proporción observada (AJ / KM)"
                               else                "Observed proportion (AJ / KM)"

  # Per-figure panels with NO axis titles; the shared x/y titles are added once as
  # full-height/full-width grobs so a long y title is never clipped. Tags (A, B, C) are
  # attached per figure with labs(tag=) so they survive the grob composition (patchwork's
  # own tag_levels would also tag the title grobs).
  n    <- length(rows)
  tags <- if (is.null(tag_levels)) rep("", n) else LETTERS[seq_len(n)]
  figs <- lapply(seq_len(n), function(i) {
    r <- rows[[i]]
    g <- cal_fig_improved(
      cal_obj    = r$cal,
      ttl        = r$ttl   %||% NULL,
      color      = r$color %||% "#2166AC",
      x_max      = r$x_max %||% NULL,
      tnr        = tnr, lang = lang,
      show_x_lab = FALSE, show_y_lab = FALSE,
      annotate   = TRUE, annot_size = annot_size)
    if (nzchar(tags[i]))
      g <- g + ggplot2::labs(tag = tags[i]) +
        ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", family = tnr))
    g
  })

  stacked <- patchwork::wrap_plots(figs, ncol = 1)

  yg <- grid::textGrob(y_lab, rot = 90,
                       gp = grid::gpar(fontfamily = tnr, fontface = "bold", fontsize = 13))
  xg <- grid::textGrob(x_lab,
                       gp = grid::gpar(fontfamily = tnr, fontface = "bold", fontsize = 13))

  body  <- (patchwork::wrap_elements(yg) | stacked) +
    patchwork::plot_layout(widths = c(0.035, 1))
  final <- (body / patchwork::wrap_elements(full = xg)) +
    patchwork::plot_layout(heights = c(1, 0.035))

  if (!is.null(file))
    ggplot2::ggsave(file, final, width = width, height = height, units = "cm", dpi = dpi)
  final
}

# ---- usage (run after cal_readmit / cal_death_bp1 / cal_death_bp2 exist) -------------
# source(file.path(project_root, "cons/_alt_scripts/cal_fig_improved.R"))
#
# # English, shared labels, saved with a sensible 4:3 canvas:
# assemble_cal_curves(
#   rows = list(
#     list(cal = cal_readmit,   color = "#2166AC", x_max = 0.40, ttl = "Readmission (Aalen-Johansen)"),
#     list(cal = cal_death_bp1, color = "#B2182B", x_max = 0.15, ttl = "Mortality - full PH (best_perf1)"),
#     list(cal = cal_death_bp2, color = "#B2182B", x_max = 0.15, ttl = "Mortality - SHAP (best_perf2)")),
#   lang = "en",
#   file = file.path(figs_out, "holdout_calibration_curves.png"))
#
# # Spanish (comma decimals, "meses", Spanish titles) -- just flip lang and the titles:
# assemble_cal_curves(
#   rows = list(
#     list(cal = cal_readmit,   color = "#2166AC", x_max = 0.40, ttl = "Readmisión (Aalen-Johansen)"),
#     list(cal = cal_death_bp1, color = "#B2182B", x_max = 0.15, ttl = "Mortalidad: todos los predictores"),
#     list(cal = cal_death_bp2, color = "#B2182B", x_max = 0.15, ttl = "Mortalidad: informados por SHAP")),
#   lang = "es",
#   file = file.path(figs_out, "holdout_calibration_curves_es.png"))
#
# # A single outcome row only:
# # cal_fig_improved(cal_readmit, ttl = "Readmisión", color = "#2166AC", x_max = 0.40, lang = "es")
