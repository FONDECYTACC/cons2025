# =====================================================================================
# plot_calibration_indices_epi.R
# Held-out calibration SUMMARY-INDICES figure for Article 2 (prediction23).
#
# This is the companion to the decile calibration-CURVE figure (cal_fig_improved):
# the curve shows predicted-vs-observed within deciles at single horizons; THIS figure
# summarizes calibration ACROSS the landmark horizons (6, 12, 36, 60 months).
#
# What changed vs the previous boot2 / boot3 panels, and why (epidemiology):
#   1. Three distinct calibration metrics, each on its own panel (default = ICI, ECE, E:O):
#        ICI -> integrated calibration error (smooth curve, integrated over the
#               distribution of predicted risk; 0 = perfect)
#        ECE -> binned calibration error (mean absolute gap over decile bins; NOT
#               integrated, so it is a different estimator from ICI, not a discretized ICI)
#        E:O -> calibration-in-the-large (systematic over/under-prediction; 1 = none)
#      Pass `panels` to show any subset (e.g. panels = c("ici", "eo")).
#   2. Horizons stay on a CONTINUOUS numeric axis (true months since discharge), with
#      breaks at 6, 12, 36, 60. No categorical rescaling.
#   3. E:O is a RATIO, so it is drawn on a LOG axis (0.8 and 1.25 sit symmetrically
#      around 1) with a reference at 1 and a faint heuristic tolerance band.
#   4. Compact y-axis titles (ICI / ECE / E:O ratio) fix the y-title overlap of the
#      stacked panels; the full definitions live in the caption.
#   5. The caption is NOT baked into the figure. It is emitted with message() so it
#      prints below the chunk (toggle with emit_caption); the text is also returned as
#      attr(g, "caption") for reuse.
#
# FIGURE CAPTION (English; also emitted via message() when the function runs):
#   Calibration of predicted risk on the held-out 20% test set across horizons (6, 12,
#   36, and 60 months since discharge). Predictions are pooled across multiple imputations
#   with the model held fixed; points are point estimates and whiskers are 95% bootstrap
#   percentile intervals. Observed risk is the Aalen-Johansen cumulative incidence for
#   readmission (death treated as a competing risk) and the Kaplan-Meier estimate for
#   mortality. ICI (integrated calibration index): the loess-smoothed mean absolute
#   difference between predicted and observed risk, integrated over the distribution of
#   predicted risk (lower is better; 0 is perfect). ECE (estimated calibration error):
#   the frequency-weighted mean absolute gap between predicted and observed risk across
#   decile bins. E:O: the expected to observed ratio (calibration-in-the-large); 1
#   indicates no systematic miscalibration and values above 1 indicate over-prediction.
#   Readmission is summarized by a single SHAP-informed model; mortality by two models
#   (Full proportional hazards, best_perf1; SHAP 13-variable, best_perf2). The y axes for
#   ICI and ECE are outcome-specific: average error is roughly an order of magnitude
#   smaller for mortality than for readmission.
#
# Input: ici_boot, a flat data.frame (one row per model x horizon) with columns
#   model, risk, horizon, ici/ici_lo/ici_hi, ece/ece_lo/ece_hi, eo/eo_lo/eo_hi, B.
# Depends: ggplot2, patchwork, dplyr. Times New Roman must be available to the device.
# =====================================================================================

plot_calibration_indices_epi <- function(ici_boot,
                                          tnr          = "Times New Roman",
                                          figs_out     = NULL,
                                          panels       = c("ici", "ece", "eo"),
                                          lang         = c("en", "es"),
                                          save         = TRUE,
                                          emit_caption = TRUE,
                                          eo_band      = c(0.80, 1.25),
                                          eo_breaks    = NULL,
                                          model_labels = NULL) {

  stopifnot(is.data.frame(ici_boot) || is.matrix(ici_boot))
  lang <- match.arg(lang)               # "en" = English, "es" = Spanish (coma decimal)
  d <- as.data.frame(ici_boot)

  # ---- choose panels (default shows all three; subset via `panels`) -------------------
  metric_keys <- c("ici", "ece", "eo")
  panels <- intersect(panels, metric_keys)
  stopifnot(length(panels) >= 1L)

  # ---- long format: one row per (model, horizon, metric) -----------------------------
  mk <- function(m) data.frame(
    risk    = d$risk,
    model   = d$model,
    horizon = d$horizon,
    metric  = m,
    value   = d[[m]],
    lower   = d[[paste0(m, "_lo")]],
    upper   = d[[paste0(m, "_hi")]],
    stringsAsFactors = FALSE)
  long <- do.call(rbind, lapply(panels, mk))

  # horizon stays a CONTINUOUS numeric axis (true months since discharge)
  hz <- sort(unique(long$horizon))
  long$horizon <- as.numeric(long$horizon)

  # ---- text labels (English / Spanish) -----------------------------------------------
  outcome_lab <- if (lang == "es") c("Readmisión", "Mortalidad")
                 else               c("Readmission", "Mortality")
  long$outcome <- factor(ifelse(long$risk == "readmission", outcome_lab[1], outcome_lab[2]),
                         levels = outcome_lab)

  # ordered model labels keyed by the internal model id (fixed display order:
  # [1] readmission, [2] death best_perf1, [3] death best_perf2). Override any or all of
  # them with `model_labels`, a named vector keyed by the SAME ids, WITHOUT recoding
  # ici_boot$model, so colours, shapes and ordering stay aligned.
  default_labels <- if (lang == "es")
      c("readmit::shared"   = "Informado por SHAP (readmisión)",
        "death::best_perf1" = "RP completo, best_perf1 (mortalidad)",
        "death::best_perf2" = "SHAP 13 variables, best_perf2 (mortalidad)")
    else
      c("readmit::shared"   = "SHAP-informed (readmission)",
        "death::best_perf1" = "Full PH, best_perf1 (mortality)",
        "death::best_perf2" = "SHAP 13-var, best_perf2 (mortality)")

  if (!is.null(model_labels)) {
    hit <- intersect(names(default_labels), names(model_labels))
    if (length(hit)) default_labels[hit] <- model_labels[hit]   # partial override, fixed order
  }
  model_levels <- unname(default_labels)

  # map each row's model id to its display label; unknown ids fall back to the raw id
  lab <- unname(default_labels[long$model])
  lab[is.na(lab)] <- long$model[is.na(lab)]
  long$model_lab <- factor(lab, levels = unique(c(model_levels, lab)))

  # colorblind-aware: cool hue for readmission, warm hues for the two mortality models
  cols <- stats::setNames(c("#2166AC", "#B2182B", "#E08214"), model_levels)
  shp  <- stats::setNames(c(21, 24, 22), model_levels)

  x_title  <- if (lang == "es") "Meses desde el egreso" else "Months since discharge"
  y_titles <- c(ici = "ICI", ece = "ECE",
                eo = if (lang == "es") "razón E:O" else "E:O ratio")
  # Spanish uses a comma decimal mark on the y axes
  num_fmt  <- if (lang == "es") scales::label_number(decimal.mark = ",", big.mark = "")
              else ggplot2::waiver()

  pd <- ggplot2::position_dodge(width = 1.8)   # small horizontal offset for the 2 mortality models

  theme_epi <- ggplot2::theme_classic(base_size = 12, base_family = tnr) +
    ggplot2::theme(
      legend.position    = "bottom",
      legend.title       = ggplot2::element_blank(),
      legend.key.width   = ggplot2::unit(1.1, "lines"),
      strip.background   = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(face = "bold", size = 12),
      axis.title         = ggplot2::element_text(face = "bold"),
      axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.text          = ggplot2::element_text(color = "black"),
      panel.grid.major.y = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      panel.spacing      = ggplot2::unit(0.9, "lines"),
      plot.tag           = ggplot2::element_text(face = "bold", size = 15, family = tnr),
      plot.margin        = ggplot2::margin(4, 8, 4, 6))

  mk_panel <- function(m, show_x, show_strip) {
    dd <- long[long$metric == m, , drop = FALSE]
    dd <- dd[order(dd$model_lab, dd$horizon), ]
    is_eo <- identical(m, "eo")

    g <- ggplot2::ggplot(dd, ggplot2::aes(horizon, value,
                                          color = model_lab, shape = model_lab,
                                          group = model_lab))

    if (is_eo) {
      if (length(eo_band) == 2L) {
        g <- g + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                                   ymin = eo_band[1], ymax = eo_band[2],
                                   fill = "grey70", alpha = 0.12)
      }
      g <- g + ggplot2::geom_hline(yintercept = 1, linetype = "22",
                                   color = "grey35", linewidth = 0.4)
    }

    g <- g +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                             width = 1.2, linewidth = 0.5, alpha = 0.85, position = pd) +
      ggplot2::geom_line(linewidth = 0.7, alpha = 0.9, position = pd) +
      ggplot2::geom_point(size = 2.6, fill = "white", stroke = 0.9, position = pd) +
      ggplot2::facet_wrap(~ outcome, nrow = 1, scales = "free_y") +
      ggplot2::scale_color_manual(values = cols, drop = FALSE) +
      ggplot2::scale_shape_manual(values = shp,  drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = hz) +
      ggplot2::labs(x = x_title, y = y_titles[[m]]) +
      theme_epi

    if (is_eo) {
      # ratio -> log axis; 0.8 and 1.25 sit symmetrically about the reference of 1.
      # scale_y_log10 defaults to powers-of-ten breaks, so in a narrow window around 1
      # only "1" gets labelled. Set explicit, band-congruent breaks (override via eo_breaks).
      if (!is.null(eo_breaks)) {
        eo_brk <- eo_breaks
      } else {
        cand <- sort(unique(c(0.5, 0.67, 0.8, 0.9, 1, 1.1, 1.25, 1.5, 2, eo_band)))
        rng  <- range(c(dd$value, dd$lower, dd$upper, eo_band), na.rm = TRUE)
        eo_brk <- cand[cand >= rng[1] * 0.98 & cand <= rng[2] * 1.02]
      }
      g <- g + ggplot2::scale_y_log10(labels = num_fmt, breaks = eo_brk)
      # facets use free_y, so a break only renders if it falls inside that facet's range.
      # With eo_band = NULL nothing widens the readmission facet (its E:O sits near 1), so
      # most breaks would be dropped. Expand each facet to span the requested breaks
      # (expand_limits only grows the range; it never clips points or intervals).
      if (length(eo_brk)) g <- g + ggplot2::expand_limits(y = range(eo_brk, na.rm = TRUE))
    } else {
      g <- g + ggplot2::scale_y_continuous(labels = num_fmt) +
               ggplot2::expand_limits(y = 0)   # 0 = perfect-calibration anchor
    }
    if (!show_strip) g <- g + ggplot2::theme(strip.text = ggplot2::element_blank())
    if (!show_x)     g <- g + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                             axis.text.x  = ggplot2::element_blank(),
                                             axis.ticks.x = ggplot2::element_blank())
    g
  }

  np <- length(panels)
  plot_list <- lapply(seq_len(np), function(k)
    mk_panel(panels[k], show_x = (k == np), show_strip = (k == 1L)))

  # Caption is emitted as a message (prints below the chunk), not baked into the figure.
  cap <- if (lang == "es") paste(
    "Conjunto de prueba retenido (20%); predicciones combinadas entre imputaciones múltiples",
    "(modelo fijo). Bigotes: intervalos percentiles bootstrap del 95%. Riesgo observado:",
    "Aalen-Johansen (readmisión, muerte como riesgo competitivo) y Kaplan-Meier (mortalidad).",
    "ICI: error absoluto medio de calibración suavizado por loess e integrado sobre la",
    "distribución del riesgo predicho (0 = perfecto). ECE: brecha absoluta media ponderada",
    "por frecuencia entre deciles. Razón E:O: esperado:observado (1 = sin mala calibración",
    "sistemática; >1 = sobrepredicción). Los ejes y de ICI y ECE son específicos por desenlace.")
  else paste(
    "Held-out 20% test set; predictions pooled over multiple imputations (model fixed).",
    "Whiskers: 95% bootstrap percentile intervals. Observed risk: Aalen-Johansen",
    "(readmission, death as a competing risk) and Kaplan-Meier (mortality).",
    "ICI: loess-smoothed mean absolute calibration error integrated over the predicted-risk",
    "distribution (0 = perfect). ECE: frequency-weighted mean absolute gap across decile bins.",
    "E:O: expected:observed (1 = no systematic miscalibration; >1 = over-prediction).",
    "y axes for ICI and ECE are outcome-specific.")

  g_all <- patchwork::wrap_plots(plot_list, ncol = 1) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(legend.position = "bottom")

  attr(g_all, "caption") <- cap
  if (isTRUE(emit_caption)) message(cap)

  if (isTRUE(save) && !is.null(figs_out)) {
    h_cm <- 6 + 5.2 * np                       # scale height with number of panels
    base <- file.path(figs_out, paste0("holdout_calibration_indices_epi_", lang))
    ggplot2::ggsave(paste0(base, ".tiff"), g_all, width = 17.8, height = h_cm,
                    units = "cm", dpi = 600, compression = "lzw")
    ggplot2::ggsave(paste0(base, ".png"),  g_all, width = 17.8, height = h_cm,
                    units = "cm", dpi = 600)
  }
  g_all
}

# ---- usage (run after ici_boot exists) --------------------------------------------
# source(file.path(project_root, "cons/_alt_scripts/plot_calibration_indices_epi.R"))
# g_cal_idx <- plot_calibration_indices_epi(ici_boot, tnr = "Times New Roman",
#                                           figs_out = figs_out)
# print(g_cal_idx)
# # the caption prints below the chunk via message(); reuse it with:
# # cat(attr(g_cal_idx, "caption"))
#
# # Spanish labels + comma decimal mark (saves *_es.{tiff,png}):
# # plot_calibration_indices_epi(ici_boot, figs_out = figs_out, lang = "es")
#
# # show only a subset of metrics, e.g. ICI + E:O:
# # plot_calibration_indices_epi(ici_boot, figs_out = figs_out, panels = c("ici", "eo"))
#
# # custom (short) legend labels WITHOUT recoding ici_boot$model -- key by the model id:
# # plot_calibration_indices_epi(
# #   ici_boot, figs_out = figs_out, lang = "es",
# #   model_labels = c("readmit::shared"   = "Readmisión",
# #                    "death::best_perf1" = "Mortalidad: modelo 1",
# #                    "death::best_perf2" = "Mortalidad: modelo 2"))
# # (partial override is fine: pass only the ids you want to rename.)
#
# # custom E:O axis ticks (defaults are band-congruent; override explicitly if you want):
# # plot_calibration_indices_epi(ici_boot, figs_out = figs_out, panels = c("ici", "eo"),
# #                              eo_band = c(0.80, 1.5), eo_breaks = c(0.8, 1, 1.25, 1.5))
#
# # If accents look wrong, source this file as UTF-8:
# # source(".../plot_calibration_indices_epi.R", encoding = "UTF-8")
