# =====================================================================================
# make_dca_panel_figure_std.R
# Standardized-net-benefit DCA panel figure for the HELD-OUT 20% of prediction23,
# language-aware (English / Spanish). Standalone: does NOT modify the shared
# make_dca_panel_figure() in dca_from_results_boot_for_metrics.R.
#
# Source objects = dca_models_full$best_perf1 / $best_perf2 (built in holdout-dca-run):
#   best_perf1 = SHAP readmit + Full PH death (Row A)
#   best_perf2 = SHAP readmit + SHAP death    (Row B)
# Readmission is the SAME model in bp1 and bp2, so the readmission A/B rows are identical
# by design; the real A-vs-B contrast is the death panel.
#
# y = standardized net benefit = net_benefit_mean / observed event rate (so the model
# curve is visible and comparable across horizons); x truncated per outcome from the
# focus windows; y cropped to the meaningful range ("treat all" may run off the bottom).
#
# Spanish (lang = "es"): axis titles, the horizon word ("meses"), the strategy legend
# ("Modelo", "Tratar a todos", "Tratar a ninguno") and a comma decimal mark on the axes.
# Every label can also be overridden directly.
#
# Depends: ggplot2, scales.
# =====================================================================================

make_dca_panel_figure_std <- function(
    summary_full,
    summary_ml,
    outcome         = c("death", "readmission"),
    horizons        = c(12, 36, 60),
    x_limits        = NULL,            # NULL = data-driven from the focus windows
    y_limits        = c(-0.2, 1.03),   # standardized scale: 1.0 = treat-all at threshold 0
    lang            = c("en", "es"),
    tnr             = "Times New Roman",
    x_lab           = NULL,
    y_lab           = NULL,
    month_word      = NULL,
    strategy_labels = NULL,            # named vector keyed by Model / Treat all / Treat none
    row_labels      = c(A = "A", B = "B"),
    emit_caption    = FALSE) {

  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("scales",  quietly = TRUE))
  outcome <- match.arg(outcome)
  lang    <- match.arg(lang)

  # ---- language-aware labels (any can be overridden by the caller) -------------------
  if (is.null(month_word)) month_word <- if (lang == "es") "meses" else "months"
  if (is.null(x_lab))      x_lab <- if (lang == "es") "Probabilidad umbral" else "Threshold probability"
  if (is.null(y_lab))      y_lab <- if (lang == "es") "Beneficio neto estandarizado"
                                    else                "Standardized net benefit"
  if (is.null(strategy_labels))
    strategy_labels <- if (lang == "es")
      c("Model" = "Modelo", "Treat all" = "Tratar a todos", "Treat none" = "Tratar a ninguno")
    else
      c("Model" = "Model", "Treat all" = "Treat all", "Treat none" = "Treat none")

  full_dat <- summary_full; full_dat$model_panel <- "A"
  ml_dat   <- summary_ml;   ml_dat$model_panel   <- "B"
  plot_dat <- rbind.data.frame(full_dat, ml_dat)
  plot_dat <- plot_dat[plot_dat$risk %in% outcome &
                         plot_dat$horizon %in% horizons, , drop = FALSE]
  if (!nrow(plot_dat)) stop("No rows for the requested outcome/horizons.", call. = FALSE)

  # Standardized net benefit = net benefit / event prevalence at the horizon.
  prev <- plot_dat$observed_event_risk_mean
  plot_dat$std_nb <- ifelse(is.finite(prev) & prev > 0,
                            plot_dat$net_benefit_mean / prev, NA_real_)

  # strategy displayed in the chosen language (keep the fixed Model/all/none order)
  disp_levels <- unname(strategy_labels[c("Model", "Treat all", "Treat none")])
  plot_dat$strategy_plot <- factor(unname(strategy_labels[plot_dat$strategy]),
                                   levels = disp_levels)
  plot_dat$model_panel   <- factor(plot_dat$model_panel, levels = c("A", "B"))
  plot_dat$horizon       <- factor(plot_dat$horizon, levels = horizons)

  lt_vals <- stats::setNames(c("solid", "dotted", "dotdash"), disp_levels)
  lw_vals <- stats::setNames(c(0.9, 0.6, 0.6),               disp_levels)

  focus_dat <- plot_dat[!duplicated(plot_dat[, c("model_panel", "horizon",
                                                 "focus_lower", "focus_upper")]), ,
                        drop = FALSE]

  # Data-driven x truncation: cover the focus windows + margin, capped at 0.5.
  if (is.null(x_limits)) {
    x_hi <- min(0.5, max(focus_dat$focus_upper, na.rm = TRUE) * 1.3)
    x_limits <- c(0, x_hi)
  }

  num_fmt <- if (lang == "es")
    scales::label_number(decimal.mark = ",", big.mark = "")
  else ggplot2::waiver()

  g <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = focus_dat,
      ggplot2::aes(xmin = focus_lower, xmax = focus_upper, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = "grey80", alpha = 0.18, color = NA
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(
      data = plot_dat,
      ggplot2::aes(x = threshold, y = std_nb,
                   linetype = strategy_plot, linewidth = strategy_plot)
    ) +
    ggplot2::facet_grid(
      model_panel ~ horizon, switch = "y",
      labeller = ggplot2::labeller(
        horizon     = function(x) paste0(x, " ", month_word),
        model_panel = row_labels
      )
    ) +
    ggplot2::scale_linetype_manual(values = lt_vals) +
    ggplot2::scale_linewidth_manual(values = lw_vals) +
    ggplot2::scale_x_continuous(labels = num_fmt) +
    ggplot2::scale_y_continuous(labels = num_fmt) +
    # adjacent facets share the x range; drop edge tick labels that would collide
    ggplot2::guides(x = ggplot2::guide_axis(check.overlap = TRUE)) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "on") +
    ggplot2::labs(x = x_lab, y = y_lab, linetype = NULL, linewidth = NULL) +
    ggplot2::theme_classic(base_size = 16, base_family = tnr) +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      strip.background  = ggplot2::element_blank(),
      strip.placement   = "outside",
      strip.text.y.left = ggplot2::element_text(face = "bold", size = 16, angle = 0),
      strip.text.x      = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
      axis.text         = ggplot2::element_text(color = "black"),
      axis.text.x       = ggplot2::element_text(size = 13),
      axis.title        = ggplot2::element_text(face = "plain"),
      panel.spacing.x   = ggplot2::unit(1.6, "lines"),   # gap so edge x labels do not collide
      panel.spacing.y   = ggplot2::unit(0.9, "lines"),
      plot.caption      = ggplot2::element_text(hjust = 0, size = 10, face = "italic")
    )

  cap <- if (lang == "es") paste(
    "DCA en el conjunto de prueba retenido (20%). El beneficio neto estandarizado es el",
    "beneficio neto dividido por la tasa de eventos observada en cada horizonte",
    "(1,0 = tratar a todos en umbral 0). Fila A = best_perf1 (mortalidad RP completo),",
    "Fila B = best_perf2 (mortalidad SHAP); las filas de readmisión son idénticas (modelo",
    "de readmisión SHAP compartido). La curva de tratar a todos puede caer por debajo del",
    "rango mostrado. Banda sombreada: ventana plausible de umbrales (0,5x a 2x la tasa de",
    "eventos).")
  else paste(
    "Held-out 20% DCA. Standardized net benefit = net benefit / observed event rate at",
    "each horizon (1.0 = treating everyone at threshold 0). Row A = best_perf1 (Full PH",
    "death), Row B = best_perf2 (SHAP death); readmission rows are identical (shared SHAP",
    "readmit model). The treat-all curve may fall below the displayed range. Shaded band:",
    "plausible threshold window (0.5x to 2x the event rate).")

  attr(g, "caption") <- cap
  if (isTRUE(emit_caption)) message(cap)
  g
}

# =====================================================================================
# make_dca_panel_figure_abc(): the THREE meaningful DCA panels in ONE figure.
#   (A) Readmission (shared SHAP readmit model; collapsed to a single row)
#   (B) Mortality, all predictors (Full PH, best_perf1)
#   (C) Mortality, SHAP-informed (best_perf2)
# Each row is faceted by horizon (default 6, 12, 36, 60). The x range is data-driven PER
# ROW (readmission spans a wider threshold range than mortality), the y axis (standardized
# net benefit) is shared, the legend is collected once, and the rows carry A/B/C tags.
# Depends: ggplot2, scales, patchwork, grid (base).
# =====================================================================================
make_dca_panel_figure_abc <- function(
    dca_models_full,
    horizons     = c(6, 12, 36, 60),
    lang         = c("en", "es"),
    tnr          = "Times New Roman",
    y_limits        = c(-0.2, 1.03),
    row_titles      = NULL,
    show_row_titles = FALSE,   # FALSE = only the A/B/C tags (titles would steal panel height)
    tags            = c("A", "B", "C"),
    x_lab           = NULL,
    y_lab           = NULL,
    month_word      = NULL,
    emit_caption    = TRUE) {

  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("scales",  quietly = TRUE),
            requireNamespace("patchwork", quietly = TRUE))
  lang <- match.arg(lang)

  if (is.null(month_word)) month_word <- if (lang == "es") "meses" else "months"
  if (is.null(x_lab))      x_lab <- if (lang == "es") "Probabilidad umbral" else "Threshold probability"
  if (is.null(y_lab))      y_lab <- if (lang == "es") "Beneficio neto estandarizado"
                                    else                "Standardized net benefit"
  strat <- if (lang == "es")
      c("Model" = "Modelo", "Treat all" = "Tratar a todos", "Treat none" = "Tratar a ninguno")
    else
      c("Model" = "Model", "Treat all" = "Treat all", "Treat none" = "Treat none")
  if (is.null(row_titles)) row_titles <- if (lang == "es")
      c("Readmisión", "Mortalidad: todos los predictores", "Mortalidad: informado por SHAP")
    else
      c("Readmission", "Mortality: all predictors", "Mortality: SHAP-informed")

  num_fmt     <- if (lang == "es") scales::label_number(decimal.mark = ",", big.mark = "")
                 else ggplot2::waiver()
  disp_levels <- unname(strat[c("Model", "Treat all", "Treat none")])
  lt_vals     <- stats::setNames(c("solid", "dotted", "dotdash"), disp_levels)
  lw_vals     <- stats::setNames(c(0.9, 0.6, 0.6),               disp_levels)

  rows_spec <- list(
    list(summary = dca_models_full$best_perf1$summary, outcome = "readmission"),
    list(summary = dca_models_full$best_perf1$summary, outcome = "death"),
    list(summary = dca_models_full$best_perf2$summary, outcome = "death"))

  build_row <- function(spec, title, tag, show_x_strip, show_x_title) {
    d <- spec$summary
    d <- d[d$risk == spec$outcome & d$horizon %in% horizons, , drop = FALSE]
    prev <- d$observed_event_risk_mean
    d$std_nb <- ifelse(is.finite(prev) & prev > 0, d$net_benefit_mean / prev, NA_real_)
    d$strategy_plot <- factor(unname(strat[d$strategy]), levels = disp_levels)
    d$horizon <- factor(d$horizon, levels = horizons)
    focus <- d[!duplicated(d[, c("horizon", "focus_lower", "focus_upper")]), , drop = FALSE]
    x_hi <- min(0.5, max(focus$focus_upper, na.rm = TRUE) * 1.3)

    g <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = focus,
        ggplot2::aes(xmin = focus_lower, xmax = focus_upper, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE, fill = "grey80", alpha = 0.18, color = NA) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
      ggplot2::geom_line(data = d,
        ggplot2::aes(x = threshold, y = std_nb, linetype = strategy_plot, linewidth = strategy_plot)) +
      ggplot2::facet_wrap(~ horizon, nrow = 1,
        labeller = ggplot2::labeller(horizon = function(x) paste0(x, " ", month_word))) +
      ggplot2::scale_linetype_manual(values = lt_vals) +
      ggplot2::scale_linewidth_manual(values = lw_vals) +
      ggplot2::scale_x_continuous(labels = num_fmt) +
      ggplot2::scale_y_continuous(labels = num_fmt) +
      ggplot2::guides(x = ggplot2::guide_axis(check.overlap = TRUE)) +
      ggplot2::coord_cartesian(xlim = c(0, x_hi), ylim = y_limits, clip = "on") +
      ggplot2::labs(title = if (isTRUE(show_row_titles)) title else NULL, tag = tag,
                    x = if (show_x_title) x_lab else NULL, y = NULL,
                    linetype = NULL, linewidth = NULL) +
      ggplot2::theme_classic(base_size = 14, base_family = tnr) +
      ggplot2::theme(
        legend.position = "bottom", legend.box = "horizontal",
        plot.title      = ggplot2::element_text(face = "bold", size = 14),
        plot.tag        = ggplot2::element_text(face = "bold", size = 16, family = tnr),
        strip.background = ggplot2::element_blank(),
        strip.text.x    = if (show_x_strip) ggplot2::element_text(face = "bold", size = 13)
                          else ggplot2::element_blank(),
        axis.text       = ggplot2::element_text(color = "black", size = 11),
        panel.spacing.x = ggplot2::unit(1.4, "lines"))
    if (!show_x_title) g <- g + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    g
  }

  rowA <- build_row(rows_spec[[1]], row_titles[1], tags[1], show_x_strip = TRUE,  show_x_title = FALSE)
  rowB <- build_row(rows_spec[[2]], row_titles[2], tags[2], show_x_strip = FALSE, show_x_title = FALSE)
  rowC <- build_row(rows_spec[[3]], row_titles[3], tags[3], show_x_strip = FALSE, show_x_title = TRUE)

  yg <- grid::textGrob(y_lab, rot = 90,
                       gp = grid::gpar(fontfamily = tnr, fontface = "bold", fontsize = 14))
  final <- (patchwork::wrap_elements(yg) | patchwork::wrap_plots(list(rowA, rowB, rowC), ncol = 1)) +
    patchwork::plot_layout(widths = c(0.04, 1), guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  cap <- if (lang == "es") paste(
    "DCA en el conjunto de prueba retenido (20%); beneficio neto estandarizado (beneficio",
    "neto dividido por la tasa de eventos observada en cada horizonte; 1,0 = tratar a todos",
    "en umbral 0) según horizonte (6, 12, 36 y 60 meses). (A) Readmisión (modelo informado",
    "por SHAP, compartido). (B) Mortalidad con todos los predictores (RP completo).",
    "(C) Mortalidad informada por SHAP. Riesgo observado: incidencia acumulada de",
    "Aalen-Johansen para readmisión (muerte como riesgo competidor) y 1 menos Kaplan-Meier",
    "para mortalidad; curvas combinadas entre cinco imputaciones múltiples con el modelo",
    "fijo. El eje x se trunca por desenlace; la curva de tratar a todos puede caer por",
    "debajo del rango mostrado. Banda sombreada: ventana plausible de umbrales (0,5x a 2x",
    "la tasa de eventos).")
  else paste(
    "Held-out 20% DCA; standardized net benefit (net benefit / observed event rate at each",
    "horizon; 1.0 = treating everyone at threshold 0) by horizon (6, 12, 36, 60 months).",
    "(A) Readmission (shared SHAP-informed model). (B) Mortality with all predictors (Full",
    "PH). (C) SHAP-informed mortality. Observed risk: Aalen-Johansen cumulative incidence",
    "for readmission (death competing) and 1 minus Kaplan-Meier for mortality; curves pooled",
    "over five multiple imputations with the model fixed. x is truncated per outcome; the",
    "treat-all curve may fall below the displayed range. Shaded band: plausible threshold",
    "window (0.5x to 2x the event rate).")

  attr(final, "caption") <- cap
  if (isTRUE(emit_caption)) message(cap)
  final
}

# ---- usage (run after dca_models_full exists) ---------------------------------------
# source(file.path(project_root, "cons/_alt_scripts/make_dca_panel_figure_std.R"))
#
# # Two-row per-outcome figure:
# # make_dca_panel_figure_std(dca_models_full$best_perf1$summary,
# #   dca_models_full$best_perf2$summary, outcome = "death", lang = "es")
#
# # The three panels (readmission + 2 mortality models) in ONE figure, A/B/C:
# p_abc <- make_dca_panel_figure_abc(dca_models_full, horizons = c(6, 12, 36, 60), lang = "es")
# print(p_abc); cat(attr(p_abc, "caption"))
