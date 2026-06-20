# ── plot_threshold_metrics_clean.R ────────────────────────────────────────────
# Versión limpia (estilo Epidemiology): umbral en x, horizonte en color.
# Drop-in para plot_threshold_metrics_separate_outcomes(): mismas firmas,
# mismo retorno ($outcomes$<Risk>$plot). Consume threshold_out_best2$summary_long.

`%||%` <- function(x, y) if (is.null(x)) y else x

.thr_time_labels <- function(h) vapply(as.numeric(h), function(hh)
  if (hh >= 12 && hh %% 12 == 0) paste0(hh/12, " y") else paste0(hh, " mo"),
  character(1))

# Etiquetas legibles para las métricas (cabeceras de columnas)
.thr_metric_labels <- c(NPV = "NPV", PPV = "PPV",
                        Sens = "Sensitivity", Spec = "Specificity", F1 = "F1")

#' Una figura por outcome: filas = modelos, columnas = métricas,
#' x = umbral, color/línea = horizonte temporal.
plot_threshold_metrics_clean <- function(
    summary_long,
    horizons     = c(6, 12, 36, 60),
    panel_models = c("Full PH updated2", "SHAP primary"),
    panel_titles = c("Full PH", "SHAP primary"),
    metrics      = c("NPV", "PPV", "Sens", "Spec"),
    outcomes     = c("Readmission", "Death"),
    base_family  = "Times New Roman",
    uncertainty  = c("ribbon", "errorbar", "none"),  # banda q025–q975
    save_files   = FALSE, output_dir = NULL,
    prefix       = "threshold_metrics_clean",
    width_cm = 19, height_cm = 11) {

  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("scales",  quietly = TRUE))
  uncertainty <- match.arg(uncertainty)

  df <- as.data.frame(summary_long)
  df$Time      <- as.numeric(df$Time)
  df$Threshold <- as.numeric(df$Threshold)
  horizons     <- sort(unique(as.numeric(horizons)))

  df <- df[df$Time %in% horizons & df$Metric %in% metrics &
           df$Risk %in% outcomes & df$Model %in% panel_models, , drop = FALSE]
  if (!nrow(df)) stop("Sin filas tras filtrar summary_long.", call. = FALSE)

  # Factores ordenados para facetas y leyenda
  df$Horizon  <- factor(.thr_time_labels(df$Time),
                        levels = .thr_time_labels(horizons))
  df$Metric_f <- factor(df$Metric, levels = metrics,
                        labels = .thr_metric_labels[metrics])
  df$Model_f  <- factor(df$Model, levels = panel_models, labels = panel_titles)

  # Paleta azul secuencial: clara = corto plazo, oscura = largo plazo.
  # Colorblind-safe y legible en B/N (luminancia monótona).
  blues <- c("#9ECAE1", "#4292C6", "#2171B5", "#08306B")
  pal   <- setNames(blues[seq_len(nlevels(df$Horizon))], levels(df$Horizon))

  thr_breaks <- sort(unique(df$Threshold))

  build_one <- function(d) {
    p <- ggplot2::ggplot(
      d, ggplot2::aes(Threshold, mean, color = Horizon, fill = Horizon,
                      group = Horizon))

    if (uncertainty == "ribbon")
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pmax(q025, 0), ymax = pmin(q975, 1)),
        alpha = 0.12, color = NA)
    if (uncertainty == "errorbar")
      p <- p + ggplot2::geom_linerange(
        ggplot2::aes(ymin = pmax(q025, 0), ymax = pmin(q975, 1)),
        linewidth = 0.3, alpha = 0.5)

    p +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::facet_grid(Model_f ~ Metric_f, switch = "y") +
      ggplot2::scale_x_continuous(
        breaks = thr_breaks,
        labels = scales::label_percent(accuracy = 1),
        expand = ggplot2::expansion(mult = 0.04)) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1), breaks = seq(0, 1, 0.2),
        labels = scales::label_percent(accuracy = 1),
        expand = ggplot2::expansion(mult = 0.02)) +
      ggplot2::scale_color_manual(values = pal, name = "Horizon") +
      ggplot2::scale_fill_manual(values = pal,  name = "Horizon") +
      ggplot2::labs(x = "Risk threshold", y = NULL) +
      ggplot2::theme_classic(base_size = 10, base_family = base_family) +
      ggplot2::theme(
        legend.position   = "bottom",
        legend.key.width  = grid::unit(1.1, "lines"),
        legend.title      = ggplot2::element_text(face = "bold"),
        axis.title.x      = ggplot2::element_text(face = "bold",
                                                  margin = ggplot2::margin(t = 4)),
        axis.text         = ggplot2::element_text(color = "black"),
        axis.text.x       = ggplot2::element_text(size = 7.5),
        axis.line         = ggplot2::element_line(linewidth = 0.3),
        axis.ticks        = ggplot2::element_line(linewidth = 0.3),
        strip.background  = ggplot2::element_blank(),
        strip.placement   = "outside",
        strip.text        = ggplot2::element_text(face = "bold"),
        panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                                   linewidth = 0.3),
        panel.spacing.x   = grid::unit(0.6, "lines"),
        panel.spacing.y   = grid::unit(0.5, "lines"))
  }

  outcomes <- as.character(outcomes)
  res <- setNames(vector("list", length(outcomes)), outcomes)
  for (oc in outcomes) {
    d <- df[df$Risk == oc, , drop = FALSE]
    if (!nrow(d)) next
    pl <- build_one(d)
    res[[oc]] <- list(plot = pl, data = d)
    if (isTRUE(save_files)) {
      output_dir <- output_dir %||% getwd()
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      slug <- tolower(gsub("[^A-Za-z0-9]+", "_", oc))
      for (ext in c("png", "pdf"))
        ggplot2::ggsave(file.path(output_dir, sprintf("%s_%s.%s", prefix, slug, ext)),
                        pl, width = width_cm, height = height_cm, units = "cm",
                        dpi = 600,
                        device = if (ext == "pdf") grDevices::cairo_pdf else NULL)
    }
  }
  list(outcomes = res)
}
