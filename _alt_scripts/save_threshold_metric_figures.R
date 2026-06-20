# Save threshold-metric figures (clean / Epidemiology style)
#
# Works on the object returned by plot_threshold_metrics_clean():
#   list(outcomes = list(Readmission = list(plot = <ggplot>, data = <df>),
#                        Death       = list(plot = <ggplot>, data = <df>)))
#
# Writes one file per outcome per format, at publication resolution.
#
# Usage:
#   source(file.path(project_root, "cons/_alt_scripts/save_threshold_metric_figures.R"))
#   save_threshold_metric_figures(
#     figs_thr_metrics,
#     output_dir = file.path(project_root, "cons", "_out", "fig_threshold_clean"),
#     prefix     = "threshold_clean_primary",
#     formats    = c("png", "pdf", "tiff"))

`%||%` <- function(x, y) if (is.null(x)) y else x

#' @param figs       object from plot_threshold_metrics_clean() (or its $outcomes).
#' @param output_dir directory to write into (created if missing).
#' @param prefix     filename stem; outcome slug is appended (e.g. *_readmission.png).
#' @param width_cm,height_cm  figure size. Epidemiology: ~17.5 cm double column.
#' @param dpi        raster resolution (png/tiff). 600 for line/figure art.
#' @param formats    any of "png", "pdf", "tiff", "eps".
save_threshold_metric_figures <- function(
    figs, output_dir,
    prefix    = "threshold_metrics_clean",
    width_cm  = 19, height_cm = 11, dpi = 600,
    formats   = c("png", "pdf")) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package `ggplot2` is required.", call. = FALSE)
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Accept either the full figs object or figs$outcomes directly.
  outc <- figs$outcomes %||% figs
  saved <- character(0)

  for (oc in names(outc)) {
    pl <- outc[[oc]]$plot %||% outc[[oc]]
    if (!inherits(pl, "ggplot")) next
    slug <- tolower(gsub("[^A-Za-z0-9]+", "_", oc))

    for (fmt in formats) {
      fn  <- file.path(output_dir, sprintf("%s_%s.%s", prefix, slug, fmt))
      dev <- switch(fmt,
        pdf  = grDevices::cairo_pdf,
        eps  = grDevices::cairo_ps,
        tiff = function(filename, ...)                       # LZW-compressed TIFF
                 grDevices::tiff(filename, compression = "lzw", ...),
        NULL)                                                # png -> ggsave default

      args <- list(filename = fn, plot = pl,
                   width = width_cm, height = height_cm, units = "cm")
      if (!fmt %in% c("pdf", "eps")) args$dpi    <- dpi      # vector fmts ignore dpi
      if (!is.null(dev))             args$device <- dev

      tryCatch(
        do.call(ggplot2::ggsave, args),
        error = function(e)
          warning(sprintf("Could not write %s: %s", fn, conditionMessage(e)),
                  call. = FALSE))
      if (file.exists(fn)) saved <- c(saved, fn)
    }
  }

  message(sprintf("Saved %d file(s) to %s", length(saved), output_dir))
  invisible(saved)
}
