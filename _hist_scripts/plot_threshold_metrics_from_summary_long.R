# Plot threshold-based Cox metrics from `summary_long`
#
# Intended input:
# - `cox_threshold_out$summary_long`, produced by
#   `run_threshold_metrics_from_results_boot()`
#
# Figure layout:
# - Panel A: full_model
# - Panel B: spline_model
# - Within each panel: rows = outcome, columns = metric
# - Points/ranges = mean and empirical 2.5th to 97.5th percentiles
#
# Usage:
# source("cons/_hist_scripts/plot_threshold_metrics_from_summary_long.R")
# fig_out <- plot_threshold_metrics_from_summary_long(
#   summary_long = cox_threshold_out$summary_long,
#   horizons = c(6, 12, 36, 60),
#   save_files = FALSE
# )

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

.extract_summary_long_ptm <- function(summary_long = NULL, threshold_object = NULL) {
    if (!is.null(summary_long)) {
        return(as.data.frame(summary_long))
    }

    if (!is.null(threshold_object)) {
        if (!is.list(threshold_object) || !"summary_long" %in% names(threshold_object)) {
            stop("`threshold_object` must be a list containing `summary_long`.", call. = FALSE)
        }
        return(as.data.frame(threshold_object$summary_long))
    }

    stop("Provide either `summary_long` or `threshold_object`.", call. = FALSE)
}

.assert_summary_long_ptm <- function(summary_long) {
    required_cols <- c("Model", "Risk", "Time", "Threshold", "Metric", "mean", "q025", "q975")
    missing_cols <- setdiff(required_cols, names(summary_long))
    if (length(missing_cols)) {
        stop(
            "Missing required columns in `summary_long`: ",
            paste(missing_cols, collapse = ", "),
            call. = FALSE
        )
    }
    invisible(TRUE)
}

.default_output_dir_ptm <- function(prefix = "threshold_metrics_panel") {
    root <- tryCatch(here::here(), error = function(e) getwd())
    cons_dir <- if (basename(root) == "cons") root else file.path(root, "cons")
    out_root <- file.path(cons_dir, "_out")
    if (!dir.exists(out_root)) {
        out_root <- file.path(getwd(), "_out")
    }
    file.path(out_root, sprintf("%s_%s", prefix, format(Sys.time(), "%Y%m%d_%H%M%S")))
}

.make_threshold_label_ptm <- function(x) {
    vapply(as.numeric(x), function(val) {
        digits <- if (abs(val * 100 - round(val * 100)) < 1e-8) 0 else 1
        paste0(formatC(100 * val, format = "f", digits = digits), "%")
    }, character(1))
}

.make_time_labels_ptm <- function(horizons) {
    vapply(as.numeric(horizons), function(hh) {
        if (hh >= 12 && hh %% 12 == 0) {
            yy <- hh / 12
            paste0(yy, " y")
        } else {
            paste0(hh, " mo")
        }
    }, character(1))
}

.plot_one_model_ptm <- function(df, horizons, metrics, outcomes, base_family, show_x_text = TRUE) {
    dodge <- ggplot2::position_dodge(width = 0.55)
    threshold_levels <- levels(df$Threshold_label)
    color_values <- setNames(scales::hue_pal(h = c(15, 375), c = 100, l = 45)(length(threshold_levels)), threshold_levels)
    facet_spec <- if (length(unique(df$Risk_f)) == 1L) {
        ggplot2::facet_grid(
            . ~ Metric_f,
            scales = "fixed",
            switch = "x"
        )
    } else {
        ggplot2::facet_grid(
            Risk_f ~ Metric_f,
            scales = "fixed",
            switch = "y"
        )
    }

    p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
            x = Time_f,
            y = mean,
            ymin = pmax(q025, 0),
            ymax = pmin(q975, 1),
            color = Threshold_label,
            shape = Threshold_label
        )
        ) +
        ggplot2::geom_hline(yintercept = seq(0, 1, by = 0.2), linewidth = 0.2, color = "grey92") +
        ggplot2::geom_errorbar(
            position = dodge,
            linewidth = 0.28,
            width = 0.14
        ) +
        ggplot2::geom_point(
            position = dodge,
            size = 3
        ) +
        facet_spec +
        ggplot2::scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, by = 0.2),
            labels = scales::label_percent(accuracy = 1)
        ) +
        ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
        ggplot2::guides(
            color = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
            shape = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
        ) +
        ggplot2::labs(
            x = NULL,
            y = NULL,
            color = "Threshold",
            shape = "Threshold"
        ) +
        ggplot2::theme_classic(base_size = 11, base_family = base_family) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title = ggplot2::element_text(face = "bold"),
            strip.background = ggplot2::element_blank(),
            strip.placement = "outside",
            strip.text = ggplot2::element_text(face = "bold", margin = ggplot2::margin(1, 1, 1, 1)),
            axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.8, margin = ggplot2::margin(t = 1)),
            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 1)),
            axis.title = ggplot2::element_blank(),
            panel.spacing.x = grid::unit(0.45, "lines"),
            panel.spacing.y = grid::unit(0.30, "lines"),
            plot.margin = ggplot2::margin(2, 3, 2, 3)
        )

    if (!isTRUE(show_x_text)) {
        p <- p + ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        )
    }

    p
}

combine_threshold_metric_panels <- function(
    model_plots,
    base_family = "Times New Roman",
    figure_title = NULL,
    figure_caption = NULL
) {
    if (!length(model_plots)) {
        stop("`model_plots` must contain at least one plot.", call. = FALSE)
    }

    annotation_args <- list(tag_levels = "A")
    if (!is.null(figure_title) && nzchar(figure_title)) {
        annotation_args$title <- figure_title
    }
    if (!is.null(figure_caption) && nzchar(figure_caption)) {
        annotation_args$caption <- figure_caption
    }

    patchwork::wrap_plots(model_plots, ncol = 1, guides = "collect") +
        do.call(patchwork::plot_annotation, annotation_args) &
        ggplot2::theme(
            legend.position = "bottom",
            legend.justification = "center",
            plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0),
            plot.caption = ggplot2::element_text(size = 9, hjust = 0),
            plot.tag = ggplot2::element_text(face = "bold", size = 16, family = base_family),
            plot.tag.position = c(0.01, 0.995)
        )
}

plot_threshold_metrics_from_summary_long <- function(
    summary_long = NULL,
    threshold_object = NULL,
    horizons = c(12, 36, 60),
    panel_models = c("full_model", "spline_model"),
    panel_titles = c("", ""),
    metrics = c("NPV", "PPV", "Sens", "Spec", "F1"),
    outcomes = c("Readmission", "Death"),
    output_dir = NULL,
    prefix = "threshold_metrics_panel",
    base_family = "Times New Roman",
    width_cm = 25,
    height_cm = 16,
    figure_title = NULL,
    figure_caption = NULL,
    save_files = FALSE
) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package `ggplot2` is required.", call. = FALSE)
    }
    if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("Package `patchwork` is required.", call. = FALSE)
    }
    if (!requireNamespace("scales", quietly = TRUE)) {
        stop("Package `scales` is required.", call. = FALSE)
    }

    summary_long <- .extract_summary_long_ptm(summary_long = summary_long, threshold_object = threshold_object)
    .assert_summary_long_ptm(summary_long)

    horizons <- sort(unique(as.numeric(horizons)))
    if (any(!is.finite(horizons))) {
        stop("`horizons` must be finite numeric values.", call. = FALSE)
    }
    if (length(panel_models) != 2L || length(panel_titles) != 2L) {
        stop("`panel_models` and `panel_titles` must each have length 2.", call. = FALSE)
    }

    plot_df <- summary_long
    plot_df$Time <- as.numeric(plot_df$Time)
    plot_df$Threshold <- as.numeric(plot_df$Threshold)

    plot_df <- plot_df[
        plot_df$Time %in% horizons &
            plot_df$Metric %in% metrics &
            plot_df$Risk %in% outcomes &
            plot_df$Model %in% panel_models,
        ,
        drop = FALSE
    ]

    if (!nrow(plot_df)) {
        stop("No rows remain after filtering `summary_long` to the requested horizons/models/metrics/outcomes.", call. = FALSE)
    }

    plot_df$Time_f <- factor(plot_df$Time, levels = horizons, labels = .make_time_labels_ptm(horizons))
    plot_df$Metric_f <- factor(plot_df$Metric, levels = metrics)
    plot_df$Risk_f <- factor(plot_df$Risk, levels = outcomes)
    plot_df$Threshold_label <- factor(
        .make_threshold_label_ptm(plot_df$Threshold),
        levels = unique(.make_threshold_label_ptm(sort(unique(plot_df$Threshold))))
    )

    plot_list <- vector("list", length(panel_models))
    names(plot_list) <- panel_models
    for (ii in seq_along(panel_models)) {
        model_name <- panel_models[ii]
        model_df <- plot_df[plot_df$Model == model_name, , drop = FALSE]
        if (!nrow(model_df)) {
            stop("No rows found for model `", model_name, "` after filtering.", call. = FALSE)
        }

        plot_list[[ii]] <- .plot_one_model_ptm(
            df = model_df,
            horizons = horizons,
            metrics = metrics,
            outcomes = outcomes,
            base_family = base_family,
            show_x_text = ii == length(panel_models)
        )
    }

    combined_plot <- combine_threshold_metric_panels(
        model_plots = plot_list,
        base_family = base_family,
        figure_title = figure_title,
        figure_caption = figure_caption
    )

    output_dir <- NULL
    png_path <- NA_character_
    pdf_path <- NA_character_
    filtered_csv_path <- NA_character_
    rds_path <- NA_character_

    if (isTRUE(save_files)) {
        output_dir <- output_dir %||% .default_output_dir_ptm(prefix = prefix)
        if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        }

        png_path <- file.path(output_dir, sprintf("%s.png", prefix))
        pdf_path <- file.path(output_dir, sprintf("%s.pdf", prefix))
        filtered_csv_path <- file.path(output_dir, sprintf("%s_filtered_summary_long.csv", prefix))
        rds_path <- file.path(output_dir, sprintf("%s_plot.rds", prefix))

        utils::write.csv(plot_df, filtered_csv_path, row.names = FALSE)
        ggplot2::ggsave(
            filename = png_path,
            plot = combined_plot,
            width = width_cm,
            height = height_cm,
            units = "cm",
            dpi = 600
        )
        ggplot2::ggsave(
            filename = pdf_path,
            plot = combined_plot,
            width = width_cm,
            height = height_cm,
            units = "cm",
            device = grDevices::cairo_pdf
        )
    }

    result <- list(
        plot = combined_plot,
        model_plots = plot_list,
        data = plot_df,
        config = list(
            horizons = horizons,
            panel_models = panel_models,
            panel_titles = panel_titles,
            metrics = metrics,
            outcomes = outcomes,
            save_files = isTRUE(save_files),
            files = list(
                png = png_path,
                pdf = pdf_path,
                filtered_summary_long = filtered_csv_path,
                rds = rds_path
            )
        )
    )
    if (isTRUE(save_files)) {
        saveRDS(result, rds_path)
    }
    result
}

plot_threshold_metrics_separate_outcomes <- function(
    summary_long = NULL,
    threshold_object = NULL,
    horizons = c(12, 36, 60),
    panel_models = c("full_model", "spline_model"),
    panel_titles = c("", ""),
    metrics = c("NPV", "PPV", "Sens", "Spec", "F1"),
    outcomes = c("Readmission", "Death"),
    output_dir = NULL,
    prefix = "threshold_metrics_by_outcome",
    base_family = "Times New Roman",
    width_cm = 24,
    height_cm = 10,
    save_files = FALSE
) {
    summary_long <- .extract_summary_long_ptm(summary_long = summary_long, threshold_object = threshold_object)
    .assert_summary_long_ptm(summary_long)

    output_dir <- output_dir %||% .default_output_dir_ptm(prefix = prefix)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    outcomes <- as.character(outcomes)
    results <- vector("list", length(outcomes))
    names(results) <- outcomes

    for (outcome_name in outcomes) {
        outcome_slug <- tolower(gsub("[^A-Za-z0-9]+", "_", outcome_name))
        outcome_prefix <- sprintf("%s_%s", prefix, outcome_slug)

        res <- plot_threshold_metrics_from_summary_long(
            summary_long = summary_long,
            horizons = horizons,
            panel_models = panel_models,
            panel_titles = panel_titles,
            metrics = metrics,
            outcomes = outcome_name,
            output_dir = output_dir,
            prefix = outcome_prefix,
            base_family = base_family,
            width_cm = width_cm,
            height_cm = height_cm,
            figure_title = NULL,
            figure_caption = NULL,
            save_files = save_files
        )

        results[[outcome_name]] <- res
    }

    out <- list(
        outcomes = results,
        files = list(index = NA_character_)
    )

    if (isTRUE(save_files)) {
        index_lines <- c(
            "Separate threshold-metric figures by outcome",
            ""
        )

        for (outcome_name in names(results)) {
            files <- results[[outcome_name]]$config$files
            index_lines <- c(
                index_lines,
                paste0(outcome_name, ":"),
                paste0("PNG: ", files$png),
                paste0("PDF: ", files$pdf),
                paste0("CSV: ", files$filtered_summary_long),
                ""
            )
        }

        index_path <- file.path(output_dir, sprintf("%s_index.txt", prefix))
        writeLines(index_lines, con = index_path, useBytes = TRUE)
        out$files$index <- index_path
        saveRDS(out, file.path(output_dir, sprintf("%s_results.rds", prefix)))
    }

    out
}
