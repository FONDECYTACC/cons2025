# Survival decision curve analysis from `results_boot`
#
# Reference:
# McLernon DJ, Giardiello D, Van Calster B, et al. Assessing performance and
# clinical usefulness in prediction models with survival outcomes: practical
# guidance for Cox proportional hazards models. Ann Intern Med. 2023;176:105-114.
# doi:10.7326/M22-0844
#
# Key implementation choice:
# McLernon et al. recommend evaluating net benefit at fixed follow-up horizons
# over a range of clinically defendable threshold probabilities. Decision curve
# analysis should not be used to "discover" one universally optimal threshold.
# When no external clinical threshold is available, this script therefore:
#   1) computes DCA on a broad default grid (1% to 50%), and
#   2) reports a horizon-specific focus window anchored on the observed event
#      risk at that horizon for quick review only.
# That focus window is a project heuristic, not a paper-derived threshold.
#
# Usage:
# source("cons/_hist_scripts/adca_from_results_boot.R")
# adca_out <- run_adca_from_results_boot(results_boot)

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

.assert_results_boot <- function(results_boot) {
    if (!is.list(results_boot)) {
        stop("`results_boot` must be a list.", call. = FALSE)
    }
    if (!"raw_predictions" %in% names(results_boot)) {
        stop("`results_boot` does not contain `raw_predictions`.", call. = FALSE)
    }
    if (!length(results_boot$raw_predictions)) {
        stop("`results_boot$raw_predictions` is empty.", call. = FALSE)
    }
    invisible(TRUE)
}

.load_results_boot_object <- function(path, object_name = "results_boot") {
    if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
        stop("`path` must be a non-empty string.", call. = FALSE)
    }
    if (!file.exists(path)) {
        stop("File does not exist: ", path, call. = FALSE)
    }

    ext <- tolower(tools::file_ext(path))
    if (ext == "rds") {
        return(readRDS(path))
    }

    env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = env)
    if (!object_name %in% loaded) {
        stop(
            "Object `", object_name, "` was not found in ", path, ". Loaded: ",
            paste(loaded, collapse = ", "),
            call. = FALSE
        )
    }
    get(object_name, envir = env)
}

.normalize_horizons <- function(horizons, available_horizons) {
    available_horizons <- sort(unique(as.numeric(available_horizons)))
    if (is.null(horizons)) {
        return(available_horizons)
    }
    horizons <- sort(unique(as.numeric(horizons)))
    if (any(!is.finite(horizons))) {
        stop("`horizons` must be finite numeric values.", call. = FALSE)
    }
    missing_h <- setdiff(horizons, available_horizons)
    if (length(missing_h)) {
        stop(
            "Requested horizons are not available in `results_boot`: ",
            paste(missing_h, collapse = ", "),
            call. = FALSE
        )
    }
    horizons
}

.normalize_thresholds <- function(thresholds) {
    thresholds <- sort(unique(as.numeric(thresholds)))
    if (length(thresholds) < 2L) {
        stop("Need at least two threshold values.", call. = FALSE)
    }
    if (any(!is.finite(thresholds)) || any(thresholds <= 0) || any(thresholds >= 1)) {
        stop("Thresholds must be finite probabilities strictly between 0 and 1.", call. = FALSE)
    }
    thresholds
}

.safe_surv_summary <- function(fit, horizon) {
    s <- summary(fit, times = horizon, extend = TRUE)
    surv <- s$surv[1]
    if (!length(surv) || !is.finite(surv)) {
        return(NA_real_)
    }
    surv
}

.km_event_risk <- function(time, event, horizon) {
    keep <- is.finite(time) & !is.na(event)
    time <- as.numeric(time[keep])
    event <- as.integer(event[keep])

    if (!length(time)) {
        return(NA_real_)
    }
    fit <- survival::survfit(survival::Surv(time, event) ~ 1)
    1 - .safe_surv_summary(fit, horizon)
}

.net_benefit <- function(event_risk, positive_rate, threshold) {
    if (!is.finite(event_risk) || !is.finite(positive_rate)) {
        return(NA_real_)
    }
    odds_threshold <- threshold / (1 - threshold)
    positive_rate * (event_risk - (1 - event_risk) * odds_threshold)
}

.standardized_net_benefit <- function(net_benefit, prevalence) {
    if (!is.finite(prevalence) || prevalence <= 0) {
        return(NA_real_)
    }
    net_benefit / prevalence
}

.interventions_avoided <- function(model_nb, treat_all_nb, threshold) {
    odds_threshold <- threshold / (1 - threshold)
    if (!is.finite(odds_threshold) || odds_threshold <= 0) {
        return(NA_real_)
    }
    (model_nb - treat_all_nb) / odds_threshold * 100
}

.extract_replicates <- function(results_boot, risk = c("readmission", "death")) {
    risk <- match.arg(risk)
    raw_predictions <- results_boot$raw_predictions
    blocks <- vector("list", length(raw_predictions))
    block_id <- 0L

    for (item in raw_predictions) {
        if (!risk %in% names(item)) {
            next
        }
        block <- item[[risk]]
        if (!is.list(block)) {
            next
        }
        if ("error" %in% names(block) && nzchar(block$error %||% "")) {
            next
        }

        eval_times <- as.numeric(item$eval_times %||% results_boot$config$eval_times)
        surv_mat <- as.matrix(block$surv_val_matrix)
        y_val <- block$y_val

        if (!is.matrix(surv_mat) || !nrow(surv_mat) || !ncol(surv_mat)) {
            next
        }
        if (ncol(surv_mat) != length(eval_times)) {
            stop(
                "Mismatch between survival matrix columns and evaluation times for ",
                risk, ".", call. = FALSE
            )
        }
        if (!all(c("time", "event") %in% names(y_val))) {
            stop("`y_val` must contain `time` and `event`.", call. = FALSE)
        }
        if (nrow(y_val) != nrow(surv_mat)) {
            stop("Mismatch between `y_val` rows and `surv_val_matrix` rows.", call. = FALSE)
        }

        block_id <- block_id + 1L
        blocks[[block_id]] <- list(
            replicate_id = block_id,
            imp_idx = item$imp_idx %||% NA_integer_,
            fold_idx = item$fold_idx %||% NA_integer_,
            risk = risk,
            eval_times = eval_times,
            time = as.numeric(y_val$time),
            event = as.integer(y_val$event),
            pred_risk = 1 - surv_mat,
            dropped_rows = block$dropped_rows %||% NA_integer_
        )
    }

    blocks <- Filter(Negate(is.null), blocks)
    if (!length(blocks)) {
        stop("No usable raw prediction blocks were found for `", risk, "`.", call. = FALSE)
    }
    blocks
}

.compute_replicate_curves <- function(block, thresholds, horizons) {
    out <- vector("list", length(horizons) * length(thresholds))
    idx <- 0L

    time <- block$time
    event <- block$event
    eval_times <- block$eval_times

    for (horizon in horizons) {
        h_idx <- which(eval_times == horizon)
        if (!length(h_idx)) {
            next
        }

        risk_vec <- block$pred_risk[, h_idx[1]]
        keep <- is.finite(risk_vec) & is.finite(time) & !is.na(event)
        if (!any(keep)) {
            next
        }

        time_h <- time[keep]
        event_h <- event[keep]
        risk_h <- as.numeric(risk_vec[keep])
        n_obs <- length(risk_h)
        all_event_risk <- .km_event_risk(time_h, event_h, horizon)

        for (threshold in thresholds) {
            selected <- risk_h >= threshold
            positive_rate <- mean(selected)

            model_event_risk <- if (any(selected)) {
                .km_event_risk(time_h[selected], event_h[selected], horizon)
            } else {
                0
            }

            model_nb <- .net_benefit(
                event_risk = model_event_risk,
                positive_rate = positive_rate,
                threshold = threshold
            )
            treat_all_nb <- .net_benefit(
                event_risk = all_event_risk,
                positive_rate = 1,
                threshold = threshold
            )
            treat_none_nb <- 0

            idx <- idx + 1L
            out[[idx]] <- data.frame(
                risk = block$risk,
                replicate_id = block$replicate_id,
                imp_idx = block$imp_idx,
                fold_idx = block$fold_idx,
                horizon = horizon,
                threshold = threshold,
                n_obs = n_obs,
                positive_rate = positive_rate,
                observed_event_risk = all_event_risk,
                observed_event_risk_positive = model_event_risk,
                nb_model = model_nb,
                nb_treat_all = treat_all_nb,
                nb_treat_none = treat_none_nb,
                std_nb_model = .standardized_net_benefit(model_nb, all_event_risk),
                std_nb_treat_all = .standardized_net_benefit(treat_all_nb, all_event_risk),
                interventions_avoided = .interventions_avoided(model_nb, treat_all_nb, threshold),
                stringsAsFactors = FALSE
            )
        }
    }

    out <- Filter(Negate(is.null), out)
    if (!length(out)) {
        return(data.frame())
    }
    do.call(rbind, out)
}

.collapse_curves_long <- function(raw_curves) {
    pieces <- list(
        data.frame(
            risk = raw_curves$risk,
            replicate_id = raw_curves$replicate_id,
            imp_idx = raw_curves$imp_idx,
            fold_idx = raw_curves$fold_idx,
            horizon = raw_curves$horizon,
            threshold = raw_curves$threshold,
            strategy = "Model",
            net_benefit = raw_curves$nb_model,
            standardized_net_benefit = raw_curves$std_nb_model,
            interventions_avoided = raw_curves$interventions_avoided,
            observed_event_risk = raw_curves$observed_event_risk,
            stringsAsFactors = FALSE
        ),
        data.frame(
            risk = raw_curves$risk,
            replicate_id = raw_curves$replicate_id,
            imp_idx = raw_curves$imp_idx,
            fold_idx = raw_curves$fold_idx,
            horizon = raw_curves$horizon,
            threshold = raw_curves$threshold,
            strategy = "Treat all",
            net_benefit = raw_curves$nb_treat_all,
            standardized_net_benefit = raw_curves$std_nb_treat_all,
            interventions_avoided = NA_real_,
            observed_event_risk = raw_curves$observed_event_risk,
            stringsAsFactors = FALSE
        ),
        data.frame(
            risk = raw_curves$risk,
            replicate_id = raw_curves$replicate_id,
            imp_idx = raw_curves$imp_idx,
            fold_idx = raw_curves$fold_idx,
            horizon = raw_curves$horizon,
            threshold = raw_curves$threshold,
            strategy = "Treat none",
            net_benefit = raw_curves$nb_treat_none,
            standardized_net_benefit = 0,
            interventions_avoided = NA_real_,
            observed_event_risk = raw_curves$observed_event_risk,
            stringsAsFactors = FALSE
        )
    )
    do.call(rbind, pieces)
}

.summarize_metric <- function(x) {
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    if (!length(x)) {
        return(c(mean = NA_real_, q025 = NA_real_, q975 = NA_real_, n = 0))
    }
    c(
        mean = mean(x),
        q025 = as.numeric(stats::quantile(x, probs = 0.025, names = FALSE, na.rm = TRUE)),
        q975 = as.numeric(stats::quantile(x, probs = 0.975, names = FALSE, na.rm = TRUE)),
        n = length(x)
    )
}

.summarize_curves <- function(curves_long) {
    split_key <- interaction(
        curves_long$risk,
        curves_long$horizon,
        curves_long$threshold,
        curves_long$strategy,
        drop = TRUE,
        lex.order = TRUE
    )

    groups <- split(curves_long, split_key)
    summary_list <- lapply(groups, function(df) {
        nb <- .summarize_metric(df$net_benefit)
        std_nb <- .summarize_metric(df$standardized_net_benefit)
        ia <- .summarize_metric(df$interventions_avoided)
        evt <- .summarize_metric(df$observed_event_risk)

        data.frame(
            risk = df$risk[1],
            horizon = df$horizon[1],
            threshold = df$threshold[1],
            strategy = df$strategy[1],
            net_benefit_mean = nb["mean"],
            net_benefit_q025 = nb["q025"],
            net_benefit_q975 = nb["q975"],
            net_benefit_n = nb["n"],
            standardized_nb_mean = std_nb["mean"],
            standardized_nb_q025 = std_nb["q025"],
            standardized_nb_q975 = std_nb["q975"],
            interventions_avoided_mean = ia["mean"],
            interventions_avoided_q025 = ia["q025"],
            interventions_avoided_q975 = ia["q975"],
            observed_event_risk_mean = evt["mean"],
            observed_event_risk_q025 = evt["q025"],
            observed_event_risk_q975 = evt["q975"],
            stringsAsFactors = FALSE
        )
    })

    out <- do.call(rbind, summary_list)
    out[order(out$risk, out$horizon, out$strategy, out$threshold), , drop = FALSE]
}

.infer_focus_windows <- function(summary_curves, thresholds) {
    step <- stats::median(diff(thresholds))
    model_ref <- summary_curves[summary_curves$strategy == "Model", , drop = FALSE]
    split_key <- interaction(model_ref$risk, model_ref$horizon, drop = TRUE, lex.order = TRUE)
    groups <- split(model_ref, split_key)

    out <- lapply(groups, function(df) {
        event_risk <- unique(df$observed_event_risk_mean)
        event_risk <- event_risk[is.finite(event_risk)]
        event_risk <- if (length(event_risk)) mean(event_risk) else NA_real_

        if (!is.finite(event_risk) || event_risk <= 0) {
            lower <- min(thresholds)
            upper <- min(max(thresholds), max(0.10, lower + 9 * step))
        } else {
            lower <- max(min(thresholds), floor((0.5 * event_risk) / step) * step)
            upper <- min(max(thresholds), ceiling((2.0 * event_risk) / step) * step)
            upper <- max(upper, lower + step)
        }

        data.frame(
            risk = df$risk[1],
            horizon = df$horizon[1],
            observed_event_risk_mean = event_risk,
            focus_lower = round(lower, 6),
            focus_upper = round(upper, 6),
            rationale = "Heuristic fallback: 0.5x to 2.0x the observed event risk at the horizon, bounded by the global threshold grid.",
            stringsAsFactors = FALSE
        )
    })

    out <- do.call(rbind, out)
    out[order(out$risk, out$horizon), , drop = FALSE]
}

.default_output_dir <- function(prefix = "results_boot_adca") {
    root <- tryCatch(here::here(), error = function(e) getwd())
    cons_dir <- if (basename(root) == "cons") root else file.path(root, "cons")
    out_root <- file.path(cons_dir, "_out")
    if (!dir.exists(out_root)) {
        out_root <- file.path(getwd(), "_out")
    }
    file.path(out_root, sprintf("%s_%s", prefix, format(Sys.time(), "%Y%m%d_%H%M%S")))
}

.plot_adca <- function(summary_curves, focus_windows, output_file) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        warning("`ggplot2` is not installed; skipping plot creation.", call. = FALSE)
        return(invisible(NULL))
    }

    plot_df <- summary_curves
    plot_df$risk_label <- ifelse(plot_df$risk == "death", "Death", "Readmission")
    plot_df$horizon_label <- paste0(plot_df$horizon, " months")
    plot_df$strategy <- factor(plot_df$strategy, levels = c("Model", "Treat all", "Treat none"))

    focus_df <- focus_windows
    focus_df$risk_label <- ifelse(focus_df$risk == "death", "Death", "Readmission")
    focus_df$horizon_label <- paste0(focus_df$horizon, " months")

    model_ribbon <- plot_df[plot_df$strategy == "Model", , drop = FALSE]

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = threshold, y = net_benefit_mean, color = strategy)) +
        ggplot2::geom_rect(
            data = focus_df,
            ggplot2::aes(
                xmin = focus_lower,
                xmax = focus_upper,
                ymin = -Inf,
                ymax = Inf
            ),
            inherit.aes = FALSE,
            fill = "grey95",
            color = NA
        ) +
        ggplot2::geom_hline(yintercept = 0, linetype = 3, linewidth = 0.3, color = "grey60") +
        ggplot2::geom_ribbon(
            data = model_ribbon,
            ggplot2::aes(
                ymin = net_benefit_q025,
                ymax = net_benefit_q975,
                fill = strategy
            ),
            alpha = 0.12,
            color = NA,
            show.legend = FALSE
        ) +
        ggplot2::geom_line(linewidth = 0.7) +
        ggplot2::facet_grid(risk_label ~ horizon_label, scales = "free_y") +
        ggplot2::scale_x_continuous(
            labels = scales::label_percent(accuracy = 1),
            breaks = scales::breaks_pretty(n = 5)
        ) +
        ggplot2::scale_color_manual(
            values = c("Model" = "#1b4d6b", "Treat all" = "#b03a2e", "Treat none" = "#666666")
        ) +
        ggplot2::scale_fill_manual(values = c("Model" = "#1b4d6b")) +
        ggplot2::labs(
            x = "Threshold probability",
            y = "Net benefit",
            color = NULL,
            title = "Time-specific survival decision curve analysis from results_boot",
            subtitle = "Grey band = heuristic focus window when no external clinical threshold was provided"
        ) +
        ggplot2::theme_bw(base_size = 10) +
        ggplot2::theme(
            legend.position = "bottom",
            strip.background = ggplot2::element_rect(fill = "grey98", color = "grey85"),
            panel.grid.minor = ggplot2::element_blank()
        )

    ggplot2::ggsave(
        filename = output_file,
        plot = p,
        width = 15,
        height = 8,
        units = "in",
        dpi = 300
    )
    invisible(p)
}

run_adca_from_results_boot <- function(
    results_boot = NULL,
    results_boot_path = NULL,
    thresholds = seq(0.01, 0.50, by = 0.01),
    horizons = NULL,
    output_dir = NULL,
    prefix = "results_boot_adca",
    save_raw = TRUE,
    create_plot = TRUE
) {
    if (is.null(results_boot)) {
        if (is.null(results_boot_path)) {
            stop(
                "Provide either `results_boot` or `results_boot_path`.",
                call. = FALSE
            )
        }
        results_boot <- .load_results_boot_object(results_boot_path)
    }

    .assert_results_boot(results_boot)
    thresholds <- .normalize_thresholds(thresholds)

    available_horizons <- results_boot$config$eval_times %||%
        unique(unlist(lapply(results_boot$raw_predictions, `[[`, "eval_times")))
    horizons <- .normalize_horizons(horizons, available_horizons)

    readmission_blocks <- .extract_replicates(results_boot, risk = "readmission")
    death_blocks <- .extract_replicates(results_boot, risk = "death")
    all_blocks <- c(readmission_blocks, death_blocks)

    raw_curves_list <- lapply(all_blocks, .compute_replicate_curves, thresholds = thresholds, horizons = horizons)
    raw_curves <- do.call(rbind, Filter(nrow, raw_curves_list))
    if (!nrow(raw_curves)) {
        stop("No DCA curves could be computed from `results_boot`.", call. = FALSE)
    }

    curves_long <- .collapse_curves_long(raw_curves)
    summary_curves <- .summarize_curves(curves_long)
    focus_windows <- .infer_focus_windows(summary_curves, thresholds = thresholds)

    summary_curves <- merge(
        summary_curves,
        focus_windows[, c("risk", "horizon", "focus_lower", "focus_upper")],
        by = c("risk", "horizon"),
        all.x = TRUE,
        sort = FALSE
    )
    summary_curves$in_focus_window <- with(
        summary_curves,
        threshold >= focus_lower & threshold <= focus_upper
    )
    summary_curves <- summary_curves[order(
        summary_curves$risk,
        summary_curves$horizon,
        summary_curves$strategy,
        summary_curves$threshold
    ), , drop = FALSE]

    output_dir <- output_dir %||% .default_output_dir(prefix = prefix)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    summary_path <- file.path(output_dir, sprintf("%s_summary.csv", prefix))
    thresholds_path <- file.path(output_dir, sprintf("%s_focus_windows.csv", prefix))
    rds_path <- file.path(output_dir, sprintf("%s_results.rds", prefix))
    raw_path <- file.path(output_dir, sprintf("%s_raw.csv", prefix))
    plot_path <- file.path(output_dir, sprintf("%s_plot.png", prefix))

    utils::write.csv(summary_curves, summary_path, row.names = FALSE)
    utils::write.csv(focus_windows, thresholds_path, row.names = FALSE)
    if (isTRUE(save_raw)) {
        utils::write.csv(curves_long, raw_path, row.names = FALSE)
    }
    if (isTRUE(create_plot)) {
        .plot_adca(summary_curves, focus_windows, plot_path)
    } else {
        plot_path <- NA_character_
    }

    result <- list(
        summary = summary_curves,
        focus_windows = focus_windows,
        raw = curves_long,
        config = list(
            thresholds = thresholds,
            horizons = horizons,
            reference = "McLernon et al. 2023; doi:10.7326/M22-0844",
            note = paste(
                "DCA was computed at fixed follow-up horizons over a broad threshold grid.",
                "The reported focus window is a project-specific heuristic fallback because no external clinical threshold was supplied."
            ),
            output_dir = output_dir,
            files = list(
                summary = summary_path,
                focus_windows = thresholds_path,
                raw = if (isTRUE(save_raw)) raw_path else NA_character_,
                plot = plot_path,
                rds = rds_path
            )
        )
    )
    saveRDS(result, rds_path)
    result
}
