# Threshold-based classification metrics from paired `results_boot` Cox outputs
#
# This mirrors the XGBoost threshold evaluation logic:
# - outcome at horizon t is defined as event by t versus known event-free beyond t
# - subjects censored before t are excluded from the binary evaluation at t
# - fixed, externally chosen thresholds are applied; thresholds are not optimized here
#
# Usage:
# source("cons/_hist_scripts/threshold_metrics_from_results_boot.R")
# threshold_out <- run_threshold_metrics_from_results_boot(
#   results_boot,
#   results_boot_full,
#   reference_label = "spline_model",
#   updated_label = "full_model"
# )

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

.assert_results_boot_tm <- function(results_boot, arg_name) {
    if (!is.list(results_boot)) {
        stop("`", arg_name, "` must be a list.", call. = FALSE)
    }
    if (!"raw_predictions" %in% names(results_boot)) {
        stop("`", arg_name, "` does not contain `raw_predictions`.", call. = FALSE)
    }
    if (!length(results_boot$raw_predictions)) {
        stop("`", arg_name, "$raw_predictions` is empty.", call. = FALSE)
    }
    invisible(TRUE)
}

.default_threshold_spec <- function() {
    list(
        death = list(
            `12` = c(0.03, 0.05),
            `36` = c(0.05),
            `60` = c(0.03, 0.05)
        ),
        readmission = list(
            `12` = c(0.10, 0.15, 0.20),
            `36` = c(0.20, 0.25),
            `60` = c(0.20, 0.25)
        )
    )
}

.threshold_rationale_df <- function() {
    data.frame(
        risk = c("death", "death", "death", "readmission", "readmission", "readmission"),
        horizon = c(12, 36, 60, 12, 36, 60),
        rationale = c(
            "Low mortality threshold range because missed deaths were considered clinically costly.",
            "Five-percent mortality threshold retained as the pragmatic mid-term decision point.",
            "Low mortality threshold range retained; 5% was favored in the DCA narrative at 60 months.",
            "Pragmatic 10% to 20% readmission range for lower-burden to more intensive 12-month interventions.",
            "Higher 20% to 25% readmission range for more resource-sensitive longer-term interventions.",
            "Higher 20% to 25% readmission range for more resource-sensitive longer-term interventions."
        ),
        stringsAsFactors = FALSE
    )
}

.normalize_threshold_spec <- function(threshold_spec, available_horizons) {
    if (is.null(threshold_spec)) {
        threshold_spec <- .default_threshold_spec()
    }
    if (!is.list(threshold_spec)) {
        stop("`threshold_spec` must be a named list.", call. = FALSE)
    }

    out <- list(readmission = list(), death = list())
    for (risk_name in c("readmission", "death")) {
        risk_spec <- threshold_spec[[risk_name]]
        if (is.null(risk_spec)) {
            next
        }
        if (!is.list(risk_spec) || is.null(names(risk_spec))) {
            stop("Each risk entry in `threshold_spec` must be a named list keyed by horizon.", call. = FALSE)
        }

        horizons <- sort(unique(as.numeric(names(risk_spec))))
        if (any(!is.finite(horizons))) {
            stop("Threshold-spec horizon names must be numeric.", call. = FALSE)
        }
        missing_h <- setdiff(horizons, available_horizons)
        if (length(missing_h)) {
            stop(
                "Threshold horizons not available in both `results_boot` objects for ",
                risk_name, ": ", paste(missing_h, collapse = ", "),
                call. = FALSE
            )
        }

        risk_out <- vector("list", length(horizons))
        names(risk_out) <- as.character(horizons)
        for (hh in horizons) {
            vals <- sort(unique(as.numeric(risk_spec[[as.character(hh)]])))
            if (!length(vals) || any(!is.finite(vals)) || any(vals <= 0) || any(vals >= 1)) {
                stop(
                    "Thresholds for ", risk_name, " at horizon ", hh,
                    " must be finite probabilities in (0, 1).",
                    call. = FALSE
                )
            }
            risk_out[[as.character(hh)]] <- vals
        }
        out[[risk_name]] <- risk_out
    }

    if (!length(out$readmission) && !length(out$death)) {
        stop("`threshold_spec` does not contain any usable horizons.", call. = FALSE)
    }
    out
}

.threshold_spec_to_df <- function(threshold_spec) {
    rows <- list()
    idx <- 0L
    rationale_df <- .threshold_rationale_df()

    for (risk_name in names(threshold_spec)) {
        if (!length(threshold_spec[[risk_name]])) {
            next
        }
        for (horizon_name in names(threshold_spec[[risk_name]])) {
            horizon <- as.numeric(horizon_name)
            rationale <- rationale_df$rationale[
                rationale_df$risk == risk_name & rationale_df$horizon == horizon
            ]
            rationale <- if (length(rationale)) rationale[1] else NA_character_

            for (threshold in threshold_spec[[risk_name]][[horizon_name]]) {
                idx <- idx + 1L
                rows[[idx]] <- data.frame(
                    risk = risk_name,
                    horizon = horizon,
                    threshold = threshold,
                    rationale = rationale,
                    stringsAsFactors = FALSE
                )
            }
        }
    }

    out <- do.call(rbind, rows)
    out[order(out$risk, out$horizon, out$threshold), , drop = FALSE]
}

.extract_blocks_tm <- function(results_boot, risk = c("readmission", "death")) {
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
            pred_risk = 1 - surv_mat
        )
    }

    blocks <- Filter(Negate(is.null), blocks)
    if (!length(blocks)) {
        stop("No usable raw prediction blocks were found for `", risk, "`.", call. = FALSE)
    }
    blocks
}

.binary_target_at_horizon <- function(time, event, horizon) {
    is_case <- (as.integer(event) == 1L) & (as.numeric(time) <= horizon)
    is_control <- as.numeric(time) > horizon
    valid_mask <- is_case | is_control
    list(
        y_true = as.integer(is_case[valid_mask]),
        valid_mask = valid_mask
    )
}

.evaluate_with_threshold_tm <- function(y_true, probas, threshold) {
    y_pred <- as.integer(probas >= threshold)

    tn <- sum(y_true == 0L & y_pred == 0L)
    fp <- sum(y_true == 0L & y_pred == 1L)
    fn <- sum(y_true == 1L & y_pred == 0L)
    tp <- sum(y_true == 1L & y_pred == 1L)

    c(
        Sens = if ((tp + fn) > 0L) tp / (tp + fn) else 0,
        Spec = if ((tn + fp) > 0L) tn / (tn + fp) else 0,
        PPV = if ((tp + fp) > 0L) tp / (tp + fp) else 0,
        NPV = if ((tn + fn) > 0L) tn / (tn + fn) else 0,
        F1 = if ((2 * tp + fp + fn) > 0L) (2 * tp) / (2 * tp + fp + fn) else 0,
        TP = tp,
        FP = fp,
        FN = fn,
        TN = tn,
        n_valid = length(y_true),
        n_cases = sum(y_true == 1L),
        n_controls = sum(y_true == 0L),
        n_pred_positive = sum(y_pred == 1L),
        n_pred_negative = sum(y_pred == 0L)
    )
}

.evaluate_model_blocks <- function(blocks, model_label, threshold_spec) {
    rows <- list()
    idx <- 0L

    for (block in blocks) {
        risk_spec <- threshold_spec[[block$risk]]
        if (!length(risk_spec)) {
            next
        }

        for (horizon_name in names(risk_spec)) {
            horizon <- as.numeric(horizon_name)
            h_idx <- which(block$eval_times == horizon)
            if (!length(h_idx)) {
                next
            }

            pred_risk <- as.numeric(block$pred_risk[, h_idx[1]])
            target <- .binary_target_at_horizon(block$time, block$event, horizon)
            valid_mask <- target$valid_mask
            if (!any(valid_mask)) {
                next
            }

            y_true <- target$y_true
            probas <- pred_risk[valid_mask]
            thresholds <- risk_spec[[horizon_name]]

            for (threshold in thresholds) {
                metric_vals <- .evaluate_with_threshold_tm(y_true, probas, threshold)
                idx <- idx + 1L
                rows[[idx]] <- data.frame(
                    Model = model_label,
                    Risk = ifelse(block$risk == "death", "Death", "Readmission"),
                    horizon = horizon,
                    threshold = threshold,
                    Imp = block$imp_idx,
                    Fold = block$fold_idx,
                    replicate_id = block$replicate_id,
                    Sens = unname(metric_vals["Sens"]),
                    Spec = unname(metric_vals["Spec"]),
                    PPV = unname(metric_vals["PPV"]),
                    NPV = unname(metric_vals["NPV"]),
                    F1 = unname(metric_vals["F1"]),
                    TP = unname(metric_vals["TP"]),
                    FP = unname(metric_vals["FP"]),
                    FN = unname(metric_vals["FN"]),
                    TN = unname(metric_vals["TN"]),
                    n_valid = unname(metric_vals["n_valid"]),
                    n_cases = unname(metric_vals["n_cases"]),
                    n_controls = unname(metric_vals["n_controls"]),
                    n_pred_positive = unname(metric_vals["n_pred_positive"]),
                    n_pred_negative = unname(metric_vals["n_pred_negative"]),
                    stringsAsFactors = FALSE
                )
            }
        }
    }

    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) {
        return(data.frame())
    }
    out <- do.call(rbind, rows)
    out[order(out$Model, out$Risk, out$horizon, out$threshold, out$Imp, out$Fold), , drop = FALSE]
}

.to_metrics_long <- function(raw_wide) {
    metric_names <- c("Sens", "Spec", "PPV", "NPV", "F1")
    rows <- vector("list", nrow(raw_wide) * length(metric_names))
    idx <- 0L

    for (ii in seq_len(nrow(raw_wide))) {
        row <- raw_wide[ii, , drop = FALSE]
        for (metric_name in metric_names) {
            idx <- idx + 1L
            rows[[idx]] <- data.frame(
                Imp = row$Imp,
                Fold = row$Fold,
                Model = row$Model,
                Risk = row$Risk,
                Metric = metric_name,
                Time = row$horizon,
                Threshold = row$threshold,
                Value = row[[metric_name]],
                n_valid = row$n_valid,
                n_cases = row$n_cases,
                n_controls = row$n_controls,
                n_pred_positive = row$n_pred_positive,
                stringsAsFactors = FALSE
            )
        }
    }

    do.call(rbind, rows)
}

.summarize_metrics_tm <- function(metrics_long) {
    split_key <- interaction(
        metrics_long$Model,
        metrics_long$Risk,
        metrics_long$Time,
        metrics_long$Threshold,
        metrics_long$Metric,
        drop = TRUE,
        lex.order = TRUE
    )
    groups <- split(metrics_long, split_key)

    out <- lapply(groups, function(df) {
        vals <- as.numeric(df$Value)
        vals <- vals[is.finite(vals)]
        if (!length(vals)) {
            mean_val <- NA_real_
            q025 <- NA_real_
            q975 <- NA_real_
            n_val <- 0L
        } else {
            mean_val <- mean(vals)
            q025 <- as.numeric(stats::quantile(vals, probs = 0.025, names = FALSE, na.rm = TRUE))
            q975 <- as.numeric(stats::quantile(vals, probs = 0.975, names = FALSE, na.rm = TRUE))
            n_val <- length(vals)
        }

        data.frame(
            Model = df$Model[1],
            Risk = df$Risk[1],
            Time = df$Time[1],
            Threshold = df$Threshold[1],
            Metric = df$Metric[1],
            mean = mean_val,
            q025 = q025,
            q975 = q975,
            n = n_val,
            stringsAsFactors = FALSE
        )
    })

    out <- do.call(rbind, out)
    out[order(out$Model, out$Risk, out$Time, out$Threshold, out$Metric), , drop = FALSE]
}

.summarize_counts_tm <- function(raw_wide) {
    split_key <- interaction(
        raw_wide$Model,
        raw_wide$Risk,
        raw_wide$horizon,
        raw_wide$threshold,
        drop = TRUE,
        lex.order = TRUE
    )
    groups <- split(raw_wide, split_key)

    out <- lapply(groups, function(df) {
        data.frame(
            Model = df$Model[1],
            Risk = df$Risk[1],
            Time = df$horizon[1],
            Threshold = df$threshold[1],
            mean_valid_n = mean(df$n_valid),
            mean_cases_n = mean(df$n_cases),
            mean_controls_n = mean(df$n_controls),
            mean_pred_positive_n = mean(df$n_pred_positive),
            n_replicates = nrow(df),
            stringsAsFactors = FALSE
        )
    })

    out <- do.call(rbind, out)
    out[order(out$Model, out$Risk, out$Time, out$Threshold), , drop = FALSE]
}

.format_num_tm <- function(x, digits = 3) {
    vapply(x, function(val) {
        if (!is.finite(val)) {
            return("NA")
        }
        formatC(val, format = "f", digits = digits)
    }, character(1))
}

.build_summary_wide_tm <- function(summary_long, counts_summary) {
    summary_long$formatted <- paste0(
        .format_num_tm(summary_long$mean), " (",
        .format_num_tm(summary_long$q025), " to ",
        .format_num_tm(summary_long$q975), ")"
    )

    wide <- reshape(
        summary_long[, c("Model", "Risk", "Time", "Threshold", "Metric", "formatted")],
        idvar = c("Model", "Risk", "Time", "Threshold"),
        timevar = "Metric",
        direction = "wide"
    )
    names(wide) <- sub("^formatted\\.", "", names(wide))

    out <- merge(
        counts_summary,
        wide,
        by = c("Model", "Risk", "Time", "Threshold"),
        all.x = TRUE,
        sort = FALSE
    )

    keep_cols <- c(
        "Model", "Risk", "Time", "Threshold",
        "n_replicates", "mean_valid_n", "mean_cases_n", "mean_controls_n", "mean_pred_positive_n",
        "NPV", "PPV", "Sens", "Spec", "F1"
    )
    out <- out[, keep_cols, drop = FALSE]
    out[order(out$Model, out$Risk, out$Time, out$Threshold), , drop = FALSE]
}

.default_output_dir_tm <- function(prefix = "results_boot_threshold_metrics") {
    root <- tryCatch(here::here(), error = function(e) getwd())
    cons_dir <- if (basename(root) == "cons") root else file.path(root, "cons")
    out_root <- file.path(cons_dir, "_out")
    if (!dir.exists(out_root)) {
        out_root <- file.path(getwd(), "_out")
    }
    file.path(out_root, sprintf("%s_%s", prefix, format(Sys.time(), "%Y%m%d_%H%M%S")))
}

run_threshold_metrics_from_results_boot <- function(
    results_boot_reference,
    results_boot_updated,
    threshold_spec = NULL,
    reference_label = "reference_model",
    updated_label = "updated_model",
    output_dir = NULL,
    prefix = "results_boot_threshold_metrics"
) {
    .assert_results_boot_tm(results_boot_reference, "results_boot_reference")
    .assert_results_boot_tm(results_boot_updated, "results_boot_updated")

    available_horizons_ref <- results_boot_reference$config$eval_times %||%
        unique(unlist(lapply(results_boot_reference$raw_predictions, `[[`, "eval_times")))
    available_horizons_upd <- results_boot_updated$config$eval_times %||%
        unique(unlist(lapply(results_boot_updated$raw_predictions, `[[`, "eval_times")))
    available_horizons <- sort(unique(intersect(as.numeric(available_horizons_ref), as.numeric(available_horizons_upd))))

    threshold_spec <- .normalize_threshold_spec(threshold_spec, available_horizons = available_horizons)
    threshold_df <- .threshold_spec_to_df(threshold_spec)

    ref_blocks <- c(
        .extract_blocks_tm(results_boot_reference, "readmission"),
        .extract_blocks_tm(results_boot_reference, "death")
    )
    upd_blocks <- c(
        .extract_blocks_tm(results_boot_updated, "readmission"),
        .extract_blocks_tm(results_boot_updated, "death")
    )

    raw_reference <- .evaluate_model_blocks(ref_blocks, reference_label, threshold_spec)
    raw_updated <- .evaluate_model_blocks(upd_blocks, updated_label, threshold_spec)
    raw_wide <- do.call(rbind, Filter(function(x) is.data.frame(x) && nrow(x) > 0, list(raw_reference, raw_updated)))
    if (!nrow(raw_wide)) {
        stop("No threshold-based metrics could be computed from the supplied `results_boot` objects.", call. = FALSE)
    }

    metrics_long <- .to_metrics_long(raw_wide)
    summary_long <- .summarize_metrics_tm(metrics_long)
    counts_summary <- .summarize_counts_tm(raw_wide)
    summary_wide <- .build_summary_wide_tm(summary_long, counts_summary)

    output_dir <- output_dir %||% .default_output_dir_tm(prefix = prefix)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    raw_wide_path <- file.path(output_dir, sprintf("%s_raw_wide.csv", prefix))
    metrics_long_path <- file.path(output_dir, sprintf("%s_metrics_long.csv", prefix))
    summary_long_path <- file.path(output_dir, sprintf("%s_summary_long.csv", prefix))
    summary_wide_path <- file.path(output_dir, sprintf("%s_summary_wide.csv", prefix))
    thresholds_path <- file.path(output_dir, sprintf("%s_thresholds.csv", prefix))
    rds_path <- file.path(output_dir, sprintf("%s_results.rds", prefix))

    utils::write.csv(raw_wide, raw_wide_path, row.names = FALSE)
    utils::write.csv(metrics_long, metrics_long_path, row.names = FALSE)
    utils::write.csv(summary_long, summary_long_path, row.names = FALSE)
    utils::write.csv(summary_wide, summary_wide_path, row.names = FALSE)
    utils::write.csv(threshold_df, thresholds_path, row.names = FALSE)

    result <- list(
        raw_wide = raw_wide,
        metrics_long = metrics_long,
        summary_long = summary_long,
        summary_wide = summary_wide,
        thresholds = threshold_df,
        config = list(
            reference_label = reference_label,
            updated_label = updated_label,
            target_definition = paste(
                "At each horizon, cases are subjects with the event by t,",
                "controls are subjects known to be event-free beyond t,",
                "and subjects censored before t are excluded."
            ),
            note = "Thresholds are fixed external choices informed by the DCA narrative and are not optimized in this script.",
            files = list(
                raw_wide = raw_wide_path,
                metrics_long = metrics_long_path,
                summary_long = summary_long_path,
                summary_wide = summary_wide_path,
                thresholds = thresholds_path,
                rds = rds_path
            )
        )
    )
    saveRDS(result, rds_path)
    result
}
