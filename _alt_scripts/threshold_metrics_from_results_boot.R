# Threshold-based classification metrics (NPV, PPV, Sens, Spec, F1) from paired
# `results_boot` Cox outputs, evaluated at fixed follow-up horizons.
#
# =============================================================================
# _alt_scripts VERSION — audited 2026-05-30. Supersedes
# cons/_hist_scripts/threshold_metrics_from_results_boot.R.
#
# CHANGES vs _hist_scripts:
#   1. UNDEFINED metrics now return NA (not 0). Sens/Spec/PPV/NPV/F1 with a zero
#      denominator (e.g. PPV when no subject is flagged positive at a high
#      threshold) are undefined; the _hist version returned 0, which silently
#      pulled the pooled means/percentiles toward zero. NA is dropped by the
#      pooling step, so the summary reflects only replicates where the metric is
#      defined. This is the single change that can move reported numbers under
#      `estimator = "complete_case"`.
#   2. NEW `estimator` argument:
#        "complete_case" (default) -> EXACTLY mirrors the _hist / prediction23
#          logic: case = event by t, control = known event-free past t, subjects
#          censored before t are EXCLUDED. (Backward compatible.)
#        "ipcw" -> censoring-adjusted classification metrics via inverse
#          probability of censoring weighting (Uno 2007; Blanche 2013). For
#          READMISSION this is COMPETING-RISK aware: death is NOT censoring,
#          patients who die before t without readmission count as controls
#          (weighted), consistent with the project's Aalen-Johansen calibration,
#          DCA and NRI/IDI engines. For DEATH it is standard admin-censoring IPCW.
#          Recommended for the manuscript; "complete_case" kept for continuity.
#      Under "ipcw", TP/FP/FN/TN are IPCW-weighted effective counts (non-integer);
#      n_cases / n_controls / n_valid / n_pred_positive remain raw head counts.
#   3. do.call(rbind, .) -> do.call(rbind.data.frame, .) everywhere, with a
#      guard against an empty model list (mice's rbind.mids S3 hijack / the
#      rbind() no-args edge case).
#
# CAVEATS (report honestly): thresholds are FIXED external choices, not
# optimized here; these are SECONDARY / illustrative metrics and must not drive
# model selection. The 95% "intervals" are empirical 2.5/97.5 percentiles ACROSS
# validation replicates (folds x imputations), i.e. split-to-split spread, not CIs.
#
# Usage:
# source("cons/_alt_scripts/threshold_metrics_from_results_boot.R")
# threshold_out <- run_threshold_metrics_from_results_boot(
#   results_boot_reference, results_boot_updated,
#   reference_label = "Full PH", updated_label = "SHAP primary",
#   estimator = "complete_case"   # or "ipcw"
# )
# =============================================================================

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
            `6`  = c(0.03, 0.05),
            `12` = c(0.03, 0.05),
            `36` = c(0.05),
            `60` = c(0.03, 0.05)
        ),
        readmission = list(
            `6`  = c(0.10, 0.15, 0.20),
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

    out <- do.call(rbind.data.frame, rows)
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
            ifkey = paste(item$imp_idx %||% NA_integer_, item$fold_idx %||% NA_integer_, sep = "::"),
            imp_idx = item$imp_idx %||% NA_integer_,
            fold_idx = item$fold_idx %||% NA_integer_,
            risk = risk,
            eval_times = eval_times,
            time = as.numeric(y_val$time),
            event = as.integer(y_val$event),
            pred_risk = 1 - surv_mat,
            use_cr = FALSE,
            cr_ftime = NULL,
            cr_fstatus = NULL
        )
    }

    blocks <- Filter(Negate(is.null), blocks)
    if (!length(blocks)) {
        stop("No usable raw prediction blocks were found for `", risk, "`.", call. = FALSE)
    }
    blocks
}

# Reconstruct competing first-event status (0=admin censor, 1=readmit, 2=death)
# and attach it to readmission blocks, using the death block of the same fold.
.tm_cr_reconstruct <- function(readmit_time, readmit_event, death_time, death_event,
                               tie_action = "death_first") {
    rt <- as.numeric(readmit_time); re <- as.integer(readmit_event)
    dt <- as.numeric(death_time);   de <- as.integer(death_event)
    ftime <- pmin(rt, dt)
    r_first <- !is.na(re) & re == 1L & is.finite(ftime) & (abs(rt - ftime) <= 1e-08)
    d_first <- !is.na(de) & de == 1L & is.finite(ftime) & (abs(dt - ftime) <= 1e-08)
    fstatus <- rep(0L, length(ftime))
    fstatus[r_first & !d_first] <- 1L
    fstatus[d_first & !r_first] <- 2L
    both <- r_first & d_first
    if (any(both)) {
        fstatus[both] <- if (identical(tie_action, "readmit_first")) 1L else 2L
    }
    list(ftime = ftime, fstatus = fstatus)
}

.tm_attach_cr <- function(readmit_blocks, death_blocks, tie_action) {
    dkey <- vapply(death_blocks, `[[`, character(1), "ifkey")
    n_cr <- 0L; n_fallback <- 0L
    for (i in seq_along(readmit_blocks)) {
        pb <- readmit_blocks[[i]]
        j <- match(pb$ifkey, dkey)
        ok <- !is.na(j) && length(death_blocks[[j]]$time) == length(pb$time)
        if (isTRUE(ok)) {
            db <- death_blocks[[j]]
            cr <- .tm_cr_reconstruct(pb$time, pb$event, db$time, db$event, tie_action = tie_action)
            readmit_blocks[[i]]$cr_ftime <- cr$ftime
            readmit_blocks[[i]]$cr_fstatus <- cr$fstatus
            readmit_blocks[[i]]$use_cr <- TRUE
            n_cr <- n_cr + 1L
        } else {
            readmit_blocks[[i]]$use_cr <- FALSE
            n_fallback <- n_fallback + 1L
        }
    }
    attr(readmit_blocks, "n_cr") <- n_cr
    attr(readmit_blocks, "n_fallback") <- n_fallback
    readmit_blocks
}

# ---- IPCW weight builders ----
.tm_survfit_lookup_ordered <- function(fit, tt, g_min = 0.05) {
    requested_times <- pmax(as.numeric(tt), 0)
    out <- rep(NA_real_, length(requested_times))
    finite <- is.finite(requested_times)
    if (!any(finite)) return(out)

    unique_times <- sort(unique(requested_times[finite]))
    surv_unique <- as.numeric(
        summary(fit, times = unique_times, extend = TRUE)$surv
    )
    if (length(surv_unique) != length(unique_times))
        stop("Censoring-survival lookup returned an unexpected length.", call. = FALSE)

    lookup_index <- match(requested_times[finite], unique_times)
    if (anyNA(lookup_index))
        stop("Censoring-survival lookup could not restore request order.", call. = FALSE)

    out[finite] <- pmax(surv_unique[lookup_index], g_min)
    out
}

.tm_admin_ipcw_weights <- function(time, event, horizon, eps = 1e-8, g_min = 0.05) {
    fitG <- survival::survfit(survival::Surv(time, 1 - event) ~ 1)
    Ghat <- function(tt) {
        s <- .tm_survfit_lookup_ordered(fitG, tt, g_min = g_min)
        s[!is.finite(s)] <- NA_real_
        s
    }
    g_t <- Ghat(horizon)[1]
    g_tm <- Ghat(pmax(time - eps, 0))
    list(
        w_event = ifelse(time <= horizon & event == 1L, 1 / g_tm, 0),
        w_nonevent = ifelse(time > horizon, 1 / g_t, 0)
    )
}

.tm_cr_ipcw_weights <- function(ftime, fstatus, horizon, eps = 1e-8, g_min = 0.05) {
    cens_indicator <- as.integer(fstatus == 0L)
    fitG <- survival::survfit(survival::Surv(ftime, cens_indicator) ~ 1)
    Ghat <- function(tt) {
        s <- .tm_survfit_lookup_ordered(fitG, tt, g_min = g_min)
        s[!is.finite(s)] <- NA_real_
        s
    }
    g_t <- Ghat(horizon)[1]
    g_tm <- Ghat(pmax(ftime - eps, 0))
    is_event     <- (ftime <= horizon) & (fstatus == 1L)
    is_compdeath <- (ftime <= horizon) & (fstatus == 2L)
    is_efree     <- (ftime >  horizon)
    list(
        w_event = ifelse(is_event, 1 / g_tm, 0),
        w_nonevent = ifelse(is_compdeath, 1 / g_tm, ifelse(is_efree, 1 / g_t, 0))
    )
}

# Weighted confusion-matrix metrics; undefined ratios return NA (not 0).
.tm_confusion <- function(pred, threshold, w_event, w_nonevent) {
    pos <- pred >= threshold
    TP <- sum(w_event[pos]);    FN <- sum(w_event[!pos])
    FP <- sum(w_nonevent[pos]); TN <- sum(w_nonevent[!pos])
    nd <- function(num, den) if (is.finite(den) && den > 0) num / den else NA_real_
    n_cases    <- sum(w_event > 0)
    n_controls <- sum(w_nonevent > 0)
    c(
        Sens = nd(TP, TP + FN),
        Spec = nd(TN, TN + FP),
        PPV  = nd(TP, TP + FP),
        NPV  = nd(TN, TN + FN),
        F1   = nd(2 * TP, 2 * TP + FP + FN),
        TP = TP, FP = FP, FN = FN, TN = TN,
        n_valid = n_cases + n_controls,
        n_cases = n_cases,
        n_controls = n_controls,
        n_pred_positive = sum(pos),
        n_pred_negative = sum(!pos)
    )
}

# Compute per-(block, horizon, threshold) weights and metrics for one estimator.
.block_horizon_weights <- function(block, horizon, h_idx, estimator, g_min) {
    pred <- as.numeric(block$pred_risk[, h_idx[1]])

    if (identical(estimator, "complete_case")) {
        tt <- as.numeric(block$time); ev <- as.integer(block$event)
        is_case <- ev == 1L & tt <= horizon
        is_control <- tt > horizon
        valid <- is_case | is_control
        list(pred = pred[valid],
             w_event = as.numeric(is_case[valid]),
             w_nonevent = as.numeric(is_control[valid]))
    } else if (identical(block$risk, "readmission") && isTRUE(block$use_cr)) {
        ft <- block$cr_ftime; fs <- block$cr_fstatus
        keep <- is.finite(pred) & is.finite(ft) & !is.na(fs)
        w <- .tm_cr_ipcw_weights(ft[keep], fs[keep], horizon, g_min = g_min)
        list(pred = pred[keep], w_event = w$w_event, w_nonevent = w$w_nonevent)
    } else {
        # death, or readmission ipcw fallback (no aligned competing block)
        tt <- as.numeric(block$time); ev <- as.integer(block$event)
        keep <- is.finite(pred) & is.finite(tt) & !is.na(ev)
        w <- .tm_admin_ipcw_weights(tt[keep], ev[keep], horizon, g_min = g_min)
        list(pred = pred[keep], w_event = w$w_event, w_nonevent = w$w_nonevent)
    }
}

.evaluate_model_blocks <- function(blocks, model_label, threshold_spec, estimator, g_min) {
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

            wts <- .block_horizon_weights(block, horizon, h_idx, estimator, g_min)
            if (!length(wts$pred)) {
                next
            }
            thresholds <- risk_spec[[horizon_name]]

            for (threshold in thresholds) {
                metric_vals <- .tm_confusion(wts$pred, threshold, wts$w_event, wts$w_nonevent)
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
    out <- do.call(rbind.data.frame, rows)
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

    do.call(rbind.data.frame, rows)
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

    out <- do.call(rbind.data.frame, out)
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

    out <- do.call(rbind.data.frame, out)
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

    wide <- stats::reshape(
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
    out <- out[, intersect(keep_cols, names(out)), drop = FALSE]
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
    estimator = c("complete_case", "ipcw"),
    tie_action = c("death_first", "readmit_first"),
    g_min = 0.05,
    output_dir = NULL,
    prefix = "results_boot_threshold_metrics",
    verbose = TRUE
) {
    estimator <- match.arg(estimator)
    tie_action <- match.arg(tie_action)
    .assert_results_boot_tm(results_boot_reference, "results_boot_reference")
    .assert_results_boot_tm(results_boot_updated, "results_boot_updated")

    available_horizons_ref <- results_boot_reference$config$eval_times %||%
        unique(unlist(lapply(results_boot_reference$raw_predictions, `[[`, "eval_times")))
    available_horizons_upd <- results_boot_updated$config$eval_times %||%
        unique(unlist(lapply(results_boot_updated$raw_predictions, `[[`, "eval_times")))
    available_horizons <- sort(unique(intersect(as.numeric(available_horizons_ref), as.numeric(available_horizons_upd))))

    threshold_spec <- .normalize_threshold_spec(threshold_spec, available_horizons = available_horizons)
    threshold_df <- .threshold_spec_to_df(threshold_spec)

    ref_readmit <- .extract_blocks_tm(results_boot_reference, "readmission")
    ref_death   <- .extract_blocks_tm(results_boot_reference, "death")
    upd_readmit <- .extract_blocks_tm(results_boot_updated, "readmission")
    upd_death   <- .extract_blocks_tm(results_boot_updated, "death")

    n_readmit_cr <- 0L
    n_readmit_fallback <- 0L
    if (identical(estimator, "ipcw")) {
        ref_readmit <- .tm_attach_cr(ref_readmit, ref_death, tie_action)
        upd_readmit <- .tm_attach_cr(upd_readmit, upd_death, tie_action)
        n_readmit_cr <- (attr(ref_readmit, "n_cr") %||% 0L) + (attr(upd_readmit, "n_cr") %||% 0L)
        n_readmit_fallback <- (attr(ref_readmit, "n_fallback") %||% 0L) + (attr(upd_readmit, "n_fallback") %||% 0L)
    }

    ref_blocks <- c(ref_readmit, ref_death)
    upd_blocks <- c(upd_readmit, upd_death)

    raw_reference <- .evaluate_model_blocks(ref_blocks, reference_label, threshold_spec, estimator, g_min)
    raw_updated   <- .evaluate_model_blocks(upd_blocks, updated_label, threshold_spec, estimator, g_min)

    parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, list(raw_reference, raw_updated))
    if (!length(parts)) {
        stop("No threshold-based metrics could be computed from the supplied `results_boot` objects.", call. = FALSE)
    }
    raw_wide <- do.call(rbind.data.frame, parts)

    metrics_long <- .to_metrics_long(raw_wide)
    summary_long <- .summarize_metrics_tm(metrics_long)
    counts_summary <- .summarize_counts_tm(raw_wide)
    summary_wide <- .build_summary_wide_tm(summary_long, counts_summary)

    if (identical(estimator, "ipcw") && isTRUE(verbose)) {
        message(sprintf(
            "Threshold metrics: IPCW. Readmission competing-risk on %d replicate(s)%s. Death: admin-censoring IPCW.",
            n_readmit_cr,
            if (n_readmit_fallback > 0L) sprintf("; %d fell back to death-as-censoring", n_readmit_fallback) else ""
        ))
    }

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

    target_definition <- if (identical(estimator, "complete_case")) {
        paste(
            "At each horizon, cases are subjects with the event by t,",
            "controls are subjects known to be event-free beyond t,",
            "and subjects censored before t are excluded (complete-case)."
        )
    } else {
        paste(
            "At each horizon, classification metrics are IPCW-weighted (Uno 2007).",
            "Death: administrative-censoring IPCW.",
            "Readmission: competing-risk IPCW (deaths before t without readmission",
            "count as controls; only administrative censoring is treated as censoring)."
        )
    }

    result <- list(
        raw_wide = raw_wide,
        metrics_long = metrics_long,
        summary_long = summary_long,
        summary_wide = summary_wide,
        thresholds = threshold_df,
        config = list(
            reference_label = reference_label,
            updated_label = updated_label,
            estimator = estimator,
            tie_action = tie_action,
            g_min = g_min,
            n_readmit_competing_risk = n_readmit_cr,
            n_readmit_ipcw_fallback = n_readmit_fallback,
            target_definition = target_definition,
            note = "Thresholds are fixed external choices informed by the DCA narrative and are not optimized in this script. Undefined metrics are NA. Intervals are 2.5/97.5 percentiles across replicates, not CIs.",
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
