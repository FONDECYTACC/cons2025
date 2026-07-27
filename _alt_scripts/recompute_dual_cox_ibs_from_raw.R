# Repair IBS metrics from stored out-of-fold survival predictions without refitting Cox models.

IPCW_ORDER_CONTRACT <- "ordered_patient_times_v1"

assert_ordered_ipcw_engine <- function() {
    if (!exists("ibs_ipcw_train", mode = "function")) {
        stop("ibs_ipcw_train() must be sourced before this helper.", call. = FALSE)
    }

    body_text <- paste(deparse(body(ibs_ipcw_train)), collapse = "\n")
    has_order_restore <- grepl("match\\(requested_times, unique_times\\)", body_text)
    if (!has_order_restore) {
        stop(
            "The active ibs_ipcw_train() does not restore patient-time order after summary.survfit().",
            call. = FALSE
        )
    }

    invisible(TRUE)
}

.recompute_dual_cox_ibs_one <- function(raw_block, outcome_key) {
    outcome <- raw_block[[outcome_key]]
    if (is.null(outcome) || !is.null(outcome$error)) {
        return(NULL)
    }

    eval_times <- as.numeric(raw_block$eval_times)
    train_time_max <- max(as.numeric(outcome$y_train$time), na.rm = TRUE)
    values <- rep(NA_real_, length(eval_times) + 1L)
    names(values) <- c("Global", as.character(eval_times))

    values[["Global"]] <- ibs_ipcw_train(
        df_train = outcome$y_train,
        df_test = outcome$y_val,
        surv_mat = outcome$surv_val_matrix,
        times = eval_times,
        time_col = "time",
        event_col = "event"
    )

    for (ii in seq_along(eval_times)) {
        horizon <- eval_times[[ii]]
        keep <- eval_times <= horizon
        if (is.finite(train_time_max) && horizon < train_time_max && sum(keep) >= 2L) {
            values[[as.character(horizon)]] <- ibs_ipcw_train(
                df_train = outcome$y_train,
                df_test = outcome$y_val,
                surv_mat = outcome$surv_val_matrix[, keep, drop = FALSE],
                times = eval_times[keep],
                time_col = "time",
                event_col = "event"
            )
        }
    }

    values
}

recompute_dual_cox_ibs_from_raw <- function(
    result,
    outcomes = c("readmission", "death"),
    verbose = TRUE
) {
    .t0 <- proc.time()
    assert_ordered_ipcw_engine()

    required <- c("metrics", "summary", "raw_predictions", "config")
    missing_fields <- setdiff(required, names(result))
    if (length(missing_fields)) {
        stop(
            "Result object is missing: ", paste(missing_fields, collapse = ", "),
            call. = FALSE
        )
    }
    if (!length(result$raw_predictions)) {
        stop("Stored raw_predictions are required for IBS repair.", call. = FALSE)
    }

    outcomes <- match.arg(outcomes, c("readmission", "death"), several.ok = TRUE)
    risk_labels <- c(readmission = "Readmission", death = "Death")
    metrics <- result$metrics

    for (raw_block in result$raw_predictions) {
        for (outcome_key in outcomes) {
            values <- .recompute_dual_cox_ibs_one(raw_block, outcome_key)
            if (is.null(values)) next

            for (time_label in names(values)) {
                row_index <- which(
                    metrics$Imp == raw_block$imp_idx &
                        metrics$Fold == raw_block$fold_idx &
                        metrics$Risk == risk_labels[[outcome_key]] &
                        metrics$Metric == "IBS" &
                        metrics$Time == time_label
                )
                if (length(row_index) != 1L) {
                    stop(
                        "Expected one IBS metric row for imputation ", raw_block$imp_idx,
                        ", fold ", raw_block$fold_idx, ", risk ", risk_labels[[outcome_key]],
                        ", time ", time_label, "; found ", length(row_index), ".",
                        call. = FALSE
                    )
                }
                metrics$Value[[row_index]] <- unname(values[[time_label]])
            }
        }
    }

    if (!exists(".dual_cox_summarize_metrics", mode = "function")) {
        stop(".dual_cox_summarize_metrics() is unavailable.", call. = FALSE)
    }
    result$metrics <- metrics
    result$summary <- .dual_cox_summarize_metrics(metrics)
    result$config$ipcw_order_contract <- IPCW_ORDER_CONTRACT
    result$config$ibs_recomputed_from_raw_at <- as.character(Sys.time())

    if (isTRUE(verbose)) {
        elapsed <- (proc.time() - .t0)[["elapsed"]] / 60
        message(sprintf(
            "Recomputed corrected IBS for %s from stored OOF predictions in %.2f min.",
            paste(outcomes, collapse = ", "), elapsed
        ))
    }

    result
}
