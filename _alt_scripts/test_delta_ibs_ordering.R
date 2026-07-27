.t0 <- proc.time()

args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args)) {
    normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
    normalizePath(file.path(getwd(), "../.."), winslash = "/", mustWork = TRUE)
}

source(file.path(project_root, "cons/_alt_scripts/evaluate_dual_cox_python_style_boot.R"))
source(file.path(project_root, "cons/_alt_scripts/recompute_dual_cox_ibs_from_raw.R"))
assert_ordered_ipcw_engine()

legacy_ibs_ipcw_train <- function(
    df_train, df_test, surv_mat, times, time_col, event_col,
    eps = 1e-8, g_min = 0.05
) {
    ev_train <- to01(df_train[[event_col]])
    sf_cens <- survival::survfit(
        survival::Surv(df_train[[time_col]], 1 - ev_train) ~ 1
    )
    safe_times <- sf_cens$time[sf_cens$surv >= g_min]
    tau_ipcw <- if (length(safe_times)) max(safe_times) else max(times)
    keep <- times <= tau_ipcw & times <= max(df_train[[time_col]], na.rm = TRUE)
    if (sum(keep) < 2L) return(NA_real_)
    times_use <- times[keep]
    surv_use <- surv_mat[, keep, drop = FALSE]
    G_at <- function(tt) {
        pmax(
            as.numeric(summary(sf_cens, times = pmax(tt, 0), extend = TRUE)$surv),
            g_min
        )
    }
    test_time <- as.numeric(df_test[[time_col]])
    test_event <- to01(df_test[[event_col]])
    bs <- vapply(seq_along(times_use), function(ii) {
        horizon <- times_use[[ii]]
        observed_survival <- as.numeric(test_time > horizon)
        weights <- ifelse(
            test_time <= horizon & test_event == 1L,
            1 / G_at(pmax(test_time - eps, 0)),
            ifelse(test_time > horizon, 1 / G_at(horizon), 0)
        )
        mean(weights * (observed_survival - surv_use[, ii])^2)
    }, numeric(1))
    sum(diff(times_use) * (head(bs, -1L) + tail(bs, -1L)) / 2) /
        (max(times_use) - min(times_use))
}

train <- data.frame(
    time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14),
    event = c(1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1)
)
test <- data.frame(
    time = c(8.2, 1.4, 6.7, 2.6, 9.3, 3.1, 7.5, 4.4),
    event = c(1, 1, 0, 1, 0, 1, 1, 0)
)
times <- c(2, 4, 6, 8)

set.seed(2125)
surv_a <- matrix(runif(nrow(test) * length(times), 0.15, 0.95), nrow(test))
surv_b <- matrix(runif(nrow(test) * length(times), 0.15, 0.95), nrow(test))
surv_a <- t(apply(surv_a, 1L, cummin))
surv_b <- t(apply(surv_b, 1L, cummin))
permutation <- c(5, 2, 8, 1, 7, 3, 6, 4)

score <- function(fun, test_data, survival_matrix) {
    fun(
        df_train = train,
        df_test = test_data,
        surv_mat = survival_matrix,
        times = times,
        time_col = "time",
        event_col = "event"
    )
}

corrected_a <- score(ibs_ipcw_train, test, surv_a)
corrected_a_permuted <- score(
    ibs_ipcw_train, test[permutation, , drop = FALSE],
    surv_a[permutation, , drop = FALSE]
)
legacy_a <- score(legacy_ibs_ipcw_train, test, surv_a)
legacy_a_permuted <- score(
    legacy_ibs_ipcw_train, test[permutation, , drop = FALSE],
    surv_a[permutation, , drop = FALSE]
)

stopifnot(isTRUE(all.equal(corrected_a, corrected_a_permuted, tolerance = 1e-14)))
stopifnot(abs(legacy_a - legacy_a_permuted) > 1e-6)
stopifnot(!"strata" %in% names(formals(ibs_ipcw_train)))

corrected_delta <- score(ibs_ipcw_train, test, surv_a) -
    score(ibs_ipcw_train, test, surv_b)
legacy_delta <- score(legacy_ibs_ipcw_train, test, surv_a) -
    score(legacy_ibs_ipcw_train, test, surv_b)

# A common weighting error cancels only when the two models have identical residuals.
stopifnot(is.finite(corrected_delta), is.finite(legacy_delta))
stopifnot(abs(corrected_delta - legacy_delta) > 1e-6)

cat(sprintf(
    paste0(
        "PASS: corrected IBS is invariant to joint patient permutation (%.8f).\n",
        "Legacy IBS changed under the same permutation: %.8f -> %.8f.\n",
        "The delta changed from %.8f (legacy) to %.8f (corrected).\n",
        "strata() is absent from the IBS function interface.\n"
    ),
    corrected_a, legacy_a, legacy_a_permuted, legacy_delta, corrected_delta
))

.elapsed <- (proc.time() - .t0)[["elapsed"]] / 60
cat(sprintf("Elapsed: %.3f minutes.\n", .elapsed))
