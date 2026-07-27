# Regression test for patient-order preservation in IPCW censoring-survival lookups.
# Run with: Rscript --vanilla cons/_alt_scripts/test_ibs_ipcw_train_order.R

.t0 <- Sys.time()

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "cons/_alt_scripts/test_ibs_ipcw_train_order.R")
}
script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))

engine_files <- c(
  dual = file.path(script_dir, "evaluate_dual_cox_python_style_boot.R"),
  dual_fixed = file.path(script_dir, "evaluate_dual_cox_python_style_boot_FIXED.R"),
  mi_cv = file.path(script_dir, "evaluate_mi_cv_safe_strata.R"),
  oob = file.path(script_dir, "cph_evaluate_boot_oob_mi_corrected.R"),
  window = file.path(script_dir, "ibs_window_bootstrap_holdout.R"),
  nri_idi = file.path(script_dir, "nri_idi_from_results_boot.R"),
  threshold = file.path(script_dir, "threshold_metrics_from_results_boot.R")
)

invisible(lapply(engine_files, function(path) parse(file = path)))

dual_env <- new.env(parent = globalenv())
dual_fixed_env <- new.env(parent = globalenv())
mi_env <- new.env(parent = globalenv())
window_env <- new.env(parent = globalenv())
nri_env <- new.env(parent = globalenv())
threshold_env <- new.env(parent = globalenv())
sys.source(engine_files[["dual"]], envir = dual_env)
sys.source(engine_files[["dual_fixed"]], envir = dual_fixed_env)
sys.source(engine_files[["mi_cv"]], envir = mi_env)
sys.source(engine_files[["window"]], envir = window_env)
sys.source(engine_files[["nri_idi"]], envir = nri_env)
sys.source(engine_files[["threshold"]], envir = threshold_env)

train <- data.frame(
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  event = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
)
test <- data.frame(
  time = c(8.5, 1.5, 6.5, 3.5, 9.5, 2.5, 7.5, 4.5, 5.5, 3.5),
  event = rep(1L, 10L)
)
eval_times <- c(2, 4, 6, 8)
surv_mat <- outer(
  seq_len(nrow(test)),
  seq_along(eval_times),
  function(i, j) pmin(0.98, pmax(0.02, 0.96 - 0.045 * i - 0.07 * j))
)

ibs_scalar_reference <- function(df_train, df_test, surv, times, eps = 1e-8, g_min = 0.05) {
  sf_cens <- survival::survfit(
    survival::Surv(df_train$time, 1 - df_train$event) ~ 1
  )
  safe_times <- sf_cens$time[sf_cens$surv >= g_min]
  tau_ipcw <- if (length(safe_times)) max(safe_times) else max(times)
  keep <- times <= tau_ipcw & times <= max(df_train$time, na.rm = TRUE)
  times_use <- times[keep]
  surv_use <- surv[, keep, drop = FALSE]

  G_scalar <- function(tt) {
    vapply(pmax(as.numeric(tt), 0), function(one_time) {
      pmax(
        as.numeric(summary(sf_cens, times = one_time, extend = TRUE)$surv)[[1]],
        g_min
      )
    }, numeric(1))
  }

  bs_t <- vapply(seq_along(times_use), function(j) {
    t0 <- times_use[[j]]
    y0 <- as.numeric(df_test$time > t0)
    w <- ifelse(
      df_test$time <= t0 & df_test$event == 1L,
      1 / G_scalar(pmax(df_test$time - eps, 0)),
      ifelse(df_test$time > t0, 1 / G_scalar(t0), 0)
    )
    mean(w * (y0 - surv_use[, j])^2, na.rm = TRUE)
  }, numeric(1))

  sum(
    diff(times_use) * (utils::head(bs_t, -1L) + utils::tail(bs_t, -1L)) / 2
  ) / (max(times_use) - min(times_use))
}

ibs_legacy_reference <- function(df_train, df_test, surv, times, eps = 1e-8, g_min = 0.05) {
  sf_cens <- survival::survfit(
    survival::Surv(df_train$time, 1 - df_train$event) ~ 1
  )
  G_legacy <- function(tt) {
    pmax(
      as.numeric(summary(sf_cens, times = pmax(tt, 0), extend = TRUE)$surv),
      g_min
    )
  }
  bs_t <- vapply(seq_along(times), function(j) {
    t0 <- times[[j]]
    y0 <- as.numeric(df_test$time > t0)
    w <- ifelse(
      df_test$time <= t0 & df_test$event == 1L,
      1 / G_legacy(pmax(df_test$time - eps, 0)),
      ifelse(df_test$time > t0, 1 / G_legacy(t0), 0)
    )
    mean(w * (y0 - surv[, j])^2, na.rm = TRUE)
  }, numeric(1))
  sum(diff(times) * (utils::head(bs_t, -1L) + utils::tail(bs_t, -1L)) / 2) /
    (max(times) - min(times))
}

expected <- ibs_scalar_reference(train, test, surv_mat, eval_times)
dual_value <- dual_env$ibs_ipcw_train(
  train, test, surv_mat, eval_times, "time", "event"
)
dual_fixed_value <- dual_fixed_env$ibs_ipcw_train(
  train, test, surv_mat, eval_times, "time", "event"
)
mi_value <- mi_env$ibs_ipcw_train(
  train, test, surv_mat, eval_times, "time", "event"
)
legacy_value <- ibs_legacy_reference(train, test, surv_mat, eval_times)

stopifnot(isTRUE(all.equal(dual_value, expected, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(dual_fixed_value, expected, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(mi_value, expected, tolerance = 1e-12)))
stopifnot(abs(legacy_value - expected) > 1e-4)

permutation <- c(7L, 2L, 10L, 4L, 1L, 9L, 5L, 3L, 8L, 6L)
permuted_value <- dual_env$ibs_ipcw_train(
  train,
  test[permutation, , drop = FALSE],
  surv_mat[permutation, , drop = FALSE],
  eval_times,
  "time",
  "event"
)
stopifnot(isTRUE(all.equal(permuted_value, expected, tolerance = 1e-12)))

requested <- c(8.5, 1.5, 6.5, 3.5, 9.5, 2.5, 7.5, 4.5, 5.5, 3.5)
window_lookup <- window_env$.ibsb_G_fun(
  train$time,
  1 - train$event,
  g_min = 0.05
)$fun(requested)
sf_cens <- survival::survfit(survival::Surv(train$time, 1 - train$event) ~ 1)
scalar_lookup <- vapply(requested, function(one_time) {
  pmax(as.numeric(summary(sf_cens, times = one_time, extend = TRUE)$surv)[[1]], 0.05)
}, numeric(1))
stopifnot(isTRUE(all.equal(window_lookup, scalar_lookup, tolerance = 1e-12)))

weight_time <- c(8.5, 1.5, 6.5, 3.5, 9.5, 2.5, 7.5, 4.5, 5.5, 3.5)
weight_event <- c(1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L)
weight_horizon <- 6

scalar_admin_weights <- function(time, event, horizon, eps = 1e-8, g_min = 0.05) {
  fit <- survival::survfit(survival::Surv(time, 1 - event) ~ 1)
  G_scalar <- function(tt) {
    vapply(pmax(as.numeric(tt), 0), function(one_time) {
      pmax(as.numeric(summary(fit, times = one_time, extend = TRUE)$surv)[[1]], g_min)
    }, numeric(1))
  }
  g_tm <- G_scalar(pmax(time - eps, 0))
  g_t <- G_scalar(horizon)[[1]]
  list(
    event = ifelse(time <= horizon & event == 1L, 1 / g_tm, 0),
    nonevent = ifelse(time > horizon, 1 / g_t, 0)
  )
}

admin_expected <- scalar_admin_weights(weight_time, weight_event, weight_horizon)
admin_nri <- nri_env$.rb_ipcw_weights(weight_time, weight_event, weight_horizon)
admin_threshold <- threshold_env$.tm_admin_ipcw_weights(
  weight_time, weight_event, weight_horizon
)
stopifnot(isTRUE(all.equal(admin_nri$event, admin_expected$event, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(admin_nri$nonevent, admin_expected$nonevent, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(admin_threshold$w_event, admin_expected$event, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(admin_threshold$w_nonevent, admin_expected$nonevent, tolerance = 1e-12)))

weight_status <- c(1L, 0L, 2L, 0L, 1L, 2L, 1L, 0L, 2L, 0L)
scalar_cr_weights <- function(ftime, fstatus, horizon, eps = 1e-8, g_min = 0.05) {
  fit <- survival::survfit(
    survival::Surv(ftime, as.integer(fstatus == 0L)) ~ 1
  )
  G_scalar <- function(tt) {
    vapply(pmax(as.numeric(tt), 0), function(one_time) {
      pmax(as.numeric(summary(fit, times = one_time, extend = TRUE)$surv)[[1]], g_min)
    }, numeric(1))
  }
  g_tm <- G_scalar(pmax(ftime - eps, 0))
  g_t <- G_scalar(horizon)[[1]]
  is_event <- ftime <= horizon & fstatus == 1L
  is_compdeath <- ftime <= horizon & fstatus == 2L
  is_efree <- ftime > horizon
  list(
    event = ifelse(is_event, 1 / g_tm, 0),
    nonevent = ifelse(is_compdeath, 1 / g_tm, ifelse(is_efree, 1 / g_t, 0))
  )
}

cr_expected <- scalar_cr_weights(weight_time, weight_status, weight_horizon)
cr_nri <- nri_env$.rb_cr_ipcw_weights(weight_time, weight_status, weight_horizon)
cr_threshold <- threshold_env$.tm_cr_ipcw_weights(
  weight_time, weight_status, weight_horizon
)
stopifnot(isTRUE(all.equal(cr_nri$event, cr_expected$event, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(cr_nri$nonevent, cr_expected$nonevent, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(cr_threshold$w_event, cr_expected$event, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(cr_threshold$w_nonevent, cr_expected$nonevent, tolerance = 1e-12)))

legacy_pattern <- "summary\\(sf_cens, times = pmax\\(tt, 0\\), extend = TRUE\\)\\$surv"
for (path in engine_files[c("dual", "dual_fixed", "mi_cv", "oob")]) {
  stopifnot(!any(grepl(legacy_pattern, readLines(path, warn = FALSE))))
}
stopifnot(!any(grepl(
  "summary\\(sf, times = pmax\\(tt, 0\\), extend = TRUE\\)\\$surv",
  readLines(engine_files[["window"]], warn = FALSE)
)))
for (path in engine_files[c("nri_idi", "threshold")]) {
  stopifnot(!any(grepl(
    "summary\\([^,]+, times = tt, extend = TRUE\\)\\$surv",
    readLines(path, warn = FALSE)
  )))
}

cat(sprintf(
  paste0(
    "PASS: IPCW/metric lookups preserve patient order and duplicates. ",
    "Corrected IBS = %.8f; legacy IBS = %.8f; elapsed = %.3f s.\n"
  ),
  expected,
  legacy_value,
  as.numeric(difftime(Sys.time(), .t0, units = "secs"))
))
