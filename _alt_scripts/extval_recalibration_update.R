# =============================================================================
# extval_recalibration_update.R
#
# Shared, self-contained engine implementing an EVALUABLE recalibration
# sequence for external validation, as opposed to the unsupported blanket
# claim "the pattern is compatible with recalibration, not respecification".
# One C-index that stays roughly stable plus a shifted E:O ratio is compatible
# with a change in the baseline hazard, but it does not by itself demonstrate
# that (a) the relative-risk ranking transported without change, or (b)
# updating only the baseline hazard is actually sufficient. E:O is a single
# summary of average calibration; it says nothing about whether predictions
# are too extreme, too narrow, or locally miscalibrated across the risk
# range. A flexible calibration curve and a calibration slope are needed for
# that, and the update step has to be evaluated on data that were not used to
# derive it, or the reported performance is optimistic.
#
# This file implements, generically over (time, event, predicted_risk,
# horizon), the four steps:
#   1. Evaluate the frozen model as published (already done elsewhere in both
#      prediction26_ext_val_readm_mod.ipynb and prediction27_ext_val_death_mod.ipynb;
#      this file does not repeat that, it only adds the calibration slope and
#      ECE where those were not already computed).
#   2. Estimate an update of the baseline risk ("recalibration in the large"):
#      a one-parameter complementary log-log intercept shift that rescales the
#      overall risk level while holding the relative-risk gradient fixed
#      (slope = 1). This changes only the baseline hazard, not the regression
#      coefficients, and is the smallest possible update.
#   3. Evaluate the updated model OUT OF FOLD via K-fold cross-fitting: the
#      intercept used to score any given subject is always estimated from the
#      OTHER folds, never from that subject's own fold. This is the "nested
#      scheme" the update step needs to avoid reusing the same data to derive
#      and to evaluate the update, without requiring a single wasteful sample
#      split (important for the small convenio-3 mortality cohort).
#   4. Report the calibration slope of the OUT-OF-FOLD updated risk. If its
#      95% CI excludes 1 by a material margin, that is the trigger to consider
#      a fuller update (re-estimated or shrunk coefficients), not something
#      this file decides or performs automatically. Escalating to a fuller
#      re-estimation is a separate, later decision and is intentionally left
#      out of scope here.
#
# Method: complementary log-log (cloglog) pseudo-value regression. For a
# single-event outcome (readmission net-risk arm treating death as censoring;
# all-cause mortality with administrative censoring), Kaplan-Meier jackknife
# pseudo-values turn the censored risk at a fixed horizon into an
# approximately unbiased per-subject "observed risk" that ordinary regression
# methods can use. Regressing pseudo-values on cloglog(predicted risk) with an
# intercept only (slope fixed at 1) is exactly a baseline-hazard-only update;
# freeing the slope gives the usual calibration-slope diagnostic. This mirrors
# the pseudo-value cloglog approach already implemented for the readmission
# net-risk arm in prediction26_ext_val_readm_mod.ipynb's own
# "calibration-brier-and-ibs-functions" cell (pseudo_event_risk(),
# calibration_slope_pseudo()); the functions below are independent,
# namespaced copies (recal_* / .recal_*) so this file has no dependency on
# either notebook's kernel and can be sourced standalone from both
# prediction26_ext_val_readm_mod.ipynb (net-risk arm) and
# prediction27_ext_val_death_mod.ipynb (mortality, convenio 3).
#
# Scope decisions (state these explicitly wherever this file's output is
# reported, since they are judgment calls, not settled facts):
#   - Applied to the PRIMARY reported model only: the net-risk arm for
#     readmission (external_predictions$horizon$pooled in prediction26), and
#     the full convenio-3 cohort for mortality (res$C3 in prediction27, not
#     the matched CS/NN subsamples and not convenio 5, which is discrimination
#     only because it has too few deaths for any calibration exercise).
#   - K = 10 folds by default for both cohorts. For readmission (n ~ 26,000,
#     ~9.6% events) this is comfortable (~250 events per fold). For convenio 3
#     (n ~ 629, ~79 deaths) it is thinner (~7-8 deaths per held-out fold), but
#     the quantity being fit per fold is a single intercept on a training set
#     of ~9/10 of the cohort, which is a low-variance estimation problem;
#     fold-level intercepts are returned so this can be inspected directly.
#   - The bootstrap CI for the recalibration comparison re-runs the FULL
#     K-fold update inside every replicate (not just the evaluation step), so
#     it captures uncertainty coming from the update itself. This is
#     computationally heavier than a fixed-prediction bootstrap, so B defaults
#     to 200 rather than the 1000 used elsewhere in both notebooks; raise it
#     if runtime allows.
# =============================================================================
suppressWarnings({
  stopifnot(requireNamespace("survival", quietly = TRUE))
})
# ---- 0. Small private helpers -------------------------------------------------
# Clamp a probability away from 0 and 1 so log/cloglog transforms stay finite.
.recal_clip_probability <- function(x, epsilon = 1e-8) {
  pmin(pmax(as.numeric(x), epsilon), 1 - epsilon)
}
# Complementary log-log inverse link, clamped on the linear-predictor scale to
# avoid overflow in exp(exp(.)) for extreme eta.
.recal_inverse_cloglog <- function(eta) {
  1 - exp(-exp(pmin(pmax(eta, -20), 20)))
}
# Kaplan-Meier jackknife pseudo-values for event risk at a single horizon.
# O(n log n) risk-set construction (sorts unique times once, uses a reverse
# cumulative sum for the number-at-risk), so it scales to the ~26,000-row
# readmission cohort as well as the much smaller mortality cohorts. Verbatim
# in method to prediction26_ext_val_readm_mod.ipynb's own pseudo_event_risk();
# reimplemented here, independently namespaced, so this file has no
# dependency on that notebook's kernel.
.recal_pseudo_event_risk <- function(time, event, horizon) {
  time <- as.numeric(time)
  event <- as.integer(event)
  n <- length(time)
  event_times <- sort(unique(time[event == 1L & time <= horizon]))
  if (!length(event_times)) return(rep(0, n))
  all_times <- sort(unique(time))
  time_index <- match(time, all_times)
  number_at_time <- tabulate(time_index, nbins = length(all_times))
  at_risk_by_time <- rev(cumsum(rev(number_at_time)))
  event_index <- match(event_times, all_times)
  at_risk <- at_risk_by_time[event_index]
  death_count_by_time <- tabulate(time_index[event == 1L & time <= horizon], nbins = length(all_times))
  deaths <- death_count_by_time[event_index]
  if (any(at_risk <= 1L)) {
    stop("recal: the Kaplan-Meier curve reaches a one-person risk set before the horizon; pseudo-values are not stable.", call. = FALSE)
  }
  base_factor <- 1 - deaths / at_risk
  leave_at_risk_factor <- 1 - deaths / (at_risk - 1)
  leave_death_factor <- 1 - (deaths - 1) / (at_risk - 1)
  if (any(base_factor <= 0) || any(leave_at_risk_factor <= 0) || any(leave_death_factor <= 0)) {
    stop("recal: a leave-one-out Kaplan-Meier factor is zero before the horizon; pseudo-values are not stable.", call. = FALSE)
  }
  log_survival <- sum(log(base_factor))
  risk_set_adjustment <- log(leave_at_risk_factor) - log(base_factor)
  death_adjustment <- log(leave_death_factor) - log(leave_at_risk_factor)
  cumulative_adjustment <- c(0, cumsum(risk_set_adjustment))
  last_event_index <- findInterval(pmin(time, horizon), event_times)
  log_leave_one_out_survival <- log_survival + cumulative_adjustment[last_event_index + 1L]
  is_horizon_death <- event == 1L & time <= horizon
  matched_event <- match(time[is_horizon_death], event_times)
  log_leave_one_out_survival[is_horizon_death] <- log_leave_one_out_survival[is_horizon_death] + death_adjustment[matched_event]
  survival <- exp(log_survival)
  leave_one_out_survival <- exp(log_leave_one_out_survival)
  1 - n * survival + (n - 1) * leave_one_out_survival
}
# Overall Kaplan-Meier observed event risk at one horizon (no confidence
# limits; those are not needed here, unlike the notebook-native km_risk_at()).
recal_km_observed <- function(time, event, horizon) {
  fit <- survival::survfit(survival::Surv(time, event) ~ 1)
  s <- summary(fit, times = horizon, extend = TRUE)$surv
  if (length(s)) 1 - s[[1L]] else NA_real_
}
# ---- 1. Calibration metrics for a single predicted-risk vector ---------------
# Calibration-in-the-large: mean predicted risk divided by the Kaplan-Meier
# observed risk. E:O < 1 means under-prediction, E:O > 1 means over-prediction.
recal_expected_to_observed <- function(time, event, predicted_risk, horizon) {
  observed <- recal_km_observed(time, event, horizon)
  mean_predicted <- mean(.recal_clip_probability(predicted_risk), na.rm = TRUE)
  if (is.finite(observed) && observed > 0) mean_predicted / observed else NA_real_
}
# Integrated Calibration Index: mean absolute difference between each
# subject's predicted risk and a smooth (loess) estimate of their observed
# risk from the Kaplan-Meier pseudo-values, across the whole predicted-risk
# range. Flexible (non-binned), unlike ECE below.
recal_ici <- function(time, event, predicted_risk, horizon, span = 0.75) {
  predicted_risk <- .recal_clip_probability(predicted_risk)
  pseudo <- .recal_pseudo_event_risk(time, event, horizon)
  smoother <- tryCatch(
    stats::loess(pseudo ~ predicted_risk, span = span, degree = 1,
                 control = stats::loess.control(surface = "interpolate", trace.hat = "approximate")),
    error = function(error) NULL)
  if (is.null(smoother)) return(NA_real_)
  fitted <- stats::predict(smoother, newdata = data.frame(predicted_risk = predicted_risk))
  fitted[!is.finite(fitted)] <- mean(pseudo, na.rm = TRUE)
  mean(abs(fitted - predicted_risk), na.rm = TRUE)
}
# Expected Calibration Error: decile-grouped, n-weighted mean absolute
# difference between mean predicted risk and Kaplan-Meier observed risk
# within each bin. Coarser than ICI (binned rather than flexible), but is the
# quantity item 6 explicitly asked for alongside ICI, and is what a
# calibration plot's bin markers actually show.
recal_ece <- function(time, event, predicted_risk, horizon, g = 10L, min_bin_n = 20L) {
  predicted_risk <- .recal_clip_probability(predicted_risk)
  breaks <- unique(stats::quantile(predicted_risk, probs = seq(0, 1, length.out = g + 1L), na.rm = TRUE, names = FALSE))
  if (length(breaks) < 3L) return(NA_real_)
  bin <- cut(predicted_risk, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  unique_bins <- sort(unique(bin))
  rows <- lapply(unique_bins, function(b) {
    idx <- which(bin == b)
    if (length(idx) < min_bin_n) return(NULL)
    observed <- recal_km_observed(time[idx], event[idx], horizon)
    data.frame(mean_predicted = mean(predicted_risk[idx]), observed = observed, n = length(idx))
  })
  curve <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(curve) || !nrow(curve)) return(NA_real_)
  stats::weighted.mean(abs(curve$mean_predicted - curve$observed), w = curve$n, na.rm = TRUE)
}
# Calibration slope: fit intercept and slope of pseudo-value ~ cloglog(risk)
# by nonlinear least squares, with a sandwich (bread-meat-bread) standard
# error for the slope. Slope = 1 with intercept = 0 is perfect calibration;
# slope < 1 means predictions are too extreme (need shrinking toward the
# mean), slope > 1 means predictions are too narrow (need spreading out).
recal_calibration_slope <- function(time, event, predicted_risk, horizon) {
  predicted_risk <- .recal_clip_probability(predicted_risk)
  pseudo <- .recal_pseudo_event_risk(time, event, horizon)
  x <- log(-log1p(-predicted_risk))
  objective <- function(parameters) {
    fitted <- .recal_inverse_cloglog(parameters[[1L]] + parameters[[2L]] * x)
    sum((pseudo - fitted)^2)
  }
  fit <- tryCatch(
    stats::optim(c(0, 1), objective, method = "BFGS", control = list(maxit = 1000, reltol = 1e-12)),
    error = function(error) NULL)
  if (is.null(fit) || fit$convergence != 0L || any(!is.finite(fit$par))) {
    return(c(intercept = NA_real_, slope = NA_real_, slope_se = NA_real_))
  }
  eta <- pmin(pmax(fit$par[[1L]] + fit$par[[2L]] * x, -20), 20)
  fitted <- .recal_inverse_cloglog(eta)
  derivative <- exp(eta - exp(eta))
  design <- cbind(1, x)
  derivative_design <- design * derivative
  bread <- crossprod(derivative_design)
  score_by_person <- derivative_design * as.numeric(pseudo - fitted)
  meat <- crossprod(score_by_person)
  inverse_bread <- tryCatch(solve(bread), error = function(error) NULL)
  robust_variance <- if (is.null(inverse_bread)) matrix(NA_real_, 2, 2) else inverse_bread %*% meat %*% inverse_bread
  standard_error <- sqrt(pmax(diag(robust_variance), 0))
  c(intercept = unname(fit$par[[1L]]), slope = unname(fit$par[[2L]]), slope_se = unname(standard_error[[2L]]))
}
# ---- 2. Baseline-hazard-only update (recalibration in the large) -------------
# One-parameter cloglog intercept fit with slope FIXED at 1: rescales the
# overall risk level while leaving the relative-risk gradient untouched. This
# is deliberately the SMALLEST possible update; it is fit here on whatever
# sample is passed in (a training fold, in recal_kfold_update() below), never
# on the same rows it will be used to score.
recal_baseline_update <- function(time, event, predicted_risk, horizon) {
  predicted_risk <- .recal_clip_probability(predicted_risk)
  pseudo <- .recal_pseudo_event_risk(time, event, horizon)
  x <- log(-log1p(-predicted_risk))
  objective <- function(intercept) sum((pseudo - .recal_inverse_cloglog(intercept + x))^2)
  fit <- stats::optimize(objective, interval = c(-10, 10), tol = 1e-10)
  list(intercept = fit$minimum)
}
# ---- 3. Out-of-fold evaluation of the updated baseline (nested scheme) -------
# K-fold cross-fitting, stratified by event status so every fold's test set
# keeps a proportional share of events: for each fold, the intercept is
# estimated on the OTHER K-1 folds only (recal_baseline_update()) and then
# applied to score that fold's own held-out rows. No subject is ever scored
# using an intercept estimated with its own outcome. If a fold's training
# data yields a non-finite intercept (can happen in a resampled replicate of
# a small cohort with very few events in some fold), that fold falls back to
# intercept = 0 (no adjustment) rather than propagating a broken value, and
# the fallback is counted so it can be reported.
recal_kfold_update <- function(time, event, predicted_risk, horizon, K = 10L, seed = 2125L) {
  n <- length(time)
  stopifnot(length(event) == n, length(predicted_risk) == n)
  set.seed(seed)
  fold_id <- integer(n)
  event_idx <- which(event == 1L)
  censor_idx <- which(event != 1L)
  fold_id[event_idx] <- sample(rep_len(seq_len(K), length(event_idx)))
  fold_id[censor_idx] <- sample(rep_len(seq_len(K), length(censor_idx)))
  updated_risk <- rep(NA_real_, n)
  fold_rows <- vector("list", K)
  n_fallback <- 0L
  for (k in seq_len(K)) {
    test_idx <- which(fold_id == k)
    train_idx <- which(fold_id != k)
    update_fit <- tryCatch(
      recal_baseline_update(time[train_idx], event[train_idx], predicted_risk[train_idx], horizon),
      error = function(error) NULL)
    is_fallback <- is.null(update_fit) || !is.finite(update_fit$intercept)
    intercept_k <- if (is_fallback) 0 else update_fit$intercept
    n_fallback <- n_fallback + as.integer(is_fallback)
    x_test <- log(-log1p(-.recal_clip_probability(predicted_risk[test_idx])))
    eta_test <- pmin(pmax(intercept_k + x_test, -20), 20)
    updated_risk[test_idx] <- .recal_inverse_cloglog(eta_test)
    fold_rows[[k]] <- data.frame(
      fold = k, n_train = length(train_idx), events_train = sum(event[train_idx] == 1L),
      n_test = length(test_idx), events_test = sum(event[test_idx] == 1L),
      intercept = intercept_k, fallback = is_fallback)
  }
  list(updated_risk = updated_risk, fold_id = fold_id,
       fold_summary = do.call(rbind, fold_rows), n_fallback_folds = n_fallback)
}
# ---- 4. Frozen-vs-updated comparison table at one horizon --------------------
# Runs the out-of-fold baseline update and reports E:O, ICI, ECE and the
# calibration slope for both the frozen (as-published) risk and the
# out-of-fold updated risk, side by side. The updated row's calibration slope
# is the diagnostic for step 4: if its 95% CI (from recal_bootstrap_compare()
# below) excludes 1 by a material margin, a baseline-only update is not
# enough and a fuller re-estimation should be considered as a SEPARATE next
# step; this function does not perform that re-estimation.
recal_compare_frozen_vs_updated <- function(time, event, predicted_risk, horizon, K = 10L, seed = 2125L,
                                            g = 10L, min_bin_n = 20L) {
  kf <- recal_kfold_update(time, event, predicted_risk, horizon, K = K, seed = seed)
  updated_risk <- kf$updated_risk
  slope_frozen <- recal_calibration_slope(time, event, predicted_risk, horizon)
  slope_updated <- recal_calibration_slope(time, event, updated_risk, horizon)
  observed <- recal_km_observed(time, event, horizon)
  table <- data.frame(
    horizon_months = horizon,
    model = c("Frozen (as published)", "Baseline-updated (out-of-fold)"),
    n = length(time),
    observed = observed,
    mean_predicted = c(mean(.recal_clip_probability(predicted_risk)), mean(.recal_clip_probability(updated_risk))),
    expected_to_observed = c(recal_expected_to_observed(time, event, predicted_risk, horizon),
                             recal_expected_to_observed(time, event, updated_risk, horizon)),
    ici = c(recal_ici(time, event, predicted_risk, horizon), recal_ici(time, event, updated_risk, horizon)),
    ece = c(recal_ece(time, event, predicted_risk, horizon, g, min_bin_n),
           recal_ece(time, event, updated_risk, horizon, g, min_bin_n)),
    calibration_intercept = c(slope_frozen[["intercept"]], slope_updated[["intercept"]]),
    calibration_slope = c(slope_frozen[["slope"]], slope_updated[["slope"]]),
    calibration_slope_se = c(slope_frozen[["slope_se"]], slope_updated[["slope_se"]]),
    stringsAsFactors = FALSE)
  list(table = table, fold_summary = kf$fold_summary, n_fallback_folds = kf$n_fallback_folds, updated_risk = updated_risk)
}
# ---- 5. Bootstrap CIs for the frozen-vs-updated comparison -------------------
# Resamples subjects with replacement and re-runs the FULL out-of-fold update
# inside every replicate (not just the evaluation step), so the resulting CI
# reflects uncertainty coming from the update itself, not only from scoring a
# fixed set of predictions. This is the honest counterpart to a bootstrap that
# only resamples rows of an already-fixed prediction; it is also much more
# expensive, hence B defaults to 200 rather than the 1000 used for the other
# bootstraps in both notebooks. Every replicate reseeds recal_kfold_update()
# with the same `seed`, so the fold-splitting RNG stream is reproducible
# end-to-end from a single seed; the randomness driving the CI comes entirely
# from which subjects are resampled, not from re-randomizing fold membership
# per replicate.
recal_bootstrap_compare <- function(time, event, predicted_risk, horizon, K = 10L, B = 200L,
                                    seed = 2125L, g = 10L, min_bin_n = 20L, parallel = FALSE) {
  n <- length(time)
  one_rep <- function(idx) {
    t <- time[idx]; e <- event[idx]; p <- predicted_risk[idx]
    kf <- tryCatch(recal_kfold_update(t, e, p, horizon, K = K, seed = seed), error = function(error) NULL)
    updated <- if (is.null(kf)) rep(NA_real_, length(idx)) else kf$updated_risk
    slope_updated <- tryCatch(recal_calibration_slope(t, e, updated, horizon)[["slope"]], error = function(error) NA_real_)
    c(frozen_eo = recal_expected_to_observed(t, e, p, horizon),
      updated_eo = recal_expected_to_observed(t, e, updated, horizon),
      frozen_ici = recal_ici(t, e, p, horizon),
      updated_ici = recal_ici(t, e, updated, horizon),
      frozen_ece = recal_ece(t, e, p, horizon, g, min_bin_n),
      updated_ece = recal_ece(t, e, updated, horizon, g, min_bin_n),
      updated_slope = slope_updated)
  }
  set.seed(seed)
  idx_list <- lapply(seq_len(B), function(b) sample.int(n, n, replace = TRUE))
  use_parallel <- isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)
  replicate_list <- if (use_parallel) {
    future.apply::future_lapply(idx_list, one_rep, future.seed = TRUE)
  } else lapply(idx_list, one_rep)
  M <- do.call(rbind, replicate_list)
  data.frame(
    metric = colnames(M),
    bootstrap_se = apply(M, 2, stats::sd, na.rm = TRUE),
    lower_95 = apply(M, 2, stats::quantile, 0.025, na.rm = TRUE, names = FALSE),
    upper_95 = apply(M, 2, stats::quantile, 0.975, na.rm = TRUE, names = FALSE),
    valid_replicates = apply(M, 2, function(x) sum(is.finite(x))),
    row.names = NULL)
}
