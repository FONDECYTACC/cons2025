# =============================================================================
# holdout_cif_cache.R
#
# Builds a joint cumulative incidence function (CIF) for READMISSION on the
# held-out (20%) validation set, from a pair of cause-specific Cox models
# (readmission + death) fit jointly via riskRegression::CSC, and injects it
# into a results_boot-shaped object in place of the cause-specific 1-S(t)
# readmission "risk" that DCA / IBS-bootstrap / threshold-metrics currently
# consume.
#
# WHY THIS EXISTS (point 3, 2026-07-15 review session):
# Every consumer of `results_boot$raw_predictions` that reports an ABSOLUTE
# probability for readmission (DCA in adca_from_results_boot.R, per-window
# IBS in ibs_window_bootstrap_holdout.R, and Sens/Spec/PPV/NPV in
# threshold_metrics_from_results_boot.R) currently reads
# `item$readmission$surv_val_matrix` and computes `pred_risk = 1 - surv_mat`
# (confirmed by direct inspection of all three files, 2026-07-15). That is
# the cause-specific "net risk" (death treated as censoring), NOT the true
# cumulative incidence under competing risks: it does not account for
# patients who die before they can be readmitted. This file computes the
# real CIF and swaps it into the SAME slot, so none of those three consumer
# files need to change -- they keep reading `1 - surv_val_matrix` exactly as
# before, they just receive the CIF instead of the net risk when handed an
# INJECTED object.
#
# `1 - S(t)` itself is NOT replaced anywhere: the original, un-injected
# `results_boot_val_bp1` / `results_boot_val_bp2` objects remain valid and
# are kept as the "net risk" arm (decision from point 3 of the 2026-07-15
# review: report all three arms side by side, not just the CIF).
#
# Two CIF caches are meant to be built, one per death sub-model pairing
# (mirrors the calibration decision in point 2: the readmission Cox model is
# identical in both configs, only the paired death formula differs, and the
# resulting CIF is reported as a deliberate sensitivity to that pairing, not
# collapsed to one number):
#   - CIF paired with best_perf1$death (Full PH, `updated2`)
#   - CIF paired with best_perf2$death (SHAP, `rule2`)
#
# Depends: riskRegression, prodlim, survival. Requires .vhm_first_event() and
# to01() from validate_holdout_metrics.R / evaluate_dual_cox_python_style_boot.R
# to already be sourced.
# =============================================================================

local({
  required_fns <- c(".vhm_first_event", "to01", ".vhm_calibrate_one", ".vhm_pool", ".vhm_aj_cif")
  missing_fns <- required_fns[!vapply(required_fns, exists, logical(1), mode = "function")]
  if (length(missing_fns) > 0L) {
    stop(
      "holdout_cif_cache.R requires validate_holdout_metrics.R to be sourced first. ",
      "Missing: ", paste(missing_fns, collapse = ", "),
      call. = FALSE
    )
  }
})

# -----------------------------------------------------------------------------
# 1. Build the CIF cache: fit CSC(readmit, death) per imputation on TRAIN,
#    predict the readmission CIF (cause = 1) on VAL at eval_times. Returns a
#    list with one matrix per imputation (rows = val rows IN THE SAME ORDER
#    AS val_list[[i]], columns = eval_times), matching how raw_predictions
#    already stores surv_val_matrix, so injection is purely positional.
# -----------------------------------------------------------------------------
.holdout_build_cif_cache <- function(formula_readmit, formula_death,
                                     train_list, val_list, eval_times,
                                     verbose = TRUE) {
  stopifnot(length(train_list) == length(val_list))
  n_imp <- length(train_list)
  by_imp <- vector("list", n_imp)

  if (verbose) cat(sprintf("Building joint CIF cache: %d imputations\n", n_imp))

  # riskRegression::CSC() requires BOTH cause-specific formulas to share the
  # SAME response, the competing-risk first-event encoding prodlim::Hist(
  # .ftime, .fstatus) (cause 1 = readmission, cause 2 = death; see
  # .vhm_first_event()) -- passing each model's own original Surv(...)
  # response (e.g. Surv(readmit_time_from_disch_m, readmit_event) and
  # Surv(death_time_from_disch_m, death_event)) directly is invalid and
  # fails inside CSC's internal model-type dispatch. Only the RHS
  # (covariates + strata) of formula_readmit/formula_death is reused here,
  # exactly mirroring the already-working pattern in
  # calibrate_readmit_holdout() in validate_holdout_metrics.R.
  .cif_rhs <- function(f) {
    txt <- paste(deparse(stats::formula(stats::delete.response(stats::terms(f))),
                         width.cutoff = 500L), collapse = " ")
    txt <- gsub("strat\\(", "strata(", txt)
    sub("^~\\s*", "", txt)
  }
  f1 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", .cif_rhs(formula_readmit)))
  f2 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", .cif_rhs(formula_death)))

  for (i in seq_len(n_imp)) {
    tr <- as.data.frame(train_list[[i]])
    te <- as.data.frame(val_list[[i]])
    fe <- .vhm_first_event(tr)
    tr$.ftime <- fe$.ftime
    tr$.fstatus <- fe$.fstatus

    fit <- riskRegression::CSC(list(f1, f2), data = tr)
    pred <- riskRegression::predictRisk(fit, newdata = te, times = eval_times, cause = 1)
    pred <- as.matrix(pred)
    stopifnot(
      nrow(pred) == nrow(te),
      ncol(pred) == length(eval_times),
      all(is.finite(pred))
    )
    by_imp[[i]] <- pmin(pmax(pred, 0), 1)
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")

  list(eval_times = as.numeric(eval_times), by_imp = by_imp)
}

# -----------------------------------------------------------------------------
# 2. Inject the cached CIF into a COPY of a results_boot-shaped object, in
#    place of readmission's surv_val_matrix (as 1 - CIF, so that every
#    consumer's own `1 - surv_val_matrix` recovers the CIF unchanged). The
#    original object (e.g. results_boot_val_bp1) is untouched; this returns
#    a new object. `$metrics`/`$summary` are dropped because they were
#    computed from the pre-injection (net-risk) predictions and would be
#    stale/misleading if kept -- callers needing summary numbers for the
#    injected object must recompute them from `raw_predictions` directly
#    (e.g. via the DCA/IBS/threshold engines, which is exactly what point 3
#    does with this object).
# -----------------------------------------------------------------------------
.holdout_inject_readmission_cif <- function(results_boot, cif_cache) {
  out <- results_boot
  rp <- out$raw_predictions
  stopifnot(length(rp) == length(cif_cache$by_imp))

  for (k in seq_along(rp)) {
    item <- rp[[k]]
    imp <- as.integer(item$imp_idx)
    block <- item$readmission
    stopifnot(
      is.null(block$error),
      isTRUE(all.equal(as.numeric(item$eval_times), cif_cache$eval_times)),
      nrow(as.matrix(block$surv_val_matrix)) == nrow(cif_cache$by_imp[[imp]])
    )
    rp[[k]]$readmission$surv_val_matrix <- 1 - cif_cache$by_imp[[imp]]
  }
  out$raw_predictions <- rp
  out$metrics <- NULL
  out$summary <- NULL
  out$config$readmission_prediction <- "joint CIF via CSC (injected, see holdout_cif_cache.R)"
  out
}

# -----------------------------------------------------------------------------
# 3. Calibration, reading directly from `raw_predictions` instead of refitting
#    a CSC model internally. Works uniformly for ANY readmission "arm": pass
#    the ORIGINAL results_boot_val_bp1/bp2 (net risk, 1-S(t)) or an object
#    produced by .holdout_inject_readmission_cif() (joint CIF). Observed side
#    is always Aalen-Johansen, reconstructed from the SAME item's readmission
#    + death y_val blocks -- by design, this lets the net-risk arm be
#    calibrated against the true competing-risk observed incidence, which is
#    exactly what quantifies how much net risk mis-tracks the real CIF (see
#    the point-2 discussion in the 2026-07-15 review: this mismatch is
#    intentional and informative, not a bug).
#
# UNIFICATION NOTE (2026-07-15): this supersedes calling
# calibrate_readmit_holdout() from validate_holdout_metrics.R for the CIF
# arms, because that function refits its OWN CSC model internally -- doing so
# here too would fit the identical joint model a second time on the full
# 70521-row development set for no benefit. calibrate_readmit_holdout() is
# left in place (still correctly fixed, still usable standalone) but the
# notebook's held-out pipeline now calls the functions below instead, reusing
# the ONE CSC fit already done by .holdout_build_cif_cache().
# -----------------------------------------------------------------------------
.holdout_cr_first_event <- function(r_yval, d_yval) {
  rt <- as.numeric(r_yval$time); re <- as.integer(r_yval$event)
  dt <- as.numeric(d_yval$time); de <- as.integer(d_yval$event)
  ftime <- pmin(rt, dt)
  r_first <- !is.na(re) & re == 1L & is.finite(ftime) & abs(rt - ftime) <= 1e-8
  d_first <- !is.na(de) & de == 1L & is.finite(ftime) & abs(dt - ftime) <= 1e-8
  fstatus <- rep(0L, length(ftime))
  fstatus[r_first & !d_first] <- 1L
  fstatus[d_first & !r_first] <- 2L
  fstatus[r_first & d_first]  <- 2L  # ties -> death first, project convention
  list(.ftime = ftime, .fstatus = fstatus)
}

.holdout_calibrate_readmit_from_raw <- function(
    results_boot, times = c(6, 12, 36, 60),
    observed = c("aalen-johansen", "km"),
    g = 10L, min_bin_n = 20L, span = 0.75, verbose = TRUE) {
  # `observed` picks the reference used for the OBSERVED side of the curve:
  #   "aalen-johansen" (default): competing-risk-aware CIF, death handled as a
  #     genuine competing event. This is the project's principal convention
  #     (2026-07-15 review: kept fixed for all three predicted arms so the
  #     net-risk arm's miscalibration against the TRUE incidence is visible).
  #   "km": 1-KM treating death as ordinary censoring, i.e. the readmission-
  #     only observed reference that matches net risk's OWN assumption. Added
  #     2026-07-15 on request, as an additional diagnostic: pairing net risk
  #     with a KM (not AJ) observed reference asks "does net risk calibrate
  #     well against the estimand it actually targets?", separate from "how
  #     far off is it from the true competing-risk CIF?" (the AJ comparison).
  #     Meaningful for ANY arm (net risk, CIF-updated2, CIF-rule2), not only
  #     net risk, though net risk vs KM is the most directly interpretable
  #     pairing since both share the same death-as-censoring assumption.
  observed <- match.arg(observed)
  rp <- results_boot$raw_predictions
  metrics_all <- list(); curves_all <- list()
  for (item in rp) {
    r <- item$readmission; d <- item$death
    stopifnot(is.null(r$error), is.null(d$error))
    et <- as.numeric(item$eval_times)
    cols <- match(times, et)
    stopifnot(all(!is.na(cols)))
    pred_mat <- 1 - as.matrix(r$surv_val_matrix)[, cols, drop = FALSE]

    if (identical(observed, "aalen-johansen")) {
      fe <- .holdout_cr_first_event(r$y_val, d$y_val)
      obs_overall_fun <- function(h) .vhm_aj_cif(fe$.ftime, fe$.fstatus, h)
      obs_by_bin_fun <- function(h, idx) .vhm_aj_cif(fe$.ftime[idx], fe$.fstatus[idx], h)
    } else {
      rt <- as.numeric(r$y_val$time); re <- to01(r$y_val$event)
      obs_overall_fun <- function(h) .vhm_km_risk(rt, re, h)
      obs_by_bin_fun <- function(h, idx) .vhm_km_risk(rt[idx], re[idx], h)
    }

    for (j in seq_along(times)) {
      h <- times[j]; pv <- pred_mat[, j]
      res <- .vhm_calibrate_one(pv, h, obs_overall_fun(h),
        observed_by_bin_fun = function(idx) obs_by_bin_fun(h, idx),
        g = g, min_bin_n = min_bin_n, span = span)
      res$metrics$imp <- item$imp_idx %||% NA_integer_
      metrics_all[[length(metrics_all) + 1L]] <- res$metrics
      if (!is.null(res$curve)) curves_all[[length(curves_all) + 1L]] <- res$curve
    }
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  out <- .vhm_pool(metrics_all, curves_all, length(rp))
  out$risk <- "readmission"
  out$method <- if (identical(observed, "aalen-johansen")) {
    "raw_predictions (net risk or injected joint CIF) + Aalen-Johansen observed (held-out 20%)"
  } else {
    "raw_predictions (net risk or injected joint CIF) + Kaplan-Meier observed, death as censoring (held-out 20%)"
  }
  out
}

# Bootstrap-CI counterpart of .holdout_calibrate_readmit_from_raw(): MI-pools
# the predicted risk across imputations (model fixed), then resamples
# VALIDATION ROWS B times, exactly mirroring bootstrap_calibration_indices()
# in validate_holdout_metrics.R but reading the predicted risk from
# `raw_predictions` instead of refitting CSC per imputation.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

.holdout_bootstrap_calibration_readmit_from_raw <- function(
    results_boot, times = c(6, 12, 36, 60), B = 500L, seed = 2125L,
    model_label = "model", observed = c("aalen-johansen", "km"),
    g = 10L, min_bin_n = 20L, span = 0.75,
    parallel = FALSE, verbose = TRUE) {
  # See .holdout_calibrate_readmit_from_raw()'s `observed` argument for the
  # rationale (AJ = principal, competing-risk-aware; km = death-as-censoring
  # reference matching net risk's own assumption, added 2026-07-15).
  observed <- match.arg(observed)
  rp <- results_boot$raw_predictions
  n_imp <- length(rp)
  et <- as.numeric(rp[[1]]$eval_times)
  cols <- match(times, et)
  stopifnot(all(!is.na(cols)))
  n_val <- nrow(as.matrix(rp[[1]]$readmission$surv_val_matrix))

  # --- MI-pooled predicted risk (model fixed, already-frozen predictions) ---
  pred_sum <- matrix(0, n_val, length(times))
  for (item in rp) {
    pred_sum <- pred_sum + (1 - as.matrix(item$readmission$surv_val_matrix)[, cols, drop = FALSE])
  }
  pred_mat <- pred_sum / n_imp

  # --- validation outcomes: identical across imputations by MI design ---
  if (identical(observed, "aalen-johansen")) {
    fe <- .holdout_cr_first_event(rp[[1]]$readmission$y_val, rp[[1]]$death$y_val)
    ftime <- fe$.ftime; fstatus <- fe$.fstatus
    obs_fun <- .vhm_aj_cif
  } else {
    ftime <- as.numeric(rp[[1]]$readmission$y_val$time)
    fstatus <- to01(rp[[1]]$readmission$y_val$event)
    obs_fun <- .vhm_km_risk
  }

  cal_pass <- function(pred, ft, fs, h) {
    m <- .vhm_calibrate_one(pred, h, obs_fun(ft, fs, h),
           observed_by_bin_fun = function(ii) obs_fun(ft[ii], fs[ii], h),
           g = g, min_bin_n = min_bin_n, span = span)$metrics
    c(ici = m$ici, ece = m$ece, eo = m$eo_ratio)
  }
  one_rep <- function(b, pj, h, j) {
    set.seed(seed + j * 100000L + b, kind = "Mersenne-Twister")
    idx <- sample.int(n_val, n_val, replace = TRUE)
    cal_pass(pj[idx], ftime[idx], fstatus[idx], h)
  }
  use_par <- isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)
  rows <- lapply(seq_along(times), function(j) {
    h <- times[j]; pj <- pred_mat[, j]
    pt <- cal_pass(pj, ftime, fstatus, h)
    bm <- if (use_par) {
      future.apply::future_sapply(seq_len(B), one_rep, pj = pj, h = h, j = j, future.seed = TRUE)
    } else {
      vapply(seq_len(B), one_rep, numeric(3), pj = pj, h = h, j = j)
    }
    qi <- function(x) stats::quantile(x, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    data.frame(model = model_label, risk = "readmission", horizon = h,
      ici = pt["ici"], ici_lo = qi(bm["ici", ])[1], ici_hi = qi(bm["ici", ])[2],
      ece = pt["ece"], ece_lo = qi(bm["ece", ])[1], ece_hi = qi(bm["ece", ])[2],
      eo  = pt["eo"],  eo_lo  = qi(bm["eo", ])[1],  eo_hi  = qi(bm["eo", ])[2],
      B = B, row.names = NULL)
  })
  do.call(rbind, rows)
}
