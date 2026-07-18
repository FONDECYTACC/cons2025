# =============================================================================
# delta_c_holdout.R
# Paired delta C (Uno) on ABSOLUTE horizon-specific predicted risk 1 - S(t) for
# the held-out 20% test set, mirroring the prediction225 chunk
# `#| label: paired-delta-c-absolute-risk`.
#
# Why absolute risk and not the linear predictor: once the baseline hazards are
# stratified (strata(plan_type_strata), strata(tr_outcome_..._undet)), 1 - S(t)
# is NOT a monotone transform of lp, so the concordance ranking changes with the
# horizon and differs from the lp ranking. The frozen-model performance the
# manuscript reports is defined on absolute risk (options(dualcox.concordance_score
# = "risk") in the CV pipeline), so the held-out delta C must use the same score.
#
# This module only READS results_boot_val[[model]]$raw_predictions (surv_val_matrix,
# y_val per imputation). It never refits. The two models are compared PAIRED: the
# same resampled subjects feed both, so the IPCW censoring distribution G cancels
# in the difference (freeze_g not needed).
#
# Companion to validate_holdout_metrics.R / run_validation_holdout.R.
# Depends: survival.
# =============================================================================

suppressWarnings(stopifnot(requireNamespace("survival", quietly = TRUE)))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.dch_to01 <- function(x) as.integer(as.numeric(x) > 0)

# -----------------------------------------------------------------------------
# Reassemble per-subject predictions for one outcome, pooled over imputations.
# Mirrors .cv_extract() from prediction225, but hardened for the held-out layout:
#   * original_val_idx in the held-out engine is seq_len(nrow(df_te)) (the FULL
#     val rows), while surv_val_matrix / lp_val are over the KEPT rows. If any row
#     was dropped for unseen strata (dropped_rows > 0) the positional placement
#     would silently misalign readmission vs death, so we ASSERT dropped_rows == 0
#     and that lengths match. (Held-out fits on ~70k train rows normally drop 0.)
#   * every imputation must carry the same subjects in the same order.
# -----------------------------------------------------------------------------
.dch_extract <- function(results_boot, outcome = c("readmission", "death")) {
  outcome <- match.arg(outcome)
  rp <- results_boot$raw_predictions
  if (is.null(rp) || !length(rp))
    stop("results_boot has no $raw_predictions (stripped object or a calibration result).",
         call. = FALSE)
  et <- as.numeric(results_boot$config$eval_times %||% rp[[1]]$eval_times)
  N  <- max(vapply(rp, function(b) max(as.integer(b$original_val_idx)), integer(1)))
  imps <- sort(unique(vapply(rp, function(b) as.integer(b$imp_idx), integer(1))))

  time_v <- rep(NA_real_, N); event_v <- rep(NA_real_, N)
  S_imp <- list()
  for (im in imps) {
    S <- matrix(NA_real_, N, length(et))
    for (b in rp) {
      if (as.integer(b$imp_idx) != im) next
      o <- b[[outcome]]
      if (is.null(o) || !is.null(o$error) || is.null(o$surv_val_matrix)) next

      # --- alignment guards (point #3): positional mapping is only valid when
      #     nothing was dropped and the pieces are the same length. ---
      dr <- o$dropped_rows %||% 0L
      if (!identical(as.integer(dr), 0L))
        stop(sprintf("outcome '%s', imp %d: dropped_rows = %d (> 0). Positional row mapping is unsafe; align by a real row_id before pooling.",
                     outcome, im, as.integer(dr)), call. = FALSE)
      idx <- as.integer(b$original_val_idx)
      sm  <- as.matrix(o$surv_val_matrix)
      if (nrow(sm) != length(idx))
        stop(sprintf("outcome '%s', imp %d: surv_val_matrix has %d rows but original_val_idx has %d; cannot place positionally.",
                     outcome, im, nrow(sm), length(idx)), call. = FALSE)

      S[idx, ] <- sm
      yv <- o$y_val
      time_v[idx]  <- as.numeric(yv[, 1])
      event_v[idx] <- as.numeric(yv[, 2])
    }
    S_imp[[length(S_imp) + 1L]] <- S
  }
  list(time = time_v, event = event_v,
       S = Reduce(`+`, S_imp) / length(S_imp),   # MI-pooled survival
       eval_times = et)
}

# -----------------------------------------------------------------------------
# Uno's C on an absolute-risk score at one horizon (timewt = n/G2), the same call
# as .safe_uno_c_risk in prediction225.
# -----------------------------------------------------------------------------
.dch_uno_c_risk <- function(time, event, risk, horizon) {
  ok <- is.finite(time) & is.finite(event) & is.finite(risk)
  time <- as.numeric(time[ok]); event <- as.numeric(event[ok]); risk <- as.numeric(risk[ok])
  if (length(time) < 2L || sum(event == 1 & time <= horizon) < 2L) return(NA_real_)
  tryCatch(
    survival::concordance(survival::Surv(time, event) ~ risk,
                          timewt = "n/G2", reverse = TRUE, ymax = horizon)$concordance,
    error = function(e) NA_real_)
}

.dch_match_horizon <- function(horizon, eval_times, tol = 1e-8) {
  idx <- which(abs(as.numeric(eval_times) - horizon) < tol)
  if (length(idx) != 1L)
    stop(sprintf("Horizon %s not found exactly once in eval_times: %s",
                 horizon, paste(eval_times, collapse = ", ")), call. = FALSE)
  idx
}

# -----------------------------------------------------------------------------
# Paired delta C at one horizon between two extracted marker sets A, Z.
# risk = 1 - S(horizon), clamped to [0, 1]. Same subjects resampled for both.
# -----------------------------------------------------------------------------
.dch_delta_c_absrisk <- function(A, Z, horizon, B = 500L, seed = 2125L) {
  risk_A <- pmin(pmax(1 - as.numeric(A$S[, .dch_match_horizon(horizon, A$eval_times)]), 0), 1)
  risk_Z <- pmin(pmax(1 - as.numeric(Z$S[, .dch_match_horizon(horizon, Z$eval_times)]), 0), 1)

  common <- is.finite(A$time) & is.finite(A$event) & is.finite(Z$time) &
            is.finite(Z$event) & is.finite(risk_A) & is.finite(risk_Z)
  if (sum(common) < 2L)
    stop(sprintf("Too few common observations at %s months.", horizon), call. = FALSE)
  if (any(abs(A$time[common] - Z$time[common]) > 1e-8, na.rm = TRUE) ||
      any(A$event[common] != Z$event[common], na.rm = TRUE))
    stop("Outcome or follow-up differs between the paired model objects.", call. = FALSE)

  time <- as.numeric(A$time[common]); event <- as.numeric(A$event[common])
  risk_A <- risk_A[common]; risk_Z <- risk_Z[common]
  n <- length(time)

  C_A <- .dch_uno_c_risk(time, event, risk_A, horizon)
  C_Z <- .dch_uno_c_risk(time, event, risk_Z, horizon)
  delta_obs <- C_A - C_Z

  set.seed(seed)
  boot <- vapply(seq_len(B), function(b) {
    s <- sample.int(n, n, replace = TRUE)
    .dch_uno_c_risk(time[s], event[s], risk_A[s], horizon) -
      .dch_uno_c_risk(time[s], event[s], risk_Z[s], horizon)
  }, numeric(1))

  ok <- is.finite(boot); nb <- sum(ok)
  if (nb < ceiling(0.90 * B))
    warning(sprintf("Only %d of %d bootstrap replicates valid at %s months.", nb, B, horizon),
            call. = FALSE)
  ci <- if (nb >= 20L) stats::quantile(boot[ok], c(0.025, 0.975), names = FALSE, na.rm = TRUE)
        else c(NA_real_, NA_real_)
  bmean <- mean(boot[ok], na.rm = TRUE)

  data.frame(
    horizon = horizon, n = n,
    events_by_horizon = sum(event == 1 & time <= horizon),
    C_A = C_A, C_B = C_Z, delta_C = delta_obs,
    delta_C_lower = ci[1], delta_C_upper = ci[2],
    bootstrap_mean = bmean, bootstrap_median = stats::median(boot[ok], na.rm = TRUE),
    centering_bias = bmean - delta_obs,
    point_inside_ci = is.finite(ci[1]) && is.finite(ci[2]) &&
                      delta_obs >= ci[1] && delta_obs <= ci[2],
    excludes_zero = is.finite(ci[1]) && is.finite(ci[2]) && (ci[1] > 0 || ci[2] < 0),
    bootstrap_valid = nb, bootstrap_requested = B,
    stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# Public: paired delta C across horizons between two held-out results_boot_val
# objects, for one outcome. delta_C > 0 favours model A.
# -----------------------------------------------------------------------------
delta_c_holdout_absrisk <- function(results_boot_A, results_boot_B,
                                    outcome = c("death", "readmission"),
                                    horizons = c(6, 12, 36, 60),
                                    B = 500L, seed = 2125L,
                                    label_A = "model_A", label_B = "model_B",
                                    verbose = TRUE) {
  outcome <- match.arg(outcome)
  A <- .dch_extract(results_boot_A, outcome)
  Z <- .dch_extract(results_boot_B, outcome)
  out <- do.call(rbind, lapply(seq_along(horizons), function(j) {
    r <- .dch_delta_c_absrisk(A, Z, horizons[j], B = B, seed = seed + j)
    r$outcome <- outcome; r$model_A <- label_A; r$model_B <- label_B
    r$favours <- if (is.finite(r$delta_C_lower) && r$delta_C_lower > 0) label_A
                 else if (is.finite(r$delta_C_upper) && r$delta_C_upper < 0) label_B
                 else "no clear difference"
    if (isTRUE(verbose))
      cat(sprintf("[%s] %s vs %s @ %2d m: C_A=%.4f  C_B=%.4f  dC=%+.4f [%+.4f, %+.4f]  %s\n",
                  outcome, label_A, label_B, horizons[j], r$C_A, r$C_B, r$delta_C,
                  r$delta_C_lower, r$delta_C_upper,
                  if (r$excludes_zero) paste0("favours ", r$favours) else "ns"))
    r
  }))
  cols <- c("outcome", "model_A", "model_B", "horizon", "n", "events_by_horizon",
            "C_A", "C_B", "delta_C", "delta_C_lower", "delta_C_upper",
            "excludes_zero", "favours", "centering_bias", "point_inside_ci",
            "bootstrap_valid", "bootstrap_requested")
  out <- out[, cols]
  rownames(out) <- NULL
  out
}
