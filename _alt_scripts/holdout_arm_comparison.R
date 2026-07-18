# =============================================================================
# holdout_arm_comparison.R
#
# Paired comparison of the three readmission "arms" built in point 3 of the
# 2026-07-15 review session (net risk = 1-S(t); joint CIF paired with
# best_perf1$death `updated2`; joint CIF paired with best_perf2$death
# `rule2`): point difference, a closed-form infinitesimal-jackknife (IJ)
# confidence interval, a paired patient bootstrap confidence interval (same
# resampled patients for both arms in every replicate, so the difference is
# genuinely paired, not two marginal CIs compared informally), and each arm's
# own coefficient of variation (CV) as a SECONDARY column (per Andres'
# explicit caveat 2026-07-15: CV is unstable at early horizons where risk is
# near zero, so it must not lead the table).
#
# SCOPE (2026-07-15): this file covers PREDICTED-PROBABILITY differences
# (what feeds calibration's mean_pred) and IBS differences, because both
# decompose exactly as an average of a PER-PATIENT contribution -- for a
# sample mean (or a paired difference of two sample means), the
# infinitesimal-jackknife variance and the classical paired-difference
# variance sd(a-b)/sqrt(n) are the SAME closed form, not an approximation to
# each other. DCA net benefit and the threshold metrics (Sens/Spec/PPV/NPV)
# do NOT decompose this way (net benefit's "event risk among positives" is
# itself a Kaplan-Meier/Aalen-Johansen estimate over a thresholded subgroup,
# not a per-patient linear average), so they have no comparably simple
# closed-form IJ here and are OUT OF SCOPE for this file. If a paired
# comparison of DCA/threshold arms is wanted, it needs a separate
# bootstrap-only (no IJ) extension -- flagged as a follow-up, not built here.
#
# Depends: .holdout_pool_dualscore() from evaluate_dual_cox_holdout_dualscore.R
# (predicted-probability pooling) and the internal .ibsb_* helpers from
# ibs_window_bootstrap_holdout.R (IBS per-patient contributions). Both files
# must already be sourced.
# =============================================================================

local({
  required_fns <- c(".holdout_pool_dualscore", ".ibsb_contrib_readmit_aj",
                    ".ibsb_contrib_readmit_ipcw", ".ibsb_cr", ".ibsb_G_fun")
  missing_fns <- required_fns[!vapply(required_fns, exists, logical(1), mode = "function")]
  if (length(missing_fns) > 0L) {
    stop(
      "holdout_arm_comparison.R requires evaluate_dual_cox_holdout_dualscore.R AND ",
      "ibs_window_bootstrap_holdout.R to be sourced first. Missing: ",
      paste(missing_fns, collapse = ", "),
      call. = FALSE
    )
  }
})

# -----------------------------------------------------------------------------
# 1. General paired-difference engine: point diff, closed-form IJ CI, paired
#    bootstrap CI, centering-bias diagnostic (bootstrap mean vs point, same
#    diagnostic already used for the project's ΔC bootstrap checks), and each
#    arm's own CV. `contrib_A`/`contrib_B` must be per-patient vectors for
#    the SAME patients in the SAME order (paired by construction).
# -----------------------------------------------------------------------------
.holdout_paired_diff_ij_boot <- function(contrib_A, contrib_B, B = 500L, seed = 2125L) {
  stopifnot(length(contrib_A) == length(contrib_B))
  ok <- is.finite(contrib_A) & is.finite(contrib_B)
  a <- contrib_A[ok]; b <- contrib_B[ok]; n <- length(a)
  if (n < 2L) {
    return(data.frame(
      n = n, mean_A = NA_real_, mean_B = NA_real_, diff = NA_real_,
      se_ij = NA_real_, ij_lower = NA_real_, ij_upper = NA_real_,
      boot_lower = NA_real_, boot_upper = NA_real_, boot_mean = NA_real_,
      centering_bias = NA_real_, cv_A = NA_real_, cv_B = NA_real_,
      excludes_zero_ij = NA, excludes_zero_boot = NA, stringsAsFactors = FALSE
    ))
  }
  d <- a - b
  point_A <- mean(a); point_B <- mean(b); point_diff <- point_A - point_B

  # Closed-form IJ / delta-method SE for a paired mean difference. For a
  # linear statistic (a sample mean, or a difference of two sample means),
  # the infinitesimal-jackknife variance and the classical variance
  # estimator sd(d)^2/n are algebraically identical -- this IS the IJ
  # result, not a stand-in for it.
  se_ij <- stats::sd(d) / sqrt(n)
  z <- stats::qnorm(0.975)
  ij_lower <- point_diff - z * se_ij
  ij_upper <- point_diff + z * se_ij

  set.seed(seed, kind = "Mersenne-Twister")
  boot_diff <- vapply(seq_len(B), function(bi) {
    idx <- sample.int(n, n, replace = TRUE)
    mean(a[idx]) - mean(b[idx])
  }, numeric(1))
  q <- stats::quantile(boot_diff, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)

  data.frame(
    n = n,
    mean_A = point_A, mean_B = point_B, diff = point_diff,
    se_ij = se_ij, ij_lower = ij_lower, ij_upper = ij_upper,
    boot_lower = q[1], boot_upper = q[2],
    boot_mean = mean(boot_diff, na.rm = TRUE),
    centering_bias = mean(boot_diff, na.rm = TRUE) - point_diff,
    cv_A = stats::sd(a) / point_A, cv_B = stats::sd(b) / point_B,
    excludes_zero_ij = (ij_lower > 0) || (ij_upper < 0),
    excludes_zero_boot = (q[1] > 0) || (q[2] < 0),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# 2. Predicted-probability comparison across the three readmission arms, at
#    a set of horizons. `results_boot_arms` is a NAMED list, e.g.
#    list(net_risk = results_boot_val_bp1, cif_bp1 = inj_bp1, cif_bp2 = inj_bp2).
#    Compares every unordered pair of arms.
# -----------------------------------------------------------------------------
.holdout_compare_predicted_prob <- function(results_boot_arms, val_list, times,
                                            B = 500L, seed = 2125L, verbose = TRUE) {
  stopifnot(length(results_boot_arms) >= 2L, !is.null(names(results_boot_arms)))
  pooled <- lapply(results_boot_arms, .holdout_pool_dualscore, val_list = val_list, risk_name = "readmission")
  nm <- names(results_boot_arms)
  pairs <- utils::combn(nm, 2L, simplify = FALSE)

  rows <- lapply(pairs, function(pr) {
    pA <- pooled[[pr[1]]]; pB <- pooled[[pr[2]]]
    stopifnot(identical(pA$id, pB$id))
    lapply(times, function(h) {
      colA <- match(h, pA$eval_times); colB <- match(h, pB$eval_times)
      stopifnot(!is.na(colA), !is.na(colB))
      out <- .holdout_paired_diff_ij_boot(pA$risk[, colA], pB$risk[, colB], B = B, seed = seed)
      out$horizon <- h; out$arm_A <- pr[1]; out$arm_B <- pr[2]; out$quantity <- "predicted_probability"
      if (verbose) cat(sprintf(
        "  predicted prob | %s vs %s @ %sm: diff=%+.4f  IJ[%+.4f,%+.4f]  boot[%+.4f,%+.4f]\n",
        pr[1], pr[2], h, out$diff, out$ij_lower, out$ij_upper, out$boot_lower, out$boot_upper))
      out
    })
  })
  out <- do.call(rbind, unlist(rows, recursive = FALSE))
  cols <- c("quantity", "arm_A", "arm_B", "horizon", "n", "mean_A", "mean_B", "diff",
            "se_ij", "ij_lower", "ij_upper", "boot_lower", "boot_upper", "boot_mean",
            "centering_bias", "excludes_zero_ij", "excludes_zero_boot", "cv_A", "cv_B")
  rownames(out) <- NULL
  out[, cols]
}

# -----------------------------------------------------------------------------
# 3. IBS comparison across the three readmission arms, reusing the internal
#    per-patient contribution matrices already computed by
#    ibs_window_bootstrap_holdout.R (.ibsb_contrib_readmit_aj/_ipcw), instead
#    of re-deriving the IPCW weighting here. Returns, per patient, the SAME
#    windowed-IBS contribution the point estimate in ibs_window_bootstrap_
#    core() averages over -- i.e. mean(contrib) == that function's point
#    estimate for the same window (cross-checked in the smoke test).
# -----------------------------------------------------------------------------
.holdout_ibs_patient_contrib <- function(item, eval_times, horizon, readmit_method, g_min = 0.05, eps = 1e-8) {
  r <- item$readmission; d <- item$death
  grid_full <- sort(unique(intersect(as.numeric(item$eval_times), eval_times)))
  surv_r <- as.matrix(r$surv_val_matrix)[, match(grid_full, as.numeric(item$eval_times)), drop = FALSE]

  if (identical(readmit_method, "ipcw")) {
    cc <- .ibsb_contrib_readmit_ipcw(
      surv_val = surv_r,
      time_val = as.numeric(r$y_val$time), event_val = r$y_val$event,
      time_train = as.numeric(r$y_train$time), event_train = r$y_train$event,
      grid = grid_full, g_min = g_min, eps = eps)
  } else {
    cc <- .ibsb_contrib_readmit_aj(
      surv_val = surv_r,
      rt_val = as.numeric(r$y_val$time), re_val = r$y_val$event,
      dt_val = as.numeric(d$y_val$time), de_val = d$y_val$event,
      rt_tr  = as.numeric(r$y_train$time), re_tr = r$y_train$event,
      dt_tr  = as.numeric(d$y_train$time), de_tr = d$y_train$event,
      grid = grid_full, g_min = g_min, eps = eps)
  }
  # windowed trapezoid weights over [min(grid), horizon], per-patient (row-wise)
  # instead of collapsing to a column mean first -- mirrors .ibsb_windows_from_C
  # but keeps one value PER PATIENT.
  t_min <- min(eval_times)
  if (horizon <= t_min || !any(abs(cc$grid - horizon) < 1e-8)) {
    return(rep(NA_real_, nrow(cc$C)))
  }
  sel <- cc$grid <= horizon
  if (sum(sel) < 2L) return(rep(NA_real_, nrow(cc$C)))
  g <- cc$grid[sel]; Cs <- cc$C[, sel, drop = FALSE]
  dg <- diff(g)
  trapz_w <- c(dg[1], dg[-1] + head(dg, -1), tail(dg, 1)) / 2   # trapezoid node weights
  as.numeric(Cs %*% trapz_w) / (horizon - t_min)
}

.holdout_compare_ibs <- function(results_boot_arms, eval_times, times,
                                 readmit_method = c("aalen-johansen", "ipcw"),
                                 B = 500L, seed = 2125L, verbose = TRUE) {
  readmit_method <- match.arg(readmit_method)
  stopifnot(length(results_boot_arms) >= 2L, !is.null(names(results_boot_arms)))
  nm <- names(results_boot_arms)

  # Per-patient contribution, POOLED (averaged) across imputations, per arm/horizon.
  pooled_contrib <- lapply(results_boot_arms, function(rb) {
    rp <- rb$raw_predictions
    sapply(times, function(h) {
      per_imp <- sapply(rp, .holdout_ibs_patient_contrib,
                        eval_times = eval_times, horizon = h, readmit_method = readmit_method)
      rowMeans(per_imp)
    })
  })

  pairs <- utils::combn(nm, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(pr) {
    lapply(seq_along(times), function(j) {
      h <- times[j]
      a <- pooled_contrib[[pr[1]]][, j]; b <- pooled_contrib[[pr[2]]][, j]
      out <- .holdout_paired_diff_ij_boot(a, b, B = B, seed = seed)
      out$horizon <- h; out$arm_A <- pr[1]; out$arm_B <- pr[2]; out$quantity <- "ibs"
      out$readmit_method <- readmit_method
      if (verbose) cat(sprintf(
        "  IBS (%s) | %s vs %s @ %sm: diff=%+.4f  IJ[%+.4f,%+.4f]  boot[%+.4f,%+.4f]\n",
        readmit_method, pr[1], pr[2], h, out$diff, out$ij_lower, out$ij_upper, out$boot_lower, out$boot_upper))
      out
    })
  })
  out <- do.call(rbind, unlist(rows, recursive = FALSE))
  cols <- c("quantity", "readmit_method", "arm_A", "arm_B", "horizon", "n", "mean_A", "mean_B", "diff",
            "se_ij", "ij_lower", "ij_upper", "boot_lower", "boot_upper", "boot_mean",
            "centering_bias", "excludes_zero_ij", "excludes_zero_boot", "cv_A", "cv_B")
  rownames(out) <- NULL
  out[, cols]
}

# -----------------------------------------------------------------------------
# 4. Wolbers competing-risks concordance (point 6, 2026-07-15): C of Uno
#    truncated for competing risks, using the CIF as the ranking marker and
#    death as a genuine competing event (not censoring) via the standard
#    "push death to a time far beyond the horizon" trick, so
#    survival::concordance() treats readmission as the only event that can
#    be concordant/discordant and death-before-readmission pairs are handled
#    per Wolbers' definition. Same trick already used by the project's other
#    paired-ΔC code (see [[dualcox-concordance-coherence-variants]]): both
#    arms' CIF are passed to a SINGLE concordance() call so the paired IC for
#    the difference comes directly from that call's own covariance matrix
#    (infinitesimal jackknife), with no separate bootstrap needed.
# -----------------------------------------------------------------------------
.holdout_wolbers_paired_ij <- function(cif_pool_A, cif_pool_B, val_list, horizon, competing_status_fun) {
  stopifnot(identical(cif_pool_A$id, cif_pool_B$id))
  colA <- match(horizon, cif_pool_A$eval_times); colB <- match(horizon, cif_pool_B$eval_times)
  stopifnot(!is.na(colA), !is.na(colB))

  cr <- competing_status_fun()  # list(time, status): status 0=censor,1=readmit,2=death
  ok <- is.finite(cr$time) & !is.na(cr$status) &
    is.finite(cif_pool_A$risk[, colA]) & is.finite(cif_pool_B$risk[, colB])
  time <- cr$time[ok]; status <- cr$status[ok]
  risk_A <- cif_pool_A$risk[ok, colA]; risk_B <- cif_pool_B$risk[ok, colB]

  big_time <- max(time, na.rm = TRUE) * 10
  time_w <- ifelse(status == 2L, big_time, time)   # death pushed past the horizon: a definitive non-event
  event_w <- as.integer(status == 1L)

  d <- data.frame(time_w = time_w, event_w = event_w, risk_A = risk_A, risk_B = risk_B)
  fit <- survival::concordance(
    survival::Surv(time_w, event_w) ~ risk_A + risk_B, data = d,
    timewt = "n/G2", reverse = TRUE, ymax = horizon
  )
  est <- unname(fit$concordance); V <- fit$var
  z <- stats::qnorm(0.975)
  se_c <- sqrt(pmax(diag(V), 0))
  delta <- est[1] - est[2]
  se_delta <- sqrt(max(V[1, 1] + V[2, 2] - 2 * V[1, 2], 0))

  data.frame(
    horizon = horizon, n = nrow(d), events_readmit = sum(status == 1L & time <= horizon),
    events_death_competing = sum(status == 2L & time <= horizon),
    C_A = est[1], C_A_lower = max(0, est[1] - z * se_c[1]), C_A_upper = min(1, est[1] + z * se_c[1]),
    C_B = est[2], C_B_lower = max(0, est[2] - z * se_c[2]), C_B_upper = min(1, est[2] + z * se_c[2]),
    delta_C = delta, se_delta = se_delta,
    delta_C_lower = delta - z * se_delta, delta_C_upper = delta + z * se_delta,
    excludes_zero = (delta - z * se_delta > 0) || (delta + z * se_delta < 0),
    stringsAsFactors = FALSE
  )
}

# Convenience wrapper: Wolbers C for the two readmission CIF arms (bp1 vs
# bp2 death pairing) across a set of horizons, from already-injected
# results_boot objects.
.holdout_compare_wolbers <- function(inj_bp1, inj_bp2, val_list, horizons, verbose = TRUE) {
  pool_A <- .holdout_pool_dualscore(inj_bp1, val_list, "readmission")
  pool_B <- .holdout_pool_dualscore(inj_bp2, val_list, "readmission")

  competing_status_fun <- function() {
    d1 <- inj_bp1$raw_predictions[[1]]$readmission$y_val
    d2 <- inj_bp1$raw_predictions[[1]]$death$y_val
    rt <- as.numeric(d1$time); re <- as.integer(d1$event)
    dt <- as.numeric(d2$time); de <- as.integer(d2$event)
    ftime <- pmin(rt, dt)
    r1 <- !is.na(re) & re == 1L & is.finite(ftime) & abs(rt - ftime) <= 1e-8
    d1e <- !is.na(de) & de == 1L & is.finite(ftime) & abs(dt - ftime) <= 1e-8
    fstatus <- rep(0L, length(ftime))
    fstatus[r1 & !d1e] <- 1L
    fstatus[d1e & !r1] <- 2L
    fstatus[r1 & d1e]  <- 2L
    list(time = ftime, status = fstatus)
  }

  rows <- lapply(horizons, function(h) {
    out <- .holdout_wolbers_paired_ij(pool_A, pool_B, val_list, h, competing_status_fun)
    if (verbose) cat(sprintf(
      "  Wolbers C | cif_bp1=%.4f cif_bp2=%.4f  dC=%+.4f [%+.4f,%+.4f]  @%sm\n",
      out$C_A, out$C_B, out$delta_C, out$delta_C_lower, out$delta_C_upper, h))
    out
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
