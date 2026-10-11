# =============================================================================
# ibs_classical_holdout.R
#
# Classical (Graf et al. 1999) integrated Brier score on the held-out 20% test
# set, plus a PAIRED bootstrap of delta IBS between the two mortality models.
#
# WHY THIS EXISTS
# ---------------
# prediction23_converted_mod reports IBS through ibs_ipcw_train(), which uses
# EVAL_TIMES itself as the quadrature grid. With EVAL_TIMES = c(3,6,12,36,60)
# that means the Brier curve is trapezoided over five nodes, the integral is
# anchored at 3 months rather than 0, and it is normalised by
# max(times) - min(times) rather than by the horizon. So the reported quantity is
#
#     (1 / (tau - 3)) * integral_3^tau BS(t) dt        <- what pred23 reports
#
# whereas the standard IBS every other paper and every standard tool
# (pec::pec, riskRegression::Score) computes is
#
#     (1 / tau) * integral_0^tau BS(t) dt              <- what this file computes
#
# Two separate departures are folded into that: a coarse 5-node quadrature, which
# over-states the integral because the Brier curve is convex and rising, and a
# non-zero lower anchor, which is a different estimand. This module reports the
# classical version so the manuscript can quote a number that is comparable with
# published IBS values, WITHOUT touching anything the notebook already computes.
#
# WHAT IS HELD FIXED
# ------------------
# Everything except the integral. The censoring survival G is still estimated on
# the TRAINING fold and floored at g_min = 0.05, exactly as ibs_ipcw_train() does,
# and the IPCW weight construction is identical. Only the grid, the lower anchor
# and the divisor change. This is deliberate: it isolates the integration choice
# from the weighting choice, so the difference cannot be blamed on the weights.
#
# MI HANDLING
# -----------
# Survival is pooled across the five imputations by averaging S(t) before the
# metric is computed, matching how delta_c_holdout.R pools for the paired delta C.
# reproduce_pred23_ibs() instead averages the per-imputation IBS, because that is
# how the notebook's own summary aggregates, and it is used only as a control.
#
# PAIRED BOOTSTRAP
# ----------------
# The two mortality models are compared on the SAME resampled subjects, so the
# censoring distribution and the sampling noise largely cancel in the difference.
# The implementation is exact but fast: because G comes from the training fold it
# does not change under patient resampling of the test set, so each subject's
# contribution to the Brier score at each grid point is a FIXED number. Those
# contributions are precomputed once into an n x n_horizons matrix of already
# trapezoided values, after which one bootstrap replicate costs a single
# matrix-vector product. There is no approximation: resampling with replacement
# is applied as integer multiplicities, so the result is identical to rebuilding
# the resampled cohort each time.
#
# Public API:
#   ibs_classical_grid(max_horizon, step)
#   fit_death_grid_predictions(formula, train_list, val_list, grid)
#   ibs_classical_point(train_y, test_y, surv_te, grid, horizons)
#   ibs_classical_delta_boot(A, Z, horizons, B, seed, label_A, label_Z)
#   reproduce_pred23_ibs(formula, train_list, val_list, eval_times)
#
# Depends: survival, pec. Requires ibs_ipcw_train()/to01() from
# evaluate_dual_cox_python_style_boot.R for the control function only.
# =============================================================================

suppressWarnings({
  stopifnot(requireNamespace("survival", quietly = TRUE))
  stopifnot(requireNamespace("pec", quietly = TRUE))
})
suppressWarnings(suppressMessages(library(survival)))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
.ibc_to01 <- function(x) if (is.logical(x)) as.integer(x) else as.integer(as.numeric(x) > 0)

# ---- Replicate retention for later audit ----------------------------------------
# Every bootstrap below summarises its replicates into percentiles and then discards the
# replicate matrices, which makes a later audit impossible without repeating the run. For the
# paired net-benefit chunk that is about 130 minutes. Setting
#   options(ibsclassical.keep_replicates = TRUE)
# before running makes each bootstrap deposit its raw replicate matrices, plus the seed, B and n
# needed to rebuild the resample indices, into an internal cache. Call sites need NO edit: the
# returned data frames are unchanged, so cells already written keep working either way.
# Retrieve everything with ibs_classical_replicates(), archive it with saveRDS(), and clear it
# with ibs_classical_replicates_reset().
# The resample INDICES are deliberately never stored: they are B x n integers, tens of megabytes,
# and are exactly reproducible from seed and n, which are stored instead.
# IDEMPOTENT ON PURPOSE: this file is source()-d from three separate cells in
# prediction23_converted_mod.ipynb (holdout-ibs-classical-delta-death,
# holdout-ibs-classical-readmit, holdout-ibs-classical-cif-bootstrap). An
# unconditional `.ibc_cache <- new.env(...)` here would silently DISCARD every
# replicate already cached from an earlier cell's bootstrap the moment a later
# cell re-sources this file -- caught this session before it caused the same
# silent data loss save-dca-bootstrap-replicates-salvage was built to patch
# after the fact elsewhere in this notebook.
if (!exists(".ibc_cache", inherits = TRUE) || !is.environment(.ibc_cache)) {
  .ibc_cache <- new.env(parent = emptyenv())
}
.ibc_keep <- function() isTRUE(getOption("ibsclassical.keep_replicates", FALSE))
.ibc_store <- function(key, ...) {
  if (!.ibc_keep()) return(invisible(NULL))
  v <- list(...)
  v$stored_at <- as.character(Sys.time())
  v$rebuild_indices <- "set.seed(seed); lapply(seq_len(B), function(i) sample.int(n, n, replace = TRUE))"
  assign(key, v, envir = .ibc_cache)
  invisible(NULL)
}
ibs_classical_replicates <- function() as.list(.ibc_cache)
ibs_classical_replicates_reset <- function() {
  rm(list = ls(.ibc_cache), envir = .ibc_cache); invisible(NULL)
}

# ---- 1. Evaluation grid ------------------------------------------------------
# Dense grid from 0 to max_horizon. The reporting horizons are always included so
# the trapezoid has a node exactly at each horizon and no interpolation is needed.
ibs_classical_grid <- function(max_horizon = 60, step = 0.25, horizons = NULL) {
  stopifnot(step > 0, max_horizon > 0)
  sort(unique(c(0, seq(step, max_horizon, by = step), max_horizon, horizons)))
}

# ---- 2. Censoring survival G, estimated on the TRAINING fold ------------------
# Same estimator, same g_min floor and the same order-safe lookup as the patched
# ibs_ipcw_train(): G is evaluated at sorted unique times and mapped back to the
# requested order with match(), which is the 2026-07-21 fix.
.ibc_G_train <- function(time_train, event_train, g_min = 0.05) {
  sf <- survival::survfit(survival::Surv(time_train, 1 - .ibc_to01(event_train)) ~ 1)
  tau <- { safe <- sf$time[sf$surv >= g_min]; if (length(safe)) max(safe) else Inf }
  list(
    tau = tau,
    at = function(tt) {
      req <- pmax(as.numeric(tt), 0)
      u <- sort(unique(req))
      g <- as.numeric(summary(sf, times = u, extend = TRUE)$surv)
      if (length(g) != length(u)) stop("Censoring lookup returned an unexpected length.", call. = FALSE)
      i <- match(req, u)
      if (anyNA(i)) stop("Censoring lookup could not restore request order.", call. = FALSE)
      pmax(g[i], g_min)
    })
}

# ---- 3. Per-subject Brier contributions on the grid ---------------------------
# Returns an n x length(grid_pos) matrix whose column means ARE the IPCW Brier
# score at each positive grid node. Keeping the per-subject decomposition is what
# makes the paired bootstrap cheap and exact.
.ibc_contrib <- function(G, time_te, event_te, surv_te, grid_pos, eps = 1e-8) {
  Tt <- as.numeric(time_te); Dt <- .ibc_to01(event_te)
  stopifnot(ncol(surv_te) == length(grid_pos), nrow(surv_te) == length(Tt))
  g_event <- G$at(pmax(Tt - eps, 0))
  out <- matrix(0, length(Tt), length(grid_pos))
  for (j in seq_along(grid_pos)) {
    t0 <- grid_pos[j]
    S0 <- surv_te[, j]
    Y0 <- as.numeric(Tt > t0)
    w <- ifelse(Tt <= t0 & Dt == 1L, 1 / g_event,
         ifelse(Tt > t0, 1 / G$at(t0), 0))
    out[, j] <- w * (Y0 - S0)^2
  }
  out
}

# ---- 4. Trapezoid weights, anchored at 0 --------------------------------------
# For horizon h the integral runs from 0 to h. BS(0) = 0 by construction (nobody
# has had the event and predicted risk is 0), so the origin contributes no mass
# and only the positive nodes carry weight. Returns length(grid_pos) x length(h).
.ibc_trap_weights <- function(grid_pos, horizons) {
  W <- matrix(0, length(grid_pos), length(horizons))
  for (k in seq_along(horizons)) {
    keep <- which(grid_pos <= horizons[k] + 1e-12)
    if (!length(keep)) next
    # Nodes are x_0 = 0 < x_1 < ... < x_m, with x_j = grid_pos[keep[j]] for j >= 1.
    # d[j] = x_j - x_{j-1}. The composite trapezoid puts weight d[j]/2 + d[j+1]/2 on
    # an interior node and d[m]/2 on the last one. b_0 = BS(0) = 0, so the origin's
    # own weight is never needed.
    d <- diff(c(0, grid_pos[keep]))
    m <- length(d)
    w <- d / 2
    if (m > 1L) w[seq_len(m - 1L)] <- w[seq_len(m - 1L)] + d[-1L] / 2
    W[keep, k] <- w
  }
  W
}

# ---- 5. Refit one death model on every imputation and predict on a fine grid ---
# The formulas in prediction23_converted_mod carry bare Surv()/strata() and live
# in the notebook's global environment, so coxph() detects strata as specials.
# The stratum count is ASSERTED because a strata() term silently demoted to a
# covariate is the exact failure documented on 2026-07-12.
fit_death_grid_predictions <- function(formula, train_list, val_list, grid,
                                       expected_strata = NULL, verbose = TRUE) {
  stopifnot(length(train_list) == length(val_list), length(train_list) >= 1L)
  n_imp <- length(train_list)
  S_acc <- NULL; y_te <- NULL; y_tr <- NULL
  for (i in seq_len(n_imp)) {
    fit <- survival::coxph(formula, data = train_list[[i]], ties = "efron",
                           x = TRUE, y = TRUE, model = TRUE)
    n_strata <- { bh <- survival::basehaz(fit, centered = FALSE)
                  if (is.null(bh$strata)) 1L else nlevels(droplevels(factor(bh$strata))) }
    if (!is.null(expected_strata) && n_strata != expected_strata)
      stop(sprintf("Imputation %d: %d baseline strata, expected %d. A strata() term may have been fitted as a covariate.",
                   i, n_strata, expected_strata), call. = FALSE)
    S <- pec::predictSurvProb(fit, newdata = val_list[[i]], times = grid)
    S <- matrix(as.numeric(S), nrow = nrow(val_list[[i]]), ncol = length(grid))
    S_acc <- if (is.null(S_acc)) S else S_acc + S
    if (i == 1L) {
      tv <- all.vars(formula[[2]])
      y_te <- data.frame(time = as.numeric(val_list[[i]][[tv[1]]]),
                         event = .ibc_to01(val_list[[i]][[tv[2]]]))
      y_tr <- data.frame(time = as.numeric(train_list[[i]][[tv[1]]]),
                         event = .ibc_to01(train_list[[i]][[tv[2]]]))
    }
    if (verbose) cat(".")
  }
  if (verbose) cat(sprintf(" %d imputations fitted, %d baseline strata\n", n_imp, n_strata))
  list(S = S_acc / n_imp, grid = grid, y_test = y_te, y_train = y_tr, n_imp = n_imp)
}

# ---- 6. Classical IBS point estimate -----------------------------------------
ibs_classical_point <- function(pred, horizons = c(3, 6, 12, 36, 60), g_min = 0.05) {
  grid <- pred$grid
  pos <- which(grid > 0)
  grid_pos <- grid[pos]
  G <- .ibc_G_train(pred$y_train$time, pred$y_train$event, g_min = g_min)
  usable <- horizons <= min(G$tau, max(pred$y_train$time, na.rm = TRUE))
  C <- .ibc_contrib(G, pred$y_test$time, pred$y_test$event, pred$S[, pos, drop = FALSE], grid_pos)
  W <- .ibc_trap_weights(grid_pos, horizons)
  ibs <- as.numeric(colMeans(C) %*% W) / horizons
  ibs[!usable] <- NA_real_
  list(ibs = data.frame(horizon_months = horizons, ibs = ibs, usable = usable),
       contrib_trapezoid = C %*% W, horizons = horizons, tau_ipcw = G$tau,
       brier_curve = data.frame(time_months = grid_pos, brier = colMeans(C)))
}

# ---- 7. Paired bootstrap of delta IBS ----------------------------------------
# delta = A - Z, so delta < 0 favours A (lower prediction error is better).
# One replicate is a matrix-vector product on the precomputed trapezoided
# contributions, which is algebraically identical to recomputing the Brier score
# on the resampled cohort because G is fixed by the training fold.
ibs_classical_delta_boot <- function(A, Z, B = 1000L, seed = 2125L,
                                     label_A = "A", label_Z = "Z", verbose = TRUE) {
  stopifnot(identical(A$horizons, Z$horizons))
  CA <- A$contrib_trapezoid; CZ <- Z$contrib_trapezoid
  stopifnot(nrow(CA) == nrow(CZ))
  n <- nrow(CA); h <- A$horizons
  if (verbose) cat(sprintf("Paired delta IBS bootstrap: n = %d, B = %d, horizons %s\n",
                           n, B, paste(h, collapse = ", ")))
  set.seed(seed)
  dA <- matrix(NA_real_, B, length(h)); dZ <- dA
  for (b in seq_len(B)) {
    cnt <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)
    dA[b, ] <- as.numeric(cnt %*% CA) / n / h
    dZ[b, ] <- as.numeric(cnt %*% CZ) / n / h
  }
  dd <- dA - dZ
  .ibc_store(paste0("delta_boot::", label_A, " vs ", label_Z),
             boot_A = dA, boot_Z = dZ, boot_delta = dd, point_A = A$ibs$ibs,
             point_Z = Z$ibs$ibs, horizons = h, seed = seed, B = B, n = n)
  q <- function(M, p) apply(M, 2, stats::quantile, p, na.rm = TRUE, names = FALSE)
  data.frame(
    horizon_months = h,
    ibs_A = A$ibs$ibs, ibs_A_lo = q(dA, 0.025), ibs_A_hi = q(dA, 0.975),
    ibs_Z = Z$ibs$ibs, ibs_Z_lo = q(dZ, 0.025), ibs_Z_hi = q(dZ, 0.975),
    delta_ibs = A$ibs$ibs - Z$ibs$ibs,
    delta_lo = q(dd, 0.025), delta_hi = q(dd, 0.975),
    p_delta_lt_0 = colMeans(dd < 0, na.rm = TRUE),
    label_A = label_A, label_Z = label_Z,
    stringsAsFactors = FALSE, row.names = NULL)
}

# =============================================================================
# READMISSION
#
# Readmission has the SAME quadrature and anchor problem as mortality: pred23
# scores it with ibs_ipcw_train() on the five-node grid, anchored at 3, divided by
# max - min. Correcting it is the same operation.
#
# What is genuinely different for readmission, and is NOT the same issue, is that
# death competes: it prevents readmission from ever being observed. That gives two
# legitimate estimands, and the correction has to be applied to both:
#
#   "ipcw"            death treated as censoring (cause-specific / net risk).
#                     This is what the stored readmission IBS reports.
#   "aalen-johansen"  death is a definitive non-event, administrative censoring
#                     only. This is the estimand the thesis reports as the headline
#                     for readmission.
#
# Rather than reimplement the competing-risk weighting, this section reuses the
# contribution builders in ibs_window_bootstrap_holdout.R, which already accept an
# arbitrary grid. Only the windowing is replaced: that file anchors at
# min(eval_times) and divides by (t - t_min), and here we anchor at 0 and divide
# by t, which is the classical definition.
#
# Note that the readmission formula is SHARED by best_perf1 and best_perf2, so
# there is no delta to compute for this outcome. The deliverable is the corrected
# level, with a patient bootstrap around it.
# =============================================================================

.ibc_require_ibsb <- function() {
  if (!exists(".ibsb_contrib_readmit_aj")) {
    pr <- if (exists("project_root", inherits = TRUE)) project_root else getwd()
    source(file.path(pr, "cons/_alt_scripts/ibs_window_bootstrap_holdout.R"))
  }
  stopifnot(exists(".ibsb_contrib_readmit_aj"), exists(".ibsb_contrib_readmit_ipcw"))
}

# Refit the readmission model on every imputation and predict S(t) on a fine grid.
# Death times and events are carried alongside because the competing-risk arm needs
# them to reconstruct the first event.
fit_readmit_grid_predictions <- function(formula, train_list, val_list, grid,
                                         expected_strata = NULL, verbose = TRUE) {
  stopifnot(length(train_list) == length(val_list), length(train_list) >= 1L)
  n_imp <- length(train_list); S_acc <- NULL; meta <- NULL
  for (i in seq_len(n_imp)) {
    fit <- survival::coxph(formula, data = train_list[[i]], ties = "efron",
                           x = TRUE, y = TRUE, model = TRUE)
    n_strata <- { bh <- survival::basehaz(fit, centered = FALSE)
                  if (is.null(bh$strata)) 1L else nlevels(droplevels(factor(bh$strata))) }
    if (!is.null(expected_strata) && n_strata != expected_strata)
      stop(sprintf("Imputation %d: %d baseline strata, expected %d. A strata() term may have been fitted as a covariate.",
                   i, n_strata, expected_strata), call. = FALSE)
    S <- pec::predictSurvProb(fit, newdata = val_list[[i]], times = grid)
    S <- matrix(as.numeric(S), nrow = nrow(val_list[[i]]), ncol = length(grid))
    S_acc <- if (is.null(S_acc)) S else S_acc + S
    if (i == 1L) {
      te <- val_list[[i]]; tr <- train_list[[i]]
      meta <- list(
        rt_val = as.numeric(te$readmit_time_from_disch_m), re_val = .ibc_to01(te$readmit_event),
        dt_val = as.numeric(te$death_time_from_disch_m),   de_val = .ibc_to01(te$death_event),
        rt_tr  = as.numeric(tr$readmit_time_from_disch_m), re_tr  = .ibc_to01(tr$readmit_event),
        dt_tr  = as.numeric(tr$death_time_from_disch_m),   de_tr  = .ibc_to01(tr$death_event))
    }
    if (verbose) cat(".")
  }
  if (verbose) cat(sprintf(" %d imputations fitted, %d baseline strata\n", n_imp, n_strata))
  c(list(S = S_acc / n_imp, grid = grid, n_imp = n_imp), meta)
}

# Classical IBS for readmission under either estimand. Anchored at 0, divided by
# the horizon, on the fine grid. Everything else, including how G is estimated on
# the training fold and the g_min floor, is inherited unchanged from
# ibs_window_bootstrap_holdout.R so only the integral differs.
ibs_classical_readmit <- function(pred, horizons = c(3, 6, 12, 36, 60),
                                  method = c("aalen-johansen", "ipcw"),
                                  g_min = 0.05, eps = 1e-8) {
  method <- match.arg(method)
  .ibc_require_ibsb()
  cc <- if (method == "aalen-johansen") {
    .ibsb_contrib_readmit_aj(pred$S, pred$rt_val, pred$re_val, pred$dt_val, pred$de_val,
                             pred$rt_tr, pred$re_tr, pred$dt_tr, pred$de_tr,
                             pred$grid, g_min, eps)
  } else {
    .ibsb_contrib_readmit_ipcw(pred$S, pred$rt_val, pred$re_val,
                               pred$rt_tr, pred$re_tr, pred$grid, g_min, eps)
  }
  pos <- which(cc$grid > 0)
  gp <- cc$grid[pos]
  C <- cc$C[, pos, drop = FALSE]
  usable <- horizons <= max(gp) + 1e-8
  W <- .ibc_trap_weights(gp, horizons)
  ibs <- as.numeric(colMeans(C) %*% W) / horizons
  ibs[!usable] <- NA_real_
  list(ibs = data.frame(horizon_months = horizons, ibs = ibs, usable = usable),
       contrib_trapezoid = C %*% W, horizons = horizons, method = method,
       grid_max_kept = max(gp),
       brier_curve = data.frame(time_months = gp, brier = colMeans(C)))
}

# Patient bootstrap around a single arm. Same exactness argument as the paired
# version: G comes from the training fold, so per-subject contributions are fixed
# and a replicate is one matrix-vector product.
ibs_classical_boot_ci <- function(pt, B = 1000L, seed = 2125L, verbose = TRUE) {
  C <- pt$contrib_trapezoid; n <- nrow(C); h <- pt$horizons
  if (verbose) cat(sprintf("IBS bootstrap: n = %d, B = %d\n", n, B))
  set.seed(seed)
  M <- matrix(NA_real_, B, length(h))
  for (b in seq_len(B)) {
    cnt <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)
    M[b, ] <- as.numeric(cnt %*% C) / n / h
  }
  .ibc_store(paste0("boot_ci::", pt$method %||% "death"),
             boot = M, point = pt$ibs$ibs, horizons = h, seed = seed, B = B, n = n)
  data.frame(horizon_months = h, ibs = pt$ibs$ibs,
             lo = apply(M, 2, stats::quantile, 0.025, na.rm = TRUE, names = FALSE),
             hi = apply(M, 2, stats::quantile, 0.975, na.rm = TRUE, names = FALSE),
             method = pt$method %||% "n/a", stringsAsFactors = FALSE, row.names = NULL)
}

# Control for readmission: reproduce the notebook's convention on the five nodes,
# using the ORIGINAL windowing (anchor at min(eval_times), divide by t - t_min).
reproduce_pred23_readmit_ibs <- function(pred5, eval_times = c(3, 6, 12, 36, 60),
                                         method = c("aalen-johansen", "ipcw"),
                                         g_min = 0.05, eps = 1e-8) {
  method <- match.arg(method)
  .ibc_require_ibsb()
  cc <- if (method == "aalen-johansen") {
    .ibsb_contrib_readmit_aj(pred5$S, pred5$rt_val, pred5$re_val, pred5$dt_val, pred5$de_val,
                             pred5$rt_tr, pred5$re_tr, pred5$dt_tr, pred5$de_tr,
                             eval_times, g_min, eps)
  } else {
    .ibsb_contrib_readmit_ipcw(pred5$S, pred5$rt_val, pred5$re_val,
                               pred5$rt_tr, pred5$re_tr, eval_times, g_min, eps)
  }
  v <- .ibsb_windows_from_C(cc$C, cc$grid, eval_times, row_weight = rep(1, nrow(cc$C)))
  data.frame(Time = as.character(eval_times), ibs_pred23_convention = as.numeric(v),
             method = method, stringsAsFactors = FALSE, row.names = NULL)
}

# ---- 8. Control: reproduce the notebook's own IBS -----------------------------
# Refits the same way but scores with ibs_ipcw_train() on the 5-node grid and
# averages the per-imputation values, which is how the notebook aggregates. If
# this does not match results_boot_val_bpX, the refit is wrong and nothing else
# in this file should be trusted.
reproduce_pred23_ibs <- function(formula, train_list, val_list,
                                 eval_times = c(3, 6, 12, 36, 60), verbose = TRUE) {
  stopifnot(exists("ibs_ipcw_train"))
  tv <- all.vars(formula[[2]])
  per_imp <- matrix(NA_real_, length(train_list), length(eval_times) + 1L)
  for (i in seq_along(train_list)) {
    fit <- survival::coxph(formula, data = train_list[[i]], ties = "efron",
                           x = TRUE, y = TRUE, model = TRUE)
    S <- pec::predictSurvProb(fit, newdata = val_list[[i]], times = eval_times)
    S <- matrix(as.numeric(S), nrow = nrow(val_list[[i]]), ncol = length(eval_times))
    dtr <- train_list[[i]]; dte <- val_list[[i]]
    per_imp[i, 1] <- ibs_ipcw_train(dtr, dte, S, eval_times, tv[1], tv[2])
    for (k in seq_along(eval_times)) {
      keep <- eval_times <= eval_times[k]
      per_imp[i, k + 1L] <- if (sum(keep) >= 2L)
        ibs_ipcw_train(dtr, dte, S[, keep, drop = FALSE], eval_times[keep], tv[1], tv[2]) else NA_real_
    }
    if (verbose) cat(".")
  }
  if (verbose) cat(" done\n")
  data.frame(Time = c("Global", as.character(eval_times)),
             ibs_pred23_convention = colMeans(per_imp, na.rm = FALSE),
             stringsAsFactors = FALSE, row.names = NULL)
}

# =============================================================================
# EXTENSION 2026-07-27: competing-risk CIF arms, null models, skill scores and
# pairwise arm comparison, all on the classical (Graf) integral.
#
# This section supplies the classical counterparts of three artefacts that the
# notebook currently produces under the pred23 convention:
#   pred23_holdout_readmission_cif_ibs_absolute_bootstrap.csv
#   pred23_holdout_arm_comparison_ibs.csv
#   the "IBS null" and "IBS skill" columns of holdout_c_ibs_combined.csv
#
# THE FIVE ARMS
#   readmit::netrisk    cause-specific Cox, prediction = 1 - S(t)
#   readmit::bp1_cif    CSC(readmit, Full PH death), prediction = CIF cause 1
#   readmit::bp2_cif    CSC(readmit, SHAP rule2 death), prediction = CIF cause 1
#   death::best_perf1   Full PH mortality
#   death::best_perf2   SHAP rule2 mortality
# The three readmission arms are scored on the SAME observed side
# (Aalen-Johansen, death a definitive non-event), so only the prediction differs.
# That is the same-estimand comparison recorded in handoff H23; scoring them on
# different observed sides would make the comparison meaningless.
#
# A NOTE ON "UNO'S C SKILL"
# A skill score is (M_model - M_null) / (M_best - M_null). For the IBS the null
# is ESTIMATED from data (the intercept-only marginal curve), so the skill is
# informative. For the C-index the null is a constant marker: every pair ties, so
# concordance is exactly 0.5 by construction, not estimated. The skill therefore
# collapses to (C - 0.5) / 0.5 = 2C - 1, which is Somers' D: a deterministic
# rescaling of C that carries no information C does not already carry. It is
# provided because it is asked for and is trivially cheap, but it must not be
# presented as the C analogue of the Brier skill score. uno_c_risk_skill() checks
# the constant-marker claim empirically rather than assuming it.
# =============================================================================

# ---- 9. Competing-risk CIF predictions on a fine grid -------------------------
# Rebuilds the joint CSC cache at the requested grid. The notebook's own cache is
# built at EVAL_TIMES only, which is why this cannot reuse it: the classical
# integral needs the whole curve.
fit_cif_grid_predictions <- function(formula_readmit, formula_death,
                                     train_list, val_list, grid, verbose = TRUE) {
  if (!exists(".holdout_build_cif_cache")) {
    pr <- if (exists("project_root", inherits = TRUE)) project_root else getwd()
    source(file.path(pr, "cons/_alt_scripts/holdout_cif_cache.R"))
  }
  stopifnot(exists(".holdout_build_cif_cache"))
  cache <- .holdout_build_cif_cache(formula_readmit, formula_death,
                                    train_list, val_list, grid, verbose = verbose)
  cif <- Reduce(`+`, cache$by_imp) / length(cache$by_imp)   # MI-pooled CIF
  te <- val_list[[1]]; tr <- train_list[[1]]
  list(S = 1 - cif, grid = grid, n_imp = length(cache$by_imp),
       rt_val = as.numeric(te$readmit_time_from_disch_m), re_val = .ibc_to01(te$readmit_event),
       dt_val = as.numeric(te$death_time_from_disch_m),   de_val = .ibc_to01(te$death_event),
       rt_tr  = as.numeric(tr$readmit_time_from_disch_m), re_tr  = .ibc_to01(tr$readmit_event),
       dt_tr  = as.numeric(tr$death_time_from_disch_m),   de_tr  = .ibc_to01(tr$death_event))
}

# ---- 10. Null (intercept-only) reference on the classical integral ------------
# The marginal curve is estimated on TRAIN and broadcast to every validation row,
# then scored with exactly the same weights, grid and integral as the model. This
# mirrors compute_null = TRUE in ibs_window_bootstrap_core(); only the integral
# differs. outcome = "readmit" follows `method` for the estimand, as it must:
# a competing-risk model must be compared against a competing-risk null.
ibs_classical_null <- function(pred, horizons, outcome = c("readmit", "death"),
                               method = c("aalen-johansen", "ipcw"),
                               g_min = 0.05, eps = 1e-8) {
  outcome <- match.arg(outcome); method <- match.arg(method)
  .ibc_require_ibsb()
  grid <- pred$grid
  n <- nrow(pred$S)
  bc <- function(curve) matrix(curve, nrow = n, ncol = length(grid), byrow = TRUE)
  if (outcome == "death") {
    curve <- .ibsb_marg_km(pred$y_train$time, pred$y_train$event, grid)
    null_pred <- pred; null_pred$S <- bc(curve)
    return(ibs_classical_point(null_pred, horizons = horizons, g_min = g_min))
  }
  curve <- if (method == "aalen-johansen") {
    fe <- .ibsb_cr(pred$rt_tr, pred$re_tr, pred$dt_tr, pred$de_tr)
    .ibsb_marg_aj_surv(fe$ftime, fe$fstatus, grid)
  } else {
    .ibsb_marg_km(pred$rt_tr, pred$re_tr, grid)
  }
  null_pred <- pred; null_pred$S <- bc(curve)
  ibs_classical_readmit(null_pred, horizons = horizons, method = method,
                        g_min = g_min, eps = eps)
}

# ---- 11. Paired bootstrap of the Brier skill score ----------------------------
# skill = 1 - IBS_model / IBS_null. Model and null share every resample, which is
# what makes the interval meaningful: the difficulty of the resampled cohort
# moves both terms together and largely cancels in the ratio.
ibs_classical_skill_boot <- function(pt_model, pt_null, B = 1000L, seed = 2125L,
                                     label = "", verbose = TRUE) {
  CM <- pt_model$contrib_trapezoid; CN <- pt_null$contrib_trapezoid
  stopifnot(nrow(CM) == nrow(CN), identical(pt_model$horizons, pt_null$horizons))
  n <- nrow(CM); h <- pt_model$horizons
  if (verbose) cat(sprintf("Skill bootstrap %s: n = %d, B = %d\n", label, n, B))
  set.seed(seed)
  sk <- im <- inull <- matrix(NA_real_, B, length(h))
  for (b in seq_len(B)) {
    cnt <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)
    m <- as.numeric(cnt %*% CM) / n / h
    v <- as.numeric(cnt %*% CN) / n / h
    im[b, ] <- m; inull[b, ] <- v; sk[b, ] <- 1 - m / v
  }
  q <- function(M, p) apply(M, 2, stats::quantile, p, na.rm = TRUE, names = FALSE)
  .ibc_store(paste0("skill::", label),
             boot_model = im, boot_null = inull, boot_skill = sk,
             point_model = pt_model$ibs$ibs, point_null = pt_null$ibs$ibs,
             horizons = h, seed = seed, B = B, n = n)
  data.frame(
    arm = label, horizon_months = h,
    ibs = pt_model$ibs$ibs, ibs_lo = q(im, 0.025), ibs_hi = q(im, 0.975),
    ibs_null = pt_null$ibs$ibs, ibs_null_lo = q(inull, 0.025), ibs_null_hi = q(inull, 0.975),
    ibs_skill = 1 - pt_model$ibs$ibs / pt_null$ibs$ibs,
    skill_lo = q(sk, 0.025), skill_hi = q(sk, 0.975),
    stringsAsFactors = FALSE, row.names = NULL)
}

# ---- 12. Risk-based Uno's C and its (degenerate) skill ------------------------
# risk_mat columns must correspond to `horizons`. The null-C is computed rather
# than assumed, so the claim that it is exactly 0.5 is checked on the real data.
uno_c_risk_skill <- function(time, event, risk_mat, horizons, label = "",
                             B = 0L, seed = 2125L, verbose = TRUE) {
  stopifnot(ncol(risk_mat) == length(horizons))
  ev <- .ibc_to01(event); tt <- as.numeric(time)
  one_c <- function(risk, h, idx = NULL) {
    if (!is.null(idx)) { t <- tt[idx]; e <- ev[idx]; r <- risk[idx] } else { t <- tt; e <- ev; r <- risk }
    ok <- is.finite(t) & is.finite(e) & is.finite(r)
    if (sum(e[ok] == 1L & t[ok] <= h) < 2L) return(NA_real_)
    tryCatch(survival::concordance(survival::Surv(t[ok], e[ok]) ~ r[ok],
               timewt = "n/G2", reverse = TRUE, ymax = h)$concordance,
             error = function(e) NA_real_)
  }
  cs <- vapply(seq_along(horizons), function(j) one_c(risk_mat[, j], horizons[j]), numeric(1))
  # Empirical check of the constant-marker null: a single-valued score ties every
  # pair, so concordance should be exactly 0.5 and no bootstrap of it is needed.
  c_null <- vapply(seq_along(horizons), function(j)
    one_c(rep(0.5, length(tt)), horizons[j]), numeric(1))
  out <- data.frame(arm = label, horizon_months = horizons, uno_c = cs,
                    uno_c_null = c_null, uno_c_skill = 2 * cs - 1,
                    stringsAsFactors = FALSE, row.names = NULL)
  if (B > 0L) {
    n <- length(tt); set.seed(seed)
    M <- matrix(NA_real_, B, length(horizons))
    for (b in seq_len(B)) {
      idx <- sample.int(n, n, replace = TRUE)
      M[b, ] <- vapply(seq_along(horizons), function(j) one_c(risk_mat[, j], horizons[j], idx), numeric(1))
      if (verbose && b %% 100L == 0L) cat(".")
    }
    if (verbose) cat("\n")
    out$uno_c_lo <- apply(M, 2, stats::quantile, 0.025, na.rm = TRUE, names = FALSE)
    out$uno_c_hi <- apply(M, 2, stats::quantile, 0.975, na.rm = TRUE, names = FALSE)
    out$skill_lo <- 2 * out$uno_c_lo - 1
    out$skill_hi <- 2 * out$uno_c_hi - 1
  }
  out
}

# ---- 13. Pairwise arm comparison on the classical integral ---------------------
# Every pair is compared on the SAME resample, so the difference is paired. Only
# arms sharing an observed side may be compared; the function refuses otherwise.
ibs_classical_arm_compare <- function(pts, B = 1000L, seed = 2125L, verbose = TRUE) {
  nm <- names(pts); stopifnot(length(nm) >= 2L, !is.null(nm))
  meth <- vapply(pts, function(p) p$method %||% "death", character(1))
  if (length(unique(meth)) != 1L)
    stop("Arms use different observed sides (", paste(unique(meth), collapse = ", "),
         "); a paired IBS difference between them is not a like-for-like comparison.",
         call. = FALSE)
  h <- pts[[1]]$horizons
  for (p in pts) stopifnot(identical(p$horizons, h), nrow(p$contrib_trapezoid) == nrow(pts[[1]]$contrib_trapezoid))
  n <- nrow(pts[[1]]$contrib_trapezoid)
  pairs <- utils::combn(nm, 2L, simplify = FALSE)
  if (verbose) cat(sprintf("Arm comparison: %d arms, %d pairs, n = %d, B = %d\n",
                           length(nm), length(pairs), n, B))
  set.seed(seed)
  idx_list <- lapply(seq_len(B), function(b) tabulate(sample.int(n, n, replace = TRUE), nbins = n))
  boot_of <- function(a) t(vapply(idx_list, function(cnt)
    as.numeric(cnt %*% pts[[a]]$contrib_trapezoid) / n / h, numeric(length(h))))
  BM <- lapply(nm, boot_of); names(BM) <- nm
  .ibc_store(paste0("arm_compare::", paste(nm, collapse = "|")),
             boot_by_arm = BM, point_by_arm = lapply(pts, function(z) z$ibs$ibs),
             horizons = h, seed = seed, B = B, n = n)
  do.call(rbind, lapply(pairs, function(pr) {
    A <- pr[1]; Z <- pr[2]
    d <- BM[[A]] - BM[[Z]]
    data.frame(
      arm_A = A, arm_B = Z, horizon_months = h,
      ibs_A = pts[[A]]$ibs$ibs, ibs_B = pts[[Z]]$ibs$ibs,
      diff = pts[[A]]$ibs$ibs - pts[[Z]]$ibs$ibs,
      boot_lower = apply(d, 2, stats::quantile, 0.025, na.rm = TRUE, names = FALSE),
      boot_upper = apply(d, 2, stats::quantile, 0.975, na.rm = TRUE, names = FALSE),
      boot_mean = colMeans(d, na.rm = TRUE),
      excludes_zero = apply(d, 2, function(z) {
        qq <- stats::quantile(z, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
        qq[1] > 0 | qq[2] < 0 }),
      stringsAsFactors = FALSE, row.names = NULL)
  }))
}
