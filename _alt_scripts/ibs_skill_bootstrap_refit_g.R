# =============================================================================
# ibs_skill_bootstrap_refit_g.R
#
# Fixes a specific, narrow problem in the "both conventions" IBS/Brier-skill
# side-analyses of prediction26_ext_val_readm_mod.ipynb
# (cell 'ibs-skill-bootstrap-both-conventions') and
# prediction27_ext_val_death_mod.ipynb (cell 'extval-ibs-skill-both-conventions').
#
# THE PROBLEM
# -----------
# Both cells precompute per-subject IPCW Brier contributions ONCE, using a
# censoring survival function G fit on the full (non-resampled) cohort, then
# bootstrap by reweighting those FIXED contributions with resample
# multiplicities (tabulate() + matrix product). This is documented explicitly
# in both cells' own comments ("G comes from a fixed cohort and does not
# change under resampling") as a deliberate speed optimisation. It means the
# reported bootstrap interval reflects only sampling variability in WHICH
# subjects are drawn, not in the estimated censoring distribution itself. That
# understates uncertainty, and the understatement is worst exactly where
# follow-up is thin, e.g. convenio 3 at 60 months.
#
# This is NOT a problem in the notebooks' PRIMARY IBS bootstraps
# (prediction26's bootstrap_calibration_and_ibs(), prediction27's
# bootstrap_extval_death() and evd_bootstrap_death()): all three already call
# survival::survfit() / ibs_ipcw_train() fresh on the resampled (time, event)
# inside every replicate. Only the two convention-comparison "skill" cells use
# the fixed-G shortcut, and only those two are patched here.
#
# THE FIX
# -------
# Refit G inside every bootstrap replicate, on that replicate's own resampled
# (time, event), exactly as the primary bootstraps already do. The point
# estimates (fit once on the full, non-resampled sample) are UNCHANGED by
# this fix; only the width of the reported interval can change, because
# resampling now also propagates uncertainty in the censoring distribution.
# For a rare outcome with a long follow-up tail (60 months, few still at
# risk), a resample can occasionally omit the latest observed times entirely,
# which shifts where the censoring KM's floor (g_min) kicks in; this is a
# real, not spurious, source of variability that the fixed-G shortcut hid.
#
# Public API:
#   ibsskill_censoring_G_at(time, event, g_min)      -> closure G_at(query)
#   ibsskill_per_subject_contrib(time, event, surv_mat, grid_pos, G_at)
#   ibsskill_bootstrap_refit(time, event, surv_model, surv_null, grid_pos,
#                            weight_list, B, seed, g_min, parallel)
# =============================================================================
suppressWarnings({
  stopifnot(requireNamespace("survival", quietly = TRUE))
})
# Fit the censoring survival function G(t) = P(C > t) on the GIVEN (time,
# event) -- the caller decides whether that is the full cohort (point
# estimate) or one bootstrap resample (replicate). Order-safe lookup: G is
# evaluated once at the sorted unique query times and mapped back with
# match(), matching the 2026-07-21 fix already applied to ibs_ipcw_train().
ibsskill_censoring_G_at <- function(time, event, g_min = 0.05) {
  fit <- survival::survfit(survival::Surv(time, 1L - event) ~ 1)
  function(query) {
    requested <- pmax(as.numeric(query), 0)
    unique_times <- sort(unique(requested))
    g <- as.numeric(summary(fit, times = unique_times, extend = TRUE)$surv)
    stopifnot(length(g) == length(unique_times))
    pmax(g[match(requested, unique_times)], g_min)
  }
}
# Per-subject IPCW squared-error contribution at every grid node, for a given
# censoring function G_at (already fit on whichever sample is in scope).
# Identical arithmetic to the .contrib()/contrib() local functions already in
# both notebook cells; extracted here so it can be called fresh inside every
# bootstrap replicate instead of once outside the loop.
#
# Performance note: this is fully matrix-vectorised, with NO per-grid-node R
# loop and NO ifelse() call. An earlier version looped over grid nodes
# (calling ifelse() once per node) and was too slow to be usable at
# B = 1000 on the ~26,000-row readmission cohort with a ~150-point fine grid
# (confirmed by timing during development: it did not finish a 100-replicate
# test in several minutes). G at every grid node is looked up in ONE batched
# call (G_at(grid_pos)); the per-subject event-time lookup (g_event) is
# likewise a single batched call; and the event/after-horizon weight
# assignment is done as two whole-matrix logical-index writes instead of a
# per-node ifelse(), which is what actually removed the bottleneck (matrix
# recycling instead of an R-level loop over ~150 grid nodes).
ibsskill_per_subject_contrib <- function(time, event, surv_mat, grid_pos, G_at, eps = 1e-8) {
  n <- length(time); k <- length(grid_pos)
  g_event <- G_at(pmax(time - eps, 0))          # length n: G just before each subject's own time
  g_grid <- G_at(grid_pos)                       # length k: G at each grid node
  is_event <- (event == 1L)
  time_le <- outer(time, grid_pos, "<=")         # n x k: TRUE where the subject's time is <= that node
  time_gt <- !time_le                            # n x k: TRUE where the subject is still event-free at that node
  event_before <- time_le & is_event             # n x k: TRUE where the subject had the event at/before that node
  weight <- matrix(0, n, k)
  weight[event_before] <- matrix(1 / g_event, n, k)[event_before]
  weight[time_gt] <- matrix(1 / g_grid, n, k, byrow = TRUE)[time_gt]
  outcome <- matrix(as.numeric(time_gt), n, k)   # 1 if still event-free at the node, 0 otherwise
  weight * (outcome - surv_mat)^2
}
# Bootstrap with G refit inside every replicate. surv_model / surv_null are
# the (already computed, NOT refit) prediction matrices for the model of
# interest and the intercept-only null, both n x length(grid_pos), indexed by
# the same resample as time/event -- only the CENSORING model is refit per
# replicate, since that is the piece the fixed-G shortcut held constant.
# weight_list is a named list of grid_pos x length(horizons) trapezoid-weight
# matrices (one per integration convention, e.g. list(classical = W_cl,
# pred23 = W_p23)); weights depend only on the (fixed) grid, not on the data,
# so they are computed once outside this function and passed in unchanged.
# Returns a named list of B x length(horizons) matrices, one per
# (convention, arm) combination, named "<convention>_model" / "<convention>_null" --
# a drop-in replacement for the notebooks' existing CW / bt / boot_mat objects.
#
# indices: optional, a pre-built list of length-n resample-index vectors (as
# produced by make_bootstrap_indices() in prediction26, or the notebook's own
# shared `bootstrap_indices`). When supplied, B/seed are ignored and these
# exact resamples are used, so this cell's bootstrap uses the SAME draws as
# every other bootstrap in the notebook (paired, not just similarly-sized).
# When NULL, B fresh resamples are drawn here with `seed` (matching
# prediction27's own self-contained IBS_SEED/IBS_B convention).
ibsskill_bootstrap_refit <- function(time, event, surv_model, surv_null, grid_pos, weight_list,
                                     B = 1000L, seed = 2125L, g_min = 0.05, parallel = FALSE,
                                     indices = NULL) {
  n <- length(time)
  stopifnot(length(event) == n, nrow(surv_model) == n, nrow(surv_null) == n,
           is.list(weight_list), length(weight_list) >= 1L)
  weight_names <- names(weight_list)
  stopifnot(!is.null(weight_names), all(nzchar(weight_names)))
  col_labels <- unlist(lapply(weight_names, function(wn) {
    W <- weight_list[[wn]]
    c(paste0(wn, "_model_", seq_len(ncol(W))), paste0(wn, "_null_", seq_len(ncol(W))))
  }))
  one_rep <- function(bi) {
    t <- time[bi]; e <- event[bi]
    Sm <- surv_model[bi, , drop = FALSE]; Sn <- surv_null[bi, , drop = FALSE]
    # The one line that fixes the problem: G is fit fresh on THIS replicate's
    # resampled (t, e), not reused from the full-sample fit.
    G_at <- ibsskill_censoring_G_at(t, e, g_min)
    Cm <- ibsskill_per_subject_contrib(t, e, Sm, grid_pos, G_at)
    Cn <- ibsskill_per_subject_contrib(t, e, Sn, grid_pos, G_at)
    values <- unlist(lapply(weight_names, function(wn) {
      W <- weight_list[[wn]]
      c(as.numeric(colMeans(Cm %*% W)), as.numeric(colMeans(Cn %*% W)))
    }))
    stats::setNames(values, col_labels)
  }
  if (!is.null(indices)) {
    stopifnot(is.list(indices), length(indices) >= 1L, all(lengths(indices) == n))
    idx <- indices
  } else {
    set.seed(seed)
    idx <- lapply(seq_len(B), function(b) sample.int(n, n, replace = TRUE))
  }
  use_parallel <- isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)
  replicate_list <- if (use_parallel) {
    future.apply::future_lapply(idx, one_rep, future.seed = TRUE)
  } else lapply(idx, one_rep)
  M <- do.call(rbind, replicate_list)
  result <- list()
  for (wn in weight_names) {
    W <- weight_list[[wn]]
    result[[paste0(wn, "_model")]] <- M[, paste0(wn, "_model_", seq_len(ncol(W))), drop = FALSE]
    result[[paste0(wn, "_null")]] <- M[, paste0(wn, "_null_", seq_len(ncol(W))), drop = FALSE]
  }
  result
}
