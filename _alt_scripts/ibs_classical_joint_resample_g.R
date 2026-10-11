# =============================================================================
# ibs_classical_joint_resample_g.R
#
# SEPARATE, more expensive robustness check for the classical-IBS engines in
# ibs_classical_holdout.R and ibs_window_bootstrap_holdout.R. Those engines fit
# the censoring survival function G ONCE on the training fold and never resample
# it: every bootstrap they run resamples only the test/validation rows, so G
# (like the Cox/CSC coefficients) is treated as part of the FROZEN model under a
# "Target 1: frozen-model performance uncertainty in this evaluated population"
# bootstrap (Collins et al., 2024, part 1). That is a legitimate, correctly
# scoped design, NOT the same omission as external-validation notebooks where G
# was estimated on the very sample being resampled (fixed there this session,
# see ibs_skill_bootstrap_refit_g.R) -- here train is never touched by the
# existing bootstraps, so "refitting G on an unresampled training fold" would be
# a no-op.
#
# This file adds the genuinely different, SEPARATE analysis: jointly resample
# BOTH the training fold (to re-estimate ONLY the censoring KM, G) and the
# test/validation fold (to re-score), while the Cox/CSC coefficients and the
# predicted S(t) curves themselves stay frozen (no model refitting, no
# re-imputation, no re-selection). This propagates sampling uncertainty in the
# training-side censoring-weight ESTIMATE itself, which the existing Target-1
# bootstrap does not and, by construction, cannot capture (its training fold
# never varies). It is intentionally NOT a "Target 2: development-process"
# bootstrap either (Collins et al., 2024, part 1): imputation, feature
# selection, tuning and the model coefficients all stay fixed here. Report it
# labelled as what it is: a training-side censoring-weight robustness check.
#
# Reuses ibsskill_censoring_G_at() / ibsskill_per_subject_contrib() from
# ibs_skill_bootstrap_refit_g.R (already validated this session: exact match
# against the original scalar-loop arithmetic) for the "simple" single-outcome
# case (death; cause-specific/net-risk readmission). Reuses .ibsb_cr() from
# ibs_window_bootstrap_holdout.R (pure first-event reconstruction, not a
# bootstrap function, so it carries none of the G-fixed issue) for the
# competing-risk (Aalen-Johansen observed side) readmission arms.
#
# Public API:
#   ibsjr_bootstrap_core(n_train, n_test, scorer, B, seed, parallel)
#   ibsjr_make_simple_scorer(time_train, event_train, time_test, event_test,
#                            surv_list, grid_pos, weight_list, g_min)
#   ibsjr_contrib_aj(ftime, fstatus, surv_mat, grid_pos, G_at)
#   ibsjr_make_aj_scorer(rt_train, re_train, dt_train, de_train,
#                        rt_test, re_test, dt_test, de_test,
#                        surv_list, grid_pos, weight_list, g_min)
#   ibsjr_summarize(boot_mat, sep)
# =============================================================================
if (!exists("ibsskill_censoring_G_at") || !exists("ibsskill_per_subject_contrib")) {
  pr <- if (exists("project_root", inherits = TRUE)) project_root else getwd()
  source(file.path(pr, "cons/_alt_scripts/ibs_skill_bootstrap_refit_g.R"))
}
if (!exists(".ibsb_cr")) {
  pr <- if (exists("project_root", inherits = TRUE)) project_root else getwd()
  source(file.path(pr, "cons/_alt_scripts/ibs_window_bootstrap_holdout.R"))
}
stopifnot(exists("ibsskill_censoring_G_at"), exists("ibsskill_per_subject_contrib"), exists(".ibsb_cr"))

# ---- 0b. Replicate retention, mirroring ibs_classical_holdout.R's .ibc_store() /
#          ibs_classical_replicates() exactly (same option-gated cache pattern, same
#          "store the rebuild recipe, not the index vectors" convention already used
#          by save-dca-bootstrap-replicates-salvage elsewhere in this notebook). These
#          bootstraps run 12-18+ minutes each; without this, the raw distribution is
#          gone the moment the cell finishes and any later audit needs a full re-run.
#          Set options(ibsjr.keep_replicates = TRUE) BEFORE the first ibsjr_bootstrap_core()
#          call of a session (do this once, at the top of the first *-jointG cell that
#          runs) -- setting it only in a LATER cell silently drops the replicates of
#          every bootstrap that already ran, exactly the bug this session found in
#          ibs_classical_holdout.R's own options(ibsclassical.keep_replicates = TRUE)
#          (set inside holdout-ibs-classical-cif-bootstrap, which runs AFTER
#          holdout-ibs-classical-delta-death and holdout-ibs-classical-readmit).
#          IDEMPOTENT ON PURPOSE, for the identical reason: this file is
#          source()-d from three separate *-jointG cells, and an unconditional
#          `.ibsjr_cache <- new.env(...)` here would silently discard whatever
#          an earlier cell already cached the moment a later cell re-sources it.
if (!exists(".ibsjr_cache", inherits = TRUE) || !is.environment(.ibsjr_cache)) {
  .ibsjr_cache <- new.env(parent = emptyenv())
}
.ibsjr_keep <- function() isTRUE(getOption("ibsjr.keep_replicates", FALSE))
.ibsjr_store <- function(key, ...) {
  if (!.ibsjr_keep()) return(invisible(NULL))
  v <- list(...)
  v$stored_at <- as.character(Sys.time())
  v$rebuild_draws <- paste(
    "set.seed(seed); lapply(seq_len(B), function(i) list(",
    "train = sample.int(n_train, n_train, replace = TRUE),",
    "test  = sample.int(n_test,  n_test,  replace = TRUE)))")
  assign(key, v, envir = .ibsjr_cache)
  invisible(NULL)
}
ibsjr_replicates <- function() as.list(.ibsjr_cache)
ibsjr_replicates_reset <- function() {
  rm(list = ls(.ibsjr_cache), envir = .ibsjr_cache)
  invisible(NULL)
}

# ---- 1. Generic joint (train, test) resampling core --------------------------
# `scorer(bi_train, bi_test)` must return a named numeric vector; every replicate's
# vector is stacked into the returned B x length(names) matrix. bi_train and
# bi_test are drawn INDEPENDENTLY (different populations, no shared subjects).
# `label`, if supplied, caches the raw B x length(names) matrix (plus seed/B/n and a
# rebuild recipe, NOT the index vectors themselves) under that key, retrievable via
# ibsjr_replicates(), whenever getOption("ibsjr.keep_replicates") is TRUE.
ibsjr_bootstrap_core <- function(n_train, n_test, scorer, B = 1000L, seed = 2125L, parallel = FALSE, label = NULL) {
  set.seed(seed)
  draws <- lapply(seq_len(B), function(b) {
    list(train = sample.int(n_train, n_train, replace = TRUE),
         test  = sample.int(n_test,  n_test,  replace = TRUE))
  })
  use_parallel <- isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)
  replicate_list <- if (use_parallel) {
    future.apply::future_lapply(draws, function(z) scorer(z$train, z$test), future.seed = TRUE)
  } else lapply(draws, function(z) scorer(z$train, z$test))
  M <- do.call(rbind, replicate_list)
  if (!is.null(label)) {
    .ibsjr_store(label, boot = M, n_train = n_train, n_test = n_test, seed = seed, B = B)
  }
  M
}

# ---- 2. Scorer for one or more "simple" single-outcome arms ------------------
# All arms in `surv_list` must share the SAME time_train/event_train/time_test/
# event_test (true for prediction23's death arms: best_perf1 and best_perf2 are
# fit on the identical train_list/val_list, so the censoring definition and the
# outcome are identical; only the predicted S(t) differs). G is therefore
# refit ONCE per replicate and shared across every arm in surv_list, matching
# how the existing paired bootstrap already shares resamples across arms.
# surv_list: named list of arm_name -> n_test x length(grid_pos) matrix.
# weight_list: named list of convention_name -> grid_pos x length(horizons)
# trapezoid-weight matrix. CONTRACT, load-bearing: each matrix must be COMPLETE --
# colMeans(C %*% W) must equal the target statistic directly, with NO further
# scaling by the caller. ibsjr_trap_weights_pred23() already satisfies this (it
# divides by (max(nodes) - min(nodes)) internally). ibs_classical_holdout.R's own
# .ibc_trap_weights() does NOT: it returns the raw trapezoid area only, and every
# one of its original callers (ibs_classical_point(), ibs_classical_delta_boot(),
# etc.) divides by `horizons` AFTERWARDS. If you build a weight matrix from
# .ibc_trap_weights() for this function, you MUST pre-divide it yourself first,
# e.g. sweep(.ibc_trap_weights(grid_pos, horizons), 2, horizons, "/") -- passing
# its raw output directly silently inflates every value by a factor equal to its
# own horizon (3x at 3 months, 60x at 60 months). This exact mistake shipped in
# this session's first version of the classical-convention weight matrices in
# prediction23_converted_mod.ipynb's three *-jointG cells (fixed the same day it
# was caught, before being treated as final).
ibsjr_make_simple_scorer <- function(time_train, event_train, time_test, event_test,
                                     surv_list, grid_pos, weight_list, g_min = 0.05) {
  stopifnot(is.list(surv_list), length(surv_list) >= 1L, is.list(weight_list), length(weight_list) >= 1L)
  function(bi_train, bi_test) {
    G_at <- ibsskill_censoring_G_at(time_train[bi_train], event_train[bi_train], g_min)
    t_te <- time_test[bi_test]; e_te <- event_test[bi_test]
    out <- numeric(0)
    for (nm in names(surv_list)) {
      C <- ibsskill_per_subject_contrib(t_te, e_te, surv_list[[nm]][bi_test, , drop = FALSE], grid_pos, G_at)
      for (wn in names(weight_list)) {
        W <- weight_list[[wn]]
        vals <- as.numeric(colMeans(C %*% W))
        names(vals) <- paste(nm, wn, seq_along(vals), sep = "__")
        out <- c(out, vals)
      }
    }
    out
  }
}

# ---- 3. Competing-risk (Aalen-Johansen observed side) per-subject contribution
# Fully matrix-vectorised port of .ibsb_contrib_readmit_aj() (ibs_window_bootstrap_holdout.R),
# parameterised over an EXTERNALLY supplied G_at (already fit on whichever train
# sample is in scope) instead of deriving it internally, so it can be called
# fresh inside every joint-resample replicate. Verified bit-for-bit equivalent
# to the original scalar-loop version (see the smoke test referenced in this
# session's delivery notes). fstatus: 0 = admin censoring, 1 = readmission,
# 2 = competing death (ties -> death-first, matching .ibsb_cr()'s convention).
ibsjr_contrib_aj <- function(ftime, fstatus, surv_mat, grid_pos, G_at, eps = 1e-8) {
  n <- length(ftime); k <- length(grid_pos)
  is_event <- fstatus %in% c(1L, 2L)             # readmit or competing death: "had an event"
  is_readmit <- (fstatus == 1L)
  g_event <- G_at(pmax(ftime - eps, 0))
  g_grid <- G_at(grid_pos)
  ft_le <- outer(ftime, grid_pos, "<=")          # TRUE where ftime <= that node
  ft_gt <- !ft_le                                 # TRUE where still event-free at that node
  event_before <- ft_le & is_event                # readmit OR competing death, at/before the node
  weight <- matrix(0, n, k)
  weight[event_before] <- matrix(1 / g_event, n, k)[event_before]
  weight[ft_gt] <- matrix(1 / g_grid, n, k, byrow = TRUE)[ft_gt]
  readmit_before <- ft_le & matrix(is_readmit, n, k)  # readmission specifically, at/before the node
  outcome <- 1 - matrix(as.numeric(readmit_before), n, k)  # 0 if readmitted by then, 1 otherwise
  weight * (outcome - surv_mat)^2
}

# ---- 4. Scorer for one or more Aalen-Johansen readmission arms ---------------
# All arms in `surv_list` must share the SAME readmit/death train and test
# columns (true for prediction23's three readmission arms: net-risk, CIF bp1,
# CIF bp2 all use the identical train_list/val_list; only the prediction
# differs). The competing-risk train reconstruction, and hence G, is therefore
# computed ONCE per replicate and shared across every arm.
ibsjr_make_aj_scorer <- function(rt_train, re_train, dt_train, de_train,
                                 rt_test, re_test, dt_test, de_test,
                                 surv_list, grid_pos, weight_list, g_min = 0.05) {
  stopifnot(is.list(surv_list), length(surv_list) >= 1L, is.list(weight_list), length(weight_list) >= 1L)
  function(bi_train, bi_test) {
    cr_tr <- .ibsb_cr(rt_train[bi_train], re_train[bi_train], dt_train[bi_train], de_train[bi_train])
    # ibsskill_censoring_G_at(time, event, ...) fits the censoring KM on
    # Surv(time, 1 - event) INTERNALLY, so it must be handed the EVENT
    # indicator (readmit or competing death, fstatus != 0), not the censoring
    # indicator directly -- passing fstatus == 0 here double-flips it and
    # silently fits G on "had an event" instead of "was admin-censored".
    # Caught by the unit test in this session's delivery notes (max abs diff
    # 0.43 against the original .ibsb_contrib_readmit_aj() before this fix,
    # 0 after).
    G_at <- ibsskill_censoring_G_at(cr_tr$ftime, as.integer(cr_tr$fstatus != 0L), g_min)
    cr_te <- .ibsb_cr(rt_test[bi_test], re_test[bi_test], dt_test[bi_test], de_test[bi_test])
    out <- numeric(0)
    for (nm in names(surv_list)) {
      C <- ibsjr_contrib_aj(cr_te$ftime, cr_te$fstatus, surv_list[[nm]][bi_test, , drop = FALSE], grid_pos, G_at)
      for (wn in names(weight_list)) {
        W <- weight_list[[wn]]
        vals <- as.numeric(colMeans(C %*% W))
        names(vals) <- paste(nm, wn, seq_along(vals), sep = "__")
        out <- c(out, vals)
      }
    }
    out
  }
}

# ---- 4b. Trapezoid weights, PRED23 CONVENTION: only the reporting nodes (typically
#          EVAL_TIMES), integral anchored at min(nodes) rather than 0, normalised by
#          max - min rather than the horizon. Mirrors .w_pred23() from this session's
#          pred26/pred27 "both conventions" chunks; ported here so the joint-resample
#          engine can report the SAME pred23-convention estimand this notebook headlines
#          (e.g. pred23_holdout_readmission_cif_ibs_absolute_bootstrap.csv), not only the
#          classical one, without a second, separate contribution-matrix computation:
#          add this weight matrix alongside a classical one in the SAME weight_list and
#          both conventions come out of one bootstrap pass over the same replicates.
#          `nodes` must all be present in `grid_pos` (true for classical_grid /
#          readmit_grid / ibs_grid here, since ibs_classical_grid() always forces its
#          `horizons` argument into the returned grid).
ibsjr_trap_weights_pred23 <- function(grid_pos, horizons, nodes) {
  W <- matrix(0, length(grid_pos), length(horizons))
  for (k in seq_along(horizons)) {
    nk <- nodes[nodes <= horizons[k] + 1e-12]
    if (length(nk) < 2L) next               # a single node cannot be integrated -> column stays 0 (NA downstream)
    idx <- match(nk, grid_pos)
    stopifnot(!anyNA(idx))
    d <- diff(nk)
    w <- c(d, 0) / 2 + c(0, d) / 2
    W[idx, k] <- w / (max(nk) - min(nk))
  }
  W
}

# ---- 5. Reshape a boot_mat (columns "<arm>__<convention>__<horizon index>")
#         into a tidy arm x convention x horizon data.frame with percentile CIs.
# `horizons` must be in the SAME order used to build each weight matrix's columns.
ibsjr_summarize <- function(boot_mat, horizons, sep = "__") {
  parts <- strsplit(colnames(boot_mat), sep, fixed = TRUE)
  arm <- vapply(parts, `[`, character(1), 1L)
  convention <- vapply(parts, `[`, character(1), 2L)
  h_idx <- as.integer(vapply(parts, `[`, character(1), 3L))
  data.frame(
    arm = arm, convention = convention, horizon_months = horizons[h_idx],
    mean = colMeans(boot_mat, na.rm = TRUE),
    lower_95 = apply(boot_mat, 2, stats::quantile, 0.025, na.rm = TRUE, names = FALSE),
    upper_95 = apply(boot_mat, 2, stats::quantile, 0.975, na.rm = TRUE, names = FALSE),
    stringsAsFactors = FALSE, row.names = NULL)
}
