# =============================================================================
# annex_calfig_chunks.R  (2026-08-21, rev 8)
#
# SELF-CONTAINED notebook chunks for the ANNEX calibration figure of
# cons/prediction23_converted_mod.ipynb. TWO rows of four horizons
# (6/12/36/60 months): row A first readmission (joint CIF), row B BOTH
# all-cause mortality models overlaid and separated by marker shape and line
# type. Binned calibration with pointwise 95% CIs; marginal density strips
# drawn as theme_void() patchwork panels outside the plot with their baseline
# flush at zero; grayscale; Epidemiology (LWW) style. Column headers appear on
# the TOP ROW ONLY, while x tick marks and numbers appear on EVERY panel so
# each panel can be read on its own.
#
# CHUNK 1b ('annex-calfig-ensure') makes the annex self-contained: it resolves
# and VALIDATES the project root, derives figs_out, and guarantees the three
# frozen prediction objects exist, reusing them if live, recovering them from
# the saved workspace (about 35 s), or refitting from source (about 1.6 min).
#
# SWITCHES
#   ANNEX_CI_METHOD          "analytic" (default) or "bootstrap"
#   ANNEX_PER_HORIZON_LIMITS FALSE (default) = one risk range per row, so the
#                            horizons stay comparable. TRUE = each horizon gets
#                            its own range driven by its calibration data,
#                            which fills every panel and is the only way the
#                            confidence bands and the miscalibration become
#                            visible at 6 and 12 months for mortality.
#   ANNEX_MORT_DODGE_FRAC    0 (default). A horizontal dodge of the two
#                            mortality series is implemented but DISABLED: on
#                            a calibration plot the reference is y = x, so
#                            offsetting x biases the apparent miscalibration
#                            of the two models in opposite directions.
#   ANNEX_USE_COLOR          FALSE (default) = grayscale-safe.
#
# ANNEX_FIG_NOTE is built by chunk 5 from what the drawing actually did (CI
# method, scale policy, any dodge, any display reduction) and printed for
# pasting into the manuscript caption, so the note cannot drift from the art.
#
# Kernel prerequisites: NONE beyond an R session whose working directory sits
# somewhere inside the project. Requires patchwork, ggplot2, dplyr, survival,
# scales, grid, ragg; future.apply only for the parallel bootstrap branch.
#
# VERIFIED 2026-08-20 against the REAL frozen holdout objects (5 imputations,
# 17,631 validation rows, 8 quantile groups per model per horizon):
#   - root resolution from <root>, <root>/cons and <root>/cons/_alt_scripts,
#     and it CORRECTS a project_root pointing at the cons subfolder
#   - tier 2 restore 34 s; tier 3 refit 1.6 min, reproducing the frozen
#     predicted risks EXACTLY (max abs diff 0.000e+00, correlation 1)
#   - tier 3 all-or-nothing; provenance preserved across re-runs
#   - bootstrap parallel branch bit-identical to sequential at the same seed;
#     median width relative to analytic 0.944 / 0.978 / 0.986 at B = 200 /
#     500 / 1000
#   - all four combinations of the two main switches render
# =============================================================================

# ---------------------------------------------------------------------------
# CHUNK 1 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-packages
# ============================================================================
# ANNEX CALIBRATION FIGURE, STEP 1/6: dependency gate.
# Verifies that every package used by the annex chunks is installed BEFORE any
# computation, so a missing package fails fast here instead of halfway through
# the figure build. No package is attached to the search path: every downstream
# call uses explicit package::function syntax. The marginal density strips are
# composed with patchwork rather than ggside, so ggside is no longer a
# dependency; future.apply is needed only for the parallel bootstrap branch
# and is checked there, not here.
# Requires in kernel: nothing.
# ============================================================================
.annex_required_pkgs <- c("ggplot2", "patchwork", "dplyr", "survival", "scales", "grid", "ragg")
.annex_pkg_present <- vapply(.annex_required_pkgs, requireNamespace, logical(1L), quietly = TRUE)
if (any(!.annex_pkg_present)) {
  stop(
    "Missing packages for the annex calibration figure: ",
    paste(.annex_required_pkgs[!.annex_pkg_present], collapse = ", "),
    ". Install them inside this renv project with renv::install(c('",
    paste(.annex_required_pkgs[!.annex_pkg_present], collapse = "', '"), "')).",
    call. = FALSE
  )
}
for (.annex_p in .annex_required_pkgs) {
  message(sprintf("annex-calfig dependency: %-10s %s", .annex_p, as.character(utils::packageVersion(.annex_p))))
}

# ---------------------------------------------------------------------------
# CHUNK 1b (self-containment cascade) -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-ensure
# ============================================================================
# ANNEX CALIBRATION FIGURE, STEP 1b/6: make the annex SELF-CONTAINED.
# Guarantees that results_boot_val_bp1, results_boot_val_bp2 and
# results_boot_val_bp2_cif exist, using a three-tier cascade and recording
# which tier supplied them, because the tiers are NOT equivalent evidence:
#   TIER 1  objects already in the kernel            -> used untouched
#   TIER 2  recover from the saved workspace dump    -> about 40 seconds, and
#           it is the SAME frozen fit the manuscript reports, so this is the
#           preferred recovery and the default
#   TIER 3  full rebuild from source                 -> refits every Cox model
#           on the 5 imputations. VERIFIED 2026-08-20: the rebuild reproduces
#           the frozen predicted risks EXACTLY (maximum absolute difference
#           0.000e+00 across all three objects, correlation 1) in about 1.7
#           minutes, because the fits are deterministic given the fixed
#           train/val split. Tiers 2 and 3 are therefore interchangeable in
#           practice; tier 2 is still preferred because it is the object the
#           manuscript was actually written from.
# Tier 3 is ALL-OR-NOTHING on purpose: it discards any partially present core
# objects first, so a single figure can never mix frozen predictions with
# freshly refit ones while reporting one provenance.
# Set ANNEX_FORCE_REBUILD <- TRUE to skip tiers 1 and 2 and always refit.
#
# NAMING WARNING: the `_boot` inside results_boot_val_* is a LEGACY misnomer.
# The cell `holdout-cindex-ibs-run` calls evaluate_dual_cox_holdout_dualscore(),
# which loops over the 5 imputations fitting on train and predicting on val
# with NO resampling whatsoever. That file's own header says the bootstrapping
# elsewhere "bootstrap PATIENTS (not model refits) ... predictions frozen,
# only resampling varies". Resampling lives in the dedicated bootstrap cells
# (`holdout-cindex-bootstrap`, `holdout-ici-bootstrap-run`,
# `holdout-threshold-bootstrap-run`), never in the cell that builds these
# objects.
#
# TIER 2 FORMAT NOTE (verified 2026-08-20): despite the .rds extension,
# data/20241015_out/pred23_holdout_validation_*.rds is an RData file written
# by save(), so readRDS() fails on it and load() is required. The 2026-07-30
# file is a full 613-object workspace dump; it is loaded into a PRIVATE
# environment and only the needed objects are copied out, because loading it
# into the global environment would overwrite 613 names, including
# project_root, figs_out, engine functions and .Random.seed.
# Requires in kernel: project_root (cell `define-project-root-and-paths`).
# Place AFTER `annex-calfig-packages` and BEFORE `annex-calfig-data`.
# ============================================================================
if (!exists("ANNEX_FORCE_REBUILD", inherits = TRUE)) ANNEX_FORCE_REBUILD <- FALSE
# ---- Resolve AND VALIDATE the project root ---------------------------------
# Taking `project_root` on faith, or falling back to a bare getwd(), is not
# enough. Running from the console or a temporary script commonly leaves the
# working directory at <root>/cons, which silently produced paths like
# <root>/cons/cons/_alt_scripts/... and <root>/cons/data/20241015_out: tier 2
# then found no workspace and tier 3 died on "engine script not found".
# This mirrors the cascade run_validation_holdout.R already uses: try several
# candidates, strip a trailing /cons, and accept only a candidate that
# actually contains cons/_alt_scripts, so a WRONG but existing project_root is
# corrected rather than trusted.
.annex_root <- local({
  cand <- character(0)
  if (exists("project_root", inherits = TRUE) && is.character(project_root) && length(project_root) == 1L && !is.na(project_root)) {
    cand <- c(cand, project_root, sub("(/|\\\\)?cons(/|\\\\)?$", "", project_root))
  }
  pr_here <- tryCatch(here::here(), error = function(e) NA_character_)
  if (length(pr_here) == 1L && !is.na(pr_here)) {
    cand <- c(cand, pr_here, sub("(/|\\\\)?cons(/|\\\\)?$", "", pr_here))
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  cand <- c(cand, wd, sub("(/|\\\\)?cons(/|\\\\)?$", "", wd))
  cand <- unique(cand[nzchar(cand)])
  ok <- cand[vapply(cand, function(p) dir.exists(file.path(p, "cons", "_alt_scripts")), logical(1L))]
  if (!length(ok)) {
    stop(sprintf("annex-calfig: cannot locate the project root. Tried: %s. None of these contains cons/_alt_scripts. Run the cell `define-project-root-and-paths`, or set project_root manually before this cell.",
                 paste(cand, collapse = " | ")), call. = FALSE)
  }
  normalizePath(ok[1L], winslash = "/", mustWork = FALSE)
})
message("annex-calfig ensure: project root resolved to ", .annex_root)
# Keep the kernel's project_root consistent with what this cell actually used,
# so the rest of the annex and the notebook cannot disagree about the root.
if (!exists("project_root", inherits = TRUE) ||
    !identical(normalizePath(project_root, winslash = "/", mustWork = FALSE), .annex_root)) {
  message("annex-calfig ensure: setting project_root to the resolved root (it was absent or pointed elsewhere).")
  project_root <- .annex_root
}
# figs_out is derived from the SAME resolved root, reproducing the definition
# in `define-project-root-and-paths` rather than inventing a location, so the
# export cell has a sanctioned destination even on a cold kernel.
if (!exists("figs_out", inherits = TRUE) || !is.character(figs_out) || length(figs_out) != 1L) {
  figs_out <- file.path(.annex_root, "cons", "_figs")
  message("annex-calfig ensure: figs_out was absent; set to ", figs_out)
}
.annex_core3 <- c("results_boot_val_bp1", "results_boot_val_bp2", "results_boot_val_bp2_cif")
# ---- Preconditions checked BEFORE anything expensive ------------------------
# EVAL_TIMES is normalised IN PLACE, not merely inspected: the refits below
# receive this object, so validating a sorted numeric copy while passing the
# raw one would let a reversed or character-typed grid through. The grid must
# be the canonical c(3, 6, 12, 36, 60), since IBS is integrated over whatever
# grid it is given and objects built on a different grid are not
# interchangeable with the frozen ones (the documented eval_times trap). This
# sits outside the tier-3 block so a bad grid fails in one second rather than
# after several minutes of refitting, and so tiers 1 and 2 are checked too.
if (!exists("EVAL_TIMES", inherits = TRUE)) EVAL_TIMES <- c(3, 6, 12, 36, 60)
EVAL_TIMES <- sort(as.numeric(EVAL_TIMES))
stopifnot("EVAL_TIMES must be the canonical c(3, 6, 12, 36, 60)" =
  identical(EVAL_TIMES, c(3, 6, 12, 36, 60)))
.annex_tier <- NA_character_
# ---- TIER 1: already in the kernel -----------------------------------------
if (!ANNEX_FORCE_REBUILD && all(vapply(.annex_core3, exists, logical(1L), inherits = TRUE))) {
  .annex_tier <- "1 (already in kernel)"
}
# ---- TIER 2: recover from the saved workspace dump --------------------------
if (is.na(.annex_tier) && !ANNEX_FORCE_REBUILD) {
  .annex_ws_all <- list.files(file.path(.annex_root, "data", "20241015_out"),
                              pattern = "^pred23_holdout_validation_.*\\.rds$", full.names = TRUE)
  if (length(.annex_ws_all) > 0L) {
    .annex_ws <- .annex_ws_all[order(file.info(.annex_ws_all)$mtime, decreasing = TRUE)][1L]
    message("annex-calfig ensure: TIER 2, reading ", basename(.annex_ws), " (",
            round(file.info(.annex_ws)$size / 1024^2), " MB).")
    .annex_ok <- tryCatch({
      .annex_renv <- new.env()
      .annex_ws_names <- load(.annex_ws, envir = .annex_renv)
      .annex_got <- vapply(.annex_core3, exists, logical(1L), envir = .annex_renv, inherits = FALSE)
      if (all(.annex_got)) {
        # Assign only after ALL THREE are confirmed present, so a workspace
        # missing one object cannot leave a half-populated global state that
        # tier 3 would then treat as "partially done".
        for (.annex_n in c(.annex_core3, "tnr")) {
          if (exists(.annex_n, envir = .annex_renv, inherits = FALSE)) {
            assign(.annex_n, get(.annex_n, envir = .annex_renv), envir = globalenv())
          }
        }
        message("annex-calfig ensure: recovered ", paste(.annex_core3, collapse = ", "),
                " from a workspace holding ", length(.annex_ws_names), " objects.")
        TRUE
      } else {
        message("annex-calfig ensure: workspace lacks ", paste(.annex_core3[!.annex_got], collapse = ", "),
                "; falling through to TIER 3.")
        FALSE
      }
    }, error = function(e) {
      message("annex-calfig ensure: TIER 2 failed (", conditionMessage(e), "); falling through to TIER 3.")
      FALSE
    })
    if (exists(".annex_renv", inherits = FALSE)) rm(.annex_renv)
    invisible(gc())
    if (isTRUE(.annex_ok)) .annex_tier <- "2 (restored from frozen workspace)"
  }
}
# ---- TIER 3: full rebuild from source ---------------------------------------
if (is.na(.annex_tier)) {
  message("annex-calfig ensure: TIER 3, rebuilding from source. This refits every Cox model on the 5 imputations and takes a few minutes.")
  # ALL-OR-NOTHING. Any core object surviving from an earlier tier is dropped
  # so that every one of the three is refit in this same pass. Without this,
  # a kernel holding only bp1 would keep that frozen object, refit bp2, and
  # stamp both "refit from source", producing a figure whose rows do not
  # share a provenance.
  .annex_stale <- intersect(.annex_core3, ls(envir = globalenv()))
  if (length(.annex_stale) > 0L) {
    message("annex-calfig ensure: discarding partially present ", paste(.annex_stale, collapse = ", "),
            " so the rebuild is all-or-nothing.")
    rm(list = .annex_stale, envir = globalenv())
    invisible(gc())
  }
  # `survival` MUST be attached, which is the one place this annex cannot use
  # package::function syntax: the frozen formulas contain bare Surv(...) and
  # strata(...), and .holdout_build_cif_cache() rebuilds cause-specific
  # formulas from RHS text for riskRegression::CSC(), whose model.frame()
  # resolves those symbols through the SEARCH PATH. Without it tier 3 dies
  # with `could not find function "strata"` at the joint CIF step, after
  # already spending minutes on both dual-Cox refits. prodlim and
  # riskRegression are attached for parity with the notebook's `setup` cell
  # rather than for search-path resolution: the engines qualify those two
  # with :: throughout.
  for (.annex_att in c("survival", "prodlim", "riskRegression")) {
    if (!.annex_att %in% .packages()) {
      suppressPackageStartupMessages(library(.annex_att, character.only = TRUE))
      message("annex-calfig ensure: attached ", .annex_att)
    }
  }
  # NOTE: run_validation_holdout.R calls setwd(project_root) by design, to
  # anchor the relative source() calls its audited engines make internally.
  # The working directory therefore changes here and is deliberately NOT
  # restored, since restoring it would break those relative paths.
  .annex_wd_before <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (.annex_eng in c("cons/_alt_scripts/run_validation_holdout.R",
                       "cons/_alt_scripts/evaluate_dual_cox_holdout_dualscore.R",
                       "cons/_alt_scripts/holdout_cif_cache.R")) {
    .annex_p <- file.path(.annex_root, .annex_eng)
    stopifnot("engine script not found under the resolved project root" = file.exists(.annex_p))
    source(.annex_p)
    message("annex-calfig ensure: sourced ", basename(.annex_p))
  }
  if (!identical(.annex_wd_before, normalizePath(getwd(), winslash = "/", mustWork = FALSE))) {
    message("annex-calfig ensure: working directory changed to ", getwd(),
            " by run_validation_holdout.R (expected; its engines rely on it).")
  }
  if (!all(vapply(c("train_list", "val_list"), exists, logical(1L), inherits = TRUE))) {
    stopifnot("build_holdout_datasets() not available after sourcing the engines" = exists("build_holdout_datasets"))
    .annex_hd <- build_holdout_datasets(force = FALSE, verify = TRUE, verbose = TRUE)
    train_list <- .annex_hd$train
    val_list <- .annex_hd$val
    message("annex-calfig ensure: rebuilt train_list/val_list (", length(train_list), " imputations).")
  }
  stopifnot("train_list and val_list must have the same number of imputations" =
    length(train_list) == length(val_list))
  # Model formulas are NEVER hardcoded here: a stale copy of a 60-term formula
  # that silently drifts from the canonical one in `holdout-define-models` is
  # exactly the error this project cannot afford. Each name is recovered
  # INDEPENDENTLY, so a live `models` is never clobbered just because
  # `f_readmit` happened to be missing, and any name that WAS live is checked
  # against the frozen copy so a stale kernel formula is reported rather than
  # silently assumed equivalent.
  if (!exists("models", inherits = TRUE) || !exists("f_readmit", inherits = TRUE)) {
    .annex_ws_all2 <- list.files(file.path(.annex_root, "data", "20241015_out"),
                                 pattern = "^pred23_holdout_validation_.*\\.rds$", full.names = TRUE)
    stopifnot("cannot recover the model formulas: run the cell `holdout-define-models` first" = length(.annex_ws_all2) > 0L)
    .annex_ws2 <- .annex_ws_all2[order(file.info(.annex_ws_all2)$mtime, decreasing = TRUE)][1L]
    .annex_fenv <- new.env()
    load(.annex_ws2, envir = .annex_fenv)
    stopifnot("the workspace does not carry `models` and `f_readmit`" =
      all(vapply(c("models", "f_readmit"), exists, logical(1L), envir = .annex_fenv, inherits = FALSE)))
    .annex_dep <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")
    for (.annex_fn in c("models", "f_readmit")) {
      if (!exists(.annex_fn, inherits = TRUE)) {
        assign(.annex_fn, get(.annex_fn, envir = .annex_fenv), envir = globalenv())
        message("annex-calfig ensure: recovered ", .annex_fn, " from the frozen workspace.")
      } else if (identical(.annex_fn, "f_readmit")) {
        stopifnot("the live f_readmit differs from the frozen one; refusing to mix a stale formula with frozen predictions" =
          identical(.annex_dep(f_readmit), .annex_dep(get("f_readmit", envir = .annex_fenv))))
        message("annex-calfig ensure: live f_readmit matches the frozen copy.")
      } else {
        .annex_fz <- get("models", envir = .annex_fenv)
        stopifnot("the live `models` formulas differ from the frozen ones; refusing to mix stale formulas with frozen predictions" =
          identical(.annex_dep(models$best_perf1$death), .annex_dep(.annex_fz$best_perf1$death)) &&
          identical(.annex_dep(models$best_perf2$death), .annex_dep(.annex_fz$best_perf2$death)))
        message("annex-calfig ensure: live `models` formulas match the frozen copies.")
      }
    }
    rm(.annex_fenv)
    invisible(gc())
  }
  results_boot_val_bp1 <- evaluate_dual_cox_holdout_dualscore(
    models$best_perf1$readmit, models$best_perf1$death,
    train_list, val_list, eval_times = EVAL_TIMES)
  message("annex-calfig ensure: rebuilt results_boot_val_bp1.")
  results_boot_val_bp2 <- evaluate_dual_cox_holdout_dualscore(
    models$best_perf2$readmit, models$best_perf2$death,
    train_list, val_list, eval_times = EVAL_TIMES)
  message("annex-calfig ensure: rebuilt results_boot_val_bp2.")
  # Joint CIF arm: cause-specific Cox for readmission paired with the SHAP
  # mortality model, then injected into a copy of bp2, exactly as the cell
  # `holdout-calibration-run` does it.
  .annex_cifc <- .holdout_build_cif_cache(f_readmit, models$best_perf2$death,
                                          train_list, val_list, EVAL_TIMES)
  results_boot_val_bp2_cif <- .holdout_inject_readmission_cif(results_boot_val_bp2, .annex_cifc)
  message("annex-calfig ensure: rebuilt results_boot_val_bp2_cif.")
  .annex_tier <- "3 (refit from source)"
}
# ---- Structural verification, whichever tier produced the objects -----------
stopifnot("the cascade did not produce all three objects" =
  all(vapply(.annex_core3, exists, logical(1L), inherits = TRUE)))
.annex_nrows <- integer(0)
for (.annex_n in .annex_core3) {
  .annex_o <- get(.annex_n, inherits = TRUE)
  .annex_rp <- .annex_o$raw_predictions
  stopifnot("object has no usable raw_predictions" = is.list(.annex_rp), length(.annex_rp) >= 1L)
  .annex_it <- .annex_rp[[1L]]
  stopifnot("raw_predictions item lacks readmission/death surv_val_matrix" =
    !is.null(.annex_it$readmission$surv_val_matrix), !is.null(.annex_it$death$surv_val_matrix))
  stopifnot("eval_times must contain the four reporting horizons" =
    all(c(6, 12, 36, 60) %in% as.numeric(.annex_it$eval_times)))
  .annex_nrows <- c(.annex_nrows, nrow(as.matrix(.annex_it$readmission$surv_val_matrix)))
  message(sprintf("annex-calfig ensure: %-26s imputations=%d eval_times=[%s] readmission=%s death=%s",
    .annex_n, length(.annex_rp), paste(.annex_it$eval_times, collapse = ","),
    paste(dim(as.matrix(.annex_it$readmission$surv_val_matrix)), collapse = "x"),
    paste(dim(as.matrix(.annex_it$death$surv_val_matrix)), collapse = "x")))
}
# All three arms must describe the SAME validation patients, otherwise the
# rows of the figure are not comparable and a mixed provenance slipped
# through despite the all-or-nothing rule above.
stopifnot("the three objects disagree on the number of validation rows" =
  length(unique(.annex_nrows)) == 1L)
# Provenance must not be silently downgraded by a second run of this cell:
# re-running would otherwise always report tier 1 and erase whether the live
# objects came from the frozen workspace or from a refit.
if (identical(.annex_tier, "1 (already in kernel)") &&
    exists("ANNEX_PROVENANCE", inherits = TRUE) &&
    is.character(ANNEX_PROVENANCE) && !is.na(ANNEX_PROVENANCE) &&
    !grepl("^1 ", ANNEX_PROVENANCE)) {
  .annex_tier <- paste0(ANNEX_PROVENANCE, " [reconfirmed in kernel]")
}
ANNEX_PROVENANCE <- .annex_tier
message("annex-calfig ensure: satisfied via TIER ", ANNEX_PROVENANCE, ". Report this provenance if the annex numbers are ever questioned.")

# ---------------------------------------------------------------------------
# CHUNK 2 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-data
# ============================================================================
# ANNEX CALIBRATION FIGURE, STEP 2/6: build the two canonical data inputs.
#   annex_cal_bins  : binned calibration estimates with pointwise 95% CIs
#                     (model, time_horizon, horizon_label, bin, n_bin,
#                      bin_mean_pred, bin_obs_prop, bin_obs_lower, bin_obs_upper)
#   annex_cal_preds : individual predicted risks feeding the marginal densities
#                     (model, time_horizon, horizon_label, pred_risk)
# Prediction sources (FROZEN holdout predictions, no refitting here; MI-pooled
# by simple average across the imputation items, the convention used by
# .holdout_bootstrap_calibration_readmit_from_raw and
# bootstrap_calibration_indices):
#   A_readmit_cif : results_boot_val_bp2_cif$raw_predictions[[i]]$readmission
#                   (joint CIF: CSC readmission paired with the SHAP-informed
#                   mortality model, panel A of the main-text figure)
#   B_death_full  : results_boot_val_bp1$raw_predictions[[i]]$death
#   C_death_shap  : results_boot_val_bp2$raw_predictions[[i]]$death
# Observed side, computed WITHIN each bin of the MI-pooled predicted risk
# (outcomes are identical across imputations, so item 1's y_val is used):
#   readmission -> Aalen-Johansen CIF, death as a competing risk
#   mortality   -> Kaplan-Meier risk, 1 - S(t)
#
# UNCERTAINTY, and why there are two options (ANNEX_CI_METHOD):
#   "analytic"  (default) pointwise 95% log-log CI of the within-bin
#               Aalen-Johansen or Kaplan-Meier estimate. It answers "given
#               this group of patients, how precisely is THEIR observed risk
#               known", holding the grouping fixed. This is what grouped
#               calibration plots conventionally display and it is cheap.
#   "bootstrap" resamples the VALIDATION ROWS B times with replacement,
#               recomputing the bin boundaries and the observed estimate in
#               every replicate, then takes 2.5/97.5 percentiles per bin
#               index. It matches the convention the rest of this project
#               uses for evaluation uncertainty: `holdout-cindex-bootstrap`
#               and `holdout-ici-bootstrap-run` both resample PATIENTS with
#               B = 1000 while holding the frozen predictions fixed, never
#               refitting the models.
#               Do NOT assume the bootstrap interval is the wider of the two:
#               choose the method for the estimand, not for its width. The two
#               answer different questions. The analytic interval conditions on
#               the observed grouping. The bootstrap targets "the observed risk
#               in the k-th quantile group of the predicted-risk distribution",
#               re-estimating the group boundaries in every replicate. Because
#               the boundaries are re-quantiled, each replicate's bin holds
#               about n/g rows by construction, so this design SUPPRESSES
#               bin-occupancy variance rather than adding it.
#               Measured on this held-out sample, median bootstrap width
#               relative to analytic was 0.944 at B = 200, 0.978 at B = 500
#               and 0.986 at B = 1000: the same interval for practical
#               purposes, converging upward as B grows because small B biases
#               extreme percentiles inward. At B = 1000 the bootstrap was the
#               wider of the two in 40 of 96 bins, no bin lost a single
#               replicate to the min_bin_n rule, and the interval contained
#               the full-sample point estimate in 100% of bins, though a
#               percentile interval carries no guarantee that it will.
#               Runtime at B = 1000 was about 2.5 minutes with 8 future
#               workers, roughly 13 minutes sequentially.
#               Further caveat: percentiles are taken per bin INDEX, so a bin
#               is only approximately the same group across replicates.
#   Neither option propagates between-imputation uncertainty in the predicted
#   risks; both condition on the MI-pooled prediction, exactly as the
#   project's own bootstrap engines do.
#
# NOTE ON THE OBJECT NAMES: the `_boot` in results_boot_val_* is legacy. The
# cell `holdout-cindex-ibs-run` does NOT bootstrap; it fits on train and
# predicts on val once per imputation. Resampling in this project lives in the
# dedicated bootstrap cells only.
# Requires in kernel: results_boot_val_bp1, results_boot_val_bp2,
# results_boot_val_bp2_cif, all guaranteed by the cell `annex-calfig-ensure`.
# Place AFTER `annex-calfig-ensure`. Adds new objects; replaces nothing.
# ============================================================================
.annex_to01 <- function(e) {
  # Event coercion for the ALREADY-0/1 y_val$event written by
  # evaluate_dual_cox_holdout_dualscore(). This is deliberately NOT the
  # project's general to01(): it does not handle 1/2 coding (it would invert
  # the indicator) nor label-coded factors (it would return all NA, which
  # .annex_first_event() would read as "nobody had an event", silently
  # producing a zero-risk calibration panel). The stopifnot below turns that
  # latent failure into a loud one if the upstream encoding ever changes.
  out <- if (is.logical(e)) as.integer(e) else suppressWarnings(as.integer(as.integer(as.character(e)) == 1L))
  stopifnot("y_val$event must already be coded 0/1; got something else, refusing to guess" =
    all(!is.na(out)) && all(out %in% c(0L, 1L)))
  out
}
.annex_first_event <- function(r_yval, d_yval) {
  # First-event time and cause. VERBATIM logic of .holdout_cr_first_event() in
  # holdout_cif_cache.R, tie convention included (simultaneous readmission and
  # death counts as death first), duplicated so the annex stays self-contained.
  rt <- as.numeric(r_yval$time)
  re <- .annex_to01(r_yval$event)
  dt <- as.numeric(d_yval$time)
  de <- .annex_to01(d_yval$event)
  ftime <- pmin(rt, dt)
  r_first <- !is.na(re) & re == 1L & is.finite(ftime) & abs(rt - ftime) <= 1e-8
  d_first <- !is.na(de) & de == 1L & is.finite(ftime) & abs(dt - ftime) <= 1e-8
  fstatus <- rep(0L, length(ftime))
  fstatus[r_first & !d_first] <- 1L
  fstatus[d_first & !r_first] <- 2L
  fstatus[r_first & d_first] <- 2L
  list(ftime = ftime, fstatus = fstatus)
}
.annex_pooled_risk <- function(results_boot, block, times) {
  # MI-pooled predicted risk matrix (n_val x length(times)) for one outcome.
  rp <- results_boot$raw_predictions
  stopifnot(is.list(rp), length(rp) >= 1L)
  stopifnot("raw_predictions item lacks the requested outcome block" = !is.null(rp[[1L]][[block]]$surv_val_matrix))
  et <- as.numeric(rp[[1L]]$eval_times)
  cols <- match(times, et)
  stopifnot("requested horizons must be a subset of eval_times" = all(!is.na(cols)))
  n_val <- nrow(as.matrix(rp[[1L]][[block]]$surv_val_matrix))
  acc <- matrix(0, nrow = n_val, ncol = length(times))
  for (item in rp) {
    blk <- item[[block]]
    stopifnot("raw prediction block carries an error" = is.null(blk$error))
    sm <- as.matrix(blk$surv_val_matrix)
    stopifnot("surv_val_matrix row count differs across imputations" = nrow(sm) == n_val)
    acc <- acc + (1 - sm[, cols, drop = FALSE])
  }
  pmin(pmax(acc / length(rp), 0), 1)
}
.annex_obs_aj <- function(ftime, fstatus, horizon, ci = TRUE) {
  # Observed readmission CIF: Aalen-Johansen with death as a competing risk.
  # ci = FALSE is the bootstrap hot path, where only the point estimate per
  # replicate is used. Note it is se.fit = FALSE, not conf.type = "none", that
  # actually skips the Aalen/Greenwood variance: conf.type only suppresses
  # building the interval from a variance that was computed anyway. Measured
  # on survival 3.6.4, dropping se.fit is roughly a fourfold speedup, and the
  # point estimate (pstate) is bit-identical either way.
  st <- factor(fstatus, levels = c(0L, 1L, 2L), labels = c("censor", "readmit", "death"))
  fit <- survival::survfit(survival::Surv(ftime, st) ~ 1,
                           conf.type = if (ci) "log-log" else "none",
                           se.fit = ci, conf.int = 0.95)
  sm <- summary(fit, times = horizon, extend = TRUE)
  k <- match("readmit", fit$states)
  stopifnot("readmit state not found in survfit states" = !is.na(k))
  est <- pmin(pmax(as.numeric(sm$pstate[1L, k]), 0), 1)
  if (!ci) return(c(est = est, lower = NA_real_, upper = NA_real_))
  c(est = est,
    lower = pmin(pmax(as.numeric(sm$lower[1L, k]), 0), 1),
    upper = pmin(pmax(as.numeric(sm$upper[1L, k]), 0), 1))
}
.annex_obs_km <- function(time, event, horizon, ci = TRUE) {
  # Observed all-cause mortality risk: Kaplan-Meier 1 - S(t). CI bounds flip
  # when moving from the survival scale to the risk scale.
  fit <- survival::survfit(survival::Surv(time, event) ~ 1,
                           conf.type = if (ci) "log-log" else "none",
                           se.fit = ci, conf.int = 0.95)
  sm <- summary(fit, times = horizon, extend = TRUE)
  est <- pmin(pmax(1 - as.numeric(sm$surv[1L]), 0), 1)
  if (!ci) return(c(est = est, lower = NA_real_, upper = NA_real_))
  c(est = est,
    lower = pmin(pmax(1 - as.numeric(sm$upper[1L]), 0), 1),
    upper = pmin(pmax(1 - as.numeric(sm$lower[1L]), 0), 1))
}
.annex_bins_cut <- function(pred, g = 10L) {
  # Quantile binning. VERBATIM logic of .vhm_deciles() in
  # validate_holdout_metrics.R (type-8 quantiles, interior cuts, left-closed
  # findInterval, duplicate breaks collapsed), so bin membership matches what
  # the project's calibration engines compute on the same predictions. Engine
  # quirk kept on purpose: findInterval(all.inside = TRUE) merges each extreme
  # decile into its neighbour, so g = 10 yields up to 8 displayed bins with
  # double-width tails, exactly as in cal_*$calibration_curves.
  cuts <- unique(stats::quantile(pred, probs = seq(0, 1, length.out = g + 1L), na.rm = TRUE, names = FALSE, type = 8))
  cuts <- cuts[-c(1L, length(cuts))]
  if (length(cuts)) findInterval(pred, cuts, all.inside = TRUE) + 1L else rep(1L, length(pred))
}
.annex_hlab <- function(x) {
  factor(paste0(x, " months"), levels = paste0(c(6L, 12L, 36L, 60L), " months"))
}
# ---- Fixed design ----------------------------------------------------------
ANNEX_TIMES <- c(6, 12, 36, 60)
.annex_g <- 10L
.annex_min_bin_n <- 20L
if (!exists("ANNEX_CI_METHOD", inherits = TRUE)) ANNEX_CI_METHOD <- "analytic"
ANNEX_CI_METHOD <- match.arg(ANNEX_CI_METHOD, c("analytic", "bootstrap"))
if (!exists("ANNEX_BOOT_B", inherits = TRUE)) ANNEX_BOOT_B <- 1000L
if (!exists("ANNEX_BOOT_SEED", inherits = TRUE)) ANNEX_BOOT_SEED <- 2125L
if (!exists("ANNEX_BOOT_PARALLEL", inherits = TRUE)) ANNEX_BOOT_PARALLEL <- FALSE
if (identical(ANNEX_CI_METHOD, "bootstrap") && ANNEX_BOOT_B < 500L) {
  # A 2.5/97.5 percentile interval is read off the extreme order statistics, so
  # with a small B each endpoint rests on one or two replicates: the interval is
  # unstable and biased NARROW, which would understate uncertainty in a figure.
  # 1000 matches ICI_B in `holdout-ici-bootstrap-run`.
  warning(sprintf("annex-calfig: ANNEX_BOOT_B = %d is too small for stable 2.5/97.5 percentiles; use at least 500, ideally 1000, for anything that goes into the manuscript.", ANNEX_BOOT_B), call. = FALSE)
}
.annex_model_specs <- list(
  A_readmit_cif = list(obj_name = "results_boot_val_bp2_cif", block = "readmission", observed = "aj"),
  B_death_full = list(obj_name = "results_boot_val_bp1", block = "death", observed = "km"),
  C_death_shap = list(obj_name = "results_boot_val_bp2", block = "death", observed = "km")
)
# ---- Preflight: name every missing object and the cell that supplies it -----
.annex_needed <- vapply(.annex_model_specs, function(s) s$obj_name, character(1L))
.annex_have <- vapply(.annex_needed, exists, logical(1L), inherits = TRUE)
for (.annex_k in seq_along(.annex_needed)) {
  message(sprintf("annex-calfig prerequisite: %-26s %s", .annex_needed[[.annex_k]],
    if (.annex_have[[.annex_k]]) "PRESENT" else "MISSING"))
}
if (any(!.annex_have)) {
  stop(sprintf(
    "annex-calfig cannot run: missing object(s) %s. Run the cell `annex-calfig-ensure`, which recovers them from the frozen workspace in seconds or refits them from source if that is unavailable.",
    paste(unique(.annex_needed[!.annex_have]), collapse = ", ")
  ), call. = FALSE)
}
message("annex-calfig: confidence intervals via ", ANNEX_CI_METHOD,
        if (identical(ANNEX_CI_METHOD, "bootstrap")) paste0(" (B = ", ANNEX_BOOT_B, ", seed ", ANNEX_BOOT_SEED, ")") else "", ".")
# ---- Bootstrap engine, used only when ANNEX_CI_METHOD == "bootstrap" -------
.annex_boot_bin_ci <- function(pv, obs, kind, horizon, keep_bins, B, seed, parallel) {
  # Resample the validation rows, recompute bins AND the within-bin observed
  # estimate in each replicate, then take percentiles per bin index. The model
  # and its predictions are held fixed, mirroring the project's other
  # bootstrap engines, which resample the evaluation data and never the fit.
  #
  # SEEDING: each replicate seeds itself from seed + horizon_index * 100000 + b,
  # the exact pattern .holdout_cindex_boot_dualscore() uses in
  # evaluate_dual_cox_holdout_dualscore.R. Seeding INSIDE the replicate, rather
  # than once before the loop, makes the resample a pure function of b, so the
  # sequential and parallel branches return bit-identical intervals. Verified:
  # without this, the two branches disagreed by up to 0.010 on interval
  # endpoints whose total width is about 0.013, i.e. the same ANNEX_BOOT_SEED
  # silently gave different answers depending on ANNEX_BOOT_PARALLEL.
  # The stream varies by horizon but NOT by model, so all three model rows
  # share their resamples at a given horizon (common random numbers), which is
  # what makes any across-arm comparison of these intervals paired.
  n <- length(pv)
  hi <- match(horizon, ANNEX_TIMES)
  one_rep <- function(.b) {
    set.seed(seed + hi * 100000L + .b, kind = "Mersenne-Twister")
    idx <- sample.int(n, n, replace = TRUE)
    pvb <- pv[idx]
    binsb <- .annex_bins_cut(pvb, .annex_g)
    vapply(keep_bins, function(k) {
      ii <- idx[binsb == k]
      if (length(ii) < .annex_min_bin_n) return(NA_real_)
      if (identical(kind, "aj")) {
        .annex_obs_aj(obs$ftime[ii], obs$fstatus[ii], horizon, ci = FALSE)[["est"]]
      } else {
        .annex_obs_km(obs$time[ii], obs$event[ii], horizon, ci = FALSE)[["est"]]
      }
    }, numeric(1L))
  }
  if (parallel && !requireNamespace("future.apply", quietly = TRUE)) {
    # Never degrade silently: without this the same ANNEX_BOOT_PARALLEL <- TRUE
    # would run in two different ways depending on whether a package happens to
    # be installed, with nothing in the log to say which one happened.
    warning("annex-calfig: ANNEX_BOOT_PARALLEL is TRUE but future.apply is not installed; running sequentially. Results are unaffected (seeding is per replicate) but this will be slower.", call. = FALSE)
    parallel <- FALSE
  }
  if (parallel) {
    # future.seed = TRUE only keeps future.apply's RNG bookkeeping quiet; the
    # set.seed() inside one_rep() runs afterwards and is what actually fixes
    # the resample. VERIFIED 2026-08-20: this branch is bit-identical to the
    # sequential one at the same seed, and about 2.3 times faster.
    reps <- future.apply::future_lapply(seq_len(B), one_rep, future.seed = TRUE)
  } else {
    reps <- lapply(seq_len(B), one_rep)
  }
  mat <- do.call(rbind, reps)
  data.frame(
    bin = keep_bins,
    lower = apply(mat, 2L, stats::quantile, probs = 0.025, na.rm = TRUE),
    upper = apply(mat, 2L, stats::quantile, probs = 0.975, na.rm = TRUE),
    n_rep_ok = apply(mat, 2L, function(z) sum(is.finite(z)))
  )
}
# ---- Build ------------------------------------------------------------------
.annex_bins_acc <- list()
.annex_preds_acc <- list()
for (.annex_mc in names(.annex_model_specs)) {
  .annex_spec <- .annex_model_specs[[.annex_mc]]
  .annex_rb <- get(.annex_spec$obj_name, inherits = TRUE)
  .annex_pred_mat <- .annex_pooled_risk(.annex_rb, .annex_spec$block, ANNEX_TIMES)
  .annex_it1 <- .annex_rb$raw_predictions[[1L]]
  if (identical(.annex_spec$observed, "aj")) {
    .annex_obs <- .annex_first_event(.annex_it1$readmission$y_val, .annex_it1$death$y_val)
    stopifnot("y_val rows must match prediction rows" = length(.annex_obs$ftime) == nrow(.annex_pred_mat))
  } else {
    .annex_obs <- list(time = as.numeric(.annex_it1$death$y_val$time),
                       event = .annex_to01(.annex_it1$death$y_val$event))
    stopifnot("y_val rows must match prediction rows" = length(.annex_obs$time) == nrow(.annex_pred_mat))
  }
  for (.annex_j in seq_along(ANNEX_TIMES)) {
    .annex_h <- ANNEX_TIMES[.annex_j]
    .annex_pv <- .annex_pred_mat[, .annex_j]
    .annex_preds_acc[[paste(.annex_mc, .annex_h, sep = "_")]] <- data.frame(
      model = .annex_mc, time_horizon = .annex_h, pred_risk = as.numeric(.annex_pv)
    )
    .annex_binvec <- .annex_bins_cut(.annex_pv, .annex_g)
    .annex_keep <- sort(unique(.annex_binvec))
    .annex_keep <- .annex_keep[vapply(.annex_keep, function(k) sum(.annex_binvec == k) >= .annex_min_bin_n, logical(1L))]
    .annex_rows_h <- lapply(.annex_keep, function(k) {
      ii <- which(.annex_binvec == k)
      ob <- if (identical(.annex_spec$observed, "aj")) {
        .annex_obs_aj(.annex_obs$ftime[ii], .annex_obs$fstatus[ii], .annex_h, ci = TRUE)
      } else {
        .annex_obs_km(.annex_obs$time[ii], .annex_obs$event[ii], .annex_h, ci = TRUE)
      }
      data.frame(model = .annex_mc, time_horizon = .annex_h, bin = k, n_bin = length(ii),
        bin_mean_pred = mean(.annex_pv[ii], na.rm = TRUE),
        bin_obs_prop = ob[["est"]], bin_obs_lower = ob[["lower"]], bin_obs_upper = ob[["upper"]])
    })
    .annex_df_h <- do.call(rbind, .annex_rows_h)
    if (identical(ANNEX_CI_METHOD, "bootstrap")) {
      .annex_bci <- .annex_boot_bin_ci(.annex_pv, .annex_obs, .annex_spec$observed, .annex_h,
                                       .annex_keep, ANNEX_BOOT_B, ANNEX_BOOT_SEED, ANNEX_BOOT_PARALLEL)
      .annex_mm <- match(.annex_df_h$bin, .annex_bci$bin)
      .annex_df_h$bin_obs_lower <- .annex_bci$lower[.annex_mm]
      .annex_df_h$bin_obs_upper <- .annex_bci$upper[.annex_mm]
      # n_rep_ok travels WITH the data, not just into a log line: replicates
      # where a bin fell below min_bin_n are dropped by na.rm, so the effective
      # B behind a given endpoint can be lower than ANNEX_BOOT_B, and a reader
      # of annex_cal_bins must be able to see that.
      .annex_df_h$n_rep_ok <- .annex_bci$n_rep_ok[.annex_mm]
      message(sprintf("annex-calfig bootstrap %s at %d months: %d bins, min usable replicates %d of %d.",
        .annex_mc, .annex_h, nrow(.annex_df_h), min(.annex_df_h$n_rep_ok), ANNEX_BOOT_B))
    } else {
      .annex_df_h$n_rep_ok <- NA_integer_
    }
    .annex_bins_acc[[paste(.annex_mc, .annex_h, sep = "_")]] <- .annex_df_h
  }
}
annex_cal_bins <- do.call(rbind, .annex_bins_acc)
rownames(annex_cal_bins) <- NULL
annex_cal_bins$horizon_label <- .annex_hlab(annex_cal_bins$time_horizon)
annex_cal_preds <- do.call(rbind, .annex_preds_acc)
rownames(annex_cal_preds) <- NULL
annex_cal_preds$horizon_label <- .annex_hlab(annex_cal_preds$time_horizon)
ANNEX_CI_LABEL <- if (identical(ANNEX_CI_METHOD, "bootstrap")) {
  sprintf("bootstrap percentile (B = %d, validation rows resampled)", ANNEX_BOOT_B)
} else {
  "analytic log-log (Aalen-Johansen / Kaplan-Meier)"
}
# ---- Validation and transparency checks, printed and never silent ----------
stopifnot(
  all(c("model", "time_horizon", "horizon_label", "bin", "n_bin", "bin_mean_pred", "bin_obs_prop", "bin_obs_lower", "bin_obs_upper") %in% names(annex_cal_bins)),
  all(c("model", "time_horizon", "horizon_label", "pred_risk") %in% names(annex_cal_preds)),
  all(sort(unique(annex_cal_bins$time_horizon)) == ANNEX_TIMES)
)
if (identical(ANNEX_CI_METHOD, "bootstrap")) {
  .annex_thin <- which(annex_cal_bins$n_rep_ok < 0.95 * ANNEX_BOOT_B)
  if (length(.annex_thin) > 0L) {
    warning(sprintf("annex-calfig: %d bins rest on fewer than 95%% of the %d replicates (minimum %d), because those replicates put the bin below min_bin_n; their percentile endpoints are correspondingly less stable.",
      length(.annex_thin), ANNEX_BOOT_B, min(annex_cal_bins$n_rep_ok, na.rm = TRUE)), call. = FALSE)
  }
}
.annex_ci_na <- sum(is.na(annex_cal_bins$bin_obs_lower) | is.na(annex_cal_bins$bin_obs_upper))
if (.annex_ci_na > 0L) {
  warning(sprintf("annex-calfig: %d bins have an undefined 95%% CI (usually zero events in the bin); their error bars are omitted.", .annex_ci_na), call. = FALSE)
}
.annex_ci_bad <- with(annex_cal_bins, which(!is.na(bin_obs_lower) & !is.na(bin_obs_upper) &
  (bin_obs_lower - bin_obs_prop > 1e-8 | bin_obs_prop - bin_obs_upper > 1e-8)))
if (length(.annex_ci_bad) > 0L) {
  # Under the bootstrap this is possible rather than pathological: the
  # percentile interval need not contain the full-sample point estimate.
  warning(sprintf("annex-calfig: %d bins have a CI that does not bracket the point estimate (%s).",
    length(.annex_ci_bad), ANNEX_CI_METHOD), call. = FALSE)
  if (identical(ANNEX_CI_METHOD, "analytic")) {
    stop("annex-calfig: analytic CIs must bracket the point estimate; this indicates a bug, not a data feature.", call. = FALSE)
  }
}
.annex_row_caps <- c(A_readmit_cif = 0.4, B_death_full = 0.15, C_death_shap = 0.15)
for (.annex_mc in names(.annex_row_caps)) {
  .annex_cap <- .annex_row_caps[[.annex_mc]]
  .annex_bb <- annex_cal_bins[annex_cal_bins$model == .annex_mc, , drop = FALSE]
  .annex_pp <- annex_cal_preds$pred_risk[annex_cal_preds$model == .annex_mc]
  message(sprintf(
    "annex-calfig %s: %d bins, max bin_mean_pred %.3f, max bin_obs_upper %.3f, axis cap %.2f (%d bin values beyond cap), %.2f%% of individual predictions beyond cap.",
    .annex_mc, nrow(.annex_bb), max(.annex_bb$bin_mean_pred, na.rm = TRUE),
    max(.annex_bb$bin_obs_upper, na.rm = TRUE), .annex_cap,
    sum(.annex_bb$bin_mean_pred > .annex_cap | .annex_bb$bin_obs_upper > .annex_cap, na.rm = TRUE),
    100 * mean(.annex_pp > .annex_cap, na.rm = TRUE)))
}
print(dplyr::count(annex_cal_bins, model, time_horizon, name = "n_bins_kept"))
print(utils::head(annex_cal_bins, 4L))

# ---------------------------------------------------------------------------
# CHUNK 3 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-theme
# ============================================================================
# ANNEX CALIBRATION FIGURE, STEP 3/6: display constants and shared theme.
# Grayscale-first (Epidemiology prints colour at the author's expense):
# ANNEX_USE_COLOR = FALSE renders everything in near-black with mid-gray
# density fills. Nothing in the figure DEPENDS on hue: the two mortality
# models are separated by marker shape and line type.
# LAYOUT: two rows of four horizons. Row A is first readmission. Row B
# overlays BOTH mortality models, which roughly doubles that row's height and
# is what makes the confidence bands and the departures from the diagonal
# legible. Marginal densities are separate theme_void() strips composed with
# patchwork::area(), the arrangement used by create_cal_published(), not
# ggside: a ggside strip inherits axis.line from the parent theme, which is
# where the stray black line along the y-side density came from.
# Requires in kernel: nothing (self-contained constants).
# Place AFTER `annex-calfig-data`. Adds new objects; replaces nothing.
# ============================================================================
ANNEX_USE_COLOR <- FALSE
.annex_font <- if (exists("tnr", inherits = TRUE) && is.character(tnr) && length(tnr) == 1L) tnr else "Times New Roman"
.annex_rows_spec <- list(
  list(tag = "A", desc = "First readmission", models = "A_readmit_cif",
       lim = c(0, 0.4), breaks = seq(0, 0.4, by = 0.1), acc = 0.01),
  list(tag = "B", desc = "All-cause mortality", models = c("B_death_full", "C_death_shap"),
       lim = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), acc = 0.01)
)
.annex_aes <- list(
  A_readmit_cif = list(shape = 21, linetype = "solid", label = "Joint CIF"),
  B_death_full  = list(shape = 21, linetype = "solid", label = "All predictors"),
  C_death_shap  = list(shape = 24, linetype = "longdash", label = "SHAP-informed")
)
.annex_col <- function(mc) {
  if (!ANNEX_USE_COLOR) return(if (identical(mc, "C_death_shap")) "gray40" else "gray5")
  switch(mc, A_readmit_cif = "#2166AC", B_death_full = "#B2182B", C_death_shap = "#4D4D4D", "gray5")
}
.annex_fill <- function(mc) {
  if (!ANNEX_USE_COLOR) return(if (identical(mc, "C_death_shap")) "gray72" else "gray48")
  switch(mc, A_readmit_cif = "#2166AC", B_death_full = "#B2182B", C_death_shap = "#999999", "gray48")
}
# HORIZONTAL DODGE, DELIBERATELY DISABLED BY DEFAULT.
# A dodge was requested so the two overlaid mortality series stop sitting on
# top of each other, and it is implemented below, but it ships OFF because on
# a calibration plot it corrupts the one reading the figure exists to support.
# The reference line is y = x, so drawing a marker at (x + d, y) turns its
# vertical distance from the diagonal into y - (x + d) instead of y - x. With
# a symmetric dodge the two mortality models are pushed in OPPOSITE
# directions, so their apparent miscalibration is biased in opposite
# directions too, and at 6 and 12 months the offset is comparable to the
# predicted risk itself. Shape and line type already separate the series.
# Set to a small positive fraction of the row range to re-enable, in which
# case the offset and its size MUST be stated in the figure note.
if (!exists("ANNEX_MORT_DODGE_FRAC", inherits = TRUE)) ANNEX_MORT_DODGE_FRAC <- 0
# Axis range policy. FALSE keeps ONE range per row across all four horizons,
# which makes the horizons directly comparable and lets the tick labels be
# printed once per row, but leaves the 6 and 12 month mortality panels nearly
# empty since observed risk there reaches only about 0.02 on a 0 to 0.15 axis.
# TRUE gives each horizon its own range, driven by that horizon's calibration
# data, and then EVERY panel is labelled, because unlabelled panels on
# differing scales would be read on the first panel's scale. x and y always
# share the same range within a panel, and panels are square, so the identity
# line is a true 45 degree diagonal either way.
if (!exists("ANNEX_PER_HORIZON_LIMITS", inherits = TRUE)) ANNEX_PER_HORIZON_LIMITS <- FALSE
ANNEX_POINT_SIZE <- 1.05
ANNEX_POINT_STROKE <- 0.45
.annex_theme_main <- function(base_family = .annex_font) {
  # aspect.ratio = 1 is not cosmetic: without it the panel is wider than it is
  # tall, the dashed identity line is not a 45 degree diagonal, and vertical
  # departures from calibration are visually exaggerated relative to
  # horizontal ones.
  ggplot2::theme_classic(base_size = 10, base_family = base_family) +
    ggplot2::theme(
      aspect.ratio = 1,
      plot.subtitle = ggplot2::element_text(size = 9.5, face = "bold", hjust = 0.5,
                                            margin = ggplot2::margin(t = 0, b = 0.5)),
      axis.text = ggplot2::element_text(size = 8.2, color = "black"),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 1)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 1)),
      axis.line = ggplot2::element_line(linewidth = 0.55, color = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.55, color = "black"),
      axis.ticks.length = ggplot2::unit(1.6, "pt"),
      axis.title = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 0, r = 1, b = 0, l = 1)
    )
}
.annex_theme_dens <- function() {
  ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(0, 0, 0, 0))
}

# ---------------------------------------------------------------------------
# CHUNK 4 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-builder
# ============================================================================
# ANNEX CALIBRATION FIGURE, STEP 4/6: panel and row builders.
# .annex_build_panel() returns ONE time-horizon panel: the calibration plot
# plus two marginal density strips composed with patchwork::area(), the same
# arrangement as create_cal_published(). The strips are theme_void(), so they
# carry no axis line, no ticks and no background.
# Panel contents: dashed identity line; binned observed vs mean predicted
# risk, joined; pointwise 95% CI error bars; a bottom strip with the density
# of INDIVIDUAL predicted risks; and a right strip with that same
# distribution projected onto the observed axis, i.e. where observed risks
# would fall under perfect calibration.
# AXIS LABELS: the y tick label at the panel minimum is always suppressed,
# since the x axis already labels the origin. x tick marks and numbers are
# drawn on EVERY panel so each can be read on its own. y tick numbers are
# kept to the leftmost panel of a row when all four share one range, and are
# drawn on every panel under ANNEX_PER_HORIZON_LIMITS, where the ranges
# genuinely differ and an unlabelled panel would be read on the neighbour.
# Column headers are printed on the top row only.
# Panel letters and row descriptions are drawn INSIDE the first panel of each
# row, in the empty upper-left corner, which is where the journal expects the
# panel letter and where nothing else competes for space.
# Every reduction the drawing performs (bins outside the range, error bars
# clipped at the edge, bins with no estimable CI, predictions excluded from
# the density) is both messaged and accumulated for the figure note.
# Requires in kernel: annex_cal_bins, annex_cal_preds (from
# `annex-calfig-data`) and the constants from `annex-calfig-theme`.
# Place AFTER `annex-calfig-theme`. Adds new objects; replaces nothing.
# ============================================================================
.annex_diag <- new.env(parent = emptyenv())
.annex_diag$rec <- list()
.annex_diag_reset <- function() .annex_diag$rec <- list()
.annex_diag_add <- function(kind, value, txt) {
  # Records are kept STRUCTURED, not as prose, so the figure note can report a
  # single summary per kind instead of replaying one sentence per panel.
  .annex_diag$rec[[length(.annex_diag$rec) + 1L]] <- list(kind = kind, value = value)
  message("annex-calfig ", txt)
}
.annex_dodge_for <- function(mc, lim) {
  # Display-only horizontal offset, zero unless ANNEX_MORT_DODGE_FRAC was set.
  # See the long note in `annex-calfig-theme` for why this ships disabled.
  if (identical(mc, "B_death_full")) return(-ANNEX_MORT_DODGE_FRAC * diff(lim) / 2)
  if (identical(mc, "C_death_shap")) return(+ANNEX_MORT_DODGE_FRAC * diff(lim) / 2)
  0
}
.annex_limits_for <- function(row_spec, horizon) {
  # One range per row (default) or one per horizon. The per-horizon top is
  # driven by the CALIBRATION data only: the highest bin mean prediction, the
  # highest observed value and the highest CI upper bound. The density strip
  # long right tail is deliberately NOT allowed to set the range, since doing
  # so pushes every panel back into being mostly empty, which is the problem
  # per-horizon ranges exist to solve; the strip simply clips its own tail.
  if (!isTRUE(ANNEX_PER_HORIZON_LIMITS)) {
    return(list(lim = row_spec$lim, breaks = row_spec$breaks, acc = row_spec$acc))
  }
  b <- dplyr::filter(annex_cal_bins, model %in% row_spec$models, time_horizon == horizon)
  hi <- max(c(b$bin_mean_pred, b$bin_obs_prop, b$bin_obs_upper), na.rm = TRUE)
  step <- 10^floor(log10(hi)) / 2
  top <- ceiling(hi / step) * step
  # At most four ticks, otherwise the labels collide into one unreadable
  # numeric string at print size.
  brk <- pretty(c(0, top), n = 3L)
  brk <- brk[brk >= 0 & brk <= top + 1e-12]
  while (length(brk) > 4L) brk <- brk[seq(1L, length(brk), by = 2L)]
  # Accuracy comes from the BREAK SPACING, not from the range top: deriving it
  # from the top produced labels that were numerically wrong and duplicated,
  # including an interior tick printed as if it were the panel minimum.
  stp <- if (length(brk) > 1L) min(diff(brk)) else top
  acc <- 10^floor(log10(stp))
  lab <- scales::number(brk[brk > 0], accuracy = acc)
  stopifnot("per-horizon tick labels collide; widen the accuracy" = !any(duplicated(lab)))
  list(lim = c(0, top), breaks = brk, acc = acc)
}
.annex_build_panel <- function(row_spec, horizon, show_x, show_y, show_legend, show_tag, show_title) {
  mcs <- row_spec$models
  .lm <- .annex_limits_for(row_spec, horizon)
  lim <- .lm$lim
  who <- paste(mcs, collapse = "+")
  bins <- dplyr::filter(annex_cal_bins, model %in% mcs, time_horizon == horizon)
  preds <- dplyr::filter(annex_cal_preds, model %in% mcs, time_horizon == horizon)
  stopifnot(nrow(bins) > 0L, nrow(preds) > 0L)
  bins$model <- factor(bins$model, levels = mcs)
  preds$model <- factor(preds$model, levels = mcs)
  # Membership is judged on the TRUE mean predicted risk, never on the dodged
  # one, so a purely cosmetic offset can never delete a bin that genuinely
  # belongs in the panel.
  keep <- bins$bin_mean_pred >= lim[1L] & bins$bin_mean_pred <= lim[2L] &
    bins$bin_obs_prop >= lim[1L] & bins$bin_obs_prop <= lim[2L]
  if (any(!keep)) {
    .annex_diag_add("outside", sum(!keep),
                    sprintf("%s at %d months: %d bins fall outside the panel range and are not drawn.",
                            who, horizon, sum(!keep)))
    bins <- bins[keep, , drop = FALSE]
  }
  bins$x_plot <- bins$bin_mean_pred + vapply(as.character(bins$model), .annex_dodge_for, numeric(1L), lim = lim)
  bins$x_plot <- pmin(pmax(bins$x_plot, lim[1L]), lim[2L])
  bins$lo_d <- pmax(bins$bin_obs_lower, lim[1L])
  bins$hi_d <- pmin(bins$bin_obs_upper, lim[2L])
  .n_trunc <- sum(bins$bin_obs_upper > lim[2L] | bins$bin_obs_lower < lim[1L], na.rm = TRUE)
  if (.n_trunc > 0L) {
    .annex_diag_add("trunc", .n_trunc,
                    sprintf("%s at %d months: %d error bars truncated at the panel edge.",
                            who, horizon, .n_trunc))
  }
  .n_noci <- sum(is.na(bins$bin_obs_lower) | is.na(bins$bin_obs_upper))
  if (.n_noci > 0L) {
    .annex_diag_add("noci", .n_noci,
                    sprintf("%s at %d months: %d bins have no estimable CI and are drawn without an error bar.",
                            who, horizon, .n_noci))
  }
  # Break the joining line wherever a bin was removed, so the curve is never
  # drawn straight through a region whose bin exists but is not shown.
  bins <- bins[order(bins$model, bins$bin), , drop = FALSE]
  bins$seg <- unlist(lapply(split(bins$bin, bins$model), function(z)
    cumsum(c(0L, as.integer(diff(z) != 1L)))), use.names = FALSE)
  bins$grp <- paste(bins$model, bins$seg, sep = "_")
  preds_in <- dplyr::filter(preds, pred_risk >= lim[1L], pred_risk <= lim[2L])
  .n_out <- nrow(preds) - nrow(preds_in)
  if (.n_out > 0L) {
    .annex_diag_add("density", 100 * .n_out / nrow(preds),
                    sprintf("%s at %d months: %.2f%% of individual predictions lie beyond the panel cap and are excluded from the density strips.",
                            who, horizon, 100 * .n_out / nrow(preds)))
  }
  shp <- vapply(mcs, function(m) .annex_aes[[m]]$shape, numeric(1L))
  lty <- vapply(mcs, function(m) .annex_aes[[m]]$linetype, character(1L))
  cols <- vapply(mcs, .annex_col, character(1L))
  fills <- vapply(mcs, .annex_fill, character(1L))
  labs <- vapply(mcs, function(m) .annex_aes[[m]]$label, character(1L))
  .ylab_fmt <- function(b) {
    out <- scales::number(b, accuracy = .lm$acc)
    out[is.finite(b) & abs(b - lim[1L]) < 1e-12] <- ""
    out
  }
  .xlab_fmt <- function(b) if (show_x) scales::number(b, accuracy = .lm$acc) else rep("", length(b))
  main <- ggplot2::ggplot(bins, ggplot2::aes(x = x_plot, y = bin_obs_prop)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dotted", linewidth = 0.3, color = "gray70") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo_d, ymax = hi_d, color = model),
                           width = 0, linewidth = 0.42, alpha = 0.95, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(color = model, linetype = model, group = grp), linewidth = 0.55) +
    ggplot2::geom_point(ggplot2::aes(color = model, shape = model),
                        size = ANNEX_POINT_SIZE, stroke = ANNEX_POINT_STROKE, fill = "white") +
    ggplot2::scale_color_manual(values = cols, labels = labs, name = NULL) +
    ggplot2::scale_shape_manual(values = shp, labels = labs, name = NULL) +
    ggplot2::scale_linetype_manual(values = lty, labels = labs, name = NULL) +
    ggplot2::scale_x_continuous(limits = lim, breaks = .lm$breaks, labels = .xlab_fmt,
                                expand = ggplot2::expansion(mult = c(0.012, 0.012))) +
    ggplot2::scale_y_continuous(limits = lim, breaks = .lm$breaks, labels = .ylab_fmt,
                                expand = ggplot2::expansion(mult = c(0.012, 0.012))) +
    ggplot2::labs(subtitle = if (show_title) paste0(horizon, " months") else NULL) +
    .annex_theme_main()
  if (show_tag) {
    main <- main +
      ggplot2::annotate("text", x = lim[1L], y = lim[2L],
                        label = paste0(row_spec$tag, "  ", row_spec$desc),
                        hjust = 0, vjust = 1, fontface = "bold", size = 3.15,
                        family = .annex_font, color = "black")
  }
  # Every panel now carries x tick numbers, and the outermost of those extend
  # past the panel edge, so without a gutter the last label of one panel butts
  # against the first label of the next (they rendered as "0.150.00"). A left
  # inset on each panel supplies that gutter.
  main <- main + ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 1, b = 0, l = 8))
  if (!show_y) {
    # An unlabelled axis must not keep its ticks: orphan tick marks beside a
    # neighbouring density strip are what made the previous version look as if
    # a black line ran down the density.
    main <- main + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                  axis.ticks.y = ggplot2::element_blank())
  }
  if (show_legend && length(mcs) > 1L) {
    main <- main + ggplot2::theme(
      legend.position = c(0.02, 0.86), legend.justification = c(0, 1),
      legend.background = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(8, "pt"), legend.key.width = ggplot2::unit(13, "pt"),
      legend.text = ggplot2::element_text(size = 8, family = .annex_font),
      legend.margin = ggplot2::margin(0, 0, 0, 0), legend.spacing.y = ggplot2::unit(0, "pt"))
  }
  .dens <- function(flip) {
    g <- ggplot2::ggplot(preds_in, ggplot2::aes(x = pred_risk, group = model, fill = model)) +
      ggplot2::geom_density(color = NA, alpha = 0.55, adjust = 1, trim = TRUE) +
      ggplot2::scale_fill_manual(values = fills) +
      ggplot2::scale_x_continuous(limits = lim, expand = ggplot2::expansion(mult = c(0.012, 0.012))) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      .annex_theme_dens()
    if (flip) g + ggplot2::coord_flip() else g
  }
  design <- c(patchwork::area(1, 1, 12, 12), patchwork::area(1, 13, 12, 13), patchwork::area(13, 1, 13, 12))
  main + .dens(TRUE) + .dens(FALSE) + patchwork::plot_layout(design = design)
}
.annex_build_row <- function(row_spec, horizons = ANNEX_TIMES, show_title = TRUE) {
  # Column headers ("6 months" ...) are printed on the TOP row only: the
  # columns are the same horizons in every row, so repeating the headers costs
  # vertical space that the panels can use instead.
  # x tick marks and numbers, by contrast, are drawn on EVERY panel of every
  # row, so each panel can be read on its own without tracing back to a
  # neighbour, which also keeps the figure honest under
  # ANNEX_PER_HORIZON_LIMITS where the ranges genuinely differ.
  .per_h <- isTRUE(ANNEX_PER_HORIZON_LIMITS)
  panels <- lapply(seq_along(horizons), function(i)
    .annex_build_panel(row_spec, horizons[i],
                       show_x = TRUE, show_y = (.per_h || i == 1L),
                       show_legend = (i == 1L), show_tag = (i == 1L),
                       show_title = show_title))
  patchwork::wrap_plots(panels, ncol = length(horizons))
}

# ---------------------------------------------------------------------------
# CHUNK 5 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-assemble
#| fig-width: 6.85
#| fig-height: 3.85
# ============================================================================
# ANNEX CALIBRATION FIGURE, STEP 5/6: assemble the two-row panel and build the
# figure note.
# Row A is first readmission. Row B overlays both mortality models, which is
# what frees the vertical space that makes the error bars and the departures
# from the diagonal readable. Panel letters are drawn inside the panels by
# `annex-calfig-builder`, not by plot_annotation(): patchwork tags were
# rendering in the wrong family and size and were colliding with the top y
# tick label, and the journal wants the letter in the panel's upper left in
# any case.
# ANNEX_FIG_NOTE is assembled from what the drawing actually did, so the
# statement in the manuscript cannot drift from the artwork. It is printed,
# not burned into the image, because a journal expects the note as caption
# text rather than as part of the figure file.
# Requires in kernel: .annex_build_row, .annex_rows_spec, ANNEX_CI_LABEL.
# Place AFTER `annex-calfig-builder`. Adds new objects; replaces nothing.
# ============================================================================
.annex_diag_reset()
annex_row_A <- .annex_build_row(.annex_rows_spec[[1L]], show_title = TRUE)
annex_row_B <- .annex_build_row(.annex_rows_spec[[2L]], show_title = FALSE)
.annex_rows <- patchwork::wrap_plots(
  patchwork::wrap_elements(full = annex_row_A, clip = FALSE),
  patchwork::wrap_elements(full = annex_row_B, clip = FALSE),
  ncol = 1L
) +
  patchwork::plot_annotation(
    caption = "Predicted probability",
    theme = ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 9.5, face = "bold", hjust = 0.5,
                                           family = .annex_font,
                                           margin = ggplot2::margin(t = 1)),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1)
    )
  )
.annex_y_title <- patchwork::wrap_elements(
  full = grid::textGrob(
    "Observed probability",
    rot = 90,
    gp = grid::gpar(fontsize = 9.5, fontface = "bold", fontfamily = .annex_font)
  ),
  clip = FALSE
)
annex_cal_fig <- patchwork::wrap_plots(
  .annex_y_title,
  patchwork::wrap_elements(full = .annex_rows, clip = FALSE),
  ncol = 2L,
  widths = c(0.014, 1)
)
print(annex_cal_fig)
# ---- Figure note, assembled from what was actually drawn -------------------
.annex_scale_txt <- if (isTRUE(ANNEX_PER_HORIZON_LIMITS)) {
  "Each time horizon is plotted on its own risk range, so panels are not directly comparable across horizons; every panel is therefore labelled."
} else {
  "Within each row all four horizons share one risk range, so panels are directly comparable across horizons; tick labels are shown once per row."
}
.annex_dodge_txt <- if (ANNEX_MORT_DODGE_FRAC > 0) {
  sprintf(" The two mortality series are displaced horizontally by %.3f probability units for legibility, so their markers do not sit exactly at their mean predicted risk.",
          ANNEX_MORT_DODGE_FRAC * diff(.annex_rows_spec[[2L]]$lim) / 2)
} else {
  ""
}
ANNEX_FIG_NOTE <- paste0(
  "Panel A shows first readmission, with observed cumulative incidence from the Aalen-Johansen estimator treating death as a competing risk. ",
  "Panel B overlays the two all-cause mortality models, with observed risk from the Kaplan-Meier estimator; the two are distinguished by marker shape and line type. ",
  "Points are observed against mean predicted risk within quantile groups of predicted risk (up to 8 groups; groups with fewer than 20 individuals suppressed). ",
  "Error bars are pointwise 95% confidence intervals: ", ANNEX_CI_LABEL, ". ",
  "Predicted risks are the frozen held-out predictions averaged across the five imputations. ",
  "The dotted line marks perfect calibration; x and y share the same range and the panels are square, so it is a true 45 degree diagonal. ",
  "Shaded marginal strips show the distribution of individual predicted risks; the strip on the right projects that distribution onto the observed-probability axis, representing where observed risks would fall under perfect calibration. ",
  .annex_scale_txt, .annex_dodge_txt
)
.annex_kind_total <- function(k) {
  v <- vapply(Filter(function(z) identical(z$kind, k), .annex_diag$rec),
              function(z) as.numeric(z$value), numeric(1L))
  if (!length(v)) 0 else sum(v)
}
.annex_kind_max <- function(k) {
  v <- vapply(Filter(function(z) identical(z$kind, k), .annex_diag$rec),
              function(z) as.numeric(z$value), numeric(1L))
  if (!length(v)) 0 else max(v)
}
.annex_bits <- character(0)
if (.annex_kind_total("outside") > 0)
  .annex_bits <- c(.annex_bits, sprintf("%d quantile groups fell outside the plotted range and are not shown", as.integer(.annex_kind_total("outside"))))
if (.annex_kind_total("trunc") > 0)
  .annex_bits <- c(.annex_bits, sprintf("%d error bars are truncated at the panel edge", as.integer(.annex_kind_total("trunc"))))
if (.annex_kind_total("noci") > 0)
  .annex_bits <- c(.annex_bits, sprintf("%d groups have no estimable confidence interval and are drawn without an error bar", as.integer(.annex_kind_total("noci"))))
if (.annex_kind_max("density") > 0)
  .annex_bits <- c(.annex_bits, sprintf("the marginal strips exclude individual predictions above the plotted range, at most %.1f%% in any panel", .annex_kind_max("density")))
if (length(.annex_bits) > 0L) {
  ANNEX_FIG_NOTE <- paste0(ANNEX_FIG_NOTE, " For display, ",
                           paste(.annex_bits, collapse = "; "), ".")
}
cat("\n---- ANNEX_FIG_NOTE (paste into the manuscript caption) ----\n")
cat(strwrap(ANNEX_FIG_NOTE, width = 100), sep = "\n")
cat("\n")

# ---------------------------------------------------------------------------
# CHUNK 6 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-save
# ============================================================================
# ANNEX CALIBRATION FIGURE, STEP 6/6: export to disk.
# Files go to the notebook's usual figure folder (`figs_out`, cons/_figs),
# falling back to <getwd()>/_figs when `figs_out` is not in the kernel.
# Sizes and formats follow the Epidemiology (LWW) production guidance:
#   - the panel is prepared at double-column width (17.4 cm), since a 4 x 2
#     panel grid is not legible at the 8.5 cm single-column width;
#   - TIFF with LZW compression at 600 dpi (combination line art + shaded
#     density strips) is the production deliverable;
#   - a 300 dpi PNG is exported for drafts and slide decks;
#   - a vector PDF (grDevices::cairo_pdf, embeds Times New Roman) is exported
#     because vector art rescales without quality loss during typesetting.
# ragg devices are used for the raster formats: they render text/fonts on
# Windows far more reliably than the base grDevices tiff/png devices.
# Requires in kernel: annex_cal_fig (from `annex-calfig-assemble`).
# Place AFTER `annex-calfig-assemble`. Adds files; replaces nothing in memory.
# ============================================================================
stopifnot(
  "figs_out must exist in the kernel so the annex figure lands in the project's sanctioned figure folder: run `define-project-root-and-paths`, or `annex-calfig-ensure`, which derives it from the validated project root" =
    exists("figs_out", inherits = TRUE)
)
.annex_figs_out <- figs_out
dir.create(.annex_figs_out, recursive = TRUE, showWarnings = FALSE)
.annex_fig_w_cm <- 17.4
.annex_fig_h_cm <- 9.8
ggplot2::ggsave(
  filename = file.path(.annex_figs_out, "annex_calibration_panel.tiff"),
  plot = annex_cal_fig, device = ragg::agg_tiff,
  width = .annex_fig_w_cm, height = .annex_fig_h_cm, units = "cm",
  dpi = 600, compression = "lzw"
)
ggplot2::ggsave(
  filename = file.path(.annex_figs_out, "annex_calibration_panel.png"),
  plot = annex_cal_fig, device = ragg::agg_png,
  width = .annex_fig_w_cm, height = .annex_fig_h_cm, units = "cm",
  dpi = 300
)
ggplot2::ggsave(
  filename = file.path(.annex_figs_out, "annex_calibration_panel.pdf"),
  plot = annex_cal_fig, device = grDevices::cairo_pdf,
  width = .annex_fig_w_cm, height = .annex_fig_h_cm, units = "cm"
)
message("annex-calfig: figure written to ", normalizePath(.annex_figs_out, winslash = "/"))

# ---------------------------------------------------------------------------
# CHUNK 7 (REFINEMENT PASS, optional) -- paste as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-calfig-refine
#| fig-width: 6.85
#| fig-height: 3.85
# ============================================================================
# ANNEX CALIBRATION FIGURE, REFINEMENT PASS. Self-contained: it rebuilds the
# figure from CACHED DATA, so none of the expensive cells has to run again.
# What it caches, and why not the plot object: `annex_cal_bins` and
# `annex_cal_preds` are two small data frames, whereas a saved ggplot or
# patchwork object carries its whole environment and cannot have its panel
# spacing or its overlays retuned after the fact. Re-assembling from the data
# takes seconds and is what makes every knob below actually settable.
# The cache lives under data/ and never under cons/, per the project rule on
# .rds files and Drive sync.
#
# The four refinements, point by point, each with its own knob:
#   (1) less horizontal space between panels
#   (2) A and B bolder and closer to the panel
#   (3) larger legend
#   (4) no vertical dead space between the legend and the panel
#
# Requires in kernel: either `annex_cal_bins` + `annex_cal_preds`, or the
# cache file written by a previous run; plus the constants and helpers from
# `annex-calfig-theme` (.annex_font, .annex_aes, .annex_col, .annex_fill,
# .annex_rows_spec, ANNEX_TIMES, ANNEX_POINT_SIZE, ANNEX_POINT_STROKE,
# .annex_theme_dens, ANNEX_PER_HORIZON_LIMITS, ANNEX_MORT_DODGE_FRAC) and
# .annex_limits_for/.annex_dodge_for from `annex-calfig-builder`.
# Place at the END of the annex, after `annex-calfig-save`.
# ============================================================================
# ---- (1) horizontal space between panels -----------------------------------
# Points of inset on the left of every panel except the first. This is the
# gutter that stops one panel's last x label butting against the next panel's
# first one. Lower it to tighten; if labels start touching, keep it low and
# switch the leading-zero knob on instead, which buys width without a gutter.
ANNEX_R1_PANEL_GAP_PT <- 3
ANNEX_R1_DROP_LEADING_ZERO <- TRUE
# ---- (2) A and B: bolder, closer to the panel -------------------------------
# Size in ggplot text units (about pt / 2.85). `BOTTOM` is the lower edge of
# the overlay box in row-relative units: higher means closer to the panel top.
# The overlay box must be TALLER than the letter or the inset viewport clips
# it: at size 5.0 with bottom 0.965 the box was about 31 px for a 59 px glyph,
# which is why the B rendered as a stub. Box height and letter size must be
# raised together, and ANNEX_R2_TOP_MARGIN_PT supplies the headroom that keeps
# the letter off the top y tick label.
ANNEX_R2_TAG_SIZE <- 4.3
ANNEX_R2_TAG_BOTTOM <- 0.90
ANNEX_R2_TAG_RIGHT <- 0.040
ANNEX_R2_TOP_MARGIN_PT <- 14
# ---- (3) and (4) legend ------------------------------------------------------
# Text size in pt, and the vertical anchor inside the first mortality panel.
# KEY_H is the height of each legend row: shrinking it is what removes the
# vertical dead space between the two entries and the panel.
ANNEX_R34_LEGEND_SIZE <- 9.5
ANNEX_R34_LEGEND_Y <- 0.99
ANNEX_R34_LEGEND_KEY_H <- 9
ANNEX_R34_LEGEND_KEY_W <- 15
# ---- Data: kernel first, then cache ------------------------------------------
.annex_cache_dir <- file.path(if (exists("project_root", inherits = TRUE)) project_root else getwd(),
                              "data", "20241015_out", "pred23")
.annex_cache_file <- file.path(.annex_cache_dir, "annex_calfig_data.rds")
if (all(vapply(c("annex_cal_bins", "annex_cal_preds"), exists, logical(1L), inherits = TRUE))) {
  dir.create(.annex_cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(bins = annex_cal_bins, preds = annex_cal_preds,
               ci_label = if (exists("ANNEX_CI_LABEL", inherits = TRUE)) ANNEX_CI_LABEL else NA_character_),
          .annex_cache_file)
  message("annex-calfig refine: cached the plotting data to ", .annex_cache_file)
} else {
  stopifnot("no plotting data in the kernel and no cache file; run `annex-calfig-data` once" =
    file.exists(.annex_cache_file))
  .annex_cached <- readRDS(.annex_cache_file)
  annex_cal_bins <- .annex_cached$bins
  annex_cal_preds <- .annex_cached$preds
  if (!exists("ANNEX_CI_LABEL", inherits = TRUE)) ANNEX_CI_LABEL <- .annex_cached$ci_label
  message("annex-calfig refine: restored the plotting data from the cache; the expensive cells were not needed.")
}
stopifnot("the theme and builder cells must have been run at least once" =
  all(vapply(c(".annex_rows_spec", ".annex_limits_for", ".annex_dodge_for", ".annex_theme_dens"),
             exists, logical(1L), inherits = TRUE)))
# ---- Panel builder carrying the four knobs -----------------------------------
.annex_num_fmt <- function(acc) {
  # Dropping the leading zero ("0.05" -> ".05") is a standard journal economy
  # and shortens every x label by about a quarter, which is what lets the
  # gutter in knob (1) come down without labels touching.
  function(b) {
    out <- scales::number(b, accuracy = acc)
    if (isTRUE(ANNEX_R1_DROP_LEADING_ZERO)) out <- sub("^0\\.", ".", out)
    out
  }
}
.annex_panel_refined <- function(row_spec, horizon, show_y, show_legend, show_title, first) {
  mcs <- row_spec$models
  .lm <- .annex_limits_for(row_spec, horizon)
  lim <- .lm$lim
  bins <- dplyr::filter(annex_cal_bins, model %in% mcs, time_horizon == horizon)
  preds <- dplyr::filter(annex_cal_preds, model %in% mcs, time_horizon == horizon)
  bins$model <- factor(bins$model, levels = mcs)
  preds$model <- factor(preds$model, levels = mcs)
  keep <- bins$bin_mean_pred >= lim[1L] & bins$bin_mean_pred <= lim[2L] &
    bins$bin_obs_prop >= lim[1L] & bins$bin_obs_prop <= lim[2L]
  bins <- bins[keep, , drop = FALSE]
  bins$x_plot <- bins$bin_mean_pred + vapply(as.character(bins$model), .annex_dodge_for, numeric(1L), lim = lim)
  bins$x_plot <- pmin(pmax(bins$x_plot, lim[1L]), lim[2L])
  bins$lo_d <- pmax(bins$bin_obs_lower, lim[1L])
  bins$hi_d <- pmin(bins$bin_obs_upper, lim[2L])
  bins <- bins[order(bins$model, bins$bin), , drop = FALSE]
  bins$seg <- unlist(lapply(split(bins$bin, bins$model), function(z)
    cumsum(c(0L, as.integer(diff(z) != 1L)))), use.names = FALSE)
  bins$grp <- paste(bins$model, bins$seg, sep = "_")
  preds_in <- dplyr::filter(preds, pred_risk >= lim[1L], pred_risk <= lim[2L])
  shp <- vapply(mcs, function(m) .annex_aes[[m]]$shape, numeric(1L))
  lty <- vapply(mcs, function(m) .annex_aes[[m]]$linetype, character(1L))
  cols <- vapply(mcs, .annex_col, character(1L))
  fills <- vapply(mcs, .annex_fill, character(1L))
  labs <- vapply(mcs, function(m) .annex_aes[[m]]$label, character(1L))
  .fmt <- .annex_num_fmt(.lm$acc)
  .ylab_fmt <- function(b) {
    out <- .fmt(b)
    out[is.finite(b) & abs(b - lim[1L]) < 1e-12] <- ""
    out
  }
  main <- ggplot2::ggplot(bins, ggplot2::aes(x = x_plot, y = bin_obs_prop)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dotted", linewidth = 0.3, color = "gray70") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo_d, ymax = hi_d, color = model),
                           width = 0, linewidth = 0.3, alpha = 0.95, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(color = model, linetype = model, group = grp), linewidth = 0.55) +
    ggplot2::geom_point(ggplot2::aes(color = model, shape = model),
                        size = ANNEX_POINT_SIZE, stroke = ANNEX_POINT_STROKE, fill = "white") +
    ggplot2::scale_color_manual(values = cols, labels = labs, name = NULL) +
    ggplot2::scale_shape_manual(values = shp, labels = labs, name = NULL) +
    ggplot2::scale_linetype_manual(values = lty, labels = labs, name = NULL) +
    ggplot2::scale_x_continuous(limits = lim, breaks = .lm$breaks, labels = .fmt,
                                expand = ggplot2::expansion(mult = c(0.012, 0.012))) +
    ggplot2::scale_y_continuous(limits = lim, breaks = .lm$breaks, labels = .ylab_fmt,
                                expand = ggplot2::expansion(mult = c(0.012, 0.012))) +
    ggplot2::labs(subtitle = if (show_title) paste0(horizon, " months") else NULL) +
    .annex_theme_main() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = ANNEX_R2_TOP_MARGIN_PT, r = 1, b = 0,
                                                 l = if (first) 0 else ANNEX_R1_PANEL_GAP_PT))
  if (!show_y) {
    main <- main + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                  axis.ticks.y = ggplot2::element_blank())
  }
  if (show_legend && length(mcs) > 1L) {
    main <- main + ggplot2::theme(
      legend.position = c(0.02, ANNEX_R34_LEGEND_Y), legend.justification = c(0, 1),
      legend.background = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(ANNEX_R34_LEGEND_KEY_H, "pt"),
      legend.key.width = ggplot2::unit(ANNEX_R34_LEGEND_KEY_W, "pt"),
      legend.text = ggplot2::element_text(size = ANNEX_R34_LEGEND_SIZE, family = .annex_font),
      legend.margin = ggplot2::margin(0, 0, 0, 0), legend.spacing.y = ggplot2::unit(0, "pt"))
  }
  .dens <- function(flip) {
    g <- ggplot2::ggplot(preds_in, ggplot2::aes(x = pred_risk, group = model, fill = model)) +
      ggplot2::geom_density(color = NA, alpha = 0.55, adjust = 1, trim = TRUE) +
      ggplot2::scale_fill_manual(values = fills) +
      ggplot2::scale_x_continuous(limits = lim, expand = ggplot2::expansion(mult = c(0.012, 0.012))) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      .annex_theme_dens()
    if (flip) g + ggplot2::coord_flip() else g
  }
  design <- c(patchwork::area(1, 1, 12, 12), patchwork::area(1, 13, 12, 13), patchwork::area(13, 1, 13, 12))
  main + .dens(TRUE) + .dens(FALSE) + patchwork::plot_layout(design = design)
}
.annex_row_refined <- function(row_spec, show_title) {
  .per_h <- isTRUE(ANNEX_PER_HORIZON_LIMITS)
  patchwork::wrap_plots(lapply(seq_along(ANNEX_TIMES), function(i)
    .annex_panel_refined(row_spec, ANNEX_TIMES[i],
                         show_y = (.per_h || i == 1L), show_legend = (i == 1L),
                         show_title = show_title, first = (i == 1L))), ncol = length(ANNEX_TIMES))
}
# ---- Assemble ----------------------------------------------------------------
.annex_tag_grob2 <- function(txt) {
  # A ggplot rather than a bare textGrob: a textGrob inset carries an opaque
  # background that bites into the top y tick label.
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = txt, fontface = "bold",
                      size = ANNEX_R2_TAG_SIZE, family = .annex_font, color = "black") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(0, 0, 0, 0))
}
.annex_tag_row2 <- function(row, txt) {
  # Overlaid at the outer level on purpose: inside a patchwork::area() layout
  # the gtable cell clips anything drawn outside the panel, even with
  # coord_cartesian(clip = "off"), so an in-panel letter silently vanishes.
  patchwork::wrap_elements(full = row, clip = FALSE) +
    patchwork::inset_element(.annex_tag_grob2(txt),
                             left = 0, bottom = ANNEX_R2_TAG_BOTTOM,
                             right = ANNEX_R2_TAG_RIGHT, top = 1.0,
                             align_to = "full", clip = FALSE, on_top = TRUE)
}
annex_row_A <- .annex_row_refined(.annex_rows_spec[[1L]], show_title = TRUE)
annex_row_B <- .annex_row_refined(.annex_rows_spec[[2L]], show_title = FALSE)
.annex_rows <- patchwork::wrap_plots(
  .annex_tag_row2(annex_row_A, "A"),
  .annex_tag_row2(annex_row_B, "B"),
  ncol = 1L
) +
  patchwork::plot_annotation(
    caption = "Predicted probability",
    theme = ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 9.5, face = "bold", hjust = 0.5,
                                           family = .annex_font,
                                           margin = ggplot2::margin(t = 1)),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1)
    )
  )
.annex_y_title <- patchwork::wrap_elements(
  full = grid::textGrob("Observed probability", rot = 90,
                        gp = grid::gpar(fontsize = 9.5, fontface = "bold", fontfamily = .annex_font)),
  clip = FALSE
)
annex_cal_fig <- patchwork::wrap_plots(.annex_y_title,
                                       patchwork::wrap_elements(full = .annex_rows, clip = FALSE),
                                       ncol = 2L, widths = c(0.022, 1))
print(annex_cal_fig)
# ---- Export ------------------------------------------------------------------
stopifnot("figs_out must exist" = exists("figs_out", inherits = TRUE))
dir.create(figs_out, recursive = TRUE, showWarnings = FALSE)
for (.ext in c("tiff", "png", "pdf")) {
  .dev <- switch(.ext, tiff = ragg::agg_tiff, png = ragg::agg_png, pdf = grDevices::cairo_pdf)
  .args <- list(filename = file.path(figs_out, paste0("annex_calibration_panel.", .ext)),
                plot = annex_cal_fig, device = .dev, width = 17.4, height = 9.8, units = "cm")
  if (identical(.ext, "tiff")) .args <- c(.args, list(dpi = 600, compression = "lzw"))
  if (identical(.ext, "png")) .args <- c(.args, list(dpi = 300))
  do.call(ggplot2::ggsave, .args)
}
message("annex-calfig refine: wrote the refined figure to ", figs_out)
