# Standalone paired classical IBS analysis for prediction225.
# This script does not modify any notebook.
# Run from the project root with: source("cons/_alt_scripts/pred225_annex_ibs_pairwise_B500.R")
# It uses B = 500 for every bootstrap and stops at the first failed control.
.t0_script <- proc.time()
# Standalone source of p225-annex-ibs-classical-dev
# ===========================================================================
# ANNEX 1 of 4. CLASSICAL (Graf 1999) IBS, OUT OF FOLD ON DEVELOPMENT.
# Run order: this chunk, then p225-annex-ibs-null-ipa, then
# p225-annex-ibs-ci-sensitivity-resampled-g, then p225-annex-ibs-classical-dev-save.
# Runs standalone or inside prediction225. The development cohort is
# py_corrected_datasets_boot, the 80 percent TRAINING split (n = 70,521); the
# frozen 20 percent test set is never touched. Same subjects, same five
# imputations and the SAME five-fold partition that produced the metrics above.
# WHAT CHANGES VERSUS THE IBS REPORTED ABOVE, PRECISELY.
# Against the pooled out-of-fold engine of the cell deltaCIBS-cv-engine
# (.ibs_prep, ibs_fast, deltaIBS_boot_x): only the integral and the grid change.
# That engine already pools out-of-fold predictions across folds, averages the
# survival matrices across imputations, and estimates the censoring curve on the
# pooled evaluation cohort, which is exactly what is done here.
# Against the per-fold engine (evaluate_dual_cox_python_style calling
# ibs_ipcw_train, the values harvested by the cell extract-metrics): MORE than the
# integral changes. That engine estimates the censoring curve on each TRAINING
# fold and averages the per-fold, per-imputation IBS values, whereas here the
# censoring curve is estimated once on the pooled cohort and the survival matrices
# are averaged across imputations before scoring. The gap against those numbers
# therefore mixes the integral with the censoring estimation and the pooling, and
# must not be attributed to the integral alone.
# In every case these remain different estimands: do not place them in one column
# and do not subtract them.
# The cached out-of-fold predictions store surv_val_matrix on five nodes only,
# which IS the coarse quadrature the classical integral removes, so the cache
# cannot be scored directly. It is used for the FOLD PARTITION: every block
# carries imp_idx and original_val_idx, so the exact assignment is recovered and
# only the prediction grid changes.
# PARALLEL: the folds of each imputation run over future.apply workers. Set
# ANNEXD_WORKERS <- 1L to force serial.
# CACHE: scored objects are reloaded from ANNEXD_CACHE when present. Set
# ANNEXD_REFIT <- TRUE to rebuild.
# MEMORY: with ANNEXD_KEEP_S = TRUE the three out-of-fold survival matrices stay
# in the session, about 136 MB each. That is what lets the G sensitivity chunk
# skip an hour of refitting. Set it to FALSE only if the session is short on RAM.
# ===========================================================================
suppressPackageStartupMessages(library(survival))
stopifnot(requireNamespace("pec", quietly = TRUE))
.t0 <- proc.time()
# ---- 1. Paths and engines ------------------------------------------------------
.annex_root <- if (exists("project_root", inherits = TRUE) &&
                   dir.exists(file.path(project_root, "cons", "_alt_scripts"))) project_root else
  { .cw <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    if (grepl("/cons$", .cw)) dirname(.cw) else .cw }
stopifnot(dir.exists(file.path(.annex_root, "cons", "_alt_scripts")))
.annex_data_out <- file.path(.annex_root, "data", "20241015_out")
.annex_out_dir  <- file.path(.annex_root, "cons", "_out")
# ibs_classical_holdout.R resolves the competing-risk engine lazily through
# project_root, falling back to getwd(). Outside the notebook getwd() is the cons
# folder, which makes it look for cons/cons/_alt_scripts and fail. Setting
# project_root and loading that engine up front removes the lazy path entirely.
project_root <- .annex_root
source(file.path(.annex_root, "cons", "_alt_scripts", "ibs_window_bootstrap_holdout.R"))
source(file.path(.annex_root, "cons", "_alt_scripts", "ibs_classical_holdout.R"))
stopifnot(exists(".ibsb_contrib_readmit_aj"), exists(".ibsb_contrib_readmit_ipcw"),
          exists(".ibsb_marg_km"), exists(".ibsb_marg_aj_surv"), exists(".ibsb_cr"),
          exists(".ibc_trap_weights"), exists("ibs_classical_readmit"),
          exists("ibs_classical_point"), exists("ibs_classical_null"),
          exists("ibs_classical_skill_boot"), exists("ibs_classical_boot_ci"))
options(ibsclassical.keep_replicates = TRUE)
ibs_classical_replicates_reset()
# ---- 2. Settings ---------------------------------------------------------------
ANNEXD_HORIZONS <- c(3, 6, 12, 36, 60)
ANNEXD_STEP     <- 0.25
ANNEXD_GRID     <- ibs_classical_grid(max_horizon = 60, step = ANNEXD_STEP, horizons = ANNEXD_HORIZONS)
ANNEXD_B        <- 500L
ANNEXD_SEED     <- 2125L
ANNEXD_STRATA   <- 10L
ANNEXD_WORKERS  <- 5L
ANNEXD_REFIT    <- FALSE
ANNEXD_KEEP_S   <- TRUE
ANNEXD_CACHE    <- file.path(.annex_data_out, "pred225_annex_ibs_classical_oof.rds")
# ---- 3. Development data, formulas, fold partition ------------------------------
if (!exists("py_corrected_datasets_boot", inherits = TRUE)) {
  .rd <- file.path(.annex_data_out, "pred22_ndp_2026_07_13.Rdata")
  stopifnot(file.exists(.rd))
  cat("py_corrected_datasets_boot is not in this session; loading", basename(.rd),
      "- 5.6 GB, takes a few minutes.\n"); flush.console()
  .e <- new.env(parent = emptyenv()); load(.rd, envir = .e)
  stopifnot(exists("py_corrected_datasets_boot", envir = .e, inherits = FALSE))
  py_corrected_datasets_boot <- get("py_corrected_datasets_boot", envir = .e)
  rm(.e); invisible(gc())
}
stopifnot(is.list(py_corrected_datasets_boot), length(py_corrected_datasets_boot) == 5L)
.dev <- lapply(py_corrected_datasets_boot, as.data.frame)
.N_dev <- nrow(.dev[[1]])
stopifnot(all(vapply(.dev, nrow, integer(1)) == .N_dev))
if (!exists("formula_shap_readmit_clean_updated", inherits = TRUE) ||
    !exists("formula_death_updated2", inherits = TRUE)) {
  .fms <- readRDS(file.path(.annex_data_out, "_val_inputs", "formulas.rds"))
  if (!exists("formula_shap_readmit_clean_updated", inherits = TRUE))
    formula_shap_readmit_clean_updated <- .fms$formula_shap_readmit_clean_updated
  if (!exists("formula_death_updated2", inherits = TRUE))
    formula_death_updated2 <- .fms$formula_death_updated2
  rm(.fms)
}
# formula_shap_death_rule2 is NOT persisted by save-results (formula_names_all
# lists formula_shap_death, the retired 12-covariate version), so it is rebuilt
# literally and pinned by the term-count checks below.
if (!exists("formula_shap_death_rule2", inherits = TRUE))
  formula_shap_death_rule2 <- stats::as.formula(paste(
    "Surv(death_time_from_disch_m, death_event) ~",
    paste(c("adm_age_rec3", "primary_sub_mod_cocaine_paste", "primary_sub_mod_cocaine_powder",
            "primary_sub_mod_alcohol", "primary_sub_mod_others", "prim_sub_freq_rec_2_2_6_days_wk",
            "prim_sub_freq_rec_3_daily", "occupation_condition_corr24_unemployed",
            "occupation_condition_corr24_inactive", "eva_ocupacion_logro_intermedio",
            "eva_ocupacion_logro_minimo", "cohabitation_family_of_origin",
            "cohabitation_with_couple_children", "cohabitation_others",
            "strata(plan_type_strata)", "strata(any_phys_dx)"), collapse = " + ")),
    env = globalenv())
# Detach whatever environment the formulas carry. A formula restored from the
# pred22 workspace can hold a reference to that session's environment, which the
# parallel workers would otherwise have to receive. Every symbol these formulas
# need comes from `data`, so globalenv is correct and much lighter.
F_READMIT <- formula_shap_readmit_clean_updated; environment(F_READMIT) <- globalenv()
F_DEATH_FULL <- formula_death_updated2;          environment(F_DEATH_FULL) <- globalenv()
F_DEATH_SHAP <- formula_shap_death_rule2;        environment(F_DEATH_SHAP) <- globalenv()
.nt <- function(f) length(attr(stats::terms(f), "term.labels"))
stopifnot(.nt(F_READMIT) == 28L, .nt(F_DEATH_FULL) == 65L, .nt(F_DEATH_SHAP) == 16L)
if (!exists("dual_fits", inherits = TRUE)) {
  .cache_fits <- file.path(.annex_data_out, "pred22_ndp_2026_07_13__dualfits.rds")
  stopifnot(file.exists(.cache_fits))
  cat("Loading fold partition from", basename(.cache_fits), "\n"); flush.console()
  dual_fits <- readRDS(.cache_fits)$dual_fits
}
stopifnot(all(c("shap", "updated2") %in% names(dual_fits)))
.annex_fold_map <- function(fit) {
  rp <- fit$raw_predictions
  stopifnot(!is.null(rp), length(rp) > 0L)
  imps <- sort(unique(vapply(rp, function(b) as.integer(b$imp_idx), integer(1))))
  lapply(imps, function(im)
    lapply(Filter(function(b) as.integer(b$imp_idx) == im, rp),
           function(b) as.integer(b$original_val_idx)))
}
# One partition for all three arms, so the paired mortality difference is really
# paired and the fold assignment cannot leak into the difference.
FOLDS <- .annex_fold_map(dual_fits$shap)
for (fm in FOLDS) stopifnot(identical(sort(unlist(fm)), seq_len(.N_dev)))
cat(sprintf("Development cohort %d x %d imputations | folds %d, exact cover\n",
            .N_dev, length(.dev), length(FOLDS[[1]])))
.same_folds <- identical(lapply(FOLDS, function(f) lapply(f, sort)),
                         lapply(.annex_fold_map(dual_fits$updated2), function(f) lapply(f, sort)))
cat("Fold partition of dual_fits$updated2 identical to dual_fits$shap:", .same_folds, "\n")
.fold_sig <- paste(vapply(FOLDS, function(f)
  paste(vapply(f, function(v) sprintf("%d:%d:%d", length(v), v[1], v[length(v)]), ""),
        collapse = "|"), ""), collapse = "//")
# ---- 4. Outcomes and the row-alignment check ------------------------------------
.to01 <- function(x) if (is.logical(x)) as.integer(x) else as.integer(as.numeric(x) > 0)
.rt <- as.numeric(.dev[[1]]$readmit_time_from_disch_m); .re <- .to01(.dev[[1]]$readmit_event)
.dt <- as.numeric(.dev[[1]]$death_time_from_disch_m);   .de <- .to01(.dev[[1]]$death_event)
# Each cached fold block carries y_val, the outcome pair of its own validation
# subjects, so comparing it against the loaded outcomes at original_val_idx proves
# the rows line up. A wrong or reordered copy of the input would otherwise give a
# plausible but meaningless IBS.
.n_checked <- 0L
for (b in Filter(function(x) as.integer(x$imp_idx) == 1L, dual_fits$shap$raw_predictions)) {
  .o <- b$death; if (is.null(.o) || is.null(.o$y_val)) next
  .yv <- as.matrix(.o$y_val); .ix <- as.integer(b$original_val_idx)
  stopifnot(nrow(.yv) == length(.ix),
            max(abs(as.numeric(.yv[, 1]) - .dt[.ix])) < 1e-8,
            all(as.integer(.yv[, 2]) == .de[.ix]))
  .n_checked <- .n_checked + length(.ix)
}
cat(sprintf("Row alignment verified against the cached CV outcomes on %d subjects\n", .n_checked))
stopifnot(.n_checked == .N_dev)
.mk_death_pred <- function(S) list(S = S, grid = ANNEXD_GRID, n_imp = length(.dev),
  y_test = data.frame(time = .dt, event = .de), y_train = data.frame(time = .dt, event = .de))
.mk_readmit_pred <- function(S) list(S = S, grid = ANNEXD_GRID, n_imp = length(.dev),
  rt_val = .rt, re_val = .re, dt_val = .dt, de_val = .de,
  rt_tr = .rt, re_tr = .re, dt_tr = .dt, de_tr = .de)
# ---- 5. Reuse the cache, or rebuild ---------------------------------------------
.cache_ok <- FALSE
if (!ANNEXD_REFIT && file.exists(ANNEXD_CACHE)) {
  .cc <- readRDS(ANNEXD_CACHE)
  .cache_ok <- identical(.cc$meta$fold_signature, .fold_sig) &&
               identical(as.numeric(.cc$meta$grid), as.numeric(ANNEXD_GRID)) &&
               identical(as.numeric(.cc$meta$horizons), as.numeric(ANNEXD_HORIZONS))
  if (.cache_ok) {
    ibs_dev_readmit_cs <- .cc$points$readmit_cs; ibs_dev_readmit_aj <- .cc$points$readmit_aj
    ibs_dev_death_full <- .cc$points$death_full; ibs_dev_death_shap <- .cc$points$death_shap
    cat("Cache hit: scored objects reloaded, no Cox refitting.\n")
    cat("NOTE: the out-of-fold survival matrices are NOT in the cache (about 136 MB each),\n",
        "  so the G sensitivity chunk will have to refit them. Set ANNEXD_REFIT <- TRUE\n",
        "  and re-run this chunk if you want them in the session.\n", sep = "")
  } else cat("Cache present but grid or fold partition does not match; rebuilding.\n")
  rm(.cc)
}
if (!.cache_ok) {
  if (!requireNamespace("future.apply", quietly = TRUE))
    install.packages("future.apply", repos = "https://cran.dcc.uchile.cl/")
  suppressPackageStartupMessages(library(future.apply))
  options(future.globals.maxSize = 8 * 1024^3)
  .n_work <- max(1L, min(ANNEXD_WORKERS, future::availableCores() - 1L))
  if (.n_work > 1L) future::plan(future::multisession, workers = .n_work) else future::plan(future::sequential)
  # The plan is reset even if a fit fails, so a crash does not leave worker
  # processes alive holding a copy of the cohort.
  on.exit(try(future::plan(future::sequential), silent = TRUE), add = TRUE)
  cat(sprintf("Refitting out of fold: %d grid nodes, %d workers\n", length(ANNEXD_GRID), .n_work))
  # One accumulator per arm. Each subject is in exactly one validation fold per
  # imputation, so adding fold blocks sums over imputations directly and only one
  # n x grid matrix is ever held. The stratum count is asserted on every fit: a
  # strata() term demoted to a covariate is the 2026-07-12 failure mode.
  .annex_oof_surv <- function(formula, dev_list, fold_map, grid,
                              expected_strata = ANNEXD_STRATA, label = "") {
    N <- nrow(dev_list[[1]]); K <- length(grid); S_acc <- matrix(0, N, K)
    for (im in seq_along(fold_map)) {
      d <- dev_list[[im]]; folds <- fold_map[[im]]
      res <- future.apply::future_lapply(folds, function(vi) {
        fit <- survival::coxph(formula, data = d[-vi, , drop = FALSE], ties = "efron",
                               x = TRUE, y = TRUE, model = TRUE)
        bh <- survival::basehaz(fit, centered = FALSE)
        ns <- if (is.null(bh$strata)) 1L else nlevels(droplevels(factor(bh$strata)))
        list(n_strata = ns, S = matrix(as.numeric(
          pec::predictSurvProb(fit, newdata = d[vi, , drop = FALSE], times = grid)),
          length(vi), K))
      }, future.seed = TRUE, future.packages = c("survival", "pec"))
      for (j in seq_along(folds)) {
        if (!is.null(expected_strata) && res[[j]]$n_strata != expected_strata)
          stop(sprintf("%s, imputation %d, fold %d: %d baseline strata, expected %d. A strata() term may have been fitted as a covariate.",
                       label, im, j, res[[j]]$n_strata, expected_strata), call. = FALSE)
        S_acc[folds[[j]], ] <- S_acc[folds[[j]], ] + res[[j]]$S
      }
      rm(res); cat("."); flush.console()
    }
    S <- S_acc / length(fold_map)
    if (anyNA(S)) stop(sprintf("%s: out-of-fold survival contains NA; predictSurvProb failed on some fold.", label), call. = FALSE)
    stopifnot(min(S) >= 0, max(S) <= 1)
    cat(sprintf(" %s done\n", label))
    S
  }
  # The matrices are KEPT under stable names when ANNEXD_KEEP_S is TRUE, because
  # the G sensitivity chunk needs the per-subject curves and cannot recover them
  # from the trapezoided contributions.
  S_oof_readmit <- .annex_oof_surv(F_READMIT, .dev, FOLDS, ANNEXD_GRID, label = "readmission")
  ibs_dev_readmit_cs <- ibs_classical_readmit(.mk_readmit_pred(S_oof_readmit), ANNEXD_HORIZONS, method = "ipcw")
  ibs_dev_readmit_aj <- ibs_classical_readmit(.mk_readmit_pred(S_oof_readmit), ANNEXD_HORIZONS, method = "aalen-johansen")
  S_oof_death_full <- .annex_oof_surv(F_DEATH_FULL, .dev, FOLDS, ANNEXD_GRID, label = "death-full")
  ibs_dev_death_full <- ibs_classical_point(.mk_death_pred(S_oof_death_full), horizons = ANNEXD_HORIZONS)
  S_oof_death_shap <- .annex_oof_surv(F_DEATH_SHAP, .dev, FOLDS, ANNEXD_GRID, label = "death-shap")
  ibs_dev_death_shap <- ibs_classical_point(.mk_death_pred(S_oof_death_shap), horizons = ANNEXD_HORIZONS)
  if (!ANNEXD_KEEP_S) { rm(S_oof_readmit, S_oof_death_full, S_oof_death_shap) }
  invisible(gc())
  future::plan(future::sequential)
}
# ---- 6. Label the mortality arms and check the sanity relation -------------------
# ibs_classical_boot_ci keys its stored replicates on pt$method, defaulting to
# "death" when absent, so the two mortality arms are tagged apart to stop the
# second from overwriting the first.
ibs_dev_death_full$method <- "death-full"
ibs_dev_death_shap$method <- "death-shap"
cat(sprintf("tau_ipcw (mortality) = %.1f months; horizons beyond it return NA\n",
            ibs_dev_death_full$tau_ipcw))
# Taking deaths out of the numerator should lower the score, so the competing-risk
# arm is expected at or below the cause-specific one. Reported, not enforced:
# halting an hour of fitting over a sanity expectation would be worse than looking
# at the number.
.inv <- which(ibs_dev_readmit_aj$ibs$ibs > ibs_dev_readmit_cs$ibs$ibs + 1e-12)
if (length(.inv))
  warning(sprintf("Readmission: the competing-risk arm exceeds the cause-specific arm at horizon(s) %s. Inspect the observed side before reporting.",
                  paste(ANNEXD_HORIZONS[.inv], collapse = ", ")), call. = FALSE) else
  cat("Sanity check passed: competing-risk arm at or below cause-specific at every horizon\n")
# ---- 7. Bootstrap and tables -----------------------------------------------------
# Subjects are resampled, not folds. G is fixed by the pooled cohort, so each
# subject's trapezoided contribution is constant and one replicate is a single
# matrix-vector product. The mortality difference is paired on the same resample.
# ARM KEYS ARE THE JOIN KEYS of the three chunks that follow. They must stay
# exactly these four strings.
.dev_row <- function(pt, arm, estimand) {
  b <- ibs_classical_boot_ci(pt, B = ANNEXD_B, seed = ANNEXD_SEED, verbose = FALSE)
  data.frame(arm = arm, estimand = estimand, horizon_months = b$horizon_months,
             ibs_oof = b$ibs, ibs_lo95 = b$lo, ibs_hi95 = b$hi,
             stringsAsFactors = FALSE, row.names = NULL)
}
ibs_dev_table <- rbind(
  .dev_row(ibs_dev_readmit_cs, "readmission-cs", "death as censoring (cause-specific)"),
  .dev_row(ibs_dev_readmit_aj, "readmission-aj", "death as definitive non-event (competing risk)"),
  .dev_row(ibs_dev_death_full, "death-full",     "all-cause mortality"),
  .dev_row(ibs_dev_death_shap, "death-shap",     "all-cause mortality"))
ibs_dev_delta_death <- ibs_classical_delta_boot(
  ibs_dev_death_full, ibs_dev_death_shap, B = ANNEXD_B, seed = ANNEXD_SEED,
  label_A = "death-full", label_Z = "death-shap")
if (!dir.exists(.annex_out_dir)) dir.create(.annex_out_dir, recursive = TRUE)
.legacy_ibs_csv <- file.path(.annex_out_dir, "pred225_annex_ibs_classical_oof_development.csv")
.legacy_delta_csv <- file.path(.annex_out_dir, "pred225_annex_ibs_classical_oof_delta_death.csv")
if (!file.exists(.legacy_ibs_csv)) utils::write.csv(ibs_dev_table, .legacy_ibs_csv, row.names = FALSE) else cat("Legacy IBS CSV exists and was not overwritten:", .legacy_ibs_csv, "\n")
if (!file.exists(.legacy_delta_csv)) utils::write.csv(ibs_dev_delta_death, .legacy_delta_csv, row.names = FALSE) else cat("Legacy delta-IBS CSV exists and was not overwritten:", .legacy_delta_csv, "\n")
.elapsed <- proc.time() - .t0
cat(sprintf("\n[chunk: %s] %.1f sec (%.1f min)\n", "p225-annex-ibs-classical-dev",
            .elapsed["elapsed"], .elapsed["elapsed"]/60))
cat(sprintf("Out-of-fold survival matrices kept in session: %s\n",
            all(vapply(c("S_oof_readmit", "S_oof_death_full", "S_oof_death_shap"),
                       exists, logical(1)))))
print(knitr::kable(ibs_dev_table, "markdown", digits = 6, caption = sprintf(
  "Annex. Classical integrated Brier score (Graf), integrated from 0 to each horizon on a 0.25-month grid and divided by the horizon, computed out of fold on the development cohort (n = %s, the 80 percent training split; the frozen 20 percent test set is not scored here) with the same five-fold partition, the same five imputations and the same models as the analyses above, with subject-level bootstrap 95 percent intervals. Relative to the pooled out-of-fold engine of this notebook, only the integral and the evaluation grid differ. Relative to the per-fold integrated Brier score harvested earlier, the censoring curve is also estimated differently, on the pooled cohort rather than on each training fold, and survival is pooled across imputations before scoring rather than after, so the difference against those values cannot be attributed to the integral alone. The two readmission rows differ only in how death enters the observed side; the cause-specific row is the estimand model selection used. Abbreviations: IBS, integrated Brier score; OOF, out of fold.",
  format(.N_dev, big.mark = ","))))
print(knitr::kable(ibs_dev_delta_death[, c("horizon_months", "ibs_A", "ibs_Z",
                                           "delta_ibs", "delta_lo", "delta_hi")],
  "markdown", digits = 6,
  caption = "Annex. Paired difference in out-of-fold classical integrated Brier score between the two mortality specifications, bootstrapped on the same resampled subjects and the same fold partition. A negative difference favours the 63-covariate Full PH model. Judge the magnitude against the 0.01 promotion threshold of the model-selection rule, not against whether the interval excludes zero."))
# ---- 8. Comprehensive paired IBS sweep requested for model and risk arms ---
.t_pair <- proc.time()
ANNEXP_HORIZONS <- c(6, 12, 36, 60)
ANNEXP_B <- 500L
ANNEXP_SEED <- 2125L
ANNEXP_CACHE <- file.path(.annex_data_out, "pred225_annex_ibs_pairwise_all_arms_v2.rds")
ANNEXP_CSV <- file.path(.annex_out_dir, "pred225_annex_ibs_pairwise_all_arms_B500_v2.csv")
.annexp_required_formulas <- c("formula_readmit", "formula_shap_readmit_interact", "formula_readmit_updated", "formula_readmit_updated2", "formula_death_updated2", "formula_shap_death_rule2")
.annexp_missing_formulas <- .annexp_required_formulas[!vapply(.annexp_required_formulas, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
if (length(.annexp_missing_formulas) > 0L) {
  .annexp_rdata <- file.path(.annex_data_out, "pred22_ndp_2026_07_13.Rdata")
  if (!file.exists(.annexp_rdata)) stop("Required formulas are missing and the canonical RData is unavailable: ", .annexp_rdata, call. = FALSE)
  cat("Loading missing formulas from the canonical RData: ", paste(.annexp_missing_formulas, collapse = ", "), "\n", sep = "")
  .annexp_formula_env <- new.env(parent = emptyenv())
  load(.annexp_rdata, envir = .annexp_formula_env)
  .annexp_still_missing <- .annexp_missing_formulas[!vapply(.annexp_missing_formulas, exists, logical(1), envir = .annexp_formula_env, inherits = FALSE)]
  if (length(.annexp_still_missing) > 0L) stop("The canonical RData does not contain required formula(s): ", paste(.annexp_still_missing, collapse = ", "), call. = FALSE)
  for (.annexp_formula_name in .annexp_missing_formulas) assign(.annexp_formula_name, get(.annexp_formula_name, envir = .annexp_formula_env, inherits = FALSE), envir = .GlobalEnv)
  rm(.annexp_formula_env); invisible(gc())
}
.annexp_clean_formula <- function(formula_object) {
  cleaned <- stats::as.formula(gsub("strat\\(", "strata(", paste(deparse(formula_object), collapse = " ")), env = globalenv())
  environment(cleaned) <- globalenv()
  cleaned
}
.annexp_readmit_formulas <- list(base = .annexp_clean_formula(formula_readmit), interact = .annexp_clean_formula(formula_shap_readmit_interact), updated = .annexp_clean_formula(formula_readmit_updated), updated2 = .annexp_clean_formula(formula_readmit_updated2))
.annexp_death_formulas <- list(`full-death` = .annexp_clean_formula(formula_death_updated2), `shap-death` = .annexp_clean_formula(formula_shap_death_rule2))
.annexp_fit_formulas <- c(stats::setNames(.annexp_readmit_formulas, paste0("readmit__", names(.annexp_readmit_formulas))), stats::setNames(.annexp_death_formulas, c("death__full", "death__shap")))
# Derive the expected baseline-stratum count from each formula rather than imposing the ten-stratum updated2 design on simpler models.
.annexp_collect_strata_calls <- function(expression) {
  if (!is.call(expression)) return(list())
  call_name <- paste(deparse(expression[[1]]), collapse = "")
  if (call_name %in% c("strata", "survival::strata")) return(list(expression))
  unlist(lapply(as.list(expression)[-1L], .annexp_collect_strata_calls), recursive = FALSE)
}
.annexp_formula_strata_count <- function(formula_object, data) {
  strata_calls <- .annexp_collect_strata_calls(formula_object[[3]])
  if (length(strata_calls) == 0L) return(1L)
  strata_values <- list()
  for (strata_call in strata_calls) for (argument in as.list(strata_call)[-1L]) strata_values[[length(strata_values) + 1L]] <- eval(argument, envir = data, enclos = environment(formula_object))
  if (length(strata_values) == 0L || any(vapply(strata_values, length, integer(1)) != nrow(data))) stop("Could not evaluate the strata terms for formula: ", paste(deparse(formula_object), collapse = " "), call. = FALSE)
  combined_strata <- if (length(strata_values) == 1L) droplevels(factor(strata_values[[1]])) else do.call(interaction, c(lapply(strata_values, factor), list(drop = TRUE, lex.order = TRUE)))
  as.integer(nlevels(droplevels(combined_strata)))
}
.annexp_expected_strata <- vapply(.annexp_fit_formulas, .annexp_formula_strata_count, integer(1), data = .dev[[1]])
if (any(!is.finite(.annexp_expected_strata)) || any(.annexp_expected_strata < 1L)) stop("At least one formula has an invalid expected baseline-stratum count.", call. = FALSE)
cat("Expected baseline strata by dense-grid model: ", paste(sprintf("%s=%d", names(.annexp_expected_strata), .annexp_expected_strata), collapse = ", "), "\n", sep = "")
.annexp_formula_key <- function(formula_object) gsub("[[:space:]]+", "", paste(deparse(formula_object), collapse = " "))
.annexp_fold_normalize <- function(fold_map) lapply(fold_map, function(imputation_folds) lapply(imputation_folds, sort))
.annexp_dual_names <- c(base = "base", interact = "interact", updated = "updated", updated2 = "updated2")
if (!all(.annexp_dual_names %in% names(dual_fits))) stop("dual_fits is missing one or more requested model families: ", paste(setdiff(.annexp_dual_names, names(dual_fits)), collapse = ", "), call. = FALSE)
for (.annexp_model in names(.annexp_dual_names)) {
  .annexp_candidate_folds <- .annex_fold_map(dual_fits[[.annexp_dual_names[[.annexp_model]]]])
  if (!identical(.annexp_fold_normalize(.annexp_candidate_folds), .annexp_fold_normalize(FOLDS))) stop("The out-of-fold partition differs for model ", .annexp_model, "; a paired comparison on a common training partition is not available.", call. = FALSE)
}
.annexp_readmit_rows_checked <- 0L
for (.annexp_block in Filter(function(block) as.integer(block$imp_idx) == 1L, dual_fits$base$raw_predictions)) {
  .annexp_outcome <- .annexp_block$readmission; if (is.null(.annexp_outcome) || is.null(.annexp_outcome$y_val)) next
  .annexp_y <- as.matrix(.annexp_outcome$y_val); .annexp_indices <- as.integer(.annexp_block$original_val_idx)
  if (nrow(.annexp_y) != length(.annexp_indices) || max(abs(as.numeric(.annexp_y[, 1]) - .rt[.annexp_indices])) >= 1e-8 || !all(as.integer(.annexp_y[, 2]) == .re[.annexp_indices])) stop("Readmission row alignment differs from the cached OOF outcomes.", call. = FALSE)
  .annexp_readmit_rows_checked <- .annexp_readmit_rows_checked + length(.annexp_indices)
}
if (.annexp_readmit_rows_checked != .N_dev) stop("Readmission row alignment covered ", .annexp_readmit_rows_checked, " subjects; expected ", .N_dev, ".", call. = FALSE)
cat(sprintf("Readmission row alignment verified against cached OOF outcomes on %d subjects.\n", .annexp_readmit_rows_checked))
.annexp_fold_signature <- paste(vapply(FOLDS, function(imputation_folds) paste(vapply(imputation_folds, function(indices) paste(sort(indices), collapse = ","), character(1)), collapse = "|"), character(1)), collapse = "//")
.annexp_free_ram_mb <- function() {
  if (.Platform$OS.type != "windows") return(NA_real_)
  value <- tryCatch(suppressWarnings(as.numeric(system('powershell -NoProfile -Command "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"', intern = TRUE, ignore.stderr = TRUE))), error = function(e) NA_real_)
  if (length(value) != 1L || !is.finite(value)) NA_real_ else value / 1024
}
.annexp_choose_workers <- function(dev_list, n_jobs) {
  cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (length(cores) != 1L || !is.finite(cores) || cores < 1L) cores <- 4L
  cpu_cap <- max(1L, as.integer(cores) - 2L)
  free_mb <- .annexp_free_ram_mb()
  data_mb <- as.numeric(utils::object.size(dev_list[[1]])) / 1024^2
  per_worker_mb <- max(768, 1.75 * data_mb + 512)
  ram_cap <- if (is.finite(free_mb)) max(1L, floor(0.70 * free_mb / per_worker_mb)) else min(8L, cpu_cap)
  workers <- max(1L, min(as.integer(n_jobs), cpu_cap, ram_cap))
  cat(sprintf("Parallel capacity: %d logical cores, %s MB free RAM, %.0f MB estimated per worker; using %d worker(s).\n", cores, if (is.finite(free_mb)) format(round(free_mb), big.mark = ",") else "unknown", per_worker_mb, workers))
  workers
}
ANNEXP_WORKERS <- .annexp_choose_workers(.dev, length(.annexp_fit_formulas) * length(FOLDS[[1]]))
options(future.globals.maxSize = max(8 * 1024^3, 3 * as.numeric(utils::object.size(.dev[[1]]))))
.annexp_start_future_workers <- function(workers) {
  if (workers <= 1L) { future::plan(future::sequential); return(NULL) }
  cat(sprintf("Starting %d persistent PSOCK workers without project startup files.\n", workers)); flush.console()
  cluster <- parallelly::makeClusterPSOCK(workers, rscript_args = "--no-init-file", rscript_libs = .libPaths(), connectTimeout = 300, validate = TRUE, autoStop = FALSE)
  future::plan(future::cluster, workers = cluster)
  cluster
}
.annexp_stop_future_workers <- function(cluster) {
  try(future::plan(future::sequential), silent = TRUE)
  if (!is.null(cluster)) try(parallel::stopCluster(cluster), silent = TRUE)
  invisible(NULL)
}
.annexp_fit_oof_all <- function(formulas, dev_list, fold_map, grid, expected_strata_by_model, seed) {
  n_subjects <- nrow(dev_list[[1]]); n_grid <- length(grid)
  accumulators <- stats::setNames(lapply(formulas, function(x) matrix(0, n_subjects, n_grid)), names(formulas))
  for (imputation_index in seq_along(dev_list)) {
    .annexp_worker_data <- dev_list[[imputation_index]]
    .annexp_worker_jobs <- unlist(lapply(names(formulas), function(model_name) lapply(seq_along(fold_map[[imputation_index]]), function(fold_index) list(model = model_name, fold = fold_index, validation_indices = fold_map[[imputation_index]][[fold_index]]))), recursive = FALSE)
    cat(sprintf("Dense-grid OOF fitting, imputation %d/%d: %d model-fold jobs.\n", imputation_index, length(dev_list), length(.annexp_worker_jobs))); flush.console()
    .annexp_worker_results <- future.apply::future_lapply(.annexp_worker_jobs, function(job) {
      validation_indices <- job$validation_indices
      fit <- survival::coxph(.annexp_worker_formulas[[job$model]], data = .annexp_worker_data[-validation_indices, , drop = FALSE], ties = "efron", x = TRUE, y = TRUE, model = TRUE)
      baseline <- survival::basehaz(fit, centered = FALSE)
      n_strata <- if (is.null(baseline$strata)) 1L else nlevels(droplevels(factor(baseline$strata)))
      survival_matrix <- matrix(as.numeric(pec::predictSurvProb(fit, newdata = .annexp_worker_data[validation_indices, , drop = FALSE], times = .annexp_worker_grid)), nrow = length(validation_indices), ncol = length(.annexp_worker_grid))
      list(model = job$model, fold = job$fold, validation_indices = validation_indices, n_strata = n_strata, S = survival_matrix)
    }, future.seed = seed + imputation_index, future.scheduling = Inf, future.packages = c("survival", "pec"), future.globals = list(.annexp_worker_formulas = formulas, .annexp_worker_data = .annexp_worker_data, .annexp_worker_grid = grid))
    for (result in .annexp_worker_results) {
      expected_strata <- expected_strata_by_model[[result$model]]
      if (length(expected_strata) != 1L || result$n_strata != expected_strata) stop(sprintf("%s, imputation %d, fold %d: %d baseline strata, expected %s from its formula and development data.", result$model, imputation_index, result$fold, result$n_strata, if (length(expected_strata) == 1L) expected_strata else "missing"), call. = FALSE)
      if (anyNA(result$S) || min(result$S) < -1e-10 || max(result$S) > 1 + 1e-10) stop("Invalid survival prediction in ", result$model, ", imputation ", imputation_index, ", fold ", result$fold, ".", call. = FALSE)
      accumulators[[result$model]][result$validation_indices, ] <- accumulators[[result$model]][result$validation_indices, ] + result$S
    }
    rm(.annexp_worker_results, .annexp_worker_data); invisible(gc())
  }
  lapply(accumulators, function(S) S / length(dev_list))
}
.annexp_cif_product_integral <- function(S_readmit, S_death) {
  if (!identical(dim(S_readmit), dim(S_death))) stop("Readmission and death survival matrices have different dimensions.", call. = FALSE)
  H_readmit <- -log(pmax(S_readmit, 1e-12)); H_death <- -log(pmax(S_death, 1e-12)); S_all <- S_readmit * S_death
  n_subjects <- nrow(S_readmit); n_grid <- ncol(S_readmit); cif <- matrix(0, n_subjects, n_grid)
  previous_survival <- rep(1, n_subjects); previous_readmit_hazard <- rep(0, n_subjects); previous_death_hazard <- rep(0, n_subjects); accumulated <- rep(0, n_subjects)
  for (grid_index in seq_len(n_grid)) {
    delta_readmit <- pmax(H_readmit[, grid_index] - previous_readmit_hazard, 0); delta_death <- pmax(H_death[, grid_index] - previous_death_hazard, 0); delta_all <- delta_readmit + delta_death
    cause_fraction <- ifelse(delta_all > 0, delta_readmit / delta_all, 0); interval_event_probability <- -expm1(-delta_all)
    accumulated <- accumulated + previous_survival * cause_fraction * interval_event_probability
    cif[, grid_index] <- accumulated
    previous_survival <- S_all[, grid_index]; previous_readmit_hazard <- H_readmit[, grid_index]; previous_death_hazard <- H_death[, grid_index]
  }
  pmin(pmax(cif, 0), 1)
}
# Mandatory numerical control: without competing mortality the CIF must equal net risk exactly; adding mortality must not increase it.
.annexp_cif_test_grid <- c(0, 0.25, 1, 6, 12, 36, 60)
.annexp_cif_test_readmit <- rbind(exp(-0.01 * .annexp_cif_test_grid), exp(-0.10 * .annexp_cif_test_grid))
.annexp_cif_test_no_death <- matrix(1, nrow(.annexp_cif_test_readmit), ncol(.annexp_cif_test_readmit))
.annexp_cif_test_with_death <- rbind(exp(-0.02 * .annexp_cif_test_grid), exp(-0.20 * .annexp_cif_test_grid))
.annexp_cif_control_no_death <- .annexp_cif_product_integral(.annexp_cif_test_readmit, .annexp_cif_test_no_death)
.annexp_cif_control_with_death <- .annexp_cif_product_integral(.annexp_cif_test_readmit, .annexp_cif_test_with_death)
.annexp_cif_control_error <- max(abs(.annexp_cif_control_no_death - (1 - .annexp_cif_test_readmit)))
.annexp_cif_control_excess <- max(.annexp_cif_control_with_death - (1 - .annexp_cif_test_readmit))
if (!is.finite(.annexp_cif_control_error) || .annexp_cif_control_error > 1e-12 || !is.finite(.annexp_cif_control_excess) || .annexp_cif_control_excess > 1e-12) stop("The exact competing-hazard CIF control failed.", call. = FALSE)
cat(sprintf("Exact CIF control passed: no-death max error %.3e; competing-death max excess %.3e.\n", .annexp_cif_control_error, .annexp_cif_control_excess))
rm(.annexp_cif_test_grid, .annexp_cif_test_readmit, .annexp_cif_test_no_death, .annexp_cif_test_with_death, .annexp_cif_control_no_death, .annexp_cif_control_with_death)
.annexp_bootstrap_points <- function(points, B, seed, workers, manage_plan = TRUE) {
  if (length(points) < 2L || is.null(names(points))) stop("A named list with at least two point objects is required.", call. = FALSE)
  horizons <- points[[1]]$horizons; n_subjects <- nrow(points[[1]]$contrib_trapezoid)
  for (point in points) if (!identical(point$horizons, horizons) || nrow(point$contrib_trapezoid) != n_subjects) stop("Point objects do not share horizons and row alignment.", call. = FALSE)
  contribution_list <- lapply(points, `[[`, "contrib_trapezoid")
  cluster <- NULL
  if (isTRUE(manage_plan)) { cluster <- .annexp_start_future_workers(workers); on.exit(.annexp_stop_future_workers(cluster), add = TRUE) }
  bootstrap_list <- future.apply::future_lapply(seq_len(B), function(replicate_index) {
    counts <- tabulate(sample.int(.annexp_boot_n, .annexp_boot_n, replace = TRUE), nbins = .annexp_boot_n)
    t(vapply(.annexp_boot_contributions, function(contribution_matrix) as.numeric(counts %*% contribution_matrix) / .annexp_boot_n / .annexp_boot_horizons, numeric(length(.annexp_boot_horizons))))
  }, future.seed = seed, future.scheduling = Inf, future.globals = list(.annexp_boot_contributions = contribution_list, .annexp_boot_n = n_subjects, .annexp_boot_horizons = horizons))
  bootstrap_array <- simplify2array(bootstrap_list)
  dimnames(bootstrap_array) <- list(names(points), as.character(horizons), paste0("b", seq_len(B)))
  bootstrap_array
}
.annexp_quantile_rows <- function(values, probability) apply(values, 1L, function(x) { finite <- x[is.finite(x)]; if (length(finite) < 20L) NA_real_ else stats::quantile(finite, probs = probability, names = FALSE, na.rm = TRUE) })
.annexp_build_pairwise_table <- function(points, arm_metadata, bootstrap_array) {
  readmission_arms <- arm_metadata$arm[arm_metadata$outcome == "readmission"]
  mortality_arms <- arm_metadata$arm[arm_metadata$outcome == "mortality"]
  pair_list <- c(utils::combn(readmission_arms, 2L, simplify = FALSE), utils::combn(mortality_arms, 2L, simplify = FALSE))
  rows <- lapply(pair_list, function(pair) {
    arm_A <- pair[1]; arm_B <- pair[2]
    metadata_A <- arm_metadata[match(arm_A, arm_metadata$arm), , drop = FALSE]; metadata_B <- arm_metadata[match(arm_B, arm_metadata$arm), , drop = FALSE]
    boot_A <- bootstrap_array[arm_A, , , drop = TRUE]; boot_B <- bootstrap_array[arm_B, , , drop = TRUE]; boot_delta <- boot_A - boot_B
    point_A <- points[[arm_A]]$ibs$ibs; point_B <- points[[arm_B]]$ibs$ibs; point_delta <- point_A - point_B
    lower <- .annexp_quantile_rows(boot_delta, 0.025); upper <- .annexp_quantile_rows(boot_delta, 0.975)
    n_valid <- rowSums(is.finite(boot_delta)); p_lower <- (1 + rowSums(boot_delta <= 0, na.rm = TRUE)) / (1 + ncol(boot_delta)); p_upper <- (1 + rowSums(boot_delta >= 0, na.rm = TRUE)) / (1 + ncol(boot_delta)); p_two_sided <- pmin(1, 2 * pmin(p_lower, p_upper))
    same_definition <- metadata_A$prediction_definition == metadata_B$prediction_definition
    scope <- if (metadata_A$outcome == "mortality") "mortality pair" else if (same_definition) "within prediction definition" else "cross prediction definition"
    data.frame(outcome = metadata_A$outcome, comparison_scope = scope, model_A = metadata_A$model, model_B = metadata_B$model, prediction_A = metadata_A$prediction_definition, prediction_B = metadata_B$prediction_definition, arm_A = arm_A, arm_B = arm_B, horizon_months = points[[arm_A]]$horizons, n = nrow(points[[arm_A]]$contrib_trapezoid), ibs_A = point_A, ibs_A_lo = .annexp_quantile_rows(boot_A, 0.025), ibs_A_hi = .annexp_quantile_rows(boot_A, 0.975), ibs_B = point_B, ibs_B_lo = .annexp_quantile_rows(boot_B, 0.025), ibs_B_hi = .annexp_quantile_rows(boot_B, 0.975), delta_ibs = point_delta, delta_lo = lower, delta_hi = upper, bootstrap_mean = rowMeans(boot_delta, na.rm = TRUE), centering_bias = rowMeans(boot_delta, na.rm = TRUE) - point_delta, point_inside_ci = is.finite(lower) & is.finite(upper) & point_delta >= lower & point_delta <= upper, bootstrap_valid = n_valid, bootstrap_requested = dim(bootstrap_array)[3], p_boot_two_sided = p_two_sided, excludes_zero = is.finite(lower) & is.finite(upper) & (lower > 0 | upper < 0), favours = ifelse(is.finite(upper) & upper < 0, arm_A, ifelse(is.finite(lower) & lower > 0, arm_B, "No clear difference")), stringsAsFactors = FALSE, row.names = NULL)
  })
  table <- do.call(rbind, rows)
  table$p_holm_within_outcome_horizon <- NA_real_
  adjustment_groups <- split(seq_len(nrow(table)), interaction(table$outcome, table$horizon_months, drop = TRUE))
  for (indices in adjustment_groups) table$p_holm_within_outcome_horizon[indices] <- stats::p.adjust(table$p_boot_two_sided[indices], method = "holm")
  table <- table[order(table$outcome, table$horizon_months, table$comparison_scope, -abs(table$delta_ibs)), , drop = FALSE]
  rownames(table) <- NULL
  table
}
.annexp_signature <- list(artifact_version = "pred225_annex_ibs_pairwise_all_arms_v2_exact_cif", horizons = as.numeric(ANNEXP_HORIZONS), grid = as.numeric(ANNEXD_GRID), B = ANNEXP_B, seed = ANNEXP_SEED, fold_signature = .annexp_fold_signature, formula_keys = vapply(.annexp_fit_formulas, .annexp_formula_key, character(1)), expected_strata = .annexp_expected_strata, models = names(.annexp_fit_formulas), cif_engine = "piecewise-constant competing hazards with exact interval event mass", readmission_observed_side = "Aalen-Johansen competing-risk observed side for net risk and both CIF variants")
.annexp_control_points <- function(points) {
  controls <- list(`mortality::full-death` = ibs_dev_death_full, `mortality::shap-death` = ibs_dev_death_shap)
  differences <- vapply(names(controls), function(arm) {
    reference <- controls[[arm]]$ibs; candidate <- points[[arm]]$ibs
    reference_indices <- match(candidate$horizon_months, reference$horizon_months)
    if (anyNA(reference_indices)) stop("The legacy mortality control is missing a requested horizon for ", arm, ".", call. = FALSE)
    reference_values <- reference$ibs[reference_indices]; candidate_values <- candidate$ibs
    if (!identical(is.na(reference_values), is.na(candidate_values))) stop("NA pattern differs from the legacy mortality control for ", arm, ".", call. = FALSE)
    finite <- is.finite(reference_values) & is.finite(candidate_values)
    if (!any(finite)) 0 else max(abs(reference_values[finite] - candidate_values[finite]))
  }, numeric(1))
  cat(sprintf("Mandatory control: max |reimplemented - legacy classical IBS| = %.3e.\n", max(differences)))
  if (max(differences) > 1e-8) stop("The comprehensive engine does not reproduce the existing mortality IBS; pairwise results were discarded.", call. = FALSE)
  differences
}
.annexp_compute <- function() {
  if (file.exists(ANNEXP_CACHE)) {
    cached <- readRDS(ANNEXP_CACHE)
    if (!is.list(cached) || !identical(cached$meta$signature, .annexp_signature)) stop("The existing comprehensive IBS cache does not match the requested formulas, strata, folds, grid, horizons, B, or seed. It was not overwritten; choose a new ANNEXP_CACHE path after inspecting it.", call. = FALSE)
    if (!is.list(cached$points) || !is.data.frame(cached$pairwise_table) || !is.array(cached$bootstrap_replicates)) stop("The comprehensive IBS cache is structurally incomplete.", call. = FALSE)
    cat("Comprehensive paired IBS cache hit: dense-grid refitting and bootstrap were skipped.\n")
    .annexp_control_points(cached$points)
    return(cached)
  }
  cluster <- .annexp_start_future_workers(ANNEXP_WORKERS)
  on.exit(.annexp_stop_future_workers(cluster), add = TRUE)
  survival_matrices <- list()
  valid_matrix <- function(S) is.matrix(S) && identical(dim(S), c(.N_dev, length(ANNEXD_GRID))) && all(is.finite(S))
  if (exists("S_oof_death_full", inherits = TRUE) && valid_matrix(S_oof_death_full)) survival_matrices$death__full <- S_oof_death_full
  if (exists("S_oof_death_shap", inherits = TRUE) && valid_matrix(S_oof_death_shap)) survival_matrices$death__shap <- S_oof_death_shap
  missing_matrix_names <- setdiff(names(.annexp_fit_formulas), names(survival_matrices))
  if (length(missing_matrix_names) > 0L) {
    cat("Dense-grid matrices requiring OOF fitting: ", paste(missing_matrix_names, collapse = ", "), "\n", sep = "")
    fitted_matrices <- .annexp_fit_oof_all(.annexp_fit_formulas[missing_matrix_names], .dev, FOLDS, ANNEXD_GRID, .annexp_expected_strata[missing_matrix_names], ANNEXP_SEED)
    survival_matrices[names(fitted_matrices)] <- fitted_matrices
  }
  if (!setequal(names(survival_matrices), names(.annexp_fit_formulas))) stop("Not all requested dense-grid survival matrices were constructed.", call. = FALSE)
  points <- list(); metadata_rows <- list()
  for (model_name in names(.annexp_readmit_formulas)) {
    readmit_survival <- survival_matrices[[paste0("readmit__", model_name)]]
    net_arm <- paste("readmission", model_name, "net-risk", sep = "::")
    net_point <- ibs_classical_readmit(.mk_readmit_pred(readmit_survival), ANNEXP_HORIZONS, method = "aalen-johansen"); net_point$method <- "readmission-aj"
    points[[net_arm]] <- net_point; metadata_rows[[length(metadata_rows) + 1L]] <- data.frame(arm = net_arm, outcome = "readmission", model = model_name, prediction_definition = "net-risk", stringsAsFactors = FALSE)
    for (death_name in names(.annexp_death_formulas)) {
      death_matrix_name <- if (death_name == "full-death") "death__full" else "death__shap"
      cif <- .annexp_cif_product_integral(readmit_survival, survival_matrices[[death_matrix_name]])
      if (any(cif[, -1L, drop = FALSE] + 1e-10 < cif[, -ncol(cif), drop = FALSE])) stop("A computed CIF is not nondecreasing for readmission model ", model_name, " with ", death_name, ".", call. = FALSE)
      cif_excess_over_net <- max(cif - (1 - readmit_survival), na.rm = TRUE)
      if (!is.finite(cif_excess_over_net) || cif_excess_over_net > 1e-10) stop(sprintf("A computed CIF exceeds its corresponding net risk for readmission model %s with %s by %.3e.", model_name, death_name, cif_excess_over_net), call. = FALSE)
      prediction_definition <- paste0("cif-", death_name); arm <- paste("readmission", model_name, prediction_definition, sep = "::")
      point <- ibs_classical_readmit(.mk_readmit_pred(1 - cif), ANNEXP_HORIZONS, method = "aalen-johansen"); point$method <- "readmission-aj"
      points[[arm]] <- point; metadata_rows[[length(metadata_rows) + 1L]] <- data.frame(arm = arm, outcome = "readmission", model = model_name, prediction_definition = prediction_definition, stringsAsFactors = FALSE)
      rm(cif)
    }
  }
  for (death_name in names(.annexp_death_formulas)) {
    matrix_name <- if (death_name == "full-death") "death__full" else "death__shap"; arm <- paste("mortality", death_name, sep = "::")
    point <- ibs_classical_point(.mk_death_pred(survival_matrices[[matrix_name]]), horizons = ANNEXP_HORIZONS); point$method <- "death"
    points[[arm]] <- point; metadata_rows[[length(metadata_rows) + 1L]] <- data.frame(arm = arm, outcome = "mortality", model = death_name, prediction_definition = death_name, stringsAsFactors = FALSE)
  }
  arm_metadata <- do.call(rbind, metadata_rows); rownames(arm_metadata) <- NULL
  stopifnot(length(points) == 14L, sum(arm_metadata$outcome == "readmission") == 12L, sum(arm_metadata$outcome == "mortality") == 2L)
  control_differences <- .annexp_control_points(points)
  rm(survival_matrices); invisible(gc())
  cat(sprintf("Running one shared, parallel subject bootstrap for %d arms, B = %d.\n", length(points), ANNEXP_B)); flush.console()
  bootstrap_replicates <- .annexp_bootstrap_points(points, ANNEXP_B, ANNEXP_SEED, ANNEXP_WORKERS, manage_plan = FALSE)
  pairwise_table <- .annexp_build_pairwise_table(points, arm_metadata, bootstrap_replicates)
  expected_rows <- (choose(12L, 2L) + choose(2L, 2L)) * length(ANNEXP_HORIZONS)
  if (nrow(pairwise_table) != expected_rows) stop("The comprehensive pairwise table has ", nrow(pairwise_table), " rows; expected ", expected_rows, ".", call. = FALSE)
  bundle <- list(points = points, arm_metadata = arm_metadata, bootstrap_replicates = bootstrap_replicates, pairwise_table = pairwise_table, control_differences = control_differences, meta = list(signature = .annexp_signature, created = as.character(Sys.time()), workers_used = ANNEXP_WORKERS, delta_definition = "delta_ibs = IBS_A - IBS_B; negative values favour A"))
  if (file.exists(ANNEXP_CACHE)) stop("Refusing to overwrite an existing comprehensive IBS cache: ", ANNEXP_CACHE, call. = FALSE)
  temporary_cache <- tempfile(pattern = "pred225_annex_ibs_pairwise_", tmpdir = dirname(ANNEXP_CACHE), fileext = ".rds")
  saveRDS(bundle, temporary_cache)
  readback <- readRDS(temporary_cache)
  if (!identical(readback$meta$signature, .annexp_signature) || !identical(readback$pairwise_table, pairwise_table)) { unlink(temporary_cache); stop("The comprehensive IBS cache failed its read-back identity check.", call. = FALSE) }
  if (!file.rename(temporary_cache, ANNEXP_CACHE)) { unlink(temporary_cache); stop("Could not atomically finalize the comprehensive IBS cache.", call. = FALSE) }
  cat("Comprehensive paired IBS cache saved outside cons/: ", normalizePath(ANNEXP_CACHE, winslash = "/", mustWork = TRUE), "\n", sep = "")
  bundle
}
.annexp_result <- .annexp_compute()
ibs_dev_pairwise_points <- .annexp_result$points
ibs_dev_pairwise_arm_metadata <- .annexp_result$arm_metadata
ibs_dev_pairwise_boot_replicates <- .annexp_result$bootstrap_replicates
ibs_dev_pairwise_table <- .annexp_result$pairwise_table
ibs_dev_pairwise_same_definition <- ibs_dev_pairwise_table[ibs_dev_pairwise_table$outcome == "mortality" | ibs_dev_pairwise_table$comparison_scope == "within prediction definition", , drop = FALSE]
stopifnot(nrow(ibs_dev_pairwise_table) == 268L, nrow(ibs_dev_pairwise_same_definition) == 76L, all(ibs_dev_pairwise_table$bootstrap_requested == ANNEXP_B))
if (!dir.exists(.annex_out_dir)) dir.create(.annex_out_dir, recursive = TRUE)
if (!file.exists(ANNEXP_CSV)) {
  utils::write.csv(ibs_dev_pairwise_table, ANNEXP_CSV, row.names = FALSE)
  cat("Full 268-row paired table saved: ", normalizePath(ANNEXP_CSV, winslash = "/", mustWork = TRUE), "\n", sep = "")
} else {
  existing_csv <- utils::read.csv(ANNEXP_CSV, stringsAsFactors = FALSE)
  if (nrow(existing_csv) != nrow(ibs_dev_pairwise_table) || !identical(names(existing_csv), names(ibs_dev_pairwise_table))) stop("The existing comprehensive IBS CSV is inconsistent with the current table and was not overwritten: ", ANNEXP_CSV, call. = FALSE)
  cat("Comprehensive IBS CSV already exists and was not overwritten: ", normalizePath(ANNEXP_CSV, winslash = "/", mustWork = TRUE), "\n", sep = "")
}
.elapsed_pair <- proc.time() - .t_pair
cat(sprintf("[extension: comprehensive paired IBS] %d total rows; %d same-definition rows; %.1f min.\n", nrow(ibs_dev_pairwise_table), nrow(ibs_dev_pairwise_same_definition), .elapsed_pair[["elapsed"]] / 60))
print(knitr::kable(ibs_dev_pairwise_same_definition[, c("outcome", "model_A", "model_B", "prediction_A", "horizon_months", "ibs_A", "ibs_B", "delta_ibs", "delta_lo", "delta_hi", "p_boot_two_sided", "p_holm_within_outcome_horizon", "favours")], "markdown", digits = 6, caption = sprintf("Focused same-definition subset of the comprehensive paired out-of-fold classical IBS comparison on the development cohort (n = %s). Readmission includes every pair among base, interact, updated and updated2 within net risk, CIF with Full PH death and CIF with SHAP-informed death; mortality compares Full PH death with SHAP-informed death. All differences use the same 500 resampled subjects per replicate. delta IBS = IBS A - IBS B, so negative values favour A. The complete object ibs_dev_pairwise_table additionally includes every cross-definition readmission comparison and has 268 rows.", format(.N_dev, big.mark = ","))))

# Standalone continuation of p225-annex-ibs-null-ipa
# ===========================================================================
# ANNEX 2 of 4. NULL MODEL AND IPA. Runs after p225-annex-ibs-classical-dev.
# Nothing is refitted: it reuses the four scored objects that chunk left in the
# session, so it costs seconds. It ADDS a table, ibs_dev_ipa_table; it does not
# modify ibs_dev_table.
# WHY. A raw IBS is a level, not a skill. Two levels computed on different
# cohorts cannot be compared, because the difficulty of the cohort sits inside
# them: a cohort with fewer events gets a lower Brier score whatever the model
# does. The null is the intercept-only marginal curve, estimated on the same data
# and scored with the SAME weights, grid and integral as the model, and IPA, the
# index of prediction accuracy, is 1 - IBS_model / IBS_null. That ratio is what
# makes development, held-out and external results comparable.
# The interval is PAIRED: model and null share every resample, so the difficulty
# of the resampled cohort moves both terms together and largely cancels.
# The readmission null follows the estimand, as it must. A competing-risk model is
# compared against an Aalen-Johansen null and a cause-specific model against a
# Kaplan-Meier null; crossing them would make the skill meaningless.
# EXPECT SMALL IPA FOR MORTALITY. With about 4 percent of subjects dying, the
# marginal curve already scores well on the Brier scale, so the achievable skill
# is small by construction. That is a property of a rare outcome, not evidence of
# a poor model, and the manuscript should say so before a reviewer reads it the
# other way.
# ===========================================================================
.t0 <- proc.time()
stopifnot(exists("ibs_dev_readmit_cs"), exists("ibs_dev_readmit_aj"),
          exists("ibs_dev_death_full"), exists("ibs_dev_death_shap"),
          exists("ANNEXD_GRID"), exists("ANNEXD_HORIZONS"), exists("ANNEXD_B"),
          exists("ANNEXD_SEED"), exists(".N_dev"),
          exists(".rt"), exists(".re"), exists(".dt"), exists(".de"))
# The competing-risk marginal curves live in ibs_window_bootstrap_holdout.R, which
# ibs_classical_holdout.R otherwise resolves lazily through project_root and fails
# to find when the working directory is the cons folder.
if (!exists(".ibsb_marg_aj_surv") || !exists(".ibsb_marg_km")) {
  .r <- if (exists("project_root", inherits = TRUE) &&
            dir.exists(file.path(project_root, "cons", "_alt_scripts"))) project_root else
    { .cw <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      if (grepl("/cons$", .cw)) dirname(.cw) else .cw }
  source(file.path(.r, "cons", "_alt_scripts", "ibs_window_bootstrap_holdout.R"))
}
stopifnot(exists("ibs_classical_null"), exists("ibs_classical_skill_boot"),
          exists(".ibsb_marg_km"), exists(".ibsb_marg_aj_surv"))
# ---- 1. Prediction shells ------------------------------------------------------
# ibs_classical_null only reads nrow(pred$S) to know how many rows to broadcast
# the marginal curve over, and takes the grid from pred$grid, so a one-column
# shell is enough and no second n x grid matrix is allocated here.
.shell <- matrix(0, .N_dev, 1L)
.shell_death <- list(S = .shell, grid = ANNEXD_GRID,
  y_test = data.frame(time = .dt, event = .de),
  y_train = data.frame(time = .dt, event = .de))
.shell_readmit <- list(S = .shell, grid = ANNEXD_GRID,
  rt_val = .rt, re_val = .re, dt_val = .dt, de_val = .de,
  rt_tr = .rt, re_tr = .re, dt_tr = .dt, de_tr = .de)
# ---- 2. Nulls -------------------------------------------------------------------
# Mortality: one Kaplan-Meier null serves both specifications, because the null
# depends on the outcome and the cohort, not on the covariates of either model.
ibs_dev_null_death <- ibs_classical_null(.shell_death,
  horizons = ANNEXD_HORIZONS, outcome = "death")
ibs_dev_null_readmit_cs <- ibs_classical_null(.shell_readmit,
  horizons = ANNEXD_HORIZONS, outcome = "readmit", method = "ipcw")
ibs_dev_null_readmit_aj <- ibs_classical_null(.shell_readmit,
  horizons = ANNEXD_HORIZONS, outcome = "readmit", method = "aalen-johansen")
rm(.shell, .shell_death, .shell_readmit); invisible(gc())
# The null must be scored on the same rows as the model, otherwise the ratio is
# not a skill. Assert it rather than trust it.
stopifnot(nrow(ibs_dev_null_death$contrib_trapezoid) == nrow(ibs_dev_death_full$contrib_trapezoid),
          nrow(ibs_dev_null_readmit_cs$contrib_trapezoid) == nrow(ibs_dev_readmit_cs$contrib_trapezoid),
          nrow(ibs_dev_null_readmit_aj$contrib_trapezoid) == nrow(ibs_dev_readmit_aj$contrib_trapezoid))
# A model that cannot beat its own marginal curve has no skill at that horizon.
# Say it out loud instead of leaving a negative IPA to be noticed in a table.
for (.p in list(list("readmission cause-specific", ibs_dev_readmit_cs, ibs_dev_null_readmit_cs),
                list("readmission competing-risk", ibs_dev_readmit_aj, ibs_dev_null_readmit_aj),
                list("death-full", ibs_dev_death_full, ibs_dev_null_death),
                list("death-shap", ibs_dev_death_shap, ibs_dev_null_death))) {
  .bad <- which(.p[[2]]$ibs$ibs >= .p[[3]]$ibs$ibs)
  if (length(.bad))
    warning(sprintf("%s: IBS is at or above its null at horizon(s) %s, so IPA is not positive there.",
                    .p[[1]], paste(ANNEXD_HORIZONS[.bad], collapse = ", ")), call. = FALSE)
}
# ---- 3. IPA with a paired bootstrap ---------------------------------------------
# Arm keys are the same four strings the other annex chunks join on.
.skill_row <- function(pt, pt_null, arm, estimand) {
  s <- ibs_classical_skill_boot(pt, pt_null, B = ANNEXD_B, seed = ANNEXD_SEED,
                                label = arm, verbose = FALSE)
  data.frame(arm = arm, estimand = estimand, horizon_months = s$horizon_months,
             ibs_oof = s$ibs, ibs_null = s$ibs_null,
             ipa = s$ibs_skill, ipa_lo95 = s$skill_lo, ipa_hi95 = s$skill_hi,
             stringsAsFactors = FALSE, row.names = NULL)
}
ibs_dev_ipa_table <- rbind(
  .skill_row(ibs_dev_readmit_cs, ibs_dev_null_readmit_cs, "readmission-cs",
             "death as censoring (cause-specific), Kaplan-Meier null"),
  .skill_row(ibs_dev_readmit_aj, ibs_dev_null_readmit_aj, "readmission-aj",
             "death as definitive non-event (competing risk), Aalen-Johansen null"),
  .skill_row(ibs_dev_death_full, ibs_dev_null_death, "death-full",
             "all-cause mortality, Kaplan-Meier null"),
  .skill_row(ibs_dev_death_shap, ibs_dev_null_death, "death-shap",
             "all-cause mortality, Kaplan-Meier null"))
# The IBS column here must be the same number the previous chunk reported. If it
# is not, the two tables are describing different objects and neither can be read.
if (exists("ibs_dev_table")) {
  .j <- merge(ibs_dev_ipa_table[, c("arm", "horizon_months", "ibs_oof")],
              ibs_dev_table[, c("arm", "horizon_months", "ibs_oof")],
              by = c("arm", "horizon_months"), suffixes = c("_ipa", "_dev"))
  if (nrow(.j) != nrow(ibs_dev_ipa_table))
    stop("Arm or horizon keys do not line up with ibs_dev_table; re-run p225-annex-ibs-classical-dev.", call. = FALSE)
  .d <- max(abs(.j$ibs_oof_ipa - .j$ibs_oof_dev), na.rm = TRUE)
  cat(sprintf("Consistency with the preceding table: max |difference| in IBS = %.3e\n", .d))
  if (!is.finite(.d) || .d > 1e-12)
    stop("The IBS in this table does not match the preceding one; the point objects changed between chunks.", call. = FALSE)
}
# ---- 4. Export -------------------------------------------------------------------
if (exists(".annex_out_dir") && dir.exists(.annex_out_dir)) {
  .legacy_ipa_csv <- file.path(.annex_out_dir, "pred225_annex_ibs_null_ipa_oof_development.csv")
  if (!file.exists(.legacy_ipa_csv)) utils::write.csv(ibs_dev_ipa_table, .legacy_ipa_csv, row.names = FALSE) else cat("Legacy IPA CSV exists and was not overwritten:", .legacy_ipa_csv, "\n")
}
.elapsed <- proc.time() - .t0
cat(sprintf("\n[chunk: %s] %.1f sec\n", "p225-annex-ibs-null-ipa", .elapsed["elapsed"]))
print(knitr::kable(ibs_dev_ipa_table, "markdown", digits = 6, caption = sprintf(
  "Annex. Intercept-only null integrated Brier score and index of prediction accuracy (IPA = 1 - IBS/IBS null), out of fold on the development cohort (n = %s, the 80 percent training split), on the same classical integral, grid, weights and fold partition as the preceding table. The null is the marginal curve estimated on the same data and scored identically, so the comparison isolates what the covariates add. IPA intervals are paired bootstrap percentiles, model and null sharing every resample, which is what makes cohort difficulty cancel. Raw IBS levels are not comparable across cohorts; IPA is the quantity to carry into any comparison with held-out or external results. The readmission null follows the estimand of its arm: Kaplan-Meier for the cause-specific arm, Aalen-Johansen for the competing-risk arm. Mortality IPA is small by construction because roughly 4 percent of the cohort dies, so the marginal curve is already accurate on the Brier scale; that is a property of a rare outcome, not a deficiency of the model. Abbreviations: IBS, integrated Brier score; IPA, index of prediction accuracy; OOF, out of fold.",
  format(.N_dev, big.mark = ","))))
# ---- 5. All-arm IPA extension matching the comprehensive paired sweep -------
.t_ipa_all <- proc.time()
stopifnot(exists("ibs_dev_pairwise_points"), exists("ibs_dev_pairwise_arm_metadata"), exists(".annexp_bootstrap_points", mode = "function"), exists("ANNEXP_B"), exists("ANNEXP_SEED"), exists("ANNEXP_WORKERS"), identical(ANNEXP_HORIZONS, c(6, 12, 36, 60)))
.annexp_null_shell <- matrix(0, .N_dev, 1L)
.annexp_null_readmit_pred <- list(S = .annexp_null_shell, grid = ANNEXD_GRID, n_imp = length(.dev), rt_val = .rt, re_val = .re, dt_val = .dt, de_val = .de, rt_tr = .rt, re_tr = .re, dt_tr = .dt, de_tr = .de)
.annexp_null_death_pred <- list(S = .annexp_null_shell, grid = ANNEXD_GRID, n_imp = length(.dev), y_test = data.frame(time = .dt, event = .de), y_train = data.frame(time = .dt, event = .de))
ibs_dev_null_readmit_aj_all <- ibs_classical_null(.annexp_null_readmit_pred, horizons = ANNEXP_HORIZONS, outcome = "readmit", method = "aalen-johansen"); ibs_dev_null_readmit_aj_all$method <- "readmission-aj"
ibs_dev_null_death_all <- ibs_classical_null(.annexp_null_death_pred, horizons = ANNEXP_HORIZONS, outcome = "death"); ibs_dev_null_death_all$method <- "death"
rm(.annexp_null_shell, .annexp_null_readmit_pred, .annexp_null_death_pred); invisible(gc())
.annexp_skill_points <- c(ibs_dev_pairwise_points, list(`null::readmission-aj` = ibs_dev_null_readmit_aj_all, `null::death` = ibs_dev_null_death_all))
cat(sprintf("Running one shared, parallel model-null bootstrap for %d model arms and two nulls, B = %d.\n", length(ibs_dev_pairwise_points), ANNEXP_B)); flush.console()
ibs_dev_ipa_all_boot_replicates <- .annexp_bootstrap_points(.annexp_skill_points, ANNEXP_B, ANNEXP_SEED + 100000L, ANNEXP_WORKERS)
.annexp_skill_quantile <- function(values, probability) apply(values, 1L, function(x) { finite <- x[is.finite(x)]; if (length(finite) < 20L) NA_real_ else stats::quantile(finite, probs = probability, names = FALSE, na.rm = TRUE) })
.annexp_skill_rows <- lapply(seq_len(nrow(ibs_dev_pairwise_arm_metadata)), function(metadata_index) {
  metadata <- ibs_dev_pairwise_arm_metadata[metadata_index, , drop = FALSE]; arm <- metadata$arm; null_arm <- if (metadata$outcome == "readmission") "null::readmission-aj" else "null::death"
  model_point <- ibs_dev_pairwise_points[[arm]]$ibs$ibs; null_point <- .annexp_skill_points[[null_arm]]$ibs$ibs
  model_boot <- ibs_dev_ipa_all_boot_replicates[arm, , , drop = TRUE]; null_boot <- ibs_dev_ipa_all_boot_replicates[null_arm, , , drop = TRUE]; skill_boot <- 1 - model_boot / null_boot
  data.frame(outcome = metadata$outcome, model = metadata$model, prediction_definition = metadata$prediction_definition, arm = arm, horizon_months = ANNEXP_HORIZONS, ibs_oof = model_point, ibs_lo95 = .annexp_skill_quantile(model_boot, 0.025), ibs_hi95 = .annexp_skill_quantile(model_boot, 0.975), ibs_null = null_point, ibs_null_lo95 = .annexp_skill_quantile(null_boot, 0.025), ibs_null_hi95 = .annexp_skill_quantile(null_boot, 0.975), ipa = 1 - model_point / null_point, ipa_lo95 = .annexp_skill_quantile(skill_boot, 0.025), ipa_hi95 = .annexp_skill_quantile(skill_boot, 0.975), bootstrap_requested = ANNEXP_B, stringsAsFactors = FALSE, row.names = NULL)
})
ibs_dev_ipa_all_table <- do.call(rbind, .annexp_skill_rows)
ibs_dev_ipa_all_table <- ibs_dev_ipa_all_table[order(ibs_dev_ipa_all_table$outcome, ibs_dev_ipa_all_table$model, ibs_dev_ipa_all_table$prediction_definition, ibs_dev_ipa_all_table$horizon_months), , drop = FALSE]
rownames(ibs_dev_ipa_all_table) <- NULL
stopifnot(nrow(ibs_dev_ipa_all_table) == 56L, all(ibs_dev_ipa_all_table$bootstrap_requested == ANNEXP_B))
.annexp_point_control <- merge(ibs_dev_ipa_all_table[, c("arm", "horizon_months", "ibs_oof")], do.call(rbind, lapply(names(ibs_dev_pairwise_points), function(arm) data.frame(arm = arm, horizon_months = ibs_dev_pairwise_points[[arm]]$ibs$horizon_months, ibs_pairwise = ibs_dev_pairwise_points[[arm]]$ibs$ibs, stringsAsFactors = FALSE))), by = c("arm", "horizon_months"), all = FALSE)
.annexp_point_difference <- max(abs(.annexp_point_control$ibs_oof - .annexp_point_control$ibs_pairwise), na.rm = TRUE)
cat(sprintf("All-arm IPA control: max |IPA-table IBS - pairwise-table IBS| = %.3e.\n", .annexp_point_difference))
if (!is.finite(.annexp_point_difference) || .annexp_point_difference > 1e-12) stop("The all-arm IPA table does not reproduce the comprehensive pairwise IBS points.", call. = FALSE)
ANNEXP_IPA_CSV <- file.path(.annex_out_dir, "pred225_annex_ibs_ipa_all_arms_B500_v2.csv")
if (!file.exists(ANNEXP_IPA_CSV)) {
  utils::write.csv(ibs_dev_ipa_all_table, ANNEXP_IPA_CSV, row.names = FALSE)
  cat("All-arm IPA table saved: ", normalizePath(ANNEXP_IPA_CSV, winslash = "/", mustWork = TRUE), "\n", sep = "")
} else {
  existing_ipa_csv <- utils::read.csv(ANNEXP_IPA_CSV, stringsAsFactors = FALSE)
  if (nrow(existing_ipa_csv) != nrow(ibs_dev_ipa_all_table) || !identical(names(existing_ipa_csv), names(ibs_dev_ipa_all_table))) stop("The existing all-arm IPA CSV is inconsistent and was not overwritten: ", ANNEXP_IPA_CSV, call. = FALSE)
  cat("All-arm IPA CSV already exists and was not overwritten: ", normalizePath(ANNEXP_IPA_CSV, winslash = "/", mustWork = TRUE), "\n", sep = "")
}
.elapsed_ipa_all <- proc.time() - .t_ipa_all
cat(sprintf("[extension: all-arm IPA] %d rows; %.1f min.\n", nrow(ibs_dev_ipa_all_table), .elapsed_ipa_all[["elapsed"]] / 60))
print(knitr::kable(ibs_dev_ipa_all_table[, c("outcome", "model", "prediction_definition", "horizon_months", "ibs_oof", "ibs_null", "ipa", "ipa_lo95", "ipa_hi95")], "markdown", digits = 6, caption = sprintf("All-arm index of prediction accuracy on the development cohort (n = %s), aligned exactly to the comprehensive paired classical IBS sweep. Every readmission arm, including net risk, is scored against the Aalen-Johansen competing-risk observed side and null so risk-definition comparisons remain like-for-like. Model and null share all 500 resamples within each replicate.", format(.N_dev, big.mark = ","))))
.elapsed_script <- (proc.time() - .t0_script)[["elapsed"]] / 60
cat(sprintf("[script: pred225_annex_ibs_pairwise_B500.R] %.3f min\n", .elapsed_script))
