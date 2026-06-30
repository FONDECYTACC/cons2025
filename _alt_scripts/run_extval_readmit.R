# =============================================================================
# run_extval_readmit.R
# Apply the FROZEN readmission Cox model (no refitting) to the 2021-2024 external
# (temporal) validation cohort and compute discrimination (Uno's C), prediction
# error (IBS) and calibration, with nonparametric bootstrap 95% CIs.
#
# Frozen model: data/20241015_out/msm_artifact_2026_06_20.rds -> $cox_models$readmit_1_2
#   - cause-specific readmission Cox, clock = time since DISCHARGE (verified:
#     Tstop == readmit_time_from_disch_m), 26 covariates + strata(plan_type_strata)
#     + strata(tr_outcome_adm_discharge_rule_violation_undet); identical to the
#     prediction23 readmission model.
#
# Reused engines (pure functions, NO development data loaded):
#   - val_holdout_02_build_sets.R : .vh_* dummify pipeline (identical encoding /
#     reference levels / plan_type_strata build / time clamps as development)
#   - evaluate_dual_cox_python_style_boot.R : ibs_ipcw_train(), to01(),
#     .dual_cox_safe_concordance()
#   - validate_holdout_metrics.R : .vhm_deciles/.vhm_km_risk/.vhm_ici_loess/
#     .vhm_calibrate_one/.vhm_pool (KM observed; death treated as censoring)
#
# Public API:
#   dummify_extval(nondum)                 -> model-ready dummified data.frame
#   load_frozen_readmit()                  -> frozen coxph
#   predict_extval(fit, newdata, times)    -> list(lp, surv_mat, risk_mat)
#   evaluate_extval_readmit(fit, dat, eval_times)
#   calibrate_extval_readmit(fit, dat, times)
#   bootstrap_extval_readmit(fit, dat, eval_times, cal_times, B, seed, parallel)
#   run_extval_readmit(...)                -> full result list (+ optional save)
# =============================================================================

suppressWarnings({
  stopifnot(requireNamespace("survival", quietly = TRUE))
  stopifnot(requireNamespace("riskRegression", quietly = TRUE))
  stopifnot(requireNamespace("janitor", quietly = TRUE))
})

if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}

# Source reused engines (these only DEFINE functions; build_holdout_datasets() is
# never called here, so no development data is touched).
source(file.path(project_root, "cons/_alt_scripts/val_holdout_02_build_sets.R"))
source(file.path(project_root, "cons/_alt_scripts/evaluate_dual_cox_python_style_boot.R"))
source(file.path(project_root, "cons/_alt_scripts/validate_holdout_metrics.R"))

.EXTVAL_MODEL_PATH <- file.path(project_root, "data/20241015_out/msm_artifact_2026_06_20.rds")

# Non-dummy source columns the readmission model needs (categorical families are
# encoded by the .vh pipeline; numeric/logical kept as-is). plan_type_corr feeds
# plan_type_strata; tr_outcome feeds tr_outcome_adm_discharge_rule_violation_undet.
.EXTVAL_MODEL_NONDUM <- c(
  "primary_sub_mod", "sex_rec", "eva_consumo", "eva_sm", "evaluacindelprocesoteraputico",
  "ed_attainment_corr", "occupation_condition_corr24", "sub_dep_icd10_status",
  "prim_sub_freq_rec", "tr_outcome", "plan_type_corr",
  "adm_age_rec3", "porc_pobr", "ethnicity", "dit_m", "polysubstance_strict", "dg_psiq_cie_10_dg")
.EXTVAL_META <- c("readmit_time_from_disch_m", "readmit_event",
                  "death_time_from_disch_m", "death_event")

# Dummy column terms the frozen readmission model consumes (for a coverage guard).
.EXTVAL_MODEL_TERMS <- c(
  "primary_sub_mod_cocaine_paste","primary_sub_mod_cocaine_powder","primary_sub_mod_alcohol",
  "primary_sub_mod_others","sex_rec_woman","eva_consumo_logro_intermedio","eva_consumo_logro_minimo",
  "ed_attainment_corr_2_completed_high_school_or_less","ed_attainment_corr_3_completed_primary_school_or_less",
  "occupation_condition_corr24_unemployed","occupation_condition_corr24_inactive",
  "sub_dep_icd10_status_drug_dependence","eva_sm_logro_intermedio","eva_sm_logro_minimo",
  "evaluacindelprocesoteraputico_logro_intermedio","tr_outcome_referral","tr_outcome_dropout",
  "tr_outcome_adm_discharge_adm_reasons","prim_sub_freq_rec_2_2_6_days_wk","prim_sub_freq_rec_3_daily")

# ---- 1. Dummify the external cohort with the development encoding -------------
# complete_case = TRUE drops rows with any missing readmission-model predictor or
# outcome BEFORE encoding (so missingness is not hidden in a __MISSING__ dummy).
dummify_extval <- function(nondum, complete_case = TRUE, verbose = TRUE) {
  stopifnot(all(.EXTVAL_MODEL_NONDUM %in% names(nondum)))
  sub <- nondum[, unique(c(.EXTVAL_META, .EXTVAL_MODEL_NONDUM)), drop = FALSE]
  n0 <- nrow(sub)

  if (complete_case) {
    need <- c(.EXTVAL_MODEL_NONDUM, "readmit_time_from_disch_m", "readmit_event")
    cc <- stats::complete.cases(sub[, need, drop = FALSE])
    sub <- sub[cc, , drop = FALSE]
    if (verbose) cat(sprintf("Complete-case on readmission predictors: %d -> %d rows (dropped %d, %.1f%%)\n",
                             n0, nrow(sub), n0 - nrow(sub), 100 * (n0 - nrow(sub)) / n0))
  }

  # Reuse the development dummify pipeline (identical reference levels + encoding).
  df_clean <- .vh_set_reference_levels(as.data.frame(sub, stringsAsFactors = FALSE),
                                       refs = .vh_DUMMY_REFERENCE)
  dumm  <- .vh_make_dummies(df_clean)
  out   <- .vh_finalize(dumm, .vh_all_required_cols)

  # Coverage guard: every model term must be a real (non-zero-filled) column.
  zero_filled <- .EXTVAL_MODEL_TERMS[
    vapply(.EXTVAL_MODEL_TERMS, function(tm)
      !tm %in% names(out) || sum(out[[tm]], na.rm = TRUE) == 0, logical(1))]
  if (length(zero_filled))
    warning("Model dummy columns absent or all-zero (possible label mismatch): ",
            paste(zero_filled, collapse = ", "), call. = FALSE)

  # Strata coverage guard against the frozen model's known levels.
  stopifnot(all(c("plan_type_strata", "tr_outcome_adm_discharge_rule_violation_undet") %in% names(out)))
  bad_plan <- setdiff(as.character(unique(out$plan_type_strata)),
                      c("pg-pab","pg-pr","m-pr","pg-pai","m-pai"))
  if (length(bad_plan)) warning("Unexpected plan_type_strata levels: ", paste(bad_plan, collapse=", "), call.=FALSE)
  out
}

# ---- 2. Frozen model + predictions -------------------------------------------
load_frozen_readmit <- function(path = .EXTVAL_MODEL_PATH) {
  stopifnot(file.exists(path))
  fit <- readRDS(path)$cox_models$readmit_1_2
  stopifnot(inherits(fit, "coxph"))
  fit
}

# Linear predictor (for ranking/C-index) + survival/risk matrices (for IBS/calibration).
# The frozen model was fit with cluster = center_id (robust SE), which makes
# riskRegression::predictCox mis-detect a frailty term. We therefore compute the
# population-average (cluster-agnostic) survival directly from the Breslow baseline
# cumulative hazard by stratum: S_i(t) = exp(-H0_s(t) * exp(lp0_i)), where lp0 is
# the uncentered linear predictor. This matches survival::survfit(fit, newdata=.)
# to machine precision (verified) and is memory-light for the full cohort.
predict_extval <- function(fit, newdata, times) {
  times <- sort(unique(as.numeric(times)))
  newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  if (is.logical(newdata$dg_psiq_cie_10_dg))
    newdata$dg_psiq_cie_10_dg <- factor(newdata$dg_psiq_cie_10_dg, levels = c("FALSE", "TRUE"))

  lp  <- as.numeric(stats::predict(fit, newdata = newdata, type = "lp"))                  # centered (ranking)
  lp0 <- as.numeric(stats::predict(fit, newdata = newdata, type = "lp", reference = "zero")) # X %*% beta

  bh <- survival::basehaz(fit, centered = FALSE)
  key <- paste0(newdata$plan_type_strata,
                ", tr_outcome_adm_discharge_rule_violation_undet=",
                newdata$tr_outcome_adm_discharge_rule_violation_undet)
  bh_strata <- as.character(bh$strata)
  if (!all(key %in% bh_strata))
    stop("External stratum key absent from the frozen model baseline hazard: ",
         paste(unique(key[!key %in% bh_strata]), collapse = " | "), call. = FALSE)

  surv_mat <- matrix(NA_real_, nrow = nrow(newdata), ncol = length(times))
  for (st in unique(key)) {
    b <- bh[bh_strata == st, c("time", "hazard")]
    b <- b[order(b$time), ]
    H0 <- stats::approx(c(0, b$time), c(0, b$hazard), xout = times,
                        method = "constant", f = 0, rule = 2)$y
    idx <- which(key == st)
    surv_mat[idx, ] <- exp(-outer(exp(lp0[idx]), H0))
  }
  surv_mat <- pmin(pmax(surv_mat, 0), 1)
  list(lp = lp, surv_mat = surv_mat, risk_mat = 1 - surv_mat)
}

# ---- 3. Discrimination (Uno's C) + IBS ---------------------------------------
evaluate_extval_readmit <- function(fit, dat, eval_times = c(3, 6, 12, 36, 60),
                                    time_col = "readmit_time_from_disch_m",
                                    event_col = "readmit_event") {
  eval_times <- sort(unique(as.numeric(eval_times)))
  pr <- predict_extval(fit, dat, eval_times)
  tt <- as.numeric(dat[[time_col]]); ee <- to01(dat[[event_col]])
  tmax <- max(tt, na.rm = TRUE)

  c_global   <- .dual_cox_safe_concordance(tt, ee, pr$lp)
  ibs_global <- ibs_ipcw_train(dat, dat, pr$surv_mat, eval_times, time_col, event_col)
  c_h <- ibs_h <- stats::setNames(rep(NA_real_, length(eval_times)), as.character(eval_times))
  for (ii in seq_along(eval_times)) {
    h <- eval_times[ii]
    if (h < tmax) {
      c_h[ii] <- .dual_cox_safe_concordance(tt, ee, pr$lp, ymax = h)
      keep <- eval_times <= h
      if (sum(keep) >= 2L)
        ibs_h[ii] <- ibs_ipcw_train(dat, dat, pr$surv_mat[, keep, drop = FALSE],
                                    eval_times[keep], time_col, event_col)
    }
  }
  rows <- rbind(
    data.frame(Metric = "Uno's C-Index", Time = "Global", Value = c_global),
    data.frame(Metric = "IBS",           Time = "Global", Value = ibs_global),
    data.frame(Metric = "Uno's C-Index", Time = names(c_h),   Value = unname(c_h)),
    data.frame(Metric = "IBS",           Time = names(ibs_h), Value = unname(ibs_h)))
  list(summary = rows, lp = pr$lp, surv_mat = pr$surv_mat, eval_times = eval_times,
       time = tt, event = ee)
}

# ---- 4. Calibration (KM observed; death as censoring) ------------------------
# Predicted = 1 - S(t) from the frozen Cox; observed = 1 - KM(t) within deciles.
# Reuses the development calibration helpers (.vhm_*) for byte-identical math.
calibrate_extval_readmit <- function(fit, dat, times = c(6, 12, 36, 60),
                                     g = 10L, min_bin_n = 20L, span = 0.75,
                                     time_col = "readmit_time_from_disch_m",
                                     event_col = "readmit_event") {
  times <- sort(unique(as.numeric(times)))
  pr <- predict_extval(fit, dat, times)
  tt <- as.numeric(dat[[time_col]]); ee <- to01(dat[[event_col]])
  metrics_all <- list(); curves_all <- list()
  for (j in seq_along(times)) {
    h <- times[j]; pv <- pr$risk_mat[, j]
    obs_overall <- .vhm_km_risk(tt, ee, h)
    res <- .vhm_calibrate_one(pv, h, obs_overall,
      observed_by_bin_fun = function(idx) .vhm_km_risk(tt[idx], ee[idx], h),
      g = g, min_bin_n = min_bin_n, span = span)
    res$metrics$imp <- 1L; metrics_all[[length(metrics_all) + 1L]] <- res$metrics
    if (!is.null(res$curve)) curves_all[[length(curves_all) + 1L]] <- res$curve
  }
  out <- .vhm_pool(metrics_all, curves_all, 1L)
  out$risk <- "readmission"; out$method <- "frozen Cox + Kaplan-Meier (death as censoring), external 2021-2024"
  out
}

# ---- 5. Bootstrap CIs (resample patients; model frozen) -----------------------
bootstrap_extval_readmit <- function(fit, dat, eval_times = c(3, 6, 12, 36, 60),
                                     cal_times = c(6, 12, 36, 60),
                                     B = 1000L, seed = 2125L, parallel = TRUE,
                                     time_col = "readmit_time_from_disch_m",
                                     event_col = "readmit_event", verbose = TRUE) {
  eval_times <- sort(unique(as.numeric(eval_times)))
  cal_times  <- sort(unique(as.numeric(cal_times)))
  # Predict once (frozen); bootstrap only resamples rows of the fixed predictions.
  pr <- predict_extval(fit, dat, sort(unique(c(eval_times, cal_times))))
  all_times <- sort(unique(c(eval_times, cal_times)))
  lp <- pr$lp; surv_all <- pr$surv_mat
  tt <- as.numeric(dat[[time_col]]); ee <- to01(dat[[event_col]]); n <- length(tt)

  one_rep <- function(bi) {
    t <- tt[bi]; e <- ee[bi]; s <- lp[bi]; sv <- surv_all[bi, , drop = FALSE]
    tmax <- max(t, na.rm = TRUE)
    cg  <- .dual_cox_safe_concordance(t, e, s)
    ig  <- ibs_ipcw_train(data.frame(t = t, e = e), data.frame(t = t, e = e),
                          sv[, match(eval_times, all_times), drop = FALSE], eval_times, "t", "e")
    ch <- vapply(eval_times, function(h) if (h < tmax) .dual_cox_safe_concordance(t, e, s, ymax = h) else NA_real_, numeric(1))
    ici <- vapply(cal_times, function(h) {
      j <- match(h, all_times); risk <- 1 - sv[, j]
      obs_overall <- .vhm_km_risk(t, e, h)
      res <- tryCatch(.vhm_calibrate_one(risk, h, obs_overall,
        observed_by_bin_fun = function(idx) .vhm_km_risk(t[idx], e[idx], h),
        g = 10L, min_bin_n = 20L, span = 0.75), error = function(err) NULL)
      if (is.null(res)) NA_real_ else res$metrics$ici
    }, numeric(1))
    eo <- vapply(cal_times, function(h) {
      j <- match(h, all_times); mp <- mean(1 - sv[, j], na.rm = TRUE)
      ob <- .vhm_km_risk(t, e, h); if (is.finite(ob) && ob > 0) mp / ob else NA_real_
    }, numeric(1))
    c(c_global = cg, ibs_global = ig,
      setNames(ch, paste0("c_", eval_times)),
      setNames(ici, paste0("ici_", cal_times)),
      setNames(eo,  paste0("eo_",  cal_times)))
  }

  set.seed(seed)
  idx <- lapply(seq_len(B), function(i) sample.int(n, n, replace = TRUE))
  reps <- if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply(idx, one_rep, future.seed = TRUE)
  } else lapply(idx, one_rep)
  M <- do.call(rbind, reps)
  qlo <- apply(M, 2, quantile, 0.025, na.rm = TRUE, names = FALSE)
  qhi <- apply(M, 2, quantile, 0.975, na.rm = TRUE, names = FALSE)
  data.frame(metric = colnames(M), lo = qlo, hi = qhi, row.names = NULL)
}

# ---- 6. Orchestrator ---------------------------------------------------------
run_extval_readmit <- function(eval_times = c(3, 6, 12, 36, 60),
                               cal_times = c(6, 12, 36, 60),
                               B = 1000L, seed = 2125L, parallel = TRUE,
                               complete_case = TRUE, save = TRUE, verbose = TRUE) {
  source(file.path(project_root, "cons/_alt_scripts/extval_01_build_cohort.R"))
  built <- build_extval_readmit_cohort(verbose = verbose)
  dat   <- dummify_extval(built$nondum, complete_case = complete_case, verbose = verbose)
  fit   <- load_frozen_readmit()

  if (verbose) cat("\n== Discrimination + IBS ==\n")
  ev  <- evaluate_extval_readmit(fit, dat, eval_times)
  if (verbose) cat("== Calibration ==\n")
  cal <- calibrate_extval_readmit(fit, dat, cal_times)
  if (verbose) cat("== Bootstrap CIs (B=", B, ") ==\n", sep = "")
  boot <- bootstrap_extval_readmit(fit, dat, eval_times, cal_times, B = B, seed = seed,
                                   parallel = parallel, verbose = verbose)

  out <- list(cohort_flow = built$flow, params = built$params,
              n_model = nrow(dat), eval_times = eval_times, cal_times = cal_times,
              metrics = ev$summary, calibration = cal, boot = boot,
              created = as.character(Sys.time()))
  if (save) {
    out_dir <- file.path(project_root, "cons", "_out")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    f <- file.path(out_dir, paste0("extval_readmit_", format(Sys.Date(), "%Y_%m_%d"), ".rds"))
    saveRDS(out, f); if (verbose) cat("\nSaved:", f, "\n")
  }
  out
}
