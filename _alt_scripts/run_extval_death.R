# =============================================================================
# run_extval_death.R
# External (case-mix) validation of the FROZEN SHAP-informed, rule-based mortality
# Cox model (best_perf2 = formula_shap_death_rule2) on SENDA convenios 3 and 5,
# which the development cohort (convenio 1 + women's plans) never saw.
#
# "Frozen" = the model is the one defined by the development data: we fit
# formula_shap_death_rule2 on the FULL development cohort (5 MICE imputations) and
# pool predictions across imputations (Rubin: average lp for ranking, average risk
# for IBS/calibration). No tuning or refitting on C3/C5. This reproduces
# prediction23_converted_mod's best_perf2 exactly (same engine internals, same
# formula) but scores an external population.
#
# 2026-07-20 update: switched from the retired 13-var hand-curated
# formula_shap_death (isolated primary_sub_mod dummy, no cohabitation) to
# formula_shap_death_rule2 (family-complete primary_sub_mod, PH-clean via a SECOND
# stratum on any_phys_dx), matching prediction23_converted_mod's current best_perf2
# and the project's "SHAP-informed / rule2 only" convention. Discrimination
# (Uno's C) also switched from linear-predictor ranking to per-horizon absolute-risk
# (1-S(t)) ranking, matching the risk-based convention already adopted by
# prediction26_ext_val_readm_mod (evaluate_uno_absolute_risk).
#
# Mortality model (best_perf2 = formula_shap_death_rule2):
#   Surv(death_time_from_disch_m, death_event) ~ adm_age_rec3 +
#     primary_sub_mod_cocaine_paste + primary_sub_mod_cocaine_powder +
#     primary_sub_mod_alcohol + primary_sub_mod_others +
#     prim_sub_freq_rec_2_2_6_days_wk + prim_sub_freq_rec_3_daily +
#     occupation_condition_corr24_unemployed + occupation_condition_corr24_inactive +
#     eva_ocupacion_logro_intermedio + eva_ocupacion_logro_minimo +
#     cohabitation_family_of_origin + cohabitation_with_couple_children +
#     cohabitation_others + strata(plan_type_strata) + strata(any_phys_dx)
#
# Plan stratum mapping (user-agreed): C3 -> pg-pr (single residential stratum);
# C5 pai-ia -> pg-pai, pr-ia/ml-pr -> pg-pr. Discrimination (Uno's C) does NOT use
# the baseline hazard, so it is transportable regardless; IBS/calibration borrow
# the development baseline of the mapped (plan_type_strata, any_phys_dx) combo.
#
# Reused engines (pure functions; identical encoding/estimators as prediction23):
#   val_holdout_02_build_sets.R              (.vh_* dummify pipeline, contract cols)
#   evaluate_dual_cox_python_style_boot.R    (to01, ibs_ipcw_train, .dual_cox_safe_concordance)
#   validate_holdout_metrics.R               (.vhm_km_risk/.vhm_calibrate_one/.vhm_pool)
#
# Public API:
#   build_dev_full(force=FALSE)                       -> list of 5 dummified dev imputations
#   fit_death_dev(dev_list)                           -> list of 5 coxph fits
#   dummify_extval_death(nondum, complete_case=TRUE)  -> model-ready external df
#   predict_death_pooled(fits, newdata, times)        -> list(lp, surv_mat, risk_mat)
#   evaluate_extval_death(fits, dat, eval_times, do_ibs)
#   calibrate_extval_death(fits, dat, times)
#   bootstrap_extval_death(fits, dat, eval_times, cal_times, B, do_ibs, do_cal)
#   match_common_support(dev_ref, ext, covars)        -> logical keep + ps info
#   run_extval_death(...)                             -> full result list (+ save)
# =============================================================================

suppressWarnings({
  stopifnot(requireNamespace("riskRegression", quietly = TRUE))
  stopifnot(requireNamespace("nanoparquet", quietly = TRUE))
  stopifnot(requireNamespace("janitor", quietly = TRUE))
})
# Attach survival so bare Surv()/strata() in formula_shap_death_rule2 resolve when
# coxph builds its model frame (the formula environment is globalenv()).
suppressWarnings(suppressMessages(library(survival)))

if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}

source(file.path(project_root, "cons/_alt_scripts/val_holdout_02_build_sets.R"))
source(file.path(project_root, "cons/_alt_scripts/evaluate_dual_cox_python_style_boot.R"))
source(file.path(project_root, "cons/_alt_scripts/validate_holdout_metrics.R"))

.EVD_DATA_DIR   <- file.path(project_root, "data/20241015_out")
.EVD_VAL_INPUTS <- file.path(.EVD_DATA_DIR, "_val_inputs")
.EVD_DEV_CACHE  <- file.path(.EVD_VAL_INPUTS, "extval_dev_full_dummified.rds")
# sorted to match the development plan_type_strata factor (as.factor in .vh_finalize)
.EVD_PLAN_LEVELS <- c("m-pai", "m-pr", "pg-pab", "pg-pai", "pg-pr")

# Mortality model formula (best_perf2 = formula_shap_death_rule2). Hardcoded
# verbatim from prediction23_converted_mod (it is NOT in formulas.rds: that cache
# was extracted from an Rdata snapshot that predates rule2's adoption as best_perf2
# -- see the 2026-07-20 header note).
load_death_formula <- function() {
  f <- stats::as.formula(paste(
    "Surv(death_time_from_disch_m, death_event) ~ adm_age_rec3 +",
    "primary_sub_mod_cocaine_paste + primary_sub_mod_cocaine_powder +",
    "primary_sub_mod_alcohol + primary_sub_mod_others +",
    "prim_sub_freq_rec_2_2_6_days_wk + prim_sub_freq_rec_3_daily +",
    "occupation_condition_corr24_unemployed + occupation_condition_corr24_inactive +",
    "eva_ocupacion_logro_intermedio + eva_ocupacion_logro_minimo +",
    "cohabitation_family_of_origin + cohabitation_with_couple_children +",
    "cohabitation_others + strata(plan_type_strata) + strata(any_phys_dx)"))
  environment(f) <- globalenv()
  f
}

# rule2 coefficient (dummy) terms -- used for the "zero-filled in external data"
# guard. any_phys_dx and plan_type_strata are strata, not coefficients, so they
# are handled separately (see .EVD_DEATH_MATCH_COVARS below); a stratifier being
# single-valued in the external set is not a modeling problem the way a zeroed-out
# coefficient dummy would be (quasi-separation).
.EVD_DEATH_TERMS <- c(
  "adm_age_rec3", "primary_sub_mod_cocaine_paste", "primary_sub_mod_cocaine_powder",
  "primary_sub_mod_alcohol", "primary_sub_mod_others",
  "prim_sub_freq_rec_2_2_6_days_wk", "prim_sub_freq_rec_3_daily",
  "occupation_condition_corr24_unemployed", "occupation_condition_corr24_inactive",
  "eva_ocupacion_logro_intermedio", "eva_ocupacion_logro_minimo",
  "cohabitation_family_of_origin", "cohabitation_with_couple_children",
  "cohabitation_others")

# Propensity-matching covariate set: the 14 rule2 coefficient dummies PLUS
# any_phys_dx (a stratifier, but still a substantively important variable to
# balance the external cohort on).
.EVD_DEATH_MATCH_COVARS <- c(.EVD_DEATH_TERMS, "any_phys_dx")

# Non-dummy source columns the death cohort provides (others are zero-filled and
# unused by the rule2 model).
.EVD_NONDUM <- c("primary_sub_mod", "prim_sub_freq_rec",
  "occupation_condition_corr24", "eva_ocupacion", "cohabitation",
  "plan_type_corr", "adm_age_rec3", "any_phys_dx")
.EVD_META <- c("death_time_from_disch_m", "death_event",
               "readmit_time_from_disch_m", "readmit_event")

# ---- 1. FULL development data, dummified to the contract (5 imputations) ------
# All 88,152 rows (no 80/20 split): the model is the one fit on the entire
# development cohort. Cached to avoid re-reading parquet.
build_dev_full <- function(force = FALSE, verbose = TRUE) {
  if (!force && file.exists(.EVD_DEV_CACHE)) {
    if (verbose) cat("Loading cached dev (full) dummified:", .EVD_DEV_CACHE, "\n")
    return(readRDS(.EVD_DEV_CACHE))
  }
  manifest <- readRDS(file.path(.EVD_DATA_DIR, "corrected_datasets_nondum_filt_manifest.rds"))
  imp_paths <- manifest$paths
  stopifnot(length(imp_paths) == 5L, all(file.exists(imp_paths)))
  if (verbose) cat("Reading 5 parquet imputations and dummifying FULL dev set...\n")
  dev_list <- lapply(imp_paths, function(p) {
    df <- as.data.frame(nanoparquet::read_parquet(p))
    split_all <- data.frame(is_train = rep(TRUE, nrow(df)))
    .vh_finalize(.vh_preprocess_one(df, split_all, keep_train = TRUE), .vh_all_required_cols)
  })
  stopifnot(length(unique(vapply(dev_list, nrow, 0L))) == 1L)
  if (verbose) cat(sprintf("  dev rows per imputation: %d\n", nrow(dev_list[[1]])))
  saveRDS(dev_list, .EVD_DEV_CACHE)
  dev_list
}

# ---- 2. Fit the frozen mortality model on each dev imputation -----------------
fit_death_dev <- function(dev_list, f_death = load_death_formula(), verbose = TRUE) {
  f <- stats::as.formula(gsub("strat\\(", "strata(",
        paste(deparse(f_death, width.cutoff = 500L), collapse = " ")))
  fits <- lapply(seq_along(dev_list), function(i) {
    survival::coxph(f, data = as.data.frame(dev_list[[i]]), x = TRUE, y = TRUE)
  })
  if (verbose) cat(sprintf("Fitted mortality model on %d dev imputations.\n", length(fits)))
  fits
}

# ---- 3. Dummify an external cohort to the contract ----------------------------
# plan_type_strata is set EXPLICITLY from plan_type_corr (the .vh_finalize builder
# silently defaults to pg-pab when the external set lacks the pg-pab level and the
# single present level yields no dummy column).
dummify_extval_death <- function(nondum, complete_case = TRUE, verbose = TRUE) {
  stopifnot(all(.EVD_NONDUM %in% names(nondum)))
  sub <- as.data.frame(nondum[, unique(c(.EVD_META, .EVD_NONDUM)), drop = FALSE],
                       stringsAsFactors = FALSE)
  # any_phys_dx must stay LOGICAL: it is a stratum in rule2 (strata(any_phys_dx)),
  # and the frozen fit's basehaz()/predict() machinery below assumes a genuine
  # logical column, matching how it was encoded at fit time.
  sub$any_phys_dx <- as.logical(sub$any_phys_dx)
  n0 <- nrow(sub)
  cc_index <- seq_len(n0)
  if (complete_case) {
    need <- c("primary_sub_mod", "prim_sub_freq_rec", "occupation_condition_corr24",
              "eva_ocupacion", "cohabitation", "plan_type_corr",
              "adm_age_rec3", "any_phys_dx", "death_time_from_disch_m", "death_event")
    cc <- stats::complete.cases(sub[, need, drop = FALSE])
    cc_index <- which(cc)
    sub <- sub[cc, , drop = FALSE]
    if (verbose) cat(sprintf("Complete-case on mortality predictors: %d -> %d rows (dropped %d, %.1f%%)\n",
                             n0, nrow(sub), n0 - nrow(sub), 100 * (n0 - nrow(sub)) / n0))
  }
  plan_corr <- as.character(sub$plan_type_corr)   # keep BEFORE dummify for explicit strata

  # plan_type_corr is dropped from the dummify input: it can be single-level in an
  # external convenio (C3 = all pg-pr), which breaks model.matrix contrasts, and we
  # set plan_type_strata EXPLICITLY below anyway (the .vh_finalize builder would
  # otherwise default to pg-pab when the pg-pab level is absent).
  sub_dum <- sub[, setdiff(names(sub), "plan_type_corr"), drop = FALSE]
  df_clean <- .vh_set_reference_levels(sub_dum, refs = .vh_DUMMY_REFERENCE)
  dumm <- .vh_make_dummies(df_clean)
  out  <- .vh_finalize(dumm, .vh_all_required_cols)

  # explicit, correct plan_type_strata (override the pipeline's pg-pab default)
  out$plan_type_strata <- factor(plan_corr, levels = .EVD_PLAN_LEVELS)
  out$any_phys_dx <- as.logical(out$any_phys_dx)

  zero_filled <- .EVD_DEATH_TERMS[vapply(.EVD_DEATH_TERMS, function(tm)
    !tm %in% names(out) || all(out[[tm]] == 0, na.rm = TRUE), logical(1))]
  zero_filled <- setdiff(zero_filled, "adm_age_rec3")  # continuous, may be fine
  if (length(zero_filled) && verbose)
    cat("  NOTE: mortality terms all-zero in this external set (expected if the level is absent): ",
        paste(zero_filled, collapse = ", "), "\n")
  bad_plan <- setdiff(as.character(unique(out$plan_type_strata)), .EVD_PLAN_LEVELS)
  if (length(bad_plan)) warning("Unexpected plan_type_strata levels: ", paste(bad_plan, collapse = ", "), call. = FALSE)
  attr(out, "cc_index") <- cc_index    # rows of the input nondum kept (for SMD alignment)
  out
}

# ---- 4. MI-pooled predictions ------------------------------------------------
# lp pooled by mean (ranking); survival pooled by mean (IBS / calibration / the
# risk-based Uno's C used per horizon, see section 5). Survival is computed
# directly from the Breslow baseline cumulative hazard by stratum:
# S_i(t) = exp(-H0_s(t) * exp(lp0_i)), lp0 = X %*% beta (reference="zero"). This
# avoids riskRegression::predictRisk, which mis-reconstructs the model frame for
# these stratified fits (logical covariate + formula environment), and matches
# survival::survfit to machine precision (same approach as run_extval_readmit.R).
#
# rule2 has TWO strata (plan_type_strata x any_phys_dx), so basehaz()'s combined
# strata labels mix two formats: survival::strata() prints a bare factor level
# with no "varname=" prefix for the FIRST term (plan_type_strata, e.g. "pg-pr")
# but DOES prefix a logical/numeric term (any_phys_dx, e.g. "any_phys_dx=FALSE"),
# joined by ", " -- e.g. "pg-pr, any_phys_dx=FALSE". .evd_parse_strata_label()
# is format-agnostic: any comma-separated part containing "=" is read directly as
# var=val; any bare part (no "=") is assigned to whichever of the two known
# stratum variables hasn't been claimed yet. Verified against survival::survfit()
# on synthetic two-strata data (incl. a logical stratum) to machine precision
# (max|manual - survfit| ~ 1e-16, both at the stratum-baseline level and for
# individual predictions using each person's own lp).
.EVD_DEATH_STRATA_VARS <- c("plan_type_strata", "any_phys_dx")
.evd_parse_strata_label <- function(lab, strata_vars = .EVD_DEATH_STRATA_VARS) {
  parts <- strsplit(lab, ",\\s*")[[1]]
  out <- stats::setNames(rep(NA_character_, length(strata_vars)), strata_vars)
  bare <- character(0)
  for (part in parts) {
    if (grepl("=", part, fixed = TRUE)) {
      kv <- strsplit(part, "=", fixed = TRUE)[[1]]
      out[[trimws(kv[1])]] <- trimws(kv[2])
    } else {
      bare <- c(bare, trimws(part))
    }
  }
  unclaimed <- strata_vars[is.na(out)]
  if (length(bare) != length(unclaimed))
    stop("strata label parsing mismatch: '", lab, "'", call. = FALSE)
  out[unclaimed] <- bare
  out
}
.evd_strata_key <- function(plan, phys) {
  paste0("plan=", as.character(plan), "|phys=", as.character(phys))
}
predict_death_pooled <- function(fits, newdata, times) {
  times <- sort(unique(as.numeric(times)))
  newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  newdata$plan_type_strata <- factor(as.character(newdata$plan_type_strata), levels = .EVD_PLAN_LEVELS)
  newdata$any_phys_dx <- as.logical(newdata$any_phys_dx)
  n <- nrow(newdata)
  keylev <- .evd_strata_key(newdata$plan_type_strata, newdata$any_phys_dx)
  lp_acc <- numeric(n); surv_acc <- matrix(0, n, length(times))
  for (fit in fits) {
    lp  <- as.numeric(stats::predict(fit, newdata = newdata, type = "lp"))
    lp0 <- as.numeric(stats::predict(fit, newdata = newdata, type = "lp", reference = "zero"))
    bh <- survival::basehaz(fit, centered = FALSE)
    bh_levels <- levels(bh$strata)
    parsed <- lapply(bh_levels, .evd_parse_strata_label)
    bh_key <- stats::setNames(
      vapply(parsed, function(p) .evd_strata_key(p[["plan_type_strata"]], p[["any_phys_dx"]]), character(1)),
      bh_levels)
    bh$.key <- bh_key[as.character(bh$strata)]
    sm <- matrix(NA_real_, n, length(times))
    for (st in unique(keylev)) {
      b <- bh[!is.na(bh$.key) & bh$.key == st, c("time", "hazard")]
      if (!nrow(b)) stop("stratum '", st, "' absent from baseline hazard", call. = FALSE)
      b <- b[order(b$time), ]
      H0 <- stats::approx(c(0, b$time), c(0, b$hazard), xout = times,
                          method = "constant", f = 0, rule = 2)$y
      idx <- which(keylev == st)
      sm[idx, ] <- exp(-outer(exp(lp0[idx]), H0))
    }
    lp_acc <- lp_acc + lp
    surv_acc <- surv_acc + pmin(pmax(sm, 0), 1)
  }
  lp <- lp_acc / length(fits); surv_mat <- surv_acc / length(fits)
  list(lp = lp, surv_mat = surv_mat, risk_mat = 1 - surv_mat)
}

# ---- 5. Discrimination (Uno's C) + optional IBS ------------------------------
# Per-horizon Uno's C ranks by ABSOLUTE RISK at that horizon (1-S(h)), not by the
# linear predictor: with a stratified baseline hazard, 1-S(t) is not a monotone
# transform of lp, so risk-based concordance is horizon-specific and matches the
# convention already adopted in prediction26_ext_val_readm_mod's primary table
# (evaluate_uno_absolute_risk) and the CV pipeline's
# options(dualcox.concordance_score = "risk"). The "Global" row has no single
# natural risk horizon, so it is reported separately on the (horizon-independent)
# linear predictor, exactly as prediction26's supplementary within-stratum table
# does; it is NOT comparable to the risk-based horizon-specific rows above it.
evaluate_extval_death <- function(fits, dat, eval_times = c(3, 6, 12, 36, 60),
                                  do_ibs = TRUE,
                                  time_col = "death_time_from_disch_m",
                                  event_col = "death_event") {
  eval_times <- sort(unique(as.numeric(eval_times)))
  pr <- predict_death_pooled(fits, dat, eval_times)
  tt <- as.numeric(dat[[time_col]]); ee <- to01(dat[[event_col]])
  tmax <- max(tt, na.rm = TRUE)

  c_global <- .dual_cox_safe_concordance(tt, ee, pr$lp)     # lp-based; see note above
  ibs_global <- if (do_ibs) ibs_ipcw_train(dat, dat, pr$surv_mat, eval_times, time_col, event_col) else NA_real_
  c_h <- ibs_h <- stats::setNames(rep(NA_real_, length(eval_times)), as.character(eval_times))
  for (ii in seq_along(eval_times)) {
    h <- eval_times[ii]
    if (h < tmax) {
      c_h[ii] <- .dual_cox_safe_concordance(tt, ee, pr$risk_mat[, ii], ymax = h)
      if (do_ibs) {
        keep <- eval_times <= h
        if (sum(keep) >= 2L)
          ibs_h[ii] <- ibs_ipcw_train(dat, dat, pr$surv_mat[, keep, drop = FALSE],
                                      eval_times[keep], time_col, event_col)
      }
    }
  }
  rows <- rbind(
    data.frame(Metric = "Uno's C-Index", Time = "Global", Value = c_global),
    data.frame(Metric = "IBS",           Time = "Global", Value = ibs_global),
    data.frame(Metric = "Uno's C-Index", Time = names(c_h),   Value = unname(c_h)),
    data.frame(Metric = "IBS",           Time = names(ibs_h), Value = unname(ibs_h)))
  list(summary = rows, lp = pr$lp, surv_mat = pr$surv_mat, eval_times = eval_times,
       n = nrow(dat), n_events = sum(ee == 1L, na.rm = TRUE))
}

# ---- 6. Calibration (KM observed; readmission/admin as censoring) ------------
calibrate_extval_death <- function(fits, dat, times = c(6, 12, 36),
                                   g = 10L, min_bin_n = 20L, span = 0.75,
                                   time_col = "death_time_from_disch_m",
                                   event_col = "death_event") {
  times <- sort(unique(as.numeric(times)))
  pr <- predict_death_pooled(fits, dat, times)
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
  out$risk <- "death"; out$method <- "frozen Cox (dev-fit, MI-pooled) + Kaplan-Meier, external"
  out
}

# ---- 7. Bootstrap CIs (resample patients; model fixed) -----------------------
bootstrap_extval_death <- function(fits, dat, eval_times = c(3, 6, 12, 36),
                                   cal_times = c(6, 12, 36), B = 1000L, seed = 2125L,
                                   do_ibs = TRUE, do_cal = TRUE, parallel = TRUE,
                                   time_col = "death_time_from_disch_m",
                                   event_col = "death_event", verbose = TRUE) {
  eval_times <- sort(unique(as.numeric(eval_times)))
  cal_times  <- sort(unique(as.numeric(cal_times)))
  all_times  <- sort(unique(c(eval_times, cal_times)))
  pr <- predict_death_pooled(fits, dat, all_times)
  lp <- pr$lp; surv_all <- pr$surv_mat; risk_all <- pr$risk_mat
  tt <- as.numeric(dat[[time_col]]); ee <- to01(dat[[event_col]]); n <- length(tt)

  one_rep <- function(bi) {
    t <- tt[bi]; e <- ee[bi]; s <- lp[bi]
    sv <- surv_all[bi, , drop = FALSE]; rv <- risk_all[bi, , drop = FALSE]
    tmax <- max(t, na.rm = TRUE)
    cg <- .dual_cox_safe_concordance(t, e, s)     # Global: lp-based, see evaluate_extval_death() note
    ig <- if (do_ibs) ibs_ipcw_train(data.frame(t = t, e = e), data.frame(t = t, e = e),
            sv[, match(eval_times, all_times), drop = FALSE], eval_times, "t", "e") else NA_real_
    # Per-horizon: absolute-risk (1-S(h)) ranking, matching evaluate_extval_death().
    ch <- vapply(eval_times, function(h) {
      j <- match(h, all_times)
      if (h < tmax) .dual_cox_safe_concordance(t, e, rv[, j], ymax = h) else NA_real_
    }, numeric(1))
    ici <- if (do_cal) vapply(cal_times, function(h) {
      j <- match(h, all_times); risk <- 1 - sv[, j]
      obs_overall <- .vhm_km_risk(t, e, h)
      res <- tryCatch(.vhm_calibrate_one(risk, h, obs_overall,
        observed_by_bin_fun = function(idx) .vhm_km_risk(t[idx], e[idx], h),
        g = 10L, min_bin_n = 20L, span = 0.75), error = function(err) NULL)
      if (is.null(res)) NA_real_ else res$metrics$ici
    }, numeric(1)) else stats::setNames(rep(NA_real_, length(cal_times)), NULL)
    eo <- if (do_cal) vapply(cal_times, function(h) {
      j <- match(h, all_times); mp <- mean(1 - sv[, j], na.rm = TRUE)
      ob <- .vhm_km_risk(t, e, h); if (is.finite(ob) && ob > 0) mp / ob else NA_real_
    }, numeric(1)) else stats::setNames(rep(NA_real_, length(cal_times)), NULL)
    c(c_global = cg, ibs_global = ig,
      stats::setNames(ch, paste0("c_", eval_times)),
      stats::setNames(ici, paste0("ici_", cal_times)),
      stats::setNames(eo,  paste0("eo_",  cal_times)))
  }

  set.seed(seed)
  idx <- lapply(seq_len(B), function(i) sample.int(n, n, replace = TRUE))
  reps <- if (parallel && requireNamespace("future.apply", quietly = TRUE))
    future.apply::future_lapply(idx, one_rep, future.seed = TRUE) else lapply(idx, one_rep)
  M <- do.call(rbind, reps)
  qlo <- apply(M, 2, quantile, 0.025, na.rm = TRUE, names = FALSE)
  qhi <- apply(M, 2, quantile, 0.975, na.rm = TRUE, names = FALSE)
  data.frame(metric = colnames(M), lo = qlo, hi = qhi, row.names = NULL)
}

# ---- 8. Propensity common-support matching to the development cohort ----------
# Keep the external rows whose membership-model propensity falls within the
# development support (common-support trimming). dev_ref is one dummified dev
# imputation; ext is a dummified external cohort. Returns the kept external rows.
match_common_support <- function(dev_ref, ext, covars = .EVD_DEATH_MATCH_COVARS,
                                 trim = 0.0, seed = 2125L, verbose = TRUE) {
  covars <- intersect(covars, intersect(names(dev_ref), names(ext)))
  d <- rbind(
    data.frame(.member = 0L, dev_ref[, covars, drop = FALSE]),
    data.frame(.member = 1L, ext[,     covars, drop = FALSE]))
  d[covars] <- lapply(d[covars], function(z) as.numeric(z))
  fml <- stats::as.formula(paste(".member ~", paste(covars, collapse = " + ")))
  ps_fit <- suppressWarnings(stats::glm(fml, data = d, family = stats::binomial()))
  ps <- stats::predict(ps_fit, type = "response")
  ps_dev <- ps[d$.member == 0L]; ps_ext <- ps[d$.member == 1L]
  lo <- stats::quantile(ps_dev, trim, names = FALSE, na.rm = TRUE)
  hi <- stats::quantile(ps_dev, 1 - trim, names = FALSE, na.rm = TRUE)
  keep <- ps_ext >= lo & ps_ext <= hi
  if (verbose) cat(sprintf("Common-support match: dev PS support [%.3f, %.3f]; kept %d of %d external (%.1f%%)\n",
                           lo, hi, sum(keep), length(keep), 100 * mean(keep)))
  list(keep = keep, ps_ext = ps_ext, ps_dev = ps_dev, support = c(lo, hi), covars = covars)
}

# Greedy 1:1 nearest-neighbour matching WITHOUT replacement on the logit propensity
# score, with a caliper. lt/lc are logit-PS of treated (external) / control (dev),
# both ascending-sortable. Returns a logical 'keep' over the external (treated) rows:
# an external unit is kept iff it can be paired to a still-unused dev unit within the
# caliper. This trims the external tail that has no close development counterpart.
.evd_nn_match <- function(lt, lc, caliper) {
  oc <- order(lc); lcs <- lc[oc]; nc <- length(lcs)
  used <- logical(nc); keep <- logical(length(lt))
  for (i in order(lt)) {                       # deterministic order (ascending logit-PS)
    p <- findInterval(lt[i], lcs)
    best <- NA_integer_; bestd <- Inf
    lo <- p                                     # scan left to first unused within caliper
    while (lo >= 1L) { dlt <- lt[i] - lcs[lo]; if (dlt > caliper) break
      if (!used[lo]) { best <- lo; bestd <- dlt; break }; lo <- lo - 1L }
    hi <- p + 1L                                # scan right to first unused within caliper
    while (hi <= nc) { drt <- lcs[hi] - lt[i]; if (drt > caliper) break
      if (!used[hi]) { if (drt < bestd) { best <- hi; bestd <- drt }; break }; hi <- hi + 1L }
    if (!is.na(best)) { keep[i] <- TRUE; used[best] <- TRUE }
  }
  keep
}

# 1:1 nearest-neighbour propensity match of the external cohort to the development
# cohort (without replacement, caliper = caliper_sd x SD of the pooled logit-PS).
# Returns the kept external rows; this is the stricter complement to the
# common-support trim and is meant to reduce residual covariate imbalance.
match_nn_1to1 <- function(dev_ref, ext, covars = .EVD_DEATH_MATCH_COVARS,
                          caliper_sd = 0.2, verbose = TRUE) {
  covars <- intersect(covars, intersect(names(dev_ref), names(ext)))
  d <- rbind(data.frame(.member = 0L, dev_ref[, covars, drop = FALSE]),
             data.frame(.member = 1L, ext[,     covars, drop = FALSE]))
  d[covars] <- lapply(d[covars], function(z) as.numeric(z))
  fml <- stats::as.formula(paste(".member ~", paste(covars, collapse = " + ")))
  ps_fit <- suppressWarnings(stats::glm(fml, data = d, family = stats::binomial()))
  lp <- stats::predict(ps_fit, type = "link")
  lt <- lp[d$.member == 1L]; lc <- lp[d$.member == 0L]
  caliper <- caliper_sd * stats::sd(lp)
  keep <- .evd_nn_match(lt, lc, caliper)
  if (verbose) cat(sprintf("1:1 NN match (caliper %.2f x SD logit-PS): kept %d of %d external (%.1f%%)\n",
                           caliper_sd, sum(keep), length(keep), 100 * mean(keep)))
  list(keep = keep, caliper = caliper, n_matched = sum(keep), covars = covars)
}

# Feasibility of a case-mix-BALANCED external subsample. Standardizing the external
# cohort to the development (convenio 1) covariate distribution uses ATT-type IPW
# weights w = (1 - e)/e on the membership propensity e = P(external | X). The
# effective sample size ESS = (sum w)^2 / sum(w^2) and the effective number of events
# (ESS x weighted event rate) show whether a balanced comparison is even powered:
# when the populations differ strongly (e.g. age/sex), ESS collapses and any balanced
# subset/weight scheme (cardinality matching, CEM, IPW) is uninformative. base-R only.
matching_feasibility <- function(dev_ref, ext, covars = .EVD_DEATH_MATCH_COVARS,
                                 event_col = "death_event", verbose = TRUE) {
  covars <- intersect(covars, intersect(names(dev_ref), names(ext)))
  d <- rbind(data.frame(.member = 0L, dev_ref[, covars, drop = FALSE]),
             data.frame(.member = 1L, ext[,     covars, drop = FALSE]))
  d[covars] <- lapply(d[covars], function(z) as.numeric(z))
  ps <- suppressWarnings(stats::predict(
    stats::glm(stats::reformulate(covars, ".member"), data = d, family = stats::binomial()),
    type = "response"))
  e <- pmin(pmax(ps[d$.member == 1L], 1e-6), 1 - 1e-6)
  w <- (1 - e) / e                                   # ATT: external -> development
  ess <- sum(w)^2 / sum(w^2)
  ev <- to01(ext[[event_col]])
  eff_events <- ess * stats::weighted.mean(ev, w)
  # max IPW-weighted SMD vs development across the covariates (residual imbalance)
  wsmd <- vapply(covars, function(v) {
    a <- as.numeric(dev_ref[[v]]); b <- as.numeric(ext[[v]])
    mb <- stats::weighted.mean(b, w); s <- sqrt((stats::var(a) + sum(w * (b - mb)^2) / sum(w)) / 2)
    if (!is.finite(s) || s == 0) 0 else abs(mean(a) - mb) / s
  }, numeric(1))
  out <- list(n = nrow(ext), events = sum(ev), ess = ess, eff_events = eff_events,
              max_weighted_smd = max(wsmd), covars = covars)
  if (verbose) cat(sprintf("Balanced-matching feasibility: n=%d (%d events) -> IPW ESS=%.0f, effective events=%.1f, max weighted SMD=%.2f\n",
                           out$n, out$events, out$ess, out$eff_events, out$max_weighted_smd))
  out
}

# ---- 9. Orchestrator ---------------------------------------------------------
run_extval_death <- function(eval_times = c(3, 6, 12, 36), cal_times = c(6, 12, 36),
                             B = 1000L, seed = 2125L, parallel = TRUE,
                             save = TRUE, verbose = TRUE) {
  source(file.path(project_root, "cons/_alt_scripts/extval_death_01_build_cohort.R"))
  source(file.path(project_root, "cons/_alt_scripts/extval_death_02_table1_smd.R"))
  dev_list <- build_dev_full(verbose = verbose)
  fits     <- fit_death_dev(dev_list, verbose = verbose)
  dev_ref  <- as.data.frame(dev_list[[1]])

  run_one <- function(which, do_ibs, do_cal) {
    if (verbose) cat("\n=================", which, "=================\n")
    built <- build_extval_death_cohort(which, verbose = verbose)
    dat   <- dummify_extval_death(built$nondum, complete_case = TRUE, verbose = verbose)
    nondum_cc <- built$nondum[attr(dat, "cc_index"), , drop = FALSE]   # aligned to dat rows
    ev    <- evaluate_extval_death(fits, dat, eval_times, do_ibs = do_ibs)
    boot  <- bootstrap_extval_death(fits, dat, eval_times, cal_times, B = B, seed = seed,
                                    do_ibs = do_ibs, do_cal = do_cal, parallel = parallel, verbose = verbose)
    cal   <- if (do_cal) calibrate_extval_death(fits, dat, cal_times) else NULL
    # matched subsample A: common-support PS trim
    mm    <- match_common_support(dev_ref, dat, verbose = verbose)
    datm  <- dat[mm$keep, , drop = FALSE]
    nondum_matched <- nondum_cc[mm$keep, , drop = FALSE]
    evm   <- evaluate_extval_death(fits, datm, eval_times, do_ibs = do_ibs)
    bootm <- bootstrap_extval_death(fits, datm, eval_times, cal_times, B = B, seed = seed,
                                    do_ibs = do_ibs, do_cal = do_cal, parallel = parallel, verbose = verbose)
    calm  <- if (do_cal) calibrate_extval_death(fits, datm, cal_times) else NULL
    # matched subsample B: 1:1 nearest-neighbour PS match (without replacement, caliper)
    nn    <- match_nn_1to1(dev_ref, dat, verbose = verbose)
    datnn <- dat[nn$keep, , drop = FALSE]
    nondum_nn <- nondum_cc[nn$keep, , drop = FALSE]
    evnn  <- evaluate_extval_death(fits, datnn, eval_times, do_ibs = do_ibs)
    bootnn <- bootstrap_extval_death(fits, datnn, eval_times, cal_times, B = B, seed = seed,
                                     do_ibs = do_ibs, do_cal = do_cal, parallel = parallel, verbose = verbose)
    calnn <- if (do_cal) calibrate_extval_death(fits, datnn, cal_times) else NULL
    # feasibility of a case-mix-balanced subsample (IPW ESS / effective events)
    feas  <- matching_feasibility(dev_ref, dat, verbose = verbose)
    list(convenio = which, flow = built$flow, n = ev$n, n_events = ev$n_events,
         eval = ev, boot = boot, cal = cal, feasibility = feas,
         nondum_cc = nondum_cc, nondum_matched = nondum_matched, nondum_nn = nondum_nn,
         match = list(info = mm, n = evm$n, n_events = evm$n_events,
                      eval = evm, boot = bootm, cal = calm),
         nn = list(info = nn, n = evnn$n, n_events = evnn$n_events,
                   eval = evnn, boot = bootnn, cal = calnn))
  }

  res_c3 <- run_one("C3", do_ibs = TRUE,  do_cal = TRUE)   # 82 deaths: full
  res_c5 <- run_one("C5", do_ibs = FALSE, do_cal = FALSE)  # ~16-21 deaths: discrimination only

  # Development-vs-external SMD table (full + both matched variants), reusing the
  # convenio-1 reference values. CS = common-support trim; NN = 1:1 nearest neighbour.
  smd <- extval_death_smd(list(
    "C3" = res_c3$nondum_cc, "C3 (CS)" = res_c3$nondum_matched, "C3 (1:1 NN)" = res_c3$nondum_nn,
    "C5" = res_c5$nondum_cc, "C5 (CS)" = res_c5$nondum_matched, "C5 (1:1 NN)" = res_c5$nondum_nn))

  out <- list(C3 = res_c3, C5 = res_c5, smd = smd, eval_times = eval_times, cal_times = cal_times,
              model = "frozen formula_shap_death_rule2 (best_perf2), dev-fit MI-pooled, applied to C3/C5",
              created = as.character(Sys.time()))
  if (save) {
    out_dir <- file.path(.EVD_DATA_DIR, "pred27"); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(out_dir, paste0("extval_death_", format(Sys.Date(), "%Y_%m_%d"), ".rds"))
    saveRDS(out, f); if (verbose) cat("\nSaved:", f, "\n")
  }
  out
}
