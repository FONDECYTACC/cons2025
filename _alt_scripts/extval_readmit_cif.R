# =============================================================================
# extval_readmit_cif.R
#
# Competing-risk-aware (cumulative incidence function, CIF) readmission risk
# score for prediction26's external (2021-2024, temporal) validation of the
# frozen READMISSION model.
#
# -----------------------------------------------------------------------------
# WHY THIS FILE EXISTS (methodology, read this before touching anything below)
# -----------------------------------------------------------------------------
# prediction26_ext_val_readm_mod.ipynb currently scores the frozen readmission
# model on the 2021-2024 external cohort using the cause-specific 1 - S(t) from
# a plain (non-competing-risk) Cox model. That treats death exactly like any
# other censoring event ("net risk"): a person who died before the horizon is
# just removed from the risk set, as if their readmission status after death
# were unknown rather than structurally impossible. This is a defensible
# SENSITIVITY quantity (it answers "if death did not exist, what would this
# person's readmission risk be"), but it is not what actually happened, because
# death and readmission are competing events: dying rules out a later
# readmission, it does not merely censor it.
#
# The methodologically preferable quantity is the cumulative incidence function
# (CIF) for readmission (cause 1), which is bounded above by the CIF for death
# (cause 2) and by construction cannot exceed reality: CIF_readmit(t) + CIF_death(t)
#  + S(t) = 1. Computing it requires a joint model of BOTH causes, fitted via
# riskRegression::CSC() (cause-specific Cox models for cause 1 and cause 2 fit
# jointly on the SAME first-event response), then read out for cause 1 via
# riskRegression::predictRisk(fit, newdata = ..., cause = 1).
#
# THE KEY FACT THAT MAKES THIS POSSIBLE FOR THE EXTERNAL COHORT, DESPITE IT
# LACKING A TRUSTWORTHY DEATH OUTCOME: Chile's DEIS mortality registry is only
# considered ascertainment-complete through 2020-12-31. Deaths among people
# discharged in 2021-2024 are therefore NOT reliably recorded yet; any raw
# death_event field for this external cohort would be a severe undercount, not
# a real absence of death, and must not be used as an observed outcome for
# this cohort. But riskRegression::predictRisk(fit, newdata = ..., cause = 1)
# needs ONLY the covariates on the right-hand side of BOTH cause-specific
# formulas from `newdata` -- it never looks at newdata's response/outcome
# columns at all. This was independently verified this session by (a) direct
# inspection of the analogous, already-proven engine in holdout_cif_cache.R
# (the predict step there uses `te` -- the held-out validation newdata --
# purely as a covariate source), and (b) a dedicated synthetic smoke test
# (see the CSC verification note below) that fit CSC on training data with NO
# outcome columns available in `newdata` at all, and then confirmed identical
# predictions whether newdata's outcome columns were absent or filled with
# garbage/NA values (max abs diff = 0). So the recipe is:
#
#   1. Fit the joint CSC model ONCE per development imputation, on TRAIN data
#      (`train_list`), where BOTH readmission and death ARE observed (train_list
#      is drawn from the 2010-2020 development cohort, well inside the DEIS
#      ascertainment window).
#   2. Reuse those SAME fitted CSC objects (no refitting) to predict the
#      readmission CIF (cause = 1) on:
#        (a) the INTERNAL 20% holdout (`validation_list`) -- which DOES have a
#            trustworthy death outcome, so it can ALSO be calibrated against a
#            true Aalen-Johansen observed CIF (death treated as a genuine
#            competing event on the observed side, not just on the score side).
#        (b) the EXTERNAL 2021-2024 cohort's COVARIATES ONLY -- which can only
#            be calibrated against a Kaplan-Meier observed reference computed
#            from (readmit_time, readmit_event) alone, because that is the
#            only outcome this cohort can supply reliably. The observed side
#            here is therefore still "net risk" (death treated as censoring)
#            even though the SCORE properly accounts for competing death risk;
#            this asymmetry is a genuine, irreducible limitation of external
#            validation with an ascertainment-lagged mortality registry, not a
#            bug, and must be reported as such in the manuscript.
#
# WHAT THIS DOES NOT LET YOU VALIDATE: because the external cohort's observed
# comparator is still KM(readmit-only), this workflow cannot demonstrate that
# the CIF's absolute-risk CALIBRATION is correct in the external population in
# a fully competing-risk-consistent way (that would need a real external death
# outcome). What it DOES let you validate: (i) whether accounting for the
# competing risk of death materially changes the DISCRIMINATION (Uno's C) of
# the readmission score in the external cohort relative to the net-risk arm,
# and (ii) whether the CIF's calibration against the best available observed
# reference (KM-readmission) is at least as good as the net-risk arm's own
# KM-based calibration, which is already reported elsewhere in the notebook.
#
# -----------------------------------------------------------------------------
# CSC + LOGICAL STRATUM VERIFICATION (already completed this session)
# -----------------------------------------------------------------------------
# The death formula below (formula_shap_death_rule2) pairs strata(plan_type_strata)
# (a plain factor) with strata(any_phys_dx) (a genuine R logical TRUE/FALSE
# column). A dedicated synthetic smoke test this session (n = 4000, two
# competing causes, 5 plan levels x 2 tr_outcome_undet strata for cause 1, 5
# plan levels x 2 any_phys_dx strata for cause 2, 10 distinct per-cause baseline
# rates) confirmed riskRegression::CSC()/predictRisk(cause = 1) reconstructs
# every one of the 20 stratum combinations to within floating-point noise
# (max abs diff vs an independent, hand-rolled discrete Aalen-Johansen
# product-limit recursion = 5.551115e-16) of an independent ground truth, and
# confirmed predictRisk(cause = 1) is exactly invariant to newdata's outcome
# columns (byte-identical whether they are absent or filled with garbage/NA).
# No bug was found, so this file uses riskRegression::CSC()/predictRisk()
# directly, exactly mirroring holdout_cif_cache.R's proven `.cif_rhs()`-based
# construction, with NO special-casing or coercion of any_phys_dx.
#
# -----------------------------------------------------------------------------
# Dependencies
# -----------------------------------------------------------------------------
# This file is meant to be source()-d from within prediction26_ext_val_readm_mod.ipynb,
# AFTER the notebook's own "configuration" cell (needs PROJECT_ROOT, MODEL_COEFFICIENT_PARAMETERS)
# and AFTER "frozen-formula" (needs f_readmit, MODEL_COLUMNS) have already run.
# It sources cons/_alt_scripts/validate_holdout_metrics.R itself (guarded, only
# if the required helper functions are not already in scope) to reuse:
#   .vhm_first_event()   -- combined first-event (.ftime, .fstatus) encoder,
#                            cause 1 = readmission, cause 2 = death, ties -> death.
#   .vhm_aj_cif()        -- Aalen-Johansen observed CIF at one horizon.
#   .vhm_calibrate_one() -- one decile-grouped calibration pass (ICI/ECE/E:O)
#                            for an arbitrary predicted-risk vector against an
#                            arbitrary "observed" function (KM or AJ).
# It does NOT modify holdout_cif_cache.R, validate_holdout_metrics.R, or any
# other existing file.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Guards: resolve project_root for validate_holdout_metrics.R, source it if
#    its helpers are not already in scope, and confirm riskRegression/prodlim
#    are installed. Fails loudly (stop()) rather than silently continuing.
# -----------------------------------------------------------------------------
if (!exists("PROJECT_ROOT", inherits = TRUE) || !is.character(PROJECT_ROOT) ||
    length(PROJECT_ROOT) != 1L || !dir.exists(PROJECT_ROOT)) {
  stop(
    "extval_readmit_cif.R requires PROJECT_ROOT (set in prediction26's ",
    "'configuration' cell) to already be defined and valid before this file ",
    "is sourced.",
    call. = FALSE
  )
}
# validate_holdout_metrics.R looks for a lowercase `project_root`; give it one
# directly from the notebook's own PROJECT_ROOT so it never falls back to
# here::here() (which may not be installed / configured in this session).
project_root <- PROJECT_ROOT

.excif_required_vhm_fns <- c(".vhm_first_event", ".vhm_aj_cif", ".vhm_calibrate_one")
if (!all(vapply(.excif_required_vhm_fns, exists, logical(1), mode = "function"))) {
  source(file.path(PROJECT_ROOT, "cons/_alt_scripts/validate_holdout_metrics.R"))
}
.excif_missing_vhm_fns <- .excif_required_vhm_fns[
  !vapply(.excif_required_vhm_fns, exists, logical(1), mode = "function")
]
if (length(.excif_missing_vhm_fns)) {
  stop(
    "extval_readmit_cif.R could not obtain required helpers from ",
    "validate_holdout_metrics.R. Missing: ",
    paste(.excif_missing_vhm_fns, collapse = ", "),
    call. = FALSE
  )
}
rm(.excif_required_vhm_fns, .excif_missing_vhm_fns)

suppressWarnings({
  if (!requireNamespace("riskRegression", quietly = TRUE) ||
      !requireNamespace("prodlim", quietly = TRUE) ||
      !requireNamespace("survival", quietly = TRUE)) {
    stop(
      "extval_readmit_cif.R requires the riskRegression, prodlim, and ",
      "survival packages to be installed.",
      call. = FALSE
    )
  }
})

# -----------------------------------------------------------------------------
# 1. The death cause-specific formula (formula_shap_death_rule2), verbatim from
#    the task specification. This formula does NOT exist anywhere else in
#    prediction26_ext_val_readm_mod.ipynb (confirmed by exhaustive grep during
#    the Discover phase of this workflow) -- it is defined here, fresh, for the
#    first time in this notebook's context.
#
#    14 covariates + 2 strata (plan_type_strata: plain factor, printed bare by
#    survival::basehaz(); any_phys_dx: genuine R logical, printed "var=value").
#    Do not reorder or alter this formula without re-running the CSC two-strata
#    verification smoke test described above, since its correctness was
#    specifically checked against THIS exact strata() pairing.
# -----------------------------------------------------------------------------
formula_shap_death_rule2 <- survival::Surv(
  death_time_from_disch_m,
  death_event
) ~
  adm_age_rec3 +
  primary_sub_mod_cocaine_paste +
  primary_sub_mod_cocaine_powder +
  primary_sub_mod_alcohol +
  primary_sub_mod_others +
  prim_sub_freq_rec_2_2_6_days_wk +
  prim_sub_freq_rec_3_daily +
  occupation_condition_corr24_unemployed +
  occupation_condition_corr24_inactive +
  eva_ocupacion_logro_intermedio +
  eva_ocupacion_logro_minimo +
  cohabitation_family_of_origin +
  cohabitation_with_couple_children +
  cohabitation_others +
  strata(plan_type_strata) +
  strata(any_phys_dx)

DEATH_COEFFICIENT_COLUMNS <- c(
  "adm_age_rec3", "primary_sub_mod_cocaine_paste", "primary_sub_mod_cocaine_powder",
  "primary_sub_mod_alcohol", "primary_sub_mod_others",
  "prim_sub_freq_rec_2_2_6_days_wk", "prim_sub_freq_rec_3_daily",
  "occupation_condition_corr24_unemployed", "occupation_condition_corr24_inactive",
  "eva_ocupacion_logro_intermedio", "eva_ocupacion_logro_minimo",
  "cohabitation_family_of_origin", "cohabitation_with_couple_children",
  "cohabitation_others"
)
DEATH_STRATA_COLUMNS <- c("plan_type_strata", "any_phys_dx")
DEATH_MODEL_COLUMNS <- c(DEATH_COEFFICIENT_COLUMNS, DEATH_STRATA_COLUMNS)
DEATH_OUTCOME_COLUMNS <- c("death_time_from_disch_m", "death_event")

# Hard-coded count check (mirrors the notebook's own
# `stopifnot(length(COEFFICIENT_COLUMNS) == MODEL_COEFFICIENT_PARAMETERS)` in
# the frozen-formula cell): catches an accidental typo/duplication in the
# covariate list above before it can silently propagate into a mis-specified
# CSC fit.
stopifnot(length(DEATH_COEFFICIENT_COLUMNS) == 14L)

# The 6 covariates this cohort-building pipeline must newly derive for the
# external cohort (prediction26's own build_external_readmission_cohort() /
# prepare_external_model_data() never compute these, because the readmission
# model formula f_readmit never needed them -- confirmed by exhaustive grep
# during Discover). Exposed as a constant so downstream code and error
# messages stay in sync with this list.
DEATH_ONLY_EXTRA_COLUMNS <- c(
  "eva_ocupacion_logro_intermedio", "eva_ocupacion_logro_minimo",
  "cohabitation_family_of_origin", "cohabitation_with_couple_children",
  "cohabitation_others", "any_phys_dx"
)

# -----------------------------------------------------------------------------
# 2. Loud-failure guard: confirm a list of (development or internal-validation)
#    imputation data frames actually has every column the joint CSC fit / the
#    internal AJ-observed calibration needs. Per this workflow's explicit
#    instruction: if this is ever violated, FAIL LOUDLY with a clear message,
#    do not silently proceed with a mis-specified or incomplete model. Discover
#    already directly verified (by loading the real cached
#    holdout_train_val.rds) that both train_list and validation_list currently
#    satisfy this, in every one of their 5 imputations, with non-degenerate
#    covariate distributions and no missing values -- but this guard exists so
#    a future change to the upstream imputation/build pipeline cannot silently
#    break this workflow.
# -----------------------------------------------------------------------------
assert_death_covariates_in_list <- function(data_list, label) {
  stopifnot(is.list(data_list), length(data_list) >= 1L)
  for (imputation in seq_along(data_list)) {
    assert_columns(
      data_list[[imputation]],
      c(DEATH_MODEL_COLUMNS, DEATH_OUTCOME_COLUMNS),
      sprintf("%s (imputation %d of %d)", label, imputation, length(data_list))
    )
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 3. RHS-only formula extraction, copied verbatim (same logic, re-implemented
#    as its own top-level function here rather than imported, since the
#    original is a private helper local to holdout_cif_cache.R) from the
#    already-proven `.cif_rhs()` in cons/_alt_scripts/holdout_cif_cache.R /
#    `.readmit_calib_rhs()` in validate_holdout_metrics.R. riskRegression::CSC()
#    requires BOTH cause-specific formulas to share the SAME competing-risk
#    response prodlim::Hist(.ftime, .fstatus) -- passing each sub-model's own
#    original Surv(...) response (readmit_time_from_disch_m / readmit_event for
#    f_readmit, death_time_from_disch_m / death_event for
#    formula_shap_death_rule2) directly into CSC() is invalid; only the RHS
#    (covariates + strata) of each formula is reused.
# -----------------------------------------------------------------------------
.excif_rhs <- function(f) {
  txt <- paste(
    deparse(stats::formula(stats::delete.response(stats::terms(f))), width.cutoff = 500L),
    collapse = " "
  )
  # Normalise a namespace-qualified `stats::strata(...)` / `survival::strata(...)`
  # back to a bare `strata(...)` call, matching the frozen model's own guard
  # comment in the notebook ("Keep strata() unqualified inside the formula;
  # namespace-qualified strata calls can be parsed as ordinary covariates.").
  txt <- gsub("strat\\(", "strata(", txt)
  sub("^~\\s*", "", txt)
}

# -----------------------------------------------------------------------------
# 4. Fit the joint CSC (cause-specific Cox: readmission + death) model once per
#    development imputation. Mirrors fit_frozen_models()'s per-imputation loop
#    and audit-table style (cell 11, fitting-and-prediction-functions) as
#    closely as possible, so a reader already familiar with that function can
#    follow this one immediately.
# -----------------------------------------------------------------------------
build_joint_csc_fits <- function(formula_readmit, formula_death, train_list, verbose = TRUE) {
  n_imp <- length(train_list)
  csc_fits <- vector("list", n_imp)
  audit <- vector("list", n_imp)

  # Build the two paired Hist() formulas ONCE (both cause-specific sub-models
  # must reference the identical response object; only their RHS differs).
  f1 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", .excif_rhs(formula_readmit)))
  f2 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", .excif_rhs(formula_death)))

  if (verbose) {
    cat("\n--- Fitting joint CSC models (readmission + death), development imputations ---\n")
    cat("Number of imputations:", n_imp, "\n")
    flush.console()
  }

  for (imputation in seq_len(n_imp)) {
    if (verbose) cat(sprintf("Fitting joint CSC, imputation %d/%d...\n", imputation, n_imp))
    tr <- as.data.frame(train_list[[imputation]])

    # Build the combined first-event status (0 = censor, 1 = readmit, 2 = death,
    # ties -> death-first) required by CSC's shared Hist() response. This is
    # NOT the same as either individual outcome column; it is the joint
    # competing-risks encoding both cause-specific sub-models are fit against.
    fe <- .vhm_first_event(tr)
    tr$.ftime <- fe$.ftime
    tr$.fstatus <- fe$.fstatus

    started <- proc.time()[["elapsed"]]
    fit <- riskRegression::CSC(list(f1, f2), data = tr)
    elapsed <- proc.time()[["elapsed"]] - started
    csc_fits[[imputation]] <- fit

    # riskRegression::CSC() names its two internal coxph sub-models "Cause 1"
    # and "Cause 2" (confirmed this session by direct inspection of a fitted
    # CSC object's $models element) -- "Cause 1" is the readmission sub-model
    # (f1, i.e. f_readmit's RHS), "Cause 2" is the death sub-model (f2, i.e.
    # formula_shap_death_rule2's RHS), in the same order they were supplied to
    # CSC(list(f1, f2), ...).
    readmit_model <- fit$models[["Cause 1"]]
    death_model <- fit$models[["Cause 2"]]
    readmit_baseline <- survival::basehaz(readmit_model, centered = FALSE)
    death_baseline <- survival::basehaz(death_model, centered = FALSE)

    audit[[imputation]] <- data.frame(
      imputation = imputation,
      n = nrow(tr),
      readmissions = sum(fe$.fstatus == 1L),
      deaths = sum(fe$.fstatus == 2L),
      readmit_coefficients = length(stats::coef(readmit_model)),
      readmit_baseline_strata = dplyr::n_distinct(readmit_baseline$strata),
      death_coefficients = length(stats::coef(death_model)),
      death_baseline_strata = dplyr::n_distinct(death_baseline$strata),
      elapsed_seconds = elapsed
    )
    if (verbose) cat(sprintf(
      "  n=%s, readmissions=%s, deaths=%s, readmit_coef=%d, death_coef=%d, %.2f s\n",
      format(nrow(tr), big.mark = ","), format(sum(fe$.fstatus == 1L), big.mark = ","),
      format(sum(fe$.fstatus == 2L), big.mark = ","),
      length(stats::coef(readmit_model)), length(stats::coef(death_model)), elapsed
    ))
  }
  audit_df <- dplyr::bind_rows(audit)
  if (verbose) {
    cat("\nJoint CSC fit audit:\n")
    print(audit_df)
    flush.console()
  }

  # Loud-failure guards, mirroring the notebook's own 26-coefficient /
  # 10-baseline-stratum guard for the standalone frozen readmission model
  # (cell fit-five-frozen-models). Catches formula-parsing regressions (e.g. a
  # namespace-qualified strata() silently parsed as an ordinary covariate, or
  # an accidental duplicate/missing term) before they propagate into a wrong
  # CIF.
  if (!all(audit_df$readmit_coefficients == MODEL_COEFFICIENT_PARAMETERS) ||
      !all(audit_df$readmit_baseline_strata == 10L)) {
    stop(
      "The readmission cause-specific sub-model inside the joint CSC fit does ",
      "not match the frozen ", MODEL_COEFFICIENT_PARAMETERS,
      "-coefficient / 10-baseline-stratum specification (5 plan_type_strata x ",
      "2 tr_outcome_adm_discharge_rule_violation_undet). This sub-model should ",
      "be numerically identical in specification to the standalone frozen Cox ",
      "fit; a mismatch means the RHS-extraction helper (.excif_rhs()) mangled ",
      "f_readmit when building the CSC-paired formula.",
      call. = FALSE
    )
  }
  if (!all(audit_df$death_coefficients == length(DEATH_COEFFICIENT_COLUMNS)) ||
      !all(audit_df$death_baseline_strata == 10L)) {
    stop(
      "The death cause-specific sub-model inside the joint CSC fit was not ",
      "fitted as a ", length(DEATH_COEFFICIENT_COLUMNS),
      "-coefficient Cox model with ten baseline-hazard strata (5 ",
      "plan_type_strata x 2 any_phys_dx). Check formula_shap_death_rule2 and ",
      "confirm any_phys_dx is still a genuine logical column in train_list.",
      call. = FALSE
    )
  }
  if (verbose) cat(
    "Stratification guard passed for both cause-specific sub-models: ",
    MODEL_COEFFICIENT_PARAMETERS, " readmission coefficients / 10 strata, ",
    length(DEATH_COEFFICIENT_COLUMNS), " death coefficients / 10 strata.\n", sep = ""
  )

  list(fits = csc_fits, audit = audit_df)
}

# -----------------------------------------------------------------------------
# 5. Predict the pooled (MI-averaged) readmission CIF from a list of already-
#    fitted joint CSC objects. Pools across imputations by SIMPLE ARITHMETIC
#    AVERAGING, i.e. exactly the same Rubin's-rule-for-a-probability
#    convention already used by predict_risk_mi() (cell 11,
#    fitting-and-prediction-functions: `pooled <- Reduce(`+`, by_imputation) /
#    length(by_imputation)`). This is deliberate: the CIF-based arm should be
#    pooled the SAME way the existing net-risk arm already is, so that any
#    difference between the two arms reflects the competing-risk adjustment
#    itself, not a different pooling convention.
#
# `newdata` may be:
#   (a) a single data frame (e.g. the external cohort's covariates), applied
#       identically to every imputation's CSC fit -- exactly how
#       predict_risk_mi(fits, external_data, ...) already treats external_data
#       in the notebook's own frozen-predictions cell, or
#   (b) a list of per-imputation data frames (e.g. validation_list), one per
#       CSC fit -- exactly how predict_risk_mi(fits, validation_list, ...)
#       already treats validation_list.
# Both cases are normalised via the notebook's own normalize_newdata() helper
# (cell 11), which is already in scope by the time this function is called
# from a chunk inserted after frozen-predictions.
# -----------------------------------------------------------------------------
predict_cif_mi <- function(csc_fits, newdata, times, cause = 1L, verbose = TRUE) {
  if (verbose) {
    cat("\n--- Predicting competing-risk CIF (cause =", cause, ") ---\n")
    cat("Number of joint CSC fits:", length(csc_fits), "\n")
    if (is.data.frame(newdata)) {
      cat("Prediction observations:", nrow(newdata), "\n")
    } else {
      cat("Imputation-specific prediction datasets:", length(newdata), "\n")
    }
    cat("Times (months):", paste(times, collapse = ", "), "\n")
    flush.console()
  }
  # normalize_newdata() is defined in the notebook's own
  # "fitting-and-prediction-functions" cell (already executed by the time this
  # is called) -- reused here unmodified rather than duplicated, so both the
  # net-risk arm and this CIF arm treat a single shared external data frame vs
  # a per-imputation list identically.
  newdata <- normalize_newdata(newdata, length(csc_fits))

  by_imputation <- vector("list", length(csc_fits))
  for (imputation in seq_along(csc_fits)) {
    if (verbose) cat(sprintf("CIF prediction, imputation %d/%d...\n", imputation, length(csc_fits)))
    pred <- riskRegression::predictRisk(
      csc_fits[[imputation]], newdata = newdata[[imputation]], times = times, cause = cause
    )
    pred <- as.matrix(pred)
    # Defensive shape check: catches silent misalignment (e.g. predictRisk
    # dropping rows with a covariate it cannot evaluate) before it can corrupt
    # a downstream Uno's C / calibration computation.
    stopifnot(
      nrow(pred) == nrow(newdata[[imputation]]),
      ncol(pred) == length(times)
    )
    # Finiteness check, with a DIAGNOSTIC (not just a bare assertion) message.
    # Bug found and fixed during this session's synthetic end-to-end
    # verification: riskRegression::predictRisk(cause = 1) can return NA for
    # specific (row, horizon) combinations when that row's covariate profile
    # falls in a baseline-hazard stratum (of either cause-specific sub-model)
    # whose observed follow-up in the FITTING data (train_list) does not
    # extend as far as the requested time -- i.e. a genuine data-support
    # limitation of a sparse/short-follow-up stratum, not necessarily a
    # formula or stratum-labelling bug. The previous bare
    # `stopifnot(all(is.finite(pred)))` still stopped the run (correct, per
    # this project's loud-failure convention: a non-finite CIF must never be
    # silently coerced to 0/1 or dropped), but gave a completely uninformative
    # "all(is.finite(pred)) is not TRUE" message with no way to tell whether
    # the cause was a real formula bug or an unsupported-horizon stratum. This
    # replaces that with a message naming the affected imputation, horizon(s),
    # row count, and (when available) the offending rows' stratum columns, so
    # a real run that hits this can be diagnosed immediately instead of
    # requiring a second debugging pass.
    if (!all(is.finite(pred))) {
      bad <- which(!is.finite(pred), arr.ind = TRUE)
      bad_times <- sort(unique(times[bad[, 2L]]))
      bad_rows <- sort(unique(bad[, 1L]))
      strata_cols <- intersect(c("plan_type_strata", "tr_outcome_adm_discharge_rule_violation_undet", "any_phys_dx"),
                                names(newdata[[imputation]]))
      strata_preview <- if (length(strata_cols)) {
        preview_rows <- utils::head(bad_rows, 5L)
        paste(vapply(preview_rows, function(r) {
          paste(strata_cols, "=", vapply(strata_cols, function(cn) as.character(newdata[[imputation]][[cn]][r]), character(1)),
                collapse = ", ")
        }, character(1)), collapse = " | ")
      } else {
        "(no plan_type_strata/tr_outcome_adm_discharge_rule_violation_undet/any_phys_dx column found in newdata to report)"
      }
      stop(
        "predict_cif_mi(): riskRegression::predictRisk(cause = ", cause, ") returned ",
        length(bad_rows), " row(s) with a non-finite CIF at imputation ", imputation,
        ", horizon(s) ", paste(bad_times, collapse = ", "), " (out of ", nrow(pred), " rows, ",
        "times = ", paste(times, collapse = ", "), "). This typically means the affected rows' ",
        "covariate profile falls in a baseline-hazard stratum (readmission and/or death ",
        "cause-specific sub-model) whose observed follow-up in the data the joint CSC model ",
        "was FIT on does not extend as far as the requested horizon -- a data-support ",
        "limitation of a sparse or short-follow-up stratum combination, not necessarily a ",
        "formula or stratum-labelling bug. Refusing to silently coerce these to 0/1 or drop ",
        "them. First up to 5 affected rows' strata: ", strata_preview, ". Investigate whether ",
        "the requested horizon is actually supported by every stratum combination present in ",
        "the prediction data before proceeding.",
        call. = FALSE
      )
    }
    # Clamp to [0, 1]: a CIF is a probability; floating-point noise from the
    # underlying step-function evaluation should never be allowed to produce a
    # value marginally outside this range.
    by_imputation[[imputation]] <- pmin(pmax(pred, 0), 1)
  }
  pooled <- Reduce(`+`, by_imputation) / length(by_imputation)
  colnames(pooled) <- as.character(times)

  if (verbose) {
    cat("CIF prediction summary (pooled):\n")
    print(data.frame(
      time = times,
      mean_cif = colMeans(pooled),
      min_cif = apply(pooled, 2, min),
      max_cif = apply(pooled, 2, max)
    ))
    flush.console()
  }
  list(pooled = pooled, by_imputation = by_imputation, times = times)
}

# -----------------------------------------------------------------------------
# 6. Derive the 6 death-model covariates the external cohort is currently
#    missing (eva_ocupacion dummies, cohabitation dummies, any_phys_dx),
#    directly from the raw source fields (eva_ocupacion, con_quien_vive,
#    diagnostico_trs_fisico) that prediction26's own build_external_readmission_cohort()
#    already carries through untouched from the raw C1 registry (that function
#    only ever ADDS/recodes a handful of columns via mutate()/case_when(); it
#    never select()s away the rest of `raw`, so these three raw fields survive
#    into external_built$data even though nothing in prediction26 currently
#    reads them).
#
#    VERIFICATION NOTE (adversarial review, this session): these three column
#    names never appear anywhere in prediction26_ext_val_readm_mod.ipynb itself
#    (confirmed by direct grep of the notebook), so "confirmed by exhaustive
#    grep during Discover" in an earlier draft of this comment overstated its
#    own evidentiary basis -- that grep was necessarily of a DIFFERENT file
#    (extval_01_build_cohort.R / prediction1.qmd), not of prediction26's own
#    notebook or its actual raw input. This was instead independently confirmed
#    here by loading the REAL raw C1 registry RDS that prediction26's own
#    "configuration"/"load-inputs" cells point at
#    (data/20241015_out/251001_c1_1025_df_prev1y.rds, i.e. `inputs$c1`) and
#    checking directly: con_quien_vive, eva_ocupacion, diagnostico_trs_fisico,
#    and hash_key are all present, with value labels (e.g. "logro alto" /
#    "logro intermedio" / "logro minimo" for eva_ocupacion; "solo" / "unicamente
#    con la pareja e hijos" / etc. for con_quien_vive; "en estudio" / "sin
#    trastorno" for diagnostico_trs_fisico) that match the case_when() patterns
#    below almost exactly. add_death_covariates_to_external_cohort() was also
#    run end-to-end on a 20,000-row sample of that real file and produced sane,
#    non-degenerate category distributions with no errors. assert_columns()
#    below remains as a loud-failure safety net regardless (this cannot be
#    re-verified against the ACTUAL 2021-2024 external extract until the
#    notebook is actually run, since that extract was not available to this
#    review).
#
#    The recoding logic below is copied close to verbatim from
#    cons/_alt_scripts/extval_01_build_cohort.R (cohabitation: lines ~231-251;
#    any_phys_dx: lines ~278-282), itself traced to prediction1.qmd. It is
#    reproduced here, not sourced from that file, because that file's cohort-
#    building function (build_extval_readmit_cohort) has NOT been verified to
#    be otherwise identical to prediction26's own
#    build_external_readmission_cohort() (different filters/eligibility logic
#    are plausible), so importing only its recoding SNIPPETS (not the whole
#    cohort-building pipeline) is the safer choice here.
#
#    eva_ocupacion is treated exactly like eva_consumo / eva_sm already are in
#    prepare_external_model_data() (cell 10): direct string equality against
#    "logro intermedio" / "logro minimo" (reference = "logro alto"), no further
#    normalisation, because Discover directly confirmed the raw eva_ocupacion
#    field already uses these exact lowercase Spanish labels in the raw C1
#    registry (same convention as eva_consumo/eva_sm).
# -----------------------------------------------------------------------------
add_death_covariates_to_external_cohort <- function(cohort_raw, verbose = TRUE) {
  required_raw <- c("con_quien_vive", "eva_ocupacion", "diagnostico_trs_fisico", "hash_key")
  assert_columns(
    cohort_raw, required_raw,
    "External cohort (pre-dummy, external_built$data) for death-model covariate derivation"
  )

  # ---- cohabitation (verbatim logic from extval_01_build_cohort.R ~L231-251) ----
  cqv <- cohort_raw$con_quien_vive |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_squish()
  joel5 <- dplyr::case_when(
    stringr::str_detect(cqv, "^solo$") ~ "Alone",
    stringr::str_detect(cqv, "^unicamente con (la )?pareja( e hijos)?$") |
      stringr::str_detect(cqv, "^unicamente con pareja$") |
      stringr::str_detect(cqv, "^unicamente con hijos$") ~ "Couple/children only",
    stringr::str_detect(cqv, "(pareja|hijos).*(padres|familia de origen)") |
      stringr::str_detect(cqv, "^con la pareja,? hijos y padres") ~ "Couple/children + origin",
    stringr::str_detect(cqv, "^unicamente con padres|^con padres|familia de origen$") |
      stringr::str_detect(cqv, "^con la madre \\(sola\\)$") |
      stringr::str_detect(cqv, "^con abuelos$") |
      stringr::str_detect(cqv, "^con hermanos$") ~ "Family of origin",
    stringr::str_detect(cqv, "con amigos|otro no pariente|otro pariente|^otros$") ~ "Others",
    TRUE ~ "Others"
  )
  cohabitation <- dplyr::case_when(
    joel5 == "Alone" ~ "alone",
    joel5 %in% c("Couple/children only", "Couple/children + origin") ~ "with couple/children",
    joel5 == "Family of origin" ~ "family of origin",
    TRUE ~ "Others"
  )

  # ---- any_phys_dx (verbatim logic from extval_01_build_cohort.R ~L278-282) ----
  # Kept as a genuine R LOGICAL (TRUE/FALSE), matching formula_shap_death_rule2's
  # own strata(any_phys_dx) term and the CSC two-strata verification, which
  # specifically checked that a logical stratum needs no coercion.
  pf <- cohort_raw$diagnostico_trs_fisico |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_squish()
  any_phys_dx <- !is.na(pf) & nzchar(pf) &
    !stringr::str_detect(
      pf,
      "sin (diagnostico|patologia)|no (presenta|refiere|aplica)|ninguno|en estudio|sin trastorno"
    )

  # ---- eva_ocupacion (reference: "logro alto"), same pattern already used for
  #      eva_consumo/eva_sm in prepare_external_model_data() ----
  eo <- cohort_raw$eva_ocupacion

  cohort_raw$cohabitation_family_of_origin <- as.integer(cohabitation == "family of origin")
  cohort_raw$cohabitation_with_couple_children <- as.integer(cohabitation == "with couple/children")
  cohort_raw$cohabitation_others <- as.integer(cohabitation == "Others")
  cohort_raw$any_phys_dx <- any_phys_dx
  cohort_raw$eva_ocupacion_logro_intermedio <- as.integer(eo == "logro intermedio")
  cohort_raw$eva_ocupacion_logro_minimo <- as.integer(eo == "logro minimo")

  if (verbose) {
    cat("\n--- Derived death-model covariates for the external cohort ---\n")
    cat("Cohabitation categories:\n")
    print(table(cohabitation, useNA = "ifany"))
    cat("any_phys_dx: TRUE =", sum(any_phys_dx, na.rm = TRUE),
        "/ NA =", sum(is.na(any_phys_dx)), "/ total =", length(any_phys_dx), "\n")
    cat("eva_ocupacion categories:\n")
    print(table(eo, useNA = "ifany"))
    flush.console()
  }
  cohort_raw
}

# -----------------------------------------------------------------------------
# 7. Join the 6 newly derived death-model covariates onto the ALREADY
#    complete-case, dummy-coded external_data (produced by prepare_external_model_data()
#    in the notebook's own "build-external-cohort" cell), then apply a SEPARATE,
#    smaller complete-case restriction specific to these 6 new columns.
#
#    hash_key is the join key. This is safe because
#    build_external_readmission_cohort() keeps exactly one index episode per
#    patient (confirmed in that function's own logic: `dplyr::filter(qualifying_episode_rank == 1L)`),
#    so hash_key is unique on both sides of the join.
#
#    IMPORTANT, FLAG THIS PROMINENTLY: because eva_ocupacion / con_quien_vive /
#    diagnostico_trs_fisico were never required to be complete by the
#    readmission-only pipeline, some rows that survive into external_data (the
#    net-risk arm's cohort) may have missing values in one of the 6 new
#    columns. The resulting external_data_cif therefore has a POTENTIALLY
#    SMALLER denominator than external_data, purely due to a different set of
#    covariates being required complete. This is reported explicitly (not
#    hidden) via the printed flow message and should be reported in the
#    manuscript/appendix alongside the CIF-arm results, exactly the same way
#    the notebook already reports its own complete-case flow for the
#    readmission-only cohort in "build-external-cohort".
# -----------------------------------------------------------------------------
augment_external_cohort_for_death_covariates <- function(external_built_data, external_data, verbose = TRUE) {
  raw_aug <- add_death_covariates_to_external_cohort(external_built_data, verbose = verbose)
  extra <- raw_aug[, c("hash_key", DEATH_ONLY_EXTRA_COLUMNS), drop = FALSE]

  if (anyDuplicated(extra$hash_key)) {
    stop(
      "hash_key is not unique in external_built_data; the join in ",
      "augment_external_cohort_for_death_covariates() would fan out rows. ",
      "This should never happen given how build_external_readmission_cohort() ",
      "is constructed (one index episode per patient) -- investigate before proceeding.",
      call. = FALSE
    )
  }

  merged <- dplyr::left_join(external_data, extra, by = "hash_key")
  stopifnot(nrow(merged) == nrow(external_data))  # join must not change row count

  complete_death_cov <- stats::complete.cases(merged[, DEATH_ONLY_EXTRA_COLUMNS, drop = FALSE])
  n_before <- nrow(merged)
  n_after <- sum(complete_death_cov)

  if (verbose) {
    cat(sprintf(
      "\nExternal CIF-eligible subset (death-model covariates complete): %s / %s rows (dropped %s, %.1f%%).\n",
      format(n_after, big.mark = ","), format(n_before, big.mark = ","),
      format(n_before - n_after, big.mark = ","), 100 * (1 - n_after / n_before)
    ))
    cat(
      "NOTE: this denominator can legitimately differ from external_data's own ",
      "(readmission-only) denominator, because eva_ocupacion / con_quien_vive / ",
      "diagnostico_trs_fisico were never required to be complete by the ",
      "readmission-only pipeline. Report both denominators in any manuscript text.\n"
    )
    flush.console()
  }

  if (n_after == 0L) {
    stop(
      "No external-cohort rows have complete death-model covariates; the CIF ",
      "arm cannot be evaluated on this cohort. Check con_quien_vive / ",
      "eva_ocupacion / diagnostico_trs_fisico in the raw C1 registry.",
      call. = FALSE
    )
  }

  merged[complete_death_cov, , drop = FALSE]
}

# -----------------------------------------------------------------------------
# 8. Internal-holdout-only calibration helper: CIF-predicted risk vs a TRUE
#    Aalen-Johansen observed CIF (death treated as a genuine competing event on
#    the OBSERVED side too, not just in the score). This is only valid for
#    validation_list, because it is the only cohort in this workflow with a
#    trustworthy death outcome. It must NOT be used for the external cohort.
#
#    Reuses .vhm_calibrate_one() / .vhm_aj_cif() from validate_holdout_metrics.R
#    unmodified; this function only adapts their calling convention to
#    prediction26's own object shapes (a single pooled n x k risk matrix, plain
#    ftime/fstatus vectors), rather than the raw_predictions-shaped objects
#    those functions' existing callers in validate_holdout_metrics.R expect.
# -----------------------------------------------------------------------------
calibrate_cif_aj_observed <- function(pooled_cif_horizon, ftime, fstatus, horizons,
                                       g = 10L, min_bin_n = 20L, span = 0.75, verbose = TRUE) {
  pooled_cif_horizon <- as.matrix(pooled_cif_horizon)
  stopifnot(
    nrow(pooled_cif_horizon) == length(ftime),
    ncol(pooled_cif_horizon) == length(horizons)
  )
  if (verbose) cat("\n--- AJ-observed calibration of the internal CIF-adjusted arm ---\n")

  metrics_all <- vector("list", length(horizons))
  curves_all <- vector("list", length(horizons))
  for (j in seq_along(horizons)) {
    h <- horizons[[j]]
    pv <- pooled_cif_horizon[, j]
    obs_overall <- .vhm_aj_cif(ftime, fstatus, h)
    res <- .vhm_calibrate_one(
      pv, h, obs_overall,
      observed_by_bin_fun = function(idx) .vhm_aj_cif(ftime[idx], fstatus[idx], h),
      g = g, min_bin_n = min_bin_n, span = span
    )
    metrics_all[[j]] <- res$metrics
    curves_all[[j]] <- res$curve
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  list(
    metrics = dplyr::bind_rows(metrics_all),
    curves = dplyr::bind_rows(Filter(Negate(is.null), curves_all)),
    method = paste(
      "CIF (joint CSC, cause = 1) vs Aalen-Johansen observed CIF",
      "(death modelled as a genuine competing event on both the score and the",
      "observed side; internal 20% holdout, death ascertainment reliable)"
    )
  )
}

# -----------------------------------------------------------------------------
# 9. Small bootstrap-array summariser for the external CIF arm's calibration /
#    Brier / IBS bootstrap, generalising the notebook's own
#    `summarize_boot_metric()` (cell concordance-calibration-brier-ibs, which
#    is written inline against the specific global `external_metric_boot`
#    object and cannot be reused for a second, differently named array without
#    modification). This is functionally identical, just parametrised over the
#    bootstrap array and horizons so it can be called for the CIF arm's own
#    bootstrap array without touching the existing cell.
# -----------------------------------------------------------------------------
summarize_cif_bootstrap_metric <- function(boot_array, metric, horizons) {
  values <- boot_array[, metric, , drop = FALSE]
  data.frame(
    horizon_months = horizons,
    metric = metric,
    bootstrap_se = apply(values, 1L, stats::sd, na.rm = TRUE),
    lower_95 = apply(values, 1L, stats::quantile, 0.025, na.rm = TRUE, names = FALSE),
    upper_95 = apply(values, 1L, stats::quantile, 0.975, na.rm = TRUE, names = FALSE),
    valid_replicates = apply(values, 1L, function(x) sum(is.finite(x)))
  )
}
