# =====================================================================
# seleccion_predictores_regla_fija.R
# Pre-specified, DETERMINISTIC predictor-selection rule for the stratified
# Cox PH models (readmission and mortality), driven by the XGBoost/SHAP
# importance table (xgb9_*). No stepwise search; not order-dependent.
#
# CORE PRINCIPLE (reproducibility-first):
#   Predictors are chosen by how ROBUSTLY they rank across resamples, not by a
#   single SHAP snapshot. The backbone is a stability gate; the importance cut
#   is set PER OUTCOME according to the (reproducible) shape of its importance
#   distribution. Re-running the upstream pipeline in the future yields the same
#   predictors (stability selection; Meinshausen & Bohlmann, 2010).
#
# THE RULE, per outcome:
#   (1) Stability gate (robustness): keep features flagged stable
#         -> stable_freq_ge_90pct == TRUE  (top-45 in >=90% of resamples)
#   (2) Importance cut (one of two methods, chosen by distribution shape):
#         by = "rel"  : keep rel_to_max_pct >= rel_min
#                       (use when importance is CONCENTRATED around a dominant
#                        predictor -> "% of the top predictor" is meaningful; DEATH)
#         by = "rank" : keep rank_ci97p5 <= top_k  (robustly within the top-K via
#                       the upper 95% bootstrap rank CI)
#                       (use when importance is FLAT/diffuse and "% of max" is
#                        unstable because the maximum is not special; READMISSION)
#   (3) Complete factors: if ANY level/feature of a variable passes, the WHOLE
#       variable (all categories) enters. No predictor of the rule is missed.
#   (4) plan_type enters ONLY via strata() (excluded from covariates).
#
# At the recommended thresholds the two methods COINCIDE exactly on this data
# (readmission: rank_ci97p5<=20 == rel>=20% -> 20 features == original model;
#  death: rel>=10% == rank_ci97p5<=7 -> 7 features), so the per-outcome choice is
# about defensibility, not about changing the output.
#
# Collinearity (GVIF) and proportional hazards (Schoenfeld) are REPORTED, not
# enforced; they drop nothing unless drop_collinear = TRUE. A final dual Uno's C
# / IBS is reported for information. Calibration (ICI/ECE/E:O) is assessed on the
# FINAL models with prediction_metrics_complement, not during selection.
#
# Source the dual evaluator first (only needed for the optional confirmation):
#   source("cons/_alt_scripts/evaluate_dual_cox_python_style_boot.R")
# =====================================================================

suppressPackageStartupMessages({
  library(survival); library(readxl); library(dplyr)
})

# ----------------------------------------------------------------------
# 0) PRE-SPECIFIED CONFIG (override any field in the call: run_selection(..., field = value))
# ----------------------------------------------------------------------
CFG <- list(
  predictor_table   = file.path("cons", "_out",
                       "xgb9_dual_predictor_analysis_no_clinical_20260306_1821.xlsx"),
  predictor_sheet   = "Predictors",
  feature_col       = "feature",
  outcome_col       = "outcome",
  shap_col          = "mean_abs_shap_log_hazard",  # used only to order / print

  # --- the rule ---
  stability_col     = "stable_freq_ge_90pct",  # rule 1 (robustness): stable across resamples
  rel_col           = "rel_to_max_pct",        # used when method = "rel"
  rank_ci_col       = "rank_ci97p5",           # used when method = "rank" (upper 95% bootstrap rank CI)
  tier_col          = "importance_tier",       # fallback if rel_col is absent (drops "MINOR")
  rel_min           = 10,                       # default relative-importance threshold (% of max)
  top_k             = 20,                       # default robust top-K threshold

  # Per-outcome selection method, justified by the shape of each distribution:
  #   death   : CONCENTRATED (top1/top2 ratio ~1.9; natural break at ~10%) -> by="rel", rel_min=10
  #   readmit : FLAT/diffuse (top1/top2 ratio ~1.1; no dominant predictor)  -> by="rank", top_k=20
  # NOTE: to override, pass the WHOLE list, e.g.
  #   run_selection(..., selection = list(readmit=list(by="rank",top_k=20),
  #                                       death  =list(by="rel", rel_min=10)))
  selection = list(
    readmit = list(by = "rank", top_k  = 20),
    death   = list(by = "rel",  rel_min = 10)
  ),

  # --- strata structure ---
  strata_terms      = "strata(plan_type_strata)",  # string (both) or list(readmit=, death=)
  extra_strata      = list(),                       # per-outcome vars to ADD to strata (wrapped + excluded)
  exclude_prefixes  = c("plan_type"),               # represented ONLY via strata

  # --- diagnostics (do NOT discard by default) ---
  drop_collinear    = FALSE,   # TRUE = iteratively drop GVIF>gvif_max; FALSE = report only
  gvif_max          = 10,
  ph_alpha          = 0.05,

  # --- optional final confirmation (reporting only) ---
  confirm           = TRUE,
  k_folds           = 5,
  eval_times        = c(6, 12, 36, 60, 108),

  outcomes = list(
    readmit = list(tag = "Readmission",
                   time = "readmit_time_from_disch_m", event = "readmit_event"),
    death   = list(tag = "Death",
                   time = "death_time_from_disch_m",   event = "death_event")
  )
)

# Manual overrides (variable -> exact columns). Empty by default.
FAMILY_MAP <- list()

# Nominal multi-level variables whose categories MUST enter together as a complete
# factor (if any level passes the rule, all its sibling columns are included).
FAMILY_GROUPS <- c(
  "primary_sub_mod",               # primary substance: cocaine_paste / cocaine_powder / alcohol / others
  "tr_outcome",                    # treatment outcome
  "adm_motive",                    # admission motive
  "occupation_condition_corr24",   # occupation status
  "marital_status_rec",            # marital status
  "tenure_status_household",       # housing tenure
  "cohabitation",                  # living arrangement
  "urbanicity_cat"                 # urbanicity
  # ,"first_sub_used"   # left out: full family caused quasi-separation. Uncomment to force it.
)

# ----------------------------------------------------------------------
# 1) Per-outcome strata / exclusion helpers
# ----------------------------------------------------------------------
strata_terms_for <- function(key) {
  base <- CFG$strata_terms
  if (is.list(base)) base <- base[[key]]
  if (is.null(base)) base <- character(0)
  extra <- CFG$extra_strata[[key]]
  extra_terms <- if (!is.null(extra) && length(extra)) sprintf("strata(%s)", extra) else character(0)
  unique(c(base, extra_terms))
}
exclude_for <- function(key) unique(c(CFG$exclude_prefixes, CFG$extra_strata[[key]]))

# ----------------------------------------------------------------------
# 2) Column resolution: feature -> model columns (COMPLETE families)
# ----------------------------------------------------------------------
resolve_columns <- function(feature, data, exclude = CFG$exclude_prefixes) {
  if (length(exclude) && any(startsWith(feature, exclude))) return(character(0))      # via strata
  if (!is.null(FAMILY_MAP[[feature]])) return(intersect(FAMILY_MAP[[feature]], names(data)))
  # nominal multi-level family -> bring ALL its categories
  grp <- FAMILY_GROUPS[vapply(FAMILY_GROUPS, function(s) startsWith(feature, s), logical(1))]
  if (length(grp)) {
    stem <- grp[which.max(nchar(grp))]
    cols <- names(data)[startsWith(names(data), paste0(stem, "_"))]
    cols <- cols[!grepl("(_score|_any)$", cols)]
    if (length(exclude))
      cols <- cols[!vapply(cols, function(cc) any(startsWith(cc, exclude)), logical(1))]
    if (length(cols)) return(cols)
  }
  if (feature %in% names(data)) return(feature)                                       # binary/continuous/factor
  fam <- names(data)[startsWith(names(data), paste0(feature, "_"))]                   # ordinal dummies var_*
  fam <- fam[!grepl("(_score|_any)$", fam)]
  if (length(fam)) return(fam)
  warning(sprintf("Variable '%s' not found in data; skipped.", feature))
  character(0)
}

build_formula <- function(cols, time_col, event_col, strata = character(0)) {
  rhs <- paste(c(cols, strata), collapse = " + ")
  as.formula(sprintf("Surv(%s, %s) ~ %s", time_col, event_col, rhs))
}

verify_factor_completeness <- function(kept_cols, data) {
  for (s in FAMILY_GROUPS) {
    sib <- names(data)[startsWith(names(data), paste0(s, "_"))]
    sib <- sib[!grepl("(_score|_any)$", sib)]
    if (length(sib) && any(sib %in% kept_cols)) {
      miss <- setdiff(sib, kept_cols)
      if (length(miss))
        message(sprintf("[NOTE] family '%s' incomplete -> missing %s", s, paste(miss, collapse = ", ")))
    }
  }
}

to_logical <- function(x) x %in% c(TRUE, "TRUE", "True", "true", 1, "1")

# ----------------------------------------------------------------------
# 3) THE RULE: stability gate + per-outcome importance cut, full families
# ----------------------------------------------------------------------
select_by_rule <- function(oc, key, table, data, exclude, strata) {
  spec <- CFG$selection[[key]]
  if (is.null(spec)) spec <- list(by = "rel", rel_min = CFG$rel_min)  # safe default

  is_oc  <- table[[CFG$outcome_col]] == oc$tag
  stable <- to_logical(table[[CFG$stability_col]])                    # rule 1

  if (identical(spec$by, "rank")) {                                  # rule 2, method "rank"
    k  <- if (!is.null(spec$top_k)) spec$top_k else CFG$top_k
    v  <- suppressWarnings(as.numeric(table[[CFG$rank_ci_col]]))
    pass <- !is.na(v) & v <= k
    crit <- sprintf("stable & %s <= %g", CFG$rank_ci_col, k)
  } else {                                                           # rule 2, method "rel"
    rmin <- if (!is.null(spec$rel_min)) spec$rel_min else CFG$rel_min
    if (!is.null(table[[CFG$rel_col]])) {
      v <- suppressWarnings(as.numeric(table[[CFG$rel_col]])); pass <- !is.na(v) & v >= rmin
    } else {                                                         # fallback: drop MINOR tier
      pass <- !grepl("MINOR", table[[CFG$tier_col]])
    }
    crit <- sprintf("stable & %s >= %g", CFG$rel_col, rmin)
  }

  keep   <- is_oc & stable & pass
  passed <- table[keep, , drop = FALSE]
  passed <- passed[order(-suppressWarnings(as.numeric(passed[[CFG$shap_col]]))), , drop = FALSE]
  feats  <- as.character(passed[[CFG$feature_col]])
  cols   <- unique(unlist(lapply(feats, resolve_columns, data = data, exclude = exclude)))

  # Transparency: stable features that FAIL the criterion AND are NOT re-entered via a family.
  # (A failing nominal level whose factor was selected is NOT reported as dropped.)
  fail_feats <- as.character(table[[CFG$feature_col]][is_oc & stable & !pass])
  truly_dropped <- fail_feats[vapply(fail_feats, function(f) {
    if (length(exclude) && any(startsWith(f, exclude))) return(FALSE)  # plan_type -> strata, not "dropped"
    cc <- resolve_columns(f, data, exclude)
    length(cc) > 0 && !all(cc %in% cols)                              # all cols already in -> via family
  }, logical(1))]

  cat(sprintf("  [%s] kept %d features -> %d model columns (%s)\n",
              key, length(feats), length(cols), crit))
  if (length(truly_dropped))
    cat(sprintf("  dropped (stable but off-criterion, not re-entered via family): %s\n",
                paste(truly_dropped, collapse = ", ")))
  verify_factor_completeness(cols, data)

  list(features = feats, cols = cols,
       formula = build_formula(cols, oc$time, oc$event, strata),
       dropped = truly_dropped, criterion = crit)
}

# ----------------------------------------------------------------------
# 4) Diagnostics (report; drop only if drop_collinear = TRUE)
# ----------------------------------------------------------------------
collinearity_step <- function(sel, data, oc, exclude, strata) {
  fit <- try(coxph(sel$formula, data = data), silent = TRUE)
  if (inherits(fit, "try-error")) { message("[GVIF] model not evaluable"); return(sel) }
  v <- try(rms::vif(fit), silent = TRUE)
  if (inherits(v, "try-error")) return(sel)
  hi <- v[v > CFG$gvif_max]
  if (!length(hi)) { message("[GVIF] all <= ", CFG$gvif_max); return(sel) }
  if (!isTRUE(CFG$drop_collinear)) {
    message(sprintf("[GVIF] %d term(s) > %.0f (KEPT; set drop_collinear=TRUE to remove):",
                    length(hi), CFG$gvif_max)); print(round(hi, 2)); return(sel)
  }
  repeat {
    fit <- try(coxph(sel$formula, data = data), silent = TRUE); if (inherits(fit, "try-error")) break
    v <- try(rms::vif(fit), silent = TRUE)
    if (inherits(v, "try-error") || all(v <= CFG$gvif_max, na.rm = TRUE)) break
    worst <- names(which.max(v)); message(sprintf("[GVIF>%.0f] dropping '%s'", CFG$gvif_max, worst))
    sel$cols <- setdiff(sel$cols, worst)
    sel$formula <- build_formula(sel$cols, oc$time, oc$event, strata)
  }
  sel
}

report_ph <- function(sel, data) {
  fit <- try(coxph(sel$formula, data = data), silent = TRUE)
  if (inherits(fit, "try-error")) return(invisible(NULL))
  z <- try(cox.zph(fit), silent = TRUE); if (inherits(z, "try-error")) return(invisible(NULL))
  tab <- as.data.frame(z$table); tab$term <- rownames(tab)
  bad <- tab[tab$p < CFG$ph_alpha & tab$term != "GLOBAL", , drop = FALSE]
  if (nrow(bad)) {
    message("[PH] terms violating proportional hazards (Schoenfeld p<", CFG$ph_alpha, "):")
    print(bad[, c("term", "p")], row.names = FALSE)
    message("    -> to stratify them for THIS outcome, add to extra_strata$<outcome> and re-run.")
  } else message("[PH] no violations (p>=", CFG$ph_alpha, ").")
  invisible(z)
}

# ----------------------------------------------------------------------
# 5) ORCHESTRATOR -- accepts ANY CFG field by name
# ----------------------------------------------------------------------
run_selection <- function(data, imputed_list = NULL, ...) {
  ov <- list(...)
  unknown <- setdiff(names(ov), names(CFG))
  if (length(unknown)) warning("Unknown CFG fields (ignored): ", paste(unknown, collapse = ", "))
  .cfg_old <- CFG
  CFG <<- utils::modifyList(CFG, ov)
  on.exit(CFG <<- .cfg_old, add = TRUE)

  table <- readxl::read_excel(CFG$predictor_table, sheet = CFG$predictor_sheet)
  keys  <- names(CFG$outcomes)

  selected <- list()
  for (k in keys) {
    oc <- CFG$outcomes[[k]]; ex <- exclude_for(k); st <- strata_terms_for(k)
    cat(sprintf("\n=== %s (%s) | strata: %s ===\n", k, oc$tag, paste(st, collapse = " + ")))
    sel <- select_by_rule(oc, k, table, data, ex, st)   # per-outcome method from CFG$selection
    sel <- collinearity_step(sel, data, oc, ex, st)
    report_ph(sel, data)
    cat(sprintf("\n--- %s formula (%d terms) ---\n", k, length(sel$cols))); print(sel$formula)
    selected[[k]] <- sel
  }

  if (isTRUE(CFG$confirm) && !is.null(imputed_list) &&
      exists("evaluate_dual_cox_python_style", mode = "function") &&
      all(c("readmit", "death") %in% keys)) {
    cat("\n--- Dual confirmation (CV) of the selected pair: Uno's C + IBS ---\n")
    fin <- try(evaluate_dual_cox_python_style(
      formula_readmit = selected$readmit$formula, formula_death = selected$death$formula,
      imputed_list = imputed_list, k_folds = CFG$k_folds, eval_times = CFG$eval_times), silent = TRUE)
    if (!inherits(fin, "try-error")) print(fin$summary[fin$summary$Time == "Global", ])
    cat("\nNOTE: calibration (ICI / ECE / E:O) comes from prediction_metrics_complement.\n")
  }
  selected
}

# ----------------------------------------------------------------------
# USAGE (in the notebook):
#   source(file.path(project_root, "cons", "_alt_scripts",
#                    "seleccion_predictores_regla_fija.R"))
#   sel <- run_selection(
#     data            = py_corrected_datasets[[1]],
#     imputed_list    = py_corrected_datasets,   # only for the optional dual confirmation
#     predictor_table = file.path(project_root, "cons", "_out",
#                       "xgb9_dual_predictor_analysis_no_clinical_20260306_1821.xlsx"),
#     selection = list(readmit = list(by = "rank", top_k  = 20),   # flat importance -> robust top-K
#                      death   = list(by = "rel",  rel_min = 10)), # concentrated   -> % of max
#     drop_collinear  = FALSE,    # keep every predictor of the rule
#     confirm         = TRUE      # set FALSE to skip the (slow) dual CV report
#   )
#   formula_shap_readmit_rule <- sel$readmit$formula
#   formula_shap_death_rule   <- sel$death$formula
# ----------------------------------------------------------------------


#formula_shap_readmit_rule
#formula_shap_death_rule