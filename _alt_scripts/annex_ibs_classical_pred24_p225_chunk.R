# =============================================================================
# annex_ibs_classical_pred24_p225_chunk.R  (2026-08-22)
#
# ONE SELF-CONTAINED notebook chunk for cons/prediction225_converted_mod.ipynb.
# It brings the prediction24 candidates, the DeepSurv-informed specifications
# that `deltaCIBS-ds-common-grid` refits as Cox models, into the CLASSICAL
# (Graf 1999) integrated Brier score reported by the annex chunk
# `p225-annex-ibs-classical-dev`.
#
# WHY THIS NEEDS ITS OWN CHUNK AND CANNOT REUSE THE CACHE.
# The classical IBS integrates from 0 to each horizon on a 0.25-month grid. The
# common predictions cache stores surv_val_matrix on FIVE nodes only, and five
# nodes are exactly the coarse quadrature the classical definition corrects, so
# interpolating from them would put back the bias the definition removes. New
# arms therefore have to be REFIT out of fold on the dense grid. That is the
# whole cost of this chunk and there is no way around it.
#
# WHAT IT REUSES INSTEAD, WHICH IS WHAT MAKES THE DIFFERENCES PAIRED.
# The existing bundle written by `p225-annex-ibs-classical-dev`
# (data/20241015_out/pred225_annex_ibs_pairwise_all_arms_v2.rds) stores, per arm,
# the per-subject trapezoided contribution matrix. That matrix is ALL the paired
# bootstrap needs: one replicate is a matrix-vector product on it. So the twelve
# readmission arms and two mortality arms already scored are loaded as they are,
# the new prediction24 arms are scored here, and ONE shared subject bootstrap is
# run over the union. Every difference in the resulting table, old against old,
# new against new, and old against new, is computed on the same resampled
# subjects. Nothing is rescored and the existing cache is never overwritten.
#
# TWO GAPS IN THE EXISTING SWEEP THAT THIS CHUNK ALSO CLOSES.
# 1. `.annexp_readmit_formulas` in `p225-annex-ibs-classical-dev` lists base,
#    interact, updated and updated2. The SHAP readmission specification
#    (formula_shap_readmit_clean_updated), which is the primary readmission model
#    of the thesis and is fitted in section 5 of that same chunk as
#    S_oof_readmit, is absent from the comprehensive pairwise sweep. It is added
#    here under ANNEXG_INCLUDE_SHAP_READMIT, at no extra cost when that matrix is
#    still in the session.
# 2. The prediction24 arms were never in any classical-IBS table at all.
#
# COST. One dense-grid out-of-fold refit is 5 imputations x 5 folds coxph fits on
# about 56,400 rows plus predictSurvProb over 241 nodes. This chunk needs one per
# UNIQUE new formula, which is three readmission and three mortality formulas for
# the standard ds_specs, plus the two Cox mortality matrices if they are not
# still in the session, plus one for the SHAP readmission arm if requested.
# Budget roughly 30 to 90 minutes depending on workers, once. Results are cached.
#
# Placement: paste as its own cell at the END of
# cons/prediction225_converted_mod.ipynb, AFTER `p225-annex-ibs-classical-dev-save`
# if you have run the classical annex, or after `session-info` otherwise. It ADDS
# objects and never modifies ibs_dev_table, ibs_dev_pairwise_table or their caches.
# =============================================================================

# ---------------------------------------------------------------------------
# CHUNK -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-ibs-classical-pred24
#| message: true
#| warning: true
# ============================================================================
# CLASSICAL (Graf) IBS EXTENDED TO THE PREDICTION24 CANDIDATES, OUT OF FOLD ON
# THE DEVELOPMENT COHORT (py_corrected_datasets_boot, the 80 percent training
# split; the frozen 20 percent test set is never touched here).
# Produces:
#   annex_ibsg_table     per-arm IBS with bootstrap 95 percent intervals, in the
#                        SAME column layout as ibs_dev_table (arm, estimand,
#                        horizon_months, ibs_oof, ibs_lo95, ibs_hi95), so the new
#                        rows can be rbind-ed straight onto it.
#   annex_ibsg_pairwise  paired differences across every arm, old and new, from
#                        one shared subject bootstrap.
#   annex_ibsg_points    the point objects, kept so a later chunk can bootstrap
#                        further comparisons without refitting.
# Kernel prerequisites: NONE, but everything already in the session is reused:
# .dev or py_corrected_datasets_boot, FOLDS, S_oof_readmit, S_oof_death_full,
# S_oof_death_shap, dual_fits, ds_dual_fits and the classical-IBS bundle.
# Place at the END of the notebook. Adds objects; replaces nothing.
# ============================================================================
suppressPackageStartupMessages(library(survival))
stopifnot(requireNamespace("pec", quietly = TRUE))
.annexg_t0 <- proc.time()
if (!exists("ANNEXG_REFIT", inherits = TRUE)) ANNEXG_REFIT <- FALSE
if (!exists("ANNEXG_INCLUDE_SHAP_READMIT", inherits = TRUE)) ANNEXG_INCLUDE_SHAP_READMIT <- TRUE
# When TRUE the prediction24 readmission arms are ALSO paired with the
# prediction24 mortality models, which adds three more prediction definitions and
# roughly triples the readmission arm count. Left FALSE so the new arms use the
# same three definitions as the existing twelve (net risk, CIF with Full PH
# death, CIF with SHAP-informed death) and slot into that table unchanged.
if (!exists("ANNEXG_DS_DEATH_PAIRINGS", inherits = TRUE)) ANNEXG_DS_DEATH_PAIRINGS <- FALSE
# Assigned only when absent, so a caller can lower them for a trial run. The
# defaults are the reporting settings: leave them alone for anything that will be
# quoted. A coarser ANNEXG_STEP changes the quadrature and therefore the estimand,
# so a trial value must never be reported next to a 0.25 one.
if (!exists("ANNEXG_HORIZONS", inherits = TRUE)) ANNEXG_HORIZONS <- c(6, 12, 36, 60)
if (!exists("ANNEXG_STEP", inherits = TRUE))     ANNEXG_STEP     <- 0.25
if (!exists("ANNEXG_B", inherits = TRUE))        ANNEXG_B        <- 500L
if (!exists("ANNEXG_SEED", inherits = TRUE))     ANNEXG_SEED     <- 2125L
if (!identical(as.numeric(ANNEXG_STEP), 0.25) || !identical(as.numeric(ANNEXG_B), 500))
  warning("annex-ibs-g: running with ANNEXG_STEP = ", ANNEXG_STEP, " and B = ", ANNEXG_B,
          ", which are NOT the reporting settings (0.25 and 500). Trial output only.", call. = FALSE)
# A trial must never be able to overwrite a reporting artifact, so every file
# this chunk writes carries the tag below when the settings are not the
# reporting ones. That also keeps a trial cache from being picked up later.
.annexg_trial <- !identical(as.numeric(ANNEXG_STEP), 0.25) || !identical(as.numeric(ANNEXG_B), 500) ||
  (exists("ANNEXG_ONLY_MODELS", inherits = TRUE) && !is.null(ANNEXG_ONLY_MODELS))
.annexg_tag <- if (.annexg_trial) "_TRIAL" else ""
if (!exists("ANNEXG_WORKERS", inherits = TRUE)) ANNEXG_WORKERS <- NULL
# ---- 1. Root, engines, paths ------------------------------------------------
# ibs_classical_holdout.R resolves its competing-risk engine lazily through
# project_root and falls back to getwd(); outside the notebook getwd() is the
# cons folder, which makes it look for cons/cons/_alt_scripts and fail. Setting
# project_root before sourcing removes that lazy path, exactly as
# `p225-annex-ibs-classical-dev` does.
.annexg_root <- local({
  cand <- character(0)
  if (exists("project_root", inherits = TRUE) && is.character(project_root) &&
      length(project_root) == 1L && !is.na(project_root))
    cand <- c(cand, project_root, sub("(/|\\\\)?cons(/|\\\\)?$", "", project_root))
  pr <- tryCatch(here::here(), error = function(e) NA_character_)
  if (length(pr) == 1L && !is.na(pr)) cand <- c(cand, pr, sub("(/|\\\\)?cons(/|\\\\)?$", "", pr))
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  cand <- c(cand, wd, sub("(/|\\\\)?cons(/|\\\\)?$", "", wd))
  cand <- unique(cand[nzchar(cand)])
  ok <- cand[vapply(cand, function(p) dir.exists(file.path(p, "cons", "_alt_scripts")), logical(1))]
  if (!length(ok)) stop("annex-ibs-g: cannot locate the project root. Tried: ",
                        paste(cand, collapse = " | "), call. = FALSE)
  normalizePath(ok[1L], winslash = "/", mustWork = FALSE)
})
project_root <- .annexg_root
.annexg_data_out <- file.path(.annexg_root, "data", "20241015_out")
.annexg_out_dir  <- file.path(.annexg_root, "cons", "_out")
message("annex-ibs-g: project root resolved to ", .annexg_root)
source(file.path(.annexg_root, "cons", "_alt_scripts", "ibs_window_bootstrap_holdout.R"))
source(file.path(.annexg_root, "cons", "_alt_scripts", "ibs_classical_holdout.R"))
stopifnot(exists("ibs_classical_grid"), exists("ibs_classical_point"),
          exists("ibs_classical_readmit"), exists("ibs_classical_boot_ci"),
          exists(".ibsb_contrib_readmit_aj"), exists(".ibc_trap_weights"))
ANNEXG_GRID <- ibs_classical_grid(max_horizon = 60, step = ANNEXG_STEP, horizons = c(3, ANNEXG_HORIZONS))
ANNEXG_CACHE <- file.path(.annexg_data_out, paste0("pred225_annex_ibs_classical_pred24", .annexg_tag, ".rds"))
ANNEXG_LEGACY_BUNDLE <- file.path(.annexg_data_out, "pred225_annex_ibs_pairwise_all_arms_v2.rds")
# ---- 2. Development cohort ---------------------------------------------------
if (exists(".dev", inherits = TRUE) && is.list(.dev) && length(.dev) == 5L) {
  .annexg_dev <- .dev
  message("annex-ibs-g: reusing .dev from the session.")
} else {
  if (!exists("py_corrected_datasets_boot", inherits = TRUE)) {
    .annexg_rd <- file.path(.annexg_data_out, "pred22_ndp_2026_07_13.Rdata")
    stopifnot("the canonical pred22 RData is unavailable" = file.exists(.annexg_rd))
    cat("annex-ibs-g: py_corrected_datasets_boot is not in this session; loading ",
        basename(.annexg_rd), ", 5.6 GB, this takes a few minutes.\n", sep = ""); flush.console()
    .annexg_e <- new.env(parent = emptyenv()); load(.annexg_rd, envir = .annexg_e)
    stopifnot(exists("py_corrected_datasets_boot", envir = .annexg_e, inherits = FALSE))
    py_corrected_datasets_boot <- get("py_corrected_datasets_boot", envir = .annexg_e)
    rm(.annexg_e); invisible(gc())
  }
  .annexg_dev <- lapply(py_corrected_datasets_boot, as.data.frame)
}
stopifnot(is.list(.annexg_dev), length(.annexg_dev) == 5L)
.annexg_N <- nrow(.annexg_dev[[1]])
stopifnot(all(vapply(.annexg_dev, nrow, integer(1)) == .annexg_N))
.annexg_to01 <- function(x) if (is.logical(x)) as.integer(x) else as.integer(as.numeric(x) > 0)
.annexg_rt <- as.numeric(.annexg_dev[[1]]$readmit_time_from_disch_m)
.annexg_re <- .annexg_to01(.annexg_dev[[1]]$readmit_event)
.annexg_dt <- as.numeric(.annexg_dev[[1]]$death_time_from_disch_m)
.annexg_de <- .annexg_to01(.annexg_dev[[1]]$death_event)
# ---- 3. Fold partition, recovered rather than re-derived ---------------------
.annexg_fold_map <- function(fit) {
  rp <- fit$raw_predictions
  stopifnot(!is.null(rp), length(rp) > 0L)
  imps <- sort(unique(vapply(rp, function(b) as.integer(b$imp_idx), integer(1))))
  lapply(imps, function(im)
    lapply(Filter(function(b) as.integer(b$imp_idx) == im, rp),
           function(b) as.integer(b$original_val_idx)))
}
.annexg_fold_norm <- function(fm) lapply(fm, function(f) lapply(f, sort))
# ---- DS PROVENANCE GUARD ---------------------------------------------------
# The DeepSurv-informed common-grid cache is named after the prediction24 sidecar
# stem it was built from. Those sidecars get regenerated, and on 2026-08-23 the
# cache on disk was still the 2026-07-26 one while 2026-08-11 sidecars existed,
# whose `interact` formulas had grown (readmission gained 8 interactions;
# mortality gained 5 main effects and 12 interactions). Reporting arms built from
# a superseded stem is exactly the silent staleness this project cannot afford,
# so the newest COMPLETE sidecar triplet is located and compared against the stem
# in the cache filename, and a mismatch STOPS here. The fix is to re-run the cell
# `deltaCIBS-ds-common-grid`, which selects the newest stem itself and writes a
# new cache without touching the old one. Set ANNEX_DS_ALLOW_STALE_STEM <- TRUE
# only to reproduce an older table on purpose.
if (!exists("ANNEX_DS_ALLOW_STALE_STEM", inherits = TRUE)) ANNEX_DS_ALLOW_STALE_STEM <- FALSE
.ds_stem_check <- function(cache_file) {
  dir0 <- dirname(cache_file)
  suf <- c("dual_shap_ds_py", "dual_shap_ds_upd_py", "dual_shap_ds_int_py")
  pat <- paste0("^pred24.*__(", paste(suf, collapse = "|"), ")[.]rds$")
  ff <- list.files(dir0, pattern = pat)
  if (!length(ff)) return(invisible(NULL))
  stems <- unique(sub(paste0("__(", paste(suf, collapse = "|"), ")[.]rds$"), "", ff))
  complete <- stems[vapply(stems, function(s)
    all(file.exists(file.path(dir0, paste0(s, "__", suf, ".rds")))), logical(1))]
  if (!length(complete)) return(invisible(NULL))
  mt <- vapply(complete, function(s)
    max(file.info(file.path(dir0, paste0(s, "__", suf, ".rds")))$mtime),
    as.POSIXct("1970-01-01", tz = "UTC"))
  newest <- complete[which.max(mt)]
  used <- sub("^pred225_ds_common_grid__", "", sub("__v1[.]rds$", "", basename(cache_file)))
  if (identical(used, newest)) {
    message("annex: DS sidecar stem check passed; the cache matches the newest complete triplet (", newest, ").")
  } else if (isTRUE(ANNEX_DS_ALLOW_STALE_STEM)) {
    warning("annex: the DS cache was built from ", used, " but ", newest,
            " is newer. Proceeding because ANNEX_DS_ALLOW_STALE_STEM is TRUE.", call. = FALSE)
  } else {
    stop("annex: the DeepSurv-informed cache was built from sidecar stem ", used,
         " but a newer complete triplet exists, ", newest,
         ". Re-run the cell `deltaCIBS-ds-common-grid` to rebuild the cache from it, then rerun this cell. ",
         "Set ANNEX_DS_ALLOW_STALE_STEM <- TRUE only if you deliberately want the older arms.",
         call. = FALSE)
  }
  invisible(newest)
}
.annexg_get_fits <- function(nm, file_pattern, field) {
  if (exists(nm, envir = globalenv(), inherits = FALSE)) return(get(nm, envir = globalenv()))
  ff <- list.files(.annexg_data_out, pattern = file_pattern, full.names = TRUE)
  if (!length(ff)) return(NULL)
  ff <- ff[order(file.info(ff)$mtime, decreasing = TRUE)][1L]
  message("annex-ibs-g: reading ", basename(ff), " (", round(file.info(ff)$size / 1024^2), " MB).")
  # Any cache read here that is a DS common-grid bundle must come from the newest
  # sidecar triplet; see the DS provenance guard above.
  if (grepl("^pred225_ds_common_grid__", basename(ff))) .ds_stem_check(ff)
  tryCatch(readRDS(ff)[[field]], error = function(e) {
    message("annex-ibs-g: could not read ", basename(ff), " (", conditionMessage(e), ").")
    NULL })
}
.annexg_cox_fits <- .annexg_get_fits("dual_fits", "^pred22_ndp_2026_07_13__dualfits\\.rds$", "dual_fits")
.annexg_ds_fits  <- .annexg_get_fits("ds_dual_fits", "^pred225_ds_common_grid__.*\\.rds$", "ds_dual_fits")
if (is.null(.annexg_ds_fits))
  stop("annex-ibs-g: the DeepSurv-informed (prediction24) arms are unavailable. Run the cell `deltaCIBS-ds-common-grid` ",
       "first, or make sure a pred225_ds_common_grid__*.rds cache exists under data/20241015_out.",
       call. = FALSE)
if (exists("FOLDS", inherits = TRUE) && is.list(FOLDS) && length(FOLDS) == 5L) {
  .annexg_folds <- FOLDS
  message("annex-ibs-g: reusing the FOLDS partition already in the session.")
} else {
  .annexg_src <- if (!is.null(.annexg_cox_fits)) .annexg_cox_fits[[1]] else .annexg_ds_fits[[1]]
  .annexg_folds <- .annexg_fold_map(.annexg_src)
  message("annex-ibs-g: fold partition recovered from the cached out-of-fold predictions.")
}
for (fm in .annexg_folds) stopifnot("the fold partition does not cover the cohort exactly" =
  identical(sort(unlist(fm)), seq_len(.annexg_N)))
# Every arm that will enter the table must share ONE partition, otherwise a
# paired difference would partly reflect which subjects each arm trained on.
.annexg_all_fits <- c(if (is.null(.annexg_cox_fits)) list() else .annexg_cox_fits, .annexg_ds_fits)
.annexg_all_fits <- .annexg_all_fits[vapply(.annexg_all_fits, function(f)
  !is.null(f$raw_predictions) && length(f$raw_predictions) > 0L, logical(1))]
for (nm in names(.annexg_all_fits)) {
  if (!identical(.annexg_fold_norm(.annexg_fold_map(.annexg_all_fits[[nm]])), .annexg_fold_norm(.annexg_folds)))
    stop("annex-ibs-g: the out-of-fold partition of arm '", nm, "' differs from the reference partition; ",
         "a paired comparison on a common training partition is not available.", call. = FALSE)
}
message("annex-ibs-g: fold-partition check passed for ", length(.annexg_all_fits), " arms.")
# Row alignment: each cached fold block carries the outcome pair of its own
# validation subjects, so comparing it against the loaded cohort at
# original_val_idx proves the rows line up. A reordered copy of the input would
# otherwise give a plausible but meaningless IBS.
.annexg_checked <- 0L
for (b in Filter(function(x) as.integer(x$imp_idx) == 1L, .annexg_all_fits[[1]]$raw_predictions)) {
  o <- b$death; if (is.null(o) || is.null(o$y_val)) next
  yv <- as.matrix(o$y_val); ix <- as.integer(b$original_val_idx)
  stopifnot("row alignment against the cached out-of-fold outcomes failed" =
    nrow(yv) == length(ix) && max(abs(as.numeric(yv[, 1]) - .annexg_dt[ix])) < 1e-8 &&
    all(as.integer(yv[, 2]) == .annexg_de[ix]))
  .annexg_checked <- .annexg_checked + length(ix)
}
stopifnot("row alignment did not cover the whole cohort" = .annexg_checked == .annexg_N)
cat(sprintf("annex-ibs-g: row alignment verified on %d subjects.\n", .annexg_checked))
# ---- 4. Formula inventory ----------------------------------------------------
.annexg_clean <- function(f) {
  ff <- if (inherits(f, "formula")) f else stats::as.formula(paste(as.character(f), collapse = " "), env = globalenv())
  out <- stats::as.formula(gsub("strat\\(", "strata(", paste(deparse(ff), collapse = " ")), env = globalenv())
  environment(out) <- globalenv()
  out
}
.annexg_key <- function(f) gsub("[[:space:]]+", "", paste(deparse(f), collapse = " "))
# prediction24 formulas come from each arm's own $config, which stores them as
# text. That is the same source the Uno's C annex uses and it avoids depending on
# ds_specs, which the DS cache does not persist.
.annexg_ds_readmit <- list(); .annexg_ds_death <- list()
for (nm in names(.annexg_ds_fits)) {
  fr <- .annexg_clean(.annexg_ds_fits[[nm]]$config$formula_readmit)
  fd <- .annexg_clean(.annexg_ds_fits[[nm]]$config$formula_death)
  kr <- .annexg_key(fr); kd <- .annexg_key(fd)
  if (!kr %in% vapply(.annexg_ds_readmit, .annexg_key, "")) .annexg_ds_readmit[[nm]] <- fr
  if (!kd %in% vapply(.annexg_ds_death, .annexg_key, "")) .annexg_ds_death[[nm]] <- fd
}
cat(sprintf("annex-ibs-g: DeepSurv-informed (prediction24) contributes %d unique readmission and %d unique mortality formulas from %d arms.\n",
            length(.annexg_ds_readmit), length(.annexg_ds_death), length(.annexg_ds_fits)))
# Arm labels drop the redundant ds_ prefix that ds_specs already carries, so an
# arm reads readmission::ds-nph_readmit::net-risk rather than ds-ds_nph_readmit.
.annexg_lab <- function(nm) paste0("ds-", sub("^ds_", "", nm))
# The two Cox mortality models are needed as the competing cause of the CIF arms,
# so the new readmission arms use the SAME prediction definitions as the existing
# twelve. Recovered rather than hardcoded wherever possible; formula_shap_death_rule2
# is not persisted by save-results, so it is rebuilt literally and pinned by a
# term count, exactly as `p225-annex-ibs-classical-dev` does.
if (!exists("formula_death_updated2", inherits = TRUE) ||
    !exists("formula_shap_readmit_clean_updated", inherits = TRUE)) {
  .annexg_fms <- readRDS(file.path(.annexg_data_out, "_val_inputs", "formulas.rds"))
  if (!exists("formula_death_updated2", inherits = TRUE))
    formula_death_updated2 <- .annexg_fms$formula_death_updated2
  if (!exists("formula_shap_readmit_clean_updated", inherits = TRUE))
    formula_shap_readmit_clean_updated <- .annexg_fms$formula_shap_readmit_clean_updated
  rm(.annexg_fms)
}
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
.annexg_nt <- function(f) length(attr(stats::terms(f), "term.labels"))
stopifnot("the recovered Cox formulas do not have their expected term counts" =
  .annexg_nt(formula_death_updated2) == 65L && .annexg_nt(formula_shap_death_rule2) == 16L &&
  .annexg_nt(formula_shap_readmit_clean_updated) == 28L)
.annexg_cox_death <- list(`full-death` = .annexg_clean(formula_death_updated2),
                          `shap-death` = .annexg_clean(formula_shap_death_rule2))
# ---- 4b. The COMPLETE candidate inventory -----------------------------------
# Every Cox readmission and mortality specification is taken from its own fitted
# object's $config, which stores the formula as text. That is the same source the
# Uno's C annex uses, it needs no 5.6 GB RData, and it cannot drift from what was
# actually fitted. Formulas are de-duplicated by key, because ds_specs varies one
# outcome at a time and therefore repeats a formula across arms.
.annexg_readmit_models <- list()
.annexg_death_models <- list()
.annexg_family <- character(0)
.annexg_addmodel <- function(store, nm, f, fam) {
  k <- .annexg_key(f)
  if (k %in% vapply(store, .annexg_key, "")) return(store)
  store[[nm]] <- f
  .annexg_family[[nm]] <<- fam
  store
}
if (!is.null(.annexg_cox_fits)) {
  for (nm in names(.annexg_cox_fits)) {
    fit <- .annexg_cox_fits[[nm]]
    if (is.null(fit$config)) next
    .annexg_readmit_models <- .annexg_addmodel(.annexg_readmit_models, nm,
      .annexg_clean(fit$config$formula_readmit), "Cox")
    .annexg_death_models <- .annexg_addmodel(.annexg_death_models, nm,
      .annexg_clean(fit$config$formula_death), "Cox")
  }
}
for (nm in names(.annexg_ds_readmit))
  .annexg_readmit_models <- .annexg_addmodel(.annexg_readmit_models, .annexg_lab(nm),
    .annexg_ds_readmit[[nm]], "DeepSurv-informed")
for (nm in names(.annexg_ds_death))
  .annexg_death_models <- .annexg_addmodel(.annexg_death_models, .annexg_lab(nm),
    .annexg_ds_death[[nm]], "DeepSurv-informed")
# The two mortality models that define the CIF arms are pinned by formula key, not
# by position, so renaming a spec cannot silently repoint a prediction definition.
.annexg_cif_death <- list()
for (nm in names(.annexg_death_models)) {
  k <- .annexg_key(.annexg_death_models[[nm]])
  if (identical(k, .annexg_key(.annexg_cox_death[["full-death"]]))) .annexg_cif_death[["full-death"]] <- nm
  if (identical(k, .annexg_key(.annexg_cox_death[["shap-death"]]))) .annexg_cif_death[["shap-death"]] <- nm
}
stopifnot("the Full PH and SHAP mortality specifications were not found among the fitted arms" =
  length(.annexg_cif_death) == 2L)
if (isTRUE(ANNEXG_DS_DEATH_PAIRINGS))
  for (nm in names(.annexg_ds_death)) .annexg_cif_death[[paste0(.annexg_lab(nm), "-death")]] <- .annexg_lab(nm)
# Set ANNEXG_ALL_DEATH_ARMS to FALSE to score only the two mortality models the
# legacy table carried, which saves one dense-grid refit per dropped arm.
if (!exists("ANNEXG_ALL_DEATH_ARMS", inherits = TRUE)) ANNEXG_ALL_DEATH_ARMS <- TRUE
if (!isTRUE(ANNEXG_ALL_DEATH_ARMS))
  .annexg_death_models <- .annexg_death_models[unique(c(unlist(.annexg_cif_death),
    names(.annexg_death_models)[.annexg_family[names(.annexg_death_models)] == "DeepSurv-informed"]))]
# TARGET ARMS: every readmission model under every prediction definition, plus
# every mortality model. This is the inventory the table must end up covering, and
# it is asserted at the end rather than left to whatever the caches happened to hold.
.annexg_target <- list()
for (mn in names(.annexg_readmit_models)) {
  .annexg_target[[paste("readmission", mn, "net-risk", sep = "::")]] <-
    list(outcome = "readmission", model = mn, definition = "net-risk",
         family = unname(.annexg_family[[mn]]), readmit = mn, death = NA_character_)
  for (dn in names(.annexg_cif_death))
    .annexg_target[[paste("readmission", mn, paste0("cif-", dn), sep = "::")]] <-
      list(outcome = "readmission", model = mn, definition = paste0("cif-", dn),
           family = unname(.annexg_family[[mn]]), readmit = mn, death = .annexg_cif_death[[dn]])
}
# The two mortality specifications that also define the CIF arms keep the names
# the legacy bundle gave them, full-death and shap-death, so their already scored
# points are recognised instead of being refit under a second name.
.annexg_mort_alias <- function(dn) {
  for (k in c("full-death", "shap-death"))
    if (!is.null(.annexg_cif_death[[k]]) && identical(.annexg_cif_death[[k]], dn)) return(k)
  dn
}
for (dn in names(.annexg_death_models)) {
  al <- .annexg_mort_alias(dn)
  .annexg_target[[paste("mortality", al, sep = "::")]] <-
    list(outcome = "mortality", model = al, definition = al,
         family = unname(.annexg_family[[dn]]), readmit = NA_character_, death = dn)
}
# ---- 4b-bis. Optional restriction to named models ---------------------------
# ANNEXG_ONLY_MODELS keeps only the arms whose MODEL is named, which is the way to
# produce one or two arms without paying for the full sweep. A readmission model
# keeps all three of its prediction definitions, so its CIF arms still pull in the
# two mortality matrices they need as the competing cause. The restriction is
# applied to the target inventory itself, so the refit list, the bootstrap and the
# completeness assertion all shrink together and cannot disagree.
# Example, the two relaxed-PH DeepSurv-informed arms only:
#   ANNEXG_ONLY_MODELS <- c("ds-nph_readmit", "ds-nph_death")
if (!exists("ANNEXG_ONLY_MODELS", inherits = TRUE)) ANNEXG_ONLY_MODELS <- NULL
if (!is.null(ANNEXG_ONLY_MODELS)) {
  .annexg_known <- unique(c(names(.annexg_readmit_models), names(.annexg_death_models),
                            unlist(lapply(.annexg_target, `[[`, "model"))))
  .annexg_bad <- setdiff(ANNEXG_ONLY_MODELS, .annexg_known)
  if (length(.annexg_bad))
    stop("annex-ibs-g: ANNEXG_ONLY_MODELS names models that do not exist: ",
         paste(.annexg_bad, collapse = ", "), ". Available: ",
         paste(sort(.annexg_known), collapse = ", "), call. = FALSE)
  .annexg_keep <- vapply(.annexg_target, function(t) t$model %in% ANNEXG_ONLY_MODELS, logical(1))
  .annexg_target <- .annexg_target[.annexg_keep]
  stopifnot("ANNEXG_ONLY_MODELS left no arms at all" = length(.annexg_target) > 0L)
  cat("annex-ibs-g: RESTRICTED to models ", paste(ANNEXG_ONLY_MODELS, collapse = ", "),
      " -> ", length(.annexg_target), " target arm(s). The table is a SUBSET; do not present it ",
      "as the full candidate comparison.\n", sep = "")
}
cat(sprintf("annex-ibs-g: candidate inventory | readmission models %d (%s) x %d definitions | mortality models %d (%s) | target arms %d\n",
            length(unique(vapply(Filter(function(t) t$outcome == "readmission", .annexg_target), `[[`, "", "model"))),
            paste(unique(vapply(Filter(function(t) t$outcome == "readmission", .annexg_target), `[[`, "", "model")), collapse = ", "),
            1L + length(.annexg_cif_death),
            length(unique(vapply(Filter(function(t) t$outcome == "mortality", .annexg_target), `[[`, "", "model"))),
            paste(unique(vapply(Filter(function(t) t$outcome == "mortality", .annexg_target), `[[`, "", "model")), collapse = ", "),
            length(.annexg_target)))
# ---- 4c. Which target arms are ALREADY scored -------------------------------
# The legacy bundle stores a scored point per arm, and a point is all the shared
# bootstrap needs, so any target arm it already holds costs nothing. Loading it
# here rather than at merge time is what lets the refit list below be the minimum.
.annexg_have <- list()
if (exists("ibs_dev_pairwise_points", inherits = TRUE) && is.list(ibs_dev_pairwise_points)) {
  .annexg_have <- ibs_dev_pairwise_points
  cat("annex-ibs-g: reusing scored arms from the live ibs_dev_pairwise_points.\n")
} else if (file.exists(ANNEXG_LEGACY_BUNDLE)) {
  cat("annex-ibs-g: reading the classical-annex bundle", basename(ANNEXG_LEGACY_BUNDLE), "for arms already scored.\n")
  .annexg_have <- tryCatch(readRDS(ANNEXG_LEGACY_BUNDLE)$points, error = function(e) list())
}
.annexg_have <- .annexg_have[vapply(.annexg_have, function(p)
  is.list(p) && !is.null(p$contrib_trapezoid) && !is.null(p$horizons) &&
  identical(as.numeric(p$horizons), as.numeric(ANNEXG_HORIZONS)) &&
  nrow(p$contrib_trapezoid) == .annexg_N, logical(1))]
.annexg_todo_arms <- setdiff(names(.annexg_target), names(.annexg_have))
cat(sprintf("annex-ibs-g: %d of %d target arms are already scored; %d must be built here.\n",
            length(.annexg_target) - length(.annexg_todo_arms), length(.annexg_target),
            length(.annexg_todo_arms)))
# ---- 5. Expected baseline strata, derived per formula ------------------------
# Imposing the ten-stratum updated2 design on every model would be wrong, so the
# count is derived from each formula against the development data. The assertion
# on every fit is what catches the 2026-07-12 failure mode, a strata() term
# silently fitted as a covariate.
.annexg_strata_calls <- function(e) {
  if (!is.call(e)) return(list())
  nm <- paste(deparse(e[[1]]), collapse = "")
  if (nm %in% c("strata", "survival::strata")) return(list(e))
  unlist(lapply(as.list(e)[-1L], .annexg_strata_calls), recursive = FALSE)
}
.annexg_n_strata <- function(f, data) {
  cs <- .annexg_strata_calls(f[[3]])
  if (!length(cs)) return(1L)
  vals <- list()
  for (cl in cs) for (a in as.list(cl)[-1L])
    vals[[length(vals) + 1L]] <- eval(a, envir = data, enclos = environment(f))
  if (!length(vals) || any(vapply(vals, length, integer(1)) != nrow(data)))
    stop("annex-ibs-g: could not evaluate the strata terms of ", paste(deparse(f), collapse = " "), call. = FALSE)
  comb <- if (length(vals) == 1L) droplevels(factor(vals[[1]]))
          else do.call(interaction, c(lapply(vals, factor), list(drop = TRUE, lex.order = TRUE)))
  as.integer(nlevels(droplevels(comb)))
}
# ---- 6. Which dense-grid matrices are needed, and which already exist --------
# Only the arms that are NOT already scored drive a refit, and a CIF arm needs its
# readmission matrix and its mortality matrix, so the set is derived from the
# to-do list instead of being declared by hand.
.annexg_need <- list()
for (a in .annexg_todo_arms) {
  t <- .annexg_target[[a]]
  if (!is.na(t$readmit)) .annexg_need[[paste0("readmit__", t$readmit)]] <- .annexg_readmit_models[[t$readmit]]
  if (!is.na(t$death))   .annexg_need[[paste0("death__", t$death)]]     <- .annexg_death_models[[t$death]]
}
.annexg_valid <- function(S) is.matrix(S) && identical(dim(S), c(.annexg_N, length(ANNEXG_GRID))) && all(is.finite(S))
.annexg_S <- list()
.annexg_session <- list()
for (nm in names(.annexg_cif_death)) {
  if (identical(nm, "full-death")) .annexg_session[[paste0("death__", .annexg_cif_death[[nm]])]] <- "S_oof_death_full"
  if (identical(nm, "shap-death")) .annexg_session[[paste0("death__", .annexg_cif_death[[nm]])]] <- "S_oof_death_shap"
}
if ("readmit__shap" %in% names(.annexg_need)) .annexg_session[["readmit__shap"]] <- "S_oof_readmit"
for (key in names(.annexg_session)) {
  obj <- .annexg_session[[key]]
  if (key %in% names(.annexg_need) && exists(obj, inherits = TRUE)) {
    cand <- get(obj, inherits = TRUE)
    if (.annexg_valid(cand)) {
      .annexg_S[[key]] <- cand
      message("annex-ibs-g: reusing ", obj, " from the session, no refit for ", key, ".")
    }
  }
}
# ---- 7. Dense-grid out-of-fold refitting ------------------------------------
.annexg_cache_hit <- FALSE
if (!ANNEXG_REFIT && file.exists(ANNEXG_CACHE)) {
  .annexg_cc <- tryCatch(readRDS(ANNEXG_CACHE), error = function(e) NULL)
  if (!is.null(.annexg_cc) &&
      identical(as.numeric(.annexg_cc$meta$grid), as.numeric(ANNEXG_GRID)) &&
      identical(as.numeric(.annexg_cc$meta$horizons), as.numeric(ANNEXG_HORIZONS)) &&
      identical(.annexg_cc$meta$formula_keys, vapply(.annexg_need, .annexg_key, "")) &&
      all(.annexg_todo_arms %in% names(.annexg_cc$points))) {
    annex_ibsg_points <- .annexg_cc$points[.annexg_todo_arms]
    .annexg_arm_meta <- .annexg_cc$arm_metadata
    .annexg_cache_hit <- TRUE
    cat("annex-ibs-g: cache hit, dense-grid refitting skipped.\n")
  } else {
    cat("annex-ibs-g: cache present but its grid, horizons, formulas or arm coverage differ; rebuilding.\n")
  }
  rm(.annexg_cc)
}
if (!.annexg_cache_hit) {
  .annexg_todo <- setdiff(names(.annexg_need), names(.annexg_S))
  .annexg_exp <- vapply(.annexg_need[.annexg_todo], .annexg_n_strata, integer(1), data = .annexg_dev[[1]])
  cat("annex-ibs-g: dense-grid matrices to fit: ",
      paste(sprintf("%s (expect %d strata)", .annexg_todo, .annexg_exp), collapse = ", "), "\n", sep = "")
  cat(sprintf("annex-ibs-g: %d formulas x %d imputations x %d folds = %d Cox fits on %d grid nodes.\n",
              length(.annexg_todo), length(.annexg_dev), length(.annexg_folds[[1]]),
              length(.annexg_todo) * length(.annexg_dev) * length(.annexg_folds[[1]]), length(ANNEXG_GRID)))
  flush.console()
  .annexg_cluster <- NULL
  if (requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)) {
    nw <- if (!is.null(ANNEXG_WORKERS)) as.integer(ANNEXG_WORKERS) else
      max(1L, min(5L, tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 4L) - 2L))
    options(future.globals.maxSize = max(8 * 1024^3, 3 * as.numeric(utils::object.size(.annexg_dev[[1]]))))
    if (nw > 1L && requireNamespace("parallelly", quietly = TRUE)) {
      # PSOCK workers started WITHOUT project startup files: the renv activation
      # script in this project's .Rprofile is what makes plain multisession
      # workers slow to start and occasionally fail, which is why
      # `p225-annex-ibs-classical-dev` uses this same construction.
      cat(sprintf("annex-ibs-g: starting %d PSOCK workers without startup files.\n", nw)); flush.console()
      .annexg_cluster <- parallelly::makeClusterPSOCK(nw, rscript_args = "--no-init-file",
                                                      rscript_libs = .libPaths(), connectTimeout = 300,
                                                      validate = TRUE, autoStop = FALSE)
      future::plan(future::cluster, workers = .annexg_cluster)
    } else if (nw > 1L) {
      future::plan(future::multisession, workers = nw)
    } else {
      future::plan(future::sequential)
    }
  }
  on.exit({
    try(future::plan(future::sequential), silent = TRUE)
    if (!is.null(.annexg_cluster)) try(parallel::stopCluster(.annexg_cluster), silent = TRUE)
  }, add = TRUE)
  .annexg_oof <- function(formula, expected_strata, label) {
    K <- length(ANNEXG_GRID); acc <- matrix(0, .annexg_N, K)
    for (im in seq_along(.annexg_dev)) {
      d <- .annexg_dev[[im]]; folds <- .annexg_folds[[im]]
      res <- if (requireNamespace("future.apply", quietly = TRUE)) {
        future.apply::future_lapply(folds, function(vi) {
          fit <- survival::coxph(formula, data = d[-vi, , drop = FALSE], ties = "efron",
                                 x = TRUE, y = TRUE, model = TRUE)
          bh <- survival::basehaz(fit, centered = FALSE)
          ns <- if (is.null(bh$strata)) 1L else nlevels(droplevels(factor(bh$strata)))
          list(n_strata = ns, S = matrix(as.numeric(pec::predictSurvProb(
            fit, newdata = d[vi, , drop = FALSE], times = ANNEXG_GRID)), length(vi), K))
        }, future.seed = TRUE, future.packages = c("survival", "pec"))
      } else {
        lapply(folds, function(vi) {
          fit <- survival::coxph(formula, data = d[-vi, , drop = FALSE], ties = "efron",
                                 x = TRUE, y = TRUE, model = TRUE)
          bh <- survival::basehaz(fit, centered = FALSE)
          ns <- if (is.null(bh$strata)) 1L else nlevels(droplevels(factor(bh$strata)))
          list(n_strata = ns, S = matrix(as.numeric(pec::predictSurvProb(
            fit, newdata = d[vi, , drop = FALSE], times = ANNEXG_GRID)), length(vi), K))
        })
      }
      for (j in seq_along(folds)) {
        if (res[[j]]$n_strata != expected_strata)
          stop(sprintf("%s, imputation %d, fold %d: %d baseline strata, expected %d. A strata() term may have been fitted as a covariate.",
                       label, im, j, res[[j]]$n_strata, expected_strata), call. = FALSE)
        acc[folds[[j]], ] <- acc[folds[[j]], ] + res[[j]]$S
      }
      rm(res); cat("."); flush.console()
    }
    S <- acc / length(.annexg_dev)
    if (anyNA(S)) stop(sprintf("%s: out-of-fold survival contains NA.", label), call. = FALSE)
    stopifnot(min(S) >= -1e-10, max(S) <= 1 + 1e-10)
    cat(sprintf(" %s done (%.1f min elapsed)\n", label, (proc.time() - .annexg_t0)[["elapsed"]] / 60))
    pmin(pmax(S, 0), 1)
  }
  for (mn in .annexg_todo)
    .annexg_S[[mn]] <- .annexg_oof(.annexg_need[[mn]], .annexg_exp[[mn]], mn)
  try(future::plan(future::sequential), silent = TRUE)
  if (!is.null(.annexg_cluster)) { try(parallel::stopCluster(.annexg_cluster), silent = TRUE); .annexg_cluster <- NULL }
  invisible(gc())
  # ---- 8. Exact competing-hazard CIF, with its mandatory control -------------
  .annexg_cif <- function(S_r, S_d) {
    if (!identical(dim(S_r), dim(S_d))) stop("annex-ibs-g: CIF inputs have different dimensions.", call. = FALSE)
    H_r <- -log(pmax(S_r, 1e-12)); H_d <- -log(pmax(S_d, 1e-12)); S_all <- S_r * S_d
    n <- nrow(S_r); K <- ncol(S_r); cif <- matrix(0, n, K)
    prev_S <- rep(1, n); prev_Hr <- rep(0, n); prev_Hd <- rep(0, n); acc <- rep(0, n)
    for (k in seq_len(K)) {
      dr <- pmax(H_r[, k] - prev_Hr, 0); dd <- pmax(H_d[, k] - prev_Hd, 0); da <- dr + dd
      frac <- ifelse(da > 0, dr / da, 0); pev <- -expm1(-da)
      acc <- acc + prev_S * frac * pev
      cif[, k] <- acc
      prev_S <- S_all[, k]; prev_Hr <- H_r[, k]; prev_Hd <- H_d[, k]
    }
    pmin(pmax(cif, 0), 1)
  }
  local({
    g <- c(0, 0.25, 1, 6, 12, 36, 60)
    Sr <- rbind(exp(-0.01 * g), exp(-0.10 * g))
    no_d <- matrix(1, nrow(Sr), ncol(Sr)); with_d <- rbind(exp(-0.02 * g), exp(-0.20 * g))
    e1 <- max(abs(.annexg_cif(Sr, no_d) - (1 - Sr)))
    e2 <- max(.annexg_cif(Sr, with_d) - (1 - Sr))
    if (!is.finite(e1) || e1 > 1e-12 || !is.finite(e2) || e2 > 1e-12)
      stop("annex-ibs-g: the exact competing-hazard CIF control failed.", call. = FALSE)
    cat(sprintf("annex-ibs-g: exact CIF control passed (no-death error %.3e, competing-death excess %.3e).\n", e1, e2))
  })
  # ---- 9. Score exactly the target arms that are not already scored ---------
  .annexg_mk_death <- function(S) list(S = S, grid = ANNEXG_GRID, n_imp = length(.annexg_dev),
    y_test = data.frame(time = .annexg_dt, event = .annexg_de),
    y_train = data.frame(time = .annexg_dt, event = .annexg_de))
  .annexg_mk_readmit <- function(S) list(S = S, grid = ANNEXG_GRID, n_imp = length(.annexg_dev),
    rt_val = .annexg_rt, re_val = .annexg_re, dt_val = .annexg_dt, de_val = .annexg_de,
    rt_tr = .annexg_rt, re_tr = .annexg_re, dt_tr = .annexg_dt, de_tr = .annexg_de)
  annex_ibsg_points <- list(); .annexg_meta <- list()
  .annexg_add <- function(arm, point, t) {
    point$method <- if (identical(t$outcome, "mortality")) "death" else "readmission-aj"
    annex_ibsg_points[[arm]] <<- point
    .annexg_meta[[length(.annexg_meta) + 1L]] <<- data.frame(
      arm = arm, outcome = t$outcome, model = t$model,
      prediction_definition = t$definition, family = t$family, stringsAsFactors = FALSE)
  }
  # The CIF is rebuilt per arm from its own two matrices. Both controls travel with
  # it: a cumulative incidence must be nondecreasing on the grid, and it can never
  # exceed the net risk of the same readmission model, since removing the competing
  # cause can only take probability away.
  .annexg_score_target <- function(arm) {
    t <- .annexg_target[[arm]]
    if (identical(t$outcome, "mortality")) {
      S <- .annexg_S[[paste0("death__", t$death)]]
      return(ibs_classical_point(.annexg_mk_death(S), horizons = ANNEXG_HORIZONS))
    }
    Sr <- .annexg_S[[paste0("readmit__", t$readmit)]]
    if (identical(t$definition, "net-risk"))
      return(ibs_classical_readmit(.annexg_mk_readmit(Sr), ANNEXG_HORIZONS, method = "aalen-johansen"))
    Sd <- .annexg_S[[paste0("death__", t$death)]]
    cif <- .annexg_cif(Sr, Sd)
    if (any(cif[, -1L, drop = FALSE] + 1e-10 < cif[, -ncol(cif), drop = FALSE]))
      stop("annex-ibs-g: a computed CIF is not nondecreasing for ", arm, ".", call. = FALSE)
    ex <- max(cif - (1 - Sr), na.rm = TRUE)
    if (!is.finite(ex) || ex > 1e-10)
      stop(sprintf("annex-ibs-g: a computed CIF exceeds its net risk for %s by %.3e.", arm, ex), call. = FALSE)
    out <- ibs_classical_readmit(.annexg_mk_readmit(1 - cif), ANNEXG_HORIZONS, method = "aalen-johansen")
    rm(cif); invisible(gc(verbose = FALSE))
    out
  }
  for (arm in .annexg_todo_arms) {
    .annexg_add(arm, .annexg_score_target(arm), .annexg_target[[arm]])
    cat(sprintf("annex-ibs-g: scored %-52s (%.1f min elapsed)\n", arm,
                (proc.time() - .annexg_t0)[["elapsed"]] / 60)); flush.console()
  }
  .annexg_arm_meta <- do.call(rbind, .annexg_meta); rownames(.annexg_arm_meta) <- NULL
  rm(.annexg_S); invisible(gc())
  saveRDS(list(points = annex_ibsg_points, arm_metadata = .annexg_arm_meta,
               meta = list(grid = ANNEXG_GRID, horizons = ANNEXG_HORIZONS,
                           formula_keys = vapply(.annexg_need, .annexg_key, ""),
                           target_arms = names(.annexg_target),
                           scored_arms = .annexg_todo_arms,
                           n = .annexg_N, created = as.character(Sys.time()))),
          ANNEXG_CACHE)
  cat("annex-ibs-g: cached to ", ANNEXG_CACHE, "\n", sep = "")
}
cat(sprintf("annex-ibs-g: %d arm(s) scored in this run.\n", length(annex_ibsg_points)))
# ---- 10. Union of scored arms, and the completeness assertion ----------------
# Arms already scored elsewhere are taken exactly as stored: only
# $contrib_trapezoid and $horizons feed the shared bootstrap, so nothing is
# rescored and pred225_annex_ibs_pairwise_all_arms_v2.rds is never written to.
# The union is then checked against the declared inventory, so a table that
# quietly lost a candidate cannot reach the manuscript.
.annexg_all_points <- c(.annexg_have[intersect(names(.annexg_target), names(.annexg_have))],
                        annex_ibsg_points)
.annexg_reused <- intersect(names(.annexg_target), names(.annexg_have))
.annexg_all_meta <- do.call(rbind, lapply(names(.annexg_all_points), function(a) {
  t <- .annexg_target[[a]]
  data.frame(arm = a, outcome = t$outcome, model = t$model,
             prediction_definition = t$definition, family = t$family,
             source = if (a %in% .annexg_reused) "reused" else "scored here",
             stringsAsFactors = FALSE)
}))
rownames(.annexg_all_meta) <- NULL
.annexg_missing <- setdiff(names(.annexg_target), names(.annexg_all_points))
if (length(.annexg_missing))
  stop("annex-ibs-g: the table does not cover the declared inventory. Missing arms: ",
       paste(.annexg_missing, collapse = ", "), call. = FALSE)
cat(sprintf("annex-ibs-g: inventory complete, %d arms (%d reused, %d scored here).\n",
            length(.annexg_all_points), length(.annexg_reused), length(annex_ibsg_points)))
print(table(.annexg_all_meta$outcome, .annexg_all_meta$family))
# ---- 11. ONE shared subject bootstrap over every arm ------------------------
# G is fixed by the pooled cohort, so each subject's trapezoided contribution is
# constant and one replicate is a matrix-vector product. Using the SAME resampled
# subjects for every arm in every replicate is what makes all the differences
# below genuinely paired, old against old, new against new, and old against new.
cat(sprintf("annex-ibs-g: shared subject bootstrap, %d arms x %d horizons x B = %d.\n",
            length(.annexg_all_points), length(ANNEXG_HORIZONS), ANNEXG_B)); flush.console()
.annexg_C <- lapply(.annexg_all_points, `[[`, "contrib_trapezoid")
.annexg_h <- as.numeric(ANNEXG_HORIZONS)
set.seed(ANNEXG_SEED, kind = "Mersenne-Twister")
.annexg_boot <- array(NA_real_, c(length(.annexg_C), length(.annexg_h), ANNEXG_B),
                      dimnames = list(names(.annexg_C), as.character(.annexg_h), paste0("b", seq_len(ANNEXG_B))))
for (b in seq_len(ANNEXG_B)) {
  cnt <- tabulate(sample.int(.annexg_N, .annexg_N, replace = TRUE), nbins = .annexg_N)
  for (a in seq_along(.annexg_C))
    .annexg_boot[a, , b] <- as.numeric(cnt %*% .annexg_C[[a]]) / .annexg_N / .annexg_h
  if (b %% 100L == 0L) { cat(sprintf(" %d", b)); flush.console() }
}
cat("\n")
.annexg_q <- function(M, p) apply(M, 1L, function(x) {
  f <- x[is.finite(x)]; if (length(f) < 20L) NA_real_ else stats::quantile(f, probs = p, names = FALSE) })
# ---- 12. Per-arm table, in the layout of ibs_dev_table ----------------------
annex_ibsg_table <- do.call(rbind, lapply(names(.annexg_all_points), function(a) {
  md <- .annexg_all_meta[match(a, .annexg_all_meta$arm), , drop = FALSE]
  bm <- .annexg_boot[a, , , drop = TRUE]
  data.frame(arm = a, family = md$family, outcome = md$outcome, model = md$model,
             estimand = md$prediction_definition,
             horizon_months = .annexg_h, ibs_oof = .annexg_all_points[[a]]$ibs$ibs,
             ibs_lo95 = .annexg_q(bm, 0.025), ibs_hi95 = .annexg_q(bm, 0.975),
             stringsAsFactors = FALSE, row.names = NULL)
}))
annex_ibsg_table <- annex_ibsg_table[order(annex_ibsg_table$outcome, annex_ibsg_table$horizon_months,
                                           annex_ibsg_table$estimand, annex_ibsg_table$ibs_oof), ]
rownames(annex_ibsg_table) <- NULL
# ---- 13. Paired differences across every arm --------------------------------
.annexg_pairs <- c(
  utils::combn(.annexg_all_meta$arm[.annexg_all_meta$outcome == "readmission"], 2L, simplify = FALSE),
  utils::combn(.annexg_all_meta$arm[.annexg_all_meta$outcome == "mortality"], 2L, simplify = FALSE))
annex_ibsg_pairwise <- do.call(rbind, lapply(.annexg_pairs, function(pr) {
  mA <- .annexg_all_meta[match(pr[1], .annexg_all_meta$arm), , drop = FALSE]
  mB <- .annexg_all_meta[match(pr[2], .annexg_all_meta$arm), , drop = FALSE]
  bA <- .annexg_boot[pr[1], , , drop = TRUE]; bB <- .annexg_boot[pr[2], , , drop = TRUE]
  bd <- bA - bB
  pA <- .annexg_all_points[[pr[1]]]$ibs$ibs; pB <- .annexg_all_points[[pr[2]]]$ibs$ibs
  data.frame(
    outcome = mA$outcome,
    comparison = if (mA$family == mB$family) paste("within", mA$family) else "Cox vs DeepSurv-informed",
    definition_scope = if (mA$outcome == "mortality") "mortality pair"
                       else if (mA$prediction_definition == mB$prediction_definition) "within prediction definition"
                       else "cross prediction definition",
    model_A = mA$model, model_B = mB$model, family_A = mA$family, family_B = mB$family,
    prediction_A = mA$prediction_definition, prediction_B = mB$prediction_definition,
    arm_A = pr[1], arm_B = pr[2], horizon_months = .annexg_h, n = .annexg_N,
    ibs_A = pA, ibs_A_lo = .annexg_q(bA, 0.025), ibs_A_hi = .annexg_q(bA, 0.975),
    ibs_B = pB, ibs_B_lo = .annexg_q(bB, 0.025), ibs_B_hi = .annexg_q(bB, 0.975),
    delta_ibs = pA - pB, delta_lo = .annexg_q(bd, 0.025), delta_hi = .annexg_q(bd, 0.975),
    bootstrap_mean = rowMeans(bd, na.rm = TRUE),
    centering_bias = rowMeans(bd, na.rm = TRUE) - (pA - pB),
    bootstrap_valid = rowSums(is.finite(bd)), bootstrap_requested = ANNEXG_B,
    stringsAsFactors = FALSE, row.names = NULL)
}))
annex_ibsg_pairwise <- annex_ibsg_pairwise[order(annex_ibsg_pairwise$outcome,
                                                 annex_ibsg_pairwise$horizon_months,
                                                 -abs(annex_ibsg_pairwise$delta_ibs)), ]
rownames(annex_ibsg_pairwise) <- NULL
# ---- 14. Report -------------------------------------------------------------
# delta_ibs = IBS_A - IBS_B, so a NEGATIVE difference favours A: lower prediction
# error is better. That is the opposite sign convention from the delta C tables,
# which is easy to misread, so it is restated here rather than left implicit.
cat("\n== Classical (Graf) IBS, out of fold on development, DeepSurv-informed candidates included ==\n")
cat(sprintf("cohort n = %s | grid step %.2f months | horizons %s | B = %d | arms %d (%d scored in this run)\n",
            format(.annexg_N, big.mark = ","), ANNEXG_STEP, paste(ANNEXG_HORIZONS, collapse = ", "),
            ANNEXG_B, nrow(.annexg_all_meta), length(annex_ibsg_points)))
cat("\n-- ALL ARMS: per-arm IBS with bootstrap 95 percent intervals --\n")
print(annex_ibsg_table[, c("outcome", "model", "family", "estimand", "horizon_months",
                           "ibs_oof", "ibs_lo95", "ibs_hi95")], row.names = FALSE, digits = 6)
cat("\n-- Cox versus DeepSurv-informed, within the same prediction definition --\n")
.annexg_focus <- annex_ibsg_pairwise[annex_ibsg_pairwise$comparison == "Cox vs DeepSurv-informed" &
                                     annex_ibsg_pairwise$definition_scope != "cross prediction definition", , drop = FALSE]
if (nrow(.annexg_focus)) {
  print(.annexg_focus[, c("outcome", "model_A", "model_B", "prediction_A", "horizon_months",
                          "ibs_A", "ibs_B", "delta_ibs", "delta_lo", "delta_hi")],
        row.names = FALSE, digits = 6)
  cat(sprintf("\nLargest |delta IBS| between families: %.6f. Judge it against the 0.01 promotion threshold\n",
              max(abs(.annexg_focus$delta_ibs), na.rm = TRUE)))
  cat("of the model-selection rule, not against whether the interval excludes zero.\n")
} else {
  cat("None available: the existing classical-annex bundle was not found, so there is nothing to compare against.\n")
}
# ---- 15. Save ----------------------------------------------------------------
# CSV under cons/_out (text, safe on Drive); the R objects under data/, never
# under cons/, per the project rule on .rds placement. Existing artifacts of the
# classical annex are never touched.
if (!dir.exists(.annexg_out_dir)) dir.create(.annexg_out_dir, recursive = TRUE)
utils::write.csv(annex_ibsg_table, file.path(.annexg_out_dir, paste0("pred225_annex_ibs_classical_pred24_arms", .annexg_tag, ".csv")), row.names = FALSE)
utils::write.csv(annex_ibsg_pairwise, file.path(.annexg_out_dir, paste0("pred225_annex_ibs_classical_pred24_pairwise", .annexg_tag, ".csv")), row.names = FALSE)
saveRDS(list(table = annex_ibsg_table, pairwise = annex_ibsg_pairwise,
             arm_metadata = .annexg_all_meta, bootstrap_replicates = .annexg_boot,
             meta = list(grid = ANNEXG_GRID, horizons = ANNEXG_HORIZONS, B = ANNEXG_B,
                         seed = ANNEXG_SEED, n = .annexg_N,
                         delta_definition = "delta_ibs = IBS_A - IBS_B; negative favours A",
                         include_shap_readmit = ANNEXG_INCLUDE_SHAP_READMIT,
                         ds_death_pairings = ANNEXG_DS_DEATH_PAIRINGS,
                         created = as.character(Sys.time()))),
        file.path(.annexg_data_out, paste0("pred225_annex_ibs_classical_pred24_tables", .annexg_tag, ".rds")))
cat(sprintf("
Saved: cons/_out/pred225_annex_ibs_classical_pred24_arms%s.csv, ...pairwise%s.csv and
data/20241015_out/pred225_annex_ibs_classical_pred24_tables%s.rds
", .annexg_tag, .annexg_tag, .annexg_tag))
cat(sprintf("[chunk: annex-ibs-classical-pred24] %.1f min\n", (proc.time() - .annexg_t0)[["elapsed"]] / 60))
cat("To append the new rows to the existing per-arm table, run:\n")
cat("  ibs_dev_table_ext <- rbind(ibs_dev_table, annex_ibsg_table[annex_ibsg_table$family == 'DeepSurv-informed',\n")
cat("    c('arm', 'estimand', 'horizon_months', 'ibs_oof', 'ibs_lo95', 'ibs_hi95')])\n")
