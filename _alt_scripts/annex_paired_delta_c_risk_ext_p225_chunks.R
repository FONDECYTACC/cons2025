# =============================================================================
# annex_paired_delta_c_risk_ext_p225_chunks.R  (2026-08-22)
#
# TWO SELF-CONTAINED notebook chunks for cons/prediction225_converted_mod.ipynb.
# They EXTEND the manuscript table
#
#   "Paired differences in horizon-specific Uno's C between candidate Cox
#    specifications, out-of-fold development sample, adults entering SUD
#    treatment, 2010-2020, Chile"
#
# so that it also carries the prediction24 candidates, that is the
# DeepSurv-informed specifications that the cell `deltaCIBS-ds-common-grid`
# already refits as Cox models on the same common grid.
#
# WHY THEY ARE MISSING TODAY. The table comes from `paired-delta-c-analytic`,
# which opens with stopifnot(exists("dual_fits"), exists("specs"), ...) and
# sweeps utils::combn(names(specs), 2). `deltaCIBS-ds-common-grid` deliberately
# writes its results to SEPARATE objects, ds_dual_fits and ds_specs, so the
# prediction24 arms are simply never seen by that sweep. Nothing is broken; the
# two families were just never merged into one comparison.
#
# WHY THE MERGE IS LEGITIMATE, and this is the part a reviewer will probe.
# Both families are produced by the SAME estimator on the SAME data:
# evaluate_dual_cox_python_style(imputed_list = py_corrected_datasets_boot,
# k_folds = 5, eval_times = c(3, 6, 12, 36, 60)), the Cox arms with the engine
# default seed = 2125 and the DS arms with DS_SEED = 2125 passed explicitly.
# The fold assignment comes from .dual_cox_fold_assignments(), whose stratum
# labels are built by .dual_cox_composite_strata() from the OUTCOME and the plan
# dummies, never from the model formula, under split_seed = seed + imp_idx - 1.
# Every arm therefore shares one out-of-fold partition, so a difference between
# a Cox arm and a prediction24 arm cannot be an artefact of fold assignment.
# Chunk 1 does not take that on trust: it recomputes a fold signature per arm
# from original_val_idx and refuses to proceed silently if any two disagree.
#
# WHAT THE ANNEX PRESERVES. The estimand is the one in `paired-delta-c-analytic`:
# marker = predicted risk 1 - S(t) at the horizon, Uno's C with timewt = "n/G2"
# and reverse = TRUE, and the same rule that skips a pair whose formula for that
# outcome is identical. On the net-risk scale that skip is CORRECT, since such a
# pair is degenerate and its delta C is exactly zero; the annex simply stops
# hiding it in a console message and returns the skipped pairs as a table, so the
# manuscript can state its own denominator. Chunk 2 reconciles its Cox-versus-Cox
# point estimates against the live delta_C_analytic_table when that object is
# present, so the extension can be shown to reproduce the existing table exactly.
#
# WHAT THE ANNEX CHANGES, deliberately. The reported interval is a NONPARAMETRIC
# PAIRED BOOTSTRAP, not the infinitesimal jackknife, which is still computed and
# kept as a secondary column. That is affordable only because chunk 2 scores every
# arm once per replicate instead of once per pair, and uses the low-level
# concordance counter without the per-replicate variance; see the header of chunk
# 2 for the measurements and for the equivalence gate that guards the fast route.
# Because a bootstrap replicate has to resample the SAME subjects for every arm,
# chunk 2 masks once across all arms rather than per pair. That is the same set
# whenever every arm has the same finite rows, which is the normal case here; the
# chunk asserts it and reports the discrepancy if it ever fails.
#
# NOT to be confused with cons/_alt_scripts/annex_cr_cindex_readmit_p225_chunks.R,
# which asks a different question: it rescores readmission on the
# competing-risk CUMULATIVE INCIDENCE. This file stays on the net risk, because
# that is the scale the table in question is defined on.
#
# CHUNK 1  `annex-paired-delta-c-risk-ext-build`  merges the arms, checks them.
# CHUNK 2  `annex-paired-delta-c-risk-ext-run`    builds the extended table.
#
# Both are self-contained: they resolve the project root themselves and, if the
# objects are not live, read dual_fits from
# data/20241015_out/pred22_ndp_2026_07_13__dualfits.rds and ds_dual_fits from the
# newest data/20241015_out/pred225_ds_common_grid__*.rds. The 5.6 GB pred22
# RData is never needed: every arm carries its own formula text in fit$config,
# which is all the annex uses for labelling and for the skip rule.
#
# Placement: paste each block as its own cell at the END of
# cons/prediction225_converted_mod.ipynb, AFTER `session-info`. Both ADD objects.
# Neither touches delta_C_analytic_table, delta_C_risk_table or delta_C_cr_table,
# so `save-results-additional-delta-tables` keeps working unchanged.
# =============================================================================

# ---------------------------------------------------------------------------
# CHUNK 1 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-paired-delta-c-risk-ext-build
#| message: true
#| warning: true
# ============================================================================
# EXTENDED PAIRED DELTA C ON PREDICTED RISK, STEP 1/2: merge and verify the arms.
# Produces one object, `annex_pdc`, holding everything chunk 2 needs:
#   $models        arm names, Cox family first, prediction24 family second
#   $family        "cox" or "deepsurv_informed", per arm
#   $f_txt         list(readmission = <named chr>, death = <named chr>), the
#                  formula TEXT per arm taken from fit$config, which is what the
#                  identical-formula skip rule compares
#   $time, $event  list(readmission = ..., death = ...), the observed out-of-fold
#                  outcome, verified identical across every arm
#   $risk[[oc]][[m]]   MI-pooled predicted risk 1 - S(t), rows = subjects,
#                  columns = eval_times, per outcome and arm
#   $ok[[oc]][[m]]     that arm's finite-row mask, so chunk 2 can apply the same
#                  per-PAIR mask `paired-delta-c-analytic` uses
#   $fold_signature    one signature per arm; they must all be equal
#   $eval_times, $horizons, $n_rows, $provenance
# Kernel prerequisites: NONE. dual_fits and ds_dual_fits are reused if live,
# otherwise read from data/20241015_out. Set ANNEX_PDC_INCLUDE_DS <- FALSE to
# rebuild the Cox-only table, which is the reconciliation case.
# Place AFTER `session-info`. Adds objects; replaces nothing.
# ============================================================================
if (!exists("ANNEX_PDC_FORCE_REBUILD", inherits = TRUE)) ANNEX_PDC_FORCE_REBUILD <- FALSE
if (!exists("ANNEX_PDC_USE_DISK_CACHE", inherits = TRUE)) ANNEX_PDC_USE_DISK_CACHE <- TRUE
if (!exists("ANNEX_PDC_INCLUDE_DS", inherits = TRUE)) ANNEX_PDC_INCLUDE_DS <- TRUE
if (!exists("ANNEX_PDC_MODELS", inherits = TRUE)) ANNEX_PDC_MODELS <- NULL
# A differing fold partition would make a Cox-versus-prediction24 difference
# partly an artefact of which subjects each arm was trained on. Left at TRUE the
# chunk stops rather than produce such a table; set it to FALSE only if you
# intend to report the mismatch as a limitation.
if (!exists("ANNEX_PDC_REQUIRE_SAME_FOLDS", inherits = TRUE)) ANNEX_PDC_REQUIRE_SAME_FOLDS <- TRUE
# The canonical common grid. IBS integrates over whatever grid it is handed, so
# objects built on a different grid are not interchangeable (the documented
# eval_times trap). Uno's C at a fixed horizon does not integrate, but the
# horizon must still be an exact column of the grid.
ANNEX_PDC_GRID     <- c(3, 6, 12, 36, 60)
ANNEX_PDC_HORIZONS <- c(6, 12, 36, 60)
stopifnot("ANNEX_PDC_HORIZONS must be exact columns of ANNEX_PDC_GRID" =
  all(ANNEX_PDC_HORIZONS %in% ANNEX_PDC_GRID))
# ---- Resolve AND VALIDATE the project root ---------------------------------
# Never take `project_root` on faith: running from the console commonly leaves
# the working directory at <root>/cons, which silently yields paths like
# <root>/cons/data/20241015_out. Accept only a candidate that really contains
# data/20241015_out.
.apdc_root <- local({
  cand <- character(0)
  if (exists("project_root", inherits = TRUE) && is.character(project_root) &&
      length(project_root) == 1L && !is.na(project_root)) {
    cand <- c(cand, project_root, sub("(/|\\\\)?cons(/|\\\\)?$", "", project_root))
  }
  pr_here <- tryCatch(here::here(), error = function(e) NA_character_)
  if (length(pr_here) == 1L && !is.na(pr_here)) {
    cand <- c(cand, pr_here, sub("(/|\\\\)?cons(/|\\\\)?$", "", pr_here))
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  cand <- c(cand, wd, sub("(/|\\\\)?cons(/|\\\\)?$", "", wd))
  cand <- unique(cand[nzchar(cand)])
  ok <- cand[vapply(cand, function(p) dir.exists(file.path(p, "data", "20241015_out")), logical(1L))]
  if (!length(ok))
    stop(sprintf("annex-pdc: cannot locate the project root. Tried: %s. None contains data/20241015_out.",
                 paste(cand, collapse = " | ")), call. = FALSE)
  normalizePath(ok[1L], winslash = "/", mustWork = FALSE)
})
message("annex-pdc build: project root resolved to ", .apdc_root)
if (!exists("project_root", inherits = TRUE) ||
    !identical(normalizePath(project_root, winslash = "/", mustWork = FALSE), .apdc_root)) {
  project_root <- .apdc_root
  message("annex-pdc build: project_root set to the resolved root (it was absent or pointed elsewhere).")
}
.apdc_data_out <- file.path(.apdc_root, "data", "20241015_out")
ANNEX_PDC_CACHE_RDS <- file.path(.apdc_data_out, "pred225_annex_paired_delta_c_risk_ext_inputs.rds")
stopifnot(requireNamespace("survival", quietly = TRUE))
# ---- Helpers, duplicated here so the annex needs no source() ----------------
.apdc_extract <- function(res, outcome = c("readmission", "death")) {
  # Reassemble the out-of-fold predictions into one per-subject matrix, pooled
  # over imputations. VERBATIM port of .cv_extract() from `deltaCIBS-cv-engine`,
  # except that it returns the RISK 1 - S(t) instead of S(t) and drops the
  # linear predictor, which this table does not use. The is.null(o$lp_val)
  # validity test is kept exactly as in the original, so a block the engine
  # considers unusable is skipped identically here.
  outcome <- match.arg(outcome)
  rp <- res$raw_predictions
  if (is.null(rp) || !length(rp))
    stop("no raw_predictions (stripped object or a calibration result, not a dual evaluation)", call. = FALSE)
  et <- as.numeric(res$config$eval_times)
  N <- max(vapply(rp, function(b) max(as.integer(b$original_val_idx)), 1L))
  imps <- sort(unique(vapply(rp, function(b) as.integer(b$imp_idx), 1L)))
  time_v <- rep(NA_real_, N); event_v <- rep(NA_real_, N); S_imp <- list()
  for (im in imps) {
    S <- matrix(NA_real_, N, length(et))
    for (b in rp) {
      if (as.integer(b$imp_idx) != im) next
      o <- b[[outcome]]
      if (is.null(o) || !is.null(o$error) || is.null(o$lp_val)) next
      idx <- as.integer(b$original_val_idx)
      S[idx, ] <- as.matrix(o$surv_val_matrix)
      yv <- o$y_val
      time_v[idx] <- as.numeric(yv[, 1]); event_v[idx] <- as.numeric(yv[, 2])
    }
    S_imp[[length(S_imp) + 1L]] <- S
  }
  S_pooled <- Reduce(`+`, S_imp) / length(S_imp)
  list(time = time_v, event = event_v,
       risk = pmin(pmax(1 - S_pooled, 0), 1), eval_times = et)
}
.apdc_fold_signature <- function(fit) {
  # A compact fingerprint of the out-of-fold partition, per imputation. Folds
  # are identified by their sorted membership, then ordered by first element so
  # that a mere relabelling of fold 1 and fold 2 does not read as a different
  # partition. Same idea as the .same_folds check in `p225-annex-ibs-classical-dev`.
  rp <- fit$raw_predictions
  imps <- sort(unique(vapply(rp, function(b) as.integer(b$imp_idx), 1L)))
  paste(vapply(imps, function(im) {
    fl <- lapply(Filter(function(b) as.integer(b$imp_idx) == im, rp),
                 function(b) sort(as.integer(b$original_val_idx)))
    fl <- fl[order(vapply(fl, function(v) v[1L], 1L))]
    paste(vapply(fl, function(v) sprintf("%d:%d:%d", length(v), v[1L], v[length(v)]), ""), collapse = "|")
  }, ""), collapse = "//")
}
.apdc_cfg_txt <- function(fit, which) paste(as.character(fit$config[[which]]), collapse = " ")
# ---- FAST PATH: a cache written by a previous run of this same chunk --------
# Validated, not trusted: the grid, the arm manifest and the row count must
# match what this run asks for, otherwise the cache is ignored and rebuilt.
annex_pdc <- NULL
if (!ANNEX_PDC_FORCE_REBUILD && ANNEX_PDC_USE_DISK_CACHE && file.exists(ANNEX_PDC_CACHE_RDS)) {
  .apdc_try <- tryCatch(readRDS(ANNEX_PDC_CACHE_RDS), error = function(e) NULL)
  .apdc_ok <- !is.null(.apdc_try) && is.list(.apdc_try) &&
    all(c("models", "family", "f_txt", "time", "event", "risk", "ok", "eval_times") %in% names(.apdc_try)) &&
    isTRUE(all.equal(as.numeric(.apdc_try$eval_times), ANNEX_PDC_GRID)) &&
    length(.apdc_try$risk$readmission) == length(.apdc_try$models) &&
    (isTRUE(ANNEX_PDC_INCLUDE_DS) ||
       !any(.apdc_try$family %in% c("deepsurv_informed", "prediction24", "DeepSurv-informed"))) &&
    (is.null(ANNEX_PDC_MODELS) || all(ANNEX_PDC_MODELS %in% .apdc_try$models))
  if (isTRUE(.apdc_ok)) {
    annex_pdc <- .apdc_try
    annex_pdc$provenance <- paste0(annex_pdc$provenance, " [reloaded from ", basename(ANNEX_PDC_CACHE_RDS), "]")
    message("annex-pdc build: reused the annex disk cache ", basename(ANNEX_PDC_CACHE_RDS),
            ". Set ANNEX_PDC_FORCE_REBUILD <- TRUE to ignore it.")
  } else {
    message("annex-pdc build: annex disk cache present but it did not validate; rebuilding.")
  }
  rm(.apdc_try)
}
# ---- Build ------------------------------------------------------------------
if (is.null(annex_pdc)) {
  .apdc_prov <- character(0)
  # 1. The Cox candidates, from the same cache `deltaCIBS-cv-fit-commongrid` and
  #    `p225-annex-ibs-classical-dev` already use.
  if (exists("dual_fits", envir = globalenv(), inherits = FALSE)) {
    .apdc_cox <- get("dual_fits", envir = globalenv())
    .apdc_prov <- c(.apdc_prov, "Cox arms from the live dual_fits")
  } else {
    .apdc_cf <- file.path(.apdc_data_out, "pred22_ndp_2026_07_13__dualfits.rds")
    stopifnot("pred22_ndp_2026_07_13__dualfits.rds not found under data/20241015_out" = file.exists(.apdc_cf))
    message("annex-pdc build: reading ", basename(.apdc_cf), " (",
            round(file.info(.apdc_cf)$size / 1024^2), " MB). This takes a moment.")
    .apdc_cox <- readRDS(.apdc_cf)$dual_fits
    .apdc_prov <- c(.apdc_prov, paste0("Cox arms from ", basename(.apdc_cf)))
  }
  .apdc_fits <- .apdc_cox
  .apdc_fam <- rep("Cox", length(.apdc_cox))
  names(.apdc_fam) <- names(.apdc_cox)
  # 2. The prediction24 candidates. `deltaCIBS-ds-common-grid` takes the DeepSurv
  #    formula sets from the p24 sidecars and refits them with the SAME dual-Cox
  #    engine, which is why they belong in a table about "candidate Cox
  #    specifications": the specification is DeepSurv-informed, the fitted model
  #    that produces 1 - S(t) is a cause-specific Cox model.
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
  if (isTRUE(ANNEX_PDC_INCLUDE_DS)) {
    .apdc_ds <- NULL
    if (exists("ds_dual_fits", envir = globalenv(), inherits = FALSE)) {
      .apdc_ds <- get("ds_dual_fits", envir = globalenv())
      .apdc_prov <- c(.apdc_prov, "DeepSurv-informed arms (prediction24) from the live ds_dual_fits")
    } else {
      .apdc_dsf <- list.files(.apdc_data_out, pattern = "^pred225_ds_common_grid__.*\\.rds$", full.names = TRUE)
      if (length(.apdc_dsf)) {
        .apdc_dsf <- .apdc_dsf[order(file.info(.apdc_dsf)$mtime, decreasing = TRUE)][1L]
        message("annex-pdc build: reading ", basename(.apdc_dsf), " (",
                round(file.info(.apdc_dsf)$size / 1024^2), " MB).")
        .ds_stem_check(.apdc_dsf)
        .apdc_ds <- tryCatch(readRDS(.apdc_dsf)$ds_dual_fits, error = function(e) {
          message("annex-pdc build: DS cache unreadable (", conditionMessage(e), ").")
          NULL
        })
        if (!is.null(.apdc_ds)) .apdc_prov <- c(.apdc_prov, paste0("DeepSurv-informed arms (prediction24) from ", basename(.apdc_dsf)))
      }
    }
    if (is.null(.apdc_ds)) {
      stop("annex-pdc: the DeepSurv-informed (prediction24) arms were requested but neither ds_dual_fits nor a ",
           "pred225_ds_common_grid__*.rds cache is available. Run the cell `deltaCIBS-ds-common-grid` ",
           "first, or set ANNEX_PDC_INCLUDE_DS <- FALSE to build the Cox-only table.", call. = FALSE)
    }
    .apdc_dup <- intersect(names(.apdc_fits), names(.apdc_ds))
    if (length(.apdc_dup))
      stop("annex-pdc: arm names collide between dual_fits and ds_dual_fits: ",
           paste(.apdc_dup, collapse = ", "), call. = FALSE)
    .apdc_fits <- c(.apdc_fits, .apdc_ds)
    .apdc_fam <- c(.apdc_fam, stats::setNames(rep("DeepSurv-informed", length(.apdc_ds)), names(.apdc_ds)))
    rm(.apdc_ds)
  }
  # 3. Keep only arms that still carry raw_predictions, the same filter
  #    `paired-delta-c-analytic` applies before any comparison.
  .apdc_fits <- .apdc_fits[!vapply(.apdc_fits, is.null, logical(1))]
  .apdc_hasrp <- vapply(.apdc_fits, function(f) !is.null(f$raw_predictions) && length(f$raw_predictions) > 0L, logical(1))
  if (any(!.apdc_hasrp))
    message("annex-pdc build: dropped for having no raw_predictions: ",
            paste(names(.apdc_fits)[!.apdc_hasrp], collapse = ", "))
  .apdc_fits <- .apdc_fits[.apdc_hasrp]
  if (!is.null(ANNEX_PDC_MODELS)) {
    stopifnot("ANNEX_PDC_MODELS names an arm that is not available" = all(ANNEX_PDC_MODELS %in% names(.apdc_fits)))
    .apdc_fits <- .apdc_fits[ANNEX_PDC_MODELS]
  }
  .apdc_fam <- .apdc_fam[names(.apdc_fits)]
  stopifnot("at least two arms are needed for a paired table" = length(.apdc_fits) >= 2L)
  # 4. Grid guard. An arm on another grid does not line up column by column.
  .apdc_g <- vapply(.apdc_fits, function(f) paste(as.numeric(f$config$eval_times), collapse = ","), "")
  .apdc_want <- paste(ANNEX_PDC_GRID, collapse = ",")
  if (any(.apdc_g != .apdc_want))
    stop("annex-pdc: these arms are not on the canonical grid ", .apdc_want, ": ",
         paste(names(.apdc_fits)[.apdc_g != .apdc_want], collapse = ", "), call. = FALSE)
  # 5. Fold-partition guard. See the header: the partition is a function of the
  #    outcome, the plan dummies and the seed, never of the formula, so every arm
  #    should share it. This verifies rather than assumes.
  .apdc_sig <- vapply(.apdc_fits, .apdc_fold_signature, "")
  .apdc_same_folds <- length(unique(.apdc_sig)) == 1L
  if (!.apdc_same_folds) {
    .apdc_groups <- split(names(.apdc_sig), .apdc_sig)
    msg <- paste(vapply(seq_along(.apdc_groups), function(i)
      sprintf("partition %d: %s", i, paste(.apdc_groups[[i]], collapse = ", ")), ""), collapse = " | ")
    if (isTRUE(ANNEX_PDC_REQUIRE_SAME_FOLDS)) {
      stop("annex-pdc: the arms do NOT share one out-of-fold partition, so a paired difference ",
           "between arms in different groups would partly reflect fold assignment. ", msg,
           ". Set ANNEX_PDC_REQUIRE_SAME_FOLDS <- FALSE only if you intend to report this as a limitation.",
           call. = FALSE)
    }
    warning("annex-pdc: the arms do NOT share one out-of-fold partition. ", msg, call. = FALSE)
  } else {
    message("annex-pdc build: fold-partition check passed, all ", length(.apdc_fits),
            " arms share one out-of-fold partition.")
  }
  # 6. Extract both outcomes for every arm, and verify the observed outcome.
  .apdc_nm <- names(.apdc_fits)
  .apdc_risk <- list(); .apdc_ok <- list(); .apdc_time <- list(); .apdc_event <- list()
  for (oc in c("readmission", "death")) {
    rl <- list(); okl <- list(); tref <- NULL; eref <- NULL
    for (m in .apdc_nm) {
      ex <- .apdc_extract(.apdc_fits[[m]], oc)
      okm <- is.finite(ex$time) & is.finite(ex$event) & rowSums(is.finite(ex$risk)) == ncol(ex$risk)
      rl[[m]] <- ex$risk; okl[[m]] <- okm
      if (is.null(tref)) {
        tref <- ex$time; eref <- ex$event
      } else {
        # Every arm is evaluated on the same subjects, so the observed outcome
        # must be identical wherever two arms both saw a subject. If it is not,
        # the arms are not comparable at all and a paired delta is meaningless.
        both <- is.finite(ex$time) & is.finite(tref)
        stopifnot("arms disagree on the observed outcome" =
          (!any(both)) || (max(abs(ex$time[both] - tref[both])) < 1e-8 &&
                           all(ex$event[both] == eref[both])))
        # Coalesce rather than take the first arm's copy verbatim: if one arm's
        # fold block failed, its rows carry NA here, and adopting that NA for
        # every pair would silently drop subjects the other arms did observe,
        # which is NOT what `paired-delta-c-analytic` does (it masks per pair).
        fill <- !is.finite(tref) & is.finite(ex$time)
        tref[fill] <- ex$time[fill]; eref[fill] <- ex$event[fill]
      }
      rm(ex)
    }
    .apdc_risk[[oc]] <- rl; .apdc_ok[[oc]] <- okl
    .apdc_time[[oc]] <- tref; .apdc_event[[oc]] <- eref
    message(sprintf("annex-pdc build: %-11s extracted for %d arms, finite rows per arm %s",
                    oc, length(.apdc_nm), paste(vapply(okl, sum, 1L), collapse = "/")))
    invisible(gc(verbose = FALSE))
  }
  annex_pdc <- list(
    models = .apdc_nm, family = .apdc_fam,
    f_txt = list(readmission = vapply(.apdc_fits, .apdc_cfg_txt, "", which = "formula_readmit"),
                 death       = vapply(.apdc_fits, .apdc_cfg_txt, "", which = "formula_death")),
    time = .apdc_time, event = .apdc_event, risk = .apdc_risk, ok = .apdc_ok,
    fold_signature = .apdc_sig, same_folds = .apdc_same_folds,
    eval_times = ANNEX_PDC_GRID, horizons = ANNEX_PDC_HORIZONS,
    n_rows = length(.apdc_time$readmission),
    provenance = paste(.apdc_prov, collapse = "; "),
    created = as.character(Sys.time())
  )
  rm(.apdc_fits, .apdc_cox, .apdc_risk, .apdc_ok); invisible(gc(verbose = FALSE))
  if (ANNEX_PDC_USE_DISK_CACHE) {
    saveRDS(annex_pdc, ANNEX_PDC_CACHE_RDS)
    message("annex-pdc build: cached to ", ANNEX_PDC_CACHE_RDS, " (",
            round(file.info(ANNEX_PDC_CACHE_RDS)$size / 1024^2), " MB)")
  }
}
# ---- Diagnostics printed whichever tier supplied the inputs -----------------
cat(sprintf("\n== Extended paired delta C on predicted risk: inputs ==\nprovenance: %s\n", annex_pdc$provenance))
cat(sprintf("subjects = %d | grid = %s | horizons = %s\n", annex_pdc$n_rows,
            paste(annex_pdc$eval_times, collapse = ", "), paste(annex_pdc$horizons, collapse = ", ")))
cat(sprintf("one shared out-of-fold partition across all arms: %s\n", annex_pdc$same_folds))
print(data.frame(arm = annex_pdc$models, family = unname(annex_pdc$family),
                 finite_readmission = vapply(annex_pdc$ok$readmission, sum, 1L),
                 finite_death = vapply(annex_pdc$ok$death, sum, 1L),
                 row.names = NULL), row.names = FALSE)
# Which arms will be skipped for which outcome, and why. On the net-risk scale a
# pair sharing the formula for an outcome is degenerate: both arms produce the
# identical 1 - S(t) for that outcome, so delta C is exactly zero and carries no
# information. Listing them here gives the manuscript table an explicit
# denominator instead of a console message that disappears with the output.
annex_pdc_formula_groups <- lapply(c("readmission", "death"), function(oc) {
  g <- split(annex_pdc$models, gsub("[[:space:]]+", "", annex_pdc$f_txt[[oc]]))
  g[lengths(g) > 1L]
})
names(annex_pdc_formula_groups) <- c("readmission", "death")
for (oc in c("readmission", "death")) {
  gg <- annex_pdc_formula_groups[[oc]]
  if (length(gg)) {
    cat(sprintf("\nArms sharing one %s formula (their pairwise delta C on 1 - S(t) is exactly zero and is skipped):\n", oc))
    for (i in seq_along(gg)) cat(sprintf("  group %d: %s\n", i, paste(gg[[i]], collapse = ", ")))
  } else {
    cat(sprintf("\nNo two arms share a %s formula: nothing will be skipped for that outcome.\n", oc))
  }
}
cat("\nInputs ready in `annex_pdc`. Run the cell `annex-paired-delta-c-risk-ext-run` next.\n")

# ---------------------------------------------------------------------------
# CHUNK 2 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-paired-delta-c-risk-ext-run
#| message: true
#| warning: true
# ============================================================================
# EXTENDED PAIRED DELTA C ON PREDICTED RISK, STEP 2/2: build the table.
# THE PRIMARY INTERVAL IS A NONPARAMETRIC PAIRED BOOTSTRAP. The analytic
# infinitesimal-jackknife interval is still computed and reported alongside, as a
# secondary column, because it costs one extra call and a visible disagreement
# between the two is itself diagnostic.
#
# HOW THE BOOTSTRAP IS MADE AFFORDABLE, because the obvious design is not.
# Bootstrapping one PAIR at a time, which is what a naive extension of
# `paired-delta-c-absolute-risk` does, means one survival::concordance() call per
# pair per replicate. Measured at n = 70,521 on this machine that is about 16.5
# seconds per replicate for ten arms, so B = 500 over two outcomes and four
# horizons runs for roughly 18 hours. Two changes remove that.
# 1. SCORE EVERY ARM ONCE PER REPLICATE, NOT EVERY PAIR. Within one outcome and
#    horizon all arms share the same follow-up, the same event indicator and the
#    same resampled subjects, so one pass over the ten markers yields all ten
#    concordances, and every one of the forty-five paired differences is then a
#    subtraction. The differences stay exactly as paired as before: they come
#    from one and the same resample.
# 2. USE THE LOW-LEVEL COUNTER AND SKIP THE PER-REPLICATE VARIANCE.
#    survival::concordance() computes an influence-function variance on every
#    call, which the bootstrap does not use because the bootstrap IS the variance
#    estimate. survival:::concordancefit(..., std.err = FALSE) returns the
#    concordant, discordant and tied.x counts without it, and
#    C = (concordant + tied.x / 2) / (concordant + discordant + tied.x)
#    reproduces concordance()$concordance exactly. Verified on this data layout
#    at 1e-16 for continuous, one-decimal and integer-month time granularities,
#    with tied markers, and on bootstrap resamples where almost every follow-up
#    time is duplicated.
# Together those take the same B = 500 from about 18 hours to about 40 minutes
# sequential, and to a few minutes across workers. Nothing about the estimand,
# the weights or the pairing changes: only how many times the same number is
# recomputed.
# concordancefit is INTERNAL to survival, so it is never trusted blindly. The
# equivalence gate below recomputes every arm, outcome and horizon on the
# OBSERVED data through both routes and refuses the fast path unless they agree
# to 1e-10, falling back to concordance() and saying so.
#
# ONE SUBJECT SET FOR EVERY ARM. `paired-delta-c-analytic` masks per PAIR. That
# is equivalent to masking once across all arms whenever every arm has the same
# finite rows, which is the normal case here because five-fold cross-validation
# gives every subject exactly one out-of-fold prediction. The chunk asserts it
# rather than assuming it, and says how many rows differ if it fails, because a
# shared resample is only possible on a shared subject set.
#
# Produces:
#   annex_pdc_table    the extended manuscript table, bootstrap interval first.
#   annex_pdc_bymodel  each arm's own horizon-specific Uno's C with a bootstrap
#                      interval from the same replicates.
#   annex_pdc_skipped  the pairs excluded by the identical-formula rule.
#   annex_pdc_boot     the replicate array [arm, horizon, B] per outcome, kept so
#                      any further comparison can be added without rescoring.
#
# Requires in kernel: `annex_pdc` and `.apdc_root` from the cell
# `annex-paired-delta-c-risk-ext-build`. Place AFTER it. Adds objects; replaces
# nothing, and in particular does not touch delta_C_analytic_table.
# ============================================================================
stopifnot("run the cell `annex-paired-delta-c-risk-ext-build` first" = exists("annex_pdc", inherits = TRUE))
stopifnot("run the cell `annex-paired-delta-c-risk-ext-build` first" = exists(".apdc_root", inherits = TRUE))
stopifnot(requireNamespace("survival", quietly = TRUE))
if (!exists("ANNEX_PDC_B", inherits = TRUE)) ANNEX_PDC_B <- 500L
if (!exists("ANNEX_PDC_SEED", inherits = TRUE)) ANNEX_PDC_SEED <- 2125L
# NULL means: use whatever future::plan() is already active, otherwise run
# sequentially. An integer starts that many PSOCK workers here and stops them
# before the chunk ends. The workers are started WITHOUT project startup files,
# the construction `p225-annex-ibs-classical-dev` settled on, because the renv
# activation script in this project's .Rprofile is what makes plain multisession
# workers slow to start and occasionally fail.
if (!exists("ANNEX_PDC_WORKERS", inherits = TRUE)) ANNEX_PDC_WORKERS <- NULL
# Set to FALSE to force the reference route, survival::concordance() on every
# replicate. Same numbers, roughly three times slower.
if (!exists("ANNEX_PDC_FAST_COUNTER", inherits = TRUE)) ANNEX_PDC_FAST_COUNTER <- TRUE
if (!exists("ANNEX_PDC_PROMOTION_DELTA", inherits = TRUE)) ANNEX_PDC_PROMOTION_DELTA <- 0.01
.apdc_crit <- stats::qnorm(0.975)
# Columns of the REPORTED table. excludes_zero, favours and meets_promotion_threshold
# are deliberately not reported: the first is a redundant restatement of whether the
# interval covers zero, and the other two dichotomise a continuous difference, which
# is exactly the reading the estimate and its interval are meant to replace. They are
# still computed and kept inside annex_pdc_table and the saved .rds, because the
# narrative summary below uses them, so nothing is lost by leaving them out here.
# ASSIGNED UNCONDITIONALLY, on purpose. An earlier draft guarded this with
# if (!exists(...)) so the list could be overridden, and that guard silently
# adopted the STALE list left in the kernel by the analytic version of this cell,
# which named se_delta, delta_C_lower and delta_C_upper. This version produces
# se_delta_ij, delta_ij_lower, delta_ij_upper, delta_boot_lower and
# delta_boot_upper, so the first print died with "undefined columns selected"
# AFTER the bootstrap had already run. Every use below also intersects with the
# names the table really has, so a later rename cannot resurrect the same failure.
ANNEX_PDC_REPORT_COLS <- c("outcome", "comparison", "model_A", "family_A", "model_B", "family_B",
                           "horizon", "n", "events_by_horizon", "C_A", "C_B", "delta_C",
                           "delta_boot_lower", "delta_boot_upper", "boot_mean", "centering_bias",
                           "se_delta_ij", "delta_ij_lower", "delta_ij_upper")
.apdc_cols <- function(d, drop = character(0))
  intersect(setdiff(ANNEX_PDC_REPORT_COLS, drop), names(d))
# Family labels are normalised here rather than trusted, so an `annex_pdc` built
# by an earlier version of chunk 1, or reloaded from its disk cache, still reports
# the manuscript labels. "prediction24" and "deepsurv_informed" were both used
# while these annexes were being written; the reported name is DeepSurv-informed.
annex_pdc$family <- c(cox = "Cox", Cox = "Cox", prediction24 = "DeepSurv-informed",
                      deepsurv_informed = "DeepSurv-informed",
                      `DeepSurv-informed` = "DeepSurv-informed")[annex_pdc$family]
names(annex_pdc$family) <- annex_pdc$models
stopifnot("unrecognised family label in annex_pdc$family" = !anyNA(annex_pdc$family))
.apdc_colj <- function(h) {
  j <- which(abs(as.numeric(annex_pdc$eval_times) - h) < 1e-8)
  if (length(j) != 1L)
    stop(sprintf("horizon %s is not exactly one column of the grid %s", h,
                 paste(annex_pdc$eval_times, collapse = ", ")), call. = FALSE)
  j
}
# ---- One subject set per outcome, asserted rather than assumed --------------
.apdc_mask <- list(); .apdc_mask_ok <- TRUE
for (oc in c("readmission", "death")) {
  per_arm <- annex_pdc$ok[[oc]]
  base_ok <- is.finite(annex_pdc$time[[oc]]) & is.finite(annex_pdc$event[[oc]])
  m <- Reduce(`&`, per_arm) & base_ok
  ndiff <- vapply(per_arm, function(a) sum(a != m), 1L)
  if (any(ndiff > 0L)) {
    .apdc_mask_ok <- FALSE
    warning("annex-pdc: the arms do not share one finite-row set for ", oc,
            " (rows differing from the shared set, by arm: ",
            paste(sprintf("%s=%d", names(ndiff), ndiff), collapse = ", "),
            "). The shared set is used so the bootstrap can resample once for every arm; ",
            "the within-Cox rows may then differ slightly from delta_C_analytic_table, ",
            "which masks per pair.", call. = FALSE)
  }
  .apdc_mask[[oc]] <- m
}
if (.apdc_mask_ok)
  message("annex-pdc run: every arm has the same finite rows in both outcomes, so the shared ",
          "subject set is identical to the per-pair mask of `paired-delta-c-analytic`.")
# ---- Markers, laid out once per outcome and horizon -------------------------
.apdc_arms <- annex_pdc$models
.apdc_K <- length(.apdc_arms)
.apdc_data <- list()
for (oc in c("readmission", "death")) {
  m <- .apdc_mask[[oc]]
  tt <- as.numeric(annex_pdc$time[[oc]][m]); ee <- as.numeric(annex_pdc$event[[oc]][m])
  per_h <- lapply(annex_pdc$horizons, function(h) {
    j <- .apdc_colj(h)
    M <- vapply(.apdc_arms, function(a) as.numeric(annex_pdc$risk[[oc]][[a]][m, j]), numeric(sum(m)))
    colnames(M) <- .apdc_arms
    M
  })
  names(per_h) <- as.character(annex_pdc$horizons)
  .apdc_data[[oc]] <- list(time = tt, event = ee, n = length(tt), markers = per_h)
}
# ---- Scoring routes, and the gate that decides which one the bootstrap uses --
.apdc_c_ref <- function(tt, ee, marker, horizon) {
  unname(survival::concordance(survival::Surv(tt, ee) ~ marker, timewt = "n/G2",
                               reverse = TRUE, ymax = horizon)$concordance)
}
.apdc_cfit <- if (isTRUE(ANNEX_PDC_FAST_COUNTER) &&
                  exists("concordancefit", where = asNamespace("survival")))
  get("concordancefit", envir = asNamespace("survival")) else NULL
.apdc_c_fast <- function(y, marker, horizon) {
  r <- .apdc_cfit(y, marker, timewt = "n/G2", reverse = TRUE, ymax = horizon, std.err = FALSE)
  cn <- r$count
  unname((cn[["concordant"]] + cn[["tied.x"]] / 2) /
         (cn[["concordant"]] + cn[["discordant"]] + cn[["tied.x"]]))
}
.apdc_use_fast <- FALSE
if (!is.null(.apdc_cfit)) {
  .apdc_gap <- 0
  for (oc in names(.apdc_data)) {
    dd <- .apdc_data[[oc]]; y <- survival::Surv(dd$time, dd$event)
    for (hh in names(dd$markers)) for (k in seq_len(.apdc_K)) {
      a <- tryCatch(.apdc_c_fast(y, dd$markers[[hh]][, k], as.numeric(hh)), error = function(e) NA_real_)
      b <- .apdc_c_ref(dd$time, dd$event, dd$markers[[hh]][, k], as.numeric(hh))
      .apdc_gap <- max(.apdc_gap, abs(a - b), na.rm = TRUE)
    }
  }
  .apdc_use_fast <- is.finite(.apdc_gap) && .apdc_gap < 1e-10
  cat(sprintf("annex-pdc run: fast-counter equivalence gate, max |concordancefit - concordance| over %d arm-outcome-horizon cells = %.3e -> %s\n",
              .apdc_K * 2L * length(annex_pdc$horizons), .apdc_gap,
              if (.apdc_use_fast) "fast route accepted" else "REFUSED, falling back to concordance()"))
} else {
  cat("annex-pdc run: the fast counter is unavailable or disabled; using survival::concordance() throughout.\n")
}
.apdc_score_all <- function(tt, ee, M, horizon) {
  # All arms, one resample, one pass. This is the whole bootstrap inner loop.
  if (.apdc_use_fast) {
    y <- survival::Surv(tt, ee)
    vapply(seq_len(ncol(M)), function(k) .apdc_c_fast(y, M[, k], horizon), numeric(1))
  } else {
    vapply(seq_len(ncol(M)), function(k) .apdc_c_ref(tt, ee, M[, k], horizon), numeric(1))
  }
}
# ---- Point estimates and the analytic covariance, one call per cell ---------
# All markers on one right-hand side, so a single call returns every arm's C and
# the full K x K infinitesimal-jackknife covariance, from which every paired
# se(delta) = sqrt(v_aa + v_bb - 2 v_ab) follows without another fit.
cat(sprintf("annex-pdc run: point estimates and analytic covariance for %d arms x 2 outcomes x %d horizons...\n",
            .apdc_K, length(annex_pdc$horizons))); flush.console()
.apdc_point <- list(); .apdc_V <- list()
for (oc in names(.apdc_data)) {
  dd <- .apdc_data[[oc]]
  .apdc_point[[oc]] <- list(); .apdc_V[[oc]] <- list()
  for (hh in names(dd$markers)) {
    M <- dd$markers[[hh]]
    dfr <- data.frame(time = dd$time, event = dd$event, M, check.names = FALSE)
    fml <- stats::as.formula(paste("survival::Surv(time, event) ~",
                                   paste(sprintf("`%s`", .apdc_arms), collapse = " + ")))
    fit <- survival::concordance(fml, data = dfr, timewt = "n/G2", reverse = TRUE, ymax = as.numeric(hh))
    est <- unname(fit$concordance)
    nmv <- names(fit$concordance)
    if (!is.null(nmv)) {
      ord <- match(.apdc_arms, gsub("^`|`$", "", nmv))
      stopifnot("concordance returned markers in an unexpected order" = !anyNA(ord))
      est <- est[ord]
      V <- as.matrix(fit$var)[ord, ord, drop = FALSE]
    } else {
      V <- as.matrix(fit$var)
    }
    names(est) <- .apdc_arms; dimnames(V) <- list(.apdc_arms, .apdc_arms)
    .apdc_point[[oc]][[hh]] <- est; .apdc_V[[oc]][[hh]] <- V
  }
}
# ---- The bootstrap ----------------------------------------------------------
# One job per outcome and horizon; inside a job, B replicates, each resampling
# the subjects ONCE and scoring every arm on that resample. Jobs are independent,
# so they parallelise cleanly and each worker receives one marker matrix.
.apdc_jobs_b <- list()
for (oc in names(.apdc_data)) for (hh in names(.apdc_data[[oc]]$markers))
  .apdc_jobs_b[[length(.apdc_jobs_b) + 1L]] <- list(outcome = oc, horizon = hh)
# A timed pilot on the real data, so the runtime announced below is measured
# rather than guessed, and a run that would take all night is visible up front.
.apdc_pilot <- local({
  dd <- .apdc_data[[1]]; M <- dd$markers[[1]]; h <- as.numeric(names(dd$markers)[1])
  t0 <- Sys.time()
  for (i in 1:3) { s <- sample.int(dd$n, dd$n, replace = TRUE)
    .apdc_score_all(dd$time[s], dd$event[s], M[s, , drop = FALSE], h) }
  as.numeric(difftime(Sys.time(), t0, units = "secs")) / 3
})
cat(sprintf("annex-pdc run: measured %.3f s per replicate for %d arms; %d jobs x B = %d -> about %.1f min sequential.\n",
            .apdc_pilot, .apdc_K, length(.apdc_jobs_b), ANNEX_PDC_B,
            .apdc_pilot * ANNEX_PDC_B * length(.apdc_jobs_b) / 60)); flush.console()
.apdc_cluster <- NULL
if (!is.null(ANNEX_PDC_WORKERS) && as.integer(ANNEX_PDC_WORKERS) > 1L &&
    requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)) {
  nw <- as.integer(ANNEX_PDC_WORKERS)
  options(future.globals.maxSize = 4 * 1024^3)
  if (requireNamespace("parallelly", quietly = TRUE)) {
    cat(sprintf("annex-pdc run: starting %d PSOCK workers without project startup files.\n", nw)); flush.console()
    .apdc_cluster <- parallelly::makeClusterPSOCK(nw, rscript_args = "--no-init-file",
                                                  rscript_libs = .libPaths(), connectTimeout = 300,
                                                  validate = TRUE, autoStop = FALSE)
    future::plan(future::cluster, workers = .apdc_cluster)
  } else {
    future::plan(future::multisession, workers = nw)
  }
  cat(sprintf("annex-pdc run: with %d workers expect about %.1f min.\n",
              nw, .apdc_pilot * ANNEX_PDC_B * length(.apdc_jobs_b) / 60 / nw))
}
.apdc_one_job <- function(job_index) {
  job <- .apdc_jobs_b[[job_index]]
  dd <- .apdc_data[[job$outcome]]; M <- dd$markers[[job$horizon]]; h <- as.numeric(job$horizon)
  set.seed(ANNEX_PDC_SEED + job_index, kind = "Mersenne-Twister")
  out <- matrix(NA_real_, ncol(M), ANNEX_PDC_B, dimnames = list(colnames(M), NULL))
  for (b in seq_len(ANNEX_PDC_B)) {
    s <- sample.int(dd$n, dd$n, replace = TRUE)
    out[, b] <- .apdc_score_all(dd$time[s], dd$event[s], M[s, , drop = FALSE], h)
  }
  out
}
cat("annex-pdc run: bootstrapping...\n"); flush.console()
.apdc_t0 <- Sys.time()
.apdc_use_par <- requireNamespace("future", quietly = TRUE) &&
  requireNamespace("future.apply", quietly = TRUE) && future::nbrOfWorkers() > 1L
.apdc_res <- if (.apdc_use_par) {
  future.apply::future_lapply(seq_along(.apdc_jobs_b), .apdc_one_job,
                              future.seed = TRUE, future.packages = "survival")
} else {
  lapply(seq_along(.apdc_jobs_b), function(i) {
    r <- .apdc_one_job(i)
    cat(sprintf("  job %d/%d (%s @ %s m) done, %.1f min elapsed\n", i, length(.apdc_jobs_b),
                .apdc_jobs_b[[i]]$outcome, .apdc_jobs_b[[i]]$horizon,
                as.numeric(difftime(Sys.time(), .apdc_t0, units = "mins")))); flush.console()
    r
  })
}
if (!is.null(.apdc_cluster)) {
  try(future::plan(future::sequential), silent = TRUE)
  try(parallel::stopCluster(.apdc_cluster), silent = TRUE)
  .apdc_cluster <- NULL
}
cat(sprintf("annex-pdc run: bootstrap finished in %.1f min.\n",
            as.numeric(difftime(Sys.time(), .apdc_t0, units = "mins"))))
annex_pdc_boot <- list()
for (i in seq_along(.apdc_jobs_b)) {
  job <- .apdc_jobs_b[[i]]
  if (is.null(annex_pdc_boot[[job$outcome]])) annex_pdc_boot[[job$outcome]] <- list()
  annex_pdc_boot[[job$outcome]][[job$horizon]] <- .apdc_res[[i]]
}
.apdc_q <- function(v, p) { f <- v[is.finite(v)]
  if (length(f) < 20L) NA_real_ else stats::quantile(f, probs = p, names = FALSE) }
# ---- Per-arm C, with the bootstrap interval from the same replicates --------
annex_pdc_bymodel <- do.call(rbind, lapply(names(.apdc_data), function(oc) {
  dd <- .apdc_data[[oc]]
  do.call(rbind, lapply(names(dd$markers), function(hh) {
    h <- as.numeric(hh); est <- .apdc_point[[oc]][[hh]]; V <- .apdc_V[[oc]][[hh]]
    B <- annex_pdc_boot[[oc]][[hh]]
    se <- sqrt(pmax(diag(V), 0))
    data.frame(outcome = oc, model = .apdc_arms, family = unname(annex_pdc$family[.apdc_arms]),
               horizon = h, n = dd$n, events_by_horizon = sum(dd$event == 1 & dd$time <= h),
               C = unname(est[.apdc_arms]),
               C_boot_lower = vapply(.apdc_arms, function(a) .apdc_q(B[a, ], 0.025), 0),
               C_boot_upper = vapply(.apdc_arms, function(a) .apdc_q(B[a, ], 0.975), 0),
               C_ij_lower = pmax(0, unname(est[.apdc_arms]) - .apdc_crit * se[.apdc_arms]),
               C_ij_upper = pmin(1, unname(est[.apdc_arms]) + .apdc_crit * se[.apdc_arms]),
               stringsAsFactors = FALSE, row.names = NULL)
  }))
}))
rownames(annex_pdc_bymodel) <- NULL
# ---- Pairwise table, with the identical-formula skip made explicit ----------
.apdc_key <- function(x) gsub("[[:space:]]+", "", x)
.apdc_prs <- utils::combn(.apdc_arms, 2L, simplify = FALSE)
annex_pdc_skipped <- do.call(rbind, lapply(c("readmission", "death"), function(oc) {
  do.call(rbind, lapply(.apdc_prs, function(p) {
    if (!identical(.apdc_key(annex_pdc$f_txt[[oc]][[p[1]]]), .apdc_key(annex_pdc$f_txt[[oc]][[p[2]]]))) return(NULL)
    data.frame(outcome = oc, model_A = p[1], model_B = p[2],
               family_A = unname(annex_pdc$family[[p[1]]]), family_B = unname(annex_pdc$family[[p[2]]]),
               reason = "identical formula for this outcome: 1 - S(t) is the same marker, delta C is exactly 0",
               stringsAsFactors = FALSE)
  }))
}))
annex_pdc_table <- do.call(rbind, lapply(c("readmission", "death"), function(oc) {
  dd <- .apdc_data[[oc]]
  do.call(rbind, lapply(.apdc_prs, function(p) {
    if (identical(.apdc_key(annex_pdc$f_txt[[oc]][[p[1]]]), .apdc_key(annex_pdc$f_txt[[oc]][[p[2]]]))) return(NULL)
    do.call(rbind, lapply(names(dd$markers), function(hh) {
      h <- as.numeric(hh); est <- .apdc_point[[oc]][[hh]]; V <- .apdc_V[[oc]][[hh]]
      B <- annex_pdc_boot[[oc]][[hh]]
      d_boot <- B[p[1], ] - B[p[2], ]
      delta <- unname(est[p[1]] - est[p[2]])
      se_ij <- sqrt(max(V[p[1], p[1]] + V[p[2], p[2]] - 2 * V[p[1], p[2]], 0))
      lo <- .apdc_q(d_boot, 0.025); hi <- .apdc_q(d_boot, 0.975)
      data.frame(
        outcome = oc, model_A = p[1], model_B = p[2],
        family_A = unname(annex_pdc$family[[p[1]]]), family_B = unname(annex_pdc$family[[p[2]]]),
        horizon = h, n = dd$n, events_by_horizon = sum(dd$event == 1 & dd$time <= h),
        C_A = unname(est[p[1]]), C_B = unname(est[p[2]]), delta_C = delta,
        delta_boot_lower = lo, delta_boot_upper = hi,
        boot_mean = mean(d_boot[is.finite(d_boot)]),
        centering_bias = mean(d_boot[is.finite(d_boot)]) - delta,
        point_inside_boot_ci = is.finite(lo) && is.finite(hi) && delta >= lo && delta <= hi,
        B_valid = sum(is.finite(d_boot)), B_requested = ANNEX_PDC_B,
        se_delta_ij = se_ij, delta_ij_lower = delta - .apdc_crit * se_ij,
        delta_ij_upper = delta + .apdc_crit * se_ij,
        boot_over_ij_width = if (se_ij > 0 && is.finite(lo) && is.finite(hi))
          (hi - lo) / (2 * .apdc_crit * se_ij) else NA_real_,
        stringsAsFactors = FALSE, row.names = NULL)
    }))
  }))
}))
annex_pdc_table$favours <- ifelse(
  is.finite(annex_pdc_table$delta_boot_lower) & annex_pdc_table$delta_boot_lower > 0, annex_pdc_table$model_A,
  ifelse(is.finite(annex_pdc_table$delta_boot_upper) & annex_pdc_table$delta_boot_upper < 0, annex_pdc_table$model_B,
         "No clear difference"))
annex_pdc_table$comparison <- ifelse(
  annex_pdc_table$family_A != annex_pdc_table$family_B, "Cox vs DeepSurv-informed",
  paste("within", annex_pdc_table$family_A))
annex_pdc_table$excludes_zero <- is.finite(annex_pdc_table$delta_boot_lower) &
  is.finite(annex_pdc_table$delta_boot_upper) &
  (annex_pdc_table$delta_boot_lower > 0 | annex_pdc_table$delta_boot_upper < 0)
annex_pdc_table$meets_promotion_threshold <-
  is.finite(annex_pdc_table$delta_C) & abs(annex_pdc_table$delta_C) >= ANNEX_PDC_PROMOTION_DELTA
annex_pdc_table <- annex_pdc_table[order(annex_pdc_table$outcome, annex_pdc_table$horizon,
                                         -abs(annex_pdc_table$delta_C)), ]
rownames(annex_pdc_table) <- NULL
# ---- Diagnostics on the bootstrap itself ------------------------------------
# Two things a reviewer will ask. Is the bootstrap centred on the point estimate,
# and does its interval agree with the analytic one. A ratio near 1 means the two
# routes concur and the analytic interval was never the problem; a ratio far from
# 1 is the reason for having run the bootstrap in the first place.
cat(sprintf("\nBootstrap diagnostics: max |centering bias| = %.5f; point inside its own interval in %d of %d rows.\n",
            max(abs(annex_pdc_table$centering_bias), na.rm = TRUE),
            sum(annex_pdc_table$point_inside_boot_ci, na.rm = TRUE), nrow(annex_pdc_table)))
cat(sprintf("Bootstrap interval width relative to the analytic one: median %.3f, range %.3f to %.3f.\n",
            stats::median(annex_pdc_table$boot_over_ij_width, na.rm = TRUE),
            min(annex_pdc_table$boot_over_ij_width, na.rm = TRUE),
            max(annex_pdc_table$boot_over_ij_width, na.rm = TRUE)))
if (any(!annex_pdc_table$point_inside_boot_ci, na.rm = TRUE))
  warning("annex-pdc: ", sum(!annex_pdc_table$point_inside_boot_ci, na.rm = TRUE),
          " point estimates fall outside their own bootstrap interval. Inspect before reporting.", call. = FALSE)
# ---- Reconciliation against the existing table ------------------------------
# The point estimates use the same estimator and the same subject set as
# `paired-delta-c-analytic`, so the within-Cox delta_C values must reproduce
# delta_C_analytic_table exactly. This is checked, not asserted. The interval is
# not compared: that table's interval is analytic and this one's is bootstrap.
annex_pdc_reconciliation <- NULL
if (exists("delta_C_analytic_table", inherits = TRUE) && is.data.frame(delta_C_analytic_table)) {
  .apdc_k2 <- function(d) paste(d$outcome, d$model_A, d$model_B, d$horizon, sep = "|")
  .apdc_old <- delta_C_analytic_table
  .apdc_new <- annex_pdc_table[annex_pdc_table$comparison == "within Cox", , drop = FALSE]
  mm <- match(.apdc_k2(.apdc_old), .apdc_k2(.apdc_new)); hit <- !is.na(mm)
  if (any(hit)) {
    d_delta <- abs(.apdc_old$delta_C[hit] - .apdc_new$delta_C[mm[hit]])
    d_ca <- abs(.apdc_old$C_A[hit] - .apdc_new$C_A[mm[hit]])
    d_ij <- abs(.apdc_old$delta_C_lower[hit] - .apdc_new$delta_ij_lower[mm[hit]])
    annex_pdc_reconciliation <- data.frame(
      rows_in_existing_table = nrow(.apdc_old), rows_matched = sum(hit),
      max_abs_diff_delta_C = max(d_delta, na.rm = TRUE),
      max_abs_diff_C_A = max(d_ca, na.rm = TRUE),
      max_abs_diff_analytic_lower = max(d_ij, na.rm = TRUE), stringsAsFactors = FALSE)
    cat("\nReconciliation against the live delta_C_analytic_table:\n")
    print(annex_pdc_reconciliation, row.names = FALSE, digits = 3)
    if (max(d_delta, na.rm = TRUE) > 1e-8)
      warning("annex-pdc: the within-Cox point estimates do NOT reproduce delta_C_analytic_table ",
              "(max |difference| = ", signif(max(d_delta, na.rm = TRUE), 3),
              "). Investigate before presenting this as an extension of that table.", call. = FALSE)
    else cat("The within-Cox point estimates reproduce the existing table exactly.\n")
  }
} else {
  cat("\ndelta_C_analytic_table is not in the kernel, so no reconciliation was run.\n")
}
# ---- Report ------------------------------------------------------------------
cat("\n-- Per-arm horizon-specific Uno's C on predicted risk, out-of-fold development sample --\n")
print(annex_pdc_bymodel[, c("outcome", "model", "family", "horizon", "n", "events_by_horizon",
                            "C", "C_boot_lower", "C_boot_upper")], row.names = FALSE, digits = 4)
cat("\n-- NEW ROWS: Cox versus DeepSurv-informed candidates (bootstrap interval) --\n")
.apdc_cross <- annex_pdc_table[annex_pdc_table$comparison == "Cox vs DeepSurv-informed", , drop = FALSE]
if (nrow(.apdc_cross)) {
  print(.apdc_cross[, .apdc_cols(.apdc_cross, c("comparison", "family_A", "family_B")), drop = FALSE],
        row.names = FALSE, digits = 4)
  cat(sprintf("\nLargest |delta C| in the Cox versus DeepSurv-informed block: %.5f (promotion threshold %.2f).\n",
              max(abs(.apdc_cross$delta_C), na.rm = TRUE), ANNEX_PDC_PROMOTION_DELTA))
  cat(if (any(.apdc_cross$meets_promotion_threshold, na.rm = TRUE))
        "At least one DeepSurv-informed candidate differs from a Cox candidate by enough to matter under the project's promotion rule.\n"
      else
        "No DeepSurv-informed candidate differs from a Cox candidate by enough to matter under the project's promotion rule, so on discrimination the two families are interchangeable and the choice rests on calibration, parsimony and interpretability.\n")
} else {
  cat("None. Set ANNEX_PDC_INCLUDE_DS <- TRUE and rerun the build cell.\n")
}
if (!is.null(annex_pdc_skipped)) {
  cat("\n-- Pairs excluded by the identical-formula rule (delta C is exactly 0 on 1 - S(t)) --\n")
  print(annex_pdc_skipped, row.names = FALSE)
}
cat("\n-- Full extended table --\n")
print(annex_pdc_table[, .apdc_cols(annex_pdc_table, c("family_A", "family_B")), drop = FALSE],
      row.names = FALSE, digits = 4)
# ---- Save --------------------------------------------------------------------
# CSV under cons/_out (text, safe on Drive); the R objects under data/, never
# under cons/, per the project rule on .rds placement.
.apdc_outdir <- file.path(.apdc_root, "cons", "_out")
dir.create(.apdc_outdir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(annex_pdc_table[, .apdc_cols(annex_pdc_table), drop = FALSE],
                 file.path(.apdc_outdir, "pred225_annex_paired_delta_c_risk_ext.csv"), row.names = FALSE)
utils::write.csv(annex_pdc_bymodel, file.path(.apdc_outdir, "pred225_annex_paired_delta_c_risk_ext_bymodel.csv"), row.names = FALSE)
if (!is.null(annex_pdc_skipped))
  utils::write.csv(annex_pdc_skipped, file.path(.apdc_outdir, "pred225_annex_paired_delta_c_risk_ext_skipped.csv"), row.names = FALSE)
saveRDS(list(table = annex_pdc_table, bymodel = annex_pdc_bymodel, skipped = annex_pdc_skipped,
             bootstrap_replicates = annex_pdc_boot, reconciliation = annex_pdc_reconciliation,
             family = annex_pdc$family, fold_signature = annex_pdc$fold_signature,
             same_folds = annex_pdc$same_folds, shared_subject_set = .apdc_mask_ok,
             fast_counter_used = .apdc_use_fast, provenance = annex_pdc$provenance,
             B = ANNEX_PDC_B, seed = ANNEX_PDC_SEED, horizons = annex_pdc$horizons,
             interval = "percentile bootstrap, primary; infinitesimal jackknife, secondary",
             created = as.character(Sys.time())),
        file.path(.apdc_root, "data", "20241015_out", "pred225_annex_paired_delta_c_risk_ext.rds"))
cat("\nSaved: cons/_out/pred225_annex_paired_delta_c_risk_ext*.csv and\n")
cat("data/20241015_out/pred225_annex_paired_delta_c_risk_ext.rds\n")
