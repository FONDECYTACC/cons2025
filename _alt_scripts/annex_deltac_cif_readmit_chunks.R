# =============================================================================
# annex_deltac_cif_readmit_chunks.R  (2026-08-22)
#
# TWO SELF-CONTAINED notebook chunks for an ANNEX of
# cons/prediction23_converted_mod.ipynb: a PAIRED delta C for READMISSION on the
# held-out 20% test set, scored on the COMPETING-RISK CUMULATIVE INCIDENCE
# instead of the cause-specific net risk 1 - S(t).
#
#   arm A = readm::bp1 = readmission CIF conditional on the FULL PH mortality
#           model (models$best_perf1$death = formula_death_updated2)
#   arm B = readm::bp2 = readmission CIF conditional on the SHAP / ML mortality
#           model (models$best_perf2$death = formula_shap_death_rule2)
#
# READMISSION ONLY. Mortality is not touched: all-cause death has no competing
# event, so its CIF and its net risk coincide and the existing
# delta_c_holdout_absrisk() output for death already answers that question.
#
# WHY THIS IS NOT THE EXISTING `paired-delta-c-absolute-risk`. That cell (and
# delta_c_holdout_absrisk() in cons/_alt_scripts/delta_c_holdout.R) ranks
# subjects by 1 - S_readmission(t), which treats death as ordinary censoring.
# best_perf1 and best_perf2 share the SAME readmission formula, so on that scale
# their readmission delta C is exactly 0 by construction and the comparison is
# vacuous. The CIF is a functional of BOTH cause-specific hazards,
#     CIF_1(t) = integral_0^t S_all(u-) dH_1(u),   S_all = S_1 * S_2,
# so swapping the paired mortality model does move the predicted absolute
# readmission risk, and whether it moves the RANKING (hence Uno's C) is the
# empirical question this annex settles.
#
# CHUNK 1  `annex-deltac-cif-readmit-inputs`  assembles and caches the inputs.
# CHUNK 2  `annex-deltac-cif-readmit-run`     computes the paired delta C.
#
# Both chunks are self-contained: they resolve the project root themselves,
# reuse live kernel objects when present, otherwise recover from the frozen
# holdout workspace on disk, otherwise rebuild the two joint CIF caches from
# source. No notebook cell needs to have been run first.
#
# Placement: paste each block as its own cell at the END of
# cons/prediction23_converted_mod.ipynb, AFTER the labelled cell
# `dca-refine-annex`. Both ADD objects; neither replaces anything.
# =============================================================================

# ---------------------------------------------------------------------------
# CHUNK 1 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-deltac-cif-readmit-inputs
#| message: true
#| warning: true
# ============================================================================
# ANNEX PAIRED DELTA C ON THE READMISSION CIF, STEP 1/2: assemble the inputs.
# Produces one object, `annex_dcr`, holding everything chunk 2 needs:
#   $time, $status   competing-risk first-event outcome on the 20% validation
#                    rows (status 0 = censored, 1 = readmission, 2 = death
#                    before readmission; simultaneous readmission and death is
#                    counted as death first, the project convention already
#                    used by .vhm_first_event() and .holdout_cr_first_event())
#   $cif_A, $cif_B   MI-pooled readmission CIF matrices (rows = validation
#                    subjects, columns = eval_times) for arm A (mortality =
#                    Full PH) and arm B (mortality = SHAP / ML)
#   $cif_A_imp, $cif_B_imp   the same, per imputation, so chunk 2 can show the
#                    result is not an artefact of MI pooling
#   $net             OPTIONAL reference marker: the cause-specific net risk
#                    1 - S_readmission(t), identical for both configurations
#                    because the readmission formula is shared. NULL if the
#                    non-injected results_boot_val_bp1 could not be located.
#   $eval_times, $horizons, $provenance, $n_imp and the event counts
# Kernel prerequisites: NONE. Live objects are reused when present, in this
# order of preference: cif_cache_bp1 / cif_cache_bp2 (the direct output of
# .holdout_build_cif_cache()), then results_boot_val_bp1_cif /
# results_boot_val_bp2_cif (the injected copies, where the readmission
# surv_val_matrix already stores 1 - CIF). Otherwise the frozen workspace
# data/20241015_out/pred23_holdout_validation_*.rds is read into a PRIVATE
# environment, and only if that also fails are the two joint CIF caches refit
# from source (10 CSC fits, a few minutes).
# Place AFTER `dca-refine-annex`. Adds objects; replaces nothing.
# ============================================================================
if (!exists("ADCR_FORCE_REBUILD", inherits = TRUE)) ADCR_FORCE_REBUILD <- FALSE
if (!exists("ADCR_USE_DISK_CACHE", inherits = TRUE)) ADCR_USE_DISK_CACHE <- TRUE
# The canonical grid. IBS is integrated over whatever grid it is handed and the
# CIF caches are built on this grid, so objects built on a different grid are
# NOT interchangeable with the frozen ones (the documented eval_times trap).
# Uno's C at a fixed horizon does not integrate, but the horizon must still be
# an exact column of the grid, which is what the check below enforces.
ADCR_EVAL_TIMES <- c(3, 6, 12, 36, 60)
ADCR_HORIZONS   <- c(6, 12, 36, 60)
stopifnot("ADCR_HORIZONS must be exact columns of ADCR_EVAL_TIMES" =
  all(ADCR_HORIZONS %in% ADCR_EVAL_TIMES))
# ---- Resolve AND VALIDATE the project root ---------------------------------
# Same cascade the annex calibration figure uses: never take `project_root` on
# faith, because running from the console commonly leaves the working directory
# at <root>/cons, which silently produces <root>/cons/cons/_alt_scripts paths.
# Accept only a candidate that actually contains cons/_alt_scripts.
.adcr_root <- local({
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
  ok <- cand[vapply(cand, function(p) dir.exists(file.path(p, "cons", "_alt_scripts")), logical(1L))]
  if (!length(ok))
    stop(sprintf("annex-deltac-cif: cannot locate the project root. Tried: %s. None contains cons/_alt_scripts.",
                 paste(cand, collapse = " | ")), call. = FALSE)
  normalizePath(ok[1L], winslash = "/", mustWork = FALSE)
})
message("annex-deltac-cif inputs: project root resolved to ", .adcr_root)
if (!exists("project_root", inherits = TRUE) ||
    !identical(normalizePath(project_root, winslash = "/", mustWork = FALSE), .adcr_root)) {
  project_root <- .adcr_root
  message("annex-deltac-cif inputs: project_root set to the resolved root (it was absent or pointed elsewhere).")
}
# The annex writes its own cache OUTSIDE cons/, per the project rule that no
# .rds may live under cons/ (Google Drive sync fragility).
ADCR_CACHE_RDS <- file.path(.adcr_root, "data", "20241015_out",
                            "pred23_annex_deltac_cif_inputs.rds")
# ---- Dependency gate -------------------------------------------------------
# survival is needed by chunk 2; riskRegression and prodlim only by the rebuild
# tier, but they are checked here so a cold machine fails in one second rather
# than after several minutes of Cox fitting.
.adcr_pkgs <- c("survival", "riskRegression", "prodlim")
.adcr_have <- vapply(.adcr_pkgs, requireNamespace, logical(1L), quietly = TRUE)
if (any(!.adcr_have))
  stop("annex-deltac-cif: missing packages: ", paste(.adcr_pkgs[!.adcr_have], collapse = ", "), call. = FALSE)
# ---- Small helpers, all duplicated here so the annex needs no source() ------
.adcr_dep <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")
.adcr_to01 <- function(x) {
  # Event coercion mirroring the project's to01(): accepts logical, 0/1 and 1/2
  # coding, and refuses anything else rather than guessing. Guessing here would
  # be silent and catastrophic: a mis-decoded event vector turns into "nobody
  # was readmitted" and every C-index collapses to a meaningless 0.5.
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  xn <- suppressWarnings(as.numeric(x))
  vals <- xn[!is.na(xn)]
  if (!length(vals)) stop("event column has no non-missing values.", call. = FALSE)
  if (all(vals %in% c(0, 1))) return(as.integer(xn))
  if (all(vals %in% c(1, 2))) return(as.integer(xn - 1L))
  stop("event column must be binary 0/1 (or 1/2).", call. = FALSE)
}
.adcr_first_event <- function(rt, re, dt, de) {
  # First-event time and cause, VERBATIM logic of .vhm_first_event() and
  # .holdout_cr_first_event(), tie convention included (simultaneous
  # readmission and death is recorded as death first). Duplicated rather than
  # sourced so this chunk stays self-contained.
  rt <- as.numeric(rt); dt <- as.numeric(dt)
  er <- .adcr_to01(re); ed <- .adcr_to01(de)
  ftime <- pmin(rt, dt)
  r1 <- !is.na(er) & er == 1L & is.finite(ftime) & abs(rt - ftime) <= 1e-8
  d1 <- !is.na(ed) & ed == 1L & is.finite(ftime) & abs(dt - ftime) <= 1e-8
  fstatus <- rep(0L, length(ftime))
  fstatus[r1 & !d1] <- 1L
  fstatus[d1 & !r1] <- 2L
  fstatus[r1 & d1]  <- 2L
  list(time = ftime, status = fstatus)
}
.adcr_place_by_idx <- function(results_boot, outcome, what = c("risk", "time_event")) {
  # Reassemble per-subject quantities from a results_boot-shaped object, one
  # matrix (or one time/event vector pair) per imputation, placed POSITIONALLY
  # through original_val_idx. Mirrors .dch_extract() in delta_c_holdout.R,
  # including its two alignment guards: the positional mapping is only valid
  # when nothing was dropped for unseen strata and when the pieces have
  # matching lengths. Without those guards a single dropped row would shift
  # readmission against death and silently corrupt the competing-risk status.
  what <- match.arg(what)
  rp <- results_boot$raw_predictions
  if (is.null(rp) || !length(rp))
    stop("object has no $raw_predictions (stripped object or a calibration result).", call. = FALSE)
  et <- as.numeric(if (!is.null(results_boot$config$eval_times)) results_boot$config$eval_times else rp[[1]]$eval_times)
  N <- max(vapply(rp, function(b) max(as.integer(b$original_val_idx)), integer(1)))
  imps <- vapply(rp, function(b) as.integer(b$imp_idx), integer(1))
  by_imp <- vector("list", length(rp))
  time_v <- rep(NA_real_, N); event_v <- rep(NA_real_, N)
  for (k in seq_along(rp)) {
    b <- rp[[k]]
    o <- b[[outcome]]
    if (is.null(o) || !is.null(o$error) || is.null(o$surv_val_matrix))
      stop(sprintf("outcome '%s', imputation %d: no usable surv_val_matrix.", outcome, imps[k]), call. = FALSE)
    dr <- if (is.null(o$dropped_rows)) 0L else as.integer(o$dropped_rows)
    if (!identical(dr, 0L))
      stop(sprintf("outcome '%s', imputation %d: dropped_rows = %d (> 0); positional row mapping is unsafe.",
                   outcome, imps[k], dr), call. = FALSE)
    idx <- as.integer(b$original_val_idx)
    sm <- as.matrix(o$surv_val_matrix)
    if (nrow(sm) != length(idx))
      stop(sprintf("outcome '%s', imputation %d: surv_val_matrix has %d rows but original_val_idx has %d.",
                   outcome, imps[k], nrow(sm), length(idx)), call. = FALSE)
    M <- matrix(NA_real_, N, ncol(sm))
    M[idx, ] <- 1 - sm
    by_imp[[k]] <- M
    yv <- o$y_val
    time_v[idx] <- as.numeric(yv[, 1]); event_v[idx] <- as.numeric(yv[, 2])
  }
  if (identical(what, "risk")) list(by_imp = by_imp[order(imps)], eval_times = et)
  else list(time = time_v, event = event_v)
}
.adcr_pool <- function(by_imp) Reduce(`+`, by_imp) / length(by_imp)
# ---- FAST PATH: a cache written by a previous run of this same chunk --------
# Validated, not trusted: the grid, the row count and the matrix dimensions
# must match what this run asks for, otherwise the cache is ignored and rebuilt.
annex_dcr <- NULL
if (!ADCR_FORCE_REBUILD && ADCR_USE_DISK_CACHE && file.exists(ADCR_CACHE_RDS)) {
  .adcr_try <- tryCatch(readRDS(ADCR_CACHE_RDS), error = function(e) NULL)
  .adcr_ok <- !is.null(.adcr_try) && is.list(.adcr_try) &&
    all(c("time", "status", "cif_A", "cif_B", "eval_times") %in% names(.adcr_try)) &&
    isTRUE(all.equal(as.numeric(.adcr_try$eval_times), ADCR_EVAL_TIMES)) &&
    nrow(.adcr_try$cif_A) == length(.adcr_try$time) &&
    identical(dim(.adcr_try$cif_A), dim(.adcr_try$cif_B))
  if (isTRUE(.adcr_ok)) {
    annex_dcr <- .adcr_try
    annex_dcr$provenance <- paste0(annex_dcr$provenance, " [reloaded from ", basename(ADCR_CACHE_RDS), "]")
    message("annex-deltac-cif inputs: reused the annex disk cache ", basename(ADCR_CACHE_RDS),
            ". Set ADCR_FORCE_REBUILD <- TRUE to ignore it.")
  } else {
    message("annex-deltac-cif inputs: annex disk cache present but it did not validate; rebuilding.")
  }
  rm(.adcr_try)
}
# ---- SOURCE RESOLUTION: kernel first, then the frozen workspace -------------
if (is.null(annex_dcr)) {
  # The frozen workspace is large (about 780 MB) so it is loaded AT MOST ONCE,
  # lazily, into a PRIVATE environment. Loading it into globalenv() would
  # overwrite 613 names including project_root, engine functions and
  # .Random.seed, which is why nothing here ever calls load() without envir=.
  .adcr_ws <- NULL
  .adcr_ws_tried <- FALSE
  .adcr_ws_env <- function() {
    if (!.adcr_ws_tried) {
      .adcr_ws_tried <<- TRUE
      ws_all <- list.files(file.path(.adcr_root, "data", "20241015_out"),
                           pattern = "^pred23_holdout_validation_.*\\.rds$", full.names = TRUE)
      if (length(ws_all)) {
        ws <- ws_all[order(file.info(ws_all)$mtime, decreasing = TRUE)][1L]
        message("annex-deltac-cif inputs: reading the frozen workspace ", basename(ws), " (",
                round(file.info(ws)$size / 1024^2), " MB). Despite the .rds extension these files ",
                "are RData written by save(), so load() is required, not readRDS().")
        .adcr_ws <<- tryCatch({
          e <- new.env(parent = emptyenv())
          nms <- load(ws, envir = e)
          message("annex-deltac-cif inputs: workspace holds ", length(nms), " objects.")
          e
        }, error = function(err) {
          message("annex-deltac-cif inputs: workspace could not be read (", conditionMessage(err), ").")
          NULL
        })
      } else {
        message("annex-deltac-cif inputs: no frozen workspace found under data/20241015_out.")
      }
    }
    .adcr_ws
  }
  .adcr_get <- function(nm) {
    # Fetch one named object, kernel first, then the frozen workspace. Returns
    # NULL when absent everywhere, so callers can fall through to the next tier.
    if (exists(nm, envir = globalenv(), inherits = FALSE))
      return(list(value = get(nm, envir = globalenv()), where = "kernel"))
    e <- .adcr_ws_env()
    if (!is.null(e) && exists(nm, envir = e, inherits = FALSE))
      return(list(value = get(nm, envir = e, inherits = FALSE), where = "frozen workspace"))
    NULL
  }
  # ---- 1. The two CIF arms -------------------------------------------------
  # Preference order and why: the raw caches from .holdout_build_cif_cache()
  # ARE the CIF, one matrix per imputation in val_list row order, so nothing
  # has to be un-transformed. The injected copies store 1 - CIF inside
  # readmission$surv_val_matrix, which every downstream consumer reads back as
  # `1 - surv_val_matrix`; recovering the CIF from them is exact but needs the
  # positional guards in .adcr_place_by_idx().
  .adcr_prov <- character(0)
  .adcr_A <- NULL; .adcr_B <- NULL
  .adcr_c1 <- if (ADCR_FORCE_REBUILD) NULL else .adcr_get("cif_cache_bp1")
  .adcr_c2 <- if (ADCR_FORCE_REBUILD) NULL else .adcr_get("cif_cache_bp2")
  if (!is.null(.adcr_c1) && !is.null(.adcr_c2)) {
    .adcr_A <- .adcr_c1$value; .adcr_B <- .adcr_c2$value
    .adcr_prov <- c(.adcr_prov, paste0("CIF arms from cif_cache_bp1/bp2 (", .adcr_c1$where, ")"))
  }
  if (is.null(.adcr_A) && !ADCR_FORCE_REBUILD) {
    .adcr_i1 <- .adcr_get("results_boot_val_bp1_cif"); .adcr_i2 <- .adcr_get("results_boot_val_bp2_cif")
    if (!is.null(.adcr_i1) && !is.null(.adcr_i2)) {
      .adcr_A <- .adcr_place_by_idx(.adcr_i1$value, "readmission", "risk")
      .adcr_B <- .adcr_place_by_idx(.adcr_i2$value, "readmission", "risk")
      .adcr_prov <- c(.adcr_prov, paste0("CIF arms from results_boot_val_bp1_cif/bp2_cif (", .adcr_i1$where, ")"))
    }
  }
  # ---- 2. Rebuild tier, only if the arms could not be located --------------
  if (is.null(.adcr_A)) {
    message("annex-deltac-cif inputs: rebuilding the two joint CIF caches from source. ",
            "This fits riskRegression::CSC() once per imputation per mortality model.")
    # survival MUST be attached, not merely installed: the frozen formulas
    # carry bare Surv(...) and strata(...) and .holdout_build_cif_cache()
    # rebuilds cause-specific formulas from RHS TEXT, whose model.frame()
    # resolves those symbols through the SEARCH PATH. Without this the rebuild
    # dies on `could not find function "strata"` after minutes of work.
    for (.adcr_att in c("survival", "prodlim", "riskRegression"))
      if (!.adcr_att %in% .packages()) suppressPackageStartupMessages(library(.adcr_att, character.only = TRUE))
    # NOTE: run_validation_holdout.R calls setwd(project_root) by design, to
    # anchor the relative source() calls its audited engines make internally.
    # The working directory therefore changes here and is deliberately NOT
    # restored, since restoring it would break those relative paths.
    for (.adcr_eng in c("cons/_alt_scripts/run_validation_holdout.R",
                        "cons/_alt_scripts/holdout_cif_cache.R")) {
      .adcr_p <- file.path(.adcr_root, .adcr_eng)
      stopifnot("engine script not found under the resolved project root" = file.exists(.adcr_p))
      source(.adcr_p)
      message("annex-deltac-cif inputs: sourced ", basename(.adcr_p))
    }
    .adcr_tl <- .adcr_get("train_list"); .adcr_vl <- .adcr_get("val_list")
    if (is.null(.adcr_tl) || is.null(.adcr_vl)) {
      .adcr_hd <- build_holdout_datasets(force = FALSE, verify = TRUE, verbose = TRUE)
      train_list <- .adcr_hd$train; val_list <- .adcr_hd$val
    } else {
      train_list <- .adcr_tl$value; val_list <- .adcr_vl$value
    }
    stopifnot("train_list and val_list must have the same number of imputations" =
      length(train_list) == length(val_list))
    # Formulas are NEVER hardcoded here. A stale local copy of a 63-term
    # formula that quietly drifts from the canonical one in
    # `holdout-define-models` is exactly the error this project cannot afford,
    # so they are recovered from the kernel or from the frozen workspace and
    # the chunk stops if neither has them.
    .adcr_m <- .adcr_get("models"); .adcr_fr <- .adcr_get("f_readmit")
    if (is.null(.adcr_m) || is.null(.adcr_fr))
      stop("annex-deltac-cif: cannot recover `models` and `f_readmit`. Run the cell `holdout-define-models` first.",
           call. = FALSE)
    models <- .adcr_m$value; f_readmit <- .adcr_fr$value
    .adcr_A <- .holdout_build_cif_cache(f_readmit, models$best_perf1$death,
                                        train_list, val_list, ADCR_EVAL_TIMES)
    .adcr_B <- .holdout_build_cif_cache(f_readmit, models$best_perf2$death,
                                        train_list, val_list, ADCR_EVAL_TIMES)
    cif_cache_bp1 <- .adcr_A; cif_cache_bp2 <- .adcr_B
    .adcr_prov <- c(.adcr_prov, "CIF arms refit from source via .holdout_build_cif_cache()")
  }
  # ---- 3. The premise check: the readmission model must be SHARED ----------
  # If it is not, a non-zero delta C no longer isolates the mortality pairing,
  # because part of it would come from a different readmission model. This is a
  # loud warning rather than an error so the chunk still runs on a variant
  # specification, but the interpretation printed by chunk 2 would change.
  .adcr_m2 <- .adcr_get("models")
  if (!is.null(.adcr_m2)) {
    .adcr_same <- identical(.adcr_dep(.adcr_m2$value$best_perf1$readmit),
                            .adcr_dep(.adcr_m2$value$best_perf2$readmit))
    if (.adcr_same) {
      message("annex-deltac-cif inputs: premise verified, best_perf1 and best_perf2 share the readmission formula, ",
              "so any delta C below is attributable ONLY to the paired mortality model.")
    } else {
      warning("annex-deltac-cif: best_perf1 and best_perf2 do NOT share the readmission formula. ",
              "The delta C then mixes two sources of difference and cannot be read as a mortality-pairing effect.",
              call. = FALSE)
    }
    .adcr_prov <- c(.adcr_prov, paste0("shared readmission formula = ", .adcr_same))
  }
  # ---- 4. The competing-risk outcome on the validation rows ---------------
  # Preferred source is val_list, because it is the raw analysis data. The
  # fallback reads the y_val blocks of a results_boot object, which the CIF
  # injection never touches. When BOTH are available they are cross-checked,
  # since a silent mismatch here would misclassify who died before readmission.
  .adcr_out <- NULL
  .adcr_vl2 <- .adcr_get("val_list")
  if (!is.null(.adcr_vl2)) {
    .adcr_v1 <- as.data.frame(.adcr_vl2$value[[1]])
    .adcr_out <- .adcr_first_event(.adcr_v1$readmit_time_from_disch_m, .adcr_v1$readmit_event,
                                   .adcr_v1$death_time_from_disch_m, .adcr_v1$death_event)
    .adcr_prov <- c(.adcr_prov, paste0("outcome from val_list (", .adcr_vl2$where, ")"))
    # Outcomes are not imputed, so every imputation must carry the same ones.
    for (.adcr_i in seq_along(.adcr_vl2$value)) {
      .adcr_vi <- as.data.frame(.adcr_vl2$value[[.adcr_i]])
      stopifnot("validation outcomes differ across imputations; they must not" =
        max(abs(as.numeric(.adcr_vi$readmit_time_from_disch_m) - as.numeric(.adcr_v1$readmit_time_from_disch_m))) < 1e-8 &&
        max(abs(as.numeric(.adcr_vi$death_time_from_disch_m) - as.numeric(.adcr_v1$death_time_from_disch_m))) < 1e-8)
    }
  }
  .adcr_rb <- .adcr_get("results_boot_val_bp1")
  if (is.null(.adcr_rb)) .adcr_rb <- .adcr_get("results_boot_val_bp2")
  if (is.null(.adcr_rb)) .adcr_rb <- .adcr_get("results_boot_val_bp1_cif")
  if (!is.null(.adcr_rb)) {
    .adcr_yr <- .adcr_place_by_idx(.adcr_rb$value, "readmission", "time_event")
    .adcr_yd <- .adcr_place_by_idx(.adcr_rb$value, "death", "time_event")
    .adcr_alt <- .adcr_first_event(.adcr_yr$time, .adcr_yr$event, .adcr_yd$time, .adcr_yd$event)
    if (is.null(.adcr_out)) {
      .adcr_out <- .adcr_alt
      .adcr_prov <- c(.adcr_prov, paste0("outcome from raw_predictions y_val (", .adcr_rb$where, ")"))
    } else {
      stopifnot("val_list and raw_predictions disagree on the competing-risk outcome" =
        length(.adcr_alt$time) == length(.adcr_out$time) &&
        max(abs(.adcr_alt$time - .adcr_out$time), na.rm = TRUE) < 1e-8 &&
        identical(.adcr_alt$status, .adcr_out$status))
      message("annex-deltac-cif inputs: competing-risk outcome cross-checked, val_list and raw_predictions agree exactly.")
    }
  }
  if (is.null(.adcr_out))
    stop("annex-deltac-cif: could not reconstruct the competing-risk outcome (no val_list, no results_boot object).",
         call. = FALSE)
  # ---- 5. OPTIONAL reference marker: the shared cause-specific net risk ----
  # 1 - S_readmission(t) taken from the NON-injected object. It is identical
  # under both configurations, which is precisely why the existing
  # delta_c_holdout_absrisk() returns 0 for readmission. Carrying it here lets
  # chunk 2 report what discrimination the competing-risk scoring gives to the
  # marker the manuscript would otherwise have used, at no extra cost.
  .adcr_net <- NULL
  .adcr_nr <- .adcr_get("results_boot_val_bp1")
  if (!is.null(.adcr_nr)) {
    .adcr_inj <- !is.null(.adcr_nr$value$config$readmission_prediction)
    if (.adcr_inj) {
      message("annex-deltac-cif inputs: results_boot_val_bp1 looks CIF-injected; skipping the net-risk reference arm.")
    } else {
      .adcr_net <- .adcr_place_by_idx(.adcr_nr$value, "readmission", "risk")
      .adcr_prov <- c(.adcr_prov, paste0("net-risk reference from results_boot_val_bp1 (", .adcr_nr$where, ")"))
    }
  }
  # ---- 6. Validate, pool over imputations, assemble -------------------------
  stopifnot("the two CIF arms have different numbers of imputations" =
    length(.adcr_A$by_imp) == length(.adcr_B$by_imp))
  stopifnot("the CIF arms were not built on the canonical grid" =
    isTRUE(all.equal(as.numeric(.adcr_A$eval_times), ADCR_EVAL_TIMES)) &&
    isTRUE(all.equal(as.numeric(.adcr_B$eval_times), ADCR_EVAL_TIMES)))
  stopifnot("CIF matrices and the outcome vector have different lengths" =
    nrow(.adcr_A$by_imp[[1]]) == length(.adcr_out$time) &&
    nrow(.adcr_B$by_imp[[1]]) == length(.adcr_out$time))
  .adcr_cifA <- .adcr_pool(.adcr_A$by_imp)
  .adcr_cifB <- .adcr_pool(.adcr_B$by_imp)
  stopifnot("pooled CIF matrices contain non-finite values" =
    all(is.finite(.adcr_cifA)) && all(is.finite(.adcr_cifB)))
  # The arms MUST differ. Identical matrices would mean both caches were built
  # with the same mortality formula, making the whole annex a tautology that
  # returns delta C = 0, indistinguishable from a genuine null result.
  .adcr_maxdiff <- max(abs(.adcr_cifA - .adcr_cifB))
  if (!(.adcr_maxdiff > 0))
    stop("annex-deltac-cif: the two CIF arms are numerically identical. Both caches were almost certainly ",
         "built with the same mortality formula; rebuild with ADCR_FORCE_REBUILD <- TRUE.", call. = FALSE)
  annex_dcr <- list(
    time = .adcr_out$time, status = .adcr_out$status,
    cif_A = .adcr_cifA, cif_B = .adcr_cifB,
    cif_A_imp = .adcr_A$by_imp, cif_B_imp = .adcr_B$by_imp,
    net = if (is.null(.adcr_net)) NULL else .adcr_pool(.adcr_net$by_imp),
    eval_times = ADCR_EVAL_TIMES, horizons = ADCR_HORIZONS,
    n_imp = length(.adcr_A$by_imp),
    label_A = "readm_cif_bp1_death_full", label_B = "readm_cif_bp2_death_shap",
    provenance = paste(.adcr_prov, collapse = "; "),
    created = as.character(Sys.time())
  )
  if (ADCR_USE_DISK_CACHE) {
    dir.create(dirname(ADCR_CACHE_RDS), recursive = TRUE, showWarnings = FALSE)
    saveRDS(annex_dcr, ADCR_CACHE_RDS)
    message("annex-deltac-cif inputs: cached to ", ADCR_CACHE_RDS)
  }
  if (!is.null(.adcr_ws)) { rm(.adcr_ws); invisible(gc()) }
}
# ---- Diagnostics printed whichever tier supplied the inputs -----------------
cat(sprintf("\n== Annex paired delta C on the readmission CIF: inputs ==\nprovenance: %s\n", annex_dcr$provenance))
cat(sprintf("validation rows = %d | imputations = %d | grid = %s\n",
            length(annex_dcr$time), annex_dcr$n_imp, paste(annex_dcr$eval_times, collapse = ", ")))
cat(sprintf("readmissions = %d (%.2f%%) | deaths before readmission = %d (%.2f%%) | censored = %d (%.2f%%)\n",
            sum(annex_dcr$status == 1L), 100 * mean(annex_dcr$status == 1L),
            sum(annex_dcr$status == 2L), 100 * mean(annex_dcr$status == 2L),
            sum(annex_dcr$status == 0L), 100 * mean(annex_dcr$status == 0L)))
cat(sprintf("net-risk reference arm available: %s\n", !is.null(annex_dcr$net)))
# How far apart the two markers are BEFORE any concordance is computed. A very
# high Spearman correlation together with a small mean absolute gap is the
# expected result, and it is exactly why the delta C is expected to be near
# zero: the mortality pairing rescales the absolute risk much more than it
# reorders subjects. Reporting this first stops a near-zero delta C from being
# read as "the two mortality models are interchangeable".
annex_dcr_marker_gap <- do.call(rbind, lapply(annex_dcr$horizons, function(h) {
  j <- which(abs(annex_dcr$eval_times - h) < 1e-8)
  a <- annex_dcr$cif_A[, j]; b <- annex_dcr$cif_B[, j]
  ra <- rank(a) / length(a); rb <- rank(b) / length(b)
  data.frame(horizon = h, mean_cif_A = mean(a), mean_cif_B = mean(b),
             mean_diff = mean(a - b), mean_abs_diff = mean(abs(a - b)), max_abs_diff = max(abs(a - b)),
             spearman_rho = stats::cor(a, b, method = "spearman"),
             pct_rank_shift_gt_1pct = 100 * mean(abs(ra - rb) > 0.01),
             stringsAsFactors = FALSE)
}))
cat("\nMarker agreement between the two arms (before any C-index):\n")
print(annex_dcr_marker_gap, row.names = FALSE, digits = 4)
cat("\nInputs ready in `annex_dcr`. Run the cell `annex-deltac-cif-readmit-run` next.\n")

# ---------------------------------------------------------------------------
# CHUNK 2 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-deltac-cif-readmit-run
#| message: true
#| warning: true
# ============================================================================
# ANNEX PAIRED DELTA C ON THE READMISSION CIF, STEP 2/2: the comparison.
#
# ESTIMAND. Uno's C for READMISSION at a fixed horizon, computed under the
# WOLBERS competing-risks convention, comparing two markers on the SAME
# patients with the SAME outcome, hence a genuinely paired difference:
#     marker A = CIF_readmission(t) conditional on the FULL PH mortality model
#     marker B = CIF_readmission(t) conditional on the SHAP / ML mortality model
# delta_C = C_A - C_B, so delta_C > 0 favours the Full PH pairing.
#
# TWO IMPLEMENTATION POINTS, both inherited from the audited engines
# (prediction225 `paired-delta-c-competing-risks` and
# cons/_alt_scripts/holdout_arm_comparison.R), not invented here.
# 1. COMPARABILITY. A subject who dies before readmission can never be
#    readmitted. Censoring them, the cause-specific convention, silently drops
#    those pairs. Instead their follow-up is pushed far beyond the whole cohort
#    and they are kept event free, so they stay comparable with every
#    readmission case. That is Wolbers' competing-risks concordance.
# 2. PAIRED VARIANCE. Both markers go into a SINGLE survival::concordance()
#    call, so the censoring weight G is estimated once on the full sample and
#    the covariance of the two concordances comes from that call's own
#    infinitesimal-jackknife matrix. se(delta) = sqrt(v11 + v22 - 2*v12), which
#    is far smaller than either marginal SE because the two markers are almost
#    perfectly correlated. Comparing two marginal CIs instead would be wrong.
# A paired patient bootstrap is reported alongside as a cross-check, with the
# SAME resampled subjects feeding both markers in every replicate. Note the
# quantile call uses `probs =`, not `probabilities =`: the latter fell through
# `...` into quantile()'s defaults and returned the sample minimum and the 25th
# percentile as "95% bounds" in an earlier version of the prediction225 cell.
#
# KNOWN CAVEAT, stated so a reviewer does not have to find it. Pushing the
# competing deaths to a far time keeps them in the at-risk set of the censoring
# Kaplan-Meier that supplies the n/G2 weights, so G is slightly optimistic.
# This is the standard implementation of the Wolbers C in this project, it is
# IDENTICAL for both arms, and it therefore cancels in the paired difference;
# it can shift the ABSOLUTE level of C by a small amount, not the delta.
#
# Requires in kernel: `annex_dcr` from the cell `annex-deltac-cif-readmit-inputs`
# (which also leaves `.adcr_root` behind, used here only to write the outputs).
# Place AFTER that cell. Adds objects; replaces nothing.
# ============================================================================
stopifnot("run the cell `annex-deltac-cif-readmit-inputs` first" = exists("annex_dcr", inherits = TRUE))
stopifnot("run the cell `annex-deltac-cif-readmit-inputs` first" = exists(".adcr_root", inherits = TRUE))
stopifnot(requireNamespace("survival", quietly = TRUE))
if (!exists("ADCR_B", inherits = TRUE)) ADCR_B <- 500L
if (!exists("ADCR_SEED", inherits = TRUE)) ADCR_SEED <- 2125L
if (!exists("ADCR_BOOTSTRAP", inherits = TRUE)) ADCR_BOOTSTRAP <- TRUE
if (!exists("ADCR_PARALLEL", inherits = TRUE)) ADCR_PARALLEL <- TRUE
if (!exists("ADCR_BY_IMPUTATION", inherits = TRUE)) ADCR_BY_IMPUTATION <- TRUE
# The project's promotion rule: about 0.01 or more in Uno's C is a meaningful
# gain. Anything below that is numerical, not decisional, and the simpler or
# already approved model stays. Reported as its own column so statistical
# separation is never silently read as decisional relevance.
if (!exists("ADCR_PROMOTION_DELTA", inherits = TRUE)) ADCR_PROMOTION_DELTA <- 0.01
ADCR_CRIT <- stats::qnorm(0.975)
# ---- Wolbers recoding, done once -------------------------------------------
.adcr_time <- as.numeric(annex_dcr$time)
.adcr_status <- as.integer(annex_dcr$status)
.adcr_big <- max(.adcr_time, na.rm = TRUE) * 10
.adcr_tw <- ifelse(.adcr_status == 2L, .adcr_big, .adcr_time)
.adcr_ew <- as.integer(.adcr_status == 1L)
.adcr_col <- function(h) {
  j <- which(abs(as.numeric(annex_dcr$eval_times) - h) < 1e-8)
  if (length(j) != 1L)
    stop(sprintf("horizon %s is not exactly one column of the grid %s", h,
                 paste(annex_dcr$eval_times, collapse = ", ")), call. = FALSE)
  j
}
.adcr_pair_fit <- function(rA, rB, horizon, tw, ew) {
  # ONE concordance call, TWO markers, so the paired covariance is exact and no
  # bootstrap is needed for the primary interval.
  d <- data.frame(tw = tw, ew = ew, risk_A = rA, risk_B = rB)
  tryCatch(survival::concordance(survival::Surv(tw, ew) ~ risk_A + risk_B, data = d,
                                 timewt = "n/G2", reverse = TRUE, ymax = horizon),
           error = function(e) NULL)
}
.adcr_row <- function(rA, rB, horizon, label_A, label_B, bootstrap, seed) {
  ok <- is.finite(rA) & is.finite(rB) & is.finite(.adcr_tw) & is.finite(.adcr_ew)
  tt <- .adcr_time[ok]; st <- .adcr_status[ok]
  rA <- rA[ok]; rB <- rB[ok]; tw <- .adcr_tw[ok]; ew <- .adcr_ew[ok]
  n <- length(tw)
  n_case <- sum(ew == 1L & tw <= horizon)
  n_comp <- sum(st == 2L & tt <= horizon)
  empty <- data.frame(marker_A = label_A, marker_B = label_B, horizon = horizon, n = n,
                      readmissions_by_horizon = n_case, competing_deaths_by_horizon = n_comp,
                      C_A = NA_real_, C_A_lower = NA_real_, C_A_upper = NA_real_,
                      C_B = NA_real_, C_B_lower = NA_real_, C_B_upper = NA_real_,
                      delta_C = NA_real_, se_delta_ij = NA_real_,
                      delta_ij_lower = NA_real_, delta_ij_upper = NA_real_,
                      excludes_zero_ij = NA, spearman_rho = NA_real_,
                      stringsAsFactors = FALSE)
  # A degenerate horizon must still return the SAME columns as a successful
  # one, otherwise the rbind that assembles the table fails halfway through and
  # every completed horizon is lost with it.
  if (isTRUE(bootstrap))
    empty <- cbind(empty, data.frame(boot_lower = NA_real_, boot_upper = NA_real_,
                                     boot_mean = NA_real_, centering_bias = NA_real_,
                                     point_inside_boot_ci = NA, excludes_zero_boot = NA,
                                     B_valid = 0L, parallel = NA, stringsAsFactors = FALSE))
  if (n_case < 2L) return(empty)
  fit <- .adcr_pair_fit(rA, rB, horizon, tw, ew)
  if (is.null(fit)) return(empty)
  # Guard the ordering rather than trusting it: survival returns the
  # concordances in right-hand-side order, and the SIGN of delta_C depends on
  # that. A silent reordering would flip which model the annex favours.
  if (!is.null(names(fit$concordance)))
    stopifnot("concordance returned the markers in an unexpected order" =
      identical(names(fit$concordance), c("risk_A", "risk_B")))
  est <- unname(fit$concordance)
  V <- fit$var
  se_c <- sqrt(pmax(diag(V), 0))
  delta <- est[1] - est[2]
  se_delta <- sqrt(max(V[1, 1] + V[2, 2] - 2 * V[1, 2], 0))
  out <- data.frame(
    marker_A = label_A, marker_B = label_B, horizon = horizon, n = n,
    readmissions_by_horizon = n_case, competing_deaths_by_horizon = n_comp,
    C_A = est[1], C_A_lower = max(0, est[1] - ADCR_CRIT * se_c[1]), C_A_upper = min(1, est[1] + ADCR_CRIT * se_c[1]),
    C_B = est[2], C_B_lower = max(0, est[2] - ADCR_CRIT * se_c[2]), C_B_upper = min(1, est[2] + ADCR_CRIT * se_c[2]),
    delta_C = delta, se_delta_ij = se_delta,
    delta_ij_lower = delta - ADCR_CRIT * se_delta, delta_ij_upper = delta + ADCR_CRIT * se_delta,
    excludes_zero_ij = (delta - ADCR_CRIT * se_delta > 0) || (delta + ADCR_CRIT * se_delta < 0),
    spearman_rho = stats::cor(rA, rB, method = "spearman"),
    stringsAsFactors = FALSE
  )
  if (isTRUE(bootstrap)) {
    one <- function(b) {
      s <- sample.int(n, n, replace = TRUE)
      f <- .adcr_pair_fit(rA[s], rB[s], horizon, tw[s], ew[s])
      if (is.null(f)) NA_real_ else unname(f$concordance[1] - f$concordance[2])
    }
    # Parallel ONLY when the caller already installed a non-sequential plan,
    # the same convention as .holdout_cindex_boot_dualscore(). This chunk never
    # spawns workers behind the user's back, which is the oversubscription
    # problem already flagged in the ici-death-models-apply cell.
    use_par <- isTRUE(ADCR_PARALLEL) && requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE) && future::nbrOfWorkers() > 1L
    d <- if (use_par) {
      future.apply::future_vapply(seq_len(ADCR_B), one, numeric(1),
                                  future.seed = seed, future.packages = "survival")
    } else {
      set.seed(seed, kind = "Mersenne-Twister")
      vapply(seq_len(ADCR_B), one, numeric(1))
    }
    d <- d[is.finite(d)]
    if (length(d) < ceiling(0.9 * ADCR_B))
      warning(sprintf("only %d of %d bootstrap replicates were valid at %s months", length(d), ADCR_B, horizon),
              call. = FALSE)
    q <- if (length(d) >= 20L) stats::quantile(d, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
         else c(NA_real_, NA_real_)
    out$boot_lower <- q[1]; out$boot_upper <- q[2]
    out$boot_mean <- mean(d)
    out$centering_bias <- mean(d) - delta
    out$point_inside_boot_ci <- is.finite(q[1]) && is.finite(q[2]) && delta >= q[1] && delta <= q[2]
    out$excludes_zero_boot <- is.finite(q[1]) && is.finite(q[2]) && (q[1] > 0 || q[2] < 0)
    out$B_valid <- length(d)
    out$parallel <- use_par
  }
  out
}
# ---- Arms in play ----------------------------------------------------------
# The decisional pair is first. The net-risk arm, when available, is a
# REFERENCE only: it is the SAME marker under both configurations, which is
# exactly why delta_c_holdout_absrisk() returns 0 for readmission and why this
# annex exists at all.
adcr_arms <- list()
adcr_arms[[annex_dcr$label_A]] <- annex_dcr$cif_A
adcr_arms[[annex_dcr$label_B]] <- annex_dcr$cif_B
if (!is.null(annex_dcr$net)) adcr_arms[["readm_netrisk_shared"]] <- annex_dcr$net
adcr_pairs <- utils::combn(names(adcr_arms), 2L, simplify = FALSE)
cat(sprintf("\n== Paired delta C, readmission, competing-risk (Wolbers) scoring ==\narms: %s\nhorizons: %s | bootstrap: %s (B = %d, seed = %d)\n",
            paste(names(adcr_arms), collapse = ", "), paste(annex_dcr$horizons, collapse = ", "),
            ADCR_BOOTSTRAP, ADCR_B, ADCR_SEED)); flush.console()
.adcr_t0 <- Sys.time()
.adcr_k <- 0L
annex_dcr_table <- do.call(rbind, lapply(adcr_pairs, function(pr) {
  do.call(rbind, lapply(annex_dcr$horizons, function(h) {
    .adcr_k <<- .adcr_k + 1L
    j <- .adcr_col(h)
    r <- .adcr_row(adcr_arms[[pr[1]]][, j], adcr_arms[[pr[2]]][, j], h, pr[1], pr[2],
                   bootstrap = ADCR_BOOTSTRAP, seed = ADCR_SEED + .adcr_k)
    cat(sprintf("[%d/%d] %s vs %s @ %2d m: C_A=%.4f C_B=%.4f  dC=%+.5f  IJ[%+.5f, %+.5f]%s  (%.0f s)\n",
                .adcr_k, length(adcr_pairs) * length(annex_dcr$horizons), pr[1], pr[2], h,
                r$C_A, r$C_B, r$delta_C, r$delta_ij_lower, r$delta_ij_upper,
                if (!is.null(r$boot_lower)) sprintf("  boot[%+.5f, %+.5f]", r$boot_lower, r$boot_upper) else "",
                as.numeric(difftime(Sys.time(), .adcr_t0, units = "secs")))); flush.console()
    r
  }))
}))
annex_dcr_table$favours <- ifelse(
  is.finite(annex_dcr_table$delta_ij_lower) & annex_dcr_table$delta_ij_lower > 0, annex_dcr_table$marker_A,
  ifelse(is.finite(annex_dcr_table$delta_ij_upper) & annex_dcr_table$delta_ij_upper < 0, annex_dcr_table$marker_B,
         "no clear difference"))
# Statistical separation and decisional relevance are different questions. At
# n around 17,600 an interval can exclude zero while the difference sits two
# orders of magnitude below the promotion threshold, so both are reported.
annex_dcr_table$meets_promotion_threshold <-
  is.finite(annex_dcr_table$delta_C) & abs(annex_dcr_table$delta_C) >= ADCR_PROMOTION_DELTA
rownames(annex_dcr_table) <- NULL
cat(sprintf("\nCompleted in %.1f seconds.\n", as.numeric(difftime(Sys.time(), .adcr_t0, units = "secs"))))
# ---- Secondary: the same delta, per imputation, analytic CI only ------------
# Guards against the objection that averaging the CIF over the 5 imputations
# smooths away a difference that exists within each completed dataset.
annex_dcr_by_imp <- NULL
if (isTRUE(ADCR_BY_IMPUTATION) && !is.null(annex_dcr$cif_A_imp)) {
  annex_dcr_by_imp <- do.call(rbind, lapply(seq_along(annex_dcr$cif_A_imp), function(i) {
    do.call(rbind, lapply(annex_dcr$horizons, function(h) {
      j <- .adcr_col(h)
      r <- .adcr_row(annex_dcr$cif_A_imp[[i]][, j], annex_dcr$cif_B_imp[[i]][, j], h,
                     annex_dcr$label_A, annex_dcr$label_B, bootstrap = FALSE, seed = ADCR_SEED)
      r$imputation <- i
      r
    }))
  }))
  annex_dcr_by_imp <- annex_dcr_by_imp[, c("imputation", "horizon", "C_A", "C_B", "delta_C",
                                           "se_delta_ij", "delta_ij_lower", "delta_ij_upper",
                                           "excludes_zero_ij", "spearman_rho")]
  rownames(annex_dcr_by_imp) <- NULL
}
# ---- Report ----------------------------------------------------------------
.adcr_main <- annex_dcr_table[annex_dcr_table$marker_A == annex_dcr$label_A &
                              annex_dcr_table$marker_B == annex_dcr$label_B, , drop = FALSE]
cat("\n-- Decisional comparison: readmission CIF paired with the Full PH mortality model (A)\n")
cat("   versus the SAME readmission model paired with the SHAP / ML mortality model (B) --\n")
print(.adcr_main[, intersect(c("horizon", "readmissions_by_horizon", "competing_deaths_by_horizon",
                               "C_A", "C_B", "delta_C", "se_delta_ij", "delta_ij_lower", "delta_ij_upper",
                               "boot_lower", "boot_upper", "excludes_zero_ij", "favours",
                               "meets_promotion_threshold", "spearman_rho"), names(.adcr_main))],
      row.names = FALSE, digits = 4)
if (!is.null(annex_dcr_by_imp)) {
  cat("\n-- Same comparison within each imputation (analytic paired CI) --\n")
  print(annex_dcr_by_imp, row.names = FALSE, digits = 4)
}
if (length(adcr_arms) > 2L) {
  cat("\n-- Reference: competing-risk C of the shared cause-specific net risk 1 - S(t) --\n")
  print(annex_dcr_table[annex_dcr_table$marker_B == "readm_netrisk_shared",
                        intersect(c("marker_A", "horizon", "C_A", "C_B", "delta_C",
                                    "delta_ij_lower", "delta_ij_upper", "excludes_zero_ij"),
                                  names(annex_dcr_table))], row.names = FALSE, digits = 4)
}
cat(sprintf("\nMaximum |delta C| over the decisional comparison: %.5f (promotion threshold %.2f).\n",
            max(abs(.adcr_main$delta_C), na.rm = TRUE), ADCR_PROMOTION_DELTA))
cat(if (any(.adcr_main$meets_promotion_threshold, na.rm = TRUE))
      "At least one horizon reaches the promotion threshold: the mortality pairing materially changes readmission discrimination.\n"
    else
      "No horizon reaches the promotion threshold: the mortality pairing rescales the absolute readmission risk without materially reordering subjects, so the choice of mortality model is a calibration and net-benefit question, not a discrimination one.\n")
# ---- Save ------------------------------------------------------------------
# CSV under cons/_out (text, safe on Drive); the R object under data/, never
# under cons/, per the project rule on .rds placement.
.adcr_outdir <- file.path(.adcr_root, "cons", "_out")
dir.create(.adcr_outdir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(annex_dcr_table, file.path(.adcr_outdir, "pred23_annex_deltac_cif_readmit.csv"), row.names = FALSE)
if (!is.null(annex_dcr_by_imp))
  utils::write.csv(annex_dcr_by_imp, file.path(.adcr_outdir, "pred23_annex_deltac_cif_readmit_by_imputation.csv"), row.names = FALSE)
saveRDS(list(table = annex_dcr_table, by_imputation = annex_dcr_by_imp,
             marker_gap = if (exists("annex_dcr_marker_gap", inherits = TRUE)) annex_dcr_marker_gap else NULL,
             provenance = annex_dcr$provenance, B = ADCR_B, seed = ADCR_SEED,
             horizons = annex_dcr$horizons, created = as.character(Sys.time())),
        file.path(.adcr_root, "data", "20241015_out", "pred23_annex_deltac_cif_readmit.rds"))
cat("\nSaved: cons/_out/pred23_annex_deltac_cif_readmit.csv and data/20241015_out/pred23_annex_deltac_cif_readmit.rds\n")
