# =============================================================================
# annex_cr_cindex_readmit_p225_chunks.R  (2026-08-22)
#
# TWO SELF-CONTAINED notebook chunks for an ANNEX of
# cons/prediction225_converted_mod.ipynb (DEVELOPMENT, 5-fold CV over the 5
# imputations). They close the gap Andres flagged on 2026-08-22: in this
# notebook the readmission C index is reported ONLY on the cause-specific NET
# RISK 1 - S_readmission(t), for every model.
#
# WHAT THE ANNEX ADDS, for ALL models in `dual_fits` at once.
#
# TABLE A, per model. The readmission C decomposed over the 2 x 2 of
#   SCORING  cause specific (death treated as censoring, what the notebook
#            reports today) versus competing risk (Wolbers, death is a genuine
#            competing event and can never be followed by a readmission)
#   MARKER   net risk 1 - S_1(t) versus the cause-specific-Cox cumulative
#            incidence CIF_1(t) = integral S_all(u-) dH_1(u), S_all = S_1 * S_2,
#            which is the quantity calibration and DCA already use.
# Reporting all four cells separates the SCORING effect from the MARKER effect
# instead of confounding them in a single "corrected" number, and each model
# also gets a reconciliation column computed on its OWN finite subset so the
# result can be checked against the numbers already in the notebook.
#
# TABLE B, between models. Full pairwise paired delta C on the CIF marker under
# competing-risk scoring, INCLUDING the pairs that `deltaCIBS-cv-run`,
# `paired-delta-c-absolute-risk` and `paired-delta-c-competing-risks` all SKIP
# with "[SKIP] readmission: X and Y use the same formula". That skip is correct
# for the net risk, where such a pair is degenerate, and WRONG for the CIF,
# where the paired mortality model still moves the predicted absolute
# readmission risk. With the DeepSurv candidates loaded this is not hypothetical:
# ds_specs is built so that ds_base, ds_nph_death and ds_interact_death share
# one readmission formula and differ only in the mortality model, so every
# comparison among them is currently discarded in silence. Table B flags those
# rows with shared_readmission_formula = TRUE.
#
# CHUNK 1  `annex-cr-cindex-readmit-build`  assembles and caches the markers.
# CHUNK 2  `annex-cr-cindex-readmit-run`    computes tables A and B.
#
# Both chunks are self-contained. They resolve the project root themselves and,
# if `dual_fits` is not live, read it from
# data/20241015_out/pred22_ndp_2026_07_13__dualfits.rds, the same cache
# `deltaCIBS-cv-fit-commongrid` and `p225-annex-ibs-classical-dev` already use.
# The 5.6 GB pred22 RData is never needed: every model carries its own formula
# text in fit$config, which is all the annex uses for labelling and for the
# shared-formula detection. No notebook cell has to have been run first.
#
# Placement: paste each block as its own cell at the END of
# cons/prediction225_converted_mod.ipynb, AFTER the labelled cell `session-info`.
# Both ADD objects; neither replaces anything.
# =============================================================================

# ---------------------------------------------------------------------------
# CHUNK 1 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-cr-cindex-readmit-build
#| message: true
#| warning: true
# ============================================================================
# ANNEX COMPETING-RISK READMISSION C INDEX, STEP 1/2: build the markers.
# Produces one object, `annex_crc`, holding everything chunk 2 needs:
#   $models          model names, in the order they will be reported
#   $f_readmit_txt   readmission formula TEXT per model, from fit$config
#   $f_death_txt     mortality formula TEXT per model, from fit$config
#   $time_r, $event_r    cause-specific readmission outcome (death censored),
#                        the outcome the notebook scores on today
#   $status_cr           competing-risk cause, 0 censored / 1 readmission /
#                        2 death before readmission
#   $netrisk[[m]]        MI-pooled 1 - S_readmission(t), per model
#   $cif[[m]]            MI-pooled cause-specific CIF for readmission, built by
#                        product integration from that model's OWN readmission
#                        and mortality survival matrices, per model
#   $ok_own[[m]]         each model's own finite-row mask, for reconciliation
#   $eval_times, $horizons, $n, $n_dropped_common, $provenance
# Every model is restricted to the SAME set of subjects (the intersection of
# the per-model finite masks). That is what makes the pairwise deltas in table
# B genuinely paired; the per-model own mask is kept so table A can also report
# each model on its own subset and be checked against the existing cells.
# Kernel prerequisites: NONE. `dual_fits` is reused if live, otherwise read from
# data/20241015_out/pred22_ndp_2026_07_13__dualfits.rds. The DeepSurv arms are
# added when `ds_dual_fits` is live or a pred225_ds_common_grid__*.rds cache
# exists; set ANNEX_CRC_INCLUDE_DS <- FALSE to leave them out.
# Place AFTER `session-info`. Adds objects; replaces nothing.
# ============================================================================
if (!exists("ANNEX_CRC_FORCE_REBUILD", inherits = TRUE)) ANNEX_CRC_FORCE_REBUILD <- FALSE
if (!exists("ANNEX_CRC_USE_DISK_CACHE", inherits = TRUE)) ANNEX_CRC_USE_DISK_CACHE <- TRUE
if (!exists("ANNEX_CRC_INCLUDE_DS", inherits = TRUE)) ANNEX_CRC_INCLUDE_DS <- TRUE
if (!exists("ANNEX_CRC_MODELS", inherits = TRUE)) ANNEX_CRC_MODELS <- NULL
# The canonical common grid. IBS integrates over whatever grid it is given, so
# objects built on a different grid are not interchangeable with the frozen
# ones (the documented eval_times trap). Uno's C at a fixed horizon does not
# integrate, but the horizon must still be an exact column of the grid, which
# is what the check below enforces.
ANNEX_CRC_GRID     <- c(3, 6, 12, 36, 60)
ANNEX_CRC_HORIZONS <- c(6, 12, 36, 60)
stopifnot("ANNEX_CRC_HORIZONS must be exact columns of ANNEX_CRC_GRID" =
  all(ANNEX_CRC_HORIZONS %in% ANNEX_CRC_GRID))
# ---- Resolve AND VALIDATE the project root ---------------------------------
# Never take `project_root` on faith: running from the console commonly leaves
# the working directory at <root>/cons, which silently produces paths like
# <root>/cons/cons/_alt_scripts and <root>/cons/data/20241015_out. Accept only
# a candidate that actually contains data/20241015_out.
.acrc_root <- local({
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
    stop(sprintf("annex-cr-cindex: cannot locate the project root. Tried: %s. None contains data/20241015_out.",
                 paste(cand, collapse = " | ")), call. = FALSE)
  normalizePath(ok[1L], winslash = "/", mustWork = FALSE)
})
message("annex-cr-cindex build: project root resolved to ", .acrc_root)
if (!exists("project_root", inherits = TRUE) ||
    !identical(normalizePath(project_root, winslash = "/", mustWork = FALSE), .acrc_root)) {
  project_root <- .acrc_root
  message("annex-cr-cindex build: project_root set to the resolved root (it was absent or pointed elsewhere).")
}
.acrc_data_out <- file.path(.acrc_root, "data", "20241015_out")
ANNEX_CRC_CACHE_RDS <- file.path(.acrc_data_out, "pred225_annex_cr_cindex_readmit_inputs.rds")
stopifnot(requireNamespace("survival", quietly = TRUE))
# ---- Helpers, duplicated here so the annex needs no source() ----------------
.acrc_extract <- function(res, outcome = c("readmission", "death")) {
  # Reassemble the out-of-fold predictions into one per-subject matrix, pooled
  # over imputations. This is a VERBATIM port of .cv_extract() from the cell
  # `deltaCIBS-cv-engine`, minus the linear predictor, which the annex does not
  # use. Ported rather than sourced so the chunk runs on a cold kernel; the
  # is.null(o$lp_val) validity test is kept exactly as in the original so a
  # block the engine considers unusable is skipped identically here.
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
  list(time = time_v, event = event_v, S = Reduce(`+`, S_imp) / length(S_imp), eval_times = et)
}
.acrc_cif_product_integral <- function(S1, S2) {
  # Cause-specific cumulative incidence by discrete product integration on the
  # prediction grid, VERBATIM from the cell `paired-delta-c-competing-risks`:
  #   CIF_1(t_k) = sum_j S_all(t_{j-1}) * [H_1(t_j) - H_1(t_{j-1})]
  # with H_1 = -log S_1 and S_all = S_1 * S_2. No refitting is involved: both
  # survival matrices already exist for the same subjects, folds and imputations.
  # NOTE on the order of operations: the survival matrices handed in here are
  # ALREADY averaged over the five imputations, so this pools first and
  # integrates second. Integrating within each imputation and pooling after
  # would not give the identical number, because the product integral is not
  # linear in S. Pooling first is what `paired-delta-c-competing-risks` does,
  # and matching it matters more than the third-decimal difference, since the
  # whole point of the annex is to be comparable with the existing cells.
  H1 <- -log(pmax(S1, 1e-12))
  S_all <- S1 * S2
  n <- nrow(S1); K <- ncol(S1)
  cif <- matrix(0, n, K)
  S_prev <- rep(1, n); H1_prev <- rep(0, n); acc <- rep(0, n)
  for (k in seq_len(K)) {
    acc <- acc + S_prev * (H1[, k] - H1_prev)
    cif[, k] <- acc
    S_prev <- S_all[, k]; H1_prev <- H1[, k]
  }
  pmin(pmax(cif, 0), 1)
}
.acrc_status <- function(ex_r, ex_d) {
  # Competing-risk cause, using the convention of the cell
  # `paired-delta-c-competing-risks`: readmission follow-up is censored at
  # death, so a subject who dies without readmission carries event 0 in the
  # readmission extract and the same time in both extracts.
  died_first <- ex_r$event == 0 & ex_d$event == 1 & (ex_d$time <= ex_r$time + 1e-8)
  ifelse(ex_r$event == 1, 1L, ifelse(died_first, 2L, 0L))
}
.acrc_first_event_alt <- function(ex_r, ex_d) {
  # Independent reconstruction using the pmin first-event rule of
  # .vhm_first_event() / .holdout_cr_first_event(), ties resolved to death.
  # Computed only to CROSS-CHECK the convention above: the two rules agree
  # whenever readmission follow-up really is censored at death, and a
  # disagreement count above zero means that assumption fails somewhere and the
  # competing-risk status cannot be trusted.
  ft <- pmin(ex_r$time, ex_d$time)
  r1 <- ex_r$event == 1 & is.finite(ft) & abs(ex_r$time - ft) <= 1e-8
  d1 <- ex_d$event == 1 & is.finite(ft) & abs(ex_d$time - ft) <= 1e-8
  st <- rep(0L, length(ft))
  st[r1 & !d1] <- 1L
  st[d1 & !r1] <- 2L
  st[r1 & d1]  <- 2L
  st
}
# ---- FAST PATH: a cache written by a previous run of this same chunk --------
# Validated, not trusted: the grid, the model manifest and the row count must
# match what this run asks for, otherwise the cache is ignored and rebuilt.
annex_crc <- NULL
if (!ANNEX_CRC_FORCE_REBUILD && ANNEX_CRC_USE_DISK_CACHE && file.exists(ANNEX_CRC_CACHE_RDS)) {
  .acrc_try <- tryCatch(readRDS(ANNEX_CRC_CACHE_RDS), error = function(e) NULL)
  .acrc_ok <- !is.null(.acrc_try) && is.list(.acrc_try) &&
    all(c("models", "time_r", "event_r", "status_cr", "netrisk", "cif", "eval_times") %in% names(.acrc_try)) &&
    isTRUE(all.equal(as.numeric(.acrc_try$eval_times), ANNEX_CRC_GRID)) &&
    length(.acrc_try$netrisk) == length(.acrc_try$models) &&
    nrow(.acrc_try$netrisk[[1]]) == length(.acrc_try$time_r) &&
    (is.null(ANNEX_CRC_MODELS) || all(ANNEX_CRC_MODELS %in% .acrc_try$models))
  if (isTRUE(.acrc_ok)) {
    annex_crc <- .acrc_try
    annex_crc$provenance <- paste0(annex_crc$provenance, " [reloaded from ", basename(ANNEX_CRC_CACHE_RDS), "]")
    message("annex-cr-cindex build: reused the annex disk cache ", basename(ANNEX_CRC_CACHE_RDS),
            ". Set ANNEX_CRC_FORCE_REBUILD <- TRUE to ignore it.")
  } else {
    message("annex-cr-cindex build: annex disk cache present but it did not validate; rebuilding.")
  }
  rm(.acrc_try)
}
# ---- Build ------------------------------------------------------------------
if (is.null(annex_crc)) {
  .acrc_prov <- character(0)
  # 1. The primary Cox arms. Same cache and same field the cells
  #    `deltaCIBS-cv-fit-commongrid` and `p225-annex-ibs-classical-dev` use.
  if (exists("dual_fits", envir = globalenv(), inherits = FALSE)) {
    .acrc_fits <- get("dual_fits", envir = globalenv())
    .acrc_prov <- c(.acrc_prov, "Cox arms from the live dual_fits")
  } else {
    .acrc_cache_fits <- file.path(.acrc_data_out, "pred22_ndp_2026_07_13__dualfits.rds")
    stopifnot("pred22_ndp_2026_07_13__dualfits.rds not found under data/20241015_out" =
      file.exists(.acrc_cache_fits))
    message("annex-cr-cindex build: reading ", basename(.acrc_cache_fits), " (",
            round(file.info(.acrc_cache_fits)$size / 1024^2), " MB). This takes a moment.")
    .acrc_fits <- readRDS(.acrc_cache_fits)$dual_fits
    .acrc_prov <- c(.acrc_prov, paste0("Cox arms from ", basename(.acrc_cache_fits)))
  }
  # 2. The DeepSurv-informed arms. These are the ones that make the skipped
  #    pairs concrete: ds_specs deliberately varies ONE outcome at a time, so
  #    several of them share a readmission formula and differ only in the
  #    mortality model.
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
  if (isTRUE(ANNEX_CRC_INCLUDE_DS)) {
    .acrc_ds <- NULL
    if (exists("ds_dual_fits", envir = globalenv(), inherits = FALSE)) {
      .acrc_ds <- get("ds_dual_fits", envir = globalenv())
      .acrc_prov <- c(.acrc_prov, "DS arms from the live ds_dual_fits")
    } else {
      .acrc_ds_files <- list.files(.acrc_data_out, pattern = "^pred225_ds_common_grid__.*\\.rds$", full.names = TRUE)
      if (length(.acrc_ds_files)) {
        .acrc_ds_f <- .acrc_ds_files[order(file.info(.acrc_ds_files)$mtime, decreasing = TRUE)][1L]
        message("annex-cr-cindex build: reading ", basename(.acrc_ds_f), " (",
                round(file.info(.acrc_ds_f)$size / 1024^2), " MB).")
        .ds_stem_check(.acrc_ds_f)
        .acrc_ds <- tryCatch(readRDS(.acrc_ds_f)$ds_dual_fits, error = function(e) {
          message("annex-cr-cindex build: DS cache unreadable (", conditionMessage(e), "); continuing without it.")
          NULL
        })
        if (!is.null(.acrc_ds)) .acrc_prov <- c(.acrc_prov, paste0("DS arms from ", basename(.acrc_ds_f)))
      } else {
        message("annex-cr-cindex build: no DS common-grid cache found; continuing with the Cox arms only.")
      }
    }
    if (!is.null(.acrc_ds)) {
      .acrc_dup <- intersect(names(.acrc_fits), names(.acrc_ds))
      if (length(.acrc_dup))
        stop("annex-cr-cindex: model names collide between dual_fits and ds_dual_fits: ",
             paste(.acrc_dup, collapse = ", "), call. = FALSE)
      .acrc_fits <- c(.acrc_fits, .acrc_ds)
    }
    rm(.acrc_ds)
  }
  # 3. Keep only objects that still carry raw_predictions, the same filter the
  #    cell `deltaCIBS-cv-run` applies before any paired comparison.
  .acrc_fits <- .acrc_fits[!vapply(.acrc_fits, is.null, logical(1))]
  .acrc_hasrp <- vapply(.acrc_fits, function(f) !is.null(f$raw_predictions) && length(f$raw_predictions) > 0L, logical(1))
  if (any(!.acrc_hasrp))
    message("annex-cr-cindex build: dropped for having no raw_predictions: ",
            paste(names(.acrc_fits)[!.acrc_hasrp], collapse = ", "))
  .acrc_fits <- .acrc_fits[.acrc_hasrp]
  if (!is.null(ANNEX_CRC_MODELS)) {
    stopifnot("ANNEX_CRC_MODELS names a model that is not available" =
      all(ANNEX_CRC_MODELS %in% names(.acrc_fits)))
    .acrc_fits <- .acrc_fits[ANNEX_CRC_MODELS]
  }
  stopifnot("at least two models are needed for the pairwise table" = length(.acrc_fits) >= 2L)
  # 4. Grid guard. A model evaluated on a different grid is not comparable and
  #    its horizon columns would not line up with the others.
  .acrc_grids <- vapply(.acrc_fits, function(f) paste(as.numeric(f$config$eval_times), collapse = ","), "")
  .acrc_want <- paste(ANNEX_CRC_GRID, collapse = ",")
  if (any(.acrc_grids != .acrc_want))
    stop("annex-cr-cindex: these models are not on the canonical grid ", .acrc_want, ": ",
         paste(names(.acrc_fits)[.acrc_grids != .acrc_want], collapse = ", "), call. = FALSE)
  message("annex-cr-cindex build: models in play: ", paste(names(.acrc_fits), collapse = ", "))
  # 5. Extract, build the CIF, and record each model's own finite mask.
  .acrc_nm <- names(.acrc_fits)
  .acrc_net <- list(); .acrc_cif <- list(); .acrc_okm <- list()
  .acrc_ref <- NULL; .acrc_status_v <- NULL; .acrc_mismatch <- 0L
  for (m in .acrc_nm) {
    ex_r <- .acrc_extract(.acrc_fits[[m]], "readmission")
    ex_d <- .acrc_extract(.acrc_fits[[m]], "death")
    okm <- is.finite(ex_r$time) & is.finite(ex_r$event) &
           is.finite(ex_d$time) & is.finite(ex_d$event) &
           rowSums(is.finite(ex_r$S)) == ncol(ex_r$S) &
           rowSums(is.finite(ex_d$S)) == ncol(ex_d$S)
    .acrc_okm[[m]] <- okm
    .acrc_net[[m]] <- pmin(pmax(1 - ex_r$S, 0), 1)
    .acrc_cif[[m]] <- .acrc_cif_product_integral(ex_r$S, ex_d$S)
    if (is.null(.acrc_ref)) {
      .acrc_ref <- list(time_r = ex_r$time, event_r = ex_r$event,
                        time_d = ex_d$time, event_d = ex_d$event)
      .acrc_status_v <- .acrc_status(ex_r, ex_d)
      .acrc_mismatch <- sum(.acrc_status_v[okm] != .acrc_first_event_alt(ex_r, ex_d)[okm])
    } else {
      # Every model is fit on the same data, so the observed outcomes must be
      # identical. If they are not, the models are not comparable at all and a
      # paired delta would be meaningless.
      stopifnot("models disagree on the observed readmission outcome" =
        max(abs(ex_r$time[okm] - .acrc_ref$time_r[okm]), na.rm = TRUE) < 1e-8 &&
        all(ex_r$event[okm] == .acrc_ref$event_r[okm], na.rm = TRUE))
      stopifnot("models disagree on the observed mortality outcome" =
        max(abs(ex_d$time[okm] - .acrc_ref$time_d[okm]), na.rm = TRUE) < 1e-8 &&
        all(ex_d$event[okm] == .acrc_ref$event_d[okm], na.rm = TRUE))
    }
    message(sprintf("annex-cr-cindex build: %-22s usable rows %d", m, sum(okm)))
    rm(ex_r, ex_d); invisible(gc(verbose = FALSE))
  }
  # 6. One COMMON subject set, so table B is genuinely paired.
  .acrc_ok <- Reduce(`&`, .acrc_okm)
  .acrc_n_all <- length(.acrc_ok)
  message(sprintf("annex-cr-cindex build: common analysable rows %d of %d (dropped %d)",
                  sum(.acrc_ok), .acrc_n_all, .acrc_n_all - sum(.acrc_ok)))
  stopifnot("fewer than 100 rows survive the common mask; something is wrong upstream" = sum(.acrc_ok) >= 100L)
  annex_crc <- list(
    models = .acrc_nm,
    f_readmit_txt = vapply(.acrc_fits, function(f) paste(as.character(f$config$formula_readmit), collapse = " "), ""),
    f_death_txt   = vapply(.acrc_fits, function(f) paste(as.character(f$config$formula_death), collapse = " "), ""),
    time_r = .acrc_ref$time_r[.acrc_ok], event_r = .acrc_ref$event_r[.acrc_ok],
    status_cr = .acrc_status_v[.acrc_ok],
    netrisk = lapply(.acrc_net, function(M) M[.acrc_ok, , drop = FALSE]),
    cif     = lapply(.acrc_cif, function(M) M[.acrc_ok, , drop = FALSE]),
    netrisk_own = .acrc_net, ok_own = .acrc_okm,
    time_r_all = .acrc_ref$time_r, event_r_all = .acrc_ref$event_r,
    eval_times = ANNEX_CRC_GRID, horizons = ANNEX_CRC_HORIZONS,
    n = sum(.acrc_ok), n_all = .acrc_n_all, n_dropped_common = .acrc_n_all - sum(.acrc_ok),
    status_rule_mismatch = .acrc_mismatch,
    provenance = paste(.acrc_prov, collapse = "; "),
    created = as.character(Sys.time())
  )
  rm(.acrc_fits, .acrc_net, .acrc_cif, .acrc_okm); invisible(gc(verbose = FALSE))
  if (ANNEX_CRC_USE_DISK_CACHE) {
    saveRDS(annex_crc, ANNEX_CRC_CACHE_RDS)
    message("annex-cr-cindex build: cached to ", ANNEX_CRC_CACHE_RDS, " (",
            round(file.info(ANNEX_CRC_CACHE_RDS)$size / 1024^2), " MB)")
  }
}
# ---- Diagnostics printed whichever tier supplied the inputs -----------------
cat(sprintf("\n== Annex competing-risk readmission C index: inputs ==\nprovenance: %s\n", annex_crc$provenance))
cat(sprintf("models (%d): %s\n", length(annex_crc$models), paste(annex_crc$models, collapse = ", ")))
cat(sprintf("common rows = %d of %d (dropped %d) | grid = %s\n",
            annex_crc$n, annex_crc$n_all, annex_crc$n_dropped_common,
            paste(annex_crc$eval_times, collapse = ", ")))
cat(sprintf("readmissions = %d (%.2f%%) | deaths before readmission = %d (%.2f%%) | censored = %d (%.2f%%)\n",
            sum(annex_crc$status_cr == 1L), 100 * mean(annex_crc$status_cr == 1L),
            sum(annex_crc$status_cr == 2L), 100 * mean(annex_crc$status_cr == 2L),
            sum(annex_crc$status_cr == 0L), 100 * mean(annex_crc$status_cr == 0L)))
cat(sprintf("competing-risk status rule cross-check, disagreements between the two rules: %d\n",
            annex_crc$status_rule_mismatch))
if (annex_crc$status_rule_mismatch > 0L)
  warning("annex-cr-cindex: the two first-event rules disagree on ", annex_crc$status_rule_mismatch,
          " rows. Readmission follow-up may not be censored at death everywhere; inspect before reporting.",
          call. = FALSE)
# Which models share a readmission formula. These are exactly the pairs the
# notebook's other paired-delta cells discard, and the reason this annex exists.
annex_crc_shared_groups <- split(annex_crc$models, gsub("[[:space:]]+", "", annex_crc$f_readmit_txt))
annex_crc_shared_groups <- annex_crc_shared_groups[lengths(annex_crc_shared_groups) > 1L]
if (length(annex_crc_shared_groups)) {
  cat("\nModel groups sharing ONE readmission formula (their pairwise delta C is skipped by\n")
  cat("`deltaCIBS-cv-run`, `paired-delta-c-absolute-risk` and `paired-delta-c-competing-risks`,\n")
  cat("and is NOT degenerate on the CIF marker):\n")
  for (g in seq_along(annex_crc_shared_groups))
    cat(sprintf("  group %d: %s\n", g, paste(annex_crc_shared_groups[[g]], collapse = ", ")))
} else {
  cat("\nNo two models share a readmission formula, so nothing is being skipped by the other cells.\n")
}
# How far apart the net risk and the CIF are, per model, before any C index.
annex_crc_marker_gap <- do.call(rbind, lapply(annex_crc$models, function(m) {
  do.call(rbind, lapply(annex_crc$horizons, function(h) {
    j <- which(abs(annex_crc$eval_times - h) < 1e-8)
    a <- annex_crc$netrisk[[m]][, j]; b <- annex_crc$cif[[m]][, j]
    data.frame(model = m, horizon = h, mean_netrisk = mean(a), mean_cif = mean(b),
               mean_diff = mean(a - b), max_abs_diff = max(abs(a - b)),
               spearman_rho = stats::cor(a, b, method = "spearman"), stringsAsFactors = FALSE)
  }))
}))
cat("\nNet risk versus CIF, per model (before any C index):\n")
print(annex_crc_marker_gap, row.names = FALSE, digits = 4)
cat("\nInputs ready in `annex_crc`. Run the cell `annex-cr-cindex-readmit-run` next.\n")

# ---------------------------------------------------------------------------
# CHUNK 2 -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-cr-cindex-readmit-run
#| message: true
#| warning: true
# ============================================================================
# ANNEX COMPETING-RISK READMISSION C INDEX, STEP 2/2: tables A and B.
# EVERY INTERVAL HERE IS A NONPARAMETRIC PAIRED BOOTSTRAP. The analytic
# infinitesimal-jackknife interval is still computed and kept as a secondary
# column, because it costs one extra call and a visible disagreement between the
# two routes is itself diagnostic.
#
# TABLE A, `annex_crc_bymodel`. For every model and horizon, Uno's C for
# readmission in the 2 x 2 of scoring and marker:
#   C_cs_netrisk  cause-specific scoring, net risk marker. THIS is what the
#                 notebook reports today, and the column C_cs_netrisk_ownmask
#                 recomputes it on the model's OWN finite subset so it can be
#                 reconciled with `paired-delta-c-absolute-risk`.
#   C_cs_cif      cause-specific scoring, CIF marker
#   C_cr_netrisk  competing-risk (Wolbers) scoring, net risk marker
#   C_cr_cif      competing-risk scoring, CIF marker. The coherent quantity,
#                 aligned with calibration (calibrate_cscox_aj_grouped_corr) and
#                 with the DCA engine, which are Aalen-Johansen based.
# delta_marker_cs and delta_marker_cr are bootstrapped: within one replicate the
# same resampled subjects are scored on both markers, so the difference is paired
# by construction. delta_scoring_netrisk stays a POINT ESTIMATE with no interval:
# the two scorings define different risk sets and different censoring
# distributions, so no single resample pairs them and any interval would be wrong.
#
# TABLE B, `annex_crc_pairs`. Paired delta C between models on the CIF marker
# under competing-risk scoring, from the SAME replicates, with the column
# shared_readmission_formula marking the pairs the notebook's other cells skip.
#
# HOW THE BOOTSTRAP IS MADE AFFORDABLE, because the obvious design is not.
# Bootstrapping one PAIR at a time means one survival::concordance() call per pair
# per replicate; at n around 70,500 with ten models that is roughly eighteen hours
# for B = 500. Two changes remove it.
# 1. SCORE EVERY MARKER ONCE PER REPLICATE, NOT EVERY PAIR. Within one scoring
#    and horizon all markers share the same follow-up, the same event indicator
#    and the same resampled subjects, so one pass over the 2K markers yields every
#    C, and each difference in tables A and B is then a subtraction of two numbers
#    that came from the same resample.
# 2. USE THE LOW-LEVEL COUNTER AND SKIP THE PER-REPLICATE VARIANCE.
#    survival::concordance() computes an influence-function variance on every
#    call, which a bootstrap does not use because the bootstrap IS the variance
#    estimate. survival:::concordancefit(..., std.err = FALSE) returns the counts,
#    and C = (concordant + tied.x / 2) / (concordant + discordant + tied.x)
#    reproduces concordance()$concordance exactly. That function is INTERNAL to
#    survival, so it is never trusted blindly: the gate below recomputes every
#    marker, scoring and horizon on the OBSERVED data through both routes and
#    refuses the fast path unless they agree to 1e-10.
#
# WOLBERS CONVENTION. A subject who dies before readmission can never be
# readmitted; censoring them, the cause-specific convention, silently drops those
# pairs. Instead their follow-up is pushed far beyond the cohort and they are kept
# event free, so they stay comparable with every readmission case.
# KNOWN CAVEAT, stated so a reviewer does not have to find it: pushing them out
# keeps them in the at-risk set of the censoring Kaplan-Meier that supplies the
# n/G2 weights, so G is slightly optimistic. It is identical for every model and
# every marker here, so it cancels in all the paired differences; it can shift the
# absolute level of C a little, not the deltas.
#
# Requires in kernel: `annex_crc` and `.acrc_root` from the cell
# `annex-cr-cindex-readmit-build`. Place AFTER it. Adds objects; replaces nothing.
# ============================================================================
stopifnot("run the cell `annex-cr-cindex-readmit-build` first" = exists("annex_crc", inherits = TRUE))
stopifnot("run the cell `annex-cr-cindex-readmit-build` first" = exists(".acrc_root", inherits = TRUE))
stopifnot(requireNamespace("survival", quietly = TRUE))
if (!exists("ANNEX_CRC_B", inherits = TRUE)) ANNEX_CRC_B <- 500L
if (!exists("ANNEX_CRC_SEED", inherits = TRUE)) ANNEX_CRC_SEED <- 2125L
# NULL means: use whatever future::plan() is already active, otherwise sequential.
# An integer starts that many PSOCK workers here and stops them before the chunk
# ends, without project startup files, the construction
# `p225-annex-ibs-classical-dev` settled on because the renv activation script in
# this project's .Rprofile makes plain multisession workers slow and flaky.
if (!exists("ANNEX_CRC_WORKERS", inherits = TRUE)) ANNEX_CRC_WORKERS <- NULL
# Set to FALSE to force the reference route, survival::concordance() on every
# replicate. Same numbers, roughly three times slower.
if (!exists("ANNEX_CRC_FAST_COUNTER", inherits = TRUE)) ANNEX_CRC_FAST_COUNTER <- TRUE
# The project's promotion rule: about 0.01 or more in Uno's C is a meaningful
# gain. Reported as its own column so statistical separation, which is easy at
# n around 70,500, is never silently read as decisional relevance.
if (!exists("ANNEX_CRC_PROMOTION_DELTA", inherits = TRUE)) ANNEX_CRC_PROMOTION_DELTA <- 0.01
.acrc_crit <- stats::qnorm(0.975)
# ---- Outcome recodings, done once ------------------------------------------
.acrc_tr <- as.numeric(annex_crc$time_r)
.acrc_er <- as.integer(annex_crc$event_r)
.acrc_st <- as.integer(annex_crc$status_cr)
.acrc_big <- max(.acrc_tr, na.rm = TRUE) * 10
.acrc_tw <- ifelse(.acrc_st == 2L, .acrc_big, .acrc_tr)
.acrc_ew <- as.integer(.acrc_st == 1L)
.acrc_colj <- function(h) {
  j <- which(abs(as.numeric(annex_crc$eval_times) - h) < 1e-8)
  if (length(j) != 1L)
    stop(sprintf("horizon %s is not exactly one column of the grid %s", h,
                 paste(annex_crc$eval_times, collapse = ", ")), call. = FALSE)
  j
}
# ---- Marker layout: 2K columns per horizon, net risk then CIF ---------------
# One matrix per horizon holds every model under both markers, so a replicate
# scores tables A and B in a single pass and every difference in either table is
# paired on the same resampled subjects.
.acrc_arms <- c(paste0("net::", annex_crc$models), paste0("cif::", annex_crc$models))
.acrc_M <- lapply(annex_crc$horizons, function(h) {
  j <- .acrc_colj(h)
  M <- cbind(vapply(annex_crc$models, function(m) annex_crc$netrisk[[m]][, j], numeric(length(.acrc_tr))),
             vapply(annex_crc$models, function(m) annex_crc$cif[[m]][, j], numeric(length(.acrc_tr))))
  colnames(M) <- .acrc_arms
  M
})
names(.acrc_M) <- as.character(annex_crc$horizons)
.acrc_n <- length(.acrc_tr)
# ---- Scoring routes, and the gate that decides which one the bootstrap uses --
.acrc_c_ref <- function(tt, ee, marker, horizon)
  unname(survival::concordance(survival::Surv(tt, ee) ~ marker, timewt = "n/G2",
                               reverse = TRUE, ymax = horizon)$concordance)
.acrc_cfit <- if (isTRUE(ANNEX_CRC_FAST_COUNTER) &&
                  exists("concordancefit", where = asNamespace("survival")))
  get("concordancefit", envir = asNamespace("survival")) else NULL
.acrc_c_fast <- function(y, marker, horizon) {
  r <- .acrc_cfit(y, marker, timewt = "n/G2", reverse = TRUE, ymax = horizon, std.err = FALSE)
  cn <- r$count
  unname((cn[["concordant"]] + cn[["tied.x"]] / 2) /
         (cn[["concordant"]] + cn[["discordant"]] + cn[["tied.x"]]))
}
.acrc_scorings <- list(cs = list(time = .acrc_tr, event = .acrc_er),
                       cr = list(time = .acrc_tw, event = .acrc_ew))
.acrc_use_fast <- FALSE
if (!is.null(.acrc_cfit)) {
  gap <- 0
  for (sc in names(.acrc_scorings)) {
    ss <- .acrc_scorings[[sc]]; y <- survival::Surv(ss$time, ss$event)
    for (hh in names(.acrc_M)) for (k in seq_along(.acrc_arms)) {
      a <- tryCatch(.acrc_c_fast(y, .acrc_M[[hh]][, k], as.numeric(hh)), error = function(e) NA_real_)
      b <- .acrc_c_ref(ss$time, ss$event, .acrc_M[[hh]][, k], as.numeric(hh))
      gap <- max(gap, abs(a - b), na.rm = TRUE)
    }
  }
  .acrc_use_fast <- is.finite(gap) && gap < 1e-10
  cat(sprintf("annex-cr-cindex run: fast-counter equivalence gate over %d marker-scoring-horizon cells, max |concordancefit - concordance| = %.3e -> %s\n",
              length(.acrc_arms) * 2L * length(annex_crc$horizons), gap,
              if (.acrc_use_fast) "fast route accepted" else "REFUSED, falling back to concordance()"))
} else {
  cat("annex-cr-cindex run: the fast counter is unavailable or disabled; using survival::concordance() throughout.\n")
}
.acrc_score_all <- function(tt, ee, M, horizon) {
  if (.acrc_use_fast) {
    y <- survival::Surv(tt, ee)
    vapply(seq_len(ncol(M)), function(k) .acrc_c_fast(y, M[, k], horizon), numeric(1))
  } else {
    vapply(seq_len(ncol(M)), function(k) .acrc_c_ref(tt, ee, M[, k], horizon), numeric(1))
  }
}
# ---- Point estimates and the analytic covariance, one call per cell ---------
# All 2K markers on one right-hand side, so a single call returns every C and the
# full covariance, from which every paired se(delta) = sqrt(v_aa + v_bb - 2 v_ab)
# follows without another fit.
cat(sprintf("annex-cr-cindex run: point estimates for %d markers x 2 scorings x %d horizons...\n",
            length(.acrc_arms), length(annex_crc$horizons))); flush.console()
.acrc_point <- list(); .acrc_V <- list()
for (sc in names(.acrc_scorings)) {
  ss <- .acrc_scorings[[sc]]; .acrc_point[[sc]] <- list(); .acrc_V[[sc]] <- list()
  for (hh in names(.acrc_M)) {
    M <- .acrc_M[[hh]]
    dfr <- data.frame(time = ss$time, event = ss$event, M, check.names = FALSE)
    fml <- stats::as.formula(paste("survival::Surv(time, event) ~",
                                   paste(sprintf("`%s`", .acrc_arms), collapse = " + ")))
    fit <- survival::concordance(fml, data = dfr, timewt = "n/G2", reverse = TRUE, ymax = as.numeric(hh))
    est <- unname(fit$concordance); V <- as.matrix(fit$var)
    nmv <- names(fit$concordance)
    if (!is.null(nmv)) {
      ord <- match(.acrc_arms, gsub("^`|`$", "", nmv))
      stopifnot("concordance returned markers in an unexpected order" = !anyNA(ord))
      est <- est[ord]; V <- V[ord, ord, drop = FALSE]
    }
    names(est) <- .acrc_arms; dimnames(V) <- list(.acrc_arms, .acrc_arms)
    .acrc_point[[sc]][[hh]] <- est; .acrc_V[[sc]][[hh]] <- V
  }
}
# ---- The bootstrap ----------------------------------------------------------
.acrc_jobs <- list()
for (sc in names(.acrc_scorings)) for (hh in names(.acrc_M))
  .acrc_jobs[[length(.acrc_jobs) + 1L]] <- list(scoring = sc, horizon = hh)
.acrc_pilot <- local({
  M <- .acrc_M[[1]]; h <- as.numeric(names(.acrc_M)[1]); ss <- .acrc_scorings[["cr"]]
  t0 <- Sys.time()
  for (i in 1:3) { s <- sample.int(.acrc_n, .acrc_n, replace = TRUE)
    .acrc_score_all(ss$time[s], ss$event[s], M[s, , drop = FALSE], h) }
  as.numeric(difftime(Sys.time(), t0, units = "secs")) / 3
})
cat(sprintf("annex-cr-cindex run: measured %.3f s per replicate for %d markers; %d jobs x B = %d -> about %.1f min sequential.\n",
            .acrc_pilot, length(.acrc_arms), length(.acrc_jobs), ANNEX_CRC_B,
            .acrc_pilot * ANNEX_CRC_B * length(.acrc_jobs) / 60)); flush.console()
.acrc_cluster <- NULL
if (!is.null(ANNEX_CRC_WORKERS) && as.integer(ANNEX_CRC_WORKERS) > 1L &&
    requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)) {
  nw <- as.integer(ANNEX_CRC_WORKERS)
  options(future.globals.maxSize = 4 * 1024^3)
  if (requireNamespace("parallelly", quietly = TRUE)) {
    cat(sprintf("annex-cr-cindex run: starting %d PSOCK workers without project startup files.\n", nw)); flush.console()
    .acrc_cluster <- parallelly::makeClusterPSOCK(nw, rscript_args = "--no-init-file",
                                                  rscript_libs = .libPaths(), connectTimeout = 300,
                                                  validate = TRUE, autoStop = FALSE)
    future::plan(future::cluster, workers = .acrc_cluster)
  } else {
    future::plan(future::multisession, workers = nw)
  }
  cat(sprintf("annex-cr-cindex run: with %d workers expect about %.1f min.\n",
              nw, .acrc_pilot * ANNEX_CRC_B * length(.acrc_jobs) / 60 / nw))
}
.acrc_one_job <- function(i) {
  job <- .acrc_jobs[[i]]
  ss <- .acrc_scorings[[job$scoring]]; M <- .acrc_M[[job$horizon]]; h <- as.numeric(job$horizon)
  set.seed(ANNEX_CRC_SEED + i, kind = "Mersenne-Twister")
  out <- matrix(NA_real_, ncol(M), ANNEX_CRC_B, dimnames = list(colnames(M), NULL))
  for (b in seq_len(ANNEX_CRC_B)) {
    s <- sample.int(.acrc_n, .acrc_n, replace = TRUE)
    out[, b] <- .acrc_score_all(ss$time[s], ss$event[s], M[s, , drop = FALSE], h)
  }
  out
}
cat("annex-cr-cindex run: bootstrapping...\n"); flush.console()
.acrc_t0 <- Sys.time()
.acrc_par <- requireNamespace("future", quietly = TRUE) &&
  requireNamespace("future.apply", quietly = TRUE) && future::nbrOfWorkers() > 1L
.acrc_res <- if (.acrc_par) {
  future.apply::future_lapply(seq_along(.acrc_jobs), .acrc_one_job,
                              future.seed = TRUE, future.packages = "survival")
} else {
  lapply(seq_along(.acrc_jobs), function(i) {
    r <- .acrc_one_job(i)
    cat(sprintf("  job %d/%d (%s scoring @ %s m) done, %.1f min elapsed\n", i, length(.acrc_jobs),
                .acrc_jobs[[i]]$scoring, .acrc_jobs[[i]]$horizon,
                as.numeric(difftime(Sys.time(), .acrc_t0, units = "mins")))); flush.console()
    r
  })
}
if (!is.null(.acrc_cluster)) {
  try(future::plan(future::sequential), silent = TRUE)
  try(parallel::stopCluster(.acrc_cluster), silent = TRUE); .acrc_cluster <- NULL
}
cat(sprintf("annex-cr-cindex run: bootstrap finished in %.1f min.\n",
            as.numeric(difftime(Sys.time(), .acrc_t0, units = "mins"))))
annex_crc_boot <- list()
for (i in seq_along(.acrc_jobs)) {
  j <- .acrc_jobs[[i]]
  if (is.null(annex_crc_boot[[j$scoring]])) annex_crc_boot[[j$scoring]] <- list()
  annex_crc_boot[[j$scoring]][[j$horizon]] <- .acrc_res[[i]]
}
.acrc_q <- function(v, p) { f <- v[is.finite(v)]
  if (length(f) < 20L) NA_real_ else stats::quantile(f, probs = p, names = FALSE) }
.acrc_se_ij <- function(V, a, b) sqrt(max(V[a, a] + V[b, b] - 2 * V[a, b], 0))
# ---- TABLE A: per model, scoring x marker ----------------------------------
annex_crc_bymodel <- do.call(rbind, lapply(annex_crc$models, function(m) {
  do.call(rbind, lapply(annex_crc$horizons, function(h) {
    hh <- as.character(h); j <- .acrc_colj(h)
    an <- paste0("net::", m); ac <- paste0("cif::", m)
    pcs <- .acrc_point$cs[[hh]]; pcr <- .acrc_point$cr[[hh]]
    bcs <- annex_crc_boot$cs[[hh]]; bcr <- annex_crc_boot$cr[[hh]]
    d_cs <- bcs[ac, ] - bcs[an, ]; d_cr <- bcr[ac, ] - bcr[an, ]
    # Reconciliation with the existing cells: same estimator, but on the model's
    # OWN finite subset instead of the common one, which is how
    # `paired-delta-c-absolute-risk` computes it. Point estimate only.
    okm <- annex_crc$ok_own[[m]]
    own <- tryCatch(.acrc_c_ref(annex_crc$time_r_all[okm], annex_crc$event_r_all[okm],
                                annex_crc$netrisk_own[[m]][okm, j], h), error = function(e) NA_real_)
    data.frame(
      model = m, horizon = h, n_common = .acrc_n, n_own = sum(okm),
      readmissions_by_horizon = sum(.acrc_er == 1L & .acrc_tr <= h),
      competing_deaths_by_horizon = sum(.acrc_st == 2L & .acrc_tr <= h),
      C_cs_netrisk = unname(pcs[an]),
      C_cs_netrisk_lower = .acrc_q(bcs[an, ], 0.025), C_cs_netrisk_upper = .acrc_q(bcs[an, ], 0.975),
      C_cs_cif = unname(pcs[ac]),
      delta_marker_cs = unname(pcs[ac] - pcs[an]),
      delta_marker_cs_lower = .acrc_q(d_cs, 0.025), delta_marker_cs_upper = .acrc_q(d_cs, 0.975),
      delta_marker_cs_se_ij = .acrc_se_ij(.acrc_V$cs[[hh]], ac, an),
      C_cr_netrisk = unname(pcr[an]),
      C_cr_cif = unname(pcr[ac]),
      C_cr_cif_lower = .acrc_q(bcr[ac, ], 0.025), C_cr_cif_upper = .acrc_q(bcr[ac, ], 0.975),
      delta_marker_cr = unname(pcr[ac] - pcr[an]),
      delta_marker_cr_lower = .acrc_q(d_cr, 0.025), delta_marker_cr_upper = .acrc_q(d_cr, 0.975),
      delta_marker_cr_se_ij = .acrc_se_ij(.acrc_V$cr[[hh]], ac, an),
      delta_scoring_netrisk_point_only = unname(pcr[an] - pcs[an]),
      C_cs_netrisk_ownmask = own,
      spearman_netrisk_cif = stats::cor(annex_crc$netrisk[[m]][, j], annex_crc$cif[[m]][, j], method = "spearman"),
      B_valid = sum(is.finite(d_cr)), B_requested = ANNEX_CRC_B,
      stringsAsFactors = FALSE, row.names = NULL)
  }))
}))
rownames(annex_crc_bymodel) <- NULL
# ---- TABLE B: pairwise paired delta C on the CIF marker --------------------
.acrc_key <- function(x) gsub("[[:space:]]+", "", x)
.acrc_prs <- utils::combn(annex_crc$models, 2L, simplify = FALSE)
annex_crc_pairs <- do.call(rbind, lapply(.acrc_prs, function(pr) {
  shared <- identical(.acrc_key(annex_crc$f_readmit_txt[[pr[1]]]), .acrc_key(annex_crc$f_readmit_txt[[pr[2]]]))
  shared_d <- identical(.acrc_key(annex_crc$f_death_txt[[pr[1]]]), .acrc_key(annex_crc$f_death_txt[[pr[2]]]))
  do.call(rbind, lapply(annex_crc$horizons, function(h) {
    hh <- as.character(h); j <- .acrc_colj(h)
    aA <- paste0("cif::", pr[1]); aB <- paste0("cif::", pr[2])
    p <- .acrc_point$cr[[hh]]; B <- annex_crc_boot$cr[[hh]]
    d <- B[aA, ] - B[aB, ]; delta <- unname(p[aA] - p[aB])
    lo <- .acrc_q(d, 0.025); hi <- .acrc_q(d, 0.975)
    se <- .acrc_se_ij(.acrc_V$cr[[hh]], aA, aB)
    data.frame(
      model_A = pr[1], model_B = pr[2], horizon = h, n = .acrc_n,
      shared_readmission_formula = shared, shared_death_formula = shared_d,
      readmissions_by_horizon = sum(.acrc_er == 1L & .acrc_tr <= h),
      competing_deaths_by_horizon = sum(.acrc_st == 2L & .acrc_tr <= h),
      C_A = unname(p[aA]), C_B = unname(p[aB]), delta_C = delta,
      delta_boot_lower = lo, delta_boot_upper = hi,
      boot_mean = mean(d[is.finite(d)]), centering_bias = mean(d[is.finite(d)]) - delta,
      point_inside_boot_ci = is.finite(lo) && is.finite(hi) && delta >= lo && delta <= hi,
      se_delta_ij = se, delta_ij_lower = delta - .acrc_crit * se, delta_ij_upper = delta + .acrc_crit * se,
      boot_over_ij_width = if (se > 0 && is.finite(lo) && is.finite(hi)) (hi - lo) / (2 * .acrc_crit * se) else NA_real_,
      spearman_rho = stats::cor(annex_crc$cif[[pr[1]]][, j], annex_crc$cif[[pr[2]]][, j], method = "spearman"),
      B_valid = sum(is.finite(d)), B_requested = ANNEX_CRC_B,
      stringsAsFactors = FALSE, row.names = NULL)
  }))
}))
annex_crc_pairs$meets_promotion_threshold <-
  is.finite(annex_crc_pairs$delta_C) & abs(annex_crc_pairs$delta_C) >= ANNEX_CRC_PROMOTION_DELTA
annex_crc_pairs <- annex_crc_pairs[order(annex_crc_pairs$horizon, -abs(annex_crc_pairs$delta_C)), ]
rownames(annex_crc_pairs) <- NULL
# ---- Bootstrap diagnostics --------------------------------------------------
cat(sprintf("\nBootstrap diagnostics: max |centering bias| = %.5f; point inside its own interval in %d of %d pairwise rows.\n",
            max(abs(annex_crc_pairs$centering_bias), na.rm = TRUE),
            sum(annex_crc_pairs$point_inside_boot_ci, na.rm = TRUE), nrow(annex_crc_pairs)))
cat(sprintf("Bootstrap interval width relative to the analytic one: median %.3f, range %.3f to %.3f.\n",
            stats::median(annex_crc_pairs$boot_over_ij_width, na.rm = TRUE),
            min(annex_crc_pairs$boot_over_ij_width, na.rm = TRUE),
            max(annex_crc_pairs$boot_over_ij_width, na.rm = TRUE)))
if (any(!annex_crc_pairs$point_inside_boot_ci, na.rm = TRUE))
  warning("annex-cr-cindex: ", sum(!annex_crc_pairs$point_inside_boot_ci, na.rm = TRUE),
          " point estimates fall outside their own bootstrap interval. Inspect before reporting.", call. = FALSE)
# ---- Report ----------------------------------------------------------------
cat("\n-- TABLE A: what changes when the readmission C stops being net-risk only --\n")
print(annex_crc_bymodel[, c("model", "horizon", "C_cs_netrisk", "C_cs_netrisk_ownmask",
                            "C_cs_cif", "C_cr_netrisk", "C_cr_cif",
                            "delta_marker_cr", "delta_marker_cr_lower", "delta_marker_cr_upper",
                            "delta_scoring_netrisk_point_only", "spearman_netrisk_cif")],
      row.names = FALSE, digits = 4)
cat("\nColumn guide. C_cs_netrisk is the quantity the notebook reports today, on the common\n")
cat("subject set; C_cs_netrisk_ownmask is the same estimator on the model's own finite subset\n")
cat("and is the number to reconcile against `paired-delta-c-absolute-risk`. delta_marker_cr is\n")
cat("C_cr_cif minus C_cr_netrisk with a paired bootstrap interval. delta_scoring_netrisk is a\n")
cat("point estimate only, because the two scorings do not share a risk set and cannot be paired.\n")
.acrc_maxmark <- max(abs(annex_crc_bymodel$delta_marker_cr), na.rm = TRUE)
cat(sprintf("\nLargest |C_cr_cif - C_cr_netrisk| across models and horizons: %.5f (promotion threshold %.2f).\n",
            .acrc_maxmark, ANNEX_CRC_PROMOTION_DELTA))
cat(if (.acrc_maxmark >= ANNEX_CRC_PROMOTION_DELTA)
      "Reporting the net risk instead of the CIF materially changes readmission discrimination for at least one model.\n"
    else
      "Reporting the net risk instead of the CIF does not materially change readmission discrimination: the competing event rescales absolute risk without reordering subjects, so it matters for calibration and net benefit, not for the C index. Stating this explicitly is the defensible way to keep a net-risk C in the manuscript.\n")
if (any(annex_crc_pairs$shared_readmission_formula)) {
  cat("\n-- TABLE B, the rows every other cell discards: pairs sharing a readmission formula --\n")
  print(annex_crc_pairs[annex_crc_pairs$shared_readmission_formula,
                        c("model_A", "model_B", "horizon", "C_A", "C_B", "delta_C",
                          "delta_boot_lower", "delta_boot_upper", "spearman_rho")],
        row.names = FALSE, digits = 4)
}
cat("\n-- TABLE B, full pairwise sweep on the CIF marker --\n")
print(annex_crc_pairs[, c("model_A", "model_B", "horizon", "shared_readmission_formula",
                          "C_A", "C_B", "delta_C", "delta_boot_lower", "delta_boot_upper")],
      row.names = FALSE, digits = 4)
# ---- Save ------------------------------------------------------------------
# CSV under cons/_out (text, safe on Drive); the R objects under data/, never
# under cons/, per the project rule on .rds placement.
.acrc_outdir <- file.path(.acrc_root, "cons", "_out")
dir.create(.acrc_outdir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(annex_crc_bymodel, file.path(.acrc_outdir, "pred225_annex_cr_cindex_readmit_bymodel.csv"), row.names = FALSE)
utils::write.csv(annex_crc_pairs, file.path(.acrc_outdir, "pred225_annex_cr_cindex_readmit_pairs.csv"), row.names = FALSE)
saveRDS(list(bymodel = annex_crc_bymodel, pairs = annex_crc_pairs,
             bootstrap_replicates = annex_crc_boot,
             marker_gap = if (exists("annex_crc_marker_gap", inherits = TRUE)) annex_crc_marker_gap else NULL,
             shared_groups = if (exists("annex_crc_shared_groups", inherits = TRUE)) annex_crc_shared_groups else NULL,
             provenance = annex_crc$provenance, fast_counter_used = .acrc_use_fast,
             B = ANNEX_CRC_B, seed = ANNEX_CRC_SEED, horizons = annex_crc$horizons, n = annex_crc$n,
             interval = "percentile bootstrap, primary; infinitesimal jackknife, secondary",
             created = as.character(Sys.time())),
        file.path(.acrc_root, "data", "20241015_out", "pred225_annex_cr_cindex_readmit.rds"))
cat("\nSaved: cons/_out/pred225_annex_cr_cindex_readmit_bymodel.csv, ...pairs.csv and\n")
cat("data/20241015_out/pred225_annex_cr_cindex_readmit.rds\n")
