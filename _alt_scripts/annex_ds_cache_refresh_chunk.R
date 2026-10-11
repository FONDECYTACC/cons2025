# =============================================================================
# annex_ds_cache_refresh_chunk.R  (2026-08-23)
#
# ONE chunk for cons/prediction225_converted_mod.ipynb that rebuilds the
# DeepSurv-informed common-grid cache from the NEWEST prediction24 sidecars,
# refitting only the arms whose formula actually changed.
#
# WHY IT IS NEEDED. The annex chunks
#   annex-cr-cindex-readmit-build / -run
#   annex-paired-delta-c-risk-ext-build / -run / -report
# do NOT build the DeepSurv-informed arms. They READ ds_dual_fits, from the
# session or from data/20241015_out/pred225_ds_common_grid__<stem>__v1.rds.
# There is therefore no switch inside them that can select a different sidecar
# stem: the fitted object for that stem has to exist first. Only
# `deltaCIBS-ds-common-grid` creates it.
#
# WHY NOT SIMPLY RE-RUN `deltaCIBS-ds-common-grid`. You can, and it is the
# sanctioned path: it selects the newest complete triplet by modification time,
# sees no cache under that name and refits all five arms, which is five calls to
# evaluate_dual_cox_python_style, that is 5 arms x 2 outcomes x 5 imputations x
# 5 folds = 250 Cox fits, run sequentially by design. This chunk does the same
# job for less, because comparing the 2026-07-26 and 2026-08-11 sidecars shows
# the `base` and `nph` formula sets are IDENTICAL and only `interact` changed
# (readmission gained 8 interactions; mortality gained 5 main effects and 12
# interactions). Under the ds_specs design, which varies one outcome at a time,
# that leaves ds_base, ds_nph_readmit and ds_nph_death unchanged and only
# ds_interact_readmit and ds_interact_death needing a refit, so 100 Cox fits
# instead of 250.
#
# Carrying a fit over is not assumed to be safe, it is CHECKED: an arm is reused
# only when its stored formula keys equal the new specification's keys exactly,
# and every arm in the new bundle is then validated the way
# `.ds_validate_cached_fit` validates it, including that all arms share one fold
# assignment. The old cache is never modified, and the new one is written
# atomically under the new stem, so `deltaCIBS-ds-common-grid` will accept it.
#
# AFTER THIS CHUNK. Re-run the annex build and run cells with
# ANNEX_PDC_FORCE_REBUILD <- TRUE and ANNEX_CRC_FORCE_REBUILD <- TRUE so they
# ignore their own input caches, which were derived from the old arms.
#
# Placement: paste as its own cell BEFORE the annex chunks. Requires
# py_corrected_datasets_boot, or it loads the canonical RData itself.
# =============================================================================

# ---------------------------------------------------------------------------
# CHUNK -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-ds-cache-refresh
#| message: true
#| warning: true
# ============================================================================
# REBUILD the DeepSurv-informed common-grid cache from the newest prediction24
# sidecars, refitting only the arms whose formula changed.
# Produces ds_dual_fits, ds_specs and DS_CACHE_NEW in the session, and writes
# data/20241015_out/pred225_ds_common_grid__<newest stem>__v1.rds.
# Kernel prerequisites: py_corrected_datasets_boot if you want to avoid a 5.6 GB
# load. Everything else is resolved here.
# Place BEFORE the annex build cells. Never modifies the existing cache.
# ============================================================================
suppressPackageStartupMessages(library(survival))
.dsr_t0 <- proc.time()
if (!exists("DSR_FORCE_FULL_REFIT", inherits = TRUE)) DSR_FORCE_FULL_REFIT <- FALSE
DSR_GRID  <- c(3, 6, 12, 36, 60)
DSR_FOLDS <- 5L
DSR_SEED  <- 2125L
# ---- 1. Root and engine ------------------------------------------------------
.dsr_root <- local({
  cand <- character(0)
  if (exists("project_root", inherits = TRUE) && is.character(project_root) &&
      length(project_root) == 1L && !is.na(project_root))
    cand <- c(cand, project_root, sub("(/|\\\\)?cons(/|\\\\)?$", "", project_root))
  pr <- tryCatch(here::here(), error = function(e) NA_character_)
  if (length(pr) == 1L && !is.na(pr)) cand <- c(cand, pr, sub("(/|\\\\)?cons(/|\\\\)?$", "", pr))
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  cand <- c(cand, wd, sub("(/|\\\\)?cons(/|\\\\)?$", "", wd))
  cand <- unique(cand[nzchar(cand)])
  ok <- cand[vapply(cand, function(p) dir.exists(file.path(p, "data", "20241015_out")), logical(1))]
  if (!length(ok)) stop("annex-ds-refresh: cannot locate the project root.", call. = FALSE)
  normalizePath(ok[1L], winslash = "/", mustWork = FALSE)
})
project_root <- .dsr_root
.dsr_out <- file.path(.dsr_root, "data", "20241015_out")
message("annex-ds-refresh: project root ", .dsr_root)
if (!exists("evaluate_dual_cox_python_style", mode = "function", inherits = TRUE))
  source(file.path(.dsr_root, "cons", "_alt_scripts", "evaluate_dual_cox_python_style_boot.R"))
stopifnot("the dual-Cox engine is unavailable" =
  exists("evaluate_dual_cox_python_style", mode = "function", inherits = TRUE))
# ---- 2. Newest complete sidecar triplet -------------------------------------
.dsr_suf <- c(base = "dual_shap_ds_py", nph = "dual_shap_ds_upd_py", interact = "dual_shap_ds_int_py")
.dsr_all <- list.files(.dsr_out, pattern = paste0("^pred24.*__(", paste(.dsr_suf, collapse = "|"), ")[.]rds$"))
stopifnot("no prediction24 DS sidecars found under data/20241015_out" = length(.dsr_all) > 0L)
.dsr_stems <- unique(sub(paste0("__(", paste(.dsr_suf, collapse = "|"), ")[.]rds$"), "", .dsr_all))
.dsr_complete <- .dsr_stems[vapply(.dsr_stems, function(s)
  all(file.exists(file.path(.dsr_out, paste0(s, "__", .dsr_suf, ".rds")))), logical(1))]
stopifnot("no complete sidecar triplet found" = length(.dsr_complete) > 0L)
.dsr_mt <- vapply(.dsr_complete, function(s)
  max(file.info(file.path(.dsr_out, paste0(s, "__", .dsr_suf, ".rds")))$mtime),
  as.POSIXct("1970-01-01", tz = "UTC"))
DSR_STEM <- .dsr_complete[which.max(.dsr_mt)]
DSR_PATHS <- stats::setNames(file.path(.dsr_out, paste0(DSR_STEM, "__", .dsr_suf, ".rds")), names(.dsr_suf))
DS_CACHE_NEW <- file.path(.dsr_out, paste0("pred225_ds_common_grid__", DSR_STEM, "__v1.rds"))
cat(sprintf("annex-ds-refresh: newest complete triplet = %s\n  target cache = %s\n",
            DSR_STEM, basename(DS_CACHE_NEW)))
if (length(.dsr_complete) > 1L)
  cat("  other complete stems on disk:", paste(setdiff(.dsr_complete, DSR_STEM), collapse = ", "), "\n")
if (file.exists(DS_CACHE_NEW)) {
  cat("annex-ds-refresh: that cache already exists; loading it and stopping here.\n")
  .dsr_have <- readRDS(DS_CACHE_NEW)
  ds_dual_fits <- .dsr_have$ds_dual_fits
  cat("  arms:", paste(names(ds_dual_fits), collapse = ", "), "\n")
} else {
# ---- 3. Formulas from the sidecars, exactly as deltaCIBS-ds-common-grid reads them
.dsr_parse <- function(v, label) {
  f <- if (inherits(v, "formula")) v else if (is.character(v) && length(v) == 1L && nzchar(v))
    stats::as.formula(v, env = globalenv()) else stop("invalid DS formula in ", label, call. = FALSE)
  environment(f) <- globalenv()
  f
}
.dsr_key <- function(v) gsub("[[:space:]]+", "", paste(deparse(.dsr_parse(v, "key")), collapse = " "))
.dsr_sets <- lapply(names(DSR_PATHS), function(lab) {
  sc <- readRDS(DSR_PATHS[[lab]])
  cfg <- sc$results$risk$config
  stopifnot("sidecar grid differs from the common grid" =
    identical(as.numeric(cfg$eval_times), DSR_GRID))
  out <- list(formula_readmit = .dsr_parse(cfg$formula_readmit, paste(lab, "readmission")),
              formula_death   = .dsr_parse(cfg$formula_death,   paste(lab, "death")))
  rm(sc, cfg); invisible(gc(verbose = FALSE))
  out
})
names(.dsr_sets) <- names(DSR_PATHS)
# The five arms, each varying ONE outcome away from the base pair.
ds_specs <- list(
  ds_base             = list(formula_readmit = .dsr_sets$base$formula_readmit,     formula_death = .dsr_sets$base$formula_death),
  ds_nph_readmit      = list(formula_readmit = .dsr_sets$nph$formula_readmit,      formula_death = .dsr_sets$base$formula_death),
  ds_interact_readmit = list(formula_readmit = .dsr_sets$interact$formula_readmit, formula_death = .dsr_sets$base$formula_death),
  ds_nph_death        = list(formula_readmit = .dsr_sets$base$formula_readmit,     formula_death = .dsr_sets$nph$formula_death),
  ds_interact_death   = list(formula_readmit = .dsr_sets$base$formula_readmit,     formula_death = .dsr_sets$interact$formula_death))
.dsr_sig <- lapply(ds_specs, function(sp)
  c(readmission = .dsr_key(sp$formula_readmit), death = .dsr_key(sp$formula_death)))
# ---- 4. What can be carried over from an older cache -------------------------
# An arm is reused only when its STORED formula keys equal the new specification
# exactly. Anything else is refit. This is the whole saving and it is verified,
# not assumed.
.dsr_old <- NULL
if (!isTRUE(DSR_FORCE_FULL_REFIT)) {
  .dsr_olds <- list.files(.dsr_out, pattern = "^pred225_ds_common_grid__.*[.]rds$", full.names = TRUE)
  .dsr_olds <- setdiff(.dsr_olds, DS_CACHE_NEW)
  if (length(.dsr_olds)) {
    .dsr_of <- .dsr_olds[order(file.info(.dsr_olds)$mtime, decreasing = TRUE)][1L]
    cat(sprintf("annex-ds-refresh: reading the previous cache %s (%d MB) to see what can be reused.\n",
                basename(.dsr_of), round(file.info(.dsr_of)$size / 1024^2))); flush.console()
    .dsr_old <- tryCatch(readRDS(.dsr_of)$ds_dual_fits, error = function(e) {
      cat("  could not read it (", conditionMessage(e), "); every arm will be refit.\n", sep = ""); NULL })
  }
}
.dsr_reuse <- vapply(names(ds_specs), function(nm) {
  if (is.null(.dsr_old) || is.null(.dsr_old[[nm]])) return(FALSE)
  f <- .dsr_old[[nm]]
  isTRUE(!is.null(f$raw_predictions)) &&
    identical(as.numeric(f$config$eval_times), DSR_GRID) &&
    identical(.dsr_key(f$config$formula_readmit), .dsr_sig[[nm]][["readmission"]]) &&
    identical(.dsr_key(f$config$formula_death),   .dsr_sig[[nm]][["death"]])
}, logical(1))
cat("annex-ds-refresh: arm plan\n")
for (nm in names(ds_specs))
  cat(sprintf("  %-20s %s\n", nm, if (.dsr_reuse[[nm]]) "reuse (formulas unchanged)" else "REFIT"))
cat(sprintf("  -> %d of %d arms need refitting, about %d Cox fits.\n",
            sum(!.dsr_reuse), length(ds_specs), sum(!.dsr_reuse) * 2L * 5L * DSR_FOLDS))
# ---- 5. Development data, only if something has to be refit -------------------
if (any(!.dsr_reuse)) {
  if (!exists("py_corrected_datasets_boot", inherits = TRUE)) {
    .dsr_rd <- file.path(.dsr_out, "pred22_ndp_2026_07_13.Rdata")
    stopifnot("the canonical pred22 RData is unavailable" = file.exists(.dsr_rd))
    cat("annex-ds-refresh: loading", basename(.dsr_rd), "- 5.6 GB, a few minutes.\n"); flush.console()
    .dsr_e <- new.env(parent = emptyenv()); load(.dsr_rd, envir = .dsr_e)
    py_corrected_datasets_boot <- get("py_corrected_datasets_boot", envir = .dsr_e)
    rm(.dsr_e); invisible(gc())
  }
  stopifnot("expected five canonical imputations" =
    is.list(py_corrected_datasets_boot) && length(py_corrected_datasets_boot) == 5L)
}
# ---- 6. Refit what changed ---------------------------------------------------
# Sequential on purpose: evaluate_dual_cox_python_style already parallelises
# internally, and nesting a second layer is the oversubscription problem this
# notebook has hit before.
ds_dual_fits <- vector("list", length(ds_specs)); names(ds_dual_fits) <- names(ds_specs)
for (nm in names(ds_specs)) {
  if (.dsr_reuse[[nm]]) {
    ds_dual_fits[[nm]] <- .dsr_old[[nm]]
    cat(sprintf("annex-ds-refresh: carried over %s\n", nm)); flush.console()
    next
  }
  cat(sprintf("annex-ds-refresh: refitting %s (%.1f min elapsed)\n", nm,
              (proc.time() - .dsr_t0)[["elapsed"]] / 60)); flush.console()
  .dsr_opt <- options(dualcox.concordance_score = "lp", dualcox.concordance_within_strata = FALSE)
  ds_dual_fits[[nm]] <- evaluate_dual_cox_python_style(
    formula_readmit = ds_specs[[nm]]$formula_readmit,
    formula_death   = ds_specs[[nm]]$formula_death,
    imputed_list    = py_corrected_datasets_boot,
    k_folds = DSR_FOLDS, eval_times = DSR_GRID, seed = DSR_SEED, verbose = TRUE)
  options(.dsr_opt)
  invisible(gc())
}
# ---- 7. Validate every arm the way deltaCIBS-ds-common-grid validates it ------
.dsr_ref_folds <- ds_dual_fits[[1]]$fold_assignments
for (nm in names(ds_specs)) {
  f <- ds_dual_fits[[nm]]
  stopifnot("incomplete raw predictions" =
    !is.null(f$raw_predictions) && length(f$raw_predictions) == 5L * DSR_FOLDS)
  stopifnot("grid differs" = identical(as.numeric(f$config$eval_times), DSR_GRID))
  stopifnot("stored formulas differ from the new specification" =
    identical(.dsr_key(f$config$formula_readmit), .dsr_sig[[nm]][["readmission"]]) &&
    identical(.dsr_key(f$config$formula_death),   .dsr_sig[[nm]][["death"]]))
  # One fold assignment across every arm, carried over or freshly fitted. If a
  # reused arm disagreed here, mixing it with a new fit would silently break the
  # pairing that all the annex comparisons rest on.
  stopifnot("fold assignments differ across arms" =
    identical(f$fold_assignments, .dsr_ref_folds))
}
cat("annex-ds-refresh: all five arms validated; one shared fold assignment.\n")
# ---- 8. Write the new cache atomically, never touching the old one ------------
.dsr_bundle <- list(
  sidecar_stem = DSR_STEM, sidecar_paths = DSR_PATHS,
  input_rdata = if (exists(".p225_input_manifest", inherits = TRUE)) .p225_input_manifest$path else
    normalizePath(file.path(.dsr_out, "pred22_ndp_2026_07_13.Rdata"), winslash = "/", mustWork = FALSE),
  input_manifest = if (exists(".p225_input_manifest", inherits = TRUE)) .p225_input_manifest else NULL,
  grid = DSR_GRID, folds = DSR_FOLDS, seed = DSR_SEED,
  specs_signature = .dsr_sig, ds_dual_fits = ds_dual_fits,
  saved_at = Sys.time(),
  built_by = "annex-ds-cache-refresh",
  reused_arms = names(ds_specs)[.dsr_reuse], refit_arms = names(ds_specs)[!.dsr_reuse])
stopifnot("refusing to overwrite an existing cache" = !file.exists(DS_CACHE_NEW))
.dsr_tmp <- tempfile(pattern = "pred225_ds_common_grid_", tmpdir = .dsr_out, fileext = ".rds")
saveRDS(.dsr_bundle, .dsr_tmp)
.dsr_back <- readRDS(.dsr_tmp)
if (!identical(.dsr_back$specs_signature, .dsr_sig) ||
    !identical(names(.dsr_back$ds_dual_fits), names(ds_specs))) {
  unlink(.dsr_tmp); stop("annex-ds-refresh: the new cache failed its read-back check.", call. = FALSE)
}
if (!file.rename(.dsr_tmp, DS_CACHE_NEW)) {
  unlink(.dsr_tmp); stop("annex-ds-refresh: could not finalise the new cache.", call. = FALSE)
}
cat("annex-ds-refresh: wrote", normalizePath(DS_CACHE_NEW, winslash = "/", mustWork = TRUE), "\n")
rm(.dsr_old, .dsr_bundle, .dsr_back); invisible(gc())
}
cat(sprintf("annex-ds-refresh: done in %.1f min.\n", (proc.time() - .dsr_t0)[["elapsed"]] / 60))
cat("NEXT: rerun the annex build and run cells with ANNEX_PDC_FORCE_REBUILD <- TRUE and\n")
cat("ANNEX_CRC_FORCE_REBUILD <- TRUE so they discard input caches derived from the old arms.\n")
