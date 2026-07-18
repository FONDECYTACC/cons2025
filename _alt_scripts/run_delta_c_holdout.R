# =============================================================================
# run_delta_c_holdout.R
# Driver: paired delta C (Uno, absolute risk 1 - S(t)) on the held-out 20% test
# set for the two death candidates (best_perf1 = Full PH, best_perf2 = SHAP).
# Readmission is shared between the two best_perf models, so its delta C is 0 by
# construction (identical formula, data, fit) and is only run as a sanity check.
#
# Reuses results_boot_val_bp1 / results_boot_val_bp2 if already in the session;
# otherwise rebuilds them from the cached held-out train/val split.
#
# Usage (from project root g:/My Drive/Alvacast/SISTRAT 2023):
#   source("cons/_alt_scripts/run_delta_c_holdout.R")
#   tab <- run_delta_c_holdout(B = 500L)          # returns the tidy delta-C table
# =============================================================================

if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}
setwd(project_root)
message("project_root = ", project_root)

source(file.path(project_root, "cons/_alt_scripts/val_holdout_02_build_sets.R"))
source(file.path(project_root, "cons/_alt_scripts/validate_holdout_metrics.R"))
source(file.path(project_root, "cons/_alt_scripts/delta_c_holdout.R"))

run_delta_c_holdout <- function(
    eval_times = c(3, 6, 12, 36, 60),      # canonical grid; must contain the horizons
    horizons   = c(6, 12, 36, 60),
    B          = 500L,
    seed       = 2125L,
    test_frac  = NULL,                       # e.g. 0.2 for a fast smoke test
    check_readmission = TRUE,
    verbose    = TRUE) {

  .msg <- function(...) if (verbose) { cat(...); flush.console() }

  have_session <- exists("results_boot_val_bp1", inherits = TRUE) &&
                  exists("results_boot_val_bp2", inherits = TRUE)

  if (have_session) {
    .msg("Using in-session results_boot_val_bp1 / results_boot_val_bp2.\n")
    rbp1 <- get("results_boot_val_bp1", inherits = TRUE)
    rbp2 <- get("results_boot_val_bp2", inherits = TRUE)
  } else {
    .msg("Rebuilding held-out results from the cached train/val split...\n")
    hd <- build_holdout_datasets(force = FALSE, verify = TRUE, verbose = verbose)
    train_list <- hd$train; val_list <- hd$val
    if (!is.null(test_frac)) {
      set.seed(seed)
      sub <- function(dl, f) lapply(dl, function(d) d[sort(sample.int(nrow(d), floor(nrow(d) * f))), , drop = FALSE])
      train_list <- sub(train_list, test_frac); val_list <- sub(val_list, test_frac)
      .msg(sprintf("  SMOKE: subsampled to train=%d, val=%d rows.\n",
                   nrow(train_list[[1]]), nrow(val_list[[1]])))
    }
    fms <- readRDS(file.path(project_root, "data/20241015_out/_val_inputs/formulas.rds"))
    f_readmit <- fms$formula_shap_readmit_clean_updated
    .msg("  Fitting best_perf1 (death = formula_death_updated2)...\n")
    rbp1 <- evaluate_dual_cox_holdout(f_readmit, fms$formula_death_updated2,
                                      train_list, val_list, eval_times = eval_times, verbose = verbose)
    .msg("  Fitting best_perf2 (death = formula_shap_death)...\n")
    rbp2 <- evaluate_dual_cox_holdout(f_readmit, fms$formula_shap_death,
                                      train_list, val_list, eval_times = eval_times, verbose = verbose)
  }

  .msg("\n== Paired delta C on absolute risk 1 - S(t) | DEATH: best_perf1 (Full PH) - best_perf2 (SHAP) ==\n")
  tab_death <- delta_c_holdout_absrisk(rbp1, rbp2, outcome = "death",
                                       horizons = horizons, B = B, seed = seed,
                                       label_A = "best_perf1", label_B = "best_perf2",
                                       verbose = verbose)

  tab_readmit <- NULL
  if (isTRUE(check_readmission)) {
    .msg("\n== Sanity check | READMISSION (shared formula -> delta C must be ~0) ==\n")
    tab_readmit <- delta_c_holdout_absrisk(rbp1, rbp2, outcome = "readmission",
                                           horizons = horizons, B = B, seed = seed,
                                           label_A = "best_perf1", label_B = "best_perf2",
                                           verbose = verbose)
    if (max(abs(tab_readmit$delta_C), na.rm = TRUE) > 1e-8)
      warning("Readmission delta C is not 0 although the readmission formula is shared; investigate.",
              call. = FALSE)
  }

  out <- rbind(tab_death, tab_readmit)
  out_dir <- file.path(project_root, "cons", "_out")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  csv <- file.path(out_dir, "holdout_delta_c_absrisk_death.csv")
  utils::write.csv(out, csv, row.names = FALSE)
  .msg("\nSaved: ", csv, "\n")

  invisible(list(death = tab_death, readmission = tab_readmit, table = out))
}

# Auto-run when sourced non-interactively (e.g. Rscript run_delta_c_holdout.R B),
# but stay silent (define-only) when sourced from a notebook or another script.
if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  if (!interactive()) {
    .args <- commandArgs(trailingOnly = TRUE)
    .B <- if (length(.args) >= 1L) suppressWarnings(as.integer(.args[[1]])) else 500L
    if (is.na(.B)) .B <- 500L
    .smoke <- if (length(.args) >= 2L) suppressWarnings(as.numeric(.args[[2]])) else NA_real_
    res <- run_delta_c_holdout(B = .B, test_frac = if (is.na(.smoke)) NULL else .smoke)
    cat("\n================ delta C (absolute risk) table ================\n")
    print(res$table, row.names = FALSE)
  }
}
