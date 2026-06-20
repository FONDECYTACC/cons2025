# =============================================================================
# run_validation_holdout.R
# Orchestrator: held-out (20% validation) internal validation of prediction23
# for best_perf1 and best_perf2, with prediction225-style metrics AND an ipeval
# bootstrap cross-check.
#
#   best_perf1 (SHAP primary):     readmit = formula_shap_readmit_clean_updated
#                                  death   = formula_death_updated2
#   best_perf2 (SHAP implemented):  readmit = formula_shap_readmit_clean_updated
#                                  death   = formula_shap_death
#
# Usage (from project root g:/My Drive/Alvacast/SISTRAT 2023):
#   source("cons/_alt_scripts/run_validation_holdout.R")
#   res <- run_validation_holdout(eval_times = c(3,6,9,12,24,36,48,60,72,84,96,108),
#                                 dca_horizons = c(6,12,36,60),
#                                 run_ipeval = TRUE, ipeval_bootstrap = 500L)
# Set TEST_MODE = TRUE (or run_validation_holdout(test_frac = 0.2, ...)) to
# subsample for a fast smoke test.
#
# Output: list saved to cons/_out/pred23_holdout_validation_<date>.rds  + CSVs.
# =============================================================================


# --- Resolve the project root robustly (works from the project root or from cons/,
#     reusing a `project_root` set earlier, e.g. in the notebook). ---
if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}
# Anchor the working directory so the audited engines' internal relative source()
# calls (e.g. dca -> adca, nri -> adca) resolve correctly.
setwd(project_root)
message("project_root = ", project_root)

source(file.path(project_root, "cons/_alt_scripts/val_holdout_02_build_sets.R"))
source(file.path(project_root, "cons/_alt_scripts/validate_holdout_metrics.R"))
source(file.path(project_root, "cons/_alt_scripts/validate_holdout_ipeval.R"))
if (!exists("run_adca_from_results_boot", mode = "function"))
  source(file.path(project_root, "cons/_alt_scripts/dca_from_results_boot_for_metrics.R"))

run_validation_holdout <- function(
    eval_times       = c(3, 12, 36, 60),
    dca_horizons     = c(6, 12, 36, 60),
    cal_times        = c(6, 12, 36, 60),
    run_ipeval       = TRUE,
    ipeval_bootstrap = 500L,
    ipeval_parallel  = TRUE,
    test_frac        = NULL,     # e.g. 0.2 -> subsample for a smoke test
    seed             = 2125L,
    out_tag          = format(Sys.Date(), "%Y_%m_%d"),
    verbose          = TRUE) {

  .msg <- function(...) if (verbose) { cat(...); flush.console() }

  # ---- 1. Data ----
  hd <- build_holdout_datasets(force = FALSE, verify = TRUE, verbose = verbose)
  train_list <- hd$train; val_list <- hd$val
  if (!is.null(test_frac)) {
    set.seed(seed)
    sub <- function(dl, frac) lapply(dl, function(d) d[sort(sample.int(nrow(d), floor(nrow(d) * frac))), , drop = FALSE])
    train_list <- sub(train_list, test_frac); val_list <- sub(val_list, test_frac)
    .msg(sprintf("TEST MODE: subsampled to train=%d, val=%d rows.\n",
                 nrow(train_list[[1]]), nrow(val_list[[1]])))
  }

  # ---- 2. Formulas + model definitions ----
  fms <- readRDS(file.path(project_root, "data/20241015_out/_val_inputs/formulas.rds"))
  f_readmit <- fms$formula_shap_readmit_clean_updated
  models <- list(
    best_perf1 = list(readmit = f_readmit, death = fms$formula_death_updated2),
    best_perf2 = list(readmit = f_readmit, death = fms$formula_shap_death)
  )

  # ---- 3. C-index + IBS + raw predictions (held-out) ----
  .msg("\n== C-index / IBS (held-out) ==\n")
  results_boot_val <- lapply(names(models), function(mn) {
    .msg("  ", mn, "\n")
    evaluate_dual_cox_holdout(models[[mn]]$readmit, models[[mn]]$death,
                              train_list, val_list, eval_times = eval_times,
                              verbose = verbose)
  })
  names(results_boot_val) <- names(models)
  cindex_ibs_global <- do.call(rbind, lapply(names(results_boot_val), function(mn) {
    s <- results_boot_val[[mn]]$summary
    s <- s[s$Time == "Global", , drop = FALSE]; if (nrow(s)) s$model <- mn; s
  }))

  # ---- 4. DCA (held-out): reuse AJ-readmission / KM-death engine ----
  .msg("\n== DCA (held-out) ==\n")
  dca_models_full <- run_dca_full_summary(
    stats::setNames(results_boot_val, names(results_boot_val)),
    horizons = dca_horizons, verbose = verbose)
  dca_nb <- lapply(dca_models_full, function(m) if (is.null(m)) NULL else summarize_dca_nb(m$summary))

  # ---- 5. Calibration (held-out) ----
  .msg("\n== Calibration (held-out) ==\n")
  cal_readmit <- calibrate_readmit_holdout(f_readmit, train_list, val_list,
                                           times = cal_times, verbose = verbose)
  cal_death <- list(
    best_perf1 = calibrate_death_holdout(fms$formula_death_updated2, train_list, val_list,
                                         times = cal_times, verbose = verbose),
    best_perf2 = calibrate_death_holdout(fms$formula_shap_death, train_list, val_list,
                                         times = cal_times, verbose = verbose)
  )

  # ---- 6. ipeval bootstrap cross-check ----
  ipeval_res <- NULL
  if (isTRUE(run_ipeval)) {
    if (.vhi_have_ipeval()) {
      .msg("\n== ipeval bootstrap cross-check (B=", ipeval_bootstrap, ") ==\n")
      ipeval_res <- tryCatch(
        ipeval_holdout(models, train_list, val_list, horizons = dca_horizons,
                       bootstrap = ipeval_bootstrap, seed = seed,
                       parallel = ipeval_parallel, verbose = verbose),
        error = function(e) { .msg("  ipeval failed: ", conditionMessage(e), "\n"); NULL })
    } else {
      .msg("\n[skip] ipeval not installed; run renv::install('ipeval') to enable.\n")
    }
  }

  # ---- 7. Assemble + save ----
  out <- list(
    results_boot_val   = results_boot_val,       # for prediction23 cells & DCA
    cindex_ibs_global  = cindex_ibs_global,
    cindex_ibs_summary = lapply(results_boot_val, `[[`, "summary"),
    dca_models_full    = dca_models_full,
    dca_nb             = dca_nb,
    cal_readmit        = cal_readmit,
    cal_death          = cal_death,
    ipeval             = ipeval_res,
    meta = list(n_train = nrow(train_list[[1]]), n_val = nrow(val_list[[1]]),
                eval_times = eval_times, dca_horizons = dca_horizons,
                cal_times = cal_times, test_frac = test_frac,
                split_file = hd$split_file, checks = hd$checks,
                models = lapply(models, function(m) lapply(m, function(f) paste(deparse(f), collapse=" "))),
                created = as.character(Sys.time()))
  )

  out_dir <- file.path(project_root, "cons", "_out")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  rds_path <- file.path(out_dir, sprintf("pred23_holdout_validation_%s.rds", out_tag))
  saveRDS(out, rds_path)
  utils::write.csv(cindex_ibs_global,
                   file.path(out_dir, sprintf("pred23_holdout_cindex_ibs_global_%s.csv", out_tag)),
                   row.names = FALSE)
  utils::write.csv(cal_readmit$pooled_summary,
                   file.path(out_dir, sprintf("pred23_holdout_cal_readmit_%s.csv", out_tag)),
                   row.names = FALSE)
  for (mn in names(cal_death))
    utils::write.csv(cal_death[[mn]]$pooled_summary,
                     file.path(out_dir, sprintf("pred23_holdout_cal_death_%s_%s.csv", mn, out_tag)),
                     row.names = FALSE)
  if (!is.null(ipeval_res) && !is.null(ipeval_res$pooled))
    utils::write.csv(ipeval_res$pooled,
                     file.path(out_dir, sprintf("pred23_holdout_ipeval_%s.csv", out_tag)),
                     row.names = FALSE)
  .msg("\nSaved: ", rds_path, "\n")
  out
}
