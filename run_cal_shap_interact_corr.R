#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Standalone calibration for ONLY the 'shap_interact_corr' readmission model.
# Mirrors the cal loop in cons/prediction225_converted_mod.ipynb so the result
# is identical to what that chunk would have produced for this model.
#
# Gentle on resources: capped to N_WORKERS so the interactive session keeps
# running. Run in background; results are printed AND saved to disk.
# ---------------------------------------------------------------------------

t0 <- Sys.time()
project_root <- "g:/My Drive/Alvacast/SISTRAT 2023"
setwd(project_root)

# --- renv (mirror the notebook) ---
source(file.path(project_root, "renv", "activate.R"))

# Attach survival so strata()/Surv() resolve when model.frame evaluates the
# formula (the engine only requireNamespace()s it, which is not enough).
suppressWarnings(suppressMessages(library(survival)))

cat(sprintf("[%s] Loading .RData (4.6 GB, give it a minute) ...\n",
            format(Sys.time(), "%H:%M:%S")))
flush.console()
rdata <- file.path(project_root, "data", "20241015_out", "pred22_ndp_2026_06_12.Rdata")
load(rdata)
stopifnot(exists("py_corrected_datasets_boot"),
          exists("formula_shap_readmit_clean_updated"))
cat(sprintf("  loaded: %d imputations, %d rows each\n",
            length(py_corrected_datasets_boot),
            nrow(py_corrected_datasets_boot[[1]])))

# --- calibration engine (same source as the notebook) ---
source("cons/_alt_scripts/calibrate_cscox_aj_grouped_corr.R")

# --- gentle parallelism: set AFTER load so nothing overrides it ---
N_WORKERS <- 4L                              # 4 of 32 cores -> won't disturb your flow
options(future.globals.maxSize = 12 * 1024^3)
suppressWarnings(suppressMessages({
  library(future); library(future.apply)
}))
future::plan(future::multisession, workers = N_WORKERS)

# --- rebuild the formula EXACTLY as the notebook does ---
# 1) clean strat( -> strata(
formula_shap_readmit_clean_updated <- stats::as.formula(
  gsub("strat\\(", "strata(",
       paste(deparse(formula_shap_readmit_clean_updated), collapse = " "))
)
# 2) full family of primary_sub_mod_* taken from the DATA (not the formula)
fam <- grep("^primary_sub_mod_", names(py_corrected_datasets_boot[[1]]), value = TRUE)
cat("Family levels included in interaction:\n"); print(fam)
# 3) main effects + complete sex interaction
terms_add <- c(fam, paste0("sex_rec_woman:", fam))
formula_shap_readmit_interact_corr <- stats::update(
  formula_shap_readmit_clean_updated,
  stats::as.formula(paste(". ~ . +", paste(terms_add, collapse = " + ")))
)
# 4) defensive: ensure strata() survives in the final formula
formula_shap_readmit_interact_corr <- stats::as.formula(
  gsub("strat\\(", "strata(",
       paste(deparse(formula_shap_readmit_interact_corr), collapse = " "))
)

# --- calibration parameters (match notebook: CAL_TIMES / CAL_ICI_TIMES / CAL_N_REPEATS) ---
CAL_TIMES     <- c(6, 12, 36, 60)
CAL_ICI_TIMES <- c(6, 12, 36, 60)
CAL_N_REPEATS <- 20L

cat(sprintf("[%s] Running calibration: SHAP + interactions (corrected) on %d workers ...\n",
            format(Sys.time(), "%H:%M:%S"), N_WORKERS))
flush.console()

res <- calibrate_cscox_aj_grouped_corr(
  formula_readmit   = formula_shap_readmit_interact_corr,
  formula_death     = NULL,
  data              = py_corrected_datasets_boot,
  times             = CAL_TIMES,
  validation        = "cv",
  folds             = 5L,
  n_repeats         = CAL_N_REPEATS,
  compute_ici       = TRUE,
  ici_times         = CAL_ICI_TIMES,
  make_plots        = FALSE,
  parallel_cv       = TRUE,
  future_scheduling = 2,
  verbose           = TRUE,
  debug_fit         = FALSE,
  seed              = 2125
)

cat("\n  OK - SHAP + interactions (corrected)\n")
print(res$pooled_summary[, c("time_months", "ici_mean", "ici_sd", "ece_mean", "eo_mean")])

# --- persist so you can splice it back into cal_results without re-running ---
out_dir  <- file.path(project_root, "data", "20241015_out")
out_rds  <- file.path(out_dir, "cal_shap_interact_corr_2026_06_17.rds")
out_csv  <- file.path(out_dir, "cal_shap_interact_corr_2026_06_17_pooled_summary.csv")
saveRDS(res, out_rds)
write.csv(res$pooled_summary, out_csv, row.names = FALSE)
cat("\nSaved full result:", out_rds, "\n")
cat("Saved summary csv :", out_csv, "\n")

cat(sprintf("[%s] DONE in %.1f min\n",
            format(Sys.time(), "%H:%M:%S"),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
