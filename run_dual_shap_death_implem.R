#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Standalone dual-Cox evaluation for ONLY the 'shap_death_implem' config of
# the run-dual-pending loop in cons/prediction225_converted_mod.ipynb:
#   readmit = formula_shap_readmit_clean_updated
#   death   = formula_shap_death_rule2  (SHAP death, 13 vars)
#
# The evaluator is single-threaded -> gentle on the interactive session.
# Run in background; result is printed AND saved to disk.
#
# Concordance options (SEE NOTE): score = "risk", within_strata = FALSE,
# matching line 931's canonical reset and the non-"_within" naming of the
# pending-loop results.
# ---------------------------------------------------------------------------

t0 <- Sys.time()
project_root <- "g:/My Drive/Alvacast/SISTRAT 2023"
setwd(project_root)

# --- renv + attach survival (strata()/Surv() must resolve at model.frame) ---
source(file.path(project_root, "renv", "activate.R"))
suppressWarnings(suppressMessages(library(survival)))

cat(sprintf("[%s] Loading .RData (4.6 GB) ...\n", format(Sys.time(), "%H:%M:%S")))
flush.console()
rdata <- file.path(project_root, "data", "20241015_out", "pred22_ndp_2026_06_12.Rdata")
load(rdata)
stopifnot(exists("py_corrected_datasets_boot"),
          exists("formula_shap_readmit_clean_updated"),
          exists("formula_shap_death_rule2"))
cat(sprintf("  loaded: %d imputations, %d rows each\n",
            length(py_corrected_datasets_boot),
            nrow(py_corrected_datasets_boot[[1]])))

# --- evaluator (same source as the notebook) ---
source("cons/_alt_scripts/evaluate_dual_cox_python_style_boot.R")
stopifnot(exists("evaluate_dual_cox_python_style"))

# --- clean strat( -> strata( on both formulas (mirrors notebook; idempotent) ---
.clean_strata <- function(f) stats::as.formula(
  gsub("strat\\(", "strata(", paste(deparse(f), collapse = " "))
)
formula_shap_readmit_clean_updated <- .clean_strata(formula_shap_readmit_clean_updated)
formula_shap_death_rule2           <- .clean_strata(formula_shap_death_rule2)

# --- settings (match notebook) ---
EVAL_TIMES <- c(6, 12, 36, 60, 108)
K_FOLDS    <- 5
options(dualcox.concordance_score = "risk",
        dualcox.concordance_within_strata = FALSE)

cat(sprintf("[%s] Running dual: SHAP clean readmit + SHAP death (13 vars) ...\n",
            format(Sys.time(), "%H:%M:%S")))
cat(sprintf("  options: concordance_score=%s | within_strata=%s\n",
            getOption("dualcox.concordance_score"),
            getOption("dualcox.concordance_within_strata")))
flush.console()

res <- evaluate_dual_cox_python_style(
  formula_readmit = formula_shap_readmit_clean_updated,
  formula_death   = formula_shap_death_rule2,
  imputed_list    = py_corrected_datasets_boot,
  k_folds         = K_FOLDS,
  eval_times      = EVAL_TIMES
)

cat("\n  OK - SHAP clean readmit + SHAP death (13 vars)\n")
cat("\n=== summary (Time == 'Global') ===\n")
print(res$summary[res$summary$Time == "Global", ])
cat("\n=== full summary ===\n")
print(res$summary)

# --- persist ---
out_dir <- file.path(project_root, "data", "20241015_out")
out_rds <- file.path(out_dir, "dual_shap_death_implem_2026_06_17.rds")
out_csv <- file.path(out_dir, "dual_shap_death_implem_2026_06_17_summary.csv")
saveRDS(res, out_rds)
write.csv(res$summary, out_csv, row.names = FALSE)
cat("\nSaved full result:", out_rds, "\n")
cat("Saved summary csv :", out_csv, "\n")

cat(sprintf("[%s] DONE in %.1f min\n", format(Sys.time(), "%H:%M:%S"),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
