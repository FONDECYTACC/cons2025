#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Standalone reproduction of the FULL run-dual-pending loop in
# cons/prediction225_converted_mod.ipynb (all pending_configs).
#
# All formulas come straight from the .RData (verified [OK] in the notebook).
# Evaluator is single-threaded -> gentle on the interactive session.
# Concordance options: score = "risk", within_strata = FALSE.
# Each result is printed (Global) AND saved; a combined list is saved too.
# ---------------------------------------------------------------------------

t0 <- Sys.time()
project_root <- "g:/My Drive/Alvacast/SISTRAT 2023"
setwd(project_root)

source(file.path(project_root, "renv", "activate.R"))
suppressWarnings(suppressMessages(library(survival)))

cat(sprintf("[%s] Loading .RData (4.6 GB) ...\n", format(Sys.time(), "%H:%M:%S")))
flush.console()
rdata <- file.path(project_root, "data", "20241015_out", "pred22_ndp_2026_06_12.Rdata")
load(rdata)
stopifnot(exists("py_corrected_datasets_boot"))
cat(sprintf("  loaded: %d imputations, %d rows each\n",
            length(py_corrected_datasets_boot),
            nrow(py_corrected_datasets_boot[[1]])))

source("cons/_alt_scripts/evaluate_dual_cox_python_style_boot.R")
stopifnot(exists("evaluate_dual_cox_python_style"))

# clean strat( -> strata( (idempotent); applied to every formula used
.clean_strata <- function(f) {
  if (is.null(f) || !inherits(f, "formula")) return(f)
  stats::as.formula(gsub("strat\\(", "strata(", paste(deparse(f), collapse = " ")))
}
.get_clean <- function(nm) if (exists(nm)) .clean_strata(get(nm)) else NULL

# --- settings (match notebook) ---
EVAL_TIMES <- c(6, 12, 36, 60, 108)
K_FOLDS    <- 5
options(dualcox.concordance_score = "risk",
        dualcox.concordance_within_strata = FALSE)
cat(sprintf("options: concordance_score=%s | within_strata=%s\n",
            getOption("dualcox.concordance_score"),
            getOption("dualcox.concordance_within_strata")))

# --- pending_configs (mirror notebook), with cleaned formulas ---
pending_configs <- list(
  full_ph_base = list(
    label   = "Full PH base",
    readmit = .get_clean("formula_readmit"),
    death   = .get_clean("formula_death")
  ),
  full_ph_upd = list(
    label   = "Full PH updated",
    readmit = .get_clean("formula_readmit_updated"),
    death   = .get_clean("formula_death_updated")
  ),
  shap_readmit_clean_death_interact = list(
    label   = "SHAP clean readmit + SHAP death interact",
    readmit = .get_clean("formula_shap_readmit_clean_updated"),
    death   = .get_clean("formula_shap_death_interact")
  ),
  shap_readmit_interact_death_interact = list(
    label   = "SHAP interact readmit + SHAP death interact",
    readmit = .get_clean("formula_shap_readmit_interact"),
    death   = .get_clean("formula_shap_death_interact")
  ),
  shap_death_implem = list(
    label   = "SHAP clean readmit + SHAP death (13 vars)",
    readmit = .get_clean("formula_shap_readmit_clean_updated"),
    death   = .get_clean("formula_shap_death_rule2")
  )
)
pending_configs <- pending_configs[
  vapply(pending_configs, function(x) !is.null(x$readmit) && !is.null(x$death), logical(1))
]

cat("Pending configurations to evaluate:\n")
for (nm in names(pending_configs))
  cat(sprintf("  - %s: %s\n", nm, pending_configs[[nm]]$label))

# --- run loop ---
out_dir <- file.path(project_root, "data", "20241015_out")
dual_pending_results <- list()

for (nm in names(pending_configs)) {
  cfg <- pending_configs[[nm]]
  cat(sprintf("\n[%s] === Dual: %s ===\n", format(Sys.time(), "%H:%M:%S"), cfg$label))
  flush.console()

  dual_pending_results[[nm]] <- evaluate_dual_cox_python_style(
    formula_readmit = cfg$readmit,
    formula_death   = cfg$death,
    imputed_list    = py_corrected_datasets_boot,
    k_folds         = K_FOLDS,
    eval_times      = EVAL_TIMES
  )

  cat(sprintf("  OK - %s\n", cfg$label))
  s <- dual_pending_results[[nm]]$summary
  print(s[s$Time == "Global", ])
  # per-config save (so a late crash never loses completed work)
  saveRDS(dual_pending_results[[nm]],
          file.path(out_dir, sprintf("dual_pending_%s_2026_06_17.rds", nm)))
  flush.console()
}

# --- combined save ---
saveRDS(dual_pending_results,
        file.path(out_dir, "dual_pending_results_ALL_2026_06_17.rds"))

# combined Global table (csv) for quick reuse
glob <- do.call(rbind, lapply(names(dual_pending_results), function(nm) {
  s <- dual_pending_results[[nm]]$summary
  g <- s[s$Time == "Global", ]
  cbind(config = nm, label = pending_configs[[nm]]$label, g)
}))
write.csv(glob, file.path(out_dir, "dual_pending_results_ALL_2026_06_17_global.csv"),
          row.names = FALSE)

cat("\n=== COMBINED GLOBAL ===\n")
print(glob)
cat("\nSaved combined:", file.path(out_dir, "dual_pending_results_ALL_2026_06_17.rds"), "\n")
cat(sprintf("[%s] DONE in %.1f min\n", format(Sys.time(), "%H:%M:%S"),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
