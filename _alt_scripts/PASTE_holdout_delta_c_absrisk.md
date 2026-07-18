# Paste-in cell: paired delta C on absolute risk (held-out 20%)

Add this as a new code cell in `cons/prediction23_converted_mod.ipynb` **after**
`#| label: holdout-cindex-ibs-run` (cell that builds `results_boot_val_bp1` /
`results_boot_val_bp2`). It reuses those in-session objects and does not refit.

Rationale: the existing `#| label: holdout-cindex-bootstrap` cell ranks Uno's C on
the linear predictor `lp_val`. With stratified baseline hazards, `1 - S(t)` is not a
monotone transform of `lp`, so the concordance ranking is horizon-specific and differs
from the `lp` ranking. The CV pipeline reports performance under
`options(dualcox.concordance_score = "risk")`, i.e. on absolute risk. This cell brings
the held-out delta C onto the same score, mirroring
`#| label: paired-delta-c-absolute-risk` in `prediction225_converted_mod.ipynb`.

```r
#| label: holdout-delta-c-absolute-risk
#| message: true
#| warning: true

suppressPackageStartupMessages(library(survival))
source(file.path(if (exists("project_root")) project_root else getwd(),
                 "cons/_alt_scripts/delta_c_holdout.R"))
stopifnot(exists("results_boot_val_bp1"), exists("results_boot_val_bp2"))

DELTA_C_HORIZONS <- c(6, 12, 36, 60)
B_DELTA_C        <- 1000L        # smoke-test with 100 first
DELTA_C_SEED     <- 2125L

# DEATH: best_perf1 (Full PH) - best_perf2 (SHAP). delta_C > 0 favours best_perf1.
delta_c_death <- delta_c_holdout_absrisk(
  results_boot_val_bp1, results_boot_val_bp2, outcome = "death",
  horizons = DELTA_C_HORIZONS, B = B_DELTA_C, seed = DELTA_C_SEED,
  label_A = "best_perf1 (Full PH)", label_B = "best_perf2 (SHAP)")

# READMISSION is shared between the two best_perf models -> delta C must be 0.
delta_c_readmit <- delta_c_holdout_absrisk(
  results_boot_val_bp1, results_boot_val_bp2, outcome = "readmission",
  horizons = DELTA_C_HORIZONS, B = B_DELTA_C, seed = DELTA_C_SEED,
  label_A = "best_perf1", label_B = "best_perf2", verbose = FALSE)
stopifnot(max(abs(delta_c_readmit$delta_C), na.rm = TRUE) < 1e-8)

cat("\n== Held-out paired delta C on absolute risk 1 - S(t) | DEATH ==\n")
print(delta_c_death[, c("horizon", "C_A", "C_B", "delta_C",
                        "delta_C_lower", "delta_C_upper", "excludes_zero", "favours")],
      row.names = FALSE)
cat("\ndelta_C = C(best_perf1) - C(best_perf2); paired patient bootstrap of the held-out",
    "test set (model frozen, no refit), MI-pooled over imputations. 95% CI = 2.5/97.5",
    "percentile of the paired difference.\n")

if (exists("out_dir"))
  utils::write.csv(delta_c_death,
                   file.path(out_dir, "holdout_delta_c_absrisk_death.csv"),
                   row.names = FALSE)
```

## Standalone (outside the notebook)

```r
source("cons/_alt_scripts/run_delta_c_holdout.R")
res <- run_delta_c_holdout(B = 1000L)   # rebuilds from cache if the bp objects are absent
res$table
```

or from a shell:

```
Rscript --vanilla cons/_alt_scripts/run_delta_c_holdout.R 1000
```
