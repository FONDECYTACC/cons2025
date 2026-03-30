# NRI and IDI Calculation Guide

This guide explains how to calculate Net Reclassification Improvement (NRI) and Integrated Discrimination Improvement (IDI) comparing Cox and XGBoost models.

## Overview

- **NRI**: Measures how well a new model (XGBoost) reclassifies patients compared to a reference model (Cox)
  - NRI > 0: New model improves classification
  - NRI = 0: No improvement
  - NRI < 0: New model worsens classification

- **IDI**: Measures improvement in separation between event and non-event distributions
  - IDI > 0: New model has better discrimination

## Prerequisites

1. **XGBoost predictions**: `xgb/_out/xgb6_corr_DUAL_final_ev_hyp_20260223_2134.pkl`
2. **Cox model results**: Path to your `results_boot.rds` file from `evaluate_dual_cox_python_style()`
3. **Risk cutoff**: The X-tile determined cutoff for high/low risk classification

## Quick Start (Recommended)

### Step 1: Extract Predictions

Use the Python script to extract predictions from both models:

```bash
python cons/_hist_scripts/extract_predictions_for_nri_idi.py \
    --xgb-pickle "xgb/_out/xgb6_corr_DUAL_final_ev_hyp_20260223_2134.pkl" \
    --cox-rds "cons/_out/your_results_boot.rds" \
    --risk death \
    --horizon 12 \
    --output "cons/_out/predictions_death_12m.csv"
```

Parameters:
- `--risk`: Choose `death` or `readmission`
- `--horizon`: Time horizon in months (e.g., 12, 24, 60)
- `--cox-rds`: Path to your Cox model results_boot RDS file

### Step 2: Calculate NRI/IDI in R

```r
source("cons/_hist_scripts/nri_idi_simple.R")

results <- calculate_nri_idi_from_csv(
  csv_path = "cons/_out/predictions_death_12m.csv",
  col_time = "time",
  col_event = "event",
  col_pred_m1 = "pred_cox",    # Reference model
  col_pred_m2 = "pred_xgb",    # New model
  cutoff = 0.15,               # X-tile determined cutoff
  n_boot = 1000                # Bootstrap CI
)
```

## Alternative: Direct R Approach

If you prefer to work entirely in R and have `reticulate` installed:

```r
source("cons/_hist_scripts/nri_idi_from_results_boot.R")

results <- run_nri_idi_comparison(
  results_boot_cox = "cons/_out/your_results_boot.rds",
  xgb_pickle_path = "xgb/_out/xgb6_corr_DUAL_final_ev_hyp_20260223_2134.pkl",
  risk = "death",
  horizon = 12,
  cutoffs = 0.15,
  n_boot = 1000,
  output_dir = "cons/_out/nri_idi_results"
)
```

## Interpreting Results

### NRI Components

```
NRI = NRI_events + NRI_nonevents

- NRI_events: Proportion of events correctly moved to higher risk category
- NRI_nonevents: Proportion of non-events correctly moved to lower risk category
```

Example output:
```
NRI: 0.0856 [0.0234, 0.1478]
  Events: 0.1234      (more events correctly identified as high risk)
  Non-events: -0.0378 (some non-events misclassified as high risk)
  p-value: 0.007
```

### IDI

```
IDI = (mean_pred_new_events - mean_pred_new_nonevents) -
      (mean_pred_ref_events - mean_pred_ref_nonevents)
```

Example:
```
IDI: 0.0234 [0.0123, 0.0345]
  Relative: 8.5%     (IDI is 8.5% of reference model's discrimination)
```

### Reclassification Tables

Shows how patients moved between risk categories:

```
Reclassification - Events:
         Model2
Model1      Low  High
  Low      45    15    <- 15 events upgraded (good!)
  High      8    32    <- 8 events downgraded (bad)

Reclassification - Non-events:
         Model2
Model1      Low  High
  Low     420    35    <- 35 non-events upgraded (bad)
  High     28   217    <- 28 non-events downgraded (good)
```

## Multiple Horizons

To calculate NRI/IDI at multiple time points:

```r
source("cons/_hist_scripts/nri_idi_simple.R")

df <- read.csv("cons/_out/predictions_death_12m.csv")

horizons <- c(12, 24, 60)
results_list <- list()

for (h in horizons) {
  # Note: You need to extract predictions at each horizon separately
  # using the Python script with --horizon parameter
}
```

## Files Generated

| File | Description |
|------|-------------|
| `predictions_*.csv` | Combined predictions from both models |
| `*_summary.csv` | NRI/IDI statistics |
| `*_bootstrap_ci.csv` | Bootstrap confidence intervals |
| `*_reclass_table.txt` | Reclassification tables |

## Troubleshooting

### Error: "results_boot does not contain raw_predictions"
- Make sure you're using the correct `results_boot` object from `evaluate_dual_cox_python_style()`

### Error: "Horizon X not found"
- Check available horizons in your models (typically: 3, 6, 12, 24, 36, 48, 60, 96 months)

### Error: "reticulate not installed"
- Use the simple CSV-based approach instead

### Mismatched patient counts
- Ensure both models were trained/evaluated on the same data splits

## References

1. Pencina MJ, D'Agostino RB Sr, D'Agostino RB Jr, Vasan RS. Evaluating the added predictive ability of a new marker: From area under the ROC curve to reclassification and beyond. Stat Med. 2008;27(2):157-172.

2. Pencina MJ, D'Agostino RB Sr, Steyerberg EW. Extensions of net reclassification improvement calculations to measure usefulness of new biomarkers. Stat Med. 2011;30(1):11-21.

3. Kerr KF, Brown MD, Zhu K, Janes H. Assessing the clinical impact of risk prediction models with decision curves: Guidance for correct interpretation and appropriate use. J Clin Oncol. 2016;34(21):2534-2540.
