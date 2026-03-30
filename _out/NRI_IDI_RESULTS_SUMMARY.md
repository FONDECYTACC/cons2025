# NRI and IDI Results Summary

**Date Generated:** 2026-03-30  
**Models Compared:** Old Model (Cox) vs New Model (XGBoost)  
**Analysis Method:** Bootstrap with 25 replicates

---

## Key Findings

### Death Prediction

| Horizon | Categorical NRI | 95% CI | Continuous NRI | 95% CI | IDI | 95% CI |
|---------|-----------------|--------|----------------|--------|-----|--------|
| 3 mo    | 0.038           | (-0.015, 0.102) | **0.350** | (0.068, 0.599) | 0.003 | (0.002, 0.006) |
| 6 mo    | **0.061**       | (0.014, 0.161)  | **0.370** | (0.163, 0.533) | **0.004** | (0.002, 0.007) |
| 12 mo   | **0.074**       | (0.005, 0.156)  | **0.277** | (0.008, 0.486) | **0.005** | (0.002, 0.009) |
| 24 mo   | 0.061           | (-0.004, 0.122) | **0.184** | (0.040, 0.305) | **0.006** | (0.002, 0.010) |
| 36 mo   | 0.035           | (-0.032, 0.107) | 0.125     | (-0.016, 0.233)| **0.006** | (0.002, 0.010) |
| 48 mo   | 0.027           | (-0.052, 0.112) | 0.116     | (-0.003, 0.192)| **0.007** | (0.002, 0.011) |
| 60 mo   | 0.027           | (-0.048, 0.086) | 0.106     | (-0.013, 0.177)| **0.007** | (0.002, 0.012) |

**Interpretation:**
- The XGBoost model significantly improves death prediction at 6 and 12 months
- Continuous NRI is consistently positive (0.11-0.37) across all horizons
- IDI is small but consistently positive (0.003-0.009), indicating improved discrimination
- At 12 months: 7.4% net improvement in correct risk classification

### Readmission Prediction

| Horizon | Categorical NRI | 95% CI | Continuous NRI | 95% CI | IDI | 95% CI |
|---------|-----------------|--------|----------------|--------|-----|--------|
| 3 mo    | 0.001           | (-0.055, 0.046) | -0.026 | (-0.098, 0.070) | ~0 | (-0.001, 0.001) |
| 6 mo    | -0.005          | (-0.036, 0.017) | -0.022 | (-0.084, 0.034) | ~0 | (-0.001, 0.001) |
| 12 mo   | -0.001          | (-0.028, 0.030) | -0.023 | (-0.091, 0.037) | ~0 | (-0.002, 0.001) |
| 24 mo   | 0.001           | (-0.023, 0.021) | 0.009  | (-0.041, 0.062) | ~0 | (-0.001, 0.001) |
| 60 mo   | **0.018**       | (0.002, 0.036)  | **0.068**| (0.026, 0.123) | 0.002 | (0.000, 0.003) |
| 96 mo   | 0.009           | (-0.007, 0.025) | **0.128**| (0.064, 0.183) | **0.004** | (0.002, 0.006) |

**Interpretation:**
- XGBoost shows minimal improvement for readmission prediction at early horizons
- Some improvement at longer horizons (60-96 months) with continuous NRI of 0.07-0.13
- IDI values are very small, suggesting limited added discriminative value

---

## Detailed Results by Outcome

### Death at 12 Months (Primary Endpoint)

**NRI Components:**
- Events correctly upgraded to high risk: +8.0% (SE ~2.5%)
- Non-events correctly downgraded to low risk: -0.6% (SE ~0.3%)
- Total NRI: 7.4% (p < 0.05)

**IDI Components:**
- Discrimination (new model): 0.022
- Discrimination (old model): 0.017
- Improvement: 0.005 (27% relative improvement)

**Risk Stratification (using X-tile cutoff ~0.15):**

| Category | Old Model | New Model |
|----------|-----------|-----------|
| Mean risk (events) | 0.028 | 0.033 |
| Mean risk (non-events) | 0.010 | 0.010 |
| Discrimination slope | 0.017 | 0.022 |

---

## Statistical Interpretation

### What is NRI?
Net Reclassification Improvement measures the net proportion of patients correctly reclassified by the new model:
- **Events** moved to higher risk category = Good
- **Non-events** moved to lower risk category = Good
- NRI = 0 means no net improvement
- NRI > 0 means improvement

### What is IDI?
Integrated Discrimination Improvement measures the improvement in separation between event and non-event distributions:
- IDI = (New model's discrimination) - (Old model's discrimination)
- IDI > 0 means the new model better separates events from non-events

### Clinical Interpretation

**For Death Prediction:**
- XGBoost provides meaningful improvement over Cox model
- The improvement is strongest at 6-12 month horizons
- At 12 months, ~7% of patients are correctly reclassified
- The new model identifies high-risk patients more accurately

**For Readmission Prediction:**
- XGBoost provides minimal added value over Cox model
- Both models perform similarly for readmission prediction
- Consider using the simpler Cox model for readmission

---

## Files Available

| File | Description |
|------|-------------|
| `results_boot_reclassification_summary.csv` | Aggregated statistics (mean, 95% CI) |
| `results_boot_reclassification_raw.csv` | Individual bootstrap replicate results |
| `results_boot_reclassification_results.rds` | Full R object with all results |

---

## Next Steps

1. **Decision Curve Analysis (DCA)** - Already calculated via `adca_from_results_boot.R`
2. **Calibration Plots** - Compare predicted vs observed probabilities
3. **Clinical Utility** - Determine if improvement warrants model complexity

## References

1. Pencina MJ et al. Stat Med. 2008;27:157-172
2. Pencina MJ et al. Stat Med. 2011;30:11-21
3. Kerr KF et al. J Clin Oncol. 2016;34:2534-2540
