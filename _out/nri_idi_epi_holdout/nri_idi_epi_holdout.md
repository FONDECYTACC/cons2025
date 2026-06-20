Table. Horizon-specific net reclassification improvement and integrated discrimination improvement comparing updated and reference dual Cox models for first treatment readmission and all-cause mortality after discharge

| Outcome | Horizon, months | Continuous NRI | Continuous NRI, events | Continuous NRI, nonevents | Categorical NRI | Categorical NRI, events | Categorical NRI, nonevents | IDI | Discrimination slope, reference vs updated |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Readmission |  6 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.018 (0.018 to 0.018) vs 0.018 (0.018 to 0.018) |
| Readmission | 12 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.024 (0.024 to 0.024) vs 0.024 (0.024 to 0.024) |
| Readmission | 36 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.032 (0.032 to 0.032) vs 0.032 (0.032 to 0.032) |
| Readmission | 60 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.030 (0.030 to 0.030) vs 0.030 (0.030 to 0.030) |
| Death |  6 | 0.728 (0.672 to 0.772) | 0.568 (0.514 to 0.611) | 0.161 (0.158 to 0.164) | 0.038 (0.018 to 0.052) | 0.041 (0.021 to 0.055) | -0.003 (-0.003 to -0.003) | 0.005 (0.005 to 0.006) | 0.010 (0.010 to 0.010) vs 0.016 (0.016 to 0.016) |
| Death | 12 | 0.606 (0.562 to 0.640) | 0.448 (0.406 to 0.482) | 0.158 (0.155 to 0.161) | 0.135 (0.126 to 0.145) | 0.140 (0.130 to 0.149) | -0.004 (-0.005 to -0.004) | 0.006 (0.006 to 0.006) | 0.016 (0.016 to 0.016) vs 0.021 (0.021 to 0.022) |
| Death | 36 | 0.455 (0.429 to 0.479) | 0.314 (0.286 to 0.338) | 0.141 (0.138 to 0.143) | 0.112 (0.103 to 0.125) | 0.115 (0.106 to 0.127) | -0.003 (-0.003 to -0.002) | 0.009 (0.009 to 0.009) | 0.048 (0.048 to 0.048) vs 0.057 (0.057 to 0.058) |
| Death | 60 | 0.390 (0.372 to 0.402) | 0.263 (0.244 to 0.275) | 0.128 (0.124 to 0.131) | 0.095 (0.082 to 0.102) | 0.091 (0.077 to 0.099) | 0.003 (0.003 to 0.004) | 0.015 (0.014 to 0.015) | 0.078 (0.078 to 0.078) vs 0.092 (0.092 to 0.093) |

Notes
1. Estimates are means across 5 paired validation replicates; 95% intervals are empirical 2.5th and 97.5th percentiles across replicates.
2. Model comparison is SHAP implemented (best_perf2) (reference) versus Full PH primary (best_perf1) (updated). Positive NRI and IDI values favor the updated model.
3. Continuous NRI is the sum of the event and nonevent components. Positive event NRI indicates upward movement in predicted risk among subjects with the event by the stated horizon; positive nonevent NRI indicates downward movement in predicted risk among subjects who remain event-free beyond that horizon.
4. Categorical NRI was computed using risk categories defined by: <5%; 5% to <10%; 10% to <20%; >=20%.
5. IDI is the difference in discrimination slopes between the updated and reference models; the reported central estimate is the mean across validation replicates and equals the updated-model mean slope minus the reference-model mean slope shown in the adjacent column.
6. The discrimination slope equals the weighted mean predicted risk among subjects with the event by time t minus the weighted mean predicted risk among subjects who are event-free beyond time t.
7. All measures were calculated at fixed follow-up horizons using censoring-adjusted weights and out-of-fold validation predictions.
