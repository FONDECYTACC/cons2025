Table. Horizon-specific net reclassification improvement and integrated discrimination improvement comparing updated and reference dual Cox models for first treatment readmission and all-cause mortality after discharge

| Outcome | Horizon, months | Continuous NRI | Continuous NRI, events | Continuous NRI, nonevents | Categorical NRI | Categorical NRI, events | Categorical NRI, nonevents | IDI | Discrimination slope, reference vs updated |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Readmission |  6 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.027 (0.027 to 0.027) vs 0.027 (0.027 to 0.027) |
| Readmission | 12 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.031 (0.031 to 0.031) vs 0.031 (0.031 to 0.031) |
| Readmission | 36 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.035 (0.035 to 0.035) vs 0.035 (0.035 to 0.035) |
| Readmission | 60 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.031 (0.031 to 0.031) vs 0.031 (0.031 to 0.031) |
| Death |  6 | 0.609 (0.575 to 0.635) | 0.093 (0.059 to 0.118) | 0.517 (0.514 to 0.519) | 0.162 (0.151 to 0.170) | 0.167 (0.155 to 0.175) | -0.005 (-0.005 to -0.004) | 0.024 (0.024 to 0.025) | 0.010 (0.010 to 0.010) vs 0.035 (0.034 to 0.035) |
| Death | 12 | 0.572 (0.545 to 0.609) | 0.203 (0.176 to 0.238) | 0.369 (0.363 to 0.373) | 0.120 (0.115 to 0.121) | 0.124 (0.119 to 0.125) | -0.004 (-0.004 to -0.004) | 0.016 (0.016 to 0.017) | 0.013 (0.013 to 0.013) vs 0.030 (0.030 to 0.030) |
| Death | 36 | 0.261 (0.252 to 0.270) | 0.090 (0.081 to 0.101) | 0.171 (0.169 to 0.174) | 0.050 (0.044 to 0.055) | 0.056 (0.050 to 0.060) | -0.006 (-0.006 to -0.005) | 0.009 (0.009 to 0.009) | 0.042 (0.042 to 0.042) vs 0.051 (0.051 to 0.051) |
| Death | 60 | 0.253 (0.246 to 0.265) | 0.120 (0.111 to 0.132) | 0.133 (0.130 to 0.136) | 0.059 (0.054 to 0.067) | 0.062 (0.057 to 0.070) | -0.003 (-0.004 to -0.003) | 0.010 (0.009 to 0.010) | 0.061 (0.060 to 0.061) vs 0.070 (0.070 to 0.070) |

Notes
1. Estimates are means across 5 paired validation replicates; 95% intervals are empirical 2.5th and 97.5th percentiles across replicates.
2. Model comparison is SHAP implemented (best_perf2) (reference) versus Full PH primary (best_perf1) (updated). Positive NRI and IDI values favor the updated model.
3. Continuous NRI is the sum of the event and nonevent components. Positive event NRI indicates upward movement in predicted risk among subjects with the event by the stated horizon; positive nonevent NRI indicates downward movement in predicted risk among subjects who remain event-free beyond that horizon.
4. Categorical NRI was computed using risk categories defined by: <5%; 5% to <10%; 10% to <20%; >=20%.
5. IDI is the difference in discrimination slopes between the updated and reference models; the reported central estimate is the mean across validation replicates and equals the updated-model mean slope minus the reference-model mean slope shown in the adjacent column.
6. The discrimination slope equals the weighted mean predicted risk among subjects with the event by time t minus the weighted mean predicted risk among subjects who are event-free beyond time t.
7. All measures were calculated at fixed follow-up horizons using censoring-adjusted weights and out-of-fold validation predictions.
