Table. Horizon-specific net reclassification improvement and integrated discrimination improvement comparing updated and reference dual Cox models for first treatment readmission and all-cause mortality after discharge

| Outcome | Horizon, months | Continuous NRI | Continuous NRI, events | Continuous NRI, nonevents | Categorical NRI | Categorical NRI, events | Categorical NRI, nonevents | IDI | Discrimination slope, reference vs updated |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Readmission |  6 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.027 (0.027 to 0.027) vs 0.027 (0.027 to 0.027) |
| Readmission | 12 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.031 (0.031 to 0.031) vs 0.031 (0.031 to 0.031) |
| Readmission | 36 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.035 (0.035 to 0.035) vs 0.035 (0.035 to 0.035) |
| Readmission | 60 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.031 (0.031 to 0.031) vs 0.031 (0.031 to 0.031) |
| Death |  6 | 0.597 (0.553 to 0.630) | 0.084 (0.040 to 0.117) | 0.513 (0.509 to 0.516) | 0.164 (0.160 to 0.170) | 0.169 (0.164 to 0.175) | -0.005 (-0.005 to -0.005) | 0.024 (0.024 to 0.025) | 0.010 (0.010 to 0.010) vs 0.034 (0.034 to 0.035) |
| Death | 12 | 0.556 (0.520 to 0.581) | 0.189 (0.151 to 0.212) | 0.367 (0.363 to 0.370) | 0.120 (0.115 to 0.121) | 0.124 (0.119 to 0.125) | -0.004 (-0.004 to -0.004) | 0.016 (0.016 to 0.016) | 0.013 (0.013 to 0.013) vs 0.030 (0.029 to 0.030) |
| Death | 36 | 0.253 (0.237 to 0.269) | 0.085 (0.069 to 0.103) | 0.168 (0.166 to 0.172) | 0.049 (0.047 to 0.052) | 0.055 (0.052 to 0.058) | -0.006 (-0.006 to -0.005) | 0.009 (0.008 to 0.009) | 0.042 (0.042 to 0.042) vs 0.051 (0.051 to 0.051) |
| Death | 60 | 0.244 (0.229 to 0.260) | 0.111 (0.095 to 0.127) | 0.133 (0.132 to 0.135) | 0.057 (0.051 to 0.062) | 0.059 (0.053 to 0.064) | -0.002 (-0.005 to -0.001) | 0.010 (0.009 to 0.010) | 0.061 (0.060 to 0.061) vs 0.070 (0.070 to 0.070) |

Notes
1. Estimates are means across 5 paired validation replicates; 95% intervals are empirical 2.5th and 97.5th percentiles across replicates.
2. Model comparison is SHAP implemented (best_perf2) (reference) versus Full PH primary (best_perf1) (updated). Positive NRI and IDI values favor the updated model.
3. Continuous NRI is the sum of the event and nonevent components. Positive event NRI indicates upward movement in predicted risk among subjects with the event by the stated horizon; positive nonevent NRI indicates downward movement in predicted risk among subjects who remain event-free beyond that horizon.
4. Categorical NRI was computed using risk categories defined by: <5%; 5% to <10%; 10% to <20%; >=20%.
5. IDI is the difference in discrimination slopes between the updated and reference models; the reported central estimate is the mean across validation replicates and equals the updated-model mean slope minus the reference-model mean slope shown in the adjacent column.
6. The discrimination slope equals the weighted mean predicted risk among subjects with the event by time t minus the weighted mean predicted risk among subjects who are event-free beyond time t.
7. All measures were calculated at fixed follow-up horizons using censoring-adjusted weights and out-of-fold validation predictions.
