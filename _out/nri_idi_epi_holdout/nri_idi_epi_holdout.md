Table. Horizon-specific net reclassification improvement and integrated discrimination improvement comparing updated and reference dual Cox models for first treatment readmission and all-cause mortality after discharge

| Outcome | Horizon, months | Continuous NRI | Continuous NRI, events | Continuous NRI, nonevents | Categorical NRI | Categorical NRI, events | Categorical NRI, nonevents | IDI | Discrimination slope, reference vs updated |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Readmission |  6 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.026 (0.026 to 0.026) vs 0.026 (0.026 to 0.026) |
| Readmission | 12 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.029 (0.029 to 0.029) vs 0.029 (0.029 to 0.029) |
| Readmission | 36 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.032 (0.032 to 0.032) vs 0.032 (0.032 to 0.032) |
| Readmission | 60 | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.000 (0.000 to 0.000) | 0.029 (0.029 to 0.029) vs 0.029 (0.029 to 0.029) |
| Death |  6 | 0.718 (0.647 to 0.769) | 0.201 (0.131 to 0.255) | 0.517 (0.514 to 0.519) | 0.158 (0.153 to 0.161) | 0.162 (0.158 to 0.166) | -0.005 (-0.005 to -0.004) | 0.025 (0.024 to 0.025) | 0.011 (0.011 to 0.011) vs 0.036 (0.036 to 0.036) |
| Death | 12 | 0.734 (0.673 to 0.771) | 0.365 (0.304 to 0.400) | 0.369 (0.363 to 0.373) | 0.141 (0.139 to 0.142) | 0.145 (0.142 to 0.146) | -0.004 (-0.004 to -0.004) | 0.015 (0.015 to 0.015) | 0.017 (0.017 to 0.017) vs 0.032 (0.032 to 0.032) |
| Death | 36 | 0.434 (0.406 to 0.461) | 0.263 (0.236 to 0.288) | 0.171 (0.169 to 0.174) | 0.078 (0.070 to 0.087) | 0.084 (0.076 to 0.092) | -0.006 (-0.006 to -0.005) | 0.011 (0.011 to 0.011) | 0.049 (0.049 to 0.049) vs 0.060 (0.060 to 0.061) |
| Death | 60 | 0.395 (0.385 to 0.401) | 0.261 (0.254 to 0.270) | 0.133 (0.130 to 0.136) | 0.093 (0.089 to 0.102) | 0.096 (0.092 to 0.104) | -0.003 (-0.004 to -0.003) | 0.014 (0.014 to 0.015) | 0.078 (0.078 to 0.078) vs 0.093 (0.092 to 0.093) |

Notes
1. Estimates are means across 5 paired validation replicates; 95% intervals are empirical 2.5th and 97.5th percentiles across replicates.
2. Model comparison is SHAP implemented (best_perf2) (reference) versus Full PH primary (best_perf1) (updated). Positive NRI and IDI values favor the updated model.
3. Continuous NRI is the sum of the event and nonevent components. Positive event NRI indicates upward movement in predicted risk among subjects with the event by the stated horizon; positive nonevent NRI indicates downward movement in predicted risk among subjects who remain event-free beyond that horizon.
4. Categorical NRI was computed using risk categories defined by: <5%; 5% to <10%; 10% to <20%; >=20%.
5. IDI is the difference in discrimination slopes between the updated and reference models; the reported central estimate is the mean across validation replicates and equals the updated-model mean slope minus the reference-model mean slope shown in the adjacent column.
6. The discrimination slope equals the weighted mean predicted risk among subjects with the event by time t minus the weighted mean predicted risk among subjects who are event-free beyond time t.
7. All measures were calculated at fixed follow-up horizons using censoring-adjusted weights and out-of-fold validation predictions.
