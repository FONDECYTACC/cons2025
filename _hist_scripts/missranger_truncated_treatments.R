# use an pmm.k which not exceed the number of observed donors
pmm_k <- min(9L, sum(!is.na(mr_in$log_dias_for_imp)))


set.seed(2125)
mr_fit <- missRanger::missRanger(
  data      = mr_mice_eval|>
    tidytable::select(
      rn, log_dias_for_imp, adm_days, TABLE_year,
      plan_type, tipo_centro, id_centro, senda, adm_age_rec,
      numero_de_tratamientos_anteriores, sub_dep_icd10_status,
      adm_motive, macrozone_center, sexo, yr_block, 
    ),
  formula   = log_dias_for_imp ~ . -rn -hash_key -dias_for_imp,
  pmm.k     = pmm_k,
  num.trees = 2e2,#3e3
  keep_forests = F,#too heavy
  maxiter   = 5,
  mtry      = function(p) max(5L, floor(p/3L)),
  seed      = 2125,
  returnOOB = TRUE,
  verbose   = 1
)


# --- OOB variance explained (on LOG scale) ---
oob_err <- attr(mr_fit, "oob")                 # named vector
r2_oob  <- 1 - unname(oob_err[["log_dias_for_imp"]])
cat(sprintf("OOB R^2 (log_dias_for_imp): %.1f%%\n", 100 * r2_oob))
message(
  paste0("The model explains ",
         scales::percent(r2_oob, accuracy = .1),
         " of the variance (OOB, log scale).")
)
#The model explains 51.8% of the variance (OOB, log scale).
#The model explains 50.8% of the variance (OOB, log scale).

# --- Back-transform + optional clipping with lambda wrapper ---
# 2.1) Constraints: dias_max_allowed = min(next_adm - adm_date_rec - 1, 1096), ≥0
constraints <-
  SISTRAT23_c1_2010_2024_df_prev1h|>
  tidytable::arrange(hash_key, adm_date_rec, rn)|>
  tidytable::group_by(hash_key)|>
  tidytable::mutate(next_adm = tidytable::lead(adm_date_rec))|>
  tidytable::ungroup()|>
  tidytable::mutate(
    gap_to_next = as.integer(next_adm - adm_date_rec) - 1L,
    dias_max_allowed = tidytable::case_when(
      is.na(gap_to_next) ~ 1096L,
      gap_to_next < 0L   ~ 0L,
      TRUE               ~ pmin(gap_to_next, 1096L)
    )
  )|>
  tidytable::select(rn, dias_max_allowed)

# 2.2) Predicciones + clipping
pred_df <-
  mr_fit|>
  tidytable::left_join(constraints, by = "rn")|>
  tidytable::mutate(dias_pred_raw = round(pmax(exp(log_dias_for_imp) - 1, 0)))|>
  tidytable::transmute(
    rn,
    pred = round(pmax(pmin(dias_pred_raw, dias_max_allowed), 0))
  )
# --- Holdout evaluation (days) ---
eval_tbl <-
  pred_df|>
  tidytable::inner_join(
    SISTRAT23_c1_2010_2024_df_prev1h|>
      tidytable::select(rn, truth = dias_en_tratamiento),
    by = "rn"
  )|>
  tidytable::filter(rn %in% holdout_rn)|>
  tidytable::mutate(err = pred - truth, ae = abs(err))

eval_summary <-
  eval_tbl|>
  tidytable::summarise(
    n            = tidytable::n(),
    MAE_days     = mean(ae),
    MedianAE     = stats::median(ae),
    RMSE_days    = sqrt(mean(err^2)),
    p_within_7d  = mean(ae <= 7),
    p_within_30d = mean(ae <= 30),
    p_within_90d = mean(ae <= 90),
  )
print(eval_summary)
#       n MAE_days MedianAE RMSE_days p_within_7d p_within_30d
#   <int>    <dbl>    <dbl>     <dbl>       <dbl>        <dbl>
# 1  1000     103.       77      154.       0.093        0.264
#       n MAE_days MedianAE RMSE_days p_within_7d p_within_30d p_within_90d
#   <int>    <dbl>    <dbl>     <dbl>       <dbl>        <dbl>        <dbl>
# 1  1000     105.       76      160.       0.097        0.272        0.567
