library(mice)



pre00_flag<- 
  SISTRAT23_c1_2010_2024_df_prev1h|>
  tidytable::mutate(is_truncated = rn %in% rows_truncated_treatments_due_to_retrieval_total)|>
  tidytable::group_by(hash_key)|>
  tidytable::mutate(max_adm_in_group = max(adm_date_rec, na.rm = TRUE))|>
  tidytable::ungroup()|>
  # Flags
  tidytable::mutate(flag_a = tidytable::case_when(
    is_truncated & !is.na(adm_date_rec) & adm_date_rec < max_adm_in_group ~
      "4.0.a.Subsequent treatment after index admission (same hash_key)",
    TRUE ~ NA_character_),
    flag_b = tidytable::case_when(
      dias_en_tratamiento > 1096 ~ "4.0.b.Ongoing treatment (>3 years)",
      TRUE ~ NA_character_))|>
  # Combine flags into a single FLAG column
  tidytable::mutate(FLAG = flag_a,
                    FLAG = ifelse(!is.na(flag_b) & !is.na(FLAG), paste0(FLAG, "; ", flag_b),
                                  ifelse(is.na(FLAG), flag_b, FLAG)))|>
  tidytable::select(-is_truncated, -max_adm_in_group, -flag_a, -flag_b)|> janitor::tabyl(FLAG)

pre00_flag[,c("FLAG","n")]|>
  knitr::kable("markdown", caption= "Flags")


# 0.a) Pick holdout rows (exclude your truncated set)
set.seed(2125)
holdout_rn <-
  SISTRAT23_c1_2010_2024_df_prev1h|>
  tidytable::filter(!is.na(dias_en_tratamiento))|>
  tidytable::anti_join(
    tidytable::tidytable(rn = rows_truncated_treatments_due_to_retrieval_total), by = "rn"
  )|>
  tidytable::slice_sample(n = 1000)|>
  tidytable::pull(rn)

# 0.b) Build mr_in and MASK the holdout on the log target
mr_in <-
  SISTRAT23_c1_2010_2024_df_prev1h|>
  tidytable::mutate(
    dias_for_imp0     = as.numeric(dias_en_tratamiento),
    dias_for_imp0     = ifelse(is.finite(dias_for_imp0) & dias_for_imp0 >= 0, dias_for_imp0, NA_real_),
    log_dias_for_imp  = ifelse(is.na(dias_for_imp0), NA_real_, log1p(dias_for_imp0)),
    adm_days          = as.integer(adm_date_rec - as.Date("1970-01-01")),
    TABLE_year        = as.integer(substr(as.character(TABLE_rec), 1, 4))
  )|>
  tidytable::mutate(
    is_holdout        = rn %in% holdout_rn,
    log_dias_for_imp  = ifelse(is_holdout, NA_real_, log_dias_for_imp)  # <- mask for missRanger
  )

mr_mice <- mr_in |>
  tidytable::arrange(hash_key, adm_date_rec) |>
  tidytable::group_by(hash_key) |>
  tidytable::mutate(
    next_adm    = tidytable::lead(adm_date_rec),
    gap_to_next = as.integer(next_adm - adm_date_rec) - 1L,
    gap_to_next = tidytable::case_when(
      is.na(gap_to_next) ~ 1096L,
      gap_to_next < 0L   ~ 0L,
      TRUE               ~ gap_to_next
    ),
    dias_max_allowed = pmin(gap_to_next, 1096L)
  ) |>
  tidytable::ungroup() |>
  tidytable::select(-next_adm) |>
  tidytable::mutate(def_days = as.integer(def_date - as.Date("1970-01-01")))

# 1) Mask the target IN mr_mice (the object you feed to mice)
mr_mice_eval <-
  mr_mice |>
  tidytable::mutate(
    is_holdout       = rn %in% holdout_rn,
    log_dias_for_imp = ifelse(is_holdout, NA_real_, log_dias_for_imp)
  )

# 2) Data frame and full method vector
# predictor matrix: include adm_days, dias_max_allowed, def_days as predictors of the target

# data for mice should be a plain data.frame
dat <- as.data.frame(mr_mice_eval)   # not mr_mice
meth <- mice::make.method(dat); meth[] <- ""; meth["log_dias_for_imp"] <- "pmm_constrained"
pred <- mice::make.predictorMatrix(dat); pred[,] <- 0
pred["log_dias_for_imp", c("adm_days","dias_max_allowed","def_days",
                           "plan_type","tipo_centro","adm_age_rec")] <- 1
pred[ , "log_dias_for_imp"] <- 0

mice.impute.pmm_constrained <- function(y, ry, x, wy = NULL, donors = 9, ...) {
  if (is.null(wy)) wy <- !ry
  # a) plain PMM on the LOG target
  yimp <- mice:::mice.impute.pmm(y = y, ry = ry, x = x, wy = wy, donors = donors, ...)
  
  # b) enforce constraints on DAYS, then back to log
  #    x must contain: adm_days, dias_max_allowed, def_days (numeric days since 1970-01-01)
  adm_days        <- x[wy, "adm_days"]
  dmax            <- x[wy, "dias_max_allowed"]
  def_days        <- x[wy, "def_days"]  # may be NA
  
  days            <- round(pmax(exp(yimp) - 1, 0))
  # cap by 1096 and gap_to_next
  days            <- pmin(days, 1096L, dmax)
  # cap by death date if available
  death_cap       <- def_days - adm_days
  days            <- ifelse(is.finite(death_cap), pmin(days, pmax(0, death_cap)), days)
  # back to log
  log_days        <- log1p(pmax(days, 0))
  return(log_days)
}

# 3) Run mice (custom imputer must be defined in the global env: mice.impute.pmm_constrained)
set.seed(2125)
imp_eval <- mice::mice(
  dat,
  m = 9,
  method = meth,
  predictorMatrix = pred,
  printFlag = FALSE
)
mice_out <- mice::complete(imp_eval)
mice_long <- mice::complete(imp_eval, action = "long")
# compute metrics per .imp, then average


N <- length(holdout_rn)

post <- mice_long |>
  tidytable::mutate(
    dias_max_allowed = dplyr::coalesce(as.integer(dias_max_allowed), 1096L),
    dias_pred_raw = round(pmax(exp(log_dias_for_imp) - 1, 0)),
    dias_capped   = pmin(dias_pred_raw, 1096L, dias_max_allowed),
    disch_raw     = as.Date(adm_days, origin = "1970-01-01") + dias_capped,
    death_trim    = !is.na(def_date) & disch_raw > def_date,
    disch_date_imp = tidytable::case_when(death_trim ~ def_date, TRUE ~ disch_raw),
    dias_final    = as.integer(disch_date_imp - as.Date(adm_days, origin = "1970-01-01"))
  )

truth <- SISTRAT23_c1_2010_2024_df_prev1h |> tidytable::select(rn, truth = dias_en_tratamiento)


# C) Evaluate on holdout with na.rm=TRUE (per imputation, then average)

eval_by_imp_mice <-
  post |>
  tidytable::inner_join(truth, by = "rn") |>
  tidytable::filter(rn %in% holdout_rn) |>
  tidytable::mutate(err = dias_final - truth, ae = abs(err)) |>
  tidytable::summarise(
    MAE_days     = mean(ae, na.rm = TRUE),
    MedianAE     = stats::median(ae, na.rm = TRUE),
    RMSE_days    = sqrt(mean(err^2, na.rm = TRUE)),
    p_within_7d  = mean(ae <= 7,  na.rm = TRUE),
    p_within_30d = mean(ae <= 30, na.rm = TRUE),
    p_within_90d = mean(ae <= 90, na.rm = TRUE),
    .by = .imp
  )

eval_summary_mice <-
  eval_by_imp_mice |>
  tidytable::summarise(
    n = N,
    dplyr::across(where(is.numeric), mean),
    .groups = "drop"
  )

eval_summary_mice
#       n  .imp MAE_days MedianAE RMSE_days p_within_7d p_within_30d p_within_90d
#   <int> <dbl>    <dbl>    <dbl>     <dbl>       <dbl>        <dbl>        <dbl>
# 1  1000     5     225.     151.      328.       0.131        0.212        0.346