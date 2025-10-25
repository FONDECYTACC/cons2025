# Create a constrained RF imputation function using mice's built-in RF
mice.impute.rf_constrained <- function(y, ry, x, wy = NULL, ntree = 3e3, ...) {
  if (is.null(wy)) wy <- !ry
  
  # Use mice's built-in random forest imputation
  yimp <- mice.impute.rf(y = y, ry = ry, x = x, wy = wy, ...)
  
  # Apply constraints (same as your PMM function)
  adm_days <- x[wy, "adm_days"]
  dmax <- x[wy, "dias_max_allowed"]
  def_days <- x[wy, "def_days"]  # may be NA
  
  days <- round(pmax(exp(yimp) - 1, 0))
  # cap by 1096 and gap_to_next
  days <- pmin(days, 1096L, dmax)
  # cap by death date if available
  death_cap <- def_days - adm_days
  days <- ifelse(is.finite(death_cap), pmin(days, pmax(0, death_cap)), days)
  # back to log
  log_days <- log1p(pmax(days, 0))
  return(log_days)
}

# Update the method to use constrained RF
meth["log_dias_for_imp"] <- "rf_constrained"

# Run mice with constrained RF
set.seed(2125)
imp_eval_rf <- mice::mice(
  dat,
  m = 9,
  ntree= 3e3,
  method = meth,
  predictorMatrix = pred,
  printFlag = FALSE, 
  parallel=T
)

# Process the results
mice_rf_long <- mice::complete(imp_eval_rf, action = "long")

# Apply any additional constraints if needed
post_mice_rf <- mice_rf_long |>
  tidytable::mutate(
    dias_pred_raw = round(exp(log_dias_for_imp) - 1),
    dias_pred_raw = pmax(dias_pred_raw, 0L),
    dias_capped = pmin(dias_pred_raw, as.integer(dias_max_allowed)),
    disch_raw = as.Date(adm_days, origin = "1970-01-01") + dias_capped,
    death_trim = !is.na(def_date) & (disch_raw > def_date),
    disch_date_imp = tidytable::case_when(
      death_trim ~ def_date,
      TRUE ~ disch_raw
    ),
    dias_final = as.integer(disch_date_imp - as.Date(adm_days, origin = "1970-01-01"))
  )

# Check the distribution
hist(post_mice_rf$dias_final, breaks=60, main="Histogram of RF-constrained imputed days")
summary(post_mice_rf$dias_final)


# Evaluate RF performance
eval_by_imp_rf <- post_mice_rf |>
  tidytable::inner_join(truth, by = "rn") |>
  tidytable::filter(rn %in% holdout_rn) |>
  tidytable::mutate(err = dias_final - truth, ae = abs(err)) |>
  tidytable::summarise(
    MAE_days = mean(ae, na.rm = TRUE),
    MedianAE = stats::median(ae, na.rm = TRUE),
    RMSE_days = sqrt(mean(err^2, na.rm = TRUE)),
    p_within_7d = mean(ae <= 7, na.rm = TRUE),
    p_within_30d = mean(ae <= 30, na.rm = TRUE),
    p_within_90d = mean(ae <= 90, na.rm = TRUE),
    .by = .imp
  )

eval_summary_rf <- eval_by_imp_rf |>
  tidytable::summarise(
    n = N,
    dplyr::across(where(is.numeric), mean),
    .groups = "drop"
  )

# Compare with your previous results
cat("Previous PMM results:\n")
print(eval_summary_mice)
cat("\nConstrained RF results:\n")
print(eval_summary_rf)

#A tidytable: 1 × 8
# n  .imp MAE_days MedianAE RMSE_days p_within_7d p_within_30d p_within_90d
# <int> <dbl>    <dbl>    <dbl>     <dbl>       <dbl>        <dbl>        <dbl>
#   1  1000     5     174.     108.      268.       0.108        0.225        0.451
post_mice_rf|>
  tidytable::filter(rn %in% rows_problematic_adm_truncated_treatments)|>
  tidytable::group_by(rn) |>
  tidytable::summarise(
    dias_pred_raw = mean(dias_pred_raw, na.rm = TRUE),
    dias_capped = mean(dias_capped, na.rm = TRUE),
    dias_final = mean(dias_final, na.rm = TRUE),
    # For dates, we might want to take the mode or median across imputations
    disch_date_imp = as.Date(round(mean(as.numeric(disch_date_imp), na.rm = TRUE)), 
                             origin = "1970-01-01"),
    # For categorical, take the most frequent value
    tr_compliance_imp = "adm truncated"
  ) |>
  tidytable::ungroup() |>  pull(dias_final) |> summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0    1096    1096    1061    1096    1096       8 


#no better