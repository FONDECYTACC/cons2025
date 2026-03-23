df0 <- readRDS(paste0(dirname(path),"/pred1/df0.rds"))
# =========================================================
# AUDIT OF ELIGIBILITY CRITERIA, SEPARATELY
# =========================================================

# Rebuild df1 with explicit component flags
df1 <- df0 |>
  tidytable::mutate(
    date_missing = is.na(adm_date_rec2),
    date_before  = !is.na(adm_date_rec2) & adm_date_rec2 <  .start_date,
    date_ok      = !is.na(adm_date_rec2) & adm_date_rec2 >= .start_date & adm_date_rec2 <= .end_date,
    date_after   = !is.na(adm_date_rec2) & adm_date_rec2 >  .end_date,
    
    age_missing  = is.na(adm_age_rec3),
    age_lt18     = !is.na(adm_age_rec3) & adm_age_rec3 < 18,
    age_ok       = !is.na(adm_age_rec3) & adm_age_rec3 >= 18 & adm_age_rec3 <= 64,
    age_gt64     = !is.na(adm_age_rec3) & adm_age_rec3 > 64,
    
    meets_crit   = date_ok & age_ok
  )

# ---------------------------------------------------------
# A) FIRST OBSERVED TREATMENT PER PATIENT
# ---------------------------------------------------------
first_ep <- df1 |>
  tidytable::filter(treatment == 1L) |>
  tidytable::select(
    hash_key,
    first_adm = adm_date_rec2,
    first_age = adm_age_rec3,
    first_date_missing = date_missing,
    first_date_before  = date_before,
    first_date_ok      = date_ok,
    first_date_after   = date_after,
    first_age_missing  = age_missing,
    first_age_lt18     = age_lt18,
    first_age_ok       = age_ok,
    first_age_gt64     = age_gt64
  )

# Counts based ONLY on the first observed treatment
first_episode_summary <- data.frame(
  metric = c(
    "Patients total",
    "First treatment date missing",
    "First treatment before 2010",
    "First treatment in 2010-2020",
    "First treatment after 2020",
    "First treatment age missing",
    "First treatment age <18",
    "First treatment age 18-64",
    "First treatment age >64"
  ),
  n = c(
    nrow(first_ep),
    sum(first_ep$first_date_missing, na.rm = TRUE),
    sum(first_ep$first_date_before,  na.rm = TRUE),
    sum(first_ep$first_date_ok,      na.rm = TRUE),
    sum(first_ep$first_date_after,   na.rm = TRUE),
    sum(first_ep$first_age_missing,  na.rm = TRUE),
    sum(first_ep$first_age_lt18,     na.rm = TRUE),
    sum(first_ep$first_age_ok,       na.rm = TRUE),
    sum(first_ep$first_age_gt64,     na.rm = TRUE)
  )
)

first_episode_summary

#                         metric      n
# 1               Patients total 121299
# 2 First treatment date missing      0
# 3  First treatment before 2010   3140
# 4 First treatment in 2010-2020  89019
# 5   First treatment after 2020  29140
# 6  First treatment age missing      0
# 7      First treatment age <18    158
# 8    First treatment age 18-64 119026
# 9      First treatment age >64   2115


# ---------------------------------------------------------
# B) PATIENT-LEVEL ELIGIBILITY AUDIT
#    (across ALL episodes for each patient)
# ---------------------------------------------------------
patient_audit <- df1 |>
  tidytable::summarise(
    any_qualifying = any(meets_crit),
    
    any_date_ok = any(date_ok),
    any_age_ok  = any(age_ok),
    
    all_dates_missing = all(date_missing),
    all_dates_after_2020 = any(!date_missing) & all(date_after[!date_missing]),
    all_dates_before_2010 = any(!date_missing) & all(date_before[!date_missing]),
    
    any_inwindow_age_lt18    = any(date_ok & age_lt18),
    any_inwindow_age_ok      = any(date_ok & age_ok),
    any_inwindow_age_gt64    = any(date_ok & age_gt64),
    any_inwindow_age_missing = any(date_ok & age_missing),
    
    all_inwindow_ages_missing =
      any(date_ok) & all(age_missing[date_ok]),
    
    all_inwindow_ages_lt18 =
      any(date_ok & !age_missing) &
      all(adm_age_rec3[date_ok & !age_missing] < 18),
    
    all_inwindow_ages_gt64 =
      any(date_ok & !age_missing) &
      all(adm_age_rec3[date_ok & !age_missing] > 64),
    
    .by = hash_key
  ) |>
  tidytable::left_join(first_ep, by = "hash_key") |>
  tidytable::mutate(
    excl_reason = dplyr::case_when(
      any_qualifying ~ "Has >=1 qualifying episode",
      all_dates_missing ~ "All admission dates missing",
      all_dates_after_2020 ~ "All admissions after 2020",
      all_dates_before_2010 ~ "All admissions before 2010",
      !any_date_ok ~ "No admission in 2010-2020",
      all_inwindow_ages_missing ~ "In-window admissions, but age missing in all",
      all_inwindow_ages_lt18 ~ "In-window admissions, but all ages <18",
      all_inwindow_ages_gt64 ~ "In-window admissions, but all ages >64",
      !any_inwindow_age_ok ~ "No in-window episode with age 18-64",
      TRUE ~ "Mixed reasons / no episode satisfies both simultaneously"
    )
  )

# Patients WITH and WITHOUT any qualifying episode
n_with_qualifying <- sum(patient_audit$any_qualifying, na.rm = TRUE)
n_without_qualifying <- sum(!patient_audit$any_qualifying, na.rm = TRUE)

message("Patients with >=1 qualifying episode: ", formatC(n_with_qualifying, big.mark=","), sep = "")
#Patients with >=1 qualifying episode: 88,632

message("Patients with NO qualifying episode: ", formatC(n_without_qualifying, big.mark=","), sep = "")
#Patients with NO qualifying episode: 32,667

# Mutually exclusive breakdown among patients WITHOUT any qualifying episode
excluded_breakdown <- patient_audit |>
  tidytable::filter(!any_qualifying) |>
  tidytable::count(excl_reason, sort = TRUE)

excluded_breakdown
# # A tidytable: 5 × 2
# excl_reason                                n
# <chr>                                  <int>
#   1 All admissions after 2020              29140
# 2 All admissions before 2010              1945
# 3 In-window admissions, but all ages >64  1391
# 4 In-window admissions, but all ages <18   112
# 5 No admission in 2010-2020                 79

# ---------------------------------------------------------
# C) NON-MUTUALLY-EXCLUSIVE CHECKS AMONG EXCLUDED PATIENTS
# ---------------------------------------------------------
excluded_flags_summary <- data.frame(
  metric = c(
    "Excluded patients total",
    "First observed treatment after 2020",
    "First observed treatment before 2010",
    "First observed age <18",
    "First observed age >64",
    "All admissions after 2020",
    "All admissions before 2010",
    "At least one admission in 2010-2020 but all those ages <18",
    "At least one admission in 2010-2020 but all those ages >64",
    "At least one admission in 2010-2020 but age missing in all those episodes"
  ),
  n = c(
    sum(!patient_audit$any_qualifying, na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$first_date_after,  na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$first_date_before, na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$first_age_lt18,    na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$first_age_gt64,    na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$all_dates_after_2020, na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$all_dates_before_2010, na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$all_inwindow_ages_lt18, na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$all_inwindow_ages_gt64, na.rm = TRUE),
    sum(!patient_audit$any_qualifying & patient_audit$all_inwindow_ages_missing, na.rm = TRUE)
  )
)

excluded_flags_summary

#                                                                       metric     n
# 1                                                    Excluded patients total 32667
# 2                                        First observed treatment after 2020 29140
# 3                                       First observed treatment before 2010  2032
# 4                                                     First observed age <18   138
# 5                                                     First observed age >64  2115
# 6                                                  All admissions after 2020 29140
# 7                                                 All admissions before 2010  1945
# 8                 At least one admission in 2010-2020 but all those ages <18   112
# 9                 At least one admission in 2010-2020 but all those ages >64  1391
# 10 At least one admission in 2010-2020 but age missing in all those episodes     0