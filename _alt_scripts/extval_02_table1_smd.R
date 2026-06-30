# =============================================================================
# extval_02_table1_smd.R
# Development-vs-external covariate comparison (Table 1) with standardized mean
# differences (SMD). The DEVELOPMENT column is taken verbatim from the project's
# training-set Table 1 (the values pasted by the user; development training set
# n = 70,521). The EXTERNAL column (2021-2024 cohort) is computed from the cohort
# built by extval_01_build_cohort.R. SMDs are computed from the two sets of
# proportions/means, so no development microdata is loaded.
#
# SMD (two-group):
#   - categorical level / binary: |p_dev - p_ext| / sqrt((p_dev(1-p_dev) + p_ext(1-p_ext))/2)
#   - continuous:                  |m_dev - m_ext| / sqrt((sd_dev^2 + sd_ext^2)/2)
# Proportions are computed among non-missing for each variable (matching how the
# development Table 1 percentages were reported).
#
# Public API:
#   extval_table1_smd(nondum)        -> data.frame (Variable, Level, Development,
#                                       External, SMD)
#   extval_smd_browsable(tab, ...)   -> htmltools::browsable() grouped table
# =============================================================================

suppressWarnings(stopifnot(requireNamespace("htmltools", quietly = TRUE)))

# ---- Development reference (training set, n = 70,521), proportions as fractions
# Keyed by the EXTERNAL category label so external and development align exactly.
.EXTVAL_DEV_N <- 70521L
.smd_bin <- function(p1, p2) {
  d <- (p1 * (1 - p1) + p2 * (1 - p2)) / 2
  if (!is.finite(d) || d <= 0) return(0)
  abs(p1 - p2) / sqrt(d)
}
.smd_cont <- function(m1, s1, m2, s2) {
  d <- (s1^2 + s2^2) / 2
  if (!is.finite(d) || d <= 0) return(NA_real_)
  abs(m1 - m2) / sqrt(d)
}

# Each block: display name, external variable, type, and per-level development
# proportions keyed by the external level string (cat); or dev mean/sd (cont);
# or the "positive" external value + dev proportion (binary).
.EXTVAL_T1_BLOCKS <- list(
  list(disp = "Admission motive", var = "adm_motive", type = "cat", dev = c(
    "spontaneous consultation" = .449, "sanitary sector" = .311, "justice sector" = .095,
    "another SUD facility/FONODROGAS/SENDA Previene" = .093, "other" = .052),
    labels = c("spontaneous consultation" = "Spontaneous consultation",
               "sanitary sector" = "Health system", "justice sector" = "Justice system",
               "another SUD facility/FONODROGAS/SENDA Previene" = "SENDA network / other center",
               "other" = "Other")),
  list(disp = "Cohabitation status", var = "cohabitation", type = "cat", dev = c(
    "family of origin" = .370, "with couple/children" = .449, "alone" = .095, "Others" = .087),
    labels = c("family of origin" = "Family of origin",
               "with couple/children" = "Only with partner and/or children",
               "alone" = "Alone", "Others" = "Others")),
  list(disp = "Educational attainment", var = "ed_attainment_corr", type = "cat", dev = c(
    "3-Completed primary school or less" = .262, "2-Completed high school or less" = .554,
    "1-More than high school" = .184),
    labels = c("3-Completed primary school or less" = "Primary or less",
               "2-Completed high school or less" = "High school or less",
               "1-More than high school" = "More than high school")),
  list(disp = "Discharge evaluation: Substance use", var = "eva_consumo", type = "cat", dev = c(
    "logro alto" = .294, "logro intermedio" = .287, "logro minimo" = .419), labels = .EVA_LAB <- c(
    "logro alto" = "High achievement", "logro intermedio" = "Intermediate achievement",
    "logro minimo" = "Minimal achievement")),
  list(disp = "Discharge evaluation: Family", var = "eva_fam", type = "cat", dev = c(
    "logro alto" = .237, "logro intermedio" = .320, "logro minimo" = .442), labels = .EVA_LAB),
  list(disp = "Discharge evaluation: Physical", var = "eva_fisica", type = "cat", dev = c(
    "logro alto" = .304, "logro intermedio" = .312, "logro minimo" = .384), labels = .EVA_LAB),
  list(disp = "Discharge evaluation: Occupational", var = "eva_ocupacion", type = "cat", dev = c(
    "logro alto" = .316, "logro intermedio" = .267, "logro minimo" = .417), labels = .EVA_LAB),
  list(disp = "Discharge evaluation: Interpersonal", var = "eva_relinterp", type = "cat", dev = c(
    "logro alto" = .244, "logro intermedio" = .319, "logro minimo" = .437), labels = .EVA_LAB),
  list(disp = "Discharge evaluation: Mental health", var = "eva_sm", type = "cat", dev = c(
    "logro alto" = .243, "logro intermedio" = .330, "logro minimo" = .427), labels = .EVA_LAB),
  list(disp = "Discharge evaluation: Norm transgression", var = "eva_transgnorma", type = "cat", dev = c(
    "logro alto" = .358, "logro intermedio" = .248, "logro minimo" = .394), labels = .EVA_LAB),
  list(disp = "Discharge evaluation: Therapeutic process", var = "evaluacindelprocesoteraputico", type = "cat", dev = c(
    "logro alto" = .240, "logro intermedio" = .327, "logro minimo" = .433), labels = .EVA_LAB),
  list(disp = "Substance of onset", var = "first_sub_used", type = "cat",
    recode = function(x) {
      y <- as.character(x)
      dplyr::case_when(y == "alcohol" ~ "alcohol", y == "marijuana" ~ "marijuana",
        y == "cocaine paste" ~ "cocaine paste", y == "cocaine powder" ~ "cocaine powder",
        is.na(y) ~ NA_character_, TRUE ~ "others")},
    dev = c("alcohol" = .589, "marijuana" = .299, "cocaine paste" = .048,
            "cocaine powder" = .037, "others" = .027),
    labels = c("alcohol" = "Alcohol", "marijuana" = "Marijuana", "cocaine paste" = "Cocaine paste",
               "cocaine powder" = "Cocaine hydrochloride", "others" = "Others")),
  list(disp = "Marital status", var = "marital_status_rec", type = "cat", dev = c(
    "single" = .548, "married/cohabiting" = .336, "separated/divorced/annulled/widowed" = .116),
    labels = c("single" = "Single", "married/cohabiting" = "Married/cohabiting/civil union",
               "separated/divorced/annulled/widowed" = "Separated/divorced/widowed")),
  list(disp = "Occupational status", var = "occupation_condition_corr24", type = "cat", dev = c(
    "employed" = .490, "unemployed" = .347, "inactive" = .163),
    labels = c("employed" = "Employed", "unemployed" = "Unemployed", "inactive" = "Inactive")),
  list(disp = "Treatment modality", var = "plan_type_corr", type = "cat", dev = c(
    "pg-pab" = .366, "pg-pai" = .425, "pg-pr" = .110, "m-pai" = .060, "m-pr" = .040),
    labels = c("pg-pab" = "Outpatient basic, general", "pg-pai" = "Outpatient intensive, general",
               "pg-pr" = "Residential, general", "m-pai" = "Outpatient intensive, women",
               "m-pr" = "Residential, women")),
  list(disp = "Frequency of use (primary substance)", var = "prim_sub_freq_rec", type = "cat", dev = c(
    "3.Daily" = .441, "2.2–6 days/wk" = .443, "1.≤1 day/wk" = .115),
    labels = c("3.Daily" = "Daily", "2.2–6 days/wk" = "2 to 6 days a week",
               "1.≤1 day/wk" = "1 day or less a week")),
  list(disp = "Primary substance at admission", var = "primary_sub_mod", type = "cat", dev = c(
    "cocaine paste" = .378, "alcohol" = .340, "cocaine powder" = .196, "marijuana" = .068,
    "others" = .018),
    labels = c("cocaine paste" = "Cocaine paste", "alcohol" = "Alcohol",
               "cocaine powder" = "Cocaine hydrochloride", "marijuana" = "Marijuana",
               "others" = "Others")),
  list(disp = "Housing situation", var = "tenure_status_household", type = "cat", dev = c(
    "stays temporarily with a relative" = .411, "owner/transferred dwellings/pays dividends" = .365,
    "renting" = .179, "illegal settlement" = .015, "others" = .029),
    labels = c("stays temporarily with a relative" = "Lives with relatives",
               "owner/transferred dwellings/pays dividends" = "Owned/transferred/paying mortgage",
               "renting" = "Renting", "illegal settlement" = "Irregular occupation", "others" = "Others")),
  list(disp = "Treatment outcome", var = "tr_outcome", type = "cat", dev = c(
    "dropout" = .533, "completion" = .265, "referral" = .115,
    "adm discharge - rule violation/undet" = .074, "adm discharge - adm reasons" = .013),
    labels = c("dropout" = "Dropout (non-completion)", "completion" = "Completion",
               "referral" = "Referral", "adm discharge - rule violation/undet" = "Administrative discharge, rule violation",
               "adm discharge - adm reasons" = "Administrative discharge, administrative reasons")),
  list(disp = "Urbanicity", var = "urbanicity_cat", type = "cat", dev = c(
    "3.Urban" = .815, "2.Mixed" = .098, "1.Rural" = .087),
    labels = c("3.Urban" = "Urban", "2.Mixed" = "Mixed", "1.Rural" = "Rural")),
  # Continuous
  list(disp = "Age at admission, mean (SD)", var = "adm_age_rec3", type = "cont", dev_mean = 35.7, dev_sd = 10.5),
  list(disp = "Time in treatment, months, mean (SD)", var = "dit_m", type = "cont", dev_mean = 7.4, dev_sd = 6.2),
  list(disp = "Communal poverty index, mean (SD)", var = "porc_pobr", type = "cont", dev_mean = 0.1, dev_sd = 0.1),
  # Binary (proportion with the indicated condition)
  list(disp = "Physical comorbidity (yes)", var = "any_phys_dx", type = "bin", pos = function(x) x %in% TRUE, dev = .095),
  list(disp = "Domestic violence/sexual abuse (yes)", var = "any_violence", type = "bin",
       pos = function(x) x == "1.Domestic violence/sex abuse", dev = .282),
  list(disp = "Psychiatric comorbidity, ICD-10 (yes)", var = "dg_psiq_cie_10_dg", type = "bin", pos = function(x) x %in% TRUE, dev = .448),
  list(disp = "Psychiatric comorbidity, in study (yes)", var = "dg_psiq_cie_10_instudy", type = "bin", pos = function(x) x %in% TRUE, dev = .173),
  list(disp = "Mood disorders (yes)", var = "dx_f3_mood", type = "bin", pos = function(x) x %in% c(1L, TRUE), dev = .102),
  list(disp = "Personality disorders (yes)", var = "dx_f6_personality", type = "bin", pos = function(x) x %in% c(1L, TRUE), dev = .285),
  list(disp = "Severe mental illness (yes)", var = "dx_f_any_severe_mental", type = "bin", pos = function(x) x %in% TRUE, dev = .049),
  list(disp = "Ethnicity (yes)", var = "ethnicity", type = "bin", pos = function(x) x %in% c(1, 1L), dev = .064),
  list(disp = "Foreign nationality (yes)", var = "national_foreign", type = "bin", pos = function(x) x %in% c(1L, 1), dev = .006),
  list(disp = "Polysubstance use (yes)", var = "polysubstance_strict", type = "bin", pos = function(x) x %in% c(1L, 1), dev = .731),
  list(disp = "Female sex (yes)", var = "sex_rec", type = "bin", pos = function(x) x == "woman", dev = .256),
  list(disp = "SUD severity: hazardous use (yes)", var = "sub_dep_icd10_status", type = "bin",
       pos = function(x) x == "hazardous consumption", dev = .273),
  list(disp = "Housing type: other/unknown (yes)", var = "tipo_de_vivienda_rec2", type = "bin",
       pos = function(x) x == "other/unknown", dev = .111)
)

extval_table1_smd <- function(nondum) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  n_ext <- nrow(nondum)

  for (bl in .EXTVAL_T1_BLOCKS) {
    x <- nondum[[bl$var]]
    if (!is.null(bl$recode)) x <- bl$recode(x)

    if (bl$type == "cont") {
      v <- suppressWarnings(as.numeric(x)); v <- v[is.finite(v)]
      m <- mean(v); s <- stats::sd(v)
      add(Variable = bl$disp, Level = "",
          Development = sprintf("%.1f (%.1f)", bl$dev_mean, bl$dev_sd),
          External = sprintf("%.1f (%.1f)", m, s),
          SMD = sprintf("%.3f", .smd_cont(bl$dev_mean, bl$dev_sd, m, s)))
    } else if (bl$type == "bin") {
      ok <- !is.na(x); p_ext <- mean(bl$pos(x[ok])); n_pos <- sum(bl$pos(x[ok]))
      add(Variable = bl$disp, Level = "",
          Development = sprintf("%.1f%%", 100 * bl$dev),
          External = sprintf("%d (%.1f%%)", n_pos, 100 * p_ext),
          SMD = sprintf("%.3f", .smd_bin(bl$dev, p_ext)))
    } else {  # cat
      ok <- !is.na(x) & x != ""; xt <- x[ok]; nden <- length(xt)
      first <- TRUE
      for (lv in names(bl$dev)) {
        n_lv <- sum(xt == lv); p_ext <- if (nden) n_lv / nden else NA_real_
        add(Variable = if (first) bl$disp else "", Level = unname(bl$labels[lv]),
            Development = sprintf("%.1f%%", 100 * bl$dev[lv]),
            External = sprintf("%d (%.1f%%)", n_lv, 100 * p_ext),
            SMD = sprintf("%.3f", .smd_bin(unname(bl$dev[lv]), p_ext)))
        first <- FALSE
      }
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "n_ext") <- n_ext
  attr(out, "n_dev") <- .EXTVAL_DEV_N
  out
}

# Grouped, scrollable browsable table (htmltools), large-table friendly.
extval_smd_browsable <- function(tab, font = "'Times New Roman', serif") {
  n_ext <- attr(tab, "n_ext"); n_dev <- attr(tab, "n_dev")
  th <- "position:sticky;top:0;background:#f3f3f3;border-bottom:2px solid #ccc;padding:6px 10px;text-align:center;white-space:nowrap;font-weight:bold;"
  td <- "border-bottom:1px solid #eee;padding:4px 10px;white-space:nowrap;"
  hdr <- htmltools::tags$thead(htmltools::tags$tr(lapply(
    c("Variable", "Level",
      sprintf("Development (n=%s)", format(n_dev, big.mark = ",")),
      sprintf("External 2021-2024 (n=%s)", format(n_ext, big.mark = ",")), "SMD"),
    function(nm) htmltools::tags$th(style = th, nm))))
  body <- lapply(seq_len(nrow(tab)), function(i) {
    bold <- nzchar(tab$Variable[i])
    htmltools::tags$tr(
      htmltools::tags$td(style = paste0(td, "text-align:left;", if (bold) "font-weight:bold;" else "padding-left:22px;"), tab$Variable[i]),
      htmltools::tags$td(style = paste0(td, "text-align:left;"), tab$Level[i]),
      htmltools::tags$td(style = paste0(td, "text-align:center;"), tab$Development[i]),
      htmltools::tags$td(style = paste0(td, "text-align:center;"), tab$External[i]),
      htmltools::tags$td(style = paste0(td, "text-align:center;",
        if (suppressWarnings(as.numeric(tab$SMD[i])) >= 0.1 && is.finite(suppressWarnings(as.numeric(tab$SMD[i])))) "color:#B2182B;font-weight:bold;" else ""),
        tab$SMD[i]))
  })
  htmltools::browsable(htmltools::tags$div(
    htmltools::tags$div(style = paste0("font-weight:bold;margin-bottom:6px;font-family:", font, ";"),
      "Table. Covariate distribution: development cohort (2010-2020 training set) versus external temporal cohort (2021-2024), with standardized mean differences."),
    htmltools::tags$div(style = paste0("max-height:600px;overflow:auto;border:1px solid #ddd;font-family:", font, ";font-size:13px;"),
      htmltools::tags$table(style = "border-collapse:collapse;width:max-content;min-width:100%;",
        hdr, htmltools::tags$tbody(body))),
    htmltools::tags$div(style = paste0("font-size:11px;color:#555;margin-top:6px;max-width:900px;font-family:", font, ";"),
      "Development values are the project training-set Table 1 figures (percentages among non-missing). External percentages are computed among non-missing. SMD computed from the two proportions (categorical/binary) or means and SDs (continuous); |SMD| >= 0.10 highlighted as a temporal shift. The communal poverty index development SD is reported to one decimal in the source table, so its SMD is approximate.")))
}
