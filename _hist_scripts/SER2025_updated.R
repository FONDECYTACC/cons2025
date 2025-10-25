# 0. Setup & packages -----------------------------------------------------

rm(list=ls());gc()
while(!dev.cur())dev.off()
cat("\014")

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  mexhaz,      # Flexible parametric hazard regression models for survival analysis
  tidyverse,   # Collection of packages for data manipulation, visualization, and more (includes dplyr, ggplot2, tidyr, etc.)
  janitor,     # Simple tools for examining and cleaning dirty data
  tableone,    # Create "Table 1" summaries for descriptive statistics in medical research
  cowplot,     # Streamlined plot theme and plot annotations for ggplot2
  grid,        # Base R package for low-level graphics functions (used for arranging plots)
  rio,         # Simplifies data import/export with a consistent interface
  coin,        # Conditional inference procedures for hypothesis testing
  kableExtra,  # Enhances 'knitr::kable()' for creating complex tables in R Markdown
  epitools,    # Epidemiological tools for data analysis
  relsurv,     # Relative survival analysis for population-based cancer studies
  survminer,   # Survival analysis and visualization based on 'survival' package
  biostat3,    # Biostatistics functions and datasets for teaching and research
  tableone,    # (Repeated) Create descriptive summary tables for clinical research
  popEpi,      # For SMRs and SIR  
  install = T  # Automatically install packages if not already installed
)

#Archived on 2020-08-26 as requires archived package 'frailtypack'.
try(devtools::install_version("dsr", version="0.2.2", repos="http:/cran.us.r.project.org"))


extract_fit <- function(modelo) {
  logLik_value <- modelo$loglik
  # n_params <- attr(logLik_value, "df")
  # n_obs <- length(modelo$y)
  n_params <- modelo$n.par
  n_obs <- modelo$n.obs #no sé si mejor n.obs.tot
  convergence_code <- modelo$code # Check convergence status (0, 1, or 2)
  
  aic <- 2 * modelo$n.par - 2 * modelo$loglik
  bic <- log(modelo$n.obs) * modelo$n.par - 2 * modelo$loglik
  
  data.frame(
    LogLikelihood = as.numeric(logLik_value),
    NumParameters = n_params,
    NumObservations = n_obs,
    AIC = aic,
    BIC = bic,
    convergence = convergence_code
  )
}
calculate_smr_orig = function(data) {
  data|>
    summarise(
      Observed = sum(observed),
      Expected = sum(expected)
    )|>
    rowwise()|>
    mutate(
      SMR = Observed / Expected,
      lo = biostat3::poisson.ci(Observed, Expected)[1],
      up = biostat3::poisson.ci(Observed, Expected)[2]
    )|>
    ungroup()
}
calculate_smr = function(data) {
  data|>
    summarise(
      Observed = sum(observed, na.rm=T),
      Expected = sum(expected, na.rm=T)
    )|>
    rowwise()|>
    mutate(
      SMR = Observed / Expected,
      lo = biostat3::poisson.ci(Observed, Expected)[1],
      up = biostat3::poisson.ci(Observed, Expected)[2]
    )|>
    ungroup()
}

calculate_smr_alt <- function(data) {
  data|>
    summarise(
      Observed = sum(observed),
      Expected = sum(expected)
    )|>
    rowwise()|>
    mutate(
      # Reemplazar Expected == 0 con un valor pequeño
      Expected = ifelse(Expected == 0, 1e-5, Expected),
      Observed = ifelse(Observed == 0, 1e-5, Observed),
      SMR = Observed / Expected,
      lo = biostat3::poisson.ci(Observed, Expected)[1],
      up = biostat3::poisson.ci(Observed, Expected)[2]
    )|>
    ungroup()
}


theme_sjPlot_manual <- function() {
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  )
}


summarize_numerical_tt <- function(df, var) {
  
  df <- as_tidytable(df)[!is.na(status)]   # drop rows with missing status
  
  # ── 1. summaries by status ──────────────────────────────────────
  by_status <- df %>% 
    summarise(
      Total  = .N,
      Mean   = sprintf("%.1f", mean(get(var), na.rm = TRUE)),
      SD     = sprintf("%.1f", sd(get(var),  na.rm = TRUE)),
      Median = sprintf("%.1f", median(get(var), na.rm = TRUE)),
      IQR    = sprintf("%.1f", IQR(get(var),   na.rm = TRUE)),
      Min    = sprintf("%.1f", min(get(var),   na.rm = TRUE)),
      Max    = sprintf("%.1f", max(get(var),   na.rm = TRUE)),
      pres   = sprintf("%.1f [%.1f-%.1f]",
                       median(get(var), na.rm = TRUE),
                       quantile(get(var), .25, na.rm = TRUE),
                       quantile(get(var), .75, na.rm = TRUE)),
      .by = status
    )
  
  # ── 2. overall (Total) row ──────────────────────────────────────
  overall <- df %>% 
    summarise(
      Total  = .N,
      Mean   = sprintf("%.1f", mean(get(var), na.rm = TRUE)),
      SD     = sprintf("%.1f", sd(get(var),  na.rm = TRUE)),
      Median = sprintf("%.1f", median(get(var), na.rm = TRUE)),
      IQR    = sprintf("%.1f", IQR(get(var),   na.rm = TRUE)),
      Min    = sprintf("%.1f", min(get(var),   na.rm = TRUE)),
      Max    = sprintf("%.1f", max(get(var),   na.rm = TRUE)),
      pres   = sprintf("%.1f [%.1f-%.1f]",
                       median(get(var), na.rm = TRUE),
                       quantile(get(var), .25, na.rm = TRUE),
                       quantile(get(var), .75, na.rm = TRUE))
    ) %>% 
    mutate(status = "Total")
  
  # ── 3. bind and return ──────────────────────────────────────────
  bind_rows(by_status, overall) |>
    mutate(status = factor(status, levels = c("Total", "0", "1"))) |>
    arrange(status)
}

summarize_categorical_tt <- function(.data, var) {
  var  <- rlang::as_name(rlang::ensym(var))    # make sure it’s a plain string
  tbl  <- as_tidytable(.data)                  # guarantee tidytable class
  
  ## ── counts per status ──────────────────────────────────────────────
  tab <- tbl %>% 
    tidytable::summarise(
      n   = .N,
      .by = c("status", var)                  # fast group-by in tidytable
    ) %>% 
    pivot_wider(
      names_from  = status,
      values_from = n,
      values_fill = 0
    ) %>% 
    mutate(Total = rowSums(across(where(is.numeric))))
  
  ## ── build “count (pct)” strings ────────────────────────────────────
  status_cols <- setdiff(names(tab), c(var, "Total"))
  status_cols <- sort(status_cols, na.last = TRUE)  # ensure "0" then "1", etc.
  pct_cols    <- c("Total", status_cols)
  
  tab <- tab %>% 
    mutate(
      across(
        all_of(pct_cols),
        \(x) paste0(x, " (", sprintf("%.1f", x / sum(x) * 100), ")"),
        .names = "{.col}_pct"
      )
    )
  
  ## ── keep only the pct columns in required order ────────────────────
  tab %>% 
    tidytable::select(all_of(c(var, "Total_pct", paste0(status_cols, "_pct"))))
}
#SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv SISTRAT23_c1_2010_2022_df_prev1q_sel3b_surv
# treatment modality
# substance use at treatment entry
# initial treatment outcome
# days in treatment
# age at treatment entry
# year of treatment initiation


year_fraction <- function(dates) {
  dates <- as.Date(dates)  # Ensure input is Date class
  
  years <- as.numeric(format(dates, "%Y"))
  day_of_year <- as.numeric(format(dates, "%j"))
  
  # Function to check leap year
  is_leap_year <- function(year) {
    (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  }
  
  days_in_year <- ifelse(is_leap_year(years), 366, 365)
  
  years + (day_of_year - 1) / days_in_year
}


overdisp_cmrs <- function(deaths, pt, overdisp= 3.708736) {  
  deaths<- deaths
  pt<-pt
  
  lambda <- deaths / pt
  phi    <- overdisp
  z      <- qnorm(0.975)
  # 2. Error estándar logarítmico
  se_log <- sqrt(phi / deaths)
  
  # 3. Límites en escala natural
  lci <- exp(log(lambda) - z*se_log)
  uci <- exp(log(lambda) + z*se_log)
  
  data.frame(
    CMR_per_personyr = lambda,
    Lower_95CI       = lci,
    Upper_95CI       = uci
  ) |> 
    mutate(across(c("CMR_per_personyr", "Lower_95CI", "Upper_95CI"), ~sprintf("%1.1f",.*1000) ))
}

sir_ci_phi <- function(obs = NULL, exp = NULL,
                       D = NULL,   E  = NULL,
                       phi, conf.level = 0.95) {
  # Allow synonyms
  if (is.null(obs) && !is.null(D)) obs <- D
  if (is.null(exp) && !is.null(E)) exp <- E
  
  if (any(is.null(c(obs, exp)))) 
    stop("Faltan los totales observados (obs/D) o esperados (exp/E).")
  if (phi <= 0) stop("phi debe ser positivo.")
  
  theta  <- obs / exp
  z      <- qnorm(1 - (1 - conf.level) / 2)
  se_log <- sqrt(phi / obs)
  
  lci <- theta * exp(-z * se_log)
  uci <- theta * exp( z * se_log)
  
  data.frame(
    SMR_SIR   = theta,
    Lower_CI  = lci,
    Upper_CI  = uci,
    phi_hat   = phi
  )
}

extract_spline_data <- function(x) {
  if (is.null(x$spline.seq.A)) stop('No splines found.')
  
  plotdim <- as.numeric(c(!is.null(x$spline.seq.A),
                          !is.null(x$spline.seq.B),
                          !is.null(x$spline.seq.C)))
  splines <- c('spline.seq.A', 'spline.seq.B', 'spline.seq.C')[1:sum(plotdim)]
  ests <- gsub("seq", "est", splines)
  
  library(tidyr)
  library(dplyr)
  
  all_data <- lapply(seq_along(splines), function(i) {
    spline_name <- splines[i]
    est_name <- ests[i]
    
    # Extract spline sequence and estimates
    spline_seq <- x[[spline_name]]
    est_df <- x[[est_name]]
    
    # Check if est_df has a factor/level column (usually first column)
    if (ncol(est_df) >= 4) {
      # Assuming first column is level, next columns are estimate, lower CI, upper CI
      df <- data.frame(
        spline = x$spline[i],
        spline_value = spline_seq,
        level = est_df[,1],
        estimate = est_df[,2],
        lower_ci = est_df[,3],
        upper_ci = est_df[,4]
      )
    } else {
      # If no levels, just estimates and CIs
      df <- data.frame(
        spline = x$spline[i],
        spline_value = spline_seq,
        estimate = est_df[,1],
        lower_ci = est_df[,2],
        upper_ci = est_df[,3]
      )
    }
    return(df)
  })
  
  # Combine all spline data into one data frame
  combined_df <- bind_rows(all_data)
  
  return(combined_df)
}

#Fay & Feuer (1997). Confidence intervals for directly standardized rates: a 
#method based on the gamma distribution. Stat Med 16:791-801.
dsr_format <- function(rate, se, phi = 1, factor = 1e3, digits = 2, conf = 0.95) {
  z <- qnorm(1 - (1 - conf)/2)
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
          rate*factor,
          pmax(0, (rate - z*se*sqrt(phi))*factor),
          (rate + z*se*sqrt(phi))*factor)
}
dsr_format_corr <- function(rate, se, phi = 1,
                       factor = 1e4,       # multiplica la tasa (p. ej. ×100 000)
                       digits = 2,         # decimales a mostrar
                       conf   = 0.95) {    # nivel de confianza
  z  <- qnorm(1 - (1 - conf) / 2)
  se <- se * sqrt(phi)                     # sobredispersión
  se_log <- ifelse(rate > 0, se / rate, NA)
  
  L <- rate * exp(-z * se_log)
  U <- rate * exp( z * se_log)
  
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
          rate * factor, L * factor, U * factor)
}

# 1. Import and format data -----------------------------------------------

library(tidytable)

cat(paste0("Load the last database available from\n", 
           paste0(gsub("/cons","",here::here()), "/data/20241015_out/4_ndp_2025_06_06.Rdata")
           ,"\n"))
load(paste0(gsub("/cons","",here::here()), "/data/20241015_out/4_ndp_2025_06_06.Rdata"))

rm("HOSP_filter_pl_filt")

cat("Select relevant variables\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel<- SISTRAT23_c1_2010_2022_df_prev1q[, c("rn",
                                                                            "hash_key",
                                                                            "adm_age_rec2",#"adm_yr_rec",
                                                                            "birth_date_rec",#"birth_date_rec_imp",
                                                                            "adm_date_rec2",#"adm_date_rec",
                                                                            "adm_date_num_rec2",#"adm_date_rec_num",
                                                                            "TABLE",#"TABLE_rec",
                                                                            "dit_rec6",#"dit_rec",
                                                                            "disch_date_num_rec6",#"disch_date_num",
                                                                            "disch_date_rec6",
                                                                            "tr_compliance_rec3",#motivo_de_egreso",
                                                                            "primary_sub",#"sustancia_principal",
                                                                            "second_sub1",#"sustancia_principal",
                                                                            "second_sub2",#"sustancia_principal",
                                                                            "second_sub3",#"sustancia_principal",
                                                                            "sub_dep_icd10_status",#"diagnostico_trs_consumo_sustancia",
                                                                            "sex_rec", #"sexo",
                                                                            "municipallity_res_cutpre18",#"comuna_residencia",
                                                                            "region_del_centro",#
                                                                            "evaluacion_del_proceso_terapeutico",
                                                                            "edad_inicio_consumo",
                                                                            "ed_attainment",
                                                                            "plan_type"# treatment modality
)]

cat("Duplicados por tener la misma fecha de ingreso (aunque algunos en verdad tienen 0 días ent tto.")
SISTRAT23_c1_2010_2022_df_prev1q_sel|>
  janitor::get_dupes(hash_key, adm_age_rec2)|>  nrow()
# sólo 66 casos están duplicados porque tienen la misma fecha de ingreso y el mismo hash_key
#
rows_with_dupes<-SISTRAT23_c1_2010_2022_df_prev1q_sel|>
  janitor::get_dupes(hash_key, adm_age_rec2)|> 
  pull(rn)

invisible("Conserva los casos que tengan más días en tratamiento para la misma fecha de ingreso y el mismo hash_key")
SISTRAT23_c1_2010_2022_df_prev1q_sel2<-SISTRAT23_c1_2010_2022_df_prev1q_sel|>
  (\(df) {
    cat(paste0("1.Number of cases before discarding duplicates in admission age and hash key: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("1.Number of patients before discarding duplicates in admission age and hash key: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()|> 
  group_by(hash_key, adm_age_rec2)|> 
  slice_max(dit_rec6)|> 
  ungroup()|> 
  (\(df) {
    cat(paste0("1.Number of cases after discarding duplicates in admission age and hash key: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("1.Number of patients after discarding duplicates in admission age and hash key: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()
  
#SISTRAT23_c1_2010_2022_df_prev1f_sel2 |> janitor::tabyl(diagnostico_trs_consumo_sustancia)
#SISTRAT23_c1_2010_2022_df_prev1f_sel2$yr<-as.integer(SISTRAT23_c1_2010_2022_df_prev1f_sel2$TABLE_rec2)

cat("Make the death date\n")
mortality$death_date <- 
  as.Date(paste0(mortality$ano_def, "-", 
                 sprintf("%02.0f",mortality$mes_def), "-", 
                 mortality$dia_def))

cat("Maximum death date available:\n")
max(mortality$death_date, na.rm=T)
#[1] "2020-12-31"
invisible(" Remember that I only have data until 2020")



cat("Join mortality data with SENDA treatments\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel2 |>
  tidytable::left_join(mortality[,c("hashkey", "death_date")], by=c("hash_key"="hashkey"), multiple="first") |>
  tidytable::mutate(status=ifelse(is.na(death_date), 0, 1))

SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv <- as_tidytable(SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv)

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
cat("count previous treatments, but i get the first treatment(3a)\n")
invisible("Me quedo con el primer tratamiento, pero antes cuento si hay posteriores")
SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv|>
  tidytable::arrange(hash_key, adm_age_rec2)|>
  #treatments between 2010 and 2020
  tidytable::filter(adm_date_rec2>="2010-01-01", adm_date_rec2<"2020-12-31")|> 
  tidytable::group_by(hash_key) |>
  tidytable::mutate(tto= tidytable::row_number())|>
  tidytable::slice_min(tto)|>
  tidytable::ungroup()|>
  #admission ages between 18-65
  tidytable::filter(adm_age_rec2>=18, adm_age_rec2<65)|> 
  tidytable::mutate(post_ttos=ifelse(tto>1, 1, 0))|> 
  (\(df) {
    cat(paste0("3a.Number of cases, 2010-2019, first treatment: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("3a.Number of patients, 2010-2019, first treatment: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()

cat("i did it in ages: 15-64\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel3a2_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv|>
  tidytable::filter(TABLE<2021) |>
  tidytable::arrange(hash_key, adm_age_rec2)|>
  #treatments between 2010 and 2020
  tidytable::filter(adm_date_rec2>="2010-01-01", adm_date_rec2<"2021-01-01")|> 
  tidytable::group_by(hash_key) |>
  tidytable::mutate(tto= tidytable::row_number())|>
  tidytable::slice_min(tto)|>
  tidytable::ungroup()|>
  #admission ages between 18-65
  tidytable::filter(adm_age_rec2>=15, adm_age_rec2<65)|> 
  tidytable::mutate(post_ttos=ifelse(tto>1, 1, 0))|> 
  (\(df) {
    cat(paste0("3a.Number of cases, 2010-2019, first treatment: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("3a.Number of patients, 2010-2019, first treatment: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()

cat("count previous treatments, but i get the last treatment(3b)\n")
invisible("Me quedo con el último tratamiento, pero antes cuento si hay previos")
SISTRAT23_c1_2010_2022_df_prev1q_sel3b_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv|>
  #tidytable::filter(TABLE<2021)|>
  tidytable::arrange(hash_key, adm_age_rec2)|>
  #treatments between 2010 and 2020
  tidytable::filter(adm_date_rec2>="2010-01-01", adm_date_rec2<"2020-12-31")|> 
  tidytable::group_by(hash_key) |>
  tidytable::mutate(tto= tidytable::row_number())|>
  tidytable::slice_max(tto)|>
  tidytable::ungroup()|>
  #admission ages between 18-65 (85,763 - 84,502)= 84.897
  tidytable::filter(adm_age_rec2>=18, adm_age_rec2<65)|> 
  tidytable::mutate(prev_ttos=ifelse(tto>1, 1, 0))|> 
  (\(df) {
    cat(paste0("3b.Number of cases, 2010-2019, last (LVCF) treatment: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("3b.Number of patients, 2010-2019, last (LVCF) treatment: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
  (\(df) {
    cat(paste0("Discarded (death, no tr. compliance), cases(4a): ", formatC(nrow(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3))), big.mark=",")),"\n")
    cat(paste0("Discarded (death, no tr. compliance), patients(4a): ", formatC(nrow(distinct(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3)), hash_key)), big.mark=",")),"\n")
  df
    })()|>
  tidytable::filter(!is.na(tr_compliance_rec3) & tr_compliance_rec3!="death" & !grepl("truncated|currently|referral", tr_compliance_rec3))|>
  # calculamos la edad al egreso
  tidytable::mutate(disch_age_rec= (dit_rec6/365.241)+adm_age_rec2)|>
  tidytable::mutate(timesurv= tidytable::case_when(
    status==1~ as.numeric(difftime(death_date, adm_date_rec2, units="days")/365.241),
    status==0~ as.numeric(difftime(as.Date("2020-12-31"), adm_date_rec2, units="days")/365.241)))|>
  tidytable::mutate(death_date_rec= tidytable::case_when(
    status==1~ death_date,
    status==0~ as.Date("2020-12-31")))|>
  tidytable::mutate(death_age_rec= as.integer(timesurv+adm_age_rec2))|> 
  (\(df) {
    cat(paste0("4b.Number of cases: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("4b.Number of patients: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()

cat("Records were excluded for patients who had not yet been discharged")
 paste0(round((13663/222945)*100,1),"%")
 

 paste0(round((14443/222945)*100,1),"%")
 
SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel3a2_surv|>
  (\(df) {
    cat(paste0("Discarded (death, no tr. compliance), cases(4a): ", formatC(nrow(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3))), big.mark=",")),"\n")
    cat(paste0("Discarded (death, no tr. compliance), patients(4a): ", formatC(nrow(distinct(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3)), hash_key)), big.mark=",")),"\n")
    df
  })()|>
  tidytable::filter(!is.na(tr_compliance_rec3) & tr_compliance_rec3!="death" & !grepl("truncated|currently|referral", tr_compliance_rec3))|>
  # calculamos la edad al egreso
  tidytable::mutate(disch_age_rec= (dit_rec6/365.241)+adm_age_rec2)|>
  tidytable::mutate(timesurv= tidytable::case_when(
    status==1~ as.numeric(difftime(death_date, adm_date_rec2, units="days")/365.241),
    status==0~ as.numeric(difftime(as.Date("2021-01-01"), adm_date_rec2, units="days")/365.241)))|>
  tidytable::mutate(death_date_rec= tidytable::case_when(
    status==1~ death_date,
    status==0~ as.Date("2021-01-01")))|>
  tidytable::mutate(death_age_rec= as.integer(timesurv+adm_age_rec2))|> 
  (\(df) {
    cat(paste0("4b.Number of cases: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("4b.Number of patients: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()

SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel3b_surv|>
  (\(df) {
    cat(paste0("Discarded (death, no tr. compliance), cases(4b): ", formatC(nrow(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3))), big.mark=",")),"\n")
    cat(paste0("Discarded (death, no tr. compliance), patients(4b): ", formatC(nrow(distinct(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3)), hash_key)), big.mark=",")),"\n")
    df
  })()|>
  tidytable::filter(!is.na(tr_compliance_rec3) & tr_compliance_rec3!="death" & !grepl("truncated|currently|referral", tr_compliance_rec3))|>  # calculamos la edad al egreso
  tidytable::mutate(disch_age_rec= (dit_rec6/365.241)+adm_age_rec2)|>
  tidytable::mutate(death_date_rec= tidytable::case_when(
    status==1~ death_date,
    status==0~ as.Date("2020-12-31")))|>
  tidytable::mutate(timesurv= tidytable::case_when(
    status==1~ as.numeric(difftime(death_date, adm_date_rec2, units="days")/365.241),
    status==0~ as.numeric(difftime(as.Date("2021-01-01"), adm_date_rec2, units="days")/365.241)))|>
  tidytable::mutate(death_age_rec= as.integer(timesurv+adm_age_rec2))|> 
  (\(df) {
    cat(paste0("4b.Number of cases: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("4b.Number of patients: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()

#add integer of admission age
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$adm_age_rec2_int <- as.integer(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$adm_age_rec2)
SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv$adm_age_rec2_int <- as.integer(SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv$adm_age_rec2)

SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv$adm_age_rec2_int <- as.integer(SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv$adm_age_rec2)

#add admission year
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$yr_adm <- floor(year(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$adm_date_rec2))
SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv$yr_adm <- floor(year(SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv$adm_date_rec2))

SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv$yr_adm <- floor(year(SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv$adm_date_rec2))  
# 2. Unite with mortality tables -----------------------------------------------

invisible("Unir con tablas de mortalidad")

message("mx (tasa de mortalidad)")
message("ax (número promedio de años vividos en el intervalo de años de quienes murieron)")
message("qx (probabilidad de morir)")
message("mx a qx: qx = 1 - exp((-1 * age_length) * mx)")
message("mx_ax_to_qx: qx = (age_length * mx) / (1 + (age_length - ax) * mx)")
message("qx_to_mx: mx = log(1 - qx) / (-1 * age_length)")
message("mx_qx_to_ax: ax = (qx + (mx * age_length * (qx - 1))) / (mx * qx)")
message("qx_ax_to_mx: mx = qx / (age_length - (age_length * qx) + (ax * qx))")
# Verificar si la carpeta G:/Mi unidad existe

## 2.1. Bring external tables ---------------------------------------------------------

base_path <- paste0(here::here(),"/cons/_input/")

# Importar los archivos desde la ruta base
mltper_1x1 <- try(rio::import(paste0(base_path, "mltper_1x1.txt")))
fltper_1x1 <- try(rio::import(paste0(base_path, "fltper_1x1.txt")))
mltper_5x1 <- try(rio::import(paste0(base_path, "mltper_5x1.txt")))
fltper_5x1 <- try(rio::import(paste0(base_path, "fltper_5x1.txt")))

mltper_1x10 <- try(rio::import(paste0(base_path, "mltper_1x10.txt")))
fltper_1x10 <- try(rio::import(paste0(base_path, "fltper_1x10.txt")))
mltper_5x10 <- try(rio::import(paste0(base_path, "mltper_5x10.txt")))
fltper_5x10 <- try(rio::import(paste0(base_path, "fltper_5x10.txt")))

#filtro para obtener las tasas de los últimos 10 años
mltper_1x10_filt<-mltper_1x10[mltper_1x10$Year=="2010-2019",]
fltper_1x10_filt<-fltper_1x10[fltper_1x10$Year=="2010-2019",]

mltper_5x10_filt<-mltper_5x10[mltper_5x10$Year=="2010-2019",]
fltper_5x10_filt<-fltper_5x10[fltper_5x10$Year=="2010-2019",]


mltper_1x1_filt<-mltper_1x1[between(mltper_1x1$Year,2010,2020),]
fltper_1x1_filt<-fltper_1x1[between(fltper_1x1$Year,2010,2020),]

mltper_5x1_filt<-mltper_5x1[between(mltper_5x1$Year,2010,2020),]
fltper_5x1_filt<-fltper_5x1[between(fltper_5x1$Year,2010,2020),]

#cambiar la edad a entero
mltper_1x10_filt$age_rec <- as.numeric(mltper_1x10_filt$Age)
fltper_1x10_filt$age_rec <- as.numeric(fltper_1x10_filt$Age)

mltper_5x1_filt$age_rec <- as.numeric(mltper_5x1_filt$Age)
fltper_5x1_filt$age_rec <- as.numeric(fltper_5x1_filt$Age)

mltper_1x10_filt$sex <- "male"
fltper_1x10_filt$sex <- "female"
mltper_5x10_filt$sex <- "male"
fltper_5x10_filt$sex <- "female"
mltper_1x1_filt$sex <- "male"
fltper_1x1_filt$sex <- "female"
mltper_5x1_filt$sex <- "male"
fltper_5x1_filt$sex <- "female"

cons_rate_sex_1x10<-
  rbind.data.frame(mltper_1x10_filt, fltper_1x10_filt)[,c("age_rec","sex", "qx","mx")]
cons_rate_sex_1x10<-cons_rate_sex_1x10[which(!is.na(cons_rate_sex_1x10$age_rec)),]
cons_rate_sex_1x10$lambda_p <- -log( 1 - cons_rate_sex_1x10$qx ) / 365.241

cons_rate_sex_5x10<-
  rbind.data.frame(mltper_5x10_filt, fltper_5x10_filt)[,c("Age","sex", "lx", "qx","mx")]
cons_rate_sex_5x10$lambda_p <- -log( 1 - cons_rate_sex_5x10$qx ) / 365.241
invisible("Capping valores cercanos al 100% muertes")
invisible("No esn ecesario porque not engo esos casos")
cons_rate_sex_5x10<-cons_rate_sex_5x10[which(cons_rate_sex_5x10$Age!="110+"),]

cons_rate_sex_5x1<-
  rbind.data.frame(mltper_5x1_filt, fltper_5x1_filt)[,c("Year","Age","sex", "lx","qx","mx")]
cons_rate_sex_5x1$lambda_p <- -log( 1 - cons_rate_sex_5x1$qx ) / 365.241
invisible("Capping valores cercanos al 100% muertes")
invisible("No esn ecesario porque not engo esos casos")
cons_rate_sex_5x1<-cons_rate_sex_5x1[which(cons_rate_sex_5x1$Age!="110+"),]


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

tablas_de_mortalidad_de_chile_1992_2050_agrupada <- 
  readxl::read_excel(paste0(base_path, "tablas-de-mortalidad-de-chile-1992-2050.xlsx"), 
                     sheet = "BD Tablas de Mortalidad", skip = 1)|> 
  janitor::clean_names()|> 
  dplyr::filter(ano>=2010, ano<=2020)|> 
  dplyr::mutate(edad= readr::parse_number(edad))|> 
  dplyr::filter(edad>=18, edad<65, region=="País")|>  
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad>=18 & edad<30~1,
                                                edad>=30 & edad<45~2,
                                                edad>=45 & edad<60~3,
                                                edad>=60 & edad<65~4,T~NA_real_))|>  
  dplyr::mutate(edad_anos_rec= factor(edad_anos_rec, levels=1:4, labels= c("18-29","30-44","45-59","60-64")))|>      
  dplyr::group_by(ano, sexo, edad_anos_rec)|> 
  dplyr::summarise(
    total_d_x = sum(d_x, na.rm = TRUE),      # Suma de muertes en el grupo
    total_l_x = sum(l_x, na.rm = TRUE),      # Suma de la población al inicio del grupo (debiese ser con años-persona)
    mean_m_x = mean(m_x),
    mortality_rate_grouped = total_d_x / total_l_x
  )

proy_ine_com<-
  rio::import("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/ine_estimaciones-y-proyecciones-2002-2035_base-2017_comunas0381d25bc2224f51b9770a705a434b74.csv?sfvrsn=b6e930a7_3&download=true")|> 
  tidyr::pivot_longer(cols = dplyr::starts_with("Poblacion"), 
                      names_to = "anio", 
                      values_to = "poblacion")|> 
  dplyr::mutate(anio= gsub("Poblacion ","",anio), anio=as.numeric(anio))|> 
  dplyr::filter(anio>=2010 & anio<=2020)|> 
  dplyr::mutate(edad_anos_rec= dplyr::case_when(Edad>=15 & Edad<30~1,
                                                Edad>=30 & Edad<45~2,
                                                Edad>=45 & Edad<60~3,
                                                Edad>=60 & Edad<65~4,T~NA_real_))|> 
  dplyr:: mutate(edad_cat = dplyr::case_when(
    Edad >= 15 & Edad < 20 ~ "15-19",
    Edad >= 20 & Edad < 25 ~ "20-24",
    Edad >= 25 & Edad < 30 ~ "25-29",
    Edad >= 30 & Edad < 35 ~ "30-34",
    Edad >= 35 & Edad < 40 ~ "35-39",
    Edad >= 40 & Edad < 45 ~ "40-44",
    Edad >= 45 & Edad < 50 ~ "45-49",
    Edad >= 50 & Edad < 55 ~ "50-54",  
    Edad >= 55 & Edad < 60 ~ "55-59",
    Edad >= 60 & Edad < 65 ~ "60-64",
    Edad >= 65 & Edad < 70 ~ "65-69",    
    Edad >= 70 & Edad < 75 ~ "70-74",
    Edad >= 75 & Edad < 80 ~ "75-79",    
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  )) 

proy_ine_reg_group<-
  proy_ine_com|> 
  #2025 le agrego eso para que sepamos que es igual que antes ser2024
  dplyr::filter(Edad>=15, Edad<65)|> 
  #fomateamos para calzarla con la anterior
  dplyr::mutate(reg_res= sprintf("%02d", Region))|> 
  dplyr::group_by(reg_res, `Sexo (1=Hombre 2=Mujer)`,edad_anos_rec, anio)|> 
  dplyr::mutate(edad_anos_rec= factor(edad_anos_rec, levels=1:4, labels= c("15-29","30-44","45-59","60-65")))|>     
  dplyr::summarise(poblacion= sum(poblacion, na.rm=T))

invisible("Ene 2025, lo hice recién")
proy_ine_group_2015<-
  proy_ine_com|> 
  #fomateamos para calzarla con la anterio
  dplyr::group_by(`Sexo (1=Hombre 2=Mujer)`,edad_cat, anio)|> 
  dplyr::summarise(poblacion= sum(poblacion, na.rm=T))|> 
  dplyr::filter(anio==2015)|> 
  dplyr::mutate(sexo= ifelse(`Sexo (1=Hombre 2=Mujer)`==1,"male","female"))

proy_ine_cont_2015<-
  proy_ine_com|> 
  #fomateamos para calzarla con la anterio
  dplyr::group_by(`Sexo (1=Hombre 2=Mujer)`,Edad, anio)|> 
  dplyr::summarise(poblacion= sum(poblacion, na.rm=T))|> 
  dplyr::filter(anio==2015) |> 
  dplyr::mutate(sexo= ifelse(`Sexo (1=Hombre 2=Mujer)`==1,"male","female"))


mort_2015<-
  mortality|> 
  filter(ano_def==2015)|> 
  mutate(edad_cant_cat = dplyr::case_when(
    edad_cant >= 15 & edad_cant < 20 ~ "15-19",
    edad_cant >= 20 & edad_cant < 25 ~ "20-24",
    edad_cant >= 25 & edad_cant < 30 ~ "25-29",
    edad_cant >= 30 & edad_cant < 35 ~ "30-34",
    edad_cant >= 35 & edad_cant < 40 ~ "35-39",
    edad_cant >= 40 & edad_cant < 45 ~ "40-44",
    edad_cant >= 45 & edad_cant < 50 ~ "45-49",
    edad_cant >= 50 & edad_cant < 55 ~ "50-54",  
    edad_cant >= 55 & edad_cant < 60 ~ "55-59",
    edad_cant >= 60 & edad_cant < 65 ~ "60-64",
    edad_cant >= 65 & edad_cant < 70 ~ "65-69",    
    edad_cant >= 70 & edad_cant < 75 ~ "70-74",
    edad_cant >= 75 & edad_cant < 80 ~ "75-79",    
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ))|>
  group_by(sexo, edad_cant_cat)|> 
  summarise(n=n())|> 
  ungroup()|> 
  mutate(sexo=if_else(sexo==1,"male","female"))

mort_cont_2015<-
  mortality |> 
  filter(ano_def==2015)|> 
  group_by(sexo, edad_cant)|> 
  summarise(n=n())|>
  ungroup()|> 
  mutate(sexo=ifelse(sexo==1,"male","female"))


#### 2.1a Caso raro--------------------------------------------------------------


rt<-
  relsurv::transrate.hmd(paste0(base_path, "mltper_1x1.txt"), 
                         paste0(base_path, "fltper_1x1.txt"))

# Supongamos que `rt` es tu objeto ratetable
# Convertir las dimensiones en vectores
ages <- attr(rt, "dimnames")$age  # Edad
years <- attr(rt, "dimnames")$year  # Año
sexes <- attr(rt, "dimnames")$sex  # Sexo

# Extraer los valores del ratetable
values <- as.vector(rt)  # Convierte el array en un vector plano

# Crear un dataframe con todas las combinaciones de las dimensiones
popmort1x1 <- expand.grid(age = ages,year = years,sex = sexes)

# Añadir los valores al dataframe
popmort1x1 <- popmort1x1 %>%
  mutate(rate = values*365.241) #introduje esto el 10-01-2025

#### 2.2 Pyramid--------------------------------------------------------------

#18-29, 30-44, 45-59, 60-64
senda_2010_2020<- 
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |> 
mutate(adm_age_cat = dplyr::case_when(
  adm_age_rec2 >= 18 & adm_age_rec2 < 30 ~ "18-29",
  adm_age_rec2 >= 30 & adm_age_rec2 < 45 ~ "30-44",
  adm_age_rec2 >= 45 & adm_age_rec2 < 60 ~ "45-59",
  adm_age_rec2 >= 60 & adm_age_rec2 < 65 ~ "60-64",
  TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
))|>
  group_by(sex_rec, adm_age_cat)|> 
  summarise(n=n())|> 
  ungroup() |> 
  filter(!is.na(adm_age_cat))

mort_2015<-
  mortality|> 
  filter(ano_def==2015)|> 
  mutate(adm_age_cat = dplyr::case_when(
    edad_cant >= 18 & edad_cant < 30 ~ "18-29",
    edad_cant >= 30 & edad_cant < 45 ~ "30-44",
    edad_cant >= 45 & edad_cant < 60 ~ "45-59",
    edad_cant >= 60 & edad_cant < 65 ~ "60-64",    
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ))|>
  group_by(sexo, adm_age_cat)|> 
  summarise(n=n())|> 
  ungroup()|> 
  mutate(sex_rec=if_else(sexo==1,"male","female"))

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
# Create a basic bar chart for one gender
basic_plot <-  ggplot(
  senda_2010_2020, 
  aes(
    x = adm_age_cat, 
    fill = sex_rec, 
    y = ifelse(
      test = sex_rec == "male", 
      yes = -n, 
      no = n
    )
  )
) + 
  geom_bar(stat = "identity") 

population_pyramid <- basic_plot +
  scale_y_continuous(
    labels = abs,
    limits = max(subset(senda_2010_2020, !is.na(sex_rec)& !is.na(adm_age_cat))$n) * c(-1,1)
  ) +
  coord_flip() +
  theme_minimal() +
  # Change scale_color_manual to scale_fill_manual
  scale_fill_manual(
    name = "Sex", # Change "Group" to "Sex" to match your labs()
    values = c(
      "female" = "#A6CEE3",
      "male" = "#1F78B4"
    ),
    na.translate = FALSE # Prevents NA from showing in legend if applicable
  ) +
  labs(
    x = "Age",
    y = "Population",
    fill = "Sex", # This matches the aesthetic and your new scale_fill_manual
    title = "SENDA population"
  )

tablas_de_mortalidad_de_chile_1992_2050_agrupada <- 
  readxl::read_excel(paste0(base_path, "tablas-de-mortalidad-de-chile-1992-2050.xlsx"), 
                     sheet = "BD Tablas de Mortalidad", skip = 1)|> 
  janitor::clean_names()|> 
  dplyr::filter(ano>=2010, ano<=2020)|> 
  dplyr::mutate(edad= readr::parse_number(edad))|> 
  dplyr::filter(edad>=18, edad<65, region=="País")|>  
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad>=18 & edad<30~1,
                                                edad>=30 & edad<45~2,
                                                edad>=45 & edad<60~3,
                                                edad>=60 & edad<65~4,T~NA_real_))|>  
  dplyr::mutate(adm_age_cat= factor(edad_anos_rec, levels=1:4, labels= c("18-29","30-44","45-59","60-64")))|> 
  mutate(sex_rec=if_else(sexo=="Hombre","male","female"))|> 
  dplyr::group_by(sex_rec, adm_age_cat)|> 
  dplyr::summarise(
    total_d_x = sum(d_x, na.rm = TRUE),      # Suma de muertes en el grupo
    total_l_x = sum(l_x, na.rm = TRUE),      # Suma de la población al inicio del grupo
    mean_m_x = mean(m_x),
    mortality_rate_grouped = total_d_x / total_l_x
  )

basic_plot2 <-  ggplot(
  tablas_de_mortalidad_de_chile_1992_2050_agrupada, 
  aes(
    x = adm_age_cat, 
    fill = sex_rec, 
    y = ifelse(
      test = sex_rec == "male", 
      yes = -total_l_x, 
      no = total_l_x
    )
  )
) + 
  geom_bar(stat = "identity") 

population_pyramid2 <- basic_plot2 +
  scale_y_continuous(
    labels = abs,
    limits = max(subset(tablas_de_mortalidad_de_chile_1992_2050_agrupada, !is.na(sex_rec))$total_l_x) * c(-1,1)
  ) +
  coord_flip() +
  theme_minimal() +
  # Change scale_color_manual to scale_fill_manual
  scale_fill_manual(
    name = "Sex", # Change "Group" to "Sex" to match your labs()
    values = c(
      "female" = "#A6CEE3",
      "male" = "#1F78B4"
    ),
    na.translate = FALSE # Prevents NA from showing in legend if applicable
  ) +
  labs(
    x = "Age",
    y = "Population",
    fill = "Sex", # This matches the aesthetic and your new scale_fill_manual
    title = "Chilean population"
  )
population_pyramid
population_pyramid2
# Create a basic bar chart for one gender
basic_plot <-  ggplot(
  senda_2010_2020, 
  aes(
    x = adm_age_cat, 
    fill = sex_rec, 
    y = ifelse(
      test = sex_rec == "male", 
      yes = -n, 
      no = n
    )
  )
) + 
  geom_bar(stat = "identity") 

plot_grid(population_pyramid+ theme(legend.position="none")+scale_y_continuous(
  labels = function(x) {
    scales::number_format(scale = 1e-6, suffix = "M")(abs(x))
  }), population_pyramid2+ labs(x=NULL)+ theme(axis.text.y = element_blank())+ scale_y_continuous(
  labels = function(x) {
    scales::number_format(scale = 1e-6, suffix = "M")(abs(x))
  }), ncol = 2)

ggsave(paste0(here::here(),"/cons/_figs/pyramid.png"), dpi = 600, width = 9)

## 2.b. Treatments ---------------------------------------------------------------

### 2.b.* C1 ---------------------------------------------------------------

### 2.b. Age of onset of substance use ---------------------------------------------------------------

CONS_C2_2324 <- c2_2324 %>% rename(edad_inicio = edad_inicio_sustancia_inicial, HASH_KEY=hashkey)
CONS_C2 <- CONS_C2 %>% rename(edad_inicio = edad_inicio_sustancia_inicial)
CONS_C4 <- CONS_C4 %>% rename(edad_inicio = edaddeiniciosustanciainicia)
CONS_C5 <- CONS_C5 %>% rename(edad_inicio = edad_inicio_sustancia_inicial)
CONS_C6 <- CONS_C6 %>% rename(edad_inicio = edaddeiniciosustanciainicia)
SISTRAT23_c1_2010_2022_df_prev1f <- SISTRAT23_c1_2010_2022_df_prev1f %>% 
  rename(edad_inicio = edad_inicio_consumo, HASH_KEY=hash_key)
SISTRAT23_c1_2023_2024_df2 <- SISTRAT23_c1_2023_2024_df2 %>% 
  rename(edad_inicio = edad_inicio_consumo, HASH_KEY=hash_key)

bases <- list(
  CONS_C2 %>% dplyr::select(HASH_KEY, edad_inicio) %>%
    dplyr::mutate(edad_inicio = as.numeric(edad_inicio)),
  CONS_C2_2324 %>% dplyr::select(HASH_KEY, edad_inicio) %>%
    dplyr::mutate(edad_inicio = as.numeric(edad_inicio)),
  CONS_C4 %>% dplyr::select(HASH_KEY, edad_inicio) %>%
    dplyr::mutate(edad_inicio = as.numeric(edad_inicio)),
  CONS_C5 %>% dplyr::select(HASH_KEY, edad_inicio) %>%
    dplyr::mutate(edad_inicio = as.numeric(edad_inicio)),
  CONS_C6 %>% dplyr::select(HASH_KEY, edad_inicio) %>%
    dplyr::mutate(edad_inicio = as.numeric(edad_inicio)),
  SISTRAT23_c1_2010_2022_df_prev1f %>%
    dplyr::select(HASH_KEY, edad_inicio) %>%
    dplyr::mutate(edad_inicio = as.numeric(edad_inicio)),
  SISTRAT23_c1_2023_2024_df2 %>%
    dplyr::select(HASH_KEY, edad_inicio) %>%
    dplyr::mutate(edad_inicio = as.numeric(edad_inicio))
)

# Unir todas las bases
edad_unificada <- bind_rows(bases)

# Calcular el promedio de edad por HASH_KEY
promedios_edades <- edad_unificada %>%
  group_by(HASH_KEY) %>%
  summarise(promedio_edad = mean(edad_inicio, na.rm = TRUE))


SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv<- 
  SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv|> 
  mutate(adm_age_cat = dplyr::case_when(
    adm_age_rec2 >= 18 & adm_age_rec2 < 30 ~ "18-29",
    adm_age_rec2 >= 30 & adm_age_rec2 < 45 ~ "30-44",
    adm_age_rec2 >= 45 & adm_age_rec2 < 60 ~ "45-59",
    adm_age_rec2 >= 60 & adm_age_rec2 < 65 ~ "60-64",
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ))|>
  filter(!is.na(adm_age_rec2))|> 
  mutate(yr_fr_birth_date_rec= year_fraction(birth_date_rec), 
         yr_fr_adm_date= year_fraction(adm_date_rec2), 
         yr_fr_disch_date= year_fraction(disch_date_rec6), 
         yr_fr_death_date_rec= year_fraction(death_date_rec))|> 
  mutate(macrozone = case_when(
    region_del_centro  %in% c("de arica y parinacota", "de tarapaca", "de antofagasta", "de atacama") ~ "1.North",
    region_del_centro  %in% c("de coquimbo", "de valparaiso")~ "2.Center",
    region_del_centro  %in% c("del libertador general bernardo ohiggins", "del maule", "del bio-bio") ~ "3.South-center",
    region_del_centro  %in% c("de la araucania	", "de los rios", "de los lagos") ~ "4.South",
    region_del_centro  %in% c("de magallanes y la antartica chilena", "aysen") ~ "5.Austral",
    TRUE ~ "Metropolitan"  # En caso de que algún código no esté especificado
  ))|>
  mutate(prim_sub_licit=ifelse(primary_sub=="alcohol","licit","illicit"))|>
  mutate(tr_compliance_status= case_when(grepl("completion", tr_compliance_rec3)~ "Completed", grepl("dropout|discharge", tr_compliance_rec3)~ "Not completed"))|> 
  mutate(rm_norm= ifelse(macrozone=="Metropolitan",1,0))|> 
  mutate(res_plan= ifelse(grepl("pr", plan_type),1,0))|>
  tidytable::select(-any_of(c("promedio_edad")))|> 
  left_join( promedios_edades, by=c("hash_key"="HASH_KEY"), multiple="first")|> 
  mutate(sub_use_onset= case_when(promedio_edad>4~ promedio_edad, T~edad_inicio_consumo))


SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv<- 
  SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv|> 
  mutate(adm_age_cat = dplyr::case_when(
    adm_age_rec2 >= 15 & adm_age_rec2 < 30 ~ "15-29",
    adm_age_rec2 >= 30 & adm_age_rec2 < 45 ~ "30-44",
    adm_age_rec2 >= 45 & adm_age_rec2 < 60 ~ "45-59",
    adm_age_rec2 >= 60 & adm_age_rec2 < 65 ~ "60-64",
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ))|>
  filter(!is.na(adm_age_rec2))|> 
  mutate(yr_fr_birth_date_rec= year_fraction(birth_date_rec), 
         yr_fr_adm_date= year_fraction(adm_date_rec2), 
         yr_fr_disch_date= year_fraction(disch_date_rec6), 
         yr_fr_death_date_rec= year_fraction(death_date_rec))|> 
  mutate(macrozone = case_when(
    region_del_centro  %in% c("de arica y parinacota", "de tarapaca", "de antofagasta", "de atacama") ~ "1.North",
    region_del_centro  %in% c("de coquimbo", "de valparaiso")~ "2.Center",
    region_del_centro  %in% c("del libertador general bernardo ohiggins", "del maule", "del bio-bio") ~ "3.South-center",
    region_del_centro  %in% c("de la araucania	", "de los rios", "de los lagos") ~ "4.South",
    region_del_centro  %in% c("de magallanes y la antartica chilena", "aysen") ~ "5.Austral",
    TRUE ~ "Metropolitan"  # En caso de que algún código no esté especificado
  ))|>
  mutate(prim_sub_licit=ifelse(primary_sub=="alcohol","licit","illicit"))|>
  mutate(tr_compliance_status= case_when(grepl("completion", tr_compliance_rec3)~ "Completed", grepl("dropout|discharge", tr_compliance_rec3)~ "Not completed"))|> 
  mutate(rm_norm= ifelse(macrozone=="Metropolitan",1,0))|> 
  mutate(res_plan= ifelse(grepl("pr", plan_type),1,0))|>
  tidytable::select(-any_of(c("promedio_edad")))|> 
  left_join( promedios_edades, by=c("hash_key"="HASH_KEY"), multiple="first")|> 
  mutate(sub_use_onset= case_when(promedio_edad>4~ promedio_edad, T~edad_inicio_consumo))|>#
  (\(df) {
    cat("RMvs.No-RM\n")
    print(janitor::chisq.test(janitor::tabyl(df, rm_norm, status))) 
    cat("RMvs.No-RM\n")
    print(janitor::chisq.test(janitor::tabyl(df, rm_norm, status)))     
    df
  })() |> 
  select(-promedio_edad)


SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv<- 
  SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv|> 
  mutate(adm_age_cat = dplyr::case_when(
    adm_age_rec2 >= 18 & adm_age_rec2 < 30 ~ "18-29",
    adm_age_rec2 >= 30 & adm_age_rec2 < 45 ~ "30-44",
    adm_age_rec2 >= 45 & adm_age_rec2 < 60 ~ "45-59",
    adm_age_rec2 >= 60 & adm_age_rec2 < 65 ~ "60-64",
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ))|>
  filter(!is.na(adm_age_rec2))|> 
  mutate(yr_fr_birth_date_rec= year_fraction(birth_date_rec), 
         yr_fr_adm_date= year_fraction(adm_date_rec2), 
         yr_fr_disch_date= year_fraction(disch_date_rec6), 
         yr_fr_death_date_rec= year_fraction(death_date_rec))|> 
  mutate(macrozone = case_when(
    region_del_centro  %in% c("de arica y parinacota", "de tarapaca", "de antofagasta", "de atacama") ~ "1.North",
    region_del_centro  %in% c("de coquimbo", "de valparaiso")~ "2.Center",
    region_del_centro  %in% c("del libertador general bernardo ohiggins", "del maule", "del bio-bio") ~ "3.South-center",
    region_del_centro  %in% c("de la araucania	", "de los rios", "de los lagos") ~ "4.South",
    region_del_centro  %in% c("de magallanes y la antartica chilena", "aysen") ~ "5.Austral",
    TRUE ~ "Metropolitan"  # En caso de que algún código no esté especificado
  ))|>
  mutate(prim_sub_licit=ifelse(primary_sub=="alcohol","licit","illicit"))|>
  mutate(tr_compliance_status= case_when(grepl("completion", tr_compliance_rec3)~ "Completed", grepl("dropout|discharge", tr_compliance_rec3)~ "Not completed"))|> 
  mutate(rm_norm= ifelse(macrozone=="Metropolitan",1,0))|> 
  mutate(res_plan= ifelse(grepl("pr", plan_type),1,0))|>
  select(-any_of(c("promedio_edad")))|> 
  left_join( promedios_edades, by=c("hash_key"="HASH_KEY"), multiple="first")|> 
  mutate(sub_use_onset= case_when(promedio_edad>4~ promedio_edad, T~edad_inicio_consumo))
  

# Relsurv final- utlimo formato (mod_c_c. 09-01-25) -----------------------------------------------------------


mx_1x1<-
  rbind.data.frame(cbind.data.frame(sex="male", mltper_1x1), 
                   cbind.data.frame(sex="female", fltper_1x1))[,c("Year", "sex", "Age", "mx", "qx")]
mx_1x1$Age<- as.numeric(mx_1x1$Age)


years_followup<-
  range(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$yr_adm)[1]:range(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$yr_adm)[2]

mx_1x1_filt<-mx_1x1[as.numeric(as.character(mx_1x1$Year)) %in% years_followup,]
mx_1x1_filt2<-mx_1x1_filt[as.numeric(as.character(mx_1x1_filt$Age)) %in% min(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$adm_age_rec2_int):max(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$adm_age_rec2_int),]

mx_1x1_filt2$lambda_p_days <- -log( 1 - mx_1x1_filt2$qx ) / 365.241
warning(paste0("El mx es rate de HMS pero multiplicado por 365.41"))
mx_1x1_filt2$lambda_p_yrs <- -log( 1 - mx_1x1_filt2$qx )



# Descriptivos ------------------------------------------------------------

## Desc-  resumen --------------------------------------------------------------------


# List of categorical variables
categorical_vars <- c("prim_sub_licit", "adm_age_cat", "res_plan", "sex_rec", "sub_dep_icd10_status",
                      "macrozone", "tr_compliance_status")

# List of numerical variables
numerical_vars <- c("adm_age_rec2", 
                    "dit_rec6", 
                    "death_age_rec",
                    "yr_fr_birth_date_rec", 
                    "yr_fr_adm_date",
                    "yr_fr_disch_date")

# Create summaries for categorical variables
categorical_summaries <- lapply(
  categorical_vars,                   # <- object you iterate over
  summarize_categorical_tt,           # <- function
  .data = SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv                    # <- extra argument “.data =”
)

# Create summaries for numerical variables
numerical_summaries <- lapply(numerical_vars, function(var) {
  summarize_numerical_tt(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv, var)
})
names(categorical_summaries) <- categorical_vars
names(numerical_summaries) <- numerical_vars

table_one <- CreateTableOne(
  vars = c(categorical_vars, numerical_vars),
  data = SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv,
  factorVars = categorical_vars,
  strata = "status",
  addOverall = TRUE,  # Incluir totales
  test = TRUE,        # Realizar pruebas de diferencias
  smd = TRUE          # Incluir SMDs
)

# Mostrar la tabla con perdidos
table_one_print <- print(table_one, 
                         nonnormal = numerical_vars, 
                         formatOptions = list(big.mark = ","),
                         quote = TRUE, 
                         noSpaces = TRUE,
                         showAllLevels = TRUE,  # Mostrar todos los niveles de variables categóricas
                         missing = TRUE,        # Incluir valores perdidos
                         explain = TRUE,        # Añadir explicación de las variables
                         digits = c(adm_age_rec2 = 1, dit_rec6 = 1, yr_fr_birth_date_rec = 1), smd = TRUE)

table_one_print |> 
  knitr::kable("html", digits=1) |> 
  kableExtra::kable_classic()

# Exportar a HTML o Word si es necesario
write.table(table_one_print, file = paste0(getwd(),"/cons/_out/table_one.txt"), sep = "\t", row.names = FALSE)


# para ver desagregación por grupos ---------------------------------------

xtabs(~tr_compliance_status+ prim_sub_licit+ res_plan+ sex_rec+ adm_age_cat, data= SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv)

# , , res_plan = 1, sex_rec = female, adm_age_cat = 60-65
# Sólo 2 en not complited e ilicit

xtabs(~status+ tr_compliance_status+ prim_sub_licit+ res_plan+ sex_rec+ adm_age_cat, data= SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv)

#, , prim_sub_licit = illicit, res_plan = 1, sex_rec = female, adm_age_cat = 60-65
#, , prim_sub_licit = illicit, res_plan = 1, sex_rec = male, adm_age_cat = 60-65
#no hay mujeres ni hombres que mueran por ilícitas de 60-65

#tiene pero pocos mueren, sin importar si completan o no, en residenciales, sustancias lícitas (alcohol), 18-29
#prim_sub_licit = licit, res_plan = 1, sex_rec = male, adm_age_cat = 18-29
#prim_sub_licit = licit, res_plan = 1, sex_rec = female, adm_age_cat = 18-29

xtabs(~sex_rec+ adm_age_cat+ yr_adm, data= SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv)


# SMRs --------------------------------------------------------------------

warning(paste0("El mx es rate de HMS pero multiplicado por 365.41"))

## SMRs- 2025-june --------------------------------------------------------------------

# library(popEpi)

#sire
# Classes ‘data.table’ and 'data.frame':	8243 obs. of  6 variables:
#   $ sex    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ bi_date: IDate, format: "1952-05-27" "1959-04-04" "1958-06-15" "1957-05-10" ...
# $ dg_date: IDate, format: "1994-02-03" "1996-09-20" "1994-05-30" "1997-09-04" ...
# $ ex_date: IDate, format: "2012-12-31" "2012-12-31" "2012-12-31" "2012-12-31" ...
# $ status : int  0 0 0 0 0 0 1 0 0 0 ...
# $ dg_age : num  41.7 37.5 36 40.3 39.7 ...
# - attr(*, ".internal.selfref")=<externalptr> 

c <- lexpand( sire, status = status, birth = bi_date, exit = ex_date, entry = dg_date,
              breaks = list(per = 1950:2013, age = 1:100, fot = c(0,10,20,Inf)), 
              aggre = list(fot, agegroup = age, year = per, sex) )

# str(popmort)
# Classes ‘data.table’ and 'data.frame':	12726 obs. of  4 variables:
#   $ sex     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ year    : int  1951 1951 1951 1951 1951 1951 1951 1951 1951 1951 ...
# $ agegroup: int  0 1 2 3 4 5 6 7 8 9 ...
# $ haz     : num  0.03636 0.00362 0.00217 0.00158 0.00118 ...
# - attr(*, ".internal.selfref")=<externalptr> 



cat("Check problematic rows")
with(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv, summary(as.numeric(death_date_rec - disch_date_rec6)))

with(SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv, summary(as.numeric(death_date_rec - disch_date_rec6)))


cat("Check birth date ranges")
round(min(parse_number(unlist(numerical_summaries$yr_fr_birth_date_rec[,"Min"]))),0)
round(max(parse_number(unlist(numerical_summaries$yr_fr_birth_date_rec[,"Max"]))),0)

round(min(SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv$yr_fr_birth_date_rec),0)
round(max(SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv$yr_fr_birth_date_rec),0)


cat("Are there any negative follow-up years?")
fot_years <- as.numeric((SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$death_date_rec - SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$disch_date_rec6) / 365.25)
summary(fot_years)

table(fot_years<0)
# FALSE  TRUE 
# 70349   506
# FALSE  TRUE 
# 70290  4180
paste0(round((as.numeric(table(fot_years<0)[2])/222945)*100,1),"%")

paste0(round((4180/222945)*100,1),"%")

cat("To clean the database we removed those who were discharge after censorship or death date\n")
cat("We also discarded missing sex, and we recoded sex into female=1, and male=0\n")

psych::describe(
as.numeric(with(SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv, 
           difftime(death_date_rec, disch_date_rec6, unit="days"))/ 365.25)
)

table(as.numeric(with(SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv, 
                      difftime(death_date_rec, disch_date_rec6, unit="days"))/ 365.25)
      <0)
# FALSE  TRUE 
# 71112  6229 
paste0(round((6229/222945)*100,1),"%")
#2.8

#:#:#:#:#:#:
disch_after_cens_death<- 
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>nrow()-
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>
  filter(!is.na(disch_date_rec6),
               !is.na(death_date_rec),
               disch_date_rec6 < death_date_rec,     # evita seguimiento negativo
               !is.na(sex_rec)) |> nrow()

disch_after_cens_death
invisible("and patients with discharge dates after censorship were excluded ")
invisible("or whose discharge date was after the censorship date ")
disch_after_cens_death/ 222945
paste0(round((disch_after_cens_death/222945)*100,1),"%")
#1.9%

disch_after_cens_death_b<- 
  SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv |>nrow()-
  SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv |>
  filter(!is.na(disch_date_rec6),
         !is.na(death_date_rec),
         disch_date_rec6 < death_date_rec,     # evita seguimiento negativo
         !is.na(sex_rec)) |> nrow()
paste0(round((disch_after_cens_death_b/222945)*100,1),"%")
#"2.8%

#:#:#:#:#:#:
#:#:#:#:#:#:
clean_df <- SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>
  (\(df) {
    cat("Before discarding missing or discharge dates \n")
    print(nrow(df))    
    df
  })() |> 
  filter(!is.na(disch_date_rec6),
         !is.na(death_date_rec),
         disch_date_rec6 < death_date_rec,     # evita seguimiento negativo
         adm_date_rec2  < disch_date_rec6,     # para sacar 831d9f7b2771ce2701ae4a4417f26e70f2e2acb21d21c6fe7ff1e766c9792d3a on días tto neg
         !is.na(sex_rec)) |>                   # grupos de agregación sin NA
  (\(df) {
    cat("After discarding missing or discharge dates \n")
    print(nrow(df))    
    df
  })() |> 
mutate(
    # sub_dep_icd10_status: reference = "Hazardous consumption"
    sub_dep_icd10_status = factor(
      sub_dep_icd10_status,
      levels = c("Hazardous consumption", "Drug dependence")
    ),
    # sex_rec: reference = "Male"  (rename from lower-case if needed)
    sex_rec = case_when(                 # optional renaming step
                sex_rec == "male"   ~ "Male",
                sex_rec == "female" ~ "Female",
                TRUE                ~ sex_rec) %>%
              factor(levels = c("Male", "Female"))
  )|>
  mutate(disch_age_cat = dplyr::case_when(
    disch_age_rec >= 18 & disch_age_rec < 30 ~ "18-29",
    disch_age_rec >= 30 & disch_age_rec < 45 ~ "30-44",
    disch_age_rec >= 45 & disch_age_rec < 60 ~ "45-59",
    disch_age_rec >= 60 & disch_age_rec < 86 ~ "60+",
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ))
nrow(clean_df)
#70214
#70064

#To check and reach the same database, but with survival estimates, similar to SIR function
source("G:/My Drive/Alvacast/SISTRAT 2023/cons/_hist_scripts/SER2025_updated_clean_df.R")
#clean_df_corr_surv
biostat3::survRate(Surv(clean_df_corr_surv$follow_up_days/365.25, status==1)~ sex_rec+ disch_age_cat+ year_death, data=clean_df_corr_surv)|>  
  dplyr::mutate(across(c("rate", "lower", "upper"),~sprintf("%1.1f",.*1000)))

biostat3::survRate(Surv(follow_up_days/365.2425, status==1)~1, data= clean_df_corr_surv|>
  mutate(year_fu = cut(disch_date_rec6 + follow_up_days,
                       breaks = as.Date(paste0(2010:2022, "-01-01")),
                       right = FALSE, labels = 2010:2021))) 

cat("Range of follow-up\n")
range(clean_df_corr_surv$disch_date_rec6 + clean_df_corr_surv$follow_up_days)
#[1] "2010-06-12" "2020-12-31"

cat("Maximum age at death\n")
clean_df_corr_surv |>  mutate(edad_salida = as.numeric((pmin(death_date_rec, disch_date_rec6 + follow_up_days) - birth_date_rec) / 365.2425)) |>
  summarise(max_age = max(edad_salida, na.rm = TRUE))

clean_df_b <- SISTRAT23_c1_2010_2022_df_prev1q_sel4b_surv |>
  (\(df) {
    cat("Before discarding missing or discharge dates \n")
    print(nrow(df))    
    df
  })() |> 
  filter(!is.na(disch_date_rec6),
         !is.na(death_date_rec),
         disch_date_rec6 < death_date_rec,     # evita seguimiento negativo
         adm_date_rec2  < disch_date_rec6,     # para sacar 831d9f7b2771ce2701ae4a4417f26e70f2e2acb21d21c6fe7ff1e766c9792d3a on días tto neg
         !is.na(sex_rec)) |>                   # grupos de agregación sin NA
  (\(df) {
    cat("After discarding missing or discharge dates \n")
    print(nrow(df))    
    df
  })() |> 
  mutate(
    # sub_dep_icd10_status: reference = "Hazardous consumption"
    sub_dep_icd10_status = factor(
      sub_dep_icd10_status,
      levels = c("Hazardous consumption", "Drug dependence")
    ),
    
    # sex_rec: reference = "Male"  (rename from lower-case if needed)
    sex_rec = case_when(                 # optional renaming step
      sex_rec == "male"   ~ "Male",
      sex_rec == "female" ~ "Female",
      TRUE                ~ sex_rec) %>%
      factor(levels = c("Male", "Female"))
  )|>
  mutate(disch_age_cat = dplyr::case_when(
    disch_age_rec >= 18 & disch_age_rec < 30 ~ "18-29",
    disch_age_rec >= 30 & disch_age_rec < 45 ~ "30-44",
    disch_age_rec >= 45 & disch_age_rec < 60 ~ "45-59",
    disch_age_rec >= 60 & disch_age_rec < 86 ~ "60+",
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ))
nrow(clean_df_b)
#71004
#70869

clean_df2 <- SISTRAT23_c1_2010_2022_df_prev1q_sel4a2_surv|>
  filter(!is.na(disch_date_rec6),
         !is.na(death_date_rec),
         disch_date_rec6 < death_date_rec,     # evita seguimiento negativo
         !is.na(sex_rec)) |>                   # grupos de agregación sin NA
  mutate(
    # sub_dep_icd10_status: reference = "Hazardous consumption"
    sub_dep_icd10_status = factor(
      sub_dep_icd10_status,
      levels = c("Hazardous consumption", "Drug dependence")
    ),
    
    # sex_rec: reference = "Male"  (rename from lower-case if needed)
    sex_rec = case_when(                 # optional renaming step
      sex_rec == "male"   ~ "Male",
      sex_rec == "female" ~ "Female",
      TRUE                ~ sex_rec) %>%
      factor(levels = c("Male", "Female"))
  )
nrow(clean_df2)
#70345

#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#### Descriptives, after cleaning ----------------------------------------
categorical_vars_corr<- 
c(categorical_vars,"disch_age_cat")

# Create summaries for categorical variables
categorical_summaries_cln <- lapply(
  c(categorical_vars_corr),# <- object you iterate over
  summarize_categorical_tt,           # <- function
  .data = clean_df                    # <- extra argument “.data =”
)
names(categorical_summaries_cln) <- categorical_vars_corr
# Create summaries for numerical variables
numerical_summaries_cln <- lapply(
  c(numerical_vars, "disch_age_rec"),# <- object you iterate over
  \(v) summarize_numerical_tt(clean_df, v)
)
names(numerical_summaries_cln) <- c(numerical_vars, "disch_age_rec")


table_one_clean <- CreateTableOne(
  vars = c(categorical_vars, numerical_vars,"disch_age_cat", "disch_age_rec"),
  data = clean_df,
  factorVars = c(categorical_vars,"disch_age_cat"),
  strata = "status",
  addOverall = TRUE,  # Incluir totales
  test = FALSE,        # Realizar pruebas de diferencias
  smd = TRUE          # Incluir SMDs
)


table_one_clean_b <- CreateTableOne(
  vars = c(categorical_vars, numerical_vars,"disch_age_cat", "disch_age_rec"),
  data = clean_df_b,
  factorVars = c(categorical_vars,"disch_age_cat"),
  strata = "status",
  addOverall = TRUE,  # Incluir totales
  test = FALSE,        # Realizar pruebas de diferencias
  smd = TRUE          # Incluir SMDs
)

clean_df|> 
  group_by(adm_age_cat)|> 
  summarise(min= min(adm_age_rec2), max= max(adm_age_rec2), sd= sd(adm_age_rec2), mean= mean(adm_age_rec2))
# 1 NA           29.0  60.0  9.92  37.2
invisible("Ahora ya no hay perdidos en la categoría de edad")

# Mostrar la tabla con perdidos
table_one_clean_print <- print(table_one_clean, 
                         nonnormal = c(numerical_vars, "disch_age_rec"), 
                         formatOptions = list(big.mark = ","),
                         quote = TRUE, 
                         noSpaces = TRUE,
                         showAllLevels = TRUE,  # Mostrar todos los niveles de variables categóricas
                         missing = FALSE,        # Incluir valores perdidos
                         explain = FALSE,        # Añadir explicación de las variables
                         digits = c(adm_age_rec2 = 1, dit_rec6 = 1, yr_fr_birth_date_rec = 1), smd = TRUE)

table_one_clean_print |> 
  knitr::kable("html", digits=1) |> 
  kableExtra::kable_classic()

table_one_clean_b_print <- print(table_one_clean_b, 
                               nonnormal = numerical_vars, 
                               formatOptions = list(big.mark = ","),
                               quote = TRUE, 
                               noSpaces = TRUE,
                               showAllLevels = TRUE,  # Mostrar todos los niveles de variables categóricas
                               missing = FALSE,        # Incluir valores perdidos
                               explain = FALSE,        # Añadir explicación de las variables
                               digits = c(adm_age_rec2 = 1, dit_rec6 = 1, yr_fr_birth_date_rec = 1), smd = TRUE)

table_one_clean_b_print |> 
  knitr::kable("html", digits=1) |> 
  kableExtra::kable_classic()


# Exportar a HTML o Word si es necesario
write.table(table_one_clean_print, file = paste0(getwd(),"/cons/_out/table_one_clean.txt"), sep = "\t", row.names = FALSE)
write.table(table_one_clean_b_print, file = paste0(getwd(),"/cons/_out/table_one_clean_b.txt"), sep = "\t", row.names = FALSE)

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#### Lexpand: 2025-june ----------------------------------------

c_SISTRAT_c1 <- lexpand( clean_df, 
              status = status, 
              birth = birth_date_rec, 
              exit = death_date_rec, entry = disch_date_rec6,
              #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
              breaks = list(per = seq(2010, 2021, by = 1), 
              #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                            age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
              aggre = list(agegroup = age, year = per, sex= sex_rec))

c_SISTRAT_c1_b <- lexpand( clean_df_b, 
                         status = status, 
                         birth = birth_date_rec, 
                         exit = death_date_rec, entry = disch_date_rec6,
                         breaks = list(per = seq(2010, 2021, by = 1), age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                         aggre = list(agegroup = age, year = per, sex= sex_rec))

c_SISTRAT_c1_lex <- Epi::Lexis(entry = list(FUT = 0, AGE = death_age_rec, CAL = get.yrs(disch_date_rec6)), 
           exit = list(CAL = get.yrs(death_date_rec)), 
           data = clean_df,
           exit.status = status)

# pyrs	Personas-año acumuladas en ese estrato.
# at.risk	Sujetos presentes al inicio del intervalo.
# from0to0	Transiciones 0 → 0 (sujetos que no experimentaron el evento).
# from0to1	Transiciones 0 → 1 (eventos) dentro del intervalo.

# Si la hazard rate λ es constante en todo el intervalo, el riesgo acumulado es.. 
# Es necesario obtener el Lambda/tasa
# Riesgo(t0, t)= 1-exp(-tasa(  t)) [diesel lung cancer life table_Taller_fin Original31.05.2024] 
# supongamos que en el intervalo [t0, t] medimos un riesgo acumulado R
# R <- 0.25    # 25% de probabilidad
# delta_t <- 2 # p. ej., 2 años

# calculamos la tasa constante lambda
# lambda <- - log(1 - R) / delta_t
# lambda

# La tasa es la probabilidad pequeñita de que algo pase en un paso (o unidad de tiempo).
# 
# El riesgo es la probabilidad acumulada de que pase al menos una vez tras varios pasos.
# 
# La fórmula usa el “exponencial” para juntar bien esas pequeñas probabilidades sin pasarse.
# Asumimos que la tasa no cambia con el tiempo (siempre hay un 10% en cada mínima unidad de tiempo de probabilidad de descontar[evento/morir/dado=6])
# En ese caso la tasa sería 1- (e^(-0.10[tasa por intento constante] x 3)=.74), es la probabilidad dado que no he experimentado el evento
# (para no hacer dobles cuentas de eventos, descartando el hecho de no haberlo experimentado hasta t)

#### Popmort; 2025-june ----------------------------------------

popmort_lt_cl <- cons_rate_sex_5x1|> # tu data-frame
  ## 1. Identificar edad inicial y amplitud n
  mutate(start_age = case_when(
    Age == "0"   ~ 0,
    Age == "1-4" ~ 1,
    TRUE         ~ as.numeric(str_extract(Age, "^\\d+"))
  ),
  n = case_when(
    Age == "0"   ~ 1,
    Age == "1-4" ~ 4,
    TRUE         ~ 5
  ))|>
  ## 2. Hazard λx del intervalo
  mutate(hazard = -log(1 - qx) / n,
         deaths  = lx * qx,                      # muertes en el intervalo
         exp_py  = deaths / hazard)              # exposición ≈ person-años

## 3. Reagrupar en los tramos pedidos
popmort_lt_cl_banded <- popmort_lt_cl|>
  mutate(band = case_when(
    start_age >= 18 & start_age < 30 ~ 18,
    start_age >= 30 & start_age < 45 ~ 30,
    start_age >= 45 & start_age < 60 ~ 45,
    start_age >= 60 & start_age < 76 ~ 60,
    TRUE                             ~ NA_real_
  ))|>
  filter(!is.na(band))|>
  group_by(Year, sex, band)|>
  summarise(
    deaths  = sum(deaths),
    exp_py  = sum(exp_py),
    hazard  = deaths / exp_py          # λ = D / PY #haz the average population 
    #mortality rate per person-year (d/(pyrs), where d is the number of deaths and pyrs is the person-years)
  )|>
  ungroup()|> 
  mutate(sex= ifelse(sex=="female","Female","Male"))|>
  rename("agegroup"="band", "year"="Year", "haz"="hazard")|> 
  select(sex, year, agegroup, haz)

#### SIR 1x1; 2025-june ----------------------------------------
cat("With ranges of 18+, but preserving 60-64 ")

#https://rpubs.com/antoine-kossi/sir-analysis-with-r

mx_1x1_banded <- mx_1x1_filt2|>
  mutate(
    year = Year,              # renombramos para que coincida con popmort
    agegroup = case_when(         # crea bandas con fcase() de tidytable
      Age >= 18 & Age <= 29~ 18,
      Age >= 30 & Age <= 44~ 30,
      Age >= 45 & Age <= 59~ 45,
      Age >= 60 & Age <= 76~ 60,
      T~ NA_real_
    )
  )|> 
  filter(!is.na(agegroup))|>    # descarta edades fuera de 18-65
  summarise(
    haz = mean(lambda_p_yrs, na.rm = TRUE),   # tasa media del grupo
    mx = mean(mx, na.rm=T),
    .by = c(year, sex, agegroup)
  )|>  
  arrange(year, sex, agegroup)|> 
  ungroup()|> 
  mutate(sex= ifelse(sex=="female","Female","Male"))


##### SIR 1x1; 2025-june; Total ----------------------------------------

sir( coh.data = c_SISTRAT_c1, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
     ref.data = mx_1x1_banded, 
     ref.rate = 'haz', 
     adjust = c('agegroup','year','sex'), 
     EAR=T)#Excess Absolute Risks

# Total sir: 3.53 (3.41-3.66)
# Total observed: 2996
# Total expected: 847.86
# Total person-years: 353826 
# 
# observed expected   pyrs   sir sir.lo sir.hi p_value   EAR
# <num>    <num>  <num> <num>  <num>  <num>   <num> <num>
#   1:     2996   847.86 353826  3.53   3.41   3.66       0 6.071

sir( coh.data = c_SISTRAT_c1_b, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
     ref.data = mx_1x1_banded, 
     ref.rate = 'haz', 
     adjust = c('agegroup','year','sex'), 
     EAR=T)#Excess Absolute Risks
#  Total sir: 3.9 (3.76-4.04)
#  Total observed: 3029
#  Total expected: 777.32
#  Total person-years: 317458 
# 
#    observed expected     pyrs   sir sir.lo sir.hi p_value   EAR
#       <num>    <num>    <num> <num>  <num>  <num>   <num> <num>
# 1:     3029   777.32 317457.9   3.9   3.76   4.04       0 7.093

xtabs(~agegroup, data= clean_df|> 
        mutate(age= round(disch_age_rec,0), agegroup = cut(
          disch_age_rec,                   # la variable de edad
          breaks = c(18, 30, 45, 60, 76), # límites (incluye 15, excluye 76+)
          right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
          labels = c("18-29", "30-44", "45-59", "60-64"),
          include.lowest = TRUE           # 15 entra en el primer tramo
        ), year= lubridate::year(as.Date(disch_date_num_rec6)))
)
# agegroup
# 18-29 30-44 45-59 60-64 
# 22977 31881 13739  1467 
xtabs(~sex_rec, data= clean_df|> 
       mutate(age= round(disch_age_rec,0), agegroup = cut(
         disch_age_rec,                   # la variable de edad
         breaks = c(18, 30, 45, 60, 76), # límites (incluye 15, excluye 76+)
         right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
         labels = c("18-29", "30-44", "45-59", "60-64"),
         include.lowest = TRUE           # 15 entra en el primer tramo
       ), year= lubridate::year(as.Date(disch_date_num_rec6)))
)
# sex_rec
# Male Female 
# 53331  16733 

cat("Crude mortality rates")
sprintf("%1.1f",biostat3::poisson.ci(2996, 353826)*1000)
#[1] "8.2" "8.8"
ci_cmr <- epitools::pois.exact(x= 2996,
                     pt     = 353826,
                     conf.level = 0.95) |> 
  mutate(across(c("rate", "lower", "upper"), ~sprintf("%1.1f",.*1000) ))
#1  2907 352202 8.3   8.0   8.6         0.95

poisson.test(2996, T = 353826, conf.level = 0.95)
# 95 percent confidence interval:
#   0.008166924 0.008776184
# sample estimates:
#   event rate 
# 0.008467439 

# $$
# % Tasa de mortalidad cruda (CMR)
# \[
#   \hat{\lambda}\;=\;\frac{D}{P}
#   \]
# 
# % Error estándar robusto a la sobredispersión (método delta, escala log)
# \[
#   \operatorname{SE}\!\bigl[\log(\hat{\lambda})\bigr]
#   \;=\;
#   \sqrt{\frac{\hat{\phi}}{D}}
#   \]
# 
# % Intervalo de confianza (1-\alpha) robusto
# \[
#   \text{IC}_{(1-\alpha)}
#   \;=\;
#   \exp\!\left\{
#     \log(\hat{\lambda})
#     \;\pm\;
#     z_{\,1-\alpha/2}
#     \sqrt{\frac{\hat{\phi}}{D}}
#     \right\}
#   \]
# 
# % Forma equivalente: límites inferior y superior
# \[
#   \bigl[L,\;U\bigr]
#   \;=\;
#   \hat{\lambda}\,
#   \exp\!\left(
#     \pm\;z_{\,1-\alpha/2}\sqrt{\frac{\hat{\phi}}{D}}
#     \right)
#   \]
# $$
# 𝐷 = número total de muertes observadas.
# 𝑃= años-persona de exposición.
# 𝜙= índice de dispersión estimado (φ > 1 indica sobredispersión).
# 𝑧_{1-𝛼/2} = cuantil estándar normal (por ejemplo, 1.96 para un IC 95 %).  

cat("Intervalos robustos")
overdisp_cmrs(deaths=2996 , pt=353826)
#   CMR_per_personyr Lower_95CI Upper_95CI
#   <chr>            <chr>      <chr>     
# 1 8.3              7.7        9.1    

cat("SMR corrected for overdisperssion")
do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(sir_ci_phi(D=2996, E=353826, phi=3.666913)[ , 1:3] * 1000))))
do.call(sprintf,c("%.2f (%.2f–%.2f)",as.list(unlist(sir_ci_phi(D=2996, E=847.86, phi=3.666913)[ , 1:3]))))

cat("Sensitivity analyses")


cat("SMR corrected for overdisperssion")
do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(sir_ci_phi(D=2996, E=353826, phi=3.666913)[ , 1:3] * 1000))))
do.call(sprintf,c("%.2f (%.2f–%.2f)",as.list(unlist(sir_ci_phi(D=2941, E=750.92, phi=3.666913)[ , 1:3]))))
Total observed: 3029
Total expected: 777.32
Total person-years: 317458 
##### SIR 1x1; 2025-june; Sex ----------------------------------------

sr_1_sex <- popEpi::sir(c_SISTRAT_c1, coh.obs = 'from0to1',
                  coh.pyrs = 'pyrs',
                  ref.data = mx_1x1_banded[ , c("sex", "year", "agegroup", "haz")],
                  ref.rate = haz,
                  print = c("sex"),
                  adjust = c("agegroup", "sex", "year"),
                  test.type = "homogeneity",
                  conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
                  conf.level = 0.95, EAR = T)
sr_1_sex
#       sex observed expected      pyrs   sir sir.lo sir.hi p_value   EAR
#    <char>    <num>    <num>     <num> <num>  <num>  <num>   <num> <num>
# 1: Female      600   111.36  85500.75  5.39   4.97   5.84       0 5.715
# 2:   Male     2396   736.50 268325.23  3.25   3.13   3.39       0 6.185

cat("Female\n")
do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(sir_ci_phi(D=600   , E=85500.75  , phi=3.666913)[ , 1:3] * 1000))))
do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(sir_ci_phi(D=600, E=111.36  , phi=3.666913)[ , 1:3]))))

cat("Male\n")
do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(sir_ci_phi(D=2396   , E=268325.23, phi=3.666913)[ , 1:3] * 1000))))
do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(sir_ci_phi(D=2396   , E=736.50 , phi=3.666913)[ , 1:3]))))

sr_1_sex_df <- sr_1_sex %>% 
  dplyr::mutate(
  CMR_1000 = purrr::map2_chr(observed, pyrs,
     ~ do.call(sprintf,
               c("%.1f (%.1f–%.1f)",
                 as.list(unlist(sir_ci_phi(..1, ..2, phi=3.666913)[, 1:3] * 1000))))),
  SMR      = purrr::map2_chr(observed, expected,
     ~ do.call(sprintf,
               c("%.2f (%.2f–%.2f)",
                 as.list(unlist(sir_ci_phi(..1, ..2, phi=3.666913)[, 1:3])))))
  )|> 
  select(sex, observed, pyrs, CMR_1000, expected, SMR, EAR)|>
  mutate(expected= round(expected,0), pyrs= round(pyrs, 0), EAR= as.character(sprintf("%.2f",EAR)))

sr_1_sex_df
#sr_1_sex_df |> arrange(desc(sex)) |>  rio::export("clipboard")


sr_1_sex_b <- popEpi::sir(c_SISTRAT_c1_b, coh.obs = 'from0to1',
                        coh.pyrs = 'pyrs',
                        ref.data = mx_1x1_banded[ , c("sex", "year", "agegroup", "haz")],
                        ref.rate = haz,
                        print = c("sex"),
                        adjust = c("agegroup", "sex", "year"),
                        test.type = "homogeneity",
                        conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
                        conf.level = 0.95, EAR = T)
sr_1_sex_b
# SIR (adjusted by agegroup, sex, year) with 95% confidence intervals (wald) 
# Test for homogeneity: p < 0.001 
# 
#  Total sir: 3.92 (3.78-4.06)
#  Total observed: 2941
#  Total expected: 750.92
#  Total person-years: 315801 
# 
# Clave <sex>
#       sex observed expected      pyrs   sir sir.lo sir.hi p_value   EAR
#    <char>    <num>    <num>     <num> <num>  <num>  <num>   <num> <num>
# 1: Female      588    97.71  74538.97  6.02   5.55   6.52       0 6.578
# 2:   Male     2353   653.21 241261.96  3.60   3.46   3.75       0 7.045

sr_1_sex_b_df <- sr_1_sex_b %>% 
  dplyr::mutate(
  CMR_1000 = purrr::map2_chr(observed, pyrs,
       ~ do.call(sprintf,
                 c("%.1f (%.1f–%.1f)",
                   as.list(unlist(sir_ci_phi(..1, ..2, phi=3.666913)[, 1:3] * 1000))))),
  SMR      = purrr::map2_chr(observed, expected,
       ~ do.call(sprintf,
                 c("%.2f (%.2f–%.2f)",
                   as.list(unlist(sir_ci_phi(..1, ..2, phi=3.666913)[, 1:3])))))
  )|> 
  select(sex, observed, expected, pyrs, EAR, CMR_1000, SMR)|>
  mutate(expected= round(expected,0), pyrs= round(pyrs, 0), EAR= as.character(sprintf("%.2f",EAR)))


sr_1_sex_b_df
#sr_1_sex_b_df |> arrange(desc(sex)) |>  rio::export("clipboard")

##### SIR 1x1; 2025-june; Age ----------------------------------------

sr_1_age <- popEpi::sir(c_SISTRAT_c1, coh.obs = 'from0to1',
                    coh.pyrs = 'pyrs',
                    ref.data = mx_1x1_banded[ , c("sex", "year", "agegroup", "haz")],
                    ref.rate = haz,
                    print = c("agegroup"),
                    adjust = c("agegroup", "sex", "year"),
                    test.type = "homogeneity",
                    conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
                    conf.level = 0.95, EAR = T)
sr_1_age
# Total sir: 3.53 (3.41-3.66)
# Total observed: 2996
# Total expected: 847.86
# Total person-years: 353826 
# 
# Clave <agegroup>
# agegroup observed expected      pyrs   sir sir.lo sir.hi p_value    EAR
# <num>    <num>    <num>     <num> <num>  <num>  <num>   <num>  <num>
# 1:       18      224    64.85  77124.89  3.45   3.03   3.94       0  2.064
# 2:       30     1070   279.35 183190.97  3.83   3.61   4.07       0  4.316
# 3:       45     1343   383.60  82118.63  3.50   3.32   3.69       0 11.683
# 4:       60      359   120.06  11391.48  2.99   2.70   3.32       0 20.976


sr_1_age_df <- sr_1_age %>% 
dplyr::mutate(
  CMR_1000 = purrr::map2_chr(observed, pyrs,
      ~ do.call(sprintf,
                c("%.1f (%.1f–%.1f)",
                  as.list(unlist(sir_ci_phi(..1, ..2, phi=3.666913)[, 1:3] * 1000))))),
  SMR      = purrr::map2_chr(observed, expected,
      ~ do.call(sprintf,
                c("%.2f (%.2f–%.2f)",
                  as.list(unlist(sir_ci_phi(..1, ..2, phi=3.666913)[, 1:3])))))
)|> 
  select(agegroup, observed, expected, pyrs, CMR_1000, SMR, EAR)|>
  mutate(expected= round(expected,0), pyrs= round(pyrs, 0), EAR= as.character(sprintf("%.2f",EAR)))

#sr_1_age_df |> rio::export("clipboard")

#Esto proporciona evidencia robusta para diseñar intervenciones de post-alta focalizadas: 
#alto volumen y prevención específica en hombres, y programas sensibles a género en mujeres, 
#con especial énfasis en las edades medias de la vida.


sr_1_age_b <- popEpi::sir(c_SISTRAT_c1_b, coh.obs = 'from0to1',
                        coh.pyrs = 'pyrs',
                        ref.data = mx_1x1_banded[ , c("sex", "year", "agegroup", "haz")],
                        ref.rate = haz,
                        print = c("agegroup"),
                        adjust = c("agegroup", "sex", "year"),
                        test.type = "homogeneity",
                        conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
                        conf.level = 0.95, EAR = T)
sr_1_age_b


sr_1_age_b_df <- sr_1_age_b %>% 
  dplyr::mutate(
    CMR_1000 = purrr::map2_chr(observed, pyrs,
                               ~ do.call(sprintf,
                                         c("%.1f (%.1f–%.1f)",
                                           as.list(unlist(sir_ci_phi(..1, ..2, phi=3.708736)[, 1:3] * 1000))))),
    SMR      = purrr::map2_chr(observed, expected,
                               ~ do.call(sprintf,
                                         c("%.2f (%.2f–%.2f)",
                                           as.list(unlist(sir_ci_phi(..1, ..2, phi=3.708736)[, 1:3])))))
  )|> 
  select(agegroup, observed, expected, pyrs, EAR, CMR_1000, SMR)|>
  mutate(expected= round(expected,0), pyrs= round(pyrs, 0), EAR= as.character(sprintf("%.2f",EAR)))

#sr_1_age_b_df |> rio::export("clipboard")

##### SIR 1x1; 2025-june; spline ----------------------------------------

st_1 <- sirspline( coh.data = c_SISTRAT_c1, coh.obs = 'from0to1', coh.pyrs = 'pyrs', 
                   ref.data = mx_1x1_banded, ref.rate = 'haz', 
                   adjust = c('agegroup','year','sex'),
                   spline = c('agegroup','year'), dependent.splines = TRUE)
st_1_lines<- extract_spline_data(st_1)
#plot(st_1, col=4, log=TRUE)
#title('Splines are dependent')


st_1_ind <- sirspline( coh.data = c_SISTRAT_c1, coh.obs = 'from0to1', coh.pyrs = 'pyrs', 
                   ref.data = mx_1x1_banded, ref.rate = 'haz', 
                   adjust = c('agegroup','year','sex'),
                   spline = c('agegroup','year'), dependent.splines = F)
st_1_ind_lines<- extract_spline_data(st_1_ind)
#plot(st_1_ind, col=4, log=TRUE)
#title('Splines are independent')

smrspline_data<- 
rbind.data.frame(
cbind.data.frame(type="dependent",st_1_lines),
cbind.data.frame(type="independent",st_1_ind_lines)
                 )

cat("Plot")
ggplot(smrspline_data, aes(x = spline_value, y = estimate, color = factor(type))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = factor(type)), alpha = 0.3) +
  facet_wrap(~ spline, scales = "free_x") +
  labs(x = "Spline Value", y = "log(SMR)", color = "All splines in the same model", fill = "All splines in the same model") +
  theme_sjPlot_manual()+
  scale_fill_manual(values = c("gray60", "lightblue")) +
  scale_color_manual(values = c("gray60", "lightblue")) +
  theme(legend.position="bottom")+
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")+#smallest value is the reference point (where SIR = 1)
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),       # Quitar grids
    panel.background = element_blank(), # Fondo blanco sin líneas
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas eje x 45 grados
    strip.text = element_text(face = "bold", size=12)
    )


#El riesgo relativo es más alto en la mediana edad (≈40 años) y disminuye en los mayores,
#pero nunca baja de 2 × lo esperado. Las bandas de confianza (ribbons) se solapan ampliamente, 
#por lo que las diferencias entre grupos difícilmente sean significativas.

#Edad: mayor exceso en la mediana edad; disminución con ≥50 años.

#Independent presenta un exceso crónico y pronunciado a lo largo de la década, con pico en 2015 y 
#ligera mejoría posterior.
#Dependent arranca sin exceso, lo adquiere gradualmente hasta mediados de la década y vuelve 
#a aproximarse a la expectativa hacia 2020.
#Las bandas no se solapan entre grupos en varios años tempranos, sugiriendo diferencias 
#estadísticamente relevantes.

#Tiempo: exceso persistente y alto en independent; exceso menor y transitorio en dependent.


#### SIR 1x1; 2025-june; Direct  ---------------------------

##### SIR 1x1; 2025-june; Direct; format  ---------------------------

cat("Format population data")

proy_ine_reg_group_25_june<-
  proy_ine_com|> 
  #2025 to resemble SER 2024, BUT NOW 18+ INSTEAD OF 15
  #2025-06-11: I need to expand so people older that stayed in treatment could fit
  dplyr::filter(Edad>=18, Edad<76)|> 
  #format to match previous
  dplyr::mutate(reg_res= sprintf("%02d", Region))|> 
  dplyr::ungroup()|> 
  dplyr::mutate(edad_anos_rec= dplyr::case_when(Edad>=18 & Edad<30~18,
                                                Edad>=30 & Edad<45~30,
                                                Edad>=45 & Edad<60~45,
                                                Edad>=60 & Edad<76~60,
                                                T~NA_real_))|>  
  dplyr::group_by(anio, `Sexo (1=Hombre 2=Mujer)`,edad_anos_rec)|>
  dplyr::summarise(poblacion= sum(poblacion, na.rm=T))|> 
  dplyr::rename("sex"="Sexo (1=Hombre 2=Mujer)", "agegroup"="edad_anos_rec", "year"="anio")|> 
  dplyr::mutate(sex= ifelse(sex==2, "Female", "Male"))
 
# In the case that you employ more than one adjusting variable, separate weights should be passed to
# match to the levels of the different adjusting variables. When supplied correctly, "grand" weights
# are formed based on the variable-specific weights by multiplying over the variable-specific weights
# (e.g. if men have w = 0.5 and the age group 0-4 has w = 0.1, the "grand" weight for men aged 0-4
#   is 0.5*0.1). The "grand" weights are then used for adjusting after ensuring they sum to one.
# When using multiple adjusting variables, you are allowed to pass either a named list of weights
# or a data.frame of weights. E.g.
# WL <- list(agegroup = age_w, sex = sex_w) 
proy_ine_reg_group_25_june_sex<-
proy_ine_reg_group_25_june|>  
  #filter(year == 2020)|>  
  summarise(pop = sum(poblacion), .by = sex) |> 
  mutate(w=pop/sum(pop))
proy_ine_reg_group_25_june_age<-
  proy_ine_reg_group_25_june|>  
  #filter(year == 2020)|>  
  summarise(pop = sum(poblacion), .by = agegroup) |> 
  mutate(w=pop/sum(pop))
proy_ine_reg_group_25_june_year<-
  proy_ine_reg_group_25_june|>  
  #filter(year == 2020)|>  
  summarise(pop = sum(poblacion), .by = year) |> 
  mutate(w=pop/sum(pop))

WL <- list(
  year=proy_ine_reg_group_25_june_year$w, 
           agegroup = proy_ine_reg_group_25_june_age$w, 
           sex = proy_ine_reg_group_25_june_sex$w) 

proy_ine_reg_group_25_june_every<- 
proy_ine_reg_group_25_june|>  
  group_by(year, agegroup, sex)|> 
  summarise(pop = sum(poblacion))|> 
  ungroup()|> 
  mutate(weights=pop/sum(pop))
weights_df <- data.table::as.data.table(proy_ine_reg_group_25_june_every)[
  , .(year        = as.integer(year),               # num o int
      agegroup    = as.integer(agegroup),
      sex         = factor(sex, levels = c("Male","Female")),  # ¡factor!
      weights     = as.numeric(weights))            # num
]


##### SIR 1x1; 2025-june; Direct; estimate  ---------------------------

r2 <- rate( data = c_SISTRAT_c1, 
            obs = from0to1, 
            pyrs = pyrs, 
            #print = year,
            adjust = c("year", "agegroup", "sex"),
            weights = weights_df
            )
r2
r2_adj <- rate(
  data    = c_SISTRAT_c1,
  obs     = from0to1,
  pyrs    = pyrs,
  #print   = year,
  adjust  = c("year", "sex", "agegroup"),
  weights = WL
)
r2_adj


DSR_1k <- mapply(
  dsr_format,                 # FUN
  r2$rate.adj,           # primer vector (rate)
  r2$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 3.666913,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))


#DSR_1k|> data.frame()|>  rio::export("clipboard")
#[1] "10.1 (5.4–14.9)"


DSR_1k_corr <- mapply(
  dsr_format_corr,                 # FUN
  r2$rate.adj,           # primer vector (rate)
  r2$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 1,#3.708736,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))
#[1] "10.1 (6.3–16.2)"

invisible("Al final son casi lo mismo")


cat("B) Last treatment")

r2_b <- rate( data = c_SISTRAT_c1_b, 
            obs = from0to1, 
            pyrs = pyrs, 
            #print = year,
            adjust = c("year", "agegroup", "sex"),
            weights = weights_df
)
r2_b
DSR_1k_corr_b <- mapply(
  dsr_format_corr,                 # FUN
  r2_b$rate.adj,           # primer vector (rate)
  r2_b$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 1,#3.708736,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))


###### SIR 1x1; 2025-june; Direct; estimate; sex  ---------------------------


r2_sex <- rate( data = c_SISTRAT_c1, 
            obs = from0to1, 
            pyrs = pyrs, 
            print = "sex",
            adjust = c("year", "agegroup"),
            weights = list(
              year=proy_ine_reg_group_25_june_year$w, 
              agegroup = proy_ine_reg_group_25_june_age$w 
              #sex = proy_ine_reg_group_25_june_sex$w) 
            )
)

DSR_1k_sex <- mapply(
  dsr_format,                 # FUN
  r2_sex$rate.adj,           # primer vector (rate)
  r2_sex$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 3.666913,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))

#DSR_1k_sex|> data.frame()|>  rio::export("clipboard")

#Fay & Feuer (1997). Confidence intervals for directly standardized rates: a 
#method based on the gamma distribution. Stat Med 16:791-801.


DSR_1k_corr_sex <- mapply(
  dsr_format_corr,                 # FUN
  r2_sex$rate.adj,           # primer vector (rate)
  r2_sex$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 3.666913,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))

cat("B) Last treatment")


r2_sex_b <- rate( data = c_SISTRAT_c1_b, 
                obs = from0to1, 
                pyrs = pyrs, 
                print = "sex",
                adjust = c("year", "agegroup"),
                weights = list(
                  year=proy_ine_reg_group_25_june_year$w, 
                  agegroup = proy_ine_reg_group_25_june_age$w 
                  #sex = proy_ine_reg_group_25_june_sex$w) 
                )
)

DSR_1k_corr_sex_b <- mapply(
  dsr_format_corr,                 # FUN
  r2_sex_b$rate.adj,           # primer vector (rate)
  r2_sex_b$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 3.708736,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))

###### SIR 1x1; 2025-june; Direct; estimate; age  ---------------------------

r2_agegr <- rate( data = c_SISTRAT_c1, 
                obs = from0to1, 
                pyrs = pyrs, 
                print = "agegroup",
                adjust = c("year", "sex"),
                weights = list(
                  year=proy_ine_reg_group_25_june_year$w, 
                  #agegroup = proy_ine_reg_group_25_june_age$w 
                  sex = proy_ine_reg_group_25_june_sex$w) 
                )


DSR_1k_agegr <- mapply(
  dsr_format,                 # FUN
  r2_agegr$rate.adj,           # primer vector (rate)
  r2_agegr$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 3.666913,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))

#DSR_1k_agegr|> data.frame()|>  rio::export("clipboard")



DSR_1k_corr_agegr <- mapply(
  dsr_format_corr,                 # FUN
  r2_agegr$rate.adj,           # primer vector (rate)
  r2_agegr$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 3.666913,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))
#DSR_1k_corr_agegr|> data.frame()|>  rio::export("clipboard")


cat("B)")

r2_agegr_b <- rate( data = c_SISTRAT_c1_b, 
                  obs = from0to1, 
                  pyrs = pyrs, 
                  print = "agegroup",
                  adjust = c("year", "sex"),
                  weights = list(
                    year=proy_ine_reg_group_25_june_year$w, 
                    #agegroup = proy_ine_reg_group_25_june_age$w 
                    sex = proy_ine_reg_group_25_june_sex$w) 
)

DSR_1k_corr_agegr_b <- mapply(
  dsr_format_corr,                 # FUN
  r2_agegr_b$rate.adj,           # primer vector (rate)
  r2_agegr_b$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = 3.708736,
    factor = 1e3,
    digits = 1,
    conf   = 0.95))
#DSR_1k_corr_agegr_b|> data.frame()|>  rio::export("clipboard")


###### SIR 1x1; 2025-june; Direct; estimate; age  ---------------------------



##### SIR 1x1; 2025-june; Direct; SRR  ---------------------------

#El DEIS/INE solo dispone del censo o la proyección de habitantes a mitad de año. 
#Si asumimos que cada persona “aporta” 1 año durante ese año calendario, el total 
#de habitantes ≈ person-years poblacionales.

tablas_de_mortalidad_de_chile_1992_2050_anio <- 
  readxl::read_excel(paste0(base_path, "tablas-de-mortalidad-de-chile-1992-2050.xlsx"), 
                     sheet = "BD Tablas de Mortalidad", skip = 1)|> 
  janitor::clean_names()|> 
  dplyr::filter(ano>=2010, ano<=2020)|> 
  dplyr::mutate(edad= readr::parse_number(edad))|> 
  dplyr::filter(edad>=18, edad<65, region=="País")|>  
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad>=18 & edad<30~1,
                                                edad>=30 & edad<45~2,
                                                edad>=45 & edad<60~3,
                                                edad>=60 & edad<65~4,T~NA_real_))|>  
  dplyr::mutate(adm_age_cat= case_when(edad_anos_rec==1~18, 
                                         edad_anos_rec==2~30, 
                                         edad_anos_rec==3~45, 
                                         edad_anos_rec==4~60,T~NA_real_))|> 
  dplyr::rename("agegroup"="adm_age_cat", "year"="ano")|> 
  dplyr::mutate(sex= ifelse(sexo=="Hombre","Male", "Female"))|> 
  dplyr::select(-cod_region, -region, -sexo)|> 
  dplyr::group_by(year, agegroup, sex)|> 
  dplyr::summarise(
    total_d_x = sum(d_x, na.rm = TRUE),      # Suma de muertes en el grupo
    total_l_x = sum(l_x, na.rm = TRUE),      # Suma de la población al inicio del grupo
    mean_m_x = mean(m_x),
    mortality_rate_grouped = total_d_x / total_l_x
  )|> 
  left_join(proy_ine_reg_group_25_june_every, by=c("year", "agegroup", "sex"))




w_df <- tablas_de_mortalidad_de_chile_1992_2050_anio %>%
  mutate(sex = factor(sex, levels = levels(c_SISTRAT_c1$sex))) %>%  # factor 'Male'/'Female'
  rename(weights = weights) |>                                           # nombre exigido por rate()
  select(year, agegroup, sex, weights)
  
# 2. Ejecuta rate() sobre la tabla nacional
nat_ASMR <- rate(
  data    = tablas_de_mortalidad_de_chile_1992_2050_anio,
  obs     = total_d_x,
  pyrs    = total_l_x,                 # usa los persona-años (mejor que pop)
  adjust  = c("year", "agegroup", "sex"),
  weights = w_df                       # mismos pesos para cohorte y nación
)



rate_coh <- r2$rate.adj              # 0.01014016
se_coh   <- r2$SE.rate.adj           # 0.00126195
rate_nat <- nat_ASMR$rate.adj        # 0.00244265
se_nat   <- nat_ASMR$SE.rate.adj     # 0.00000474

### 1.  Escala las tasas a ×1 000 PY 
mult <- 1000                         # factor de escala

rate_tbl <- tibble(
  grupo      = c("Cohorte", "Chile"),
  tasa_1k    = c(rate_coh,    rate_nat)   * mult,
  se_1k      = c(se_coh,      se_nat)     * mult,
  ic_low_1k  = tasa_1k - 1.96 * se_1k,
  ic_hi_1k   = tasa_1k + 1.96 * se_1k
)
### 2.  Exceso absoluto (cohorte – Chile) 
exceso_abs <- (rate_coh - rate_nat) * mult         # muertes extra/1 000 PY
se_exceso  <- sqrt(se_coh^2 + se_nat^2) * mult
ic_exc_lo  <- exceso_abs - 1.96 * se_exceso
ic_exc_hi  <- exceso_abs + 1.96 * se_exceso

### 3.  Delta method, SRR directo y and CI 95 % 
SRR   <- rate_coh / rate_nat
#
se_SRR <- SRR * sqrt( (se_coh / rate_coh)^2 + (se_nat / rate_nat)^2 )
ic_SRR <- SRR * exp(c(-1, 1) * 1.96 * se_SRR / SRR)

### 4.  Final table 
tabla_final <- data.table::rbindlist(list(
  rate_tbl,
  tibble(
    grupo      = "Exceso absoluto",
    tasa_1k    = exceso_abs,
    se_1k      = se_exceso,
    ic_low_1k  = ic_exc_lo,
    ic_hi_1k   = ic_exc_hi
  ),
  tibble(
    grupo      = "SRR directo",
    tasa_1k    = SRR,          # SRR is not multiplied
    se_1k      = se_SRR,
    ic_low_1k  = ic_SRR[1],
    ic_hi_1k   = ic_SRR[2]
  )
)
)

print(tabla_final[, .(grupo,
                      estimate = round(tasa_1k, 3),
                      CI95     = sprintf("%.3f – %.3f", ic_low_1k, ic_hi_1k))])




## SMRs- 2025jan; updated --------------------------------------------------------------------


clean_df_survset<-
  clean_df|> 
  mutate(timesurv_disch= case_when(
    status==1~ as.numeric(difftime(death_date_rec,  disch_date_rec6, units="days")/365.241),
    status==0~ as.numeric(difftime(as.Date("2020-12-31"),  disch_date_rec6, units="days")/365.241)))|>
  mutate(adx= disch_age_rec,
         astart= adx, 
         death_age_rec= adx+timesurv_disch,
         astop= death_age_rec)|> 
  mutate(
    ydx = lubridate::year(disch_date_rec6) + (yday(disch_date_rec6) - 1) /
      ifelse(leap_year(year(disch_date_rec6)), 366, 365)
  )

biostat3::survRate(Surv(clean_df_corr_surv$follow_up_days/365.25, status==1)~1, data=clean_df_corr_surv)|>  
  dplyr::mutate(across(c("rate", "lower", "upper"),~sprintf("%1.1f",.*1000)))


psych::describeBy(clean_df_survset$timesurv_disch, group=clean_df_survset$status, quant = c(.25, .75))
# Descriptive statistics by group 
# group: 0
# vars     n mean  sd median trimmed  mad min   max range skew kurtosis   se Q0.25 Q0.75
# X1    1 67208 5.14 2.8   4.99    5.09 3.29   0 10.97 10.97 0.13    -0.99 0.01  2.84  7.29
# ---------------------------------------------------------------------------------------------------------------------------------- 
#   group: 1
# vars    n mean   sd median trimmed  mad min  max range skew kurtosis   se Q0.25 Q0.75
# X1    1 3006  3.3 2.37   2.85     3.1 2.51   0 10.4  10.4 0.64     -0.4 0.04  1.36  4.89

psych::describe(clean_df_survset$timesurv_disch, quant = c(.25, .75))
#    vars     n mean  sd median trimmed  mad min   max range skew kurtosis   se Q0.25 Q0.75
# X1    1 70064 5.05 2.8    4.9    4.99 3.29   0 10.97 10.97 0.16    -0.98 0.01  2.76  7.19

clean_df_survset_group<- 
  clean_df_survset|>
  filter(disch_age_rec>17, disch_age_rec<65)|> 
  mutate(age= round(disch_age_rec,0), agegroup = cut(
    disch_age_rec,                   # la variable de edad
    breaks = c(18, 30, 45, 60, 65), # límites (incluye 15, excluye 65)
    right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
    labels = c("18-29", "30-44", "45-59", "60-64"),
    include.lowest = TRUE           # 15 entra en el primer tramo
  ), year= lubridate::year(as.Date(disch_date_num_rec6)))|> 
  filter(!is.na(agegroup))|> 
  group_by(year, agegroup, sex_rec)|>
  summarise(pt= sum(timesurv_disch, na.rm=T), observed= sum(status==1, na.rm=T))|>
  ungroup()|> 
  mutate(agegroup= readr::parse_number(substr(agegroup, 1, 2)), 
         sex_rec= as.character(sex_rec))

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

cat("Generated a population dataframe")


proy_ine_com_18_64<-
  rio::import("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/ine_estimaciones-y-proyecciones-2002-2035_base-2017_comunas0381d25bc2224f51b9770a705a434b74.csv?sfvrsn=b6e930a7_3&download=true")|> 
  tidyr::pivot_longer(cols = dplyr::starts_with("Poblacion"), 
                      names_to = "anio", 
                      values_to = "poblacion")|> 
  dplyr::mutate(anio= gsub("Poblacion ","",anio), anio=as.numeric(anio))|> 
  dplyr::filter(anio>=2010 & anio<=2020, Edad>=18 & Edad<65)|> 
  dplyr::mutate(sex_rec= ifelse(`Sexo (1=Hombre 2=Mujer)`==2,"female","male"))|> 
  group_by(anio, Edad, sex_rec)|>
  summarise(pop=sum(poblacion, na.rm=T))|>
  ungroup()

cat("Mortality database now have a weighted m_x by population")
tablas_de_mortalidad_de_chile_1992_2050_group_18_64 <- 
  readxl::read_excel(paste0(base_path, "tablas-de-mortalidad-de-chile-1992-2050.xlsx"), 
                     sheet = "BD Tablas de Mortalidad", skip = 1)|> 
  janitor::clean_names()|> 
  dplyr::filter(ano>=2010, ano<=2020)|> 
  dplyr::mutate(edad= readr::parse_number(edad))|> 
  dplyr::filter(edad>=18, edad<65, region=="País")|>  
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad>=18 & edad<30~18,
                                                edad>=30 & edad<45~30,
                                                edad>=45 & edad<60~45,
                                                edad>=60 & edad<76~60,T~NA_real_))|>
  dplyr::mutate(sex_rec= ifelse(sexo=="Mujer","female","male"))|> 
  left_join(proy_ine_com_18_64, by= c("ano"="anio", "edad"="Edad", "sex_rec"="sex_rec"))|>
  dplyr::group_by(ano, sex_rec, edad_anos_rec)|> 
  dplyr::summarise(
    total_d_x = sum(d_x, na.rm = TRUE),      # Suma de muertes en el grupo
    total_l_x = sum(l_x, na.rm = TRUE),      # Suma de la población al inicio del grupo (debiese ser con años-persona)
    sumpop= sum(pop),
    mean_m_x = mean(m_x),
    mean_m_x_w = weighted.mean(m_x, w = pop),
    mortality_rate_grouped = total_d_x / total_l_x
  )|> 
  dplyr::mutate(sex_rec= ifelse(sex_rec=="female","Female","Male"))|>
  ungroup()

#### SMR- adm date 08-01 -------------------------------------------------------
invisible("Ojo que aquí estamos viendo otra cosa, porque estmaos viendo el tiempo de exposición")
invisible("EN los otros estamos viendo no como parte del estudio, como el tiempo de exposición")

sr_tot <- biostat3::survRate(Surv(timesurv_disch, status==1) ~ 1, data=clean_df_survset|> 
                           mutate(agegroup = cut(
                             disch_age_rec,                   # la variable de edad
                             breaks = c(18, 30, 45, 60, 65), # límites (incluye 15, excluye 65)
                             right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
                             labels = c("18-29", "30-44", "45-59", "60-64"),
                             include.lowest = TRUE           # 18 entra en el primer tramo
                           )
                           )
)

sr <- biostat3::survRate(Surv(timesurv_disch, status==1) ~ sex_rec + 
                           agegroup, data=clean_df_survset|> 
                           mutate(agegroup = cut(
                             disch_age_rec,                   # la variable de edad
                             breaks = c(18, 30, 45, 60, 65), # límites (incluye 15, excluye 65)
                             right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
                             labels = c("18-29", "30-44", "45-59", "60-64"),
                             include.lowest = TRUE           # 18 entra en el primer tramo
                           )
                           )
                         )
# sex_rec=Male, agegroup=18-29      Male    18-29  871621.55   318 0.0003648372 0.0003258357 0.0004072219
# sex_rec=Male, agegroup=30-44      Male    30-44 1243297.06   925 0.0007439895 0.0006968107 0.0007935220
# sex_rec=Male, agegroup=45-59      Male    45-59  543708.03  1005 0.0018484185 0.0017358917 0.0019663252
# sex_rec=Male, agegroup=60-64      Male    60-64   54374.02   143 0.0026299324 0.0022165599 0.0030980224
# sex_rec=Female, agegroup=18-29  Female    18-29  301202.86    96 0.0003187221 0.0002581659 0.0003892142
# sex_rec=Female, agegroup=30-44  Female    30-44  382329.12   259 0.0006774268 0.0005974301 0.0007651515
# sex_rec=Female, agegroup=45-59  Female    45-59  154783.74   213 0.0013761135 0.0011975022 0.0015738532
# sex_rec=Female, agegroup=60-64  Female    60-64   13274.21    31 0.0023353560 0.0015867611 0.0033148514

sr_porsex <- biostat3::survRate(Surv(timesurv_disch, status==1) ~ sex_rec  
                                , data= clean_df_survset|> 
                                  mutate(agegroup = cut(
                                    disch_age_rec,                   # la variable de edad
                                    breaks = c(18, 30, 45, 60, 65), # límites (incluye 15, excluye 65)
                                    right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
                                    labels = c("18-29", "30-44", "45-59", "60-64"),
                                    include.lowest = TRUE           # 15 entra en el primer tramo
                                  )
                                  ) |> filter(timesurv_disch)
)
#               sexo     tstop event       rate      lower      upper
# sexo=hombre hombre 4018.1633   156 0.03882371 0.03297042 0.04541659
# sexo=mujer   mujer  581.3011    16 0.02752446 0.01573261 0.04469800


sr_poredadsex <- biostat3::survRate(Surv(timesurv_disch, status==1) ~ strata(sex_rec)+ 
                           strata(agegroup), data=clean_df_survset|> 
                           mutate(agegroup = cut(
                             disch_age_rec,                   # la variable de edad
                             breaks = c(18, 30, 45, 60, 65), # límites (incluye 15, excluye 65)
                             right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
                             labels = c("18-29", "30-44", "45-59", "60-64"),
                             include.lowest = TRUE           # 15 entra en el primer tramo
                           )
                           )
)
# strata(sex_rec) strata(agegroup)      tstop event         rate        lower        upper
# strata(sex_rec)=Male, strata(agegroup)=18-29              Male            18-29  871621.55   318 0.0003648372 0.0003258357 0.0004072219
# strata(sex_rec)=Male, strata(agegroup)=30-44              Male            30-44 1243297.06   925 0.0007439895 0.0006968107 0.0007935220
# strata(sex_rec)=Male, strata(agegroup)=45-59              Male            45-59  543708.03  1005 0.0018484185 0.0017358917 0.0019663252
# strata(sex_rec)=Male, strata(agegroup)=60-64              Male            60-64   54374.02   143 0.0026299324 0.0022165599 0.0030980224
# strata(sex_rec)=Female, strata(agegroup)=18-29          Female            18-29  301202.86    96 0.0003187221 0.0002581659 0.0003892142
# strata(sex_rec)=Female, strata(agegroup)=30-44          Female            30-44  382329.12   259 0.0006774268 0.0005974301 0.0007651515
# strata(sex_rec)=Female, strata(agegroup)=45-59          Female            45-59  154783.74   213 0.0013761135 0.0011975022 0.0015738532
# strata(sex_rec)=Female, strata(agegroup)=60-64          Female            60-64   13274.21    31 0.0023353560 0.0015867611 0.0033148514
 
sr_poredadsexanio <- biostat3::survRate(Surv(timesurv_disch, status==1) ~ strata(sex_rec)+ 
                                      strata(agegroup)+ strata(year), data=clean_df_survset |> 
                                      mutate(agegroup = cut(
                                        disch_age_rec,                   # la variable de edad
                                        breaks = c(18, 30, 45, 60, 65), # límites (incluye 15, excluye 65)
                                        right  = FALSE,                 # intervalo izquierdo cerrado  [15–30)
                                        labels = c("18-29", "30-44", "45-59", "60-64"),
                                        include.lowest = TRUE           # 15 entra en el primer tramo
                                      ), 
                                      year= lubridate::year(as.Date(disch_date_num_rec6))
                                      )|> 
                                        filter(disch_age_rec>17, disch_age_rec<64)
)


sr_df_year_age_sex<- 
cbind.data.frame(sex= sr_poredadsexanio$`strata(sex_rec)`,
                 agegroup= readr::parse_number(substr(sr_poredadsexanio$`strata(agegroup)`, 1, 2)),
                 year= readr::parse_number(substr(sr_poredadsexanio$`strata(year)`,6,9)),
                 pt= sr_poredadsexanio$tstop, 
                 observed= sr_poredadsexanio$event)

#tablas_de_mortalidad_de_chile_1992_2050_group_18_64
joint <- sr_df_year_age_sex|> 
  left_join(tablas_de_mortalidad_de_chile_1992_2050_group_18_64[, c("ano", "sex_rec", "edad_anos_rec","mean_m_x_w", "mean_m_x", "sumpop")], 
            by= c("year"="ano","sex"="sex_rec","agegroup"="edad_anos_rec"))|>
  mutate(expected = pt * mean_m_x_w)


joint|> 
  summarise(
    Observed = sum(observed),
    Expected = sum(expected)
  )|>
  rowwise()|>
  dplyr::mutate(
    # Reemplazar Expected == 0 con un valor pequeño
    Expected = ifelse(Expected == 0, 1e-5, Expected),
    Observed = ifelse(Observed == 0, 1e-5, Observed),
    SMR = Observed / Expected,
    lo = biostat3::poisson.ci(Observed, Expected)[1],
    up = biostat3::poisson.ci(Observed, Expected)[2]
  )|>
  ungroup()
invisible("SMR sin ponderar haz")
#   Observed Expected   SMR lo$`` up$``
#      <dbl>    <dbl> <dbl> <dbl> <dbl>
# 1     2969     703.  4.22  4.07  4.38

#   Observed Expected   SMR lo$`` up$``
#      <dbl>    <dbl> <dbl> <dbl> <dbl>
# 1     2969     690.  4.30  4.15  4.46

clean_df_survset_group|> 
  left_join(mx_1x1_banded, by= c("year"="year","sex_rec"="sex","agegroup"="agegroup"))|>
  mutate(expected = pt * haz) |> 
  summarise(
    Observed = sum(observed),
    Expected = sum(expected)
  )|>
  rowwise()|>
  dplyr::mutate(
    # Reemplazar Expected == 0 con un valor pequeño
    Expected = ifelse(Expected == 0, 1e-5, Expected),
    Observed = ifelse(Observed == 0, 1e-5, Observed),
    SMR = Observed / Expected,
    lo = biostat3::poisson.ci(Observed, Expected)[1],
    up = biostat3::poisson.ci(Observed, Expected)[2]
  )|>
  ungroup()
#   Observed Expected   SMR lo$`` up$``
#      <int>    <dbl> <dbl> <dbl> <dbl>
# 1     2990     704.  4.25  4.09  4.40


cat("Son bien similares, pero un SMR SUPERIOR al de lexpand popEpi")



SMR_bysex <- group_by(joint, sex) |> 
  summarise(
    Observed = sum(observed),
    Expected = sum(expected)
  )|>
  rowwise()|>
  dplyr::mutate(
    # Reemplazar Expected == 0 con un valor pequeño
    Expected = ifelse(Expected == 0, 1e-5, Expected),
    Observed = ifelse(Observed == 0, 1e-5, Observed),
    SMR = Observed / Expected,
    lo = biostat3::poisson.ci(Observed, Expected)[1],
    up = biostat3::poisson.ci(Observed, Expected)[2]
  )|>
  ungroup()
SMR_bysex
#   sex    Observed Expected   SMR lo$`` up$``
#   <chr>     <dbl>    <dbl> <dbl> <dbl> <dbl>
# 1 Female      595     87.0  6.84  6.30  7.41
# 2 Male       2374    603.   3.93  3.78  4.10

SMR_byedad <- group_by(joint, agegroup ) |> 
  summarise(
    Observed = sum(observed),
    Expected = sum(expected)
  )|>
  rowwise()|>
  dplyr::mutate(
    # Reemplazar Expected == 0 con un valor pequeño
    Expected = ifelse(Expected == 0, 1e-5, Expected),
    Observed = ifelse(Observed == 0, 1e-5, Observed),
    SMR = Observed / Expected,
    lo = biostat3::poisson.ci(Observed, Expected)[1],
    up = biostat3::poisson.ci(Observed, Expected)[2]
  )|>
  ungroup()
SMR_byedad
#   agegroup Observed Expected   SMR lo$`` up$``
#      <dbl>    <dbl>    <dbl> <dbl> <dbl> <dbl>
# 1       18      414    107.   3.87  3.51  4.26
# 2       30     1184    257.   4.60  4.35  4.87
# 3       45     1218    282.   4.31  4.08  4.56
# 4       60      153     44.1  3.47  2.94  4.07


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
cat("Ahora matcheandolo por edades 1 a 1")


clean_df_survset_group1<- 
clean_df_survset|>
  filter(adm_age_rec2>=18, adm_age_rec2<65)|> 
  mutate(age= round(disch_age_rec,0), year= lubridate::year(as.Date(disch_date_num_rec6)))|> 
  group_by(year, age, sex_rec)|>
  summarise(pt= sum(timesurv_disch, na.rm=T), observed= sum(status==1, na.rm=T))

joint1 <- clean_df_survset_group1|> 
  mutate(sex_rec= ifelse(sex_rec=="Male", "male", "female"))|> 
  left_join(mx_1x1_filt2, by= c("year"="Year","sex_rec"="sex","age"="Age"))|>
  mutate(expected = pt * mx)

joint1|>
  ungroup() |> 
  filter(age>17)|> 
  summarise(
    Observed = sum(observed, na.rm=T),
    Expected = sum(expected, na.rm=T)
  )|>
  rowwise()|>
  dplyr::mutate(
    # Reemplazar Expected == 0 con un valor pequeño
    Expected = ifelse(Expected == 0, 1e-5, Expected),
    Observed = ifelse(Observed == 0, 1e-5, Observed),
    SMR = Observed / Expected,
    lo = biostat3::poisson.ci(Observed, Expected)[1],
    up = biostat3::poisson.ci(Observed, Expected)[2]
  )|>
  ungroup()
#   Observed Expected   SMR lo$`` up$``
#      <int>    <dbl> <dbl> <dbl> <dbl>
# 1     3006     672.  4.47  4.31  4.64


SMR_bysex1 <- group_by(joint1, sex_rec) |> 
  summarise(
    Observed = sum(observed, na.rm=T),
    Expected = sum(expected, na.rm=T)
  )|>
  rowwise()|>
  dplyr::mutate(
    # Reemplazar Expected == 0 con un valor pequeño
    Expected = ifelse(Expected == 0, 1e-5, Expected),
    Observed = ifelse(Observed == 0, 1e-5, Observed),
    SMR = Observed / Expected,
    lo = biostat3::poisson.ci(Observed, Expected)[1],
    up = biostat3::poisson.ci(Observed, Expected)[2]
  )|>
  ungroup()
SMR_bysex1
#  sex_rec Observed Expected   SMR lo$`` up$``
#   <chr>      <int>    <dbl> <dbl> <dbl> <dbl>
# 1 female       602     83.2  7.23  6.67  7.83
# 2 male        2404    589.   4.08  3.92  4.25

invisible("Did not make the rest because they have many categories and it makes losing time")

#### SMR- survrate, poisson -------------------------------------------------------------

invisible("Ojo que aquí estamos viendo otra cosa, porque estmaos viendo el tiempo de exposición")
invisible("EN los otros estamos viendo no como parte del estudio, como el tiempo de exposición")

#https://stats.stackexchange.com/questions/466404/poisson-regression-for-modeling-standardized-mortality-ratio-smr
fit = glm(observed ~ sex+ agegroup+ year, 
          data=joint|> mutate_at(c("sex","agegroup","year"),~factor(.)),
          family=poisson, offset=log(mean_m_x_w))
library(multcomp)
glht(fit, linfct = mcp(sex = "Tukey"))
glht(fit, linfct = mcp(agegroup = "Tukey"))


fit = glm(observed ~ sex+ agegroup+ year, 
          data=joint|> mutate_at(c("sex","agegroup","year"),~factor(.)),
          family=poisson, offset=log(mean_m_x_w))
library(multcomp)
glht(fit, linfct = mcp(sex = "Tukey"))
glht(fit, linfct = mcp(agegroup = "Tukey"))
clean_df_corr_surv

warning(paste0("Antònia Barceló, M., Saez, M., Cano-Serral, G., Ángel Martínez-Beneito, M., Miguel Martínez, J., Borrell, C., Ocaña-Riola, R., Montoya, I., Calvo, M., López-Abente, G., Rodríguez-Sanz, M., Toro, S., Tomás Alcalá, J., Saurina, C., Sánchez-Villegas, P., & Figueiras, A. (2008). Métodos para la suavización de indicadores de mortalidad: Aplicación al análisis de desigualdades en mortalidad en ciudades del Estado español (Proyecto MEDEA). Gaceta Sanitaria, 22(6), 596-608. https://doi.org/10.1016/S0213-9111(08)75362-7. En el caso del proyecto MEDEA, el modelo BYM se especifica como un modelo lineal generalizado mixto (GLMM) con variable respuesta de Poisson y considerando como offset los casos esperados33:"))
warning(paste0("Por qué se utiliza los casos esperados y no el hazard ocmo offset?"))
warning("yo en mi tesis: modelos de regresión con enlace-log e incorporando al proceso de estimación el término denominado offset y que aludirá a los años-persona, utilizando el paquete popEpi R.")
broom::tidy(fit, exponentiate=T, conf.int=T)
#    term          estimate std.error statistic   p.value  conf.low  conf.high
#    <chr>            <dbl>     <dbl>     <dbl>     <dbl>     <dbl>      <dbl>
#  1 (Intercept) 11140.        0.0940    99.1   0         9240.     13360.    
#  2 sexMale         1.86      0.0460    13.5   1.59e- 41    1.70       2.04  
#  3 agegroup30      1.56      0.0571     7.73  1.05e- 14    1.39       1.74  
#  4 agegroup45      0.524     0.0570   -11.3   9.26e- 30    0.469      0.587 
#  5 agegroup60      0.0289    0.0947   -37.4   2.19e-306    0.0240     0.0347
#  6 year2011        1.90      0.0896     7.16  8.12e- 13    1.60       2.27  
#  7 year2012        1.88      0.0901     6.99  2.74e- 12    1.58       2.24  
#  8 year2013        2.03      0.0891     7.97  1.59e- 15    1.71       2.43  
#  9 year2014        2.62      0.0858    11.2   3.48e- 29    2.22       3.10  
# 10 year2015        2.12      0.0889     8.48  2.32e- 17    1.79       2.53  
# 11 year2016        1.95      0.0905     7.37  1.67e- 13    1.63       2.33  
# 12 year2017        1.42      0.0967     3.63  2.78e-  4    1.18       1.72  
# 13 year2018        0.971     0.107     -0.280 7.80e-  1    0.787      1.20  
# 14 year2019        0.546     0.128     -4.73  2.24e-  6    0.423      0.699 
# 15 year2020        0.155     0.213     -8.78  1.68e- 18    0.0995     0.230 

# Pearson residuals
pearson_res <- residuals(fit, type = "pearson")

# Dispersion
disp <- sum(pearson_res^2) / fit$df.residual
#They're working residuals instead, which means that this computed dispersion won't be 
#exactly the same as (deviance/df.residual), but for reasonably large data sets it should be very close

# Imprimir índice de dispersión
cat("dispersión:", disp, "\n")
#Índice de dispersión: 3.708736 


#### SMR- survrate, quasi-poisson para la dispersión -------------------------------------------------------------

fit_qp = glm(observed ~ sex+ agegroup+ year, 
          data=joint|> mutate_at(c("sex","agegroup","year"),~factor(.)),
          family=quasipoisson, offset=log(mean_m_x_w))
summary(fit_qp)

broom::tidy(fit_qp, exponentiate=T, conf.int=T)
# # A tibble: 2 × 7
#    term          estimate std.error statistic  p.value  conf.low  conf.high
#    <chr>            <dbl>     <dbl>     <dbl>    <dbl>     <dbl>      <dbl>
#  1 (Intercept) 11140.        0.181     51.4   4.22e-59 7733.     15740.    
#  2 sexMale         1.86      0.0885     7.01  1.00e- 9    1.57       2.22  
#  3 agegroup30      1.56      0.110      4.02  1.42e- 4    1.26       1.94  
#  4 agegroup45      0.524     0.110     -5.88  1.13e- 7    0.424      0.653 
#  5 agegroup60      0.0289    0.182    -19.4   1.44e-30    0.0200     0.0410
#  6 year2011        1.90      0.173      3.72  3.92e- 4    1.36       2.68  
#  7 year2012        1.88      0.174      3.63  5.23e- 4    1.34       2.65  
#  8 year2013        2.03      0.172      4.14  9.26e- 5    1.46       2.87  
#  9 year2014        2.62      0.165      5.82  1.44e- 7    1.91       3.65  
# 10 year2015        2.12      0.171      4.40  3.61e- 5    1.53       2.99  
# 11 year2016        1.95      0.174      3.83  2.70e- 4    1.39       2.76  
# 12 year2017        1.42      0.186      1.89  6.31e- 2    0.988      2.05  
# 13 year2018        0.971     0.205     -0.145 8.85e- 1    0.647      1.45  
# 14 year2019        0.546     0.246     -2.46  1.64e- 2    0.332      0.875 
# 15 year2020        0.155     0.410     -4.56  2.03e- 5    0.0629     0.321 

#¿Es esperable que el riesgo baje tan drásticamente con la edad? 
#Quizás convenga revisar cómo definiste los cortes de agegroup.

# A partir de 2019 y 2020 hay caídas muy marcadas (IRR ≈ 0.55 y 0.15, respectivamente), 
# quizá reflejando cambios en la población o el impacto de la pandemia.

#broom::tidy(fit2, exponentiate=T, conf.int=T)

### SMR- epitools, sólo edad y sexo (1x10, pop 2015) -------------------------------------------------------------

# count: vector of age-specific count of events
# pop	: vector of age-specific person-years or population estimates
# rate	: vector of age-specific rates
# stdpop	: vector of age-specific standard population

message("Para formatear base integrada con población")
cat("Indirecto C1")
CONS_C1_mod_c_c_int_pop<-
  expand_grid_cont |> 
  dplyr::left_join(c1_age_admdate_int_sexo, by= c("age_admdate_int"="adm_age_int_rec","sexo"="sexo"))|> 
  dplyr::mutate(deaths= ifelse(is.na(deaths),0,deaths),n= ifelse(is.na(n),0,n)) |> 
  dplyr::left_join(cons_rate_sex_1x10, by=c("age_admdate_int"="age_rec", "sexo"="sex"))%>% 
  dplyr::left_join(proy_ine_cont_2015, by=c("age_admdate_int"="Edad", "sexo"="sexo"))%>%
  dplyr::ungroup() %>% 
  # dplyr::filter(!is.na(age_admdate_int_cat)) %>% 
  dplyr::left_join(mort_cont_2015, by=c("age_admdate_int"="edad_cant", "sexo"="sexo"), suffix=c("","_mort_2015"))%>%
  dplyr::select(-`Sexo (1=Hombre 2=Mujer)` , -anio) %>%  
  dplyr::rename("edad"="age_admdate_int",
                "deaths_subpop"="deaths", 
                "tot_subpop"="n",
                "pop_rate_mx"="mx",
                "pop"="poblacion",
                "deaths_pop"="n_mort_2015")%>%
  dplyr::mutate(
    rate_subpop = deaths_subpop / py_seguimiento_desde_adm,    # tasa en tu subpob, por año
    hazard_anual = lambda_p * 365.241,#Es un hazard diario aproximado, asumiendo mortalidad constante a lo largo del año.
    # Multiplico la tasa de TU subpoblación por la "estructura" (pop) de la
    # población estándar. A esto lo llamamos muertes "esperadas" si la
    # población estándar tuviera la tasa de la subpoblación.
    exp_deaths_ind = hazard_anual * py_seguimiento_desde_adm,     
    exp_deaths_ind2 = pop_rate_mx * py_seguimiento_desde_adm,
    exp_direct = rate_subpop * pop
  ) 
cat("Indirecto C3")
CONS_C3_mod_c_c_int_pop<-
  expand_grid_cont |> 
  dplyr::left_join(age_admdate_int_sexo, by= c("age_admdate_int","sexo"))|> 
  dplyr::mutate(deaths= ifelse(is.na(deaths),0,deaths),n= ifelse(is.na(n),0,n)) |> 
  dplyr::left_join(cons_rate_sex_1x10, by=c("age_admdate_int"="age_rec", "sexo"="sex"))%>% 
  dplyr::left_join(proy_ine_cont_2015, by=c("age_admdate_int"="Edad", "sexo"="sexo"))%>%
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(age_admdate_int)) %>% 
  dplyr::left_join(mutate(mort_cont_2015, edad_cant=as.numeric(edad_cant)), by=c("age_admdate_int"="edad_cant", "sexo"="sexo"), suffix=c("","_mort_2015"))%>%
  dplyr::select(-`Sexo (1=Hombre 2=Mujer)` , -anio) %>%  
  dplyr::rename("edad"="age_admdate_int",
                "deaths_subpop"="deaths", 
                "tot_subpop"="n",
                "pop_rate_mx"="mx",
                "pop"="poblacion",
                "deaths_pop"="n_mort_2015")%>%
  dplyr::mutate(
    rate_subpop = deaths_subpop / py_seguimiento_desde_adm,    # tasa en tu subpob, por año
    hazard_anual = lambda_p * 365.241,#Es un hazard diario aproximado, asumiendo mortalidad constante a lo largo del año.
    # Multiplico la tasa de TU subpoblación por la "estructura" (pop) de la
    # población estándar. A esto lo llamamos muertes "esperadas" si la
    # población estándar tuviera la tasa de tu subpoblación.
    exp_deaths_ind = hazard_anual * py_seguimiento_desde_adm,     
    exp_deaths_ind2 = pop_rate_mx * py_seguimiento_desde_adm,
    exp_direct = rate_subpop * pop
  ) 

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#tablas_de_mortalidad_de_chile_1992_2050_group_18_64


cat("Directo C1")

epitools::ageadjust.direct(count= subset(joint, sex=="Female")$observed,
                           pop=subset(joint, sex=="Female")$pt,
                           rate= NULL, #tasas_dtX2023_02_03_DatosEgresosHosp_encrip_filt5_dir_method_exp0$rate,
                           stdpop= subset(joint, sex=="Female")$sumpop)
# crude.rate    adj.rate         lci         uci 
# 0.006955265 0.010093032 0.008659164 0.013633262 
epitools::ageadjust.direct(count= subset(joint, sex=="Male")$observed,
                           pop=subset(joint, sex=="Male")$pt,
                           rate= NULL, #tasas_dtX2023_02_03_DatosEgresosHosp_encrip_filt5_dir_method_exp0$rate,
                           stdpop= subset(joint, sex=="Male")$sumpop)
# crude.rate    adj.rate         lci         uci 
# 0.008843409 0.012116953 0.011196911 0.013329932 


#### SMR epitools, indirec ---------------------------------------------------


epitools::ageadjust.indirect(count = subset(joint, sex=="Male")$observed, 
                             pop = subset(joint, sex=="Male")$pt,
                             stdrate
                             stdcount = subset(joint, sex=="Male")$expected, 
                             stdpop = subset(joint, sex=="Male")$sumpop)


