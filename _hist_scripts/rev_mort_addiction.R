#R CMD BATCH --no-save --no-restore "G:/My Drive/Alvacast/SISTRAT 2023/cons/rev_mort_addiction.R" "G:/My Drive/Alvacast/SISTRAT 2023/cons/postrev_addiction.Rout"


rm(list = ls()); gc()

# Captura stdout sink(logfile_out, type = "output") # Captura messages/warnings (mensajes de message() y warnings) sink(logfile_msg, type = "message") options(warn = 1) # imprime warnings inmediatamente
# log_all <- file("post_rev_addiction_log.log", open = "wt") 
# 
# sink(log_all, type = "output") 
# sink(log_all, type = "message") 
# options(warn = 1) #print warnings

# Load packages & data ----------------------------------------------------


load('G:/My Drive/Alvacast/SISTRAT 2023//data/20241015_out/mort_2025_08_21.Rdata')



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
  metafor,     # For heterogeneity test /Cochrane Q
  parallel,    # Parallel computing (for bootstrap)
  install = T  # Automatically install packages if not already installed
)


#Fay & Feuer (1997). Confidence intervals for directly standardized rates: a 
#method based on the gamma distribution. Stat Med 16:791-801.
dsr_format <- function(rate, se, phi = 1, factor = 1e3, digits = 2, conf = 0.95) {
  z <- qnorm(1 - (1 - conf)/2)
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
          rate*factor,
          pmax(0, (rate - z*se*sqrt(phi))*factor),
          (rate + z*se*sqrt(phi))*factor)
}
#2025-12-16= corregí porque multiplicaba por phi
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

invisible("using sir output")
sir_ci_phi_improved <- function(sir_obj, phi, conf.level = 0.95) {
  #Método log-normal, the best, dont overestimate, or subestimate variance
  # extract totals
  total_obs <- sir_obj$observed
  total_exp <- sir_obj$expected
  
  # Calculate SEs
  theta <- total_obs / total_exp
  # Normal approximation, n>20  
  # Corrected SEs (McCullagh & Nelder, 1989)
  # “For ratios of Poisson means (such as SIR or CMR), the appropriate approach is to use multinomial or binomial models conditioned on the total observed.”
  # Breslow NE, Day NE. Statistical Methods in Cancer Research, Vol. II (IARC, 1987), §2.2. – Derives the same SE formula and recommends inflating by φ in the presence of heterogeneity.
  z <- qnorm(1 - (1 - conf.level)/2)
  
  phi<- base::pmax(1, phi)
  se_log <- sqrt(phi / total_obs)  # Valid formula
  
  # ICs
  lci <- theta * exp(-z * se_log)
  uci <- theta * exp(z * se_log)
  
  data.frame(
    SIR = theta,
    CI_low = lci,
    CI_high = uci,
    phi_used = phi
  )
}

# Miss adm dates & discharge info --------------------------------

#n y porcentaje de perdidos por fechas de ingreso perdidas

#Total n= 70,064 Survived n= 67,068	Deceased n= 2,996
# 2996/70064

cat("Duplicados\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel|>
  janitor::get_dupes(hash_key, adm_age_rec2)|>  nrow()

SISTRAT23_c1_2010_2022_df_prev1q_sel|>
  (\(df) {
    nrow(df)->>before_disc_dup_hash_age_adm_nrow
    cat(paste0("1.Number of cases before discarding duplicates in admission age and hash key: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("1.Number of patients before discarding duplicates in admission age and hash key: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n");
    mutate(df, years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year")) |> 
      filter(is.na(years_in_tr)) ->> first_st_miss_days_in_tr
    cat(paste0("Records with unavailable missing days in treatment (eg., currently in treatment): ",nrow(first_st_miss_days_in_tr), "\n"));
    mutate(df, years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year")) |> 
      filter(years_in_tr<0) ->> first_st_neg_days_in_tr;
    cat(paste0("Records with negative days in treatment: ",nrow(first_st_neg_days_in_tr), "\n")) ;
    mutate(df, years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year")) |> 
      filter(years_in_tr>3) ->> first_st_more3yrs; 
    cat(paste0("Records with more than 3 years in treatment: ",nrow(first_st_more3yrs), "\n")) 
    df
  })()|>
  #2025-12-10: Con este código en la práctica estoy sacando los con años de tto. perdidos.
  mutate(years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year"))|>
  filter(!is.na(years_in_tr)|years_in_tr>=0|years_in_tr<=3)|> 
  group_by(hash_key, adm_age_rec2)|> 
  slice_max(dit_rec6)|> 
  ungroup()|> 
  (\(df) {
    nrow(df)->>after_disc_dup_hash_age_adm_nrow
    cat(paste0("1.Number of cases after discarding duplicates in admission age and hash key and validating days in treatment: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("1.Number of patients after discarding duplicates in admission age and hash key and validating days in treatment: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
  })()

warning("Resultado práctico: con | estás manteniendo todas las filas no-missing,")
warning("independientemente de si years_in_tr está fuera del rango\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel|>
  (\(df) {
    #nrow(df)->>before_disc_dup_hash_age_adm_nrow
    cat(paste0("1.Number of cases before discarding duplicates in admission age and hash key: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("1.Number of patients before discarding duplicates in admission age and hash key: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    cat(paste0("Records with unavailable missing days in treatment (eg., currently in treatment): ",df |> mutate(years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year")) |> 
                 filter(is.na(years_in_tr)) |> nrow(), "\n"));
    cat(paste0("Records with negative days in treatment: ",df |> mutate(years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year")) |> 
                 filter(years_in_tr<0)|>  nrow(), "\n")) ;
    cat(paste0("Records with more than 3 years in treatment: ",df |> mutate(years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year")) |> 
                 filter(years_in_tr>3)|> nrow(), "\n")) 
    df
  })()|>
  mutate(years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year"))|>
  filter(is.na(years_in_tr))|> 
  nrow()

nrow(SISTRAT23_c1_2010_2022_df_prev1q_sel)-nrow(SISTRAT23_c1_2010_2022_df_prev1q_sel2)
#4034: 27 (last tr. duplicate + 4007 unavailable days in tr.)

cat("Si dije que descartaba quienes no completaban los ttos.\n")
# ORIGINAL DATABASE
# 1.Number of cases before discarding duplicates in admission age and hash key: 150,046 
# 1.Number of patients before discarding duplicates in admission age and hash key: 106,283 

cat("Discarded rows\n")
before_disc_dup_hash_age_adm_nrow-after_disc_dup_hash_age_adm_nrow #before 18-06 there was  150,019 , p= 106,283 as a result in this step
cat("Discarded patients\n")
length(unique(SISTRAT23_c1_2010_2022_df_prev1q_sel$hash_key))-length(unique(SISTRAT23_c1_2010_2022_df_prev1q_sel2$hash_key))

# 1.Number of cases after discarding duplicates in admission age and hash key and validating days in treatment: 146,012 
# 1.Number of patients after discarding duplicates in admission age and hash key and validating days in treatment: 103,612 

df0 <- SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv

df1 <- df0 |>
  filter(adm_date_rec2 >= "2010-01-01", adm_date_rec2 < "2021-01-01")

df2 <- df1 |>
  arrange(hash_key, adm_date_rec2) |>
  group_by(hash_key) |>
  mutate(n_ttos = n(), tto = row_number(), post_ttos = as.integer(n_ttos > 1)) |>
  slice_min(tto) |>
  ungroup()

df3 <- df2 |>
  filter(adm_age_rec2 >= 18, adm_age_rec2 < 65)

df2 |>
  filter(adm_age_rec2 >= 65) |> count(!is.na(death_date))
#   `!is.na(death_date)`     n
#   <lgl>                <int>
# 1 FALSE                  978
# 2 TRUE                   222
warning("Muere el 18.5% de los atendidos >64")

# mini auditoría
audit <- data.frame(
  step = c("inicio", "ventana temporal", "primer tratamiento", "edad 18-64"),
  cases = c(nrow(df0), nrow(df1), nrow(df2), nrow(df3)),
  patients = c(
    nrow(distinct(df0, hash_key)),
    nrow(distinct(df1, hash_key)),
    nrow(distinct(df2, hash_key)),
    nrow(distinct(df3, hash_key))
  )
)

audit

# 3a. Before discarding cases, cases, 2010-2020, first treatment: 146,014 
# 3a. Before discarding cases, patients, 2010-2020, first treatment: 103,612 
# 3a.Number of cases, in treatments between 2010-2020, first treatment: 88,774 
# 3a.Number of patients, in treatments between 2010-2020, first treatment: 88,774 

cat("Discarded rows\n")
before_df3a_nrow-after_df3a_nrow
cat("Discarded patients\n")
length(unique(SISTRAT23_c1_2010_2022_df_prev1q_sel2_surv$hash_key))-
  length(unique(SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv$hash_key))


cat("Por qué vuelvo a sacar ongoing tr. después (truncated, death or currently")
cat("in treatment, o referrals to teratments outside SENDA network)\n")

cat("Discarded records:\n")
df4a_nrow_pre-df4a_nrow_post

length(unique(SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv$hash_key))-
  length(unique(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$hash_key))

cat("NA tr compliance\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
  filter(is.na(tr_compliance_rec3)) |> nrow()
#0
cat("Death tr complience\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
tidytable::filter(tr_compliance_rec3=="death" )|>
  nrow()
#0

SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
tidytable::filter(grepl("truncated|currently|referral", tr_compliance_rec3))|>
  nrow()
SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
  tidytable::filter(grepl("referral", tr_compliance_rec3))|>
  nrow()
#13886
SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
  tidytable::filter(grepl("truncated", tr_compliance_rec3))|>
  nrow()
#417
SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
  tidytable::filter(grepl("currently", tr_compliance_rec3))|>
  nrow()
#1
message("2015-12-10: Al final decidí juntar truncated con currently, pq es poco")
message("realista que no haya terminado el tratamiento al 2022")

#14304
scales::percent(
SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
  tidytable::filter(grepl("truncated|currently|referral", tr_compliance_rec3))|>
  nrow()/after_df3a_nrow, 
  accuracy=.1
)

days_years  <- 365.2425
censor_date <- as.Date("2020-12-31")

# 0) Start from step 3a output
df4a_0 <- SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv

# 1) Exclude records with missing/invalid treatment compliance
df4a_1 <- df4a_0 |>
  filter(
    !is.na(tr_compliance_rec3),
    tr_compliance_rec3 != "death",
    !grepl("truncated|currently|referral", tr_compliance_rec3)
  )

# 2) Discharge age
df4a_2 <- df4a_1 |>
  mutate(disch_age_rec = (dit_rec6 / days_years) + adm_age_rec2)

# 3) Survival time + death date/age (with censoring)
df4a_3 <- df4a_2 |>
  mutate(
    timesurv = case_when(
      status == 1 ~ time_length(interval(adm_date_rec2, death_date), unit = "year"),
      status == 0 ~ time_length(interval(adm_date_rec2, censor_date), unit = "year"),
      TRUE ~ NA_real_
    ),
    death_date_rec = case_when(
      status == 1 ~ death_date,
      status == 0 ~ censor_date,
      TRUE ~ as.Date(NA)
    ),
    death_age_rec = as.integer(timesurv + adm_age_rec2)
  )
#Discarded (death, no tr. compliance)
# Final object
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv_rev <- df4a_3

cat("#_#_#_#_#_\nAudit table for 4a\n#_#_#_#_\n")
audit_4a <- data.frame(
  step = c(
    "start (3a)",
    "exclude invalid tr. compliance",
    "add discharge age",
    "derive survival/death vars"
  ),
  n = c(
    nrow(df4a_0),
    nrow(df4a_1),
    nrow(df4a_2),
    nrow(df4a_3)
  ),
  patients = c(
    nrow(distinct(df4a_0, hash_key)),
    nrow(distinct(df4a_1, hash_key)),
    nrow(distinct(df4a_2, hash_key)),
    nrow(distinct(df4a_3, hash_key))
  )
)

audit_4a

## SI HUBIESE HECHO LA SELECCIÓN DE CASOS DE FORMA ORDENADA! --------------------------------

#days_years<- 365.2425

SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv|>
  (\(df) {
    nrow(df)->>df4a_nrow_pre
    cat(paste0("Discarded (death, no tr. compliance), cases(4a): ", formatC(nrow(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3))), big.mark=",")),"\n")
    cat(paste0("Discarded (death, no tr. compliance), patients(4a): ", formatC(nrow(distinct(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3)), hash_key)), big.mark=",")),"\n")
  df
    })()|>
  tidytable::filter(!is.na(tr_compliance_rec3) & tr_compliance_rec3!="death" & !grepl("truncated|currently|referral", tr_compliance_rec3))|>
  # calculamos la edad al egreso
  tidytable::mutate(disch_age_rec= (dit_rec6/365.241)+adm_age_rec2)|>
  tidytable::mutate(timesurv= tidytable::case_when(
    status==1~ time_length(interval(adm_date_rec2, death_date), unit="year"),
    status==0~ time_length(interval(adm_date_rec2, as.Date("2020-12-31")), unit="year")))|>
  tidytable::mutate(death_date_rec= tidytable::case_when(
    status==1~ death_date,
    status==0~ as.Date("2020-12-31")))|>
  tidytable::mutate(death_age_rec= as.integer(timesurv+adm_age_rec2))|> 
  (\(df) {
    cat(paste0("4a.Number of cases: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("4a.Number of patients: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    nrow(df) ->>df4a_nrow_post
    df
  })()
cat("Discarded records:\n")
df4a_nrow_pre-df4a_nrow_post

cat("Records were excluded for patients who had not yet been discharged")
 paste0(round(((df4a_nrow_pre-df4a_nrow_post)/222945)*100,1),"%")

 
 SISTRAT23_c1_2010_2022_df_prev1q_sel2_POST<-SISTRAT23_c1_2010_2022_df_prev1q_sel|>
  tidytable::filter(adm_date_rec2>="2010-01-01", adm_date_rec2<"2020-12-31")|> 
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
    nrow(df)->>before_disc_dup_hash_age_adm_nrow_POST
    cat(paste0("1.Number of cases before discarding duplicates in admission age and hash key: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("1.Number of patients before discarding duplicates in admission age and hash key: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    mutate(df, years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year")) |> filter(is.na(years_in_tr))->>alt_proc_miss_day_tr_curr_in;
    cat(paste0("Records with unavailable missing days in treatment (eg., currently in treatment): ", nrow(alt_proc_miss_day_tr_curr_in), "\n"));
    mutate(df, years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year"))|> filter(years_in_tr<0) ->> alt_proc_neg_dit
    cat(paste0("Records with negative days in treatment: ",nrow(alt_proc_neg_dit), "\n")) ;
    mutate(df, years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year"))|> filter(years_in_tr>3) ->> alt_proc_more3yr; 
    cat(paste0("Records with more than 3 years in treatment: ", nrow(alt_proc_more3yr), "\n")) 
    df
  })()|>
  mutate(years_in_tr= time_length(interval(adm_date_rec2, disch_date_rec6), unit="year"))|>
  filter(!is.na(years_in_tr)|years_in_tr>=0|years_in_tr<=3)|> 
  group_by(hash_key, adm_age_rec2)|> 
  slice_max(dit_rec6)|> 
  ungroup()|> 
  (\(df) {
    nrow(df)->>after_disc_dup_hash_age_adm_nrow_POST
    cat(paste0("1.Number of cases after discarding duplicates in admission age and hash key and validating days in treatment: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("1.Number of patients after discarding duplicates in admission age and hash key and validating days in treatment: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    df
  })()
# Se procesaron 90265 grupos de 90265. 100% hecho. Transcurrido: 7s. ETA: 0s.
# 1.Number of cases before discarding duplicates in admission age and hash key: 88,929 
# 1.Number of patients before discarding duplicates in admission age and hash key: 88,929 
# Records with unavailable missing days in treatment (eg., currently in treatment): 155
# Records with negative days in treatment: 7
# Records with more than 3 years in treatment: 690
# 1.Number of cases after discarding duplicates in admission age and hash key and validating days in treatment: 88,774 
# 1.Number of patients after discarding duplicates in admission age and hash key and validating days in treatment: 88,774 

nrow(SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv)==nrow(SISTRAT23_c1_2010_2022_df_prev1q_sel2_POST)
# 88774

identical(SISTRAT23_c1_2010_2022_df_prev1q_sel3a_surv,
          SISTRAT23_c1_2010_2022_df_prev1q_sel2_POST)

warning("Se queda con la misma cantidad de pacientes, pero no son los mismos episodios\n")

SISTRAT23_c1_2010_2022_df_prev1q_sel2_POST_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel2_POST |>
  tidytable::left_join(mortality[,c("hashkey", "death_date")], by=c("hash_key"="hashkey"), multiple="first") |>
  tidytable::mutate(status=ifelse(is.na(death_date), 0, 1))

cat("\n#_#_#_#_#_\nAhora hacemos el proceso: Discarded ongoing treatments (truncated, death or currently in treatment, o referrals to teratments outside SENDA network).\n#_#_#_#_#_\n")
days_years<- 365.2425

SISTRAT23_c1_2010_2022_df_prev1q_sel4a_POST_surv<-
  SISTRAT23_c1_2010_2022_df_prev1q_sel2_POST_surv|>
  (\(df) {
    nrow(df)->>df4a_nrow_pre_POST
    cat(paste0("Discarded (death, no tr. compliance), cases(4a): ", formatC(nrow(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3))), big.mark=",")),"\n")
    cat(paste0("Discarded (death, no tr. compliance), patients(4a): ", formatC(nrow(distinct(tidytable::filter(df, is.na(tr_compliance_rec3) | tr_compliance_rec3=="death"|grepl("truncated|currently|referral", tr_compliance_rec3)), hash_key)), big.mark=",")),"\n")
    df
  })()|>
  tidytable::filter(!is.na(tr_compliance_rec3) & tr_compliance_rec3!="death" & !grepl("truncated|currently|referral", tr_compliance_rec3))|>
  # calculamos la edad al egreso
  tidytable::mutate(disch_age_rec= (dit_rec6/365.241)+adm_age_rec2)|>
  tidytable::mutate(timesurv= tidytable::case_when(
    status==1~ time_length(interval(adm_date_rec2, death_date), unit="year"),
    status==0~ time_length(interval(adm_date_rec2, as.Date("2020-12-31")), unit="year")))|>
  tidytable::mutate(death_date_rec= tidytable::case_when(
    status==1~ death_date,
    status==0~ as.Date("2020-12-31")))|>
  tidytable::mutate(death_age_rec= as.integer(timesurv+adm_age_rec2))|> 
  (\(df) {
    cat(paste0("4a.Number of cases: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("4a.Number of patients: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    nrow(df) ->>df4a_nrow_post_POST
    df
  })()
cat("Discarded records:\n")
df4a_nrow_pre_POST-df4a_nrow_post_POST
# Discarded (death, no tr. compliance), cases(4a): 14,311 
# Discarded (death, no tr. compliance), patients(4a): 14,311 
# 4a.Number of cases: 74,464 
# 4a.Number of patients: 74,463
warning("Ojo, hay más casos que pacientes (+1)\n")

cat("vs. el anterior\n")
# Discarded (death, no tr. compliance), cases(4a): 14,304 
# Discarded (death, no tr. compliance), patients(4a): 14,304 
# 4a.Number of cases: 74,470 
# 4a.Number of patients: 74,470 
# > cat("Discarded records:\n")
# Discarded records:
#   > df4a_nrow_pre-df4a_nrow_post
# [1] 14304

cat("Records were excluded for patients who had not yet been discharged")
paste0(round(((df4a_nrow_pre-df4a_nrow_post)/222945)*100,1),"%")

# > table(!is.na(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_POST_surv$death_date))
# FALSE  TRUE 
# 71101  3363 
# > table(!is.na(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$death_date))
# FALSE  TRUE 
# 71096  3374 

message("Estas son los pacientes y filas que habría perdido (mejor hablar de pacientes)\n")
message("de haber eliminado casos correctamente\n")
alt_proc_miss_day_tr_curr_in
alt_proc_neg_dit 
alt_proc_more3yr


SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |> 
  filter( rn %in% alt_proc_miss_day_tr_curr_in$rn)
#originalmente 155, pero ahora 0
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |> 
  filter( rn %in% alt_proc_neg_dit$rn)
#6
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |> 
  filter( rn %in% alt_proc_more3yr $rn) |> nrow()
#431

clean_df |> 
  filter( rn %in% alt_proc_miss_day_tr_curr_in$rn)
message("Ninguno persiste de sin días en tto por ttos en curso\n")
#originalmente 155, pero ahora 0
clean_df |> 
  filter( rn %in% alt_proc_neg_dit$rn)
#6
message("Ya ninguno persiste de los días de tto negativos\n")
clean_df |> 
  filter( rn %in% alt_proc_more3yr $rn) |> nrow()
#278
warning("278 con mpas de 3 años persisten en la base final\n")



cat("#_#_#_#_#_\nesto es lo que tengo en mortality.qmd\n#_#_#_#_")
# En "We discard missing values in sex, discharge and death dates 
# and negative follow-up periods."

#2025-12-10: cambié de 4b a 4a
# negative days in treatment
neg_tr_d<- table(as.numeric(with(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv, 
  difftime(death_date_rec, disch_date_rec6, unit="days"))/ 365.25)<0)[[2]]
#4180

#over 3 years
over3yrs<- 
table(as.numeric(with(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv, 
  difftime(disch_date_rec6, adm_date_rec2, unit="days"))/ 365.25)>3)[[2]]
#433

cat("#_#_#_#_#_\ncon las3 anteriores hago este:\n#_#_#_#_")
disch_after_cens_death

cat("En este objeto contamos los dáis negativos usando las fechas concretas\n")

cat("Sólo los con fechas negativas son los 4256, por tanto, los filtros de datos \n")
cat("perdidos (fallecimiento, abandono, sexo) no sirven \n")


SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>
  dplyr::filter(disch_date_rec6 >= death_date_rec     # evita seguimiento negativo
  ) |> 
  #dplyr::select(disch_date_rec6, death_date_rec)
  dplyr::filter(death_date_rec=="2020-12-31", is.na(death_date))
#3888
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>
  dplyr::filter(disch_date_rec6 >= death_date_rec     # evita seguimiento negativo
  ) |> 
  #dplyr::select(disch_date_rec6, death_date_rec)
  dplyr::filter(death_date_rec=="2020-12-31", !is.na(death_date))

SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>
  dplyr::filter(disch_date_rec6 >= death_date_rec     # evita seguimiento negativo
  ) |> 
  #dplyr::select(disch_date_rec6, death_date_rec)
  dplyr::filter(!is.na(death_date))

SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>
  dplyr::filter(adm_date_rec2 >= disch_date_rec6     # evita seguimiento negativo
  ) |> 
  #dplyr::select(disch_date_rec6, death_date_rec)
  nrow()
#150
cat("No me gustó este criterio, pero lo ocupé. Sin querer eliminé 10 muertos\n")
cat("De posibles 0 días en tratamiento\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv |>
  dplyr::filter(adm_date_rec2 >= disch_date_rec6     # evita seguimiento negativo
  ) |> 
  dplyr::filter(!is.na(death_date)) |> nrow()

368+3888+150
#4406

cat("Qué pasa si aplicamos el filtro corregido para aprovechar los 0 
DIT en el análisis de sensibilidad\n")
SISTRAT23_c1_2010_2022_df_prev1q_sel4c_surv|>
  mutate(disch_date_corr= as.Date(disch_date_corr2))|> #changed at 25-08-03
  (\(df) {
    cat("Before discarding missing or discharge dates \n")
    print(nrow(df))    
    df
  })()  |> 
  filter(#!is.na(disch_date_corr),
         #!is.na(death_date_rec),
         #disch_date_corr <= death_date_rec,     # evita seguimiento negativo  #2025-08-03: changed censorship to allow more deceased to enter
         adm_date_rec2  >= disch_date_corr#,     # para sacar 831d9f7b2771ce2701ae4a4417f26e70f2e2acb21d21c6fe7ff1e766c9792d3a on días tto neg
         #!is.na(sex_rec)
         ) 

cat("En la base de sensibilidad en que imputo con la mediana, 
    202 tienen fecha de ingreso posterior o igual a la de egreso, y los elimino\n")

### Resumen ==================

cat("Pacientes con fechas de ingresos iguales o mayores a la de egreso: 150\n")
cat("Pacientes en primer tratamiento que terminaron posterior al 2020-12-31, los elimino: 3888.\n")
cat("Pacientes mueren antes de terminar el primer tto., los elimino: 368\n")

nrow(clean_df)
#70064



clean_df |> 
  filter(hash_key %in% first_st_miss_days_in_tr$hash_key)

clean_df |> 
  filter( rn %in% first_st_neg_days_in_tr$rn)

clean_df |> 
  filter( rn %in% first_st_more3yrs$rn)



cat("Del primer filtro, estos casos son los que pudieron haber afectado mi muestra
de los que no tienen tratamiento terminado (en curso),
porque son los que inician tratamiento antes de la fecha de censura \n")
first_st_miss_days_in_tr |> 
  filter(adm_date_rec2>="2010-01-01", 
         adm_date_rec2<"2020-12-31", 
         adm_age_rec2>=18, 
         adm_age_rec2<65) |> nrow()
#212

cat("Del primer filtro, estos casos son los que podrían haber sido afectados por
duplicados, porque son filas que persisten en la base de datos. Serían nrow x 2")
clean_df |> filter(rn %in% rows_with_dupes) |> nrow()
#9x2 = 18

### Flowchart corregido ==================

gr<-
  DiagrammeR::grViz("
    digraph flowchart {
      graph [layout = dot, rankdir = TB, nodesep = 0.2, ranksep = 0.3]
    
      # General node styling
      node [fontname = Times, shape = rectangle, fontsize = 17, style = filled, fillcolor = transparent]
    
      # Main flow nodes
      original [label = 'Original Database\\n2010-2022\\n(n = 150,046;\\nPatients = 106,283)', fillcolor = lightgray]
      c1_dataset [label = 'Database\\n(n = 146,012;\\nPatients = 103,612)']
      after_discard [label = 'Database\\n(n = 88,774;\\nPatients = 88,774)']
      after_discard2 [label = 'Database\\n(n = 74,470;\\nPatients = 74,470)']
      final_dataset [label = 'Final Database\\n(n = 70,064;\\nPatients = 70,064)', fillcolor = lightgray]
      
      # Discard nodes (aligned between main flow steps)
      discard_referrals [label = '&#8226;Duplicates in admission age and patient ID kept last (n= 54): 27\\l&#8226;Records with missing days in treatment (e.g., still in treatment\\l  at the dataset extraction date, April 28, 2023): 4,007\\l']
      discard_duplicates [label = '&#8226;Restricted time window of tr. episode (admission\\l  date: 2010-2020) (n=21,434; patients=13,506)\\l&#8226;Kept one episode per patient (younger age) (n=13,506)\\l&#8226;Restricted admission age to 18-64 (n=1,332; patients=1,332)\\l'
      ]
      discard_single [label = '&#8226;Excluded episodes of referrals outside SENDA network: 13,886\\l&#8226;Excluded truncated episodes (not administratively closed): 418\\l'
      ] // &#8226;Discarded (death, no tr. compliance): 14,304
      discard_single2 [label = '&#8226;Discarded patients that finished treatment after 2020 (n=3,888),\\l patients that died before finishing their first treatment (n=368), and \\linconsistent treatment dates (negative or no follow-up periods; n=150): 4,406\\l']

      # Invisible vertices for middle line
      v1 [shape = point, width = 0, style = invis]
      v2 [shape = point, width = 0, style = invis]
      v3 [shape = point, width = 0, style = invis]
      v4 [shape = point, width = 0, style = invis]
    
      # Main flow edges (vertical line)
      original -> v1 [arrowhead = none]
      v1 -> c1_dataset
      c1_dataset -> v2 [arrowhead = none]
      v2 -> after_discard
      after_discard ->v3  [arrowhead = none]
      v3 -> after_discard2
      after_discard2 -> v4 [arrowhead = none]
      v4 -> final_dataset
    
      # Discard connections (from the middle line)
      v1 -> discard_referrals
      v2 -> discard_duplicates
      v3 -> discard_single
      v4 -> discard_single2
    
      # Alignment
      { rank = same; discard_referrals; v1 }
      { rank = same; discard_duplicates; v2 }
      { rank = same; discard_single; v3 }
      { rank = same; discard_single2; v4 }
    }
  ", width = 1100, height = 1550)

base    <- gsub("/cons","", getwd())
fig_dir <- file.path(base, "cons/_figs")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

html_file <- file.path(fig_dir, "_mortality_flowchart_corr.html")
png_file  <- file.path(fig_dir, "_mortality_flowchart_corr.png")

# Guarda el widget
htmlwidgets::saveWidget(gr, html_file, selfcontained = TRUE)

# Captura grande y nítida
webshot::webshot(
  url = html_file,
  file = png_file,
  vwidth = 1100,
  vheight = 1550,
  zoom = 4,      # 1100*4 = 4400 px de ancho aprox
  expand = 20
)

png_file
# 5-year bins Sensitivity  ----------------------------------------------------

cat("Para comparar si hay alguna categoría muy chica en muestra, e inestable\n")
#xtabs(~tr_compliance_status+ prim_sub_licit+ res_plan+ sex_rec+ adm_age_cat, data= SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv)

# 15-19 20-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 65-69 70-74 75-79
mltper_5x1 <- try(rio::import(paste0(base_path, "mltper_5x1.txt")))
fltper_5x1 <- try(rio::import(paste0(base_path, "fltper_5x1.txt")))


mltper_5x1_filt<-mltper_5x1[between(mltper_5x1$Year,2010,2020),]
fltper_5x1_filt<-fltper_5x1[between(fltper_5x1$Year,2010,2020),]


mltper_5x1_filt$age_rec <- as.numeric(mltper_5x1_filt$Age)
fltper_5x1_filt$age_rec <- as.numeric(fltper_5x1_filt$Age)

mltper_5x1_filt$sex <- "male"
fltper_5x1_filt$sex <- "female"

cons_rate_sex_5x1<-
  rbind.data.frame(mltper_5x1_filt, fltper_5x1_filt)[,c("Year","Age","sex", "lx","qx","mx")]
cons_rate_sex_5x1$lambda_p <- -log( 1 - cons_rate_sex_5x1$qx ) / 365.241

cons_rate_sex_5x1<-cons_rate_sex_5x1[which(cons_rate_sex_5x1$Age!="110+"),]

# 1. Load your object (from the dput provided)
mx_national_raw <- cons_rate_sex_5x1 
# 2. Clean it to match popEpi requirements
mx_national_clean <- mx_national_raw |>
    dplyr::mutate(
    # Convert "15-19" -> 15 (numeric)
    agegroup = dplyr::case_when(
      Age == "0"   ~ 0,
      Age == "1-4" ~ 1,
      TRUE         ~ as.numeric(stringr::str_extract(Age, "^[0-9]+")) 
    ),
    # Capitalize sex to match cohort ("male" -> "Male")
    sex = stringr::str_to_title(sex), 
    # Ensure year is integer
    year = as.integer(Year)
  ) |>
  # Select only the columns needed for merging
  dplyr::select(year, sex, agegroup, mx) |>
  # Ensure it is sorted and distinct
  dplyr::distinct(year, sex, agegroup, .keep_all = TRUE) |>
  dplyr::arrange(year, sex, agegroup)


## Format 5-yr-bin lexis ---------------------------------------------------

# Define standard 5-year breaks (0, 1, 5, 10, 15, 20... 80)
# This aligns perfectly with your new 'mx_national_clean'
std_age_breaks <- c(0, 1, seq(5, 85, by = 5))
age_breaks <- sort(unique(mx_national_clean$agegroup))
# If my cohort is 18+, Can start in 15:
age_breaks <- age_breaks[age_breaks >= 15]

c_SISTRAT_std_bins <- lexpand(
  clean_df, 
  status = status, 
  birth = birth_date_rec, 
  exit = death_date_rec, 
  entry = disch_date_rec6,
  breaks = list(
    per = seq(2010, 2021, by = 1), 
    age = age_breaks # <--- This matches the reference keys (15, 20, 25...)
  ),
  aggre = list(year = per, 
               agegroup = age, 
               sex = sex_rec)
)

cat("Agegroups:\n")
sort(unique(c_SISTRAT_std_bins$agegroup))

## SMR w/ 5-yr-bin lexis ---------------------------------------------------

sir_final_std <- sir(
  coh.data = c_SISTRAT_std_bins, 
  coh.obs = 'from0to1',    # Observed deaths column
  coh.pyrs = 'pyrs',       # Person-years column
  ref.data = mx_national_clean, 
  ref.rate = 'mx',         # The mortality rate column in reference
  adjust = c('agegroup', 'year', 'sex')#, 
  #print = 'total'
)

print(sir_final_std)
# SIR (adjusted by agegroup, year, sex) with 95% confidence intervals (profile) 
# 
#  Total sir: 3.65 (3.52-3.78)
#  Total observed: 2996
#  Total expected: 820.46
#  Total person-years: 353826 
# 
#    observed expected   pyrs   sir sir.lo sir.hi p_value
#       <num>    <num>  <num> <num>  <num>  <num>   <num>
# 1:     2996   820.46 353826  3.65   3.52   3.78       0

cat("Check differences in age-sex-year strata \n")
keys_coh <- unique(c_SISTRAT_std_bins[, c("year","sex","agegroup")])
keys_ref <- unique(mx_national_clean[, c("year","sex","agegroup")])

stopifnot(
nrow(dplyr::anti_join(keys_coh, keys_ref, by=c("year","sex","agegroup")))==0
)


sir_tot
# SIR (adjusted by agegroup, year, sex) with 95% confidence intervals (profile) 
# 
# Total sir: 3.59 (3.46-3.72)
# Total observed: 2996
# Total expected: 834.72
# Total person-years: 353826 
# 
# observed expected   pyrs   sir sir.lo sir.hi p_value   EAR
# <num>    <num>  <num> <num>  <num>  <num>   <num> <num>
#   1:     2996   834.72 353826  3.59   3.46   3.72       0 6.108

sir_ci_phi_improved( sir_tot, extract_phi(c_SISTRAT_std_bins)) |>
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()

#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# 1: 3.589208 3.459544 3.723732  1.05589 3.59 (3.46–3.72)

sir_ci_phi_improved( sir_final_std, extract_phi(c_SISTRAT_std_bins)) |>
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# 1: 3.651626 3.519707 3.788489  1.05589 3.65 (3.52–3.79)



## DSR w/ 5-yr-bin lexis ---------------------------------------------------

### Correct pop-database for 5-yr-bin ---------------------------------------------------

last_attained_age <- dplyr::last(sort(unique(c_SISTRAT_std_bins$agegroup)))+5


proy_ine_com_2010_2020_corr<-
  rio::import("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/ine_estimaciones-y-proyecciones-2002-2035_base-2017_comunas0381d25bc2224f51b9770a705a434b74.csv?sfvrsn=b6e930a7_3&download=true")|> 
  tidyr::pivot_longer(cols = dplyr::starts_with("Poblacion"), 
                      names_to = "anio", 
                      values_to = "poblacion")|> 
  dplyr::mutate(anio= gsub("Poblacion ","",anio), anio=as.numeric(anio))|> 
  dplyr::filter(anio>=2010 & anio<=2020)|> 
  #2025-12-12: Modified agegroup
  dplyr::mutate(agegroup = floor(Edad / 5) * 5) |>
  dplyr::filter(agegroup >= 15 & agegroup <= 75)|> 
  dplyr::mutate(sex_rec= ifelse(`Sexo (1=Hombre 2=Mujer)`==2,"female","male"))|> 
  group_by(anio, agegroup, sex_rec)|>
  summarise(pop=sum(poblacion, na.rm=T))|>
  ungroup()|>
  rename("year"="anio")

proy_ine_com_2010_2020_corr_old <-
  rio::import("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/ine_estimaciones-y-proyecciones-2002-2035_base-2017_comunas0381d25bc2224f51b9770a705a434b74.csv?sfvrsn=b6e930a7_3&download=true") |>
  tidyr::pivot_longer(
    cols      = dplyr::starts_with("Poblacion"),
    names_to  = "anio",
    values_to = "poblacion"
  ) |>
  dplyr::mutate(
    year = base::as.integer(base::gsub("Poblacion\\s*", "", anio))
  ) |>
  dplyr::filter(year >= 2010, year <= 2020) |>
  dplyr::filter(Edad >= 18, Edad <= 75) |> #2025-12-13: correct age ranges
  dplyr::mutate(
    agegroup = dplyr::case_when(
      Edad < 30 ~ 18,
      Edad < 45 ~ 30,
      Edad < 60 ~ 45,
      TRUE      ~ 60
    ),
    sex = dplyr::if_else(`Sexo (1=Hombre 2=Mujer)` == 2, "Female", "Male")
  ) |>
  dplyr::group_by(year, sex, agegroup) |>
  dplyr::summarise(pop = base::sum(poblacion, na.rm = TRUE), .groups = "drop")

invisible("Backup")
proy_ine_com_2010_2020_corr_dput<- 
structure(list(year = c(2010, 2010, 2010, 2010, 2010, 2010, 2010, 
2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 
2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2011, 2011, 2011, 
2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 
2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 
2011, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 
2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 
2012, 2012, 2012, 2012, 2012, 2013, 2013, 2013, 2013, 2013, 2013, 
2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 
2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2014, 2014, 
2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 
2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 
2014, 2014, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 
2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 
2015, 2015, 2015, 2015, 2015, 2015, 2016, 2016, 2016, 2016, 2016, 
2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 
2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2017, 
2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 
2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 
2017, 2017, 2017, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 
2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 
2018, 2018, 2018, 2018, 2018, 2018, 2018, 2019, 2019, 2019, 2019, 
2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 
2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 
2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 
2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 
2020, 2020, 2020, 2020), agegroup = c(15, 15, 20, 20, 25, 25, 
30, 30, 35, 35, 40, 40, 45, 45, 50, 50, 55, 55, 60, 60, 65, 65, 
70, 70, 75, 75, 15, 15, 20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 
45, 45, 50, 50, 55, 55, 60, 60, 65, 65, 70, 70, 75, 75, 15, 15, 
20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 45, 45, 50, 50, 55, 55, 
60, 60, 65, 65, 70, 70, 75, 75, 15, 15, 20, 20, 25, 25, 30, 30, 
35, 35, 40, 40, 45, 45, 50, 50, 55, 55, 60, 60, 65, 65, 70, 70, 
75, 75, 15, 15, 20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 45, 45, 
50, 50, 55, 55, 60, 60, 65, 65, 70, 70, 75, 75, 15, 15, 20, 20, 
25, 25, 30, 30, 35, 35, 40, 40, 45, 45, 50, 50, 55, 55, 60, 60, 
65, 65, 70, 70, 75, 75, 15, 15, 20, 20, 25, 25, 30, 30, 35, 35, 
40, 40, 45, 45, 50, 50, 55, 55, 60, 60, 65, 65, 70, 70, 75, 75, 
15, 15, 20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 45, 45, 50, 50, 
55, 55, 60, 60, 65, 65, 70, 70, 75, 75, 15, 15, 20, 20, 25, 25, 
30, 30, 35, 35, 40, 40, 45, 45, 50, 50, 55, 55, 60, 60, 65, 65, 
70, 70, 75, 75, 15, 15, 20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 
45, 45, 50, 50, 55, 55, 60, 60, 65, 65, 70, 70, 75, 75, 15, 15, 
20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 45, 45, 50, 50, 55, 55, 
60, 60, 65, 65, 70, 70, 75, 75), sex_rec = c("female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male", "female", "male", "female", 
"male", "female", "male", "female", "male", "female", "male", 
"female", "male", "female", "male"), pop = c(718807L, 743747L, 
700844L, 721712L, 651747L, 665800L, 636910L, 643269L, 620777L, 
618466L, 615928L, 604291L, 593977L, 572682L, 532223L, 502241L, 
460758L, 423279L, 353217L, 316501L, 282447L, 236770L, 227215L, 
173906L, 184146L, 126082L, 704622L, 729167L, 722559L, 744315L, 
656909L, 671355L, 639244L, 646359L, 629885L, 628553L, 613669L, 
603028L, 603375L, 583032L, 543530L, 513808L, 475741L, 438299L, 
371129L, 332825L, 291821L, 246303L, 233943L, 180435L, 185095L, 
127269L, 691663L, 715867L, 737382L, 759766L, 663801L, 678573L, 
644708L, 652555L, 638013L, 637599L, 611938L, 602296L, 610683L, 
591302L, 555257L, 525919L, 489520L, 452103L, 389821L, 349904L, 
302659L, 257197L, 240798L, 187193L, 187312L, 129498L, 679315L, 
703097L, 741630L, 764144L, 674610L, 690054L, 651097L, 659952L, 
642257L, 642948L, 612162L, 603593L, 615037L, 596624L, 567153L, 
538337L, 501908L, 464468L, 409230L, 367507L, 314255L, 268797L, 
247697L, 194070L, 190985L, 133063L, 667402L, 690764L, 736894L, 
759230L, 693814L, 710225L, 658102L, 668023L, 644042L, 645919L, 
616382L, 608948L, 616705L, 599261L, 579059L, 550876L, 513590L, 
476158L, 429931L, 386049L, 325502L, 280226L, 254914L, 201286L, 
195997L, 137737L, 655486L, 678381L, 727914L, 749950L, 715607L, 
733050L, 664676L, 675697L, 644932L, 648049L, 624417L, 618168L, 
615823L, 599371L, 590184L, 562763L, 524839L, 487452L, 449246L, 
403299L, 338528L, 293235L, 262977L, 209301L, 201998L, 143246L, 
645120L, 667637L, 715197L, 737079L, 739938L, 758947L, 671990L, 
684650L, 648412L, 653153L, 634081L, 629195L, 614001L, 598633L, 
599814L, 573252L, 536313L, 499023L, 464218L, 418025L, 356062L, 
308815L, 272134L, 218321L, 208499L, 149208L, 637170L, 659655L, 
708070L, 731079L, 763405L, 786166L, 685707L, 702548L, 657944L, 
665878L, 644529L, 641778L, 613900L, 599846L, 608172L, 582507L, 
548689L, 511526L, 478319L, 431806L, 374526L, 325215L, 282746L, 
228621L, 215167L, 155410L, 632215L, 654197L, 710154L, 734072L, 
783826L, 810309L, 709153L, 730734L, 672873L, 684911L, 654543L, 
654195L, 618340L, 605717L, 616031L, 590843L, 563490L, 525949L, 
492992L, 445526L, 395171L, 343220L, 294932L, 240260L, 222282L, 
162027L, 625645L, 647322L, 711606L, 735379L, 800590L, 827193L, 
745995L, 769391L, 691342L, 705386L, 663414L, 665145L, 627382L, 
616547L, 620912L, 597164L, 577392L, 540699L, 505861L, 458457L, 
416071L, 361643L, 306258L, 251433L, 229448L, 168861L, 619434L, 
640628L, 710171L, 733010L, 811792L, 837355L, 785982L, 810229L, 
709886L, 725354L, 671719L, 675288L, 640360L, 631153L, 623388L, 
601037L, 590614L, 554862L, 518362L, 471032L, 435687L, 378921L, 
319314L, 264075L, 237389L, 176396L)), class = c("tbl_df", "tbl", 
"data.frame"), row.names = c(NA, -286L))
proy_ine_com_2010_2020_corr_old<- 
structure(list(year = c(2010L, 2010L, 2010L, 2010L, 2010L, 2010L, 
2010L, 2010L, 2011L, 2011L, 2011L, 2011L, 2011L, 2011L, 2011L, 
2011L, 2012L, 2012L, 2012L, 2012L, 2012L, 2012L, 2012L, 2012L, 
2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2014L, 
2014L, 2014L, 2014L, 2014L, 2014L, 2014L, 2014L, 2015L, 2015L, 
2015L, 2015L, 2015L, 2015L, 2015L, 2015L, 2016L, 2016L, 2016L, 
2016L, 2016L, 2016L, 2016L, 2016L, 2017L, 2017L, 2017L, 2017L, 
2017L, 2017L, 2017L, 2017L, 2018L, 2018L, 2018L, 2018L, 2018L, 
2018L, 2018L, 2018L, 2019L, 2019L, 2019L, 2019L, 2019L, 2019L, 
2019L, 2019L, 2020L, 2020L, 2020L, 2020L, 2020L, 2020L, 2020L, 
2020L), sex = c("Female", "Female", "Female", "Female", "Male", 
"Male", "Male", "Male", "Female", "Female", "Female", "Female", 
"Male", "Male", "Male", "Male", "Female", "Female", "Female", 
"Female", "Male", "Male", "Male", "Male", "Female", "Female", 
"Female", "Female", "Male", "Male", "Male", "Male", "Female", 
"Female", "Female", "Female", "Male", "Male", "Male", "Male", 
"Female", "Female", "Female", "Female", "Male", "Male", "Male", 
"Male", "Female", "Female", "Female", "Female", "Male", "Male", 
"Male", "Male", "Female", "Female", "Female", "Female", "Male", 
"Male", "Male", "Male", "Female", "Female", "Female", "Female", 
"Male", "Male", "Male", "Male", "Female", "Female", "Female", 
"Female", "Male", "Male", "Male", "Male", "Female", "Female", 
"Female", "Female", "Male", "Male", "Male", "Male"), agegroup = c(18, 
30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 
30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 
30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 
30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 
30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 30, 45, 60, 18, 
30, 45, 60, 18, 30, 45, 60), pop = c(1650078L, 1873615L, 1586958L, 
902141L, 1695015L, 1866026L, 1498202L, 755214L, 1669576L, 1882798L, 
1622646L, 936942L, 1715573L, 1877940L, 1535139L, 788381L, 1685637L, 
1894659L, 1655460L, 974511L, 1732436L, 1892450L, 1569324L, 824176L, 
1696085L, 1905516L, 1684098L, 1013834L, 1743551L, 1906493L, 1599429L, 
861567L, 1705573L, 1918526L, 1709354L, 1054356L, 1753695L, 1922890L, 
1626295L, 900040L, 1713258L, 1934025L, 1730846L, 1096071L, 1761945L, 
1941914L, 1649586L, 939581L, 1720149L, 1954483L, 1750128L, 1139058L, 
1770106L, 1966998L, 1670908L, 980198L, 1733471L, 1988180L, 1770761L, 
1183561L, 1788395L, 2010204L, 1693879L, 1021982L, 1754688L, 2036569L, 
1797861L, 1232668L, 1814114L, 2069840L, 1722509L, 1066886L, 1770862L, 
2100751L, 1825686L, 1279455L, 1830114L, 2139922L, 1754410L, 1111056L, 
1778723L, 2167587L, 1854362L, 1326774L, 1835738L, 2210871L, 1787052L, 
1155575L)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
-88L))

### Make weights by year ---------------------------------------------------

weights_corr <- proy_ine_com_2010_2020_corr |>
  dplyr::group_by(year, sex_rec, agegroup) |>
  dplyr::summarise(pop = sum(pop), .groups = "drop") |>
  #dplyr::group_by(year) |>
  dplyr::mutate(weights = pop / sum(pop)) |>
  #dplyr::ungroup() |>
  dplyr::rename(sex = sex_rec) |>
  dplyr::select(year, sex, agegroup, weights)|>
  dplyr::mutate(
    sex = stringr::str_to_title(sex)  # "female" -> "Female"
  )
cat("Format in data.table\n")
weights_corr <- data.table::as.data.table(weights_corr)[
  , .(year        = as.integer(year),               # num o int
      agegroup    = as.integer(agegroup),
      sex         = factor(sex, levels = c("Male","Female")),  # ¡factor!
      weights     = as.numeric(weights))            # num
]

cat("Weights for old agegroups\n")
weights_corr_old <- proy_ine_com_2010_2020_corr_old |>
  dplyr::group_by(year, sex, agegroup) |>
  dplyr::summarise(pop = sum(pop), .groups = "drop") |>
  #dplyr::group_by(year) |>
  dplyr::mutate(weights = pop / sum(pop)) |>
  #dplyr::ungroup() |>
  dplyr::rename(sex = sex) |>
  dplyr::select(year, sex, agegroup, weights)|>
  dplyr::mutate(
    sex = stringr::str_to_title(sex)  # "female" -> "Female"
  )
cat("Format in data.table\n")
weights_corr_old <- data.table::as.data.table(weights_corr_old)[
  , .(year        = as.integer(year),               # num o int
      agegroup    = as.integer(agegroup),
      sex         = factor(sex, levels = c("Male","Female")),  # ¡factor!
      weights     = as.numeric(weights))            # num
]
cat("Sum 1?\n")
# 2) Pesos deben sumar 1 POR AÑO
weights_corr |>
  dplyr::group_by(year) |>
  dplyr::summarise(s = sum(weights))

#“What would the rate be if my cohort had the overall 2010–2020 Chilean 
#population structure (year×sex×age) ?”

# “For direct standardization, we used joint weights derived from INE population 
# projections (2010–2020), pooled across years so that the standard reflects the 
# overall calendar-year–sex–age distribution during the study period (weights sum to
# 1 across all year×sex×age strata).”

### DSR ---------------------------------------------------

dsr_tot <- popEpi::rate(
  data    = c_SISTRAT_std_bins,
  obs     = "from0to1",
  pyrs    = "pyrs",
  adjust  = c("year","sex","agegroup"),
  weights = weights_corr
)

dsr_tot$rate_1k <- dsr_tot$rate * 1e3

cat("Previous rate:\n")
DSR_1k
#[1] "13.1 (8.1–18.1)"

#Eso te afecta principalmente SE/IC, no el punto rate.adj. 
#Si quieres ser conservador, usa phi <- max(1, phi) al formatear intervalos.

DSR_1k_corr_corr <- mapply(
  dsr_format_corr,                 # FUN
  dsr_tot$rate.adj,           # primer vector (rate)
  dsr_tot$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    #phi    = extract_phi_dir(c_SISTRAT_std_bins),
    #opc. conservadora
    phi    = max(1,  extract_phi_dir(c_SISTRAT_std_bins)),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

DSR_1k_corr_corr 
#[1] "10.638095 (8.637565–13.101965)"


message("#_#_#_#_#_")
message("There no people with attained age 75-80: ")
message("#_#_#_#_#_")
c_SISTRAT_std_bins |> 
  dplyr::filter(agegroup==75) |> 
  dplyr::filter(at.risk>0) |> nrow()

c_SISTRAT_std_bins %>%
  filter(agegroup == 75) %>%
  summarise(
    n       = n(),
    n_na    = sum(is.na(at.risk)),
    n_zero  = sum(at.risk == 0, na.rm = TRUE),
    min_py  = min(pyrs, na.rm = TRUE),
    max_py  = max(pyrs, na.rm = TRUE),
    deaths  = sum(replace_na(from0to1, 0L))
  )
c_SISTRAT_std_bins %>%
  mutate(deaths = tidyr::replace_na(from0to1, 0L)) %>%
  summarise(
    min_pyrs_any_death = min(pyrs[deaths > 0], na.rm = TRUE),
    n_death_with_pyrs_lt5 = sum(deaths > 0 & pyrs < 5, na.rm = TRUE)
  )
bad_cell <- c_SISTRAT_std_bins %>%
  mutate(deaths = tidyr::replace_na(from0to1, 0L),
         rate   = deaths / pyrs) %>%
  filter(deaths > 0, pyrs < 5) %>%
  arrange(pyrs)

bad_cell

#   min_pyrs_any_death n_death_with_pyrs_lt5
# 1           2.063014                     1
message("#_#_#_#_#_")


#### DSR 5-yr bin pooled yr --------------------------------------------------

warning("Pooling by year to get the correct 5-yr binned standardized rates\n")
dsr_pool_se <- c_SISTRAT_std_bins %>%
  mutate(deaths = tidyr::replace_na(from0to1, 0L)) %>%
  group_by(sex, agegroup) %>%
  summarise(deaths = sum(deaths),
            pyrs   = sum(pyrs),
            .groups = "drop") %>%
  left_join(
    weights_corr %>%
      group_by(sex, agegroup) %>%
      summarise(weights = sum(weights), .groups="drop"),
    by = c("sex","agegroup")
  ) %>%
  mutate(
    rate = deaths / pyrs,
    var_component = (weights^2) * deaths / (pyrs^2)
  ) %>%
  summarise(
    dsr = sum(weights * rate, na.rm = TRUE),
    se  = sqrt(sum(var_component, na.rm = TRUE)),
    lcl = dsr - 1.96 * se,
    ucl = dsr + 1.96 * se
  )

dsr_pool_se
#      dsr       se     lcl    ucl
#    <dbl>    <dbl>   <dbl>  <dbl>
# 1 0.0109 0.000569 0.00975 0.0120


warning("As a sensitivity analysis to mitigate instability from sparse age–sex–year")
warning("cells in 5-year bins, we (i) pooled person-time across years (age×sex")
warning("standardization unchanged) and (ii) estimated smoothed stratum-specific")
warning("rates using a Poisson model with log(PY) offset; both approaches reduced")
warning("the influence of extreme low-PY cells (e.g., a single stratum contributing") 
warning("9.4% of the crude DSR was reduced to 0.7% after smoothing) without materially changing the overall DSR.")

sprintf("%1.2f",dsr_pool_se$dsr*1e3)
#[1] "10.86"

dsr_format_corr(rate= dsr_pool_se$dsr, 
                se= dsr_pool_se$se, 
                phi=extract_phi(c_SISTRAT_std_bins), 
                factor=1e3, digits=1)
#[1] "10.9 (9.8–12.1)"

## DSR & SMR w/ 5-yr-bin lexis, by age -------------------------------------------

## Construct weights

# INE: year, agegroup (5y: 15,20,...), sex_rec (male/female), pop
proy_base <- proy_ine_com_2010_2020_corr |>
  dplyr::mutate(
    year = base::as.integer(year),
    agegroup = base::as.numeric(agegroup),
    sex = stringr::str_to_title(sex_rec)   # "Male"/"Female"
  )

# 5y attained-age (15,20,...,75) -> old bins (18,30,45,60)
to_old_agegroup <- function(age5) {
  dplyr::case_when(
    age5 < 30 ~ 18,
    age5 < 45 ~ 30,
    age5 < 60 ~ 45,
    TRUE      ~ 60
  )
}

message("#_#_#_#_#_\n Transform to 15-year bins \n#_#_#_#_#_\n")
c_SISTRAT_std_old <- c_SISTRAT_std_bins |>
  dplyr::mutate(agegroup_old = to_old_agegroup(agegroup)) |>
  dplyr::group_by(year, sex, agegroup_old) |>
  dplyr::summarise(
    pyrs     = base::sum(pyrs),
    from0to1 = base::sum(from0to1),
    .groups  = "drop"
  )

### Constructing weights  ======================

# Named weights vector in the order of levels you pass
make_w_named <- function(df, var, lev) {
  tmp <- df |>
    dplyr::summarise(pop = base::sum(pop), .by = dplyr::all_of(var)) |>
    dplyr::mutate(w = pop / base::sum(pop))
  
  tmp <- tmp[base::match(lev, tmp[[var]]), , drop = FALSE]
  stats::setNames(tmp$w, tmp[[var]])
}
#________________
# Cohort: real levels
#________________
lev_year <- base::sort(base::unique(c_SISTRAT_std_old$year))
lev_sex  <- base::levels(c_SISTRAT_std_bins$sex)

#________________
# Make weights
#________________
proy_for_w <- proy_base |>
  dplyr::filter(year %in% lev_year)
w_year <- make_w_named(proy_for_w, "year", lev_year)
w_sex  <- make_w_named(proy_for_w, "sex",  lev_sex)

### DSR by old ages  ======================
extract_phi_by_age <- function(df, min_rows = 3) {
  df %>%
    dplyr::group_by(agegroup) %>%
    dplyr::reframe(
      phi = {
        d <- dplyr::pick(dplyr::everything()) %>%
          dplyr::mutate(from0to1 = tidyr::replace_na(from0to1, 0L)) %>%
          dplyr::filter(pyrs > 0)
        
        # too sparse to estimate dispersion
        if (nrow(d) < min_rows || sum(d$from0to1, na.rm = TRUE) == 0) {
          NA_real_
        } else {
          
          rhs <- character(0)
          if (dplyr::n_distinct(d$sex)  > 1) rhs <- c(rhs, "factor(sex)")
          if (dplyr::n_distinct(d$year) > 1) rhs <- c(rhs, "factor(year)")
          
          f <- stats::as.formula(
            paste("from0to1 ~", if (length(rhs)) paste(rhs, collapse = " + ") else "1")
          )
          
          m <- tryCatch(
            stats::glm(
              f,
              offset = log(d$pyrs),
              family = stats::poisson(),
              data   = d
            ),
            error = function(e) NULL
          )
          
          if (is.null(m)) {
            NA_real_
          } else {
            dfres <- stats::df.residual(m)
            if (is.na(dfres) || dfres <= 0) {
              NA_real_
            } else {
              sum(stats::residuals(m, type = "pearson")^2) / dfres
            }
          }
        }
      }
    )
}

phi_df_age_old <- extract_phi_by_age(c_SISTRAT_std_old |> dplyr::rename(agegroup = agegroup_old))
phi_df_age <- extract_phi_by_age(c_SISTRAT_std_bins)

dsr_age_old <- popEpi::rate(
  data    = c_SISTRAT_std_old,
  obs     = "from0to1",
  pyrs    = "pyrs",
  print   = "agegroup_old",
  adjust  = c("year","sex"),
  weights = list(year = w_year, sex = w_sex)
)
dsr_age <- popEpi::rate(
  data    = c_SISTRAT_std_bins,
  obs     = "from0to1",
  pyrs    = "pyrs",
  print   = "agegroup",
  adjust  = c("year","sex"),
  weights = list(year = w_year, sex = w_sex)
)

phi_vec_age <- tidyr::replace_na(base::pmax(1, phi_df_age$phi[match(dsr_age$agegroup, phi_df_age$agegroup)]),1)
phi_vec_age_old <- base::pmax(1, phi_df_age_old$phi[match(dsr_age_old$agegroup_old, phi_df_age_old$agegroup)])

dplyr::select(dsr_age_old, rate, rate.lo, rate.hi)|> 
  mutate_if(is.numeric, ~sprintf("%1.1f",.*1e3))
#   rate rate.lo rate.hi
# 1  2.9     2.5     3.3
# 2  5.8     5.5     6.2
# 3 16.4    15.5    17.3
# 4 31.5    28.4    34.9

warning("Is not feasible to conduct DSRs by 5-yr bins due to sparse data bias")

dsr_age_old<- cbind.data.frame(dplyr::as_tibble(dsr_age_old),phi= phi_vec_age_old) 

dsr_age_df<- cbind.data.frame(dplyr::as_tibble(dsr_age),phi= phi_vec_age) 

DSR_1k_corr_agegr_new_corr <- mapply(
  dsr_format_corr,
  dsr_age_df$rate.adj,
  dsr_age_df$SE.rate.adj,
  dsr_age_df$phi,
  MoreArgs = list(factor = 1e3, digits = 6, conf = .95)
)


DSR_1k_corr_agegr_corr <- mapply(
  dsr_format_corr,
  dsr_age_old$rate.adj,
  dsr_age_old$SE.rate.adj,
  dsr_age_old$phi,
  MoreArgs = list(factor = 1e3, digits = 6, conf = .95)
)

format_interval <- function(s) {
  # Encuentra todas las coincidencias de números decimales
  m <- gregexpr("[0-9]+\\.[0-9]+", s)
  regmatches(s, m) <- lapply(regmatches(s, m), function(nums) {
    formatC(as.numeric(nums), format = "f", digits = 1)
  })
  s
}

cat("\n#_#_#_#_#_#_#_\nResults of DSRs by age\n#_#_#_#_#_#_#_\n")
data.table::data.table(as.vector(sapply(DSR_1k_corr_agegr_corr, 
                                        format_interval)))
# 1:     2.9 (2.4–3.4)
# 2:     5.7 (4.7–6.9)
# 3:  14.2 (12.9–15.7)
# 4: 46.8 (20.7–106.1)

cat("5-yr binned (call wrong due to sparsity\n")
data.table::data.table(as.vector(sapply(DSR_1k_corr_agegr_new_corr, 
                                        format_interval)))
#                    V1
#                <char>
#  1:       0.0 (NA–NA)
#  2:     2.4 (1.6–3.4)
#  3:     3.1 (2.6–3.8)
#  4:     3.5 (2.8–4.5)
#  5:     5.2 (4.5–6.0)
#  6:   10.2 (5.8–18.1)
#  7:   11.1 (9.3–13.4)
#  8:  15.4 (13.4–17.8)
#  9:  20.1 (15.2–26.5)
# 10: 48.6 (21.2–111.6)
# 11:  21.4 (15.1–30.2)
# 12:  23.2 (11.1–48.2)
# 13:       0.0 (NA–NA)
cat("Previous DSRs by age groups\n")
# 2.9 (2.5–3.4)
# 5.7 (4.7–6.9)
# 14.2 (13.4–15.1)
# 46.7 (21.2–102.9)


### SMR by old ages ======================

c_SISTRAT_std_bins_wold <- c_SISTRAT_std_bins |>
  dplyr::mutate(agegroup_old = to_old_agegroup(agegroup))

smr_age_old <- 
popEpi::sir(c_SISTRAT_std_bins_wold, coh.obs = 'from0to1',
                        coh.pyrs = 'pyrs',
                        ref.data = mx_national_clean,
                        ref.rate = mx,
                        print = c("agegroup_old"),
                        adjust = c("year", "sex", "agegroup"), #2025-12-16: the original still adjusts for agegroup
                        test.type = "homogeneity",
                        conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
                        conf.level = 0.95, EAR = T)
# SIR (adjusted by year, sex, agegroup) with 95% confidence intervals (profile) 
# Test for homogeneity: p < 0.001 
# 
# Total sir: 3.65 (3.52-3.78)
# Total observed: 2996
# Total expected: 820.46
# Total person-years: 353826 
# 
# Clave <agegroup_old>
#   agegroup_old observed expected      pyrs   sir sir.lo sir.hi p_value    EAR
# <num>    <num>    <num>     <num> <num>  <num>  <num>   <num>  <num>
#   1:           18      224    69.63  77124.89  3.22   2.82   3.67       0  2.002
# 2:           30     1070   270.87 183190.97  3.95   3.72   4.19       0  4.362
# 3:           45     1343   341.29  82118.63  3.94   3.73   4.15       0 12.198
# 4:           60      359   138.68  11391.48  2.59   2.33   2.87       0 19.341

sr_1_age
# SIR (adjusted by agegroup, sex, year) with 95% confidence intervals (wald) 
# Test for homogeneity: p < 0.001 
# 
#  Total sir: 3.59 (3.46-3.72)
#  Total observed: 2996
#  Total expected: 834.72
#  Total person-years: 353826 
# 
# Clave <agegroup>
#    agegroup observed expected      pyrs   sir sir.lo sir.hi p_value    EAR
#       <num>    <num>    <num>     <num> <num>  <num>  <num>   <num>  <num>
# 1:       18      224    65.15  77124.89  3.44   3.02   3.92       0  2.060
# 2:       30     1070   276.04 183190.97  3.88   3.65   4.12       0  4.334
# 3:       45     1343   374.27  82118.63  3.59   3.40   3.79       0 11.797
# 4:       60      359   119.27  11391.48  3.01   2.71   3.34       0 21.045

smr_age <- popEpi::sir(
  coh.data = c_SISTRAT_std_bins,
  coh.obs  = "from0to1",
  coh.pyrs = "pyrs",
  ref.data = mx_national_clean,
  ref.rate = "mx",
  print    = "agegroup",
  adjust   = c("year","sex","agegroup")  # <- 5y internal
)
smr_age
# SIR (adjusted by year, sex, agegroup) with 95% confidence intervals (wald) 
# Test for homogeneity: p < 0.001 
# 
#  Total sir: 3.65 (3.52-3.78)
#  Total observed: 2996
#  Total expected: 820.46
#  Total person-years: 353826 
# 
# Clave <agegroup>
#     agegroup observed expected     pyrs   sir sir.lo sir.hi p_value
#        <num>    <num>    <num>    <num> <num>  <num>  <num>   <num>
#  1:       15        0     0.32   633.35  0.00   0.00    Inf  0.9998
#  2:       20       55    17.11 21601.73  3.22   2.47   4.19  0.0000
#  3:       25      169    52.21 54889.81  3.24   2.78   3.76  0.0000
#  4:       30      283    77.78 68691.19  3.64   3.24   4.09  0.0000
#  5:       35      365    93.97 64698.78  3.88   3.51   4.30  0.0000
#  6:       40      422    99.12 49800.99  4.26   3.87   4.68  0.0000
#  7:       45      469   113.56 38935.65  4.13   3.77   4.52  0.0000
#  8:       50      487   116.36 26397.47  4.19   3.83   4.57  0.0000
#  9:       55      387   111.37 16785.51  3.48   3.15   3.84  0.0000
# 10:       60      260    89.92  8554.90  2.89   2.56   3.27  0.0000
# 11:       65       88    44.05  2648.85  2.00   1.62   2.46  0.0000
# 12:       70       11     4.68   186.90  2.35   1.30   4.24  0.0046
# 13:       75        0     0.02     0.84  0.00   0.00    Inf  0.9998


smr_age_tbl <- cbind.data.frame(dplyr::as_tibble(smr_age), phi=phi_vec_age)
smr_age_old_tbl <- cbind.data.frame(dplyr::as_tibble(smr_age_old), phi=phi_vec_age_old)

cat("\n#_#_#_#_#_#_#_\nResults of SMRs by age\n#_#_#_#_#_#_#_\n")

smr_age_old_tbl
#   agegroup_old observed expected      pyrs    sir sir.lo sir.hi p_value      phi
# 1           18      224  69.6292  77124.89 3.2170 2.8139 3.6569       0 1.000000
# 2           30     1070 270.8657 183190.97 3.9503 3.7183 4.1917       0 1.286653
# 3           45     1343 341.2859  82118.63 3.9351 3.7284 4.1493       0 1.000000
# 4           60      359 138.6756  11391.48 2.5888 2.3301 2.8659       0 1.000000


sir_ci_phi_improved( smr_age_old, smr_age_old_tbl$phi)|> 
dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
        SIR, 
        CI_low, 
        CI_high)) |> 
  data.table::data.table()

#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# 1: 3.217041 2.822171 3.667160 1.000000 3.22 (2.82–3.67)
# 2: 3.950297 3.690735 4.228114 1.286653 3.95 (3.69–4.23)
# 3: 3.935117 3.730187 4.151306 1.000000 3.94 (3.73–4.15)
# 4: 2.588776 2.334370 2.870907 1.000000 2.59 (2.33–2.87)



cat("5-yr bins, unstable enough\n")
sir_ci_phi_improved( smr_age_tbl, smr_age_tbl$phi)|> 
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#          SIR   CI_low  CI_high phi_used            print
#        <num>    <num>    <num>    <num>           <char>
#  1: 0.000000 0.000000      NaN 1.000000  0.00 (0.00–NaN)
#  2: 3.215152 2.468457 4.187719 1.000000 3.22 (2.47–4.19)
#  3: 3.237101 2.784064 3.763859 1.000000 3.24 (2.78–3.76)
#  4: 3.638589 3.238429 4.088196 1.000000 3.64 (3.24–4.09)
#  5: 3.884285 3.505558 4.303928 1.000000 3.88 (3.51–4.30)
#  6: 4.257470 3.741111 4.845098 1.836381 4.26 (3.74–4.85)
#  7: 4.130077 3.725918 4.578076 1.294809 4.13 (3.73–4.58)
#  8: 4.185247 3.829566 4.573964 1.000000 4.19 (3.83–4.57)
#  9: 3.474981 3.108969 3.884084 1.247930 3.47 (3.11–3.88)
# 10: 2.891433 2.560495 3.265145 1.000000 2.89 (2.56–3.27)
# 11: 1.997766 1.621087 2.461972 1.000000 2.00 (1.62–2.46)
# 12: 2.349975 1.301417 4.243364 1.000000 2.35 (1.30–4.24)
# 13: 0.000000 0.000000      NaN 1.000000  0.00 (0.00–NaN)



cat("Previous SMRs by age groups\n")
# 18: 3.44 (2.73-4.33)
# 30: 3.88 (3.36-4.47)
# 45: 3.59 (3.33-3.87)
# 60: 3.01 (2.67-3.40)


## DSR & SMR w/ 5-yr-bin lexis, by sex -------------------------------------------

### Constructing weights  ======================
#________________
# Cohort: real levels
#________________
# For 5 -year bins
lev_age_5y  <- base::sort(base::unique(c_SISTRAT_std_bins$agegroup))
lev_year_5y <- base::sort(base::unique(c_SISTRAT_std_bins$year))
proy_5y <- proy_base |>
  dplyr::filter(year %in% lev_year_5y, agegroup %in% lev_age_5y) |>
  dplyr::summarise(pop = base::sum(pop), .by = c(year, agegroup))  # colapsa sex
#________________
# Make weights
#________________
# For 5 -year bins
w_year_5y <- make_w_named(proy_5y, "year", lev_year_5y)
w_age_5y  <- make_w_named(proy_5y, "agegroup", lev_age_5y)


### DSR by sex  ======================

extract_phi_by_sex <- function(df) {
  df |>
    dplyr::group_by(sex) |>
    dplyr::reframe(
      phi = {
        d <- dplyr::pick(dplyr::everything()) |>
          dplyr::filter(pyrs > 0)
        
        if (base::nrow(d) < 3) return(NA_real_)
        
        m <- stats::glm(
          from0to1 ~ factor(agegroup) + factor(year),
          offset = base::log(pyrs),
          family = stats::poisson(),
          data   = d
        )
        
        dfres <- stats::df.residual(m)
        if (dfres <= 0) return(NA_real_)
        
        base::sum(stats::residuals(m, type = "pearson")^2) / dfres
      },
      n_rows    = dplyr::n(),
      n_events  = base::sum(from0to1, na.rm = TRUE),
      pyrs_total = base::sum(pyrs, na.rm = TRUE)
    )
}

phi_sex_df  <- extract_phi_by_sex(c_SISTRAT_std_bins)

dsr_by_sex <- popEpi::rate(
  data    = c_SISTRAT_std_bins,
  obs     = "from0to1",
  pyrs    = "pyrs",
  print   = "sex",
  adjust  = c("year","agegroup"),
  weights = list(year = w_year_5y, agegroup = w_age_5y)
)

dplyr::select(dsr_by_sex, rate, rate.lo, rate.hi)|> 
  mutate_if(is.numeric, ~sprintf("%1.1f",.*1e3))
#   rate rate.lo rate.hi
# 1  8.9     8.6     9.3
# 2  7.0     6.5     7.6

phi_sex_vec <- pmax(1, phi_sex_df$phi[match(dsr_by_sex$sex, phi_sex_df$sex)])
dsr_by_sex <- cbind.data.frame(dplyr::as_tibble(dsr_by_sex),phi= phi_sex_vec) 

DSR_1k_corr_sex_corr <- mapply(
  dsr_format_corr,
  dsr_by_sex $rate.adj,
  dsr_by_sex $SE.rate.adj,
  phi     = pmax(1,phi_sex_vec)[match(dsr_by_sex$sex, phi_sex_df$sex)],
  MoreArgs = list(factor = 1e3, digits = 6, conf = .95)
)


cat("\n#_#_#_#_#_#_#_\nResults of DSRs by sex\n#_#_#_#_#_#_#_\n")
data.table::data.table(as.vector(sapply(DSR_1k_corr_sex_corr, 
                                        format_interval)))
# Male: 13.7 (9.5–19.9)
# Female:   7.9 (6.6–9.5)

cat("Previous DSRs by age groups\n")

# Male: 18.5 (8.1–42.0)
# Female: 9.1 (7.7–10.6)

### SMR by sex ======================

smr_sex_corr <- popEpi::sir(
  coh.data = c_SISTRAT_std_bins,
  coh.obs  = "from0to1",
  coh.pyrs = "pyrs",
  ref.data = mx_national_clean,
  ref.rate = "mx",
  print    = "sex",
  adjust   = c("year","sex","agegroup")  # <- 5y internal
)
smr_sex_corr
# SIR (adjusted by year, sex, agegroup) with 95% confidence intervals (profile) 
# Test for homogeneity: p < 0.001 
# 
#  Total sir: 3.65 (3.52-3.78)
#  Total observed: 2996
#  Total expected: 820.46
#  Total person-years: 353826 
# 
# Clave <sex>
#       sex observed expected      pyrs   sir sir.lo sir.hi p_value
#    <char>    <num>    <num>     <num> <num>  <num>  <num>   <num>
# 1: Female      600   107.72  85500.75  5.57   5.14   6.03       0
# 2:   Male     2396   712.73 268325.23  3.36   3.23   3.50       0


smr_sex_corr_tbl <- cbind.data.frame(dplyr::as_tibble(smr_sex_corr), phi=phi_sex_vec)

smr_sex_corr_tbl
#      sex observed expected      pyrs    sir sir.lo sir.hi p_value phi
# 1 Female      600 107.7233  85500.75 5.5698 5.1360 6.0275       0   1
# 2   Male     2396 712.7331 268325.23 3.3617 3.2289 3.4981       0   1


sir_ci_phi_improved( smr_sex_corr, smr_sex_corr_tbl$phi)|> 
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#                 SIR   CI_low  CI_high phi_used            print
#               <num>    <num>    <num>    <num>           <char>
# Female:    5.569826 5.141519 6.033812        1 5.57 (5.14–6.03)
# Male:      3.361707 3.229760 3.499044        1 3.36 (3.23–3.50)

cat("\n#_#_#_#_#_#_#_\nResults of SMRs by sex\n#_#_#_#_#_#_#_\n")
# Female: 5.47 (4.98-6.00)
# Male: 3.30 (3.14-3.48)



# Heterogeneity -----------------------------------------------------------

se_log_from_ci <- function(est, lo, hi, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf)/2)
  
  # proteger contra ceros
  eps <- .Machine$double.eps
  est <- base::pmax(est, eps)
  lo  <- base::pmax(lo,  eps)
  hi  <- base::pmax(hi,  eps)
  
  se_u <- (base::log(hi)  - base::log(est)) / z
  se_l <- (base::log(est) - base::log(lo))  / z
  
  # promedio por elemento (NO mean() global)
  0.5 * (se_u + se_l)
}
se_log_from_rate_se <- function(rate, se_rate, phi = 1) {
  se_rate_phi <- se_rate * base::sqrt(phi)
  se_rate_phi / rate
}

## Het. SMR By sex -----------------------------------------------------------

smr_sex_dat <- sir_ci_phi_improved( smr_sex_corr, smr_sex_corr_tbl$phi) |>
  dplyr::mutate(
    yi  = base::log(SIR),
    sei = se_log_from_ci(SIR, CI_low, CI_high, conf = 0.95)
  )

meta_smr_sex <- metafor::rma(
  yi = yi,
  sei = sei,
  method = "FE",
  data = smr_sex_dat
)

meta_smr_sex
# Fixed-Effects Model (k = 2)
# 
# I^2 (total heterogeneity / total variability):   99.18%
# H^2 (total variability / sampling variability):  122.33
# 
# Test for Heterogeneity:
#   Q(df = 1) = 122.3299, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval   ci.lb   ci.ub      
# 1.3136  0.0183  71.8990  <.0001  1.2778  1.3494  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
c(Q = sprintf("%1.2f",meta_smr_sex$QE), 
  df = meta_smr_sex$k - meta_smr_sex$p, 
  p = sprintf("%1.3f",meta_smr_sex$QEp), 
  I2 = sprintf("%1.1f",meta_smr_sex$I2))
#        Q       df        p       I2 
# "122.33"      "3"  "0.000"   "99.2" 

## Het. SMR By age -----------------------------------------------------------

smr_age_dat <- sir_ci_phi_improved( smr_age_old, smr_age_old_tbl$phi) |>
  dplyr::mutate(
    yi  = base::log(SIR),
    sei = se_log_from_ci(SIR,CI_low, CI_high, conf = 0.95)
  )

meta_smr_age <- metafor::rma(
  yi = yi,
  sei = sei,
  method = "FE",
  data = smr_age_dat
)

meta_smr_age
# Fixed-Effects Model (k = 4)
# 
# I^2 (total heterogeneity / total variability):   94.89%
# H^2 (total variability / sampling variability):  19.55
# 
# Test for Heterogeneity:
#   Q(df = 3) = 58.6535, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval   ci.lb   ci.ub      
# 1.3002  0.0190  68.2785  <.0001  1.2629  1.3375  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
c(Q = sprintf("%1.2f",meta_smr_age$QE), 
  df = meta_smr_age$k - meta_smr_age$p, 
  p = sprintf("%1.3f",meta_smr_age$QEp), 
  I2 = sprintf("%1.1f",meta_smr_age$I2))
#       Q      df       p      I2 
# "58.65"     "3" "0.000"  "94.9" 

## Het. DSR By sex -----------------------------------------------------------

dsr_sex_dat <- dsr_by_sex |>
  dplyr::mutate(sex = base::as.character(sex)) |>
  dplyr::mutate(
    yi  = base::log(rate.adj),
    sei = se_log_from_rate_se(rate.adj, SE.rate.adj, phi = phi)
  )

meta_dsr_sex <- metafor::rma(
  yi = yi,
  sei = sei,
  method = "FE",
  data = dsr_sex_dat
)

meta_dsr_sex
# Fixed-Effects Model (k = 2)
# 
# I^2 (total heterogeneity / total variability):   85.38%
# H^2 (total variability / sampling variability):  6.84
# 
# Test for Heterogeneity:
# Q(df = 1) = 6.8422, p-val = 0.0089
# 
# Model Results:
# 
# estimate      se      zval    pval    ci.lb    ci.ub      
#  -4.7312  0.0828  -57.1516  <.0001  -4.8935  -4.5690  *** 
# 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
c(Q = sprintf("%1.2f",meta_dsr_sex$QE), 
  df = meta_dsr_sex$k - meta_dsr_sex$p, 
  p = sprintf("%1.3f",meta_dsr_sex$QEp), 
  I2 = sprintf("%1.1f",meta_dsr_sex$I2))
 #      Q      df       p      I2 
 # "6.84"     "1" "0.009"  "85.4" 

## Het. DSR By age -----------------------------------------------------------

dsr_age_dat <- dsr_age_old |>
  dplyr::mutate(
    yi  = base::log(rate.adj),
    sei = se_log_from_rate_se(rate.adj, SE.rate.adj, phi = phi)
  )

meta_dsr_age <- metafor::rma(yi = yi, sei = sei, method = "FE", data = dsr_age_dat)
meta_dsr_age
# Fixed-Effects Model (k = 4)
# 
# I^2 (total heterogeneity / total variability):   98.91%
# H^2 (total variability / sampling variability):  91.89
# 
# Test for Heterogeneity:
# Q(df = 3) = 275.6673, p-val < .0001
# 
# Model Results:
# 
# estimate      se       zval    pval    ci.lb    ci.ub      
#  -4.7003  0.0400  -117.3948  <.0001  -4.7788  -4.6219  *** 
# 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

c(Q = meta_dsr_age$QE, 
  df = meta_dsr_age$k - meta_dsr_age$p, 
  p = meta_dsr_age$QEp, 
  I2 = meta_dsr_age$I2)
c(Q = sprintf("%1.2f",meta_dsr_age$QE), 
  df = meta_dsr_sex$k - meta_dsr_age$p, 
  p = sprintf("%1.3f",meta_dsr_age$QEp), 
  I2 = sprintf("%1.1f",meta_dsr_age$I2))
#        Q       df        p       I2 
# "275.67"      "1"  "0.000"   "98.9" 



# SMR por sexo: I² ~ 99% (Q enorme)
# SMR por edad: I² ~ 95%
# DSR por edad: I² ~ 99%


### Pairwise diffs, by age -------------------------------------------

pairwise_smr_test <- function(smrs, lowers, uppers,
                              alpha = 0.05, a = NULL,
                              ci_level = 0.95,
                              p_adjust = "holm") {
  # allow legacy 'a='
  if (!is.null(a)) alpha <- a
  
  stopifnot(
    length(smrs) == length(lowers),
    length(smrs) == length(uppers),
    ci_level > 0, ci_level < 1
  )
  
  # z corresponding to the CI level (usually 95%)
  z_ci <- qnorm(1 - (1 - ci_level) / 2)
  
  # SE on log scale from log-normal CI
  se_log <- (log(uppers) - log(lowers)) / (2 * z_ci)
  
  n <- length(smrs)
  group_names <- paste("Group", seq_len(n))
  comparisons <- combn(n, 2, simplify = FALSE)
  
  results <- lapply(comparisons, function(pair) {
    i <- pair[1]; j <- pair[2]
    
    diff_log <- log(smrs[i]) - log(smrs[j])
    se_diff  <- sqrt(se_log[i]^2 + se_log[j]^2)
    
    data.frame(
      group1 = group_names[i],
      group2 = group_names[j],
      smr1 = smrs[i],
      smr2 = smrs[j],
      difference = smrs[i] - smrs[j],        # keep your original column
      ratio_1_over_2 = exp(diff_log),        # more appropriate for ratios
      se_diff = se_diff,
      z = abs(diff_log) / se_diff
    )
  }) |> do.call(what = rbind)
  
  results$p_unadj <- 2 * pnorm(-results$z)
  results$p_holm  <- p.adjust(results$p_unadj, method = p_adjust)
  
  results
}

dsr_sep <- strcapture("^\\s*([0-9.]+)\\s*\\(\\s*([0-9.]+)\\s*[–-]\\s*([0-9.]+)\\s*\\)\\s*$", 
                      DSR_1k_corr_agegr_corr, 
                      proto = list(estimate = numeric(), 
                                   ci_low = numeric(), 
                                   ci_high = numeric()))

age_labels <- c("18-29", "30-44", "45-59", "60+")

map_groups <- function(df, labels = age_labels) {
  df |>
    dplyr::mutate(
      group1 = ifelse(grepl("^Group", group1), labels[as.integer(sub("Group ", "", group1))], group1),
      group2 = ifelse(grepl("^Group", group2), labels[as.integer(sub("Group ", "", group2))], group2)
    )
}

tab_pairwise <- rbind.data.frame(
  tibble::tibble(
    group1     = "SMR",
    group2     = NA_character_,
    smr1       = NA_real_,
    smr2       = NA_real_,
    difference = NA_real_,
    ratio_1_over_2 = NA_real_,
    se_diff    = NA_real_,
    z          = NA_real_,
    p_unadj    = NA_real_,
    p_holm     = NA_real_
  ),
  pairwise_smr_test(
    smrs   = sir_ci_phi_improved( smr_age_old, smr_age_old_tbl$phi)[,1],
    lowers = sir_ci_phi_improved( smr_age_old, smr_age_old_tbl$phi)[,2],
    uppers = sir_ci_phi_improved( smr_age_old, smr_age_old_tbl$phi)[,3]
  ) |> map_groups(),
  tibble::tibble(
    group1     = "DSR",
    group2     = NA_character_,
    smr1       = NA_real_,
    smr2       = NA_real_,
    difference = NA_real_,
    ratio_1_over_2 = NA_real_,
    se_diff    = NA_real_,
    z          = NA_real_,
    p_unadj    = NA_real_,
    p_holm     = NA_real_
  ),
  pairwise_smr_test(
    smrs   = dsr_sep$estimate,
    lowers = dsr_sep$ci_low,
    uppers = dsr_sep$ci_high
  ) |> map_groups()
) |>
  dplyr::rename(estimate1 = smr1, estimate2 = smr2) |>
  dplyr::mutate(dplyr::across(c(se_diff, z), ~ round(., 2)))

knitr::kable(tab_pairwise, "markdown", caption = "Pairwise comparison, age groups")

# |group1 |group2 | estimate1| estimate2|  difference| ratio_1_over_2| se_diff|     z|   p_unadj|    p_holm|
# |:------|:------|---------:|---------:|-----------:|--------------:|-------:|-----:|---------:|---------:|
# |SMR    |NA     |        NA|        NA|          NA|             NA|      NA|    NA|        NA|        NA|
# |18-29  |30-44  |  3.217041|  3.950297|  -0.7332560|      0.8143795|    0.08|  2.73| 0.0063795| 0.0209779|
# |18-29  |45-59  |  3.217041|  3.935117|  -0.7180760|      0.8175211|    0.07|  2.79| 0.0052445| 0.0209779|
# |18-29  |60+    |  3.217041|  2.588776|   0.6282656|      1.2426883|    0.09|  2.55| 0.0107161| 0.0214321|
# |30-44  |45-59  |  3.950297|  3.935117|   0.0151800|      1.0038576|    0.04|  0.09| 0.9304696| 0.9304696|
# |30-44  |60+    |  3.950297|  2.588776|   1.3615216|      1.5259327|    0.06|  6.69| 0.0000000| 0.0000000|
# |45-59  |60+    |  3.935117|  2.588776|   1.3463416|      1.5200689|    0.06|  7.05| 0.0000000| 0.0000000|
# |DSR    |NA     |        NA|        NA|          NA|             NA|      NA|    NA|        NA|        NA|
# |18-29  |30-44  |  2.883079|  5.683480|  -2.8004010|      0.5072735|    0.13|  5.04| 0.0000005| 0.0000014|
# |18-29  |45-59  |  2.883079| 14.212263| -11.3291840|      0.2028585|    0.10| 15.32| 0.0000000| 0.0000000|
# |18-29  |60+    |  2.883079| 46.833428| -43.9503490|      0.0615603|    0.43|  6.53| 0.0000000| 0.0000000|
# |30-44  |45-59  |  5.683480| 14.212263|  -8.5287830|      0.3998997|    0.11|  8.25| 0.0000000| 0.0000000|
# |30-44  |60+    |  5.683480| 46.833428| -41.1499480|      0.1213552|    0.43|  4.92| 0.0000009| 0.0000018|
# |45-59  |60+    | 14.212263| 46.833428| -32.6211650|      0.3034641|    0.42|  2.84| 0.0045613| 0.0045613|


# By strata ---------------------------------------------------------------

extract_phi_by <- function(df, by, rhs_vars) {
  by_sym <- rlang::sym(by)
  rhs <- base::paste(sprintf("factor(%s)", rhs_vars), collapse = " + ")
  fml <- stats::as.formula(base::paste("from0to1 ~", rhs))
  
  df |>
    dplyr::group_by(!!by_sym) |>
    dplyr::reframe(
      phi = {
        d <- dplyr::pick(dplyr::everything()) |>
          dplyr::filter(pyrs > 0)
        
        if (base::nrow(d) < 3) return(1)  # default conservador
        
        m <- stats::glm(
          formula = fml,
          offset  = base::log(pyrs),
          family  = stats::poisson(),
          data    = d
        )
        
        dfres <- stats::df.residual(m)
        if (dfres <= 0) return(1)
        
        base::sum(stats::residuals(m, type = "pearson")^2) / dfres
      }
    ) |>
    dplyr::rename(group = !!by_sym) |>
    dplyr::mutate(phi = base::pmax(1, phi))
}
run_dsr_by <- function(lexis_df, weights_df, print_var, digits=2) {

  # DSR adjusted by year+sex+5y agegroup (same as your main)
  dsr <- popEpi::rate(
    data    = lexis_df,
    obs     = "from0to1",
    pyrs    = "pyrs",
    print   = print_var,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df
  ) |>
    dplyr::as_tibble()
  
  phi_tbl <- extract_phi_by(lexis_df, by = print_var, rhs_vars = c("year","sex","agegroup"))
  
  z <- 1.959964
  
  dsr |>
    dplyr::rename(group = !!print_var) |>
    dplyr::left_join(phi_tbl, by = "group") |>
    dplyr::mutate(
      phi = dplyr::if_else(is.na(phi) | phi < 1, 1, phi),   # conservative + stable
      rate_1k = rate.adj * 1e3,      
      sei_log = dplyr::if_else(
        rate.adj > 0,
        (SE.rate.adj * sqrt(phi)) / rate.adj,
        NA_real_
      ),
      rate_1k_L = dplyr::if_else(
        rate.adj > 0,
        (rate.adj * exp(-z * sei_log)) * 1e3,
        NA_real_
      ),
      rate_1k_U = dplyr::if_else(
        rate.adj > 0,
        (rate.adj * exp(+z * sei_log)) * 1e3,
        NA_real_
      ),
      print = dplyr::if_else(
        rate.adj > 0,
        sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
                rate_1k, rate_1k_L, rate_1k_U),
        sprintf(paste0("%.", digits, "f (NA–NA)"), rate_1k)
      )
    )
}
run_dsr_by_agecat <- function(lexis_df, w_year, w_sex, conf.level = 0.95, digits = 2) {
  
  dsr <- popEpi::rate(
    data    = lexis_df,
    obs     = "from0to1",
    pyrs    = "pyrs",
    print   = "disch_age_cat",
    adjust  = c("year","sex"),
    weights = list(year = w_year, sex = w_sex)
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(group = disch_age_cat)
  
  phi_tbl <- extract_phi_by(
    lexis_df,
    by       = "disch_age_cat",
    rhs_vars = c("year","sex")
  )
  
  z <- stats::qnorm(1 - (1 - conf.level)/2)
  
  dsr |>
    dplyr::left_join(phi_tbl, by = "group") |>
    dplyr::mutate(
      phi = dplyr::if_else(is.na(phi) | phi < 1, 1, phi),  # conservative + stable
      
      rate_1k = rate.adj * 1e3,
      
      sei_log = dplyr::if_else(
        rate.adj > 0,
        (SE.rate.adj * sqrt(phi)) / rate.adj,
        NA_real_
      ),
      
      rate_1k_L = dplyr::if_else(
        rate.adj > 0,
        (rate.adj * exp(-z * sei_log)) * 1e3,
        NA_real_
      ),
      rate_1k_U = dplyr::if_else(
        rate.adj > 0,
        (rate.adj * exp(+z * sei_log)) * 1e3,
        NA_real_
      ),
      
      print = dplyr::if_else(
        rate.adj > 0,
        sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
                rate_1k, rate_1k_L, rate_1k_U),
        sprintf(paste0("%.", digits, "f (NA–NA)"), rate_1k)
      )
    )
}


run_smr_by <- function(lexis_df, mx_national_clean, print_var) {
  popEpi::sir(
    coh.data = lexis_df,
    coh.obs  = "from0to1",
    coh.pyrs = "pyrs",
    ref.data = mx_national_clean,
    ref.rate = "mx",
    print    = print_var,
    adjust   = c("year","sex","agegroup")
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(group = !!print_var)
}
sir_ci_phi_profile <- function(sir_tbl, phi, conf.level = 0.95,
                               sir = "sir", lo = "sir.lo", hi = "sir.hi",
                               obs = "observed") {
  
  z <- stats::qnorm(1 - (1 - conf.level)/2)
  
  stopifnot(all(c(sir, lo, hi, obs) %in% names(sir_tbl)))
  
  O <- base::as.numeric(sir_tbl[[obs]])
  th <- base::as.numeric(sir_tbl[[sir]])
  lo0 <- base::as.numeric(sir_tbl[[lo]])
  hi0 <- base::as.numeric(sir_tbl[[hi]])
  
  # SE(log SIR) estimado desde el CI "profile" original
  se_u <- (base::log(hi0) - base::log(th)) / z
  se_l <- (base::log(th) - base::log(lo0)) / z
  se_log <- base::rowMeans(cbind(se_u, se_l), na.rm = TRUE)
  
  # φ alineado
  phi <- base::as.numeric(phi)
  if (length(phi) == 1L) phi <- base::rep(phi, length(th))
  phi <- base::pmax(1, phi)  # conservador
  
  # Evita división por 0 / NA: si O==0 no hay forma “bonita” de φ-correct sin modelar distinto
  se_log_phi <- se_log * base::sqrt(phi)
  se_log_phi[!is.finite(se_log_phi)] <- NA_real_
  
  out <- sir_tbl
  out$sir.phi.lo <- base::exp(base::log(th) - z * se_log_phi)
  out$sir.phi.hi <- base::exp(base::log(th) + z * se_log_phi)
  
  out
}


## Format DB ------------------------------------------------------------------


clean_df2 <- clean_df |>
  dplyr::mutate(
    # 0/1 -> labels ( 1 = Residential, 0 = Ambulatory)
    setting = dplyr::if_else(res_plan == 1, "Residential", "Ambulatory"),
    setting = base::factor(setting, levels = c("Ambulatory","Residential")),
    
    completed = base::as.character(tr_compliance_status),
    completed = base::factor(completed, levels = c("Completed","Not completed")),
    
    licit_illicit = base::as.character(prim_sub_licit),
    licit_illicit = base::factor(licit_illicit, levels = c("licit","illicit")),
    
    disch_age_cat = base::as.character(disch_age_cat),
    disch_age_cat = base::factor(disch_age_cat, levels = c("18-29","30-44","45-59","60+")),
    
    # Sex: asegúrate que sea Male/Female como tus tasas nacionales
    sex = stringr::str_to_title(base::as.character(sex_rec)),
    sex = base::factor(sex, levels = c("Male","Female"))
  )

# ---- 2) 5-year attained-age Lexis (keep 2021 as boundary)

c_SISTRAT_5y <- popEpi::lexpand(
  data   = clean_df2,
  status = status,
  birth  = birth_date_rec,
  entry  = disch_date_rec6,
  exit   = death_date_rec,
  breaks = list(
    per = base::seq(2010, 2021, by = 1),
    age = age_breaks
  ),
  aggre = list(
    year    = per,
    agegroup= age,      # 5y attained-age keys (15,20,25,...)
    sex     = sex,
    
    # subgroup vars (constantes por individuo; quedan replicadas por split)
    setting      = setting,
    completed    = completed,
    licit_illicit= licit_illicit,
    disch_age_cat= disch_age_cat
  )
) 
warning("disch_age_cat es edad al alta (baseline) → perfecto para “preservar 
los grupos antiguos” aunque el Lexis use attained-age 5y internamente para el ajuste.")



## Run ------------------------------------------------------------------



w_year <- proy_base |>
  dplyr::summarise(pop = base::sum(pop), .by = year) |>
  dplyr::mutate(w = pop/base::sum(pop)) |>
  {\(d) stats::setNames(d$w, d$year)}()

w_sex <- proy_base |>
  dplyr::summarise(pop = base::sum(pop), .by = sex) |>
  dplyr::mutate(w = pop/base::sum(pop)) |>
  {\(d) stats::setNames(d$w, d$sex)}()



### DSR strata ---------------------------------------------------------------------

dsr_setting   <- run_dsr_by(c_SISTRAT_5y, weights_corr, "setting")
dsr_completed <- run_dsr_by(c_SISTRAT_5y, weights_corr, "completed")
dsr_licit     <- run_dsr_by(c_SISTRAT_5y, weights_corr, "licit_illicit")

as.data.frame(t(dsr_setting))|> 
  row_to_names(row_number = 1)
#                     Ambulatory       Residential
# from0to1                  2488               508
# pyrs                 302963.29          50862.68
# rate.adj           0.010541504       0.009730295
# SE.rate.adj       0.0011483391      0.0009103649
# rate.adj.lo        0.008514815       0.008100023
# rate.adj.hi         0.01305058        0.01168869
# rate               0.008212216       0.009987676
# SE.rate           0.0001646399      0.0004431315
# rate.lo            0.007895780       0.009155831
# rate.hi            0.008541334       0.010895097
# phi                   1.337197          1.270598
# rate_1k              10.541504          9.730295
# sei_log              0.1259695         0.1054614
# rate_1k_L             8.235258          7.913290
# rate_1k_U             13.49360          11.96451
# print       10.54 (8.24–13.49) 9.73 (7.91–11.96)

as.data.frame(t(dsr_completed))|> 
  row_to_names(row_number = 1)
#                    Completed      Not completed
# from0to1                 671               2325
# pyrs                83248.16          270577.82
# rate.adj         0.007657462        0.011931372
# SE.rate.adj     0.0007985046       0.0013069726
# rate.adj.lo      0.006241970        0.009626033
# rate.adj.hi      0.009393946        0.014788817
# rate             0.008060238        0.008592722
# SE.rate         0.0003111620       0.0001782048
# rate.lo          0.007472862        0.008250445
# rate.hi          0.008693782        0.008949200
# phi                 1.629178           1.442545
# rate_1k             7.657462          11.931372
# sei_log            0.1330996          0.1315651
# rate_1k_L           5.899161           9.219385
# rate_1k_U           9.939842          15.441121
# print       7.66 (5.90–9.94) 11.93 (9.22–15.44)

as.data.frame(t(dsr_licit))|> 
  row_to_names(row_number = 1)
#                           licit           illicit
# from0to1                   1742              1254
# pyrs                   107780.2          246045.8
# rate.adj            0.012818352       0.008784955
# SE.rate.adj        0.0008695156      0.0019052699
# rate.adj.lo         0.011222536       0.005742866
# rate.adj.hi          0.01464109        0.01343849
# rate                0.016162528       0.005096612
# SE.rate            0.0003872445      0.0001439239
# rate.lo             0.015421075       0.004822186
# rate.hi             0.016939631       0.005386655
# phi                    1.505375          1.000000
# rate_1k               12.818352          8.784955
# sei_log              0.08322764        0.21687872
# rate_1k_L              10.88902           5.74291
# rate_1k_U              15.08953          13.43838
# print       12.82 (10.89–15.09) 8.78 (5.74–13.44)

as.data.frame(t(run_dsr_by_agecat (c_SISTRAT_5y, w_year, w_sex)))|> 
  row_to_names(row_number = 1)
#                        18-29            30-44               45-59                  60+
# from0to1                 411             1182                1213                  190
# pyrs              124371.383       163653.066           60457.997             5343.532
# rate.adj         0.003132995      0.006748092         0.016515097          0.048785220
# SE.rate.adj     0.0002380580     0.0005104227        0.0007940633         0.0202363334
# rate.adj.lo      0.002699484      0.005818289         0.015029817          0.021637121
# rate.adj.hi      0.003636125      0.007826484         0.018147155          0.109996045
# rate             0.003304619      0.007222596         0.020063516          0.035557003
# SE.rate         0.0001630048     0.0002100799        0.0005760718         0.0025795761
# rate.lo          0.003000087      0.006822357         0.018965599          0.030844048
# rate.hi          0.003640062      0.007646316         0.021224992          0.040990096
# phi                 1.335390         2.111608            1.376811             1.000000
# rate_1k             3.132995         6.748092           16.515097            48.785220
# sei_log           0.08780659       0.10991468          0.05641716           0.41480459
# rate_1k_L           2.637659         5.440282           14.786274            21.637444
# rate_1k_U           3.721353         8.370290           18.446055           109.994402
# print       3.13 (2.64–3.72) 6.75 (5.44–8.37) 16.52 (14.79–18.45) 48.79 (21.64–109.99)



### SMR strata ---------------------------------------------------------------------

smr_setting   <- run_smr_by(c_SISTRAT_5y, mx_national_clean, "setting")
smr_completed <- run_smr_by(c_SISTRAT_5y, mx_national_clean, "completed")
smr_licit     <- run_smr_by(c_SISTRAT_5y, mx_national_clean, "licit_illicit")
smr_agecat    <- run_smr_by(c_SISTRAT_5y, mx_national_clean, "disch_age_cat")


smr_setting_phi <- smr_setting%>%
  dplyr::left_join(dplyr::select(dsr_setting, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
#   group       observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>          <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 Ambulatory      2488     717. 302963.  3.47   3.34   3.61       0  1.34       3.32       3.63 3.47 (3.32–3.63)
# 2 Residential      508     103.  50863.  4.91   4.50   5.35       0  1.27       4.45       5.42 4.91 (4.45–5.42)
smr_completed_phi <- smr_completed%>%
  dplyr::left_join(dplyr::select(dsr_completed, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
#   group         observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>            <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 Completed          671     245.  83248.  2.74   2.54   2.95       0  1.63       2.49       3.02 2.74 (2.49–3.02)
# 2 Not completed     2325     575. 270578.  4.04   3.88   4.21       0  1.44       3.85       4.24 4.04 (3.85–4.24)
smr_licit_phi <- smr_licit%>%
  dplyr::left_join(dplyr::select(dsr_licit, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
#   group   observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>      <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 licit       1742     379. 107780.  4.59   4.38   4.81       0  1.51       4.33       4.86 4.59 (4.33–4.86)
# 2 illicit     1254     441. 246046.  2.84   2.69   3.00       0  1          2.69       3.00 2.84 (2.69–3.00)
smr_agecat_phi <- smr_agecat%>%
  dplyr::left_join(subset(run_dsr_by_agecat(c_SISTRAT_5y, w_year, w_sex), select=c("group","phi")), 
                   by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
#   group observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>    <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 18-29      411    125.  124371.  3.28   2.97   3.60       0  1.34       2.93       3.66 3.28 (2.93–3.66)
# 2 30-44     1182    300.  163653.  3.94   3.72   4.17       0  2.11       3.63       4.28 3.94 (3.63–4.28)
# 3 45-59     1213    322.   60458.  3.77   3.56   3.98       0  1.38       3.53       4.02 3.77 (3.53–4.02)
# 4 60+        190     72.9   5344.  2.61   2.25   2.99       0  1          2.26       3.00 2.61 (2.26–3.00)

# By causes ---------------------------------------------------------------

assign_cause_cat_vec_corr <- function(diag1, diag2) {
  n <- length(diag1)
  
  # 1. Cleaner Key Extraction
  key3 <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x[is.na(x) | x == ""] <- NA_character_
    substr(x, 1, 3)
  }
  
  # 2. Vectorized ICD10 to Numeric
  # Maps A->1000 ... U->21000 ... Z->26000 reliably
  to_num <- function(k3) {
    let <- substr(k3, 1, 1)
    num <- suppressWarnings(as.integer(substr(k3, 2, 3)))
    # Ensure valid letter/number combo to avoid NA errors later
    valid <- !is.na(let) & !is.na(num) & let %in% LETTERS
    out <- rep(NA_integer_, length(k3))
    out[valid] <- match(let[valid], LETTERS) * 1000L + num[valid]
    out
  }
  
  k1 <- key3(diag1)
  k2 <- key3(diag2)
  n1 <- to_num(k1)
  n2 <- to_num(k2)
  
  # --- RANGE DEFINITIONS (Numeric) ---
  # V01(22001) - Y98(25098)
  
  # 3. External Cause Logic (Priority)
  # Check if diag1 OR diag2 is an external cause (V-Y)
  is_ext1 <- !is.na(n1) & n1 >= 22001 & n1 <= 25098
  is_ext2 <- !is.na(n2) & n2 >= 22001 & n2 <= 25098
  
  # If diag1 is external, use it. If not, but diag2 is, use diag2.
  kext <- ifelse(is_ext1, n1, ifelse(is_ext2, n2, NA_integer_))
  
  # Initialize Output
  out <- rep(NA_character_, n)
  
  # --- GROUP 1: EXTERNAL CAUSES ---
  # Assault (X85-Y09) -> 24085 - 25009
  out[!is.na(kext) & kext >= 24085 & kext <= 25009] <- "Assaults/ Aggressions / Homicide (X85–Y09)"
  
  # Self-harm (X60-X84) -> 24060 - 24084
  out[!is.na(kext) & kext >= 24060 & kext <= 24084] <- "Intentional self-harm (X60–X84)"
  
  # Transport (V01-V99) -> 22001 - 22099
  out[!is.na(kext) & kext >= 22001 & kext <= 22099] <- "Transport accidents (V01–V99)"
  
  # Other Unintentional (W00-X59) -> 23000 - 24059
  out[!is.na(kext) & kext >= 23000 & kext <= 24059] <- "Other unintentional external causes (W00–X59)"
  
  # Residual External (Any other V, W, X, Y code not caught above)
  out[!is.na(kext) & is.na(out)] <- "Other external causes (Y10–Y98)"
  
  # --- GROUP 2: UNDERLYING CAUSES (diag1) ---
  # Only process if 'out' is still NA (not external)
  
  # Infectious (A00-B99) -> 1000 - 2099
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 1000 & n1 <= 2099] <- "Infectious & parasitic (A00–B99)"
  
  # Neoplasms (C00-C96) -> 3000 - 3096 (Strict Malignant)
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 3000 & n1 <= 3096] <- "Malignant neoplasms (C00–C96)"
  
  # Endocrine (E00-E99) -> 5000 - 5099
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 5000 & n1 <= 5099] <- "Endocrine & metabolic (E00–E99)"
  
  # Mental (F01-F99) -> 6001 - 6099
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 6001 & n1 <= 6099] <- "Mental and behavioral (F01–F99)"
  
  # Nervous (G00-G99) -> 7000 - 7099
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 7000 & n1 <= 7099] <- "Nervous system (G00–G99)"
  
  # Circulatory (I00-I99) -> 9000 - 9099
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 9000 & n1 <= 9099] <- "Circulatory (I00–I99)"
  
  # Respiratory (J00-J99) -> 10000 - 10099
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 10000 & n1 <= 10099] <- "Respiratory (J00–J99)"
  
  # Digestive (K00-K93) -> 11000 - 11093
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 11000 & n1 <= 11093] <- "Digestive (K00–K93)"
  
  # Symptoms (R00-R99) -> 18000 - 18099
  na_mask <- is.na(out) & !is.na(n1)
  out[na_mask & n1 >= 18000 & n1 <= 18099] <- "Symptoms & signs (R00–R99)"
  
  # --- GROUP 3: CATCH-ALL FOR REMAINING ---
  # Corrected to include missing S, T, U, Z codes
  # U = COVID/Special (21000s)
  # S, T = Injury (19000s, 20000s)
  # Z = Health Factors (26000s)
  na_mask <- is.na(out) & !is.na(k1)
  let1 <- substr(k1[na_mask], 1, 1)
  
  # Explicitly added S, T, U, Z here
  other_letters <- c("D", "H", "L", "M", "N", "O", "P", "Q", "S", "T", "U", "Z")
  
  out[na_mask][let1 %in% other_letters] <- "Other underlying causes (D, H, L, M, N, O, P, Q, S, T, U)"
  
  # --- GROUP 4: FINAL RESIDUAL ---
  # If still NA, it means code was invalid or diag1 was missing.
  # Assign a label instead of NA so they aren't dropped from summaries.
  out[is.na(out)] <- "Unknown/Unclassified"
  
  out
}

cause_levels <- c(
  "Infectious & parasitic (A00–B99)",
  "Malignant neoplasms (C00–C96)",
  "Endocrine & metabolic (E00–E99)",
  "Mental and behavioral (F01–F99)",
  "Nervous system (G00–G99)",
  "Circulatory (I00–I99)",
  "Respiratory (J00–J99)",
  "Digestive (K00–K93)",
  "Symptoms & signs (R00–R99)",
  "Other underlying causes (D, H, L, M, N, O, P, Q, S, T, U)", 
  "Assaults/ Aggressions / Homicide (X85–Y09)",
  "Intentional self-harm (X60–X84)",
  "Transport accidents (V01–V99)",
  "Other unintentional external causes (W00–X59)",
  "Other external causes (Y10–Y98)",
  "Unknown/Unclassified"  # <--- ADD THIS LINE
)

# Flag external vs underlying (keeps original 15-category label)
flag_external_underlying <- function(cause_cat) {
  cc <- as.character(cause_cat)
  is_external <- cc %in% c(
    "Assaults/ Aggressions / Homicide (X85–Y09)",
    "Intentional self-harm (X60–X84)",
    "Transport accidents (V01–V99)",
    "Other unintentional external causes (W00–X59)",
    "Other external causes (Y10–Y98)"
  )
  
  dplyr::case_when(
    is.na(cc)        ~ NA_character_,
    is_external      ~ "External causes",
    TRUE             ~ "Underlying causes"
  )
}

# Collapse to 2 cause groups directly from diag1/diag2
assign_cause_group2_vec <- function(diag1, diag2) {
  cause_cat <- assign_cause_cat_vec_corr(diag1, diag2)
  flag_external_underlying(cause_cat)
}

age_to_5y <- function(age_years) 5L * base::floor(age_years / 5)

build_mx_national_by_cause <- function(
    deaths_nat_df, pop_df, years_keep, agegroups_keep,
    year_col = "ano_def", sex_col = "sexo", age_col = "edad_cant",
    diag1_col = "diag1", diag2_col = "diag2",
    pop_year_col = "year", pop_agegroup_col = "agegroup",
    pop_sex_col = NULL, pop_pop_col = "pop",
    age_upper_exclusive = NULL
) {
  # auto-detect pop sex column
  if (is.null(pop_sex_col)) {
    pop_sex_col <- intersect(c("sex_rec","sex"), names(pop_df))[1]
    if (is.na(pop_sex_col)) stop("No sex column in pop_df")
  }
  # precompute age bins
  keep_sorted <- sort(unique(as.integer(agegroups_keep)))
  if (is.null(age_upper_exclusive) && length(keep_sorted) >= 2 && median(diff(keep_sorted)) == 5) {
    age_upper_exclusive <- max(keep_sorted) + 5L
  }
  # sex lookup (vectorized, no recode)
  sex_map <- c(`1`="Male",`2`="Female",hombre="Male",male="Male",m="Male",
               mujer="Female",female="Female",f="Female")
  # convert to data.table ONCE
  dt_deaths <- data.table::as.data.table(deaths_nat_df)
  dt_pop    <- data.table::as.data.table(pop_df)
  # DEATHS: filter early, select minimal cols, compute in-place
  dt_deaths <- dt_deaths[get(year_col) %in% years_keep,
                         .(yr = as.integer(get(year_col)),
                           sx = tolower(as.character(get(sex_col))),
                           ag = as.numeric(get(age_col)),
                           d1 = get(diag1_col),
                           d2 = get(diag2_col))]
  # vectorized sex
  dt_deaths[, sex := sex_map[sx]]
  dt_deaths[is.na(sex), sex := NA_character_]
  dt_deaths[, sex := factor(sex, levels = c("Male","Female"))]
  # vectorized agegroup
  dt_deaths[, agegroup := keep_sorted[findInterval(ag, keep_sorted, rightmost.closed = TRUE)]]
  dt_deaths[findInterval(ag, keep_sorted, rightmost.closed = TRUE) == 0L, agegroup := NA_integer_]
  if (!is.null(age_upper_exclusive)) dt_deaths[ag >= age_upper_exclusive, agegroup := NA_integer_]
  # filter valid rows
  dt_deaths <- dt_deaths[agegroup %in% keep_sorted & !is.na(sex)]
  # cause_cat: vectorized if available, else mapply
  if (exists("assign_cause_cat_vec_corr", mode = "function")) {
    dt_deaths[, cause_cat := assign_cause_cat_vec_corr(d1, d2)]
  } else {
    dt_deaths[, cause_cat := mapply(assign_cause_cat, d1, d2)]
  }
  dt_deaths[, cause_cat := factor(cause_cat, levels = cause_levels)]
  dt_deaths <- dt_deaths[!is.na(cause_cat)]
  # aggregate deaths
  deaths_agg <- dt_deaths[, .(deaths = .N), by = .(year = yr, sex, agegroup, cause_cat)]
  # POP: filter + aggregate
  dt_pop <- dt_pop[get(pop_year_col) %in% years_keep & get(pop_agegroup_col) %in% keep_sorted]
  dt_pop[, `:=`(
    year     = as.integer(get(pop_year_col)),
    agegroup = as.integer(get(pop_agegroup_col)),
    sx       = tolower(as.character(get(pop_sex_col))),
    pop      = as.numeric(get(pop_pop_col))
  )]
  dt_pop[, sex := factor(sex_map[sx], levels = c("Male","Female"))]
  dt_pop <- dt_pop[!is.na(sex)]
  pop_agg <- dt_pop[, .(pop = sum(pop, na.rm = TRUE)), by = .(year, sex, agegroup)]
  # expand grid: pop x cause_levels
  grid <- data.table::CJ(
    year      = unique(pop_agg$year),
    sex       = factor(c("Male","Female"), levels = c("Male","Female")),
    agegroup  = keep_sorted,
    cause_cat = factor(cause_levels, levels = cause_levels)
  )
  # merge pop
  grid <- pop_agg[grid, on = .(year, sex, agegroup)]
  # merge deaths
  grid <- deaths_agg[grid, on = .(year, sex, agegroup, cause_cat)]
  # fill NA deaths with 0, compute rate
  grid[is.na(deaths), deaths := 0L]
  grid[, mx := deaths / pmax(pop, 1)]
  
  grid[, .(year, sex, agegroup, cause_cat, mx, deaths, pop)]
}

build_cohort_by_cause <- function(clean_df, lexis_df) {
  # PY desde Lexis (una vez)
  expo <- lexis_df |>
    tidytable::summarise(pyrs = sum(pyrs), .by = c(year, sex, agegroup))
  
  years_keep <- sort(unique(expo$year))
  ages_keep  <- sort(unique(expo$agegroup))
  
  map_age_to_bins <- function(age_years) {
    idx <- findInterval(age_years, ages_keep, rightmost.closed = TRUE)
    out <- ages_keep[idx]
    out[idx == 0L] <- NA_integer_
    out
  }
  # Deaths by cause (from clean_df; NO from Lexis)
  deaths <- clean_df |>
    tidytable::filter(status == 1, !is.na(death_date_rec), !is.na(birth_date_rec)) |>
    tidytable::mutate(
      year = as.integer(format(death_date_rec, "%Y")),
      age_years = as.numeric(difftime(death_date_rec, birth_date_rec, units = "days")) / 365.241,
      agegroup  = map_age_to_bins(age_years),
      sex = factor(stringr::str_to_title(as.character(sex_rec)), levels = c("Male","Female")),
      cause_cat = assign_cause_cat_vec_corr(diag1, diag2),
      cause_cat = factor(cause_cat, levels = cause_levels)
    ) |>
    tidytable::filter(
      year %in% years_keep,
      agegroup %in% ages_keep,
      !is.na(sex),
      !is.na(cause_cat)
    ) |>
    tidytable::summarise(from0to1 = n(), .by = c(year, sex, agegroup, cause_cat))
  
  tidyr::crossing(
    expo,
    cause_cat = factor(cause_levels, levels = cause_levels)
  ) |>
    tidytable::left_join(deaths, by = c("year","sex","agegroup","cause_cat")) |>
    tidytable::mutate(from0to1 = ifelse(is.na(from0to1), 0L, from0to1))
}
extract_phi_by <- function(df,
                           by,
                           rhs_vars = c("year","sex","agegroup"),
                           min_events = 20,
                           phi_fallback = 1) {
  
  needed <- c(by, rhs_vars, "from0to1", "pyrs")
  if (!all(needed %in% names(df))) {
    stop("Missing columns: ", paste(setdiff(needed, names(df)), collapse = ", "))
  }
  rhs <- paste(sprintf("factor(%s)", rhs_vars), collapse = " + ")
  fml <- stats::as.formula(paste("from0to1 ~", rhs))
  out <- df |>
    dplyr::group_by(.data[[by]]) |>
    dplyr::summarise(
      result = list({
        d_all <- dplyr::cur_data_all()
        n_events_val <- sum(d_all$from0to1, na.rm = TRUE)
        n_rows_val <- nrow(d_all)
        d <- d_all |> 
          dplyr::filter(pyrs > 0) |>
          dplyr::mutate(.offset = log(pmax(pyrs, .Machine$double.eps)))
        n_rows_used <- nrow(d)
        O <- sum(d$from0to1, na.rm = TRUE)
        
        phi_val <- NA_real_
        warning_msg <- NA_character_
        converged <- FALSE
        
        if (nrow(d) < 3L) {
          warning_msg <- sprintf("< 3 rows with pyrs > 0 (only %d)", nrow(d))
        } else if (!is.null(min_events) && is.finite(min_events) && O < min_events) {
          warning_msg <- sprintf("events (%d) < min_events (%d)", O, min_events)
        } else {
          res <- tryCatch({
            m <- stats::glm(
              formula = fml,
              family  = stats::poisson(),
              data    = d,
              offset  = .offset
            )
            dfres <- stats::df.residual(m)
            
            if (!is.finite(dfres) || dfres <= 0) {
              list(phi = NA_real_, converged = FALSE, warning = sprintf("df.residual invalid: %.2f", dfres))
            } else {
              list(
                phi = sum(stats::residuals(m, type = "pearson")^2) / dfres,
                converged = TRUE,
                warning = NA_character_
              )
            }
          }, error = function(e) {
            list(phi = NA_real_, converged = FALSE, warning = substr(e$message, 1, 80))
          }, warning = function(w) {
            # Capture warnings but still return result
            m <- stats::glm(
              formula = fml,
              family  = stats::poisson(),
              data    = d,
              offset  = .offset
            )
            dfres <- stats::df.residual(m)
            list(
              phi = if (is.finite(dfres) && dfres > 0) sum(stats::residuals(m, type = "pearson")^2) / dfres else NA_real_,
              converged = TRUE,
              warning = substr(w$message, 1, 80)
            )
          })
          
          phi_val <- res$phi
          converged <- res$converged
          warning_msg <- res$warning
        }
        
        list(
          n_events = n_events_val,
          n_rows = n_rows_val,
          n_rows_used = n_rows_used,
          phi_raw = phi_val,
          converged = converged,
          warning = warning_msg
        )
      }),
      .groups = "drop"
    ) |>
    tidyr::unnest_wider(result) |>
    dplyr::rename(group = 1)
  
  phi_fb <- as.numeric(phi_fallback)[1]
  if (!is.finite(phi_fb)) phi_fb <- 1
  
  out |>
    dplyr::mutate(
      phi = dplyr::if_else(is.finite(phi_raw), pmax(1, phi_raw), phi_fb)
    )
}
extract_phi_global <- function(df, rhs_vars = c("year","sex","agegroup")) {
  rhs <- base::paste(base::sprintf("factor(%s)", rhs_vars), collapse = " + ")
  fml <- stats::as.formula(base::paste("from0to1 ~", rhs))
  
  d <- df |>
    dplyr::filter(pyrs > 0)
  
  off <- base::log(base::pmax(d$pyrs, .Machine$double.eps))
  
  m <- stats::glm(fml, family = stats::poisson(), data = d, offset = off)
  phi <- base::sum(stats::residuals(m, type = "pearson")^2) / stats::df.residual(m)
  base::pmax(1, phi)
}

sir_ci_phi_profile <- function(sir_tbl, phi, conf.level = 0.95,
                               sir = "sir", lo = "sir.lo", hi = "sir.hi") {
  
  z <- stats::qnorm(1 - (1 - conf.level)/2)
  
  th  <- base::as.numeric(sir_tbl[[sir]])
  lo0 <- base::as.numeric(sir_tbl[[lo]])
  hi0 <- base::as.numeric(sir_tbl[[hi]])
  
  se_u <- (base::log(hi0) - base::log(th)) / z
  se_l <- (base::log(th) - base::log(lo0)) / z
  se_log <- base::rowMeans(cbind(se_u, se_l), na.rm = TRUE)
  
  phi <- base::as.numeric(phi)
  if (length(phi) == 1L) phi <- base::rep(phi, length(th))
  phi <- base::pmax(1, phi)
  
  se_log_phi <- se_log * base::sqrt(phi)
  
  out <- sir_tbl
  out$sir.phi.lo <- base::exp(base::log(th) - z * se_log_phi)
  out$sir.phi.hi <- base::exp(base::log(th) + z * se_log_phi)
  out
}

run_smr_by_cause <- function(coh_cause, mx_national_by_cause, phi_tbl, digits = 2, conf = 0.95) {
  
  smr <- popEpi::sir(
    coh.data = coh_cause,
    coh.obs  = "from0to1",
    coh.pyrs = "pyrs",
    ref.data = mx_national_by_cause,
    ref.rate = "mx",
    print    = "cause_cat",
    adjust   = c("year","sex","agegroup","cause_cat")
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(group = cause_cat) |>
    dplyr::left_join(phi_tbl, by = "group")
  
  smr_phi <- sir_ci_phi_profile(smr, phi = smr$phi, conf.level = conf)
  
  smr_phi |>
    dplyr::mutate(
      print = base::sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
                            sir, sir.phi.lo, sir.phi.hi)
    )
}
run_dsr_by_cause <- function(coh_cause, weights_corr, digits = 2, conf = 0.95) {
  
  z <- stats::qnorm(1 - (1 - conf)/2)
  
  dsr <- popEpi::rate(
    data    = coh_cause,
    obs     = "from0to1",
    pyrs    = "pyrs",
    print   = "cause_cat",
    adjust  = c("year","sex","agegroup"),
    weights = weights_corr
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(group = cause_cat)
  
  phi_tbl <- extract_phi_by(
    coh_cause,
    by       = "cause_cat",
    rhs_vars = c("year","sex","agegroup")
  )
  
  dsr |>
    dplyr::left_join(phi_tbl, by = "group") |>
    dplyr::mutate(
      # conservative + stable: if phi missing or <1, use 1
      phi = dplyr::if_else(is.na(phi) | phi < 1, 1, phi),
      
      rate_1k = rate.adj * 1e3,
      
      # don't use pmax(); just compute when rate>0
      sei_log = dplyr::if_else(
        rate.adj > 0,
        (SE.rate.adj * sqrt(phi)) / rate.adj,
        NA_real_
      ),
      
      rate_1k_L = dplyr::if_else(
        rate.adj > 0,
        (rate.adj * exp(-z * sei_log)) * 1e3,
        NA_real_
      ),
      rate_1k_U = dplyr::if_else(
        rate.adj > 0,
        (rate.adj * exp(+z * sei_log)) * 1e3,
        NA_real_
      ),
      
      print = dplyr::if_else(
        rate.adj > 0,
        sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
                rate_1k, rate_1k_L, rate_1k_U),
        sprintf(paste0("%.", digits, "f (NA–NA)"), rate_1k)
      )
    )
}



## Implementation ---------------------------------------------------------------

### National Ref. Pop. ---------------------------------------------------------------

years_keep    <- base::sort(base::unique(base::as.integer(c_SISTRAT_std_bins_wold$year)))
agegroups_keep<- base::sort(base::unique(base::as.integer(c_SISTRAT_std_bins_wold$agegroup)))

mx_national_by_cause <- build_mx_national_by_cause(
  deaths_nat_df = mortality_deduplicated,
  pop_df        = proy_ine_com_2010_2020_corr,
  years_keep    = years_keep,
  agegroups_keep= agegroups_keep,
  year_col  = "ano_def",
  sex_col   = "sexo",
  age_col   = "edad_cant",
  diag1_col = "diag1",
  diag2_col = "diag2",
  pop_sex_col = "sex_rec"   # for the main pop
)


agegroups_keep_old<-  base::sort(base::unique(c_SISTRAT_std_old$agegroup_old))

mx_national_by_cause_old <- build_mx_national_by_cause(
  deaths_nat_df = mortality_deduplicated,
  pop_df        = proy_ine_com_2010_2020_corr_old,
  years_keep    = years_keep,
  agegroups_keep= agegroups_keep_old,
  year_col  = "ano_def",
  sex_col   = "sexo",
  age_col   = "edad_cant",
  diag1_col = "diag1",
  diag2_col = "diag2",
  pop_sex_col = "sex"       # for the old pop
)

#readr::write_rds(mx_national_by_cause, paste0(getwd(), "/cons/_out/mx_national_by_cause.Rds"))
#readr::read_rds(paste0(getwd(), "/cons/_out/mx_national_by_cause.Rds"))

### Cohort by cause ---------------------------------------------------------------

clean_df3<- left_join(clean_df2,
                      dplyr::select(mortality_deduplicated, 
                                    hashkey,
                                    diag1, 
                                    diag2 ), by=c("hash_key"="hashkey"))

coh_cause <- build_cohort_by_cause(clean_df3, c_SISTRAT_5y)

# c_SISTRAT_std_old
# c_SISTRAT_std_bins
clean_df3_old<- left_join(clean_df,
                      dplyr::select(mortality_deduplicated, 
                                    hashkey,
                                    diag1, 
                                    diag2 ), by=c("hash_key"="hashkey"))
summary(clean_df3_old$death_age_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   33.00   39.00   40.87   48.00   75.00 

coh_cause_old <- build_cohort_by_cause(clean_df3_old, 
                                       tidytable::rename(c_SISTRAT_std_old, 
                                                         agegroup=agegroup_old))
coh_cause_old <- build_cohort_by_cause(
       clean_df3_old,
       tidytable::rename(c_SISTRAT_std_old, agegroup = agegroup_old)
   )


#### Sanity check ---------------------------------------------------------------

# sanity checks 
sum(c_SISTRAT_std_old$from0to1)
#[1] 2996
sum(coh_cause_old$from0to1)
#[1] 2996

cat("Does this differences comes from unassesssed deaths or deaths classified outside my scheme?\n")
clean_df3_old |>
  tidytable::filter(status == 1, !is.na(death_date_rec)) |>
  tidytable::mutate(cause_cat = assign_cause_cat_vec_corr(diag1, diag2)) |>
  tidytable::summarise(
    n_deaths = n(),
    n_missing_diag = sum(is.na(diag1) & is.na(diag2)),
    n_unclassified = sum(is.na(cause_cat))
  )
#   n_deaths n_missing_diag n_unclassified
#      <int>          <int>          <int>
# 1     2996              0             43
#   n_deaths n_missing_diag n_unclassified
#      <int>          <int>          <int>
# 1     2996              0              0

warning("previous to 2025-12-30= 43 unclassified")

# O sea: no es un problema de Lexis ni de los bins, es que 43 muertes tienen ICD 
# en diag1/diag2 pero tu assign_cause_cat_vec() las deja en NA, y como en build_cohort_by_cause() 
#tú filtras !is.na(cause_cat), esas 43 quedan fuera del conteo por causa.

warning("Haz are 18–64 only, not 18+ and not 18–74. And that restriction will 
        systematically change the expected counts: causes for youth, reduces SMR")
#For causes more common at older ages (cancer, circulatory), excluding 65+ 
#reduces the reference hazard → reduces expected → inflates SMR.
#For causes more common at younger ages (self-harm, assaults, some externals), 
#excluding 65+ often increases the reference hazard (you drop low-risk older ages) 
#→ increases expected → reduces SMR.
#That’s why your popEpi::sir() for self-harm gives 402 / 81.89 = 4.91, matching the 
#old table (402/82 ≈ 4.91). You’re using the same “18–64 reference world”.

warning("previous to 2025-12-30= “43 deaths (1.4%) had ICD-10 codes that could not be mapped to the predefined 
    cause categories and were excluded from cause-specific analyses.”")

message("2025-12-30= I ended amplifying the category to include other causes (S, T, U, Z)")

### SMR ---------------------------------------------------------------

smr_cause <- popEpi::sir(
  coh.data = coh_cause,
  coh.obs  = "from0to1",
  coh.pyrs = "pyrs",
  ref.data = mx_national_by_cause,
  ref.rate = "mx",
  print    = "cause_cat",
  adjust   = c("year","sex","agegroup","cause_cat")
) |>
  dplyr::as_tibble()

#    cause_cat                                        observed expected    pyrs   sir sir.lo sir.hi p_value
#    <fct>                                               <int>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl>
#  1 Infectious & parasitic (A00–B99)                      129    33.2  353826.  3.89   3.25   4.60 0      
#  2 Malignant neoplasms (C00–C96)                         220   159.   353826.  1.39   1.21   1.58 0      
#  3 Endocrine & metabolic (E00–E99)                        42    21.3  353826.  1.97   1.44   2.63 0.00001
#  4 Mental and behavioral (F01–F99)                        49     5.61 353826.  8.73   6.51  11.4  0      
#  5 Nervous system (G00–G99)                               47    20.0  353826.  2.35   1.74   3.09 0      
#  6 Circulatory (I00–I99)                                 407   131.   353826.  3.11   2.82   3.42 0      
#  7 Respiratory (J00–J99)                                 181    34.9  353826.  5.19   4.47   5.98 0      
#  8 Digestive (K00–K93)                                   704    85.8  353826.  8.21   7.62   8.83 0      
#  9 Symptoms & signs (R00–R99)                            107    20.6  353826.  5.19   4.26   6.23 0      
# 10 Other underlying causes (D, H, L, M, N, O, P, Q)       53    31.6  353826.  1.68   1.27   2.17 0.00016
# 11 Assaults/ Aggressions / Homicide (X85–Y09)            120    24.1  353826.  4.98   4.14   5.93 0      
# 12 Intentional self-harm (X60–X84)                       402    60.2  353826.  6.67   6.04   7.35 0      
# 13 Transport accidents (V01–V99)                         162    56.6  353826.  2.86   2.44   3.32 0      
# 14 Other unintentional external causes (W00–X59)         323    60.1  353826.  5.37   4.81   5.98 0      
# 15 Other external causes (Y10–Y98)                         7     2.08 353826.  3.37   1.45   6.52 0.0013
# 
# # A tibble: 16 × 8
#    cause_cat                                                 observed expected    pyrs   sir sir.lo sir.hi p_value
#    <fct>                                                        <int>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl>
#  1 Infectious & parasitic (A00–B99)                               129  33.2    353826.  3.88   3.27   4.62 0      
#  2 Malignant neoplasms (C00–C96)                                  220 159.     353826.  1.38   1.21   1.58 0      
#  3 Endocrine & metabolic (E00–E99)                                 42  21.3    353826.  1.97   1.46   2.67 0.00001
#  4 Mental and behavioral (F01–F99)                                 49   5.62   353826.  8.71   6.59  11.5  0      
#  5 Nervous system (G00–G99)                                        47  20.0    353826.  2.35   1.77   3.13 0      
#  6 Circulatory (I00–I99)                                          407 131.     353826.  3.11   2.82   3.42 0      
#  7 Respiratory (J00–J99)                                          181  35.0    353826.  5.18   4.47   5.99 0      
#  8 Digestive (K00–K93)                                            704  85.8    353826.  8.20   7.62   8.83 0      
#  9 Symptoms & signs (R00–R99)                                     107  20.6    353826.  5.18   4.29   6.26 0      
# 10 Other underlying causes (D, H, L, M, N, O, P, Q, S, T, U)       96  55.7    353826.  1.72   1.41   2.11 0      
# 11 Assaults/ Aggressions / Homicide (X85–Y09)                     120  24.1    353826.  4.98   4.16   5.96 0      
# 12 Intentional self-harm (X60–X84)                                402  60.2    353826.  6.67   6.05   7.36 0      
# 13 Transport accidents (V01–V99)                                  162  56.6    353826.  2.86   2.45   3.34 0      
# 14 Other unintentional external causes (W00–X59)                  323  60.1    353826.  5.37   4.82   5.99 0      
# 15 Other external causes (Y10–Y98)                                  7   2.08   353826.  3.37   1.61   7.06 0.00132
# 16 Unknown/Unclassified                                             0   0.0427 353826.  0      0    Inf    1.00   

smr_cause_old <- popEpi::sir(
  coh.data = coh_cause_old,
  coh.obs  = "from0to1",
  coh.pyrs = "pyrs",
  ref.data = mx_national_by_cause_old,
  ref.rate = "mx",
  print    = "cause_cat",
  adjust   = c("year","sex","agegroup","cause_cat")
) |>
  dplyr::as_tibble()
# # A tibble: 16 × 8
#    cause_cat                                                 observed expected    pyrs   sir sir.lo sir.hi p_value
#    <fct>                                                        <int>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl>
#  1 Infectious & parasitic (A00–B99)                               129  37.0    353826. 3.49   2.94    4.15 0      
#  2 Malignant neoplasms (C00–C96)                                  220 237.     353826. 0.930  0.815   1.06 0.280  
#  3 Endocrine & metabolic (E00–E99)                                 42  35.1    353826. 1.20   0.885   1.62 0.242  
#  4 Mental and behavioral (F01–F99)                                 49   9.62   353826. 5.10   3.85    6.74 0      
#  5 Nervous system (G00–G99)                                        47  29.1    353826. 1.61   1.21    2.15 0.00105
#  6 Circulatory (I00–I99)                                          407 206.     353826. 1.98   1.79    2.18 0      
#  7 Respiratory (J00–J99)                                          181  63.1    353826. 2.87   2.48    3.32 0      
#  8 Digestive (K00–K93)                                            704 107.     353826. 6.59   6.12    7.09 0      
#  9 Symptoms & signs (R00–R99)                                     107  25.1    353826. 4.26   3.52    5.15 0      
# 10 Other underlying causes (D, H, L, M, N, O, P, Q, S, T, U)       96  85.0    353826. 1.13   0.925   1.38 0.232  
# 11 Assaults/ Aggressions / Homicide (X85–Y09)                     120  23.7    353826. 5.07   4.24    6.06 0      
# 12 Intentional self-harm (X60–X84)                                402  60.7    353826. 6.62   6.00    7.30 0      
# 13 Transport accidents (V01–V99)                                  162  58.0    353826. 2.79   2.39    3.26 0      
# 14 Other unintentional external causes (W00–X59)                  323  64.8    353826. 4.98   4.47    5.56 0      
# 15 Other external causes (Y10–Y98)                                  7   2.59   353826. 2.70   1.29    5.67 0.00857
# 16 Unknown/Unclassified                                             0   0.0617 353826. 0      0     Inf    1.00    

warning("In trasnport accidents, in fact were observed 162 and expected 77.5")


#### Assessments of differences ------

#Lo más probable: assign_cause_cat_vec() está dejando muchos ICD-10 como NA
mortality_deduplicated |>
  tidytable::mutate(
    icd = stringr::str_to_upper(as.character(diag1)),
    icd = stringr::str_replace_all(icd, "\\.", ""),
    icd3 = stringr::str_extract(icd, "^[A-Z][0-9]{2}"),
    cause_cat = assign_cause_cat_vec_corr(diag1, diag2)
  ) |>
  tidytable::summarise(
    n = .N,
    n_na = sum(is.na(cause_cat)),
    prop_na = mean(is.na(cause_cat))
  )
#        n  n_na prop_na
#    <int> <int>   <dbl>
# 1 996339 15622  0.0157

warning("From 2025-12-30, now there are no NAs")

#### Phi ==========

##### 5-yr bin ----------

phi_global <- extract_phi_global(coh_cause, rhs_vars = c("year","sex","agegroup"))
phi_cause <- extract_phi_by(
  coh_cause,
  by = "cause_cat",
  rhs_vars = c("year","sex","agegroup"),
  min_events = 20,
  phi_fallback = phi_global
)

##### 15-yr bin -----

phi_cause_old <- extract_phi_by(
  coh_cause,
  by = "cause_cat",
  rhs_vars = c("year","sex","agegroup"),
  min_events = 20,
  phi_fallback = phi_global 
)

coh_cause_old |>
  dplyr::filter(cause_cat == "Assaults/ Aggressions / Homicide (X85–Y09)") |>
  dplyr::count(from0to1 == 0)
warning("With 56% zero cells, the Poisson GLM tries to model these patterns 
across year × sex × agegroup. For certain combinations (e.g., specific age groups 
with zero homicides), the model predicts rates extremely close to zero, triggering the warning.")


# O = 7 →
# SE(log) ≈ 1 / √7 ≈ 0.38 (big SEs)
# 
# O = 42–53 →
# SE(log) ≈ 1 / √O ≈ 0.14–0.15 (still BIG)
# 
# O = 129 →
# SE(log) ≈ 1 / √129 ≈ 0.088 (acceptable)
# 
# O = 704 →
# SE(log) ≈ 1 / √704 ≈ 0.038 (stable)


#### FINAL=Phi-corrected SMR -------------------------------------------------------
subset(
  run_smr_by_cause(coh_cause, mx_national_by_cause, phi_cause)|> 
    dplyr::mutate(obs_exp= paste0(observed, "/",as.integer(expected)), 
                  pyrs_int= as.integer(pyrs), phi=sprintf("%1.2f",phi)),
  select= c("group", "obs_exp", "pyrs_int", "phi","print")
)# |> rio::export("clipboard")

### DSR ---------------------------------------------------------------

#weights_corr_old
dsr_cause <- run_dsr_by_cause(coh_cause, weights_corr, digits = 2)

dsr_cause_old <- run_dsr_by_cause(coh_cause_old, weights_corr_old, digits = 2)


vals <- c(
  "0.4 (0.3–0.6)",
  "1.2 (0.9–1.6)",
  "0.1 (0.1–0.2)",
  "0.1 (0.1–0.1)",
  "0.3 (0.1–0.6)",
  "1.8 (1.3–2.4)",
  "0.5 (0.4–0.7)",
  "2.9 (2.2–3.8)",
  "0.6 (0.3–0.9)",
  "0.2 (0.1–0.2)",
  "0.3 (0.2–0.4)",
  "1.0 (0.8–1.2)",
  "0.4 (0.3–0.5)",
  "3.3 (0.4–24.3)",
  "",
  ""
)

cbind.data.frame(
  dplyr::select(dsr_cause_old, group, print),
  dplyr::select(dsr_cause, print),
  vals
) |>
  (\(df) setNames(df, c("group","bins15yr","bins5yr","sent_paper")))()
#                                               group          bins15yr          bins5yr     sent_paper
# 1                  Infectious & parasitic (A00–B99)  0.44 (0.28–0.70) 0.41 (0.23–0.72)  0.4 (0.3–0.6)
# 2                     Malignant neoplasms (C00–C96)  1.18 (0.86–1.62) 1.17 (0.76–1.79)  1.2 (0.9–1.6)
# 3                   Endocrine & metabolic (E00–E99)  0.13 (0.08–0.24) 0.13 (0.07–0.25)  0.1 (0.1–0.2)
# 4                   Mental and behavioral (F01–F99)  0.11 (0.07–0.16) 0.11 (0.06–0.20)  0.1 (0.1–0.1)
# 5                          Nervous system (G00–G99)  0.26 (0.11–0.66) 0.29 (0.09–1.01)  0.3 (0.1–0.6)
# 6                             Circulatory (I00–I99)  1.74 (1.23–2.46) 1.55 (1.21–1.99)  1.8 (1.3–2.4)
# 7                             Respiratory (J00–J99)  0.54 (0.43–0.69) 0.60 (0.41–0.88)  0.5 (0.4–0.7)
# 8                               Digestive (K00–K93)  2.90 (2.20–3.82) 2.42 (1.99–2.95)  2.9 (2.2–3.8)
# 9                        Symptoms & signs (R00–R99)  0.56 (0.31–1.00) 0.44 (0.28–0.71)  0.6 (0.3–0.9)
# 10 Other underlying causes (D, H, L, M, N, O, P, Q)  0.16 (0.10–0.25) 0.11 (0.08–0.16)  0.2 (0.1–0.2)
# 11       Assaults/ Aggressions / Homicide (X85–Y09)  0.29 (0.21–0.39) 0.24 (0.18–0.33)  0.3 (0.2–0.4)
# 12                  Intentional self-harm (X60–X84)  1.00 (0.80–1.25) 0.97 (0.69–1.36)  1.0 (0.8–1.2)
# 13                    Transport accidents (V01–V99)  0.40 (0.29–0.55) 0.31 (0.24–0.39)  0.4 (0.3–0.5)
# 14    Other unintentional external causes (W00–X59) 3.30 (0.45–24.31) 1.78 (0.56–5.71) 3.3 (0.4–24.3)
# 15                  Other external causes (Y10–Y98)  0.01 (0.01–0.03) 0.01 (0.00–0.04)               

#                                                        group          bins15yr          bins5yr     sent_paper
# 1                           Infectious & parasitic (A00–B99)  0.44 (0.28–0.70) 0.41 (0.23–0.72)  0.4 (0.3–0.6)
# 2                              Malignant neoplasms (C00–C96)  1.18 (0.86–1.62) 1.17 (0.76–1.79)  1.2 (0.9–1.6)
# 3                            Endocrine & metabolic (E00–E99)  0.13 (0.08–0.24) 0.13 (0.07–0.25)  0.1 (0.1–0.2)
# 4                            Mental and behavioral (F01–F99)  0.11 (0.07–0.16) 0.11 (0.06–0.20)  0.1 (0.1–0.1)
# 5                                   Nervous system (G00–G99)  0.26 (0.11–0.66) 0.29 (0.09–1.01)  0.3 (0.1–0.6)
# 6                                      Circulatory (I00–I99)  1.74 (1.23–2.46) 1.55 (1.21–1.99)  1.8 (1.3–2.4)
# 7                                      Respiratory (J00–J99)  0.54 (0.43–0.69) 0.60 (0.41–0.88)  0.5 (0.4–0.7)
# 8                                        Digestive (K00–K93)  2.90 (2.20–3.82) 2.42 (1.99–2.95)  2.9 (2.2–3.8)
# 9                                 Symptoms & signs (R00–R99)  0.56 (0.31–1.00) 0.44 (0.28–0.71)  0.6 (0.3–0.9)
# 10 Other underlying causes (D, H, L, M, N, O, P, Q, S, T, U)  0.24 (0.17–0.34) 0.19 (0.15–0.25)  0.2 (0.1–0.2)
# 11                Assaults/ Aggressions / Homicide (X85–Y09)  0.29 (0.21–0.39) 0.24 (0.18–0.33)  0.3 (0.2–0.4)
# 12                           Intentional self-harm (X60–X84)  1.00 (0.80–1.25) 0.97 (0.69–1.36)  1.0 (0.8–1.2)
# 13                             Transport accidents (V01–V99)  0.40 (0.29–0.55) 0.31 (0.24–0.39)  0.4 (0.3–0.5)
# 14             Other unintentional external causes (W00–X59) 3.30 (0.45–24.31) 1.78 (0.56–5.71) 3.3 (0.4–24.3)
# 15                           Other external causes (Y10–Y98)  0.01 (0.01–0.03) 0.01 (0.00–0.04)               
# 16                                      Unknown/Unclassified      0.00 (NA–NA)     0.00 (NA–NA)               

dsr_cause |> 
  dplyr::select(group, rate, rate.lo, rate.hi, print) |> 
  dplyr::mutate_if(is.numeric, ~sprintf("%1.1f",.*1e3)) |>
  dplyr::mutate(CMR= paste0(rate, " (",rate.lo,"–",rate.hi,")")) #|> rio::export("clipboard")

#### Sanity check ---------------------------------------------------------------

coh_cause %>%
  mutate(age15 = cut(agegroup,
                     breaks = c(18, 30, 45, 60, Inf),
                     right = FALSE)) %>%
  group_by(cause_cat, age15, agegroup) %>%
  summarise(d = sum(deaths), py = sum(pyrs), .groups = "drop") %>%
  mutate(r = d / py) %>%
  group_by(cause_cat, age15) %>%
  summarise(
    min_r = min(r, na.rm=TRUE),
    max_r = max(r, na.rm=TRUE),
    ratio = max_r / pmax(min_r, 1e-12),
    .groups="drop"
  ) %>%
  arrange(desc(ratio)) |> 
  head()

coh_cause %>%
  mutate(r = from0to1  / pyrs) %>%
  arrange(desc(r)) %>%
  dplyr::select(cause_cat, agegroup, from0to1 , pyrs, r) |> 
  head()
#   cause_cat                                     agegroup from0to1  pyrs      r
#   <fct>                                            <dbl>    <int> <dbl>  <dbl>
# 1 Other unintentional external causes (W00–X59)       60        1  2.06 0.485 
# 2 Malignant neoplasms (C00–C96)                       70        1  7.47 0.134 
# 3 Digestive (K00–K93)                                 70        2 15.6  0.128 
# 4 Circulatory (I00–I99)                               70        4 61.1  0.0655
# 5 Respiratory (J00–J99)                               70        1 15.6  0.0640
# 6 Digestive (K00–K93)                                 60        1 17.8  0.0563

coh_cause %>%
  left_join(weights_corr, by = c("year", "sex", "agegroup")) %>%
  mutate(r = from0to1 / pyrs,
         contrib = weights * r) %>%
  group_by(cause_cat) %>%
  slice_max(order_by = abs(contrib), n = 5, with_ties = FALSE) %>%
  dplyr::select(cause_cat, agegroup, from0to1, pyrs, weights, r, contrib) %>%
  arrange(cause_cat, desc(abs(contrib)))|> 
  head()
#   cause_cat                        agegroup from0to1    pyrs weights       r   contrib
#   <fct>                               <dbl>    <int>   <dbl>   <dbl>   <dbl>     <dbl>
# 1 Infectious & parasitic (A00–B99)       65        1   18.9  0.00213 0.0530  0.000113 
# 2 Infectious & parasitic (A00–B99)       55        1  201.   0.00335 0.00497 0.0000167
# 3 Infectious & parasitic (A00–B99)       65        1  181.   0.00285 0.00553 0.0000157
# 4 Infectious & parasitic (A00–B99)       50        3 1004.   0.00402 0.00299 0.0000120
# 5 Infectious & parasitic (A00–B99)       45        1  334.   0.00402 0.00299 0.0000120
# 6 Malignant neoplasms (C00–C96)          70        1    7.47 0.00149 0.134   0.000200 


#### External /Underlying ------------------------------------------------------------------

external_cats <- c(
  "Assaults/ Aggressions / Homicide (X85–Y09)",
  "Intentional self-harm (X60–X84)",
  "Transport accidents (V01–V99)",
  "Other unintentional external causes (W00–X59)",
  "Other external causes (Y10–Y98)"
)

clean_df4 <- clean_df2 %>%
  left_join(
    mortality_deduplicated %>% dplyr::select(hashkey, diag1, diag2),
    by = c("hash_key" = "hashkey")
  ) %>%
  mutate(
    cause_cat   = assign_cause_cat_vec_corr(diag1, diag2),
    is_external = cause_cat %in% external_cats,
    status= ifelse(!is.na(death_date),1,0),
    # split the *same* death indicator into 2 mutually exclusive outcomes
    death_external   = dplyr::case_when(!is.na(death_date) & is_external ~ 1L, TRUE ~ 0L),
    death_underlying = dplyr::case_when(!is.na(death_date) & !is_external ~ 1L, TRUE ~ 0L)
  )

age_breaks <- sort(unique(mx_national_clean$agegroup))
age_breaks <- age_breaks[age_breaks >= 15]

c_external <- lexpand(
  clean_df4,
  status = death_external,
  birth  = birth_date_rec,
  exit   = death_date_rec,
  entry  = disch_date_rec6,
  breaks = list(per = seq(2010, 2021, 1),
                age = age_breaks),
  aggre  = list(year = per, agegroup = age, sex = sex_rec)
)

c_underlying <- lexpand(
  clean_df4,
  status = death_underlying,
  birth  = birth_date_rec,
  exit   = death_date_rec,
  entry  = disch_date_rec6,
  breaks = list(per = seq(2010, 2021, 1),
                age = age_breaks),
  aggre  = list(year = per, agegroup = age, sex = sex_rec)
)
sum(c_underlying$from0to1)
#[1] 1982
sum(c_external$from0to1)
#[1] 1014
1982+1014
#[1] 2996

ref_mx_agg <- function(mx_df, cats){
  mx_df %>%
    filter(cause_cat %in% cats) %>%
    group_by(year, sex, agegroup) %>%
    summarise(
      deaths = sum(deaths, na.rm = TRUE),
      pop    = dplyr::first(pop),
      mx     = deaths / pop,
      .groups = "drop"
    )
}

ref_external   <- ref_mx_agg(mx_national_by_cause, external_cats)
ref_underlying <- ref_mx_agg(mx_national_by_cause, setdiff(cause_levels, external_cats))



smr_external <- popEpi::sir(
  coh.data = c_external, coh.obs = "from0to1", coh.pyrs = "pyrs",
  ref.data = ref_external, ref.rate = "mx",
  adjust = c("year","sex","agegroup"), EAR = TRUE
)

smr_underlying <- popEpi::sir(
  coh.data = c_underlying, coh.obs = "from0to1", coh.pyrs = "pyrs",
  ref.data = ref_underlying, ref.rate = "mx",
  adjust = c("year","sex","agegroup"), EAR = TRUE
)

dsr_external <- popEpi::rate(
  data = c_external, obs = "from0to1", pyrs = "pyrs",
  adjust = c("year","sex","agegroup"), weights = weights_corr
)

dsr_underlying <- popEpi::rate(
  data = c_underlying, obs = "from0to1", pyrs = "pyrs",
  adjust = c("year","sex","agegroup"), weights = weights_corr
)


extract_phi_dir(c_underlying)

extract_phi_dir(c_external)

sir_ci_phi_profile(smr_underlying,pmax(1,extract_phi_dir(c_underlying)))
# SIR (adjusted by year, sex, agegroup) with 95% confidence intervals (profile) 
# 
# Total sir: 3.49 (3.34-3.65)
# Total observed: 1982
# Total expected: 567.25
# Total person-years: 353826 
# 
# observed expected   pyrs   sir sir.lo sir.hi p_value   EAR sir.phi.lo sir.phi.hi
# <num>    <num>  <num> <num>  <num>  <num>   <num> <num>      <num>      <num>
#   1:     1982   567.25 353826  3.49   3.34   3.65       0 3.998   3.343588   3.651388
sir_ci_phi_profile(smr_external,pmax(1,extract_phi_dir(c_external)))
# SIR (adjusted by year, sex, agegroup) with 95% confidence intervals (profile) 
# 
#  Total sir: 4.99 (4.69-5.31)
#  Total observed: 1014
#  Total expected: 203.18
#  Total person-years: 353826 
# 
#    observed expected   pyrs   sir sir.lo sir.hi p_value   EAR sir.phi.lo sir.phi.hi
#       <num>    <num>  <num> <num>  <num>  <num>   <num> <num>      <num>      <num>
# 1:     1014   203.18 353826  4.99   4.69    5.3       0 2.292   4.641594   5.366063

with(sir_ci_phi_profile(smr_external,pmax(1,extract_phi_dir(c_external))), sprintf("%.2f (%.2f–%.2f)", 
    sir, sir.phi.lo, sir.phi.hi))
#[1] "4.99 (4.64–5.37)"
paste0(smr_external$observed,"/",round(smr_external$expected,0))
#[1] "1014/203"

with(sir_ci_phi_profile(smr_underlying,pmax(1,extract_phi_dir(c_underlying))), sprintf("%.2f (%.2f–%.2f)", 
    sir, sir.phi.lo, sir.phi.hi))
#[1] "3.65 (3.49–3.81)"
paste0(smr_underlying$observed,"/",round(smr_underlying$expected,0))
#[1] "1982/567"

dsr_format_corr(dsr_external$rate.adj, dsr_external$SE.rate.adj, factor=1e3, digits=1)
#[1] "3.3 (1.8–6.1)"

dsr_format_corr(dsr_underlying$rate.adj, dsr_underlying$SE.rate.adj, factor=1e3, digits=1)
#[1] "7.3 (6.4–8.3)"

with(dsr_external, sprintf("%.1f (%.1f–%.1f)", rate*1e3, rate.lo*1e3 , rate.hi*1e3))
#[1] "2.9 (2.7–3.0)"
with(dsr_underlying, sprintf("%.1f (%.1f–%.1f)", rate*1e3, rate.lo*1e3 , rate.hi*1e3))
#[1] "5.6 (5.4–5.9)"


### Percentages, external underlying ----------------------------------------

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# 1. Reference Database Distribution (National)
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Aggregating national deaths from the reference grid
ref_dist <- mx_national_by_cause |> 
  dplyr::group_by(cause_cat) |> 
  dplyr::summarise(
    Ref_Deaths = sum(deaths, na.rm = TRUE)
  ) |> 
  dplyr::mutate(
    Ref_Pct = (Ref_Deaths / sum(Ref_Deaths)) * 100
  )

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# 2. Study Database Distribution (Cohort)
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Aggregating observed deaths from the cohort object
coh_dist <- coh_cause |> 
  dplyr::group_by(cause_cat) |> 
  dplyr::summarise(
    Study_Deaths = sum(from0to1, na.rm = TRUE)
  ) |> 
  dplyr::mutate(
    Study_Pct = (Study_Deaths / sum(Study_Deaths)) * 100
  )

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# 3. Combined Comparison Table
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
cause_distribution <- ref_dist |> 
  dplyr::full_join(coh_dist, by = "cause_cat") |> 
  # Handle NAs if a cause is missing in one DB (unlikely now)
  dplyr::mutate(
    across(c(Ref_Deaths, Study_Deaths), ~tidyr::replace_na(., 0)),
    across(c(Ref_Pct, Study_Pct), ~tidyr::replace_na(., 0))
  ) |> 
  # Formatting for display
  dplyr::transmute(
    Category = cause_cat,
    `National Deaths (n)` = formatC(Ref_Deaths, format="d", big.mark=","),
    `National (%)` = sprintf("%.1f%%", Ref_Pct),
    `Cohort Deaths (n)` = formatC(Study_Deaths, format="d", big.mark=","),
    `Cohort (%)` = sprintf("%.1f%%", Study_Pct)
  )

# Print the table
print(as.data.frame(cause_distribution), row.names = FALSE)

cause_distribution #|> rio::export("clipboard")
# Optional: Export to CSV
# write.csv(cause_distribution, "Cause_Distribution_Comparison.csv", row.names = FALSE)


### Percentages overall underlying / external -------------------------------------------------------------

# 1. Define Categories
external_cats <- c(
  "Assaults/ Aggressions / Homicide (X85–Y09)",
  "Intentional self-harm (X60–X84)",
  "Transport accidents (V01–V99)",
  "Other unintentional external causes (W00–X59)",
  "Other external causes (Y10–Y98)"
)

# 2. Reference Population Distribution
# Sum deaths for External vs Non-External in the reference grid
ref_broad_dist <- mx_national_by_cause |> 
  dplyr::mutate(
    Broad_Group = dplyr::if_else(cause_cat %in% external_cats, "External causes", "Underlying causes")
  ) |> 
  dplyr::group_by(Broad_Group) |> 
  dplyr::summarise(
    Ref_Deaths = sum(deaths, na.rm = TRUE)
  ) |> 
  dplyr::mutate(
    Ref_Pct = (Ref_Deaths / sum(Ref_Deaths)) * 100
  )

# 3. Study Cohort Distribution
# Using your calculated sums (1014 and 1982)
coh_broad_dist <- tibble::tibble(
  Broad_Group = c("External causes", "Underlying causes"),
  Study_Deaths = c(1014, 1982)
) |> 
  dplyr::mutate(
    Study_Pct = (Study_Deaths / sum(Study_Deaths)) * 100
  )

# 4. Combine and Display
broad_comparison <- ref_broad_dist |> 
  dplyr::left_join(coh_broad_dist, by = "Broad_Group") |> 
  dplyr::transmute(
    Group = Broad_Group,
    `National Deaths (n)` = formatC(Ref_Deaths, format="d", big.mark=","),
    `National (%)` = sprintf("%.1f%%", Ref_Pct),
    `Cohort Deaths (n)` = formatC(Study_Deaths, format="d", big.mark=","),
    `Cohort (%)` = sprintf("%.1f%%", Study_Pct)
  )

print(broad_comparison)

## Hypothetical DSR population ====================

# weights_std <- mx_national_by_cause |>
#   dplyr::group_by(sex, agegroup, year) |>
#   dplyr::summarise(pop = sum(pop), .groups = "drop") |>
#   dplyr::select(year, agegroup, sex, weight)
# 
# dsr_full <- mx_national_by_cause |>
#   dplyr::left_join(weights_std, by = c("sex","agegroup")) |>
#   dplyr::group_by(year, sex, agegroup, cause_cat) |>
#   dplyr::summarise(
#     dsr = sum(weight * mx, na.rm = TRUE),
#     .groups = "drop"
#   )

# dsr_by_cause <- dsr_full |>
#   dplyr::group_by(cause_cat) |>
#   dplyr::summarise(
#     dsr = sum(dsr, na.rm = TRUE),
#     .groups = "drop"
#   )

pop_std <- mx_national_by_cause %>%
  dplyr::distinct(year, sex, agegroup, pop)

weights_std <- pop_std %>%
  dplyr::group_by(sex, agegroup) %>%
  dplyr::summarise(pop = sum(pop), .groups = "drop") %>%
  dplyr::mutate(weight = pop / sum(pop)) %>%
  dplyr::select(sex, agegroup, weight)

dsr_year_cause <- mx_national_by_cause %>%
  dplyr::left_join(weights_std, by = c("sex","agegroup")) %>%
  dplyr::group_by(year, cause_cat) %>%
  dplyr::summarise(dsr = sum(weight * mx, na.rm = TRUE), .groups = "drop")

mx_pooled <- mx_national_by_cause %>%
  dplyr::group_by(sex, agegroup, cause_cat) %>%
  dplyr::summarise(deaths = sum(deaths), pop = sum(pop), .groups = "drop") %>%
  dplyr::mutate(mx = deaths / pop)

dsr_pooled_cause <- mx_pooled %>%
  dplyr::left_join(weights_std, by = c("sex","agegroup")) %>%
  dplyr::group_by(cause_cat) %>%
  dplyr::summarise(dsr = sum(weight * mx, na.rm = TRUE), .groups = "drop")

dsr_pooled_cause$dsr_print <- sprintf("%1.2f",dsr_pooled_cause$dsr*1e3)

cat("Correct Population DSR\n")
print(subset(dsr_pooled_cause, select= c("cause_cat", "dsr_print") ))
#    cause_cat                                        dsr_print
#    <fct>                                            <chr>    
#  1 Infectious & parasitic (A00–B99)                 0.1      
#  2 Malignant neoplasms (C00–C96)                    1.3      
#  3 Endocrine & metabolic (E00–E99)                  0.2      
#  4 Mental and behavioral (F01–F99)                  0.0      
#  5 Nervous system (G00–G99)                         0.1      
#  6 Circulatory (I00–I99)                            1.0      
#  7 Respiratory (J00–J99)                            0.3      
#  8 Digestive (K00–K93)                              0.4      
#  9 Symptoms & signs (R00–R99)                       0.1      
# 10 Other underlying causes (D, H, L, M, N, O, P, Q) 0.2      
# 11 Assaults/ Aggressions / Homicide (X85–Y09)       0.0      
# 12 Intentional self-harm (X60–X84)                  0.1      
# 13 Transport accidents (V01–V99)                    0.1      
# 14 Other unintentional external causes (W00–X59)    0.2      
# 15 Other external causes (Y10–Y98)                  0.0  
subset(dsr_pooled_cause, select= c("cause_cat", "dsr_print") )#|> rio::export("clipboard")

## Plot --------------------------------------------------------------------

#weights_corr

c_SISTRAT_std_bins_fot <- lexpand(
  clean_df2, 
  status = status, 
  birth = birth_date_rec, 
  exit = death_date_rec, 
  entry = disch_date_rec6,
  breaks = list(
    per = seq(2010, 2021, by = 1), 
    age = age_breaks, # <--- This matches the reference keys (15, 20, 25...)
    fot = c(0, .0386,.2465, .5, 1, 3, 5, 7, 9, Inf)
  ),
  aggre = list(agegroup = age, year = per, sex = sex_rec, fot= fot)
)

sr_1_std_bins_sex_fot <- popEpi::sir(c_SISTRAT_std_bins_fot, coh.obs = 'from0to1',
                            coh.pyrs = 'pyrs',
                            ref.data = mx_national_clean,
                            ref.rate = "mx",
                            print = c("sex", "fot"),
                            adjust = c("agegroup", "sex", "year"),
                            test.type = "homogeneity",
                            conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
                            conf.level = 0.95, EAR = T)

r2_std_bins_sex_fot <- popEpi::rate( data = c_SISTRAT_std_bins_fot, 
                             obs = from0to1, 
                             pyrs = pyrs, 
                             print = c("sex","fot"),
                             adjust = c("year", "agegroup"),
                             weights =  list(year = w_year_5y, agegroup = w_age_5y), 
)

rates_std_bins_sex_df_fot <- r2_std_bins_sex_fot %>% 
  mutate(
    # conviene expresar la tasa por 1 000 persona-año
    rate_adj_1k     = rate.adj     * 1e3,
    rate_adj_lo_1k  = rate.adj.lo  * 1e3,
    rate_adj_hi_1k  = rate.adj.hi  * 1e3
  )

p_std_bins_rate2 <- ggplot(rates_std_bins_sex_df_fot, aes(x = fot, y = rate_adj_1k, fill=sex)) +
  geom_ribbon(aes(ymin = rate_adj_lo_1k, ymax = rate_adj_hi_1k, fill=sex),
              alpha = .20) +
  geom_line(aes(color=sex), size = .9) +
  geom_point(size = 2,aes(color=sex, shape=sex)) +
  scale_x_continuous("Years since discharge",
                     breaks = setdiff(round(rates_df_fot$fot,2),c(0.04, 0.25))) +
  scale_y_continuous("Adjusted rate \n(deaths ×1,000 PY)",
                     limits = c(0, NA)) +
  theme_minimal(base_family = "serif")+
  theme(axis.title.x = element_blank())+
  scale_colour_manual(
    values = c(Male = "#2C3E8B", Female = "#E69F00")
  ) +
  scale_shape_manual(values = c(Male = 16, Female = 17))+
  scale_fill_manual(
    values = c(Male = alpha("#2C3E8B", 0.25),
               Female = alpha("#E69F00", 0.25))
  )+
  guides(
    fill = guide_legend(title = "Sex"),
    color = guide_legend(title = "Sex"),
    shape = guide_legend(title = "Sex")  # Add this line
  )

p_std_bins_sir2 <- ggplot(sr_1_std_bins_sex_fot, aes(x = fot, y = sir, fill= sex)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = sir.lo, ymax = sir.hi, fill=sex),
              alpha = .20) +
  geom_line(aes(color=sex), size = .9) +
  geom_point(aes(color=sex, shape=sex),size = 2) +
  scale_x_continuous("Years since discharge",
                     breaks = setdiff(round(rates_df_fot$fot,2),c(0.04, 0.25))) +
  theme_minimal(base_family = "serif")+
  theme(axis.title.x = element_blank())+ 
  scale_y_log10(
    "Adjusted SMR",
    breaks = setdiff(round(rates_df_fot$fot,2),c(0.04, 0.25)),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  scale_colour_manual(
    values = c(Male = "#2C3E8B", Female = "#E69F00")
  ) +
  scale_shape_manual(values = c(Male = 16, Female = 17))+
  scale_fill_manual(
    values = c(Male = alpha("#2C3E8B", 0.25),
               Female = alpha("#E69F00", 0.25))
  )+ theme(legend.position="none")+
  guides(
    fill = guide_legend(title = "Sex"),
    color = guide_legend(title = "Sex"),
    shape = guide_legend(title = "Sex")  # Add this line
  )
# scale_y_continuous(trans = "exp", breaks = c(0,.5, 1, 1.5,2),
#                        name = "Adjusted SMR")           # removes the need for readers to exponentiate

legend_shared_std_bins <- ggpubr::get_legend(
  p_std_bins_rate2 +
    theme(legend.position = "bottom") +
    guides(
      fill = guide_legend(title = "Sex"),
      color = guide_legend(title = "Sex"),
      shape = guide_legend(title = "Sex")  # Add this line
    )
)
panels_std_bins <- cowplot::plot_grid(
  p_std_bins_rate2+ theme(legend.position="none"), p_std_bins_sir2+ theme(legend.position="none"),
  ncol             = 1,
  labels           = c("a", "b"),
  label_size       = 14,
  label_fontfamily = "serif",
  align            = "v",   # alinea verticalmente
  axis             = "l",   # toma eje izquierdo como referencia
  label_x          = 0,     # esquina izq.
  label_y          = 1,
  hjust            = -0.1,
  vjust            = 1.2
)

# Etiqueta global del eje-x
xlab_shared_std_bins <- cowplot::ggdraw() +
  cowplot::draw_label("Years since discharge",
                      fontfamily = "serif",
                      fontface = "plain", size = 12, hjust = 0.5)

# Figura final (ajusta rel_heights si necesitas más/menos espacio)
final_std_bins_plot <- cowplot::plot_grid(
  panels_std_bins,
  xlab_shared_std_bins,
  legend_shared_std_bins,
  ncol = 1,
  rel_heights = c(1, 0.06, 0.10)   # ajusta espacio a tu gusto
)

# Mostrar o guardar
print(final_std_bins_plot)
#ggsave(paste0(gsub("/cons","",getwd()),"/cons/_figs/Figure_1_rates_and_SIR_by_fot.png"), dpi = 600, width = 6*.9, height = 7*.9)

figexp<- 1.5

deinflar_word <- 1/1.07653631284916

ggsave(
  paste0(gsub("/cons","",getwd()), "/cons/_figs/Figure_1_rates_and_SIR_by_fot_updated_postrev.pdf"),
  dpi = 600,
  width = 80 *figexp,  # Target width in mm (directly from journal instructions)
  height = 80 * figexp* (7/6), # Adjust height proportionally based on your original plot ratio
  units = "mm",
  device = cairo_pdf  # This is the key to fixing the font error
)

ggsave(
  paste0(gsub("/cons","",getwd()), "/cons/_figs/Figure_1_rates_and_SIR_by_fot_updated_postrev.png"),
  dpi = 600,
  width = 80 *figexp*deinflar_word,  # Target width in mm (directly from journal instructions)
  height = 80 * figexp*deinflar_word* (7/6), # Adjust height proportionally based on your original plot ratio
  units = "mm"#,
  #device = cairo_pdf  # This is the key to fixing the font error
)

### Deaths within 2 weeks -------------------------------------------------------

cat("Nearly inmediate deaths")
scales::percent((9+22+40+113)/2996, accuracy=.1)


## Median follow-up -------------------------------------------------------


clean_df |>
  mutate(fu_years = as.numeric(difftime(death_date_rec, disch_date_rec6, units = "days")) / 365.2425) |>
  summarise(
    median = sprintf("%1.2f",median(fu_years)),
    q25 = sprintf("%1.2f",quantile(fu_years, 0.25)),
    q75 = sprintf("%1.2f",quantile(fu_years, 0.75))
  )
#   median  q25  q75
# 1   4.90 2.76 7.19


# Sensibility, last treatment --------------------------------------------------------------------


## SMR, Overall  -----------------------------------------------------------

c_SISTRAT_b_std_bins <- lexpand(
  clean_df_b, 
  status = status, 
  birth = birth_date_rec, 
  exit = death_date_rec, 
  entry = disch_date_rec6,
  breaks = list(
    per = seq(2010, 2021, by = 1), 
    age = age_breaks # <--- This matches the reference keys (15, 20, 25...)
  ),
  aggre = list(year = per, 
               agegroup = age, 
               sex = sex_rec)
)

sort(unique(c_SISTRAT_b_std_bins$agegroup))


sir_final_b_std <- sir(
  coh.data = c_SISTRAT_b_std_bins, 
  coh.obs = 'from0to1',    # Observed deaths column
  coh.pyrs = 'pyrs',       # Person-years column
  ref.data = mx_national_clean, 
  ref.rate = 'mx',         # The mortality rate column in reference
  adjust = c('agegroup', 'year', 'sex')#, 
  #print = 'total'
)

print(sir_final_b_std)
# SIR (adjusted by agegroup, year, sex) with 95% confidence intervals (profile) 
# 
#  Total sir: 4.01 (3.87-4.16)
#  Total observed: 3029
#  Total expected: 754.55
#  Total person-years: 317628 
# 
#    observed expected     pyrs   sir sir.lo sir.hi p_value
#       <num>    <num>    <num> <num>  <num>  <num>   <num>
# 1:     3029   754.55 317627.6  4.01   3.87   4.16       0

sir_ci_phi_improved( sir_final_b_std, extract_phi(c_SISTRAT_b_std_bins)) |>
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#         SIR   CI_low  CI_high  phi_used            print
#       <num>    <num>    <num>     <num>           <char>
# 1: 4.014304 3.883452 4.149565 0.8659597 4.01 (3.88–4.15)

cat("Check differences in age-sex-year strata \n")
keys_coh_b <- unique(c_SISTRAT_b_std_bins[, c("year","sex","agegroup")])
keys_ref_b <- unique(mx_national_clean[, c("year","sex","agegroup")])

stopifnot(
  nrow(dplyr::anti_join(keys_coh_b, keys_ref_b, by=c("year","sex","agegroup")))==0
)

sir_tot_b

# SIR (adjusted by agegroup, year, sex) with 95% confidence intervals (profile) 
# 
#  Total sir: 3.96 (3.82-4.1)
#  Total observed: 3029
#  Total expected: 765.8
#  Total person-years: 317628 



# 
#    observed expected     pyrs   sir sir.lo sir.hi p_value   EAR
#       <num>    <num>    <num> <num>  <num>  <num>   <num> <num>
# 1:     3029    765.8 317627.6  3.96   3.82    4.1       0 7.125


## DSRs  -----------------------------------------------------------


dsr_tot_b <- popEpi::rate(
  data    = c_SISTRAT_b_std_bins,
  obs     = "from0to1",
  pyrs    = "pyrs",
  adjust  = c("year","sex","agegroup"),
  weights = weights_corr
)

dsr_tot_b$rate_1k <- dsr_tot_b$rate * 1e3


DSR_1k_corr_corr_b <- mapply(
  dsr_format_corr,                 # FUN
  dsr_tot_b$rate.adj,           # primer vector (rate)
  dsr_tot_b$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    #phi    = extract_phi_dir(c_SISTRAT_std_bins),
    #opc. conservadora
    phi    = max(1,  extract_phi_dir(c_SISTRAT_b_std_bins)),
    factor = 1e3,
    digits = 1,
    conf   = 0.95))

DSR_1k_corr_corr_b 
# [1] "12.1 (9.7–15.2)"

message("DSRs decreases from 15.3 to 12.1 from last treatment sensitivity analysis\nbut SMRs risens from 3.96 to 4.01")
message("For main analysis: DSRs decreases from 13.1 to 10.6 from last treatment sensitivity analysis\nbut SMRs risens from 3.59 to 3.65")



## DSR w/ 5-yr-bin lexis, last tr.- by old ages -------------------------------------------

### DSR, by age ---------------------------

c_SISTRAT_b_std_old <- c_SISTRAT_b_std_bins |>
  dplyr::mutate(agegroup_old = to_old_agegroup(agegroup)) |>
  dplyr::group_by(year, sex, agegroup_old) |>
  dplyr::summarise(
    pyrs     = base::sum(pyrs),
    from0to1 = base::sum(from0to1),
    .groups  = "drop"
  )


dsr_age_old_b <- popEpi::rate(
  data    = c_SISTRAT_b_std_old,
  obs     = "from0to1",
  pyrs    = "pyrs",
  print   = "agegroup_old",
  adjust  = c("year","sex"),
  weights = list(year = w_year, sex = w_sex)
)

#### Etract phi ---------------------------


phi_df_age_old_b <- extract_phi_by_age(c_SISTRAT_b_std_old|> dplyr::rename(agegroup = agegroup_old))

phi_vec_age_old_b <- base::pmax(1, phi_df_age_old_b$phi[match(dsr_age_old_b$agegroup_old, phi_df_age_old_b$agegroup)])


#### Combine phi ---------------------------


dsr_age_old_b<- cbind.data.frame(dplyr::as_tibble(dsr_age_old_b),phi= phi_vec_age_old_b) 

dplyr::select(dsr_age_old_b, rate, rate.lo, rate.hi)|> 
  mutate_if(is.numeric, ~sprintf("%1.1f",.*1e3))
#   rate rate.lo rate.hi
# 1  3.3     2.9     3.8
# 2  6.7     6.3     7.1
# 3 17.8    16.9    18.8
# 4 32.7    29.5    36.3

DSR_1k_corr_agegr_corr_b <- mapply(
  dsr_format_corr,
  dsr_age_old_b$rate.adj,
  dsr_age_old_b$SE.rate.adj,
  dsr_age_old_b$phi,
  MoreArgs = list(factor = 1e3, digits = 6, conf = .95)
)

cat("5-yr binned (intrinsecallt wrong due to sparsity\n")
data.table::data.table(as.vector(sapply(DSR_1k_corr_agegr_corr_b, 
                                        format_interval)))
#                   V1
#               <char>
# 1:     3.7 (3.0–4.5)
# 2:     7.3 (5.7–9.3)
# 3:  16.4 (14.8–18.2)
# 4: 54.4 (21.6–137.0)


#### Het. DSR By age- last tr. -----------------------------------------------------------

dsr_age_dat_b <- dsr_age_old_b |>
  dplyr::mutate(
    yi  = base::log(rate.adj),
    sei = se_log_from_rate_se(rate.adj, SE.rate.adj, phi = phi)
  )

meta_dsr_age_b <- metafor::rma(yi = yi, sei = sei, method = "FE", data = dsr_age_dat_b)

c(Q = sprintf("%1.2f",meta_dsr_age_b$QE), 
  df = meta_dsr_age_b$k - meta_dsr_age_b$p, 
  p = sprintf("%1.3f",meta_dsr_age_b$QEp), 
  I2 = sprintf("%1.1f",meta_dsr_age_b$I2))
#        Q       df        p       I2 
# "190.66"      "1"  "0.000"   "98.4" 

### SMR by old ages ---------------------------

c_SISTRAT_b_std_bins_wold <- c_SISTRAT_b_std_bins |>
  dplyr::mutate(agegroup_old = to_old_agegroup(agegroup))

smr_age_old_b <- 
  popEpi::sir(c_SISTRAT_b_std_bins_wold, coh.obs = 'from0to1',
              coh.pyrs = 'pyrs',
              ref.data = mx_national_clean,
              ref.rate = mx,
              print = c("agegroup_old"),
              adjust = c("year", "sex", "agegroup"), #2025-12-16: the original still adjusts for agegroup
              test.type = "homogeneity",
              conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
              conf.level = 0.95, EAR = T)


sir_ci_phi_improved( smr_age_old_b, dsr_age_old_b$phi)|> 
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# 1: 3.643448 3.194358 4.155674 1.000000 3.64 (3.19–4.16)
# 2: 4.495205 4.210290 4.799401 1.224403 4.50 (4.21–4.80)
# 3: 4.272924 4.051045 4.506957 1.000000 4.27 (4.05–4.51)
# 4: 2.691037 2.426582 2.984313 1.000000 2.69 (2.43–2.98)

#### Het. SMR By age- last tr. -----------------------------------------------------------

smr_age_dat_b <- sir_ci_phi_improved( smr_age_old_b, dsr_age_old_b$phi) |>
  dplyr::mutate(
    yi  = base::log(SIR),
    sei = se_log_from_ci(SIR,CI_low, CI_high, conf = 0.95)
  )

meta_smr_age_b <- metafor::rma(
  yi = yi,
  sei = sei,
  method = "FE",
  data = smr_age_dat_b
)

c(Q = sprintf("%1.2f",meta_smr_age_b$QE), 
  df = meta_smr_age_b$k - meta_smr_age_b$p, 
  p = sprintf("%1.3f",meta_smr_age_b$QEp), 
  I2 = sprintf("%1.1f",meta_smr_age_b$I2))
#       Q      df       p      I2 
# "76.09"     "3" "0.000"  "96.1"


## DSR w/ 5-yr-bin lexis, last tr.- by sex -------------------------------------------

### DSR by sex--------------------------

phi_sex_df_b  <- extract_phi_by_sex(c_SISTRAT_b_std_bins)

dsr_by_sex_b <- popEpi::rate(
  data    = c_SISTRAT_b_std_bins,
  obs     = "from0to1",
  pyrs    = "pyrs",
  print   = "sex",
  adjust  = c("year","agegroup"),
  weights = list(year = w_year_5y, agegroup = w_age_5y)
)

dplyr::select(dsr_by_sex_b, rate, rate.lo, rate.hi)|> 
  mutate_if(is.numeric, ~sprintf("%1.1f",.*1e3))
#   rate rate.lo rate.hi
# 1 10.0     9.6    10.4
# 2  8.0     7.4     8.7

phi_sex_vec_b <- pmax(1, phi_sex_df_b$phi[match(dsr_by_sex_b$sex, phi_sex_df_b$sex)])
dsr_by_sex_b <- cbind.data.frame(dplyr::as_tibble(dsr_by_sex_b),phi= phi_sex_vec_b) 

DSR_1k_corr_sex_corr_b <- mapply(
  dsr_format_corr,
  dsr_by_sex_b $rate.adj,
  dsr_by_sex_b $SE.rate.adj,
  phi     = 1, #not necessary (phis <1)
  MoreArgs = list(factor = 1e3, digits = 6, conf = .95)
)


cat("\n#_#_#_#_#_#_#_\nResults of DSRs by sex\n#_#_#_#_#_#_#_\n")
data.table::data.table(as.vector(sapply(DSR_1k_corr_sex_corr_b, 
                                        format_interval)))
#                  V1
#              <char>
# 1: 15.7 (10.6–23.3)
# 2:   9.0 (7.4–11.0)

message("Previous DSRs by sex\nMen = 21.9 (9.1–53.0)\nWomen = 10.3 (8.8–12.0)\nFor men and women, rates decreased.")
message("For main analyses: Previous DSRs by sex\nMen = 18.5\nWomen = 9.1 \n \nFor men and women, rates decreased.")
message("For men, it has decreased substantially")


#### Het. DSR By sex- last tr. -----------------------------------------------------------

dsr_sex_dat_b <- dsr_by_sex_b |>
  dplyr::mutate(sex = base::as.character(sex)) |>
  dplyr::mutate(
    yi  = base::log(rate.adj),
    sei = se_log_from_rate_se(rate.adj, SE.rate.adj, phi = phi)
  )

meta_dsr_sex_b <- metafor::rma(
  yi = yi,
  sei = sei,
  method = "FE",
  data = dsr_sex_dat_b
)

c(Q = sprintf("%1.2f",meta_dsr_sex_b$QE), 
  df = meta_dsr_sex_b$k - meta_dsr_sex_b$p, 
  p = sprintf("%1.3f",meta_dsr_sex_b$QEp), 
  I2 = sprintf("%1.1f",meta_dsr_sex_b$I2))
#      Q      df       p      I2 
# "6.18"     "1" "0.013"  "83.8"


### SMR by sex--------------------------

smr_sex_corr_b <- popEpi::sir(
  coh.data = c_SISTRAT_b_std_bins,
  coh.obs  = "from0to1",
  coh.pyrs = "pyrs",
  ref.data = mx_national_clean,
  ref.rate = "mx",
  print    = "sex",
  adjust   = c("year","sex","agegroup")  # <- 5y internal
)
smr_sex_corr_b 
# SIR (adjusted by year, sex, agegroup) with 95% confidence intervals (profile) 
# Test for homogeneity: p < 0.001 
# 
#  Total sir: 4.01 (3.87-4.16)
#  Total observed: 3029
#  Total expected: 754.55
#  Total person-years: 317628 
# 
# Clave <sex>
#       sex observed expected      pyrs   sir sir.lo sir.hi p_value
#    <char>    <num>    <num>     <num> <num>  <num>  <num>   <num>
# 1: Female      604    98.77  75044.33  6.12   5.64   6.62       0
# 2:   Male     2425   655.78 242583.25  3.70   3.55   3.85       0


smr_sex_corr_tbl_b <- cbind.data.frame(dplyr::as_tibble(smr_sex_corr_b), phi=phi_sex_vec_b)

smr_sex_corr_tbl_b
#      sex observed expected      pyrs    sir sir.lo sir.hi p_value phi
# 1 Female      604  98.7740  75044.33 6.1150 5.6402 6.6157       0   1
# 2   Male     2425 655.7777 242583.25 3.6979 3.5527 3.8470       0   1

sir_ci_phi_improved( smr_sex_corr_b, smr_sex_corr_tbl_b$phi)|> 
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# Female: 6.114970 5.646241 6.622610        1 6.11 (5.65–6.62)
# Male: 3.697899 3.553610 3.848047        1 3.70 (3.55–3.85)


message("Previous DSRs by sex\nMen = 6.03 (5.44-6.69)\nWomen = 3.65 (3.42-3.89)\nFor men and women, SMRs increased from 5.86 and 3.62.")
message("For main analyses: Previous SMRs by sex\nMen = 5.47\nWomen = 3.30 \n \nFor men and women, SMRs increased to 5.57 and 3.36.")


#### Het. SMR By sex- last tr. -----------------------------------------------------------

smr_sex_dat_b <- sir_ci_phi_improved( smr_sex_corr_b, phi_sex_vec_b) |>
  dplyr::mutate(
    yi  = base::log(SIR),
    sei = se_log_from_ci(SIR, CI_low, CI_high, conf = 0.95)
  )

meta_smr_sex_b <- metafor::rma(
  yi = yi,
  sei = sei,
  method = "FE",
  data = smr_sex_dat_b
)

c(Q = sprintf("%1.2f",meta_smr_sex_b$QE), 
  df = meta_smr_sex_b$k - meta_smr_sex_b$p, 
  p = sprintf("%1.3f",meta_smr_sex_b$QEp), 
  I2 = sprintf("%1.1f",meta_smr_sex_b$I2))
#        Q       df        p       I2 
# "122.33"      "1"  "0.000"   "99.2" 


# Sensibility, incomplete/unfinished treatments --------------------------------------------------------------------


## SMR, Overall  -----------------------------------------------------------

c_SISTRAT_c_std_bins <- lexpand(
  clean_df_c, 
  status = status, 
  birth = birth_date_rec, 
  exit = death_date_rec, 
  entry = disch_date_corr,
  breaks = list(
    per = seq(2010, 2021, by = 1), 
    age = age_breaks # <--- This matches the reference keys (15, 20, 25...)
  ),
  aggre = list(year = per, 
               agegroup = age, 
               sex = sex_rec)
)

sort(unique(c_SISTRAT_c_std_bins$agegroup))


sir_final_c_std <- sir(
  coh.data = c_SISTRAT_c_std_bins, 
  coh.obs = 'from0to1',    # Observed deaths column
  coh.pyrs = 'pyrs',       # Person-years column
  ref.data = mx_national_clean, 
  ref.rate = 'mx',         # The mortality rate column in reference
  adjust = c('agegroup', 'year', 'sex')#, 
  #print = 'total'
)

print(sir_final_c_std)
# SIR (adjusted by agegroup, year, sex) with 95% confidence intervals (profile) 
# 
#  Total sir: 4 (3.88-4.13)
#  Total observed: 3817
#  Total expected: 953.61
#  Total person-years: 416286 
# 
#    observed expected     pyrs   sir sir.lo sir.hi p_value
#       <num>    <num>    <num> <num>  <num>  <num>   <num>
# 1:     3817   953.61 416285.6     4   3.88   4.13       0

sir_ci_phi_improved( sir_final_c_std, extract_phi(c_SISTRAT_b_std_bins)) |>
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# 1: 4.002704 3.877715 4.131721        1 4.00 (3.88–4.13)

sir_tot_c

# SIR (adjusted by agegroup, year, sex) with 95% confidence intervals (profile) 
# 
#  Total sir: 3.93 (3.81-4.06)
#  Total observed: 3817
#  Total expected: 970.26
#  Total person-years: 416286 
# 
#    observed expected     pyrs   sir sir.lo sir.hi p_value   EAR
#       <num>    <num>    <num> <num>  <num>  <num>   <num> <num>
# 1:     3817   970.26 416285.6  3.93   3.81   4.06       0 6.838

## DSRs  -----------------------------------------------------------


dsr_tot_c <- popEpi::rate(
  data    = c_SISTRAT_c_std_bins,
  obs     = "from0to1",
  pyrs    = "pyrs",
  adjust  = c("year","sex","agegroup"),
  weights = weights_corr
)

dsr_tot_c$rate_1k <- dsr_tot_c$rate * 1e3


DSR_1k_corr_corr_c <- mapply(
  dsr_format_corr,                 # FUN
  dsr_tot_c$rate.adj,           # primer vector (rate)
  dsr_tot_c$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    #phi    = extract_phi_dir(c_SISTRAT_std_bins),
    #opc. conservadora
    phi    = max(1,  extract_phi_dir(c_SISTRAT_c_std_bins)),
    factor = 1e3,
    digits = 1,
    conf   = 0.95))

DSR_1k_corr_corr_c 
# [1] "11.2 (10.0–12.5)"

message("DSRs decreases from 12.7 to 11.2 from first tr. including unfinished 
tr. sensitivity analysis\nbut SMRs risens from 3.93 to 4.00")


## DSR w/ 5-yr-bin lexis, unfinished tr.- by old ages -------------------------------------------

### DSR, by age ---------------------------

c_SISTRAT_c_std_old <- c_SISTRAT_c_std_bins |>
  dplyr::mutate(agegroup_old = to_old_agegroup(agegroup)) |>
  dplyr::group_by(year, sex, agegroup_old) |>
  dplyr::summarise(
    pyrs     = base::sum(pyrs),
    from0to1 = base::sum(from0to1),
    .groups  = "drop"
  )


dsr_age_old_c <- popEpi::rate(
  data    = c_SISTRAT_c_std_old,
  obs     = "from0to1",
  pyrs    = "pyrs",
  print   = "agegroup_old",
  adjust  = c("year","sex"),
  weights = list(year = w_year, sex = w_sex)
)

#### Etract phi ---------------------------


phi_df_age_old_c <- extract_phi_by_age(c_SISTRAT_c_std_old|> dplyr::rename(agegroup = agegroup_old))

phi_vec_age_old_c <- base::pmax(1, phi_df_age_old_c$phi[match(dsr_age_old_c$agegroup_old, phi_df_age_old_c$agegroup)])


#### Combine phi ---------------------------


dsr_age_old_c<- cbind.data.frame(dplyr::as_tibble(dsr_age_old_c),phi= phi_vec_age_old_c) 

dplyr::select(dsr_age_old_c, rate, rate.lo, rate.hi)|> 
  mutate_if(is.numeric, ~sprintf("%1.1f",.*1e3))
#   rate rate.lo rate.hi
# 1  3.1     2.7     3.5
# 2  6.3     6.0     6.7
# 3 17.8    17.0    18.7
# 4 34.9    31.8    38.2

DSR_1k_corr_agegr_corr_c <- mapply(
  dsr_format_corr,
  dsr_age_old_c$rate.adj,
  dsr_age_old_c$SE.rate.adj,
  dsr_age_old_c$phi,
  MoreArgs = list(factor = 1e3, digits = 6, conf = .95)
)

cat("5-yr binned (intrinsecallt wrong due to sparsity\n")
data.table::data.table(as.vector(sapply(DSR_1k_corr_agegr_corr_c, 
                                        format_interval)))
#                  V1
#              <char>
# 1:    3.0 (2.5–3.5)
# 2:    6.2 (5.4–7.1)
# 3: 16.5 (15.0–18.2)
# 4: 38.0 (24.7–58.5)


### SMR by old ages ---------------------------

c_SISTRAT_c_std_bins_wold <- c_SISTRAT_c_std_bins |>
  dplyr::mutate(agegroup_old = to_old_agegroup(agegroup))

smr_age_old_c <- 
  popEpi::sir(c_SISTRAT_c_std_bins_wold, coh.obs = 'from0to1',
              coh.pyrs = 'pyrs',
              ref.data = mx_national_clean,
              ref.rate = mx,
              print = c("agegroup_old"),
              adjust = c("year", "sex", "agegroup"), #2025-12-16: the original still adjusts for agegroup
              test.type = "homogeneity",
              conf.type = "wald", #conf.type = "wald" usa la aproximación normal de Poisson (la misma lógica que explicamos antes).
              conf.level = 0.95, EAR = T)


sir_ci_phi_improved( smr_age_old_c, dsr_age_old_c$phi)|> 
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# 1: 3.438973 3.060766 3.863914        1 3.44 (3.06–3.86)
# 2: 4.315371 4.092010 4.550924        1 4.32 (4.09–4.55)
# 3: 4.327075 4.126770 4.537103        1 4.33 (4.13–4.54)
# 4: 2.882439 2.631746 3.157011        1 2.88 (2.63–3.16)


## DSR w/ 5-yr-bin lexis, unfinished tr.- by sex -------------------------------------------

### DSR by sex--------------------------

phi_sex_df_c  <- extract_phi_by_sex(c_SISTRAT_c_std_bins)

dsr_by_sex_c <- popEpi::rate(
  data    = c_SISTRAT_c_std_bins,
  obs     = "from0to1",
  pyrs    = "pyrs",
  print   = "sex",
  adjust  = c("year","agegroup"),
  weights = list(year = w_year_5y, agegroup = w_age_5y)
)

dplyr::select(dsr_by_sex_c, rate, rate.lo, rate.hi)|> 
  mutate_if(is.numeric, ~sprintf("%1.1f",.*1e3))
#   rate rate.lo rate.hi
# 1  9.7     9.4    10.1
# 2  7.6     7.1     8.1

phi_sex_vec_c <- pmax(1, phi_sex_df_c$phi[match(dsr_by_sex_c$sex, phi_sex_df_c$sex)])
dsr_by_sex_c <- cbind.data.frame(dplyr::as_tibble(dsr_by_sex_c),phi= phi_sex_vec_c) 

DSR_1k_corr_sex_corr_c <- mapply(
  dsr_format_corr,
  dsr_by_sex_c $rate.adj,
  dsr_by_sex_c $SE.rate.adj,
  phi     = 1, #not necessary (phis <1)
  MoreArgs = list(factor = 1e3, digits = 6, conf = .95)
)


cat("\n#_#_#_#_#_#_#_\nResults of DSRs by sex\n#_#_#_#_#_#_#_\n")
data.table::data.table(as.vector(sapply(DSR_1k_corr_sex_corr_c, 
                                        format_interval)))
#                  V1
#              <char>
# 1: 14.2 (11.9–16.8)
# 2:    8.4 (7.3–9.8)

message("Previous DSRs by sex\nMen = 16.6 (12.2–22.7)\nWomen = 9.5 (8.4–10.6)\nFor men and women, rates decreased.")

### SMR by sex--------------------------

smr_sex_corr_c <- popEpi::sir(
  coh.data = c_SISTRAT_c_std_bins,
  coh.obs  = "from0to1",
  coh.pyrs = "pyrs",
  ref.data = mx_national_clean,
  ref.rate = "mx",
  print    = "sex",
  adjust   = c("year","sex","agegroup")  # <- 5y internal
)
smr_sex_corr_c 
# SIR (adjusted by year, sex, agegroup) with 95% confidence intervals (profile) 
# Test for homogeneity: p < 0.001 
# 
#  Total sir: 4 (3.88-4.13)
#  Total observed: 3817
#  Total expected: 953.61
#  Total person-years: 416286 
# 
# Clave <sex>
#       sex observed expected     pyrs   sir sir.lo sir.hi p_value
#    <char>    <num>    <num>    <num> <num>  <num>  <num>   <num>
# 1: Female      805   134.91 106197.2  5.97   5.56   6.39       0
# 2:   Male     3012   818.70 310088.4  3.68   3.55   3.81       0


smr_sex_corr_tbl_c <- cbind.data.frame(dplyr::as_tibble(smr_sex_corr_c), phi=phi_sex_vec_c)

smr_sex_corr_tbl_c
#      sex observed expected     pyrs   sir sir.lo sir.hi p_value phi
# 1 Female      805 134.9093 106197.2 5.967 5.5642 6.3887       0   1
# 2   Male     3012 818.6961 310088.4 3.679 3.5492 3.8120       0   1

sir_ci_phi_improved( smr_sex_corr_c, smr_sex_corr_tbl_c$phi)|> 
  dplyr::mutate(print= sprintf("%.2f (%.2f–%.2f)", 
                               SIR, 
                               CI_low, 
                               CI_high)) |> 
  data.table::data.table()
#         SIR   CI_low  CI_high phi_used            print
#       <num>    <num>    <num>    <num>           <char>
# 1: 5.966972 5.568690 6.393739        1 5.97 (5.57–6.39)
# 2: 3.679021 3.549952 3.812782        1 3.68 (3.55–3.81)

message("Previous DSRs by sex\nWomen = 5.86 (5.40–6.36)\nMen = 3.62 (3.46–3.78)\nFor men and women, SMRs increased")




## By strata, last tr. -----------------------------------------------------

clean_df2_b <- clean_df_b |>
  dplyr::mutate(
    # 0/1 -> labels ( 1 = Residential, 0 = Ambulatory)
    setting = dplyr::if_else(res_plan == 1, "Residential", "Ambulatory"),
    setting = base::factor(setting, levels = c("Ambulatory","Residential")),
    
    completed = base::as.character(tr_compliance_status),
    completed = base::factor(completed, levels = c("Completed","Not completed")),
    
    licit_illicit = base::as.character(prim_sub_licit),
    licit_illicit = base::factor(licit_illicit, levels = c("licit","illicit")),
    
    disch_age_cat = base::as.character(disch_age_cat),
    disch_age_cat = base::factor(disch_age_cat, levels = c("18-29","30-44","45-59","60+")),
    
    # Sex: asegúrate que sea Male/Female como tus tasas nacionales
    sex = stringr::str_to_title(base::as.character(sex_rec)),
    sex = base::factor(sex, levels = c("Male","Female"))
  )

# ---- 2) 5-year attained-age Lexis (keep 2021 as boundary)

c_SISTRAT_5y_b <- popEpi::lexpand(
  data   = clean_df2_b,
  status = status,
  birth  = birth_date_rec,
  entry  = disch_date_rec6,
  exit   = death_date_rec,
  breaks = list(
    per = base::seq(2010, 2021, by = 1),
    age = age_breaks
  ),
  aggre = list(
    year    = per,
    agegroup= age,      # 5y attained-age keys (15,20,25,...)
    sex     = sex,
    
    # subgroup vars (constantes por individuo; quedan replicadas por split)
    setting      = setting,
    completed    = completed,
    licit_illicit= licit_illicit,
    disch_age_cat= disch_age_cat
  )
) 
warning("disch_age_cat es edad al alta (baseline) → perfecto para “preservar 
los grupos antiguos” aunque el Lexis use attained-age 5y internamente para el ajuste.")


### DSR strata, last tr. ---------------------------------------------------------------------

dsr_setting_b   <- run_dsr_by(c_SISTRAT_5y_b, weights_corr, "setting")
dsr_completed_b <- run_dsr_by(c_SISTRAT_5y_b, weights_corr, "completed")
dsr_licit_b     <- run_dsr_by(c_SISTRAT_5y_b, weights_corr, "licit_illicit")

as.data.frame(t(dsr_setting_b))|> 
  row_to_names(row_number = 1)
#                     Ambulatory        Residential
# from0to1                  2452                577
# pyrs                 268495.40           49132.18
# rate.adj            0.01164708         0.01285002
# SE.rate.adj        0.001379992        0.001490091
# rate.adj.lo        0.009233399        0.010237564
# rate.adj.hi         0.01469172         0.01612914
# rate               0.009132372        0.011743831
# SE.rate           0.0001844265       0.0004889021
# rate.lo            0.008777957        0.010823635
# rate.hi            0.009501098        0.012742259
# n_events                  2452                577
# n_rows                    1355               1193
# n_rows_used               1355               1193
# phi_raw               1.340774           1.335833
# converged                 TRUE               TRUE
# warning                   <NA>               <NA>
# phi                   1.340774           1.335833
# rate_1k               11.64708           12.85002
# sei_log              0.1371946          0.1340247
# rate_1k_L             8.900960           9.881477
# rate_1k_U             15.24044           16.71036
# print       11.65 (8.90–15.24) 12.85 (9.88–16.71)

as.data.frame(t(dsr_completed_b))|> 
  row_to_names(row_number = 1)
#                     Completed       Not completed
# from0to1                  716                2313
# pyrs                 81225.85           236401.73
# rate.adj           0.00864130          0.01362958
# SE.rate.adj      0.0009837103        0.0016164120
# rate.adj.lo       0.006913183         0.010802687
# rate.adj.hi        0.01080140          0.01719624
# rate              0.008814928         0.009784192
# SE.rate          0.0003294293        0.0002034404
# rate.lo           0.008192327         0.009393465
# rate.hi           0.009484845         0.010191172
# n_events                  716                2313
# n_rows                   1236                1312
# n_rows_used              1236                1312
# phi_raw              1.583580            1.641092
# converged                TRUE                TRUE
# warning                  <NA>                <NA>
# phi                  1.583580            1.641092
# rate_1k               8.64130            13.62958
# sei_log             0.1432545           0.1519273
# rate_1k_L            6.525904           10.119564
# rate_1k_U            11.44241            18.35707
# print       8.64 (6.53–11.44) 13.63 (10.12–18.36)

as.data.frame(t(dsr_licit_b))|> 
  row_to_names(row_number = 1)
#                           licit            illicit
# from0to1                   1787               1242
# pyrs                   98631.23          218996.35
# rate.adj             0.01493749         0.01056017
# SE.rate.adj         0.001139485        0.002770844
# rate.adj.lo         0.012863042        0.006314267
# rate.adj.hi          0.01734648         0.01766113
# rate                0.018117994        0.005671327
# SE.rate            0.0004285957       0.0001609251
# rate.lo             0.017297123        0.005364525
# rate.hi             0.018977820        0.005995677
# n_events                   1787               1242
# n_rows                     1257               1291
# n_rows_used                1257               1291
# phi_raw                1.627785           0.946239
# converged                  TRUE               TRUE
# warning                    <NA>               <NA>
# phi                    1.627785           1.000000
# rate_1k                14.93749           10.56017
# sei_log              0.09732614         0.26238638
# rate_1k_L             12.343360           6.314327
# rate_1k_U              18.07681           17.66097
# print       14.94 (12.34–18.08) 10.56 (6.31–17.66)


as.data.frame(t(run_dsr_by_agecat (c_SISTRAT_5y_b, w_year, w_sex)))|> 
  row_to_names(row_number = 1)
#                        18-29             30-44               45-59                  60+
# from0to1                 381              1204                1245                  199
# pyrs              105162.370        149338.603           57747.092             5379.515
# rate.adj         0.003840627       0.008326081         0.018713564          0.055269788
# SE.rate.adj     0.0003507792      0.0008252769        0.0009621252         0.0257445900
# rate.adj.lo      0.003211125       0.006855962         0.016919700          0.022181557
# rate.adj.hi      0.004593535       0.010111436         0.020697617          0.137715738
# rate             0.003622969       0.008062215         0.021559527          0.036992184
# SE.rate         0.0001856103      0.0002323492        0.0006110188         0.0026223064
# rate.lo          0.003276841       0.007619434         0.020394585          0.032193544
# rate.hi          0.004005657       0.008530728         0.022791011          0.042506090
# n_events                 381              1204                1245                  199
# n_rows                   722               771                 723                  332
# n_rows_used              722               771                 723                  332
# phi_raw             1.378092          2.215765            1.403023             1.417272
# converged               TRUE              TRUE                TRUE                 TRUE
# warning                 <NA>              <NA>                <NA>                 <NA>
# phi                 1.378092          2.215765            1.403023             1.417272
# rate_1k             3.840627          8.326081           18.713564            55.269788
# sei_log           0.10721875        0.14754380          0.06089862           0.55452970
# rate_1k_L           3.112701          6.235210           16.608083            18.641088
# rate_1k_U           4.738784         11.118089           21.085966           163.871844
# print       3.84 (3.11–4.74) 8.33 (6.24–11.12) 18.71 (16.61–21.09) 55.27 (18.64–163.87)



### SMR strata, last tr. ---------------------------------------------------------------------

smr_setting_b   <- run_smr_by(c_SISTRAT_5y_b, mx_national_clean, "setting")
smr_completed_b <- run_smr_by(c_SISTRAT_5y_b, mx_national_clean, "completed")
smr_licit_b     <- run_smr_by(c_SISTRAT_5y_b, mx_national_clean, "licit_illicit")
smr_agecat_b    <- run_smr_by(c_SISTRAT_5y_b, mx_national_clean, "disch_age_cat")


smr_setting_phi_b <- smr_setting_b%>%
  dplyr::left_join(dplyr::select(dsr_setting_b, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
smr_completed_phi_b <- smr_completed_b%>%
  dplyr::left_join(dplyr::select(dsr_completed_b, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
smr_licit_phi_b <- smr_licit_b%>%
  dplyr::left_join(dplyr::select(dsr_licit_b, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))

smr_agecat_phi_b <- smr_agecat_b%>%
  dplyr::left_join(subset(run_dsr_by_agecat(c_SISTRAT_5y_b, w_year, w_sex), select=c("group","phi")), 
                   by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))

#   group       observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>          <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 Ambulatory      2452     653. 268495.  3.76   3.61   3.91       0  1.34       3.59       3.93 3.76 (3.59–3.93)
# 2 Residential      577     102.  49132.  5.68   5.23   6.16       0  1.34       5.17       6.24 5.68 (5.17–6.24)

#   group         observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>            <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 Completed          716     242.  81226.  2.96   2.75   3.18       0  1.58       2.70       3.25 2.96 (2.70–3.25)
# 2 Not completed     2313     513. 236402.  4.51   4.33   4.70       0  1.64       4.28       4.75 4.51 (4.28–4.75)

#   group   observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>      <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 licit       1787     353.  98631.  5.06   4.83   5.30       0  1.63       4.77       5.37 5.06 (4.77–5.37)
# 2 illicit     1242     401. 218996.  3.10   2.93   3.27       0  1          2.93       3.27 3.10 (2.93–3.27)

#   group observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>    <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 18-29      381    107.  105162.  3.58   3.23   3.95       0  1.38       3.18       4.02 3.58 (3.18–4.02)
# 2 30-44     1204    271.  149339.  4.44   4.19   4.70       0  2.22       4.08       4.83 4.44 (4.08–4.83)
# 3 45-59     1245    304.   57747.  4.09   3.87   4.33       0  1.40       3.83       4.37 4.09 (3.83–4.37)
# 4 60+        199     72.7   5380.  2.74   2.37   3.13       0  1.42       2.32       3.23 2.74 (2.32–3.23)


### Het.- last tr.  -----------------------------------------------------------

# --- DSR: Treatment Setting ---
meta_dsr_setting_b <- dsr_setting_b |>
  dplyr::mutate(
    # Use phi if present, else 1
    phi_used = if("phi" %in% names(dsr_setting_b)) phi else 1,
    yi  = log(rate.adj),
    sei = (SE.rate.adj * sqrt(phi_used)) / rate.adj
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- DSR Heterogeneity: Setting ---\n")
c(Q = sprintf("%1.2f", meta_dsr_setting_b$QE), 
  df = meta_dsr_setting_b$k - meta_dsr_setting_b$p, 
  p = sprintf("%1.3f", meta_dsr_setting_b$QEp), 
  I2 = sprintf("%1.1f", meta_dsr_setting_b$I2))
#     Q      df       p      I2 
# "0.26"     "1" "0.608"   "0.0" 

# --- SMR: Treatment Setting ---
meta_smr_setting_b <- smr_setting_phi_b |>
  dplyr::mutate(
    yi  = log(sir),
    # Calculate SE on log scale from the Phi-corrected CIs (averaging upper and lower)
    sei = 0.5 * ((log(sir.phi.hi) - log(sir))/1.96 + (log(sir) - log(sir.phi.lo))/1.96)
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- SMR Heterogeneity: Setting ---\n")
c(Q = sprintf("%1.2f", meta_smr_setting_b$QE), 
  df = meta_smr_setting_b$k - meta_smr_setting_b$p, 
  p = sprintf("%1.3f", meta_smr_setting_b$QEp), 
  I2 = sprintf("%1.1f", meta_smr_setting_b$I2))
#       Q      df       p      I2 
# "59.87"     "1" "0.000"  "98.3" 

# --- DSR: Compliance ---
meta_dsr_compl_b <- dsr_completed_b |>
  dplyr::mutate(
    phi_used = if("phi" %in% names(dsr_completed_b)) phi else 1,
    yi  = log(rate.adj),
    sei = (SE.rate.adj * sqrt(phi_used)) / rate.adj
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- DSR Heterogeneity: Compliance ---\n")
c(Q = sprintf("%1.2f", meta_dsr_compl_b$QE), 
  df = meta_dsr_compl_b$k - meta_dsr_compl_b$p, 
  p = sprintf("%1.3f", meta_dsr_compl_b$QEp), 
  I2 = sprintf("%1.1f", meta_dsr_compl_b$I2))
#     Q      df       p      I2 
# "4.76"     "1" "0.029"  "79.0" 

# --- SMR: Compliance ---
meta_smr_compl_b <- smr_completed_phi_b |>
  dplyr::mutate(
    yi  = log(sir),
    sei = 0.5 * ((log(sir.phi.hi) - log(sir))/1.96 + (log(sir) - log(sir.phi.lo))/1.96)
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- SMR Heterogeneity: Compliance ---\n")
c(Q = sprintf("%1.2f", meta_smr_compl_b$QE), 
  df = meta_smr_compl_b$k - meta_smr_compl_b$p, 
  p = sprintf("%1.3f", meta_smr_compl_b$QEp), 
  I2 = sprintf("%1.1f", meta_smr_compl_b$I2))
#       Q      df       p      I2 
# "60.69"     "1" "0.000"  "98.4" 

# --- DSR: Licit/Illicit ---
meta_dsr_licit_b <- dsr_licit_b |>
  dplyr::mutate(
    phi_used = if("phi" %in% names(dsr_licit_b)) phi else 1,
    yi  = log(rate.adj),
    sei = (SE.rate.adj * sqrt(phi_used)) / rate.adj
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- DSR Heterogeneity: Licit/Illicit ---\n")
c(Q = sprintf("%1.2f", meta_dsr_licit_b$QE), 
  df = meta_dsr_licit_b$k - meta_dsr_licit_b$p, 
  p = sprintf("%1.3f", meta_dsr_licit_b$QEp), 
  I2 = sprintf("%1.1f", meta_dsr_licit_b$I2))
#      Q      df       p      I2 
# "1.54"     "1" "0.215"  "34.9" 

# --- SMR: Licit/Illicit ---
meta_smr_licit_b <- smr_licit_phi_b |>
  dplyr::mutate(
    yi  = log(sir),
    sei = 0.5 * ((log(sir.phi.hi) - log(sir))/1.96 + (log(sir) - log(sir.phi.lo))/1.96)
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- SMR Heterogeneity: Licit/Illicit ---\n")
c(Q = sprintf("%1.2f", meta_smr_licit_b$QE), 
  df = meta_smr_licit_b$k - meta_smr_licit_b$p, 
  p = sprintf("%1.3f", meta_smr_licit_b$QEp), 
  I2 = sprintf("%1.1f", meta_smr_licit_b$I2))
#        Q       df        p       I2 
# "140.64"      "1"  "0.000"   "99.3" 


## By strata, unfinished tr. -----------------------------------------------------



clean_df2_c <- clean_df_c |>
  dplyr::mutate(
    # 0/1 -> labels ( 1 = Residential, 0 = Ambulatory)
    setting = dplyr::if_else(res_plan == 1, "Residential", "Ambulatory"),
    setting = base::factor(setting, levels = c("Ambulatory","Residential")),
    
    completed = base::as.character(tr_compliance_status),
    completed = base::factor(completed, levels = c("Completed","Not completed")),
    
    licit_illicit = base::as.character(prim_sub_licit),
    licit_illicit = base::factor(licit_illicit, levels = c("licit","illicit")),
    
    disch_age_cat = base::as.character(disch_age_cat),
    disch_age_cat = base::factor(disch_age_cat, levels = c("18-29","30-44","45-59","60+")),
    
    # Sex: asegúrate que sea Male/Female como tus tasas nacionales
    sex = stringr::str_to_title(base::as.character(sex_rec)),
    sex = base::factor(sex, levels = c("Male","Female"))
  )

# ---- 2) 5-year attained-age Lexis (keep 2021 as boundary)

table(clean_df2_c$tr_compliance_status, exclude=NULL)
# Completed Not completed          <NA> 
#     19213         51047         13522 

c_SISTRAT_5y_c <- popEpi::lexpand(
  data   = clean_df2_c|>
    dplyr::mutate(tr_compliance_status= 
                    ifelse(is.na(tr_compliance_status),"Unfinished", tr_compliance_status)),
  status = status,
  birth  = birth_date_rec,
  entry  = disch_date_corr,
  exit   = death_date_rec,
  breaks = list(
    per = base::seq(2010, 2021, by = 1),
    age = age_breaks
  ),
  aggre = list(
    year    = per,
    agegroup= age,      # 5y attained-age keys (15,20,25,...)
    sex     = sex,
    
    # subgroup vars (constantes por individuo; quedan replicadas por split)
    setting      = res_plan,
    completed    = tr_compliance_status,
    licit_illicit= licit_illicit,
    disch_age_cat= disch_age_cat
  )
) 
warning("disch_age_cat es edad al alta (baseline) → perfecto para “preservar 
los grupos antiguos” aunque el Lexis use attained-age 5y internamente para el ajuste.")

warning("2025-12-30= I chose to separate into Unfinished and Tr. completed\n")
c_SISTRAT_5y_c <- c_SISTRAT_5y_c |>
  dplyr::mutate(
    setting = factor(setting, levels = c(0, 1),
                     labels = c("Ambulatory", "Residential"))
  )


### DSR strata, unfinished tr. ---------------------------------------------------------------------

dsr_setting_c   <- run_dsr_by(c_SISTRAT_5y_c, weights_corr, "setting")
dsr_completed_c <- run_dsr_by(c_SISTRAT_5y_c, weights_corr, "completed")
dsr_licit_c     <- run_dsr_by(c_SISTRAT_5y_c, weights_corr, "licit_illicit")

as.data.frame(t(dsr_setting_c))|> 
  row_to_names(row_number = 1)

#                     Ambulatory        Residential
# from0to1                  3163                654
# pyrs                 355444.65           60840.95
# rate.adj            0.01159299         0.01097142
# SE.rate.adj       0.0009717955       0.0008340229
# rate.adj.lo        0.009836515        0.009452686
# rate.adj.hi         0.01366312         0.01273417
# rate               0.008898713        0.010749339
# SE.rate           0.0001582259       0.0004203324
# rate.lo            0.008593932        0.009956267
# rate.hi            0.009214303        0.011605584
# n_events                  3163                654
# n_rows                    2061               1812
# n_rows_used               2061               1812
# phi_raw               1.398012           1.418281
# converged                 TRUE               TRUE
# warning                   <NA>               <NA>
# phi                   1.398012           1.418281
# rate_1k               11.59299           10.97142
# sei_log             0.09911398         0.09053079
# rate_1k_L             9.546178           9.187617
# rate_1k_U             14.07866           13.10155
# print       11.59 (9.55–14.08) 10.97 (9.19–13.10)

as.data.frame(t(dsr_completed_c))|> 
  row_to_names(row_number = 1)
#                     Completed       Not completed                                   Unfinished
# from0to1                  678                2476                                          663
# pyrs                 83249.53           270653.66                                     62382.41
# rate.adj          0.007978942         0.012949888                                  0.012950903
# SE.rate.adj      0.0008320842        0.0013262607                                 0.0010927357
# rate.adj.lo       0.006503933         0.010594702                                  0.010976866
# rate.adj.hi       0.009788463         0.015828628                                  0.015279942
# rate              0.008144190         0.009148223                                  0.010627996
# SE.rate          0.0003127758        0.0001838491                                 0.0004127572
# rate.lo           0.007553655         0.008794883                                  0.009849016
# rate.hi           0.008780894         0.009515758                                  0.011468587
# n_events                  678                2476                                          663
# n_rows                   1265                1320                                         1288
# n_rows_used              1265                1320                                         1288
# phi_raw              1.627670            1.513628                                     1.370549
# converged                TRUE                TRUE                                         TRUE
# warning                  <NA>                <NA> glm.fit: fitted rates numerically 0 occurred
# phi                  1.627670            1.513628                                     1.370549
# rate_1k              7.978942           12.949888                                    12.950903
# sei_log            0.13304704          0.12600055                                   0.09877849
# rate_1k_L            6.147456           10.116124                                    10.671357
# rate_1k_U            10.35607            16.57746                                     15.71739
# print       7.98 (6.15–10.36) 12.95 (10.12–16.58)                          12.95 (10.67–15.72)

as.data.frame(t(dsr_licit_c))|> 
  row_to_names(row_number = 1)
#                           licit           illicit
# from0to1                   2240              1577
# pyrs                   124652.9          291632.7
# rate.adj            0.014775256       0.009529933
# SE.rate.adj        0.0008437851      0.0017237200
# rate.adj.lo         0.013210635       0.006685387
# rate.adj.hi          0.01652518        0.01358480
# rate                0.017969894       0.005407488
# SE.rate            0.0003796833      0.0001361695
# rate.lo             0.017240914       0.005147075
# rate.hi             0.018729698       0.005681076
# n_events                   2240              1577
# n_rows                     1933              1940
# n_rows_used                1933              1940
# phi_raw                1.468904          1.068609
# converged                  TRUE              TRUE
# warning                    <NA>              <NA>
# phi                    1.468904          1.068609
# rate_1k               14.775256          9.529933
# sei_log              0.06921394        0.18697614
# rate_1k_L             12.900899          6.605953
# rate_1k_U              16.92193          13.74815
# print       14.78 (12.90–16.92) 9.53 (6.61–13.75)


as.data.frame(t(run_dsr_by_agecat (c_SISTRAT_5y_c, w_year, w_sex)))|> 
  row_to_names(row_number = 1)
#                        18-29            30-44               45-59                 60+
# from0to1                 509             1496                1551                 261
# pyrs              147306.002       191918.470           70697.093            6364.028
# rate.adj         0.003224959      0.007267978         0.019101432         0.041672186
# SE.rate.adj     0.0002157427     0.0004326928        0.0008981793        0.0082395819
# rate.adj.lo      0.002828653      0.006467510         0.017419687         0.028283973
# rate.adj.hi      0.003676790      0.008167518         0.020945536         0.061397708
# rate             0.003455392      0.007794977         0.021938667         0.041011760
# SE.rate         0.0001531576     0.0002015343        0.0005570630        0.0025385642
# rate.lo          0.003167873      0.007409811         0.020873548         0.036326151
# rate.hi          0.003769006      0.008200164         0.023058137         0.046301752
# n_events                 509             1496                1551                 261
# n_rows                  1106             1178                1089                 500
# n_rows_used             1106             1178                1089                 500
# phi_raw             1.424251         1.924245            1.511991            1.182568
# converged               TRUE             TRUE                TRUE                TRUE
# warning                 <NA>             <NA>                <NA>                <NA>
# phi                 1.424251         1.924245            1.511991            1.182568
# rate_1k             3.224959         7.267978           19.101432           41.672186
# sei_log           0.07983718       0.08258407          0.05781915          0.21501658
# rate_1k_L           2.757825         6.181842           17.054939           27.341595
# rate_1k_U           3.771219         8.544946           21.393491           63.513893
# print       3.22 (2.76–3.77) 7.27 (6.18–8.54) 19.10 (17.05–21.39) 41.67 (27.34–63.51)



### SMR strata, unfinished tr. ---------------------------------------------------------------------

smr_setting_c   <- run_smr_by(c_SISTRAT_5y_c, mx_national_clean, "setting")
smr_completed_c <- run_smr_by(c_SISTRAT_5y_c, mx_national_clean, "completed")
smr_licit_c     <- run_smr_by(c_SISTRAT_5y_c, mx_national_clean, "licit_illicit")
smr_agecat_c    <- run_smr_by(c_SISTRAT_5y_c, mx_national_clean, "disch_age_cat")


smr_setting_phi_c <- smr_setting_c%>%
  dplyr::left_join(dplyr::select(dsr_setting_c, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
smr_completed_phi_c <- smr_completed_c%>%
  dplyr::left_join(dplyr::select(dsr_completed_c, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))
smr_licit_phi_c <- smr_licit_c%>%
  dplyr::left_join(dplyr::select(dsr_licit_c, group, phi), by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))

smr_agecat_phi_c <- smr_agecat_c%>%
  dplyr::left_join(subset(run_dsr_by_agecat(c_SISTRAT_5y_c, w_year, w_sex), select=c("group","phi")), 
                   by = "group")%>%
  sir_ci_phi_profile(phi = .$phi) |>    # <- aplica el CI corregido
  dplyr::mutate(print = sprintf(paste0("%.", 2, "f (%.", 2, "f–%.", 2, "f)"),
                                sir, sir.phi.lo, sir.phi.hi))

#   group       observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>          <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 Ambulatory      3163     829. 355445.  3.81   3.68   3.95       0  1.40       3.66       3.98 3.81 (3.66–3.98)
# 2 Residential      654     124.  60841.  5.26   4.86   5.67       0  1.42       4.80       5.76 5.26 (4.80–5.76)

#   group         observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <chr>            <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 Completed          678     245.  83250.  2.77   2.56   2.98       0  1.63       2.51       3.05 2.77 (2.51–3.05)
# 2 Not completed     2476     576. 270654.  4.30   4.13   4.47       0  1.51       4.10       4.51 4.30 (4.10–4.51)
# 3 Unfinished         663     133.  62382.  4.99   4.62   5.38       0  1.37       4.57       5.46 4.99 (4.57–5.46)

#   group   observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>      <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 licit       2240     435. 124653.  5.15   4.94   5.36       0  1.47       4.90       5.41 5.15 (4.90–5.41)
# 2 illicit     1577     518. 291633.  3.04   2.89   3.19       0  1.07       2.89       3.20 3.04 (2.89–3.20)

#   group observed expected    pyrs   sir sir.lo sir.hi p_value   phi sir.phi.lo sir.phi.hi print           
#   <fct>    <dbl>    <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>      <dbl> <chr>           
# 1 18-29      509    147.  147306.  3.46   3.17   3.77       0  1.42       3.12       3.83 3.46 (3.12–3.83)
# 2 30-44     1496    349.  191918.  4.29   4.08   4.51       0  1.92       4.00       4.60 4.29 (4.00–4.60)
# 3 45-59     1551    372.   70697.  4.17   3.97   4.38       0  1.51       3.93       4.44 4.17 (3.93–4.44)
# 4 60+        261     86.1   6364.  3.03   2.68   3.41       0  1.18       2.66       3.46 3.03 (2.66–3.46)



### Het.- unfinished tr.  -----------------------------------------------------------

# --- DSR: Treatment Setting ---
meta_dsr_setting_c <- dsr_setting_c |>
  dplyr::mutate(
    # Use phi if present, else 1
    phi_used = if("phi" %in% names(dsr_setting_b)) phi else 1,
    yi  = log(rate.adj),
    sei = (SE.rate.adj * sqrt(phi_used)) / rate.adj
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- DSR Heterogeneity: Setting ---\n")
c(Q = sprintf("%1.2f", meta_dsr_setting_c$QE), 
  df = meta_dsr_setting_c$k - meta_dsr_setting_c$p, 
  p = sprintf("%1.3f", meta_dsr_setting_c$QEp), 
  I2 = sprintf("%1.1f", meta_dsr_setting_c$I2))
#      Q      df       p      I2 
# "0.17"     "1" "0.681"   "0.0" 

# --- SMR: Treatment Setting ---
meta_smr_setting_c <- smr_setting_phi_c |>
  dplyr::mutate(
    yi  = log(sir),
    # Calculate SE on log scale from the Phi-corrected CIs (averaging upper and lower)
    sei = 0.5 * ((log(sir.phi.hi) - log(sir))/1.96 + (log(sir) - log(sir.phi.lo))/1.96)
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- SMR Heterogeneity: Setting ---\n")
c(Q = sprintf("%1.2f", meta_smr_setting_c$QE), 
  df = meta_smr_setting_c$k - meta_smr_setting_c$p, 
  p = sprintf("%1.3f", meta_smr_setting_c$QEp), 
  I2 = sprintf("%1.1f", meta_smr_setting_c$I2))
#       Q      df       p      I2 
# "39.37"     "1" "0.000"  "97.5" 

# --- DSR: Compliance ---
meta_dsr_compl_c <- dsr_completed_c |>
  dplyr::mutate(
    phi_used = if("phi" %in% names(dsr_completed_b)) phi else 1,
    yi  = log(rate.adj),
    sei = (SE.rate.adj * sqrt(phi_used)) / rate.adj
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- DSR Heterogeneity: Compliance ---\n")
c(Q = sprintf("%1.2f", meta_dsr_compl_c$QE), 
  df = meta_dsr_compl_c$k - meta_dsr_compl_c$p, 
  p = sprintf("%1.3f", meta_dsr_compl_c$QEp), 
  I2 = sprintf("%1.1f", meta_dsr_compl_c$I2))
#     Q      df       p      I2 
# "9.88"     "2" "0.007"  "79.8" 

# --- SMR: Compliance ---
meta_smr_compl_c <- smr_completed_phi_c |>
  dplyr::mutate(
    yi  = log(sir),
    sei = 0.5 * ((log(sir.phi.hi) - log(sir))/1.96 + (log(sir) - log(sir.phi.lo))/1.96)
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- SMR Heterogeneity: Compliance ---\n")
c(Q = sprintf("%1.2f", meta_smr_compl_c$QE), 
  df = meta_smr_compl_c$k - meta_smr_compl_c$p, 
  p = sprintf("%1.3f", meta_smr_compl_c$QEp), 
  I2 = sprintf("%1.1f", meta_smr_compl_c$I2))
#       Q      df       p      I2 
# "86.84"     "2" "0.000"  "97.7" 

# --- DSR: Licit/Illicit ---
meta_dsr_licit_c <- dsr_licit_c |>
  dplyr::mutate(
    phi_used = if("phi" %in% names(dsr_licit_b)) phi else 1,
    yi  = log(rate.adj),
    sei = (SE.rate.adj * sqrt(phi_used)) / rate.adj
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- DSR Heterogeneity: Licit/Illicit ---\n")
c(Q = sprintf("%1.2f", meta_dsr_licit_c$QE), 
  df = meta_dsr_licit_c$k - meta_dsr_licit_c$p, 
  p = sprintf("%1.3f", meta_dsr_licit_c$QEp), 
  I2 = sprintf("%1.1f", meta_dsr_licit_c$I2))
#     Q      df       p      I2 
# "4.84"     "1" "0.028"  "79.3"

# --- SMR: Licit/Illicit ---
meta_smr_licit_c <- smr_licit_phi_c |>
  dplyr::mutate(
    yi  = log(sir),
    sei = 0.5 * ((log(sir.phi.hi) - log(sir))/1.96 + (log(sir) - log(sir.phi.lo))/1.96)
  ) |>
  metafor::rma(yi = yi, sei = sei, method = "FE", data = _)

cat("\n--- SMR Heterogeneity: Licit/Illicit ---\n")
c(Q = sprintf("%1.2f", meta_smr_licit_c$QE), 
  df = meta_smr_licit_c$k - meta_smr_licit_c$p, 
  p = sprintf("%1.3f", meta_smr_licit_c$QEp), 
  I2 = sprintf("%1.1f", meta_smr_licit_c$I2))
#        Q       df        p       I2 
# "207.44"      "1"  "0.000"   "99.5" 


# Close project -----------------------------------------------------------


# sink(type = "message") 
# sink(type = "output") 
# close(log_all)