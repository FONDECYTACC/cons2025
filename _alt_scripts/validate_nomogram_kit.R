## validate_nomogram_kit.R ------------------------------------------------------
## Regression check: the de-identified kit must reproduce the authoritative
## internal engine (pooled_probtrans_engine_INTERNAL.rds) for both the pooled
## POINT estimate and the pooled Rubin CI, on random patient profiles.
##
## POINT: must match to ~machine precision (identical math, baselines vs survfit).
## CI:    the kit centres the MVN draw scaling at the sample mean (consistent
##        with its sample-centred baseline); the full engine used predict's
##        default (stratum) centring, which is unavailable without microdata.
##        The two are numerically indistinguishable for reporting -- this script
##        quantifies the gap so it can be stated honestly.
##
## Usage: "C:/Program Files/R/R-4.4.1/bin/Rscript.exe" cons/_alt_scripts/validate_nomogram_kit.R

suppressPackageStartupMessages({ library(survival); library(mstate) })

PROJ      <- "g:/My Drive/Alvacast/SISTRAT 2023"
OUT_DIR   <- file.path(PROJ, "data/20241015_out")
ENGINE_IN <- file.path(OUT_DIR, "pooled_probtrans_engine_INTERNAL.rds")
PREDICT_R <- file.path(PROJ, "cons/_alt_scripts/nomogram_predict.R")

e <- readRDS(ENGINE_IN); list2env(e, globalenv())     # engine fns + ms_fits + spec
source(PREDICT_R)                                       # kit_predict_point / kit_predict_ci
kit_files <- sort(list.files(OUT_DIR, pattern = "^nomogram_kit_deident_.*\\.rds$", full.names = TRUE))
if (!length(kit_files)) stop("No kit found; run build_nomogram_kit.R first.")
kit <- readRDS(tail(kit_files, 1))
cat("Kit:", basename(tail(kit_files, 1)), "| version", kit$version, "| imputations", kit$m_imp, "\n\n")

spec_by <- setNames(kit$predictor_spec, vapply(kit$predictor_spec, `[[`, "", "name"))
ref     <- kit$reference
tvec    <- c(3, 6, 12, 36, 60)

set.seed(2125)
rand_profile <- function() {
  p <- ref
  p$adm_age_rec3 <- runif(1,18,64); p$dit_m <- runif(1,0,36); p$porc_pobr <- runif(1,0,0.4)
  for (nm in c("primary_sub_mod_cocaine_paste","primary_sub_mod_cocaine_powder","primary_sub_mod_alcohol",
               "primary_sub_mod_others","prim_sub_freq_rec_2_2_6_days_wk","prim_sub_freq_rec_3_daily",
               "occupation_condition_corr24_unemployed","occupation_condition_corr24_inactive",
               "sex_rec_woman","ethnicity","sub_dep_icd10_status_drug_dependence","polysubstance_strict",
               "eva_consumo_logro_intermedio","eva_consumo_logro_minimo","eva_sm_logro_intermedio","eva_sm_logro_minimo",
               "eva_ocupacion_logro_intermedio","eva_ocupacion_logro_minimo",
               "evaluacindelprocesoteraputico_logro_intermedio","evaluacindelprocesoteraputico_logro_minimo",
               "ed_attainment_corr_2_completed_high_school_or_less","ed_attainment_corr_3_completed_primary_school_or_less",
               "cohabitation_family_of_origin","cohabitation_with_couple_children","cohabitation_others",
               "tr_outcome_dropout","tr_outcome_referral","tr_outcome_adm_discharge_adm_reasons",
               "tr_outcome_adm_discharge_rule_violation_undet"))
    p[[nm]] <- rbinom(1, 1, 0.4)
  p$plan_type_strata <- factor(sample(spec_by$plan_type_strata$levels, 1), levels = spec_by$plan_type_strata$levels)
  p$any_phys_dx      <- factor(sample(c("FALSE","TRUE"), 1), levels = c("FALSE","TRUE"))
  p$dg_psiq_cie_10_dg<- factor(sample(c("FALSE","TRUE"), 1), levels = c("FALSE","TRUE"))
  p
}

## ---- POINT: kit vs engine ----
max_point <- 0
for (it in 1:25) {
  p <- rand_profile()
  k <- kit_predict_point(p, kit, tvec = tvec)
  g <- predict_profile_markov_pooled(p, tvec = tvec, fits = ms_fits)
  max_point <- max(max_point, max(abs(as.matrix(k[,2:4]) - as.matrix(g[,2:4]))))
}
cat(sprintf("POINT  max|kit - engine| over 25 profiles = %.3e  (expect ~1e-15)\n", max_point))

## ---- CI: kit (sample-centred z) vs engine (stratum-centred z), same seed -----
cols <- c("alive_lo","alive_hi","readm_lo","readm_hi","dead_lo","dead_hi")
max_ci <- 0; worst <- NULL
for (it in 1:6) {
  p <- rand_profile()
  k <- kit_predict_ci(p, kit, tvec = tvec, B = 300)
  g <- predict_profile_ci_mvn_pooled(p, tvec = tvec, B = 300, fits = ms_fits, cl = 0.95)
  d <- max(abs(as.matrix(k[,cols]) - as.matrix(g[,cols])))
  if (d > max_ci) { max_ci <- d; worst <- list(kit=k, eng=g) }
}
cat(sprintf("CI     max|kit - engine| on CI bounds over 6 profiles (B=300) = %.3e\n", max_ci))
cat("       (same seed/draws + reconstructed stratum-centred z => expect ~1e-6)\n")

cat("\nWorst-case profile, side-by-side CI bounds (kit | engine):\n")
w <- worst
for (i in seq_len(nrow(w$kit))) {
  cat(sprintf("  %2.0f mo  dead: [%.4f,%.4f] | [%.4f,%.4f]   readm: [%.4f,%.4f] | [%.4f,%.4f]\n",
    w$kit$month[i], w$kit$dead_lo[i], w$kit$dead_hi[i], w$eng$dead_lo[i], w$eng$dead_hi[i],
    w$kit$readm_lo[i], w$kit$readm_hi[i], w$eng$readm_lo[i], w$eng$readm_hi[i]))
}

stopifnot(max_point < 1e-9, max_ci < 1e-4)
cat("\nVALIDATION OK: point and CI both reproduce the internal engine to numerical precision.\n")
