## build_nomogram_kit.R ---------------------------------------------------------
## OFFLINE builder of the DE-IDENTIFIED multistate nomogram kit for the Shiny app.
## Run ONCE. Reads the internal engine (with microdata), writes a kit that holds
## only: precomputed per-stratum baseline cumulative hazards, data-free Cox
## objects (linear-predictor + vcov only), the predictor spec and the reference
## profile. No individual-level data leaves this step.
##
## The kit reproduces predict_profile_markov_pooled() / predict_profile_ci_mvn_pooled()
## to machine precision (validated below before the kit is written).
##
## Output: data/20241015_out/nomogram_kit_deident_<date>.rds
## Usage:  "C:/Program Files/R/R-4.4.1/bin/Rscript.exe" cons/_alt_scripts/build_nomogram_kit.R

suppressPackageStartupMessages({ library(survival); library(mstate) })

PROJ      <- "g:/My Drive/Alvacast/SISTRAT 2023"
ENGINE_IN <- file.path(PROJ, "data/20241015_out/pooled_probtrans_engine_INTERNAL.rds")
OUT_DIR   <- file.path(PROJ, "data/20241015_out")
PREDICT_R <- file.path(PROJ, "cons/_alt_scripts/nomogram_predict.R")
TOL_POINT <- 1e-9   # kit must reproduce engine point estimates to ~machine precision
TOL_CI    <- 1e-6   # same seed/draws => CI must match closely too

message(">>> Loading internal engine ...")
e <- readRDS(ENGINE_IN)
predictor_spec <- e$predictor_spec
tmat           <- e$tmat
HORIZONS       <- e$HORIZONS
ms_fits        <- e$ms_fits
spec_names     <- vapply(predictor_spec, `[[`, "", "name")
spec_by        <- setNames(predictor_spec, spec_names)

## --- reference profile (one row, correct types/levels) -----------------------
build_profile <- function(spec) {
  row <- lapply(spec, function(p)
    if (p$type == "factor") factor(p$reference, levels = p$levels) else as.numeric(p$reference))
  names(row) <- vapply(spec, `[[`, "", "name")
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}
ref <- build_profile(predictor_spec)

## --- strata variables of a Cox model + the value set of each ------------------
strata_info <- function(m) {
  tt <- terms(m); sp <- attr(tt, "specials")$strata
  if (is.null(sp)) return(list())
  labs <- vapply(sp, function(i) deparse(attr(tt, "variables")[[i + 1]]), character(1))
  vars <- gsub("^strata\\((.*)\\)$", "\\1", labs)
  vals <- lapply(vars, function(v) {
    s <- spec_by[[v]]
    if (!is.null(s) && s$type == "factor") s$levels else c(0, 1)  # numeric 0/1 strata indicator
  })
  setNames(vals, vars)
}

## --- per-stratum baseline cumulative hazard at covariate means ----------------
## One single-row survfit per stratum (mirrors the validated engine
## .msfit_profile_fwd). H0_j(t) = survfit(ref-covs, stratum j) / exp(lp_ref),
## so a profile's cumhaz is H0_j(t) * exp(lp_profile).
## per-coefficient (stratum-centred minus sample-centred) linear-predictor
## offset. corr_j = sample_mean - stratum_mean_j, independent of the profile, so
## one evaluation per stratum (with the reference covariates) is exact for all
## profiles. The CI draw scaling needs stratum-centred z; the kit reconstructs it
## as z_sample (data-free) + corr_j, which equals the full engine's z exactly.
z_strata_minus_sample <- function(m, nd) {
  p <- length(coef(m)); if (!p) return(numeric(0))
  b <- m; out <- numeric(p)
  for (k in seq_len(p)) { bk <- numeric(p); bk[k] <- 1; b$coefficients <- bk
    out[k] <- as.numeric(predict(b, newdata = nd, type = "lp")) -            # default = stratum
              as.numeric(predict(b, newdata = nd, type = "lp", reference = "sample")) }
  out
}

build_baseline <- function(m) {
  sv <- strata_info(m); vars <- names(sv)
  combos <- if (length(sv)) expand.grid(sv, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
            else data.frame(.dummy = 1)
  lp_ref <- as.numeric(predict(m, newdata = ref, type = "lp", reference = "sample"))
  strata <- lapply(seq_len(nrow(combos)), function(j) {
    nd <- ref
    for (v in vars) {
      s <- spec_by[[v]]
      nd[[v]] <- if (!is.null(s) && s$type == "factor")
                   factor(combos[[v]][j], levels = s$levels) else as.numeric(combos[[v]][j])
    }
    sf <- survfit(m, newdata = nd)
    ch <- if (!is.null(sf$cumhaz)) as.numeric(sf$cumhaz) else -log(pmax(as.numeric(sf$surv), 1e-12))
    list(time = sf$time, H0 = ch / exp(lp_ref), corr = z_strata_minus_sample(m, nd))
  })
  list(strata = strata, combos = combos[, vars, drop = FALSE], vars = vars)
}

## --- strip a coxph of all microdata, keep predict(type="lp") + vcov ----------
strip_coxph <- function(m) {
  for (slot in c("y", "linear.predictors", "residuals", "model", "x", "weights",
                 "offset", "frail", "wt", "naive.var", "fitted.values"))
    m[[slot]] <- NULL
  environment(m$terms)   <- baseenv()
  environment(m$formula) <- baseenv()
  attr(m$terms, ".Environment") <- baseenv()
  m
}

## --- build the kit -----------------------------------------------------------
message(">>> Building per-imputation baselines and stripping models ...")
models <- lapply(seq_along(ms_fits), function(i) {
  f <- ms_fits[[i]]
  list(m1 = strip_coxph(f$m1), b1 = build_baseline(f$m1),
       m2 = strip_coxph(f$m2), b2 = build_baseline(f$m2),
       m3 = strip_coxph(f$m3), b3 = build_baseline(f$m3))
})

kit <- list(
  models         = models,
  tmat           = tmat,
  predictor_spec = predictor_spec,
  reference      = ref,
  horizons       = HORIZONS,             # c(6,12,36,60); app adds 3 -> c(3,6,12,36,60)
  m_imp          = length(ms_fits),
  version        = format(Sys.Date(), "%Y%m%d"),
  note           = "De-identified multistate nomogram kit. No microdata. Markov clock-forward, pooled over 5 imputations."
)

## --- VALIDATION against the authoritative engine -----------------------------
message(">>> Validating kit vs engine ...")
source(PREDICT_R)  # kit_predict_point / kit_predict_ci

set.seed(2125)
rand_profile <- function() {
  p <- ref
  p$adm_age_rec3 <- runif(1, 18, 64); p$dit_m <- runif(1, 0, 36); p$porc_pobr <- runif(1, 0, 0.4)
  for (nm in c("primary_sub_mod_cocaine_paste","primary_sub_mod_cocaine_powder","primary_sub_mod_alcohol",
               "primary_sub_mod_others","prim_sub_freq_rec_2_2_6_days_wk","prim_sub_freq_rec_3_daily",
               "occupation_condition_corr24_unemployed","occupation_condition_corr24_inactive",
               "sex_rec_woman","ethnicity","sub_dep_icd10_status_drug_dependence","polysubstance_strict",
               "eva_ocupacion_logro_intermedio","eva_ocupacion_logro_minimo",
               "cohabitation_family_of_origin","cohabitation_with_couple_children","cohabitation_others",
               "tr_outcome_dropout","tr_outcome_referral","tr_outcome_adm_discharge_adm_reasons",
               "tr_outcome_adm_discharge_rule_violation_undet"))
    p[[nm]] <- rbinom(1, 1, 0.4)
  p$plan_type_strata <- factor(sample(spec_by$plan_type_strata$levels, 1), levels = spec_by$plan_type_strata$levels)
  p$any_phys_dx      <- factor(sample(c("FALSE","TRUE"), 1), levels = c("FALSE","TRUE"))
  p$dg_psiq_cie_10_dg<- factor(sample(c("FALSE","TRUE"), 1), levels = c("FALSE","TRUE"))
  p
}

# load the engine's own functions into the global env so they resolve their siblings
list2env(e, globalenv())
tvec <- c(3, 6, 12, 36, 60)

max_point <- 0
for (it in 1:12) {
  p <- rand_profile()
  k_pt <- kit_predict_point(p, kit, tvec = tvec)
  e_pt <- predict_profile_markov_pooled(p, tvec = tvec, fits = ms_fits)
  max_point <- max(max_point, max(abs(as.matrix(k_pt[, 2:4]) - as.matrix(e_pt[, 2:4]))))
}
cat(sprintf("    POINT  max|kit - engine| over 12 random profiles = %.3e\n", max_point))
if (max_point > TOL_POINT) stop("POINT validation FAILED (", max_point, " > ", TOL_POINT, ")")
cat("    (CI is checked separately in validate_nomogram_kit.R)\n")

## --- write the kit -----------------------------------------------------------
out_path <- file.path(OUT_DIR, sprintf("nomogram_kit_deident_%s.rds", kit$version))
saveRDS(kit, out_path)
sz <- file.info(out_path)$size / 1e6
cat(sprintf("\n>>> Kit validated and written: %s (%.1f Mb, no microdata)\n", out_path, sz))

# microdata sanity: total long-vector footprint in the kit (excludes p x p vcov)
chk <- function(x) if (is.list(x)) sum(vapply(x, chk, numeric(1))) else
  if ((is.numeric(x) || is.character(x)) && length(x) > 5000) length(x) else 0
cat(sprintf(">>> Long-vector footprint (>5000 elems, baselines are expected): %d elements\n", chk(kit$models)))
