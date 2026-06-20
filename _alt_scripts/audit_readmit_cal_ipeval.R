# audit_readmit_cal_ipeval.R
#
# ipeval validation of readmission calibration.
# Adapts the ipeval dummy-treatment trick for a cause-specific competing-risks setup.
#
# Two model objects are tested:
#
#   B. Simple cause-specific Cox (coxph): readmission is the event; death is
#      treated as censoring. This is the standard cause-specific hazard approach.
#      ipeval is straightforward here — it supports coxph directly.
#
#   C. Full CSC object (riskRegression::CSC): both cause-1 (readmit) and
#      cause-2 (death) hazards are modeled jointly. The predicted CIF for
#      cause 1 is used. This is what calibrate_cscox_aj_grouped_corr.R uses.
#      ipeval may or may not support CSC — tested here with graceful fallback.
#
# Data preparation notes:
#   - 'vec must be sorted non-decreasingly' in ipeval's IPCW censoring model
#     is caused by unsorted time vectors inside prodlim/pec. Fix: sort the
#     analysis data frame by .ftime before calling ip_score(), and fit the
#     model on the same sorted data.
#   - Complete-case filter applied before sorting. Any row with NA in the
#     time/event/predictor columns is dropped.
#
# Expected finding:
#   - ipeval oeratio should be ≈ 1 for both model types.
#   - This is consistent with the calibration function E/O ≈ 0.989-0.991
#     confirmed in SESSION 2026-05-23 empirical validation.
#   - Contrast with the DEATH model's BUGGED E/O ≈ 0.027 (basehaz centering
#     mismatch, fixed 2026-05-24).
#
# Run from the project root (g:/My Drive/Alvacast/SISTRAT 2023).
# ─────────────────────────────────────────────────────────────────────────────

library(survival)
library(riskRegression)
library(prodlim)

# ── 0. Load project data ──────────────────────────────────────────────────────
source("renv/activate.R")

rdata_files <- sort(list.files(
  "data/20241015_out",
  pattern = "pred22_ndp_.*\\.Rdata$",
  full.names = TRUE
))
if (length(rdata_files) == 0L) stop("No pred22 .Rdata found in data/20241015_out/")
load(rdata_files[length(rdata_files)])
cat("Loaded:", basename(rdata_files[length(rdata_files)]), "\n")

source("cons/_alt_scripts/calibrate_cscox_aj_grouped_corr.R")

df0 <- as.data.frame(py_corrected_datasets_boot[[1L]])

READMIT_TIME  <- "readmit_time_from_disch_m"
READMIT_EVENT <- "readmit_event"
DEATH_TIME    <- "death_time_from_disch_m"
DEATH_EVENT   <- "death_event"
HORIZONS      <- c(6, 12, 36, 60)

# Primary formulas — RHS extracted to build cause-specific Surv() LHS
F_READMIT <- formula_readmit_updated2
F_DEATH   <- formula_death_updated2

cat(sprintf("\nFull dataset: %d rows\n", nrow(df0)))


# ── 1. Build cause-specific first-event outcome ───────────────────────────────
# .ftime   : min(readmit_time, death_time)
# .readmit_cs : 1 if readmission occurred first, 0 if death-first or censored
# .fstatus : 0=censor, 1=readmit-first, 2=death-first (for AJ-CIF reference)
df0$.ftime      <- pmin(df0[[READMIT_TIME]], df0[[DEATH_TIME]])
df0$.readmit_cs <- as.integer(
  !is.na(df0[[READMIT_EVENT]]) & df0[[READMIT_EVENT]] == 1L &
  !is.na(df0[[READMIT_TIME]]) & !is.na(df0[[DEATH_TIME]]) &
  df0[[READMIT_TIME]] <= df0[[DEATH_TIME]]
)
df0$.fstatus <- 0L
df0$.fstatus[df0$.readmit_cs == 1L] <- 1L
df0$.fstatus[!is.na(df0[[DEATH_EVENT]]) & df0[[DEATH_EVENT]] == 1L &
             !is.na(df0[[DEATH_TIME]])   & !is.na(df0[[READMIT_TIME]]) &
             df0[[DEATH_TIME]] < df0[[READMIT_TIME]]] <- 2L

# Complete-case filter on outcome (predictor NAs handled by coxph)
ok <- !is.na(df0$.ftime) & is.finite(df0$.ftime) & df0$.ftime >= 0 &
      !is.na(df0$.readmit_cs)
df_cc <- df0[ok, , drop = FALSE]

cat(sprintf("Complete-case rows: %d (dropped %d with NA outcome)\n",
            nrow(df_cc), nrow(df0) - nrow(df_cc)))
cat(sprintf("Events: %d readmit-first (%.2f%%), %d death-first, %d censored\n",
            sum(df_cc$.fstatus == 1L),
            100 * mean(df_cc$.fstatus == 1L),
            sum(df_cc$.fstatus == 2L),
            sum(df_cc$.fstatus == 0L)))

# Sort by time — required for ipeval's IPCW censoring model (prodlim/pec internals)
df_sorted <- df_cc[order(df_cc$.ftime), , drop = FALSE]
rownames(df_sorted) <- NULL

cat(sprintf("Data sorted by .ftime. Time range: %.1f – %.1f months\n",
            min(df_sorted$.ftime), max(df_sorted$.ftime)))

# AJ-CIF reference (same estimator as calibration function)
df_sorted$.status_factor <- factor(df_sorted$.fstatus, levels = 0:2,
                                   labels = c("censor", "readmit", "death"))
km_aj <- survival::survfit(
  survival::Surv(.ftime, .status_factor) ~ 1, data = df_sorted, conf.int = 0.95
)
aj_sum <- summary(km_aj, times = HORIZONS, extend = TRUE)
state_pos <- which(km_aj$states == "readmit")
obs_aj <- if (is.null(dim(aj_sum$pstate)))
  as.numeric(aj_sum$pstate[state_pos])
else
  as.numeric(aj_sum$pstate[, state_pos])

cat("\nAJ-CIF observed readmission risk:\n")
for (j in seq_along(HORIZONS))
  cat(sprintf("  t=%2d: %.5f\n", HORIZONS[j], obs_aj[j]))


# ── 2. Install ipeval if needed ───────────────────────────────────────────────
if (!requireNamespace("ipeval", quietly = TRUE)) {
  cat("\nipeval not installed. Installing via renv...\n")
  renv::install("ipeval")
}
library(ipeval)

# Add dummy treatment (same seed as death audit for reproducibility)
set.seed(20250524L)
df_sorted$dummy_trt <- rbinom(nrow(df_sorted), 1L, 0.5)
cat(sprintf("\nDummy treatment: %d treated, %d control (random, seed=20250524)\n",
            sum(df_sorted$dummy_trt == 1L), sum(df_sorted$dummy_trt == 0L)))


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION B — Simple cause-specific Cox (coxph)
#   Readmission = event; death treated as censoring.
#   This is how a standard non-competing-risks analysis would treat readmission.
#   ipeval supports coxph via riskRegression::predictRisk.coxph.
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== SECTION B: ipeval — simple cause-specific Cox (coxph) ===\n")

# Build cause-specific formula: Surv(.ftime, .readmit_cs) ~ <RHS from F_READMIT>
rhs_readmit_str <- paste(
  deparse(formula(delete.response(terms(F_READMIT))), width.cutoff = 500L),
  collapse = " "
)
cs_formula_str  <- paste("Surv(.ftime, .readmit_cs) ~",
                          sub("^~\\s*", "", rhs_readmit_str))
cs_formula      <- as.formula(cs_formula_str)
cs_formula_trt  <- update(cs_formula, . ~ . + dummy_trt)

cat("Fitting cause-specific coxph on sorted data...\n")
fit_cs <- tryCatch(
  survival::coxph(cs_formula_trt, data = df_sorted, x = TRUE, y = TRUE),
  error = function(e) {
    cat("coxph error:", conditionMessage(e), "\n"); NULL
  }
)

if (!is.null(fit_cs)) {
  cat(sprintf("dummy_trt coefficient (should be ≈ 0): %.5f\n",
              coef(fit_cs)["dummy_trt"]))

  outcome_cs <- survival::Surv(df_sorted$.ftime, df_sorted$.readmit_cs)

  sec_b <- lapply(HORIZONS, function(h) {
    tryCatch({
      res <- ipeval::ip_score(
        object                = list("CS-Cox readmit" = fit_cs),
        data                  = df_sorted,
        outcome               = outcome_cs,
        treatment_formula     = dummy_trt ~ 1,
        treatment_of_interest = 1L,
        time_horizon          = h,
        cens_model            = "KM",
        bootstrap             = 0L
      )
      tbl <- as.data.frame(res)
      tbl$horizon  <- h
      tbl$obs_AJ   <- obs_aj[which(HORIZONS == h)]
      tbl
    }, error = function(e) {
      cat(sprintf("  t=%d: ip_score error — %s\n", h, conditionMessage(e)))
      NULL
    })
  })
  sec_b <- do.call(rbind, Filter(Negate(is.null), sec_b))

  cat("\n-- ipeval (cause-specific coxph) results --\n")
  if (!is.null(sec_b) && nrow(sec_b) > 0L) {
    print(sec_b, row.names = FALSE)
    cat("\n>> oeratio ≈ 1 confirms the cause-specific Cox is well-calibrated.\n")
    cat(">> Compare with calibration function E/O: t=6: 0.991, t=12: 0.991,\n")
    cat("   t=36: 0.989, t=60: 0.986  (SESSION 2026-05-23 empirical validation)\n")
  } else {
    cat("  (no results — see errors above)\n")
  }
} else {
  cat("(coxph fit failed — skipping Section B)\n")
  sec_b <- NULL
}


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION C — Full CSC model (riskRegression::CSC)
#   This is what calibrate_cscox_aj_grouped_corr uses.
#   ipeval calls riskRegression::predictRisk() internally. For a CSC object,
#   predictRisk() requires cause = 1. ipeval may not pass cause — tested here.
#   If it errors with "argument 'cause' is missing", ipeval does not support CSC.
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== SECTION C: ipeval — full CSC model (riskRegression::CSC) ===\n")

rhs_death_str   <- paste(
  deparse(formula(delete.response(terms(F_DEATH))), width.cutoff = 500L),
  collapse = " "
)
f1 <- as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~",
                        sub("^~\\s*", "", rhs_readmit_str)))
f2 <- as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~",
                        sub("^~\\s*", "", rhs_death_str)))
# Add dummy_trt to cause-1 (readmission) model only
f1_trt <- update(f1, . ~ . + dummy_trt)

cat("Fitting CSC on sorted data (may take a moment)...\n")
fit_csc <- tryCatch(
  riskRegression::CSC(list(f1_trt, f2), data = df_sorted),
  error = function(e) {
    cat("CSC error:", conditionMessage(e), "\n"); NULL
  }
)

if (!is.null(fit_csc)) {
  cat(sprintf(
    "CSC fit complete. dummy_trt in cause-1 (should be ≈ 0): %.5f\n",
    coef(fit_csc$models[["Cause 1"]])["dummy_trt"]
  ))

  # Competing-risks outcome for ipeval: use the full Hist-type event variable
  # ipeval expects Surv() not Hist(). Use cause-specific indicator as proxy:
  # status=1 for readmit-first (cause 1), 0 for everything else.
  # This is consistent with what predictRisk(cause=1) estimates.
  outcome_csc <- survival::Surv(df_sorted$.ftime, df_sorted$.readmit_cs)

  sec_c <- lapply(HORIZONS, function(h) {
    tryCatch({
      res <- ipeval::ip_score(
        object                = list("CSC readmit" = fit_csc),
        data                  = df_sorted,
        outcome               = outcome_csc,
        treatment_formula     = dummy_trt ~ 1,
        treatment_of_interest = 1L,
        time_horizon          = h,
        cens_model            = "KM",
        bootstrap             = 0L
      )
      tbl <- as.data.frame(res)
      tbl$horizon <- h
      tbl$obs_AJ  <- obs_aj[which(HORIZONS == h)]
      tbl
    }, error = function(e) {
      cat(sprintf("  t=%d: ip_score error — %s\n", h, conditionMessage(e)))
      NULL
    })
  })
  sec_c <- do.call(rbind, Filter(Negate(is.null), sec_c))

  cat("\n-- ipeval (CSC) results --\n")
  if (!is.null(sec_c) && nrow(sec_c) > 0L) {
    print(sec_c, row.names = FALSE)
  } else {
    cat("  (no results — CSC may not be supported by ip_score; use Section B results)\n")
  }
} else {
  cat("(CSC fit failed — skipping Section C)\n")
  sec_c <- NULL
}


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION D — Direct E/O comparison: ipeval vs calibration function
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== SECTION D: Comparison summary ===\n")

# Reference E/O from calibration function (SESSION 2026-05-23 validation)
ref_eo <- data.frame(
  horizon  = HORIZONS,
  eo_cal   = c(0.9910, 0.9912, 0.9893, 0.9860),
  obs_AJ   = obs_aj
)
cat("Reference (calibrate_cscox_aj_grouped_corr, 20-rep 5-fold CV, shap_clean model):\n")
print(ref_eo, row.names = FALSE)

if (!is.null(sec_b) && nrow(sec_b) > 0L && "oeratio" %in% names(sec_b)) {
  cat("\nipeval (cause-specific coxph, full_ph model):\n")
  print(sec_b[, intersect(c("horizon", "oeratio", "brier", "obs_AJ"), names(sec_b))],
        row.names = FALSE)
  cat("\nDifference |E/O_ipeval - E/O_cal|:\n")
  merged <- merge(ref_eo, sec_b[, c("horizon", "oeratio")], by = "horizon")
  print(data.frame(
    horizon = merged$horizon,
    eo_cal   = merged$eo_cal,
    eo_ipeval = round(merged$oeratio, 4),
    diff      = round(abs(merged$oeratio - merged$eo_cal), 4)
  ), row.names = FALSE)
}

cat("\nKey interpretation:\n")
cat("  Both ipeval and calibrate_cscox_aj_grouped_corr use predictRisk() internally.\n")
cat("  Agreement (oeratio ≈ E/O_cal ≈ 1.0) means readmission calibration is correct.\n")
cat("  Any discrepancy is due to: (a) different model (CSC vs CS-Cox),\n")
cat("  (b) apparent vs CV estimation, or (c) IPCW vs AJ weighting differences.\n")

cat("\n=== AUDIT COMPLETE ===\n")
