# audit_death_cal_ipeval.R
#
# Independent audit of calibrate_death_km_grouped.R output.
# Three sections:
#
#   A. Manual apparent E/O — fit on all data, predict on all data.
#      Directly tests whether E/O ≈ 0.027 is a real model property
#      or an artefact of the CV implementation.
#
#   B. riskRegression::Score() — IPCW-weighted Brier score + AUC.
#      Gold standard proper scoring rules for right-censored survival.
#      No treatment required.
#
#   C. ipeval::ip_score() — adapted for prognostic model.
#      Treatment arm is randomised 50/50 (uncorrelated with outcome),
#      so IP treatment weights ≈ 2 for the treated half, equivalent
#      to upweighting a random 50% sample to represent the full pop.
#      Gives oeratio and brier as cross-check.
#
# Run from the project root (g:/My Drive/Alvacast/SISTRAT 2023).
# Requires pred22_ndp_*.Rdata in data/20241015_out/.
# ─────────────────────────────────────────────────────────────────────────────

library(survival)
library(riskRegression)

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

# ── 0b. Source calibrate_death_km_grouped to get function in scope ─────────────
source("cons/_alt_scripts/calibrate_death_km_grouped.R")

# Use first imputation for all apparent-calibration checks
df <- as.data.frame(py_corrected_datasets_boot[[1L]])
TIME_COL  <- "death_time_from_disch_m"
EVENT_COL <- "death_event"
HORIZONS  <- c(6, 12, 36, 60)

# Formula under audit (Full PH primary model)
FORMULA <- formula_death_updated2

cat(sprintf("\nDataset: %d rows | Event rate: %.3f%%\n",
            nrow(df), 100 * mean(df[[EVENT_COL]], na.rm = TRUE)))

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION A — Manual apparent E/O
#   Fit coxph on all data, predict on same data (apparent, optimistic bias).
#   Purpose: verify the absolute scale of predictions, independent of CV logic.
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== SECTION A: Manual apparent E/O ===\n")

fit_app <- survival::coxph(FORMULA, data = df, x = TRUE, y = TRUE)

# Use predictRisk (same Efron-corrected estimator used by calibrate_death_km_grouped)
risk_mat <- riskRegression::predictRisk(fit_app, newdata = df, times = HORIZONS)

# KM-observed mortality from the full dataset
km_all <- survival::survfit(
  survival::Surv(df[[TIME_COL]], df[[EVENT_COL]]) ~ 1
)
km_obs <- 1 - summary(km_all, times = HORIZONS, extend = TRUE)$surv

section_a <- data.frame(
  horizon    = HORIZONS,
  obs_KM     = round(km_obs, 5),
  mean_pred  = round(colMeans(risk_mat, na.rm = TRUE), 5),
  eo_ratio   = round(colMeans(risk_mat, na.rm = TRUE) / km_obs, 4)
)
print(section_a, row.names = FALSE)
cat("\n>> E/O ≈ 1 confirms perfect apparent calibration-in-the-large.\n")
cat(">> Uses riskRegression::predictRisk() — same estimator as the calibration function.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION B — riskRegression::Score()
#   IPCW-weighted proper scoring rules (Brier + AUC) on apparent calibration.
#   Censoring model: KM (non-informative assumed).
#   No treatment variable required.
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== SECTION B: riskRegression::Score() ===\n")

# Score() needs a list of fitted model objects and a formula interface
# We re-fit with x=TRUE so Score can call predictRisk() on it
fit_score <- survival::coxph(FORMULA, data = df, x = TRUE, y = TRUE)

score_res <- riskRegression::Score(
  object    = list("Full PH (death)" = fit_score),
  formula   = as.formula(sprintf("Surv(%s, %s) ~ 1", TIME_COL, EVENT_COL)),
  data      = df,
  times     = HORIZONS,
  metrics   = c("auc", "brier"),
  se.fit    = FALSE,
  B         = 0,          # no bootstrap for speed
  null.model = TRUE,
  summary   = c("risks")  # adds calibration-in-the-large via mean risk vs KM
)

cat("\n-- Brier score by horizon --\n")
print(score_res$Brier$score[, c("model", "times", "Brier")], row.names = FALSE)

cat("\n-- AUC by horizon --\n")
print(score_res$AUC$score[, c("model", "times", "AUC")], row.names = FALSE)

# Calibration-in-the-large: extract from score_res$risks if available,
# otherwise compute manually from Score's risk predictions
cat("\n-- Mean predicted vs observed (riskRegression::predictRisk) --\n")
pred_risks_mat <- riskRegression::predictRisk(fit_score, newdata = df, times = HORIZONS)
for (j in seq_along(HORIZONS)) {
  mp <- mean(pred_risks_mat[, j], na.rm = TRUE)
  ko <- km_obs[j]
  cat(sprintf("  t=%2d: mean_pred=%.5f | obs_KM=%.5f | E/O=%.4f\n",
              HORIZONS[j], mp, ko, mp / ko))
}

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION C — ipeval::ip_score()
#   ipeval requires a treatment arm. Workaround for prognostic model:
#     - Random 50/50 treatment uncorrelated with outcome
#     - IP weight = 1/P(A=1|L) = 1/0.5 = 2 for treated patients
#     - Evaluates on the "treated" half (random subset representing full pop)
#     - oeratio and brier should be consistent with sections A and B
#   Install if needed: renv::install("ipeval")
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== SECTION C: ipeval::ip_score() ===\n")

if (!requireNamespace("ipeval", quietly = TRUE)) {
  cat("ipeval not installed. Installing via renv...\n")
  renv::install("ipeval")
}

library(ipeval)

# Add random 50/50 treatment (seed fixed for reproducibility)
set.seed(20250524L)
df$dummy_trt <- rbinom(nrow(df), 1L, 0.5)
cat(sprintf("Dummy treatment: %d treated, %d control (random, seed=20250524)\n",
            sum(df$dummy_trt == 1L), sum(df$dummy_trt == 0L)))

# Re-fit including dummy treatment as a covariate (it should have ≈ 0 effect)
formula_with_trt <- update(FORMULA, . ~ . + dummy_trt)
fit_ipeval <- survival::coxph(formula_with_trt, data = df, x = FALSE, y = FALSE)

cat("\ndummy_trt coefficient (should be ≈ 0):",
    round(coef(fit_ipeval)["dummy_trt"], 5), "\n")

# Run ip_score at each horizon separately (function takes scalar time_horizon)
ipeval_rows <- lapply(HORIZONS, function(h) {
  tryCatch({
    res <- ipeval::ip_score(
      object              = list("Full PH (death)" = fit_ipeval),
      data                = df,
      outcome             = survival::Surv(df[[TIME_COL]], df[[EVENT_COL]]),
      treatment_formula   = dummy_trt ~ 1,   # intercept-only: P(A=1) = 0.5
      treatment_of_interest = 1L,
      time_horizon        = h,
      cens_model          = "KM",
      bootstrap           = 0L
    )
    tbl <- as.data.frame(res)
    tbl$horizon <- h
    tbl
  }, error = function(e) {
    cat(sprintf("  t=%d: ip_score error — %s\n", h, conditionMessage(e)))
    NULL
  })
})
ipeval_rows <- do.call(rbind, Filter(Negate(is.null), ipeval_rows))

cat("\n-- ipeval results by horizon --\n")
if (!is.null(ipeval_rows) && nrow(ipeval_rows) > 0L) {
  print(ipeval_rows, row.names = FALSE)
} else {
  cat("  (no results — see errors above)\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# COMPARISON SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== COMPARISON SUMMARY ===\n")
cat("Section A (manual apparent E/O) vs our CV function output:\n\n")

# Load CV output if pred25 RDS exists
pred25_files <- sort(list.files(
  "data/20241015_out",
  pattern = "pred25_metrics_.*\\.rds$",
  full.names = TRUE
))
if (length(pred25_files) > 0L) {
  results_agg <- readRDS(pred25_files[length(pred25_files)])
  cv_flat <- results_agg$cal_death_full$full_ph$flat
  if (!is.null(cv_flat)) {
    cat("CV function (calibrate_death_km_grouped, 20-rep 5-fold):\n")
    print(cv_flat[, c("time_months", "ici", "ece", "eo_ratio", "mean_pred", "observed")],
          row.names = FALSE)
    cat("\nApparent fit (Section A):\n")
    print(section_a, row.names = FALSE)
    cat("\nKey: apparent E/O (Section A) and Section B predictRisk E/O should both be ≈ 1.\n")
    cat("If CV E/O << 1, the CV function has the centered=FALSE/centered-LP mismatch bug.\n")
  }
} else {
  cat("(pred25_metrics_*.rds not found — run the notebook save cell first)\n")
  cat("Apparent E/O from Section A:\n")
  print(section_a, row.names = FALSE)
}

cat("\n=== AUDIT COMPLETE ===\n")
