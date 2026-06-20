setwd("g:/My Drive/Alvacast/SISTRAT 2023")
source("renv/activate.R")
suppressPackageStartupMessages({ library(survival); library(riskRegression) })

rdata_files <- sort(list.files("data/20241015_out", pattern="pred22_ndp_.*\\.Rdata$", full.names=TRUE))
load(rdata_files[length(rdata_files)])
cat("Loaded:", basename(rdata_files[length(rdata_files)]), "\n")

df <- as.data.frame(py_corrected_datasets_boot[[1L]])
rm(py_corrected_datasets_boot); invisible(gc())

TIME_COL  <- "death_time_from_disch_m"
EVENT_COL <- "death_event"
FORMULA   <- formula_death_updated2
HORIZONS  <- c(6, 12, 36, 60)

cat(sprintf("N=%d | event_rate=%.3f%%\n", nrow(df), 100 * mean(df[[EVENT_COL]], na.rm=TRUE)))

# One 80/20 split
set.seed(2126L)
n         <- nrow(df)
test_idx  <- sample(n, floor(n / 5))
train_idx <- setdiff(seq_len(n), test_idx)
cat(sprintf("Train: %d  Test: %d\n", length(train_idx), length(test_idx)))

# Fit on training data
cat("Fitting coxph on training data...\n")
fit <- survival::coxph(FORMULA, data = df[train_idx, ], x = TRUE, y = TRUE)

# KM observed on test fold
km_test <- 1 - summary(
  survfit(Surv(df[test_idx, TIME_COL], df[test_idx, EVENT_COL]) ~ 1),
  times = HORIZONS, extend = TRUE
)$surv

# ---- Method A: riskRegression::predictRisk (gold standard, used in readmit) ----
cat("\n=== Method A: riskRegression::predictRisk() ===\n")
pred_rr <- riskRegression::predictRisk(fit, newdata = df[test_idx, ], times = HORIZONS)

for (j in seq_along(HORIZONS))
  cat(sprintf("  t=%2d: mean_pred=%.6f  obs=%.6f  E/O=%.4f\n",
      HORIZONS[j],
      mean(pred_rr[, j], na.rm = TRUE), km_test[j],
      mean(pred_rr[, j], na.rm = TRUE) / km_test[j]))

# ---- Method B: basehaz(centered=TRUE) + lp + outer (current fix) ----
cat("\n=== Method B: basehaz(centered=TRUE) + predict(lp) + outer ===\n")
lp   <- stats::predict(fit, newdata = df[test_idx, ], type = "lp")
bh_T <- survival::basehaz(fit, centered = TRUE)
bh_T <- bh_T[!duplicated(bh_T$time, fromLast = TRUE), , drop = FALSE]
H_cT <- stats::approx(bh_T$time, bh_T$hazard, xout = HORIZONS,
                       method = "constant", f = 1L, rule = 2L)$y
cat(sprintf("  H_c(t) [centered=TRUE]:  %s\n", paste(formatC(H_cT, format="e", digits=4), collapse=", ")))
cat(sprintf("  lp range: [%.3f, %.3f]  mean=%.4f\n", min(lp), max(lp), mean(lp)))
risk_T <- 1 - exp(-outer(exp(lp), H_cT))

for (j in seq_along(HORIZONS))
  cat(sprintf("  t=%2d: mean_pred=%.6f  obs=%.6f  E/O=%.4f\n",
      HORIZONS[j],
      mean(risk_T[, j], na.rm = TRUE), km_test[j],
      mean(risk_T[, j], na.rm = TRUE) / km_test[j]))

# ---- Method C: basehaz(centered=FALSE) + lp + outer (original bug) ----
cat("\n=== Method C: basehaz(centered=FALSE) + predict(lp) + outer [BUGGED ref] ===\n")
bh_F <- survival::basehaz(fit, centered = FALSE)
bh_F <- bh_F[!duplicated(bh_F$time, fromLast = TRUE), , drop = FALSE]
H_cF <- stats::approx(bh_F$time, bh_F$hazard, xout = HORIZONS,
                       method = "constant", f = 1L, rule = 2L)$y
cat(sprintf("  H_0(t) [centered=FALSE]: %s\n", paste(formatC(H_cF, format="e", digits=4), collapse=", ")))
risk_F <- 1 - exp(-outer(exp(lp), H_cF))

for (j in seq_along(HORIZONS))
  cat(sprintf("  t=%2d: mean_pred=%.6f  obs=%.6f  E/O=%.4f\n",
      HORIZONS[j],
      mean(risk_F[, j], na.rm = TRUE), km_test[j],
      mean(risk_F[, j], na.rm = TRUE) / km_test[j]))

# ---- Centering factor ----
cat("\n=== Centering factor ===\n")
cat(sprintf("  H_c(t) / H_0(t) at t=6: %.4f  -> exp(mean_lp_train) = %.4f\n",
    H_cT[1] / H_cF[1], H_cT[1] / H_cF[1]))
cat(sprintf("  mean_lp_train (from training fit): %.4f\n",
    as.numeric(fit$means %*% coef(fit))))

cat("\n=== KEY QUESTION ===\n")
cat(sprintf("  Method A E/O at t=6: %.4f  (predictRisk — gold standard)\n",
    mean(pred_rr[, 1], na.rm=TRUE) / km_test[1]))
cat(sprintf("  Method B E/O at t=6: %.4f  (basehaz centered=TRUE — current fix)\n",
    mean(risk_T[, 1], na.rm=TRUE) / km_test[1]))
cat("  If A =/= B: still a code bug. If A == B but both << 1: genuine model overfitting.\n")
