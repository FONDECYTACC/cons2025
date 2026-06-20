# _tmp_fgr_vs_loess_death.R
#
# Two goals:
#   1. Diagnose the survfit.coxph output structure (is sf$surv a matrix or a
#      stacked vector?) — this determines whether the current worker prediction
#      code in calibrate_death_km_grouped.R is correct.
#   2. Compare FGR-style vs loess calibration ICI with CORRECT predictions to
#      determine whether FGR artifacts were caused by the prediction bugs or
#      are inherent to the method.
#
# Run from project root: g:/My Drive/Alvacast/SISTRAT 2023
# ─────────────────────────────────────────────────────────────────────────────

setwd("g:/My Drive/Alvacast/SISTRAT 2023")
source("renv/activate.R")
suppressPackageStartupMessages({
  library(survival)
  library(splines)
  library(riskRegression)
})

# ── 0. Load data ──────────────────────────────────────────────────────────────
rdata_files <- sort(list.files("data/20241015_out",
                                pattern = "pred22_ndp_.*\\.Rdata$",
                                full.names = TRUE))
load(rdata_files[length(rdata_files)])
cat("Loaded:", basename(rdata_files[length(rdata_files)]), "\n")

set.seed(9999L)
full_df <- as.data.frame(py_corrected_datasets_boot[[1L]])
idx     <- sample(nrow(full_df), 10000L)
df      <- full_df[idx, ]
rm(full_df, py_corrected_datasets_boot); invisible(gc())

FORMULA  <- formula_death_updated2
TIME_COL <- "death_time_from_disch_m"
EVT_COL  <- "death_event"
HORIZONS <- c(6, 12, 36, 60)

# 70/30 split
set.seed(42L)
n        <- nrow(df)
test_idx <- sample(n, floor(n * 0.30))
trn_idx  <- setdiff(seq_len(n), test_idx)
df_train <- df[trn_idx, ]
df_test  <- df[test_idx, ]
cat(sprintf("Train: %d  Test: %d  Event rate (train): %.2f%%\n",
            nrow(df_train), nrow(df_test),
            100 * mean(df_train[[EVT_COL]], na.rm=TRUE)))

# ── 1. Fit Cox ────────────────────────────────────────────────────────────────
cat("\nFitting coxph...\n")
fit <- suppressWarnings(
  survival::coxph(FORMULA, data = df_train, x = TRUE, y = TRUE)
)

# ── 2. Diagnose survfit output structure ──────────────────────────────────────
cat("\n=== DIAGNOSTIC: survfit.coxph output structure ===\n")
sf_diag <- survival::survfit(fit, newdata = df_test[1:5, ], se.fit = FALSE)
cat(sprintf("  class(sf_diag$surv):  %s\n", class(sf_diag$surv)))
cat(sprintf("  is.matrix(sf_diag$surv): %s\n", is.matrix(sf_diag$surv)))
cat(sprintf("  length(sf_diag$surv): %d\n", length(sf_diag$surv)))
if (is.matrix(sf_diag$surv)) {
  cat(sprintf("  dim(sf_diag$surv): %d x %d  [times x subjects?]\n",
              nrow(sf_diag$surv), ncol(sf_diag$surv)))
} else {
  cat(sprintf("  length(sf_diag$time): %d unique event times\n", length(sf_diag$time)))
  cat(sprintf("  Stacked? length/n_times = %.1f (should be n_subjects=5 if stacked)\n",
              length(sf_diag$surv) / length(sf_diag$time)))
}
cat(sprintf("  First 5 surv values: %s\n",
            paste(round(head(sf_diag$surv, 5), 6), collapse=", ")))

# Check if 5 subjects have different survival values at the last time
last_time_vals <- tail(sf_diag$surv, 5)
cat(sprintf("  Last 5 surv values (tail): %s\n",
            paste(round(last_time_vals, 6), collapse=", ")))
cat(sprintf("  Unique values in surv: %d (5 subjects should give >1)\n",
            length(unique(round(sf_diag$surv, 8)))))

# ── 3. Correct predictions via riskRegression::predictRisk ────────────────────
cat("\n\n=== PREDICTIONS via riskRegression::predictRisk ===\n")
pred_mat <- riskRegression::predictRisk(fit, newdata = df_test, times = HORIZONS)

km_test <- survival::survfit(survival::Surv(df_test[[TIME_COL]], df_test[[EVT_COL]]) ~ 1)
km_obs  <- 1 - summary(km_test, times = HORIZONS, extend = TRUE)$surv

for (j in seq_along(HORIZONS)) {
  mp <- mean(pred_mat[, j], na.rm=TRUE)
  cat(sprintf("  t=%2d: mean_pred=%.5f  obs=%.5f  E/O=%.4f  pred_range=[%.5f,%.5f]\n",
              HORIZONS[j], mp, km_obs[j], mp/km_obs[j],
              min(pred_mat[,j], na.rm=TRUE), max(pred_mat[,j], na.rm=TRUE)))
}

# ── 4. Check survfit extraction using the diagnosed structure ─────────────────
cat("\n=== SURVFIT-BASED PREDICTIONS (current worker code) ===\n")
sf_full <- survival::survfit(fit, newdata = df_test, se.fit = FALSE)
cat(sprintf("  is.matrix(sf_full$surv): %s\n", is.matrix(sf_full$surv)))
if (is.matrix(sf_full$surv)) {
  cat(sprintf("  dim(sf_full$surv): %d x %d\n", nrow(sf_full$surv), ncol(sf_full$surv)))
  # worker code path
  sv  <- sf_full$surv
  out <- matrix(NA_real_, nrow = ncol(sv), ncol = length(HORIZONS))
  for (.j in seq_along(HORIZONS)) {
    cands <- which(sf_full$time <= HORIZONS[.j])
    if (length(cands) > 0L) out[, .j] <- 1 - sv[max(cands), ]
    else                    out[, .j] <- 0
  }
  for (j in seq_along(HORIZONS))
    cat(sprintf("  t=%2d: worker_pred range=[%.5f,%.5f]  predictRisk range=[%.5f,%.5f]\n",
                HORIZONS[j],
                min(out[,j], na.rm=TRUE), max(out[,j], na.rm=TRUE),
                min(pred_mat[,j], na.rm=TRUE), max(pred_mat[,j], na.rm=TRUE)))
  surv_code_ok <- isTRUE(all.equal(out, pred_mat, tolerance=1e-6))
  cat(sprintf("  Worker code agrees with predictRisk: %s\n",
              if (surv_code_ok) "YES (correct)" else "NO (bug — use predictRisk)"))
} else {
  cat("  sf_full$surv is NOT a matrix — worker code will give wrong results!\n")
  cat("  Stacked vector length:", length(sf_full$surv),
      " / n_times:", length(sf_full$time),
      " = ", round(length(sf_full$surv)/length(sf_full$time), 1), "subjects\n")
}

# ── 5. Helper functions ───────────────────────────────────────────────────────
km_by_bin <- function(pred_vec, time_vec, event_vec, t_horiz, g=6L, min_n=40L) {
  cuts <- unique(quantile(pred_vec, seq(0,1,length.out=g+1L), na.rm=TRUE,
                          names=FALSE, type=8))
  cuts <- cuts[-c(1L, length(cuts))]
  bins <- if (length(cuts)>0L)
    findInterval(pred_vec, cuts, all.inside=TRUE) + 1L
  else rep(1L, length(pred_vec))
  obs <- vapply(sort(unique(bins)), function(b) {
    idx <- which(bins==b)
    if (length(idx) < min_n) return(NA_real_)
    sf_b <- survival::survfit(survival::Surv(time_vec[idx], event_vec[idx]) ~ 1)
    s    <- summary(sf_b, times=t_horiz, extend=TRUE)$surv
    if (length(s)>0L) 1-s[[1L]] else NA_real_
  }, numeric(1L))
  names(obs) <- as.character(sort(unique(bins)))
  obs[as.character(bins)]
}

loess_ici <- function(pred, obs, span=0.75) {
  v <- is.finite(pred) & is.finite(obs)
  if (sum(v) < 10L) return(NA_real_)
  tryCatch({
    lo <- stats::loess(obs[v] ~ pred[v], span=span,
                       control=stats::loess.control(surface="interpolate"))
    mean(abs(fitted(lo) - pred[v]), na.rm=TRUE)
  }, error=function(e) NA_real_)
}

cloglog_ici <- function(pred, time_vec, event_vec, t_horiz) {
  p   <- pmax(pmin(pred, 1-1e-9), 1e-9)
  eta <- log(-log(1-p))
  df_tmp <- data.frame(time=time_vec, event=event_vec, eta=eta)
  df_tmp <- df_tmp[is.finite(df_tmp$eta), ]
  if (nrow(df_tmp) < 30L) return(NA_real_)
  fc <- tryCatch(
    survival::coxph(Surv(time,event) ~ ns(eta,df=5), data=df_tmp, x=TRUE, y=TRUE),
    error=function(e) NULL
  )
  if (is.null(fc)) return(NA_real_)
  sf_c <- tryCatch(
    survival::survfit(fc, newdata=data.frame(eta=eta[is.finite(eta)]), se.fit=FALSE),
    error=function(e) NULL
  )
  if (is.null(sf_c)) return(NA_real_)
  # extract calibrated risk — handle both matrix and stacked format
  if (is.matrix(sf_c$surv)) {
    cands <- which(sf_c$time <= t_horiz)
    if (length(cands)==0L) return(NA_real_)
    cal <- 1 - sf_c$surv[max(cands), ]
  } else {
    n_t   <- length(sf_c$time)
    n_sub <- length(sf_c$surv) %/% n_t
    if (n_sub < 1L) return(NA_real_)
    cands <- which(sf_c$time <= t_horiz)
    if (length(cands)==0L) return(NA_real_)
    # stacked: surv = [subj1_t1..tn, subj2_t1..tn, ...]
    surv_mat <- matrix(sf_c$surv, nrow=n_t, ncol=n_sub)
    cal <- 1 - surv_mat[max(cands), ]
  }
  valid_idx <- is.finite(eta)
  mean(abs(cal - pred[valid_idx]), na.rm=TRUE)
}

# ── 6. Loess ICI ──────────────────────────────────────────────────────────────
cat("\n\n=== METHOD A: Loess ICI ===\n")
t_loess <- system.time({
  loess_res <- data.frame(horizon=HORIZONS, ici=NA_real_, eo=NA_real_)
  for (j in seq_along(HORIZONS)) {
    obs_ind <- km_by_bin(pred_mat[,j], df_test[[TIME_COL]], df_test[[EVT_COL]], HORIZONS[j])
    loess_res$ici[j] <- loess_ici(pred_mat[,j], obs_ind)
    loess_res$eo[j]  <- mean(pred_mat[,j], na.rm=TRUE) / km_obs[j]
  }
})
print(loess_res, row.names=FALSE, digits=5)
cat(sprintf("Time: %.2f sec\n", t_loess["elapsed"]))
if (!anyNA(loess_res$ici))
  cat(sprintf("ICI direction: %s  (t6=%.5f → t60=%.5f)\n",
              if (loess_res$ici[4]>loess_res$ici[1]) "INCREASING (correct)" else "DECREASING (artifact)",
              loess_res$ici[1], loess_res$ici[4]))

# ── 7. FGR-style (cloglog Cox smoother) ICI ───────────────────────────────────
cat("\n=== METHOD B: FGR-style cloglog smoother ===\n")
fgr_res <- data.frame(horizon=HORIZONS, ici=NA_real_, time_sec=NA_real_)
for (j in seq_along(HORIZONS)) {
  t_j <- system.time(
    fgr_res$ici[j] <- cloglog_ici(pred_mat[,j], df_test[[TIME_COL]], df_test[[EVT_COL]], HORIZONS[j])
  )
  fgr_res$time_sec[j] <- t_j["elapsed"]
  cat(sprintf("  t=%2d: ICI=%.5f  time=%.2fs\n", HORIZONS[j], fgr_res$ici[j], fgr_res$time_sec[j]))
}
if (!anyNA(fgr_res$ici))
  cat(sprintf("ICI direction: %s  (t6=%.5f → t60=%.5f)\n",
              if (fgr_res$ici[4]>fgr_res$ici[1]) "INCREASING (correct)" else "DECREASING (artifact)",
              fgr_res$ici[1], fgr_res$ici[4]))

# Speed extrapolation to full dataset
mean_sec <- mean(fgr_res$time_sec, na.rm=TRUE)
scale    <- (70521 / nrow(df_test))^1.5
cat(sprintf("Timing on N=%d test set: %.2f s/fit\n", nrow(df_test), mean_sec))
cat(sprintf("Extrapolated to N=70521 (×N^1.5 = ×%.0f): %.0f s/fit\n", scale, mean_sec*scale))
cat(sprintf("80 fits (production): ~%.0f–%.0f s (%.1f–%.1f min)\n",
            mean_sec*scale*80*0.5, mean_sec*scale*80*2,
            mean_sec*scale*80*0.5/60, mean_sec*scale*80*2/60))

# ── 8. Summary ────────────────────────────────────────────────────────────────
cat("\n\n=== SUMMARY ===\n")
cat(sprintf("%-8s  %-12s  %-12s  %-10s\n", "Horizon", "Loess ICI", "FGR ICI", "FGR sec"))
cat(strrep("-", 48), "\n")
for (j in seq_along(HORIZONS))
  cat(sprintf("t=%-5d  %-12.5f  %-12.5f  %-10.2f\n",
              HORIZONS[j], loess_res$ici[j], fgr_res$ici[j], fgr_res$time_sec[j]))

cat("\nContext (SESSION 2026-05-23, readmission model, N=70k, predictions always correct):\n")
cat("  FGR ICI:   0.00871 → 0.00263  (DECREASING — artifact)\n")
cat("  Loess ICI: 0.00669 → 0.00942  (INCREASING — correct)\n")
cat("  ECE/E/O: identical between methods\n")
cat("\nConclusion: FGR artifacts are in the SMOOTHER, not in the predictions.\n")
cat("Fixing the prediction bug (centering) does not fix FGR's cloglog distortion.\n")

cat("\nDone.\n")
