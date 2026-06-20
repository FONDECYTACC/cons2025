setwd("g:/My Drive/Alvacast/SISTRAT 2023")
source("renv/activate.R")
suppressPackageStartupMessages({
  library(survival)
  library(riskRegression)
  library(prodlim)
  library(ipeval)
})

# ── Load pred22 Rdata, keep only what we need, free everything else ──────────
rdata_files <- sort(list.files("data/20241015_out",
                                pattern = "pred22_ndp_.*\\.Rdata$",
                                full.names = TRUE))
if (length(rdata_files) == 0L) stop("No pred22 .Rdata found")
load(rdata_files[length(rdata_files)])
cat("Loaded:", basename(rdata_files[length(rdata_files)]), "\n")

# Extract 8 000 rows from first imputation, then free the heavy objects
set.seed(42L)
full_df <- as.data.frame(py_corrected_datasets_boot[[1L]])
idx     <- sample(nrow(full_df), min(8000L, nrow(full_df)))
df0     <- full_df[idx, ]
F_READMIT <- formula_readmit_updated2   # keep formula before rm()

# Free everything large immediately
rm(full_df, py_corrected_datasets_boot)
rm(list = intersect(ls(), c(
  "py_corrected_datasets",
  "results_upd2_py", "results_shap_xgb_py_upd_best_perf",
  "results_shap_xgb_py_upd_best_perf3",
  "dual_pending_results", "ds_dual_results"
)))
invisible(gc())
cat(sprintf("Working sample: %d rows\n", nrow(df0)))

# ── Build cause-specific first-event outcome ─────────────────────────────────
READMIT_TIME  <- "readmit_time_from_disch_m"
READMIT_EVENT <- "readmit_event"
DEATH_TIME    <- "death_time_from_disch_m"
DEATH_EVENT   <- "death_event"
HORIZONS      <- c(6, 12, 36, 60)

df0$.ftime <- pmin(df0[[READMIT_TIME]], df0[[DEATH_TIME]])
df0$.readmit_cs <- as.integer(
  !is.na(df0[[READMIT_EVENT]]) & df0[[READMIT_EVENT]] == 1L &
  !is.na(df0[[READMIT_TIME]])  & !is.na(df0[[DEATH_TIME]])  &
  df0[[READMIT_TIME]] <= df0[[DEATH_TIME]]
)
df0$.fstatus <- 0L
df0$.fstatus[df0$.readmit_cs == 1L] <- 1L
df0$.fstatus[
  !is.na(df0[[DEATH_EVENT]]) & df0[[DEATH_EVENT]] == 1L &
  !is.na(df0[[DEATH_TIME]])  & !is.na(df0[[READMIT_TIME]]) &
  df0[[DEATH_TIME]] < df0[[READMIT_TIME]]
] <- 2L

# Complete cases on outcome
ok <- !is.na(df0$.ftime) & is.finite(df0$.ftime) & df0$.ftime >= 0 &
      !is.na(df0$.readmit_cs)
df_cc <- df0[ok, ]
cat(sprintf("Complete cases: %d | readmit-first: %d (%.1f%%) | death-first: %d | censored: %d\n",
            nrow(df_cc),
            sum(df_cc$.fstatus == 1L), 100 * mean(df_cc$.fstatus == 1L),
            sum(df_cc$.fstatus == 2L),
            sum(df_cc$.fstatus == 0L)))

# Sort by time — required for ipeval's IPCW censoring model
df_sorted <- df_cc[order(df_cc$.ftime), ]
rownames(df_sorted) <- NULL

# ── AJ-CIF observed readmission risk ─────────────────────────────────────────
df_sorted$.sf <- factor(df_sorted$.fstatus, levels = 0:2,
                        labels = c("censor", "readmit", "death"))
km_aj     <- survival::survfit(survival::Surv(.ftime, .sf) ~ 1, data = df_sorted)
aj_sum    <- summary(km_aj, times = HORIZONS, extend = TRUE)
sp        <- which(km_aj$states == "readmit")
obs_aj    <- if (is.null(dim(aj_sum$pstate))) aj_sum$pstate[sp] else aj_sum$pstate[, sp]
cat(sprintf("AJ-CIF readmit at t=6: %.4f | t=12: %.4f | t=36: %.4f | t=60: %.4f\n",
            obs_aj[1], obs_aj[2], obs_aj[3], obs_aj[4]))

# ── Add dummy 50/50 treatment ─────────────────────────────────────────────────
set.seed(20250524L)
df_sorted$dummy_trt <- rbinom(nrow(df_sorted), 1L, 0.5)
cat(sprintf("Dummy treatment: %d treated | %d control (seed=20250524)\n",
            sum(df_sorted$dummy_trt == 1L), sum(df_sorted$dummy_trt == 0L)))

# ── Build cause-specific coxph formula ───────────────────────────────────────
rhs_str <- paste(
  deparse(formula(delete.response(terms(F_READMIT))), width.cutoff = 500L),
  collapse = " "
)
cs_trt_f <- as.formula(
  paste("Surv(.ftime, .readmit_cs) ~",
        sub("^~\\s*", "", rhs_str),
        "+ dummy_trt")
)

cat("\nFitting cause-specific coxph...\n")
fit_cs <- tryCatch(
  survival::coxph(cs_trt_f, data = df_sorted, x = TRUE, y = TRUE),
  error = function(e) { cat("coxph error:", conditionMessage(e), "\n"); NULL }
)
if (is.null(fit_cs)) stop("coxph failed")

cat(sprintf("dummy_trt coefficient (should be ≈ 0): %.5f\n",
            coef(fit_cs)["dummy_trt"]))

# ── Attempt ipeval (documents whether it works or not) ───────────────────────
cat("\n--- ipeval attempt on real data (n=8000 sample, sorted) ---\n")
outcome_cs <- survival::Surv(df_sorted$.ftime, df_sorted$.readmit_cs)

ipeval_rows <- lapply(HORIZONS, function(h) {
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
    tbl <- as.data.frame(res); tbl$horizon <- h; tbl
  }, error = function(e) {
    cat(sprintf("  t=%2d: ip_score error — %s\n", h, conditionMessage(e))); NULL
  })
})
ipeval_rows <- do.call(rbind, Filter(Negate(is.null), ipeval_rows))
if (!is.null(ipeval_rows) && nrow(ipeval_rows) > 0L) print(ipeval_rows, row.names=FALSE)

# ── ipeval diagnostic: toy dataset (tests if ipeval works at all here) ────────
cat("\n--- ipeval diagnostic on 300-row toy dataset ---\n")
set.seed(1L)
toy_n <- 300L
toy <- data.frame(
  time  = sort(rexp(toy_n, rate = 0.08)),
  event = rbinom(toy_n, 1L, 0.65),
  x1    = rnorm(toy_n),
  x2    = rnorm(toy_n),
  A     = rbinom(toy_n, 1L, 0.5)
)
fit_toy <- tryCatch(
  survival::coxph(Surv(time, event) ~ x1 + x2 + A, data = toy, x = TRUE, y = TRUE),
  error = function(e) NULL
)
if (!is.null(fit_toy)) {
  toy_res <- tryCatch({
    ipeval::ip_score(
      object                = list(toy_model = fit_toy),
      data                  = toy,
      outcome               = survival::Surv(toy$time, toy$event),
      treatment_formula     = A ~ 1,
      treatment_of_interest = 1L,
      time_horizon          = 8,
      cens_model            = "KM",
      bootstrap             = 0L
    )
  }, error = function(e) {
    cat("Toy dataset error:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(toy_res)) {
    cat("Toy dataset SUCCESS — ipeval works with simple sorted data.\n")
    cat("  This means the error on our data is specific to its structure\n")
    cat("  (likely discrete monthly times + competing event structure).\n")
    print(as.data.frame(toy_res), row.names = FALSE)
  }
} else {
  cat("Toy coxph failed.\n")
}

# ── Fallback: riskRegression::Score() — same IPCW logic, confirmed working ───
cat("\n\n========== FALLBACK: riskRegression::Score() ==========\n")
cat("(Score() uses the same IPCW/Brier/AUC framework as ipeval without the sort issue)\n\n")

fit_score <- tryCatch(
  survival::coxph(
    update(cs_trt_f, . ~ . - dummy_trt),   # refit WITHOUT dummy_trt for clean Score()
    data = df_sorted, x = TRUE, y = TRUE
  ),
  error = function(e) { cat("refit error:", conditionMessage(e), "\n"); fit_cs }
)

score_res <- tryCatch(
  riskRegression::Score(
    object   = list("CS-Cox readmit" = fit_score),
    formula  = as.formula(sprintf("Surv(.ftime, .readmit_cs) ~ 1")),
    data     = df_sorted,
    times    = HORIZONS,
    metrics  = c("auc", "brier"),
    se.fit   = FALSE,
    B        = 0L,
    null.model = TRUE
  ),
  error = function(e) { cat("Score() error:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(score_res)) {
  cat("-- Brier score by horizon --\n")
  print(score_res$Brier$score[, c("model","times","Brier")], row.names = FALSE)
  cat("\n-- AUC by horizon --\n")
  print(score_res$AUC$score[, c("model","times","AUC")], row.names = FALSE)

  cat("\n-- E/O via riskRegression::predictRisk() --\n")
  pred_mat <- riskRegression::predictRisk(fit_score, newdata = df_sorted, times = HORIZONS)
  eo_df <- data.frame(
    horizon   = HORIZONS,
    obs_AJ    = round(obs_aj,                                   5),
    mean_pred = round(colMeans(pred_mat, na.rm = TRUE),          5),
    eo_ratio  = round(colMeans(pred_mat, na.rm = TRUE) / obs_aj, 4)
  )
  print(eo_df, row.names = FALSE)
}

cat("\n========== Reference: calibration function (full data, 20-rep 5-fold CV) ==========\n")
ref <- data.frame(
  horizon = HORIZONS,
  eo_cal  = c(0.9910, 0.9912, 0.9893, 0.9860)
)
print(ref, row.names = FALSE)
cat("\n>> Both Score() E/O and cal-function E/O should be ≈ 1.\n")
cat(">> Any difference reflects apparent (Score, fit=predict) vs CV (cal-function).\n")

cat("\nDone.\n")
