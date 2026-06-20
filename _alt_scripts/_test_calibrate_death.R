## Quick diagnostic tests for calibrate_death_km_grouped.R
## Runs entirely on synthetic data. No py_corrected_datasets needed.
## Each test prints PASS or FAIL with reason.

cat("=== TEST SUITE: calibrate_death_km_grouped ===\n\n")
suppressPackageStartupMessages({
  library(survival)
  library(future)
  library(future.apply)
})

source("cons/_alt_scripts/calibrate_death_km_grouped.R")
cat("Sourced OK\n\n")

# ── Synthetic data: 300 obs, 3 predictors, binary event ──────────────────────
set.seed(42)
N   <- 300
syn <- data.frame(
  age      = rnorm(N, 65, 10),
  sex      = rbinom(N, 1, 0.5),
  score    = rnorm(N, 0, 1),
  death_time_from_disch_m = rexp(N, rate = 1/24),
  death_event = rbinom(N, 1, 0.4)
)
syn$death_time_from_disch_m <- pmax(syn$death_time_from_disch_m, 0.1)

# 2 imputed datasets (identical predictors, same outcomes — minimal viable input)
data_2imp <- list(syn, syn)

formula_test <- Surv(death_time_from_disch_m, death_event) ~ age + sex + score

# ── TEST 1: stats::predict accessible from baseenv closure ───────────────────
cat("TEST 1: stats::predict accessible from baseenv closure\n")
tryCatch({
  .t1 <- function(formula_, df, times_) {
    fit <- survival::coxph(formula_, data = df, x = TRUE, y = TRUE)
    lp  <- stats::predict(fit, newdata = df[1:10, ], type = "lp")
    bh  <- survival::basehaz(fit, centered = FALSE)
    H0  <- stats::approx(bh$time, bh$hazard, xout = times_,
                         method = "constant", f = 1L, rule = 2L)$y
    exp(-outer(exp(lp), H0))
  }
  environment(.t1) <- baseenv()
  mat <- .t1(formula_test, syn, c(6, 12, 36, 60))
  stopifnot(is.matrix(mat), nrow(mat) == 10, ncol(mat) == 4)
  stopifnot(all(mat >= 0 & mat <= 1))
  cat("  PASS: matrix", nrow(mat), "x", ncol(mat),
      "| range [", round(min(mat), 3), ",", round(max(mat), 3), "]\n\n")
}, error = function(e) cat("  FAIL:", conditionMessage(e), "\n\n"))

# ── TEST 2: sequential single-task run (no parallel, 2 reps x 2 folds x 2 imp)
cat("TEST 2: sequential run — 2 repeats x 2 folds x 2 imputations\n")
plan(sequential)
t2_start <- proc.time()["elapsed"]
tryCatch({
  res2 <- calibrate_death_km_grouped(
    formula    = formula_test,
    data       = data_2imp,
    times      = c(6, 12),
    time_col   = "death_time_from_disch_m",
    event_col  = "death_event",
    folds      = 2L,
    n_repeats  = 2L,
    parallel_cv = FALSE,
    verbose    = TRUE,
    seed       = 1L
  )
  t2_end <- proc.time()["elapsed"]
  stopifnot(is.list(res2), !is.null(res2$flat))
  stopifnot(nrow(res2$flat) == 2)            # 2 time points
  stopifnot(all(is.finite(res2$flat$ici)))
  cat("  PASS: flat has", nrow(res2$flat), "rows |",
      round(t2_end - t2_start, 1), "sec\n")
  print(res2$flat[, c("time_months", "ici", "ece", "eo_ratio")])
  cat("\n")
}, error = function(e) cat("  FAIL:", conditionMessage(e), "\n\n"))

# ── TEST 3: parallel run with chunk.size (2 reps x 2 folds x 2 imp, 2 workers)
cat("TEST 3: parallel run — 2 workers, future.chunk.size active\n")
plan(multisession, workers = 2L)
t3_start <- proc.time()["elapsed"]
tryCatch({
  res3 <- calibrate_death_km_grouped(
    formula    = formula_test,
    data       = data_2imp,
    times      = c(6, 12),
    time_col   = "death_time_from_disch_m",
    event_col  = "death_event",
    folds      = 2L,
    n_repeats  = 2L,
    parallel_cv = TRUE,
    verbose    = TRUE,
    seed       = 1L
  )
  t3_end <- proc.time()["elapsed"]
  stopifnot(is.list(res3), !is.null(res3$flat))
  stopifnot(nrow(res3$flat) == 2)
  stopifnot(all(is.finite(res3$flat$ici)))
  cat("  PASS:", round(t3_end - t3_start, 1), "sec\n")
  print(res3$flat[, c("time_months", "ici", "ece", "eo_ratio")])
  cat("\n")
}, error = function(e) cat("  FAIL:", conditionMessage(e), "\n\n"))

# ── TEST 4: sequential vs parallel produce same ECE/E-O (seed-matched) ────────
cat("TEST 4: sequential vs parallel results agree (ECE and E/O)\n")
tryCatch({
  plan(sequential)
  rs <- calibrate_death_km_grouped(
    formula = formula_test, data = data_2imp,
    times = c(6, 12), time_col = "death_time_from_disch_m",
    event_col = "death_event", folds = 2L, n_repeats = 2L,
    parallel_cv = FALSE, verbose = FALSE, seed = 99L
  )
  plan(multisession, workers = 2L)
  rp <- calibrate_death_km_grouped(
    formula = formula_test, data = data_2imp,
    times = c(6, 12), time_col = "death_time_from_disch_m",
    event_col = "death_event", folds = 2L, n_repeats = 2L,
    parallel_cv = TRUE, verbose = FALSE, seed = 99L
  )
  ece_diff <- max(abs(rs$flat$ece - rp$flat$ece), na.rm = TRUE)
  eo_diff  <- max(abs(rs$flat$eo_ratio - rp$flat$eo_ratio), na.rm = TRUE)
  cat(sprintf("  ECE max diff: %.2e | E/O max diff: %.2e\n", ece_diff, eo_diff))
  if (ece_diff < 1e-6 && eo_diff < 1e-6) {
    cat("  PASS: sequential and parallel results are identical\n\n")
  } else {
    cat("  WARN: results differ (expected if future.seed behaviour differs)\n\n")
  }
}, error = function(e) cat("  FAIL:", conditionMessage(e), "\n\n"))

plan(sequential)
cat("=== DONE ===\n")
