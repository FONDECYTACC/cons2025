# =============================================================================
# Validity + functional tests for the AJ-for-readmission DCA engine
#   cons/_alt_scripts/adca_from_results_boot.R
#
# Builds synthetic results_boot objects WITH a real competing risk (death),
# row-aligned readmission/death blocks (as evaluate_dual_cox_python_style stores
# them), and checks:
#   A1  Aalen-Johansen readmit CIF < 1-KM readmit CIF (competing-risk correction)
#   A2  pipeline AJ observed risk == independent survfit() AJ (validity)
#   A3  DEATH net benefit is IDENTICAL to the _hist_scripts (KM) engine
#   A4  model-arm net benefit matches an independent AJ hand-calc (dedup correct)
#   A5  graceful fallback to KM when the death block is not row-aligned
#   A6  end-to-end through compute_dca_from_boot (the _alt wrapper)
#
# Run: Rscript cons/_alt_scripts/test_adca_aj_from_results_boot.R
# =============================================================================

ROOT <- "G:/My Drive/Alvacast/SISTRAT 2023"

# _alt (AJ) engine into the global env
source(file.path(ROOT, "cons/_alt_scripts/adca_from_results_boot.R"))
# _hist (KM) engine isolated in its own environment for the death A/B
hist_env <- new.env()
sys.source(file.path(ROOT, "cons/_hist_scripts/adca_from_results_boot.R"), envir = hist_env)

PASS <- 0L; FAIL <- 0L
report <- function(name, ok, extra = "") {
  tag <- if (isTRUE(ok)) { PASS <<- PASS + 1L; "PASS" } else { FAIL <<- FAIL + 1L; "FAIL" }
  cat(sprintf("[%s] %s%s\n", tag, name, if (nzchar(extra)) paste0(" — ", extra) else ""))
}
TD <- file.path(tempdir(), "adca_aj_tests"); dir.create(TD, showWarnings = FALSE)

# one row-aligned (readmission + death) replicate with a real competing risk
make_cr_item <- function(N, eval_times, seed, imp, fold, trunc_death = 0L) {
  set.seed(seed)
  lp_r   <- rnorm(N); lp_d <- rnorm(N)
  rate_r <- 0.020 * exp(0.45 * lp_r)   # readmission hazard (signal via lp_r)
  rate_d <- 0.015 * exp(0.30 * lp_d)   # death hazard (substantial competing risk)
  Tr <- rexp(N, rate_r); Td <- rexp(N, rate_d); Ca <- runif(N, 12, 120)
  readmit_time  <- pmin(Tr, Td, Ca)
  readmit_event <- as.integer(Tr <= pmin(Td, Ca))
  death_time    <- pmin(Td, Ca)
  death_event   <- as.integer(Td <= Ca)
  surv_r <- vapply(eval_times, function(tt) exp(-rate_r * tt), numeric(N))
  surv_d <- vapply(eval_times, function(tt) exp(-rate_d * tt), numeric(N))
  dN <- N - trunc_death                # optionally break row-alignment
  list(
    imp_idx = imp, fold_idx = fold, eval_times = eval_times,
    original_val_idx = seq_len(N),
    readmission = list(surv_val_matrix = surv_r,
                       y_val = data.frame(time = readmit_time, event = readmit_event)),
    death = list(surv_val_matrix = surv_d[seq_len(dN), , drop = FALSE],
                 y_val = data.frame(time = death_time[seq_len(dN)], event = death_event[seq_len(dN)]))
  )
}
make_rb <- function(eval_times, n_blocks = 2L, N = 800L, seed0 = 10L, trunc_death = 0L) {
  raw <- lapply(seq_len(n_blocks), function(b)
    make_cr_item(N, eval_times, seed0 + b, imp = b, fold = b,
                 trunc_death = if (b == 2L) trunc_death else 0L))
  list(raw_predictions = raw, config = list(eval_times = eval_times))
}

EV <- c(12, 36, 60); THR <- seq(0.05, 0.40, by = 0.05)
getv <- function(sm, rk, h, thr, strat, col = "net_benefit_mean")
  sm[[col]][sm$risk == rk & sm$horizon == h & abs(sm$threshold - thr) < 1e-9 & sm$strategy == strat]

cat("\n========== AJ-DCA validity suite ==========\n\n")

# ---- single-replicate object for exact (non-pooled) hand checks ----
rb1 <- list(raw_predictions = list(make_cr_item(1500L, EV, seed = 99L, imp = 1, fold = 1)),
            config = list(eval_times = EV))
ad_aj <- run_adca_from_results_boot(rb1, thresholds = THR, horizons = EV,
                                    output_dir = file.path(TD, "aj1"),
                                    create_plot = FALSE, save_raw = FALSE, verbose = FALSE)

# rebuild the competing-risk first-event encoding the way the engine does
blk <- rb1$raw_predictions[[1]]
rt <- blk$readmission$y_val$time; re <- blk$readmission$y_val$event
dt <- blk$death$y_val$time;       de <- blk$death$y_val$event
ftime <- pmin(rt, dt)
r_first <- re == 1L & abs(rt - ftime) <= 1e-8
d_first <- de == 1L & abs(dt - ftime) <= 1e-8
fstatus <- rep(0L, length(ftime)); fstatus[r_first & !d_first] <- 1L
fstatus[d_first & !r_first] <- 2L; fstatus[r_first & d_first] <- 2L  # death_first tie rule

aj_indep <- function(ft, fs, h) {                 # independent AJ via survfit
  sf <- factor(fs, levels = 0:2, labels = c("censor","readmit","death"))
  s  <- summary(survival::survfit(survival::Surv(ft, sf) ~ 1), times = h, extend = TRUE)
  ps <- s$pstate; as.numeric(ps[1, match("readmit", colnames(ps))])
}
km_risk <- function(tm, ev, h) {                  # 1 - KM (death censored)
  1 - summary(survival::survfit(survival::Surv(tm, ev) ~ 1), times = h, extend = TRUE)$surv[1]
}

# ---- A1: AJ < 1-KM at a long horizon (competing-risk correction) ----
aj60 <- aj_indep(ftime, fstatus, 60); km60 <- km_risk(rt, re, 60)
report("A1 Aalen-Johansen readmit CIF < 1-KM at 60m (competing-risk correction)",
       is.finite(aj60) && is.finite(km60) && aj60 < km60 && aj60 > 0,
       sprintf("AJ=%.4f < KM=%.4f", aj60, km60))

# ---- A2: pipeline treat-all observed risk == independent AJ ----
pipe_all_36 <- getv(ad_aj$summary, "readmission", 36, THR[1], "Treat all", "observed_event_risk_mean")
indep_all_36 <- aj_indep(ftime, fstatus, 36)
report("A2 engine readmission observed risk == independent survfit AJ",
       isTRUE(abs(pipe_all_36 - indep_all_36) < 1e-8),
       sprintf("|diff|=%.2e", abs(pipe_all_36 - indep_all_36)))

# ---- A4: model-arm net benefit matches independent AJ hand-calc (dedup correct) ----
h4 <- 36; thr4 <- 0.20; odds <- thr4 / (1 - thr4)
risk_r36 <- 1 - blk$readmission$surv_val_matrix[, match(h4, EV)]
sel <- risk_r36 >= thr4
p_pos <- mean(sel)
aj_pos <- aj_indep(ftime[sel], fstatus[sel], h4)
nb_hand <- p_pos * (aj_pos - (1 - aj_pos) * odds)
nb_pipe <- getv(ad_aj$summary, "readmission", h4, thr4, "Model")
report("A4 model-arm NB == independent AJ hand-calc (validates dedup + plumbing)",
       isTRUE(abs(nb_hand - nb_pipe) < 1e-8),
       sprintf("|diff|=%.2e (hand=%.5f)", abs(nb_hand - nb_pipe), nb_hand))

# ---- A3: DEATH net benefit identical between AJ engine and _hist KM engine ----
rb2 <- make_rb(EV, n_blocks = 2L, N = 900L, seed0 = 4L)
ad_alt  <- run_adca_from_results_boot(rb2, thresholds = THR, horizons = EV,
              output_dir = file.path(TD,"alt"), create_plot = FALSE, save_raw = FALSE, verbose = FALSE)
ad_hist <- hist_env$run_adca_from_results_boot(rb2, thresholds = THR, horizons = EV,
              output_dir = file.path(TD,"hist"), create_plot = FALSE, save_raw = FALSE)
d_alt  <- ad_alt$summary[ad_alt$summary$risk == "death", ]
d_hist <- ad_hist$summary[ad_hist$summary$risk == "death", ]
key <- function(s) paste(s$horizon, s$threshold, s$strategy)
d_alt  <- d_alt[order(key(d_alt)), ];  d_hist <- d_hist[order(key(d_hist)), ]
death_diff <- max(abs(d_alt$net_benefit_mean - d_hist$net_benefit_mean), na.rm = TRUE)
report("A3 DEATH net benefit identical to _hist (KM) engine (death untouched)",
       isTRUE(death_diff < 1e-9), sprintf("max |diff|=%.2e", death_diff))

# sanity: readmission SHOULD differ between AJ and KM engines
r_alt  <- ad_alt$summary[ad_alt$summary$risk == "readmission" & ad_alt$summary$strategy == "Treat all", ]
r_hist <- ad_hist$summary[ad_hist$summary$risk == "readmission" & ad_hist$summary$strategy == "Treat all", ]
r_alt <- r_alt[order(r_alt$horizon, r_alt$threshold), ]; r_hist <- r_hist[order(r_hist$horizon, r_hist$threshold), ]
readmit_gap <- max(abs(r_alt$observed_event_risk_mean - r_hist$observed_event_risk_mean), na.rm = TRUE)
report("A3b readmission observed risk DIFFERS between AJ and KM engines (as expected)",
       isTRUE(readmit_gap > 1e-4), sprintf("max |AJ-KM|=%.4f", readmit_gap))

# ---- A5: graceful fallback when death block not row-aligned ----
rb_mis <- make_rb(EV, n_blocks = 2L, N = 800L, seed0 = 7L, trunc_death = 3L)  # block 2 death has N-3 rows
ad_fb <- tryCatch(
  run_adca_from_results_boot(rb_mis, thresholds = THR, horizons = EV,
     output_dir = file.path(TD,"fb"), create_plot = FALSE, save_raw = FALSE, verbose = FALSE),
  error = function(e) e)
fb_ok <- !inherits(ad_fb, "error") &&
         ad_fb$config$n_readmit_km_fallback == 1L &&
         ad_fb$config$n_readmit_aalen_johansen == 1L &&
         nrow(ad_fb$summary) > 0L
report("A5 misaligned death block -> that replicate falls back to KM, run still succeeds",
       fb_ok, if (inherits(ad_fb,"error")) conditionMessage(ad_fb) else
         sprintf("AJ reps=%d, KM-fallback reps=%d",
                 ad_fb$config$n_readmit_aalen_johansen, ad_fb$config$n_readmit_km_fallback))

# ---- A6: end-to-end through the _alt wrapper (compute_dca_from_boot) ----
source(file.path(ROOT, "cons/_alt_scripts/dca_from_results_boot_for_metrics.R"))
dca <- tryCatch(
  compute_dca_from_boot(list(shap = rb2), risk = "readmission",
                        horizons = EV, thresholds = THR, verbose = FALSE),
  error = function(e) e)
a6_ok <- !inherits(dca, "error") &&
         is.data.frame(dca[["36"]]$shap) && nrow(dca[["36"]]$shap) == length(THR) &&
         any(is.finite(dca[["36"]]$shap$net_benefit_model))
report("A6 compute_dca_from_boot (wrapper) runs with the AJ engine", a6_ok,
       if (inherits(dca,"error")) conditionMessage(dca) else "")

cat(sprintf("\n========== %d passed, %d failed ==========\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
