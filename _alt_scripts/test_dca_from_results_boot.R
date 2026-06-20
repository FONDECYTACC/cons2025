# =============================================================================
# Functional + validity tests for the corrected
#   cons/_alt_scripts/dca_from_results_boot_for_metrics.R
#
# Self-contained: builds synthetic results_boot objects with KNOWN risk
# structure, sources the corrected wrapper (and the OLD _hist_scripts wrapper
# for an A/B comparison), and checks:
#   T1  normal path produces well-formed curves at all requested horizons
#   T2  eval_times mismatch: OLD aborts (stop), NEW degrades gracefully
#   T3  numerical round-trip: wrapper extracts adca's net_benefit unchanged
#   T4  INDEPENDENT validity of the net-benefit math (hand KM computation)
#   T5  run_dca_full_summary + summarize_dca_nb + panel figure run end to end
#   T6  new guard rails give clear errors (NULL summary, missing outcome)
#   T7  plot_dca_multi_horizon survives an all-NULL horizon (fix #3)
#
# Does NOT touch the notebook or write to cons/_out (uses tempdir()).
# Run:  Rscript cons/_alt_scripts/test_dca_from_results_boot.R
# =============================================================================

ROOT <- "G:/My Drive/Alvacast/SISTRAT 2023"

# Source adca FIRST (absolute path) so the wrapper's internal
# `if (!exists("run_adca_from_results_boot")) source(...)` guard is skipped and
# the test does not depend on the working directory.
source(file.path(ROOT, "cons/_hist_scripts/adca_from_results_boot.R"))

# --- NEW (corrected) wrapper: grab all public fns before OLD overrides them ---
source(file.path(ROOT, "cons/_alt_scripts/dca_from_results_boot_for_metrics.R"))
new_compute <- compute_dca_from_boot
new_full    <- run_dca_full_summary
new_summ    <- summarize_dca_nb
new_plot    <- plot_dca_multi_horizon
new_panel   <- make_dca_panel_figure

# --- OLD wrapper (for A/B on the mismatch case) ---
source(file.path(ROOT, "cons/_hist_scripts/dca_from_results_boot_for_metrics.R"))
old_compute <- compute_dca_from_boot

# restore NEW as the active definitions for the rest of the script
compute_dca_from_boot   <- new_compute
run_dca_full_summary    <- new_full
summarize_dca_nb        <- new_summ
plot_dca_multi_horizon  <- new_plot
make_dca_panel_figure   <- new_panel

# ----------------------------- helpers ---------------------------------------
PASS <- 0L; FAIL <- 0L
report <- function(name, ok, extra = "") {
  tag <- if (isTRUE(ok)) { PASS <<- PASS + 1L; "PASS" } else { FAIL <<- FAIL + 1L; "FAIL" }
  cat(sprintf("[%s] %s%s\n", tag, name, if (nzchar(extra)) paste0(" — ", extra) else ""))
}

# one replicate block: predicted survival from the SAME exponential model that
# generated the times => predictions carry real signal (model NB should beat 0).
make_block <- function(N, eval_times, seed) {
  set.seed(seed)
  lp    <- rnorm(N)
  rate  <- 0.01 * exp(0.5 * lp)
  ttime <- rexp(N, rate)
  cens  <- runif(N, 0, 120)
  obs_t <- pmin(ttime, cens)
  event <- as.integer(ttime <= cens)
  surv  <- vapply(eval_times, function(tt) exp(-rate * tt), numeric(N))  # N x k matrix
  list(surv_val_matrix = surv, y_val = data.frame(time = obs_t, event = event))
}

make_results_boot <- function(eval_times, n_blocks = 3L, N = 400L, seed0 = 1L) {
  raw <- lapply(seq_len(n_blocks), function(b) {
    list(
      imp_idx    = b, fold_idx = b,
      eval_times = eval_times,
      readmission = make_block(N, eval_times, seed0 + b),
      death       = make_block(N, eval_times, seed0 + 100L + b)
    )
  })
  list(raw_predictions = raw, config = list(eval_times = eval_times))
}

km_event_risk <- function(time, event, horizon) {
  f <- survival::survfit(survival::Surv(time, event) ~ 1)
  1 - summary(f, times = horizon, extend = TRUE)$surv[1]
}

THR <- seq(0.05, 0.45, by = 0.05)
EV4 <- c(6, 12, 36, 60)

cat("\n================ DCA wrapper test suite ================\n\n")

# ---------------------------------------------------------------- T1 normal ---
rb_a <- make_results_boot(EV4, seed0 = 1L)
rb_b <- make_results_boot(EV4, seed0 = 50L)
dca_r <- compute_dca_from_boot(list(shap = rb_a, full = rb_b),
                               risk = "readmission", horizons = EV4,
                               thresholds = THR, verbose = FALSE)
ok1 <- identical(names(dca_r), as.character(EV4)) &&
       all(vapply(EV4, function(h) {
         df <- dca_r[[as.character(h)]]$shap
         is.data.frame(df) && nrow(df) == length(THR) &&
           all(c("threshold","net_benefit_model","net_benefit_all","net_benefit_none") %in% names(df)) &&
           any(is.finite(df$net_benefit_model))
       }, logical(1)))
report("T1 normal path: curves at all 4 horizons, well-formed", ok1)

# ------------------------------------------------------ T2 eval_times mismatch ---
rb_mis <- make_results_boot(c(60, 108), seed0 = 7L)   # object only has 60 & 108
old_res <- tryCatch(
  suppressWarnings(old_compute(list(m = rb_mis), risk = "death",
                               horizons = EV4, thresholds = THR, verbose = FALSE)),
  error = function(e) structure("error", msg = conditionMessage(e)))
old_aborts <- identical(as.character(old_res), "error")

new_res <- tryCatch(
  suppressWarnings(new_compute(list(m = rb_mis), risk = "death",
                               horizons = EV4, thresholds = THR, verbose = FALSE)),
  error = function(e) structure("error", msg = conditionMessage(e)))
new_ok <- is.list(new_res) &&
          is.null(new_res[["6"]]$m) && is.null(new_res[["12"]]$m) &&
          is.null(new_res[["36"]]$m) &&
          is.data.frame(new_res[["60"]]$m) && nrow(new_res[["60"]]$m) == length(THR)
report("T2a OLD wrapper ABORTS on horizon not in eval_times",
       old_aborts, if (old_aborts) attr(old_res, "msg") else "did not error")
report("T2b NEW wrapper degrades gracefully (skips 6/12/36, keeps 60)", new_ok)

# --------------------------------------------- T3 numerical round-trip vs adca ---
ad <- run_adca_from_results_boot(rb_a, thresholds = THR, horizons = EV4,
                                 output_dir = file.path(tempdir(), "adca_t3"),
                                 create_plot = FALSE, save_raw = FALSE)
# adca's Model net_benefit for readmission at each (h, thr) must equal the
# wrapper's net_benefit_model exactly (the fix changed control flow, not math).
rt_ok <- TRUE; max_abs <- 0
for (h in EV4) {
  sub <- ad$summary[ad$summary$risk == "readmission" &
                    ad$summary$horizon == h &
                    ad$summary$strategy == "Model", c("threshold","net_benefit_mean")]
  sub <- sub[order(sub$threshold), ]
  wf  <- dca_r[[as.character(h)]]$shap
  wf  <- wf[order(wf$threshold), ]
  d   <- abs(sub$net_benefit_mean - wf$net_benefit_model)
  max_abs <- max(max_abs, max(d, na.rm = TRUE))
  if (any(d > 1e-9, na.rm = TRUE)) rt_ok <- FALSE
}
report("T3 round-trip: wrapper == adca net_benefit (math unchanged)",
       rt_ok, sprintf("max |diff| = %.2e", max_abs))

# ---------------------------------- T4 INDEPENDENT validity of net-benefit math ---
# single replicate => adca pooled mean == that replicate, so we can hand-check.
seed_single <- 321L
rb1 <- list(raw_predictions = list(list(
            imp_idx = 1L, fold_idx = 1L, eval_times = 12,
            readmission = make_block(600L, 12, seed_single),
            death       = make_block(600L, 12, seed_single + 1L))),
            config = list(eval_times = 12))
ad1 <- run_adca_from_results_boot(rb1, thresholds = c(0.10, 0.20, 0.30), horizons = 12,
                                  output_dir = file.path(tempdir(), "adca_t4"),
                                  create_plot = FALSE, save_raw = FALSE)
blkR <- make_block(600L, 12, seed_single)               # same data as went in
risk <- 1 - blkR$surv_val_matrix[, 1]
tt   <- blkR$y_val$time; ev <- blkR$y_val$event
pt   <- 0.20; odds <- pt / (1 - pt)
sel  <- risk >= pt
prate <- mean(sel)
risk_all <- km_event_risk(tt, ev, 12)
risk_pos <- if (any(sel)) km_event_risk(tt[sel], ev[sel], 12) else 0
nb_model_hand <- prate * (risk_pos - (1 - risk_pos) * odds)
nb_all_hand   <- risk_all - (1 - risk_all) * odds
get_adca <- function(strat) {
  ad1$summary$net_benefit_mean[ad1$summary$risk == "readmission" &
    ad1$summary$horizon == 12 & abs(ad1$summary$threshold - pt) < 1e-9 &
    ad1$summary$strategy == strat]
}
d_model <- abs(get_adca("Model")     - nb_model_hand)
d_all   <- abs(get_adca("Treat all") - nb_all_hand)
report("T4a net_benefit(Model) matches independent KM hand-calc",
       isTRUE(d_model < 1e-8), sprintf("|diff| = %.2e (hand=%.5f)", d_model, nb_model_hand))
report("T4b net_benefit(Treat all) matches independent KM hand-calc",
       isTRUE(d_all < 1e-8), sprintf("|diff| = %.2e (hand=%.5f)", d_all, nb_all_hand))
report("T4c Treat none net benefit == 0",
       isTRUE(all(ad1$summary$net_benefit_mean[ad1$summary$strategy == "Treat none"] == 0)))

# ----------------------------------------- T5 full summary + nb + panel figure ---
full_res <- run_dca_full_summary(list(full = rb_b, shap = rb_a),
                                 horizons = EV4, thresholds = THR, verbose = FALSE)
nb <- tryCatch(summarize_dca_nb(full_res$shap$summary), error = function(e) e)
ok5a <- is.list(nb) && all(c("dca_nb","any_useful","focus_summary") %in% names(nb)) &&
        all(c("nb_model","nb_treat_all","nb_treat_none","model_useful") %in% names(nb$dca_nb))
report("T5a run_dca_full_summary + summarize_dca_nb end to end", ok5a)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  pnl <- tryCatch(make_dca_panel_figure(full_res$full$summary, full_res$shap$summary,
                                        outcome = "death", horizons = c(12, 36, 60)),
                  error = function(e) e)
  report("T5b make_dca_panel_figure returns a ggplot",
         inherits(pnl, "ggplot"), if (inherits(pnl,"error")) conditionMessage(pnl) else "")
} else {
  report("T5b make_dca_panel_figure (skipped: ggplot2 not installed)", TRUE)
}

# ------------------------------------------------------------ T6 guard rails ---
g1 <- tryCatch({ summarize_dca_nb(NULL); FALSE }, error = function(e) grepl("non-empty", conditionMessage(e)))
report("T6a summarize_dca_nb(NULL) stops with a clear message", isTRUE(g1))
if (requireNamespace("ggplot2", quietly = TRUE)) {
  g2 <- tryCatch({ make_dca_panel_figure(full_res$full$summary, full_res$shap$summary,
                                         outcome = "death", horizons = 999); FALSE },
                 error = function(e) grepl("No rows", conditionMessage(e)))
  report("T6b make_dca_panel_figure(missing horizon) stops clearly", isTRUE(g2))
} else {
  report("T6b panel guard (skipped: ggplot2 not installed)", TRUE)
}

# --------------------------------------------- T7 plot survives all-NULL horizon ---
if (requireNamespace("ggplot2", quietly = TRUE)) {
  # new_res has horizons 6/12/36 = all NULL, 60 populated
  p <- tryCatch(suppressWarnings(plot_dca_multi_horizon(new_res, title = "T7")),
                error = function(e) e)
  report("T7 plot_dca_multi_horizon survives all-NULL horizons (fix #3)",
         inherits(p, "ggplot"), if (inherits(p,"error")) conditionMessage(p) else "")
} else {
  report("T7 plot guard (skipped: ggplot2 not installed)", TRUE)
}

cat(sprintf("\n================ %d passed, %d failed ================\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
