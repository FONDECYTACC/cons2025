# =============================================================================
# Validity tests for cons/_alt_scripts/nri_idi_from_results_boot.R
#   N1 runs; both risks; required metrics present
#   N2 readmission CR (Aalen-Johansen) differs from death-as-censoring IPCW;
#      death is unaffected by readmit_method
#   N3 corrected DEATH NRI/IDI differs from the historical order-bug engine
#   N4 identical death predictions  ->  death IDI/NRI ~ 0  (the Full PH vs SHAP
#      primary case: same death_updated2 on both sides)
#   N5 a competing death before t is a NON-event under CR weights (not dropped)
#   N6 make_nri_idi_epidemiology_table builds a table from the result
# Run: Rscript cons/_alt_scripts/test_nri_idi_aj.R
# =============================================================================
ROOT <- "G:/My Drive/Alvacast/SISTRAT 2023"
source(file.path(ROOT, "cons/_alt_scripts/nri_idi_from_results_boot.R"))
hist_env <- new.env()
sys.source(file.path(ROOT, "cons/_hist_scripts/nri_idi_from_results_boot.R"), envir = hist_env)

PASS <- 0L; FAIL <- 0L
report <- function(name, ok, extra = "") {
  tag <- if (isTRUE(ok)) { PASS <<- PASS + 1L; "PASS" } else { FAIL <<- FAIL + 1L; "FAIL" }
  cat(sprintf("[%s] %s%s\n", tag, name, if (nzchar(extra)) paste0(" — ", extra) else ""))
}
TD <- file.path(tempdir(), "nri_aj"); dir.create(TD, showWarnings = FALSE)
EV <- c(12, 36, 60); CP <- c(0.05, 0.10, 0.20)

# shared latent outcome per fold; OLD and NEW get different prediction matrices
make_pair <- function(N, EV, seed, imp, fold, same_death = FALSE) {
  set.seed(seed)
  lp <- rnorm(N); lpd <- rnorm(N)
  rate_r <- 0.020*exp(0.45*lp); rate_d <- 0.015*exp(0.30*lpd)
  Tr <- rexp(N, rate_r); Td <- rexp(N, rate_d); Ca <- runif(N, 12, 120)
  rt <- pmin(Tr,Td,Ca); re <- as.integer(Tr <= pmin(Td,Ca))
  dt <- pmin(Td,Ca);    de <- as.integer(Td <= Ca)
  S <- function(rate) vapply(EV, function(tt) exp(-rate*tt), numeric(N))
  rate_r_new <- 0.020*exp(0.45*lp + 0.35*rnorm(N))
  rate_d_new <- if (same_death) rate_d else 0.015*exp(0.30*lpd + 0.35*rnorm(N))
  list(yr=data.frame(time=rt,event=re), yd=data.frame(time=dt,event=de),
       sr_old=S(rate_r), sd_old=S(rate_d), sr_new=S(rate_r_new), sd_new=S(rate_d_new),
       imp=imp, fold=fold)
}
build_rb <- function(items, which = c("old","new"), EV) {
  which <- match.arg(which)
  raw <- lapply(items, function(it) list(
    imp_idx=it$imp, fold_idx=it$fold, eval_times=EV, original_val_idx=seq_len(nrow(it$yr)),
    readmission=list(surv_val_matrix=if(which=="old") it$sr_old else it$sr_new, y_val=it$yr),
    death      =list(surv_val_matrix=if(which=="old") it$sd_old else it$sd_new, y_val=it$yd)))
  list(raw_predictions=raw, config=list(eval_times=EV))
}
items  <- lapply(1:2, function(b) make_pair(900L, EV, 100L+b, b, b))
rb_old <- build_rb(items, "old", EV); rb_new <- build_rb(items, "new", EV)

cat("\n========== NRI/IDI (AJ) validity suite ==========\n\n")

# ---- N1 ----
res_aj <- run_nri_idi_from_results_boot(rb_old, rb_new, horizons=EV, cut_points=CP,
            old_label="Full PH", new_label="SHAP", output_dir=file.path(TD,"aj"),
            prefix="t", save_raw=FALSE, verbose=TRUE)
req <- c("continuous_nri","continuous_nri_events","continuous_nri_nonevents",
         "categorical_nri","idi","discrimination_old","discrimination_new")
n1 <- is.data.frame(res_aj$summary) && all(req %in% res_aj$summary$metric) &&
      all(c("readmission","death") %in% res_aj$summary$risk)
report("N1 runs; both risks present; required metrics present", n1,
       sprintf("CR reps=%d, fallback=%d", res_aj$config$n_readmit_competing_risk,
               res_aj$config$n_readmit_ipcw_fallback))

# ---- N2 readmission CR != death-as-censoring IPCW; death unaffected ----
res_ip <- run_nri_idi_from_results_boot(rb_old, rb_new, horizons=EV, cut_points=CP,
            readmit_method="ipcw", output_dir=file.path(TD,"ip"), prefix="t",
            save_raw=FALSE, verbose=FALSE)
gv <- function(s, rk, h, mt) s$mean[s$risk==rk & s$horizon==h & s$metric==mt]
re_aj <- gv(res_aj$summary, "readmission", 60, "discrimination_old")
re_ip <- gv(res_ip$summary, "readmission", 60, "discrimination_old")
de_aj <- gv(res_aj$summary, "death", 60, "idi")
de_ip <- gv(res_ip$summary, "death", 60, "idi")
report("N2a readmission discrimination differs CR vs death-as-censoring",
       isTRUE(abs(re_aj - re_ip) > 1e-4), sprintf("CR=%.4f vs IPCW=%.4f", re_aj, re_ip))
report("N2b death NRI/IDI unaffected by readmit_method",
       isTRUE(abs(de_aj - de_ip) < 1e-9), sprintf("|diff|=%.2e", abs(de_aj - de_ip)))

# ---- N3 corrected death differs from the historical order-bug engine ----
res_hist <- hist_env$run_nri_idi_from_results_boot(rb_old, rb_new, horizons=EV, cut_points=CP,
              output_dir=file.path(TD,"hist"), prefix="t", save_raw=FALSE)
da <- res_aj$summary[res_aj$summary$risk=="death", ]
dh <- res_hist$summary[res_hist$summary$risk=="death", ]
k <- function(s) paste(s$horizon, s$metric)
da <- da[order(k(da)), ]; dh <- dh[order(k(dh)), ]
common <- intersect(paste(da$horizon,da$metric), paste(dh$horizon,dh$metric))
da2 <- da[paste(da$horizon,da$metric) %in% common, ]; dh2 <- dh[paste(dh$horizon,dh$metric) %in% common, ]
death_diff <- max(abs(da2$mean - dh2$mean), na.rm=TRUE)
report("N3 corrected DEATH NRI/IDI differs from order-bug _hist engine",
       isTRUE(death_diff > 1e-4),
       sprintf("max |diff|=%.2e", death_diff))

# ---- N4 identical death predictions -> death IDI/NRI ~ 0 ----
items_sd <- lapply(1:2, function(b) make_pair(900L, EV, 200L+b, b, b, same_death=TRUE))
rb_old_sd <- build_rb(items_sd,"old",EV); rb_new_sd <- build_rb(items_sd,"new",EV)
res_sd <- run_nri_idi_from_results_boot(rb_old_sd, rb_new_sd, horizons=EV, cut_points=CP,
            output_dir=file.path(TD,"sd"), prefix="t", save_raw=FALSE, verbose=FALSE)
d_idi  <- gv(res_sd$summary, "death", 60, "idi")
d_cnri <- gv(res_sd$summary, "death", 60, "continuous_nri")
report("N4 identical death model -> death IDI ~ 0 and continuous NRI ~ 0",
       isTRUE(abs(d_idi) < 1e-9 && abs(d_cnri) < 1e-9),
       sprintf("idi=%.2e, cNRI=%.2e", d_idi, d_cnri))

# ---- N5 competing death before t is a NON-event under CR weights ----
# 3 subjects: readmit@10, death@20, event-free (admin censor @80). horizon=36.
ftime  <- c(10, 20, 80); fstatus <- c(1L, 2L, 0L)
w <- .rb_cr_ipcw_weights(ftime, fstatus, horizon = 36, g_min = 0.01)
n5 <- w$event[1] > 0 && w$event[2] == 0 && w$event[3] == 0 &&        # only readmit is event
      w$nonevent[1] == 0 && w$nonevent[2] > 0 && w$nonevent[3] > 0    # death + efree are nonevents
report("N5 CR weights: competing death counts as NON-event (not dropped)", isTRUE(n5),
       sprintf("event=[%.2f,%.2f,%.2f] nonevent=[%.2f,%.2f,%.2f]",
               w$event[1],w$event[2],w$event[3],w$nonevent[1],w$nonevent[2],w$nonevent[3]))

# ---- N6 epidemiology table builds ----
source(file.path(ROOT, "cons/_hist_scripts/make_nri_idi_epidemiology_table.R"))
tab <- tryCatch(make_nri_idi_epidemiology_table(res_aj, output_dir=file.path(TD,"tab"),
                  prefix="t", old_label="Full PH", new_label="SHAP"),
                error=function(e) e)
report("N6 make_nri_idi_epidemiology_table builds from CR result",
       !inherits(tab,"error") && is.data.frame(tab$table) && nrow(tab$table) > 0,
       if (inherits(tab,"error")) conditionMessage(tab) else paste(nrow(tab$table),"rows"))

# ---- N7 different eval_times grids align (only requested horizons need to match) ----
# rich grid (results_upd2_py-style) vs lean grid; same outcome rows (same seed).
mk_grid <- function(N, EV, seed, imp, fold, pshift = 0) {
  set.seed(seed)
  lp <- rnorm(N); lpd <- rnorm(N)
  rr <- 0.02*exp(0.45*lp); rd <- 0.015*exp(0.30*lpd)
  Tr <- rexp(N, rr); Td <- rexp(N, rd); Ca <- runif(N, 12, 120)
  rt <- pmin(Tr,Td,Ca); re <- as.integer(Tr <= pmin(Td,Ca))
  dt <- pmin(Td,Ca);    de <- as.integer(Td <= Ca)
  S <- function(rate) vapply(EV, function(tt) exp(-rate*tt), numeric(N))
  list(imp_idx=imp, fold_idx=fold, eval_times=EV, original_val_idx=seq_len(N),
       readmission=list(surv_val_matrix=S(rr*exp(pshift)), y_val=data.frame(time=rt, event=re)),
       death      =list(surv_val_matrix=S(rd*exp(pshift)), y_val=data.frame(time=dt, event=de)))
}
rbg <- function(EV, pshift) list(raw_predictions=lapply(1:2, function(b) mk_grid(800L, EV, 300L+b, b, b, pshift)),
                                 config=list(eval_times=EV))
old_rich <- rbg(c(3,6,12,36,60,108), 0.0); new_lean <- rbg(c(6,12,36,60), 0.3); old_lean <- rbg(c(6,12,36,60), 0.0)
r_diff <- tryCatch(run_nri_idi_from_results_boot(old_rich, new_lean, horizons=c(6,12,36,60), cut_points=CP,
            output_dir=file.path(TD,"g1"), prefix="t", save_raw=FALSE, verbose=FALSE), error=function(e) e)
r_same <- run_nri_idi_from_results_boot(old_lean, new_lean, horizons=c(6,12,36,60), cut_points=CP,
            output_dir=file.path(TD,"g2"), prefix="t", save_raw=FALSE, verbose=FALSE)
kg <- function(s) paste(s$risk,s$horizon,s$metric)
n7 <- !inherits(r_diff,"error")
if (n7) {
  a7 <- r_diff$summary[order(kg(r_diff$summary)),]; b7 <- r_same$summary[order(kg(r_same$summary)),]
  m7 <- match(kg(a7), kg(b7)); n7 <- max(abs(a7$mean - b7$mean[m7]), na.rm=TRUE) < 1e-9
}
report("N7 different eval_times grids run + match same-grid at shared horizons",
       isTRUE(n7), if (inherits(r_diff,"error")) conditionMessage(r_diff) else "diff 0")

cat(sprintf("\n========== %d passed, %d failed ==========\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
