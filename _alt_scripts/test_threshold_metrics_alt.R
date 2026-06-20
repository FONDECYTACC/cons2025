# =============================================================================
# Validity tests for cons/_alt_scripts/threshold_metrics_from_results_boot.R
#   TM1 complete_case == _hist engine where metrics are defined (regression)
#   TM2 undefined metric -> NA (alt) vs 0 (_hist)  [the pooling-bias fix]
#   TM3 ipcw == complete_case when there is NO censoring before t (sanity)
#   TM4 ipcw readmission counts competing deaths as controls (n_controls up)
#   TM5 ipcw death differs from complete_case under real censoring
#   TM6 end-to-end both estimators; summary_wide well-formed
# Run: Rscript cons/_alt_scripts/test_threshold_metrics_alt.R
# =============================================================================
ROOT <- "G:/My Drive/Alvacast/SISTRAT 2023"
source(file.path(ROOT, "cons/_alt_scripts/threshold_metrics_from_results_boot.R"))
hist_env <- new.env()
sys.source(file.path(ROOT, "cons/_hist_scripts/threshold_metrics_from_results_boot.R"), envir = hist_env)

PASS <- 0L; FAIL <- 0L
report <- function(name, ok, extra = "") {
  tag <- if (isTRUE(ok)) { PASS <<- PASS + 1L; "PASS" } else { FAIL <<- FAIL + 1L; "FAIL" }
  cat(sprintf("[%s] %s%s\n", tag, name, if (nzchar(extra)) paste0(" — ", extra) else ""))
}
TD <- file.path(tempdir(), "tm"); dir.create(TD, showWarnings = FALSE)

mk <- function(N, EV, seed, imp, fold, shift = 0, comp = TRUE, admin = c(6, 120)) {
  set.seed(seed)
  lp <- rnorm(N); lpd <- rnorm(N)
  rr <- 0.02*exp(0.45*lp + shift); rd <- if (comp) 0.015*exp(0.30*lpd) else rep(1e-7, N)
  Tr <- rexp(N, rr); Td <- rexp(N, rd); Ca <- runif(N, admin[1], admin[2])
  rt <- pmin(Tr,Td,Ca); re <- as.integer(Tr <= pmin(Td,Ca))
  dt <- pmin(Td,Ca);    de <- as.integer(Td <= Ca)
  S <- function(rate) vapply(EV, function(tt) exp(-rate*tt), numeric(N))
  list(imp_idx=imp, fold_idx=fold, eval_times=EV, original_val_idx=seq_len(N),
       readmission=list(surv_val_matrix=S(rr), y_val=data.frame(time=rt, event=re)),
       death      =list(surv_val_matrix=S(rd), y_val=data.frame(time=dt, event=de)))
}
mkrb <- function(EV, seed0, shift = 0, comp = TRUE, admin = c(6,120))
  list(raw_predictions = lapply(1:3, function(b) mk(900L, EV, seed0+b, b, b, shift, comp, admin)),
       config = list(eval_times = EV))

EV <- c(12, 36, 60)
spec_def <- list(death = list(`36`=c(0.05)), readmission = list(`36`=c(0.15, 0.20)))

cat("\n========== threshold metrics (alt) validity ==========\n\n")

# ---- TM1 regression: complete_case == _hist where defined ----
ref <- mkrb(EV, 10, 0.0); upd <- mkrb(EV, 40, 0.3)
alt_cc <- run_threshold_metrics_from_results_boot(ref, upd, threshold_spec = spec_def,
            reference_label="A", updated_label="B", estimator="complete_case",
            output_dir=file.path(TD,"cc"), prefix="t", verbose=FALSE)
his <- hist_env$run_threshold_metrics_from_results_boot(ref, upd, threshold_spec = spec_def,
            reference_label="A", updated_label="B", output_dir=file.path(TD,"his"), prefix="t")
ka <- function(s) paste(s$Model,s$Risk,s$Time,s$Threshold,s$Metric)
a <- alt_cc$summary_long; h <- his$summary_long
a <- a[order(ka(a)),]; h <- h[order(ka(h)),]
m <- match(ka(h), ka(a))
both_fin <- is.finite(h$mean) & is.finite(a$mean[m])
diff1 <- max(abs(h$mean[both_fin] - a$mean[m][both_fin]), na.rm = TRUE)
report("TM1 complete_case == _hist where defined (regression)",
       isTRUE(diff1 < 1e-9) && mean(both_fin) > 0.8,
       sprintf("max|diff|=%.2e over %d/%d rows", diff1, sum(both_fin), length(both_fin)))

# ---- TM2 undefined metric -> NA (alt) vs 0 (_hist) ----
# no subject flagged positive (all pred < threshold) -> PPV undefined
conf <- .tm_confusion(pred = rep(0.10, 5), threshold = 0.50,
                      w_event = c(1,1,0,0,0), w_nonevent = c(0,0,1,1,1))
hist_v <- hist_env$.evaluate_with_threshold_tm(y_true = c(1L,1L,0L,0L,0L),
                                               probas = rep(0.10,5), threshold = 0.50)
report("TM2 undefined PPV -> NA (alt) vs 0 (_hist)",
       is.na(conf["PPV"]) && hist_v["PPV"] == 0 && conf["Spec"] == 1 && conf["Sens"] == 0,
       sprintf("alt PPV=%s, hist PPV=%.0f", as.character(conf["PPV"]), hist_v["PPV"]))

# ---- TM3 ipcw == complete_case when NO censoring before t (admin beyond 60) ----
ref_nc <- mkrb(EV, 70, 0.0, comp = FALSE, admin = c(200, 240))  # no deaths, censor >> 60
upd_nc <- mkrb(EV, 90, 0.3, comp = FALSE, admin = c(200, 240))
spec_d <- list(death = list(`36`=c(0.03,0.05), `60`=c(0.03,0.05)))
a_cc <- run_threshold_metrics_from_results_boot(ref_nc, upd_nc, threshold_spec=spec_d,
          estimator="complete_case", output_dir=file.path(TD,"nc_cc"), prefix="t", verbose=FALSE)
a_ip <- run_threshold_metrics_from_results_boot(ref_nc, upd_nc, threshold_spec=spec_d,
          estimator="ipcw", output_dir=file.path(TD,"nc_ip"), prefix="t", verbose=FALSE)
sc <- a_cc$summary_long[order(ka(a_cc$summary_long)),]
si <- a_ip$summary_long[order(ka(a_ip$summary_long)),]
mi <- match(ka(sc), ka(si))
diff3 <- max(abs(sc$mean - si$mean[mi]), na.rm = TRUE)
report("TM3 ipcw == complete_case when no censoring before t (sanity)",
       isTRUE(diff3 < 1e-6), sprintf("max|diff|=%.2e", diff3))

# ---- TM4 ipcw readmission counts competing deaths as controls ----
ref_c <- mkrb(EV, 11, 0.0, comp = TRUE, admin = c(6, 120))
upd_c <- mkrb(EV, 51, 0.3, comp = TRUE, admin = c(6, 120))
spec_r <- list(readmission = list(`12`=c(0.20), `36`=c(0.40), `60`=c(0.60)))  # discriminating
r_cc <- run_threshold_metrics_from_results_boot(ref_c, upd_c, threshold_spec=spec_r,
          estimator="complete_case", output_dir=file.path(TD,"r_cc"), prefix="t", verbose=FALSE)
r_ip <- run_threshold_metrics_from_results_boot(ref_c, upd_c, threshold_spec=spec_r,
          estimator="ipcw", output_dir=file.path(TD,"r_ip"), prefix="t", verbose=FALSE)
nc_cc <- mean(r_cc$raw_wide$n_controls[r_cc$raw_wide$Risk=="Readmission" & r_cc$raw_wide$horizon==60])
nc_ip <- mean(r_ip$raw_wide$n_controls[r_ip$raw_wide$Risk=="Readmission" & r_ip$raw_wide$horizon==60])
report("TM4 ipcw readmission n_controls > complete_case (competing deaths kept)",
       isTRUE(nc_ip > nc_cc + 1), sprintf("cc=%.1f vs ipcw=%.1f controls", nc_cc, nc_ip))
report("TM4b ipcw fallback == 0 (death blocks aligned)",
       isTRUE(r_ip$config$n_readmit_ipcw_fallback == 0L && r_ip$config$n_readmit_competing_risk == 6L),
       sprintf("CR=%d fallback=%d", r_ip$config$n_readmit_competing_risk, r_ip$config$n_readmit_ipcw_fallback))

# ---- TM5 competing-risk ipcw materially changes READMISSION Spec/NPV ----
# (reuse r_cc / r_ip from TM4: readmission, threshold 0.20 @ 60m). With death as
# a competing event the control set changes (efree -> efree + early deaths), so
# Spec / NPV move. For death with independent admin censoring, cc ~ ipcw (both
# approx unbiased) — the meaningful correction is the readmission competing risk.
rc <- r_cc$summary_long[r_cc$summary_long$Risk == "Readmission", ]
ri <- r_ip$summary_long[r_ip$summary_long$Risk == "Readmission", ]
rc <- rc[order(ka(rc)), ]; ri <- ri[order(ka(ri)), ]
mm <- match(ka(rc), ka(ri))
bothf <- is.finite(rc$mean) & is.finite(ri$mean[mm])
d5 <- max(abs(rc$mean[bothf] - ri$mean[mm][bothf]), na.rm = TRUE)
report("TM5 competing-risk ipcw shifts readmission metrics vs complete_case",
       isTRUE(d5 > 1e-3), sprintf("max readmission |cc-ipcw|=%.3f over %d defined metrics", d5, sum(bothf)))

# ---- TM6 end-to-end structure ----
ok6 <- is.data.frame(alt_cc$summary_wide) && nrow(alt_cc$summary_wide) > 0 &&
       all(c("NPV","PPV","Sens","Spec") %in% names(alt_cc$summary_wide)) &&
       all(c("readmission","death") %in% NA) == FALSE
report("TM6 summary_wide well-formed", ok6,
       sprintf("%d rows, cols incl NPV/PPV/Sens/Spec", nrow(alt_cc$summary_wide)))

cat(sprintf("\n========== %d passed, %d failed ==========\n", PASS, FAIL))
if (FAIL > 0L) quit(status = 1L)
