suppressMessages({library(survival); library(riskRegression); library(prodlim)})
options(future.globals.maxSize = 10 * 1024^3)
source("cons/_alt_scripts/run_validation_holdout.R")
t0 <- Sys.time()
res <- run_validation_holdout(
  eval_times       = c(3,6,9,12,24,36,48,60,72,84,96,108),
  dca_horizons     = c(6,12,36,60),
  cal_times        = c(6,12,36,60),
  run_ipeval       = TRUE,
  ipeval_bootstrap = 500L,
  test_frac        = NULL,
  out_tag          = format(Sys.Date(), "%Y_%m_%d"),
  verbose          = TRUE)
cat(sprintf("\n=== FULL RUN done in %.1f min ===\n", as.numeric(difftime(Sys.time(),t0,units="mins"))))
cat("\n== C-index/IBS GLOBAL (held-out 20%) ==\n")
print(res$cindex_ibs_global[,c("model","Risk","Metric","mean","sd","q025","q975")], row.names=FALSE)
cat("\n== Readmit calibration (held-out) ==\n")
print(res$cal_readmit$pooled_summary[,c("time_months","ici_mean","ece_mean","eo_mean","mean_pred","observed")], row.names=FALSE)
cat("\n== Death calibration best_perf1 (held-out) ==\n")
print(res$cal_death$best_perf1$pooled_summary[,c("time_months","ici_mean","ece_mean","eo_mean","mean_pred","observed")], row.names=FALSE)
cat("\n== Death calibration best_perf2 (held-out) ==\n")
print(res$cal_death$best_perf2$pooled_summary[,c("time_months","ici_mean","ece_mean","eo_mean","mean_pred","observed")], row.names=FALSE)
cat("\n== ipeval pooled ==\n")
if(!is.null(res$ipeval)) print(res$ipeval$pooled, row.names=FALSE)
cat("\nDONE\n")
