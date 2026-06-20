# =============================================================================
# validate_holdout_ipeval.R
# Bootstrap cross-check of held-out (20% validation) performance using the
# ipeval package (https://cran.r-project.org/web/packages/ipeval/, time-to-event
# vignette). Gives AUC / Brier / O:E ratio with bootstrap CIs at each horizon, to
# check CONSISTENCY against the prediction225-style estimators in
# validate_holdout_metrics.R.
#
# Method (matches cons/_alt_scripts/audit_*_cal_ipeval.R, extended to held-out):
#   * Add a random 50/50 dummy treatment (the ipeval prognostic-model trick: IPT
#     weight = 1/0.5 = 2, equivalent to upweighting a random half to the full pop).
#   * Fit the model on TRAIN (incl. dummy_trt, coef ~ 0); evaluate ip_score() on
#     the held-out VALIDATION data.
#   * Readmission is treated cause-specifically (death = censoring), matching the
#     readmit calibration audit; death is the standard outcome.
#   * Run per imputation; pool point estimates across the 5 imputations and report
#     bootstrap CIs (mean of per-imputation bounds).
#
# ipeval::ip_score(object, data, outcome, treatment_formula, treatment_of_interest,
#                  time_horizon, cens_model, bootstrap) -> model/auc/brier/oeratio
# =============================================================================

# --- Resolve the project root robustly (works from the project root or from cons/,
#     reusing a `project_root` set earlier, e.g. in the notebook). ---
if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}

# Depends on .vhm_first_event() and to01() from validate_holdout_metrics.R
local({
  if (!exists(".vhm_first_event", mode = "function") || !exists("to01", mode = "function"))
    source(file.path(project_root, "cons/_alt_scripts/validate_holdout_metrics.R"))
})

.vhi_have_ipeval <- function() requireNamespace("ipeval", quietly = TRUE)

# ONE task = one model x one imputation, all horizons. Returns a tidy data.frame.
#   risk = "readmission" (cause-specific CSC CIF) or "death" (coxph 1-S).
# We pass MY OWN strata-correct predictRisk vectors to ip_score (object = numeric
# vector), bypassing ipeval's predict_cox which mishandles stratified Cox. ipeval
# then computes IPCW AUC/Brier/O:E + bootstrap CIs on the held-out predictions.
# set.seed(seed + 1000 + imp) makes the dummy treatment AND the ip_score bootstrap
# reproducible per imputation, in serial or parallel.
.vhi_one_task <- function(model_name, fobj, tr, te, risk,
                          imp, horizons = c(6, 12, 36, 60), bootstrap = 200L,
                          seed = 2125L) {
  rhs_txt <- paste(deparse(stats::formula(stats::delete.response(stats::terms(fobj))),
                           width.cutoff = 500L), collapse = " ")
  rhs_txt <- base::gsub("strat\\(", "strata(", rhs_txt)
  rhs_txt <- base::sub("^~[[:space:]]*", "", rhs_txt)
  metrics_want <- c("auc", "brier", "oeratio")
  tr <- as.data.frame(tr); te <- as.data.frame(te)

  if (risk == "readmission") {
    fe_tr <- .vhm_first_event(tr); fe_te <- .vhm_first_event(te)
    tr$.ftime <- fe_tr$.ftime; tr$.fstatus <- fe_tr$.fstatus
    te$.y_time <- fe_te$.ftime; te$.y_event <- as.integer(fe_te$.fstatus == 1L)
    f1 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", rhs_txt))
    fit <- tryCatch(riskRegression::CSC(list(f1, f1), data = tr), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    pred_mat <- tryCatch(riskRegression::predictRisk(fit, newdata = te, times = horizons, cause = 1),
                         error = function(e) NULL)
  } else {
    te$.y_time <- as.numeric(te[["death_time_from_disch_m"]]); te$.y_event <- to01(te[["death_event"]])
    f <- stats::as.formula(paste("survival::Surv(death_time_from_disch_m, death_event) ~", rhs_txt))
    fit <- tryCatch(survival::coxph(f, data = tr, x = TRUE, y = TRUE), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    pred_mat <- tryCatch(riskRegression::predictRisk(fit, newdata = te, times = horizons),
                         error = function(e) NULL)
  }
  if (is.null(pred_mat)) return(NULL)
  pred_mat <- matrix(pmin(pmax(as.matrix(pred_mat), 0), 1), nrow = nrow(te))

  # Pin the generator so the dummy treatment + ip_score bootstrap are reproducible
  # AND identical whether run serially or in parallel (future.seed = TRUE would
  # otherwise switch the worker RNG to L'Ecuyer-CMRG and change the draws).
  set.seed(seed + 1000L + imp, kind = "Mersenne-Twister")
  te$dummy_trt <- stats::rbinom(nrow(te), 1L, 0.5)
  ord <- order(te$.y_time); te <- te[ord, , drop = FALSE]; pred_mat <- pred_mat[ord, , drop = FALSE]

  rows <- list()
  for (j in seq_along(horizons)) {
    h <- horizons[j]; pv <- pred_mat[, j]
    r <- tryCatch(
      ipeval::ip_score(
        object = stats::setNames(list(pv), model_name),
        data = te, outcome = survival::Surv(.y_time, .y_event),
        treatment_formula = dummy_trt ~ 1, treatment_of_interest = 1L,
        metrics = metrics_want, time_horizon = h, cens_model = "KM",
        null_model = FALSE, bootstrap = as.integer(bootstrap),
        bootstrap_progress = FALSE, quiet = TRUE),
      error = function(e) NULL)
    if (is.null(r)) next
    row <- data.frame(risk = risk, model = model_name, horizon = h, imp = imp, stringsAsFactors = FALSE)
    for (m in metrics_want) {
      val <- tryCatch(r$score[[m]][[model_name]], error = function(e) NA_real_)
      row[[m]] <- if (is.null(val)) NA_real_ else as.numeric(val)
      if (as.integer(bootstrap) > 1L && !is.null(r$bootstrap)) {
        ci <- tryCatch(r$bootstrap$results[[m]][[model_name]], error = function(e) NULL)
        row[[paste0(m, "_lower")]] <- if (length(ci) >= 1L) as.numeric(ci[[1]]) else NA_real_
        row[[paste0(m, "_upper")]] <- if (length(ci) >= 2L) as.numeric(ci[[2]]) else NA_real_
      }
    }
    rows[[length(rows) + 1L]] <- row
  }
  if (!length(rows)) return(NULL)
  allcols <- Reduce(union, lapply(rows, names))
  rows <- lapply(rows, function(d) { for (c0 in setdiff(allcols, names(d))) d[[c0]] <- NA; d[allcols] })
  do.call(rbind, rows)
}

# Pool across imputations: mean of auc/brier/oeratio (+ CI bounds if present)
.vhi_pool <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  num_metrics <- intersect(c("auc", "brier", "oeratio", "lower", "upper",
                             "auc_lower", "auc_upper", "brier_lower", "brier_upper",
                             "oeratio_lower", "oeratio_upper"), names(df))
  keys <- df[, c("risk", "model", "horizon"), drop = FALSE]
  spl <- split(df, interaction(df$risk, df$model, df$horizon, drop = TRUE))
  do.call(rbind, lapply(spl, function(z) {
    base <- data.frame(risk = z$risk[1], model = z$model[1], horizon = z$horizon[1],
                       n_imp = nrow(z), stringsAsFactors = FALSE)
    for (m in num_metrics) base[[paste0(m, "_mean")]] <- mean(suppressWarnings(as.numeric(z[[m]])), na.rm = TRUE)
    base
  }))
}

# Public: run ipeval cross-check for the best_perf model pair.
#   models: named list, each = list(readmit = <formula>, death = <formula>)
#   Readmission is identical across models (same readmit formula) -> computed once.
#   parallel = TRUE distributes the (model x imputation) tasks over a future plan
#   (set future::plan(multisession, workers = N) first). seed = 2125 makes the
#   bootstrap reproducible regardless of parallel/serial.
ipeval_holdout <- function(models, train_list, val_list, horizons = c(6, 12, 36, 60),
                           bootstrap = 200L, seed = 2125L, parallel = FALSE, verbose = TRUE) {
  if (!.vhi_have_ipeval())
    stop("Package 'ipeval' is not installed. Run renv::install('ipeval') first.", call. = FALSE)

  # Deduped specs: readmission once (shared formula) + one death model per entry.
  specs <- list(); readmit_done <- character(0)
  for (mn in names(models)) {
    fr <- models[[mn]]$readmit; sig <- paste(deparse(fr), collapse = " ")
    if (!(sig %in% readmit_done)) {
      readmit_done <- c(readmit_done, sig)
      specs[[length(specs) + 1L]] <- list(label = paste0("readmit::", mn), fobj = fr, risk = "readmission")
    }
  }
  for (mn in names(models))
    specs[[length(specs) + 1L]] <- list(label = paste0("death::", mn),
                                        fobj = models[[mn]]$death, risk = "death")

  # Flat task list: one (spec x imputation) per element -> the unit of parallelism.
  nimp <- length(train_list)
  tasks <- list()
  for (s in specs) for (i in seq_len(nimp)) tasks[[length(tasks) + 1L]] <- list(spec = s, imp = i)

  run_task <- function(tk)
    .vhi_one_task(tk$spec$label, tk$spec$fobj, train_list[[tk$imp]], val_list[[tk$imp]],
                  tk$spec$risk, tk$imp, horizons, bootstrap, seed)

  use_par <- isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)
  if (verbose) cat(sprintf("ipeval: %d tasks (%d models x %d imputations), B=%d, parallel=%s\n",
                           length(tasks), length(specs), nimp, bootstrap, use_par))
  res <- if (use_par)
    future.apply::future_lapply(
      tasks, run_task, future.seed = TRUE,
      future.packages = c("survival", "riskRegression", "prodlim", "ipeval", "stats"))
  else
    lapply(tasks, run_task)

  ok <- !vapply(res, is.null, logical(1L))
  if (verbose && any(!ok))
    cat(sprintf("  WARNING: %d/%d ipeval tasks returned NULL (no result).\n", sum(!ok), length(res)))
  per_imp_df <- do.call(rbind, res[ok])
  if (is.null(per_imp_df) || !nrow(per_imp_df))
    stop("All ipeval tasks returned NULL. Check that survival/riskRegression/prodlim/ipeval ",
         "are installed and that train/val contain the model variables.", call. = FALSE)
  list(per_imputation = per_imp_df, pooled = .vhi_pool(per_imp_df),
       config = list(horizons = horizons, bootstrap = bootstrap, seed = seed,
                     parallel = use_par, n_tasks = length(tasks),
                     method = "ipeval ip_score, dummy-treatment IPW, held-out 20%",
                     timestamp = as.character(Sys.time())))
}
