# =============================================================================
# validate_holdout_metrics.R
# Held-out (20% validation) evaluation engine for prediction23.
#
# Fits the frozen best_perf models on the 80% TRAIN and evaluates on the held-out
# 20% VALIDATION set, reusing the EXACT prediction225 estimators:
#   * C-index (Uno) + IBS (IPCW)  -> reuses .dual_cox_fit_one_risk internals from
#                                    evaluate_dual_cox_python_style_boot.R
#   * DCA (net benefit)           -> produces a results_boot object consumed by
#                                    run_adca_from_results_boot (AJ readmission /
#                                    KM death), unchanged.
#   * Calibration                 -> readmission: CSC + predictRisk(cause=1) ->
#                                    Aalen-Johansen CIF deciles + loess ICI;
#                                    death: coxph + predictRisk -> KM deciles +
#                                    loess ICI. Same math as the calibrate_*
#                                    engines, applied to held-out predictions.
#
# Fit-on-train / predict-on-val per imputation; pool across the 5 imputations
# (mean / sd / 2.5-97.5% percentiles), mirroring how the CV engines pool across
# repetitions. Output objects are drop-in compatible with prediction23's cells.
#
# Companion: validate_holdout_ipeval.R adds the ipeval bootstrap cross-check.
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

local({
  if (!exists("evaluate_dual_cox_python_style", mode = "function") ||
      !exists(".dual_cox_fit_one_risk", mode = "function")) {
    source(file.path(project_root, "cons/_alt_scripts/evaluate_dual_cox_python_style_boot.R"))
  }
})

suppressWarnings({
  stopifnot(requireNamespace("survival",       quietly = TRUE))
  stopifnot(requireNamespace("riskRegression", quietly = TRUE))
  stopifnot(requireNamespace("prodlim",        quietly = TRUE))
})

# -----------------------------------------------------------------------------
# 1. C-index + IBS + raw predictions on the held-out validation set
#    Same output structure as evaluate_dual_cox_python_style(), but fit-on-train
#    / predict-on-val instead of k-fold CV. Each imputation = one replicate.
# -----------------------------------------------------------------------------
evaluate_dual_cox_holdout <- function(
    formula_readmit,
    formula_death,
    train_list,
    val_list,
    readmit_time_col  = "readmit_time_from_disch_m",
    readmit_event_col = "readmit_event",
    death_time_col    = "death_time_from_disch_m",
    death_event_col   = "death_event",
    eval_times        = c(3, 6, 9, 12, 24, 36, 48, 60, 72, 84, 96, 108),
    min_test_rows     = 10L,
    verbose           = TRUE
) {
  stopifnot(length(train_list) == length(val_list))
  eval_times <- sort(unique(as.numeric(eval_times)))
  n_imp <- length(train_list)

  to_clean <- function(dl) lapply(dl, function(df) {
    df <- as.data.frame(df)
    df[[readmit_time_col]] <- as.numeric(df[[readmit_time_col]])
    df[[death_time_col]]   <- as.numeric(df[[death_time_col]])
    df[[readmit_event_col]] <- to01(df[[readmit_event_col]])
    df[[death_event_col]]   <- to01(df[[death_event_col]])
    df
  })
  train_list <- to_clean(train_list)
  val_list   <- to_clean(val_list)

  # Formula metadata derived from the union (level consistency across train+val).
  meta_readmit <- .dual_cox_prepare_formula_metadata(formula_readmit, c(train_list, val_list))
  meta_death   <- .dual_cox_prepare_formula_metadata(formula_death,   c(train_list, val_list))

  metrics_log <- list(); raw_predictions <- list(); failures_log <- list()
  mi <- 0L; ri <- 0L; fi <- 0L

  add_metric <- function(imp, risk, metric, time, value) {
    mi <<- mi + 1L
    metrics_log[[mi]] <<- data.frame(Imp = imp, Fold = 1L, Risk = risk,
      Metric = metric, Time = time, Value = value, stringsAsFactors = FALSE)
  }

  if (verbose) cat(sprintf("Held-out dual Cox: %d imputations, fit train -> predict val\n", n_imp))

  for (i in seq_len(n_imp)) {
    df_tr <- train_list[[i]]; df_te <- val_list[[i]]
    readmit_res <- tryCatch(.dual_cox_fit_one_risk(df_tr, df_te, meta_readmit,
        readmit_time_col, readmit_event_col, eval_times, min_test_rows),
      error = function(e) list(error = paste0("holdout_error:", conditionMessage(e)), dropped = 0L))
    death_res <- tryCatch(.dual_cox_fit_one_risk(df_tr, df_te, meta_death,
        death_time_col, death_event_col, eval_times, min_test_rows),
      error = function(e) list(error = paste0("holdout_error:", conditionMessage(e)), dropped = 0L))

    for (rr in list(list(res = readmit_res, lbl = "Readmission"),
                    list(res = death_res,   lbl = "Death"))) {
      res <- rr$res
      if (nzchar(res$error)) {
        fi <- fi + 1L
        failures_log[[fi]] <- data.frame(Imp = i, Fold = 1L, Risk = rr$lbl,
          Error = res$error, stringsAsFactors = FALSE)
      } else {
        add_metric(i, rr$lbl, "Uno's C-Index", "Global", res$c_global)
        add_metric(i, rr$lbl, "IBS",           "Global", res$ibs_global)
        for (tl in names(res$c_horizon))   add_metric(i, rr$lbl, "Uno's C-Index", tl, unname(res$c_horizon[[tl]]))
        for (tl in names(res$ibs_horizon)) add_metric(i, rr$lbl, "IBS",           tl, unname(res$ibs_horizon[[tl]]))
      }
    }

    ri <- ri + 1L
    raw_predictions[[ri]] <- list(
      imp_idx = i, fold_idx = 1L, original_val_idx = seq_len(nrow(df_te)),
      eval_times = eval_times,
      readmission = if (nzchar(readmit_res$error)) list(error = readmit_res$error) else list(
        lp_train = readmit_res$lp_train, lp_val = readmit_res$lp_val,
        surv_val_matrix = readmit_res$surv_val,
        y_train = readmit_res$y_train, y_val = readmit_res$y_val,
        dropped_rows = readmit_res$dropped),
      death = if (nzchar(death_res$error)) list(error = death_res$error) else list(
        lp_train = death_res$lp_train, lp_val = death_res$lp_val,
        surv_val_matrix = death_res$surv_val,
        y_train = death_res$y_train, y_val = death_res$y_val,
        dropped_rows = death_res$dropped))
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")

  metrics_df <- if (length(metrics_log)) do.call(rbind, metrics_log) else
    data.frame(Imp=integer(), Fold=integer(), Risk=character(), Metric=character(),
               Time=character(), Value=numeric(), stringsAsFactors = FALSE)
  failures_df <- if (length(failures_log)) do.call(rbind, failures_log) else
    data.frame(Imp=integer(), Fold=integer(), Risk=character(), Error=character(),
               stringsAsFactors = FALSE)

  list(
    metrics = metrics_df,
    summary = .dual_cox_summarize_metrics(metrics_df),
    raw_predictions = raw_predictions,
    failures = failures_df,
    config = list(
      formula_readmit = meta_readmit$formula_text,
      formula_death = meta_death$formula_text,
      readmit_time_col = readmit_time_col, readmit_event_col = readmit_event_col,
      death_time_col = death_time_col, death_event_col = death_event_col,
      eval_times = eval_times, k_folds = 1L, seed = NA_integer_,
      plan_dummy_cols = c("plan_type_corr_pg_pr","plan_type_corr_m_pr",
                          "plan_type_corr_pg_pai","plan_type_corr_m_pai"),
      plan_strata_fallback_col = "plan_type_strata",
      min_test_rows = min_test_rows, validation = "holdout-20pct",
      timestamp = as.character(Sys.time())
    )
  )
}

# -----------------------------------------------------------------------------
# 2. Shared calibration helpers (mirror calibrate_*_grouped engines exactly)
# -----------------------------------------------------------------------------
.vhm_run_stats <- function(x, probs = c(0.025, 0.975)) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(mean=NA, sd=NA, p025=NA, p975=NA, min=NA, max=NA, n=0))
  c(mean = mean(x), sd = stats::sd(x),
    p025 = unname(stats::quantile(x, probs[1], na.rm = TRUE, type = 8)),
    p975 = unname(stats::quantile(x, probs[2], na.rm = TRUE, type = 8)),
    min = min(x), max = max(x), n = length(x))
}
.vhm_deciles <- function(pred, g = 10L) {
  cuts <- unique(stats::quantile(pred, probs = seq(0, 1, length.out = g + 1L),
                                 na.rm = TRUE, names = FALSE, type = 8))
  cuts <- cuts[-c(1L, length(cuts))]
  if (length(cuts)) findInterval(pred, cuts, all.inside = TRUE) + 1L else rep(1L, length(pred))
}
.vhm_state_position <- function(pstate_obj, states, target) {
  cn <- if (is.null(dim(pstate_obj))) names(pstate_obj) else colnames(pstate_obj)
  if (!is.null(cn)) { idx <- match(target, cn); if (!is.na(idx)) return(idx) }
  idx <- match(target, states)
  if (is.na(idx)) stop("state '", target, "' not found")
  ncols <- if (is.null(dim(pstate_obj))) length(pstate_obj) else ncol(pstate_obj)
  if (ncols == length(states) + 1L) return(idx + 1L)
  idx
}
.vhm_aj_cif <- function(ftime, fstatus, horizon) {
  sf <- factor(fstatus, levels = 0:2, labels = c("censor", "readmit", "death"))
  fit <- survival::survfit(survival::Surv(ftime, sf) ~ 1)
  s <- summary(fit, times = horizon, extend = TRUE)
  if (!length(s$pstate)) return(0)
  pos <- .vhm_state_position(s$pstate, fit$states, "readmit")
  val <- if (is.null(dim(s$pstate))) s$pstate[pos] else s$pstate[1, pos]
  min(max(as.numeric(val), 0), 1)
}
.vhm_km_risk <- function(time, event, horizon) {
  fit <- survival::survfit(survival::Surv(time, event) ~ 1)
  s <- summary(fit, times = horizon, extend = TRUE)$surv
  if (length(s)) 1 - s[[1L]] else NA_real_
}
.vhm_ici_loess <- function(pred, obs_individual, span = 0.75) {
  valid <- is.finite(pred) & is.finite(obs_individual)
  if (sum(valid) < 10L) return(NA_real_)
  lo <- tryCatch(stats::loess(obs_individual[valid] ~ pred[valid], span = span,
                              control = stats::loess.control(surface = "interpolate")),
                 error = function(e) NULL)
  if (is.null(lo)) return(NA_real_)
  mean(abs(stats::fitted(lo) - pred[valid]), na.rm = TRUE)
}
# First-event competing-risk status (0 censor / 1 readmit / 2 death), as in the
# AJ DCA + readmission calibration engines.
.vhm_first_event <- function(df, rt = "readmit_time_from_disch_m", re = "readmit_event",
                             dt = "death_time_from_disch_m", de = "death_event") {
  tr <- as.numeric(df[[rt]]); td <- as.numeric(df[[dt]])
  er <- to01(df[[re]]);       ed <- to01(df[[de]])
  ftime <- pmin(tr, td)
  r1 <- !is.na(er) & er == 1L & is.finite(ftime) & abs(tr - ftime) <= 1e-8
  d1 <- !is.na(ed) & ed == 1L & is.finite(ftime) & abs(td - ftime) <= 1e-8
  fstatus <- rep(0L, length(ftime))
  fstatus[r1 & !d1] <- 1L
  fstatus[d1 & !r1] <- 2L
  fstatus[r1 & d1]  <- 2L  # ties -> death-first (engine default)
  data.frame(.ftime = ftime, .fstatus = fstatus)
}
# Compute one calibration curve + metrics for one prediction vector at one horizon
.vhm_calibrate_one <- function(pred, horizon, observed_overall, observed_by_bin_fun,
                               g = 10L, min_bin_n = 20L, span = 0.75) {
  bins <- .vhm_deciles(pred, g)
  ub <- sort(unique(bins))
  rows <- lapply(ub, function(b) {
    idx <- which(bins == b)
    if (length(idx) < min_bin_n) return(NULL)
    data.frame(bin = b, mean_predicted = mean(pred[idx], na.rm = TRUE),
               observed = observed_by_bin_fun(idx), n_patients = length(idx))
  })
  curve <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(curve) || !nrow(curve)) {
    return(list(metrics = data.frame(time_months = horizon, ici = NA, ece = NA,
      eo_ratio = NA, mean_predicted = mean(pred, na.rm=TRUE), observed = observed_overall,
      n_bins = 0L), curve = NULL))
  }
  obs_lookup <- stats::setNames(curve$observed, as.character(curve$bin))
  obs_indiv  <- as.numeric(obs_lookup[as.character(bins)])
  ece <- stats::weighted.mean(abs(curve$mean_predicted - curve$observed),
                              w = curve$n_patients, na.rm = TRUE)
  ici <- .vhm_ici_loess(pred, obs_indiv, span)
  mp  <- mean(pred, na.rm = TRUE)
  eo  <- if (is.finite(observed_overall) && observed_overall > 0) mp / observed_overall else NA_real_
  list(
    metrics = data.frame(time_months = horizon, ici = ici, ece = ece, eo_ratio = eo,
                         mean_predicted = mp, observed = observed_overall,
                         n_bins = nrow(curve)),
    curve = data.frame(time_months = horizon, bin = curve$bin,
                       mean_predicted = curve$mean_predicted, observed = curve$observed,
                       n_patients = curve$n_patients)
  )
}
# Pool per-imputation metrics + curves into the calibrate_* output shape
.vhm_pool <- function(metrics_all, curves_all, n_imp) {
  ms <- do.call(rbind, metrics_all)
  pooled <- do.call(rbind, lapply(sort(unique(ms$time_months)), function(tt) {
    z <- ms[ms$time_months == tt, , drop = FALSE]
    ic <- .vhm_run_stats(z$ici); ec <- .vhm_run_stats(z$ece); eo <- .vhm_run_stats(z$eo_ratio)
    mp <- .vhm_run_stats(z$mean_predicted); ob <- .vhm_run_stats(z$observed)
    data.frame(time_months = tt,
      ici_mean = ic["mean"], ici_sd = ic["sd"], ici_p025 = ic["p025"], ici_p975 = ic["p975"],
      ece_mean = ec["mean"], ece_sd = ec["sd"],
      eo_mean = eo["mean"], eo_sd = eo["sd"],
      mean_pred = mp["mean"], observed = ob["mean"], n_imp = n_imp, row.names = NULL)
  }))
  curves_df <- do.call(rbind, curves_all)
  cal_curves <- NULL
  if (!is.null(curves_df) && nrow(curves_df)) {
    key <- paste(curves_df$time_months, curves_df$bin, sep = "___")
    cal_curves <- do.call(rbind, lapply(split(curves_df, key), function(z) {
      mp <- .vhm_run_stats(z$mean_predicted); ob <- .vhm_run_stats(z$observed)
      data.frame(time_months = z$time_months[1], bin = z$bin[1],
        mean_predicted = mp["mean"], observed = ob["mean"],
        observed_sd = ob["sd"], observed_lower = ob["p025"], observed_upper = ob["p975"],
        n_patients = sum(z$n_patients), n_reps = nrow(z), row.names = NULL)
    }))
    cal_curves <- cal_curves[order(cal_curves$time_months, cal_curves$mean_predicted), ]
    rownames(cal_curves) <- NULL
  }
  list(pooled_summary = pooled, calibration_curves = cal_curves,
       flat = ms)
}

# -----------------------------------------------------------------------------
# 3. Readmission calibration on held-out val (CSC -> AJ-CIF), pooled over imputations
# -----------------------------------------------------------------------------
calibrate_readmit_holdout <- function(
    formula_readmit, formula_death, train_list, val_list, times = c(6, 12, 36, 60),
    g = 10L, min_bin_n = 20L, span = 0.75, verbose = TRUE) {
  # `formula_death` is REQUIRED (no default) on purpose: before 2026-07-15
  # this function silently set f2 <- f1, i.e. the cause-2 (death) sub-model
  # of the joint CSC fit reused the READMISSION covariates, because no
  # caller ever had to supply a death formula. That mis-specified the
  # predicted CIF (it conditioned on the wrong death risk factors). Forcing
  # an explicit argument here, instead of a default that falls back to f1,
  # makes that bug impossible to repeat by omission: a missing argument now
  # throws instead of silently miscomputing.
  .readmit_calib_rhs <- function(f) {
    txt <- paste(deparse(formula(stats::delete.response(stats::terms(f))),
                         width.cutoff = 500L), collapse = " ")
    txt <- gsub("strat\\(", "strata(", txt)
    sub("^~\\s*", "", txt)
  }
  f1 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", .readmit_calib_rhs(formula_readmit)))
  f2 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", .readmit_calib_rhs(formula_death)))

  metrics_all <- list(); curves_all <- list()
  for (i in seq_along(train_list)) {
    tr <- as.data.frame(train_list[[i]]); te <- as.data.frame(val_list[[i]])
    tr <- cbind(tr, .vhm_first_event(tr)); te <- cbind(te, .vhm_first_event(te))
    fit <- tryCatch(riskRegression::CSC(list(f1, f2), data = tr),
                    error = function(e) { if (verbose) cat("  CSC fail imp", i, ":", conditionMessage(e), "\n"); NULL })
    if (is.null(fit)) next
    pred_mat <- tryCatch(riskRegression::predictRisk(fit, newdata = te, times = times, cause = 1),
                         error = function(e) { if (verbose) cat("  predictRisk fail imp", i, ":", conditionMessage(e), "\n"); NULL })
    if (is.null(pred_mat)) next
    pred_mat <- matrix(pmin(pmax(as.matrix(pred_mat), 0), 1), nrow = nrow(te))
    for (j in seq_along(times)) {
      h <- times[j]; pv <- pred_mat[, j]
      obs_overall <- .vhm_aj_cif(te$.ftime, te$.fstatus, h)
      res <- .vhm_calibrate_one(pv, h, obs_overall,
        observed_by_bin_fun = function(idx) .vhm_aj_cif(te$.ftime[idx], te$.fstatus[idx], h),
        g = g, min_bin_n = min_bin_n, span = span)
      res$metrics$imp <- i; metrics_all[[length(metrics_all) + 1L]] <- res$metrics
      if (!is.null(res$curve)) curves_all[[length(curves_all) + 1L]] <- res$curve
    }
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  out <- .vhm_pool(metrics_all, curves_all, length(train_list))
  out$risk <- "readmission"; out$method <- "CSC + Aalen-Johansen CIF (held-out 20%)"
  out
}

# -----------------------------------------------------------------------------
# 4. Death calibration on held-out val (coxph -> KM), pooled over imputations
# -----------------------------------------------------------------------------
calibrate_death_holdout <- function(
    formula_death, train_list, val_list, times = c(6, 12, 36, 60),
    time_col = "death_time_from_disch_m", event_col = "death_event",
    g = 10L, min_bin_n = 20L, span = 0.75, verbose = TRUE) {

  f <- stats::as.formula(gsub("strat\\(", "strata(",
        paste(deparse(formula_death, width.cutoff = 500L), collapse = " ")))
  metrics_all <- list(); curves_all <- list()
  for (i in seq_along(train_list)) {
    tr <- as.data.frame(train_list[[i]]); te <- as.data.frame(val_list[[i]])
    fit <- tryCatch(survival::coxph(f, data = tr, x = TRUE, y = TRUE),
                    error = function(e) { if (verbose) cat("  coxph fail imp", i, ":", conditionMessage(e), "\n"); NULL })
    if (is.null(fit)) next
    pred_mat <- tryCatch(riskRegression::predictRisk(fit, newdata = te, times = times),
                         error = function(e) { if (verbose) cat("  predictRisk fail imp", i, ":", conditionMessage(e), "\n"); NULL })
    if (is.null(pred_mat)) next
    pred_mat <- matrix(pmin(pmax(as.matrix(pred_mat), 0), 1), nrow = nrow(te))
    tt_v <- as.numeric(te[[time_col]]); ev_v <- to01(te[[event_col]])
    for (j in seq_along(times)) {
      h <- times[j]; pv <- pred_mat[, j]
      obs_overall <- .vhm_km_risk(tt_v, ev_v, h)
      res <- .vhm_calibrate_one(pv, h, obs_overall,
        observed_by_bin_fun = function(idx) .vhm_km_risk(tt_v[idx], ev_v[idx], h),
        g = g, min_bin_n = min_bin_n, span = span)
      res$metrics$imp <- i; metrics_all[[length(metrics_all) + 1L]] <- res$metrics
      if (!is.null(res$curve)) curves_all[[length(curves_all) + 1L]] <- res$curve
    }
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  out <- .vhm_pool(metrics_all, curves_all, length(train_list))
  out$risk <- "death"; out$method <- "coxph + Kaplan-Meier (held-out 20%)"
  out
}

# -----------------------------------------------------------------------------
# 5. Bootstrap CIs for the calibration indices (ICI / ECE / E:O) on the held-out
#    validation set -- the calibration counterpart to the ipeval AUC/Brier/O:E
#    bootstrap. Predictions are MI-pooled across the 5 imputations (model fixed),
#    then the validation ROWS are resampled B times (same logic as ipeval: the
#    evaluation data is bootstrapped, not the fit). Readmission uses CSC ->
#    Aalen-Johansen CIF; mortality uses coxph -> Kaplan-Meier. Same loess-ICI
#    (span 0.75), n-weighted ECE and E:O as the point-estimate engines.
#
# Returns a tidy data.frame: model, risk, horizon, and <metric>/<metric>_lo/_hi
# for metric in {ici, ece, eo}, where the point estimate is from the full sample
# and the CI is the 2.5/97.5% bootstrap percentile.
bootstrap_calibration_indices <- function(
    formula, train_list, val_list, risk = c("readmission", "death"),
    times = c(6, 12, 36, 60), B = 500L, g = 10L, min_bin_n = 20L, span = 0.75,
    seed = 2125L, model_label = "model", parallel = FALSE, verbose = TRUE,
    formula_death = NULL) {
  risk <- match.arg(risk)
  # `formula_death` is REQUIRED when risk == "readmission": the CSC cause-2
  # (death) sub-model that builds the joint CIF must use the real death
  # formula, not the readmission one. Before 2026-07-15 this branch did
  # CSC(list(f1, f1), ...), silently reusing readmission covariates for
  # death (same bug as calibrate_readmit_holdout(), fixed there the same
  # day). `formula_death` is ignored (may be left NULL) when risk == "death".
  if (identical(risk, "readmission")) {
    stopifnot(
      "formula_death is required when risk == 'readmission' (no silent f1,f1 fallback)" =
        !is.null(formula_death)
    )
  }
  .bci_rhs <- function(f) {
    txt <- paste(deparse(stats::formula(stats::delete.response(stats::terms(f))),
                         width.cutoff = 500L), collapse = " ")
    txt <- base::gsub("strat\\(", "strata(", txt)
    base::sub("^~[[:space:]]*", "", txt)
  }
  rhs_txt <- .bci_rhs(formula)

  nimp <- length(train_list); n_val <- nrow(as.data.frame(val_list[[1]])); ntimes <- length(times)

  # --- MI-pooled predicted risk on the validation set (model fixed) ---
  pred_sum <- matrix(0, n_val, ntimes)
  for (i in seq_len(nimp)) {
    tr <- as.data.frame(train_list[[i]]); te <- as.data.frame(val_list[[i]])
    if (risk == "readmission") {
      fe <- .vhm_first_event(tr); tr$.ftime <- fe$.ftime; tr$.fstatus <- fe$.fstatus
      f1 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", rhs_txt))
      f2 <- stats::as.formula(paste("prodlim::Hist(.ftime, .fstatus) ~", .bci_rhs(formula_death)))
      fit <- riskRegression::CSC(list(f1, f2), data = tr)
      pm  <- riskRegression::predictRisk(fit, newdata = te, times = times, cause = 1)
    } else {
      f   <- stats::as.formula(paste("survival::Surv(death_time_from_disch_m, death_event) ~", rhs_txt))
      fit <- survival::coxph(f, data = tr, x = TRUE, y = TRUE)
      pm  <- riskRegression::predictRisk(fit, newdata = te, times = times)
    }
    pred_sum <- pred_sum + matrix(pmin(pmax(as.matrix(pm), 0), 1), n_val, ntimes)
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  pred_mat <- pred_sum / nimp

  # --- Validation outcomes (identical across imputations) ---
  v1 <- as.data.frame(val_list[[1]])
  if (risk == "readmission") {
    fev <- .vhm_first_event(v1); ftime <- fev$.ftime; fstatus <- fev$.fstatus
    obs_overall <- function(ft, fs, h) .vhm_aj_cif(ft, fs, h)
  } else {
    ftime <- as.numeric(v1$death_time_from_disch_m); fstatus <- to01(v1$death_event)
    obs_overall <- function(ft, fs, h) .vhm_km_risk(ft, fs, h)
  }
  # one calibration pass on an (optionally resampled) set, returning c(ici, ece, eo)
  cal_pass <- function(pred, ft, fs, h) {
    m <- .vhm_calibrate_one(pred, h, obs_overall(ft, fs, h),
           observed_by_bin_fun = function(ii) obs_overall(ft[ii], fs[ii], h),
           g = g, min_bin_n = min_bin_n, span = span)$metrics
    c(ici = m$ici, ece = m$ece, eo = m$eo_ratio)
  }

  # 2026-06-22: seed INSIDE the per-replicate function so the bootstrap resamples, and hence
  # the ICI/ECE/E:O percentile CIs, are IDENTICAL whether this runs serial or parallel.
  # Before, the serial branch seeded once per horizon (Mersenne-Twister) while the parallel
  # branch used independent L'Ecuyer streams (future.seed = seed + j), so the CIs were not
  # reproducible across backends (the point estimates were never affected). This mirrors the
  # seeding of bootstrap_threshold_metrics_holdout (validated serial == parallel).
  # NOTE for the notebook (holdout-ici-bootstrap-run): re-running regenerates the calibration
  # CIs ONCE (resamples re-mixed) relative to any previously saved ici_boot; points do not move.
  one_rep <- function(b, pj, h, j) {
    set.seed(seed + j * 100000L + b, kind = "Mersenne-Twister")
    idx <- sample.int(n_val, n_val, replace = TRUE)
    cal_pass(pj[idx], ftime[idx], fstatus[idx], h)
  }
  use_par <- isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)
  rows <- lapply(seq_along(times), function(j) {
    h  <- times[j]; pj <- pred_mat[, j]
    pt <- cal_pass(pj, ftime, fstatus, h)                       # point estimate (full sample)
    bm <- if (use_par) {
      future.apply::future_sapply(seq_len(B), one_rep, pj = pj, h = h, j = j, future.seed = TRUE)
    } else {
      vapply(seq_len(B), one_rep, numeric(3), pj = pj, h = h, j = j)
    }
    qi <- function(x) stats::quantile(x, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    data.frame(model = model_label, risk = risk, horizon = h,
      ici = pt["ici"], ici_lo = qi(bm["ici", ])[1], ici_hi = qi(bm["ici", ])[2],
      ece = pt["ece"], ece_lo = qi(bm["ece", ])[1], ece_hi = qi(bm["ece", ])[2],
      eo  = pt["eo"],  eo_lo  = qi(bm["eo", ])[1],  eo_hi  = qi(bm["eo", ])[2],
      B = B, row.names = NULL)
  })
  do.call(rbind, rows)
}

# Convenience wrapper: bootstrap calibration indices for the held-out best_perf
# models (readmission computed once; mortality for each death model).
ici_bootstrap_holdout <- function(models, train_list, val_list, times = c(6, 12, 36, 60),
                                  B = 500L, seed = 2125L, parallel = FALSE, verbose = TRUE) {
  # 2026-07-15: DEATH ONLY. Readmission calibration (net risk AND both joint-
  # CIF pairings) is now bootstrapped via
  # .holdout_bootstrap_calibration_readmit_from_raw() in holdout_cif_cache.R,
  # which reads the already-frozen predictions from raw_predictions /
  # the CIF cache instead of refitting CSC per imputation here. Keeping a
  # second, refit-based readmission bootstrap in this function would fit the
  # identical joint model a second time on the full development set for no
  # benefit (same numbers, confirmed to match exactly by cross-check in the
  # 2026-07-15 review session). bootstrap_calibration_indices()'s
  # `risk = "readmission"` branch (and its required `formula_death` guard)
  # is left in place for standalone/other use, just not called from here.
  out <- list()
  for (mn in names(models)) {
    if (verbose) cat(sprintf("ICI bootstrap: death [%s]\n", mn))
    out[[length(out) + 1L]] <- bootstrap_calibration_indices(models[[mn]]$death, train_list,
      val_list, "death", times, B, seed = seed, model_label = paste0("death::", mn),
      parallel = parallel, verbose = verbose)
  }
  do.call(rbind, out)
}

# -----------------------------------------------------------------------------
# 6. Bootstrap threshold-dependent metrics (Sens / Spec / PPV / NPV) on the
#    held-out validation set, AJ/competing-risk-aware for readmission.
#    Reuses the EXACT estimator from threshold_metrics_from_results_boot.R:
#      predicted risk = 1 - surv_val (cause-specific Cox); observed side via IPCW
#      (readmission: .tm_cr_ipcw_weights, deaths-before-t weighted as controls;
#       death: .tm_admin_ipcw_weights) -> .tm_confusion.
#    Predictions are MI-pooled across imputations (model fixed); the validation
#    rows are then resampled B times. set.seed(seed + b, "Mersenne-Twister") makes
#    it reproducible AND identical serial/parallel.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

bootstrap_threshold_metrics_holdout <- function(
    results_boot_val, model_label = "model",
    risks = c("readmission", "death"),
    horizons = c(6, 12, 36, 60),
    thresholds = list(readmission = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.40),
                      death       = c(0.01, 0.02, 0.03, 0.05, 0.075, 0.10)),
    metrics = c("Sens", "Spec", "PPV", "NPV"),
    central = c("plugin", "median", "mean"),
    ci_method = c("percentile", "bca"),
    freeze_g = TRUE,
    B = 500L, seed = 2125L, g_min = 0.05, tie_action = "death_first",
    parallel = FALSE, verbose = TRUE) {

  central <- match.arg(central)
  ci_method <- match.arg(ci_method)
  if (!exists(".tm_confusion", mode = "function") ||
      !exists(".tm_cr_ipcw_weights", mode = "function"))
    source(file.path(project_root, "cons/_alt_scripts/threshold_metrics_from_results_boot.R"))

  rp <- results_boot_val$raw_predictions
  eval_times <- as.numeric(results_boot_val$config$eval_times %||% rp[[1]]$eval_times)

  # MI-pool predicted risk per risk + assemble outcomes (from replicate 1)
  pool <- list()
  for (risk in risks) {
    blk <- Filter(function(b) is.list(b) && is.null(b$error) && !is.null(b$surv_val_matrix),
                  lapply(rp, function(it) it[[risk]]))
    nrs <- vapply(blk, function(b) nrow(as.matrix(b$surv_val_matrix)), integer(1))
    if (length(unique(nrs)) != 1L)
      stop("Imputation blocks for '", risk, "' have different row counts; cannot MI-pool.", call. = FALSE)
    surv_sum <- Reduce(`+`, lapply(blk, function(b) as.matrix(b$surv_val_matrix)))
    pred_mat <- 1 - surv_sum / length(blk)
    yv <- rp[[1]][[risk]]$y_val
    if (risk == "readmission") {
      dv <- rp[[1]][["death"]]$y_val
      cr <- .tm_cr_reconstruct(yv$time, yv$event, dv$time, dv$event, tie_action = tie_action)
      pool[[risk]] <- list(pred = pred_mat, ftime = cr$ftime, fstatus = cr$fstatus, type = "cr")
    } else {
      pool[[risk]] <- list(pred = pred_mat, time = as.numeric(yv$time),
                           event = as.integer(yv$event), type = "km")
    }
  }
  n <- nrow(pool[[risks[1]]]$pred)

  # freeze_g = TRUE: the IPCW censoring distribution G is part of the FROZEN held-out
  # estimator, so estimate it ONCE on the full sample and only resample patients. This
  # keeps the bootstrap coherent with the full-sample plug-in point (otherwise re-estimating
  # G per resample shifts the bootstrap below the plug-in, leaving the point marginally
  # outside its own percentile interval for rare-event metrics like death sensitivity).
  Wfull <- list()
  if (isTRUE(freeze_g)) {
    for (risk in risks) {
      P <- pool[[risk]]
      for (h in horizons) {
        hj <- which(eval_times == h); if (!length(hj)) next
        Wfull[[paste(risk, h, sep = "|")]] <-
          if (P$type == "cr") .tm_cr_ipcw_weights(P$ftime, P$fstatus, h, g_min = g_min)
          else                .tm_admin_ipcw_weights(P$time, P$event, h, g_min = g_min)
      }
    }
  }

  one_rep <- function(idx) {
    out <- list()
    for (risk in risks) {
      P <- pool[[risk]]; thr <- thresholds[[risk]]
      for (h in horizons) {
        hj <- which(eval_times == h); if (!length(hj)) next
        pr <- P$pred[idx, hj[1]]
        if (isTRUE(freeze_g)) {
          wf <- Wfull[[paste(risk, h, sep = "|")]]
          we <- wf$w_event[idx]; wn <- wf$w_nonevent[idx]      # frozen weights, just indexed
        } else {
          w <- if (P$type == "cr") .tm_cr_ipcw_weights(P$ftime[idx], P$fstatus[idx], h, g_min = g_min)
               else                .tm_admin_ipcw_weights(P$time[idx], P$event[idx], h, g_min = g_min)
          we <- w$w_event; wn <- w$w_nonevent
        }
        for (t in thr) {
          cm <- .tm_confusion(pr, t, we, wn)
          out[[length(out) + 1L]] <- data.frame(risk = risk, horizon = h, threshold = t,
            as.list(cm[metrics]), check.names = FALSE, stringsAsFactors = FALSE)
        }
      }
    }
    do.call(rbind, out)
  }

  point <- one_rep(seq_len(n))                       # point estimate (full sample)
  rep_fun <- function(b) {
    set.seed(seed + b, kind = "Mersenne-Twister")
    one_rep(sample.int(n, n, replace = TRUE))
  }
  use_par <- isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)
  if (verbose) cat(sprintf("threshold bootstrap [%s]: B=%d, parallel=%s\n", model_label, B, use_par))
  boots <- if (use_par)
    future.apply::future_lapply(seq_len(B), rep_fun, future.seed = TRUE,
      future.packages = c("survival"))
  else lapply(seq_len(B), rep_fun)

  allb <- do.call(rbind, boots)
  k_all <- paste(allb$risk, allb$horizon, allb$threshold, sep = "|")
  k_pt  <- paste(point$risk, point$horizon, point$threshold, sep = "|")
  res <- point
  bq    <- function(mt, p) as.numeric(tapply(allb[[mt]], k_all,
             function(x) stats::quantile(x, p, na.rm = TRUE, names = FALSE))[k_pt])
  bmean <- function(mt)    as.numeric(tapply(allb[[mt]], k_all,
             function(x) mean(x, na.rm = TRUE))[k_pt])
  for (mt in metrics) {
    res[[paste0(mt, "_plugin")]] <- res[[mt]]      # full-sample plug-in (the original point)
    res[[paste0(mt, "_med")]]    <- bq(mt, 0.5)    # bootstrap median: inside [lo, hi] by construction
    res[[paste0(mt, "_mean")]]   <- bmean(mt)      # bootstrap mean
    res[[paste0(mt, "_lo")]]     <- bq(mt, 0.025)
    res[[paste0(mt, "_hi")]]     <- bq(mt, 0.975)
    # Headline central estimate, HOMOGENIZED with the percentile interval. The plug-in
    # point can fall marginally outside [lo, hi] for skewed rare-event metrics (e.g. death
    # sensitivity); central = "median" guarantees the point lies inside its own interval.
    res[[mt]] <- switch(central,
                        plugin = res[[paste0(mt, "_plugin")]],
                        median = res[[paste0(mt, "_med")]],
                        mean   = res[[paste0(mt, "_mean")]])
  }

  # ---- BCa intervals (keep the plug-in point; bias-correct + accelerate the interval) ----
  # Fixes the "plug-in marginally outside its percentile CI" artifact: z0 shifts the interval
  # toward the plug-in (median-bias correction) and a (acceleration) handles skewness. The
  # acceleration uses a leave-one-out jackknife with the IPCW censoring distribution G held
  # fixed at the full-sample estimate (standard; a is a 2nd-order skewness term).
  if (identical(ci_method, "bca")) {
    zlo <- stats::qnorm(0.025); zhi <- stats::qnorm(0.975)
    acc <- new.env(parent = emptyenv())
    for (risk in risks) {
      P <- pool[[risk]]; thr <- thresholds[[risk]]
      for (h in horizons) {
        hj <- which(eval_times == h); if (!length(hj)) next
        pr <- P$pred[, hj[1]]
        w  <- if (P$type == "cr") .tm_cr_ipcw_weights(P$ftime, P$fstatus, h, g_min = g_min)
              else                .tm_admin_ipcw_weights(P$time, P$event, h, g_min = g_min)
        we <- w$w_event; wn <- w$w_nonevent
        for (t in thr) {
          pos  <- pr >= t
          tp_i <- ifelse(pos, we, 0); fn_i <- ifelse(pos, 0, we)
          fp_i <- ifelse(pos, wn, 0); tn_i <- ifelse(pos, 0, wn)
          TP <- sum(tp_i); FN <- sum(fn_i); FP <- sum(fp_i); TN <- sum(tn_i)
          jk <- list(
            Sens = { d <- (TP - tp_i) + (FN - fn_i); ifelse(d > 0, (TP - tp_i) / d, NA_real_) },
            Spec = { d <- (TN - tn_i) + (FP - fp_i); ifelse(d > 0, (TN - tn_i) / d, NA_real_) },
            PPV  = { d <- (TP - tp_i) + (FP - fp_i); ifelse(d > 0, (TP - tp_i) / d, NA_real_) },
            NPV  = { d <- (TN - tn_i) + (FN - fn_i); ifelse(d > 0, (TN - tn_i) / d, NA_real_) })
          av <- vapply(metrics, function(mt) {
            x <- jk[[mt]]; x <- x[is.finite(x)]
            if (length(x) < 3L) return(0)
            u <- mean(x) - x; s2 <- sum(u^2)
            if (!is.finite(s2) || s2 == 0) return(0)
            sum(u^3) / (6 * s2^1.5)
          }, numeric(1))
          assign(paste(risk, h, t, sep = "|"), av, envir = acc)
        }
      }
    }
    for (mt in metrics) {
      lo_v <- res[[paste0(mt, "_lo")]]; hi_v <- res[[paste0(mt, "_hi")]]
      for (r in seq_len(nrow(res))) {
        key  <- paste(res$risk[r], res$horizon[r], res$threshold[r], sep = "|")
        vals <- allb[[mt]][k_all == key]; vals <- vals[is.finite(vals)]
        th0  <- res[[paste0(mt, "_plugin")]][r]
        if (length(vals) < 20L || !is.finite(th0)) next     # too few reps -> keep percentile
        Bv <- length(vals)
        p_less <- (sum(vals < th0) + 0.5 * sum(vals == th0)) / Bv
        p_less <- min(max(p_less, 1 / (Bv + 1)), Bv / (Bv + 1))
        z0 <- stats::qnorm(p_less)
        a  <- get(key, envir = acc)[[mt]]; if (!is.finite(a)) a <- 0
        adj <- function(z) { d <- 1 - a * (z0 + z); if (abs(d) < 1e-8) d <- sign(d) * 1e-8 + 1e-12
                             stats::pnorm(z0 + (z0 + z) / d) }
        a1 <- adj(zlo); a2 <- adj(zhi)
        a1 <- min(max(a1, 1 / (Bv + 1)), Bv / (Bv + 1))
        a2 <- min(max(a2, 1 / (Bv + 1)), Bv / (Bv + 1))
        qq <- sort(stats::quantile(vals, c(a1, a2), names = FALSE, type = 8))
        lo_v[r] <- qq[1]; hi_v[r] <- qq[2]
      }
      res[[paste0(mt, "_lo")]] <- lo_v
      res[[paste0(mt, "_hi")]] <- hi_v
    }
  }
  attr(res, "ci_method") <- ci_method

  res$model <- model_label
  res <- res[order(res$risk, res$horizon, res$threshold), ]
  rownames(res) <- NULL
  res
}

# Convenience: run for the best_perf pair (readmission shared -> computed once).
threshold_bootstrap_holdout <- function(results_boot_val, horizons = c(6, 12, 36, 60),
                                        thresholds = NULL, metrics = c("Sens", "Spec", "PPV", "NPV"),
                                        central = c("plugin", "median", "mean"),
                                        ci_method = c("percentile", "bca"),
                                        freeze_g = TRUE,
                                        B = 500L, seed = 2125L, parallel = FALSE, verbose = TRUE) {
  central <- match.arg(central); ci_method <- match.arg(ci_method)
  args <- list(horizons = horizons, metrics = metrics, central = central, ci_method = ci_method,
               freeze_g = freeze_g, B = B, seed = seed, parallel = parallel, verbose = verbose)
  if (!is.null(thresholds)) args$thresholds <- thresholds
  out <- list()
  out[[1]] <- do.call(bootstrap_threshold_metrics_holdout,
    c(list(results_boot_val[[1]], model_label = "readmit::shared", risks = "readmission"), args))
  for (mn in names(results_boot_val))
    out[[length(out) + 1L]] <- do.call(bootstrap_threshold_metrics_holdout,
      c(list(results_boot_val[[mn]], model_label = paste0("death::", mn), risks = "death"), args))
  do.call(rbind, out)
}
