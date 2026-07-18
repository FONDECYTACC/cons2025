# =============================================================================
# evaluate_dual_cox_holdout_dualscore.R
#
# Held-out (20% validation) Uno's C-index evaluator for prediction23, extended
# to report concordance under BOTH ranking scores in a single pass:
#   - "lp"   : the linear predictor (original convention). Ignores the
#              stratum-specific baseline hazards. Kept as an audit/stability
#              check: Andres' concern (2026-07-15 review session) is that a
#              horizon-specific risk-based C could be unstable in strata or
#              horizons with few events/at-risk, so lp is retained as a
#              fallback comparator rather than dropped.
#   - "risk" : 1 - S(t) at each evaluation horizon. Accounts for the
#              stratum-specific baseline hazards, so it is the PRIMARY
#              discrimination metric under the thesis' current model-
#              selection rule (described for readmission as "cause-specific
#              net risk").
#
# WHY A NEW FILE, NOT AN EDIT TO evaluate_dual_cox_python_style_boot.R:
# `.dual_cox_fit_one_risk_coherent()` already exists there and already
# supports picking ONE of {lp, risk} via
# `getOption("dualcox.concordance_score")`, but that function (and its
# dispatcher inside `evaluate_dual_cox_python_style()`) is also used by
# prediction225's development-set (80%) comparisons, which expect ONE score
# per call and rely on its current output shape. Changing its behaviour or
# return shape here would risk silently changing already-approved
# prediction225 numbers. This file adds SIBLING functions instead, used ONLY
# by prediction23's held-out (20%) pipeline. It reuses, WITHOUT MODIFYING,
# the shared low-level helpers already defined in
# evaluate_dual_cox_python_style_boot.R:
#   .dual_cox_prepare_fold_data(), .dual_cox_matrixify(),
#   .dual_cox_safe_concordance(), .dual_cox_safe_concordance_coherent(),
#   .dual_cox_prepare_formula_metadata(), ibs_ipcw_train(), to01(),
#   .dual_cox_summarize_metrics().
#
# WHERE THIS PLUGS IN (prediction23_converted_mod.ipynb):
#   - Cell 24 ("holdout-cindex-ibs-run"): replace the two
#     `evaluate_dual_cox_holdout(...)` calls with
#     `evaluate_dual_cox_holdout_dualscore(...)`. Same arguments, same
#     `raw_predictions` shape (so every downstream cell that reads
#     `results_boot_val_bp1$raw_predictions` -- calibration, DCA,
#     IBS-bootstrap, threshold metrics -- keeps working unmodified). The
#     ONLY change downstream is that `$metrics`/`$summary` gain a
#     `ScoreType` column ("lp" / "risk" for Uno's C-Index rows, "n/a" for
#     IBS rows, since IBS does not depend on the ranking score).
#   - Cell 53 ("holdout-cindex-bootstrap"): replace the lp-only
#     `.extract_lp()` / `.run_cidx_boot()` patient-bootstrap block with
#     `.holdout_pool_dualscore()` + `.holdout_cindex_boot_dualscore()`
#     below, which bootstrap PATIENTS (not model refits) with lp and risk
#     both pooled from the frozen `raw_predictions`, exactly mirroring the
#     existing lp-only bootstrap's design (predictions frozen, only
#     resampling varies).
#
# Source this file AFTER validate_holdout_metrics.R (which already ensures
# evaluate_dual_cox_python_style_boot.R is sourced).
# =============================================================================

local({
  required_fns <- c(
    ".dual_cox_prepare_fold_data", ".dual_cox_matrixify",
    ".dual_cox_safe_concordance", ".dual_cox_safe_concordance_coherent",
    ".dual_cox_prepare_formula_metadata", "ibs_ipcw_train", "to01",
    ".dual_cox_summarize_metrics"
  )
  missing_fns <- required_fns[!vapply(required_fns, exists, logical(1), mode = "function")]
  if (length(missing_fns) > 0L) {
    stop(
      "evaluate_dual_cox_holdout_dualscore.R requires ",
      "evaluate_dual_cox_python_style_boot.R to be sourced first. ",
      "Missing: ", paste(missing_fns, collapse = ", "),
      call. = FALSE
    )
  }
})

# -----------------------------------------------------------------------------
# 1. Per-imputation fit: ONE Cox fit, TWO concordance scores computed off the
#    SAME fitted model and the SAME predicted survival matrix. IBS is
#    computed once (it does not depend on the ranking score).
# -----------------------------------------------------------------------------
.dual_cox_fit_one_risk_holdout_dualscore <- function(
    df_train_raw,
    df_test_raw,
    meta,
    time_col,
    event_col,
    eval_times,
    min_test_rows
) {
  prepared <- .dual_cox_prepare_fold_data(
    df_train_raw = df_train_raw,
    df_test_raw = df_test_raw,
    meta = meta,
    min_test_rows = min_test_rows
  )
  if (nzchar(prepared$error)) {
    return(list(error = prepared$error, dropped = prepared$dropped))
  }

  df_train <- prepared$train
  df_test <- prepared$test
  fit <- survival::coxph(
    meta$model_formula,
    data = df_train,
    ties = "efron",
    x = TRUE,
    y = TRUE,
    model = TRUE
  )

  lp_train <- as.numeric(stats::predict(fit, newdata = df_train, type = "lp"))
  lp_val <- as.numeric(stats::predict(fit, newdata = df_test, type = "lp"))
  surv_mat <- pec::predictSurvProb(fit, newdata = df_test, times = eval_times)
  surv_mat <- .dual_cox_matrixify(
    x = surv_mat,
    nrow_expected = nrow(df_test),
    ncol_expected = length(eval_times)
  )
  risk_at <- function(col_idx) as.numeric(1 - surv_mat[, col_idx])

  time_train <- as.numeric(df_train[[time_col]])
  event_train <- to01(df_train[[event_col]])
  time_val <- as.numeric(df_test[[time_col]])
  event_val <- to01(df_test[[event_col]])
  train_time_max <- max(time_train, na.rm = TRUE)

  ibs_global <- ibs_ipcw_train(
    df_train = df_train, df_test = df_test, surv_mat = surv_mat,
    times = eval_times, time_col = time_col, event_col = event_col
  )

  # Global concordance, both scores. The "risk" global score uses the risk
  # at the LARGEST evaluation horizon, mirroring the convention already used
  # by .dual_cox_fit_one_risk_coherent() for dualcox.concordance_score =
  # "risk" (no ymax passed here either, so pairs are compared over the full
  # follow-up, not restricted to that horizon).
  c_global_lp <- .dual_cox_safe_concordance(time = time_val, event = event_val, lp = lp_val)
  c_global_risk <- .dual_cox_safe_concordance_coherent(
    time = time_val, event = event_val, lp = risk_at(length(eval_times))
  )

  c_horizon_lp <- rep(NA_real_, length(eval_times))
  c_horizon_risk <- rep(NA_real_, length(eval_times))
  ibs_horizon <- rep(NA_real_, length(eval_times))

  for (ii in seq_along(eval_times)) {
    t_h <- eval_times[ii]
    if (is.finite(train_time_max) && t_h < train_time_max) {
      c_horizon_lp[ii] <- .dual_cox_safe_concordance(
        time = time_val, event = event_val, lp = lp_val, ymax = t_h
      )
      c_horizon_risk[ii] <- .dual_cox_safe_concordance_coherent(
        time = time_val, event = event_val, lp = risk_at(ii), ymax = t_h
      )

      keep <- eval_times <= t_h
      if (sum(keep) >= 2L) {
        ibs_horizon[ii] <- ibs_ipcw_train(
          df_train = df_train, df_test = df_test,
          surv_mat = surv_mat[, keep, drop = FALSE],
          times = eval_times[keep], time_col = time_col, event_col = event_col
        )
      }
    }
  }

  list(
    error = "",
    dropped = prepared$dropped,
    train = df_train,
    test = df_test,
    lp_train = lp_train,
    lp_val = lp_val,
    surv_val = surv_mat,
    y_train = data.frame(time = time_train, event = event_train),
    y_val = data.frame(time = time_val, event = event_val),
    ibs_global = ibs_global,
    ibs_horizon = stats::setNames(ibs_horizon, as.character(eval_times)),
    c_global_lp = c_global_lp,
    c_horizon_lp = stats::setNames(c_horizon_lp, as.character(eval_times)),
    c_global_risk = c_global_risk,
    c_horizon_risk = stats::setNames(c_horizon_risk, as.character(eval_times))
  )
}

# -----------------------------------------------------------------------------
# 2. Imputation loop: fit-on-train / predict-on-val per imputation, pooled
#    across imputations. Mirrors evaluate_dual_cox_holdout() from
#    validate_holdout_metrics.R exactly (same signature, same
#    raw_predictions shape), except it calls the dual-score fitter above and
#    tags every Uno's C-Index row with ScoreType in {"lp", "risk"}. IBS rows
#    get ScoreType = "n/a".
# -----------------------------------------------------------------------------
evaluate_dual_cox_holdout_dualscore <- function(
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

  add_metric <- function(imp, risk, metric, time, value, score_type) {
    mi <<- mi + 1L
    metrics_log[[mi]] <<- data.frame(
      Imp = imp, Fold = 1L, Risk = risk, Metric = metric, Time = time,
      Value = value, ScoreType = score_type, stringsAsFactors = FALSE
    )
  }

  if (verbose) cat(sprintf(
    "Held-out dual-score Cox: %d imputations, fit train -> predict val, C by lp AND by risk\n",
    n_imp
  ))

  for (i in seq_len(n_imp)) {
    df_tr <- train_list[[i]]; df_te <- val_list[[i]]
    readmit_res <- tryCatch(
      .dual_cox_fit_one_risk_holdout_dualscore(
        df_tr, df_te, meta_readmit,
        readmit_time_col, readmit_event_col, eval_times, min_test_rows
      ),
      error = function(e) list(error = paste0("holdout_error:", conditionMessage(e)), dropped = 0L)
    )
    death_res <- tryCatch(
      .dual_cox_fit_one_risk_holdout_dualscore(
        df_tr, df_te, meta_death,
        death_time_col, death_event_col, eval_times, min_test_rows
      ),
      error = function(e) list(error = paste0("holdout_error:", conditionMessage(e)), dropped = 0L)
    )

    for (rr in list(list(res = readmit_res, lbl = "Readmission"),
                    list(res = death_res,   lbl = "Death"))) {
      res <- rr$res
      if (nzchar(res$error)) {
        fi <- fi + 1L
        failures_log[[fi]] <- data.frame(
          Imp = i, Fold = 1L, Risk = rr$lbl, Error = res$error, stringsAsFactors = FALSE
        )
      } else {
        add_metric(i, rr$lbl, "Uno's C-Index", "Global", res$c_global_lp,   "lp")
        add_metric(i, rr$lbl, "Uno's C-Index", "Global", res$c_global_risk, "risk")
        add_metric(i, rr$lbl, "IBS",           "Global", res$ibs_global,    "n/a")
        for (tl in names(res$c_horizon_lp))
          add_metric(i, rr$lbl, "Uno's C-Index", tl, unname(res$c_horizon_lp[[tl]]),   "lp")
        for (tl in names(res$c_horizon_risk))
          add_metric(i, rr$lbl, "Uno's C-Index", tl, unname(res$c_horizon_risk[[tl]]), "risk")
        for (tl in names(res$ibs_horizon))
          add_metric(i, rr$lbl, "IBS",           tl, unname(res$ibs_horizon[[tl]]),    "n/a")
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
        dropped_rows = readmit_res$dropped
      ),
      death = if (nzchar(death_res$error)) list(error = death_res$error) else list(
        lp_train = death_res$lp_train, lp_val = death_res$lp_val,
        surv_val_matrix = death_res$surv_val,
        y_train = death_res$y_train, y_val = death_res$y_val,
        dropped_rows = death_res$dropped
      )
    )
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")

  metrics_df <- if (length(metrics_log)) do.call(rbind, metrics_log) else
    data.frame(Imp = integer(), Fold = integer(), Risk = character(), Metric = character(),
               Time = character(), Value = numeric(), ScoreType = character(),
               stringsAsFactors = FALSE)
  failures_df <- if (length(failures_log)) do.call(rbind, failures_log) else
    data.frame(Imp = integer(), Fold = integer(), Risk = character(), Error = character(),
               stringsAsFactors = FALSE)

  list(
    metrics = metrics_df,
    summary = .dual_cox_summarize_metrics_by_scoretype(metrics_df),
    raw_predictions = raw_predictions,
    failures = failures_df,
    config = list(
      formula_readmit = meta_readmit$formula_text,
      formula_death = meta_death$formula_text,
      readmit_time_col = readmit_time_col, readmit_event_col = readmit_event_col,
      death_time_col = death_time_col, death_event_col = death_event_col,
      eval_times = eval_times, k_folds = 1L, seed = NA_integer_,
      plan_dummy_cols = c("plan_type_corr_pg_pr", "plan_type_corr_m_pr",
                           "plan_type_corr_pg_pai", "plan_type_corr_m_pai"),
      plan_strata_fallback_col = "plan_type_strata",
      min_test_rows = min_test_rows, validation = "holdout-20pct-dualscore",
      timestamp = as.character(Sys.time())
    )
  )
}

# -----------------------------------------------------------------------------
# 3. Summary that respects ScoreType. Thin wrapper around the SHARED, already
#    -validated .dual_cox_summarize_metrics() (defined in
#    evaluate_dual_cox_python_style_boot.R, unmodified here). Splitting by
#    ScoreType FIRST matters: that shared summarizer groups only by
#    Risk/Metric/Time, so without this split the "lp" and "risk" Uno's
#    C-Index rows for the same Risk/Metric/Time would be silently pooled
#    into one incorrect mean.
# -----------------------------------------------------------------------------
.dual_cox_summarize_metrics_by_scoretype <- function(metrics_df) {
  if (!nrow(metrics_df)) {
    return(data.frame(
      ScoreType = character(0), Risk = character(0), Metric = character(0),
      Time = character(0), mean = numeric(0), sd = numeric(0),
      q025 = numeric(0), q975 = numeric(0), n = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  parts <- split(metrics_df, metrics_df$ScoreType)
  out <- do.call(rbind, lapply(names(parts), function(st) {
    summarized <- .dual_cox_summarize_metrics(parts[[st]])
    cbind(ScoreType = st, summarized, stringsAsFactors = FALSE)
  }))
  time_num <- suppressWarnings(as.numeric(out$Time))
  out <- out[order(out$Risk, out$Metric, out$ScoreType, is.na(time_num), time_num, out$Time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# 4. Patient-bootstrap C-index, both scores, for cell 53. Pools lp and risk
#    across the 5 imputations into one value per patient (same
#    average-across-imputations convention already used elsewhere in this
#    pipeline for pooled point estimates), then bootstraps PATIENTS (not
#    model refits) with predictions frozen -- exactly like the existing
#    lp-only bootstrap already in cell 53. Reuses
#    .dual_cox_safe_concordance_coherent() so lp and risk go through the
#    IDENTICAL concordance code path (no separate "lp bootstrap" and "risk
#    bootstrap" implementations to keep in sync by hand).
# -----------------------------------------------------------------------------
.holdout_pool_dualscore <- function(res, val_list, risk_name = c("readmission", "death")) {
  risk_name <- match.arg(risk_name)
  rp <- res$raw_predictions
  n_imp <- length(rp)
  stopifnot(n_imp == length(val_list))

  pieces <- lapply(rp, function(item) {
    imp <- as.integer(item$imp_idx)
    block <- item[[risk_name]]
    idx <- as.integer(item$original_val_idx)
    stopifnot(is.null(block$error))
    sm <- as.matrix(block$surv_val_matrix)
    stopifnot(
      nrow(sm) == length(idx),
      nrow(block$y_val) == length(idx),
      "row_id" %in% names(val_list[[imp]])
    )
    id <- as.character(val_list[[imp]]$row_id[idx])
    ord <- order(id)
    list(
      id = id[ord],
      time = as.numeric(block$y_val$time[ord]),
      event = as.numeric(block$y_val$event[ord]),
      lp = as.numeric(block$lp_val[ord]),
      risk = 1 - sm[ord, , drop = FALSE]
    )
  })

  ref <- pieces[[1]]
  for (p in pieces[-1]) {
    stopifnot(
      identical(p$id, ref$id),
      max(abs(p$time - ref$time), na.rm = TRUE) < 1e-8,
      identical(p$event, ref$event)
    )
  }

  list(
    id = ref$id,
    time = ref$time,
    event = ref$event,
    lp = Reduce(`+`, lapply(pieces, `[[`, "lp")) / n_imp,
    risk = Reduce(`+`, lapply(pieces, `[[`, "risk")) / n_imp,
    eval_times = as.numeric(res$config$eval_times)
  )
}

## `horizons` may include NA to request an unrestricted ("Global", no ymax)
## row alongside the per-horizon ones, mirroring the "Global" row the
## original lp-only bootstrap in `holdout-cindex-bootstrap` reported (there
## via a call to .dual_cox_safe_concordance() with no `ymax`). Pass
## `parallel = TRUE` (default) to run the B replicates via
## `future.apply::future_vapply()` when a non-sequential `future::plan()` is
## already active (set up by the caller, e.g. the notebook cell, same as the
## original bootstrap); otherwise falls back to a plain sequential `vapply`.
## `future_vapply()`'s default global-detection exports `.dual_cox_safe_
## concordance_coherent` (and `pooled`) to the workers automatically, so no
## manual re-source-inside-worker guard is needed here.
.holdout_cindex_boot_dualscore <- function(
    pooled, horizons, B = 1000L, seed = 2125L,
    include_global = TRUE, parallel = TRUE
) {
  n <- length(pooled$id)
  horizon_labels <- c(if (include_global) "Global", as.character(horizons))
  horizon_values <- c(if (include_global) NA_real_, horizons)

  one_pass <- function(idx, h, score_type) {
    if (identical(score_type, "lp")) {
      score <- pooled$lp[idx]
    } else if (is.na(h)) {
      # "Global" risk score: risk at the LARGEST evaluation horizon, same
      # convention as .dual_cox_fit_one_risk_coherent()'s global score.
      score <- pooled$risk[idx, length(pooled$eval_times)]
    } else {
      col <- match(h, pooled$eval_times)
      stopifnot(!is.na(col))
      score <- pooled$risk[idx, col]
    }
    ymax_arg <- if (is.na(h)) NULL else h
    .dual_cox_safe_concordance_coherent(
      time = pooled$time[idx], event = pooled$event[idx], lp = score, ymax = ymax_arg
    )
  }

  use_parallel <- isTRUE(parallel) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  rows <- lapply(seq_along(horizon_values), function(hi) {
    h <- horizon_values[hi]
    lapply(c("lp", "risk"), function(st) {
      point <- one_pass(seq_len(n), h, st)
      boot_fun <- function(b) {
        set.seed(seed + hi * 100000L + b, kind = "Mersenne-Twister")
        one_pass(sample.int(n, n, replace = TRUE), h, st)
      }
      boot <- if (use_parallel) {
        future.apply::future_vapply(
          seq_len(B), boot_fun, numeric(1),
          future.seed = TRUE, future.packages = "survival"
        )
      } else {
        vapply(seq_len(B), boot_fun, numeric(1))
      }
      q <- stats::quantile(boot, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
      data.frame(
        Horizon = horizon_labels[hi], ScoreType = st, C = point,
        C_lower = q[1], C_upper = q[2],
        B = B, B_valid = sum(is.finite(boot)),
        stringsAsFactors = FALSE
      )
    })
  })
  out <- do.call(rbind, unlist(rows, recursive = FALSE))
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# 5. Paired ΔC BETWEEN TWO MODELS (2026-07-15, added on request to complement
#    .holdout_cindex_boot_dualscore() above, which only reports ONE model's C
#    -- comparing two separate calls' marginal CIs is not a valid paired
#    comparison). Aligns both models' scores for the SAME patients in a
#    SINGLE survival::concordance(Surv(...) ~ score_A + score_B) call and
#    reads the paired difference and its SE directly from that call's own
#    covariance matrix (infinitesimal jackknife) -- the same trick already
#    used by delta_c_holdout.R (risk-only, model-vs-model) and by
#    .holdout_wolbers_paired_ij() in holdout_arm_comparison.R (competing-
#    risks version). This version covers BOTH lp and risk scoring, which
#    delta_c_holdout.R does not.
#
# `pooled_A`/`pooled_B` are .holdout_pool_dualscore() outputs for the SAME
# patients (same id order); typically for the SAME risk (e.g. both "death",
# comparing best_perf1 vs best_perf2), since comparing different outcomes
# would not be a meaningful concordance comparison.
# -----------------------------------------------------------------------------
.holdout_delta_c_dualscore_ij <- function(
    pooled_A, pooled_B, horizons, label_A = "A", label_B = "B", include_global = TRUE
) {
  stopifnot(identical(pooled_A$id, pooled_B$id))
  horizon_values <- c(if (include_global) NA_real_, horizons)

  one_score_pair <- function(h, score_type) {
    if (identical(score_type, "lp")) {
      sA <- pooled_A$lp; sB <- pooled_B$lp
    } else {
      col_h <- function(pooled) if (is.na(h)) length(pooled$eval_times) else match(h, pooled$eval_times)
      colA <- col_h(pooled_A); colB <- col_h(pooled_B)
      stopifnot(!is.na(colA), !is.na(colB))
      sA <- pooled_A$risk[, colA]; sB <- pooled_B$risk[, colB]
    }
    d <- data.frame(time = pooled_A$time, event = pooled_A$event, score_A = sA, score_B = sB)
    ok <- is.finite(d$time) & is.finite(d$event) & is.finite(d$score_A) & is.finite(d$score_B)
    d <- d[ok, , drop = FALSE]

    args <- list(
      object = survival::Surv(time, event) ~ score_A + score_B, data = d,
      timewt = "n/G2", reverse = TRUE
    )
    if (!is.na(h)) args$ymax <- h
    fit <- do.call(survival::concordance, args)
    est <- unname(fit$concordance); V <- as.matrix(fit$var)
    z <- stats::qnorm(0.975)
    se_c <- sqrt(pmax(diag(V), 0))
    delta <- est[1] - est[2]
    se_delta <- sqrt(max(V[1, 1] + V[2, 2] - 2 * V[1, 2], 0))

    data.frame(
      score_type = score_type,
      horizon = if (is.na(h)) "Global" else as.character(h),
      n = nrow(d),
      C_A = est[1], C_A_lower = max(0, est[1] - z * se_c[1]), C_A_upper = min(1, est[1] + z * se_c[1]),
      C_B = est[2], C_B_lower = max(0, est[2] - z * se_c[2]), C_B_upper = min(1, est[2] + z * se_c[2]),
      delta_C = delta, se_delta = se_delta,
      delta_C_lower = delta - z * se_delta, delta_C_upper = delta + z * se_delta,
      excludes_zero = (delta - z * se_delta > 0) || (delta + z * se_delta < 0),
      favours = if ((delta - z * se_delta) > 0) label_A
                else if ((delta + z * se_delta) < 0) label_B
                else "no clear difference",
      stringsAsFactors = FALSE
    )
  }

  rows <- lapply(horizon_values, function(h) rbind(one_score_pair(h, "lp"), one_score_pair(h, "risk")))
  out <- do.call(rbind, rows)
  out$model_A <- label_A; out$model_B <- label_B
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# 6. Within-strata Uno's C-index (2026-07-17, added on request). Sensitivity
#    check mirroring the dev-set (CV) "within_stratum" variant already reported
#    by concordance_coherence_table.R (VARIANTS$within_stratum there): restricts
#    concordant/discordant pairs to patients sharing the SAME stratum
#    (survival::concordance(... + strata(stratum_))) instead of scoring pairs
#    across the whole held-out set regardless of stratum. On the dev set this
#    is treated as a SENSITIVITY metric, not the headline number (the canonical
#    CV option is dualcox.concordance_score = "risk",
#    dualcox.concordance_within_strata = FALSE); this is the held-out-set
#    analogue of that same check. It reuses the frozen `.holdout_pool_dualscore()`
#    output (re_dat/de1_dat/de2_dat in the notebook's `holdout-cindex-bootstrap`
#    cell) -- no model is refit.
#
# `.holdout_pool_strata_for()` recomputes each imputation's stratum signature
# straight from `val_list` (via the shared `.dual_cox_make_strata_signature_safe()`,
# the SAME helper `.dual_cox_prepare_fold_data()` already uses internally to
# drop unseen-stratum rows during fitting), row_id-sorted exactly like
# `.holdout_pool_dualscore()` so the two align by position. It requires
# `dropped_rows == 0` for every imputation -- already true in practice for this
# pipeline (`.holdout_pool_dualscore()` itself silently assumes this via its
# own nrow() stopifnot) -- so the un-filtered `val_list[[imp]]` lines up 1:1
# with the model's `original_val_idx`.
# -----------------------------------------------------------------------------
.holdout_pool_strata_for <- function(res, val_list, formula, risk_name = c("readmission", "death")) {
  risk_name <- match.arg(risk_name)
  meta <- .dual_cox_prepare_formula_metadata(formula, val_list)
  if (length(meta$strata_term_labels) == 0L) {
    stop("Model for `", risk_name, "` has no strata() term; within-strata C-index is not defined.",
         call. = FALSE)
  }

  rp <- res$raw_predictions
  n_imp <- length(rp)
  stopifnot(n_imp == length(val_list))

  pieces <- lapply(rp, function(item) {
    imp <- as.integer(item$imp_idx)
    block <- item[[risk_name]]
    idx <- as.integer(item$original_val_idx)
    stopifnot(is.null(block$error), identical(block$dropped_rows, 0L))
    df_imp <- val_list[[imp]][idx, , drop = FALSE]
    sig <- .dual_cox_make_strata_signature_safe(
      data = df_imp,
      strata_term_labels = meta$strata_term_labels,
      formula_env = environment(meta$model_formula)
    )
    id <- as.character(df_imp$row_id)
    ord <- order(id)
    list(id = id[ord], strata = sig[ord])
  })

  ref <- pieces[[1L]]
  mismatched <- vapply(pieces[-1L], function(p) !identical(p$strata, ref$strata), logical(1))
  if (any(mismatched)) {
    n_diff <- sum(ref$strata != pieces[[which(mismatched)[1L] + 1L]]$strata)
    warning(sprintf(
      "%s: stratum labels differ across imputations in %d imputation(s) (e.g. %d rows differ vs imputation 1); using imputation 1's labels. This can happen when a stratifying covariate is itself imputed.",
      risk_name, sum(mismatched), n_diff
    ), call. = FALSE)
  }
  list(id = ref$id, strata = ref$strata)
}

## `pooled` is a `.holdout_pool_dualscore()` output; `strata_obj` is a
## `.holdout_pool_strata_for()` output for the SAME patients (`id` order must
## match -- checked). Mirrors `.holdout_cindex_boot_dualscore()`, but scores ONE
## `score_type` per call (call twice, "lp" and "risk", and rbind, to get the same
## long shape `.holdout_cindex_boot_dualscore()`/`.fmt_cidx()` already expect),
## passing `strata =` into `.dual_cox_safe_concordance_coherent()` so
## `survival::concordance()` restricts pairs to within-stratum comparisons.
.holdout_cindex_boot_within_strata <- function(
    pooled, strata_obj, horizons, score_type = c("risk", "lp"),
    B = 1000L, seed = 2125L, include_global = TRUE, parallel = TRUE
) {
  score_type <- match.arg(score_type)
  stopifnot(identical(pooled$id, strata_obj$id))
  strata_vec <- strata_obj$strata
  n <- length(pooled$id)
  horizon_labels <- c(if (include_global) "Global", as.character(horizons))
  horizon_values <- c(if (include_global) NA_real_, horizons)

  one_pass <- function(idx, h) {
    score <- if (identical(score_type, "lp")) {
      pooled$lp[idx]
    } else if (is.na(h)) {
      pooled$risk[idx, length(pooled$eval_times)]
    } else {
      col <- match(h, pooled$eval_times)
      stopifnot(!is.na(col))
      pooled$risk[idx, col]
    }
    ymax_arg <- if (is.na(h)) NULL else h
    .dual_cox_safe_concordance_coherent(
      time = pooled$time[idx], event = pooled$event[idx], lp = score,
      ymax = ymax_arg, strata = strata_vec[idx]
    )
  }

  use_parallel <- isTRUE(parallel) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  rows <- lapply(seq_along(horizon_values), function(hi) {
    h <- horizon_values[hi]
    point <- one_pass(seq_len(n), h)
    boot_fun <- function(b) {
      set.seed(seed + hi * 100000L + b, kind = "Mersenne-Twister")
      one_pass(sample.int(n, n, replace = TRUE), h)
    }
    boot <- if (use_parallel) {
      future.apply::future_vapply(
        seq_len(B), boot_fun, numeric(1),
        future.seed = TRUE, future.packages = "survival"
      )
    } else {
      vapply(seq_len(B), boot_fun, numeric(1))
    }
    q <- stats::quantile(boot, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
    data.frame(
      Horizon = horizon_labels[hi], ScoreType = score_type, C = point,
      C_lower = q[1], C_upper = q[2], B = B, B_valid = sum(is.finite(boot)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
