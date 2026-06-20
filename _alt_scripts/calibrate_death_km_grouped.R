calibrate_death_km_grouped <- function(
    formula,
    data,
    times        = c(6, 12, 36, 60),
    time_col     = "death_time_from_disch_m",
    event_col    = "death_event",
    validation   = "cv",
    folds        = 5L,
    n_repeats    = 20L,
    g            = 10L,
    min_bin_n    = 20L,
    loess_span   = 0.75,
    parallel_cv  = TRUE,
    verbose      = TRUE,
    seed         = NULL
) {
  stopifnot(requireNamespace("survival",        quietly = TRUE))
  stopifnot(requireNamespace("riskRegression", quietly = TRUE))
  stopifnot(requireNamespace("prodlim",        quietly = TRUE))
  stopifnot(requireNamespace("future.apply",   quietly = TRUE))

  if (!identical(validation, "cv"))
    stop("Only validation = 'cv' is implemented.", call. = FALSE)
  if (!is.list(data) || length(data) == 0L)
    stop("'data' must be a non-empty list of imputed data frames.", call. = FALSE)

  folds     <- as.integer(folds)
  n_repeats <- as.integer(n_repeats)
  g         <- as.integer(g)
  n_imp     <- length(data)

  # ── Pre-process: subset to needed columns only ────────────────────────────────
  needed_cols  <- unique(c(all.vars(formula), time_col, event_col))

  # Convert formula to string, then remove the formula object from the local frame.
  # A formula carries its enclosing environment as an attribute. If created in the
  # user's global session, that environment contains all large data objects and
  # gets serialized transitively into .run_task's closure — causing the hang.
  # Storing the formula as a text string avoids this entirely.
  # .run_task reconstructs it with as.formula(env=globalenv()) on each worker,
  # where globalenv() is the WORKER's small global env with survival attached.
  formula_str <- paste(deparse(formula, width.cutoff = 500L), collapse = " ")
  rm(formula)  # remove from frame so the closure never references it
  data_trimmed <- lapply(data, function(d) {
    df <- as.data.frame(d)
    df[, intersect(needed_cols, names(df)), drop = FALSE]
  })
  rm(data)  # release original — closure of .run_task must not carry it
  n_obs <- nrow(data_trimmed[[1L]])

  # Outcome vectors from first imputation (identical across all)
  time_obs  <- data_trimmed[[1L]][[time_col]]
  event_obs <- as.integer(data_trimmed[[1L]][[event_col]])

  # ── Pre-generate fold assignments for all repeats ─────────────────────────────
  .make_folds <- function(n, k, rep_seed) {
    if (!is.null(rep_seed)) set.seed(rep_seed)
    ids <- sample(rep(seq_len(k), length.out = n))
    lapply(seq_len(k), function(f) which(ids == f))
  }
  fold_assignments <- lapply(seq_len(n_repeats), function(r) {
    .make_folds(n_obs, folds, if (!is.null(seed)) seed + r else NULL)
  })

  # ── Calibration metrics at one time horizon (runs in main process) ────────────
  .metrics_at_time <- function(pred_vec, t_horizon) {
    cuts <- unique(stats::quantile(pred_vec, probs = seq(0, 1, length.out = g + 1L),
                                   na.rm = TRUE, names = FALSE, type = 8))
    cuts <- cuts[-c(1L, length(cuts))]
    bins <- if (length(cuts) > 0L)
      findInterval(pred_vec, cuts, all.inside = TRUE) + 1L
    else
      rep(1L, length(pred_vec))

    sf_all      <- survival::survfit(survival::Surv(time_obs, event_obs) ~ 1)
    obs_at_t    <- summary(sf_all, times = t_horizon, extend = TRUE)$surv
    obs_overall <- if (length(obs_at_t) > 0L) 1 - obs_at_t[[1L]] else NA_real_

    unique_bins <- sort(unique(bins))
    obs_by_bin  <- vapply(unique_bins, function(b) {
      idx <- which(bins == b)
      if (length(idx) < min_bin_n) return(NA_real_)
      sf_b  <- survival::survfit(survival::Surv(time_obs[idx], event_obs[idx]) ~ 1)
      obs_b <- summary(sf_b, times = t_horizon, extend = TRUE)$surv
      if (length(obs_b) > 0L) 1 - obs_b[[1L]] else NA_real_
    }, numeric(1L))

    bin_pred <- vapply(unique_bins, function(b)
      mean(pred_vec[bins == b], na.rm = TRUE), numeric(1L))
    bin_n    <- vapply(unique_bins, function(b)
      sum(bins == b), integer(1L))

    keep <- !is.na(obs_by_bin) & bin_n >= min_bin_n
    ece  <- if (any(keep))
      stats::weighted.mean(abs(bin_pred[keep] - obs_by_bin[keep]), w = bin_n[keep], na.rm = TRUE)
    else NA_real_

    obs_individual <- obs_by_bin[match(bins, unique_bins)]
    ici <- tryCatch({
      valid <- !is.na(obs_individual) & is.finite(pred_vec)
      if (sum(valid) < 10L) {
        NA_real_
      } else {
        lo <- stats::loess(
          obs_individual[valid] ~ pred_vec[valid],
          span    = loess_span,
          control = stats::loess.control(surface = "interpolate")
        )
        mean(abs(stats::fitted(lo) - pred_vec[valid]), na.rm = TRUE)
      }
    }, error = function(e) NA_real_)

    eo_ratio <- if (!is.na(obs_overall) && obs_overall > 0)
      mean(pred_vec, na.rm = TRUE) / obs_overall
    else NA_real_

    list(ici = ici, ece = ece, eo_ratio = eo_ratio,
         mean_pred = mean(pred_vec, na.rm = TRUE),
         observed  = obs_overall, n_bins = sum(keep),
         curve_data = data.frame(
           bin            = unique_bins[keep],
           mean_predicted = bin_pred[keep],
           observed       = obs_by_bin[keep],
           n_patients     = bin_n[keep],
           stringsAsFactors = FALSE
         ))
  }

  # ── Task list: one task per (repeat, fold) ─────────────────────────────────────
  # Imputations are averaged INSIDE each task — same pattern as the readmission
  # function. 100 tasks instead of 500: fewer futures to serialize.
  tasks <- vector("list", n_repeats * folds)
  k <- 0L
  for (r in seq_len(n_repeats)) {
    for (fi in seq_len(folds)) {
      k <- k + 1L
      tasks[[k]] <- list(r = r, fi = fi)
    }
  }

  if (verbose) {
    cat(sprintf(
      "\ncalibrate_death_km_grouped: %d repeats x %d folds x %d imputations = %d fits (%d tasks)\n",
      n_repeats, folds, n_imp, n_repeats * folds * n_imp, length(tasks)
    ))
    flush.console()
  }

  # Worker: fit Cox on ALL imputations for one (rep, fold); return averaged risk.
  # Closure carries: formula_str (string, ~100 bytes), data_trimmed (~5-25 MB),
  # fold_assignments, n_obs, times — no large env objects.
  # full closure + future.globals=FALSE works because closure is self-contained.
  .run_task <- function(task) {
    tryCatch({
      r  <- task$r
      fi <- task$fi
      test_idx  <- fold_assignments[[r]][[fi]]
      train_idx <- setdiff(seq_len(n_obs), test_idx)

      # Reconstruct formula in worker's globalenv (has survival/splines attached)
      formula_local <- stats::as.formula(formula_str, env = globalenv())

      risk_sum <- matrix(0, nrow = length(test_idx), ncol = length(times))
      n_valid  <- 0L

      for (mi in seq_along(data_trimmed)) {
        df_imp <- data_trimmed[[mi]]
        fit <- tryCatch(
          survival::coxph(formula_local, data = df_imp[train_idx, , drop = FALSE],
                          x = TRUE, y = TRUE, model = TRUE),
          error = function(e) {
            warning(sprintf("[r=%d fi=%d mi=%d] coxph failed: %s", r, fi, mi,
                            conditionMessage(e)))
            NULL
          }
        )
        if (is.null(fit)) next
        risk_hat <- tryCatch(
          riskRegression::predictRisk(
            fit, newdata = df_imp[test_idx, , drop = FALSE], times = times
          ),
          error = function(e) {
            warning(sprintf("[r=%d fi=%d mi=%d] predictRisk: %s",
                            r, fi, mi, conditionMessage(e)))
            NULL
          }
        )
        if (is.null(risk_hat)) next
        risk_sum <- risk_sum + risk_hat
        n_valid  <- n_valid + 1L
      }

      if (n_valid == 0L) return(NULL)
      list(r = r, fi = fi, test_idx = test_idx, risk_mat = risk_sum / n_valid)
    }, error = function(e) {
      warning(sprintf("[TASK r=%d fi=%d] FATAL: %s", task$r, task$fi, conditionMessage(e)))
      NULL
    })
  }

  # ── Pre-flight: run task 1 sequentially to surface any hidden error ───────────
  if (verbose) {
    cat("  Pre-flight: running task 1 sequentially...\n"); flush.console()
    pf <- tryCatch(.run_task(tasks[[1L]]), error = function(e) e)
    if (inherits(pf, "error")) {
      stop(sprintf("Pre-flight task 1 FAILED: %s", conditionMessage(pf)), call. = FALSE)
    } else if (is.null(pf)) {
      warning("Pre-flight task 1 returned NULL (n_valid=0). Check formula variables and data.")
    } else {
      cat(sprintf("  Pre-flight OK: risk_mat dim=%dx%d\n",
                  nrow(pf$risk_mat), ncol(pf$risk_mat)))
      flush.console()
    }
  }

  # ── Safety net: raise per-future size limit ───────────────────────────────────
  .prev_maxsize <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = +Inf)
  on.exit(options(future.globals.maxSize = .prev_maxsize), add = TRUE)

  # ── Run all tasks ──────────────────────────────────────────────────────────────
  if (parallel_cv) {
    task_results <- future.apply::future_lapply(
      tasks,
      .run_task,
      future.seed       = if (!is.null(seed)) seed else TRUE,
      future.globals    = FALSE,
      future.packages   = c("survival", "splines", "riskRegression", "prodlim"),
      future.chunk.size = ceiling(length(tasks) / 20L)
    )
  } else {
    task_results <- lapply(tasks, .run_task)
  }

  n_failed <- sum(vapply(task_results, is.null, logical(1L)))
  if (verbose && n_failed > 0L)
    message(sprintf("  WARNING: %d/%d tasks returned NULL (coxph failed)", n_failed, length(tasks)))

  # ── Compute metrics per repetition (sequential, main process) ─────────────────
  all_reps <- lapply(seq_len(n_repeats), function(r) {
    rep_tasks <- Filter(function(res) !is.null(res) && res$r == r, task_results)
    if (length(rep_tasks) == 0L) return(NULL)

    risk_final <- matrix(NA_real_, nrow = n_obs, ncol = length(times))
    for (res in rep_tasks) risk_final[res$test_idx, ] <- res$risk_mat
    if (anyNA(risk_final)) return(NULL)

    if (verbose) {
      cat(sprintf("  Pooling repetition %d/%d\r", r, n_repeats))
      flush.console()
    }

    horizon_out <- lapply(seq_along(times), function(j) {
      m <- .metrics_at_time(risk_final[, j], times[j])
      metrics <- data.frame(time_months = times[j], ici = m$ici, ece = m$ece,
                            eo_ratio = m$eo_ratio, mean_pred = m$mean_pred,
                            observed = m$observed, n_bins = m$n_bins,
                            stringsAsFactors = FALSE)
      curves <- if (nrow(m$curve_data) > 0L)
        cbind(data.frame(time_months = times[j], repetition = r,
                         stringsAsFactors = FALSE), m$curve_data)
      else NULL
      list(metrics = metrics, curves = curves)
    })
    list(
      metrics = do.call(rbind.data.frame, lapply(horizon_out, `[[`, "metrics")),
      curves  = do.call(rbind.data.frame,
                        Filter(Negate(is.null), lapply(horizon_out, `[[`, "curves")))
    )
  })
  if (verbose) cat("\n")

  all_reps <- Filter(Negate(is.null), all_reps)
  if (length(all_reps) == 0L)
    stop(sprintf(
      "All %d repetitions failed. %d/%d tasks returned NULL. Verify formula variables exist in data.",
      n_repeats, n_failed, length(tasks)
    ), call. = FALSE)
  all_df         <- do.call(rbind.data.frame, lapply(all_reps, `[[`, "metrics"))
  all_curves_raw <- do.call(rbind.data.frame,
                            Filter(Negate(is.null), lapply(all_reps, `[[`, "curves")))

  # ── Pool across repetitions ────────────────────────────────────────────────────
  pooled <- do.call(rbind.data.frame, lapply(times, function(t) {
    sub <- all_df[all_df$time_months == t, , drop = FALSE]
    data.frame(
      time_months    = t,
      ici_mean       = mean(sub$ici,       na.rm = TRUE),
      ici_sd         = if (nrow(sub) > 1L) stats::sd(sub$ici, na.rm = TRUE) else 0,
      ici_median     = stats::median(sub$ici, na.rm = TRUE),
      ece_mean       = mean(sub$ece,       na.rm = TRUE),
      ece_sd         = if (nrow(sub) > 1L) stats::sd(sub$ece, na.rm = TRUE) else 0,
      eo_mean        = mean(sub$eo_ratio,  na.rm = TRUE),
      eo_sd          = if (nrow(sub) > 1L) stats::sd(sub$eo_ratio, na.rm = TRUE) else 0,
      mean_pred_mean = mean(sub$mean_pred, na.rm = TRUE),
      observed_mean  = mean(sub$observed,  na.rm = TRUE),
      n_bins_mean    = mean(sub$n_bins,    na.rm = TRUE),
      n_reps         = sum(!is.na(sub$ici)),
      stringsAsFactors = FALSE
    )
  }))

  if (verbose) {
    cat("\n  Pooled calibration summary:\n")
    print(pooled[, c("time_months", "ici_mean", "ici_sd", "ece_mean", "eo_mean")])
    flush.console()
  }

  flat <- data.frame(
    time_months = pooled$time_months,
    ici         = pooled$ici_mean,
    ici_sd      = pooled$ici_sd,
    ece         = pooled$ece_mean,
    ece_sd      = pooled$ece_sd,
    eo_ratio    = pooled$eo_mean,
    eo_ratio_sd = pooled$eo_sd,
    mean_pred   = pooled$mean_pred_mean,
    observed    = pooled$observed_mean,
    n_bins      = pooled$n_bins_mean,
    stringsAsFactors = FALSE
  )

  # ── Pool calibration curves across repetitions ────────────────────────────────
  calibration_curves <- if (nrow(all_curves_raw) > 0L) {
    key <- paste(all_curves_raw$time_months, all_curves_raw$bin, sep = "___")
    spl <- split(all_curves_raw, key)
    out_curves <- lapply(spl, function(z) {
      mp <- z$mean_predicted
      ob <- z$observed
      data.frame(
        time_months    = unique(z$time_months),
        bin            = unique(z$bin),
        mean_predicted = mean(mp, na.rm = TRUE),
        observed       = mean(ob, na.rm = TRUE),
        observed_sd    = if (length(ob) > 1L) stats::sd(ob, na.rm = TRUE) else 0,
        observed_lower = as.numeric(stats::quantile(ob, 0.025, na.rm = TRUE)),
        observed_upper = as.numeric(stats::quantile(ob, 0.975, na.rm = TRUE)),
        n_patients     = mean(z$n_patients, na.rm = TRUE),
        n_reps         = sum(!is.na(ob)),
        stringsAsFactors = FALSE
      )
    })
    out_curves <- do.call(rbind.data.frame, out_curves)
    out_curves[order(out_curves$time_months, out_curves$mean_predicted), , drop = FALSE]
  } else {
    data.frame()
  }

  list(
    pooled_summary     = pooled,
    rep_results        = all_df,
    flat               = flat,
    calibration_curves = calibration_curves
  )
}
