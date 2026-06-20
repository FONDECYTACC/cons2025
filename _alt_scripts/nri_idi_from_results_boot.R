# Net Reclassification Improvement (NRI) and Integrated Discrimination
# Improvement (IDI) for DUAL survival models, computed from results_boot
# (output of evaluate_dual_cox_python_style). Paired old-vs-new comparison.
#
# References:
# - Pencina MJ, D'Agostino RB Sr, Steyerberg EW. Extensions of net
#   reclassification improvement... Stat Med. 2011;30(1):11-21.
# - Uno H, Tian L, Cai T, et al. A unified inference procedure for a class of
#   measures to assess improvement in risk prediction... Stat Med. 2013.
# - Blanche P, Dartigues JF, Jacqmin-Gadda H. Estimating and comparing
#   time-dependent areas under ROC curves for censored event times with
#   competing risks. Stat Med. 2013;32(30):5381-5397. (competing-risk IPCW)
#
# =============================================================================
# _alt_scripts VERSION — audited 2026-05-30. Supersedes
# cons/_hist_scripts/nri_idi_from_results_boot.R for the results_boot workflow.
#
# CHANGES vs _hist_scripts:
#   1. READMISSION NRI/IDI is now COMPETING-RISK AWARE (readmit_method =
#      "aalen-johansen", default). The competing first-event status
#      (0=admin censoring, 1=readmission, 2=death) is reconstructed per replicate
#      by combining the readmission and death y_val blocks of the same fold (same
#      trick as the AJ DCA engine; row-aligned because both primary models share
#      strata). IPCW then uses the ADMINISTRATIVE-censoring distribution only
#      (deaths are NOT censoring), and patients who die without readmission count
#      as definitive NON-events for readmission (they are no longer dropped as if
#      censored). This makes readmission NRI/IDI consistent with the project's
#      Aalen-Johansen calibration and DCA. Set readmit_method = "ipcw" to
#      reproduce the old death-as-censoring behavior.
#      If a replicate's death block is missing/row-misaligned, that replicate
#      falls back to standard IPCW (counted in config$n_readmit_ipcw_fallback).
#   2. DEATH NRI/IDI is UNCHANGED (death is the terminal event; standard IPCW
#      with administrative censoring is already correct). Numerically identical
#      to the _hist engine.
#   3. do.call(rbind, .) -> do.call(rbind.data.frame, .) everywhere (mice's
#      rbind.mids S3 hijack, documented elsewhere in this project).
#   4. Legacy Cox-vs-XGBoost-pickle path (run_nri_idi_comparison,
#      calculate_nri_idi, load_xgb_predictions, bootstrap_nri_idi) was DROPPED:
#      unused by prediction225 and statistically weaker (ignored censoring in its
#      "km" mode). Use run_nri_idi_from_results_boot.
#
# CAVEAT (report honestly): the 95% "intervals" are empirical 2.5/97.5
# percentiles of the estimate ACROSS validation replicates (folds x imputations),
# i.e. split-to-split spread, NOT bootstrap/analytic CIs. NRI/IDI here is a
# SECONDARY / complementary metric; it must not drive model selection.
# =============================================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.rb_assert_results_boot <- function(results_boot, arg_name) {
  if (!is.list(results_boot)) {
    stop("`", arg_name, "` must be a list.", call. = FALSE)
  }
  if (!"raw_predictions" %in% names(results_boot)) {
    stop("`", arg_name, "` does not contain `raw_predictions`.", call. = FALSE)
  }
  if (!length(results_boot$raw_predictions)) {
    stop("`", arg_name, "$raw_predictions` is empty.", call. = FALSE)
  }
  invisible(TRUE)
}

.rb_normalize_horizons <- function(horizons, available_horizons) {
  available_horizons <- sort(unique(as.numeric(available_horizons)))
  if (is.null(horizons)) {
    return(available_horizons)
  }
  horizons <- sort(unique(as.numeric(horizons)))
  if (any(!is.finite(horizons))) {
    stop("`horizons` must be finite numeric values.", call. = FALSE)
  }
  missing_h <- setdiff(horizons, available_horizons)
  if (length(missing_h)) {
    stop(
      "Requested horizons are not available in both `results_boot` objects: ",
      paste(missing_h, collapse = ", "),
      call. = FALSE
    )
  }
  horizons
}

.rb_normalize_cut_points <- function(cut_points, horizons) {
  if (is.null(cut_points)) {
    return(NULL)
  }
  if (is.numeric(cut_points)) {
    cp <- sort(unique(as.numeric(cut_points)))
    if (any(!is.finite(cp)) || any(cp <= 0) || any(cp >= 1)) {
      stop("`cut_points` must contain probabilities strictly between 0 and 1.", call. = FALSE)
    }
    return(stats::setNames(rep(list(cp), length(horizons)), as.character(horizons)))
  }
  if (!is.list(cut_points) || is.null(names(cut_points))) {
    stop("`cut_points` must be NULL, a numeric vector, or a named list keyed by horizon.", call. = FALSE)
  }
  out <- vector("list", length(horizons))
  names(out) <- as.character(horizons)
  for (hh in horizons) {
    key <- as.character(hh)
    if (!key %in% names(cut_points)) {
      stop("Missing cut-points for horizon ", hh, ".", call. = FALSE)
    }
    cp <- sort(unique(as.numeric(cut_points[[key]])))
    if (any(!is.finite(cp)) || any(cp <= 0) || any(cp >= 1)) {
      stop("Cut-points for horizon ", hh, " must be in (0, 1).", call. = FALSE)
    }
    out[[key]] <- cp
  }
  out
}

.rb_extract_blocks <- function(results_boot, risk = c("readmission", "death")) {
  risk <- match.arg(risk)
  raw_predictions <- results_boot$raw_predictions
  blocks <- vector("list", length(raw_predictions))
  block_id <- 0L

  for (item in raw_predictions) {
    if (!risk %in% names(item)) next
    block <- item[[risk]]
    if (!is.list(block)) next
    if ("error" %in% names(block) && nzchar(block$error %||% "")) next

    eval_times <- as.numeric(item$eval_times %||% results_boot$config$eval_times)
    surv_mat <- as.matrix(block$surv_val_matrix)
    y_val <- block$y_val

    if (!is.matrix(surv_mat) || !nrow(surv_mat) || !ncol(surv_mat)) next
    if (ncol(surv_mat) != length(eval_times)) {
      stop("Mismatch between survival matrix columns and evaluation times for ", risk, ".", call. = FALSE)
    }
    if (!all(c("time", "event") %in% names(y_val))) {
      stop("`y_val` must contain `time` and `event`.", call. = FALSE)
    }
    if (nrow(y_val) != nrow(surv_mat)) {
      stop("Mismatch between `y_val` rows and `surv_val_matrix` rows.", call. = FALSE)
    }

    block_id <- block_id + 1L
    blocks[[block_id]] <- list(
      replicate_id = block_id,
      key = paste(risk, item$imp_idx %||% NA_integer_, item$fold_idx %||% NA_integer_, sep = "::"),
      ifkey = paste(item$imp_idx %||% NA_integer_, item$fold_idx %||% NA_integer_, sep = "::"),
      risk = risk,
      imp_idx = item$imp_idx %||% NA_integer_,
      fold_idx = item$fold_idx %||% NA_integer_,
      eval_times = eval_times,
      time = as.numeric(y_val$time),
      event = as.integer(y_val$event),
      pred_risk = 1 - surv_mat
    )
  }

  blocks <- Filter(Negate(is.null), blocks)
  if (!length(blocks)) {
    stop("No usable raw prediction blocks were found for `", risk, "`.", call. = FALSE)
  }
  blocks
}

.rb_align_blocks <- function(old_blocks, new_blocks, risk) {
  old_keys <- vapply(old_blocks, `[[`, character(1), "key")
  new_keys <- vapply(new_blocks, `[[`, character(1), "key")

  if (!setequal(old_keys, new_keys)) {
    stop("The old/new results objects do not contain the same replicate keys for ", risk, ".", call. = FALSE)
  }

  sorted_keys <- sort(old_keys)
  old_blocks <- old_blocks[match(sorted_keys, old_keys)]
  new_blocks <- new_blocks[match(sorted_keys, new_keys)]

  out <- vector("list", length(old_blocks))
  for (i in seq_along(old_blocks)) {
    old_block <- old_blocks[[i]]
    new_block <- new_blocks[[i]]

    # The two objects may have been evaluated on DIFFERENT eval_times grids
    # (e.g. a 12-point grid in results_upd2_py vs c(6,12,36,60) in a fresh object).
    # That is fine: both grids only need to contain the requested horizons, and we
    # index pred_old / pred_new each by its OWN grid downstream. We therefore keep
    # both grids instead of forcing them to be identical. What MUST match is the
    # validation rows per replicate (same patients => same CV fold split / seed).
    if (length(old_block$time) != length(new_block$time)) {
      stop("Validation row counts do not match for replicate ", old_block$key,
           ". The two objects must share the same CV fold split (same seed).", call. = FALSE)
    }
    if (!isTRUE(all.equal(old_block$time, new_block$time, tolerance = 0))) {
      stop("Validation times do not align for replicate ", old_block$key,
           ". The two objects must share the same CV fold split (same seed).", call. = FALSE)
    }
    if (!identical(old_block$event, new_block$event)) {
      stop("Validation events do not align for replicate ", old_block$key,
           ". The two objects must share the same CV fold split (same seed).", call. = FALSE)
    }

    out[[i]] <- list(
      replicate_id = i,
      key = old_block$key,
      ifkey = old_block$ifkey,
      risk = risk,
      imp_idx = old_block$imp_idx,
      fold_idx = old_block$fold_idx,
      eval_times_old = old_block$eval_times,
      eval_times_new = new_block$eval_times,
      time = old_block$time,
      event = old_block$event,
      pred_old = old_block$pred_risk,
      pred_new = new_block$pred_risk,
      use_cr = FALSE,
      cr_ftime = NULL,
      cr_fstatus = NULL
    )
  }
  out
}

.rb_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  sum(x[keep] * w[keep]) / sum(w[keep])
}

# ---- Standard IPCW (used for DEATH; treats admin censoring via 1 - event) ----
.rb_censor_survival_function <- function(time, event, g_min = 0.05) {
  fit <- survival::survfit(survival::Surv(time, 1 - event) ~ 1)
  function(tt) {
    surv <- summary(fit, times = tt, extend = TRUE)$surv
    surv <- as.numeric(surv)
    surv[!is.finite(surv)] <- NA_real_
    pmax(surv, g_min)
  }
}

.rb_ipcw_weights <- function(time, event, horizon, eps = 1e-8, g_min = 0.05) {
  Ghat <- .rb_censor_survival_function(time, event, g_min = g_min)
  g_t <- Ghat(horizon)[1]
  g_tm <- Ghat(pmax(time - eps, 0))
  list(
    event = ifelse(time <= horizon & event == 1L, 1 / g_tm, 0),
    nonevent = ifelse(time > horizon, 1 / g_t, 0)
  )
}

# ---- Competing-risk reconstruction + IPCW (used for READMISSION, AJ-aware) ----
# fstatus: 0 = administrative censoring, 1 = readmission first, 2 = death first.
.rb_cr_reconstruct <- function(readmit_time, readmit_event, death_time, death_event,
                               tie_action = "death_first") {
  rt <- as.numeric(readmit_time); re <- as.integer(readmit_event)
  dt <- as.numeric(death_time);   de <- as.integer(death_event)
  ftime <- pmin(rt, dt)
  r_first <- !is.na(re) & re == 1L & is.finite(ftime) & (abs(rt - ftime) <= 1e-08)
  d_first <- !is.na(de) & de == 1L & is.finite(ftime) & (abs(dt - ftime) <= 1e-08)
  fstatus <- rep(0L, length(ftime))
  fstatus[r_first & !d_first] <- 1L
  fstatus[d_first & !r_first] <- 2L
  both <- r_first & d_first
  if (any(both)) {
    fstatus[both] <- if (identical(tie_action, "readmit_first")) 1L else 2L
  }
  list(ftime = ftime, fstatus = fstatus)
}

.rb_cr_ipcw_weights <- function(ftime, fstatus, horizon, eps = 1e-8, g_min = 0.05) {
  # Censoring = administrative only (status 0). Readmission AND death are events.
  cens_indicator <- as.integer(fstatus == 0L)
  fitG <- survival::survfit(survival::Surv(ftime, cens_indicator) ~ 1)
  Ghat <- function(tt) {
    s <- summary(fitG, times = tt, extend = TRUE)$surv
    s <- as.numeric(s); s[!is.finite(s)] <- NA_real_
    pmax(s, g_min)
  }
  g_t  <- Ghat(horizon)[1]
  g_tm <- Ghat(pmax(ftime - eps, 0))
  is_event     <- (ftime <= horizon) & (fstatus == 1L)  # cause 1 = readmission
  is_compdeath <- (ftime <= horizon) & (fstatus == 2L)  # competing death -> non-event
  is_efree     <- (ftime >  horizon)                    # event-free at t  -> non-event
  list(
    event    = ifelse(is_event, 1 / g_tm, 0),
    nonevent = ifelse(is_compdeath, 1 / g_tm, ifelse(is_efree, 1 / g_t, 0))
  )
}

# ---- Shared NRI/IDI core given IPCW weights (identical formulas for both risks) ----
.rb_metrics_core <- function(old_risk, new_risk, w_event, w_nonevent, cut_points = NULL) {
  diff_risk <- new_risk - old_risk

  event_up      <- .rb_weighted_mean(as.numeric(diff_risk > 0), w_event)
  event_down    <- .rb_weighted_mean(as.numeric(diff_risk < 0), w_event)
  nonevent_up   <- .rb_weighted_mean(as.numeric(diff_risk > 0), w_nonevent)
  nonevent_down <- .rb_weighted_mean(as.numeric(diff_risk < 0), w_nonevent)

  mean_old_event    <- .rb_weighted_mean(old_risk, w_event)
  mean_new_event    <- .rb_weighted_mean(new_risk, w_event)
  mean_old_nonevent <- .rb_weighted_mean(old_risk, w_nonevent)
  mean_new_nonevent <- .rb_weighted_mean(new_risk, w_nonevent)

  disc_old <- mean_old_event - mean_old_nonevent
  disc_new <- mean_new_event - mean_new_nonevent

  out <- c(
    continuous_nri = (event_up - event_down) + (nonevent_down - nonevent_up),
    continuous_nri_events = event_up - event_down,
    continuous_nri_nonevents = nonevent_down - nonevent_up,
    idi = disc_new - disc_old,
    discrimination_old = disc_old,
    discrimination_new = disc_new,
    mean_risk_old_events = mean_old_event,
    mean_risk_new_events = mean_new_event,
    mean_risk_old_nonevents = mean_old_nonevent,
    mean_risk_new_nonevents = mean_new_nonevent
  )

  if (!is.null(cut_points)) {
    cat_old <- findInterval(old_risk, vec = cut_points, rightmost.closed = FALSE)
    cat_new <- findInterval(new_risk, vec = cut_points, rightmost.closed = FALSE)
    e_up  <- .rb_weighted_mean(as.numeric(cat_new > cat_old), w_event)
    e_dn  <- .rb_weighted_mean(as.numeric(cat_new < cat_old), w_event)
    ne_up <- .rb_weighted_mean(as.numeric(cat_new > cat_old), w_nonevent)
    ne_dn <- .rb_weighted_mean(as.numeric(cat_new < cat_old), w_nonevent)
    out <- c(out,
      categorical_nri = (e_up - e_dn) + (ne_dn - ne_up),
      categorical_nri_events = e_up - e_dn,
      categorical_nri_nonevents = ne_dn - ne_up
    )
  }
  out
}

.rb_metric_rows <- function(block, horizons, cut_points, g_min, old_label, new_label) {
  rows <- vector("list", length(horizons) * 16L)
  idx <- 0L

  for (horizon in horizons) {
    # Index each model's prediction matrix by ITS OWN eval_times grid, so the
    # two objects may differ in their full grids as long as both contain `horizon`.
    h_old <- which(block$eval_times_old == horizon)
    h_new <- which(block$eval_times_new == horizon)
    if (!length(h_old) || !length(h_new)) next

    old_risk <- as.numeric(block$pred_old[, h_old[1]])
    new_risk <- as.numeric(block$pred_new[, h_new[1]])

    if (isTRUE(block$use_cr)) {
      keep <- is.finite(old_risk) & is.finite(new_risk) &
        is.finite(block$cr_ftime) & !is.na(block$cr_fstatus)
      if (!any(keep)) next
      w <- .rb_cr_ipcw_weights(block$cr_ftime[keep], block$cr_fstatus[keep], horizon, g_min = g_min)
    } else {
      keep <- is.finite(old_risk) & is.finite(new_risk) &
        is.finite(block$time) & !is.na(block$event)
      if (!any(keep)) next
      w <- .rb_ipcw_weights(block$time[keep], block$event[keep], horizon, g_min = g_min)
    }

    cp_h <- if (is.null(cut_points)) NULL else cut_points[[as.character(horizon)]]
    metric_values <- .rb_metrics_core(old_risk[keep], new_risk[keep], w$event, w$nonevent, cut_points = cp_h)

    for (metric_name in names(metric_values)) {
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        risk = block$risk,
        replicate_id = block$replicate_id,
        imp_idx = block$imp_idx,
        fold_idx = block$fold_idx,
        horizon = horizon,
        metric = metric_name,
        estimate = unname(metric_values[[metric_name]]),
        old_model = old_label,
        new_model = new_label,
        stringsAsFactors = FALSE
      )
    }
  }

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame())
  }
  do.call(rbind.data.frame, rows)
}

.rb_summarize_metrics <- function(raw_metrics) {
  split_key <- interaction(raw_metrics$risk, raw_metrics$horizon, raw_metrics$metric, drop = TRUE, lex.order = TRUE)
  groups <- split(raw_metrics, split_key)

  out <- lapply(groups, function(df) {
    est <- as.numeric(df$estimate)
    est <- est[is.finite(est)]
    if (!length(est)) {
      mean_est <- NA_real_; q025 <- NA_real_; q975 <- NA_real_; n_est <- 0L
    } else {
      mean_est <- mean(est)
      q025 <- as.numeric(stats::quantile(est, probs = 0.025, names = FALSE, na.rm = TRUE))
      q975 <- as.numeric(stats::quantile(est, probs = 0.975, names = FALSE, na.rm = TRUE))
      n_est <- length(est)
    }
    data.frame(
      risk = df$risk[1], horizon = df$horizon[1], metric = df$metric[1],
      mean = mean_est, q025 = q025, q975 = q975, n = n_est,
      old_model = df$old_model[1], new_model = df$new_model[1],
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind.data.frame, out)

  # Keep the reported IDI exactly consistent with the reported discrimination
  # slopes. By construction idi = disc_new - disc_old per replicate (see
  # .rb_metrics_core), but each metric above is averaged over its OWN
  # finite-replicate set, so the three means can drift apart when NA patterns
  # differ across metrics. Reconcile the IDI POINT ESTIMATE (the across-replicate
  # MEAN) to disc_new_mean - disc_old_mean within each risk x horizon, so the
  # printed table always satisfies IDI = (updated slope) - (reference slope).
  # The percentile interval is left as the per-replicate IDI bootstrap spread.
  for (rk in unique(out$risk)) {
    for (hz in unique(out$horizon[out$risk == rk])) {
      sel   <- out$risk == rk & out$horizon == hz
      i_idi <- which(sel & out$metric == "idi")
      i_old <- which(sel & out$metric == "discrimination_old")
      i_new <- which(sel & out$metric == "discrimination_new")
      if (length(i_idi) == 1L && length(i_old) == 1L && length(i_new) == 1L &&
          is.finite(out$mean[i_old]) && is.finite(out$mean[i_new])) {
        out$mean[i_idi] <- out$mean[i_new] - out$mean[i_old]
      }
    }
  }

  out <- out[order(out$risk, out$horizon, out$metric), , drop = FALSE]
  rownames(out) <- paste(out$risk, out$horizon, out$metric, sep = ".")
  out
}

.rb_default_output_dir <- function(prefix = "results_boot_reclassification") {
  root <- tryCatch(here::here(), error = function(e) getwd())
  cons_dir <- if (basename(root) == "cons") root else file.path(root, "cons")
  out_root <- file.path(cons_dir, "_out")
  if (!dir.exists(out_root)) {
    out_root <- file.path(getwd(), "_out")
  }
  file.path(out_root, sprintf("%s_%s", prefix, format(Sys.time(), "%Y%m%d_%H%M%S")))
}

# Attach competing-risk first-event status to readmission paired blocks, using
# the death outcome from the OLD object's death block of the same imp::fold.
.rb_attach_competing <- function(paired_readmit, death_blocks_old, tie_action) {
  dkey <- vapply(death_blocks_old, `[[`, character(1), "ifkey")
  n_cr <- 0L; n_fallback <- 0L
  for (i in seq_along(paired_readmit)) {
    pb <- paired_readmit[[i]]
    j <- match(pb$ifkey, dkey)
    ok <- !is.na(j) && length(death_blocks_old[[j]]$time) == length(pb$time)
    if (isTRUE(ok)) {
      db <- death_blocks_old[[j]]
      cr <- .rb_cr_reconstruct(pb$time, pb$event, db$time, db$event, tie_action = tie_action)
      paired_readmit[[i]]$cr_ftime <- cr$ftime
      paired_readmit[[i]]$cr_fstatus <- cr$fstatus
      paired_readmit[[i]]$use_cr <- TRUE
      n_cr <- n_cr + 1L
    } else {
      paired_readmit[[i]]$use_cr <- FALSE
      n_fallback <- n_fallback + 1L
    }
  }
  attr(paired_readmit, "n_cr") <- n_cr
  attr(paired_readmit, "n_fallback") <- n_fallback
  paired_readmit
}

# readmit_method "aalen-johansen" (default) = competing-risk-aware readmission;
#                "ipcw" = legacy death-as-censoring (reproduces _hist behaviour).
run_nri_idi_from_results_boot <- function(
  results_boot_old,
  results_boot_new,
  horizons = NULL,
  cut_points = c(0.05, 0.10, 0.20),
  old_label = "old_model",
  new_label = "new_model",
  g_min = 0.05,
  readmit_method = c("aalen-johansen", "ipcw"),
  tie_action = c("death_first", "readmit_first"),
  output_dir = NULL,
  prefix = "results_boot_reclassification",
  save_raw = TRUE,
  verbose = TRUE
) {
  readmit_method <- match.arg(readmit_method)
  tie_action <- match.arg(tie_action)
  .rb_assert_results_boot(results_boot_old, "results_boot_old")
  .rb_assert_results_boot(results_boot_new, "results_boot_new")

  available_horizons_old <- results_boot_old$config$eval_times %||%
    unique(unlist(lapply(results_boot_old$raw_predictions, `[[`, "eval_times")))
  available_horizons_new <- results_boot_new$config$eval_times %||%
    unique(unlist(lapply(results_boot_new$raw_predictions, `[[`, "eval_times")))

  horizons <- .rb_normalize_horizons(horizons, intersect(available_horizons_old, available_horizons_new))
  cut_points <- .rb_normalize_cut_points(cut_points, horizons)

  n_readmit_cr <- 0L
  n_readmit_fallback <- 0L

  risks <- c("readmission", "death")
  raw_parts <- lapply(risks, function(risk_name) {
    old_blocks <- .rb_extract_blocks(results_boot_old, risk = risk_name)
    new_blocks <- .rb_extract_blocks(results_boot_new, risk = risk_name)
    paired_blocks <- .rb_align_blocks(old_blocks, new_blocks, risk = risk_name)

    if (risk_name == "readmission" && identical(readmit_method, "aalen-johansen")) {
      death_old <- tryCatch(.rb_extract_blocks(results_boot_old, risk = "death"),
                            error = function(e) list())
      if (length(death_old)) {
        paired_blocks <- .rb_attach_competing(paired_blocks, death_old, tie_action)
        n_readmit_cr <<- attr(paired_blocks, "n_cr") %||% 0L
        n_readmit_fallback <<- attr(paired_blocks, "n_fallback") %||% 0L
      } else {
        n_readmit_fallback <<- length(paired_blocks)
      }
    }

    metric_rows <- lapply(
      paired_blocks, .rb_metric_rows,
      horizons = horizons, cut_points = cut_points, g_min = g_min,
      old_label = old_label, new_label = new_label
    )
    metric_rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, metric_rows)
    if (!length(metric_rows)) {
      return(NULL)
    }
    do.call(rbind.data.frame, metric_rows)
  })

  raw_parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, raw_parts)
  if (!length(raw_parts)) {
    stop("No NRI/IDI results could be computed from the supplied `results_boot` objects.", call. = FALSE)
  }
  raw_metrics <- do.call(rbind.data.frame, raw_parts)
  summary_metrics <- .rb_summarize_metrics(raw_metrics)

  if (identical(readmit_method, "aalen-johansen") && isTRUE(verbose)) {
    message(sprintf(
      "Readmission NRI/IDI: competing-risk (Aalen-Johansen) IPCW on %d replicate(s)%s. Death: standard IPCW.",
      n_readmit_cr,
      if (n_readmit_fallback > 0L) sprintf("; %d fell back to death-as-censoring IPCW", n_readmit_fallback) else ""
    ))
  }

  output_dir <- output_dir %||% .rb_default_output_dir(prefix = prefix)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  summary_path <- file.path(output_dir, sprintf("%s_summary.csv", prefix))
  raw_path <- file.path(output_dir, sprintf("%s_raw.csv", prefix))
  rds_path <- file.path(output_dir, sprintf("%s_results.rds", prefix))

  utils::write.csv(summary_metrics, summary_path, row.names = FALSE)
  if (isTRUE(save_raw)) {
    utils::write.csv(raw_metrics, raw_path, row.names = FALSE)
  } else {
    raw_path <- NA_character_
  }

  result <- list(
    summary = summary_metrics,
    raw = raw_metrics,
    config = list(
      old_model = old_label,
      new_model = new_label,
      horizons = horizons,
      cut_points = cut_points,
      g_min = g_min,
      readmit_method = readmit_method,
      death_method = "ipcw",
      tie_action = tie_action,
      n_readmit_competing_risk = n_readmit_cr,
      n_readmit_ipcw_fallback = n_readmit_fallback,
      reference = c(
        "Pencina et al. 2011; doi:10.1002/sim.4085",
        "Uno et al. 2013; doi:10.1002/sim.5647",
        "Blanche et al. 2013; doi:10.1002/sim.5958 (competing-risk IPCW)"
      ),
      files = list(summary = summary_path, raw = raw_path, rds = rds_path)
    )
  )
  saveRDS(result, rds_path)
  result
}
