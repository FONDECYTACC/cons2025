# =============================================================================
# extval_death_03_ibs_grid_cc.R
#
# ############################################################################
# # READ THIS FIRST (2026-07-27). PART (A) OF THIS FILE IS DEPRECATED FOR    #
# # prediction27_ext_val_death. USE ONLY PART (B).                           #
# #                                                                          #
# # Part (A) reimplements IBS on a fine grid anchored at 0 and normalised by  #
# # the horizon. That is the classical definition and it is what              #
# # prediction26_ext_val_readm_mod does, but it is NOT what the project's     #
# # authority notebook does. prediction23_converted_mod integrates the IPCW   #
# # Brier over exactly EVAL_TIMES = c(3, 6, 12, 36, 60), anchored at 3 and    #
# # normalised by max - min, in all four of its IBS paths (main dualscore,    #
# # ibs_window bootstrap, the Aalen-Johansen CIF path, and the arm            #
# # comparison). prediction27 already matches that contract exactly.          #
# # Switching pred27 to Part (A) would make it INCOMPARABLE with pred23,      #
# # which is the opposite of what is wanted. Do not do it.                    #
# #                                                                          #
# # Part (A) is retained only as a sensitivity tool and as the reference      #
# # implementation of pred26_mod's definition. Part (B), the complete-case    #
# # diagnostics, is unaffected by any of this and IS the part to use: it      #
# # touches no metric, only reporting.                                        #
# ############################################################################
#
# Two additions that bring prediction27_ext_val_death into line with
# prediction26_ext_val_readm_mod:
#
#   (A) IBS on a FINE evaluation grid, using the SAME definition as
#       prediction26_ext_val_readm_mod's inline engine (cell
#       'calibration-brier-and-ibs-functions'), so the mortality and readmission
#       external validations report one comparable estimand.
#
#   (B) Complete-case diagnostics: missingness counts before the filter, a flow
#       row for the complete-case step, and an SMD table comparing retained
#       complete cases against the rows dropped for missingness. This mirrors
#       'cohort-diagnostics' in prediction26_ext_val_readm_mod.
#
# WHY (A) IS NOT COSMETIC
# -----------------------
# evaluate_extval_death() currently integrates the Brier curve with
# ibs_ipcw_train(), whose quadrature grid IS eval_times. With
# eval_times = c(3, 6, 12, 36, 60) the "integral" is a 5-point trapezoid, and
# the per-horizon values are worse still: IBS at 3 months is undefined (1 point),
# IBS at 6 months integrates over {3, 6}. ibs_ipcw_train() also normalises by
# max(times) - min(times), so it starts the integral at 3 months, whereas the
# standard IBS and prediction26_ext_val_readm_mod both integrate from 0 and
# divide by the horizon.
#
# Measured on a simulated convenio-3-like cohort (n = 1900, 95 deaths, follow-up
# to 79 months, identical predictions fed to both estimators):
#
#   horizon   grid pts (now)   IBS now     grid pts (fine)   IBS fine    diff
#         3                1        NA                  13   0.001002      --
#         6                2   0.006739                 25   0.004118   +63.6%
#        12                3   0.011095                 49   0.008796   +26.1%
#        36                4   0.024766                145   0.022143   +11.8%
#        60                5   0.035850                241   0.034711    +3.3%
#
# The bias is upward and largest at the short horizons that matter most for
# post-discharge mortality. Reproduce with the script noted at the bottom.
#
# WHAT THIS FILE DOES NOT CHANGE
# ------------------------------
# Nothing about the frozen model, the cohort definition, the concordance
# convention (per-horizon absolute risk; global on the linear predictor), or the
# calibration engine. Only the Brier/IBS quadrature and the complete-case
# reporting.
#
# REQUIREMENTS
# ------------
# Source AFTER run_extval_death.R, which brings in predict_death_pooled(), to01(),
# .dual_cox_safe_concordance() and the .vhm_* calibration helpers. Nothing here
# depends on extval_death_02_table1_smd.R.
#
# Public API:
#   evd_evaluation_grid(horizons, step)
#   evd_ibs_to_horizons(time, event, risk_matrix, times, horizons)
#   evd_evaluate_death(fits, dat, horizons, grid_step)
#   evd_bootstrap_death(fits, dat, horizons, cal_times, B, seed, grid_step, ...)
#   evd_missingness_report(nondum, need)
#   evd_complete_case_flow(flow, n_before, n_after)
#   evd_smd_complete_vs_dropped(nondum, cc_index)
# =============================================================================

suppressWarnings(suppressMessages(library(survival)))

# ---- 0. Model-term contract, stated explicitly (mirrors pred26_mod's
#         'frozen-formula' cell, which writes the contract out and asserts it) --
EVD_COEFFICIENT_COLUMNS <- c(
  "adm_age_rec3",
  "primary_sub_mod_cocaine_paste", "primary_sub_mod_cocaine_powder",
  "primary_sub_mod_alcohol", "primary_sub_mod_others",
  "prim_sub_freq_rec_2_2_6_days_wk", "prim_sub_freq_rec_3_daily",
  "occupation_condition_corr24_unemployed", "occupation_condition_corr24_inactive",
  "eva_ocupacion_logro_intermedio", "eva_ocupacion_logro_minimo",
  "cohabitation_family_of_origin", "cohabitation_with_couple_children",
  "cohabitation_others"
)
EVD_STRATA_COLUMNS <- c("plan_type_strata", "any_phys_dx")
EVD_OUTCOME_COLUMNS <- c("death_time_from_disch_m", "death_event")
EVD_CONTINUOUS_COLUMNS <- c("adm_age_rec3")

# Source (pre-dummy) columns that must be complete for a row to enter the
# complete-case analysis. Kept identical to the `need` vector inside
# dummify_extval_death() so the two never drift apart.
EVD_NEED_COMPLETE <- c(
  "primary_sub_mod", "prim_sub_freq_rec", "occupation_condition_corr24",
  "eva_ocupacion", "cohabitation", "plan_type_corr",
  "adm_age_rec3", "any_phys_dx", "death_time_from_disch_m", "death_event"
)

# =============================================================================
# (A) IBS ON A FINE GRID -- definition shared with pred26_mod
# =============================================================================

# Build the evaluation grid: from 0 to the largest horizon, at `step` months,
# with the exact reporting horizons always included.
evd_evaluation_grid <- function(horizons, step = 0.25) {
  stopifnot(is.numeric(horizons), length(horizons) >= 1L, step > 0)
  sort(unique(c(0, seq(step, max(horizons), by = step), horizons)))
}

# Censoring survival function G(t) = P(C > t), estimated on the cohort being
# evaluated. Verbatim from pred26_mod's censoring_survival_function(), including
# the 0.01 floor, so the two external validations weight identically.
evd_censoring_survival_function <- function(time, event, minimum = 0.01) {
  fit <- survival::survfit(survival::Surv(time, 1L - event) ~ 1, conf.type = "none")
  function(query, before = FALSE) {
    if (!length(fit$time)) return(rep(1, length(query)))
    # fit$time is already sorted by survfit, so findInterval is order-safe here.
    index <- findInterval(query, fit$time, left.open = before)
    value <- rep(1, length(query))
    has_step <- index > 0L
    value[has_step] <- fit$surv[index[has_step]]
    pmax(value, minimum)
  }
}

# IPCW Brier score at one horizon, given a prebuilt G. Hoisting G out of the
# per-horizon loop is what makes the fine grid affordable: G depends only on
# (time, event), never on the horizon, so this is numerically identical to
# pred26_mod's ipcw_brier_at() while avoiding one survfit call per grid point.
evd_ipcw_brier_at <- function(time, event, survival_probability, horizon, G = NULL) {
  if (is.null(G)) G <- evd_censoring_survival_function(time, event)
  event_by_horizon <- event == 1L & time <= horizon
  event_free_at_horizon <- time > horizon
  weight <- numeric(length(time))
  weight[event_by_horizon] <- 1 / G(time[event_by_horizon], before = TRUE)
  weight[event_free_at_horizon] <- 1 / G(rep(horizon, sum(event_free_at_horizon)))
  outcome_survival <- as.numeric(time > horizon)
  mean(weight * (outcome_survival - survival_probability)^2)
}

# Brier score at every grid point.
evd_brier_curve <- function(time, event, risk_matrix, times) {
  stopifnot(ncol(risk_matrix) == length(times))
  G <- evd_censoring_survival_function(time, event)
  vapply(seq_along(times), function(j) {
    evd_ipcw_brier_at(time, event, 1 - risk_matrix[, j], times[[j]], G = G)
  }, numeric(1))
}

# IBS up to each horizon: trapezoidal area under the Brier curve from 0,
# normalised by the horizon. Same definition as pred26_mod's ibs_to_horizons().
evd_ibs_to_horizons <- function(time, event, risk_matrix, times, horizons) {
  brier <- evd_brier_curve(time, event, risk_matrix, times)
  rows <- lapply(horizons, function(horizon) {
    keep <- times <= horizon
    x <- times[keep]; y <- brier[keep]
    if (!length(x)) return(data.frame(horizon_months = horizon, ibs = NA_real_))
    # Anchor the integral at the origin: at t = 0 nobody has had the event and
    # predicted risk is 0, so the Brier score is 0 by construction.
    if (x[[1L]] > 0) { x <- c(0, x); y <- c(0, y) }
    area <- sum(diff(x) * (utils::head(y, -1L) + utils::tail(y, -1L)) / 2)
    data.frame(horizon_months = horizon, ibs = area / horizon)
  })
  list(summary = do.call(rbind, rows),
       curve = data.frame(time_months = times, brier = brier))
}

# Drop-in replacement for evaluate_extval_death(): identical concordance
# convention, IBS recomputed on the fine grid.
evd_evaluate_death <- function(fits, dat, horizons = c(3, 6, 12, 36, 60),
                               grid_step = 0.25,
                               time_col = "death_time_from_disch_m",
                               event_col = "death_event",
                               verbose = TRUE) {
  horizons <- sort(unique(as.numeric(horizons)))
  grid <- evd_evaluation_grid(horizons, grid_step)
  if (verbose) cat(sprintf("Evaluation grid: %d points, step %.2f mo, max %.0f mo\n",
                           length(grid), grid_step, max(horizons)))

  pr_grid <- predict_death_pooled(fits, dat, grid)
  h_index <- match(horizons, grid)
  stopifnot(!anyNA(h_index))

  tt <- as.numeric(dat[[time_col]])
  ee <- to01(dat[[event_col]])
  tmax <- max(tt, na.rm = TRUE)

  # Global concordance on the linear predictor (horizon-independent), matching
  # the existing convention and pred26_mod's supplementary table.
  c_global <- .dual_cox_safe_concordance(tt, ee, pr_grid$lp)

  # Per-horizon concordance on absolute risk at that horizon.
  c_h <- vapply(seq_along(horizons), function(ii) {
    h <- horizons[ii]
    if (h < tmax) .dual_cox_safe_concordance(tt, ee, pr_grid$risk_mat[, h_index[ii]], ymax = h)
    else NA_real_
  }, numeric(1))

  ibs <- evd_ibs_to_horizons(tt, ee, pr_grid$risk_mat, grid, horizons)
  # "Global" IBS is the value at the longest horizon, which is what the
  # normalise-by-horizon definition makes it.
  ibs_global <- ibs$summary$ibs[ibs$summary$horizon_months == max(horizons)]

  rows <- rbind(
    data.frame(Metric = "Uno's C-Index", Time = "Global", Value = c_global),
    data.frame(Metric = "IBS",           Time = "Global", Value = ibs_global),
    data.frame(Metric = "Uno's C-Index", Time = as.character(horizons), Value = c_h),
    data.frame(Metric = "IBS",           Time = as.character(horizons), Value = ibs$summary$ibs)
  )
  list(summary = rows, lp = pr_grid$lp, surv_mat = pr_grid$surv_mat,
       risk_mat = pr_grid$risk_mat, grid = grid, grid_step = grid_step,
       eval_times = horizons, brier_curve = ibs$curve,
       n = nrow(dat), n_events = sum(ee == 1L, na.rm = TRUE),
       ibs_definition = "trapezoid from 0 on a fine grid, normalised by horizon (pred26_mod convention)")
}

# Bootstrap CIs on the same fine grid. Predictions are computed ONCE outside the
# replicate loop and only re-indexed, exactly as bootstrap_extval_death() does.
evd_bootstrap_death <- function(fits, dat, horizons = c(3, 6, 12, 36, 60),
                                cal_times = c(6, 12, 36, 60), B = 1000L, seed = 2125L,
                                grid_step = 0.25, do_cal = TRUE, parallel = TRUE,
                                time_col = "death_time_from_disch_m",
                                event_col = "death_event", verbose = TRUE) {
  horizons  <- sort(unique(as.numeric(horizons)))
  cal_times <- sort(unique(as.numeric(cal_times)))
  grid <- evd_evaluation_grid(sort(unique(c(horizons, cal_times))), grid_step)
  h_index <- match(horizons, grid)
  c_index <- match(cal_times, grid)
  stopifnot(!anyNA(h_index), !anyNA(c_index))

  pr <- predict_death_pooled(fits, dat, grid)
  lp <- pr$lp; risk_all <- pr$risk_mat
  tt <- as.numeric(dat[[time_col]]); ee <- to01(dat[[event_col]]); n <- length(tt)
  if (verbose) cat(sprintf("Bootstrap: B=%d, grid %d points (step %.2f mo), n=%d\n",
                           B, length(grid), grid_step, n))

  one_rep <- function(bi) {
    t <- tt[bi]; e <- ee[bi]; s <- lp[bi]
    rv <- risk_all[bi, , drop = FALSE]
    tmax <- max(t, na.rm = TRUE)
    cg <- .dual_cox_safe_concordance(t, e, s)
    ch <- vapply(seq_along(horizons), function(ii) {
      h <- horizons[ii]
      if (h < tmax) .dual_cox_safe_concordance(t, e, rv[, h_index[ii]], ymax = h) else NA_real_
    }, numeric(1))
    ib <- evd_ibs_to_horizons(t, e, rv, grid, horizons)$summary$ibs
    ig <- ib[length(ib)]
    ici <- eo <- stats::setNames(rep(NA_real_, length(cal_times)), NULL)
    if (do_cal) {
      ici <- vapply(seq_along(cal_times), function(jj) {
        h <- cal_times[jj]; risk <- rv[, c_index[jj]]
        obs_overall <- .vhm_km_risk(t, e, h)
        res <- tryCatch(.vhm_calibrate_one(risk, h, obs_overall,
          observed_by_bin_fun = function(idx) .vhm_km_risk(t[idx], e[idx], h),
          g = 10L, min_bin_n = 20L, span = 0.75), error = function(err) NULL)
        if (is.null(res)) NA_real_ else res$metrics$ici
      }, numeric(1))
      eo <- vapply(seq_along(cal_times), function(jj) {
        h <- cal_times[jj]; mp <- mean(rv[, c_index[jj]], na.rm = TRUE)
        ob <- .vhm_km_risk(t, e, h); if (is.finite(ob) && ob > 0) mp / ob else NA_real_
      }, numeric(1))
    }
    c(c_global = cg, ibs_global = ig,
      stats::setNames(ch, paste0("c_",   horizons)),
      stats::setNames(ib, paste0("ibs_", horizons)),
      stats::setNames(ici, paste0("ici_", cal_times)),
      stats::setNames(eo,  paste0("eo_",  cal_times)))
  }

  set.seed(seed)
  idx <- lapply(seq_len(B), function(i) sample.int(n, n, replace = TRUE))
  reps <- if (parallel && requireNamespace("future.apply", quietly = TRUE))
    future.apply::future_lapply(idx, one_rep, future.seed = TRUE) else lapply(idx, one_rep)
  M <- do.call(rbind, reps)
  data.frame(metric = colnames(M),
             lo = apply(M, 2, stats::quantile, 0.025, na.rm = TRUE, names = FALSE),
             hi = apply(M, 2, stats::quantile, 0.975, na.rm = TRUE, names = FALSE),
             row.names = NULL)
}

# =============================================================================
# (B) COMPLETE-CASE DIAGNOSTICS
# =============================================================================

# Missingness counts on the source columns, BEFORE the complete-case filter.
evd_missingness_report <- function(nondum, need = EVD_NEED_COMPLETE) {
  present <- intersect(need, names(nondum))
  absent  <- setdiff(need, names(nondum))
  if (length(absent))
    warning("Columns absent from the cohort and skipped: ",
            paste(absent, collapse = ", "), call. = FALSE)
  out <- data.frame(
    variable = present,
    missing = vapply(nondum[present], function(v) sum(is.na(v)), integer(1)),
    stringsAsFactors = FALSE
  )
  out$pct <- round(100 * out$missing / nrow(nondum), 2)
  out[order(-out$missing), ]
}

# Append the complete-case step to the cohort flow table returned by
# build_extval_death_cohort(), so the flow ends at the analysed n.
evd_complete_case_flow <- function(flow, n_before, n_after, n_events_after = NA_integer_) {
  extra <- data.frame(
    step = c("complete case on mortality predictors and outcome",
             "dropped for missingness"),
    n = c(n_after, n_before - n_after),
    stringsAsFactors = FALSE
  )
  if (!is.na(n_events_after)) {
    extra <- rbind(extra, data.frame(
      step = "deaths in complete-case sample (ANALYSED)", n = n_events_after,
      stringsAsFactors = FALSE))
  }
  rbind(flow, extra)
}

# SIGNED standardised mean differences.
# The project's .smd_bin()/.smd_cont() in extval_02_table1_smd.R return absolute
# values, which is right for a development-versus-external balance table. Here the
# direction is the point (do the dropped rows skew older, sicker, more residential?),
# so these mirror pred26_mod's signed smd_numeric()/smd_binary() instead. Positive
# means the DROPPED rows sit above the retained complete cases.
.evd_smd_cont_signed <- function(reference, target) {
  denominator <- sqrt((stats::var(reference, na.rm = TRUE) + stats::var(target, na.rm = TRUE)) / 2)
  if (!is.finite(denominator) || denominator == 0) return(NA_real_)
  (mean(target, na.rm = TRUE) - mean(reference, na.rm = TRUE)) / denominator
}
.evd_smd_bin_signed <- function(p_reference, p_target) {
  denominator <- sqrt((p_reference * (1 - p_reference) + p_target * (1 - p_target)) / 2)
  if (!is.finite(denominator) || denominator == 0) return(NA_real_)
  (p_target - p_reference) / denominator
}

# SMD between the retained complete cases and the rows dropped for missingness.
# The dropped rows are incomplete by construction, so each variable is compared
# on its own observed values (pairwise), not on complete rows.
evd_smd_complete_vs_dropped <- function(nondum, cc_index,
                                        need = EVD_NEED_COMPLETE,
                                        continuous = EVD_CONTINUOUS_COLUMNS) {
  stopifnot(is.numeric(cc_index) || is.integer(cc_index))
  keep <- rep(FALSE, nrow(nondum)); keep[cc_index] <- TRUE
  a <- nondum[keep, , drop = FALSE]     # retained
  b <- nondum[!keep, , drop = FALSE]    # dropped
  if (!nrow(b)) {
    message("No rows were dropped for missingness; SMD table is empty.")
    return(data.frame(variable = character(0), complete_mean = numeric(0),
                      dropped_mean = numeric(0), smd = numeric(0), flag = character(0)))
  }
  vars <- intersect(need, names(nondum))
  rows <- lapply(vars, function(v) {
    x <- a[[v]]; y <- b[[v]]
    if (v %in% continuous || (is.numeric(x) && !is.logical(x) &&
                              length(unique(stats::na.omit(x))) > 2L)) {
      xn <- as.numeric(x); yn <- as.numeric(y)
      data.frame(variable = v, complete_mean = mean(xn, na.rm = TRUE),
                 dropped_mean = mean(yn, na.rm = TRUE),
                 smd = .evd_smd_cont_signed(xn, yn),
                 stringsAsFactors = FALSE)
    } else if (is.logical(x) || length(unique(stats::na.omit(as.character(x)))) <= 2L) {
      px <- mean(as.numeric(as.logical(x)), na.rm = TRUE)
      py <- mean(as.numeric(as.logical(y)), na.rm = TRUE)
      data.frame(variable = v, complete_mean = px, dropped_mean = py,
                 smd = .evd_smd_bin_signed(px, py), stringsAsFactors = FALSE)
    } else {
      # Categorical: one row per level, on the level indicator.
      levs <- sort(unique(stats::na.omit(as.character(c(x, y)))))
      do.call(rbind, lapply(levs, function(l) {
        px <- mean(as.character(x) == l, na.rm = TRUE)
        py <- mean(as.character(y) == l, na.rm = TRUE)
        data.frame(variable = paste0(v, ": ", l), complete_mean = px,
                   dropped_mean = py, smd = .evd_smd_bin_signed(px, py),
                   stringsAsFactors = FALSE)
      }))
    }
  })
  out <- do.call(rbind, rows)
  out$flag <- ifelse(is.finite(out$smd) & abs(out$smd) > 0.1, "imbalanced", "")
  out[order(-abs(replace(out$smd, !is.finite(out$smd), 0))), ]
}

# -----------------------------------------------------------------------------
# Verification note
# The IBS quadrature comparison quoted in the header is reproducible from
# scratchpad/ibs_definition_check.R, which loads the real ibs_ipcw_train() from
# evaluate_dual_cox_python_style_boot.R (lines 11-103) and feeds both estimators
# identical predictions on the same simulated cohort.
# -----------------------------------------------------------------------------
