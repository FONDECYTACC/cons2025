# =====================================================================================
# ibs_window_bootstrap_holdout.R
# Per-window IBS with patient-level bootstrap CIs on the held-out 20% test set,
# WITH THE MODEL FROZEN. It does NOT refit anything: it reuses the frozen-model
# predicted survival already stored in validation$results_boot_val[[model]]$raw_predictions
# (surv_val_matrix, y_val, y_train per imputation, on eval_times = c(3,6,12,36,60)), and
# resamples only the validation rows. This gives the IBS the same patient-bootstrap
# uncertainty the C-index/AUC get from the ipeval routine, instead of the 5-imputation
# spread currently stored.
#
# ESTIMAND (matches the stored dual IBS exactly):
#   IBS(t) = time-averaged IPCW Brier over [min(grid), t], by trapezoid, divided by
#   (t - min(grid)). The grid lower bound is min(eval_times) = 3 (NOT 0), so IBS(3) is
#   undefined (one point) and is returned as NA; windows are 6, 12, 36, 60.
#   Censoring weights G() are estimated on TRAIN (frozen), identical to
#   ibs_ipcw_train() in evaluate_dual_cox_python_style_boot.R.
#
# READMISSION estimand options:
#   readmit_method = "ipcw"           -> death treated as censoring (cause-specific),
#                                        reproduces the STORED readmission IBS exactly.
#   readmit_method = "aalen-johansen" -> competing-risk aware (death = definitive
#                                        non-event, administrative-censoring G), matching
#                                        the project's AJ calibration / DCA / NRI engines.
#                                        The prediction stays 1 - S_cause-specific (the
#                                        project convention); only the observed side is
#                                        competing-risk aware. This is a DIFFERENT estimand
#                                        from the stored readmission IBS (no stored
#                                        reference to validate against).
#   DEATH is unaffected (terminal event; standard IPCW with administrative censoring).
#
# Depends: survival. No refit, no covariate data needed.
# =====================================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.ibsb_to01 <- function(x) as.integer(as.numeric(x) > 0)

# Step function S(t) of the censoring (or admin-censoring) distribution, floored at g_min.
.ibsb_G_fun <- function(time, cens_event, g_min = 0.05) {
  sf <- survival::survfit(survival::Surv(time, cens_event) ~ 1)
  list(
    fun = function(tt) {
      requested_times <- pmax(as.numeric(tt), 0)
      unique_times <- sort(unique(requested_times))
      g_unique <- as.numeric(summary(sf, times = unique_times, extend = TRUE)$surv)

      if (length(g_unique) != length(unique_times))
        stop("Censoring-survival lookup returned an unexpected length.", call. = FALSE)

      lookup_index <- match(requested_times, unique_times)
      if (anyNA(lookup_index))
        stop("Censoring-survival lookup could not restore request order.", call. = FALSE)

      pmax(g_unique[lookup_index], g_min)
    },
    # largest time where the censoring survival is still >= g_min (IPCW horizon cap)
    tau = {
      safe <- sf$time[sf$surv >= g_min]
      if (length(safe)) max(safe) else NA_real_
    }
  )
}

# Competing-risk first-event reconstruction: 0 = admin censoring, 1 = readmission, 2 = death.
.ibsb_cr <- function(readmit_time, readmit_event, death_time, death_event) {
  rt <- as.numeric(readmit_time); re <- .ibsb_to01(readmit_event)
  dt <- as.numeric(death_time);   de <- .ibsb_to01(death_event)
  ftime <- pmin(rt, dt)
  r_first <- re == 1L & is.finite(ftime) & abs(rt - ftime) <= 1e-8
  d_first <- de == 1L & is.finite(ftime) & abs(dt - ftime) <= 1e-8
  fstatus <- rep(0L, length(ftime))
  fstatus[r_first & !d_first] <- 1L
  fstatus[d_first & !r_first] <- 2L
  fstatus[r_first & d_first]  <- 2L          # ties -> death first (project convention)
  list(ftime = ftime, fstatus = fstatus)
}

# Per-row, per-grid IPCW Brier contribution matrix C (n x K). The window IBS for any
# patient resample is then trapz over the grid of weighted column means of C, divided by
# the window length, so the bootstrap is a cheap reweighting (no recomputation of G).
.ibsb_contrib_death <- function(surv_val, time_val, event_val, time_train, event_train,
                                grid, g_min, eps) {
  G <- .ibsb_G_fun(time_train, 1 - .ibsb_to01(event_train), g_min = g_min)
  tau <- G$tau %||% NA_real_
  tmax_train <- max(time_train, na.rm = TRUE)
  keep <- grid <= (if (is.finite(tau)) tau else max(grid)) & grid <= tmax_train
  gk <- grid[keep]
  ev <- .ibsb_to01(event_val)
  C <- matrix(0, nrow = length(time_val), ncol = length(gk))
  for (j in seq_along(gk)) {
    t0 <- gk[j]
    Y0 <- as.numeric(time_val > t0)
    w  <- ifelse(time_val <= t0 & ev == 1L, 1 / G$fun(pmax(time_val - eps, 0)),
                 ifelse(time_val > t0, 1 / G$fun(t0), 0))
    C[, j] <- w * (Y0 - surv_val[, which(keep)[j]])^2
  }
  list(C = C, grid = gk)
}

.ibsb_contrib_readmit_ipcw <- function(surv_val, time_val, event_val, time_train, event_train,
                                       grid, g_min, eps) {
  # death-as-censoring (cause-specific): identical machinery to death, on readmit times
  .ibsb_contrib_death(surv_val, time_val, event_val, time_train, event_train, grid, g_min, eps)
}

.ibsb_contrib_readmit_aj <- function(surv_val,
                                     rt_val, re_val, dt_val, de_val,
                                     rt_tr,  re_tr,  dt_tr,  de_tr,
                                     grid, g_min, eps) {
  cr_v <- .ibsb_cr(rt_val, re_val, dt_val, de_val)
  cr_t <- .ibsb_cr(rt_tr,  re_tr,  dt_tr,  de_tr)
  G <- .ibsb_G_fun(cr_t$ftime, as.integer(cr_t$fstatus == 0L), g_min = g_min)  # admin censoring on TRAIN
  tau <- G$tau %||% NA_real_
  tmax_train <- max(cr_t$ftime, na.rm = TRUE)
  keep <- grid <= (if (is.finite(tau)) tau else max(grid)) & grid <= tmax_train
  gk <- grid[keep]
  ft <- cr_v$ftime; fs <- cr_v$fstatus
  C <- matrix(0, nrow = length(ft), ncol = length(gk))
  for (j in seq_along(gk)) {
    t0 <- gk[j]
    is_readmit  <- fs == 1L & ft <= t0                 # event   -> Y0 = 0
    is_death    <- fs == 2L & ft <= t0                 # competing death -> definitive non-event, Y0 = 1
    is_efree    <- ft > t0                              # event-free at t0 -> Y0 = 1
    Y0 <- as.numeric(!is_readmit)                       # "not yet readmitted" survival scale
    w  <- ifelse(is_readmit | is_death, 1 / G$fun(pmax(ft - eps, 0)),
                 ifelse(is_efree, 1 / G$fun(t0), 0))     # admin-censored before t0 -> w = 0
    C[, j] <- w * (Y0 - surv_val[, which(keep)[j]])^2
  }
  list(C = C, grid = gk)
}

# IBS over [min(full grid), t] for every requested window t, from a contribution matrix.
# Normalizes by (t - t_min) and returns NA when t is not actually a node of the kept grid
# (e.g. dropped by the IPCW tau cap), so a returned value is never mislabeled vs [t_min, t].
.ibsb_windows_from_C <- function(C, grid, eval_times, row_weight) {
  cm <- as.numeric(row_weight %*% C) / sum(row_weight)   # weighted column means (the bootstrap)
  t_min <- min(eval_times)
  vapply(eval_times, function(t) {
    if (t <= t_min) return(NA_real_)
    if (!any(abs(grid - t) < 1e-8)) return(NA_real_)      # t must be a node of the kept grid
    sel <- grid <= t
    if (sum(sel) < 2L) return(NA_real_)
    g <- grid[sel]; b <- cm[sel]
    sum(diff(g) * (utils::head(b, -1L) + utils::tail(b, -1L)) / 2) / (t - t_min)
  }, numeric(1))
}

# Marginal (intercept-only) survival prediction on TRAIN, evaluated at the grid. This is
# the NULL reference for a Brier skill comparison: predict the cohort-average curve to
# everyone, scored with the SAME IPCW weights/grid as the model. It must use the same
# estimand as the model IBS, so the readmission null follows readmit_method
# (KM death-as-censoring vs 1 - Aalen-Johansen CIF for cause 1).
.ibsb_marg_km <- function(time, event, grid) {
  sf <- survival::survfit(survival::Surv(time, .ibsb_to01(event)) ~ 1)
  as.numeric(summary(sf, times = grid, extend = TRUE)$surv)
}
.ibsb_marg_aj_surv <- function(ftime, fstatus, grid) {
  sf <- survival::survfit(survival::Surv(ftime, factor(fstatus, levels = c(0L, 1L, 2L))) ~ 1)
  sm <- summary(sf, times = grid, extend = TRUE)
  col1 <- which(colnames(sm$pstate) == "1")
  1 - as.numeric(sm$pstate[, col1])
}

ibs_window_bootstrap_core <- function(results_boot,
                                      B              = 1000,
                                      seed           = 2125,
                                      eval_times     = c(3, 6, 12, 36, 60),
                                      readmit_method = c("ipcw", "aalen-johansen"),
                                      g_min          = 0.05,
                                      eps            = 1e-8,
                                      compute_null   = TRUE,
                                      verbose        = TRUE) {

  readmit_method <- match.arg(readmit_method)   # default "ipcw" reproduces the stored dual IBS
  rp <- results_boot$raw_predictions
  if (is.null(rp) || !length(rp)) stop("`results_boot` has no $raw_predictions.", call. = FALSE)

  # Bootstrap row weights are applied POSITIONALLY across imputations, so the validation
  # rows must be the same patients in the same order in every imputation. Assert it.
  if (length(rp) > 1L) {
    ref <- rp[[1]]$death$y_val
    for (k in 2:length(rp)) {
      yk <- rp[[k]]$death$y_val
      if (!isTRUE(all.equal(as.numeric(ref$time), as.numeric(yk$time))) ||
          !identical(as.integer(ref$event), as.integer(yk$event)))
        stop("Validation rows differ across imputations (death y_val); cannot pool positionally.",
             call. = FALSE)
    }
  }

  # ---- precompute the frozen per-imputation contribution matrices (no resample yet) ----
  per_imp <- lapply(rp, function(item) {
    et <- as.numeric(item$eval_times)
    grid <- sort(unique(intersect(et, eval_times)))
    d <- item$death; r <- item$readmission
    n_i <- nrow(d$surv_val_matrix)
    bc  <- function(s) matrix(s, nrow = n_i, ncol = length(grid), byrow = TRUE)  # broadcast a curve to all rows

    surv_d <- as.matrix(d$surv_val_matrix)[, match(grid, et), drop = FALSE]
    Cd <- .ibsb_contrib_death(
      surv_val = surv_d,
      time_val = as.numeric(d$y_val$time), event_val = d$y_val$event,
      time_train = as.numeric(d$y_train$time), event_train = d$y_train$event,
      grid = grid, g_min = g_min, eps = eps)
    Cd_null <- if (compute_null) .ibsb_contrib_death(
      surv_val = bc(.ibsb_marg_km(as.numeric(d$y_train$time), d$y_train$event, grid)),
      time_val = as.numeric(d$y_val$time), event_val = d$y_val$event,
      time_train = as.numeric(d$y_train$time), event_train = d$y_train$event,
      grid = grid, g_min = g_min, eps = eps) else NULL

    surv_r <- as.matrix(r$surv_val_matrix)[, match(grid, et), drop = FALSE]
    if (identical(readmit_method, "ipcw")) {
      Cr <- .ibsb_contrib_readmit_ipcw(
        surv_val = surv_r,
        time_val = as.numeric(r$y_val$time), event_val = r$y_val$event,
        time_train = as.numeric(r$y_train$time), event_train = r$y_train$event,
        grid = grid, g_min = g_min, eps = eps)
      Cr_null <- if (compute_null) .ibsb_contrib_readmit_ipcw(
        surv_val = bc(.ibsb_marg_km(as.numeric(r$y_train$time), r$y_train$event, grid)),
        time_val = as.numeric(r$y_val$time), event_val = r$y_val$event,
        time_train = as.numeric(r$y_train$time), event_train = r$y_train$event,
        grid = grid, g_min = g_min, eps = eps) else NULL
    } else {
      Cr <- .ibsb_contrib_readmit_aj(
        surv_val = surv_r,
        rt_val = as.numeric(r$y_val$time), re_val = r$y_val$event,
        dt_val = as.numeric(d$y_val$time), de_val = d$y_val$event,
        rt_tr  = as.numeric(r$y_train$time), re_tr = r$y_train$event,
        dt_tr  = as.numeric(d$y_train$time), de_tr = d$y_train$event,
        grid = grid, g_min = g_min, eps = eps)
      cr_tr <- .ibsb_cr(as.numeric(r$y_train$time), r$y_train$event,
                        as.numeric(d$y_train$time), d$y_train$event)
      Cr_null <- if (compute_null) .ibsb_contrib_readmit_aj(
        surv_val = bc(.ibsb_marg_aj_surv(cr_tr$ftime, cr_tr$fstatus, grid)),
        rt_val = as.numeric(r$y_val$time), re_val = r$y_val$event,
        dt_val = as.numeric(d$y_val$time), de_val = d$y_val$event,
        rt_tr  = as.numeric(r$y_train$time), re_tr = r$y_train$event,
        dt_tr  = as.numeric(d$y_train$time), de_tr = d$y_train$event,
        grid = grid, g_min = g_min, eps = eps) else NULL
    }
    list(death   = list(C = Cd$C, Cnull = if (is.null(Cd_null)) NULL else Cd_null$C, grid = Cd$grid),
         readmit = list(C = Cr$C, Cnull = if (is.null(Cr_null)) NULL else Cr_null$C, grid = Cr$grid),
         n = n_i)
  })

  n_vec <- vapply(per_imp, `[[`, integer(1), "n")
  if (length(unique(n_vec)) != 1L)
    stop("Imputations have different validation row counts: ", paste(n_vec, collapse = ", "), call. = FALSE)
  n <- n_vec[1]

  # pooled (over imputations) window-IBS for a given C field ("C" model, "Cnull" null)
  dnm <- paste("death",   eval_times, sep = ".")
  rnm <- paste("readmit", eval_times, sep = ".")
  pooled_field <- function(field, row_weight) {
    Dt <- vapply(per_imp, function(p) .ibsb_windows_from_C(p$death[[field]],   p$death$grid,   eval_times, row_weight), numeric(length(eval_times)))
    Rt <- vapply(per_imp, function(p) .ibsb_windows_from_C(p$readmit[[field]], p$readmit$grid, eval_times, row_weight), numeric(length(eval_times)))
    out <- stats::setNames(c(rowMeans(Dt, na.rm = TRUE), rowMeans(Rt, na.rm = TRUE)), c(dnm, rnm))
    out[is.nan(out)] <- NA_real_      # grid-min row is all-NA across imputations -> NA, not NaN
    out
  }

  mk_df <- function(point, bt) {
    qrow <- function(p) suppressWarnings(apply(bt, 1, stats::quantile, p, na.rm = TRUE, names = FALSE))
    o <- data.frame(metric = names(point), point = point,
                    mean = rowMeans(bt, na.rm = TRUE),
                    q025 = qrow(0.025),
                    q975 = qrow(0.975),
                    row.names = NULL, stringsAsFactors = FALSE)
    o$mean[is.nan(o$mean)] <- NA_real_
    o$risk    <- ifelse(grepl("^death", o$metric), "death", "readmission")
    o$horizon <- as.numeric(sub(".*\\.", "", o$metric))
    o <- o[order(o$risk, o$horizon), c("risk", "horizon", "point", "mean", "q025", "q975")]
    rownames(o) <- NULL
    o
  }

  # ---- point estimates (full sample) ----
  point_m <- pooled_field("C", rep(1, n))
  point_n <- if (compute_null) pooled_field("Cnull", rep(1, n)) else NULL

  # ---- patient bootstrap: resample val rows; model and null share each resample ----
  set.seed(seed)
  bt_m <- matrix(NA_real_, nrow = length(point_m), ncol = B, dimnames = list(names(point_m), NULL))
  bt_n <- if (compute_null) bt_m else NULL
  for (b in seq_len(B)) {
    rw <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)   # multinomial row weights
    bt_m[, b] <- pooled_field("C", rw)
    if (compute_null) bt_n[, b] <- pooled_field("Cnull", rw)
    if (isTRUE(verbose) && (b %% 100 == 0)) message(sprintf("  bootstrap %d/%d", b, B))
  }

  out <- mk_df(point_m, bt_m)
  if (compute_null) attr(out, "null") <- mk_df(point_n, bt_n)
  attr(out, "config") <- list(B = B, seed = seed,
                              eval_times = eval_times, readmit_method = readmit_method,
                              lower_bound = min(eval_times), g_min = g_min)
  out
}

# Convenience wrapper for a full validation object: picks results_boot_val[[model]].
ibs_window_bootstrap_holdout <- function(validation, model = "best_perf1", ...) {
  stopifnot(model %in% names(validation$results_boot_val))
  out <- ibs_window_bootstrap_core(validation$results_boot_val[[model]], ...)
  attr(out, "config")$model <- model
  out
}
