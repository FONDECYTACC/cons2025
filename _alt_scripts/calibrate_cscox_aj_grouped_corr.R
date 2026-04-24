calibrate_cscox_aj_grouped_corr <- function(
    formula_readmit,
    formula_death = NULL,
    data,
    times = c(12, 36, 60),
    readmit_time_var = "readmit_time_from_disch_m",
    readmit_event_var = "readmit_event",
    death_time_var = "death_time_from_disch_m",
    death_event_var = "death_event",
    n_imputations = NULL,
    validation = c("apparent", "cv"),
    folds = 5L,
    n_repeats = 10L,
    g = 10L,
    min_bin_n = 20L,
    compute_ici = FALSE,
    ici_times = NULL,
    ici_df = 4L,
    seed = 2125L,
    tie_action = c("death_first", "readmit_first", "drop", "error"),
    verbose = TRUE,
    make_plots = TRUE,
    parallel_cv = TRUE,
    future_scheduling = 2,
    debug_fit = FALSE
) {
    if (!requireNamespace("survival", quietly = TRUE)) stop("Package 'survival' is required.")
    if (!requireNamespace("riskRegression", quietly = TRUE)) stop("Package 'riskRegression' is required.")
    if (!requireNamespace("prodlim", quietly = TRUE)) stop("Package 'prodlim' is required.")
    if (isTRUE(compute_ici) && !requireNamespace("splines", quietly = TRUE)) stop("Package 'splines' is required when compute_ici = TRUE.")
    if (!inherits(formula_readmit, "formula")) stop("'formula_readmit' must be a formula.")
    if (!is.null(formula_death) && !inherits(formula_death, "formula")) stop("'formula_death' must be NULL or a formula.")
    validation <- match.arg(validation)
    tie_action <- match.arg(tie_action)
    if (!is.numeric(times) || length(times) == 0L || any(!is.finite(times)) || any(times < 0)) {
        stop("'times' must be a non-empty numeric vector of non-negative finite values.")
    }
    times <- sort(unique(as.numeric(times)))
    folds <- as.integer(folds)
    n_repeats <- as.integer(n_repeats)
    g <- as.integer(g)
    min_bin_n <- as.integer(min_bin_n)
    ici_df <- as.integer(ici_df)
    if (is.null(ici_times)) {
        ici_times <- times
    } else {
        if (!is.numeric(ici_times) || any(!is.finite(ici_times))) stop("'ici_times' must be numeric.")
        ici_times <- sort(unique(as.numeric(ici_times)))
    }
    if (any(!ici_times %in% times)) stop("'ici_times' must be a subset of 'times'.")
    if (validation == "cv" && folds < 2L) stop("'folds' must be >= 2 when validation = 'cv'.")
    if (n_repeats < 1L) stop("'n_repeats' must be >= 1.")
    if (g < 1L) stop("'g' must be >= 1.")
    if (min_bin_n < 1L) stop("'min_bin_n' must be >= 1.")
    if (ici_df < 1L) stop("'ici_df' must be >= 1.")
    if (validation == "apparent" && n_repeats != 1L) {
        if (isTRUE(verbose)) message("validation = 'apparent': using a single run and ignoring n_repeats > 1.")
        n_repeats <- 1L
    }
    .msg <- function(...) {
        if (isTRUE(verbose)) message(...)
    }
    .as_df_list <- function(x, n_imputations = NULL) {
        if (is.data.frame(x)) {
            out <- list(as.data.frame(x))
        } else if (is.list(x) && length(x) > 0L && all(vapply(x, is.data.frame, logical(1)))) {
            out <- lapply(x, as.data.frame)
        } else {
            stop("'data' must be a data.frame or a list of data.frames.")
        }
        if (!is.null(n_imputations)) {
            out <- out[seq_len(min(as.integer(n_imputations), length(out)))]
        }
        out
    }
    .run_stats <- function(x, probs = c(0.025, 0.975), type = 8) {
        x <- x[is.finite(x)]
        n <- length(x)
        if (n == 0L) {
            return(c(mean = NA_real_, sd = NA_real_, min = NA_real_, p025 = NA_real_, p975 = NA_real_, max = NA_real_, n = 0L))
        }
        q <- stats::quantile(x, probs = c(0, probs, 1), na.rm = TRUE, names = FALSE, type = type)
        c(
            mean = mean(x),
            sd = if (n > 1L) stats::sd(x) else 0,
            min = q[1],
            p025 = q[2],
            p975 = q[3],
            max = q[4],
            n = n
        )
    }
    .coerce_event01 <- function(x, varname) {
        if (is.logical(x)) return(as.integer(x))
        if (is.factor(x)) x <- as.character(x)
        if (is.character(x)) {
            out <- rep(NA_integer_, length(x))
            ok <- !is.na(x)
            x0 <- trimws(tolower(x[ok]))
            out_ok <- ifelse(
                x0 %in% c("1", "true", "yes", "y"), 1L,
                ifelse(x0 %in% c("0", "false", "no", "n"), 0L, NA_integer_)
            )
            bad_nonmissing <- is.na(out_ok)
            if (any(bad_nonmissing)) {
                bad_vals <- unique(x[ok][bad_nonmissing])
                stop(
                    "Variable '", varname,
                    "' contains non-missing values that cannot be coerced to 0/1: ",
                    paste(utils::head(bad_vals, 10), collapse = ", ")
                )
            }
            out[ok] <- out_ok
            return(out)
        }
        if (is.numeric(x) || is.integer(x)) {
            ux <- unique(stats::na.omit(x))
            if (!all(ux %in% c(0, 1))) {
                stop("Variable '", varname, "' must contain only 0/1 values (besides NA).")
            }
            return(as.integer(x))
        }
        stop("Variable '", varname, "' must be logical, numeric/integer 0/1, factor, or character.")
    }
    .rhs_formula <- function(formula_obj) {
        f_rhs <- stats::formula(stats::delete.response(stats::terms(formula_obj)))
        environment(f_rhs) <- environment(formula_obj)
        f_rhs
    }
    .build_hist_formula <- function(rhs_formula) {
        f_hist <- stats::update.formula(rhs_formula, prodlim::Hist(.ftime, .fstatus) ~ .)
        environment(f_hist) <- environment(rhs_formula)
        f_hist
    }
    .make_first_event_data <- function(d) {
        req <- c(readmit_time_var, readmit_event_var, death_time_var, death_event_var)
        miss <- setdiff(req, names(d))
        if (length(miss) > 0L) stop("Missing required columns: ", paste(miss, collapse = ", "))
        tr <- d[[readmit_time_var]]
        td <- d[[death_time_var]]
        er <- .coerce_event01(d[[readmit_event_var]], readmit_event_var)
        ed <- .coerce_event01(d[[death_event_var]], death_event_var)
        if (!is.numeric(tr) || !is.numeric(td)) stop("Time variables must be numeric.")
        if (any(tr < 0, na.rm = TRUE) || any(td < 0, na.rm = TRUE)) stop("Negative follow-up times are not allowed.")
        d$.tr_raw <- tr
        d$.td_raw <- td
        d$.er_raw <- er
        d$.ed_raw <- ed
        .ftime <- pmin(tr, td)
        r_at_first <- (er == 1L) & !is.na(er) & !is.na(.ftime) & (abs(tr - .ftime) <= 1e-08)
        d_at_first <- (ed == 1L) & !is.na(ed) & !is.na(.ftime) & (abs(td - .ftime) <= 1e-08)
        .fstatus <- rep(0L, length(.ftime))
        .fstatus[r_at_first & !d_at_first] <- 1L
        .fstatus[d_at_first & !r_at_first] <- 2L
        both_at_first <- r_at_first & d_at_first
        n_ties <- sum(both_at_first, na.rm = TRUE)
        if (n_ties > 0L) {
            if (tie_action == "error") {
                stop("Found ", n_ties, " simultaneous readmission/death first events.")
            } else if (tie_action == "death_first") {
                .msg("Found ", n_ties, " ties; assigning them to death-first.")
                .fstatus[both_at_first] <- 2L
            } else if (tie_action == "readmit_first") {
                .msg("Found ", n_ties, " ties; assigning them to readmission-first.")
                .fstatus[both_at_first] <- 1L
            } else if (tie_action == "drop") {
                .msg("Found ", n_ties, " ties; they will be dropped by unified filtering.")
                .fstatus[both_at_first] <- NA_integer_
            }
        }
        d$.ftime <- .ftime
        d$.fstatus <- .fstatus
        d
    }
    .analysis_mask <- function(d0, rhs_readmit, rhs_death) {
        mf_readmit <- stats::model.frame(rhs_readmit, data = d0, na.action = stats::na.pass)
        mf_death <- stats::model.frame(rhs_death, data = d0, na.action = stats::na.pass)
        stats::complete.cases(d0[, c(".ftime", ".fstatus"), drop = FALSE]) &
            stats::complete.cases(mf_readmit) &
            stats::complete.cases(mf_death)
    }
    .validate_analysis_data <- function(dcc, validation, folds) {
        n <- nrow(dcc)
        n1 <- sum(dcc$.fstatus == 1L, na.rm = TRUE)
        n2 <- sum(dcc$.fstatus == 2L, na.rm = TRUE)
        if (n == 0L) stop("No complete cases remain after unified filtering.")
        if (n1 < 1L) stop("No readmission-first events remain in the analysis cohort.")
        if (n2 < 1L) stop("No death-first events remain in the analysis cohort.")
        if (validation == "cv") {
            if (folds > n) stop("'folds' cannot exceed the analysis sample size.")
            if (n1 < 2L) stop("Cross-validation is not feasible: fewer than 2 readmission-first events remain.")
            if (n2 < 2L) stop("Cross-validation is not feasible: fewer than 2 death-first events remain.")
        }
        invisible(TRUE)
    }
    .make_stratified_folds <- function(status, folds, seed_local) {
        set.seed(seed_local)
        n <- length(status)
        if (folds > n) stop("'folds' cannot exceed sample size.")
        fold_id <- integer(n)
        classes <- sort(unique(status))
        offset <- 0L
        for (cl in classes) {
            idx <- which(status == cl)
            idx <- sample(idx, length(idx), replace = FALSE)
            assign <- ((seq_along(idx) - 1L + offset) %% folds) + 1L
            fold_id[idx] <- assign
            offset <- (offset + length(idx)) %% folds
        }
        tab <- tabulate(fold_id, nbins = folds)
        while (any(tab == 0L)) {
            empty_fold <- which(tab == 0L)[1L]
            donor_fold <- which.max(tab)
            donor_idx <- which(fold_id == donor_fold)
            move_idx <- sample(donor_idx, 1L)
            fold_id[move_idx] <- empty_fold
            tab <- tabulate(fold_id, nbins = folds)
        }
        for (k in seq_len(folds)) {
            tr_status <- status[fold_id != k]
            if (sum(tr_status == 1L, na.rm = TRUE) == 0L) stop("Invalid fold split: training fold ", k, " has zero readmission-first events.")
            if (sum(tr_status == 2L, na.rm = TRUE) == 0L) stop("Invalid fold split: training fold ", k, " has zero death-first events.")
            if (sum(fold_id == k) == 0L) stop("Invalid fold split: test fold ", k, " is empty.")
        }
        fold_id
    }
    .fit_csc_and_predict <- function(train_data, test_data, f1, f2, times) {
        fit <- tryCatch(
            riskRegression::CSC(list(f1, f2), data = train_data),
            error = function(e) stop("riskRegression::CSC failed.\nOriginal error: ", e$message)
        )
        pred <- riskRegression::predictRisk(object = fit, newdata = test_data, times = times, cause = 1)
        if (is.null(dim(pred))) {
            pred <- matrix(pred, nrow = nrow(test_data), byrow = TRUE)
        } else {
            pred <- as.matrix(pred)
        }
        if (nrow(pred) != nrow(test_data) || ncol(pred) != length(times)) {
            stop(
                "Predicted matrix has wrong dimensions: expected ",
                nrow(test_data), "x", length(times),
                ", got ", nrow(pred), "x", ncol(pred), "."
            )
        }
        if (any(!is.finite(pred))) stop("Non-finite predicted risks were produced. Calibration would be invalid.")
        pred <- pmin(pmax(pred, 0), 1)
        colnames(pred) <- paste0("t_", times)
        pred
    }
    .run_one_fold_csc <- function(task, analysis_data, times, M, f1, f2) {
        tr_idx <- task$tr_idx
        te_idx <- task$te_idx
        pred_sum <- matrix(0, nrow = length(te_idx), ncol = length(times))
        for (m in seq_len(M)) {
            pred_m <- tryCatch(
                .fit_csc_and_predict(
                    train_data = analysis_data[[m]][tr_idx, , drop = FALSE],
                    test_data = analysis_data[[m]][te_idx, , drop = FALSE],
                    f1 = f1,
                    f2 = f2,
                    times = times
                ),
                error = function(e) {
                    stop(
                        "Parallel task failed at repetition=", task$r,
                        ", fold=", task$k,
                        ", imputation=", m,
                        ": ", conditionMessage(e),
                        call. = FALSE
                    )
                }
            )
            pred_sum <- pred_sum + pred_m
        }
        list(r = task$r, te_idx = te_idx, pred_fold_avg = pred_sum / M)
    }
    .make_cutpoints <- function(x, g = 10L) {
        x <- x[is.finite(x)]
        if (length(x) == 0L) return(numeric(0))
        if (length(unique(x)) == 1L) return(numeric(0))
        probs <- seq(0, 1, length.out = g + 1L)
        q <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 8)
        unique(q[-c(1, length(q))])
    }
    .assign_bins <- function(x, cutpoints) {
        bins <- rep(NA_integer_, length(x))
        ok <- is.finite(x)
        if (!any(ok)) return(bins)
        if (length(cutpoints) == 0L) {
            bins[ok] <- 1L
        } else {
            bins[ok] <- findInterval(x[ok], vec = cutpoints, all.inside = TRUE) + 1L
        }
        bins
    }
    .build_global_bin_map <- function(x, cutpoints, min_bin_n) {
        bins0 <- .assign_bins(x, cutpoints)
        levs <- sort(unique(stats::na.omit(bins0)))
        if (length(levs) == 0L) return(integer(0))
        groups <- lapply(levs, function(z) z)
        counts_full <- tabulate(bins0[!is.na(bins0)], nbins = max(levs))
        counts <- as.numeric(counts_full[levs])
        repeat {
            n_groups <- length(groups)
            if (n_groups == 1L) break
            small <- which(counts < min_bin_n)
            if (length(small) == 0L) break
            g_idx <- small[1L]
            if (g_idx == 1L) {
                target <- 2L
            } else if (g_idx == n_groups) {
                target <- n_groups - 1L
            } else {
                target <- if (counts[g_idx - 1L] <= counts[g_idx + 1L]) g_idx - 1L else g_idx + 1L
            }
            groups[[target]] <- sort(c(groups[[target]], groups[[g_idx]]))
            counts[target] <- counts[target] + counts[g_idx]
            keep <- setdiff(seq_len(n_groups), g_idx)
            groups <- groups[keep]
            counts <- counts[keep]
        }
        n_orig_bins <- if (length(cutpoints) == 0L) 1L else length(cutpoints) + 1L
        bin_map <- rep(NA_integer_, n_orig_bins)
        if (length(groups) == 1L && counts[1L] < min_bin_n) return(bin_map)
        for (i in seq_along(groups)) {
            bin_map[unlist(groups[[i]], use.names = FALSE)] <- i
        }
        bin_map
    }
    .apply_bin_map <- function(bins, bin_map) {
        out <- rep(NA_integer_, length(bins))
        ok <- !is.na(bins) & bins >= 1L & bins <= length(bin_map)
        if (any(ok)) out[ok] <- bin_map[bins[ok]]
        out
    }
    .state_position <- function(pstate_obj, states, target) {
        cn <- if (is.null(dim(pstate_obj))) names(pstate_obj) else colnames(pstate_obj)
        if (!is.null(cn)) {
            idx <- match(target, cn)
            if (!is.na(idx)) return(idx)
        }
        idx <- match(target, states)
        if (is.na(idx)) stop("Could not locate state '", target, "' in survfit output.")
        ncols <- if (is.null(dim(pstate_obj))) length(pstate_obj) else ncol(pstate_obj)
        if (ncols == length(states) + 1L) return(idx + 1L)
        if (ncols == length(states)) return(idx)

        stop("Unexpected pstate structure: ", ncols, " columns for ", length(states), " states.")
    }
    .aj_cif_readmit <- function(d, t_horizon) {
        d$.status_factor <- factor(d$.fstatus, levels = 0:2, labels = c("censor", "readmit", "death"))
        fit <- survival::survfit(survival::Surv(.ftime, .status_factor) ~ 1, data = d, conf.int = 0.95)
        s <- summary(fit, times = t_horizon, extend = TRUE)
        if (length(s$pstate) == 0L) return(list(risk = 0, lower = 0, upper = 0))
        state_pos <- .state_position(s$pstate, fit$states, "readmit")
        if (is.null(dim(s$pstate))) {
            risk <- as.numeric(s$pstate[state_pos])
            lower <- if (!is.null(s$lower) && length(s$lower) >= state_pos) as.numeric(s$lower[state_pos]) else NA_real_
            upper <- if (!is.null(s$upper) && length(s$upper) >= state_pos) as.numeric(s$upper[state_pos]) else NA_real_
        } else {
            risk <- as.numeric(s$pstate[1, state_pos])
            lower <- if (!is.null(s$lower) && ncol(s$lower) >= state_pos) as.numeric(s$lower[1, state_pos]) else NA_real_
            upper <- if (!is.null(s$upper) && ncol(s$upper) >= state_pos) as.numeric(s$upper[1, state_pos]) else NA_real_
        }
        if (is.na(lower)) lower <- risk
        if (is.na(upper)) upper <- risk
        list(
            risk = pmin(pmax(risk, 0), 1),
            lower = pmin(pmax(lower, 0), 1),
            upper = pmin(pmax(upper, 0), 1)
        )
    }
    .grouped_calibration <- function(d, pred_vec, t_horizon, cutpoints, bin_map, min_bin_n) {
        bins0 <- .assign_bins(pred_vec, cutpoints)
        bins <- .apply_bin_map(bins0, bin_map)
        levs <- sort(unique(stats::na.omit(bins)))
        if (length(levs) == 0L) {
            return(list(curve = NULL, n_used = 0L, n_excluded = sum(is.finite(pred_vec))))
        }
        out <- vector("list", length(levs))
        keep_idx <- 0L
        for (b in levs) {
            idx <- which(bins == b)
            if (length(idx) < min_bin_n) next
            obs <- .aj_cif_readmit(d[idx, , drop = FALSE], t_horizon)
            keep_idx <- keep_idx + 1L
            out[[keep_idx]] <- data.frame(
                bin = b,
                mean_predicted = mean(pred_vec[idx], na.rm = TRUE),
                observed = obs$risk,
                n_patients = length(idx),
                stringsAsFactors = FALSE
            )
        }
        if (keep_idx == 0L) {
            return(list(curve = NULL, n_used = 0L, n_excluded = sum(is.finite(pred_vec))))
        }
        curve <- do.call(rbind, out[seq_len(keep_idx)])
        list(
            curve = curve,
            n_used = sum(curve$n_patients),
            n_excluded = sum(is.finite(pred_vec)) - sum(curve$n_patients)
        )
    }
    .smooth_calibration_metrics <- function(d, pred_vec, t_horizon, ici_df = 4L, eps = 1e-06) {
        pred_use <- pmin(pmax(pred_vec, eps), 1 - eps)
        z <- log(-log(1 - pred_use))
        if (any(!is.finite(z))) {
            return(list(
                ici = NA_real_,
                e50 = NA_real_,
                e90 = NA_real_,
                emax = NA_real_,
                smoothed_observed = rep(NA_real_, length(pred_vec))
            ))
        }
        fit_once <- function(df_use) {
            d_fit <- d
            d_fit$.cll_pred <- z
            if (df_use <= 1L || length(unique(z)) <= 3L) {
                f_cal <- prodlim::Hist(.ftime, .fstatus) ~ .cll_pred
            } else {
                df_eff <- min(as.integer(df_use), length(unique(z)) - 1L)
                basis <- as.data.frame(splines::ns(z, df = df_eff))
                names(basis) <- paste0("ici_s", seq_len(ncol(basis)))
                d_fit[names(basis)] <- basis
                rhs <- paste(names(basis), collapse = " + ")
                f_cal <- stats::as.formula(paste0("prodlim::Hist(.ftime, .fstatus) ~ ", rhs))
            }
            list(
                fit = riskRegression::FGR(formula = f_cal, data = d_fit, cause = 1),
                newdata = d_fit
            )
        }
        fit_obj <- tryCatch(
            fit_once(ici_df),
            error = function(e1) {
                tryCatch(
                    fit_once(1L),
                    error = function(e2) {
                        warning("ICI smoother failed at time ", t_horizon, ": ", conditionMessage(e1), call. = FALSE)
                        NULL
                    }
                )
            }
        )
        if (is.null(fit_obj)) {
            return(list(
                ici = NA_real_,
                e50 = NA_real_,
                e90 = NA_real_,
                emax = NA_real_,
                smoothed_observed = rep(NA_real_, length(pred_vec))
            ))
        }
        obs_smooth <- riskRegression::predictRisk(
            object = fit_obj$fit,
            newdata = fit_obj$newdata,
            times = t_horizon,
            cause = 1
        )
        if (!is.null(dim(obs_smooth))) {
            obs_smooth <- as.numeric(obs_smooth[, 1L])
        } else {
            obs_smooth <- as.numeric(obs_smooth)
        }
        if (length(obs_smooth) != length(pred_vec) || any(!is.finite(obs_smooth))) {
            warning("ICI prediction failed at time ", t_horizon, ".", call. = FALSE)
            return(list(
                ici = NA_real_,
                e50 = NA_real_,
                e90 = NA_real_,
                emax = NA_real_,
                smoothed_observed = rep(NA_real_, length(pred_vec))
            ))
        }
        abs_err <- abs(obs_smooth - pred_vec)
        list(
            ici = mean(abs_err, na.rm = TRUE),
            e50 = as.numeric(stats::quantile(abs_err, probs = 0.50, na.rm = TRUE, names = FALSE, type = 8)),
            e90 = as.numeric(stats::quantile(abs_err, probs = 0.90, na.rm = TRUE, names = FALSE, type = 8)),
            emax = max(abs_err, na.rm = TRUE),
            smoothed_observed = obs_smooth
        )
    }
    .pool_summary <- function(by_repetition) {
        if (nrow(by_repetition) == 0L) return(by_repetition)
        time_levels <- sort(unique(by_repetition$time_months))
        out <- lapply(time_levels, function(tt) {
            z <- by_repetition[by_repetition$time_months == tt, , drop = FALSE]
            ici_st <- .run_stats(z$ici)
            e50_st <- .run_stats(z$e50)
            e90_st <- .run_stats(z$e90)
            emax_st <- .run_stats(z$emax)
            ece_st <- .run_stats(z$ece)
            eo_st <- .run_stats(z$eo_ratio)
            mp_st <- .run_stats(z$mean_predicted)
            obs_st <- .run_stats(z$observed)
            data.frame(
                time_months = tt,
                ici_mean = unname(ici_st["mean"]),
                ici_sd = unname(ici_st["sd"]),
                ici_min = unname(ici_st["min"]),
                ici_p025 = unname(ici_st["p025"]),
                ici_p975 = unname(ici_st["p975"]),
                ici_max = unname(ici_st["max"]),
                e50_mean = unname(e50_st["mean"]),
                e50_sd = unname(e50_st["sd"]),
                e50_min = unname(e50_st["min"]),
                e50_p025 = unname(e50_st["p025"]),
                e50_p975 = unname(e50_st["p975"]),
                e50_max = unname(e50_st["max"]),
                e90_mean = unname(e90_st["mean"]),
                e90_sd = unname(e90_st["sd"]),
                e90_min = unname(e90_st["min"]),
                e90_p025 = unname(e90_st["p025"]),
                e90_p975 = unname(e90_st["p975"]),
                e90_max = unname(e90_st["max"]),
                emax_mean = unname(emax_st["mean"]),
                emax_sd = unname(emax_st["sd"]),
                emax_min = unname(emax_st["min"]),
                emax_p025 = unname(emax_st["p025"]),
                emax_p975 = unname(emax_st["p975"]),
                emax_max = unname(emax_st["max"]),
                ece_mean = unname(ece_st["mean"]),
                ece_sd = unname(ece_st["sd"]),
                ece_min = unname(ece_st["min"]),
                ece_p025 = unname(ece_st["p025"]),
                ece_p975 = unname(ece_st["p975"]),
                ece_max = unname(ece_st["max"]),
                eo_mean = unname(eo_st["mean"]),
                eo_sd = unname(eo_st["sd"]),
                eo_min = unname(eo_st["min"]),
                eo_p025 = unname(eo_st["p025"]),
                eo_p975 = unname(eo_st["p975"]),
                eo_max = unname(eo_st["max"]),
                mean_pred = unname(mp_st["mean"]),
                mean_pred_sd = unname(mp_st["sd"]),
                mean_pred_min = unname(mp_st["min"]),
                mean_pred_p025 = unname(mp_st["p025"]),
                mean_pred_p975 = unname(mp_st["p975"]),
                mean_pred_max = unname(mp_st["max"]),
                observed = unname(obs_st["mean"]),
                observed_sd = unname(obs_st["sd"]),
                observed_min = unname(obs_st["min"]),
                observed_p025 = unname(obs_st["p025"]),
                observed_p975 = unname(obs_st["p975"]),
                observed_max = unname(obs_st["max"]),
                n_runs = nrow(z),
                n_imp = M,
                stringsAsFactors = FALSE
            )
        })
        out <- do.call(rbind, out)
        rownames(out) <- NULL
        out[order(out$time_months), , drop = FALSE]
    }
    .pool_curves <- function(curves_df) {
        if (nrow(curves_df) == 0L) return(curves_df)
        key <- paste(curves_df$time_months, curves_df$bin, sep = "___")
        spl <- split(curves_df, key)
        out <- lapply(spl, function(z) {
            mp_st <- .run_stats(z$mean_predicted)
            obs_st <- .run_stats(z$observed)
            data.frame(
                time_months = unique(z$time_months),
                bin = unique(z$bin),
                mean_predicted = unname(mp_st["mean"]),
                mean_predicted_sd = unname(mp_st["sd"]),
                mean_predicted_min = unname(mp_st["min"]),
                mean_predicted_p025 = unname(mp_st["p025"]),
                mean_predicted_p975 = unname(mp_st["p975"]),
                mean_predicted_max = unname(mp_st["max"]),
                observed = unname(obs_st["mean"]),
                observed_sd = unname(obs_st["sd"]),
                observed_min = unname(obs_st["min"]),
                observed_p025 = unname(obs_st["p025"]),
                observed_p975 = unname(obs_st["p975"]),
                observed_max = unname(obs_st["max"]),
                n_total = sum(z$n_patients, na.rm = TRUE),
                n_runs = nrow(z),
                n_imp = M,
                stringsAsFactors = FALSE
            )
        })
        out <- do.call(rbind, out)
        rownames(out) <- NULL
        out[order(out$time_months, out$bin), , drop = FALSE]
    }
    data_list <- .as_df_list(data, n_imputations = n_imputations)
    M <- length(data_list)
    rhs_readmit <- .rhs_formula(formula_readmit)
    rhs_death <- if (is.null(formula_death)) .rhs_formula(formula_readmit) else .rhs_formula(formula_death)
    f_readmit <- .build_hist_formula(rhs_readmit)
    f_death <- .build_hist_formula(rhs_death)
    effective_repeats <- if (validation == "cv") n_repeats else 1L
    .msg("Using ", M, " imputed dataset(s).")
    .msg("Validation: ", validation)
    if (validation == "cv") .msg("Repetitions: ", effective_repeats, " | same fold split across imputations within each repetition.")
    if (isTRUE(compute_ici)) {
        .msg("ICI: enabled at horizons ", paste(ici_times, collapse = ", "), ".")
    } else {
        .msg("ICI: disabled.")
    }
    d0_list <- lapply(data_list, .make_first_event_data)
    if (M > 1L) {
        ref0 <- d0_list[[1L]]
        for (m in 2:M) {
            if (nrow(d0_list[[m]]) != nrow(ref0)) stop("All imputations must have the same number of rows and row order.")
            same_ftime <- isTRUE(all.equal(ref0$.ftime, d0_list[[m]]$.ftime, tolerance = 1e-08, check.attributes = FALSE))
            same_fstatus <- identical(ref0$.fstatus, d0_list[[m]]$.fstatus)
            if (!same_ftime || !same_fstatus) {
                stop("Outcome/time information differs across imputations. This workflow assumes only predictors were imputed.")
            }
        }
    }
    mask_list <- lapply(d0_list, function(d0) .analysis_mask(d0, rhs_readmit, rhs_death))
    keep_all <- Reduce(`&`, mask_list)
    n_input <- nrow(d0_list[[1L]])
    n_analysis <- sum(keep_all)
    n_dropped <- n_input - n_analysis
    if (n_dropped > 0L) {
        .msg("Dropping ", n_dropped, " rows due to missing outcomes/times/model covariates in at least one imputation.")
    }
    analysis_data <- lapply(d0_list, function(d0) {
        out <- d0[keep_all, , drop = FALSE]
        rownames(out) <- NULL
        out
    })
    reference_data <- analysis_data[[1L]]
    .validate_analysis_data(reference_data, validation, folds)
    n_readmit <- sum(reference_data$.fstatus == 1L, na.rm = TRUE)
    n_death <- sum(reference_data$.fstatus == 2L, na.rm = TRUE)
    reference_observed <- do.call(rbind, lapply(times, function(tt) {
        obs <- .aj_cif_readmit(reference_data, tt)
        data.frame(
            time_months = tt,
            observed = obs$risk,
            observed_lower = obs$lower,
            observed_upper = obs$upper,
            stringsAsFactors = FALSE
        )
    }))
    pred_rep_list <- list()
    if (validation == "apparent") {
        .msg("Fitting apparent predictions averaged across imputations...", appendLF = FALSE)
        pred_sum <- matrix(0, nrow = n_analysis, ncol = length(times))
        for (m in seq_len(M)) {
            pred_sum <- pred_sum + .fit_csc_and_predict(
                train_data = analysis_data[[m]],
                test_data = analysis_data[[m]],
                f1 = f_readmit,
                f2 = f_death,
                times = times
            )
        }
        pred_avg <- pred_sum / M
        colnames(pred_avg) <- paste0("t_", times)
        pred_rep_list[[1L]] <- list(repetition = 1L, pred = pred_avg)
        .msg(" done")
    } else {
        task_list <- unlist(
            lapply(seq_len(effective_repeats), function(r) {
                fold_id <- .make_stratified_folds(reference_data$.fstatus, folds = folds, seed_local = seed + r)
                lapply(seq_len(folds), function(k) {
                    list(r = r, k = k, tr_idx = which(fold_id != k), te_idx = which(fold_id == k))
                })
            }),
            recursive = FALSE
        )

        if (parallel_cv) {
            if (!requireNamespace("future.apply", quietly = TRUE)) {
                stop("Package 'future.apply' is required when parallel_cv = TRUE.")
            }
            .msg(
                "Running CV predictions in parallel across ",
                length(task_list),
                " repetition x fold tasks...",
                appendLF = FALSE
            )
            task_results <- future.apply::future_lapply(
                X = task_list,
                FUN = .run_one_fold_csc,
                analysis_data = analysis_data,
                times = times,
                M = M,
                f1 = f_readmit,
                f2 = f_death,
                future.seed = TRUE,
                future.globals = FALSE,
                future.packages = c("survival", "riskRegression", "prodlim", "splines"),
                future.scheduling = future_scheduling
            )
            .msg(" done")
        } else {
            .msg(
                "Running CV predictions sequentially across ",
                length(task_list),
                " repetition x fold tasks...",
                appendLF = FALSE
            )
            task_results <- lapply(
                X = task_list,
                FUN = .run_one_fold_csc,
                analysis_data = analysis_data,
                times = times,
                M = M,
                f1 = f_readmit,
                f2 = f_death
            )
            .msg(" done")
        }

        for (r in seq_len(effective_repeats)) {
            pred_avg <- matrix(NA_real_, nrow = n_analysis, ncol = length(times))
            r_hits <- task_results[vapply(task_results, function(x) x$r, integer(1L)) == r]
            for (res in r_hits) {
                pred_avg[res$te_idx, ] <- res$pred_fold_avg
            }
            if (any(!is.finite(pred_avg))) stop("Non-finite predictions remained after averaging across imputations in repetition ", r, ".")
            colnames(pred_avg) <- paste0("t_", times)
            pred_rep_list[[length(pred_rep_list) + 1L]] <- list(repetition = r, pred = pred_avg)
        }
    }
    common_cutpoints <- vector("list", length(times))
    common_bin_maps <- vector("list", length(times))
    names(common_cutpoints) <- paste0("t_", times)
    names(common_bin_maps) <- paste0("t_", times)
    for (j in seq_along(times)) {
        pooled_preds_j <- unlist(lapply(pred_rep_list, function(obj) obj$pred[, j]), use.names = FALSE)
        cuts_j <- .make_cutpoints(pooled_preds_j, g = g)
        common_cutpoints[[j]] <- cuts_j
        common_bin_maps[[j]] <- .build_global_bin_map(
            x = pooled_preds_j,
            cutpoints = cuts_j,
            min_bin_n = min_bin_n
        )
    }
    curves_all <- list()
    metrics_all <- list()
    for (rep_idx in seq_along(pred_rep_list)) {
        rep_obj <- pred_rep_list[[rep_idx]]
        r <- rep_obj$repetition
        pred_r <- rep_obj$pred
        .msg("Calibrating repetition ", r, "/", length(pred_rep_list), "...", appendLF = FALSE)
        for (j in seq_along(times)) {
            t_horizon <- times[j]
            pred_vec <- pred_r[, j]
            calib_obj <- .grouped_calibration(
                d = reference_data,
                pred_vec = pred_vec,
                t_horizon = t_horizon,
                cutpoints = common_cutpoints[[j]],
                bin_map = common_bin_maps[[j]],
                min_bin_n = min_bin_n
            )
            calib_df <- calib_obj$curve
            if (is.null(calib_df) || nrow(calib_df) == 0L) {
                warning("No calibration bins survived for repetition ", r, " at time ", t_horizon, ".", call. = FALSE)
                next
            }
            obs_row <- reference_observed[reference_observed$time_months == t_horizon, , drop = FALSE]
            ece <- stats::weighted.mean(abs(calib_df$mean_predicted - calib_df$observed), w = calib_df$n_patients, na.rm = TRUE)
            if (isTRUE(compute_ici) && t_horizon %in% ici_times) {
                ici_obj <- .smooth_calibration_metrics(
                    d = reference_data,
                    pred_vec = pred_vec,
                    t_horizon = t_horizon,
                    ici_df = ici_df
                )
            } else {
                ici_obj <- list(
                    ici = NA_real_,
                    e50 = NA_real_,
                    e90 = NA_real_,
                    emax = NA_real_
                )
            }
            mean_pred <- mean(pred_vec, na.rm = TRUE)
            eo_ratio <- if (is.na(obs_row$observed[1]) || obs_row$observed[1] <= 0) NA_real_ else mean_pred / obs_row$observed[1]
            calib_df$repetition <- r
            calib_df$time_months <- t_horizon
            curves_all[[length(curves_all) + 1L]] <- calib_df
            metrics_all[[length(metrics_all) + 1L]] <- data.frame(
                repetition = r,
                time_months = t_horizon,
                ici = ici_obj$ici,
                e50 = ici_obj$e50,
                e90 = ici_obj$e90,
                emax = ici_obj$emax,
                ece = ece,
                eo_ratio = eo_ratio,
                mean_predicted = mean_pred,
                observed = obs_row$observed[1],
                observed_lower = obs_row$observed_lower[1],
                observed_upper = obs_row$observed_upper[1],
                n_input = n_input,
                n_analysis = n_analysis,
                n_dropped = n_dropped,
                n_readmit = n_readmit,
                n_death = n_death,
                n_used_calibration = calib_obj$n_used,
                n_excluded_calibration = calib_obj$n_excluded,
                n_bins = nrow(calib_df),
                n_imputations = M,
                stringsAsFactors = FALSE
            )
        }
        .msg(" done")
    }
    by_repetition <- if (length(metrics_all) > 0L) do.call(rbind, metrics_all) else data.frame()
    curves_df <- if (length(curves_all) > 0L) do.call(rbind, curves_all) else data.frame()
    if (isTRUE(compute_ici) && nrow(by_repetition) > 0L) {
        ici_rows <- by_repetition$time_months %in% ici_times
        if (any(ici_rows) && all(is.na(by_repetition$ici[ici_rows]))) {
            warning("ICI/E50/E90/Emax could not be computed for the requested ICI horizons.", call. = FALSE)
        }
    }
    pooled_summary <- if (nrow(by_repetition) > 0L) .pool_summary(by_repetition) else data.frame()
    pooled_curves <- if (nrow(curves_df) > 0L) .pool_curves(curves_df) else data.frame()
    plot_list <- list()
    combined_plot <- NULL
    if (isTRUE(make_plots) && nrow(pooled_curves) > 0L && requireNamespace("ggplot2", quietly = TRUE)) {
        for (tt in times) {
            pdat <- pooled_curves[pooled_curves$time_months == tt, , drop = FALSE]
            rdat <- curves_df[curves_df$time_months == tt, , drop = FALSE]
            if (nrow(pdat) == 0L) next
            lim_max <- max(c(rdat$mean_predicted, rdat$observed, pdat$mean_predicted_p975, pdat$observed_p975), na.rm = TRUE)
            lim_max <- max(0.05, lim_max * 1.05)
            srow <- pooled_summary[pooled_summary$time_months == tt, , drop = FALSE]
            subtxt <- if (nrow(srow) == 1L) {
                paste0(
                    "Validation: ", validation,
                    if (validation == "cv") paste0(" | repetitions = ", effective_repeats) else "",
                    if (M > 1L) paste0(" | imputations averaged = ", M) else "",
                    if (isTRUE(compute_ici) && tt %in% ici_times) {
                        paste0(
                            " | ICI mean = ", formatC(srow$ici_mean, digits = 4, format = "f"),
                            " | SD = ", formatC(srow$ici_sd, digits = 4, format = "f")
                        )
                    } else {
                        " | ICI not computed"
                    },
                    " | E/O mean = ", formatC(srow$eo_mean, digits = 4, format = "f"),
                    " | SD = ", formatC(srow$eo_sd, digits = 4, format = "f")
                )
            } else {
                paste0("Validation: ", validation)
            }
            p <- ggplot2::ggplot() +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                ggplot2::geom_line(
                    data = rdat,
                    ggplot2::aes(x = mean_predicted, y = observed, group = repetition),
                    color = "gray70",
                    alpha = 0.55,
                    linewidth = 0.4
                ) +
                ggplot2::geom_ribbon(
                    data = pdat,
                    ggplot2::aes(x = mean_predicted, ymin = observed_p025, ymax = observed_p975, group = 1),
                    fill = "gray70",
                    alpha = 0.25
                ) +
                ggplot2::geom_line(
                    data = pdat,
                    ggplot2::aes(x = mean_predicted, y = observed, group = 1),
                    color = "steelblue",
                    linewidth = 0.9
                ) +
                ggplot2::geom_point(
                    data = pdat,
                    ggplot2::aes(x = mean_predicted, y = observed),
                    size = 2.5,
                    shape = 21,
                    fill = "white",
                    color = "steelblue"
                ) +
                ggplot2::coord_equal(xlim = c(0, lim_max), ylim = c(0, lim_max)) +
                ggplot2::labs(
                    title = paste0(tt, " months"),
                    subtitle = subtxt,
                    x = "Predicted CIF of readmission",
                    y = "Observed AJ CIF of readmission"
                ) +
                ggplot2::theme_minimal()

            plot_list[[paste0("month_", tt)]] <- p
        }
        combined_plot <- ggplot2::ggplot() +
            ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
            ggplot2::geom_line(
                data = curves_df,
                ggplot2::aes(x = mean_predicted, y = observed, group = interaction(time_months, repetition)),
                color = "gray70",
                alpha = 0.5,
                linewidth = 0.35
            ) +
            ggplot2::geom_ribbon(
                data = pooled_curves,
                ggplot2::aes(x = mean_predicted, ymin = observed_p025, ymax = observed_p975, group = time_months),
                fill = "gray70",
                alpha = 0.25
            ) +
            ggplot2::geom_line(
                data = pooled_curves,
                ggplot2::aes(x = mean_predicted, y = observed, group = time_months),
                color = "steelblue",
                linewidth = 0.85
            ) +
            ggplot2::geom_point(
                data = pooled_curves,
                ggplot2::aes(x = mean_predicted, y = observed),
                size = 1.8,
                color = "steelblue"
            ) +
            ggplot2::facet_wrap(~time_months, scales = "free") +
            ggplot2::labs(
                title = "Grouped calibration of cause-specific Cox predictions",
                subtitle = paste0(
                    "Gray lines = repetitions | Gray ribbon = pointwise 2.5th-97.5th percentile across repetitions | Blue = mean",
                    if (validation == "cv") paste0(" | repeated ", folds, "-fold CV x ", effective_repeats) else ""
                ),
                x = "Predicted CIF of readmission",
                y = "Observed AJ CIF of readmission"
            ) +
            ggplot2::theme_minimal()
    }

    structure(
        list(
            by_repetition = by_repetition,
            by_run = by_repetition,
            pooled_summary = pooled_summary,
            calibration_curves = pooled_curves,
            raw_curves = curves_df,
            reference_observed = reference_observed,
            plots = plot_list,
            combined_plot = combined_plot,
            cutpoints = common_cutpoints,
            bin_maps = common_bin_maps,
            settings = list(
                times = times,
                readmit_time_var = readmit_time_var,
                readmit_event_var = readmit_event_var,
                death_time_var = death_time_var,
                death_event_var = death_event_var,
                n_imputations = M,
                validation = validation,
                folds = if (validation == "cv") folds else NA_integer_,
                n_repeats = effective_repeats,
                n_runs_total = effective_repeats,
                g = g,
                min_bin_n = min_bin_n,
                compute_ici = isTRUE(compute_ici),
                ici_times = ici_times,
                ici_df = ici_df,
                tie_action = tie_action,
                parallel_cv = parallel_cv,
                future_scheduling = future_scheduling,
                debug_fit = debug_fit,
                workflow = "parallel by repetition x fold; same folds across imputations; average held-out predictions across imputations; variability across repetitions"
            )
        ),
        class = "cscox_aj_grouped_calibration_corr"
    )
}
print.cscox_aj_grouped_calibration_corr <- function(x, ...) {
    cat("Grouped calibration: cause-specific Cox vs Aalen-Johansen\n")
    cat("========================================================\n")
    cat("Validation :", x$settings$validation, "\n")
    if (!is.na(x$settings$folds)) cat("Folds      :", x$settings$folds, "\n")
    if (!is.null(x$settings$n_repeats)) cat("Repetitions:", x$settings$n_repeats, "\n")
    cat("Imputations:", x$settings$n_imputations, "\n")
    if (!is.null(x$settings$n_runs_total)) cat("Runs total :", x$settings$n_runs_total, "\n")
    cat("Groups     :", x$settings$g, "\n")
    cat("Min bin n  :", x$settings$min_bin_n, "\n")
    cat("ICI        :", if (isTRUE(x$settings$compute_ici)) "enabled" else "disabled", "\n")
    if (isTRUE(x$settings$compute_ici)) {
        cat("ICI times  :", paste(x$settings$ici_times, collapse = ", "), "\n")
        cat("ICI df     :", x$settings$ici_df, "\n")
    }
    cat("\n")

    if (is.null(x$pooled_summary) || nrow(x$pooled_summary) == 0L) {
        cat("No pooled summary available.\n")
        return(invisible(x))
    }
    cat("Variability across repetitions:\n")
    print(
        x$pooled_summary[, c(
            "time_months",
            "ici_mean", "ici_sd", "ici_p025", "ici_p975",
            "e50_mean", "e50_sd", "e50_p025", "e50_p975",
            "e90_mean", "e90_sd", "e90_p025", "e90_p975",
            "ece_mean", "ece_sd", "ece_p025", "ece_p975",
            "eo_mean", "eo_sd", "eo_p025", "eo_p975",
            "n_runs", "n_imp"
        )],
        row.names = FALSE
    )
    cat("\nNotes:\n")
    cat("  - Each repetition uses the same fold split across all imputations.\n")
    cat("  - Models are fit separately within each imputation; held-out predictions are averaged across imputations.\n")
    cat("  - Thin gray lines are repetition-specific calibration curves.\n")
    cat("  - Gray ribbon is the pointwise 2.5th-97.5th percentile band across repetitions.\n")
    cat("  - Blue points/line are the mean calibration curve across repetitions.\n")
    cat("  - ICI, E50, and E90 are only computed when requested.\n")
    cat("  - These summaries describe split-to-split variability, not formal bootstrap optimism correction.\n")
    cat("  - ECE is grouped expected calibration error.\n")
    cat("  - E/O is mean predicted / observed probability.\n")
    invisible(x)
}
plot.cscox_aj_grouped_calibration_corr <- function(x, horizon = NULL, ...) {
    if (is.null(horizon)) {
        if (is.null(x$combined_plot)) stop("No combined plot stored in object.")
        return(x$combined_plot)
    }
    nm <- paste0("month_", horizon)
    if (!nm %in% names(x$plots)) stop("Requested horizon not found.")
    x$plots[[nm]]
}
plot_schoenfeld_residuals <- function(formula, variable, df, span = 0.50) {
    df <- base::as.data.frame(base::lapply(base::as.data.frame(df), base::as.vector))
    .GlobalEnv$.tmp_df <- df
    base::on.exit(base::rm(".tmp_df", envir = .GlobalEnv))
    fit <- survival::coxph(formula, data = .tmp_df)
    res <- stats::residuals(fit, type = "scaledsch")
    time <- base::as.numeric(base::rownames(res))
    # Filter range (10th to 90th percentile)
    t_range <- stats::quantile(time, c(0.1, 0.9), na.rm = TRUE)
    valid <- time >= t_range[1] & time <= t_range[2]
    time_clean <- time[valid]
    res_clean <- res[valid, variable]
    z2 <- stats::loess(res_clean ~ time_clean, span = span)
    # THE FIX: Create an ordering index based on time
    ord <- base::order(time_clean)
    # Apply the ordering index to both x and y when plotting
    graphics::plot(time_clean[ord], stats::fitted(z2)[ord], type = "l", col = "blue", lwd = 2,
         main = base::paste("Sch. res. –", variable),
         xlab = "Time", ylab = "Residuals")
    # stats::supsmu() handles sorting internally, so it's safe as-is
    graphics::lines(stats::supsmu(time_clean, res_clean), lty = 2, col = "darkgray")
    graphics::abline(h = 0, col = "red", lty = 3)
}

compute_mi_aicc <- function(formula, imputed_list) {
    # Adjust the model in each imputation
    fits <- lapply(imputed_list, function(df) {
        survival::coxph(formula, data = df)
    })
    # Extract metrics from each model
    results <- lapply(fits, function(fit) {
        aic_val <- stats::AIC(fit)
        k <- length(fit$coefficients) # No. of parameters (df)
        n_events <- fit$nevent        # Effective sample size (no. of events)
        # Calcular la corrección para muestras pequeñas
        if (n_events <= k + 1) {
            correction <- NA_real_
          } else {
            correction <- (2 * k * (k + 1)) / (n_events - k - 1)
          }
        aicc_val <- aic_val + correction
        c(aic = aic_val, correction = correction, aicc = aicc_val, k = k, n_events = n_events)
    })
    # As dataframe
    res_df <- as.data.frame(do.call(rbind, results))
    mean_aic <- mean(res_df$aic)
    mean_aicc <- mean(res_df$aicc)
    mean_correction <- mean(res_df$correction)
    cat("\n======================================================\n")
    cat("AICc ANALYSIS FOR MULTIPLY IMPUTED DATA\n")
    cat("========================================================\n")
    cat(sprintf("Mean AIC:  %.2f\n", mean_aic))
    cat(sprintf("Mean AICc: %.2f\n", mean_aicc))
    cat(sprintf("Correction added (Penalty): %.6f\n", mean_correction))
    cat(sprintf("Parameters (k): %d | Effective Sample Size (Events): %d\n", 
                res_df$k[1], res_df$n_events[1]))
    cat("========================================================\n")
    return(list(mean_aicc = mean_aicc, full_results = res_df))
}
.cph_boot_prepare_model_split <- function(train, test, categorical_formula_vars) {
    collect_observed_levels <- function(x) {
        raw <- as.character(x)
        sort(unique(raw[!is.na(x)]), method = "radix")
    }

    sanitize_level_name <- function(var, level) {
        paste0(var, "__", make.names(as.character(level)))
    }

    build_level_mapping <- function(var, raw_levels) {
        sanitized <- vapply(
            raw_levels,
            function(level) sanitize_level_name(var, level),
            character(1)
        )
        sanitized <- make.unique(sanitized, sep = "__dup")
        stats::setNames(sanitized, raw_levels)
    }

    train_out <- train
    test_out <- test
    for (var in categorical_formula_vars) {
        if (!var %in% names(train_out)) {
            next
        }
        train_raw <- train_out[[var]]
        test_raw <- test_out[[var]]
        train_chr <- as.character(train_raw)
        test_chr <- as.character(test_raw)
        train_levels <- collect_observed_levels(train_raw)
        if (length(train_levels) == 0L) {
            next
        }
        unseen <- !is.na(test_raw) & !(test_chr %in% train_levels)
        if (any(unseen)) {
            return(list(train = train_out, test = test_out, error = paste0("unseen_factor_level:", var)))
        }
        mapping <- build_level_mapping(var, train_levels)
        train_sanitized <- rep(NA_character_, length(train_chr))
        test_sanitized <- rep(NA_character_, length(test_chr))
        train_non_missing <- !is.na(train_raw)
        test_non_missing <- !is.na(test_raw)
        train_sanitized[train_non_missing] <- unname(mapping[train_chr[train_non_missing]])
        test_sanitized[test_non_missing] <- unname(mapping[test_chr[test_non_missing]])
        train_out[[var]] <- factor(train_sanitized, levels = unname(mapping))
        test_out[[var]] <- factor(test_sanitized, levels = unname(mapping))
    }
    list(train = train_out, test = test_out, error = "")
}

.cph_boot_extract_ibs <- function(pe_obj, t_value, model_name, verbose_flag = FALSE) {
    if (!inherits(pe_obj, "pec")) {
        return(list(value = NA_real_, err = "invalid_pec_object"))
    }
    cr <- tryCatch(
        suppressWarnings(pec::crps(pe_obj, times = t_value)),
        error = function(e) NULL
    )
    if (is.null(cr)) {
        return(list(value = NA_real_, err = "crps_extraction_failed"))
    }
    val <- NA_real_
    if (is.matrix(cr) || is.data.frame(cr)) {
        if (model_name %in% rownames(cr)) {
            val <- as.numeric(cr[model_name, 1L])
        } else if (nrow(cr) > 0L) {
            val <- as.numeric(cr[1L, 1L])
            if (verbose_flag) {
                warning("Model name '", model_name, "' not found, using first available.")
            }
        }
    } else {
        val <- as.numeric(cr[1L])
    }
    if (!is.finite(val)) {
        return(list(value = NA_real_, err = "non_finite_crps"))
    }
    list(value = val, err = "")
}

.cph_boot_worker <- function(b, state) {
    row_ibs <- rep(NA_real_, state$M)
    row_fails <- rep("", state$M)
    split_fail <- ""

    idx_train <- sample.int(n = state$n, size = state$n, replace = TRUE)
    idx_test <- setdiff(seq_len(state$n), unique(idx_train))
    if (length(idx_test) < state$min_oob_n) {
        split_fail <- "too_few_oob_rows"
        return(list(ibs = row_ibs, fails = row_fails, split_fail = split_fail))
    }

    for (m in seq_len(state$M)) {
        oob_events <- sum(state$event_indicator[[m]][idx_test] == 1L, na.rm = TRUE)
        if (oob_events < state$min_oob_events) {
            row_fails[m] <- "too_few_oob_events"
            next
        }
        n_evt_tau <- sum(
            state$event_indicator[[m]][idx_test] == 1L &
                state$df_list[[m]][idx_test, state$time_col] <= state$tau,
            na.rm = TRUE
        )
        if (n_evt_tau < 1L) {
            row_fails[m] <- "no_events_before_tau"
            next
        }

        df_train <- state$df_list[[m]][idx_train, , drop = FALSE]
        df_test <- state$df_list[[m]][idx_test, , drop = FALSE]
        prepared <- .cph_boot_prepare_model_split(df_train, df_test, state$categorical_formula_vars)
        if (nzchar(prepared$error)) {
            row_fails[m] <- prepared$error
            next
        }
        df_train <- prepared$train
        df_test <- prepared$test

        res <- tryCatch(
            {
                fit <- rms::cph(
                    state$model_formula,
                    data = df_train,
                    x = TRUE,
                    y = TRUE,
                    surv = TRUE,
                    time.inc = state$tau
                )
                fit$call$data <- quote(df_train)
                pe <- do.call(
                    pec::pec,
                    list(
                        object = stats::setNames(list(fit), state$model_name),
                        data = df_test,
                        formula = state$cens_fml,
                        splitMethod = "none",
                        times = state$eval_times,
                        cens.model = "marginal",
                        exact = TRUE,
                        verbose = FALSE
                    )
                )
                ibs_res <- .cph_boot_extract_ibs(pe, state$tau, model_name = state$model_name, verbose_flag = state$verbose)
                if (is.na(ibs_res$value) && length(state$fallback_times) > 0L) {
                    for (tt in state$fallback_times) {
                        ibs_try <- .cph_boot_extract_ibs(pe, tt, model_name = state$model_name, verbose_flag = state$verbose)
                        if (!is.na(ibs_try$value)) {
                            ibs_res <- ibs_try
                            break
                        }
                    }
                }
                if (is.na(ibs_res$value)) {
                    list(value = NA_real_, err = ibs_res$err)
                } else {
                    list(value = ibs_res$value, err = "")
                }
            },
            error = function(e) {
                list(value = NA_real_, err = paste0("cph_or_pec_error:", conditionMessage(e)))
            }
        )
        row_ibs[m] <- res$value
        if (nzchar(res$err)) {
            row_fails[m] <- res$err
        }
    }

    list(ibs = row_ibs, fails = row_fails, split_fail = split_fail)
}
compare_mi_models_d1 <- function(formula_full, formula_reduced, imputed_list) {
  # 1. Validate that the input is a list of dataframes
  if (!is.list(imputed_list) || length(imputed_list) == 0) {
    stop("imputed_list must be a list of dataframes.")
  }
  cat("Fitting the FULL model on", length(imputed_list), "imputations...\n")
  fits_full <- lapply(imputed_list, function(df) {
    survival::coxph(formula_full, data = df)
  })
  cat("Fitting the REDUCED model on", length(imputed_list), "imputations...\n")
  fits_reduced <- lapply(imputed_list, function(df) {
    survival::coxph(formula_reduced, data = df)
  })
  # 2. Convert to MIRA objects (Multiple Imputation Repeated Analyses)
  mira_full    <- mice::as.mira(fits_full)
  mira_reduced <- mice::as.mira(fits_reduced)
  # 3. Execute the Multivariate Wald Test (D1)
  cat("Executing the Wald Test (D1)...\n")
  comparison <- mice::D1(mira_full, mira_reduced)
  cat("\n============================================================\n")
  cat("MODEL COMPARISON RESULT (D1 Statistic)\n")
  cat("============================================================\n")
  print(comparison)
  cat("============================================================\n")
  # Quick interpretation printed to console
  p_val <- comparison$p.value[1]
}

