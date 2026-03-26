to01 <- function(x) {
    if (is.logical(x)) {
        return(as.integer(x))
    }
    if (is.factor(x)) {
        x <- as.character(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    vals <- x_num[!is.na(x_num)]
    if (length(vals) == 0L) {
        stop("event_col has no non-missing values.")
    }
    if (all(vals %in% c(0, 1))) {
        return(as.integer(x_num))
    }
    if (all(vals %in% c(1, 2))) {
        return(as.integer(x_num - 1L))
    }
    stop("event_col must be binary 0/1 (or 1/2).")
}
ibs_ipcw_train <- function(
    df_train,
    df_test,
    surv_mat,
    times,
    time_col,
    event_col,
    eps = 1e-8,
    g_min = 0.05
) {
    ev_train <- to01(df_train[[event_col]])
    sf_cens <- survival::survfit(
        survival::Surv(df_train[[time_col]], 1 - ev_train) ~ 1
    )

    safe_times <- sf_cens$time[sf_cens$surv >= g_min]
    tau_ipcw <- if (length(safe_times)) max(safe_times) else max(times)

    keep <- times <= tau_ipcw
    if (sum(keep) < 2L) {
        return(NA_real_)
    }

    times_use <- times[keep]
    surv_use <- surv_mat[, keep, drop = FALSE]

    G_at <- function(tt) {
        g <- summary(sf_cens, times = pmax(tt, 0), extend = TRUE)$surv
        pmax(g, g_min)
    }

    Tt <- as.numeric(df_test[[time_col]])
    Dt <- to01(df_test[[event_col]])

    bs_t <- vapply(seq_along(times_use), function(j) {
        t0 <- times_use[j]
        S0 <- surv_use[, j]
        Y0 <- as.numeric(Tt > t0)
        w <- ifelse(
            Tt <= t0 & Dt == 1L,
            1 / G_at(pmax(Tt - eps, 0)),
            ifelse(Tt > t0, 1 / G_at(t0), 0)
        )
        mean(w * (Y0 - S0)^2, na.rm = TRUE)
    }, numeric(1))

    denom <- max(times_use) - min(times_use)
    if (!is.finite(denom) || denom <= 0) {
        return(NA_real_)
    }

    as.numeric(
        sum(diff(times_use) * (head(bs_t, -1L) + tail(bs_t, -1L)) / 2) / denom
    )
}

.cph_boot_event01 <- function(x, varname) {
    if (is.logical(x)) {
        return(as.integer(x))
    }
    if (is.factor(x)) {
        x <- as.character(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    vals <- x_num[!is.na(x_num)]

    if (length(vals) == 0L) {
        stop("No non-missing event values found in '", varname, "'.")
    }
    if (all(vals %in% c(0, 1))) {
        return(as.integer(x_num))
    }
    if (all(vals %in% c(1, 2))) {
        return(as.integer(x_num - 1L))
    }

    stop(
        "Variable '", varname,
        "' must be binary 0/1 (or 1/2), possibly stored as factor/character."
    )
}

.cph_boot_normalize_time <- function(x, varname) {
    if (!is.numeric(x)) {
        stop("Variable '", varname, "' must be numeric.")
    }
    x <- as.numeric(x)
    non_missing <- !is.na(x)
    if (any(!is.finite(x[non_missing]))) {
        stop("Variable '", varname, "' contains non-finite values.")
    }
    if (any(x[non_missing] < 0)) {
        stop("Variable '", varname, "' contains negative follow-up times.")
    }
    x
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
            return(list(
                train = train_out,
                test = test_out,
                error = paste0("unseen_factor_level:", var)
            ))
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

.cph_boot_worker <- function(b, state) {
    tryCatch(
        {
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

                prepared <- .cph_boot_prepare_model_split(
                    df_train,
                    df_test,
                    state$categorical_formula_vars
                )
                if (nzchar(prepared$error)) {
                    row_fails[m] <- prepared$error
                    next
                }
                df_train <- prepared$train
                df_test <- prepared$test

                res <- tryCatch(
                    {
                        fit <- survival::coxph(
                            state$model_formula,
                            data = df_train,
                            x = TRUE,
                            y = TRUE,
                            model = TRUE
                        )

                        surv_mat <- pec::predictSurvProb(
                            fit,
                            newdata = df_test,
                            times = state$eval_times
                        )

                        if (is.null(dim(surv_mat))) {
                            surv_mat <- matrix(
                                surv_mat,
                                nrow = nrow(df_test),
                                ncol = length(state$eval_times),
                                byrow = TRUE
                            )
                        } else {
                            surv_mat <- as.matrix(surv_mat)
                        }

                        if (nrow(surv_mat) != nrow(df_test) ||
                            ncol(surv_mat) != length(state$eval_times)) {
                            list(value = NA_real_, err = "predictSurvProb_dimension_mismatch")
                        } else {
                            ibs_val <- ibs_ipcw_train(
                                df_train = df_train,
                                df_test = df_test,
                                surv_mat = surv_mat,
                                times = state$eval_times,
                                time_col = state$time_col,
                                event_col = state$event_col
                            )

                            if (is.finite(ibs_val)) {
                                list(value = ibs_val, err = "")
                            } else {
                                list(value = NA_real_, err = "non_finite_ibs")
                            }
                        }
                    },
                    error = function(e) {
                        list(
                            value = NA_real_,
                            err = paste0("coxph_or_predict_error:", conditionMessage(e))
                        )
                    }
                )

                row_ibs[m] <- res$value
                if (nzchar(res$err)) {
                    row_fails[m] <- res$err
                }
            }

            list(ibs = row_ibs, fails = row_fails, split_fail = split_fail)
        },
        error = function(e) {
            list(
                ibs = rep(NA_real_, state$M),
                fails = rep("", state$M),
                split_fail = paste0("worker_error:", conditionMessage(e))
            )
        }
    )
}

cph_evaluate_boot_oob_mi_corrected <- function(
    formula,
    imputed_list,
    time_col,
    event_col,
    tau = NULL,
    B = 200,
    n_eval_times = 100,
    seed = 2125,
    cpus = 8,
    verbose = TRUE,
    min_oob_n = 10,
    min_oob_events = 3,
    min_valid_imputations = NULL,
    id_col = NULL
) {
    stop_design_validation <- function(msg) {
        stop(paste0("design_validation_error:", msg), call. = FALSE)
    }

    is_pos_int <- function(x) {
        is.numeric(x) && length(x) == 1L && !is.na(x) && x > 0 && x == as.integer(x)
    }

    normalize_call_name <- function(fun_expr) {
        txt <- paste(deparse(fun_expr, width.cutoff = 500L), collapse = "")
        txt <- gsub("`", "", txt, fixed = TRUE)
        sub("^.*::+", "", txt)
    }

    expr_contains_special_call <- function(expr, specials) {
        found <- FALSE
        walk <- function(node) {
            if (found || !is.call(node)) {
                return(invisible(NULL))
            }
            node_name <- normalize_call_name(node[[1L]])
            if (node_name %in% specials) {
                found <<- TRUE
                return(invisible(NULL))
            }
            if (length(node) >= 2L) {
                for (i in 2L:length(node)) {
                    walk(node[[i]])
                    if (found) {
                        break
                    }
                }
            }
            invisible(NULL)
        }
        walk(expr)
        found
    }

    collect_special_single_vars <- function(expr, specials) {
        vars <- character(0)
        walk <- function(node) {
            if (!is.call(node)) {
                return(invisible(NULL))
            }
            node_name <- normalize_call_name(node[[1L]])
            if (node_name %in% specials) {
                if (length(node) != 2L || !is.symbol(node[[2L]])) {
                    stop_design_validation(
                        paste0(
                            "complex stratification unsupported for term '",
                            paste(deparse(node, width.cutoff = 500L), collapse = ""),
                            "'. Use a bare column name inside ",
                            node_name,
                            "()."
                        )
                    )
                }
                vars <<- c(vars, as.character(node[[2L]]))
                return(invisible(NULL))
            }
            if (length(node) >= 2L) {
                for (i in 2L:length(node)) {
                    walk(node[[i]])
                }
            }
            invisible(NULL)
        }
        walk(expr)
        unique(vars)
    }

    collect_explicit_factor_vars <- function(expr) {
        vars <- character(0)
        walk <- function(node) {
            if (!is.call(node)) {
                return(invisible(NULL))
            }
            node_name <- normalize_call_name(node[[1L]])
            if (node_name %in% c("factor", "as.factor")) {
                factor_vars <- unique(all.vars(node[[2L]]))
                if (length(factor_vars) != 1L) {
                    stop_design_validation(
                        paste0(
                            "explicit categorical coercion must reference exactly one column in term '",
                            paste(deparse(node, width.cutoff = 500L), collapse = ""),
                            "'."
                        )
                    )
                }
                vars <<- c(vars, factor_vars)
            }
            if (length(node) >= 2L) {
                for (i in 2L:length(node)) {
                    walk(node[[i]])
                }
            }
            invisible(NULL)
        }
        walk(expr)
        unique(vars)
    }

    rewrite_explicit_factor_calls <- function(expr) {
        if (!is.call(expr)) {
            return(expr)
        }
        node_name <- normalize_call_name(expr[[1L]])
        if (node_name %in% c("factor", "as.factor")) {
            factor_vars <- unique(all.vars(expr[[2L]]))
            if (length(factor_vars) != 1L) {
                stop_design_validation(
                    paste0(
                        "explicit categorical coercion must reference exactly one column in term '",
                        paste(deparse(expr, width.cutoff = 500L), collapse = ""),
                        "'."
                    )
                )
            }
            return(as.name(factor_vars))
        }
        for (i in seq_along(expr)) {
            expr[[i]] <- rewrite_explicit_factor_calls(expr[[i]])
        }
        expr
    }

    rewrite_strata_calls <- function(expr) {
        if (!is.call(expr)) {
            return(expr)
        }
        node_name <- normalize_call_name(expr[[1L]])
        if (node_name %in% c("strat", "strata")) {
            expr[[1L]] <- str2lang("survival::strata")
        }
        for (i in seq_along(expr)) {
            expr[[i]] <- rewrite_strata_calls(expr[[i]])
        }
        expr
    }

    collect_observed_levels <- function(x) {
        raw <- as.character(x)
        sort(unique(raw[!is.na(x)]), method = "radix")
    }

    format_level_sets <- function(level_sets) {
        paste(
            vapply(
                seq_along(level_sets),
                function(i) {
                    sprintf(
                        "imputation %d: [%s]",
                        i,
                        paste(level_sets[[i]], collapse = ", ")
                    )
                },
                character(1)
            ),
            collapse = "; "
        )
    }

    t_start <- Sys.time()

    if (!inherits(formula, "formula")) {
        stop("formula must be a formula.")
    }
    if (!is.list(imputed_list) || length(imputed_list) == 0L) {
        stop("imputed_list must be a non-empty list.")
    }
    if (!is.character(time_col) || length(time_col) != 1L) {
        stop("time_col must be a single column name.")
    }
    if (!is.character(event_col) || length(event_col) != 1L) {
        stop("event_col must be a single column name.")
    }
    if (!is_pos_int(B)) stop("B must be a positive integer.")
    if (!is_pos_int(min_oob_n)) stop("min_oob_n must be a positive integer.")
    if (!is_pos_int(min_oob_events)) stop("min_oob_events must be a positive integer.")
    if (!is_pos_int(n_eval_times)) stop("n_eval_times must be a positive integer.")
    if (!is_pos_int(cpus)) stop("cpus must be a positive integer.")

    B <- as.integer(B)
    min_oob_n <- as.integer(min_oob_n)
    min_oob_events <- as.integer(min_oob_events)
    n_eval_times <- as.integer(n_eval_times)
    cpus <- as.integer(cpus)

    df_list_raw <- lapply(imputed_list, as.data.frame)
    M <- length(df_list_raw)
    n_by_imp <- vapply(df_list_raw, nrow, integer(1))
    if (length(unique(n_by_imp)) != 1L) {
        stop("All imputations must have same number of rows.")
    }
    n <- n_by_imp[1L]

    full_terms <- stats::terms(formula, specials = c("strat", "strata"), data = df_list_raw[[1L]])
    rhs_terms <- stats::delete.response(full_terms)
    rhs_vars <- unique(all.vars(rhs_terms))
    required_cols <- unique(c(rhs_vars, time_col, event_col))

    for (m in seq_len(M)) {
        miss <- setdiff(required_cols, names(df_list_raw[[m]]))
        if (length(miss) > 0L) {
            stop(sprintf("Imputation %d is missing columns: %s", m, paste(miss, collapse = ", ")))
        }
    }

    if (!is.null(id_col)) {
        if (!is.character(id_col) || length(id_col) != 1L) {
            stop("id_col must be NULL or a single column name.")
        }
        if (!all(vapply(df_list_raw, function(d) id_col %in% names(d), logical(1)))) {
            stop("id_col not found in at least one imputation.")
        }
        ref_id <- df_list_raw[[1L]][[id_col]]
        same_order <- vapply(df_list_raw, function(d) identical(d[[id_col]], ref_id), logical(1))
        if (!all(same_order)) {
            stop("Row order differs across imputations (checked with id_col).")
        }
    }

    if (is.null(min_valid_imputations)) {
        min_valid_imputations <- M
    }
    if (!is_pos_int(min_valid_imputations) || min_valid_imputations > M) {
        stop("min_valid_imputations must be an integer between 1 and M.")
    }
    min_valid_imputations <- as.integer(min_valid_imputations)

    raw_event_values_first_imputation <- unique(df_list_raw[[1L]][[event_col]])
    raw_event_values_first_imputation <- raw_event_values_first_imputation[
        !is.na(raw_event_values_first_imputation)
    ]

    df_list <- lapply(df_list_raw, function(d) {
        d <- as.data.frame(d)
        d[[time_col]] <- .cph_boot_normalize_time(d[[time_col]], time_col)
        d[[event_col]] <- .cph_boot_event01(d[[event_col]], event_col)
        d
    })

    if (M > 1L) {
        ref_time <- df_list[[1L]][[time_col]]
        ref_event <- df_list[[1L]][[event_col]]
        same_time <- vapply(
            df_list[-1L],
            function(d) isTRUE(all.equal(
                d[[time_col]],
                ref_time,
                tolerance = 1e-12,
                check.attributes = FALSE
            )),
            logical(1)
        )
        same_event <- vapply(
            df_list[-1L],
            function(d) identical(d[[event_col]], ref_event),
            logical(1)
        )
        if (!all(same_time) || !all(same_event)) {
            stop(
                "Outcome/time information differs across imputations. ",
                "This workflow assumes only predictors were imputed."
            )
        }
    }

    term_labels <- attr(full_terms, "term.labels")
    rhs_expr <- formula[[length(formula)]]
    strata_vars <- collect_special_single_vars(rhs_expr, c("strat", "strata"))
    explicit_factor_vars <- collect_explicit_factor_vars(rhs_expr)
    character_factor_vars <- rhs_vars[vapply(
        rhs_vars,
        function(var) {
            any(vapply(
                df_list_raw,
                function(d) is.factor(d[[var]]) || is.character(d[[var]]),
                logical(1)
            ))
        },
        logical(1)
    )]
    categorical_formula_vars <- unique(c(strata_vars, explicit_factor_vars, character_factor_vars))

    non_strat_term_labels <- term_labels[!vapply(
        term_labels,
        function(label) expr_contains_special_call(str2lang(label), c("strat", "strata")),
        logical(1)
    )]

    for (strata_var in strata_vars) {
        used_outside_strat <- vapply(
            non_strat_term_labels,
            function(label) strata_var %in% all.vars(str2lang(label)),
            logical(1)
        )
        if (any(used_outside_strat)) {
            offending_terms <- paste(non_strat_term_labels[used_outside_strat], collapse = ", ")
            stop_design_validation(
                paste0(
                    "variable '",
                    strata_var,
                    "' appears inside strat()/strata() and elsewhere in the formula (",
                    offending_terms,
                    "). This implementation does not permit a variable to be both a stratifier and an ordinary predictor/interactor."
                )
            )
        }
    }

    if (length(categorical_formula_vars) > 0L) {
        mismatches <- character(0)
        for (var in categorical_formula_vars) {
            level_sets <- lapply(df_list_raw, function(d) collect_observed_levels(d[[var]]))
            ref_levels <- level_sets[[1L]]
            same_levels <- vapply(level_sets[-1L], identical, logical(1), ref_levels)
            if (!all(same_levels)) {
                mismatches <- c(
                    mismatches,
                    paste0(var, " -> ", format_level_sets(level_sets))
                )
            }
        }
        if (length(mismatches) > 0L) {
            stop_design_validation(
                paste0(
                    "categorical formula variables have inconsistent observed levels across imputations. ",
                    paste(mismatches, collapse = " | ")
                )
            )
        }
    }

    event_value <- 1L
    event_indicator <- lapply(df_list, function(d) d[[event_col]])

    if (is.null(tau)) {
        all_times <- unlist(lapply(df_list, function(d) d[[time_col]]), use.names = FALSE)
        tau <- as.numeric(stats::quantile(all_times, probs = 0.90, na.rm = TRUE))
        if (verbose) {
            message("Auto-selected tau (90th percentile): ", round(tau, 2))
        }
    }
    if (!is.numeric(tau) || length(tau) != 1L || !is.finite(tau) || tau <= 0) {
        stop("tau must be a finite positive scalar.")
    }

    eval_times <- seq(1e-6, tau, length.out = n_eval_times)
    model_formula_expr <- rewrite_strata_calls(rewrite_explicit_factor_calls(formula))
    model_formula_txt <- paste(deparse(model_formula_expr, width.cutoff = 500L), collapse = " ")
    model_formula <- stats::as.formula(model_formula_txt, env = environment(formula))
    has_strata <- length(strata_vars) > 0L
    formula_validated <- FALSE

    preflight_split <- .cph_boot_prepare_model_split(
        df_list[[1L]],
        df_list[[1L]],
        categorical_formula_vars
    )
    if (nzchar(preflight_split$error)) {
        stop_design_validation(
            paste0("preflight categorical preparation failed: ", preflight_split$error)
        )
    }

    preflight_error <- tryCatch(
        {
            fit <- survival::coxph(
                model_formula,
                data = preflight_split$train,
                x = TRUE,
                y = TRUE,
                model = TRUE
            )
            tmp <- pec::predictSurvProb(
                fit,
                newdata = preflight_split$test[1L, , drop = FALSE],
                times = eval_times[1:2]
            )
            invisible(tmp)
            NULL
        },
        error = function(e) conditionMessage(e)
    )
    if (!is.null(preflight_error)) {
        stop_design_validation(
            paste0("preflight coxph/predictSurvProb failed. Original error: ", preflight_error)
        )
    }
    formula_validated <- TRUE

    detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
    if (!is.numeric(detected) || length(detected) != 1L || !is.finite(detected) || detected < 1L) {
        detected <- 1L
    }
    is_windows <- identical(.Platform$OS.type, "windows")
    worker_cap <- if (is_windows) {
        min(8L, max(1L, detected - 1L))
    } else {
        max(1L, detected - 1L)
    }
    n_cores <- as.integer(max(1L, min(cpus, B, worker_cap)))

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    if (n_cores > 1L) {
        future::plan(future::multisession, workers = n_cores)
    } else {
        future::plan(future::sequential)
    }

    if (verbose) {
        if (has_strata) {
            message(
                "Stratified model detected: validated strata variables = ",
                paste(strata_vars, collapse = ", ")
            )
        }
        if (length(categorical_formula_vars) > 0L) {
            message(
                "Categorical formula variables: ",
                paste(categorical_formula_vars, collapse = ", ")
            )
        }
        cat(sprintf("Starting OOB bootstrap + MI: B=%d, M=%d\n", B, M))
        cat(sprintf("Evaluating IBS up to tau = %.4f\n", tau))
        cat(sprintf("Using %d worker(s)...\n", n_cores))
        cat("------------------------------------------------------------\n")
    }

    worker_state <- list(
        M = M,
        n = n,
        min_oob_n = min_oob_n,
        min_oob_events = min_oob_events,
        tau = tau,
        time_col = time_col,
        event_col = event_col,
        event_indicator = event_indicator,
        df_list = df_list,
        categorical_formula_vars = categorical_formula_vars,
        model_formula = model_formula,
        eval_times = eval_times,
        verbose = verbose
    )

    boot_results <- future.apply::future_lapply(
        X = seq_len(B),
        FUN = .cph_boot_worker,
        state = worker_state,
        future.seed = seed,
        future.packages = c("survival", "pec"),
        future.globals = list(
            .cph_boot_worker = .cph_boot_worker,
            .cph_boot_prepare_model_split = .cph_boot_prepare_model_split,
            ibs_ipcw_train = ibs_ipcw_train,
            to01 = to01
        )
    )

    results_matrix <- do.call(rbind, lapply(boot_results, `[[`, "ibs"))
    fail_matrix <- do.call(rbind, lapply(boot_results, `[[`, "fails"))
    split_fail_reason <- vapply(boot_results, `[[`, "", "split_fail")

    boot_pooled <- apply(results_matrix, 1L, function(x) {
        valid <- x[is.finite(x)]
        if (length(valid) < min_valid_imputations) {
            NA_real_
        } else {
            mean(valid)
        }
    })
    boot_pooled <- boot_pooled[is.finite(boot_pooled)]

    if (length(boot_pooled) < 2L) {
        cat("\n[!] CRITICAL FAILURE: Almost all bootstrap replicates failed.\n")
        cat("\n--- TOP REASONS FOR FAILURE ---\n")
        all_fails <- c(
            as.vector(fail_matrix[fail_matrix != ""]),
            split_fail_reason[split_fail_reason != ""]
        )
        if (length(all_fails) > 0L) {
            print(sort(table(all_fails), decreasing = TRUE))
        } else {
            cat("No explicit errors captured.\n")
        }
        cat("-------------------------------\n")
        stop("Too few valid bootstrap replicates to estimate IBS or confidence intervals.")
    }

    pooled_ibs <- mean(boot_pooled)
    ci <- stats::quantile(
        boot_pooled,
        probs = c(0.025, 0.975),
        na.rm = TRUE,
        names = FALSE
    )

    if (verbose) {
        cat("\n============================================================\n")
        cat("FINAL POOLED RESULTS (OOB Bootstrap + MI)\n")
        cat("============================================================\n")
        cat(sprintf("Pooled OOB IBS: %.4f (95%% CI: %.4f - %.4f)\n", pooled_ibs, ci[1L], ci[2L]))
        cat(sprintf("Valid bootstrap replicates: %d / %d\n", length(boot_pooled), B))
        cat("Event coding: internally normalized to 0/1\n")
        cat("============================================================\n")
        cat(sprintf(
            "Total runtime: %.2f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))
        ))
    }

    invisible(list(
        pooled_ibs = pooled_ibs,
        ci_95 = c(lower = ci[1L], upper = ci[2L]),
        boot_pooled = boot_pooled,
        imputation_means = colMeans(results_matrix, na.rm = TRUE),
        raw_matrix = results_matrix,
        fail_matrix = fail_matrix,
        split_fail_reason = split_fail_reason,
        event_value = event_value,
        reproducibility = list(
            call = match.call(),
            seed = seed,
            B = B,
            M = M,
            tau = tau,
            min_oob_n = min_oob_n,
            min_oob_events = min_oob_events,
            min_valid_imputations = min_valid_imputations,
            event_value = event_value,
            raw_event_values_first_imputation = raw_event_values_first_imputation,
            n_eval_times = n_eval_times,
            n_cores = n_cores,
            valid_boot = length(boot_pooled),
            has_strata = has_strata,
            strata_vars = strata_vars,
            categorical_formula_vars = categorical_formula_vars,
            formula_validated = formula_validated
        )
    ))
}
