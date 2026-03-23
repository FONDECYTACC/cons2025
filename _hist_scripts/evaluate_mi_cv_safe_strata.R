# Standalone evaluator for multiply-imputed Cox CV with safer strata handling.
#
# Usage:
# source("evaluate_mi_cv_safe_strata.R")
# results_death <- evaluate_mi_cv_safe_strata(
#   formula = formula_death,
#   imputed_list = py_corrected_datasets,
#   time_col = "death_time_from_disch_m",
#   event_col = "death_event"
# )

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
    # Out-of-sample IPCW should use the censoring distribution estimated on train.
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

.mi_cv_normalize_call_name <- function(fun_expr) {
    txt <- paste(deparse(fun_expr, width.cutoff = 500L), collapse = "")
    txt <- gsub("`", "", txt, fixed = TRUE)
    sub("^.*::+", "", txt)
}

.mi_cv_rewrite_strata_calls <- function(expr) {
    if (!is.call(expr)) {
        return(expr)
    }
    node_name <- .mi_cv_normalize_call_name(expr[[1L]])
    if (node_name %in% c("strat", "strata")) {
        expr[[1L]] <- str2lang("survival::strata")
    }
    for (i in seq_along(expr)) {
        expr[[i]] <- .mi_cv_rewrite_strata_calls(expr[[i]])
    }
    expr
}

.mi_cv_collect_explicit_factor_vars <- function(expr) {
    vars <- character(0)
    walk <- function(node) {
        if (!is.call(node)) {
            return(invisible(NULL))
        }
        node_name <- .mi_cv_normalize_call_name(node[[1L]])
        if (node_name %in% c("factor", "as.factor")) {
            factor_vars <- unique(all.vars(node[[2L]]))
            if (length(factor_vars) != 1L) {
                stop(
                    "Explicit categorical coercion must reference exactly one column in term '",
                    paste(deparse(node, width.cutoff = 500L), collapse = ""),
                    "'.",
                    call. = FALSE
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

.mi_cv_collect_observed_levels <- function(x) {
    raw <- as.character(x)
    sort(unique(raw[!is.na(x)]), method = "radix")
}

.mi_cv_format_level_sets <- function(level_sets) {
    paste(
        vapply(
            seq_along(level_sets),
            function(i) {
                sprintf("imputation %d: [%s]", i, paste(level_sets[[i]], collapse = ", "))
            },
            character(1)
        ),
        collapse = "; "
    )
}

.mi_cv_sanitize_level_name <- function(var, level) {
    paste0(var, "__", make.names(as.character(level)))
}

.mi_cv_build_level_mapping <- function(var, raw_levels) {
    sanitized <- vapply(
        raw_levels,
        function(level) .mi_cv_sanitize_level_name(var, level),
        character(1)
    )
    sanitized <- make.unique(sanitized, sep = "__dup")
    stats::setNames(sanitized, raw_levels)
}

.mi_cv_prepare_model_split <- function(train, test, categorical_formula_vars) {
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
        train_levels <- .mi_cv_collect_observed_levels(train_raw)

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

        mapping <- .mi_cv_build_level_mapping(var, train_levels)

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

.mi_cv_mean_or_na <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) {
        NA_real_
    } else {
        mean(x)
    }
}

.mi_cv_quantile_or_na <- function(x, probs) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) {
        rep(NA_real_, length(probs))
    } else {
        as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
    }
}

.mi_cv_expr_label <- function(expr) {
    paste(deparse(expr, width.cutoff = 500L), collapse = "")
}

.mi_cv_collect_strata_term_labels <- function(expr) {
    labels <- character(0)

    walk <- function(node) {
        if (!is.call(node)) {
            return(invisible(NULL))
        }
        node_name <- .mi_cv_normalize_call_name(node[[1L]])
        if (node_name %in% c("strat", "strata")) {
            labels <<- c(labels, .mi_cv_expr_label(node))
        }
        if (length(node) >= 2L) {
            for (i in 2L:length(node)) {
                walk(node[[i]])
            }
        }
        invisible(NULL)
    }

    walk(expr)
    unique(labels)
}

.mi_cv_extract_strata_args <- function(term_expr, term_label) {
    expr <- .mi_cv_rewrite_strata_calls(term_expr)
    if (!is.call(expr) || .mi_cv_normalize_call_name(expr[[1L]]) != "strata") {
        stop("Invalid strata term label: ", term_label, call. = FALSE)
    }
    if (length(expr) < 2L) {
        stop("Strata term has no arguments: ", term_label, call. = FALSE)
    }
    as.list(expr)[-1L]
}

.mi_cv_coerce_rowwise_value <- function(value, n_rows, term_label, arg_index) {
    if (is.data.frame(value)) {
        if (ncol(value) != 1L) {
            stop(
                "Strata term '", term_label, "' argument ", arg_index,
                " produced a data frame with ", ncol(value),
                " columns; expected one row-wise value per row.",
                call. = FALSE
            )
        }
        value <- value[[1L]]
    } else if (is.matrix(value) || is.array(value)) {
        if (length(dim(value)) != 2L || dim(value)[2L] != 1L) {
            stop(
                "Strata term '", term_label, "' argument ", arg_index,
                " produced a matrix/array with incompatible dimensions.",
                call. = FALSE
            )
        }
        value <- value[, 1L, drop = TRUE]
    }

    if (is.list(value) && !is.object(value)) {
        if (length(value) != n_rows) {
            stop(
                "Strata term '", term_label, "' argument ", arg_index,
                " produced length ", length(value), " but expected ", n_rows, ".",
                call. = FALSE
            )
        }
        out <- vapply(value, function(x) paste(as.character(x), collapse = ","), character(1))
    } else {
        if (length(value) != n_rows) {
            stop(
                "Strata term '", term_label, "' argument ", arg_index,
                " produced length ", length(value), " but expected ", n_rows, ".",
                call. = FALSE
            )
        }
        out <- as.character(value)
    }

    if (length(out) != n_rows) {
        stop(
            "Strata term '", term_label, "' argument ", arg_index,
            " did not coerce to one value per row.",
            call. = FALSE
        )
    }

    out
}

.mi_cv_make_strata_signature_safe <- function(
    data,
    strata_term_labels,
    formula_env,
    na_token = ".__NA__."
) {
    if (length(strata_term_labels) == 0L) {
        return(rep.int(".__no_strata__", nrow(data)))
    }

    n_rows <- nrow(data)

    sig_parts <- lapply(strata_term_labels, function(term_label) {
        term_expr <- str2lang(term_label)
        arg_exprs <- .mi_cv_extract_strata_args(term_expr, term_label)

        arg_keys <- lapply(seq_along(arg_exprs), function(arg_index) {
            arg_expr <- arg_exprs[[arg_index]]

            value <- tryCatch(
                eval(arg_expr, envir = data, enclos = formula_env),
                error = function(e) {
                    stop(
                        "Failed to evaluate strata term '", term_label,
                        "' argument ", arg_index, ": ", conditionMessage(e),
                        call. = FALSE
                    )
                }
            )

            out <- .mi_cv_coerce_rowwise_value(value, n_rows, term_label, arg_index)
            out[is.na(out)] <- na_token
            out
        })

        if (length(arg_keys) == 1L) {
            arg_keys[[1L]]
        } else {
            do.call(paste, c(arg_keys, sep = "\f"))
        }
    })

    out <- do.call(paste, c(sig_parts, sep = "\r"))
    if (length(out) != n_rows) {
        stop("Safe strata signature construction did not return one value per row.", call. = FALSE)
    }
    out
}

evaluate_mi_cv_safe_strata <- function(
    formula,
    imputed_list,
    time_col,
    event_col,
    cv_strata_col = "plan_type_strata",
    k = 5,
    seed = 2125,
    n_eval_times = 100,
    n_repeats = 20,
    cpus = 16,
    id_col = NULL,
    verbose = FALSE,
    log_every = 100L
) {
    horizons_month <- c(12, 36, 60)

    is_pos_int <- function(x) {
        is.numeric(x) && length(x) == 1L && !is.na(x) && x > 0 && x == as.integer(x)
    }

    normalize_time <- function(x, varname) {
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

    t_start <- Sys.time()

    if (!inherits(formula, "formula")) {
        formula <- stats::as.formula(formula, env = parent.frame())
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
    if (!is.character(cv_strata_col) || length(cv_strata_col) != 1L) {
        stop("cv_strata_col must be a single column name.")
    }
    if (!is_pos_int(k) || as.integer(k) < 2L) {
        stop("k must be an integer >= 2.")
    }
    if (!is_pos_int(n_repeats)) {
        stop("n_repeats must be a positive integer.")
    }
    if (!is_pos_int(n_eval_times) || as.integer(n_eval_times) < 2L) {
        stop("n_eval_times must be an integer >= 2.")
    }
    if (!is_pos_int(cpus)) {
        stop("cpus must be a positive integer.")
    }

    k <- as.integer(k)
    n_repeats <- as.integer(n_repeats)
    n_eval_times <- as.integer(n_eval_times)
    cpus <- as.integer(cpus)
    log_every <- as.integer(log_every)
    if (!is.finite(log_every) || log_every < 1L) {
        log_every <- 100L
    }

    df_list_raw <- lapply(imputed_list, as.data.frame)
    M <- length(df_list_raw)
    n_by_imp <- vapply(df_list_raw, nrow, integer(1))
    if (length(unique(n_by_imp)) != 1L) {
        stop("All imputations must have same number of rows.")
    }
    n <- n_by_imp[1L]
    if (k > n) {
        stop("k cannot exceed the number of rows.")
    }

    rhs_expr <- formula[[length(formula)]]
    rhs_vars <- unique(all.vars(rhs_expr))
    required_cols <- unique(c(rhs_vars, time_col, event_col, cv_strata_col))

    missing_by_imp <- lapply(df_list_raw, function(df) setdiff(required_cols, names(df)))
    bad <- which(lengths(missing_by_imp) > 0L)
    if (length(bad)) {
        detail <- paste(
            vapply(
                bad,
                function(i) {
                    sprintf("imputation %d missing: %s", i, paste(missing_by_imp[[i]], collapse = ", "))
                },
                character(1)
            ),
            collapse = "\n"
        )
        stop(
            sprintf(
                "Input schema mismatch.\nRequired vars: %s\n%s",
                paste(required_cols, collapse = ", "),
                detail
            ),
            call. = FALSE
        )
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

    df_list <- lapply(df_list_raw, function(d) {
        d <- as.data.frame(d)
        d[[time_col]] <- normalize_time(d[[time_col]], time_col)
        d[[event_col]] <- to01(d[[event_col]])
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

    model_formula <- .mi_cv_rewrite_strata_calls(formula)
    class(model_formula) <- class(formula)
    environment(model_formula) <- environment(formula)

    strata_term_labels <- .mi_cv_collect_strata_term_labels(rhs_expr)
    explicit_factor_vars <- .mi_cv_collect_explicit_factor_vars(rhs_expr)
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
    categorical_formula_vars <- unique(c(explicit_factor_vars, character_factor_vars))

    if (length(categorical_formula_vars) > 0L) {
        mismatches <- character(0)
        for (var in categorical_formula_vars) {
            level_sets <- lapply(df_list_raw, function(d) .mi_cv_collect_observed_levels(d[[var]]))
            ref_levels <- level_sets[[1L]]
            same_levels <- vapply(level_sets[-1L], identical, logical(1), ref_levels)
            if (!all(same_levels)) {
                mismatches <- c(mismatches, paste0(var, " -> ", .mi_cv_format_level_sets(level_sets)))
            }
        }
        if (length(mismatches) > 0L) {
            stop(
                paste0(
                    "Categorical formula variables have inconsistent observed levels across imputations. ",
                    paste(mismatches, collapse = " | ")
                ),
                call. = FALSE
            )
        }
    }

    tau <- as.numeric(stats::quantile(df_list[[1L]][[time_col]], probs = 0.90, na.rm = TRUE))
    if (!is.numeric(tau) || length(tau) != 1L || !is.finite(tau) || tau <= 0) {
        stop("tau must resolve to a finite positive scalar.")
    }

    c_tau <- tau * 0.95
    t0_min <- min(1e-6, tau / 1000)
    eval_times <- seq(t0_min, tau, length.out = n_eval_times)
    pred_times <- sort(unique(c(eval_times, horizons_month)))
    eval_idx <- match(eval_times, pred_times)

    preflight_split <- .mi_cv_prepare_model_split(df_list[[1L]], df_list[[1L]], categorical_formula_vars)
    if (nzchar(preflight_split$error)) {
        stop("Preflight categorical preparation failed: ", preflight_split$error, call. = FALSE)
    }

    if (length(strata_term_labels) > 0L) {
        # Avoid evaluating outer strata(); build one deterministic key per row instead.
        preflight_strata <- .mi_cv_make_strata_signature_safe(
            data = preflight_split$train,
            strata_term_labels = strata_term_labels,
            formula_env = environment(model_formula)
        )
        if (length(preflight_strata) != nrow(preflight_split$train)) {
            stop(
                "Preflight safe strata signature failed for terms: ",
                paste(strata_term_labels, collapse = ", "),
                call. = FALSE
            )
        }
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
                times = pred_times[1:2]
            )
            invisible(tmp)
            NULL
        },
        error = function(e) conditionMessage(e)
    )
    if (!is.null(preflight_error)) {
        stop("Preflight coxph/predictSurvProb failed: ", preflight_error, call. = FALSE)
    }

    total_models <- M * k * n_repeats
    cat(sprintf(
        "Starting Evaluation: %d Imputations x %d Folds x %d Repeats = %d Total Models\n",
        M, k, n_repeats, total_models
    ))
    cat("------------------------------------------------------------\n")
    cat(sprintf("Global evaluation tau: %.2f months | C-tau: %.2f months\n", tau, c_tau))

    cv_strata <- interaction(
        df_list[[1L]][[event_col]],
        df_list[[1L]][[cv_strata_col]],
        drop = TRUE,
        lex.order = TRUE
    )

    seeds <- seed + seq_len(n_repeats) - 1L
    folds_by_repeat <- lapply(seq_len(n_repeats), function(rep_id) {
        set.seed(seeds[rep_id])
        caret::createFolds(cv_strata, k = k, list = TRUE, returnTrain = FALSE)
    })

    fold_sizes_by_repeat <- lapply(
        folds_by_repeat,
        function(folds) vapply(folds, length, integer(1))
    )

    folds_first <- folds_by_repeat[[1L]]
    fold_id_first <- integer(length(cv_strata))
    for (ii in seq_along(folds_first)) {
        fold_id_first[folds_first[[ii]]] <- ii
    }

    cv_strata_table <- as.data.frame(table(cv_strata), stringsAsFactors = FALSE)
    names(cv_strata_table) <- c("cv_strata", "n")

    cv_strata_by_fold <- as.data.frame(
        table(fold = fold_id_first, cv_strata = cv_strata),
        stringsAsFactors = FALSE
    )

    c_array <- array(NA_real_, dim = c(n_repeats, M, k))
    unoC_array <- array(NA_real_, dim = c(n_repeats, M, k))
    ibs_array <- array(NA_real_, dim = c(n_repeats, M, k))
    ibs_ipcw_array <- array(NA_real_, dim = c(n_repeats, M, k))
    c_horizon <- array(
        NA_real_,
        dim = c(n_repeats, M, k, length(horizons_month)),
        dimnames = list(NULL, NULL, NULL, paste0("t", horizons_month))
    )

    task_grid <- expand.grid(
        r = seq_len(n_repeats),
        m = seq_len(M),
        i = seq_len(k),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )

    detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
    if (!is.numeric(detected) || length(detected) != 1L || !is.finite(detected) || detected < 1L) {
        detected <- 1L
    }
    n_cores <- as.integer(max(1L, min(cpus, nrow(task_grid), max(1L, detected - 1L))))

    cat(sprintf("Booting up %d CPU cores using future backend...\n", n_cores))

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    if (n_cores > 1L) {
        future::plan(future::multisession, workers = n_cores)
    } else {
        future::plan(future::sequential)
    }

    all_results <- future.apply::future_lapply(
        X = seq_len(nrow(task_grid)),
        FUN = function(row_id) {
            rep_id <- task_grid$r[row_id]
            imp_id <- task_grid$m[row_id]
            fold_id <- task_grid$i[row_id]

            tryCatch(
                {
                    test_idx <- folds_by_repeat[[rep_id]][[fold_id]]
                    df_train <- df_list[[imp_id]][-test_idx, , drop = FALSE]
                    df_test <- df_list[[imp_id]][test_idx, , drop = FALSE]

                    prepared <- .mi_cv_prepare_model_split(df_train, df_test, categorical_formula_vars)
                    if (nzchar(prepared$error)) {
                        return(list(
                            r = rep_id,
                            m = imp_id,
                            i = fold_id,
                            c_uno = NA_real_,
                            unoC = NA_real_,
                            ibs = NA_real_,
                            ibs_ipcw = NA_real_,
                            c_horizons = rep(NA_real_, length(horizons_month)),
                            fail = prepared$error
                        ))
                    }
                    df_train <- prepared$train
                    df_test <- prepared$test

                    if (length(strata_term_labels) > 0L) {
                        train_strata <- .mi_cv_make_strata_signature_safe(
                            df_train,
                            strata_term_labels,
                            environment(model_formula)
                        )
                        test_strata <- .mi_cv_make_strata_signature_safe(
                            df_test,
                            strata_term_labels,
                            environment(model_formula)
                        )
                        train_keys <- unique(train_strata)
                        keep <- test_strata %in% train_keys
                        dropped <- sum(!keep)

                        if (!any(keep)) {
                            return(list(
                                r = rep_id,
                                m = imp_id,
                                i = fold_id,
                                c_uno = NA_real_,
                                unoC = NA_real_,
                                ibs = NA_real_,
                                ibs_ipcw = NA_real_,
                                c_horizons = rep(NA_real_, length(horizons_month)),
                                fail = "all_test_rows_unseen_strata"
                            ))
                        }

                        if (dropped > 0L) {
                            # Drop test rows whose strata combination never appears in training.
                            df_test <- df_test[keep, , drop = FALSE]
                        }

                        if (verbose && dropped > 0L && (row_id == 1L || row_id %% log_every == 0L)) {
                            message(sprintf(
                                "Rep %d Imp %d Fold %d | filtered %d rows with unseen strata combinations",
                                rep_id, imp_id, fold_id, dropped
                            ))
                        }
                    }

                    if (nrow(df_test) < 10L) {
                        return(list(
                            r = rep_id,
                            m = imp_id,
                            i = fold_id,
                            c_uno = NA_real_,
                            unoC = NA_real_,
                            ibs = NA_real_,
                            ibs_ipcw = NA_real_,
                            c_horizons = rep(NA_real_, length(horizons_month)),
                            fail = "too_few_test_rows_after_filter"
                        ))
                    }

                    cv_model <- survival::coxph(
                        model_formula,
                        data = df_train,
                        x = TRUE,
                        y = TRUE,
                        model = TRUE
                    )

                    surv_mat_all <- pec::predictSurvProb(cv_model, newdata = df_test, times = pred_times)
                    if (is.null(dim(surv_mat_all))) {
                        surv_mat_all <- matrix(
                            surv_mat_all,
                            nrow = nrow(df_test),
                            ncol = length(pred_times),
                            byrow = TRUE
                        )
                    }

                    if (nrow(surv_mat_all) != nrow(df_test) || ncol(surv_mat_all) != length(pred_times)) {
                        stop("Survival matrix dimensions do not match test data.")
                    }

                    surv_mat_eval <- surv_mat_all[, eval_idx, drop = FALSE]
                    if (any(!is.finite(surv_mat_eval))) {
                        stop("Non-finite survival predictions were produced.")
                    }

                    intervals <- diff(eval_times)
                    rmst_score <- as.numeric(
                        surv_mat_eval[, -ncol(surv_mat_eval), drop = FALSE] %*% intervals
                    )
                    risk_score <- -rmst_score

                    df_test$rmst_score <- rmst_score
                    df_test$risk_score <- risk_score

                    ev_train <- df_train[[event_col]]
                    ev_test <- df_test[[event_col]]

                    ok_c <- is.finite(risk_score) &
                        is.finite(df_test[[time_col]]) &
                        is.finite(ev_test)

                    c_uno_val <- if (sum(ok_c) >= 2L) {
                        tryCatch(
                            survival::concordance(
                                survival::Surv(df_test[[time_col]][ok_c], ev_test[ok_c]) ~ risk_score[ok_c],
                                timewt = "n/G2",
                                ymax = c_tau,
                                reverse = TRUE
                            )$concordance,
                            error = function(e) NA_real_
                        )
                    } else {
                        NA_real_
                    }

                    y_true_test <- survival::Surv(df_test[[time_col]], ev_test)

                    ibs_val <- tryCatch(
                        survex::integrated_brier_score(
                            y_true = y_true_test,
                            surv = surv_mat_eval,
                            times = eval_times
                        ),
                        error = function(e) NA_real_
                    )

                    ibs_ipcw_val <- tryCatch(
                        ibs_ipcw_train(
                            df_train = df_train,
                            df_test = df_test,
                            surv_mat = surv_mat_eval,
                            times = eval_times,
                            time_col = time_col,
                            event_col = event_col
                        ),
                        error = function(e) NA_real_
                    )

                    ok_uno <- ok_c & complete.cases(surv_mat_eval)

                    sf_cens <- survival::survfit(
                        survival::Surv(df_train[[time_col]], 1 - ev_train) ~ 1
                    )
                    g_ctau <- tryCatch(
                        summary(sf_cens, times = c_tau, extend = TRUE)$surv,
                        error = function(e) NA_real_
                    )

                    tau_uno <- c_tau
                    if (!is.finite(g_ctau) || g_ctau <= 1e-6) {
                        tt <- sf_cens$time[sf_cens$surv > 1e-6]
                        tau_uno <- if (length(tt)) min(c_tau, max(tt)) else NA_real_
                    }

                    unoC_val <- NA_real_
                    n_evt_tau <- NA_integer_

                    if (any(ok_uno) && is.finite(tau_uno) && tau_uno > 0) {
                        n_evt_tau <- sum(
                            ev_test[ok_uno] == 1L & df_test[[time_col]][ok_uno] <= tau_uno,
                            na.rm = TRUE
                        )

                        if (verbose && n_evt_tau < 50L) {
                            message(sprintf(
                                "Rep %d Imp %d Fold %d | UnoC computed with only %d events <= tau_uno",
                                rep_id, imp_id, fold_id, n_evt_tau
                            ))
                        }

                        lp_u <- rank(risk_score[ok_uno], ties.method = "average")

                        unoC_val <- tryCatch(
                            withCallingHandlers(
                                survAUC::UnoC(
                                    survival::Surv(df_train[[time_col]], ev_train),
                                    survival::Surv(df_test[[time_col]][ok_uno], ev_test[ok_uno]),
                                    lp_u,
                                    time = tau_uno
                                ),
                                warning = function(w) {
                                    if (verbose) {
                                        message(sprintf(
                                            "Rep %d Imp %d Fold %d | UnoC warning: %s",
                                            rep_id, imp_id, fold_id, conditionMessage(w)
                                        ))
                                    }
                                    invokeRestart("muffleWarning")
                                }
                            ),
                            error = function(e) NA_real_
                        )
                    }

                    if (verbose && (row_id == 1L || row_id %% log_every == 0L)) {
                        surv_f <- surv_mat_eval[is.finite(surv_mat_eval)]
                        surv_min <- if (length(surv_f)) min(surv_f) else NA_real_
                        surv_max <- if (length(surv_f)) max(surv_f) else NA_real_
                        rmst_min <- .mi_cv_mean_or_na(c(min(df_test$rmst_score, na.rm = TRUE)))
                        rmst_max <- .mi_cv_mean_or_na(c(max(df_test$rmst_score, na.rm = TRUE)))

                        message(sprintf(
                            "Rep %d Imp %d Fold %d | surv=[%.4f, %.4f] | RMST=[%.2f, %.2f] | G(c_tau)=%.3g | tau_uno=%.3f | UnoC=%s",
                            rep_id, imp_id, fold_id,
                            surv_min, surv_max,
                            rmst_min, rmst_max,
                            g_ctau, tau_uno, format(unoC_val, digits = 6)
                        ))
                    }

                    c_horizons_vec <- rep(NA_real_, length(horizons_month))
                    for (h in seq_along(horizons_month)) {
                        t_h <- horizons_month[h]
                        idx_h <- which(pred_times <= t_h)
                        if (length(idx_h) < 2L) {
                            next
                        }

                        rmst_t0 <- as.numeric(
                            surv_mat_all[, idx_h[-length(idx_h)], drop = FALSE] %*% diff(pred_times[idx_h])
                        )
                        risk_t0 <- -rmst_t0

                        ok_h <- is.finite(risk_t0) &
                            is.finite(df_test[[time_col]]) &
                            is.finite(ev_test)

                        c_horizons_vec[h] <- if (sum(ok_h) >= 2L) {
                            tryCatch(
                                survival::concordance(
                                    survival::Surv(df_test[[time_col]][ok_h], ev_test[ok_h]) ~ risk_t0[ok_h],
                                    timewt = "n/G2",
                                    ymax = t_h,
                                    reverse = TRUE
                                )$concordance,
                                error = function(e) NA_real_
                            )
                        } else {
                            NA_real_
                        }
                    }

                    list(
                        r = rep_id,
                        m = imp_id,
                        i = fold_id,
                        c_uno = c_uno_val,
                        unoC = unoC_val,
                        ibs = ibs_val,
                        ibs_ipcw = ibs_ipcw_val,
                        c_horizons = c_horizons_vec,
                        fail = ""
                    )
                },
                error = function(e) {
                    list(
                        r = rep_id,
                        m = imp_id,
                        i = fold_id,
                        c_uno = NA_real_,
                        unoC = NA_real_,
                        ibs = NA_real_,
                        ibs_ipcw = NA_real_,
                        c_horizons = rep(NA_real_, length(horizons_month)),
                        fail = paste0("fold_error:", conditionMessage(e))
                    )
                }
            )
        },
        future.seed = TRUE,
        future.packages = c("survival", "pec", "survex", "survAUC")
    )

    for (res in all_results) {
        c_array[res$r, res$m, res$i] <- res$c_uno
        unoC_array[res$r, res$m, res$i] <- res$unoC
        ibs_array[res$r, res$m, res$i] <- res$ibs
        ibs_ipcw_array[res$r, res$m, res$i] <- res$ibs_ipcw
        c_horizon[res$r, res$m, res$i, ] <- res$c_horizons
    }

    task_failures <- do.call(rbind, lapply(all_results, function(res) {
        data.frame(
            repetition = res$r,
            imputation = res$m,
            fold = res$i,
            error = res$fail,
            stringsAsFactors = FALSE
        )
    }))
    task_failures <- task_failures[nzchar(task_failures$error), , drop = FALSE]

    rep_mean <- function(arr3) {
        vapply(seq_len(dim(arr3)[1L]), function(rep_id) {
            .mi_cv_mean_or_na(as.numeric(arr3[rep_id, , ]))
        }, numeric(1))
    }

    c_rep <- rep_mean(c_array)
    unoC_rep <- rep_mean(unoC_array)
    ibs_rep <- rep_mean(ibs_array)
    ibs_ipcw_rep <- rep_mean(ibs_ipcw_array)

    ci95_c <- .mi_cv_quantile_or_na(c_rep, c(0.025, 0.975))
    ci95_unoC <- .mi_cv_quantile_or_na(unoC_rep, c(0.025, 0.975))
    ci95_ibs <- .mi_cv_quantile_or_na(ibs_rep, c(0.025, 0.975))
    ci95_ibs_ipcw <- .mi_cv_quantile_or_na(ibs_ipcw_rep, c(0.025, 0.975))

    final_pooled_c <- .mi_cv_mean_or_na(c_rep)
    final_pooled_unoC <- .mi_cv_mean_or_na(unoC_rep)
    final_pooled_ibs <- .mi_cv_mean_or_na(ibs_rep)
    final_pooled_ibs_ipcw <- .mi_cv_mean_or_na(ibs_ipcw_rep)

    h_rep <- vapply(seq_len(length(horizons_month)), function(h) {
        vapply(seq_len(n_repeats), function(rep_id) {
            .mi_cv_mean_or_na(as.numeric(c_horizon[rep_id, , , h]))
        }, numeric(1))
    }, numeric(n_repeats))
    if (n_repeats == 1L) {
        h_rep <- matrix(h_rep, nrow = 1L)
    }

    rmst_h_summary <- data.frame(
        horizon_month = horizons_month,
        c_mean = vapply(seq_len(length(horizons_month)), function(h) .mi_cv_mean_or_na(h_rep[, h]), numeric(1)),
        ci95_lower = vapply(seq_len(length(horizons_month)), function(h) .mi_cv_quantile_or_na(h_rep[, h], 0.025), numeric(1)),
        ci95_upper = vapply(seq_len(length(horizons_month)), function(h) .mi_cv_quantile_or_na(h_rep[, h], 0.975), numeric(1)),
        valid_repeats = vapply(seq_len(length(horizons_month)), function(h) sum(is.finite(h_rep[, h])), integer(1)),
        stringsAsFactors = FALSE
    )

    cat("\n============================================================\n")
    cat("FINAL POOLED RESULTS (Cross-Validated + Multiply Imputed)\n")
    cat("============================================================\n")
    cat(sprintf("Global Evaluation Time (tau): %.2f months\n", tau))
    cat(sprintf(
        "Pooled Uno's C-Index: %.4f (repeat-quantile interval %.4f-%.4f)\n",
        final_pooled_c, ci95_c[1L], ci95_c[2L]
    ))
    cat(sprintf(
        "Pooled UnoC (survAUC::UnoC): %.4f (repeat-quantile interval %.4f-%.4f)\n",
        final_pooled_unoC, ci95_unoC[1L], ci95_unoC[2L]
    ))
    cat(sprintf(
        "Pooled IBS: %.4f (repeat-quantile interval %.4f-%.4f)\n",
        final_pooled_ibs, ci95_ibs[1L], ci95_ibs[2L]
    ))
    cat(sprintf(
        "Pooled IBS (IPCW-train): %.4f (repeat-quantile interval %.4f-%.4f)\n",
        final_pooled_ibs_ipcw, ci95_ibs_ipcw[1L], ci95_ibs_ipcw[2L]
    ))
    cat("RMST-based Uno C-index by horizon:\n")
    for (hh in seq_len(nrow(rmst_h_summary))) {
        cat(sprintf(
            "  t=%d m: C=%.4f (repeat-quantile interval %.4f-%.4f, valid repeats=%d)\n",
            rmst_h_summary$horizon_month[hh],
            rmst_h_summary$c_mean[hh],
            rmst_h_summary$ci95_lower[hh],
            rmst_h_summary$ci95_upper[hh],
            rmst_h_summary$valid_repeats[hh]
        ))
    }
    cat("============================================================\n")

    si <- utils::sessionInfo()
    other_pkgs <- if (is.null(si$otherPkgs)) character(0) else names(si$otherPkgs)

    t_end <- Sys.time()
    elapsed_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))
    cat(sprintf("\nTotal runtime: %.2f sec (%.2f min)\n", elapsed_sec, elapsed_sec / 60))

    invisible(list(
        raw_c_array = c_array,
        raw_unoC_array = unoC_array,
        raw_ibs_array = ibs_array,
        raw_ibs_ipcw_array = ibs_ipcw_array,
        ci95_pooled_c = unname(ci95_c),
        ci95_pooled_unoC = unname(ci95_unoC),
        ci95_pooled_ibs = unname(ci95_ibs),
        ci95_pooled_ibs_ipcw = unname(ci95_ibs_ipcw),
        repeat_values = list(
            c = c_rep,
            unoC = unoC_rep,
            ibs = ibs_rep,
            ibs_ipcw = ibs_ipcw_rep
        ),
        raw_failures = task_failures,
        reproducibility = list(
            call = match.call(),
            seed = seed,
            repeat_seeds = seeds,
            k = k,
            M = M,
            n_repeats = n_repeats,
            time_col = time_col,
            event_col = event_col,
            formula = paste(deparse(formula, width.cutoff = 500L), collapse = " "),
            tau = tau,
            c_tau = c_tau,
            n_eval_times = n_eval_times,
            eval_times = eval_times,
            prediction_times = pred_times,
            fold_sizes = fold_sizes_by_repeat[[1L]],
            fold_sizes_by_repeat = fold_sizes_by_repeat,
            folds_by_repeat = folds_by_repeat,
            rmst_unoC_by_horizon = c_horizon,
            rmst_unoC_summary = rmst_h_summary,
            cv_strata_table = cv_strata_table,
            cv_strata_by_fold = cv_strata_by_fold,
            categorical_formula_vars = categorical_formula_vars,
            strata_terms = strata_term_labels,
            timestamp = as.character(Sys.time()),
            R_version = R.version.string,
            attached_packages = other_pkgs
        )
    ))
}
