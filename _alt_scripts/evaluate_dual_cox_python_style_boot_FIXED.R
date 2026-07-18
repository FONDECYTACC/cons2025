# Standalone evaluator for Python-style dual Cox IPCW CV on boot-spline MI data.
#
# Usage:
# source("cons/_hist_scripts/evaluate_dual_cox_python_style_boot.R")
# results_boot <- evaluate_dual_cox_python_style(
#   formula_readmit = formula_readmit_int_spline,
#   formula_death = formula_death_int_spline,
#   imputed_list = py_corrected_datasets_boot
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
        stop("event column has no non-missing values.")
    }
    if (all(vals %in% c(0, 1))) {
        return(as.integer(x_num))
    }
    if (all(vals %in% c(1, 2))) {
        return(as.integer(x_num - 1L))
    }

    stop("event column must be binary 0/1 (or 1/2).")
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
    train_time_max <- max(df_train[[time_col]], na.rm = TRUE)

    keep <- times <= tau_ipcw & times <= train_time_max
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

.dual_cox_normalize_call_name <- function(fun_expr) {
    txt <- paste(deparse(fun_expr, width.cutoff = 500L), collapse = "")
    txt <- gsub("`", "", txt, fixed = TRUE)
    sub("^.*::+", "", txt)
}

.dual_cox_rewrite_strata_calls <- function(expr) {
    if (!is.call(expr)) {
        return(expr)
    }
    node_name <- .dual_cox_normalize_call_name(expr[[1L]])
    if (node_name %in% c("strat", "strata")) {
        expr[[1L]] <- str2lang("survival::strata")
    }
    for (i in seq_along(expr)) {
        expr[[i]] <- .dual_cox_rewrite_strata_calls(expr[[i]])
    }
    expr
}

.dual_cox_collect_explicit_factor_vars <- function(expr) {
    vars <- character(0)
    walk <- function(node) {
        if (!is.call(node)) {
            return(invisible(NULL))
        }
        node_name <- .dual_cox_normalize_call_name(node[[1L]])
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

.dual_cox_collect_observed_levels <- function(x) {
    raw <- as.character(x)
    sort(unique(raw[!is.na(x)]), method = "radix")
}

.dual_cox_format_level_sets <- function(level_sets) {
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

.dual_cox_sanitize_level_name <- function(var, level) {
    paste0(var, "__", make.names(as.character(level)))
}

.dual_cox_build_level_mapping <- function(var, raw_levels) {
    sanitized <- vapply(
        raw_levels,
        function(level) .dual_cox_sanitize_level_name(var, level),
        character(1)
    )
    sanitized <- make.unique(sanitized, sep = "__dup")
    stats::setNames(sanitized, raw_levels)
}

.dual_cox_prepare_model_split <- function(train, test, categorical_formula_vars) {
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
        train_levels <- .dual_cox_collect_observed_levels(train_raw)

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

        mapping <- .dual_cox_build_level_mapping(var, train_levels)

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

.dual_cox_expr_label <- function(expr) {
    paste(deparse(expr, width.cutoff = 500L), collapse = "")
}

.dual_cox_collect_strata_term_labels <- function(expr) {
    labels <- character(0)

    walk <- function(node) {
        if (!is.call(node)) {
            return(invisible(NULL))
        }
        node_name <- .dual_cox_normalize_call_name(node[[1L]])
        if (node_name %in% c("strat", "strata")) {
            labels <<- c(labels, .dual_cox_expr_label(node))
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

.dual_cox_extract_strata_args <- function(term_expr, term_label) {
    expr <- .dual_cox_rewrite_strata_calls(term_expr)
    if (!is.call(expr) || .dual_cox_normalize_call_name(expr[[1L]]) != "strata") {
        stop("Invalid strata term label: ", term_label, call. = FALSE)
    }
    if (length(expr) < 2L) {
        stop("Strata term has no arguments: ", term_label, call. = FALSE)
    }
    as.list(expr)[-1L]
}

.dual_cox_coerce_rowwise_value <- function(value, n_rows, term_label, arg_index) {
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

.dual_cox_make_strata_signature_safe <- function(
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
        arg_exprs <- .dual_cox_extract_strata_args(term_expr, term_label)

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

            out <- .dual_cox_coerce_rowwise_value(value, n_rows, term_label, arg_index)
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

.dual_cox_prepare_formula_metadata <- function(formula, df_list_raw) {
    if (!inherits(formula, "formula")) {
        formula <- stats::as.formula(formula, env = parent.frame())
    }

    rhs_expr <- formula[[length(formula)]]
    rhs_vars <- unique(all.vars(rhs_expr))

    ## --- FIX 2026-07 (strata special detection) --------------------------------
    ## coxph()/terms() detect strata() as a stratification *special by bare symbol
    ## name only*. The previous version rewrote strata(...) -> survival::strata(...),
    ## which terms() does NOT recognise as the special: plan_type_strata and
    ## tr_outcome_adm_discharge_rule_violation_undet were then silently fitted as
    ## factor COVARIATES, so the model lost its stratified baseline hazards and
    ## gained spurious coefficients (readmission 26 -> 31; 10 strata -> 1 baseline).
    ## Keep BARE strata() so the special is detected, and bind bare strata()/Surv()
    ## in the model-formula environment so the fit still resolves when the survival
    ## package is not attached (same device already used by the _coherent concordance).
    model_formula <- formula
    class(model_formula) <- class(formula)
    .f_parent <- environment(formula)
    if (is.null(.f_parent)) .f_parent <- globalenv()
    .f_env <- new.env(parent = .f_parent)
    .f_env$strata <- survival::strata
    .f_env$Surv   <- survival::Surv
    environment(model_formula) <- .f_env
    ## --- end FIX ---------------------------------------------------------------

    strata_term_labels <- .dual_cox_collect_strata_term_labels(rhs_expr)
    explicit_factor_vars <- .dual_cox_collect_explicit_factor_vars(rhs_expr)
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
            level_sets <- lapply(df_list_raw, function(d) .dual_cox_collect_observed_levels(d[[var]]))
            ref_levels <- level_sets[[1L]]
            same_levels <- vapply(level_sets[-1L], identical, logical(1), ref_levels)
            if (!all(same_levels)) {
                mismatches <- c(mismatches, paste0(var, " -> ", .dual_cox_format_level_sets(level_sets)))
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

    list(
        original_formula = formula,
        model_formula = model_formula,
        rhs_vars = rhs_vars,
        categorical_formula_vars = categorical_formula_vars,
        strata_term_labels = strata_term_labels,
        formula_text = paste(deparse(formula, width.cutoff = 500L), collapse = " ")
    )
}

.dual_cox_matrixify <- function(x, nrow_expected, ncol_expected) {
    out <- x
    if (is.null(dim(out))) {
        out <- matrix(out, nrow = nrow_expected, ncol = ncol_expected, byrow = TRUE)
    } else {
        out <- as.matrix(out)
    }
    if (!identical(dim(out), c(nrow_expected, ncol_expected))) {
        stop("Prediction matrix dimensions do not match expectations.", call. = FALSE)
    }
    out
}

.dual_cox_make_unique_path <- function(path) {
    if (!file.exists(path)) {
        return(path)
    }

    ext <- tools::file_ext(path)
    stem <- if (nzchar(ext)) {
        substr(path, 1L, nchar(path) - nchar(ext) - 1L)
    } else {
        path
    }

    idx <- 1L
    repeat {
        candidate <- if (nzchar(ext)) {
            sprintf("%s_%d.%s", stem, idx, ext)
        } else {
            sprintf("%s_%d", stem, idx)
        }
        if (!file.exists(candidate)) {
            return(candidate)
        }
        idx <- idx + 1L
    }
}

.dual_cox_metric_summary <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) {
        return(c(mean = NA_real_, sd = NA_real_, q025 = NA_real_, q975 = NA_real_, n = 0))
    }

    c(
        mean = mean(x),
        sd = stats::sd(x),
        q025 = stats::quantile(x, probs = 0.025, na.rm = TRUE, names = FALSE),
        q975 = stats::quantile(x, probs = 0.975, na.rm = TRUE, names = FALSE),
        n = length(x)
    )
}

.dual_cox_summarize_metrics <- function(metrics_df) {
    if (!nrow(metrics_df)) {
        return(data.frame(
            Risk = character(0),
            Metric = character(0),
            Time = character(0),
            mean = numeric(0),
            sd = numeric(0),
            q025 = numeric(0),
            q975 = numeric(0),
            n = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    grouping <- split(
        metrics_df,
        list(metrics_df$Risk, metrics_df$Metric, metrics_df$Time),
        drop = TRUE
    )

    rows <- lapply(grouping, function(df) {
        stats_vec <- .dual_cox_metric_summary(df$Value)
        data.frame(
            Risk = df$Risk[1L],
            Metric = df$Metric[1L],
            Time = df$Time[1L],
            mean = unname(stats_vec["mean"]),
            sd = unname(stats_vec["sd"]),
            q025 = unname(stats_vec["q025"]),
            q975 = unname(stats_vec["q975"]),
            n = as.integer(unname(stats_vec["n"])),
            stringsAsFactors = FALSE
        )
    })

    out <- do.call(rbind, rows)
    time_num <- suppressWarnings(as.numeric(out$Time))
    out <- out[order(out$Risk, out$Metric, is.na(time_num), time_num, out$Time), , drop = FALSE]
    rownames(out) <- NULL
    out
}

.dual_cox_safe_concordance <- function(time, event, lp, ymax = NULL) {
    ok <- is.finite(time) & is.finite(event) & is.finite(lp)
    if (sum(ok) < 2L) {
        return(NA_real_)
    }

    surv_formula <- survival::Surv(time[ok], event[ok]) ~ lp[ok]

    tryCatch(
        {
            fit <- if (is.null(ymax)) {
                survival::concordance(
                    surv_formula,
                    timewt = "n/G2",
                    reverse = TRUE
                )
            } else {
                survival::concordance(
                    surv_formula,
                    timewt = "n/G2",
                    ymax = ymax,
                    reverse = TRUE
                )
            }
            fit$concordance
        },
        error = function(e) NA_real_
    )
}

.dual_cox_safe_concordance_coherent <- function(time, event, lp, ymax = NULL, strata = NULL) {
    ok <- is.finite(time) & is.finite(event) & is.finite(lp)
    if (!is.null(strata)) {
        ok <- ok & !is.na(strata)
    }
    if (sum(ok) < 2L) {
        return(NA_real_)
    }

    cdat <- data.frame(
        time_ = as.numeric(time[ok]),
        event_ = as.numeric(event[ok]),
        score_ = as.numeric(lp[ok])
    )

    if (is.null(strata)) {
        surv_formula <- survival::Surv(time_, event_) ~ score_
    } else {
        cdat$stratum_ <- strata[ok]
        ## NB: survival::concordance() only recognises a *bare* strata() term as a
        ## stratification special (its detection is name-based). A namespaced
        ## `survival::strata(...)` is silently treated as a second predictor, so
        ## $concordance comes back length-2 and the score is scored GLOBALLY, not
        ## within strata. We therefore build the formula in an env where bare
        ## strata/Surv resolve to the survival functions (works unattached).
        f_env <- new.env(parent = parent.frame())
        f_env$strata <- survival::strata
        f_env$Surv <- survival::Surv
        surv_formula <- stats::as.formula(
            "Surv(time_, event_) ~ score_ + strata(stratum_)",
            env = f_env
        )
    }

    tryCatch(
        {
            args <- list(
                object = surv_formula,
                data = cdat,
                timewt = "n/G2",
                reverse = TRUE
            )
            if (!is.null(ymax)) {
                args$ymax <- ymax
            }
            fit <- do.call(survival::concordance, args)
            cval <- fit$concordance
            ## Defensive: a correctly stratified fit returns a scalar pooled
            ## concordance; if any survival version returns per-stratum values,
            ## pool them from the (concordant/discordant) pair counts.
            if (length(cval) != 1L) {
                cnt <- fit$count
                if (is.matrix(cnt)) {
                    cnt <- colSums(cnt)
                }
                cval <- (cnt[["concordant"]] + 0.5 * cnt[["tied.x"]]) /
                    (cnt[["concordant"]] + cnt[["discordant"]] + cnt[["tied.x"]])
            }
            as.numeric(cval)
        },
        error = function(e) NA_real_
    )
}

.dual_cox_plan_idx <- function(df, plan_dummy_cols, plan_strata_fallback_col) {
    n <- nrow(df)
    out <- integer(n)

    code_map <- c(
        plan_type_corr_pg_pr = 1L,
        plan_type_corr_m_pr = 2L,
        plan_type_corr_pg_pai = 3L,
        plan_type_corr_m_pai = 4L
    )

    present_dummy_cols <- intersect(plan_dummy_cols, names(df))
    present_dummy_cols <- intersect(present_dummy_cols, names(code_map))
    if (length(present_dummy_cols) > 0L) {
        active_counts <- integer(n)
        for (col in present_dummy_cols) {
            values <- suppressWarnings(as.numeric(as.character(df[[col]])))
            active <- !is.na(values) & values == 1
            out[active] <- unname(code_map[col])
            active_counts <- active_counts + as.integer(active)
        }
        if (any(active_counts > 1L)) {
            stop("Invalid plan encoding: more than one plan dummy active in the same row.", call. = FALSE)
        }
        return(out)
    }

    if (!plan_strata_fallback_col %in% names(df)) {
        stop(
            "Neither plan dummy columns nor fallback column '",
            plan_strata_fallback_col,
            "' were found.",
            call. = FALSE
        )
    }

    raw <- as.character(df[[plan_strata_fallback_col]])
    known_map <- c(
        "pg-pab" = 0L,
        "pg-pr" = 1L,
        "m-pr" = 2L,
        "pg-pai" = 3L,
        "m-pai" = 4L
    )

    out[] <- 0L
    non_missing <- !is.na(raw)
    matched <- non_missing & raw %in% names(known_map)
    out[matched] <- unname(known_map[raw[matched]])

    extras <- sort(unique(raw[non_missing & !matched]), method = "radix")
    if (length(extras) > 0L) {
        extra_codes <- seq.int(from = max(known_map) + 1L, length.out = length(extras))
        extra_map <- stats::setNames(extra_codes, extras)
        extra_match <- non_missing & raw %in% names(extra_map)
        out[extra_match] <- unname(extra_map[raw[extra_match]])
    }

    out
}

.dual_cox_composite_strata <- function(
    df,
    readmit_time_col,
    readmit_event_col,
    death_time_col,
    death_event_col,
    plan_dummy_cols,
    plan_strata_fallback_col
) {
    t_d <- as.numeric(df[[death_time_col]])
    e_d <- to01(df[[death_event_col]]) == 1L
    t_r <- as.numeric(df[[readmit_time_col]])
    e_r <- to01(df[[readmit_event_col]]) == 1L

    events_cr <- integer(length(e_d))
    mask_death_first <- e_d & (!e_r | (t_d < t_r))
    events_cr[mask_death_first] <- 1L
    mask_readmit_first <- e_r & (!e_d | (t_r < t_d))
    events_cr[mask_readmit_first] <- 2L

    plan_idx <- .dual_cox_plan_idx(df, plan_dummy_cols, plan_strata_fallback_col)
    list(
        plan_idx = plan_idx,
        events_cr = events_cr,
        strat_label = events_cr * 10L + plan_idx
    )
}

.dual_cox_fold_assignments <- function(strat_labels, k_folds, seed) {
    set.seed(seed)
    folds <- caret::createFolds(
        y = factor(strat_labels),
        k = k_folds,
        list = TRUE,
        returnTrain = FALSE
    )

    fold_id <- integer(length(strat_labels))
    for (idx in seq_along(folds)) {
        fold_id[folds[[idx]]] <- idx
    }

    list(folds = folds, fold_id = fold_id)
}

.dual_cox_prepare_fold_data <- function(df_train_raw, df_test_raw, meta, min_test_rows) {
    prepared <- .dual_cox_prepare_model_split(
        train = df_train_raw,
        test = df_test_raw,
        categorical_formula_vars = meta$categorical_formula_vars
    )
    if (nzchar(prepared$error)) {
        return(list(train = prepared$train, test = prepared$test, error = prepared$error, dropped = 0L))
    }

    df_train <- prepared$train
    df_test <- prepared$test
    dropped <- 0L

    if (length(meta$strata_term_labels) > 0L) {
        train_strata <- .dual_cox_make_strata_signature_safe(
            data = df_train,
            strata_term_labels = meta$strata_term_labels,
            formula_env = environment(meta$model_formula)
        )
        test_strata <- .dual_cox_make_strata_signature_safe(
            data = df_test,
            strata_term_labels = meta$strata_term_labels,
            formula_env = environment(meta$model_formula)
        )
        train_keys <- unique(train_strata)
        keep <- test_strata %in% train_keys
        dropped <- sum(!keep)

        if (!any(keep)) {
            return(list(train = df_train, test = df_test, error = "all_test_rows_unseen_strata", dropped = dropped))
        }

        if (dropped > 0L) {
            df_test <- df_test[keep, , drop = FALSE]
        }
    }

    if (nrow(df_test) < min_test_rows) {
        return(list(
            train = df_train,
            test = df_test,
            error = "too_few_test_rows_after_filter",
            dropped = dropped
        ))
    }

    list(train = df_train, test = df_test, error = "", dropped = dropped)
}

.dual_cox_fit_one_risk <- function(
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

    time_train <- as.numeric(df_train[[time_col]])
    event_train <- to01(df_train[[event_col]])
    time_val <- as.numeric(df_test[[time_col]])
    event_val <- to01(df_test[[event_col]])
    train_time_max <- max(time_train, na.rm = TRUE)

    c_global <- .dual_cox_safe_concordance(
        time = time_val,
        event = event_val,
        lp = lp_val
    )
    ibs_global <- ibs_ipcw_train(
        df_train = df_train,
        df_test = df_test,
        surv_mat = surv_mat,
        times = eval_times,
        time_col = time_col,
        event_col = event_col
    )

    c_horizon <- rep(NA_real_, length(eval_times))
    ibs_horizon <- rep(NA_real_, length(eval_times))

    for (ii in seq_along(eval_times)) {
        t_h <- eval_times[ii]
        if (is.finite(train_time_max) && t_h < train_time_max) {
            c_horizon[ii] <- .dual_cox_safe_concordance(
                time = time_val,
                event = event_val,
                lp = lp_val,
                ymax = t_h
            )

            keep <- eval_times <= t_h
            if (sum(keep) >= 2L) {
                ibs_horizon[ii] <- ibs_ipcw_train(
                    df_train = df_train,
                    df_test = df_test,
                    surv_mat = surv_mat[, keep, drop = FALSE],
                    times = eval_times[keep],
                    time_col = time_col,
                    event_col = event_col
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
        c_global = c_global,
        ibs_global = ibs_global,
        c_horizon = stats::setNames(c_horizon, as.character(eval_times)),
        ibs_horizon = stats::setNames(ibs_horizon, as.character(eval_times))
    )
}

.dual_cox_fit_one_risk_coherent <- function(
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

    ## --- coherence patch: configurable concordance ranking score + optional within-strata ---
    ## driven by options(); defaults reproduce the ORIGINAL behaviour exactly.
    .dualcox_score_type <- getOption("dualcox.concordance_score", "lp")
    .dualcox_within_strata <- isTRUE(getOption("dualcox.concordance_within_strata", FALSE))
    .dualcox_risk_at <- function(col_idx) as.numeric(1 - surv_mat[, col_idx])
    ## global marker: eta (lp) or stratum-correct absolute risk at the largest eval_time
    .dualcox_global_score <- if (identical(.dualcox_score_type, "risk")) {
        .dualcox_risk_at(length(eval_times))
    } else {
        lp_val
    }
    .dualcox_val_strata <- NULL
    if (.dualcox_within_strata && length(meta$strata_term_labels) > 0L) {
        .dualcox_val_strata <- .dual_cox_make_strata_signature_safe(
            data = df_test,
            strata_term_labels = meta$strata_term_labels,
            formula_env = environment(meta$model_formula)
        )
    }
    ## --- end coherence patch ---

    time_train <- as.numeric(df_train[[time_col]])
    event_train <- to01(df_train[[event_col]])
    time_val <- as.numeric(df_test[[time_col]])
    event_val <- to01(df_test[[event_col]])
    train_time_max <- max(time_train, na.rm = TRUE)

    c_global <- .dual_cox_safe_concordance_coherent(
        time = time_val,
        event = event_val,
        lp = .dualcox_global_score,
        strata = .dualcox_val_strata
    )
    ibs_global <- ibs_ipcw_train(
        df_train = df_train,
        df_test = df_test,
        surv_mat = surv_mat,
        times = eval_times,
        time_col = time_col,
        event_col = event_col
    )

    c_horizon <- rep(NA_real_, length(eval_times))
    ibs_horizon <- rep(NA_real_, length(eval_times))

    for (ii in seq_along(eval_times)) {
        t_h <- eval_times[ii]
        if (is.finite(train_time_max) && t_h < train_time_max) {
            c_horizon[ii] <- .dual_cox_safe_concordance_coherent(
                time = time_val,
                event = event_val,
                lp = if (identical(.dualcox_score_type, "risk")) .dualcox_risk_at(ii) else lp_val,
                ymax = t_h,
                strata = .dualcox_val_strata
            )

            keep <- eval_times <= t_h
            if (sum(keep) >= 2L) {
                ibs_horizon[ii] <- ibs_ipcw_train(
                    df_train = df_train,
                    df_test = df_test,
                    surv_mat = surv_mat[, keep, drop = FALSE],
                    times = eval_times[keep],
                    time_col = time_col,
                    event_col = event_col
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
        c_global = c_global,
        ibs_global = ibs_global,
        c_horizon = stats::setNames(c_horizon, as.character(eval_times)),
        ibs_horizon = stats::setNames(ibs_horizon, as.character(eval_times))
    )
}

.dual_cox_preflight <- function(meta, df, eval_times) {
    prepared <- .dual_cox_prepare_model_split(
        train = df,
        test = df,
        categorical_formula_vars = meta$categorical_formula_vars
    )
    if (nzchar(prepared$error)) {
        stop("Preflight categorical preparation failed: ", prepared$error, call. = FALSE)
    }

    fit <- survival::coxph(
        meta$model_formula,
        data = prepared$train,
        ties = "efron",
        x = TRUE,
        y = TRUE,
        model = TRUE
    )
    tmp <- pec::predictSurvProb(
        fit,
        newdata = prepared$test[1L, , drop = FALSE],
        times = eval_times[seq_len(min(2L, length(eval_times)))]
    )
    invisible(tmp)
}

evaluate_dual_cox_python_style <- function(
    formula_readmit,
    formula_death,
    imputed_list,
    readmit_time_col = "readmit_time_from_disch_m",
    readmit_event_col = "readmit_event",
    death_time_col = "death_time_from_disch_m",
    death_event_col = "death_event",
    eval_times = c(3, 6, 9, 12, 24, 36, 48, 60, 72, 84, 96, 108),
    k_folds = 10L,
    seed = 2125L,
    plan_dummy_cols = c(
        "plan_type_corr_pg_pr",
        "plan_type_corr_m_pr",
        "plan_type_corr_pg_pai",
        "plan_type_corr_m_pai"
    ),
    plan_strata_fallback_col = "plan_type_strata",
    min_test_rows = 10L,
    save_prefix = NULL,
    output_dir = NULL,
    verbose = TRUE
) {
    if (!is.list(imputed_list) || length(imputed_list) == 0L) {
        stop("imputed_list must be a non-empty list.", call. = FALSE)
    }
    if (!is.numeric(eval_times) || any(!is.finite(eval_times)) || length(eval_times) < 2L) {
        stop("eval_times must contain at least two finite numeric values.", call. = FALSE)
    }
    if (!is.numeric(k_folds) || length(k_folds) != 1L || k_folds < 2L) {
        stop("k_folds must be an integer >= 2.", call. = FALSE)
    }
    if (!is.numeric(min_test_rows) || length(min_test_rows) != 1L || min_test_rows < 1L) {
        stop("min_test_rows must be a positive integer.", call. = FALSE)
    }

    eval_times <- sort(unique(as.numeric(eval_times)))
    k_folds <- as.integer(k_folds)
    min_test_rows <- as.integer(min_test_rows)
    seed <- as.integer(seed)

    df_list_raw <- lapply(imputed_list, as.data.frame)
    n_by_imp <- vapply(df_list_raw, nrow, integer(1))
    if (length(unique(n_by_imp)) != 1L) {
        stop("All imputations must have the same number of rows.", call. = FALSE)
    }
    if (k_folds > n_by_imp[1L]) {
        stop("k_folds cannot exceed the number of rows.", call. = FALSE)
    }

    meta_readmit <- .dual_cox_prepare_formula_metadata(formula_readmit, df_list_raw)
    meta_death <- .dual_cox_prepare_formula_metadata(formula_death, df_list_raw)

    required_cols <- unique(c(
        meta_readmit$rhs_vars,
        meta_death$rhs_vars,
        readmit_time_col,
        readmit_event_col,
        death_time_col,
        death_event_col
    ))

    missing_by_imp <- lapply(df_list_raw, function(df) setdiff(required_cols, names(df)))
    bad <- which(lengths(missing_by_imp) > 0L)
    if (length(bad) > 0L) {
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

    df_list <- lapply(df_list_raw, function(df) {
        df[[readmit_time_col]] <- as.numeric(df[[readmit_time_col]])
        df[[death_time_col]] <- as.numeric(df[[death_time_col]])
        df[[readmit_event_col]] <- to01(df[[readmit_event_col]])
        df[[death_event_col]] <- to01(df[[death_event_col]])

        if (any(!is.finite(df[[readmit_time_col]]) | df[[readmit_time_col]] < 0, na.rm = TRUE)) {
            stop("readmission time column contains non-finite or negative values.", call. = FALSE)
        }
        if (any(!is.finite(df[[death_time_col]]) | df[[death_time_col]] < 0, na.rm = TRUE)) {
            stop("death time column contains non-finite or negative values.", call. = FALSE)
        }
        df
    })

    ref_readmit_time <- df_list[[1L]][[readmit_time_col]]
    ref_readmit_event <- df_list[[1L]][[readmit_event_col]]
    ref_death_time <- df_list[[1L]][[death_time_col]]
    ref_death_event <- df_list[[1L]][[death_event_col]]

    same_outcomes <- vapply(df_list[-1L], function(df) {
        isTRUE(all.equal(df[[readmit_time_col]], ref_readmit_time, tolerance = 1e-12, check.attributes = FALSE)) &&
            identical(df[[readmit_event_col]], ref_readmit_event) &&
            isTRUE(all.equal(df[[death_time_col]], ref_death_time, tolerance = 1e-12, check.attributes = FALSE)) &&
            identical(df[[death_event_col]], ref_death_event)
    }, logical(1))
    if (length(same_outcomes) > 0L && !all(same_outcomes)) {
        stop(
            "Outcome/time information differs across imputations. This workflow assumes only predictors were imputed.",
            call. = FALSE
        )
    }

    .dual_cox_preflight(meta_readmit, df_list[[1L]], eval_times)
    .dual_cox_preflight(meta_death, df_list[[1L]], eval_times)

    ## --- coherence dispatch ---
    ## Default options (score = "lp", within_strata = FALSE) keep the ORIGINAL
    ## fold fitter, so published numbers are reproduced byte-for-byte. Any active
    ## coherence option routes to the _coherent fitter (configurable concordance).
    .dual_cox_fit_one_risk_dispatch <-
        if (isTRUE(getOption("dualcox.concordance_within_strata", FALSE)) ||
            !identical(getOption("dualcox.concordance_score", "lp"), "lp")) {
            .dual_cox_fit_one_risk_coherent
        } else {
            .dual_cox_fit_one_risk
        }

    metrics_log <- list()
    failures_log <- list()
    raw_predictions <- list()
    fold_assignments <- list()
    metric_id <- 0L
    failure_id <- 0L
    raw_id <- 0L

    n_imputations <- length(df_list)

    if (isTRUE(verbose)) {
        cat(sprintf(
            "Starting dual Cox evaluation: %d imputations x %d folds = %d fold pairs\n",
            n_imputations,
            k_folds,
            n_imputations * k_folds
        ))
    }

    for (imp_idx in seq_len(n_imputations)) {
        df_imp <- df_list[[imp_idx]]
        composite <- .dual_cox_composite_strata(
            df = df_imp,
            readmit_time_col = readmit_time_col,
            readmit_event_col = readmit_event_col,
            death_time_col = death_time_col,
            death_event_col = death_event_col,
            plan_dummy_cols = plan_dummy_cols,
            plan_strata_fallback_col = plan_strata_fallback_col
        )

        split_seed <- seed + imp_idx - 1L
        fold_info <- .dual_cox_fold_assignments(
            strat_labels = composite$strat_label,
            k_folds = k_folds,
            seed = split_seed
        )

        fold_assignments[[imp_idx]] <- data.frame(
            Imp = imp_idx,
            Row = seq_len(nrow(df_imp)),
            Fold = fold_info$fold_id,
            StratLabel = composite$strat_label,
            EventComposite = composite$events_cr,
            PlanIdx = composite$plan_idx,
            SplitSeed = split_seed,
            stringsAsFactors = FALSE
        )

        if (isTRUE(verbose)) {
            cat(sprintf("Imp %d/%d: ", imp_idx, n_imputations))
        }

        for (fold_idx in seq_along(fold_info$folds)) {
            val_idx <- fold_info$folds[[fold_idx]]
            train_idx <- setdiff(seq_len(nrow(df_imp)), val_idx)
            df_train_raw <- df_imp[train_idx, , drop = FALSE]
            df_test_raw <- df_imp[val_idx, , drop = FALSE]

            readmit_res <- tryCatch(
                .dual_cox_fit_one_risk_dispatch(
                    df_train_raw = df_train_raw,
                    df_test_raw = df_test_raw,
                    meta = meta_readmit,
                    time_col = readmit_time_col,
                    event_col = readmit_event_col,
                    eval_times = eval_times,
                    min_test_rows = min_test_rows
                ),
                error = function(e) list(error = paste0("fold_error:", conditionMessage(e)), dropped = 0L)
            )

            death_res <- tryCatch(
                .dual_cox_fit_one_risk_dispatch(
                    df_train_raw = df_train_raw,
                    df_test_raw = df_test_raw,
                    meta = meta_death,
                    time_col = death_time_col,
                    event_col = death_event_col,
                    eval_times = eval_times,
                    min_test_rows = min_test_rows
                ),
                error = function(e) list(error = paste0("fold_error:", conditionMessage(e)), dropped = 0L)
            )

            if (nzchar(readmit_res$error)) {
                failure_id <- failure_id + 1L
                failures_log[[failure_id]] <- data.frame(
                    Imp = imp_idx,
                    Fold = fold_idx,
                    Risk = "Readmission",
                    Error = readmit_res$error,
                    stringsAsFactors = FALSE
                )
            } else {
                metric_id <- metric_id + 1L
                metrics_log[[metric_id]] <- data.frame(
                    Imp = imp_idx,
                    Fold = fold_idx,
                    Risk = "Readmission",
                    Metric = "Uno's C-Index",
                    Time = "Global",
                    Value = readmit_res$c_global,
                    stringsAsFactors = FALSE
                )

                metric_id <- metric_id + 1L
                metrics_log[[metric_id]] <- data.frame(
                    Imp = imp_idx,
                    Fold = fold_idx,
                    Risk = "Readmission",
                    Metric = "IBS",
                    Time = "Global",
                    Value = readmit_res$ibs_global,
                    stringsAsFactors = FALSE
                )

                for (time_label in names(readmit_res$c_horizon)) {
                    metric_id <- metric_id + 1L
                    metrics_log[[metric_id]] <- data.frame(
                        Imp = imp_idx,
                        Fold = fold_idx,
                        Risk = "Readmission",
                        Metric = "Uno's C-Index",
                        Time = time_label,
                        Value = unname(readmit_res$c_horizon[[time_label]]),
                        stringsAsFactors = FALSE
                    )

                    metric_id <- metric_id + 1L
                    metrics_log[[metric_id]] <- data.frame(
                        Imp = imp_idx,
                        Fold = fold_idx,
                        Risk = "Readmission",
                        Metric = "IBS",
                        Time = time_label,
                        Value = unname(readmit_res$ibs_horizon[[time_label]]),
                        stringsAsFactors = FALSE
                    )
                }
            }

            if (nzchar(death_res$error)) {
                failure_id <- failure_id + 1L
                failures_log[[failure_id]] <- data.frame(
                    Imp = imp_idx,
                    Fold = fold_idx,
                    Risk = "Death",
                    Error = death_res$error,
                    stringsAsFactors = FALSE
                )
            } else {
                metric_id <- metric_id + 1L
                metrics_log[[metric_id]] <- data.frame(
                    Imp = imp_idx,
                    Fold = fold_idx,
                    Risk = "Death",
                    Metric = "Uno's C-Index",
                    Time = "Global",
                    Value = death_res$c_global,
                    stringsAsFactors = FALSE
                )

                metric_id <- metric_id + 1L
                metrics_log[[metric_id]] <- data.frame(
                    Imp = imp_idx,
                    Fold = fold_idx,
                    Risk = "Death",
                    Metric = "IBS",
                    Time = "Global",
                    Value = death_res$ibs_global,
                    stringsAsFactors = FALSE
                )

                for (time_label in names(death_res$c_horizon)) {
                    metric_id <- metric_id + 1L
                    metrics_log[[metric_id]] <- data.frame(
                        Imp = imp_idx,
                        Fold = fold_idx,
                        Risk = "Death",
                        Metric = "Uno's C-Index",
                        Time = time_label,
                        Value = unname(death_res$c_horizon[[time_label]]),
                        stringsAsFactors = FALSE
                    )

                    metric_id <- metric_id + 1L
                    metrics_log[[metric_id]] <- data.frame(
                        Imp = imp_idx,
                        Fold = fold_idx,
                        Risk = "Death",
                        Metric = "IBS",
                        Time = time_label,
                        Value = unname(death_res$ibs_horizon[[time_label]]),
                        stringsAsFactors = FALSE
                    )
                }
            }

            raw_id <- raw_id + 1L
            raw_predictions[[raw_id]] <- list(
                imp_idx = imp_idx,
                fold_idx = fold_idx,
                original_val_idx = val_idx,
                eval_times = eval_times,
                readmission = if (nzchar(readmit_res$error)) {
                    list(error = readmit_res$error)
                } else {
                    list(
                        lp_train = readmit_res$lp_train,
                        lp_val = readmit_res$lp_val,
                        surv_val_matrix = readmit_res$surv_val,
                        y_train = readmit_res$y_train,
                        y_val = readmit_res$y_val,
                        dropped_rows = readmit_res$dropped
                    )
                },
                death = if (nzchar(death_res$error)) {
                    list(error = death_res$error)
                } else {
                    list(
                        lp_train = death_res$lp_train,
                        lp_val = death_res$lp_val,
                        surv_val_matrix = death_res$surv_val,
                        y_train = death_res$y_train,
                        y_val = death_res$y_val,
                        dropped_rows = death_res$dropped
                    )
                }
            )

            if (isTRUE(verbose)) {
                cat(".")
            }
        }

        if (isTRUE(verbose)) {
            cat("\n")
        }
    }

    metrics_df <- if (length(metrics_log) > 0L) {
        do.call(rbind, metrics_log)
    } else {
        data.frame(
            Imp = integer(0),
            Fold = integer(0),
            Risk = character(0),
            Metric = character(0),
            Time = character(0),
            Value = numeric(0),
            stringsAsFactors = FALSE
        )
    }

    failures_df <- if (length(failures_log) > 0L) {
        do.call(rbind, failures_log)
    } else {
        data.frame(
            Imp = integer(0),
            Fold = integer(0),
            Risk = character(0),
            Error = character(0),
            stringsAsFactors = FALSE
        )
    }

    fold_assignments_df <- do.call(rbind, fold_assignments)
    summary_df <- .dual_cox_summarize_metrics(metrics_df)

    result <- list(
        metrics = metrics_df,
        summary = summary_df,
        raw_predictions = raw_predictions,
        failures = failures_df,
        fold_assignments = fold_assignments_df,
        config = list(
            formula_readmit = meta_readmit$formula_text,
            formula_death = meta_death$formula_text,
            readmit_time_col = readmit_time_col,
            readmit_event_col = readmit_event_col,
            death_time_col = death_time_col,
            death_event_col = death_event_col,
            eval_times = eval_times,
            k_folds = k_folds,
            seed = seed,
            plan_dummy_cols = plan_dummy_cols,
            plan_strata_fallback_col = plan_strata_fallback_col,
            min_test_rows = min_test_rows,
            timestamp = as.character(Sys.time())
        )
    )

    if (!is.null(save_prefix)) {
        if (!is.character(save_prefix) || length(save_prefix) != 1L || !nzchar(save_prefix)) {
            stop("save_prefix must be NULL or a non-empty string.", call. = FALSE)
        }

        if (is.null(output_dir)) {
            output_dir <- getwd()
        }
        if (!dir.exists(output_dir)) {
            stop("output_dir does not exist: ", output_dir, call. = FALSE)
        }

        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        metrics_path <- .dual_cox_make_unique_path(file.path(
            output_dir,
            sprintf("%s_metrics_%s.csv", save_prefix, timestamp)
        ))
        summary_path <- .dual_cox_make_unique_path(file.path(
            output_dir,
            sprintf("%s_summary_%s.csv", save_prefix, timestamp)
        ))
        artifacts_path <- .dual_cox_make_unique_path(file.path(
            output_dir,
            sprintf("%s_artifacts_%s.rds", save_prefix, timestamp)
        ))

        utils::write.csv(metrics_df, metrics_path, row.names = FALSE)
        utils::write.csv(summary_df, summary_path, row.names = FALSE)
        saveRDS(result, artifacts_path)

        result$config$save_paths <- list(
            metrics = metrics_path,
            summary = summary_path,
            artifacts = artifacts_path
        )
    }

    result
}
