#' Bootstrap OOB IBS Evaluation for Cox Models with Multiple Imputation
#' 
#' Fully self-contained version with all helpers nested inside to avoid conflicts.
#' Uses rms::cph for faster computation (no SE calculation).

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
    
    # ========================================================================
    # NESTED HELPER FUNCTIONS (self-contained to avoid conflicts)
    # ========================================================================
    
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
    
    collect_special_single_vars <- function(expr, specials) {
        vars <- character(0)
        walk <- function(node) {
            if (!is.call(node)) return(invisible(NULL))
            node_name <- normalize_call_name(node[[1L]])
            if (node_name %in% specials) {
                if (length(node) == 2L && is.symbol(node[[2L]])) {
                    vars <<- c(vars, as.character(node[[2L]]))
                }
                return(invisible(NULL))
            }
            if (length(node) >= 2L) {
                for (i in 2L:length(node)) walk(node[[i]])
            }
            invisible(NULL)
        }
        walk(expr)
        unique(vars)
    }
    
    collect_explicit_factor_vars <- function(expr) {
        vars <- character(0)
        walk <- function(node) {
            if (!is.call(node)) return(invisible(NULL))
            node_name <- normalize_call_name(node[[1L]])
            if (node_name %in% c("factor", "as.factor")) {
                factor_vars <- unique(all.vars(node[[2L]]))
                if (length(factor_vars) == 1L) vars <<- c(vars, factor_vars)
            }
            if (length(node) >= 2L) {
                for (i in 2L:length(node)) walk(node[[i]])
            }
            invisible(NULL)
        }
        walk(expr)
        unique(vars)
    }
    
    rewrite_explicit_factor_calls <- function(expr) {
        if (!is.call(expr)) return(expr)
        node_name <- normalize_call_name(expr[[1L]])
        if (node_name %in% c("factor", "as.factor")) {
            factor_vars <- unique(all.vars(expr[[2L]]))
            if (length(factor_vars) == 1L) return(as.name(factor_vars))
        }
        for (i in seq_along(expr)) {
            expr[[i]] <- rewrite_explicit_factor_calls(expr[[i]])
        }
        expr
    }
    
    # Convert strata() to strat() for rms::cph compatibility
    convert_strata_to_strat <- function(expr) {
        if (!is.call(expr)) return(expr)
        node_name <- normalize_call_name(expr[[1L]])
        if (node_name == "strata") {
            expr[[1L]] <- as.name("strat")
        }
        for (i in seq_along(expr)) {
            expr[[i]] <- convert_strata_to_strat(expr[[i]])
        }
        expr
    }
    
    collect_observed_levels <- function(x) {
        raw <- as.character(x)
        sort(unique(raw[!is.na(x)]), method = "radix")
    }
    
    to01 <- function(x) {
        if (is.logical(x)) return(as.integer(x))
        if (is.factor(x)) x <- as.character(x)
        x_num <- suppressWarnings(as.numeric(x))
        vals <- x_num[!is.na(x_num)]
        if (length(vals) == 0L) stop("event_col has no non-missing values.")
        if (all(vals %in% c(0, 1))) return(as.integer(x_num))
        if (all(vals %in% c(1, 2))) return(as.integer(x_num - 1L))
        stop("event_col must be binary 0/1 (or 1/2).")
    }
    
    cph_boot_event01 <- function(x, varname) {
        if (is.logical(x)) return(as.integer(x))
        if (is.factor(x)) x <- as.character(x)
        x_num <- suppressWarnings(as.numeric(x))
        vals <- x_num[!is.na(x_num)]
        if (length(vals) == 0L) stop("No non-missing event values in '", varname, "'.")
        if (all(vals %in% c(0, 1))) return(as.integer(x_num))
        if (all(vals %in% c(1, 2))) return(as.integer(x_num - 1L))
        stop("Variable '", varname, "' must be binary 0/1 (or 1/2).")
    }
    
    cph_boot_normalize_time <- function(x, varname) {
        if (!is.numeric(x)) stop("'", varname, "' must be numeric.")
        x <- as.numeric(x)
        non_missing <- !is.na(x)
        if (any(!is.finite(x[non_missing]))) stop("'", varname, "' contains non-finite values.")
        if (any(x[non_missing] < 0)) stop("'", varname, "' contains negative times.")
        x
    }
    
    sanitize_level_name <- function(var, level) {
        paste0(var, "__", make.names(as.character(level)))
    }
    
    build_level_mapping <- function(var, raw_levels) {
        sanitized <- vapply(raw_levels, function(level) sanitize_level_name(var, level), character(1))
        sanitized <- make.unique(sanitized, sep = "__dup")
        stats::setNames(sanitized, raw_levels)
    }
    
    cph_boot_prepare_model_split <- function(train, test, categorical_formula_vars) {
        train_out <- train
        test_out <- test
        for (var in categorical_formula_vars) {
            if (!var %in% names(train_out)) next
            
            train_raw <- train_out[[var]]
            test_raw <- test_out[[var]]
            train_chr <- as.character(train_raw)
            test_chr <- as.character(test_raw)
            train_levels <- collect_observed_levels(train_raw)
            if (length(train_levels) == 0L) next
            
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
    
    ibs_ipcw_train <- function(df_train, df_test, surv_mat, times, time_col, event_col, eps = 1e-8, g_min = 0.05) {
        ev_train <- to01(df_train[[event_col]])
        sf_cens <- survival::survfit(survival::Surv(df_train[[time_col]], 1 - ev_train) ~ 1)
        
        safe_times <- sf_cens$time[sf_cens$surv >= g_min]
        tau_ipcw <- if (length(safe_times)) max(safe_times) else max(times)
        
        keep <- times <= tau_ipcw
        if (sum(keep) < 2L) return(NA_real_)
        
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
            w <- ifelse(Tt <= t0 & Dt == 1L, 1 / G_at(pmax(Tt - eps, 0)), ifelse(Tt > t0, 1 / G_at(t0), 0))
            mean(w * (Y0 - S0)^2, na.rm = TRUE)
        }, numeric(1))
        
        denom <- max(times_use) - min(times_use)
        if (!is.finite(denom) || denom <= 0) return(NA_real_)
        
        as.numeric(sum(diff(times_use) * (head(bs_t, -1L) + tail(bs_t, -1L)) / 2) / denom)
    }
    
    cph_boot_worker <- function(b, state) {
        tryCatch({
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
                
                n_evt_tau <- sum(state$event_indicator[[m]][idx_test] == 1L &
                    state$df_list[[m]][idx_test, state$time_col] <= state$tau, na.rm = TRUE)
                if (n_evt_tau < 1L) {
                    row_fails[m] <- "no_events_before_tau"
                    next
                }
                
                df_train <- state$df_list[[m]][idx_train, , drop = FALSE]
                df_test <- state$df_list[[m]][idx_test, , drop = FALSE]
                
                prepared <- cph_boot_prepare_model_split(df_train, df_test, state$categorical_formula_vars)
                if (nzchar(prepared$error)) {
                    row_fails[m] <- prepared$error
                    next
                }
                df_train <- prepared$train
                df_test <- prepared$test
                
                res <- tryCatch({
                    fit <- rms::cph(state$model_formula, data = df_train, x = TRUE, y = TRUE, 
                                   surv = TRUE, se.fit = FALSE)
                    surv_mat <- pec::predictSurvProb(fit, newdata = df_test, times = state$eval_times)
                    
                    if (is.null(dim(surv_mat))) {
                        surv_mat <- matrix(surv_mat, nrow = nrow(df_test), 
                                          ncol = length(state$eval_times), byrow = TRUE)
                    } else {
                        surv_mat <- as.matrix(surv_mat)
                    }
                    
                    if (nrow(surv_mat) != nrow(df_test) || ncol(surv_mat) != length(state$eval_times)) {
                        list(value = NA_real_, err = "dimension_mismatch")
                    } else {
                        ibs_val <- ibs_ipcw_train(df_train, df_test, surv_mat, state$eval_times,
                                                  state$time_col, state$event_col)
                        if (is.finite(ibs_val)) {
                            list(value = ibs_val, err = "")
                        } else {
                            list(value = NA_real_, err = "non_finite_ibs")
                        }
                    }
                }, error = function(e) {
                    list(value = NA_real_, err = paste0("cph_error:", conditionMessage(e)))
                })
                
                row_ibs[m] <- res$value
                if (nzchar(res$err)) row_fails[m] <- res$err
            }
            
            list(ibs = row_ibs, fails = row_fails, split_fail = split_fail)
        }, error = function(e) {
            list(ibs = rep(NA_real_, state$M), fails = rep("", state$M),
                 split_fail = paste0("worker_error:", conditionMessage(e)))
        })
    }
    
    # ========================================================================
    # MAIN FUNCTION BODY
    # ========================================================================
    
    t_start <- Sys.time()
    
    if (!inherits(formula, "formula")) stop("formula must be a formula.")
    if (!is.list(imputed_list) || length(imputed_list) == 0L) stop("imputed_list must be non-empty.")
    
    B <- as.integer(B)
    min_oob_n <- as.integer(min_oob_n)
    min_oob_events <- as.integer(min_oob_events)
    n_eval_times <- as.integer(n_eval_times)
    cpus <- as.integer(cpus)
    
    df_list_raw <- lapply(imputed_list, as.data.frame)
    M <- length(df_list_raw)
    
    if (is.null(min_valid_imputations)) min_valid_imputations <- M
    min_valid_imputations <- as.integer(min_valid_imputations)
    
    n_by_imp <- vapply(df_list_raw, nrow, integer(1))
    if (length(unique(n_by_imp)) != 1L) stop("All imputations must have same number of rows.")
    n <- n_by_imp[1L]
    
    rhs_expr <- formula[[length(formula)]]
    strata_vars <- collect_special_single_vars(rhs_expr, c("strat", "strata"))
    explicit_factor_vars <- collect_explicit_factor_vars(rhs_expr)
    
    character_factor_vars <- unique(unlist(lapply(df_list_raw, function(d) {
        names(d)[vapply(d, function(x) is.factor(x) || is.character(x), logical(1))]
    })))
    character_factor_vars <- intersect(character_factor_vars, all.vars(formula))
    
    categorical_formula_vars <- unique(c(strata_vars, explicit_factor_vars, character_factor_vars))
    
    df_list <- lapply(df_list_raw, function(d) {
        d <- as.data.frame(d)
        d[[time_col]] <- cph_boot_normalize_time(d[[time_col]], time_col)
        d[[event_col]] <- cph_boot_event01(d[[event_col]], event_col)
        d
    })
    
    if (M > 1L) {
        ref_time <- df_list[[1L]][[time_col]]
        ref_event <- df_list[[1L]][[event_col]]
        same_time <- vapply(df_list[-1L], function(d) {
            isTRUE(all.equal(d[[time_col]], ref_time, tolerance = 1e-12))
        }, logical(1))
        same_event <- vapply(df_list[-1L], function(d) identical(d[[event_col]], ref_event), logical(1))
        if (!all(same_time) || !all(same_event)) {
            stop("Outcome/time information differs across imputations.")
        }
    }
    
    event_value <- 1L
    event_indicator <- lapply(df_list, function(d) d[[event_col]])
    
    if (is.null(tau)) {
        all_times <- unlist(lapply(df_list, function(d) d[[time_col]]), use.names = FALSE)
        tau <- as.numeric(stats::quantile(all_times, probs = 0.90, na.rm = TRUE))
        if (verbose) message("Auto-selected tau (90th percentile): ", round(tau, 2))
    }
    if (!is.numeric(tau) || length(tau) != 1L || !is.finite(tau) || tau <= 0) {
        stop("tau must be a finite positive scalar.")
    }
    
    eval_times <- seq(1e-6, tau, length.out = n_eval_times)
    
    # Convert strata() to strat() for rms::cph
    model_formula_expr <- convert_strata_to_strat(rewrite_explicit_factor_calls(formula))
    model_formula_txt <- paste(deparse(model_formula_expr, width.cutoff = 500L), collapse = " ")
    model_formula <- stats::as.formula(model_formula_txt, env = environment(formula))
    has_strata <- length(strata_vars) > 0L
    
    # Preflight check
    preflight_split <- cph_boot_prepare_model_split(df_list[[1L]], df_list[[1L]], categorical_formula_vars)
    if (nzchar(preflight_split$error)) stop(paste0("Preflight failed: ", preflight_split$error))
    
    preflight_err <- tryCatch({
        fit <- rms::cph(model_formula, data = preflight_split$train, x = TRUE, y = TRUE, surv = TRUE, se.fit = FALSE)
        pec::predictSurvProb(fit, newdata = preflight_split$test[1:5, , drop = FALSE], times = eval_times[1:2])
        NULL
    }, error = function(e) conditionMessage(e))
    
    if (!is.null(preflight_err)) stop(paste0("Preflight cph/predictSurvProb failed: ", preflight_err))
    
    # Setup parallel
    detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
    detected <- if (is.numeric(detected) && length(detected) == 1 && is.finite(detected) && detected >= 1) detected else 1L
    is_windows <- identical(.Platform$OS.type, "windows")
    n_cores <- as.integer(max(1L, min(cpus, B, if (is_windows) min(8L, detected - 1L) else detected - 1L)))
    
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    if (n_cores > 1L) {
        future::plan(future::multisession, workers = n_cores)
    } else {
        future::plan(future::sequential)
    }
    
    if (verbose) {
        if (has_strata) message("Strata: ", paste(strata_vars, collapse = ", "))
        cat(sprintf("Bootstrap OOB + MI: B=%d, M=%d, tau=%.2f, cores=%d\n", B, M, tau, n_cores))
    }
    
    # CRITICAL: Pass all nested functions to workers
    worker_state <- list(
        M = M, n = n, min_oob_n = min_oob_n, min_oob_events = min_oob_events,
        tau = tau, time_col = time_col, event_col = event_col, 
        event_indicator = event_indicator, df_list = df_list, 
        categorical_formula_vars = categorical_formula_vars,
        model_formula = model_formula, eval_times = eval_times
    )
    
    boot_results <- future.apply::future_lapply(
        X = seq_len(B),
        FUN = cph_boot_worker,
        state = worker_state,
        future.seed = seed,
        future.packages = c("survival", "pec", "rms"),
        future.globals = structure(TRUE, add = list(
            cph_boot_worker = cph_boot_worker,
            cph_boot_prepare_model_split = cph_boot_prepare_model_split,
            ibs_ipcw_train = ibs_ipcw_train,
            to01 = to01,
            collect_observed_levels = collect_observed_levels,
            sanitize_level_name = sanitize_level_name,
            build_level_mapping = build_level_mapping
        ))
    )
    
    results_matrix <- do.call(rbind, lapply(boot_results, `[[`, "ibs"))
    fail_matrix <- do.call(rbind, lapply(boot_results, `[[`, "fails"))
    split_fail_reason <- vapply(boot_results, `[[`, "", "split_fail")
    
    boot_pooled <- apply(results_matrix, 1L, function(x) {
        valid <- x[is.finite(x)]
        if (length(valid) < min_valid_imputations) NA_real_ else mean(valid)
    })
    boot_pooled <- boot_pooled[is.finite(boot_pooled)]
    
    if (length(boot_pooled) < max(1L, min(2L, B))) {
        cat("\n[!] CRITICAL FAILURE\n")
        all_fails <- c(as.vector(fail_matrix[fail_matrix != ""]), split_fail_reason[split_fail_reason != ""])
        if (length(all_fails) > 0L) print(sort(table(all_fails), decreasing = TRUE))
        stop("Too few valid bootstrap replicates.")
    }
    
    pooled_ibs <- mean(boot_pooled)
    ci <- stats::quantile(boot_pooled, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    
    if (verbose) {
        cat("\n============================================================\n")
        cat(sprintf("Pooled OOB IBS: %.4f (95%% CI: %.4f - %.4f)\n", pooled_ibs, ci[1L], ci[2L]))
        cat(sprintf("Valid replicates: %d / %d\n", length(boot_pooled), B))
        cat("============================================================\n")
    }
    
    invisible(list(
        pooled_ibs = pooled_ibs, ci_95 = c(lower = ci[1L], upper = ci[2L]),
        boot_pooled = boot_pooled, raw_matrix = results_matrix, fail_matrix = fail_matrix
    ))
}
