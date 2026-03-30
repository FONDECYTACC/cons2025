#' Cox Model C-Index Evaluation with Multiple Imputation and Bootstrap
#' 
#' Calculates Uno's C-index (time-dependent concordance) for Cox models
#' using multiple imputed datasets with cross-validation.
#' Equivalent to the Python XGBoost C-index calculation.

# =============================================================================
# REQUIRED PACKAGES
# =============================================================================
required_packages <- c("survival", "survAUC", "pec", "caret", "future", "future.apply")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Convert event indicator to 0/1
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

#' Normalize time variable
normalize_time <- function(x, varname) {
  if (!is.numeric(x)) stop("Variable '", varname, "' must be numeric.")
  x <- as.numeric(x)
  non_missing <- !is.na(x)
  if (any(!is.finite(x[non_missing]))) stop("Variable '", varname, "' contains non-finite values.")
  if (any(x[non_missing] < 0)) stop("Variable '", varname, "' contains negative follow-up times.")
  x
}

#' Collect observed levels from a factor/character variable
collect_observed_levels <- function(x) {
  raw <- as.character(x)
  sort(unique(raw[!is.na(x)]), method = "radix")
}

#' Sanitize level names for model matrix compatibility
sanitize_level_name <- function(var, level) {
  paste0(var, "__", make.names(as.character(level)))
}

#' Build level mapping between raw and sanitized names
build_level_mapping <- function(var, raw_levels) {
  sanitized <- vapply(raw_levels, function(level) sanitize_level_name(var, level), character(1))
  sanitized <- make.unique(sanitized, sep = "__dup")
  stats::setNames(sanitized, raw_levels)
}

#' Prepare train/test split with proper factor level handling
prepare_model_split <- function(train, test, categorical_vars) {
  train_out <- train
  test_out <- test
  
  for (var in categorical_vars) {
    if (!var %in% names(train_out)) next
    
    train_raw <- train_out[[var]]
    test_raw <- test_out[[var]]
    train_chr <- as.character(train_raw)
    test_chr <- as.character(test_raw)
    train_levels <- collect_observed_levels(train_raw)
    
    if (length(train_levels) == 0L) next
    
    # Check for unseen levels in test
    unseen <- !is.na(test_raw) & !(test_chr %in% train_levels)
    if (any(unseen)) {
      return(list(train = train_out, test = test_out, 
                  error = paste0("unseen_factor_level:", var)))
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

#' Calculate Uno's C-index (time-dependent concordance)
#' Equivalent to sksurv.metrics.concordance_index_ipcw in Python
calculate_uno_cindex <- function(train_data, test_data, risk_scores, 
                                  time_col, event_col, tau = NULL) {
  
  # Prepare survival objects
  y_train <- survival::Surv(train_data[[time_col]], train_data[[event_col]])
  y_test <- survival::Surv(test_data[[time_col]], test_data[[event_col]])
  
  # Determine tau if not provided
  if (is.null(tau)) {
    tau <- max(test_data[[time_col]], na.rm = TRUE) * 0.95
  }
  
  # Check censoring distribution at tau
  sf_cens <- survival::survfit(survival::Surv(train_data[[time_col]], 
                                               1 - train_data[[event_col]]) ~ 1)
  g_tau <- tryCatch(
    summary(sf_cens, times = tau, extend = TRUE)$surv,
    error = function(e) NA_real_
  )
  
  # Adjust tau if censoring probability is too low
  if (!is.finite(g_tau) || g_tau <= 1e-6) {
    tt <- sf_cens$time[sf_cens$surv > 1e-6]
    tau <- if (length(tt)) min(tau, max(tt)) else NA_real_
  }
  
  if (!is.finite(tau) || tau <= 0) {
    return(list(uno_c = NA_real_, tau = NA_real_, n_events = 0L))
  }
  
  # Count events at or before tau
  ok <- is.finite(risk_scores) & 
        is.finite(test_data[[time_col]]) & 
        is.finite(test_data[[event_col]])
  
  n_events_tau <- sum(
    test_data[[event_col]][ok] == 1L & test_data[[time_col]][ok] <= tau,
    na.rm = TRUE
  )
  
  if (n_events_tau < 2L) {
    return(list(uno_c = NA_real_, tau = tau, n_events = n_events_tau))
  }
  
  # Calculate Uno's C-index using survAUC
  # Use ranked risk scores for numerical stability
  lp_rank <- rank(risk_scores[ok], ties.method = "average")
  
  uno_c <- tryCatch({
    survAUC::UnoC(
      y_train,
      survival::Surv(test_data[[time_col]][ok], test_data[[event_col]][ok]),
      lp_rank,
      time = tau
    )
  }, error = function(e) NA_real_)
  
  list(uno_c = uno_c, tau = tau, n_events = n_events_tau)
}

#' Calculate Harrell's C-index (standard concordance)
calculate_harrell_cindex <- function(test_data, risk_scores, 
                                      time_col, event_col, tau = NULL) {
  
  ok <- is.finite(risk_scores) & 
        is.finite(test_data[[time_col]]) & 
        is.finite(test_data[[event_col]])
  
  if (sum(ok) < 2L) {
    return(list(harrell_c = NA_real_, n_pairs = 0L))
  }
  
  ymax <- if (is.null(tau)) NULL else tau
  
  harrell_c <- tryCatch({
    if (is.null(ymax)) {
      survival::concordance(
        survival::Surv(test_data[[time_col]][ok], test_data[[event_col]][ok]) ~ risk_scores[ok],
        reverse = TRUE
      )$concordance
    } else {
      survival::concordance(
        survival::Surv(test_data[[time_col]][ok], test_data[[event_col]][ok]) ~ risk_scores[ok],
        timewt = "n/G2",
        ymax = ymax,
        reverse = TRUE
      )$concordance
    }
  }, error = function(e) NA_real_)
  
  list(harrell_c = harrell_c, n_pairs = sum(ok))
}

# =============================================================================
# MAIN FUNCTION: C-INDEX EVALUATION FOR COX MODELS
# =============================================================================

#' Evaluate Cox Model C-Index with Multiple Imputation and Cross-Validation
#' 
#' @param formula Cox model formula (e.g., Surv(time, event) ~ x1 + x2)
#' @param imputed_list List of imputed datasets (e.g., py_corrected_datasets_boot)
#' @param time_col Name of the time column
#' @param event_col Name of the event column
#' @param cv_strata_col Column for CV stratification (default: "plan_type_strata")
#' @param k Number of CV folds (default: 10)
#' @param n_repeats Number of CV repeats (default: 5)
#' @param seed Random seed (default: 2125)
#' @param cpus Number of CPU cores (default: 8)
#' @param tau_max Maximum time horizon for C-index (NULL = auto)
#' @param time_horizons Specific time horizons for time-dependent C-index (default: c(12, 36, 60))
#' @param verbose Print progress messages (default: TRUE)
#' 
#' @return List containing:
#'   - summary: Data frame with mean C-indices across imputations and folds
#'   - raw_results: Data frame with all fold-level results
#'   - uno_c_global: Global Uno's C-index (pooled across all folds)
#'   - harrell_c_global: Global Harrell's C-index
#'   - time_dependent: Time-dependent C-indices at specified horizons

evaluate_cox_cindex <- function(
    formula,
    imputed_list,
    time_col,
    event_col,
    cv_strata_col = "plan_type_strata",
    k = 10,
    n_repeats = 5,
    seed = 2125,
    cpus = 8,
    tau_max = NULL,
    time_horizons = c(12, 36, 60),
    verbose = TRUE
) {
  
  t_start <- Sys.time()
  
  # Validate inputs
  if (!inherits(formula, "formula")) {
    formula <- stats::as.formula(formula, env = parent.frame())
  }
  if (!is.list(imputed_list) || length(imputed_list) == 0L) {
    stop("imputed_list must be a non-empty list.")
  }
  
  # Convert to data frames and normalize
  df_list <- lapply(imputed_list, function(d) {
    d <- as.data.frame(d)
    d[[time_col]] <- normalize_time(d[[time_col]], time_col)
    d[[event_col]] <- to01(d[[event_col]])
    d
  })
  
  M <- length(df_list)
  n <- nrow(df_list[[1L]])

  # Identify categorical variables
  rhs_vars <- all.vars(formula[[length(formula)]])

  # Validate required columns referenced by the formula and outcome names
  required_cols <- unique(c(rhs_vars, time_col, event_col))
  missing_by_imp <- lapply(df_list, function(df) setdiff(required_cols, names(df)))
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
  
  # Verify consistent outcomes across imputations
  if (M > 1L) {
    ref_time <- df_list[[1L]][[time_col]]
    ref_event <- df_list[[1L]][[event_col]]
    same_time <- vapply(df_list[-1L], function(d) {
      isTRUE(all.equal(d[[time_col]], ref_time, tolerance = 1e-12))
    }, logical(1))
    same_event <- vapply(df_list[-1L], function(d) {
      identical(d[[event_col]], ref_event)
    }, logical(1))
    if (!all(same_time) || !all(same_event)) {
      stop("Outcome/time information differs across imputations.")
    }
  }
  
  categorical_vars <- rhs_vars[vapply(rhs_vars, function(var) {
    any(vapply(df_list, function(d) {
      is.factor(d[[var]]) || is.character(d[[var]])
    }, logical(1)))
  }, logical(1))]
  
  # Determine tau_max
  if (is.null(tau_max)) {
    tau_max <- as.numeric(quantile(df_list[[1L]][[time_col]], probs = 0.90, na.rm = TRUE))
  }
  c_tau <- tau_max * 0.95
  
  # Create CV strata
  if (!cv_strata_col %in% names(df_list[[1L]])) {
    warning(sprintf("cv_strata_col '%s' not found. Using event indicator only.", cv_strata_col))
    cv_strata <- factor(df_list[[1L]][[event_col]])
  } else {
    cv_strata <- interaction(
      df_list[[1L]][[event_col]],
      df_list[[1L]][[cv_strata_col]],
      drop = TRUE,
      lex.order = TRUE
    )
  }
  
  # Generate CV folds
  seeds <- seed + seq_len(n_repeats) - 1L
  folds_by_repeat <- lapply(seq_len(n_repeats), function(rep_id) {
    set.seed(seeds[rep_id])
    caret::createFolds(cv_strata, k = k, list = TRUE, returnTrain = FALSE)
  })
  
  if (verbose) {
    total_models <- M * k * n_repeats
    cat(sprintf("\n========================================\n"))
    cat(sprintf("Cox Model C-Index Evaluation\n"))
    cat(sprintf("========================================\n"))
    cat(sprintf("Imputations: %d | Folds: %d | Repeats: %d\n", M, k, n_repeats))
    cat(sprintf("Total models: %d\n", total_models))
    cat(sprintf("Tau (90th percentile): %.2f months\n", tau_max))
    cat(sprintf("C-tau (evaluation horizon): %.2f months\n", c_tau))
    cat(sprintf("Time horizons: %s months\n", paste(time_horizons, collapse = ", ")))
    cat(sprintf("========================================\n\n"))
  }
  
  # Setup parallel processing
  detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (!is.numeric(detected) || length(detected) != 1L || !is.finite(detected)) {
    detected <- 1L
  }
  n_cores <- as.integer(max(1L, min(cpus, M * k * n_repeats, max(1L, detected - 1L))))
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  if (n_cores > 1L) {
    plan_error <- tryCatch(
      {
        future::plan(future::multisession, workers = n_cores)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (is.null(plan_error)) {
      if (verbose) cat(sprintf("Using %d CPU cores\n\n", n_cores))
    } else {
      future::plan(future::sequential)
      n_cores <- 1L
      warning(
        sprintf(
          "Parallel cluster setup failed; falling back to sequential evaluation. Original error: %s",
          plan_error
        ),
        call. = FALSE
      )
    }
  } else {
    future::plan(future::sequential)
  }
  
  # Create task grid
  task_grid <- expand.grid(
    r = seq_len(n_repeats),
    m = seq_len(M),
    i = seq_len(k),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Main computation loop
  all_results <- future.apply::future_lapply(
    X = seq_len(nrow(task_grid)),
    FUN = function(row_id) {
      rep_id <- task_grid$r[row_id]
      imp_id <- task_grid$m[row_id]
      fold_id <- task_grid$i[row_id]
      
      tryCatch({
        # Get train/test indices
        test_idx <- folds_by_repeat[[rep_id]][[fold_id]]
        df_train <- df_list[[imp_id]][-test_idx, , drop = FALSE]
        df_test <- df_list[[imp_id]][test_idx, , drop = FALSE]
        
        # Handle categorical variables
        prepared <- prepare_model_split(df_train, df_test, categorical_vars)
        if (nzchar(prepared$error)) {
          return(list(
            rep = rep_id, imp = imp_id, fold = fold_id,
            uno_c = NA_real_, harrell_c = NA_real_,
            tau = NA_real_, n_events = 0L,
            time_dep_c = rep(NA_real_, length(time_horizons)),
            fail = prepared$error
          ))
        }
        df_train <- prepared$train
        df_test <- prepared$test
        
        # Fit Cox model
        fit <- survival::coxph(formula, data = df_train, x = TRUE, y = TRUE, model = TRUE)
        
        # Get linear predictors (risk scores)
        risk_scores <- predict(fit, newdata = df_test, type = "lp")
        
        # Calculate Uno's C-index (global)
        uno_result <- calculate_uno_cindex(
          df_train, df_test, risk_scores,
          time_col, event_col, tau = c_tau
        )
        
        # Calculate Harrell's C-index
        harrell_result <- calculate_harrell_cindex(
          df_test, risk_scores,
          time_col, event_col, tau = c_tau
        )
        
        # Calculate time-dependent C-indices at specified horizons
        time_dep_c <- vapply(time_horizons, function(t_h) {
          if (t_h > max(df_test[[time_col]], na.rm = TRUE)) {
            return(NA_real_)
          }
          result <- calculate_harrell_cindex(
            df_test, risk_scores,
            time_col, event_col, tau = t_h
          )
          result$harrell_c
        }, numeric(1))
        
        list(
          rep = rep_id,
          imp = imp_id,
          fold = fold_id,
          uno_c = uno_result$uno_c,
          harrell_c = harrell_result$harrell_c,
          tau = uno_result$tau,
          n_events = uno_result$n_events,
          n_test = nrow(df_test),
          time_dep_c = time_dep_c,
          fail = ""
        )
        
      }, error = function(e) {
        list(
          rep = rep_id,
          imp = imp_id,
          fold = fold_id,
          uno_c = NA_real_,
          harrell_c = NA_real_,
          tau = NA_real_,
          n_events = 0L,
          n_test = 0L,
          time_dep_c = rep(NA_real_, length(time_horizons)),
          fail = conditionMessage(e)
        )
      })
    },
    future.seed = seed,
    future.packages = c("survival", "survAUC")
  )
  
  # Convert to data frame
  results_df <- data.frame(
    rep = vapply(all_results, `[[`, integer(1), "rep"),
    imp = vapply(all_results, `[[`, integer(1), "imp"),
    fold = vapply(all_results, `[[`, integer(1), "fold"),
    uno_c = vapply(all_results, `[[`, numeric(1), "uno_c"),
    harrell_c = vapply(all_results, `[[`, numeric(1), "harrell_c"),
    tau = vapply(all_results, `[[`, numeric(1), "tau"),
    n_events = vapply(all_results, `[[`, integer(1), "n_events"),
    n_test = vapply(all_results, `[[`, integer(1), "n_test"),
    fail = vapply(all_results, `[[`, character(1), "fail"),
    stringsAsFactors = FALSE
  )
  
  # Add time-dependent C-indices
  time_dep_matrix <- do.call(rbind, lapply(all_results, `[[`, "time_dep_c"))
  colnames(time_dep_matrix) <- paste0("c_at_", time_horizons, "m")
  results_df <- cbind(results_df, time_dep_matrix)
  
  # Calculate summary statistics
  valid_results <- results_df[results_df$fail == "", ]
  
  if (nrow(valid_results) == 0) {
    fail_counts <- sort(table(results_df$fail[nzchar(results_df$fail)]), decreasing = TRUE)
    fail_preview <- if (length(fail_counts) == 0L) {
      "No fold-level error messages were captured."
    } else {
      paste(
        utils::head(
          sprintf("%s (n=%d)", names(fail_counts), as.integer(fail_counts)),
          5L
        ),
        collapse = " | "
      )
    }
    stop(
      sprintf("No valid results obtained. Fold errors: %s", fail_preview),
      call. = FALSE
    )
  }
  
  # Pool by imputation (mean across folds and repeats)
  imp_summary <- aggregate(
    cbind(uno_c, harrell_c) ~ imp,
    data = valid_results,
    FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                        sd = sd(x, na.rm = TRUE),
                        n = sum(!is.na(x)))
  )
  
  # Overall pooled estimates
  pooled_uno <- mean(valid_results$uno_c, na.rm = TRUE)
  pooled_harrell <- mean(valid_results$harrell_c, na.rm = TRUE)
  
  # Confidence intervals (across all folds)
  ci_uno <- quantile(valid_results$uno_c, probs = c(0.025, 0.975), na.rm = TRUE)
  ci_harrell <- quantile(valid_results$harrell_c, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Time-dependent summaries
  time_dep_summary <- data.frame(
    horizon = time_horizons,
    mean_c = colMeans(time_dep_matrix[results_df$fail == "", , drop = FALSE], na.rm = TRUE),
    ci_lower = apply(time_dep_matrix[results_df$fail == "", , drop = FALSE], 2, 
                     quantile, probs = 0.025, na.rm = TRUE),
    ci_upper = apply(time_dep_matrix[results_df$fail == "", , drop = FALSE], 2,
                     quantile, probs = 0.975, na.rm = TRUE)
  )
  
  t_end <- Sys.time()
  elapsed <- difftime(t_end, t_start, units = "mins")
  
  if (verbose) {
    cat(sprintf("\n========================================\n"))
    cat(sprintf("RESULTS SUMMARY\n"))
    cat(sprintf("========================================\n"))
    cat(sprintf("Completed in: %.2f minutes\n", as.numeric(elapsed)))
    cat(sprintf("Valid models: %d / %d\n", nrow(valid_results), nrow(results_df)))
    cat(sprintf("\n--- Global C-Index (across all folds) ---\n"))
    cat(sprintf("Uno's C-index:    %.4f (95%% CI: %.4f - %.4f)\n", 
                pooled_uno, ci_uno[1], ci_uno[2]))
    cat(sprintf("Harrell's C-index: %.4f (95%% CI: %.4f - %.4f)\n",
                pooled_harrell, ci_harrell[1], ci_harrell[2]))
    cat(sprintf("\n--- Time-Dependent C-Index ---\n"))
    for (i in seq_len(nrow(time_dep_summary))) {
      cat(sprintf("At %d months: %.4f (95%% CI: %.4f - %.4f)\n",
                  time_dep_summary$horizon[i],
                  time_dep_summary$mean_c[i],
                  time_dep_summary$ci_lower[i],
                  time_dep_summary$ci_upper[i]))
    }
    cat(sprintf("========================================\n"))
    
    # Show failures if any
    failures <- results_df[results_df$fail != "", ]
    if (nrow(failures) > 0) {
      cat(sprintf("\n[!] Failures: %d models failed\n", nrow(failures)))
      fail_table <- sort(table(failures$fail), decreasing = TRUE)
      print(fail_table)
    }
  }
  
  # Return comprehensive results
  list(
    summary = data.frame(
      metric = c("Uno's C-index", "Harrell's C-index"),
      mean = c(pooled_uno, pooled_harrell),
      ci_lower = c(ci_uno[1], ci_harrell[1]),
      ci_upper = c(ci_uno[2], ci_harrell[2])
    ),
    time_dependent = time_dep_summary,
    by_imputation = imp_summary,
    raw_results = results_df,
    uno_c_global = pooled_uno,
    harrell_c_global = pooled_harrell,
    elapsed_time = elapsed,
    parameters = list(
      M = M,
      k = k,
      n_repeats = n_repeats,
      tau_max = tau_max,
      c_tau = c_tau,
      time_horizons = time_horizons,
      n_cores = n_cores
    )
  )
}

# =============================================================================
# BOOTSTRAP OOB C-INDEX (Alternative approach)
# =============================================================================

#' Bootstrap OOB C-Index for Cox Models with Multiple Imputation
#' 
#' Uses bootstrap sampling with out-of-bag evaluation, similar to your
#' existing IBS evaluation function but for C-index.
#' 
#' @param formula Cox model formula
#' @param imputed_list List of imputed datasets
#' @param time_col Time column name
#' @param event_col Event column name
#' @param B Number of bootstrap replicates (default: 200)
#' @param seed Random seed
#' @param cpus Number of CPU cores
#' @param tau_max Maximum time horizon
#' @param verbose Print progress

evaluate_cox_cindex_bootstrap <- function(
    formula,
    imputed_list,
    time_col,
    event_col,
    B = 200,
    seed = 2125,
    cpus = 8,
    tau_max = NULL,
    verbose = TRUE
) {
  
  t_start <- Sys.time()
  
  if (!inherits(formula, "formula")) {
    formula <- stats::as.formula(formula, env = parent.frame())
  }
  
  df_list <- lapply(imputed_list, function(d) {
    d <- as.data.frame(d)
    d[[time_col]] <- normalize_time(d[[time_col]], time_col)
    d[[event_col]] <- to01(d[[event_col]])
    d
  })
  
  M <- length(df_list)
  n <- nrow(df_list[[1L]])
  
  if (is.null(tau_max)) {
    tau_max <- as.numeric(quantile(df_list[[1L]][[time_col]], probs = 0.90, na.rm = TRUE))
  }
  c_tau <- tau_max * 0.95
  
  # Identify categorical variables
  rhs_vars <- all.vars(formula[[length(formula)]])
  categorical_vars <- rhs_vars[vapply(rhs_vars, function(var) {
    any(vapply(df_list, function(d) {
      is.factor(d[[var]]) || is.character(d[[var]])
    }, logical(1)))
  }, logical(1))]
  
  if (verbose) {
    cat(sprintf("\n========================================\n"))
    cat(sprintf("Bootstrap OOB C-Index Evaluation\n"))
    cat(sprintf("========================================\n"))
    cat(sprintf("Bootstrap replicates: %d\n", B))
    cat(sprintf("Imputations: %d\n", M))
    cat(sprintf("Tau: %.2f months\n", tau_max))
    cat(sprintf("========================================\n\n"))
  }
  
  # Setup parallel
  detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
  n_cores <- as.integer(max(1L, min(cpus, B, max(1L, detected - 1L))))
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  if (n_cores > 1L) {
    future::plan(future::multisession, workers = n_cores)
  } else {
    future::plan(future::sequential)
  }
  
  # Bootstrap worker function
  boot_worker <- function(b) {
    tryCatch({
      # Bootstrap sample
      idx_train <- sample.int(n, n, replace = TRUE)
      idx_test <- setdiff(seq_len(n), unique(idx_train))
      
      if (length(idx_test) < 10) {
        return(list(uno_c = NA_real_, harrell_c = NA_real_, fail = "too_few_oob"))
      }
      
      # Results for each imputation
      uno_vals <- rep(NA_real_, M)
      harrell_vals <- rep(NA_real_, M)
      
      for (m in seq_len(M)) {
        df_train <- df_list[[m]][idx_train, , drop = FALSE]
        df_test <- df_list[[m]][idx_test, , drop = FALSE]
        
        # Check minimum events
        oob_events <- sum(df_test[[event_col]] == 1L, na.rm = TRUE)
        if (oob_events < 3) {
          next
        }
        
        # Handle categorical variables
        prepared <- prepare_model_split(df_train, df_test, categorical_vars)
        if (nzchar(prepared$error)) next
        df_train <- prepared$train
        df_test <- prepared$test
        
        # Fit model and predict
        fit <- survival::coxph(formula, data = df_train, x = TRUE, y = TRUE, model = TRUE)
        risk_scores <- predict(fit, newdata = df_test, type = "lp")
        
        # Calculate C-indices
        uno_result <- calculate_uno_cindex(
          df_train, df_test, risk_scores,
          time_col, event_col, tau = c_tau
        )
        harrell_result <- calculate_harrell_cindex(
          df_test, risk_scores,
          time_col, event_col, tau = c_tau
        )
        
        uno_vals[m] <- uno_result$uno_c
        harrell_vals[m] <- harrell_result$harrell_c
      }
      
      # Pool across imputations (mean)
      list(
        uno_c = mean(uno_vals, na.rm = TRUE),
        harrell_c = mean(harrell_vals, na.rm = TRUE),
        fail = ""
      )
      
    }, error = function(e) {
      list(uno_c = NA_real_, harrell_c = NA_real_, fail = conditionMessage(e))
    })
  }
  
  # Run bootstrap
  boot_results <- future.apply::future_lapply(
    X = seq_len(B),
    FUN = boot_worker,
    future.seed = seed,
    future.packages = c("survival", "survAUC")
  )
  
  # Extract results
  uno_c_vec <- vapply(boot_results, `[[`, numeric(1), "uno_c")
  harrell_c_vec <- vapply(boot_results, `[[`, numeric(1), "harrell_c")
  fail_vec <- vapply(boot_results, `[[`, character(1), "fail")
  
  # Remove NAs
  valid_uno <- uno_c_vec[is.finite(uno_c_vec)]
  valid_harrell <- harrell_c_vec[is.finite(harrell_c_vec)]
  
  # Calculate statistics
  result <- list(
    uno_c = list(
      mean = mean(valid_uno),
      median = median(valid_uno),
      sd = sd(valid_uno),
      ci_95 = quantile(valid_uno, probs = c(0.025, 0.975), na.rm = TRUE),
      n_valid = length(valid_uno)
    ),
    harrell_c = list(
      mean = mean(valid_harrell),
      median = median(valid_harrell),
      sd = sd(valid_harrell),
      ci_95 = quantile(valid_harrell, probs = c(0.025, 0.975), na.rm = TRUE),
      n_valid = length(valid_harrell)
    ),
    raw_uno_c = valid_uno,
    raw_harrell_c = valid_harrell,
    failures = fail_vec[fail_vec != ""]
  )
  
  t_end <- Sys.time()
  elapsed <- difftime(t_end, t_start, units = "mins")
  
  if (verbose) {
    cat(sprintf("\n========================================\n"))
    cat(sprintf("BOOTSTRAP RESULTS\n"))
    cat(sprintf("========================================\n"))
    cat(sprintf("Completed in: %.2f minutes\n", as.numeric(elapsed)))
    cat(sprintf("Valid replicates: Uno=%d, Harrell=%d / %d\n", 
                result$uno_c$n_valid, result$harrell_c$n_valid, B))
    cat(sprintf("\nUno's C-index:\n"))
    cat(sprintf("  Mean: %.4f (95%% CI: %.4f - %.4f)\n",
                result$uno_c$mean, result$uno_c$ci_95[1], result$uno_c$ci_95[2]))
    cat(sprintf("\nHarrell's C-index:\n"))
    cat(sprintf("  Mean: %.4f (95%% CI: %.4f - %.4f)\n",
                result$harrell_c$mean, result$harrell_c$ci_95[1], result$harrell_c$ci_95[2]))
    cat(sprintf("========================================\n"))
  }
  
  result
}

# =============================================================================
# USAGE EXAMPLE
# =============================================================================

#' Example usage:
#' 
#' # Load your imputed datasets
#' # py_corrected_datasets_boot <- readRDS("path/to/your/data.rds")
#' 
#' # Define your Cox model formula
#' formula_death <- Surv(death_time_from_disch_m, death_event) ~ 
#'   age + sex + plan_type_corr + comorbidity_score
#' 
#' formula_readm <- Surv(readm_time_from_disch_m, readm_event) ~ 
#'   age + sex + plan_type_corr + comorbidity_score
#' 
#' # Method 1: Cross-validation approach (recommended)
#' results_death_cv <- evaluate_cox_cindex(
#'   formula = formula_death,
#'   imputed_list = py_corrected_datasets_boot,
#'   time_col = "death_time_from_disch_m",
#'   event_col = "death_event",
#'   k = 10,
#'   n_repeats = 5,
#'   cpus = 8,
#'   verbose = TRUE
#' )
#' 
#' results_readm_cv <- evaluate_cox_cindex(
#'   formula = formula_readm,
#'   imputed_list = py_corrected_datasets_boot,
#'   time_col = "readm_time_from_disch_m",
#'   event_col = "readm_event",
#'   k = 10,
#'   n_repeats = 5,
#'   cpus = 8,
#'   verbose = TRUE
#' )
#' 
#' # Method 2: Bootstrap OOB approach
#' results_death_boot <- evaluate_cox_cindex_bootstrap(
#'   formula = formula_death,
#'   imputed_list = py_corrected_datasets_boot,
#'   time_col = "death_time_from_disch_m",
#'   event_col = "death_event",
#'   B = 200,
#'   cpus = 8,
#'   verbose = TRUE
#' )
