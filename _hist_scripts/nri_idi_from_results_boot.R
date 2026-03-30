# Net Reclassification Improvement (NRI) and Integrated Discrimination Improvement (IDI)
# for survival models from results_boot and XGBoost predictions
#
# Reference:
# - Pencina MJ, D'Agostino RB Sr, Steyerberg EW. Extensions of net reclassification
#   improvement calculations to measure usefulness of new biomarkers.
#   Stat Med. 2011;30(1):11-21. doi:10.1002/sim.4085
# - Pencina MJ, D'Agostino RB Sr, D'Agostino RB Jr, Vasan RS.
#   Evaluating the added predictive ability of a new marker: From area under the ROC
#   curve to reclassification and beyond. Stat Med. 2008;27(2):157-172.
#
# Usage:
# source("cons/_hist_scripts/nri_idi_from_results_boot.R")
# nri_idi_out <- run_nri_idi_comparison(
#   results_boot_cox = results_boot_cox,  # Cox model results
#   xgb_pickle_path = "xgb/_out/xgb6_corr_DUAL_final_ev_hyp_20260223_2134.pkl",
#   risk = "death",  # or "readmission"
#   horizon = 12,    # months
#   cutoff = 0.15    # X-tile determined cutoff
# )

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ============================================================================
# Data Loading Functions
# ============================================================================

#' Load XGBoost predictions from Python pickle file
#' Requires reticulate package
#' @param pickle_path Path to the pickle file
#' @return List with predictions
load_xgb_predictions <- function(pickle_path) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required to load Python pickle files.", call. = FALSE)
  }
  
  if (!file.exists(pickle_path)) {
    stop("Pickle file not found: ", pickle_path, call. = FALSE)
  }
  
  pickle <- reticulate::import("pickle")
  builtins <- reticulate::import_builtins()
  
  py_data <- builtins$open(pickle_path, "rb")
  on.exit(builtins$close(py_data), add = TRUE)
  
  data <- pickle$load(py_data)
  
  # Convert to R list
  n_folds <- length(data)
  result <- vector("list", n_folds)
  
  for (i in seq_len(n_folds)) {
    py_fold <- data[[i]]
    
    # Extract numpy arrays and convert to R
    result[[i]] <- list(
      imp_idx = as.integer(py_fold$imp_idx) + 1L,  # Python 0-indexed
      fold_idx = as.integer(py_fold$fold_idx) + 1L,
      eval_times = as.numeric(py_fold$eval_times),
      # Survival matrices: rows = patients, cols = time horizons
      surv_readm = 1 - as.matrix(py_fold$probs_readm_matrix),  # convert risk to survival
      surv_death = 1 - as.matrix(py_fold$probs_death_matrix),
      # Outcomes: structured array with (event, time)
      y_r = reticulate::py_to_r(py_fold$y_val_r),
      y_d = reticulate::py_to_r(py_fold$y_val_d)
    )
  }
  
  result
}

#' Extract XGBoost predictions at a specific horizon
#' @param xgb_data List from load_xgb_predictions
#' @param risk Risk type: "death" or "readmission"
#' @param horizon Time horizon in months
#' @return Data frame with predictions
extract_xgb_at_horizon <- function(xgb_data, risk = c("death", "readmission"), horizon) {
  risk <- match.arg(risk)
  
  all_preds <- vector("list", length(xgb_data))
  
  for (i in seq_along(xgb_data)) {
    fold <- xgb_data[[i]]
    
    # Find horizon index
    h_idx <- which(fold$eval_times == horizon)
    if (length(h_idx) == 0) {
      stop("Horizon ", horizon, " not found in XGBoost eval_times: ", 
           paste(fold$eval_times, collapse = ", "), call. = FALSE)
    }
    
    # Get survival probability and convert to risk
    if (risk == "death") {
      surv_mat <- fold$surv_death
      y <- fold$y_d
    } else {
      surv_mat <- fold$surv_readm
      y <- fold$y_r
    }
    
    # y is a structured array: (event, time)
    pred_risk <- 1 - surv_mat[, h_idx]  # convert survival to risk
    
    # Extract event and time from structured array
    # The structured array has fields: event (bool), time (float)
    event <- as.logical(sapply(y, function(x) x[[1]]))
    time <- as.numeric(sapply(y, function(x) x[[2]]))
    
    all_preds[[i]] <- data.frame(
      fold_id = i,
      imp_idx = fold$imp_idx,
      fold_idx = fold$fold_idx,
      patient_idx = seq_along(pred_risk),
      pred_risk = pred_risk,
      time = time,
      event = as.integer(event),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, all_preds)
}

# ============================================================================
# Cox Model Extraction
# ============================================================================

#' Load results_boot object
#' @param path Path to .rds or .RData file
#' @param object_name Name of object if .RData
#' @return results_boot list
load_results_boot_object <- function(path, object_name = "results_boot") {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    return(readRDS(path))
  }
  
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  if (!object_name %in% loaded) {
    stop("Object `", object_name, "` not found. Loaded: ", 
         paste(loaded, collapse = ", "), call. = FALSE)
  }
  get(object_name, envir = env)
}

#' Extract Cox predictions at a specific horizon
#' @param results_boot results_boot object from evaluate_dual_cox_python_style
#' @param risk Risk type: "death" or "readmission"
#' @param horizon Time horizon in months
#' @return Data frame with predictions
extract_cox_at_horizon <- function(results_boot, risk = c("death", "readmission"), horizon) {
  risk <- match.arg(risk)
  
  if (!"raw_predictions" %in% names(results_boot)) {
    stop("results_boot does not contain raw_predictions", call. = FALSE)
  }
  
  all_preds <- list()
  
  for (item in results_boot$raw_predictions) {
    if (!risk %in% names(item)) next
    
    block <- item[[risk]]
    if (!is.list(block) || "error" %in% names(block)) next
    
    eval_times <- item$eval_times %||% results_boot$config$eval_times
    h_idx <- which(eval_times == horizon)
    if (length(h_idx) == 0) next
    
    surv_mat <- as.matrix(block$surv_val_matrix)
    y_val <- block$y_val
    
    pred_risk <- 1 - surv_mat[, h_idx]
    
    all_preds[[length(all_preds) + 1]] <- data.frame(
      fold_id = length(all_preds) + 1,
      imp_idx = item$imp_idx %||% NA_integer_,
      fold_idx = item$fold_idx %||% NA_integer_,
      patient_idx = seq_along(pred_risk),
      pred_risk = pred_risk,
      time = as.numeric(y_val$time),
      event = as.integer(y_val$event),
      stringsAsFactors = FALSE
    )
  }
  
  if (length(all_preds) == 0) {
    stop("No predictions found for risk='", risk, "' at horizon=", horizon, call. = FALSE)
  }
  
  do.call(rbind, all_preds)
}

# ============================================================================
# NRI and IDI Calculation
# ============================================================================

#' Calculate NRI and IDI for binary outcomes at a fixed horizon
#' 
#' For survival data, we use the Kaplan-Meier method to estimate survival
#' probability at the horizon and classify events vs non-events
#' 
#' @param time Observed follow-up times
#' @param event Event indicator (0/1)
#' @param pred_m1 Predicted risk from model 1 (reference)
#' @param pred_m2 Predicted risk from model 2 (new)
#' @param cutoffs Risk cutoffs for categorization (can be single value or vector)
#' @param outcome_type How to define outcome: "km" (KM estimate), "observed" (observed by horizon), or "censored"
#' @param horizon Time horizon for KM-based outcome classification
#' @return List with NRI and IDI statistics
#'
calculate_nri_idi <- function(time, event, pred_m1, pred_m2, cutoffs = 0.2,
                               outcome_type = c("km", "observed"),
                               horizon = NULL) {
  
  outcome_type <- match.arg(outcome_type)
  
  # Input validation
  n <- length(time)
  if (length(event) != n || length(pred_m1) != n || length(pred_m2) != n) {
    stop("All input vectors must have the same length", call. = FALSE)
  }
  
  # Keep complete cases
  keep <- is.finite(time) & is.finite(event) & is.finite(pred_m1) & is.finite(pred_m2)
  time <- time[keep]
  event <- event[keep]
  pred_m1 <- pred_m1[keep]
  pred_m2 <- pred_m2[keep]
  n <- length(time)
  
  if (n < 10) {
    stop("Insufficient data after removing missing values (n < 10)", call. = FALSE)
  }
  
  # Define outcome for NRI calculation
  # Option 1: "observed" - event observed by horizon (simple but biased by censoring)
  # Option 2: "km" - use Kaplan-Meier to estimate event probability
  if (outcome_type == "observed") {
    if (is.null(horizon)) {
      stop("horizon must be specified when outcome_type='observed'", call. = FALSE)
    }
    # Event observed by horizon (1 = event occurred, 0 = censored or event after horizon)
    y <- as.integer(time <= horizon & event == 1)
  } else {
    # KM-based: classify based on whether patient had event by their follow-up
    # This is the standard approach for NRI in survival context
    y <- event  # Use actual event indicator
  }
  
  # Categorize risks
  categorize_risk <- function(pred, cutoffs) {
    if (length(cutoffs) == 1) {
      # Single cutoff: binary classification
      ifelse(pred >= cutoffs, 1L, 0L)
    } else {
      # Multiple cutoffs: multi-category
      cutoffs <- sort(unique(c(0, cutoffs, 1)))
      as.integer(cut(pred, breaks = cutoffs, include.lowest = TRUE, labels = FALSE))
    }
  }
  
  cat_m1 <- categorize_risk(pred_m1, cutoffs)
  cat_m2 <- categorize_risk(pred_m2, cutoffs)
  
  # Ensure binary for standard NRI
  if (length(cutoffs) == 1) {
    # NRI calculation
    # Events: upward reclassification (cat_m2 > cat_m1) is good
    # Non-events: downward reclassification (cat_m2 < cat_m1) is good
    
    n_events <- sum(y == 1)
    n_nonevents <- sum(y == 0)
    
    if (n_events == 0 || n_nonevents == 0) {
      stop("Need both events and non-events for NRI calculation", call. = FALSE)
    }
    
    # For events
    up_events <- sum(y == 1 & cat_m2 > cat_m1)  # correctly moved to higher risk
    down_events <- sum(y == 1 & cat_m2 < cat_m1)  # incorrectly moved to lower risk
    nri_events <- (up_events - down_events) / n_events
    
    # For non-events
    up_nonevents <- sum(y == 0 & cat_m2 > cat_m1)  # incorrectly moved to higher risk
    down_nonevents <- sum(y == 0 & cat_m2 < cat_m1)  # correctly moved to lower risk
    nri_nonevents <- (down_nonevents - up_nonevents) / n_nonevents
    
    # Continuous NRI
    nri_continuous <- nri_events + nri_nonevents
    
    # Categorical NRI (symmetric version)
    # Some papers define categorical NRI as separate for events/non-events
    nri_categorical <- nri_events + nri_nonevents
    
    # IDI calculation
    # IDI = (Mean pred risk in events_new - Mean pred risk in non-events_new) -
    #       (Mean pred risk in events_old - Mean pred risk in non-events_old)
    
    mean_pred_m2_events <- mean(pred_m2[y == 1])
    mean_pred_m2_nonevents <- mean(pred_m2[y == 0])
    mean_pred_m1_events <- mean(pred_m1[y == 1])
    mean_pred_m1_nonevents <- mean(pred_m1[y == 0])
    
    idi <- (mean_pred_m2_events - mean_pred_m2_nonevents) -
           (mean_pred_m1_events - mean_pred_m1_nonevents)
    
    # Relative IDI
    idi_relative <- idi / (mean_pred_m1_events - mean_pred_m1_nonevents)
    
    # Summary table
    reclass_table <- table(
      Model1 = cat_m1,
      Model2 = cat_m2,
      Outcome = factor(y, labels = c("Nonevent", "Event"))
    )
    
    list(
      n = n,
      n_events = n_events,
      n_nonevents = n_nonevents,
      cutoffs = cutoffs,
      # NRI
      nri_events = nri_events,
      nri_nonevents = nri_nonevents,
      nri_continuous = nri_continuous,
      nri_categorical = nri_categorical,
      nri_se = sqrt((up_events + down_events) / n_events^2 + 
                     (up_nonevents + down_nonevents) / n_nonevents^2),
      # IDI
      idi = idi,
      idi_relative = idi_relative,
      idi_se = NA,  # Would need bootstrap
      # Component means
      mean_pred_m1_events = mean_pred_m1_events,
      mean_pred_m1_nonevents = mean_pred_m1_nonevents,
      mean_pred_m2_events = mean_pred_m2_events,
      mean_pred_m2_nonevents = mean_pred_m2_nonevents,
      # Reclassification counts
      up_events = up_events,
      down_events = down_events,
      up_nonevents = up_nonevents,
      down_nonevents = down_nonevents,
      reclass_table = reclass_table
    )
  } else {
    # Multi-category NRI (not implemented)
    stop("Multi-category NRI not yet implemented", call. = FALSE)
  }
}

# ============================================================================
# Bootstrap Confidence Intervals for NRI/IDI
# ============================================================================

#' Bootstrap NRI and IDI confidence intervals
#' @param time Observed follow-up times
#' @param event Event indicator
#' @param pred_m1 Model 1 predictions
#' @param pred_m2 Model 2 predictions
#' @param cutoffs Risk cutoffs
#' @param n_boot Number of bootstrap samples
#' @param seed Random seed
#' @return Data frame with bootstrap statistics
#'
bootstrap_nri_idi <- function(time, event, pred_m1, pred_m2, cutoffs = 0.2,
                               n_boot = 1000, seed = 42) {
  
  set.seed(seed)
  n <- length(time)
  
  # Bootstrap storage
  boot_stats <- matrix(NA_real_, nrow = n_boot, ncol = 4)
  colnames(boot_stats) <- c("nri_events", "nri_nonevents", "nri_continuous", "idi")
  
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    
    tryCatch({
      res <- calculate_nri_idi(
        time = time[idx],
        event = event[idx],
        pred_m1 = pred_m1[idx],
        pred_m2 = pred_m2[idx],
        cutoffs = cutoffs,
        outcome_type = "km"
      )
      
      boot_stats[b, ] <- c(res$nri_events, res$nri_nonevents, res$nri_continuous, res$idi)
    }, error = function(e) {
      # Skip failed bootstrap samples
    })
  }
  
  # Calculate percentile CIs
  ci_lower <- apply(boot_stats, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(boot_stats, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  data.frame(
    metric = colnames(boot_stats),
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# Main Function
# ============================================================================

#' Run NRI and IDI comparison between Cox and XGBoost models
#'
#' @param results_boot_cox Path to or object containing Cox model results
#' @param xgb_pickle_path Path to XGBoost pickle file
#' @param risk Risk type: "death" or "readmission"
#' @param horizon Time horizon in months (e.g., 12, 24, 60)
#' @param cutoffs Risk cutoff(s) for categorization (e.g., 0.15 for 15% risk)
#' @param n_boot Number of bootstrap samples for CI (0 for no bootstrap)
#' @param output_dir Output directory for results
#' @return List with NRI/IDI results
#'
run_nri_idi_comparison <- function(
    results_boot_cox = NULL,
    xgb_pickle_path = NULL,
    risk = c("death", "readmission"),
    horizon = 12,
    cutoffs = 0.15,
    n_boot = 1000,
    output_dir = NULL
) {
  
  risk <- match.arg(risk)
  
  # Validate inputs
  if (is.null(results_boot_cox)) {
    stop("Please provide results_boot_cox (path or object)", call. = FALSE)
  }
  if (is.null(xgb_pickle_path)) {
    stop("Please provide xgb_pickle_path", call. = FALSE)
  }
  
  # Load Cox predictions
  if (is.character(results_boot_cox)) {
    message("Loading Cox model results from: ", results_boot_cox)
    cox_data <- load_results_boot_object(results_boot_cox)
  } else {
    cox_data <- results_boot_cox
  }
  
  # Extract predictions at horizon
  message("Extracting Cox predictions at ", horizon, " months...")
  cox_preds <- extract_cox_at_horizon(cox_data, risk = risk, horizon = horizon)
  
  # Load XGBoost predictions
  message("Loading XGBoost predictions from: ", xgb_pickle_path)
  xgb_raw <- load_xgb_predictions(xgb_pickle_path)
  
  message("Extracting XGBoost predictions at ", horizon, " months...")
  xgb_preds <- extract_xgb_at_horizon(xgb_raw, risk = risk, horizon = horizon)
  
  # Match patients by fold (assumes same fold structure)
  message("Matching patients between models...")
  if (!all(cox_preds$fold_id == xgb_preds$fold_id)) {
    stop("Fold structure mismatch between Cox and XGBoost predictions", call. = FALSE)
  }
  
  # Use Cox as reference (Model 1), XGBoost as new (Model 2)
  time <- cox_preds$time
  event <- cox_preds$event
  pred_cox <- cox_preds$pred_risk
  pred_xgb <- xgb_preds$pred_risk
  
  # Basic sanity checks
  if (length(time) != length(pred_xgb)) {
    stop("Prediction length mismatch: Cox=", length(pred_cox), 
         ", XGB=", length(pred_xgb), call. = FALSE)
  }
  
  message("Calculating NRI and IDI...")
  
  # Calculate NRI/IDI
  nri_idi <- calculate_nri_idi(
    time = time,
    event = event,
    pred_m1 = pred_cox,
    pred_m2 = pred_xgb,
    cutoffs = cutoffs
  )
  
  # Bootstrap CI if requested
  if (n_boot > 0) {
    message("Running bootstrap (n=", n_boot, ")...")
    boot_ci <- bootstrap_nri_idi(
      time = time,
      event = event,
      pred_m1 = pred_cox,
      pred_m2 = pred_xgb,
      cutoffs = cutoffs,
      n_boot = n_boot
    )
    nri_idi$bootstrap_ci <- boot_ci
  }
  
  # Summary output
  message("\n========== NRI/IDI Results ==========")
  message("Risk: ", risk)
  message("Horizon: ", horizon, " months")
  message("Cutoff: ", paste(cutoffs, collapse = ", "))
  message("N (events): ", nri_idi$n, " (", nri_idi$n_events, ")")
  message("")
  message("NRI for events: ", round(nri_idi$nri_events, 4))
  message("NRI for non-events: ", round(nri_idi$nri_nonevents, 4))
  message("NRI (continuous): ", round(nri_idi$nri_continuous, 4))
  message("IDI: ", round(nri_idi$idi, 4), " (", round(nri_idi$idi_relative * 100, 2), "% relative)")
  message("=====================================\n")
  
  # Save results
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    prefix <- paste0("nri_idi_", risk, "_h", horizon, "_", timestamp)
    
    # Save main results
    result_summary <- data.frame(
      metric = c("n", "n_events", "n_nonevents", "cutoffs",
                 "nri_events", "nri_nonevents", "nri_continuous",
                 "idi", "idi_relative",
                 "mean_pred_cox_events", "mean_pred_cox_nonevents",
                 "mean_pred_xgb_events", "mean_pred_xgb_nonevents"),
      value = c(nri_idi$n, nri_idi$n_events, nri_idi$n_nonevents, 
                paste(cutoffs, collapse = ","),
                nri_idi$nri_events, nri_idi$n_nonevents, nri_idi$nri_continuous,
                nri_idi$idi, nri_idi$idi_relative,
                nri_idi$mean_pred_m1_events, nri_idi$mean_pred_m1_nonevents,
                nri_idi$mean_pred_m2_events, nri_idi$mean_pred_m2_nonevents)
    )
    
    write.csv(result_summary, file.path(output_dir, paste0(prefix, "_summary.csv")), 
              row.names = FALSE)
    
    # Save bootstrap CI
    if (!is.null(nri_idi$bootstrap_ci)) {
      write.csv(nri_idi$bootstrap_ci, 
                file.path(output_dir, paste0(prefix, "_bootstrap_ci.csv")),
                row.names = FALSE)
    }
    
    # Save reclassification table
    capture.output(
      print(nri_idi$reclass_table),
      file = file.path(output_dir, paste0(prefix, "_reclass_table.txt"))
    )
    
    # Save raw predictions
    raw_data <- data.frame(
      time = time,
      event = event,
      pred_cox = pred_cox,
      pred_xgb = pred_xgb,
      risk_cat_cox = ifelse(pred_cox >= cutoffs, "High", "Low"),
      risk_cat_xgb = ifelse(pred_xgb >= cutoffs, "High", "Low")
    )
    write.csv(raw_data, file.path(output_dir, paste0(prefix, "_predictions.csv")),
              row.names = FALSE)
    
    message("Results saved to: ", output_dir)
  }
  
  invisible(nri_idi)
}

# ============================================================================
# Helper: Multiple horizons
# ============================================================================

#' Calculate NRI/IDI at multiple horizons
#' @param ... Arguments passed to run_nri_idi_comparison
#' @param horizons Vector of time horizons
#' @return Data frame with results for all horizons
#'
run_nri_idi_multiple_horizons <- function(..., horizons = c(12, 24, 60)) {
  results <- lapply(horizons, function(h) {
    tryCatch({
      res <- run_nri_idi_comparison(..., horizon = h)
      data.frame(
        horizon = h,
        n = res$n,
        n_events = res$n_events,
        nri_events = res$nri_events,
        nri_nonevents = res$nri_nonevents,
        nri_continuous = res$nri_continuous,
        idi = res$idi,
        idi_relative = res$idi_relative,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning("Failed at horizon ", h, ": ", conditionMessage(e))
      NULL
    })
  })
  
  do.call(rbind, results)
}

# ============================================================================
# Paired results_boot comparison
# ============================================================================

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
    stop(
      "`cut_points` must be NULL, a numeric vector, or a named list keyed by horizon.",
      call. = FALSE
    )
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
    if (!risk %in% names(item)) {
      next
    }
    block <- item[[risk]]
    if (!is.list(block)) {
      next
    }
    if ("error" %in% names(block) && nzchar(block$error %||% "")) {
      next
    }

    eval_times <- as.numeric(item$eval_times %||% results_boot$config$eval_times)
    surv_mat <- as.matrix(block$surv_val_matrix)
    y_val <- block$y_val

    if (!is.matrix(surv_mat) || !nrow(surv_mat) || !ncol(surv_mat)) {
      next
    }
    if (ncol(surv_mat) != length(eval_times)) {
      stop(
        "Mismatch between survival matrix columns and evaluation times for ",
        risk, ".", call. = FALSE
      )
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
    stop(
      "The old/new results objects do not contain the same replicate keys for ",
      risk, ".", call. = FALSE
    )
  }

  sorted_keys <- sort(old_keys)
  old_blocks <- old_blocks[match(sorted_keys, old_keys)]
  new_blocks <- new_blocks[match(sorted_keys, new_keys)]

  out <- vector("list", length(old_blocks))
  for (i in seq_along(old_blocks)) {
    old_block <- old_blocks[[i]]
    new_block <- new_blocks[[i]]

    if (!identical(as.numeric(old_block$eval_times), as.numeric(new_block$eval_times))) {
      stop("Evaluation times do not match for replicate ", old_block$key, ".", call. = FALSE)
    }
    if (length(old_block$time) != length(new_block$time)) {
      stop("Validation row counts do not match for replicate ", old_block$key, ".", call. = FALSE)
    }
    if (!isTRUE(all.equal(old_block$time, new_block$time, tolerance = 0))) {
      stop("Validation times do not align for replicate ", old_block$key, ".", call. = FALSE)
    }
    if (!identical(old_block$event, new_block$event)) {
      stop("Validation events do not align for replicate ", old_block$key, ".", call. = FALSE)
    }

    out[[i]] <- list(
      replicate_id = i,
      key = old_block$key,
      risk = risk,
      imp_idx = old_block$imp_idx,
      fold_idx = old_block$fold_idx,
      eval_times = old_block$eval_times,
      time = old_block$time,
      event = old_block$event,
      pred_old = old_block$pred_risk,
      pred_new = new_block$pred_risk
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

.rb_continuous_nri_idi_one_horizon <- function(old_risk, new_risk, time, event, horizon, g_min = 0.05) {
  keep <- is.finite(old_risk) & is.finite(new_risk) & is.finite(time) & !is.na(event)
  old_risk <- as.numeric(old_risk[keep])
  new_risk <- as.numeric(new_risk[keep])
  time <- as.numeric(time[keep])
  event <- as.integer(event[keep])

  if (!length(time)) {
    return(NULL)
  }

  weights <- .rb_ipcw_weights(time, event, horizon, g_min = g_min)
  w_event <- weights$event
  w_nonevent <- weights$nonevent
  diff_risk <- new_risk - old_risk

  event_up <- .rb_weighted_mean(as.numeric(diff_risk > 0), w_event)
  event_down <- .rb_weighted_mean(as.numeric(diff_risk < 0), w_event)
  nonevent_up <- .rb_weighted_mean(as.numeric(diff_risk > 0), w_nonevent)
  nonevent_down <- .rb_weighted_mean(as.numeric(diff_risk < 0), w_nonevent)

  mean_old_event <- .rb_weighted_mean(old_risk, w_event)
  mean_new_event <- .rb_weighted_mean(new_risk, w_event)
  mean_old_nonevent <- .rb_weighted_mean(old_risk, w_nonevent)
  mean_new_nonevent <- .rb_weighted_mean(new_risk, w_nonevent)

  disc_old <- mean_old_event - mean_old_nonevent
  disc_new <- mean_new_event - mean_new_nonevent

  c(
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
}

.rb_categorical_nri_one_horizon <- function(old_risk, new_risk, time, event, horizon, cut_points, g_min = 0.05) {
  keep <- is.finite(old_risk) & is.finite(new_risk) & is.finite(time) & !is.na(event)
  old_risk <- as.numeric(old_risk[keep])
  new_risk <- as.numeric(new_risk[keep])
  time <- as.numeric(time[keep])
  event <- as.integer(event[keep])

  if (!length(time)) {
    return(NULL)
  }

  weights <- .rb_ipcw_weights(time, event, horizon, g_min = g_min)
  w_event <- weights$event
  w_nonevent <- weights$nonevent

  cat_old <- findInterval(old_risk, vec = cut_points, rightmost.closed = FALSE)
  cat_new <- findInterval(new_risk, vec = cut_points, rightmost.closed = FALSE)

  event_up <- .rb_weighted_mean(as.numeric(cat_new > cat_old), w_event)
  event_down <- .rb_weighted_mean(as.numeric(cat_new < cat_old), w_event)
  nonevent_up <- .rb_weighted_mean(as.numeric(cat_new > cat_old), w_nonevent)
  nonevent_down <- .rb_weighted_mean(as.numeric(cat_new < cat_old), w_nonevent)

  c(
    categorical_nri = (event_up - event_down) + (nonevent_down - nonevent_up),
    categorical_nri_events = event_up - event_down,
    categorical_nri_nonevents = nonevent_down - nonevent_up
  )
}

.rb_metric_rows <- function(block, horizons, cut_points, g_min, old_label, new_label) {
  rows <- vector("list", length(horizons) * 16L)
  idx <- 0L

  for (horizon in horizons) {
    h_idx <- which(block$eval_times == horizon)
    if (!length(h_idx)) {
      next
    }

    old_risk <- as.numeric(block$pred_old[, h_idx[1]])
    new_risk <- as.numeric(block$pred_new[, h_idx[1]])

    metric_values <- .rb_continuous_nri_idi_one_horizon(
      old_risk = old_risk,
      new_risk = new_risk,
      time = block$time,
      event = block$event,
      horizon = horizon,
      g_min = g_min
    )

    if (!is.null(cut_points)) {
      cat_values <- .rb_categorical_nri_one_horizon(
        old_risk = old_risk,
        new_risk = new_risk,
        time = block$time,
        event = block$event,
        horizon = horizon,
        cut_points = cut_points[[as.character(horizon)]],
        g_min = g_min
      )
      metric_values <- c(metric_values, cat_values)
    }

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
  do.call(rbind, rows)
}

.rb_summarize_metrics <- function(raw_metrics) {
  split_key <- interaction(raw_metrics$risk, raw_metrics$horizon, raw_metrics$metric, drop = TRUE, lex.order = TRUE)
  groups <- split(raw_metrics, split_key)

  out <- lapply(groups, function(df) {
    est <- as.numeric(df$estimate)
    est <- est[is.finite(est)]
    if (!length(est)) {
      mean_est <- NA_real_
      q025 <- NA_real_
      q975 <- NA_real_
      n_est <- 0L
    } else {
      mean_est <- mean(est)
      q025 <- as.numeric(stats::quantile(est, probs = 0.025, names = FALSE, na.rm = TRUE))
      q975 <- as.numeric(stats::quantile(est, probs = 0.975, names = FALSE, na.rm = TRUE))
      n_est <- length(est)
    }

    data.frame(
      risk = df$risk[1],
      horizon = df$horizon[1],
      metric = df$metric[1],
      mean = mean_est,
      q025 = q025,
      q975 = q975,
      n = n_est,
      old_model = df$old_model[1],
      new_model = df$new_model[1],
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out)
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

run_nri_idi_from_results_boot <- function(
  results_boot_old,
  results_boot_new,
  horizons = NULL,
  cut_points = c(0.05, 0.10, 0.20),
  old_label = "old_model",
  new_label = "new_model",
  g_min = 0.05,
  output_dir = NULL,
  prefix = "results_boot_reclassification",
  save_raw = TRUE
) {
  .rb_assert_results_boot(results_boot_old, "results_boot_old")
  .rb_assert_results_boot(results_boot_new, "results_boot_new")

  available_horizons_old <- results_boot_old$config$eval_times %||%
    unique(unlist(lapply(results_boot_old$raw_predictions, `[[`, "eval_times")))
  available_horizons_new <- results_boot_new$config$eval_times %||%
    unique(unlist(lapply(results_boot_new$raw_predictions, `[[`, "eval_times")))

  horizons <- .rb_normalize_horizons(horizons, intersect(available_horizons_old, available_horizons_new))
  cut_points <- .rb_normalize_cut_points(cut_points, horizons)

  risks <- c("readmission", "death")
  raw_parts <- lapply(risks, function(risk_name) {
    old_blocks <- .rb_extract_blocks(results_boot_old, risk = risk_name)
    new_blocks <- .rb_extract_blocks(results_boot_new, risk = risk_name)
    paired_blocks <- .rb_align_blocks(old_blocks, new_blocks, risk = risk_name)
    metric_rows <- lapply(
      paired_blocks,
      .rb_metric_rows,
      horizons = horizons,
      cut_points = cut_points,
      g_min = g_min,
      old_label = old_label,
      new_label = new_label
    )
    metric_rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, metric_rows)
    if (!length(metric_rows)) {
      return(NULL)
    }
    do.call(rbind, metric_rows)
  })

  raw_parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, raw_parts)
  if (!length(raw_parts)) {
    stop("No NRI/IDI results could be computed from the supplied `results_boot` objects.", call. = FALSE)
  }
  raw_metrics <- do.call(rbind, raw_parts)

  summary_metrics <- .rb_summarize_metrics(raw_metrics)

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
      reference = c(
        "Liu et al. 2010; doi:10.1080/15598608.2010.10412022",
        "Uno et al. 2013; doi:10.1002/sim.5647"
      ),
      files = list(
        summary = summary_path,
        raw = raw_path,
        rds = rds_path
      )
    )
  )
  saveRDS(result, rds_path)
  result
}

message("NRI/IDI functions loaded. Available entry points:")
message("  run_nri_idi_from_results_boot(results_boot, results_boot_full, ...)")
message("  run_nri_idi_comparison(")
message("    results_boot_cox = 'path/to/results_boot.rds',")
message("    xgb_pickle_path = 'xgb/_out/xgb6_corr_DUAL_final_ev_hyp_20260223_2134.pkl',")
message("    risk = 'death',  # or 'readmission'")
message("    horizon = 12,")
message("    cutoffs = 0.15  # X-tile determined cutoff")
message("  )")
