# =============================================================================
# imputation_leakage_sensitivity.R
# Reproducible sensitivity analysis for outcome-informed, pre-split imputation.
#
# This file defines functions only. The companion notebook orchestrates them:
#   cons/prediction23_mortality_imputation_leakage_sensitivity.ipynb
#
# No completed patient-level dataset is written to disk. Summaries are saved
# under data/20241015_out/leakage_sensitivity, never under cons/.
# =============================================================================

leakage_required_packages <- function(full = FALSE) {
  pkgs <- c("nanoparquet", "survival", "dplyr", "janitor", "pec")
  if (isTRUE(full)) pkgs <- c(pkgs, "missRanger")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required R packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(pkgs)
}

leakage_project_root <- function(start = getwd()) {
  candidates <- unique(c(
    normalizePath(start, winslash = "/", mustWork = FALSE),
    normalizePath(file.path(start, ".."), winslash = "/", mustWork = FALSE)
  ))
  hit <- candidates[vapply(
    candidates,
    function(x) file.exists(file.path(x, "cons", "_alt_scripts", "val_holdout_02_build_sets.R")),
    logical(1)
  )]
  if (!length(hit)) {
    stop("Could not resolve the SISTRAT 2023 project root.", call. = FALSE)
  }
  hit[[1L]]
}

leakage_source_project_engines <- function(project_root) {
  scripts <- c(
    "cons/_alt_scripts/val_holdout_02_build_sets.R",
    "cons/_alt_scripts/validate_holdout_metrics.R",
    "cons/_alt_scripts/evaluate_dual_cox_holdout_dualscore.R",
    "cons/_alt_scripts/dca_from_results_boot_for_metrics.R"
  )
  paths <- file.path(project_root, scripts)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing project scripts: ", paste(missing, collapse = ", "), call. = FALSE)
  for (path in paths) source(path, local = .GlobalEnv)
  invisible(paths)
}

leakage_to01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  out <- suppressWarnings(as.integer(x))
  values <- sort(unique(out[!is.na(out)]))
  if (all(values %in% c(0L, 1L))) return(out)
  if (all(values %in% c(1L, 2L))) return(out - 1L)
  stop("Event variable is not encoded as 0/1 or 1/2.", call. = FALSE)
}

leakage_find_validation_rds <- function(data_out) {
  discovered <- list.files(
    data_out,
    pattern = "^pred23_holdout_validation_.*\\.rds$",
    full.names = TRUE
  )
  if (!length(discovered)) stop("No prediction23 held-out validation RDS was found.", call. = FALSE)
  preferred <- file.path(data_out, "pred23_holdout_validation_2026_07_17.rds")
  info <- file.info(discovered)
  discovered <- discovered[order(info$mtime, decreasing = TRUE)]
  candidates <- unique(c(preferred[file.exists(preferred)], discovered))
  for (path in candidates) {
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    valid <- is.list(obj) && is.list(obj$results_boot_val) &&
      all(c("best_perf1", "best_perf2") %in% names(obj$results_boot_val)) &&
      is.list(obj$models)
    if (isTRUE(valid)) return(list(path = path, object = obj))
  }
  stop("No valid prediction23 held-out validation list was readable.", call. = FALSE)
}

leakage_raw_predictors_for_formula <- function(formula, raw_names) {
  terms <- setdiff(
    all.vars(formula),
    c(
      "death_time_from_disch_m", "death_event",
      "readmit_time_from_disch_m", "readmit_event"
    )
  )
  map_one <- function(term) {
    if (term %in% raw_names) return(term)
    if (identical(term, "plan_type_strata") && "plan_type_corr" %in% raw_names) {
      return("plan_type_corr")
    }
    candidates <- raw_names[startsWith(term, paste0(raw_names, "_"))]
    if (!length(candidates)) return(NA_character_)
    candidates[[which.max(nchar(candidates))]]
  }
  unique(stats::na.omit(vapply(terms, map_one, character(1))))
}

leakage_load_inputs <- function(project_root = leakage_project_root()) {
  leakage_required_packages(full = FALSE)

  data_out <- file.path(project_root, "data", "20241015_out")
  raw_path <- file.path(data_out, "pred1_ndp_2026_01_05.Rdata")
  split_path <- file.path(project_root, "cons", "_out", "comb_split_seed2125_test20_mar26.parquet")
  center_path <- file.path(data_out, "pred1", "id_centro.Rds")
  schema_path <- file.path(data_out, "corrected_datasets_nondum_filt_imp1.parquet")
  required_files <- c(raw_path, split_path, center_path, schema_path)
  if (!all(file.exists(required_files))) {
    stop("Missing required input files: ", paste(required_files[!file.exists(required_files)], collapse = ", "), call. = FALSE)
  }

  raw_env <- new.env(parent = baseenv())
  load(raw_path, envir = raw_env)
  if (!exists("df_pred", envir = raw_env, inherits = FALSE)) {
    stop("df_pred is absent from the prediction1 RData file.", call. = FALSE)
  }
  raw_all <- as.data.frame(get("df_pred", envir = raw_env, inherits = FALSE))
  center_all <- readRDS(center_path)
  if (length(center_all) != nrow(raw_all)) stop("center_id is not row-aligned to df_pred.", call. = FALSE)

  death_event_all <- leakage_to01(raw_all$death_event)
  tr_outcome_chr <- as.character(raw_all$tr_outcome)
  drop_artifact <- grepl("adm reasons", tr_outcome_chr, ignore.case = TRUE) &
    death_event_all == 1L & as.numeric(raw_all$death_time_from_disch_m) <= 0.23
  drop_other <- grepl("other", tr_outcome_chr, ignore.case = TRUE)
  keep <- !(drop_artifact | drop_other)

  raw <- raw_all[keep, , drop = FALSE]
  center_id <- center_all[keep]
  rownames(raw) <- NULL
  split <- as.data.frame(nanoparquet::read_parquet(split_path))
  schema <- as.data.frame(nanoparquet::read_parquet(schema_path))

  if (nrow(raw) != nrow(split) || nrow(raw) != nrow(schema)) {
    stop(sprintf(
      "Row-count mismatch after canonical exclusions: raw=%d, split=%d, schema=%d.",
      nrow(raw), nrow(split), nrow(schema)
    ), call. = FALSE)
  }
  if (!all(c("is_train", "death_time_from_disch_m") %in% names(split))) {
    stop("The split file lacks is_train or death_time_from_disch_m.", call. = FALSE)
  }
  align_diff <- abs(round(as.numeric(raw$death_time_from_disch_m), 2) -
    as.numeric(split$death_time_from_disch_m))
  if (mean(align_diff > 0.01, na.rm = TRUE) > 0.001) {
    stop("The filtered raw data are not positionally aligned to the canonical split.", call. = FALSE)
  }

  validation <- leakage_find_validation_rds(data_out)
  formulas <- lapply(validation$object$models, function(x) {
    list(readmit = stats::as.formula(x$readmit), death = stats::as.formula(x$death))
  })

  endpoint_cols <- c(
    "readmit_time_from_disch_m", "readmit_event",
    "death_time_from_disch_m", "death_event"
  )
  outcome_time_cols <- c("readmit_time_from_adm_m", "death_time_from_adm_m")
  leakage_cols <- c(endpoint_cols, outcome_time_cols)
  canonical_nondum_cols <- names(schema)
  retained_input_cols <- intersect(
    setdiff(canonical_nondum_cols, c(endpoint_cols, "center_id")),
    names(raw)
  )
  validation_idx <- which(!as.logical(split$is_train))
  missing_count <- rowSums(is.na(raw[validation_idx, retained_input_cols, drop = FALSE]))
  missing_mask <- missing_count > 0L
  model_raw_predictors <- lapply(formulas, function(x) {
    leakage_raw_predictors_for_formula(x$death, names(raw))
  })
  expected_predictor_counts <- c(best_perf1 = 36L, best_perf2 = 8L)
  observed_predictor_counts <- vapply(
    model_raw_predictors[names(expected_predictor_counts)],
    length,
    integer(1)
  )
  if (!identical(observed_predictor_counts, expected_predictor_counts)) {
    stop("The frozen mortality formulas no longer map to the expected raw predictors.", call. = FALSE)
  }
  expected_strata <- list(
    best_perf1 = c(
      "strata(plan_type_strata)",
      "strata(tr_outcome_adm_discharge_rule_violation_undet)"
    ),
    best_perf2 = c("strata(plan_type_strata)", "strata(any_phys_dx)")
  )
  for (model in names(expected_strata)) {
    labels <- attr(stats::terms(formulas[[model]]$death), "term.labels")
    observed_strata <- grep("^strata\\(", labels, value = TRUE)
    if (!setequal(observed_strata, expected_strata[[model]])) {
      stop("The frozen mortality strata changed for ", model, ".", call. = FALSE)
    }
  }
  model_missing_counts <- lapply(model_raw_predictors, function(columns) {
    rowSums(is.na(raw[validation_idx, columns, drop = FALSE]))
  })
  model_missing_masks <- lapply(model_missing_counts, function(x) x > 0L)

  death_val <- leakage_to01(raw$death_event[validation_idx])
  model_exposure_audit <- do.call(rbind, lapply(names(model_missing_masks), function(model) {
    mask <- model_missing_masks[[model]]
    data.frame(
      model = model,
      n_model_raw_predictors = length(model_raw_predictors[[model]]),
      validation_rows = length(mask),
      rows_with_any_model_predictor_missing = sum(mask),
      percent_with_any_model_predictor_missing = 100 * mean(mask),
      deaths_in_exposed_rows = sum(death_val[mask] == 1L, na.rm = TRUE),
      percent_of_all_deaths_in_exposed_rows = 100 * sum(death_val[mask] == 1L, na.rm = TRUE) /
        sum(death_val == 1L, na.rm = TRUE),
      missing_cells = sum(model_missing_counts[[model]]),
      percent_missing_cells = 100 * sum(model_missing_counts[[model]]) /
        (length(mask) * length(model_raw_predictors[[model]])),
      stringsAsFactors = FALSE
    )
  }))

  audit <- data.frame(
    quantity = c(
      "raw_rows_before_exclusions", "excluded_administrative_artifacts",
      "excluded_other_outcome", "analysis_rows", "development_rows",
      "validation_rows", "validation_any_missing_predictor",
      "validation_complete_predictors"
    ),
    value = c(
      nrow(raw_all), sum(drop_artifact), sum(drop_other & !drop_artifact), nrow(raw),
      sum(as.logical(split$is_train)), length(validation_idx), sum(missing_mask), sum(!missing_mask)
    ),
    stringsAsFactors = FALSE
  )

  list(
    project_root = project_root,
    data_out = data_out,
    # "leakage_sensitivity" is left in place: its Google Drive placeholder is stuck
    # (ERROR_NO_SYSTEM_RESOURCES on every read/write/delete attempt), confirmed unrelated
    # to any other file being synced. Do not reuse that folder name.
    output_dir = file.path(data_out, "leakage_sensitivity_v2"),
    raw = raw,
    center_id = center_id,
    split = split,
    train_idx = which(as.logical(split$is_train)),
    validation_idx = validation_idx,
    missing_mask = missing_mask,
    missing_count = missing_count,
    model_raw_predictors = model_raw_predictors,
    model_missing_masks = model_missing_masks,
    model_missing_counts = model_missing_counts,
    model_exposure_audit = model_exposure_audit,
    retained_input_cols = retained_input_cols,
    endpoint_cols = endpoint_cols,
    outcome_time_cols = outcome_time_cols,
    leakage_cols = leakage_cols,
    mortality_leakage_cols = intersect(
      c("death_time_from_adm_m", "death_time_from_disch_m", "death_event"), names(raw)
    ),
    readmission_leakage_cols = intersect(
      c("readmit_time_from_adm_m", "readmit_time_from_disch_m", "readmit_event"), names(raw)
    ),
    canonical_nondum_cols = canonical_nondum_cols,
    validation_object = validation$object,
    validation_path = validation$path,
    formulas = formulas,
    audit = audit,
    paths = list(raw = raw_path, split = split_path, center = center_path, schema = schema_path)
  )
}

leakage_pool_original_predictions <- function(
    results_boot, outcome = "death", n_imputations = NULL
) {
  blocks <- results_boot$raw_predictions
  if (!length(blocks)) stop("results_boot has no raw_predictions.", call. = FALSE)
  if (!is.null(n_imputations)) {
    n_imputations <- as.integer(n_imputations)
    if (n_imputations < 1L || n_imputations > length(blocks)) {
      stop("Invalid n_imputations for original predictions.", call. = FALSE)
    }
    blocks <- blocks[seq_len(n_imputations)]
  }
  valid_block <- vapply(blocks, function(block) {
    is.list(block) && outcome %in% names(block) && is.list(block[[outcome]]) &&
      is.null(block[[outcome]]$error) &&
      is.matrix(as.matrix(block[[outcome]]$surv_val_matrix)) &&
      all(c("time", "event") %in% names(block[[outcome]]$y_val)) &&
      all(c("time", "event") %in% names(block[[outcome]]$y_train))
  }, logical(1))
  if (!all(valid_block)) {
    stop("One or more original prediction blocks are invalid for ", outcome, ".", call. = FALSE)
  }
  eval_times <- as.numeric(blocks[[1L]]$eval_times)
  expected_n <- nrow(as.matrix(blocks[[1L]][[outcome]]$surv_val_matrix))
  for (block in blocks) {
    block_times <- as.numeric(block$eval_times)
    survival_matrix <- as.matrix(block[[outcome]]$surv_val_matrix)
    if (!identical(block_times, eval_times) ||
        !identical(dim(survival_matrix), c(expected_n, length(eval_times)))) {
      stop("Original prediction blocks have inconsistent horizons or dimensions.", call. = FALSE)
    }
    risk_matrix <- 1 - survival_matrix
    if (any(!is.finite(risk_matrix)) || any(risk_matrix < 0 | risk_matrix > 1)) {
      stop("Original prediction risks are nonfinite or outside [0, 1].", call. = FALSE)
    }
    if (nrow(block[[outcome]]$y_val) != expected_n) {
      stop("Original outcomes and predictions are not row-aligned.", call. = FALSE)
    }
    first <- blocks[[1L]][[outcome]]
    if (!identical(as.numeric(block[[outcome]]$y_val$time), as.numeric(first$y_val$time)) ||
        !identical(leakage_to01(block[[outcome]]$y_val$event), leakage_to01(first$y_val$event)) ||
        !identical(as.numeric(block[[outcome]]$y_train$time), as.numeric(first$y_train$time)) ||
        !identical(leakage_to01(block[[outcome]]$y_train$event), leakage_to01(first$y_train$event))) {
      stop("Original outcomes differ across imputations.", call. = FALSE)
    }
  }
  risk_array <- simplify2array(lapply(blocks, function(block) {
    1 - as.matrix(block[[outcome]]$surv_val_matrix)
  }))
  if (length(dim(risk_array)) == 2L) risk_array <- array(risk_array, dim = c(dim(risk_array), 1L))
  pooled_risk <- apply(risk_array, c(1L, 2L), mean, na.rm = TRUE)
  list(
    risk = pooled_risk,
    eval_times = eval_times,
    time = as.numeric(blocks[[1L]][[outcome]]$y_val$time),
    event = leakage_to01(blocks[[1L]][[outcome]]$y_val$event),
    train_time = as.numeric(blocks[[1L]][[outcome]]$y_train$time),
    train_event = leakage_to01(blocks[[1L]][[outcome]]$y_train$event),
    n_imputations = length(blocks)
  )
}

leakage_km_risk <- function(time, event, horizon) {
  keep <- is.finite(time) & !is.na(event)
  if (!any(keep)) return(NA_real_)
  fit <- survival::survfit(survival::Surv(time[keep], event[keep]) ~ 1)
  out <- summary(fit, times = horizon, extend = TRUE)$surv
  if (!length(out) || !is.finite(out[[1L]])) return(NA_real_)
  1 - out[[1L]]
}

leakage_cindex <- function(time, event, risk, horizon = NULL) {
  keep <- is.finite(time) & !is.na(event) & is.finite(risk)
  if (sum(keep) < 10L || sum(event[keep] == 1L) < 2L) return(NA_real_)
  dat <- data.frame(time = time[keep], event = event[keep], risk = risk[keep])
  fit <- tryCatch(
    survival::concordance(
      survival::Surv(time, event) ~ risk,
      data = dat,
      reverse = TRUE,
      timewt = "n/G2",
      ymax = if (is.null(horizon)) Inf else horizon
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) NA_real_ else as.numeric(fit$concordance)
}

leakage_censoring_fit <- function(train_time, train_event) {
  survival::survfit(survival::Surv(train_time, 1L - train_event) ~ 1)
}

leakage_censoring_survival <- function(censoring_fit, times, floor = 0.05) {
  requested_times <- pmax(as.numeric(times), 0)
  unique_times <- sort(unique(requested_times))
  survival_unique <- as.numeric(
    summary(censoring_fit, times = unique_times, extend = TRUE)$surv
  )
  if (length(survival_unique) != length(unique_times)) {
    stop("Censoring survival lookup returned unexpected dimensions.", call. = FALSE)
  }
  matched <- match(requested_times, unique_times)
  if (anyNA(matched)) stop("Censoring survival lookup failed to restore order.", call. = FALSE)
  pmax(survival_unique[matched], floor)
}

leakage_brier <- function(
    train_time, train_event, time, event, risk, horizon,
    censoring_fit = NULL, eps = 1e-8, floor = 0.05
) {
  keep <- is.finite(time) & !is.na(event) & is.finite(risk)
  time <- time[keep]; event <- event[keep]; risk <- risk[keep]
  if (!length(time)) return(NA_real_)
  if (is.null(censoring_fit)) censoring_fit <- leakage_censoring_fit(train_time, train_event)
  g_event <- leakage_censoring_survival(censoring_fit, pmax(time - eps, 0), floor)
  g_horizon <- leakage_censoring_survival(censoring_fit, rep(horizon, length(time)), floor)
  survival_prediction <- 1 - risk
  alive <- as.numeric(time > horizon)
  weights <- ifelse(
    time <= horizon & event == 1L,
    1 / g_event,
    ifelse(time > horizon, 1 / g_horizon, 0)
  )
  mean(weights * (alive - survival_prediction)^2, na.rm = TRUE)
}

leakage_ibs <- function(train_time, train_event, time, event, risk_matrix, eval_times) {
  censoring_fit <- leakage_censoring_fit(train_time, train_event)
  bs <- vapply(seq_along(eval_times), function(j) {
    leakage_brier(
      train_time, train_event, time, event, risk_matrix[, j], eval_times[[j]],
      censoring_fit = censoring_fit
    )
  }, numeric(1))
  denom <- max(eval_times) - min(eval_times)
  if (!is.finite(denom) || denom <= 0 || any(!is.finite(bs))) return(NA_real_)
  sum(diff(eval_times) * (head(bs, -1L) + tail(bs, -1L)) / 2) / denom
}

leakage_dca_point <- function(time, event, risk, horizon, threshold) {
  keep <- is.finite(time) & !is.na(event) & is.finite(risk)
  time <- time[keep]; event <- event[keep]; risk <- risk[keep]
  n <- length(time)
  if (!n) return(data.frame())
  positive <- risk >= threshold
  positive_rate <- mean(positive)
  all_risk <- leakage_km_risk(time, event, horizon)
  selected_risk <- if (any(positive)) leakage_km_risk(time[positive], event[positive], horizon) else 0
  odds <- threshold / (1 - threshold)
  nb_model <- positive_rate * (selected_risk - (1 - selected_risk) * odds)
  nb_all <- all_risk - (1 - all_risk) * odds
  data.frame(
    horizon = horizon,
    threshold = threshold,
    n = n,
    positive_rate = positive_rate,
    observed_risk = all_risk,
    observed_risk_positive = selected_risk,
    net_benefit = nb_model,
    net_cases_per1000 = 1000 * nb_model,
    net_benefit_vs_all = nb_model - nb_all,
    avoided_per1000_vs_all = (nb_model - nb_all) / odds * 1000,
    stringsAsFactors = FALSE
  )
}

leakage_default_thresholds <- function() {
  list(
    `6` = c(0.005, 0.010),
    `12` = c(0.005, 0.010, 0.015),
    `36` = seq(0.015, 0.055, by = 0.005),
    `60` = seq(0.025, 0.085, by = 0.005)
  )
}

leakage_metric_table <- function(pool, group, model, scenario = "original") {
  group <- as.character(group)
  groups <- unique(c("all", sort(unique(group))))
  censoring_fit <- leakage_censoring_fit(pool$train_time, pool$train_event)
  out <- list(); k <- 0L
  for (label in groups) {
    idx <- if (identical(label, "all")) seq_along(group) else which(group == label)
    for (j in seq_along(pool$eval_times)) {
      horizon <- pool$eval_times[[j]]
      k <- k + 1L
      observed <- leakage_km_risk(pool$time[idx], pool$event[idx], horizon)
      predicted <- mean(pool$risk[idx, j], na.rm = TRUE)
      out[[k]] <- data.frame(
        scenario = scenario,
        model = model,
        group = label,
        horizon = horizon,
        n = length(idx),
        deaths_by_horizon = sum(pool$event[idx] == 1L & pool$time[idx] <= horizon, na.rm = TRUE),
        observed_risk = observed,
        mean_predicted_risk = predicted,
        predicted_observed_ratio = predicted / observed,
        uno_c = leakage_cindex(pool$time[idx], pool$event[idx], pool$risk[idx, j], horizon),
        brier = leakage_brier(
          pool$train_time, pool$train_event,
          pool$time[idx], pool$event[idx], pool$risk[idx, j], horizon,
          censoring_fit = censoring_fit
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

leakage_dca_table <- function(
    pool, group, model, scenario = "original",
    thresholds = leakage_default_thresholds()
) {
  group <- as.character(group)
  groups <- unique(c("all", sort(unique(group))))
  out <- list(); k <- 0L
  for (label in groups) {
    idx <- if (identical(label, "all")) seq_along(group) else which(group == label)
    for (horizon_name in names(thresholds)) {
      horizon <- as.numeric(horizon_name)
      j <- match(horizon, pool$eval_times)
      if (is.na(j)) next
      for (threshold in thresholds[[horizon_name]]) {
        k <- k + 1L
        row <- leakage_dca_point(
          pool$time[idx], pool$event[idx], pool$risk[idx, j], horizon, threshold
        )
        row$scenario <- scenario
        row$model <- model
        row$group <- label
        out[[k]] <- row
      }
    }
  }
  do.call(rbind, out)
}

leakage_quick_diagnostic <- function(inputs, thresholds = leakage_default_thresholds()) {
  metrics <- list(); dca <- list(); pools <- list()
  for (model in c("best_perf1", "best_perf2")) {
    group <- ifelse(
      inputs$model_missing_masks[[model]],
      "any_model_predictor_imputed",
      "complete_model_predictors"
    )
    pool <- leakage_pool_original_predictions(
      inputs$validation_object$results_boot_val[[model]], outcome = "death"
    )
    pools[[model]] <- pool
    metrics[[model]] <- leakage_metric_table(pool, group, model, scenario = "original")
    dca[[model]] <- leakage_dca_table(pool, group, model, scenario = "original", thresholds = thresholds)
  }
  list(
    metrics = do.call(rbind, metrics),
    dca = do.call(rbind, dca),
    pools = pools,
    warning = paste(
      "Complete-versus-imputed subgroup contrasts are diagnostic only.",
      "They mix leakage with case-mix and missingness-pattern differences."
    )
  )
}

leakage_missranger_fit <- function(data, seed, config) {
  missRanger::missRanger(
    data = as.data.frame(data),
    formula = stats::as.formula(". ~ ."),
    pmm.k = config$pmm_k,
    num.trees = config$num_trees,
    maxiter = config$maxiter,
    seed = seed,
    num.threads = config$num_threads,
    keep_forests = TRUE,
    data_only = FALSE,
    returnOOB = TRUE,
    verbose = config$verbose,
    respect.unordered.factors = "order"
  )
}

leakage_prepare_nondum <- function(completed, truth, center_id, canonical_cols) {
  out <- as.data.frame(completed)
  # Outcomes are always restored before evaluation. In permutation analyses,
  # the permuted outcome is visible only to the imputer.
  restore <- intersect(
    c(
      "readmit_time_from_adm_m", "death_time_from_adm_m",
      "readmit_time_from_disch_m", "readmit_event",
      "death_time_from_disch_m", "death_event"
    ),
    names(truth)
  )
  # Clean imputers never receive these columns. Append them only after
  # imputation; leaky/permutation arms overwrite their temporary copies here.
  for (column in restore) out[[column]] <- truth[[column]]
  out$readmit_event <- leakage_to01(out$readmit_event)
  out$death_event <- leakage_to01(out$death_event)
  readmit_time <- as.numeric(out$readmit_time_from_disch_m)
  death_time <- as.numeric(out$death_time_from_disch_m)
  competing <- out$death_event == 1L & death_time <= readmit_time
  out$readmit_time_from_disch_m[competing] <- death_time[competing]
  out$readmit_event[competing] <- 0L
  out$center_id <- factor(center_id)
  missing <- setdiff(canonical_cols, names(out))
  if (length(missing)) stop("Completed data lack canonical columns: ", paste(missing, collapse = ", "), call. = FALSE)
  out[, canonical_cols, drop = FALSE]
}

leakage_to_model_frame <- function(nondum, is_train) {
  if (!exists(".vh_preprocess_one", mode = "function") || !exists(".vh_finalize", mode = "function")) {
    stop("Source val_holdout_02_build_sets.R before preprocessing.", call. = FALSE)
  }
  flag <- rep(isTRUE(is_train), nrow(nondum))
  proc <- .vh_preprocess_one(
    nondum,
    split_info = data.frame(is_train = flag),
    keep_train = isTRUE(is_train)
  )
  .vh_finalize(proc, .vh_all_required_cols)
}

leakage_align_model_frames <- function(train, validation) {
  if (!identical(names(train), names(validation))) {
    stop("Development and validation model frames have different columns.", call. = FALSE)
  }
  factor_cols <- names(train)[vapply(train, is.factor, logical(1))]
  for (column in factor_cols) {
    validation[[column]] <- factor(as.character(validation[[column]]), levels = levels(train[[column]]))
    if (anyNA(validation[[column]])) {
      stop("Unseen validation level in ", column, ".", call. = FALSE)
    }
  }
  list(train = train, validation = validation)
}

leakage_fit_death_model <- function(formula, train, validation, eval_times) {
  aligned <- leakage_align_model_frames(train, validation)
  formula_env <- new.env(parent = environment(formula))
  formula_env$Surv <- survival::Surv
  formula_env$strata <- survival::strata
  environment(formula) <- formula_env
  fit <- survival::coxph(
    formula,
    data = aligned$train,
    ties = "efron",
    x = TRUE,
    y = TRUE,
    model = TRUE
  )
  survival_matrix <- pec::predictSurvProb(fit, newdata = aligned$validation, times = eval_times)
  survival_matrix <- as.matrix(survival_matrix)
  if (nrow(survival_matrix) != nrow(aligned$validation)) survival_matrix <- t(survival_matrix)
  if (!identical(dim(survival_matrix), c(nrow(aligned$validation), length(eval_times)))) {
    stop("Unexpected survival prediction dimensions.", call. = FALSE)
  }
  risk_matrix <- 1 - survival_matrix
  if (any(!is.finite(risk_matrix)) || any(risk_matrix < 0 | risk_matrix > 1)) {
    stop("Survival predictions produced invalid risks.", call. = FALSE)
  }
  list(
    fit = fit,
    risk = risk_matrix,
    lp = as.numeric(stats::predict(fit, newdata = aligned$validation, type = "lp"))
  )
}

leakage_predict_death_model <- function(fit, train, validation, eval_times) {
  aligned <- leakage_align_model_frames(train, validation)
  survival_matrix <- pec::predictSurvProb(fit, newdata = aligned$validation, times = eval_times)
  survival_matrix <- as.matrix(survival_matrix)
  if (nrow(survival_matrix) != nrow(aligned$validation)) survival_matrix <- t(survival_matrix)
  if (!identical(dim(survival_matrix), c(nrow(aligned$validation), length(eval_times)))) {
    stop("Unexpected survival prediction dimensions.", call. = FALSE)
  }
  risk_matrix <- 1 - survival_matrix
  if (any(!is.finite(risk_matrix)) || any(risk_matrix < 0 | risk_matrix > 1)) {
    stop("Survival predictions produced invalid risks.", call. = FALSE)
  }
  risk_matrix
}

leakage_make_null_formula <- function(formula, type = c("marginal", "structure")) {
  type <- match.arg(type)
  lhs <- paste(deparse(formula[[2L]]), collapse = " ")
  rhs <- "1"
  if (identical(type, "structure")) {
    term_labels <- attr(stats::terms(formula), "term.labels")
    strata_terms <- grep("^strata\\(", term_labels, value = TRUE)
    if (length(strata_terms)) rhs <- paste(strata_terms, collapse = " + ")
  }
  stats::as.formula(
    paste(lhs, "~", rhs),
    env = environment(formula)
  )
}

leakage_model_skill_specs <- function(inputs) {
  readmit_1 <- paste(deparse(inputs$formulas$best_perf1$readmit), collapse = " ")
  readmit_2 <- paste(deparse(inputs$formulas$best_perf2$readmit), collapse = " ")
  if (!identical(readmit_1, readmit_2)) {
    stop(
      "The two registry entries do not share the same readmission formula; evaluate them separately.",
      call. = FALSE
    )
  }
  list(
    mortality_full_ph = list(
      label = "Mortality Full PH",
      outcome = "death",
      registry_model = "best_perf1",
      formula = inputs$formulas$best_perf1$death,
      estimand = "all-cause mortality risk"
    ),
    mortality_shap_rule2 = list(
      label = "Mortality SHAP rule2",
      outcome = "death",
      registry_model = "best_perf2",
      formula = inputs$formulas$best_perf2$death,
      estimand = "all-cause mortality risk"
    ),
    readmission = list(
      label = "Readmission shared model",
      outcome = "readmit",
      registry_model = "best_perf1",
      formula = inputs$formulas$best_perf1$readmit,
      estimand = "cause-specific net readmission risk"
    )
  )
}

leakage_initialize_skill_sums <- function(specs, n, n_h) {
  lapply(specs, function(x) {
    list(
      model = matrix(0, nrow = n, ncol = n_h),
      marginal_null = matrix(0, nrow = n, ncol = n_h),
      structure_null = matrix(0, nrow = n, ncol = n_h)
    )
  })
}

leakage_skill_strata_event_audit <- function(data, spec, eval_times, imputation) {
  structure_formula <- leakage_make_null_formula(spec$formula, "structure")
  outcome_variables <- all.vars(spec$formula[[2L]])
  if (length(outcome_variables) != 2L) {
    stop("The survival outcome must contain exactly time and event variables.", call. = FALSE)
  }
  strata_variables <- setdiff(all.vars(structure_formula), outcome_variables)
  if (length(strata_variables)) {
    missing <- setdiff(strata_variables, names(data))
    if (length(missing)) {
      stop("Structural-null strata are absent from the model frame.", call. = FALSE)
    }
    stratum <- interaction(
      data[strata_variables],
      drop = TRUE,
      lex.order = TRUE,
      sep = " | "
    )
  } else {
    stratum <- factor(rep("marginal", nrow(data)))
  }
  time <- as.numeric(data[[outcome_variables[[1L]]]])
  event <- leakage_to01(data[[outcome_variables[[2L]]]])
  out <- list()
  k <- 0L
  for (level in levels(stratum)) {
    index <- which(stratum == level)
    for (horizon in eval_times) {
      k <- k + 1L
      event_count <- sum(event[index] == 1L & time[index] <= horizon, na.rm = TRUE)
      out[[k]] <- data.frame(
        imputation = imputation,
        label = spec$label,
        outcome = spec$outcome,
        stratum = as.character(level),
        horizon = horizon,
        n = length(index),
        events_by_horizon = event_count,
        fewer_than_10_events = event_count < 10L,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

leakage_assert_observed_preserved <- function(completed, original, columns, label) {
  missing_columns <- setdiff(columns, intersect(names(completed), names(original)))
  if (length(missing_columns)) {
    stop(label, " lacks columns required for the observed-cell audit.", call. = FALSE)
  }
  for (column in columns) {
    observed <- !is.na(original[[column]])
    if (!any(observed)) next
    completed_values <- as.character(completed[[column]][observed])
    original_values <- as.character(original[[column]][observed])
    if (anyNA(completed_values) || !identical(completed_values, original_values)) {
      stop(label, " changed an originally observed value in ", column, ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

leakage_source_files <- function(inputs) {
  files <- unique(c(
    unlist(inputs$paths, use.names = FALSE),
    inputs$validation_path,
    file.path(inputs$project_root, "cons", "_alt_scripts", "imputation_leakage_sensitivity.R"),
    file.path(
      inputs$project_root,
      "cons", "prediction23_mortality_imputation_leakage_sensitivity.ipynb"
    )
  ))
  files[file.exists(files)]
}

leakage_write_in_progress_manifest <- function(inputs, config, run_id) {
  dir.create(inputs$output_dir, recursive = TRUE, showWarnings = FALSE)
  source_files <- leakage_source_files(inputs)
  manifest <- list(
    completion_status = "in_progress",
    run_id = run_id,
    started = as.character(Sys.time()),
    config = config,
    source_validation = inputs$validation_path,
    source_paths = inputs$paths,
    source_md5 = tools::md5sum(source_files),
    note = "A complete manifest is written only after every confirmatory CSV passes its guards."
  )
  saveRDS(manifest, file.path(inputs$output_dir, "run_manifest.rds"))
  invisible(manifest)
}

leakage_counterfactual <- function(inputs, config) {
  leakage_required_packages(full = TRUE)
  leakage_source_project_engines(inputs$project_root)

  run_id <- paste0(
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    "_seed", config$seed
  )
  if (identical(config$run_mode, "confirmatory")) {
    leakage_write_in_progress_manifest(inputs, config, run_id)
  }

  eval_times <- as.numeric(config$eval_times)
  n <- length(inputs$validation_idx)
  n_h <- length(eval_times)
  n_perm <- as.integer(config$n_permutations)
  model_names <- c("best_perf1", "best_perf2")
  skill_specs <- leakage_model_skill_specs(inputs)
  skill_sums <- leakage_initialize_skill_sums(skill_specs, n, n_h)
  clean_sums <- lapply(model_names, function(x) matrix(0, nrow = n, ncol = n_h))
  names(clean_sums) <- model_names
  original_model_clean_validation_sums <- clean_sums
  leaky_sums <- clean_sums
  perm_sums <- lapply(model_names, function(x) array(0, dim = c(n, n_h, n_perm)))
  names(perm_sums) <- model_names
  aligned_perm_sums <- lapply(model_names, function(x) array(0, dim = c(n, n_h, n_perm)))
  names(aligned_perm_sums) <- model_names
  permutation_orders <- if (n_perm > 0L) {
    lapply(seq_len(n_perm), function(perm) {
      set.seed(as.integer(config$permutation_seed + perm))
      sample.int(n)
    })
  } else {
    list()
  }

  train_truth <- inputs$raw[inputs$train_idx, , drop = FALSE]
  val_truth <- inputs$raw[inputs$validation_idx, , drop = FALSE]
  readmission_admission_offset <-
    as.numeric(val_truth$readmit_time_from_adm_m) -
    as.numeric(val_truth$readmit_time_from_disch_m)
  mortality_admission_offset <-
    as.numeric(val_truth$death_time_from_adm_m) -
    as.numeric(val_truth$death_time_from_disch_m)
  train_center <- inputs$center_id[inputs$train_idx]
  val_center <- inputs$center_id[inputs$validation_idx]
  clean_cols <- setdiff(names(inputs$raw), inputs$leakage_cols)
  leaky_cols <- names(inputs$raw)
  imputation_log <- list()
  imputed_value_log <- list()
  skill_strata_log <- list()
  original_holdout <- build_holdout_datasets(force = FALSE, verify = TRUE, verbose = FALSE)
  if (length(original_holdout$train) < config$n_imputations) {
    stop("The original holdout cache has fewer imputations than requested.", call. = FALSE)
  }

  for (imp in seq_len(config$n_imputations)) {
    seed_imp <- as.integer(config$seed + imp - 1L)
    message(sprintf("Imputation %d/%d: clean development-only imputer", imp, config$n_imputations))
    clean_fit <- leakage_missranger_fit(train_truth[clean_cols], seed_imp, config)
    if (length(intersect(names(clean_fit$data), inputs$leakage_cols))) {
      stop("The clean imputer unexpectedly contains future outcome columns.", call. = FALSE)
    }
    clean_mean_oob <- mean(clean_fit$mean_pred_errors, na.rm = TRUE)
    clean_train_completed <- clean_fit$data
    leakage_assert_observed_preserved(
      clean_train_completed, train_truth, clean_cols, "Clean development imputation"
    )
    clean_val_completed <- stats::predict(
      clean_fit,
      newdata = as.data.frame(val_truth[clean_cols]),
      pmm.k = config$pmm_k,
      iter = config$predict_iter,
      num.threads = config$num_threads,
      seed = seed_imp,
      verbose = config$verbose
    )
    leakage_assert_observed_preserved(
      clean_val_completed, val_truth, clean_cols, "Clean validation imputation"
    )
    clean_train_nondum <- leakage_prepare_nondum(
      clean_train_completed, train_truth, train_center, inputs$canonical_nondum_cols
    )
    clean_val_nondum <- leakage_prepare_nondum(
      clean_val_completed, val_truth, val_center, inputs$canonical_nondum_cols
    )
    clean_train_model <- leakage_to_model_frame(clean_train_nondum, is_train = TRUE)
    clean_val_model <- leakage_to_model_frame(clean_val_nondum, is_train = FALSE)

    for (skill_id in names(skill_specs)) {
      audit_row <- leakage_skill_strata_event_audit(
        clean_train_model,
        skill_specs[[skill_id]],
        eval_times,
        imp
      )
      audit_row$model_id <- skill_id
      skill_strata_log[[length(skill_strata_log) + 1L]] <- audit_row
    }

    for (model in model_names) {
      pred <- leakage_fit_death_model(
        inputs$formulas[[model]]$death,
        clean_train_model,
        clean_val_model,
        eval_times
      )
      clean_sums[[model]] <- clean_sums[[model]] + pred$risk
      skill_id <- if (identical(model, "best_perf1")) {
        "mortality_full_ph"
      } else {
        "mortality_shap_rule2"
      }
      skill_sums[[skill_id]]$model <- skill_sums[[skill_id]]$model + pred$risk

      # LC arm: preserve the original outcome-informed development model but
      # replace its validation covariates with development-only imputations.
      original_fit <- leakage_fit_death_model(
        inputs$formulas[[model]]$death,
        original_holdout$train[[imp]],
        original_holdout$val[[imp]],
        eval_times
      )
      clean_validation_prediction <- leakage_predict_death_model(
        original_fit$fit,
        original_holdout$train[[imp]],
        clean_val_model,
        eval_times
      )
      original_model_clean_validation_sums[[model]] <-
        original_model_clean_validation_sums[[model]] + clean_validation_prediction
      original_saved_risk <- 1 - as.matrix(
        inputs$validation_object$results_boot_val[[model]]$raw_predictions[[imp]]$death$surv_val_matrix
      )
      original_reproduction_error <- max(abs(original_fit$risk - original_saved_risk), na.rm = TRUE)
      if (!is.finite(original_reproduction_error) || original_reproduction_error >= 1e-8) {
        stop("Original prediction reproduction failed for ", model, ", imputation ", imp, ".", call. = FALSE)
      }
      complete <- !inputs$model_missing_masks[[model]]
      complete_difference <- abs(
        original_saved_risk[complete, , drop = FALSE] -
          clean_validation_prediction[complete, , drop = FALSE]
      )
      if (!length(complete_difference) || any(!is.finite(complete_difference))) {
        stop("The complete-row negative control is empty or nonfinite.", call. = FALSE)
      }
      negative_control_error <- max(complete_difference)
      if (negative_control_error >= 1e-10) {
        stop("Complete-row LL versus LC negative control failed for ", model, ".", call. = FALSE)
      }
      imputation_log[[length(imputation_log) + 1L]] <- data.frame(
        imputation = imp,
        model = model,
        quantity = c(
          "max_abs_original_prediction_reproduction_error",
          "max_abs_complete_row_LL_LC_negative_control_error"
        ),
        value = c(original_reproduction_error, negative_control_error),
        stringsAsFactors = FALSE
      )
    }

    readmission_pred <- leakage_fit_death_model(
      skill_specs$readmission$formula,
      clean_train_model,
      clean_val_model,
      eval_times
    )
    skill_sums$readmission$model <-
      skill_sums$readmission$model + readmission_pred$risk

    for (skill_id in names(skill_specs)) {
      spec <- skill_specs[[skill_id]]
      for (null_type in c("marginal", "structure")) {
        null_pred <- leakage_fit_death_model(
          leakage_make_null_formula(spec$formula, null_type),
          clean_train_model,
          clean_val_model,
          eval_times
        )
        target <- paste0(null_type, "_null")
        skill_sums[[skill_id]][[target]] <-
          skill_sums[[skill_id]][[target]] + null_pred$risk
      }
    }

    original_nondum_path <- file.path(
      inputs$data_out,
      sprintf("corrected_datasets_nondum_filt_imp%d.parquet", imp)
    )
    original_nondum <- as.data.frame(nanoparquet::read_parquet(original_nondum_path))
    original_val_nondum <- original_nondum[inputs$validation_idx, , drop = FALSE]
    for (variable in inputs$retained_input_cols) {
      originally_missing <- is.na(val_truth[[variable]])
      if (!any(originally_missing) || !variable %in% names(clean_val_nondum)) next
      original_values <- original_val_nondum[[variable]][originally_missing]
      clean_values <- clean_val_nondum[[variable]][originally_missing]
      if (is.numeric(original_values) || is.integer(original_values)) {
        original_num <- as.numeric(original_values)
        clean_num <- as.numeric(clean_values)
        correlation <- if (length(unique(original_num)) > 1L && length(unique(clean_num)) > 1L) {
          suppressWarnings(stats::cor(original_num, clean_num, method = "spearman", use = "complete.obs"))
        } else NA_real_
        imputed_value_log[[length(imputed_value_log) + 1L]] <- data.frame(
          imputation = imp, variable = variable, type = "numeric",
          n_originally_missing = sum(originally_missing),
          agreement = NA_real_, mean_signed_change = mean(original_num - clean_num, na.rm = TRUE),
          mean_absolute_change = mean(abs(original_num - clean_num), na.rm = TRUE),
          rmse = sqrt(mean((original_num - clean_num)^2, na.rm = TRUE)),
          spearman = correlation, stringsAsFactors = FALSE
        )
      } else {
        agreement <- mean(as.character(original_values) == as.character(clean_values), na.rm = TRUE)
        imputed_value_log[[length(imputed_value_log) + 1L]] <- data.frame(
          imputation = imp, variable = variable, type = "categorical",
          n_originally_missing = sum(originally_missing),
          agreement = agreement, mean_signed_change = NA_real_,
          mean_absolute_change = NA_real_, rmse = NA_real_, spearman = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
    rm(original_nondum, original_val_nondum, original_fit)
    rm(clean_fit, clean_train_completed, clean_val_completed, clean_train_nondum,
       clean_val_nondum, clean_train_model, clean_val_model, readmission_pred,
       null_pred)
    invisible(gc())

    message(sprintf("Imputation %d/%d: outcome-informed development-only imputer", imp, config$n_imputations))
    leaky_fit <- leakage_missranger_fit(train_truth[leaky_cols], seed_imp, config)
    if (!all(inputs$leakage_cols %in% names(leaky_fit$data))) {
      stop("The outcome-informed imputer lacks one or more future outcome columns.", call. = FALSE)
    }
    leaky_mean_oob <- mean(leaky_fit$mean_pred_errors, na.rm = TRUE)
    leaky_train_completed <- leaky_fit$data
    leakage_assert_observed_preserved(
      leaky_train_completed, train_truth, leaky_cols,
      "Outcome-informed development imputation"
    )
    leaky_val_completed <- stats::predict(
      leaky_fit,
      newdata = as.data.frame(val_truth[leaky_cols]),
      pmm.k = config$pmm_k,
      iter = config$predict_iter,
      num.threads = config$num_threads,
      seed = seed_imp,
      verbose = config$verbose
    )
    leakage_assert_observed_preserved(
      leaky_val_completed, val_truth, leaky_cols,
      "Outcome-informed validation imputation"
    )
    leaky_train_nondum <- leakage_prepare_nondum(
      leaky_train_completed, train_truth, train_center, inputs$canonical_nondum_cols
    )
    leaky_val_nondum <- leakage_prepare_nondum(
      leaky_val_completed, val_truth, val_center, inputs$canonical_nondum_cols
    )
    leaky_train_model <- leakage_to_model_frame(leaky_train_nondum, is_train = TRUE)
    leaky_val_model <- leakage_to_model_frame(leaky_val_nondum, is_train = FALSE)

    model_fits <- list()
    for (model in model_names) {
      pred <- leakage_fit_death_model(
        inputs$formulas[[model]]$death,
        leaky_train_model,
        leaky_val_model,
        eval_times
      )
      model_fits[[model]] <- pred$fit
      leaky_sums[[model]] <- leaky_sums[[model]] + pred$risk
    }

    if (n_perm > 0L) {
      for (perm in seq_len(n_perm)) {
        permutation_seed <- as.integer(config$permutation_seed + 10000L * imp + perm)
        order_perm <- permutation_orders[[perm]]

        aligned_completed <- stats::predict(
          leaky_fit,
          newdata = as.data.frame(val_truth[leaky_cols]),
          pmm.k = config$pmm_k,
          iter = config$predict_iter,
          num.threads = config$num_threads,
          seed = permutation_seed,
          verbose = 0L
        )
        leakage_assert_observed_preserved(
          aligned_completed, val_truth, leaky_cols,
          "Seed-matched aligned validation imputation"
        )
        aligned_nondum <- leakage_prepare_nondum(
          aligned_completed, val_truth, val_center, inputs$canonical_nondum_cols
        )
        aligned_model <- leakage_to_model_frame(aligned_nondum, is_train = FALSE)

        val_for_imputer <- as.data.frame(val_truth[leaky_cols])
        endpoint_columns <- inputs$endpoint_cols
        val_for_imputer[endpoint_columns] <-
          val_for_imputer[order_perm, endpoint_columns, drop = FALSE]
        val_for_imputer$readmit_time_from_adm_m <-
          readmission_admission_offset +
          as.numeric(val_for_imputer$readmit_time_from_disch_m)
        val_for_imputer$death_time_from_adm_m <-
          mortality_admission_offset +
          as.numeric(val_for_imputer$death_time_from_disch_m)
        reconstructed_readmission_offset <-
          as.numeric(val_for_imputer$readmit_time_from_adm_m) -
          as.numeric(val_for_imputer$readmit_time_from_disch_m)
        reconstructed_mortality_offset <-
          as.numeric(val_for_imputer$death_time_from_adm_m) -
          as.numeric(val_for_imputer$death_time_from_disch_m)
        if (max(abs(
          reconstructed_readmission_offset - readmission_admission_offset
        ), na.rm = TRUE) >= 1e-10 || max(abs(
          reconstructed_mortality_offset - mortality_admission_offset
        ), na.rm = TRUE) >= 1e-10) {
          stop("The joint outcome permutation broke a recorded time offset.", call. = FALSE)
        }
        perm_completed <- stats::predict(
          leaky_fit,
          newdata = val_for_imputer,
          pmm.k = config$pmm_k,
          iter = config$predict_iter,
          num.threads = config$num_threads,
          seed = permutation_seed,
          verbose = 0L
        )
        leakage_assert_observed_preserved(
          perm_completed, val_for_imputer, leaky_cols,
          "Seed-matched permuted validation imputation"
        )
        perm_nondum <- leakage_prepare_nondum(
          perm_completed, val_truth, val_center, inputs$canonical_nondum_cols
        )
        perm_model <- leakage_to_model_frame(perm_nondum, is_train = FALSE)
        for (model in model_names) {
          aligned_perm_sums[[model]][, , perm] <-
            aligned_perm_sums[[model]][, , perm] +
            leakage_predict_death_model(
              model_fits[[model]], leaky_train_model, aligned_model, eval_times
            )
          perm_sums[[model]][, , perm] <- perm_sums[[model]][, , perm] +
            leakage_predict_death_model(
              model_fits[[model]], leaky_train_model, perm_model, eval_times
            )
        }
        if (config$verbose >= 1L && (perm %% max(1L, floor(n_perm / 5L)) == 0L)) {
          message(sprintf("  imputation %d: permutation %d/%d", imp, perm, n_perm))
        }
        rm(aligned_completed, aligned_nondum, aligned_model, val_for_imputer,
           perm_completed, perm_nondum, perm_model)
      }
    }

    imputation_log[[length(imputation_log) + 1L]] <- data.frame(
      imputation = imp,
      model = NA_character_,
      quantity = c("clean_mean_oob", "leaky_mean_oob"),
      value = c(
        clean_mean_oob,
        leaky_mean_oob
      ),
      stringsAsFactors = FALSE
    )
    rm(leaky_fit, leaky_train_completed, leaky_val_completed, leaky_train_nondum,
       leaky_val_nondum, leaky_train_model, leaky_val_model, model_fits)
    invisible(gc())
  }

  denominator <- as.numeric(config$n_imputations)
  clean <- lapply(clean_sums, `/`, denominator)
  original_model_clean_validation <- lapply(original_model_clean_validation_sums, `/`, denominator)
  leaky <- lapply(leaky_sums, `/`, denominator)
  permuted <- lapply(perm_sums, `/`, denominator)
  aligned_permuted <- lapply(aligned_perm_sums, `/`, denominator)
  skill_predictions <- lapply(skill_sums, function(x) {
    lapply(x, `/`, denominator)
  })
  if (max(abs(
    skill_predictions$mortality_full_ph$model - clean$best_perf1
  ), na.rm = TRUE) >= 1e-12 || max(abs(
    skill_predictions$mortality_shap_rule2$model - clean$best_perf2
  ), na.rm = TRUE) >= 1e-12) {
    stop("Mortality skill predictions do not reproduce the clean leakage arms.", call. = FALSE)
  }

  original <- lapply(model_names, function(model) {
    leakage_pool_original_predictions(
      inputs$validation_object$results_boot_val[[model]],
      outcome = "death",
      n_imputations = config$n_imputations
    )$risk
  })
  names(original) <- model_names
  outcome_reference <- leakage_pool_original_predictions(
    inputs$validation_object$results_boot_val$best_perf1,
    outcome = "death",
    n_imputations = config$n_imputations
  )
  readmission_outcome_reference <- leakage_pool_original_predictions(
    inputs$validation_object$results_boot_val$best_perf1,
    outcome = "readmission",
    n_imputations = config$n_imputations
  )

  list(
    original = original,
    original_model_clean_validation = original_model_clean_validation,
    clean = clean,
    leaky_true = leaky,
    leaky_aligned_matched = aligned_permuted,
    leaky_permuted = permuted,
    eval_times = eval_times,
    outcome = outcome_reference,
    model_skill = list(
      predictions = skill_predictions,
      specs = skill_specs,
      outcomes = list(death = outcome_reference, readmit = readmission_outcome_reference),
      eval_times = eval_times,
      scenario = "clean_development_only_imputation",
      strata_event_audit = do.call(rbind, skill_strata_log)
    ),
    missing_mask = inputs$missing_mask,
    model_missing_masks = inputs$model_missing_masks,
    run_id = run_id,
    config = config,
    imputation_log = do.call(rbind, imputation_log),
    imputed_value_log = if (length(imputed_value_log)) do.call(rbind, imputed_value_log) else data.frame()
  )
}

leakage_scenario_metrics <- function(counterfactual) {
  scenarios <- c("original", "original_model_clean_validation", "clean", "leaky_true")
  model_names <- names(counterfactual$original)
  ref <- counterfactual$outcome
  censoring_fit <- leakage_censoring_fit(ref$train_time, ref$train_event)
  out <- list(); k <- 0L
  for (scenario in scenarios) {
    for (model in model_names) {
      risk_matrix <- counterfactual[[scenario]][[model]]
      for (j in seq_along(counterfactual$eval_times)) {
        horizon <- counterfactual$eval_times[[j]]
        observed <- leakage_km_risk(ref$time, ref$event, horizon)
        predicted <- mean(risk_matrix[, j], na.rm = TRUE)
        k <- k + 1L
        out[[k]] <- data.frame(
          scenario = scenario,
          model = model,
          horizon = horizon,
          uno_c = leakage_cindex(ref$time, ref$event, risk_matrix[, j], horizon),
          brier = leakage_brier(
            ref$train_time, ref$train_event, ref$time, ref$event,
            risk_matrix[, j], horizon, censoring_fit = censoring_fit
          ),
          observed_risk = observed,
          mean_predicted_risk = predicted,
          predicted_observed_ratio = predicted / observed,
          stringsAsFactors = FALSE
        )
      }
      k <- k + 1L
      out[[k]] <- data.frame(
        scenario = scenario,
        model = model,
        horizon = Inf,
        uno_c = NA_real_,
        brier = leakage_ibs(
          ref$train_time, ref$train_event, ref$time, ref$event,
          risk_matrix, counterfactual$eval_times
        ),
        observed_risk = NA_real_, mean_predicted_risk = NA_real_,
        predicted_observed_ratio = NA_real_, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

leakage_scenario_dca <- function(counterfactual, thresholds = leakage_default_thresholds()) {
  scenarios <- c("original", "original_model_clean_validation", "clean", "leaky_true")
  model_names <- names(counterfactual$original)
  ref <- counterfactual$outcome
  out <- list(); k <- 0L
  for (scenario in scenarios) {
    for (model in model_names) {
      risk_matrix <- counterfactual[[scenario]][[model]]
      for (horizon_name in names(thresholds)) {
        horizon <- as.numeric(horizon_name)
        j <- match(horizon, counterfactual$eval_times)
        if (is.na(j)) next
        for (threshold in thresholds[[horizon_name]]) {
          k <- k + 1L
          row <- leakage_dca_point(ref$time, ref$event, risk_matrix[, j], horizon, threshold)
          row$scenario <- scenario
          row$model <- model
          out[[k]] <- row
        }
      }
    }
  }
  do.call(rbind, out)
}

leakage_prediction_shift <- function(counterfactual) {
  out <- list(); k <- 0L
  for (model in names(counterfactual$original)) {
    group <- ifelse(
      counterfactual$model_missing_masks[[model]],
      "any_model_predictor_imputed",
      "complete_model_predictors"
    )
    comparisons <- list(
      holdout_imputation_repair = counterfactual$original[[model]] -
        counterfactual$original_model_clean_validation[[model]],
      total_pipeline_change = counterfactual$original[[model]] - counterfactual$clean[[model]]
    )
    for (comparison in names(comparisons)) {
      delta <- comparisons[[comparison]]
      for (j in seq_along(counterfactual$eval_times)) {
        for (label in unique(group)) {
          values <- delta[group == label, j]
          k <- k + 1L
          out[[k]] <- data.frame(
            comparison = comparison,
            model = model,
            horizon = counterfactual$eval_times[[j]],
            group = label,
            n = length(values),
            mean_signed_change = mean(values, na.rm = TRUE),
            mean_absolute_change = mean(abs(values), na.rm = TRUE),
            median_absolute_change = stats::median(abs(values), na.rm = TRUE),
            q95_absolute_change = stats::quantile(abs(values), 0.95, na.rm = TRUE, names = FALSE),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, out)
}

leakage_threshold_crossings <- function(
    counterfactual, thresholds = leakage_default_thresholds()
) {
  comparisons <- list(
    holdout_imputation_repair_LL_vs_LC = c("original", "original_model_clean_validation"),
    total_pipeline_change_LL_vs_CC = c("original", "clean")
  )
  out <- list(); k <- 0L
  for (model in names(counterfactual$original)) {
    group <- ifelse(
      counterfactual$model_missing_masks[[model]],
      "any_model_predictor_imputed",
      "complete_model_predictors"
    )
    for (comparison in names(comparisons)) {
      scenario_a <- comparisons[[comparison]][[1L]]
      scenario_b <- comparisons[[comparison]][[2L]]
      risk_a <- counterfactual[[scenario_a]][[model]]
      risk_b <- counterfactual[[scenario_b]][[model]]
      for (horizon_name in names(thresholds)) {
        horizon <- as.numeric(horizon_name)
        j <- match(horizon, counterfactual$eval_times)
        if (is.na(j)) next
        for (threshold in thresholds[[horizon_name]]) {
          for (label in c("all", unique(group))) {
            idx <- if (identical(label, "all")) seq_along(group) else which(group == label)
            selected_a <- risk_a[idx, j] >= threshold
            selected_b <- risk_b[idx, j] >= threshold
            k <- k + 1L
            out[[k]] <- data.frame(
              comparison = comparison,
              model = model,
              group = label,
              horizon = horizon,
              threshold = threshold,
              n = length(idx),
              selected_reference_n = sum(selected_a),
              selected_corrected_n = sum(selected_b),
              crossed_any_n = sum(selected_a != selected_b),
              crossed_any_percent = 100 * mean(selected_a != selected_b),
              crossed_up_after_correction_n = sum(!selected_a & selected_b),
              crossed_down_after_correction_n = sum(selected_a & !selected_b),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
  do.call(rbind, out)
}

leakage_permutation_test <- function(counterfactual, thresholds = leakage_default_thresholds()) {
  ref <- counterfactual$outcome
  n_perm <- dim(counterfactual$leaky_permuted[[1L]])[[3L]]
  if (!is.finite(n_perm) || n_perm < 1L) return(data.frame())
  out <- list(); k <- 0L

  add_row <- function(model, horizon, threshold, metric,
                      aligned_values, permuted_values, higher_better) {
    keep <- is.finite(aligned_values) & is.finite(permuted_values)
    aligned_values <- aligned_values[keep]
    permuted_values <- permuted_values[keep]
    if (!length(aligned_values)) return(NULL)
    paired_gain <- if (isTRUE(higher_better)) {
      aligned_values - permuted_values
    } else {
      permuted_values - aligned_values
    }
    data.frame(
      model = model,
      horizon = horizon,
      threshold = threshold,
      metric = metric,
      true_aligned = stats::median(aligned_values),
      aligned_q025 = stats::quantile(aligned_values, 0.025, names = FALSE),
      aligned_q975 = stats::quantile(aligned_values, 0.975, names = FALSE),
      permuted_median = stats::median(permuted_values),
      permuted_q025 = stats::quantile(permuted_values, 0.025, names = FALSE),
      permuted_q975 = stats::quantile(permuted_values, 0.975, names = FALSE),
      median_aligned_advantage_over_permuted = stats::median(paired_gain),
      aligned_advantage_q025 = stats::quantile(paired_gain, 0.025, names = FALSE),
      aligned_advantage_q975 = stats::quantile(paired_gain, 0.975, names = FALSE),
      fraction_pairs_not_favoring_aligned = mean(paired_gain <= 0),
      n_permutations = length(paired_gain),
      seed_matched = TRUE,
      permuted_outcomes = paste(
        "readmission_and_mortality_endpoints_jointly",
        "with_admission_origin_times_reconstructed"
      ),
      monte_carlo_quantiles_are_not_confidence_intervals = TRUE,
      stringsAsFactors = FALSE
    )
  }

  censoring_fit <- leakage_censoring_fit(ref$train_time, ref$train_event)
  for (model in names(counterfactual$leaky_aligned_matched)) {
    aligned_array <- counterfactual$leaky_aligned_matched[[model]]
    perm_array <- counterfactual$leaky_permuted[[model]]
    for (j in seq_along(counterfactual$eval_times)) {
      horizon <- counterfactual$eval_times[[j]]
      aligned_c <- vapply(seq_len(n_perm), function(p) {
        leakage_cindex(ref$time, ref$event, aligned_array[, j, p], horizon)
      }, numeric(1))
      perm_c <- vapply(seq_len(n_perm), function(p) {
        leakage_cindex(ref$time, ref$event, perm_array[, j, p], horizon)
      }, numeric(1))
      k <- k + 1L
      out[[k]] <- add_row(
        model, horizon, NA_real_, "uno_c", aligned_c, perm_c, TRUE
      )

      aligned_brier <- vapply(seq_len(n_perm), function(p) {
        leakage_brier(
          ref$train_time, ref$train_event, ref$time, ref$event,
          aligned_array[, j, p], horizon, censoring_fit = censoring_fit
        )
      }, numeric(1))
      perm_brier <- vapply(seq_len(n_perm), function(p) {
        leakage_brier(
          ref$train_time, ref$train_event, ref$time, ref$event,
          perm_array[, j, p], horizon, censoring_fit = censoring_fit
        )
      }, numeric(1))
      k <- k + 1L
      out[[k]] <- add_row(
        model, horizon, NA_real_, "brier", aligned_brier, perm_brier, FALSE
      )

      threshold_values <- thresholds[[as.character(horizon)]]
      if (!is.null(threshold_values)) {
        for (threshold in threshold_values) {
          aligned_dca <- lapply(seq_len(n_perm), function(p) {
            leakage_dca_point(
              ref$time, ref$event, aligned_array[, j, p], horizon, threshold
            )
          })
          perm_dca <- lapply(seq_len(n_perm), function(p) {
            leakage_dca_point(ref$time, ref$event, perm_array[, j, p], horizon, threshold)
          })
          aligned_nb <- vapply(aligned_dca, function(x) x$net_benefit, numeric(1))
          aligned_avoided <- vapply(
            aligned_dca, function(x) x$avoided_per1000_vs_all, numeric(1)
          )
          perm_nb <- vapply(perm_dca, function(x) x$net_benefit, numeric(1))
          perm_avoided <- vapply(perm_dca, function(x) x$avoided_per1000_vs_all, numeric(1))
          k <- k + 1L
          out[[k]] <- add_row(
            model, horizon, threshold, "net_benefit",
            aligned_nb, perm_nb, TRUE
          )
          k <- k + 1L
          out[[k]] <- add_row(
            model, horizon, threshold, "avoided_per1000_vs_all",
            aligned_avoided, perm_avoided, TRUE
          )
        }
      }
    }
  }
  do.call(rbind, Filter(Negate(is.null), out))
}

leakage_total_optimism <- function(metrics, dca) {
  metric_original <- metrics[metrics$scenario == "original", ]
  metric_clean <- metrics[metrics$scenario == "clean", ]
  key <- c("model", "horizon")
  merged_metric <- merge(metric_original, metric_clean, by = key, suffixes = c("_original", "_clean"))
  metric_out <- rbind(
    data.frame(
      model = merged_metric$model, horizon = merged_metric$horizon, threshold = NA_real_,
      metric = "uno_c", original = merged_metric$uno_c_original,
      clean = merged_metric$uno_c_clean,
      estimated_optimism = merged_metric$uno_c_original - merged_metric$uno_c_clean
    ),
    data.frame(
      model = merged_metric$model, horizon = merged_metric$horizon, threshold = NA_real_,
      metric = "brier_or_ibs", original = merged_metric$brier_original,
      clean = merged_metric$brier_clean,
      estimated_optimism = merged_metric$brier_clean - merged_metric$brier_original
    )
  )

  dca_original <- dca[dca$scenario == "original", ]
  dca_clean <- dca[dca$scenario == "clean", ]
  dca_key <- c("model", "horizon", "threshold")
  merged_dca <- merge(dca_original, dca_clean, by = dca_key, suffixes = c("_original", "_clean"))
  dca_out <- rbind(
    data.frame(
      model = merged_dca$model, horizon = merged_dca$horizon, threshold = merged_dca$threshold,
      metric = "net_benefit", original = merged_dca$net_benefit_original,
      clean = merged_dca$net_benefit_clean,
      estimated_optimism = merged_dca$net_benefit_original - merged_dca$net_benefit_clean
    ),
    data.frame(
      model = merged_dca$model, horizon = merged_dca$horizon, threshold = merged_dca$threshold,
      metric = "avoided_per1000_vs_all", original = merged_dca$avoided_per1000_vs_all_original,
      clean = merged_dca$avoided_per1000_vs_all_clean,
      estimated_optimism = merged_dca$avoided_per1000_vs_all_original - merged_dca$avoided_per1000_vs_all_clean
    )
  )
  rbind(metric_out, dca_out)
}

leakage_component_optimism <- function(metrics, dca) {
  make_contrast <- function(reference_scenario, corrected_scenario, label) {
    metric_a <- metrics[metrics$scenario == reference_scenario, ]
    metric_b <- metrics[metrics$scenario == corrected_scenario, ]
    merged_metric <- merge(metric_a, metric_b, by = c("model", "horizon"), suffixes = c("_reference", "_corrected"))
    metric_out <- rbind(
      data.frame(
        comparison = label, model = merged_metric$model, horizon = merged_metric$horizon,
        threshold = NA_real_, metric = "uno_c",
        reference = merged_metric$uno_c_reference, corrected = merged_metric$uno_c_corrected,
        estimated_optimism = merged_metric$uno_c_reference - merged_metric$uno_c_corrected
      ),
      data.frame(
        comparison = label, model = merged_metric$model, horizon = merged_metric$horizon,
        threshold = NA_real_, metric = "brier_or_ibs",
        reference = merged_metric$brier_reference, corrected = merged_metric$brier_corrected,
        estimated_optimism = merged_metric$brier_corrected - merged_metric$brier_reference
      )
    )

    dca_a <- dca[dca$scenario == reference_scenario, ]
    dca_b <- dca[dca$scenario == corrected_scenario, ]
    merged_dca <- merge(dca_a, dca_b, by = c("model", "horizon", "threshold"), suffixes = c("_reference", "_corrected"))
    dca_out <- rbind(
      data.frame(
        comparison = label, model = merged_dca$model, horizon = merged_dca$horizon,
        threshold = merged_dca$threshold, metric = "net_benefit",
        reference = merged_dca$net_benefit_reference, corrected = merged_dca$net_benefit_corrected,
        estimated_optimism = merged_dca$net_benefit_reference - merged_dca$net_benefit_corrected
      ),
      data.frame(
        comparison = label, model = merged_dca$model, horizon = merged_dca$horizon,
        threshold = merged_dca$threshold, metric = "avoided_per1000_vs_all",
        reference = merged_dca$avoided_per1000_vs_all_reference,
        corrected = merged_dca$avoided_per1000_vs_all_corrected,
        estimated_optimism = merged_dca$avoided_per1000_vs_all_reference -
          merged_dca$avoided_per1000_vs_all_corrected
      )
    )
    rbind(metric_out, dca_out)
  }
  rbind(
    make_contrast(
      "original", "original_model_clean_validation",
      "holdout_imputation_repair_LL_minus_LC"
    ),
    make_contrast("original", "clean", "total_pipeline_change_LL_minus_CC")
  )
}

leakage_paired_bootstrap <- function(
    counterfactual,
    b = 500L,
    seed = 2125L,
    primary_thresholds = c(`36` = 0.03, `60` = 0.05)
) {
  if (b < 20L) stop("Use at least 20 bootstrap replicates.", call. = FALSE)
  ref <- counterfactual$outcome
  n <- length(ref$time)
  contrasts <- list(
    holdout_imputation_repair_LL_minus_LC = c("original", "original_model_clean_validation"),
    total_pipeline_change_LL_minus_CC = c("original", "clean")
  )
  censoring_fit <- leakage_censoring_fit(ref$train_time, ref$train_event)
  set.seed(seed)
  boot_indices <- replicate(b, sample.int(n, n, replace = TRUE), simplify = FALSE)
  out <- list(); k <- 0L

  summarize_boot <- function(point, values) {
    data.frame(
      estimate = point,
      bootstrap_mean = mean(values, na.rm = TRUE),
      lower = stats::quantile(values, 0.025, na.rm = TRUE, names = FALSE),
      upper = stats::quantile(values, 0.975, na.rm = TRUE, names = FALSE),
      b_valid = sum(is.finite(values))
    )
  }

  for (comparison in names(contrasts)) {
    scenario_a <- contrasts[[comparison]][[1L]]
    scenario_b <- contrasts[[comparison]][[2L]]
    for (model in names(counterfactual$original)) {
      risk_a <- counterfactual[[scenario_a]][[model]]
      risk_b <- counterfactual[[scenario_b]][[model]]
      for (j in seq_along(counterfactual$eval_times)) {
        horizon <- counterfactual$eval_times[[j]]
        point_c <- leakage_cindex(ref$time, ref$event, risk_a[, j], horizon) -
          leakage_cindex(ref$time, ref$event, risk_b[, j], horizon)
        boot_c <- vapply(boot_indices, function(idx) {
          leakage_cindex(ref$time[idx], ref$event[idx], risk_a[idx, j], horizon) -
            leakage_cindex(ref$time[idx], ref$event[idx], risk_b[idx, j], horizon)
        }, numeric(1))
        k <- k + 1L
        out[[k]] <- cbind(
          data.frame(comparison = comparison, model = model, horizon = horizon,
                     threshold = NA_real_, metric = "uno_c_optimism"),
          summarize_boot(point_c, boot_c)
        )

        point_brier <- leakage_brier(
          ref$train_time, ref$train_event, ref$time, ref$event, risk_b[, j], horizon,
          censoring_fit = censoring_fit
        ) - leakage_brier(
          ref$train_time, ref$train_event, ref$time, ref$event, risk_a[, j], horizon,
          censoring_fit = censoring_fit
        )
        boot_brier <- vapply(boot_indices, function(idx) {
          leakage_brier(
            ref$train_time, ref$train_event, ref$time[idx], ref$event[idx],
            risk_b[idx, j], horizon, censoring_fit = censoring_fit
          ) - leakage_brier(
            ref$train_time, ref$train_event, ref$time[idx], ref$event[idx],
            risk_a[idx, j], horizon, censoring_fit = censoring_fit
          )
        }, numeric(1))
        k <- k + 1L
        out[[k]] <- cbind(
          data.frame(comparison = comparison, model = model, horizon = horizon,
                     threshold = NA_real_, metric = "brier_optimism"),
          summarize_boot(point_brier, boot_brier)
        )

        threshold <- unname(primary_thresholds[as.character(horizon)])
        if (length(threshold) == 1L && is.finite(threshold)) {
          dca_a <- leakage_dca_point(ref$time, ref$event, risk_a[, j], horizon, threshold)
          dca_b <- leakage_dca_point(ref$time, ref$event, risk_b[, j], horizon, threshold)
          point_nb <- dca_a$net_benefit - dca_b$net_benefit
          point_avoided <- dca_a$avoided_per1000_vs_all - dca_b$avoided_per1000_vs_all
          boot_dca <- lapply(boot_indices, function(idx) {
            a <- leakage_dca_point(ref$time[idx], ref$event[idx], risk_a[idx, j], horizon, threshold)
            b0 <- leakage_dca_point(ref$time[idx], ref$event[idx], risk_b[idx, j], horizon, threshold)
            c(nb = a$net_benefit - b0$net_benefit,
              avoided = a$avoided_per1000_vs_all - b0$avoided_per1000_vs_all)
          })
          boot_dca <- do.call(rbind, boot_dca)
          k <- k + 1L
          out[[k]] <- cbind(
            data.frame(comparison = comparison, model = model, horizon = horizon,
                       threshold = threshold, metric = "net_benefit_optimism"),
            summarize_boot(point_nb, boot_dca[, "nb"])
          )
          k <- k + 1L
          out[[k]] <- cbind(
            data.frame(comparison = comparison, model = model, horizon = horizon,
                       threshold = threshold, metric = "avoided_per1000_optimism"),
            summarize_boot(point_avoided, boot_dca[, "avoided"])
          )
        }
      }
    }
  }
  do.call(rbind, out)
}

leakage_model_skill_thresholds <- function() {
  list(
    death = c(`6` = 0.005, `12` = 0.010, `36` = 0.030, `60` = 0.050),
    readmit = c(`6` = 0.080, `12` = 0.100, `36` = 0.200, `60` = 0.200)
  )
}

leakage_skill_state_position <- function(pstate, states, target) {
  column_names <- if (is.null(dim(pstate))) names(pstate) else colnames(pstate)
  if (!is.null(column_names)) {
    position <- match(target, column_names)
    if (!is.na(position)) return(position)
  }
  position <- match(target, states)
  if (is.na(position)) stop("Could not locate the requested multistate target.", call. = FALSE)
  n_columns <- if (is.null(dim(pstate))) length(pstate) else ncol(pstate)
  if (n_columns == length(states) + 1L) return(position + 1L)
  if (n_columns == length(states)) return(position)
  stop("Unexpected Aalen-Johansen state-probability structure.", call. = FALSE)
}

leakage_readmission_first_event <- function(model_skill) {
  readmit <- model_skill$outcomes$readmit
  death <- model_skill$outcomes$death
  n <- length(readmit$time)
  if (length(death$time) != n) {
    stop("Readmission and mortality validation outcomes are not row-aligned.", call. = FALSE)
  }
  first_time <- pmin(readmit$time, death$time)
  readmit_first <- readmit$event == 1L & is.finite(first_time) &
    abs(readmit$time - first_time) <= 1e-8
  death_first <- death$event == 1L & is.finite(first_time) &
    abs(death$time - first_time) <= 1e-8
  status <- rep(0L, n)
  status[readmit_first & !death_first] <- 1L
  status[death_first] <- 2L
  data.frame(time = first_time, status = status)
}

leakage_aj_readmission_risk <- function(time, status, horizon) {
  keep <- is.finite(time) & !is.na(status)
  time <- time[keep]
  status <- status[keep]
  if (!length(time) || !any(status == 1L)) return(0)
  state <- factor(
    status,
    levels = c(0L, 1L, 2L),
    labels = c("censor", "readmit", "death")
  )
  fit <- tryCatch(
    survival::survfit(survival::Surv(time, state) ~ 1),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)
  summary_fit <- summary(fit, times = horizon, extend = TRUE)
  pstate <- summary_fit$pstate
  if (is.null(pstate) || !length(pstate)) return(0)
  position <- tryCatch(
    leakage_skill_state_position(pstate, fit$states, "readmit"),
    error = function(e) NA_integer_
  )
  if (is.na(position)) return(NA_real_)
  risk <- if (is.null(dim(pstate))) {
    as.numeric(pstate[position])
  } else {
    as.numeric(pstate[1L, position])
  }
  if (!is.finite(risk)) return(NA_real_)
  min(max(risk, 0), 1)
}

leakage_readmission_dca_point <- function(first_event, risk, horizon, threshold) {
  keep <- is.finite(first_event$time) & !is.na(first_event$status) & is.finite(risk)
  time <- first_event$time[keep]
  status <- first_event$status[keep]
  risk <- risk[keep]
  n <- length(risk)
  if (!n) return(data.frame())
  positive <- risk >= threshold
  positive_rate <- mean(positive)
  all_risk <- leakage_aj_readmission_risk(time, status, horizon)
  selected_risk <- if (any(positive)) {
    leakage_aj_readmission_risk(time[positive], status[positive], horizon)
  } else {
    0
  }
  odds <- threshold / (1 - threshold)
  net_benefit <- positive_rate * (selected_risk - (1 - selected_risk) * odds)
  treat_all_net_benefit <- all_risk - (1 - all_risk) * odds
  data.frame(
    horizon = horizon,
    threshold = threshold,
    n = n,
    positive_rate = positive_rate,
    observed_risk = all_risk,
    observed_risk_positive = selected_risk,
    net_benefit = net_benefit,
    net_cases_per1000 = 1000 * net_benefit,
    net_benefit_vs_all = net_benefit - treat_all_net_benefit,
    avoided_per1000_vs_all = (net_benefit - treat_all_net_benefit) / odds * 1000,
    stringsAsFactors = FALSE
  )
}

leakage_skill_dca_point <- function(model_skill, spec, risk, horizon, threshold, index = NULL) {
  if (is.null(index)) index <- seq_len(nrow(risk))
  horizon_index <- match(horizon, model_skill$eval_times)
  if (is.na(horizon_index)) stop("Requested DCA horizon is absent.", call. = FALSE)
  risk_vector <- risk[index, horizon_index]
  if (identical(spec$outcome, "death")) {
    reference <- model_skill$outcomes$death
    return(leakage_dca_point(
      reference$time[index], reference$event[index], risk_vector, horizon, threshold
    ))
  }
  first_event <- leakage_readmission_first_event(model_skill)
  leakage_readmission_dca_point(first_event[index, , drop = FALSE], risk_vector, horizon, threshold)
}

leakage_model_skill_definitions <- function(counterfactual) {
  model_skill <- counterfactual$model_skill
  do.call(rbind, lapply(names(model_skill$specs), function(model_id) {
    spec <- model_skill$specs[[model_id]]
    structure_formula <- leakage_make_null_formula(spec$formula, "structure")
    data.frame(
      model_id = model_id,
      label = spec$label,
      outcome = spec$outcome,
      registry_model = spec$registry_model,
      estimand = spec$estimand,
      evaluation_scenario = model_skill$scenario,
      full_formula = paste(deparse(spec$formula), collapse = " "),
      marginal_null_formula = paste(
        deparse(leakage_make_null_formula(spec$formula, "marginal")), collapse = " "
      ),
      structure_null_formula = paste(deparse(structure_formula), collapse = " "),
      stringsAsFactors = FALSE
    )
  }))
}

leakage_model_skill_point <- function(
    counterfactual,
    thresholds = leakage_model_skill_thresholds()
) {
  model_skill <- counterfactual$model_skill
  out <- list()
  k <- 0L
  add_row <- function(spec, model_id, null_type, horizon, threshold, metric,
                      model_value, null_value, estimate, direction) {
    data.frame(
      model_id = model_id,
      label = spec$label,
      outcome = spec$outcome,
      registry_model = spec$registry_model,
      null_type = null_type,
      estimand = spec$estimand,
      evaluation_scenario = model_skill$scenario,
      horizon = horizon,
      threshold = threshold,
      metric = metric,
      model_value = model_value,
      null_value = null_value,
      estimate = estimate,
      favorable_direction = direction,
      stringsAsFactors = FALSE
    )
  }

  for (model_id in names(model_skill$specs)) {
    spec <- model_skill$specs[[model_id]]
    predictions <- model_skill$predictions[[model_id]]
    reference <- model_skill$outcomes[[spec$outcome]]
    censoring_fit <- leakage_censoring_fit(reference$train_time, reference$train_event)
    for (null_type in c("marginal_null", "structure_null")) {
      model_risk <- predictions$model
      null_risk <- predictions[[null_type]]
      for (j in seq_along(model_skill$eval_times)) {
        horizon <- model_skill$eval_times[[j]]
        model_brier <- leakage_brier(
          reference$train_time, reference$train_event,
          reference$time, reference$event, model_risk[, j], horizon,
          censoring_fit = censoring_fit
        )
        null_brier <- leakage_brier(
          reference$train_time, reference$train_event,
          reference$time, reference$event, null_risk[, j], horizon,
          censoring_fit = censoring_fit
        )
        brier_skill <- if (is.finite(null_brier) && null_brier > 0) {
          1 - model_brier / null_brier
        } else {
          NA_real_
        }
        k <- k + 1L
        out[[k]] <- add_row(
          spec, model_id, null_type, horizon, NA_real_, "brier_skill_score",
          model_brier, null_brier, brier_skill, "positive"
        )
        k <- k + 1L
        out[[k]] <- add_row(
          spec, model_id, null_type, horizon, NA_real_, "delta_brier",
          model_brier, null_brier, null_brier - model_brier, "positive"
        )

        model_c <- leakage_cindex(reference$time, reference$event, model_risk[, j], horizon)
        null_c <- leakage_cindex(reference$time, reference$event, null_risk[, j], horizon)
        k <- k + 1L
        out[[k]] <- add_row(
          spec, model_id, null_type, horizon, NA_real_, "delta_uno_c",
          model_c, null_c, model_c - null_c, "positive"
        )
      }

      model_ibs <- leakage_ibs(
        reference$train_time, reference$train_event,
        reference$time, reference$event, model_risk, model_skill$eval_times
      )
      null_ibs <- leakage_ibs(
        reference$train_time, reference$train_event,
        reference$time, reference$event, null_risk, model_skill$eval_times
      )
      integrated_skill <- if (is.finite(null_ibs) && null_ibs > 0) {
        1 - model_ibs / null_ibs
      } else {
        NA_real_
      }
      k <- k + 1L
      out[[k]] <- add_row(
        spec, model_id, null_type, Inf, NA_real_, "integrated_brier_skill_score",
        model_ibs, null_ibs, integrated_skill, "positive"
      )
      k <- k + 1L
      out[[k]] <- add_row(
        spec, model_id, null_type, Inf, NA_real_, "delta_integrated_brier",
        model_ibs, null_ibs, null_ibs - model_ibs, "positive"
      )

      if (!identical(spec$outcome, "death")) next
      outcome_thresholds <- thresholds[[spec$outcome]]
      for (horizon_name in names(outcome_thresholds)) {
        horizon <- as.numeric(horizon_name)
        threshold <- unname(outcome_thresholds[[horizon_name]])
        model_dca <- leakage_skill_dca_point(
          model_skill, spec, model_risk, horizon, threshold
        )
        null_dca <- leakage_skill_dca_point(
          model_skill, spec, null_risk, horizon, threshold
        )
        delta_nb <- model_dca$net_benefit - null_dca$net_benefit
        odds <- threshold / (1 - threshold)
        k <- k + 1L
        out[[k]] <- add_row(
          spec, model_id, null_type, horizon, threshold, "delta_net_benefit",
          model_dca$net_benefit, null_dca$net_benefit, delta_nb, "positive"
        )
        k <- k + 1L
        out[[k]] <- add_row(
          spec, model_id, null_type, horizon, threshold,
          "decision_analytic_interventions_avoided_per1000_vs_null",
          model_dca$net_benefit, null_dca$net_benefit,
          delta_nb / odds * 1000, "positive"
        )
        k <- k + 1L
        out[[k]] <- add_row(
          spec, model_id, null_type, horizon, threshold, "delta_selected_per1000",
          model_dca$positive_rate, null_dca$positive_rate,
          1000 * (model_dca$positive_rate - null_dca$positive_rate),
          "context_dependent"
        )
      }
    }
  }
  do.call(rbind, out)
}

leakage_model_skill_bootstrap <- function(
    counterfactual,
    b = 500L,
    seed = 42125L,
    thresholds = leakage_model_skill_thresholds()
) {
  if (b < 20L) stop("Use at least 20 bootstrap replicates.", call. = FALSE)
  model_skill <- counterfactual$model_skill
  n <- length(model_skill$outcomes$death$time)
  set.seed(seed)
  bootstrap_indices <- replicate(b, sample.int(n, n, replace = TRUE), simplify = FALSE)
  out <- list()
  k <- 0L

  summarize_values <- function(point, values) {
    data.frame(
      estimate = point,
      bootstrap_mean = mean(values, na.rm = TRUE),
      lower = stats::quantile(values, 0.025, na.rm = TRUE, names = FALSE),
      upper = stats::quantile(values, 0.975, na.rm = TRUE, names = FALSE),
      b_valid = sum(is.finite(values)),
      b_requested = b
    )
  }
  add_summary <- function(spec, model_id, null_type, horizon, threshold, metric,
                          point, values) {
    cbind(
      data.frame(
        model_id = model_id,
        label = spec$label,
        outcome = spec$outcome,
        registry_model = spec$registry_model,
        null_type = null_type,
        estimand = spec$estimand,
        evaluation_scenario = model_skill$scenario,
        horizon = horizon,
        threshold = threshold,
        metric = metric,
        stringsAsFactors = FALSE
      ),
      summarize_values(point, values)
    )
  }

  for (model_id in names(model_skill$specs)) {
    spec <- model_skill$specs[[model_id]]
    predictions <- model_skill$predictions[[model_id]]
    reference <- model_skill$outcomes[[spec$outcome]]
    censoring_fit <- leakage_censoring_fit(reference$train_time, reference$train_event)
    for (null_type in c("marginal_null", "structure_null")) {
      model_risk <- predictions$model
      null_risk <- predictions[[null_type]]
      for (j in seq_along(model_skill$eval_times)) {
        horizon <- model_skill$eval_times[[j]]
        calculate_brier_contrasts <- function(index) {
          model_brier <- leakage_brier(
            reference$train_time, reference$train_event,
            reference$time[index], reference$event[index], model_risk[index, j], horizon,
            censoring_fit = censoring_fit
          )
          null_brier <- leakage_brier(
            reference$train_time, reference$train_event,
            reference$time[index], reference$event[index], null_risk[index, j], horizon,
            censoring_fit = censoring_fit
          )
          c(
            brier_skill_score = if (is.finite(null_brier) && null_brier > 0) {
              1 - model_brier / null_brier
            } else {
              NA_real_
            },
            delta_brier = null_brier - model_brier
          )
        }
        point_brier <- calculate_brier_contrasts(seq_len(n))
        bootstrap_brier <- vapply(
          bootstrap_indices, calculate_brier_contrasts, numeric(2)
        )
        k <- k + 1L
        out[[k]] <- add_summary(
          spec, model_id, null_type, horizon, NA_real_, "brier_skill_score",
          point_brier[["brier_skill_score"]],
          bootstrap_brier["brier_skill_score", ]
        )
        k <- k + 1L
        out[[k]] <- add_summary(
          spec, model_id, null_type, horizon, NA_real_, "delta_brier",
          point_brier[["delta_brier"]], bootstrap_brier["delta_brier", ]
        )

        calculate_delta_c <- function(index) {
          leakage_cindex(
            reference$time[index], reference$event[index], model_risk[index, j], horizon
          ) - leakage_cindex(
            reference$time[index], reference$event[index], null_risk[index, j], horizon
          )
        }
        point_delta_c <- calculate_delta_c(seq_len(n))
        bootstrap_delta_c <- vapply(bootstrap_indices, calculate_delta_c, numeric(1))
        k <- k + 1L
        out[[k]] <- add_summary(
          spec, model_id, null_type, horizon, NA_real_, "delta_uno_c",
          point_delta_c, bootstrap_delta_c
        )
      }

      calculate_integrated_contrasts <- function(index) {
        model_ibs <- leakage_ibs(
          reference$train_time, reference$train_event,
          reference$time[index], reference$event[index],
          model_risk[index, , drop = FALSE], model_skill$eval_times
        )
        null_ibs <- leakage_ibs(
          reference$train_time, reference$train_event,
          reference$time[index], reference$event[index],
          null_risk[index, , drop = FALSE], model_skill$eval_times
        )
        c(
          integrated_brier_skill_score = if (is.finite(null_ibs) && null_ibs > 0) {
            1 - model_ibs / null_ibs
          } else {
            NA_real_
          },
          delta_integrated_brier = null_ibs - model_ibs
        )
      }
      point_integrated <- calculate_integrated_contrasts(seq_len(n))
      bootstrap_integrated <- vapply(
        bootstrap_indices, calculate_integrated_contrasts, numeric(2)
      )
      k <- k + 1L
      out[[k]] <- add_summary(
        spec, model_id, null_type, Inf, NA_real_, "integrated_brier_skill_score",
        point_integrated[["integrated_brier_skill_score"]],
        bootstrap_integrated["integrated_brier_skill_score", ]
      )
      k <- k + 1L
      out[[k]] <- add_summary(
        spec, model_id, null_type, Inf, NA_real_, "delta_integrated_brier",
        point_integrated[["delta_integrated_brier"]],
        bootstrap_integrated["delta_integrated_brier", ]
      )

      if (!identical(spec$outcome, "death")) next
      outcome_thresholds <- thresholds[[spec$outcome]]
      for (horizon_name in names(outcome_thresholds)) {
        horizon <- as.numeric(horizon_name)
        threshold <- unname(outcome_thresholds[[horizon_name]])
        calculate_dca <- function(index) {
          model_dca <- leakage_skill_dca_point(
            model_skill, spec, model_risk, horizon, threshold, index
          )
          null_dca <- leakage_skill_dca_point(
            model_skill, spec, null_risk, horizon, threshold, index
          )
          delta_nb <- model_dca$net_benefit - null_dca$net_benefit
          odds <- threshold / (1 - threshold)
          c(
            delta_net_benefit = delta_nb,
            decision_analytic_interventions_avoided_per1000_vs_null =
              delta_nb / odds * 1000,
            delta_selected_per1000 = 1000 *
              (model_dca$positive_rate - null_dca$positive_rate)
          )
        }
        point_dca <- calculate_dca(seq_len(n))
        bootstrap_dca <- do.call(rbind, lapply(bootstrap_indices, calculate_dca))
        for (metric in names(point_dca)) {
          k <- k + 1L
          out[[k]] <- add_summary(
            spec, model_id, null_type, horizon, threshold, metric,
            point_dca[[metric]], bootstrap_dca[, metric]
          )
        }
      }
    }
  }
  do.call(rbind, out)
}

leakage_assert_unique_metric_keys <- function(data, columns, label) {
  if (!all(columns %in% names(data))) {
    stop(label, " lacks one or more key columns.", call. = FALSE)
  }
  key_parts <- lapply(data[columns], function(x) {
    value <- as.character(x)
    value[is.na(value)] <- "<NA>"
    value
  })
  key <- do.call(paste, c(key_parts, sep = "\r"))
  if (anyDuplicated(key)) stop(label, " contains duplicate metric keys.", call. = FALSE)
  invisible(TRUE)
}

leakage_save_model_skill_outputs <- function(
    inputs, counterfactual, point_estimates, paired_bootstrap
) {
  dir.create(inputs$output_dir, recursive = TRUE, showWarnings = FALSE)
  definitions <- leakage_model_skill_definitions(counterfactual)
  metric_key <- c("model_id", "null_type", "horizon", "threshold", "metric")
  if (nrow(definitions) != 3L || nrow(point_estimates) != 150L ||
      nrow(paired_bootstrap) != 150L) {
    stop("Model-skill outputs have unexpected row counts.", call. = FALSE)
  }
  leakage_assert_unique_metric_keys(point_estimates, metric_key, "Point skill output")
  leakage_assert_unique_metric_keys(paired_bootstrap, metric_key, "Bootstrap skill output")
  if (any(paired_bootstrap$b_valid < 0.95 * paired_bootstrap$b_requested)) {
    stop("Too few valid paired bootstrap replicates for model skill.", call. = FALSE)
  }
  utils::write.csv(
    definitions,
    file.path(inputs$output_dir, "model_skill_vs_null_definitions.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    point_estimates,
    file.path(inputs$output_dir, "model_skill_vs_null_point_estimates.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    paired_bootstrap,
    file.path(inputs$output_dir, "model_skill_vs_null_paired_bootstrap.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    counterfactual$model_skill$strata_event_audit,
    file.path(inputs$output_dir, "model_skill_structure_null_strata_events.csv"),
    row.names = FALSE
  )
  expected_output_names <- c(
    "counterfactual_metrics.csv",
    "counterfactual_dca.csv",
    "prediction_shift_by_missingness.csv",
    "joint_outcome_permutation_test.csv",
    "estimated_total_optimism.csv",
    "estimated_leakage_components.csv",
    "paired_patient_bootstrap.csv",
    "threshold_crossings.csv",
    "imputation_oob_log.csv",
    "imputed_value_comparison.csv",
    "model_skill_vs_null_definitions.csv",
    "model_skill_vs_null_point_estimates.csv",
    "model_skill_vs_null_paired_bootstrap.csv",
    "model_skill_structure_null_strata_events.csv"
  )
  expected_outputs <- file.path(inputs$output_dir, expected_output_names)
  if (!all(file.exists(expected_outputs))) {
    stop("One or more guarded confirmatory outputs are missing.", call. = FALSE)
  }
  source_files <- leakage_source_files(inputs)
  manifest_path <- file.path(inputs$output_dir, "run_manifest.rds")
  output_files <- unique(c(expected_outputs, manifest_path))
  output_rows <- vapply(expected_outputs, function(path) {
    max(length(readLines(path, warn = FALSE)) - 1L, 0L)
  }, integer(1))
  manifest <- list(
    completion_status = "complete",
    run_id = counterfactual$run_id,
    completed = as.character(Sys.time()),
    config = counterfactual$config,
    source_validation = inputs$validation_path,
    source_paths = inputs$paths,
    source_md5 = tools::md5sum(source_files),
    model_definitions = definitions,
    model_exposure_audit = inputs$model_exposure_audit,
    output_files = output_files,
    output_md5 = tools::md5sum(expected_outputs),
    output_rows = output_rows,
    session = utils::sessionInfo(),
    notes = c(
      "No patient-level completed data or random-forest objects were saved.",
      "Readmission skill targets cause-specific net risk with competing death censored.",
      "Seed-matched permutation quantiles describe Monte Carlo variability and are not confidence intervals or calibrated p-values."
    )
  )
  saveRDS(manifest, manifest_path)
  invisible(inputs$output_dir)
}

leakage_save_quick_outputs <- function(inputs, quick) {
  dir.create(inputs$output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(inputs$audit, file.path(inputs$output_dir, "missingness_audit.csv"), row.names = FALSE)
  utils::write.csv(inputs$model_exposure_audit, file.path(inputs$output_dir, "model_specific_exposure_audit.csv"), row.names = FALSE)
  utils::write.csv(quick$metrics, file.path(inputs$output_dir, "quick_subgroup_metrics.csv"), row.names = FALSE)
  utils::write.csv(quick$dca, file.path(inputs$output_dir, "quick_subgroup_dca.csv"), row.names = FALSE)
  invisible(inputs$output_dir)
}

leakage_save_counterfactual_outputs <- function(
    inputs, counterfactual, metrics, dca, shifts, permutation, optimism,
    component_optimism = data.frame(), paired_bootstrap = data.frame(),
    threshold_crossings = data.frame()
) {
  dir.create(inputs$output_dir, recursive = TRUE, showWarnings = FALSE)
  if (nrow(permutation)) {
    leakage_assert_unique_metric_keys(
      permutation,
      c("model", "horizon", "threshold", "metric"),
      "Joint outcome permutation output"
    )
    if (!all(permutation$seed_matched) ||
        any(permutation$n_permutations != counterfactual$config$n_permutations)) {
      stop("Joint outcome permutation output is incomplete or not seed-matched.", call. = FALSE)
    }
  }
  utils::write.csv(metrics, file.path(inputs$output_dir, "counterfactual_metrics.csv"), row.names = FALSE)
  utils::write.csv(dca, file.path(inputs$output_dir, "counterfactual_dca.csv"), row.names = FALSE)
  utils::write.csv(shifts, file.path(inputs$output_dir, "prediction_shift_by_missingness.csv"), row.names = FALSE)
  utils::write.csv(permutation, file.path(inputs$output_dir, "joint_outcome_permutation_test.csv"), row.names = FALSE)
  utils::write.csv(optimism, file.path(inputs$output_dir, "estimated_total_optimism.csv"), row.names = FALSE)
  utils::write.csv(component_optimism, file.path(inputs$output_dir, "estimated_leakage_components.csv"), row.names = FALSE)
  utils::write.csv(paired_bootstrap, file.path(inputs$output_dir, "paired_patient_bootstrap.csv"), row.names = FALSE)
  utils::write.csv(threshold_crossings, file.path(inputs$output_dir, "threshold_crossings.csv"), row.names = FALSE)
  utils::write.csv(counterfactual$imputation_log, file.path(inputs$output_dir, "imputation_oob_log.csv"), row.names = FALSE)
  utils::write.csv(counterfactual$imputed_value_log, file.path(inputs$output_dir, "imputed_value_comparison.csv"), row.names = FALSE)
  invisible(inputs$output_dir)
}
