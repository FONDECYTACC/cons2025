We conducted a stratified model‐selection analysis to identify parsimonious Cox proportional hazards models for time to readmission. For each cleaned analytical dataset, we first defined time to readmission as the number of months from treatment admission to first readmission and excluded variables not relevant to this outcome (center identifier, death event, and time to death). We then stratified the data by treatment plan type (`plan_type_corr`) and, within each stratum, evaluated candidate Cox models using automated best‐subset selection with the `glmulti` algorithm. The candidate predictor set included all available baseline sociodemographic, clinical, and treatment characteristics that varied within the stratum, and the search space allowed for pairwise interaction terms while enforcing hierarchical model structure. Model selection was guided by the Akaike Information Criterion, with safeguards to limit model complexity relative to the number of observed readmission events. For each plan type and dataset, we retained the best‐supported model and refitted it using standard Cox regression to obtain interpretable hazard ratio estimates for reporting and downstream analyses.


```{r stratified-model-selection-glmulti}
#| echo: true
#| error: false
#| warning: true
#| message: true
#| paged.print: true
#| results: "hold"
#| eval: true


#---------------------------
# 1. Helpers (Same as before)
#---------------------------
drop_constant_and_id_cols <- function(df, protect_vars = NULL) {
  df <- base::as.data.frame(df)
  keep_cols <- base::vapply(df, function(x) {
    base::length(base::unique(stats::na.omit(x))) > 1L
  }, logical(1))
  if (!is.null(protect_vars)) {
    keep_cols[names(keep_cols) %in% protect_vars] <- TRUE
  }
  df[, keep_cols, drop = FALSE]
}

make_ttr <- function(df, time_var="readmit_time_from_adm_m", event_var="readmit_event") {
  df |>
    dplyr::mutate(
      ttr_m = as.numeric(.data[[time_var]]),
      ttr_m = dplyr::if_else(is.na(ttr_m) | ttr_m <= 0, 1e-6, ttr_m),
      event = as.integer(.data[[event_var]])
    )
}

#---------------------------
# 2. The Two-Stage Fitting Function
#---------------------------

fit_glmulti_two_stage <- function(df_plan,
                                  crit = "aicc",
                                  confsetsize = 25,
                                  popsize = 200,
                                  conseq = 10,
                                  # Constraints
                                  max_vars_for_interaction = 10, # Max variables to keep for Level 2
                                  min_events_per_var = 10) {     # Rule of thumb
  
  # --- A. Setup and Exclusion ---
  exclude_from_x <- c(
    "ttr_m", "ttr_y", "event",
    "plan_type_corr",
    "readmit_time_from_adm_m", "readmit_time_from_disch_m", "readmit_event",
    "death_time_from_adm_m", "death_time_from_disch_m", "death_event",
    "center_id", "dit_m"
  )
  
  potential_x <- base::setdiff(base::names(df_plan), exclude_from_x)
  df_modeling <- df_plan[, c("ttr_m", "event", potential_x), drop = FALSE]
  df_modeling <- drop_constant_and_id_cols(df_modeling, protect_vars = c("ttr_m", "event"))
  actual_x <- base::setdiff(base::names(df_modeling), c("ttr_m", "event"))
  
  # Validation
  nevents <- sum(df_modeling$event == 1, na.rm = TRUE)
  if (length(actual_x) == 0 || nevents < 5) {
    return(list(ok = FALSE, error = "Insufficient data/variance."))
  }
  
  # --- B. STAGE 1: SCREENING (Level 1 Only) ---
  message(paste("    ... Stage 1: Screening", length(actual_x), "variables..."))
  
  # Scoping Fix for Stage 1
  tmp_name_1 <- paste0("glm_s1_", sample(100000, 1))
  assign(tmp_name_1, df_modeling, envir = .GlobalEnv)
  
  f1 <- stats::as.formula(paste0("survival::Surv(ttr_m, event) ~ ", paste(actual_x, collapse = " + ")))
  
  stage1_res <- tryCatch({
    gm1 <- do.call(glmulti::glmulti, list(
      y = f1, data = as.name(tmp_name_1), fitfunction = "coxph",
      level = 1, method = "g", crit = crit,
      confsetsize = confsetsize, popsize = popsize, conseq = conseq,
      plotty = FALSE, report = FALSE, includeobjects = FALSE
    ))
    gm1
  }, error = function(e) return(NULL), finally = {
    if(exists(tmp_name_1, envir=.GlobalEnv)) rm(list=tmp_name_1, envir=.GlobalEnv)
  })
  
  if (is.null(stage1_res)) return(list(ok=FALSE, error="Stage 1 failed"))
  
  # --- C. VARIABLE SELECTION ---
  # Extract variable importance from Stage 1 models (sum of AIC weights)
  # glmulti calculates this automatically if we ask for consensus, 
  # but here we parse the top model formulas manually to be safe.
  
  # Get variables from the single best model of Stage 1
  # (Alternatively, you could weight them, but this is safer for code stability)
  best_s1_formula <- stage1_res@formulas[[1]]
  best_s1_vars <- all.vars(best_s1_formula)
  best_s1_vars <- setdiff(best_s1_vars, c("ttr_m", "event"))
  
  # If the best model has too many vars, cap them. 
  # If too few (e.g. null model), take top 5 from the input list just to try.
  selected_vars <- best_s1_vars
  
  if (length(selected_vars) == 0) {
    # If Stage 1 selected NOTHING (Null model), interactions are unlikely to help, 
    # but we force the top few univariate predictors just to check.
    # For now, we return the Null result.
    return(list(
      ok = TRUE, stage="Stage 1 (Null)", 
      best_formula = "1", best_ic = stage1_res@crits[1]
    ))
  }
  
  # Cap variables to prevent Level 2 explosion
  if (length(selected_vars) > max_vars_for_interaction) {
    selected_vars <- selected_vars[1:max_vars_for_interaction]
  }
  
  message(paste("    ... Stage 2: Testing interactions on top", length(selected_vars), "vars..."))
  
  # --- D. STAGE 2: INTERACTIONS (Level 2) ---
  # Filter data to ONLY selected variables
  df_stage2 <- df_modeling[, c("ttr_m", "event", selected_vars), drop = FALSE]
  
  # Scoping Fix for Stage 2
  tmp_name_2 <- paste0("glm_s2_", sample(100000, 1))
  assign(tmp_name_2, df_stage2, envir = .GlobalEnv)
  
  f2 <- stats::as.formula(paste0("survival::Surv(ttr_m, event) ~ ", paste(selected_vars, collapse = " + ")))
  
  stage2_res <- tryCatch({
    gm2 <- do.call(glmulti::glmulti, list(
      y = f2, data = as.name(tmp_name_2), fitfunction = "coxph",
      level = 2,  # <--- HERE IS YOUR PROMISED LEVEL 2
      method = "g", crit = crit,
      confsetsize = confsetsize, popsize = popsize, conseq = conseq,
      plotty = FALSE, report = FALSE, includeobjects = FALSE
    ))
    gm2
  }, error = function(e) return(NULL), finally = {
    if(exists(tmp_name_2, envir=.GlobalEnv)) rm(list=tmp_name_2, envir=.GlobalEnv)
  })
  
  if (is.null(stage2_res)) return(list(ok=FALSE, error="Stage 2 failed"))
  
  # --- E. Final Outputs ---
  best_formula <- stage2_res@formulas[[1]]
  best_ic <- stage2_res@crits[1]
  
  list(
    ok = TRUE,
    stage = "Stage 2 (Interactions)",
    n = nrow(df_stage2),
    events = nevents,
    selected_vars_s1 = paste(selected_vars, collapse=", "),
    best_formula = best_formula,
    best_ic = best_ic
  )
}

#---------------------------
# 3. Execution Wrapper
#---------------------------

fit_glmulti_cox_by_plan <- function(df, plan_var = "plan_type_corr", ...) {
  df_ready <- df |>
    make_ttr(time_var = "readmit_time_from_adm_m", event_var = "readmit_event") |>
    base::as.data.frame()
  
  df_split <- base::split(df_ready, df_ready[[plan_var]], drop = TRUE)
  
  purrr::imap(df_split, function(df_stratum, stratum_name) {
    message(paste("  >> Stratum:", stratum_name, "| N =", nrow(df_stratum)))
    
    # Run the two-stage process
    res <- fit_glmulti_two_stage(df_stratum, ...)
    
    # If successful, extract formula string immediately to avoid object bloat
    if (isTRUE(res$ok) && !is.character(res$best_formula)) {
      res$best_formula <- paste(deparse(res$best_formula), collapse = "")
    }
    return(res)
  })
}

#---------------------------
# 4. Run It
#---------------------------

# Use your list of datasets here: corrected_datasets_nondum
results_by_db <- purrr::imap(
  corrected_datasets_nondum, 
  function(df, db_id) {
    message(paste("\n=== DB:", db_id, "==="))
    fit_glmulti_cox_by_plan(df, plan_var = "plan_type_corr")
  }
)

#---------------------------
# 5. View Results
#---------------------------
best_models_tbl <- purrr::imap_dfr(results_by_db, function(plan_list, db_id) {
  purrr::imap_dfr(plan_list, function(res, plan_name) {
    tibble::tibble(
      db = as.character(db_id),
      plan = as.character(plan_name),
      ok = res$ok,
      vars_from_s1 = if(res$ok) res$selected_vars_s1 else NA_character_,
      formula = if(res$ok) res$best_formula else NA_character_,
      aic = if(res$ok) res$best_ic else NA_real_,
      error = if(!res$ok) res$error else NA_character_
    )
  })
})

print(best_models_tbl)

```
