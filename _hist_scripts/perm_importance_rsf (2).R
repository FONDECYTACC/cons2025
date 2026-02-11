#clean enviroment
rm(list = ls()); gc()

# Helpers ------------------------------------------------------------

# For survival ranger, we use cumulative hazard ("chf") and take the last timepoint as a scalar risk.
predict_risk_surv <- function(fit, newdata) {
  pr <- predict(fit, data = newdata, type = "chf")$predictions
  if (is.null(dim(pr))) return(as.numeric(pr))
  pr[, ncol(pr), drop = TRUE]
}

cindex_harrell <- function(time, event, risk) {
  ok <- is.finite(time) & is.finite(event) & is.finite(risk)
  time <- time[ok]; event <- event[ok]; risk <- risk[ok]
  if (length(time) < 10) return(NA_real_)
  # survival::concordance returns C-index in $concordance
  survival::concordance(Surv(time, event) ~ risk)$concordance
}

pool_named_vectors_mean <- function(vlist) {
  vlist <- Filter(function(x) !is.null(x) && length(x) > 0, vlist)
  all_names <- sort(unique(unlist(lapply(vlist, names))))
  if (length(all_names) == 0) return(setNames(numeric(0), character(0)))
  
  mat <- vapply(
    vlist,
    FUN = function(v) {
      out <- setNames(rep(NA_real_, length(all_names)), all_names)
      out[names(v)] <- as.numeric(v)
      out
    },
    FUN.VALUE = rep(NA_real_, length(all_names))
  )
  rowMeans(mat, na.rm = TRUE)
}

# Import RDS ------------------------------------------------------------


processed_datasets<- 
rio::import(paste0(file.path(wdpath, "data/20241015_out/pred1"),"/processed_datasets.Rds"))

library(ranger)
library(survival)

target_cols <- c(
  "readmit_time_from_disch_m", "readmit_event",
  "death_time_from_disch_m", "death_event"
)

leak_time_cols <- c("readmit_time_from_adm_m", "death_time_from_adm_m")
cols_to_exclude <- c(target_cols, leak_time_cols)

pred_cols <- setdiff(names(processed_datasets[[1]]), cols_to_exclude)

# Safer than building one long "a + b + c" string:
preds_formula <- paste(pred_cols, collapse = " + ")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


## Function ------------------------------------------------------------

run_surv_permimp_checkpointed <- function(
    processed_datasets,
    time_col, event_col,
    pred_cols,
    outcome_tag,
    output_dir,
    # performance / safety knobs:
    eval_frac = 0.20,
    eval_n_max = 4000,
    n_repeats = 1,
    chunk_size = 25,
    seed = 2125,
    # ranger knobs:
    num_trees = 200,
    min_node_size = 30,
    sample_fraction = 0.632,
    mtry = NULL,
    num_threads = 1,
    save_model = FALSE
) {
  if (is.null(mtry)) mtry <- max(5, floor(sqrt(length(pred_cols)) / 2))
  
  per_imp_results <- vector("list", length(processed_datasets))
  
  for (i in seq_along(processed_datasets)) {
    cat("\n========================================\n")
    cat("Outcome:", outcome_tag, "| imputation", i, "of", length(processed_datasets), "\n")
    cat("========================================\n")
    
    gc(full = TRUE)
    
    final_file <- file.path(output_dir, sprintf("permimp_%s_imp_%03d.rds", outcome_tag, i))
    ckpt_file  <- file.path(output_dir, sprintf("permimp_%s_imp_%03d_ckpt.rds", outcome_tag, i))
    model_file <- file.path(output_dir, sprintf("ranger_%s_imp_%03d.rds", outcome_tag, i))
    
    # If final exists, load + continue (idempotent)
    if (file.exists(final_file)) {
      cat("Found final result:", final_file, "-> loading and skipping.\n")
      per_imp_results[[i]] <- readRDS(final_file)
      next
    }
    
    imp_data <- processed_datasets[[i]]
    
    # Basic sanity
    needed <- c(time_col, event_col, pred_cols)
    missing_cols <- setdiff(needed, names(imp_data))
    if (length(missing_cols) > 0) {
      warning("Missing columns in imputation ", i, ": ", paste(missing_cols, collapse = ", "))
      next
    }
    
    # Keep rows with defined outcome
    ok_outcome <- is.finite(imp_data[[time_col]]) & !is.na(imp_data[[event_col]])
    imp_data2 <- imp_data[ok_outcome, , drop = FALSE]
    n <- nrow(imp_data2)
    if (n < 50) {
      warning("Too few rows after outcome filtering in imputation ", i)
      next
    }
    
    # Evaluation subset (caps runtime)
    set.seed(seed + i)
    eval_n <- min(eval_n_max, max(200, floor(n * eval_frac)))
    eval_idx <- sample.int(n, size = eval_n, replace = FALSE)
    
    eval_x <- imp_data2[eval_idx, pred_cols, drop = FALSE]
    eval_time  <- imp_data2[[time_col]][eval_idx]
    eval_event <- imp_data2[[event_col]][eval_idx]
    
    # Fit (or load) model
    fit <- NULL
    if (file.exists(model_file)) {
      cat("Loading saved model:", model_file, "\n")
      fit <- readRDS(model_file)
    } else {
      cat("Fitting ranger survival forest...\n")
      fit <- tryCatch(
        ranger::ranger(
          formula = as.formula(paste0("Surv(", time_col, ", ", event_col, ") ~ ", paste(pred_cols, collapse = " + "))),
          data = imp_data2[, c(time_col, event_col, pred_cols), drop = FALSE],
          num.trees = num_trees,
          min.node.size = min_node_size,
          sample.fraction = sample_fraction,
          mtry = mtry,
          splitrule = "logrank",
          importance = "none",          # IMPORTANT: we do permutation ourselves (checkpointable)
          write.forest = TRUE,          # required for predict()
          save.memory = TRUE,
          num.threads = num_threads,
          seed = seed + i,
          verbose = TRUE
        ),
        error = function(e) {
          warning("Model fit failed for imputation ", i, ": ", conditionMessage(e))
          NULL
        }
      )
      if (is.null(fit)) next
      
      if (save_model) {
        saveRDS(fit, model_file)
        cat("Saved model:", model_file, "\n")
      }
    }
    
    # Baseline performance on eval set
    base_risk <- tryCatch(predict_risk_surv(fit, eval_x), error = function(e) rep(NA_real_, nrow(eval_x)))
    base_c <- cindex_harrell(eval_time, eval_event, base_risk)
    if (!is.finite(base_c)) {
      warning("Baseline C-index not finite for imputation ", i, " (outcome=", outcome_tag, ")")
      next
    }
    cat("Baseline C-index (eval subset) =", round(base_c, 4), "\n")
    
    # Resume from checkpoint if present
    done <- setNames(rep(FALSE, length(pred_cols)), pred_cols)
    imp_vec <- setNames(rep(NA_real_, length(pred_cols)), pred_cols)
    
    if (file.exists(ckpt_file)) {
      ck <- readRDS(ckpt_file)
      if (!is.null(ck$importance)) {
        imp_vec[names(ck$importance)] <- ck$importance
        done[names(ck$importance)] <- TRUE
        cat("Resuming from checkpoint. Already computed:", sum(done), "variables.\n")
      }
    }
    
    # Work data frame we mutate in-place
    x_work <- eval_x
    
    vars_left <- names(done)[!done]
    if (length(vars_left) == 0) {
      cat("Nothing left to compute; writing final.\n")
    } else {
      cat("Computing permutation importance for", length(vars_left), "variables...\n")
    }
    
    # Chunked computation with frequent checkpoint writes
    for (start in seq(1, length(vars_left), by = chunk_size)) {
      chunk <- vars_left[start:min(length(vars_left), start + chunk_size - 1)]
      cat("  Chunk", ceiling(start / chunk_size), "/", ceiling(length(vars_left) / chunk_size),
          "(", length(chunk), "vars )\n")
      
      for (v in chunk) {
        orig <- x_work[[v]]
        perm_cs <- rep(NA_real_, n_repeats)
        
        for (b in seq_len(n_repeats)) {
          set.seed(seed + i * 100000 + start * 1000 + b)
          x_work[[v]] <- sample(orig, length(orig), replace = FALSE)
          
          prisk <- tryCatch(predict_risk_surv(fit, x_work), error = function(e) rep(NA_real_, nrow(x_work)))
          perm_cs[b] <- cindex_harrell(eval_time, eval_event, prisk)
        }
        
        # restore
        x_work[[v]] <- orig
        
        perm_c <- mean(perm_cs, na.rm = TRUE)
        
        # Importance = drop in C-index (positive = important)
        imp_vec[v] <- base_c - perm_c
        done[v] <- TRUE
        
        cat("    ", v, "-> ΔC =", round(imp_vec[v], 6), "\n")
      }
      
      # checkpoint every chunk (critical for crash safety)
      saveRDS(
        list(
          outcome = outcome_tag,
          imputation = i,
          baseline_c = base_c,
          eval_n = eval_n,
          importance = imp_vec[done]
        ),
        ckpt_file
      )
      gc(full = TRUE)
    }
    
    # Final per-imputation save
    out <- list(
      outcome = outcome_tag,
      imputation = i,
      baseline_c = base_c,
      baseline_error = 1 - base_c,
      eval_n = eval_n,
      importance = imp_vec
    )
    saveRDS(out, final_file)
    cat("Saved final:", final_file, "\n")
    
    # cleanup
    rm(fit, imp_data, imp_data2, eval_x, x_work, base_risk)
    gc(full = TRUE)
    
    per_imp_results[[i]] <- out
  }
  
  # Pool across imputations (mean, aligned by names)
  pooled <- pool_named_vectors_mean(lapply(per_imp_results, `[[`, "importance"))
  pooled <- sort(pooled, decreasing = TRUE)
  
  pooled_file <- file.path(output_dir, sprintf("pooled_permimp_%s.rds", outcome_tag))
  saveRDS(list(pooled_importance = pooled, per_imputation = per_imp_results), pooled_file)
  cat("\nSaved pooled:", pooled_file, "\n")
  
  invisible(list(pooled = pooled, per_imputation = per_imp_results))
}



# Apply models ------------------------------------------------------------


# Readmission
res_readmit <- run_surv_permimp_checkpointed(
  processed_datasets = processed_datasets,
  time_col  = "readmit_time_from_disch_m",
  event_col = "readmit_event",
  pred_cols = pred_cols,
  outcome_tag = "readmit",
  output_dir = output_dir,
  eval_frac = 0.20,
  eval_n_max = 4000,
  n_repeats = 1,
  chunk_size = 25,
  num_trees = 200,
  min_node_size = 30,
  sample_fraction = 0.632,
  num_threads = 1,
  save_model = FALSE
)

importance_pooled <- res_readmit$pooled
print(head(importance_pooled, 20))
imp_df <- tibble::enframe(importance_pooled, name = "Variable", value = "Importance")


# Death
res_death <- run_surv_permimp_checkpointed(
  processed_datasets = processed_datasets,
  time_col  = "death_time_from_disch_m",
  event_col = "death_event",
  pred_cols = pred_cols,
  outcome_tag = "death",
  output_dir = output_dir,
  eval_frac = 0.20,
  eval_n_max = 4000,
  n_repeats = 1,
  chunk_size = 25,
  num_trees = 200,
  min_node_size = 30,
  sample_fraction = 0.632,
  num_threads = 1,
  save_model = FALSE
)

importance_pooled_death <- res_death$pooled
print(head(importance_pooled_death, 20))
imp_death_df <- tibble::enframe(importance_pooled_death, name = "Variable", value = "Importance")