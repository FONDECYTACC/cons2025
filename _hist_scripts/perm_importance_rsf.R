# job::job({
#   source(file.path(wdpath, "cons", "_hist_scripts", "perm_importance_rsf.R"))
# }, title = "Permutation")

#clean enviroment
rm(list = ls()); gc()

# Helpers ------------------------------------------------------------



# =========================
# Robust save (safer on crash)
# =========================
safe_saveRDS <- function(object, file) {
  tmp <- base::paste0(file, ".tmp")
  base::saveRDS(object, tmp)
  if (base::file.exists(file)) base::unlink(file)
  ok <- base::file.rename(tmp, file)
  if (!ok) {
    # fallback: try copy + delete
    ok2 <- base::file.copy(tmp, file, overwrite = TRUE)
    if (ok2) base::unlink(tmp)
  }
  base::invisible(TRUE)
}


# =========================
# Risk prediction (SCALAR, low RAM)
# =========================
predict_risk_surv <- function(fit, newdata) {
  # For ranger survival: type="response" returns a scalar "mortality"/risk
  pr <- ranger::predict(fit, data = newdata, type = "response")
  base::as.numeric(pr$predictions)
}


# =========================
# Harrell C-index
# =========================
cindex_harrell <- function(time, event, risk) {
  ok <- base::is.finite(time) & !base::is.na(event) & base::is.finite(risk)
  time <- time[ok]
  event <- event[ok]
  risk <- risk[ok]
  
  if (base::length(time) < 50) return(NA_real_)
  
  # event should be 0/1
  event <- base::as.integer(event)
  
  # survival::concordance returns list with $concordance
  survival::concordance(survival::Surv(time, event) ~ risk)$concordance
}


# =========================
# Pool named vectors (mean, aligned by names)
# =========================
pool_named_vectors_mean <- function(vlist) {
  vlist <- base::Filter(function(x) !base::is.null(x) && base::length(x) > 0, vlist)
  all_names <- base::sort(base::unique(base::unlist(base::lapply(vlist, base::names))))
  if (base::length(all_names) == 0) {
    return(stats::setNames(base::numeric(0), base::character(0)))
  }
  
  mat <- base::vapply(
    vlist,
    FUN = function(v) {
      out <- stats::setNames(base::rep(NA_real_, base::length(all_names)), all_names)
      out[base::names(v)] <- base::as.numeric(v)
      out
    },
    FUN.VALUE = base::rep(NA_real_, base::length(all_names))
  )
  
  base::rowMeans(mat, na.rm = TRUE)
}


# Import RDS ------------------------------------------------------------

wdpath<- paste0(gsub("/cons","",gsub("cons","",paste0(getwd(),"/cons"))))

warning(wdpath)

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


output_dir <- file.path(wdpath, "data/20241015_out/pred1")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


## Function ------------------------------------------------------------

# =========================
# User inputs / column sets
# =========================


# =========================
# Main runner: fit + checkpointed permutation importance (resumable)
# =========================
run_surv_permimp_checkpointed <- function(
    processed_datasets,
    time_col, event_col,
    pred_cols,
    outcome_tag,
    # For your size (~80k rows, 72 predictors), these are safe defaults:
    eval_frac   = 0.10,
    eval_n_max  = 5000,
    n_repeats   = 1,
    chunk_size  = 12,
    seed        = 2125,
    # ranger knobs
    num_trees       = 200,
    min_node_size   = 30,
    sample_fraction = 0.632,
    mtry            = NULL,
    num_threads     = 1,
    save_model      = TRUE
) {
  if (base::is.null(mtry)) {
    mtry <- base::max(5L, base::floor(base::sqrt(base::length(pred_cols)) / 2))
  }
  
  per_imp_results <- base::vector("list", base::length(processed_datasets))
  
  for (i in base::seq_along(processed_datasets)) {
    
    base::cat("\n========================================\n")
    base::cat("Outcome:", outcome_tag, "| imputation", i, "of", base::length(processed_datasets), "\n")
    base::cat("========================================\n")
    
    base::gc(full = TRUE)
    
    final_file <- base::sprintf("permimp_%s_imp_%03d.rds", outcome_tag, i)
    ckpt_file  <- base::sprintf("permimp_%s_imp_%03d_ckpt.rds", outcome_tag, i)
    model_file <- base::sprintf("ranger_%s_imp_%03d.rds", outcome_tag, i)
    
    # Idempotent: if final exists, load and skip
    if (base::file.exists(final_file)) {
      base::cat("Found final result -> loading:", final_file, "\n")
      per_imp_results[[i]] <- base::readRDS(final_file)
      next
    }
    
    imp_data <- processed_datasets[[i]]
    
    needed <- base::c(time_col, event_col, pred_cols)
    missing_cols <- base::setdiff(needed, base::names(imp_data))
    if (base::length(missing_cols) > 0) {
      base::warning("Imputation ", i, " missing columns: ", base::paste(missing_cols, collapse = ", "))
      next
    }
    
    # Filter only on outcome availability (predictors assumed imputed)
    ok_outcome <- base::is.finite(imp_data[[time_col]]) & !base::is.na(imp_data[[event_col]])
    imp_data2 <- imp_data[ok_outcome, ]
    
    n <- base::nrow(imp_data2)
    if (base::is.null(n) || n < 200) {
      base::warning("Too few rows after outcome filtering in imputation ", i)
      next
    }
    
    # Evaluation subset (caps runtime)
    base::set.seed(seed + i)
    eval_n <- base::min(eval_n_max, base::max(500L, base::floor(n * eval_frac)))
    eval_idx <- base::sample.int(n, size = eval_n, replace = FALSE)
    
    # data.table-safe subsetting
    if (data.table::is.data.table(imp_data2)) {
      train_dt <- imp_data2[, base::c(time_col, event_col, pred_cols), with = FALSE]
      train_df <- base::as.data.frame(train_dt)
      
      eval_x <- imp_data2[eval_idx, pred_cols, with = FALSE]
      eval_x <- base::as.data.frame(eval_x)
    } else {
      train_df <- imp_data2[, base::c(time_col, event_col, pred_cols), drop = FALSE]
      train_df <- base::as.data.frame(train_df)
      
      eval_x <- imp_data2[eval_idx, pred_cols, drop = FALSE]
      eval_x <- base::as.data.frame(eval_x)
    }
    
    eval_time  <- base::as.numeric(imp_data2[[time_col]][eval_idx])
    eval_event <- base::as.integer(imp_data2[[event_col]][eval_idx])
    
    # Fit or load model (saving model makes resume truly failproof)
    fit <- NULL
    if (base::file.exists(model_file)) {
      base::cat("Loading saved model:", model_file, "\n")
      fit <- base::readRDS(model_file)
    } else {
      base::cat("Fitting ranger survival forest...\n")
      
      fit <- base::tryCatch(
        ranger::ranger(
          formula = stats::as.formula(
            base::paste0(
              "survival::Surv(", time_col, ", ", event_col, ") ~ ",
              base::paste(pred_cols, collapse = " + ")
            )
          ),
          data = train_df,
          num.trees = num_trees,
          min.node.size = min_node_size,
          sample.fraction = sample_fraction,
          mtry = mtry,
          splitrule = "logrank",
          importance = "none",      # we do permutation ourselves (checkpointable)
          write.forest = TRUE,      # required for prediction
          save.memory = TRUE,
          num.threads = num_threads,
          oob.error = FALSE,        # saves time/memory; we compute eval C-index ourselves
          seed = seed + i,
          verbose = TRUE
        ),
        error = function(e) {
          base::warning("Model fit failed for imputation ", i, ": ", base::conditionMessage(e))
          NULL
        }
      )
      
      if (base::is.null(fit)) next
      
      if (isTRUE(save_model)) {
        safe_saveRDS(fit, model_file)
        base::cat("Saved model:", model_file, "\n")
      }
    }
    
    # Baseline C-index on eval subset
    base_risk <- base::tryCatch(
      predict_risk_surv(fit, eval_x),
      error = function(e) {
        base::warning("Baseline prediction failed: ", base::conditionMessage(e))
        base::rep(NA_real_, base::nrow(eval_x))
      }
    )
    
    base_c <- cindex_harrell(eval_time, eval_event, base_risk)
    if (!base::is.finite(base_c)) {
      base::warning("Baseline C-index not finite for imputation ", i, " (outcome=", outcome_tag, ")")
      next
    }
    
    base::cat("Baseline C-index (eval subset) =", base::round(base_c, 4), "\n")
    
    # Resume checkpoint if present
    done <- stats::setNames(base::rep(FALSE, base::length(pred_cols)), pred_cols)
    imp_vec <- stats::setNames(base::rep(NA_real_, base::length(pred_cols)), pred_cols)
    
    if (base::file.exists(ckpt_file)) {
      ck <- base::readRDS(ckpt_file)
      if (!base::is.null(ck$importance) && base::length(ck$importance) > 0) {
        imp_vec[base::names(ck$importance)] <- ck$importance
        done[base::names(ck$importance)] <- TRUE
        base::cat("Resuming from checkpoint; already computed:", base::sum(done), "variables.\n")
      }
    }
    
    vars_left <- base::names(done)[!done]
    if (base::length(vars_left) > 0) {
      base::cat("Computing permutation importance for", base::length(vars_left), "variables...\n")
    } else {
      base::cat("Nothing left to compute; writing final.\n")
    }
    
    x_work <- eval_x
    
    for (start in base::seq(1L, base::length(vars_left), by = chunk_size)) {
      chunk <- vars_left[start:base::min(base::length(vars_left), start + chunk_size - 1L)]
      base::cat("  Chunk", base::ceiling(start / chunk_size), "/", base::ceiling(base::length(vars_left) / chunk_size),
                "(", base::length(chunk), "vars )\n")
      
      for (v in chunk) {
        orig <- x_work[[v]]
        
        perm_cs <- base::rep(NA_real_, n_repeats)
        
        for (b in base::seq_len(n_repeats)) {
          base::set.seed(seed + i * 100000L + start * 1000L + b)
          
          # permute (keep NAs in place naturally via sample)
          x_work[[v]] <- base::sample(orig, base::length(orig), replace = FALSE)
          
          prisk <- base::tryCatch(
            predict_risk_surv(fit, x_work),
            error = function(e) base::rep(NA_real_, base::nrow(x_work))
          )
          
          perm_cs[b] <- cindex_harrell(eval_time, eval_event, prisk)
        }
        
        # restore
        x_work[[v]] <- orig
        
        perm_c <- base::mean(perm_cs, na.rm = TRUE)
        
        # Importance = drop in C-index (positive = important)
        imp_vec[v] <- base_c - perm_c
        done[v] <- TRUE
        
        base::cat("    ", v, "-> ΔC =", base::round(imp_vec[v], 6), "\n")
      }
      
      # Checkpoint every chunk (critical)
      safe_saveRDS(
        list(
          outcome = outcome_tag,
          imputation = i,
          baseline_c = base_c,
          eval_n = eval_n,
          importance = imp_vec[done]
        ),
        ckpt_file
      )
      
      base::gc(full = TRUE)
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
    
    safe_saveRDS(out, final_file)
    base::cat("Saved final:", final_file, "\n")
    
    # cleanup
    base::rm(fit, imp_data, imp_data2, train_df, eval_x, x_work, base_risk)
    base::gc(full = TRUE)
    
    per_imp_results[[i]] <- out
  }
  
  # Pool (mean across imputations)
  pooled <- pool_named_vectors_mean(base::lapply(per_imp_results, `[[`, "importance"))
  pooled <- base::sort(pooled, decreasing = TRUE)
  
  pooled_file <- base::sprintf("pooled_permimp_%s.rds", outcome_tag)
  safe_saveRDS(list(pooled_importance = pooled, per_imputation = per_imp_results), pooled_file)
  base::cat("\nSaved pooled:", pooled_file, "\n")
  
  base::invisible(list(pooled = pooled, per_imputation = per_imp_results))
}



# Apply models ------------------------------------------------------------



res_readmit <- run_surv_permimp_checkpointed(
  processed_datasets = processed_datasets,
  time_col   = "readmit_time_from_disch_m",
  event_col  = "readmit_event",
  pred_cols  = pred_cols,
  outcome_tag = "readmit",
  eval_frac  = 0.10,
  eval_n_max = 5000,
  n_repeats  = 1,
  chunk_size = 12,
  num_trees  = 200,
  min_node_size = 50, #Readmission: try min.node.size = 40–60
  sample_fraction = 0.632,
  num_threads = 1,
  save_model = TRUE
)

importance_pooled <- res_readmit$pooled
base::print(base::head(importance_pooled, 20))
imp_df <- tibble::enframe(importance_pooled, name = "Variable", value = "Importance")


res_death <- run_surv_permimp_checkpointed(
  processed_datasets = processed_datasets,
  time_col   = "death_time_from_disch_m",
  event_col  = "death_event",
  pred_cols  = pred_cols,
  outcome_tag = "death",
  eval_frac  = 0.10,
  eval_n_max = 5000,
  n_repeats  = 1,
  chunk_size = 12,
  num_trees  = 200,
  min_node_size = 160, #Death: try min.node.size = 120–200
  sample_fraction = 0.632,
  num_threads = 1,
  save_model = TRUE
)

importance_pooled_death <- res_death$pooled
base::print(base::head(importance_pooled_death, 20))
imp_death_df <- tibble::enframe(importance_pooled_death, name = "Variable", value = "Importance")


#~3 hrs