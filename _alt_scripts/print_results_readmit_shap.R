# Generic print function for pooled CV+MI survival analysis results

print_pooled_results <- function(x, ...) {
  if (missing(x) || is.null(x)) {
    cat('Error: No object provided\n')
    return(invisible(NULL))
  }

  cat('\n============================================================\n')
  cat('FINAL POOLED RESULTS (Cross-Validated + Multiply Imputed)\n')
  cat('============================================================\n')

  tau <- NULL
  if (!is.null(x[['reproducibility']]) && !is.null(x[['reproducibility']][['tau']])) {
    tau <- x[['reproducibility']][['tau']]
  }
  if (!is.null(tau)) {
    cat('\nGlobal Evaluation Time (tau):', round(tau, 2), 'months\n')
  }

  get_mean <- function(name) {
    if (!is.null(x[['repeat_values']]) && !is.null(x[['repeat_values']][[name]])) {
      return(mean(x[['repeat_values']][[name]], na.rm = TRUE))
    }
    return(NULL)
  }

  c_mean <- get_mean('c')
  if (!is.null(c_mean)) {
    cat('\nPooled Uno C-Index:', round(c_mean, 4))
    if (!is.null(x[['ci95_pooled_c']]) && length(x[['ci95_pooled_c']]) >= 2) {
      cat(' (repeat-quantile interval', round(x[['ci95_pooled_c']][1], 4), '-', round(x[['ci95_pooled_c']][2], 4), ')')
    }
    cat('\n')
  }

  unoC_mean <- get_mean('unoC')
  if (!is.null(unoC_mean)) {
    cat('Pooled UnoC (survAUC::UnoC):', round(unoC_mean, 4))
    if (!is.null(x[['ci95_pooled_unoC']]) && length(x[['ci95_pooled_unoC']]) >= 2) {
      cat(' (repeat-quantile interval', round(x[['ci95_pooled_unoC']][1], 4), '-', round(x[['ci95_pooled_unoC']][2], 4), ')')
    }
    cat('\n')
  }

  ibs_mean <- get_mean('ibs')
  if (!is.null(ibs_mean)) {
    cat('Pooled IBS:', round(ibs_mean, 4))
    if (!is.null(x[['ci95_pooled_ibs']]) && length(x[['ci95_pooled_ibs']]) >= 2) {
      cat(' (repeat-quantile interval', round(x[['ci95_pooled_ibs']][1], 4), '-', round(x[['ci95_pooled_ibs']][2], 4), ')')
    }
    cat('\n')
  }

  ibs_ipcw_mean <- get_mean('ibs_ipcw')
  if (!is.null(ibs_ipcw_mean)) {
    cat('Pooled IBS (IPCW-train):', round(ibs_ipcw_mean, 4))
    if (!is.null(x[['ci95_pooled_ibs_ipcw']]) && length(x[['ci95_pooled_ibs_ipcw']]) >= 2) {
      cat(' (repeat-quantile interval', round(x[['ci95_pooled_ibs_ipcw']][1], 4), '-', round(x[['ci95_pooled_ibs_ipcw']][2], 4), ')')
    }
    cat('\n')
  }

  rmst_summary <- NULL
  if (!is.null(x[['reproducibility']]) && !is.null(x[['reproducibility']][['rmst_unoC_summary']])) {
    rmst_summary <- x[['reproducibility']][['rmst_unoC_summary']]
  }
  if (!is.null(rmst_summary) && nrow(rmst_summary) > 0) {
    cat('\nRMST-based Uno C-index by horizon:\n')
    for (i in seq_len(nrow(rmst_summary))) {
      h <- rmst_summary[['horizon_month']][i]
      c_idx <- rmst_summary[['c_mean']][i]
      ci_low <- rmst_summary[['ci95_lower']][i]
      ci_high <- rmst_summary[['ci95_upper']][i]
      valid <- rmst_summary[['valid_repeats']][i]
      cat(sprintf('  t=%2d m: C=%.4f (repeat-quantile interval %.4f-%.4f, valid repeats=%d)\n',
                  h, c_idx, ci_low, ci_high, valid))
    }
  }

  cat('============================================================\n')
  return(invisible(x))
}

view_pooled_results <- function(x = NULL, obj_name = 'results_readmit_shap') {
  if (!is.null(x)) {
    return(print_pooled_results(x))
  }
  if (exists(obj_name, envir = parent.frame())) {
    x <- get(obj_name, envir = parent.frame())
    return(print_pooled_results(x))
  }
  if (exists(obj_name, envir = .GlobalEnv)) {
    x <- get(obj_name, envir = .GlobalEnv)
    return(print_pooled_results(x))
  }
  cat('Object not found:', obj_name, '\n')
  return(invisible(NULL))
}

prs <- function(x = NULL) {
  if (!is.null(x)) {
    return(print_pooled_results(x))
  }
  if (exists('results_readmit_shap', envir = parent.frame())) {
    x <- get('results_readmit_shap', envir = parent.frame())
    return(print_pooled_results(x))
  }
  if (exists('results_readmit_shap', envir = .GlobalEnv)) {
    x <- get('results_readmit_shap', envir = .GlobalEnv)
    return(print_pooled_results(x))
  }
  cat('results_readmit_shap not found. Use: prs(your_object)\n')
  return(invisible(NULL))
}

print.pooled_cv_results <- function(x, ...) {
  print_pooled_results(x, ...)
}

cat('print_results_readmit_shap.R loaded. Functions: print_pooled_results, view_pooled_results, prs\n')
invisible(print_pooled_results)