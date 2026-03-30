# Simple NRI and IDI calculation from CSV files
# Alternative version that does not require reticulate
#
# Usage:
# source("cons/_hist_scripts/nri_idi_simple.R")
#
# results <- calculate_nri_idi_from_csv(
#   csv_path = "path/to/predictions.csv",
#   col_time = "time",
#   col_event = "event", 
#   col_pred_m1 = "pred_cox",
#   col_pred_m2 = "pred_xgb",
#   cutoff = 0.15
# )

#' Calculate NRI and IDI
calculate_nri_idi <- function(time, event, pred_m1, pred_m2, cutoff = 0.15) {
  
  n <- length(time)
  stopifnot(length(event) == n, length(pred_m1) == n, length(pred_m2) == n)
  
  # Keep complete cases
  keep <- is.finite(time) & is.finite(event) & is.finite(pred_m1) & is.finite(pred_m2)
  time <- time[keep]
  event <- as.integer(event[keep])
  pred_m1 <- as.numeric(pred_m1[keep])
  pred_m2 <- as.numeric(pred_m2[keep])
  n <- length(time)
  
  if (n < 10) stop("Insufficient data (n < 10)")
  
  n_events <- sum(event == 1)
  n_nonevents <- sum(event == 0)
  
  if (n_events == 0 || n_nonevents == 0) {
    stop("Need both events and non-events")
  }
  
  # Categorize
  cat1 <- ifelse(pred_m1 >= cutoff, 1L, 0L)
  cat2 <- ifelse(pred_m2 >= cutoff, 1L, 0L)
  
  # Reclassification tables
  reclass_events <- table(Model1 = cat1[event == 1], Model2 = cat2[event == 1])
  reclass_nonevents <- table(Model1 = cat1[event == 0], Model2 = cat2[event == 0])
  
  # NRI components
  up_events <- sum(event == 1 & cat2 > cat1)
  down_events <- sum(event == 1 & cat2 < cat1)
  nri_events <- (up_events - down_events) / n_events
  
  up_nonevents <- sum(event == 0 & cat2 > cat1)
  down_nonevents <- sum(event == 0 & cat2 < cat1)
  nri_nonevents <- (down_nonevents - up_nonevents) / n_nonevents
  
  nri <- nri_events + nri_nonevents
  
  # Standard errors
  se_nri_events <- sqrt((up_events + down_events) / n_events^2)
  se_nri_nonevents <- sqrt((up_nonevents + down_nonevents) / n_nonevents^2)
  se_nri <- sqrt(se_nri_events^2 + se_nri_nonevents^2)
  
  z_nri <- nri / se_nri
  p_nri <- 2 * (1 - pnorm(abs(z_nri)))
  
  # IDI
  mean_m1_events <- mean(pred_m1[event == 1])
  mean_m1_nonevents <- mean(pred_m1[event == 0])
  mean_m2_events <- mean(pred_m2[event == 1])
  mean_m2_nonevents <- mean(pred_m2[event == 0])
  
  idi <- (mean_m2_events - mean_m2_nonevents) - (mean_m1_events - mean_m1_nonevents)
  idi_relative <- idi / (mean_m1_events - mean_m1_nonevents)
  
  list(
    n = n,
    n_events = n_events,
    n_nonevents = n_nonevents,
    cutoff = cutoff,
    nri = nri,
    nri_events = nri_events,
    nri_nonevents = nri_nonevents,
    se_nri = se_nri,
    z_nri = z_nri,
    p_nri = p_nri,
    idi = idi,
    idi_relative = idi_relative,
    mean_m1_events = mean_m1_events,
    mean_m1_nonevents = mean_m1_nonevents,
    mean_m2_events = mean_m2_events,
    mean_m2_nonevents = mean_m2_nonevents,
    up_events = up_events,
    down_events = down_events,
    up_nonevents = up_nonevents,
    down_nonevents = down_nonevents,
    reclass_events = reclass_events,
    reclass_nonevents = reclass_nonevents
  )
}

#' Bootstrap confidence intervals
bootstrap_nri_idi <- function(time, event, pred_m1, pred_m2, cutoff = 0.15, 
                               n_boot = 1000, seed = 42) {
  set.seed(seed)
  n <- length(time)
  
  boot_nri <- numeric(n_boot)
  boot_idi <- numeric(n_boot)
  
  for (i in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    tryCatch({
      res <- calculate_nri_idi(time[idx], event[idx], pred_m1[idx], pred_m2[idx], cutoff)
      boot_nri[i] <- res$nri
      boot_idi[i] <- res$idi
    }, error = function(e) {
      boot_nri[i] <- NA
      boot_idi[i] <- NA
    })
  }
  
  data.frame(
    metric = c("NRI", "IDI"),
    estimate = c(mean(boot_nri, na.rm = TRUE), mean(boot_idi, na.rm = TRUE)),
    ci_lower = c(quantile(boot_nri, 0.025, na.rm = TRUE), 
                 quantile(boot_idi, 0.025, na.rm = TRUE)),
    ci_upper = c(quantile(boot_nri, 0.975, na.rm = TRUE),
                 quantile(boot_idi, 0.975, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
}

#' Calculate from CSV
calculate_nri_idi_from_csv <- function(
    csv_path,
    col_time = "time",
    col_event = "event",
    col_pred_m1 = "pred_cox",
    col_pred_m2 = "pred_xgb",
    cutoff = 0.15,
    n_boot = 1000
) {
  
  if (!file.exists(csv_path)) {
    stop("File not found: ", csv_path)
  }
  
  message("Loading: ", csv_path)
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  required <- c(col_time, col_event, col_pred_m1, col_pred_m2)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
  
  results <- calculate_nri_idi(
    time = df[[col_time]],
    event = df[[col_event]],
    pred_m1 = df[[col_pred_m1]],
    pred_m2 = df[[col_pred_m2]],
    cutoff = cutoff
  )
  
  if (n_boot > 0) {
    message("Bootstrapping...")
    results$bootstrap <- bootstrap_nri_idi(
      time = df[[col_time]],
      event = df[[col_event]],
      pred_m1 = df[[col_pred_m1]],
      pred_m2 = df[[col_pred_m2]],
      cutoff = cutoff,
      n_boot = n_boot
    )
  }
  
  print_nri_idi(results)
  invisible(results)
}

#' Print results
print_nri_idi <- function(results) {
  cat("\n========== NRI/IDI Results ==========\n")
  cat("N:", results$n, "(Events:", results$n_events, ")\n")
  cat("Cutoff:", results$cutoff, "\n\n")
  
  cat("NRI:", round(results$nri, 4))
  if (!is.null(results$bootstrap)) {
    nri_row <- results$bootstrap[results$bootstrap$metric == "NRI", ]
    cat(" [", round(nri_row$ci_lower, 4), ", ", round(nri_row$ci_upper, 4), "]", sep = "")
  }
  cat("\n")
  cat("  Events:", round(results$nri_events, 4), "\n")
  cat("  Non-events:", round(results$nri_nonevents, 4), "\n")
  cat("  p-value:", format.pval(results$p_nri, eps = 0.001), "\n\n")
  
  cat("IDI:", round(results$idi, 4))
  if (!is.null(results$bootstrap)) {
    idi_row <- results$bootstrap[results$bootstrap$metric == "IDI", ]
    cat(" [", round(idi_row$ci_lower, 4), ", ", round(idi_row$ci_upper, 4), "]", sep = "")
  }
  cat("\n")
  cat("  Relative:", round(results$idi_relative * 100, 2), "%\n\n")
  
  cat("Reclassification - Events:\n")
  print(results$reclass_events)
  cat("\nReclassification - Non-events:\n")
  print(results$reclass_nonevents)
  cat("=====================================\n\n")
}

message("NRI/IDI functions loaded.")
message("Usage: calculate_nri_idi_from_csv('predictions.csv', cutoff = 0.15)")
