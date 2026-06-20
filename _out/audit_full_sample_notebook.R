# =============================================================================
# audit_full_sample_notebook.R
# Run the notebook's audit() on the FULL SAMPLE using the notebook's own
# full-sample construction (rbind train+val from build_holdout_datasets).
# Does NOT overwrite any file starting with "prediction26_".
#
# Writes:
#   cons/_out/audit_full_sample_notebook.rds
#   cons/_out/audit_full_sample_notebook_d1.csv
#   cons/_out/audit_full_sample_notebook_d1_2strata.csv
#   cons/_out/audit_full_sample_notebook_d2.csv
#   cons/_out/audit_full_sample_notebook_d3.csv
# =============================================================================

library(survival)
library(mstate)
library(dplyr)

project_root <- normalizePath(getwd(), winslash = "/")
out_dir <- file.path(project_root, "cons", "_out")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

source(file.path(project_root, "cons/_alt_scripts/val_holdout_02_build_sets.R"))

cat("Building holdout datasets...\n")
hd <- build_holdout_datasets(force = FALSE, verify = TRUE, verbose = FALSE)

test_comp <- hd$test %||% hd$val %||% hd$holdout %||% hd$test_sets

full_imp <- lapply(seq_along(hd$train), function(i) {
  d <- if (!is.null(test_comp))
    rbind(as.data.frame(hd$train[[i]]), as.data.frame(test_comp[[i]]))
  else
    as.data.frame(hd$train[[i]])
  # logical -> factor, exactly as the notebook does
  lg <- names(d)[vapply(d, is.logical, logical(1))]
  for (v in lg) d[[v]] <- factor(d[[v]], levels = c("FALSE", "TRUE"))
  d
})

cat(sprintf("Full sample: %d imputations x %d rows\n",
            length(full_imp), nrow(full_imp[[1]])))

STRATA_READMIT <- "plan_type_strata"
STRATA_READMIT2 <- c("plan_type_strata", "tr_outcome_adm_discharge_rule_violation_undet")
STRATA_DEATH   <- c("plan_type_strata", "any_phys_dx")
KEEP_VARS <- unique(c(STRATA_READMIT, STRATA_READMIT2, STRATA_DEATH))

tmat <- transMat(x = list(c(2, 3), c(3), c()),
                 names = c("Discharge", "Readmission", "Death"))

eps <- 1e-4

audit <- function(d, vars) {
  d %>%
    group_by(across(all_of(vars))) %>%
    summarise(n_at_risk = n(), n_events = sum(status), .groups = "drop") %>%
    arrange(n_events)
}

run_one <- function(d) {
  bad <- with(d, readmit_event == 1 & death_event == 1 &
                death_time_from_disch_m < readmit_time_from_disch_m)
  if (any(bad)) {
    d$readmit_event[bad] <- 0
    d$readmit_time_from_disch_m[bad] <- d$death_time_from_disch_m[bad]
  }
  tie <- with(d, readmit_event == 1 & death_event == 1 &
                death_time_from_disch_m == readmit_time_from_disch_m)
  if (any(tie)) d$death_time_from_disch_m[tie] <- d$death_time_from_disch_m[tie] + eps

  keep <- KEEP_VARS[KEEP_VARS %in% names(d)]

  ms <- msprep(time = c(NA, "readmit_time_from_disch_m", "death_time_from_disch_m"),
               status = c(NA, "readmit_event", "death_event"),
               data = d, trans = tmat, keep = keep)

  d1 <- ms[ms$trans == 1, ]
  d2 <- ms[ms$trans == 2, ]
  d3 <- ms[ms$trans == 3, ]

  list(
    d1         = audit(d1, STRATA_READMIT),
    d1_2strata = audit(d1, STRATA_READMIT2),
    d2         = audit(d2, STRATA_DEATH),
    d3         = audit(d3, STRATA_DEATH),
    events     = c(d1 = sum(d1$status), d2 = sum(d2$status), d3 = sum(d3$status))
  )
}

cat("Running audits per imputation...\n")
per_imp <- lapply(full_imp, run_one)
names(per_imp) <- paste0("imp", seq_along(per_imp))

cat("Events per transition (per imputation):\n")
print(do.call(rbind, lapply(per_imp, `[[`, "events")))

pool_audit <- function(audit_list, name) {
  bind_rows(lapply(audit_list, `[[`, name), .id = "imp") %>%
    group_by(across(any_of(setdiff(names(.), c("imp", "n_at_risk", "n_events"))))) %>%
    summarise(n_at_risk = mean(n_at_risk),
              n_events  = mean(n_events),
              .groups = "drop") %>%
    arrange(n_events)
}

pooled <- list(
  d1         = pool_audit(per_imp, "d1"),
  d1_2strata = pool_audit(per_imp, "d1_2strata"),
  d2         = pool_audit(per_imp, "d2"),
  d3         = pool_audit(per_imp, "d3")
)

cat("\n=== Pooled audit (full sample via hd$train+val, mean over 5 imputations) ===\n")
for (nm in names(pooled)) {
  cat("\n--", nm, "--\n")
  print(pooled[[nm]], row.names = FALSE, n = Inf)
}

saveRDS(list(per_imp = per_imp, pooled = pooled),
        file.path(out_dir, "audit_full_sample_notebook.rds"))

for (nm in names(pooled)) {
  write.csv(pooled[[nm]],
            file.path(out_dir, paste0("audit_full_sample_notebook_", nm, ".csv")),
            row.names = FALSE)
}

cat("\nSaved to:\n")
cat("  ", file.path(out_dir, "audit_full_sample_notebook.rds"), "\n")
cat("  ", file.path(out_dir, "audit_full_sample_notebook_*.csv"), "\n")
