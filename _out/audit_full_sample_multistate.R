# =============================================================================
# audit_full_sample_multistate.R
# Run the notebook's audit() on the FULL SAMPLE (88,152 x 5 imputations)
# WITHOUT touching any file starting with "prediction26_".
#
# Reads: data/20241015_out/corrected_datasets_nondum_filt.rds
# Writes: cons/_out/audit_full_sample_multistate.rds
#         cons/_out/audit_full_sample_multistate_d1.csv
#         cons/_out/audit_full_sample_multistate_d1_2strata.csv
#         cons/_out/audit_full_sample_multistate_d2.csv
#         cons/_out/audit_full_sample_multistate_d3.csv
# =============================================================================

project_root <- normalizePath(getwd(), winslash = "/")
out_dir <- file.path(project_root, "cons", "_out")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat("Reading full non-dummy imputations...\n")
imp_list <- readRDS(file.path(project_root, "data/20241015_out/corrected_datasets_nondum_filt.rds"))
cat("Imputations:", length(imp_list), "| rows:", nrow(imp_list[[1]]), "\n")

make_ms <- function(d) {
  eps <- 1e-4

  # Derive the same strata / covariates the notebook uses
  d$plan_type_strata <- factor(
    d$plan_type_corr,
    levels = c("pg-pr", "m-pr", "pg-pai", "m-pai", "pg-pab")
  )
  d$tr_outcome_adm_discharge_rule_violation_undet <- as.integer(
    d$tr_outcome == "adm discharge - rule violation/undet"
  )
  d$any_phys_dx <- factor(d$any_phys_dx, levels = c("FALSE", "TRUE"))

  # Handle inconsistent/tied event times exactly like the notebook
  bad <- with(d,
              readmit_event == 1 & death_event == 1 &
                death_time_from_disch_m < readmit_time_from_disch_m)
  if (any(bad)) {
    d$readmit_event[bad] <- 0
    d$readmit_time_from_disch_m[bad] <- d$death_time_from_disch_m[bad]
  }
  tie <- with(d,
              readmit_event == 1 & death_event == 1 &
                death_time_from_disch_m == readmit_time_from_disch_m)
  if (any(tie)) d$death_time_from_disch_m[tie] <- d$death_time_from_disch_m[tie] + eps

  # Transition 1: Discharge -> Readmission (1 -> 2)
  d1 <- data.frame(
    status = d$readmit_event,
    plan_type_strata = d$plan_type_strata,
    tr_outcome_adm_discharge_rule_violation_undet = d$tr_outcome_adm_discharge_rule_violation_undet,
    any_phys_dx = d$any_phys_dx,
    stringsAsFactors = FALSE
  )

  # Transition 2: Discharge -> Death (1 -> 3)
  status2 <- with(d, as.integer(death_event == 1 & readmit_event == 0))
  d2 <- data.frame(
    status = status2,
    plan_type_strata = d$plan_type_strata,
    any_phys_dx = d$any_phys_dx,
    stringsAsFactors = FALSE
  )

  # Transition 3: Readmission -> Death (2 -> 3)
  ridx <- which(d$readmit_event == 1)
  d3 <- data.frame(
    status = d$death_event[ridx],
    plan_type_strata = d$plan_type_strata[ridx],
    any_phys_dx = d$any_phys_dx[ridx],
    stringsAsFactors = FALSE
  )

  list(d1 = d1, d2 = d2, d3 = d3)
}

audit <- function(df, vars) {
  res <- aggregate(
    list(n_at_risk = rep(1L, nrow(df)), n_events = df$status),
    by = df[, vars, drop = FALSE],
    FUN = sum
  )
  res <- res[order(res$n_events), ]
  rownames(res) <- NULL
  res
}

run_one <- function(d) {
  ms <- make_ms(as.data.frame(d))
  list(
    d1         = audit(ms$d1, "plan_type_strata"),
    d1_2strata = audit(ms$d1, c("plan_type_strata",
                                "tr_outcome_adm_discharge_rule_violation_undet")),
    d2         = audit(ms$d2, c("plan_type_strata", "any_phys_dx")),
    d3         = audit(ms$d3, c("plan_type_strata", "any_phys_dx")),
    events     = c(d1 = sum(ms$d1$status),
                   d2 = sum(ms$d2$status),
                   d3 = sum(ms$d3$status))
  )
}

cat("Running audits per imputation...\n")
per_imp <- lapply(imp_list, run_one)
names(per_imp) <- paste0("imp", seq_along(per_imp))

cat("Events per transition (per imputation):\n")
print(do.call(rbind, lapply(per_imp, `[[`, "events")))

# Pool by averaging n_at_risk / n_events across the 5 imputations
pool_audit <- function(audit_list, name) {
  all <- do.call(rbind, lapply(names(audit_list), function(nm) {
    x <- audit_list[[nm]][[name]]
    x$imp <- nm
    x
  }))
  grp_vars <- setdiff(names(all), c("imp", "n_at_risk", "n_events"))
  res <- aggregate(list(n_at_risk = all$n_at_risk, n_events = all$n_events),
                   by = all[, grp_vars, drop = FALSE],
                   FUN = mean)
  res <- res[order(res$n_events), ]
  rownames(res) <- NULL
  res
}

pooled <- list(
  d1         = pool_audit(per_imp, "d1"),
  d1_2strata = pool_audit(per_imp, "d1_2strata"),
  d2         = pool_audit(per_imp, "d2"),
  d3         = pool_audit(per_imp, "d3")
)

cat("\n=== Pooled audit (full sample, mean over 5 imputations) ===\n")
for (nm in names(pooled)) {
  cat("\n--", nm, "--\n")
  print(pooled[[nm]], row.names = FALSE)
}

saveRDS(list(per_imp = per_imp, pooled = pooled),
        file.path(out_dir, "audit_full_sample_multistate.rds"))

for (nm in names(pooled)) {
  write.csv(pooled[[nm]],
            file.path(out_dir, paste0("audit_full_sample_multistate_", nm, ".csv")),
            row.names = FALSE)
}

cat("\nSaved to:\n")
cat("  ", file.path(out_dir, "audit_full_sample_multistate.rds"), "\n")
cat("  ", file.path(out_dir, "audit_full_sample_multistate_*.csv"), "\n")
