# =============================================================================
# consolidate_audit_ama.R
# Consolidate the four full-sample audit tables into a single publication-style
# (AMA-style) table and export as CSV + Excel.
# Input:  cons/_out/audit_full_sample_notebook.rds
# Output: cons/_out/audit_full_sample_consolidated_ama.csv
#         cons/_out/audit_full_sample_consolidated_ama.xlsx
# =============================================================================

library(dplyr)
library(writexl)

project_root <- normalizePath(getwd(), winslash = "/")
out_dir <- file.path(project_root, "cons", "_out")

obj <- readRDS(file.path(out_dir, "audit_full_sample_notebook.rds"))
pooled <- obj$pooled

make_rows <- function(tbl, transition_label, audit_label, subgroup_var = NA_character_) {
  vars <- setdiff(names(tbl), c("n_at_risk", "n_events"))
  df <- as.data.frame(tbl)
  if (length(vars) == 1L) {
    names(df)[names(df) == vars] <- "plan_type_strata"
    df$subgroup_value <- NA_character_
  } else {
    names(df)[names(df) == vars[1]] <- "plan_type_strata"
    names(df)[names(df) == vars[2]] <- "subgroup_value"
  }
  df$transition <- transition_label
  df$audit <- audit_label
  df$subgroup_variable <- subgroup_var
  df$event_rate_pct <- round(100 * df$n_events / df$n_at_risk, 1)
  df$subgroup_value <- as.character(df$subgroup_value)
  df[, c("transition", "audit", "plan_type_strata", "subgroup_variable",
         "subgroup_value", "n_at_risk", "n_events", "event_rate_pct")]
}

consolidated <- bind_rows(
  make_rows(pooled$d1, "1→2 Readmission", "plan_type_strata"),
  make_rows(pooled$d1_2strata, "1→2 Readmission", "plan_type_strata + rule violation",
            "tr_outcome_adm_discharge_rule_violation_undet"),
  make_rows(pooled$d2, "1→3 Death", "plan_type_strata + any_phys_dx",
            "any_phys_dx"),
  make_rows(pooled$d3, "2→3 Death after readmission", "plan_type_strata + any_phys_dx",
            "any_phys_dx")
)

# Convert subgroup_value to character for clean printing
consolidated$subgroup_value <- as.character(consolidated$subgroup_value)

# Order transitions and plan_type_strata
consolidated$transition <- factor(
  consolidated$transition,
  levels = c("1→2 Readmission", "1→3 Death", "2→3 Death after readmission")
)
consolidated$plan_type_strata <- factor(
  consolidated$plan_type_strata,
  levels = c("pg-pr", "m-pr", "pg-pai", "m-pai", "pg-pab")
)
consolidated <- consolidated %>%
  mutate(n_at_risk = round(n_at_risk),
         n_events = round(n_events),
         event_rate_pct = round(100 * n_events / n_at_risk, 1)) %>%
  arrange(transition, audit, plan_type_strata, subgroup_value)

cat("Consolidated table:\n")
print(as.data.frame(consolidated), row.names = FALSE)

write.csv(consolidated,
          file.path(out_dir, "audit_full_sample_consolidated_ama.csv"),
          row.names = FALSE)

write_xlsx(consolidated,
           file.path(out_dir, "audit_full_sample_consolidated_ama.xlsx"))

cat("\nSaved:\n")
cat("  ", file.path(out_dir, "audit_full_sample_consolidated_ama.csv"), "\n")
cat("  ", file.path(out_dir, "audit_full_sample_consolidated_ama.xlsx"), "\n")
