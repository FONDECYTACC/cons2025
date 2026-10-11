# =============================================================================
# annex_pdc_report_recovery_chunk.R  (2026-08-23)
#
# ONE recovery chunk for cons/prediction225_converted_mod.ipynb.
#
# WHAT WENT WRONG. The cell `annex-paired-delta-c-risk-ext-run` defined its
# column list as
#   if (!exists("ANNEX_PDC_REPORT_COLS", inherits = TRUE)) ANNEX_PDC_REPORT_COLS <- c(...)
# The guard was there so the reported columns could be overridden, but it also
# adopts a STALE value left in the kernel by the earlier ANALYTIC version of the
# same cell, whose list named se_delta, delta_C_lower and delta_C_upper. The
# bootstrap version does not produce those columns; it produces se_delta_ij,
# delta_ij_lower, delta_ij_upper, delta_boot_lower and delta_boot_upper. The
# first print therefore asked a data frame for columns it does not have and
# stopped with "undefined columns selected".
#
# NOTHING WAS LOST. The failure is in the REPORTING block, which runs after the
# bootstrap and after both tables are assembled, so annex_pdc_table,
# annex_pdc_bymodel, annex_pdc_boot, annex_pdc_skipped and
# annex_pdc_reconciliation are all sitting in the session. The bootstrap does not
# have to run again. What did NOT happen is the save, because it comes after the
# failed print, so this chunk prints the tables and then writes the CSVs and the
# .rds that the interrupted cell never got to.
#
# Placement: paste as its own cell immediately AFTER
# `annex-paired-delta-c-risk-ext-run`. Adds no new results; it reports and saves
# what is already in the session.
# =============================================================================

# ---------------------------------------------------------------------------
# CHUNK -- paste the block below as its own notebook cell
# ---------------------------------------------------------------------------
#| label: annex-paired-delta-c-risk-ext-report
#| message: true
#| warning: true
# ============================================================================
# REPORT AND SAVE the extended paired delta C table from objects already in the
# session. Recomputes nothing. See the file header for why the run cell stopped.
# Requires in kernel: annex_pdc_table and annex_pdc_bymodel from
# `annex-paired-delta-c-risk-ext-run`; the other objects are used if present.
# ============================================================================
.apdc_root0 <- if (exists(".apdc_root", inherits = TRUE) && is.character(.apdc_root)) .apdc_root else
  { p0 <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    sub("(/|\\)?cons(/|\\)?$", "", p0) }
.apdc_csv <- file.path(.apdc_root0, "cons", "_out")
# The tables are taken from the session when they are there, and otherwise
# re-read from the CSVs this chunk itself wrote, so it still works after a kernel
# restart. Either way nothing is recomputed.
if (!exists("annex_pdc_table", inherits = TRUE) || !is.data.frame(annex_pdc_table)) {
  .f <- file.path(.apdc_csv, "pred225_annex_paired_delta_c_risk_ext.csv")
  stopifnot("neither annex_pdc_table nor its CSV is available" = file.exists(.f))
  annex_pdc_table <- utils::read.csv(.f, stringsAsFactors = FALSE)
  message("annex-pdc report: annex_pdc_table re-read from ", basename(.f))
}
if (!exists("annex_pdc_bymodel", inherits = TRUE) || !is.data.frame(annex_pdc_bymodel)) {
  .f <- file.path(.apdc_csv, "pred225_annex_paired_delta_c_risk_ext_bymodel.csv")
  stopifnot("neither annex_pdc_bymodel nor its CSV is available" = file.exists(.f))
  annex_pdc_bymodel <- utils::read.csv(.f, stringsAsFactors = FALSE)
  message("annex-pdc report: annex_pdc_bymodel re-read from ", basename(.f))
}
if (!exists("ANNEX_PDC_PROMOTION_DELTA", inherits = TRUE)) ANNEX_PDC_PROMOTION_DELTA <- 0.01
# Defined UNCONDITIONALLY and then intersected with the columns the table really
# has, so neither a stale value from an earlier cell version nor a later column
# rename can break the printing again. This is the actual fix.
.apdc_wanted <- c("outcome", "comparison", "model_A", "family_A", "model_B", "family_B",
                  "horizon", "n", "events_by_horizon",
                  "C_A", "C_A_boot_lower", "C_A_boot_upper", "C_A_ij_lower", "C_A_ij_upper",
                  "C_B", "C_B_boot_lower", "C_B_boot_upper", "C_B_ij_lower", "C_B_ij_upper",
                  "delta_C", "delta_boot_lower", "delta_boot_upper", "boot_mean",
                  "centering_bias", "se_delta_ij", "delta_ij_lower", "delta_ij_upper")
# Family labels are normalised on the objects ALREADY in the session, so the
# manuscript name is used without recomputing anything. "prediction24" and
# "deepsurv_informed" were both used while these annexes were being written; the
# reported name is DeepSurv-informed.
.apdc_fam_map <- c(cox = "Cox", Cox = "Cox", prediction24 = "DeepSurv-informed",
                   deepsurv_informed = "DeepSurv-informed",
                   `DeepSurv-informed` = "DeepSurv-informed")
.apdc_relabel <- function(v) { out <- unname(.apdc_fam_map[as.character(v)])
  ifelse(is.na(out), as.character(v), out) }
for (.cn in c("family_A", "family_B"))
  if (.cn %in% names(annex_pdc_table)) annex_pdc_table[[.cn]] <- .apdc_relabel(annex_pdc_table[[.cn]])
if ("family" %in% names(annex_pdc_bymodel)) annex_pdc_bymodel$family <- .apdc_relabel(annex_pdc_bymodel$family)
if (exists("annex_pdc_skipped", inherits = TRUE) && is.data.frame(annex_pdc_skipped))
  for (.cn in c("family_A", "family_B"))
    if (.cn %in% names(annex_pdc_skipped)) annex_pdc_skipped[[.cn]] <- .apdc_relabel(annex_pdc_skipped[[.cn]])
if (all(c("family_A", "family_B") %in% names(annex_pdc_table)))
  annex_pdc_table$comparison <- ifelse(annex_pdc_table$family_A != annex_pdc_table$family_B,
                                       "Cox vs DeepSurv-informed",
                                       paste("within", annex_pdc_table$family_A))
cat("Family labels in use:", paste(sort(unique(c(annex_pdc_table$family_A, annex_pdc_table$family_B))), collapse = ", "), "
")
# ---- Each arm's OWN interval, attached to the pairwise table ---------------
# annex_pdc_bymodel already carries every arm's C with its bootstrap and analytic
# bounds, from the SAME replicates, so the pairwise table gets them by a join and
# nothing is recomputed. Joined on outcome, model and horizon, which is the key
# that identifies an arm-horizon cell uniquely.
# READ THIS BEFORE USING THE NEW COLUMNS. C_A and C_B with their own intervals are
# MARGINAL: they say how precisely each arm's C is estimated on its own. Two
# marginal intervals that overlap do NOT mean the difference is compatible with
# zero. The two markers are almost perfectly correlated, so the paired interval
# delta_boot_lower to delta_boot_upper is far narrower and is the only one that
# answers whether the models differ. Report the marginal bounds as description of
# each arm, never as the comparison.
.apdc_bm <- annex_pdc_bymodel[, intersect(c("outcome", "model", "horizon", "C_boot_lower",
                                            "C_boot_upper", "C_ij_lower", "C_ij_upper"),
                                          names(annex_pdc_bymodel)), drop = FALSE]
if (all(c("outcome", "model", "horizon") %in% names(.apdc_bm)) && nrow(.apdc_bm)) {
  .apdc_key3 <- function(o, m, h) paste(o, m, h, sep = "|")
  .apdc_src <- .apdc_key3(.apdc_bm$outcome, .apdc_bm$model, .apdc_bm$horizon)
  for (side in c("A", "B")) {
    idx <- match(.apdc_key3(annex_pdc_table$outcome, annex_pdc_table[[paste0("model_", side)]],
                            annex_pdc_table$horizon), .apdc_src)
    if (anyNA(idx))
      warning("annex-pdc report: ", sum(is.na(idx)), " rows of arm ", side,
              " found no match in annex_pdc_bymodel; their interval columns stay NA.", call. = FALSE)
    for (nm in c("C_boot_lower", "C_boot_upper", "C_ij_lower", "C_ij_upper"))
      if (nm %in% names(.apdc_bm))
        annex_pdc_table[[sub("^C_", paste0("C_", side, "_"), nm)]] <- .apdc_bm[[nm]][idx]
  }
  cat("Per-arm intervals attached to the pairwise table from annex_pdc_bymodel.
")
} else {
  warning("annex-pdc report: annex_pdc_bymodel lacks the join keys or the interval columns; ",
          "the pairwise table keeps C_A and C_B without their own bounds.", call. = FALSE)
}
ANNEX_PDC_REPORT_COLS <- intersect(.apdc_wanted, names(annex_pdc_table))
.apdc_show <- function(d, drop = character(0)) d[, setdiff(ANNEX_PDC_REPORT_COLS, drop), drop = FALSE]
cat(sprintf("Reporting columns in use: %s\n", paste(ANNEX_PDC_REPORT_COLS, collapse = ", ")))
if (exists("annex_pdc_reconciliation", inherits = TRUE) && !is.null(annex_pdc_reconciliation)) {
  cat("\nReconciliation against the live delta_C_analytic_table:\n")
  print(annex_pdc_reconciliation, row.names = FALSE, digits = 3)
}
cat("\n-- Per-arm horizon-specific Uno's C on predicted risk, out-of-fold development sample --\n")
print(annex_pdc_bymodel[, intersect(c("outcome", "model", "family", "horizon", "n",
                                      "events_by_horizon", "C", "C_boot_lower", "C_boot_upper"),
                                    names(annex_pdc_bymodel)), drop = FALSE],
      row.names = FALSE, digits = 4)
.apdc_cross <- annex_pdc_table[annex_pdc_table$comparison == "Cox vs DeepSurv-informed", , drop = FALSE]
cat("\n-- NEW ROWS: Cox versus DeepSurv-informed candidates (bootstrap interval) --\n")
if (nrow(.apdc_cross)) {
  print(.apdc_show(.apdc_cross, c("comparison", "family_A", "family_B")), row.names = FALSE, digits = 4)
  cat(sprintf("\nLargest |delta C| in the Cox versus DeepSurv-informed block: %.5f (promotion threshold %.2f).\n",
              max(abs(.apdc_cross$delta_C), na.rm = TRUE), ANNEX_PDC_PROMOTION_DELTA))
  if ("meets_promotion_threshold" %in% names(.apdc_cross))
    cat(if (any(.apdc_cross$meets_promotion_threshold, na.rm = TRUE))
          "At least one DeepSurv-informed candidate differs from a Cox candidate by enough to matter under the project's promotion rule.\n"
        else
          "No DeepSurv-informed candidate differs from a Cox candidate by enough to matter under the project's promotion rule, so on discrimination the two families are interchangeable and the choice rests on calibration, parsimony and interpretability.\n")
} else {
  cat("None: the build cell ran without the DeepSurv-informed arms.\n")
}
if (exists("annex_pdc_skipped", inherits = TRUE) && !is.null(annex_pdc_skipped)) {
  cat("\n-- Pairs excluded by the identical-formula rule (delta C is exactly 0 on 1 - S(t)) --\n")
  print(annex_pdc_skipped, row.names = FALSE)
}
cat("\n-- Full extended table --\n")
print(.apdc_show(annex_pdc_table, c("family_A", "family_B", "C_A_ij_lower", "C_A_ij_upper",
                                    "C_B_ij_lower", "C_B_ij_upper", "boot_mean", "centering_bias")),
      row.names = FALSE, digits = 4)
# ---- The save the interrupted cell never reached ----------------------------
# CSV under cons/_out (text, safe on Drive); the R objects under data/, never
# under cons/, per the project rule on .rds placement.
.apdc_root2 <- if (exists(".apdc_root", inherits = TRUE) && is.character(.apdc_root)) .apdc_root else
  { p <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    sub("(/|\\\\)?cons(/|\\\\)?$", "", p) }
stopifnot("cannot locate the project root to save into; set project_root and rerun this cell" =
  dir.exists(file.path(.apdc_root2, "data", "20241015_out")))
.apdc_outdir <- file.path(.apdc_root2, "cons", "_out")
dir.create(.apdc_outdir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(annex_pdc_table[, ANNEX_PDC_REPORT_COLS, drop = FALSE],
                 file.path(.apdc_outdir, "pred225_annex_paired_delta_c_risk_ext.csv"), row.names = FALSE)
utils::write.csv(annex_pdc_bymodel,
                 file.path(.apdc_outdir, "pred225_annex_paired_delta_c_risk_ext_bymodel.csv"), row.names = FALSE)
if (exists("annex_pdc_skipped", inherits = TRUE) && !is.null(annex_pdc_skipped))
  utils::write.csv(annex_pdc_skipped,
                   file.path(.apdc_outdir, "pred225_annex_paired_delta_c_risk_ext_skipped.csv"), row.names = FALSE)
saveRDS(list(table = annex_pdc_table, bymodel = annex_pdc_bymodel,
             skipped = if (exists("annex_pdc_skipped", inherits = TRUE)) annex_pdc_skipped else NULL,
             bootstrap_replicates = if (exists("annex_pdc_boot", inherits = TRUE)) annex_pdc_boot else NULL,
             reconciliation = if (exists("annex_pdc_reconciliation", inherits = TRUE)) annex_pdc_reconciliation else NULL,
             family = if (exists("annex_pdc", inherits = TRUE)) annex_pdc$family else NULL,
             provenance = if (exists("annex_pdc", inherits = TRUE)) annex_pdc$provenance else NA_character_,
             B = if (exists("ANNEX_PDC_B", inherits = TRUE)) ANNEX_PDC_B else NA_integer_,
             seed = if (exists("ANNEX_PDC_SEED", inherits = TRUE)) ANNEX_PDC_SEED else NA_integer_,
             interval = "percentile bootstrap, primary; infinitesimal jackknife, secondary",
             recovered_by = "annex-paired-delta-c-risk-ext-report",
             created = as.character(Sys.time())),
        file.path(.apdc_root2, "data", "20241015_out", "pred225_annex_paired_delta_c_risk_ext.rds"))
cat("\nSaved: cons/_out/pred225_annex_paired_delta_c_risk_ext*.csv and\n")
cat("data/20241015_out/pred225_annex_paired_delta_c_risk_ext.rds\n")
