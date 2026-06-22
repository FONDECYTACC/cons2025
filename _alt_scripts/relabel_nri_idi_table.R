# =====================================================================================
# relabel_nri_idi_table.R
# Cosmetic relabeling for the held-out NRI/IDI table (prediction23). It does NOT touch
# the computation in nri_idi_from_results_boot.R nor any value: it only makes the
# "Comparison" label and the kable caption read in REFERENCE -> UPDATED order, so they
# match the existing "Discrimination slope, reference vs updated" column (which is
# old=reference vs new=updated).
#
# Convention kept (unchanged): old = SHAP implemented (best_perf2) is the REFERENCE,
# new = Full PH primary (best_perf1) is the UPDATED model, and positive NRI/IDI favor
# the UPDATED (Full PH) model.
#
# Usage (drop-in for the two notebook cells; nothing upstream changes):
#   source(file.path(project_root, "cons/_alt_scripts/relabel_nri_idi_table.R"))
#   fixed <- relabel_nri_idi_table(
#     nri_idi_tab$table,
#     old_label = "SHAP implemented (best_perf2)",   # reference
#     new_label = "Full PH primary (best_perf1)")    # updated
#   nri_idi_model_table <- fixed$model_table         # for the CSV / stored object
#   fixed$table |> knitr::kable("markdown", caption = fixed$caption)
# =====================================================================================

relabel_nri_idi_table <- function(tab,
                                  old_label            = "SHAP implemented (best_perf2)",
                                  new_label            = "Full PH primary (best_perf1)",
                                  name_models_in_slope = FALSE) {
  stopifnot(is.data.frame(tab))

  # reference -> updated order, matching the slope column
  comparison <- paste0(old_label, " [reference] vs ", new_label, " [updated]")

  out <- tab

  # Optional: name the models inside the slope column header so the pairing is explicit
  # (off by default to keep the table structure untouched).
  if (isTRUE(name_models_in_slope)) {
    slope_col <- grep("^Discrimination slope", names(out))
    if (length(slope_col) == 1L) {
      names(out)[slope_col] <- sprintf(
        "Discrimination slope, reference (%s) vs updated (%s)", old_label, new_label)
    }
  }

  model_table <- cbind(Comparison = comparison, out, stringsAsFactors = FALSE)

  caption <- paste0(
    "Held-out 20%: NRI/IDI (", comparison,
    "; readmission shared, so all readmission values are 0). ",
    "Positive values favor the updated model (", new_label, ").")

  list(table = out, model_table = model_table, comparison = comparison, caption = caption)
}
