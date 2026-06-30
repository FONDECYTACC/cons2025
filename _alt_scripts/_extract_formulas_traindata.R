# Final load of pred22 Rdata: extract formulas + authoritative train data +
# split2125 + column diagnostics, then save small reusable artifacts.
# Read-only with respect to the notebooks. Run once.
t0 <- Sys.time()
# Source workspace: point at the CURRENT pred22 workspace. Updated 2026-06-29 from
# the 2026-05-28 file (which predated the fixed-rule selection of 2026-06-08 and has
# since been replaced on disk), so the extracted formulas match the selected models.
rd <- "data/20241015_out/pred22_ndp_2026_06_18.Rdata"
cat("Loading", rd, "...\n"); flush.console()
e <- new.env(); load(rd, envir = e)
cat(sprintf("Loaded in %.1f min.\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

outdir <- "data/20241015_out/_val_inputs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

fns <- c("formula_shap_readmit_clean_updated", "formula_death_updated2",
         "formula_shap_death", "formula_readmit_updated2")

formulas <- list()
txt_lines <- character(0)
for (fn in fns) {
  f <- get(fn, envir = e)
  # Strip the heavy global environment so the saved object stays tiny.
  environment(f) <- globalenv()
  formulas[[fn]] <- f
  ftxt <- paste(deparse(f, width.cutoff = 500L), collapse = " ")
  txt_lines <- c(txt_lines, paste0("### ", fn), ftxt, "")
  cat("\n###", fn, "\n  vars:", paste(all.vars(f), collapse = ", "), "\n")
}
writeLines(txt_lines, file.path(outdir, "formulas.txt"))
saveRDS(formulas, file.path(outdir, "formulas.rds"))

# Column diagnostics
pcd  <- get("py_corrected_datasets", envir = e)
pcdb <- get("py_corrected_datasets_boot", envir = e)
cols75 <- names(pcd[[1]]); cols86 <- names(pcdb[[1]])
writeLines(cols75, file.path(outdir, "cols_py_corrected_datasets_75.txt"))
writeLines(cols86, file.path(outdir, "cols_py_corrected_datasets_boot_86.txt"))
cat("\n=== cols only in _boot (86) not in standard (75) ===\n"); print(setdiff(cols86, cols75))
cat("\n=== cols only in standard (75) not in _boot (86) ===\n"); print(setdiff(cols75, cols86))

allvars <- unique(unlist(lapply(formulas, all.vars)))
cat("\n=== formula vars NOT in standard 75-col set ===\n"); print(setdiff(allvars, cols75))
cat("\n=== formula vars NOT in boot 86-col set    ===\n"); print(setdiff(allvars, cols86))

# Save authoritative artifacts
saveRDS(pcdb, file.path(outdir, "py_corrected_datasets_boot.rds"))
saveRDS(pcd,  file.path(outdir, "py_corrected_datasets.rds"))
saveRDS(get("split2125", envir = e), file.path(outdir, "split2125.rds"))

# Cross-check train boot
b1 <- as.data.frame(pcdb[[1]])
cat("\nmean adm_age_rec3 (train boot):", mean(b1$adm_age_rec3, na.rm = TRUE), "\n")
cat("sum national_foreign==1:", sum(b1$national_foreign == 1, na.rm = TRUE), "\n")
cat("nrow train:", nrow(b1), "\n")
keyc <- intersect(c("readmit_event","death_event","readmit_time_from_disch_m",
  "death_time_from_disch_m","plan_type_strata","center_id","row_id"), names(b1))
cat("key cols present in boot:", paste(keyc, collapse = ", "), "\n")
cat("\nDONE. Artifacts in", outdir, "\n")
