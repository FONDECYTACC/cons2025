# Read-only extraction from the big pred22 Rdata: pull split2125 + formulas +
# verify py_corrected_datasets row counts, save small files for reuse.
suppressWarnings(suppressMessages({
  t0 <- Sys.time()
}))
rd <- "data/20241015_out/pred22_ndp_2026_05_28.Rdata"
cat("Loading", rd, "...\n"); flush.console()
e <- new.env()
load(rd, envir = e)
cat(sprintf("Loaded in %.1f min. %d objects.\n",
            as.numeric(difftime(Sys.time(), t0, units="mins")), length(ls(e))))

nms <- ls(e)
# print all object names
cat("\n=== OBJECT NAMES ===\n")
print(nms)

# look for split-like objects
split_like <- grep("split|is_train|partition|train_idx|test_idx", nms, value=TRUE, ignore.case=TRUE)
cat("\n=== SPLIT-LIKE OBJECTS ===\n"); print(split_like)

formula_names <- c("formula_shap_readmit_clean_updated","formula_death_updated2","formula_shap_death",
                   "formula_readmit_updated2")
cat("\n=== FORMULAS PRESENT ===\n")
for (fn in formula_names) cat(sprintf("  %s: %s\n", fn, exists(fn, envir=e)))

# py_corrected_datasets info
for (objn in c("py_corrected_datasets","py_corrected_datasets_boot")) {
  if (exists(objn, envir=e)) {
    o <- get(objn, envir=e)
    cat(sprintf("\n%s: list len=%d, nrow[[1]]=%d, ncol[[1]]=%d\n",
                objn, length(o), nrow(o[[1]]), ncol(o[[1]])))
  } else cat(sprintf("\n%s: NOT PRESENT\n", objn))
}

# Inspect each split-like object
for (s in split_like) {
  o <- get(s, envir=e)
  cat(sprintf("\n--- %s --- class=%s\n", s, paste(class(o), collapse=",")))
  if (is.data.frame(o)) {
    cat("dim:", paste(dim(o), collapse=" x "), "\n")
    cat("names:", paste(utils::head(names(o),30), collapse=", "), "\n")
    if ("is_train" %in% names(o)) print(table(o$is_train, useNA="ifany"))
  } else {
    cat("length:", length(o), " ", paste(utils::head(as.character(o),5),collapse=","), "\n")
  }
}
