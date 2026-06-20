# Convert corrected_datasets_nondum_filt.rds (list of 5 imputations, 88152 x 43,
# non-dummy) into 5 individual parquet files for reproducibility.
# Idempotent: skips files that already exist (unless FORCE = TRUE).
#
# Output: data/20241015_out/corrected_datasets_nondum_filt_imp{1..5}.parquet
#         data/20241015_out/corrected_datasets_nondum_filt_manifest.rds
#
# Reader of choice: nanoparquet (arrow not installed in this renv).
suppressWarnings(stopifnot(requireNamespace("nanoparquet", quietly = TRUE)))

# --- Resolve the project root robustly (works from the project root or from cons/,
#     reusing a `project_root` set earlier, e.g. in the notebook). ---
if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}

FORCE <- FALSE
src   <- file.path(project_root, "data/20241015_out/corrected_datasets_nondum_filt.rds")
dir   <- file.path(project_root, "data/20241015_out")

x <- readRDS(src)
stopifnot(is.list(x), length(x) >= 1L)
cat(sprintf("Loaded %s: %d imputations, [[1]] = %d x %d\n",
            basename(src), length(x), nrow(x[[1]]), ncol(x[[1]])))

paths <- character(length(x))
for (i in seq_along(x)) {
  p <- file.path(dir, sprintf("corrected_datasets_nondum_filt_imp%d.parquet", i))
  paths[i] <- p
  if (file.exists(p) && !FORCE) { cat("  exists, skip:", basename(p), "\n"); next }
  di <- as.data.frame(x[[i]])               # drop tidytable/data.table class
  # nanoparquet writes factors as their character labels; coerce explicitly so
  # the round-trip is predictable (preprocessing re-factors anyway).
  is_fac <- vapply(di, is.factor, logical(1))
  di[is_fac] <- lapply(di[is_fac], as.character)
  nanoparquet::write_parquet(di, p)
  cat(sprintf("  wrote %s (%d x %d)\n", basename(p), nrow(di), ncol(di)))
}

# Round-trip check on imputation 1
chk <- as.data.frame(nanoparquet::read_parquet(paths[1]))
ref <- as.data.frame(x[[1]])
stopifnot(nrow(chk) == nrow(ref), ncol(chk) == ncol(ref),
          identical(names(chk), names(ref)))
num_ref <- vapply(ref, is.numeric, logical(1))
max_abs <- max(abs(unlist(Map(function(a, b) if (is.numeric(b)) a - b else 0,
                              chk[num_ref], ref[num_ref]))), na.rm = TRUE)
cat(sprintf("Round-trip numeric max |diff| (imp1): %.3g\n", max_abs))

manifest <- list(
  source = src, n_imp = length(x), nrow = nrow(x[[1]]), ncol = ncol(x[[1]]),
  paths = paths, colnames = names(ref), created = as.character(Sys.time())
)
saveRDS(manifest, file.path(dir, "corrected_datasets_nondum_filt_manifest.rds"))
cat("Manifest saved. DONE.\n")
