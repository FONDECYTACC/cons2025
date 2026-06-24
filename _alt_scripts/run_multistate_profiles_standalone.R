# =====================================================================================
# run_multistate_profiles_standalone.R
# Standalone runner for the multistate state-occupancy figure (multistate_profiles_probtrans.R)
# WITHOUT the live notebook: it loads the exported probtrans engine + reference profile from
# data/20241015_out, builds the Spanish 2x2 profile set, renders, and saves the figure.
#   Rscript cons/_alt_scripts/run_multistate_profiles_standalone.R
# =====================================================================================
suppressPackageStartupMessages({
  library(survival); library(mstate); library(ggplot2); library(scales)
})

project_root <- getwd()
data_out <- file.path(project_root, "data", "20241015_out")
figs_out <- file.path(project_root, "cons", "_figs")
if (!dir.exists(figs_out)) dir.create(figs_out, recursive = TRUE)

source(file.path(project_root, "cons", "_alt_scripts", "multistate_profiles_probtrans.R"))

# --- load forward transition models (imputation 1) + reference profile + tmat ---------
engine <- readRDS(file.path(data_out, "pooled_probtrans_engine_INTERNAL.rds"))
fm     <- engine$ms_fits[[1]]                       # m1 (1->2), m2 (1->3), m3 (2->3 forward)
tmat   <- engine$tmat
base   <- readRDS(file.path(data_out, "msm_artifact_2026_06_20.rds"))$newdata_template
models <- list(fm$m1, fm$m2, fm$m3)

TNR <- "Times New Roman"

# ---- Spanish (profs_es) --------------------------------------------------------------
profs_es <- ms_profiles_substance_age(base, lang = "es")           # 2x2 sustancia x edad (Q1/Q3 27/43)
g_es <- plot_multistate_profiles_probtrans(
  profs_es, models = models, trans = tmat, lang = "es",
  tnr = TNR, emit_message = TRUE)

png_es <- file.path(figs_out, "multistate_profiles_probtrans_es.png")
pdf_es <- file.path(figs_out, "multistate_profiles_probtrans_es.pdf")
ggsave(png_es, g_es, width = 21, height = 16, units = "cm", dpi = 600)
ggsave(pdf_es, g_es, width = 21, height = 16, units = "cm", device = grDevices::cairo_pdf)

cat("\nSaved:\n  ", normalizePath(png_es, winslash = "/"), "\n  ",
    normalizePath(pdf_es, winslash = "/"), "\n", sep = "")
cat("\n--- caption (ES) ---\n", attr(g_es, "caption"), "\n", sep = "")
