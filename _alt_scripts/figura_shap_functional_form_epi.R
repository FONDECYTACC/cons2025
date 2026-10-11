#!/usr/bin/env Rscript --vanilla
# =============================================================================
# Article 2 -- final functional-form figure for the journal Epidemiology (LWW).
# 30 cells (15 predictors x 2 outcomes) on an 8-row x 4-column grid, entirely
# achromatic, Times New Roman, 17.8 cm wide and at most 24.0 cm tall.
# No text is burned into the raster. Two .txt files are written beside the image:
# a SHORT journal legend for typesetting under the artwork, and a LONG eAppendix
# methods note that carries every detail the legend cannot hold. LWW typesets
# figure legends from manuscript text, so a baked caption both fails production
# and wastes vertical space; an in-image key does the same for less reason.
# Run with: "C:/Program Files/R/R-4.4.1/bin/Rscript.exe" --vanilla <this file>
# Self-contained: globs for its own input, defines every label itself, and
# depends on nothing pre-existing in the workspace.
# NOTE on ggh4x: under ggplot2 4.0 the theme elements are S7 objects, and
# ggh4x 0.3.0's per-layer strip theming (elem_list_text / elem_list_rect) dies
# with "cannot coerce type 'object' to vector of type 'list'" at draw time. The
# nested facet itself is fine, so the outer and inner strips are differentiated
# by editing the finished gtable instead of by theming the strip.
# =============================================================================
options(stringsAsFactors = FALSE, warn = 1, scipen = 999)
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggh4x)
  library(scales)
  library(grid)
  library(gtable)
  library(ragg)
  library(systemfonts)
  library(png)
})
set.seed(20260820L)
t_start <- Sys.time()
# -----------------------------------------------------------------------------
# 0. Output locations. Everything the script writes lives beside THIS FILE, which
#    is located from the command line rather than hardcoded: a hardcoded absolute
#    path makes the script ignore its own working directory, so a copy run from
#    another folder silently overwrites the originals. The directory step is
#    fatal on failure, because dir.create() only warns on an unreachable drive
#    and the render would then waste a full minute before dying at readPNG.
# -----------------------------------------------------------------------------
script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f[1], winslash = "/", mustWork = FALSE))
  else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
OUT_DIR <- script_dir()
if (!dir.exists(OUT_DIR) && !dir.create(OUT_DIR, recursive = TRUE)) {
  stop(sprintf("Output directory does not exist and cannot be created: %s", OUT_DIR))
}
if (file.access(OUT_DIR, mode = 2L) != 0L) {
  stop(sprintf("Output directory is not writable: %s", OUT_DIR))
}
message(sprintf("[out] writing to %s", OUT_DIR))
PNG_PATH <- file.path(OUT_DIR, "figura_shap_functional_form_epi.png")
PDF_PATH <- file.path(OUT_DIR, "figura_shap_functional_form_epi.pdf")
# Two text deliverables rather than one. The single 41-line caption written by
# earlier versions was far too long for a journal legend and repeated its own
# warnings three times over, so it is split: LEGEND_PATH is the short block that
# gets typeset under the figure, EAPP_PATH is the supplementary methods note that
# carries everything else. The stale single-file caption is deleted on every run
# so a reader can never pick up a superseded version sitting beside the new ones.
LEGEND_PATH <- file.path(OUT_DIR, "figura_shap_functional_form_epi_legend.txt")
EAPP_PATH <- file.path(OUT_DIR, "figura_shap_functional_form_epi_eappendix.txt")
OLD_CAP_PATH <- file.path(OUT_DIR, "figura_shap_functional_form_epi_caption.txt")
if (file.exists(OLD_CAP_PATH)) {
  unlink(OLD_CAP_PATH)
  message(sprintf("[out] removed superseded single-file caption: %s", basename(OLD_CAP_PATH)))
}
RASTER_TMP <- file.path(OUT_DIR, ".points_layer_raster.png")
# -----------------------------------------------------------------------------
# 1. Locate the input. Search plausible roots, prefer parquet over csv, then the
#    newest embedded timestamp (YYYYMMDD_HHMM), then file mtime.
# -----------------------------------------------------------------------------
FILE_PAT <- "^XGB12_corr_Functional_Forms_Dual_Aggregated_.*[.](parquet|csv)$"
candidate_roots <- function() {
  roots <- c(
    "g:/My Drive/Alvacast/SISTRAT 2023/cons/_out",
    "G:/My Drive/Alvacast/SISTRAT 2023/cons/_out",
    "g:/My Drive/Alvacast/SISTRAT 2023/_out",
    "g:/My Drive/Alvacast/SISTRAT 2023/data",
    OUT_DIR
  )
  here <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  base <- here
  for (up in 0:5) {
    if (is.na(base) || base == "" || base == dirname(base)) break
    roots <- c(roots, base, file.path(base, "_out"), file.path(base, "cons", "_out"),
               file.path(base, "cons"), file.path(base, "data"))
    base <- dirname(base)
  }
  unique(roots[dir.exists(roots)])
}
find_input <- function() {
  roots <- candidate_roots()
  if (!length(roots)) stop("No plausible input directory exists on this machine.")
  hits <- unlist(lapply(roots, function(r) list.files(r, pattern = FILE_PAT, full.names = TRUE)), use.names = FALSE)
  hits <- unique(normalizePath(hits, winslash = "/", mustWork = FALSE))
  if (!length(hits)) stop("Could not find any XGB12_corr_Functional_Forms_Dual_Aggregated_* input file.")
  ext <- tolower(tools::file_ext(hits))
  stamp <- sub("^.*Aggregated_([0-9]{8}_[0-9]{4}).*$", "\\1", basename(hits))
  stamp[!grepl("^[0-9]{8}_[0-9]{4}$", stamp)] <- "00000000_0000"
  mt <- as.numeric(file.info(hits)$mtime)
  ordkey <- order(match(ext, c("parquet", "csv")), -as.numeric(gsub("_", "", stamp)), -mt)
  hits[ordkey][1]
}
INPUT <- find_input()
message(sprintf("[input] %s (%.1f MB)", INPUT, file.info(INPUT)$size / 1024^2))
read_input <- function(path) {
  if (tolower(tools::file_ext(path)) == "parquet") {
    as.data.frame(arrow::read_parquet(path))
  } else {
    as.data.frame(utils::read.csv(path, stringsAsFactors = FALSE))
  }
}
dat <- read_input(INPUT)
req_cols <- c("Feature_Value", "SHAP_Impact", "Predictor", "Outcome", "Scope")
if (!all(req_cols %in% names(dat))) stop("Input is missing required columns.")
dat <- dat[, req_cols]
dat$Feature_Value <- as.numeric(dat$Feature_Value)
dat$SHAP_Impact <- as.numeric(dat$SHAP_Impact)
dat <- dat[is.finite(dat$Feature_Value) & is.finite(dat$SHAP_Impact), , drop = FALSE]
SCOPE_TXT <- paste(unique(dat$Scope), collapse = ", ")
# -----------------------------------------------------------------------------
# 2. Predictor dictionary, defined here so the script needs no workspace.
#    Level direction is taken from the notebook ordered_mappings block and is
#    spelled out on the tick labels, so a reader never decodes bare integers.
# -----------------------------------------------------------------------------
CONT_VARS <- c("adm_age_rec3", "dit_m", "porc_pobr")
ORD_VARS <- c("ed_attainment_corr", "eva_consumo", "eva_fam", "eva_fisica",
              "eva_ocupacion", "eva_relinterp", "eva_sm", "eva_transgnorma",
              "evaluacindelprocesoteraputico", "prim_sub_freq_rec",
              "tenure_status_household", "urbanicity_cat")
# The nine discharge-evaluation headings carry the word "attainment", because the
# tick words High / Interm. / Minim. name GOAL ATTAINMENT, that is High is the
# FAVOURABLE end. Under a bare heading such as "Discharge evaluation, physical
# health" a reader naturally parses High as high severity and reads the panel
# backwards, which reverses the direction of every one of those nine predictors.
# tenure_status_household is NOT called "Housing precarity": its level 2 is a
# residual "other" category, so the family is a nominal tenure classification and
# a precarity title would assert an ordering the coding does not support.
PRED_LAB <- c(
  adm_age_rec3 = "Age at admission, yr",
  dit_m = "Treatment episode length, mo",
  porc_pobr = "Communal poverty rate",
  ed_attainment_corr = "Educational attainment",
  eva_consumo = "Discharge evaluation, substance use (attainment)",
  eva_fam = "Discharge evaluation, family (attainment)",
  eva_fisica = "Discharge evaluation, physical health (attainment)",
  eva_ocupacion = "Discharge evaluation, occupation (attainment)",
  eva_relinterp = "Discharge evaluation, interpersonal (attainment)",
  eva_sm = "Discharge evaluation, mental health (attainment)",
  eva_transgnorma = "Discharge evaluation, rule adherence (attainment)",
  evaluacindelprocesoteraputico = "Discharge evaluation, therapeutic process (attainment)",
  prim_sub_freq_rec = "Use frequency, primary substance",
  tenure_status_household = "Household tenure status",
  urbanicity_cat = "Rural residence"
)
# Families whose levels are genuinely ordered on a single underlying scale. The
# connecting line between level medians is drawn ONLY for these: joining the
# medians of a nominal family asserts a dose-response the coding cannot support.
ORDERED_FAMILIES <- c("ed_attainment_corr", "eva_consumo", "eva_fam", "eva_fisica",
                      "eva_ocupacion", "eva_relinterp", "eva_sm", "eva_transgnorma",
                      "evaluacindelprocesoteraputico", "prim_sub_freq_rec", "urbanicity_cat")
NOMINAL_FAMILIES <- setdiff(ORD_VARS, ORDERED_FAMILIES)
EVA_TICKS <- c("High", "Interm.", "Minim.")
ORD_TICKS <- list(
  ed_attainment_corr = c(">HS", "HS", "\u2264Prim."),
  eva_consumo = EVA_TICKS,
  eva_fam = EVA_TICKS,
  eva_fisica = EVA_TICKS,
  eva_ocupacion = EVA_TICKS,
  eva_relinterp = EVA_TICKS,
  eva_sm = EVA_TICKS,
  eva_transgnorma = EVA_TICKS,
  evaluacindelprocesoteraputico = EVA_TICKS,
  prim_sub_freq_rec = c("\u22641/wk", "2-6/wk", "Daily"),
  tenure_status_household = c("Own", "Rent", "Other", "Relat.", "Illeg."),
  urbanicity_cat = c("Urban", "Mixed", "Rural")
)
ORD_GLOSS <- list(
  ed_attainment_corr = c("more than high school", "high school", "primary or less"),
  eva_consumo = c("high goal attainment", "intermediate", "minimum"),
  eva_fam = c("high goal attainment", "intermediate", "minimum"),
  eva_fisica = c("high goal attainment", "intermediate", "minimum"),
  eva_ocupacion = c("high goal attainment", "intermediate", "minimum"),
  eva_relinterp = c("high goal attainment", "intermediate", "minimum"),
  eva_sm = c("high goal attainment", "intermediate", "minimum"),
  eva_transgnorma = c("high goal attainment", "intermediate", "minimum"),
  evaluacindelprocesoteraputico = c("high goal attainment", "intermediate", "minimum"),
  prim_sub_freq_rec = c("at most 1 day per week", "2 to 6 days per week", "daily"),
  tenure_status_household = c("owner", "renting", "other, a residual category outside any ordering", "staying with a relative", "illegal settlement"),
  urbanicity_cat = c("urban", "mixed", "rural")
)
OUT_DISPLAY <- c(Readmission = "Readmission", Death = "Mortality")
ALL_VARS <- c(CONT_VARS, ORD_VARS)
if (!setequal(unique(dat$Predictor), ALL_VARS)) {
  stop(sprintf("Predictor set mismatch. Data only: %s ; dictionary only: %s",
               paste(setdiff(unique(dat$Predictor), ALL_VARS), collapse = ", "),
               paste(setdiff(ALL_VARS, unique(dat$Predictor)), collapse = ", ")))
}
if (!setequal(unique(dat$Outcome), names(OUT_DISPLAY))) stop("Outcome set mismatch.")
# -----------------------------------------------------------------------------
# 3. Runtime assertion: Feature_Value must be identical across the two outcomes.
#    Verified rather than assumed, because every x position depends on it.
# -----------------------------------------------------------------------------
fv_check <- lapply(ALL_VARS, function(v) {
  a <- dat$Feature_Value[dat$Predictor == v & dat$Outcome == "Readmission"]
  b <- dat$Feature_Value[dat$Predictor == v & dat$Outcome == "Death"]
  if (length(a) != length(b)) stop(sprintf("Row-count mismatch across outcomes for %s.", v))
  data.frame(Predictor = v, n = length(a), max_abs_diff = max(abs(a - b)))
})
fv_check <- do.call(rbind, fv_check)
FV_MAX_DIFF <- max(fv_check$max_abs_diff)
if (FV_MAX_DIFF > 1e-12) {
  stop(sprintf("Feature_Value differs across outcomes (max abs diff = %.3g). Layout assumption broken.", FV_MAX_DIFF))
}
N_PER_CELL <- unique(fv_check$n)
if (length(N_PER_CELL) != 1L) N_PER_CELL <- max(fv_check$n)
# -----------------------------------------------------------------------------
# 4. Ordinal handling. Non-integer Feature_Value on ordinals are cross-validation
#    fold-averaging artifacts, not real categories, so they are rounded to the
#    nearest integer level. The exact-integer share is COMPUTED, never hardcoded.
# -----------------------------------------------------------------------------
ord_raw <- dat[dat$Predictor %in% ORD_VARS, , drop = FALSE]
ord_raw$lv <- round(ord_raw$Feature_Value)
EXACT_INT_SHARE <- 100 * mean(abs(ord_raw$Feature_Value - ord_raw$lv) < 1e-9)
exact_by_pred <- ord_raw %>%
  group_by(Predictor) %>%
  summarise(exact_pct = 100 * mean(abs(Feature_Value - lv) < 1e-9), .groups = "drop") %>%
  arrange(exact_pct)
level_share <- ord_raw %>%
  filter(Outcome == "Readmission") %>%
  count(Predictor, lv, name = "n_lv") %>%
  group_by(Predictor) %>%
  mutate(share_pct = 100 * n_lv / sum(n_lv)) %>%
  ungroup()
# Per-level clipping for DISPLAY of the cloud only. Clipping at the same
# percentiles WITHIN each level cannot be biased against rare levels, because
# every level loses the same tail fraction by construction. Counts are reported.
CLIP_LO <- 0.01
CLIP_HI <- 0.99
clip_bounds <- ord_raw %>%
  group_by(Predictor, Outcome, lv) %>%
  summarise(lo = quantile(SHAP_Impact, CLIP_LO, names = FALSE),
            hi = quantile(SHAP_Impact, CLIP_HI, names = FALSE), .groups = "drop")
ord_clip <- ord_raw %>%
  inner_join(clip_bounds, by = c("Predictor", "Outcome", "lv")) %>%
  mutate(keep = (lo >= hi) | (SHAP_Impact >= lo & SHAP_Impact <= hi))
clip_report <- ord_clip %>%
  group_by(Predictor, Outcome, lv) %>%
  summarise(n_total = n(), n_kept = sum(keep), .groups = "drop") %>%
  mutate(n_dropped = n_total - n_kept, pct_dropped = 100 * n_dropped / n_total)
ord_clip <- ord_clip[ord_clip$keep, c("Predictor", "Outcome", "lv", "SHAP_Impact")]
# Clipping costs about 2 percent of POINTS but far more than 2 percent of visible
# RANGE, because the discarded points are by construction the extremes and the
# cell's vertical limits are then set from the clipped extremes. Reporting only
# the point loss invites the reader to treat the picture as essentially complete,
# so the surviving fraction of the true SHAP span is computed per ordinal cell.
span_full <- ord_raw %>%
  group_by(Predictor, Outcome) %>%
  summarise(full_span = max(SHAP_Impact) - min(SHAP_Impact), .groups = "drop")
span_shown <- ord_clip %>%
  group_by(Predictor, Outcome) %>%
  summarise(shown_span = max(SHAP_Impact) - min(SHAP_Impact), .groups = "drop")
span_keep <- span_full %>%
  inner_join(span_shown, by = c("Predictor", "Outcome")) %>%
  mutate(pct_span_shown = 100 * shown_span / full_span)
SPAN_SHOWN_MIN <- min(span_keep$pct_span_shown)
SPAN_SHOWN_MAX <- max(span_keep$pct_span_shown)
# Median and IQR come from the FULL, UNCLIPPED data. Quartiles sit strictly
# inside a 1st-to-99th percentile window, so clipping cannot move them.
ord_summ <- ord_raw %>%
  group_by(Predictor, Outcome, lv) %>%
  summarise(med = median(SHAP_Impact),
            q25 = quantile(SHAP_Impact, 0.25, names = FALSE),
            q75 = quantile(SHAP_Impact, 0.75, names = FALSE), .groups = "drop")
# How much wider is the interquartile bar than an actual interval about the
# median? The comparison uses the DISTRIBUTION-FREE order-statistic interval,
# which is exact and needs no resampling: with n observations the 95 percent
# interval for the median runs between order statistics n/2 -/+ 1.96*sqrt(n)/2.
# The ratio is computed rather than asserted, because the caption states it.
median_ci_width <- function(v) {
  n <- length(v)
  if (n < 30L) return(NA_real_)
  k <- floor(n / 2 - 1.959964 * sqrt(n) / 2)
  if (k < 1L) k <- 1L
  s <- sort(v)
  s[n - k + 1L] - s[k]
}
iqr_vs_ci <- ord_raw %>%
  group_by(Predictor, Outcome, lv) %>%
  summarise(iqr_w = quantile(SHAP_Impact, 0.75, names = FALSE) - quantile(SHAP_Impact, 0.25, names = FALSE),
            ci_w = median_ci_width(SHAP_Impact), .groups = "drop") %>%
  filter(is.finite(ci_w) & ci_w > 0) %>%
  mutate(ratio = iqr_w / ci_w)
IQR_CI_MIN <- min(iqr_vs_ci$ratio)
IQR_CI_MED <- stats::median(iqr_vs_ci$ratio)
IQR_CI_MAX <- max(iqr_vs_ci$ratio)
# -----------------------------------------------------------------------------
# 5. Hand-rolled sina jitter, since ggforce is absent and must not be used.
#    EQUAL-N subsample, cap 1000 per level: proportional subsampling leaves the
#    rarest level so few points that rarity reads as noise instead of rarity.
#    ORDER MATTERS HERE. The violin subsample is drawn FIRST and the cloud is then
#    drawn from that same subsample, and the density that scales the jitter is
#    estimated on that same subsample too. Estimating the cloud density on the
#    full level data while handing geom_violin a capped subsample gives the two
#    layers DIFFERENT bandwidths, because bw.nrd0 scales as n^(-1/5); with the
#    cap at 20000 that happened in 46 of the 76 level-cells and the outline was
#    over-smoothed by up to 23 percent relative to the speckle it describes.
#    Sharing one sample and one bandwidth makes the claim that the cloud and the
#    outline describe one and the same shape literally true.
# -----------------------------------------------------------------------------
SINA_HALF_WIDTH <- 0.22
SINA_CAP <- 1000L
VIOLIN_ADJUST <- 1.15
VIOLIN_CAP <- 20000L
BW_TOL <- 1e-12
ord_key <- unique(ord_clip[, c("Predictor", "Outcome", "lv")])
ord_key <- ord_key[order(ord_key$Predictor, ord_key$Outcome, ord_key$lv), ]
rownames(ord_key) <- NULL
sina_list <- vector("list", nrow(ord_key))
viol_list <- vector("list", nrow(ord_key))
bw_list <- vector("list", nrow(ord_key))
for (i in seq_len(nrow(ord_key))) {
  kk <- ord_key[i, ]
  sel <- which(ord_clip$Predictor == kk$Predictor & ord_clip$Outcome == kk$Outcome & ord_clip$lv == kk$lv)
  n_lv_clip <- length(sel)
  if (n_lv_clip > VIOLIN_CAP) sel <- sort(sample(sel, VIOLIN_CAP))
  vsub <- ord_clip[sel, , drop = FALSE]
  viol_list[[i]] <- vsub
  v <- vsub$SHAP_Impact
  nv <- length(v)
  # The cloud is a subsample OF the violin subsample, so both layers see the same
  # empirical distribution and the same bandwidth by construction.
  cidx <- if (nv > SINA_CAP) sample.int(nv, SINA_CAP) else seq_len(nv)
  keepv <- v[cidx]
  dens_scaled <- rep(1, length(keepv))
  bw_used <- NA_real_
  if (nv >= 8L && stats::sd(v) > 0) {
    bw_used <- stats::bw.nrd0(v) * VIOLIN_ADJUST
    if (is.finite(bw_used) && bw_used > 0) {
      dd <- stats::density(v, bw = bw_used, n = 512L, from = min(v), to = max(v))
      dv <- stats::approx(dd$x, dd$y, xout = keepv, rule = 2)$y
      if (is.finite(max(dv, na.rm = TRUE)) && max(dv, na.rm = TRUE) > 0) {
        dens_scaled <- dv / max(dv, na.rm = TRUE)
      }
    }
  }
  s <- data.frame(SHAP_Impact = keepv,
                  xoff = stats::runif(length(keepv), -1, 1) * SINA_HALF_WIDTH * dens_scaled)
  s$Predictor <- kk$Predictor
  s$Outcome <- kk$Outcome
  s$lv <- kk$lv
  s$xpos <- kk$lv + s$xoff
  sina_list[[i]] <- s
  bw_list[[i]] <- data.frame(Predictor = kk$Predictor, Outcome = kk$Outcome, lv = kk$lv,
                             n_clip = n_lv_clip, n_violin = nv, n_cloud = length(keepv),
                             bw = bw_used)
}
ord_points <- do.call(rbind, sina_list)
ord_violin <- do.call(rbind, viol_list)
bw_report <- do.call(rbind, bw_list)
N_PLOTTED_POINTS <- nrow(ord_points)
N_LEVEL_CELLS <- nrow(ord_key)
# Runtime proof that the two layers now share one bandwidth: the cloud density is
# estimated on exactly the rows handed to geom_violin, so the ratio is one to
# machine precision for every level-cell. Asserted rather than asserted in prose.
BW_SHARED_OK <- all(bw_report$n_violin == vapply(seq_len(nrow(ord_key)), function(i)
  nrow(viol_list[[i]]), integer(1)))
if (!BW_SHARED_OK) stop("Cloud and violin no longer share their subsample; bandwidths would diverge.")
N_CELLS_CAPPED <- sum(bw_report$n_clip > VIOLIN_CAP)
# -----------------------------------------------------------------------------
# 6. Continuous handling. Equal-count quantile bins of x itself, target 25 bins
#    but never more bins than distinct values. Each bin, including the top bin,
#    is plotted at its OWN median x. x stays on NATURAL units: a rank or ECDF
#    axis would squeeze the upper tail of a skewed predictor into a sliver.
# -----------------------------------------------------------------------------
TARGET_BINS <- 25L
# One binning routine, used both for the display curve and for the bin-count
# sensitivity analysis, so the two can never drift apart.
bin_continuous <- function(s, nb_target) {
  nd <- length(unique(s$Feature_Value))
  nb <- min(nb_target, nd)
  br <- unique(stats::quantile(s$Feature_Value, probs = seq(0, 1, length.out = nb + 1L), names = FALSE, type = 7))
  s$bin <- cut(s$Feature_Value, breaks = br, include.lowest = TRUE, labels = FALSE)
  s %>%
    group_by(bin) %>%
    summarise(x = median(Feature_Value),
              x_lo = min(Feature_Value),
              x_hi = max(Feature_Value),
              med = median(SHAP_Impact),
              q25 = quantile(SHAP_Impact, 0.25, names = FALSE),
              q75 = quantile(SHAP_Impact, 0.75, names = FALSE),
              n_bin = n(),
              n_distinct_bin = length(unique(Feature_Value)), .groups = "drop")
}
cont_list <- list()
cont_meta <- list()
for (v in CONT_VARS) {
  for (oc in names(OUT_DISPLAY)) {
    s <- dat[dat$Predictor == v & dat$Outcome == oc, , drop = FALSE]
    nd <- length(unique(s$Feature_Value))
    z <- bin_continuous(s, TARGET_BINS)
    z$Predictor <- v
    z$Outcome <- oc
    cont_list[[length(cont_list) + 1L]] <- z
    # Realised bin counts are recorded because they are NOT equal: unique() on the
    # quantile breaks plus cut() cannot equalise counts on tied data, and the
    # communal poverty rate has only 863 distinct values. Any caption sentence
    # about bin size must quote these numbers, never the nominal n divided by k.
    cont_meta[[length(cont_meta) + 1L]] <- data.frame(
      Predictor = v, Outcome = oc, n_bins = nrow(z), n_distinct_x = nd,
      x_data_min = min(s$Feature_Value), x_data_max = max(s$Feature_Value),
      x_plot_min = min(z$x), x_plot_max = max(z$x),
      n_bin_min = min(z$n_bin), n_bin_max = max(z$n_bin), n_bin_sd = stats::sd(z$n_bin),
      distinct_per_bin_min = min(z$n_distinct_bin),
      distinct_per_bin_med = stats::median(z$n_distinct_bin),
      distinct_per_bin_max = max(z$n_distinct_bin),
      top_bin_lo = z$x_lo[nrow(z)], top_bin_hi = z$x_hi[nrow(z)],
      top_bin_n = z$n_bin[nrow(z)],
      bot_bin_lo = z$x_lo[1], bot_bin_hi = z$x_hi[1])
  }
}
cont_curve <- do.call(rbind, cont_list)
cont_meta <- do.call(rbind, cont_meta)
# Bin-count sensitivity of the continuous peak-to-trough. This is the reason the
# printed swing symbol must name its own support: the statistic is an extreme
# order statistic over k groups and grows with k, so a 25-group value and a
# 3-level value are two different estimands and must not be compared silently.
SENS_BINS <- c(3L, 5L, 10L, 25L, 50L)
sens_list <- list()
for (v in CONT_VARS) {
  for (oc in names(OUT_DISPLAY)) {
    s <- dat[dat$Predictor == v & dat$Outcome == oc, , drop = FALSE]
    for (k in SENS_BINS) {
      zz <- bin_continuous(s, k)
      sens_list[[length(sens_list) + 1L]] <- data.frame(
        Predictor = v, Outcome = oc, n_groups = nrow(zz),
        requested_groups = k, swing = max(zz$med) - min(zz$med))
    }
  }
}
swing_sens <- do.call(rbind, sens_list)
# Like-for-like comparison value: the continuous swing recomputed on as many
# quantile groups as the median ordinal family has levels (three). This is the
# only continuous number that may be set beside an ordinal swing.
ORD_MEDIAN_LEVELS <- as.integer(stats::median(vapply(ORD_VARS, function(v) length(ORD_TICKS[[v]]), integer(1))))
swing_matched <- swing_sens[swing_sens$requested_groups == ORD_MEDIAN_LEVELS, ]
# -----------------------------------------------------------------------------
# 7. Per-cell swing: peak-to-trough of the PLOTTED MEDIAN, in log-hazard units.
#    This is what makes a free y scale per cell safe. No cell carries a
#    power-of-ten multiplier, because multipliers made apparent amplitude
#    anti-correlated with real swing in an earlier rendered bake-off.
#    TWO THINGS ARE PINNED HERE AND MUST BE PINNED THE SAME WAY IN THE METHODS.
#    First, the summary is the MEDIAN, not the mean; the mean-based version of the
#    same statistic is computed below only so the discrepancy with any mean-based
#    number in the manuscript is visible rather than discovered at proof stage.
#    Second, the SUPPORT differs by cell type: 25 quantile groups for continuous
#    cells against 3 or 5 natural levels for ordinal cells. The support is
#    therefore printed inside the symbol itself in every strip, as delta(25q) or
#    delta(3lv), so no single unqualified symbol carries two estimands.
# -----------------------------------------------------------------------------
swing_ord <- ord_summ %>%
  group_by(Predictor, Outcome) %>%
  summarise(swing = max(med) - min(med), .groups = "drop")
swing_cont <- cont_curve %>%
  group_by(Predictor, Outcome) %>%
  summarise(swing = max(med) - min(med), .groups = "drop")
swing_all <- bind_rows(swing_cont, swing_ord)
# Mean-based twin of the same statistic, for reconciliation only. It is never
# plotted and never printed in a strip.
swing_ord_mean <- ord_raw %>%
  group_by(Predictor, Outcome, lv) %>%
  summarise(mu = mean(SHAP_Impact), .groups = "drop") %>%
  group_by(Predictor, Outcome) %>%
  summarise(swing_mean = max(mu) - min(mu), .groups = "drop")
cont_mean_list <- list()
for (v in CONT_VARS) {
  for (oc in names(OUT_DISPLAY)) {
    s <- dat[dat$Predictor == v & dat$Outcome == oc, , drop = FALSE]
    nd <- length(unique(s$Feature_Value))
    nb <- min(TARGET_BINS, nd)
    br <- unique(stats::quantile(s$Feature_Value, probs = seq(0, 1, length.out = nb + 1L), names = FALSE, type = 7))
    s$bin <- cut(s$Feature_Value, breaks = br, include.lowest = TRUE, labels = FALSE)
    mu <- tapply(s$SHAP_Impact, s$bin, mean)
    cont_mean_list[[length(cont_mean_list) + 1L]] <- data.frame(
      Predictor = v, Outcome = oc, swing_mean = max(mu) - min(mu))
  }
}
swing_mean_all <- bind_rows(do.call(rbind, cont_mean_list), swing_ord_mean)
# Monotonicity of the level medians, checked rather than assumed. A connecting
# line between level medians reads as a dose-response, so the cells where the
# medians actually reverse are counted and named in the caption.
mono_ord <- ord_summ %>%
  group_by(Predictor, Outcome) %>%
  arrange(lv, .by_group = TRUE) %>%
  summarise(monotone = all(diff(med) >= 0) || all(diff(med) <= 0), .groups = "drop")
N_NONMONO <- sum(!mono_ord$monotone)
NONMONO_CELLS <- mono_ord[!mono_ord$monotone, ]
maxswing <- swing_all %>%
  group_by(Predictor) %>%
  summarise(max_swing = max(swing), .groups = "drop")
cont_order <- maxswing %>% filter(Predictor %in% CONT_VARS) %>% arrange(desc(max_swing)) %>% pull(Predictor)
ord_order <- maxswing %>% filter(Predictor %in% ORD_VARS) %>% arrange(desc(max_swing)) %>% pull(Predictor)
PRED_ORDER <- c(cont_order, ord_order)
# -----------------------------------------------------------------------------
# 8. Per-cell x and y limits, computed explicitly. Explicit limits also make the
#    figure reproducible independently of which layers happen to be drawn.
# -----------------------------------------------------------------------------
cell_key <- expand.grid(Predictor = PRED_ORDER, Outcome = names(OUT_DISPLAY),
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
cell_key$is_cont <- cell_key$Predictor %in% CONT_VARS
cell_key <- cell_key %>% left_join(swing_all, by = c("Predictor", "Outcome"))
cell_key$pred_rank <- match(cell_key$Predictor, PRED_ORDER)
cell_key$out_rank <- match(cell_key$Outcome, names(OUT_DISPLAY))
cell_key <- cell_key[order(cell_key$pred_rank, cell_key$out_rank), ]
rownames(cell_key) <- NULL
lim_rows <- vector("list", nrow(cell_key))
for (i in seq_len(nrow(cell_key))) {
  v <- cell_key$Predictor[i]
  oc <- cell_key$Outcome[i]
  if (cell_key$is_cont[i]) {
    z <- cont_curve[cont_curve$Predictor == v & cont_curve$Outcome == oc, ]
    m <- cont_meta[cont_meta$Predictor == v & cont_meta$Outcome == oc, ]
    # The axis runs over the FULL OBSERVED RANGE of the predictor, not over the
    # plotted bin medians. Ending the axis at the top bin's median put 59 percent
    # of the treatment-episode-length range and 57 percent of the poverty range
    # off panel, signalled only by 6 pt grey text, so the panel read as a 0 to 25
    # month axis. The end bins are instead drawn as horizontal whiskers spanning
    # their own observed extent, which states what the endpoint summarises.
    xl <- c(m$x_data_min, m$x_data_max)
    yl <- range(c(0, z$q25, z$q75))
  } else {
    p <- ord_points[ord_points$Predictor == v & ord_points$Outcome == oc, ]
    q <- ord_violin[ord_violin$Predictor == v & ord_violin$Outcome == oc, ]
    su <- ord_summ[ord_summ$Predictor == v & ord_summ$Outcome == oc, ]
    lvs <- sort(unique(su$lv))
    xl <- c(min(lvs) - 0.38, max(lvs) + 0.38)
    yl <- range(c(0, p$SHAP_Impact, q$SHAP_Impact, su$q25, su$q75))
  }
  if (diff(yl) <= 0) yl <- yl + c(-1, 1) * max(1e-6, abs(yl[1]) * 0.05 + 1e-6)
  lim_rows[[i]] <- data.frame(Predictor = v, Outcome = oc,
                              xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2])
}
cell_lims <- do.call(rbind, lim_rows)
cell_key <- cell_key %>% left_join(cell_lims, by = c("Predictor", "Outcome"))
# -----------------------------------------------------------------------------
# 9. Honest diagnostic of what the free y scale costs. The Spearman correlation
#    between apparent amplitude and the true swing CANNOT serve as evidence here:
#    apparent amplitude is swing divided by y_span, so it shares its numerator
#    with the swing and the correlation is positive by construction. Permuting
#    y_span across cells, which destroys every real relation, already produces a
#    correlation of about 0.83, so the observed value sits inside its own null.
#    The reportable quantity is instead the DISCORDANT PAIR RATE: over all pairs
#    of cells, how often is the cell with the larger true swing drawn with the
#    smaller apparent amplitude, and by what factor at worst.
# -----------------------------------------------------------------------------
cell_key$y_span <- cell_key$ymax - cell_key$ymin
cell_key$apparent_frac <- cell_key$swing / cell_key$y_span
RHO_ALL <- suppressWarnings(stats::cor(cell_key$apparent_frac, cell_key$swing, method = "spearman"))
ord_cells <- cell_key[!cell_key$is_cont, ]
RHO_ORD <- suppressWarnings(stats::cor(ord_cells$apparent_frac, ord_cells$swing, method = "spearman"))
# Permutation null for the Spearman, so the statistic is reported against the
# distribution it would take with panel scales carrying zero information.
perm_rho_null <- function(app, sw, y_span, n_perm = 4000L) {
  out <- numeric(n_perm)
  for (i in seq_len(n_perm)) {
    ys <- sample(y_span)
    out[i] <- suppressWarnings(stats::cor(sw / ys, sw, method = "spearman"))
  }
  out
}
NULL_ALL <- perm_rho_null(cell_key$apparent_frac, cell_key$swing, cell_key$y_span)
NULL_ORD <- perm_rho_null(ord_cells$apparent_frac, ord_cells$swing, ord_cells$y_span)
RHO_ALL_PCTL <- 100 * mean(NULL_ALL <= RHO_ALL)
RHO_ORD_PCTL <- 100 * mean(NULL_ORD <= RHO_ORD)
# Exhaustive pairwise discordance. A pair is discordant when the cell with the
# larger true swing is drawn at the smaller fraction of its own panel height.
discordance <- function(sw, app) {
  n <- length(sw)
  if (n < 2L) return(list(n_pairs = 0L, n_disc = 0L, pct = NA_real_, worst = NA_real_,
                          wi = NA_integer_, wj = NA_integer_))
  n_pairs <- 0L
  n_disc <- 0L
  worst <- 0
  wi <- NA_integer_
  wj <- NA_integer_
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      n_pairs <- n_pairs + 1L
      big <- if (sw[i] >= sw[j]) i else j
      small <- if (sw[i] >= sw[j]) j else i
      if (app[big] < app[small]) {
        n_disc <- n_disc + 1L
        fac <- if (sw[small] > 0) sw[big] / sw[small] else Inf
        if (is.finite(fac) && fac > worst) {
          worst <- fac
          wi <- big
          wj <- small
        }
      }
    }
  }
  list(n_pairs = n_pairs, n_disc = n_disc, pct = 100 * n_disc / n_pairs,
       worst = worst, wi = wi, wj = wj)
}
DISC_ALL <- discordance(cell_key$swing, cell_key$apparent_frac)
DISC_ORD <- discordance(ord_cells$swing, ord_cells$apparent_frac)
DELTA_GLYPH <- intToUtf8(0x0394)
EN_DASH <- intToUtf8(0x2013)
disc_pair_txt <- function(d, tab) {
  if (is.na(d$wi)) return("none")
  sprintf(paste0("%s (%s), ", DELTA_GLYPH, " %s drawn at %.0f%% of its panel, against %s (%s), ",
                 DELTA_GLYPH, " %s drawn at %.0f%%"),
          PRED_LAB[[tab$Predictor[d$wi]]], OUT_DISPLAY[[tab$Outcome[d$wi]]],
          sprintf("%.4f", tab$swing[d$wi]), 100 * tab$apparent_frac[d$wi],
          PRED_LAB[[tab$Predictor[d$wj]]], OUT_DISPLAY[[tab$Outcome[d$wj]]],
          sprintf("%.4f", tab$swing[d$wj]), 100 * tab$apparent_frac[d$wj])
}
DISC_ORD_WORST_TXT <- disc_pair_txt(DISC_ORD, ord_cells)
# The mortality-to-readmission ratio is computed WITHIN a predictor, so both
# outcomes share one support and the ratio is well defined for every cell type.
SWING_RATIO <- swing_all %>%
  select(Predictor, Outcome, swing) %>%
  pivot_wider(names_from = Outcome, values_from = swing) %>%
  mutate(ratio = Death / Readmission)
RATIO_MIN <- min(SWING_RATIO$ratio)
RATIO_MAX <- max(SWING_RATIO$ratio)
RATIO_MIN_VAR <- SWING_RATIO$Predictor[which.min(SWING_RATIO$ratio)]
RATIO_MAX_VAR <- SWING_RATIO$Predictor[which.max(SWING_RATIO$ratio)]
SWING_RATIO_MEAN <- swing_mean_all %>%
  pivot_wider(names_from = Outcome, values_from = swing_mean) %>%
  mutate(ratio = Death / Readmission)
RATIO_MEAN_MIN <- min(SWING_RATIO_MEAN$ratio)
RATIO_MEAN_MAX <- max(SWING_RATIO_MEAN$ratio)
RATIO_MEAN_MAX_VAR <- SWING_RATIO_MEAN$Predictor[which.max(SWING_RATIO_MEAN$ratio)]
# -----------------------------------------------------------------------------
# 10. Facet keys. The outer strip carries the bold predictor label MERGED across
#     that predictor's two cells; the inner strip carries the outcome name plus
#     that cell's raw swing. Putting the swing in the strip buys the mandatory
#     magnitude annotation without spending scarce panel height on it, and it can
#     never be occluded by a dense cloud.
# -----------------------------------------------------------------------------
fmt_swing <- function(x) sprintf("%.3f", x)
cell_key$pred_lab <- factor(PRED_LAB[cell_key$Predictor], levels = unname(PRED_LAB[PRED_ORDER]))
# The SUPPORT of the swing is CONSTANT BY CELL TYPE: every continuous cell uses the
# same number of quantile groups and every ordinal cell uses its own natural levels,
# so the parenthetical delta(25q) or delta(3lv) never varied within a type and was
# being printed 30 times to say one thing twice. The strips now carry a BARE delta
# and the two supports are named ONCE, in the legend, together with the warning that
# they are different estimands and must not be set side by side. The support is
# still COMPUTED here, because both text files quote it and no number may be
# hardcoded; only its repetition inside the artwork is removed.
cell_key$support_n <- vapply(seq_len(nrow(cell_key)), function(i) {
  v <- cell_key$Predictor[i]
  if (cell_key$is_cont[i]) {
    as.integer(cont_meta$n_bins[cont_meta$Predictor == v & cont_meta$Outcome == cell_key$Outcome[i]])
  } else {
    length(ORD_TICKS[[v]])
  }
}, integer(1))
# Render an integer set as English prose, so "25" or "3 or 5" is produced from the
# data rather than typed into a sentence that a later change would silently falsify.
fmt_int_set <- function(v) {
  v <- sort(unique(as.integer(v)))
  if (length(v) == 1L) return(sprintf("%d", v))
  paste0(paste(sprintf("%d", v[-length(v)]), collapse = ", "), " or ",
         sprintf("%d", v[length(v)]))
}
CONT_SUPPORT_TXT <- fmt_int_set(cell_key$support_n[cell_key$is_cont])
ORD_SUPPORT_TXT <- fmt_int_set(cell_key$support_n[!cell_key$is_cont])
N_CONT_CELLS <- sum(cell_key$is_cont)
N_ORD_CELLS <- sum(!cell_key$is_cont)
strip_lab <- function(out, val) paste0(out, "  ", DELTA_GLYPH, " ", val)
cell_key$out_lab_txt <- strip_lab(OUT_DISPLAY[cell_key$Outcome], fmt_swing(cell_key$swing))
# Two cells sharing an inner-strip label would either abort with an opaque
# duplicated-level error or, worse, silently merge two panels, so the collision is
# detected explicitly and the label precision is widened rather than guessed at.
if (anyDuplicated(cell_key$out_lab_txt)) {
  cell_key$out_lab_txt <- strip_lab(OUT_DISPLAY[cell_key$Outcome], sprintf("%.4f", cell_key$swing))
  message("[labels] two cells collided at 3 decimals; inner strips widened to 4 decimals.")
}
if (anyDuplicated(cell_key$out_lab_txt)) {
  stop("Two cells still share an inner strip label at 4 decimals; increase fmt_swing precision.")
}
cell_key$out_lab <- factor(cell_key$out_lab_txt, levels = unique(cell_key$out_lab_txt))
key_small <- cell_key[, c("Predictor", "Outcome", "pred_lab", "out_lab")]
attach_key <- function(d) dplyr::inner_join(d, key_small, by = c("Predictor", "Outcome"))
ord_points_f <- attach_key(ord_points)
ord_violin_f <- attach_key(ord_violin)
ord_summ_f <- attach_key(as.data.frame(ord_summ))
# Only the genuinely ordered families receive a connecting line between their
# level medians; the nominal families keep their circles and interquartile bars.
ord_summ_line <- ord_summ_f[ord_summ_f$Predictor %in% ORDERED_FAMILIES, , drop = FALSE]
cont_curve_f <- attach_key(as.data.frame(cont_curve))
# Foot row: the TRUE level share, printed as text inside each ordinal cell. This
# is how prevalence is communicated without encoding it as ink density: equal-n
# subsampling erases prevalence, and width-by-share turns rare levels into
# slivers that read as error bars.
# The LEFTMOST number in every ordinal foot row carries a percent sign, so the row
# cannot be mistaken for a second set of y values in a panel whose vertical axis
# already carries small decimals. Only the first is marked, to buy the unit at the
# smallest possible cost in width.
# The foot row is printed in the LEFT cell of each predictor pair ONLY. A level
# share is a property of the PREDICTOR, not of the outcome: level_share is itself
# computed on a single outcome precisely because the two outcomes carry identical
# Feature_Value vectors, which is asserted at runtime in section 3. Printing the
# same row again in the right-hand cell was therefore 12 redundant rows of the
# smallest type on the page. The reserved bottom gutter is untouched, because it
# comes from the y-scale expansion applied uniformly to all 30 cells, so panel
# heights stay identical and only the ink is removed.
FOOT_OUTCOME <- names(OUT_DISPLAY)[1]
foot_ord <- ord_summ_f %>%
  filter(Outcome == FOOT_OUTCOME) %>%
  left_join(level_share, by = c("Predictor", "lv")) %>%
  group_by(Predictor, Outcome) %>%
  arrange(lv, .by_group = TRUE) %>%
  mutate(lab = ifelse(lv == min(lv), sprintf("%.1f%%", share_pct), sprintf("%.1f", share_pct))) %>%
  ungroup() %>%
  select(Predictor, Outcome, pred_lab, out_lab, x = lv, lab)
# Three decimals for the poverty rate: two decimals printed its minimum of
# 0.0017 as "0.00", which asserts communes with a zero poverty rate.
fmt_x <- function(v, x) {
  if (v == "porc_pobr") sprintf("%.3f", x) else if (v == "dit_m") sprintf("%.1f", x) else sprintf("%.0f", x)
}
# The foot row of a continuous cell names the TOP BIN's interval, which is the
# quantity actually at risk of misreading: the rightmost plotted point summarises
# that whole interval. The full range no longer needs stating in text because the
# axis itself now runs over it. Like the ordinal foot row, this is a property of the
# PREDICTOR and not of the outcome, since the quantile breaks are cut on
# Feature_Value alone and Feature_Value is identical across the two outcomes, so it
# too is printed in the LEFT cell of the pair only.
foot_cont <- cont_meta %>%
  filter(Outcome == FOOT_OUTCOME) %>%
  inner_join(key_small, by = c("Predictor", "Outcome")) %>%
  left_join(cell_key[, c("Predictor", "Outcome", "xmin", "xmax")], by = c("Predictor", "Outcome")) %>%
  rowwise() %>%
  mutate(lab = sprintf(paste0("top bin %s", EN_DASH, "%s (%.1f%%)"), fmt_x(Predictor, top_bin_lo),
                       fmt_x(Predictor, top_bin_hi), 100 * top_bin_n / N_PER_CELL),
         x = (xmin + xmax) / 2) %>%
  ungroup() %>%
  select(Predictor, Outcome, pred_lab, out_lab, x, lab)
# End-bin geometry for the continuous cells. The top and bottom quantile bins are
# wide, so each is drawn as a band across its OWN observed extent at its own
# interquartile range, with a horizontal whisker at its median and a hollow point
# at the median x. This is what lets the axis run to the observed maximum without
# the panel reading as a curve that simply stops.
cont_ends <- cont_curve_f %>%
  group_by(Predictor, Outcome, pred_lab, out_lab) %>%
  arrange(bin, .by_group = TRUE) %>%
  slice(c(1L, dplyr::n())) %>%
  ungroup()
# -----------------------------------------------------------------------------
# 11. Typography. Serif Times New Roman, achromatic throughout so a grayscale
#     proof is pixel-identical to the colour proof. Type is pushed as large as an
#     8 x 4 grid on a 17.8 cm page allows; the smallest size is MEASURED later.
# -----------------------------------------------------------------------------
FAM <- "Times New Roman"
fam_ok <- tryCatch({
  m <- systemfonts::match_fonts(FAM)
  grepl("times", tolower(basename(m$path[1])), fixed = TRUE)
}, error = function(e) FALSE)
if (!fam_ok) {
  FAM <- "serif"
  message("[font] Times New Roman not matched; falling back to the generic serif family.")
}
PT_STRIP <- 7.6
PT_AXIS <- 6.4
PT_YTITLE <- 7.8
# The foot rows were the smallest type in the figure AND the only text set in
# grey25, the weakest combination on the page. They are raised to the axis size
# and to full grey10 density, so the smallest class is now 6.4 pt everywhere.
PT_FOOT <- 6.4
FOOT_COL <- "grey10"
GEOM_PT <- 1 / .pt
FIG_W_CM <- 17.8
FIG_H_CM <- 23.9
N_COL <- 4L
N_ROW <- 8L
str_cm <- function(s, size_pt, bold = FALSE) {
  w <- tryCatch(systemfonts::string_width(s, family = FAM, size = size_pt, bold = bold),
                error = function(e) rep(NA_real_, length(s)))
  if (all(is.na(w))) return(nchar(s) * size_pt * 0.5 / 72 * 2.54)
  w / 72 * 2.54
}
# -----------------------------------------------------------------------------
# 12. Per-panel scales, built explicitly for all 30 cells and handed to
#     facetted_pos_scales in the panel order the facet actually produces.
# -----------------------------------------------------------------------------
# Thin an over-full break vector by keeping every k-th entry, which preserves
# even spacing. When zero is among the breaks the stride is anchored on it, so
# thinning can never discard the value the dotted reference rule marks.
# Vertical labels are short and stack on a 2 cm axis without touching, so the
# vertical axis tolerates five; horizontal labels are wide and four is the limit.
MAX_TICKS_Y <- 5L
# The horizontal tick budget is no longer a single hard number. A fixed cap of
# four labelled a month axis on the odd stride 5, 15, 25 with no tick at 10 or 20,
# which is a needless reading obstacle. The cap is now the largest count whose
# rendered labels actually fit the cell, measured with the real font metrics.
MAX_TICKS_X <- 5L
MIN_TICKS_X <- 3L
EST_CELL_W_CM <- FIG_W_CM / N_COL * 0.80
X_LABEL_FILL <- 0.82
ZERO_TOL <- 1e-12
# Typographic minus sign (U+2212), written as an escape so no editor or encoding
# round trip can turn it into a mojibaked byte pair.
MINUS_GLYPH <- "\u2212"
thin_breaks <- function(b, max_ticks) {
  n <- length(b)
  if (n <= max_ticks) return(b)
  for (k in seq(ceiling(n / max_ticks), 1L)) {
    anchor <- which(abs(b) < ZERO_TOL)
    starts <- if (length(anchor)) unique(c(((anchor[1] - 1L) %% k) + 1L, 1L)) else 1L
    for (st in starts) {
      idx <- seq(st, n, by = k)
      if (length(idx) >= 3L && length(idx) <= max_ticks) return(b[idx])
    }
  }
  b[unique(round(seq(1, n, length.out = max_ticks)))]
}
# A grid of multiples of a "nice" step (1, 2, 2.5 or 5 times a power of ten).
# When the cell straddles zero this grid necessarily contains zero, because zero
# is a multiple of every step, so the value marked by the dotted reference rule
# is always named on the axis. Steps yielding 3 or 4 in-range values are kept and
# the one closest to three and a half labels wins, which suits a 2 cm tall cell.
nice_grid_breaks <- function(lo, hi, max_ticks) {
  span <- hi - lo
  if (!is.finite(span) || span <= 0) return(numeric(0))
  mags <- 10^(seq(floor(log10(span)) - 2L, floor(log10(span)) + 1L))
  steps <- sort(unique(as.vector(outer(c(1, 2, 2.5, 5), mags))))
  best <- NULL
  for (step in steps) {
    lo_i <- ceiling(lo / step - 1e-9)
    hi_i <- floor(hi / step + 1e-9)
    if (hi_i < lo_i) next
    b <- (lo_i:hi_i) * step
    if (length(b) < 3L || length(b) > max_ticks) next
    sc <- abs(length(b) - 3.5)
    if (is.null(best) || sc < best$sc) best <- list(b = b, sc = sc)
  }
  if (is.null(best)) numeric(0) else best$b
}
# A cell 2 cm tall still needs three reference values on its vertical axis, and
# one break count often yields only one or two once out-of-range entries are
# dropped. The zero-anchored nice grid is tried first for cells that straddle
# zero, then the standard algorithms, and every path is checked for a minimum of
# three labels before it is accepted.
pick_breaks_in <- function(lo, hi, prefer_zero = TRUE, max_ticks = MAX_TICKS_Y) {
  if (prefer_zero && lo <= 0 && hi >= 0) {
    b <- nice_grid_breaks(lo, hi, max_ticks)
    if (length(b) >= 3L) return(b)
  }
  for (n in c(4L, 3L, 5L, 6L)) {
    b <- scales::breaks_extended(n = n)(c(lo, hi))
    b <- b[is.finite(b) & b >= lo & b <= hi]
    if (length(b) >= 3L && length(b) <= max_ticks) return(b)
  }
  b <- nice_grid_breaks(lo, hi, max_ticks)
  if (length(b) >= 3L) return(b)
  for (n in c(4L, 5L, 6L, 8L)) {
    b <- pretty(c(lo, hi), n = n)
    b <- b[is.finite(b) & b >= lo & b <= hi]
    if (length(b) >= 3L) return(thin_breaks(b, max_ticks))
  }
  b <- unique(signif(seq(lo, hi, length.out = 3L), 2))
  b <- b[b >= lo & b <= hi]
  if (length(b) < 2L) b <- unique(c(lo, hi))
  b
}
# Decimal places are chosen as the fewest that both keep every label distinct AND
# reproduce each break value to within one percent of the break spacing. Deriving
# the count from the spacing alone is wrong: breaks of 0.05, -0.05 and -0.15 have
# a spacing of 0.1, and one decimal renders them as 0.0, -0.0 and -0.2, which is
# both ambiguous and untrue. Any residual signed zero loses its sign.
num_lab <- function(b) {
  out <- rep("", length(b))
  ok <- is.finite(b)
  b2 <- b[ok]
  if (!length(b2)) return(out)
  sp <- if (length(b2) > 1) min(diff(sort(b2))) else max(abs(b2), 1)
  tol <- max(1e-12, abs(sp) * 0.01)
  d <- 6L
  for (dd in 0:6) {
    s_try <- formatC(b2, format = "f", digits = dd)
    if (anyDuplicated(s_try)) next
    back <- suppressWarnings(as.numeric(s_try))
    if (all(is.finite(back)) && max(abs(back - b2)) <= tol) {
      d <- dd
      break
    }
  }
  s <- formatC(b2, format = "f", digits = d)
  s <- sub("^-(0[.]?0*)$", "\\1", s)
  s <- sub("^-", MINUS_GLYPH, s)
  out[ok] <- s
  out
}
y_lab_fun <- function(b) num_lab(b)
# Choose the horizontal breaks by MEASURING the labels they would produce, from
# the most generous cap downwards, and accepting the first set that fits.
pick_x_breaks <- function(xmin, xmax) {
  budget <- EST_CELL_W_CM * X_LABEL_FILL
  best <- NULL
  for (mt in seq(MAX_TICKS_X, MIN_TICKS_X)) {
    br <- pick_breaks_in(xmin, xmax, prefer_zero = FALSE, max_ticks = mt)
    if (!length(br)) next
    labs <- num_lab(br)
    w <- sum(str_cm(labs, PT_AXIS, bold = FALSE), na.rm = TRUE)
    if (is.null(best)) best <- list(br = br, labs = labs, w = w)
    if (is.finite(w) && w <= budget) return(list(br = br, labs = labs, w = w))
  }
  if (is.null(best)) {
    br <- pick_breaks_in(xmin, xmax, prefer_zero = FALSE, max_ticks = MAX_TICKS_X)
    best <- list(br = br, labs = num_lab(br), w = NA_real_)
  }
  best
}
make_x_scale <- function(v, xmin, xmax) {
  if (v %in% CONT_VARS) {
    px <- pick_x_breaks(xmin, xmax)
    br <- px$br
    labs <- px$labs
    scale_x_continuous(limits = c(xmin, xmax), breaks = br, labels = labs,
                       expand = expansion(mult = 0.035), oob = scales::squish)
  } else {
    lvs <- seq(ceiling(xmin), floor(xmax), by = 1)
    tk <- ORD_TICKS[[v]]
    labs <- tk[lvs + 1L]
    labs[is.na(labs)] <- as.character(lvs[is.na(labs)])
    scale_x_continuous(limits = c(xmin, xmax), breaks = lvs, labels = labs,
                       expand = expansion(mult = 0.01), oob = scales::squish)
  }
}
make_y_scale <- function(ymin, ymax) {
  br <- pick_breaks_in(ymin, ymax)
  scale_y_continuous(limits = c(ymin, ymax), breaks = br, labels = y_lab_fun,
                     expand = expansion(mult = c(0.155, 0.055)), oob = scales::squish)
}
# -----------------------------------------------------------------------------
# 13. Theme. Achromatic, no panel grid, thin panel borders to delineate 30 small
#     cells. Strips are given one uniform size here; the outer and inner strips
#     are differentiated by gtable surgery below, because ggh4x's own per-layer
#     strip theming is broken against ggplot2 4.0.
# -----------------------------------------------------------------------------
fig_theme <- theme_bw(base_family = FAM) +
  theme(
    text = element_text(family = FAM, colour = "grey10"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    # The rect stroke is centred on the panel edge and its outer half is clipped
    # away, so a nominal 0.17 rendered at about 0.18 pt, below the 0.25 pt minimum
    # rule weight LWW production specifies. Doubling the nominal lands the visible
    # half at roughly 0.36 pt, matching the ticks, which are not clipped.
    panel.border = element_rect(fill = NA, colour = "grey35", linewidth = 0.34),
    panel.spacing.x = unit(2.6, "pt"),
    panel.spacing.y = unit(2.8, "pt"),
    strip.background = element_rect(fill = "grey90", colour = NA),
    strip.text = element_text(family = FAM, size = PT_STRIP, colour = "grey10",
                              margin = margin(t = 1.1, b = 1.1, l = 1.2, r = 1.2, unit = "pt")),
    strip.clip = "off",
    axis.text.x = element_text(family = FAM, size = PT_AXIS, colour = "grey10",
                               margin = margin(t = 0.8, unit = "pt")),
    axis.text.y = element_text(family = FAM, size = PT_AXIS, colour = "grey10",
                               margin = margin(r = 0.8, unit = "pt")),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.17),
    axis.ticks.length = unit(1.2, "pt"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = FAM, size = PT_YTITLE, colour = "grey10",
                                margin = margin(r = 1.8, unit = "pt")),
    legend.position = "none",
    plot.margin = margin(t = 2, r = 3, b = 2, l = 2, unit = "pt")
  )
# -----------------------------------------------------------------------------
# 14. Plot assembly. One plot object holds every layer; the two output passes are
#     produced later by masking grobs inside a single finished gtable, which is
#     the only way to guarantee that the rasterized points register exactly with
#     the vector artwork drawn over them.
# -----------------------------------------------------------------------------
build_plot <- function() {
  ggplot() +
    # Density-scaled sina cloud, half width 0.22 of the level spacing, equal-n.
    geom_point(data = ord_points_f, aes(x = xpos, y = SHAP_Impact),
               size = 0.18, alpha = 0.25, colour = "grey58", shape = 16, stroke = 0) +
    # End-bin bands FIRST, so the interquartile shading is continuous from the
    # left panel edge to the right one. Each band covers the end bin's own
    # observed extent at that bin's own quartiles; it is a summary of a wide bin,
    # not an extrapolation.
    geom_rect(data = cont_ends, aes(xmin = x_lo, xmax = x_hi, ymin = q25, ymax = q75),
              fill = "grey80", colour = NA) +
    # Continuous cells: IQR ribbon, then a median line heavier than the default.
    geom_ribbon(data = cont_curve_f, aes(x = x, ymin = q25, ymax = q75),
                fill = "grey80", colour = NA) +
    geom_line(data = cont_curve_f, aes(x = x, y = med),
              colour = "grey10", linewidth = 0.62, lineend = "round") +
    # Horizontal whisker across each end bin at its median, with a hollow point at
    # the bin median. This is what states that the endpoint summarises a wide
    # interval: for treatment episode length the top bin runs to 60.7 months.
    geom_segment(data = cont_ends, aes(x = x_lo, xend = x_hi, y = med, yend = med),
                 colour = "grey10", linewidth = 0.62, lineend = "butt") +
    geom_point(data = cont_ends, aes(x = x, y = med), shape = 21, fill = "white",
               colour = "grey10", size = 1.15, stroke = 0.38) +
    # White halo underneath keeps the summary marker open inside a dense cloud.
    # It is drawn BEFORE the violin outline: at 600 dpi a halo of linewidth 1.05
    # is 25 px against the outline's 4 px, so drawing the halo afterwards cut a
    # clean white band through the very outline that carries the shape.
    geom_linerange(data = ord_summ_f, aes(x = lv, ymin = q25, ymax = q75),
                   colour = "white", linewidth = 1.05, lineend = "butt") +
    geom_line(data = ord_summ_line, aes(x = lv, y = med), colour = "white", linewidth = 0.95) +
    # Unfilled violin outline: 66 of 76 level-cells are unimodal, so the bare
    # cloud does not pay for its ink; the outline is what carries the shape.
    geom_violin(data = ord_violin_f, aes(x = lv, y = SHAP_Impact, group = lv),
                fill = NA, colour = "grey20", linewidth = 0.18,
                scale = "width", trim = TRUE, adjust = VIOLIN_ADJUST,
                width = 2 * SINA_HALF_WIDTH, orientation = "x") +
    geom_linerange(data = ord_summ_f, aes(x = lv, ymin = q25, ymax = q75),
                   colour = "grey10", linewidth = 0.42, lineend = "butt") +
    # The connecting line is drawn ONLY for genuinely ordered families. Household
    # tenure status is a nominal classification containing a residual "other"
    # level, so joining its medians would assert a precarity gradient the coding
    # does not support, and its medians are in fact non-monotone in both outcomes.
    geom_line(data = ord_summ_line, aes(x = lv, y = med), colour = "grey10", linewidth = 0.38) +
    geom_point(data = ord_summ_f, aes(x = lv, y = med), shape = 21, fill = "white",
               colour = "grey10", size = 1.15, stroke = 0.38) +
    # Foot rows live in the reserved bottom gutter of every cell.
    geom_text(data = foot_ord, aes(x = x, y = -Inf, label = lab),
              family = FAM, size = PT_FOOT * GEOM_PT, colour = FOOT_COL, vjust = -0.25) +
    geom_text(data = foot_cont, aes(x = x, y = -Inf, label = lab),
              family = FAM, size = PT_FOOT * GEOM_PT, colour = FOOT_COL, vjust = -0.25) +
    # Zero reference LAST, so no flat curve can occlude it.
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.24, colour = "grey30") +
    ggh4x::facet_nested_wrap(vars(pred_lab, out_lab), ncol = N_COL, scales = "free",
                             dir = "h", nest_line = element_blank(), solo_line = FALSE) +
    labs(x = NULL, y = "SHAP contribution to the log hazard") +
    fig_theme
}
p_probe <- build_plot()
lay <- ggplot_build(p_probe)$layout$layout
lay <- lay[order(lay$PANEL), ]
if (nrow(lay) != nrow(cell_key)) stop("Facet produced an unexpected number of panels.")
if (!identical(as.character(cell_key$pred_lab), as.character(lay$pred_lab)) ||
    !identical(as.character(cell_key$out_lab), as.character(lay$out_lab))) {
  stop("Panel order does not match the intended order; per-panel scales would be misassigned.")
}
if (max(lay$ROW) != N_ROW || max(lay$COL) != N_COL) {
  stop(sprintf("Grid is %d x %d, expected %d x %d.", max(lay$ROW), max(lay$COL), N_ROW, N_COL))
}
x_scales <- lapply(seq_len(nrow(cell_key)), function(i)
  make_x_scale(cell_key$Predictor[i], cell_key$xmin[i], cell_key$xmax[i]))
y_scales <- lapply(seq_len(nrow(cell_key)), function(i)
  make_y_scale(cell_key$ymin[i], cell_key$ymax[i]))
# Independent audit of every axis: recompute the breaks and their printed labels,
# then check that each label reads back to its own break, that no two labels in a
# cell collide, and that zero is named whenever the cell straddles it. Silent
# mislabelling of a break is a defect that is invisible in a thumbnail.
axis_audit <- lapply(seq_len(nrow(cell_key)), function(i) {
  yb <- pick_breaks_in(cell_key$ymin[i], cell_key$ymax[i])
  yl <- num_lab(yb)
  back <- suppressWarnings(as.numeric(sub(MINUS_GLYPH, "-", yl, fixed = TRUE)))
  # The horizontal axis is audited too. Reporting only vertical results under
  # unqualified names such as "axis_labels_all_unique" reads as a figure-wide
  # guarantee that was never actually computed for the horizontal axis.
  if (cell_key$is_cont[i]) {
    px <- pick_x_breaks(cell_key$xmin[i], cell_key$xmax[i])
    xb <- px$br
    xlab <- px$labs
    xback <- suppressWarnings(as.numeric(sub(MINUS_GLYPH, "-", xlab, fixed = TRUE)))
    x_err <- if (length(xb)) max(abs(xback - xb)) else 0
    x_uniq <- !as.logical(anyDuplicated(xlab))
    x_w <- px$w
  } else {
    lvs <- seq(ceiling(cell_key$xmin[i]), floor(cell_key$xmax[i]), by = 1)
    xlab <- ORD_TICKS[[cell_key$Predictor[i]]][lvs + 1L]
    xlab[is.na(xlab)] <- as.character(lvs[is.na(xlab)])
    xb <- lvs
    x_err <- 0
    x_uniq <- !as.logical(anyDuplicated(xlab))
    x_w <- sum(str_cm(xlab, PT_AXIS, bold = FALSE), na.rm = TRUE)
  }
  data.frame(
    cell = i,
    Predictor = unname(PRED_LAB[cell_key$Predictor[i]]),
    Outcome = unname(OUT_DISPLAY[cell_key$Outcome[i]]),
    n_y_breaks = length(yb),
    y_labels = paste(yl, collapse = " "),
    max_label_error = max(abs(back - yb)),
    labels_unique = !as.logical(anyDuplicated(yl)),
    zero_in_range = cell_key$ymin[i] <= 0 && cell_key$ymax[i] >= 0,
    zero_labelled = any(abs(yb) < ZERO_TOL),
    n_x_breaks = length(xb),
    x_labels = paste(xlab, collapse = " "),
    x_max_label_error = x_err,
    x_labels_unique = x_uniq,
    x_label_cm = x_w,
    stringsAsFactors = FALSE)
})
axis_audit <- do.call(rbind, axis_audit)
AXIS_ALL_UNIQUE <- all(axis_audit$labels_unique)
AXIS_MAX_ERR <- max(axis_audit$max_label_error)
AXIS_MIN_BREAKS <- min(axis_audit$n_y_breaks)
AXIS_ZERO_OK <- all(axis_audit$zero_labelled[axis_audit$zero_in_range])
AXIS_X_ALL_UNIQUE <- all(axis_audit$x_labels_unique)
AXIS_X_MAX_ERR <- max(axis_audit$x_max_label_error)
AXIS_X_MIN_BREAKS <- min(axis_audit$n_x_breaks)
AXIS_X_WIDEST_CM <- max(axis_audit$x_label_cm, na.rm = TRUE)
if (!AXIS_ALL_UNIQUE) stop("Two breaks in the same cell would print the same label.")
if (!AXIS_X_ALL_UNIQUE) stop("Two horizontal breaks in the same cell would print the same label.")
if (AXIS_MAX_ERR > 1e-6) {
  message(sprintf("[warn] largest axis label rounding error is %.3g log-hazard units.", AXIS_MAX_ERR))
}
p_full <- build_plot() + ggh4x::facetted_pos_scales(x = x_scales, y = y_scales)
# -----------------------------------------------------------------------------
# 15. Gtable surgery helpers.
#     absolutize(): grobwidth and grobheight units resolve against whatever
#     device is drawing, so a gtable measured on ragg would shift on a PDF
#     device. Freezing every non-null unit to centimetres on the ragg device is
#     what keeps the rasterized point layer registered with the vector overlay.
# -----------------------------------------------------------------------------
absolutize <- function(u, horizontal = TRUE) {
  conv <- if (horizontal) grid::convertWidth else grid::convertHeight
  out <- u
  for (i in seq_along(u)) {
    ui <- u[i]
    if (grepl("null", as.character(ui), fixed = TRUE)) next
    out[i] <- grid::unit(conv(ui, "cm", valueOnly = TRUE), "cm")
  }
  out
}
# Outer strips sit in internal row 1 of the strip gtable, inner strips in row 2.
# The outer label is set bold on a darker ground; the inner stays plain on a
# lighter ground, which is the whole point of the merged nested strip.
style_strip_tree <- function(x, bold, fill) {
  if (inherits(x, "rect")) {
    gp <- x$gp
    if (is.null(gp)) gp <- grid::gpar()
    gp$fill <- fill
    gp$col <- NA
    x$gp <- gp
    return(x)
  }
  if (inherits(x, "text")) {
    gp <- x$gp
    if (is.null(gp)) gp <- grid::gpar()
    gp$font <- if (bold) 2L else 1L
    gp$fontface <- NULL
    x$gp <- gp
    return(x)
  }
  if (inherits(x, "gtable")) {
    if (length(x$grobs)) x$grobs <- lapply(x$grobs, style_strip_tree, bold = bold, fill = fill)
    return(x)
  }
  if (inherits(x, "gTree")) {
    nms <- names(x$children)
    for (nm in nms) x$children[[nm]] <- style_strip_tree(x$children[[nm]], bold = bold, fill = fill)
    return(x)
  }
  x
}
restyle_strips <- function(g, fill_outer = "grey86", fill_inner = "grey96") {
  idx <- grep("^strip", g$layout$name)
  n_outer <- 0L
  n_inner <- 0L
  for (i in idx) {
    sg <- g$grobs[[i]]
    if (!inherits(sg, "gtable") || !nrow(sg$layout)) next
    is_outer <- min(sg$layout$t) == 1L
    if (is_outer) n_outer <- n_outer + 1L else n_inner <- n_inner + 1L
    g$grobs[[i]] <- style_strip_tree(sg, bold = is_outer,
                                     fill = if (is_outer) fill_outer else fill_inner)
  }
  attr(g, "n_outer_strips") <- n_outer
  attr(g, "n_inner_strips") <- n_inner
  g
}
# Gestalt proximity was fighting the grouping: the two cells of ONE predictor sat
# 0.80 cm apart while two DIFFERENT predictors sat only 0.69 cm apart, leaving the
# merged grey strip as the sole grouping cue. The gutter between the two nested
# blocks is widened here, after the widths are frozen, so the block boundary is
# the widest white space in the row. The null panel columns absorb the cost.
BLOCK_GAP_EXTRA_CM <- 0.22
panel_cols <- function(g) sort(unique(g$layout$l[grep("^panel", g$layout$name)]))
panel_rows <- function(g) sort(unique(g$layout$t[grep("^panel", g$layout$name)]))
widen_block_gutter <- function(g) {
  pc <- panel_cols(g)
  if (length(pc) < 4L) return(g)
  gap_col <- pc[2] + 1L
  if (gap_col >= pc[3]) return(g)
  cur <- as.numeric(g$widths[gap_col])
  if (!is.finite(cur)) return(g)
  g$widths[gap_col] <- grid::unit(cur + BLOCK_GAP_EXTRA_CM, "cm")
  # Report the TOTAL fixed white space between two adjacent panel columns, not the
  # width of one gtable column: the panel spacing and the next panel's axis column
  # are separate columns and quoting only one of them understates the gap.
  gap_between <- function(a, b) {
    idx <- seq.int(a + 1L, b - 1L)
    idx <- idx[idx >= 1L & idx <= length(g$widths)]
    sum(vapply(idx, function(k) {
      if (grepl("null", as.character(g$widths[k]), fixed = TRUE)) 0 else as.numeric(g$widths[k])
    }, numeric(1)))
  }
  attr(g, "block_gap_cm") <- gap_between(pc[2], pc[3])
  attr(g, "within_gap_cm") <- mean(c(gap_between(pc[1], pc[2]), gap_between(pc[3], pc[4])))
  g
}
# The 8 x 4 grid leaves two empty slots and they are LEFT BLANK. An earlier version
# filled them with an in-image KEY. That key was removed on request: every clause it
# carried is a caption clause, it duplicated the legend word for word, and a journal
# legend is typeset under the artwork anyway, so the key bought repetition at the
# cost of the only white space on a page holding 30 cells. Nothing replaces it; the
# blank slots are counted in the build summary and every statement the key made now
# lives in the legend file, which is checked below for length.
# Count the empty slots so the removal is a MEASURED fact in the summary rather than
# an assumption about how facet_nested_wrap fills its last row.
count_free_slots <- function(g) {
  pc <- panel_cols(g)
  pr <- panel_rows(g)
  if (!length(pc) || !length(pr)) return(NA_integer_)
  last_t <- pr[length(pr)]
  occupied <- g$layout$l[grep("^panel", g$layout$name)][
    g$layout$t[grep("^panel", g$layout$name)] == last_t]
  length(setdiff(pc, occupied))
}
# A point grob holding more than this many marks is the sina cloud; the median
# markers hold at most five. This is how the two geom_point layers are told
# apart without depending on child naming or layer order.
CLOUD_MIN_MARKS <- 50L
set_alpha0 <- function(x) {
  gp <- x$gp
  if (is.null(gp)) gp <- grid::gpar()
  gp$alpha <- 0
  x$gp <- gp
  x
}
is_cloud_grob <- function(x) inherits(x, "points") && length(x$x) > CLOUD_MIN_MARKS
mask_panel <- function(pg, hide_cloud) {
  if (!inherits(pg, "gTree")) return(pg)
  nms <- names(pg$children)
  for (nm in nms) {
    ch <- pg$children[[nm]]
    if (identical(is_cloud_grob(ch), hide_cloud)) pg$children[[nm]] <- set_alpha0(ch)
  }
  pg
}
count_cloud_grobs <- function(g) {
  idx <- grep("^panel", g$layout$name)
  n <- 0L
  for (i in idx) {
    pg <- g$grobs[[i]]
    if (!inherits(pg, "gTree")) next
    for (nm in names(pg$children)) if (is_cloud_grob(pg$children[[nm]])) n <- n + 1L
  }
  n
}
# Mask 1: clouds only, everything else invisible, for the rasterized pass.
mask_cloud_only <- function(g) {
  for (i in seq_along(g$grobs)) {
    nm <- g$layout$name[i]
    if (grepl("^panel", nm)) {
      g$grobs[[i]] <- mask_panel(g$grobs[[i]], hide_cloud = FALSE)
    } else {
      g$grobs[[i]] <- set_alpha0(g$grobs[[i]])
    }
  }
  g
}
# Mask 2: everything except the clouds, for the vector pass drawn over the
# raster. The plot background is masked too, because the composite paints its
# own white ground before laying the raster down.
mask_no_cloud <- function(g) {
  for (i in seq_along(g$grobs)) {
    nm <- g$layout$name[i]
    if (grepl("^panel", nm)) {
      g$grobs[[i]] <- mask_panel(g$grobs[[i]], hide_cloud = TRUE)
    } else if (identical(nm, "background")) {
      g$grobs[[i]] <- set_alpha0(g$grobs[[i]])
    }
  }
  g
}
# -----------------------------------------------------------------------------
# 16. Render the master PNG: exactly 17.8 cm wide at 600 dpi via ragg::agg_png.
#     The gtable is built, frozen and restyled inside the ragg device so that all
#     text metrics come from the device that actually renders the figure.
# -----------------------------------------------------------------------------
message("[render] building gtable and master PNG ...")
ragg::agg_png(PNG_PATH, width = FIG_W_CM, height = FIG_H_CM, units = "cm", res = 600,
              background = "white", scaling = 1)
G <- ggplot2::ggplotGrob(p_full)
G$widths <- absolutize(G$widths, horizontal = TRUE)
G$heights <- absolutize(G$heights, horizontal = FALSE)
G <- restyle_strips(G)
N_OUTER_STRIPS <- attr(G, "n_outer_strips")
N_INNER_STRIPS <- attr(G, "n_inner_strips")
G <- widen_block_gutter(G)
BLOCK_GAP_CM <- attr(G, "block_gap_cm")
WITHIN_GAP_CM <- attr(G, "within_gap_cm")
FREE_SLOTS <- count_free_slots(G)
N_CLOUD_GROBS <- count_cloud_grobs(G)
grid::grid.newpage()
grid::grid.draw(G)
invisible(dev.off())
if (N_OUTER_STRIPS != length(PRED_ORDER) || N_INNER_STRIPS != nrow(cell_key)) {
  message(sprintf("[warn] strip counts unexpected: outer=%d (want %d), inner=%d (want %d)",
                  N_OUTER_STRIPS, length(PRED_ORDER), N_INNER_STRIPS, nrow(cell_key)))
}
if (N_CLOUD_GROBS != sum(!cell_key$is_cont)) {
  message(sprintf("[warn] found %d cloud grobs, expected %d ordinal cells.",
                  N_CLOUD_GROBS, sum(!cell_key$is_cont)))
}
png_dim <- dim(png::readPNG(PNG_PATH))
PNG_W_PX <- png_dim[2]
PNG_H_PX <- png_dim[1]
PNG_W_CM <- PNG_W_PX / 600 * 2.54
PNG_H_CM <- PNG_H_PX / 600 * 2.54
PNG_BYTES <- file.info(PNG_PATH)$size
# -----------------------------------------------------------------------------
# 17. Measure the real cell geometry from the frozen gtable rather than trusting
#     the budget arithmetic that chose the type sizes.
# -----------------------------------------------------------------------------
is_null_unit <- function(u) vapply(seq_along(u), function(i) grepl("null", as.character(u[i]), fixed = TRUE), logical(1))
wnull <- is_null_unit(G$widths)
hnull <- is_null_unit(G$heights)
W_FIXED <- sum(vapply(which(!wnull), function(i) as.numeric(G$widths[i]), numeric(1)))
H_FIXED <- sum(vapply(which(!hnull), function(i) as.numeric(G$heights[i]), numeric(1)))
W_NULLW <- vapply(which(wnull), function(i) as.numeric(G$widths[i]), numeric(1))
H_NULLW <- vapply(which(hnull), function(i) as.numeric(G$heights[i]), numeric(1))
CELL_W_CM <- if (length(W_NULLW)) (FIG_W_CM - W_FIXED) * W_NULLW[1] / sum(W_NULLW) else NA_real_
CELL_H_CM <- if (length(H_NULLW)) (FIG_H_CM - H_FIXED) * H_NULLW[1] / sum(H_NULLW) else NA_real_
# The merged outer strip spans two panels plus the inner gutter between them.
strip_span_cm <- function(g) {
  idx <- grep("^strip", g$layout$name)
  best <- NA_real_
  for (i in idx) {
    l <- g$layout$l[i]
    r <- g$layout$r[i]
    if (r <= l) next
    tot <- 0
    for (k in l:r) {
      if (is_null_unit(g$widths[k])) tot <- tot + CELL_W_CM else tot <- tot + as.numeric(g$widths[k])
    }
    if (is.na(best) || tot > best) best <- tot
  }
  best
}
OUTER_SPAN_CM <- strip_span_cm(G)
longest_outer <- unname(PRED_LAB[which.max(nchar(PRED_LAB))])
OUTER_LAB_CM <- str_cm(longest_outer, PT_STRIP, bold = TRUE)
longest_inner <- cell_key$out_lab_txt[which.max(nchar(cell_key$out_lab_txt))]
INNER_LAB_CM <- str_cm(longest_inner, PT_STRIP, bold = FALSE)
OUTER_FITS <- is.finite(OUTER_LAB_CM) && is.finite(OUTER_SPAN_CM) && OUTER_LAB_CM <= OUTER_SPAN_CM
INNER_FITS <- is.finite(INNER_LAB_CM) && is.finite(CELL_W_CM) && INNER_LAB_CM <= CELL_W_CM
# -----------------------------------------------------------------------------
# 18. Measure the smallest glyph ACTUALLY rendered, instead of claiming that the
#     nominal point size is what reached the page.
# -----------------------------------------------------------------------------
SIZES_USED <- c(strip_outer = PT_STRIP, strip_inner = PT_STRIP, axis_text = PT_AXIS,
                axis_title_y = PT_YTITLE, foot_text = PT_FOOT)
SMALLEST_PT <- min(SIZES_USED)
# Measured, not asserted: glyph_info returns the actual inked bounding box of a
# rendered glyph in device pixels at the requested resolution, so this reports
# what reaches the page rather than the nominal font size that was requested.
measure_glyph <- function(size_pt, glyph = "0") {
  out <- list(ink_h_pt = NA_real_, ink_w_pt = NA_real_, method = "none")
  try({
    gi <- systemfonts::glyph_info(glyph, family = FAM, size = size_pt, res = 600)
    if (nrow(gi) >= 1L && is.finite(gi$height[1]) && gi$height[1] > 0) {
      out$ink_h_pt <- gi$height[1] / 600 * 72
      out$ink_w_pt <- gi$width[1] / 600 * 72
      out$method <- "glyph_info"
    }
  }, silent = TRUE)
  if (!is.finite(out$ink_h_pt) || out$ink_h_pt <= 0) {
    try({
      m <- systemfonts::shape_string(glyph, family = FAM, size = size_pt, res = 600)$metrics
      h <- m$height[1] - m$top_bearing[1] - m$bottom_bearing[1]
      if (is.finite(h) && h > 0) {
        out$ink_h_pt <- h / 600 * 72
        out$ink_w_pt <- m$width[1] / 600 * 72
        out$method <- "shape_string"
      }
    }, silent = TRUE)
  }
  if (!is.finite(out$ink_h_pt) || out$ink_h_pt <= 0) {
    out$ink_h_pt <- size_pt * 0.662
    out$ink_w_pt <- size_pt * 0.5
    out$method <- "analytic_fallback"
  }
  out
}
GLYPH <- measure_glyph(SMALLEST_PT)
SMALLEST_INK_PT <- GLYPH$ink_h_pt
SMALLEST_INK_PX <- if (is.finite(SMALLEST_INK_PT)) SMALLEST_INK_PT / 72 * 600 else NA_real_
SMALLEST_INK_MM <- if (is.finite(SMALLEST_INK_PT)) SMALLEST_INK_PT / 72 * 25.4 else NA_real_
# -----------------------------------------------------------------------------
# 19. PDF with the point layers rasterized. ggrastr is absent, so this is done by
#     hand: the cloud-only pass is rendered to a transparent 600 dpi raster and
#     laid down first, then the vector pass, from the SAME frozen gtable, is
#     drawn over it. Tens of thousands of marks therefore ship as one image
#     rather than as vector points.
# -----------------------------------------------------------------------------
message("[render] rasterized point layer ...")
ragg::agg_png(RASTER_TMP, width = FIG_W_CM, height = FIG_H_CM, units = "cm", res = 600,
              background = "transparent", scaling = 1)
grid::grid.newpage()
grid::grid.draw(mask_cloud_only(G))
invisible(dev.off())
RASTER_BYTES <- file.info(RASTER_TMP)$size
pts_raster <- png::readPNG(RASTER_TMP, native = FALSE)
message("[render] PDF composite ...")
# The PDF page is set from the RASTER's exact pixel count, not from the nominal
# centimetre budget, so the two deliverables describe the same page instead of
# differing by the fraction of a point that ragg rounds away.
w_in <- PNG_W_PX / 600
h_in <- PNG_H_PX / 600
pdf_ok <- TRUE
pdf_dev <- "none"
tryCatch({
  if (isTRUE(capabilities("cairo"))) {
    pdf_dev <- "cairo_pdf"
    grDevices::cairo_pdf(PDF_PATH, width = w_in, height = h_in, onefile = FALSE,
                         bg = "white", family = FAM, fallback_resolution = 600)
  } else {
    # The classic pdf device cannot encode U+0394 under an mbcs to sbcs
    # conversion, so every strip would silently lose its swing symbol, which is
    # the figure's entire magnitude defence. Fail loudly rather than ship that.
    pdf_dev <- "pdf"
    message("[pdf] WARNING: cairo is unavailable. grDevices::pdf() cannot encode the swing symbol U+0394 and every inner strip would lose it. Install a cairo-enabled R or take the PNG as the deliverable.")
    grDevices::pdf(PDF_PATH, width = w_in, height = h_in, onefile = FALSE,
                   bg = "white", family = "Times")
  }
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  grid::grid.raster(pts_raster, x = 0.5, y = 0.5, width = 1, height = 1, interpolate = FALSE)
  grid::grid.draw(mask_no_cloud(G))
  invisible(dev.off())
}, error = function(e) {
  pdf_ok <<- FALSE
  message(sprintf("[pdf] composite failed: %s", conditionMessage(e)))
  while (dev.cur() > 1) invisible(dev.off())
})
PDF_BYTES <- if (pdf_ok && file.exists(PDF_PATH)) file.info(PDF_PATH)$size else NA_real_
PDF_SANE <- is.finite(PDF_BYTES) && PDF_BYTES < 20 * 1024^2
# A PDF page is expressed in whole points, so the realised page is quoted rather
# than the requested one. Production typesets the PDF, so its size is the size
# the caption states.
PDF_W_PT <- round(w_in * 72)
PDF_H_PT <- round(h_in * 72)
PDF_W_CM <- PDF_W_PT / 72 * 2.54
PDF_H_CM <- PDF_H_PT / 72 * 2.54
if (file.exists(RASTER_TMP)) unlink(RASTER_TMP)
# -----------------------------------------------------------------------------
# 20. The TWO text deliverables. Nothing here is burned into the image and every
#     number is injected with sprintf from a value computed above, so no printed
#     statement can drift from the data that produced the figure.
#     LEGEND: the short block typeset under the artwork. It states each thing ONCE
#     and is checked at runtime against a journal-length ceiling.
#     EAPPENDIX: the supplementary methods note. Everything that does not fit the
#     legend lives here under a numbered heading, and each fact appears in exactly
#     one of the two files rather than in both.
# -----------------------------------------------------------------------------
# English ordinal suffix, so a computed percentile never prints as "1th".
ordinal_en <- function(n) {
  n <- as.integer(round(n))
  suf <- if (n %% 100 %in% c(11L, 12L, 13L)) "th" else switch(as.character(n %% 10),
    "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suf)
}
clip_mean <- mean(clip_report$pct_dropped)
clip_min <- min(clip_report$pct_dropped)
clip_max <- max(clip_report$pct_dropped)
# The maximum loss is an exact two-way tie across the two outcomes. Naming one
# outcome would be arbitrary row order presented as a fact, so ties are detected
# and the sentence names every outcome that attains the maximum.
clip_worst_rows <- clip_report[abs(clip_report$pct_dropped - clip_max) < 1e-9, ]
clip_worst <- clip_worst_rows[1, ]
CLIP_WORST_OUT <- if (nrow(clip_worst_rows) > 1L &&
                      length(unique(clip_worst_rows$Predictor)) == 1L &&
                      length(unique(clip_worst_rows$lv)) == 1L) {
  "both outcomes"
} else {
  unname(OUT_DISPLAY[[clip_worst$Outcome]])
}
gloss_line <- function(v) {
  gl <- ORD_GLOSS[[v]]
  tk <- ORD_TICKS[[v]]
  ord_txt <- if (v %in% ORDERED_FAMILIES) "ordered" else "NOT ordered, so its level medians are not joined by a line"
  sprintf("%s [%s]. %s.", PRED_LAB[[v]], ord_txt,
          paste(sprintf("%d (%s) = %s", seq_along(gl) - 1L, tk, gl), collapse = "; "))
}
# Bin sizes are quoted from the REALISED counts. The nominal n divided by the bin
# count is never an observed value: ties at repeated predictor values prevent
# exact equality, and for the communal poverty rate the bins differ by 24 percent.
cont_line <- function(v) {
  m <- cont_meta[cont_meta$Predictor == v & cont_meta$Outcome == "Readmission", ]
  sn <- swing_sens[swing_sens$Predictor == v & swing_sens$Outcome == "Readmission", ]
  sn <- sn[order(sn$requested_groups), ]
  sprintf(paste0("%s. Data span %s to %s across %s distinct values. %d approximately equal-count quantile bins, ",
                 "%s to %s observations per bin (never exactly equal, because ties at repeated values prevent it), ",
                 "and %s to %s distinct predictor values per bin (median %s). The top bin runs %s to %s and holds ",
                 "%s observations, drawn as the whisker at the right-hand end. Bin-count sensitivity of the readmission ",
                 "swing: %s."),
          PRED_LAB[[v]], fmt_x(v, m$x_data_min), fmt_x(v, m$x_data_max),
          format(m$n_distinct_x, big.mark = ","), m$n_bins,
          format(m$n_bin_min, big.mark = ","), format(m$n_bin_max, big.mark = ","),
          format(m$distinct_per_bin_min, big.mark = ","), format(m$distinct_per_bin_max, big.mark = ","),
          format(round(m$distinct_per_bin_med), big.mark = ","),
          fmt_x(v, m$top_bin_lo), fmt_x(v, m$top_bin_hi), format(m$top_bin_n, big.mark = ","),
          paste(sprintf("%d groups %.4f", sn$n_groups, sn$swing), collapse = "; "))
}
# Largest and smallest are reported WITHIN each cell type, never pooled: the two
# types measure the swing on different supports and a pooled extreme would be an
# artifact of the group count rather than a fact about the predictors.
cont_cells <- cell_key[cell_key$is_cont, ]
top_swing_ord <- ord_cells[order(-ord_cells$swing), ][1, ]
low_swing_ord <- ord_cells[order(ord_cells$swing), ][1, ]
top_swing_cont <- cont_cells[order(-cont_cells$swing), ][1, ]
low_swing_cont <- cont_cells[order(cont_cells$swing), ][1, ]
# ---- LEGEND -----------------------------------------------------------------
# The journal legend. Target 170 to 220 words, hard ceiling 260, because LWW sets
# the legend under the artwork and a 41-line block is unusable there. It carries
# only: the title sentence, what a cell is, what the printed swing is together with
# the one warning that matters most, one sentence of geometry per cell type, the
# not-a-confidence-interval sentence, the zero rule, a pointer to the eAppendix and
# the abbreviations. Nothing is said twice, and everything it drops is in EAPP_PATH.
legend_txt <- c(
  "FIGURE. Functional form of each predictor's SHAP contribution to the log hazard for readmission to substance use disorder treatment and for post-discharge all-cause mortality.",
  "",
  sprintf(paste0(
    "Each of the %d cells shows one predictor for one outcome; the bold heading is merged across a predictor's two cells. ",
    "The lighter heading gives the outcome and that cell's %s, the peak-to-trough of the plotted median in log-hazard units, ",
    "over %s quantile groups in continuous cells and %s levels in ordinal cells; the two supports are not comparable. ",
    "Every cell has its own vertical scale, so compare the printed %s and never the drawn height. ",
    "Continuous predictors are drawn on natural units as approximately equal-count quantile bins: heavy line, within-bin median; band, within-bin interquartile range; whiskers, the end bins. ",
    "Ordinal predictors are density-scaled clouds with a violin outline, an open circle at the level median and a bar over the interquartile range; ",
    "percentages beneath the levels are the true share of observations there, printed once per predictor in the %s cell and applying to both. ",
    "Bands and bars show cross-sectional spread of individual SHAP values from interactions, not confidence intervals. ",
    "The dotted rule marks a zero contribution. ",
    "Full methodological detail, including bin-count sensitivity, clipping, level glosses and prevalence, is given in eAppendix N."),
    nrow(cell_key), DELTA_GLYPH, CONT_SUPPORT_TXT, ORD_SUPPORT_TXT, DELTA_GLYPH,
    tolower(unname(OUT_DISPLAY[[FOOT_OUTCOME]]))),
  "",
  "Abbreviations: SHAP, Shapley additive explanations; HS, high school; wk, week."
)
writeLines(legend_txt, LEGEND_PATH, useBytes = FALSE)
# Word count of what was actually written, so the length claim is measured on the
# delivered file rather than estimated while drafting it.
LEGEND_WORDS <- sum(nzchar(unlist(strsplit(paste(legend_txt, collapse = " "), "[[:space:]]+"))))
LEGEND_WORD_CAP <- 260L
if (LEGEND_WORDS > LEGEND_WORD_CAP) {
  message(sprintf("[legend] WARNING: legend is %d words, above the %d word ceiling a journal legend can carry.",
                  LEGEND_WORDS, LEGEND_WORD_CAP))
}
# ---- EAPPENDIX --------------------------------------------------------------
# The supplementary methods note. Eight numbered sections, each holding the facts
# the legend cannot carry. Nothing that used to be in the single long caption is
# dropped: if it is not in the legend above, it is under one of these headings.
eapp_txt <- c(
  "eAPPENDIX N. Supplementary methods for the functional-form figure.",
  "",
  "1. DATA SOURCE AND UNIT OF ANALYSIS",
  sprintf(paste0("Source data: %s. Values are SHAP contributions to the MODEL MARGIN of the gradient boosting models (scope: %s). ",
                 "A Cox-objective gradient boosting model produces a single time-invariant linear predictor, so these contributions are not indexed by prediction time; ",
                 "the scope label distinguishes this export from the multi-horizon one rather than denoting an average over horizons. ",
                 "Each predictor and outcome contributes %s rows, one per observation in the sample on which the SHAP values were computed. ",
                 "The export does not label those rows as persons or as treatment episodes, so the unit of analysis must be taken from the methods rather than from this figure. ",
                 "Each observation's predictor value and SHAP value are means over the cross-validation repetitions in which that observation fell in a validation fold, ",
                 "which is why a small fraction of otherwise integer-valued predictors take non-integer values. ",
                 "Values are interpretable on the log-hazard scale but remain descriptive and predictive, not causal."),
          basename(INPUT), SCOPE_TXT, format(N_PER_CELL, big.mark = ",")),
  "",
  "2. VERTICAL SCALES AND WHY THEY ARE FREE",
  sprintf(paste0("Every cell carries its own vertical scale and no cell uses a power-of-ten multiplier. A shared vertical scale between outcomes is not usable here: ",
                 "within a predictor, where both outcomes share one support, the ratio of the mortality swing to the readmission swing spans %.2f (%s) to %.0f (%s), ",
                 "so any common scale renders one outcome as a flat line. THE PRICE OF THE FREE SCALE IS THAT PANEL HEIGHT MUST NOT BE COMPARED ACROSS CELLS. ",
                 "Of the %d pairs of ordinal cells, %d (%.1f%%) are drawn with their apparent amplitudes inverted relative to their true swings, by up to a factor of %.1f; ",
                 "across all %d pairs the rate is %.1f%%. The worst ordinal case is %s. ",
                 "A Spearman correlation between apparent amplitude and the true swing (%.2f across all cells, %.2f across the ordinal cells) is NOT evidence to the contrary, ",
                 "because apparent amplitude is the swing divided by the cell's own plotted range and so shares its numerator with the swing; ",
                 "permuting the plotted ranges across cells, which destroys any real relation, already places those observed values at the %s and %s percentiles of their own null distributions. ",
                 "The vertical extent of a cell is also defined differently by type: a continuous cell is scaled to the interquartile band, a 50%% interval, ",
                 "while an ordinal cell is scaled to a 98%% point cloud, so cell heights are comparable within a type and not between types."),
          RATIO_MIN, PRED_LAB[[RATIO_MIN_VAR]], RATIO_MAX, PRED_LAB[[RATIO_MAX_VAR]],
          DISC_ORD$n_pairs, DISC_ORD$n_disc, DISC_ORD$pct, DISC_ORD$worst,
          DISC_ALL$n_pairs, DISC_ALL$pct, DISC_ORD_WORST_TXT,
          RHO_ALL, RHO_ORD, ordinal_en(RHO_ALL_PCTL), ordinal_en(RHO_ORD_PCTL)),
  "",
  "3. THE SWING STATISTIC AND ITS BIN-COUNT SENSITIVITY",
  sprintf(paste0("The swing printed in each inner strip is the peak-to-trough difference of the plotted MEDIAN, not of the mean. ",
                 "The mean-based version of the same statistic gives a mortality-to-readmission ratio spanning %.3f to %.0f instead of %.3f to %.0f, ",
                 "so whichever definition the manuscript quotes must be the one stated here. ",
                 "The continuous and the ordinal swing are two different estimands: a peak-to-trough is an extreme order statistic, so it grows with the number of groups, ",
                 "and the continuous version is taken over %s quantile groups against the %s natural levels of an ordinal family. ",
                 "For age at admission and mortality the swing takes the values %s on %s quantile groups respectively. ",
                 "On a like-for-like %d-group grid, the continuous swings are %s, which is the only footing on which they may be compared with an ordinal swing. ",
                 "Within each cell type, the largest ordinal swing is %s for %s (%s) and the smallest is %s for %s (%s); ",
                 "among the continuous cells the largest is %s for %s (%s) and the smallest is %s for %s (%s). ",
                 "Largest and smallest are reported WITHIN a type and never pooled, because a pooled extreme would be an artifact of the group count rather than a fact about the predictors."),
          RATIO_MEAN_MIN, RATIO_MEAN_MAX, RATIO_MIN, RATIO_MAX,
          CONT_SUPPORT_TXT, ORD_SUPPORT_TXT,
          paste(sprintf("%.4f", swing_sens$swing[swing_sens$Predictor == "adm_age_rec3" &
                                                   swing_sens$Outcome == "Death"]), collapse = ", "),
          paste(sprintf("%d", swing_sens$n_groups[swing_sens$Predictor == "adm_age_rec3" &
                                                    swing_sens$Outcome == "Death"]), collapse = ", "),
          ORD_MEDIAN_LEVELS,
          paste(sprintf("%s (%s) %.4f", PRED_LAB[swing_matched$Predictor],
                        OUT_DISPLAY[swing_matched$Outcome], swing_matched$swing), collapse = "; "),
          fmt_swing(top_swing_ord$swing), PRED_LAB[[top_swing_ord$Predictor]], OUT_DISPLAY[[top_swing_ord$Outcome]],
          fmt_swing(low_swing_ord$swing), PRED_LAB[[low_swing_ord$Predictor]], OUT_DISPLAY[[low_swing_ord$Outcome]],
          fmt_swing(top_swing_cont$swing), PRED_LAB[[top_swing_cont$Predictor]], OUT_DISPLAY[[top_swing_cont$Outcome]],
          fmt_swing(low_swing_cont$swing), PRED_LAB[[low_swing_cont$Predictor]], OUT_DISPLAY[[low_swing_cont$Outcome]]),
  "",
  "4. CONTINUOUS PREDICTORS",
  sprintf(paste0("Continuous predictors occupy the first %d cells, shown on NATURAL units as approximately equal-count quantile bins of the predictor itself, ",
                 "with a target of %d bins and never more bins than distinct values. The horizontal axis runs over the FULL OBSERVED RANGE of the predictor. ",
                 "The first and last quantile bins are wide, so each is drawn as a horizontal whisker spanning its own observed extent with a hollow circle at its median, ",
                 "and the foot of the %s cell names the top bin's interval, which applies to both cells of that predictor because the quantile breaks are cut on the predictor alone: ",
                 "the whisker states that the endpoint summarises that whole interval rather than a point. ",
                 "For treatment episode length the top bin runs %s to %s months and holds %s of %s observations (%.1f%%), which is %.0f%% of the natural range compressed into one summary. ",
                 "Rank or empirical distribution function axes were deliberately avoided because they compress the upper tail of a skewed predictor into a sliver of axis width: ",
                 "on such an axis that same top bin would occupy about %.0f%% of the axis. ",
                 "Communal poverty is a COMMUNE-level variable with only %s distinct values, so although a bin contains %s to %s observations it holds a median of only about %d distinct commune values, ",
                 "and the bin-to-bin excursions in those two cells reflect commune clustering more than a dose-response in poverty."),
          2 * length(CONT_VARS), TARGET_BINS, tolower(unname(OUT_DISPLAY[[FOOT_OUTCOME]])),
          fmt_x("dit_m", cont_meta$top_bin_lo[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"]),
          fmt_x("dit_m", cont_meta$top_bin_hi[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"]),
          format(cont_meta$top_bin_n[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"], big.mark = ","),
          format(N_PER_CELL, big.mark = ","),
          100 * cont_meta$top_bin_n[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"] / N_PER_CELL,
          100 * (cont_meta$top_bin_hi[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"] -
                   cont_meta$top_bin_lo[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"]) /
            (cont_meta$x_data_max[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"] -
               cont_meta$x_data_min[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"]),
          100 / cont_meta$n_bins[cont_meta$Predictor == "dit_m" & cont_meta$Outcome == "Readmission"],
          format(cont_meta$n_distinct_x[cont_meta$Predictor == "porc_pobr" & cont_meta$Outcome == "Readmission"], big.mark = ","),
          format(cont_meta$n_bin_min[cont_meta$Predictor == "porc_pobr" & cont_meta$Outcome == "Readmission"], big.mark = ","),
          format(cont_meta$n_bin_max[cont_meta$Predictor == "porc_pobr" & cont_meta$Outcome == "Readmission"], big.mark = ","),
          round(cont_meta$distinct_per_bin_med[cont_meta$Predictor == "porc_pobr" & cont_meta$Outcome == "Readmission"])),
  "Realised binning, per continuous predictor:",
  paste0("  ", vapply(cont_order, cont_line, character(1))),
  "",
  "5. ORDINAL PREDICTORS",
  sprintf(paste0("Ordinal predictors are integer-valued and are shown as clouds at their levels, never as smooth curves. ",
                 "Non-integer values in the export are cross-validation fold-averaging artifacts rather than real categories: ",
                 "%.2f%% of ordinal rows are already exact integers and the remainder were rounded to the nearest level. ",
                 "Grey clouds are density-scaled (sina) jitter with a half width of %.2f of the level spacing, drawn from an equal-n subsample of at most %s observations per level under a fixed seed, ",
                 "so cloud density carries distributional shape and not prevalence. The unfilled outline is a width-scaled violin estimated on up to %s observations per level, ",
                 "and the cloud is a subsample of exactly those observations, so the outline and the speckle share one sample and one bandwidth. ",
                 "%s points are drawn across the %d ordinal level-cells after equal-n subsampling. ",
                 "Open circles are the level medians and the dark vertical bars are interquartile ranges, both computed on ALL observations. ",
                 "The medians are joined by a line only for the %d families whose levels are genuinely ordered; %s is a nominal classification whose level 2 is a residual category, ",
                 "so its medians are shown as unconnected circles. Even among the ordered families the level medians are not always monotone: ",
                 "%d of the %d ordinal cells reverse direction at least once (%s). ",
                 "The small number printed beneath each level is the TRUE percentage of observations at that level. ",
                 "It is stated in text rather than encoded as ink because equal-n subsampling deliberately removes prevalence from the cloud, ",
                 "and scaling violin width by prevalence would reduce rare levels to slivers that read as error bars. ",
                 "It is printed in the %s cell only: the level share is a property of the predictor and both outcomes carry identical predictor values, which is asserted at runtime, ",
                 "so a second copy in the mortality cell added twelve redundant rows of the smallest type on the page without adding information."),
          EXACT_INT_SHARE, SINA_HALF_WIDTH, format(SINA_CAP, big.mark = ","), format(VIOLIN_CAP, big.mark = ","),
          format(N_PLOTTED_POINTS, big.mark = ","), N_LEVEL_CELLS,
          length(ORDERED_FAMILIES),
          paste(unname(PRED_LAB[NOMINAL_FAMILIES]), collapse = " and "),
          N_NONMONO, nrow(ord_cells),
          paste(sprintf("%s, %s", PRED_LAB[NONMONO_CELLS$Predictor], OUT_DISPLAY[NONMONO_CELLS$Outcome]), collapse = "; "),
          tolower(unname(OUT_DISPLAY[[FOOT_OUTCOME]]))),
  "",
  "6. CLIPPING AND ITS COST IN VISIBLE RANGE",
  sprintf(paste0("For display only, the cloud and the violin outline were clipped WITHIN each level at the %s and %s percentiles of that level's own SHAP distribution, ",
                 "removing between %.2f%% and %.2f%% of points per level (mean %.2f%%; largest loss %.2f%% at level %d of %s, for %s). ",
                 "That small loss of POINTS is nonetheless a large loss of visible RANGE, because the discarded points are by construction the extremes ",
                 "and the cell's vertical limits are then set from what survives: the displayed vertical range is between %.0f%% and %.0f%% of the full observed SHAP span, ",
                 "so the visible spread is a 98%% interval and not the full support. ",
                 "Clipping within levels rather than by a panel-wide percentile is what keeps the loss from falling disproportionately on rare levels. ",
                 "Medians and interquartile ranges are computed on the unclipped data and are unaffected, because the quartiles lie strictly inside the clipping window."),
          ordinal_en(CLIP_LO * 100), ordinal_en(CLIP_HI * 100), clip_min, clip_max, clip_mean,
          clip_worst$pct_dropped, clip_worst$lv, PRED_LAB[[clip_worst$Predictor]], CLIP_WORST_OUT,
          SPAN_SHOWN_MIN, SPAN_SHOWN_MAX),
  "",
  "7. INTERPRETATION OF THE BANDS AND THE BARS",
  sprintf(paste0("The shaded bands and the interquartile bars show the CROSS-SECTIONAL SPREAD of individual SHAP values at a given predictor value, ",
                 "which arises from interactions with the other covariates. They carry no information about uncertainty in the median. ",
                 "Read as intervals they would understate precision by a large factor: across the %d ordinal level-cells the interquartile bar is %.0f to %.0f times wider ",
                 "than a distribution-free 95%% interval for the same median (median ratio %.0f). ",
                 "A circle with a bar is the visual idiom of a point estimate with an interval, so the warning is carried in the figure legend. ",
                 "It is not repeated inside the artwork: the %d unused slots of the %d by %d grid are left BLANK, ",
                 "because an in-image key restated the legend word for word and spent the only white space on a page holding %d cells."),
          nrow(iqr_vs_ci), IQR_CI_MIN, IQR_CI_MAX, IQR_CI_MED,
          FREE_SLOTS, N_ROW, N_COL, nrow(cell_key)),
  "",
  "8. ORDERING, LEVEL GLOSSES, AND RENDERING SPECIFICATIONS",
  sprintf(paste0("Ordering: the %d continuous predictors appear first, then the %d ordinal predictors in descending order of their maximum median swing across either outcome. ",
                 "Plotted order: %s. ",
                 "The dotted horizontal rule marking a zero SHAP contribution is drawn on top of the data layers, so a flat curve cannot occlude it."),
          length(CONT_VARS), length(ORD_VARS), paste(unname(PRED_LAB[PRED_ORDER]), collapse = "; ")),
  "Ordinal levels, left to right on each axis (tick abbreviation in parentheses):",
  paste0("  ", vapply(ord_order, gloss_line, character(1))),
  sprintf(paste0("Rendering: the raster is %.2f cm wide by %.2f cm tall (%d by %d pixels at %d dpi) and the companion PDF page is %.2f by %.2f cm (%d by %d pt), ",
                 "the difference being that a PDF page is expressed in whole points; production typesets the PDF. ",
                 "Each cell measures %.2f by %.2f cm and the smallest type is %.1f pt (measured ink height %.2f pt, %.3f mm), ",
                 "which is below the 8 pt that some production guidelines request and is stated here so it can be raised at proof stage if required. ",
                 "The PDF carries the point clouds as an embedded %d dpi raster and everything else as vector artwork. ",
                 "Type is set in %s and the figure is achromatic throughout, so a grayscale proof is identical to the colour proof. ",
                 "Additional abbreviations used in this note: IQR, interquartile range; CV, cross-validation; Interm., intermediate; Minim., minimum."),
          PNG_W_CM, PNG_H_CM, PNG_W_PX, PNG_H_PX, 600L,
          PDF_W_CM, PDF_H_CM, PDF_W_PT, PDF_H_PT,
          CELL_W_CM, CELL_H_CM, SMALLEST_PT, SMALLEST_INK_PT, SMALLEST_INK_MM, 600L, FAM)
)
writeLines(eapp_txt, EAPP_PATH, useBytes = FALSE)
# -----------------------------------------------------------------------------
# 21. Machine-readable summary to stdout.
# -----------------------------------------------------------------------------
kv <- function(k, v) cat(sprintf("%-30s %s\n", k, v))
cat("\n================ FIGURE BUILD SUMMARY ================\n")
kv("input_file", INPUT)
kv("input_rows", format(nrow(dat), big.mark = ","))
kv("rows_per_predictor_outcome", format(N_PER_CELL, big.mark = ","))
kv("feature_value_max_abs_diff", sprintf("%.3g", FV_MAX_DIFF))
kv("png_path", PNG_PATH)
kv("png_bytes", format(PNG_BYTES, big.mark = ","))
kv("png_width_px", PNG_W_PX)
kv("png_height_px", PNG_H_PX)
kv("png_width_cm", sprintf("%.4f", PNG_W_CM))
kv("png_height_cm", sprintf("%.4f", PNG_H_CM))
kv("png_dpi", 600L)
kv("height_limit_24cm_ok", as.character(PNG_H_CM <= 24.0 + 1e-6))
kv("width_17p8cm_ok", as.character(abs(PNG_W_CM - 17.8) < 0.01))
kv("pdf_path", if (pdf_ok) PDF_PATH else "FAILED")
kv("pdf_device", pdf_dev)
kv("pdf_bytes", if (is.finite(PDF_BYTES)) format(PDF_BYTES, big.mark = ",") else "NA")
kv("pdf_mb", if (is.finite(PDF_BYTES)) sprintf("%.2f", PDF_BYTES / 1024^2) else "NA")
kv("pdf_under_20mb", as.character(PDF_SANE))
kv("raster_layer_bytes", format(RASTER_BYTES, big.mark = ","))
kv("legend_path", LEGEND_PATH)
kv("legend_bytes", format(file.info(LEGEND_PATH)$size, big.mark = ","))
kv("legend_words", sprintf("%d (ceiling %d)", LEGEND_WORDS, LEGEND_WORD_CAP))
kv("legend_within_ceiling", as.character(LEGEND_WORDS <= LEGEND_WORD_CAP))
kv("eappendix_path", EAPP_PATH)
kv("eappendix_bytes", format(file.info(EAPP_PATH)$size, big.mark = ","))
kv("old_caption_removed", as.character(!file.exists(OLD_CAP_PATH)))
kv("grid_rows_x_cols", sprintf("%d x %d", N_ROW, N_COL))
kv("n_cells", nrow(cell_key))
kv("blank_slots", N_ROW * N_COL - nrow(cell_key))
kv("cell_width_cm", sprintf("%.4f", CELL_W_CM))
kv("cell_height_cm", sprintf("%.4f", CELL_H_CM))
kv("outer_strip_span_cm", sprintf("%.3f", OUTER_SPAN_CM))
kv("longest_outer_label_cm", sprintf("%.3f", OUTER_LAB_CM))
kv("outer_label_fits", as.character(OUTER_FITS))
kv("longest_inner_label_cm", sprintf("%.3f", INNER_LAB_CM))
kv("inner_label_fits", as.character(INNER_FITS))
kv("n_outer_strips", N_OUTER_STRIPS)
kv("n_inner_strips", N_INNER_STRIPS)
kv("n_cloud_grobs_rasterized", N_CLOUD_GROBS)
kv("font_family", FAM)
kv("smallest_type_pt_nominal", sprintf("%.2f", SMALLEST_PT))
kv("smallest_type_ink_pt", sprintf("%.2f", SMALLEST_INK_PT))
kv("smallest_type_ink_px_600dpi", sprintf("%.1f", SMALLEST_INK_PX))
kv("smallest_type_ink_mm", sprintf("%.3f", SMALLEST_INK_MM))
kv("smallest_type_measure_method", GLYPH$method)
kv("smallest_type_ink_width_pt", sprintf("%.2f", GLYPH$ink_w_pt))
kv("exact_integer_share_pct", sprintf("%.4f", EXACT_INT_SHARE))
kv("sina_half_width", sprintf("%.2f", SINA_HALF_WIDTH))
kv("sina_cap_per_level", SINA_CAP)
kv("violin_cap_per_level", VIOLIN_CAP)
kv("points_plotted", format(N_PLOTTED_POINTS, big.mark = ","))
kv("level_cells", N_LEVEL_CELLS)
kv("clip_rule", sprintf("per level, %.0f to %.0f pct", CLIP_LO * 100, CLIP_HI * 100))
kv("clip_pct_dropped_mean", sprintf("%.3f", clip_mean))
kv("clip_pct_dropped_range", sprintf("%.3f to %.3f", clip_min, clip_max))
kv("axis_y_labels_all_unique", as.character(AXIS_ALL_UNIQUE))
kv("axis_y_max_label_error", sprintf("%.3g", AXIS_MAX_ERR))
kv("axis_y_min_breaks_per_cell", AXIS_MIN_BREAKS)
kv("axis_y_zero_labelled_in_range", as.character(AXIS_ZERO_OK))
kv("axis_x_labels_all_unique", as.character(AXIS_X_ALL_UNIQUE))
kv("axis_x_max_label_error", sprintf("%.3g", AXIS_X_MAX_ERR))
kv("axis_x_min_breaks_per_cell", AXIS_X_MIN_BREAKS)
kv("axis_x_widest_label_row_cm", sprintf("%.3f", AXIS_X_WIDEST_CM))
kv("spearman_apparent_vs_swing", sprintf("all=%.3f (null pctl %.0f) ordinal=%.3f (null pctl %.0f)",
                                         RHO_ALL, RHO_ALL_PCTL, RHO_ORD, RHO_ORD_PCTL))
kv("discordant_pairs_all", sprintf("%d of %d (%.1f%%), worst factor %.2f",
                                   DISC_ALL$n_disc, DISC_ALL$n_pairs, DISC_ALL$pct, DISC_ALL$worst))
kv("discordant_pairs_ordinal", sprintf("%d of %d (%.1f%%), worst factor %.2f",
                                       DISC_ORD$n_disc, DISC_ORD$n_pairs, DISC_ORD$pct, DISC_ORD$worst))
kv("death_readm_swing_ratio_median", sprintf("%.3f to %.1f", RATIO_MIN, RATIO_MAX))
kv("death_readm_swing_ratio_mean", sprintf("%.3f to %.1f", RATIO_MEAN_MIN, RATIO_MEAN_MAX))
kv("iqr_over_median_ci_ratio", sprintf("%.1f to %.1f (median %.1f)", IQR_CI_MIN, IQR_CI_MAX, IQR_CI_MED))
kv("ordinal_span_shown_pct", sprintf("%.1f to %.1f", SPAN_SHOWN_MIN, SPAN_SHOWN_MAX))
kv("nonmonotone_ordinal_cells", sprintf("%d of %d", N_NONMONO, nrow(ord_cells)))
kv("line_drawn_for_families", length(ORDERED_FAMILIES))
kv("nominal_families_no_line", paste(NOMINAL_FAMILIES, collapse = ", "))
kv("violin_cells_hitting_cap", sprintf("%d of %d", N_CELLS_CAPPED, N_LEVEL_CELLS))
kv("cloud_violin_share_sample", as.character(BW_SHARED_OK))
kv("block_gutter_cm", if (is.null(BLOCK_GAP_CM)) "NA" else sprintf("%.3f", BLOCK_GAP_CM))
kv("within_block_gutter_cm", if (is.null(WITHIN_GAP_CM)) "NA" else sprintf("%.3f", WITHIN_GAP_CM))
kv("free_slots_left_blank", FREE_SLOTS)
kv("in_image_key", "removed; every clause now lives in the legend file")
kv("foot_rows_printed_in", unname(OUT_DISPLAY[[FOOT_OUTCOME]]))
kv("inner_strip_example", cell_key$out_lab_txt[1])
kv("pdf_page_cm", sprintf("%.4f x %.4f", PDF_W_CM, PDF_H_CM))
kv("pdf_page_pt", sprintf("%d x %d", PDF_W_PT, PDF_H_PT))
kv("elapsed_sec", sprintf("%.1f", as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
cat("\n---- EXACT-INTEGER SHARE BY ORDINAL PREDICTOR (pct) ----\n")
print(as.data.frame(exact_by_pred), row.names = FALSE, digits = 5)
cat("\n---- PER-LEVEL CLIP COUNTS ----\n")
cr <- clip_report
cr$Predictor <- unname(PRED_LAB[cr$Predictor])
cr$Outcome <- unname(OUT_DISPLAY[cr$Outcome])
print(as.data.frame(cr), row.names = FALSE, digits = 4)
cat("\n---- TRUE LEVEL SHARES (pct) ----\n")
ls2 <- level_share
ls2$Predictor <- unname(PRED_LAB[ls2$Predictor])
print(as.data.frame(ls2), row.names = FALSE, digits = 4)
cat("\n---- PER-CELL SWING TABLE (log hazard, peak-to-trough of plotted median) ----\n")
st <- cell_key[, c("pred_rank", "Predictor", "Outcome", "is_cont", "swing", "ymin", "ymax", "y_span", "apparent_frac")]
st$Predictor <- unname(PRED_LAB[st$Predictor])
st$Outcome <- unname(OUT_DISPLAY[st$Outcome])
st$type <- ifelse(st$is_cont, "continuous", "ordinal")
st$is_cont <- NULL
st$swing_rank <- rank(-st$swing, ties.method = "min")
st$apparent_rank <- rank(-st$apparent_frac, ties.method = "min")
print(as.data.frame(st), row.names = FALSE, digits = 4)
cat("\n---- PER-CELL VERTICAL AXIS AUDIT ----\n")
print(axis_audit, row.names = FALSE, digits = 4)
cat("\n---- CONTINUOUS SWING BY BIN COUNT (the estimand is bin-count dependent) ----\n")
ss <- swing_sens
ss$Predictor <- unname(PRED_LAB[ss$Predictor])
ss$Outcome <- unname(OUT_DISPLAY[ss$Outcome])
print(as.data.frame(ss), row.names = FALSE, digits = 4)
cat("\n---- ORDINAL DISPLAYED SPAN AS PCT OF FULL SHAP SPAN ----\n")
sk <- as.data.frame(span_keep)
sk$Predictor <- unname(PRED_LAB[sk$Predictor])
sk$Outcome <- unname(OUT_DISPLAY[sk$Outcome])
print(sk, row.names = FALSE, digits = 4)
cat("\n---- CONTINUOUS PLOTTED X RANGES ----\n")
cm2 <- cont_meta
cm2$Predictor <- unname(PRED_LAB[cm2$Predictor])
cm2$Outcome <- unname(OUT_DISPLAY[cm2$Outcome])
print(as.data.frame(cm2), row.names = FALSE, digits = 5)
cat("\n---- TYPE SIZES USED (pt) ----\n")
print(SIZES_USED, digits = 4)
cat("\n================ END SUMMARY ================\n")
