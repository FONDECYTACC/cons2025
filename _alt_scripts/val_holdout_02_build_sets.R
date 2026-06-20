# =============================================================================
# val_holdout_02_build_sets.R
# Build the 80% TRAIN and 20% VALIDATION analysis datasets (dummified) for the
# held-out (internal) validation of prediction23, from the reproducible parquet
# layer + the canonical seed-2125 split.
#
# Inputs (all in data/20241015_out/):
#   corrected_datasets_nondum_filt_imp{1..5}.parquet  (5 imputations, 88152x43)
#   _val_inputs/formulas.rds                          (4 frozen formulas)
#   _val_inputs/py_corrected_datasets.rds             (authoritative TRAIN, verify)
# Split (user-specified):
#   cons/_out/comb_split_seed2125_test20_mar26.parquet  (row_id, is_train, ...)
#
# Preprocessing reproduces exactly the pipeline that produced
# py_corrected_datasets (see project CLAUDE.md), generalized so TRAIN and VAL are
# encoded with IDENTICAL dummy columns and reference levels.
#
# Public API:
#   build_holdout_datasets(force=FALSE, verify=TRUE, verbose=TRUE)
#     -> list(train = <list of 5>, val = <list of 5>, checks = <data.frame>,
#             cols = <chr>, n_train, n_val)
#   Cached to data/20241015_out/_val_inputs/holdout_train_val.rds
# =============================================================================

suppressWarnings({
  stopifnot(requireNamespace("nanoparquet", quietly = TRUE))
  stopifnot(requireNamespace("dplyr",       quietly = TRUE))
  stopifnot(requireNamespace("janitor",     quietly = TRUE))
  stopifnot(requireNamespace("purrr",       quietly = TRUE))
})

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

# ---- Paths (all anchored to project_root) ------------------------------------
.VH_DATA_DIR      <- file.path(project_root, "data/20241015_out")
.VH_VAL_INPUTS    <- file.path(.VH_DATA_DIR, "_val_inputs")
.VH_SPLIT_PARQUET <- file.path(project_root, "cons/_out/comb_split_seed2125_test20_mar26.parquet")
.VH_CACHE         <- file.path(.VH_VAL_INPUTS, "holdout_train_val.rds")

# ---- Column contract (verbatim from CLAUDE.md) -------------------------------
.vh_meta_cols <- c(
  "readmit_event", "death_event",
  "readmit_time_from_disch_m", "death_time_from_disch_m",
  "center_id", "row_id"
)
.vh_cox_columns <- c(
  "adm_age_rec3", "porc_pobr", "dit_m", "national_foreign",
  "ethnicity", "dg_psiq_cie_10_instudy", "dg_psiq_cie_10_dg",
  "dx_f3_mood", "dx_f6_personality", "dx_f_any_severe_mental",
  "any_phys_dx", "polysubstance_strict",
  "sex_rec_woman", "cohabitation_family_of_origin",
  "cohabitation_with_couple_children", "cohabitation_others",
  "sub_dep_icd10_status_drug_dependence",
  "any_violence_1_domestic_violence_sex_abuse", "tr_outcome_referral",
  "tr_outcome_dropout", "tr_outcome_adm_discharge_rule_violation_undet",
  "tr_outcome_adm_discharge_adm_reasons",
  "adm_motive_sanitary_sector", "adm_motive_another_sud_facility_fonodrogas_senda_previene",
  "adm_motive_justice_sector", "adm_motive_other", "first_sub_used_alcohol",
  "first_sub_used_cocaine_paste", "first_sub_used_cocaine_powder", "first_sub_used_other",
  "primary_sub_mod_cocaine_paste", "primary_sub_mod_cocaine_powder",
  "primary_sub_mod_alcohol", "primary_sub_mod_others",
  "tipo_de_vivienda_rec2_other_unknown", "plan_type_corr_pg_pr",
  "plan_type_corr_m_pr", "plan_type_corr_pg_pai", "plan_type_corr_m_pai",
  "occupation_condition_corr24_unemployed", "occupation_condition_corr24_inactive",
  "marital_status_rec_single", "marital_status_rec_separated_divorced_annulled_widowed",
  "tenure_status_household_renting",
  "tenure_status_household_others",
  "tenure_status_household_stays_temporarily_with_a_relative",
  "tenure_status_household_illegal_settlement",
  "urbanicity_cat_2_mixed",
  "urbanicity_cat_1_rural",
  "evaluacindelprocesoteraputico_logro_intermedio", "evaluacindelprocesoteraputico_logro_minimo",
  "eva_consumo_logro_intermedio", "eva_consumo_logro_minimo",
  "eva_fam_logro_intermedio", "eva_fam_logro_minimo",
  "eva_relinterp_logro_intermedio", "eva_relinterp_logro_minimo",
  "eva_ocupacion_logro_intermedio", "eva_ocupacion_logro_minimo",
  "eva_sm_logro_intermedio", "eva_sm_logro_minimo",
  "eva_fisica_logro_intermedio", "eva_fisica_logro_minimo",
  "eva_transgnorma_logro_intermedio", "eva_transgnorma_logro_minimo",
  "prim_sub_freq_rec_2_2_6_days_wk",
  "prim_sub_freq_rec_3_daily",
  "ed_attainment_corr_2_completed_high_school_or_less",
  "ed_attainment_corr_3_completed_primary_school_or_less"
)
.vh_all_required_cols <- c(.vh_meta_cols, .vh_cox_columns)

.vh_DUMMY_REFERENCE <- c(
  sex_rec = "man",
  plan_type_corr = "pg-pab",
  marital_status_rec = "married/cohabiting",
  cohabitation = "alone",
  sub_dep_icd10_status = "hazardous consumption",
  tr_outcome = "completion",
  adm_motive = "spontaneous consultation",
  tipo_de_vivienda_rec2 = "formal housing",
  occupation_condition_corr24 = "employed",
  any_violence = "0.No domestic violence/sex abuse",
  first_sub_used = "marijuana",
  primary_sub_mod = "marijuana",
  tenure_status_household = "owner/transferred dwellings/pays dividends",
  urbanicity_cat = "3.Urban",
  evaluacindelprocesoteraputico = "logro alto",
  eva_consumo = "logro alto",
  eva_fam = "logro alto",
  eva_relinterp = "logro alto",
  eva_ocupacion = "logro alto",
  eva_sm = "logro alto",
  eva_fisica = "logro alto",
  eva_transgnorma = "logro alto",
  prim_sub_freq_rec = "1.≤1 day/wk",
  ed_attainment_corr = "1-More than high school"
)

# ---- Helpers (verbatim from CLAUDE.md) ---------------------------------------
.vh_get_mode_numeric <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (!length(x_no_na)) return(NA_real_)
  tab <- table(x_no_na)
  as.numeric(names(tab)[which.max(tab)])
}
.vh_apply_ordered_mappings <- function(df, ordered_mappings = NULL) {
  if (is.null(ordered_mappings) || !length(ordered_mappings)) return(df)
  for (col in intersect(names(ordered_mappings), names(df))) {
    raw_values <- trimws(as.character(df[[col]]))
    mapped <- as.numeric(unname(ordered_mappings[[col]][raw_values]))
    mode_val <- .vh_get_mode_numeric(mapped)
    mapped[is.na(mapped)] <- mode_val
    df[[col]] <- mapped
  }
  df
}
.vh_set_reference_levels <- function(df, refs = .vh_DUMMY_REFERENCE) {
  for (col in intersect(names(refs), names(df))) {
    values <- trimws(as.character(df[[col]]))
    ref <- refs[[col]]
    cats <- unique(values)
    df[[col]] <- if (ref %in% cats) {
      factor(values, levels = c(ref, cats[cats != ref]), ordered = FALSE)
    } else {
      factor(values, ordered = FALSE)
    }
  }
  df
}
.vh_make_dummies <- function(df) {
  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  if (!length(cat_cols)) return(df)
  cat_df <- as.data.frame(df[cat_cols], stringsAsFactors = FALSE)
  cat_df[] <- lapply(cat_df, function(x) {
    x <- factor(x)
    x <- addNA(x, ifany = TRUE)
    lev <- levels(x)
    lev[is.na(lev)] <- "__MISSING__"
    levels(x) <- paste0("_", lev)
    x
  })
  mm <- stats::model.matrix(~ ., data = cat_df) |> as.data.frame(check.names = FALSE)
  mm[["(Intercept)"]] <- NULL
  cbind(df[setdiff(names(df), cat_cols)], mm)
}
.vh_collapse_first_sub_used_other <- function(df) {
  cols_to_group <- c(
    "first_sub_used_opioids", "first_sub_used_others",
    "first_sub_used_hallucinogens", "first_sub_used_inhalants",
    "first_sub_used_tranquilizers_hypnotics",
    "first_sub_used_amphetamine_type_stimulants"
  )
  cols_present <- intersect(cols_to_group, names(df))
  df$first_sub_used_other <- if (length(cols_present)) {
    as.integer(rowSums(df[cols_present], na.rm = TRUE) > 0)
  } else 0L
  df
}

# Preprocess ONE imputation, selecting rows by is_train flag.
# split_info must be row-aligned to df (same nrow, same order).
.vh_preprocess_one <- function(df, split_info, keep_train) {
  if (!"is_train" %in% names(split_info)) stop("split_info must include `is_train`.")
  if (nrow(df) != nrow(split_info)) stop("`df` and `split_info` must have equal nrow.")
  df_clean <- df |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::bind_cols(dplyr::select(as.data.frame(split_info), is_train)) |>
    dplyr::filter(is_train == keep_train) |>
    dplyr::select(-is_train)
  if ("center_id" %in% names(df_clean))
    df_clean$center_id <- suppressWarnings(as.numeric(df_clean$center_id))
  df_clean <- df_clean |>
    .vh_apply_ordered_mappings(ordered_mappings = NULL) |>
    .vh_set_reference_levels(refs = .vh_DUMMY_REFERENCE)
  .vh_make_dummies(df_clean)
}

# Post-process: select contract columns, add plan_type_strata + clamp times.
.vh_finalize <- function(df_proc, canonical_cols) {
  df_proc <- df_proc |>
    janitor::clean_names() |>
    .vh_collapse_first_sub_used_other()
  # Align to canonical column set: add any missing contract column as 0.
  for (cc in setdiff(canonical_cols, names(df_proc))) df_proc[[cc]] <- 0
  df_proc <- dplyr::select(df_proc, dplyr::any_of(canonical_cols))
  df_proc |>
    dplyr::mutate(
      plan_type_strata = dplyr::case_when(
        plan_type_corr_pg_pr  == 1 ~ "pg-pr",
        plan_type_corr_m_pr   == 1 ~ "m-pr",
        plan_type_corr_pg_pai == 1 ~ "pg-pai",
        plan_type_corr_m_pai  == 1 ~ "m-pai",
        TRUE ~ "pg-pab"
      ),
      plan_type_strata = as.factor(plan_type_strata),
      death_time_from_disch_m   = dplyr::if_else(death_time_from_disch_m   <= 0, 1e-6, death_time_from_disch_m),
      readmit_time_from_disch_m = dplyr::if_else(readmit_time_from_disch_m <= 0, 1e-6, readmit_time_from_disch_m)
    )
}

# ---- Main builder ------------------------------------------------------------
build_holdout_datasets <- function(force = FALSE, verify = TRUE, verbose = TRUE) {
  if (!force && file.exists(.VH_CACHE)) {
    if (verbose) cat("Loading cached holdout sets:", .VH_CACHE, "\n")
    return(readRDS(.VH_CACHE))
  }
  .msg <- function(...) if (verbose) { cat(...); flush.console() }

  # 1. Load the 5 parquet imputations (non-dummy, 88152 x 43)
  manifest <- readRDS(file.path(.VH_DATA_DIR, "corrected_datasets_nondum_filt_manifest.rds"))
  imp_paths <- manifest$paths
  stopifnot(length(imp_paths) == 5L, all(file.exists(imp_paths)))
  .msg("Reading", length(imp_paths), "parquet imputations...\n")
  cdf <- lapply(imp_paths, function(p) as.data.frame(nanoparquet::read_parquet(p)))

  # 2. Load split (user-specified parquet) and verify row alignment
  split <- as.data.frame(nanoparquet::read_parquet(.VH_SPLIT_PARQUET))
  stopifnot(all(c("row_id", "is_train") %in% names(split)))
  if (nrow(split) != nrow(cdf[[1]]))
    stop("Split rows (", nrow(split), ") != data rows (", nrow(cdf[[1]]), ").")
  # death_time is an outcome (not imputed) -> must match positionally.
  # The split stores death_time rounded to 2 dp, so compare on that scale: a true
  # misalignment would shuffle rows and produce diffs ~ the time range (~100 m).
  dt_data  <- as.numeric(cdf[[1]]$death_time_from_disch_m)
  dt_split <- as.numeric(split$death_time_from_disch_m)
  align_diff  <- abs(round(dt_data, 2) - dt_split)
  frac_mismatch <- mean(align_diff > 0.01, na.rm = TRUE)
  .msg(sprintf("Row-alignment check: max |death_time data-split| = %.3g; frac rows >0.01 off = %.4f\n",
               max(abs(dt_data - dt_split), na.rm = TRUE), frac_mismatch))
  if (is.finite(frac_mismatch) && frac_mismatch > 0.001)
    stop("Split is NOT row-aligned to corrected_datasets (death_time mismatch in ",
         round(100 * frac_mismatch, 2), "% of rows). Refusing a positional bind.")

  n_train_expected <- sum(split$is_train)
  n_val_expected   <- sum(!split$is_train)
  .msg(sprintf("Split: %d train / %d validation.\n", n_train_expected, n_val_expected))

  # 3. Build TRAIN (is_train==TRUE). Its dummy columns define the canonical set.
  .msg("Preprocessing TRAIN (5 imputations)...\n")
  train_proc <- lapply(cdf, .vh_preprocess_one, split_info = split, keep_train = TRUE)
  train_proc <- lapply(train_proc, janitor::clean_names)
  train_proc <- lapply(train_proc, .vh_collapse_first_sub_used_other)
  canonical_cols <- intersect(.vh_all_required_cols, Reduce(intersect, lapply(train_proc, names)))
  missing_contract <- setdiff(.vh_all_required_cols, canonical_cols)
  if (length(missing_contract))
    .msg("  NOTE: contract cols absent from TRAIN (added as 0 downstream): ",
         paste(missing_contract, collapse = ", "), "\n")
  canonical_cols <- .vh_all_required_cols  # keep full contract; .vh_finalize zero-fills

  train_list <- lapply(cdf, function(d)
    .vh_finalize(.vh_preprocess_one(d, split, keep_train = TRUE), canonical_cols))
  val_list   <- lapply(cdf, function(d)
    .vh_finalize(.vh_preprocess_one(d, split, keep_train = FALSE), canonical_cols))

  stopifnot(all(vapply(train_list, nrow, 0L) == n_train_expected))
  stopifnot(all(vapply(val_list,   nrow, 0L) == n_val_expected))
  # Identical columns across train & val
  stopifnot(identical(names(train_list[[1]]), names(val_list[[1]])))

  # 4. Consistency checks on TRAIN (from CLAUDE.md)
  checks <- data.frame(
    check = character(0), expected = character(0),
    observed = character(0), pass = logical(0), stringsAsFactors = FALSE
  )
  add_check <- function(name, expected, observed, pass)
    checks[nrow(checks) + 1L, ] <<- list(name, as.character(expected), as.character(observed), pass)

  n_tr <- nrow(train_list[[1]])
  add_check("n_train", 70521, n_tr, n_tr == 70521L)
  foreign_n <- sum(train_list[[1]]$national_foreign == 1, na.rm = TRUE)
  add_check("national_foreign==1 (train)", 453, foreign_n, foreign_n == 453L)
  age_mean <- mean(train_list[[1]]$adm_age_rec3, na.rm = TRUE)
  add_check("mean adm_age_rec3 (train)", 35.7256, round(age_mean, 4),
            abs(age_mean - 35.7256) <= 0.01)
  add_check("n_val", n_val_expected, nrow(val_list[[1]]), nrow(val_list[[1]]) == n_val_expected)

  # 5. Optional verification against authoritative py_corrected_datasets (TRAIN)
  if (isTRUE(verify)) {
    auth_path <- file.path(.VH_VAL_INPUTS, "py_corrected_datasets.rds")
    if (file.exists(auth_path)) {
      auth <- readRDS(auth_path)
      a1 <- as.data.frame(auth[[1]])
      formula_vars <- unique(unlist(lapply(readRDS(file.path(.VH_VAL_INPUTS, "formulas.rds")), all.vars)))
      cmp_cols <- intersect(intersect(formula_vars, names(a1)), names(train_list[[1]]))
      max_diff <- 0
      for (cc in cmp_cols) {
        av <- suppressWarnings(as.numeric(a1[[cc]]))
        tv <- suppressWarnings(as.numeric(train_list[[1]][[cc]]))
        if (length(av) == length(tv)) max_diff <- max(max_diff, max(abs(av - tv), na.rm = TRUE))
      }
      add_check("max|rebuilt-train - authoritative| over formula vars (imp1)",
                "<=1e-8", signif(max_diff, 3), is.finite(max_diff) && max_diff <= 1e-8)
    } else {
      .msg("  (authoritative py_corrected_datasets.rds not found; skipping verify)\n")
    }
  }

  if (verbose) { cat("\n=== CONSISTENCY CHECKS ===\n"); print(checks, row.names = FALSE) }
  if (!all(checks$pass))
    warning("One or more holdout consistency checks FAILED. Inspect `$checks`.", call. = FALSE)

  out <- list(
    train = train_list, val = val_list, checks = checks,
    cols = names(train_list[[1]]), n_train = n_tr, n_val = nrow(val_list[[1]]),
    split_file = .VH_SPLIT_PARQUET, created = as.character(Sys.time())
  )
  saveRDS(out, .VH_CACHE)
  .msg("Cached holdout sets ->", .VH_CACHE, "\n")
  out
}
