# =============================================================================
# extval_01_build_cohort.R
# Build the EXTERNAL (temporal) validation cohort for the frozen readmission Cox
# model: new index treatment episodes admitted 2021-01-01 .. 2024-12-31, followed
# for first readmission to SUD treatment up to the administrative censoring date
# 2025-05-28. "New patients only": every patient who already contributed a
# qualifying episode to the 2010-2020 development cohort is excluded, so the
# external set has no patient overlap with development.
#
# Source: data/20241015_out/251001_c1_1025_df_prev1y.rds
#         (object SISTRAT23_c1_2010_2024_df_prev1y, cleaned & collapsed C1 series).
#
# The cohort/outcome/recode logic mirrors cons/prediction1.qmd verbatim, only the
# date window changes:
#   - qualifying flag (age 18-64 & admission in window): prediction1.qmd:1046-1051
#   - first-qualifying-per-patient + q_rank<=2:           prediction1.qmd:1053-1073
#   - anchors (first_adm/first_disch/second_adm):         prediction1.qmd:1087-1094
#   - readmission event/time + censoring:                 prediction1.qmd:1106-1141
#   - drop "currently in":                                prediction1.qmd:1174
#   - dit_m, primary_sub_mod, ethnicity, tr_outcome,
#     prim_sub_freq_rec, sex_rec and the Table-1 recodes: prediction1.qmd:1660-1896, 2005-2013
#
# Key data fact: admissions stop at 2024-12-20 (no 2025 admissions), so every
# readmission falls inside the qualifying window; the 2025-05-28 censor only
# extends follow-up time for non-readmitted patients. Hence prediction1's exact
# logic applies with .end_date=2024-12-31 and .censor_date=2025-05-28.
#
# Public API:
#   build_extval_readmit_cohort(prev1y_path = NULL,
#                               start = "2021-01-01", end = "2024-12-31",
#                               censor = "2025-05-28",
#                               dev_start = "2010-01-01", dev_end = "2020-12-31",
#                               verbose = TRUE)
#     -> list(nondum = <data.frame>, flow = <data.frame>, params = <list>)
# =============================================================================

suppressWarnings({
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("lubridate", quietly = TRUE))
})

# --- Resolve project root (works from project root or from cons/) -------------
if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}

# --- small helpers (warning-free min over a flagged subset) -------------------
.ev_min_date_where <- function(x, flag) {
  idx <- which(flag %in% TRUE & !is.na(x))
  if (length(idx) == 0L) as.Date(NA) else min(x[idx])
}
.ev_min_date <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0L) as.Date(NA) else min(x) }
.ev_min_int_where <- function(x, flag) {
  idx <- which(flag %in% TRUE & !is.na(x))
  if (length(idx) == 0L) NA_integer_ else as.integer(min(x[idx]))
}

build_extval_readmit_cohort <- function(
    prev1y_path = NULL,
    start  = "2021-01-01", end = "2024-12-31", censor = "2025-05-28",
    dev_start = "2010-01-01", dev_end = "2020-12-31",
    verbose = TRUE) {

  .msg <- function(...) if (verbose) { cat(...); flush.console() }
  if (is.null(prev1y_path))
    prev1y_path <- file.path(project_root, "data/20241015_out/251001_c1_1025_df_prev1y.rds")
  stopifnot(file.exists(prev1y_path))

  .start_date  <- as.Date(start)
  .end_date    <- as.Date(end)
  .censor_date <- as.Date(censor)
  .dev_start   <- as.Date(dev_start)
  .dev_end     <- as.Date(dev_end)

  .msg("Loading prev1y:", prev1y_path, "\n")
  raw <- as.data.frame(readRDS(prev1y_path), stringsAsFactors = FALSE)

  # 0) canonical order + per-patient treatment index (prediction1.qmd:1034-1037)
  df0 <- raw |>
    dplyr::arrange(hash_key, adm_date_rec2, disch_date_rec6) |>
    dplyr::mutate(treatment = dplyr::row_number(), .by = hash_key)
  n0_cases <- nrow(df0); n0_pat <- dplyr::n_distinct(df0$hash_key)
  .msg(sprintf("0. INITIAL prev1y: cases=%s, patients=%s\n",
               format(n0_cases, big.mark = ","), format(n0_pat, big.mark = ",")))

  # New-patients-only: exclude every patient with >=1 qualifying (age 18-64 &
  # admission 2010-2020) episode, i.e. anyone eligible for the development cohort.
  dev_qual <- df0 |>
    dplyr::mutate(meets_dev = !is.na(adm_age_rec3) & adm_age_rec3 >= 18 & adm_age_rec3 <= 64 &
                    !is.na(adm_date_rec2) & adm_date_rec2 >= .dev_start & adm_date_rec2 <= .dev_end)
  dev_patients <- unique(dev_qual$hash_key[dev_qual$meets_dev])
  .msg(sprintf("   Development-cohort patients (qualifying 2010-2020) to exclude: %s\n",
               format(length(dev_patients), big.mark = ",")))

  # 1) Qualifying flag for the EXTERNAL window (age 18-64 & admission 2021-2024)
  df1 <- df0 |>
    dplyr::filter(!hash_key %in% dev_patients) |>
    dplyr::mutate(meets_crit = !is.na(adm_age_rec3) & adm_age_rec3 >= 18 & adm_age_rec3 <= 64 &
                    !is.na(adm_date_rec2) & adm_date_rec2 >= .start_date & adm_date_rec2 <= .end_date)
  n_after_excl_pat <- dplyr::n_distinct(df1$hash_key)
  n_qual_pat <- dplyr::n_distinct(df1$hash_key[df1$meets_crit])
  .msg(sprintf("1. After excluding development patients: %s patients remain; %s have >=1 qualifying 2021-2024 episode\n",
               format(n_after_excl_pat, big.mark = ","), format(n_qual_pat, big.mark = ",")))

  # 2) qualifying episodes only; rank; drop 3rd+ (prediction1.qmd:1069-1073)
  df_q <- df1 |>
    dplyr::filter(meets_crit) |>
    dplyr::arrange(hash_key, adm_date_rec2, disch_date_rec6) |>
    dplyr::mutate(q_rank = dplyr::row_number(), .by = hash_key) |>
    dplyr::filter(q_rank <= 2L)

  # 3) anchors per patient (prediction1.qmd:1087-1094)
  anchors <- df_q |>
    dplyr::summarise(
      first_adm   = .ev_min_date_where(adm_date_rec2,   q_rank == 1L),
      first_disch = .ev_min_date_where(disch_date_rec6, q_rank == 1L),
      second_adm  = .ev_min_date_where(adm_date_rec2,   q_rank == 2L),
      .by = hash_key)
  death_tbl <- df0 |>
    dplyr::summarise(death_date = .ev_min_date(def_date), .by = hash_key)

  # 4) per-person time-to-event (prediction1.qmd:1106-1142); censor at 2025-05-28
  perperson <- df_q |>
    dplyr::distinct(hash_key) |>
    dplyr::left_join(anchors,   by = "hash_key") |>
    dplyr::left_join(death_tbl, by = "hash_key") |>
    dplyr::mutate(
      readmit_event = as.integer(!is.na(second_adm) & second_adm <= .censor_date),
      death_event   = as.integer(!is.na(death_date)  & death_date  <= .censor_date),
      readmit_end_from_adm   = dplyr::if_else(readmit_event == 1L, second_adm, .censor_date),
      readmit_end_from_disch = dplyr::if_else(readmit_event == 1L, second_adm, .censor_date),
      death_end_from_disch   = dplyr::if_else(death_event   == 1L, death_date, .censor_date),
      readmit_time_from_adm_m = dplyr::if_else(!is.na(first_adm),
        as.numeric(lubridate::time_length(lubridate::interval(first_adm,   readmit_end_from_adm),   "months")), NA_real_),
      readmit_time_from_disch_m = dplyr::if_else(!is.na(first_disch),
        as.numeric(lubridate::time_length(lubridate::interval(first_disch, readmit_end_from_disch), "months")), NA_real_),
      death_time_from_disch_m = dplyr::if_else(!is.na(first_disch),
        as.numeric(lubridate::time_length(lubridate::interval(first_disch, death_end_from_disch),   "months")), NA_real_)
    ) |>
    dplyr::mutate(
      readmit_time_from_adm_m   = dplyr::if_else(!is.na(readmit_time_from_adm_m),   pmax(readmit_time_from_adm_m,   0), NA_real_),
      readmit_time_from_disch_m = dplyr::if_else(!is.na(readmit_time_from_disch_m), pmax(readmit_time_from_disch_m, 0), NA_real_),
      death_time_from_disch_m   = dplyr::if_else(!is.na(death_time_from_disch_m),   pmax(death_time_from_disch_m,   0), NA_real_)
    )

  # 5) FINAL: first qualifying row per patient; drop "currently in" (prediction1.qmd:1144-1196)
  ext <- df_q |>
    dplyr::filter(q_rank == 1L) |>
    dplyr::left_join(perperson, by = "hash_key") |>
    dplyr::select(-meets_crit, -q_rank)
  n_first <- nrow(ext)
  ext <- dplyr::filter(ext, !(tr_compliance_rec7 %in% "currently in"))
  n_not_currently <- nrow(ext)
  .msg(sprintf("2. First qualifying episode per patient: %s\n3. After dropping 'currently in': %s\n",
               format(n_first, big.mark = ","), format(n_not_currently, big.mark = ",")))

  # ---- Variable construction (prediction1.qmd, English labels = dev contract) ----
  ext <- ext |>
    dplyr::mutate(
      # length of stay in months (prediction1.qmd:1666)
      dit_m = readmit_time_from_adm_m - readmit_time_from_disch_m,

      # primary substance (prediction1.qmd:1733-1744)
      .psub = stringr::str_to_lower(stringr::str_squish(primary_sub)),
      primary_sub_mod = dplyr::case_when(
        stringr::str_detect(.psub, "cocaine\\s*(base\\s*)?paste|pasta\\s*base|\\bpaco\\b") ~ "cocaine paste",
        stringr::str_detect(.psub, "cocaine\\s*(powder|hydrochloride|hcl)") |
          stringr::str_detect(.psub, "clorhidrato\\s*de\\s*coca|clorhidrato|hidrocloruro") ~ "cocaine powder",
        stringr::str_detect(.psub, "\\balcohol\\b") ~ "alcohol",
        stringr::str_detect(.psub, "marij|cannab|marihu") ~ "marijuana",
        TRUE ~ "others"),

      # ethnicity (prediction1.qmd:1747)
      ethnicity = ifelse(!is.na(ethnicity_c1_c6_historic), 1, 0),

      # treatment outcome (prediction1.qmd:1748-1761)
      tr_outcome = dplyr::case_when(
        tr_compliance_rec7 == "currently in" & is.na(adm_disch_reason) ~ NA_character_,
        stringr::str_detect(dplyr::coalesce(tr_compliance_rec7, ""), "dropout") & is.na(adm_disch_reason) ~ "dropout",
        tr_compliance_rec7 == "referral"   & is.na(adm_disch_reason) ~ "referral",
        tr_compliance_rec7 == "completion" & is.na(adm_disch_reason) ~ "completion",
        stringr::str_detect(dplyr::coalesce(tr_compliance_rec7, ""), "discharge") &
          adm_disch_reason %in% c("agreement_end", "death", "no_local_service") ~ "adm discharge - adm reasons",
        stringr::str_detect(dplyr::coalesce(tr_compliance_rec7, ""), "discharge") &
          (adm_disch_reason == "rule_violation" | is.na(adm_disch_reason)) ~ "adm discharge - rule violation/undet",
        TRUE ~ "other"),

      # primary-substance frequency (prediction1.qmd:1786-1797); en-dash U+2013 preserved
      prim_sub_freq_rec = dplyr::case_when(
        prim_sub_freq %in% c("1. Less than 1 day a week", "2. 1 day a week") ~ "1.≤1 day/wk",
        prim_sub_freq %in% c("3. 2 to 3 days a week", "4. 4 to 6 days a week") ~ "2.2–6 days/wk",
        is.na(prim_sub_freq) ~ NA_character_,
        TRUE ~ "3.Daily"),

      # sex (prediction1.qmd:1862-1871)
      sex_rec = dplyr::case_when(sex_rec == "mujer" ~ "woman", sex_rec == "hombre" ~ "man", TRUE ~ NA_character_),

      # type of housing dichotomy (prediction1.qmd:1800-1813)
      tipo_de_vivienda_rec2 = dplyr::case_when(
        tipo_de_vivienda %in% c("casa", "departamento") ~ "formal housing",
        is.na(tipo_de_vivienda) ~ NA_character_,
        TRUE ~ "other/unknown"),

      # marital status (prediction1.qmd:1680)
      marital_status_rec = dplyr::case_when(
        marital_status == "separated/divorced/annulled" ~ "separated/divorced/annulled/widowed",
        marital_status == "widowed" ~ "separated/divorced/annulled/widowed",
        TRUE ~ marital_status),

      # urbanicity (prediction1.qmd:1746)
      urbanicity_cat = dplyr::case_when(
        grepl("Mix", clasificacion) ~ "2.Mixed",
        grepl("Urb", clasificacion) ~ "3.Urban",
        grepl("Rur", clasificacion) ~ "1.Rural",
        TRUE ~ NA_character_),

      # foreign nationality (prediction1.qmd:1873-1882, 2573)
      national_foreign = dplyr::case_when(
        nationality_cons == "chile" ~ 0L,
        !is.na(nationality_cons) & nationality_cons != "chile" ~ 1L,
        TRUE ~ NA_integer_)
    )

  # cohabitation (prediction1.qmd:1682-1715)
  cqv <- ext$con_quien_vive |> stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |> stringr::str_squish()
  joel5 <- dplyr::case_when(
    stringr::str_detect(cqv, "^solo$") ~ "Alone",
    stringr::str_detect(cqv, "^unicamente con (la )?pareja( e hijos)?$") |
      stringr::str_detect(cqv, "^unicamente con pareja$") |
      stringr::str_detect(cqv, "^unicamente con hijos$") ~ "Couple/children only",
    stringr::str_detect(cqv, "(pareja|hijos).*(padres|familia de origen)") |
      stringr::str_detect(cqv, "^con la pareja,? hijos y padres") ~ "Couple/children + origin",
    stringr::str_detect(cqv, "^unicamente con padres|^con padres|familia de origen$") |
      stringr::str_detect(cqv, "^con la madre \\(sola\\)$") |
      stringr::str_detect(cqv, "^con abuelos$") |
      stringr::str_detect(cqv, "^con hermanos$") ~ "Family of origin",
    stringr::str_detect(cqv, "con amigos|otro no pariente|otro pariente|^otros$") ~ "Others",
    TRUE ~ "Others")
  ext$cohabitation <- dplyr::case_when(
    joel5 == "Alone" ~ "alone",
    joel5 %in% c("Couple/children only", "Couple/children + origin") ~ "with couple/children",
    joel5 == "Family of origin" ~ "family of origin",
    TRUE ~ "Others")

  # domestic violence / sex abuse -> any_violence (prediction1.qmd:1725-1732, 1885-1896)
  dom <- dplyr::case_when(grepl("Violencia Intrafamiliar$", ext$otros_probl_at_sm_or, ignore.case = TRUE) ~ 1,
                          is.na(ext$otros_probl_at_sm_or) ~ NA_real_, TRUE ~ 0)
  sab <- dplyr::case_when(grepl("Abuso Sexual", ext$otros_probl_at_sm_or, ignore.case = TRUE) ~ 1,
                          is.na(ext$otros_probl_at_sm_or) ~ NA_real_, TRUE ~ 0)
  ext$any_violence <- dplyr::case_when(
    sab == 1 | dom == 1 ~ "1.Domestic violence/sex abuse",
    sab == 0 & dom == 0 ~ "0.No domestic violence/sex abuse",
    sab == 0 & is.na(dom) ~ "0.No domestic violence/sex abuse",
    is.na(sab) & dom == 0 ~ "0.No domestic violence/sex abuse",
    TRUE ~ NA_character_)

  # episode-level psychiatric F-group flags (prediction1.qmd:1669-1679, 2007-2011)
  dxn <- ext$mod_psiq_cie_10 |> stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower(); dxn[is.na(dxn)] <- ""
  f0  <- stringr::str_detect(dxn, "mentales organicos")
  f2  <- stringr::str_detect(dxn, "esquizofren|esquizotip|ideas? delirant|psicosis")
  f3  <- stringr::str_detect(dxn, "humor \\(afectivos\\)")
  f6  <- stringr::str_detect(dxn, "personalidad y del comportamiento del adulto|habitos y del control de los impulsos|transformacion persistente de la personalidad")
  f7  <- stringr::str_detect(dxn, "retraso mental|discapacidad intelectual")
  f89 <- stringr::str_detect(dxn, "desarrollo psicolog|comienzo habitual en la infancia y adolescencia")
  ext$dx_f3_mood            <- as.integer(f3)
  ext$dx_f6_personality     <- as.integer(f6)
  ext$dx_f_any_severe_mental <- (f0 | f2 | f7 | f89)        # SMI/neurocognitive (prediction1.qmd:2445, 2576)

  # any physical comorbidity: a non-empty, non-"none"/"in study" physical diagnosis
  pf <- ext$diagnostico_trs_fisico |> stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |> stringr::str_squish()
  ext$any_phys_dx <- !is.na(pf) & nzchar(pf) &
    !stringr::str_detect(pf, "sin (diagnostico|patologia)|no (presenta|refiere|aplica)|ninguno|en estudio|sin trastorno")

  # coalesce types expected by the model / dummify pipeline
  ext$dg_psiq_cie_10_dg      <- as.logical(ext$dg_psiq_cie_10_dg)
  ext$dg_psiq_cie_10_instudy <- as.logical(ext$dg_psiq_cie_10_instudy)
  ext$polysubstance_strict   <- as.integer(ext$polysubstance_strict)
  ext$ethnicity              <- as.numeric(ext$ethnicity)

  flow <- data.frame(
    step = c("prev1y rows", "prev1y patients", "development patients excluded",
             "patients remaining", "patients with qualifying 2021-2024 episode",
             "first qualifying episode per patient", "after dropping 'currently in' (FINAL)",
             "readmission events", "readmission censored"),
    n = c(n0_cases, n0_pat, length(dev_patients), n_after_excl_pat, n_qual_pat,
          n_first, n_not_currently,
          sum(ext$readmit_event == 1L, na.rm = TRUE), sum(ext$readmit_event == 0L, na.rm = TRUE)),
    stringsAsFactors = FALSE)
  if (verbose) { cat("\n=== EXTERNAL COHORT FLOW ===\n"); print(flow, row.names = FALSE) }

  list(nondum = ext, flow = flow,
       params = list(start = .start_date, end = .end_date, censor = .censor_date,
                     dev_start = .dev_start, dev_end = .dev_end,
                     prev1y_path = prev1y_path, created = as.character(Sys.time())))
}
