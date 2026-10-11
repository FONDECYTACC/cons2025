# =============================================================================
# extval_death_01_build_cohort.R
# Build the EXTERNAL (case-mix) validation cohorts for the frozen mortality Cox
# model from SENDA agreements (convenios) 3 and 5, which the development cohort
# (convenio 1 + women's plans = "la muestra original") never saw.
#
#   - Convenio 3 = population experiencing homelessness ("situacion de calle"),
#     adults. The raw extract has NO treatment-plan column, so plan_type is set to
#     a single residential stratum (pg-pr) per the agreed mapping.
#   - Convenio 5 = adolescent population ("infanto-adolescente"); only the adult
#     fraction (18-64) is in the model's scope. Plan codes map pai-ia -> pg-pai,
#     pr-ia / ml-pr -> pg-pr.
#
# Sources (small RDS pre-extracted ONCE from 22_ndp_2025_09_27.Rdata so the 404 MB
# workspace is never reloaded here):
#   data/20241015_out/_val_inputs/extval_CONS_C3_25_df.rds   (raw C3, 3077 x 72)
#   data/20241015_out/_val_inputs/extval_CONS_C5_25_df.rds   (raw C5, 12440 x 91)
#   data/20241015_out/_val_inputs/extval_death_by_hash.rds   (first def_date / hashkey)
#   data/20241015_out/_val_inputs/extval_dev_hash.rds         (development hash_keys)
#
# Variables come RAW (Spanish), so this script recodes them to the C1 development
# schema/labels (verified against the codebook and the reference levels in
# val_holdout_02_build_sets.R / extval_02_table1_smd.R). Only the covariates the
# SHAP mortality model (best_perf2) needs are required; a few extra are kept for
# the Table 1 / SMD comparison and the propensity matching.
#
# Death outcome: the DEIS mortality registry covers deaths 2008..2020, so follow
# -up is censored at 2020-12-31 (the SAME death-ascertainment window the model was
# developed on) and only episodes discharged on/before that date are kept. Clock =
# time since DISCHARGE (months), matching the development mortality model.
#
# Public API:
#   build_extval_death_cohort(which = c("C3","C5"),
#                             censor = "2020-12-31",
#                             age_lo = 18, age_hi = 64, verbose = TRUE)
#     -> list(nondum = <data.frame>, flow = <data.frame>, params = <list>)
# =============================================================================

suppressWarnings({
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("stringi", quietly = TRUE))
  stopifnot(requireNamespace("lubridate", quietly = TRUE))
})

if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}

# ---- shared raw->dev recoders (byte-identical target labels) ------------------
.evd_norm <- function(x) stringr::str_squish(stringr::str_to_lower(
  stringi::stri_trans_general(as.character(x), "Latin-ASCII")))

.evd_sex <- function(x) {
  v <- .evd_norm(x)
  dplyr::case_when(v %in% c("mujer", "femenino") ~ "woman",
                   v %in% c("hombre", "masculino") ~ "man",
                   TRUE ~ NA_character_)
}
# primary substance -> {cocaine paste, cocaine powder, alcohol, marijuana(ref), others}
.evd_primary_sub <- function(x) {
  v <- .evd_norm(x)
  dplyr::case_when(
    stringr::str_detect(v, "pasta\\s*base|pasta|\\bpaco\\b") ~ "cocaine paste",
    stringr::str_detect(v, "clorhidrato|cocaina|cocaina|cocaina|cocano|cocaina") ~ "cocaine powder",
    stringr::str_detect(v, "^alcohol$|\\balcohol\\b") ~ "alcohol",
    stringr::str_detect(v, "marihu|marij|cannab") ~ "marijuana",
    is.na(x) ~ NA_character_,
    TRUE ~ "others")
}
# substance of onset -> same families (only first_sub_used_alcohol is in the model)
.evd_first_sub <- function(x) {
  v <- .evd_norm(x)
  dplyr::case_when(
    stringr::str_detect(v, "^alcohol$|\\balcohol\\b") ~ "alcohol",
    stringr::str_detect(v, "marihu|marij|cannab") ~ "marijuana",
    stringr::str_detect(v, "pasta\\s*base|pasta|\\bpaco\\b") ~ "cocaine paste",
    stringr::str_detect(v, "clorhidrato|cocaina|cocano") ~ "cocaine powder",
    is.na(x) ~ NA_character_,
    TRUE ~ "others")
}
# frequency -> {1.<=1 day/wk(ref), 2.2-6 days/wk, 3.Daily}; en-dash U+2013 preserved
.evd_freq <- function(x) {
  v <- .evd_norm(x)
  dplyr::case_when(
    stringr::str_detect(v, "todos los dias|diario|diaria") ~ "3.Daily",
    stringr::str_detect(v, "2-3 dias|4-6 dias|2 a 3|4 a 6") ~ "2.2–6 days/wk",
    stringr::str_detect(v, "1 dia|1 dias|menos de 1|no consumio") ~ "1.≤1 day/wk",
    stringr::str_detect(v, "desconocida|desconocido") ~ NA_character_,
    is.na(x) ~ NA_character_,
    TRUE ~ NA_character_)
}
# occupational condition -> {employed(ref), unemployed, inactive} (corr24 logic:
# working -> employed; cesante / first-time job seeker -> unemployed; the rest,
# incl. "sin actividad" / students / homemakers / pensioners -> inactive)
.evd_occupation <- function(x) {
  v <- .evd_norm(x)
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    stringr::str_detect(v, "trabajando") ~ "employed",
    v %in% c("cesante", "buscando trabajo por primera vez") ~ "unemployed",
    TRUE ~ "inactive")
}
# discharge-evaluation domains -> {logro alto(ref), logro intermedio, logro minimo}
.evd_eva <- function(x) {
  v <- .evd_norm(x)
  dplyr::case_when(
    stringr::str_detect(v, "logro alto|alto") ~ "logro alto",
    stringr::str_detect(v, "intermedio") ~ "logro intermedio",
    stringr::str_detect(v, "minimo|nulo") ~ "logro minimo",
    TRUE ~ NA_character_)
}
# cohabitation status -> {alone(ref), with couple/children, family of origin, others}.
# Same category scheme/logic as extval_01_build_cohort.R's cohabitation recoder
# (traced to prediction1.qmd:1682-1715), extended for the "/a"/"/as" gender-suffix
# raw variants observed in the SENDA C3/C5 con_quien_vive field (e.g. "solo/a",
# "unicamente con hijos/as") and for "con el padre (solo)" (C5), which was missing
# from the original regex even though its exact counterpart "con la madre (sola)"
# was already mapped to Family of origin.
.evd_cohabitation <- function(x) {
  cqv <- .evd_norm(x)
  joel5 <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    stringr::str_detect(cqv, "^solo(/a)?$") ~ "Alone",
    stringr::str_detect(cqv, "^unicamente con (la )?pareja( e hijos(/as)?)?$") |
      stringr::str_detect(cqv, "^unicamente con pareja$") |
      stringr::str_detect(cqv, "^unicamente con hijos(/as)?$") ~ "Couple/children only",
    stringr::str_detect(cqv, "(pareja|hijos).*(padres|familia de origen)") |
      stringr::str_detect(cqv, "^con la pareja,? hijos y padres") ~ "Couple/children + origin",
    stringr::str_detect(cqv, "^unicamente con padres|^con padres|familia de origen$") |
      stringr::str_detect(cqv, "^con la madre \\(sola\\)$") |
      stringr::str_detect(cqv, "^con el padre \\(solo\\)$") |
      stringr::str_detect(cqv, "^con abuelos$") |
      stringr::str_detect(cqv, "^con hermanos$") ~ "Family of origin",
    stringr::str_detect(cqv, "con amigos|otro no pariente|otro pariente|^otros$") ~ "Others",
    TRUE ~ "Others")
  dplyr::case_when(
    joel5 == "Alone" ~ "alone",
    joel5 %in% c("Couple/children only", "Couple/children + origin") ~ "with couple/children",
    joel5 == "Family of origin" ~ "family of origin",
    is.na(joel5) ~ NA_character_,
    TRUE ~ "others")
}

# any physical comorbidity: non-empty, non-"none"/"in study" physical diagnosis
.evd_any_phys <- function(...) {
  cols <- list(...)
  pf <- do.call(paste, c(lapply(cols, function(z) ifelse(is.na(z), "", as.character(z))), sep = " ::: "))
  pf <- .evd_norm(pf)
  out <- nzchar(pf) &
    !stringr::str_detect(pf, "sin (diagnostico|patologia|trastorno)|no (presenta|refiere|aplica)|ninguno|en estudio")
  # rows that were entirely missing -> NA
  allmiss <- Reduce(`&`, lapply(cols, function(z) is.na(z) | !nzchar(trimws(as.character(z)))))
  out[allmiss] <- NA
  out
}

# ---- per-convenio raw column maps --------------------------------------------
.evd_colmap <- list(
  C3 = list(
    id = "HASH_KEY", age = "edad", sex = "sexo",
    primary_sub = "sustancia_principal", first_sub = "sustancia_de_inicio",
    freq = "frecuencia_de_consumo_sustancia_principal",
    occupation = "condicion_ocupacional",
    eva_ocupacion = "evaluacion_al_egreso_respecto_sit_ocup",
    eva_fisica = "evaluacion_al_egreso_respecto_salud_fisica",
    cohab = "con_quien_vive",
    phys = c("diagnostico_trs_fisico", "diagnostico_trs_fisico2", "diagnostico_trs_fisico3"),
    adm = "fecha_ingreso_a_tratamiento", disch = "fecha_egreso_de_tratamiento",
    # Discharge reason. These two columns were always present in the extract but were not mapped,
    # which is why earlier notes claimed the convenio files had no discharge reason at all.
    disch_reason = "motivo_de_egreso", disch_admin_reason = "motivodeegreso_alta_administra",
    plan = NA_character_),                          # no plan column -> fixed pg-pr
  C5 = list(
    id = "HASH_KEY", age = "edad", sex = "sexo",
    primary_sub = "sustancia_principal", first_sub = "sustancia_inicial",
    freq = "frecuencia_consumo",
    occupation = "estado_laboral",
    eva_ocupacion = "evaluacion_al_egreso_respecto_a_situacion_ocupacional",
    eva_fisica = "evaluacion_al_egreso_respecto_salud_fisica",
    cohab = "con_quien_vive",
    phys = c("diagnostico_trs_fisico"),
    adm = "fecha_ingreso_a_tratamiento", disch = "fecha_egreso_de_tratamiento",
    disch_reason = "motivo_de_egreso", disch_admin_reason = "motivodeegreso_alta_administra",
    plan = "tipo_de_plan")
)

# adolescent plan codes (C5) -> development modality
.evd_plan_c5 <- function(x) {
  v <- .evd_norm(x)
  dplyr::case_when(
    stringr::str_detect(v, "^pai") ~ "pg-pai",   # outpatient intensive
    stringr::str_detect(v, "^pr|^ml-pr|^ml pr") ~ "pg-pr",  # residential
    is.na(x) ~ "pg-pr",
    TRUE ~ "pg-pr")
}

build_extval_death_cohort <- function(which = c("C3", "C5"),
                                      censor = "2020-12-31",
                                      age_lo = 18, age_hi = 64,
                                      alive_at_discharge = TRUE,
                                      precaution_months = 0.23,
                                      precaution_scope = c("administrative", "any", "none"),
                                      inputs_dir = NULL, verbose = TRUE) {
  which <- match.arg(which)
  precaution_scope <- match.arg(precaution_scope)
  .msg <- function(...) if (verbose) { cat(...); flush.console() }
  if (is.null(inputs_dir)) inputs_dir <- file.path(project_root, "data/20241015_out/_val_inputs")
  cm <- .evd_colmap[[which]]
  src <- file.path(inputs_dir, sprintf("extval_CONS_%s_25_df.rds", which))
  stopifnot(file.exists(src))
  raw <- as.data.frame(readRDS(src), stringsAsFactors = FALSE)
  dev_hash      <- readRDS(file.path(inputs_dir, "extval_dev_hash.rds"))
  death_by_hash <- as.data.frame(readRDS(file.path(inputs_dir, "extval_death_by_hash.rds")))
  .censor <- as.Date(censor)

  n0_rows <- nrow(raw); n0_pat <- dplyr::n_distinct(raw[[cm$id]])
  .msg(sprintf("[%s] raw: rows=%s patients=%s\n", which,
               format(n0_rows, big.mark = ","), format(n0_pat, big.mark = ",")))

  d <- data.frame(hash_key = as.character(raw[[cm$id]]), stringsAsFactors = FALSE)
  d$adm_age_rec3 <- suppressWarnings(as.numeric(raw[[cm$age]]))
  d$.adm   <- suppressWarnings(as.Date(raw[[cm$adm]],   format = "%d/%m/%Y"))
  d$.disch <- suppressWarnings(as.Date(raw[[cm$disch]], format = "%d/%m/%Y"))
  d$sex_rec           <- .evd_sex(raw[[cm$sex]])
  d$primary_sub_mod   <- .evd_primary_sub(raw[[cm$primary_sub]])
  d$first_sub_used    <- .evd_first_sub(raw[[cm$first_sub]])
  d$prim_sub_freq_rec <- .evd_freq(raw[[cm$freq]])
  d$occupation_condition_corr24 <- .evd_occupation(raw[[cm$occupation]])
  d$eva_ocupacion     <- .evd_eva(raw[[cm$eva_ocupacion]])
  d$eva_fisica        <- .evd_eva(raw[[cm$eva_fisica]])
  d$cohabitation      <- .evd_cohabitation(raw[[cm$cohab]])
  d$any_phys_dx       <- do.call(.evd_any_phys, lapply(cm$phys, function(cc) raw[[cc]]))
  d$plan_type_corr <- if (is.na(cm$plan)) "pg-pr" else .evd_plan_c5(raw[[cm$plan]])
  # Discharge reason, carried through so the alive-at-origin rules below can be reason-based rather
  # than date-based only. Not a model covariate: the frozen death model does not use it.
  d$disch_reason <- .evd_norm(raw[[cm$disch_reason]])
  d$disch_admin_reason <- .evd_norm(raw[[cm$disch_admin_reason]])

  # 1) exclude development patients (convenio 1 + women's plans = original sample)
  in_dev <- d$hash_key %in% dev_hash
  d1 <- d[!in_dev, , drop = FALSE]
  .msg(sprintf("1. exclude development-cohort patients: removed %s rows; %s remain\n",
               format(sum(in_dev), big.mark = ","), format(nrow(d1), big.mark = ",")))

  # 2) adults in model scope + valid admission/discharge in death-ascertainment window
  d2 <- d1[!is.na(d1$adm_age_rec3) & d1$adm_age_rec3 >= age_lo & d1$adm_age_rec3 <= age_hi, , drop = FALSE]
  n_adult <- nrow(d2)
  d2 <- d2[!is.na(d2$.disch) & d2$.disch <= .censor, , drop = FALSE]
  n_disch_ok <- nrow(d2)
  .msg(sprintf("2. adults %d-%d: %s ; with discharge on/before %s: %s\n",
               age_lo, age_hi, format(n_adult, big.mark = ","),
               format(.censor), format(n_disch_ok, big.mark = ",")))

  # 3) first qualifying episode per patient (discharge clock anchor)
  d3 <- d2 |>
    dplyr::arrange(hash_key, .adm, .disch) |>
    dplyr::distinct(hash_key, .keep_all = TRUE)
  .msg(sprintf("3. first episode per patient: %s\n", format(nrow(d3), big.mark = ",")))

  # 4) death outcome from the registry (clock = discharge), censor 2020-12-31
  d3 <- dplyr::left_join(d3, death_by_hash, by = c("hash_key" = "hashkey"))
  d3$death_event <- as.integer(!is.na(d3$def_date) & d3$def_date <= .censor &
                                 d3$def_date >= d3$.disch)
  end_date <- dplyr::if_else(d3$death_event == 1L, d3$def_date, .censor)
  d3$death_time_from_disch_m <- as.numeric(
    lubridate::time_length(lubridate::interval(d3$.disch, end_date), "months"))
  d3$death_time_from_disch_m <- pmax(d3$death_time_from_disch_m, 0)

  # 5) post-discharge definition: the cohort must be ALIVE at the time origin, and deaths falling
  #    inside a short precautionary window after an administrative discharge are removed because
  #    such a discharge is plausibly a consequence of the terminal event rather than a prognosis
  #    made from a stable state. Both rules are counted before being applied so the flow table
  #    shows what was removed and why, and both can be switched off to reproduce the historical
  #    code literally as an adversarial arm.
  # WHY THIS IS NEEDED: death_event above requires def_date >= .disch. A patient whose registered
  # death date PRECEDES the discharge date therefore fails that test and is carried as a CENSORED
  # SURVIVOR, that is, a dead patient counted as alive. Convenio 3 has 20 such patients against 82
  # deaths, so the misclassification is not a rare edge case.
  .not_alive <- !is.na(d3$def_date) & !is.na(d3$.disch) & d3$def_date <= d3$.disch
  .is_admin <- grepl("administrativ", dplyr::coalesce(d3$disch_reason, ""), ignore.case = TRUE)
  .early <- d3$death_event == 1L & d3$death_time_from_disch_m > 0 &
    d3$death_time_from_disch_m <= precaution_months
  .precautionary <- switch(precaution_scope,
                           administrative = .early & .is_admin,
                           any = .early,
                           none = rep(FALSE, nrow(d3)))
  n_not_alive <- sum(.not_alive)
  n_zero_time <- sum(d3$death_event == 1L & d3$death_time_from_disch_m <= 0)
  n_precaution <- sum(.precautionary & !.not_alive)
  .drop <- (alive_at_discharge & .not_alive) | .precautionary
  if (verbose) {
    .msg(sprintf("5. post-discharge definition (alive_at_discharge=%s, precaution=%s %.2f m):\n",
                 alive_at_discharge, precaution_scope, precaution_months))
    .msg(sprintf("   not alive at the discharge origin: %d (of which %d were carried as censored survivors, %d as zero-time deaths)\n",
                 n_not_alive, sum(.not_alive & d3$death_event == 0L), n_zero_time))
    .msg(sprintf("   deaths inside the precautionary window: %d total, %d after an administrative discharge, %d removed under the chosen scope\n",
                 sum(.early), sum(.early & .is_admin), n_precaution))
    .msg(sprintf("   removed in total: %d ; remaining: %d\n", sum(.drop), sum(!.drop)))
  }
  d3 <- d3[!.drop, , drop = FALSE]

  # placeholders so the shared dummify/contract pipeline is happy (readmission is
  # out of scope here; the death evaluator ignores these)
  d3$readmit_event <- 0L
  d3$readmit_time_from_disch_m <- d3$death_time_from_disch_m
  d3$center_id <- NA_real_

  flow <- data.frame(
    step = c("raw rows", "raw patients", "after excluding development patients",
             paste0("adults ", age_lo, "-", age_hi),
             paste0("with discharge <= ", format(.censor)),
             "first episode per patient",
             "removed: not alive at the discharge origin",
             paste0("removed: death within ", precaution_months, " months of a ",
                    precaution_scope, " discharge"),
             "alive-at-origin cohort (FINAL)",
             "deaths (post-discharge, <= censor)", "censored"),
    n = c(n0_rows, n0_pat, nrow(d1), n_adult, n_disch_ok, nrow(d3) + sum(.drop),
          if (alive_at_discharge) n_not_alive else 0L, n_precaution, nrow(d3),
          sum(d3$death_event == 1L, na.rm = TRUE), sum(d3$death_event == 0L, na.rm = TRUE)),
    stringsAsFactors = FALSE)
  if (verbose) { cat(sprintf("\n=== %s DEATH COHORT FLOW ===\n", which)); print(flow, row.names = FALSE) }

  list(nondum = d3, flow = flow,
       params = list(convenio = which, censor = .censor, age_lo = age_lo, age_hi = age_hi,
                     alive_at_discharge = alive_at_discharge,
                     precaution_months = precaution_months, precaution_scope = precaution_scope,
                     n_removed_not_alive = if (alive_at_discharge) n_not_alive else 0L,
                     n_removed_precautionary = n_precaution,
                     source = src, created = as.character(Sys.time())))
}
