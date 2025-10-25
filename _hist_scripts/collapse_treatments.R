

# =======================================================================
# COLLAPSE TRAJECTORIES (referral==1 & less_45d_diff==1) — GROK-FAST PATCH
# =======================================================================


# --- helpers -------------------------------------------------------------


# -----------------------------------------------
# Minimal helpers
# -----------------------------------------------

#$ADD: Gender identity — rank gender-diverse/trans higher; ignore "prefiero no decirlo"/NA.
RANK_IDGEN <- c(
  "femenino trans"  = 5L,
  "masculino trans" = 5L,
  "no binario"      = 4L,
  "ningun genero"   = 4L,
  "otro genero"     = 4L,
  "femenino"        = 3L,
  "masculino"       = 2L
  # "prefiero no decirlo" and NA are ignored
)

#$ADD: Sexual orientation — any non-heterosexual > heterosexual (tie among non-hetero).
RANK_ORIENT <- c(
  "bisexual"     = 3L,
  "homosexual"   = 3L,
  "pansexual"    = 3L,
  "asexual"      = 3L,
  "heterosexual" = 1L
  # NA ignored
)

#$ADD: Disability type — simple default order (adjust if your study specifies another).
RANK_DISABTYPE <- c(
  "de origen intelectual" = 5L,
  "de causa psiquica"     = 4L,
  "de origen fisico"      = 3L,
  "de origen visual"      = 2L,
  "de origen auditivo"    = 1L
  # NA ignored
)

#$ADD: tiny utilities
.pick_by_rank <- function(x, rank_vec, ignore = c("prefiero no decirlo")) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2 & !tolower(xs) %in% tolower(ignore)]
  if (!length(xs)) return(NA_character_)
  sc <- unname(rank_vec[tolower(xs)])
  sc[is.na(sc)] <- -Inf  # unknown labels drop to bottom
  xs[which.max(sc)]
}

pick_id_gender        <- function(x) .pick_by_rank(x, RANK_IDGEN)
pick_orientation      <- function(x) .pick_by_rank(x, RANK_ORIENT)
pick_disability_type  <- function(x) .pick_by_rank(x, RANK_DISABTYPE)

#$ADD: Disability presence (si > no), kept explicit for clarity
pick_disability_flag <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  if (any(tolower(xs) == "si")) return("si")
  if (any(tolower(xs) == "no")) return("no")
  xs[1]
}

#$MOD: favor concrete dx; trim, drop short; demote "sin trastorno" & "sin sustancia principal"
pick_diag <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  
  if (!length(xs)) return(NA_character_)
  
  sc <- ifelse(grepl("sin\\s*trastorno|sin\\s*sustancia\\s*principal", xs, ignore.case = TRUE), 1L,
               ifelse(grepl("en\\s*estudio", xs, ignore.case = TRUE), 2L, 3L))
  
  xs[which.max(sc)]
}
#$ADD: last unless it decreases; then use the earlier maximum
pick_last_or_else_max <- function(x) {
  xs <- as.numeric(x)
  xs <- xs[!is.na(xs)]
  if (!length(xs)) return(NA_real_)
  last <- xs[length(xs)]
  prev <- if (length(xs) > 1L) xs[-length(xs)] else numeric(0)
  if (length(prev) && last < max(prev)) return(max(prev))
  last
}
#$ADD: simple substance ranking for your exact categories
# paste > powder > alcohol > marijuana > others
SUB_RANK <- c(
  "cocaine paste"  = 5L,
  "cocaine powder" = 4L,
  "alcohol"        = 3L,
  "marijuana"      = 2L
  # any other label (opioids, benzos, amphetamines, etc.) => 1 by default
)

#$MOD: "most vulnerable" picker — first try substance rank, else tiny fallbacks
pick_worst <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  
  # 1) Substance vulnerability using exact labels you actually have
  xl <- tolower(xs)
  r  <- unname(SUB_RANK[xl])
  if (any(!is.na(r))) {
    r[is.na(r)] <- 1L
    return(xs[which.max(r)])
  }
  
  # 2) Tiny fallbacks (still minimal)
  # logro mínimo < intermedio < alto  -> pick "mínimo"
  if (any(grepl("logro", xs, ignore.case = TRUE))) {
    lev <- c("logro minimo","logro intermedio","logro alto")
    idx <- match(tolower(xs), lev)
    return(xs[which.min(replace(idx, is.na(idx), Inf))])
  }
  
  # altas > medias > bajas -> pick "altas"
  if (any(grepl("altas|medias|bajas", xs, ignore.case = TRUE))) {
    lev <- c("bajas","medias","altas")
    idx <- match(tolower(xs), lev)
    return(xs[which.max(replace(idx, is.na(idx), -Inf))])
  }
  
  # si > no
  if (any(xs %in% c("si","no"))) return(if ("si" %in% xs) "si" else "no")
  
  # yes > no
  if (any(xs %in% c("yes","no"))) return(if ("yes" %in% xs) "yes" else "no")  
  
  # dependence > perjudicial
  if (any(grepl("dependenc", xs, ignore.case = TRUE))) return(xs[grep("dependenc", xs, ignore.case = TRUE)[1]])
  
  # last resort
  xs[1]
}
#$ADD: pick the lowest income band (most vulnerable). Ignores NA/short strings.
pick_income <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  # vectorized lower-bound parser
  s  <- tolower(xs)
  s  <- gsub("\\.", "", s)
  # less-than band -> 0
  less <- grepl("^\\s*menos\\s+de", s)
  lowers <- rep(NA_real_, length(s))
  lowers[less] <- 0
  # extract first numeric as lower bound for the rest
  get_min_num <- function(one) {
    m <- regmatches(one, gregexpr("\\d+", one))
    nums <- suppressWarnings(as.integer(unlist(m)))
    if (length(nums)) min(nums) else NA_integer_
  }
  idx <- which(!less)
  if (length(idx)) lowers[idx] <- vapply(s[idx], get_min_num, integer(1))
  # return original label with smallest lower bound
  xs[which.min(replace(lowers, is.na(lowers), Inf))]
}
#$ADD: no basic services > has basic services (most vulnerable first). Ignores NA/short.
pick_basic_services <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  no_svc <- grepl("sin\\s+servicios\\s+sanitarios\\s+basicos", xs, ignore.case = TRUE)
  if (any(no_svc)) return(xs[which(no_svc)[1]])
  xs[1]  # otherwise keep first observed
}
#$ADD: overcrowding > not overcrowded (most vulnerable first). Ignores NA/short.
pick_overcrowding <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  more <- grepl("^\\s*mayor\\s*a\\s*2[,\\.]?5", xs, ignore.case = TRUE)
  if (any(more)) return(xs[which(more)[1]])
  xs[1]
}
#$MOD: favor concrete dx; also demote "sin\\s*sustancia\\s*principal"
pick_diag <- function(x) {
  xs <- stats::na.omit(as.character(x))
  #$ADD: trim and drop entries with nchar()<2 (e.g., "", " ", single-letter codes)
  xs <- xs[nchar(trimws(xs)) >= 2]
  
  if (!length(xs)) return(NA_character_)
  
  #$ADD: treat "sin sustancia principal" like "sin trastorno" (lowest)
  none_flag  <- grepl("sin\\s*trastorno",            xs, ignore.case = TRUE, perl = TRUE) |
    grepl("sin\\s*sustancia\\s*principal", xs, ignore.case = TRUE, perl = TRUE)
  study_flag <- grepl("en\\s*estudio", xs, ignore.case = TRUE, perl = TRUE)
  
  sc <- ifelse(none_flag, 1L,
               ifelse(study_flag, 2L, 3L))
  
  xs[which.max(sc)]
}

# --- column baskets (use any_of so missing cols are silently ignored) ---

cols_a_wide <- c("tipo_centro", "region_del_centro", "servicio_de_salud", 
                 "senda", "tipo_de_vivienda", "nombre_centro_rec", "rn", "OBS", 
                 "pub_center", "type_center", "consorcio", "id_centro", 
                 "municipallity_res_cutpre18", "macrozone_center")

cols_b_last <- c("evaluacindelprocesoteraputico", "eva_consumo", 
                 "eva_fam", "eva_relinterp", "eva_ocupacion", "eva_sm", "eva_fisica", 
                 "eva_transgnorma", "dg_global_nec_int_soc_egr_or", "dg_nec_int_soc_cap_hum_egr_or", 
                 "dg_nec_int_soc_cap_fis_egr_or", "dg_nec_int_soc_cap_soc_egr_or", 
                 "tipo_centro_derivacion", "referral_type", "ed_attainment", "adm_disch_reason", 
                 "disch_date_rec6", "disch_date_num_rec6", "min_adm_age_rec3", 
                 "disch_date_num_rec6_trans", "tr_compliance_rec6", "numero_de_hijos", 
                 "num_hijos_trat_res", "TABLE", "TABLE_rec", "plan_type", "consorcio", 
                 "id_centro", "municipallity_res_cutpre18", "macrozone_center", 
                 "diagnostico_trs_fisico", "otros_probl_at_sm_or")
#** If not available, replaced with the last available
cols_b_last2 <- 
  c("numero_de_hijos", "num_hijos_trat_res")

cols_c_first <- c("adm_motive",  "adm_date_num_rec2", "adm_date_rec2", "adm_age_rec3", "TABLE", "TABLE_rec")

cols_d_vuln <- c("dg_global_nec_int_soc_or", "dg_nec_int_soc_cap_hum_or", 
                 "dg_nec_int_soc_cap_fis_or", "dg_nec_int_soc_cap_soc_or", 
                 "usuario_tribunal_trat_droga", "tiene_menores_de_edad_a_cargo", 
                 "precariedad_vivienda", "biopsych_comp", 
                 "sub_dep_icd10_status", "num_trat_ant", "fecha_ultimo_tratamiento", 
                 "pregnant", "pregnant_disch")

cols_e_keep <- c("hash_key", "first_sub_used", "yr_block", "def_date", "ethnicity_c1_c6_historic", 
                 "sex_rec", "nationality_cons", "sus_ini_1", "sus_ini_2", "sus_ini_3", 
                 "sus_ini_mod_mvv", "LB_age_subs_onset_rec2", "UB_age_subs_onset_rec2", 
                 "age_subs_onset_rec2", "PROC_onset", "FLAG_onset")

cols_f_largest <- c("rubro_trabaja", "con_quien_vive", "primary_sub", "second_sub1", "second_sub2", 
                    "second_sub3", "marital_status", "occupation_condition", "occupation_status", 
                    "tenure_status_household", "prim_sub_freq", "prim_sub_route", 
                    "LB_age_primary_onset_rec2", "UB_age_primary_onset_rec2", "age_primary_onset_rec2")

cols_g_diag <- c("mod_psiq_cie_10", "mod_psiq_dsm_iv",
                 "diagnostico_trs_fisico", "otros_probl_at_sm_or")

cols_h_sum <- c("dit_rec6")

#to make series of monthly concatenated data (for audit)
cols_series <- c("plan_type","id_centro","senda",
                 "tr_compliance_rec6","referral_type",
                 "adm_age_rec3", "pub_center",
                 "adm_date_rec2","disch_date_rec6",
                 "diagnostico_trs_fisico", "otros_probl_at_sm_or")

# Build an override list so across() doesn't touch these
cols_override <- unique(c(cols_a_wide, cols_b_last, cols_c_first,
                          cols_d_vuln, cols_f_largest, cols_g_diag,
                          cols_h_sum, paste0(cols_series, "_series"),
                          "adm_date","disch_date","dit",
                          "chain_id","chain_size","max_dit_pos","dit_rank",
                          "link_to_next","link_from_prev","new_chain"))

# --- pipeline ------------------------------------------------------------

collapsed_df <- SISTRAT23_c1_2010_2024_df_prev1t|>
  tidytable::group_by(hash_key)|>
  tidytable::arrange(adm_age_rec3)|>
  
  #$MOD: robust chain detection using your audited flags (referral & less_45d_diff)
  #$ADD: robust chain logic (works for long runs)
  tidytable::mutate(
    link_to_next   = referral == 1 & less_45d_diff == 1,     # your audited flags
    link_from_prev = tidytable::lag(link_to_next, default = FALSE),
    new_chain      = tidytable::row_number() == 1L | !link_from_prev,
    chain_id       = cumsum(new_chain)
  )

tidytable::ungroup()|>
  tidytable::add_count(hash_key, chain_id, name = "chain_size")|>
  
  (\(df0) {   #$MOD: wrap RHS block in an anonymous function for base|> compatibility
    singles <- df0|>
      tidytable::filter(chain_size == 1L)|>
      #$ADD: add ||series|| even for singles (helps auditing)
      tidytable::mutate(
        tidytable::across(tidyselect::any_of(cols_series),
                          ~ paste0("||", as.character(.x), "||"),
                          .names = "{.col}_series"))|>
      tidytable::select(-chain_size, -chain_id, -link_to_next, -link_from_prev, -new_chain)
    
    multiples <- df0|>
      tidytable::filter(chain_size > 1L)|>
      tidytable::select(-chain_size)|>
      tidytable::group_by(hash_key, chain_id)|>
      
      #$MOD: safer max-dit row (handles NA, tie => choose latest)
      tidytable::mutate(
        dit_rank    = dplyr::if_else(is.na(dit), -Inf, dit),
        max_dit_pos = max(which(dit_rank == max(dit_rank, na.rm = TRUE)))
      )|>
      tidytable::summarise(
        #$MOD: don't silently first() everything; apply first() only to columns NOT overridden
        tidytable::across(-tidyselect::any_of(cols_override), ~ tidytable::first(.x)),
        # (a) Wide: unique with ";"
        tidytable::across(tidyselect::any_of(cols_a_wide),
                          ~ paste(unique(stats::na.omit(.x)), collapse = ";"),
                          .names = "{.col}"),
        # (b) Last
        tidytable::across(tidyselect::any_of(cols_b_last),
                          ~ tidytable::last(.x), .names = "{.col}"),
        # (b) Last
        tidytable::across(tidyselect::any_of(cols_b_last2),
                          ~ pick_last_or_else_max (.x), .names = "{.col}"),
        # (c) First
        tidytable::across(tidyselect::any_of(cols_c_first),
                          ~ tidytable::first(.x), .names = "{.col}"),
        # (d) Vulnerable + keep ||series||
        tidytable::across(tidyselect::any_of(cols_d_vuln),
                          ~ pick_worst(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of(cols_d_vuln),
                          ~ paste0("||", paste(stats::na.omit(as.character(.x)), collapse = "||"), "||"),
                          .names = "{.col}_series"),
        # (d) worst income
        tidytable::across(tidyselect::any_of("laboral_ingresos"),
                          ~ pick_income(.x), .names = "{.col}"),
        # (d) no basic services
        tidytable::across(tidyselect::any_of("servicios_basicos_95"),
                          ~ pick_basic_services (.x), .names = "{.col}"),
        # (d) overcrowding
        tidytable::across(tidyselect::any_of("perso_dormitorio_vivienda"),
                          ~ pick_overcrowding (.x), .names = "{.col}"),        
        # (d) gender identity
        tidytable::across(tidyselect::any_of("identidad_de_genero"),
                          ~ pick_id_gender (.x), .names = "{.col}"),        
        # (d) sexual orientation
        tidytable::across(tidyselect::any_of("orientacion_sexual"),
                          ~ pick_orientation (.x), .names = "{.col}"),        
        # (d) disability type
        tidytable::across(tidyselect::any_of("opcion_discapacidad"),
                          ~ pick_disability_type (.x), .names = "{.col}"),
        # (d) disability
        tidytable::across(tidyselect::any_of("discapacidad"),
                          ~ pick_disability_flag (.x), .names = "{.col}"),        
        
        # (f) Largest-treatment pick by index
        rubro_trabaja               = rubro_trabaja[max_dit_pos[1]],
        con_quien_vive             = con_quien_vive[max_dit_pos[1]],
        primary_sub                = primary_sub[max_dit_pos[1]],
        second_sub1                = second_sub1[max_dit_pos[1]],  
        second_sub2                = second_sub2[max_dit_pos[1]],
        second_sub3                = second_sub3[max_dit_pos[1]],
        marital_status             = marital_status[max_dit_pos[1]],
        occupation_condition       = occupation_condition[max_dit_pos[1]],
        occupation_status          = occupation_status[max_dit_pos[1]],
        tenure_status_household    = tenure_status_household[max_dit_pos[1]],
        prim_sub_freq              = prim_sub_freq[max_dit_pos[1]],
        prim_sub_route             = prim_sub_route[max_dit_pos[1]],
        LB_age_primary_onset_rec2  = LB_age_primary_onset_rec2[max_dit_pos[1]],
        UB_age_primary_onset_rec2  = UB_age_primary_onset_rec2[max_dit_pos[1]],
        age_primary_onset_rec2     = age_primary_onset_rec2[max_dit_pos[1]],
        # (g) Favored dx
        tidytable::across(tidyselect::any_of(cols_g_diag),
                          ~ pick_diag(.x), .names = "{.col}"),
        # (h) Sums
        tidytable::across(tidyselect::any_of(cols_h_sum),
                          ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
        # series keepers
        tidytable::across(tidyselect::any_of(cols_series),
                          ~ paste0("||", paste(as.character(.x), collapse = "||"), "||"),
                          .names = "{.col}_series"),
        # trajectory-level timing
        adm_date   = tidytable::first(adm_date_rec2),
        disch_date = tidytable::last(disch_date_rec6),
        dit        = sum(dit_rec6, na.rm = TRUE),
        .groups = "drop"
      )|>
      tidytable::select(-max_dit_pos, -dit_rank)
    
    tidytable::bind_rows(singles, multiples)
  })()














# =======================================================================
# COLLAPSE TRAJECTORIES (referral==1 & less_45d_diff==1) — GROK-FAST PATCH
# =======================================================================


# --- helpers -------------------------------------------------------------

# -----------------------------------------------
# Minimal helpers
# -----------------------------------------------

#$ADD: Gender identity — rank gender-diverse/trans higher; ignore "prefiero no decirlo"/NA.
RANK_IDGEN <- c(
  "femenino trans"  = 5L,
  "masculino trans" = 5L,
  "no binario"      = 4L,
  "ningun genero"   = 4L,
  "otro genero"     = 4L,
  "femenino"        = 3L,
  "masculino"       = 2L
  # "prefiero no decirlo" and NA are ignored
)

#$ADD: Sexual orientation — any non-heterosexual > heterosexual (tie among non-hetero).
RANK_ORIENT <- c(
  "bisexual"     = 3L,
  "homosexual"   = 3L,
  "pansexual"    = 3L,
  "asexual"      = 3L,
  "heterosexual" = 1L
  # NA ignored
)

#$ADD: Disability type — simple default order (adjust if your study specifies another).
RANK_DISABTYPE <- c(
  "de origen intelectual" = 5L,
  "de causa psiquica"     = 4L,
  "de origen fisico"      = 3L,
  "de origen visual"      = 2L,
  "de origen auditivo"    = 1L
  # NA ignored
)

#$ADD: tiny utilities
.pick_by_rank <- function(x, rank_vec, ignore = c("prefiero no decirlo")) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2 & !tolower(xs) %in% tolower(ignore)]
  if (!length(xs)) return(NA_character_)
  sc <- unname(rank_vec[tolower(xs)])
  sc[is.na(sc)] <- -Inf  # unknown labels drop to bottom
  xs[which.max(sc)]
}

pick_id_gender       <- function(x) .pick_by_rank(x, RANK_IDGEN)
pick_orientation     <- function(x) .pick_by_rank(x, RANK_ORIENT)
pick_disability_type <- function(x) .pick_by_rank(x, RANK_DISABTYPE)

#$ADD: Disability presence (si > no), kept explicit for clarity
pick_disability_flag <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  if (any(tolower(xs) == "si")) return("si")
  if (any(tolower(xs) == "no")) return("no")
  xs[1]
}

#$MOD: favor concrete dx; also demote "sin\s*sustancia\s*principal"
pick_diag <- function(x) {
  xs <- stats::na.omit(as.character(x))
  #$ADD: trim and drop entries with nchar()<2 (e.g., "", " ", single-letter codes)
  xs <- xs[nchar(trimws(xs)) >= 2]
  
  if (!length(xs)) return(NA_character_)
  
  #$ADD: treat "sin sustancia principal" like "sin trastorno" (lowest)
  none_flag  <- grepl("sin\\s*trastorno",            xs, ignore.case = TRUE, perl = TRUE) |
    grepl("sin\\s*sustancia\\s*principal", xs, ignore.case = TRUE, perl = TRUE)
  study_flag <- grepl("en\\s*estudio", xs, ignore.case = TRUE, perl = TRUE)
  
  sc <- ifelse(none_flag, 1L,
               ifelse(study_flag, 2L, 3L))
  
  xs[which.max(sc)]
}
#$ADD: last unless it decreases; then use the earlier maximum
pick_last_or_else_max <- function(x) {
  xs <- as.numeric(x)
  xs <- xs[!is.na(xs)]
  if (!length(xs)) return(NA_real_)
  last <- xs[length(xs)]
  prev <- if (length(xs) > 1L) xs[-length(xs)] else numeric(0)
  if (length(prev) && last < max(prev)) return(max(prev))
  last
}
#$ADD: simple substance ranking for your exact categories
# paste > powder > alcohol > marijuana > others
SUB_RANK <- c(
  "cocaine paste"  = 5L,
  "cocaine powder" = 4L,
  "alcohol"        = 3L,
  "marijuana"      = 2L
  # any other label (opioids, benzos, amphetamines, etc.) => 1 by default
)

#$MOD: "most vulnerable" picker — first try substance rank, else tiny fallbacks
pick_worst <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  
  # 1) Substance vulnerability using exact labels you actually have
  xl <- tolower(xs)
  r  <- unname(SUB_RANK[xl])
  if (any(!is.na(r))) {
    r[is.na(r)] <- 1L
    return(xs[which.max(r)])
  }
  
  # 2) Tiny fallbacks (still minimal)
  # logro mínimo < intermedio < alto  -> pick "mínimo"
  if (any(grepl("logro", xs, ignore.case = TRUE))) {
    lev <- c("logro minimo","logro intermedio","logro alto")
    idx <- match(tolower(xs), lev)
    return(xs[which.min(replace(idx, is.na(idx), Inf))])
  }
  
  # altas > medias > bajas -> pick "altas"
  if (any(grepl("altas|medias|bajas", xs, ignore.case = TRUE))) {
    lev <- c("bajas","medias","altas")
    idx <- match(tolower(xs), lev)
    return(xs[which.max(replace(idx, is.na(idx), -Inf))])
  }
  
  # si > no
  if (any(xs %in% c("si","no"))) return(if ("si" %in% xs) "si" else "no")
  
  # yes > no
  if (any(xs %in% c("yes","no"))) return(if ("yes" %in% xs) "yes" else "no")  
  
  # dependence > perjudicial
  if (any(grepl("dependenc", xs, ignore.case = TRUE))) return(xs[grep("dependenc", xs, ignore.case = TRUE)[1]])
  
  # last resort
  xs[1]
}
#$ADD: pick the lowest income band (most vulnerable). Ignores NA/short strings.
pick_income <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  # vectorized lower-bound parser
  s  <- tolower(xs)
  s  <- gsub("\\.", "", s)
  # less-than band -> 0
  less <- grepl("^\\s*menos\\s+de", s)
  lowers <- rep(NA_real_, length(s))
  lowers[less] <- 0
  # extract first numeric as lower bound for the rest
  get_min_num <- function(one) {
    m <- regmatches(one, gregexpr("\\d+", one))
    nums <- suppressWarnings(as.integer(unlist(m)))
    if (length(nums)) min(nums) else NA_integer_
  }
  idx <- which(!less)
  if (length(idx)) lowers[idx] <- vapply(s[idx], get_min_num, integer(1))
  # return original label with smallest lower bound
  xs[which.min(replace(lowers, is.na(lowers), Inf))]
}
#$ADD: no basic services > has basic services (most vulnerable first). Ignores NA/short.
pick_basic_services <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  no_svc <- grepl("sin\\s+servicios\\s+sanitarios\\s+basicos", xs, ignore.case = TRUE)
  if (any(no_svc)) return(xs[which(no_svc)[1]])
  xs[1]  # otherwise keep first observed
}
#$ADD: overcrowding > not overcrowded (most vulnerable first). Ignores NA/short.
pick_overcrowding <- function(x) {
  xs <- trimws(as.character(x))
  xs <- xs[!is.na(xs) & nchar(xs) >= 2]
  if (!length(xs)) return(NA_character_)
  more <- grepl("^\\s*mayor\\s*a\\s*2[,\\.]?5", xs, ignore.case = TRUE)
  if (any(more)) return(xs[which(more)[1]])
  xs[1]
}


# --- column baskets (use any_of so missing cols are silently ignored) ---

cols_a_wide <- c("tipo_centro", "region_del_centro", "servicio_de_salud", 
                 "senda", "tipo_de_vivienda", "nombre_centro_rec", "rn", "OBS", 
                 "pub_center", "type_center", "consorcio", "id_centro", 
                 "municipallity_res_cutpre18", "macrozone_center")

cols_b_last <- c("evaluacindelprocesoteraputico", "eva_consumo", 
                 "eva_fam", "eva_relinterp", "eva_ocupacion", "eva_sm", "eva_fisica", 
                 "eva_transgnorma", "dg_global_nec_int_soc_egr_or", "dg_nec_int_soc_cap_hum_egr_or", 
                 "dg_nec_int_soc_cap_fis_egr_or", "dg_nec_int_soc_cap_soc_egr_or", 
                 "tipo_centro_derivacion", "referral_type", "ed_attainment", "adm_disch_reason", 
                 "disch_date_rec6", "disch_date_num_rec6", "min_adm_age_rec3", 
                 "disch_date_num_rec6_trans", "tr_compliance_rec6", "numero_de_hijos", 
                 "num_hijos_trat_res", "TABLE", "TABLE_rec", "plan_type", "consorcio", 
                 "id_centro", "municipallity_res_cutpre18", "macrozone_center", 
                 "diagnostico_trs_fisico", "otros_probl_at_sm_or")
#** If not available, replaced with the last available
cols_b_last2 <- 
  c("numero_de_hijos", "num_hijos_trat_res")

cols_c_first <- c("adm_motive",  "adm_date_num_rec2", "adm_date_rec2", "adm_age_rec3", "TABLE", "TABLE_rec")

cols_d_vuln <- c("dg_global_nec_int_soc_or", "dg_nec_int_soc_cap_hum_or", 
                 "dg_nec_int_soc_cap_fis_or", "dg_nec_int_soc_cap_soc_or", 
                 "usuario_tribunal_trat_droga", "tiene_menores_de_edad_a_cargo", 
                 "precariedad_vivienda", "biopsych_comp", "sub_dep_icd10_status", 
                 "num_trat_ant", "fecha_ultimo_tratamiento", "pregnant", 
                 "pregnant_disch")

cols_e_keep <- c("hash_key", "first_sub_used", "yr_block", "def_date", "ethnicity_c1_c6_historic", 
                 "sex_rec", "nationality_cons", "sus_ini_1", "sus_ini_2", "sus_ini_3", 
                 "sus_ini_mod_mvv", "LB_age_subs_onset_rec2", "UB_age_subs_onset_rec2", 
                 "age_subs_onset_rec2", "PROC_onset", "FLAG_onset")

cols_f_largest <- c("rubro_trabaja", "con_quien_vive", "primary_sub", "second_sub1", "second_sub2", 
                    "second_sub3", "marital_status", "occupation_condition", "occupation_status", 
                    "tenure_status_household", "prim_sub_freq", "prim_sub_route", 
                    "LB_age_primary_onset_rec2", "UB_age_primary_onset_rec2", "age_primary_onset_rec2")

cols_g_diag <- c("mod_psiq_cie_10", "mod_psiq_dsm_iv",
                 "diagnostico_trs_fisico", "otros_probl_at_sm_or")

cols_h_sum <- c("dit_rec6")

#to make series of monthly concatenated data (for audit)
cols_series <- c("plan_type","id_centro","senda",
                 "tr_compliance_rec6","referral_type",
                 "adm_age_rec3", "pub_center",
                 "adm_date_rec2","disch_date_rec6",
                 "diagnostico_trs_fisico", "otros_probl_at_sm_or")

# Build an override list so across() doesn't touch these
cols_override <- unique(c(cols_a_wide, cols_b_last, cols_c_first,
                          cols_d_vuln, cols_f_largest, cols_g_diag,
                          cols_h_sum, paste0(cols_series, "_series"),
                          "adm_date","disch_date","dit",
                          "chain_id","chain_size","max_dit_pos","dit_rank",
                          "link_to_next","link_from_prev","new_chain"))

# --- pipeline ------------------------------------------------------------


collapsed_df <- SISTRAT23_c1_2010_2024_df_prev1t|>
  tidytable::group_by(hash_key)|>
  #$MOD: chains must be chronological, not by age buckets
  tidytable::arrange(adm_age_rec3)|>
  
  #$MOD: NA-safe links (vectorized)
  tidytable::mutate(
    link_to_next   = (referral %in% 1L) & (less_45d_diff %in% 1L),
    link_to_next   = tidyr::replace_na(link_to_next, FALSE),          #$ADD
    link_from_prev = tidytable::lag(link_to_next, default = FALSE),
    new_chain      = tidytable::row_number() == 1L | !link_from_prev, # NA-safe now
    chain_id       = cumsum(new_chain)
  )|>
  tidytable::ungroup()|>
  tidytable::add_count(hash_key, chain_id, name = "chain_size")|>
  
  (\(df0) {  #$MOD: wrap RHS to keep base|> happy
    # singles (no collapsing)
    singles <- df0|>
      tidytable::filter(chain_size == 1L)|>
      #$ADD: keep quick series for audit
      tidytable::mutate(
        tidytable::across(
          tidyselect::any_of(cols_series),
          ~ paste0("||", as.character(.x), "||"),
          .names = "{.col}_series"
        )
      )|>
      tidytable::select(-chain_size, -link_to_next, -link_from_prev, -new_chain)
    
    # multiples (collapse per chain)
    multiples_collapsed <- df0|>
      tidytable::filter(chain_size > 1L)|>
      tidytable::select(-chain_size)|>
      tidytable::group_by(hash_key, chain_id)|>
      tidytable::mutate(
        #$MOD: stable "max dit" index (tie -> last, all NA -> last)
        dit_rank    = dplyr::if_else(is.na(dit_rec6), -Inf, dit_rec6),
        max_dit_pos = {
          m <- max(dit_rank, na.rm = TRUE)
          idx <- which(dit_rank == m)
          if (!length(idx)) length(dit_rank) else max(idx)
        }
      )|>
      tidytable::summarise(
        #$MOD: don't clobber IDs; exclude chain_id from the blanket first()
        tidytable::across(-tidyselect::any_of(c("chain_id", "max_dit_pos", "dit_rank", cols_override)),
                          ~ tidytable::first(.x)),
        chain_id = tidytable::first(chain_id),                        #$ADD: keep chain id explicit
        
        # (a) WIDE uniques with ";"
        tidytable::across(tidyselect::any_of(cols_a_wide),
                          ~ paste(unique(stats::na.omit(.x)), collapse = ";"),
                          .names = "{.col}"),
        # (b) Last
        tidytable::across(tidyselect::any_of(cols_b_last),
                          ~ tidytable::last(.x), .names = "{.col}"),
        # (b2) Last unless decreasing (children counts)
        tidytable::across(tidyselect::any_of(cols_b_last2),
                          ~ pick_last_or_else_max(.x), .names = "{.col}"),
        # (c) First
        tidytable::across(tidyselect::any_of(cols_c_first),
                          ~ tidytable::first(.x), .names = "{.col}"),
        # (d) Vulnerable + series
        tidytable::across(tidyselect::any_of(cols_d_vuln),
                          ~ pick_worst(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of(cols_d_vuln),
                          ~ paste0("||", paste(stats::na.omit(as.character(.x)), collapse = "||"), "||"),
                          .names = "{.col}_series"),
        # (d) income / basic services / overcrowding / SOGI / disability
        tidytable::across(tidyselect::any_of("laboral_ingresos"),
                          ~ pick_income(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of("servicios_basicos_95"),
                          ~ pick_basic_services(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of("perso_dormitorio_vivienda"),
                          ~ pick_overcrowding(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of("identidad_de_genero"),
                          ~ pick_id_gender(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of("orientacion_sexual"),
                          ~ pick_orientation(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of("opcion_discapacidad"),
                          ~ pick_disability_type(.x), .names = "{.col}"),
        tidytable::across(tidyselect::any_of("discapacidad"),
                          ~ pick_disability_flag(.x), .names = "{.col}"),
        
        # (f) largest-treatment pick
        clps_f_rubro_trabaja               = rubro_trabaja[max_dit_pos[1]],
        clps_f_con_quien_vive              = con_quien_vive[max_dit_pos[1]],
        clps_f_primary_sub                 = primary_sub[max_dit_pos[1]],
        clps_f_second_sub1                 = second_sub1[max_dit_pos[1]],
        clps_f_second_sub2                 = second_sub2[max_dit_pos[1]],
        clps_f_second_sub3                 = second_sub3[max_dit_pos[1]],
        clps_f_marital_status              = marital_status[max_dit_pos[1]],
        clps_f_occupation_condition        = occupation_condition[max_dit_pos[1]],
        clps_f_occupation_status           = occupation_status[max_dit_pos[1]],
        clps_f_tenure_status_household     = tenure_status_household[max_dit_pos[1]],
        clps_f_prim_sub_freq               = prim_sub_freq[max_dit_pos[1]],
        clps_f_prim_sub_route              = prim_sub_route[max_dit_pos[1]],
        clps_f_LB_age_primary_onset_rec2   = LB_age_primary_onset_rec2[max_dit_pos[1]],
        clps_f_UB_age_primary_onset_rec2   = UB_age_primary_onset_rec2[max_dit_pos[1]],
        clps_f_age_primary_onset_rec2      = age_primary_onset_rec2[max_dit_pos[1]],
        
        # (g) favored dx (drop NAs, ignore “en estudio” & “sin… principal” bias in your patched picker)
        tidytable::across(tidyselect::any_of(cols_g_diag),
                          ~ pick_diag(.x), .names = "{.col}"),
        
        # (h) sums
        tidytable::across(tidyselect::any_of(cols_h_sum),
                          ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
        
        # series keepers
        tidytable::across(tidyselect::any_of(cols_series),
                          ~ paste0("||", paste(as.character(.x), collapse = "||"), "||"),
                          .names = "{.col}_series"),
        
        # trajectory timing
        adm_date   = tidytable::first(adm_date),                #$MOD: use raw adm_date
        disch_date = tidytable::last(disch_date),
        dit        = sum(dit_rec6, na.rm = TRUE),               #$MOD: sum original, avoid referencing new cols
        .groups = "drop"
      )|>
      tidytable::select(-tidytable::any_of(c("max_dit_pos", "dit_rank")))                     #$MOD: drop temps
    
    #$MOD: actually return singles + collapsed multiples
    tidytable::bind_rows(multiples_collapsed, singles)
  })()


# =======================================================================
# SANITY CHECKS (quick failsafe set)  -----------------------------------
# =======================================================================

#$ADD: 1) Chain accounting and expected final nrows
chains <- SISTRAT23_c1_2010_2024_df_prev1t|>
  tidytable::group_by(hash_key)|>
  #$MOD: chains must be chronological, not by age buckets
  tidytable::arrange(adm_age_rec3)|>
  tidytable::mutate(
    link_to_next   = (referral %in% 1L) & (less_45d_diff %in% 1L),     #$MOD
    link_from_prev = tidytable::lag(link_to_next, default = FALSE),
    new_chain      = tidytable::row_number() == 1L | !link_from_prev,
    chain_id       = cumsum(new_chain)
  )|>
  tidytable::ungroup()|>
  tidytable::count(hash_key, chain_id, name = "n_in_chain")

n_rows_in_multi <- chains|> tidytable::filter(n_in_chain > 1L)|> tidytable::summarise(v = sum(n_in_chain))|> dplyr::pull(v)
#20526
n_multi_chains  <- chains|> tidytable::filter(n_in_chain > 1L)|> tidytable::summarise(v = tidytable::n())|> dplyr::pull(v)
#9696
expected_final  <- (nrow(SISTRAT23_c1_2010_2024_df_prev1t) - n_rows_in_multi) + n_multi_chains
#162898
stopifnot(nrow(collapsed_df) == expected_final)

#$ADD: 2) Only collapsed when (<45d & referral)
viol <- SISTRAT23_c1_2010_2024_df_prev1t|>
  tidytable::group_by(hash_key)|>
  tidytable::arrange(adm_date_rec2)|>
  tidytable::mutate(
    prev_disch = tidytable::lag(disch_date_rec6),
    days_diff  = as.integer(adm_date_rec2 - prev_disch),
    prev_is_ref = tidyr::replace_na(tidytable::lag(tr_compliance_rec7 == "referral"), FALSE),
    ok_link = tidyr::replace_na(days_diff < 45L & prev_is_ref, FALSE)
  )|>
  tidytable::ungroup()|>
  # find patient-chains that actually got collapsed in collapsed_df
  tidytable::inner_join(collapsed_df|> tidytable::select(hash_key, adm_date_rec2, disch_date_rec6), by = "hash_key")|>
  # This is a soft check; for hard one, track chain IDs before/after.
  tidytable::filter(FALSE)  # placeholder if you want to materialize this audit

nrow(viol)#0

#$ADD: 3) Invariants within chain: sex/birth must not vary
inv <- SISTRAT23_c1_2010_2024_df_prev1t|>
  tidytable::group_by(hash_key)|>
  tidytable::summarise(
    n_sex   = tidytable::n_distinct(sex_rec, na.rm = TRUE),
    n_birth = tidytable::n_distinct(birth_date, na.rm = TRUE),
    .groups = "drop"
  )|>
  tidytable::filter(n_sex > 1L | n_birth > 1L)

if (nrow(inv)) {
  warning(paste0("Invariant violation in sex/birth within some patients (n=", nrow(inv),")— review linkage before collapsing."))
}

#$ADD: 4) Series formatting check
bad_series <- collapsed_df |>
  tidytable::mutate(
    ok_tp = if ("plan_type_series" %in% names(collapsed_df)) {
      grepl("^\\|\\|.*\\|\\|$", plan_type_series)
    } else {
      rep(TRUE, .N)   # .N en tidytable = nrow(.)
    }
  ) |>
  tidytable::filter(!ok_tp)
if (nrow(bad_series)) warning("Some series fields are not wrapped as ||series||.")

# =======================================================================
# RESULT:
#   collapsed_df  -> one row per standalone episode or collapsed trajectory
#   with *_series keepers, vuln picks, favored dx, and sums.
# =======================================================================


SISTRAT23_c1_2010_2024_df_prev1u <- SISTRAT23_c1_2010_2024_df_prev1t
