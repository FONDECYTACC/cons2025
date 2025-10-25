
mx_1x1_comp<-
  rbind.data.frame(cbind.data.frame(sex="male", mltper_1x1), 
                   cbind.data.frame(sex="female", fltper_1x1))
mx_1x1_comp$Age<- as.numeric(mx_1x1_comp$Age)
mx_1x1_comp_filt<-mx_1x1_comp[as.numeric(as.character(mx_1x1_comp$Year)) %in% years_followup,]
mx_1x1_comp_filt2<-mx_1x1_comp_filt[as.numeric(as.character(mx_1x1_comp_filt$Age)) %in% 
                                      min(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$adm_age_rec2_int):max(SISTRAT23_c1_2010_2022_df_prev1q_sel4a_surv$adm_age_rec2_int),]


clean_df |> 
  left_join(mortality[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey"))
#70064
#
clean_df |> 
left_join(mortality[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")) |> nrow()
#7065
invisible("+1 row added")

clean_df |> 
  left_join(mortality[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")) |> group_by(hash_key) |> 
  count() |> 
  filter(n>1)


mortality_deduplicated <- mortality |> 
  arrange(hashkey, ano_def, mes_def, dia_def) |>
  group_by(hashkey) |>
  slice_head(n = 1) |>
  ungroup()

mortality_deduplicated|> 
  filter(death_date>="2010-01-01", death_date<"2021-01-01")|> 
  filter(hashkey %in% clean_df$hash_key) |>   
  janitor::tabyl(chapter, show_na = T) |> 
  mutate(valid_percent= sprintf("%1.1f",100*valid_percent)) |> 
  pull(valid_percent)
mortality_deduplicated|> 
  filter(death_date>="2010-01-01", death_date<"2021-01-01")|> 
  filter(hashkey %in% clean_df$hash_key) |>   
  janitor::tabyl(category, show_na = T) |> 
  mutate(percent= sprintf("%1.1f",100*percent)) |> 
  pull(percent)
mortality_deduplicated|> 
  filter(death_date>="2010-01-01", death_date<"2021-01-01")|> 
  #filter(hashkey %in% clean_df$hash_key) |>   
  janitor::tabyl(category, show_na = T) |> 
  mutate(percent= sprintf("%1.1f",100*percent)) |> 
  pull(percent)

mortality_summary <- 
  mortality_deduplicated |> 
    filter(ano_def>=2010) |> 
    filter(!is.na(category)) |> 
    #filter(!grepl("Other causes",category)) |> 
    group_by(ano_def, sex_rec, edad_cant) |> 
    summarise(assaults=sum(category=="Assaults", na.rm=T),
              self_harm=sum(category=="Intentional self-harm", na.rm=T),
              other_causes=sum(category=="Other causes", na.rm=T), 
              other_ext_causes= sum(category=="Other external causes", na.rm=T), 
              transport_accidents= sum(grepl("Transport",category), na.rm=T)
              )

mx_1x1_comp_filt3 <- 
mx_1x1_comp_filt2|> 
  mutate(edad_cant_cat = dplyr::case_when(
    Age>= 18 & Age < 30 ~ "18-29",
    Age>= 30 & Age < 45 ~ "30-44",
    Age>= 45 & Age < 60 ~ "45-59",
    Age>= 60 & Age < 86 ~ "60+",
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ),
  sex_rec = dplyr::case_when(                 # optional renaming step
    sex == "male"   ~ "Male",
    sex == "female" ~ "Female",
    TRUE~ NA_character_),
  sex_rec= factor(sex_rec, levels = c("Male", "Female"))
  )|> 
  left_join(mortality_summary, by= c("Year"="ano_def", "sex_rec"= "sex_rec", "Age"= "edad_cant"))|> 
  group_by(Year, sex_rec, edad_cant_cat)|> 
  summarise(
    sum_assaults = sum(assaults),
    sum_self_harm = sum(self_harm),
    sum_other_causes = sum(other_causes),
    sum_other_ext_causes = sum(other_ext_causes),
    sum_transport_accidents = sum(transport_accidents),
    Lx_total = sum(Lx)  # Sumar los años-persona
  ) |> 
  mutate(haz_assaults= sum_assaults/Lx_total, 
         haz_self_harm= sum_self_harm/Lx_total, 
         haz_other_causes= sum_other_causes/Lx_total, 
         haz_other_ext_causes= sum_other_ext_causes/Lx_total, 
         haz_transport_accidents= sum_transport_accidents/Lx_total) 

mx_1x1_comp_filt3 <- 
  mx_1x1_comp_filt3|>
  mutate(agegroup = dplyr::case_when(
    grepl("18",edad_cant_cat)~ 18,
    grepl("30",edad_cant_cat)~ 30,
    grepl("45",edad_cant_cat)~ 45,
    grepl("60",edad_cant_cat)~ 60,
    TRUE ~ NA_real_  # Opcional: manejo de valores fuera de rango
  ))|> 
  rename("year"="Year", "sex"="sex_rec")

c_SISTRAT_c1_assaults <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                              status = category=="Assaults", 
                              birth = birth_date_rec, 
                              exit = death_date_rec, entry = disch_date_rec6,
                              #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                              breaks = list(per = seq(2010, 2021, by = 1), 
                                            #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                            age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                              aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_assaults_df <- 
popEpi::sir( coh.data = c_SISTRAT_c1_assaults, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
             ref.data = mx_1x1_comp_filt3, #this should be only for people with assaults
             ref.rate = 'haz_assaults', 
             adjust = c('agegroup','year','sex'), 
             EAR=T)

c_SISTRAT_c1_self_harm <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                 status = category=="Intentional self-harm", 
                                 birth = birth_date_rec, 
                                 exit = death_date_rec, entry = disch_date_rec6,
                                 #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                 breaks = list(per = seq(2010, 2021, by = 1), 
                                               #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                               age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                 aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_self_harm_df <- 
popEpi::sir( coh.data = c_SISTRAT_c1_self_harm, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
             ref.data = mx_1x1_comp_filt3, #this should be only for people with assaults
             ref.rate = 'haz_self_harm', 
             adjust = c('agegroup','year','sex'), 
             EAR=T)

c_SISTRAT_c1_transport <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                  status = category=="Transport accidents", 
                                  birth = birth_date_rec, 
                                  exit = death_date_rec, entry = disch_date_rec6,
                                  #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                  breaks = list(per = seq(2010, 2021, by = 1), 
                                                #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                  aggre = list(agegroup = age, year = per, sex= sex_rec))

sr_1_accidents_df <- 
popEpi::sir( coh.data = c_SISTRAT_c1_transport, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
             ref.data = mx_1x1_comp_filt3, #this should be only for people with assaults
             ref.rate = 'haz_transport_accidents', 
             adjust = c('agegroup','year','sex'), 
             EAR=T)

c_SISTRAT_c1_other_ext <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                  status = category=="Other external causes", 
                                  birth = birth_date_rec, 
                                  exit = death_date_rec, entry = disch_date_rec6,
                                  #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                  breaks = list(per = seq(2010, 2021, by = 1), 
                                                #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                  aggre = list(agegroup = age, year = per, sex= sex_rec))

sr_1_other_ext_df <- 
popEpi::sir( coh.data = c_SISTRAT_c1_other_ext, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
             ref.data = mx_1x1_comp_filt3, #this should be only for people with assaults
             ref.rate = 'haz_other_ext_causes', 
             adjust = c('agegroup','year','sex'), 
             EAR=T)

c_SISTRAT_c1_other <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                  status = category=="Other causes", 
                                  birth = birth_date_rec, 
                                  exit = death_date_rec, entry = disch_date_rec6,
                                  #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                  breaks = list(per = seq(2010, 2021, by = 1), 
                                                #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                  aggre = list(agegroup = age, year = per, sex= sex_rec))

sr_1_other_causes_df <- 
popEpi::sir( coh.data = c_SISTRAT_c1_other, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
             ref.data = mx_1x1_comp_filt3, #this should be only for people with assaults
             ref.rate = 'haz_other_causes', 
             adjust = c('agegroup','year','sex'), 
             EAR=T)


sir_assaults_df<- 
  cbind.data.frame(
    total= "Assaults/ Aggressions  (X85–Y09)",
    observed= round(sr_1_assaults_df$observed,0),
    pyrs= round(sr_1_assaults_df$pyrs,0),
    CMR_1000= do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(cmr_ci_phi(sr_1_assaults_df$observed, sr_1_assaults_df$pyrs, phi= 1))))),
    expected= round(sr_1_assaults_df$expected,0),
    SMR= do.call(sprintf,c("%.6f (%.6f–%.6f)",as.list(unlist(sir_ci_phi_improved(sr_1_assaults_df, phi= extract_phi(c_SISTRAT_c1_assaults))[ , 1:3])))),
    EAR= as.character(sprintf("%.2f",sr_1_assaults_df$EAR)), 
    phi=extract_phi(c_SISTRAT_c1_assaults))
sir_self_harm_df<- 
  cbind.data.frame(
    total= "Intentional self-harm (X60–X84)",
    observed= round(sr_1_self_harm_df$observed,0),
    pyrs= round(sr_1_self_harm_df$pyrs,0),
    CMR_1000= do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(cmr_ci_phi(sr_1_self_harm_df$observed, sr_1_self_harm_df$pyrs, phi= 1))))),
    expected= round(sr_1_self_harm_df$expected,0),
    SMR= do.call(sprintf,c("%.6f (%.6f–%.6f)",as.list(unlist(sir_ci_phi_improved(sr_1_self_harm_df, phi= extract_phi(c_SISTRAT_c1_self_harm))[ , 1:3])))),
    EAR= as.character(sprintf("%.2f",sr_1_self_harm_df$EAR)), 
    phi=extract_phi(c_SISTRAT_c1_self_harm))
sir_other_ext_df<- 
  cbind.data.frame(
    total= "Other external causes",
    observed= round(sr_1_other_ext_df$observed,0),
    pyrs= round(sr_1_other_ext_df$pyrs,0),
    CMR_1000= do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(cmr_ci_phi(sr_1_other_ext_df$observed, sr_1_other_ext_df$pyrs, phi= 1))))),
    expected= round(sr_1_other_ext_df$expected,0),
    SMR= do.call(sprintf,c("%.6f (%.6f–%.6f)",as.list(unlist(sir_ci_phi_improved(sr_1_other_ext_df, phi= extract_phi(c_SISTRAT_c1_other_ext))[ , 1:3])))),
    EAR= as.character(sprintf("%.2f",sr_1_other_ext_df$EAR)), 
    phi=extract_phi(c_SISTRAT_c1_other_ext))
sir_other_df<- 
  cbind.data.frame(
    total= "No external causes",
    observed= round(sr_1_other_causes_df$observed,0),
    pyrs= round(sr_1_other_causes_df$pyrs,0),
    CMR_1000= do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(cmr_ci_phi(sr_1_other_causes_df$observed, sr_1_other_causes_df$pyrs, phi= 1))))),
    expected= round(sr_1_other_causes_df$expected,0),
    SMR= do.call(sprintf,c("%.6f (%.6f–%.6f)",as.list(unlist(sir_ci_phi_improved(sr_1_other_causes_df, phi= extract_phi(c_SISTRAT_c1_other))[ , 1:3])))),
    EAR= as.character(sprintf("%.2f",sr_1_other_causes_df$EAR)), 
    phi=extract_phi(c_SISTRAT_c1_other))
sir_transport_df<- 
  cbind.data.frame(
    total= "Transport accidents",
    observed= round(sr_1_accidents_df$observed,0),
    pyrs= round(sr_1_accidents_df$pyrs,0),
    CMR_1000= do.call(sprintf,c("%.1f (%.1f–%.1f)",as.list(unlist(cmr_ci_phi(sr_1_accidents_df$observed, sr_1_accidents_df$pyrs, phi= 1))))),
    expected= round(sr_1_accidents_df$expected,0),
    SMR= do.call(sprintf,c("%.6f (%.6f–%.6f)",as.list(unlist(sir_ci_phi_improved(sr_1_accidents_df, phi= extract_phi(c_SISTRAT_c1_transport))[ , 1:3])))),
    EAR= as.character(sprintf("%.2f",sr_1_accidents_df$EAR)), 
    phi=extract_phi(c_SISTRAT_c1_transport))

#c_SISTRAT_c1_assaults c_SISTRAT_c1_self_harm c_SISTRAT_c1_transport c_SISTRAT_c1_other_ext c_SISTRAT_c1_other
#sr_1_assaults_df sr_1_self_harm_df sr_1_accidents_df sr_1_other_unint_ext_df sr_1_other_causes_df

cat("Dispersion-corrected 95% confidence intervals\n")
bind_rows(sir_assaults_df, sir_self_harm_df, sir_other_ext_df, sir_other_df, sir_transport_df)|> 
  rename("Characteristic"="total")|>
  (\(df) {
    df->> df_smr_ind_ext
    df
  })()|> 
  extract(
    SMR,
    into   = c("est", "low", "high"),
    regex  = "^\\s*([0-9.]+)\\s*\\(([^–-]+)[–-]([^)]+)\\)\\s*$",
    convert = TRUE            # convierte a numérico
  )|>
  dplyr::mutate(across(c(est, low, high), \(x) round(x, 2)),
                SMR_dir = sprintf("%.2f (%.2f–%.2f)", est, low, high))|>
  dplyr::select(-est, -low, -high)|>  
  mutate(obs_exp= paste0(observed, "/", expected)) |> 
  knitr::kable("markdown", caption="All-cause SMRs for patients who accessed SUD treatment by sex and age group. External causes")


r2_adj_assaults <-
  rate(
    data    = c_SISTRAT_c1_assaults,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_assaults <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_assaults$rate.adj,           # primer vector (rate)
  r2_adj_assaults$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    #phi    = extract_phi_dir(c_SISTRAT_c1_assaults),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_self_harm <-
  rate(
    data    = c_SISTRAT_c1_self_harm,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_self_harm <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_self_harm$rate.adj,           # primer vector (rate)
  r2_adj_self_harm$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_self_harm),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_transport <-
  rate(
    data    = c_SISTRAT_c1_transport,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_transport <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_transport$rate.adj,           # primer vector (rate)
  r2_adj_transport$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_transport),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_other_ext <-
  rate(
    data    = c_SISTRAT_c1_other_ext,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_other_ext <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_other_ext$rate.adj,           # primer vector (rate)
  r2_adj_other_ext$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_other_ext),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_other <-
rate(
  data    = c_SISTRAT_c1_other,
  obs     = from0to1,
  pyrs    = pyrs,
  #print   = year,
  adjust  = c("year", "sex", "agegroup"),
  weights = weights_df #weights inglm should be applied in the offset
)

DSR_1k_other <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_other$rate.adj,           # primer vector (rate)
  r2_adj_other$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_other),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

tasa_ponderada <- mx_1x1_comp_filt3 |> 
  ungroup() |> 
  left_join(
    weights_df,
    by = c("year", "agegroup", "sex")) |> 
  summarise(
    haz_assaults_w = sum(haz_assaults * weights),
    haz_self_harm_w = sum(haz_self_harm * weights),
    haz_other_causes_w = sum(haz_other_causes * weights),
    haz_other_ext_causes_w = sum(haz_other_ext_causes * weights),
    haz_transport_accidents_w = sum(haz_transport_accidents * weights)
  ) *1e3
round(tasa_ponderada,2)


rbind.data.frame(
  cbind.data.frame(var="Assaults/ Aggressions", t(r2_adj_assaults[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_assaults),
  cbind.data.frame(var="Intentional self-harm", t(r2_adj_self_harm[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_self_harm),
  cbind.data.frame(var="Other external causes", t(r2_adj_other_ext[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_other_ext),
  cbind.data.frame(var="No external causes", t(r2_adj_other[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_other),
  cbind.data.frame(var="Transport accidents", t(r2_adj_transport[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_transport)
)|>
  (\(df) {
    df->> df_smr_dir_ext
    df
  })()|> 
  mutate(Rate_95ci= sprintf("%.1f (%.1f–%.1f)", `1`*1000, `2`*1000, `3`*1000))|> 
  mutate(AdjRate_95ci = sprintf("%.1f (%.1f–%.1f)", `4`*1000, `5`*1000, `6`*1000))|>
  dplyr::select(-any_of(2:7))|>
  extract(
    SMR_dir,
    into   = c("est", "low", "high"),
    regex  = "^\\s*([0-9.]+)\\s*\\(([^–-]+)[–-]([^)]+)\\)\\s*$",
    convert = TRUE            # convierte a numérico
  )|>
  dplyr::mutate(across(c(est, low, high), \(x) round(x, 2)),
                SMR_dir = sprintf("%.1f (%.1f–%.1f)", est, low, high))|>  
  dplyr::select(-est, -low, -high)|>  
  rename("DSR (SEs robust to dispersion)"="SMR_dir", "DSR"="AdjRate_95ci", "CMR"="Rate_95ci")|> 
  knitr::kable("markdown", caption= "SMRs, direct method, last treatment followed")



# Create mortality summary by disease category
mortality_summary2 <- 
  mortality_deduplicated |> 
  filter(ano_def>=2010) |> 
  filter(!is.na(category)) |> 
  #filter(!grepl("Other causes",category)) |> 
  group_by(ano_def, sex_rec, edad_cant) |> 
  summarise(circulatory=sum(chapter=="Circulatory", na.rm=T),
            digestive=sum(chapter=="Digestive", na.rm=T), 
            endocrine_metabolic= sum(grepl("Endocrine", chapter), na.rm=T), 
            infectious_parasitic= sum(grepl("parasitic", chapter), na.rm=T), 
            other_causes= sum(grepl("Other", chapter), na.rm=T), 
            respiratory= sum(grepl("Respiratory",chapter), na.rm=T),
            symptoms_signs= sum(grepl("Symptoms",chapter), na.rm=T)
  )

# Create hazard rates dataset with age categories and disease-specific hazards
mx_1x1_comp_filt3b <- 
  mx_1x1_comp_filt2|> 
  mutate(edad_cant_cat = dplyr::case_when(
    Age>= 18 & Age < 30 ~ "18-29",
    Age>= 30 & Age < 45 ~ "30-44",
    Age>= 45 & Age < 60 ~ "45-59",
    Age>= 60 & Age < 86 ~ "60+",
    TRUE ~ NA_character_  # Opcional: manejo de valores fuera de rango
  ),
  sex_rec = dplyr::case_when(                 # optional renaming step
    sex == "male"   ~ "Male",
    sex == "female" ~ "Female",
    TRUE~ NA_character_),
  sex_rec= factor(sex_rec, levels = c("Male", "Female"))
  )|> 
  left_join(mortality_summary2, by= c("Year"="ano_def", "sex_rec"= "sex_rec", "Age"= "edad_cant"))|> 
  group_by(Year, sex_rec, edad_cant_cat)|> 
  summarise(
    sum_circulatory = sum(circulatory, na.rm=T),
    sum_digestive = sum(digestive, na.rm=T),
    sum_endocrine_metabolic = sum(endocrine_metabolic, na.rm=T),
    sum_infectious_parasitic = sum(infectious_parasitic, na.rm=T),
    sum_other_causes = sum(other_causes, na.rm=T),
    sum_respiratory = sum(respiratory, na.rm=T),
    sum_symptoms_signs = sum(symptoms_signs, na.rm=T),
    Lx_total = sum(Lx)  # Sumar los años-persona
  ) |> 
  mutate(haz_circulatory= sum_circulatory/Lx_total, 
         haz_digestive= sum_digestive/Lx_total, 
         haz_endocrine_metabolic= sum_endocrine_metabolic/Lx_total, 
         haz_infectious_parasitic= sum_infectious_parasitic/Lx_total,
         haz_other_causes= sum_other_causes/Lx_total,
         haz_respiratory= sum_respiratory/Lx_total,
         haz_symptoms_signs= sum_symptoms_signs/Lx_total
  )

# Add agegroup variable and rename columns for analysis
mx_1x1_comp_filt3b <- 
  mx_1x1_comp_filt3b|>
  mutate(agegroup = dplyr::case_when(
    grepl("18",edad_cant_cat)~ 18,
    grepl("30",edad_cant_cat)~ 30,
    grepl("45",edad_cant_cat)~ 45,
    grepl("60",edad_cant_cat)~ 60,
    TRUE ~ NA_real_  # Opcional: manejo de valores fuera de rango
  ))|> 
  rename("year"="Year", "sex"="sex_rec")


c_SISTRAT_c1_circulatory <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                 status = grepl("Circulatory",chapter), 
                                 birth = birth_date_rec, 
                                 exit = death_date_rec, entry = disch_date_rec6,
                                 #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                 breaks = list(per = seq(2010, 2021, by = 1), 
                                               #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                               age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                 aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_circulatory_df <- 
  popEpi::sir( coh.data = c_SISTRAT_c1_circulatory, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
               ref.data = mx_1x1_comp_filt3b, #this should be only for people with assaults
               ref.rate = 'haz_circulatory', 
               adjust = c('agegroup','year','sex'), 
               EAR=T)
c_SISTRAT_c1_digestive <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                    status = grepl("Digestive",chapter), 
                                    birth = birth_date_rec, 
                                    exit = death_date_rec, entry = disch_date_rec6,
                                    #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                    breaks = list(per = seq(2010, 2021, by = 1), 
                                                  #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                  age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                    aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_digestive_df <- 
  popEpi::sir( coh.data = c_SISTRAT_c1_digestive, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
               ref.data = mx_1x1_comp_filt3b, #this should be only for people with assaults
               ref.rate = 'haz_digestive', 
               adjust = c('agegroup','year','sex'), 
               EAR=T)
c_SISTRAT_c1_endocrine_metabolic <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                  status = grepl("Endocrine",chapter), 
                                  birth = birth_date_rec, 
                                  exit = death_date_rec, entry = disch_date_rec6,
                                  #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                  breaks = list(per = seq(2010, 2021, by = 1), 
                                                #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                  aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_endocrine_metabolic_df <- 
  popEpi::sir( coh.data = c_SISTRAT_c1_endocrine_metabolic, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
               ref.data = mx_1x1_comp_filt3b, #this should be only for people with assaults
               ref.rate = 'haz_endocrine_metabolic', 
               adjust = c('agegroup','year','sex'), 
               EAR=T)
c_SISTRAT_c1_infectious_parasitic <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                      status = grepl("parasitic",chapter), 
                                      birth = birth_date_rec, 
                                      exit = death_date_rec, entry = disch_date_rec6,
                                      #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                      breaks = list(per = seq(2010, 2021, by = 1), 
                                                    #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                    age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                      aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_infectious_parasitic_df <- 
  popEpi::sir( coh.data = c_SISTRAT_c1_infectious_parasitic, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
               ref.data = mx_1x1_comp_filt3b, #this should be only for people with assaults
               ref.rate = 'haz_infectious_parasitic', 
               adjust = c('agegroup','year','sex'), 
               EAR=T)
c_SISTRAT_c1_other_causes <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                             status = grepl("Other",chapter), 
                                             birth = birth_date_rec, 
                                             exit = death_date_rec, entry = disch_date_rec6,
                                             #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                             breaks = list(per = seq(2010, 2021, by = 1), 
                                                           #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                           age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                             aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_other_causes_df <- 
  popEpi::sir( coh.data = c_SISTRAT_c1_other_causes, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
               ref.data = mx_1x1_comp_filt3b, #this should be only for people with assaults
               ref.rate = 'haz_other_causes', 
               adjust = c('agegroup','year','sex'), 
               EAR=T)
c_SISTRAT_c1_respiratory <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                            status = grepl("Respiratory",chapter), 
                                            birth = birth_date_rec, 
                                            exit = death_date_rec, entry = disch_date_rec6,
                                            #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                            breaks = list(per = seq(2010, 2021, by = 1), 
                                                          #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                          age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                            aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_respiratory_df <- 
  popEpi::sir( coh.data = c_SISTRAT_c1_respiratory, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
               ref.data = mx_1x1_comp_filt3b, #this should be only for people with assaults
               ref.rate = 'haz_respiratory', 
               adjust = c('agegroup','year','sex'), 
               EAR=T)
c_SISTRAT_c1_symptoms_signs <- lexpand(clean_df|> left_join(mortality_deduplicated[,c("hashkey", "edad_cant_cat", "category", "chapter")], by=c("hash_key"="hashkey")), 
                                    status = grepl("Symptoms",chapter), 
                                    birth = birth_date_rec, 
                                    exit = death_date_rec, entry = disch_date_rec6,
                                    #2025-06-11= le tuve que dar apertura para que integrarara a los que se retiraban después.
                                    breaks = list(per = seq(2010, 2021, by = 1), 
                                                  #2025-06-11=No filtro por la edad en que fallece la persona, el filtro ya lo hice arriba
                                                  age = c(18, 30, 45, 60, 76)), #, fot = c(0, 1, 3, 5, Inf)
                                    aggre = list(agegroup = age, year = per, sex= sex_rec))
sr_1_symptoms_signs_df <- 
  popEpi::sir( coh.data = c_SISTRAT_c1_symptoms_signs, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
               ref.data = mx_1x1_comp_filt3b, #this should be only for people with assaults
               ref.rate = 'haz_symptoms_signs', 
               adjust = c('agegroup','year','sex'), 
               EAR=T)

sir_circulatory_df <- 
  cbind.data.frame(
    total = "Circulatory System Diseases",
    observed = round(sr_1_circulatory_df$observed, 0),
    pyrs = round(sr_1_circulatory_df$pyrs, 0),
    CMR_1000 = do.call(sprintf, c("%.1f (%.1f–%.1f)", as.list(unlist(cmr_ci_phi(sr_1_circulatory_df$observed, sr_1_circulatory_df$pyrs, phi = 1))))),
    expected = round(sr_1_circulatory_df$expected, 0),
    SMR = do.call(sprintf, c("%.6f (%.6f–%.6f)", as.list(unlist(sir_ci_phi_improved(sr_1_circulatory_df, phi = extract_phi(c_SISTRAT_c1_circulatory))[, 1:3])))),
    EAR = as.character(sprintf("%.2f", sr_1_circulatory_df$EAR)), 
    phi = extract_phi(c_SISTRAT_c1_circulatory))

sir_digestive_df <- 
  cbind.data.frame(
    total = "Digestive System Diseases",
    observed = round(sr_1_digestive_df$observed, 0),
    pyrs = round(sr_1_digestive_df$pyrs, 0),
    CMR_1000 = do.call(sprintf, c("%.1f (%.1f–%.1f)", as.list(unlist(cmr_ci_phi(sr_1_digestive_df$observed, sr_1_digestive_df$pyrs, phi = 1))))),
    expected = round(sr_1_digestive_df$expected, 0),
    SMR = do.call(sprintf, c("%.6f (%.6f–%.6f)", as.list(unlist(sir_ci_phi_improved(sr_1_digestive_df, phi = extract_phi(c_SISTRAT_c1_digestive))[, 1:3])))),
    EAR = as.character(sprintf("%.2f", sr_1_digestive_df$EAR)), 
    phi = extract_phi(c_SISTRAT_c1_digestive))

sir_endocrine_metabolic_df <- 
  cbind.data.frame(
    total = "Endocrine and Metabolic Diseases",
    observed = round(sr_1_endocrine_metabolic_df$observed, 0),
    pyrs = round(sr_1_endocrine_metabolic_df$pyrs, 0),
    CMR_1000 = do.call(sprintf, c("%.1f (%.1f–%.1f)", as.list(unlist(cmr_ci_phi(sr_1_endocrine_metabolic_df$observed, sr_1_endocrine_metabolic_df$pyrs, phi = 1))))),
    expected = round(sr_1_endocrine_metabolic_df$expected, 0),
    SMR = do.call(sprintf, c("%.6f (%.6f–%.6f)", as.list(unlist(sir_ci_phi_improved(sr_1_endocrine_metabolic_df, phi = extract_phi(c_SISTRAT_c1_endocrine_metabolic))[, 1:3])))),
    EAR = as.character(sprintf("%.2f", sr_1_endocrine_metabolic_df$EAR)), 
    phi = extract_phi(c_SISTRAT_c1_endocrine_metabolic))

sir_infectious_parasitic_df <- 
  cbind.data.frame(
    total = "Infectious and Parasitic Diseases",
    observed = round(sr_1_infectious_parasitic_df$observed, 0),
    pyrs = round(sr_1_infectious_parasitic_df$pyrs, 0),
    CMR_1000 = do.call(sprintf, c("%.1f (%.1f–%.1f)", as.list(unlist(cmr_ci_phi(sr_1_infectious_parasitic_df$observed, sr_1_infectious_parasitic_df$pyrs, phi = 1))))),
    expected = round(sr_1_infectious_parasitic_df$expected, 0),
    SMR = do.call(sprintf, c("%.6f (%.6f–%.6f)", as.list(unlist(sir_ci_phi_improved(sr_1_infectious_parasitic_df, phi = extract_phi(c_SISTRAT_c1_infectious_parasitic))[, 1:3])))),
    EAR = as.character(sprintf("%.2f", sr_1_infectious_parasitic_df$EAR)), 
    phi = extract_phi(c_SISTRAT_c1_infectious_parasitic))

sir_other_causes_df <- 
  cbind.data.frame(
    total = "Malignant Neoplasms",
    observed = round(sr_1_other_causes_df$observed, 0),
    pyrs = round(sr_1_other_causes_df$pyrs, 0),
    CMR_1000 = do.call(sprintf, c("%.1f (%.1f–%.1f)", as.list(unlist(cmr_ci_phi(sr_1_other_causes_df$observed, sr_1_other_causes_df$pyrs, phi = 1))))),
    expected = round(sr_1_other_causes_df$expected, 0),
    SMR = do.call(sprintf, c("%.6f (%.6f–%.6f)", as.list(unlist(sir_ci_phi_improved(sr_1_other_causes_df, phi = extract_phi(c_SISTRAT_c1_other_causes))[, 1:3])))),
    EAR = as.character(sprintf("%.2f", sr_1_other_causes_df$EAR)), 
    phi = extract_phi(c_SISTRAT_c1_other_causes))

sir_respiratory_df <- 
  cbind.data.frame(
    total = "Respiratory System Diseases",
    observed = round(sr_1_respiratory_df$observed, 0),
    pyrs = round(sr_1_respiratory_df$pyrs, 0),
    CMR_1000 = do.call(sprintf, c("%.1f (%.1f–%.1f)", as.list(unlist(cmr_ci_phi(sr_1_respiratory_df$observed, sr_1_respiratory_df$pyrs, phi = 1))))),
    expected = round(sr_1_respiratory_df$expected, 0),
    SMR = do.call(sprintf, c("%.6f (%.6f–%.6f)", as.list(unlist(sir_ci_phi_improved(sr_1_respiratory_df, phi = extract_phi(c_SISTRAT_c1_respiratory))[, 1:3])))),
    EAR = as.character(sprintf("%.2f", sr_1_respiratory_df$EAR)), 
    phi = extract_phi(c_SISTRAT_c1_respiratory))

sir_symptoms_signs_df <- 
  cbind.data.frame(
    total = "Symptoms, Signs and Abnormal Findings",
    observed = round(sr_1_symptoms_signs_df$observed, 0),
    pyrs = round(sr_1_symptoms_signs_df$pyrs, 0),
    CMR_1000 = do.call(sprintf, c("%.1f (%.1f–%.1f)", as.list(unlist(cmr_ci_phi(sr_1_symptoms_signs_df$observed, sr_1_symptoms_signs_df$pyrs, phi = 1))))),
    expected = round(sr_1_symptoms_signs_df$expected, 0),
    SMR = do.call(sprintf, c("%.6f (%.6f–%.6f)", as.list(unlist(sir_ci_phi_improved(sr_1_symptoms_signs_df, phi = extract_phi(c_SISTRAT_c1_symptoms_signs))[, 1:3])))),
    EAR = as.character(sprintf("%.2f", sr_1_symptoms_signs_df$EAR)), 
    phi = extract_phi(c_SISTRAT_c1_symptoms_signs))


cat("Dispersion-corrected 95% confidence intervals\n")
bind_rows(sir_circulatory_df,  sir_digestive_df, sir_endocrine_metabolic_df,  
          sir_infectious_parasitic_df, sir_other_causes_df, sir_respiratory_df, sir_symptoms_signs_df)|> 
  rename("Characteristic"="total")|>
  (\(df) {
    df->> df_smr_ind_non_ext
    df
  })()|> 
  extract(
    SMR,
    into   = c("est", "low", "high"),
    regex  = "^\\s*([0-9.]+)\\s*\\(([^–-]+)[–-]([^)]+)\\)\\s*$",
    convert = TRUE            # convierte a numérico
  )|>
  dplyr::mutate(across(c(est, low, high), \(x) round(x, 2)),
                SMR_dir = sprintf("%.2f (%.2f–%.2f)", est, low, high))|>
  dplyr::select(-est, -low, -high)|>  
  knitr::kable("markdown", caption="All-cause SMRs for patients who accessed SUD treatment by sex and age group. Non-external causes")

sr2_adj_circulatory <-
  rate(
    data    = c_SISTRAT_c1_circulatory,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_circulatory <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_circulatory$rate.adj,           # primer vector (rate)
  r2_adj_circulatory$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_circulatory),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_congenital_anomalies <-
  rate(
    data    = c_SISTRAT_c1_congenital_anomalies,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_congenital_anomalies <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_congenital_anomalies$rate.adj,           # primer vector (rate)
  r2_adj_congenital_anomalies$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_congenital_anomalies),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_digestive <-
  rate(
    data    = c_SISTRAT_c1_digestive,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_digestive <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_digestive$rate.adj,           # primer vector (rate)
  r2_adj_digestive$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_digestive),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_endocrine_metabolic <-
  rate(
    data    = c_SISTRAT_c1_endocrine_metabolic,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_endocrine_metabolic <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_endocrine_metabolic$rate.adj,           # primer vector (rate)
  r2_adj_endocrine_metabolic$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_endocrine_metabolic),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_genitourinary <-
  rate(
    data    = c_SISTRAT_c1_genitourinary,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_genitourinary <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_genitourinary$rate.adj,           # primer vector (rate)
  r2_adj_genitourinary$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_genitourinary),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_infectious_parasitic <-
  rate(
    data    = c_SISTRAT_c1_infectious_parasitic,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_infectious_parasitic <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_infectious_parasitic$rate.adj,           # primer vector (rate)
  r2_adj_infectious_parasitic$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_infectious_parasitic),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_malignant_neoplasms <-
  rate(
    data    = c_SISTRAT_c1_malignant_neoplasms,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_malignant_neoplasms <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_malignant_neoplasms$rate.adj,           # primer vector (rate)
  r2_adj_malignant_neoplasms$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_malignant_neoplasms),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_respiratory <-
  rate(
    data    = c_SISTRAT_c1_respiratory,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_respiratory <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_respiratory$rate.adj,           # primer vector (rate)
  r2_adj_respiratory$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_respiratory),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

r2_adj_symptoms_signs <-
  rate(
    data    = c_SISTRAT_c1_symptoms_signs,
    obs     = from0to1,
    pyrs    = pyrs,
    #print   = year,
    adjust  = c("year", "sex", "agegroup"),
    weights = weights_df #weights inglm should be applied in the offset
  )

DSR_1k_symptoms_signs <- mapply(
  dsr_format_corr,                 # FUN
  r2_adj_symptoms_signs$rate.adj,           # primer vector (rate)
  r2_adj_symptoms_signs$SE.rate.adj,        # segundo vector (se)
  MoreArgs = list(            # argumentos fijos extra
    phi    = extract_phi_dir(c_SISTRAT_c1_symptoms_signs),
    factor = 1e3,
    digits = 6,
    conf   = 0.95))

tasa_ponderada_diseases <- mx_1x1_comp_filt3b |> 
  ungroup() |> 
  left_join(
    weights_df,
    by = c("year", "agegroup", "sex")) |> 
  summarise(
    haz_circulatory_w = sum(haz_circulatory * weights),
    haz_congenital_anomalies_w = sum(haz_congenital_anomalies * weights),
    haz_digestive_w = sum(haz_digestive * weights),
    haz_endocrine_metabolic_w = sum(haz_endocrine_metabolic * weights),
    haz_genitourinary_w = sum(haz_genitourinary * weights),
    haz_infectious_parasitic_w = sum(haz_infectious_parasitic * weights),
    haz_malignant_neoplasms_w = sum(haz_malignant_neoplasms * weights),
    haz_respiratory_w = sum(haz_respiratory * weights),
    haz_symptoms_signs_w = sum(haz_symptoms_signs * weights)
  ) *1e3
round(tasa_ponderada_diseases,1)

rbind.data.frame(
  cbind.data.frame(var="Circulatory System Diseases", t(r2_adj_circulatory[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_circulatory),
  cbind.data.frame(var="Congenital Malformations and Anomalies", t(r2_adj_congenital_anomalies[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_congenital_anomalies),
  cbind.data.frame(var="Digestive System Diseases", t(r2_adj_digestive[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_digestive),
  cbind.data.frame(var="Endocrine and Metabolic Diseases", t(r2_adj_endocrine_metabolic[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_endocrine_metabolic),
  cbind.data.frame(var="Genitourinary System Diseases", t(r2_adj_genitourinary[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_genitourinary),
  cbind.data.frame(var="Infectious and Parasitic Diseases", t(r2_adj_infectious_parasitic[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_infectious_parasitic),
  cbind.data.frame(var="Malignant Neoplasms", t(r2_adj_malignant_neoplasms[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_malignant_neoplasms),
  cbind.data.frame(var="Respiratory System Diseases", t(r2_adj_respiratory[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_respiratory),
  cbind.data.frame(var="Symptoms, Signs and Abnormal Findings", t(r2_adj_symptoms_signs[, c(rate, rate.lo, rate.hi, rate.adj, rate.adj.lo, rate.adj.hi)]), SMR_dir= DSR_1k_symptoms_signs)
)|>
  (\(df) {
    df->> df_smr_dir_diseases
    df
  })()|> 
  mutate(Rate_95ci= sprintf("%.1f (%.1f–%.1f)", `1`*1000, `2`*1000, `3`*1000))|> 
  mutate(AdjRate_95ci = sprintf("%.1f (%.1f–%.1f)", `4`*1000, `5`*1000, `6`*1000))|>
  dplyr::select(-any_of(2:7))|>
  extract(
    SMR_dir,
    into   = c("est", "low", "high"),
    regex  = "^\\s*([0-9.]+)\\s*\\(([^–-]+)[–-]([^)]+)\\)\\s*$",
    convert = TRUE            # convierte a numérico
  )|>
  dplyr::mutate(across(c(est, low, high), \(x) round(x, 2)),
                SMR_dir = sprintf("%.1f (%.1f–%.1f)", est, low, high))|>  
  dplyr::select(-est, -low, -high)|>  
  rename("DSR (SEs robust to dispersion)"="SMR_dir", "DSR"="AdjRate_95ci", "CMR"="Rate_95ci")|> 
  knitr::kable("markdown", caption= "Disease-specific SMRs, direct method, non-external")