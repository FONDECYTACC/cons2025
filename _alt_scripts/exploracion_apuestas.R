

pattern <- "^F63"


def_enc17_21 |> 
#  dplyr::filter(RUN %in% subset(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens, filter= death_time_rec==1)$run) |> 
#  dplyr::filter(!RUN %in% runs_6025$run) |>
  dplyr::select(RUN, DIAG1, DIAG2) |> 
  reshape2::melt(id.vars="RUN") |> 
  dplyr::filter(!is.na(value)) |> 
  # janitor::tabyl(value) |> 
  # arrange(desc(n)) |> 
  dplyr::mutate(apuestas = grepl("^F63", value, ignore.case = F)) |> 
  janitor::tabyl(apuestas) 


grepl("^(X4[0-5]|X6[0-5]|Y1[0-5])$", datos$causa)

df_f630
def_enc17_21 |> 
    dplyr::filter(RUN %in% df_f630$run) 

ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens |> 
  dplyr::filter(run %in% df_f630$run) 

df_f630 |> 
  janitor::tabyl(glosa_sexo)

df_f630 |> 
  janitor::tabyl(ci_conadi)


sd(df_f630$edad_anos)
df_f630 |> 
dplyr::select(run, diag1, diag2, diag3, diag4, diag5, diag6, diag7, diag8, diag9, diag10, diag11) |> 
  reshape2::melt(id.vars="run") |> 
  dplyr::filter(!is.na(value)) |> 
  janitor::tabyl(value) |> 
  arrange(desc(n))

# Records with causes of death coded as X40–X45 (accidental
# poisoning), X60–X65 (intentional poisoning), or Y10–Y15 (undetermined
# intentionality poisoning) were included. The dataset comprised 21,410 deaths,
# including 933 subjects with mood disorders (ICD-10 codes F30-F39)

def_enc17_21 |> 
  dplyr::filter(RUN %in% subset(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens, filter= death_time_rec==1)$run) |> 
  dplyr::filter(!RUN %in% runs_6025$run) |>
  dplyr::select(RUN, DIAG1, DIAG2) |> 
  reshape2::melt(id.vars="RUN") |> 
  dplyr::filter(!is.na(value)) |> 
  # janitor::tabyl(value) |> 
  # arrange(desc(n)) |> 
  dplyr::mutate(apuestas = grepl("^(X4[0-5]|X6[0-5]|Y1[0-5])", value, ignore.case = F)) |> 
  janitor::tabyl(apuestas) 
def_enc17_21 |> 
  #dplyr::filter(RUN %in% subset(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens, filter= death_time_rec==1)$run) |> 
  dplyr::filter(RUN %in% runs_6025$run) |>
  dplyr::select(RUN, DIAG1, DIAG2) |> 
  reshape2::melt(id.vars="RUN") |> 
  dplyr::filter(!is.na(value)) |> 
  # janitor::tabyl(value) |> 
  # arrange(desc(n)) |> 
  dplyr::mutate(apuestas = grepl("^(X4[0-5]|X6[0-5]|Y1[0-5])", value, ignore.case = F)) |> 
  janitor::tabyl(apuestas) 

