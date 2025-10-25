clean_df_corr <- clean_df %>% 
  mutate(
    year_death = year(death_date_rec),
    age_death  = as.numeric(difftime(death_date_rec, birth_date_rec,
                                     units = "days")) / 365.2425#365.25
  )

# -------------------------------------------------------------------
# 2.  Reconstruimos 'per' (2010-2021) tal como lo agrupa lexpand()
#     (intervalos cerrados a la izquierda, abiertos a la derecha)
# -------------------------------------------------------------------
breaks_vec <- seq(as.Date("2010-01-01"), as.Date("2022-01-01"), by = "year")

clean_df_corr <- clean_df_corr %>% 
  mutate(
    per = cut(
      death_date_rec,
      breaks = breaks_vec,
      right  = FALSE,            # [2010, 2011)
      labels = 2010:2021
    ) %>% as.integer()
  )

# -------------------------------------------------------------------
# 3.  Etiquetamos la razón de inclusión / exclusión
# -------------------------------------------------------------------
clean_df_corr <- clean_df_corr %>%
  mutate(
    excl_reason = case_when(
      status != 1                         ~ "vivo / censurado",
      !is.na(per) & age_death < 65        ~ "incluido",
      is.na(per)    & age_death >= 65     ~ "edad≥65 & año≥2022",
      is.na(per)                          ~ "año≥2022",
      age_death >= 65                     ~ "edad≥65",
      TRUE                                ~ "otro"
    )
  )

# -------------------------------------------------------------------
# 4.  Subconjuntos solicitados
# -------------------------------------------------------------------
muertes_per2021   <- clean_df_corr %>% filter(status == 1, per == 2021)
muertes_fuera_per <- clean_df_corr %>% filter(status == 1, is.na(per))          # ≥ 2022
muertes_age65plus <- clean_df_corr %>% filter(status == 1, age_death >= 65)

excluidos <- clean_df_corr %>%
  filter(status == 1, excl_reason != "incluido") %>%
  select(rn, hash_key, death_date_rec, age_death, excl_reason)

max(excluidos$age_death)
#[1] 74.42574
# -------------------------------------------------------------------
# 5.  Resumen rápido de exclusiones
# -------------------------------------------------------------------
clean_df_corr %>%
  filter(status == 1) %>%
  count(excl_reason, name = "n") %>%
  arrange(desc(n))



start_fup <- as.Date("2010-01-01")
end_fup   <- as.Date("2020-12-31")

pyrs_raw <- clean_df_corr %>%
  ## Criterios de inclusión idénticos a los del SIR ------------------
mutate(
  age_death = as.numeric(difftime(death_date_rec, birth_date_rec,
                                  units = "days")) / 365.2425#365.25
) %>%
  filter(
    ## Solo quienes entran en el SIR
    (is.na(age_death) | age_death < 76),          # top age 65
    death_date_rec >= start_fup | is.na(death_date_rec), 
    disch_date_rec6 <= end_fup                    # entrada ≤ 31-dic-2021
  ) %>%
  ## Define fecha de salida (muerte o censoría) ----------------------
mutate(
  exit_date = coalesce(death_date_rec, end_fup),      # si vivo → 31-dic-2021
  exit_date = pmin(exit_date, end_fup),               # corta las muertes >2021
  follow_up_days = as.numeric(exit_date - disch_date_rec6),
  pyrs = pmax(follow_up_days, 0) / 365.2425#365.25             # evita negativos
) %>%
  (\(df) {
    cat("Deaths \n")
    print(janitor::tabyl(df,status))
    cat("Number of rows \n")
    print(nrow(df))
    df ->>clean_df_corr_surv
  })() |> 
  summarise(total_pyrs = sum(pyrs, na.rm = TRUE)) %>%
  pull()

pyrs_raw   # debería ~ 353826