
```{r more-one-val-nationality-2}
#| echo: true
#| error: false
#| warning: true
#| message: true
#| paged.print: true
#| results: "hold"
#| eval: false

invisible("This database is useless. We cant obtain information because there is no 1:1 linkage")

hosp_un_inv_2<-
  rio::import(paste0(gsub("/cons","/data/20231205_original_data",getwd()),"/EH_2010_2022_Pasantes_v2_encrip.csv"))

#hosp_un_inv_2[,c("RUN", "ESTAB_HOMO", "FECHA_INGRESO_FMT_DEIS", "FECHA_EGRESO_FMT_DEIS", "SEXO", "EDAD_ANOS", "DIAG1", "DIAG2", "COND_EGR")]
hosp_un_inv_2_df<- hosp_un_inv_2 %>%
  mutate(DIAG2= ifelse(nchar(DIAG2)<2, NA_character_, DIAG2)) %>%
  mutate(
    KEY = paste(ESTAB_HOMO, FECHA_INGRESO_FMT_DEIS, FECHA_EGRESO_FMT_DEIS,
                SEXO, EDAD_ANOS, DIAG1, DIAG2, COND_EGR, sep = "|")
  )

#HOSP_filter_df[, c("run", "estab_homo", "fecha_ingreso", "fecha_egreso", "sexo", "edad_anos", "diag1", "diag2", "cond_egr")]
HOSP_filter_df<- HOSP_filter_df %>%
  mutate(
    KEY = paste(estab_homo, fecha_ingreso, fecha_egreso,
                sexo, edad_anos, diag1, diag2, cond_egr, sep = "|")
  )

HOSP_filter_df_join_KEY_more_one<-
  HOSP_filter_df|> 
  inner_join(hosp_un_inv_2_df, by="KEY")|> 
  group_by(KEY)|> 
  count()|> 
  ungroup()|> 
  filter(n>1)


HOSP_filter_df_join_KEY_only_one<-
  HOSP_filter_df|> 
  inner_join(hosp_un_inv_2_df, by="KEY")|> 
  group_by(KEY)|> 
  mutate(n=n())|> 
  ungroup()|> 
  filter(n==1)

# HOSP_filter_df|> 
# inner_join(hosp_un_inv_2_df, by="KEY")|> 
#   filter(GLOSA_PAIS_ORIGEN!="")|> 
#   #select(run, GLOSA_PAIS_ORIGEN)|> 
#   distinct(run, GLOSA_PAIS_ORIGEN)|> 
#   ungroup()|>
#   group_by(run)|>
#   mutate(id = as.character(dplyr::row_number()))|>  # Convertir `id` a carácter
#   pivot_wider(names_from = id, values_from = GLOSA_PAIS_ORIGEN, 
#                          names_prefix = "hosp_nat_")
```

