# Estimado Andres,
# subí el el encriptado del c1 2019 y top del 2018 y 2019.
# Estoy tratando de rescatar 2018 del c1.

invisible("Only run from Ubuntu")
if (!(Sys.getenv("RSTUDIO_SESSION_TYPE") == "server" || file.exists("/.dockerenv"))) {
  if(Sys.info()["sysname"]!="Windows"){
    Sys.setenv(RETICULATE_PYTHON = "/home/fondecytacc/.pyenv/versions/3.11.5/bin/python")
  }
}

#clean enviroment
rm(list = ls()); gc()
file.path(paste0(gsub("/cons","",gsub("cons","",paste0(getwd(),"/cons"))),"data/20241015_out"))

wdpath<-
  paste0(gsub("/cons","",gsub("cons","",paste0(getwd(),"/cons"))))
wdpath

envpath<- if(regmatches(wdpath, regexpr("[A-Za-z]+", wdpath))=="G"){"G:/Mi unidad/Alvacast/SISTRAT 2023/"}else{"E:/Mi unidad/Alvacast/SISTRAT 2023/"}
envpath

time_before_dedup2<-Sys.time()

base::load(paste0(wdpath,"data/20241015_out/","3_ndp_2025_03_24.Rdata"))


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
library(tidyverse)

cat("Import database, select vector of trucated treatments, and visualize markdown")
SISTRAT23_c1_2010_2022_df_prev1g|>
  filter(is.na(disch_date_num)) |> 
  mutate(disch_date_na= as.Date(adm_date_rec_num+ dias_en_tratamiento, origin = "1970-01-01")) |>
  select(TABLE_rec, rn, hash_key, dias_en_tratamiento, adm_age_rec, adm_date_rec, disch_date_na, id_centro, tr_compliance, plan_type, senda) |> 
  filter(disch_date_na<"2023-04-28" & grepl("currently",tr_compliance)) |> 
  (\(df) { 
    cat(paste0("00. Missing discharge dates due to truncation, cases: ", formatC(nrow(df), big.mark=",")),"\n")
    cat(paste0("00. Missing discharge dates due to truncation, RUNs: ", formatC(nrow(distinct(df, hash_key)), big.mark=",")),"\n")
    distinct(df, hash_key) |> pull(hash_key) ->> hash_truncated_treatments_due_to_retrieval_2019 
    df|> pull(rn) ->> rows_truncated_treatments_due_to_retrieval_2019 
    df
  })() |> 
  #View()
  filter(hash_key %in% (sample_n_with_seed(data.frame(hash_truncated_treatments_due_to_retrieval_2019),20, seed=2125) |> pull(1)))|>
  mutate(hash_key= as.numeric(factor(hash_key)))|>
  knitr::kable("markdown", caption= "Missing discharge dates due to administrative truncation (sample)")

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
cat("TEST")
cat("We take the new database of 2019 and try to standardize according to formatting made previous to the first step of the deduplication phase")
X2019_2019dup_encrip <- readr::read_delim("G:/My Drive/Alvacast/SISTRAT 2023/data/20250508_original_data/2019_2019dup_encrip.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                          locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", 
                                                          encoding = "latin1"),#encoding = "ISO-8859-1"),
                                          na = c("", "NA","null"),
                                          guess_max = min(1e5, Inf)) |> janitor::clean_names()|> 
  mutate(cod_indentificacion= tolower(codigo_identificaci_a_a3n))

cat("Overview of the 2019 database, filtered to include only those SENDA IDs that are among the administratively truncated treatments in the formatted database.")
X2019_2019dup_encrip |> 
  #vemos coincidencias encódigo de identificación
  filter(cod_indentificacion %in% (SISTRAT23_c1_2010_2022_df_prev1g|>
    filter(hash_key %in% as.character(hash_truncated_treatments_due_to_retrieval_2019)) |> 
      select(codigo_identificacion) |> 
      #tomar vector
      pull(codigo_identificacion)
)) |> glimpse()

# "dae822f0d02ff173dbb9b4018be895861a6f3628e2ce79a29d5e2bff9d3a90c7"
# "550d7d6c6997bf61c444714e97f6c7aadbce57b03d0b1d77f50f1c116398a9bb" 
# "80c76c2779ad8d2771359f077ab67b0c14b9a6a425378abd2114b18ce3e13202"
# "977faa9ea0b7444beef0e0204117b0a06195032e8e3f95682849e620cab052bb" 
# "90d1f47dae85732447d5815b63e7fdb00fecc53ff687b2d0a39295fffa188753


cat("Check from the last formatted database a sample of RUNs that have missing discharge dates")
SISTRAT23_c1_2010_2022_df_prev1g|>
filter(hash_key %in% c("dae822f0d02ff173dbb9b4018be895861a6f3628e2ce79a29d5e2bff9d3a90c7",
  "550d7d6c6997bf61c444714e97f6c7aadbce57b03d0b1d77f50f1c116398a9bb", 
  "80c76c2779ad8d2771359f077ab67b0c14b9a6a425378abd2114b18ce3e13202",
  "977faa9ea0b7444beef0e0204117b0a06195032e8e3f95682849e620cab052bb",
  "90d1f47dae85732447d5815b63e7fdb00fecc53ff687b2d0a39295fffa188753"
)) |> 
  select(TABLE, hash_key, adm_date_rec, disch_date)

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
cat("Format as most as possible the updated 2019 database in terms of dates")
df2019_mod_maureen <- 
X2019_2019dup_encrip|>
  filter(hashkey %in% hash_truncated_treatments_due_to_retrieval_2019
  )|> 
  #select(TABLE, hash_key, adm_date_rec, disch_date)
  select(hashkey, fecha_ingresoa_tratamiento, fecha_egresode_tratamiento) |> 
  mutate(discharge_date= stringr::str_replace_all(fecha_egresode_tratamiento,"/","-"))%>% 
  mutate(discharge_date= readr::parse_date(discharge_date, format="%d-%m-%Y")) |> 
  mutate(adm_date_orig= stringr::str_replace_all(fecha_ingresoa_tratamiento,"/","-"))%>% 
  mutate(adm_date_orig= readr::parse_date(adm_date_orig, format="%d-%m-%Y"))|> 
  mutate(discharge_date= as.character(discharge_date))|> 
  tidytable::mutate(discharge_date = tidytable::case_when(
    #rn== 1294
    hashkey=="c4795829b6ea9cfc50b988c85deb391fa041d99a0ebca6b68a1378f37e3eb420" & adm_date_orig=="2009-06-30" ~ "2019-12-30",
    #rn== 1934 
    hashkey=="23874d59570adaac6690c85481b869570c10c2f8931fc20636037cdff04af067" & adm_date_orig=="2008-07-02" ~ "2009-05-13",
    #rn== 1938 
    hashkey=="5a16413f76625a09585c89fd3ea4fb05d1ea5cbfbc18247a9fb6e7e21534562d" & adm_date_orig=="2008-07-23" ~ "2009-04-14",
    #rn== 2602 
    hashkey=="11b143acdce4bf1d3a72acd4a703ea8c38543fd02585b4f3b0433e227929ed3c" & adm_date_orig=="2008-03-04" ~ "2009-09-15",
    #rn== 2603 
    hashkey=="986ded00e6ca834805a169ed528655e22f819bf5104d1729b2e1453f20f38065" & adm_date_orig=="2008-12-05" ~ "2009-06-02",
    #rn== 2604 
    hashkey=="d402a1e13f25b2411ca346b0dc84b9fffa45887e628abf09262777b6deae85aa" & adm_date_orig=="2009-06-09" ~ "2009-06-09",
    #rn== 2896
    hashkey=="0d248b372c7224ae2cc1cabb750d6201150175b5d65ec0397ff2127d32b6b675" & adm_date_orig=="2009-02-05" ~ "2009-03-09",
    #rn== 3198
    hashkey== "6eb67e1ead556eb1dbd21951747440057a17a872b33b468a37c9bf781219cef8" & adm_date_orig=="2009-10-07" ~ "2010-04-10",
    #rn== 3260
    hashkey=="e0acff1477306ee93abfca7e251cc6d23db916b390a9fe506fbbefc371ce1d43" & adm_date_orig=="2009-12-07" ~ "2010-06-01",
    #rn== 5175
    hashkey=="eb13b44585501a35df9ce6d262ca6e69e4aa34063af219e19cc95e7609e38cdf" & adm_date_orig=="2010-04-26" ~ "2011-05-03",
    #rn== 5760
    hashkey=="058e8b2c02f98d488a78d78d80435e516c6628cd7edb87ecaf9f8c981d9614ba" & adm_date_orig=="2010-05-03" ~ "2010-10-04",      #rn== 6354
    hashkey=="4d42363412d6a435dd2762bbee7f9b4fe4117ff4c94d55e10472342156238ccb" & adm_date_orig=="2010-06-17" ~ "2010-07-01", 
    #rn== 5760
    hashkey=="058e8b2c02f98d488a78d78d80435e516c6628cd7edb87ecaf9f8c981d9614ba" & adm_date_orig=="2010-05-03" ~ "2010-10-04", 
    #rn== 8176
    hashkey=="228fc5b7b88c5f544f71f9ecfbad4d1750470b717f869a7aa9f01b0169a5d890" & adm_date_orig=="2010-07-01" ~ "2011-01-13", 
    #rn== 8756
    hashkey=="7ebe4155bb7741beef0f30ce47ecbc735bd1f7137d22e81ba21d5f12f8398fa2" & adm_date_orig=="2010-10-04" ~ "2011-01-31", 
    #rn== 5760
    hashkey=="058e8b2c02f98d488a78d78d80435e516c6628cd7edb87ecaf9f8c981d9614ba" & adm_date_orig=="2010-05-03" ~ "2010-10-04", 
    #rn== 9092
    hashkey=="93478aa27b121dbad91cb8e36ef60caa42fce6ca5b99478a77e9b8478df600f3" & adm_date_orig=="2010-11-23" ~ "2011-01-14", 
    #rn== 9171
    hashkey=="6500209f17b52ab4e00a140f7c8f0a10d9b073f81ac9443203f0a1b84c4dc1e8" & adm_date_orig=="2010-11-25" ~ "2011-06-10", 
    #rn== 9177
    hashkey=="4d6e97bfc2aeb15a8c6457ad1c84335de48b5456177b9749159ec2974537634f" & adm_date_orig=="2010-11-25" ~ "2011-06-20", 
    #rn== 9444
    hashkey=="1d5a63a966cea8241228f0057a38ef4e63e0fb353dda174dc95d4393e4cdcefa" & adm_date_orig=="2010-12-02" ~ "2011-06-10", 
    #rn== 10424
    hashkey=="eb13b44585501a35df9ce6d262ca6e69e4aa34063af219e19cc95e7609e38cdf" & adm_date_orig=="2010-04-26" ~ "2011-05-03", 
    #rn== 11482
    hashkey=="228fc5b7b88c5f544f71f9ecfbad4d1750470b717f869a7aa9f01b0169a5d890" & adm_date_orig=="2010-07-01" ~ "2011-01-13",  
    #rn== 12097
    hashkey=="6500209f17b52ab4e00a140f7c8f0a10d9b073f81ac9443203f0a1b84c4dc1e8" & adm_date_orig=="2010-11-25" ~ "2011-06-10",      #rn== 12102
    hashkey=="4d6e97bfc2aeb15a8c6457ad1c84335de48b5456177b9749159ec2974537634f" & adm_date_orig=="2010-11-25" ~ "2011-06-20",      #rn== 12301
    hashkey=="1d5a63a966cea8241228f0057a38ef4e63e0fb353dda174dc95d4393e4cdcefa" & adm_date_orig=="2010-12-02" ~ "2011-06-10",      #rn== 13086
    hashkey=="c75bb8c43963dbad7a1b311497073a58b0e97bb82c5c63a4bc7ae4d1c9014592" & adm_date_orig=="2011-01-13" ~ "2011-07-10",  
    #rn== 13644
    hashkey=="f40999d751e9eb84f5ed6d832d96a1de872599c181e28dd420507c58d7464ccf" & adm_date_orig=="2011-02-08" ~ "2011-08-04", 
    #rn== 14099
    hashkey=="dbe7ddec7591332da15c3c4a1d4a2a1559d455a67b6c31a390ea546ea259c045" & adm_date_orig=="2011-02-10" ~ "2011-05-03", 
    #rn== 14339
    hashkey=="05ff2bf96ef3a294c09b39cf91c19f7a74b080487f13f62c449812f14cefff37" & adm_date_orig=="2011-03-22" ~ "2011-07-31", 
    #rn== 15403
    hashkey=="bdf81829448433489a21d8ac17de96f3765707798d8e2beb7653414f43f272aa" & adm_date_orig=="2011-04-15" ~ "2011-06-12", 
    #rn== 16016
    hashkey=="0bd45263c5217ae4324c23ca4bfec945d4100276fcac4e3e66ad5b6f5341d3fd" & adm_date_orig=="2011-05-20" ~ "2011-06-01", 
    #rn== 16150
    hashkey=="d6d0aaa21c50981871615a6b8886d1f69a3d0f125165f63f6a1c54729be5eea2" & adm_date_orig=="2011-05-23" ~ "2011-06-05",  
    #rn== 16413
    hashkey=="4728851a593a1490d73682e45945fe0f253d0f18dfc12aa1d2d21deef206c39c" & adm_date_orig=="2011-04-18" ~ "2011-08-30", 
    #rn== 16742
    hashkey=="caafb47faaab3c9637821a50ce4dcef33b8e3a9fc275f0ef76f0c93681eb15ba" & adm_date_orig=="2011-06-06" ~ "2011-07-04", 
    #rn== 16745
    hashkey=="18096679bef8db59dbd0ca3be91fa36d7d9dcbbf06b85be2662f410d0146d1a2" & adm_date_orig=="2011-06-17" ~ "2011-07-31", 
    #rn== 16755
    hashkey=="40d3ff594c6c3ddd96e37e5e53fbd22030916a99a4f04cf6283ad188058f2a5b" & adm_date_orig=="2011-06-23" ~ "2011-07-07", 
    #rn== 17500
    hashkey=="667766680894eb203756044682c8445365bb0a831012ec49341b080390133d5d" & adm_date_orig=="2011-06-20" ~ "2011-08-02", 
    #rn== 30449
    hashkey=="60e3066c438a10246353d3a3bce07a58fbfda39465aa84debd48cede21319a94" & adm_date_orig=="2012-10-16" ~ "2013-08-13", 
    #rn== 34193
    hashkey=="60e3066c438a10246353d3a3bce07a58fbfda39465aa84debd48cede21319a94" & adm_date_orig=="2012-10-16" ~ "2013-08-13", 
    #rn== 35638
    hashkey=="08a5dc9a016c0525d7ceea954a8078391701ea9743b71bc2a012f0949952029f" & adm_date_orig=="2013-01-07" ~ "2013-07-17", 
    #rn== 36161
    hashkey=="71049ebb5d958e0647c01c4398c91ff3e02275f7dc5e2fefee5bc263a7653c96" & adm_date_orig=="2013-01-28" ~ "2013-08-12", 
    #rn== 36415
    hashkey=="52e218f6406835e8624ffe71595152560ec44a02a7580d673019eefa88df7a61" & adm_date_orig=="2013-01-29" ~ "2013-04-02", 
    #rn== 37116
    hashkey=="22c282462adfb8e48b3a6b697d533244c9c656a6b31ff87d0180679d9f5ce98d" & adm_date_orig=="2013-02-08" ~ "2013-08-02", 
    #rn== 37958
    hashkey=="221d71ae6c4dba4aee931b3ee518d47fd3972fed3fbf7f4d44c676bedca786c4" & adm_date_orig=="2013-03-18" ~ "2013-07-10", 
    #rn== 38907
    hashkey=="877ea9b68dde038d9f63d04d4e65d1eb27ac3f46af22e310c7c2114feb7f871b" & adm_date_orig=="2013-04-18" ~ "2013-07-31", 
    #rn== 38908
    hashkey=="14af0ddf318fb49877b16491b0fb7df491d98bd32dd854bdbec526f898dd9946" & adm_date_orig=="2013-04-18" ~ "2013-06-17", 
    #rn== 38909
    hashkey=="243a1044f746ae87432532552b4b93b6978fb3b18fa3a4305a11b2af698eb013" & adm_date_orig=="2013-04-16" ~ "2013-07-27", 
    #rn== 39617
    hashkey=="0e729e637c95d5d4486a7f822d14f0f1925ac358fff61d9bba9d7407b8e9abe7" & adm_date_orig=="2013-04-29" ~ "2013-07-25", 
    #rn== 39618
    hashkey=="289a7b6c884980dc60c9171bb05939bacf18a62551ebda723af75cbfc8308db9" & adm_date_orig=="2013-05-08" ~ "2013-07-14", 
    #rn== 39620
    hashkey=="cde086d548022a94e623bfc3d6b34202b28141ed2134ba35425ce4807e75f2fb" & adm_date_orig=="2013-04-29" ~ "2013-07-02", 
    #rn== 40045
    hashkey=="10fc40384411161967b222bf530a0378e0ae585bd69370d57d9c4fb49a1a34c3" & adm_date_orig=="2013-05-22" ~ "2013-08-02",  
    #rn== 40293
    hashkey=="67353760ae53ad8963176af0ec6cab9c4bdad13b9e53058e68e53f80b409b224" & adm_date_orig=="2013-05-29" ~ "2013-08-07",
    #rn== 40599
    hashkey=="3ce639d4d0330242d1f7c1e6496e834ad3fa2b41bef89b09bc373e9dede8c981" & adm_date_orig=="2013-05-02" ~ "2013-07-03",
    #rn== 41114
    hashkey=="5e6d9dcec9e717d4536f7cfa5cc0f713e7c2c7933058aeb9a37fec0a24da5151" & adm_date_orig=="2013-06-06" ~ "2013-07-31",
    #rn== 41117
    hashkey=="e01e3218ba73e9d26178e7a6aceb86357695bc88117f1d7b89c8adbf55210528" & adm_date_orig=="2013-06-05" ~ "2013-06-27",
    #rn== 42456
    hashkey=="421abbc2c85687aa87adec1c3146debf5ddea3ea71f65d708c2cf4d4dde86e38" & adm_date_orig=="2013-07-02" ~ "2013-07-08",
    #rn== 42633
    hashkey=="567f1fd735550a9bc1a2ea8a838d87b69369caa106c2d0cd0a1b38581d09919f" & adm_date_orig=="2013-07-09" ~ "2013-08-16",
    #rn== 42634
    hashkey=="7f259b5289b209cc669db813abfcd14519a21c4f69aaeb0190f094c61a52afad" & adm_date_orig=="2013-06-28" ~ "2013-07-09", 
    #rn== 42854
    hashkey=="49cca05a51baac5c836a053eac96674c775e2d7164209a04f09f8da34952b789" & adm_date_orig=="2013-07-02" ~ "2013-08-02",
    #rn== 43076
    hashkey=="6adbbaff91e32138777abcf66a161d953722255c88368f9a5877d1ddfa48decd" & adm_date_orig=="2013-08-06" ~ "2013-08-20",
    #rn== 43181
    hashkey=="02c866ee44e5a3a310cf18728753e3a4c3751d4ea4d61edc22d78606cde0fcc8" & adm_date_orig=="2013-08-01" ~ "2013-08-16",
    #rn== 43182
    hashkey=="506be60207917af56fa39175f11ee5b3b874c0883245e37d0b2a79e0b24f08ad" & adm_date_orig=="2013-08-01" ~ "2013-08-22",
    TRUE ~ as.character(discharge_date)
  )) |> 
  tidytable::mutate(discharge_date= readr::parse_date(discharge_date, format="%Y-%m-%d"))


cat("#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:\n")
cat("#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:\n")
cat("#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:\n")
cat("De la base de datos ya formateada en pasos previos, filtramos los que tienen fecha de egreso missing, les generamos una fecha de egreso ficticia sumando los días de tratamiento\n")
cat("Seleccionamos columnas de interés, y filtramos aquellas con fechas menores al 28 de Abril de 2023")
cat("Unimos registros que coincida la fecha de ingreso y el HASH con la base de 2019 actualizada y algo formateada para traer la columna disch_date_na, que sería\n") 
cat("la nueva información de egreso, sin truncamiento administrativo\n")

SISTRAT23_c1_2010_2022_df_prev1g|>
  filter(is.na(disch_date_num)) |> 
  mutate(disch_date_na= as.Date(adm_date_rec_num+ dias_en_tratamiento, origin = "1970-01-01")) |>
  (\(df){
    cat("Table of dates of discharge with days in treatment\n")
    print(table(df$disch_date_na))
    cat("We should discard dates previous to 2023-04-28 because they were part of the actuala dministrative truncation process\n\n")
    df
  })() |> 
  select(TABLE_rec, rn, hash_key, dias_en_tratamiento, adm_age_rec, adm_date_rec, disch_date, disch_date_na, id_centro, tr_compliance, plan_type, senda) |> 
  #filter(disch_date_na<"2023-04-28" & grepl("currently",tr_compliance)) |> 
  filter(disch_date_na<"2023-04-28")|> 
  #Take only 
  inner_join(df2019_mod_maureen, by= c("hash_key"="hashkey", "adm_date_rec"="adm_date_orig")) |> 
  select(-fecha_ingresoa_tratamiento, -fecha_egresode_tratamiento)|> 
  (\(df) { 
    cat(paste0("New discharge date from updated C1 2019 database (discarding discharges in 2024-04-28; COINCIDENCE by HASH & admission date), cases: ", formatC(nrow(filter(df, !is.na(discharge_date))), big.mark=",")),"\n")
    cat(paste0("New discharge date from updated C1 2019 database (discarding discharges in 2024-04-28; COINCIDENCE by HASH & admission date), RUNs: ", formatC(nrow(distinct(filter(df, !is.na(discharge_date)), hash_key)), big.mark=","),"\n\n"))
    cat("Lets check yearly database origin. Where do they come from?...\n")
    print(janitor::tabyl(df, TABLE_rec))
    cat("\n\nDiscarding discharges in 2024-04-28; coincidence by HASH & adm date= PLOT\n")
    print(df |> mutate(adm_num = unclass(adm_date_rec), miss= factor(ifelse(is.na(discharge_date),1,0))) %>%      # 1) fechas → número de días desde 1970-01-01
      ggplot(aes(x = adm_num, fill=miss), alpha=.6) +                       # 2) histograma
        scale_fill_manual(
          name   = "Updated\ndate\nfound",                                 # título de la leyenda (puedes dejar "" si no quieres título)
          values = c("1" = "#1f78b470", "0" = "#e31a1c70"),     # dos colores personalizados
          labels = c("Yes", "No")                              # dos etiquetas para los niveles, en orden de aparición
        ) +
      geom_histogram(binwidth = 30) +                  #   30 ≃ 1 mes; ajústalo a tu gusto
      scale_x_continuous(
        breaks  = scales::pretty_breaks(10),                   #   10 marcas “lógicas”
        labels  = ~ format(as.Date(.x, origin = "1970-01-01"), "%y-%m")  # 3) números → fechas legibles
      ) +
      labs(
        title = NULL,#"Admission dates",
        x     = "Year and admission month",
        y     = "Number of cases"
      ) +
      theme_minimal()+ theme(    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    #df
  })()  
  
cat(paste0("17467 tenía el 2019 pacientes vs. los ", nrow(X2019_2019dup_encrip), " encriptados por Maureen el 2025. Es decir, todavía faltan casos.\n"))


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
cat("2025-06-05")

SISTRAT23_c1_2010_2022_df$rn<-seq_along(SISTRAT23_c1_2010_2022_df$hash_key)

SISTRAT23_c1_2010_2022_df |> 
  mutate(codigo_identificacion=toupper(codigo_identificacion))|> 
  filter(hash_key %in% hash_truncated_treatments_due_to_retrieval_2019) |> 
  filter(!hash_key %in% hashs_dates_updated_disch_date$hash_key) |> 
  select(TABLE, hash_key, codigo_identificacion, fecha_ingresoa_tratamiento, fecha_egresode_tratamiento) |> 
  filter(TABLE >2017, TABLE<2020)|>
  rio::export(paste0(here::here(), "/_out/perdidos_para_maureen_codint.xlsx"))







