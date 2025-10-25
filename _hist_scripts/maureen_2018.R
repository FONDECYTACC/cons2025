
wdpath<-
  paste0(gsub("/cons","",gsub("cons","",paste0(getwd(),"/cons"))))
envpath<-
if(!file.exists(paste0(wdpath,"renv_info.txt"))){gsub("G:","H:",wdpath)}else{wdpath}
base::load(paste0(envpath,"data/20241015_out/","3_ndp_2024_11_08.Rdata"))

invisible("Mi base:")
invisible("https://drive.google.com/file/d/1UGX7Ip43LY2IeSA7BZokXvJBvQVvBe5g/view?usp=sharing")

library(dplyr)
library(tidytable)



# datos -------------------------------------------------------------------


year <-2019

data<- as.data.frame(SISTRAT23_c1_2010_2022)
#data<- as.data.frame(SISTRAT23_c1_2010_2022_df_prev1d)
#data<-rio::import("E:/Mi unidad/Alvacast/SISTRAT 2019 (github)/Encriptados c1 - Minsal/2018_encrip.csv")
#data<-rio::import("E:/Mi unidad/Alvacast/SISTRAT 2019 (github)/Encriptados c1 - Minsal/2019_encrip.csv")

invisible("para filtrar informe 2020, del 2019")
# data[data$TABLE_rec %in% c( "2010",  "2011",  "2012",  "2013", "20141", 
#                             "20142", "20151", "20152", "20161", "20162", 
#                             "20171", "20172", "20181", "20182"),] |> nrow()
# 
# data<-data[data$TABLE_rec %in% c( "2010",  "2011",  "2012",  "2013", "20141", 
#                             "20142", "20151", "20152", "20161", "20162", 
#                             "20171", "20172", "20181", "20182"),]


data_informe_senda <- data.frame(
  Año = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
  `Personas (RUT distintos)` = c(8706, 9369, 9930, 13689, 15627, 17869, 18110, 18573, 17907, 17256, 13749, 15935),
  `Personas (Casos)` = c(9305, 10058, 10672, 14653, 16672, 19256, 19309, 19885, 19312, 18816, 14581, 17174)
)

#get database retrieval year
data<-
  data[grepl(as.character(year),data$TABLE_rec),]

try(
data$fecha_ingresoa_tratamiento<- readr::parse_date(data$fecha_ingresoa_tratamiento, 
                                                    "%d/%m/%Y")
)
try(
data$fecha_egresode_tratamiento<- readr::parse_date(data$fecha_egresode_tratamiento, 
                                                    "%d/%m/%Y")
)
if(!is.null(data$`Fecha Ingreso a Tratamiento`)){
  try(
data$`Fecha Ingreso a Tratamiento`<- readr::parse_date(data$`Fecha Ingreso a Tratamiento`, 
                                                    "%d/%m/%Y")
)
  try(
data$`Fecha Egreso de Tratamiento`<- readr::parse_date(data$`Fecha Egreso de Tratamiento`, 
                                                    "%d/%m/%Y")
)
}

nrow(data)
try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) &  
                    fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> nrow()
)
invisible("la mia (2023- mod) baja al toque a 14173")
try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) &  
                    is.na(fecha_egresode_tratamiento)) |> nrow()
)


invisible("si maureen no lo hace, yo tpoko")
# data$fecha_ingresoa_tratamiento[is.na(data$fecha_ingresoa_tratamiento)]  <- 
#   as.Date(data$fecha_ingresoa_tratamiento[is.na(data$fecha_ingresoa_tratamiento)],
#           format="%d/%m/%y")
# data$fecha_egresode_tratamiento[is.na(data$fecha_egresode_tratamiento)]  <- 
#   as.Date(data$fecha_egresode_tratamiento[is.na(data$fecha_egresode_tratamiento)],
#           format="%d/%m/%y")

if(is.null(data$`Tipo de Plan`)){
try(
data <- data[data$senda != "no", ]
)
try(
data <- data[data$senda != "No", ]
)
}
if(!is.null(data$`Tipo de Plan`)){
try(
data <- data[data$SENDA != "No", ]
)
}

nrow(data)
try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) &  
                    fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> nrow()
)
invisible("la mia (2023- mod) baja al toque a 14173")
try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) &  
                    is.na(fecha_egresode_tratamiento)) |> nrow()
)

invisible("Sólo para distinto a 2018")
if(year!=2018){
  try(
    data$tipode_plan <- ifelse(data$tipode_plan == "M PAI (p)", "M-PAI", data$tipode_plan)
    )
  try(  
    data$tipode_plan <- ifelse(data$tipode_plan == "PG PAI 2", "PG-PAI", data$tipode_plan)
  )
}
# 2. Eliminar 'libertad vigilada' en tipo_de_plan
#table(data$tipode_plan)

# 3. Convertir valores de 'PR fijo', 'PRFlexible' y 'PR fijo 2' a 'PG PR' en tipo_de_plan
try(
data$tipode_plan <- ifelse(data$tipode_plan %in% c("pr fijo", "prflexible", "pr fijo 2"), "pg pr", 
                            data$tipode_plan)
)
try(
data$tipode_plan <- ifelse(data$tipode_plan %in% c("PR fijo", "PRflexible", "PR fijo 2"), "PG-PR", 
                            data$tipode_plan)
)
if(!is.null(data$`Tipo de Plan`)){
  data$`Tipo de Plan` <- ifelse(data$`Tipo de Plan` %in% c("PR fijo", "PRflexible", "PR fijo 2"), "PG-PR", 
                                data$`Tipo de Plan`)
}

# 4. Cambiar Alcohol-plan a PG-plan en tipo_de_plan
if(!is.null(data$`Tipo de Plan`)){
try(
data$tipode_plan <- ifelse(data$tipode_plan == "Alcohol-plan", "PG-plan", data$tipode_plan)
)
# 5. Cambiar OTRO-plan a PG-plan en tipo_de_plan
try(
data$tipode_plan <- ifelse(data$tipode_plan == "otro", "pg-pr", data$tipode_plan)
)
try(
data$tipode_plan <- ifelse(data$tipode_plan == "Otro", "PG-PR", data$tipode_plan)
)
}
if(!is.null(data$`Tipo de Plan`)){
  # 4. Cambiar Alcohol-plan a PG-plan en tipo_de_plan
  data$`Tipo de Plan` <- ifelse(data$`Tipo de Plan` == "Alcohol-plan", 
                                "PG-plan", data$`Tipo de Plan`)
  data$`Tipo de Plan` <- ifelse(data$`Tipo de Plan` == "PG PAI 2", 
                                "PG-PAI", data$`Tipo de Plan`)

  data$`Tipo de Plan` <- ifelse(data$`Tipo de Plan` == "Otro", 
                                "PG-PR", data$`Tipo de Plan`)
}

# 6. Si tipo_de_programa es "M-PAI" y no está en mujeres, cambiar a "mujeres"
if(is.null(data$`Tipo de Plan`)){
try(
data$tipode_programa <- ifelse(data$tipode_plan == "m-pai" & 
                                 data$tipode_programa != "programa especifico mujeres", 
                               "programa especifico mujeres", data$tipode_programa)
)
try(
data$tipode_programa <- ifelse(data$tipode_plan == "M-PAI" & 
                                 data$tipode_programa != "Programa EspecÃƒÂ­fico Mujeres", 
                               "Programa EspecÃƒÂ­fico Mujeres", data$tipode_programa)
)
}
if(!is.null(data$`Tipo de Plan`)){
  data$`Tipo de Programa` <- ifelse(data$`Tipo de Plan` == "M-PAI" & 
                                   data$`Tipo de Programa` != "Programa Específico Mujeres", 
                                 "Programa Específico Mujeres", data$`Tipo de Programa`)
}

# 7. Mantener solo los planes específicos en tipo_de_plan
# 

allowed_plans <- c("m-pai", "m-pr", "pg-pab", "pg-pai", "pg-pr")

if(length(data$tipode_plan[grepl("m-pai",data$tipode_plan)])>0){
  data <- data[data$tipode_plan %in% allowed_plans, ]  
} else if (!is.null(data$tipode_plan)){
  data <- data[data$tipode_plan %in% toupper(allowed_plans), ]  
}

if(!is.null(data$`Tipo de Plan`)){
  data <- data[data$`Tipo de Plan` %in% toupper(allowed_plans), ] 
}

nrow(data)

try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
                    fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> nrow()
)
try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
                    is.na(fecha_egresode_tratamiento)) |> nrow()
)
invisible("Con esto la del 2023 sin tocar, queda en 25815")
invisible("La mía queda en 14172 obs, eso si con el filtro de TABLE_rec aplicado")

invisible("Base sin-mod-2021, 30901")

# 9. Eliminar duplicados por id, fech_ing y tipo_de_plan
 if((is.null(data$adm_date_rec) & !is.null(data$codigo_identificacion))){
  # data <- data[!duplicated(data[, c("codigo_identificacion", 
  #                                   "adm_date_rec", 
  #                                   "tipode_plan")]), ]
  # Crear una columna que indica duplicados en las combinaciones de id, fech_ing y tipo_de_plan
  # 
   # Crear la columna `duplica1` que indica duplicados en la combinación de codigo_identificacion, fech_ing y tipo_de_plan
   # Crear la columna `duplica1` que indica duplicados en la combinación de codigo_identificacion, fech_ing y tipo_de_plan
   data <- data |>
     tidytable::arrange(codigo_identificacion, 
                        fecha_ingresoa_tratamiento, 
                        tipode_plan, 
                        fecha_egresode_tratamiento)
   # Crear las columnas duplica1 y duplicaid, y filtrar filas duplicadas
   data <- data |>
     tidytable::group_by(codigo_identificacion, 
                         fecha_ingresoa_tratamiento, 
                         tipode_plan) |>
     tidytable::mutate(
       duplica1 = .N > 1,  # Marcar duplicados en el grupo
       duplicaid = seq_len(.N)  # Numerar las filas dentro de cada grupo, similar a _n en Stata
     ) |>
     tidytable::ungroup() |>
     tidytable::filter(!(duplicaid == 1 & duplica1)) |>
     tidytable::select(-duplica1, -duplicaid)  # Eliminar las columnas auxiliares

}

if (!is.null(data$adm_date_rec)) {
   # Crear la columna `duplica1` que indica duplicados en la combinación de codigo_identificacion, fech_ing y tipo_de_plan
   data <- data |>
     tidytable::arrange(codigo_identificacion, 
              adm_date_rec, 
              tipode_plan, 
              disch_date)
   # Crear las columnas duplica1 y duplicaid, y filtrar filas duplicadas
   data <- data |>
     tidytable::group_by(codigo_identificacion, 
                         adm_date_rec, 
                         tipode_plan) |>
     tidytable::mutate(
       duplica1 = .N > 1,  # Marcar duplicados en el grupo
       duplicaid = seq_len(.N)  # Numerar las filas dentro de cada grupo, similar a _n en Stata
     ) |>
     tidytable::ungroup() |>
     tidytable::filter(!(duplicaid == 1 & duplica1)) |>
     tidytable::select(-duplica1, -duplicaid)  # Eliminar las columnas auxiliares   
 } 

if (!is.null(data$`Codigo Identificación`)) {
   data <- data |>
     tidytable::arrange(`Codigo Identificación`, 
                        `Fecha Ingreso a Tratamiento`, 
                        `Tipo de Plan`, 
                        `Fecha Egreso de Tratamiento`)
   # Crear las columnas duplica1 y duplicaid, y filtrar filas duplicadas
   data <- data |>
     tidytable::group_by(`Codigo Identificación`, 
                         `Fecha Ingreso a Tratamiento`, 
                         `Tipo de Plan`) |>
     tidytable::mutate(
       duplica1 = .N > 1,  # Marcar duplicados en el grupo
       duplicaid = seq_len(.N)  # Numerar las filas dentro de cada grupo, similar a _n en Stata
     ) |>
     tidytable::ungroup() |>
     tidytable::filter(!(duplicaid == 1 & duplica1)) |>
     tidytable::select(-duplica1, -duplicaid)  # Eliminar las columnas auxiliares        
   
   }

nrow(data)

try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
                    fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> nrow()
)
try(
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
                    is.na(fecha_egresode_tratamiento)) |> nrow()
)

invisible("**********************************************************")
invisible("************************solo 2018*************************")
invisible("**********************************************************")
#*18 existen 1codigos que son dos rut diferentes. se le agrega un numero mas a estos ALCA103021988
#replace id="ALCA1030219881" if id=="ALCA103021988" & NombreCentro=="CESFAM Boca Sur"

if(year==2018){
  try(
  data$codigo_identificacion <- ifelse(data$codigo_identificacion == "ALCA103021988" & 
                                          data$nombre_centro == "CESFAM Boca Sur", "ALCA1030219881", 
                                          data$codigo_identificacion)
  )
  try(
  data$codigo_identificacion <- ifelse(data$codigo_identificacion == "alca103021988" & 
                                        data$nombre_centro == "cesfam boca sur", "alca1030219881", 
                                        data$codigo_identificacion)
  )
  if (!is.null(data$`Codigo Identificación`)) {
  try(
    data$`Codigo Identificación` <- ifelse(data$`Codigo Identificación` == "ALCA103021988" & 
                                           data$`Nombre Centro` ==  "CESFAM Boca Sur", "ALCA1030219881", 
                                         data$`Codigo Identificación`)
  )
 }

  # 19. Eliminar proyectos de mujeres específicos
  try(
  data <- 
  data[!grepl("orion lagunillas", data$nombre_centro),]
  )
  try(
  data <- 
  data[!grepl("ORION Lagunillas", data$nombre_centro),]
  )
  if (!is.null(data$`Codigo Identificación`)) {
    try(
    data <- 
      data[!grepl("ORION Lagunillas", data$`Nombre Centro`),]
    )
  }
}

invisible("ESTO NO SIRVE!!!")


#para ver ID
if((!is.null(data$adm_date_rec) & is.null(data$fecha_ingresoa_tratamiento))){
  data |>
  dplyr::filter(adm_date_rec <= as.Date(paste0(year,"-12-31")) & 
                  disch_date >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(codigo_identificacion) |> 
  nrow()
} 
if(!is.null(data$fecha_ingresoa_tratamiento)) {
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
                    fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(codigo_identificacion) |> 
    nrow()  
} 
if (!is.null(data$`Codigo Identificación`)) {
  data |>
    dplyr::filter(`Fecha Ingreso a Tratamiento` <= as.Date(paste0(year,"-12-31")) & 
                    `Fecha Egreso de Tratamiento` >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(`Codigo Identificación`) |> 
    nrow()  
}
#para ver RUN
if((!is.null(data$adm_date_rec) & is.null(data$fecha_ingresoa_tratamiento))){
  data |>
    dplyr::filter(adm_date_rec <= as.Date(paste0(year,"-12-31")) & 
                    disch_date >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(hash_key) |> 
    nrow()
} 
if(!is.null(data$fecha_ingresoa_tratamiento)) {
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
                    fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(hash_key) |> 
    nrow()  
} 
if (!is.null(data$`Codigo Identificación`)) {
  data |>
    dplyr::filter(`Fecha Ingreso a Tratamiento` <= as.Date(paste0(year,"-12-31")) & 
                    `Fecha Egreso de Tratamiento` >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(RUN) |> 
    nrow()  
}

#Para ver casos
if((!is.null(data$adm_date_rec) & is.null(data$fecha_ingresoa_tratamiento))){
  data |>
    dplyr::filter(adm_date_rec <= as.Date(paste0(year,"-12-31")) & 
                    disch_date >= as.Date(paste0(year,"-01-01"))) |> 
    #distinct(hash_key) |> 
    nrow()
} 
if(!is.null(data$fecha_ingresoa_tratamiento)) {
  data |>
    dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
                    fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    #distinct(hash_key) |> 
    nrow()  
} 
if (!is.null(data$`Codigo Identificación`)) {
  data |>
    dplyr::filter(`Fecha Ingreso a Tratamiento` <= as.Date(paste0(year,"-12-31")) & 
                    `Fecha Egreso de Tratamiento` >= as.Date(paste0(year,"-01-01"))) |> 
    #dplyr::distinct(RUN) |> 
    nrow()  
}

cat("Ahora sin filtro")


#para ver ID
if((!is.null(data$adm_date_rec) & is.null(data$fecha_ingresoa_tratamiento))){
  data |>
    # dplyr::filter(adm_date_rec <= as.Date(paste0(year,"-12-31")) & 
    #                 disch_date >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(codigo_identificacion) |> 
    nrow()
} 
if(!is.null(data$fecha_ingresoa_tratamiento)) {
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(codigo_identificacion) |> 
    nrow()  
} 
if (!is.null(data$`Codigo Identificación`)) {
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(`Codigo Identificación`) |> 
    nrow()  
}
#para ver RUN
if((!is.null(data$adm_date_rec) & is.null(data$fecha_ingresoa_tratamiento))){
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(hash_key) |> 
    nrow()
} 
if(!is.null(data$fecha_ingresoa_tratamiento)) {
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(hash_key) |> 
    nrow()  
} 
if (!is.null(data$`Codigo Identificación`)) {
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    dplyr::distinct(RUN) |> 
    nrow()  
}

#Para ver casos
if((!is.null(data$adm_date_rec) & is.null(data$fecha_ingresoa_tratamiento))){
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    #distinct(hash_key) |> 
    nrow()
} 
if(!is.null(data$fecha_ingresoa_tratamiento)) {
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    #distinct(hash_key) |> 
    nrow()  
} 
if (!is.null(data$`Codigo Identificación`)) {
  data |>
    # dplyr::filter(fecha_ingresoa_tratamiento <= as.Date(paste0(year,"-12-31")) & 
    #                 fecha_egresode_tratamiento >= as.Date(paste0(year,"-01-01"))) |> 
    #dplyr::distinct(RUN) |> 
    nrow()  
}

cat("TIpo de centro")

data |>
  janitor::tabyl(tipo_centro) 
data |>
  janitor::tabyl(tipode_programa) 