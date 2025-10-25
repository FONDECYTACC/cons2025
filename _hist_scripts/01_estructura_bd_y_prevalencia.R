
# 0. paquetes y setting ---------------------------------------------------

# remover objetos y memoria utilizada
rm(list=ls());gc()
# paquetes en R
if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
if(!require(janitor)){install.packages("janitor");require(janitor)}
if(!require(DiagrammeR)){install.packages("DiagrammeR");require(DiagrammeR)}
if(!require(arrow)){install.packages("arrow");require(arrow)}
if(!require(rsvg)){install.packages("rsvg");require(rsvg)}
if(!require(DiagrammeRsvg)){install.packages("DiagrammeRsvg");require(DiagrammeRsvg)}
if(!require(webshot)){install.packages("webshot");require(webshot)}
if(!require(pROC)){install.packages("pROC");require(pROC)}
if(!require(caret)){install.packages("caret");require(caret)}
if(!require(ggplot2)){install.packages("ggplot2");require(ggplot2)}
if(!require(polycor)){install.packages("polycor");require(polycor)}
if(!require(ROCit)){install.packages("ROCit");require(ROCit)}
if(!require(FactoMineR)){install.packages("FactoMineR");require(FactoMineR)}
if(!require(glca)){install.packages("glca");require(glca)}
if(!require(parallel)){install.packages("parallel");require(parallel)}
if(!require(naniar)){install.packages("naniar");require(naniar)}
if(!require(epitools)){install.packages("epitools");require(epitools)}
if(!require(epiR)){install.packages("epiR");require(epiR)}
if(!require(psych)){install.packages("psych");require(psych)}
if(!require(boot)){install.packages("boot");require(boot)}
if(!require(tableone)){install.packages("tableone");require(tableone)}
if(!require(lubridate)){install.packages("lubridate");require(lubridate)}


# 1. construir bases de datos ---------------------------------------------------

#este sí
#:#:#:#:#:#:#:#:#:
# X2023_12_05_DatosEgresosHosp_encrip <- 
#   read.delim("E:/Mi unidad/Alvacast/SISTRAT 2023/data/20231205_original_data/EH_2010_2022_Pasantes_encrip.csv", sep=";")
# 
# dtX2023_12_05_DatosEgresosHosp_encrip<-data.table::data.table(janitor::clean_names(X2023_12_05_DatosEgresosHosp_encrip))
# rm(X2023_12_05_DatosEgresosHosp_encrip)
#:#:#:#:#:#:#:#:#:


invisible("Exploración de datos")

# > length(unique(dtX2023_12_05_DatosEgresosHosp_encrip$run))
# [1] 9646325
# > length(dtX2023_12_05_DatosEgresosHosp_encrip$run)
# [1] 20957004

# > table(dtX2023_12_05_DatosEgresosHosp_encrip$cond_egr)
# 1        2 
# 20475745   481259 
# > prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip$cond_egr))
# 1          2 
# 0.97703589 0.02296411 

#obtengo la base de datos de hospitalizaciones filtrada en python
dtX2023_12_05_DatosEgresosHosp_encrip_filt<-rio::import("20231205_hosp_filt.parquet.gzip")
dtX2023_12_05_DatosEgresosHosp_encrip<-rio::import("20231205_hosp.parquet.gzip")

#paste0("https://drive.google.com/file/d/",as.character(readLines("secret_link.txt")),"/view?usp=sharing")

# Acreditación de pertenencia a PPOO, obtenida por CONADI
# Variable autorreportada de pertenencia a PPOO, obtenida por RSH
# PUEBLO_ORIGINARIO= Variable autorreportada de pertenencia a PPOO, obtenida por MINSAL
# GLOSA_PAIS_ORIGEN= Glosa País de origen
# ESTAB= Código establecimiento
# ESTAB_HOMO= Código establecimiento homologado
# FECHA_INGRESO= Fecha de ingreso del paciente, formato DD-MM-AA
# AREAF_EGR= Nivel de cuidado del que egreso el usuario
# FECHA_EGRESO= Fecha de alta del papciente, formato DD-MM-AA
# PREVI= Previsión usuario (ISAPRE, FONASA, etc.)
# BENEF= Tramo usuario beneficiario de FONASA (A, B, etc.)
# SEXO= Código del sexo biologico del usuario
# TIPO_EDAD= Tipo de edad de usuario
# EDAD_ANOS= Edad en años
# DIAG1= Diagnóstico principal del usuario
# DIAG2= Causa externa de hospitalización
# COND_EGR= Condición del paciente al egreso: vivo (0) y fallecido (1).

set.seed(2125)  # Semilla aleatoria
# Genera una muestra más pequeña para entender los datos
dtX2023_12_05_DatosEgresosHosp_encrip_samp <- dtX2023_12_05_DatosEgresosHosp_encrip[sample(nrow(dtX2023_12_05_DatosEgresosHosp_encrip), 500), ]


invisible("Personas con al menos un diagnóstico de los que menciono")
# 26,568

# causas CIE-10 relacionadas al consumo de drogas y alcohol (F10; K70, K73-K74 ; I10-I15; Y10-Y14; F11-F16, F18-F19; B20-B24)
# otras causas (ej., C16; C56; C61; C81; C82–C85, C96; F01–F03, G30–G31)

#20231208: no lo necesito ya 
# #generamos un índice numérico de observaciones
# dtX2023_12_05_DatosEgresosHosp_encrip$rn<-1:length(dtX2023_12_05_DatosEgresosHosp_encrip$run)


## 1b. comparar con datos ANID ---------------------------------------------------

#ingresar CSV
X2023_11_07_DatosEgresosHosp_encrip <-
  read.delim("E:/Mi unidad/Alvacast/SISTRAT 2023/data/20231107_egres_hosp/2023-11-07  DatosEgresosHosp_encrip.csv", sep="~")
#formatear base
dtX2023_11_07_DatosEgresosHosp_encrip<-data.table::data.table(janitor::clean_names(
  X2023_11_07_DatosEgresosHosp_encrip
  ))
#113180, 6 letras

#eliminar el base 
rm(X2023_11_07_DatosEgresosHosp_encrip) #113180 formato homolog

#para homologar id de centro
#https://docs.google.com/spreadsheets/d/1Ztg38BGjUzbY7acVfh4rXaXYaOi8EBuX/edit#gid=243483182
homolog_estab_misal<-
  readr::read_delim("https://docs.google.com/spreadsheets/d/1Ztg38BGjUzbY7acVfh4rXaXYaOi8EBuX/export?format=tsv&id=1Ztg38BGjUzbY7acVfh4rXaXYaOi8EBuX&gid=243483182")

invisible("adaptar el código")
dtX2023_12_05_DatosEgresosHosp_encrip$estab_homo_rec <- 
  gsub("^([0-9]{2})([0-9]{3})$", "\\1-\\2", dtX2023_12_05_DatosEgresosHosp_encrip$estab_homo)

invisible("unir bases de datos originales ")
dtX2023_12_05_DatosEgresosHosp_encrip_a<-
dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  dplyr::left_join(homolog_estab_misal[,1:2], by=c("estab_homo_rec"="Código Antiguo Establecimiento"))

table(dtX2023_12_05_DatosEgresosHosp_encrip_a$estab_homo_rec, exclude=NULL)
structure(c(`10-110` = 194013L, `10-120` = 834L, `10-121` = 49670L, 
            `10-122` = 316L, `10-123` = 3338L, `10-210` = 220425L, `10-220` = 92155L, 
            `10-222` = 861L, `10-310` = 388511L, `10-320` = 124310L, `10-321` = 115545L, 
            `10-322` = 85798L, `10-325` = 3237L, `10-410` = 238888L, `10-420` = 4233L, 
            `10-424` = 37368L, `10-510` = 559438L, `10-520` = 80268L, `10-526` = 1939L, 
            `10-610` = 480636L, `10-620` = 19534L, `10-621` = 48662L, `10-710` = 613708L, 
            `10-711` = 6198L, `10-720` = 225061L, `10-721` = 217793L, `10-722` = 196945L, 
            `10-726` = 1099L, `10-810` = 289526L, `10-820` = 14392L, `10-910` = 524143L, 
            `10-920` = 736874L, `11-010` = 289897L, `11-011` = 45871L, `11-012` = 212805L, 
            `11-013` = 86067L, `11-014` = 32395L, `11-015` = 92219L, `11-016` = 5047L, 
            `11-020` = 1108L, `11-027` = 18742L, `11-110` = 434467L, `11-119` = 119607L, 
            `11-120` = 344860L, `11-121` = 99875L, `11-122` = 41431L, `11-123` = 113754L, 
            `11-127` = 55998L, `11-129` = 199321L, `11-135` = 12308L, `11-210` = 682962L, 
            `11-220` = 573410L, `11-221` = 620132L, `11-222` = 14754L, `11-223` = 103289L, 
            `11-224` = 764587L, `11-225` = 65813L, `11-226` = 78741L, `11-227` = 19665L, 
            `11-250` = 16941L, `11-251` = 20479L, `11-252` = 54006L, `11-253` = 141248L, 
            `11-295` = 124616L, `11-298` = 1179L, `11-299` = 81L, `11-310` = 362856L, 
            `11-313` = 94552L, `11-315` = 72871L, `11-316` = 27280L, `11-317` = 2340L, 
            `11-318` = 192180L, `11-319` = 100482L, `11-321` = 2840L, `11-322` = 12L, 
            `11-324` = 40L, `11-410` = 945032L, `11-415` = 672L, `11-420` = 5756L, 
            `11-421` = 1761L, `11-422` = 149348L, `11-505` = 1026L, `11-510` = 521708L, 
            `11-511` = 85091L, `11-520` = 88111L, `11-522` = 163003L, `11-524` = 1877L, 
            `11-605` = 4908L, `11-610` = 743064L, `11-611` = 132381L, `11-620` = 43325L, 
            `11-626` = 70873L, `11-710` = 419655L, `11-720` = 31122L, `11-722` = 64885L, 
            `11-810` = 629497L, `11-820` = 232988L, `11-821` = 1716L, `11-850` = 8711L, 
            `11-851` = 1160L, `11-910` = 338601L, `11-920` = 251703L, `12-010` = 428265L, 
            `12-020` = 75273L, `12-105` = 1318L, `12-110` = 388455L, `12-111` = 241729L, 
            `12-112` = 83472L, `12-120` = 271394L, `12-210` = 403107L, `12-220` = 137991L, 
            `12-222` = 754L, `12-310` = 242146L, `12-320` = 72200L, `12-410` = 284613L, 
            `12-411` = 17283L, `12-412` = 18390L, `12-413` = 16860L, `12-414` = 5258L, 
            `12-421` = 62694L, `12-425` = 58999L, `12-426` = 62635L, `12-427` = 1903L, 
            `12-438` = 2429L, `12-510` = 136659L, `12-520` = 130L, `12-610` = 173218L, 
            `12-620` = 83766L, `12-670` = 343L, `12-810` = 82327L, `12-811` = 118843L, 
            `12-910` = 260169L, `13-315` = 139533L, `13-316` = 32785L, `13-317` = 2042L, 
            `13-320` = 514L, `20-005` = 57785L, `20-006` = 23436L, `20-008` = 217L, 
            `20-009` = 3346L, `20-019` = 1382L, `20-023` = 30599L, `20-028` = 356L, 
            `20-048` = 61626L, `20-055` = 903L, `20-056` = 424L, `20-070` = 414L, 
            `20-071` = 9834L, `20-072` = 177L, `20-086` = 253L, `20-088` = 1908L, 
            `20-101` = 7L, `20-108` = 20L), dim = 153L, dimnames = structure(list(
              c("10-110", "10-120", "10-121", "10-122", "10-123", "10-210", 
                "10-220", "10-222", "10-310", "10-320", "10-321", "10-322", 
                "10-325", "10-410", "10-420", "10-424", "10-510", "10-520", 
                "10-526", "10-610", "10-620", "10-621", "10-710", "10-711", 
                "10-720", "10-721", "10-722", "10-726", "10-810", "10-820", 
                "10-910", "10-920", "11-010", "11-011", "11-012", "11-013", 
                "11-014", "11-015", "11-016", "11-020", "11-027", "11-110", 
                "11-119", "11-120", "11-121", "11-122", "11-123", "11-127", 
                "11-129", "11-135", "11-210", "11-220", "11-221", "11-222", 
                "11-223", "11-224", "11-225", "11-226", "11-227", "11-250", 
                "11-251", "11-252", "11-253", "11-295", "11-298", "11-299", 
                "11-310", "11-313", "11-315", "11-316", "11-317", "11-318", 
                "11-319", "11-321", "11-322", "11-324", "11-410", "11-415", 
                "11-420", "11-421", "11-422", "11-505", "11-510", "11-511", 
                "11-520", "11-522", "11-524", "11-605", "11-610", "11-611", 
                "11-620", "11-626", "11-710", "11-720", "11-722", "11-810", 
                "11-820", "11-821", "11-850", "11-851", "11-910", "11-920", 
                "12-010", "12-020", "12-105", "12-110", "12-111", "12-112", 
                "12-120", "12-210", "12-220", "12-222", "12-310", "12-320", 
                "12-410", "12-411", "12-412", "12-413", "12-414", "12-421", 
                "12-425", "12-426", "12-427", "12-438", "12-510", "12-520", 
                "12-610", "12-620", "12-670", "12-810", "12-811", "12-910", 
                "13-315", "13-316", "13-317", "13-320", "20-005", "20-006", 
                "20-008", "20-009", "20-019", "20-023", "20-028", "20-048", 
                "20-055", "20-056", "20-070", "20-071", "20-072", "20-086", 
                "20-088", "20-101", "20-108")), names = ""), class = "table")

table(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_a$estab_homo_rec), exclude=NULL)
# FALSE 
# 20957004 
structure(c(`FALSE` = 20957004L), dim = 1L, dimnames = structure(list(
  "FALSE"), names = ""), class = "table")

table(dtX2023_12_05_DatosEgresosHosp_encrip_a$`Código nuevo Establecimiento`, exclude=NULL)
# Var1     Freq
# 1  110110   194013
# 2  110120      834
# 3  110310   388511
# 4  110320   124310
# 5  110325     3237
# 6  110410   238888
# 7  110510   559438
# 8  110620    19534
# 9  110621    48662
# 10 110720   225061
# 11 111010   289897
# 12 111011    45871
# 13 111210   682962
# 14 111220   573410
# 15 111221   620132
# 16 111222    14754
# 17 111223   103289
# 18 111224   764587
# 19 111225    65813
# 20 111295   124616
# 21 111298     1179
# 22 111299       81
# 23 111310   362856
# 24 112010   428265
# 25 112105     1318
# 26 112220   137991
# 27 112222      754
# 28 112310   242146
# 29 112320    72200
# 30 112510   136659
# 31 112520      130
# 32 112610   173218
# 33 112810    82327
# 34 113315   139533
# 35 113316    32785
# 36 113317     2042
# 37 113320      514
# 38 120101        7
# 39 200128   524143
# 40 200138   736874
# 41 200308   260169
# 42 200482   338601
# 43   <NA> 12195393
structure(c(194013L, 834L, 388511L, 124310L, 3237L, 238888L, 
            559438L, 19534L, 48662L, 225061L, 289897L, 45871L, 682962L, 573410L, 
            620132L, 14754L, 103289L, 764587L, 65813L, 124616L, 1179L, 81L, 
            362856L, 428265L, 1318L, 137991L, 754L, 242146L, 72200L, 136659L, 
            130L, 173218L, 82327L, 139533L, 32785L, 2042L, 514L, 7L, 524143L, 
            736874L, 260169L, 338601L, 12195393L), dim = 43L, dimnames = structure(list(
              c("110110", "110120", "110310", "110320", "110325", "110410", 
                "110510", "110620", "110621", "110720", "111010", "111011", 
                "111210", "111220", "111221", "111222", "111223", "111224", 
                "111225", "111295", "111298", "111299", "111310", "112010", 
                "112105", "112220", "112222", "112310", "112320", "112510", 
                "112520", "112610", "112810", "113315", "113316", "113317", 
                "113320", "120101", "200128", "200138", "200308", "200482", 
                NA)), names = ""), class = "table")

data.frame(table(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_a$`Código nuevo Establecimiento`), 
                 exclude=NULL))
# Var1     Freq
# 1 FALSE  8761611
# 2  TRUE 12195393
structure(list(Var1 = structure(1:2, levels = c("FALSE", "TRUE"
), class = "factor"), Freq = c(8761611L, 12195393L)), class = "data.frame", row.names = c(NA, 
                                                                                          -2L))
data.frame(prop.table(table(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_a$`Código nuevo Establecimiento`), exclude=NULL)))
# Var1      Freq
# 1 FALSE 0.4180756
# 2  TRUE 0.5819244
structure(list(Var1 = structure(1:2, levels = c("FALSE", "TRUE"
), class = "factor"), Freq = c(0.418075551257231, 0.581924448742769
)), class = "data.frame", row.names = c(NA, -2L))

#estab_homo: debeiese ser de 6, no de 5 como aparece en esta base
#https://www.minsal.cl/wp-content/uploads/2019/12/Listado-establecimientos-DEIS.pdf


### 1b0. comparar ANID, unir ---------------------------------------------------

dtX2023_12_05_DatosEgresosHosp_encrip_a$diag2<-ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_a$diag2=="None","",dtX2023_12_05_DatosEgresosHosp_encrip_a$diag2)
dtX2023_12_05_DatosEgresosHosp_encrip_a$diag3<-ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_a$diag3=="None","",dtX2023_12_05_DatosEgresosHosp_encrip_a$diag3)
#podría buscar las fallas en la fecha concatenando estas
#estab_homo     #fecha_ingreso #fecha_egreso #sexo     #edad_anos                     #diag1         #diag2     #diag3                                 
dtX2023_11_07_DatosEgresosHosp_encrip <- tidyr::unite(dtX2023_11_07_DatosEgresosHosp_encrip,
                                               "concat",
                                               c("estab_homo", "fecha_ingreso", "fecha_egreso", "edad_anos", "diag1", "diag2", "diag3"),
                                               sep = "_",
                                               remove = FALSE)
dtX2023_12_05_DatosEgresosHosp_encrip_a <- transform(dtX2023_12_05_DatosEgresosHosp_encrip_a,
                                                   concat = paste(`Código nuevo Establecimiento`, fecha_ingreso_rec, fecha_egreso_rec, edad_anos,
                                                                  diag1, diag2, diag3, sep = "_"))

dtX2023_11_07_DatosEgresosHosp_encrip_2<-
dtX2023_11_07_DatosEgresosHosp_encrip %>% 
  dplyr::left_join(dtX2023_12_05_DatosEgresosHosp_encrip_a, by="concat")
# Warning message:
#   In dplyr::left_join(., dtX2023_12_05_DatosEgresosHosp_encrip_a,  :
#                         Detected an unexpected many-to-many relationship between `x` and `y`.
#                       ℹ Row 16296941 of `x` matches multiple rows in `y`.
#                       ℹ Row 1681828 of `y` matches multiple rows in `x`.
#                       ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.



# nrow(dtX2023_11_07_DatosEgresosHosp_encrip_2)
# [1] 29146192
write_parquet(dtX2023_11_07_DatosEgresosHosp_encrip_2, 
  "E:/Mi unidad/Alvacast/SISTRAT 2023/data/20231107_egres_hosp/2023-11-07  DatosEgresosHosp_encrip_2.gz.parquet", 
  compression = "gzip", compression_level = 5)

### 1b1. comparar ANID, edad ---------------------------------------------------

# nrow(dtX2023_11_07_DatosEgresosHosp_encrip)
# length(unique(dtX2023_11_07_DatosEgresosHosp_encrip$concat))

nrow(dtX2023_12_05_DatosEgresosHosp_encrip)
#20957004
length(unique(dtX2023_12_05_DatosEgresosHosp_encrip$concat))
#0

#son iguales? en términos de edad
# min(dtX2023_11_07_DatosEgresosHosp_encrip$edad, na.rm=T) #tiene perdidos
# max(dtX2023_11_07_DatosEgresosHosp_encrip$edad, na.rm=T)
#0-121
min(dtX2023_12_05_DatosEgresosHosp_encrip$edad)
max(dtX2023_12_05_DatosEgresosHosp_encrip$edad)
#0-121

# table(dtX2023_11_07_DatosEgresosHosp_encrip$tipo_edad)
#Años Días  Horas y Minutos Meses
# 1        2        3        4 
# 27599460   767134   688628    90934 

table(dtX2023_12_05_DatosEgresosHosp_encrip$tipo_edad)
# 1        2        3        4 
# 19925988   497042   443040    90934 
structure(c(`1` = 19925988L, `2` = 497042L, `3` = 443040L, `4` = 90934L
), dim = 4L, dimnames = structure(list(c("1", "2", "3", "4")), names = ""), class = "table")

### 1b2. comparar ANID, fechas ---------------------------------------------------

invisible("Válidas")

# table(is.na(dtX2023_11_07_DatosEgresosHosp_encrip$fecha_ingreso))
# FALSE 
# 29146157 
table(!is.na(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso))
# FALSE     TRUE 
# 75 20956929 
structure(c(`FALSE` = 75L, `TRUE` = 20956929L), dim = 2L, dimnames = structure(list(
  c("FALSE", "TRUE")), names = ""), class = "table")

table(!is.na(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec))
# FALSE     TRUE 
# 81 20956923 
structure(c(`FALSE` = 81L, `TRUE` = 20956923L), dim = 2L, dimnames = structure(list(
  c("FALSE", "TRUE")), names = ""), class = "table")

invisible("Distribución")

#función para resumir fechas
sum_dates <- function(x){
  
  cbind.data.frame(
min= as.Date(min(unclass(as.Date(x)), na.rm=T), origin = "1970-01-01"),
p001= as.Date(quantile(unclass(as.Date(x)), .001, na.rm=T), origin = "1970-01-01"),
p005= as.Date(quantile(unclass(as.Date(x)), .005, na.rm=T), origin = "1970-01-01"),
p025= as.Date(quantile(unclass(as.Date(x)), .025, na.rm=T), origin = "1970-01-01"),
p25= as.Date(quantile(unclass(as.Date(x)), .25, na.rm=T), origin = "1970-01-01"),
p50= as.Date(quantile(unclass(as.Date(x)), .5, na.rm=T), origin = "1970-01-01"),
p75= as.Date(quantile(unclass(as.Date(x)), .75, na.rm=T), origin = "1970-01-01"),
p975= as.Date(quantile(unclass(as.Date(x)), .975, na.rm=T), origin = "1970-01-01"),
p995= as.Date(quantile(unclass(as.Date(x)), .995, na.rm=T), origin = "1970-01-01"),
p999= as.Date(quantile(unclass(as.Date(x)), .999, na.rm=T), origin = "1970-01-01"),
max= as.Date(max(unclass(as.Date(x)), na.rm=T), origin = "1970-01-01")
  )
}

sum_dates(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec)
#           min       p001       p005       p025        p25        p50        p75       p975       p995       p999        max
# 0.1% 1969-04-07 2010-01-02 2010-01-19 2010-04-28 2013-03-04 2016-04-25 2019-06-23 2022-08-30 2022-12-03 2022-12-23 2068-12-31
structure(list(min = structure(-269, class = "Date"), p001 = structure(14611, class = "Date"), 
               p005 = structure(14628, class = "Date"), p025 = structure(14727, class = "Date"), 
               p25 = structure(15768, class = "Date"), p50 = structure(16916, class = "Date"), 
               p75 = structure(18070, class = "Date"), p975 = structure(19234, class = "Date"), 
               p995 = structure(19329, class = "Date"), p999 = structure(19349, class = "Date"), 
               max = structure(36159, class = "Date")), class = "data.frame", row.names = "0.1%")

sum_dates(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso_rec)
# min       p001       p005       p025        p25        p50        p75       p975       p995       p999        max
# 0.1% 2010-01-01 2010-01-07 2010-01-25 2010-05-04 2013-03-09 2016-04-29 2019-06-28 2022-09-05 2022-12-08 2022-12-27 2022-12-31
structure(list(min = structure(14610, class = "Date"), p001 = structure(14616, class = "Date"), 
               p005 = structure(14634, class = "Date"), p025 = structure(14733, class = "Date"), 
               p25 = structure(15773, class = "Date"), p50 = structure(16920, class = "Date"), 
               p75 = structure(18075, class = "Date"), p975 = structure(19240, class = "Date"), 
               p995 = structure(19334, class = "Date"), p999 = structure(19353, class = "Date"), 
               max = structure(19357, class = "Date")), class = "data.frame", row.names = "0.1%")

# sum_dates(dtX2023_11_07_DatosEgresosHosp_encrip$fecha_ingreso)
# min       p025        p25        p50        p75       p975        max
# 2.5% 1904-08-13 2005-11-02 2009-11-18 2014-02-22 2018-05-29 2022-07-21 2031-06-07
# sum_dates(dtX2023_11_07_DatosEgresosHosp_encrip$fecha_egreso)
# min       p025        p25        p50        p75       p975        max
# 2.5% 2005-01-01 2005-06-19 2009-06-21 2013-11-11 2018-04-12 2022-07-21 2022-12-31

sum_dates(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec)
#           min       p001       p005       p025        p25        p50        p75       p975       p995       p999        max
#0.1% 1969-09-30 2010-01-01 2010-02-02 2010-07-30 2014-08-27 2018-04-13 2020-12-14 2022-10-17 2022-12-12 2022-12-26 2063-09-23
structure(list(min = structure(14610, class = "Date"), p001 = structure(14619, class = "Date"), 
               p005 = structure(14652, class = "Date"), p025 = structure(14831, class = "Date"), 
               p25 = structure(16318, class = "Date"), p50 = structure(17645, class = "Date"), 
               p75 = structure(18619, class = "Date"), p975 = structure(19289, class = "Date"), 
               p995 = structure(19345, class = "Date"), p999 = structure(19355, class = "Date"), 
               max = structure(19357, class = "Date")), class = "data.frame", row.names = "0.1%")

sum_dates(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec)
#           min       p001       p005       p025        p25        p50        p75       p975       p995       p999        max
# 0.1% 2010-01-01 2010-01-10 2010-02-12 2010-08-10 2014-09-05 2018-04-24 2020-12-23 2022-10-24 2022-12-19 2022-12-29 2022-12-31
structure(list(min = structure(14610, class = "Date"), p001 = structure(14619, class = "Date"), 
               p005 = structure(14652, class = "Date"), p025 = structure(14831, class = "Date"), 
               p25 = structure(16318, class = "Date"), p50 = structure(17645, class = "Date"), 
               p75 = structure(18619, class = "Date"), p975 = structure(19289, class = "Date"), 
               p995 = structure(19345, class = "Date"), p999 = structure(19355, class = "Date"), 
               max = structure(19357, class = "Date")), class = "data.frame", row.names = "0.1%")

dtX2023_12_05_DatosEgresosHosp_encrip_filt %>%
  dplyr::select(fecha_ingreso_rec, fecha_egreso_rec) %>%
  dplyr::mutate(fecha_ingreso_rec = lubridate::as_date(fecha_ingreso_rec),
         fecha_egreso_rec = lubridate::as_date(fecha_egreso_rec)) -> vuelos

pdf("./_figs/fig_hist.pdf") #, res= 600 , width = 600, height = 800
#par(mfrow = c(2, 1), mar = c(4, 4, 2, 2), oma = c(2, 2, 2, 2)) # 
par(mfrow = c(2, 1))#, mar = c(4, 4, 1, 2) + 1)  # Reduced top margin

hist(vuelos$fecha_ingreso_rec, breaks = "years", freq = T, 
     col = "lightblue", border = "white", xlab = "Fecha de Ingreso", 
     main = NULL,ylab = "Frecuencias",
     xaxt = "n", # Disable default x-axis to customize
     las = 1, # Make axis labels horizontal
     cex.axis = 0.6, # Axis label size
     cex.main = 0.000001, # Main title size
     cex.lab = 1.1) # X and Y labels size
axis.Date(1, at = seq(min(vuelos$fecha_ingreso_rec, na.rm=T), max(vuelos$fecha_ingreso_rec, na.rm=T), by = "years"),
          format = "%Y-%m", cex.axis = 0.6, las = 2)

hist(vuelos$fecha_egreso_rec, breaks = "years", freq = T, 
     col = "lightblue", border = "white", xlab = "Fecha de Egreso", 
     main = NULL,ylab = "Frecuencias",
xaxt = "n", # Disable default x-axis to customize
las = 1, # Make axis labels horizontal
cex.axis = 0.6, # Axis label size
cex.main = 0.000001, # Main title size
cex.lab = 1.1) # X and Y labels size
axis.Date(1, at = seq(min(vuelos$fecha_egreso_rec, na.rm=T), max(vuelos$fecha_egreso_rec, na.rm=T), by = "years"),
          format = "%Y-%m", cex.axis = 0.6, las = 2)
dev.off()


#check
dtX2023_12_05_DatosEgresosHosp_encrip[is.na(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec), 
                                      c("row_num_index","run", "fecha_ingreso", "fecha_egreso")]
# row_num_index                                                              run fecha_ingreso fecha_egreso
# 472579         472579 401719b37024dbd037cb43e6eb826466f0224e25b5e04f0bf5e3385045c703b2          <NA>     06-01-13
# 538981         538981 726d6f2bd1f08ed279d68c88719a21a0e30aabdff1b9701abaed8248dc3bc7a9          <NA>     14-03-13
# 593378         593378 d26d58e9bcad08b0e6f8833ef4a66eb1d083fff3a9827bd4c5f0008dcb6052c6          <NA>     20-02-13
# 913981         913981 65a5390ed465fcd7db6dcfc5563666579f3f81530508a155f2a9a00e05488a0d          <NA>         <NA>
#   917427         917427 c317e16371efa2caeca2d6dc5334b24e0a85d25a17b1c5171c3bf1d4c9333b65          <NA>     03-03-13
# 920766         920766 40bf3deacbb81eb532f4b1cec9b0eb9bc6e392177b6637e65b3315ca13b79161          <NA>         <NA>
#   979671         979671 ccb3259ee509220fa7a0132043c6d0d5d92e12f4ee194a67a5bb2b8d225c34c9          <NA>         <NA>
#   1152448       1152448 5722cbb7ba4aa39fa6579b0e283860c56b32cfd8cc76b25c429b172a1c6502d2          <NA>         <NA>
#   1159615       1159615 0a79b7b7c22ebb603605e306c9b997905bce97245217c31945c1c133c4cf173e          <NA>     06-01-13
# 1659872       1659872 2fb4fb75efb403ee9145ec648678258d614c5012627430acc004658b9f11890f          <NA>     12-02-13
# 1722422       1722422 d2cf5f0ea2b6ad6ef86efc0e96c6e3365fac8f423c4c9e754e124db2dd030604          <NA>     04-03-13
# 1761476       1761476 549f53c07632ebd196dc385c4dd5bd0b94bd0b78d69740e92e5a9e7f8373ef5b          <NA>     25-02-13
# 1806382       1806382 66152d21fad871c32265ee49417d0d97b8685f043e243caadab8a0dcde50e2dc          <NA>     18-01-13
# 2276815       2276815 bb9e2bba99160364fb67675becbc2a9bd5047f396d4490c1640f5a49ee992030          <NA>     27-02-10
# 2313811       2313811 d6aed0a736769e4b0a3f77a3cfecd6f50afda7c8799c05e3537275c648f1b2d7          <NA>     13-10-10
# 2330320       2330320 d2e2734779e18e328eb28539864a7790a33a5910cd1decbcc2b1a0118f30c216          <NA>     15-01-10
# 2332113       2332113 ca2afb831679e0867b593c9edf6ce48f0b9c9c524d30e3d3047c6cee2b395381          <NA>         <NA>
#   2405654       2405654 03824e5ee0bbe092f66ccbb84f9936c2a15bc15829fae9ba95bccf85c1f26b14          <NA>     30-12-10
# 2456176       2456176 71f06388c43a8b532266b2c27174754b7d429dceb1158403ae7fbce768fd22e4          <NA>     01-12-10
# 2466233       2466233 07b51ea09e8e28e883f2eb2e5f35616bfe0b0f0df0acbdc80e16d4b2815df3c2          <NA>     20-05-10
# 2468066       2468066 ca348ff4784eaeeb4b710f6e608a10c8ae4a546799815e85a00ca4459c7ece36      00-09-10     16-09-10
# 2493052       2493052 e2509bcbed3add75ea3e461e06fca3d562e911d9e119ec82e1f1f6353a015c4c          <NA>     24-12-10
# 2510754       2510754 d8c93102ad69567d9f0c3856fff1f9fbc4a41576437b7b43b3e399ef9e7dae64          <NA>     22-12-10
# 2516500       2516500 d383770456cf20a92d2edbb36320e47d72cde620f024ed4aa055c1b3ea0d40b0          <NA>     29-12-10
# 2586500       2586500 c074bbcf863ae76771d559ce417bb0b66d63ce8cd377598489fcfe3d3cf64650          <NA>     30-12-10
# 2606479       2606479 fb69b16ad1b4c5d757d214622b988e799e60e1f8eb7d507a9568591aee8c39c1          <NA>     15-01-10
# 2695754       2695754 b42b7d0da10ea77b78796512862fc9c8108146ac7d20393ba30a5e0f36ec9015          <NA>     04-10-10
# 2697923       2697923 47dd42614d99d6d09024d241953861173e38136fc933544f3fd54d130453c1e0          <NA>     15-11-10
# 2788210       2788210 dcf4925ed0bebbb7fda6d4c39d079ae4641a21d7e89da2a1250275333c4a6f5d      31-06-10     01-06-10
# 2788291       2788291 226ae8cbea738b2c51e64b7d6fe6c1f9f7c9ab1a97a448ddb4c91d008a2afd98          <NA>     11-08-10
# 2846867       2846867 f526945033524f3f4ef0414244c5a41d49d6f3a27fe3b75175aa330009fe2e79          <NA>     05-05-10
# 2965182       2965182 e14a2ef6a0a8f44c439e7f5c75d682625a1603cd305b075ab62223a6cd17ebba          <NA>     16-04-10
# 3008264       3008264 0f043779b126240261b75d8f76ec06795f6b8680682cbc4f40acf5efad4f979c          <NA>     19-11-10
# 3014856       3014856 5f55229254fb9a8a5d81beab67784ab8399e472960b2523de6bd0dd8eecb25d2          <NA>     28-10-10
# 3028004       3028004 72ce25e1bf3eac89ae8753013660f53974e6ff4186e4974bc9e3cc525b6c640d          <NA>     12-09-10
# 3069419       3069419 1297c7316dec40c544cd397354c602bafb7f0449cdf2eb5a5c3de57245e09302          <NA>     05-06-10
# 3087626       3087626 14d2323102fa6e7b35f1457c36303cf9911fc6c90dfa6662e89a853ff9ba10c0          <NA>     23-02-10
# 3095316       3095316 980bbbdea3918373c41360d28a7b170b8600d143abac6697045a8d8637895db2          <NA>     23-05-10
# 3177153       3177153 a0c97dfe3c9f42942a1454ce01663fef8862b49256e19667f55461c75f2c889a          <NA>     27-09-10
# 3266580       3266580 1cb021b257b175dad7697dc6e63e6db1d315f1960c84750c1bfa7622913f3059          <NA>     03-12-10
# 3278899       3278899 a6409f378c80a57101a15555e77b01d611774617f2e5f4e361fda9a1a17faa08          <NA>     23-04-10
# 3306427       3306427 5e983542ab76dc6e8d56482c234876513b736430785621ffb41631d2a038b0ad          <NA>     24-08-10
# 3359166       3359166 d12959ae3e8d591e7d79604bfcbd2148eb4582151cf63ef8936d08d38d58dfcc          <NA>     01-12-10
# 3375104       3375104 79c07ee620ceb56ddb0257d270ae48fefe9c2d0cbf3b626640ca055cacf48e18      22-00-10     01-07-10
# 3404222       3404222 e505e7eec2a18f0d74d0fbeee3f7eb680905770e259cc284b2566425f9f434e7          <NA>     15-10-10
# 3434457       3434457 25881fdd09d87e4f1770e97bfd59fa4f6c6d41f0d2a6707858da4a5a837aa2cd          <NA>     29-08-10
# 3459855       3459855 c8ef36492493e91bde28b95131bcc53277f6c5fc6fe9f52cd6d270a828db0104          <NA>     29-09-10
# 3460193       3460193 e29284ae711bd75baad198501e3041940ef5a3cbd585e5bcfc7e30988c852a3f          <NA>     13-10-10
# 3477998       3477998 38774641797271ac6fdd6c784a1f7e31f4faf4c8bb1142ae50bcd40049736516          <NA>     20-12-10
# 3478584       3478584 a7faf4b9752bd1a8b42985ea3f2efa61b075aa6e59b41d23c7352765108f8575          <NA>     30-06-10
# 3547257       3547257 0da333fc458aa6e1a889dccb5f41c70c58c524a27622f04477ef82b9a18f35e7      00-04-10     07-04-10
# 3578137       3578137 bd6752d8e4e26d2afd2cbc434ab7e8762475eb4e422640e573923ab452e06b93          <NA>     15-01-10
# 3588323       3588323 788394dd5d6caab6e36a33cf0e1deeb6263c498de67093e59cdc8dea264ccc16          <NA>     21-12-10
# 3643394       3643394 4b7e4f3e0505f1847b322888359f145d8ac1336b1094f64a23078552bf490503      29-02-10     26-02-10
# 3785779       3785779 a3932113afd89a58ca80125e280cd0eed745f0cabba96e20737096979f222866          <NA>     29-12-10
# 3832042       3832042 11cde3f9c49fc1fc98711b51b07ac40ccd6fad4027cface21d9bd22197448788          <NA>     03-02-10
# 3884903       3884903 dba1a5d3e299d82bfb5f2fecf1e7375b110dc8297995ccd75e3bd8b986b13a35      24-00-10     06-09-10
# 3996555       3996555 a173bd733afff291a37cb9040cd20e39af3ea9559216a6c5d4ac2255fcde67ba          <NA>     26-07-10
# 4351249       4351249 460414a9a6890a3d0832e12db6962e3760584ec538702ae953ebe5a4e395be47          <NA>     29-11-10
# 4442311       4442311 82164410cf7217483d32be41ea74b9ae462214b6cec41e433d599fc78b5b2fa8          <NA>     18-07-10
# 4454912       4454912 10af42365e0d478d8999d91c0db9f1df7fe882a2997dfdec16995f57aeed9bfa          <NA>     05-11-10
# 4505448       4505448 875240e528f36267aff2223ff2fa2982bb52dbe413722d14cd0e9e4c751cbc05          <NA>     12-11-10
# 4565455       4565455 9a65a1dd293d2bf2ec088513530bc590374583a81b3d6d6438f7cdf72e0ba6e0          <NA>     10-04-10
# 4574370       4574370 cb472412033471365503616ac81d3722248353d600fd6de501f4388ba0147cd0          <NA>     06-10-10
# 4727914       4727914 eea730748dd1ed272b251bfcba296479fffce357c0cfe01c875d160958c8e63a          <NA>     17-04-10
# 4811819       4811819 59862b49b955c9fc29c2976fbedf39521a152c04b2fbe7c3dccecf337c9c3c69          <NA>     15-10-10
# 4857851       4857851 c3f1e6241fc200607d7d367a4a19312064575aad7c8e5ea46a89de809efe9f89          <NA>     12-10-10
# 4879173       4879173 49045f922bf7624facc4df91739a63e71437a1c5a32a3f092eabdfaef8d13ad1          <NA>     31-12-10
# 4896267       4896267 4866ccde0d71128362b314d710af6aa9c12d77f28549b69f9a2044fd1963fe4b          <NA>     02-06-10
# 4913518       4913518 900c0d93d7294c1d094546ed400a9cf171d4d0a6904c9d787687503ab48ef73e          <NA>     19-12-10
# 4944710       4944710 1339719d62eb1e573fdae7ebd662dcbfd6cd40f323810bc991f1974329c819c9          <NA>     01-07-10
# 4950696       4950696 3c2cb2dceb02281595c16d63be2d7d67c11444111c9fe11e9f10a42143e33a45          <NA>     17-02-10
# 4990257       4990257 6863d0337c3868724d1a563f4d379f83353d958db1aad4dfcda724c878a09a46          <NA>     24-11-10
# 5047110       5047110 3e9c5b8dd6d4ef6ee29abd9cd88084e1032c44d889f4b826e215064e96b40b1c          <NA>     02-02-10
# 5060627       5060627 06fd01fe178a05602b145f9821582145b41ad83420b5ed9358afb4bfec4f1a61          <NA>     27-12-10
# 5145615       5145615 49f504aae1c67299f61d90cbcd24723abc14e94c9608f19b548b86e25036383c          <NA>     03-12-10
# 5180745       5180745 53a4394163db11c196866d5e4990bad9b99ff7b0850ad5e66070bf735cc6fd61          <NA>     13-03-10
# 5205951       5205951 d16a734cb4e6f18c67253e2e870c31cb37aade4f1dfb73112b939915a2d22407          <NA>     16-11-10
# 5286303       5286303 14d7a0bbefa56ba90badf99b6002ff49cd8b7df75e73726c62f9ab020bcd0474          <NA>     11-07-10
# 5317734       5317734 7bc74e26c2f22902eaed0c64f7c5d0e1750ff96c718493c4d39f2c97b3f7d387          <NA>     29-11-10
# 5319252       5319252 e2fb5c08dfaf9ddeb51ef7986745b30b7846968e98496c8a7b292d8a7da2b02f          <NA>     29-10-10

dtX2023_12_05_DatosEgresosHosp_encrip[is.na(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso_rec), 
                                      c("row_num_index","run", "fecha_ingreso", "fecha_egreso")]
# row_num_index                                                              run fecha_ingreso fecha_egreso
# 913981         913981 65a5390ed465fcd7db6dcfc5563666579f3f81530508a155f2a9a00e05488a0d          <NA>         <NA>
#   920766         920766 40bf3deacbb81eb532f4b1cec9b0eb9bc6e392177b6637e65b3315ca13b79161          <NA>         <NA>
#   979671         979671 ccb3259ee509220fa7a0132043c6d0d5d92e12f4ee194a67a5bb2b8d225c34c9          <NA>         <NA>
#   1152448       1152448 5722cbb7ba4aa39fa6579b0e283860c56b32cfd8cc76b25c429b172a1c6502d2          <NA>         <NA>
#   1326169       1326169 49d1d1cda3f16d9458a91b6172048474265649d6abf4752ed1430aee6161e576      18-12-13         <NA>
#   2332113       2332113 ca2afb831679e0867b593c9edf6ce48f0b9c9c524d30e3d3047c6cee2b395381          <NA>         <NA>
#   2432103       2432103 62d5e0cbd43d4be20044f089f30370ff07b1aa9adc8c1a206786b55ddaeeccf4      19-02-11         <NA>
#   2509847       2509847 edad877f832f08a9e657ace0637996db99d6705ab80e455a33949b9652f00aa9      03-10-11         <NA>
#   3889692       3889692 4b4dd3f823198ffee81e60fde28dd7255b6184a0aeecde2c9264db1d55eced0e      14-05-11         <NA>
#   4082353       4082353 51b7e70e969942d3a26c96a68d920d38777bc09e5a4c1ca8053adda17e51891c      21-02-11         <NA>
#   4106791       4106791 8481f64a3f070a53eace1cdf832614f14c9eb26b8df03630efdebd4fea7659cd      06-10-11         <NA>
#   4831300       4831300 fad37b00c20b8cc19ed49a4518b4bb8611c74f642386fdecb2476dcf01d28a82      17-02-11         <NA>
#   5071391       5071391 889ff1689f7efd70d902d2f2a8bb3d11d03465134380d0fec8f3851fcd0bc934      11-10-11         <NA>
#   5258835       5258835 8ea720fc8e52fcbd4a2be36be7c2da7f9af3c99fb5c5d573a9d1e9af1a785eed      21-02-11         <NA>
#   5282484       5282484 f624296d6516186b46b42df847089ff84a657c5d172e7aafbc63ac13550761ab      11-10-11         <NA>


dtX2023_12_05_DatosEgresosHosp_encrip %>%
  filter(as.Date(fecha_egreso_rec) <= 
           as.Date(quantile(unclass(as.Date(fecha_egreso_rec)), .000005, na.rm=T), origin = "1970-01-01")) %>%
  select(row_num_index, run, fecha_ingreso, fecha_ingreso_rec, fecha_egreso_rec, fecha_egreso) %>%
  arrange(fecha_egreso_rec)
# row_num_index                                                              run fecha_ingreso fecha_ingreso_rec fecha_egreso_rec fecha_egreso
# 1         2191534 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464      27-12-09        2009-12-27       2010-01-01     01-01-10
# 2         2191535 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464      24-12-09        2009-12-24       2010-01-01     01-01-10

dtX2023_12_05_DatosEgresosHosp_encrip %>%
  filter(as.Date(fecha_egreso_rec) >= 
           as.Date(quantile(unclass(as.Date(fecha_egreso_rec)), .999995, na.rm=T), origin = "1970-01-01")) %>%
  select(row_num_index, run, fecha_ingreso, fecha_ingreso_rec, fecha_egreso_rec, fecha_egreso) %>%
  arrange(desc(fecha_egreso_rec))
# row_num_index                                                              run fecha_ingreso fecha_ingreso_rec fecha_egreso_rec fecha_egreso
# 1        17951425 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464      28-12-22        2022-12-28       2022-12-31     31-12-22
# 2        17952223 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464      28-12-22        2022-12-28       2022-12-31     31-12-22
# 3        17953416 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464      27-12-22        2022-12-27       2022-12-31     31-12-22
# 4        17954000 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464      26-12-22        2022-12-26       2022-12-31     31-12-22

sum_dates(dtX2023_12_05_DatosEgresosHosp_encrip_filt[dtX2023_12_05_DatosEgresosHosp_encrip_filt$run == "1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464", "fecha_egreso_rec"])
# min       p001       p005       p025        p25        p50        p75       p975       p995       p999        max
# 0.1% 2010-01-03 2010-01-05 2010-01-11 2010-02-10 2014-01-29 2017-01-12 2019-10-07 2022-10-19 2022-12-12 2022-12-27 2022-12-30

# dtX2023_11_07_DatosEgresosHosp_encrip %>%
#   filter(as.Date(fecha_egreso) <= 
#            as.Date(quantile(unclass(as.Date(fecha_egreso)), .000005, na.rm=T), origin = "1970-01-01")) %>%
#   select(run, fecha_ingreso, fecha_egreso) %>%
#   arrange(fecha_egreso)
# run fecha_ingreso fecha_egreso
# 1   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464                 2005-01-01
# 2   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464    2004-12-30   2005-01-01
# 3   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464    2004-12-28   2005-01-01
# 4   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464                 2005-01-01
# 5   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464                 2005-01-01
# 6   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464                 2005-01-01

# dtX2023_11_07_DatosEgresosHosp_encrip %>%
#   filter(as.Date(fecha_egreso) >= 
#            as.Date(quantile(unclass(as.Date(fecha_egreso)), .999995, na.rm=T), origin = "1970-01-01")) %>%
#   select(run, fecha_ingreso, fecha_egreso) %>%
#   arrange(desc(fecha_egreso))
# run fecha_ingreso fecha_egreso
# 1   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464    2022-12-28   2022-12-31
# 2   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464    2022-12-28   2022-12-31
# 3   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464    2022-12-27   2022-12-31
# 4   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464    2022-12-26   2022-12-31
# 5   1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464    2022-12-27   2022-12-31


invisible("Check the amount of 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464")
dtX2023_12_05_DatosEgresosHosp_encrip %>%
  dplyr::filter(run== "1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% nrow()
# [1] 168240

invisible("Fechas perdidas en ingreso o egreso")
dtX2023_12_05_DatosEgresosHosp_encrip_comp<- na.omit(dtX2023_12_05_DatosEgresosHosp_encrip[,c("run","fecha_ingreso_rec", "fecha_egreso_rec")])
#20956913 de 20957004 =0.9999957

invisible("Recodificar fechas a numérico")
dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_ingreso_rec_num<-
  as.numeric(unclass(as.Date(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_ingreso_rec)))
dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_egreso_rec_num<-
  as.numeric(unclass(as.Date(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_egreso_rec)))

dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec_num<-
  as.numeric(unclass(as.Date(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec)))
dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec_num<-
  as.numeric(unclass(as.Date(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec)))

invisible("Dividir en secciones")
dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_ingreso_rec_cut<-
  cut(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_ingreso_rec_num, 
             breaks= 30, dig.lab = 4, ordered_result= T)
dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_egreso_rec_cut<-
  cut(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_egreso_rec_num, 
      breaks= 30, dig.lab = 4, ordered_result= T)

labs <- levels(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_ingreso_rec_cut)
labs_date<- paste0(as.Date(as.numeric( sub("\\((.+),.*", "\\1", labs)), origin = "1970-01-01"),";\n",
       as.Date(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs)), origin = "1970-01-01"))
labs2 <- levels(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_egreso_rec_cut)
labs2_date<- paste0(as.Date(as.numeric( sub("\\((.+),.*", "\\1", labs2)), origin = "1970-01-01"),";\n",
          as.Date(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs2)), origin = "1970-01-01"))


dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec_cut<-
  cut(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec_num, 
      breaks= 30, dig.lab = 4, ordered_result= T)
dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec_cut<-
  cut(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec_num, 
      breaks= 30, dig.lab = 4, ordered_result= T)

labs3 <- levels(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec_cut)
labs3_date<- paste0(as.Date(as.numeric( sub("\\((.+),.*", "\\1", labs3)), origin = "1970-01-01"),";\n",
                   as.Date(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs3)), origin = "1970-01-01"))
labs4 <- levels(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec_cut)
labs4_date<- paste0(as.Date(as.numeric( sub("\\((.+),.*", "\\1", labs4)), origin = "1970-01-01"),";\n",
                    as.Date(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs4)), origin = "1970-01-01"))


dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_ingreso_rec_cut<-
  factor(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_ingreso_rec_cut,levels= labs, labels= labs_date)

dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_egreso_rec_cut<-
factor(dtX2023_12_05_DatosEgresosHosp_encrip_comp$fecha_egreso_rec_cut,levels= labs2, labels= labs2_date)

dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec_cut<-
  factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec_cut,levels= labs3, labels= labs3_date)

dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec_cut<-
  factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_egreso_rec_cut,levels= labs4, labels= labs4_date)


invisible("Ver distribución de pueblo de origen, datos filtrados")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt$glosa_pueblo_originario, 
      dtX2023_12_05_DatosEgresosHosp_encrip_filt$pueblo_originario)
#                              1      2      3      4      5      6      7      8      9     10     96
# AYMARA                       0    705      0      0      0      0      0      0      0      0      0
# COLLA                        0      0      0      0      0    122      0      0      0      0      0
# DIAGUITA                     0      0      0      0      0      0    623      0      0      0      0
# KAWÉSQAR                     0      0      0      0      0      0      0    608      0      0      0
# LICAN ANTAI (ATACAMEÑO)      0      0      0    126      0      0      0      0      0      0      0
# MAPUCHE                  17368      0      0      0      0      0      0      0      0      0      0
# NINGUNO                      0      0      0      0      0      0      0      0      0      0 965690
# OTRO (ESPECIFICAR)           0      0      0      0      0      0      0      0      0  39839      0
# QUECHUA                      0      0      0      0     64      0      0      0      0      0      0
# RAPA NUI (PASCUENSE)         0      0    213      0      0      0      0      0      0      0      0
# YAGÁN (YÁMANA)               0      0      0      0      0      0      0      0    142      0      0

structure(c(0L, 705L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
    0L, 0L, 0L, 0L, 122L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
    0L, 623L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 608L, 0L, 
    0L, 0L, 0L, 0L, 0L, 126L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 17368L, 
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
    0L, 0L, 0L, 0L, 965690L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
    39839L, 0L, 0L, 0L, 0L, 0L, 64L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
    0L, 213L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
    0L, 0L, 0L, 142L, 0L, 0L), dim = c(11L, 11L), dimnames = structure(list(
      c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "96"
      ), c("AYMARA", "COLLA", "DIAGUITA", "KAWÉSQAR", "LICAN ANTAI (ATACAMEÑO)", 
           "MAPUCHE", "NINGUNO", "OTRO (ESPECIFICAR)", "QUECHUA", "RAPA NUI (PASCUENSE)", 
           "YAGÁN (YÁMANA)")), names = c("", "")), class = "table")

invisible("Hay clasificación única entre la glosa y el código")

# 2. otros filtros y transformaciones ------------------------------------------
invisible("Borramos las bases que nos quitan memoria RAM")
rm("dtX2023_11_07_DatosEgresosHosp_encrip")
rm("dtX2023_11_07_DatosEgresosHosp_encrip_2")
rm("dtX2023_12_05_DatosEgresosHosp_encrip_comp")
#rm("dtX2023_12_05_DatosEgresosHosp_encrip") #no puedo todavía
rm("dtX2023_12_05_DatosEgresosHosp_encrip_a")

dplyr::glimpse(dtX2023_12_05_DatosEgresosHosp_encrip_filt)


invisible("Chilenos")
#chilenos
dtX2023_12_05_DatosEgresosHosp_encrip_filt2<-
dtX2023_12_05_DatosEgresosHosp_encrip_filt %>% 
  dplyr::filter(pais_origen==152) %>% #531,638
  dplyr::select(!ends_with("_rec2")) %>% 
  dplyr::select(!ends_with("_rec")) 

invisible("Gente con previsión (se excluye a Ninguna=96 y Desconocido=99")
dtX2023_12_05_DatosEgresosHosp_encrip_filt3<-
  dtX2023_12_05_DatosEgresosHosp_encrip_filt2 %>% 
  dplyr::filter(!previ %in% c(96, 99)) #522,267


invisible("Se eliminan registros con RUNs inválidos")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt3$run) %>% 
  data.frame() %>% arrange(desc(Freq)) %>% head(n=10)
#     Var1 Freq
# 1  1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464  391
# 2  fed44ca61608101c1186b3833c8499452cb374291aa60160d6ff920f7867184b   67
# 3  1add369b84c2f3b1d69ffcb42560c2e4f1b2a6fe1a18aff6444e36e190fa869f   66
# 4  700a4e8a6107b26d9ce0813542f99224145770330990d36c481d2dd7829a0eed   60
# 5  08cfc5b6d5b815663f6d0c0b63aac46b0c6b1486e2f06e3428b60ee9abafd506   57
# 6  71980914b95004cc06985d370e95b889686907124d5400e17b6fe4b243a59b9d   52
# 7  2977ef0baece43e5a80b85db1c05167ce54925e5adf9515ad8874fe8cbf71b6e   51
# 8  f089c771c7af75a519264b2b7749767557a9b7b527e1014cbe8a7a2ae1c79a8c   51
# 9  074f6f9d4d9d7cf1953415ca411da1a4e07923a57edc653c9050ba26a180cde7   46
# 10 981656842fe3a920cae6dee235d48b7b6901ed467c93b85767db6f47fd4534fa   46

invisible("Se eliminan RUNs erróneos: 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464")

dtX2023_12_05_DatosEgresosHosp_encrip_filt4<-
  dtX2023_12_05_DatosEgresosHosp_encrip_filt3 %>% 
  dplyr::filter(!run =="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") #521,996

invisible("Se eliminan edades que no van entre los 15 y los 64 años")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5<-
  dtX2023_12_05_DatosEgresosHosp_encrip_filt4 %>% 
  dplyr::filter(
    dplyr::case_when(grepl("Años",glosa_tipo_edad) & edad_anos>14 & edad_anos<64~TRUE,TRUE~FALSE)
  ) %>% 
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad_anos>=15 & edad_anos<=29~1,
                                                edad_anos>=30 & edad_anos<=44~2,
                                                edad_anos>=45 & edad_anos<=59~3,
                                                edad_anos>=60 & edad_anos<=65~4,T~NA_real_)) #236,922

invisible("Visualizar")
p1_fechas_ingr<-
  ggplot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, aes(x = fecha_ingreso_rec_cut)) +
  geom_bar(fill = "steelblue", color = "white") +
  xlab("Categoría de fechas de ingreso") +
  ylab("Frecuencia") +
  theme_minimal()
p1_fechas_egr<-
  ggplot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, aes(x = fecha_egreso_rec_cut)) +
  geom_bar(fill = "steelblue", color = "white") +
  xlab("Categoría de fechas de egreso") +
  ylab("Frecuencia") +
  theme_minimal()
# 
# ggplot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, aes(x = fecha_ingreso_rec_num)) +
#   geom_histogram(binwidth = 1, fill = "blue", color = "black") +
#   xlab("Year") +
#   ylab("Frequency") +
#   ggtitle("Histogram of Dates by Year")

invisible("Demasiado lento")
# ggsave(p1_fechas_ingr,"fig1_ingr_dates",width = 10, height=5, dpi=600)
# 
# ggsave(p1_fechas_egr,"fig1_egr_dates",width = 10, height=5, dpi=600)
invisible("La base de ANID tiene más casos porque cubre casos del 2005 inclusive")


invisible("cambiar valores perdidos")
for(i in 1:11){
dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[paste0("diag",i)]]<-
ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[paste0("diag",i)]]=="None",
       NA,
       dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[paste0("diag",i)]])
}


# 3. análisis  ---------------------------------------------------

## 3.a. general  ---------------------------------------------------

invisible("CI vs. CYA CONADI")
table(dtX2023_12_05_DatosEgresosHosp_encrip$cya_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip$ci_conadi, exclude=NULL)
#           0        1     <NA>
# 0      697757   759003        0
# 1      150990   127614        0
# <NA>        0        0 19221640
matrix(c(697757L, 150990L, 0L, 759003L, 127614L, 0L, 0L, 0L, 
         19221640L), 3,3)

invisible("CI vs. CYA CONADI, datos filtrados")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt$cya_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt$ci_conadi, exclude=NULL)
#         0      1   <NA>
# 0     23434  23656      0
# 1     11331   5751      0
# <NA>      0      0 965271
structure(c(23434L, 11331L, 0L, 23656L, 5751L, 0L, 0L, 0L, 965271L
), dim = c(3L, 3L), dimnames = structure(list(c("0", "1", NA), 
                                              c("0", "1", NA)), names = c("", "")), class = "table")

invisible("CI vs. CYA CONADI, datos filtrados 3")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, exclude=NULL)
#             0      1   <NA>
# 0      8555   9293      0
# 1      2518   1707      0
# <NA>      0      0 218019
structure(c(8451L, 2472L, 0L, 9181L, 1683L, 0L, 0L, 0L, 215135L
), dim = c(3L, 3L), dimnames = structure(list(c("0", "1", NA), 
                                              c("0", "1", NA)), names = c("", "")), class = "table")

invisible("CYA CONADI vs. mortalidad intrahospitalaria")
table(dtX2023_12_05_DatosEgresosHosp_encrip$cya_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip$cond_egr, exclude=NULL)
#         1        2
# 0     1451323     5437
# 1      272701     5903
# <NA> 18751721   469919
structure(c(1451323L, 272701L, 18751721L, 5437L, 5903L, 469919L
), dim = 3:2, dimnames = structure(list(c("0", "1", NA), c("1", 
                                                           "2")), names = c("", "")), class = "table")

invisible("CYA CONADI vs. mortalidad intrahospitalaria")
round(prop.table(
  table(dtX2023_12_05_DatosEgresosHosp_encrip$cya_conadi,
        dtX2023_12_05_DatosEgresosHosp_encrip$cond_egr),2),3)
#     1     2
# 0 0.842 0.479
# 1 0.158 0.521
structure(c(0.842, 0.158, 0.479, 0.521), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("1", "2")), names = c("", 
"")))

invisible("CYA CONADI vs. mortalidad intrahospitalaria, datos filtrados")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt$cya_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt$cond_egr, exclude=NULL)
#         1      2
# 0     46357    733
# 1     16258    824
# <NA> 915592  49679
matrix(c(46357L, 16258L, 915592L, 733L, 824L, 49679L),3,2)

invisible("CYA CONADI vs. mortalidad intrahospitalaria, datos filtrados 3")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr, exclude=NULL)
#         1      2
# 0     17592    256
# 1      4035    190
# <NA> 211179   6840
structure(c(17386L, 3977L, 208481L, 246L, 178L, 6654L), dim = 3:2, dimnames = structure(list(
  c("0", "1", NA), c("1", "2")), names = c("", "")), class = "table")

invisible("Porc: CYA CONADI vs. mortalidad intrahospitalaria, datos filtrados 3")
round(prop.table(
  table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi,
        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr),2),3)
#     1     2
# 0 0.814 0.580
# 1 0.186 0.420
structure(c(0.814, 0.186, 0.58, 0.42), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("1", "2")), names = c("", "")))

invisible("CI CONADI vs. mortalidad intrahospitalaria")
table(dtX2023_12_05_DatosEgresosHosp_encrip$ci_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip$cond_egr, exclude=NULL)
#           1        2
# 0      844445     4302
# 1      879579     7038
# <NA> 18751721   469919
structure(c(844445L, 879579L, 18751721L, 4302L, 7038L, 469919L
), dim = 3:2, dimnames = structure(list(c("0", "1", NA), c("1", 
      "2")), names = c("", "")), class = "table")

invisible("CI CONADI vs. mortalidad intrahospitalaria, datos filtrados")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt$ci_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt$cond_egr, exclude=NULL)
#         1      2
# 0     34126    639
# 1     28489    918
# <NA> 915592  49679
matrix(c(46357L, 16258L, 915592L, 733L, 824L, 49679L),3,2)

invisible("CI CONADI vs. mortalidad intrahospitalaria, datos filtrados 3")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr, exclude=NULL)
#         1      2
# 0     10800    123
# 1     10563    301
# <NA> 208481   6654
structure(c(10800L, 10563L, 208481L, 123L, 301L, 6654L), dim = 3:2, dimnames = structure(list(
  c("0", "1", NA), c("1", "2")), names = c("", "")), class = "table")

invisible("Perc: CI CONADI vs. mortalidad intrahospitalaria, datos filtrados 3")
round(prop.table(
  table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi,
        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr),2),3)
#     1     2
# 0 0.506 0.290
# 1 0.494 0.710
structure(c(0.506, 0.494, 0.29, 0.71), class = "table", dim = c(2L, 
   2L), dimnames = structure(list(c("0", "1"), c("1", "2")), names = c("", "")))

invisible("RSH vs. mortalidad intrahospitalaria, datos filtrados")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr, exclude=NULL)
#         1      2
# 0      5267    422
# 1     16096      2
# <NA> 208481   6654
matrix(c(5267L, 16096L, 208481L, 422L, 2L, 6654L),3,2)

invisible("RSH vs. mortalidad intrahospitalaria")
table(dtX2023_12_05_DatosEgresosHosp_encrip$rsh,
      dtX2023_12_05_DatosEgresosHosp_encrip$cond_egr, exclude=NULL)
# 1        2
# 0      352787    11207
# 1     1371237      133
# <NA> 18751721   469919
matrix(c(352787L, 1371237L, 18751721L, 11207L, 133L, 469919L),3,2)

invisible("RSH vs. mortalidad intrahospitalaria, filtrados 3")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr, exclude=NULL)
#         1        2
# 0      5267    422
# 1     16096      2
# <NA> 208481   6654
structure(c(5267L, 16096L, 208481L, 422L, 2L, 6654L), dim = 3:2, dimnames = structure(list(
  c("0", "1", NA), c("1", "2")), names = c("", "")), class = "table")

invisible("Porc: RSH vs. mortalidad intrahospitalaria, filtrados 3")
round(prop.table(
  table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh,
        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr),2),3)
#     1     2
# 0 0.247 0.995
# 1 0.753 0.005
structure(c(0.247, 0.753, 0.995, 0.005), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("1", "2")), names = c("", 
    "")))

## 3.b. CONADI  ---------------------------------------------------

invisible("Preguntar por qué tiene tanto perdido y si hay alguna explicación por implementación")

paste0(" CONADI CYA Sí (de válidos): ",
       scales::percent(prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi))[2])
)
#[1] " CONADI CYA Sí (de válidos): 19%"

paste0("Perdidos del total muestra: ",
       scales::percent(table(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi))[2]/
                         length(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi))
       )
#[1] "Perdidos del total muestra: 91%"

dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi_rec<-ifelse(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi),0,
                                                                   dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi)

paste0(" CONADI CYA Sí (de total de la muestra): ",
scales::percent(as.numeric(prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi_rec)))[2])
)
# [1] " CONADI CYA Sí (de total de la muestra): 2%"

invisible("En otras causas, hay menos hospitalizados x0.73")
round(prop.table(
  table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh,
        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_notrel_dg),2),3)
#     0     1
# 0 0.237 0.443
# 1 0.763 0.557
structure(c(0.237, 0.763, 0.443, 0.557), class = "table", dim = c(2L, 
   2L), dimnames = structure(list(c("0", "1"), c("0", "1")), names = c("", "")))

invisible("En rel. con drogas, hay más hospitalizados x1.29")
round(prop.table(
  table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi_rec,
        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_rel_dg),2),3)
#     0      1
# 0 0.986 0.982
# 1 0.014 0.018
structure(c(0.986, 0.014, 0.982, 0.018), class = "table", dim = c(2L, 
        2L), dimnames = structure(list(c("0", "1"), c("0", "1")), names = c("","")))
invisible("En otras causas, hay una cantidad similar de muertes")
table(
subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cya_conadi_rec)
#           0     1
# FALSE 38740   533
# TRUE   1545    25
structure(c(38740L, 1545L, 533L, 25L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En rel. con drogas, en términos absolutos hay más mortalidades")
invisible("En rel. con drogas, CONADI (1) registra más mortalidades")
table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cya_conadi_rec)
#             0     1
# FALSE 187127   3444
# TRUE    5355    153
structure(c(187127L, 5355L, 3444L, 153L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En otras causas, CONADI (1) hay 1.21 más prob muerte")
round(prop.table(
  table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cya_conadi_rec),2),3)
#         0     1
# FALSE 0.962 0.955
# TRUE  0.038 0.045
structure(c(0.962, 0.038, 0.955, 0.045), class = "table", dim = c(2L, 
   2L), dimnames = structure(list(c("FALSE", "TRUE"), c("0", "1"
   )), names = c("", "")))
invisible("En rel. con drogas, en términos relativos no hay más mortalidades; ")
invisible("CONADI (1) hay 1.61 más prob prob muerte")
round(prop.table(
  table(
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cya_conadi_rec),2
  ),3)
#          0     1
# FALSE 0.972 0.957
# TRUE  0.028 0.043
structure(c(0.972, 0.028, 0.957, 0.043), class = "table", dim = c(2L, 
    2L), dimnames = structure(list(c("FALSE", "TRUE"), c("0", "1"
  )), names = c("", "")))
invisible("Hacer un mantel haenzel")
require(epiR)

matrix.array <- array(c(
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cya_conadi_rec,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr!=2), 
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cya_conadi_rec,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr!=2)
  ), dim = c(2, 2, 2))

epi.2by2(matrix.array)
#              Outcome +    Outcome -      Total                 Inc risk *
# Exposed +          178         3977       4155        4.28 (3.69 to 4.94)
# Exposed -         6900       225867     232767        2.96 (2.90 to 3.03)
# Total             7078       229844     236922        2.99 (2.92 to 3.06)
# 
# 
# Point estimates and 95% CIs:
# Inc risk ratio (crude)                         1.45 (1.25, 1.67)
# Inc risk ratio (M-H)                           1.47 (1.27, 1.69)
# Inc risk ratio (crude:M-H)                     0.99
# Inc odds ratio (crude)                         1.47 (1.26, 1.71)
# Inc odds ratio (M-H)                           1.49 (1.28, 1.73)
# Inc odds ratio (crude:M-H)                     0.99
# Attrib risk in the exposed (crude) *           1.32 (0.70, 1.94)
# Attrib risk in the exposed (M-H) *             1.36 (-3.29, 6.01)
# Attrib risk (crude:M-H)                        0.97
#  M-H test of homogeneity of IRRs: chi2(1) = 1.599 Pr>chi2 = 0.206
#  M-H test of homogeneity of ORs: chi2(1) = 1.409 Pr>chi2 = 0.235
#  Test that M-H adjusted OR = 1:  chi2(1) = 26.392 Pr>chi2 = <0.001
#  Wald confidence limits
#  M-H: Mantel-Haenszel; CI: confidence interval
#  * Outcomes per 100 population units 

#El OR ajustado por M-H fue de 1,54 (IC del 95%: 1,33 a 1,78), 
#lo que indica un aumento del 54% en las probabilidades de resultado en el grupo expuesto.
#La prueba M-H de homogeneidad de los OR no fue estadísticamente significativa (chi2(1) = 1.407, 
#Pr>chi2 = 0.236), lo que sugiere que no hubo una diferencia significativa en el OR entre los estudios incluidos

## 3.c. análisis RSH ---------------------------------------------------

paste0(" RSH Sí (de válidos): ",
scales::percent(as.numeric(prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh)))[2])
)
#[1] " RSH Sí (de válidos): 74%"

invisible("Muy importante cantidad de perdidos, se replica con el anterior")
paste0("Perdidos del total muestra: ",
       scales::percent(table(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh))[2]/
                         length(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh))
)
#[1] "Perdidos del total muestra: 91%"

invisible("Recodifico RSH para descartar perdidos y dejarlos como 0")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec<-ifelse(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh),0,
                                                            dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh)

paste0("RSH Sí (de total): ",
       scales::percent(prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec))[2])
       )
#[1] "RSH Sí (de total): 7%"

invisible("En otras causas, hay menos hospitalizados x0.47")
round(prop.table(
  table(
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec,
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_notrel_dg
    ),2),3)
# 0          1
# 0 0.925 0.965
# 1 0.075 0.035
structure(c(0.925, 0.075, 0.965, 0.035), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("0", "1")), names = c("", "")))

invisible("En otras causas, no tengo muertes en RSH Sí")
table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$rsh_rec)
#         0     1
# FALSE 37836  1437
# TRUE   1570     0
structure(c(37836L, 1570L, 1437L, 0L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En rel. con drogas, en términos absolutos hay más mortalidades")
invisible("En rel. con drogas, RSH Sí registra menos mortalidades (o sea, al menos registra)")
table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$rsh_rec)
#           0      1
# FALSE 175912  14659
# TRUE    5506      2
structure(c(175912L, 5506L, 14659L, 2L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En otras causas, no hay en RSH Sí")
round(prop.table(
  table(
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$rsh_rec
  ),2),3)
#         0     1
# FALSE 0.96 1.00
# TRUE  0.04 0.00
structure(c(0.96, 0.04, 1, 0), class = "table", dim = c(2L, 2L
), dimnames = structure(list(c("FALSE", "TRUE"), c("0", "1")), names = c("", 
                                                                         "")))

invisible("En rel. con drogas, en términos relativos no hay más mortalidades; ")
invisible("RSH sí tiene mucho menos prob prob muerte")
round(prop.table(
  table(
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$rsh_rec
  ),2),3)
#         0     1
# FALSE 0.97 1.00
# TRUE  0.03 0.00
structure(c(0.97, 0.03, 1, 0), class = "table", dim = c(2L, 2L
), dimnames = structure(list(c("FALSE", "TRUE"), c("0", "1")), names = c("", 
                                                                         "")))
invisible("no vale la pena hacer matrix array porque está desbalanceado")

## 3.d. análisis PPOO ---------------------------------------------------

paste0(" PPOO Distinto a ninguno (de válidos [todos son válidos]): ",
       scales::percent(
         prop.table(
           table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario!=96))[2]
         )
)
#[1] " PPOO Distinto a ninguno (de válidos [todos son válidos]): 2%"

invisible("Recodificar la variable para tener un marcador binario")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec<- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario!=96,1,0)

scales::percent(as.numeric(prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario)))[2])
#[1] "0%"

invisible("En otras causas, hay menos hospitalizados x0.67")
round(prop.table(
  table(
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec,
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_notrel_dg
  ),2),3)
#     0     1
# 0 0.976 0.984
# 1 0.024 0.016
structure(c(0.976, 0.024, 0.984, 0.016), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("0", "1")), names = c("", "")))

invisible("En rel. con drogas,hay más hospitalizados x1.5")
round(prop.table(
  table(
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec,
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_rel_dg
  ),2),3)
#     0     1
# 0 0.984 0.976
# 1 0.016 0.024
structure(c(0.984, 0.016, 0.976, 0.024), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("0", "1")), names = c("", 
  "")))

invisible("En otras causas, hay menos muertes en PPOO")
table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$pueblo_originario_rec)
#         0       1
# FALSE 38653   620
# TRUE   1529    41
structure(c(38653L, 1529L, 620L, 41L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En rel. con drogas, en términos absolutos hay más mortalidades")
invisible("En rel. con drogas, PPOO registra más mortalidades")
table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$pueblo_originario_rec)
#           0       1
# FALSE 185982   4589
# TRUE    5312    196
structure(c(185982L, 5312L, 4589L, 196L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En otras causas, PPOO hay 1.66 más prob muerte")
round(prop.table(
  table(
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$pueblo_originario_rec
  ),2),3)
#         0     1
# FALSE 0.962 0.938
# TRUE  0.038 0.062

invisible("En rel. con drogas, en términos relativos no hay más mortalidades, hasta menos; ")
invisible("PPOO hay 1.5 más prob prob muerte")
round(prop.table(
  table(
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$pueblo_originario_rec
  ),2),3)
#         0     1
# FALSE 0.972 0.959
# TRUE  0.028 0.041
structure(c(0.972, 0.028, 0.959, 0.041), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("FALSE", "TRUE"), c("0", "1")), names = c("", "")))
invisible("Pero de los que mueren q reportan PPOO el % es menor en rel. con drogas (x0.66)")
#0.041/0.062

invisible("Hacer un mantel haenzel")

#https://rpubs.com/mbounthavong/confounding_interaction

matrix.array2 <- array(c(
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$pueblo_originario_rec,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr!=2), 
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$pueblo_originario_rec,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr!=2)
), dim = c(2, 2, 2))

epi.2by2(matrix.array2)
# Outcome +    Outcome -      Total                 Inc risk *
# Exposed +          237         5209       5446        4.35 (3.83 to 4.93)
# Exposed -         6841       224635     231476        2.96 (2.89 to 3.03)
# Total             7078       229844     236922        2.99 (2.92 to 3.06)
# 
# 
# Point estimates and 95% CIs:
#   Inc risk ratio (crude)                         1.47 (1.30, 1.67)
# Inc risk ratio (M-H)                           1.50 (1.32, 1.70)
# Inc risk ratio (crude:M-H)                     0.98
# Inc odds ratio (crude)                         1.49 (1.31, 1.71)
# Inc odds ratio (M-H)                           1.52 (1.33, 1.74)
# Inc odds ratio (crude:M-H)                     0.98
# Attrib risk in the exposed (crude) *           1.40 (0.85, 1.94)
# Attrib risk in the exposed (M-H) *             1.45 (-2.16, 5.06)
# Attrib risk (crude:M-H)                        0.96
#   M-H test of homogeneity of IRRs: chi2(1) = 0.349 Pr>chi2 = 0.555
# M-H test of homogeneity of ORs: chi2(1) = 0.453 Pr>chi2 = 0.501
# Test that M-H adjusted OR = 1:  chi2(1) = 39.319 Pr>chi2 = <0.001
# Wald confidence limits
# M-H: Mantel-Haenszel; CI: confidence interval
# * Outcomes per 100 population units 

#No modifica el riesgo ni los ORs el hecho de controlar por el tipo de enfermedad

#La prueba de Mantel-Haenszel de homogeneidad de los OR mostró que no hubo 
#evidencia de heterogeneidad entre los estudios (p = 0,463)

## 3.e. análisis todos ---------------------------------------------------

invisible("generamos un criterio inclusivo")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec<-
  ifelse(rowSums(dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("cya_conadi_rec","rsh_rec","pueblo_originario_rec")])>0,1,0)

paste0(" PPOO inclusivo (de válidos [hicimos q todos sean válidos]): ",
       scales::percent(
         prop.table(
           table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec))[2]
         )
)
#[1] " PPOO inclusivo (de válidos [hicimos q todos sean válidos]): 8%"

invisible("En otras causas, hay menos hospitalizados x 0.56")
round(prop.table(
  table(
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec,
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_notrel_dg
    ),2),3)
#   0     1
# 0 0.909 0.948
# 1 0.091 0.052
structure(c(0.909, 0.091, 0.948, 0.052), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("0", "1")), names = c("", "")))

invisible("En rel. con drogas,hay más hospitalizados x1.78")
round(prop.table(
  table(
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec,
    dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_rel_dg
  ),2),3)
#     0     1
# 0 0.948 0.909
# 1 0.052 0.091
structure(c(0.948, 0.052, 0.909, 0.091), class = "table", dim = c(2L, 
  2L), dimnames = structure(list(c("0", "1"), c("0", "1")), names = c("", "")))

invisible("En otras causas, hay menos muertes en PPOO")
table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$inclusivo_rec)
#         0       1
# FALSE 37217  2056
# TRUE   1517    53
structure(c(37217L, 1517L, 2056L, 53L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En rel. con drogas, en términos absolutos hay más mortalidades")
invisible("En rel. con drogas, PPOO registra más mortalidades")
table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$inclusivo_rec)
#         0       1
# FALSE 173101  17470
# TRUE    5231    277
structure(c(173101L, 5231L, 17470L, 277L), dim = c(2L, 2L), dimnames = structure(list(
  c("FALSE", "TRUE"), c("0", "1")), names = c("", "")), class = "table")

invisible("En otras causas, PPOO hay menos prob muerte (x0.64)")
round(prop.table(
  table(
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr==2,
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$inclusivo_rec
  ),2),3)
#         0     1
# FALSE 0.961 0.975
# TRUE  0.039 0.025
structure(c(0.961, 0.039, 0.975, 0.025), class = "table", dim = c(2L, 
    2L), dimnames = structure(list(c("FALSE", "TRUE"), c("0", "1")), names = c("", "")))
invisible("En rel. con drogas, en términos relativos no hay más mortalidades, hasta menos; ")
invisible("PPOO hay menos prob muerte (x0.53)")
round(prop.table(
  table(
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr==2,
    subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$inclusivo_rec
  ),2),3)
#         0     1
# FALSE 0.971 0.984
# TRUE  0.029 0.016

invisible("Pero de los que mueren q son PPOO el % es menor en rel. con drogas (x0.64)")
#0.016/0.025

invisible("Hacer un mantel haenzel")
matrix.array3 <- array(c(
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$inclusivo_rec,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr!=2), 
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$inclusivo_rec,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1)$cond_egr!=2)
), dim = c(2, 2, 2))

epi.2by2(matrix.array3)
#gemini("interpreta esto (sólo lo más importante): 
# Outcome +    Outcome -      Total                 Inc risk *
# Exposed +          330        19526      19856        1.66 (1.49 to 1.85)
# Exposed -         6748       210318     217066        3.11 (3.04 to 3.18)
# Total             7078       229844     236922        2.99 (2.92 to 3.06)
# 
# 
# Point estimates and 95% CIs:
# Inc risk ratio (crude)                         0.53 (0.48, 0.60)
# Inc risk ratio (M-H)                           0.55 (0.49, 0.61)
# Inc risk ratio (crude:M-H)                     0.98
# Inc odds ratio (crude)                         0.53 (0.47, 0.59)
# Inc odds ratio (M-H)                           0.54 (0.48, 0.60)
# Inc odds ratio (crude:M-H)                     0.98
# Attrib risk in the exposed (crude) *           -1.45 (-1.64, -1.25)
# Attrib risk in the exposed (M-H) *             -1.38 (-1.98, -0.78)
# Attrib risk (crude:M-H)                        1.05
#   M-H test of homogeneity of IRRs: chi2(1) = 1.540 Pr>chi2 = 0.215
# M-H test of homogeneity of ORs: chi2(1) = 1.585 Pr>chi2 = 0.208
# Test that M-H adjusted OR = 1:  chi2(1) = 121.061 Pr>chi2 = <0.001
# Wald confidence limits
# M-H: Mantel-Haenszel; CI: confidence interval
# * Outcomes per 100 population units 

# - El OR de exposición es de 0.58 (0.54, 0.62)
# - La prueba de que el OR ajustado por MH es igual a 1 es significativa (p<0,001), 
# lo que sugiere que la exposición está asociada con un mayor riesgo de resultado negativo.


rm("dtX2023_12_05_DatosEgresosHosp_encrip") #ahora sí


# 4. análisis de clasificación ---------------------------------------------------
source("https://raw.githubusercontent.com/ianhussey/SCED/master/R/ruscios_A.R")

v <- c("CI", "CYA", "PPOO", "RSH")
combinaciones <- combn(v, 2)
print(combinaciones)
# [,1]  [,2]   [,3]  [,4]   [,5]  [,6]  
# [1,] "CI"  "CI"   "CI"  "CYA"  "CYA" "PPOO"
# [2,] "CYA" "PPOO" "RSH" "PPOO" "RSH" "RSH" 
perc_clas<-
function(x,y){
paste0("% clasificación correcta: ", 
 scales::percent(sum(diag(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[x]], 
        dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[y]])), na.rm=T)/sum(
          table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[x]], 
                dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[y]]), na.rm=T))
)
}

invisible("Para obtener intervalos de confianza de correlación policórica")
rho_ci <- function(x){
  rho<-round(x[["rho"]],3)
  ci_int<-round(readr::parse_double(stringr::str_extract(capture.output(print(x))[2], "(?<=\\().*?(?=\\))"))*1.96,3)
  return(paste0(rho, " (", rho-ci_int, ", ", rho+ci_int,")"))
}

#https://stackoverflow.com/questions/63677484/get-confidence-intervals-for-phi-statistics-using-bootstrapping-in-r
boot.fun <- function(data, x, y, inds){
  # x= es el test (a ser testeado)
  # y el estándar de oro
  
  # Convertir x y y en símbolos
  x_sym <- ensym(x)
  y_sym <- ensym(y)
  
  # Crear una fórmula con los símbolos
  formula <- as.formula(paste0("~ (!", deparse(x_sym), ") + (!", deparse(y_sym),")"))
  
  # Calcular la tabla de contingencia y el coeficiente phi
  tab <- xtabs(formula = formula,data[inds, ])
  psych::phi(tab)
}


sens_spec<-
  function(x,y){
    
  # Disease +	Disease -	Total

  # Test +	a	b	a+b
  # Test -	c	d	c+d

  #   Total	a+c	b+d	a+b+c+d
  #   
  # x= es el test (a ser testeado)
  # y el estándar de oro
  esp_sens<- 
    epi.tests(table(!dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[x]], 
                    !dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[y]]), conf.level = 0.95)$detail %>% 
      dplyr::mutate(res= paste0(sprintf(est, fmt = '%#.2f'), 
                                " (",sprintf(lower, fmt = '%#.2f'), "; ", 
                                sprintf(upper, fmt = '%#.2f'),")")) %>% 
  dplyr::filter(statistic %in% c("se", "sp")) %>% 
  dplyr::select(statistic, res)
    return(
      esp_sens
    )
  }

invisible("Definir número de bootstrap (remuestreos)")
n_boot<- 2e2

suppressMessages(suppressWarnings(library(ROCit)))
## Warning: package 'ROCit' was built under R version 3.5.2
message("Error in convertclass(class, reference = negref) :    class must have exactly two unique values)")

pol_p1<-
  polychor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
           dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, std.err=T, ML=T)
message(capture.output(print(pol_p1))[c(2,3)])
#Polychoric Correlation, ML est. = -0.1659 (0.01196)
rho_ci(pol_p1)
#"-0.164 (-0.188, -0.14)"
set.seed(2125)
out <- boot::boot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                  x="ci_conadi", y="cya_conadi", 
                  statistic=boot.fun, 
                  R=n_boot,
                  parallel="multicore")
#boot.ci(out) #error
#bootstrap percentil 
IC_inferior <- quantile(out$t, probs = 0.025)
IC_superior <- quantile(out$t, probs = 0.975)
paste0("Phi", 
       psych::phi(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
                        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi)),
       " (95% IC: ",
       sprintf("%1.2f", IC_inferior),", ",
       sprintf("%1.2f", IC_superior),
       ")"
)
#[1] "Phi-0.09 (95% IC: -0.11, -0.08)"

pol_p2<-
  polychor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
           dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec, std.err=T, ML=T)
set.seed(2125)
out2 <- boot::boot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                  x="ci_conadi", y="pueblo_originario_rec", 
                  statistic=boot.fun, 
                  R=n_boot,
                  parallel="multicore")
#boot.ci(out2) #error
#bootstrap percentil 
IC_inferior2 <- quantile(out2$t, probs = 0.025)
IC_superior2 <- quantile(out2$t, probs = 0.975)
paste0("Phi", 
       psych::phi(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
                        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec)),
       " (95% IC: ",
       sprintf("%1.2f", IC_inferior2),", ",
       sprintf("%1.2f", IC_superior2),
       ")"
)
#[1] "Phi-0.09 (95% IC: -0.11, -0.08)""

message(capture.output(print(pol_p2))[c(2,3)])
#Polychoric Correlation, ML est. = -0.1716 (0.01229)
rho_ci(pol_p2)
#[1] "-0.171 (-0.195, -0.147)"

pol_p3<-
  polychor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
           dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec, std.err=T, ML=T)
set.seed(2125)
out3 <- boot::boot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                  x="ci_conadi", y="rsh_rec", 
                  statistic=boot.fun, 
                  R=n_boot,
                  parallel="multicore")
#boot.ci(out3) #error
#bootstrap percentil 
IC_inferior3 <- quantile(out3$t, probs = 0.025)
IC_superior3 <- quantile(out3$t, probs = 0.975)
paste0("Phi", 
       psych::phi(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
                        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec)),
       " (95% IC: ",
       sprintf("%1.2f", IC_inferior3),", ",
       sprintf("%1.2f", IC_superior3),
       ")"
)
#[1] "Phi-0.38 (95% IC: -0.39, -0.37)"

message(capture.output(print(pol_p3))[c(2,3)])
#Polychoric Correlation, ML est. = -0.6122 (0.00835)
rho_ci(pol_p3)
#[1] "-0.613 (-0.629, -0.597)"

pol_p4<-
  polychor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, 
           dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec, std.err=T, ML=T)
set.seed(2125)
out4 <- boot::boot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                  x="cya_conadi", y="pueblo_originario_rec", 
                  statistic=boot.fun, 
                  R=n_boot,
                  parallel="multicore")
#boot.ci(out4)
#bootstrap percentil 
IC_inferior4 <- quantile(out4$t, probs = 0.025)
IC_superior4 <- quantile(out4$t, probs = 0.975)
paste0("Phi", 
       psych::phi(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, 
                        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec)),
       " (95% IC: ",
       sprintf("%1.2f", IC_inferior4),", ",
       sprintf("%1.2f", IC_superior4),
       ")"
)
#[1] "Phi0.30 (95% IC: 0.28, 0.31)"

message(capture.output(print(pol_p4))[c(2,3)])
#Polychoric Correlation, ML est. = 0.5134 (0.0109)
rho_ci(pol_p4)
#[1] ""0.515 (0.494, 0.536)"

pol_p5<-
  polychor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, 
           dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec, std.err=T, ML=T)
set.seed(2125)
out5 <- boot::boot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                  x="cya_conadi", y="rsh_rec", 
                  statistic=boot.fun, 
                  R=n_boot,
                  parallel="multicore")
#boot.ci(out)
#bootstrap percentil 
IC_inferior5 <- quantile(out5$t, probs = 0.025)
IC_superior5 <- quantile(out5$t, probs = 0.975)
paste0("Phi", 
       psych::phi(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, 
                        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec)),
       " (95% IC: ",
       sprintf("%1.2f", IC_inferior5),", ",
       sprintf("%1.2f", IC_superior5),
       ")"
)
#[1] "Phi-0.16 (95% IC: -0.17, -0.14)"

message(capture.output(print(pol_p5))[c(2,3)])
#Polychoric Correlation, ML est. = -0.2856 (0.01203)
rho_ci(pol_p5)
#"-0.284 (-0.308, -0.26)"

pol_p6<-
  polychor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec, 
           dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec, std.err=T, ML=T)
set.seed(2125)
out6 <- boot::boot(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                  x="pueblo_originario_rec", y="rsh_rec", 
                  statistic=boot.fun, 
                  R=n_boot,
                  parallel="multicore")
#boot.ci(out)
#bootstrap percentil 
IC_inferior6 <- quantile(out6$t, probs = 0.025)
IC_superior6 <- quantile(out6$t, probs = 0.975)
paste0("Phi", 
       psych::phi(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_rec, 
                        dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh_rec)),
       " (95% IC: ",
       sprintf("%1.2f", IC_inferior6),", ",
       sprintf("%1.2f", IC_superior6),
       ")"
)
#"Phi0.27 (95% IC: 0.26, 0.27)"

message(capture.output(print(pol_p6))[c(2,3)])
#Polychoric Correlation, ML est. = 0.6335 (0.005547)
rho_ci(pol_p6)
#[1] "0.633 (0.622, 0.644)"

invisible("CI CONADI")
broom::tidy(caret::confusionMatrix(factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi), 
                  factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi)))
# term                 class estimate conf.low conf.high p.value
# <chr>                <chr>    <dbl>    <dbl>     <dbl>   <dbl>
# 1 accuracy             NA      0.465     0.458     0.472       1
# 2 kappa                NA     -0.0724   NA        NA          NA
# 3 mcnemar              NA     NA        NA        NA           0
# 4 sensitivity          0       0.479    NA        NA          NA
# 5 specificity          0       0.404    NA        NA          NA
# 6 pos_pred_value       0       0.773    NA        NA          NA
# 7 neg_pred_value       0       0.155    NA        NA          NA
# 8 precision            0       0.773    NA        NA          NA
# 9 recall               0       0.479    NA        NA          NA
# 10 f1                   0       0.592    NA        NA          NA
# 11 prevalence           0       0.809    NA        NA          NA
# 12 detection_rate       0       0.388    NA        NA          NA
# 13 detection_prevalence 0       0.502    NA        NA          NA
# 14 balanced_accuracy    0       0.442    NA        NA          NA

sens_spec("ci_conadi","cya_conadi")
# statistic               res
# 1        se 0.41 (0.39; 0.42)
# 2        sp 0.48 (0.47; 0.49)

# - La precisión del modelo para clasificar correctamente las observaciones es del 46,5%.
# - La sensibilidad del modelo para detectar casos positivos es del 47,9%.
# - La especificidad del modelo para detectar casos negativos es del 40,4%.
perc_clas("ci_conadi","cya_conadi")
#[1] "% clasificación correcta: 46%"

broom::tidy(caret::confusionMatrix(factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi), 
                                   factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_01)))
# term                 class estimate conf.low conf.high p.value
# <chr>                <chr>    <dbl>    <dbl>     <dbl>   <dbl>
# 1 accuracy             NA      0.466     0.460     0.473       1
# 2 kappa                NA     -0.0695   NA        NA          NA
# 3 mcnemar              NA     NA        NA        NA           0
# 4 sensitivity          0       0.481    NA        NA          NA
# 5 specificity          0       0.396    NA        NA          NA
# 6 pos_pred_value       0       0.795    NA        NA          NA
# 7 neg_pred_value       0       0.135    NA        NA          NA
# 8 precision            0       0.795    NA        NA          NA
# 9 recall               0       0.481    NA        NA          NA
# 10 f1                   0       0.599    NA        NA          NA
# 11 prevalence           0       0.830    NA        NA          NA
# 12 detection_rate       0       0.399    NA        NA          NA
# 13 detection_prevalence 0       0.502    NA        NA          NA
# 14 balanced_accuracy    0       0.439    NA        NA          NA

sens_spec("pueblo_originario_01","ci_conadi")  #respetar orden
# statistic               res
# 1        se 0.14 (0.13; 0.14)
# 2        sp 0.80 (0.79; 0.80)

# - La precisión del modelo es del 46,6%.
# - La sensibilidad del modelo es del 48,1% y la especificidad del 39,6%.
# - El valor predictivo positivo es del 79,5% y el valor predictivo negativo del 13,5%.
perc_clas("ci_conadi","pueblo_originario_01")
#[1] "% clasificación correcta: 47%"

broom::tidy(caret::confusionMatrix(factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi), 
                                   factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh)))
# term                 class estimate conf.low conf.high p.value
# <chr>                <chr>    <dbl>    <dbl>     <dbl>   <dbl>
# 1 accuracy             NA      0.334     0.328     0.340       1
# 2 kappa                NA     -0.330    NA        NA          NA
# 3 mcnemar              NA     NA        NA        NA           0
# 4 sensitivity          0       0.186    NA        NA          NA
# 5 specificity          0       0.386    NA        NA          NA
# 6 pos_pred_value       0       0.0970   NA        NA          NA
# 7 neg_pred_value       0       0.572    NA        NA          NA
# 8 precision            0       0.0970   NA        NA          NA
# 9 recall               0       0.186    NA        NA          NA
# 10 f1                   0       0.127    NA        NA          NA
# 11 prevalence           0       0.262    NA        NA          NA
# 12 detection_rate       0       0.0487   NA        NA          NA
# 13 detection_prevalence 0       0.502    NA        NA          NA
# 14 balanced_accuracy    0       0.286    NA        NA          NA

sens_spec("ci_conadi","rsh")
# statistic               res
# 1        se 0.39 (0.38; 0.39)
# 2        sp 0.18 (0.17; 0.20)

#"- La precisión del modelo es baja, con una exactitud del 33,4%.
#- La sensibilidad del modelo es baja, con un 18,6% de los casos positivos correctamente identificados.
#- La especificidad del modelo es moderada, con un 38,6% de los casos negativos correctamente identificados.
perc_clas("ci_conadi","rsh")
#[1] "% clasificación correcta: 33%"

invisible("está alta")
broom::tidy(caret::confusionMatrix(factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi), 
                                   factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_01)))
# term                 class estimate conf.low conf.high   p.value
# <chr>                <chr>    <dbl>    <dbl>     <dbl>     <dbl>
# 1 accuracy             NA       0.791    0.786     0.797  1   e+ 0
# 2 kappa                NA       0.297   NA        NA     NA       
# 3 mcnemar              NA      NA       NA        NA      6.49e-12
# 4 sensitivity          0        0.862   NA        NA     NA       
# 5 specificity          0        0.450   NA        NA     NA       
# 6 pos_pred_value       0        0.884   NA        NA     NA       
# 7 neg_pred_value       0        0.4     NA        NA     NA       
# 8 precision            0        0.884   NA        NA     NA       
# 9 recall               0        0.862   NA        NA     NA       
# 10 f1                   0        0.873   NA        NA     NA       
# 11 prevalence           0        0.830   NA        NA     NA       
# 12 detection_rate       0        0.715   NA        NA     NA       
# 13 detection_prevalence 0        0.809   NA        NA     NA       
# 14 balanced_accuracy    0        0.656   NA        NA     NA       

sens_spec("pueblo_originario_01","cya_conadi") #respetar orden
# statistic               res
# 1        se 0.40 (0.39; 0.42)
# 2        sp 0.88 (0.88; 0.89)

# - La precisión del modelo es del 79,1%.
# - La sensibilidad del modelo es del 86,2%.
# - La especificidad del modelo es del 45,0%.

perc_clas("cya_conadi","pueblo_originario_01")
#[1] "% clasificación correcta: 79%"

invisible("CYA CONADI")
broom::tidy(caret::confusionMatrix(factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi), 
                                   factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh)))
# term                 class estimate conf.low conf.high p.value
# <chr>                <chr>    <dbl>    <dbl>     <dbl>   <dbl>
# 1 accuracy             NA      0.298     0.292     0.304       1
# 2 kappa                NA     -0.0840   NA        NA          NA
# 3 mcnemar              NA     NA        NA        NA           0
# 4 sensitivity          0       0.705    NA        NA          NA
# 5 specificity          0       0.154    NA        NA          NA
# 6 pos_pred_value       0       0.227    NA        NA          NA
# 7 neg_pred_value       0       0.596    NA        NA          NA
# 8 precision            0       0.227    NA        NA          NA
# 9 recall               0       0.705    NA        NA          NA
# 10 f1                   0       0.344    NA        NA          NA
# 11 prevalence           0       0.261    NA        NA          NA
# 12 detection_rate       0       0.184    NA        NA          NA
# 13 detection_prevalence 0       0.809    NA        NA          NA
# 14 balanced_accuracy    0       0.429    NA        NA          NA

sens_spec("cya_conadi","rsh_rec")
# statistic               res
# 1        se 0.15 (0.15; 0.16)
# 2        sp 0.71 (0.69; 0.72)

perc_clas("cya_conadi","rsh")
#[1] "% clasificación correcta: 30%"

invisible("PPOO rsh")
broom::tidy(caret::confusionMatrix(factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_01), 
                                   factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh)))
sens_spec("pueblo_originario_01","rsh_rec") #respetar orden
# statistic               res
# 1        se 0.17 (0.16; 0.18)
# 2        sp 0.99 (0.99; 0.99)

perc_clas("rsh","pueblo_originario_01")
# [1] "% clasificación correcta: 34%"

invisible("CONADI CYA vs. CI")
roc1 <- roc(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, 
            dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi)
  roc1_auc<-ci.auc(roc1, method= "bootstrap", boot.n= 500)
paste0("AUC= ", round(attr(roc1_auc,"auc")[[1]],3),
       " (95% CI: ",round(as.numeric(roc1_auc[1]),3), ", ",
       round(as.numeric(roc1_auc[3]),3),")")
#[1] "AUC= 0.558 (95% CI: 0.55, 0.566)"
invisible("CONADI CYA vs. rsh")
roc2 <- roc(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, 
            dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh)
roc2_auc<-ci.auc(roc2, method= "bootstrap", boot.n= 500)
paste0("AUC= ", round(attr(roc2_auc,"auc")[[1]],3),
       " (95% CI: ",round(as.numeric(roc2_auc[1]),3), ", ",
       round(as.numeric(roc2_auc[3]),3),")")
#[1] "AUC= 0.412 (95% CI: 0.403, 0.42)"
invisible("CONADI CYA vs. pueblo_originario_01")
roc3 <- roc(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cya_conadi, 
            dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_01)
roc3_auc<-ci.auc(roc3, method= "bootstrap", boot.n= 500)
paste0("AUC= ", round(attr(roc3_auc,"auc")[[1]],3),
       " (95% CI: ",round(as.numeric(roc3_auc[1]),3), ", ",
       round(as.numeric(roc3_auc[3]),3),")")
#[1] "AUC= 0.643 (95% CI: 0.635, 0.65)"
invisible("RSH vs. pueblo_originario_01")
roc4 <- roc(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh, 
            dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_01)
roc4_auc<-ci.auc(roc4, method= "bootstrap", boot.n= 500)
paste0("AUC= ", round(attr(roc4_auc,"auc")[[1]],3),
       " (95% CI: ",round(as.numeric(roc4_auc[1]),3), ", ",
       round(as.numeric(roc4_auc[3]),3),")")
#[1] "AUC= 0.499 (95% CI: 0.493, 0.505)"
invisible("CONADI CI vs. pueblo_originario_01")
roc5 <- roc(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
            dtX2023_12_05_DatosEgresosHosp_encrip_filt5$pueblo_originario_01)
  roc5_auc<-ci.auc(roc5, method= "bootstrap", boot.n= 500)
paste0("AUC= ", round(attr(roc5_auc,"auc")[[1]],3),
       " (95% CI: ",round(as.numeric(roc5_auc[1]),3), ", ",
       round(as.numeric(roc5_auc[3]),3),")")
#[1] "AUC= 0.465 (95% CI: 0.46, 0.471)"
invisible("RSH vs. CONADI CI")
roc6 <- roc(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$ci_conadi, 
            dtX2023_12_05_DatosEgresosHosp_encrip_filt5$rsh)
  roc6_auc<-ci.auc(roc6, method= "bootstrap", boot.n= 500)
paste0("AUC= ", round(attr(roc6_auc,"auc")[[1]],3),
       " (95% CI: ",round(as.numeric(roc6_auc[1]),3), ", ",
       round(as.numeric(roc6_auc[3]),3),")")
#[1] "AUC= 0.335 (95% CI: 0.329, 0.34)"

roclist <- list("CYA vs. CI" = roc1,
                "CI  vs. PPOO" = roc5,
                "RSH  vs. CI" = roc6,                
                "CYA vs. PPOO" = roc3,
                "CYA vs. RSH" = roc2,
                "RSH vs. PPOO" = roc4
)

g <- ggroc(roclist, aes = "linetype", legacy.axes = TRUE) +
  geom_abline() +
  theme_classic() +
  labs(x = "1 - Especificidad",
       y = "Sensibilidad",
       linetype = "Contraste de criterio\nde clasificación")
g

ggsave("./_figs/roc.png",g, height=6, width=8, dpi=600)



# 5. Análisis exploratorio dirigido por datos ---------------------------------------------------


invisible("Recodificar en factores de manera binaria en 0's y 1's")
# dtX2023_12_05_DatosEgresosHosp_encrip_filt5<-
#   dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
#   dplyr::mutate(pueblo_originario_01_rec=dplyr::case_when(is.na(pueblo_originario_01)~1,
#                                                           pueblo_originario_01==1~2,
#                                                           pueblo_originario_01==0~3)) %>% 
#   dplyr::mutate(rsh_rec=dplyr::case_when(is.na(rsh)~1,
#                                          rsh==1~2,
#                                          rsh==0~3)) %>% 
#   dplyr::mutate(cya_conadi_rec=dplyr::case_when(is.na(cya_conadi)~1,
#                                                 cya_conadi==1~2,
#                                                 cya_conadi==0~3)) %>% 
#   dplyr::mutate(ci_conadi_rec=dplyr::case_when(is.na(ci_conadi)~1,
#                                                ci_conadi==1~2,
#                                                ci_conadi==0~3))

dtX2023_12_05_DatosEgresosHosp_encrip_filt5<-
  dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::mutate(pueblo_originario_01_rec=factor(dplyr::case_when(is.na(pueblo_originario_01)~1,
                                                          pueblo_originario_01==1~2,
                                                          pueblo_originario_01==0~3))) %>% 
  dplyr::mutate(rsh_rec=factor(dplyr::case_when(is.na(rsh)~1,
                                         rsh==1~2,
                                         rsh==0~3))) %>% 
  dplyr::mutate(cya_conadi_rec=factor(dplyr::case_when(is.na(cya_conadi)~1,
                                                cya_conadi==1~2,
                                                cya_conadi==0~3))) %>% 
  dplyr::mutate(ci_conadi_rec=factor(dplyr::case_when(is.na(ci_conadi)~1,
                                               ci_conadi==1~2,
                                               ci_conadi==0~3))) %>% 
  #generar la variable de "exposición"= subreporte
  dplyr::mutate(exposicion=dplyr::case_when(
    (rsh_rec==2|cya_conadi_rec==2|ci_conadi_rec==2) & pueblo_originario_01_rec!=2~1,
    T~0)) %>% 
  #cuando no corresponde: perdidos en RSH, CYA y CI CONADI
  dplyr::mutate(a_excluir=dplyr::case_when(
    rsh_rec==1&cya_conadi_rec==1&ci_conadi_rec==1~1,
    T~0)) 

invisible("Recodificar Previsión y Tramo de beneficios")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5$prev_benef<-
glue::glue("{factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$previ)} {factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$benef)}")

dtX2023_12_05_DatosEgresosHosp_encrip_filt5$prev_benef_rec <-
  car::recode(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$prev_benef,
              "'1 A'='FONASA AB';'1 B'='FONASA AB';'1 C'='FONASA CD';'1 D'='FONASA CD';'2 NA'='ISAPRE';'3 NA'='FFAA';'4 NA'='FFAA';'5 NA'='FFAA'")
table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$prev_benef,
      dtX2023_12_05_DatosEgresosHosp_encrip_filt5$prev_benef_rec, exclude=NULL)
#       FONASA AB FONASA CD ISAPRE & FFAA
# 1 A      52198         0             0
# 1 B      80633         0             0
# 1 C          0     20398             0
# 1 D          0     33183             0
# 2 NA         0         0         46812
# 3 NA         0         0          1190
# 4 NA         0         0          1390
# 5 NA         0         0          1118

# table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$glosa_prevision,
#       dtX2023_12_05_DatosEgresosHosp_encrip_filt5$prev_benef, exclude=NULL)

invisible("Hacer formato factor el Sexo")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5$sexo<-factor(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$sexo)

invisible("CAPREDENA   DIPRECA y SISA, se transforman en uno solo")
# dtX2023_12_05_DatosEgresosHosp_encrip_filt5$previ_rec<-
#   ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_filt5[[paste0("diag",i)]] %in% c(3,4,5),
#          3,
#          dtX2023_12_05_DatosEgresosHosp_encrip_filt5$previ)

#Para la muestra seleccionada de 236.922 episodios hospitalarios, esta cifra aumentó al...
paste0(" Mortalidad: ",
       table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr)[2]," (",
       scales::percent(
         prop.table(
           table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr))[2]
       ),")"
)
#[1] " Mortalidad: 7078 (3%)"

## 5.00 Tabla 1 ---------------------------------------------------
#to format rows in bold
format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){
  
  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]  
  
  for (r in rows){
    for(c in cols){
      
      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])
      
      # Update formatting
      df[r, c] <- ifelse(nchar(df[r, c])==0,"",paste0(markup, gsub(" ", "", df[r, c]), markup))
    }
  }
  
  return(df)
}
#To produce line breaks in messages and warnings
knitr::knit_hooks$set(
  error = function(x, options) {
    paste('\n\n<div class="alert alert-danger">',
          gsub('##', '\n', gsub('^##\ Error', '**Error**', x)),
          '</div>', sep = '\n')
  },
  warning = function(x, options) {
    paste('\n\n<div class="alert alert-warning">',
          gsub('##', '\n', gsub('^##\ Warning:', '**Warning**', x)),
          '</div>', sep = '\n')
  },
  message = function(x, options) {
    paste('<div class="message">',
          gsub('##', '\n', x),
          '</div>', sep = '\n')
  }
)

as.data.frame.TableOne <- function(x, ...) {
  
  capture.output(print(x, showAllLevels = TRUE, ...) -> x)
  
  y <- as.data.frame(x)
  y$charactersitic <- dplyr::na_if(rownames(x), "")
  y <- y %>%
    tidyr::fill(charactersitic, .direction = "down") %>%
    dplyr::select(charactersitic, everything())
  
  rownames(y) <- NULL
  y 
}

require(tableone)
## Vector of variables to summarize
myVars <- c("cond_egr", "exposicion", "edad_anos", "edad_anos_rec","sexo", "prev_benef_rec")
## Vector of categorical variables that need transformation
catVars <- c("exposicion", "cond_egr", "sexo", "prev_benef_rec","edad_anos_rec")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, strata = c("SUD_rel_dg", "a_excluir"), 
                       data = dtX2023_12_05_DatosEgresosHosp_encrip_filt5, factorVars = catVars, addOverall = T)

print(tab2, 
      nonnormal = "edad_anos", 
      formatOptions = list(big.mark = ".", decimal.mark=","), 
      smd = T, noSpaces = T) #exact = "stage",

df_tableone<-
as.data.frame.TableOne(tab2, 
                       nonnormal = "edad_anos", 
                       formatOptions = list(big.mark = ".", decimal.mark=","), 
                       smd = T, 
                       noSpaces = T)

cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

cb(df_tableone)

## 5.01 exposición ---------------------------------------------------


chisq.test(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$exposicion,dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_rel_dg))
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$exposicion,     dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_rel_dg)
# X-squared = 357.65, df = 1, p-value < 2.2e-16


freqTable <- table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$exposicion, dtX2023_12_05_DatosEgresosHosp_encrip_filt5$SUD_rel_dg)
propTable <- round(prop.table(freqTable) * 100, 1)

# Initialize an empty matrix to hold the combined strings, with the same dimensions as the original tables
combinedMatrix <- matrix(nrow = nrow(freqTable), ncol = ncol(freqTable))

# Fill the matrix with combined strings
for (i in 1:nrow(freqTable)) {
  for (j in 1:ncol(freqTable)) {
    combinedMatrix[i, j] <- paste0(freqTable[i, j], " (", propTable[i, j], "%)")
  }
}

# Optionally, convert the matrix to a more readable format, such as a data frame
combinedDF <- as.data.frame.matrix(combinedMatrix)

#Los eventos hospitalarios que subreportan pertenencia a PPOO en quienes ingresan por otras causas fueron 

### 5.011 exposición general ---------------------------------------------------

#De la muestra final ...
paste0(" Subreporte (de todos): ",format(as.numeric(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$exposicion)[2]),big.mark=".", decimal.mark=",")," (",
       as.character(scales::percent(prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$exposicion))[2])),")")
# [1] " Subreporte (de todos): 18.072 (8%)"

# Hay sujetos que nunca tuvieron alguna clasificación, ya sea por RSH o CONADI: ..."

paste0(" Inválidos [personas que no tuvieron clasificación alguna /Casos válidos (sin perdidos en CI, CYA CONADI y RSH)]): ",
       format(as.numeric(table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==1)$exposicion)[1]),big.mark=".", decimal.mark=",")
)

paste0(" Subreporte (de válidos [sin personas que no tuvieron clasificación alguna /Casos válidos (sin perdidos en CI, CYA CONADI y RSH)]): ",
       format(as.numeric(table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0)$exposicion)[2]),big.mark=".", decimal.mark=","),
       " (",
       scales::percent(
         prop.table(
           table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0)$exposicion))[2]
       ),") de ",nrow(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0))
)
#[1] " Subreporte (de válidos [sin personas que no tuvieron clasificación alguna /Casos válidos (sin perdidos en CI, CYA CONADI y RSH)]): 18.072 (83%)"

paste0(" Subreporte (de sólo PPOO [con clasificación positiva en CI, CYA CONADI y RSH)]): ",
       format(as.numeric(table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, rsh_rec==2|ci_conadi_rec==2|cya_conadi_rec==2)$exposicion)[2]),big.mark=".", decimal.mark=","),
       " (",
       scales::percent(
         prop.table(
           table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5,  rsh_rec==2|ci_conadi_rec==2|cya_conadi_rec==2)$exposicion))[2]
       ),")"
)
#[1] " Subreporte (de sólo PPOO [con clasificación positiva en CI, CYA CONADI y RSH)]): 18.072 (83%)"

paste0("Alguna clasificación [Casos válidos (no perdidos en CI, CYA CONADI y RSH) ]: ",format(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$a_excluir)[[1]],big.mark=".", decimal.mark=","), " (",
       (round(prop.table(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$a_excluir)),3)*100)[[1]],"%)")
# [1] "Ninguna clasificación [Casos inválidos (perdidos en CI, CYA CONADI y RSH) ]: 21.787 (9.2%)"

#Dicho grupo al menos pertenece a un pueblo originario por al menos una clasificación...
invisible("NO EXISTEN CASOS QUE TENGAN PERDIDOS EN RSH O CONADI QUE NO TENGAN PUEBLO ORIGINARIO!!!!")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5%>% 
  dplyr::filter(a_excluir==0) %>% #nrow() #21.787
dplyr::ungroup() %>% 
  mutate(sumVar = rowSums(select(., c("ci_conadi","rsh","cya_conadi")), na.rm=T)) %>% 
  dplyr::filter(sumVar==0) %>% nrow()

### 5.012 mortalidad intrahosp, prev gen ---------------------------------------------------

invisible("Para combinar tablas con frecuencias y porcentajes")
combinarTablas <- function(dataframe, variable_exposicion, df_condition=NULL, variable_cond_egr) {
  dataframe<- if(is.null(df_condition)){
    dataframe
  } else{
    subset(dataframe,eval(parse(text=df_condition)))
  }
  # Crear la primera tabla de contingencia para el subconjunto específico
  tabla1 <-             table(
    dataframe[[variable_exposicion]],
    dataframe[[variable_cond_egr]]
  )
  
  # Crear la segunda tabla de proporciones para el dataframe completo
  tabla2 <- round(
    prop.table(
      table(
        dataframe[[variable_exposicion]],
        dataframe[[variable_cond_egr]]
      ), 
      1
    ),
    3
  )
  
  # Convertir las tablas en data frames
  df_tabla1 <- as.data.frame(tabla1)
  df_tabla2 <- as.data.frame(tabla2)
  names(df_tabla1) <- c(variable_exposicion, variable_cond_egr, "frecuencia")
  names(df_tabla2) <- c(variable_exposicion, variable_cond_egr,"proporcion")
  
  # Unir las tablas basándose en la columna 'exposicion'
  tabla_combinada <- merge(df_tabla1, df_tabla2, all = TRUE)
  
  return(tabla_combinada)
}



invisible("MORTALIDAD INTRAHOSPITALARIA, PREVALENCIAS GENERALES")
paste0("Mortalidad instrahospitalaria (de sólo PPOO [con clasificación positiva en CI, CYA CONADI y RSH)]): ",
       format(as.numeric(table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, rsh_rec==2|ci_conadi_rec==2|cya_conadi_rec==2)$cond_egr)[2]),big.mark=".", decimal.mark=","),
       " (",
       scales::percent(
         prop.table(
           table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5,  rsh_rec==2|ci_conadi_rec==2|cya_conadi_rec==2)$cond_egr))[2]
       ),") de ",nrow(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, rsh_rec==2|ci_conadi_rec==2|cya_conadi_rec==2))
)
paste0("Mortalidad instrahospitalaria  [Casos válidos (sin perdidos en CI, CYA CONADI y RSH) ]: ",
       format(as.numeric(table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0)$cond_egr)[2]),big.mark=".", decimal.mark=","),
       " (",
       scales::percent(
         prop.table(
           table(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5,  a_excluir==0)$cond_egr))[2]
       ),") de ",nrow(subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0))
)
paste0("Mortalidad instrahospitalaria (total): ",
       format(as.numeric(table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr)[2]),big.mark=".", decimal.mark=","),
       " (",
       scales::percent(
         prop.table(
           table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$cond_egr))[2]
       ),") de ",nrow(dtX2023_12_05_DatosEgresosHosp_encrip_filt5)
)

invisible("Prevalencia mortalidad intrahospitalaria")
invisible("volvemos a generar un criterio inclusivo")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec2<-
   ifelse(
     rowSums(dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","rsh","pueblo_originario_rec","cya_conadi")], na.rm=T)>0,
     1,0)
dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec2_sinar<-
  ifelse(
    rowSums(dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","rsh","cya_conadi")], na.rm=T)>0,
    1,0)

table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec, dtX2023_12_05_DatosEgresosHosp_encrip_filt5$inclusivo_rec2)
#añade 3662 casos más

dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","rsh","cya_conadi", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  dplyr::filter(inclusivo_rec2==1, inclusivo_rec==0, ci_conadi==1) %>% nrow()
invisible("por lo visto, son casos ci_conadi que se habían ido")


table(
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5)$a_excluir,
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5)$inclusivo_rec, exclude=NULL
)
#por lo visto son los 0x0 en esta tabla esos 3662

rbind.data.frame(
cbind.data.frame(ind="Total",combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                                            variable_exposicion="inclusivo_rec2", 
                                            #df_condition= "SUD_rel_dg==0 & rsh_rec==3|ci_conadi_rec==3|cya_conadi_rec==3",
                                            variable_cond_egr="cond_egr")),
cbind.data.frame(ind="Otras causas",combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                                            variable_exposicion="inclusivo_rec2", 
                                            df_condition= "SUD_rel_dg==0",
                                            variable_cond_egr="cond_egr")),
cbind.data.frame(ind="Rel. con consumo",combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                                            variable_exposicion="inclusivo_rec2", 
                                            df_condition= "SUD_rel_dg==1",
                                            variable_cond_egr="cond_egr"))
)%>% 
  dplyr::mutate(print=glue::glue('{format(frecuencia, big.mark=".", decimal.mark=",")} ({scales::percent(proporcion, accuracy=0.1, decimal.mark=",")})')) %>% 
  dplyr::select(-frecuencia, -proporcion) %>% 
  tidyr::pivot_wider(values_from=print, names_from=cond_egr)
#                 ind inclusivo_rec2              X1             X2
# 1            Total              0 206.877 (96,9%)   6.527 (3,1%)
# 2            Total              1  22.967 (97,7%)     551 (2,3%)
# 3     Otras causas              0  36.512 (96,1%)   1.478 (3,9%)
# 4     Otras causas              1   2.761 (96,8%)      92 (3,2%)
# 5 Rel. con consumo              0 170.365 (97,1%)   5.049 (2,9%)
# 6 Rel. con consumo              1  20.206 (97,8%)     459 (2,2%)
data.frame(ind = c("Total", "Total", "Otras causas", "Otras causas", 
"Rel. con consumo", "Rel. con consumo"), inclusivo_rec2 = structure(c(1L, 
2L, 1L, 2L, 1L, 2L), levels = c("0", "1"), class = "factor"), 
`1` = structure(c("206.877 (96,9%)", " 22.967 (97,7%)", " 36.512 (96,1%)", 
"  2.761 (96,8%)", "170.365 (97,1%)", " 20.206 (97,8%)"), class = c("glue", 
"character")), `2` = structure(c("  6.527 (3,1%)", "    551 (2,3%)", 
"  1.478 (3,9%)", "     92 (3,2%)", "  5.049 (2,9%)", "    459 (2,2%)"
), class = c("glue", "character")))

# Austin, P. C. (2009). The Relative Ability of Different Propensity 
# Score Methods to Balance Measured Covariates Between 
# Treated and Untreated Subjects in Observational Studies. Medical 
# Decision Making. https://doi.org/10.1177/0272989X09341755
smd_bin <- function(x,y){
  z <- x*(1-x)
  t <- y*(1-y)
  k <- sum(z,t)
  l <- k/2
  
  return((x-y)/sqrt(l))
  
}
smd_bin(0.09693908 ,0.00232566) #x is frequency in group 1, y frequency in group 2 e.g. race_black 0.8432 and 0.2028


invisible("Mortalidad en casos que al menos tengan un dato en CONADI o RSH, según autorreporte a PPOO")
rbind.data.frame(
cbind.data.frame(ind="Total",combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
            variable_exposicion="pueblo_originario_rec", 
            df_condition= "a_excluir==0 ",
            variable_cond_egr="cond_egr")),
cbind.data.frame(ind="Otras causas",combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
           variable_exposicion="pueblo_originario_rec", 
           df_condition= "a_excluir==0 &SUD_rel_dg==0",
           variable_cond_egr="cond_egr")),
cbind.data.frame(ind="Rel. con consumo",combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
           variable_exposicion="pueblo_originario_rec", 
           df_condition= "a_excluir==0 & SUD_rel_dg==1",
           variable_cond_egr="cond_egr"))
) %>% 
  dplyr::mutate(print=glue::glue('{format(frecuencia, big.mark=".", decimal.mark=",")} ({scales::percent(proporcion, accuracy=0.1, decimal.mark=",")})')) %>% 
  dplyr::select(-frecuencia, -proporcion)
#               ind inclusivo_rec2 cond_egr frecuencia proporcion
# 1            Total              1        1      21363      0.981
# 2            Total              1        2        424      0.019
# 3     Otras causas              1        1       2513      0.974
# 4     Otras causas              1        2         67      0.026
# 5 Rel. con consumo              1        1      18850      0.981
# 6 Rel. con consumo              1        2        357      0.019


#### 5.0121 mortalidad, inv. inconsistencias ---------------------------------------------------

invisible("Será posible que criterio inclusivo abarque a los casos que tienen al menos uno válido CONADI o RSH??")

# PARA RECORDAAAAAAR!!!
# dplyr::mutate(a_excluir=dplyr::case_when(
#   rsh_rec==1 & cya_conadi_rec==1 & ci_conadi_rec==1~1,
#   T~0)) 
dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","rsh","cya_conadi","a_excluir","cond_egr", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  dplyr::filter(a_excluir==0, inclusivo_rec2==0) %>% nrow()
invisible("No hay muertos excluidos sin valores en alguna de RSH, CI o CONADI y que no tenga al menos una pertenencia")

dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","rsh","cya_conadi","a_excluir","cond_egr", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  dplyr::filter(a_excluir==0, inclusivo_rec2_sinar==0,cond_egr==0) %>% nrow()
invisible("No hay muertos excluidos sin valores en alguna de RSH, CI o CONADI y que no tenga al menos una pertenencia flexible")

invisible("qué pasa con los a excluir: aquí 3715 son pueblo originario")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","pueblo_originario_rec","rsh","cya_conadi","a_excluir","cond_egr", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  dplyr::filter(a_excluir==0) %>% janitor::tabyl(inclusivo_rec2_sinar,pueblo_originario_rec)
# inclusivo_rec2_sinar     0     1
# 0                       18072 3715
#no hay inclusivo

invisible("")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","pueblo_originario_rec","rsh","cya_conadi","a_excluir","cond_egr", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  dplyr::filter(a_excluir==0) %>% janitor::tabyl(cond_egr,pueblo_originario_rec)
# cond_egr     0    1
#         1 17758 3605
#         2   314  110

invisible("filtramos datos no perdidos en RSH, CYA y CI CONADI, pero con pueblo originario válido: 1731 PPOO")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","pueblo_originario_rec","rsh","cya_conadi","a_excluir","cond_egr", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  dplyr::filter(!is.na(rsh),!is.na(cya_conadi),!is.na(cya_conadi)) %>% 
  janitor::tabyl(inclusivo_rec2_sinar,pueblo_originario_rec)
# inclusivo_rec2_sinar    0    1
#                   0 18072 3715
#1731 en pueblo originario; no hay inclusivo

invisible("En las bases de datos originales, filtramos datos no perdidos en RSH, CYA y CI CONADI, pero con pueblo originario válido")
dtX2023_12_05_DatosEgresosHosp_encrip_filt[,c("row_num_index","ci_conadi","pueblo_originario","rsh","cya_conadi")] %>% 
  #dplyr::filter(!is.na(rsh)|!is.na(cya_conadi)|!is.na(ci_conadi)) %>% #nrow() 64172, todos distintos a NA
  dplyr::filter(case_when(is.na(rsh) & is.na(cya_conadi)& is.na(ci_conadi)~F,T~T)) %>%  #64172
  dplyr::filter(pueblo_originario<11) %>% nrow()
#13914
invisible("Casos en los que puede sobrerreporte: que es identificarse pero no serlo recocido por inst. del estado")
# dplyr::mutate(a_excluir=dplyr::case_when(
#   rsh_rec==1 & cya_conadi_rec==1 & ci_conadi_rec==1~1,
#   T~0)) 

invisible("En las bases de datos originales, filtramos datos no perdidos en RSH, CYA y CI CONADI; 3715")
dtX2023_12_05_DatosEgresosHosp_encrip_filt[,c("row_num_index","ci_conadi","pueblo_originario","rsh","cya_conadi")] %>% 
  dplyr::filter(!is.na(rsh),!is.na(cya_conadi),!is.na(cya_conadi)) %>% 
  dplyr::filter(pueblo_originario<11) %>% #nrow() #13914
  dplyr::mutate(row_num_index=as.character(row_num_index)) %>% 
  dplyr::filter(row_num_index %in% as.character(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$row_num_index)) %>%
  #1731
  janitor::tabyl(pueblo_originario)%>% 
  dplyr::summarise(n=sum(n))
# 3715

invisible("Obviamente, es el complemetno: porque 1= en inclusivo_rec2_sinar significa tener registros objetivos")
invisible("mientras que 0 en a_excluir, es no tener ningún elemento de RSH, CONADI o CYA")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","pueblo_originario_rec","rsh","cya_conadi","a_excluir","cond_egr", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  janitor::tabyl(inclusivo_rec2_sinar,a_excluir)
# inclusivo_rec2_sinar     0      1
#                   0     0 215135
#                   1 21787      0
invisible("complemento perfecto")

dtX2023_12_05_DatosEgresosHosp_encrip_filt5[,c("ci_conadi","pueblo_originario_rec","rsh","cya_conadi","a_excluir","cond_egr", "inclusivo_rec","inclusivo_rec2","inclusivo_rec2_sinar")] %>% 
  janitor::tabyl(inclusivo_rec2,a_excluir)
# inclusivo_rec2     0      1
#             0     0 213404
#             1 21787   1731
invisible("yo no veo los ya excluidos, por eso esos 1731 1x1 no existen para mí")

### 5.013 vuelta a exp x submuestras ---------------------------------------------------

invisible("Muestra total")
rbind.data.frame(
combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
               variable_exposicion="exposicion", 
               #df_condition= "SUD_rel_dg==0 & rsh_rec==3|ci_conadi_rec==3|cya_conadi_rec==3",
               variable_cond_egr="cond_egr") %>% 
  dplyr::mutate(print= glue::glue("{format(frecuencia, big.mark='.', decimal.mark=',')} ({scales::percent(proporcion, accuracy=0.1, decimal.mark = ',')})")) %>% 
  dplyr::select(-frecuencia, -proporcion) %>% 
  dplyr::mutate(ind="Total") %>% 
  dplyr::select(ind, exposicion, cond_egr, print) %>% 
  dplyr::arrange(ind, cond_egr, exposicion)%>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=print),
combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
               variable_exposicion="exposicion", 
               df_condition= "SUD_rel_dg==0",
               variable_cond_egr="cond_egr") %>% 
  dplyr::mutate(print= glue::glue("{format(frecuencia, big.mark='.', decimal.mark=',')} ({scales::percent(proporcion, accuracy=0.1, decimal.mark = ',')})")) %>% 
  dplyr::select(-frecuencia, -proporcion) %>% 
  dplyr::mutate(ind="Otras causas") %>% 
  dplyr::select(ind, exposicion, cond_egr, print) %>% 
  dplyr::arrange(ind, cond_egr, exposicion) %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=print),
combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
               variable_exposicion="exposicion", 
               df_condition= "SUD_rel_dg==1",
               variable_cond_egr="cond_egr") %>% 
  dplyr::mutate(print= glue::glue("{format(frecuencia, big.mark='.', decimal.mark=',')} ({scales::percent(proporcion, accuracy=0.1, decimal.mark = ',')})")) %>% 
  dplyr::select(-frecuencia, -proporcion) %>% 
  dplyr::mutate(ind="Relacionados con el consumo de alcohol u otras sustancias") %>% 
  dplyr::select(ind, exposicion, cond_egr, print) %>% 
  dplyr::arrange(ind, cond_egr, exposicion) %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=print)
)
#                   ind                                                       exposicion `1`             `2`           
# <chr>                                                     <fct>      <glue>          <glue>        
#   1 Total                                                     0          212.086 (96,9%)   6.764 (3,1%)
# 2 Total                                                     1           17.758 (98,3%)     314 (1,7%)
# 3 Otras causas                                              0          37.132 (96,1%)   1.519 (3,9%) 
# 4 Otras causas                                              1           2.141 (97,7%)      51 (2,3%) 
# 5 Relacionados con el consumo de alcohol u otras sustancias 0          174.954 (97,1%)   5.245 (2,9%)
# 6 Relacionados con el consumo de alcohol u otras sustancias 1           15.617 (98,3%)     263 (1,7%)

invisible("Casos válidos")
rbind.data.frame(
  combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                 variable_exposicion="exposicion", 
                 df_condition= "a_excluir==0",
                 variable_cond_egr="cond_egr") %>% 
    dplyr::mutate(print= glue::glue("{format(frecuencia, big.mark='.', decimal.mark=',')} ({scales::percent(proporcion, accuracy=0.1, decimal.mark = ',')})")) %>% 
    dplyr::select(-frecuencia, -proporcion) %>% 
    dplyr::mutate(ind="Total") %>% 
    dplyr::select(ind, exposicion, cond_egr, print) %>% 
    dplyr::arrange(ind, cond_egr, exposicion)%>% 
    tidyr::pivot_wider(names_from=cond_egr, values_from=print),
  combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                 variable_exposicion="exposicion", 
                 df_condition= "a_excluir==0& SUD_rel_dg==0",
                 variable_cond_egr="cond_egr") %>% 
    dplyr::mutate(print= glue::glue("{format(frecuencia, big.mark='.', decimal.mark=',')} ({scales::percent(proporcion, accuracy=0.1, decimal.mark = ',')})")) %>% 
    dplyr::select(-frecuencia, -proporcion) %>% 
    dplyr::mutate(ind="Otras causas") %>% 
    dplyr::select(ind, exposicion, cond_egr, print) %>% 
    dplyr::arrange(ind, cond_egr, exposicion) %>% 
    tidyr::pivot_wider(names_from=cond_egr, values_from=print),
  combinarTablas(dataframe=dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
                 variable_exposicion="exposicion", 
                 df_condition= "a_excluir==0& SUD_rel_dg==1",
                 variable_cond_egr="cond_egr") %>% 
    dplyr::mutate(print= glue::glue("{format(frecuencia, big.mark='.', decimal.mark=',')} ({scales::percent(proporcion, accuracy=0.1, decimal.mark = ',')})")) %>% 
    dplyr::select(-frecuencia, -proporcion) %>% 
    dplyr::mutate(ind="Relacionados con el consumo de alcohol u otras sustancias") %>% 
    dplyr::select(ind, exposicion, cond_egr, print) %>% 
    dplyr::arrange(ind, cond_egr, exposicion) %>% 
    tidyr::pivot_wider(names_from=cond_egr, values_from=print)
)


invisible("Hacer un mantel haenzel, todos los casos")
matrix.array_f3 <- array(c(
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==0)$exposicion,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==0)$cond_egr!=2), 
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$exposicion,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)$cond_egr!=2)
), dim = c(2, 2, 2))

epi.2by2(matrix.array_f3)
#              Outcome +    Outcome -      Total                 Inc risk *
# Exposed +          314        17758      18072        1.74 (1.55 to 1.94)
# Exposed -         6764       212086     218850        3.09 (3.02 to 3.16)
# Total             7078       229844     236922        2.99 (2.92 to 3.06)
# 
# 
# Point estimates and 95% CIs:
# Inc risk ratio (crude)                         0.56 (0.50, 0.63)
# Inc risk ratio (M-H)                           0.57 (0.51, 0.64)
# Inc risk ratio (crude:M-H)                     0.98
# Inc odds ratio (crude)                         0.55 (0.49, 0.62)
# Inc odds ratio (M-H)                           0.57 (0.50, 0.63)
# Inc odds ratio (crude:M-H)                     0.98
# Attrib risk in the exposed (crude) *           -1.35 (-1.56, -1.15)
# Attrib risk in the exposed (M-H) *             -1.30 (-1.97, -0.63)
# Attrib risk (crude:M-H)                        1.04
#  M-H test of homogeneity of IRRs: chi2(1) = 0.066 Pr>chi2 = 0.797
#  M-H test of homogeneity of ORs: chi2(1) = 0.076 Pr>chi2 = 0.782
#  Test that M-H adjusted OR = 1:  chi2(1) = 98.510 Pr>chi2 = <0.001
#  Wald confidence limits
#  M-H: Mantel-Haenszel; CI: confidence interval
#  * Outcomes per 100 population units 


invisible("Hacer un mantel haenzel, Casos válidos (sin perdidos en CI, CYA CONADI y RSH)")
matrix.array_f4 <- array(c(
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==0& a_excluir==0)$exposicion,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==0& a_excluir==0)$cond_egr!=2), 
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1& a_excluir==0)$exposicion,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1& a_excluir==0)$cond_egr!=2)
), dim = c(2, 2, 2))

epi.2by2(matrix.array_f4)
#              Outcome +    Outcome -      Total                 Inc risk *
# Exposed +          314        17758      18072        1.74 (1.55 to 1.94)
# Exposed -          110         3605       3715        2.96 (2.44 to 3.56)
# Total              424        21363      21787        1.95 (1.77 to 2.14)
# 
# 
# Point estimates and 95% CIs:
# Inc risk ratio (crude)                         0.59 (0.47, 0.73)
# Inc risk ratio (M-H)                           0.58 (0.47, 0.72)
# Inc risk ratio (crude:M-H)                     1.01
# Inc odds ratio (crude)                         0.58 (0.47, 0.72)
# Inc odds ratio (M-H)                           0.58 (0.46, 0.72)
# Inc odds ratio (crude:M-H)                     1.01
# Attrib risk in the exposed (crude) *           -1.22 (-1.80, -0.65)
# Attrib risk in the exposed (M-H) *             -1.24 (-2.46, -0.02)
# Attrib risk (crude:M-H)                        0.99
#  M-H test of homogeneity of IRRs: chi2(1) = 0.016 Pr>chi2 = 0.900
#  M-H test of homogeneity of ORs: chi2(1) = 0.039 Pr>chi2 = 0.844
#  Test that M-H adjusted OR = 1:  chi2(1) = 24.786 Pr>chi2 = <0.001
#  Wald confidence limits
#  M-H: Mantel-Haenszel; CI: confidence interval
#  * Outcomes per 100 population units 



invisible("Hacer un mantel haenzel, solo PPOO")
matrix.array_f1 <- array(c(
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1& rsh_rec==3|ci_conadi_rec==3|cya_conadi_rec==3)$exposicion,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1& rsh_rec==3|ci_conadi_rec==3|cya_conadi_rec==3)$cond_egr!=2), 
  table(!subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1& rsh_rec==3|ci_conadi_rec==3|cya_conadi_rec==3)$exposicion,
        subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_notrel_dg==1& rsh_rec==3|ci_conadi_rec==3|cya_conadi_rec==3)$cond_egr!=2)
), dim = c(2, 2, 2))

epi.2by2(matrix.array_f1)
#              Outcome +    Outcome -      Total                 Inc risk *
# Exposed +          585        33560      34145        1.71 (1.58 to 1.86)
# Exposed -          206         6484       6690        3.08 (2.68 to 3.52)
# Total              791        40044      40835        1.94 (1.81 to 2.08)
# 
# 
# Point estimates and 95% CIs:

# Inc risk ratio (crude)                         0.56 (0.48, 0.65)
# Inc risk ratio (M-H)                           0.56 (0.48, 0.65)
# Inc risk ratio (crude:M-H)                     1.00
# Inc odds ratio (crude)                         0.55 (0.47, 0.64)
# Inc odds ratio (M-H)                           0.55 (0.47, 0.64)
# Inc odds ratio (crude:M-H)                     1.00
# Attrib risk in the exposed (crude) *           -1.37 (-1.80, -0.93)
# Attrib risk in the exposed (M-H) *             -1.37 (-2.31, -0.42)
# Attrib risk (crude:M-H)                        1.00

#  M-H test of homogeneity of IRRs: chi2(1) = 0.017 Pr>chi2 = 0.896
#  M-H test of homogeneity of ORs: chi2(1) = 0.016 Pr>chi2 = 0.899
#  Test that M-H adjusted OR = 1:  chi2(1) = 54.903 Pr>chi2 = <0.001
#  Wald confidence limits
#  M-H: Mantel-Haenszel; CI: confidence interval
#  * Outcomes per 100 population units 

## 5.02 regresiones ---------------------------------------------------


invisible("La exposición parece no n")
broom::tidy(glm(cond_egr~ factor(exposicion)+ edad_anos+ factor(sexo)+ factor(prev_benef), 
                data= subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==0)),exponentiate=T, conf.int=T)
# term                   estimate std.error statistic  p.value conf.low conf.high
# <chr>                     <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
# 1 (Intercept)               2.82  0.00476     217.    0           2.79      2.84 
# 2 factor(exposicion)1       0.982 0.00424      -4.26  2.01e- 5    0.974     0.990
# 3 edad_anos                 1.00  0.0000789     5.82  5.95e- 9    1.00      1.00 
# 4 factor(sexo)2             1.00  0.00193       0.212 8.32e- 1    0.997     1.00 
# 5 factor(prev_benef)1 B     0.985 0.00298      -4.91  9.37e- 7    0.980     0.991
# 6 factor(prev_benef)1 C     0.978 0.00414      -5.40  6.89e- 8    0.970     0.986
# 7 factor(prev_benef)1 D     0.978 0.00350      -6.44  1.24e-10    0.971     0.984
# 8 factor(prev_benef)2 NA    0.969 0.00313      -9.93  3.43e-23    0.964     0.975
# 9 factor(prev_benef)3 NA    0.972 0.00812      -3.56  3.70e- 4    0.956     0.987
# 10 factor(prev_benef)4 NA    0.978 0.00847      -2.65  8.17e- 3    0.962     0.994
# 11 factor(prev_benef)5 NA    0.983 0.0103       -1.68  9.22e- 2    0.963     1.00 
structure(list(term = c("(Intercept)", "factor(exposicion)1", 
"edad_anos", "factor(sexo)2", "factor(prev_benef)1 B", "factor(prev_benef)1 C", 
"factor(prev_benef)1 D", "factor(prev_benef)2 NA", "factor(prev_benef)3 NA", 
"factor(prev_benef)4 NA", "factor(prev_benef)5 NA"), estimate = c(2.81527006788762, 
0.98208628823146, 1.00045913278513, 1.00040903865475, 0.985497547400461, 
0.97792275494407, 0.977718712298278, 0.969444305890407, 0.971507853269455, 
0.97784870761281, 0.982852420425325), std.error = c(0.00476304355020992, 
0.00423948672550333, 7.88799548238949e-05, 0.00193237735728072, 
0.00297817095368198, 0.00413796549016723, 0.00350108812397561, 
0.00312651726719185, 0.00811730805498029, 0.00846855109546445, 
0.0102717047057436), statistic = c(217.310252349684, -4.26374836599754, 
5.81931641506025, 0.21163310556487, -4.90523925763107, -5.39506547225663, 
-6.43607494456931, -9.92550160987324, -3.56102372214372, -2.6451179610114, 
-1.68387842299896), p.value = c(0, 2.01476879391515e-05, 5.95292782475814e-09, 
0.832394336022511, 9.36719084820976e-07, 6.88788411611484e-08, 
1.23958640235393e-10, 3.4259824779217e-23, 0.000369831566323141, 
0.00816937016741516, 0.0922128106695994), conf.low = c(2.78911070705505, 
0.973959705914043, 1.00030447188753, 0.996627265486589, 0.979761848203522, 
0.970023619218485, 0.971032566437046, 0.963521850229217, 0.956173824198957, 
0.961752275113714, 0.963263314394915), conf.high = c(2.8416747800965, 
                   0.990280677604707, 1.00061381759545, 1.00420516203065, 0.991266824395257, 
                   0.985886215232466, 0.984450896313114, 0.975403164961711, 0.987087792070575, 
                   0.994214539151454, 1.00283989424298)), row.names = c(NA, -11L
                                                                                                                                                                                                                                                                                                                             ), class = c("tbl_df", "tbl", "data.frame"))
broom::tidy(glm(cond_egr~ factor(exposicion)+ edad_anos+ factor(sexo)+ factor(prev_benef), 
                data= subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1)),exponentiate=T, conf.int=T)
# term                   estimate std.error statistic   p.value conf.low conf.high
# <chr>                     <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
# 1 (Intercept)               2.76  0.00157     647.    0            2.75      2.77 
# 2 factor(exposicion)1       0.986 0.00137     -10.4   3.99e- 25    0.983     0.989
# 3 edad_anos                 1.00  0.0000295    28.5   8.58e-179    1.00      1.00 
# 4 factor(sexo)2             0.983 0.000758    -22.0   5.68e-107    0.982     0.985
# 5 factor(sexo)3             0.986 0.0734       -0.194 8.46e-  1    0.854     1.14 
# 6 factor(prev_benef)1 B     0.984 0.000999    -15.9   1.28e- 56    0.982     0.986
# 7 factor(prev_benef)1 C     0.976 0.00147     -16.4   1.63e- 60    0.973     0.979
# 8 factor(prev_benef)1 D     0.974 0.00126     -20.7   4.51e- 95    0.972     0.977
# 9 factor(prev_benef)2 NA    0.961 0.00116     -34.7   1.47e-263    0.958     0.963
# 10 factor(prev_benef)3 NA    0.982 0.00690      -2.66  7.79e-  3    0.969     0.995
# 11 factor(prev_benef)4 NA    0.977 0.00576      -4.01  6.10e-  5    0.966     0.988
# 12 factor(prev_benef)5 NA    0.963 0.00606      -6.29  3.21e- 10    0.951     0.974

structure(list(term = c("(Intercept)", "factor(exposicion)1", 
"edad_anos", "factor(sexo)2", "factor(sexo)3", "factor(prev_benef)1 B", 
"factor(prev_benef)1 C", "factor(prev_benef)1 D", "factor(prev_benef)2 NA", 
"factor(prev_benef)3 NA", "factor(prev_benef)4 NA", "factor(prev_benef)5 NA"
), estimate = c(2.75876462668941, 0.985931706859858, 1.00084088883659, 
0.983468627679057, 0.985833402520961, 0.984282846405038, 0.976095061561836, 
0.974236804220041, 0.960611351271349, 0.981795286335952, 0.977162077494307, 
0.962594900331526), std.error = c(0.00156753862916358, 0.00136810442053076, 
2.9450590774586e-05, 0.000758303058251443, 0.0734398960616823, 
0.000998786281778214, 0.00147395638071414, 0.00126100013405793, 
0.00115690023794968, 0.00690370539550509, 0.00576245885124696, 
0.00606218359564643), statistic = c(647.373507498576, -10.3560732538249, 
28.5405306155323, -21.9826890173434, -0.194279981058978, -15.8612287632309, 
-16.4152063706832, -20.6985540304668, -34.7353830931837, -2.66124600634074, 
-4.00918223632815, -6.28859540990701), p.value = c(0, 3.98665709709921e-25, 
8.57807011662724e-179, 5.68418265909242e-107, 0.845956861068774, 
1.27525192262424e-56, 1.63412941016767e-60, 4.51376308246466e-95, 
1.46780066767403e-263, 0.00778583950627479, 6.09519039090359e-05, 
3.21022558243469e-10), conf.low = c(2.75030182786946, 0.983291535993391, 
1.00078311986877, 0.982008036361184, 0.853672666706051, 0.982357913655417, 
0.973279288355416, 0.971831936550906, 0.958435649170516, 0.968600074257957, 
0.966187891065225, 0.951225351869445), conf.high = c(2.76725346591671, 
0.988578966673979, 1.00089866113906, 0.984931391409929, 1.1384545100595, 
0.986211551065103, 0.978918981020679, 0.97664762290631, 0.962791992336657, 
0.995170256423886, 0.988260911281166, 0.974100343649616)), row.names = c(NA, 
-12L), class = c("tbl_df", "tbl", "data.frame"))

invisible(" Relacionados con sustancias y no rel-sustancias, la exposición")
invisible("es protectora .90, o sea, subreportar tiene mejores resultados")

broom::tidy(glm(cond_egr~ factor(exposicion)+ edad_anos+ factor(sexo)+ factor(prev_benef), 
                data= subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==0& a_excluir==0)),exponentiate=T, conf.int=T)
# term                   estimate std.error statistic p.value conf.low conf.high
# <chr>                     <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
# 1 (Intercept)               2.81   0.0160     64.6    0          2.72      2.90 
# 2 factor(exposicion)1       0.987  0.00891    -1.47   0.142      0.970     1.00 
# 3 edad_anos                 1.00   0.000243    1.26   0.210      1.00      1.00 
# 4 factor(sexo)2             0.996  0.00634    -0.555  0.579      0.984     1.01 
# 5 factor(prev_benef)1 B     0.999  0.00800    -0.0855 0.932      0.984     1.02 
# 6 factor(prev_benef)1 C     0.975  0.0127     -1.96   0.0504     0.951     1.00 
# 7 factor(prev_benef)1 D     0.972  0.0108     -2.62   0.00896    0.952     0.993
# 8 factor(prev_benef)2 NA    0.987  0.0116     -1.12   0.262      0.965     1.01 
# 9 factor(prev_benef)3 NA    0.967  0.0293     -1.15   0.252      0.913     1.02 
# 10 factor(prev_benef)4 NA    0.969  0.0277     -1.12   0.262      0.918     1.02 
# 11 factor(prev_benef)5 NA    0.968  0.0713     -0.457  0.648      0.842     1.11 

structure(list(term = c("(Intercept)", "factor(exposicion)1", 
"edad_anos", "factor(sexo)2", "factor(prev_benef)1 B", "factor(prev_benef)1 C", 
"factor(prev_benef)1 D", "factor(prev_benef)2 NA", "factor(prev_benef)3 NA", 
"factor(prev_benef)4 NA", "factor(prev_benef)5 NA"), estimate = c(2.80849397090103, 
0.986996968879373, 1.00030550769257, 0.996487577139821, 0.999316788409357, 
0.975408650004707, 0.972045362524338, 0.987065700715373, 0.967020753616259, 
0.969399432055453, 0.967923618782938), std.error = c(0.0159799405353473, 
0.00890790690233832, 0.000243353360899142, 0.00633612241318836, 
0.00799533992520415, 0.0127196072454935, 0.0108403487332512, 
0.0116083425516108, 0.0292878750576331, 0.0276984367334833, 0.0713449960633506
), statistic = c(64.6215412360944, -1.4692913544187, 1.25521600964208, 
-0.555324798109209, -0.0854804289039505, -1.95751072367814, -2.61548839844826, 
-1.12149306689203, -1.1450240700493, -1.12203232775594, -0.456964087654357
), p.value = c(0, 0.141876222949833, 0.209514520042448, 0.578720871082122, 
0.931886114944529, 0.05039557620289, 0.00896212699726436, 0.262182856802633, 
0.252305847015792, 0.261953553823054, 0.647735494121435), conf.low = c(2.72189486233527, 
0.969914372355139, 0.999828511917484, 0.984189147206591, 0.983778977635871, 
0.95139234630441, 0.951610468713246, 0.964861598655713, 0.91307386120798, 
0.918175729813838, 0.841612382722382), conf.high = c(2.89784829448581, 
1.00438043227631, 1.00078273103166, 1.00893968828286, 1.01510000345466, 
1.0000312050015, 0.992919075472882, 1.00978077984051, 1.0241549754665, 
1.02348083090801, 1.11319195276966)), row.names = c(NA, -11L), class = c("tbl_df", 
          "tbl", "data.frame"))

broom::tidy(glm(cond_egr~ factor(exposicion)+ edad_anos+ factor(sexo)+ factor(prev_benef), 
                data= subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, SUD_rel_dg==1& a_excluir==0)),exponentiate=T, conf.int=T)
# term                   estimate std.error statistic  p.value conf.low conf.high
# <chr>                     <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
# 1 (Intercept)               2.75  0.00445    227.     0           2.72      2.77 
# 2 factor(exposicion)1       0.993 0.00261     -2.85   4.41e- 3    0.988     0.998
# 3 edad_anos                 1.00  0.0000745    8.00   1.33e-15    1.00      1.00 
# 4 factor(sexo)2             0.991 0.00198     -4.50   6.69e- 6    0.987     0.995
# 5 factor(sexo)3             0.999 0.0952      -0.0153 9.88e- 1    0.829     1.20 
# 6 factor(prev_benef)1 B     0.988 0.00232     -5.16   2.49e- 7    0.984     0.993
# 7 factor(prev_benef)1 C     0.985 0.00355     -4.35   1.38e- 5    0.978     0.992
# 8 factor(prev_benef)1 D     0.984 0.00324     -4.90   9.44e- 7    0.978     0.990
# 9 factor(prev_benef)2 NA    0.978 0.00451     -4.83   1.37e- 6    0.970     0.987
# 10 factor(prev_benef)3 NA    0.971 0.0197      -1.49   1.35e- 1    0.934     1.01 
# 11 factor(prev_benef)4 NA    1.02  0.0175       1.24   2.14e- 1    0.988     1.06 
# 12 factor(prev_benef)5 NA    0.972 0.0193      -1.49   1.36e- 1    0.936     1.01 
structure(list(term = c("(Intercept)", "factor(exposicion)1", 
"edad_anos", "factor(sexo)2", "factor(sexo)3", "factor(prev_benef)1 B", 
"factor(prev_benef)1 C", "factor(prev_benef)1 D", "factor(prev_benef)2 NA", 
"factor(prev_benef)3 NA", "factor(prev_benef)4 NA", "factor(prev_benef)5 NA"
), estimate = c(2.74759754091199, 0.992604070491227, 1.00059578799426, 
0.991129403749082, 0.998547401961746, 0.988096198380086, 0.984678806293527, 
0.984223600504649, 0.978449403854066, 0.970971804117021, 1.02192764415949, 
0.97162434333143), std.error = c(0.00444920839374321, 0.00260681000204267, 
7.44658598305756e-05, 0.00197797227364242, 0.0952019593415404, 
0.00232062894076905, 0.00355137842059832, 0.00324226420600825, 
0.00450931699599832, 0.0197061894214852, 0.017461780846965, 0.0193111654760511
), statistic = c(227.170053392787, -2.84770082743421, 7.99843826972939, 
-4.50470127203138, -0.0152691613873629, -5.16033346919934, -4.34754457000559, 
-4.90465009898237, -4.83137498811239, -1.49485263065805, 1.24218091997074, 
-1.49064163493253), p.value = c(0, 0.00440830580142363, 1.33110329198153e-15, 
6.6858238571419e-06, 0.987817603954685, 2.48965468376431e-07, 
1.38378567015287e-05, 9.43596483430416e-07, 1.36639887806404e-06, 
0.134969345936137, 0.214184993770839, 0.136072038845382), conf.low = c(2.72374186382743, 
0.987545538119878, 1.000449761292, 0.987294476791551, 0.828576537437864, 
0.983612197012733, 0.977848675185397, 0.977988964717356, 0.969839873231098, 
0.93418454871999, 0.987544449992589, 0.935536407709524), conf.high = c(2.77166215605088, 
0.997688514325651, 1.00074183601073, 0.994979226631914, 1.20338540727666, 
0.992600641002963, 0.991556644876383, 0.990497981815463, 0.987135363606825, 
1.00920770492518, 1.05750795309032, 1.00910435636125)), row.names = c(NA, 
                                           -12L), class = c("tbl_df", "tbl", "data.frame"))

broom::glance(glm(cond_egr~ factor(exposicion)*factor(SUD_rel_dg)+ edad_anos+ factor(sexo)+ factor(prev_benef),
    data= subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0)),exponentiate=T, conf.int=T)
#     null.deviance df.null logLik     AIC     BIC deviance df.residual  nobs
#           <dbl>   <int>  <dbl>   <dbl>   <dbl>    <dbl>       <int> <int>
# 1          416.   21786 12292. -24555. -24435.     413.       21773 21787
structure(list(null.deviance = 415.748473860559, df.null = 21786L, 
               logLik = 12292.2826732535, AIC = -24554.565346507, BIC = -24434.7293153976, 
               deviance = 412.727024437966, df.residual = 21773L, nobs = 21787L), class = c("tbl_df", 
                                                                                            "tbl", "data.frame"), row.names = c(NA, -1L))

broom::glance(glm(cond_egr~ factor(exposicion)+ factor(SUD_rel_dg)+ edad_anos+ factor(sexo)+ factor(prev_benef), 
                  data= subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0)),exponentiate=T, conf.int=T)
# null.deviance df.null logLik  AIC     BIC deviance df.residual  nobs
# <dbl>         <int>   <dbl>   <dbl>   <dbl>    <dbl>       <int> <int>
#  416.   21786 12292. -24556. -24444.     413.       21774 21787
structure(list(null.deviance = 415.748473860559, df.null = 21786L, 
               logLik = 12292.048165504, AIC = -24556.0963310079, BIC = -24444.2493686392, 
               deviance = 412.735909436126, df.residual = 21774L, nobs = 21787L), class = c("tbl_df", 
                                                                                            "tbl", "data.frame"), row.names = c(NA, -1L))

broom::glance(glm(cond_egr~ factor(exposicion)+ edad_anos+ factor(sexo)+ factor(prev_benef), 
                  data= subset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, a_excluir==0)),exponentiate=T, conf.int=T)
# null.deviance df.null logLik     AIC     BIC deviance df.residual  nobs
# <dbl>   <int>  <dbl>   <dbl>   <dbl>    <dbl>       <int> <int>
#   416.   21786 12288. -24549. -24445.     413.       21775 21787
structure(list(null.deviance = 415.748473860559, df.null = 21786L, 
logLik = 12287.5986820934, AIC = -24549.1973641869, BIC = -24445.3394705587, 
deviance = 412.90452711522, df.residual = 21775L, nobs = 21787L), class = c("tbl_df", 
"tbl", "data.frame"), row.names = c(NA, -1L))


invisible("Mejor ajuste un modelo sin ajustar por tipo de causas")

## 5.1 naniar ---------------------------------------------------

#gemini("ejemplos de usar el paquete naniar en una base de datos de nombre 'dtX2023_12_05_DatosEgresosHosp_encrip_filt5'")
require(naniar)

gg_miss_upset1<-
gg_miss_upset(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
              nsets = 5,
              nintersects = 50)
diags_cie_10<-
dplyr::bind_rows(
cbind.data.frame(dg="diag1", table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$diag1, exclude=NULL)),
cbind.data.frame(dg="diag2", table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$diag2, exclude=NULL)),
cbind.data.frame(dg="diag3", table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$diag3, exclude=NULL)),
cbind.data.frame(dg="diag4", table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5$diag4, exclude=NULL)))


diags_cie_10def<-
diags_cie_10 %>% 
  group_by(dg) %>% 
  dplyr::mutate(perc=scales::percent(Freq/sum(Freq), accuracy=0.01)) %>% 
  slice_max(order_by=Freq, n=15) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(dg_clas, by=c("Var1"="titulo")) %>% 
  dplyr::mutate(rel_SUD= dplyr::case_when(grepl('F10|K70|K73|K74|I1[0-5]|Y1[0-4]|F1[1-6]|F18|F19|B2[0-4]',Var1)~1,
                                          grepl('C16|C56|C61|C81|C8[2-5]|C96|F0[1-3]|G30|G31',Var1)~0,
                                          T~0))

# 
# diags_cie_10 %>% 
#   group_by(dg) %>% 
#   dplyr::mutate(perc=scales::percent(Freq/sum(Freq), accuracy=0.01)) %>% 
#   slice_max(order_by=Freq, n=15) %>% 
#   dplyr::ungroup() %>% 
#   ggplot()+
#   geom_bar(aes(x=Var1, y=Freq))+
#   facet_wrap(~dg)

dg_clas<-
  cbind.data.frame(titulo = c("B24X", "C169", "C56X", "C61X", "C859", 
    "E039", "E119", "E149", "E669", "E785", "F101", "F102", "F103", 
    "F121", "F171", "F172", "F191", "F192", "I10X", "I219", "J128", 
    "J459", "J960", "K701", "K703", "K709", "K746", "N185", "R730", 
    "U071", "W010", "W019", "W179", "W189", "W199", "X590", "X599", 
    "X999", "Y119", "Y149", "Y831", "Y832", "Y838", "Y841", "Z511", 
    "Z922"), explicacion = c("Enfermedad por virus de la inmunodeficiencia humana [VIH], no especificada.", 
     "Cáncer de estómago, localización no especificada.", "Cáncer de ovario.", 
     "Cáncer de próstata.", "Linfoma no Hodgkin de tipo no especificado.", 
     "Hipotiroidismo, no especificado.", "Diabetes mellitus no insulinodependiente, sin mención de complicación.", 
     "Diabetes mellitus, no especificada.", "Obesidad, no especificada.", 
     "Hiperlipidemia, no especificada.", "Trastornos mentales y del comportamiento debido al uso de alcohol, uso perjudicial.", 
     "Trastornos mentales y del comportamiento debido al uso de alcohol, síndrome de dependencia.", 
     "Trastornos mentales y del comportamiento debido al uso de alcohol, síndrome de abstinencia.", 
     "Trastornos mentales y del comportamiento debido al uso de cannabinoides, síndrome de dependencia.", 
     "Trastornos mentales y del comportamiento debido al uso de tabaco, uso perjudicial.", 
     "Trastornos mentales y del comportamiento debido al uso de tabaco, síndrome de dependencia.", 
     "Trastornos mentales y del comportamiento debido al uso de cannabinoides, uso perjudicial.", 
     "Trastornos mentales y del comportamiento debido al uso de cannabinoides, trastorno psicótico.", 
     "Hipertensión esencial (primaria).", "Infarto agudo de miocardio, no especificado.", 
     "Neumonía viral, otros tipos.", "Asma, no especificada.", "Insuficiencia respiratoria aguda.", 
     "Enfermedad hepática alcohólica con coma hepático.", "Cirrosis hepática alcohólica.", 
     "Enfermedad hepática alcohólica, no especificada.", "Fibrosis y cirrosis del hígado.", 
     "Enfermedad renal crónica, etapa 5.", "Nivel elevado de glucosa en sangre.", 
     "COVID-19, virus identificado.", "Caída al mismo nivel por resbalón, tropezón y caída.", 
     "Caída que involucra patines de hielo, esquís, patines de ruedas o monopatines, no especificada.", 
     "Caída de altura no especificada, no especificada.", "Caída no especificada, nivel sin especificar.", 
     "Caída, no especificada.", "Exposición a factor no especificado que causa fractura.", 
     "Exposición accidental a otros factores no especificados y a los no especificados.", 
     "Contacto con animal venenoso no especificado.", "Envenenamiento y exposición a otros medicamentos que actúan sobre el sistema nervioso autónomo, no especificados.", 
     "Envenenamiento accidental por, y exposición a sustancias nocivas no especificadas.", 
     "Complicaciones de procedimientos quirúrgicos y médicos no clasificados en otra parte, no especificadas.", 
     "Otros procedimientos quirúrgicos y médicos como causa de reacción anormal del paciente o de complicación tardía, sin mención de accidente al momento del procedimiento.", 
     "Reacción anormal en paciente o complicación tardía sin mención de accidente en el momento de la intervención médica.", 
     "Otros procedimientos médicos como causa de reacción anormal del paciente o de complicación tardía, sin mención de accidente al momento del procedimiento.", 
     "Encuentro para procedimientos de quimioterapia.", "Antecedentes personales de uso a largo plazo (actual) de anticoagulantes."
))
  

png("_figs/missfig1.png", width = 2200, height = 1600, units = "px", res= 300)#
gg_miss_upset1
dev.off()

#ggsave("_figs/missfig1b.png",gg_miss_upset1, height=10, width=10, dpi=600)
# Error in UseMethod("grid.draw") : 
#   no applicable method for 'grid.draw' applied to an object of class "upset"


missfig2<-
gg_miss_fct(x = dtX2023_12_05_DatosEgresosHosp_encrip_filt5, fct = fecha_ingreso_rec_cut)+ labs(x = "Fecha de ingreso (discreta)")

ggsave("_figs/missfig2.png",missfig2, height=6, width=8, dpi=600)

## 5.2 glca ---------------------------------------------------

invisible("Demasiado pesado")
no__correr=1

if(no__correr==0){

clus_iter= 500#500 #500
n_thread <- parallel::detectCores()
nrep <- clus_iter # number of different initial values (could be n_thread too)
n_class_max <- 10 # maximum number of classes to investigate
n_bootstrap <- 100#00 #30 # 50 number of bootstrap samples
print(n_thread)

seed<-2125
old <- Sys.time()
library(glca)
f_preds <- item(edad_anos, sexo, prev_benef, pueblo_originario, ci_conadi_rec, cya_conadi_rec, cond_egr, SUD_rel_dg) ~ 1

lca2 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 2, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca3 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 3, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca4 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 4, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca5 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 5, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca6 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 6, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca7 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 7, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca8 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 8, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca9 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 9, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)
lca10 <- glca(f_preds, data = dtX2023_12_05_DatosEgresosHosp_encrip_filt3, nclass = 10, seed = seed, verbose = FALSE, n.init = 5e2, decreasing=T, maxiter = 1e4,testiter = 500)

new_med<-(Sys.time())
paste0("The model took ",round(new_med-old,2)," until every LCA was computed")

gof<-
  gofglca(lca2, lca3, lca4, lca5, lca6, lca7, lca8, lca9, lca10, test = "chisq")

bootlrt<-
  gofglca(lca2, lca3, lca4, lca5, lca6, lca7, lca8, lca9, lca10, test = "boot", nboot=n_bootstrap, seed=2125)
}

# 6. Tasas ---------------------------------------------------

dtX2023_12_05_DatosEgresosHosp_encrip<-rio::import("20231205_hosp.parquet.gzip")
invisible("si quisiera cargar las bases anteriores sin tener que cargar todo")
#dtX2023_12_05_DatosEgresosHosp_encrip_filt<-rio::import("20231205_hosp_filt.parquet.gzip")
#dtX2023_12_05_DatosEgresosHosp_encrip_filt5<-rio::import("20231205_hosp_parquet_filt5.gz.parquet")


#15-29 years of age 30-44 years of age 45-59 years of age 60-65 years of age
#(15-29, 30-44, 45-59 and 60 and over).
#se obtuvo la proporción de hombres y mujeres
#en cada categoría de edad (15-29; 30-44; 45-59 y; 60 y más) y categoría de consumo,
dtX2023_12_05_DatosEgresosHosp_encrip<-
  dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad_anos>=15 & edad_anos<=29~1,
                                               edad_anos>=30 & edad_anos<=44~2,
                                               edad_anos>=45 & edad_anos<=59~3,
                                               edad_anos>=60 & edad_anos<=65~4,T~NA_real_)) 
dtX2023_12_05_DatosEgresosHosp_encrip$ing_year <-
  lubridate::epiyear(as.Date(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec))

dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  group_by(SUD_rel_dg, ing_year, edad_anos_rec, sexo, cond_egr) %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022, !is.na(edad_anos_rec), sexo<3) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) #%>% dput()
source("_tasas/tasas_dtX2023_12_05_DatosEgresosHosp_encrip.R")
#tasas_dtX2023_12_05_DatosEgresosHosp_encrip

invisible("ahora sin años de ingreso, para evitar escasez; igual dejamos años entre 2010-2022")
dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  group_by(SUD_rel_dg, edad_anos_rec, sexo, cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo<3) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) #%>%  dput()
source("_tasas/tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip.R")
#tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip

dtX2023_12_05_DatosEgresosHosp_encrip_filt<-
  dtX2023_12_05_DatosEgresosHosp_encrip_filt %>% 
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad_anos>=15 & edad_anos<=29~1,
                                                edad_anos>=30 & edad_anos<=44~2,
                                                edad_anos>=45 & edad_anos<=59~3,
                                                edad_anos>=60 & edad_anos<=65~4,T~NA_real_)) 
dtX2023_12_05_DatosEgresosHosp_encrip_filt$ing_year <-
  lubridate::epiyear(as.Date(dtX2023_12_05_DatosEgresosHosp_encrip_filt$fecha_ingreso_rec))

dtX2023_12_05_DatosEgresosHosp_encrip_filt %>% 
  group_by(SUD_rel_dg, ing_year, edad_anos_rec, sexo, cond_egr) %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022, !is.na(edad_anos_rec), sexo<3) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) # %>% dput()
source("_tasas/tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt.R")
#tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt

invisible("No hay casos sin tasas")
tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt %>% 
  dplyr::filter(`2`==0) %>% nrow()


invisible("ahora sin años de ingreso, para evitar escasez")
dtX2023_12_05_DatosEgresosHosp_encrip_filt %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  group_by(SUD_rel_dg, edad_anos_rec, sexo, cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo<3) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) # %>%  dput()
source("_tasas/tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip_filt.R")
#tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip_filt


## 6.2 contraste base---------------------------------------------------

#tasas_dtX2023_12_05_DatosEgresosHosp_encrip
#tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt

dtX2023_12_05_DatosEgresosHosp_encrip_filt5<-
  dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::mutate(edad_anos_rec= dplyr::case_when(edad_anos>=15 & edad_anos<=29~1,
                                                edad_anos>=30 & edad_anos<=44~2,
                                                edad_anos>=45 & edad_anos<=59~3,
                                                edad_anos>=60 & edad_anos<=65~4,T~NA_real_)) 
invisible("genera inestabilidad?= 0 está bien (no hay estratos sin casos)")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5%>% 
  dplyr::mutate(ing_year= lubridate::epiyear(as.Date(fecha_ingreso_rec_num, origin="1970-01-01"))) %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  dplyr::group_by(exposicion, SUD_rel_dg, edad_anos_rec, sexo, cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo %in% c(1,2)) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(perc= `2`/(`1`+`2`)) %>% 
  dplyr::filter(`2`==0) %>% nrow()

dtX2023_12_05_DatosEgresosHosp_encrip_filt5%>% 
  dplyr::mutate(ing_year= lubridate::epiyear(as.Date(fecha_ingreso_rec_num, origin="1970-01-01"))) %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  dplyr::group_by(exposicion, SUD_rel_dg, edad_anos_rec, sexo, cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo %in% c(1,2)) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) 
source("_tasas/tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5.R")

cat("RMEs")

#https://www3.paho.org/Spanish/SHA/be_v23n3-estandariz.htm#:~:text=M%C3%A9todo%20directo,-En%20el%20m%C3%A9todo&text=La%20tasa%20ajustada%20o%20%E2%80%9Cestandarizada,selecci%C3%B3n%20de%20la%20poblaci%C3%B3n%20est%C3%A1ndar.
# Vandenbroucke JP. A shortcut method for calculating the 95 percent confidence interval of the 
#standardized mortality ratio. (Letter). Am J Epidemiol 1982; 115:303-4

invisible("TOtal, según exposición")

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  #2023-01-05 corregí el cálculo del expected death
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=(`2`/(`1`+`2`))*tot) %>% 
  #2023-01-04 Expected Deaths in Stratum = (Number of Individuals in Stratum of Study Population) × (Age-Specific Death Rate in Standard Population).
  #dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=perc.y*tot) %>% 
  # dplyr::rename("observed_deaths"="X2","obs_rates"="perc.x","total_observed_sample"="tot","%_stratum_population"="perc.y", "expected deaths"="exp_x2") %>%
  # dplyr::select(exposicion, SUD_rel_dg, edad_anos_rec, sexo, total_observed_sample, observed_deaths, obs_rates, "%_stratum_population", "expected deaths")
  dplyr::summarise(obs_sum=sum(X2),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
# obs_sum  exp_sum      RME         EE     lo_95    up_95  lo2_95   up2_95                 print                print2
# 1    6760 6602.848 1.023801 0.01245208 0.9993945 1.048207 0.99954 1.048352 1.02 IC 95% 1.00;1.05 1.02 IC 95% 1.00;1.05

#1.02 IC 95% 1.00;1.05
#1.02 IC 95% 1.00;1.05

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot) %>% 
  dplyr::summarise(obs_sum=sum(X2),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#     obs_sum  exp_sum       RME        EE     lo_95     up_95    lo2_95    up2_95                 print                print2
# 1     314 513.8344 0.6110918 0.0344859 0.5434994 0.6786842 0.5453685 0.6805532 0.61 IC 95% 0.54;0.68 0.61 IC 95% 0.55;0.68

#0.61 IC 95% 0.54;0.68
#0.61 IC 95% 0.55;0.68

invisible("Por otras causas, según exposición")

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot) %>% 
  dplyr::summarise(obs_sum=sum(X2),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#     obs_sum  exp_sum      RME         EE    lo_95    up_95   lo2_95   up2_95                 print                print2
# 1    1517 723.0496 2.098058 0.05386723 1.992478 2.203638 1.993806 2.204966 2.10 IC 95% 1.99;2.20 2.10 IC 95% 1.99;2.20

#2.10 IC 95% 1.99;2.20
#2.10 IC 95% 1.99;2.20

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot) %>% 
  dplyr::summarise(obs_sum=sum(X2),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#   obs_sum  exp_sum     RME        EE   lo_95    up_95  lo2_95   up2_95                 print                print2
# 1      51 34.32447 1.48582 0.2080565 1.07803 1.893611 1.10601 1.921591 1.49 IC 95% 1.08;1.89 1.49 IC 95% 1.11;1.92

#1.49 IC 95% 1.08;1.89
#1.49 IC 95% 1.11;1.92

invisible("Relacionados con el consumo de alcohol u otras sustancias, según exposición")

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot) %>% 
  dplyr::summarise(obs_sum=sum(X2),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#     obs_sum  exp_sum       RME        EE     lo_95     up_95    lo2_95    up2_95                 print                print2
# 1    5243 5879.798 0.8916972 0.0123148 0.8675602 0.9158342 0.8677236 0.9159976 0.89 IC 95% 0.87;0.92 0.89 IC 95% 0.87;0.92

#0.89 IC 95% 0.87;0.92
#0.89 IC 95% 0.87;0.92

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo")) %>%
  dplyr::mutate(tot=(`X1`+`X2`), w=tot/sum(tot), exp_x2=rate.y*tot) %>% 
  dplyr::summarise(obs_sum=sum(X2),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#   obs_sum exp_sum       RME         EE     lo_95     up_95    lo2_95    up2_95                 print                print2
# 1     263  479.51 0.5484766 0.03382052 0.4821884 0.6147648 0.4841913 0.6167677 0.55 IC 95% 0.48;0.61 0.55 IC 95% 0.48;0.62

#0.55 IC 95% 0.48;0.61
#0.55 IC 95% 0.48;0.62

# 1) Se calcula el error estándar (EE) de la RME:
#   EE = RME/(raíz cuadrada de las defunciones observadas)
# 2) El intervalo de confianza (IC) de 95% se calcula de la siguiente manera:
# IC (95%) : RME +/- (1,96 x EE), donde 1,96 es el valor de la distribución Z con 
# un nivel de confianza de 95%, que es el valor con el que se contrasta. Se 
# asume que los valores siguen una distribución normal

## 6.3 prev, contraste base  ---------------------------------------------------

invisible("#prev, generamos filt5")
invisible("genera inestabilidad?= 0 está bien (no hay estratos sin casos)")
dtX2023_12_05_DatosEgresosHosp_encrip_filt5%>% 
  dplyr::mutate(ing_year= lubridate::epiyear(as.Date(fecha_ingreso_rec_num, origin="1970-01-01"))) %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  dplyr::group_by(exposicion, SUD_rel_dg, edad_anos_rec, sexo, prev_benef_rec,cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo %in% c(1,2)) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(perc= `2`/(`1`+`2`)) %>% 
  dplyr::filter(`1`==0 & `2`==0) %>% nrow()
invisible("2023-01-06: Lo pensé mejor y la verdad es que necesito q haya submuestra, no que haya ")

dtX2023_12_05_DatosEgresosHosp_encrip_filt5%>% 
  dplyr::mutate(ing_year= lubridate::epiyear(as.Date(fecha_ingreso_rec_num, origin="1970-01-01"))) %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  dplyr::group_by(exposicion, SUD_rel_dg, edad_anos_rec, sexo, prev_benef_rec,cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo %in% c(1,2)) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) 
source("_tasas/tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5.R")
#tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5

dtX2023_12_05_DatosEgresosHosp_encrip$prev_benef<-
  glue::glue("{factor(dtX2023_12_05_DatosEgresosHosp_encrip$previ)} {factor(dtX2023_12_05_DatosEgresosHosp_encrip$benef)}")

dtX2023_12_05_DatosEgresosHosp_encrip$prev_benef_rec <-
  car::recode(dtX2023_12_05_DatosEgresosHosp_encrip$prev_benef,
#"'99 NA'=NA;'96 NA'=NA;'1 A'='FONASA AB';'1 B'='FONASA AB';'1 C'='FONASA CD';'1 D'='FONASA CD';'2 NA'='ISAPRE & FFAA';'3 NA'='ISAPRE & FFAA';'4 NA'='ISAPRE & FFAA';'5 NA'='ISAPRE & FFAA'")
"'99 NA'=NA;'96 NA'=NA;'1 NA'='FONASA AB';'1 A'='FONASA AB';'1 B'='FONASA AB';'1 C'='FONASA CD';'1 D'='FONASA CD';'2 NA'='ISAPRE';'3 NA'='FFAA';'4 NA'='FFAA';'5 NA'='FFAA'")

table(dtX2023_12_05_DatosEgresosHosp_encrip$prev_benef,
      dtX2023_12_05_DatosEgresosHosp_encrip$prev_benef_rec, exclude=NULL)
table(dtX2023_12_05_DatosEgresosHosp_encrip$glosa_prevision,
      dtX2023_12_05_DatosEgresosHosp_encrip$prev_benef_rec, exclude=NULL)

dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  group_by(SUD_rel_dg, ing_year, edad_anos_rec, sexo, prev_benef_rec, cond_egr) %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022, !is.na(edad_anos_rec), sexo<3) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) #%>% dput()
source("_tasas/tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip.R")
#tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip

paste0("Submuestra")
dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  group_by(SUD_rel_dg, edad_anos_rec, sexo, prev_benef_rec, cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo<3,!is.na(prev_benef_rec)) %>% nrow()

invisible("prev, ahora sin años de ingreso, para evitar escasez; igual dejamos años entre 2010-2022")
dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  dplyr::filter(ing_year>=2010,ing_year<=2022) %>% 
  group_by(SUD_rel_dg, edad_anos_rec, sexo, prev_benef_rec, cond_egr) %>% 
  dplyr::filter(!is.na(edad_anos_rec), sexo<3,!is.na(prev_benef_rec)) %>% # %>% nrow() 
  #12.857.906
  dplyr::summarise(n=n()) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from=cond_egr, values_from=n, values_fill=0) %>%
  dplyr::mutate(rate= `2`/(`1`+`2`)) #%>%   dput()
source("_tasas/tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip.R")
#tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip

invisible("Prev, Total")
invisible("Prev,TOtal, según exposición")

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  #2023-01-05 corregí el cálculo del expected death
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=(`X2.y`/(`X1.y`+`X2.y`))*tot) %>% 
  #   exposicion SUD_rel_dg edad_anos_rec sexo    X1   X2      rate.x       1     2       rate.y   tot           w      exp_x2
  dplyr::summarise(obs_sum=sum(X2.x),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#   obs_sum  exp_sum      RME         EE    lo_95    up_95   lo2_95   up2_95                 print                print2
# 1    6760 6341.564 1.065983 0.01296513 1.040571 1.091395 1.040723 1.091546 1.07 IC 95% 1.04;1.09 1.07 IC 95% 1.04;1.09
# 1.07 IC 95% 1.04;1.09 
# 1.07 IC 95% 1.04;1.09


tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  #2023-01-05 corregí el cálculo del expected death
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=(`X2.y`/(`X1.y`+`X2.y`))*tot) %>% 
  #   exposicion SUD_rel_dg edad_anos_rec sexo    X1   X2      rate.x       1     2       rate.y   tot           w      exp_x2
  dplyr::summarise(obs_sum=sum(X2.x),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#1     314 560.1085 0.5606057 0.03163681 0.4985975 0.6226138 0.5003122 0.6243285 0.56 IC 95% 0.50;0.62 0.56 IC 95% 0.50;0.62
# 0.56 IC 95% 0.50;0.62 
# 0.56 IC 95% 0.50;0.62

invisible("Prev, Por otras causas, según exposición")

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=(`X2.y`/(`X1.y`+`X2.y`))*tot) %>% 
  dplyr::summarise(obs_sum=sum(`X2.x`),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
# obs_sum  exp_sum      RME         EE    lo_95    up_95   lo2_95   up2_95                 print                print2
# 1    1517 728.4251 2.082575 0.05346971 1.977774 2.187376 1.979093 2.188694 2.08 IC 95% 1.98;2.19 2.08 IC 95% 1.98;2.19

#2.08 IC 95% 1.98;2.19 
#2.08 IC 95% 1.98;2.19

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==0) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=(`X2.y`/(`X1.y`+`X2.y`))*tot) %>% 
  dplyr::summarise(obs_sum=sum(`X2.x`),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#   obs_sum  exp_sum      RME        EE     lo_95    up_95    lo2_95   up2_95                 print                print2
# 1      51 40.92337 1.246232 0.1745073 0.9041972 1.588266 0.9276655 1.611734 1.25 IC 95% 0.90;1.59 1.25 IC 95% 0.93;1.61

#1.25 IC 95% 0.90;1.59 
#1.25 IC 95% 0.93;1.61


invisible("Prev, Rel. con sustancias, según exposición")

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==0, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=(`X2.y`/(`X1.y`+`X2.y`))*tot) %>% 
  dplyr::summarise(obs_sum=sum(`X2.x`),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#   obs_sum  exp_sum       RME         EE     lo_95     up_95    lo2_95    up2_95                 print                print2
# 1    5243 5613.139 0.9340584 0.01289983 0.9087748 0.9593421 0.9089459 0.9595132 0.93 IC 95% 0.91;0.96 0.93 IC 95% 0.91;0.96
 
#0.93 IC 95% 0.91;0.96
#0.93 IC 95% 0.91;0.96

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=rate.y*tot)

tasas_prev_dtX2023_12_05_DatosEgresosHosp_encrip_filt5 %>% 
  dplyr::filter(exposicion==1, SUD_rel_dg==1) %>% 
  dplyr::mutate(sexo=as.numeric(sexo)) %>% 
  dplyr::left_join(tasas_simp_prev_dtX2023_12_05_DatosEgresosHosp_encrip, by=c("SUD_rel_dg", "edad_anos_rec", "sexo", "prev_benef_rec")) %>%
  dplyr::mutate(tot=(`X1.x`+`X2.x`), w=tot/sum(tot), exp_x2=(`X2.y`/(`X1.y`+`X2.y`))*tot) %>% 
  dplyr::summarise(obs_sum=sum(`X2.x`),exp_sum=sum(exp_x2),
                   RME=obs_sum/exp_sum, EE=RME/sqrt(obs_sum),
                   lo_95= RME - (1.96 * EE),
                   up_95= RME + (1.96 * EE),
                   lo2_95= ((sqrt(obs_sum) - 1.96*0.5)^2)/exp_sum,
                   up2_95= ((sqrt(obs_sum) + 1.96*0.5)^2)/exp_sum) %>% 
  dplyr::mutate(print=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo_95)};{sprintf("%.2f", up_95)}')) %>% 
  dplyr::mutate(print2=glue::glue('{sprintf("%.2f", RME)} IC 95% {sprintf("%.2f", lo2_95)};{sprintf("%.2f", up2_95)}'))
#   obs_sum  exp_sum       RME         EE     lo_95     up_95    lo2_95    up2_95                 print                print2
# 1     263 519.1851 0.5065631 0.03123602 0.4453405 0.5677856 0.4471903 0.5696355 0.51 IC 95% 0.45;0.57 0.51 IC 95% 0.45;0.57

#0.51 IC 95% 0.45;0.57 
#0.51 IC 95% 0.45;0.57

# 6.4. otro ---------------------------------------------------


#https://stats.stackexchange.com/questions/259502/in-using-the-cbind-function-in-r-for-a-logistic-regression-on-a-2-times-2-t


# 99. diagrama de flujo ---------------------------------------------------


tab1_lab<-    paste0('Base de datos \n(n = ', 
                  formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip), 
                          format='f', big.mark='.', digits=0), 
                  ';\npacientes: ',
                  formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip$run)), 
                          format='f', big.mark='.', digits=0),')')

tab2a1_lab<-  nrow(data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip)[!row_num_index %in% 
        data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt)[, `__index_level_0__`]])
  
tab2a2_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip) %>% 
  dplyr::filter(!row_num_index%in%dtX2023_12_05_DatosEgresosHosp_encrip_filt$`__index_level_0__`) %>% 
  dplyr::distinct(run) %>% 
  nrow()
tab2a3_lab<-  scales::percent(tab2a1_lab/nrow(dtX2023_12_05_DatosEgresosHosp_encrip))
tab2a_lab<-   paste0("n= ",formatC(tab2a1_lab, format='f', big.mark='.', digits=0) ,
                  "; p= ", formatC(tab2a2_lab, format='f', big.mark='.', digits=0),"; ",tab2a3_lab) 

tab4a1_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip) %>% 
  dplyr::filter(!row_num_index%in%data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt2)$row_num_index) %>% 
  nrow()
tab4a2_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip) %>% 
  dplyr::filter(!row_num_index%in%data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt2)$row_num_index) %>% 
  dplyr::distinct(run) %>% 
  nrow()
tab4a3_lab<-  scales::percent(tab4a1_lab/nrow(data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt)))
tab4a_lab<-   paste0("n= ",formatC(tab4a1_lab, format='f', big.mark='.', digits=0) ,
                     "; p= ", formatC(tab4a2_lab, format='f', big.mark='.', digits=0),"; ",tab4a3_lab)
tab4b1_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt2) %>% 
  dplyr::filter(!row_num_index%in%dtX2023_12_05_DatosEgresosHosp_encrip_filt3$row_num_index) %>% 
  nrow()
tab4b2_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt2) %>% 
  dplyr::filter(!row_num_index%in%dtX2023_12_05_DatosEgresosHosp_encrip_filt3$row_num_index) %>% 
  dplyr::distinct(run) %>% 
  nrow()
tab4b3_lab<-  scales::percent(tab4b1_lab/nrow(dtX2023_12_05_DatosEgresosHosp_encrip_filt2))
tab4b_lab<-   paste0("n= ",formatC(tab4b1_lab, format='f', big.mark='.', digits=0) ,
                     "; p= ", formatC(tab4b2_lab, format='f', big.mark='.', digits=0),"; ",tab4b3_lab)
tab4c1_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt3) %>% 
  dplyr::filter(!row_num_index%in%dtX2023_12_05_DatosEgresosHosp_encrip_filt4$row_num_index) %>% 
  nrow()
tab4c2_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt3) %>% 
  dplyr::filter(!row_num_index%in%dtX2023_12_05_DatosEgresosHosp_encrip_filt4$row_num_index) %>% 
  dplyr::distinct(run) %>% 
  nrow()
tab4c3_lab<-  scales::percent(tab4c1_lab/nrow(dtX2023_12_05_DatosEgresosHosp_encrip_filt3))
tab4c_lab<-   paste0("n= ",formatC(tab4c1_lab, format='f', big.mark='.', digits=0) ,
                     "; p= ", formatC(tab4c2_lab, format='f', big.mark='.', digits=0),"; ",tab4c3_lab)

tab4d1_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt4) %>% 
  dplyr::filter(!row_num_index%in%dtX2023_12_05_DatosEgresosHosp_encrip_filt5$row_num_index) %>% 
  nrow()
tab4d2_lab<-  data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt4) %>% 
  dplyr::filter(!row_num_index%in%dtX2023_12_05_DatosEgresosHosp_encrip_filt5$row_num_index) %>% 
  dplyr::distinct(run) %>% 
  nrow()
tab4d3_lab<-  scales::percent(tab4d1_lab/nrow(dtX2023_12_05_DatosEgresosHosp_encrip_filt4))
tab4d_lab<-   paste0("n= ",formatC(tab4d1_lab, format='f', big.mark='.', digits=0) ,
                     "; p= ", formatC(tab4d2_lab, format='f', big.mark='.', digits=0),"; ",tab4d3_lab)
#&#8226;Descarta personas de otras nacionalidades (', ,')\\\\\\l
tab2_lab<-    paste0('&#8226;Descarta usuarios que no reporten alguno de los diagnósticos CIE-10 \n(',
                  tab2a_lab,')\\\\\\l')

tab3_lab<-    paste0('      Hospitalizaciones con\ndiagnósticos de interés\n(n = ', 
                  formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_filt), format='f', big.mark='.', digits=0), ';\np= ',
                  formatC(data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt)%>% dplyr::distinct(run)%>% nrow(), format='f', big.mark='.', digits=0),')')
tab4_lab<-    paste0('&#8226;Descarta personas que no son chilenas (',tab4a_lab,')\\\\\\l&#8226;Descarta personas sin previsión registrada(96) o desconocida(99) (', 
                     tab4b_lab,')\\\\\\l&#8226;Descarta registros con RUNs erróneos (', 
                     tab4c_lab,')\\\\\\l&#8226;Descarta registros con pacientes de edades <15|>64(', tab4d_lab,')\\\\\\l')
tab5_lab<-    paste0('      Muestra de Hospitalizaciones \n(n = ', 
                     formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_filt5), format='f', big.mark='.', digits=0), ';\np= ',
                     formatC(data.table::as.data.table(dtX2023_12_05_DatosEgresosHosp_encrip_filt5)%>% dplyr::distinct(run)%>% nrow(), format='f', big.mark='.', digits=0),
                     ')')

#https://stackoverflow.com/questions/46750364/diagrammer-and-graphviz
#https://mikeyharper.uk/flowcharts-in-r-using-diagrammer/
#http://blog.nguyenvq.com/blog/2012/05/29/better-decision-tree-graphics-for-rpart-via-party-and-partykit/
#http://blog.nguyenvq.com/blog/2014/01/17/skeleton-to-create-fast-automatic-tree-diagrams-using-r-and-graphviz/
#https://cran.r-project.org/web/packages/DiagrammeR/vignettes/graphviz-mermaid.html
#https://stackoverflow.com/questions/39133058/how-to-use-graphviz-graphs-in-diagrammer-for-r
#https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781789802566/1/ch01lvl1sec21/creating-diagrams-via-the-diagrammer-package
#https://justlegal.be/2019/05/using-flowcharts-to-display-legal-procedures/
#

library(DiagrammeR) #⋉
plot_merge_flowchart<-
  grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Times, shape = rectangle, fontsize = 9]        
      tab1 [label = '@@1']
      blank [label = '', width = 0.0001, height = 0.0001]
      tab2 [label = '@@2',fontsize = 7]
      tab3 [label = '@@3']

      blank2 [label = '', width = 0.0001, height = 0.0001]
      tab4 [label = '@@4',fontsize = 7]
      tab5 [label = '@@5']      

      # edge definitions with the node IDs
      rankdir='TB'; rank= same; tab1 -> blank [arrowhead = none, label='  Paso 1', fontsize = 8];
      rankdir='TB'; rank= same; tab1; tab3;   
      blank -> tab2;
                  subgraph {
              rank = same; tab2; blank;
                  }
      rankdir='TB'; rank= same; blank -> tab3;      

      #chile
      rankdir='TB'; rank= same; tab3 -> blank2 [arrowhead = none, label='  Paso 2', fontsize = 8];
      rankdir='TB'; rank= same; tab3; tab5;   
      blank2 -> tab4;
                  subgraph {
              rank = same; tab4; blank2;
                  }
      rankdir='TB'; rank= same; blank2 -> tab5;  
      }
      [1]:  tab1_lab
      [2]:  tab2_lab
      [3]:  tab3_lab
      [4]:  tab4_lab
      [5]:  tab5_lab
      ", width = 800,
        height = 1200)

plot_merge_flowchart

# WidthCM<-8
# HeightCM<-6
# DPI<-600

# plot_merge_flowchart %>%
#   export_svg %>% charToRaw %>% rsvg::rsvg_pdf("_diagrama_flujo.pdf")
# plot_merge_flowchart %>% DiagrammeRsvg::export_svg()%>%
#   charToRaw %>% rsvg::rsvg(width = WidthCM *(DPI/2.54), 
#                 height = HeightCM *(DPI/2.54)) %>% png::writePNG("_diagrama_flujo.png")
htmlwidgets::saveWidget(plot_merge_flowchart, "_figs/_diagrama_flujo.html")
webshot::webshot("_figs/_diagrama_flujo.html", "_figs/_diagrama_flujo2.png",
                 vwidth = 1200, vheight = 900, zoom = 4)

invisible("exporté el gráfico a https://rpubs.com/agscl/flowchart")


invisible("Escribir nueva base de datos")
write_parquet(dtX2023_12_05_DatosEgresosHosp_encrip_filt5, 
              "20231205_hosp_parquet_filt5.gz.parquet", 
              compression = "gzip", compression_level = 5)


#https://stackoverflow.com/questions/1554635/graphviz-how-to-have-a-subgraph-be-left-to-right-when-main-graph-is-top-to-bot
#https://stackoverflow.com/questions/65509087/diagrammer-flowchart-align-vertical-nodes
#https://stackoverflow.com/questions/39451158/how-to-specify-vertical-alignment-of-nodes-in-r-package-diagrammer
#https://stackoverflow.com/questions/64323943/graphviz-and-dot-files-horizontal-and-vertical-node-alignment-intervening-node
#https://stackoverflow.com/questions/5424555/changing-edge-direction-in-dot
#https://graphviz.org/docs/attrs/rankdir/


#save.image("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/10_Egresos Hospitalarios/20250105.RData")