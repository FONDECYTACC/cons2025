fix_encoding <- function(x) {
  if (is.character(x)) {
    # Remove soft hyphen
    x <- str_replace_all(x, "\\u00AD", "")
    
    # Fix accented vowels - basic patterns
    x <- str_replace_all(x, "Ã¡", "á")
    x <- str_replace_all(x, "Ã©", "é")
    x <- str_replace_all(x, "Ã­", "í")
    x <- str_replace_all(x, "Ã³", "ó")
    x <- str_replace_all(x, "Ãº", "ú")
    x <- str_replace_all(x, "Ã\\u0081", "Á")
    x <- str_replace_all(x, "Ã‰", "É")
    x <- str_replace_all(x, "Ã\\u008d", "Í")
    x <- str_replace_all(x, "Ã'", "Ó")
    x <- str_replace_all(x, "Ãš", "Ú")
    
    # Fix ñ/Ñ
    x <- str_replace_all(x, "Ã±", "ñ")
    x <- str_replace_all(x, "Ã'", "Ñ")
    
    # Fix combined patterns (double-encoded UTF-8)
    x <- str_replace_all(x, "Ã\\u0083¡", "á")
    x <- str_replace_all(x, "Ã\\u0083©", "é")
    x <- str_replace_all(x, "Ã\\u0083­", "í")
    x <- str_replace_all(x, "Ã\\u0083³", "ó")
    x <- str_replace_all(x, "Ã\\u0083º", "ú")
    x <- str_replace_all(x, "Ã\\u0083±", "ñ")
    x <- str_replace_all(x, "Ã\\u0083\\u0081", "Á")
    x <- str_replace_all(x, "Ã\\u0083‰", "É")
    x <- str_replace_all(x, "Ã\\u0083\\u008d", "Í")
    x <- str_replace_all(x, "Ã\\u0083", "Ó")
    x <- str_replace_all(x, "Ã\\u0083š", "Ú")
    x <- str_replace_all(x, "Ã\\u0083'", "Ñ")
    
    # Fix other special characters
    x <- str_replace_all(x, "Â¿", "¿")
    x <- str_replace_all(x, "Â¡", "¡")
    x <- str_replace_all(x, "â€œ", '"')
    x <- str_replace_all(x, "â€\\u009d", '"')
    #x <- str_replace_all(x, "â€™, "'")
    x <- str_replace_all(x, "â€", "–")
    x <- str_replace_all(x, "â€", "—")
    
    # Fix degree symbol and other special chars
    x <- str_replace_all(x, "Âº", "º")
    x <- str_replace_all(x, "Âª", "ª")
    x <- str_replace_all(x, "Â°", "°")
    
    # Additional patterns found in data
    x <- str_replace_all(x, "Ã\\u0083\\u009a", "Ú")
    x <- str_replace_all(x, "Ã\\u0083\\u00ad", "í")
    x <- str_replace_all(x, "Ã\\u0083\\u00a1", "á")
    x <- str_replace_all(x, "Ã\\u0083\\u00b3", "ó")
    x <- str_replace_all(x, "Ã\\u0083\\u00ba", "ú")
    x <- str_replace_all(x, "Ã\\u0083\\u00a9", "é")
    
    # Fix common problematic patterns
    x <- str_replace_all(x, "PoblaciÃ\\u0083³n", "Población")
    x <- str_replace_all(x, "EspecÃ\\u0083­fico", "Específico")
    x <- str_replace_all(x, "TÃ\\u0083©rmino", "Término")
    x <- str_replace_all(x, "EstÃ\\u0083¡", "Está")
    x <- str_replace_all(x, "PÃ\\u0083ºblico", "Público")
    x <- str_replace_all(x, "Ã\\u0083ºltimo", "último")
    x <- str_replace_all(x, "aÃ\\u0083±os", "años")
    x <- str_replace_all(x, "Ã\\u0083\\u009anicamente", "Únicamente")
    x <- str_replace_all(x, "dÃ\\u0083­as", "días")
    x <- str_replace_all(x, "administraciÃ\\u0083³n", "administración")
    x <- str_replace_all(x, "TerapÃ\\u0083©utica", "Terapéutica")
    x <- str_replace_all(x, "MÃ\\u0083­nimo", "Mínimo")
    x <- str_replace_all(x, "gÃ\\u0083©nero", "género")
    x <- str_replace_all(x, "PsÃ\\u0083­quica", "Psíquica")
    x <- str_replace_all(x, "fÃ\\u0083­sico", "físico")
    x <- str_replace_all(x, "bÃ\\u0083¡sicos", "básicos")
    x <- str_replace_all(x, "mecÃ\\u0083¡nica", "mecánica")
    x <- str_replace_all(x, "lÃ\\u0083­mite", "límite")
    x <- str_replace_all(x, "esquizotÃ\\u0083­pico", "esquizotípico")
    x <- str_replace_all(x, "neurÃ\\u0083³ticos", "neuróticos")
    x <- str_replace_all(x, "hÃ\\u0083¡bitos", "hábitos")
    x <- str_replace_all(x, "especÃ\\u0083­ficos", "específicos")
    x <- str_replace_all(x, "cardiopatÃ\\u0083­as", "cardiopatías")
    x <- str_replace_all(x, "AfganistÃ\\u0083¡n", "Afganistán")
    x <- str_replace_all(x, "CocaÃ\\u0083­na", "Cocaína")
    x <- str_replace_all(x, "aspiraciÃ\\u0083³n", "aspiración")
    
    x <- str_replace_all(x, "ÃƒÂ³", "ó")
    x <- str_replace_all(x, "ÃƒÂ¡", "á")
    x <- str_replace_all(x, "ÃƒÂ©", "é")
    x <- str_replace_all(x, "ÃƒÂº", "ú")
    x <- str_replace_all(x, "ÃƒÂ±", "ñ")
    x <- str_replace_all(x, "Ãƒâ€˜", "Ñ")
    x <- str_replace_all(x, "ÃƒÂ", "Á")
    x <- str_replace_all(x, "Ã‚Âº", "º")
    x <- str_replace_all(x, "Ã‚Â°", "°")
    x <- str_replace_all(x, "Ã‚Âª", "ª")
    x <- str_replace_all(x, "Ã‚Â¡", "¡")
    x <- str_replace_all(x, "Ã‚Â¿", "¿")
    x <- str_replace_all(x, "ÃƒÂ", "í")
    x <- str_replace_all(x, "ÃƒÂ­", "í")
    x <- str_replace_all(x, "Ãƒâ€œ", "Ó")
    x <- str_replace_all(x, "Ãƒâ€š", "Ê")
    x <- str_replace_all(x, "Ãƒâ€", "É")
    x <- str_replace_all(x, "ÃƒÂ¼", "ü")
    x <- str_replace_all(x, "ÃƒÂ¯", "ï")
    x <- str_replace_all(x, "ÃƒÂ¶", "ö")
    x <- str_replace_all(x, "Ã‚Â«", "«")
    x <- str_replace_all(x, "Ã‚Â»", "»")
    x <- str_replace_all(x, "Ãƒâ€¡", "Ç")
    x <- str_replace_all(x, "ÃƒÂ§", "ç")
    x <- str_replace_all(x, "Ã‚Â", "")
    x <- str_replace_all(x, "Ãƒ", "")
    x <- str_replace_all(x, "\u00AD", "")
    x <- str_replace_all(x, "\u00C2\u00AD", "")
    x <- str_replace_all(x, "\u00C2", "")
    x <- str_replace_all(x, "VIÁ'A", "VIÑA")
    x <- str_replace_all(x, "RELONCAVÁ\u008d", "RELONCAVI")
    x <- str_replace_all(x, "MarÁa", "María")
    x <- str_replace_all(x, "Á'UBLE", "ÑUBLE")
    x <- str_replace_all(x, "VÁnculos", "Vínculos")#x <- str_replace_all(x, "CONCEPCIÁ"N", "CONCEPCIÓN")
    #x <- str_replace_all(x,'CONCEPCIÁ\“N', "CONCEPCIÓN")    
    x <- str_replace_all(x, "AYSÁ‰N", "AYSÉN")
    x <- str_replace_all(x, "MÁnimo", "Mínimo")
    x <- str_replace_all(x, "M\\?mo", "Mínimo")
    x <- str_replace_all(x, "ClÁnica", "Clínica")
    x <- str_replace_all(x, "Prisionizaci\\?", "Prisionalización")
    x <- str_replace_all(x, "Explotaci\\?omercial", "Explotación comercial")
    x <- str_replace_all(x, "PatologÁa", "Patología")
    x <- str_replace_all(x, "CardiopatÁas", "Cardiopatías")
    x <- str_replace_all(x, "especÁfico", "específico")
    x <- str_replace_all(x, "esquizotÁpico", "esquizotípico")
    x <- str_replace_all(x, "TricotilomanÁa", "Tricotilomanía")
    x <- str_replace_all(x, "hipomanÁaco", "hipomaníaco")
    x <- str_replace_all(x, "lÁmite", "límite")
    x <- str_replace_all(x, "manÁaco", "maníaco")
    x <- str_replace_all(x, "Á\u0081nimo", "Ánimo")
    x <- str_replace_all(x, "CleptomanÁa", "Cleptomanía")
    x <- str_replace_all(x, "HipocondrÁa", "Hipocondría")
    x <- str_replace_all(x, "RAÁ\u008dCES", "RAÍCES")
    x <- str_replace_all(x, "RAÁ\\u008dCES", "RAÍCES")
    x <- str_replace_all(x, "CuracavÁ", "Curacaví")
    x <- str_replace_all(x, "raÁces", "raíces")
    x <- str_replace_all(x, "TERAPÁ‰UTICA", "TERAPÉUTICA")
    x <- str_replace_all(x, "RaÁces", "Raíces")
    x <- str_replace_all(x, "\\?BLE", "ÑUBLE")
    x <- str_replace_all(x, "BÁo-BÁo", "Bío-Bío")
    x <- str_replace_all(x, "IBA\\?S", "IBAÑEZ")
    x <- str_replace_all(x, "ReloncavÁ", "Reloncaví")
    x <- str_replace_all(x, "ValparaÁso", "Valparaíso")
    x <- str_replace_all(x, "AraucanÁa ", "Araucanía")
    x <- str_replace_all(x, "Á'uble", "Ñuble")
    x <- str_replace_all(x, "EspecÁfico", "Específico")
    x <- str_replace_all(x, "VI\\? DEL MAR", "VIÑA DEL MAR")
    x <- str_replace_all(x, "DO\\?HUE", "DOÑIHUE")
    x <- str_replace_all(x, "HUALA\\?", "HUALAÑÉ")
    x <- str_replace_all(x, "\\?qu\\?", "ÑIQUÉN")
    x <- str_replace_all(x, "CHA\\?RAL", "CHAÑARAL")
    x <- str_replace_all(x, "OLLAG\\?", "OLLAGÜE")
    x <- str_replace_all(x, "VICU\\?", "VICUÑA")
    x <- str_replace_all(x, "CA\\?TE", "CAÑETE")
    x <- str_replace_all(x, "\\?\\?A", "ÑUÑOA")
    x <- str_replace_all(x, "PolicÁa", "Policía")
    x <- str_replace_all(x, "GarantÁa", "Garantía")
    x <- str_replace_all(x, "fiscalÁa", "fiscalía")
    x <- str_replace_all(x, "HaitÁ", "Haití")
    x <- str_replace_all(x, "HungrÁa", "Hungría")
    x <- str_replace_all(x, "PaÁses Bajos", "Países Bajos")
    x <- str_replace_all(x, "Atacame\\?", "Atacameño")
    x <- str_replace_all(x, "Y\\?na", "Yámana")
    x <- str_replace_all(x, "Y\\?gan", "Yagán")
    x <- str_replace_all(x, "Hipn\\?os", "Hipnóticos")
    x <- str_replace_all(x, "Hero\\?", "Heroína")
    x <- str_replace_all(x, "code\\?", "codeína")
    x <- str_replace_all(x, "Analg\\?cos", "Analgésicos")
    x <- str_replace_all(x, "barbit\\?os", "barbitúricos")
    x <- str_replace_all(x, "Alucin\\?os", "Alucinógenos")
    x <- str_replace_all(x, "ãƒâ³n", "ón")
    x <- str_replace_all(x, "ãƒâ©n", "én")
    x <- str_replace_all(x, "ãƒâº", "ú")
    x <- str_replace_all(x, "ãƒâºa", "úa")
    x <- str_replace_all(x, "ãƒâos", "íos")
    x <- str_replace_all(x, "ãƒâuble", "Ñuble")
    x <- str_replace_all(x, "ãƒâ³n general", "ón general")
    x <- str_replace_all(x, "ãƒâ", "í")
    x <- str_replace_all(x, "ãƒâ³n casa", "ón casa")
    x <- str_replace_all(x, "ãƒârbara", "árbara")
    x <- str_replace_all(x, "nãƒâ", "ñ")
    x <- str_replace_all(x, "raãƒâces", "raíces")
    x <- str_replace_all(x, "bãƒâsico", "básico")
    x <- str_replace_all(x, "ãƒâ©utico", "éutico")
    x <- str_replace_all(x, "vãƒânculos", "vínculos")
    x <- str_replace_all(x, "marãƒâa", "maría")
    x <- str_replace_all(x, "inãƒâ©s", "inés")
    x <- str_replace_all(x, "raí\\u008dces", "raíces")
    x <- str_replace_all(x, "chiloí©", "chiloé")
    x <- str_replace_all(x, "terapí©utico", "terapéutico")
    x <- str_replace_all(x, "bísico", "básico")
    x <- str_replace_all(x, "peí±ablanca", "peñablanca")
    x <- str_replace_all(x, "iní©s", "inés")
  }
  return(x)
}
library(tidyverse)
library(tidytable)

cat("We take the new database of 2019 and try to standardize according to formatting made previous to the first step of the deduplication phase")

pathwd<- gsub("cons/","",paste0(getwd(),"/data/20250529_original_data/"))

paste0(pathwd, "2023_c1_dup1_encrip.csv")

X2023dup_encrip <- readr::read_delim(paste0(pathwd, "2023_c1_dup1_encrip.csv"), delim = ";", escape_double = FALSE, 
                                     trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", encoding = "latin1"),#encoding = "ISO-8859-1"),
                                     na = c("", "NA","null"), guess_max = min(1e5, Inf)) |> janitor::clean_names()
  
X2023dup2_encrip <- readr::read_delim(paste0(pathwd, "2023_c1_dup2_encrip.csv"), delim = ";", escape_double = FALSE, 
                                     trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", encoding = "latin1"),#encoding = "ISO-8859-1"),
                                     na = c("", "NA","null"), guess_max = min(1e5, Inf)) |> janitor::clean_names()

X2024dup_encrip <- readr::read_delim(paste0(pathwd, "2024_c1_dup1_encrip.csv"), delim = ";", escape_double = FALSE, 
                                     trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", encoding = "latin1"),#encoding = "ISO-8859-1"),
                                     na = c("", "NA","null"), guess_max = min(1e5, Inf)) |> janitor::clean_names()

X2024dup2_encrip <- readr::read_delim(paste0(pathwd, "2024_c1_dup2_encrip.csv"), delim = ";", escape_double = FALSE, 
                                      trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", encoding = "latin1"),#encoding = "ISO-8859-1"),
                                      na = c("", "NA","null"), guess_max = min(1e5, Inf)) |> janitor::clean_names()



SISTRAT23_c1_2023_2024= data.table::rbindlist(mget(c("X2023dup_encrip", "X2023dup2_encrip", "X2024dup_encrip", "X2024dup2_encrip")), idcol="TABLE", fill=T) %>%
  dplyr::mutate(TABLE = str_extract(TABLE, "\\d+")) %>% #distinct(TABLE)
  dplyr::mutate(TABLE_rec = sub("^(\\d{4}).*dup(\\d*)?.*", "\\1\\2", TABLE)) %>% 
  dplyr::select(TABLE, hashkey, everything())

#get the columns with characters
char_cols <- names(which(sapply(SISTRAT23_c1_2023_2024, is.character)))


# Apply the function to columns in character format
SISTRAT23_c1_2023_2024 <- SISTRAT23_c1_2023_2024 |>
  mutate(across(.cols = all_of(char_cols), .fns = fix_encoding))

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
##:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
invisible("Obtain unique values by column")

# Obtain unique values per column
unique_values_list23_24 <- setNames(
  lapply(char_cols, function(col_name) {
    SISTRAT23_c1_2023_2024 |>
      select(all_of(col_name)) |>
      distinct() |>
      pull()
  }),
  char_cols
)


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
##:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
fix_encoding_spanish <- function(text) {
  if (!is.character(text)) return(text)
  
  # UTF-8 interpreted as Latin-1
  text <- gsub("Ã¡", "á", text, fixed = TRUE)
  text <- gsub("Ã©", "é", text, fixed = TRUE)
  text <- gsub("Ã­", "í", text, fixed = TRUE)
  text <- gsub("Ã³", "ó", text, fixed = TRUE)
  text <- gsub("Ãº", "ú", text, fixed = TRUE)
  text <- gsub("Ã±", "ñ", text, fixed = TRUE)
  text <- gsub("Ã¼", "ü", text, fixed = TRUE)
  text <- gsub("Ã", "í", text, fixed = TRUE)
  
  # Capital letters
  text <- gsub("Ã\u0081", "Á", text, fixed = TRUE)
  text <- gsub("Ã\u0089", "É", text, fixed = TRUE)
  text <- gsub("Ã\u008d", "Í", text, fixed = TRUE)
  text <- gsub("Ã\u0093", "Ó", text, fixed = TRUE)
  text <- gsub("Ã\u009a", "Ú", text, fixed = TRUE)
  text <- gsub("Ã\u0091", "Ñ", text, fixed = TRUE)
  
  # Context-based replacements for question marks
  text <- gsub("AYS\\\\?N", "AYSÉN", text)
  text <- gsub("IBA\\\\?ES", "IBAÑEZ", text)
  text <- gsub("\\\\?UBLE", "ÑUBLE", text)
  text <- gsub("PE\\\\?ALOLEN", "PEÑALOLÉN", text)
  text <- gsub("\\\\?U\\\\?OA", "ÑUÑOA", text)
  text <- gsub("VI\\\\?A", "VIÑA", text)
  text <- gsub("Coca\\\\?na", "Cocaína", text)
  text <- gsub("Analg\\\\?sicos", "Analgésicos", text)
  text <- gsub("Hipn\\\\?ticos", "Hipnóticos", text)
  text <- gsub("T\\\\?cnica", "Técnica", text)
  text <- gsub("T\\\\?cnico", "Técnico", text)
  text <- gsub("a\\\\?os", "años", text)
  text <- gsub("b\\\\?sica", "básica", text)
  text <- gsub("Educaci\\\\?n", "Educación", text)
  text <- gsub("pensi\\\\?n", "pensión", text)
  text <- gsub("Hospeder\\\\?a", "Hospedería", text)
  text <- gsub("Ocupaci\\\\?n", "Ocupación", text)
  text <- gsub("Explotaci\\\\?n", "Explotación", text)
  text <- gsub("M\\\\?nimo", "Mínimo", text)
  
  # Other fixes
  text <- gsub("ï¿½", "í", text, fixed = TRUE)
  text <- gsub("fÓsico", "físico", text, fixed = TRUE)
  text <- gsub("PsÓquica", "Psíquica", text, fixed = TRUE)
  text <- gsub("ASOCIACIÓ", "ASOCIACIÓN", text, fixed = TRUE)
  text <- gsub("fÓsico", "físico", text, fixed = TRUE)
  text <- gsub("PsÓquica", "Psíquica", text, fixed = TRUE)
  text <- gsub("ASOCIACIÓ\u0093N", "ASOCIACIÓN", text, fixed = TRUE)
  text <- gsub("EDUCACIÓ\u0093N", "EDUCACIÓN", text, fixed = TRUE)
  text <- gsub("Ó\u0091UBLE", "ÑUBLE", text, fixed = TRUE)
  text <- gsub("OÃ\u0082´HIGGINS", "O'HIGGINS", text, fixed = TRUE)
  text <- gsub("VIÓ\u0091A", "VIÑA", text, fixed = TRUE)
  text <- gsub("CONCEPCIÓ\u0093N", "CONCEPCIÓN", text, fixed = TRUE)
  text <- gsub("AYS?N", "AYSÉN", text, fixed = TRUE)
  text <- gsub("MÓnimo", "Mínimo", text, fixed = TRUE)
  text <- gsub("M?nimo", "Mínimo", text, fixed = TRUE)
  text <- gsub("EstadÓa", "Estadía", text, fixed = TRUE)
  text <- gsub("Explotaci?n", "Explotación", text, fixed = TRUE)
  text <- gsub("miocardiopatÓa", "miocardiopatía", text, fixed = TRUE)
  text <- gsub("Prisionalizaciónn", "Prisionalización", text, fixed = TRUE)
  text <- gsub("esquizotÓpico", "esquizotípico", text, fixed = TRUE)
  text <- gsub("especÓficos", "específicos", text, fixed = TRUE)
  text <- gsub("especÓfico", "específico", text, fixed = TRUE)
  text <- gsub("lÓmite", "límite", text, fixed = TRUE)
  text <- gsub("dÓas", "días", text, fixed = TRUE)
  text <- gsub("dÓa", "día", text, fixed = TRUE)
  text <- gsub("CocaÓna", "Cocaína", text, fixed = TRUE)
  text <- gsub("Ocupaci?n", "Ocupación", text, fixed = TRUE)
  text <- gsub("Ó\u009anicamente", "Únicamente", text, fixed = TRUE)
  text <- gsub("Educaci?n b?sica", "Educación básica", text, fixed = TRUE)
  text <- gsub("4 o m?s a?os", "4 o más años", text, fixed = TRUE)
  text <- gsub("T?cnica", "Técnica", text, fixed = TRUE)
  text <- gsub("T?cnico", "Técnico", text, fixed = TRUE)
  text <- gsub("1-3 a?os", "1-3 años", text, fixed = TRUE)
  text <- gsub("Nunca estudi?", "Nunca estudió", text, fixed = TRUE)
  text <- gsub("Alucin?genos", "Alucinógenos", text, fixed = TRUE)
  text <- gsub("fiscalÓa", "fiscalía", text, fixed = TRUE)
  text <- gsub("GarantÓa", "Garantía", text, fixed = TRUE)
  text <- gsub("AraucanÓa", "Araucanía", text, fixed = TRUE)
  text <- gsub("ValparaÓso", "Valparaíso", text, fixed = TRUE)
  text <- gsub("?U?OA", "ÑUÑOA", text, fixed = TRUE)
  text <- gsub("?iqu?n", "Ñiquén", text, fixed = TRUE)
  text <- gsub("HaitÓ", "Haití", text, fixed = TRUE)
  
  # Additional common fixes
  text <- gsub("ASOCIACIÓ", "ASOCIACIÓN", text, fixed = TRUE)
  text <- gsub("PE?ALOLEN", "PEÑALOLÉN", text, fixed = TRUE)
  text <- gsub("VI?A DEL MAR", "VIÑA DEL MAR", text, fixed = TRUE)
  text <- gsub("Coca?na", "Cocaína", text, fixed = TRUE)
  text <- gsub("Analg?sicos", "Analgésicos", text, fixed = TRUE)
  text <- gsub("Hipn?ticos", "Hipnóticos", text, fixed = TRUE)
  text <- gsub("a?os", "años", text, fixed = TRUE)
  text <- gsub("b?sica", "básica", text, fixed = TRUE)
  text <- gsub("Educaci?n", "Educación", text, fixed = TRUE)
  text <- gsub("pensi?n", "pensión", text, fixed = TRUE)
  text <- gsub("Hospeder?a", "Hospedería", text, fixed = TRUE)
  text <- gsub("IBA?ES", "IBAÑEZ", text, fixed = TRUE)
  text <- gsub("?UBLE", "ÑUBLE", text, fixed = TRUE)
  
  return(text)
}
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
##:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:


# Apply the function to each character column
SISTRAT23_c1_2023_2024 <- SISTRAT23_c1_2023_2024 |>
  as_tidytable() |>  # Convert to tidytable if it isn't already
  mutate(across(all_of(char_cols), fix_encoding_spanish))


invisible("Obtain unique values by column, again")

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# Obtain unique values per column
unique_values_list23_24_2 <- setNames(
  lapply(char_cols, function(col_name) {
    SISTRAT23_c1_2023_2024 |>
      select(all_of(col_name)) |>
      distinct() |>
      pull()
  }),
  char_cols
)
#write_json(unique_values_list23_24_2, "unique_values_list23_24.json", pretty = TRUE)

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:


fix_encoding_spanish2 <- function(text) {
  if (!is.character(text)) return(text)
  # Most common mojibake for Spanish in your file
  text <- gsub("Ó¡", "á", text, fixed = TRUE)
  text <- gsub("Ó©", "é", text, fixed = TRUE)
  text <- gsub("Ó­", "í", text, fixed = TRUE)
  text <- gsub("Ó³", "ó", text, fixed = TRUE)
  text <- gsub("Óº", "ú", text, fixed = TRUE)
  text <- gsub("Ó±", "ñ", text, fixed = TRUE)
  text <- gsub("Ó", "ñ", text, fixed = TRUE)
  text <- gsub("Ó", "Ó", text, fixed = TRUE) # fallback, rarely needed
  text <- gsub("Ã¡", "á", text, fixed = TRUE)
  text <- gsub("Ã©", "é", text, fixed = TRUE)
  text <- gsub("Ã­", "í", text, fixed = TRUE)
  text <- gsub("Ã³", "ó", text, fixed = TRUE)
  text <- gsub("Ãº", "ú", text, fixed = TRUE)
  text <- gsub("Ã±", "ñ", text, fixed = TRUE)
  text <- gsub("Ã", "Á", text, fixed = TRUE)
  text <- gsub("Ã‰", "É", text, fixed = TRUE)
  text <- gsub("Ã", "Í", text, fixed = TRUE)
  text <- gsub("Ã“", "Ó", text, fixed = TRUE)
  text <- gsub("Ãš", "Ú", text, fixed = TRUE)
  text <- gsub("Ã‘", "Ñ", text, fixed = TRUE)
  text <- gsub("Ã¼", "ü", text, fixed = TRUE)
  text <- gsub("Ãœ", "Ü", text, fixed = TRUE)
  text <- gsub("Â", "", text, fixed = TRUE) # Remove stray 'Â'
  return(text)
}


# Apply the function to each character column
SISTRAT23_c1_2023_2024 <- SISTRAT23_c1_2023_2024 |>
  as_tidytable() |>  # Convert to tidytable if it isn't already
  mutate(across(all_of(char_cols), fix_encoding_spanish2))


invisible("Obtain unique values by column, again")

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# Obtain unique values per column
unique_values_list23_24_3 <- setNames(
  lapply(char_cols, function(col_name) {
    SISTRAT23_c1_2023_2024 |>
      select(all_of(col_name)) |>
      distinct() |>
      pull()
  }),
  char_cols
)
#write_json(unique_values_list23_24_3, "unique_values_list23_24.json", pretty = TRUE)


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
##:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
invisible("Standardize values (to lower, correct tildes, etc.)")

# Function to standardize text
standardize_text <- function(x) {
  # Replace NA with empty string to avoid errors
  x <- ifelse(is.na(x), "", x)
  
  # Convert to lowercase
  x <- tolower(x)
  
  # Trim leading and trailing spaces
  x <- stringr::str_trim(x)
  
  # Replace multiple spaces with a single space
  x <- stringr::str_replace_all(x, "\\s+", " ")
  
  # Remove periods at the end
  x <- stringr::str_replace_all(x, "\\s*\\.\\s*$", "")
  
  # Replace accented characters
  accent_replacements <- c(
    "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
    "Á" = "a", "É" = "e", "Í" = "i", "Ó" = "o", "Ú" = "u",
    "ñ" = "n", "Ñ" = "n"
  )
  
  for (accent in names(accent_replacements)) {
    x <- gsub(accent, accent_replacements[accent], x, fixed = TRUE)
  }
  
  return(x)
}

##:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# Apply to all character columns
SISTRAT23_c1_2023_2024_df <- SISTRAT23_c1_2023_2024|>
  mutate(across(all_of(char_cols), standardize_text))


SISTRAT23_c1_2023_2024_df2 <- SISTRAT23_c1_2023_2024_df %>%
  dplyr::rename(
    hash_key = hashkey,
    codigo_identificacion = codigo_identificaci_a_a_n,
    nombre_centro = nombre_centro,
    tipo_centro = tipo_centro,
    region_del_centro = regi_a_a_ndel_centro,
    servicio_de_salud = serviciode_salud,
    tipo_de_programa = tipode_programa,
    tipo_de_plan = tipode_plan,
    senda = senda,
    dias_en_tratamiento = diasen_tratamiento,
    n_meses_en_tratamiento = n_mesesen_tratamiento,
    dias_en_senda = diasen_senda,
    n_meses_en_senda = n_mesesen_senda,
    sexo = sexo,
    edad = edad,
    nombre_usuario = nombre_usuario,
    comuna_residencia = comuna_residencia,
    origen_de_ingreso = origende_ingreso,
    pais_nacimiento = pa_a_a_s_nacimiento,
    nacionalidad = nacionalidad,
    etnia = etnia,
    estado_conyugal = estado_conyugal,
    numero_de_hijos = na_a_a_aomerode_hijos,
    numero_de_hijos_ingreso_tratamiento_residencial = na_a_a_aomerode_hijos_ingreso_tratami,
    parentesco_con_el_jefe_de_hogar = parentescoconel_jefede_hogar,
    numero_de_tratamientos_anteriores = numerode_tratamientos_anteriore,
    fecha_ultimo_tratamiento = fecha_ultimo_tratamiento,
    sustancia_de_inicio = sustanciade_inicio,
    edad_inicio_consumo = edad_inicio_consumo,
    se_trata_de_una_mujer_embarazada = a_a_setratadeunamujerembaraza,
    escolaridad_ultimo_ano_cursado = escolaridad_a_a_a_aoltimoa_a_a_ocursad,
    condicion_ocupacional = condicion_ocupacional,
    categoria_ocupacional = categor_a_a_a_ocupacional,
    rubro_trabaja = rubro_trabaja,
    con_quien_vive = con_qui_a_a_n_vive,
    tipo_de_vivienda = tipodevivienda,
    tenencia_de_la_vivienda = tenenciadelavivienda,
    sustancia_principal = sustancia_principal,
    otras_sustancias_no1 = otras_sustanciasn_a_a_a_ao1,
    otras_sustancias_no2 = otras_sustanciasn_a_a_a_ao2,
    otras_sustancias_no3 = otras_sustanciasn_a_a_a_ao3,
    frecuencia_de_consumo_sustancia_principal = frecuenciade_consumo_sustancia,
    edad_inicio_sustancia_principal = edad_inicio_sustancia_principal,
    via_administracion_sustancia_principal = va_a_a_administraci_a_a_n_sustancia,
    diagnostico_trs_consumo_sustancia = diagn_a_a_stico_trs_consumo_sustan,
    diagnostico_trs_psiquiatrico_dsm_iv = diagn_a_a_stico_trs_psiqui_a_a_trico,
    diagnostico_trs_psiquiatrico_sub_dsm_iv = at,
    x2_diagnostico_trs_psiquiatrico_dsm_iv = diagn_a_a_stico_trs_psiqui_a_a_tric,
    x2_diagnostico_trs_psiquiatrico_sub_dsm_iv = av,
    x3_diagnostico_trs_psiquiatrico_dsm_iv = aw,
    x3_diagnostico_trs_psiquiatrico_sub_dsm_iv = ax,
    diagnostico_trs_psiquiatrico_cie_10 = ay,
    diagnostico_trs_psiquiatrico_sub_cie_10 = az,
    x2_diagnostico_trs_psiquiatrico_cie_10 = ba,
    x2_diagnostico_trs_psiquiatrico_sub_cie_10 = bb,
    x3_diagnostico_trs_psiquiatrico_cie_10 = bc,
    x3_diagnostico_trs_psiquiatrico_sub_cie_10 = bd,
    diagnostico_trs_fisico = diagn_a_a_stico_trs_fa_a_sico,
    otros_problemas_de_atencion_de_salud_mental = otros_problemasde_atenci_a_a_nde,
    compromiso_biopsicosocial = compromiso_biopsicosocial,
    diagnostico_global_de_necesidades_de_integracion_social_60 = diagnosticoglobaldenecesidade,
    diagnostico_de_necesidades_de_integrac_io_n_social_en_capital_humano_61 = diagnosticodenecesidadesdein,
    diagnostico_de_necesidades_de_integrac_io_n_social_en_capital_fisico_62 = bj,
    diagnostico_de_necesidades_de_integrac_io_n_social_en_capital_social_63 = bk,
    fecha_ingreso_a_tratamiento = fecha_ingresoa_tratamiento,
    fecha_ingreso_a_convenio_senda = fecha_ingresoa_convenio_senda,
    usuario_de_tribunales_tratamiento_drogas = usuariode_tribunales_tratamien,
    consentimiento_informado = consentimiento_informado,
    fecha_egreso_de_tratamiento = fecha_egresode_tratamiento,
    motivo_de_egreso = motivode_egreso,
    tipo_centro_derivacion = tipo_centro_derivaci_a_a_n,
    evaluacion_del_proceso_terapeutico = evaluaci_a_a_ndel_proceso_terap_a_a_u,
    evaluacion_al_egreso_respecto_al_patron_de_consumo = evaluaci_a_a_nal_egreso_respectoa,
    evaluacion_al_egreso_respecto_a_situacion_familiar = bu,
    evaluacion_al_egreso_respecto_relaciones_interpersonales = evaluaci_a_a_nal_egreso_respecto_r,
    evaluacion_al_egreso_respecto_a_situacion_ocupacional = bw,
    evaluacion_al_egreso_respecto_salud_mental = evaluaci_a_a_nal_egreso_respecto_s,
    evaluacion_al_egreso_respecto_salud_fisica = by,
    evaluacion_al_egreso_respecto_trasgresion_a_la_norma_social = evaluaci_a_a_nal_egreso_respecto_t,
    diagnostico_trastorno_psiquiatrico_cie_10_al_egreso = diagn_a_a_stico_trastorno_psiqui_a_a_t,
    diagnostico_global_de_necesidades_de_integracion_social_80 = cb,
    diagnostico_de_necesidades_de_integrac_io_n_social_en_capital_humano_81 = cc,
    diagnostico_de_necesidades_de_integrac_io_n_social_en_capital_fisico_82 = cd,
    diagnostico_de_necesidades_de_integrac_io_n_social_en_capital_social_83 = ce,
    tiene_menores_de_edad_a_cargo = tienemenoresdeedadacargo,
    motivo_de_egreso_alta_administrativa = motivodeegreso_alta_administra,
    consorcio = consorcio,
    id_centro = i_dcentro,
    ha_estado_embarazada_egreso = haestadoembarazadaegreso,
    identidad_de_genero = identidaddegenero,
    discapacidad = discapacidad,
    opcion_discapacidad = opci_a_a_ndiscapacidad,
    orientacion_sexual = orientaci_a_a_n_sexual,
    servicios_basicos_95 = servicios_basicos,
    laboral_ingresos = laboral_ingresos,
    perso_dormitorio_vivienda = perso_dormitorio_vivienda,
    precariedad_vivienda = precariedad_vivienda,
    servicios_basicos_99 = ct
  )

SISTRAT23_c1_2023_2024_df2<-
  SISTRAT23_c1_2023_2024_df2 %>% 
  dplyr::mutate(birth_date= stringr::str_sub(codigo_identificacion, nchar(codigo_identificacion)-7,nchar(codigo_identificacion))) %>% 
  dplyr::mutate(birth_date= readr::parse_date(birth_date, format="%d%m%Y")) %>% 
  dplyr::mutate(adm_date = str_replace_all(fecha_ingreso_a_tratamiento ,"/","-"),
                senda_adm_date = str_replace_all(fecha_ingreso_a_convenio_senda ,"/","-"),
                discharge_date= str_replace_all(fecha_egreso_de_tratamiento,"/","-"))%>% 
  dplyr::mutate(adm_date= readr::parse_date(adm_date, format="%d-%m-%Y")) %>%
  dplyr::mutate(senda_adm_date= readr::parse_date(senda_adm_date, format="%d-%m-%Y")) %>% 
  dplyr::mutate(discharge_date= readr::parse_date(discharge_date, format="%d-%m-%Y")) 



c2_2324 <- readr::read_delim(paste0(pathwd, "2022-2024_c2_dup_encrip.csv"), delim = ";", escape_double = FALSE, 
                                     trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", encoding = "latin1"),#encoding = "ISO-8859-1"),
                                     na = c("", "NA","null"), guess_max = min(1e5, Inf)) |> janitor::clean_names()%>% 
  dplyr::mutate(fecha_nacimiento = readr::parse_date(fecha_nacimiento, format="%d/%m/%Y")) 


c5_2324 <- readr::read_delim(paste0(pathwd, "2022-2024_c5_dup_encrip.csv"), delim = ";", escape_double = FALSE, 
                             trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", encoding = "latin1"),#encoding = "ISO-8859-1"),
                             na = c("", "NA","null"), guess_max = min(1e5, Inf)) |> janitor::clean_names()%>% 
  dplyr::mutate(fecha_nacimiento = readr::parse_date(fecha_nacimiento, format="%d/%m/%Y")) 








top_2224 <- readr::read_delim(paste0(pathwd, "2022-2024_top_dup_encrip.csv"), delim = ";", escape_double = FALSE, 
                                     trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = ".", tz = "America/Santiago", encoding = "latin1"),#encoding = "ISO-8859-1"),
                                     na = c("", "NA","null"), guess_max = min(1e5, Inf)) |> janitor::clean_names()%>% 
  dplyr::mutate(fecha_nacimiento = readr::parse_date(fecha_nacimiento, format="%d/%m/%Y")) |> 
  select(hashkey, id, everything())
