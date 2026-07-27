options(width = 200)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

tab <- read.csv("G:/My Drive/Alvacast/SISTRAT 2023/cons/_out/table_incl_vs_excl_v3_smd.csv",
                 stringsAsFactors = FALSE, colClasses = "character")

es_labels <- c(
  "Sex (biological categorization)" = "Sexo",
  "Housing situation" = "Situación de vivienda",
  "Employment status" = "Condición ocupacional",
  "Marital status" = "Estado conyugal",
  "Ethnicity (=1)" = "Etnicidad (pueblo originario)",
  "Urbanization level of commune of residence" = "Clasificación de urbanización de la comuna de residencia",
  "Educational attainment" = "Escolaridad",
  "Cohabitation status (harmonized)" = "Estado de convivencia y cohabitación",
  "Psychiatric comorbidity (ICD-10, in-study)" = "Comorbilidad psiquiátrica (CIE-10, en estudio)",
  "Psychiatric comorbidity (ICD-10, diagnosis record)" = "Comorbilidad psiquiátrica (CIE-10, registro diagnóstico)",
  "Psychotic disorders (F2)" = "Trastornos psicóticos (F2)",
  "Mood disorders (F3)" = "Trastornos del ánimo (F3)",
  "Anxiety & stress-related (F4-F5)" = "Trastornos ansiosos y relacionados al estrés (F4-F5)",
  "Personality disorders (F6)" = "Trastornos de personalidad (F6)",
  "Neurocognitive & neurodevelopmental (F0, F7-F9)" = "Neurocognitivo y del neurodesarrollo (F0, F7-F9)",
  "SUD severity (ICD-10)" = "Severidad del TUS (CIE-10)",
  "Domestic violence/Sexual abuse" = "Violencia doméstica/abuso sexual",
  "Physical diagnosis supergroup" = "Comorbilidad física (categoría agrupada)",
  "Primary-substance use frequency at admission (simplified)" = "Frecuencia de consumo de la sustancia principal al ingreso",
  "Polysubstance use (strict)" = "Policonsumo",
  "Admission motive" = "Origen de ingreso a tratamiento",
  "Primary substance of use (recoded)" = "Sustancia principal al ingreso",
  "Housing type (recoded & simplified)" = "Tipo de vivienda",
  "Treatment modality (plan type)" = "Modalidad de tratamiento",
  "Nationality (Chile)" = "Nacionalidad (chilena)",
  "Admission age, y" = "Edad al ingreso, años",
  "Poverty index of commune of residence, %" = "Índice de pobreza de la comuna de residencia, %"
)

d <- tab |>
  dplyr::filter(Variable != "", SMD != "") |>
  dplyr::transmute(
    VariableEN = Variable,
    SMD = as.numeric(SMD)
  ) |>
  dplyr::mutate(
    Variable = dplyr::recode(VariableEN, !!!es_labels)
  ) |>
  dplyr::arrange(SMD) |>
  dplyr::mutate(
    Variable = factor(Variable, levels = Variable),
    flagged = SMD >= 0.15
  )

cat("n variables:", nrow(d), "\n")
unmapped <- setdiff(d$VariableEN, names(es_labels))
if (length(unmapped)) cat("WARNING - sin traducción, se deja en inglés:\n") else cat("Todas las variables tienen traducción.\n")
if (length(unmapped)) print(unmapped)
print(d[, c("Variable", "SMD")])

col_ok   <- "#5B7C8D"
col_flag <- "#B5541A"
col_line <- "#8B979C"

comma_dec <- function(x) gsub("\\.", ",", format(x, nsmall = 1))

p <- ggplot(d, aes(x = SMD, y = Variable)) +
  geom_vline(xintercept = 0.15, linetype = "22", linewidth = 0.45, color = col_line) +
  geom_segment(aes(x = 0, xend = SMD, y = Variable, yend = Variable),
               color = "#D8DEDD", linewidth = 0.5) +
  geom_point(aes(color = flagged), size = 2.6) +
  scale_color_manual(
    values = c(`FALSE` = col_ok, `TRUE` = col_flag),
    labels = c(`FALSE` = "|DME| < 0,15", `TRUE` = "|DME| ≥ 0,15"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(0, max(0.55, max(d$SMD) * 1.05)),
                      breaks = seq(0, 0.8, 0.1), expand = c(0.01, 0),
                      labels = comma_dec) +
  labs(
    x = "Diferencia de medias estandarizada (excluidos − incluidos)",
    y = NULL
  ) +
  theme_minimal(base_size = 11, base_family = "") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ECEFEE", linewidth = 0.4),
    axis.text.y = element_text(color = "#1E2A32", size = 9.3),
    axis.text.x = element_text(color = "#5B6B73", size = 9),
    axis.title.x = element_text(color = "#5B6B73", size = 9.5, margin = margin(t = 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 9),
    legend.margin = margin(b = -4),
    plot.margin = margin(10, 18, 6, 10)
  )

out_dir <- "G:/My Drive/Alvacast/SISTRAT 2023/cons/_figs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "loveplot_incl_vs_excl_smd_es_sintitulo.png"), p,
       width = 8.3, height = 7.1, dpi = 400, bg = "white")
ggsave(file.path(out_dir, "loveplot_incl_vs_excl_smd_es_sintitulo.pdf"), p,
       width = 8.3, height = 7.1, device = cairo_pdf)

cat("\nGuardado:\n", file.path(out_dir, "loveplot_incl_vs_excl_smd_es_sintitulo.png"), "\n",
    file.path(out_dir, "loveplot_incl_vs_excl_smd_es_sintitulo.pdf"), "\n")
cat("\nDONE\n")
