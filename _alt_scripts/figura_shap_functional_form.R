project_root <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
fname        <- "pred21_ndp_2026_03_26.Rdata"
hits <- list.files(project_root, pattern = paste0("^", fname, "$"),
                   recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
find_latest_file <- function(project_root, prefix, extension = "csv") {
  # Construct the regex pattern using the provided extension.
  # We use \\. to match the literal dot before the extension.
  pattern <- paste0(prefix, "_\\d{8}_\\d{4}\\.", extension, "$")
  # List files matching the pattern recursively under the project root
  files <- list.files(
    path = project_root,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(files) == 0) {
    stop(paste("No files found matching pattern:", pattern, "under", project_root))
  }
  # Helper to extract timestamps from filenames (expected format: YYYYMMDD_HHMM)
  extract_timestamp <- function(filename) {
    matches <- regmatches(
      basename(filename),
      gregexpr("\\d{8}_\\d{4}", basename(filename))
    )
    if (length(matches[[1]]) == 0) {
      return(NA)
    }
    # Use the last match in case there are multiple date-like strings in the path
    return(matches[[1]][length(matches[[1]])])
  }
  # Compute timestamps for all found files
  timestamps <- sapply(files, extract_timestamp)
  # Remove any files where timestamp extraction failed
  valid_idx <- !is.na(timestamps)
  files <- files[valid_idx]
  timestamps <- timestamps[valid_idx]
  if (length(files) == 0) {
    stop("No valid timestamps found in the matching filenames.")
  }
    # Sort by timestamp (descending) and return the latest file path
  # Lexicographical sorting works correctly for YYYYMMDD_HHMM format
  latest_idx <- order(timestamps, decreasing = TRUE)[1]
  return(files[latest_idx])
}

library(tidytable)
library(ggplot2)
library(readr)
library(tableone)
library(survival)
library(scales)
global_xgb_func_form <- rio::import(
  find_latest_file(
    file.path(project_root, "cons", "_out"),
    "XGB12_corr_Functional_Forms_Dual_Aggregated")
)
shap_norm <- global_xgb_func_form %>%
  filter(
    is.finite(Feature_Value),
    is.finite(SHAP_Impact)
  ) %>%
  group_by(Predictor) %>%
  mutate(
    n_unique = n_distinct(Feature_Value),
    Feature_Value_norm = dplyr::case_when(
      n_unique <= 1 ~ 0.5,
      n_unique <= 10 ~ (dense_rank(Feature_Value) - 1) / (n_unique - 1),
      TRUE ~ percent_rank(Feature_Value)
    )
  ) %>%
  ungroup()
shap_curve_norm <- shap_norm %>%
  group_by(Outcome, Predictor) %>%
  mutate(value_bin = dplyr::ntile(Feature_Value_norm, 50)) %>%
  group_by(Outcome, Predictor, value_bin) %>%
  summarise(
    x_norm = median(Feature_Value_norm, na.rm = TRUE),
    x_raw_median = median(Feature_Value, na.rm = TRUE),
    shap_median = median(SHAP_Impact, na.rm = TRUE),
    shap_q25 = quantile(SHAP_Impact, 0.25, na.rm = TRUE),
    shap_q75 = quantile(SHAP_Impact, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

library(tibble)

# 1) Tabla completa de homologación
homologacion_tbl <- tribble(
  ~variable, ~nombre_legible, ~tipo, ~categorias_rango,
  "adm_age_rec3", "Edad al ingreso", "Numérica", "18 a 64 años",
  "porc_pobr", "Índice de pobreza de la comuna de residencia", "Numérica", "0 a 1 (continua)",
  "dit_m", "Tiempo de estadía en tratamiento", "Numérica", "0 a 36 meses",
  "tenure_status_household", "Situación de vivienda", "Politómica", "Allegado, Arrienda, Ocupación irregular, Vivienda propia/Cedida/Paga dividendo, Otros",
  "urbanicity_cat", "Urbanización de la comuna de residencia", "Politómica", "Rural, Urbana, Mixta",
  "evaluacindelprocesoteraputico", "Evaluación egreso: proceso terapéutico", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "eva_consumo", "Evaluación egreso: patrón de consumo", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "eva_fam", "Evaluación egreso: situación familiar", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "eva_relinterp", "Evaluación egreso: relaciones interpersonales", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "eva_ocupacion", "Evaluación egreso: situación ocupacional", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "eva_sm", "Evaluación egreso: salud mental", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "eva_fisica", "Evaluación egreso: salud física", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "eva_transgnorma", "Evaluación egreso: transgresión a la norma", "Politómica", "Logro Alto, Intermedio, Mínimo",
  "prim_sub_freq_rec", "Frecuencia de consumo (sustancia principal al ingreso)", "Politómica", "≤1 día/semana, 2-6 días/semana, Diario",
  "ed_attainment_corr", "Escolaridad", "Politómica", "Hasta primaria, Hasta secundaria, Mayor que secundaria"
)

# 2) Vector nombrado para ggplot2 / facet labels
homologacion_labels <- c(
  adm_age_rec3 = "Edad al ingreso",
  porc_pobr = "Índice de pobreza comunal",
  dit_m = "Tiempo de estadía (meses)",
  tenure_status_household = "Situación de vivienda",
  urbanicity_cat = "Urbanización de la comuna",
  evaluacindelprocesoteraputico = "Egreso: proceso terapéutico",
  eva_consumo = "Egreso: patrón de consumo",
  eva_fam = "Egreso: situación familiar",
  eva_relinterp = "Egreso: relaciones interpersonales",
  eva_ocupacion = "Egreso: situación ocupacional",
  eva_sm = "Egreso: salud mental",
  eva_fisica = "Egreso: salud física",
  eva_transgnorma = "Egreso: transgresión a la norma",
  prim_sub_freq_rec = "Frecuencia de consumo al ingreso",
  ed_attainment_corr = "Escolaridad"
)

ggplot(
  shap_curve_norm,
  aes(x = x_norm, y = shap_median, color = Outcome, group = Outcome)
) +
  geom_ribbon(
    aes(ymin = shap_q25, ymax = shap_q75, fill = Outcome),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(vars(Predictor),
          labeller = labeller(Predictor = homologacion_labels),
          nrow = 4, ncol = 5, scales = "free_y") +
  scale_color_manual(
    values = c("Readmission" = "#1f77b4", "Death" = "#d62728")
  ) +
  scale_fill_manual(
    values = c("Readmission" = "#1f77b4", "Death" = "#d62728")
  ) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    x = "Percentil normalizado del predictor (0 = bajo, 1 = alto)",
    y = "Impacto SHAP (mediana ± IQR)",
    title = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text.y = element_text(angle = 0),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
ratio_fig=.75
ggsave(
  paste0(file.path(project_root, "cons", "_figs"), "/shap_curves.png"),
  plot = last_plot(),
  width = 16*ratio_fig,
  height = 12*ratio_fig,
  units = "in",
  dpi = 300
)