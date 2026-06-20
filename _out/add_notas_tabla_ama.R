# =============================================================================
# add_notas_tabla_ama.R
# Add explanatory footnotes to the consolidated AMA-style audit table and export
# an Excel file with two sheets (Tabla + Notas) plus a Markdown file.
# =============================================================================

library(writexl)

project_root <- normalizePath(getwd(), winslash = "/")
out_dir <- file.path(project_root, "cons", "_out")

tabla <- read.csv(file.path(out_dir, "audit_full_sample_consolidated_ama.csv"),
                  na.strings = "")
tabla$subgroup_variable[tabla$subgroup_variable %in% c(NA, "NA")] <- "—"
tabla$subgroup_value[tabla$subgroup_value %in% c(NA, "NA")] <- "—"

notes <- data.frame(
  Nota = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
  Texto = c(
    "Cohorte completa = 88 152 personas; 5 imputaciones múltiples. Los conteos n_at_risk y n_events son el promedio de las 5 imputaciones (reglas de Rubin) redondeado al entero más cercano. Los desenlaces (rehospitalización y muerte) son observados e idénticos en todas las imputaciones; solo any_phys_dx varía ligeramente porque depende de diagnósticos imputados.",
    "Modelo multiestado illness-death: 1→2 = rehospitalización tras el egreso; 1→3 = muerte sin rehospitalización previa; 2→3 = muerte después de una rehospitalización.",
    "plan_type_strata: estrato del plan de tratamiento, derivado de plan_type_corr. Categorías: pg-pr, m-pr, pg-pai, m-pai, pg-pab.",
    "tr_outcome_adm_discharge_rule_violation_undet: indicador de egreso administrativo por infracción de normas o motivo indeterminado (0 = no; 1 = sí). Aplica solo a la transición 1→2 (rehospitalización).",
    "any_phys_dx: indicador de cualquier comorbilidad física (FALSE = no; TRUE = sí). Aplica a las transiciones de mortalidad 1→3 y 2→3.",
    "n_at_risk: número de sujetos en riesgo de experimentar la transición dentro de ese estrato/subgrupo.",
    "n_events: número de eventos observados (transiciones ocurridas) dentro de ese estrato/subgrupo.",
    "Tasa (%): tasa bruta de eventos = (n_events / n_at_risk) × 100.",
    "La auditoría 'plan_type_strata' de la transición 1→2 se muestra como resumen; sus totales coinciden con la suma de los subgrupos de rule violation.",
    "Precaución con celdas ralas: varios estratos de la transición 2→3 tienen muy pocos eventos (p. ej., m-pai + any_phys_dx TRUE = 3 eventos), lo que indica inestabilidad potencial en esos subgrupos."
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list(Tabla = tabla, Notas = notes),
           file.path(out_dir, "audit_full_sample_consolidated_ama_con_notas.xlsx"))

md <- paste0(
  "# Tabla consolidada de auditoría multiestado — muestra completa\n\n",
  "**Fuente:** cohorte SISTRAT 2010-2022, N = 88 152 (5 imputaciones).\n\n",
  "| Transición | Auditoría | plan_type_strata | Variable de subgrupo | Subgrupo | n en riesgo | n eventos | Tasa (%) |\n",
  "|---|---|---|---|---:|---:|---:|---:|\n"
)

for (i in seq_len(nrow(tabla))) {
  r <- tabla[i, ]
  md <- paste0(md, "| ", paste(c(
    r$transition, r$audit, r$plan_type_strata,
    ifelse(is.na(r$subgroup_variable), "—", r$subgroup_variable),
    ifelse(is.na(r$subgroup_value), "—", r$subgroup_value),
    format(r$n_at_risk, big.mark = ",", trim = TRUE),
    format(r$n_events, big.mark = ",", trim = TRUE),
    r$event_rate_pct
  ), collapse = " | "), " |\n")
}

md <- paste0(md, "\n## Notas\n\n")
for (i in seq_len(nrow(notes))) {
  md <- paste0(md, "**", notes$Nota[i], ".** ", notes$Texto[i], "\n\n")
}

writeLines(md, file.path(out_dir, "audit_full_sample_consolidated_ama_con_notas.md"))

cat("Saved:\n")
cat("  ", file.path(out_dir, "audit_full_sample_consolidated_ama_con_notas.xlsx"), "\n")
cat("  ", file.path(out_dir, "audit_full_sample_consolidated_ama_con_notas.md"), "\n")
