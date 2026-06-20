# =============================================================================
# characterize_profile_ref.R
# Describe el sujeto de referencia (profile_ref) del nomograma multistate.
# Correr DESPUES de construir profile_ref. Depende de: dat_ms, profile_ref,
# y (opcional) covs_readmit, covs_death, strata_readmit, strata_death.
#
# profile_ref = mediana (numericas) + moda (categoricas), variable por variable.
# Para dummies 0/1 la "mediana" cae en la categoria de referencia (la omitida)
# salvo que la categoria tenga >50% de prevalencia -> revisa la columna prev.
# =============================================================================

stopifnot(exists("profile_ref"), exists("dat_ms"))

.describe_one <- function(v, ref_val, data) {
  x <- data[[v]]
  if (is.numeric(x)) {
    u <- sort(unique(x[!is.na(x)]))
    is_dummy <- length(u) <= 2 && all(u %in% c(0, 1))
    if (is_dummy) {
      prev <- 100 * mean(x == 1, na.rm = TRUE)
      data.frame(variable = v, tipo = "dummy 0/1",
                 valor_ref = as.character(ref_val),
                 detalle = sprintf("prev(=1)=%.1f%%%s", prev,
                                   if (prev > 40 && prev < 60) "  <-- BORDERLINE" else ""),
                 stringsAsFactors = FALSE)
    } else {
      qs <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
      data.frame(variable = v, tipo = "continua",
                 valor_ref = sprintf("%.4g", ref_val),
                 detalle = sprintf("mediana | IQR [%.4g, %.4g]", qs[1], qs[2]),
                 stringsAsFactors = FALSE)
    }
  } else {
    tab <- sort(prop.table(table(x)), decreasing = TRUE)
    data.frame(variable = v, tipo = "categorica",
               valor_ref = as.character(ref_val),
               detalle = sprintf("moda=%s (%.1f%%); niveles: %s",
                                 names(tab)[1], 100 * tab[1],
                                 paste(names(tab), collapse = ", ")),
               stringsAsFactors = FALSE)
  }
}

ref_vars <- names(profile_ref)
ref_long <- do.call(rbind, lapply(ref_vars,
                    function(v) .describe_one(v, profile_ref[[v]], dat_ms)))

cat(sprintf("\n== Sujeto de referencia | n=%d (dat_ms) | %d covariables ==\n\n",
            nrow(dat_ms), length(ref_vars)))
print(ref_long, row.names = FALSE, right = FALSE)

# ---- Estratos: definen el baseline hazard de las curvas de los plots ----
if (exists("strata_readmit") || exists("strata_death")) {
  sv <- intersect(unique(c(if (exists("strata_readmit")) strata_readmit,
                           if (exists("strata_death"))   strata_death)),
                  names(profile_ref))
  if (length(sv)) {
    cat("\n== Estratos de la referencia (baseline hazard de los plots) ==\n")
    print(t(profile_ref[, sv, drop = FALSE]), quote = FALSE)
  }
}

# ---- Opcional: exportar a CSV para usar como Tabla / caption ----
# utils::write.csv(ref_long,
#   file.path(project_root, "cons", "_out", "profile_ref_caracterizacion.csv"),
#   row.names = FALSE)
