# Debug script para diagnosticar el problema

# 1. Primero verificar que la función carga correctamente
source("cph_evaluate_boot_oob_mi_corrected.R")

# 2. Verificar que los helpers internos existen (deberían, al estar anidados)
cat("Función cargada correctamente\n")

# 3. Crear un estado de prueba mínimo
set.seed(2125)

# Usar solo la primera imputación
M <- 1
df_list <- py_corrected_datasets[1]
n <- nrow(df_list[[1]])

# Preparar datos como lo hace la función principal
df_list <- lapply(df_list, function(d) {
    d <- as.data.frame(d)
    d[["readmit_time_from_disch_m"]] <- as.numeric(d[["readmit_time_from_disch_m"]])
    d[["readmit_event"]] <- as.integer(as.character(d[["readmit_event"]]))
    d
})

# Estado simplificado
state <- list(
    M = 1,
    n = n,
    min_oob_n = 10,
    min_oob_events = 3,
    tau = 99,
    time_col = "readmit_time_from_disch_m",
    event_col = "readmit_event",
    event_indicator = list(df_list[[1]][["readmit_event"]]),
    df_list = df_list,
    categorical_formula_vars = c("plan_type_strata", "tr_outcome_adm_discharge_rule_violation_undet"),
    model_formula = as.formula("Surv(readmit_time_from_disch_m, readmit_event) ~ primary_sub_mod_cocaine_paste + strata(plan_type_strata) + strata(tr_outcome_adm_discharge_rule_violation_undet)"),
    eval_times = seq(1e-6, 99, length.out = 10),
    verbose = TRUE
)

# 4. Intentar ejecutar el worker manualmente (sin parallel)
cat("\n=== Probando worker manualmente ===\n")

# Primero verificar que los helpers están disponibles
cat("to01 existe:", exists("to01"), "\n")
cat("ibs_ipcw_train existe:", exists("ibs_ipcw_train"), "\n")

tryCatch({
    # Simular una iteración bootstrap
    idx_train <- sample.int(n = state$n, size = state$n, replace = TRUE)
    idx_test <- setdiff(seq_len(state$n), unique(idx_train))
    
    cat("OOB sample size:", length(idx_test), "\n")
    
    # Intentar ajustar el modelo directamente
    df_train <- state$df_list[[1]][idx_train, , drop = FALSE]
    df_test <- state$df_list[[1]][idx_test, , drop = FALSE]
    
    cat("Train rows:", nrow(df_train), "Test rows:", nrow(df_test), "\n")
    
    # Verificar si hay eventos
    oob_events <- sum(state$event_indicator[[1]][idx_test] == 1L, na.rm = TRUE)
    cat("OOB events:", oob_events, "\n")
    
    # Intentar ajustar cph
    cat("\nIntentando ajustar rms::cph...\n")
    fit <- rms::cph(
        state$model_formula,
        data = df_train,
        x = TRUE,
        y = TRUE,
        surv = TRUE,
        se.fit = FALSE
    )
    cat("Modelo ajustado exitosamente!\n")
    
    # Intentar predictSurvProb
    cat("\nIntentando pec::predictSurvProb...\n")
    surv_mat <- pec::predictSurvProb(fit, newdata = df_test, times = state$eval_times)
    cat("Predictions shape:", dim(surv_mat), "\n")
    
    # Intentar calcular IBS
    cat("\nIntentando calcular IBS...\n")
    ibs_val <- ibs_ipcw_train(
        df_train = df_train,
        df_test = df_test,
        surv_mat = surv_mat,
        times = state$eval_times,
        time_col = state$time_col,
        event_col = state$event_col
    )
    cat("IBS calculado:", ibs_val, "\n")
    
}, error = function(e) {
    cat("ERROR capturado:\n")
    cat(conditionMessage(e), "\n")
    cat("\nTraceback:\n")
    traceback()
})
