rm(list = ls()); gc()

wdpath<-
paste0(gsub("/cons","",gsub("cons","",paste0(getwd(),"/cons"))))
envpath<-
if(!file.exists(paste0(wdpath,"renv_info.txt"))){gsub("G:","H:",wdpath)}else{wdpath}
load(paste0(envpath,"data/20241015_out/duplicates1_5.RData"))
#
if(!require(missRanger)){install.packages("missRanger")}
if(!require(mice)){install.packages("mice")}
if(!require(tidyverse)){install.packages("tidyverse")}

rm(list = setdiff(ls(), "SISTRAT23_c1_2010_2022_df_prev1f"))

k_folds<-10
results <- data.frame(fold = 1:k_folds, mae = NA, rmse = NA)
set.seed(2125)
seeds <- sample(1:10000, k_folds)  # Generar semillas rastreables
for (fold in 1:k_folds) {
  # Copiar el dataset original
  dataset_with_na <- SISTRAT23_c1_2010_2022_df_prev1f
  
  set.seed(seeds[fold])
  # Introducir valores faltantes en un 20% de los datos (diferente en cada iteración)
  missing_indices <- sample(1:nrow(dataset_with_na), size = floor(0.2 * nrow(dataset_with_na)))
  dataset_with_na$birth_date_rec_imp[missing_indices] <- NA
  
  # Imputar con kNN
  imputed_data <- missRanger(
    data = dataset_with_na |> 
      #valores irreales en 
      dplyr::mutate(numero_de_hijos= ifelse(numero_de_hijos>=10,NA_real_,numero_de_hijos))|>
      #, las ssaqué porque tenían mucho perdido, posiblemente porque eran hombres
      dplyr::mutate(se_trata_de_una_mujer_embarazada= ifelse(sexo=="hombre","no",se_trata_de_una_mujer_embarazada)),
    formula = birth_date_rec_imp ~ sexo+ tipo_centro + tipo_de_plan + 
      se_trata_de_una_mujer_embarazada + escolaridad_ultimo_ano_cursado +
      sustancia_principal + edad_inicio_sustancia_principal +
      edad_inicio_consumo + numero_de_hijos + estado_conyugal+ 
      TABLE_rec2+ numero_de_tratamientos_anteriores+ usuario_de_tribunales_tratamiento_drogas, 
    num.trees = 5e3,
    pmm.k = 3,  # Predictive mean matching
    keep_forests = F,
    returnOOB= T, 
    #mtry= function(p) max(3, trunc(p / 3)), # At least 3 or parameters/3, whichever is greater.
    maxiter= 5e2,
    verbose = 2,
    seed= seeds[fold],
    #case.weights = rowSums(!is.na(SISTRAT23_c1_2010_2022_df_prev1f)) #pass case weights to the imputation models. For instance, this allows to reduce the contribution of rows with many missings
  )
  
  # Calcular métricas (solo en los índices con datos faltantes imputados)
  real_values <- SISTRAT23_c1_2010_2022_df_prev1f$birth_date_rec_imp[missing_indices]
  imputed_values <- imputed_data$birth_date_rec_imp[missing_indices]
  
  mae <- mean(abs(real_values - imputed_values),na.rm=T)
  rmse <- sqrt(mean((real_values - imputed_values)^2,na.rm=T))
  
  # Guardar resultados
  results$mae[fold] <- mae
  results$rmse[fold] <- rmse
}
cat("Average MAE RF (in years):", mean(results$mae)/365.25, "\n")
cat("Average RMSE RF (in years):", mean(results$rmse)/365.25, "\n")
#Average MAE RF (in years): 8.396122 
#Average RMSE RF (in years): 10.887 

#without pais_nacimiento and tiene_menores_de_edad_a_cargo
#Average MAE RF (in years): 8.649611 
#Average RMSE RF (in years): 11.15144 


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

results2 <- data.frame(fold = 1:k_folds, mae = NA, rmse = NA)

for (fold in 1:k_folds) {
  # Copiar el dataset original
  dataset_with_na <- SISTRAT23_c1_2010_2022_df_prev1f[, 
    c("birth_date_rec_imp", 
      "sexo", 
      "tipo_centro", 
      "tipo_de_plan", 
      "se_trata_de_una_mujer_embarazada", 
      "escolaridad_ultimo_ano_cursado", 
      "sustancia_principal", 
      "edad_inicio_sustancia_principal", 
      "edad_inicio_consumo", 
      "numero_de_hijos", 
      "estado_conyugal", 
      "TABLE_rec2", 
      "numero_de_tratamientos_anteriores", 
      "usuario_de_tribunales_tratamiento_drogas")] |> 
  as.data.frame()|> 
    #dplyr::mutate(tiene_menores_de_edad_a_cargo= ifelse(sexo=="hombre","no",tiene_menores_de_edad_a_cargo))|>
    dplyr::mutate(se_trata_de_una_mujer_embarazada= ifelse(sexo=="hombre","no",se_trata_de_una_mujer_embarazada))#, las ssaqué porque tenían mucho perdido, posiblemente porque eran hombres
  
  
  categorical_vars <- c("sexo", 
                        "tipo_centro", 
                        "tipo_de_plan", 
                        "se_trata_de_una_mujer_embarazada", 
                        "escolaridad_ultimo_ano_cursado", 
                        "sustancia_principal", 
                        "estado_conyugal", 
                        "usuario_de_tribunales_tratamiento_drogas")
  
  dataset_with_na[categorical_vars] <- lapply(
    dataset_with_na[categorical_vars], 
    as.factor
  )
  
  set.seed(seeds[fold])
  # Introducir valores faltantes en un 20% de los datos (diferente en cada iteración)
  missing_indices <- sample(1:nrow(dataset_with_na), size = floor(0.2 * nrow(dataset_with_na)))
  dataset_with_na$birth_date_rec_imp[missing_indices] <- NA
  
  set.seed(seeds[fold])
  # Imputar con kNN
  imputed_data <- VIM::kNN(dataset_with_na, variable = c("birth_date_rec_imp"), 
        dist_var=c("sexo", 
                   "tipo_centro", 
                   "tipo_de_plan", 
                   "se_trata_de_una_mujer_embarazada", 
                   "escolaridad_ultimo_ano_cursado", 
                   "sustancia_principal", 
                   "edad_inicio_sustancia_principal", 
                   "edad_inicio_consumo", 
                   "numero_de_hijos", 
                   "estado_conyugal", 
                   "TABLE_rec2", 
                   "numero_de_tratamientos_anteriores", 
                   "usuario_de_tribunales_tratamiento_drogas"),
                           numFun = "mean", 
                           k=3,
                           trace=T)
  
  # Calcular métricas (solo en los índices con datos faltantes imputados)
  real_values <- SISTRAT23_c1_2010_2022_df_prev1f$birth_date_rec_imp[missing_indices]
  imputed_values <- imputed_data$birth_date_rec_imp[missing_indices]
  
  mae <- mean(abs(real_values - imputed_values),na.rm=T)
  rmse <- sqrt(mean((real_values - imputed_values)^2,na.rm=T))
  
  # Guardar resultados
  results2$mae[fold] <- mae
  results2$rmse[fold] <- rmse
}
invisible("Tomó 12 horas en mi DELL!")
cat("Average MAE kNN (in years):", mean(results2$mae)/365.25, "\n")
cat("Average RMSE kNN (in years):", mean(results2$rmse)/365.25, "\n")
# > cat("Average MAE kNN (in years):", mean(results2$mae)/365.25, "\n")
# Average MAE kNN (in years): 6.823537 
# > cat("Average RMSE kNN (in years):", mean(results2$rmse)/365.25, "\n")
# Average RMSE kNN (in years): 8.924294 #


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


results3 <- data.frame(fold = 1:k_folds, mae = NA, rmse = NA)

for (fold in 1:k_folds) {
  # Copiar el dataset original
  dataset_with_na <- SISTRAT23_c1_2010_2022_df_prev1f[, c("birth_date_rec_imp", 
      "sexo", 
      "tipo_centro", 
      "tipo_de_plan", 
      "se_trata_de_una_mujer_embarazada", 
      "escolaridad_ultimo_ano_cursado", 
      "sustancia_principal", 
      "edad_inicio_sustancia_principal", 
      "tiene_menores_de_edad_a_cargo", 
      "edad_inicio_consumo", 
      "numero_de_hijos", 
      "estado_conyugal", 
      "TABLE_rec2", 
      "numero_de_tratamientos_anteriores", 
      "usuario_de_tribunales_tratamiento_drogas")] |> 
    as.data.frame()|> 
    #valores irreales en 
    dplyr::mutate(numero_de_hijos= ifelse(numero_de_hijos>=10,NA_real_,numero_de_hijos))|>
    #, las ssaqué porque tenían mucho perdido, posiblemente porque eran hombres
    dplyr::mutate(se_trata_de_una_mujer_embarazada= ifelse(sexo=="hombre","no",se_trata_de_una_mujer_embarazada))
  
  set.seed(seeds[fold])
  # Introducir valores faltantes en un 20% de los datos (diferente en cada iteración)
  missing_indices <- sample(1:nrow(dataset_with_na), size = floor(0.2 * nrow(dataset_with_na)))
  dataset_with_na$birth_date_rec_imp[missing_indices] <- NA
  
  set.seed(seeds[fold])
  # Imputar con kNN
  imputed_data_m <- mice(dataset_with_na[,c("birth_date_rec_imp", "sexo", "tipo_centro", "tipo_de_plan", "pais_nacimiento","se_trata_de_una_mujer_embarazada", "escolaridad_ultimo_ano_cursado", "sustancia_principal", "edad_inicio_sustancia_principal", "tiene_menores_de_edad_a_cargo", "edad_inicio_consumo","numero_de_hijos", "estado_conyugal", "TABLE_rec2", "numero_de_tratamientos_anteriores", "usuario_de_tribunales_tratamiento_drogas")], m=10, seed = 2125, maxit = 5e1)
  
  #tiene_menores_de_edad_a_cargo, sólo perdido de 2015 pa atrás
  #  numero_de_hijos <11, el resto están malos
  #  usuario_de_tribunales_tratamiento_drogas, tiene más NAs en 2016
  # pais de nacimiento se comienza a preguntar del 2016
  # se_trata_de_una_mujer_embarazada, mucho perdido, pero se distribuye en todas las bases de datos anuales igual
  
  imputed_val <- matrix(NA, nrow = nrow(SISTRAT23_c1_2010_2022_df_prev1f), ncol = 10)
  for (i in 1:10) {
    imputed_data <- mice::complete(imputed_data_m, i) # Extraer el conjunto imputado i
    imputed_val[, i] <- imputed_data$birth_date_rec_imp
  }
  
  # Calcular el promedio de las imputaciones por fila
  row_means <- rowMeans(imputed_val, na.rm = TRUE)
  
  # Calcular métricas (solo en los índices con datos faltantes imputados)
  real_values <- SISTRAT23_c1_2010_2022_df_prev1f$birth_date_rec_imp[missing_indices]
  imputed_values <- row_means[missing_indices]
  
  mae <- mean(abs(real_values - imputed_values),na.rm=T)
  rmse <- sqrt(mean((real_values - imputed_values)^2,na.rm=T))
  
  # Guardar resultados
  results3$mae[fold] <- mae
  results3$rmse[fold] <- rmse
}
invisible("Tomó 2 horas en mi DELL!")
cat("Average MAE MICE 50 iter (in years):", mean(results3$mae)/365.25, "\n")
cat("Average RMSE MICE 50 iter (in years):", mean(results3$rmse)/365.25, "\n")
#Average MAE MICE 50 iter (in years): 7.906666 
#Average RMSE MICE 50 iter (in years): 10.16269 

##_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#__#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
library(caret)

# Select relevant variables
SISTRAT23_c1_2010_2022_df_prev1f_for_imp <- SISTRAT23_c1_2010_2022_df_prev1f %>%
  dplyr::select(
    sexo, tipo_centro, tipo_de_plan, #pais_nacimiento, 
    escolaridad_ultimo_ano_cursado, 
    sustancia_principal, edad_inicio_sustancia_principal, 
    edad_inicio_consumo, 
    numero_de_hijos, estado_conyugal, birth_date_rec_imp,
    se_trata_de_una_mujer_embarazada, tiene_menores_de_edad_a_cargo,
    TABLE_rec2, numero_de_tratamientos_anteriores, usuario_de_tribunales_tratamiento_drogas 
  ) |> 
  #dplyr::mutate(tiene_menores_de_edad_a_cargo= ifelse(sexo=="hombre","no",tiene_menores_de_edad_a_cargo))|>
  dplyr::mutate(se_trata_de_una_mujer_embarazada= ifelse(sexo=="hombre","no",se_trata_de_una_mujer_embarazada))#, las ssaqué porque tenían mucho perdido, posiblemente porque eran hombres

# Remove NAs
SISTRAT23_c1_2010_2022_df_prev1f_for_imp <- na.omit(SISTRAT23_c1_2010_2022_df_prev1f_for_imp)

# Encode categorical variables
dummy_model <- dummyVars(" ~ .", data = SISTRAT23_c1_2010_2022_df_prev1f_for_imp)
data_prepared <- predict(dummy_model, newdata = SISTRAT23_c1_2010_2022_df_prev1f_for_imp)

# Scale numerical variables
data_scaled <- as.data.frame(scale(data_prepared))

# Define predictor matrix (X) and target (Y)
X <- data_scaled[, !colnames(data_scaled) %in% "birth_date_rec_imp"]
Y <- data_scaled[, "birth_date_rec_imp"]

# Create a custom function for RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Define number of folds
K <- 10
folds <- createFolds(Y, k = K)

# Perform cross-validation
results <- data.frame(fold = numeric(K), RMSE = numeric(K))
for (i in seq_along(folds)) {
  train_idx <- unlist(folds[-i])
  test_idx <- unlist(folds[i])
  
  X_train <- X[train_idx, ]
  Y_train <- Y[train_idx]
  X_test <- X[test_idx, ]
  Y_test <- Y[test_idx]
  
  # Train neural network
  nn_model <- nnet::nnet(
    x = X_train, y = Y_train,
    size = 5, decay = 0.1, linout = TRUE, maxit = 200, trace = FALSE
  )
  
  # Predict on test set
  predictions <- predict(nn_model, X_test)
  
  # Calculate RMSE
  rmse <- sqrt(mean((predictions - Y_test)^2))
  results[i, ] <- c(i, rmse)
}

# Average RMSE across folds
print(results)
cat("Average RMSE:", round(mean(results$RMSE),2), "\n")
#Average RMSE: 0.7159635 
#  neural network is, on average, only about 0.72 days off from the true birth date
#~30 min
# 
invisible("Its very similar to missRanger")
invisible("But its not that easy to use because of na.omit, requires complete information on X vector")

