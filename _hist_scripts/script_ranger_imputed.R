
wdpath<- paste0(gsub("/cons","",gsub("cons","",paste0(getwd(),"/cons"))))

output_dir <- file.path(wdpath, "data/20241015_out/pred1")

#https://apps.abacus.ai/chatllm/?appId=89038b2c2&convoId=167b8b92eb

target_cols <- c(
  "readmit_time_from_disch_m", "readmit_event",
  "death_time_from_disch_m", "death_event"
)
leak_time_cols <- c("readmit_time_from_adm_m", "death_time_from_adm_m")
cols_to_exclude <- c(target_cols, leak_time_cols)

preds_formula<- 
  paste(setdiff(names(processed_datasets[[1]]), cols_to_exclude), collapse="+ ")

# 1. Readmission ---------------------------------------------------------------

# CRITICAL CHANGE 1: Switch to impurity importance
# Permutation is O(Trees × Vars × N) - can take DAYS with large data
# Impurity is O(Trees × Vars) - 10-100x faster

# CRITICAL CHANGE 2: Fit and SAVE models one at a time
# This prevents memory accumulation

library(ranger)
library(survival)

# Create directory for saving models
dir.create("ranger_models", showWarnings = FALSE)

ranger_models <- list()

for (i in seq_along(processed_datasets)) {
  cat("\n========================================\n")
  cat("Fitting model on imputation", i, "of", length(processed_datasets), "\n")
  cat("========================================\n")
  
  # Force garbage collection
  gc(full = TRUE)
  
  imp_data <- processed_datasets[[i]]
  
  # Fit model with OPTIMIZED settings
  r_fit <- ranger::ranger(
    formula = as.formula(paste("Surv(readmit_time_from_disch_m, readmit_event)", 
                               preds_formula, sep = " ~ ")),
    data = imp_data,
    
    # OPTIMIZATION 1: Use impurity instead of permutation (100x faster)
    importance = "impurity",  # CRITICAL CHANGE
    
    # OPTIMIZATION 2: Reduce trees (50-100 is often sufficient)
    num.trees = 50,  # Was 100 - try 50 first
    
    # OPTIMIZATION 3: Increase min.node.size (less deep trees = less memory)
    min.node.size = 30,  # Was 10 - larger = less memory
    
    # OPTIMIZATION 4: Subsample data per tree (reduce memory)
    sample.fraction = 0.632,  # Default but explicit - can reduce to 0.5
    
    # OPTIMIZATION 5: Reduce mtry (fewer variables tried per split)
    mtry = max(5, floor(sqrt(length(setdiff(names(imp_data), cols_to_exclude))) / 2)),
    
    splitrule = "logrank",
    verbose = TRUE,
    seed = 2125 + i,  # Different seed per imputation
    save.memory = TRUE,
    
    # OPTIMIZATION 6: Don't save forest if you only need predictions/importance
    write.forest = TRUE  # Set FALSE if you don't need predictions later
  )
  
  # SAVE MODEL IMMEDIATELY to disk (free memory)
  saveRDS(r_fit, file = paste0(output_dir,"/ranger_imp_", i, ".rds"))
  
  # Extract key results before clearing
  results <- list(
    prediction_error = r_fit$prediction.error,
    variable_importance = r_fit$variable.importance,
    num_samples = r_fit$num.samples,
    imputation = i
  )
  
  ranger_models[[i]] <- results
  
  # Clear the full model from memory
  rm(r_fit)
  gc(full = TRUE)
  
  cat("Completed imputation", i, "- OOB Error:", results$prediction_error, "\n")
  cat("Prediction Error = 1 - Harrell's c-index = ", 1-results$prediction_error,"\n")
}

# 7. Pool variable importance across imputations
importance_pooled <- Reduce("+", lapply(ranger_models, function(m) m$variable_importance)) / length(ranger_models)
importance_pooled <- sort(importance_pooled, decreasing = TRUE)

# Save pooled results
saveRDS(list(
  pooled_importance = importance_pooled,
  individual_results = ranger_models
), "ranger_models/pooled_results.rds")

print(head(importance_pooled, 20))

#👉 C-index ≈ 0.61 (1 − 0.39) for readmission risk.
# Impurity favor categorical variables with many levels, and continuous variables with wide ranges.
# A C-index of 0.5 is random guessing. 0.61 indicates your model has predictive power, though it is modest.
# In complex clinical readmission data (which is notoriously noisy), 0.60–0.65 is actually quite common.
# The fact that any_violence (a binary variable) is #4 with a score of 92 is significant. It means Violence is 
# a very strong predictor, arguably stronger than the score suggests, because it managed to rank high despite 
# the mathematical bias against it.
# For continuous covariates concordance is equivalent to Kendall’s tau, and for logistic
# regression is is equivalent to the area under the ROC curve.
# Harrell’s c-index (See [8] p. 370 for the definition),
# [8] Harrell, Frank, Lee, Kerry & Mark, Daniel. Multivariable Prognostic Models: Issues in Developing Models, Evaluating Assumptions and Adequacy, 
# and Measuring and Reducing Errors. Statistics in Medicine, Vol 15 (1996), pp. 361-387 

# the ranger model doesn’t do anything to address the time varying coefficients. 
# This apparently is a challenge. In a 2011 paper [16], Hamad observes
# [16] Bou-Hamad, I.(2011). A review of survival trees Statistics Surveys, 5, 44-71. DOI:10.1214/09-SS047

# However, in the context of survival trees, a further difficulty arises when 
# time–varying effects are included. Hence, we feel that the interpretation of 
# covariate effects with tree ensembles in general is still mainly unsolved and 
# should attract future research.


# c(dit_m = 547.055345363214, adm_age_rec3 = 521.577921184936, 
#   porc_pobr = 495.316870591635, any_violence_1_domestic_violence_sex_abuse = 92.7550750969321, 
#   marital_status_rec_single = 91.7626368035874, ed_attainment_corr_2_completed_high_school_or_less = 90.6798521869041, 
#   adm_motive_spontaneous_consultation = 88.4708415373849, tenure_status_household_owner_transferred_dwellings_pays_dividends = 88.3696905246988, 
#   tenure_status_household_stays_temporarily_with_a_relative = 88.1260985823301, 
#   ethnicity = 87.8215312685467, sex_rec_woman = 87.009252899851, 
#   cohabitation_with_couple_children = 86.476916689255, plan_type_corr_pg_pai = 86.4020126601613, 
#   adm_motive_sanitary_sector = 85.5023405552245, cohabitation_family_of_origin = 85.4428732274852, 
#   prim_sub_freq_rec_2_2_6_days_wk = 83.8640880935122, polysubstance_strict = 82.7103186835034, 
#   sub_dep_icd10_status_drug_dependence = 82.1797465221701, occupation_condition_corr24_unemployed = 82.1526591413468, 
#   prim_sub_freq_rec_3_daily = 82.0257871130272, tenure_status_household_renting = 80.9557096266803, 
#   ed_attainment_corr_3_completed_primary_school_or_less = 80.7886963322741, 
#   urbanicity_cat_2_mixed = 79.4926499984571, dg_psiq_cie_10_dg = 78.4084436351908, 
#   first_sub_used_alcohol = 78.3521319987293, any_phys_dx = 77.2331754889688, 
#   eva_fisica_logro_intermedio = 77.2295766005766, eva_ocupacion_logro_intermedio = 77.0407249471586, 
#   eva_fam_logro_intermedio = 75.7168105996635, tipo_de_vivienda_rec2_other_unknown = 75.493727941547, 
#   tr_outcome_dropout = 75.1116879437475, dx_f6_personality = 74.469723296637, 
#   occupation_condition_corr24_inactive = 73.9304513693731, first_sub_used_marijuana = 73.9075396226479, 
#   eva_consumo_logro_intermedio = 73.7899802297212, marital_status_rec_separated_divorced_annulled_widowed = 73.1155891906167, 
#   plan_type_corr_pg_pr = 72.5736493961733, eva_relinterp_logro_intermedio = 72.3893263383333, 
#   eva_transgnorma_logro_intermedio = 72.283692169338, urbanicity_cat_1_rural = 71.649933666453, 
#   adm_motive_another_sud_facility_fonodrogas_senda_previene = 70.6542550416366, 
#   eva_sm_logro_intermedio = 70.2030499708239, primary_sub_mod_cocaine_paste = 69.8136595719615, 
#   dx_f3_mood = 68.6539032174407, dg_psiq_cie_10_instudy = 66.4140368328074, 
#   eva_ocupacion_logro_minimo = 66.12222946582, adm_motive_justice_sector = 66.0020552663613, 
#   evaluacindelprocesoteraputico_logro_intermedio = 65.1642008779454, 
#   eva_transgnorma_logro_minimo = 65.0820768317854, plan_type_corr_m_pr = 64.367287459337, 
#   eva_fisica_logro_minimo = 63.2293303300417, tr_outcome_referral = 62.3904490479267, 
#   cohabitation_alone = 62.3803879440426, eva_fam_logro_minimo = 61.9083568412307, 
#   primary_sub_mod_cocaine_powder = 61.356305532893, eva_consumo_logro_minimo = 60.9795433699767, 
#   tr_outcome_adm_discharge_rule_violation_undet = 59.9993766468321, 
#   primary_sub_mod_alcohol = 58.3211220596726, eva_relinterp_logro_minimo = 58.1012293787257, 
#   eva_sm_logro_minimo = 56.096615334271, evaluacindelprocesoteraputico_logro_minimo = 52.1140659148904, 
#   tr_outcome_completion = 50.6710062750096, dx_f_any_severe_mental = 49.5939331356094, 
#   first_sub_used_cocaine_powder = 45.5334490663535, plan_type_corr_m_pai = 41.8720616183182, 
#   first_sub_used_cocaine_paste = 41.6259290203876, primary_sub_mod_marijuana = 38.8983968266639, 
#   tenure_status_household_illegal_settlement = 21.8635340161718, 
#   tr_outcome_adm_discharge_adm_reasons = 19.5454947897168, national_foreign = 11.6664224871333, 
#   first_sub_used_tranquilizers_hypnotics = 7.78926409997715, first_sub_used_opioids = 2.3891917178751
# )

library(ggplot2)
library(tibble)

# 1. Convert your named vector to a data frame
imp_df <- tibble::enframe(importance_pooled, name = "Variable", value = "Importance")

# 2. Select top 30 for readability
top_vars <- head(imp_df, 30)

# 3. Create the plot
ggplot(top_vars, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # Flip to make labels readable
  labs(
    title = "Top 30 Predictors of Readmission\n(Pooled Impurity Importance)",
    #subtitle = "Note: Continuous variables (Age, Time,\nPoverty) are naturally favored\nby Impurity metrics",
    x = "",
    y = "Importance Score (Gini/LogRank)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10) # Adjust text size if labels are cut off
  )



# 2. Death ---------------------------------------------------------------

ranger_models_death <- list()

for (i in seq_along(processed_datasets)) {
  cat("\n========================================\n")
  cat("Fitting model on imputation", i, "of", length(processed_datasets), "\n")
  cat("========================================\n")
  
  # Force garbage collection
  gc(full = TRUE)
  
  imp_data_death <- processed_datasets[[i]]
  
  # Fit model with OPTIMIZED settings
  r_fit_death <- ranger::ranger(
    formula = as.formula(paste("Surv(death_time_from_disch_m, death_event)", 
                               preds_formula, sep = " ~ ")),
    data = imp_data_death,
    
    # OPTIMIZATION 1: Use impurity instead of permutation (100x faster)
    importance = "impurity",  # CRITICAL CHANGE
    
    # OPTIMIZATION 2: Reduce trees (50-100 is often sufficient)
    num.trees = 50,  # Was 100 - try 50 first
    
    # OPTIMIZATION 3: Increase min.node.size (less deep trees = less memory)
    min.node.size = 30,  # Was 10 - larger = less memory
    
    # OPTIMIZATION 4: Subsample data per tree (reduce memory)
    sample.fraction = 0.632,  # Default but explicit - can reduce to 0.5
    
    # OPTIMIZATION 5: Reduce mtry (fewer variables tried per split)
    mtry = max(5, floor(sqrt(length(setdiff(names(imp_data_death), cols_to_exclude))) / 2)),
    
    splitrule = "logrank",
    verbose = TRUE,
    seed = 2125 + i,  # Different seed per imputation
    save.memory = TRUE,
    
    # OPTIMIZATION 6: Don't save forest if you only need predictions/importance
    write.forest = TRUE  # Set FALSE if you don't need predictions later
  )
  
  # SAVE MODEL IMMEDIATELY to disk (free memory)
  saveRDS(r_fit_death, file = paste0(output_dir,"/ranger_imp_death_", i, ".rds"))
  
  # Extract key results before clearing
  results_death <- list(
    prediction_error = r_fit_death$prediction.error,
    variable_importance = r_fit_death$variable.importance,
    num_samples = r_fit_death$num.samples,
    imputation = i
  )
  
  ranger_models_death[[i]] <- results_death
  
  # Clear the full model from memory
  rm(r_fit_death)
  gc(full = TRUE)
  
  cat("Completed imputation", i, "- OOB Error:", results_death$prediction_error, "\n")
  cat("Prediction Error = 1 - Harrell's c-index = ", 1-results_death$prediction_error,"\n")
}

# 7. Pool variable importance across imputations
importance_pooled_death <- Reduce("+", lapply(ranger_models_death, function(m) m$variable_importance)) / length(ranger_models)
importance_pooled_death <- sort(importance_pooled_death, decreasing = TRUE)

# Save pooled results
saveRDS(list(
  pooled_importance = importance_pooled_death,
  individual_results = ranger_models_death
), "ranger_models/pooled_results_death.rds")

print(head(importance_pooled_death, 20))


library(ggplot2)
library(tibble)

# 1. Convert named vector to data frame
imp_death_df <- tibble::enframe(importance_pooled_death, name = "Variable", value = "Importance")

# 2. Select Top 20
top_vars_death <- head(imp_death_df, 20)

# 3. Plot
ggplot(top_vars_death, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "firebrick") + # Red for Death model
  coord_flip() +
  labs(
    title = "Top 20 Predictors of Mortality (Impurity Importance)",
    subtitle = "Age, Treatment Time, and Poverty are the dominant drivers",
    x = "",
    y = "Importance Score"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))
