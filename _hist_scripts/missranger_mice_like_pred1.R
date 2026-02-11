
library(dplyr)
library(purrr)
library(tibble)

# Run MICE imputation (corrected syntax)
time_event_vars <- c(
  "readmit_time_from_disch_m", "readmit_event",
  "death_time_from_disch_m", "death_event"
)
predictor_formula <- as.formula(
  #paste(". ~ . -", paste(time_event_vars, collapse = " - "))
  paste(". ~ . ", sep="")
)

cat("MICE-like but with missranger")
#https://cran.r-project.org/web/packages/missRanger/vignettes/working_with_censoring.html
set.seed(2125)
filled_datasets <- replicate(
  replicates_n,
  missRanger::missRanger(
    df_pred,
    formula = predictor_formula,  # Excluye explícitamente outcomes
    verbose = 2,
    pmm.k = 15,#10, # predictive mean matching with 15 nearest neighbors for num variables
    num.trees = 200, # trees sufficient for stability
    #seed      = 2125, #if you want 5 different but reproducible imputations. dont use insede missRanger
    maxiter = 10, #50 is overkill unless you have very heavy missingness and time isn’t an issue
    returnOOB = TRUE,
    respect.unordered.factors = "order" # controls how categorical variables are handled inside the trees, giving a better treatment of unordered factors.
  ),
  simplify = FALSE
)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
cat("Check remaining missing values\n")

imputed_data <- map_dfr(1:5, function(i) {
  df <- filled_datasets[[i]]
  # add control variables
  dplyr::mutate(df,
         .imp = i,  # imputation number
         .id = SISTRAT23_c1_2010_2024_df_model$original_row_id  # Use original ID
  ) %>%
    # Keep variables necessary for analysis
    dplyr::select(
      .imp, .id,
      everything()
    )
}, .id = "imputation_id") %>% 
  dplyr::select(-imputation_id)  # cleaning

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
cat("Check remaining missing values\n")

get_all_oobs <- function(imputed_list) {
  
  # 1. Extract OOBs from all 5 datasets into one big table
  all_oobs <- map_dfr(seq_along(imputed_list), function(i) {
    
    # Get the attribute
    oob_vec <- attr(imputed_list[[i]], "oob")
    
    # Create a tidy table
    tibble::tibble(
      Variable = names(oob_vec),
      Error_Value = as.numeric(oob_vec),
      Imputation_Run = as.factor(i)
    )
  })
  
  # 2. Create a Summary Table (Mean across the 5 runs)
  oob_summary <- all_oobs %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(
      Mean_OOB = mean(Error_Value),
      SD_OOB = sd(Error_Value),
      Best_Run = min(Error_Value),
      Worst_Run = max(Error_Value)
    ) %>%
    dplyr::arrange(Mean_OOB) # Sort: Best variables (lowest error) first
  
  # 3. Print the summary nicely
  cat("\n=== COMPLETE OOB ERROR REPORT ===\n")
  cat("Scale: 0.00 = Perfect Prediction | 1.00 = Useless/Random\n")
  print(as.data.frame(oob_summary), digits = 3)
  
  return(oob_summary)
}
