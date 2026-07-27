# =============================================================================
# smoke_imputation_leakage_confirmatory_path.R
# One-imputation execution test for the confirmatory notebook path.
# No scientific result files are written by this smoke test.
# =============================================================================

.t0 <- Sys.time()

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
helper_path <- file.path(
  project_root,
  "cons", "_alt_scripts", "imputation_leakage_sensitivity.R"
)
stopifnot(file.exists(helper_path))
source(helper_path)

inputs <- leakage_load_inputs(project_root)
leakage_source_project_engines(project_root)

config <- list(
  run_mode = "smoke",
  seed = 2125L,
  permutation_seed = 92125L,
  n_imputations = 1L,
  pmm_k = 15L,
  num_trees = 20L,
  maxiter = 2L,
  predict_iter = 2L,
  num_threads = max(1L, parallel::detectCores(logical = TRUE) - 2L),
  verbose = 1L,
  eval_times = c(3, 6, 12, 36, 60),
  n_permutations = 2L
)

counterfactual <- leakage_counterfactual(inputs, config)
point <- leakage_model_skill_point(counterfactual)
permutation <- leakage_permutation_test(counterfactual)
bootstrap <- leakage_model_skill_bootstrap(
  counterfactual,
  b = 20L,
  seed = 42125L
)

stopifnot(nrow(point) == 150L, nrow(bootstrap) == 150L)
stopifnot(all(is.finite(point$estimate)))
stopifnot(all(bootstrap$b_valid >= 19L))
stopifnot(nrow(permutation) > 0L, all(permutation$seed_matched))
stopifnot(identical(
  names(counterfactual$model_skill$predictions),
  c("mortality_full_ph", "mortality_shap_rule2", "readmission")
))
stopifnot(nrow(counterfactual$model_skill$strata_event_audit) > 0L)

reproduction <- subset(
  counterfactual$imputation_log,
  quantity == "max_abs_original_prediction_reproduction_error"
)
negative_control <- subset(
  counterfactual$imputation_log,
  quantity == "max_abs_complete_row_LL_LC_negative_control_error"
)
stopifnot(nrow(reproduction) == 2L, max(reproduction$value) < 1e-8)
stopifnot(nrow(negative_control) == 2L, max(negative_control$value) < 1e-10)

cat("Real-data confirmatory-path smoke test passed.\n")
cat(sprintf(
  "Elapsed: %.3f minutes\n",
  as.numeric(difftime(Sys.time(), .t0, units = "mins"))
))
