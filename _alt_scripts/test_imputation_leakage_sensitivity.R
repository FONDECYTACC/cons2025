# =============================================================================
# test_imputation_leakage_sensitivity.R
# Failable structural tests for the leakage and model-skill helper.
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
specs <- leakage_model_skill_specs(inputs)
stopifnot(identical(
  names(specs),
  c("mortality_full_ph", "mortality_shap_rule2", "readmission")
))

for (spec in specs) {
  marginal_terms <- attr(
    stats::terms(leakage_make_null_formula(spec$formula, "marginal")),
    "term.labels"
  )
  structure_terms <- attr(
    stats::terms(leakage_make_null_formula(spec$formula, "structure")),
    "term.labels"
  )
  stopifnot(!length(marginal_terms))
  stopifnot(!length(structure_terms) | all(grepl("^strata\\(", structure_terms)))
}

set.seed(2125L)
n <- 400L
n_train <- 1000L
eval_times <- c(3, 6, 12, 36, 60)
latent <- stats::rnorm(n)
death_time <- pmin(stats::rexp(n, rate = exp(0.7 * latent) / 80), 72)
death_event <- as.integer(death_time < 72)
readmit_time_raw <- pmin(stats::rexp(n, rate = exp(0.5 * latent) / 28), 72)
readmit_event_raw <- as.integer(readmit_time_raw < 72)
readmit_before_death <- readmit_event_raw == 1L &
  (death_event == 0L | readmit_time_raw < death_time)
readmit_time <- pmin(readmit_time_raw, death_time)
readmit_event <- as.integer(readmit_before_death)

train_death_time <- pmin(stats::rexp(n_train, rate = 1 / 80), 72)
train_death_event <- as.integer(train_death_time < 72)
train_readmit_time <- pmin(stats::rexp(n_train, rate = 1 / 28), 72)
train_readmit_event <- as.integer(train_readmit_time < 72)

censoring_fit_test <- leakage_censoring_fit(train_death_time, train_death_event)
unordered_times <- c(4.5, 1.5, 3.5, 1.5, 12, 0.5)
vector_lookup <- leakage_censoring_survival(censoring_fit_test, unordered_times)
scalar_lookup <- vapply(unordered_times, function(time) {
  leakage_censoring_survival(censoring_fit_test, time)
}, numeric(1))
stopifnot(isTRUE(all.equal(vector_lookup, scalar_lookup, tolerance = 0)))

make_risk <- function(scale, baseline) {
  outer(stats::plogis(scale * latent + baseline), eval_times / max(eval_times))
}
make_prediction_set <- function(scale, baseline) {
  model <- make_risk(scale, baseline)
  marginal <- matrix(
    rep(colMeans(model), each = n),
    nrow = n,
    ncol = length(eval_times)
  )
  structure <- pmin(pmax(marginal + 0.01 * as.numeric(latent > 0), 0), 1)
  list(model = model, marginal_null = marginal, structure_null = structure)
}

synthetic_specs <- list(
  mortality_full_ph = list(
    label = "Mortality Full PH", outcome = "death", registry_model = "best_perf1",
    formula = survival::Surv(time, event) ~ x + strata(group),
    estimand = "all-cause mortality risk"
  ),
  mortality_shap_rule2 = list(
    label = "Mortality SHAP rule2", outcome = "death", registry_model = "best_perf2",
    formula = survival::Surv(time, event) ~ x + strata(group2),
    estimand = "all-cause mortality risk"
  ),
  readmission = list(
    label = "Readmission shared model", outcome = "readmit", registry_model = "best_perf1",
    formula = survival::Surv(time, event) ~ x + strata(group),
    estimand = "cause-specific net readmission risk"
  )
)

synthetic_counterfactual <- list(
  model_skill = list(
    predictions = list(
      mortality_full_ph = make_prediction_set(0.8, -3.5),
      mortality_shap_rule2 = make_prediction_set(0.65, -3.6),
      readmission = make_prediction_set(0.55, -2.0)
    ),
    specs = synthetic_specs,
    outcomes = list(
      death = list(
        time = death_time, event = death_event,
        train_time = train_death_time, train_event = train_death_event
      ),
      readmit = list(
        time = readmit_time, event = readmit_event,
        train_time = train_readmit_time, train_event = train_readmit_event
      )
    ),
    eval_times = eval_times,
    scenario = "synthetic_clean_test"
  )
)

first_event <- leakage_readmission_first_event(synthetic_counterfactual$model_skill)
aj_risk <- leakage_aj_readmission_risk(first_event$time, first_event$status, 36)
stopifnot(is.finite(aj_risk), aj_risk >= 0, aj_risk <= 1)

point <- leakage_model_skill_point(synthetic_counterfactual)
bootstrap <- leakage_model_skill_bootstrap(
  synthetic_counterfactual,
  b = 20L,
  seed = 62125L
)
stopifnot(nrow(point) == 150L, nrow(bootstrap) == 150L)
stopifnot(all(is.finite(point$estimate)))
stopifnot(all(bootstrap$b_valid == 20L))

consistency <- merge(
  point[, c("model_id", "null_type", "horizon", "threshold", "metric", "estimate")],
  bootstrap[, c("model_id", "null_type", "horizon", "threshold", "metric", "estimate")],
  by = c("model_id", "null_type", "horizon", "threshold", "metric"),
  suffixes = c("_point", "_bootstrap")
)
stopifnot(nrow(consistency) == nrow(point))
stopifnot(max(
  abs(consistency$estimate_point - consistency$estimate_bootstrap),
  na.rm = TRUE
) < 1e-10)

cat("All leakage helper structural tests passed.\n")
cat(sprintf(
  "Elapsed: %.3f minutes\n",
  as.numeric(difftime(Sys.time(), .t0, units = "mins"))
))
