source("cph_evaluate_boot_oob_mi_corrected.R")

run_test <- function(name, expr) {
    force(name)
    force(expr)
    cat(sprintf("TEST %s ... ", name))
    eval.parent(substitute(expr))
    cat("OK\n")
}

expect_error_contains <- function(expr, pattern) {
    msg <- tryCatch(
        {
            force(expr)
            NULL
        },
        error = function(e) conditionMessage(e)
    )
    if (is.null(msg)) {
        stop(sprintf("expected an error containing '%s' but no error was raised", pattern))
    }
    if (!grepl(pattern, msg, fixed = TRUE)) {
        stop(sprintf("expected an error containing '%s' but got '%s'", pattern, msg))
    }
    invisible(msg)
}

make_imputation <- function(seed_shift = 0L, n = 160L) {
    set.seed(20260318L + seed_shift)
    age <- rnorm(n, mean = 45, sd = 11)
    urbanicity_cat <- sample(0:2, size = n, replace = TRUE, prob = c(0.45, 0.35, 0.20))
    linpred <- 0.02 * (age - 45) + 0.18 * urbanicity_cat
    time <- pmax(rexp(n, rate = exp(linpred) / 14), 0.05)
    event <- rbinom(n, size = 1, prob = 0.72)
    data.frame(
        time = time,
        event = event,
        age = age,
        urbanicity_cat = urbanicity_cat
    )
}

make_rare_level_imputation <- function(seed_shift = 0L, n = 30L) {
    set.seed(20260401L + seed_shift)
    age <- rnorm(n, mean = 47, sd = 9)
    urbanicity_cat <- rep(0L, n)
    urbanicity_cat[1:4] <- 1L
    urbanicity_cat[5] <- 2L
    linpred <- 0.02 * (age - 47) + 0.30 * urbanicity_cat
    time <- pmax(rexp(n, rate = exp(linpred) / 10), 0.05)
    event <- rbinom(n, size = 1, prob = 0.78)
    data.frame(
        time = time,
        event = event,
        age = age,
        urbanicity_cat = urbanicity_cat
    )
}

base_imputations <- lapply(0:2, make_imputation)
base_tau <- unname(stats::quantile(base_imputations[[1]]$time, probs = 0.75))

run_test("numeric ordinal stays numeric", {
    res <- cph_evaluate_boot_oob_mi_corrected(
        formula = survival::Surv(time, event) ~ age + urbanicity_cat,
        imputed_list = base_imputations,
        time_col = "time",
        event_col = "event",
        tau = base_tau,
        B = 6,
        n_eval_times = 20,
        seed = 2125,
        cpus = 1,
        verbose = FALSE,
        min_valid_imputations = 2
    )
    stopifnot(isTRUE(res$reproducibility$formula_validated))
    stopifnot(!("urbanicity_cat" %in% res$reproducibility$categorical_formula_vars))
    stopifnot(length(res$boot_pooled) >= 2L)
})

run_test("explicit factor sanitization succeeds", {
    res <- cph_evaluate_boot_oob_mi_corrected(
        formula = survival::Surv(time, event) ~ age + factor(urbanicity_cat),
        imputed_list = base_imputations,
        time_col = "time",
        event_col = "event",
        tau = base_tau,
        B = 6,
        n_eval_times = 20,
        seed = 2125,
        cpus = 1,
        verbose = FALSE,
        min_valid_imputations = 2
    )
    stopifnot("urbanicity_cat" %in% res$reproducibility$categorical_formula_vars)
    stopifnot(isTRUE(res$reproducibility$formula_validated))
})

run_test("parallel worker smoke test", {
    res <- cph_evaluate_boot_oob_mi_corrected(
        formula = survival::Surv(time, event) ~ age + urbanicity_cat,
        imputed_list = base_imputations,
        time_col = "time",
        event_col = "event",
        tau = base_tau,
        B = 2,
        n_eval_times = 20,
        seed = 2125,
        cpus = 2,
        verbose = FALSE,
        min_valid_imputations = 2
    )
    stopifnot(length(res$boot_pooled) >= 2L)
})

run_test("strat plus factor fails fast", {
    expect_error_contains(
        cph_evaluate_boot_oob_mi_corrected(
            formula = survival::Surv(time, event) ~ rms::strat(urbanicity_cat) + factor(urbanicity_cat),
            imputed_list = base_imputations,
            time_col = "time",
            event_col = "event",
            tau = base_tau,
            B = 3,
            n_eval_times = 10,
            seed = 2125,
            cpus = 1,
            verbose = FALSE
        ),
        "design_validation_error:variable 'urbanicity_cat' appears inside strat()/strata() and elsewhere in the formula"
    )
})

run_test("strat plus interaction fails fast", {
    expect_error_contains(
        cph_evaluate_boot_oob_mi_corrected(
            formula = survival::Surv(time, event) ~ rms::strat(urbanicity_cat) + rms::rcs(age, 4) * urbanicity_cat,
            imputed_list = base_imputations,
            time_col = "time",
            event_col = "event",
            tau = base_tau,
            B = 3,
            n_eval_times = 10,
            seed = 2125,
            cpus = 1,
            verbose = FALSE
        ),
        "design_validation_error:variable 'urbanicity_cat' appears inside strat()/strata() and elsewhere in the formula"
    )
})

run_test("cross-imputation categorical mismatch fails fast", {
    bad_imputations <- base_imputations
    bad_imputations[[2]]$urbanicity_cat[bad_imputations[[2]]$urbanicity_cat == 2L] <- 1L
    expect_error_contains(
        cph_evaluate_boot_oob_mi_corrected(
            formula = survival::Surv(time, event) ~ age + factor(urbanicity_cat),
            imputed_list = bad_imputations,
            time_col = "time",
            event_col = "event",
            tau = base_tau,
            B = 3,
            n_eval_times = 10,
            seed = 2125,
            cpus = 1,
            verbose = FALSE
        ),
        "design_validation_error:categorical formula variables have inconsistent observed levels across imputations"
    )
})

run_test("rare level can trigger unseen_factor_level skips", {
    rare_imputations <- lapply(0:2, make_rare_level_imputation)
    rare_tau <- unname(stats::quantile(rare_imputations[[1]]$time, probs = 0.75))
    res <- cph_evaluate_boot_oob_mi_corrected(
        formula = survival::Surv(time, event) ~ age + factor(urbanicity_cat),
        imputed_list = rare_imputations,
        time_col = "time",
        event_col = "event",
        tau = rare_tau,
        B = 20,
        n_eval_times = 15,
        seed = 2125,
        cpus = 1,
        verbose = FALSE,
        min_oob_n = 3,
        min_oob_events = 1,
        min_valid_imputations = 2
    )
    fail_values <- as.vector(res$fail_matrix)
    stopifnot(any(grepl("^unseen_factor_level:urbanicity_cat$", fail_values)))
})

cat("All tests passed.\n")
