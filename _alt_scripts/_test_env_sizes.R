## Directly test which environment setup works with future + globals=FALSE
suppressPackageStartupMessages({
  library(future)
  library(future.apply)
  library(survival)
})

set.seed(1)
N <- 300
syn <- data.frame(
  age   = rnorm(N), sex = rbinom(N,1,.5), score = rnorm(N),
  death_time_from_disch_m = rexp(N, 1/24),
  death_event = rbinom(N,1,.4)
)
data_list <- list(syn, syn)
formula_  <- Surv(death_time_from_disch_m, death_event) ~ age + sex + score

make_worker <- function(env_label, env) {
  f <- function(task, data_list, folds_list, formula_, times_, n) {
    set.seed(task)
    idx  <- sample(n, floor(n * 0.8))
    test <- setdiff(seq_len(n), idx)
    fit  <- tryCatch(
      survival::coxph(formula_, data = data_list[[1L]][idx, , drop = FALSE],
                      x = TRUE, y = TRUE),
      error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    lp <- stats::predict(fit, newdata = data_list[[1L]][test, , drop = FALSE], type = "lp")
    H0 <- stats::approx(survival::basehaz(fit, centered=FALSE)$hazard,
                        xout = times_, rule = 2L)$y
    list(r = 1L, task = task, n_test = length(test))
  }
  environment(f) <- env
  sz <- as.numeric(object.size(serialize(f, NULL))) / 1e6
  cat(sprintf("%-30s serialized size: %.1f MB\n", env_label, sz))
  f
}

# Try each environment type
envs <- list(
  "full closure (default)"   = environment(),          # huge — control
  "new.env(parent=baseenv)"  = new.env(parent = baseenv()),
  "new.env(parent=globalenv)"= new.env(parent = globalenv()),
  "list2env(parent=baseenv)" = list2env(list(), parent = baseenv()),
  "emptyenv()"               = emptyenv()
)

cat("=== Serialized sizes ===\n")
workers <- lapply(names(envs), function(nm) make_worker(nm, envs[[nm]]))
names(workers) <- names(envs)

cat("\n=== Parallel dispatch test (globals=FALSE, 2 workers) ===\n")
plan(multisession, workers = 2L)

tasks  <- as.list(1:4)
folds_ <- list(list(list(1:150)))
times_ <- c(6, 12)

for (nm in names(workers)) {
  cat(sprintf("%-30s ... ", nm))
  flush.console()
  res <- tryCatch(
    withCallingHandlers(
      future.apply::future_lapply(
        tasks, workers[[nm]],
        data_list  = data_list,
        folds_list = folds_,
        formula_   = formula_,
        times_     = times_,
        n          = N,
        future.globals  = FALSE,
        future.packages = c("survival"),
        future.chunk.size = 2L
      ),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) list(ERROR = conditionMessage(e))
  )
  if (!is.null(res$ERROR)) {
    cat(sprintf("FAIL: %s\n", substr(res$ERROR, 1, 80)))
  } else if (length(res) == 4) {
    cat("PASS\n")
  } else {
    cat(sprintf("PARTIAL: %d/4 results\n", length(res)))
  }
  flush.console()
}

plan(sequential)
cat("\nDone\n")
