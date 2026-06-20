## Targeted parallel debug: isolates exactly which future_lapply settings hang
suppressPackageStartupMessages({
  library(future)
  library(future.apply)
  library(survival)
})

cat("Packages loaded\n")

# Minimal test function with baseenv closure (same design as .run_task)
.worker <- function(x) {
  survival::survfit(survival::Surv(c(1,2,3,4,5), c(1,0,1,0,1)) ~ 1)
  x * 2L
}
environment(.worker) <- baseenv()

tasks <- as.list(1:8)

plan(multisession, workers = 2L)
cat("Plan set: multisession 2 workers\n")

# ── Test A: future.globals=FALSE, no chunk.size ──────────────────────────────
cat("\nTest A: future.globals=FALSE, no chunk.size ... ")
res_a <- tryCatch(
  withCallingHandlers(
    future.apply::future_lapply(tasks, .worker, future.globals = FALSE,
                                future.packages = c("survival")),
    error = function(e) cat("ERROR:", conditionMessage(e), "\n")
  ),
  error = function(e) NULL
)
cat(if (!is.null(res_a)) "PASS\n" else "FAIL\n")

# ── Test B: future.globals=FALSE, chunk.size=4 ───────────────────────────────
cat("Test B: future.globals=FALSE, chunk.size=4 ... ")
res_b <- tryCatch(
  future.apply::future_lapply(tasks, .worker, future.globals = FALSE,
                              future.packages = c("survival"),
                              future.chunk.size = 4L),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
cat(if (!is.null(res_b)) "PASS\n" else "FAIL\n")

# ── Test C: future.globals=TRUE (auto-detect), chunk.size=4 ──────────────────
cat("Test C: future.globals=TRUE, chunk.size=4 ... ")
res_c <- tryCatch(
  future.apply::future_lapply(tasks, .worker, future.globals = TRUE,
                              future.packages = c("survival"),
                              future.chunk.size = 4L),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
cat(if (!is.null(res_c)) "PASS\n" else "FAIL\n")

# ── Test D: globals as list naming .worker explicitly, chunk.size=4 ──────────
cat("Test D: future.globals=list(.worker), chunk.size=4 ... ")
res_d <- tryCatch(
  future.apply::future_lapply(tasks, .worker,
                              future.globals = list(.worker = .worker),
                              future.packages = c("survival"),
                              future.chunk.size = 4L),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
cat(if (!is.null(res_d)) "PASS\n" else "FAIL\n")

plan(sequential)
cat("\nDone\n")
