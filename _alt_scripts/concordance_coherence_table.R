# =============================================================================
# concordance_coherence_table.R
# -----------------------------------------------------------------------------
# Quantifies the gap between three Uno's C-index variants for the dual Cox
# evaluator, so the stratification question can be settled with numbers.
#
#   (1) lp_global      score = linear predictor eta = Xbeta, NO strata term.
#                      This is the ORIGINAL behaviour (covariate-only ranking,
#                      stratum baseline ignored). Your reported 0.7424 / 0.6041.
#   (2) risk_absolute  score = stratum-correct absolute risk 1 - S(t|x).
#                      Coherent with IBS / calibration / DCA (same risk object).
#                      This is the recommended HEADLINE discrimination.
#   (3) within_stratum score = eta, concordance computed WITHIN strata and pooled.
#                      Sensitivity. Expected to be NA / low-n for mortality when
#                      fine strata contain no deaths (that NA is itself a finding).
#
# REQUIREMENT: source the PATCHED evaluate_dual_cox_python_style_boot.R first
# (apply concordance_coherence.patch). With the patch unapplied this script
# silently returns three identical columns, because the options are inert.
#
# The three variants run on IDENTICAL folds (same seed / k_folds / eval_times /
# imputed_list); the ONLY thing that changes between them is the concordance
# step. So the reported deltas are pure, not cross-validation noise.
# =============================================================================

suppressPackageStartupMessages({
  library(survival)
  library(pec)
})

# ---- source the patched evaluator -------------------------------------------
# EDIT this path to wherever you keep the patched file:
EVALUATOR_PATH <- "evaluate_dual_cox_python_style_boot.R"
if (!exists("evaluate_dual_cox_python_style")) source(EVALUATOR_PATH)

# ---- INPUTS: edit to your objects -------------------------------------------
# imputed_list : your list of 5 imputed data.frames (predictors imputed, outcomes shared).
#                In your pipeline this was e.g. `py_corrected_datasets_boot`.
if (!exists("imputed_list")) {
  if (exists("py_corrected_datasets_boot")) {
    imputed_list <- py_corrected_datasets_boot
  } else {
    stop("Set `imputed_list` to your list of imputed data.frames before running.")
  }
}

# Named list of model pairs to evaluate, given by the NAME of the formula object
# in your session. A pair = one readmit formula + one death formula (the evaluator
# scores both on the same folds in a single call). Add the implementation pair if
# you also want the parsimonious models.
MODEL_NAMES <- list(
  primary = c(readmit = "formula_shap_readmit_clean_updated",
              death   = "formula_death_updated2")
  # ,
  # implementation = c(readmit = "formula_shap_readmit_clean_updated",
  #                    death   = "formula_shap_death")
)

resolve_formula <- function(nm) {
  if (!exists(nm)) stop(sprintf("Formula object '%s' not found in the session.", nm))
  f <- get(nm)
  if (!inherits(f, "formula")) stop(sprintf("'%s' is not a formula.", nm))
  f
}
MODELS <- lapply(MODEL_NAMES, function(p) {
  list(readmit = resolve_formula(p[["readmit"]]),
       death   = resolve_formula(p[["death"]]))
})

EVAL_TIMES <- c(6, 12, 36, 60)   # horizons to report (must be < max follow-up)
K_FOLDS    <- 5L                 # match your dual pipeline
SEED       <- 2125L              # match your dual pipeline (fixes folds across variants)
SHOW_TIMES <- c("Global", "6", "12", "36", "60")   # rows to keep in the table
OUT_CSV    <- "concordance_coherence_table.csv"
OUT_MD     <- "concordance_coherence_table.md"
# -----------------------------------------------------------------------------

VARIANTS <- list(
  lp_global      = list(score = "lp",   within = FALSE),
  risk_absolute  = list(score = "risk", within = FALSE),
  within_stratum = list(score = "lp",   within = TRUE)
)

run_variant <- function(readmit_f, death_f, score, within) {
  old <- options(
    dualcox.concordance_score        = score,
    dualcox.concordance_within_strata = within
  )
  on.exit(options(old), add = TRUE)

  res <- evaluate_dual_cox_python_style(
    formula_readmit = readmit_f,
    formula_death   = death_f,
    imputed_list    = imputed_list,
    eval_times      = EVAL_TIMES,
    k_folds         = K_FOLDS,
    seed            = SEED,
    verbose         = FALSE
  )
  s <- res$summary
  s[s$Metric == "Uno's C-Index",
    c("Risk", "Time", "mean", "sd", "q025", "q975", "n")]
}

# ---- run every variant for every model pair ---------------------------------
collected <- list()
for (mname in names(MODELS)) {
  for (vname in names(VARIANTS)) {
    v <- VARIANTS[[vname]]
    message(sprintf("Running model='%s' variant='%s' ...", mname, vname))
    s <- run_variant(MODELS[[mname]]$readmit, MODELS[[mname]]$death, v$score, v$within)
    s$Model   <- mname
    s$Variant <- vname
    collected[[paste(mname, vname, sep = "::")]] <- s
  }
}
long <- do.call(rbind, collected)
rownames(long) <- NULL

# keep only the requested horizons, ordered
ord <- c("Global", as.character(sort(EVAL_TIMES)))
long <- long[long$Time %in% SHOW_TIMES, , drop = FALSE]
long$Time <- factor(long$Time, levels = ord)

# ---- build the wide comparison table (merge, no reshape) --------------------
make_sub <- function(vn) {
  d <- long[long$Variant == vn, c("Model", "Risk", "Time", "mean", "q025", "q975", "n")]
  names(d)[4:7] <- paste0(vn, c("_mean", "_lo", "_hi", "_n"))
  d
}
wide <- Reduce(
  function(a, b) merge(a, b, by = c("Model", "Risk", "Time"), all = TRUE),
  lapply(names(VARIANTS), make_sub)
)

# deltas on the means (what the stratum baseline / within-strata restriction adds)
wide$delta_risk_minus_lp   <- round(wide$risk_absolute_mean  - wide$lp_global_mean, 4)
wide$delta_within_minus_lp <- round(wide$within_stratum_mean - wide$lp_global_mean, 4)

wide <- wide[order(wide$Model, wide$Risk, match(as.character(wide$Time), ord)), ]
rownames(wide) <- NULL

# ---- write outputs ----------------------------------------------------------
write.csv(wide, OUT_CSV, row.names = FALSE)

fmt_cell <- function(m, lo, hi, n) {
  ifelse(is.na(m), "—",
         sprintf("%.4f (%.4f, %.4f); n=%d", m, lo, hi, n))
}
md <- data.frame(
  Model   = wide$Model,
  Outcome = wide$Risk,
  Horizon = as.character(wide$Time),
  `C eta (lp, global)`        = fmt_cell(wide$lp_global_mean, wide$lp_global_lo, wide$lp_global_hi, wide$lp_global_n),
  `C risk (absolute)`         = fmt_cell(wide$risk_absolute_mean, wide$risk_absolute_lo, wide$risk_absolute_hi, wide$risk_absolute_n),
  `C within-stratum`          = fmt_cell(wide$within_stratum_mean, wide$within_stratum_lo, wide$within_stratum_hi, wide$within_stratum_n),
  `Δ risk−lp`                 = sprintf("%+.4f", wide$delta_risk_minus_lp),
  check.names = FALSE
)
con <- file(OUT_MD, "w")
writeLines("| Model | Outcome | Horizon | C eta (lp, global) | C risk (absolute) | C within-stratum | Δ risk−lp |", con)
writeLines("|---|---|---|---|---|---|---|", con)
for (i in seq_len(nrow(md))) {
  writeLines(paste0("| ", paste(unlist(md[i, ]), collapse = " | "), " |"), con)
}
close(con)

cat("\n================ Uno's C coherence comparison ================\n")
print(wide, row.names = FALSE)
cat(sprintf("\nSaved: %s  and  %s\n", OUT_CSV, OUT_MD))
cat("\nReading guide:\n")
cat(" - Horizon rows (6/12/36/60) are the clean comparison: same horizon, appropriate marker.\n")
cat(" - 'Global' risk_absolute ranks by absolute risk at the largest eval_time (",
    max(EVAL_TIMES), "m).\n", sep = "")
cat(" - delta_risk_minus_lp > 0  => the stratum baseline adds discrimination the lp-only C missed.\n")
cat(" - within_stratum with low n or NA (esp. Death) => fine strata lack events; report as a\n")
cat("   documented limitation, not a primary metric.\n")
