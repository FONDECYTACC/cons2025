# =============================================================================
# extval_death_02_table1_smd.R
# Covariate comparison (Table 1) with standardized mean differences for the
# mortality external validation: development cohort (convenio 1, training-set
# Table 1, n = 70,521) versus each external convenio (C3, C5), BEFORE and AFTER
# common-support matching. The development column reuses the project training-set
# figures (verbatim from extval_02_table1_smd.R); external columns are computed
# from the recoded C3/C5 cohorts. Only the variables recoded for the mortality
# model are shown (the SHAP best_perf2 covariate set plus age/sex).
#
# Public API:
#   extval_death_smd(cohorts)             cohorts = named list of nondum data.frames
#       -> data.frame (Variable, Level, Development, then <name>/SMD_<name> pairs)
#   extval_death_smd_browsable(tab, ...)  -> htmltools::browsable() grouped table
# =============================================================================

suppressWarnings(stopifnot(requireNamespace("htmltools", quietly = TRUE)))

if (!exists("project_root", inherits = TRUE) || !is.character(project_root) ||
    length(project_root) != 1L || !dir.exists(file.path(project_root, "cons", "_alt_scripts"))) {
  project_root <- local({
    pr <- tryCatch(here::here(), error = function(e) NA_character_)
    if (length(pr) != 1L || is.na(pr) || !dir.exists(file.path(pr, "cons", "_alt_scripts")))
      pr <- sub("(/)?cons/?$", "", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    normalizePath(pr, winslash = "/", mustWork = FALSE)
  })
}

# Reuse the development reference blocks + SMD helpers.
source(file.path(project_root, "cons/_alt_scripts/extval_02_table1_smd.R"))

# Variables available in the mortality cohort (recoded). Order = display order.
# plan_type_corr is intentionally EXCLUDED: it is a model stratum that was MAPPED for
# C3/C5 (C3 has no plan column -> all pg-pr), so its SMD vs convenio 1 reflects the
# mapping assumption, not an observed case-mix difference. The mapping is documented
# in the cohort/methods section.
.EVD_SMD_VARS <- c("adm_age_rec3", "sex_rec", "primary_sub_mod", "first_sub_used",
  "prim_sub_freq_rec", "occupation_condition_corr24", "eva_ocupacion", "eva_fisica",
  "any_phys_dx")

extval_death_smd <- function(cohorts) {
  stopifnot(is.list(cohorts), length(cohorts) >= 1L, !is.null(names(cohorts)))
  blocks <- Filter(function(bl) bl$var %in% .EVD_SMD_VARS, .EXTVAL_T1_BLOCKS)
  # keep .EVD_SMD_VARS order
  blocks <- blocks[order(match(vapply(blocks, function(b) b$var, ""), .EVD_SMD_VARS))]

  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)

  # one external cell (value + smd) for a categorical level
  cell_cat <- function(x, recode, lv, dev_p) {
    if (!is.null(recode)) x <- recode(x)
    ok <- !is.na(x) & x != ""; xt <- x[ok]; nden <- length(xt)
    n_lv <- sum(xt == lv); p <- if (nden) n_lv / nden else NA_real_
    list(disp = sprintf("%d (%.1f%%)", n_lv, 100 * p), smd = .smd_bin(unname(dev_p), p))
  }
  cell_bin <- function(x, posfun, dev_p) {
    ok <- !is.na(x); p <- mean(posfun(x[ok])); n_pos <- sum(posfun(x[ok]))
    list(disp = sprintf("%d (%.1f%%)", n_pos, 100 * p), smd = .smd_bin(dev_p, p))
  }
  cell_cont <- function(x, dev_m, dev_s) {
    v <- suppressWarnings(as.numeric(x)); v <- v[is.finite(v)]
    list(disp = sprintf("%.1f (%.1f)", mean(v), stats::sd(v)),
         smd = .smd_cont(dev_m, dev_s, mean(v), stats::sd(v)))
  }

  cn <- names(cohorts)
  for (bl in blocks) {
    if (bl$type == "cont") {
      base <- list(Variable = bl$disp, Level = "",
                   Development = sprintf("%.1f (%.1f)", bl$dev_mean, bl$dev_sd))
      for (nm in cn) {
        c1 <- cell_cont(cohorts[[nm]][[bl$var]], bl$dev_mean, bl$dev_sd)
        base[[nm]] <- c1$disp; base[[paste0("SMD_", nm)]] <- sprintf("%.3f", c1$smd)
      }
      do.call(add, base)
    } else if (bl$type == "bin") {
      base <- list(Variable = bl$disp, Level = "",
                   Development = sprintf("%.1f%%", 100 * bl$dev))
      for (nm in cn) {
        c1 <- cell_bin(cohorts[[nm]][[bl$var]], bl$pos, bl$dev)
        base[[nm]] <- c1$disp; base[[paste0("SMD_", nm)]] <- sprintf("%.3f", c1$smd)
      }
      do.call(add, base)
    } else { # cat
      first <- TRUE
      for (lv in names(bl$dev)) {
        base <- list(Variable = if (first) bl$disp else "",
                     Level = unname(bl$labels[lv]),
                     Development = sprintf("%.1f%%", 100 * bl$dev[lv]))
        for (nm in cn) {
          c1 <- cell_cat(cohorts[[nm]][[bl$var]], bl$recode, lv, bl$dev[lv])
          base[[nm]] <- c1$disp; base[[paste0("SMD_", nm)]] <- sprintf("%.3f", c1$smd)
        }
        do.call(add, base)
        first <- FALSE
      }
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "n_ext") <- vapply(cohorts, nrow, 0L)
  attr(out, "n_dev") <- .EXTVAL_DEV_N
  attr(out, "cohorts") <- cn
  out
}

extval_death_smd_browsable <- function(tab, font = "'Times New Roman', serif",
                                       title = NULL) {
  cn <- attr(tab, "cohorts"); n_ext <- attr(tab, "n_ext"); n_dev <- attr(tab, "n_dev")
  th <- "position:sticky;top:0;background:#f3f3f3;border-bottom:2px solid #ccc;padding:6px 10px;text-align:center;white-space:nowrap;font-weight:bold;"
  td <- "border-bottom:1px solid #eee;padding:4px 10px;white-space:nowrap;"
  head_labels <- c("Variable", "Level", sprintf("Development C1 (n=%s)", format(n_dev, big.mark = ",")))
  for (nm in cn) head_labels <- c(head_labels, sprintf("%s (n=%s)", nm, format(n_ext[[nm]], big.mark = ",")), "SMD")
  hdr <- htmltools::tags$thead(htmltools::tags$tr(lapply(head_labels,
    function(nm) htmltools::tags$th(style = th, nm))))
  smd_col <- function(v) {
    x <- suppressWarnings(as.numeric(v))
    htmltools::tags$td(style = paste0(td, "text-align:center;",
      if (is.finite(x) && x >= 0.1) "color:#B2182B;font-weight:bold;" else ""), v)
  }
  body <- lapply(seq_len(nrow(tab)), function(i) {
    bold <- nzchar(tab$Variable[i])
    cells <- list(
      htmltools::tags$td(style = paste0(td, "text-align:left;", if (bold) "font-weight:bold;" else "padding-left:22px;"), tab$Variable[i]),
      htmltools::tags$td(style = paste0(td, "text-align:left;"), tab$Level[i]),
      htmltools::tags$td(style = paste0(td, "text-align:center;"), tab$Development[i]))
    for (nm in cn) {
      cells <- c(cells, list(
        htmltools::tags$td(style = paste0(td, "text-align:center;"), tab[[nm]][i]),
        smd_col(tab[[paste0("SMD_", nm)]][i])))
    }
    htmltools::tags$tr(cells)
  })
  if (is.null(title))
    title <- "Table. Covariate distribution: development cohort (convenio 1) versus external convenios 3 and 5, before and after common-support matching, with standardized mean differences."
  htmltools::browsable(htmltools::tags$div(
    htmltools::tags$div(style = paste0("font-weight:bold;margin-bottom:6px;font-family:", font, ";"), title),
    htmltools::tags$div(style = paste0("max-height:640px;overflow:auto;border:1px solid #ddd;font-family:", font, ";font-size:13px;"),
      htmltools::tags$table(style = "border-collapse:collapse;width:max-content;min-width:100%;",
        hdr, htmltools::tags$tbody(body))),
    htmltools::tags$div(style = paste0("font-size:11px;color:#555;margin-top:6px;max-width:1000px;font-family:", font, ";"),
      "Development values are the convenio 1 training-set Table 1 figures (percentages among non-missing). External percentages are among non-missing. SMD computed from the two proportions (categorical/binary) or means and SDs (continuous); |SMD| >= 0.10 highlighted. Matched columns use the common-support propensity subsample.")))
}
