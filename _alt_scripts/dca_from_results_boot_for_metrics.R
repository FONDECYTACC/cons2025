# Drop-in replacement for the inline compute_dca_simple in
# prediction_metrics_complement.ipynb, section 7 (DCA).
#
# Why: compute_dca_simple calls pec::predictSurvProb on fitted Cox objects,
# which triggers predict.coxph -> model.frame.coxph -> eval on formula$terms
# environment. That fails when predictor columns are absent from newdata or the
# formula's stored environment no longer holds the training data.
# adca_from_results_boot works from pre-computed survival matrices stored inside
# results_boot — it never re-runs predict() and never hits that issue.
#
# Output: a nested list  dca[[horizon_label]][[model_name]]
# each leaf is a data.frame(threshold, net_benefit_model, net_benefit_all,
# net_benefit_none) — same format as compute_dca_simple, so the existing
# make_dca_plot_data helper still works per horizon.
#
# =============================================================================
# AUDIT / CORRECTIONS (this _alt_scripts copy supersedes the _hist_scripts one)
# Reviewed 2026-05-30. No change to the net-benefit math (that lives in
# adca_from_results_boot.R and was left untouched). Robustness fixes only:
#
#   1. compute_dca_from_boot() and run_dca_full_summary() now intersect the
#      requested horizons with EACH object's own config$eval_times before
#      calling run_adca_from_results_boot(). Previously the requested horizons
#      were passed verbatim and .normalize_horizons() inside the adca function
#      raised a hard stop() whenever a requested horizon was missing from
#      config$eval_times (e.g. asking for c(6,12,36,60) on an object that was
#      evaluated at c(60,108)). That aborted the whole run. Now: missing
#      horizons are warned and skipped; a model with no available horizon is
#      skipped (its slot is NULL) instead of aborting every other model.
#
#   2. compute_dca_from_boot() pivot loop guards against NULL adca slots and
#      only extracts horizons actually computed for each model. This removes a
#      duplicate warning path and avoids NULL$summary indexing.
#
#   3. plot_dca_multi_horizon() guards the "Treat all / Treat none" base-row
#      construction against a horizon where every model returned NULL
#      (previously evaluated h_data[[NA]] -> subscript error).
#
#   4. summarize_dca_nb() validates the required columns up front with an
#      explicit message instead of failing cryptically downstream.
#
#   5. make_dca_panel_figure() stops with a clear message when no rows match
#      the requested outcome/horizons (previously produced range() warnings and
#      an empty/invalid plot).
# =============================================================================
#
# ── Notebook cell "dca-setup" replacement ────────────────────────────────────
#
#   source("cons/_alt_scripts/dca_from_results_boot_for_metrics.R")
#
# ── Notebook cell "dca-run" replacement ──────────────────────────────────────
#
#   DCA_HORIZONS <- c(6, 12, 36, 60)
#
#   cat("Computing DCA for readmission...\n")
#   dca_readmit <- compute_dca_from_boot(
#     list(shap_clean = results_shap_xgb_py_upd_best_perf,
#          full_ph    = results_upd2_py),
#     risk     = "readmission",
#     horizons = DCA_HORIZONS
#   )
#   cat("DCA readmission completed.\n")
#
#   cat("Computing DCA for mortality...\n")
#   dca_death <- compute_dca_from_boot(
#     list(full_ph    = results_upd2_py,
#          shap_clean = results_shap_xgb_py_upd_best_perf),
#     risk     = "death",
#     horizons = DCA_HORIZONS
#   )
#   cat("DCA mortality completed.\n")
#
# ── Notebook cell "dca-plot" replacement ─────────────────────────────────────
#
#   if (requireNamespace("ggplot2", quietly = TRUE)) {
#     p_r <- plot_dca_multi_horizon(
#       dca_readmit,
#       model_labels = c(shap_clean = "SHAP clean", full_ph = "Full PH"),
#       title = "DCA - Readmission"
#     )
#     print(p_r)
#
#     p_d <- plot_dca_multi_horizon(
#       dca_death,
#       model_labels = c(full_ph = "Full PH", shap_clean = "SHAP (13 vars)"),
#       title = "DCA - Mortality"
#     )
#     print(p_d)
#
#     saveRDS(list(readmit = dca_readmit, death = dca_death),
#             file.path(project_root, "cons", "_out", "dca_results.rds"))
#     cat("DCA results saved to cons/_out/dca_results.rds\n")
#   }
#
# ── For summary analytics + panel figure, also run ───────────────────────────
# (covers both risks in one pass per model; avoids double-computation)
#
#   dca_models_full <- run_dca_full_summary(
#     list(full_ph    = results_upd2_py,
#          shap_clean = results_shap_xgb_py_upd_best_perf),
#     horizons = DCA_HORIZONS
#   )
#
#   dca_nb_full_ph    <- summarize_dca_nb(dca_models_full$full_ph$summary)
#   dca_nb_shap_clean <- summarize_dca_nb(dca_models_full$shap_clean$summary)
#
#   # Inspect useful threshold ranges
#   print(dca_nb_full_ph$any_useful)
#   print(dca_nb_shap_clean$focus_summary)
#
#   # Panel figures (A = Full PH, B = SHAP)
#   p_readmit_panel <- make_dca_panel_figure(
#     summary_full = dca_models_full$full_ph$summary,
#     summary_ml   = dca_models_full$shap_clean$summary,
#     outcome      = "readmission",
#     horizons     = c(12, 36, 60)
#   )
#   print(p_readmit_panel)
#
#   p_death_panel <- make_dca_panel_figure(
#     summary_full = dca_models_full$full_ph$summary,
#     summary_ml   = dca_models_full$shap_clean$summary,
#     outcome      = "death",
#     horizons     = c(12, 36, 60)
#   )
#   print(p_death_panel)

if (!exists("run_adca_from_results_boot", mode = "function")) {
  # AJ-for-readmission / KM-for-death version (audited). See header of that file.
  # Resolve from the project root; fall back one level up when the working
  # directory is cons/ (e.g. when sourced from a notebook kernel).
  adca_path <- "cons/_alt_scripts/adca_from_results_boot.R"
  if (!file.exists(adca_path)) adca_path <- file.path("..", adca_path)
  if (!file.exists(adca_path)) {
    stop("Could not locate cons/_alt_scripts/adca_from_results_boot.R from ",
         getwd(), call. = FALSE)
  }
  source(adca_path)
}

# Internal: horizons actually available inside a results_boot object.
# Mirrors the logic run_adca_from_results_boot() uses to derive
# available_horizons, so we can pre-filter before calling it (and avoid the
# hard stop() in .normalize_horizons when a requested horizon is absent).
.available_eval_times <- function(results_boot) {
  et <- results_boot$config$eval_times
  if (is.null(et)) {
    et <- unique(unlist(lapply(results_boot$raw_predictions, `[[`, "eval_times")))
  }
  sort(unique(as.numeric(et)))
}

# Internal: pivot adca_out$summary for one risk + horizon into the
# compute_dca_simple output format.
.dca_extract_horizon <- function(adca_out, risk, horizon, thresholds) {
  sub <- adca_out$summary
  sub <- sub[sub$risk == risk & sub$horizon == horizon, , drop = FALSE]
  if (nrow(sub) == 0L) {
    warning(sprintf(
      "No DCA results for risk='%s' at horizon=%g. ",
      risk, horizon
    ), "Verify that this horizon is in results_boot$config$eval_times.",
    call. = FALSE)
    return(NULL)
  }

  thr_in_data <- round(sort(unique(sub$threshold)), 8)
  thr         <- intersect(round(sort(thresholds), 8), thr_in_data)

  get_nb <- function(strategy) {
    s <- sub[sub$strategy == strategy, c("threshold", "net_benefit_mean"),
             drop = FALSE]
    s <- s[order(s$threshold), , drop = FALSE]
    s$net_benefit_mean[match(thr, round(s$threshold, 8))]
  }

  data.frame(
    threshold         = thr,
    net_benefit_model = get_nb("Model"),
    net_benefit_all   = get_nb("Treat all"),
    net_benefit_none  = get_nb("Treat none"),
    stringsAsFactors  = FALSE
  )
}

# Public: compute DCA from a named list of results_boot objects at multiple
# time horizons.
#
# results_boot_list  named list; each element is one results_boot object
#                    (output of evaluate_dual_cox_python_style).
#                    Each must have raw_predictions with surv_val_matrix.
# risk               "readmission" or "death"
# horizons           numeric vector; values not present in a given object's
#                    config$eval_times are warned and skipped for that object
#                    (rather than aborting the whole run).
# thresholds         probability grid (default 1 %–50 %)
# verbose            print progress to console
#
# Returns a named list keyed by horizon (as character), each element itself
# a named list parallel to results_boot_list:
#   dca[["12"]][["shap_clean"]] → data.frame(threshold, net_benefit_model, ...)
# A horizon/model combination that could not be computed is NULL.
compute_dca_from_boot <- function(
    results_boot_list,
    risk       = c("readmission", "death"),
    horizons   = c(6, 12, 36, 60),
    thresholds = seq(0.01, 0.50, by = 0.01),
    verbose    = TRUE
) {
  risk <- match.arg(risk)
  stopifnot(is.list(results_boot_list), length(results_boot_list) >= 1L)
  nms <- names(results_boot_list)
  if (is.null(nms)) nms <- paste0("model_", seq_along(results_boot_list))
  horizons <- sort(unique(as.numeric(horizons)))

  # Run adca_from_results_boot once per model (covers all available horizons
  # in one pass). Pre-filter horizons per object to avoid a hard stop().
  adca_list <- vector("list", length(results_boot_list))
  for (i in seq_along(results_boot_list)) {
    nm      <- nms[i]
    avail   <- .available_eval_times(results_boot_list[[i]])
    use_h   <- sort(intersect(horizons, avail))
    missing <- setdiff(horizons, avail)
    if (length(missing)) {
      warning(sprintf(
        "Model '%s': horizon(s) %s not in this object's eval_times (%s); skipped.",
        nm, paste(missing, collapse = ", "),
        if (length(avail)) paste(avail, collapse = ", ") else "<none>"),
        call. = FALSE)
    }
    if (!length(use_h)) {
      warning(sprintf("Model '%s': no requested horizon is available; model skipped.",
                      nm), call. = FALSE)
      next
    }
    if (verbose) {
      cat(sprintf("  DCA: %s | risk=%s | horizons=%s\n",
                  nm, risk, paste(use_h, collapse = ", ")))
      flush.console()
    }
    adca_list[[i]] <- run_adca_from_results_boot(
      results_boot = results_boot_list[[i]],
      thresholds   = thresholds,
      horizons     = use_h,
      output_dir   = file.path(tempdir(), sprintf("adca_%s_%d", nm, Sys.getpid())),
      create_plot  = FALSE,
      save_raw     = FALSE
    )
  }

  # Pivot to list keyed by horizon then by model
  horizon_labels <- as.character(horizons)
  out <- stats::setNames(vector("list", length(horizons)), horizon_labels)

  for (h in horizons) {
    h_label  <- as.character(h)
    # Initialised to all-NULL; assign ONLY non-NULL results so the list keeps
    # its length. (x[[i]] <- NULL DELETES the element from a list, which would
    # shrink per_model and break the setNames() below.)
    per_model <- vector("list", length(results_boot_list))
    for (i in seq_along(results_boot_list)) {
      adca_i <- adca_list[[i]]
      if (is.null(adca_i)) next
      computed_h <- as.numeric(adca_i$config$horizons)
      if (h %in% computed_h) {
        val <- .dca_extract_horizon(adca_i, risk, h, thresholds)
        if (!is.null(val)) per_model[[i]] <- val
      }
    }
    out[[h_label]] <- stats::setNames(per_model, nms)
  }

  out
}

# Public: ggplot2 decision curve faceted by horizon.
#
# dca_multi  output of compute_dca_from_boot
# model_labels  named character vector mapping model names to display labels
# title  plot title
plot_dca_multi_horizon <- function(
    dca_multi,
    model_labels = NULL,
    title        = "Decision Curve Analysis"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not installed; skipping plot.", call. = FALSE)
    return(invisible(NULL))
  }

  rows <- list()
  for (h_label in names(dca_multi)) {
    h_data <- dca_multi[[h_label]]
    for (nm in names(h_data)) {
      df <- h_data[[nm]]
      if (is.null(df) || nrow(df) == 0L) next
      display <- if (!is.null(model_labels) && nm %in% names(model_labels))
        model_labels[[nm]] else nm

      rows[[length(rows) + 1L]] <- data.frame(
        horizon    = paste0(h_label, " months"),
        threshold  = df$threshold,
        net_benefit = df$net_benefit_model,
        strategy   = display,
        stringsAsFactors = FALSE
      )
    }
    # Treat all and Treat none come from the first non-NULL model (same for all).
    # Guard against a horizon where every model is NULL.
    present <- which(!vapply(h_data, is.null, logical(1L)))
    if (length(present)) {
      first <- h_data[[present[1L]]]
      if (!is.null(first) && nrow(first) > 0L) {
        base <- data.frame(
          horizon   = paste0(h_label, " months"),
          threshold = first$threshold,
          stringsAsFactors = FALSE
        )
        rows[[length(rows) + 1L]] <- cbind(base,
          data.frame(net_benefit = first$net_benefit_all,  strategy = "Treat all",  stringsAsFactors = FALSE))
        rows[[length(rows) + 1L]] <- cbind(base,
          data.frame(net_benefit = first$net_benefit_none, strategy = "Treat none", stringsAsFactors = FALSE))
      }
    }
  }

  if (!length(rows)) {
    warning("plot_dca_multi_horizon: no non-NULL curves to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  plot_df <- do.call(rbind.data.frame, rows)

  # Order facets by horizon numerically
  h_order <- paste0(sort(as.integer(names(dca_multi))), " months")
  plot_df$horizon <- factor(plot_df$horizon, levels = h_order)

  model_strats  <- setdiff(unique(plot_df$strategy), c("Treat all", "Treat none"))
  ref_strats    <- c("Treat all", "Treat none")
  all_strats    <- c(model_strats, ref_strats)
  n_model       <- length(model_strats)
  model_colors  <- if (n_model <= 8L)
    c("#1b4d6b", "#2ca02c", "#9467bd", "#8c564b",
      "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")[seq_len(n_model)]
  else
    scales::hue_pal()(n_model)
  color_map  <- c(stats::setNames(model_colors, model_strats),
                  "Treat all"  = "#b03a2e",
                  "Treat none" = "#666666")
  ltype_map  <- c(stats::setNames(rep("solid", n_model), model_strats),
                  "Treat all"  = "dashed",
                  "Treat none" = "dotted")
  size_map   <- c(stats::setNames(rep(0.9, n_model), model_strats),
                  "Treat all"  = 0.6,
                  "Treat none" = 0.6)

  plot_df$strategy <- factor(plot_df$strategy, levels = all_strats)

  ggplot2::ggplot(plot_df,
    ggplot2::aes(x = threshold, y = net_benefit,
                 color = strategy, linetype = strategy, linewidth = strategy)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ horizon, nrow = 1L, scales = "free_y") +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = scales::breaks_pretty(n = 5)
    ) +
    ggplot2::scale_color_manual(values = color_map, breaks = all_strats) +
    ggplot2::scale_linetype_manual(values = ltype_map, breaks = all_strats) +
    ggplot2::scale_linewidth_manual(values = size_map, breaks = all_strats) +
    ggplot2::labs(
      title    = title,
      x        = "Threshold probability",
      y        = "Net benefit",
      color    = NULL, linetype = NULL, linewidth = NULL
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position   = "bottom",
      strip.background  = ggplot2::element_rect(fill = "grey98", color = "grey85"),
      panel.grid.minor  = ggplot2::element_blank()
    )
}

# ── run_dca_full_summary ──────────────────────────────────────────────────────
# Runs run_adca_from_results_boot ONCE per model, covering BOTH readmission and
# death risks in a single pass. Use this when you need summary analytics
# (summarize_dca_nb) or the panel figure (make_dca_panel_figure).
# Avoids the double-computation that compute_dca_from_boot would incur if called
# separately for each risk.
#
# results_boot_list  named list of results_boot objects (one per model)
# horizons           values not present in a given object's config$eval_times
#                    are warned and skipped for that object; a model with no
#                    available horizon is returned as NULL.
# thresholds         probability grid
#
# Returns a named list parallel to results_boot_list; each element is the full
# adca_out object from run_adca_from_results_boot (has $summary, $focus_windows,
# $raw, $config), or NULL for a skipped model.
run_dca_full_summary <- function(
    results_boot_list,
    horizons   = c(6, 12, 36, 60),
    thresholds = seq(0.01, 0.50, by = 0.01),
    verbose    = TRUE
) {
  stopifnot(is.list(results_boot_list), length(results_boot_list) >= 1L)
  nms <- names(results_boot_list)
  if (is.null(nms)) nms <- paste0("model_", seq_along(results_boot_list))
  horizons <- sort(unique(as.numeric(horizons)))

  out <- vector("list", length(results_boot_list))
  for (i in seq_along(results_boot_list)) {
    nm      <- nms[i]
    avail   <- .available_eval_times(results_boot_list[[i]])
    use_h   <- sort(intersect(horizons, avail))
    missing <- setdiff(horizons, avail)
    if (length(missing)) {
      warning(sprintf(
        "Model '%s': horizon(s) %s not in this object's eval_times (%s); skipped.",
        nm, paste(missing, collapse = ", "),
        if (length(avail)) paste(avail, collapse = ", ") else "<none>"),
        call. = FALSE)
    }
    if (!length(use_h)) {
      warning(sprintf("Model '%s': no requested horizon is available; model skipped.",
                      nm), call. = FALSE)
      next
    }
    if (verbose) {
      cat(sprintf("  DCA full summary: %s | horizons=%s\n",
                  nm, paste(use_h, collapse = ", ")))
      flush.console()
    }
    out[[i]] <- run_adca_from_results_boot(
      results_boot = results_boot_list[[i]],
      thresholds   = thresholds,
      horizons     = use_h,
      output_dir   = file.path(tempdir(), sprintf("adca_%s_%d", nm, Sys.getpid())),
      create_plot  = FALSE,
      save_raw     = FALSE
    )
  }
  stats::setNames(out, nms)
}

# ── summarize_dca_nb ──────────────────────────────────────────────────────────
# Computes net-benefit comparisons from a full adca_out$summary data.frame.
# Equivalent to the dca-summary-full-function cell in prediction23_converted.
#
# adca_summary  adca_out$summary from run_adca_from_results_boot or
#               run_dca_full_summary(...)$<model>$summary
#
# Returns list with:
#   $dca_nb           — wide data.frame with nb_model / nb_treat_all / nb_treat_none
#                       plus model_beats_none / model_beats_all / model_useful flags
#   $any_useful       — summary by risk x horizon of useful threshold ranges
#   $focus_summary    — thresholds in the focus window where model is useful
summarize_dca_nb <- function(adca_summary) {
  stopifnot(requireNamespace("dplyr",  quietly = TRUE))
  stopifnot(requireNamespace("tidyr",  quietly = TRUE))

  if (is.null(adca_summary) || !is.data.frame(adca_summary) || !nrow(adca_summary)) {
    stop("`adca_summary` must be a non-empty data.frame. ",
         "Pass run_dca_full_summary(...)$<model>$summary (a skipped model is NULL).",
         call. = FALSE)
  }
  required <- c("risk", "horizon", "threshold", "strategy", "net_benefit_mean",
                "observed_event_risk_mean", "focus_lower", "focus_upper",
                "in_focus_window")
  missing_cols <- setdiff(required, names(adca_summary))
  if (length(missing_cols)) {
    stop("`adca_summary` is missing required columns: ",
         paste(missing_cols, collapse = ", "),
         ". Expected the $summary element of an adca_out object.", call. = FALSE)
  }

  df <- dplyr::mutate(
    adca_summary,
    strategy = dplyr::recode(
      strategy,
      "Model"      = "model",
      "Treat all"  = "treat_all",
      "Treat none" = "treat_none"
    )
  )

  dca_nb <- tidyr::pivot_wider(
    data = dplyr::select(
      df,
      risk, horizon, threshold, observed_event_risk_mean,
      focus_lower, focus_upper, in_focus_window,
      strategy, net_benefit_mean
    ),
    id_cols     = c(risk, horizon, threshold, observed_event_risk_mean,
                    focus_lower, focus_upper, in_focus_window),
    names_from  = strategy,
    values_from = net_benefit_mean,
    names_prefix = "nb_"
  )

  dca_nb <- dplyr::mutate(
    dca_nb,
    model_beats_none = !is.na(nb_model) & !is.na(nb_treat_none) &
      (nb_model > nb_treat_none),
    model_beats_all  = !is.na(nb_model) & !is.na(nb_treat_all) &
      (nb_model > nb_treat_all),
    model_useful     = model_beats_none & model_beats_all
  )

  any_useful <- dca_nb |>
    dplyr::group_by(risk, horizon) |>
    dplyr::summarise(
      any_useful_anywhere  = any(model_useful, na.rm = TRUE),
      useful_threshold_min = if (any(model_useful, na.rm = TRUE))
        min(threshold[model_useful], na.rm = TRUE) else NA_real_,
      useful_threshold_max = if (any(model_useful, na.rm = TRUE))
        max(threshold[model_useful], na.rm = TRUE) else NA_real_,
      max_model_nb      = max(nb_model,      na.rm = TRUE),
      max_treat_all_nb  = max(nb_treat_all,  na.rm = TRUE),
      max_treat_none_nb = max(nb_treat_none, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(risk, horizon)

  focus_summary <- dca_nb |>
    dplyr::filter(in_focus_window, model_useful) |>
    dplyr::group_by(risk, horizon, focus_lower, focus_upper,
                    observed_event_risk_mean) |>
    dplyr::summarise(
      useful_threshold_min = min(threshold, na.rm = TRUE),
      useful_threshold_max = max(threshold, na.rm = TRUE),
      peak_nb              = max(nb_model,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(risk, horizon)

  list(dca_nb = dca_nb, any_useful = any_useful, focus_summary = focus_summary)
}

# ── make_dca_panel_figure ─────────────────────────────────────────────────────
# Two-row panel figure (A = full PH model, B = ML/SHAP model) faceted by
# horizon. Adapted from the dca-plot-function cell in prediction23_converted.
#
# summary_full  adca_out$summary for the Full PH model
# summary_ml    adca_out$summary for the ML/SHAP model
# outcome       "readmission" or "death" — which risk to plot
# horizons      subset of horizons to show (must be in both summaries)
# x_limits      NULL = auto from data
# y_limits      NULL = auto from data (padded by 5 %)
#
# Returns a ggplot2 object.
make_dca_panel_figure <- function(
    summary_full,
    summary_ml,
    outcome   = c("death", "readmission"),
    horizons  = c(12, 36, 60),
    x_limits  = NULL,
    y_limits  = NULL
) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  outcome <- match.arg(outcome)
  if (is.null(summary_full) || is.null(summary_ml) ||
      !is.data.frame(summary_full) || !is.data.frame(summary_ml)) {
    stop("`summary_full` and `summary_ml` must both be adca_out$summary data.frames ",
         "(a skipped model is NULL).", call. = FALSE)
  }

  full_dat <- summary_full; full_dat$model_panel <- "A"
  ml_dat   <- summary_ml;   ml_dat$model_panel   <- "B"

  plot_dat <- rbind.data.frame(full_dat, ml_dat)
  plot_dat <- plot_dat[
    plot_dat$risk %in% outcome & plot_dat$horizon %in% horizons, , drop = FALSE
  ]
  if (!nrow(plot_dat)) {
    stop(sprintf(
      "No rows for outcome='%s' at horizon(s) %s in the provided summaries. ",
      outcome, paste(horizons, collapse = ", ")),
      "Check that both summaries contain this risk and these horizons ",
      "(horizons must be in each object's config$eval_times).", call. = FALSE)
  }
  plot_dat$strategy_plot <- ifelse(
    plot_dat$strategy == "Model", "Model", plot_dat$strategy
  )
  plot_dat <- plot_dat[, c(
    "risk", "horizon", "threshold", "model_panel",
    "strategy_plot", "net_benefit_mean", "focus_lower", "focus_upper"
  )]
  plot_dat$model_panel   <- factor(plot_dat$model_panel,   levels = c("A", "B"))
  plot_dat$horizon       <- factor(plot_dat$horizon,       levels = horizons)
  plot_dat$strategy_plot <- factor(plot_dat$strategy_plot,
    levels = c("Model", "Treat all", "Treat none"))

  focus_dat <- plot_dat[
    !duplicated(plot_dat[, c("model_panel", "horizon", "focus_lower", "focus_upper")]),
    , drop = FALSE
  ]

  if (is.null(x_limits)) x_limits <- range(plot_dat$threshold, na.rm = TRUE)
  if (is.null(y_limits)) {
    y_rng <- range(plot_dat$net_benefit_mean, na.rm = TRUE)
    y_pad <- 0.05 * diff(y_rng)
    if (!is.finite(y_pad) || y_pad == 0) y_pad <- 0.01
    y_limits <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)
  }

  panel_labels <- data.frame(
    model_panel = factor(c("A", "B"), levels = c("A", "B")),
    horizon     = factor(min(horizons), levels = horizons),
    label       = c("A", "B"),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = focus_dat,
      ggplot2::aes(xmin = focus_lower, xmax = focus_upper,
                   ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = "grey80", alpha = 0.18, color = NA
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(
      data = plot_dat,
      ggplot2::aes(x = threshold, y = net_benefit_mean,
                   linetype = strategy_plot, linewidth = strategy_plot)
    ) +
    ggplot2::geom_text(
      data = panel_labels,
      ggplot2::aes(x = x_limits[1], y = y_limits[2], label = label),
      hjust = 4.5, vjust = -0.90,
      fontface = "bold", family = "Times New Roman", size = 6,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(
      model_panel ~ horizon,
      switch    = "y",
      labeller  = ggplot2::labeller(
        horizon = function(x) paste0(x, " months")
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Model" = "solid", "Treat all" = "dotted", "Treat none" = "dotdash")
    ) +
    ggplot2::scale_linewidth_manual(
      values = c("Model" = 0.9, "Treat all" = 0.6, "Treat none" = 0.6)
    ) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off") +
    ggplot2::labs(
      x = "Threshold probability", y = "Net benefit",
      linetype = NULL, linewidth = NULL
    ) +
    ggplot2::theme_classic(base_size = 16, base_family = "Times New Roman") +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      strip.background  = ggplot2::element_blank(),
      strip.placement   = "outside",
      strip.text.y.left = ggplot2::element_blank(),
      strip.text.x      = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
      axis.text         = ggplot2::element_text(color = "black"),
      axis.title        = ggplot2::element_text(face = "plain"),
      panel.spacing     = ggplot2::unit(0.9, "lines")
    )
}
