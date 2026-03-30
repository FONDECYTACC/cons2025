# Build a single publication-style table for NRI/IDI results
#
# Usage:
# source("cons/_hist_scripts/make_nri_idi_epidemiology_table.R")
# tab_out <- make_nri_idi_epidemiology_table(reclass_out)

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

.format_num <- function(x, digits = 3) {
    if (!is.finite(x)) {
        return("NA")
    }
    formatC(x, format = "f", digits = digits)
}

.format_ci <- function(mean, q025, q975, digits = 3) {
    paste0(
        .format_num(mean, digits = digits),
        " (",
        .format_num(q025, digits = digits),
        " to ",
        .format_num(q975, digits = digits),
        ")"
    )
}

.normalize_model_label <- function(x, fallback) {
    if (is.null(x) || !nzchar(x) || x %in% c("old_model", "new_model")) {
        return(fallback)
    }
    x
}

.extract_summary_df <- function(reclass_object = NULL, summary_path = NULL) {
    if (!is.null(reclass_object)) {
        if (!is.list(reclass_object) || !"summary" %in% names(reclass_object)) {
            stop("`reclass_object` must be a list containing `summary`.", call. = FALSE)
        }
        return(as.data.frame(reclass_object$summary))
    }

    if (is.null(summary_path) || !is.character(summary_path) || length(summary_path) != 1L) {
        stop("Provide either `reclass_object` or a valid `summary_path`.", call. = FALSE)
    }
    if (!file.exists(summary_path)) {
        stop("File does not exist: ", summary_path, call. = FALSE)
    }
    utils::read.csv(summary_path, stringsAsFactors = FALSE)
}

.extract_config <- function(reclass_object = NULL) {
    if (is.null(reclass_object) || !is.list(reclass_object) || !"config" %in% names(reclass_object)) {
        return(list())
    }
    reclass_object$config
}

.assert_required_metrics <- function(summary_df) {
    required <- c(
        "continuous_nri",
        "continuous_nri_events",
        "continuous_nri_nonevents",
        "categorical_nri",
        "categorical_nri_events",
        "categorical_nri_nonevents",
        "idi",
        "discrimination_old",
        "discrimination_new"
    )
    missing_metrics <- setdiff(required, unique(summary_df$metric))
    if (length(missing_metrics)) {
        stop(
            "Missing required metrics in summary: ",
            paste(missing_metrics, collapse = ", "),
            call. = FALSE
        )
    }
    invisible(TRUE)
}

.metric_row <- function(df, metric_name) {
    out <- df[df$metric == metric_name, , drop = FALSE]
    if (!nrow(out)) {
        return(NULL)
    }
    out[1, , drop = FALSE]
}

.build_table_df <- function(summary_df, old_label, new_label) {
    summary_df$risk <- factor(summary_df$risk, levels = c("readmission", "death"))
    split_key <- interaction(summary_df$risk, summary_df$horizon, drop = TRUE, lex.order = TRUE)
    groups <- split(summary_df, split_key)

    rows <- lapply(groups, function(df) {
        risk_value <- as.character(df$risk[1])
        outcome_label <- if (identical(risk_value, "death")) "Death" else "Readmission"
        horizon <- df$horizon[1]

        cont_total <- .metric_row(df, "continuous_nri")
        cont_event <- .metric_row(df, "continuous_nri_events")
        cont_nonevent <- .metric_row(df, "continuous_nri_nonevents")
        cat_total <- .metric_row(df, "categorical_nri")
        cat_event <- .metric_row(df, "categorical_nri_events")
        cat_nonevent <- .metric_row(df, "categorical_nri_nonevents")
        idi <- .metric_row(df, "idi")
        disc_old <- .metric_row(df, "discrimination_old")
        disc_new <- .metric_row(df, "discrimination_new")

        data.frame(
            Outcome = outcome_label,
            `Horizon, months` = horizon,
            `Continuous NRI` = .format_ci(cont_total$mean, cont_total$q025, cont_total$q975),
            `Continuous NRI, events` = .format_ci(cont_event$mean, cont_event$q025, cont_event$q975),
            `Continuous NRI, nonevents` = .format_ci(cont_nonevent$mean, cont_nonevent$q025, cont_nonevent$q975),
            `Categorical NRI` = .format_ci(cat_total$mean, cat_total$q025, cat_total$q975),
            `Categorical NRI, events` = .format_ci(cat_event$mean, cat_event$q025, cat_event$q975),
            `Categorical NRI, nonevents` = .format_ci(cat_nonevent$mean, cat_nonevent$q025, cat_nonevent$q975),
            `IDI` = .format_ci(idi$mean, idi$q025, idi$q975),
            `Discrimination slope, reference vs updated` = paste0(
                .format_ci(disc_old$mean, disc_old$q025, disc_old$q975),
                " vs ",
                .format_ci(disc_new$mean, disc_new$q025, disc_new$q975)
            ),
            stringsAsFactors = FALSE,
            check.names = FALSE
        )
    })

    out <- do.call(rbind, rows)
    outcome_ord <- match(out[["Outcome"]], c("Readmission", "Death"))
    horizon_ord <- as.numeric(out[["Horizon, months"]])
    out <- out[order(outcome_ord, horizon_ord), , drop = FALSE]

    attr(out, "reference_label") <- old_label
    attr(out, "updated_label") <- new_label
    out
}

.same_cut_points_text <- function(cut_points) {
    if (is.null(cut_points) || !length(cut_points)) {
        return("not reported")
    }

    cp_list <- unname(cut_points)
    first <- cp_list[[1]]
    same <- all(vapply(cp_list, function(x) identical(as.numeric(x), as.numeric(first)), logical(1)))
    if (!same) {
        return("horizon-specific cut-points")
    }

    cp <- as.numeric(first)
    if (!length(cp)) {
        return("not reported")
    }

    bins <- c(
        paste0("<", .format_num(cp[1] * 100, digits = 0), "%"),
        vapply(seq_len(length(cp) - 1L), function(i) {
            paste0(
                .format_num(cp[i] * 100, digits = 0),
                "% to <",
                .format_num(cp[i + 1L] * 100, digits = 0),
                "%"
            )
        }, character(1)),
        paste0(">=", .format_num(cp[length(cp)] * 100, digits = 0), "%")
    )
    paste(bins, collapse = "; ")
}

.build_notes <- function(summary_df, config, old_label, new_label) {
    n_reps <- sort(unique(summary_df$n))
    reps_txt <- if (length(n_reps) == 1L) {
        paste0("Estimates are means across ", n_reps, " paired validation replicates; 95% intervals are empirical 2.5th and 97.5th percentiles across replicates.")
    } else {
        "Estimates are means across paired validation replicates; the column `n` in the source summary gives the number of contributing replicates for each metric."
    }

    cut_points_txt <- .same_cut_points_text(config$cut_points %||% NULL)

    c(
        reps_txt,
        paste0(
            "Model comparison is ",
            old_label,
            " (reference) versus ",
            new_label,
            " (updated). Positive NRI and IDI values favor the updated model."
        ),
        "Continuous NRI is the sum of the event and nonevent components. Positive event NRI indicates upward movement in predicted risk among subjects with the event by the stated horizon; positive nonevent NRI indicates downward movement in predicted risk among subjects who remain event-free beyond that horizon.",
        paste0("Categorical NRI was computed using risk categories defined by: ", cut_points_txt, "."),
        "IDI is the difference in discrimination slopes between the updated and reference models.",
        "The discrimination slope equals the weighted mean predicted risk among subjects with the event by time t minus the weighted mean predicted risk among subjects who are event-free beyond time t.",
        "All measures were calculated at fixed follow-up horizons using censoring-adjusted weights and out-of-fold validation predictions."
    )
}

.build_title <- function(old_label, new_label) {
    paste0(
        "Table. Horizon-specific net reclassification improvement and integrated discrimination improvement ",
        "comparing updated and reference dual Cox models for first treatment readmission and all-cause mortality after discharge"
    )
}

.html_escape <- function(x) {
    if (requireNamespace("htmltools", quietly = TRUE)) {
        return(htmltools::htmlEscape(x))
    }
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
}

.save_table_html <- function(table_df, title, notes, output_file) {
    headers <- names(table_df)

    thead <- paste0(
        "<tr>",
        paste0("<th>", vapply(headers, .html_escape, character(1)), "</th>", collapse = ""),
        "</tr>"
    )

    body_rows <- apply(table_df, 1, function(row) {
        paste0(
            "<tr>",
            paste0("<td>", vapply(as.character(row), .html_escape, character(1)), "</td>", collapse = ""),
            "</tr>"
        )
    })

    notes_html <- paste0(
        "<ol>",
        paste0("<li>", vapply(notes, .html_escape, character(1)), "</li>", collapse = ""),
        "</ol>"
    )

    html <- paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>", .html_escape(title), "</title>",
        "<style>",
        "body{font-family:Georgia,'Times New Roman',serif;margin:28px;color:#111;line-height:1.35;}",
        "h1{font-size:18px;font-weight:600;margin:0 0 12px 0;}",
        "table{border-collapse:collapse;width:100%;font-size:12px;}",
        "th,td{border:1px solid #333;padding:6px 8px;vertical-align:top;}",
        "th{background:#f3f3f3;text-align:left;}",
        ".notes{margin-top:14px;font-size:11px;}",
        ".notes strong{display:block;margin-bottom:4px;}",
        "</style></head><body>",
        "<h1>", .html_escape(title), "</h1>",
        "<table><thead>", thead, "</thead><tbody>", paste0(body_rows, collapse = ""), "</tbody></table>",
        "<div class='notes'><strong>Notes.</strong>", notes_html, "</div>",
        "</body></html>"
    )

    writeLines(html, con = output_file, useBytes = TRUE)
    invisible(output_file)
}

.save_table_markdown <- function(table_df, title, notes, output_file) {
    header <- paste(names(table_df), collapse = " | ")
    sep <- paste(rep("---", ncol(table_df)), collapse = " | ")
    rows <- apply(table_df, 1, function(row) paste(as.character(row), collapse = " | "))
    note_lines <- paste0(seq_along(notes), ". ", notes)

    lines <- c(
        title,
        "",
        paste0("| ", header, " |"),
        paste0("| ", sep, " |"),
        paste0("| ", rows, " |"),
        "",
        "Notes",
        note_lines
    )
    writeLines(lines, con = output_file, useBytes = TRUE)
    invisible(output_file)
}

.default_output_dir <- function(prefix = "nri_idi_epidemiology_table") {
    root <- tryCatch(here::here(), error = function(e) getwd())
    cons_dir <- if (basename(root) == "cons") root else file.path(root, "cons")
    out_root <- file.path(cons_dir, "_out")
    if (!dir.exists(out_root)) {
        out_root <- file.path(getwd(), "_out")
    }
    file.path(out_root, sprintf("%s_%s", prefix, format(Sys.time(), "%Y%m%d_%H%M%S")))
}

make_nri_idi_epidemiology_table <- function(
    reclass_object = NULL,
    summary_path = NULL,
    output_dir = NULL,
    prefix = "nri_idi_epidemiology_table",
    old_label = NULL,
    new_label = NULL
) {
    summary_df <- .extract_summary_df(reclass_object = reclass_object, summary_path = summary_path)
    .assert_required_metrics(summary_df)
    config <- .extract_config(reclass_object)

    old_label <- .normalize_model_label(old_label %||% config$old_model %||% "", "Reference model")
    new_label <- .normalize_model_label(new_label %||% config$new_model %||% "", "Updated model")

    table_df <- .build_table_df(summary_df = summary_df, old_label = old_label, new_label = new_label)
    title <- .build_title(old_label = old_label, new_label = new_label)
    notes <- .build_notes(summary_df = summary_df, config = config, old_label = old_label, new_label = new_label)

    output_dir <- output_dir %||% .default_output_dir(prefix = prefix)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    csv_path <- file.path(output_dir, sprintf("%s.csv", prefix))
    html_path <- file.path(output_dir, sprintf("%s.html", prefix))
    md_path <- file.path(output_dir, sprintf("%s.md", prefix))
    rds_path <- file.path(output_dir, sprintf("%s.rds", prefix))

    utils::write.csv(table_df, csv_path, row.names = FALSE, na = "")
    .save_table_html(table_df = table_df, title = title, notes = notes, output_file = html_path)
    .save_table_markdown(table_df = table_df, title = title, notes = notes, output_file = md_path)

    result <- list(
        title = title,
        notes = notes,
        table = table_df,
        files = list(
            csv = csv_path,
            html = html_path,
            markdown = md_path,
            rds = rds_path
        )
    )
    saveRDS(result, rds_path)
    result
}
