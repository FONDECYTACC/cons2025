options(width = 200)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

tab <- read.csv("G:/My Drive/Alvacast/SISTRAT 2023/cons/_out/table_incl_vs_excl_v3_smd.csv",
                 stringsAsFactors = FALSE, colClasses = "character")

d <- tab |>
  dplyr::filter(Variable != "", SMD != "") |>
  dplyr::transmute(
    Variable = Variable,
    SMD = as.numeric(SMD)
  ) |>
  dplyr::arrange(SMD) |>
  dplyr::mutate(
    Variable = factor(Variable, levels = Variable),
    flagged = SMD >= 0.15
  )

cat("n variables:", nrow(d), "\n")
print(d)

# --- Palette (status: balanced vs imbalanced), colorblind-safe, matches table's flag hue ---
col_ok   <- "#5B7C8D"   # muted slate blue -- "balanced"
col_flag <- "#B5541A"   # rust/amber -- "imbalanced" (>= 0.10), same hue used in the HTML table
col_line <- "#8B979C"

p <- ggplot(d, aes(x = SMD, y = Variable)) +
  geom_vline(xintercept = 0.15, linetype = "22", linewidth = 0.45, color = col_line) +
  geom_segment(aes(x = 0, xend = SMD, y = Variable, yend = Variable),
               color = "#D8DEDD", linewidth = 0.5) +
  geom_point(aes(color = flagged), size = 2.6) +
  scale_color_manual(
    values = c(`FALSE` = col_ok, `TRUE` = col_flag),
    labels = c(`FALSE` = "|SMD| < 0.15", `TRUE` = "|SMD| ≥ 0.15"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(0, max(0.55, max(d$SMD) * 1.05)),
                      breaks = seq(0, 0.8, 0.1), expand = c(0.01, 0)) +
  labs(
    title = "Standardized mean differences:\nexcluded vs included",
    subtitle = "Final Article 2 modeling sample (n=88,152) vs\npatients excluded from it (n=480)",
    x = "Standardized mean difference (excluded − included)",
    y = NULL,
    caption = "Dashed line: imbalance threshold, |SMD| = 0.15."
  ) +
  theme_minimal(base_size = 11, base_family = "") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ECEFEE", linewidth = 0.4),
    axis.text.y = element_text(color = "#1E2A32", size = 9.3),
    axis.text.x = element_text(color = "#5B6B73", size = 9),
    axis.title.x = element_text(color = "#5B6B73", size = 9.5, margin = margin(t = 8)),
    plot.title = element_text(face = "bold", size = 13, color = "#1E2A32"),
    plot.subtitle = element_text(size = 9.8, color = "#5B6B73", margin = margin(b = 10)),
    plot.caption = element_text(size = 7.8, color = "#8B979C", hjust = 0, margin = margin(t = 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 9),
    legend.margin = margin(b = -4),
    plot.margin = margin(14, 18, 10, 10)
  )

out_dir <- "G:/My Drive/Alvacast/SISTRAT 2023/cons/_figs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "loveplot_incl_vs_excl_smd.png"), p,
       width = 8.3, height = 7.6, dpi = 400, bg = "white")
ggsave(file.path(out_dir, "loveplot_incl_vs_excl_smd.pdf"), p,
       width = 8.3, height = 7.6, device = cairo_pdf)

cat("\nSaved:\n", file.path(out_dir, "loveplot_incl_vs_excl_smd.png"), "\n",
    file.path(out_dir, "loveplot_incl_vs_excl_smd.pdf"), "\n")
cat("\nDONE\n")
