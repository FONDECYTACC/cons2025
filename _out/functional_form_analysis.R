library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Read data
file_path <- "_out/XGB12_corr_Functional_Forms_Dual_Aggregated_20260306_1834.xlsx"
df <- read_excel(file_path)

# Inspect
print(head(df))
print(paste("Rows:", nrow(df)))
print("Unique predictors:")
predictors <- unique(df$Predictor)
print(predictors)
print(paste("N predictors:", length(predictors)))

# Sample for speed if too large (but keep enough to see shape)
set.seed(42)
# We'll work with up to 50k rows per predictor to keep plotting fast
# but first let's see distribution of rows per predictor
counts <- df %>% count(Predictor) %>% arrange(desc(n))
print(counts)

# For each predictor, sample at most 30k rows
df_sample <- df %>%
  group_by(Predictor) %>%
  slice_sample(n = min(30000, n())) %>%
  ungroup()

# Function to fit and evaluate candidate transformations
# We return a data frame with fitted values for plotting
create_transformed_fits <- function(data) {
  x <- data$Feature_Value
  y <- data$SHAP_Impact
  
  # Remove NA/Inf
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]
  
  if (length(x) < 10) return(NULL)
  
  # Sort by x for line plotting
  ord <- order(x)
  xo <- x[ord]
  yo <- y[ord]
  
  out <- data.frame(
    x = xo,
    y = yo,
    Linear = NA,
    Quadratic = NA,
    Cubic = NA,
    Log = NA,
    Sqrt = NA,
    Spline = NA
  )
  
  # Linear
  try({
    m <- lm(y ~ x)
    out$Linear <- predict(m, newdata = data.frame(x = xo))
  }, silent = TRUE)
  
  # Quadratic
  try({
    m <- lm(y ~ poly(x, 2, raw = TRUE))
    out$Quadratic <- predict(m, newdata = data.frame(x = xo))
  }, silent = TRUE)
  
  # Cubic
  try({
    m <- lm(y ~ poly(x, 3, raw = TRUE))
    out$Cubic <- predict(m, newdata = data.frame(x = xo))
  }, silent = TRUE)
  
  # Log (only if x > 0)
  if (all(xo > 0)) {
    try({
      m <- lm(y ~ log(x))
      out$Log <- predict(m, newdata = data.frame(x = xo))
    }, silent = TRUE)
  }
  
  # Sqrt (only if x >= 0)
  if (all(xo >= 0)) {
    try({
      m <- lm(y ~ sqrt(x))
      out$Sqrt <- predict(m, newdata = data.frame(x = xo))
    }, silent = TRUE)
  }
  
  # Natural spline with 3 df (GAM-like but parametric)
  try({
    m <- lm(y ~ splines::ns(x, df = 3))
    out$Spline <- predict(m, newdata = data.frame(x = xo))
  }, silent = TRUE)
  
  out
}

# Compute fits per predictor
fits_list <- df_sample %>%
  group_split(Predictor, .keep = TRUE) %>%
  map(~ {
    pred_name <- unique(.x$Predictor)
    message("Processing: ", pred_name)
    res <- create_transformed_fits(.x)
    if (!is.null(res)) {
      res$Predictor <- pred_name
    }
    res
  }) %>%
  keep(~ !is.null(.x))

fits_df <- bind_rows(fits_list)

# Pivot longer for ggplot
fits_long <- fits_df %>%
  pivot_longer(cols = c(Linear, Quadratic, Cubic, Log, Sqrt, Spline),
               names_to = "Transformation", values_to = "Fitted")

# Plot: small multiples by Predictor, showing raw SHAP points (alpha low) + fitted lines
# Use a subset for points to avoid overplotting
points_long <- df_sample %>%
  group_by(Predictor) %>%
  slice_sample(n = min(5000, n())) %>%
  ungroup()

# We'll create one big faceted plot, but if there are too many predictors
# we may need to split into multiple PDF pages.
n_pred <- length(unique(fits_df$Predictor))
cat("Number of predictors to plot:", n_pred, "\n")

# Faceted plot function
make_facet_plot <- function(pred_subset, points_data, fits_data) {
  pd <- points_data %>% filter(Predictor %in% pred_subset)
  fd <- fits_data %>% filter(Predictor %in% pred_subset)
  
  ggplot() +
    geom_point(data = pd, aes(x = Feature_Value, y = SHAP_Impact),
               alpha = 0.15, size = 0.5, color = "grey40") +
    geom_line(data = fd, aes(x = x, y = Fitted, color = Transformation),
              linewidth = 0.8) +
    facet_wrap(~ Predictor, scales = "free", ncol = 4) +
    scale_color_manual(values = c(
      Linear = "#E41A1C",
      Quadratic = "#377EB8",
      Cubic = "#4DAF4A",
      Log = "#984EA3",
      Sqrt = "#FF7F00",
      Spline = "#000000"
    )) +
    labs(
      title = "Functional Form Inference: Feature Value vs SHAP Impact",
      subtitle = "Black = Spline (flexible reference); Colored lines = parametric candidates",
      x = "Feature Value",
      y = "SHAP Impact",
      color = "Transformation"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 8),
      panel.spacing = unit(0.5, "lines"),
      legend.position = "bottom"
    )
}

# Determine number of pages (max 12 panels per page for readability)
preds <- unique(fits_df$Predictor)
panels_per_page <- 12
n_pages <- ceiling(length(preds) / panels_per_page)

cat("Creating", n_pages, "pages...\n")

pdf_file <- "_out/functional_forms_inference.pdf"
pdf(pdf_file, width = 11, height = 8.5)

for (i in seq_len(n_pages)) {
  idx_start <- (i - 1) * panels_per_page + 1
  idx_end <- min(i * panels_per_page, length(preds))
  p_subset <- preds[idx_start:idx_end]
  
  p <- make_facet_plot(p_subset, points_long, fits_long)
  print(p)
}

dev.off()
cat("Saved PDF:", pdf_file, "\n")

# Also create a summary table of R-squared for each transformation per predictor
rsq_summary <- fits_df %>%
  group_by(Predictor) %>%
  summarise(
    rsq_linear = cor(y, Linear, use = "pairwise.complete.obs")^2,
    rsq_quad   = cor(y, Quadratic, use = "pairwise.complete.obs")^2,
    rsq_cubic  = cor(y, Cubic, use = "pairwise.complete.obs")^2,
    rsq_log    = ifelse(all(is.na(Log)), NA, cor(y, Log, use = "pairwise.complete.obs")^2),
    rsq_sqrt   = ifelse(all(is.na(Sqrt)), NA, cor(y, Sqrt, use = "pairwise.complete.obs")^2),
    rsq_spline = cor(y, Spline, use = "pairwise.complete.obs")^2,
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("rsq"), ~ round(.x, 3)))

print(rsq_summary)
write.csv(rsq_summary, "_out/functional_forms_rsq_summary.csv", row.names = FALSE)
cat("Saved R-sq summary: _out/functional_forms_rsq_summary.csv\n")
