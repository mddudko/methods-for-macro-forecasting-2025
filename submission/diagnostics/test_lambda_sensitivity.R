#!/usr/bin/env Rscript
# Lambda Sensitivity Analysis with Proper Evaluation
# Tests lambda1 values using out-of-sample forecast accuracy

source("renv/activate.R")
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

source("R/setup.R")
source("R/data_processing.R")
source("R/latent_states.R")
source("R/plotting.R")

# Test values
lambda_values <- c(0.06, 0.08, 0.1)

# Prepare output directory
outdir <- "diagnostics/output/lambda_sensitivity"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(outdir, "plots"), recursive = TRUE, showWarnings = FALSE)

cat("=== Lambda Sensitivity Analysis ===\n")
cat(sprintf("Testing lambda1 values: %s\n", paste(lambda_values, collapse = ", ")))
cat("Evaluation: Holdout (last 4 quarters)\n\n")

# Load data using exact same pattern as run_mfvar_package.R
cat("Loading data...\n")
DATA_DIR <- file.path(".", "data")
qdat_raw <- read_quarterly_data(DATA_DIR)
monthly_variables <- resolve_monthly_indicators()
monthly_data <- read_combined_timeseries(DATA_DIR, variables = monthly_variables)

# Apply publication lags
monthly_data$ts_list <- apply_publication_lags(monthly_data$ts_list, monthly_publication_lags)

trimmed <- trim_to_overlap(
  qdat_raw,
  monthly_data$ts_list,
  mode = "ragged",
  fill_method = "locf",
  start_strategy = "fill"
)
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_adj <- stationary$data
transforms <- stationary$transforms
monthly_ts <- window_monthly_series(trimmed$monthly, trimmed$qdat, end_mode = "available")

# Split into train/test
n_lags <- 5
holdout_size <- min(4, nrow(qdat_adj) - (n_lags + 1))
train_end <- nrow(qdat_adj) - holdout_size

qdat_train <- qdat_adj[1:train_end, , drop = FALSE]
qdat_test <- qdat_adj[(train_end + 1):nrow(qdat_adj), , drop = FALSE]
monthly_train <- window(monthly_ts, end = zoo::index(qdat_train)[train_end])

cat(sprintf("Training sample: %d quarters (through %s)\n", nrow(qdat_train), 
            zoo::index(qdat_train)[nrow(qdat_train)]))
cat(sprintf("Test sample: %d quarters\n\n", nrow(qdat_test)))

# Results storage
results_list <- list()
latent_list <- list()

for (lambda1 in lambda_values) {
  cat(sprintf("\n--- Testing lambda1 = %.2f ---\n", lambda1))
  
  # Set options for this lambda value
  options(mfvar.lambda1 = lambda1)
  options(mfvar.aggregation = "first")
  options(mfvar.n_reps = 10000L)
  options(mfvar.n_burnin = 5000L)
  
  # Build Y structure
  Y <- build_Y(qdat_train, monthly_train)
  
  # Estimate model with this lambda
  cat("Estimating MF-VAR model...\n")
  mod_ss <- estimate_mfvar_model(
    Y = Y,
    n_lags = n_lags,
    n_fcst = 12,  # 12 months = 4 quarters
    seed = 123
  )
  
  # Extract latent states
  cat("Extracting latent states...\n")
  latent_states <- extract_latent_states(mod_ss)
  latent_list[[sprintf("lambda_%.2f", lambda1)]] <- latent_states
  
  # Generate forecasts for test period
  cat("Generating forecasts...\n")
  fcst_raw <- predict(mod_ss, pred_bands = 0.8, aggregate_fcst = TRUE)
  
  # Aggregate forecasts already come as quarterly
  fcst_df <- fcst_raw %>%
    group_by(variable) %>%
    mutate(horizon = row_number()) %>%
    filter(horizon <= 4) %>%
    select(variable, horizon, median) %>%
    tidyr::pivot_wider(names_from = variable, values_from = median) %>%
    as.data.frame()
  
  # Compute metrics for each variable
  metrics <- data.frame()
  for (var in target_variables) {
    actual <- as.numeric(qdat_test[[var]])
    pred <- fcst_df[[var]][1:min(4, nrow(qdat_test))]
    
    rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
    mae <- mean(abs(actual - pred), na.rm = TRUE)
    
    metrics <- rbind(metrics, data.frame(
      lambda1 = lambda1,
      variable = var,
      rmse = rmse,
      mae = mae
    ))
  }
  
  results_list[[sprintf("lambda_%.2f", lambda1)]] <- metrics
  
  cat(sprintf("  RMSE: GDP=%.3f, Inflation=%.3f, ExchRate=%.4f\n",
              metrics$rmse[metrics$variable == "gdp_growth"],
              metrics$rmse[metrics$variable == "inflation"],
              metrics$rmse[metrics$variable == "exch_rate"]))
}

# Combine results
all_metrics <- bind_rows(results_list)
write_csv(all_metrics, file.path(outdir, "lambda_sensitivity_metrics.csv"))

cat("\n=== Summary of Results ===\n")
print(all_metrics %>% 
        arrange(variable, lambda1) %>%
        select(lambda1, variable, rmse, mae))

# Compute average RMSE across variables for overall ranking
avg_metrics <- all_metrics %>%
  group_by(lambda1) %>%
  summarise(
    avg_rmse = mean(rmse),
    avg_mae = mean(mae),
    .groups = "drop"
  ) %>%
  arrange(avg_rmse)

cat("\n=== Overall Performance (Average across variables) ===\n")
print(avg_metrics)

write_csv(avg_metrics, file.path(outdir, "lambda_sensitivity_summary.csv"))

# Create plots for each lambda's latent states
cat("\n=== Generating latent state plots ===\n")

for (lambda_name in names(latent_list)) {
  lambda_val <- as.numeric(sub("lambda_", "", lambda_name))
  latent <- latent_list[[lambda_name]]
  
  # Add year column
  latent$year <- as.numeric(format(as.Date(latent$date), '%Y'))
  
  # Filter to 2000-2020 for GDP growth
  gdp_latent <- latent %>%
    filter(year >= 2000, year <= 2020) %>%
    select(date, gdp_growth)
  
  # Create plot
  p <- ggplot(gdp_latent, aes(x = date, y = gdp_growth)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = sprintf("GDP Growth Latent State (lambda1 = %.2f)", lambda_val),
      subtitle = sprintf("2000-2020: Range %.1f to %.1f, %d sign changes",
                         min(gdp_latent$gdp_growth), max(gdp_latent$gdp_growth),
                         sum(diff(sign(gdp_latent$gdp_growth)) != 0)),
      x = NULL,
      y = "GDP Growth (detrended)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  filename <- sprintf("latent_gdp_lambda_%.2f.png", lambda_val)
  ggsave(file.path(outdir, "plots", filename), p, width = 10, height = 5, dpi = 150)
  cat(sprintf("  Saved: %s\n", filename))
}

# Create comparison plot showing all three
cat("\nCreating combined comparison plot...\n")
combined_latent <- bind_rows(lapply(names(latent_list), function(lambda_name) {
  lambda_val <- as.numeric(sub("lambda_", "", lambda_name))
  latent_list[[lambda_name]] %>%
    mutate(lambda1 = sprintf("λ₁ = %.2f", lambda_val),
           year = as.numeric(format(as.Date(date), '%Y'))) %>%
    filter(year >= 2000, year <= 2020) %>%
    select(date, lambda1, gdp_growth)
}))

p_combined <- ggplot(combined_latent, aes(x = date, y = gdp_growth, color = lambda1)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_brewer(palette = "Set1", name = "Lambda") +
  labs(
    title = "GDP Growth Latent States: Lambda Sensitivity (2000-2020)",
    x = NULL,
    y = "GDP Growth (detrended)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(outdir, "plots", "latent_gdp_combined.png"), p_combined, 
       width = 12, height = 6, dpi = 150)

cat("\n✓ Lambda sensitivity analysis complete!\n")
cat(sprintf("Results saved to: %s\n", outdir))
cat(sprintf("Optimal lambda1 (lowest avg RMSE): %.2f\n", avg_metrics$lambda1[1]))
