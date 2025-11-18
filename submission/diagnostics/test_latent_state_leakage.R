#!/usr/bin/env Rscript
# Test: Verify latent states don't leak future information

source("renv/activate.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/setup.R")
source("R/data_processing.R")
source("R/latent_states.R")

cat("=== Testing Latent States for Data Leakage ===\n\n")

# Load data
DATA_DIR <- file.path(".", "data")
qdat_raw <- read_quarterly_data(DATA_DIR)
monthly_variables <- resolve_monthly_indicators()
monthly_data <- read_combined_timeseries(DATA_DIR, variables = monthly_variables)

# Apply publication lags
monthly_data$ts_list <- apply_publication_lags(monthly_data$ts_list, monthly_publication_lags)

# Trim and prepare
trimmed <- trim_to_overlap(
  qdat_raw,
  monthly_data$ts_list,
  mode = "ragged",
  fill_method = "locf",
  start_strategy = "fill"
)
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_adj <- stationary$data
monthly_ts <- window_monthly_series(trimmed$monthly, trimmed$qdat, end_mode = "available")

# Test with different training sizes
train_sizes <- c(50, 100, 129)

for (train_rows in train_sizes) {
  cat(sprintf("\n--- Testing with train_rows = %d ---\n", train_rows))
  
  qdat_train <- qdat_adj[1:train_rows, , drop = FALSE]
  monthly_train <- window(monthly_ts, end = zoo::index(qdat_train)[train_rows])
  
  # Build Y and estimate model
  Y <- build_Y(qdat_train, monthly_train)
  
  cat("Estimating MF-VAR model...\n")
  mod <- estimate_mfvar_model(Y, n_lags = 5, n_fcst = 12, seed = 999)
  
  # Extract latent states
  cat("Extracting latent states...\n")
  latent_states <- extract_latent_states(mod, summary = "mean")
  
  # Check dimensions
  n_months_latent <- nrow(latent_states)
  expected_months <- train_rows * 3  # 3 months per quarter
  
  cat(sprintf("  Latent states: %d months\n", n_months_latent))
  cat(sprintf("  Expected: %d months (%d quarters * 3)\n", expected_months, train_rows))
  
  if (n_months_latent > expected_months) {
    cat("  ⚠️  WARNING: Latent states EXCEED training period!\n")
    cat(sprintf("  Excess months: %d\n", n_months_latent - expected_months))
  } else if (n_months_latent == expected_months) {
    cat("  ✅ SAFE: Latent states match training period exactly\n")
  } else {
    cat(sprintf("  ℹ️  INFO: Latent states are %d months shorter (expected for edge effects)\n", 
                expected_months - n_months_latent))
  }
  
  # Check dates
  first_date <- latent_states$date[1]
  last_date <- latent_states$date[nrow(latent_states)]
  last_train_qtr <- zoo::index(qdat_train)[train_rows]
  last_train_date <- zoo::as.Date(last_train_qtr, frac = 1)
  
  cat(sprintf("  Latent states date range: %s to %s\n", first_date, last_date))
  cat(sprintf("  Last training quarter end: %s\n", last_train_date))
  
  if (last_date > last_train_date) {
    cat("  ⚠️  WARNING: Latent states extend BEYOND training data!\n")
    cat(sprintf("  Days beyond: %d\n", as.numeric(last_date - last_train_date)))
  } else {
    cat("  ✅ SAFE: Latent states do not exceed training period\n")
  }
}

cat("\n=== Test Complete ===\n")
