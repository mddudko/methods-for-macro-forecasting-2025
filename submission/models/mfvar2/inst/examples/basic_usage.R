# ==============================================================================
# Basic Usage Example for mfvar2 Package
# ==============================================================================
# This script demonstrates the complete workflow for mixed frequency VAR
# forecasting with Swiss National Bank data
# ==============================================================================

library(mfvar2)

# Set seed for reproducibility
set.seed(123)

cat("=================================================================\n")
cat("MF-VAR2 Basic Usage Example\n")
cat("=================================================================\n\n")

# ==============================================================================
# Step 1: Prepare Data
# ==============================================================================

cat("Step 1: Preparing data...\n\n")

# Example: Load your SNB data
# In practice, you would load real CSV files:
# data_prepared <- prepare_data_snb(
#   data_sources = c(
#     "data/CPI-snb-data.csv",
#     "data/Labour_market-snb-data.csv",
#     "data/data_quarterly.csv"
#   ),
#   quarterly_vars = c("gdp", "consumption", "investment"),
#   flow_vars = c("gdp"),
#   start_date = "2000-01-01",
#   end_date = "2024-12-31",
#   log_transform = NULL,  # Auto-detect
#   difference = TRUE,
#   verbose = TRUE
# )

# For this example, simulate some data
n_months <- 200
n_vars <- 4

# Simulate monthly data
dates <- seq(as.Date("2000-01-01"), by = "month", length.out = n_months)
dates_yearmon <- zoo::as.yearmon(dates)

# Create example data frame
example_data <- data.frame(
  date = dates,
  cpi = 100 * exp(cumsum(rnorm(n_months, 0.002, 0.01))),
  unemployment = 5 + cumsum(rnorm(n_months, 0, 0.1)),
  gdp = rep(NA, n_months), # Quarterly
  investment = rep(NA, n_months) # Quarterly
)

# Add quarterly observations (every 3rd month)
for (i in seq(3, n_months, by = 3)) {
  example_data$gdp[i] <- 1000 * exp(cumsum(rnorm(1, 0.005, 0.02)))
  example_data$investment[i] <- 200 * exp(cumsum(rnorm(1, 0.003, 0.03)))
}

# Prepare the data
data_prepared <- prepare_data_snb(
  data_sources = example_data,
  quarterly_vars = c("gdp", "investment"),
  flow_vars = c("gdp"), # GDP is a flow variable
  log_transform = c("cpi", "gdp", "investment"),
  difference = FALSE, # Skip unit root tests for example
  standardize = TRUE,
  verbose = TRUE
)

cat("\nData preparation complete!\n\n")

# ==============================================================================
# Step 2: Tune Hyperparameters
# ==============================================================================

cat("Step 2: Tuning hyperparameters...\n\n")

hyper_optimal <- tune_minnesota_hyper(
  data_prepared = data_prepared,
  p = 2, # Use 2 lags for this small example
  lambda_grid = c(0.1, 0.5, 1.0, 2.0),
  refine_continuous = FALSE, # Skip for speed
  verbose = TRUE,
  seed = 123
)

cat("\nOptimal hyperparameters:\n")
print(hyper_optimal)
cat("\n")

# ==============================================================================
# Step 3: Estimate MF-BVAR
# ==============================================================================

cat("Step 3: Estimating MF-BVAR...\n\n")

posterior <- estimate_mf_bvar(
  data_prepared = data_prepared,
  p = 2,
  hyperparameters = hyper_optimal,
  n_draws = 1000, # Use more in practice (e.g., 5000)
  burnin = 500,
  thinning = 1,
  verbose = TRUE,
  seed = 456
)

cat("\nPosterior estimation complete!\n")
cat("Posterior mean Sigma (diagonal):\n")
print(diag(posterior$Sigma_mean))
cat("\n")

# ==============================================================================
# Step 4: Generate Forecasts
# ==============================================================================

cat("Step 4: Generating forecasts...\n\n")

forecasts <- forecast_mf_bvar(
  posterior = posterior,
  horizon_months = 12,
  n_sim = 500,
  seed = 789
)

cat("Forecasts generated!\n")
cat("\nMedian forecasts for CPI (first 6 months):\n")
print(forecasts$forecasts_monthly["cpi", 1:6, "q0.5"])
cat("\n")

# ==============================================================================
# Step 5: Visualize
# ==============================================================================

cat("Step 5: Creating fan chart...\n\n")

# Create fan chart for CPI
p_cpi <- plot_fan_chart(
  forecast = forecasts,
  variable = "cpi",
  intervals = c(0.5, 0.7, 0.9)
)

print(p_cpi)

cat("\n=================================================================\n")
cat("Example complete! Check the output directory for saved plots.\n")
cat("=================================================================\n")

# Optional: Save results
# saveRDS(posterior, "output/mfvar_posterior.rds")
# saveRDS(forecasts, "output/mfvar_forecasts.rds")
# ggsave("output/fan_chart_cpi.png", p_cpi, width = 10, height = 6)
