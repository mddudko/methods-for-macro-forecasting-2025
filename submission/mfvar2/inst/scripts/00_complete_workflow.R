#!/usr/bin/env Rscript
#
# 00_complete_workflow.R
# ======================
# Complete end-to-end workflow for MF-VAR forecasting
#
# This master script demonstrates the full pipeline:
# 1. Data preparation from raw input
# 2. Hyperparameter tuning (optional)
# 3. Model estimation
# 4. Forecast generation
# 5. Results visualization
# 6. Summary report
#
# Modify the settings below to run on your own data.

# ==============================================================================
# SETTINGS - MODIFY THESE FOR YOUR DATA
# ==============================================================================

# Input data
INPUT_DATA <- system.file("extdata", "snb_data_3m_3q.rds", package = "mfvar2")
# Or use your own:
# INPUT_DATA <- "path/to/your/data.csv"
# INPUT_DATA <- c("path/to/monthly.csv", "path/to/quarterly.csv")

# Variable specification (NULL = auto-detect)
QUARTERLY_VARS <- c("GDP", "Consumption", "Investment")
FLOW_VARS <- c("GDP", "Consumption", "Investment")

# Date range (NULL = use all available)
START_DATE <- "2000-01-01"
END_DATE <- NULL # NULL = latest available

# Model settings
LAG_ORDER <- 2
TUNE_HYPERPARAMETERS <- TRUE # FALSE for quick testing (uses defaults)

# Hyperparameter grids (only used if TUNE_HYPERPARAMETERS = TRUE)
# Set to single values to fix parameters during tuning
LAMBDA1_GRID <- c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5) # Overall tightness
LAMBDA2_GRID <- c(1, 2, 3, 4, 5) # Lag decay
LAMBDA3_GRID <- c(1) # Error variance prior (typically fixed at 1)
LAMBDA4_GRID <- c(1, 2, 3, 4, 5) # Sum-of-coefficients
LAMBDA5_GRID <- c(1, 2, 3, 4, 5) # Co-persistence

# MCMC settings
N_DRAWS <- 4000
BURNIN <- 1000
FORECAST_HORIZON <- 12 # months

# Output directory
OUTPUT_DIR <- "mfvar_output"

# Random seed for reproducibility
SEED <- 42

# ==============================================================================
# LOAD SCRIPTS
# ==============================================================================

library(mfvar2)

# Source utility scripts
script_dir <- system.file("scripts", package = "mfvar2")
source(file.path(script_dir, "01_prepare_data.R"))
source(file.path(script_dir, "02_estimate_and_forecast.R"))
source(file.path(script_dir, "03_visualize_results.R"))

# ==============================================================================
# RUN COMPLETE WORKFLOW
# ==============================================================================

cat("\n")
cat("╔", rep("═", 70), "╗\n", sep = "")
cat("║", rep(" ", 18), "MF-VAR COMPLETE WORKFLOW", rep(" ", 28), "║\n", sep = "")
cat("╚", rep("═", 70), "╝\n", sep = "")
cat("\n")

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR, recursive = TRUE)
    cat("Created output directory:", OUTPUT_DIR, "\n")
}

# Step 1: Prepare data
cat("\n")
cat("STEP 1: Data Preparation\n")
cat(rep("-", 70), "\n", sep = "")

data_prep <- prepare_my_data(
    input = INPUT_DATA,
    quarterly_vars = QUARTERLY_VARS,
    flow_vars = FLOW_VARS,
    start_date = START_DATE,
    end_date = END_DATE,
    difference = TRUE,
    output_path = file.path(OUTPUT_DIR, "data_prepared.rds"),
    verbose = TRUE
)

# Step 2: Estimate and forecast
cat("\n")
cat("STEP 2: Model Estimation & Forecasting\n")
cat(rep("-", 70), "\n", sep = "")

results <- run_mfvar_forecasting(
    data_prep = data_prep,
    p = LAG_ORDER,
    tune_hyperparameters = TUNE_HYPERPARAMETERS,
    lambda1_grid = LAMBDA1_GRID,
    lambda2_grid = LAMBDA2_GRID,
    lambda3_grid = LAMBDA3_GRID,
    lambda4_grid = LAMBDA4_GRID,
    lambda5_grid = LAMBDA5_GRID,
    n_draws = N_DRAWS,
    burnin = BURNIN,
    forecast_horizon = FORECAST_HORIZON,
    output_dir = file.path(OUTPUT_DIR, "estimation"),
    verbose = TRUE,
    seed = SEED
)

# Step 3: Visualize results
cat("\n")
cat("STEP 3: Results Visualization\n")
cat(rep("-", 70), "\n", sep = "")

plots <- visualize_all_results(
    results = results,
    data_prep = data_prep,
    output_dir = file.path(OUTPUT_DIR, "plots"),
    format = "both", # Save as PNG and PDF
    verbose = TRUE
)

# Step 4: Create summary report
cat("\n")
cat("STEP 4: Summary Report\n")
cat(rep("-", 70), "\n", sep = "")

summary_file <- file.path(OUTPUT_DIR, "SUMMARY_REPORT.txt")
sink(summary_file)

cat("=", rep("=", 70), "=\n", sep = "")
cat("  MF-VAR ESTIMATION & FORECASTING SUMMARY\n")
cat("=", rep("=", 70), "=\n", sep = "")
cat("\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\n")

cat("DATA\n")
cat(rep("-", 70), "\n", sep = "")
cat("Variables:       ", ncol(data_prep$data), "\n")
cat("  ", paste(colnames(data_prep$data), collapse = ", "), "\n")
cat("Observations:    ", nrow(data_prep$data), "\n")
cat(
    "Sample period:   ", format(zoo::index(data_prep$data)[1]), "to",
    format(zoo::index(data_prep$data)[nrow(data_prep$data)]), "\n"
)
cat("Quarterly vars:  ", paste(QUARTERLY_VARS, collapse = ", "), "\n")
cat("Flow vars:       ", paste(FLOW_VARS, collapse = ", "), "\n")
cat("\n")

cat("MODEL SPECIFICATION\n")
cat(rep("-", 70), "\n", sep = "")
cat("Lag order:       ", LAG_ORDER, "\n")
cat("Hyperparameters:\n")
cat("  λ₁ (tightness):       ", sprintf("%.4f", results$hyperparameters$lambda1_optimal), "\n")
cat("  λ₂ (lag decay):       ", sprintf("%.4f", results$hyperparameters$lambda2_optimal), "\n")
cat("  λ₃ (Σ prior):         ", sprintf("%.4f", results$hyperparameters$lambda3_optimal), "\n")
cat("  λ₄ (sum-of-coef):     ", sprintf("%.4f", results$hyperparameters$lambda4_optimal), "\n")
cat("  λ₅ (co-persistence):  ", sprintf("%.4f", results$hyperparameters$lambda5_optimal), "\n")
if (!is.null(results$hyperparameters$log_mdd)) {
    cat("  Log MDD:              ", sprintf("%.2f", results$hyperparameters$log_mdd), "\n")
}
cat("\n")

cat("ESTIMATION\n")
cat(rep("-", 70), "\n", sep = "")
cat("MCMC draws:      ", N_DRAWS, "\n")
cat("Burn-in:         ", BURNIN, "\n")
cat("Effective draws: ", N_DRAWS - BURNIN, "\n")
cat("\n")

cat("CONVERGENCE DIAGNOSTICS\n")
cat(rep("-", 70), "\n", sep = "")
geweke_pass <- sum(abs(results$diagnostics$geweke_z) < 2)
geweke_total <- length(results$diagnostics$geweke_z)
cat(
    "Geweke test:     ", geweke_pass, "/", geweke_total,
    "(", sprintf("%.1f%%", 100 * geweke_pass / geweke_total), ") pass\n"
)
ess_stats <- summary(results$diagnostics$eff_sample_size)
cat("Eff. sample size:\n")
cat("  Min:    ", sprintf("%.0f", ess_stats["Min."]), "\n")
cat("  Median: ", sprintf("%.0f", ess_stats["Median"]), "\n")
cat("  Mean:   ", sprintf("%.0f", ess_stats["Mean"]), "\n")
cat("  Max:    ", sprintf("%.0f", ess_stats["Max."]), "\n")
cat("\n")

cat("FORECASTS\n")
cat(rep("-", 70), "\n", sep = "")
cat("Horizon:         ", FORECAST_HORIZON, "months\n")
cat("Simulations:     ", results$settings$n_sim, "per posterior draw\n")
cat("\n")
cat("12-month ahead forecast (median with 90% intervals):\n\n")
cat(sprintf("%-15s %8s %8s %8s\n", "Variable", "h=1", "h=6", "h=12"))
cat(rep("-", 45), "\n", sep = "")

fc_monthly <- results$forecasts$forecasts_monthly
for (var in colnames(data_prep$data)) {
    h1 <- fc_monthly[[1]][, var]["50%"]
    h6 <- if (length(fc_monthly) >= 6) fc_monthly[[6]][, var]["50%"] else NA
    h12 <- if (length(fc_monthly) >= 12) fc_monthly[[12]][, var]["50%"] else NA

    cat(sprintf("%-15s %8.3f", var, h1))
    if (!is.na(h6)) cat(sprintf(" %8.3f", h6))
    if (!is.na(h12)) cat(sprintf(" %8.3f", h12))
    cat("\n")

    # Show interval for h=12
    if (!is.na(h12) && length(fc_monthly) >= 12) {
        lower <- fc_monthly[[12]][, var]["5%"]
        upper <- fc_monthly[[12]][, var]["95%"]
        cat(sprintf("%15s %8s %8s [%6.3f, %6.3f]\n", "", "", "", lower, upper))
    }
}
cat("\n")

cat("OUTPUT FILES\n")
cat(rep("-", 70), "\n", sep = "")
cat("Results saved to:", OUTPUT_DIR, "\n\n")
cat("Key files:\n")
cat("  data_prepared.rds              - Prepared data\n")
cat("  estimation/mfvar_results.rds   - Complete results object\n")
cat("  estimation/forecasts.rds       - Forecast quantiles\n")
cat("  estimation/hyperparameters.rds - Selected hyperparameters\n")
cat("  estimation/summary.csv         - Summary statistics\n")
cat("  plots/                         - All visualization plots\n")
cat("  SUMMARY_REPORT.txt             - This report\n")
cat("\n")

cat("=", rep("=", 70), "=\n", sep = "")
cat("  END OF REPORT\n")
cat("=", rep("=", 70), "=\n", sep = "")

sink()

cat("\nSummary report saved to:", summary_file, "\n")

# Print summary to console
cat("\n")
cat("=", rep("=", 70), "=\n", sep = "")
cat("  WORKFLOW COMPLETE!\n")
cat("=", rep("=", 70), "=\n", sep = "")
cat("\n")
cat(
    "✓ Data prepared:        ", ncol(data_prep$data), "variables,",
    nrow(data_prep$data), "observations\n"
)
cat("✓ Model estimated:      ", N_DRAWS - BURNIN, "posterior draws\n")
cat("✓ Forecasts generated:  ", FORECAST_HORIZON, "months ahead\n")
cat("✓ Plots created:        ", length(plots), "visualizations\n")
cat("\n")
cat("All results saved to:", OUTPUT_DIR, "\n")
cat("Review the summary report:", summary_file, "\n")
cat("\n")
cat("Next steps:\n")
cat("  1. Review plots in:", file.path(OUTPUT_DIR, "plots"), "\n")
cat("  2. Check convergence diagnostics\n")
cat("  3. Examine forecast accuracy (if you have holdout data)\n")
cat("  4. Compare to benchmarks (e.g., AR models)\n")
cat("\n")
