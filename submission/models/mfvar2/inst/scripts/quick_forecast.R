#!/usr/bin/env Rscript
#
# quick_forecast.R
# ================
# Quick MF-VAR forecasting script for rapid testing
#
# This script runs a fast version of the complete workflow:
# - Skips hyperparameter tuning (uses sensible defaults)
# - Uses fewer MCMC draws
# - Perfect for quick tests and prototyping
#
# Usage:
#   Rscript quick_forecast.R path/to/data.csv
#
# Or in R:
#   source("inst/scripts/quick_forecast.R")
#   quick_forecast("path/to/data.csv")

library(mfvar2)

#' Quick MF-VAR forecast (no tuning, fewer draws)
#'
#' @param input Path to data file, data frame, or list
#' @param quarterly_vars Character vector (NULL = auto-detect)
#' @param flow_vars Character vector (NULL = all quarterly are flow)
#' @param forecast_horizon Integer, months ahead
#' @param output_dir Directory to save results (NULL = don't save)
#' @param verbose Logical
#' @return List with $data_prep, $results, $plots
#' @export
quick_forecast <- function(input,
                           quarterly_vars = NULL,
                           flow_vars = NULL,
                           forecast_horizon = 12,
                           output_dir = "quick_results",
                           verbose = TRUE) {
    if (verbose) {
        cat("\n")
        cat("╔", rep("═", 60), "╗\n", sep = "")
        cat("║", rep(" ", 15), "QUICK MF-VAR FORECAST", rep(" ", 24), "║\n", sep = "")
        cat("║", rep(" ", 12), "(Fast mode - no tuning)", rep(" ", 25), "║\n", sep = "")
        cat("╚", rep("═", 60), "╝\n", sep = "")
    }

    # Step 1: Prepare data
    if (verbose) cat("\n[1/3] Preparing data...\n")

    script_dir <- system.file("scripts", package = "mfvar2")
    source(file.path(script_dir, "01_prepare_data.R"))

    data_prep <- prepare_my_data(
        input = input,
        quarterly_vars = quarterly_vars,
        flow_vars = flow_vars,
        verbose = verbose
    )

    # Step 2: Quick estimation (no tuning, 1000 draws)
    if (verbose) {
        cat("\n[2/3] Running quick estimation...\n")
        cat("  Using default hyperparameters (λ₁=0.2, others=1)\n")
        cat("  MCMC: 1000 draws + 200 burn-in\n")
    }

    source(file.path(script_dir, "02_estimate_and_forecast.R"))

    results <- run_mfvar_forecasting(
        data_prep = data_prep,
        p = 2,
        tune_hyperparameters = FALSE, # Skip tuning!
        n_draws = 1000, # Fewer draws
        burnin = 200, # Shorter burn-in
        forecast_horizon = forecast_horizon,
        output_dir = if (!is.null(output_dir)) file.path(output_dir, "estimation") else NULL,
        verbose = verbose,
        seed = 42
    )

    # Step 3: Quick plots
    if (verbose) cat("\n[3/3] Creating plots...\n")

    source(file.path(script_dir, "03_visualize_results.R"))

    plots <- visualize_all_results(
        results = results,
        data_prep = data_prep,
        output_dir = if (!is.null(output_dir)) file.path(output_dir, "plots") else NULL,
        format = "png",
        verbose = verbose
    )

    # Summary
    if (verbose) {
        cat("\n")
        cat("=", rep("=", 60), "=\n", sep = "")
        cat("  QUICK FORECAST COMPLETE!\n")
        cat("=", rep("=", 60), "=\n", sep = "")
        cat("\n")
        cat("✓ Variables:  ", ncol(data_prep$data), "\n")
        cat("✓ Draws:      ", 800, "(1000 - 200 burn-in)\n")
        cat("✓ Horizon:    ", forecast_horizon, "months\n")
        cat("✓ Plots:      ", length(plots), "\n")
        if (!is.null(output_dir)) {
            cat("\nResults saved to:", output_dir, "\n")
        }
        cat("\n")
    }

    return(invisible(list(
        data_prep = data_prep,
        results = results,
        plots = plots
    )))
}


# Command-line interface
if (!interactive()) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) == 0 || args[1] %in% c("-h", "--help")) {
        cat("\n")
        cat("Quick MF-VAR Forecast\n")
        cat("=====================\n\n")
        cat("Usage: Rscript quick_forecast.R <data_file>\n\n")
        cat("Example:\n")
        cat("  Rscript quick_forecast.R data/my_data.csv\n\n")
        cat("This runs a fast forecast without hyperparameter tuning.\n")
        cat("For full pipeline with tuning, use 00_complete_workflow.R\n\n")
        quit(save = "no")
    }

    input_file <- args[1]

    if (!file.exists(input_file)) {
        cat("Error: File not found:", input_file, "\n")
        quit(save = "no", status = 1)
    }

    quick_forecast(
        input = input_file,
        output_dir = "quick_results",
        verbose = TRUE
    )
}
