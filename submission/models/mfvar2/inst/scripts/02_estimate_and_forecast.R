#!/usr/bin/env Rscript
#
# 02_estimate_and_forecast.R
# ===========================
# Estimate MF-VAR model and generate forecasts
#
# This script:
# - Optionally tunes hyperparameters via grid search
# - Estimates posterior using Gibbs sampler
# - Generates out-of-sample forecasts
# - Saves all results
#
# Usage:
#   Rscript 02_estimate_and_forecast.R [--help]
#
# Or source in R:
#   source("inst/scripts/02_estimate_and_forecast.R")
#   results <- run_mfvar_forecasting(
#     data_prep = my_data_prep,
#     forecast_horizon = 12
#   )

library(mfvar2)

#' Run complete MF-VAR estimation and forecasting pipeline
#'
#' @param data_prep Output from prepare_my_data() or prepare_data_snb()
#' @param p Integer, lag order (default 2)
#' @param tune_hyperparameters Logical, run grid search (default TRUE, takes 10-30 min)
#' @param hyperparameters Manual hyperparameters if tune_hyperparameters=FALSE
#' @param lambda1_grid Grid for overall tightness (if tuning)
#' @param lambda2_grid Grid for lag decay (if tuning)
#' @param lambda3_grid Grid for error variance prior (if tuning)
#' @param lambda4_grid Grid for sum-of-coefficients (if tuning)
#' @param lambda5_grid Grid for co-persistence (if tuning)
#' @param n_gibbs_mdd Draws for MDD computation during tuning
#' @param burnin_mdd Burn-in for MDD computation during tuning
#' @param n_draws Posterior draws to collect
#' @param burnin Burn-in draws to discard
#' @param thinning Keep every nth draw (1 = no thinning)
#' @param forecast_horizon Integer, months ahead to forecast
#' @param n_sim Number of forecast simulations per posterior draw
#' @param output_dir Directory to save results (NULL = don't save)
#' @param verbose Logical, print progress
#' @param seed Integer, random seed for reproducibility
#' @return List with $posterior, $forecasts, $hyperparameters, $diagnostics
#' @export
run_mfvar_forecasting <- function(data_prep,
                                  p = 2,
                                  tune_hyperparameters = TRUE,
                                  hyperparameters = NULL,
                                  lambda1_grid = c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5),
                                  lambda2_grid = c(1, 2, 3, 4, 5),
                                  lambda3_grid = c(1),
                                  lambda4_grid = c(1, 2, 3, 4, 5),
                                  lambda5_grid = c(1, 2, 3, 4, 5),
                                  n_gibbs_mdd = 2000,
                                  burnin_mdd = 1000,
                                  n_draws = 4000,
                                  burnin = 1000,
                                  thinning = 1,
                                  forecast_horizon = 12,
                                  n_sim = 1000,
                                  output_dir = NULL,
                                  verbose = TRUE,
                                  seed = 42) {
    if (verbose) {
        cat("\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("  MF-VAR Estimation & Forecasting Pipeline\n")
        cat("=", rep("=", 70), "=\n", sep = "")
    }

    # Set seed for reproducibility
    set.seed(seed)

    # Step 1: Hyperparameter tuning (optional)
    if (tune_hyperparameters) {
        if (verbose) {
            cat("\n[1/3] Tuning hyperparameters...\n")
            n_combos <- length(lambda1_grid) * length(lambda2_grid) *
                length(lambda3_grid) * length(lambda4_grid) * length(lambda5_grid)
            cat("  Grid search over", n_combos, "combinations\n")
            cat("  This may take 10-30 minutes...\n")
        }

        hyperparams <- tune_minnesota_hyper(
            data_prepared = data_prep,
            p = p,
            lambda1_grid = lambda1_grid,
            lambda2_grid = lambda2_grid,
            lambda3_grid = lambda3_grid,
            lambda4_grid = lambda4_grid,
            lambda5_grid = lambda5_grid,
            n_gibbs_mdd = n_gibbs_mdd,
            burnin_mdd = burnin_mdd,
            verbose = verbose,
            seed = seed
        )

        if (verbose) {
            cat("\n  Optimal hyperparameters:\n")
            cat("    λ₁ (tightness):       ", sprintf("%.4f", hyperparams$lambda1_optimal), "\n")
            cat("    λ₂ (lag decay):       ", sprintf("%.4f", hyperparams$lambda2_optimal), "\n")
            cat("    λ₃ (Σ prior):         ", sprintf("%.4f", hyperparams$lambda3_optimal), "\n")
            cat("    λ₄ (sum-of-coef):     ", sprintf("%.4f", hyperparams$lambda4_optimal), "\n")
            cat("    λ₅ (co-persistence):  ", sprintf("%.4f", hyperparams$lambda5_optimal), "\n")
            cat("    Log MDD:              ", sprintf("%.2f", hyperparams$log_mdd), "\n")
        }
    } else {
        if (verbose) cat("\n[1/3] Using manual hyperparameters (no tuning)\n")

        if (is.null(hyperparameters)) {
            # Use defaults
            hyperparams <- list(
                lambda1_optimal = 0.2,
                lambda2_optimal = 1,
                lambda3_optimal = 1,
                lambda4_optimal = 1,
                lambda5_optimal = 1
            )
            if (verbose) cat("  Using defaults: λ₁=0.2, λ₂=1, λ₃=1, λ₄=1, λ₅=1\n")
        } else {
            hyperparams <- hyperparameters
            if (verbose) {
                cat("  Using provided:\n")
                cat("    λ₁:", hyperparams$lambda1_optimal, "\n")
                cat("    λ₂:", hyperparams$lambda2_optimal, "\n")
                cat("    λ₃:", hyperparams$lambda3_optimal, "\n")
                cat("    λ₄:", hyperparams$lambda4_optimal, "\n")
                cat("    λ₅:", hyperparams$lambda5_optimal, "\n")
            }
        }
    }

    # Step 2: Estimate posterior
    if (verbose) {
        cat("\n[2/3] Estimating MF-VAR posterior...\n")
        cat("  MCMC:", n_draws, "draws +", burnin, "burn-in\n")
        if (thinning > 1) {
            cat("  Thinning: keeping every", thinning, "draw\n")
        }
    }

    posterior <- estimate_mf_bvar(
        data_prepared = data_prep,
        p = p,
        hyperparameters = hyperparams,
        n_draws = n_draws,
        burnin = burnin,
        thinning = thinning,
        verbose = verbose,
        seed = seed
    )

    # Step 3: Generate forecasts
    if (verbose) {
        cat("\n[3/3] Generating forecasts...\n")
        cat("  Horizon:", forecast_horizon, "months\n")
        cat("  Simulations:", n_sim, "per posterior draw\n")
    }

    forecasts <- forecast_mf_bvar(
        posterior = posterior,
        horizon_months = forecast_horizon,
        n_sim = n_sim,
        quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
        seed = seed
    )

    # Compile results
    results <- list(
        posterior = posterior,
        forecasts = forecasts,
        hyperparameters = hyperparams,
        diagnostics = posterior$diagnostics,
        settings = list(
            p = p,
            n_draws = n_draws,
            burnin = burnin,
            thinning = thinning,
            forecast_horizon = forecast_horizon,
            n_sim = n_sim,
            seed = seed
        )
    )

    # Save results
    if (!is.null(output_dir)) {
        if (verbose) cat("\nSaving results...\n")

        if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
        }

        # Save complete results
        saveRDS(results, file.path(output_dir, "mfvar_results.rds"))
        if (verbose) cat("  Complete results:", file.path(output_dir, "mfvar_results.rds"), "\n")

        # Save forecasts separately (for easy access)
        saveRDS(forecasts, file.path(output_dir, "forecasts.rds"))
        if (verbose) cat("  Forecasts:", file.path(output_dir, "forecasts.rds"), "\n")

        # Save hyperparameters
        saveRDS(hyperparams, file.path(output_dir, "hyperparameters.rds"))
        if (verbose) cat("  Hyperparameters:", file.path(output_dir, "hyperparameters.rds"), "\n")

        # Save summary statistics as CSV
        summary_df <- data.frame(
            setting = c(
                "lag_order", "variables", "observations", "draws", "burnin",
                "forecast_horizon", "lambda1", "lambda2", "lambda3", "lambda4", "lambda5"
            ),
            value = c(
                p, ncol(data_prep$data), nrow(data_prep$data), n_draws, burnin,
                forecast_horizon, hyperparams$lambda1_optimal, hyperparams$lambda2_optimal,
                hyperparams$lambda3_optimal, hyperparams$lambda4_optimal, hyperparams$lambda5_optimal
            )
        )
        write.csv(summary_df, file.path(output_dir, "summary.csv"), row.names = FALSE)
        if (verbose) cat("  Summary:", file.path(output_dir, "summary.csv"), "\n")
    }

    if (verbose) {
        cat("\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("  Estimation & forecasting complete!\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("\n")
        cat("Results stored in output object:\n")
        cat("  $posterior      - Posterior draws and estimates\n")
        cat("  $forecasts      - Forecast quantiles and paths\n")
        cat("  $hyperparameters - Selected hyperparameters\n")
        cat("  $diagnostics    - Convergence statistics\n")
        cat("  $settings       - Estimation settings\n")
        cat("\n")
    }

    return(results)
}


# Example usage if run as script
if (!interactive()) {
    cat("\n")
    cat("MF-VAR Estimation & Forecasting Script\n")
    cat("======================================\n\n")
    cat("This script estimates the MF-VAR model and generates forecasts.\n\n")
    cat("Usage examples:\n\n")
    cat("1. Full pipeline with hyperparameter tuning:\n")
    cat('   data_prep <- readRDS("data/prepared_data.rds")\n')
    cat("   results <- run_mfvar_forecasting(\n")
    cat("     data_prep = data_prep,\n")
    cat("     forecast_horizon = 12,\n")
    cat('     output_dir = "results"\n')
    cat("   )\n\n")
    cat("2. Quick run without tuning (uses defaults):\n")
    cat("   results <- run_mfvar_forecasting(\n")
    cat("     data_prep = data_prep,\n")
    cat("     tune_hyperparameters = FALSE,\n")
    cat("     n_draws = 1000,\n")
    cat("     burnin = 200\n")
    cat("   )\n\n")
    cat("3. With custom hyperparameters:\n")
    cat("   my_hyper <- list(\n")
    cat("     lambda1_optimal = 0.1,\n")
    cat("     lambda2_optimal = 1,\n")
    cat("     lambda3_optimal = 1,\n")
    cat("     lambda4_optimal = 1,\n")
    cat("     lambda5_optimal = 1\n")
    cat("   )\n")
    cat("   results <- run_mfvar_forecasting(\n")
    cat("     data_prep = data_prep,\n")
    cat("     tune_hyperparameters = FALSE,\n")
    cat("     hyperparameters = my_hyper\n")
    cat("   )\n\n")
}
