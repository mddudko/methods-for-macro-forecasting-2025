#!/usr/bin/env Rscript
#
# 03_visualize_results.R
# =======================
# Create comprehensive visualizations of MF-VAR results
#
# This script:
# - Creates forecast fan charts for all variables
# - Plots historical data with forecast overlay
# - Shows posterior diagnostics (trace plots, etc.)
# - Compares forecast quantiles
# - Saves all plots to files
#
# Usage:
#   Rscript 03_visualize_results.R [--help]
#
# Or source in R:
#   source("inst/scripts/03_visualize_results.R")
#   plots <- visualize_all_results(results)

library(mfvar2)
library(ggplot2)

#' Create comprehensive visualizations of MF-VAR results
#'
#' @param results Output from run_mfvar_forecasting()
#' @param data_prep Original prepared data (for historical context)
#' @param variables Character vector of variables to plot (NULL = all)
#' @param output_dir Directory to save plots (NULL = display only)
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param format Output format ("png", "pdf", "both")
#' @param verbose Logical, print progress
#' @return List of ggplot objects
#' @export
visualize_all_results <- function(results,
                                  data_prep = NULL,
                                  variables = NULL,
                                  output_dir = NULL,
                                  width = 10,
                                  height = 6,
                                  format = "png",
                                  verbose = TRUE) {
    if (verbose) {
        cat("\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("  MF-VAR Results Visualization\n")
        cat("=", rep("=", 70), "=\n", sep = "")
    }

    # Extract components
    forecasts <- results$forecasts
    posterior <- results$posterior

    # Get variable names
    if (is.null(variables)) {
        variables <- colnames(forecasts$forecasts_monthly[[1]])
    }

    plots <- list()

    # Create output directory if saving
    if (!is.null(output_dir) && !dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # 1. Forecast fan charts
    if (verbose) cat("\n[1/4] Creating forecast fan charts...\n")

    for (var in variables) {
        if (verbose) cat("  ", var, "\n")

        p <- plot(forecasts, var = var)
        plots[[paste0("forecast_", var)]] <- p

        if (!is.null(output_dir)) {
            filename_base <- file.path(output_dir, paste0("forecast_", var))
            if (format %in% c("png", "both")) {
                ggsave(paste0(filename_base, ".png"), p, width = width, height = height)
            }
            if (format %in% c("pdf", "both")) {
                ggsave(paste0(filename_base, ".pdf"), p, width = width, height = height)
            }
        }
    }

    # 2. Historical data with forecast overlay
    if (!is.null(data_prep) && verbose) {
        cat("\n[2/4] Creating historical + forecast plots...\n")

        # Get historical data
        hist_data <- data_prep$data
        forecast_start <- zoo::index(hist_data)[nrow(hist_data)]

        for (var in variables) {
            if (verbose) cat("  ", var, "\n")

            # Build combined data for plotting
            fc_monthly <- forecasts$forecasts_monthly
            fc_median <- sapply(fc_monthly, function(x) x[, var]["50%"])
            fc_lower <- sapply(fc_monthly, function(x) x[, var]["5%"])
            fc_upper <- sapply(fc_monthly, function(x) x[, var]["95%"])

            # Create forecast dates
            fc_dates <- seq(forecast_start, by = "month", length.out = length(fc_median) + 1)[-1]

            # Historical data
            hist_dates <- zoo::index(hist_data)
            hist_vals <- as.numeric(hist_data[, var])

            # Combine for plotting
            plot_df <- data.frame(
                date = c(hist_dates, fc_dates),
                value = c(hist_vals, rep(NA, length(fc_median))),
                forecast = c(rep(NA, length(hist_vals)), fc_median),
                lower = c(rep(NA, length(hist_vals)), fc_lower),
                upper = c(rep(NA, length(hist_vals)), fc_upper),
                type = c(rep("Historical", length(hist_vals)), rep("Forecast", length(fc_median)))
            )

            p <- ggplot(plot_df) +
                geom_line(aes(x = date, y = value), color = "black", linewidth = 0.8) +
                geom_line(aes(x = date, y = forecast), color = "blue", linewidth = 0.8) +
                geom_ribbon(aes(x = date, ymin = lower, ymax = upper),
                    fill = "blue", alpha = 0.2
                ) +
                geom_vline(xintercept = forecast_start, linetype = "dashed", color = "red") +
                labs(
                    title = paste(var, "- Historical & Forecast"),
                    subtitle = "90% forecast interval shown",
                    x = "Date",
                    y = var
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(face = "bold", size = 14),
                    plot.subtitle = element_text(size = 10)
                )

            plots[[paste0("history_forecast_", var)]] <- p

            if (!is.null(output_dir)) {
                filename_base <- file.path(output_dir, paste0("history_forecast_", var))
                if (format %in% c("png", "both")) {
                    ggsave(paste0(filename_base, ".png"), p, width = width, height = height)
                }
                if (format %in% c("pdf", "both")) {
                    ggsave(paste0(filename_base, ".pdf"), p, width = width, height = height)
                }
            }
        }
    } else if (is.null(data_prep)) {
        if (verbose) cat("\n[2/4] Skipping historical plots (no data_prep provided)\n")
    }

    # 3. Convergence diagnostics
    if (verbose) cat("\n[3/4] Creating diagnostic plots...\n")

    # Geweke z-scores
    geweke_df <- data.frame(
        parameter = names(posterior$diagnostics$geweke_z),
        z_score = posterior$diagnostics$geweke_z
    )
    geweke_df$pass <- abs(geweke_df$z_score) < 2

    p_geweke <- ggplot(geweke_df, aes(x = z_score, y = parameter, color = pass)) +
        geom_point(size = 3) +
        geom_vline(xintercept = c(-2, 2), linetype = "dashed", color = "red") +
        scale_color_manual(
            values = c("TRUE" = "green", "FALSE" = "red"),
            labels = c("TRUE" = "Pass", "FALSE" = "Fail")
        ) +
        labs(
            title = "Geweke Convergence Diagnostic",
            subtitle = "Z-scores should be within [-2, 2]",
            x = "Z-score",
            y = "Parameter",
            color = "Status"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))

    plots[["diagnostics_geweke"]] <- p_geweke

    if (!is.null(output_dir)) {
        filename_base <- file.path(output_dir, "diagnostics_geweke")
        if (format %in% c("png", "both")) {
            ggsave(paste0(filename_base, ".png"), p_geweke, width = width, height = height)
        }
        if (format %in% c("pdf", "both")) {
            ggsave(paste0(filename_base, ".pdf"), p_geweke, width = width, height = height)
        }
    }

    # Effective sample size
    ess_df <- data.frame(
        parameter = names(posterior$diagnostics$eff_sample_size),
        ess = posterior$diagnostics$eff_sample_size
    )
    ess_df$adequate <- ess_df$ess > 1000

    p_ess <- ggplot(ess_df, aes(x = ess, y = parameter, color = adequate)) +
        geom_point(size = 3) +
        geom_vline(xintercept = 1000, linetype = "dashed", color = "red") +
        scale_color_manual(
            values = c("TRUE" = "green", "FALSE" = "orange"),
            labels = c("TRUE" = "Good (>1000)", "FALSE" = "Low (<1000)")
        ) +
        labs(
            title = "Effective Sample Size",
            subtitle = "Higher is better (>1000 recommended)",
            x = "Effective Sample Size",
            y = "Parameter",
            color = "Status"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))

    plots[["diagnostics_ess"]] <- p_ess

    if (!is.null(output_dir)) {
        filename_base <- file.path(output_dir, "diagnostics_ess")
        if (format %in% c("png", "both")) {
            ggsave(paste0(filename_base, ".png"), p_ess, width = width, height = height)
        }
        if (format %in% c("pdf", "both")) {
            ggsave(paste0(filename_base, ".pdf"), p_ess, width = width, height = height)
        }
    }

    # 4. Forecast comparison across variables
    if (verbose) cat("\n[4/4] Creating forecast comparison plot...\n")

    # Build comparison data
    fc_monthly <- forecasts$forecasts_monthly
    comparison_data <- lapply(variables, function(var) {
        data.frame(
            variable = var,
            horizon = 1:length(fc_monthly),
            median = sapply(fc_monthly, function(x) x[, var]["50%"]),
            lower = sapply(fc_monthly, function(x) x[, var]["5%"]),
            upper = sapply(fc_monthly, function(x) x[, var]["95%"])
        )
    })
    comparison_df <- do.call(rbind, comparison_data)

    p_comparison <- ggplot(comparison_df, aes(x = horizon, y = median, color = variable, fill = variable)) +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
        labs(
            title = "Forecast Comparison Across Variables",
            subtitle = "Median forecast with 90% intervals",
            x = "Horizon (months)",
            y = "Value",
            color = "Variable",
            fill = "Variable"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )

    plots[["forecast_comparison"]] <- p_comparison

    if (!is.null(output_dir)) {
        filename_base <- file.path(output_dir, "forecast_comparison")
        if (format %in% c("png", "both")) {
            ggsave(paste0(filename_base, ".png"), p_comparison, width = width, height = height * 1.2)
        }
        if (format %in% c("pdf", "both")) {
            ggsave(paste0(filename_base, ".pdf"), p_comparison, width = width, height = height * 1.2)
        }
    }

    if (verbose) {
        cat("\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("  Visualization complete!\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("\n")
        cat("Created", length(plots), "plots:\n")
        for (name in names(plots)) {
            cat("  -", name, "\n")
        }
        if (!is.null(output_dir)) {
            cat("\nPlots saved to:", output_dir, "\n")
        }
        cat("\n")
    }

    return(invisible(plots))
}


# Example usage if run as script
if (!interactive()) {
    cat("\n")
    cat("MF-VAR Results Visualization Script\n")
    cat("===================================\n\n")
    cat("This script creates comprehensive plots of estimation results.\n\n")
    cat("Usage examples:\n\n")
    cat("1. Visualize with data prep (shows historical + forecast):\n")
    cat('   results <- readRDS("results/mfvar_results.rds")\n')
    cat('   data_prep <- readRDS("data/prepared_data.rds")\n')
    cat("   plots <- visualize_all_results(\n")
    cat("     results = results,\n")
    cat("     data_prep = data_prep,\n")
    cat('     output_dir = "plots"\n')
    cat("   )\n\n")
    cat("2. Visualize without historical context:\n")
    cat("   plots <- visualize_all_results(\n")
    cat("     results = results,\n")
    cat('     output_dir = "plots"\n')
    cat("   )\n\n")
    cat("3. Select specific variables and save as PDF:\n")
    cat("   plots <- visualize_all_results(\n")
    cat("     results = results,\n")
    cat("     data_prep = data_prep,\n")
    cat('     variables = c("GDP", "CPI"),\n')
    cat('     output_dir = "plots",\n')
    cat('     format = "pdf"\n')
    cat("   )\n\n")
}
