#!/usr/bin/env Rscript
#
# 01_prepare_data.R
# =================
# Prepare data for MF-VAR estimation from various input formats
#
# This script handles:
# - CSV files, Excel files, RDS files, data frames
# - Automatic detection of quarterly vs monthly variables
# - Flow vs stock classification for quarterly variables
# - Unit root testing and differencing
# - Data validation and cleaning
#
# Usage:
#   Rscript 01_prepare_data.R [--help]
#
# Or source in R:
#   source("inst/scripts/01_prepare_data.R")
#   data_prep <- prepare_my_data(
#     input = "path/to/data.csv",
#     quarterly_vars = c("GDP", "Consumption"),
#     flow_vars = c("GDP", "Consumption")
#   )

library(mfvar2)

#' Prepare data for MF-VAR from various input formats
#'
#' @param input Character path to file(s), data frame, or list of data frames
#' @param quarterly_vars Character vector of quarterly variable names (NULL = auto-detect)
#' @param flow_vars Character vector of flow quarterly variables (NULL = all quarterly are flow)
#' @param start_date Character or Date, start of sample (NULL = earliest available)
#' @param end_date Character or Date, end of sample (NULL = latest available)
#' @param log_transform Character vector of variables to log (NULL = auto-detect)
#' @param difference Logical, apply differencing based on unit root tests (default TRUE)
#' @param output_path Character path to save prepared data (NULL = don't save)
#' @param verbose Logical, print progress messages
#' @return List with $data (zoo object), $metadata, $transformation_params
#' @export
prepare_my_data <- function(input,
                            quarterly_vars = NULL,
                            flow_vars = NULL,
                            start_date = NULL,
                            end_date = NULL,
                            log_transform = NULL,
                            difference = TRUE,
                            output_path = NULL,
                            verbose = TRUE) {
    if (verbose) {
        cat("\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("  MF-VAR Data Preparation\n")
        cat("=", rep("=", 70), "=\n", sep = "")
    }

    # Step 1: Load data from input
    if (verbose) cat("\n[1/4] Loading data...\n")

    if (is.character(input)) {
        # File path(s) provided
        if (length(input) == 1) {
            ext <- tolower(tools::file_ext(input))
            if (ext == "csv") {
                data_raw <- read.csv(input, stringsAsFactors = FALSE)
            } else if (ext %in% c("xlsx", "xls")) {
                if (!requireNamespace("readxl", quietly = TRUE)) {
                    stop("Package 'readxl' needed for Excel files. Install with: install.packages('readxl')")
                }
                data_raw <- readxl::read_excel(input)
            } else if (ext == "rds") {
                data_raw <- readRDS(input)
            } else {
                stop("Unsupported file type: ", ext, ". Use CSV, Excel, or RDS.")
            }
            if (verbose) cat("  Loaded:", input, "\n")
        } else {
            # Multiple files
            data_raw <- lapply(input, function(f) {
                ext <- tolower(tools::file_ext(f))
                if (ext == "csv") {
                    read.csv(f, stringsAsFactors = FALSE)
                } else if (ext %in% c("xlsx", "xls")) {
                    if (!requireNamespace("readxl", quietly = TRUE)) {
                        stop("Package 'readxl' needed for Excel files.")
                    }
                    readxl::read_excel(f)
                } else if (ext == "rds") {
                    readRDS(f)
                } else {
                    stop("Unsupported file type: ", ext)
                }
            })
            if (verbose) cat("  Loaded", length(input), "files\n")
        }
    } else if (is.data.frame(input)) {
        data_raw <- input
        if (verbose) cat("  Using provided data frame\n")
    } else if (is.list(input)) {
        data_raw <- input
        if (verbose) cat("  Using provided list of data frames\n")
    } else {
        stop("Input must be file path(s), data frame, or list of data frames")
    }

    # Step 2: Call prepare_data_snb
    if (verbose) cat("\n[2/4] Processing with prepare_data_snb()...\n")

    data_prep <- prepare_data_snb(
        data_sources = data_raw,
        quarterly_vars = quarterly_vars,
        flow_vars = flow_vars,
        start_date = start_date,
        end_date = end_date,
        log_transform = log_transform,
        difference = difference,
        verbose = verbose
    )

    # Step 3: Validate
    if (verbose) {
        cat("\n[3/4] Validation...\n")
        cat("  Variables:", ncol(data_prep$data), "\n")
        cat("  Observations:", nrow(data_prep$data), "\n")
        cat(
            "  Sample period:", format(zoo::index(data_prep$data)[1]), "to",
            format(zoo::index(data_prep$data)[nrow(data_prep$data)]), "\n"
        )

        if (!is.null(data_prep$metadata$quarterly_vars)) {
            cat("  Quarterly vars:", paste(data_prep$metadata$quarterly_vars, collapse = ", "), "\n")
            cat("  Flow vars:", paste(data_prep$metadata$flow_vars, collapse = ", "), "\n")
        }

        # Check for NAs
        na_counts <- colSums(is.na(data_prep$data))
        if (any(na_counts > 0)) {
            cat("\n  Warning: Missing values detected:\n")
            for (v in names(na_counts)[na_counts > 0]) {
                cat("    ", v, ":", na_counts[v], "NAs\n")
            }
        } else {
            cat("  No missing values ✓\n")
        }
    }

    # Step 4: Save if requested
    if (!is.null(output_path)) {
        if (verbose) cat("\n[4/4] Saving prepared data...\n")
        saveRDS(data_prep, output_path)
        if (verbose) cat("  Saved to:", output_path, "\n")
    } else {
        if (verbose) cat("\n[4/4] Skipping save (no output_path specified)\n")
    }

    if (verbose) {
        cat("\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("  Data preparation complete!\n")
        cat("=", rep("=", 70), "=\n", sep = "")
        cat("\n")
    }

    return(data_prep)
}


# Example usage if run as script
if (!interactive()) {
    cat("\n")
    cat("MF-VAR Data Preparation Script\n")
    cat("==============================\n\n")
    cat("This script prepares data for MF-VAR estimation.\n\n")
    cat("Usage examples:\n\n")
    cat("1. From CSV file:\n")
    cat("   data_prep <- prepare_my_data(\n")
    cat('     input = "data/my_data.csv",\n')
    cat('     quarterly_vars = c("GDP", "Consumption"),\n')
    cat('     flow_vars = c("GDP", "Consumption"),\n')
    cat('     output_path = "data/prepared_data.rds"\n')
    cat("   )\n\n")
    cat("2. From multiple CSV files:\n")
    cat("   data_prep <- prepare_my_data(\n")
    cat('     input = c("data/monthly.csv", "data/quarterly.csv"),\n')
    cat('     output_path = "data/prepared_data.rds"\n')
    cat("   )\n\n")
    cat("3. From data frame:\n")
    cat('   df <- read.csv("data/my_data.csv")\n')
    cat("   data_prep <- prepare_my_data(input = df)\n\n")
    cat("4. Auto-detect everything:\n")
    cat("   data_prep <- prepare_my_data(\n")
    cat('     input = "data/my_data.csv"\n')
    cat("   )  # Auto-detects quarterly vars, flow vars, log transforms\n\n")
}
