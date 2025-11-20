#!/usr/bin/env Rscript

# Run full cross-validation for manual MF-VAR with lambda1=0.06
# This script runs CV with the manual implementation only and saves results
# that can be combined with other models later.

if (!interactive()) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg)) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
    script_dir <- dirname(script_path)
    project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
    setwd(project_root)
  }
}

message("Running full CV for MF-VAR (manual) with lambda1=0.06")
message("Working directory: ", getwd())

# Parse command line arguments
cli_args <- commandArgs(trailingOnly = TRUE)
fast_mode <- any(cli_args %in% c("--fast", "-f")) || identical(Sys.getenv("MFVAR_FAST"), "1")

# Force full mode (not fast) unless explicitly requested
if (fast_mode) {
  message("Note: Fast mode requested but we're running full CV as specified")
  fast_mode <- FALSE
}

# Load sources
source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "plotting.R"))
source(file.path("R", "evaluation.R"))
source(file.path("R", "latent_states.R"))
source(file.path("R", "mfvar2_adapter.R"))
source(file.path("R", "benchmark_shared.R"))
source(file.path("R", "benchmark_cv.R"))

# Setup project
activate_project()
all_pkgs <- unique(c(required_pkgs, "pkgload"))
load_required_packages(all_pkgs)

# Load mfvar2 package
if (!requireNamespace("mfvar2", quietly = TRUE)) {
  pkgload::load_all(file.path("models", "mfvar2"), export_all = FALSE, quiet = TRUE)
}

# Constants
DATA_DIR <- file.path(".", "data")
OUT_DIR <- file.path(".", "output", "benchmarks")
OUT_CSV_DIR <- file.path(OUT_DIR, "csv")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
if (!dir.exists(OUT_CSV_DIR)) dir.create(OUT_CSV_DIR, recursive = TRUE)

# Model configuration - only run manual MF-VAR
manual_model_label <- "MF-VAR (manual)"
models_to_run <- c(manual_model_label)
message("Running CV for: ", paste(models_to_run, collapse = ", "))

# CV configuration with lambda1=0.06 (FULL mode, not fast)
mfvar2_cv_config <- list(
  p = 2L,
  n_draws = 2000L,     # Full mode: 2000 draws (not 800)
  burnin = 700L,        # Full mode: 700 burnin (not 250)
  n_sim = 600L,         # Full mode: 600 simulations (not 250)
  hyperparameters = list(
    lambda1 = 0.06,     # Updated from 0.04 to 0.06 as requested
    lambda2 = 1.0,
    lambda3 = 1.0,
    lambda4 = 1.0,
    lambda5 = 1.0
  ),
  verbose = FALSE
)

message("Configuration:")
message("  - n_draws: ", mfvar2_cv_config$n_draws)
message("  - burnin: ", mfvar2_cv_config$burnin)
message("  - n_sim: ", mfvar2_cv_config$n_sim)
message("  - lambda1: ", mfvar2_cv_config$hyperparameters$lambda1)

# Forecast settings
forecast_steps <- c(1L, 4L)
history_quarters <- 4L
n_lags <- 5

# Load data
qdat_raw <- read_quarterly_data(DATA_DIR)

# Load KOF Barometer for reference
baro_raw <- fetch_kof_barometer()

# Load combined timeseries for monthly indicators
monthly_raw <- read_combined_timeseries(
  DATA_DIR,
  variables = resolve_monthly_indicators()
)

# Apply publication lags
monthly_raw$ts_list <- apply_publication_lags(monthly_raw$ts_list, monthly_publication_lags)

# Trim quarterly data to overlap with monthly indicators
early_strategy <- getOption("mfvar.early_monthly", Sys.getenv("MFVAR_EARLY_MONTHLY", "fill"))
message("Early monthly data handling strategy: ", early_strategy)

trimmed <- trim_to_overlap(
  qdat_raw,
  monthly_raw$ts_list,
  mode = "ragged",
  fill_method = "locf",
  start_strategy = early_strategy
)
qdat_orig <- trimmed$qdat
monthly_series_list <- trimmed$monthly

# Prepare KOF Barometer
baro_ts <- window_baro(baro_raw, qdat_orig, end_mode = "available")

# Stationarize
stationary <- stationarise_quarterly(qdat_orig)
qdat_adj <- stationary$data
transforms <- stationary$transforms

# Prepare barometer differences
baro_diff_series <- local({
  first_quarter_date <- zoo::as.Date(qdat_orig$qtr[1], frac = 0)
  prev_year <- lubridate::year(first_quarter_date)
  prev_month <- (lubridate::quarter(first_quarter_date) - 1L) * months_per_quarter
  if (prev_month == 0L) {
    prev_month <- 12L
    prev_year <- prev_year - 1L
  }

  diff_source <- stats::window(baro_ts, start = c(prev_year, prev_month))
  diff_series <- base::diff(diff_source)
  first_month_of_quarter <- (lubridate::quarter(first_quarter_date) - 1L) * months_per_quarter + 1L
  stats::window(
    diff_series,
    start = c(lubridate::year(first_quarter_date), first_month_of_quarter)
  )
})

# Target variables
target_vars <- target_variables
y_ts_list <- build_q_ts(qdat_orig)

message("\nStarting cross-validation...")
message("Target variables: ", paste(target_vars, collapse = ", "))
message("Forecast steps: ", paste(forecast_steps, collapse = ", "))

# CV settings
cv_initial_quarter <- zoo::as.yearqtr("2015 Q4")
cv_extra_months <- 0:2  # Test all three coverage options

# Override CV folds if specified
max_folds_override <- NA_integer_
max_folds_arg <- cli_args[grepl("^--max-folds=", cli_args)]
if (length(max_folds_arg)) {
  max_folds_override <- suppressWarnings(as.integer(sub("^--max-folds=", "", max_folds_arg[1])))
}

cv_max_folds <- 10L  # Default to 10 folds for full CV
if (!is.na(max_folds_override)) {
  cv_max_folds <- max_folds_override
  message("Using custom max folds: ", cv_max_folds)
} else {
  message("Using default max folds: ", cv_max_folds)
}

# Run cross-validation
cv_start_time <- Sys.time()

cv_output <- run_benchmark_cross_validation(
  qdat_adj = qdat_adj,
  qdat_orig = qdat_orig,
  transforms = transforms,
  monthly_series_list = monthly_series_list,
  baro_diff_series = baro_diff_series,
  y_ts_list = y_ts_list,
  target_vars = target_vars,
  forecast_steps = forecast_steps,
  n_lags = n_lags,
  data_dir = DATA_DIR,
  extra_months_options = cv_extra_months,
  max_folds = cv_max_folds,
  initial_train_quarter = cv_initial_quarter,
  models_to_run = models_to_run,
  mfvar2_opts = mfvar2_cv_config,
  progress = TRUE
)

cv_elapsed <- difftime(Sys.time(), cv_start_time, units = "mins")
message(sprintf("\n✓ Cross-validation completed in %.1f minutes", as.numeric(cv_elapsed)))

# Extract results
cv_results <- cv_output$results
cv_folds_tbl <- cv_output$folds

if (!nrow(cv_results)) {
  stop("No CV results generated!")
}

message(sprintf("Generated %d predictions across %d folds", nrow(cv_results), cv_output$fold_count))

# Calculate metrics
filtered_results <- cv_results |>
  dplyr::filter(.data$step_ahead %in% forecast_steps)

cv_metrics_tbl <- filtered_results |>
  dplyr::group_by(extra_months, variable, model, horizon) |>
  summarise_metrics() |>
  dplyr::arrange(extra_months, variable, model, horizon)

cv_metrics_overall <- filtered_results |>
  dplyr::group_by(extra_months, variable, model) |>
  summarise_metrics() |>
  dplyr::arrange(extra_months, variable, model)

message("\nCV Metrics Summary:")
print(cv_metrics_tbl)

# Save outputs
output_prefix <- "mfvar_manual_lambda006"

# Add coverage labels
cv_metrics_export <- cv_metrics_tbl |>
  dplyr::mutate(monthly_coverage = coverage_label(extra_months)) |>
  dplyr::relocate(monthly_coverage, .before = model)

cv_results_export <- cv_results |>
  dplyr::mutate(monthly_coverage = coverage_label(extra_months)) |>
  dplyr::rename(fold = fold_index) |>
  dplyr::relocate(monthly_coverage, extra_months, fold, cutoff_quarter, forecast_quarter, .before = variable) |>
  dplyr::arrange(extra_months, fold, variable, step_ahead)

# Write CSV files
readr::write_csv(
  cv_metrics_export,
  file.path(OUT_CSV_DIR, paste0(output_prefix, "_cv_metrics.csv"))
)
readr::write_csv(
  cv_results_export,
  file.path(OUT_CSV_DIR, paste0(output_prefix, "_cv_predictions.csv"))
)

# Also save timings if available
cv_timings_tbl <- if (!is.null(cv_output$timings$per_fold)) cv_output$timings$per_fold else tibble::tibble()
if (nrow(cv_timings_tbl)) {
  readr::write_csv(
    cv_timings_tbl,
    file.path(OUT_CSV_DIR, paste0(output_prefix, "_cv_timings.csv"))
  )
}

# Create a summary markdown file
summary_lines <- c(
  sprintf("# MF-VAR (manual) Cross-Validation Results - lambda1=%.2f", 
          mfvar2_cv_config$hyperparameters$lambda1),
  "",
  "## Configuration",
  "",
  sprintf("- Lambda1: %.2f", mfvar2_cv_config$hyperparameters$lambda1),
  sprintf("- Lambda2: %.1f", mfvar2_cv_config$hyperparameters$lambda2),
  sprintf("- Lambda3: %.1f", mfvar2_cv_config$hyperparameters$lambda3),
  sprintf("- Lambda4: %.1f", mfvar2_cv_config$hyperparameters$lambda4),
  sprintf("- Lambda5: %.1f", mfvar2_cv_config$hyperparameters$lambda5),
  sprintf("- Draws: %d", mfvar2_cv_config$n_draws),
  sprintf("- Burnin: %d", mfvar2_cv_config$burnin),
  sprintf("- Simulations: %d", mfvar2_cv_config$n_sim),
  "",
  "## Results",
  "",
  sprintf("- Total predictions: %d", nrow(cv_results)),
  sprintf("- Folds: %d", cv_output$fold_count),
  sprintf("- Coverage options: %s", paste(cv_extra_months, collapse = ", ")),
  sprintf("- Runtime: %.1f minutes", as.numeric(cv_elapsed)),
  "",
  "## Metrics by Variable and Coverage",
  ""
)

# Add metrics table
for (var in target_vars) {
  var_metrics <- cv_metrics_export |>
    dplyr::filter(variable == var)
  
  if (nrow(var_metrics)) {
    summary_lines <- c(
      summary_lines,
      sprintf("### %s", var),
      "",
      "| Coverage | Horizon | RMSE | MAE | Observations |",
      "|----------|---------|------|-----|--------------|"
    )
    
    for (i in seq_len(nrow(var_metrics))) {
      row <- var_metrics[i, ]
      summary_lines <- c(
        summary_lines,
        sprintf("| %s | %s | %.4f | %.4f | %d |",
                row$monthly_coverage,
                row$horizon,
                row$rmse,
                row$mae,
                row$observations)
      )
    }
    summary_lines <- c(summary_lines, "")
  }
}

readr::write_lines(
  summary_lines,
  file.path(OUT_DIR, paste0(output_prefix, "_summary.md"))
)

# Final message
message("\n" , paste(rep("=", 60), collapse = ""))
message("MF-VAR (manual) CV complete with lambda1=0.06")
message(paste(rep("=", 60), collapse = ""))
message("\nOutput files:")
message("  - ", file.path(OUT_CSV_DIR, paste0(output_prefix, "_cv_predictions.csv")))
message("  - ", file.path(OUT_CSV_DIR, paste0(output_prefix, "_cv_metrics.csv")))
if (nrow(cv_timings_tbl)) {
  message("  - ", file.path(OUT_CSV_DIR, paste0(output_prefix, "_cv_timings.csv")))
}
message("  - ", file.path(OUT_DIR, paste0(output_prefix, "_summary.md")))
message("\nThese outputs can be combined with other model CV results.")
