#!/usr/bin/env Rscript

# Cross-validation runner for manual MF-VAR (mfvar2 package)
# Uses the benchmark CV infrastructure to evaluate manual implementation

source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "benchmark_cv.R"))
source(file.path("R", "mfvar2_adapter.R"))

activate_project()
load_required_packages(c(required_pkgs, "pkgload"))

pkgload::load_all(file.path("models", "mfvar2"), export_all = FALSE, quiet = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)

# Parse command-line arguments
max_folds_override <- NA_integer_
max_folds_arg <- cli_args[grepl("^--max-folds=", cli_args)]
if (length(max_folds_arg)) {
  max_folds_override <- suppressWarnings(as.integer(sub("^--max-folds=", "", max_folds_arg[1])))
}

cv_max_folds <- 10L
if (!is.na(max_folds_override)) {
  cv_max_folds <- max_folds_override
}

merge_results <- any(cli_args == "--merge" | cli_args == "--merge-results")

message("═══════════════════════════════════════════════════════")
message("Manual MF-VAR Cross-Validation Runner")
message("═══════════════════════════════════════════════════════")
message(sprintf("→ Maximum folds: %d", cv_max_folds))
message(sprintf("→ Merge results: %s", merge_results))

# Settings
n_lags <- 2L
forecast_steps <- c(1L, 4L)
horizon_max <- max(forecast_steps)
initial_train_quarter <- zoo::as.yearqtr("2015 Q4")
extra_months_option <- 0L

# Load data
qdat_raw <- read_quarterly_data(file.path(".", "data"))
qdat_adj <- qdat_raw |>
  dplyr::select(qtr, gdp_growth, inflation, exch_rate)

target_vars <- c("gdp_growth", "inflation", "exch_rate")

# Prepare CV plan
cv_plan <- prepare_cv_plan(
  qdat_adj = qdat_adj,
  qdat_orig = qdat_raw,
  forecast_steps = forecast_steps,
  n_lags = n_lags,
  extra_months_options = extra_months_option,
  max_folds = cv_max_folds,
  initial_train_quarter = initial_train_quarter
)

if (!cv_plan$valid) {
  stop("CV plan is not valid: ", cv_plan$reason)
}

message(sprintf("→ Running %d-fold cross-validation", cv_plan$total_folds))

# CV fold function for manual MF-VAR
run_mfvar2_cv_fold <- function(fold_idx, cv_plan, qdat_raw, target_vars, position, total_folds) {
  fold_start <- Sys.time()
  
  train_rows <- fold_idx - 1L
  if (train_rows <= n_lags) {
    message(sprintf("  • CV fold %02d/%02d | skipped (too few quarters)", position, total_folds))
    return(NULL)
  }
  
  q_train <- dplyr::slice_head(qdat_raw, n = train_rows)
  q_eval <- dplyr::slice(qdat_raw, fold_idx:(fold_idx + cv_plan$horizon_max - 1L))
  
  cutoff_quarter <- q_train$qtr[nrow(q_train)]
  cutoff_label <- as.character(cutoff_quarter)
  
  message(sprintf("  • CV fold %02d/%02d | training through %s", position, total_folds, cutoff_label))
  
  # Prepare data with training window
  adapter_result <- tryCatch({
    prepare_mfvar2_input(
      data_override = q_train,
      verbose = FALSE
    )
  }, error = function(e) {
    warning(sprintf("Fold %02d data prep failed: %s", position, e$message))
    return(NULL)
  })
  
  if (is.null(adapter_result)) {
    return(NULL)
  }
  
  # Fixed hyperparameters (no tuning in CV)
  hyperparams <- list(
    lambda1 = 0.06,
    lambda2 = 1.0,
    lambda3 = 1.0,
    lambda4 = 1.0,
    lambda5 = 1.0
  )
  
  # Estimate model
  posterior <- tryCatch({
    mfvar2::estimate_mf_bvar(
      data_prepared = adapter_result$prepared,
      p = 2L,
      hyperparameters = hyperparams,
      n_draws = 2000L,
      burnin = 500L,
      thinning = 1L,
      verbose = FALSE,
      seed = 321L + position
    )
  }, error = function(e) {
    warning(sprintf("Fold %02d estimation failed: %s", position, e$message))
    return(NULL)
  })
  
  if (is.null(posterior)) {
    return(NULL)
  }
  
  # Generate forecasts
  forecast_obj <- tryCatch({
    mfvar2::forecast_mf_bvar(
      posterior = posterior,
      horizon_months = cv_plan$horizon_months,
      n_sim = 500L,
      seed = 321L + position
    )
  }, error = function(e) {
    warning(sprintf("Fold %02d forecast failed: %s", position, e$message))
    return(NULL)
  })
  
  if (is.null(forecast_obj)) {
    return(NULL)
  }
  
  # Extract quarterly forecasts
  quantiles <- forecast_obj$quantiles
  median_idx <- which.min(abs(quantiles - 0.5))
  
  predictions_list <- list()
  for (var in target_vars) {
    qmat <- forecast_obj$forecasts_quarterly[[var]]
    if (is.null(qmat)) next
    
    median_forecasts <- qmat[, median_idx]
    
    # Back-transform exchange rate
    if (var == "exch_rate") {
      median_forecasts <- exp(median_forecasts)
    }
    
    for (step in forecast_steps) {
      if (step <= length(median_forecasts)) {
        predictions_list[[length(predictions_list) + 1]] <- tibble::tibble(
          fold_index = fold_idx,
          cutoff_quarter = cutoff_quarter,
          variable = var,
          step_ahead = step,
          horizon = if (step == 1L) "1-step ahead" else "1-year ahead",
          quarter_end = zoo::as.Date(q_eval$qtr[step], frac = 1),
          predicted = median_forecasts[step],
          actual = q_eval[[var]][step],
          model = "Manual MF-VAR",
          extra_months = 0L
        )
      }
    }
  }
  
  fold_elapsed <- as.numeric(difftime(Sys.time(), fold_start, units = "secs"))
  
  if (length(predictions_list) == 0) {
    return(list(predictions = NULL, timing = fold_elapsed))
  }
  
  predictions_df <- dplyr::bind_rows(predictions_list)
  
  list(
    predictions = predictions_df,
    timing = fold_elapsed
  )
}

# Run CV folds
cv_start <- Sys.time()
all_predictions <- list()
all_timings <- numeric(cv_plan$total_folds)

for (i in seq_along(cv_plan$cv_indices)) {
  fold_result <- run_mfvar2_cv_fold(
    fold_idx = cv_plan$cv_indices[i],
    cv_plan = cv_plan,
    qdat_raw = qdat_raw,
    target_vars = target_vars,
    position = i,
    total_folds = cv_plan$total_folds
  )
  
  if (!is.null(fold_result) && !is.null(fold_result$predictions)) {
    all_predictions[[i]] <- fold_result$predictions
    all_timings[i] <- fold_result$timing
  }
  
  # Progress reporting
  completed <- i
  avg_time <- mean(all_timings[all_timings > 0], na.rm = TRUE)
  remaining_folds <- cv_plan$total_folds - completed
  est_remaining_min <- (remaining_folds * avg_time) / 60
  
  message(sprintf("    Completed in %.1f sec | Avg: %.1f sec/fold | Est. remaining: %.1f min",
                  all_timings[i], avg_time, est_remaining_min))
}

cv_elapsed <- as.numeric(difftime(Sys.time(), cv_start, units = "mins"))

# Combine results
cv_predictions <- dplyr::bind_rows(all_predictions)

if (nrow(cv_predictions) == 0) {
  stop("No CV predictions were generated")
}

# Compute metrics
cv_metrics <- cv_predictions |>
  dplyr::group_by(variable, horizon, model) |>
  dplyr::summarise(
    n_folds = dplyr::n(),
    rmse = sqrt(mean((predicted - actual)^2, na.rm = TRUE)),
    mae = mean(abs(predicted - actual), na.rm = TRUE),
    mape = mean(abs((predicted - actual) / actual) * 100, na.rm = TRUE),
    .groups = "drop"
  )

# Output paths
output_dir <- file.path("output", "forecasts", "mfvar2_cv")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

predictions_file <- file.path(output_dir, "mfvar2_cv_predictions.csv")
metrics_file <- file.path(output_dir, "mfvar2_cv_metrics.csv")
summary_file <- file.path(output_dir, "mfvar2_cv_summary.txt")

readr::write_csv(cv_predictions, predictions_file)
readr::write_csv(cv_metrics, metrics_file)

# Write summary
sink(summary_file)
cat("Manual MF-VAR Cross-Validation Summary\n")
cat(rep("=", 60), "\n", sep = "")
cat(sprintf("Folds completed: %d\n", cv_plan$total_folds))
cat(sprintf("Total time: %.1f minutes\n", cv_elapsed))
cat(sprintf("Average time per fold: %.1f seconds\n", mean(all_timings[all_timings > 0])))
cat("\nPerformance Metrics:\n")
print(cv_metrics)
sink()

message("═══════════════════════════════════════════════════════")
message(sprintf("✓ Manual MF-VAR CV complete in %.1f minutes", cv_elapsed))
message(sprintf("  Predictions: %s", predictions_file))
message(sprintf("  Metrics: %s", metrics_file))

# Merge with existing benchmark results if requested
if (merge_results) {
  benchmark_predictions_file <- file.path("output", "model_benchmark_cv_predictions.csv")
  
  if (file.exists(benchmark_predictions_file)) {
    message("→ Merging with existing benchmark CV results")
    
    benchmark_predictions <- readr::read_csv(benchmark_predictions_file, show_col_types = FALSE)
    
    # Remove any existing "Manual MF-VAR" entries
    benchmark_predictions <- benchmark_predictions |>
      dplyr::filter(model != "Manual MF-VAR")
    
    # Combine with new results
    combined_predictions <- dplyr::bind_rows(benchmark_predictions, cv_predictions)
    
    # Write back
    readr::write_csv(combined_predictions, benchmark_predictions_file)
    
    message(sprintf("  ✓ Merged into: %s", benchmark_predictions_file))
  } else {
    warning("Benchmark predictions file not found, cannot merge")
  }
}

message("═══════════════════════════════════════════════════════")
