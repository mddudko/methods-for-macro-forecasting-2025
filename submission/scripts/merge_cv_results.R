#!/usr/bin/env Rscript

# Merge CV results: New MIDAS results + Manual MF-VAR from backup
# ---------------------------------------------------------------
# This script combines:
# 1. Fresh CV results (MF-VAR package, MIDAS, MIDAS-Latent, AR)
# 2. Manual MF-VAR results from backup file
# ---------------------------------------------------------------

library(readr)
library(dplyr)

message("Merging CV results...")

# Paths
backup_pred <- "output/benchmarks/csv/model_benchmark_cv_predictions_backup.csv"
backup_metrics <- "output/benchmarks/csv/model_benchmark_cv_metrics_backup.csv"
new_pred <- "output/benchmarks/csv/model_benchmark_cv_predictions.csv"
new_metrics <- "output/benchmarks/csv/model_benchmark_cv_metrics.csv"
merged_pred <- "output/benchmarks/csv/model_benchmark_cv_predictions_merged.csv"
merged_metrics <- "output/benchmarks/csv/model_benchmark_cv_metrics_merged.csv"

# Check files exist
if (!file.exists(backup_pred) || !file.exists(new_pred)) {
  stop("Required CV files not found. Make sure backup and new CV runs are complete.")
}

# Load data
message("  • Loading backup data (manual MF-VAR)...")
backup_predictions <- read_csv(backup_pred, show_col_types = FALSE)
backup_metrics_df <- read_csv(backup_metrics, show_col_types = FALSE)

message("  • Loading new data (updated MIDAS)...")
new_predictions <- read_csv(new_pred, show_col_types = FALSE)
new_metrics_df <- read_csv(new_metrics, show_col_types = FALSE)

# Extract only manual MF-VAR from backup
message("  • Extracting manual MF-VAR from backup...")
manual_predictions <- backup_predictions |>
  filter(model == "MF-VAR (manual)")

manual_metrics <- backup_metrics_df |>
  filter(model == "MF-VAR (manual)")

if (!nrow(manual_predictions)) {
  warning("No manual MF-VAR results found in backup. Skipping merge.")
  quit(save = "no", status = 1)
}

message(sprintf("    Found %d manual MF-VAR predictions", nrow(manual_predictions)))

# Remove any existing manual results from new data (shouldn't exist but be safe)
new_predictions_clean <- new_predictions |>
  filter(model != "MF-VAR (manual)")

new_metrics_clean <- new_metrics_df |>
  filter(model != "MF-VAR (manual)")

# Combine
message("  • Merging datasets...")
merged_predictions <- bind_rows(new_predictions_clean, manual_predictions) |>
  arrange(fold, monthly_coverage, variable, model, step_ahead)

merged_metrics_df <- bind_rows(new_metrics_clean, manual_metrics) |>
  arrange(monthly_coverage, variable, horizon, model)

# Save merged results
message("  • Writing merged files...")
write_csv(merged_predictions, merged_pred)
write_csv(merged_metrics_df, merged_metrics)

# Also update the main files
message("  • Updating main CV files...")
write_csv(merged_predictions, new_pred)
write_csv(merged_metrics_df, new_metrics)

# Summary
models_in_merged <- unique(merged_predictions$model)
message("\n✓ Merge complete!")
message(sprintf("  Models in merged dataset: %s", paste(models_in_merged, collapse = ", ")))
message(sprintf("  Total predictions: %d rows", nrow(merged_predictions)))
message(sprintf("  Total metric entries: %d rows", nrow(merged_metrics_df)))
message("\nOutput files:")
message("  - output/benchmarks/csv/model_benchmark_cv_predictions.csv (updated)")
message("  - output/benchmarks/csv/model_benchmark_cv_metrics.csv (updated)")
message("  - output/benchmarks/csv/model_benchmark_cv_predictions_merged.csv (copy)")
message("  - output/benchmarks/csv/model_benchmark_cv_metrics_merged.csv (copy)")
