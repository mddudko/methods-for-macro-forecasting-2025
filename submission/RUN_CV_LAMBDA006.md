# Running Full CV for Manual MF-VAR with lambda1=0.06

This document provides instructions for running the full cross-validation of the manual MF-VAR implementation with lambda1=0.06.

## Quick Start

### Prerequisites

- R >= 4.3.0
- All dependencies installed via `renv::restore()`

### Run the CV

From the `submission/` directory:

```bash
Rscript scripts/run_cv_manual_lambda006.R
```

This will run the full CV (not fast mode) with:
- **lambda1 = 0.06** (updated from default 0.04)
- **n_draws = 2000** (full mode)
- **burnin = 700** (full mode)
- **n_sim = 600** (full mode)
- **10 folds** (default, covers 2015 Q4 onwards)
- **3 coverage options** (cutoff only, +1 month, +2 months)

### Custom Fold Count

To run with a different number of folds:

```bash
Rscript scripts/run_cv_manual_lambda006.R --max-folds=20
```

## Output Files

The script generates the following files in `output/benchmarks/csv/`:

1. **`mfvar_manual_lambda006_cv_predictions.csv`**
   - All CV fold predictions
   - Columns: monthly_coverage, extra_months, fold, cutoff_quarter, forecast_quarter, variable, step_ahead, prediction, model, quarter_end, horizon, actual, error

2. **`mfvar_manual_lambda006_cv_metrics.csv`**
   - Aggregated metrics by variable, model, and horizon
   - Columns: monthly_coverage, extra_months, variable, model, horizon, rmse, mae, observations

3. **`mfvar_manual_lambda006_cv_timings.csv`**
   - Timing information per fold
   - Columns: fold_index, mfvar_manual_seconds, total_seconds

4. **`output/benchmarks/mfvar_manual_lambda006_summary.md`**
   - Human-readable summary with configuration and results

## Configuration Details

The script modifies the following from the default benchmark configuration:

| Parameter | Default | This Script |
|-----------|---------|-------------|
| lambda1 | 0.04 | **0.06** |
| n_draws | 800 (fast) / 2000 (full) | **2000** |
| burnin | 250 (fast) / 700 (full) | **700** |
| n_sim | 250 (fast) / 600 (full) | **600** |
| Models run | All models | **Manual MF-VAR only** |

## Combining with Other Models

These outputs use the same structure as the main benchmark CV results, so they can be combined later:

```r
# Load both datasets
manual_cv <- readr::read_csv("output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv")
other_models_cv <- readr::read_csv("output/benchmarks/csv/model_benchmark_cv_predictions.csv")

# Filter out old manual results if they exist
other_models_cv <- other_models_cv %>%
  dplyr::filter(model != "MF-VAR (manual)")

# Combine
combined_cv <- dplyr::bind_rows(other_models_cv, manual_cv)

# Save combined results
readr::write_csv(combined_cv, "output/benchmarks/csv/model_benchmark_cv_predictions_combined.csv")
```

## Expected Runtime

With 10 folds and 3 coverage options (30 total runs):
- **Estimated time:** 30-60 minutes depending on hardware
- Each fold processes all 3 target variables (gdp_growth, inflation, exch_rate)
- Each fold forecasts 1-step and 4-step ahead

With 20 folds:
- **Estimated time:** 60-120 minutes

## Troubleshooting

### "mfvar2 package not available"

The mfvar2 package should be loaded automatically from `models/mfvar2/`. If you see this error:

```bash
# Make sure you're in the submission/ directory
cd submission/
Rscript scripts/run_cv_manual_lambda006.R
```

### Network/SSL Issues with renv

If you encounter SSL certificate errors during package installation:

```r
# In R console:
options(download.file.method = "libcurl")
renv::restore()
```

### Out of Memory

If the CV runs out of memory, reduce the number of folds:

```bash
Rscript scripts/run_cv_manual_lambda006.R --max-folds=5
```

## Alternative: Docker

If you prefer to use Docker:

```bash
# From project root
docker-compose build
docker-compose run --rm momf Rscript scripts/run_cv_manual_lambda006.R
```

## Verification

After completion, verify the outputs:

```bash
# Check predictions file exists and has data
wc -l output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv

# Should show ~900-1800 lines (depends on folds)
# Formula: (10 folds * 3 coverage options * 3 variables * 2 horizons) + 1 header
#        = (10 * 3 * 3 * 2) + 1 = 181 lines minimum

# Check metrics
cat output/benchmarks/csv/mfvar_manual_lambda006_cv_metrics.csv

# Check summary
cat output/benchmarks/mfvar_manual_lambda006_summary.md
```

## Questions?

If you encounter issues:
1. Check that R and all dependencies are properly installed
2. Ensure you're running from the `submission/` directory
3. Verify the `models/mfvar2/` package exists
4. Check `output/benchmarks/` directory has write permissions
