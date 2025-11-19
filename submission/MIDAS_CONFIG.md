# Configuring MIDAS Parameters for Benchmarking

## Overview

The benchmark script (`run_benchmarks.R`) runs **all models together** (MF-VAR, MIDAS, MIDAS-Latent, AR(2)). You cannot run only MIDAS models, but you can:

1. **Configure MIDAS parameters** via R options
2. **Set the number of CV folds** to 20
3. **Filter results** to compare MIDAS vs other models

## Quick Start: 20 Folds with Custom MIDAS Parameters

### Option 1: Using R Options (Recommended)

Create a simple R script or run in R console:

```r
# Set MIDAS parameters
options(
  midas.y_lags = 1:2,    # AR(2): use lags 1 and 2 (or just 2 for lag-2 only)
  midas.x_lags = 2L,     # 2 quarterly lags of monthly data
  midas.x_m = 3L         # 3 months per quarter (always 3)
)

# Set 20 folds
options(mfvar.cv_max_folds = 20L)

# Run benchmarks
source("run_benchmarks.R")
```

### Option 2: Command Line with Environment Variables

```bash
# Set options via environment, then run
Rscript -e "options(midas.y_lags=1:2, midas.x_lags=2L, midas.x_m=3L, mfvar.cv_max_folds=20L); source('run_benchmarks.R')"
```

### Option 3: Command Line with --max-folds

```bash
# Use --max-folds for folds, but set MIDAS params in R first
Rscript -e "options(midas.y_lags=1:2); source('run_benchmarks.R')" --max-folds=20
```

## MIDAS Parameters Explained

### `midas.y_lags` (AR Component)
- **Default**: `2L` (only lag 2)
- **Options**:
  - `2L` or `c(2L)` - Only lag 2 (matches `midas.qmd` notebook)
  - `1:2` or `c(1L, 2L)` - AR(2) with lags 1 and 2 (matches `run_midas.R`)
  - `1:3` - AR(3) with lags 1, 2, 3

### `midas.x_lags` (Monthly Data Lags)
- **Default**: `2L` (2 quarterly lags of monthly data)
- **Meaning**: How many quarters of monthly data to include
- **Options**: `1L`, `2L`, `3L`, `4L`, etc.

### `midas.x_m` (Frequency Ratio)
- **Default**: `3L` (always 3 months per quarter)
- **Should not be changed** unless you're using different frequency data

## Examples

### Example 1: Match `midas.qmd` notebook (lag 2 only)
```r
options(midas.y_lags = 2L, mfvar.cv_max_folds = 20L)
source("run_benchmarks.R")
```

### Example 2: Match `run_midas.R` (AR(2) with lags 1:2)
```r
options(midas.y_lags = 1:2, mfvar.cv_max_folds = 20L)
source("run_benchmarks.R")
```

### Example 3: Test different x_lags
```r
options(midas.y_lags = 1:2, midas.x_lags = 3L, mfvar.cv_max_folds = 20L)
source("run_benchmarks.R")
```

## Viewing Results

After running, results are in:
- `output/benchmarks/csv/model_benchmark_cv_metrics.csv` - Error metrics by model
- `output/benchmarks/csv/model_benchmark_cv_predictions.csv` - All predictions
- `output/benchmarks/model_benchmark_summary.md` - Summary table

### Filter to MIDAS models only:

```r
library(readr)
library(dplyr)

# Load CV metrics
cv_metrics <- read_csv("output/benchmarks/csv/model_benchmark_cv_metrics.csv")

# Filter to MIDAS models
midas_results <- cv_metrics %>%
  filter(model %in% c("MIDAS", "MIDAS (trend)", "MIDAS-Latent", "MIDAS-Latent (trend)"))

# Compare with AR(2) benchmark
comparison <- cv_metrics %>%
  filter(model %in% c("MIDAS", "MIDAS (trend)", "AR(2)")) %>%
  arrange(variable, horizon, model)

print(comparison)
```

## Notes

1. **All models run together**: You cannot skip MF-VAR or AR(2) models. They all run in the same CV loop.

2. **MIDAS parameters affect both**:
   - `MIDAS` and `MIDAS (trend)` models (use KOF Barometer)
   - `MIDAS-Latent` and `MIDAS-Latent (trend)` models (use latent states from MF-VAR)

3. **Other model parameters are unchanged**: Only MIDAS parameters are configurable. MF-VAR and AR(2) use their default specifications.

4. **20 folds**: With `mfvar.cv_max_folds = 20L`, the CV will use up to 20 folds (if enough data is available).

