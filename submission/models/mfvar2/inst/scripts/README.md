# MF-VAR Scripts

This directory contains essential scripts for using the `mfvar2` package. These scripts provide a complete, production-ready workflow for mixed-frequency Bayesian VAR forecasting.

## Scripts Overview

### 🚀 Quick Start

**`quick_forecast.R`** - Fast forecasting for rapid prototyping

- Skips hyperparameter tuning (uses sensible defaults)
- Uses fewer MCMC draws (1000 vs 4000)
- Perfect for testing and quick results
- Runtime: ~2-5 minutes

```r
source("inst/scripts/quick_forecast.R")
quick_forecast("path/to/data.csv")
```

Or from command line:

```bash
Rscript inst/scripts/quick_forecast.R data/my_data.csv
```

### 📋 Complete Workflow

**`00_complete_workflow.R`** - Full end-to-end pipeline

- Data preparation
- Hyperparameter tuning (optional)
- Model estimation
- Forecast generation
- Comprehensive visualization
- Summary report generation
- Runtime: ~15-45 minutes (depending on tuning)

```r
# Edit settings at top of file, then:
source("inst/scripts/00_complete_workflow.R")
```

### 🔧 Modular Components

Use these for custom workflows:

**`01_prepare_data.R`** - Versatile data preparation

- Handles CSV, Excel, RDS, data frames
- Auto-detects quarterly vs monthly variables
- Auto-detects flow vs stock variables
- Unit root testing and differencing
- Data validation

```r
source("inst/scripts/01_prepare_data.R")
data_prep <- prepare_my_data(
  input = "data.csv",
  quarterly_vars = c("GDP"),
  flow_vars = c("GDP")
)
```

**`02_estimate_and_forecast.R`** - Model estimation & forecasting

- Optional hyperparameter tuning via grid search
- Gibbs sampler estimation
- Out-of-sample forecasting
- Saves all results

```r
source("inst/scripts/02_estimate_and_forecast.R")
results <- run_mfvar_forecasting(
  data_prep = data_prep,
  forecast_horizon = 12
)
```

**`03_visualize_results.R`** - Comprehensive visualization

- Forecast fan charts
- Historical data with forecast overlay
- Convergence diagnostics
- Forecast comparison plots
- Saves as PNG and/or PDF

```r
source("inst/scripts/03_visualize_results.R")
plots <- visualize_all_results(
  results = results,
  data_prep = data_prep,
  output_dir = "plots"
)
```

## Usage Examples

### Example 1: Quick Test on Your Data

```r
library(mfvar2)
source("inst/scripts/quick_forecast.R")

# Works with any of these input formats:
quick_forecast("my_data.csv")                          # Single CSV
quick_forecast(c("monthly.csv", "quarterly.csv"))      # Multiple CSVs
quick_forecast(my_dataframe)                            # Data frame
quick_forecast("my_data.xlsx")                          # Excel file
```

### Example 2: Full Pipeline with Custom Settings

```r
library(mfvar2)

# Load scripts
source("inst/scripts/01_prepare_data.R")
source("inst/scripts/02_estimate_and_forecast.R")
source("inst/scripts/03_visualize_results.R")

# Step 1: Prepare data
data_prep <- prepare_my_data(
  input = "data.csv",
  quarterly_vars = c("GDP", "Consumption"),
  flow_vars = c("GDP", "Consumption"),
  start_date = "2000-01-01",
  output_path = "prepared_data.rds"
)

# Step 2: Estimate with custom hyperparameters
my_hyper <- list(
  lambda1_optimal = 0.15,  # Tighter prior
  lambda2_optimal = 1,
  lambda3_optimal = 1,
  lambda4_optimal = 1,
  lambda5_optimal = 1
)

results <- run_mfvar_forecasting(
  data_prep = data_prep,
  p = 3,  # 3 lags instead of 2
  tune_hyperparameters = FALSE,
  hyperparameters = my_hyper,
  n_draws = 5000,
  burnin = 1500,
  forecast_horizon = 18,
  output_dir = "results"
)

# Step 3: Visualize
plots <- visualize_all_results(
  results = results,
  data_prep = data_prep,
  variables = c("GDP", "Consumption"),  # Only these vars
  output_dir = "plots",
  format = "both"  # PNG and PDF
)
```

### Example 3: Production Workflow

```r
# Use the complete workflow script with your settings
# Edit the settings section in 00_complete_workflow.R:

INPUT_DATA <- "my_data.csv"
QUARTERLY_VARS <- c("GDP", "Consumption", "Investment")
FLOW_VARS <- c("GDP", "Consumption", "Investment")
LAG_ORDER <- 2
TUNE_HYPERPARAMETERS <- TRUE
N_DRAWS <- 4000
BURNIN <- 1000
FORECAST_HORIZON <- 12
OUTPUT_DIR <- "production_results"

# Then run:
source("inst/scripts/00_complete_workflow.R")
```

## Input Data Requirements

### Data Format

Your data should have:

1. **Date column** (first column, any parseable date format)
2. **Variable columns** (one per variable)
3. **Monthly frequency** (with NAs for quarterly-only variables at non-quarter-end months)

### Example CSV Format

```csv
date,CPI,IP,UnempRate,GDP,Consumption,Investment
2020-01-01,100.5,95.2,3.5,NA,NA,NA
2020-02-01,100.8,95.8,3.6,NA,NA,NA
2020-03-01,101.1,96.1,3.7,1000.5,600.2,200.1
2020-04-01,101.3,94.5,4.2,NA,NA,NA
...
```

### Supported Input Formats

- **CSV files**: `.csv`
- **Excel files**: `.xlsx`, `.xls` (requires `readxl` package)
- **RDS files**: `.rds`
- **Data frames**: In-memory R data frames
- **Multiple files**: List or vector of file paths

### Auto-Detection Features

The scripts automatically detect:

- ✅ **Quarterly vs monthly variables** (by NA pattern)
- ✅ **Flow vs stock variables** (you can override)
- ✅ **Variables needing log transformation** (based on scale/variance)
- ✅ **Variables needing differencing** (via ADF unit root tests)

## Output Structure

When you run the complete workflow, you'll get:

```
OUTPUT_DIR/
├── data_prepared.rds              # Prepared data object
├── estimation/
│   ├── mfvar_results.rds         # Complete results (posterior, forecasts, etc.)
│   ├── forecasts.rds              # Forecast quantiles (easy access)
│   ├── hyperparameters.rds        # Selected hyperparameters
│   └── summary.csv                # Summary statistics
├── plots/
│   ├── forecast_GDP.png           # Fan chart for GDP
│   ├── forecast_CPI.png           # Fan chart for CPI
│   ├── history_forecast_GDP.png   # Historical + forecast for GDP
│   ├── diagnostics_geweke.png     # Convergence diagnostic
│   ├── diagnostics_ess.png        # Effective sample size
│   └── forecast_comparison.png    # Compare all variables
└── SUMMARY_REPORT.txt             # Text summary report
```

## Performance Tips

### For Speed

- Use `quick_forecast.R` for testing (2-5 min vs 15-45 min)
- Set `tune_hyperparameters = FALSE` and use defaults
- Reduce `n_draws` (e.g., 1000 instead of 4000)
- Use smaller hyperparameter grids

### For Accuracy

- Use full hyperparameter tuning
- Increase `n_draws` (e.g., 10000)
- Increase `burnin` (e.g., 2000)
- Check convergence diagnostics carefully

### For Memory

- Use `thinning > 1` to keep fewer draws
- Don't store state draws if not needed
- Process variables in batches

## Troubleshooting

### "Non-positive definite covariance"

**Solution**: Check for perfect collinearity between variables

```r
cor(data_prep$data, use = "pairwise.complete.obs")
```

### "Hyperparameter tuning takes too long"

**Solutions**:

- Use smaller grids in `02_estimate_and_forecast.R`
- Set `tune_hyperparameters = FALSE` and use defaults
- Use `quick_forecast.R` instead

### "Geweke diagnostic failed"

**Solutions**:

- Increase `burnin` (e.g., 2000 instead of 1000)
- Increase `n_draws` (e.g., 10000 instead of 4000)
- Check for identification issues in your model

### "File not found" errors

**Solution**: Use absolute paths or ensure working directory is correct

```r
setwd("/path/to/project")
# Or use absolute paths:
prepare_my_data("/full/path/to/data.csv")
```

## Advanced Usage

### Custom Hyperparameter Grid

Edit `02_estimate_and_forecast.R` or specify in function call:

```r
# Tune all 5 hyperparameters
results <- run_mfvar_forecasting(
  data_prep = data_prep,
  lambda1_grid = c(0.1, 0.2),        # Overall tightness
  lambda2_grid = c(1, 3),            # Lag decay
  lambda3_grid = c(0.5, 1, 2),       # Error variance prior (now tunable!)
  lambda4_grid = c(1, 3),            # Sum-of-coefficients
  lambda5_grid = c(1, 3),            # Co-persistence
  n_gibbs_mdd = 1000                 # Fewer draws for MDD
)

# Or fix some parameters (e.g., only tune λ₁ and λ₂)
results <- run_mfvar_forecasting(
  data_prep = data_prep,
  lambda1_grid = c(0.05, 0.1, 0.15, 0.2),  # Tune this
  lambda2_grid = c(1, 2, 3),                # Tune this
  lambda3_grid = c(1),                       # Fix at 1
  lambda4_grid = c(1),                       # Fix at 1
  lambda5_grid = c(1)                        # Fix at 1
)
```

### Parallel Execution

For multiple datasets or settings:

```r
library(parallel)

datasets <- list("data1.csv", "data2.csv", "data3.csv")

results_list <- mclapply(datasets, function(d) {
  source("inst/scripts/quick_forecast.R")
  quick_forecast(d, output_dir = paste0("results_", basename(d)))
}, mc.cores = 3)
```

### Custom Variable Selection

```r
# Prepare with only specific variables
data_prep <- prepare_my_data(
  input = "big_data.csv",
  quarterly_vars = c("GDP", "Consumption"),
  flow_vars = c("GDP", "Consumption")
)

# Then manually subset if needed
data_prep$data <- data_prep$data[, c("GDP", "CPI", "IP")]
```

## Getting Help

- **Package documentation**: `?prepare_data_snb`, `?estimate_mf_bvar`, etc.
- **Main README**: See package root `README.md`
- **Issues**: Report bugs on GitHub
- **Questions**: See package vignettes with `browseVignettes("mfvar2")`

## Citation

If you use these scripts in research, please cite:

```bibtex
@Manual{mfvar2,
  title = {mfvar2: Mixed Frequency Bayesian VAR},
  author = {Your Name},
  year = {2025},
  note = {R package version 0.1.0}
}
```

And the original methodology:

```bibtex
@article{schorfheide2015,
  title={Real-time forecasting with a mixed-frequency VAR},
  author={Schorfheide, Frank and Song, Dongho},
  journal={Journal of Business \& Economic Statistics},
  volume={33},
  number={3},
  pages={366--380},
  year={2015}
}
```

---

**Last Updated:** November 2025  
**Package Version:** 0.1.0  
**Status:** Production Ready
