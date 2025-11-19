# Swiss Macroeconomic Forecasting: MF-VAR vs MIDAS

**Course Project**: Methods for Macroeconomic Forecasting  
**Institution**: ETH Zurich 
**Authors**: See `AUTHORS.yml`    
**Date**: November 2025

## Overview

This project implements and compares two approaches for forecasting Swiss macroeconomic indicators using mixed-frequency data:

1. **Mixed-Frequency Bayesian VAR (MF-VAR)** - Two implementations:
   - **MF-VAR (package)**: Uses `mfbvar` package with SNB monthly indicators (CPI, FX turnover, unemployment, policy rate, SMI returns)
   - **MF-VAR (manual)**: Custom implementation using `mfvar2` package with alternative specification
2. **MIDAS Regression** - Uses exponential Almon polynomial distributed lag structures with:
   - **MIDAS-KOF**: KOF Economic Barometer as monthly indicator
   - **MIDAS-Latent**: MF-VAR latent states as monthly indicators (extension)

The analysis includes evaluation through expanding window cross-validation (20 folds, 1992-2025), with comprehensive benchmark comparisons against AR(2).

## Authors

See [`AUTHORS.yml`](AUTHORS.yml) for contributor information.

## Project Structure

```
.
├── R/                          # Helper modules
│   ├── setup.R                 # Environment setup and package management
│   ├── data_processing.R       # Data ingestion and transformation
│   ├── evaluation.R            # (Legacy) Model evaluation framework
│   ├── benchmark_cv.R          # Cross-validation evaluation logic
│   ├── benchmark_shared.R      # Shared forecasting functions for all models
│   ├── plotting.R              # Visualization functions
│   └── latent_states.R         # Latent state extraction and plotting
│
├── data/                       # Data files
│   ├── processed/
│   │   ├── data_quarterly.csv      # Main quarterly dataset (GDP, CPI, exchange rate)
│   │   ├── combined_timeseries.csv # SNB monthly indicators
│   │   └── metadata_quarterly*.csv # Data provenance and documentation
│   └── raw_data/               # Raw source data from SNB
│
├── output/                     # Generated outputs (tracked in git for reproducibility)
│   ├── forecasts/             # Current forecasts from latest data
│   │   ├── mfvar/             # MF-VAR (package) results
│   │   │   ├── csv/           # Forecasts and latent states
│   │   │   ├── plots/         # Forecast and latent state visualizations
│   │   │   └── models/        # Saved model objects (.rds)
│   │   ├── mfvar2/            # MF-VAR (manual) results
│   │   ├── midas/             # MIDAS results (forecasts, plots)
│   │   └── combined/          # Multi-model comparison plots (.png + .pdf)
│   ├── benchmarks/            # Model comparison & evaluation
│   │   ├── csv/               # CV metrics, predictions, timings
│   │   ├── plots/             # Comparison plots (.png + .pdf for CV errors)
│   │   ├── test_runs/         # Diagnostic runs
│   │   └── model_benchmark_summary.{md,html} # Formatted results
│   └── diagnostics/           # Sensitivity analysis and diagnostics
│
├── docs/                       # Documentation and analysis
│   ├── notebooks/
│   │   ├── mfvar_walkthrough.* # Tutorial notebook
│   │   └── midas/              # MIDAS model exploration
│   ├── presentation_V2.*       # Presentation slides (current)
│   └── old/                    # Archived presentations
│
├── models/                     # Custom package implementations
│   └── mfvar2/                 # Manual MF-VAR implementation
│
├── diagnostics/                # Experimental diagnostics and tests
│
├── renv/                       # R package management
│
├── main.R                     # Entry point - unified workflow interface
├── scripts/                   # Standalone workflow scripts (CLI-friendly)
│   ├── run_mfvar_package.R    # MF-VAR (package) workflow script
│   ├── run_mfvar_manual.R     # MF-VAR (manual) workflow script
│   ├── run_mfvar_manual_cv.R  # MF-VAR (manual) CV workflow
│   ├── run_midas.R            # MIDAS workflow script
│   ├── run_combined_plots.R   # Multi-model comparison visualization
│   ├── run_benchmarks.R       # Benchmark comparison across all models
│   ├── run_cv_20folds.R       # Helper to force 20-fold CV
│   └── merge_cv_results.R     # Utility to combine CV run results
└── README.md                  # This file
```

## Quick Start

### Prerequisites

- R >= 4.5.0
- RStudio (recommended)
- Required packages managed via `renv`

### Installation

```r
# Restore package environment
renv::restore()
```

### Running the Analysis

All workflows can be run through the unified `main.R` interface:

#### Option 1: MF-VAR Pipeline (Default)

```bash
Rscript main.R mfvar
# or simply: Rscript main.R
```

This runs the complete MF-VAR workflow:
1. Loads quarterly data and monthly indicators
2. Estimates full-sample model
3. Generates forecasts and plots
4. Extracts latent states (extension)

**Outputs:**
- `output/forecasts/mfvar/csv/mfvar_forecasts_full.csv` - All forecast horizons
- `output/forecasts/mfvar/csv/mfvar_forecasts_targets.csv` - 1-step and 1-year ahead only
- `output/forecasts/mfvar/csv/mfvar_latent_states.csv` - Extracted latent states (extension)
- `output/forecasts/mfvar/mfvar_summary.txt` - Model diagnostics and evaluation
- `output/forecasts/mfvar/plots/forecast_*_context.png` - Forecast visualizations with history
- `output/forecasts/mfvar/plots/mfvar_latent_states_all.png` - All latent states (landscape, 14×6)
- `output/forecasts/mfvar/plots/mfvar_latent_states_{timeseries,heatmap}.png` - Individual visualizations

#### Option 2: MIDAS Pipeline

```bash
Rscript main.R midas
```

Runs MIDAS regression models:
1. **MIDAS-KOF**: Uses KOF Economic Barometer as monthly indicator
2. Estimates models with and without trend
3. Generates forecasts for target variables
4. Computes evaluation metrics

**Outputs:**
- `output/forecasts/midas/csv/midas_forecasts_full.csv` - All forecast horizons
- `output/forecasts/midas/csv/midas_forecasts_targets.csv` - 1-step and 1-year ahead only
- `output/forecasts/midas/midas_summary.txt` - Model diagnostics
- `output/forecasts/midas/plots/midas_forecast_*.png` - Forecast visualizations

#### Option 3: Benchmark Comparison

```bash
Rscript main.R benchmarks
```

Compares all model variants:
- **MF-VAR**: Mixed-frequency Bayesian VAR with SNB indicators
- **MIDAS-KOF (trend/simple)**: MIDAS with KOF Barometer
- **MIDAS-Latent (trend/simple)**: MIDAS with MF-VAR latent states (extension)
- **AR(2)**: Autoregressive benchmark
- **RW-trend**: Random walk with drift

Evaluation framework:
- Expanding window cross-validation (default: 10 folds, configurable via `--max-folds`)
- CV coverage: 1993 Q4 to 2025 Q2 (training starts from 6 quarters in 1992 Q2-1993 Q3)
- Per-fold timing breakdowns for performance profiling

**Outputs:**
- `output/benchmarks/csv/model_benchmark_cv_metrics.csv` - CV metrics by model/horizon/extra_months
- `output/benchmarks/csv/model_benchmark_cv_predictions.csv` - Full CV predictions by fold
- `output/benchmarks/csv/model_benchmark_cv_timings.csv` - Per-fold runtime breakdowns
- `output/benchmarks/model_benchmark_summary.{md,html}` - Formatted summary tables
- `output/benchmarks/plots/model_benchmark_plot_*.png` - Forecast comparison plots
- `output/benchmarks/plots/cv_errors_by_variable_{rmse,mae}.png` - CV error by variable
- `output/benchmarks/plots/cv_errors_heatmap_h4_{rmse,mae}.png` - 1-year ahead heatmaps
- `output/benchmarks/plots/cv_relative_errors_{rmse,mae}.{png,pdf}` - Relative error plots with PDF export

#### Option 4: Combined Forecast Plots

```bash
Rscript scripts/run_combined_plots.R
```

Generates multi-model comparison visualizations:
- Overlays all 6 forecast series (MF-VAR, MIDAS-KOF trend/simple, MIDAS-Latent trend/simple, AR(2))
- Shows 1-year (4 quarters) of historical context
- Requires prior runs of `scripts/run_mfvar_package.R` and `scripts/run_midas.R`

**Outputs:**
- `output/forecasts/combined/combined_*_{png,pdf}` - Multi-model comparison plots (PNG + PDF)

## Target Variables

- **gdp_growth**: Real GDP annualized quarterly growth rate
- **inflation**: CPI annualized quarterly inflation rate
- **exch_rate**: CHF/EUR exchange rate (log-transformed)

## Forecast Horizons

- **1-step ahead**: Next quarter (nowcast/1-quarter ahead)
- **1-year ahead**: 4 quarters ahead (annual forecast)

## Model Specifications

### MF-VAR (package via `mfbvar`)
- **Lags**: 5 quarters
- **Prior**: Minnesota with inverse-Wishart covariance
- **Lambda1**: 0.06 (optimal from sensitivity analysis: avg RMSE=0.748 vs 0.801 for 0.08, 0.937 for 0.1)
- **Aggregation**: Average (8-13% RMSE improvement over 'first' aggregation across all models)
- **MCMC**: 10000 draws, 5000 burn-in, no thinning (package defaults)
- **Monthly indicators**: CPI, FX turnover, unemployment, SNB rate, SMI returns

### MF-VAR (manual via `mfvar2`)
- **Lags**: 2 quarters
- **MCMC**: 1200 draws, 400 burn-in
- **Hyperparameters**: Lambda1=0.2, Lambda2-5=1.0
- **Monthly indicators**: Same SNB series as package implementation
- Custom Gibbs sampler with state-space representation

### Benchmarks
- **AR(2)**: Autoregressive model (Yule-Walker/OLS/ARIMA fallbacks)
- **MIDAS-KOF (trend/simple)**: Mixed-data sampling with KOF Economic Barometer and exponential Almon lag polynomials (with/without deterministic trend)
- **MIDAS-Latent (trend/simple)** (Extension): MIDAS using MF-VAR latent states as monthly indicators (with/without trend)

## Data Sources

Quarterly data sourced from Swiss National Bank (SNB) (`data/processed/data_quarterly.csv`):
- Real GDP growth (`rvgdp`)
- Consumer Price Index (`cpi`)
- Exchange rates CHF/EUR (`wkfreuro`)

Monthly indicators:
- **MF-VAR**: SNB series from `data/processed/combined_timeseries.csv`:
  - `plkopr`: Consumer Price Index (monthly, 2020=100)
  - `devkum`: Foreign exchange turnover (CHF/EUR)
  - `amarbma`: Registered unemployment
  - `snboffzisa`: SNB policy rate (3-month Libor target)
  - `smi_monthly_return`: Swiss Market Index monthly log returns
- **MIDAS-KOF**: KOF Economic Barometer (via `kofdata` package API)
- **MIDAS-Latent** (Extension): MF-VAR latent states extracted from state-space model

See `data/raw_data/data_sources.md` for detailed provenance and `data/processed/metadata_quarterly*.csv` for variable documentation.

## Key Configuration

Edit `R/setup.R` to modify:
- Target variables
- Required packages
- Global settings

Edit main scripts to adjust:
- Number of lags (`n_lags`)
- Forecast horizon (`n_fcst`)
- Prior hyperparameters
- Cross-validation settings

## Helper Modules

### `R/setup.R`
- Activates `renv` environment
- Loads required packages
- Defines target variables

### `R/data_processing.R`
- `read_quarterly_data()` - Load and validate quarterly CSV
- `fetch_kof_barometer()` - Retrieve KOF Barometer series
- `trim_to_overlap()` - Align quarterly and monthly frequencies
- `stationarise_quarterly()` - Apply transformations (growth rates, log)
- `build_Y()` - Construct input for `mfbvar` package

### `R/evaluation.R`
- `run_cross_validation()` - Expanding window one-step-ahead CV
- `predict_ar2()` - AR(2) benchmark with fallbacks
- `compute_time_index()` - Map forecast steps to time indices
- `restore_series_values()` - Reverse transformations

### `R/plotting.R`
- `plot_gdp_forecasts()` - GDP forecast visualization
- `plot_inflation_forecasts()` - Inflation forecast visualization
- `plot_exch_rate_forecasts()` - Exchange rate visualization
- `*_with_history()` variants - Include historical context

## Workflow Details

### Data Preparation
1. Load `data_quarterly.csv` (requires `date`, `rvgdp`, `cpi`, `wkfreuro` columns)
2. Load monthly indicators:
   - **MF-VAR**: SNB series from combined timeseries file (CPI, FX turnover, unemployment, policy rate, SMI returns)
   - **MIDAS-KOF**: Fetch KOF Economic Barometer via API
   - **MIDAS-Latent**: Extract latent states from fitted MF-VAR model (requires MF-VAR to be estimated first)
3. Trim series to overlapping time range
4. Transform quarterly variables to stationarity
5. Window monthly data (includes 2-month lookback for ragged-edge aggregation)

### Model Estimation
1. Set up Minnesota prior with IW covariance
2. Run MCMC sampler (10000 draws, 5000 burn-in - package defaults)
3. Generate 12-quarter-ahead forecasts
4. Aggregate monthly predictions to quarterly frequency
5. Restore original scale (reverse transformations)

### Evaluation
- **Cross-validation**: Expanding window with multiple horizons
  - Default: 10 folds (configurable via `--max-folds`)
  - Full run: 20 folds (via `scripts/run_cv_20folds.R`)
  - Coverage: 1992 Q1 to 2025 Q2 (training starts from 1992 Q2-1993 Q3)
  - Test folds: 2019 Q3 to 2024 Q2
- **Extra months parameter**: Tests ragged-edge nowcasting (0, 1, 2 months beyond training quarter)
- **Metrics**: RMSE, MAE by horizon, variable, and extra_months configuration

### Output
- Forecast tables (CSV)
- Model summary and diagnostics (TXT)
- Comparison plots (PNG)
- Serialized model object (RDS)

## Dependencies

Core packages:
- `mfbvar` - Mixed-frequency Bayesian VAR ([Ankargren et al.](https://github.com/ankargren/mfbvar))
- `kofdata` - KOF data API client
- `tidyverse` - Data manipulation and visualization
- `zoo` - Time series utilities
- `midasr` - MIDAS regression
- `forecast` - Time series forecasting

See `renv.lock` for complete dependency list with versions.

## Reproducibility

This project uses `renv` for package management to ensure reproducibility across different systems. All dependencies are locked to specific versions.

## Results

Pre-computed results are included in `output/forecasts/` and `output/benchmarks/` directories, allowing inspection without re-running the models (which can take 20-30 minutes per workflow with package default hyperparameters).

## Troubleshooting

### KOF Barometer fetch fails (MIDAS-KOF only)
- Check internet connection
- The `kofdata` package requires API access to KOF Swiss Economic Institute
- MF-VAR and MIDAS-Latent models use local data and are not affected

### Insufficient data for evaluation
- Reduce `n_lags` in workflow scripts
- Shorten evaluation horizon
- Extend historical data sample

### MCMC convergence issues
- Increase `n_reps` and `n_burnin`
- Check for data quality issues
- Review prior hyperparameters

### Package installation problems
```r
renv::restore()        # Restore from lock file
renv::rebuild()        # Rebuild if needed
renv::status()         # Check consistency
```

## References

### Methodology
- Schorfheide, F., & Song, D. (2015). "Real-time forecasting with a mixed-frequency VAR." *Journal of Business & Economic Statistics*, 33(3), 403-418.
- Ghysels, E., Sinko, A., & Valkanov, R. (2007). "MIDAS regressions: Further results and new directions." *Econometric Reviews*, 26(1), 53-90.

### Data Sources
- KOF Swiss Economic Institute: https://kof.ethz.ch/
- Swiss National Bank (SNB) economic data

### Software
- `mfbvar` package: https://github.com/ankargren/mfbvar
- `midasr` package: https://CRAN.R-project.org/package=midasr

## License

This project is submitted as coursework for educational purposes. See `AUTHORS.yml` for contributor information.
