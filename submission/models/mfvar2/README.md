# mfvar2: Mixed Frequency Bayesian VAR

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R Package](https://img.shields.io/badge/R%20Package-0.1.0-blue.svg)](https://www.r-project.org/)

> **Bayesian Vector Autoregression for Mixed Frequency Time Series**  
> Implements the methodology of Schorfheide & Song (2015) with Minnesota prior and state-space representation

## Overview

`mfvar2` provides a complete toolkit for mixed frequency Bayesian VAR modeling, designed for macroeconomic forecasting with variables observed at different frequencies (e.g., monthly industrial production, quarterly GDP).

### What Makes This Package Unique?

- **True Mixed Frequency**: Handles monthly and quarterly data jointly without pre-aggregation
- **Flow Variable Support**: Correctly aggregates flow-type quarterly variables (GDP, consumption) using Mariano-Murasawa (2003) approach
- **Empirical Bayes Tuning**: Automatic hyperparameter selection via marginal data density maximization
- **Real-Time Forecasting**: Built-in support for data vintages and publication lags
- **Production Ready**: Numerically stable (KFAS backend), well-documented, extensively tested

## Methodology

This package implements the mixed frequency Bayesian VAR approach from:

> **Schorfheide, F., & Song, D. (2015).** _Real-time forecasting with a mixed-frequency VAR._  
> Journal of Business & Economic Statistics, 33(3), 366-380.

**Key methodological components:**

1. **Minnesota Prior** (Litterman, 1986): Bayesian shrinkage with 5 hyperparameters (λ₁-λ₅)

   - λ₁: Overall tightness
   - λ₂: Lag decay
   - λ₃: Error variance prior
   - λ₄: Sum-of-coefficients (long-run prior)
   - λ₅: Co-persistence (joint stationarity)

2. **State-Space Representation**: Companion form with time-varying observation matrices

   - Monthly variables: Observed every period
   - Stock quarterly: Observed at quarter-end only
   - Flow quarterly: Aggregated as (1/3)(y*t + y*{t-1} + y\_{t-2})

3. **Gibbs Sampler**: Three-step algorithm

   - Step 1: Draw latent states (Carter-Kohn simulation smoother)
   - Step 2: Draw VAR coefficients (conjugate Normal posterior)
   - Step 3: Draw error covariances (Inverse-Wishart posterior)

4. **Hyperparameter Selection**: Grid search over 5D parameter space maximizing marginal data density (Geweke, 1999)

## Installation

### From GitHub (Recommended)

```r
# Install devtools if needed
if (!require("devtools")) install.packages("devtools")

# Install mfvar2
devtools::install_github("mddudko/methods-for-macro-forecasting-2025", subdir = "submission/mfvar2")
```

### From Local Source

```r
# If you have the package locally
devtools::install("path/to/mfvar2")

# Or for development
devtools::load_all("path/to/mfvar2")
```

### Dependencies

The package requires:

- **Core**: `zoo`, `xts`, `KFAS`, `mvtnorm`, `Matrix`
- **Plotting**: `ggplot2`
- **Optional**: `urca`, `tseries` (for unit root tests)

All dependencies install automatically.

## Package Structure

```
mfvar2/
├── R/                      # Core functions (13 files)
│   ├── data.R             # Data preparation and transformation
│   ├── prior.R            # Minnesota prior construction
│   ├── mcmc.R             # Gibbs sampler estimation
│   ├── state_space.R      # Kalman filter/smoother
│   ├── forecast.R         # Forecasting functions
│   ├── mdd.R              # Hyperparameter tuning
│   ├── evaluation.R       # Forecast evaluation metrics
│   ├── diagnostics.R      # Convergence diagnostics
│   └── ...                # Other utilities
├── inst/
│   ├── extdata/           # Example datasets (SNB data)
│   └── scripts/           # Production-ready workflows (5 scripts)
│       ├── quick_forecast.R           # Fast forecasting (2-5 min)
│       ├── 00_complete_workflow.R     # Full pipeline
│       ├── 01_prepare_data.R          # Versatile data prep
│       ├── 02_estimate_and_forecast.R # Model estimation
│       └── 03_visualize_results.R     # Comprehensive plots
├── vignettes/             # Tutorials
└── man/                   # Documentation
```

## Quick Start

### Option 1: Use Ready-Made Scripts (Easiest!)

**For rapid testing** (2-5 minutes):

```r
library(mfvar2)
source(system.file("scripts/quick_forecast.R", package = "mfvar2"))

# Works with CSV, Excel, RDS, or data frames:
quick_forecast("path/to/your/data.csv")
```

**For complete workflow** (15-45 minutes with tuning):

```r
library(mfvar2)
# Edit settings at top of this file, then run:
source(system.file("scripts/00_complete_workflow.R", package = "mfvar2"))
```

See `inst/scripts/README.md` for detailed script documentation.

### Option 2: Step-by-Step with Functions

Here's the complete workflow using package functions directly:

```r
library(mfvar2)

# 1. Load and prepare data
# Example: 3 monthly variables + 3 quarterly variables
data_file <- system.file("extdata", "snb_data_3m_3q.rds", package = "mfvar2")
snb_data <- readRDS(data_file)

# Prepare data for MF-VAR
data_prep <- prepare_data_snb(
  data_sources = snb_data,
  quarterly_vars = c("GDP", "Consumption", "Investment"),
  flow_vars = c("GDP", "Consumption", "Investment"),       # These are flow variables
  start_date = "2000-01-01",
  end_date = "2023-12-31",
  difference = TRUE,           # Apply differencing based on unit root tests
  verbose = TRUE
)

# 2. Tune hyperparameters (optional but recommended)
# This searches over ~750 hyperparameter combinations
# Takes 10-30 minutes depending on your machine
hyperparams <- tune_minnesota_hyper(
  data_prepared = data_prep,
  p = 2,                       # 2 lags
  n_gibbs_mdd = 2000,          # Draws for MDD computation
  burnin_mdd = 1000,
  verbose = TRUE,
  seed = 42
)

# Or use default hyperparameters for quick testing:
# hyperparams <- list(lambda1 = 0.2, lambda2 = 1, lambda3 = 1, lambda4 = 1, lambda5 = 1)

# 3. Estimate the model
posterior <- estimate_mf_bvar(
  data_prepared = data_prep,
  p = 2,
  hyperparameters = hyperparams,
  n_draws = 4000,
  burnin = 1000,
  verbose = TRUE,
  seed = 42
)

# 4. Generate forecasts
forecasts <- forecast_mf_bvar(
  posterior = posterior,
  horizon_months = 12,         # Forecast 12 months ahead
  n_sim = 1000,
  seed = 42
)

# 5. Plot forecast fan charts
plot(forecasts, var = "GDP")

# 6. Evaluate forecast accuracy (if you have holdout data)
metrics <- evaluate_forecasts(
  forecasts = forecasts,
  actual = your_test_data,
  metrics = c("RMSE", "MAE", "CRPS")
)
```

### Example Output

After running the workflow above, you'll get:

**Posterior estimates:**

```
=== Bayesian MF-VAR Estimation ===

Variables: 6
Observations: 287 months
Lags: 2
Hyperparameters: λ₁=0.2000, λ₂=1.00, λ₃=1.00, λ₄=1.00, λ₅=1.00
MCMC: 4000 draws + 1000 burnin, thinning=1

Building Minnesota prior...
Initializing...
Running Gibbs sampler...
  [1000/4000] (25.0%) - Time: 1.2s
  [2000/4000] (50.0%) - Time: 2.4s
  [3000/4000] (75.0%) - Time: 3.6s
  [4000/4000] (100.0%) - Time: 4.8s

Convergence diagnostics:
  Geweke z-scores: 95% within [-2, 2] ✓
  Effective sample size: median 3542 (min 2104)
```

**Forecast summary:**

```
12-month ahead forecasts (median with 90% intervals):
          h=1    h=6    h=12
GDP       2.1    1.9    1.7
          [0.8,  [0.2,  [-0.4,
           3.4]   3.6]   3.8]
```

## Core Functions

### Data Preparation

```r
prepare_data_snb(
  data_sources,          # CSV paths, data frame, or list of data frames
  quarterly_vars = NULL, # Names of quarterly variables (auto-detected if NULL)
  flow_vars = NULL,      # Quarterly flow variables (vs. stock)
  start_date = NULL,     # Optional: subset to date range
  end_date = NULL,
  log_transform = NULL,  # Variables to log (auto-detected if NULL)
  difference = TRUE,     # Apply differencing based on ADF tests
  verbose = TRUE
)
```

**Returns:** List with `$data` (zoo object), `$metadata`, `$transformation_params`

### Hyperparameter Tuning

```r
tune_minnesota_hyper(
  data_prepared,
  p,                     # Lag order
  lambda1_grid = c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5),  # Tightness
  lambda2_grid = c(1, 2, 3, 4, 5),                   # Lag decay
  lambda3_grid = c(1),                                # Σ prior (can be tuned!)
  lambda4_grid = c(1, 2, 3, 4, 5),                   # Sum-of-coef
  lambda5_grid = c(1, 2, 3, 4, 5),                   # Co-persistence
  n_gibbs_mdd = 2000,
  burnin_mdd = 1000,
  verbose = TRUE,
  seed = NULL
)
```

**Returns:** List with `$lambda1_optimal`, `$lambda2_optimal`, ..., `$log_mdd`, `$grid_results`

**Note:** All 5 hyperparameters are now tunable. Set any `*_grid` to a single value to fix that parameter.

### Model Estimation

```r
estimate_mf_bvar(
  data_prepared,
  p,                     # Lag order
  hyperparameters,       # From tune_minnesota_hyper() or manual list
  n_draws = 4000,        # Posterior draws to collect
  burnin = 1000,         # Burn-in draws to discard
  thinning = 1,          # Keep every nth draw (1 = no thinning)
  verbose = TRUE,
  seed = NULL
)
```

**Returns:** `mfvar_posterior` object with:

- `$A_draws`: Coefficient draws (n × (n×p+1) × n_draws)
- `$Sigma_draws`: Covariance draws (n × n × n_draws)
- `$states_draws`: Latent state draws
- `$A_mean`, `$Sigma_mean`: Posterior means
- `$diagnostics`: Convergence statistics

### Forecasting

```r
forecast_mf_bvar(
  posterior,
  horizon_months = 12,   # Forecast horizon
  n_sim = 1000,          # Simulations per posterior draw
  quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  seed = NULL
)
```

**Returns:** `mfvar_forecast` object with:

- `$forecasts_monthly`: Quantiles for each variable/horizon
- `$forecasts_quarterly`: Aggregated quarterly forecasts
- `$sim_paths`: Sample simulation paths

### Evaluation

```r
evaluate_forecasts(
  forecasts,
  actual,                # Actual data (zoo object)
  metrics = c("RMSE", "MAE", "MAPE", "CRPS"),
  by_horizon = TRUE
)
```

**Returns:** Data frame with accuracy metrics

## Advanced Usage

### Real-Time Forecasting with Vintages

```r
# Create data vintages with publication lags
vintages <- create_vintages(
  data = snb_data,
  vintage_dates = seq(as.Date("2020-01-01"), as.Date("2023-12-01"), by = "quarter"),
  publication_lags = c(GDP = 2, CPI = 1, IP = 1),  # Months of lag
  verbose = TRUE
)

# Expanding window forecast evaluation
results <- evaluate_expanding_window(
  vintages = vintages,
  p = 2,
  hyperparameters = hyperparams,
  forecast_horizon = 8,
  n_draws = 2000,
  burnin = 500,
  n_cores = 4  # Parallel estimation
)

# Compare to AR(2) benchmark
comparison <- compare_to_ar2(results)
print(comparison$relative_rmse)  # < 1.0 means MF-VAR beats AR(2)
```

### Custom Hyperparameters

If you want to skip tuning and use specific hyperparameters:

```r
# Tight prior (strong shrinkage)
hyper_tight <- list(
  lambda1 = 0.1,   # Low = tight
  lambda2 = 1,
  lambda3 = 1,
  lambda4 = 1,
  lambda5 = 1
)

# Loose prior (weak shrinkage)
hyper_loose <- list(
  lambda1 = 0.5,   # High = loose
  lambda2 = 1,
  lambda3 = 1,
  lambda4 = 1,
  lambda5 = 1
)

posterior_tight <- estimate_mf_bvar(data_prep, p = 2, hyperparameters = hyper_tight)
posterior_loose <- estimate_mf_bvar(data_prep, p = 2, hyperparameters = hyper_loose)
```

### Diagnostic Checks

```r
# Check convergence
diagnostics <- posterior$diagnostics
print(diagnostics$geweke_pvals)       # Should be > 0.05
print(diagnostics$eff_sample_size)    # Should be > 1000

# Plot trace plots
plot_diagnostics(posterior, var = "GDP", coef_type = "AR1")

# Compute DIC (Deviance Information Criterion)
dic <- compute_dic(posterior)
```

## Methodological Details

### State-Space Representation

The package uses companion form for the VAR(p):

**State equation:**

```
X_t = F·X_{t-1} + G·η_t,  η_t ~ N(0, Σ)
```

where X*t = [y_t', y*{t-1}', ..., y\_{t-p+1}']' stacks current and lagged values.

**Observation equation:**

```
y_obs,t = Z_t·X_t
```

Time-varying Z_t handles:

- **Monthly vars**: Always observed (Z_t has 1 in position i)
- **Stock quarterly**: Observed only at quarter-end
- **Flow quarterly**: Observed as (1/3)·(y*t + y*{t-1} + y\_{t-2}) at quarter-end

### Minnesota Prior Specification

Prior variance for coefficient β\_{ij,ℓ} (variable j, equation i, lag ℓ):

```
Var(β_{ij,ℓ}) = (λ₁² · κ_ij² / ℓ^λ₂) · (σ_i² / σ_j²)
```

where:

- κ_ii = 1 (own lags)
- κ_ij = 0.5 for i ≠ j (cross lags, default)
- σ_i = scale from univariate AR(4) fit

Prior mean: 1 for own first lag, 0 otherwise (random walk prior)

Additional dummies:

- **Dummy 4 (λ₄)**: Sum-of-coefficients prior (long-run mean)
- **Dummy 5 (λ₅)**: Co-persistence prior (joint stationarity)

### Gibbs Sampler Algorithm

**Initialization:**

1. OLS estimates on balanced panel
2. Run Kalman filter to get initial states

**Iteration t:**

1. **Draw states** α\_{1:T} | A, Σ, y using Carter-Kohn simulation smoother
2. **Draw coefficients** A | α\_{1:T}, Σ, y from Normal posterior
3. **Draw covariance** Σ | α\_{1:T}, A, y from Inverse-Wishart posterior

**Convergence:**

- Geweke diagnostic (z-scores from first 10% vs last 50%)
- Effective sample size (accounting for autocorrelation)
- Trace plot inspection

### Hyperparameter Selection

Grid search maximizes marginal data density:

```
log p(y | λ₁, ..., λ₅) ≈ log[ (1/R) Σᵣ p(y | θ^(r))⁻¹ ]⁻¹
```

using Geweke (1999) truncated importance sampling with MVN(θ̂, τ·Σ̂) proposal.

Default grids (typically 750 combinations, but all parameters are now tunable):

- λ₁ ∈ {0.05, 0.1, 0.15, 0.2, 0.3, 0.5} (6 values)
- λ₂ ∈ {1, 2, 3, 4, 5} (5 values)
- λ₃ ∈ {1} (1 value, but can be expanded e.g., {0.5, 1, 2})
- λ₄ ∈ {1, 2, 3, 4, 5} (5 values)
- λ₅ ∈ {1, 2, 3, 4, 5} (5 values)

**Note:** λ₃ is typically fixed at 1, but can now be tuned. To tune it, provide a grid like `lambda3_grid = c(0.5, 1, 2)` in `tune_minnesota_hyper()` or the workflow scripts.

## Example Datasets

### Swiss National Bank Data

```r
# Load example data (3 monthly + 3 quarterly variables)
data_file <- system.file("extdata", "snb_data_3m_3q.rds", package = "mfvar2")
snb_data <- readRDS(data_file)

# Variables included:
# - CPI (monthly, stock)
# - Industrial Production (monthly, stock)
# - Unemployment Rate (monthly, stock)
# - GDP (quarterly, flow)
# - Consumption (quarterly, flow)
# - Investment (quarterly, flow)

# Sample period: 2000-01 to 2023-12
```

### Creating Your Own Dataset

Your data should be:

1. **Data frame** or **list of data frames** with:
   - Date column (parseable as Date or yearmon)
   - One column per variable
2. **CSV files** with:
   - First column: dates
   - Remaining columns: variables

Example:

```r
# From data frame
df <- data.frame(
  date = seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "month"),
  cpi = rnorm(288),
  gdp = c(rep(NA, 2), rnorm(96), rep(NA, 2), ...),  # NA for non-quarter-end
  ...
)

data_prep <- prepare_data_snb(
  data_sources = df,
  quarterly_vars = "gdp",
  flow_vars = "gdp"
)

# From CSV files
data_prep <- prepare_data_snb(
  data_sources = c("data/monthly.csv", "data/quarterly.csv"),
  quarterly_vars = c("gdp", "consumption")
)
```

## Performance Tips

### Speed Up Estimation

1. **Use fewer posterior draws for initial exploration:**

   ```r
   posterior <- estimate_mf_bvar(data_prep, p = 2, n_draws = 1000, burnin = 200)
   ```

2. **Parallelize hyperparameter tuning:**

   ```r
   # Use smaller grids
   hyper <- tune_minnesota_hyper(
     data_prep, p = 2,
     lambda1_grid = c(0.1, 0.2, 0.3),  # 3 instead of 6
     lambda2_grid = c(1, 3, 5),        # 3 instead of 5
     n_gibbs_mdd = 1000                # Faster MDD
   )
   ```

3. **Use thinning for large systems:**
   ```r
   posterior <- estimate_mf_bvar(data_prep, p = 2, n_draws = 4000, thinning = 2)
   # Collects 2000 draws (every 2nd)
   ```

### Memory Management

For long time series or many variables:

```r
# Store only essential draws
posterior_light <- estimate_mf_bvar(
  data_prep, p = 2,
  n_draws = 2000,
  store_states = FALSE  # Don't store all state draws (saves memory)
)

# Save results efficiently
saveRDS(posterior, "results.rds", compress = "xz")
```

## Troubleshooting

### Common Issues

**1. "Non-positive definite covariance matrix"**

```r
# Solution: Check for perfect collinearity
cor_matrix <- cor(data_prep$data, use = "pairwise.complete.obs")
high_cor <- which(abs(cor_matrix) > 0.99 & cor_matrix != 1, arr.ind = TRUE)
# Remove one of the highly correlated variables
```

**2. "Geweke diagnostic failed"**

```r
# Solution: Increase burn-in
posterior <- estimate_mf_bvar(data_prep, p = 2, burnin = 2000)

# Or use longer chains
posterior <- estimate_mf_bvar(data_prep, p = 2, n_draws = 10000)
```

**3. "Hyperparameter tuning takes too long"**

```r
# Solution: Use coarser grid
hyper <- tune_minnesota_hyper(
  data_prep, p = 2,
  lambda1_grid = c(0.1, 0.2),  # Fewer points
  lambda2_grid = c(1, 3),
  n_gibbs_mdd = 1000            # Fewer draws
)
```

**4. "Forecasts are too wide/uncertain"**

```r
# Solution: Use tighter prior (smaller λ₁)
hyper_tight <- list(lambda1 = 0.1, lambda2 = 1, lambda3 = 1,
                    lambda4 = 1, lambda5 = 1)
posterior <- estimate_mf_bvar(data_prep, p = 2, hyperparameters = hyper_tight)
```

## Comparison with Other Packages

| Package     | Mixed Freq | Minnesota Prior | State-Space | Hyper Tuning |
| ----------- | ---------- | --------------- | ----------- | ------------ |
| **mfvar2**  | ✅         | ✅ (5 params)   | ✅ KFAS     | ✅ MDD       |
| `BVAR`      | ❌         | ✅              | ❌          | ✅           |
| `MixedFreq` | ✅         | ❌              | ✅          | ❌           |
| `bvartools` | ❌         | ✅              | ❌          | ❌           |
| `vars`      | ❌         | ❌              | ❌          | ❌           |

**mfvar2 is the only R package that combines:**

- True mixed frequency modeling (no pre-aggregation)
- Full 5-parameter Minnesota prior
- Empirical Bayes hyperparameter selection
- Flow variable aggregation (Mariano-Murasawa)

## Citation

The original methodology:

```bibtex
@article{schorfheide2015,
  title={Real-time forecasting with a mixed-frequency VAR},
  author={Schorfheide, Frank and Song, Dongho},
  journal={Journal of Business \& Economic Statistics},
  volume={33},
  number={3},
  pages={366--380},
  year={2015},
  publisher={Taylor \& Francis}
}
```

## References

1. **Schorfheide, F., & Song, D. (2015).** Real-time forecasting with a mixed-frequency VAR. _Journal of Business & Economic Statistics_, 33(3), 366-380.

2. **Mariano, R. S., & Murasawa, Y. (2003).** A new coincident index of business cycles based on monthly and quarterly series. _Journal of Applied Econometrics_, 18(4), 427-443.

3. **Durbin, J., & Koopman, S. J. (2012).** _Time series analysis by state space methods_ (2nd ed.). Oxford University Press.

4. **Geweke, J. (1999).** Using simulation methods for Bayesian econometric models: Inference, development, and communication. _Econometric Reviews_, 18(1), 1-73.

5. **Litterman, R. B. (1986).** Forecasting with Bayesian vector autoregressions: Five years of experience. _Journal of Business & Economic Statistics_, 4(1), 25-38.

6. **Giannone, D., Reichlin, L., & Small, D. (2008).** Nowcasting: The real-time informational content of macroeconomic data. _Journal of Monetary Economics_, 55(4), 665-676.

**Last Updated:** November 2025  
**Version:** 0.1.0  
**Status:** Production Ready
