# MF-VAR Manual CV Configuration Comparison

## Lambda1 Parameter Change

| Configuration | Previous Value | New Value | Purpose |
|--------------|----------------|-----------|---------|
| lambda1 (Minnesota prior tightness) | 0.04 | **0.06** | Controls the overall shrinkage toward the prior |

## Why lambda1=0.06?

According to the README, lambda1=0.06 is optimal from sensitivity analysis:
- Average RMSE = 0.748 (with lambda1=0.06)
- Average RMSE = 0.801 (with lambda1=0.08) 
- Average RMSE = 0.937 (with lambda1=0.10)

This represents the best balance between:
- **Lower values** (e.g., 0.04): More shrinkage, less variance, but potential underfitting
- **Higher values** (e.g., 0.1): Less shrinkage, more variance, potential overfitting

## Full Configuration Details

### MCMC Settings (Full Mode)

| Parameter | Fast Mode | Full Mode (This Run) |
|-----------|-----------|---------------------|
| n_draws | 800 | **2000** |
| burnin | 250 | **700** |
| n_sim | 250 | **600** |

### Hyperparameters

| Parameter | Value | Description |
|-----------|-------|-------------|
| lambda1 | **0.06** | Overall shrinkage (Minnesota prior) |
| lambda2 | 1.0 | Cross-variable shrinkage |
| lambda3 | 1.0 | Lag decay rate |
| lambda4 | 1.0 | Exogenous variable shrinkage |
| lambda5 | 1.0 | Contemporaneous shrinkage |

### Model Settings

| Parameter | Value |
|-----------|-------|
| p (lags) | 2 |
| Forecast horizon | 12 months (4 quarters) |
| Target variables | gdp_growth, inflation, exch_rate |
| Forecast steps | 1-step ahead, 4-step ahead (1-year) |

### Cross-Validation Settings

| Parameter | Value |
|-----------|-------|
| Initial training quarter | 2015 Q4 |
| Max folds (default) | 10 |
| Coverage options | 0, 1, 2 extra months |
| Total CV runs | 10 folds × 3 coverage = 30 runs |

## Output Files Structure

### mfvar_manual_lambda006_cv_predictions.csv

Each row represents one prediction:
```
monthly_coverage,extra_months,fold,cutoff_quarter,forecast_quarter,variable,step_ahead,prediction,model,quarter_end,horizon,actual,error
```

Example:
```
Cutoff only,0,111,2019 Q3,2019 Q4,gdp_growth,1,2.5,MF-VAR (manual),2019-12-31,1-step ahead,2.3,0.2
```

### mfvar_manual_lambda006_cv_metrics.csv

Aggregated metrics by variable and horizon:
```
monthly_coverage,extra_months,variable,model,horizon,rmse,mae,observations
```

Example:
```
Cutoff only,0,gdp_growth,MF-VAR (manual),1-step ahead,0.85,0.65,10
```

## Comparing with Previous Results

To compare the impact of changing lambda1 from 0.04 to 0.06:

```bash
# Extract manual model results from existing benchmark
grep "MF-VAR (manual)" output/benchmarks/csv/model_benchmark_cv_predictions.csv > old_manual_lambda004.csv

# Compare metrics
# Old (lambda1=0.04):
grep "MF-VAR (manual)" output/benchmarks/csv/model_benchmark_cv_metrics.csv

# New (lambda1=0.06):
cat output/benchmarks/csv/mfvar_manual_lambda006_cv_metrics.csv
```

## Expected Changes with lambda1=0.06

Compared to lambda1=0.04, we expect:
- **Slightly higher variance** in predictions (less shrinkage)
- **Potentially better fit** to historical patterns
- **Lower RMSE** on average (based on sensitivity analysis)
- **Better performance** particularly for GDP growth forecasts

## Implementation Note

The script `run_cv_manual_lambda006.R` runs **only** the manual MF-VAR model to:
1. Save computation time (skip other models that don't need re-running)
2. Generate separate output files for easy comparison
3. Allow merging with existing benchmark results later

To merge with other models:
```r
# R code to merge
manual_new <- readr::read_csv("output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv")
others <- readr::read_csv("output/benchmarks/csv/model_benchmark_cv_predictions.csv") %>%
  dplyr::filter(model != "MF-VAR (manual)")
combined <- dplyr::bind_rows(others, manual_new)
```
