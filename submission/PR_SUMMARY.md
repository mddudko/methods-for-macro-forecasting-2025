# Full CV Run for Manual MF-VAR with lambda1=0.06

This PR adds the capability to run a full cross-validation of the manual MF-VAR implementation with lambda1=0.06.

## What's Included

### 1. Main Script
- **`scripts/run_cv_manual_lambda006.R`** - Standalone CV runner for manual MF-VAR

### 2. Helper Script
- **`run_cv_lambda006.sh`** - Bash wrapper with pre-flight checks and timing

### 3. Documentation
- **`RUN_CV_LAMBDA006.md`** - Complete instructions and troubleshooting
- **`LAMBDA006_CONFIG.md`** - Configuration details and rationale

## Quick Start

```bash
cd submission/
./run_cv_lambda006.sh
```

Or directly:

```bash
cd submission/
Rscript scripts/run_cv_manual_lambda006.R
```

## Key Changes from Default Configuration

| Parameter | Default | This Run | Reason |
|-----------|---------|----------|--------|
| lambda1 | 0.04 | **0.06** | Optimal from sensitivity analysis (RMSE 0.748 vs 0.801) |
| Mode | Fast (800/250/250) | **Full (2000/700/600)** | Complete MCMC sampling |
| Models | All models | **Manual only** | Focus on specific model, save time |

## Outputs

Files generated in `output/benchmarks/`:

```
csv/
  mfvar_manual_lambda006_cv_predictions.csv  # All fold predictions
  mfvar_manual_lambda006_cv_metrics.csv      # Aggregated metrics
  mfvar_manual_lambda006_cv_timings.csv      # Timing per fold
mfvar_manual_lambda006_summary.md            # Human-readable summary
```

## Why This Matters

1. **Lambda1=0.06 is optimal**: Based on sensitivity analysis in the README, this value achieves the best RMSE (0.748) compared to 0.04, 0.08, or 0.10.

2. **Full mode ensures quality**: Using 2000 draws with 700 burnin gives more stable posterior estimates than fast mode.

3. **Separate outputs enable comparison**: The dedicated output files make it easy to compare against the existing lambda1=0.04 results.

4. **Ready for combination**: The output format matches the main benchmark structure, so results can be merged later.

## Expected Runtime

- **10 folds** (default): ~30-60 minutes
- **20 folds**: ~60-120 minutes

Each fold processes:
- 3 target variables (gdp_growth, inflation, exch_rate)
- 2 forecast horizons (1-step, 4-step ahead)
- 3 coverage options (cutoff only, +1 month, +2 months)

## Combining Results

After the run completes, combine with other models:

```r
# Load new manual results
manual_new <- readr::read_csv("output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv")

# Load existing benchmarks (excluding old manual results)
others <- readr::read_csv("output/benchmarks/csv/model_benchmark_cv_predictions.csv") %>%
  dplyr::filter(model != "MF-VAR (manual)")

# Combine
combined <- dplyr::bind_rows(others, manual_new)
readr::write_csv(combined, "output/benchmarks/csv/model_benchmark_cv_predictions_combined.csv")
```

## Verification

After completion:

```bash
# Check predictions count (should be ~180-360 depending on folds)
wc -l output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv

# View summary
cat output/benchmarks/mfvar_manual_lambda006_summary.md

# Check metrics
head output/benchmarks/csv/mfvar_manual_lambda006_cv_metrics.csv
```

## Files in This PR

```
submission/
├── scripts/
│   └── run_cv_manual_lambda006.R          # Main CV runner
├── run_cv_lambda006.sh                     # Bash wrapper
├── RUN_CV_LAMBDA006.md                     # Instructions
├── LAMBDA006_CONFIG.md                     # Configuration details
└── PR_SUMMARY.md                           # This file
```

## Next Steps

1. Set up R environment: `renv::restore()`
2. Run the script: `./run_cv_lambda006.sh`
3. Wait for completion (~30-60 min)
4. Verify outputs
5. Combine with other models if needed

## Notes

- The script only runs the manual MF-VAR model (not other models) to save computation time
- Results are saved with a unique prefix to avoid overwriting existing benchmark results
- The script can be run multiple times with different configurations (e.g., different fold counts)
- All outputs use the same structure as the main benchmarks for easy integration
