# Quick Start: Run CV for Manual MF-VAR with lambda1=0.06

This branch contains scripts to run a full cross-validation of the manual MF-VAR implementation with lambda1=0.06.

## TL;DR

```bash
cd submission/

# Run with 20 folds (recommended, ~60-120 minutes)
./run_cv_lambda006_20folds.sh

# Or run with 10 folds (default, ~30-60 minutes)
./run_cv_lambda006.sh
```

Then check outputs in `output/benchmarks/`.

## What This Does

Runs a complete cross-validation with:
- **lambda1 = 0.06** (optimal from sensitivity analysis)
- **Full MCMC mode** (2000 draws, 700 burnin, 600 simulations)
- **Manual MF-VAR only** (other models already run)
- **20 folds × 3 coverage options** = 60 CV runs (recommended)
- Or **10 folds × 3 coverage options** = 30 CV runs (default)

## Output Files

```
output/benchmarks/
├── csv/
│   ├── mfvar_manual_lambda006_cv_predictions.csv  # All predictions
│   ├── mfvar_manual_lambda006_cv_metrics.csv      # RMSE/MAE by variable
│   └── mfvar_manual_lambda006_cv_timings.csv      # Performance data
└── mfvar_manual_lambda006_summary.md              # Human-readable summary
```

## Documentation

- **`submission/PR_SUMMARY.md`** - Overview of this PR
- **`submission/RUN_CV_LAMBDA006.md`** - Detailed instructions
- **`submission/LAMBDA006_CONFIG.md`** - Configuration details

## Prerequisites

1. R >= 4.3.0 installed
2. Dependencies installed: `cd submission/ && Rscript -e "renv::restore()"`
3. `models/mfvar2/` package available

## Alternative Execution

```bash
# Recommended: 20 folds
cd submission/
./run_cv_lambda006_20folds.sh

# Or with default 10 folds
./run_cv_lambda006.sh

# Or direct R execution with custom fold count
Rscript scripts/run_cv_manual_lambda006.R --max-folds=20
Rscript scripts/run_cv_manual_lambda006.R --max-folds=10
```

## Verification

```bash
# Check output has data
# For 20 folds: should be ~360 lines
# For 10 folds: should be ~180 lines
wc -l output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv

# View summary
cat output/benchmarks/mfvar_manual_lambda006_summary.md
```

## Why lambda1=0.06?

Based on sensitivity analysis:
- **lambda1=0.06**: Average RMSE = 0.748 ⭐ (Best)
- lambda1=0.08: Average RMSE = 0.801
- lambda1=0.10: Average RMSE = 0.937

## Combining with Other Models

```r
# R code to merge results
manual_new <- readr::read_csv("output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv")
others <- readr::read_csv("output/benchmarks/csv/model_benchmark_cv_predictions.csv") %>%
  dplyr::filter(model != "MF-VAR (manual)")
combined <- dplyr::bind_rows(others, manual_new)
readr::write_csv(combined, "output/benchmarks/csv/combined_cv_predictions.csv")
```

## Questions?

See detailed documentation in `submission/RUN_CV_LAMBDA006.md`.
