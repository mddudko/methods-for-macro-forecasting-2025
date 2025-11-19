# Quick Start: Run CV for Manual MF-VAR with lambda1=0.06

This branch contains scripts to run a full cross-validation of the manual MF-VAR implementation with lambda1=0.06.

## TL;DR

```bash
cd submission/
./run_cv_lambda006.sh
```

Wait 30-60 minutes, then check outputs in `output/benchmarks/`.

## What This Does

Runs a complete cross-validation with:
- **lambda1 = 0.06** (optimal from sensitivity analysis)
- **Full MCMC mode** (2000 draws, 700 burnin, 600 simulations)
- **Manual MF-VAR only** (other models already run)
- **10 folds × 3 coverage options** = 30 CV runs

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
# Direct R execution
cd submission/
Rscript scripts/run_cv_manual_lambda006.R

# Custom fold count (e.g., 20 folds)
Rscript scripts/run_cv_manual_lambda006.R --max-folds=20
```

## Verification

```bash
# Check output has data (should be ~180+ lines for 10 folds)
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
