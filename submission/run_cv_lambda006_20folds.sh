#!/bin/bash
# Run CV for manual MF-VAR with lambda1=0.06 and 20 folds
# This script is configured specifically for 20 folds as requested

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}MF-VAR (manual) CV with lambda1=0.06${NC}"
echo -e "${GREEN}Running with 20 folds${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# Check if we're in the right directory
if [ ! -f "scripts/run_cv_manual_lambda006.R" ]; then
    echo -e "${RED}Error: Must run from submission/ directory${NC}"
    echo "Current directory: $(pwd)"
    exit 1
fi

# Check if Rscript is available
if ! command -v Rscript &> /dev/null; then
    echo -e "${RED}Error: Rscript not found${NC}"
    echo "Please install R (version >= 4.3.0)"
    exit 1
fi

echo "R version:"
Rscript --version
echo ""

# Check if models/mfvar2 exists
if [ ! -d "models/mfvar2" ]; then
    echo -e "${YELLOW}Warning: models/mfvar2 not found${NC}"
    echo "The manual MF-VAR package may not be available"
fi

echo -e "${GREEN}Starting CV run with 20 folds...${NC}"
echo "Configuration:"
echo "  - lambda1: 0.06"
echo "  - n_draws: 2000"
echo "  - burnin: 700"
echo "  - n_sim: 600"
echo "  - folds: 20"
echo ""
echo "This may take 60-120 minutes..."
echo ""

# Run with timing
START_TIME=$(date +%s)

Rscript scripts/run_cv_manual_lambda006.R --max-folds=20

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))
MINUTES=$((ELAPSED / 60))
SECONDS=$((ELAPSED % 60))

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}CV Complete!${NC}"
echo -e "${GREEN}========================================${NC}"
echo "Total time: ${MINUTES}m ${SECONDS}s"
echo ""
echo "Output files:"
echo "  - output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv"
echo "  - output/benchmarks/csv/mfvar_manual_lambda006_cv_metrics.csv"
echo "  - output/benchmarks/csv/mfvar_manual_lambda006_cv_timings.csv"
echo "  - output/benchmarks/mfvar_manual_lambda006_summary.md"
echo ""
echo "To verify:"
echo "  wc -l output/benchmarks/csv/mfvar_manual_lambda006_cv_predictions.csv"
echo "  cat output/benchmarks/mfvar_manual_lambda006_summary.md"
