#!/bin/bash
#
# Side-by-Side Comparison: DuckDBArray vs HDF5Array
#
# This script runs both backends on the same machine with the same parameters
# for a direct comparison. 
#
# Usage:
#   cd path/to/DuckDBArray/inst/scripts
#   ./run_comparison.sh <ncells> <num_var_genes>
#
# Examples:
#   ./run_comparison.sh 50000 1000    # Quick test (~5-10 min)
#   ./run_comparison.sh 200000 1000   # Full comparison (~30-60 min)
#
# For batch mode:
#   (./run_comparison.sh 200000 1000) >comparison.log 2>&1 &
#
set -e

# Check arguments
if [ $# -lt 2 ]; then
    echo "Usage: $0 <ncells> <num_var_genes>"
    echo ""
    echo "Examples:"
    echo "  $0 50000 1000    # Quick test"
    echo "  $0 200000 1000   # Full comparison (matches HDF5Array vignette)"
    exit 1
fi

NCELLS=$1
NUM_VAR_GENES=$2

# Get Rscript path
RSCRIPT=`R -s --vanilla -e 'cat(file.path(R.home("bin"), "Rscript"))'`

# Get script paths
DUCKDB_SCRIPT=`$RSCRIPT -e 'suppressPackageStartupMessages(library(DuckDBArray)); cat(system.file(package="DuckDBArray", "scripts", "normalize_and_PCA.R", mustWork=TRUE))'`
HDF5_SCRIPT=`$RSCRIPT -e 'suppressPackageStartupMessages(library(HDF5Array)); cat(system.file(package="HDF5Array", "scripts", "normalize_and_PCA.R", mustWork=TRUE))'`

echo "============================================================"
echo "Side-by-Side Comparison: DuckDBArray vs HDF5Array"
echo "============================================================"
echo ""
echo "Parameters:"
echo "  ncells:        $NCELLS"
echo "  num_var_genes: $NUM_VAR_GENES"
echo ""
echo "Started: $(date)"
echo ""

# Create output directory
OUTDIR="comparison_${NCELLS}_${NUM_VAR_GENES}"
mkdir -p "$OUTDIR"
cd "$OUTDIR"

echo "============================================================"
echo "Running DuckDBArray (no tuning required)"
echo "============================================================"
echo ""
DUCKDB_START=$(date +%s)
$RSCRIPT "$DUCKDB_SCRIPT" "$NCELLS" "$NUM_VAR_GENES" 2>&1 | tee duckdb.log
DUCKDB_END=$(date +%s)
DUCKDB_TOTAL=$((DUCKDB_END - DUCKDB_START))
mv timings.dcf duckdb_timings.dcf 2>/dev/null || true

echo ""
echo "============================================================"
echo "Running HDF5Array (optimal block size: 250 Mb, sparse format)"
echo "============================================================"
echo ""
echo "Note: HDF5Array requires block size tuning. We use 250 Mb (the"
echo "optimal size from the HDF5Array performance vignette)."
echo ""
HDF5_START=$(date +%s)
# Use format=s (TENxMatrix sparse), block_size=250 (optimal from vignette)
$RSCRIPT "$HDF5_SCRIPT" "$NCELLS" "$NUM_VAR_GENES" s 250 250 250 2>&1 | tee hdf5.log
HDF5_END=$(date +%s)
HDF5_TOTAL=$((HDF5_END - HDF5_START))
mv timings.dcf hdf5_timings.dcf 2>/dev/null || true

echo ""
echo "============================================================"
echo "COMPARISON SUMMARY"
echo "============================================================"
echo ""
echo "Wall clock time (seconds):"
echo "  DuckDBArray: $DUCKDB_TOTAL s"
echo "  HDF5Array:  $HDF5_TOTAL s"
if [ $DUCKDB_TOTAL -gt 0 ]; then
    SPEEDUP=$($RSCRIPT -e "cat(round($HDF5_TOTAL / $DUCKDB_TOTAL, 1))")
    echo "  Speedup:    ${SPEEDUP}x"
fi
echo ""
echo "Completed: $(date)"
echo ""
echo "Detailed results saved to:"
echo "  $(pwd)/duckdb.log"
echo "  $(pwd)/hdf5.log"
echo "  $(pwd)/duckdb_timings.dcf"
echo "  $(pwd)/hdf5_timings.dcf"
echo ""

# Generate comparison report
$RSCRIPT -e "
duckdb <- read.dcf('duckdb_timings.dcf')
hdf5 <- read.dcf('hdf5_timings.dcf')

cat('\n')
cat('Detailed Timing Comparison:\n')
cat('----------------------------------------------------------\n')
cat(sprintf('%-20s %12s %12s %10s\n', 'Step', 'DuckDB (s)', 'HDF5 (s)', 'Speedup'))
cat('----------------------------------------------------------\n')

d_norm <- as.numeric(duckdb[1, 'norm_time'])
h_norm <- as.numeric(hdf5[1, 'norm_time'])
cat(sprintf('%-20s %12.1f %12.1f %9.1fx\n', 'Normalization', d_norm, h_norm, h_norm/d_norm))

d_realize <- as.numeric(duckdb[1, 'realize_time'])
h_realize <- as.numeric(hdf5[1, 'realize_time'])
cat(sprintf('%-20s %12.1f %12.1f %10s\n', 'Realization', d_realize, h_realize, 
    if(d_realize == 0) 'N/A' else sprintf('%.1fx', h_realize/d_realize)))

d_pca <- as.numeric(duckdb[1, 'pca_time'])
h_pca <- as.numeric(hdf5[1, 'pca_time'])
cat(sprintf('%-20s %12.1f %12.1f %9.1fx\n', 'PCA', d_pca, h_pca, h_pca/d_pca))

d_total <- d_norm + d_realize + d_pca
h_total <- h_norm + h_realize + h_pca
cat('----------------------------------------------------------\n')
cat(sprintf('%-20s %12.1f %12.1f %9.1fx\n', 'TOTAL', d_total, h_total, h_total/d_total))
cat('\n')

cat('Key Observations:\n')
cat('  - DuckDBArray required NO block size tuning\n')
cat('  - DuckDBArray required NO realization step\n')
cat('  - HDF5Array used optimal 250 Mb block size (from vignette)\n')
cat('\n')
" 2>/dev/null || echo "(Install R packages for detailed comparison)"

echo "============================================================"
