#!/bin/bash
#
# DuckDBArray Benchmark Runner
#
# This script mirrors HDF5Array's run_benchmarks.sh but is much simpler because:
#   - No block size variations needed (DuckDB handles memory automatically)
#   - No format variations (just DuckDB/Parquet)
#   - Faster total runtime
#
# To run this script in "batch mode":
#
#   cd path/to/DuckDBArray/inst/scripts/timings_dbs/<machine-name>
#   (time ../../run_benchmarks.sh) >run_benchmarks.log 2>&1 &
#
# Expected runtime: 1-3 hours depending on machine (vs 20-55 hours for HDF5Array)
#
set -e  # exit immediately if a simple command exits with a non-zero status

## Try to use R to obtain the path to Rscript (requires R in the PATH).
RSCRIPT=`R -s --vanilla -e 'cat(file.path(R.home("bin"), "Rscript"))'`

## Manually set RSCRIPT here if R is not in the PATH.
#RSCRIPT=path/to/Rscript

NORMALIZE_AND_PCA_R=`$RSCRIPT -e 'suppressPackageStartupMessages(library(DuckDBArray)); cat(system.file(package="DuckDBArray", "scripts", "normalize_and_PCA.R", mustWork=TRUE))'`

normalize_and_PCA()
{
	ncells="$1"
	num_var_genes="$2"
	part_block_size="$3"
	part_block_shape="$4"
	echo "Running: ncells=$ncells, num_var_genes=$num_var_genes, part_block_size=${part_block_size}Mb, part_block_shape=$part_block_shape"
	$RSCRIPT $NORMALIZE_AND_PCA_R "$ncells" "$num_var_genes" "$part_block_size" "$part_block_shape"
	echo ""
}

# Default block settings (can be overridden by environment variables)
PART_BLOCK_SIZE=${PART_BLOCK_SIZE:-1000}  # 1000 Mb = 1 GB
PART_BLOCK_SHAPE=${PART_BLOCK_SHAPE:-scale}

echo "=== DuckDBArray Benchmark Suite ==="
echo "Starting run_benchmarks.sh on `date`."
echo ""
echo "Partition settings: part_block_size=${PART_BLOCK_SIZE}Mb, part_block_shape=$PART_BLOCK_SHAPE"
echo "(Override with: PART_BLOCK_SIZE=250 PART_BLOCK_SHAPE=scale ./run_benchmarks.sh)"
echo ""

# Cell counts to test (same as HDF5Array)
for ncells in 12500 25000 50000 100000 200000; do
	# Number of variable genes (same as HDF5Array)
	for num_var_genes in 1000 2000; do
		normalize_and_PCA "$ncells" "$num_var_genes" "$PART_BLOCK_SIZE" "$PART_BLOCK_SHAPE"
	done
done

echo "=== Benchmark Complete ==="
echo "Completed run_benchmarks.sh on `date`."
echo ""

dest_file="timings-`date +\%Y\%m\%d`.dcf"
mv timings.dcf $dest_file
echo "See timings in '$dest_file'."
echo ""

# Print summary
echo "=== Quick Summary ==="
echo "DuckDBArray completed benchmarks for:"
echo "  - Cell counts: 12500, 25000, 50000, 100000, 200000"
echo "  - Variable genes: 1000, 2000"
echo "  - Total runs: 10 (vs 240 for HDF5Array with block size/format variations)"
echo ""
