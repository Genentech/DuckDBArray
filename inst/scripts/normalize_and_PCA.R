# DuckDBArray Benchmark: Normalization and PCA
#
# This script mirrors HDF5Array's normalize_and_PCA.R but is optimized for
# DuckDBArray. Key differences:
#   - Partition block size affects Parquet file structure, not query performance
#   - No "realization" step (data stays in DuckDB format)
#   - SQL-optimized aggregations via DuckDBArray methods
#
# To run this R script:
#
#   Rscript normalize_and_PCA.R <ncells> <num_var_genes> [part_block_size] [part_block_shape]
#
# Arguments:
#   ncells           - Number of cells to process
#   num_var_genes    - Number of variable genes to select
#   part_block_size  - Partition block size in Mb (default: 1000 = 1GB)
#   part_block_shape - Partition block shape: "scale" or "first-dim-grows-first" (default: "scale")
#
# To run it in "batch mode":
#
#   Rscript normalize_and_PCA.R 200000 1000 >normalize_and_PCA.log 2>&1 &
#
# To test with different partition block sizes:
#
#   Rscript normalize_and_PCA.R 200000 1000 250 scale   # 250 Mb partitions
#   Rscript normalize_and_PCA.R 200000 1000 1000 scale  # 1 Gb partitions (default)
#

suppressPackageStartupMessages(library(S4Vectors))
suppressPackageStartupMessages(library(DuckDBArray))
suppressPackageStartupMessages(library(ExperimentHub))
suppressPackageStartupMessages(library(MatrixGenerics))
suppressPackageStartupMessages(library(RSpectra))

# Source process utilities from HDF5Array if available (for memory tracking)
tryCatch({
    process_utils_path <- system.file(package="HDF5Array",
                                      "scripts", "process_utils.R", mustWork=TRUE)
    source(process_utils_path)
    HAS_PROCESS_UTILS <- TRUE
}, error = function(e) {
    HAS_PROCESS_UTILS <<- FALSE
    message("Note: HDF5Array process_utils.R not available; memory tracking disabled")
})

pid <- Sys.getpid()
process_info_log <- tempfile()

## Retrieve and check script arguments.

args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 0L) {
    # Default values for interactive testing
    ncells <- 100000L
    num_var_genes <- 1000L
    part_block_size <- 1000L
    part_block_shape <- "scale"
} else {
    stopifnot(length(args) >= 2L)
    ncells <- as.integer(args[[1L]])
    num_var_genes <- as.integer(args[[2L]])
    part_block_size <- if (length(args) >= 3L) as.integer(args[[3L]]) else 1000L
    part_block_shape <- if (length(args) >= 4L) args[[4L]] else "scale"
}

stopifnot(isSingleInteger(ncells), ncells > 0L,
          isSingleInteger(num_var_genes), num_var_genes > 0L,
          isSingleInteger(part_block_size), part_block_size > 0L,
          part_block_shape %in% c("scale", "first-dim-grows-first"))

cat("=== DuckDBArray Benchmark ===\n")
cat("ncells = ", ncells, "\n", sep="")
cat("num_var_genes = ", num_var_genes, "\n", sep="")
cat("part_block_size = ", part_block_size, " Mb\n", sep="")
cat("part_block_shape = ", part_block_shape, "\n", sep="")
cat("\n")

## Prepare dataset - convert HDF5 to DuckDB format.

cat("Preparing dataset...\n")
hub <- ExperimentHub(localHub=TRUE)
brain_s_path <- suppressMessages(hub[["EH1039"]])

# Load as TENxMatrix first, then convert to DuckDBMatrix
suppressPackageStartupMessages(library(HDF5Array))
brain_s <- TENxMatrix(brain_s_path, group="mm10")
stopifnot(identical(dim(brain_s), c(27998L, 1306127L)))

# Subset to requested number of cells
cat("Subsetting to ", ncells, " cells...\n", sep="")
dataset_hdf5 <- brain_s[, seq_len(ncells)]

# Convert to DuckDBMatrix (transposed storage for efficient feature access)
cat("Converting to DuckDBMatrix...\n")
setAutoBlockSize(part_block_size * 1e6)
setAutoBlockShape(part_block_shape)
if (.Platform$OS.type != "windows") {
    setAutoBPPARAM(BiocParallel::MulticoreParam(progressbar = TRUE))
}
duckdb_path <- tempfile()
conversion_time <- system.time({
    # Transpose for efficient gene access (cells x genes layout in Parquet)
    dataset_t <- t(dataset_hdf5)
    writeCoordArray(dataset_t, duckdb_path)
    dimtbls <- createDimTables(dataset_t)
    dataset <- DuckDBMatrix(duckdb_path, datacol = "value",
                            keycols = list(
                                index2 = setNames(seq_len(ncol(dataset_t)),
                                                  colnames(dataset_t)),
                                index1 = setNames(seq_len(nrow(dataset_t)),
                                                  rownames(dataset_t))
                            ),
                            dimtbls = dimtbls)
})
cat("Conversion completed in ", conversion_time[["elapsed"]], " s.\n", sep="")
cat("DuckDBMatrix dimensions: ", nrow(dataset), " x ", ncol(dataset), "\n\n", sep="")

## Define simple_normalize() - optimized for DuckDBMatrix.
## This function leverages DuckDBArray's SQL-optimized row/col operations.
##
## IMPORTANT: This implementation differs from HDF5Array's simple_normalize()
## in the ORDER of operations. We compute all margin statistics and sweep
## operations on the FULL matrix first, then filter rows at the very end.
##
## Why? DuckDBMatrix excels at SQL aggregations (rowSums, colSums, rowVars,
## rowSds) which run as efficient GROUP BY queries. However, row subsetting
## (e.g., mat[row_sums > 0, ]) creates inefficient NOT IN queries with
## potentially thousands of values. By deferring all filtering to the end,
## we only pay the subsetting cost once, on a small set of selected genes.
##
## The results are mathematically equivalent because:
## 1. Zero-sum rows have zero variance, so they won't be selected as HVGs
## 2. Column sums are unaffected by zero-sum rows
## 3. We explicitly exclude zero-sum rows from HVG selection

simple_normalize <- function(mat, num_var_genes=1000)
{
    stopifnot(length(dim(mat)) == 2, !is.null(rownames(mat)))
    row_sums <- rowSums(mat)
    col_sums <- colSums(mat) / 10000
    mat <- sweep(mat, 2, col_sums, "/")
    row_vars <- rowVars(mat)
    row_vars[row_sums == 0] <- -Inf
    row_vars_order <- order(row_vars, decreasing=TRUE)
    variable_idx <- head(row_vars_order, n=num_var_genes)
    mat <- log1p(mat[variable_idx, ])
    row_sds <- rowSds(mat)
    sweep(mat, 1, row_sds, "/")
}

## Define simple_PCA() - same as HDF5Array version.
## PCA requires matrix operations that work on any DelayedArray backend.

simple_PCA <- function(mat, k=25)
{
    stopifnot(length(dim(mat)) == 2)
    row_means <- rowMeans(mat)
    Ax <- function(x, args)
        (as.numeric(mat %*% x) - row_means * sum(x))
    Atx <- function(x, args)
        (as.numeric(x %*% mat) - as.vector(row_means %*% x))
    RSpectra::svds(Ax, Atrans=Atx, k=k, dim=dim(mat))
}

## Normalization.

cat("Running normalization ...\n")
if (HAS_PROCESS_UTILS) {
    loop_pid <- start_log_process_info(pid, process_info_log)
    on.exit(stop_log_process_info(loop_pid))
}
timing <- system.time(normalized <- simple_normalize(dataset, num_var_genes=num_var_genes))
if (HAS_PROCESS_UTILS) {
    stop_log_process_info(loop_pid)
    norm_max_mem_used <- extract_max_mem_used(process_info_log, pid)
    mem <- paste0(names(norm_max_mem_used), "=",
                  norm_max_mem_used, "Mb", collapse=" ")
} else {
    norm_max_mem_used <- c(max_vsz=NA_integer_, max_rss=NA_integer_)
    mem <- "N/A"
}
norm_time <- timing[["elapsed"]]
cat("---> normalization completed in ", norm_time, " s (", mem, ").\n\n", sep="")

## Note: No "realization" step needed with DuckDB!
## The normalized matrix is already efficiently queryable.

cat("Note: DuckDBArray does not require an on-disk realization step.\n")
cat("      The normalized matrix remains efficiently queryable as a DuckDBMatrix.\n\n")
realize_time <- 0
realize_max_mem_used <- c(max_vsz=0L, max_rss=0L)

## PCA.

cat("Running PCA ...\n")
if (HAS_PROCESS_UTILS) {
    loop_pid <- start_log_process_info(pid, process_info_log)
    on.exit(stop_log_process_info(loop_pid))
}
timing <- system.time(pca <- simple_PCA(normalized))
if (HAS_PROCESS_UTILS) {
    stop_log_process_info(loop_pid)
    pca_max_mem_used <- extract_max_mem_used(process_info_log, pid)
    mem <- paste0(names(pca_max_mem_used), "=",
                  pca_max_mem_used, "Mb", collapse=" ")
} else {
    pca_max_mem_used <- c(max_vsz=NA_integer_, max_rss=NA_integer_)
    mem <- "N/A"
}
pca_time <- timing[["elapsed"]]
cat("---> PCA completed in ", pca_time, " s (", mem,").\n\n", sep="")

## Summary.

total_time <- norm_time + pca_time
cat("=== Summary ===\n")
cat("Normalization: ", norm_time, " s\n", sep="")
cat("PCA:           ", pca_time, " s\n", sep="")
cat("Total:         ", total_time, " s\n", sep="")
cat("\n")

## Write timings to DCF file (compatible with HDF5Array format).

cat("ncells: ", ncells, "\n",
    "num_var_genes: ", num_var_genes, "\n",
    "format: ", "duckdb", "\n",
    "part_block_size: ", part_block_size, "\n",
    "part_block_shape: ", part_block_shape, "\n",
    "norm_time: ", norm_time, "\n",
    "norm_max_vsz: ", norm_max_mem_used[["max_vsz"]], "\n",
    "norm_max_rss: ", norm_max_mem_used[["max_rss"]], "\n",
    "realize_time: ", realize_time, "\n",
    "realize_max_vsz: ", realize_max_mem_used[["max_vsz"]], "\n",
    "realize_max_rss: ", realize_max_mem_used[["max_rss"]], "\n",
    "pca_time: ", pca_time, "\n",
    "pca_max_vsz: ", pca_max_mem_used[["max_vsz"]], "\n",
    "pca_max_rss: ", pca_max_mem_used[["max_rss"]], "\n",
    "\n", sep="", file="timings.dcf", append=TRUE)

