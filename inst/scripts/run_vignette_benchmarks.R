#!/usr/bin/env Rscript
# Run vignette benchmarks and output timing results
# Usage: Rscript run_vignette_benchmarks.R

suppressPackageStartupMessages({
    library(DelayedArray)
    library(HDF5Array)
    library(TileDBArray)
    library(DuckDBArray)
    library(Matrix)
    library(MatrixGenerics)
    library(scuttle)
    library(scran)
    library(scater)
    library(ExperimentHub)
    library(BiocParallel)
})

# Configure
setAutoBlockSize(2^30)
setAutoBlockShape("scale")
BPPARAM <- SerialParam()
setAutoBPPARAM(BPPARAM)

cat("=== DuckDBArray Vignette Benchmarks ===\n\n")

# Load data
cat("Loading 10x Brain Cell Dataset...\n")
hub <- ExperimentHub()
brain_path <- hub[["EH1039"]]
brain_full <- TENxMatrix(brain_path, group = "mm10")

n_cells <- 12500
brain_hdf5 <- brain_full[, seq_len(n_cells)]
cat(sprintf("Dataset: %d genes x %d cells\n", nrow(brain_hdf5), ncol(brain_hdf5)))

# Create backends
cat("\nCreating backends...\n")

# In-memory
time_to_mem <- system.time({
    brain_mem <- as(brain_hdf5, "dgCMatrix")
})
cat(sprintf("dgCMatrix: %.1f sec\n", time_to_mem["elapsed"]))

# TileDBArray
tiledb_path <- tempfile()
time_tiledb <- system.time({
    brain_tiledb <- writeTileDBArray(brain_mem, path = tiledb_path)
})
cat(sprintf("TileDBArray: %.1f sec\n", time_tiledb["elapsed"]))

# DuckDBMatrix
duckdb_path <- tempfile()
time_duckdb <- system.time({
    brain_t <- t(brain_mem)
    writeCoordArray(brain_t, duckdb_path)
    dimtbls <- createDimTables(brain_t)
    brain_ddb <- DuckDBMatrix(duckdb_path, datacol = "value",
                              keycols = list(
                                  index2 = setNames(seq_len(ncol(brain_t)), colnames(brain_t)),
                                  index1 = setNames(seq_len(nrow(brain_t)), rownames(brain_t))
                              ),
                              dimtbls = dimtbls)
})
cat(sprintf("DuckDBMatrix: %.1f sec\n", time_duckdb["elapsed"]))

results <- list()

# Benchmark function
bench <- function(name, expr_mem, expr_hdf5, expr_tiledb, expr_ddb) {
    cat(sprintf("\n--- %s ---\n", name))
    
    time_mem <- system.time(eval(expr_mem))["elapsed"]
    time_hdf5 <- system.time(eval(expr_hdf5))["elapsed"]
    time_tiledb <- tryCatch(system.time(eval(expr_tiledb))["elapsed"], error = function(e) NA)
    time_ddb <- system.time(eval(expr_ddb))["elapsed"]
    
    cat(sprintf("  In-memory: %.2f sec\n", time_mem))
    cat(sprintf("  HDF5Array: %.2f sec\n", time_hdf5))
    cat(sprintf("  TileDBArray: %s\n", ifelse(is.na(time_tiledb), "N/A", sprintf("%.2f sec", time_tiledb))))
    cat(sprintf("  DuckDBMatrix: %.2f sec\n", time_ddb))
    cat(sprintf("  DuckDB vs HDF5: %.1fx faster\n", time_hdf5 / time_ddb))
    
    results[[name]] <<- c(InMemory = time_mem, HDF5Array = time_hdf5, 
                           TileDBArray = time_tiledb, DuckDB = time_ddb)
}

# Feature selection benchmarks
cat("\n=== Feature Selection ===\n")
bench("colSums",
      quote(Matrix::colSums(brain_mem)),
      quote(colSums(brain_hdf5)),
      quote(colSums(brain_tiledb)),
      quote(colSums(brain_ddb)))

bench("rowVars",
      quote(rowVars(brain_mem)),
      quote(rowVars(brain_hdf5)),
      quote(rowVars(brain_tiledb)),
      quote(rowVars(brain_ddb)))

bench("rowDeviances",
      quote(rowDeviances(brain_mem, family = "binomial")),
      quote(rowDeviances(brain_hdf5, family = "binomial")),
      quote(rowDeviances(brain_tiledb, family = "binomial")),
      quote(rowDeviances(brain_ddb, family = "binomial")))

bench("rowNnzs",
      quote(rowNnzs(brain_mem)),
      quote(rowNnzs(brain_hdf5)),
      quote(NA),  # TileDBArray doesn't support rowCounts for sparse blocks
      quote(rowNnzs(brain_ddb)))

bench("nexprs",
      quote(nexprs(brain_mem)),
      quote(nexprs(brain_hdf5)),
      quote(nexprs(brain_tiledb)),
      quote(nexprs(brain_ddb)))

# QC benchmarks
cat("\n=== QC Metrics ===\n")
bench("perCellQCMetrics",
      quote(perCellQCMetrics(brain_mem)),
      quote(perCellQCMetrics(brain_hdf5)),
      quote(perCellQCMetrics(brain_tiledb)),
      quote(perCellQCMetrics(brain_ddb)))

bench("perFeatureQCMetrics",
      quote(perFeatureQCMetrics(brain_mem)),
      quote(perFeatureQCMetrics(brain_hdf5)),
      quote(perFeatureQCMetrics(brain_tiledb)),
      quote(perFeatureQCMetrics(brain_ddb)))

# Pseudo-bulk
cat("\n=== Pseudo-bulk ===\n")
cell_types <- paste0("Cluster_", sample(1:20, n_cells, replace = TRUE))
bench("summarizeAssayByGroup",
      quote(summarizeAssayByGroup(brain_mem, cell_types, statistics = "sum")),
      quote(summarizeAssayByGroup(brain_hdf5, cell_types, statistics = "sum")),
      quote(summarizeAssayByGroup(brain_tiledb, cell_types, statistics = "sum")),
      quote(summarizeAssayByGroup(brain_ddb, cell_types, statistics = "sum")))

# Normalization
cat("\n=== Normalization ===\n")
bench("normalizeCounts",
      quote(normalizeCounts(brain_mem)),
      quote(normalizeCounts(brain_hdf5)),
      quote(normalizeCounts(brain_tiledb)),
      quote(normalizeCounts(brain_ddb)))

# Create log-normalized matrices for scran benchmarks
cat("\nCreating log-normalized matrices for scran benchmarks...\n")
log_mem <- normalizeCounts(brain_mem)
log_hdf5 <- normalizeCounts(brain_hdf5)
log_tiledb <- tryCatch(normalizeCounts(brain_tiledb), error = function(e) NULL)
log_ddb <- normalizeCounts(brain_ddb)

# scran benchmarks
cat("\n=== scran Methods ===\n")
bench("modelGeneVar",
      quote(modelGeneVar(log_mem)),
      quote(modelGeneVar(log_hdf5)),
      quote(NA),  # TileDBArray fails
      quote(modelGeneVar(log_ddb)))

bench("modelGeneVarByPoisson",
      quote(modelGeneVarByPoisson(brain_mem)),
      quote(modelGeneVarByPoisson(brain_hdf5)),
      quote(NA),
      quote(modelGeneVarByPoisson(brain_ddb)))

bench("modelGeneCV2",
      quote(modelGeneCV2(brain_mem)),
      quote(modelGeneCV2(brain_hdf5)),
      quote(NA),
      quote(modelGeneCV2(brain_ddb)))

# correlatePairs with HVGs
mgv_ddb <- modelGeneVar(log_ddb)
hvg <- head(order(mgv_ddb$bio, decreasing = TRUE), 200)
bench("correlatePairs",
      quote(correlatePairs(log_mem, subset.row = hvg)),
      quote(correlatePairs(log_hdf5, subset.row = hvg)),
      quote(NA),
      quote(correlatePairs(log_ddb, subset.row = hvg)))

bench("pairwiseTTests",
      quote(pairwiseTTests(log_mem, groups = cell_types)),
      quote(pairwiseTTests(log_hdf5, groups = cell_types)),
      quote(NA),
      quote(pairwiseTTests(log_ddb, groups = cell_types)))

bench("pairwiseBinom",
      quote(pairwiseBinom(log_mem, groups = cell_types)),
      quote(pairwiseBinom(log_hdf5, groups = cell_types)),
      quote(NA),
      quote(pairwiseBinom(log_ddb, groups = cell_types)))

bench("findMarkers",
      quote(findMarkers(log_mem, groups = cell_types, BPPARAM = BPPARAM)),
      quote(findMarkers(log_hdf5, groups = cell_types, BPPARAM = BPPARAM)),
      quote(NA),
      quote(findMarkers(log_ddb, groups = cell_types, BPPARAM = BPPARAM)))

bench("scoreMarkers",
      quote(scoreMarkers(log_mem, groups = cell_types)),
      quote(scoreMarkers(log_hdf5, groups = cell_types)),
      quote(NA),
      quote(scoreMarkers(log_ddb, groups = cell_types)))

bench("summaryMarkerStats",
      quote(summaryMarkerStats(log_mem, groups = cell_types)),
      quote(summaryMarkerStats(log_hdf5, groups = cell_types)),
      quote(NA),
      quote(summaryMarkerStats(log_ddb, groups = cell_types)))

# Summary table
cat("\n\n=== SUMMARY TABLE ===\n\n")
summary_df <- do.call(rbind, lapply(names(results), function(op) {
    r <- results[[op]]
    data.frame(
        Operation = op,
        InMemory = round(r["InMemory"], 2),
        HDF5Array = round(r["HDF5Array"], 2),
        DuckDB = round(r["DuckDB"], 2),
        DuckDB_vs_HDF5 = round(r["HDF5Array"] / r["DuckDB"], 1),
        stringsAsFactors = FALSE
    )
}))
print(summary_df, row.names = FALSE)

# Save results
output_file <- "benchmark_results.rds"
saveRDS(results, output_file)
cat(sprintf("\nResults saved to %s\n", output_file))

cat("\n=== Benchmark Complete ===\n")



