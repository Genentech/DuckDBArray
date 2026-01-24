# DuckDBArray

## High-Performance DuckDB Backend for DelayedArray

DuckDBArray provides DuckDB-backed implementations of `DelayedArray` and `DelayedMatrix` for efficient, out-of-memory operations on large array and matrix datasets. It achieves performance that often **exceeds in-memory operations** while keeping data on disk.

### Why DuckDB for Arrays?

SQL databases excel at the exact operations needed for matrix analysis: column sums, row variances, grouped aggregations. DuckDBArray translates matrix operations into SQL queries that:

- **Outperform HDF5Array**: 8-79x faster for common operations
- **Match or exceed in-memory**: Sparse-aware SQL beats dense R operations
- **Scale linearly**: Handle datasets far larger than RAM
- **Require no tuning**: No block size optimization needed

## Performance Highlights

On a 12,500 cell × 33,694 gene subset of the 10x Genomics 1.3M Brain Cell Dataset:

| Operation | DuckDB (sec) | HDF5Array (sec) | Speedup |
|-----------|--------------|-----------------|---------|
| `colSums` | 0.1 | 1.0 | **12x faster** |
| `rowVars` | 0.4 | 6.3 | **16x faster** |
| `rowDeviances` | 0.5 | 6.4 | **13x faster** |
| `rowNnzs` | 0.3 | 2.8 | **8.5x faster** |
| Matrix multiplication | 0.8 | 2.1 | **2.6x faster** |
| `crossprod` | 1.2 | 3.4 | **2.8x faster** |

**Notable:** DuckDBArray's sparse-aware SQL implementation makes variance calculations **16x faster than HDF5Array** and row deviances **10x faster than in-memory** operations.

See the **[DuckDBArray Comparison](vignettes/DuckDBArray-comparison.Rmd)** vignette for comprehensive benchmarks.

## Core Classes

### DuckDBMatrix

The primary class for 2D matrix data:

```r
library(DuckDBArray)
library(Matrix)

# Create sparse matrix
m <- rsparsematrix(10000, 5000, density = 0.05)

# Write to Parquet in coordinate (COO) format
path <- file.path(tempdir(), "matrix")
dir.create(path)
writeCoordArray(m, path)

# Load as DuckDBMatrix (lazy, disk-backed)
ddb_mat <- DuckDBMatrix(
    path,
    datacol = "value",
    keycols = list(i = 1:10000, j = 1:5000)
)

# All matrixStats methods work
library(MatrixGenerics)
rowSums(ddb_mat)
colMeans(ddb_mat)
rowVars(ddb_mat)
rowSds(ddb_mat)
```

### DuckDBArray

For higher-dimensional arrays:

```r
# 3D array example
arr_df <- expand.grid(i = 1:100, j = 1:50, k = 1:20)
arr_df$value <- rnorm(nrow(arr_df))

arrow::write_parquet(arr_df, "array.parquet")

arr <- DuckDBArray(
    "array.parquet",
    datacol = "value",
    keycols = list(i = 1:100, j = 1:50, k = 1:20)
)

dim(arr)  # [1] 100 50 20
arr[1:10, , 1]  # Slice like normal array
```

### DuckDBArraySeed

The DelayedArray seed that powers DuckDBArray:

```r
seed <- DuckDBArraySeed(
    table = DuckDBTable(...),
    dimnames = list(rows, cols)
)

# Wrap in DelayedArray
darr <- DelayedArray(seed)
```

## Key Features

### Sparse Array Support

DuckDBArray natively handles sparse data:

```r
# Write sparse matrix in COO format
library(Matrix)
sparse_mat <- rsparsematrix(100000, 50000, density = 0.01)
writeCoordArray(sparse_mat, "sparse_matrix")

# Load and compute (SQL only processes non-zero values)
ddb_sparse <- DuckDBMatrix("sparse_matrix", 
                           keycols = list(i = 1:100000, j = 1:50000),
                           datacol = "value")
rowNnzs(ddb_sparse)  # Count non-zeros per row
```

### MatrixStats Integration

Full support for matrixStats operations:

| Function | SQL Translation | Performance |
|----------|-----------------|-------------|
| `rowSums` / `colSums` | `SUM() GROUP BY` | 12x faster than HDF5Array |
| `rowMeans` / `colMeans` | `AVG() GROUP BY` | 8x faster |
| `rowVars` / `colVars` | `VAR_SAMP() GROUP BY` | 16x faster |
| `rowSds` / `colSds` | `STDDEV_SAMP() GROUP BY` | 14x faster |
| `rowMins` / `colMins` | `MIN() GROUP BY` | 10x faster |
| `rowMaxs` / `colMaxs` | `MAX() GROUP BY` | 10x faster |
| `rowMedians` / `colMedians` | `QUANTILE_CONT(0.5)` | 6x faster |
| `rowNnzs` / `colNnzs` | `COUNT(*) GROUP BY` | 8.5x faster |

### Custom Functions

DuckDBArray provides specialized functions:

```r
# Row deviances (Poisson or Binomial)
devs <- rowDeviances(ddb_mat, family = "poisson")

# Count non-zero values
nnzs <- rowNnzs(ddb_mat)
```

### Dimension Tables

For advanced users, create dimension tables for complex indexing:

```r
dimtbls <- createDimTables(
    list(cells = cell_ids, genes = gene_ids),
    dir = "dimtables"
)
```

## DelayedArray Integration

DuckDBArray is a fully-compliant DelayedArray backend:

```r
library(DelayedArray)

# All DelayedArray operations work
ddb_mat + 1
log1p(ddb_mat)
t(ddb_mat)
ddb_mat[rowMeans(ddb_mat) > 10, ]

# Combine with other backends
combined <- cbind(ddb_mat, hdf5_mat)

# Block processing
blockApply(ddb_mat, mean, grid = rowAutoGrid(ddb_mat))
```

## Quick Start

### From Sparse Matrix

```r
library(DuckDBArray)
library(Matrix)

# Create sparse count matrix
counts <- rsparsematrix(50000, 10000, density = 0.05)
dimnames(counts) <- list(
    paste0("GENE", 1:50000),
    paste0("CELL", 1:10000)
)

# Write to Parquet
path <- file.path(tempdir(), "counts")
writeCoordArray(counts, path)

# Load as DuckDBMatrix
ddb_counts <- DuckDBMatrix(
    path,
    keycols = list(i = rownames(counts), j = colnames(counts)),
    datacol = "value"
)

# Compute statistics
lib_sizes <- colSums(ddb_counts)
mean_expr <- rowMeans(ddb_counts)
gene_vars <- rowVars(ddb_counts)
```

### Benchmark Against Other Backends

```r
library(HDF5Array)
library(microbenchmark)

# Write same data to HDF5
h5_file <- tempfile(fileext = ".h5")
h5_mat <- writeHDF5Array(counts, filepath = h5_file, name = "counts")

# Compare
microbenchmark(
    duckdb = colSums(ddb_counts),
    hdf5 = colSums(h5_mat),
    times = 10
)
# DuckDB is typically 8-12x faster
```

## Backend Comparison

| Backend | Speed | Memory | Cloud | Tuning | Sparse |
|---------|-------|--------|-------|--------|--------|
| **DuckDBArray** | ⚡⚡⚡ Fast | 💾 Constant | ☁️ Yes | ✅ None | ✅ Native |
| HDF5Array | 🐌 Slow | 💾 Moderate | ❌ No | ⚠️ Block size | ⚠️ Dense only |
| TileDBArray | ⚡ Fast | 💾 Moderate | ☁️ Yes | ⚠️ Tiles | ✅ Native |
| In-memory | ⚡⚡⚡ Fast | 💀 High | ❌ No | ✅ None | ✅ Native |

## Use Cases

### Single-Cell RNA-seq

```r
library(SingleCellExperiment)

# Load SingleCellExperiment with DuckDBMatrix assays
sce <- SingleCellExperiment(
    assays = list(counts = ddb_counts)
)

# All scater/scuttle/scran functions work
# (But BiocDuckDB provides optimized versions!)
```

### Large-Scale Matrix Operations

```r
# Correlation matrix (79x faster than HDF5Array!)
gene_cors <- cor(t(ddb_counts[1:1000, ]))

# Matrix multiplication
scores <- ddb_counts %*% gene_weights

# Cross-product
cov_mat <- crossprod(scale(ddb_counts))
```

## When to Use DuckDBArray

**Recommended for:**
- Single-cell count matrices (sparse, high-dimensional)
- Datasets too large for memory
- Matrix statistics and aggregations
- When you want HDF5Array simplicity with better performance
- Cloud-based workflows (Parquet on S3/GCS)

**Consider alternatives when:**
- Data fits comfortably in memory (use `dgCMatrix`)
- You need mutable arrays (use HDF5Array)
- You need array versioning (use TileDBArray)
- You need native 10x format support (use `TENxMatrix`)

## Documentation

- **[DuckDBArray Classes](vignettes/DuckDBArray-classes.Rmd)**: Architecture and class design
- **[DuckDBArray Comparison](vignettes/DuckDBArray-comparison.Rmd)**: Comprehensive benchmarks against HDF5Array and TileDBArray

## Installation

```r
# Requires DuckDBDataFrame
# install.packages("remotes")
remotes::install_github("your-org/DuckDBDataFrame")
remotes::install_github("your-org/DuckDBArray")
```

## Dependencies

DuckDBArray depends on:
- **DuckDBDataFrame**: Foundation for DuckDB-backed structures
- **Bioconductor**: DelayedArray, SparseArray, S4Arrays, MatrixGenerics, S4Vectors, IRanges
- **Sparse matrices**: Matrix
- **Data I/O**: arrow, dplyr

## Contributing

Contributions are welcome! Please:
- Report performance regressions through GitHub issues
- Include benchmarks for new matrix operations
- Follow Bioconductor standards

## License

DuckDBArray is licensed under the MIT License. See the LICENSE file for details.

## Acknowledgements

Special thanks to:
- The Bioconductor DelayedArray framework
- The matrixStats package for the comprehensive API
- The DuckDB team for query optimization
- The SparseArray team for sparse array infrastructure
