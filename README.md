# DuckDBArray

*A DuckDB backend for DelayedArray — out-of-core arrays and matrices, queried as
columnar Parquet.*

## Overview

`DuckDBArray` provides DuckDB-backed implementations of Bioconductor's
`DelayedArray` / `DelayedMatrix`, so a large array can live **on disk** behind the
ordinary R array API. Data is stored as coordinate (COO) **Parquet** and queried
lazily through [DuckDB](https://duckdb.org): `dim()`, `[`, arithmetic, and the
`MatrixGenerics` summaries (`rowSums`, `rowVars`, …) all work without loading the
matrix into memory. Row/column reductions are pushed down into SQL aggregations, so
DuckDB does the scan and the arithmetic and only the (small) per-margin result
returns to R.

It is part of the **BiocDuckDB** suite and builds on `DuckDBDataFrame` (the
tabular/SQL foundation); it slots in alongside `HDF5Array` and `TileDBArray` as
another `DelayedArray` backend.

## Installation

```r
# once available from Bioconductor:
if (!require("BiocManager")) install.packages("BiocManager")
BiocManager::install("DuckDBArray")
```

`DuckDBArray` requires `DuckDBDataFrame`; both are part of the BiocDuckDB suite.

## Quick start

A `DuckDBMatrix` is backed by a Parquet file in coordinate form. Write a matrix with
`writeCoordArray()`, add dimension tables with `createDimTables()` for pruning, and
open it as a lazy, disk-backed matrix:

```r
library(DuckDBArray)
library(Matrix)
library(MatrixGenerics)

m <- Matrix(rpois(200 * 50, lambda = 1), nrow = 200, ncol = 50, sparse = TRUE)
rownames(m) <- paste0("Gene", seq_len(nrow(m)))
colnames(m) <- paste0("Cell", seq_len(ncol(m)))

path <- tempfile()
writeCoordArray(m, path)
mat <- DuckDBMatrix(path, datacol = "value",
                    keycols = list(index1 = setNames(seq_len(nrow(m)), rownames(m)),
                                   index2 = setNames(seq_len(ncol(m)), colnames(m))),
                    dimtbls = createDimTables(m))

mat
rowSums(mat)[1:5]
rowVars(mat)[1:5]
```

Two count-oriented helpers are added for single-cell feature selection:
`rowNnzs()` / `colNnzs()` (non-zero counts) and `rowDeviances()` (binomial/Poisson
deviance). `DuckDBArray()` handles arrays of any dimension; a `DuckDBMatrix` is the
2-D case.

## Performance

On the 10x Genomics 1.3M brain-cell dataset (200,000-cell subset, 16 cores), DuckDB
**matches or beats an in-memory `dgCMatrix` while staying on disk** — e.g. `rowVars`
in ~1 s (vs ~114 s for `HDF5Array` at its best-effort 8-worker configuration), and
`rowDeviances` about **20× faster than the in-memory** sparse matrix by computing the
deviance directly in SQL. Just as important, DuckDB reaches this by **autotuning its
own parallelism**, whereas the other on-disk backends need manual finesse
(`SnowParam` workers, per-worker contexts, thread budgeting, block-size tuning) and
still scale unevenly.

The **Benchmarking DuckDBArray** vignette has the full picture: both single-threaded
and best-effort-parallel regimes, each backend configured at its best, with the
exact configuration recorded alongside the numbers. Reproduce or extend it with the
scripts in [`inst/scripts/`](inst/scripts).

## Documentation

- **Introduction to DuckDBArray** — motivation, construction, and the common
  operations (`vignettes/DuckDBArray.Rmd`).
- **Benchmarking DuckDBArray** — a fair comparison against in-memory, `HDF5Array`,
  and `TileDBArray` (`vignettes/DuckDBArray-comparison.Rmd`).
- **Implementing the DuckDBArray backend** — the `DelayedArray` seed contract and SQL
  translation, for developers (`vignettes/DuckDBArray-backend.Rmd`).

## When to use DuckDBArray

A good fit when the matrix is larger than memory (or you want to keep memory free),
when the data is sparse (single-cell counts, scATAC accessibility), when the workload
is dominated by columnar aggregations, or when the data already lives on disk as
Parquet that other tools should read. An in-memory `dgCMatrix` remains the fastest
choice when the data fits comfortably in RAM, and dense linear-algebra or heavy
random-access workloads may favor other backends. For higher-level single-cell
analysis (QC, normalization, variance modelling, marker detection) built on this
backend, see the **BiocDuckDB** package.

## License

Apache License 2.0. Copyright Genentech, Inc.
