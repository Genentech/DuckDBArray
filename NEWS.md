# DuckDBArray 0.99.6

## Bug fixes

- Removed two duplicate `switch()` case labels ("integer", "double") in
  `.duckdbTypeToArrow()` (`writeCoordArray-types.R`); R's `switch()` only ever
  matches the first occurrence of a label, so the later entries were dead code.
  Both duplicates mapped to the same value as their first occurrence, so this
  is not a behavior change.
- Removed the unused internal helper `.buildTempTableFilter()`.
- `sapply()` calls in `R/DuckDBTable-matrixStats.R` are replaced with
  `lapply()` (where the result's names already came from a named input) or
  `setNames(lapply(...), ...)` (where `sapply()`'s auto-naming of an unnamed
  character-vector input was load-bearing), resolving the corresponding
  `BiocCheck` NOTE with no behavior change.
- The multi-line `paste0()`-built format string in `.check_gram_size()`'s
  error message (`R/DuckDBMatrix-utils.R`) is replaced with two `sprintf()`/
  literal arguments passed directly to `stop()`, which concatenates its `...`
  arguments natively; this resolves the "avoid paste in condition signals"
  NOTE and produces byte-identical message text (verified).

## Documentation

- Benchmarking vignette: the `rowVars()` comparison against a plain
  `dgCMatrix` (which dispatches through `MatrixGenerics` to a method supplied
  by `sparseMatrixStats` or `DelayedMatrixStats`) is now gated on one of those
  optional packages being installed, so the vignette no longer errors when
  neither is available. `rowDeviances()`, DuckDBArray's own generic, is
  unaffected and always runs.
- Introduction vignette: added a section on introspecting a Parquet file's
  columns and key-column value ranges with `arrow::open_dataset()` before
  constructing a `DuckDBArray`/`DuckDBMatrix`, for the case where the file was
  produced by another tool and the key structure isn't already known.

## Testing

- Loading the `airway` Suggests package in `tests/testthat/setup.R` is now
  conditional, and the `test_that()` blocks that use `airway_counts` /
  `airway_counts_path` call `skip_if_not_installed("airway")`. Previously the
  unconditional `data(airway, ...)` call in `setup.R` would fail and abort the
  entire test suite (not just the airway-dependent tests) when `airway` was
  not installed.

# DuckDBArray 0.99.5

## Documentation

- Replaced em dashes with commas or colons in the vignettes.

# DuckDBArray 0.99.4

## Documentation

- Added `URL` and `BugReports` fields to DESCRIPTION.
- Added a package-level man page (`?DuckDBArray`).
- Removed the redundant `library(BiocStyle)` call from the vignettes (the
  `BiocStyle::html_document` output and `::`-qualified helpers apply the style
  without loading the package). Applied suite-wide for consistency with the
  Bioconductor review of DuckDBDataFrame.

# DuckDBArray 0.99.3

## New features

- `writeCoordArray()` / `writeParquet()` for `DuckDBArray` gain a `cluster_by`
  argument: an optional within-partition row ordering (a
  `DuckDBDataFrame::zorder()` / `hilbert()` spec, or a character vector of index
  columns) lowered to the `COPY TO` `ORDER BY` via
  `DuckDBDataFrame::clusterOrderSQL()`. Clustering the COO on a chosen axis (e.g.
  `cluster_by = "<sample index>"`) tightens that axis's row-group zonemaps so a
  range or point query on it prunes row groups, a faithful reorder (the COO is
  order-agnostic on read; no schema, contract, or reader change). Default `NULL`
  keeps the existing index ordering, so output is byte-identical and existing
  goldens are unaffected. Reuses DuckDBDataFrame's single Morton/Hilbert
  generator rather than re-deriving it.

# DuckDBArray 0.99.2

## Bug fixes

- The DuckDB fast-path append now writes new parts into an existing
  hive-partitioned dataset (via the DuckDB `APPEND` copy option) instead of
  failing with "Directory ... is not empty". A second `writeCoordArray(...,
  append = TRUE)` slab is preserved alongside the first, and the combined
  dataset round-trips; previously the fast path supported only a fresh write.

# DuckDBArray 0.99.1

## Bug fixes

- The DuckDB fast write path now types the coordinate-index columns from the
  pre-computed, `max_dim`-aware `idxtypes` (`.buildIndexMappings()`) instead of
  inferring them from the remapped indices. Previously a `> 2^31` along-axis
  offset made the remapped indices a double, which inferred `float64` and
  degraded to a DuckDB `INTEGER` temp column that overflowed; and an append part
  could narrow differently from part 0. The index type now matches the R write
  path and is consistent across parts.

- `writeCoordArray()` accepts a whole-number `max_dim` beyond the 32-bit integer
  range (a coordinate axis larger than ~2.1e9). The validator previously coerced
  `max_dim` via `as.integer()`, overflowing such a dimension to `NA`; it now
  validates integrality without coercion and keeps within-32-bit dims as integer.

# DuckDBArray 0.9.20

## New features

- Fast one-query coercions to concrete sparse-matrix targets:
  `as(x, "dgCMatrix")` / `as(x, "CsparseMatrix")` / `as(x, "COO_SparseMatrix")` /
  `as(x, "COO_SparseArray")` on a `DuckDBArray` / `DuckDBMatrix` now fetch the
  whole COO in a single query and build the target directly, skipping the
  `COO_SparseArray -> SVT_SparseArray` round-trip the default DelayedArray
  coercions pay for (measured ~1.3-1.7x on a cell x gene realize, byte-identical
  to the default). They fall back to the default path when the array is not
  zero-filled, dropped, the wrong rank, or (for `dgCMatrix`) non-numeric, so
  correctness is never traded for speed. Dense `as.matrix` / `as.array` stay on
  the existing (already single-query) path: a direct dense build measured only
  ~1.1x, not worth a separate code path.

## Changes

- The test harness pins DuckDB to a single thread
  (`options(DuckDBDataFrame.threads = 1L)` in `setup.R`) so parallel
  floating-point reductions (`sum` / `var_samp`) accumulate in a fixed order and
  tight-tolerance expectations stay reproducible run-to-run. This is a
  test-only change; real sessions use all cores.

# DuckDBArray 0.9.19

## Changes

- Relicensed under the MIT License.

# DuckDBArray 0.9.18

## Bug fixes

- `crossprod()` and `tcrossprod()` on a `DuckDBMatrix` (the SQL self-join form,
  with no second operand) now raise a clear error before the self-join can
  exhaust memory, instead of silently OOMing on a large matrix. The self-join
  emits roughly `nnz^2 / contracted_dim` intermediate row-pairs before the
  `GROUP BY` can reduce them — quadratic and un-spillable at scale — so a size
  estimate now trips a `stop()` with guidance (subset to highly variable genes,
  or materialize with `as.matrix()`) above a safeguard of 5e8 pairs. The
  threshold is overridable with `options(DuckDBArray.gram_pair_limit = )` on a
  machine with more memory.

# DuckDBArray 0.9.17

## Bug fixes

- The `DuckDBTable` margin statistics (`rowSums`/`colSums`, `rowMeans`/`colMeans`,
  `rowVars`/`colVars`, `rowSds`/`colSds`, `rowMaxs`/`colMaxs`, `rowMins`/`colMins`,
  `rowCounts`/`colCounts`) now honor their `na.rm` argument. Previously `na.rm`
  was accepted but ignored — the statistic always dropped `NA`/`NULL`, diverging
  from the `MatrixGenerics` default of `na.rm = FALSE`. Because SQL aggregate
  functions inherently drop `NULL`, `na.rm = FALSE` is emulated with a per-group
  guard: a group containing any `NULL` yields `NA`. The common `na.rm = TRUE`
  path (and any group without `NULL`s) is unchanged.

# DuckDBArray 0.9.16

## Documentation

- Restructured the vignettes into a user-first set:
  *Introduction to DuckDBArray* (overview and usage),
  *Benchmarking DuckDBArray* (a best-effort comparison against in-memory,
  HDF5Array, and TileDBArray), and
  *Implementing the DuckDBArray backend* (the DelayedArray seed contract and
  SQL translation, for developers).
- The benchmarking vignette renders precomputed results (produced offline by
  `inst/scripts/run_vignette_benchmarks.R`) for both a single-threaded and a
  best-effort-parallel regime, with each backend configured at its best and the
  configuration recorded alongside the numbers.
- Rewrote the README.

## Internal changes

- Added `\value` sections to all exported-object man pages.
- Manual-page examples build objects via their constructors/accessors instead of
  accessing S4 slots with `@`.
