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
