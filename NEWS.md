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
