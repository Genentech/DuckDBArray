# DuckDBArray 0.99.13

## Bug fixes

- `.check_gram_size()` (`DuckDBMatrix-utils.R`), the size tripwire run before
  a `crossprod(x)`/`tcrossprod(x)` self-join, called `nzcount(x@seed)` to
  bound the self-join unconditionally. If `x` was itself the lazy,
  uncollected result of a prior `crossprod()`/`tcrossprod()` call (these
  deliberately return a still-lazy `DuckDBMatrix`), `nzcount()` re-executed
  that entire upstream join + GROUP BY, embedded as a subquery, just to
  answer a count, e.g. `G <- tcrossprod(pqmat); tcrossprod(G)` ran `pqmat`'s
  self-join twice. `.check_gram_size()` now first checks a matrix's shape
  (`prod(dim(x))`), which is always a valid, zero-query upper bound on its
  true nonzero count, and only falls back to the real `nzcount()` query when
  that cheap bound alone cannot already prove the operation is within
  budget. On a chained lazy Gram matrix built from a 600 x 100, 20%-dense
  fixture, this measures a 580x-840x median wall-clock speedup for the size
  check alone across repeated runs (15 reps each; see `test-DuckDBMatrix.R`);
  an earlier pass had reported 205x from a single run. Re-measurement (with
  the fixture rewritten under a density-aware `writeCoordArray()` block
  configuration, per the methodology note on the `.marginVars()` entry
  below) found the new path's cost is a pure shape/metadata check with no
  query at all (~0.14 ms, flat across every run and both block
  configurations), so the fix is structural (1 query -> 0 queries) rather
  than throughput-per-row and is insensitive to Parquet partition/row-group
  layout: writing the fixture under corrected vs. uncorrected block settings
  changed the measured speedup only from 813x to 839x, well within run-to-run
  noise (both writes landed in a single partition at this fixture size, so
  this particular case never actually exercised the block-shape distortion
  either way). What *did* move the number run-to-run was ambient DuckDB
  thread count / machine load, which changes how expensive the eliminated
  old-path self-join happens to be (580x-840x across sessions with the new
  path pinned at ~0.14 ms throughout); 205x sits below that range but is the
  same phenomenon, not evidence of a smaller real effect.

## Performance

- `.duckdb_matmult_copy()` (`DuckDBMatrix-utils.R`), used by the `%*%`/
  `crossprod(x, v)`-style matrix-vector product pushdown to register the
  vector operand as a temp table before joining it against the underlying
  data, called `copy_to()` with its defaults (`analyze = TRUE,
  in_transaction = TRUE`), adding an unconditional BEGIN/CREATE TEMP TABLE/
  ANALYZE/COMMIT sequence for a table that is joined exactly once and
  immediately discarded. It now passes `analyze = FALSE, in_transaction =
  FALSE` (both plain pass-through arguments `copy_to()`/
  `db_copy_to.DBIConnection()` already accept, not a new dependency). On the
  fixture sizes measured (a 500 x 400 and a 20000 x 5000 sparse matrix),
  this overhead was a small fraction of total `copy_to()` cost (measured
  1.00-1.04x), so this is primarily a hygiene fix (removing DDL/transaction
  overhead that buys nothing for a single-use table) rather than a
  measurable win at these scales; the benefit should grow with call
  frequency and under lock contention from concurrent matrix-vector
  products, which were not measured here. `dbplyr::copy_inline()` was
  considered as an alternative (avoiding the round trip entirely) and
  rejected per an earlier evaluation: DuckDB's SQL parser re-parsing
  inlined literals was found to cost more than its native zero-copy
  dataframe registration via `copy_to()`, making it slower overall despite
  fewer round trips.
- `.marginVars()`'s `fill != 0` branch (`DuckDBTable-matrixStats.R`, used by
  `rowVars`/`colVars`/`rowSds`/`colSds` on a non-zero-filled `DuckDBArray`/
  `DuckDBMatrix`) was a genuine two-pass query: a `group_by`/`summarize` to
  compute each group's mean, a `left_join` back to the underlying data, and a
  second `group_by`/`summarize` on squared deviations, scanning the
  underlying data twice. It has been generalized to a single query, reusing
  the same `n = COUNT(*)`, `sum_y = SUM(y)`, `S2 = COALESCE(VAR_SAMP(y), 0)`
  native aggregates already used by the (already single-pass) `fill == 0`
  case, and folding in the implicit fill-valued elements analytically via
  the law of total variance. `VAR_SAMP` (DuckDB's numerically stable
  aggregate) is still the sole source of the stored-value variance; no
  hand-rolled sum-of-squares is introduced. On a 2000 x 300, ~60%-dense
  fixture (`fill = 7`, `rowVars`), this measures a 1.6x-1.7x median
  wall-clock speedup over the old two-pass query (15-25 reps; see
  `test-DuckDBTable-matrixStats.R`); on a larger, more realistic single-cell-
  like 20000 x 5000, 2%-dense fixture, 1.8x-1.85x. An earlier pass had
  reported 1.57x from the smaller fixture alone.

## Documentation

- Documented a real, previously-unstated inconsistency: a `DuckDBArraySeed`'s
  `keycols` are implicitly expected to uniquely identify each row (each
  `(i, j)` key combination appears at most once), but this was not written
  down anywhere, and the invariant is not enforced at construction time. If
  it is violated, every materialization path behaves differently: the
  `dgCMatrix` coercion (`DuckDBArray-class.R`'s
  `setAs(..., "dgCMatrix", ...)`, via `Matrix::sparseMatrix(..., repr =
  "C")`) silently SUMS duplicate `(i, j)` entries, while `as.matrix()`,
  `as(x, "COO_SparseArray")`, and `as(x, "SparseArray")` silently keep the
  LAST one. Added a "Keycol uniqueness" section to `?DuckDBArray` and
  `?DuckDBArraySeed`, plus comments at `.collectCOO()` and the `dgCMatrix`
  coercion itself. This is documentation only: `sparseMatrix(...,
  use.last.ij = TRUE)` (to make the `dgCMatrix` path match the others) was
  measured 7-10x slower on the normal, invariant-holding case, and an eager
  duplicate-key guard was measured to cost 159% of the construction it would
  protect; both are an unacceptable cost to guard against a case that is
  supposed to be structurally impossible.

# DuckDBArray 0.99.12

## Bug fixes

- `rowDeviances(x, family = "poisson")`'s sparse path (`.poissonDevianceBlock`,
  `rowDeviances.R`) computed `v * log(v / sz[j])` directly on a `dgCMatrix`'s
  stored values with no zero guard, unlike the dense path three lines above
  it (`ratio[x == 0] <- 1  # so log(1) = 0`). A `dgCMatrix` is not required to
  be `drop0()`'d; an explicitly stored zero hit this unguarded branch as
  `log(0) = -Inf`, so `0 * -Inf = NaN`, which then propagated through
  `rowSums` into that gene's deviance and was silently zeroed by the
  function's own `dev[is.na(dev)] <- 0` cleanup, reporting a plausible-looking
  `0` instead of the correct value. Found while checking BiocDuckDB against
  an independently discovered deviance bug of the same shape in an unrelated
  codebase (stored zeros/saturated counts silently returning 0 instead of the
  correct likelihood). The production `DuckDBMatrix` method was never
  affected: its generated SQL already guards both families with
  `if (value > 0) ... else 0`, so it never evaluates `log(0)`. Only an
  in-memory `dgCMatrix` with an explicitly stored zero (never produced by
  `as(x, "dgCMatrix")` from a dense matrix, but not excluded by the class
  itself) could trigger this. A swept check of every other sparse/dense
  statistical computation in the BiocDuckDB suite (`scran.R`, `scuttle.R`,
  the matrix-stats files) found no other instance of this pattern; it is an
  isolated case, not a systemic one, since `rowDeviances.R` is the only file
  in the suite that does manual arithmetic on a `dgCMatrix`'s stored-value
  slot rather than pushing the computation down as SQL.

# DuckDBArray 0.99.11

## Bug fixes

- `Ops(DuckDBArray, atomic)`/`Ops(atomic, DuckDBArray)` (`DuckDBArray-utils.R`)
  only ever supported a length-1 `atomic` operand; anything longer fell
  through to `Ops(DuckDBTable, atomic)`'s scalar-only check and errored with
  "can only perform binary operations with a scalar value". This was latent
  but harmless as long as every caller realized a `%*%`/`crossprod` result
  (itself a lazy `DuckDBMatrix` by design, see 0.99.9) before combining it
  with a plain vector. CRAN's `irlba` 2.4.1 (2026-10-05) rewrote its Lanczos
  loop and, while doing so, dropped the `drop()` call that used to wrap
  `mult()`'s result before the subsequent `F <- F - sub` centering step; for
  ordinary dense/sparse matrices this is invisible, since `%*%` already
  returns a realized object, but it left `F` as a lazy `DuckDBMatrix` for
  `DuckDBMatrix` input, so centered SVDs (`BiocSingular::runSVD(center =
  TRUE)`, and anything built on it: `scran::fixedPCA()`,
  `scater::runPCA()`) started failing. Found via BiocDuckDB's
  `DuckDBIrlbaParam`/SVD composition tests immediately after the 2.4.1
  release. Fixed by having these two `Ops` methods realize `e1`/`e2` via
  `as.array()` and hand off to base R whenever the atomic operand isn't a
  true scalar, since elementwise arithmetic between two plain vectors is
  never something base R itself defers.

# DuckDBArray 0.99.10

## Documentation

- Documented `writeCoordArray()`'s partition sizing for large or sparse
  arrays: the `grid = NULL` default (`defaultAutoGrid(COO_SparseArray(dim(x)))`)
  budgets partitions from `dim(x)` alone, with no sparsity awareness; the
  density correction via `setAutoBlockSize()`/`setAutoBlockShape("scale")`;
  and the hard ceiling on `block.length` (`.Machine$integer.max`) beyond
  which no such tuning can reach a given partition-count target.
  `createDimTables()` cross-references this. Added a matching vignette
  section with a worked example.

# DuckDBArray 0.99.9

## Bug fixes

- `%*%`/`crossprod`/`tcrossprod`'s vector-multiply helpers
  (`.matmult_DuckDBMatrix_vector`/`.matmult_vector_DuckDBMatrix`,
  `DuckDBMatrix-utils.R`) named their `left_join(..., copy = TRUE)` temp
  table via `dplyr`'s default, `dbplyr::unique_table_name()`, which draws its
  "random" 10-character suffix from R's **global RNG stream**, not an
  independent source. Under a caller-seeded RNG, e.g. any reproducible
  analysis that pins `set.seed()` before an iterative solver like `irlba`
  (needed for deterministic SVD results, since `irlba` also draws its
  starting vector from the global RNG when none is supplied), repeated
  `%*%` calls within one Lanczos iteration, or separate calls across a
  session that reset to the same seed, could regenerate the exact same
  "random" table name and collide with a still-live temp table: none of
  these helpers ever drop theirs (matching every other lazy pushdown in this
  file, the result stays an un-executed lazy query, so dropping the source
  temp table on function return would break it before anyone materializes
  the result). Found via BiocDuckDB's SVD composition tests, which pin
  `set.seed()` for reproducibility and hit exactly this collision under a
  full test run. Fixed by copying the small in-memory vector explicitly with
  a monotonic, RNG-independent name (`.duckdb_matmult_copy()`) instead of
  relying on `copy = TRUE`'s naming.

# DuckDBArray 0.99.8

## Bug fixes

- Constructing a `DuckDBArray`/`DuckDBArraySeed`/`DuckDBMatrix` over a
  genuinely `BIGINT`/`HUGEINT`-typed value column no longer crashes with
  `vector: cannot make a vector of mode 'integer64'`. `coltypes()` reports
  `"integer64"` for such columns, and 9 call sites across
  `DuckDBArraySeed-class.R` / `DuckDBArraySeed-matrixStats.R` built the
  seed's structural fill value via `vector(coltypes(...), 1L)`, which errors
  because `"integer64"` is a `bit64` package class name, not a valid
  `vector()` mode. Replaced with a shared `.coltypeFillVector()` helper.
- Fixed a second, related bug this surfaced: `as.matrix()` /
  `extract_array()` on a successfully-constructed `integer64`-valued
  `DuckDBArray` (the standard construction path opens its DuckDB connection
  with `bigint = "integer64"`, see `acquireDuckDBConn()`) silently returned
  garbage values (e.g. `4.94e-324` instead of `1`) rather than erroring.
  `array()` and `[<-` are class-blind and operate on the underlying
  `double` storage only, so they never invoke `bit64`'s
  `as.double.integer64()` conversion; a genuinely `integer64`-classed value
  copied into a plain array reinterprets its raw 64-bit pattern as a double
  instead of converting it. `rowSums()` / `rowCounts()` were unaffected
  (computed via SQL, not R-side materialization). `DuckDBArray` already
  treats `integer64` as double-precision fidelity everywhere else, so
  `extract_array()`, `extract_sparse_array()`, and the COO fast path
  (`.collectCOO()`) now normalize any `integer64`-classed datacol value to
  plain `double` at the point it's materialized from the database, via a
  new `.dropInteger64()` helper, instead of trying to keep a real
  `bit64`-classed value alive through `array()`/`[<-`/`COO_SparseArray()`.
- The `DuckDBArraySeed` `fill` slot is now `"ANY"` (was `"atomic"`), with
  validity enforced via `is.atomic()` (`TRUE` for `integer64` values too)
  rather than the slot's formal class, so a genuine `integer64` fill value
  is accepted.

## Documentation

- `writeCoordArray()`'s `arrowtype` and `max_dim` docs now correctly
  describe the `DuckDBArray` fast-path method: unlike the `ANY` method,
  which infers the narrowest type from the data (or from `dim(x)` for
  index columns) when these are `NULL`, the `DuckDBArray` method does not
  materialize values to compute this; it takes the source's existing
  declared column type as-is. Pass `arrowtype`/`max_dim` explicitly to
  narrow the on-disk type for a `DuckDBArray` source.

# DuckDBArray 0.99.7

## New features

- Added `scanKeycols(path, keycols)`, a convenience function that scans a
  COO Parquet file/dataset for the distinct, sorted values of the named key
  columns and returns them already in the list shape expected by the
  `keycols` argument of `DuckDBArray()`/`DuckDBMatrix()`. Introduced in
  response to Bioconductor review feedback on the introspection example in
  the vignette, which required repeating a manual
  `distinct()`/`arrange()`/`collect()` scan by hand for each key column; the
  sort is pushed down into the query plan rather than done in R after
  collecting. The vignette's "Introspecting a Parquet file you did not
  write" section now uses it.

## Testing

- Added `skip_if_not_installed("sparseMatrixStats")` to the `rowNnzs()`
  tests that exercise a `dgCMatrix` (`test-rowNnzs.R`), which otherwise
  error rather than skip when `sparseMatrixStats`/`DelayedMatrixStats`
  aren't installed, since `MatrixGenerics::rowCounts()` on a `dgCMatrix`
  has no method without one of them.

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
