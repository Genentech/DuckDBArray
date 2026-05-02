### Nothing in this file is exported.
#
# Generic write path utilities for writeCoordArray,ANY method.
# These helpers support blockApply-based writes to Arrow/Parquet for
# array-like objects that don't have a specialized fast-path implementation.

# blockApply FUN used by writeCoordArray for multi-cell grids. Computes
# this cell's partition-group coordinates, builds the hive-style
# partition subdirectory, and hands the block to .writeCoordArray.
#' @importFrom DelayedArray currentViewport effectiveGrid
#' @importFrom S4Arrays mapToGrid
.writeCellFUN <- function(x, path, indexcols, datacol, grid_suffix,
                          idxtypes, arrowtype, along, offset,
                          group_offset, ...)
{
    grid <- effectiveGrid()
    viewport <- currentViewport()
    group <- as.vector(mapToGrid(start(viewport), grid)[["major"]])

    if (!is.null(along) && group_offset > 0L) {
        group[along] <- group[along] + group_offset
    }

    subdir <- paste0(indexcols, grid_suffix, "=", group)
    path <- do.call(file.path, c(list(path), subdir))

    .writeCoordArray(x, path = path, indexcols = indexcols,
                     datacol = datacol, idxtypes = idxtypes,
                     arrowtype = arrowtype, along = along,
                     offset = offset, ...)
}

# Write a single coordinate-format block to 'path'. Every call writes
# with the caller-supplied (or externally-inferred) arrow types
# verbatim: no per-block type inference happens here, so every partition
# of a multi-cell write and every successive append share an identical
# schema.
.writeCoordArray <-
function(x, path, indexcols, datacol, idxtypes, arrowtype = NULL,
         along = NULL, offset = 0L, ...)
{
    coords_df <- .extractSparseCoordinates(x, indexcols, datacol, along, offset)
    .writeArrowDataset(coords_df, path, indexcols, datacol, idxtypes, arrowtype, ...)
    invisible(NULL)
}

# Extract sparse coordinates from array-like object and build a data frame
# with index columns and value column. Handles coordinate remapping via
# dimnames and offset shifting for append mode.
#' @importFrom SparseArray nzwhich nzvals
.extractSparseCoordinates <-
function(x, indexcols, datacol, along = NULL, offset = 0L)
{
    lst <- asplit(nzwhich(x, arr.ind = TRUE), 2L)
    names(lst) <- indexcols
    lst[[datacol]] <- nzvals(x)

    # Map block-local row indices back to the original coordinates, then
    # shift 'along' coordinates by 'offset' in append mode.
    indices <- lapply(dimnames(x), as.integer)
    for (j in seq_along(indices)) {
        lst[[j]] <- indices[[j]][lst[[j]]]
    }
    if (!is.null(along) && offset > 0L) {
        lst[[along]] <- lst[[along]] + offset
    }

    lst
}

# Write coordinate data frame to Arrow/Parquet with type coercion and
# routing to either empty or non-empty write path.
#' @importFrom arrow Array
.writeArrowDataset <-
function(coords_df, path, indexcols, datacol, idxtypes, arrowtype = NULL, ...)
{
    # Coerce index columns to their caller-pinned arrow types. If any
    # coordinate overflows the chosen unsigned type (which shouldn't
    # happen when upstream validation is correct), arrow::Array$create
    # throws loudly -- that's the last line of defense against silently
    # writing wrong coordinates.
    for (j in seq_along(indexcols)) {
        coords_df[[j]] <- Array$create(coords_df[[j]], type = idxtypes[[j]])
    }
    if (!is.null(arrowtype)) {
        coords_df[[datacol]] <- Array$create(coords_df[[datacol]], type = arrowtype)
    }

    class(coords_df) <- "data.frame"
    attr(coords_df, "row.names") <- .set_row_names(length(coords_df[[1L]]))

    if (nrow(coords_df) == 0L) {
        .writeEmptyParquet(coords_df, path, ...)
    } else {
        .writeParquetDataset(coords_df, path, ...)
    }
}

# Write an empty coord-format parquet file (preserves the schema) at
# path/part-0.parquet. Refuses to overwrite an existing file.
#' @importFrom arrow write_parquet
.writeEmptyParquet <- function(lst, path, ...) {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    target <- file.path(path, "part-0.parquet")
    if (file.exists(target)) {
        stop("target file already exists: ", target,
             " (refusing to overwrite)")
    }
    write_parquet(lst, target,
                  compression = "zstd", compression_level = 3L, ...)
}

# Write a non-empty coord-format block as a parquet dataset under
# 'path'. Defaults 'existing_data_behavior' to "error" (overriding
# arrow's silent-clobber default) so a re-run into a populated path
# fails loudly unless the caller explicitly opts into overwrite
# semantics via '...'.
#
# Row-group size tuning notes for DuckDB query performance:
#
#   DuckDB processes data in vectors of 2048 rows (STANDARD_VECTOR_SIZE).
#   Row-group sizes that are multiples of 2048 align with DuckDB's
#   execution engine.
#
#   Benchmarks on realistic sparse single-cell data (30K genes x 50K
#   cells, 75M non-zeros):
#
#   | min_rows_per_group | File Size | Single Gene Query |
#   |--------------------|-----------|-------------------|
#   |    122,880 (60x)   |  286.2 MB |       0.014 sec   |
#   |    245,760 (120x)  |  237.4 MB |       0.008 sec   |
#   |    491,520 (240x)  |  208.7 MB |       0.004 sec   | <- chosen
#   |    983,040 (480x)  |  194.2 MB |       0.004 sec   |
#   |  1,966,080 (960x)  |  193.6 MB |       0.004 sec   |
#
#   491,520 (240 vectors) gives ~27% smaller files than DuckDB's default
#   and the fastest selective gene queries. See duckdb's
#   storage_info.hpp / vector_size.hpp.
#' @importFrom arrow write_dataset
.writeParquetDataset <- function(lst, path, ...) {
    args <- list(lst, path, format = "parquet", compression = "zstd",
                 compression_level = 3L, partitioning = NULL,
                 min_rows_per_group = 491520L,
                 write_statistics = TRUE, ...)
    if (!"existing_data_behavior" %in% names(args)) {
        args$existing_data_behavior <- "error"
    }
    do.call(write_dataset, args)
}
