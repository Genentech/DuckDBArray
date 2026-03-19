#' Write an Array-Like Object in Coordinate Format
#'
#' @description
#' An \code{arrow::write_dataset} wrapper function to write array-like objects
#' in coordinate format. If dimension names are present, they are substituted
#' for the corresponding indices.
#'
#' @param x An array-like object.
#' @param path The path to write the array-like object to.
#' @param indexcols A character vector of column names for the dimensions of the
#' array.
#' @param datacol A character string specifying the column name containing the
#' array values in the resulting table. Defaults to "value".
#' @param grid An optional \link[S4Arrays]{ArrayGrid} to use for partitioning
#' the array. If \code{NULL}, uses
#' \code{defaultAutoGrid(COO_SparseArray(dim(x)))}.
#' @param grid_suffix A character string to append to the partitioning
#' directories if the grid contains more than one cell. Defaults to "_group".
#' @param BPPARAM A \link[BiocParallel]{BiocParallelParam} object to use for
#' parallel processing when \code{grid} contains multiple cells. Defaults to
#' \code{getAutoBPPARAM()}.
#' @param ... Additional arguments to pass to \code{arrow::write_dataset}.
#'
#' @author Patrick Aboyoun
#'
#' @examples
#' # Write the state.x77 matrix to multiple Parquet files using grid partitioning
#' tf <- tempfile()
#' state_grid <- RegularArrayGrid(dim(state.x77), c(10, 4))
#' writeCoordArray(state.x77, file.path(tf, "state"), grid = state_grid)
#' list.files(tf, full.names = TRUE, recursive = TRUE)
#'
#' @keywords IO
#'
#' @export
#' @importFrom DelayedArray blockApply currentViewport defaultAutoGrid
#' @importFrom DelayedArray effectiveGrid getAutoBPPARAM
#' @importFrom S4Arrays mapToGrid
#' @importFrom S4Vectors head tail
#' @importFrom SparseArray COO_SparseArray nzvals
#' @name writeCoordArray
writeCoordArray <-
function(x,
         path,
         indexcols = names(dimnames(x)) %||% sprintf("index%d", seq_along(dim(x))),
         datacol = "value",
         grid = defaultAutoGrid(COO_SparseArray(dim(x))),
         grid_suffix = "_group",
         BPPARAM = getAutoBPPARAM(),
         ...)
{
    if (is.null(dim(x))) {
        stop("the default method of writeParquet requires 'x' to be array-like")
    }

    if (inherits(x, "table")) {
        x <- unclass(x)
    }

    # Make column names unique
    unique_names <- make.unique(c(indexcols, datacol), sep = "_")
    indexcols <- head(unique_names, -1L)
    datacol <- tail(unique_names, 1L)

    # Get dimensions of the array for storage optimization
    dim_x <- dim(x)

    # Manage dimnames
    dimnames_x <- dimnames(x) %||% lapply(dim(x), function(d) NULL)
    dimnames(x) <- lapply(dim(x), function(d) as.character(seq_len(d)))

    arrowtype <- NULL
    if (length(grid) == 1L) {
        vals <- c(0L, nzvals(x))
        if ((is.integer(vals) ||
             (is.numeric(vals) && all(vals == floor(vals), na.rm = TRUE)))) {
            arrowtype <- .arrowIntType(range(vals, na.rm = TRUE))
        }

        .writeCoordArray(x, path = path, indexcols = indexcols,
                         datacol = datacol, dim_x = dim_x,
                         arrowtype = arrowtype, ...)
    } else {
        ranges <- try(
            blockApply(x,
                       FUN = function(block) {
                           vals <- c(0L, nzvals(block))
                           if (!(is.integer(vals) ||
                                 (is.numeric(vals) &&
                                  all(vals == floor(vals), na.rm = TRUE)))) {
                               stop("not an integer array")
                           }
                           range(vals, na.rm = TRUE)
                       },
                       grid = grid,
                       as.sparse = TRUE,
                       BPPARAM = BPPARAM,
                       verbose = NA),
            silent = TRUE
        )

        if (!inherits(ranges, "try-error")) {
            min_x <- min(sapply(ranges, `[`, 1L))
            max_x <- max(sapply(ranges, `[`, 2L))
            arrowtype <- .arrowIntType(c(min_x, max_x))
        }

        FUN <- function(x, path, indexcols, datacol, grid_suffix, dim_x,
                        arrowtype, ...)
        {
            # Append subdirectories to path
            grid <- effectiveGrid()
            viewport <- currentViewport()
            group <- as.vector(mapToGrid(start(viewport), grid)[["major"]])
            subdir <- paste0(indexcols, grid_suffix, "=", group)
            path <- do.call(file.path, c(list(path), subdir))

            .writeCoordArray(x, path = path, indexcols = indexcols,
                             datacol = datacol, dim_x = dim_x,
                             arrowtype = arrowtype, ...)
        }

        blockApply(x, FUN = FUN,
                   path = path,
                   indexcols = indexcols,
                   datacol = datacol,
                   dim_x = dim_x,
                   arrowtype = arrowtype,
                   grid_suffix = grid_suffix,
                   ...,
                   grid = grid,
                   as.sparse = TRUE,
                   BPPARAM = BPPARAM,
                   verbose = NA)
    }

    invisible(NULL)
}

#' @importFrom arrow Array write_dataset write_parquet
#' @importFrom SparseArray nzwhich nzvals
.writeCoordArray <- function(x, path, indexcols, datacol, dim_x, arrowtype, ...) {
    # Create a list of columns containing the non-zero values and their indices
    lst <- apply(nzwhich(x, arr.ind = TRUE), 2L, identity, simplify = FALSE)
    names(lst) <- indexcols
    lst[[datacol]] <- nzvals(x)

    # Map back to the original indices
    indices <- lapply(dimnames(x), as.integer)
    for (j in seq_along(indices)) {
        lst[[j]] <- indices[[j]][lst[[j]]]
    }

    # Use smallest unsigned integer type based on array dimensions
    for (j in seq_along(indexcols)) {
        type <- .arrowIntType(c(0L, dim_x[j]))
        lst[[j]] <- Array$create(lst[[j]], type = type)
    }

    # Apply pre-determined optimal integer type to data column
    if (!is.null(arrowtype)) {
        lst[[datacol]] <- as.integer(lst[[datacol]])
        lst[[datacol]] <- Array$create(lst[[datacol]], type = arrowtype)
    }

    # Convert to a data frame
    class(lst) <- "data.frame"
    attr(lst, "row.names") <- .set_row_names(length(lst[[1L]]))

    if (nrow(lst) == 0L) {
        if (!dir.exists(path)) {
            dir.create(path, recursive = TRUE)
        }
        write_parquet(lst, file.path(path, "part-0.parquet"),
                      compression = "zstd", compression_level = 3L, ...)
    } else {
        # Row group size tuning for DuckDB query performance:
        #
        # DuckDB processes data in vectors of 2048 rows (STANDARD_VECTOR_SIZE).
        # Row group sizes that are multiples of 2048 align with DuckDB's execution.
        #
        # Benchmarks on realistic sparse single-cell data (30K genes x 50K cells,
        # 75M non-zeros) showed:
        #
        # | min_rows_per_group | File Size | Single Gene Query |
        # |--------------------|-----------|-------------------|
        # |    122,880 (60x)   |  286.2 MB |       0.014 sec   |
        # |    245,760 (120x)  |  237.4 MB |       0.008 sec   |
        # |    491,520 (240x)  |  208.7 MB |       0.004 sec   | <- chosen
        # |    983,040 (480x)  |  194.2 MB |       0.004 sec   |
        # |  1,966,080 (960x)  |  193.6 MB |       0.004 sec   |
        #
        # 491,520 (240 vectors) provides:
        # - 27% smaller files vs DuckDB default (122,880)
        # - Fastest selective gene queries
        # - Good balance of compression and query performance
        #
        # See: duckdb/src/include/duckdb/storage/storage_info.hpp
        #      duckdb/src/include/duckdb/common/vector_size.hpp
        write_dataset(lst, path, format = "parquet", compression = "zstd",
                      compression_level = 3L, partitioning = NULL,
                      min_rows_per_group = 491520L, write_statistics = TRUE,
                      ...)
    }

    invisible(NULL)
}

#' @importFrom arrow uint8 uint16 uint32 uint64 int8 int16 int32 int64
.arrowIntType <- function(range_x) {
    min_x <- range_x[1L]
    max_x <- range_x[2L]
    if (min_x >= 0L) {
        if (max_x <= 255L) {
            uint8()
        } else if (max_x <= 65535L) {
            uint16()
        } else if (max_x <= 2147483647L) {
            int32()
        } else if (max_x <= 4294967295) {
            uint32()
        } else {
            int64()
        }
    } else {
        if (min_x >= -128L && max_x <= 127L) {
            int8()
        } else if (min_x >= -32768L && max_x <= 32767L) {
            int16()
        } else if (min_x >= -2147483648 && max_x <= 2147483647L) {
            int32()
        } else {
            int64()
        }
    }
}
