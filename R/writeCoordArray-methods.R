### Nothing in this file is exported except the public writeCoordArray generic and methods.
#
# Public S4 generic and methods for writing array-like objects in coordinate format.
# Includes argument validation helpers used by both methods.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validation helpers
###

### Validate arrow type
.validateArrowtype <- function(arrowtype) {
    if (!is.null(arrowtype) && !inherits(arrowtype, "DataType")) {
        stop("'arrowtype' must be NULL or an arrow DataType object ",
             "(see e.g. arrow::int32(), arrow::uint16())")
    }
}

# Validate the append-mode arguments as a group. Returns a named list
# (along, offset, group_offset) with each element coerced to integer when
# append = TRUE, or a "no-op" list with along = NULL / offset = 0L /
# group_offset = 0L when append = FALSE.
#' @importFrom S4Vectors isSingleNumber isTRUEorFALSE
#' @importFrom DuckDBDataFrame validateAppendOffset
.validateAppend <-
function(append, grid, along, offset, group_offset, path, ndim)
{
    if (!isTRUEorFALSE(append)) {
        stop("'append' must be a single logical (TRUE or FALSE)")
    }
    if (!append) {
        return(list(along = NULL, offset = 0L, group_offset = 0L))
    }

    if (length(grid) <= 1L) {
        stop("'append = TRUE' requires hive-partitioned output ",
             "(length(grid) > 1L)")
    }
    if (!isSingleNumber(along) || along != as.integer(along) ||
        along < 1L || along > ndim) {
        stop("'along' must be a single integer in 1:", ndim,
             " when 'append = TRUE'")
    }
    offset <- validateAppendOffset(offset)
    if (!isSingleNumber(group_offset) ||
        group_offset != as.integer(group_offset) ||
        group_offset < 0L) {
        stop("'group_offset' must be a single non-negative integer")
    }
    if (!dir.exists(path)) {
        stop("'append = TRUE' but target directory does not exist: ", path)
    }

    list(along = as.integer(along),
         offset = offset,
         group_offset = as.integer(group_offset))
}

# Validate 'max_dim'. Returns the coerced integer vector, or NULL when
# the caller did not supply one.
.validateMaxDim <-
function(max_dim, x, append, along, offset, ndim)
{
    if (is.null(max_dim)) {
        return(NULL)
    }
    if (!is.numeric(max_dim) || length(max_dim) != ndim ||
        anyNA(max_dim) ||
        !all(max_dim == as.integer(max_dim))) {
        stop("'max_dim' must be NULL or an integer vector of length ", ndim)
    }
    max_dim <- as.integer(max_dim)
    if (any(max_dim < dim(x))) {
        j <- which(max_dim < dim(x))[1L]
        stop("'max_dim[", j, "]' (", max_dim[j],
             ") must be >= dim(x)[", j, "] (", dim(x)[j], ")")
    }
    if (append && max_dim[along] < dim(x)[along] + offset) {
        stop("'max_dim[", along, "]' (", max_dim[along],
             ") must be >= dim(x)[", along, "] + offset (",
             dim(x)[along] + offset, ")")
    }
    max_dim
}

# Shared preamble for writeCoordArray methods: unique column names,
# append validation, max_dim validation, and CoordSchema preparation.
.setupCoordWrite <-
function(x, path, indexcols, datacol, grid, grid_suffix,
         arrowtype, max_dim, append, along, offset, group_offset,
         infer_value = TRUE, BPPARAM = NULL)
{
    if (is.null(dim(x))) {
        stop("the default method of writeCoordArray requires 'x' to be array-like")
    }
    if (inherits(x, "table")) {
        x <- unclass(x)
    }
    ndim <- length(dim(x))

    unique_names <- make.unique(c(indexcols, datacol), sep = "_")
    indexcols <- head(unique_names, -1L)
    datacol <- tail(unique_names, 1L)

    .validateArrowtype(arrowtype)

    ap <- .validateAppend(append, grid, along, offset, group_offset,
                          path, ndim)
    along <- ap$along
    offset <- ap$offset
    group_offset <- ap$group_offset

    max_dim <- .validateMaxDim(max_dim, x, append, along, offset, ndim)

    schema <- .prepareCoordSchema(
        x, indexcols, datacol, arrowtype, max_dim,
        append, along, offset, path, grid, BPPARAM,
        infer_value = infer_value
    )

    if (append) {
        checkHiveAppendPartitions(path = path, indexcols = indexcols,
                                  grid_suffix = grid_suffix, grid = grid,
                                  along = along, group_offset = group_offset)
    }

    list(x = x,
         indexcols = indexcols,
         datacol = datacol,
         schema = schema,
         along = along,
         offset = offset,
         group_offset = group_offset)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Public API
###

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
#' parallel processing when \code{grid} contains multiple cells (ANY method only).
#' Defaults to \code{getAutoBPPARAM()}. Note: The \code{DuckDBArray} method
#' does not support this parameter because DuckDB connections are not thread-safe
#' for concurrent execution. The \code{DuckDBArray} method uses sequential
#' execution and relies on DuckDB's internal parallelism for performance.
#' @param arrowtype An optional \link[arrow]{DataType} for the value column
#' (e.g. \code{arrow::int32()}, \code{arrow::uint16()},
#' \code{arrow::float64()}). When \code{NULL} and \code{append = FALSE}, the
#' type is inferred from the data: the narrowest type capable of representing
#' the range of non-zero values if they are all integer-valued, and
#' otherwise \code{double}. When \code{NULL} and \code{append = TRUE}, the
#' type is read from an existing parquet file under \code{path} so that the
#' appended slab always matches the existing value-column schema. When
#' supplied explicitly in append mode, it is verified to match the existing
#' schema; a mismatch aborts the call before any file is written.
#' @param max_dim Optional integer vector of length \code{length(dim(x))}
#' giving an upper bound on the coordinates in each dimension. Controls the
#' arrow type chosen for each index column: the narrowest unsigned integer
#' type that can represent \code{0..max_dim[j]} is picked for column
#' \code{indexcols[j]}. When \code{NULL} and \code{append = FALSE},
#' \code{dim(x)} is used. When \code{NULL} and \code{append = TRUE}, the
#' index-column types are read from an existing parquet file under
#' \code{path} so that the appended slab always matches the existing
#' index-column schema. When supplied explicitly in append mode, the
#' resulting types are verified to match the existing schema; a mismatch
#' aborts the call before any file is written. The caller does not need to
#' know the exact final extent, only an upper bound on it; setting
#' \code{max_dim} too high only costs a slightly wider integer type on
#' disk.
#' @param append Logical; when \code{TRUE}, write additional hive-style
#' partitions into an existing directory previously created by
#' \code{writeCoordArray}. Append mode is only supported for hive-partitioned
#' output (i.e. \code{length(grid) > 1L}); each cell of the supplied
#' \code{grid} produces a new partition directory along dimension
#' \code{along}. Before writing, \code{writeCoordArray} performs several
#' corruption-prevention checks against the existing dataset:
#' \itemize{
#'   \item \code{path} must already exist and be laid out as a hive
#'   partitioning (top-level entries must all be
#'   \code{paste0(indexcols[1], grid_suffix, "=", <integer>)} subdirectories).
#'   \item At least one parquet file must exist under \code{path}, and its
#'   schema must contain every \code{indexcols[j]} and \code{datacol}.
#'   \item The value-column and every index-column arrow type must match
#'   the existing schema. If \code{arrowtype}/\code{max_dim} were supplied,
#'   a mismatch errors; if not, the existing types are adopted.
#'   \item No partition directory that this call would write to may already
#'   exist (otherwise \code{group_offset} is too small and the call would
#'   silently overwrite previously-written data).
#' }
#' Because arrow's dataset reader requires a uniform schema across files,
#' these checks make silent schema drift and silent partition collisions
#' impossible. Defaults to \code{FALSE}.
#' @param along Integer index of the dimension along which to append new
#' partitions. Required when \code{append = TRUE}.
#' @param offset Non-negative integer added to the coordinates in dimension
#' \code{along} for every non-zero written in this call. Typically the total
#' extent already written along \code{along} by previous calls. Defaults to
#' \code{0L}.
#' @param group_offset Non-negative integer added to the partition group
#' index along dimension \code{along} (i.e. the value embedded in the
#' \code{paste0(indexcols[along], grid_suffix)} subdirectory) so that new
#' partition directories do not collide with existing ones. Typically the
#' total number of partition groups already written along \code{along} by
#' previous calls. Defaults to \code{0L}.
#' @param ... Additional arguments to pass to \code{arrow::write_dataset}.
#' Note that \code{existing_data_behavior} defaults to \code{"error"} (rather
#' than arrow's default of \code{"overwrite_or_ignore"}) so that a re-run
#' into a populated \code{path} fails loudly instead of silently clobbering
#' or merging existing data. Pass \code{existing_data_behavior = "delete_matching"}
#' or similar to override.
#'
#' @author Patrick Aboyoun
#'
#' @return Called for its side effect of writing \code{x} to \code{path} as
#' coordinate-format Parquet; returns \code{NULL} invisibly.
#'
#' @examples
#' # Write the state.x77 matrix to multiple Parquet files using grid partitioning
#' tf <- tempfile()
#' state_grid <- RegularArrayGrid(dim(state.x77), c(10, 4))
#' writeCoordArray(state.x77, file.path(tf, "state"), grid = state_grid)
#' list.files(tf, full.names = TRUE, recursive = TRUE)
#'
#' # Append mode: write two column-slabs as distinct hive partitions without
#' # first materializing cbind(slab1, slab2). Omitting 'arrowtype' and
#' # 'max_dim' is safe -- both are read from the existing schema so the
#' # appended slab is guaranteed to match.
#' tf2 <- tempfile()
#' slab1 <- state.x77[, 1:4]
#' slab2 <- state.x77[, 5:8]
#' grid1 <- RegularArrayGrid(dim(slab1), c(10L, 2L))  # 2 col partitions
#' grid2 <- RegularArrayGrid(dim(slab2), c(10L, 2L))  # 2 more col partitions
#' writeCoordArray(slab1, file.path(tf2, "state"), grid = grid1)
#' writeCoordArray(slab2, file.path(tf2, "state"), grid = grid2,
#'                 append = TRUE, along = 2L,
#'                 offset = ncol(slab1), group_offset = dim(grid1)[2L])
#'
#' # Pinning 'arrowtype' / 'max_dim' is still useful when the first slab's
#' # inferred schema is narrower than you want the final dataset to have
#' # (e.g. a small first slab that picks uint8 but you know later slabs
#' # will need uint16). Pin both on every call for a fully-predictable
#' # dataset schema.
#' tf3 <- tempfile()
#' int_slab   <- state.x77[, c("Population", "Income")]
#' float_slab <- state.x77[, c("Illiteracy", "Life Exp")]
#' grid_i <- RegularArrayGrid(dim(int_slab),   c(10L, 2L))
#' grid_f <- RegularArrayGrid(dim(float_slab), c(10L, 2L))
#' max_dim <- c(nrow(state.x77), ncol(state.x77))
#' writeCoordArray(int_slab, file.path(tf3, "state"), grid = grid_i,
#'                 arrowtype = arrow::float64(), max_dim = max_dim)
#' writeCoordArray(float_slab, file.path(tf3, "state"), grid = grid_f,
#'                 arrowtype = arrow::float64(), max_dim = max_dim,
#'                 append = TRUE, along = 2L,
#'                 offset = ncol(int_slab),
#'                 group_offset = dim(grid_i)[2L])
#'
#' @keywords IO
#'
#' @export
#' @rdname writeCoordArray
setGeneric("writeCoordArray",
function(x,
         path,
         indexcols = names(dimnames(x)) %||% sprintf("index%d", seq_along(dim(x))),
         datacol = "value",
         grid = defaultAutoGrid(COO_SparseArray(dim(x))),
         grid_suffix = "_group",
         arrowtype = NULL,
         max_dim = NULL,
         append = FALSE,
         along = NULL,
         offset = 0L,
         group_offset = 0L,
         ...)
    standardGeneric("writeCoordArray")
)

#' @export
#' @importFrom DelayedArray blockApply defaultAutoGrid getAutoBPPARAM
#' @importFrom DuckDBDataFrame checkHiveAppendPartitions
#' @importFrom S4Vectors head tail
#' @importFrom SparseArray COO_SparseArray nzvals
#' @rdname writeCoordArray
setMethod("writeCoordArray", "ANY",
function(x,
         path,
         indexcols = names(dimnames(x)) %||% sprintf("index%d", seq_along(dim(x))),
         datacol = "value",
         grid = defaultAutoGrid(COO_SparseArray(dim(x))),
         grid_suffix = "_group",
         arrowtype = NULL,
         max_dim = NULL,
         append = FALSE,
         along = NULL,
         offset = 0L,
         group_offset = 0L,
         BPPARAM = getAutoBPPARAM(),
         ...)
{
    prep <- .setupCoordWrite(x, path, indexcols, datacol, grid, grid_suffix,
                             arrowtype, max_dim, append, along, offset,
                             group_offset, infer_value = TRUE,
                             BPPARAM = BPPARAM)
    x <- prep$x
    indexcols <- prep$indexcols
    datacol <- prep$datacol
    schema <- prep$schema
    along <- prep$along
    offset <- prep$offset
    group_offset <- prep$group_offset

    dimnames(x) <- lapply(dim(x), function(d) as.character(seq_len(d)))

    if (length(grid) == 1L) {
        .writeCoordArray(x, path = path, indexcols = indexcols,
                         datacol = datacol, schema = schema, ...)
    } else {
        blockApply(x, FUN = .writeCellFUN,
                   path = path,
                   indexcols = indexcols,
                   datacol = datacol,
                   schema = schema,
                   grid_suffix = grid_suffix,
                   along = along,
                   offset = offset,
                   group_offset = group_offset,
                   ...,
                   grid = grid,
                   as.sparse = TRUE,
                   BPPARAM = BPPARAM,
                   verbose = NA)
    }

    invisible(NULL)
})

#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom dbplyr sql_render
#' @importFrom DuckDBArray dbconn tblconn
#' @importFrom DuckDBDataFrame arrowTypeToName checkHiveAppendPartitions
#' @rdname writeCoordArray
setMethod("writeCoordArray", "DuckDBArray",
function(x,
         path,
         indexcols = names(dimnames(x)) %||% sprintf("index%d", seq_along(dim(x))),
         datacol = "value",
         grid = defaultAutoGrid(COO_SparseArray(dim(x))),
         grid_suffix = "_group",
         arrowtype = NULL,
         max_dim = NULL,
         append = FALSE,
         along = NULL,
         offset = 0L,
         group_offset = 0L,
         ...)
{
    # DuckDBArray fast-path using SQL COPY TO
    # NOTE: This method does NOT support BPPARAM because DuckDB connections
    # are not thread-safe for concurrent query execution. All DuckDBArray
    # objects share a singleton connection via acquireDuckDBConn(). DuckDB's
    # internal parallelism (worker_threads setting) provides sufficient
    # performance for COPY TO operations.
    seed <- x@seed
    conn <- dbconn(seed)
    tbl <- tblconn(seed)

    # Auto-detect indexcols from table keycols if not provided
    if (is.null(indexcols)) {
        indexcols <- names(seed@table@keycols)
    }

    prep <- .setupCoordWrite(x, path, indexcols, datacol, grid, grid_suffix,
                             arrowtype, max_dim, append, along, offset,
                             group_offset, infer_value = FALSE,
                             BPPARAM = NULL)
    indexcols <- prep$indexcols
    datacol <- prep$datacol
    schema <- prep$schema
    along <- prep$along
    offset <- prep$offset
    group_offset <- prep$group_offset

    if (!append) {
        schema_probe <- dbGetQuery(conn, sprintf(
            "SELECT * FROM (%s) LIMIT 0",
            sql_render(tbl)
        ))
        if (is.null(arrowtype)) {
            schema$value <- arrowTypeToName(
                .duckdbTypeToArrow(class(schema_probe[[datacol]])[1L])
            )
        }
        if (is.null(max_dim)) {
            schema$index <- vapply(indexcols, function(col) {
                arrowTypeToName(
                    .duckdbTypeToArrow(class(schema_probe[[col]])[1L])
                )
            }, character(1L))
            names(schema$index) <- indexcols
        }
    }

    schema_arrow <- .coordSchemaArrowTypes(schema)

    if (length(grid) == 1L) {
        if (append) {
            stop("'append = TRUE' requires hive-partitioned output ",
                 "(length(grid) > 1L)")
        }
        .writeDuckDBArraySingle(x,
                                path = path,
                                indexcols = indexcols,
                                datacol = datacol,
                                arrowtype = schema_arrow$value,
                                ...)
    } else {
        .writeDuckDBArrayPartitionedWithPartitionBy(x,
                                                    path = path,
                                                    indexcols = indexcols,
                                                    datacol = datacol,
                                                    grid = grid,
                                                    grid_suffix = grid_suffix,
                                                    idxtypes = schema_arrow$index,
                                                    arrowtype = schema_arrow$value,
                                                    along = along,
                                                    offset = offset,
                                                    group_offset = group_offset,
                                                    ...)
    }

    invisible(NULL)
})
