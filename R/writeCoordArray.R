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
         BPPARAM = getAutoBPPARAM(),
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
         BPPARAM = getAutoBPPARAM(),
         arrowtype = NULL,
         max_dim = NULL,
         append = FALSE,
         along = NULL,
         offset = 0L,
         group_offset = 0L,
         ...)
{
    # Input normalization
    if (is.null(dim(x))) {
        stop("the default method of writeParquet requires 'x' to be array-like")
    }
    if (inherits(x, "table")) {
        x <- unclass(x)
    }
    ndim <- length(dim(x))

    # 'indexcols'/'datacol' are deduplicated up front because every
    # downstream step (hive-layout checks, schema I/O, file writing)
    # relies on the post-make.unique names.
    unique_names <- make.unique(c(indexcols, datacol), sep = "_")
    indexcols <- head(unique_names, -1L)
    datacol <- tail(unique_names, 1L)

    # Argument validation
    .validateArrowtype(arrowtype)

    ap <- .validateAppend(append, grid, along, offset, group_offset,
                          path, ndim)
    along <- ap$along
    offset <- ap$offset
    group_offset <- ap$group_offset

    max_dim <- .validateMaxDim(max_dim, x, append, along, offset, ndim)

    # Index-column arrow types
    #
    # 'idxtypes' is the single source of truth for index-column arrow
    # types and is threaded into every cell write so partitions cannot
    # drift from one another.
    idxtypes <- .computeIdxtypes(dim(x), max_dim, append, along, offset)

    # Append pre-flight
    #
    # Everything here runs before any file is written so a failed check
    # leaves the existing dataset byte-for-byte unchanged. When the
    # caller did not pin 'arrowtype' / 'max_dim', we adopt the existing
    # dataset's types so the appended slab cannot drift.
    if (append) {
        reconciled <- .reconcileAppendSchema(path, indexcols, datacol,
                                             arrowtype, max_dim, idxtypes)
        arrowtype <- reconciled$arrowtype
        idxtypes <- reconciled$idxtypes

        .checkAppendPartitions(path = path, indexcols = indexcols,
                               grid_suffix = grid_suffix, grid = grid,
                               along = along, group_offset = group_offset)
    }

    # Value-column type inference (non-append only)
    if (!append && is.null(arrowtype)) {
        arrowtype <- .inferValueType(x, grid, BPPARAM)
    }

    # Write
    dimnames(x) <- lapply(dim(x), function(d) as.character(seq_len(d)))

    if (length(grid) == 1L) {
        # append precluded above (requires length(grid) > 1L).
        .writeCoordArray(x, path = path, indexcols = indexcols,
                         datacol = datacol, idxtypes = idxtypes,
                         arrowtype = arrowtype, ...)
    } else {
        blockApply(x, FUN = .writeCellFUN,
                   path = path,
                   indexcols = indexcols,
                   datacol = datacol,
                   idxtypes = idxtypes,
                   arrowtype = arrowtype,
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
#' @rdname writeCoordArray
setMethod("writeCoordArray", "DuckDBArray",
function(x,
         path,
         indexcols = names(dimnames(x)) %||% sprintf("index%d", seq_along(dim(x))),
         datacol = "value",
         grid = defaultAutoGrid(COO_SparseArray(dim(x))),
         grid_suffix = "_group",
         BPPARAM = getAutoBPPARAM(),
         arrowtype = NULL,
         max_dim = NULL,
         append = FALSE,
         along = NULL,
         offset = 0L,
         group_offset = 0L,
         ...)
{
    # DuckDBArray fast-path using SQL COPY TO
    seed <- x@seed
    conn <- dbconn(seed)
    tbl <- tblconn(seed)

    # Auto-detect indexcols from table keycols if not provided
    if (is.null(indexcols)) {
        indexcols <- names(seed@table@keycols)
    }

    # Make column names unique
    unique_names <- make.unique(c(indexcols, datacol), sep = "_")
    indexcols <- head(unique_names, -1L)
    datacol <- tail(unique_names, 1L)

    # Extract arrow types from DuckDB schema (skip inference)
    schema_probe <- dbGetQuery(conn, sprintf(
        "SELECT * FROM (%s) LIMIT 0",
        sql_render(tbl)
    ))

    # Map DuckDB types to arrow types
    arrowtype <- .duckdbTypeToArrow(class(schema_probe[[datacol]])[1L])
    idxtypes <- lapply(indexcols, function(col) {
        .duckdbTypeToArrow(class(schema_probe[[col]])[1L])
    })

    if (length(grid) == 1L) {
        # Single-file write: Direct COPY TO
        .writeDuckDBArraySingle(x,
                                path = path,
                                indexcols = indexcols,
                                datacol = datacol,
                                arrowtype = arrowtype,
                                ...)
    } else {
        # Multi-partition write: blockApply with SQL-based writes
        .writeDuckDBArrayPartitioned(x,
                                     path = path,
                                     indexcols = indexcols,
                                     datacol = datacol,
                                     grid = grid,
                                     grid_suffix = grid_suffix,
                                     idxtypes = idxtypes,
                                     arrowtype = arrowtype,
                                     BPPARAM = BPPARAM,
                                     ...)
    }

    invisible(NULL)
})

# Validation

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
    if (!isSingleNumber(offset) || offset != as.integer(offset) ||
        offset < 0L) {
        stop("'offset' must be a single non-negative integer")
    }
    if (!isSingleNumber(group_offset) ||
        group_offset != as.integer(group_offset) ||
        group_offset < 0L) {
        stop("'group_offset' must be a single non-negative integer")
    }
    if (!dir.exists(path)) {
        stop("'append = TRUE' but target directory does not exist: ", path)
    }

    list(along = as.integer(along),
         offset = as.integer(offset),
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

# Type derivation

# Returns a list of arrow DataTypes (one per dimension), one for each
# index column. When 'max_dim' is supplied it is used verbatim as the
# upper bound; otherwise dim(x) is used, widened along 'along' by
# 'offset' in append mode so that shifted coordinates still fit.
.computeIdxtypes <- function(dim_x, max_dim, append, along, offset) {
    if (!is.null(max_dim)) {
        dim_bound <- max_dim
    } else {
        dim_bound <- dim_x
        if (append) {
            dim_bound[along] <- dim_bound[along] + offset
        }
    }
    lapply(seq_along(dim_bound),
           function(j) .arrowIntType(c(0L, dim_bound[j])))
}

# Predicate: is 'vals' (a vector) an integer-valued numeric vector with
# no NAs interfering in the 'floor(x) == x' check? Integer vectors pass
# trivially; double vectors pass only if every value equals its floor.
.isIntegerValued <- function(vals) {
    is.integer(vals) ||
        (is.numeric(vals) && all(vals == floor(vals), na.rm = TRUE))
}

# Infer an arrow DataType for the value column by scanning blocks of 'x'
# to find the range of non-zero values. Returns the narrowest integer
# type capable of representing the range, or NULL when any block
# contains non-integer-valued data (in which case the caller should fall
# back to letting arrow auto-pick 'double').
#' @importFrom DelayedArray blockApply
#' @importFrom SparseArray nzvals
.inferValueType <- function(x, grid, BPPARAM) {
    block_range <- function(block) {
        vals <- c(0L, nzvals(block))
        if (!.isIntegerValued(vals)) {
            stop("not an integer array")
        }
        range(vals, na.rm = TRUE)
    }

    if (length(grid) == 1L) {
        vals <- c(0L, nzvals(x))
        if (!.isIntegerValued(vals)) return(NULL)
        return(.arrowIntType(range(vals, na.rm = TRUE)))
    }

    ranges <- try(blockApply(x, FUN = block_range,
                             grid = grid, as.sparse = TRUE,
                             BPPARAM = BPPARAM, verbose = NA),
                  silent = TRUE)
    if (inherits(ranges, "try-error")) return(NULL)

    min_x <- min(vapply(ranges, `[`, numeric(1L), 1L))
    max_x <- max(vapply(ranges, `[`, numeric(1L), 2L))
    .arrowIntType(c(min_x, max_x))
}

# Append reconciliation

# Read one existing parquet file and return its index-column and
# value-column arrow types. Used both to verify a caller-supplied
# schema and to adopt the existing schema when none was pinned.
#' @importFrom arrow read_parquet schema
.readExistingSchema <- function(path, indexcols, datacol) {
    files <- list.files(path, pattern = "\\.parquet$",
                        recursive = TRUE, full.names = TRUE)
    if (length(files) == 0L) {
        stop("'append = TRUE' but no parquet files found under ", path)
    }
    sch <- schema(read_parquet(files[1L], as_data_frame = FALSE))
    required <- c(indexcols, datacol)
    missing_fields <- setdiff(required, names(sch))
    if (length(missing_fields) > 0L) {
        stop("'append = TRUE' schema mismatch: existing dataset at ", path,
             " lacks field(s) ",
             paste(shQuote(missing_fields), collapse = ", "),
             " (check 'indexcols' / 'datacol')")
    }
    list(idxtypes = lapply(indexcols,
                           function(nm) sch$GetFieldByName(nm)$type),
         datatype = sch$GetFieldByName(datacol)$type)
}

# Merge the caller's (possibly-NULL) 'arrowtype'/'idxtypes' with the
# existing dataset's schema. If the caller pinned a type, it must match
# the existing type or we error. If the caller did not pin, we adopt the
# existing type so the appended slab cannot drift. Returns a list with
# the resolved 'arrowtype' and 'idxtypes'.
.reconcileAppendSchema <-
function(path, indexcols, datacol, arrowtype, max_dim, idxtypes)
{
    existing <- .readExistingSchema(path, indexcols, datacol)

    if (is.null(arrowtype)) {
        arrowtype <- existing$datatype
    } else if (!arrowtype$Equals(existing$datatype)) {
        stop("'append = TRUE' schema mismatch on '", datacol,
             "': existing type is ", existing$datatype$ToString(),
             ", supplied 'arrowtype' is ", arrowtype$ToString())
    }

    if (is.null(max_dim)) {
        idxtypes <- existing$idxtypes
    } else {
        for (j in seq_along(indexcols)) {
            if (!idxtypes[[j]]$Equals(existing$idxtypes[[j]])) {
                stop("'append = TRUE' schema mismatch on column '",
                     indexcols[j], "': existing type is ",
                     existing$idxtypes[[j]]$ToString(),
                     ", supplied 'max_dim' implies ",
                     idxtypes[[j]]$ToString())
            }
        }
    }

    list(arrowtype = arrowtype, idxtypes = idxtypes)
}

# Pre-flight partition collision check for append mode. Enforces two
# invariants: (1) the existing dataset is hive-partitioned at the top
# level (no loose parquet files or unexpected subdirectories), and (2)
# every partition directory this call would create does not already
# exist. Both failure modes would otherwise silently merge or overwrite
# existing data and corrupt the dataset.
.checkAppendPartitions <-
function(path, indexcols, grid_suffix, grid, along, group_offset)
{
    ndim <- length(indexcols)
    dim_grid <- dim(grid)
    pfx1 <- paste0(indexcols[1L], grid_suffix, "=")

    top <- list.files(path, all.files = FALSE, full.names = FALSE,
                      no.. = TRUE)
    top_dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
    loose <- setdiff(top, top_dirs)
    bad_dirs <- top_dirs[!startsWith(top_dirs, pfx1)]
    if (length(loose) > 0L || length(bad_dirs) > 0L) {
        stop("'append = TRUE' requires a hive-partitioned dataset at ",
             path, " (expected subdirectories '", pfx1,
             "<n>'); found unexpected entries: ",
             paste(c(loose, bad_dirs), collapse = ", "))
    }

    groups <- lapply(seq_len(ndim), function(k) seq_len(dim_grid[k]))
    groups[[along]] <- group_offset + seq_len(dim_grid[along])
    cells <- do.call(expand.grid,
                     c(groups, list(KEEP.OUT.ATTRS = FALSE)))
    for (i in seq_len(nrow(cells))) {
        cell <- as.integer(cells[i, ])
        parts <- paste0(indexcols, grid_suffix, "=", cell)
        target <- do.call(file.path, c(list(path), as.list(parts)))
        if (dir.exists(target)) {
            stop("'append = TRUE' would write into an existing partition: ",
                 target,
                 " (check 'group_offset' -- it must skip past every ",
                 "previously-written partition along dimension ", along,
                 ")")
        }
    }
}

# Block writer

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
#' @importFrom arrow Array write_dataset write_parquet
#' @importFrom SparseArray nzwhich nzvals
.writeCoordArray <-
function(x, path, indexcols, datacol, idxtypes, arrowtype = NULL,
         along = NULL, offset = 0L, ...)
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

    # Coerce index columns to their caller-pinned arrow types. If any
    # coordinate overflows the chosen unsigned type (which shouldn't
    # happen when upstream validation is correct), arrow::Array$create
    # throws loudly -- that's the last line of defense against silently
    # writing wrong coordinates.
    for (j in seq_along(indexcols)) {
        lst[[j]] <- Array$create(lst[[j]], type = idxtypes[[j]])
    }
    if (!is.null(arrowtype)) {
        lst[[datacol]] <- Array$create(lst[[datacol]], type = arrowtype)
    }

    class(lst) <- "data.frame"
    attr(lst, "row.names") <- .set_row_names(length(lst[[1L]]))

    if (nrow(lst) == 0L) {
        .writeEmptyParquet(lst, path, ...)
    } else {
        .writeParquetDataset(lst, path, ...)
    }
    invisible(NULL)
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

# Arrow type helpers

# Pick the narrowest arrow integer type that can represent every value
# in 'range_x'. Used for both value-column inference and index-column
# type selection.
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

# Helper functions for type inference and conversion

#' @importFrom arrow infer_type
.arrowType <- function(x) {
    if (is.integer(x)) {
        x <- x[!is.na(x)]
    }

    if (is.integer(x) && length(x) > 0L) {
        .arrowIntType(range(x))
    } else {
        infer_type(x)
    }
}

#' @importFrom arrow int8 int16 int32 int64 uint8 uint16 uint32 uint64
#' @importFrom arrow float32 float64 utf8 bool
#' @importFrom arrow date32 timestamp time32 duration binary
#' @importFrom arrow infer_type
.duckdbTypeToArrow <- function(duckdb_type) {
    # Counterpart to DuckDBDataFrame::.duckdb_type_to_r() (for R representation)
    type <- tolower(duckdb_type)

    # Handle DuckDB complex types
    if (grepl("^(list<.*>|struct[<(].*[>)]|map<.*,.*>)$", type)) {
        return(infer_type(list()))
    }

    if (grepl("^array<.*,\\d+>$", type)) {
        return(infer_type(list()))
    }

    # Scalar types
    switch(type,
           # Boolean
           "boolean" = bool(),

           # Integer types (signed)
           "tinyint" = int8(),
           "smallint" = int16(),
           "integer" = int32(),
           "bigint" = int64(),
           "hugeint" = int64(),

           # Integer types (unsigned)
           "utinyint" = uint8(),
           "usmallint" = uint16(),
           "uinteger" = uint32(),
           "ubigint" = uint64(),
           "uhugeint" = uint64(),

           # Floating point
           "float" = float32(),
           "real" = float32(),
           "double" = float64(),
           "decimal" = float64(),

           # String types
           "varchar" = utf8(),
           "char" = utf8(),
           "bpchar" = utf8(),
           "text" = utf8(),
           "string" = utf8(),

           # Temporal types
           "date" = date32(),
           "timestamp" = timestamp("us", timezone = "UTC"),
           "time" = time32("s"),
           "interval" = duration("s"),

           # Binary types
           "blob" = binary(),
           "bytea" = binary(),

           # Geometry (WKB encoding)
           "geometry" = binary(),
           "geometry_type" = utf8(),

           # Fallback for R class names (from schema probe)
           "integer" = int32(),
           "integer64" = int64(),
           "numeric" = float64(),
           "double" = float64(),
           "character" = utf8(),
           "logical" = bool(),

           # Default fallback
           {
               warning("Unknown DuckDB type '", duckdb_type,
                       "', defaulting to float64")
               float64()
           })
}

.arrowTypeToFormat <- function(arrow_type) {
    type_name <- arrow_type$ToString()

    formats <- c("int8" = "int8",
                 "int16" = "int16",
                 "int32" = "int32",
                 "int64" = "int64",
                 "uint8" = "uint8",
                 "uint16" = "uint16",
                 "uint32" = "uint32",
                 "uint64" = "uint64",
                 "binary" = "binary",
                 "large_binary" = "binary")

    if (type_name %in% names(formats)) {
        formats[[type_name]]
    } else {
        NULL
    }
}


# Convert Arrow type to DuckDB type name for CREATE TABLE statements
.arrowToDuckDBTypeName <- function(arrow_type) {
    format_str <- .arrowTypeToFormat(arrow_type)
    if (is.null(format_str)) {
        return("INTEGER")  # fallback
    }

    switch(format_str,
           "int8" = "TINYINT",
           "int16" = "SMALLINT",
           "int32" = "INTEGER",
           "int64" = "BIGINT",
           "uint8" = "UTINYINT",
           "uint16" = "USMALLINT",
           "uint32" = "UINTEGER",
           "uint64" = "UBIGINT",
           "INTEGER")  # fallback
}

#' @importFrom DBI dbQuoteIdentifier
.quoteColumns <-
function(conn, cols)
{
    vapply(cols, function(col) {
        as.character(dbQuoteIdentifier(conn, col))
    }, character(1L), USE.NAMES = FALSE)
}

#' @importFrom DBI dbQuoteIdentifier dbGetQuery
#' @importFrom dbplyr sql_render
.buildCopyToSQL <-
function(tbl, indexcols, datacol, target_path, where_clause = NULL,
         mapping_tables = NULL, grid_group = NULL)
{
    # Extract connection from tbl
    conn <- tbl$src$con

    # Quote datacol
    quoted_datacol <- .quoteColumns(conn, datacol)

    # Build SELECT clause and JOINs for remapped indices
    if (!is.null(mapping_tables)) {
        # With remapping: SELECT m1.new_idx AS __sample__, m2.new_idx AS __feature__, t.value
        #                 FROM tbl t
        #                 JOIN temp_idxmap_sample m1 ON t.__sample__ = m1.old_key AND m1.grid_group = ?
        #                 JOIN temp_idxmap_feature m2 ON t.__feature__ = m2.old_key AND m2.grid_group = ?

        select_parts <- character(length(indexcols) + 1L)
        join_clauses <- character(length(indexcols))

        for (j in seq_along(indexcols)) {
            col <- indexcols[j]
            map_alias <- sprintf("m%d", j)
            map_table <- mapping_tables[[col]]
            col_quoted <- as.character(dbQuoteIdentifier(conn, col))

            select_parts[j] <- sprintf("%s.new_idx AS %s", map_alias, col_quoted)

            # Add grid_group filter to JOIN condition if provided
            if (!is.null(grid_group)) {
                join_clauses[j] <- sprintf(
                    "INNER JOIN %s %s ON t.%s = %s.old_key AND %s.grid_group = %d",
                    map_table, map_alias, col_quoted, map_alias, map_alias, grid_group[j]
                )
            } else {
                join_clauses[j] <- sprintf(
                    "INNER JOIN %s %s ON t.%s = %s.old_key",
                    map_table, map_alias, col_quoted, map_alias
                )
            }
        }

        select_parts[length(indexcols) + 1L] <- sprintf("t.%s", quoted_datacol)

        # Order by remapped (new) indices in column-major order
        order_cols <- rev(sprintf("m%d.new_idx", seq_along(indexcols)))

        # Build query
        base_query <- sprintf(
            "SELECT %s FROM (%s) t %s",
            paste(select_parts, collapse = ", "),
            sql_render(tbl),
            paste(join_clauses, collapse = " ")
        )
    } else {
        # No remapping: original behavior
        quoted_cols <- .quoteColumns(conn, c(indexcols, datacol))
        order_cols <- .quoteColumns(conn, rev(indexcols))
        base_query <- sprintf(
            "SELECT %s FROM (%s)",
            paste(quoted_cols, collapse = ", "),
            sql_render(tbl)
        )
    }

    # Add WHERE clause if provided
    if (!is.null(where_clause) && nzchar(where_clause)) {
        base_query <- sprintf("%s WHERE %s", base_query, where_clause)
    }

    # Add ORDER BY and COPY TO
    copy_sql <- sprintf(
        "COPY (%s ORDER BY %s) TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd', ROW_GROUP_SIZE 491520, COMPRESSION_LEVEL 3)",
        base_query,
        paste(order_cols, collapse = ", "),
        target_path
    )

    copy_sql
}

#' @importFrom DBI dbExecute
#' @importFrom DuckDBArray dbconn tblconn
.writeDuckDBArraySingle <-
function(x, path, indexcols, datacol, arrowtype, ...)
{
    # Extract components from DuckDBArray
    seed <- x@seed
    conn <- dbconn(seed)
    tbl <- tblconn(seed)
    keycols <- seed@table@keycols

    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    target <- file.path(path, "part-0.parquet")

    # Create index mappings for remapping old keys to 1..N
    viewport_coords <- keycols
    mappings_result <- .buildIndexMappings(tbl, indexcols, viewport_coords)
    mappings <- mappings_result[["mapping_tables"]]

    on.exit({
        for (sql in mappings_result$cleanup_sql) {
            try(dbExecute(conn, sql), silent = TRUE)
        }
    })

    sql <- .buildCopyToSQL(tbl, indexcols, datacol, target,
                           where_clause = NULL, mappings)
    dbExecute(conn, sql)

    invisible(NULL)
}

#' @importFrom DBI dbExecute dbQuoteIdentifier
#' @importFrom duckdb duckdb_register
.buildTempTableFilter <- function(conn, col_name, values) {
    col_quoted <- as.character(dbQuoteIdentifier(conn, col_name))

    # Small lists: inline IN clause
    if (length(values) <= 100L) {
        vals_str <- paste(values, collapse = ", ")
        return(sprintf("%s IN (%s)", col_quoted, vals_str))
    }

    # Medium lists: temp table with VALUES clause
    if (length(values) <= 10000L) {
        temp_suffix <- basename(tempfile(pattern = ""))
        temp_name <- sprintf("temp_viewport_%s_%s", col_name, temp_suffix)

        # Build VALUES clause: (1), (2), (3), ...
        values_clause <- paste0("(", paste(values, collapse = "), ("), ")")
        dbExecute(conn, sprintf(
            "CREATE TEMP TABLE %s (val) AS SELECT * FROM (VALUES %s) t(val)",
            temp_name, values_clause
        ))

        return(sprintf("%s IN (SELECT val FROM %s)", col_quoted, temp_name))
    }

    # Large lists: register R data frame as virtual table
    temp_suffix <- basename(tempfile(pattern = ""))
    temp_name <- sprintf("temp_viewport_%s_%s", col_name, temp_suffix)
    df <- data.frame(val = values)
    duckdb_register(conn, temp_name, df)

    sprintf("%s IN (SELECT val FROM %s)", col_quoted, temp_name)
}

#' @importFrom S4Arrays mapToGrid
.computeGridGroup <- function(n_keys, grid, dim_index) {
    if (is.null(grid)) {
        return(rep.int(1L, n_keys))
    }

    ndim <- length(grid@refdim)
    coords <- matrix(1L, nrow = n_keys, ncol = ndim)
    coords[, dim_index] <- seq_len(n_keys)
    groups <- mapToGrid(coords, grid)[["major"]][, dim_index]

    groups
}

#' @importFrom DBI dbAppendTable dbExecute dbGetQuery
.buildIndexMappings <- function(tbl, indexcols, keycols, grid = NULL) {
    # For each dimension, create temp table: old_key → new_idx → grid_group
    # Returns list(mapping_tables = c(names), cleanup_sql = c(DROP statements))

    # Extract connection from tbl
    conn <- tbl$src$con

    mapping_tables <- character(length(indexcols))
    cleanup_sql <- character(length(indexcols))

    for (j in seq_along(indexcols)) {
        col <- indexcols[j]
        old_keys <- keycols[[col]]
        n_keys <- length(old_keys)
        new_indices <- seq_len(n_keys)

        # Compute which grid partition each index belongs to
        grid_groups <- .computeGridGroup(n_keys, grid, j)

        # Generate unique temp table name
        temp_suffix <- basename(tempfile(pattern = ""))
        temp_name <- sprintf("temp_idxmap_%s_%s", col, temp_suffix)
        mapping_tables[j] <- temp_name

        # Determine optimal integer types using existing helpers
        old_key_type <- .arrowType(old_keys)
        new_idx_type <- .arrowType(new_indices)
        grid_group_type <- .arrowType(grid_groups)

        # Convert Arrow types to DuckDB type names
        old_key_duckdb <- .arrowToDuckDBTypeName(old_key_type)
        new_idx_duckdb <- .arrowToDuckDBTypeName(new_idx_type)
        grid_group_duckdb <- .arrowToDuckDBTypeName(grid_group_type)

        # Create table with explicit types including grid_group column
        create_sql <- sprintf(
            "CREATE TEMP TABLE %s (old_key %s, new_idx %s, grid_group %s)",
            temp_name, old_key_duckdb, new_idx_duckdb, grid_group_duckdb
        )

        dbExecute(conn, create_sql)
        df <- data.frame(old_key = old_keys, new_idx = new_indices, grid_group = grid_groups)
        dbAppendTable(conn, temp_name, df)

        cleanup_sql[j] <- sprintf("DROP TABLE IF EXISTS %s", temp_name)
    }

    list(mapping_tables = setNames(mapping_tables, indexcols),
         cleanup_sql = cleanup_sql)
}

#' @importFrom DBI dbExecute
#' @importFrom DelayedArray currentViewport effectiveGrid
#' @importFrom IRanges ranges start end
#' @importFrom S4Arrays mapToGrid
.writeDuckDBCellFUN <-
function(block, tbl, path, indexcols, datacol, grid_suffix, keycols, mappings, ...)
{
    # Extract connection from tbl
    conn <- tbl$src$con

    # Get grid context from blockApply environment
    grid <- effectiveGrid()
    viewport <- currentViewport()
    group <- as.vector(mapToGrid(start(viewport), grid)[["major"]])

    # Extract coordinate VALUES for this viewport from keycols using viewport ranges
    vp_ranges <- ranges(viewport)
    viewport_coords <- lapply(seq_along(indexcols), function(j) {
        col <- indexcols[j]
        start_idx <- start(vp_ranges)[j]
        end_idx <- end(vp_ranges)[j]
        keycols[[col]][start_idx:end_idx]
    })
    names(viewport_coords) <- indexcols

    # Build hive partition directory
    subdir <- paste0(indexcols, grid_suffix, "=", group)
    target_dir <- do.call(file.path, c(list(path), as.list(subdir)))
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

    # Build SQL WHERE clause using IN lists
    where_clauses <- mapply(function(col, vals) {
        .buildTempTableFilter(conn, col, as.integer(vals))
    }, indexcols, viewport_coords, SIMPLIFY = FALSE)
    where_clause <- paste(where_clauses, collapse = " AND ")

    # Generate and execute COPY TO with grid_group filtering
    target_path <- file.path(target_dir, "part-0.parquet")
    sql <- .buildCopyToSQL(tbl, indexcols, datacol, target_path,
                           where_clause, mappings, grid_group = group)

    dbExecute(conn, sql)

    NULL
}

#' @importFrom DelayedArray blockApply
#' @importFrom DuckDBDataFrame dbconn tblconn
#' @importFrom SparseArray COO_SparseArray
.writeDuckDBArrayPartitioned <-
function(x, path, indexcols, datacol, grid, grid_suffix, idxtypes,
         arrowtype, BPPARAM, ...)
{
    # Extract components from DuckDBArray once
    seed <- x@seed
    conn <- dbconn(seed)
    tbl <- tblconn(seed)
    keycols <- seed@table@keycols

    # Create GLOBAL index mappings once for entire subset array with grid_group column
    mappings_result <- .buildIndexMappings(tbl, indexcols, keycols, grid)
    mappings <- mappings_result[["mapping_tables"]]

    # Cleanup after all viewports complete
    on.exit({
        for (sql in mappings_result$cleanup_sql) {
            try(dbExecute(conn, sql), silent = TRUE)
        }
    })

    # Create empty COO_SparseArray
    empty_array <- COO_SparseArray(dim(x), dimnames = dimnames(x))

    # Use blockApply to iterate viewports
    blockApply(empty_array,
               FUN = .writeDuckDBCellFUN,
               tbl = tbl,
               path = path,
               indexcols = indexcols,
               datacol = datacol,
               grid_suffix = grid_suffix,
               keycols = keycols,
               mappings = mappings,
               grid = grid,
               as.sparse = TRUE,
               BPPARAM = BPPARAM,
               verbose = NA)

    invisible(NULL)
}
