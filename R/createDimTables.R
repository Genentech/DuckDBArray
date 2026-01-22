#' Create Dimension Lookup Tables
#'
#' @description
#' Creates lookup tables for array dimensions that map dimension names to their
#' corresponding indices and grid group identifiers. These tables are useful
#' for understanding the structure of partitioned arrays and for reconstructing
#' the original array from coordinate format data.
#'
#' @param x An array-like object with dimensions and dimension names. Must be
#' coercible to a \code{SparseArray} object. Common examples include matrices,
#' arrays, and \code{table} objects.
#' @param indexcols A character vector of column names for the dimensions of the
#' array.
#' @param grid An optional \link[S4Arrays]{ArrayGrid} to use for partitioning
#' the array. If \code{NULL}, uses
#' \code{defaultAutoGrid(COO_SparseArray(dim(x)))}.
#' @param grid_suffix A character string to append to the partitioning
#' directories if the grid contains more than one cell. Defaults to "_group".
#' @param BPPARAM A \link[BiocParallel]{BiocParallelParam} object to use for
#' parallel processing when \code{grid} contains multiple cells. Defaults to
#' \code{getAutoBPPARAM()}.
#'
#' @return A named list of data frames, one for each dimension in \code{x}.
#' Each data frame contains:
#' \itemize{
#'   \item The dimension names as row names
#'   \item A column with the corresponding indices
#'   \item A column with the grid group identifier (if grid has multiple cells)
#' }
#' The list is named according to \code{indexcols}.
#'
#' @details
#' This function is particularly useful when working with partitioned arrays
#' created by \code{BiocDuckDB::writeParquet}. The returned lookup tables map
#' between dimension names and their positions in the original array, as well
#' as identify which grid partition contains each dimension element.
#'
#' When the grid contains only one cell, the lookup tables contain only the
#' dimension names and their indices. When multiple grid cells are present,
#' additional columns indicate which grid group each dimension element belongs
#' to.
#'
#' @author Patrick Aboyoun
#'
#' @seealso
#' \code{\link[S4Arrays]{ArrayGrid}} for grid partitioning,
#' \code{\link[BiocParallel]{BiocParallelParam}} for parallel processing options
#'
#' @examples
#' # Create dimension lookup tables for the state.x77 matrix
#' state_grid <- RegularArrayGrid(dim(state.x77), c(10, 4))
#' createDimTables(state.x77, grid = state_grid)
#'
#' @export
#' @importFrom DelayedArray blockApply defaultAutoGrid getAutoBPPARAM
#' @importFrom SparseArray COO_SparseArray
#' @rdname createDimTables
createDimTables <-
function(x,
         indexcols = names(dimnames(x)) %||% sprintf("index%d", seq_along(dim(x))),
         grid = defaultAutoGrid(COO_SparseArray(dim(x))),
         grid_suffix = "_group",
         BPPARAM = getAutoBPPARAM())
{
    if (is.null(dim(x))) {
        stop("'x' must be an array-like object")
    }

    if (inherits(x, "table")) {
        x <- unclass(x)
    }

    # Make index column names unique
    indexcols <- make.unique(indexcols, sep = "_")

    if (length(grid) == 1L) {
        dimtbls <- vector("list", length(indexcols))
    } else {
        dimtbls <- blockApply(COO_SparseArray(dim(x)),
                              FUN = .createDimTables,
                              indexcols = indexcols,
                              grid_suffix = grid_suffix,
                              grid = grid,
                              as.sparse = TRUE,
                              BPPARAM = BPPARAM,
                              verbose = NA)
        dimtbls <- lapply(seq_along(indexcols), function(i) {
            df <- do.call(rbind, lapply(dimtbls, function(x) x[[i]]))
            df[order(df[[1L]]), -1L, drop = FALSE]
        })
    }
    names(dimtbls) <- indexcols

    dimtbls
}

#' @importFrom DelayedArray currentViewport effectiveGrid
#' @importFrom S4Arrays mapToGrid
.createDimTables <-
function(x, paths, indexcols, grid_suffix, idxcol)
{
    grid <- effectiveGrid()
    viewport <- currentViewport()
    groups <- as.vector(mapToGrid(start(viewport), grid)[["major"]])
    dimtbls <- vector("list", length(indexcols))
    for (i in seq_along(indexcols)) {
        if (all(groups[-i] == 1L)) {
            tbl <- data.frame(start(viewport)[i]:end(viewport)[i], groups[i])
            colnames(tbl) <- c("", paste0(indexcols[i], grid_suffix))
            dimtbls[[i]] <- tbl
        }
    }
    dimtbls
}
