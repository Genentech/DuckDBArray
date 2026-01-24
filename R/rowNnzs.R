#' Row and Column Non-Zero Counts
#'
#' @description
#' Count the number of non-zero elements per row or column of a matrix-like
#' object. This is useful for filtering genes by detection rate (number of
#' cells where the gene is expressed) or filtering cells by complexity
#' (number of genes detected).
#'
#' @param x A matrix-like object.
#' @param value The value to treat as "zero" (i.e., elements NOT equal to this
#'   value are counted). Defaults to the zero value for the storage mode of
#'   \code{x}.
#' @param na.rm If \code{TRUE}, missing values are excluded first.
#' @param dims A single integer specifying the dimension(s) over which to
#'   operate. For \code{rowNnzs}, \code{dims = 1} counts non-zeros in each row.
#'   For \code{colNnzs}, \code{dims = 1} counts non-zeros in each column.
#'   Only used for \code{DuckDBArray} objects.
#' @param ... Additional arguments passed to \code{\link[MatrixGenerics]{rowCounts}}
#'   or \code{\link[MatrixGenerics]{colCounts}}.
#' @param useNames If \code{TRUE}, the names of the input are preserved in
#'   the output.
#'
#' @details
#' These functions are convenience wrappers around \code{rowCounts} and
#' \code{colCounts} that count elements not equal to zero. The zero value
#' is determined by the storage mode of \code{x}:
#' \itemize{
#'   \item For logical matrices: \code{FALSE}
#'   \item For integer matrices: \code{0L}
#'   \item For numeric matrices: \code{0}
#'   \item For character matrices: \code{""}
#' }
#'
#' @return An integer vector with length equal to \code{nrow(x)} for
#'   \code{rowNnzs} or \code{ncol(x)} for \code{colNnzs}.
#'
#' @author Patrick Aboyoun
#'
#' @seealso
#' \code{\link[MatrixGenerics]{rowCounts}} for counting specific values.
#'
#' @examples
#' # Dense matrix
#' mat <- matrix(c(0L, 1L, 2L, 0L, 0L, 3L), nrow = 2)
#' rowNnzs(mat)
#' colNnzs(mat)
#'
#' @include DuckDBArray-matrixStats.R
#'
#' @export
#' @rdname rowNnzs
setGeneric("rowNnzs", function(x, value = vector(type(x), 1L), ...)
    standardGeneric("rowNnzs")
)

#' @export
#' @rdname rowNnzs
setGeneric("colNnzs", function(x, value = vector(type(x), 1L), ...)
    standardGeneric("colNnzs")
)

#' @export
#' @importFrom MatrixGenerics rowCounts
#' @importFrom S4Vectors tail
#' @rdname rowNnzs
setMethod("rowNnzs", "ANY",
function(x, value = vector(type(x), 1L), ...) {
    n <- prod(tail(dim(x), -1L))
    n - rowCounts(x, value = value, ...)
})

#' @export
#' @importFrom MatrixGenerics rowCounts
#' @importFrom S4Vectors tail
#' @rdname rowNnzs
setMethod("rowNnzs", "DuckDBArray",
function(x, value = vector(type(x), 1L), na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    n <- prod(tail(dim(x), -dims))
    n - rowCounts(x, value = value, na.rm = na.rm, dims = dims, ...,
                  useNames = useNames)
})

#' @export
#' @importFrom MatrixGenerics colCounts
#' @importFrom S4Vectors head
#' @rdname rowNnzs
setMethod("colNnzs", "ANY",
function(x, value = vector(type(x), 1L), ...) {
    n <- prod(head(dim(x), 1L))
    n - colCounts(x, value = value, ...)
})

#' @export
#' @importFrom MatrixGenerics colCounts
#' @importFrom S4Vectors head
#' @rdname rowNnzs
setMethod("colNnzs", "DuckDBArray",
function(x, value = vector(type(x), 1L), na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    n <- prod(head(dim(x), dims))
    n - colCounts(x, value = value, na.rm = na.rm, dims = dims, ...,
                  useNames = useNames)
})
