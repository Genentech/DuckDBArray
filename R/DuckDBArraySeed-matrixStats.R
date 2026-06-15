#' DuckDBArraySeed row / column summarization methods
#'
#' @description
#' Row / column summarization methods for \linkS4class{DuckDBArraySeed} objects.
#'
#' @section Row / Column Summarization Methods:
#' In the code snippets below, \code{x} is a DuckDBArraySeed object:
#' \describe{
#'   \item{\code{rowCounts(x, value = TRUE)}:}{
#'     Calculates the row counts of \code{x} that are equal to \code{value}.
#'     \describe{
#'       \item{\code{value}}{The value to count.}
#'     }
#'   }
#'   \item{\code{colCounts(x, value = TRUE)}:}{
#'     Calculates the column counts of \code{x} that are equal to \code{value}.
#'     \describe{
#'       \item{\code{value}}{The value to count.}
#'     }
#'   }
#'   \item{\code{rowMaxs(x)}:}{
#'     Calculates the row maxima of \code{x}.
#'   }
#'   \item{\code{colMaxs(x)}:}{
#'     Calculates the column maxima of \code{x}.
#'   }
#'   \item{\code{rowMeans(x, dims = 1)}:}{
#'     Calculates the row means of \code{x}.
#'     \describe{
#'       \item{\code{dims}}{An integer specifying which dimensions to average over,
#'         namely \code{dims + 1}, \ldots.}
#'     }
#'   }
#'   \item{\code{colMeans(x, dims = 1)}:}{
#'     Calculates the column means of \code{x}.
#'     \describe{
#'       \item{\code{dims}}{An integer specifying which dimensions to average over,
#'         namely \code{1:dims}.}
#'     }
#'   }
#'   \item{\code{rowMins(x)}:}{
#'     Calculates the row minima of \code{x}.
#'   }
#'   \item{\code{colMins(x)}:}{
#'     Calculates the column minima of \code{x}.
#'   }
#'   \item{\code{rowSums(x, dims = 1)}:}{
#'     Calculates the row sums of \code{x}.
#'     \describe{
#'       \item{\code{dims}}{An integer specifying which dimensions to sum over,
#'         namely \code{dims + 1}, \ldots.}
#'     }
#'   }
#'   \item{\code{colSums(x, dims = 1)}:}{
#'     Calculates the column sums of \code{x}.
#'     \describe{
#'       \item{\code{dims}}{An integer specifying which dimensions to sum over,
#'         namely \code{1:dims}.}
#'     }
#'   }
#'   \item{\code{rowSds(x)}:}{
#'     Calculates the row standard deviations of \code{x}.
#'   }
#'   \item{\code{colSds(x)}:}{
#'     Calculates the column standard deviations of \code{x}.
#'   }
#'   \item{\code{rowVars(x)}:}{
#'     Calculates the row variances of \code{x}.
#'   }
#'   \item{\code{colVars(x)}:}{
#'     Calculates the column variances of \code{x}.
#'   }
#' }
#'
#' @return
#' Method return types are documented in the sections above.
#'
#' @author Patrick Aboyoun
#'
#' @aliases rowCounts,DuckDBArraySeed-method
#' @aliases colCounts,DuckDBArraySeed-method
#' @aliases rowMaxs,DuckDBArraySeed-method
#' @aliases colMaxs,DuckDBArraySeed-method
#' @aliases rowMeans,DuckDBArraySeed-method
#' @aliases colMeans,DuckDBArraySeed-method
#' @aliases rowMins,DuckDBArraySeed-method
#' @aliases colMins,DuckDBArraySeed-method
#' @aliases rowSums,DuckDBArraySeed-method
#' @aliases colSums,DuckDBArraySeed-method
#' @aliases rowSds,DuckDBArraySeed-method
#' @aliases colSds,DuckDBArraySeed-method
#' @aliases rowVars,DuckDBArraySeed-method
#' @aliases colVars,DuckDBArraySeed-method
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{DuckDBArraySeed-class}} for the internal seed class
#'   \item \code{\link{DuckDBArray-class}} for the main class
#'   \item \code{\link{DuckDBArray-matrixStats}} for the main class matrixStats methods
#'   \item \code{\link[S4Arrays]{Array}} for the base class
#' }
#'
#' @include DuckDBArraySeed-class.R
#' @include DuckDBTable-matrixStats.R
#'
#' @keywords utilities methods
#'
#' @examples
#' df <- do.call(expand.grid, c(dimnames(Titanic), stringsAsFactors = FALSE))
#' df$fate <- as.integer(Titanic[as.matrix(df)])
#' df <- df[df$fate != 0L, ]
#' tf <- tempfile(fileext = ".parquet")
#' on.exit(unlink(tf))
#' arrow::write_parquet(df, tf)
#' seed <- DuckDBArray(tf, datacol = "fate",
#'                     keycols = c("Class", "Sex", "Age", "Survived"))@seed
#' colMeans(seed)
#' rowSums(seed, dims = 2L)
#'
#' @name DuckDBArraySeed-matrixStats
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### matrixStats methods
###

#' @export
#' @importFrom MatrixGenerics rowCounts
setMethod("rowCounts", "DuckDBArraySeed",
function(x, value = TRUE, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    if (x@fill == value) {
        fill <- prod(tail(dim(x), - dims))
    } else {
        fill <- vector(coltypes(x@table), 1L)
    }
    replaceSlots(x, table = callGeneric(x@table, value = value, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = fill, check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics colCounts
setMethod("colCounts", "DuckDBArraySeed",
function(x, value = TRUE, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    if (x@fill == value) {
        fill <- prod(head(dim(x), dims))
    } else {
        fill <- vector(coltypes(x@table), 1L)
    }
    replaceSlots(x, table = callGeneric(x@table, value = value, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = fill, check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics rowMaxs
setMethod("rowMaxs", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics colMaxs
setMethod("colMaxs", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics rowMeans
setMethod("rowMeans", "DuckDBArraySeed", function(x, na.rm = FALSE, dims = 1, ...) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics colMeans
setMethod("colMeans", "DuckDBArraySeed", function(x, na.rm = FALSE, dims = 1, ...) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics rowMins
setMethod("rowMins", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics colMins
setMethod("colMins", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics rowSums
setMethod("rowSums", "DuckDBArraySeed", function(x, na.rm = FALSE, dims = 1, ...) {
    fill <- x@fill
    if (fill != 0) {
        fill <- fill * prod(tail(dim(x), - dims))
    }
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = fill, check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics colSums
setMethod("colSums", "DuckDBArraySeed", function(x, na.rm = FALSE, dims = 1, ...) {
    fill <- x@fill
    if (fill != 0) {
        fill <- fill * prod(head(dim(x), dims))
    }
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = fill, check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics rowSds
setMethod("rowSds", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = vector(coltypes(x@table), 1L), check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics colSds
setMethod("colSds", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = vector(coltypes(x@table), 1L), check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics rowVars
setMethod("rowVars", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = vector(coltypes(x@table), 1L), check = FALSE)
})

#' @export
#' @importFrom MatrixGenerics colVars
setMethod("colVars", "DuckDBArraySeed",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    replaceSlots(x, table = callGeneric(x@table, na.rm = na.rm, dims = dims, fill = x@fill, ...),
                 fill = vector(coltypes(x@table), 1L), check = FALSE)
})
