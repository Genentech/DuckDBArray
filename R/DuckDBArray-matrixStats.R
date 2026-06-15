#' DuckDBArray row / column summarization methods
#'
#' @description
#' Row / column summarization methods for \linkS4class{DuckDBArray} objects.
#'
#' @section Row / Column Summarization Methods:
#' In the code snippets below, \code{x} is a DuckDBArray object:
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
#' @aliases rowCounts,DuckDBArray-method
#' @aliases colCounts,DuckDBArray-method
#' @aliases rowMaxs,DuckDBArray-method
#' @aliases colMaxs,DuckDBArray-method
#' @aliases rowMeans,DuckDBArray-method
#' @aliases colMeans,DuckDBArray-method
#' @aliases rowMins,DuckDBArray-method
#' @aliases colMins,DuckDBArray-method
#' @aliases rowSums,DuckDBArray-method
#' @aliases colSums,DuckDBArray-method
#' @aliases rowSds,DuckDBArray-method
#' @aliases colSds,DuckDBArray-method
#' @aliases rowVars,DuckDBArray-method
#' @aliases colVars,DuckDBArray-method
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{DuckDBArray-class}} for the main class
#'   \item \code{\link{DuckDBMatrix-class}} for the matrix class
#'   \item \code{\link{DuckDBArraySeed-class}} for the internal seed class
#'   \item \code{\link{DuckDBArraySeed-matrixStats}} for the internal seed class
#'         matrixStats methods
#'   \item \code{\link[DelayedArray]{DelayedArray}} for the base class
#' }
#'
#' @include DuckDBArray-class.R
#' @include DuckDBArraySeed-matrixStats.R
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
#' pqarray <- DuckDBArray(tf, datacol = "fate",
#'                        keycols = c("Class", "Sex", "Age", "Survived"))
#' colMeans(pqarray)
#' rowSums(pqarray, dims = 2L)
#'
#' @name DuckDBArray-matrixStats
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### matrixStats methods
###

#' @export
#' @importFrom MatrixGenerics rowCounts
setMethod("rowCounts", "DuckDBArray",
function(x, value = TRUE, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, value = value, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics colCounts
setMethod("colCounts", "DuckDBArray",
function(x, value = TRUE, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, value = value, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics rowMaxs
setMethod("rowMaxs", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics colMaxs
setMethod("colMaxs", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics rowMeans
setMethod("rowMeans", "DuckDBArray", function(x, na.rm = FALSE, dims = 1, ...) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics colMeans
setMethod("colMeans", "DuckDBArray", function(x, na.rm = FALSE, dims = 1, ...) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics rowMins
setMethod("rowMins", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics colMins
setMethod("colMins", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics rowSums
setMethod("rowSums", "DuckDBArray", function(x, na.rm = FALSE, dims = 1, ...) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics colSums
setMethod("colSums", "DuckDBArray", function(x, na.rm = FALSE, dims = 1, ...) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics rowSds
setMethod("rowSds", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics colSds
setMethod("colSds", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics rowVars
setMethod("rowVars", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})

#' @export
#' @importFrom MatrixGenerics colVars
setMethod("colVars", "DuckDBArray",
function(x, na.rm = FALSE, dims = 1, ..., useNames = TRUE) {
    as.array(replaceSlots(x, seed = callGeneric(x@seed, na.rm = na.rm, dims = dims, ...),
                          check = FALSE), drop = TRUE)
})
