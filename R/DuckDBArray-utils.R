#' Common operations on DuckDBArray objects
#'
#' @description
#' Common operations on \linkS4class{DuckDBArray} objects.
#'
#' @section Group Generics:
#' DuckDBArray objects have support for S4 group generic functionality:
#' \describe{
#'   \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
#'     \code{"\%\%"}, \code{"\%/\%"}, \code{"/"}}
#'   \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"}, \code{"!="},
#'     \code{"<="}, \code{">="}}
#'   \item{\code{Logic}}{\code{"&"}, \code{"|"}, \code{"!"}}
#'   \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}}
#'   \item{\code{Math}}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
#'     \code{"ceiling"}, \code{"floor"}, \code{"trunc"}, \code{"log"},
#'     \code{"log10"}, \code{"log2"}, \code{"acos"}, \code{"acosh"},
#'     \code{"asin"}, \code{"asinh"}, \code{"atan"}, \code{"atanh"},
#'     \code{"exp"}, \code{"expm1"}, \code{"cos"}, \code{"cosh"},
#'     \code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"},
#'     \code{"gamma"}, \code{"lgamma"}}
#'   \item{\code{Summary}}{\code{"max"}, \code{"min"}, \code{"range"},
#'     \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}}
#'  }
#'  See \link[methods]{S4groupGeneric} for more details.
#'
#' @section Numerical Data Methods:
#' In the code snippets below, \code{x} is a DuckDBArray object:
#' \describe{
#'   \item{\code{is.finite(x)}:}{
#'     Returns a DuckDBArray containing logicals that indicate which values are
#'     finite.
#'   }
#'   \item{\code{is.infinite(x)}:}{
#'     Returns a DuckDBArray containing logicals that indicate which values are
#'     infinite.
#'   }
#'   \item{\code{is.nan(x)}:}{
#'     Returns a DuckDBArray containing logicals that indicate which values are
#'     Not a Number.
#'   }
#'   \item{\code{sweep(x, MARGIN, STATS, FUN = "/")}:}{
#'     Sweeps out array summaries from \code{x}. Applies \code{FUN} to each
#'     element of \code{x} using the corresponding value from \code{STATS}
#'     based on \code{MARGIN}.
#'     \describe{
#'       \item{\code{MARGIN}}{integer specifying the dimension (1 for rows,
#'         2 for columns)}
#'       \item{\code{STATS}}{numeric vector with length equal to the extent
#'         of dimension \code{MARGIN}}
#'       \item{\code{FUN}}{function to be used to carry out the sweep}
#'     }
#'   }
#' }
#'
#' @section Sparsity Methods:
#' In the code snippets below, \code{x} is a DuckDBArray object:
#' \describe{
#'   \item{\code{is_nonzero(x)}:}{
#'     Returns a DuckDBArray containing logicals that indicate if the values in
#'     each of the columns of \code{x} are non-zero.
#'   }
#'   \item{\code{nzcount(x)}:}{
#'     Returns the total number of non-zero values.
#'   }
#'   \item{\code{nzvals(x)}:}{
#'     Returns the non-zero values.
#'   }
#'   \item{\code{is_sparse(x)}:}{
#'     Returns \code{TRUE} since data are stored in a sparse array representation.
#'   }
#' }
#'
#' @author Patrick Aboyoun
#'
#' @aliases
#' Ops,DuckDBArray,DuckDBArray-method
#' Ops,DuckDBArray,atomic-method
#' Ops,atomic,DuckDBArray-method
#' Ops,DuckDBArray,missing-method
#' !,DuckDBArray-method
#' Math,DuckDBArray-method
#'
#' is.finite,DuckDBArray-method
#' is.infinite,DuckDBArray-method
#' is.nan,DuckDBArray-method
#' sweep,DuckDBArray-method
#'
#' is_nonzero,DuckDBArray-method
#' nzcount,DuckDBArray-method
#' nzvals,DuckDBArray-method
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{DuckDBArray-class}} for the main class
#'   \item \code{\link{DuckDBMatrix-class}} for the matrix class
#'   \item \code{\link{DuckDBArraySeed-class}} for the internal seed class
#'   \item \code{\link{DuckDBArraySeed-utils}} for the internal seed class
#'         utilities
#'   \item \code{\link[DelayedArray]{DelayedArray}} for the base class
#' }
#'
#' @include DuckDBArray-class.R
#'
#' @keywords utilities methods
#'
#' @name DuckDBArray-utils
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

#' @export
setMethod("Ops", c(e1 = "DuckDBArray", e2 = "DuckDBArray"), function(e1, e2) {
    replaceSlots(e1, seed = callGeneric(e1@seed, e2@seed), check = FALSE)
})

#' @export
setMethod("Ops", c(e1 = "DuckDBArray", e2 = "atomic"), function(e1, e2) {
    replaceSlots(e1, seed = callGeneric(e1@seed, e2), check = FALSE)
})

#' @export
setMethod("Ops", c(e1 = "atomic", e2 = "DuckDBArray"), function(e1, e2) {
    replaceSlots(e2, seed = callGeneric(e1, e2@seed), check = FALSE)
})

#' @export
setMethod("Ops", c(e1 = "DuckDBArray", e2 = "missing"), function(e1, e2) {
    # Unary operators (e.g., -, +)
    replaceSlots(e1, seed = callGeneric(e1@seed), check = FALSE)
})

#' @export
setMethod("!", "DuckDBArray", function(x) {
    replaceSlots(x, seed = !x@seed, check = FALSE)
})

#' @export
setMethod("Math", "DuckDBArray", function(x) {
    replaceSlots(x, seed = callGeneric(x@seed), check = FALSE)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Numerical methods
###

#' @export
setMethod("is.finite", "DuckDBArray", function(x) {
    replaceSlots(x, seed = callGeneric(x@seed), check = FALSE)
})

#' @export
setMethod("is.infinite", "DuckDBArray", function(x) {
    replaceSlots(x, seed = callGeneric(x@seed), check = FALSE)
})

#' @export
setMethod("is.nan", "DuckDBArray", function(x) {
    replaceSlots(x, seed = callGeneric(x@seed), check = FALSE)
})

#' @export
#' @importFrom DelayedArray sweep
setMethod("sweep", "DuckDBArray",
function(x, MARGIN, STATS, FUN = "/", check.margin = TRUE, ...) {
    replaceSlots(x, seed = callGeneric(x@seed, MARGIN = MARGIN, STATS = STATS,
                                       FUN = FUN, check.margin = check.margin, ...),
                 check = FALSE)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Sparsity methods
###

#' @export
#' @importFrom SparseArray is_nonzero
setMethod("is_nonzero", "DuckDBArray", function(x) {
    replaceSlots(x, seed = callGeneric(x@seed), check = FALSE)
})

#' @export
#' @importFrom SparseArray nzcount
setMethod("nzcount", "DuckDBArray", function(x) {
    callGeneric(x@seed)
})

#' @export
#' @importClassesFrom SparseArray COO_SparseArray
#' @importFrom SparseArray nzvals
setMethod("nzvals", "DuckDBArray", function(x) {
    callGeneric(as(x, "COO_SparseArray"))
})
