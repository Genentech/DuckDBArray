#' Common operations on DuckDBArraySeed objects
#'
#' @description
#' Common operations on \linkS4class{DuckDBArraySeed} objects.
#'
#' @section Group Generics:
#' DuckDBArraySeed objects have support for S4 group generic functionality:
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
#' In the code snippets below, \code{x} is a DuckDBArraySeed object:
#' \describe{
#'   \item{\code{is.finite(x)}:}{
#'     Returns a DuckDBArraySeed containing logicals that indicate which values
#'     are finite.
#'   }
#'   \item{\code{is.infinite(x)}:}{
#'     Returns a DuckDBArraySeed containing logicals that indicate which values
#'     are infinite.
#'   }
#'   \item{\code{is.nan(x)}:}{
#'     Returns a DuckDBArraySeed containing logicals that indicate which values
#'     are Not a Number.
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
#' In the code snippets below, \code{x} is a DuckDBArraySeed object:
#' \describe{
#'   \item{\code{is_nonzero(x)}:}{
#'     Returns a DuckDBArraySeed containing logicals that indicate if the
#'     values in each of the columns of \code{x} are non-zero.
#'   }
#'   \item{\code{nzcount(x)}:}{
#'     Returns the total number of non-zero values.
#'   }
#'   \item{\code{is_sparse(x)}:}{
#'     Returns \code{TRUE} since data are stored in a sparse array representation.
#'   }
#' }
#'
#' @return
#' Method return types are documented in the sections above.
#'
#' @author Patrick Aboyoun
#'
#' @aliases Ops,DuckDBArraySeed,DuckDBArraySeed-method
#' @aliases Ops,DuckDBArraySeed,atomic-method
#' @aliases Ops,atomic,DuckDBArraySeed-method
#' @aliases Ops,DuckDBArraySeed,missing-method
#' @aliases !,DuckDBArraySeed-method
#' @aliases Math,DuckDBArraySeed-method
#'
#' @aliases is.finite,DuckDBArraySeed-method
#' @aliases is.infinite,DuckDBArraySeed-method
#' @aliases is.nan,DuckDBArraySeed-method
#' @aliases sweep,DuckDBArraySeed-method
#'
#' @aliases is_nonzero,DuckDBArraySeed-method
#' @aliases nzcount,DuckDBArraySeed-method
#' @aliases is_sparse,DuckDBArraySeed-method
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{DuckDBArraySeed-class}} for the internal seed class
#'   \item \code{\link{DuckDBArray-class}} for the main class
#'   \item \code{\link{DuckDBArray-utils}} for the main class utilities
#'   \item \code{\link[S4Arrays]{Array}} for the base class
#' }
#'
#' @include DuckDBArraySeed-class.R
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
#' is_sparse(seed)
#' nzcount(seed)
#' seed + 1L
#'
#' @name DuckDBArraySeed-utils
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

#' @export
setMethod("Ops", c(e1 = "DuckDBArraySeed", e2 = "DuckDBArraySeed"), function(e1, e2) {
    if (!isTRUE(all.equal(e1@table, e2@table)) || !identical(e1@drop, e2@drop)) {
        stop("can only perform binary operations with compatible objects")
    }
    replaceSlots(e1, table = callGeneric(e1@table, e2@table), fill = callGeneric(e1@fill, e2@fill), check = FALSE)
})

#' @export
setMethod("Ops", c(e1 = "DuckDBArraySeed", e2 = "atomic"), function(e1, e2) {
    replaceSlots(e1, table = callGeneric(e1@table, e2), fill = callGeneric(e1@fill, e2), check = FALSE)
})

#' @export
setMethod("Ops", c(e1 = "atomic", e2 = "DuckDBArraySeed"), function(e1, e2) {
    replaceSlots(e2, table = callGeneric(e1, e2@table), fill = callGeneric(e1, e2@fill), check = FALSE)
})

#' @export
setMethod("Ops", c(e1 = "DuckDBArraySeed", e2 = "missing"), function(e1, e2) {
    # Unary operators (e.g., -, +)
    replaceSlots(e1, table = callGeneric(e1@table), fill = callGeneric(e1@fill), check = FALSE)
})

#' @export
setMethod("!", "DuckDBArraySeed", function(x) {
    replaceSlots(x, table = !x@table, fill = !x@fill, check = FALSE)
})

#' @export
setMethod("Math", "DuckDBArraySeed", function(x) {
    replaceSlots(x, table = callGeneric(x@table), fill = callGeneric(x@fill), check = FALSE)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Numerical methods
###

#' @export
setMethod("is.finite", "DuckDBArraySeed", function(x) {
    replaceSlots(x, table = callGeneric(x@table), fill = callGeneric(x@fill), check = FALSE)
})

#' @export
setMethod("is.infinite", "DuckDBArraySeed", function(x) {
    replaceSlots(x, table = callGeneric(x@table), fill = callGeneric(x@fill), check = FALSE)
})

#' @export
setMethod("is.nan", "DuckDBArraySeed", function(x) {
    replaceSlots(x, table = callGeneric(x@table), fill = callGeneric(x@fill), check = FALSE)
})

#' @export
#' @importFrom DelayedArray sweep
setMethod("sweep", "DuckDBArraySeed",
function(x, MARGIN, STATS, FUN = "/", check.margin = TRUE, ...) {
    FUN <- match.arg(FUN, c("*", "/", "%/%", "%%"))
    if (x@fill != 0) {
        stop("must be a zero-filled array")
    }
    replaceSlots(x, table = callGeneric(x@table, MARGIN = MARGIN, STATS = STATS,
                                        FUN = FUN, check.margin = check.margin, ...),
                 check = FALSE)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Sparsity methods
###

#' @export
#' @importFrom SparseArray is_nonzero
setMethod("is_nonzero", "DuckDBArraySeed", function(x) {
    replaceSlots(x, table = callGeneric(x@table), fill = callGeneric(x@fill), check = FALSE)
})

#' @export
#' @importFrom SparseArray nzcount
setMethod("nzcount", "DuckDBArraySeed", function(x) {
    callGeneric(x@table)
})

#' @export
#' @importFrom S4Arrays is_sparse
setMethod("is_sparse", "DuckDBArraySeed", function(x) {
    callGeneric(x@table)
})
