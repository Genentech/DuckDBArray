#' DuckDBTable row / column summarization methods
#'
#' @description
#' Row / column summarization methods for
#' \link[DuckDBDataFrame]{DuckDBTable-class} objects.
#'
#' @section Row / Column Summarization Methods:
#' In the code snippets below, \code{x} is a DuckDBTable object:
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
#' @section Sweep Method:
#' In the code snippets below, \code{x} is a DuckDBTable object:
#' \describe{
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
#' @return
#' Method return types are documented in the sections above.
#'
#' @author Patrick Aboyoun
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[DuckDBDataFrame]{DuckDBTable}} for the main class
#'   \item \code{\link[S4Vectors]{RectangularData}} for the base class
#' }
#'
#' @aliases rowCounts,DuckDBTable-method
#' @aliases colCounts,DuckDBTable-method
#' @aliases rowMaxs,DuckDBTable-method
#' @aliases colMaxs,DuckDBTable-method
#' @aliases rowMeans,DuckDBTable-method
#' @aliases colMeans,DuckDBTable-method
#' @aliases rowMins,DuckDBTable-method
#' @aliases colMins,DuckDBTable-method
#' @aliases rowSums,DuckDBTable-method
#' @aliases colSums,DuckDBTable-method
#' @aliases rowSds,DuckDBTable-method
#' @aliases colSds,DuckDBTable-method
#' @aliases rowVars,DuckDBTable-method
#' @aliases colVars,DuckDBTable-method
#' @aliases sweep,DuckDBTable-method
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
#' tbl <- DuckDBTable(tf, datacols = "fate",
#'                    keycols = c("Class", "Sex", "Age", "Survived"))
#' rowSums(tbl)
#' colMeans(tbl)
#'
#' @name DuckDBTable-matrixStats
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### matrixStats methods
###

#' @importFrom S4Vectors isSingleNumber
.marginSetup <- function(x, dims = 1, margin = c("row", "col")) {
    margin <- match.arg(margin)
    nk <- nkey(x)
    if (nk < 2L) {
        stop("'x' must be an array of at least two dimensions")
    }
    if (!isSingleNumber(dims) || dims < 1L || dims >= nk) {
        stop("invalid 'dims'")
    }
    if (length(x@datacols) != 1L) {
        stop("requires a single datacols")
    }
    if (margin == "row") {
        keycols <- head(x@keycols, dims)
        along <- tail(x@keycols, -dims)
    } else {
        keycols <- tail(x@keycols, -dims)
        along <- head(x@keycols, dims)
    }
    k <- prod(lengths(along, use.names = FALSE))
    along <- lapply(names(along), as.name)
    groups <- lapply(names(keycols), as.name)
    list(keycols = keycols, groups = groups, along = along, k = k)
}

.na_rm_guard <- function(expr, y, na.rm) {
    if (isTRUE(na.rm)) {
        return(expr)
    }
    has_na <- call(">", call("sum", call("as.integer", call("is.na", y)),
                             na.rm = TRUE), 0L)
    call("ifelse", has_na, NA_real_, expr)
}

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
#' @importFrom stats setNames
.marginCounts <-
function(x, value = TRUE, na.rm = FALSE, dims = 1, fill = 0,
         margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    if (value != fill) {
        aggr <- lapply(datacols, function(y)
                       .na_rm_guard(call("countif", call("==", call("(", y), value)),
                                    y, na.rm))
    } else {
        aggr <- lapply(datacols, function(y)
                       .na_rm_guard(
                            call("+",
                                 call("countif", call("==", call("(", y), value)),
                                 call("(", call("-", k, call("n")))),
                            y, na.rm))
    }
    aggr <- setNames(aggr, names(datacols))
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(setNames(lapply(names(aggr), as.name), names(aggr)))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowCounts
setMethod("rowCounts", "DuckDBTable",
function(x, value = TRUE, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginCounts(x, value = value, na.rm = na.rm, dims = dims, fill = fill,
                  margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colCounts
setMethod("colCounts", "DuckDBTable",
function(x, value = TRUE, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginCounts(x, value = value, na.rm = na.rm, dims = dims, fill = fill,
                  margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
#' @importFrom stats setNames
.marginMaxs <- function(x, na.rm = FALSE, dims = 1, fill = 0,
                        margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    nfill <- call("(", call("-", k, call("n")))
    aggr <- setNames(lapply(datacols, function(y) {
        stat <- call("max", y, na.rm = TRUE)
        expr <- call("if", call("==", nfill, 0L), stat, call("greatest", stat, fill))
        .na_rm_guard(expr, y, na.rm)
    }), names(datacols))
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(setNames(lapply(names(aggr), as.name), names(aggr)))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowMaxs
setMethod("rowMaxs", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMaxs(x, na.rm = na.rm, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colMaxs
setMethod("colMaxs", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMaxs(x, na.rm = na.rm, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
#' @importFrom stats setNames
.marginMeans <- function(x, na.rm = FALSE, dims = 1, fill = 0,
                         margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    if (fill == 0) {
        aggr <- lapply(datacols, function(y)
                       .na_rm_guard(call("/", call("sum", y, na.rm = TRUE), k),
                                    y, na.rm))
    } else {
        aggr <- lapply(datacols, function(y)
                       .na_rm_guard(
                            call("/",
                                 call("(",
                                      call("+",
                                           call("sum", y, na.rm = TRUE),
                                           call("*",
                                                fill,
                                                call("(", call("-", k, call("n")))))),
                                 k),
                            y, na.rm))
    }
    aggr <- setNames(aggr, names(datacols))
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(setNames(lapply(names(aggr), as.name), names(aggr)))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowMeans
setMethod("rowMeans", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginMeans(x, na.rm = na.rm, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colMeans
setMethod("colMeans", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginMeans(x, na.rm = na.rm, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
#' @importFrom stats setNames
.marginMins <- function(x, na.rm = FALSE, dims = 1, fill = 0,
                        margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    nfill <- call("(", call("-", k, call("n")))
    aggr <- setNames(lapply(datacols, function(y) {
        stat <- call("min", y, na.rm = TRUE)
        expr <- call("if", call("==", nfill, 0L), stat, call("least", stat, fill))
        .na_rm_guard(expr, y, na.rm)
    }), names(datacols))
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(setNames(lapply(names(aggr), as.name), names(aggr)))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowMins
setMethod("rowMins", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMins(x, na.rm = na.rm, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colMins
setMethod("colMins", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMins(x, na.rm = na.rm, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
#' @importFrom stats setNames
.marginSums <- function(x, na.rm = FALSE, dims = 1, fill = 0,
                        margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    if (fill == 0) {
        aggr <- lapply(datacols, function(y)
                       .na_rm_guard(call("sum", y, na.rm = TRUE), y, na.rm))
    } else {
        aggr <- lapply(datacols, function(y)
                       .na_rm_guard(
                            call("+",
                                 call("sum", y, na.rm = TRUE),
                                 call("*",
                                      fill,
                                      call("(", call("-", k, call("n"))))),
                            y, na.rm))
    }
    aggr <- setNames(aggr, names(datacols))
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(setNames(lapply(names(aggr), as.name), names(aggr)))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowSums
setMethod("rowSums", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginSums(x, na.rm = na.rm, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colSums
setMethod("colSums", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginSums(x, na.rm = na.rm, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
#' @importFrom stats setNames
.marginVars <- function(x, na.rm = FALSE, dims = 1, fill = 0,
                        margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols

    # Single-pass VAR_SAMP optimization, for any fill value, via the law of
    # total variance. The native aggregates n = COUNT(*) (stored rows),
    # sum_y = SUM(y), and S2 = COALESCE(VAR_SAMP(y), 0) are computed once per
    # group in ONE query; the k - n implicit fill-valued elements (positions
    # that are not stored rows) are folded in analytically instead of via a
    # left_join back to the underlying data followed by a second GROUP BY.
    #
    # Writing ybar = sum_y / n (mean of the n stored values) and
    # mu = (sum_y + fill * (k - n)) / k (mean over all k elements), the decomposition is
    #   sum_sq_dev_full =
    #       (n - 1) * S2 + n * (ybar - mu)^2 + (k - n) * (fill - mu)^2
    #   var = sum_sq_dev_full / (k - 1)
    # Both (ybar - mu) and (fill - mu) collapse to exact multiples of the
    # same quantity d = ybar - fill:
    #   ybar - mu = (k - n) / k * d,   fill - mu = -(n / k) * d
    # so n * (ybar-mu)^2 + (k-n) * (fill-mu)^2 == d^2 * n * (k-n) / k. This
    # avoids ever forming mu itself, which (for fill close to the true mean)
    # would need the much larger quantities sum_y/n and fill * (k-n) / k to
    # nearly cancel; canceling ybar directly against fill in d is the one
    # cancellation this computation cannot avoid (it reflects a genuine fact
    # about the data), and is strictly more precise than going through mu.
    # When fill == 0, d == ybar and this reduces algebraically to the
    # previous closed form
    # (VAR_SAMP(y) * (n-1) + sum_y^2 * (1/n - 1/k)) / (k-1); see
    # test-DuckDBTable-matrixStats.R for the regression check.
    conn <- tblconn(x, select = FALSE)
    aggr <- setNames(lapply(names(datacols), function(nm) {
        y <- datacols[[nm]]
        n <- call("n")
        sum_y <- call("sum", y, na.rm = TRUE)
        s2 <- call("coalesce", call("var_samp", y), 0)
        nfill <- call("(", call("-", k, n))

        d <- call("(", call("-", call("(", call("/", sum_y, n)), fill))
        fill_term <- call("/", call("*", call("*", d, d), call("*", n, nfill)), k)
        s2_term <- call("*", call("(", call("-", n, 1L)), s2)
        sum_sq_dev_full <- call("+", s2_term, fill_term)

        expr <- call("/", call("(", sum_sq_dev_full), k - 1L)
        .na_rm_guard(expr, y, na.rm)
    }), names(datacols))

    conn <- summarize(group_by(conn, !!!groups), !!!aggr)
    datacols <- as.expression(setNames(lapply(names(aggr), as.name), names(aggr)))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @importFrom DuckDBDataFrame sql_call
.marginSds <- function(x, na.rm = FALSE, dims = 1, fill = 0,
                       margin = c("row", "col")) {
    margin <- match.arg(margin)
    ans <- .marginVars(x, na.rm = na.rm, dims = dims, fill = fill, margin = margin)
    sql_call(ans, "sqrt")
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowSds
setMethod("rowSds", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginSds(x, na.rm = na.rm, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colSds
setMethod("colSds", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginSds(x, na.rm = na.rm, dims = dims, fill = fill, margin = "col")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowVars
setMethod("rowVars", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginVars(x, na.rm = na.rm, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colVars
setMethod("colVars", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginVars(x, na.rm = na.rm, dims = dims, fill = fill, margin = "col")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### sweep method
###

#' @export
#' @importFrom DelayedArray sweep
#' @importFrom dplyr left_join
#' @importFrom S4Vectors endoapply isSingleNumber
#' @importFrom stats setNames
setMethod("sweep", "DuckDBTable",
function(x, MARGIN, STATS, FUN = "/", check.margin = TRUE, ...) {
    nk <- nkey(x)
    if (nk < 2L) {
        stop("'x' must be an array of at least two dimensions")
    }
    if (!isSingleNumber(MARGIN) || MARGIN < 1L || MARGIN > nk) {
        stop("'MARGIN' must be between 1 and ", nk)
    }
    if (length(x@datacols) != 1L) {
        stop("sweep requires a single datacols")
    }

    levels <- x@keycols[[MARGIN]]
    if (length(STATS) != length(levels)) {
        stop("length of 'STATS' (", length(STATS), ") must equal the extent ",
             "of dimension ", MARGIN, " (", length(levels), ")")
    }

    key <- names(x@keycols)[MARGIN]
    conn <- tblconn(x, select = FALSE)
    stats <- tail(make.unique(c(colnames(conn), "__sweep_stats__"), sep = "_"), 1L)
    df <- setNames(data.frame(levels, STATS), c(key, stats))

    conn <- left_join(conn, df, by = key, copy = TRUE)

    datacols <- endoapply(x@datacols, function(y) call(FUN, call("(", y), as.name(stats)))
    replaceSlots(x, conn = conn, datacols = datacols, check = FALSE)
})
