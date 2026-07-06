#' DuckDBTable row / column summarization methods
#'
#' @description
#' Row / column summarization methods for \linkS4class{DuckDBTable} objects.
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
#'   \item \code{\link{DuckDBTable-class}} for the main class
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

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
.marginCounts <-
function(x, value = TRUE, dims = 1, fill = 0, margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    if (value != fill) {
        aggr <- sapply(datacols, function(y) call("countif", call("==", call("(", y), value)),
                       simplify = FALSE)
    } else {
        aggr <- sapply(datacols, function(y)
                       call("+",
                            call("countif", call("==", call("(", y), value)),
                            call("(", call("-", k, call("n")))),
                       simplify = FALSE)
    }
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(sapply(names(aggr), as.name, simplify = FALSE))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowCounts
setMethod("rowCounts", "DuckDBTable",
function(x, value = TRUE, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginCounts(x, value = value, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colCounts
setMethod("colCounts", "DuckDBTable",
function(x, value = TRUE, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginCounts(x, value = value, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
.marginMaxs <- function(x, dims = 1, fill = 0, margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    nfill <- call("(", call("-", k, call("n")))
    aggr <- sapply(datacols, function(y) {
        stat <- call("max", y, na.rm = TRUE)
        call("if", call("==", nfill, 0L), stat, call("greatest", stat, fill))
    }, simplify = FALSE)
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(sapply(names(aggr), as.name, simplify = FALSE))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowMaxs
setMethod("rowMaxs", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMaxs(x, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colMaxs
setMethod("colMaxs", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMaxs(x, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
.marginMeans <- function(x, dims = 1, fill = 0, margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    if (fill == 0) {
        aggr <- sapply(datacols, function(y) call("/", call("sum", y, na.rm = TRUE), k),
                       simplify = FALSE)
    } else {
        aggr <- sapply(datacols, function(y)
                       call("/",
                            call("(",
                                 call("+",
                                      call("sum", y, na.rm = TRUE),
                                      call("*",
                                           fill,
                                           call("(", call("-", k, call("n")))))),
                            k),
                       simplify = FALSE)
    }
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(sapply(names(aggr), as.name, simplify = FALSE))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowMeans
setMethod("rowMeans", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginMeans(x, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colMeans
setMethod("colMeans", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginMeans(x, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
.marginMins <- function(x, dims = 1, fill = 0, margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    nfill <- call("(", call("-", k, call("n")))
    aggr <- sapply(datacols, function(y) {
        stat <- call("min", y, na.rm = TRUE)
        call("if", call("==", nfill, 0L), stat, call("least", stat, fill))
    }, simplify = FALSE)
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(sapply(names(aggr), as.name, simplify = FALSE))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowMins
setMethod("rowMins", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMins(x, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colMins
setMethod("colMins", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginMins(x, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
.marginSums <- function(x, dims = 1, fill = 0, margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    if (fill == 0) {
        aggr <- sapply(datacols, function(y) call("sum", y, na.rm = TRUE),
                       simplify = FALSE)
    } else {
        aggr <- sapply(datacols, function(y)
                       call("+",
                            call("sum", y, na.rm = TRUE),
                            call("*",
                                 fill,
                                 call("(", call("-", k, call("n"))))),
                       simplify = FALSE)
    }
    conn <- summarize(group_by(tblconn(x, select = FALSE), !!!groups), !!!aggr)
    datacols <- as.expression(sapply(names(aggr), as.name, simplify = FALSE))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowSums
setMethod("rowSums", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginSums(x, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colSums
setMethod("colSums", "DuckDBTable", function(x, na.rm = FALSE, dims = 1, fill = 0, ...) {
    .marginSums(x, dims = dims, fill = fill, margin = "col")
})

#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom dplyr group_by left_join n summarize
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom S4Vectors new2
.marginVars <- function(x, dims = 1, fill = 0, margin = c("row", "col")) {
    lst <- .marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; along <- lst$along; k <- lst$k
    datacols <- x@datacols
    nfill <- call("(", call("-", k, call("n")))

    # For fill == 0 (sparse matrices), use single-pass VAR_SAMP optimization
    # Formula: (VAR_SAMP(y) * (n-1) + sum_y² * (1/n - 1/k)) / (k-1)
    # This leverages DuckDB's numerically stable VAR_SAMP and reduces Parquet scans
    if (fill == 0) {
        conn <- tblconn(x, select = FALSE)
        aggr <- sapply(names(datacols), function(nm) {
            y <- datacols[[nm]]
            n <- call("n")
            sum_y <- call("sum", y, na.rm = TRUE)
            var_samp <- call("var_samp", y)

            # (VAR_SAMP * (n-1) + sum_y² * (1/n - 1/k)) / (k-1)
            # COALESCE handles edge case where n=1 (VAR_SAMP returns NULL)
            var_samp_term <- call("*", call("coalesce", var_samp, 0),
                                  call("(", call("-", n, 1L)))
            sum_sq_term <- call("*", call("*", sum_y, sum_y),
                               call("(", call("-", call("/", 1, n), call("/", 1, k))))
            call("/", call("(", call("+", var_samp_term, sum_sq_term)), k - 1L)
        }, simplify = FALSE)

        conn <- summarize(group_by(conn, !!!groups), !!!aggr)
        datacols <- as.expression(sapply(names(aggr), as.name, simplify = FALSE))
        return(new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE))
    }

    # For fill != 0, use two-pass approach (original implementation)
    aggr <- sapply(datacols, function(y)
                   call("/",
                        call("(",
                             call("+",
                                  call("sum", y, na.rm = TRUE),
                                  call("*", fill, nfill))),
                        k),
                   simplify = FALSE)
    conn <- tblconn(x, select = FALSE)
    mean_names <- vapply(names(aggr), function(nm) {
        tail(make.unique(c(colnames(conn), paste0(nm, "_mean")), sep = "_"), 1L)
    }, character(1L))
    names(aggr) <- mean_names

    conn <- left_join(conn, summarize(group_by(conn, !!!groups), !!!aggr), by = names(keycols))

    aggr <- sapply(names(datacols), function(nm) {
        y <- datacols[[nm]]
        y_mean <- as.name(mean_names[[nm]])
        y_mean_agg <- call("any_value", y_mean)
        dev <- call("(", call("-", y, y_mean))
        sum_dev_sq <- call("sum", call("*", dev, dev), na.rm = TRUE)
        fill_dev <- call("(", call("-", fill, y_mean_agg))
        zero_contrib <- call("*", call("(", call("*", fill_dev, fill_dev)), nfill)
        call("/", call("(", call("+", sum_dev_sq, zero_contrib)), k - 1L)
    }, simplify = FALSE)

    conn <- summarize(group_by(conn, !!!groups), !!!aggr)
    datacols <- as.expression(sapply(names(aggr), as.name, simplify = FALSE))
    new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

#' @importFrom DuckDBDataFrame sql_call
.marginSds <- function(x, dims = 1, fill = 0, margin = c("row", "col")) {
    margin <- match.arg(margin)
    ans <- .marginVars(x, dims = dims, fill = fill, margin = margin)
    sql_call(ans, "sqrt")
}

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowSds
setMethod("rowSds", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginSds(x, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colSds
setMethod("colSds", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginSds(x, dims = dims, fill = fill, margin = "col")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics rowVars
setMethod("rowVars", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginVars(x, dims = dims, fill = fill, margin = "row")
})

#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBTable
#' @importFrom MatrixGenerics colVars
setMethod("colVars", "DuckDBTable",
function(x, na.rm = FALSE, dims = 1, fill = 0, ..., useNames = TRUE) {
    .marginVars(x, dims = dims, fill = fill, margin = "col")
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
