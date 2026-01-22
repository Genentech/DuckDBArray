#' DuckDBMatrix utility methods
#'
#' @description
#' Matrix multiplication methods for \linkS4class{DuckDBMatrix} objects.
#'
#' @section Matrix Multiplication Methods:
#' In the code snippets below, \code{x} is a DuckDBMatrix object and \code{y}
#' is an ordinary matrix or vector:
#' \describe{
#'   \item{\code{x \%*\% y}:}{
#'     Matrix multiplication via SQL aggregation.
#'   }
#'   \item{\code{crossprod(x, y)}:}{
#'     Cross-product, equivalent to \code{t(x) \%*\% y}.
#'   }
#'   \item{\code{tcrossprod(x, y)}:}{
#'     Transposed cross-product, equivalent to \code{x \%*\% t(y)}.
#'   }
#'   \item{\code{crossprod(x)}:}{
#'     Self cross-product via SQL self-join.
#'   }
#'   \item{\code{tcrossprod(x)}:}{
#'     Self transposed cross-product via SQL self-join.
#'   }
#' }
#'
#' @author Patrick Aboyoun
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{DuckDBMatrix-class}} for the main class
#'   \item \code{\link{DuckDBArray-utils}} for array utilities
#' }
#'
#' @aliases
#' %*%,DuckDBMatrix,numeric-method
#' %*%,numeric,DuckDBMatrix-method
#' crossprod,DuckDBMatrix,missing-method
#' crossprod,DuckDBMatrix,numeric-method
#' crossprod,numeric,DuckDBMatrix-method
#' tcrossprod,DuckDBMatrix,missing-method
#' tcrossprod,DuckDBMatrix,numeric-method
#' tcrossprod,numeric,DuckDBMatrix-method
#'
#' @include DuckDBMatrix-class.R
#'
#' @keywords utilities methods
#'
#' @name DuckDBMatrix-utils
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helper functions for matrix-vector multiplication
###

.get_matrix_keycols <- function(table) {
    keycols <- table@keycols
    lens <- lengths(keycols)
    dim_idx <- which(lens > 1L)
    if (length(dim_idx) != 2L) {
        stop("DuckDBMatrix must have exactly 2 dimensions with length > 1")
    }
    list(row_key = names(keycols)[dim_idx[1L]],
         col_key = names(keycols)[dim_idx[2L]],
         row_keycol = keycols[[dim_idx[1L]]],
         col_keycol = keycols[[dim_idx[2L]]])
}

#' @importFrom dplyr group_by left_join mutate summarize
#' @importFrom stats setNames
#' @importFrom DuckDBDataFrame dimtbls tblconn
.matmult_DuckDBMatrix_vector <- function(x, y) {
    if (x@seed@fill != 0) {
        stop("must be a zero-filled array")
    }
    table <- x@seed@table
    keys <- .get_matrix_keycols(table)
    row_key <- keys$row_key
    col_key <- keys$col_key
    row_keycol <- keys$row_keycol
    col_keycol <- keys$col_keycol
    datacol <- table@datacols[[1L]]
    datacol_name <- names(table@datacols)
    if (length(y) != length(col_keycol)) {
        stop("non-conformable arguments: ncol(x) = ", length(col_keycol),
             ", length(y) = ", length(y))
    }
    y_df <- data.frame(col_idx = col_keycol, y_value = as.numeric(y),
                       stringsAsFactors = FALSE)
    names(y_df)[1L] <- col_key
    conn <- tblconn(table, select = FALSE)
    product_expr <- call("*", call("(", datacol), as.name("y_value"))
    sum_expr <- call("sum", product_expr, na.rm = TRUE)
    aggr <- setNames(list(sum_expr), datacol_name)
    result_conn <- conn |>
        left_join(y_df, by = col_key, copy = TRUE) |>
        group_by(!!as.name(row_key)) |>
        summarize(!!!aggr, .groups = "drop") |>
        mutate(`__col__` = 1L)
    # Build keycols for result matrix (n x 1)
    rn <- names(row_keycol)
    result_keycols <- setNames(
        list(setNames(row_keycol, rn), setNames(1L, NULL)),
        c(row_key, "__col__")
    )
    # Propagate row dimtbl from input
    input_dimtbls <- dimtbls(table, drop = FALSE)
    result_dimtbls <- input_dimtbls[["dimtbls"]][names(input_dimtbls[["dimtbls"]]) == row_key]
    # Update underlying table and x
    datacols <- as.expression(setNames(list(as.name(datacol_name)), datacol_name))
    table <- replaceSlots(table, conn = result_conn, datacols = datacols,
                          keycols = result_keycols, dimtbls = result_dimtbls,
                          check = FALSE)
    x@seed@table <- table
    x
}

#' @importFrom dplyr group_by left_join mutate summarize
#' @importFrom stats setNames
#' @importFrom DuckDBDataFrame dimtbls tblconn
.matmult_vector_DuckDBMatrix <- function(y, x) {
    if (x@seed@fill != 0) {
        stop("must be a zero-filled array")
    }
    table <- x@seed@table
    keys <- .get_matrix_keycols(table)
    row_key <- keys$row_key
    col_key <- keys$col_key
    row_keycol <- keys$row_keycol
    col_keycol <- keys$col_keycol
    datacol <- table@datacols[[1L]]
    datacol_name <- names(table@datacols)
    if (length(y) != length(row_keycol)) {
        stop("non-conformable arguments: length(y) = ", length(y),
             ", nrow(x) = ", length(row_keycol))
    }
    y_df <- data.frame(row_idx = row_keycol, y_value = as.numeric(y),
                       stringsAsFactors = FALSE)
    names(y_df)[1L] <- row_key
    conn <- tblconn(table, select = FALSE)
    product_expr <- call("*", as.name("y_value"), call("(", datacol))
    sum_expr <- call("sum", product_expr, na.rm = TRUE)
    aggr <- setNames(list(sum_expr), datacol_name)
    result_conn <- conn |>
        left_join(y_df, by = row_key, copy = TRUE) |>
        group_by(!!as.name(col_key)) |>
        summarize(!!!aggr, .groups = "drop") |>
        mutate(`__row__` = 1L)
    # Build keycols for result matrix (1 x m)
    cn <- names(col_keycol)
    result_keycols <- setNames(
        list(setNames(1L, NULL), setNames(col_keycol, cn)),
        c("__row__", col_key)
    )
    # Propagate col dimtbl from input
    input_dimtbls <- dimtbls(table, drop = FALSE)
    result_dimtbls <- input_dimtbls[["dimtbls"]][names(input_dimtbls[["dimtbls"]]) == col_key]
    # Update underlying table and x
    datacols <- as.expression(setNames(list(as.name(datacol_name)), datacol_name))
    table <- replaceSlots(table, conn = result_conn, datacols = datacols,
                          keycols = result_keycols, dimtbls = result_dimtbls,
                          check = FALSE)
    x@seed@table <- table
    x
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helper functions for self-crossproducts via SQL self-join
###

#' @importFrom dplyr group_by inner_join select summarize
#' @importFrom stats setNames
#' @importFrom DuckDBDataFrame dimtbls tblconn
.tcrossprod_self_DuckDBMatrix <- function(x) {
    if (x@seed@fill != 0) {
        stop("must be a zero-filled array")
    }
    table <- x@seed@table
    keys <- .get_matrix_keycols(table)
    row_key <- keys$row_key
    col_key <- keys$col_key
    row_keycol <- keys$row_keycol
    datacol_name <- names(table@datacols)
    conn <- tblconn(table)
    a_row <- paste0(row_key, "_a")
    b_row <- paste0(row_key, "_b")
    select_a <- setNames(lapply(c(row_key, col_key, datacol_name), as.name),
                         c(a_row, col_key, "value_a"))
    select_b <- setNames(lapply(c(row_key, col_key, datacol_name), as.name),
                         c(b_row, col_key, "value_b"))
    conn_a <- select(conn, !!!select_a)
    conn_b <- select(conn, !!!select_b)
    aggr <- setNames(list(call("sum", call("*", as.name("value_a"), as.name("value_b")),
                               na.rm = TRUE)), datacol_name)
    result_conn <- conn_a |>
        inner_join(conn_b, by = col_key) |>
        group_by(!!as.name(a_row), !!as.name(b_row)) |>
        summarize(!!!aggr, .groups = "drop")
    # Build keycols for the result matrix (n x n where n = nrow(x))
    rn <- names(row_keycol)
    result_keycols <- setNames(
        list(setNames(row_keycol, rn), setNames(row_keycol, rn)),
        c(a_row, b_row)
    )
    # Propagate row dimtbl from input for both dimensions
    input_dimtbls <- dimtbls(table, drop = FALSE)
    row_dimtbl <- input_dimtbls[["dimtbls"]][names(input_dimtbls[["dimtbls"]]) == row_key]
    if (length(row_dimtbl) > 0L) {
        result_dimtbls <- setNames(c(row_dimtbl, row_dimtbl), c(a_row, b_row))
    } else {
        result_dimtbls <- NULL
    }
    # Update underlying table and x
    datacols <- as.expression(setNames(list(as.name(datacol_name)), datacol_name))
    table <- replaceSlots(table, conn = result_conn, datacols = datacols,
                          keycols = result_keycols, dimtbls = result_dimtbls,
                          check = FALSE)
    x@seed@table <- table
    x
}

#' @importFrom dplyr group_by inner_join select summarize
#' @importFrom stats setNames
#' @importFrom DuckDBDataFrame dimtbls tblconn
.crossprod_self_DuckDBMatrix <- function(x) {
    if (x@seed@fill != 0) {
        stop("must be a zero-filled array")
    }
    table <- x@seed@table
    keys <- .get_matrix_keycols(table)
    row_key <- keys$row_key
    col_key <- keys$col_key
    col_keycol <- keys$col_keycol
    datacol_name <- names(table@datacols)
    conn <- tblconn(table)
    a_col <- paste0(col_key, "_a")
    b_col <- paste0(col_key, "_b")
    select_a <- setNames(lapply(c(row_key, col_key, datacol_name), as.name),
                         c(row_key, a_col, "value_a"))
    select_b <- setNames(lapply(c(row_key, col_key, datacol_name), as.name),
                         c(row_key, b_col, "value_b"))
    conn_a <- select(conn, !!!select_a)
    conn_b <- select(conn, !!!select_b)
    aggr <- setNames(list(call("sum", call("*", as.name("value_a"), as.name("value_b")),
                               na.rm = TRUE)), datacol_name)
    result_conn <- conn_a |>
        inner_join(conn_b, by = row_key) |>
        group_by(!!as.name(a_col), !!as.name(b_col)) |>
        summarize(!!!aggr, .groups = "drop")
    # Build keycols for the result matrix (n x n where n = ncol(x))
    cn <- names(col_keycol)
    result_keycols <- setNames(
        list(setNames(col_keycol, cn), setNames(col_keycol, cn)),
        c(a_col, b_col)
    )
    # Propagate col dimtbl from input for both dimensions
    input_dimtbls <- dimtbls(table, drop = FALSE)
    col_dimtbl <- input_dimtbls[["dimtbls"]][names(input_dimtbls[["dimtbls"]]) == col_key]
    if (length(col_dimtbl) > 0L) {
        result_dimtbls <- setNames(c(col_dimtbl, col_dimtbl), c(a_col, b_col))
    } else {
        result_dimtbls <- NULL
    }
    # Update underlying table and x
    datacols <- as.expression(setNames(list(as.name(datacol_name)), datacol_name))
    table <- replaceSlots(table, conn = result_conn, datacols = datacols,
                          keycols = result_keycols, dimtbls = result_dimtbls,
                          check = FALSE)
    x@seed@table <- table
    x
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %*% methods
###

#' @export
setMethod("%*%", c("DuckDBMatrix", "numeric"), function(x, y) {
    if (is.matrix(y)) {
        if (ncol(y) == 1L) {
            .matmult_DuckDBMatrix_vector(x, as.vector(y))
        } else {
            callNextMethod()
        }
    } else {
        .matmult_DuckDBMatrix_vector(x, y)
    }
})

#' @export
setMethod("%*%", c("numeric", "DuckDBMatrix"), function(x, y) {
    if (is.matrix(x)) {
        if (nrow(x) == 1L) {
            .matmult_vector_DuckDBMatrix(as.vector(x), y)
        } else {
            callNextMethod()
        }
    } else {
        .matmult_vector_DuckDBMatrix(x, y)
    }
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### crossprod methods
###

#' @export
#' @importFrom Matrix crossprod
setMethod("crossprod", c("DuckDBMatrix", "missing"), function(x, y = NULL) {
    .crossprod_self_DuckDBMatrix(x)
})

#' @export
#' @importFrom Matrix crossprod
setMethod("crossprod", c("DuckDBMatrix", "numeric"), function(x, y = NULL) {
    if (is.null(y)) {
        .crossprod_self_DuckDBMatrix(x)
    } else if (is.matrix(y)) {
        if (ncol(y) == 1L) {
            t(.matmult_vector_DuckDBMatrix(as.vector(y), x))
        } else {
            callNextMethod()
        }
    } else {
        t(.matmult_vector_DuckDBMatrix(y, x))
    }
})

#' @export
#' @importFrom Matrix crossprod
setMethod("crossprod", c("numeric", "DuckDBMatrix"), function(x, y = NULL) {
    if (is.null(y)) {
        callNextMethod()
    } else if (is.matrix(x)) {
        if (nrow(x) == 1L) {
            .matmult_DuckDBMatrix_vector(y, as.vector(x))
        } else {
            callNextMethod()
        }
    } else {
        .matmult_vector_DuckDBMatrix(x, y)
    }
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### tcrossprod methods
###

#' @export
#' @importFrom Matrix tcrossprod
setMethod("tcrossprod", c("DuckDBMatrix", "missing"), function(x, y = NULL) {
    .tcrossprod_self_DuckDBMatrix(x)
})

#' @export
#' @importFrom Matrix tcrossprod
setMethod("tcrossprod", c("DuckDBMatrix", "numeric"), function(x, y = NULL) {
    if (is.null(y)) {
        .tcrossprod_self_DuckDBMatrix(x)
    } else if (is.matrix(y)) {
        if (nrow(y) == 1L) {
            .matmult_DuckDBMatrix_vector(x, as.vector(y))
        } else {
            callNextMethod()
        }
    } else {
        .matmult_DuckDBMatrix_vector(x, y)
    }
})

#' @export
#' @importFrom Matrix tcrossprod
setMethod("tcrossprod", c("numeric", "DuckDBMatrix"), function(x, y = NULL) {
    if (is.null(y)) {
        callNextMethod()
    } else if (is.matrix(x)) {
        callNextMethod()
    } else {
        if (nrow(y) == 1L) {
            outer(x, as.vector(y))
        } else {
            callNextMethod()
        }
    }
})
