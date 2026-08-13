#' Scan a Coordinate Parquet File for keycols
#'
#' @description
#' Introspects a coordinate (COO) Parquet file or dataset and returns a
#' \code{keycols} list, in the exact shape expected by the \code{keycols}
#' argument of \code{\link{DuckDBArray}}/\code{\link{DuckDBMatrix}}: one
#' element per key column, each holding that column's distinct values in
#' sorted order.
#'
#' @param path A single string giving the path to a Parquet file or
#' directory of Parquet files (as accepted by
#' \code{\link[arrow]{open_dataset}}), or an already-open Arrow
#' \code{Dataset}/\code{Table}.
#' @param keycols A character vector of key column names to scan. Each must
#' match a column of \code{path}.
#'
#' @return A named list with one element per name in \code{keycols}, each a
#' vector of that column's distinct values in ascending order.
#'
#' @details
#' Given a COO Parquet file produced by another tool, the key columns'
#' extent isn't known ahead of time the way it is for a file this package
#' wrote itself. \code{scanKeycols} automates the per-column
#' \code{dplyr::distinct()}/\code{dplyr::arrange()} scan needed to recover
#' it, so the result can be passed directly as \code{keycols}.
#'
#' @author Patrick Aboyoun
#'
#' @seealso
#' \code{\link{DuckDBArray}} and \code{\link{DuckDBMatrix}}, whose
#' \code{keycols} argument this function's return value is shaped for.
#'
#' @examples
#' arr_df <- expand.grid(i = 1:4, j = 1:3, k = 1:2)
#' arr_df$value <- rpois(nrow(arr_df), lambda = 2)
#' arr_path <- tempfile(fileext = ".parquet")
#' arrow::write_parquet(arr_df, arr_path)
#'
#' scanKeycols(arr_path, keycols = c("i", "j", "k"))
#'
#' arr <- DuckDBArray(arr_path, datacol = "value",
#'                    keycols = scanKeycols(arr_path, c("i", "j", "k")))
#' dim(arr)
#'
#' @export
#' @importFrom arrow open_dataset
#' @importFrom dplyr across all_of arrange collect distinct pull select
scanKeycols <-
function(path, keycols)
{
    if (!is.character(keycols) || !length(keycols)) {
        stop("'keycols' must be a non-empty character vector")
    }
    ds <- if (is(path, "Dataset") || is(path, "Table")) path else open_dataset(path)
    unknown <- setdiff(keycols, names(ds))
    if (length(unknown)) {
        stop("'keycols' not found in 'path': ",
             paste(unknown, collapse = ", "))
    }
    setNames(
        lapply(keycols, function(cn) {
            scanned <- select(ds, all_of(cn))
            scanned <- distinct(scanned)
            scanned <- arrange(scanned, across(all_of(cn)))
            pull(collect(scanned), 1L)
        }),
        keycols)
}
