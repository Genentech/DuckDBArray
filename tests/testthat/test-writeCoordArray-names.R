# The names of the index columns and of the value column are refused, and not
# renamed, when DuckDB would take two of them for one or a file system would
# take them for something else.

.coord_matrix <- function() {
    m <- matrix(c(1, 0, 2, 3, 0, 4), 2, 3)
    dimnames(m) <- list(1:2, 1:3)
    m
}

test_that("writeCoordArray refuses index and value names that are the same up to case", {
    m <- .coord_matrix()
    out <- tempfile()
    on.exit(unlink(out, recursive = TRUE))
    expect_error(writeCoordArray(m, out, indexcols = c("Row", "row"), datacol = "value"),
                 "'Row', 'row'")
    expect_error(writeCoordArray(m, out, indexcols = c("row", "col"), datacol = "ROW"),
                 "same name, ignoring case")
    # it used to be renamed value_1, silently
    expect_error(writeCoordArray(m, out, indexcols = c("value", "col"), datacol = "value"),
                 "same name")
    expect_false(dir.exists(out))
})

test_that("writeCoordArray refuses index names that cannot be directories", {
    m <- .coord_matrix()
    out <- tempfile()
    on.exit(unlink(out, recursive = TRUE))
    expect_error(writeCoordArray(m, out, indexcols = c("a/b", "col"), datacol = "value"),
                 "become file names")
    expect_error(writeCoordArray(m, out, indexcols = c("a=b", "col"), datacol = "value"),
                 "must not contain '='")
    expect_error(writeCoordArray(m, out, indexcols = c("row", ""), datacol = "value"),
                 "must not be NA or empty")
    expect_error(writeCoordArray(m, out, indexcols = c("row", "col"), datacol = c("a", "b")),
                 "single string")
})

test_that("a value column named like the column of a grid cell is refused", {
    m <- .coord_matrix()
    out <- tempfile()
    on.exit(unlink(out, recursive = TRUE))
    expect_error(writeCoordArray(m, out, indexcols = c("row", "col"), datacol = "ROW_group",
                                 grid = c(1L, 3L)),
                 "same name")
})

test_that("names that are fine are written as they are", {
    m <- .coord_matrix()
    out <- tempfile()
    on.exit(unlink(out, recursive = TRUE))
    writeCoordArray(m, out, indexcols = c("Row index", "col-2"), datacol = "n cells")
    pq <- list.files(out, "[.]parquet$", recursive = TRUE, full.names = TRUE)
    expect_identical(names(arrow::read_parquet(pq[[1L]], as_data_frame = FALSE)$schema),
                     c("Row index", "col-2", "n cells"))
})
