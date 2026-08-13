# Tests for scanKeycols()

test_that("scanKeycols recovers keycols from a Parquet file it did not write", {
    arr_df <- expand.grid(i = 1:4, j = 1:3, k = 1:2)
    arr_df$value <- sample.int(100L, nrow(arr_df))
    arr_path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(arr_df, arr_path)

    kc <- scanKeycols(arr_path, keycols = c("i", "j", "k"))
    expect_identical(names(kc), c("i", "j", "k"))
    expect_identical(kc[["i"]], 1:4)
    expect_identical(kc[["j"]], 1:3)
    expect_identical(kc[["k"]], 1:2)

    arr <- DuckDBArray(arr_path, datacol = "value", keycols = kc)
    expected <- array(arr_df$value, dim = c(4L, 3L, 2L),
                      dimnames = list(i = as.character(1:4),
                                      j = as.character(1:3),
                                      k = as.character(1:2)))
    expect_equal(as.array(arr), expected)
})

test_that("scanKeycols returns distinct values in ascending order, not row order", {
    df <- data.frame(idx = c(3L, 1L, 2L, 1L, 3L, 2L), value = 1:6)
    path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(df, path)

    kc <- scanKeycols(path, keycols = "idx")
    expect_identical(kc, list(idx = 1:3))
})

test_that("scanKeycols can scan a subset of the key columns", {
    arr_df <- expand.grid(i = 1:4, j = 1:3, k = 1:2)
    arr_df$value <- sample.int(100L, nrow(arr_df))
    path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(arr_df, path)

    kc <- scanKeycols(path, keycols = c("j", "k"))
    expect_identical(kc, list(j = 1:3, k = 1:2))
})

test_that("scanKeycols accepts an already-open Arrow Dataset", {
    arr_df <- expand.grid(i = 1:2, j = 1:2)
    arr_df$value <- 1:4
    path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(arr_df, path)

    ds <- arrow::open_dataset(path)
    expect_identical(scanKeycols(path, keycols = c("i", "j")),
                     scanKeycols(ds, keycols = c("i", "j")))
})

test_that("scanKeycols errors on unknown keycols", {
    arr_df <- expand.grid(i = 1:2, j = 1:2)
    arr_df$value <- 1:4
    path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(arr_df, path)

    expect_error(scanKeycols(path, keycols = c("i", "nonexistent")),
                 "not found in 'path'")
})

test_that("scanKeycols errors on a non-character or empty keycols", {
    expect_error(scanKeycols(tempfile(), keycols = 1L),
                 "must be a non-empty character vector")
    expect_error(scanKeycols(tempfile(), keycols = character(0)),
                 "must be a non-empty character vector")
})
