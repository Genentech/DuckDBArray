# Tests for writeCoordArray,DuckDBArray (SQL COPY fast path).
# library(testthat); library(DuckDBArray); source("setup.R"); source("test-writeCoordArray-DuckDBArray.R")

.newCoordPath <- function(label) {
    path <- file.path(tempdir(), paste0("ddb_", label, "_", sample.int(.Machine$integer.max, 1L)))
    dir.create(path, recursive = TRUE)
    path
}

.rebuildMatrixFromCoord <- function(tbl, template) {
    mat <- matrix(0, nrow = nrow(template), ncol = ncol(template),
                  dimnames = dimnames(template))
    if (nrow(tbl) > 0L) {
        storage.mode(mat) <- typeof(template)
        mat[cbind(tbl$index1, tbl$index2)] <- tbl$value
    }
    mat
}

test_that("writeCoordArray,DuckDBArray round-trips a matrix (single-cell grid)", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    path <- .newCoordPath("single")
    grid <- RegularArrayGrid(dim(state.x77), dim(state.x77))
    writeCoordArray(pqmat, path, grid = grid)

    expect_true(dir.exists(path))
    expect_gte(length(list.files(path, pattern = "\\.parquet$", recursive = TRUE)), 1L)
    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(.rebuildMatrixFromCoord(tbl, state.x77), state.x77)
})

test_that("writeCoordArray,DuckDBArray round-trips a matrix (multi-cell grid)", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    path <- .newCoordPath("multi")
    grid <- RegularArrayGrid(dim(state.x77), c(10L, 4L))
    writeCoordArray(pqmat, path, grid = grid)

    row_dirs <- list.dirs(path, recursive = FALSE, full.names = FALSE)
    expect_equal(sort(row_dirs), paste0("index1_group=", 1:5))
    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_true(all(c("index1", "index2", "value",
                      "index1_group", "index2_group") %in% names(tbl)))
    expect_equal(.rebuildMatrixFromCoord(tbl, state.x77), state.x77)
})

test_that("writeCoordArray,DuckDBArray respects dimtbls during partitioned write", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)
    path <- .newCoordPath("dimtbls")
    grid <- RegularArrayGrid(dim(state.x77), c(10L, 4L))
    writeCoordArray(pqmat, path, grid = grid)

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(.rebuildMatrixFromCoord(tbl, state.x77), state.x77)
})

test_that("writeCoordArray,DuckDBArray infers schema from DuckDB column types", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    path <- .newCoordPath("schema_probe")
    writeCoordArray(pqmat, path,
                    grid = RegularArrayGrid(dim(state.x77), dim(state.x77)))
    types <- vapply(list.files(path, pattern = "\\.parquet$", full.names = TRUE),
                    function(f) {
                        sch <- arrow::schema(arrow::read_parquet(f, as_data_frame = FALSE))
                        sch$GetFieldByName("value")$type$ToString()
                    }, character(1L))
    expect_true(all(types == "double"))
})
