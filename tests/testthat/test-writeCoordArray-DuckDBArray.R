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

# the DuckDB fast path must type the index columns from the pre-computed
# idxtypes (max_dim-aware) rather than inferring from the remapped indices, so a
# pinned-wide index (and, on a > 2^31 offset, a double index) does not narrow /
# overflow differently from the R write path.
.f4IndexType <- function(path, column) {
    f <- list.files(path, pattern = "parquet$", recursive = TRUE,
                    full.names = TRUE)[1L]
    arrow::ParquetFileReader$create(f)$GetSchema()$
        GetFieldByName(column)$type$ToString()
}

test_that("fast path types the index from max_dim (idxtypes honored, not inferred)", {
    skip_if_not_installed("arrow")
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    path <- .newCoordPath("f4-maxdim")
    # Row axis pinned to 70000 (int32) though its 50 values alone infer uint8.
    writeCoordArray(pqmat, path,
                    grid = RegularArrayGrid(dim(state.x77), c(10L, 4L)),
                    max_dim = c(70000L, 8L))
    expect_identical(.f4IndexType(path, "index1"), "int32")
})

test_that("fast path types the index from a > 2^31 max_dim (no float64/overflow)", {
    skip_if_not_installed("arrow")
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    path <- .newCoordPath("f4-bigdim")
    # Declare axis 2 larger than the 32-bit range.
    writeCoordArray(pqmat, path,
                    grid = RegularArrayGrid(dim(state.x77), c(10L, 4L)),
                    max_dim = c(nrow(state.x77), 3e9 + ncol(state.x77)))
    expect_identical(.f4IndexType(path, "index2"), "uint32")
})
