# Tests for createDimTables.
# library(testthat); library(DuckDBArray); source("setup.R"); source("test-createDimTables.R")

test_that("createDimTables accepts table objects", {
    dtbls <- createDimTables(Titanic,
                             grid = RegularArrayGrid(dim(Titanic), dim(Titanic)))
    expect_length(dtbls, length(dim(Titanic)))
    expect_named(dtbls, names(dimnames(Titanic)))
})

test_that("createDimTables returns named list for single-cell grid", {
    dtbls <- createDimTables(state.x77,
                             grid = RegularArrayGrid(dim(state.x77), dim(state.x77)))
    expect_length(dtbls, 2L)
    expect_named(dtbls, c("index1", "index2"))
})

test_that("createDimTables matches writeCoordArray grid layout", {
    dtbls <- createDimTables(state.x77, grid = state_grid)
    expect_length(dtbls, 2L)
    expect_true(nrow(dtbls[[1L]]) > 0L)
    expect_true(nrow(dtbls[[2L]]) > 0L)
    expect_true("index1_group" %in% colnames(dtbls[[1L]]))
    expect_true("index2_group" %in% colnames(dtbls[[2L]]))
})

test_that("createDimTables rejects non-array inputs", {
    expect_error(createDimTables(1:10), "array-like")
})
