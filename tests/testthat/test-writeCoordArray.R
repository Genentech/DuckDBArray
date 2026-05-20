# Tests for writeCoordArray.
#
# Organized in five groups, priority given to corruption prevention:
#   1. Basic round-trip correctness (single-cell, multi-cell, 3D, sparse).
#   2. Argument validation.
#   3. Schema correctness (arrowtype + max_dim respected end-to-end).
#   4. Corruption prevention in append mode (schema drift, partition
#      collision, non-hive target, missing fields). Each of these asserts
#      that a failed append leaves the existing dataset byte-for-byte
#      unchanged -- silent corruption at scale is the single hardest class
#      of bug to discover after the fact.
#   5. Corruption prevention in non-append mode (no silent overwrite).
#
# library(testthat); library(DuckDBArray); source("setup.R"); source("test-writeCoordArray.R")


# Helpers

# Reconstruct a dense matrix from a coord-format table. `tbl` must contain
# columns 'index1', 'index2', 'value'; any additional partition columns
# (e.g. 'index1_group', 'index2_group') are ignored.
.rebuildFromCoordTable <- function(tbl, template) {
    mat <- matrix(0, nrow = nrow(template), ncol = ncol(template),
                  dimnames = dimnames(template))
    if (nrow(tbl) > 0L) {
        storage.mode(mat) <- typeof(template)
        mat[cbind(tbl$index1, tbl$index2)] <- tbl$value
    }
    mat
}

# Arrow type of `column` across every parquet file beneath `path`.
.columnTypes <- function(path, column = "value") {
    files <- list.files(path, pattern = "\\.parquet$", recursive = TRUE,
                        full.names = TRUE)
    vapply(files, function(f) {
        sch <- arrow::schema(arrow::read_parquet(f, as_data_frame = FALSE))
        sch$GetFieldByName(column)$type$ToString()
    }, character(1L), USE.NAMES = FALSE)
}

# Snapshot of (relative path, byte-size, mtime) for every file under
# `path`. Used to assert that a failed call did not touch the existing
# dataset.
.datasetFingerprint <- function(path) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE)
    if (length(files) == 0L) return(data.frame(rel = character(0),
                                               size = double(0),
                                               mtime = .POSIXct(double(0))))
    info <- file.info(files)
    data.frame(rel = sub(paste0("^", path, "/?"), "", files),
               size = info$size,
               mtime = info$mtime,
               row.names = NULL)
}


# (1) round-trip

test_that("writeCoordArray round-trips state.x77 through a single-cell grid", {
    path <- file.path(tempfile(), "state_single")
    grid <- RegularArrayGrid(dim(state.x77), dim(state.x77))
    expect_equal(length(grid), 1L)

    writeCoordArray(state.x77, path, grid = grid)

    expect_true(dir.exists(path))
    expect_equal(list.dirs(path, recursive = FALSE, full.names = FALSE),
                 character(0))
    expect_gte(length(list.files(path, pattern = "\\.parquet$")), 1L)

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_true(all(c("index1", "index2", "value") %in% names(tbl)))
    expect_equal(.rebuildFromCoordTable(tbl, state.x77), state.x77)
})


test_that("writeCoordArray round-trips state.x77 through a multi-cell grid", {
    # state_path / state_grid come from setup.R (dim(grid) == c(5, 2)).
    expect_equal(dim(state_grid), c(5L, 2L))

    row_dirs <- list.dirs(state_path, recursive = FALSE, full.names = FALSE)
    expect_equal(sort(row_dirs), paste0("index1_group=", 1:5))
    for (rg in row_dirs) {
        col_dirs <- list.dirs(file.path(state_path, rg),
                              recursive = FALSE, full.names = FALSE)
        expect_equal(sort(col_dirs), paste0("index2_group=", 1:2))
    }

    tbl <- as.data.frame(arrow::open_dataset(state_path) |> dplyr::collect())
    expect_true(all(c("index1", "index2", "value",
                      "index1_group", "index2_group") %in% names(tbl)))
    expect_equal(.rebuildFromCoordTable(tbl, state.x77), state.x77)
})


test_that("writeCoordArray round-trips a 3D sparse integer array", {
    # sparse_array from setup.R is 26x26x12 integer with ~1000 non-zeros.
    # Its dimnames are named dim1/dim2/dim3; writeCoordArray adopts those
    # as indexcols by default, so we read them back by the same names.
    path <- file.path(tempfile(), "sparse_multi")
    grid <- RegularArrayGrid(dim(sparse_array), c(13L, 13L, 6L))
    writeCoordArray(sparse_array, path, grid = grid)

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    rebuilt <- array(0L, dim = dim(sparse_array),
                     dimnames = dimnames(sparse_array))
    rebuilt[cbind(tbl$dim1, tbl$dim2, tbl$dim3)] <- tbl$value
    expect_equal(rebuilt, sparse_array)
})


test_that("writeCoordArray round-trips an all-zero input (single-cell)", {
    m <- matrix(0L, nrow = 3L, ncol = 4L)
    path <- file.path(tempfile(), "zeros_single")
    writeCoordArray(m, path,
                    grid = RegularArrayGrid(dim(m), dim(m)))
    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(nrow(tbl), 0L)
    expect_equal(.rebuildFromCoordTable(tbl, m), m)
})


test_that("writeCoordArray round-trips an all-zero input (multi-cell)", {
    m <- matrix(0L, nrow = 4L, ncol = 4L)
    path <- file.path(tempfile(), "zeros_multi")
    writeCoordArray(m, path, grid = RegularArrayGrid(dim(m), c(2L, 2L)))
    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(nrow(tbl), 0L)
    expect_equal(.rebuildFromCoordTable(tbl, m), m)
})


# (2) argument validation

test_that("writeCoordArray rejects non-array-like inputs", {
    expect_error(writeCoordArray(1:10, tempfile()), "array-like")
})


test_that("writeCoordArray validates 'arrowtype'", {
    expect_error(
        writeCoordArray(state.x77, tempfile(), arrowtype = "float64"),
        "'arrowtype' must be NULL or an arrow DataType"
    )
    expect_error(
        writeCoordArray(state.x77, tempfile(), arrowtype = 1L),
        "'arrowtype' must be NULL or an arrow DataType"
    )
    expect_silent(
        writeCoordArray(state.x77, file.path(tempfile(), "ok"),
                        arrowtype = arrow::float64())
    )
})


test_that("writeCoordArray validates 'max_dim'", {
    grid <- RegularArrayGrid(dim(state.x77), c(10L, 4L))

    expect_error(
        writeCoordArray(state.x77, tempfile(), grid = grid,
                        max_dim = "big"),
        "'max_dim' must be NULL or an integer vector"
    )
    expect_error(
        writeCoordArray(state.x77, tempfile(), grid = grid,
                        max_dim = c(50L, 8L, 1L)),
        "'max_dim' must be NULL or an integer vector of length 2"
    )
    expect_error(
        writeCoordArray(state.x77, tempfile(), grid = grid,
                        max_dim = c(50L, NA_integer_)),
        "'max_dim' must be NULL or an integer vector"
    )
    expect_error(
        writeCoordArray(state.x77, tempfile(), grid = grid,
                        max_dim = c(50.5, 8)),
        "'max_dim' must be NULL or an integer vector"
    )
    expect_error(
        writeCoordArray(state.x77, tempfile(), grid = grid,
                        max_dim = c(49L, 8L)),
        "'max_dim\\[1\\]'.*must be >= dim\\(x\\)\\[1\\]"
    )

    # In append mode, max_dim[along] must also cover dim(x)[along] + offset.
    path <- file.path(tempfile(), "mdim_val")
    writeCoordArray(state.x77[, 1:4], path,
                    grid = RegularArrayGrid(c(50L, 4L), c(25L, 2L)))
    expect_error(
        writeCoordArray(
            state.x77[, 5:8], path,
            grid = RegularArrayGrid(c(50L, 4L), c(25L, 2L)),
            append = TRUE, along = 2L, offset = 4L, group_offset = 2L,
            max_dim = c(50L, 6L)
        ),
        "'max_dim\\[2\\]'.*must be >= dim\\(x\\)\\[2\\] \\+ offset"
    )
})


test_that("writeCoordArray validates append-mode arguments", {
    grid <- RegularArrayGrid(dim(state.x77), c(10L, 4L))
    path <- file.path(tempfile(), "state_val")
    writeCoordArray(state.x77, path, grid = grid)

    expect_error(writeCoordArray(state.x77, path, grid = grid, append = NA),
                 "'append' must be a single logical")
    expect_error(writeCoordArray(state.x77, path, grid = grid, append = "yes"),
                 "'append' must be a single logical")
    expect_error(writeCoordArray(state.x77, path, grid = grid,
                                 append = c(TRUE, FALSE)),
                 "'append' must be a single logical")

    for (bad_along in list(NULL, 0L, 3L, 1.5)) {
        expect_error(writeCoordArray(state.x77, path, grid = grid,
                                     append = TRUE, along = bad_along),
                     "'along' must be")
    }
    for (bad_offset in list(-1L, 1.5)) {
        expect_error(writeCoordArray(state.x77, path, grid = grid,
                                     append = TRUE, along = 2L,
                                     offset = bad_offset),
                     "'offset' must be")
    }
    for (bad_go in list(-1L, 0.5)) {
        expect_error(writeCoordArray(state.x77, path, grid = grid,
                                     append = TRUE, along = 2L,
                                     offset = 0L, group_offset = bad_go),
                     "'group_offset' must be")
    }

    missing_path <- file.path(tempfile(), "doesnotexist")
    expect_error(writeCoordArray(state.x77, missing_path, grid = grid,
                                 append = TRUE, along = 2L),
                 "target directory does not exist")

    path_single <- file.path(tempfile(), "state_single_val")
    single_grid <- RegularArrayGrid(dim(state.x77), dim(state.x77))
    writeCoordArray(state.x77, path_single, grid = single_grid)
    expect_error(writeCoordArray(state.x77, path_single, grid = single_grid,
                                 append = TRUE, along = 2L),
                 "hive-partitioned")
})


# (3) schema correctness

test_that("explicit 'arrowtype' is respected across every partition", {
    int_mat <- state.x77[, c("Population", "Income")]

    # Single-cell
    path <- file.path(tempfile(), "at_single")
    writeCoordArray(int_mat, path,
                    grid = RegularArrayGrid(dim(int_mat), dim(int_mat)),
                    arrowtype = arrow::int32())
    expect_equal(unique(.columnTypes(path, "value")), "int32")

    # Multi-cell; pin to float64 even though data is integer-valued.
    path2 <- file.path(tempfile(), "at_multi")
    writeCoordArray(int_mat, path2,
                    grid = RegularArrayGrid(dim(int_mat), c(10L, 1L)),
                    arrowtype = arrow::float64())
    expect_equal(unique(.columnTypes(path2, "value")), "double")

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(.rebuildFromCoordTable(tbl, int_mat), int_mat)
})


test_that("explicit 'max_dim' widens index-column type", {
    # state.x77: without max_dim, both index columns fit in uint8.
    path <- file.path(tempfile(), "md_single")
    writeCoordArray(state.x77, path,
                    grid = RegularArrayGrid(dim(state.x77), dim(state.x77)),
                    max_dim = c(1000L, 8L))
    expect_equal(unique(.columnTypes(path, "index1")), "uint16")
    expect_equal(unique(.columnTypes(path, "index2")), "uint8")

    path2 <- file.path(tempfile(), "md_multi")
    writeCoordArray(state.x77, path2,
                    grid = RegularArrayGrid(dim(state.x77), c(10L, 4L)),
                    max_dim = c(1000L, 8L))
    expect_equal(unique(.columnTypes(path2, "index1")), "uint16")

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(.rebuildFromCoordTable(tbl, state.x77), state.x77)
})


# (4) corruption prevention: append

# Helper to set up a first slab for each append-corruption test. Uses
# integer-valued columns (Population, Income) so that pinning a narrow
# integer 'arrowtype' on the first call does not lossily coerce values.
.writeFirstSlab <- function(arrowtype = NULL, max_dim = NULL) {
    path <- file.path(tempfile(), "slab1")
    slab1 <- state.x77[, c("Population", "Income"), drop = FALSE]
    writeCoordArray(slab1, path,
                    grid = RegularArrayGrid(dim(slab1), c(25L, 1L)),
                    arrowtype = arrowtype,
                    max_dim = max_dim)
    list(path = path, slab = slab1,
         grid = RegularArrayGrid(dim(slab1), c(25L, 1L)))
}


test_that("append without pinned 'arrowtype' adopts the existing value type", {
    # Pin int32 on the first call; omit arrowtype on the second. Without
    # the schema-adoption logic the second call would re-infer a type from
    # its own data and potentially drift (int8/uint16/double). Both slabs
    # use integer-valued columns so the pinned int32 is lossless.
    ctx <- .writeFirstSlab(arrowtype = arrow::int32())

    slab2 <- state.x77[, c("Frost", "Area"), drop = FALSE]
    writeCoordArray(slab2, ctx$path,
                    grid = RegularArrayGrid(dim(slab2), c(25L, 1L)),
                    append = TRUE, along = 2L,
                    offset = ncol(ctx$slab),
                    group_offset = dim(ctx$grid)[2L])

    expect_equal(unique(.columnTypes(ctx$path, "value")), "int32")
    tbl <- as.data.frame(arrow::open_dataset(ctx$path) |> dplyr::collect())
    combined <- state.x77[, c("Population", "Income", "Frost", "Area"),
                          drop = FALSE]
    expect_equal(.rebuildFromCoordTable(tbl, combined), combined)
})


test_that("append without pinned 'max_dim' adopts the existing index types", {
    # Pin max_dim so the first slab's index1 lands in uint16 (not its
    # natural uint8). Also pin an explicit arrowtype wide enough for all
    # data so we don't have to reason about value-column type drift in
    # this test; the second call omits max_dim and must adopt uint16 from
    # the existing schema rather than falling back to dim(x)-based
    # inference.
    ctx <- .writeFirstSlab(arrowtype = arrow::float64(),
                           max_dim = c(1000L, 8L))

    slab2 <- state.x77[, c("Frost", "Area"), drop = FALSE]
    writeCoordArray(slab2, ctx$path,
                    grid = RegularArrayGrid(dim(slab2), c(25L, 1L)),
                    append = TRUE, along = 2L,
                    offset = ncol(ctx$slab),
                    group_offset = dim(ctx$grid)[2L])

    expect_equal(unique(.columnTypes(ctx$path, "index1")), "uint16")
    tbl <- as.data.frame(arrow::open_dataset(ctx$path) |> dplyr::collect())
    combined <- state.x77[, c("Population", "Income", "Frost", "Area"),
                          drop = FALSE]
    expect_equal(.rebuildFromCoordTable(tbl, combined), combined)
})


test_that("append rejects mismatched 'arrowtype' and keeps existing data intact", {
    ctx <- .writeFirstSlab(arrowtype = arrow::int32())
    before <- .datasetFingerprint(ctx$path)

    slab2 <- state.x77[, 5:8]
    expect_error(
        writeCoordArray(slab2, ctx$path,
                        grid = RegularArrayGrid(dim(slab2), c(25L, 2L)),
                        append = TRUE, along = 2L,
                        offset = ncol(ctx$slab),
                        group_offset = dim(ctx$grid)[2L],
                        arrowtype = arrow::float64()),
        "schema mismatch on 'value'"
    )

    after <- .datasetFingerprint(ctx$path)
    expect_equal(before, after)
})


test_that("append rejects mismatched 'max_dim' and keeps existing data intact", {
    # First slab pinned to uint16 on index1. Appending with max_dim that
    # would produce uint8 must be rejected before writing anything.
    ctx <- .writeFirstSlab(max_dim = c(1000L, 8L))
    before <- .datasetFingerprint(ctx$path)

    slab2 <- state.x77[, 5:8]
    expect_error(
        writeCoordArray(slab2, ctx$path,
                        grid = RegularArrayGrid(dim(slab2), c(25L, 2L)),
                        append = TRUE, along = 2L,
                        offset = ncol(ctx$slab),
                        group_offset = dim(ctx$grid)[2L],
                        max_dim = c(50L, 8L)),
        "schema mismatch on 'index1'"
    )

    after <- .datasetFingerprint(ctx$path)
    expect_equal(before, after)
})


test_that("append rejects partition collision and keeps existing data intact", {
    ctx <- .writeFirstSlab()
    before <- .datasetFingerprint(ctx$path)
    slab2 <- state.x77[, 5:8]

    # group_offset = 0 would write into existing 'index2_group=1..2'.
    expect_error(
        writeCoordArray(slab2, ctx$path,
                        grid = RegularArrayGrid(dim(slab2), c(25L, 2L)),
                        append = TRUE, along = 2L,
                        offset = ncol(ctx$slab),
                        group_offset = 0L),
        "would write into an existing partition"
    )
    expect_equal(.datasetFingerprint(ctx$path), before)

    # group_offset = 1 would still overlap (new groups 2..3, existing 1..2).
    expect_error(
        writeCoordArray(slab2, ctx$path,
                        grid = RegularArrayGrid(dim(slab2), c(25L, 2L)),
                        append = TRUE, along = 2L,
                        offset = ncol(ctx$slab),
                        group_offset = 1L),
        "would write into an existing partition"
    )
    expect_equal(.datasetFingerprint(ctx$path), before)
})


test_that("append rejects a non-hive target", {
    # Write a single-cell (non-hive) dataset and try to append to it.
    path <- file.path(tempfile(), "nonhive")
    writeCoordArray(state.x77, path,
                    grid = RegularArrayGrid(dim(state.x77), dim(state.x77)))
    before <- .datasetFingerprint(path)

    slab2 <- state.x77[, 5:8]
    expect_error(
        writeCoordArray(slab2, path,
                        grid = RegularArrayGrid(dim(slab2), c(25L, 2L)),
                        append = TRUE, along = 2L,
                        offset = 4L, group_offset = 0L),
        "hive-partitioned dataset"
    )
    expect_equal(.datasetFingerprint(path), before)
})


test_that("append rejects an empty target directory", {
    path <- file.path(tempfile(), "emptydir")
    dir.create(path, recursive = TRUE)

    slab <- state.x77[, 1:4]
    expect_error(
        writeCoordArray(slab, path,
                        grid = RegularArrayGrid(dim(slab), c(25L, 2L)),
                        append = TRUE, along = 2L, offset = 0L,
                        group_offset = 0L),
        "no parquet files|hive-partitioned"
    )
})


test_that("append rejects missing fields in the existing schema", {
    ctx <- .writeFirstSlab()
    before <- .datasetFingerprint(ctx$path)
    slab2 <- state.x77[, 5:8]

    # Wrong datacol name.
    expect_error(
        writeCoordArray(slab2, ctx$path, datacol = "val",
                        grid = RegularArrayGrid(dim(slab2), c(25L, 2L)),
                        append = TRUE, along = 2L,
                        offset = ncol(ctx$slab),
                        group_offset = dim(ctx$grid)[2L]),
        "lacks field"
    )
    expect_equal(.datasetFingerprint(ctx$path), before)

    # Wrong indexcol name.
    expect_error(
        writeCoordArray(slab2, ctx$path,
                        indexcols = c("row", "col"),
                        grid = RegularArrayGrid(dim(slab2), c(25L, 2L)),
                        append = TRUE, along = 2L,
                        offset = ncol(ctx$slab),
                        group_offset = dim(ctx$grid)[2L]),
        "lacks field"
    )
    expect_equal(.datasetFingerprint(ctx$path), before)
})


# (5) corruption prevention: non-append

test_that("non-append refuses to silently overwrite an existing dataset", {
    path <- file.path(tempfile(), "overwrite")
    grid <- RegularArrayGrid(dim(state.x77), c(10L, 4L))
    writeCoordArray(state.x77, path, grid = grid)
    before <- .datasetFingerprint(path)

    # A re-run without append = TRUE into a populated path must not
    # silently clobber. arrow's default existing_data_behavior would do
    # exactly that; we override it to "error".
    expect_error(
        writeCoordArray(state.x77, path, grid = grid),
        regexp = NULL
    )
    expect_equal(.datasetFingerprint(path), before)
})


# (6) end-to-end append

test_that("writeCoordArray appends new hive partitions along a dimension", {
    slab1 <- state.x77[, 1:4, drop = FALSE]
    slab2 <- state.x77[, 5:8, drop = FALSE]

    path <- file.path(tempfile(), "state_append")
    grid1 <- RegularArrayGrid(dim(slab1), c(10L, 2L))  # 5 x 2 = 10 cells
    grid2 <- RegularArrayGrid(dim(slab2), c(10L, 2L))  # 5 x 2 = 10 cells

    writeCoordArray(slab1, path, grid = grid1)
    writeCoordArray(slab2, path, grid = grid2,
                    append = TRUE, along = 2L,
                    offset = ncol(slab1),
                    group_offset = dim(grid1)[2L])

    row_dirs <- list.dirs(path, recursive = FALSE, full.names = FALSE)
    expect_equal(sort(row_dirs), paste0("index1_group=", 1:5))
    for (rg in row_dirs) {
        col_dirs <- list.dirs(file.path(path, rg),
                              recursive = FALSE, full.names = FALSE)
        expect_equal(sort(col_dirs), paste0("index2_group=", 1:4))
    }

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(sort(unique(tbl$index2_group)), 1:4)
    # The first two partitions hold samples 1..4; the appended ones 5..8.
    slab1_rows <- tbl[tbl$index2_group %in% 1:2, , drop = FALSE]
    slab2_rows <- tbl[tbl$index2_group %in% 3:4, , drop = FALSE]
    expect_true(all(slab1_rows$index2 >= 1L & slab1_rows$index2 <= 4L))
    expect_true(all(slab2_rows$index2 >= 5L & slab2_rows$index2 <= 8L))

    expect_equal(.rebuildFromCoordTable(tbl, state.x77), state.x77)
})


test_that("writeCoordArray supports multiple successive appends", {
    slab1 <- state.x77[, 1:3, drop = FALSE]
    slab2 <- state.x77[, 4:5, drop = FALSE]
    slab3 <- state.x77[, 6:8, drop = FALSE]

    path <- file.path(tempfile(), "state_append_chain")
    grid1 <- RegularArrayGrid(dim(slab1), c(25L, 3L))  # 2 x 1
    grid2 <- RegularArrayGrid(dim(slab2), c(25L, 2L))  # 2 x 1
    grid3 <- RegularArrayGrid(dim(slab3), c(25L, 3L))  # 2 x 1

    writeCoordArray(slab1, path, grid = grid1)
    writeCoordArray(slab2, path, grid = grid2,
                    append = TRUE, along = 2L,
                    offset = ncol(slab1),
                    group_offset = dim(grid1)[2L])
    writeCoordArray(slab3, path, grid = grid3,
                    append = TRUE, along = 2L,
                    offset = ncol(slab1) + ncol(slab2),
                    group_offset = dim(grid1)[2L] + dim(grid2)[2L])

    for (rg in list.dirs(path, recursive = FALSE, full.names = FALSE)) {
        col_dirs <- list.dirs(file.path(path, rg),
                              recursive = FALSE, full.names = FALSE)
        expect_equal(sort(col_dirs), paste0("index2_group=", 1:3))
    }

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    expect_equal(.rebuildFromCoordTable(tbl, state.x77), state.x77)
})


test_that("append contributes multiple new partitions per call", {
    # The appended slab's own grid has >1 cell along `along`: each must
    # land in its own new partition.
    slab1 <- state.x77[, 1:3, drop = FALSE]
    slab2 <- state.x77[, 4:7, drop = FALSE]

    path <- file.path(tempfile(), "multi_new")
    grid1 <- RegularArrayGrid(dim(slab1), c(25L, 3L))  # 2 x 1
    grid2 <- RegularArrayGrid(dim(slab2), c(25L, 2L))  # 2 x 2

    writeCoordArray(slab1, path, grid = grid1)
    writeCoordArray(slab2, path, grid = grid2,
                    append = TRUE, along = 2L,
                    offset = ncol(slab1),
                    group_offset = dim(grid1)[2L])

    for (rg in list.dirs(path, recursive = FALSE, full.names = FALSE)) {
        col_dirs <- list.dirs(file.path(path, rg),
                              recursive = FALSE, full.names = FALSE)
        expect_equal(sort(col_dirs), paste0("index2_group=", 1:3))
    }

    tbl <- as.data.frame(arrow::open_dataset(path) |> dplyr::collect())
    combined <- state.x77[, 1:7, drop = FALSE]
    expect_equal(.rebuildFromCoordTable(tbl, combined), combined)
})
