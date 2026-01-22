# Tests for rowNnzs and colNnzs generic and methods
#
# Tests dgCMatrix and DelayedMatrix (via HDF5Matrix) methods
# using the airway_counts matrix from setup.R

test_that("rowNnzs works on dgCMatrix", {
    # Convert airway_counts to dgCMatrix
    sparse_mat <- as(airway_counts, "dgCMatrix")

    # Reference: in-memory matrix result
    dense_mat <- as.matrix(airway_counts)

    # Test rowNnzs
    nnz_sparse <- rowNnzs(sparse_mat)
    nnz_dense <- rowNnzs(dense_mat)
    expect_equal(nnz_sparse, nnz_dense)

    # Test colNnzs
    nnz_sparse <- colNnzs(sparse_mat)
    nnz_dense <- colNnzs(dense_mat)
    expect_equal(nnz_sparse, nnz_dense)

    # Verify results are sensible
    expect_length(rowNnzs(sparse_mat), nrow(airway_counts))
    expect_length(colNnzs(sparse_mat), ncol(airway_counts))
    expect_true(all(rowNnzs(sparse_mat) >= 0))
    expect_true(all(colNnzs(sparse_mat) >= 0))
    expect_true(all(rowNnzs(sparse_mat) <= ncol(airway_counts)))
    expect_true(all(colNnzs(sparse_mat) <= nrow(airway_counts)))
    expect_equal(names(rowNnzs(sparse_mat)), rownames(airway_counts))
    expect_equal(names(colNnzs(sparse_mat)), colnames(airway_counts))
})

test_that("rowNnzs works on HDF5Matrix (DelayedMatrix) with default settings", {
    skip_if_not_installed("HDF5Array")

    # Create HDF5Matrix from airway_counts
    h5_file <- tempfile(fileext = ".h5")
    h5_mat <- HDF5Array::writeHDF5Array(airway_counts, filepath = h5_file,
                                         name = "counts")

    # Reference: in-memory matrix result
    dense_mat <- as.matrix(airway_counts)

    # Test rowNnzs
    nnz_h5 <- rowNnzs(h5_mat)
    nnz_dense <- rowNnzs(dense_mat)
    expect_equal(nnz_h5, nnz_dense)

    # Test colNnzs
    nnz_h5 <- colNnzs(h5_mat)
    nnz_dense <- colNnzs(dense_mat)
    expect_equal(nnz_h5, nnz_dense)

    # Verify results are sensible
    expect_length(rowNnzs(h5_mat), nrow(airway_counts))
    expect_length(colNnzs(h5_mat), ncol(airway_counts))
    expect_equal(names(rowNnzs(h5_mat)), rownames(airway_counts))
    expect_equal(names(colNnzs(h5_mat)), colnames(airway_counts))

    # Clean up
    unlink(h5_file)
})

test_that("rowNnzs handles edge cases correctly", {
    # Small test matrix with known properties
    set.seed(42)
    small_mat <- matrix(rpois(500, lambda = 5), nrow = 50, ncol = 10)
    rownames(small_mat) <- paste0("Gene", seq_len(50))
    colnames(small_mat) <- paste0("Cell", seq_len(10))

    # Add some zero rows and columns
    small_mat[1:3, ] <- 0
    small_mat[, 1:2] <- 0

    # Test matrix method
    row_nnz <- rowNnzs(small_mat)
    col_nnz <- colNnzs(small_mat)
    expect_length(row_nnz, 50)
    expect_length(col_nnz, 10)
    expect_equal(row_nnz[1:3], c(Gene1 = 0L, Gene2 = 0L, Gene3 = 0L))
    expect_equal(col_nnz[1:2], c(Cell1 = 0L, Cell2 = 0L))
    expect_true(all(row_nnz >= 0))
    expect_true(all(col_nnz >= 0))

    # Test dgCMatrix method
    sparse_mat <- as(small_mat, "dgCMatrix")
    row_nnz_sparse <- rowNnzs(sparse_mat)
    col_nnz_sparse <- colNnzs(sparse_mat)
    expect_equal(row_nnz, row_nnz_sparse)
    expect_equal(col_nnz, col_nnz_sparse)
})

test_that("rowNnzs preserves row/column names", {
    # Test with named matrix
    mat <- matrix(rpois(100, 10), nrow = 10, ncol = 10)
    rownames(mat) <- letters[1:10]
    colnames(mat) <- LETTERS[1:10]

    row_nnz <- rowNnzs(mat)
    col_nnz <- colNnzs(mat)
    expect_equal(names(row_nnz), letters[1:10])
    expect_equal(names(col_nnz), LETTERS[1:10])

    # Test with dgCMatrix
    sparse_mat <- as(mat, "dgCMatrix")
    row_nnz_sparse <- rowNnzs(sparse_mat)
    col_nnz_sparse <- colNnzs(sparse_mat)
    expect_equal(names(row_nnz_sparse), letters[1:10])
    expect_equal(names(col_nnz_sparse), LETTERS[1:10])

    # Test with unnamed matrix
    mat_nonames <- unname(mat)
    row_nnz_nonames <- rowNnzs(mat_nonames)
    col_nnz_nonames <- colNnzs(mat_nonames)
    expect_null(names(row_nnz_nonames))
    expect_null(names(col_nnz_nonames))
})

test_that("rowNnzs equals ncol minus rowCounts(value=0)", {
    mat <- matrix(rpois(100, 10), nrow = 10, ncol = 10)

    # rowNnzs should equal ncol - rowCounts(value = 0)
    row_nnz <- rowNnzs(mat)
    row_counts_zero <- MatrixGenerics::rowCounts(mat, value = 0L)
    expect_equal(row_nnz, ncol(mat) - row_counts_zero)

    # colNnzs should equal nrow - colCounts(value = 0)
    col_nnz <- colNnzs(mat)
    col_counts_zero <- MatrixGenerics::colCounts(mat, value = 0L)
    expect_equal(col_nnz, nrow(mat) - col_counts_zero)
})

test_that("rowNnzs handles different data types", {
    # Integer matrix (zero = 0L)
    int_mat <- matrix(c(0L, 1L, 2L, 0L), nrow = 2)
    expect_equal(as.integer(rowNnzs(int_mat)), c(1L, 1L))
    expect_equal(as.integer(colNnzs(int_mat)), c(1L, 1L))

    # Numeric matrix (zero = 0)
    num_mat <- matrix(c(0, 1.5, 2.5, 0), nrow = 2)
    expect_equal(as.integer(rowNnzs(num_mat)), c(1L, 1L))
    expect_equal(as.integer(colNnzs(num_mat)), c(1L, 1L))

    # Logical matrix (zero = FALSE)
    log_mat <- matrix(c(FALSE, TRUE, TRUE, FALSE), nrow = 2)
    expect_equal(as.integer(rowNnzs(log_mat)), c(1L, 1L))
    expect_equal(as.integer(colNnzs(log_mat)), c(1L, 1L))
})
