# Tests for rowDeviances generic and methods
#
# Tests dgCMatrix and DelayedMatrix (via HDF5Matrix) methods
# using the airway_counts matrix from setup.R

test_that("rowDeviances works on dgCMatrix", {
    # Convert airway_counts to dgCMatrix
    sparse_mat <- as(airway_counts, "dgCMatrix")

    # Reference: in-memory matrix result
    dense_mat <- as.matrix(airway_counts)

    # Test Poisson deviance
    dev_pois_sparse <- rowDeviances(sparse_mat, family = "poisson")
    dev_pois_dense <- rowDeviances(dense_mat, family = "poisson")
    expect_equal(dev_pois_sparse, dev_pois_dense, tolerance = 1e-10)

    # Test Binomial deviance
    dev_binom_sparse <- rowDeviances(sparse_mat, family = "binomial")
    dev_binom_dense <- rowDeviances(dense_mat, family = "binomial")
    expect_equal(dev_binom_sparse, dev_binom_dense, tolerance = 1e-10)

    # Verify results are sensible
    expect_length(dev_pois_sparse, nrow(airway_counts))
    expect_true(all(dev_pois_sparse >= 0 | is.na(dev_pois_sparse)))
    expect_equal(names(dev_pois_sparse), rownames(airway_counts))

    # Poisson and binomial should be highly correlated
    valid <- dev_pois_sparse > 0 & dev_binom_sparse > 0
    expect_gt(cor(dev_pois_sparse[valid], dev_binom_sparse[valid]), 0.95)
})

test_that("rowDeviances works on HDF5Matrix (DelayedMatrix) with default settings", {
    skip_if_not_installed("HDF5Array")

    # Create HDF5Matrix from airway_counts
    h5_file <- tempfile(fileext = ".h5")
    h5_mat <- HDF5Array::writeHDF5Array(airway_counts, filepath = h5_file,
                                         name = "counts")

    # Reference: in-memory matrix result
    dense_mat <- as.matrix(airway_counts)

    # Test Poisson deviance
    dev_pois_h5 <- rowDeviances(h5_mat, family = "poisson")
    dev_pois_dense <- rowDeviances(dense_mat, family = "poisson")
    expect_equal(dev_pois_h5, dev_pois_dense, tolerance = 1e-10)

    # Test Binomial deviance
    dev_binom_h5 <- rowDeviances(h5_mat, family = "binomial")
    dev_binom_dense <- rowDeviances(dense_mat, family = "binomial")
    expect_equal(dev_binom_h5, dev_binom_dense, tolerance = 1e-10)

    # Verify results are sensible
    expect_length(dev_pois_h5, nrow(airway_counts))
    expect_true(all(dev_pois_h5 >= 0 | is.na(dev_pois_h5)))
    expect_equal(names(dev_pois_h5), rownames(airway_counts))

    # Poisson and binomial should be highly correlated
    valid <- dev_pois_h5 > 0 & dev_binom_h5 > 0
    expect_gt(cor(dev_pois_h5[valid], dev_binom_h5[valid]), 0.95)

    # Clean up
    unlink(h5_file)
})

test_that("rowDeviances works on DelayedMatrix with dense blocks (small grid)", {
    skip_if_not_installed("HDF5Array")

    # Create HDF5Matrix from airway_counts
    h5_file <- tempfile(fileext = ".h5")
    h5_mat <- HDF5Array::writeHDF5Array(airway_counts, filepath = h5_file,
                                        name = "counts")

    # Reference: in-memory matrix result
    dense_mat <- as.matrix(airway_counts)

    # Create a small column grid to ensure multiple blocks (2 columns per block)
    small_grid <- DelayedArray::colAutoGrid(h5_mat, ncol = 2)
    expect_gt(length(small_grid), 1)  # Ensure we have multiple blocks

    # Test with as.sparse = TRUE (force sparse blocks)
    dev_pois_dense_blocks <- rowDeviances(h5_mat, family = "poisson",
                                          grid = small_grid, as.sparse = TRUE)
    dev_pois_ref <- rowDeviances(dense_mat, family = "poisson")
    expect_equal(dev_pois_dense_blocks, dev_pois_ref, tolerance = 1e-10)

    dev_binom_dense_blocks <- rowDeviances(h5_mat, family = "binomial",
                                           grid = small_grid, as.sparse = TRUE)
    dev_binom_ref <- rowDeviances(dense_mat, family = "binomial")
    expect_equal(dev_binom_dense_blocks, dev_binom_ref, tolerance = 1e-10)

    # Test with as.sparse = FALSE (force dense blocks)
    dev_pois_dense_blocks <- rowDeviances(h5_mat, family = "poisson",
                                          grid = small_grid, as.sparse = FALSE)
    dev_pois_ref <- rowDeviances(dense_mat, family = "poisson")
    expect_equal(dev_pois_dense_blocks, dev_pois_ref, tolerance = 1e-10)

    dev_binom_dense_blocks <- rowDeviances(h5_mat, family = "binomial",
                                           grid = small_grid, as.sparse = FALSE)
    dev_binom_ref <- rowDeviances(dense_mat, family = "binomial")
    expect_equal(dev_binom_dense_blocks, dev_binom_ref, tolerance = 1e-10)

    # Clean up
    unlink(h5_file)
})

test_that("rowDeviances works on DelayedMatrix with sparse blocks (small grid)", {
    skip_if_not_installed("HDF5Array")

    # Create HDF5Matrix from airway_counts
    h5_file <- tempfile(fileext = ".h5")
    h5_mat <- HDF5Array::writeHDF5Array(airway_counts, filepath = h5_file,
                                        name = "counts")

    # Reference: in-memory matrix result
    dense_mat <- as.matrix(airway_counts)

    # Create a small column grid to ensure multiple blocks (2 columns per block)
    small_grid <- DelayedArray::colAutoGrid(h5_mat, ncol = 2)
    expect_gt(length(small_grid), 1)  # Ensure we have multiple blocks

    # Test with as.sparse = TRUE (force sparse blocks)
    dev_pois_sparse_blocks <- rowDeviances(h5_mat, family = "poisson",
                                           grid = small_grid, as.sparse = TRUE)
    dev_pois_ref <- rowDeviances(dense_mat, family = "poisson")
    expect_equal(dev_pois_sparse_blocks, dev_pois_ref, tolerance = 1e-10)

    dev_binom_sparse_blocks <- rowDeviances(h5_mat, family = "binomial",
                                            grid = small_grid, as.sparse = TRUE)
    dev_binom_ref <- rowDeviances(dense_mat, family = "binomial")
    expect_equal(dev_binom_sparse_blocks, dev_binom_ref, tolerance = 1e-10)

    # Clean up
    unlink(h5_file)
})

test_that("rowDeviances handles edge cases correctly", {
    # Small test matrix with known properties
    set.seed(42)
    small_mat <- matrix(rpois(500, lambda = 5), nrow = 50, ncol = 10)
    rownames(small_mat) <- paste0("Gene", seq_len(50))
    colnames(small_mat) <- paste0("Cell", seq_len(10))

    # Add some zero rows
    small_mat[1:3, ] <- 0

    # Test matrix method
    dev_mat <- rowDeviances(small_mat, family = "poisson")
    expect_length(dev_mat, 50)
    expect_equal(dev_mat[1:3], c(Gene1 = 0, Gene2 = 0, Gene3 = 0))
    expect_true(all(dev_mat >= 0))

    # Test dgCMatrix method
    sparse_mat <- as(small_mat, "dgCMatrix")
    dev_sparse <- rowDeviances(sparse_mat, family = "poisson")
    expect_equal(dev_mat, dev_sparse, tolerance = 1e-10)

    # Test binomial family
    dev_binom_mat <- rowDeviances(small_mat, family = "binomial")
    dev_binom_sparse <- rowDeviances(sparse_mat, family = "binomial")
    expect_equal(dev_binom_mat, dev_binom_sparse, tolerance = 1e-10)
})

test_that("rowDeviances preserves row names", {
    # Test with named matrix
    mat <- matrix(rpois(100, 10), nrow = 10, ncol = 10)
    rownames(mat) <- letters[1:10]

    dev <- rowDeviances(mat, family = "poisson")
    expect_equal(names(dev), letters[1:10])

    # Test with dgCMatrix
    sparse_mat <- as(mat, "dgCMatrix")
    dev_sparse <- rowDeviances(sparse_mat, family = "poisson")
    expect_equal(names(dev_sparse), letters[1:10])

    # Test with unnamed matrix
    mat_nonames <- unname(mat)
    dev_nonames <- rowDeviances(mat_nonames, family = "poisson")
    expect_null(names(dev_nonames))
})

test_that("rowDeviances family argument works correctly", {
    mat <- matrix(rpois(100, 10), nrow = 10, ncol = 10)

    # Default should be binomial
    dev_default <- rowDeviances(mat)
    dev_binom <- rowDeviances(mat, family = "binomial")
    expect_equal(dev_default, dev_binom)

    # Poisson and binomial give different but correlated results
    dev_pois <- rowDeviances(mat, family = "poisson")
    expect_false(isTRUE(all.equal(dev_pois, dev_binom)))
    expect_gt(cor(dev_pois, dev_binom), 0.9)

    # Invalid family should error
    expect_error(rowDeviances(mat, family = "gaussian"))
})
