# Tests the basic functions of a DuckDBMatrix.
# library(testthat); library(DuckDBArray); source("setup.R"); source("test-DuckDBMatrix.R")

test_that("basic methods work as expected for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")

    pqmat <- DuckDBMatrix(state_path, datacol = "value", row = "index1", col = "index2")
    expect_s4_class(pqmat, "DuckDBMatrix")
    expect_identical(type(pqmat), "double")
    expect_identical(type(pqmat), typeof(state.x77))
    expect_identical(length(pqmat), length(state.x77))
    expect_identical(dim(pqmat), dim(state.x77))
    expect_equivalent(as.matrix(pqmat), state.x77)

    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          row = list(index1 = setNames(seq_len(nrow(state.x77)), rownames(state.x77))),
                          col = "index2")
    expect_s4_class(pqmat, "DuckDBMatrix")
    expect_identical(type(pqmat), "double")
    expect_identical(type(pqmat), typeof(state.x77))
    expect_identical(length(pqmat), length(state.x77))
    expect_identical(dim(pqmat), dim(state.x77))
    expect_setequal(rownames(pqmat), rownames(state.x77))
    expect_equivalent(as.matrix(pqmat), state.x77)

    pqmat <- DuckDBMatrix(state_path, datacol = "value", row = "index1",
                          col = list(index2 = setNames(seq_len(ncol(state.x77)), colnames(state.x77))))
    expect_s4_class(pqmat, "DuckDBMatrix")
    expect_identical(type(pqmat), "double")
    expect_identical(type(pqmat), typeof(state.x77))
    expect_identical(length(pqmat), length(state.x77))
    expect_identical(dim(pqmat), dim(state.x77))
    expect_setequal(colnames(pqmat), colnames(state.x77))
    expect_equivalent(as.matrix(pqmat), state.x77)

    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          row = list(index1 = setNames(seq_len(nrow(state.x77)), rownames(state.x77))),
                          col = list(index2 = setNames(seq_len(ncol(state.x77)), colnames(state.x77))))
    checkDuckDBMatrix(pqmat, state.x77)

    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    checkDuckDBMatrix(pqmat, state.x77)

    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)
    checkDuckDBMatrix(pqmat, state.x77)
})

test_that("renaming dimensions works for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")

    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    expected <- state.x77

    replacements <- sprintf("ROW%d", seq_len(nrow(pqmat)))
    rownames(pqmat) <- replacements
    rownames(expected) <- replacements
    checkDuckDBMatrix(pqmat, expected)

    replacements <- sprintf("COL%d", seq_len(ncol(pqmat)))
    colnames(pqmat) <- replacements
    colnames(expected) <- replacements
    checkDuckDBMatrix(pqmat, expected)
})

test_that("extraction methods work as expected for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")

    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)

    expected <- as.array(state.x77[1, ])
    names(dimnames(expected)) <- "index2"
    checkDuckDBArray(pqmat[1, ], expected)
    checkDuckDBMatrix(pqmat[1, , drop = FALSE], state.x77[1, , drop = FALSE])

    expected <- as.array(state.x77["New Jersey", ])
    names(dimnames(expected)) <- "index2"
    checkDuckDBArray(pqmat["New Jersey", ], expected)
    checkDuckDBMatrix(pqmat["New Jersey", , drop = FALSE], state.x77["New Jersey", , drop = FALSE])

    checkDuckDBMatrix(pqmat[c("New Jersey", "Washington"), ], state.x77[c("New Jersey", "Washington"), ])

    expected <- as.array(state.x77[, 4])
    names(dimnames(expected)) <- "index1"
    checkDuckDBArray(pqmat[, 4], expected)
    checkDuckDBMatrix(pqmat[, 4, drop = FALSE], state.x77[, 4, drop = FALSE])

    expected <- as.array(state.x77[, "Murder"])
    names(dimnames(expected)) <- "index1"
    checkDuckDBArray(pqmat[, "Murder"], expected)
    checkDuckDBMatrix(pqmat[, "Murder", drop = FALSE], state.x77[, "Murder", drop = FALSE])

    checkDuckDBMatrix(pqmat[, c("Income", "Life Exp", "Murder")], state.x77[, c("Income", "Life Exp", "Murder")])

    checkDuckDBMatrix(pqmat[c(13, 7), c(1, 3, 5, 7)], state.x77[c(13, 7), c(1, 3, 5, 7)])
    checkDuckDBMatrix(pqmat[c("New Jersey", "Washington"), c("Income", "Life Exp", "Murder")],
                       state.x77[c("New Jersey", "Washington"), c("Income", "Life Exp", "Murder")])
})

test_that("aperm and t methods work as expected for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)
    checkDuckDBMatrix(aperm(pqmat, c(2, 1)), aperm(state.x77, c(2, 1)))
    checkDuckDBMatrix(t(pqmat), t(state.x77))
})

test_that("Arith methods work as expected for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)

    checkDuckDBMatrix(pqmat + sqrt(pqmat), as.array(pqmat) + sqrt(as.array(pqmat)))
    checkDuckDBMatrix(pqmat - 1L, as.array(pqmat) - 1L)
    checkDuckDBMatrix(pqmat * 3.14, as.array(pqmat) * 3.14)
    checkDuckDBMatrix(1L / pqmat, 1L / as.array(pqmat))
    checkDuckDBMatrix(3.14 ^ pqmat, 3.14 ^ as.array(pqmat))
    checkDuckDBMatrix(pqmat %% sqrt(pqmat), as.array(pqmat) %% sqrt(as.array(pqmat)))
})

test_that("Compare methods work as expected for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)

    checkDuckDBMatrix(pqmat == sqrt(pqmat), as.array(pqmat) == sqrt(as.array(pqmat)))
    checkDuckDBMatrix(pqmat > 1L, as.array(pqmat) > 1L)
    checkDuckDBMatrix(pqmat < 3.14, as.array(pqmat) < 3.14)
    checkDuckDBMatrix(1L != pqmat, 1L != as.array(pqmat))
    checkDuckDBMatrix(3.14 <= pqmat, 3.14 <= as.array(pqmat))
    checkDuckDBMatrix(pqmat >= sqrt(pqmat), as.array(pqmat) >= sqrt(as.array(pqmat)))
})

test_that("Logic methods work as expected for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)

    ## "&"
    x <- pqmat > 70
    y <- pqmat < 4000
    checkDuckDBMatrix(x & y, as.array(x) & as.array(y))

    ## "|"
    x <- pqmat > 70
    y <- sqrt(pqmat) > 0
    checkDuckDBMatrix(x | y, as.array(x) | as.array(y))
})

test_that("Math methods work as expected for a DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)),
                          dimtbls = state_tables)

    income <- pqmat[, "Income", drop = FALSE]
    ikeep <-
      c("Colorado", "Delaware", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
        "Maine", "Maryland", "Michigan", "Minnesota", "Missouri", "Montana",
        "Nebraska", "Nevada", "New Hampshire", "North Dakota", "Ohio", "Oregon",
        "Pennsylvania", "South Dakota", "Utah", "Vermont", "Washington", "Wisconsin",
        "Wyoming")
    illiteracy <- pqmat[ikeep, "Illiteracy", drop = FALSE]

    checkDuckDBMatrix(abs(income), abs(as.array(income)))
    checkDuckDBMatrix(sqrt(income), sqrt(as.array(income)))
    checkDuckDBMatrix(ceiling(income), ceiling(as.array(income)))
    checkDuckDBMatrix(floor(income), floor(as.array(income)))
    checkDuckDBMatrix(trunc(income), trunc(as.array(income)))

    expect_error(cummax(income))
    expect_error(cummin(income))
    expect_error(cumprod(income))
    expect_error(cumsum(income))

    checkDuckDBMatrix(log(income), log(as.array(income)))
    checkDuckDBMatrix(log10(income), log10(as.array(income)))
    checkDuckDBMatrix(log2(income), log2(as.array(income)))
    checkDuckDBMatrix(log1p(income), log1p(as.array(income)))

    checkDuckDBMatrix(acos(illiteracy), acos(as.array(illiteracy)))
    checkDuckDBMatrix(acosh(income), acosh(as.array(income)))
    checkDuckDBMatrix(asin(illiteracy), asin(as.array(illiteracy)))
    checkDuckDBMatrix(asinh(income), asinh(as.array(income)))
    checkDuckDBMatrix(atan(income), atan(as.array(income)))
    checkDuckDBMatrix(atanh(illiteracy), atanh(as.array(illiteracy)))

    checkDuckDBMatrix(exp(income), exp(as.array(income)))
    checkDuckDBMatrix(expm1(income), expm1(as.array(income)))

    checkDuckDBMatrix(cos(illiteracy), cos(as.array(illiteracy)))
    checkDuckDBMatrix(cosh(illiteracy), cosh(as.array(illiteracy)))

    expect_error(cospi(illiteracy))

    checkDuckDBMatrix(sin(illiteracy), sin(as.array(illiteracy)))
    checkDuckDBMatrix(sinh(illiteracy), sinh(as.array(illiteracy)))

    expect_error(sinpi(illiteracy))

    checkDuckDBMatrix(tan(illiteracy), tan(as.array(illiteracy)))
    checkDuckDBMatrix(tanh(illiteracy), tanh(as.array(illiteracy)))

    expect_error(tanpi(illiteracy))

    checkDuckDBMatrix(gamma(illiteracy), gamma(as.array(illiteracy)))
    checkDuckDBMatrix(lgamma(illiteracy), lgamma(as.array(illiteracy)))

    expect_error(digamma(illiteracy))
    expect_error(trigamma(illiteracy))
})

test_that("matrix multiplication works for DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")

    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))

    # matrix %*% vector (returns DuckDBMatrix n x 1)
    y <- seq_len(ncol(pqmat))
    object <- pqmat %*% y
    expected <- state.x77 %*% y
    dimnames(expected) <- list(rownames(expected), "1")
    names(dimnames(expected)) <- c("index1", "__col__")
    checkDuckDBMatrix(object, expected)

    # vector %*% matrix (returns DuckDBMatrix 1 x m)
    x <- seq_len(nrow(pqmat))
    object <- x %*% pqmat
    expected <- x %*% state.x77
    dimnames(expected) <- list("1", colnames(expected))
    names(dimnames(expected)) <- c("__row__", "index2")
    checkDuckDBMatrix(object, expected)

    # crossprod(matrix, vector) = t(matrix) %*% vector (returns DuckDBMatrix m x 1)
    object <- crossprod(pqmat, x)
    expected <- crossprod(state.x77, x)
    dimnames(expected) <- list(rownames(expected), "1")
    names(dimnames(expected)) <- c("index2", "__row__")
    checkDuckDBMatrix(object, expected)

    # tcrossprod(matrix, vector) = matrix %*% t(vector) (returns DuckDBMatrix n x 1)
    object <- tcrossprod(pqmat, y)
    expected <- state.x77 %*% y
    dimnames(expected) <- list(rownames(expected), "1")
    names(dimnames(expected)) <- c("index1", "__col__")
    checkDuckDBMatrix(object, expected)

    # crossprod(matrix) = t(matrix) %*% matrix via SQL self-join
    object <- crossprod(pqmat)
    expected <- crossprod(state.x77)
    names(dimnames(expected)) <- c("index2_a", "index2_b")
    checkDuckDBMatrix(object, expected)

    # tcrossprod(matrix) = matrix %*% t(matrix) via SQL self-join
    object <- tcrossprod(pqmat)
    expected <- tcrossprod(state.x77)
    names(dimnames(expected)) <- c("index1_a", "index1_b")
    checkDuckDBMatrix(object, expected)
})

test_that("crossprod/tcrossprod self-join has an OOM size tripwire (LKT-59; R<->Python parity with scibis)", {
    pqmat <- DuckDBMatrix(state_path, datacol = "value", row = "index1", col = "index2")
    # Under the default safeguard a small matrix is well below the limit: no trip, a real result is returned.
    expect_s4_class(crossprod(pqmat), "DuckDBMatrix")
    expect_s4_class(tcrossprod(pqmat), "DuckDBMatrix")
    # Lowering the limit trips the guard BEFORE the self-join runs, which
    # monkeypatches _GRAM_PAIR_LIMIT to 1 so any matrix trips.
    old <- options(DuckDBArray.gram_pair_limit = 1)
    on.exit(options(old), add = TRUE)
    expect_error(crossprod(pqmat), "does not scale")
    expect_error(tcrossprod(pqmat), "does not scale")
})

test_that("matrix-matrix multiplication works for DuckDBMatrix", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))

    B <- matrix(seq_len(ncol(pqmat) * 2L), nrow = ncol(pqmat), ncol = 2L)
    expect_equal(as.matrix(pqmat %*% B), state.x77 %*% B, check.attributes = FALSE)

    A <- matrix(seq_len(nrow(pqmat) * 3L), nrow = 3L, ncol = nrow(pqmat))
    expect_equal(as.matrix(A %*% pqmat), A %*% state.x77, check.attributes = FALSE)

    Y2 <- matrix(seq_len(nrow(pqmat) * 2L), nrow = nrow(pqmat), ncol = 2L)
    expect_equal(as.matrix(crossprod(pqmat, Y2)), crossprod(state.x77, Y2),
                 check.attributes = FALSE)

    A2 <- matrix(seq_len(nrow(pqmat) * 3L), nrow = nrow(pqmat), ncol = 3L)
    expect_equal(as.matrix(crossprod(A2, pqmat)), crossprod(A2, state.x77),
                 check.attributes = FALSE)

    Y <- matrix(seq_len(ncol(pqmat) * 2L), nrow = 2L, ncol = ncol(pqmat))
    expect_equal(as.matrix(tcrossprod(pqmat, Y)), tcrossprod(state.x77, Y),
                 check.attributes = FALSE)

    rowmat <- pqmat[1L, , drop = FALSE]
    x <- seq_len(nrow(pqmat))
    expect_equal(as.matrix(tcrossprod(x, rowmat)), outer(x, as.vector(state.x77[1L, ])),
                 check.attributes = FALSE)
})

test_that("matrix multiplication errors on non-conformable operands", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(state_path, datacol = "value",
                          keycols = lapply(dimnames(state.x77),
                                           function(x) setNames(seq_along(x), x)))
    expect_error(pqmat %*% rep(1, ncol(pqmat) + 1L), "non-conformable")
    expect_error(rep(1, nrow(pqmat) + 1L) %*% pqmat, "non-conformable")
})

test_that("rowDeviances works on airway counts DuckDBMatrix", {
    names(dimnames(airway_counts)) <- c("index1", "index2")
    pqmat <- DuckDBMatrix(airway_counts_path, datacol = "value",
                          keycols = lapply(dimnames(airway_counts),
                                           function(x) setNames(seq_along(x), x)))

    # Test Poisson deviance
    dev_pois_ddb <- rowDeviances(pqmat, family = "poisson")
    dev_pois_mem <- rowDeviances(as.matrix(airway_counts), family = "poisson")
    expect_equal(dev_pois_ddb, dev_pois_mem, tolerance = 1e-8)

    # Test Binomial deviance
    dev_binom_ddb <- rowDeviances(pqmat, family = "binomial")
    dev_binom_mem <- rowDeviances(as.matrix(airway_counts), family = "binomial")
    expect_equal(dev_binom_ddb, dev_binom_mem, tolerance = 1e-8)

    # Verify results are sensible
    expect_length(dev_pois_ddb, nrow(airway_counts))
    expect_true(all(dev_pois_ddb >= 0 | is.na(dev_pois_ddb)))
    expect_equal(names(dev_pois_ddb), rownames(airway_counts))

    # Poisson and binomial should be highly correlated
    valid <- dev_pois_ddb > 0 & dev_binom_ddb > 0
    expect_gt(cor(dev_pois_ddb[valid], dev_binom_ddb[valid]), 0.95)
})

test_that("margin stats honor na.rm (NA propagates when na.rm = FALSE)", {
    # A stored NULL/NA in the value column: base MatrixGenerics propagates NA for
    # the default na.rm = FALSE, and removes it for na.rm = TRUE. SQL aggregates
    # always drop NULLs, so the na.rm = FALSE path is emulated with a per-group
    # NA guard. fill = 0 (sparse); row 1 has an NA, row 2 does not.
    df <- data.frame(r = c(1L, 1L, 2L, 2L, 2L),
                     c = c(1L, 3L, 1L, 2L, 3L),
                     v = c(1.0, NA, 4.0, 5.0, 6.0))
    tf <- tempfile(fileext = ".parquet")
    on.exit(unlink(tf))
    arrow::write_parquet(df, tf)
    m <- DuckDBMatrix(tf, datacol = "v", keycols = list(r = 1:2, c = 1:3))

    D <- matrix(0, 2, 3)
    D[1, 1] <- 1; D[1, 3] <- NA; D[2, 1] <- 4; D[2, 2] <- 5; D[2, 3] <- 6

    # Compare values as plain numerics (the DuckDBMatrix result is a named 1-D
    # array; the dense oracle is an unnamed numeric vector).
    eq <- function(a, b) expect_equal(as.numeric(a), as.numeric(b))
    eq(rowSums(m, na.rm = TRUE),  MatrixGenerics::rowSums(D, na.rm = TRUE))
    eq(rowSums(m, na.rm = FALSE), MatrixGenerics::rowSums(D, na.rm = FALSE))
    eq(rowMeans(m, na.rm = FALSE), MatrixGenerics::rowMeans(D, na.rm = FALSE))
    eq(rowMaxs(m, na.rm = FALSE), MatrixGenerics::rowMaxs(D, na.rm = FALSE))
    eq(rowMins(m, na.rm = FALSE), MatrixGenerics::rowMins(D, na.rm = FALSE))
})
