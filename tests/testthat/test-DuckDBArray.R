# Tests the basic functions of a DuckDBArray.
# library(testthat); library(DuckDBArray); source("setup.R"); source("test-DuckDBArray.R")

test_that("basic methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    checkDuckDBArray(pqarray, titanic_array)
    expect_true(is_sparse(pqarray))

    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array), type = "double")
    expect_s4_class(pqarray, "DuckDBArray")
    expect_identical(type(pqarray), "double")
    expect_identical(length(pqarray), length(titanic_array))
    expect_identical(dim(pqarray), dim(titanic_array))
    expect_identical(dimnames(pqarray), dimnames(titanic_array))
    expect_equal(as.array(pqarray), titanic_array)

    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array), type = "character")
    expect_s4_class(pqarray, "DuckDBArray")
    expect_identical(type(pqarray), "character")
    expect_identical(length(pqarray), length(titanic_array))
    expect_identical(dim(pqarray), dim(titanic_array))
    expect_identical(dimnames(pqarray), dimnames(titanic_array))
})

test_that("basic methods work as expected for a sparse DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate",
                           keycols = list(Class = c("1st", "2nd", "Crew"),
                                          Sex = c("Male", "Female"),
                                          Age = "Child", Survived = "No"))
    checkDuckDBArray(pqarray, titanic_array[c("1st", "2nd", "Crew"), c("Male", "Female"), "Child", "No", drop = FALSE])
    expect_true(is_sparse(pqarray))

    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate",
                           keycols = list(Class = c("1st", "2nd", "3rd", "Crew"),
                                          Sex = c("Male", "Female"),
                                          Age = "Child", Survived = "No"))
    checkDuckDBArray(pqarray, titanic_array[, , "Child", "No", drop = FALSE])
    expect_true(is_sparse(pqarray))

    pqarray <- DuckDBArray(sparse_parquet, datacol = "value", keycols = list(dim1 = LETTERS, dim2 = letters, dim3 = month.abb))
    checkDuckDBArray(pqarray, sparse_array)
    expect_true(is_sparse(pqarray))

    pqarray <- DuckDBArray(sparse_parquet, datacol = "value", keycols = list(dim1 = LETTERS, dim2 = letters, dim3 = month.abb), type = "double")
    expect_s4_class(pqarray, "DuckDBArray")
    expect_identical(type(pqarray), "double")
    expect_identical(length(pqarray), length(sparse_array))
    expect_identical(dim(pqarray), dim(sparse_array))
    expect_identical(dimnames(pqarray), dimnames(sparse_array))
    expect_equal(as.array(pqarray), sparse_array)

    pqarray <- DuckDBArray(sparse_parquet, datacol = "value", keycols = list(dim1 = LETTERS, dim2 = letters, dim3 = month.abb), type = "character")
    expect_s4_class(pqarray, "DuckDBArray")
    expect_identical(type(pqarray), "character")
    expect_identical(length(pqarray), length(sparse_array))
    expect_identical(dim(pqarray), dim(sparse_array))
    expect_identical(dimnames(pqarray), dimnames(sparse_array))
})

test_that("renaming dimensions works for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    expected <- titanic_array

    replacements <- lapply(dim(pqarray), function(n) letters[seq_len(n)])
    dimnames(pqarray) <- replacements
    for (i in seq_along(replacements)) {
        dimnames(expected)[[i]] <- replacements[[i]]
    }
    checkDuckDBArray(pqarray, expected)
})

test_that("renaming dimensions works for a sparse DuckDBArray", {
    pqarray <- DuckDBArray(sparse_parquet, datacol = "value", keycols = list(dim1 = LETTERS, dim2 = letters, dim3 = month.abb))
    expected <- sparse_array

    replacements <- lapply(dim(pqarray), function(n) letters[seq_len(n)])
    dimnames(pqarray) <- replacements
    for (i in seq_along(replacements)) {
        dimnames(expected)[[i]] <- replacements[[i]]
    }
    checkDuckDBArray(pqarray, expected)
})

test_that("DuckDBArray can be cast to a different type", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    type(pqarray) <- "double"
    expected <- titanic_array
    storage.mode(expected) <- "double"
    checkDuckDBArray(pqarray, expected)

    pqarray <- DuckDBArray(sparse_parquet, datacol = "value", keycols = list(dim1 = LETTERS, dim2 = letters, dim3 = month.abb))
    type(pqarray) <- "double"
    expected <- sparse_array
    storage.mode(expected) <- "double"
    checkDuckDBArray(pqarray, expected)
})

test_that("nonzero functions work for DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    checkDuckDBArray(is_nonzero(pqarray), is_nonzero(titanic_array))
    expect_equal(nzcount(pqarray), nzcount(titanic_array))
    expect_equal(nzvals(pqarray), nzvals(titanic_array))

    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate",
                           keycols = list(Class = c("1st", "2nd", "Crew"),
                                          Sex = c("Male", "Female"),
                                          Age = "Child", Survived = "No"))
    expected <- titanic_array[c("1st", "2nd", "Crew"), c("Male", "Female"), "Child", "No", drop = FALSE]
    checkDuckDBArray(is_nonzero(pqarray), is_nonzero(expected))
    expect_equal(nzcount(pqarray), nzcount(expected))
    expect_equal(nzvals(pqarray), nzvals(expected))

    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate",
                           keycols = list(Class = c("1st", "2nd", "3rd", "Crew"),
                                          Sex = c("Male", "Female"),
                                          Age = "Child", Survived = "No"))
    expected <- titanic_array[, , "Child", "No", drop = FALSE]
    checkDuckDBArray(pqarray, expected)
    expect_equal(nzcount(pqarray), nzcount(expected))
    expect_equal(nzvals(pqarray), nzvals(expected))
})

test_that("extraction methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    expect_error(pqarray[,])

    object <- pqarray[]
    checkDuckDBArray(object, titanic_array)

    object <- pqarray[, 2:1, , ]
    expected <- titanic_array[, 2:1, , ]
    checkDuckDBArray(object, expected)

    object <- pqarray[c(4, 2), , 1, ]
    expected <- titanic_array[c(4, 2), , 1, ]
    checkDuckDBArray(object, expected)

    object <- pqarray[c(4, 2), , 1, , drop = FALSE]
    expected <- titanic_array[c(4, 2), , 1, , drop = FALSE]
    checkDuckDBArray(object, expected)

    object <- pqarray[4, 2, 1, 2]
    expected <- as.array(titanic_array[4, 2, 1, 2])
    checkDuckDBArray(object, expected)

    object <- pqarray[4, 2, 1, 2, drop = FALSE]
    expected <- titanic_array[4, 2, 1, 2, drop = FALSE]
    checkDuckDBArray(object, expected)

    object <- pqarray[c("1st", "2nd", "3rd"), "Female", "Child", ]
    expected <- titanic_array[c("1st", "2nd", "3rd"), "Female", "Child", ]
    checkDuckDBArray(object, expected)
})

test_that("aperm and t methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    object <- aperm(pqarray, c(4, 2, 1, 3))
    expected <- aperm(titanic_array, c(4, 2, 1, 3))
    checkDuckDBArray(object, expected)

    names(dimnames(state.x77)) <- c("index1", "index2")
    pqarray <- DuckDBArray(state_path, datacol = "value",
                           keycols = lapply(dimnames(state.x77),
                                            function(x) setNames(seq_along(x), x)),
                           dimtbls = state_tables)

    object <- t(pqarray)
    expected <- t(state.x77)
    expect_s4_class(object, "DuckDBArray")
    expect_identical(type(object), "double")
    expect_identical(length(object), length(expected))
    expect_identical(dim(object), dim(expected))
    expect_identical(dimnames(object)[[1L]], dimnames(expected)[[1L]])
    expect_setequal(dimnames(object)[[2L]], dimnames(expected)[[2L]])
    expect_identical(as.array(object)[, colnames(expected)], expected)
})

test_that("Arith methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    checkDuckDBArray(pqarray + sqrt(pqarray), as.array(pqarray) + sqrt(as.array(pqarray)))
    checkDuckDBArray(+ pqarray, + as.array(pqarray))
    checkDuckDBArray(pqarray - 1L, as.array(pqarray) - 1L)
    checkDuckDBArray(- pqarray, - as.array(pqarray))
    checkDuckDBArray(pqarray * 3.14, as.array(pqarray) * 3.14)
    checkDuckDBArray(1L / pqarray, 1L / as.array(pqarray))
    checkDuckDBArray(3.14 ^ pqarray, 3.14 ^ as.array(pqarray))
    checkDuckDBArray(pqarray %% sqrt(pqarray), as.array(pqarray) %% sqrt(as.array(pqarray)))
    checkDuckDBArray(pqarray %/% 3.14, as.array(pqarray) %/% 3.14)
})

test_that("Compare methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    checkDuckDBArray(pqarray == sqrt(pqarray), as.array(pqarray) == sqrt(as.array(pqarray)))
    checkDuckDBArray(pqarray > 1L, as.array(pqarray) > 1L)
    checkDuckDBArray(pqarray < 3.14, as.array(pqarray) < 3.14)
    checkDuckDBArray(1L != pqarray, 1L != as.array(pqarray))
    checkDuckDBArray(3.14 <= pqarray, 3.14 <= as.array(pqarray))
    checkDuckDBArray(pqarray >= sqrt(pqarray), as.array(pqarray) >= sqrt(as.array(pqarray)))
})

test_that("Logic methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    ## "&"
    x <- pqarray > 70
    y <- pqarray < 4000
    checkDuckDBArray(x & y, as.array(x) & as.array(y))

    ## "|"
    x <- pqarray > 70
    y <- sqrt(pqarray) > 0
    checkDuckDBArray(x | y, as.array(x) | as.array(y))

    ## "!"
    x <- pqarray > 70
    checkDuckDBArray(!x, !as.array(x))
})

test_that("Math methods work as expected for a DuckDBArray", {
    names(dimnames(state.x77)) <- c("index1", "index2")
    pqarray <- DuckDBArray(state_path, datacol = "value",
                           keycols = lapply(dimnames(state.x77),
                                            function(x) setNames(seq_along(x), x)),
                           dimtbls = state_tables)

    income <- pqarray[, "Income"]
    ikeep <-
      c("Colorado", "Delaware", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
        "Maine", "Maryland", "Michigan", "Minnesota", "Missouri", "Montana",
        "Nebraska", "Nevada", "New Hampshire", "North Dakota", "Ohio", "Oregon",
        "Pennsylvania", "South Dakota", "Utah", "Vermont", "Washington", "Wisconsin",
        "Wyoming")
    illiteracy <- pqarray[ikeep, "Illiteracy"]

    checkDuckDBArray(abs(income), abs(as.array(income)))
    checkDuckDBArray(sqrt(income), sqrt(as.array(income)))
    checkDuckDBArray(ceiling(income), ceiling(as.array(income)))
    checkDuckDBArray(floor(income), floor(as.array(income)))
    checkDuckDBArray(trunc(income), trunc(as.array(income)))

    expect_error(cummax(income))
    expect_error(cummin(income))
    expect_error(cumprod(income))
    expect_error(cumsum(income))

    checkDuckDBArray(log(income), log(as.array(income)))
    checkDuckDBArray(log10(income), log10(as.array(income)))
    checkDuckDBArray(log2(income), log2(as.array(income)))
    checkDuckDBArray(log1p(income), log1p(as.array(income)))

    checkDuckDBArray(acos(illiteracy), acos(as.array(illiteracy)))
    checkDuckDBArray(acosh(income), acosh(as.array(income)))
    checkDuckDBArray(asin(illiteracy), asin(as.array(illiteracy)))
    checkDuckDBArray(asinh(income), asinh(as.array(income)))
    checkDuckDBArray(atan(income), atan(as.array(income)))
    checkDuckDBArray(atanh(illiteracy), atanh(as.array(illiteracy)))

    checkDuckDBArray(exp(income), exp(as.array(income)))
    checkDuckDBArray(expm1(income), expm1(as.array(income)))

    checkDuckDBArray(cos(illiteracy), cos(as.array(illiteracy)))
    checkDuckDBArray(cosh(illiteracy), cosh(as.array(illiteracy)))

    expect_error(cospi(illiteracy))

    checkDuckDBArray(sin(illiteracy), sin(as.array(illiteracy)))
    checkDuckDBArray(sinh(illiteracy), sinh(as.array(illiteracy)))

    expect_error(sinpi(illiteracy))

    checkDuckDBArray(tan(illiteracy), tan(as.array(illiteracy)))
    checkDuckDBArray(tanh(illiteracy), tanh(as.array(illiteracy)))

    expect_error(tanpi(illiteracy))

    checkDuckDBArray(gamma(illiteracy), gamma(as.array(illiteracy)))
    checkDuckDBArray(lgamma(illiteracy), lgamma(as.array(illiteracy)))

    expect_error(digamma(illiteracy))
    expect_error(trigamma(illiteracy))
})

test_that("Special numeric functions work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(special_path, datacol = "x", keycols = list(id = letters[1:4]))

    checkDuckDBArray(is.finite(pqarray), is.finite(as.array(pqarray)))
    checkDuckDBArray(is.infinite(pqarray), is.infinite(as.array(pqarray)))
    checkDuckDBArray(is.nan(pqarray), is.nan(as.array(pqarray)))
})

test_that("sweep method works as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    object <- sweep(pqarray, 1L, 1:4, "*")
    expected <- sweep(as.array(pqarray), 1L, 1:4, "*")
    checkDuckDBArray(object, expected)

    object <- sweep(pqarray, 2L, 1:2, "*")
    expected <- sweep(as.array(pqarray), 2L, 1:2, "*")
    checkDuckDBArray(object, expected)


    object <- sweep(pqarray, 1L, 1:4, "/")
    expected <- sweep(as.array(pqarray), 1L, 1:4, "/")
    checkDuckDBArray(object, expected)

    object <- sweep(pqarray, 2L, 1:2, "/")
    expected <- sweep(as.array(pqarray), 2L, 1:2, "/")
    checkDuckDBArray(object, expected)


    object <- sweep(pqarray, 1L, 1:4, "%/%")
    expected <- sweep(as.array(pqarray), 1L, 1:4, "%/%")
    storage.mode(expected) <- "numeric"
    checkDuckDBArray(object, expected)

    object <- sweep(pqarray, 2L, 1:2, "%/%")
    expected <- sweep(as.array(pqarray), 2L, 1:2, "%/%")
    storage.mode(expected) <- "numeric"
    checkDuckDBArray(object, expected)


    object <- sweep(pqarray, 1L, 1:4, "%%")
    expected <- sweep(as.array(pqarray), 1L, 1:4, "%%")
    storage.mode(expected) <- "numeric"
    checkDuckDBArray(object, expected)

    object <- sweep(pqarray, 2L, 1:2, "%%")
    expected <- sweep(as.array(pqarray), 2L, 1:2, "%%")
    storage.mode(expected) <- "numeric"
    checkDuckDBArray(object, expected)

    # Test chained sweeps (requires unique column names)
    object <- sweep(sweep(pqarray, 1L, 1:4, "/"), 2L, 1:2, "/")
    expected <- sweep(sweep(as.array(pqarray), 1L, 1:4, "/"), 2L, 1:2, "/")
    checkDuckDBArray(object, expected)

    object <- sweep(sweep(pqarray, 2L, 1:2, "*"), 1L, 1:4, "/")
    expected <- sweep(sweep(as.array(pqarray), 2L, 1:2, "*"), 1L, 1:4, "/")
    checkDuckDBArray(object, expected)
})

test_that("row/colCounts methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    pqarray <- as(pqarray[ , "Female", "Child", ], "DuckDBMatrix")

    # with fill = 0
    object <- rowCounts(pqarray, value = 0L)
    expected <- matrixStats::rowCounts(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowCounts(pqarray, value = 14L)
    expected <- matrixStats::rowCounts(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colCounts(pqarray, value = 0L)
    expected <- matrixStats::colCounts(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colCounts(pqarray, value = 14L)
    expected <- matrixStats::colCounts(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    # with fill != 0
    object <- rowCounts(pqarray + 7L, value = 7L)
    expected <- matrixStats::rowCounts(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowCounts(pqarray + 7L, value = 21L)
    expected <- matrixStats::rowCounts(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colCounts(pqarray + 7L, value = 7L)
    expected <- matrixStats::colCounts(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colCounts(pqarray + 7L, value = 21L)
    expected <- matrixStats::colCounts(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)
})

test_that("row/colMaxs methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    pqarray <- as(pqarray[ , "Female", "Child", ], "DuckDBMatrix")

    # with fill = 0
    object <- rowMaxs(pqarray, value = 0L)
    expected <- matrixStats::rowMaxs(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowMaxs(pqarray, value = 14L)
    expected <- matrixStats::rowMaxs(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMaxs(pqarray, value = 0L)
    expected <- matrixStats::colMaxs(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMaxs(pqarray, value = 14L)
    expected <- matrixStats::colMaxs(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    # with fill != 0
    object <- rowMaxs(pqarray + 7L, value = 7L)
    expected <- matrixStats::rowMaxs(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowMaxs(pqarray + 7L, value = 21L)
    expected <- matrixStats::rowMaxs(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMaxs(pqarray + 7L, value = 7L)
    expected <- matrixStats::colMaxs(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMaxs(pqarray + 7L, value = 21L)
    expected <- matrixStats::colMaxs(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)
})

test_that("row/colMeans methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    # with fill = 0
    object <- rowMeans(pqarray)
    expect_identical(setNames(as.vector(object), names(object)), rowMeans(as.array(pqarray)))
    object <- rowMeans(pqarray, dims = 2L)
    expect_identical(setNames(object, names(object)), rowMeans(as.array(pqarray), dims = 2L))
    object <- rowMeans(pqarray, dims = 3L)
    expect_identical(setNames(object, names(object)), rowMeans(as.array(pqarray), dims = 3L))

    # with fill != 0
    object <- rowMeans(pqarray + 7L)
    expect_identical(setNames(as.vector(object), names(object)), rowMeans(as.array(pqarray) + 7L))
    object <- rowMeans(pqarray + 7L, dims = 2L)
    expect_identical(setNames(object, names(object)), rowMeans(as.array(pqarray) + 7L, dims = 2L))
    object <- rowMeans(pqarray + 7L, dims = 3L)
    expect_identical(setNames(object, names(object)), rowMeans(as.array(pqarray) + 7L, dims = 3L))

    # with fill = 0
    object <- colMeans(pqarray)
    expect_identical(setNames(object, names(object)), colMeans(as.array(pqarray)))
    object <- colMeans(pqarray, dims = 2L)
    expect_identical(setNames(object, names(object)), colMeans(as.array(pqarray), dims = 2L))
    object <- colMeans(pqarray, dims = 3L)
    expect_identical(setNames(as.vector(object), names(object)), colMeans(as.array(pqarray), dims = 3L))

    # with fill != 0
    object <- colMeans(pqarray + 7L)
    expect_identical(setNames(object, names(object)), colMeans(as.array(pqarray) + 7L))
    object <- colMeans(pqarray + 7L, dims = 2L)
    expect_identical(setNames(object, names(object)), colMeans(as.array(pqarray) + 7L, dims = 2L))
    object <- colMeans(pqarray + 7L, dims = 3L)
    expect_identical(setNames(as.vector(object), names(object)), colMeans(as.array(pqarray) + 7L, dims = 3L))
})

test_that("row/colMins methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    pqarray <- as(pqarray[ , "Female", "Child", ], "DuckDBMatrix")

    # with fill = 0
    object <- rowMins(pqarray, value = 0L)
    expected <- matrixStats::rowMins(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowMins(pqarray, value = 14L)
    expected <- matrixStats::rowMins(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMins(pqarray, value = 0L)
    expected <- matrixStats::colMins(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMins(pqarray, value = 14L)
    expected <- matrixStats::colMins(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    # with fill != 0
    object <- rowMins(pqarray + 7L, value = 7L)
    expected <- matrixStats::rowMins(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowMins(pqarray + 7L, value = 21L)
    expected <- matrixStats::rowMins(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMins(pqarray + 7L, value = 7L)
    expected <- matrixStats::colMins(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colMins(pqarray + 7L, value = 21L)
    expected <- matrixStats::colMins(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)
})


test_that("row/colNnzs methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    pqarray <- as(pqarray[ , "Female", "Child", ], "DuckDBMatrix")

    # with fill = 0
    object <- rowNnzs(pqarray, value = 0L)
    expected <- rowNnzs(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowNnzs(pqarray, value = 14L)
    expected <- rowNnzs(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colNnzs(pqarray, value = 0L)
    expected <- colNnzs(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colNnzs(pqarray, value = 14L)
    expected <- colNnzs(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    # with fill != 0
    object <- rowNnzs(pqarray + 7L, value = 7L)
    expected <- rowNnzs(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowNnzs(pqarray + 7L, value = 21L)
    expected <- rowNnzs(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colNnzs(pqarray + 7L, value = 7L)
    expected <- colNnzs(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colNnzs(pqarray + 7L, value = 21L)
    expected <- colNnzs(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)
})

test_that("row/colSums methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))

    # with fill = 0
    object <- rowSums(pqarray)
    expect_identical(setNames(as.vector(object), names(object)), rowSums(as.array(pqarray)))
    object <- rowSums(pqarray, dims = 2L)
    expect_identical(setNames(object, names(object)), rowSums(as.array(pqarray), dims = 2L))
    object <- rowSums(pqarray, dims = 3L)
    expect_identical(setNames(object, names(object)), rowSums(as.array(pqarray), dims = 3L))

    # with fill != 0
    object <- rowSums(pqarray + 7L)
    expect_identical(setNames(as.vector(object), names(object)), rowSums(as.array(pqarray) + 7L))
    object <- rowSums(pqarray + 7L, dims = 2L)
    expect_identical(setNames(object, names(object)), rowSums(as.array(pqarray) + 7L, dims = 2L))
    object <- rowSums(pqarray + 7L, dims = 3L)
    expect_identical(setNames(object, names(object)), rowSums(as.array(pqarray) + 7L, dims = 3L))

    # with fill = 0
    object <- colSums(pqarray)
    expect_identical(setNames(object, names(object)), colSums(as.array(pqarray)))
    object <- colSums(pqarray, dims = 2L)
    expect_identical(setNames(object, names(object)), colSums(as.array(pqarray), dims = 2L))
    object <- colSums(pqarray, dims = 3L)
    expect_identical(setNames(as.vector(object), names(object)), colSums(as.array(pqarray), dims = 3L))

    # with fill != 0
    object <- colSums(pqarray + 7L)
    expect_identical(setNames(object, names(object)), colSums(as.array(pqarray) + 7L))
    object <- colSums(pqarray + 7L, dims = 2L)
    expect_identical(setNames(object, names(object)), colSums(as.array(pqarray) + 7L, dims = 2L))
    object <- colSums(pqarray + 7L, dims = 3L)
    expect_identical(setNames(as.vector(object), names(object)), colSums(as.array(pqarray) + 7L, dims = 3L))
})

test_that("row/colSds methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    pqarray <- as(pqarray[ , "Female", "Child", ], "DuckDBMatrix")

    # with fill = 0
    object <- rowSds(pqarray, value = 0L)
    expected <- matrixStats::rowSds(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowSds(pqarray, value = 14L)
    expected <- matrixStats::rowSds(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colSds(pqarray, value = 0L)
    expected <- matrixStats::colSds(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colSds(pqarray, value = 14L)
    expected <- matrixStats::colSds(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    # with fill != 0
    object <- rowSds(pqarray + 7L, value = 7L)
    expected <- matrixStats::rowSds(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowSds(pqarray + 7L, value = 21L)
    expected <- matrixStats::rowSds(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colSds(pqarray + 7L, value = 7L)
    expected <- matrixStats::colSds(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colSds(pqarray + 7L, value = 21L)
    expected <- matrixStats::colSds(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)
})

test_that("row/colVars methods work as expected for a DuckDBArray", {
    pqarray <- DuckDBArray(titanic_parquet, datacol = "fate", keycols = dimnames(titanic_array))
    pqarray <- as(pqarray[ , "Female", "Child", ], "DuckDBMatrix")

    # with fill = 0
    object <- rowVars(pqarray, value = 0L)
    expected <- matrixStats::rowVars(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowVars(pqarray, value = 14L)
    expected <- matrixStats::rowVars(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colVars(pqarray, value = 0L)
    expected <- matrixStats::colVars(as.matrix(pqarray), value = 0L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colVars(pqarray, value = 14L)
    expected <- matrixStats::colVars(as.matrix(pqarray), value = 14L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    # with fill != 0
    object <- rowVars(pqarray + 7L, value = 7L)
    expected <- matrixStats::rowVars(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- rowVars(pqarray + 7L, value = 21L)
    expected <- matrixStats::rowVars(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colVars(pqarray + 7L, value = 7L)
    expected <- matrixStats::colVars(as.matrix(pqarray + 7L), value = 7L)
    expect_equal(setNames(as.vector(object), names(object)), expected)

    object <- colVars(pqarray + 7L, value = 21L)
    expected <- matrixStats::colVars(as.matrix(pqarray + 7L), value = 21L)
    expect_equal(setNames(as.vector(object), names(object)), expected)
})
