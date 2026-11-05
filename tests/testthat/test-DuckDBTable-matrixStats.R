# Tests for .marginVars()'s generalized single-pass fill != 0 formula.
# library(testthat); library(DuckDBArray); source("setup.R"); source("test-DuckDBTable-matrixStats.R")

# A verbatim copy of the two-pass fill != 0 .marginVars() implementation that
# was replaced by the single-pass generalization (as of commit 62de507, the
# last commit before that fix), kept here only as an independent regression
# oracle for the new formula.
.marginVars_twopass_reference <- function(x, na.rm = FALSE, dims = 1, fill = 0,
                                          margin = c("row", "col")) {
    lst <- DuckDBArray:::.marginSetup(x, dims = dims, margin = margin)
    keycols <- lst$keycols; groups <- lst$groups; k <- lst$k
    datacols <- x@datacols
    nfill <- call("(", call("-", k, call("n")))

    if (fill == 0) {
        conn <- tblconn(x, select = FALSE)
        aggr <- stats::setNames(lapply(names(datacols), function(nm) {
            y <- datacols[[nm]]
            n <- call("n")
            sum_y <- call("sum", y, na.rm = TRUE)
            var_samp <- call("var_samp", y)
            var_samp_term <- call("*", call("coalesce", var_samp, 0),
                                  call("(", call("-", n, 1L)))
            sum_sq_term <- call("*", call("*", sum_y, sum_y),
                               call("(", call("-", call("/", 1, n), call("/", 1, k))))
            expr <- call("/", call("(", call("+", var_samp_term, sum_sq_term)), k - 1L)
            DuckDBArray:::.na_rm_guard(expr, y, na.rm)
        }), names(datacols))
        conn <- dplyr::summarize(dplyr::group_by(conn, !!!groups), !!!aggr)
        datacols <- as.expression(stats::setNames(lapply(names(aggr), as.name), names(aggr)))
        return(S4Vectors::new2("DuckDBTable", conn = conn, datacols = datacols,
                               keycols = keycols, check = FALSE))
    }

    aggr <- stats::setNames(lapply(datacols, function(y)
                   call("/",
                        call("(",
                             call("+",
                                  call("sum", y, na.rm = TRUE),
                                  call("*", fill, nfill))),
                        k)), names(datacols))
    conn <- tblconn(x, select = FALSE)
    mean_names <- vapply(names(aggr), function(nm) {
        tail(make.unique(c(colnames(conn), paste0(nm, "_mean")), sep = "_"), 1L)
    }, character(1L))
    names(aggr) <- mean_names

    conn <- dplyr::left_join(conn, dplyr::summarize(dplyr::group_by(conn, !!!groups), !!!aggr),
                             by = names(keycols))

    aggr <- stats::setNames(lapply(names(datacols), function(nm) {
        y <- datacols[[nm]]
        y_mean <- as.name(mean_names[[nm]])
        y_mean_agg <- call("any_value", y_mean)
        dev <- call("(", call("-", y, y_mean))
        sum_dev_sq <- call("sum", call("*", dev, dev), na.rm = TRUE)
        fill_dev <- call("(", call("-", fill, y_mean_agg))
        zero_contrib <- call("*", call("(", call("*", fill_dev, fill_dev)), nfill)
        expr <- call("/", call("(", call("+", sum_dev_sq, zero_contrib)), k - 1L)
        DuckDBArray:::.na_rm_guard(expr, y, na.rm)
    }), names(datacols))

    conn <- dplyr::summarize(dplyr::group_by(conn, !!!groups), !!!aggr)
    datacols <- as.expression(stats::setNames(lapply(names(aggr), as.name), names(aggr)))
    S4Vectors::new2("DuckDBTable", conn = conn, datacols = datacols, keycols = keycols, check = FALSE)
}

# Builds a small sparse (fill-valued) 2D DuckDBTable fixture plus the dense
# reference matrix it was derived from.
.make_margin_fixture <- function(seed, n_rows, n_cols, mean, sd, sparsity, decimals = 6) {
    set.seed(seed)
    rows <- paste0("r", seq_len(n_rows))
    cols <- paste0("c", seq_len(n_cols))
    m <- matrix(0, nrow = n_rows, ncol = n_cols, dimnames = list(idx1 = rows, idx2 = cols))
    m[] <- round(stats::rnorm(n_rows * n_cols, mean = mean, sd = sd), decimals)
    zero_idx <- sample(seq_len(n_rows * n_cols), floor(n_rows * n_cols * sparsity))
    m[zero_idx] <- 0
    df <- do.call(expand.grid, c(dimnames(m), stringsAsFactors = FALSE))
    df$value <- as.vector(m)
    df <- df[df$value != 0, ]
    tf <- tempfile(fileext = ".parquet")
    arrow::write_parquet(df, tf)
    arr <- DuckDBArray(tf, datacol = "value", keycols = dimnames(m))
    mat <- as(arr, "DuckDBMatrix")
    list(m = m, tbl = mat@seed@table)
}

.collect_margin <- function(ans, keycol) {
    res <- as.data.frame(dplyr::collect(tblconn(ans)))
    res[order(res[[keycol]]), ]
}

test_that("generalized single-pass rowVars/colVars matches the old two-pass formula and base R", {
    f <- .make_margin_fixture(42, 6, 5, mean = 10, sd = 3, sparsity = 0.4)
    for (fill in c(0, 5, -3.5, 100)) {
        for (margin in c("row", "col")) {
            keycol <- if (margin == "row") "idx1" else "idx2"
            new_ans <- DuckDBArray:::.marginVars(f$tbl, fill = fill, margin = margin)
            old_ans <- .marginVars_twopass_reference(f$tbl, fill = fill, margin = margin)
            new_df <- .collect_margin(new_ans, keycol)
            old_df <- .collect_margin(old_ans, keycol)
            m2 <- f$m; m2[m2 == 0] <- fill
            oracle_fun <- if (margin == "row") matrixStats::rowVars else matrixStats::colVars
            oracle <- oracle_fun(m2)[new_df[[keycol]]]

            expect_equal(new_df$value, old_df$value, tolerance = 1e-9,
                        info = sprintf("fill=%s margin=%s vs old two-pass formula", fill, margin))
            expect_equal(new_df$value, unname(oracle), tolerance = 1e-9,
                        info = sprintf("fill=%s margin=%s vs base R oracle", fill, margin))
        }
    }
})

test_that("fill == 0 output is unchanged (generalized formula reduces to the previous closed form)", {
    f <- .make_margin_fixture(123, 7, 4, mean = 2, sd = 1, sparsity = 0.5)
    for (margin in c("row", "col")) {
        keycol <- if (margin == "row") "idx1" else "idx2"
        new_ans <- DuckDBArray:::.marginVars(f$tbl, fill = 0, margin = margin)
        old_ans <- .marginVars_twopass_reference(f$tbl, fill = 0, margin = margin)
        new_df <- .collect_margin(new_ans, keycol)
        old_df <- .collect_margin(old_ans, keycol)
        expect_equal(new_df$value, old_df$value, tolerance = 1e-12)
    }
})

test_that("adversarial high-mean/low-relative-variance case stays non-catastrophic", {
    # Values ~1e8 with a genuine spread of only 0.01: stresses the
    # fill-correction cancellation when fill is close to the true group mean.
    # The residual error here is bounded by float64's representable precision
    # at this magnitude (~1e8 * 2^-52 ~ 2e-8 absolute), not by the fix's
    # algebra -- base R's own var() on the same double-precision values is
    # equally bounded, so this is not something Fix 1 can or should eliminate.
    f <- .make_margin_fixture(7, 8, 6, mean = 1e8, sd = 0.01, sparsity = 0.3)
    for (fill in c(0, 1e8, 1e8 + 50, -1e8)) {
        for (margin in c("row", "col")) {
            keycol <- if (margin == "row") "idx1" else "idx2"
            new_ans <- DuckDBArray:::.marginVars(f$tbl, fill = fill, margin = margin)
            old_ans <- .marginVars_twopass_reference(f$tbl, fill = fill, margin = margin)
            new_df <- .collect_margin(new_ans, keycol)
            old_df <- .collect_margin(old_ans, keycol)
            m2 <- f$m; m2[m2 == 0] <- fill
            oracle_fun <- if (margin == "row") matrixStats::rowVars else matrixStats::colVars
            oracle <- oracle_fun(m2)[new_df[[keycol]]]
            # Not bit-exact at this magnitude, but must stay in the same
            # ballpark as both the old formula and the base R oracle.
            expect_equal(new_df$value, unname(oracle), tolerance = 1e-4)
            expect_equal(new_df$value, old_df$value, tolerance = 1e-4)
        }
    }
})

test_that("COUNT(*) collects as integer64 and silently corrupts arithmetic if not cast", {
    f <- .make_margin_fixture(55, 5, 4, mean = 10, sd = 2, sparsity = 0.4)
    conn <- tblconn(f$tbl, select = FALSE)
    raw <- conn |>
        dplyr::group_by(idx1) |>
        dplyr::summarize(n = dplyr::n(), sum_y = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::collect()
    raw <- raw[order(raw$idx1), ]

    # The hazard is real on this package's own connection (bigint = "integer64",
    # set by DuckDBDataFrame::acquireDuckDBConn()): COUNT(*) collects as bit64.
    expect_s3_class(raw$n, "integer64")

    k <- ncol(f$m)
    # Uncast (buggy) arithmetic: stripping the integer64 class via unclass()
    # reinterprets its internal bit pattern as a plain double, producing tiny
    # nonsense values -- not float noise, a silent wrong answer.
    n_bugged <- unclass(raw$n)
    expect_true(all(n_bugged != as.double(raw$n)))
    fill <- 3
    mu_bugged <- (raw$sum_y + fill * (k - n_bugged)) / k
    var_bugged <- (n_bugged * (raw$sum_y / n_bugged - mu_bugged)^2) / (k - 1)

    # Correct oracle: cast to as.double() immediately after collect(), before
    # any arithmetic.
    m2 <- f$m; m2[m2 == 0] <- fill
    expected <- matrixStats::rowVars(m2)[raw$idx1]

    new_ans <- DuckDBArray:::.marginVars(f$tbl, fill = fill, margin = "row")
    new_df <- .collect_margin(new_ans, "idx1")

    expect_equal(new_df$value, unname(expected), tolerance = 1e-9)
    # This is the test that would fail if the production formula were ever
    # refactored to collect() n without casting it: its output would drift
    # toward the uncast var_bugged values instead of matching `expected`.
    expect_false(isTRUE(all.equal(var_bugged, new_df$value, tolerance = 1e-6)))
})

test_that("rowSds/colSds (sqrt of the generalized rowVars/colVars) stay correct", {
    f <- .make_margin_fixture(321, 5, 5, mean = 4, sd = 1.5, sparsity = 0.4)
    for (fill in c(0, 2)) {
        for (margin in c("row", "col")) {
            keycol <- if (margin == "row") "idx1" else "idx2"
            ans <- DuckDBArray:::.marginSds(f$tbl, fill = fill, margin = margin)
            df <- .collect_margin(ans, keycol)
            m2 <- f$m; m2[m2 == 0] <- fill
            oracle_fun <- if (margin == "row") matrixStats::rowSds else matrixStats::colSds
            oracle <- oracle_fun(m2)[df[[keycol]]]
            expect_equal(df$value, unname(oracle), tolerance = 1e-9)
        }
    }
})

test_that("na.rm semantics are preserved by the merged single-pass formula", {
    f <- .make_margin_fixture(11, 5, 4, mean = 5, sd = 1, sparsity = 0.3)
    m_na <- f$m
    stored_idx <- which(m_na[1, ] != 0)[1]
    m_na[1, stored_idx] <- NA_real_
    df <- do.call(expand.grid, c(dimnames(m_na), stringsAsFactors = FALSE))
    df$value <- as.vector(m_na)
    keep <- !is.na(df$value) & df$value != 0
    keep <- keep | (df$idx1 == "r1" & df$idx2 == colnames(m_na)[stored_idx])
    df <- df[keep, ]
    tf <- tempfile(fileext = ".parquet")
    arrow::write_parquet(df, tf)
    arr <- DuckDBArray(tf, datacol = "value", keycols = dimnames(m_na))
    mat <- as(arr, "DuckDBMatrix")
    tbl_na <- mat@seed@table

    ans_false <- DuckDBArray:::.marginVars(tbl_na, fill = 0, na.rm = FALSE, margin = "row")
    df_false <- .collect_margin(ans_false, "idx1")
    expect_true(is.na(df_false$value[df_false$idx1 == "r1"]))

    ans_true <- DuckDBArray:::.marginVars(tbl_na, fill = 0, na.rm = TRUE, margin = "row")
    df_true <- .collect_margin(ans_true, "idx1")
    expect_false(is.na(df_true$value[df_true$idx1 == "r1"]))
})
