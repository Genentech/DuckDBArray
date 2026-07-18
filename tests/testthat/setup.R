# Pin DuckDB to a single thread so parallel float reductions (sum / var_samp)
# accumulate in a fixed order. Otherwise a reduction can differ in its last ULP
# run-to-run and flake a tight-tolerance expectation. Test-harness only (real
# sessions use all cores); applied via configureOutOfCore() at connection setup,
# so it must be set before the first acquireDuckDBConn() call below.
options(DuckDBDataFrame.threads = 1L)

# State dataset
state_df <- data.frame(
  index1 = rep(rownames(state.x77), times = ncol(state.x77)),
  index2 = rep(colnames(state.x77), each = nrow(state.x77)),
  value = as.vector(state.x77)
)
state_df <- subset(state_df, value != 0)
state_path <- file.path(tempfile(), "state")
state_grid <- RegularArrayGrid(dim(state.x77), c(10, 4))
writeCoordArray(state.x77, state_path, grid = state_grid)
state_tables <- createDimTables(state.x77, grid = state_grid)


# Titanic dataset
titanic_array <- unclass(Titanic)
storage.mode(titanic_array) <- "integer"
titanic_df <- do.call(expand.grid, c(dimnames(Titanic), stringsAsFactors = FALSE))
titanic_df$fate <- as.integer(Titanic[as.matrix(titanic_df)])
titanic_df <- titanic_df[titanic_df$fate != 0L, ]
titanic_csv <- tempfile(fileext = ".csv")
write.csv(titanic_df, titanic_csv, row.names = FALSE)
titanic_csv_gz <- tempfile(fileext = ".csv.gz")
write.csv(titanic_df, gzfile(titanic_csv_gz), row.names = FALSE)
titanic_parquet <- tempfile(fileext = ".parquet")
arrow::write_parquet(titanic_df, titanic_parquet)


# Airway counts dataset
data(airway, package = "airway")
airway_counts <- SummarizedExperiment::assay(airway, "counts")
airway_counts_path <- file.path(tempfile(), "airway_counts")
writeCoordArray(airway_counts, airway_counts_path)


# Random array
set.seed(123)
sparse_df <- data.frame(dim1 = sample(LETTERS, 1000, replace = TRUE),
                        dim2 = sample(letters, 1000, replace = TRUE),
                        dim3 = sample(month.abb, 1000, replace = TRUE),
                        value = sample(100L, 1000, replace = TRUE))
sparse_df <- sparse_df[!duplicated(sparse_df[,1:3]), ]
sparse_df <- sparse_df[order(sparse_df$dim1, sparse_df$dim2, sparse_df$dim3),]
rownames(sparse_df) <- NULL
sparse_array <- array(0L, dim = c(26L, 26L, 12L), dimnames = list(dim1 = LETTERS, dim2 = letters, dim3 = month.abb))
sparse_array[as.matrix(sparse_df[,1:3])] <- sparse_df[["value"]]
sparse_csv <- tempfile(fileext = ".csv")
write.csv(sparse_df, sparse_csv, row.names = FALSE)
sparse_csv_gz <- tempfile(fileext = ".csv.gz")
write.csv(sparse_df, gzfile(sparse_csv_gz), row.names = FALSE)
sparse_parquet <- tempfile(fileext = ".parquet")
arrow::write_parquet(sparse_df, sparse_parquet)


# Special characters
special_df <- data.frame(id = letters[1:4], x = c(-Inf, 0, Inf, NaN))
special_path <- tempfile(fileext = ".parquet")
arrow::write_parquet(special_df, special_path)


# Helper functions
checkDuckDBArraySeed <- function(object, expected) {
    expect_true(validObject(object))
    expect_s4_class(object, "DuckDBArraySeed")
    expect_identical(dbconn(object), acquireDuckDBConn())
    expect_s3_class(tblconn(object), "tbl_duckdb_connection")
    expect_identical(type(object), type(expected))
    expect_identical(length(object), length(expected))
    expect_identical(dim(object), dim(expected))
    expect_identical(dimnames(object), dimnames(expected))
    expect_equal(as.array(object), expected)
}

checkDuckDBArray <- function(object, expected) {
    expect_true(validObject(object))
    expect_s4_class(object, "DuckDBArray")
    expect_identical(dbconn(object), acquireDuckDBConn())
    expect_s3_class(tblconn(object), "tbl_duckdb_connection")
    expect_identical(type(object), type(expected))
    expect_identical(length(object), length(expected))
    expect_identical(dim(object), dim(expected))
    expect_identical(dimnames(object), dimnames(expected))
    expect_equal(as.array(object), expected)
    expect_equivalent(as(object, "SparseArray"), as(expected, "COO_SparseArray"))
    expect_equivalent(as(object, "COO_SparseArray"), as(expected, "COO_SparseArray"))
}

checkDuckDBMatrix <- function(object, expected) {
    expect_true(validObject(object))
    expect_s4_class(object, "DuckDBMatrix")
    expect_identical(dbconn(object), acquireDuckDBConn())
    expect_s3_class(tblconn(object), "tbl_duckdb_connection")
    expect_identical(type(object), typeof(expected))
    expect_identical(length(object), length(expected))
    expect_identical(dim(object), dim(expected))
    expect_identical(dimnames(object), dimnames(expected))
    expect_equal(as.matrix(object), expected)
    expect_equal(as(object, "CsparseMatrix"), as(expected, "CsparseMatrix"))
    expect_equivalent(as(object, "SparseMatrix"), as(expected, "COO_SparseMatrix"))
    expect_equivalent(as(object, "COO_SparseMatrix"), as(expected, "COO_SparseMatrix"))
}
