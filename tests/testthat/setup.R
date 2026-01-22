# writeParquet snippet from BiocDuckDB
setGeneric("writeParquet", signature = "x",
function(x, path, ...)
{
  standardGeneric("writeParquet")
})

.writeCoordArray <- function(x, path, indexcols, datacol, ...) {
    # Create a list of columns containing the non-zero values and their indices
    lst <- apply(nzwhich(x, arr.ind = TRUE), 2L, identity, simplify = FALSE)
    names(lst) <- indexcols
    lst[[datacol]] <- nzvals(x)

    # Map back to the original indices
    indices <- lapply(dimnames(x), as.integer)
    for (j in seq_along(indices)) {
        lst[[j]] <- indices[[j]][lst[[j]]]
    }

    # Convert to a data frame
    class(lst) <- "data.frame"
    attr(lst, "row.names") <- .set_row_names(length(lst[[1L]]))

    arrow::write_dataset(lst, path, format = "parquet", compression = "zstd",
                         compression_level = 3L, partitioning = NULL,
                         min_rows_per_group = 491520L, ...)

    invisible(NULL)
}

setMethod("writeParquet", "ANY",
function(x,
         path,
         indexcols = names(dimnames(x)) %||% sprintf("index%d", seq_along(dim(x))),
         indexrefs = NULL,
         datacol = "value",
         grid = defaultAutoGrid(COO_SparseArray(dim(x))),
         grid_suffix = "_group",
         BPPARAM = getAutoBPPARAM(),
         ...)
{
    if (is.null(dim(x))) {
        stop("the default method of writeParquet requires 'x' to be array-like")
    }

    if (!(is.null(indexrefs) || length(indexrefs) == length(indexcols))) {
        stop("'indexrefs' must be NULL or a list of length(indexcols)")
    }

    if (inherits(x, "table")) {
        x <- unclass(x)
    }

    # Make column names unique
    unique_names <- make.unique(c(indexcols, datacol), sep = "_")
    indexcols <- head(unique_names, -1L)
    datacol <- tail(unique_names, 1L)

    # Get dimensions of the array for storage optimization
    dim_x <- dim(x)

    # Manage dimnames
    dimnames_x <- dimnames(x) %||% lapply(dim(x), function(d) NULL)
    dimnames(x) <- lapply(dim(x), function(d) as.character(seq_len(d)))

    if (length(grid) == 1L) {
        .writeCoordArray(x, path = path, indexcols = indexcols,
                         datacol = datacol, ...)
    } else {
        FUN <- function(x, path, indexcols, datacol, grid_suffix, ...)
        {
            grid <- effectiveGrid()
            viewport <- currentViewport()
            group <- as.vector(mapToGrid(start(viewport), grid)[["major"]])
            subdir <- paste0(indexcols, grid_suffix, "=", group)
            path <- do.call(file.path, c(list(path), subdir))
            .writeCoordArray(x, path = path, indexcols = indexcols,
                             datacol = datacol, ...)
        }
        blockApply(x, FUN = FUN,
                   path = path,
                   indexcols = indexcols,
                   datacol = datacol,
                   grid_suffix = grid_suffix,
                   ...,
                   grid = grid,
                   as.sparse = TRUE,
                   BPPARAM = BPPARAM,
                   verbose = NA)
    }

    invisible(NULL)
})

# State dataset
state_df <- data.frame(
  index1 = rep(rownames(state.x77), times = ncol(state.x77)),
  index2 = rep(colnames(state.x77), each = nrow(state.x77)),
  value = as.vector(state.x77)
)
state_df <- subset(state_df, value != 0)
state_path <- file.path(tempfile(), "state")
state_grid <- RegularArrayGrid(dim(state.x77), c(10, 4))
writeParquet(state.x77, state_path, grid = state_grid)
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
writeParquet(airway_counts, airway_counts_path)


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
