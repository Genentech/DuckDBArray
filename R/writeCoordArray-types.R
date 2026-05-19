### Nothing in this file is exported.
#
# Type inference and conversion utilities for writeCoordArray.
# Handles Arrow types, DuckDB types, R storage modes, and append mode
# schema reconciliation.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Type inference
###

### Compute index column types
# Returns a list of arrow DataTypes (one per dimension), one for each
# index column. When 'max_dim' is supplied it is used verbatim as the
# upper bound; otherwise dim(x) is used, widened along 'along' by
# 'offset' in append mode so that shifted coordinates still fit.
#' @importFrom DuckDBDataFrame arrowIntType
.computeIdxtypes <- function(dim_x, max_dim, append, along, offset) {
    if (!is.null(max_dim)) {
        dim_bound <- max_dim
    } else {
        dim_bound <- dim_x
        if (append) {
            dim_bound[along] <- dim_bound[along] + offset
        }
    }
    lapply(seq_along(dim_bound),
           function(j) arrowIntType(c(0L, dim_bound[j])))
}

# Predicate: is 'vals' (a vector) an integer-valued numeric vector with
# no NAs interfering in the 'floor(x) == x' check? Integer vectors pass
# trivially; double vectors pass only if every value equals its floor.
.isIntegerValued <- function(vals) {
    is.integer(vals) ||
        (is.numeric(vals) && all(vals == floor(vals), na.rm = TRUE))
}

# Infer an arrow DataType for the value column by scanning blocks of 'x'
# to find the range of non-zero values. Returns the narrowest integer
# type capable of representing the range, or NULL when any block
# contains non-integer-valued data (in which case the caller should fall
# back to letting arrow auto-pick 'double').
#' @importFrom DelayedArray blockApply
#' @importFrom DuckDBDataFrame arrowIntType
#' @importFrom SparseArray nzvals
.inferValueType <- function(x, grid, BPPARAM) {
    block_range <- function(block) {
        vals <- c(0L, nzvals(block))
        if (!.isIntegerValued(vals)) {
            stop("not an integer array")
        }
        range(vals, na.rm = TRUE)
    }

    if (length(grid) == 1L) {
        vals <- c(0L, nzvals(x))
        if (!.isIntegerValued(vals)) return(NULL)
        return(arrowIntType(range(vals, na.rm = TRUE)))
    }

    ranges <- try(blockApply(x, FUN = block_range,
                             grid = grid, as.sparse = TRUE,
                             BPPARAM = BPPARAM, verbose = NA),
                  silent = TRUE)
    if (inherits(ranges, "try-error")) return(NULL)

    min_x <- min(vapply(ranges, `[`, numeric(1L), 1L))
    max_x <- max(vapply(ranges, `[`, numeric(1L), 2L))
    arrowIntType(c(min_x, max_x))
}

# Pick the narrowest arrow integer type that can represent every value
# in 'range_x'. Used for both value-column inference and index-column
# type selection.
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Type conversion (Arrow ↔ DuckDB)
###

# Convert DuckDB type string to Arrow DataType
#' @importFrom arrow int8 int16 int32 int64 uint8 uint16 uint32 uint64
#' @importFrom arrow float32 float64 utf8 bool
#' @importFrom arrow date32 timestamp time32 duration binary
#' @importFrom arrow infer_type
.duckdbTypeToArrow <- function(duckdb_type) {
    # Counterpart to DuckDBDataFrame::.duckdb_type_to_r() (for R representation)
    type <- tolower(duckdb_type)

    # Handle DuckDB complex types
    if (grepl("^(list<.*>|struct[<(].*[>)]|map<.*,.*>)$", type)) {
        return(infer_type(list()))
    }

    if (grepl("^array<.*,\\d+>$", type)) {
        return(infer_type(list()))
    }

    # Scalar types
    switch(type,
           # Boolean
           "boolean" = bool(),

           # Integer types (signed)
           "tinyint" = int8(),
           "smallint" = int16(),
           "integer" = int32(),
           "bigint" = int64(),
           "hugeint" = int64(),

           # Integer types (unsigned)
           "utinyint" = uint8(),
           "usmallint" = uint16(),
           "uinteger" = uint32(),
           "ubigint" = uint64(),
           "uhugeint" = uint64(),

           # Floating point
           "float" = float32(),
           "real" = float32(),
           "double" = float64(),
           "decimal" = float64(),

           # String types
           "varchar" = utf8(),
           "char" = utf8(),
           "bpchar" = utf8(),
           "text" = utf8(),
           "string" = utf8(),

           # Temporal types
           "date" = date32(),
           "timestamp" = timestamp("us", timezone = "UTC"),
           "time" = time32("s"),
           "interval" = duration("s"),

           # Binary types
           "blob" = binary(),
           "bytea" = binary(),

           # Geometry (WKB encoding)
           "geometry" = binary(),
           "geometry_type" = utf8(),

           # Fallback for R class names (from schema probe)
           "integer" = int32(),
           "integer64" = int64(),
           "numeric" = float64(),
           "double" = float64(),
           "character" = utf8(),
           "logical" = bool(),

           # Default fallback: return float64 for unknown types
           # (avoids noisy warnings in production for rare/custom types)
           float64())
}

# Convert Arrow type to format string
.arrowTypeToFormat <- function(arrow_type) {
    type_name <- arrow_type$ToString()

    formats <- c("int8" = "int8",
                 "int16" = "int16",
                 "int32" = "int32",
                 "int64" = "int64",
                 "uint8" = "uint8",
                 "uint16" = "uint16",
                 "uint32" = "uint32",
                 "uint64" = "uint64",
                 "binary" = "binary",
                 "large_binary" = "binary")

    if (type_name %in% names(formats)) {
        formats[[type_name]]
    } else {
        NULL
    }
}

# Convert Arrow type to DuckDB type name for CREATE TABLE statements
.arrowToDuckDBTypeName <- function(arrow_type) {
    format_str <- .arrowTypeToFormat(arrow_type)
    if (is.null(format_str)) {
        return("INTEGER")  # fallback
    }

    switch(format_str,
           "int8" = "TINYINT",
           "int16" = "SMALLINT",
           "int32" = "INTEGER",
           "int64" = "BIGINT",
           "uint8" = "UTINYINT",
           "uint16" = "USMALLINT",
           "uint32" = "UINTEGER",
           "uint64" = "UBIGINT",
           "INTEGER")  # fallback
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Append mode schema reconciliation
###

#' @importFrom DuckDBDataFrame readParquetSchema
.readExistingSchema <- function(path, indexcols, datacol) {
    required <- c(indexcols, datacol)
    sch <- readParquetSchema(path, columns = required)
    list(idxtypes = lapply(indexcols,
                           function(nm) sch$GetFieldByName(nm)$type),
         datatype = sch$GetFieldByName(datacol)$type)
}

.reconcileAppendSchema <-
function(path, indexcols, datacol, arrowtype, max_dim, idxtypes)
{
    existing <- .readExistingSchema(path, indexcols, datacol)

    if (is.null(arrowtype)) {
        arrowtype <- existing$datatype
    } else if (!arrowtype$Equals(existing$datatype)) {
        stop("'append = TRUE' schema mismatch on '", datacol,
             "': existing type is ", existing$datatype$ToString(),
             ", supplied 'arrowtype' is ", arrowtype$ToString())
    }

    if (is.null(max_dim)) {
        idxtypes <- existing$idxtypes
    } else {
        for (j in seq_along(indexcols)) {
            if (!idxtypes[[j]]$Equals(existing$idxtypes[[j]])) {
                stop("'append = TRUE' schema mismatch on column '",
                     indexcols[j], "': existing type is ",
                     existing$idxtypes[[j]]$ToString(),
                     ", supplied 'max_dim' implies ",
                     idxtypes[[j]]$ToString())
            }
        }
    }

    list(arrowtype = arrowtype, idxtypes = idxtypes)
}
