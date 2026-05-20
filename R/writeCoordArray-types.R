### Nothing in this file is exported.
#
# Type inference and conversion utilities for writeCoordArray.
# Handles Arrow types, DuckDB types, R storage modes, and append mode
# schema reconciliation.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### CoordSchema (internal)
###
# schema <- list(
#   index = named character vector of Arrow type names (one per index column),
#   value = character Arrow type name or NULL (auto-pick at write time)
# )

#' @importFrom DuckDBDataFrame arrowIntType arrowTypeFromName arrowTypeToName
#' @importFrom DuckDBDataFrame reconcileParquetSchema
.computeIndexTypeNames <- function(dim_x, max_dim, append, along, offset) {
    if (!is.null(max_dim)) {
        dim_bound <- max_dim
    } else {
        dim_bound <- dim_x
        if (append) {
            dim_bound[along] <- dim_bound[along] + offset
        }
    }
    vapply(seq_along(dim_bound),
           function(j) arrowTypeToName(arrowIntType(c(0L, dim_bound[j]))),
           character(1L))
}

.isIntegerValued <- function(vals) {
    is.integer(vals) ||
        (is.numeric(vals) && all(vals == floor(vals), na.rm = TRUE))
}

#' @importFrom DelayedArray blockApply
#' @importFrom DuckDBDataFrame arrowIntType arrowTypeToName
#' @importFrom SparseArray nzvals
.inferValueTypeName <- function(x, grid, BPPARAM) {
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
        return(arrowTypeToName(arrowIntType(range(vals, na.rm = TRUE))))
    }

    ranges <- try(blockApply(x, FUN = block_range,
                             grid = grid, as.sparse = TRUE,
                             BPPARAM = BPPARAM, verbose = NA),
                  silent = TRUE)
    if (inherits(ranges, "try-error")) return(NULL)

    min_x <- min(vapply(ranges, `[`, numeric(1L), 1L))
    max_x <- max(vapply(ranges, `[`, numeric(1L), 2L))
    arrowTypeToName(arrowIntType(c(min_x, max_x)))
}

.prepareCoordSchema <-
function(x, indexcols, datacol, arrowtype, max_dim,
         append, along, offset, path, grid, BPPARAM,
         infer_value = TRUE)
{
    index_names <- .computeIndexTypeNames(dim(x), max_dim, append, along, offset)
    names(index_names) <- indexcols
    value_name <- if (is.null(arrowtype)) NULL else arrowTypeToName(arrowtype)
    schema <- list(index = index_names, value = value_name)

    if (append) {
        schema <- .reconcileCoordSchema(path, indexcols, datacol, schema, max_dim)
    } else if (infer_value && is.null(schema$value)) {
        schema$value <- .inferValueTypeName(x, grid, BPPARAM)
    }

    schema
}

.reconcileCoordSchema <-
function(path, indexcols, datacol, schema, max_dim)
{
    columns <- c(indexcols, datacol)
    if (is.null(max_dim)) {
        index_proposed <- setNames(
            rep(list(NULL), length(indexcols)),
            indexcols
        )
    } else {
        index_proposed <- as.list(schema$index)
    }
    arrowtypes <- c(index_proposed, setNames(list(schema$value), datacol))
    resolved <- reconcileParquetSchema(path, columns, arrowtypes)

    index_resolved <- vapply(indexcols, function(nm) {
        resolved[[nm]]$ToString()
    }, character(1L))
    names(index_resolved) <- indexcols
    list(index = index_resolved,
         value = resolved[[datacol]]$ToString())
}

.coordSchemaArrowTypes <- function(schema) {
    list(
        index = lapply(schema$index, arrowTypeFromName),
        value = if (is.null(schema$value)) NULL else arrowTypeFromName(schema$value)
    )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Type conversion (Arrow ↔ DuckDB)
###

#' @importFrom arrow int8 int16 int32 int64 uint8 uint16 uint32 uint64
#' @importFrom arrow float32 float64 utf8 bool
#' @importFrom arrow date32 timestamp time32 duration binary
#' @importFrom arrow infer_type
.duckdbTypeToArrow <- function(duckdb_type) {
    type <- tolower(duckdb_type)

    if (grepl("^(list<.*>|struct[<(].*[>)]|map<.*,.*>)$", type)) {
        return(infer_type(list()))
    }

    if (grepl("^array<.*,\\d+>$", type)) {
        return(infer_type(list()))
    }

    switch(type,
           "boolean" = bool(),
           "tinyint" = int8(),
           "smallint" = int16(),
           "integer" = int32(),
           "bigint" = int64(),
           "hugeint" = int64(),
           "utinyint" = uint8(),
           "usmallint" = uint16(),
           "uinteger" = uint32(),
           "ubigint" = uint64(),
           "uhugeint" = uint64(),
           "float" = float32(),
           "real" = float32(),
           "double" = float64(),
           "decimal" = float64(),
           "varchar" = utf8(),
           "char" = utf8(),
           "bpchar" = utf8(),
           "text" = utf8(),
           "string" = utf8(),
           "date" = date32(),
           "timestamp" = timestamp("us", timezone = "UTC"),
           "time" = time32("s"),
           "interval" = duration("s"),
           "blob" = binary(),
           "bytea" = binary(),
           "geometry" = binary(),
           "geometry_type" = utf8(),
           "integer" = int32(),
           "integer64" = int64(),
           "numeric" = float64(),
           "double" = float64(),
           "character" = utf8(),
           "logical" = bool(),
           float64())
}

#' @importFrom DuckDBDataFrame arrowTypeFromName
.resolveArrowType <- function(type) {
    if (is.null(type)) {
        return(NULL)
    }
    if (is.character(type)) {
        arrowTypeFromName(type)
    } else {
        type
    }
}

.resolveArrowTypeList <- function(types) {
    if (length(types) == 0L) {
        return(types)
    }
    if (is.character(types)) {
        return(lapply(types, arrowTypeFromName))
    }
    types
}

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

.arrowToDuckDBTypeName <- function(arrow_type) {
    format_str <- .arrowTypeToFormat(arrow_type)
    if (is.null(format_str)) {
        return("INTEGER")
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
           "INTEGER")
}
