### NOT exported but used internally for DuckDB fast-path writes.
#
# SQL generation and execution utilities for writeCoordArray,DuckDBArray.
# These helpers implement zero-copy writes via DuckDB COPY TO for
# DuckDBArray objects, bypassing R materialization entirely.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL generation utilities
###

### Build COPY TO SQL with optional remapping
#' @importFrom dbplyr sql_render
#' @importFrom DuckDBDataFrame buildParquetCopySQL clusterOrderSQL
#' @importFrom DuckDBDataFrame quoteSQLColumns
.buildCopyToSQL <-
function(tbl, indexcols, datacol, target_path, where_clause = NULL,
         mapping_tables = NULL, grid_group = NULL, partition_by = NULL,
         grid_suffix = "_group", cluster_by = NULL, append = FALSE)
{
    conn <- tbl$src$con
    include_grid_groups <- !is.null(partition_by) && partition_by

    if (!is.null(mapping_tables)) {
        join_clauses <- .buildRemappingJoins(mapping_tables, indexcols, grid_group, conn)
        select_parts <- .buildRemappingSelect(indexcols, datacol, conn,
                                              include_grid_groups = include_grid_groups,
                                              grid_suffix = grid_suffix)
        order_cols <- rev(sprintf("m%d.new_idx", seq_along(indexcols)))
        base_query <- sprintf(
            "SELECT %s FROM (%s) t %s",
            paste(select_parts, collapse = ", "),
            sql_render(tbl),
            paste(join_clauses, collapse = " ")
        )
    } else {
        quoted_cols <- quoteSQLColumns(conn, c(indexcols, datacol))
        order_cols <- quoteSQLColumns(conn, rev(indexcols))
        base_query <- sprintf(
            "SELECT %s FROM (%s)",
            paste(quoted_cols, collapse = ", "),
            sql_render(tbl)
        )
    }

    if (!is.null(where_clause) && nzchar(where_clause)) {
        base_query <- sprintf("%s WHERE %s", base_query, where_clause)
    }

    partition_cols <- if (include_grid_groups) {
        quoteSQLColumns(conn, paste0(indexcols, grid_suffix))
    } else {
        NULL
    }

    if (!is.null(cluster_by)) {
        avail <- c(indexcols, datacol,
                   if (include_grid_groups) paste0(indexcols, grid_suffix))
        base_query <- sprintf("SELECT * FROM (%s) AS _co", base_query)
        order_cols <- clusterOrderSQL(conn, base_query, cluster_by,
                                      available = avail)
    }

    buildParquetCopySQL(
        base_query, target_path,
        order_cols = order_cols,
        partition_by = partition_cols,
        row_group_size = 491520L,
        append = append)
}

### Build remapping JOIN clauses
#' @importFrom DBI dbQuoteIdentifier
.buildRemappingJoins <- function(mapping_tables, indexcols, grid_group, conn) {
    join_clauses <- character(length(indexcols))

    for (j in seq_along(indexcols)) {
        col <- indexcols[j]
        map_alias <- sprintf("m%d", j)
        map_table <- mapping_tables[[col]]
        col_quoted <- as.character(dbQuoteIdentifier(conn, col))

        # Add grid_group filter to JOIN condition if provided
        if (!is.null(grid_group)) {
            join_clauses[j] <- sprintf(
                "INNER JOIN %s %s ON t.%s = %s.old_key AND %s.grid_group = %d",
                map_table, map_alias, col_quoted, map_alias, map_alias, grid_group[j]
            )
        } else {
            join_clauses[j] <- sprintf(
                "INNER JOIN %s %s ON t.%s = %s.old_key",
                map_table, map_alias, col_quoted, map_alias
            )
        }
    }

    join_clauses
}

### Build remapping SELECT clause
#' @importFrom DBI dbQuoteIdentifier
#' @importFrom DuckDBDataFrame quoteSQLColumns
.buildRemappingSelect <- function(indexcols, datacol, conn, include_grid_groups = FALSE, grid_suffix = "_group") {
    quoted_datacol <- quoteSQLColumns(conn, datacol)

    if (include_grid_groups) {
        # Include grid_group columns for PARTITION_BY
        select_parts <- character(length(indexcols) * 2 + 1L)

        for (j in seq_along(indexcols)) {
            col <- indexcols[j]
            map_alias <- sprintf("m%d", j)
            col_quoted <- as.character(dbQuoteIdentifier(conn, col))
            group_col_quoted <- as.character(dbQuoteIdentifier(conn, paste0(col, grid_suffix)))

            select_parts[j] <- sprintf("%s.new_idx AS %s", map_alias, col_quoted)
            select_parts[length(indexcols) + j] <- sprintf("%s.grid_group AS %s", map_alias, group_col_quoted)
        }

        select_parts[length(select_parts)] <- sprintf("t.%s", quoted_datacol)
    } else {
        # Original behavior: just index columns
        select_parts <- character(length(indexcols) + 1L)

        for (j in seq_along(indexcols)) {
            col <- indexcols[j]
            map_alias <- sprintf("m%d", j)
            col_quoted <- as.character(dbQuoteIdentifier(conn, col))
            select_parts[j] <- sprintf("%s.new_idx AS %s", map_alias, col_quoted)
        }

        select_parts[length(indexcols) + 1L] <- sprintf("t.%s", quoted_datacol)
    }

    select_parts
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Single-file write
###

### Single-file DuckDB write
#' @importFrom DBI dbExecute
#' @importFrom DuckDBArray dbconn tblconn
.writeDuckDBArraySingle <-
function(x, path, indexcols, datacol, arrowtype, cluster_by = NULL, ...)
{
    # Extract components from DuckDBArray
    seed <- x@seed
    conn <- dbconn(seed)
    tbl <- tblconn(seed)
    keycols <- seed@table@keycols

    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    target <- file.path(path, "part-0.parquet")

    # Create index mappings for remapping old keys to 1..N
    viewport_coords <- keycols
    mappings_result <- .buildIndexMappings(tbl, indexcols, viewport_coords)
    mappings <- mappings_result[["mapping_tables"]]

    on.exit({
        for (sql in mappings_result$cleanup_sql) {
            try(dbExecute(conn, sql), silent = TRUE)
        }
    })

    sql <- .buildCopyToSQL(tbl, indexcols, datacol, target,
                           where_clause = NULL, mappings,
                           cluster_by = cluster_by)
    dbExecute(conn, sql)

    invisible(NULL)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Temporary table utilities
###

### Build temp table filter (IN clause)
#' @importFrom DBI dbExecute dbQuoteIdentifier
#' @importFrom duckdb duckdb_register
.buildTempTableFilter <- function(conn, col_name, values) {
    col_quoted <- as.character(dbQuoteIdentifier(conn, col_name))

    # Small lists: inline IN clause (no temp table needed)
    if (length(values) <= 100L) {
        vals_str <- paste(values, collapse = ", ")
        return(list(
            sql = sprintf("%s IN (%s)", col_quoted, vals_str),
            temp_name = NULL,
            type = "inline"
        ))
    }

    # Medium lists: temp table with VALUES clause
    if (length(values) <= 10000L) {
        temp_suffix <- basename(tempfile(pattern = ""))
        temp_name <- sprintf("temp_viewport_%s_%s", col_name, temp_suffix)

        # Build VALUES clause: (1), (2), (3), ...
        values_clause <- paste0("(", paste(values, collapse = "), ("), ")")
        dbExecute(conn, sprintf(
            "CREATE TEMP TABLE %s (val) AS SELECT * FROM (VALUES %s) t(val)",
            temp_name, values_clause
        ))

        return(list(
            sql = sprintf("%s IN (SELECT val FROM %s)", col_quoted, temp_name),
            temp_name = temp_name,
            type = "temp_table"
        ))
    }

    # Large lists: register R data frame as virtual table
    temp_suffix <- basename(tempfile(pattern = ""))
    temp_name <- sprintf("temp_viewport_%s_%s", col_name, temp_suffix)
    df <- data.frame(val = values)
    duckdb_register(conn, temp_name, df)

    list(
        sql = sprintf("%s IN (SELECT val FROM %s)", col_quoted, temp_name),
        temp_name = temp_name,
        type = "registered"
    )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Grid group utilities
###

### Compute grid group assignments
#' @importFrom S4Arrays mapToGrid
.computeGridGroup <- function(n_keys, grid, dim_index) {
    if (is.null(grid)) {
        return(rep.int(1L, n_keys))
    }

    ndim <- length(grid@refdim)
    coords <- matrix(1L, nrow = n_keys, ncol = ndim)
    coords[, dim_index] <- seq_len(n_keys)
    groups <- mapToGrid(coords, grid)[["major"]][, dim_index]

    groups
}

### Build index mapping temp tables
#' @importFrom DBI dbAppendTable dbExecute dbGetQuery
#' @importFrom DuckDBDataFrame arrowType
.buildIndexMappings <-
function(tbl, indexcols, keycols, grid = NULL, along = NULL, offset = 0L,
         group_offset = 0L, idxtypes = NULL)
{
    # For each dimension, create temp table: old_key → new_idx → grid_group
    # Returns list(mapping_tables = c(names), cleanup_sql = c(DROP statements))

    # Extract connection from tbl
    conn <- tbl$src$con

    # Per-axis new_idx types resolved up front (max_dim-aware). Using these
    # instead of inferring from the remapped indices keeps the new_idx column
    # consistent with the R write path and, on a > 2^31 (append) offset, avoids
    # the double offset degrading to float64 -> INTEGER and overflowing.
    if (!is.null(idxtypes)) {
        idxtypes <- .resolveArrowTypeList(idxtypes)
    }

    mapping_tables <- character(length(indexcols))
    cleanup_sql <- character(length(indexcols))

    for (j in seq_along(indexcols)) {
        col <- indexcols[j]
        old_keys <- keycols[[col]]
        n_keys <- length(old_keys)
        new_indices <- seq_len(n_keys)
        if (!is.null(along) && j == along && offset > 0L) {
            new_indices <- new_indices + offset
        }

        # Compute which grid partition each index belongs to
        grid_groups <- .computeGridGroup(n_keys, grid, j)
        if (!is.null(along) && j == along && group_offset > 0L) {
            grid_groups <- grid_groups + group_offset
        }

        # Generate unique temp table name
        temp_suffix <- basename(tempfile(pattern = ""))
        temp_name <- sprintf("temp_idxmap_%s_%s", col, temp_suffix)
        mapping_tables[j] <- temp_name

        # Determine optimal integer types using existing helpers. The new_idx
        # type is taken from the pre-resolved per-axis idxtypes when available
        # (max_dim-aware, and correct for a > 2^31 offset that makes the
        # remapped indices a double); otherwise inferred from the data.
        old_key_type <- arrowType(old_keys)
        new_idx_type <- if (!is.null(idxtypes) && length(idxtypes) >= j &&
                            !is.null(idxtypes[[j]])) {
            idxtypes[[j]]
        } else {
            arrowType(new_indices)
        }
        grid_group_type <- arrowType(grid_groups)

        # Convert Arrow types to DuckDB type names
        old_key_duckdb <- .arrowToDuckDBTypeName(old_key_type)
        new_idx_duckdb <- .arrowToDuckDBTypeName(new_idx_type)
        grid_group_duckdb <- .arrowToDuckDBTypeName(grid_group_type)

        # Create table with explicit types including grid_group column
        create_sql <- sprintf(
            "CREATE TEMP TABLE %s (old_key %s, new_idx %s, grid_group %s)",
            temp_name, old_key_duckdb, new_idx_duckdb, grid_group_duckdb
        )

        dbExecute(conn, create_sql)
        df <- data.frame(old_key = old_keys, new_idx = new_indices, grid_group = grid_groups)
        dbAppendTable(conn, temp_name, df)

        cleanup_sql[j] <- sprintf("DROP TABLE IF EXISTS %s", temp_name)
    }

    list(mapping_tables = setNames(mapping_tables, indexcols),
         cleanup_sql = cleanup_sql)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Partitioned write with native PARTITION_BY
###

### Partitioned DuckDB write using PARTITION_BY
#' @importFrom DBI dbExecute
#' @importFrom DuckDBDataFrame dbconn tblconn
.writeDuckDBArrayPartitionedWithPartitionBy <-
function(x, path, indexcols, datacol, grid, grid_suffix, idxtypes, arrowtype,
         along = NULL, offset = 0L, group_offset = 0L, append = FALSE,
         cluster_by = NULL, ...)
{
    # Extract components from DuckDBArray once
    seed <- x@seed
    conn <- dbconn(seed)
    tbl <- tblconn(seed)
    keycols <- seed@table@keycols

    # Create index mappings ONCE for entire dataset with grid_group column
    # The grid_group column is computed using S4Arrays::mapToGrid
    mappings_result <- .buildIndexMappings(tbl, indexcols, keycols, grid = grid,
                                           along = along, offset = offset,
                                           group_offset = group_offset,
                                           idxtypes = idxtypes)
    mappings <- mappings_result[["mapping_tables"]]

    # Setup cleanup handler for mapping tables
    on.exit({
        for (sql in mappings_result$cleanup_sql) {
            try(dbExecute(conn, sql), silent = TRUE)
        }
    }, add = TRUE)

    # Build single COPY TO with PARTITION_BY
    # DuckDB will automatically:
    # 1. Group data by __sample__group__ and __feature__group__ (from JOIN)
    # 2. Create Hive-style directories: __sample__group__=0/__feature__group__=0/
    # 3. Write each partition in parallel using internal worker threads
    # 4. Handle all file creation and management
    sql <- .buildCopyToSQL(
        tbl = tbl,
        indexcols = indexcols,
        datacol = datacol,
        target_path = path,
        where_clause = NULL,
        mapping_tables = mappings,
        grid_group = NULL,
        partition_by = TRUE,
        grid_suffix = grid_suffix,
        cluster_by = cluster_by,
        append = append
    )

    # Execute single COPY TO - DuckDB handles all partitioning internally
    dbExecute(conn, sql)

    invisible(NULL)
}
