#' Row-wise Deviance Statistics
#'
#' @description
#' Compute per-row (per-gene) deviance statistics for feature selection
#' in single-cell RNA-seq data. Genes with high deviance are likely to be
#' informative for downstream analysis.
#'
#' @param x A matrix-like object with genes in rows and cells in columns.
#'   Typically a count matrix (not log-transformed).
#' @param family Character string specifying the null model distribution.
#'   Either \code{"binomial"} (default) or \code{"poisson"}.
#' @param ... Additional arguments (currently unused).
#' @param grid For \code{DelayedMatrix} objects, an optional
#'   \code{\linkS4class{ArrayGrid}} object specifying the block structure.
#'   Defaults to \code{\link[DelayedArray]{colAutoGrid}(x)} for column-wise
#'   processing since deviance is additive across cells.
#' @param as.sparse For \code{DelayedMatrix} objects, logical indicating
#'   whether blocks should be returned as sparse matrices. Default is \code{NA},
#'   which lets \code{\link[DelayedArray]{blockApply}} decide based on data
#'   sparsity. Set to \code{TRUE} to force sparse blocks or \code{FALSE} to
#'   force dense blocks.
#' @param BPPARAM For \code{DelayedMatrix} objects, a
#'   \code{\link[BiocParallel]{BiocParallelParam}} object specifying the parallel
#'   backend. Defaults to \code{\link[DelayedArray]{getAutoBPPARAM}()}.
#'
#' @details
#' The deviance statistic measures how poorly each gene fits a simple null
#' model where expression is constant across cells (after accounting for
#' library size). Genes with high deviance show more variation than expected
#' under the null model and are good candidates for highly variable gene
#' selection.
#'
#' For \code{family = "binomial"}, the null model treats each UMI as a
#' Bernoulli trial with gene-specific success probability. This is the
#' closest approximation to multinomial sampling.
#'
#' For \code{family = "poisson"}, the null model assumes Poisson-distributed
#' counts with rate proportional to library size. This is faster to compute
#' and often gives similar results to binomial.
#'
#' @author Patrick Aboyoun
#'
#' @return A numeric vector of deviance statistics, one per row of \code{x}.
#'   Names are preserved from \code{rownames(x)} if present.
#'
#' @references
#' Townes FW, Hicks SC, Aryee MJ, and Irizarry RA (2019). Feature Selection
#' and Dimension Reduction for Single Cell RNA-Seq based on a Multinomial
#' Model. \emph{Genome Biology} \url{https://doi.org/10.1186/s13059-019-1861-6}
#'
#' @seealso
#' \code{\link[MatrixGenerics]{rowVars}} for variance-based feature selection.
#'
#' @examples
#' # Generate example count data
#' set.seed(123)
#' mat <- matrix(rpois(1000, lambda = 5), nrow = 100, ncol = 10)
#' rownames(mat) <- paste0("Gene", 1:100)
#'
#' # Compute deviances
#' dev_binom <- rowDeviances(mat, family = "binomial")
#' dev_pois <- rowDeviances(mat, family = "poisson")
#'
#' # Select top variable genes
#' top_genes <- head(order(dev_binom, decreasing = TRUE), 20)
#'
#' @include DuckDBMatrix-class.R
#'
#' @export
#' @rdname rowDeviances
setGeneric("rowDeviances", function(x, family = c("binomial", "poisson"), ...)
    standardGeneric("rowDeviances"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helper functions for vectorized deviance computation
###

# Poisson deviance block - sparse-aware
# Returns partial ll_sat values (to be summed across blocks)
#' @importFrom Matrix t rowSums sparseMatrix
#' @importClassesFrom Matrix CsparseMatrix dgCMatrix
.poissonDevianceBlock <- function(x, sz) {
    if (is.matrix(x)) {
        # Dense path
        ratio <- sweep(x, 2, sz, "/")
        ratio[x == 0] <- 1  # so log(1) = 0
        rowSums(x * log(ratio))
    } else {
        # Sparse path: modify non-zero values in place, use Matrix::rowSums
        x <- as(x, "dgCMatrix")
        col_idx <- rep(seq_len(ncol(x)), diff(x@p))
        # Compute v * log(v / sz[j]) for each non-zero v
        x@x <- x@x * log(x@x / sz[col_idx])
        Matrix::rowSums(x)
    }
}

# Binomial deviance block - sparse-aware
# Returns partial deviance values (to be summed across blocks)
# For sparse data: compute terms on non-zeros + correction for zeros
#' @importFrom Matrix rowSums t sparseMatrix
#' @importClassesFrom Matrix CsparseMatrix dgCMatrix
.binomDevianceBlock <- function(x, p, libSizes) {
    if (is.matrix(x)) {
        # Dense path: use sweep to avoid large outer products
        nx <- sweep(-x, 2, libSizes, "+")

        # Term 1: x * log(x / (n*p))
        ratio1 <- sweep(x, 2, libSizes, "/")
        ratio1 <- sweep(ratio1, 1, p, "/")
        log_term1 <- log(ratio1)
        log_term1[!is.finite(log_term1)] <- 0
        term1 <- rowSums(x * log_term1)

        # Term 2: (n-x) * log((n-x) / (n*(1-p)))
        ratio2 <- sweep(nx, 2, libSizes, "/")
        ratio2 <- sweep(ratio2, 1, 1 - p, "/")
        log_term2 <- log(ratio2)
        log_term2[!is.finite(log_term2)] <- 0
        term2 <- rowSums(nx * log_term2)

        term1 + term2
    } else {
        # Sparse path: build sparse matrices for each term, use Matrix::rowSums
        x <- as(x, "dgCMatrix")
        nrow_x <- nrow(x)
        ncol_x <- ncol(x)

        # Column index for each non-zero value
        col_idx <- rep(seq_len(ncol_x), diff(x@p))
        row_idx <- x@i + 1L
        vals <- x@x

        # For each non-zero entry
        n_vals <- libSizes[col_idx]
        nx_vals <- n_vals - vals
        p_vals <- p[row_idx]
        np_vals <- n_vals * p_vals
        n1p_vals <- n_vals * (1 - p_vals)

        # Term 1: x * log(x / (n*p)) - create sparse matrix and sum
        log_t1 <- log(vals / np_vals)
        log_t1[!is.finite(log_t1)] <- 0
        contrib1 <- vals * log_t1
        term1_mat <- sparseMatrix(i = row_idx, j = col_idx, x = contrib1,
                                  dims = c(nrow_x, ncol_x))
        term1 <- Matrix::rowSums(term1_mat)

        # Term 2: (n-x) * log((n-x) / (n*(1-p))) - create sparse matrix and sum
        log_t2 <- log(nx_vals / n1p_vals)
        log_t2[!is.finite(log_t2)] <- 0
        contrib2 <- nx_vals * log_t2
        term2_mat <- sparseMatrix(i = row_idx, j = col_idx, x = contrib2,
                                  dims = c(nrow_x, ncol_x))
        term2_sparse <- Matrix::rowSums(term2_mat)

        # Sum of lib_sizes for non-zero cells per gene (for zero correction)
        lib_mat <- sparseMatrix(i = row_idx, j = col_idx, x = n_vals,
                                dims = c(nrow_x, ncol_x))
        lib_nz <- Matrix::rowSums(lib_mat)

        # Correction for zero cells: when x=0, term2 = n * log(1/(1-p))
        lib_total <- sum(libSizes)
        lib_zero <- lib_total - lib_nz
        zero_correction <- ifelse(p < 1, lib_zero * log(1 / (1 - p)), 0)

        term1 + term2_sparse + zero_correction
    }
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for in-memory matrices
###

#' @export
#' @rdname rowDeviances
setMethod("rowDeviances", "matrix", function(x, family = c("binomial", "poisson"), ...) {
    family <- match.arg(family)
    libSizes <- colSums(x)
    geneSums <- rowSums(x)
    p <- geneSums / sum(libSizes)

    if (family == "poisson") {
        log_lib <- log(libSizes)
        sz <- exp(log_lib - mean(log_lib))
        sz_sum <- sum(sz)

        ll_sat <- .poissonDevianceBlock(x, sz)
        ll_null <- geneSums * log(geneSums / sz_sum)
        ll_null[geneSums == 0] <- 0
        dev <- 2 * (ll_sat - ll_null)
    } else {
        dev <- 2 * .binomDevianceBlock(x, p, libSizes)
    }

    dev[is.na(dev)] <- 0
    names(dev) <- rownames(x)
    dev
})


#' @export
#' @rdname rowDeviances
#' @importFrom Matrix colSums rowSums
#' @importClassesFrom Matrix dgCMatrix
setMethod("rowDeviances", "dgCMatrix", function(x, family = c("binomial", "poisson"), ...) {
    family <- match.arg(family)
    libSizes <- Matrix::colSums(x)
    geneSums <- Matrix::rowSums(x)
    p <- geneSums / sum(libSizes)

    if (family == "poisson") {
        log_lib <- log(libSizes)
        sz <- exp(log_lib - mean(log_lib))
        sz_sum <- sum(sz)

        ll_sat <- .poissonDevianceBlock(x, sz)
        ll_null <- geneSums * log(geneSums / sz_sum)
        ll_null[geneSums == 0] <- 0
        dev <- 2 * (ll_sat - ll_null)
    } else {
        dev <- 2 * .binomDevianceBlock(x, p, libSizes)
    }

    dev[is.na(dev)] <- 0
    names(dev) <- rownames(x)
    dev
})

#' @export
#' @rdname rowDeviances
#' @importFrom DelayedArray blockApply colAutoGrid colSums rowSums getAutoBPPARAM currentViewport
#' @importFrom IRanges ranges start end
#' @importFrom S4Vectors isTRUEorFALSE
#' @importClassesFrom DelayedArray DelayedMatrix
setMethod("rowDeviances", "DelayedMatrix",
function(x, family = c("binomial", "poisson"), ...,
         grid = NULL, as.sparse = NA, BPPARAM = getAutoBPPARAM()) {
    family <- match.arg(family)

    # Pre-compute global statistics
    libSizes <- as.numeric(DelayedArray::colSums(x))
    geneSums <- as.numeric(DelayedArray::rowSums(x))
    p <- geneSums / sum(libSizes)

    # Use column-wise grid - deviance is additive across cells
    if (is.null(grid))
        grid <- colAutoGrid(x)

    if (family == "poisson") {
        # Normalize library sizes to geometric mean = 1
        log_lib <- log(libSizes)
        sz <- exp(log_lib - mean(log_lib))
        sz_sum <- sum(sz)

        # Process column blocks with sparse-aware helper
        # as.sparse = NA lets DelayedArray decide based on data sparsity
        gdev <- blockApply(x, function(block) {
            col_range <- ranges(currentViewport())[2L]
            col_idx <- start(col_range):end(col_range)
            .poissonDevianceBlock(block, sz[col_idx])
        }, grid = grid, as.sparse = as.sparse, BPPARAM = BPPARAM)

        # Sum partial contributions across blocks
        ll_sat <- Reduce(`+`, gdev)
        ll_null <- geneSums * log(geneSums / sz_sum)
        ll_null[geneSums == 0] <- 0
        dev <- 2 * (ll_sat - ll_null)
    } else {
        # Binomial with sparse-aware helper
        # Handles zero-cell correction internally
        gdev <- blockApply(x, function(block) {
            col_range <- ranges(currentViewport())[2L]
            col_idx <- start(col_range):end(col_range)
            .binomDevianceBlock(block, p, libSizes[col_idx])
        }, grid = grid, as.sparse = as.sparse, BPPARAM = BPPARAM)

        # Sum partial contributions across blocks
        dev <- 2 * Reduce(`+`, gdev)
    }

    dev[is.na(dev)] <- 0
    names(dev) <- rownames(x)
    dev
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Method for DuckDBMatrix
###

#' @export
#' @rdname rowDeviances
#' @importFrom dplyr collect group_by left_join mutate summarize
#' @importFrom stats setNames
#' @importFrom DuckDBDataFrame tblconn
setMethod("rowDeviances", "DuckDBMatrix",
function(x, family = c("binomial", "poisson"), ...) {
    family <- match.arg(family)

    if (x@seed@fill != 0) {
        stop("rowDeviances requires a zero-filled sparse matrix")
    }

    table <- x@seed@table
    keycols <- table@keycols
    datacol <- table@datacols[[1L]]
    datacol_name <- names(table@datacols)

    row_key <- names(keycols)[1L]
    col_key <- names(keycols)[2L]
    row_keycol <- keycols[[1L]]
    col_keycol <- keycols[[2L]]

    conn <- tblconn(table, select = FALSE)

    # Optimized approach: compute lib_sizes first (one scan), then do main
    # computation with gene_sum computed inline during final aggregation.
    # This reduces from 3 separate scans to 2.

    # Step 1: Compute library sizes (required for both families)
    lib_aggr <- setNames(list(call("sum", datacol, na.rm = TRUE)), "lib_size")
    lib_sizes_query <- conn |>
        group_by(!!as.name(col_key)) |>
        summarize(!!!lib_aggr, .groups = "drop")
    lib_sizes_df <- as.data.frame(collect(lib_sizes_query))
    total_sum <- sum(lib_sizes_df$lib_size)

    if (family == "poisson") {
        # For Poisson deviance:
        # ll_sat = sum(x * log(x / sz)) where sz = exp(log(lib_size) - mean(log(lib_size)))
        # ll_null = gene_sum * log(gene_sum / sz_sum)

        # Normalize library sizes to geometric mean = 1
        log_lib <- log(lib_sizes_df$lib_size)
        mean_log_lib <- mean(log_lib)
        lib_sizes_df$sz <- exp(log_lib - mean_log_lib)
        sz_sum <- sum(lib_sizes_df$sz)

        # Build expression: x * log(x / sz) when x > 0, else 0
        ll_sat_inner <- call("*", datacol, call("log", call("/", datacol, as.name("sz"))))
        ll_sat_term <- call("if", call(">", datacol, 0), ll_sat_inner, 0)

        # Aggregations: ll_sat and gene_sum computed together
        ll_sat_aggr <- setNames(list(call("sum", ll_sat_term, na.rm = TRUE)), "ll_sat")
        gene_sum_aggr <- setNames(list(call("sum", datacol, na.rm = TRUE)), "gene_sum")

        # Single main query: join lib_sizes, compute ll_sat and gene_sum per gene
        result_query <- conn |>
            left_join(lib_sizes_df, by = col_key, copy = TRUE) |>
            group_by(!!as.name(row_key)) |>
            summarize(!!!ll_sat_aggr, !!!gene_sum_aggr, .groups = "drop")

        result_df <- as.data.frame(collect(result_query))

        # Compute ll_null and deviance in R (fast vector ops)
        result_df$ll_null <- ifelse(result_df$gene_sum > 0,
                                    result_df$gene_sum * log(result_df$gene_sum / sz_sum),
                                    0)
        result_df$deviance <- 2 * (result_df$ll_sat - result_df$ll_null)

    } else {
        # Binomial deviance
        # deviance = 2 * sum(x * log(x/(n*p)) + (n-x) * log((n-x)/(n*(1-p))))
        # where n = lib_size, p = gene_sum / total_sum
        #
        # For sparse data, we also need correction for zero cells.

        # p = gene_sum / total_sum (gene_sum computed inline)
        # We'll compute gene_sum as part of the aggregation
        gene_sum_aggr <- setNames(list(call("sum", datacol, na.rm = TRUE)), "gene_sum")

        # For the deviance terms, we need p = gene_sum / total_sum
        # But gene_sum varies per gene, so we compute it inline
        # p_expr uses the per-row gene_sum which we compute as SUM(value) in aggregation
        # However, we need p BEFORE aggregation for the term calculations...
        #
        # Alternative: use gene_sum from a subquery or compute iteratively
        # For efficiency, compute gene_sum first, then join it back

        # Compute gene_sums (can be done in same scan as lib_sizes if using window,
        # but let's keep it simple with separate query for now)
        gene_sum_query <- conn |>
            group_by(!!as.name(row_key)) |>
            summarize(gene_sum = sum(!!datacol, na.rm = TRUE), .groups = "drop")
        gene_sums_df <- as.data.frame(collect(gene_sum_query))

        # p = gene_sum / total_sum
        p_expr <- call("/", as.name("gene_sum"), total_sum)
        np_expr <- call("*", as.name("lib_size"), p_expr)
        n1p_expr <- call("*", as.name("lib_size"), call("-", 1, p_expr))
        nx_expr <- call("-", as.name("lib_size"), datacol)

        # Term 1: x * log(x / np) when x > 0
        term1_inner <- call("*", datacol, call("log", call("/", datacol, np_expr)))
        term1_expr <- call("if", call(">", datacol, 0), term1_inner, 0)

        # Term 2: (n-x) * log((n-x) / n(1-p)) when (n-x) > 0
        term2_inner <- call("*", nx_expr, call("log", call("/", nx_expr, n1p_expr)))
        term2_expr <- call("if", call(">", nx_expr, 0), term2_inner, 0)

        # Sum of both terms
        sum_terms <- call("+", term1_expr, term2_expr)
        dev_aggr <- setNames(list(call("sum", sum_terms, na.rm = TRUE)), "dev_sparse")

        # Sum of lib_sizes for non-zero entries per gene (for zero correction)
        lib_aggr_nz <- setNames(list(call("sum", as.name("lib_size"), na.rm = TRUE)), "lib_sum_nz")

        # Gene sum aggregation using any_value SQL function
        gene_sum_agg <- setNames(list(call("any_value", as.name("gene_sum"))), "gene_sum")

        # Single main query with both joins
        result_query <- conn |>
            left_join(lib_sizes_df, by = col_key, copy = TRUE) |>
            left_join(gene_sums_df, by = row_key, copy = TRUE) |>
            group_by(!!as.name(row_key)) |>
            summarize(!!!dev_aggr, !!!lib_aggr_nz, !!!gene_sum_agg, .groups = "drop")

        result_df <- as.data.frame(collect(result_query))

        # Correction for zero cells in R (simple vector ops)
        p <- result_df$gene_sum / total_sum
        lib_sum_zero <- total_sum - result_df$lib_sum_nz
        zero_correction <- ifelse(p < 1, lib_sum_zero * log(1 / (1 - p)), 0)
        result_df$deviance <- 2 * (result_df$dev_sparse + zero_correction)
    }

    # Align results with original row order
    row_order <- match(row_keycol, result_df[[row_key]])
    dev <- result_df$deviance[row_order]

    # Handle missing genes (all zeros) - they weren't in the sparse data
    dev[is.na(dev)] <- 0

    names(dev) <- names(row_keycol)
    dev
})
