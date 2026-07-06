#!/usr/bin/env Rscript
# Best-effort backend comparison for the "Benchmarking DuckDBArray" vignette.
#
# Times the four core DelayedArray-backend operations (colSums, rowVars,
# rowDeviances, rowNnzs) across four backends -- in-memory dgCMatrix, HDF5Array,
# TileDBArray, DuckDBMatrix -- under TWO regimes:
#
#   * serial   : one core per backend. Per-core efficiency; matches the
#                single-threaded methodology of the HDF5Array performance vignette.
#   * parallel : each backend given its BEST-EFFORT configuration on a shared core
#                budget, so the comparison is fair:
#                  - DuckDBMatrix  : DuckDB autotunes; we cap it at the budget
#                                    (SET threads = <cores>).
#                  - HDF5Array     : SnowParam workers over blocks (separate
#                                    processes -> no fork/file-lock issues), with
#                                    HDF5 file locking disabled per worker.
#                  - TileDBArray   : SnowParam workers, each with a FRESH TileDB
#                                    context and the internal thread budget divided
#                                    across workers (avoids oversubscription); file
#                                    locks disabled. Generalizes the cancerdb
#                                    preprocess.R pattern.
#                  - dgCMatrix     : single-threaded baseline (Matrix is not
#                                    parallelized); reported under both regimes.
#
# Writes benchmark_results.rds: a tidy data.frame (Operation, Backend, Regime,
# Seconds) with a per-backend configuration recorded in attr(., "config"). The
# vignette renders it via inst/scripts/make_timings_table.R.
#
# Usage:  Rscript run_vignette_benchmarks.R
# Env:    BENCH_CORES     core budget (default = detectCores() - 1)
#         BENCH_NCELLS    cell subset (default 200000)
#         BENCH_SYNTHETIC if set, use a small synthetic matrix instead of EH1039
#                         (for testing the harness without the 2 GB download)
#         BENCH_REGIMES   comma-separated regimes to run (default "serial,parallel";
#                         e.g. "parallel" to skip the slow single-threaded pass)
#         BENCH_BLOCK_MB  DelayedArray auto block size in MB (default 1024 = 2^30)
#         BENCH_HDF5_WORKERS    SnowParam workers for HDF5Array (default = cores)
#         BENCH_TILEDB_WORKERS  SnowParam workers for TileDBArray (default = min(cores,8));
#                               internal threads are budgeted to cores/workers.
#                               Use probe_workers.R to find each backend's sweet spot.

suppressPackageStartupMessages({
    library(DelayedArray)
    library(HDF5Array)
    library(TileDBArray)
    library(DuckDBArray)
    library(Matrix)
    library(MatrixGenerics)
    library(BiocParallel)
    library(tiledb)
    library(DBI)
})

## ---- configuration -----------------------------------------------------------
# BiocParallel caps SnowParam/MulticoreParam to 4 workers when IS_BIOC_BUILD_MACHINE
# is set (to be gentle on the Bioc build farm). This is an OFFLINE benchmark, not a
# vignette build, so clear it here to honor the requested core budget; without this
# the disk backends would be silently limited to 4 workers while DuckDB uses all cores.
Sys.unsetenv("IS_BIOC_BUILD_MACHINE")

n_cells <- as.integer(Sys.getenv("BENCH_NCELLS", "200000"))
# Cores AVAILABLE to this process. parallel::detectCores() reports the whole
# machine (e.g. 96 on a shared node) even when a scheduler/cgroup grants you far
# fewer -- using that would massively oversubscribe. Prefer the SLURM allocation,
# then `nproc` (honors cgroup/CPU affinity), then detectCores() as a last resort.
available_cores <- function() {
    for (v in c("SLURM_CPUS_PER_TASK", "SLURM_CPUS_ON_NODE")) {
        x <- suppressWarnings(as.integer(Sys.getenv(v, "")))
        if (length(x) == 1L && !is.na(x) && x > 0L) return(x)
    }
    np <- tryCatch(as.integer(system("nproc", intern = TRUE, ignore.stderr = TRUE)),
                   error = function(e) NA_integer_, warning = function(w) NA_integer_)
    if (length(np) == 1L && !is.na(np) && np > 0L) return(np)
    max(1L, parallel::detectCores())
}
cores <- as.integer(Sys.getenv("BENCH_CORES", as.character(max(1L, available_cores() - 1L))))
# Per-backend worker counts (best effort differs by backend: R-level SnowParam is
# process parallelism with serialization + single-file I/O contention, so the
# optimum is usually FEWER workers than the core budget -- especially for HDF5Array,
# which has no internal threading. Use probe_workers.R to find each backend's sweet
# spot, then set these. DuckDB is not listed: it autotunes internal threads up to
# the core budget.
hdf5_workers <- as.integer(Sys.getenv("BENCH_HDF5_WORKERS", as.character(cores)))
tiledb_workers <- as.integer(Sys.getenv("BENCH_TILEDB_WORKERS",
                                        as.character(max(1L, min(cores, 8L)))))
block_bytes <- 2^30    # 1 GiB blocks: large blocks favor these sparse column/row
                       # aggregations (fewer scans); this is the DelayedArray max
                       # the original benchmark used. Overridable via BENCH_BLOCK_MB.
if (nzchar(Sys.getenv("BENCH_BLOCK_MB"))) {
    block_bytes <- as.numeric(Sys.getenv("BENCH_BLOCK_MB")) * 2^20
}
block_MB <- round(block_bytes / 2^20)   # for display / config record
setAutoBlockShape("scale")
cat(sprintf("Config: %d cells | cores = %d | HDF5 workers = %d | TileDB workers = %d | block = %d MB%s\n",
            n_cells, cores, hdf5_workers, tiledb_workers, block_MB,
            if (nzchar(Sys.getenv("BENCH_SYNTHETIC"))) " | SYNTHETIC" else ""))

## ---- data --------------------------------------------------------------------
cat("Preparing backends...\n")
if (nzchar(Sys.getenv("BENCH_SYNTHETIC"))) {
    set.seed(1L)
    ng <- 2000L
    brain_mem <- as(Matrix(rpois(ng * n_cells, 0.2), ng, n_cells, sparse = TRUE),
                    "dgCMatrix")
    rownames(brain_mem) <- paste0("Gene", seq_len(ng))
    colnames(brain_mem) <- paste0("Cell", seq_len(n_cells))
    brain_hdf5 <- writeHDF5Array(brain_mem)
} else {
    library(ExperimentHub)
    hub <- ExperimentHub()
    brain_full <- TENxMatrix(hub[["EH1039"]], group = "mm10")
    brain_hdf5 <- brain_full[, seq_len(n_cells)]
    brain_mem <- as(brain_hdf5, "dgCMatrix")
}
cat(sprintf("  matrix: %d genes x %d cells\n", nrow(brain_mem), ncol(brain_mem)))

# TileDBArray backend
tiledb_path <- tempfile()
brain_tiledb <- writeTileDBArray(brain_mem, path = tiledb_path)

# DuckDBMatrix backend (transpose so features are columns for columnar access)
duckdb_path <- tempfile()
brain_t <- t(brain_mem)
writeCoordArray(brain_t, duckdb_path)
brain_ddb <- DuckDBMatrix(duckdb_path, datacol = "value",
    keycols = list(index2 = setNames(seq_len(ncol(brain_t)), colnames(brain_t)),
                   index1 = setNames(seq_len(nrow(brain_t)), rownames(brain_t))),
    dimtbls = createDimTables(brain_t))

## ---- best-effort parallel configuration --------------------------------------
serial <- SerialParam()

# HDF5Array: SnowParam over blocks, file locking disabled on each worker.
hdf5_snow <- SnowParam(workers = hdf5_workers, progressbar = FALSE)
bpstart(hdf5_snow)
invisible(bplapply(seq_len(bpnworkers(hdf5_snow)), function(i) {
    Sys.setenv(HDF5_USE_FILE_LOCKING = "FALSE")
    suppressPackageStartupMessages(library(HDF5Array))
    if (requireNamespace("rhdf5", quietly = TRUE)) {
        try(rhdf5::h5disableFileLocking(), silent = TRUE)
    }
    NULL
}, BPPARAM = hdf5_snow))

# TileDBArray: SnowParam with a fresh TileDB context per worker and internal
# threads budgeted to cores / workers (generalizes cancerdb/preprocess.R).
n_tile_workers <- max(1L, tiledb_workers)       # TileDB also threads internally
tpw <- max(1L, cores %/% n_tile_workers)        # threads per worker
tile_cfg <- tiledb_config()
tile_cfg["sm.compute_concurrency_level"] <- as.character(tpw)
tile_cfg["sm.io_concurrency_level"] <- as.character(tpw)
tile_cfg["vfs.num_threads"] <- as.character(tpw)
try(tile_cfg["vfs.file.enable_filelocks"] <- "false", silent = TRUE)
tile_cfg_chr <- as.character(tile_cfg)
tiledb_set_context(tiledb_ctx(tile_cfg))        # main-process context too
tiledb_snow <- SnowParam(workers = n_tile_workers, progressbar = FALSE)
bpstart(tiledb_snow)
invisible(bplapply(seq_len(bpnworkers(tiledb_snow)), function(i, cfg) {
    suppressPackageStartupMessages(library(tiledb))
    tiledb::tiledb_set_context(tiledb::tiledb_ctx(cfg))
    NULL
}, cfg = tile_cfg_chr, BPPARAM = tiledb_snow))

# DuckDBMatrix: threads set on the shared connection (autotunes otherwise).
ddb_conn <- DuckDBDataFrame::acquireDuckDBConn()
set_duckdb_threads <- function(n) dbExecute(ddb_conn, sprintf("SET threads = %d;", n))

## ---- timing ------------------------------------------------------------------
elapsed <- function(expr)
    tryCatch(unname(system.time(force(expr))["elapsed"]),
             error = function(e) NA_real_)

# Time one backend on one operation under one regime, applying that backend's
# regime-appropriate parallelism.
time_backend <- function(backend, fun, regime) {
    if (is.null(fun)) return(NA_real_)
    if (backend == "DuckDB") {
        set_duckdb_threads(if (regime == "parallel") cores else 1L)
        return(elapsed(fun()))
    }
    if (backend == "InMemory") return(elapsed(fun()))  # single-threaded either way
    bp <- if (regime == "serial") serial
          else switch(backend, HDF5Array = hdf5_snow, TileDBArray = tiledb_snow, serial)
    old <- getAutoBPPARAM()
    on.exit(setAutoBPPARAM(old))
    setAutoBlockSize(block_bytes)
    setAutoBPPARAM(bp)
    elapsed(fun())
}

# op -> per-backend thunk (NULL = unsupported on that backend)
ops <- list(
    colSums = list(
        InMemory = function() Matrix::colSums(brain_mem),
        HDF5Array = function() colSums(brain_hdf5),
        TileDBArray = function() colSums(brain_tiledb),
        DuckDB = function() colSums(brain_ddb)),
    rowVars = list(
        InMemory = function() rowVars(brain_mem),
        HDF5Array = function() rowVars(brain_hdf5),
        TileDBArray = function() rowVars(brain_tiledb),
        DuckDB = function() rowVars(brain_ddb)),
    rowDeviances = list(
        InMemory = function() rowDeviances(brain_mem, family = "binomial"),
        HDF5Array = function() rowDeviances(brain_hdf5, family = "binomial"),
        TileDBArray = function() rowDeviances(brain_tiledb, family = "binomial"),
        DuckDB = function() rowDeviances(brain_ddb, family = "binomial")),
    rowNnzs = list(
        InMemory = function() rowNnzs(brain_mem),
        HDF5Array = function() rowNnzs(brain_hdf5),
        TileDBArray = NULL,   # TileDBArray lacks rowCounts for its sparse block type
        DuckDB = function() rowNnzs(brain_ddb))
)

backends <- c("InMemory", "HDF5Array", "TileDBArray", "DuckDB")
regimes <- trimws(strsplit(Sys.getenv("BENCH_REGIMES", "serial,parallel"), ",")[[1]])
regimes <- regimes[nzchar(regimes)]
rows <- list()
for (opname in names(ops)) {
    cat(sprintf("\n--- %s ---\n", opname))
    for (bk in backends) {
        fun <- ops[[opname]][[bk]]
        for (rg in regimes) {
            secs <- time_backend(bk, fun, rg)
            rows[[length(rows) + 1L]] <- data.frame(
                Operation = opname, Backend = bk, Regime = rg,
                Seconds = secs, stringsAsFactors = FALSE)
            cat(sprintf("  %-11s %-8s %s\n", bk, rg,
                        if (is.na(secs)) "NA" else sprintf("%.2f s", secs)))
        }
    }
}
results <- do.call(rbind, rows)

## ---- record configuration + save ---------------------------------------------
attr(results, "config") <- list(
    n_cells = n_cells,
    cores = cores,
    block_MB = block_MB,
    InMemory = "single-threaded (Matrix)",
    HDF5Array = sprintf("SnowParam(%d) over blocks; HDF5 file locking disabled; %d MB blocks",
                        hdf5_workers, block_MB),
    TileDBArray = sprintf("SnowParam(%d) x %d threads/worker; file locks off; %d MB blocks",
                          n_tile_workers, tpw, block_MB),
    DuckDB = sprintf("internal threads (SET threads = %d; serial = 1)", cores)
)

bpstop(hdf5_snow)
bpstop(tiledb_snow)

saveRDS(results, "benchmark_results.rds")
cat("\nSaved benchmark_results.rds\n")
print(results, row.names = FALSE)
