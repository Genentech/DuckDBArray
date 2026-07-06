#!/usr/bin/env Rscript
# Worker-count sensitivity probe for the disk-backed DelayedArray backends.
#
# R-level (SnowParam) parallelism is process parallelism: results serialize across
# processes and multiple workers contend on a single on-disk file, so throughput
# often PEAKS WELL BELOW the core budget -- especially for HDF5Array, which has no
# internal threading. This probe sweeps SnowParam worker counts for HDF5Array and
# TileDBArray on one operation (rowVars, a clear scaler both support) so we can set
# each backend to its own best-effort worker count instead of a blanket maximum.
#
# Feed the reported sweet spots into run_vignette_benchmarks.R via
# BENCH_HDF5_WORKERS / BENCH_TILEDB_WORKERS.
#
# Usage:  Rscript probe_workers.R
# Env:    BENCH_CORES, BENCH_NCELLS, BENCH_SYNTHETIC, BENCH_BLOCK_MB
#             (same meaning as in run_vignette_benchmarks.R)
#         BENCH_PROBE_WORKERS  comma-separated worker counts (default "1,2,4,8,16")

suppressPackageStartupMessages({
    library(DelayedArray)
    library(HDF5Array)
    library(TileDBArray)
    library(DuckDBArray)
    library(Matrix)
    library(MatrixGenerics)
    library(BiocParallel)
    library(tiledb)
})
Sys.unsetenv("IS_BIOC_BUILD_MACHINE")   # honor our worker counts (not the 4-worker cap)

n_cells <- as.integer(Sys.getenv("BENCH_NCELLS", "200000"))
# Cores available to this process (SLURM allocation / cgroup), not the whole node;
# see run_vignette_benchmarks.R.
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
block_bytes <- if (nzchar(Sys.getenv("BENCH_BLOCK_MB")))
    as.numeric(Sys.getenv("BENCH_BLOCK_MB")) * 2^20 else 2^30
sweep <- as.integer(trimws(strsplit(Sys.getenv("BENCH_PROBE_WORKERS", "1,2,4,8,16"), ",")[[1]]))
sweep <- sweep[!is.na(sweep) & sweep > 0L]
setAutoBlockSize(block_bytes)
setAutoBlockShape("scale")
cat(sprintf("Probe: %d cells | cores = %d | block = %d MB | workers: %s%s\n",
            n_cells, cores, round(block_bytes / 2^20), paste(sweep, collapse = ", "),
            if (nzchar(Sys.getenv("BENCH_SYNTHETIC"))) " | SYNTHETIC" else ""))

## ---- data --------------------------------------------------------------------
if (nzchar(Sys.getenv("BENCH_SYNTHETIC"))) {
    set.seed(1L)
    ng <- 2000L
    brain_mem <- as(Matrix(rpois(ng * n_cells, 0.2), ng, n_cells, sparse = TRUE), "dgCMatrix")
    rownames(brain_mem) <- paste0("Gene", seq_len(ng))
    colnames(brain_mem) <- paste0("Cell", seq_len(n_cells))
    brain_hdf5 <- writeHDF5Array(brain_mem)
} else {
    library(ExperimentHub)
    hub <- ExperimentHub()
    brain_hdf5 <- TENxMatrix(hub[["EH1039"]], group = "mm10")[, seq_len(n_cells)]
    brain_mem <- as(brain_hdf5, "dgCMatrix")
}
tiledb_path <- tempfile()
brain_tiledb <- writeTileDBArray(brain_mem, path = tiledb_path)

op <- function(x) rowVars(x)   # probe operation (edit here to probe a different op)

## ---- per-worker-count timings ------------------------------------------------
time_hdf5 <- function(nw) {
    bp <- SnowParam(workers = nw, progressbar = FALSE)
    bpstart(bp)
    on.exit({ setAutoBPPARAM(SerialParam()); bpstop(bp) })
    invisible(bplapply(seq_len(bpnworkers(bp)), function(i) {
        Sys.setenv(HDF5_USE_FILE_LOCKING = "FALSE")
        suppressPackageStartupMessages(library(HDF5Array))
        if (requireNamespace("rhdf5", quietly = TRUE)) try(rhdf5::h5disableFileLocking(), silent = TRUE)
        NULL
    }, BPPARAM = bp))
    setAutoBPPARAM(bp)
    tryCatch(unname(system.time(op(brain_hdf5))["elapsed"]), error = function(e) NA_real_)
}

time_tiledb <- function(nw) {
    tpw <- max(1L, cores %/% nw)                 # budget internal threads across workers
    cfg <- tiledb_config()
    cfg["sm.compute_concurrency_level"] <- as.character(tpw)
    cfg["sm.io_concurrency_level"] <- as.character(tpw)
    cfg["vfs.num_threads"] <- as.character(tpw)
    try(cfg["vfs.file.enable_filelocks"] <- "false", silent = TRUE)
    cfgc <- as.character(cfg)
    tiledb_set_context(tiledb_ctx(cfg))
    bp <- SnowParam(workers = nw, progressbar = FALSE)
    bpstart(bp)
    on.exit({ setAutoBPPARAM(SerialParam()); bpstop(bp) })
    invisible(bplapply(seq_len(bpnworkers(bp)), function(i, c) {
        suppressPackageStartupMessages(library(tiledb))
        tiledb::tiledb_set_context(tiledb::tiledb_ctx(c))
        NULL
    }, c = cfgc, BPPARAM = bp))
    setAutoBPPARAM(bp)
    tryCatch(unname(system.time(op(brain_tiledb))["elapsed"]), error = function(e) NA_real_)
}

res <- data.frame(
    workers = sweep,
    HDF5Array = vapply(sweep, function(nw) {
        cat(sprintf("  HDF5   workers=%-2d ...", nw)); t <- time_hdf5(nw)
        cat(sprintf(" %.2f s\n", t)); t }, numeric(1)),
    TileDBArray = vapply(sweep, function(nw) {
        cat(sprintf("  TileDB workers=%-2d ...", nw)); t <- time_tiledb(nw)
        cat(sprintf(" %.2f s\n", t)); t }, numeric(1))
)

cat("\n=== rowVars worker-count sweep (seconds; lower is better) ===\n")
print(res, row.names = FALSE)
best_h <- res$workers[which.min(res$HDF5Array)]
best_t <- res$workers[which.min(res$TileDBArray)]
cat(sprintf("\nSweet spot -> HDF5Array: %d workers (%.2f s) | TileDBArray: %d workers (%.2f s)\n",
            best_h, min(res$HDF5Array, na.rm = TRUE),
            best_t, min(res$TileDBArray, na.rm = TRUE)))
cat(sprintf(paste0("\nThen run the full benchmark with:\n",
                   "  BENCH_HDF5_WORKERS=%d BENCH_TILEDB_WORKERS=%d BENCH_CORES=%d \\\n",
                   "    Rscript inst/scripts/run_vignette_benchmarks.R\n"),
            best_h, best_t, cores))
