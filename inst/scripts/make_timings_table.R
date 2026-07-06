## make_timings_table.R
##
## Helpers used by the "Benchmarking DuckDBArray" vignette to render PRECOMPUTED
## benchmark timings, so the vignette does not run the (multi-GB, multi-minute)
## benchmark at build time. Mirrors the HDF5Array performance vignette's approach.
##
## The timings are produced offline by run_vignette_benchmarks.R and saved as
## inst/scripts/benchmark_results.rds. To regenerate (see that script's header):
##
##   Rscript inst/scripts/run_vignette_benchmarks.R
##   # then copy benchmark_results.rds into inst/scripts/ and commit it
##
## benchmark_results.rds is a tidy data.frame with columns
##   Operation, Backend (InMemory/HDF5Array/TileDBArray/DuckDB), Regime
##   (serial/parallel), Seconds
## and attr(., "config"): a list describing the per-backend configuration
## (n_cells, cores, block_MB, and one string per backend).

## Load the shipped timings, or NULL if absent (so the vignette degrades cleanly).
load_vignette_timings <- function() {
    path <- system.file("scripts", "benchmark_results.rds", package = "DuckDBArray")
    if (!nzchar(path) || !file.exists(path)) {
        return(NULL)
    }
    readRDS(path)
}

.BACKENDS <- c("InMemory", "HDF5Array", "TileDBArray", "DuckDB")

## Pivot one regime to wide form (one row per operation), adding a DuckDB-vs-
## HDF5Array speedup column.
timings_wide <- function(results, regime = c("parallel", "serial")) {
    regime <- match.arg(regime)
    df <- results[results$Regime == regime, , drop = FALSE]
    ops <- unique(df$Operation)
    out <- data.frame(Operation = ops, stringsAsFactors = FALSE)
    for (b in .BACKENDS) {
        out[[b]] <- vapply(ops, function(o) {
            v <- df$Seconds[df$Operation == o & df$Backend == b]
            if (length(v) == 0L) NA_real_ else v[[1L]]
        }, numeric(1))
    }
    out[["DuckDB_vs_HDF5"]] <- round(out[["HDF5Array"]] / out[["DuckDB"]], 1)
    out
}

## Render one regime's timings as a knitr::kable, or a short note if the
## precomputed results are absent from this build.
make_timings_table <- function(regime = c("parallel", "serial"), caption = NULL,
                               results = load_vignette_timings()) {
    regime <- match.arg(regime)
    if (is.null(results)) {
        cat("_Precomputed benchmark timings are not bundled in this build. ",
            "Generate them with `inst/scripts/run_vignette_benchmarks.R` ",
            "(see that script's header)._\n", sep = "")
        return(invisible(NULL))
    }
    knitr::kable(timings_wide(results, regime), digits = 2, caption = caption,
                 col.names = c("Operation", "In-memory", "HDF5Array", "TileDBArray",
                               "DuckDB", "DuckDB vs HDF5Array (x)"))
}

## Emit the per-backend configuration as a Markdown paragraph (for reproducibility
## / a fair-comparison footnote).
timings_config_note <- function(results = load_vignette_timings()) {
    cfg <- if (is.null(results)) NULL else attr(results, "config")
    if (is.null(cfg)) {
        return(invisible(NULL))
    }
    cat(sprintf(paste0(
        "Configuration (%d cells, %d-core budget, %.0f MB blocks). ",
        "**In-memory:** %s. **HDF5Array:** %s. **TileDBArray:** %s. **DuckDB:** %s.\n"),
        cfg$n_cells, cfg$cores, cfg$block_MB,
        cfg$InMemory, cfg$HDF5Array, cfg$TileDBArray, cfg$DuckDB))
}
