# Source from the package root; this supplements the unprofiled timing study.
pkgload::load_all(".", quiet = TRUE)

inspect_performance_overhead <- function(output_directory = "analysis/profiles/performance-study") {
  # Hydraulic fixtures use explicit units/CRS; allocation sizes below are bytes/MiB.
  source("analysis/performance/benchmark-isw.R", local = TRUE)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  scenario <- tibble::tibble(reach_spacing_m = 100, evaluation_step_day = 5,
                             number_of_observations = 20L)
  inspect_workload <- function(method) {
    workload <- .make_performance_workload(asNamespace("isw"), scenario, method, FALSE)
    suppressWarnings(workload$run())
    gc()
    memory_path <- file.path(output_directory, paste0("more_observations-", method, "-allocations.Rprofmem"))
    Rprofmem(memory_path)
    result <- tryCatch(suppressWarnings(workload$run()), finally = Rprofmem(NULL))
    allocation_lines <- readLines(memory_path)
    sized_allocations <- grepl("^[0-9]+ ", allocation_lines)
    allocated_bytes <- as.numeric(sub(" .*", "", allocation_lines[sized_allocations]))
    allocation_stacks <- allocation_lines[sized_allocations]
    # Rprofmem also reports small-vector heap pages without byte sizes. These
    # totals cover sized allocations, not all allocator traffic or peak memory.
    memory_summary <- tibble::tibble(
      method = method, recorded_allocations = length(allocated_bytes),
      allocated_mib = sum(allocated_bytes) / 1024^2,
      largest_allocation_mib = max(allocated_bytes) / 1024^2,
      cache_lookup_allocated_mib = sum(allocated_bytes[
        grepl(".get_cached_line_response_matrix", allocation_stacks, fixed = TRUE)
      ]) / 1024^2,
      format_small_vector_pages = sum(grepl("^new page:", allocation_lines) &
        grepl('"format.default"', allocation_lines, fixed = TRUE)),
      result_mib = as.numeric(object.size(result)) / 1024^2
    )
    function_names <- c(".get_cached_line_response_matrix", ".evaluate_line_response_operator",
                        ".get_response_ratios_for_injection_events",
                        ".calculate_stream_recovery_at_observations", ".theis_aquifer_drawdown_ratio",
                        ".theis_response_at_distance")
    counts <- stats::setNames(integer(length(function_names)), function_names)
    bindings <- stats::setNames(lapply(function_names, function(name) {
      original <- get(name, asNamespace("isw"))
      force(original)
      force(name)
      function(...) {
        counts[[name]] <<- counts[[name]] + 1L
        original(...)
      }
    }), function_names)
    # Count in a separate untimed call so instrumentation cannot distort the
    # allocation recording or timing study. Bindings are restored on function exit.
    do.call(testthat::local_mocked_bindings,
            c(bindings, list(.package = "isw", .env = environment())))
    instrumented_result <- suppressWarnings(workload$run())
    stopifnot(isTRUE(all.equal(result, instrumented_result, tolerance = 1e-12)))
    list(memory = memory_summary,
         calls = tibble::tibble(method = method, function_name = names(counts), calls = unname(counts)))
  }
  inspections <- lapply(c("adf", "constant_head"), inspect_workload)
  memory <- do.call(rbind, lapply(inspections, `[[`, "memory"))
  calls <- do.call(rbind, lapply(inspections, `[[`, "calls"))
  utils::write.csv(memory, file.path(output_directory, "allocation-summary.csv"), row.names = FALSE)
  utils::write.csv(calls, file.path(output_directory, "call-counts.csv"), row.names = FALSE)
  invisible(list(memory = memory, calls = calls))
}
