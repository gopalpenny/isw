# Run from the package root; use real calendar months rather than 30-day steps.
source("analysis/performance/benchmark-scaling.R")

run_monthly_study <- function(repetitions = 3L,
                              output_directory = "analysis/profiles/monthly-study") {
  # Fixtures carry units/CRS; elapsed benchmark time is in seconds.
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  scenario <- data.frame(axis = "monthly", number_of_reaches = 1L,
    number_of_segments = 100L, number_of_pumps = 2L,
    number_of_observations = 1000L, distinct_aquifers = FALSE)
  evaluation_times <- seq(as.Date("2020-01-01"), by = "month", length.out = 121L)
  records <- list()
  writeLines(c(paste("HEAD:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    "Current batched working tree; two pumps sharing K/D/V, 1,000 observations, 100 segments.",
    "Real calendar monthly dates over ten years: 120 intervals, 121 evaluation nodes.",
    "Timings include stream preparation; warm-up excluded; no concurrent benchmarks.",
    capture.output(sessionInfo())), file.path(output_directory, "session-info.txt"))
  writeLines(system2("git", c("diff", "HEAD", "--", "R"), stdout = TRUE),
             file.path(output_directory, "current-package.patch"))
  for (schedule in c("three_changes", "monthly_changes")) {
    pumping_times <- if (schedule == "three_changes") evaluation_times[1:3] else evaluation_times
    pumping_rates <- units::set_units(if (schedule == "three_changes") c(400, 250, 0) else
      400 + 100 * sin(2 * pi * (seq_along(pumping_times) - 1L) / 12), "m^3/day")
    for (method in c("adf", "constant_head")) {
      workload <- .make_scaling_workload(scenario, method, evaluation_times,
                                         pumping_times, pumping_rates)
      cat("Warming", method, schedule, "\n")
      flush.console()
      warm_result <- workload$run(TRUE)
      stopifnot(nrow(warm_result) == 242000L,
                all(is.finite(as.numeric(warm_result$water_level_change))))
      for (repetition in seq_len(repetitions)) {
        gc()
        elapsed_seconds <- unname(system.time(result <- workload$run(TRUE), gcFirst = FALSE)[["elapsed"]])
        stopifnot(isTRUE(all.equal(result, warm_result, tolerance = 1e-9)))
        records[[length(records) + 1L]] <- data.frame(schedule, method, repetition, elapsed_seconds)
        utils::write.csv(do.call(rbind, records), file.path(output_directory, "timings.csv"), row.names = FALSE)
        cat(method, schedule, "repetition", repetition, ":", elapsed_seconds, "seconds\n")
        flush.console()
      }
    }
  }
  summary <- dplyr::summarise(dplyr::group_by(do.call(rbind, records), schedule, method),
    median_seconds = median(elapsed_seconds), min_seconds = min(elapsed_seconds),
    max_seconds = max(elapsed_seconds), .groups = "drop")
  utils::write.csv(summary, file.path(output_directory, "timing-summary.csv"), row.names = FALSE)
  invisible(summary)
}
