# Run from the package root; benchmark integration orders on the ten-year case.
source("analysis/performance/benchmark-scaling.R")

run_quadrature_study <- function(repetitions = 3L,
                                 output_directory = "analysis/profiles/quadrature-study") {
  # Fixtures retain units/CRS; quadrature orders are counts and runtime is seconds.
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  scenario <- data.frame(axis = "monthly", number_of_reaches = 1L,
    number_of_segments = 100L, number_of_pumps = 2L,
    number_of_observations = 1000L, distinct_aquifers = FALSE)
  evaluation_times <- seq(as.Date("2020-01-01"), by = "month", length.out = 121L)
  pumping_rates <- units::set_units(
    400 + 100 * sin(2 * pi * (seq_along(evaluation_times) - 1L) / 12), "m^3/day"
  )
  writeLines(c(paste("HEAD:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    "Current batched working tree; same ten-year monthly-changing-pumping fixture.",
    "1000 observations, 100 segments, 2 pumps sharing an aquifer, 121 calendar dates.",
    "Warm-up excluded; stream preparation included; order 16 comparison is untimed.",
    capture.output(sessionInfo())), file.path(output_directory, "session-info.txt"))
  writeLines(system2("git", c("diff", "HEAD", "--", "R"), stdout = TRUE),
             file.path(output_directory, "current-package.patch"))
  timings <- differences <- list()
  for (method in c("adf", "constant_head")) {
    lower_order_results <- list()
    for (quadrature_order in c(1L, 4L)) {
      workload <- .make_scaling_workload(scenario, method, evaluation_times,
        evaluation_times, pumping_rates, quadrature_order)
      cat("Warming", method, "order", quadrature_order, "\n")
      flush.console()
      warm_result <- workload$run(TRUE)
      stopifnot(nrow(warm_result) == 242000L,
                all(is.finite(as.numeric(warm_result$water_level_change))))
      lower_order_results[[as.character(quadrature_order)]] <- warm_result
      for (repetition in seq_len(repetitions)) {
        gc()
        elapsed_seconds <- unname(system.time(result <- workload$run(TRUE), gcFirst = FALSE)[["elapsed"]])
        stopifnot(isTRUE(all.equal(result, warm_result, tolerance = 1e-9)))
        timings[[length(timings) + 1L]] <- data.frame(method, quadrature_order, repetition, elapsed_seconds)
        utils::write.csv(do.call(rbind, timings), file.path(output_directory, "timings.csv"), row.names = FALSE)
        cat(method, "order", quadrature_order, "repetition", repetition, ":", elapsed_seconds, "seconds\n")
        flush.console()
      }
    }
    cat("Comparing", method, "against order 16 (untimed)\n")
    flush.console()
    reference <- .make_scaling_workload(scenario, method, evaluation_times,
      evaluation_times, pumping_rates, 16L)$run(TRUE)
    for (quadrature_order in c(1L, 4L)) {
      for (quantity in c("stream_recovery", "water_level_change")) {
        reference_m <- as.numeric(units::set_units(reference[[quantity]], "m"))
        difference_m <- as.numeric(units::set_units(
          lower_order_results[[as.character(quadrature_order)]][[quantity]], "m"
        )) - reference_m
        differences[[length(differences) + 1L]] <- data.frame(method, quadrature_order, quantity,
          max_absolute_difference_m = max(abs(difference_m)),
          rmse_m = sqrt(mean(difference_m^2)),
          max_difference_percent_of_reference_peak = 100 * max(abs(difference_m)) / max(abs(reference_m)))
      }
    }
    utils::write.csv(do.call(rbind, differences), file.path(output_directory, "differences-from-order16.csv"), row.names = FALSE)
  }
  summary <- dplyr::summarise(dplyr::group_by(do.call(rbind, timings), method, quadrature_order),
    median_seconds = median(elapsed_seconds), min_seconds = min(elapsed_seconds),
    max_seconds = max(elapsed_seconds), .groups = "drop")
  utils::write.csv(summary, file.path(output_directory, "timing-summary.csv"), row.names = FALSE)
  invisible(list(summary = summary, differences = do.call(rbind, differences)))
}
