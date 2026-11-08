# Compare the same calendar-monthly workload without changing the checkout.
source("analysis/performance/benchmark-isw.R")
source("analysis/performance/benchmark-scaling.R")

run_point_injection_study <- function(repetitions = 5L, baseline_ref = "c2ed2a4",
                                      output_directory = "analysis/profiles/point-injection-study") {
  # Fixtures retain units/CRS; order and repetition counts are dimensionless.
  stopifnot(length(repetitions) == 1L, is.finite(repetitions),
            repetitions >= 1L, repetitions == as.integer(repetitions))
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  baseline <- .load_performance_baseline(baseline_ref)
  current <- asNamespace("isw")
  scenario <- data.frame(axis = "monthly", number_of_reaches = 1L,
    number_of_segments = 100L, number_of_pumps = 2L,
    number_of_observations = 1000L, distinct_aquifers = FALSE)
  evaluation_times <- seq(as.Date("2020-01-01"), by = "month", length.out = 121L)
  pumping_rates <- units::set_units(
    400 + 100 * sin(2 * pi * (seq_along(evaluation_times) - 1L) / 12), "m^3/day"
  )
  writeLines(c(paste("Baseline:", system2("git", c("rev-parse", baseline_ref), stdout = TRUE)),
    paste("Current:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    "Baseline uses quadrature_order = 1; current uses 0; monthly-changing-pumping ten-year fixture.",
    "1000 observations, 100 straight segments, 2 pumps sharing K/D/V, 121 calendar dates.",
    "Stream preparation included; warm-up excluded; alternating unprofiled version order.",
    paste("Repetitions per version per method:", repetitions),
    capture.output(sessionInfo())), file.path(output_directory, "session-info.txt"))
  package_patch <- system2("git", c("diff", "HEAD", "--", "R"), stdout = TRUE)
  if (length(package_patch)) writeLines(package_patch, file.path(output_directory, "current-package.patch"))
  timings <- differences <- list()
  for (method in c("adf", "constant_head")) {
    workloads <- Map(function(api, order) {
      .make_scaling_workload(scenario, method, evaluation_times,
        evaluation_times, pumping_rates, quadrature_order = order, api = api)
    }, list(baseline = baseline, current = current), c(baseline = 1L, current = 0L))
    cat("Warming and comparing", method, "\n")
    flush.console()
    baseline_result <- workloads$baseline$run(TRUE)
    current_result <- workloads$current$run(TRUE)
    identifiers <- c("pump_id", "observation_id", "evaluation_time")
    stopifnot(identical(baseline_result[identifiers], current_result[identifiers]),
      nrow(current_result) == 242000L,
      all(is.finite(as.numeric(current_result$water_level_change))),
      all(is.finite(as.numeric(baseline_result$water_level_change))))
    comparison <- all.equal(baseline_result, current_result, tolerance = 1e-9)
    for (quantity in c("pumping_drawdown", "stream_recovery", "water_level_change")) {
      baseline_m <- as.numeric(units::set_units(baseline_result[[quantity]], "m"))
      difference_m <- as.numeric(units::set_units(current_result[[quantity]], "m")) - baseline_m
      differences[[length(differences) + 1L]] <- data.frame(method, quantity,
        max_absolute_difference_m = max(abs(difference_m)), rmse_m = sqrt(mean(difference_m^2)),
        complete_outputs_agree = isTRUE(comparison))
    }
    utils::write.csv(do.call(rbind, differences), file.path(output_directory, "output-comparison.csv"), row.names = FALSE)
    if (!isTRUE(comparison)) stop(paste(comparison, collapse = "\n"))
    for (repetition in seq_len(repetitions)) {
      version_order <- if (repetition %% 2L == 1L) c("baseline", "current") else c("current", "baseline")
      for (version in version_order) {
        gc()
        elapsed_seconds <- unname(system.time(
          result <- workloads[[version]]$run(TRUE), gcFirst = FALSE
        )[["elapsed"]])
        stopifnot(isTRUE(all.equal(result, baseline_result, tolerance = 1e-9)))
        timings[[length(timings) + 1L]] <- data.frame(method, version, repetition, elapsed_seconds)
        utils::write.csv(do.call(rbind, timings), file.path(output_directory, "timings.csv"), row.names = FALSE)
        cat(method, version, "repetition", repetition, ":", elapsed_seconds, "seconds\n")
        flush.console()
      }
    }
  }
  summary <- dplyr::summarise(dplyr::group_by(do.call(rbind, timings), method, version),
    median_seconds = median(elapsed_seconds), min_seconds = min(elapsed_seconds),
    max_seconds = max(elapsed_seconds), iqr_seconds = stats::IQR(elapsed_seconds), .groups = "drop")
  utils::write.csv(summary, file.path(output_directory, "timing-summary.csv"), row.names = FALSE)
  invisible(list(summary = summary, differences = do.call(rbind, differences)))
}

run_small_point_injection_study <- function(repetitions = 15L, baseline_ref = "c2ed2a4",
                                            output_directory = "analysis/profiles/small-point-injection-study") {
  # Counts are dimensionless; fixtures retain physical units and sf CRS.
  stopifnot(length(repetitions) == 1L, is.finite(repetitions),
            repetitions >= 1L, repetitions == as.integer(repetitions))
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  baseline <- .load_performance_baseline(baseline_ref)
  current <- asNamespace("isw")
  scenarios <- tibble::tribble(
    ~scenario, ~reach_spacing_m, ~evaluation_step_day, ~number_of_observations,
    "example", 400, 30, 2L,
    "finer_grid", 200, 10, 2L
  )
  writeLines(c(paste("Baseline:", system2("git", c("rev-parse", baseline_ref), stdout = TRUE)),
    paste("Current:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    "Baseline uses quadrature_order = 1; current uses 0; bundled example geometry and two pumps/aquifers.",
    "7 segments/4 evaluations and 14 segments/10 evaluations; two observations, 90 days.",
    "Both stream-preparation modes; alternating versions; warmed unprofiled timings.",
    "Expected pump-specific aquifer parameter warnings muffled for both versions.",
    paste("Repetitions per version/method/preparation/scenario:", repetitions),
    capture.output(sessionInfo())), file.path(output_directory, "session-info.txt"))
  quiet_expected_warning <- function(expression) withCallingHandlers(expression, warning = function(warning) {
    if (grepl("pump-specific aquifer parameter sets", conditionMessage(warning), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
  timings <- differences <- list()
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index, ]
    for (method in c("adf", "constant_head")) {
      for (include_preparation in c(FALSE, TRUE)) {
        workloads <- Map(function(api, order) {
          .make_performance_workload(api, scenario, method, include_preparation, quadrature_order = order)
        }, list(baseline = baseline, current = current), c(baseline = 1L, current = 0L))
        warm_results <- lapply(workloads, function(workload) quiet_expected_warning(workload$run()))
        identifiers <- c("pump_id", "observation_id", "evaluation_time")
        stopifnot(identical(warm_results$baseline[identifiers], warm_results$current[identifiers]),
          nrow(warm_results$current) == 2L * scenario$number_of_observations *
            workloads$current$number_of_evaluations)
        for (quantity in c("pumping_drawdown", "stream_recovery", "water_level_change")) {
          baseline_m <- as.numeric(units::set_units(warm_results$baseline[[quantity]], "m"))
          current_m <- as.numeric(units::set_units(warm_results$current[[quantity]], "m"))
          stopifnot(all(is.finite(baseline_m)), all(is.finite(current_m)))
          difference_m <- current_m - baseline_m
          differences[[length(differences) + 1L]] <- data.frame(scenario = scenario$scenario,
            method, include_preparation, quantity, max_absolute_difference_m = max(abs(difference_m)),
            rmse_m = sqrt(mean(difference_m^2)), complete_outputs_agree = isTRUE(all.equal(
              warm_results$baseline, warm_results$current, tolerance = 1e-9)))
        }
        for (repetition in seq_len(repetitions)) {
          version_order <- if (repetition %% 2L == 1L) c("baseline", "current") else c("current", "baseline")
          for (version in version_order) {
            gc()
            elapsed_seconds <- unname(system.time(
              result <- quiet_expected_warning(workloads[[version]]$run()), gcFirst = FALSE
            )[["elapsed"]])
            stopifnot(isTRUE(all.equal(result, warm_results[[version]], tolerance = 1e-9)))
            timings[[length(timings) + 1L]] <- data.frame(scenario = scenario$scenario,
              method, include_preparation, version, repetition, elapsed_seconds,
              number_of_segments = workloads$current$number_of_segments,
              number_of_evaluations = workloads$current$number_of_evaluations,
              number_of_observations = scenario$number_of_observations)
          }
        }
        utils::write.csv(do.call(rbind, timings), file.path(output_directory, "timings.csv"), row.names = FALSE)
        utils::write.csv(do.call(rbind, differences), file.path(output_directory, "output-comparison.csv"), row.names = FALSE)
        cat("Completed", scenario$scenario, method, "include_preparation =", include_preparation, "\n")
        flush.console()
      }
    }
  }
  summary <- dplyr::summarise(dplyr::group_by(do.call(rbind, timings), scenario, method,
    include_preparation, version, number_of_segments, number_of_evaluations, number_of_observations),
    median_seconds = median(elapsed_seconds), min_seconds = min(elapsed_seconds),
    max_seconds = max(elapsed_seconds), iqr_seconds = stats::IQR(elapsed_seconds), .groups = "drop")
  utils::write.csv(summary, file.path(output_directory, "timing-summary.csv"), row.names = FALSE)
  invisible(list(summary = summary, differences = do.call(rbind, differences)))
}
