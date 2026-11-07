# Compare the pre-line-sink main branch with current point stream injection.
source("analysis/performance/benchmark-isw.R")

.make_pre_line_sink_comparison_apis <- function(baseline_ref) {
  # Adapt historical user-facing names/arguments without changing either engine.
  legacy <- .load_performance_baseline(baseline_ref)
  # The old namespace imported this operator; the current namespace no longer does.
  legacy[["%>%"]] <- magrittr::`%>%`
  current <- asNamespace("isw")
  legacy_api <- list(
    prep_stream_segments = function(stream_reaches, reach_spacing) {
      legacy$get_stream_segments(stream_reaches, reach_spacing)
    },
    prep_adf_stream_apportionment = legacy$get_adf_stream_apportionment,
    model_aquifer_water_level_change = function(..., quadrature_order = 1L) {
      legacy$get_aquifer_water_level_change(..., allow_many_aquifer_parameter_sets = TRUE)
    }
  )
  current_api <- list(
    prep_stream_segments = function(stream_reaches, reach_spacing) {
      stream_segments <- current$prep_stream_segments(stream_reaches, reach_spacing)
      # Legacy diameter = segment length/2; matching width gives the same
      # minimum radius (segment length/4) in the current point response.
      stream_segments$stream_width <- stream_segments$represented_length / 2
      stream_segments
    },
    prep_adf_stream_apportionment = current$prep_adf_stream_apportionment,
    model_aquifer_water_level_change = current$model_aquifer_water_level_change
  )
  list(baseline = legacy_api, current = current_api)
}

run_pre_line_sink_study <- function(repetitions = 5L, baseline_ref = "3d4dcb7",
                                    output_directory = "analysis/profiles/pre-line-sink-study") {
  # Hydraulic fixtures carry units/CRS; repetition counts are dimensionless.
  stopifnot(length(repetitions) == 1L, is.finite(repetitions),
            repetitions >= 1L, repetitions == as.integer(repetitions))
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  apis <- .make_pre_line_sink_comparison_apis(baseline_ref)
  scenarios <- tibble::tribble(
    ~scenario, ~reach_spacing_m, ~evaluation_step_day, ~number_of_observations,
    "example", 400, 30, 2L,
    "finer_grid", 200, 10, 2L,
    "more_observations", 100, 5, 20L
  )
  writeLines(c(paste("Baseline:", system2("git", c("rev-parse", baseline_ref), stdout = TRUE)),
    paste("Current:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    "Last pre-line-sink main versus current order 1; matched source minimum radius.",
    "Legacy segment diameter and current segment width both equal represented_length/2.",
    "90-day bundled fixtures, two pumps/aquifers; both preparation modes included.",
    "Historical function names/allow-many option adapted; old engine remains unchanged.",
    "Expected multi-aquifer warnings muffled for both implementations.",
    paste("Alternating timing pairs per case:", repetitions),
    capture.output(sessionInfo())), file.path(output_directory, "session-info.txt"))
  quiet_expected_warning <- function(expression) withCallingHandlers(expression, warning = function(warning) {
    if (grepl("aquifer parameter sets", conditionMessage(warning), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
  timings <- differences <- list()
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index, ]
    for (method in c("adf", "constant_head")) {
      for (include_preparation in c(FALSE, TRUE)) {
        workloads <- lapply(apis, function(api) {
          .make_performance_workload(api, scenario, method, include_preparation, quadrature_order = 1L)
        })
        cat("Warming", scenario$scenario, method, "include_preparation =", include_preparation, "\n")
        flush.console()
        warm_results <- lapply(workloads, function(workload) quiet_expected_warning(workload$run()))
        identifiers <- c("pump_id", "observation_id", "evaluation_time")
        stopifnot(identical(warm_results$baseline[identifiers], warm_results$current[identifiers]),
          nrow(warm_results$current) == 2L * scenario$number_of_observations *
            workloads$current$number_of_evaluations)
        complete_outputs_agree <- isTRUE(all.equal(warm_results$baseline,
          warm_results$current, tolerance = 1e-9))
        for (quantity in c("pumping_drawdown", "stream_recovery", "water_level_change")) {
          baseline_m <- as.numeric(units::set_units(warm_results$baseline[[quantity]], "m"))
          current_m <- as.numeric(units::set_units(warm_results$current[[quantity]], "m"))
          stopifnot(all(is.finite(baseline_m)), all(is.finite(current_m)))
          difference_m <- current_m - baseline_m
          differences[[length(differences) + 1L]] <- data.frame(scenario = scenario$scenario,
            method, include_preparation, quantity, max_absolute_difference_m = max(abs(difference_m)),
            rmse_m = sqrt(mean(difference_m^2)), complete_outputs_agree)
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
        cat("Completed", scenario$scenario, method, "include_preparation =", include_preparation,
          "; complete outputs agree:", complete_outputs_agree, "\n")
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
