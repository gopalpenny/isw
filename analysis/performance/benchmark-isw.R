# Source from the package root; run_performance_study() writes ignored reports.
pkgload::load_all(".", quiet = TRUE)

.load_performance_baseline <- function(baseline_ref) {
  # Load the original R functions in an isolated environment; do not change checkout.
  baseline_directory <- tempfile("isw-baseline-")
  dir.create(baseline_directory)
  on.exit(unlink(baseline_directory, recursive = TRUE))
  archive <- file.path(baseline_directory, "source.tar")
  status <- system2("git", c("archive", shQuote(baseline_ref), "R"), stdout = archive)
  if (status != 0L) stop("Could not read baseline R sources: ", baseline_ref)
  utils::untar(archive, exdir = baseline_directory)
  baseline <- new.env(parent = asNamespace("isw"))
  for (file in list.files(file.path(baseline_directory, "R"), full.names = TRUE)) {
    sys.source(file, envir = baseline, keep.source = TRUE)
  }
  baseline
}

.make_performance_workload <- function(api, scenario, method, include_preparation,
                                       quadrature_order = NULL) {
  # User-facing fixtures retain physical units and sf CRS; stripped lengths use meters.
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 30, 60), "days"),
    pump_1 = units::set_units(c(400, 400, 0), "m^3/day"),
    pump_2 = units::set_units(c(0, 250, 0), "m^3/day")
  )
  evaluation_times <- units::set_units(
    sort(unique(c(seq(0, 90, by = scenario$evaluation_step_day), 90))), "days"
  )
  observation_wells <- isw::example_observation_wells
  if (scenario$number_of_observations != nrow(observation_wells)) {
    # Interpolate a fixed transect between the two example observations.
    endpoint_coordinates_m <- sf::st_coordinates(observation_wells)
    observation_coordinates_m <- lapply(seq(0, 1, length.out = scenario$number_of_observations),
      function(fraction) sf::st_point(as.numeric(
        (1 - fraction) * endpoint_coordinates_m[1, ] + fraction * endpoint_coordinates_m[2, ]
      )))
    observation_wells <- sf::st_sf(
      observation_id = paste0("observation_", seq_len(scenario$number_of_observations)),
      geometry = sf::st_sfc(observation_coordinates_m, crs = sf::st_crs(observation_wells))
    )
  }
  prepare_inputs <- function() {
    stream_segments <- api$prep_stream_segments(
      isw::example_stream_reaches, units::set_units(scenario$reach_spacing_m, "m")
    )
    stream_apportionment <- if (method == "adf") api$prep_adf_stream_apportionment(
      isw::example_pumping_wells, stream_segments, units::set_units(50, "m")
    ) else NULL
    list(stream_segments = stream_segments, stream_apportionment = stream_apportionment)
  }
  prepared_inputs <- prepare_inputs()
  run_model <- function() {
    model_inputs <- if (include_preparation) prepare_inputs() else prepared_inputs
    model_arguments <- list(
      pumping_wells = isw::example_pumping_wells, pumping_schedules = pumping_schedules,
      observation_wells = observation_wells, stream_segments = model_inputs$stream_segments,
      evaluation_times = evaluation_times,
      injection_method = method, stream_apportionment = model_inputs$stream_apportionment
    )
    if (!is.null(quadrature_order)) model_arguments$quadrature_order <- quadrature_order
    do.call(api$model_aquifer_water_level_change, model_arguments)
  }
  list(run = run_model, number_of_segments = nrow(prepared_inputs$stream_segments),
       number_of_evaluations = length(evaluation_times),
       number_of_observations = nrow(observation_wells))
}

run_performance_study <- function(repetitions = 5L, baseline_ref = "9956c8c",
                                  output_directory = "analysis/profiles/performance-study",
                                  include_profiles = TRUE) {
  # Repetition counts are dimensionless; measured runtime is seconds, outside hydraulic units.
  stopifnot(length(repetitions) == 1L, is.finite(repetitions),
            repetitions >= 1, repetitions == as.integer(repetitions))
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  baseline <- .load_performance_baseline(baseline_ref)
  current <- asNamespace("isw")
  scenarios <- tibble::tribble(
    ~scenario, ~reach_spacing_m, ~evaluation_step_day, ~number_of_observations,
    "example", 400, 30, 2L,
    "finer_grid", 200, 10, 2L,
    "more_observations", 100, 5, 20L
  )
  baseline_commit <- system2("git", c("rev-parse", shQuote(baseline_ref)), stdout = TRUE)
  current_commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  package_changes <- system2("git", c("diff", "HEAD", "--", "R"), stdout = TRUE)
  if (length(package_changes)) {
    writeLines(package_changes, file.path(output_directory, "current-package.patch"))
  }
  writeLines(c(paste("Baseline:", baseline_commit), paste("Current:", current_commit),
               if (length(package_changes)) "Current includes uncommitted R changes; see current-package.patch.",
               paste("Repetitions:", repetitions),
               "Two pumps, two aquifer parameter sets, 90-day horizon; alternating version order.",
               capture.output(sessionInfo())), file.path(output_directory, "session-info.txt"))
  timing_records <- list()
  profile_workloads <- list()
  quiet_expected_warning <- function(expression) withCallingHandlers(expression, warning = function(warning) {
    if (grepl("pump-specific aquifer parameter sets", conditionMessage(warning), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index, ]
    for (method in c("adf", "constant_head")) {
      for (include_preparation in c(TRUE, FALSE)) {
        baseline_workload <- .make_performance_workload(baseline, scenario, method, include_preparation)
        current_workload <- .make_performance_workload(current, scenario, method, include_preparation)
        # Warm both implementations and require agreement before comparing runtime.
        baseline_result <- quiet_expected_warning(baseline_workload$run())
        current_result <- quiet_expected_warning(current_workload$run())
        comparison <- all.equal(baseline_result, current_result, tolerance = 1e-9)
        if (!isTRUE(comparison)) stop(paste(comparison, collapse = "\n"))
        cat(scenario$scenario, method, "include_preparation =", include_preparation,
            ": outputs agree; timing", repetitions, "alternating pairs\n")
        workloads <- list(baseline = baseline_workload, current = current_workload)
        for (repetition in seq_len(repetitions)) {
          version_order <- if (repetition %% 2L == 1L) c("baseline", "current") else c("current", "baseline")
          for (version in version_order) {
            gc()
            # Garbage collection before timing is excluded; collections during the call count.
            elapsed_seconds <- unname(system.time(
              quiet_expected_warning(workloads[[version]]$run()), gcFirst = FALSE
            )[["elapsed"]])
            timing_records[[length(timing_records) + 1L]] <- tibble::tibble(
              scenario = scenario$scenario, method = method, include_preparation = include_preparation,
              version = version, repetition = repetition, elapsed_seconds = elapsed_seconds,
              number_of_pumps = 2L, number_of_segments = current_workload$number_of_segments,
              number_of_evaluations = current_workload$number_of_evaluations,
              number_of_observations = current_workload$number_of_observations
            )
          }
        }
        # Persist completed cases so an interrupted long run still leaves useful results.
        utils::write.csv(do.call(rbind, timing_records), file.path(output_directory, "timings.csv"), row.names = FALSE)
        if (!include_preparation && scenario$scenario != "finer_grid") {
          profile_workloads[[paste(scenario$scenario, method, sep = "-")]] <- current_workload
        }
      }
    }
  }
  timings <- do.call(rbind, timing_records)
  summary <- dplyr::summarise(
    dplyr::group_by(timings, scenario, method, include_preparation, version,
                   number_of_pumps, number_of_segments, number_of_evaluations, number_of_observations),
    median_seconds = median(elapsed_seconds), min_seconds = min(elapsed_seconds),
    max_seconds = max(elapsed_seconds), iqr_seconds = stats::IQR(elapsed_seconds), .groups = "drop"
  )
  utils::write.csv(summary, file.path(output_directory, "timing-summary.csv"), row.names = FALSE)
  if (include_profiles) {
    for (name in names(profile_workloads)) {
      workload <- profile_workloads[[name]]
      method <- if (grepl("constant_head$", name)) "constant_head" else "adf"
      scenario_name <- sub(paste0("-", method, "$"), "", name)
      median_seconds <- summary$median_seconds[summary$scenario == scenario_name &
        summary$method == method & !summary$include_preparation & summary$version == "current"]
      profile_repetitions <- max(3L, min(30L, ceiling(4 / max(median_seconds, 0.001))))
      cat("Profiling", name, "with", profile_repetitions, "repetitions\n")
      profile_path <- file.path(output_directory, paste0(name, ".Rprof"))
      profile <- profvis::profvis({
        for (iteration in seq_len(profile_repetitions)) quiet_expected_warning(workload$run())
      }, interval = 0.01, prof_output = profile_path)
      htmlwidgets::saveWidget(profile, file.path(output_directory, paste0(name, ".html")), selfcontained = TRUE)
      profile_summary <- utils::summaryRprof(profile_path)
      for (aggregation in c("by.self", "by.total")) {
        profile_table <- profile_summary[[aggregation]]
        profile_table$function_name <- rownames(profile_table)
        utils::write.csv(profile_table, file.path(output_directory,
          paste0(name, "-", aggregation, ".csv")), row.names = FALSE)
      }
    }
  }
  invisible(list(timings = timings, summary = summary))
}
