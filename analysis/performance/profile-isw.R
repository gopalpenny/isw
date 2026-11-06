# Run from the package root. See analysis/performance/PROFILING.md for usage.
for (package in c("profvis", "pkgload")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install the profiling tools with install.packages(c('profvis', 'pkgload'))")
  }
}

# Retain source references so profvis can show lines inside R/ functions.
pkgload::load_all(".", quiet = TRUE)

profile_isw <- function(method = c("adf", "constant_head"),
                        repetitions = 3L,
                        reach_spacing_m = 400,
                        evaluation_step_days = 30,
                        include_preparation = TRUE) {
  method <- match.arg(method)
  stopifnot(
    length(repetitions) == 1L, is.finite(repetitions),
    repetitions >= 1, repetitions == as.integer(repetitions),
    length(reach_spacing_m) == 1L, is.finite(reach_spacing_m),
    reach_spacing_m > 0,
    length(evaluation_step_days) == 1L, is.finite(evaluation_step_days),
    evaluation_step_days > 0,
    is.logical(include_preparation), length(include_preparation) == 1L,
    !is.na(include_preparation)
  )

  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 30, 60), "days"),
    pump_1 = units::set_units(c(400, 400, 0), "m^3/day"),
    pump_2 = units::set_units(c(0, 250, 0), "m^3/day")
  )
  evaluation_times <- units::set_units(
    sort(unique(c(seq(0, 90, by = evaluation_step_days), 90))), "days"
  )

  prepare_inputs <- function() {
    segments <- isw::prep_stream_segments(
      isw::example_stream_reaches,
      reach_spacing = units::set_units(reach_spacing_m, "m")
    )
    apportionment <- if (method == "adf") {
      isw::prep_adf_stream_apportionment(
        isw::example_pumping_wells, segments,
        sample_spacing = units::set_units(50, "m")
      )
    } else {
      NULL
    }
    list(segments = segments, apportionment = apportionment)
  }

  run_model <- function(inputs) {
    isw::model_aquifer_water_level_change(
      pumping_wells = isw::example_pumping_wells,
      pumping_schedules = pumping_schedules,
      observation_wells = isw::example_observation_wells,
      stream_segments = inputs$segments,
      evaluation_times = evaluation_times,
      injection_method = method,
      stream_apportionment = inputs$apportionment
    )
  }

  # Warm up code and lazy-loaded data before recording steady-state work.
  inputs <- prepare_inputs()
  invisible(run_model(inputs))
  gc()

  profvis::profvis({
    for (iteration in seq_len(repetitions)) {
      if (include_preparation) inputs <- prepare_inputs()
      result <- run_model(inputs)
    }
  }, interval = 0.01)
}
