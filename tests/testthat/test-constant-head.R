make_constant_head_inputs <- function(two_pumps = FALSE) {
  pump_table <- if (two_pumps) {
    tibble::tibble(
      pump_id = c("pump_1", "pump_2"),
      x = c(0, 20),
      y = c(0, 20),
      K = units::set_units(c(10, 10), "m/day"),
      D = units::set_units(c(20, 20), "m"),
      V = c(0.15, 0.15)
    )
  } else {
    tibble::tibble(
      pump_id = "pump_1",
      x = 0,
      y = 0,
      K = units::set_units(10, "m/day"),
      D = units::set_units(20, "m"),
      V = 0.15
    )
  }
  pumping_wells <- sf::st_as_sf(
    pump_table,
    coords = c("x", "y"),
    crs = 32615
  )
  stream_reaches <- sf::st_sf(
    reach_id = "stream_1",
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(c(100, -50, 100, 50), ncol = 2, byrow = TRUE)
      ),
      crs = 32615
    )
  )
  stream_segments <- prep_stream_segments(
    stream_reaches,
    units::set_units(50, "m")
  )
  pumping_schedules <- if (two_pumps) {
    tibble::tibble(
      t = units::set_units(c(0, 10), "days"),
      pump_1 = units::set_units(c(100, 0), "m^3/day"),
      pump_2 = units::set_units(c(60, 0), "m^3/day")
    )
  } else {
    tibble::tibble(
      t = units::set_units(c(0, 10), "days"),
      pump_1 = units::set_units(c(100, 0), "m^3/day")
    )
  }

  list(
    pumping_wells = pumping_wells,
    pumping_schedules = pumping_schedules,
    stream_segments = stream_segments,
    evaluation_times = units::set_units(c(10, 20), "days")
  )
}

test_that("constant-head schedule enforces endpoint boundary conditions", {
  inputs <- make_constant_head_inputs()

  schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    method = "constant_head"
  )

  expect_named(
    schedule,
    c(
      "pump_id", "aquifer_id", "reach_id", "reach_segment_id",
      "interval_start", "interval_end", "injection_rate",
      "boundary_residual", "matrix_condition_number"
    )
  )
  expect_true(all(as.numeric(schedule$injection_rate[1:2]) > 0))
  expect_lt(
    max(abs(as.numeric(units::set_units(
      schedule$boundary_residual,
      "m",
      mode = "standard"
    )))),
    1e-10
  )
  expect_true(all(is.finite(schedule$matrix_condition_number)))

  stream_observations <- sf::st_sf(
    observation_id = paste0(
      "stream_point_",
      seq_len(nrow(inputs$stream_segments))
    ),
    geometry = inputs$stream_segments$model_point
  )
  boundary_response <- model_aquifer_water_level_change(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    stream_observations,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_injection_schedule = schedule
  )

  expect_lt(
    max(abs(as.numeric(units::set_units(
      boundary_response$water_level_change,
      "m",
      mode = "standard"
    )))),
    1e-10
  )
})

test_that("constant-head schedule metadata preserves quadrature order", {
  inputs <- make_constant_head_inputs()
  schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    method = "constant_head",
    quadrature_order = 8L
  )
  metadata <- attr(schedule, "isw_schedule_metadata", exact = TRUE)

  expect_identical(metadata$injection_method, "constant_head")
  expect_identical(metadata$quadrature_order, 8L)

  observation_wells <- sf::st_sf(
    observation_id = "obs_1",
    geometry = sf::st_sfc(sf::st_point(c(50, 0)), crs = 32615)
  )
  reused_order <- model_aquifer_water_level_change(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    observation_wells,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_injection_schedule = schedule
  )
  explicit_order <- model_aquifer_water_level_change(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    observation_wells,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_injection_schedule = schedule,
    quadrature_order = 8L
  )

  expect_equal(reused_order, explicit_order)
  expect_warning(
    model_aquifer_water_level_change(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      observation_wells,
      inputs$stream_segments,
      inputs$evaluation_times,
      stream_injection_schedule = schedule,
      quadrature_order = 16L
    ),
    "differs from the value used to construct"
  )
})

test_that("shared aquifer factorization preserves pump-specific schedules", {
  inputs <- make_constant_head_inputs(two_pumps = TRUE)

  combined_schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    method = "constant_head"
  )

  expect_identical(unique(combined_schedule$aquifer_id), "aquifer_1")

  for (pump_id in inputs$pumping_wells$pump_id) {
    pump_row <- inputs$pumping_wells$pump_id == pump_id
    single_well <- inputs$pumping_wells[pump_row, , drop = FALSE]
    single_schedule_input <- inputs$pumping_schedules[c("t", pump_id)]
    single_schedule <- generate_stream_injection_schedule(
      single_well,
      single_schedule_input,
      inputs$stream_segments,
      inputs$evaluation_times,
      method = "constant_head"
    )
    combined_pump_schedule <- combined_schedule[
      combined_schedule$pump_id == pump_id,
      ,
      drop = FALSE
    ]

    expect_equal(
      combined_pump_schedule$injection_rate,
      single_schedule$injection_rate
    )
  }
})

test_that("explicit aquifer identifiers must match hydraulic properties", {
  inputs <- make_constant_head_inputs(two_pumps = TRUE)
  inputs$pumping_wells$aquifer_id <- c("shared", "shared")
  inputs$pumping_wells$K[[2]] <- units::set_units(12, "m/day")

  expect_error(
    isw:::.prepare_aquifer_groups(isw:::.prepare_numeric_pumping_wells(inputs$pumping_wells)),
    "identical K, D, and V"
  )
})

test_that("the aquifer parameter-set limit is configurable", {
  inputs <- make_constant_head_inputs(two_pumps = TRUE)
  inputs$pumping_wells$K[[2]] <- units::set_units(12, "m/day")

  expect_warning(
    groups <- isw:::.prepare_aquifer_groups(isw:::.prepare_numeric_pumping_wells(inputs$pumping_wells)),
    "2 pump-specific aquifer parameter sets"
  )
  expect_identical(length(unique(groups$aquifer_id)), 2L)

  many_wells <- inputs$pumping_wells[rep(1, 6), , drop = FALSE]
  many_wells$pump_id <- paste0("pump_", seq_len(6))
  many_wells$K <- units::set_units(seq_len(6), "m/day")

  expect_error(
    suppressWarnings(isw:::.prepare_aquifer_groups(isw:::.prepare_numeric_pumping_wells(many_wells))),
    "allow_n_aquifer_parameter_sets = 5"
  )
  expect_warning(
    allowed_groups <- isw:::.prepare_aquifer_groups(
      isw:::.prepare_numeric_pumping_wells(many_wells),
      allow_n_aquifer_parameter_sets = 6
    ),
    "6 pump-specific aquifer parameter sets"
  )
  expect_identical(length(unique(allowed_groups$aquifer_id)), 6L)

  expect_warning(
    unlimited_groups <- isw:::.prepare_aquifer_groups(
      isw:::.prepare_numeric_pumping_wells(many_wells),
      allow_n_aquifer_parameter_sets = Inf
    ),
    "6 pump-specific aquifer parameter sets"
  )
  expect_identical(length(unique(unlimited_groups$aquifer_id)), 6L)

  expect_identical(
    formals(generate_stream_injection_schedule)$allow_n_aquifer_parameter_sets,
    5
  )
  expect_identical(
    formals(model_aquifer_water_level_change)$allow_n_aquifer_parameter_sets,
    5
  )
})

test_that("the aquifer parameter-set limit is validated", {
  inputs <- make_constant_head_inputs()

  invalid_limits <- list(0, -1, 1.5, NA_real_, NaN, c(1, 2), "5")

  for (invalid_limit in invalid_limits) {
    expect_error(
      isw:::.prepare_aquifer_groups(
        isw:::.prepare_numeric_pumping_wells(inputs$pumping_wells),
        allow_n_aquifer_parameter_sets = invalid_limit
      ),
      "positive whole-number scalar or Inf"
    )
  }
})

test_that("ADF remains the default injection method", {
  inputs <- make_constant_head_inputs()
  stream_apportionment <- prep_adf_stream_apportionment(
    inputs$pumping_wells,
    inputs$stream_segments,
    units::set_units(25, "m")
  )

  default_schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = stream_apportionment
  )
  explicit_schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    method = "adf",
    stream_apportionment = stream_apportionment
  )

  expect_equal(default_schedule, explicit_schedule)

  mismatched_apportionment <- stream_apportionment
  mismatched_apportionment$reach_segment_id[[1]] <- "different_segment"
  expect_error(
    generate_stream_injection_schedule(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$stream_segments,
      inputs$evaluation_times,
      method = "adf",
      stream_apportionment = mismatched_apportionment
    ),
    "must match stream_segments"
  )

  direct_depletion <- model_adf_stream_depletion(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    stream_apportionment,
    units::set_units(c(0, 10, 20, 25), "days")
  )
  reused_schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    method = "adf",
    stream_apportionment = stream_apportionment,
    adf_stream_depletion = direct_depletion
  )

  expect_equal(reused_schedule, explicit_schedule)

  expect_warning(
    completed_schedule <- generate_stream_injection_schedule(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$stream_segments,
      inputs$evaluation_times,
      method = "adf",
      stream_apportionment = stream_apportionment,
      adf_stream_depletion = dplyr::filter(
        direct_depletion,
        evaluation_time != units::set_units(20, "days")
      )
    ),
    "missing ADF stream depletion rates at 1 required injection time"
  )
  expect_equal(completed_schedule, explicit_schedule)

  nonmatching_pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 7, 14), "days"),
    pump_1 = units::set_units(c(100, 100, 0), "m^3/day")
  )
  nonmatching_evaluation_times <- units::set_units(c(0, 10, 20), "days")
  evaluation_only_depletion <- model_adf_stream_depletion(
    inputs$pumping_wells,
    nonmatching_pumping_schedules,
    stream_apportionment,
    nonmatching_evaluation_times
  )
  complete_nonmatching_schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    nonmatching_pumping_schedules,
    inputs$stream_segments,
    nonmatching_evaluation_times,
    method = "adf",
    stream_apportionment = stream_apportionment
  )
  expect_warning(
    completed_nonmatching_schedule <- generate_stream_injection_schedule(
      inputs$pumping_wells,
      nonmatching_pumping_schedules,
      inputs$stream_segments,
      nonmatching_evaluation_times,
      method = "adf",
      stream_apportionment = stream_apportionment,
      adf_stream_depletion = evaluation_only_depletion
    ),
    "missing ADF stream depletion rates at 2 required injection times.*7 \\[d\\].*14 \\[d\\]"
  )
  expect_equal(
    completed_nonmatching_schedule,
    complete_nonmatching_schedule
  )

  expect_error(
    generate_stream_injection_schedule(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$stream_segments,
      inputs$evaluation_times,
      method = "constant_head",
      adf_stream_depletion = direct_depletion
    ),
    "only be supplied"
  )

  observation_wells <- sf::st_sf(
    observation_id = "obs_1",
    geometry = sf::st_sfc(sf::st_point(c(50, 0)), crs = 32615)
  )
  expect_equal(
    model_aquifer_water_level_change(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      observation_wells,
      inputs$stream_segments,
      inputs$evaluation_times,
      stream_injection_schedule = explicit_schedule
    )$water_level_change,
    model_aquifer_water_level_change(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      observation_wells,
      inputs$stream_segments,
      inputs$evaluation_times,
      stream_injection_schedule = explicit_schedule,
      stream_apportionment = stream_apportionment
    )$water_level_change
  )
})

test_that("injection schedules report stream-to-aquifer flow as positive", {
  inputs <- make_constant_head_inputs()

  schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    method = "constant_head"
  )

  expect_true("injection_rate" %in% names(schedule))
  expect_true(all(as.numeric(schedule$injection_rate[1:2]) > 0))
})

test_that("constant-head schedules retain Date interval start and end times", {
  inputs <- make_constant_head_inputs()
  inputs$pumping_schedules$t <- as.Date("2025-01-01") + c(0, 10)
  evaluation_dates <- as.Date("2025-01-01") + c(10, 20)

  schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    evaluation_dates,
    method = "constant_head"
  )

  expect_s3_class(schedule$interval_start, "Date")
  expect_s3_class(schedule$interval_end, "Date")
  expect_identical(
    sort(unique(schedule$interval_end)),
    evaluation_dates
  )
})

test_that("constant-head history cancels pumping at every interval endpoint", {
  inputs <- make_constant_head_inputs(two_pumps = TRUE)
  inputs$pumping_schedules$t <- units::set_units(c(0, 7), "days")
  evaluation_time_day <- c(3, 10, 20)
  schedule <- generate_stream_injection_schedule(
    inputs$pumping_wells, inputs$pumping_schedules, inputs$stream_segments,
    units::set_units(evaluation_time_day, "days"), method = "constant_head"
  )
  segments <- isw:::.prepare_numeric_segments(inputs$stream_segments)
  wells <- isw:::.prepare_numeric_pumping_wells(inputs$pumping_wells, sf::st_crs(segments))
  times <- isw:::.prepare_numeric_model_times(
    inputs$pumping_schedules, units::set_units(evaluation_time_day, "days")
  )
  prepared_schedule <- isw:::.prepare_numeric_schedule(
    inputs$pumping_schedules, inputs$pumping_wells, times
  )
  events <- attr(prepared_schedule, "isw_pumping_events")
  operator <- isw:::.prepare_line_response_operator(
    segments$model_point, isw:::.prepare_line_elements(
      segments, attr(schedule, "isw_schedule_metadata", exact = TRUE)$quadrature_order
    )
  )
  distances_m <- units::drop_units(sf::st_distance(wells, segments$model_point))
  for (pump_row in seq_len(nrow(wells))) {
    pump <- wells[pump_row, ]
    pump_events <- events[events$pump_id == pump$pump_id, ]
    injection <- schedule[schedule$pump_id == pump$pump_id, ]
    for (endpoint_day in sort(unique(as.numeric(schedule$interval_end)))) {
      head_m <- isw:::.get_pumping_head_at_stream(
        pump, pump_events, distances_m[pump_row, ], endpoint_day
      )
      for (start_day in unique(as.numeric(injection$interval_start))) {
        rows <- which(as.numeric(injection$interval_start) == start_day)
        end_day <- as.numeric(injection$interval_end[rows[1]])
        response <- function(age_day) {
          isw:::.evaluate_line_response_operator(
            operator, pump$K_m_per_day, pump$D_m, pump$V, age_day
          )
        }
        if (endpoint_day > start_day) {
          pulse_day_per_m2 <- response(endpoint_day - start_day)
          if (endpoint_day > end_day) {
            pulse_day_per_m2 <- pulse_day_per_m2 - response(endpoint_day - end_day)
          }
          head_m <- head_m - as.numeric(pulse_day_per_m2 %*%
            as.numeric(injection$injection_rate[rows]))
        }
      }
      expect_equal(head_m, rep(0, nrow(segments)), tolerance = 1e-10)
    }
  }
})

test_that("single-segment constant-head schedules preserve units and pump dimensions", {
  inputs <- make_constant_head_inputs(two_pumps = TRUE)
  inputs$stream_segments <- inputs$stream_segments[1, ]
  baseline <- generate_stream_injection_schedule(
    inputs$pumping_wells, inputs$pumping_schedules, inputs$stream_segments,
    inputs$evaluation_times, method = "constant_head"
  )
  inputs$pumping_schedules$t <- units::set_units(inputs$pumping_schedules$t, "hours")
  for (id in inputs$pumping_wells$pump_id) {
    inputs$pumping_schedules[[id]] <- units::set_units(
      inputs$pumping_schedules[[id]], "ft^3/day"
    )
  }
  converted <- generate_stream_injection_schedule(
    inputs$pumping_wells, inputs$pumping_schedules, inputs$stream_segments,
    units::set_units(inputs$evaluation_times, "hours"), method = "constant_head"
  )
  expect_equal(nrow(converted), 4L)
  expect_equal(units::set_units(converted$injection_rate, "m^3/day"),
               baseline$injection_rate)
  expect_equal(converted$boundary_residual, baseline$boundary_residual)
  expect_equal(converted$matrix_condition_number, baseline$matrix_condition_number)
  expect_identical(units::deparse_unit(converted$interval_end), "h")
  expect_identical(units::deparse_unit(converted$injection_rate),
                   units::deparse_unit(inputs$pumping_schedules$pump_1))

  inputs$pumping_wells$K[2] <- 2 * inputs$pumping_wells$K[1]
  expect_warning(separate <- generate_stream_injection_schedule(
    inputs$pumping_wells, inputs$pumping_schedules, inputs$stream_segments,
    units::set_units(inputs$evaluation_times, "hours"), method = "constant_head"
  ), "2 pump-specific aquifer")
  expect_length(unique(separate$aquifer_id), 2L)
  expect_lt(max(abs(as.numeric(separate$boundary_residual))), 1e-10)
})

test_that("constant-head schedules handle zero pumping and empty time horizons", {
  inputs <- make_constant_head_inputs()
  inputs$pumping_schedules$pump_1[] <- units::set_units(0, "m^3/day")
  run_schedule <- function(evaluation_times) generate_stream_injection_schedule(
    inputs$pumping_wells, inputs$pumping_schedules, inputs$stream_segments,
    evaluation_times, method = "constant_head"
  )
  schedule <- run_schedule(inputs$evaluation_times)
  expect_equal(as.numeric(schedule$injection_rate), rep(0, nrow(schedule)))
  expect_equal(as.numeric(schedule$boundary_residual), rep(0, nrow(schedule)))
  empty <- run_schedule(units::set_units(0, "days"))
  expect_equal(nrow(empty), 0L)
  expect_named(empty, names(schedule))
  expect_identical(units::deparse_unit(empty$boundary_residual), "m")
  expect_identical(units::deparse_unit(empty$injection_rate),
                   units::deparse_unit(schedule$injection_rate))
})

test_that("constant-head numerical diagnostics survive numeric preparation", {
  inputs <- make_constant_head_inputs()
  run_schedule <- function() generate_stream_injection_schedule(
    inputs$pumping_wells, inputs$pumping_schedules, inputs$stream_segments,
    inputs$evaluation_times, method = "constant_head"
  )
  response_day_per_m2 <- diag(c(1, 1e-8))
  testthat::local_mocked_bindings(
    .get_cached_stream_response_matrix = function(...) response_day_per_m2,
    .package = "isw"
  )
  expect_warning(schedule <- run_schedule(), "condition number exceeded 1e6")
  expect_true(all(schedule$matrix_condition_number > 1e6))
  response_day_per_m2 <- matrix(0, 2, 2)
  expect_error(run_schedule(), "produced nonfinite rates.*aquifer_1")
})
