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
  stream_segments <- get_stream_segments(
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

  schedule <- get_stream_injection_schedule(
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
  expect_true(all(as.numeric(schedule$injection_rate[1:2]) < 0))
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
  boundary_response <- get_aquifer_water_level_change(
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

test_that("shared aquifer factorization preserves pump-specific schedules", {
  inputs <- make_constant_head_inputs(two_pumps = TRUE)

  combined_schedule <- get_stream_injection_schedule(
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
    single_schedule <- get_stream_injection_schedule(
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
    isw:::.get_aquifer_groups(inputs$pumping_wells),
    "identical K, D, and V"
  )
})

test_that("multiple aquifer parameter sets warn and more than ten stop", {
  inputs <- make_constant_head_inputs(two_pumps = TRUE)
  inputs$pumping_wells$K[[2]] <- units::set_units(12, "m/day")

  expect_warning(
    groups <- isw:::.get_aquifer_groups(inputs$pumping_wells),
    "2 pump-specific aquifer parameter sets"
  )
  expect_identical(length(unique(groups$aquifer_id)), 2L)

  many_wells <- inputs$pumping_wells[rep(1, 11), , drop = FALSE]
  many_wells$pump_id <- paste0("pump_", seq_len(11))
  many_wells$K <- units::set_units(seq_len(11), "m/day")

  expect_error(
    suppressWarnings(isw:::.get_aquifer_groups(many_wells)),
    "More than 10"
  )
  expect_warning(
    allowed_groups <- isw:::.get_aquifer_groups(
      many_wells,
      allow_many_aquifer_parameter_sets = TRUE
    ),
    "11 pump-specific aquifer parameter sets"
  )
  expect_identical(length(unique(allowed_groups$aquifer_id)), 11L)
})

test_that("ADF remains the default injection method", {
  inputs <- make_constant_head_inputs()
  stream_apportionment <- get_adf_stream_apportionment(
    inputs$pumping_wells,
    inputs$stream_segments,
    units::set_units(25, "m")
  )

  default_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = stream_apportionment
  )
  explicit_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    method = "adf",
    stream_apportionment = stream_apportionment
  )

  expect_equal(default_schedule, explicit_schedule)

  observation_wells <- sf::st_sf(
    observation_id = "obs_1",
    geometry = sf::st_sfc(sf::st_point(c(50, 0)), crs = 32615)
  )
  expect_equal(
    get_aquifer_water_level_change(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      observation_wells,
      inputs$stream_segments,
      inputs$evaluation_times,
      stream_injection_schedule = explicit_schedule
    )$water_level_change,
    get_apportioned_aquifer_drawdown(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      observation_wells,
      stream_apportionment,
      inputs$evaluation_times,
      stream_injection_schedule = explicit_schedule
    )$water_level_change
  )
})

test_that("constant-head schedules retain Date interval boundaries", {
  inputs <- make_constant_head_inputs()
  inputs$pumping_schedules$t <- as.Date("2025-01-01") + c(0, 10)
  evaluation_dates <- as.Date("2025-01-01") + c(10, 20)

  schedule <- get_stream_injection_schedule(
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
