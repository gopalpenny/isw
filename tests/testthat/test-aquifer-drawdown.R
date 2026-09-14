make_drawdown_test_inputs <- function() {
  pumping_wells <- sf::st_as_sf(
    tibble::tibble(
      pump_id = "pump_1",
      x = 0,
      y = 0,
      K = units::set_units(10, "m/day"),
      D = units::set_units(20, "m"),
      V = 0.15
    ),
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

  observation_wells <- sf::st_as_sf(
    tibble::tibble(
      observation_id = "obs_1",
      x = 50,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 32615
  )

  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 10), "days"),
    pump_1 = units::set_units(c(100, 0), "m^3/day")
  )
  evaluation_times <- units::set_units(c(0, 10, 20), "days")
  stream_segments <- prep_stream_segments(
    stream_reaches,
    reach_spacing = units::set_units(100, "m"),
    analysis_crs = 32615
  )
  stream_apportionment <- prep_adf_stream_apportionment(
    pumping_wells,
    stream_segments,
    sample_spacing = units::set_units(25, "m")
  )
  stream_depletion <- model_adf_stream_depletion(
    pumping_wells,
    pumping_schedules,
    stream_apportionment,
    evaluation_times
  )

  list(
    pumping_wells = pumping_wells,
    pumping_schedules = pumping_schedules,
    observation_wells = observation_wells,
    stream_segments = stream_segments,
    stream_apportionment = stream_apportionment,
    stream_depletion = stream_depletion,
    evaluation_times = evaluation_times
  )
}

make_injection_schedule <- function(...) {
  generate_stream_injection_schedule(...)
}

test_that("straight-stream drawdown is the real well minus its image", {
  x1 <- units::set_units(100, "m")
  x2 <- units::set_units(40, "m")
  y_diff <- units::set_units(30, "m")
  K <- units::set_units(10, "m/day")
  D <- units::set_units(20, "m")
  V <- 0.15
  elapsed_time <- units::set_units(10, "days")
  real_distance <- sqrt((x2 - x1)^2 + y_diff^2)
  image_distance <- sqrt((x2 + x1)^2 + y_diff^2)

  expected <- calc_infinite_aquifer_drawdown_ratio(
    distance = real_distance,
    K = K,
    D = D,
    V = V,
    t = elapsed_time
  ) - calc_infinite_aquifer_drawdown_ratio(
    distance = image_distance,
    K = K,
    D = D,
    V = V,
    t = elapsed_time
  )

  expect_equal(
    calc_straight_stream_drawdown_ratio(
      x1 = x1,
      x2 = x2,
      y_diff = y_diff,
      K = K,
      D = D,
      V = V,
      t = elapsed_time
    ),
    expected
  )
})

test_that("interval injection rates average depletion endpoints", {
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 10), "days"),
    pump_1 = units::set_units(c(100, 0), "m^3/day")
  )
  stream_depletion <- tibble::tibble(
    pump_id = rep("pump_1", 3),
    evaluation_time = units::set_units(c(0, 10, 20), "days"),
    reach_id = rep("stream_1", 3),
    reach_segment_id = rep("stream_1_segment_1", 3),
    stream_depletion_rate = units::set_units(c(0, 20, 40), "m^3/day")
  )

  events <- isw:::.get_interval_average_injection_rate_changes(
    stream_depletion,
    pumping_schedules
  )

  expect_equal(
    events$injection_time,
    units::set_units(c(0, 10), "days")
  )
  expect_equal(
    events$injection_rate_change,
    units::set_units(c(-10, -20), "m^3/day")
  )
})

test_that("zero depletion is assumed at an unevaluated initial boundary", {
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 10), "days"),
    pump_1 = units::set_units(c(100, 0), "m^3/day")
  )
  stream_depletion <- tibble::tibble(
    pump_id = rep("pump_1", 2),
    evaluation_time = units::set_units(c(10, 20), "days"),
    reach_id = rep("stream_1", 2),
    reach_segment_id = rep("stream_1_segment_1", 2),
    stream_depletion_rate = units::set_units(c(20, 40), "m^3/day")
  )

  events <- isw:::.get_interval_average_injection_rate_changes(
    stream_depletion,
    pumping_schedules
  )

  expect_equal(
    events$injection_rate_change,
    units::set_units(c(-10, -20), "m^3/day")
  )
})

test_that("water-level model superimposes pumping and stream injection", {
  inputs <- make_drawdown_test_inputs()
  result <- model_aquifer_water_level_change(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$observation_wells,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = inputs$stream_apportionment
  )
  injection_schedule <- make_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = inputs$stream_apportionment
  )
  injection_events <- isw:::.get_injection_rate_changes(injection_schedule)

  pumping_ratio_10 <- isw:::.theis_aquifer_drawdown_ratio(
    distance = units::set_units(50, "m"),
    K = inputs$pumping_wells$K,
    D = inputs$pumping_wells$D,
    V = inputs$pumping_wells$V,
    t = units::set_units(10, "days"),
    well_diam = units::set_units(0, "m")
  )
  expected_pumping_10 <- units::set_units(
    -inputs$pumping_schedules$pump_1[[1]] * pumping_ratio_10,
    "m"
  )
  line_ratio_10 <- isw:::.line_sink_aquifer_drawdown_ratio(
    along_distance = units::set_units(0, "m"),
    perpendicular_distance = units::set_units(50, "m"),
    line_length = inputs$stream_segments$represented_length[[1]],
    K = inputs$pumping_wells$K,
    D = inputs$pumping_wells$D,
    V = inputs$pumping_wells$V,
    t = units::set_units(10, "days"),
    stream_width = inputs$stream_segments$stream_width[[1]]
  )
  expected_recovery_10 <- units::set_units(
    injection_events$injection_rate_change[[1]] * line_ratio_10,
    "m"
  )

  expect_named(
    result,
    c(
      "pump_id",
      "observation_id",
      "evaluation_time",
      "pumping_drawdown",
      "stream_recovery",
      "water_level_change"
    )
  )
  expect_equal(nrow(result), 3)
  expect_equal(result$pumping_drawdown[[1]], units::set_units(0, "m"))
  expect_equal(result$stream_recovery[[1]], units::set_units(0, "m"))
  expect_equal(result$pumping_drawdown[[2]], expected_pumping_10)
  expect_equal(result$stream_recovery[[2]], expected_recovery_10)
  expect_equal(
    result$water_level_change,
    -result$pumping_drawdown + result$stream_recovery
  )
})

test_that("stream injection uses finite-line geometry and stream width", {
  inputs <- make_drawdown_test_inputs()
  stream_point <- sf::st_as_sf(
    tibble::tibble(
      observation_id = "on_stream",
      x = 100,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 32615
  )
  evaluation_time <- units::set_units(10, "days")
  injection_schedule <- make_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    evaluation_time,
    stream_apportionment = inputs$stream_apportionment
  )
  injection_events <- isw:::.get_injection_rate_changes(injection_schedule)

  result <- model_aquifer_water_level_change(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    stream_point,
    inputs$stream_segments,
    evaluation_time,
    stream_injection_schedule = injection_schedule,
    stream_apportionment = inputs$stream_apportionment
  )

  expected_ratio <- isw:::.line_sink_aquifer_drawdown_ratio(
    along_distance = units::set_units(0, "m"),
    perpendicular_distance = units::set_units(0, "m"),
    line_length = inputs$stream_segments$represented_length[[1]],
    K = inputs$pumping_wells$K,
    D = inputs$pumping_wells$D,
    V = inputs$pumping_wells$V,
    t = evaluation_time,
    stream_width = inputs$stream_segments$stream_width[[1]]
  )
  expected_recovery <- units::set_units(
    injection_events$injection_rate_change[[1]] * expected_ratio,
    "m"
  )

  expect_true(is.finite(as.numeric(result$stream_recovery)))
  expect_equal(result$stream_recovery, expected_recovery)
})

test_that("apportioned drawdown reuses a supplied injection schedule", {
  inputs <- make_drawdown_test_inputs()
  injection_schedule <- make_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = inputs$stream_apportionment
  )
  internal_result <- model_aquifer_water_level_change(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$observation_wells,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = inputs$stream_apportionment
  )
  supplied_result <- model_aquifer_water_level_change(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$observation_wells,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_injection_schedule = injection_schedule,
    stream_apportionment = inputs$stream_apportionment
  )

  expect_equal(supplied_result, internal_result)
})

test_that("a supplied injection schedule controls the injection grid", {
  inputs <- make_drawdown_test_inputs()
  injection_schedule <- make_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = inputs$stream_apportionment
  )

  expect_error(
    model_aquifer_water_level_change(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$observation_wells,
      inputs$stream_segments,
      inputs$evaluation_times,
      injection_times = units::set_units(c(5, 15), "days"),
      stream_injection_schedule = injection_schedule,
      stream_apportionment = inputs$stream_apportionment
    ),
    "injection_times must be NULL"
  )
})

test_that("a supplied injection schedule must cover the evaluation period", {
  inputs <- make_drawdown_test_inputs()
  injection_schedule <- make_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    inputs$evaluation_times,
    stream_apportionment = inputs$stream_apportionment
  )
  incomplete_schedule <- injection_schedule[
    injection_schedule$interval_end < units::set_units(20, "days"),
    ,
    drop = FALSE
  ]

  expect_error(
    model_aquifer_water_level_change(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$observation_wells,
      inputs$stream_segments,
      inputs$evaluation_times,
      stream_injection_schedule = incomplete_schedule,
      stream_apportionment = inputs$stream_apportionment
    ),
    "continuous intervals"
  )
})

test_that("injection grid includes pumping times with sparse evaluations", {
  inputs <- make_drawdown_test_inputs()
  inputs$pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 10, 20), "days"),
    pump_1 = units::set_units(c(100, 100, 0), "m^3/day")
  )
  injection_schedule <- make_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    evaluation_times = units::set_units(30, "days"),
    stream_apportionment = inputs$stream_apportionment
  )

  expect_equal(
    unique(injection_schedule$interval_start),
    units::set_units(c(0, 10, 20), "days")
  )
  expect_equal(
    unique(injection_schedule$interval_end),
    units::set_units(c(10, 20, 30), "days")
  )
})

test_that("optional injection times refine the schedule", {
  inputs <- make_drawdown_test_inputs()
  injection_schedule <- make_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_segments,
    evaluation_times = units::set_units(20, "days"),
    injection_times = units::set_units(c(5, 15), "days"),
    stream_apportionment = inputs$stream_apportionment
  )

  expect_equal(
    unique(injection_schedule$interval_start),
    units::set_units(c(0, 5, 10, 15), "days")
  )
  expect_equal(
    unique(injection_schedule$interval_end),
    units::set_units(c(5, 10, 15, 20), "days")
  )
})
