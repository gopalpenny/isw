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
  stream_apportionment <- get_stream_reach_apportionment(
    pumping_wells,
    stream_reaches,
    reach_spacing = units::set_units(100, "m"),
    sample_spacing = units::set_units(25, "m"),
    analysis_crs = 32615
  )
  stream_depletion <- get_apportioned_stream_depletion(
    pumping_wells,
    pumping_schedules,
    stream_apportionment,
    evaluation_times
  )

  list(
    pumping_wells = pumping_wells,
    pumping_schedules = pumping_schedules,
    observation_wells = observation_wells,
    stream_apportionment = stream_apportionment,
    stream_depletion = stream_depletion,
    evaluation_times = evaluation_times
  )
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

  expected <- get_aquifer_drawdown_ratio(
    distance = real_distance,
    K = K,
    D = D,
    V = V,
    t = elapsed_time
  ) - get_aquifer_drawdown_ratio(
    distance = image_distance,
    K = K,
    D = D,
    V = V,
    t = elapsed_time
  )

  expect_equal(
    get_straight_stream_drawdown_ratio(
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

test_that("apportioned drawdown superimposes pumping and stream injection", {
  inputs <- make_drawdown_test_inputs()
  result <- get_apportioned_aquifer_drawdown(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$observation_wells,
    inputs$stream_apportionment,
    inputs$evaluation_times
  )
  injection_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_apportionment,
    inputs$evaluation_times
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
  expected_recovery_10 <- units::set_units(
    injection_events$injection_rate_change[[1]] * pumping_ratio_10,
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
      "aquifer_drawdown"
    )
  )
  expect_equal(nrow(result), 3)
  expect_equal(result$pumping_drawdown[[1]], units::set_units(0, "m"))
  expect_equal(result$stream_recovery[[1]], units::set_units(0, "m"))
  expect_equal(result$pumping_drawdown[[2]], expected_pumping_10)
  expect_equal(result$stream_recovery[[2]], expected_recovery_10)
  expect_equal(
    result$aquifer_drawdown,
    result$pumping_drawdown - result$stream_recovery
  )
})

test_that("apportioned drawdown reuses a supplied injection schedule", {
  inputs <- make_drawdown_test_inputs()
  injection_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_apportionment,
    inputs$evaluation_times
  )
  internal_result <- get_apportioned_aquifer_drawdown(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$observation_wells,
    inputs$stream_apportionment,
    inputs$evaluation_times
  )
  supplied_result <- get_apportioned_aquifer_drawdown(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$observation_wells,
    inputs$stream_apportionment,
    inputs$evaluation_times,
    stream_injection_schedule = injection_schedule
  )

  expect_equal(supplied_result, internal_result)
})

test_that("a supplied injection schedule controls the injection grid", {
  inputs <- make_drawdown_test_inputs()
  injection_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_apportionment,
    inputs$evaluation_times
  )

  expect_error(
    get_apportioned_aquifer_drawdown(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$observation_wells,
      inputs$stream_apportionment,
      inputs$evaluation_times,
      injection_times = units::set_units(c(5, 15), "days"),
      stream_injection_schedule = injection_schedule
    ),
    "injection_times must be NULL"
  )
})

test_that("a supplied injection schedule must cover the evaluation period", {
  inputs <- make_drawdown_test_inputs()
  injection_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_apportionment,
    inputs$evaluation_times
  )
  incomplete_schedule <- injection_schedule[
    injection_schedule$interval_end < units::set_units(20, "days"),
    ,
    drop = FALSE
  ]

  expect_error(
    get_apportioned_aquifer_drawdown(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$observation_wells,
      inputs$stream_apportionment,
      inputs$evaluation_times,
      stream_injection_schedule = incomplete_schedule
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
  injection_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_apportionment,
    evaluation_times = units::set_units(30, "days")
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
  injection_schedule <- get_stream_injection_schedule(
    inputs$pumping_wells,
    inputs$pumping_schedules,
    inputs$stream_apportionment,
    evaluation_times = units::set_units(20, "days"),
    injection_times = units::set_units(c(5, 15), "days")
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
