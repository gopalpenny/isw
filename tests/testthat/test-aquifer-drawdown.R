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
  stream_apportionment <- get_stream_depletion_apportionment(
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
    stream_depletion = stream_depletion
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
    inputs$stream_depletion
  )
  injection_events <- isw:::.get_interval_average_injection_rate_changes(
    inputs$stream_depletion,
    inputs$pumping_schedules
  )

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

test_that("apportioned drawdown requires complete depletion combinations", {
  inputs <- make_drawdown_test_inputs()
  second_segment <- inputs$stream_apportionment
  second_segment$reach_segment_id <- "stream_1_segment_2"
  inputs$stream_apportionment$apportionment_fraction <- 0.5
  second_segment$apportionment_fraction <- 0.5
  expanded_apportionment <- rbind(
    inputs$stream_apportionment,
    second_segment
  )

  expect_error(
    get_apportioned_aquifer_drawdown(
      inputs$pumping_wells,
      inputs$pumping_schedules,
      inputs$observation_wells,
      expanded_apportionment,
      inputs$stream_depletion
    ),
    "every pump, reach-segment, and evaluation-time combination"
  )
})
