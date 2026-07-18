make_processing_pumping_wells <- function() {
  sf::st_as_sf(
    tibble::tibble(
      pump_id = c("pump_1", "pump_2"),
      x = c(-93.25, -93.20),
      y = c(44.95, 45.00),
      K = units::set_units(c(10, 15), "m/day"),
      D = units::set_units(c(20, 25), "m"),
      V = c(0.15, 0.20)
    ),
    coords = c("x", "y"),
    crs = 4326
  )
}

test_that("nonzero pumping-rate changes are returned with units", {
  pumping_wells <- make_processing_pumping_wells()
  pumping_schedules <- tibble::tibble(
    t = as.Date(c(
      "2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01"
    )),
    pump_1 = units::set_units(c(100, 100, 80, 0), "m^3/day"),
    pump_2 = units::set_units(c(0, 0, 50, 50), "m^3/day")
  )
  events <- isw:::.get_pumping_rate_changes(
    pumping_schedules,
    pumping_wells
  )

  expect_identical(
    events$pump_id,
    c("pump_1", "pump_1", "pump_1", "pump_2")
  )
  expect_equal(
    events$pumping_time,
    units::set_units(c(0, 59, 90, 59), "days")
  )
  expect_equal(
    events$pumping_rate_change,
    units::set_units(c(100, -20, -80, 50), "m^3/day")
  )
  expect_identical(
    units::deparse_unit(events$pumping_rate_change),
    units::deparse_unit(pumping_schedules$pump_1)
  )
})

test_that("signed rate changes are retained", {
  pumping_wells <- make_processing_pumping_wells()[1, ]
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 1, 2, 3), "days"),
    pump_1 = units::set_units(c(0, -10, -10, 0), "ft^3/day")
  )

  events <- isw:::.get_pumping_rate_changes(
    pumping_schedules,
    pumping_wells
  )

  expect_equal(
    events$pumping_rate_change,
    units::set_units(c(-10, 10), "ft^3/day")
  )
  expect_equal(
    events$pumping_time,
    units::set_units(c(1, 3), "days")
  )
})

test_that("an all-zero schedule returns a typed empty tibble", {
  pumping_wells <- make_processing_pumping_wells()
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 1, 2), "days"),
    pump_1 = units::set_units(c(0, 0, 0), "m^3/day"),
    pump_2 = units::set_units(c(0, 0, 0), "m^3/day")
  )

  events <- isw:::.get_pumping_rate_changes(
    pumping_schedules,
    pumping_wells
  )

  expect_s3_class(events, "tbl_df")
  expect_equal(nrow(events), 0)
  expect_identical(events$pump_id, character())
  expect_true(inherits(events$pumping_time, "units"))
  expect_true(inherits(events$pumping_rate_change, "units"))
  expect_identical(
    units::deparse_unit(events$pumping_rate_change),
    units::deparse_unit(pumping_schedules$pump_1)
  )
})

test_that("pumping changes are paired only with later evaluation times", {
  pumping_wells <- make_processing_pumping_wells()[1, ]
  pumping_schedules <- tibble::tibble(
    t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
    pump_1 = units::set_units(c(100, 80, 0), "m^3/day")
  )

  response_times <- isw:::.get_pumping_response_times(
    pumping_schedules,
    pumping_wells
  )

  expect_identical(
    names(response_times),
    c(
      "pump_id",
      "evaluation_time",
      "pumping_time",
      "elapsed_time",
      "pumping_rate_change"
    )
  )
  expect_identical(
    response_times$pump_id,
    rep("pump_1", 3)
  )
  expect_equal(
    response_times$pumping_time,
    units::set_units(c(0, 0, 31), "days")
  )
  expect_identical(
    response_times$evaluation_time,
    as.Date(c("2025-02-01", "2025-03-01", "2025-03-01"))
  )
  expect_equal(
    response_times$elapsed_time,
    units::set_units(c(31, 59, 28), "days")
  )
  expect_equal(
    response_times$pumping_rate_change,
    units::set_units(c(100, 100, -20), "m^3/day")
  )
  expect_true(all(response_times$elapsed_time > units::set_units(0, "days")))
})

test_that("response times are ordered for grouping and interpretation", {
  pumping_wells <- make_processing_pumping_wells()
  pumping_schedules <- tibble::tibble(
    t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
    pump_1 = units::set_units(c(100, 80, 0), "m^3/day"),
    pump_2 = units::set_units(c(0, 50, 50), "m^3/day")
  )
  evaluation_times <- seq.Date(
    from = pumping_schedules$t[2],
    by = "month",
    length.out = nrow(pumping_schedules)
  )

  response_times <- isw:::.get_pumping_response_times(
    pumping_schedules,
    pumping_wells,
    evaluation_times
  )

  expected_order <- order(
    response_times$pump_id,
    as.numeric(response_times$evaluation_time)
  )

  expect_identical(expected_order, seq_len(nrow(response_times)))
  expect_false(any(
    response_times$pump_id == "pump_2" &
      response_times$pumping_time == units::set_units(59, "days")
  ))
})

test_that("custom evaluation times retain their original representation", {
  pumping_wells <- make_processing_pumping_wells()[1, ]
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 24, 48), "hours"),
    pump_1 = units::set_units(c(10, 20, 0), "ft^3/day")
  )
  evaluation_times <- units::set_units(c(0, 0.5, 1, 3), "days")

  response_times <- isw:::.get_pumping_response_times(
    pumping_schedules,
    pumping_wells,
    evaluation_times
  )

  expect_identical(
    units::deparse_unit(response_times$evaluation_time),
    units::deparse_unit(evaluation_times)
  )
  expect_identical(
    units::deparse_unit(response_times$elapsed_time),
    units::deparse_unit(units::set_units(1, "days"))
  )
  expect_false(any(response_times$elapsed_time == units::set_units(0, "days")))
})

test_that("zero pumping produces a typed empty response-time table", {
  pumping_wells <- make_processing_pumping_wells()[1, ]
  pumping_schedules <- tibble::tibble(
    t = as.Date(c("2025-01-01", "2025-02-01")),
    pump_1 = units::set_units(c(0, 0), "m^3/day")
  )

  response_times <- isw:::.get_pumping_response_times(
    pumping_schedules,
    pumping_wells
  )

  expect_s3_class(response_times, "tbl_df")
  expect_equal(nrow(response_times), 0)
  expect_true(inherits(response_times$pumping_time, "units"))
  expect_s3_class(response_times$evaluation_time, "Date")
  expect_true(inherits(response_times$elapsed_time, "units"))
  expect_true(inherits(response_times$pumping_rate_change, "units"))
})
