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
