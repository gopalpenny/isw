make_valid_pumping_wells <- function() {
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

make_valid_observation_wells <- function() {
  sf::st_as_sf(
    tibble::tibble(
      observation_id = c("obs_1", "obs_2"),
      x = c(-93.22, -93.18),
      y = c(44.97, 45.02)
    ),
    coords = c("x", "y"),
    crs = 4326
  )
}

make_valid_stream_reaches <- function() {
  geometry <- sf::st_sfc(
    sf::st_linestring(
      matrix(c(-93.30, 44.90, -93.25, 44.95), ncol = 2, byrow = TRUE)
    ),
    sf::st_multilinestring(
      list(
        matrix(c(-93.25, 44.95, -93.20, 45.00), ncol = 2, byrow = TRUE)
      )
    ),
    crs = 4326
  )

  sf::st_sf(
    reach_id = c("reach_1", "reach_2"),
    stream_name = c("Example River", "Example Creek"),
    geometry = geometry
  )
}

make_valid_pumping_schedules <- function() {
  tibble::tibble(
    t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
    pump_1 = units::set_units(c(100, 80, 0), "m^3/day"),
    pump_2 = units::set_units(c(50, 40, 0), "m^3/day")
  )
}

test_that("valid pumping_wells are returned unchanged", {
  pumping_wells <- make_valid_pumping_wells()

  expect_identical(
    isw:::.validate_pumping_wells(pumping_wells),
    pumping_wells
  )
})

test_that("optional well_diam is validated when present", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$well_diam <- units::set_units(c(0, 0.25), "m")

  expect_identical(
    isw:::.validate_pumping_wells(pumping_wells),
    pumping_wells
  )

  pumping_wells$well_diam <- units::set_units(c(0, -0.25), "m")

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "finite, nonnegative"
  )
})

test_that("pumping_wells must be a nonempty sf object with a CRS", {
  pumping_wells <- make_valid_pumping_wells()

  expect_error(
    isw:::.validate_pumping_wells(sf::st_drop_geometry(pumping_wells)),
    "must be an sf object"
  )

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells[0, ]),
    "at least one"
  )

  pumping_wells_without_crs <- suppressWarnings(
    sf::st_set_crs(pumping_wells, NA)
  )

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells_without_crs),
    "defined CRS"
  )
})

test_that("pumping_wells geometries must be nonempty points", {
  pumping_wells <- make_valid_pumping_wells()

  line_geometry <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  sf::st_geometry(pumping_wells) <- line_geometry

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "must be a POINT"
  )

  pumping_wells <- make_valid_pumping_wells()[1, ]
  sf::st_geometry(pumping_wells) <- sf::st_sfc(sf::st_point(), crs = 4326)

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "empty geometries"
  )
})

test_that("required pumping_wells columns are enforced", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$K <- NULL
  pumping_wells$D <- NULL

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "K, D"
  )
})

test_that("pump_id values must be valid and unique", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$pump_id <- factor(pumping_wells$pump_id)

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "character vector"
  )

  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$pump_id[2] <- " "

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "missing or empty"
  )

  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$pump_id[2] <- pumping_wells$pump_id[1]

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "must be unique"
  )

  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$pump_id[2] <- "t"

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "reserved for the pumping-schedule time column"
  )
})

test_that("aquifer properties have valid units and values", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$K <- units::set_units(c(10, 15), "m")

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "conversion failed"
  )

  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$K <- units::set_units(c(10, 0), "m/day")

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "finite, positive"
  )

  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$D <- units::set_units(c(20, -1), "m")

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "finite, positive"
  )

  pumping_wells <- make_valid_pumping_wells()
  pumping_wells$V <- c(0.15, 1.1)

  expect_error(
    isw:::.validate_pumping_wells(pumping_wells),
    "greater than 0 and at most 1"
  )
})

test_that("observation_wells may be NULL", {
  expect_null(isw:::.validate_observation_wells(NULL))
})

test_that("valid observation_wells are returned unchanged", {
  observation_wells <- make_valid_observation_wells()

  expect_identical(
    isw:::.validate_observation_wells(observation_wells),
    observation_wells
  )
})

test_that("observation_wells must have valid point geometry", {
  observation_wells <- make_valid_observation_wells()

  expect_error(
    isw:::.validate_observation_wells(
      sf::st_drop_geometry(observation_wells)
    ),
    "must be an sf object"
  )

  observation_wells_without_crs <- suppressWarnings(
    sf::st_set_crs(observation_wells, NA)
  )

  expect_error(
    isw:::.validate_observation_wells(observation_wells_without_crs),
    "defined CRS"
  )

  line_geometry <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  sf::st_geometry(observation_wells) <- line_geometry

  expect_error(
    isw:::.validate_observation_wells(observation_wells),
    "must be a POINT"
  )
})

test_that("observation_id is required", {
  observation_wells <- make_valid_observation_wells()
  observation_wells$observation_id <- NULL

  expect_error(
    isw:::.validate_observation_wells(observation_wells),
    "missing required column: observation_id"
  )
})

test_that("observation_id values must be valid and unique", {
  observation_wells <- make_valid_observation_wells()
  observation_wells$observation_id <- factor(
    observation_wells$observation_id
  )

  expect_error(
    isw:::.validate_observation_wells(observation_wells),
    "character vector"
  )

  observation_wells <- make_valid_observation_wells()
  observation_wells$observation_id[2] <- NA_character_

  expect_error(
    isw:::.validate_observation_wells(observation_wells),
    "missing or empty"
  )

  observation_wells <- make_valid_observation_wells()
  observation_wells$observation_id[2] <-
    observation_wells$observation_id[1]

  expect_error(
    isw:::.validate_observation_wells(observation_wells),
    "must be unique"
  )
})

test_that("valid stream reaches are returned unchanged", {
  stream_reaches <- make_valid_stream_reaches()

  expect_identical(
    isw:::.validate_stream_reaches(stream_reaches),
    stream_reaches
  )
})

test_that("stream reaches must be a nonempty sf object with a CRS", {
  stream_reaches <- make_valid_stream_reaches()

  expect_error(
    isw:::.validate_stream_reaches(sf::st_drop_geometry(stream_reaches)),
    "must be an sf object"
  )

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches[0, ]),
    "at least one"
  )

  stream_reaches_without_crs <- suppressWarnings(
    sf::st_set_crs(stream_reaches, NA)
  )

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches_without_crs),
    "defined CRS"
  )
})

test_that("reach_id values must be present, valid, and unique", {
  stream_reaches <- make_valid_stream_reaches()
  stream_reaches$reach_id <- NULL

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches),
    "missing required column: reach_id"
  )

  stream_reaches <- make_valid_stream_reaches()
  stream_reaches$reach_id <- factor(stream_reaches$reach_id)

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches),
    "character vector"
  )

  stream_reaches <- make_valid_stream_reaches()
  stream_reaches$reach_id[2] <- " "

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches),
    "missing or empty"
  )

  stream_reaches <- make_valid_stream_reaches()
  stream_reaches$reach_id[2] <- stream_reaches$reach_id[1]

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches),
    "must be unique"
  )
})

test_that("stream reaches must have nonempty line geometry", {
  stream_reaches <- make_valid_stream_reaches()
  sf::st_geometry(stream_reaches) <- sf::st_sfc(
    sf::st_point(c(-93.30, 44.90)),
    sf::st_point(c(-93.20, 45.00)),
    crs = 4326
  )

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches),
    "LINESTRING or MULTILINESTRING"
  )

  stream_reaches <- make_valid_stream_reaches()[1, ]
  sf::st_geometry(stream_reaches) <- sf::st_sfc(
    sf::st_linestring(matrix(numeric(), ncol = 2)),
    crs = 4326
  )

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches),
    "empty geometries"
  )
})

test_that("stream reaches must have finite, positive length", {
  stream_reaches <- make_valid_stream_reaches()[1, ]
  sf::st_geometry(stream_reaches) <- sf::st_sfc(
    sf::st_linestring(
      matrix(c(-93.25, 44.95, -93.25, 44.95), ncol = 2, byrow = TRUE)
    ),
    crs = 4326
  )

  expect_error(
    isw:::.validate_stream_reaches(stream_reaches),
    "finite, positive length"
  )
})

test_that("valid Date pumping schedules are returned unchanged", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()

  expect_identical(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    pumping_schedules
  )
})

test_that("valid unit-based pumping schedules are returned unchanged", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$t <- units::set_units(c(0, 31, 59), "days")

  expect_identical(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    pumping_schedules
  )
})

test_that("pumping schedule columns must match pump_id values", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$pump_2 <- NULL

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "missing required columns: pump_2"
  )

  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$unknown_pump <- units::set_units(c(1, 1, 1), "m^3/day")

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "do not match pump_id values: unknown_pump"
  )
})

test_that("pumping schedule times must use an accepted time representation", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$t <- c("2025-01-01", "2025-02-01", "2025-03-01")

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "must be a Date vector or a units vector"
  )

  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$t <- units::set_units(c(0, 1, 2), "m")

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "conversion failed"
  )
})

test_that("pumping schedule times must be complete and increasing", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$t[2] <- NA

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "cannot contain missing dates"
  )

  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$t <- pumping_schedules$t[c(1, 3, 2)]

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "unique and strictly increasing"
  )

  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$t[2] <- pumping_schedules$t[1]

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "unique and strictly increasing"
  )
})

test_that("pumping rates must have flow-rate units and finite values", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$pump_1 <- units::set_units(c(100, 80, 0), "m^3")

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "conversion failed"
  )

  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$pump_1[2] <- Inf

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "finite pumping rates"
  )
})

test_that("pumping-rate columns must use the same units", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$pump_2 <- units::set_units(
    c(50, 40, 0),
    "ft^3/day"
  )

  expect_error(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    "must use the same units"
  )
})

test_that("signed pumping rates are accepted", {
  pumping_wells <- make_valid_pumping_wells()
  pumping_schedules <- make_valid_pumping_schedules()
  pumping_schedules$pump_1[2] <- -20

  expect_identical(
    isw:::.validate_pumping_schedules(pumping_schedules, pumping_wells),
    pumping_schedules
  )
})

test_that("evaluation_times may be NULL", {
  pumping_schedules <- make_valid_pumping_schedules()

  expect_null(
    isw:::.validate_evaluation_times(NULL, pumping_schedules$t)
  )
})

test_that("valid Date evaluation_times are returned unchanged", {
  pumping_schedules <- make_valid_pumping_schedules()
  evaluation_times <- as.Date(
    c("2025-01-01", "2025-01-15", "2025-04-01")
  )

  expect_identical(
    isw:::.validate_evaluation_times(
      evaluation_times,
      pumping_schedules$t
    ),
    evaluation_times
  )
})

test_that("valid unit-based evaluation_times are returned unchanged", {
  schedule_times <- units::set_units(c(0, 31, 59), "days")
  evaluation_times <- units::set_units(c(0, 12, 24, 2160), "hours")

  expect_identical(
    isw:::.validate_evaluation_times(evaluation_times, schedule_times),
    evaluation_times
  )
})

test_that("evaluation_times must match the schedule time representation", {
  pumping_schedules <- make_valid_pumping_schedules()
  evaluation_times <- units::set_units(c(0, 31, 59), "days")

  expect_error(
    isw:::.validate_evaluation_times(
      evaluation_times,
      pumping_schedules$t
    ),
    "must both use Date values or both use units time values"
  )

  schedule_times <- units::set_units(c(0, 31, 59), "days")
  evaluation_times <- as.Date(c("2025-01-01", "2025-02-01"))

  expect_error(
    isw:::.validate_evaluation_times(evaluation_times, schedule_times),
    "must both use Date values or both use units time values"
  )
})

test_that("evaluation_times must be complete, unique, and increasing", {
  pumping_schedules <- make_valid_pumping_schedules()
  evaluation_times <- as.Date(c("2025-01-01", NA_character_))

  expect_error(
    isw:::.validate_evaluation_times(
      evaluation_times,
      pumping_schedules$t
    ),
    "cannot contain missing dates"
  )

  evaluation_times <- as.Date(
    c("2025-01-01", "2025-01-15", "2025-01-15")
  )

  expect_error(
    isw:::.validate_evaluation_times(
      evaluation_times,
      pumping_schedules$t
    ),
    "unique and strictly increasing"
  )
})

test_that("evaluation_times cannot precede the pumping schedule", {
  pumping_schedules <- make_valid_pumping_schedules()
  evaluation_times <- as.Date(c("2024-12-31", "2025-01-15"))

  expect_error(
    isw:::.validate_evaluation_times(
      evaluation_times,
      pumping_schedules$t
    ),
    "cannot occur before the first pumping-schedule time"
  )

  schedule_times <- units::set_units(c(24, 48, 72), "hours")
  evaluation_times <- units::set_units(c(0.5, 1, 2), "days")

  expect_error(
    isw:::.validate_evaluation_times(evaluation_times, schedule_times),
    "cannot occur before the first pumping-schedule time"
  )
})

test_that("injection_times may be NULL or use matching time inputs", {
  schedule_dates <- as.Date(c("2025-01-01", "2025-02-01"))
  injection_dates <- as.Date(c("2025-01-15", "2025-01-20"))

  expect_null(isw:::.validate_injection_times(NULL, schedule_dates))
  expect_identical(
    isw:::.validate_injection_times(injection_dates, schedule_dates),
    injection_dates
  )
})

test_that("injection_times must match the schedule representation", {
  schedule_times <- units::set_units(c(0, 10), "days")

  expect_error(
    isw:::.validate_injection_times(
      as.Date(c("2025-01-01", "2025-01-02")),
      schedule_times
    ),
    "must both use Date values"
  )
})

test_that("injection_times cannot precede the pumping schedule", {
  schedule_times <- units::set_units(c(1, 10), "days")

  expect_error(
    isw:::.validate_injection_times(
      units::set_units(c(0, 5), "days"),
      schedule_times
    ),
    "cannot occur before"
  )
})
