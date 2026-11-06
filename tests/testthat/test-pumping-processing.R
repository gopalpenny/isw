make_processing_pumping_wells <- function() {
  sf::st_as_sf(tibble::tibble(
    pump_id = c("pump_1", "pump_2"), x = c(-93.25, -93.20), y = c(44.95, 45),
    K = units::set_units(c(10, 15), "m/day"), D = units::set_units(c(20, 25), "m"), V = c(.15, .2)
  ), coords = c("x", "y"), crs = 4326)
}

prepare_processing_inputs <- function(schedules, wells, evaluation_times = NULL) {
  # Exercise the public-boundary preparation contract before numeric helper calls.
  times <- isw:::.prepare_numeric_model_times(schedules, evaluation_times)
  isw:::.prepare_numeric_schedule(schedules, wells, times)
}

test_that("nonzero pumping events are numeric days and m3 per day", {
  wells <- make_processing_pumping_wells()
  schedules <- tibble::tibble(
    t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01")),
    pump_1 = units::set_units(c(100, 100, 80, 0), "m^3/day"),
    pump_2 = units::set_units(c(0, 0, 50, 50), "m^3/day")
  )
  prepared <- prepare_processing_inputs(schedules, wells)
  events <- attr(prepared, "isw_pumping_events")
  expect_identical(events$pump_id, c("pump_1", "pump_1", "pump_1", "pump_2"))
  expect_equal(events$pumping_time_day, c(0, 59, 90, 59))
  expect_equal(events$pumping_rate_change_m3_per_day, c(100, -20, -80, 50))
  expect_type(events$pumping_time_day, "double")
  expect_false(inherits(events$pumping_rate_change_m3_per_day, "units"))
  expect_identical(events, isw:::.get_pumping_rate_changes(prepared, wells$pump_id))
})

test_that("signed events convert volume units without changing signs", {
  wells <- make_processing_pumping_wells()[1, ]
  schedules <- tibble::tibble(
    t = units::set_units(c(0, 1, 2, 3), "day"),
    pump_1 = units::set_units(c(0, -10, -10, 0), "ft^3/day")
  )
  events <- attr(prepare_processing_inputs(schedules, wells), "isw_pumping_events")
  expect_equal(events$pumping_rate_change_m3_per_day,
               as.numeric(units::set_units(units::set_units(c(-10, 10), "ft^3/day"), "m^3/day")))
  expect_equal(events$pumping_time_day, c(1, 3))
})

test_that("zero schedules retain typed empty events and responses", {
  wells <- make_processing_pumping_wells()
  schedules <- tibble::tibble(t = as.Date("2025-01-01") + 0:2,
                              pump_1 = units::set_units(c(0, 0, 0), "m^3/day"),
                              pump_2 = units::set_units(c(0, 0, 0), "m^3/day"))
  prepared <- prepare_processing_inputs(schedules, wells)
  events <- attr(prepared, "isw_pumping_events")
  responses <- attr(prepared, "isw_pumping_response_times")
  expect_s3_class(events, "tbl_df")
  expect_identical(events$pump_id, character())
  expect_identical(events$pumping_time_day, numeric())
  expect_identical(events$pumping_rate_change_m3_per_day, numeric())
  expect_identical(responses$evaluation_index, integer())
  expect_identical(responses$elapsed_time_day, numeric())
  expect_equal(nrow(responses), 0)
})

test_that("events pair only with strictly later evaluations", {
  wells <- make_processing_pumping_wells()[1, ]
  schedules <- tibble::tibble(t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
                              pump_1 = units::set_units(c(100, 80, 0), "m^3/day"))
  prepared <- prepare_processing_inputs(schedules, wells)
  responses <- attr(prepared, "isw_pumping_response_times")
  expect_identical(names(responses), c("pump_id", "evaluation_index", "evaluation_time_day",
                                      "pumping_time_day", "elapsed_time_day", "pumping_rate_change_m3_per_day"))
  expect_identical(responses$evaluation_index, c(2L, 3L, 3L))
  expect_equal(responses$evaluation_time_day, c(31, 59, 59))
  expect_equal(responses$pumping_time_day, c(0, 0, 31))
  expect_equal(responses$elapsed_time_day, c(31, 59, 28))
  expect_equal(responses$pumping_rate_change_m3_per_day, c(100, 100, -20))
  expect_true(all(responses$elapsed_time_day > 0))
})

test_that("multiple pumps preserve grouping order and omit unchanged rates", {
  wells <- make_processing_pumping_wells()
  schedules <- tibble::tibble(t = units::set_units(c(0, 1, 2), "day"),
                              pump_1 = units::set_units(c(100, 80, 0), "m^3/day"),
                              pump_2 = units::set_units(c(0, 50, 50), "m^3/day"))
  prepared <- prepare_processing_inputs(schedules, wells, units::set_units(c(1, 2, 3), "day"))
  responses <- attr(prepared, "isw_pumping_response_times")
  expect_identical(order(responses$pump_id, responses$evaluation_index), seq_len(nrow(responses)))
  expect_false(any(responses$pump_id == "pump_2" & responses$pumping_time_day == 2))
})

test_that("nonzero unit-based origins and caller output metadata are preserved", {
  wells <- make_processing_pumping_wells()[1, ]
  schedules <- tibble::tibble(t = units::set_units(c(24, 48, 72), "hour"),
                              pump_1 = units::set_units(c(1, 2, 0), "L/s"))
  evaluation_times <- units::set_units(c(1, 1.5, 2, 4), "day")
  prepared <- prepare_processing_inputs(schedules, wells, evaluation_times)
  expect_equal(prepared$t_day, c(1, 2, 3))
  expect_equal(prepared$pump_1_m3_per_day, c(86.4, 172.8, 0))
  metadata <- attr(prepared, "isw_time_metadata")
  expect_null(metadata$time_origin)
  expect_identical(metadata$output_evaluation_times, evaluation_times)
  expect_identical(metadata$output_schedule_times, schedules$t)
  expect_true(all(attr(prepared, "isw_pumping_response_times")$elapsed_time_day > 0))
})

test_that("prepared events can be reused for different evaluation grids without validation", {
  wells <- make_processing_pumping_wells()[1, ]
  schedules <- tibble::tibble(t = units::set_units(c(0, 1), "day"),
                              pump_1 = units::set_units(c(10, 0), "m^3/day"))
  prepared <- prepare_processing_inputs(schedules, wells)
  events <- attr(prepared, "isw_pumping_events")
  local_mocked_bindings(
    .normalize_time_inputs = function(...) stop("unexpected normalization"),
    .validate_pumping_schedules = function(...) stop("unexpected validation"),
    check_dimensionality = function(...) stop("unexpected units check"), .package = "isw"
  )
  expect_identical(isw:::.get_pumping_rate_changes(prepared, wells$pump_id), events)
  first <- isw:::.get_pumping_response_times(events, c(0, 1, 2))
  second <- isw:::.get_pumping_response_times(events, c(0, .5, 1, 2))
  expect_equal(first$elapsed_time_day, c(1, 2, 1))
  expect_equal(second$elapsed_time_day, c(.5, 1, 2, 1))
  expect_equal(sum(events$pumping_rate_change_m3_per_day), 0)
})
