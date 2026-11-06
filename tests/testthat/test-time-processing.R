test_that("Date times are converted to elapsed days", {
  schedule_times <- as.Date(
    c("2025-01-01", "2025-02-01", "2025-03-01")
  )
  target_times <- as.Date(
    c("2025-01-01", "2025-01-15", "2025-04-01")
  )

  normalized_times <- isw:::.normalize_time_inputs(
    schedule_times,
    target_times
  )

  expect_equal(
    normalized_times$schedule_time_day,
    c(0, 31, 59)
  )
  expect_equal(
    normalized_times$target_time_day,
    c(0, 14, 90)
  )
  expect_identical(
    normalized_times$output_target_times,
    target_times
  )
  expect_identical(
    normalized_times$time_origin,
    as.Date("2025-01-01")
  )
  expect_true("target_times" %in% names(formals(
    isw:::.normalize_time_inputs
  )))
})

test_that("NULL target_times default to schedule_times", {
  schedule_times <- as.Date(
    c("2025-01-01", "2025-02-01", "2025-03-01")
  )

  normalized_times <- isw:::.normalize_time_inputs(schedule_times)

  expect_equal(
    normalized_times$target_time_day,
    normalized_times$schedule_time_day
  )
  expect_identical(
    normalized_times$output_target_times,
    schedule_times
  )
})

test_that("unit-based times are converted to days without shifting", {
  schedule_times <- units::set_units(c(24, 48, 72), "hours")
  target_times <- units::set_units(c(1, 2, 4), "days")

  normalized_times <- isw:::.normalize_time_inputs(
    schedule_times,
    target_times
  )

  expect_equal(
    normalized_times$schedule_time_day,
    c(1, 2, 3)
  )
  expect_equal(
    normalized_times$target_time_day,
    c(1, 2, 4)
  )
  expect_identical(
    normalized_times$output_target_times,
    target_times
  )
  expect_null(normalized_times$time_origin)
})

test_that("normalization does not modify input vectors", {
  schedule_times <- units::set_units(c(24, 48, 72), "hours")
  target_times <- units::set_units(c(24, 60, 96), "hours")
  original_schedule_times <- schedule_times
  original_target_times <- target_times

  isw:::.normalize_time_inputs(schedule_times, target_times)

  expect_identical(schedule_times, original_schedule_times)
  expect_identical(target_times, original_target_times)
})

test_that("normalization applies target-time validation", {
  schedule_times <- units::set_units(c(1, 2, 3), "days")
  target_times <- units::set_units(c(0, 2, 3), "days")

  expect_error(
    isw:::.normalize_time_inputs(schedule_times, target_times),
    "target_times cannot occur before the first pumping-schedule time"
  )
})

test_that("output formatting restores Date origins and caller time units", {
  origin <- as.Date("2025-01-01")
  expect_identical(isw:::.restore_time_values(c(0, 1, 31), origin, origin),
                   origin + c(0, 1, 31))
  hours <- units::set_units(c(24, 48), "hour")
  restored <- isw:::.restore_time_values(c(1, 2), hours)
  expect_equal(restored, hours)
  expect_identical(units::deparse_unit(restored), units::deparse_unit(hours))
})

test_that("numeric injection grids handle defaults and a single initial boundary", {
  schedule <- tibble::tibble(t_day = c(1, 2, 3))
  grid <- isw:::.get_stream_injection_times(schedule)
  expect_equal(grid$boundary_time_day, c(1, 2, 3))
  expect_equal(grid$interval_duration_day, c(1, 1))
  initial <- isw:::.get_stream_injection_times(schedule, evaluation_time_day = 1)
  expect_identical(initial$interval_start_day, numeric())
  expect_identical(initial$interval_duration_day, numeric())
})
