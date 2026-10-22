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
    normalized_times$normalized_schedule_times,
    units::set_units(c(0, 31, 59), "days")
  )
  expect_equal(
    normalized_times$normalized_target_times,
    units::set_units(c(0, 14, 90), "days")
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
    normalized_times$normalized_target_times,
    normalized_times$normalized_schedule_times
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
    normalized_times$normalized_schedule_times,
    units::set_units(c(1, 2, 3), "days")
  )
  expect_equal(
    normalized_times$normalized_target_times,
    units::set_units(c(1, 2, 4), "days")
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
