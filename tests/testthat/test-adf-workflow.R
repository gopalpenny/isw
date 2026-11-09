make_adf_workflow_inputs <- function(dates = FALSE, zero = FALSE) {
  # External inputs retain physical units and CRS; rates deliberately use L/s.
  segments <- prep_stream_segments(example_stream_reaches, units::set_units(400, "m"))
  pumping_schedules <- tibble::tibble(
    t = if (dates) as.Date("2025-01-01") + c(0, 10, 20) else units::set_units(c(24, 264, 504), "hour"),
    pump_1 = units::set_units(if (zero) c(0, 0, 0) else c(1, .5, 0), "L/s"),
    pump_2 = units::set_units(if (zero) c(0, 0, 0) else c(0, .5, 0), "L/s")
  )
  times <- function(day) {
    if (dates) as.Date("2025-01-01") + day else units::set_units(24 + day * 24, "hour")
  }
  list(wells = example_pumping_wells, segments = segments, schedules = pumping_schedules,
       apportionment = prep_adf_stream_apportionment(example_pumping_wells, segments, units::set_units(100, "m")),
       evaluate = times(c(0, 5, 20, 30)), refine = times(c(2, 12)),
       time_boundaries = times(c(0, 2, 5, 10, 12, 20, 30)), times = times)
}

test_that("ADF schedules preserve dates, time units, rate units and endpoint averaging", {
  for (dates in c(FALSE, TRUE)) {
    x <- make_adf_workflow_inputs(dates)
    depletion <- model_adf_stream_depletion(x$wells, x$schedules, x$apportionment, x$time_boundaries)
    schedule <- generate_stream_injection_schedule(x$wells, x$schedules, x$segments, x$evaluate,
                                                   x$refine, stream_apportionment = x$apportionment)
    expect_identical(units::deparse_unit(depletion$stream_depletion_rate), "L s-1")
    expect_identical(units::deparse_unit(schedule$injection_rate), "L s-1")
    expect_identical(sort(unique(depletion$evaluation_time)), x$time_boundaries)
    expect_identical(sort(unique(schedule$interval_end)), x$time_boundaries[-1])
    first_pair <- depletion[depletion$pump_id == "pump_1" &
                              depletion$reach_segment_id == x$segments$reach_segment_id[[1]], ]
    first_schedule <- schedule[schedule$pump_id == "pump_1" &
                                schedule$reach_segment_id == x$segments$reach_segment_id[[1]], ]
    expect_equal(first_schedule$injection_rate,
                 (utils::head(first_pair$stream_depletion_rate, -1) + utils::tail(first_pair$stream_depletion_rate, -1)) / 2)
    expect_identical(attr(schedule, "isw_schedule_metadata")$injection_method, "adf")
  }
})

test_that("supplied ADF stream depletion rates are reused and rates at missing injection times are calculated", {
  for (dates in c(FALSE, TRUE)) {
    x <- make_adf_workflow_inputs(dates)
    complete <- model_adf_stream_depletion(x$wells, x$schedules, x$apportionment, x$time_boundaries)
    supplied <- complete[complete$evaluation_time %in% x$times(c(0, 20, 30)), ]
    supplied$stream_depletion_rate <- supplied$stream_depletion_rate + units::set_units(.1, "L/s")
    supplied$extra_note <- "unused attribute"
    expected_complete <- complete
    expected_complete$stream_depletion_rate[complete$evaluation_time %in% x$times(c(0, 20, 30))] <- supplied$stream_depletion_rate
    expected <- generate_stream_injection_schedule(
      x$wells, x$schedules, x$segments, x$evaluate, x$refine,
      stream_apportionment = x$apportionment, adf_stream_depletion = expected_complete
    )
    expect_warning(actual <- generate_stream_injection_schedule(
      x$wells, x$schedules, x$segments, x$evaluate, x$refine,
      stream_apportionment = x$apportionment, adf_stream_depletion = supplied
    ), "missing stream depletion rates at 4 required injection times")
    expect_equal(actual, expected)
    extra <- model_adf_stream_depletion(x$wells, x$schedules, x$apportionment, x$times(c(0, 2, 5, 10, 12, 20, 30, 40)))
    expect_silent(reused <- generate_stream_injection_schedule(
      x$wells, x$schedules, x$segments, x$evaluate, x$refine,
      stream_apportionment = x$apportionment, adf_stream_depletion = extra
    ))
    baseline <- generate_stream_injection_schedule(x$wells, x$schedules, x$segments, x$evaluate,
                                                   x$refine, stream_apportionment = x$apportionment)
    expect_equal(reused, baseline)
  }
})

test_that("zero ADF pumping preserves full grids and typed empty initial schedules", {
  x <- make_adf_workflow_inputs(zero = TRUE)
  depletion <- model_adf_stream_depletion(x$wells, x$schedules, x$apportionment, x$evaluate)
  expect_equal(nrow(depletion), nrow(x$apportionment) * length(x$evaluate))
  expect_true(all(as.numeric(depletion$stream_depletion_rate) == 0))
  schedule <- generate_stream_injection_schedule(x$wells, x$schedules, x$segments, x$evaluate,
                                                 stream_apportionment = x$apportionment)
  expect_true(all(as.numeric(schedule$injection_rate) == 0))
  initial <- generate_stream_injection_schedule(x$wells, x$schedules, x$segments, x$evaluate[1],
                                                stream_apportionment = x$apportionment)
  expect_equal(nrow(initial), 0)
  expect_s3_class(initial$injection_rate, "units")
  expect_s3_class(initial$interval_start, "units")
})

test_that("numeric ADF calculations reuse prepared events without boundary checks", {
  x <- make_adf_workflow_inputs()
  times <- isw:::.prepare_numeric_model_times(x$schedules, x$evaluate, x$refine)
  schedules <- isw:::.prepare_numeric_schedule(x$schedules, x$wells, times)
  wells <- isw:::.prepare_numeric_pumping_wells(x$wells)
  apportionment <- isw:::.prepare_numeric_apportionment(x$apportionment, x$wells)
  segments <- isw:::.prepare_numeric_segments(x$segments)
  local_mocked_bindings(
    .get_pumping_rate_changes = function(...) stop("unexpected repeated event preparation"),
    .validate_pumping_schedules = function(...) stop("unexpected repeated validation"),
    .normalize_time_inputs = function(...) stop("unexpected time conversion"),
    check_dimensionality = function(...) stop("unexpected units check"), .package = "isw"
  )
  expect_silent(result <- isw:::.generate_stream_injection_schedule(
    wells, schedules, segments, times$evaluation_time_day, times$injection_time_day,
    stream_apportionment = apportionment
  ))
  expect_false(inherits(result$injection_rate_m3_per_day, "units"))
  expect_false(inherits(result$interval_start_day, "units"))
})
