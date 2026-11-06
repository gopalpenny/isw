make_boundary_inputs <- function(dates = FALSE) {
  # Public fixtures retain units and CRS metadata; no modeling calls are needed.
  reaches <- sf::st_sf(
    reach_id = "stream", geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(100, -50, 100, 50), ncol = 2, byrow = TRUE)),
      crs = 32615
    )
  )
  segments <- reaches
  segments$reach_segment_id <- "segment"
  segments$represented_length <- units::set_units(100, "m")
  segments$stream_width <- units::set_units(2, "ft")
  segments$model_point <- sf::st_sfc(sf::st_point(c(100, 0)), crs = 32615)
  wells <- sf::st_sf(
    pump_id = "pump", K = units::set_units(1, "ft/day"),
    D = units::set_units(10, "ft"), V = 0.2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 32615)
  )
  observations <- sf::st_sf(
    observation_id = "obs",
    geometry = sf::st_sfc(sf::st_point(c(50, 0)), crs = 32615)
  )
  schedules <- tibble::tibble(
    t = if (dates) as.Date("2025-01-01") + c(0, 1) else units::set_units(c(0, 24), "hour"),
    pump = units::set_units(c(1, 0), "L/s")
  )
  evaluation_times <- if (dates) {
    as.Date("2025-01-01") + c(0, 1, 2)
  } else units::set_units(c(0, 24, 48), "hour")
  apportionment <- tibble::tibble(
    pump_id = "pump", reach_id = "stream", reach_segment_id = "segment",
    pump_to_reach_distance = units::set_units(100, "ft"), apportionment_fraction = 1
  )
  injection <- tibble::tibble(
    pump_id = "pump", reach_id = "stream", reach_segment_id = "segment",
    interval_start = evaluation_times[1], interval_end = evaluation_times[3],
    injection_rate = units::set_units(1, "L/s")
  )
  list(reaches = reaches, segments = segments, wells = wells,
       observations = observations, schedules = schedules,
       evaluation_times = evaluation_times, apportionment = apportionment,
       injection = injection)
}

test_that("public workflows validate each context object once", {
  inputs <- make_boundary_inputs()
  depletion <- model_adf_stream_depletion(
    inputs$wells, inputs$schedules, inputs$apportionment, inputs$evaluation_times
  )
  validation_names <- c(".validate_pumping_wells", ".validate_pumping_schedules",
                        ".validate_stream_segments", ".validate_stream_depletion_apportionment",
                        ".validate_observation_wells")
  counts <- stats::setNames(integer(length(validation_names)), validation_names)
  bindings <- stats::setNames(lapply(validation_names, function(name) {
    original <- get(name, envir = asNamespace("isw"))
    force(original)
    force(name)
    function(...) {
      counts[[name]] <<- counts[[name]] + 1L
      original(...)
    }
  }), validation_names)
  do.call(local_mocked_bindings, c(bindings, list(.package = "isw", .env = environment())))
  routes <- list(
    apportionment = function() prep_adf_stream_apportionment(
      inputs$wells, inputs$segments, units::set_units(25, "m")
    ),
    depletion = function() model_adf_stream_depletion(
      inputs$wells, inputs$schedules, inputs$apportionment, inputs$evaluation_times
    ),
    adf_schedule = function() generate_stream_injection_schedule(
      inputs$wells, inputs$schedules, inputs$segments, inputs$evaluation_times,
      stream_apportionment = inputs$apportionment, adf_stream_depletion = depletion
    ),
    constant_head_schedule = function() generate_stream_injection_schedule(
      inputs$wells, inputs$schedules, inputs$segments, inputs$evaluation_times,
      method = "constant_head"
    ),
    adf_model = function() model_aquifer_water_level_change(
      inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
      inputs$evaluation_times, stream_apportionment = inputs$apportionment
    ),
    constant_head_model = function() model_aquifer_water_level_change(
      inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
      inputs$evaluation_times, injection_method = "constant_head"
    ),
    supplied_schedule_model = function() model_aquifer_water_level_change(
      inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
      inputs$evaluation_times, stream_injection_schedule = inputs$injection
    )
  )
  expected_counts <- list(c(1L, 0L, 1L, 0L, 0L), c(1L, 1L, 0L, 1L, 0L),
                          c(1L, 1L, 1L, 1L, 0L), c(1L, 1L, 1L, 0L, 0L),
                          c(1L, 1L, 1L, 1L, 1L), c(1L, 1L, 1L, 0L, 1L),
                          c(1L, 1L, 1L, 0L, 1L))
  for (index in seq_along(routes)) {
    counts[] <- 0L
    routes[[index]]()
    expect_identical(unname(counts), expected_counts[[index]], info = names(routes)[[index]])
  }
})

test_that("public boundaries reject invalid values before unchecked calculations", {
  inputs <- make_boundary_inputs()
  run_model <- function(observations = inputs$observations, schedules = inputs$schedules,
                        injection = inputs$injection, quadrature_order = 16L) {
    model_aquifer_water_level_change(
      inputs$wells, schedules, observations, inputs$segments, inputs$evaluation_times,
      stream_injection_schedule = injection, quadrature_order = quadrature_order
    )
  }
  expect_error(run_model(observations = NULL), "observation_wells must be a nonempty sf")
  dates <- inputs$schedules
  dates$t <- as.Date(c(0, Inf), origin = "2025-01-01")
  expect_error(run_model(schedules = dates), "finite dates")
  expect_error(run_model(quadrature_order = units::set_units(16, "m")), "positive integer")
  injection <- inputs$injection
  injection$boundary_residual <- 1
  expect_error(run_model(injection = injection), "boundary_residual.*units object")
  injection$boundary_residual <- units::set_units(Inf, "m")
  expect_error(run_model(injection = injection), "boundary_residual must be finite")

  # Metadata is optional; malformed recorded orders must fall back to the default.
  injection <- inputs$injection
  attr(injection, "isw_schedule_metadata") <- list(
    injection_method = "constant_head", quadrature_order = "sixteen"
  )
  expect_equal(run_model(injection = injection), run_model())
  empty <- inputs$injection[0, ]
  empty$injection_rate <- numeric()
  expect_error(model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
    inputs$evaluation_times[1], stream_injection_schedule = empty
  ), "injection_rate.*units object")

  zero_length <- inputs$segments
  sf::st_geometry(zero_length) <- sf::st_sfc(
    sf::st_linestring(matrix(c(100, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 32615
  )
  zero_length$represented_length <- units::set_units(0, "m")
  expect_error(prep_stream_sample_points(zero_length, units::set_units(25, "m")),
               "positive geometry lengths")
  empty_line <- inputs$segments
  sf::st_geometry(empty_line) <- sf::st_sfc(sf::st_linestring(), crs = 32615)
  expect_error(generate_stream_injection_schedule(
    inputs$wells, inputs$schedules, empty_line, inputs$evaluation_times,
    method = "constant_head"
  ), "nonempty geometries")
})

test_that("unit validation preserves caller unit options", {
  previous_mode <- units::units_options("set_units_mode")
  on.exit(units::units_options(set_units_mode = previous_mode))
  units::units_options(set_units_mode = "symbols")
  inputs <- make_boundary_inputs()
  model_adf_stream_depletion(
    inputs$wells, inputs$schedules, inputs$apportionment, inputs$evaluation_times
  )
  expect_identical(units::units_options("set_units_mode"), "symbols")
})

test_that("prepared aquifer coordinators do not call boundary validators", {
  inputs <- make_boundary_inputs()
  segments <- isw:::.prepare_numeric_segments(inputs$segments)
  wells <- isw:::.prepare_numeric_wells(inputs$wells, sf::st_crs(segments))
  attr(wells, "isw_aquifer_groups") <- isw:::.prepare_aquifer_groups(wells)
  times <- isw:::.prepare_numeric_model_times(inputs$schedules, inputs$evaluation_times)
  schedules <- isw:::.prepare_numeric_schedule(inputs$schedules, inputs$wells, times)
  apportionment <- isw:::.prepare_numeric_apportionment(inputs$apportionment, inputs$wells)
  methods <- c("adf", "constant_head")
  expected <- lapply(methods, function(method) model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
    inputs$evaluation_times, injection_method = method,
    stream_apportionment = inputs$apportionment
  ))
  reject_validation <- function(...) stop("unexpected internal boundary validation")
  local_mocked_bindings(
    .validate_pumping_wells = reject_validation,
    .validate_pumping_schedules = reject_validation,
    .validate_stream_segments = reject_validation,
    .validate_stream_depletion_apportionment = reject_validation,
    .validate_observation_wells = reject_validation,
    .validate_evaluation_times = reject_validation,
    .validate_quadrature_order = reject_validation,
    check_dimensionality = reject_validation, .package = "isw"
  )
  for (index in seq_along(methods)) {
    numeric_result <- isw:::.model_aquifer_water_level_change(
      wells, schedules, inputs$observations, segments, times$evaluation_time_day,
      injection_method = methods[[index]], stream_apportionment = apportionment
    )
    expect_equal(isw:::.restore_aquifer_output(numeric_result, times, "ft"),
                 expected[[index]], info = methods[[index]])
  }
})

test_that("analytical public boundaries pass normalized numeric arguments", {
  captured <- NULL
  capture_kernel <- function(...) {
    captured <<- list(...)
    0
  }
  local_mocked_bindings(
    .glover_stream_depletion_fraction = capture_kernel,
    .theis_aquifer_drawdown_ratio = capture_kernel,
    .glover_aquifer_drawdown_ratio = capture_kernel,
    .package = "isw"
  )
  K <- units::set_units(1, "ft/s")
  D <- units::set_units(10, "ft")
  t <- units::set_units(24, "hour")
  distance <- units::set_units(100, "ft")
  calc_straight_stream_depletion_fraction(distance, K, D, 0.2, t)
  expect_equal(captured$x1_m, 30.48)
  expect_equal(captured$K_m_per_day, 26334.72)
  expect_equal(captured$D_m, 3.048)
  expect_identical(captured$t_day, 1)
  expect_false(any(vapply(captured, inherits, logical(1), "units")))

  result <- calc_infinite_aquifer_drawdown_ratio(distance, K, D, 0.2, t)
  expect_equal(captured$distance_m, 30.48)
  expect_equal(captured$well_diam_m, 0)
  expect_identical(units::deparse_unit(result), units::deparse_unit(1 / (K * D)))

  calc_straight_stream_drawdown_ratio(distance, distance, -distance, K, D, 0.2, t)
  expect_equal(captured$y_diff_m, -30.48)
  expect_equal(captured$t_day, 1)
  expect_error(calc_infinite_aquifer_drawdown_ratio(1, K, D, 0.2, t), "units object")
  expect_error(calc_straight_stream_depletion_fraction(distance, K, D, 2, t), "V must")
  expect_error(calc_straight_stream_depletion_fraction(distance, K, D, 0.2, t * 0), "positive")
  expect_error(calc_infinite_aquifer_drawdown_ratio(distance, K, D, 0.2, t,
                                                   units::set_units(-1, "m")), "nonnegative")
  expect_error(calc_straight_stream_depletion_fraction(rep(distance, 2), rep(K, 3),
                                                      D, 0.2, t), "common length")
})

test_that("spatial public boundaries strip measurements but preserve CRS", {
  inputs <- make_boundary_inputs()
  local_mocked_bindings(
    .prep_stream_segments = function(stream_reaches, reach_spacing, ...) {
      list(reaches = stream_reaches, spacing_m = reach_spacing)
    },
    .prep_stream_sample_points = function(stream_segments, sample_spacing) {
      list(segments = stream_segments, spacing_m = sample_spacing)
    },
    .prep_adf_stream_apportionment = function(pumping_wells, stream_segments,
                                             sample_spacing, method, maximum_distance) {
      list(wells = pumping_wells, segments = stream_segments,
           spacing_m = sample_spacing, maximum_distance_m = maximum_distance)
    }, .package = "isw"
  )
  result <- prep_stream_segments(inputs$reaches, units::set_units(100, "ft"))
  expect_equal(result$spacing_m, 30.48)
  expect_identical(result$reaches$stream_width_m, 1)
  expect_identical(sf::st_crs(result$reaches), sf::st_crs(inputs$reaches))
  result <- prep_stream_sample_points(inputs$segments, units::set_units(10, "ft"))
  expect_equal(result$segments$represented_length_m, 100)
  expect_equal(result$segments$stream_width_m, 0.6096)
  expect_equal(result$spacing_m, 3.048)
  result <- prep_adf_stream_apportionment(
    inputs$wells, inputs$segments, units::set_units(10, "ft"),
    maximum_distance = units::set_units(1, "km")
  )
  expect_equal(result$wells$K_m_per_day, 0.3048)
  expect_equal(result$wells$D_m, 3.048)
  expect_equal(result$wells$well_diam_m, 0)
  expect_equal(result$maximum_distance_m, 1000)
  expect_error(prep_stream_segments(inputs$reaches, units::set_units(-1, "m")), "positive")
  expect_error(prep_stream_sample_points(inputs$segments, 10), "units object")
  expect_true("K" %in% names(inputs$wells))
})

test_that("public ADF and injection boundaries prepare numeric tables and times", {
  local_mocked_bindings(
    .calculate_adf_stream_depletion = function(pumping_wells, pumping_schedules,
                                               stream_apportionment, evaluation_times) {
      list(wells = pumping_wells, schedules = pumping_schedules,
           apportionment = stream_apportionment, evaluation_time_day = evaluation_times)
    },
    .generate_stream_injection_schedule = function(pumping_wells, pumping_schedules,
                                                   stream_segments, evaluation_times,
                                                   injection_times, method,
                                                   stream_apportionment, ...) {
      list(wells = pumping_wells, schedules = pumping_schedules,
           segments = stream_segments, evaluation_time_day = evaluation_times,
           injection_time_day = injection_times, apportionment = stream_apportionment)
    }, .package = "isw"
  )
  for (dates in c(FALSE, TRUE)) {
    inputs <- make_boundary_inputs(dates)
    result <- model_adf_stream_depletion(inputs$wells, inputs$schedules,
                                        inputs$apportionment, inputs$evaluation_times)
    expect_equal(result$schedules$t_day, c(0, 1))
    expect_equal(result$schedules$pump_m3_per_day, c(86.4, 0))
    expect_equal(result$apportionment$pump_to_reach_distance_m, 30.48)
    expect_equal(result$evaluation_time_day, c(0, 1, 2))
    expect_identical(attr(result$schedules, "isw_time_metadata")$output_evaluation_times,
                     inputs$evaluation_times)
    expect_identical(attr(result$schedules, "isw_time_metadata")$output_schedule_times,
                     inputs$schedules$t)
    result <- generate_stream_injection_schedule(
      inputs$wells, inputs$schedules, inputs$segments, inputs$evaluation_times,
      injection_times = inputs$evaluation_times[2], stream_apportionment = inputs$apportionment
    )
    expect_equal(result$injection_time_day, 1)
    expect_equal(result$segments$stream_width_m, 0.6096)
  }
})

test_that("aquifer boundary validates and normalizes a supplied injection schedule", {
  local_mocked_bindings(
    .model_aquifer_water_level_change = function(pumping_wells, pumping_schedules,
                                                 observation_wells, stream_segments,
                                                 evaluation_times, injection_times,
                                                 stream_injection_schedule, injection_method,
                                                 stream_apportionment,
                                                 allow_n_aquifer_parameter_sets, quadrature_order) {
      list(injection = stream_injection_schedule, quadrature_order = quadrature_order)
    }, .package = "isw"
  )
  for (dates in c(FALSE, TRUE)) {
    inputs <- make_boundary_inputs(dates)
    attr(inputs$injection, "isw_schedule_metadata") <- list(
      injection_method = "constant_head", quadrature_order = 4L
    )
    result <- model_aquifer_water_level_change(
      inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
      inputs$evaluation_times, stream_injection_schedule = inputs$injection
    )
    expect_equal(result$injection$injection_rate_m3_per_day, 86.4)
    expect_equal(result$injection$interval_start_day, 0)
    expect_equal(result$injection$interval_end_day, 2)
    expect_identical(result$quadrature_order, 4L)
    inputs$injection$injection_rate <- 1
    expect_error(model_aquifer_water_level_change(
      inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
      inputs$evaluation_times, stream_injection_schedule = inputs$injection
    ), "units object")
  }
})

test_that("USGS boundary passes a numeric meter buffer to its spatial adapter", {
  inputs <- make_boundary_inputs()
  captured_buffer_m <- NULL
  local_mocked_bindings(
    .make_usgs_stream_query_area = function(aoi, buffer_distance) {
      captured_buffer_m <<- buffer_distance
      stop("query boundary reached")
    }, .package = "isw"
  )
  expect_error(fetch_usgs_stream_reaches(inputs$wells, units::set_units(1, "km")),
               "query boundary reached")
  expect_identical(captured_buffer_m, 1000)
})

test_that("schedule boundary prepares supplied depletion and constant-head inputs", {
  local_mocked_bindings(
    .generate_stream_injection_schedule = function(pumping_wells, pumping_schedules,
                                                   stream_segments, evaluation_times,
                                                   injection_times, method,
                                                   stream_apportionment,
                                                   allow_n_aquifer_parameter_sets,
                                                   adf_stream_depletion, quadrature_order) {
      list(wells = pumping_wells, schedules = pumping_schedules,
           depletion = adf_stream_depletion, method = method)
    }, .package = "isw"
  )
  for (dates in c(FALSE, TRUE)) {
    inputs <- make_boundary_inputs(dates)
    depletion <- tibble::tibble(
      pump_id = "pump", reach_id = "stream", reach_segment_id = "segment",
      evaluation_time = inputs$evaluation_times,
      stream_depletion_rate = units::set_units(c(0, 1, 0), "L/s")
    )
    result <- generate_stream_injection_schedule(
      inputs$wells, inputs$schedules, inputs$segments, inputs$evaluation_times,
      stream_apportionment = inputs$apportionment, adf_stream_depletion = depletion
    )
    expect_equal(result$depletion$stream_depletion_rate_m3_per_day, c(0, 86.4, 0))
    expect_equal(result$depletion$evaluation_time_day, c(0, 1, 2))
    result <- generate_stream_injection_schedule(
      inputs$wells, inputs$schedules, inputs$segments, inputs$evaluation_times,
      method = "constant_head"
    )
    expect_equal(result$wells$K_m_per_day, 0.3048)
    expect_equal(result$schedules$t_day, c(0, 1))
    expect_null(result$depletion)
    expect_error(generate_stream_injection_schedule(
      inputs$wells, inputs$schedules, inputs$segments, inputs$evaluation_times,
      method = "constant_head", quadrature_order = 0
    ), "positive integer")
  }
})

test_that("aquifer boundary prepares both generated-schedule paths", {
  local_mocked_bindings(
    .model_aquifer_water_level_change = function(pumping_wells, pumping_schedules,
                                                 observation_wells, stream_segments,
                                                 evaluation_times, injection_times,
                                                 stream_injection_schedule, injection_method,
                                                 stream_apportionment, ...) {
      list(wells = pumping_wells, schedules = pumping_schedules,
           apportionment = stream_apportionment, injection = stream_injection_schedule)
    }, .package = "isw"
  )
  inputs <- make_boundary_inputs()
  for (method in c("adf", "constant_head")) {
    result <- model_aquifer_water_level_change(
      inputs$wells, inputs$schedules, inputs$observations, inputs$segments,
      inputs$evaluation_times, injection_method = method,
      stream_apportionment = inputs$apportionment
    )
    expect_equal(result$wells$D_m, 3.048)
    expect_equal(result$schedules$pump_m3_per_day, c(86.4, 0))
    expect_null(result$injection)
    if (method == "adf") expect_equal(result$apportionment$pump_to_reach_distance_m, 30.48)
  }
  invalid_observations <- inputs$observations
  invalid_observations$observation_id <- NULL
  expect_error(model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, invalid_observations, inputs$segments,
    inputs$evaluation_times, stream_apportionment = inputs$apportionment
  ), "observation_id")
})
