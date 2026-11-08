make_point_injection_inputs <- function() {
  # Each segment has two edges; its model point is not an edge midpoint.
  reaches <- sf::st_sf(
    reach_id = c("r1", "r2"),
    stream_width = units::set_units(c(10, 20), "m"),
    geometry = sf::st_sfc(lapply(c(0, 60), function(offset) {
      sf::st_linestring(matrix(c(offset, 0, offset + 30, 0,
                                offset + 30, 40), ncol = 2, byrow = TRUE))
    }), crs = 32615)
  )
  segments <- prep_stream_segments(reaches, units::set_units(1000, "m"))
  wells <- sf::st_sf(
    pump_id = "p", K = units::set_units(10, "m/day"),
    D = units::set_units(20, "m"), V = 0.15,
    geometry = sf::st_sfc(sf::st_point(c(-100, 0)), crs = 32615)
  )
  list(
    segments = segments, wells = wells,
    schedules = tibble::tibble(
      t = units::set_units(c(0, 10, 25), "days"),
      p = units::set_units(c(100, 50, 0), "m^3/day")
    ),
    times = units::set_units(c(0, 10, 20, 30), "days"),
    observations = sf::st_sf(observation_id = c("a", "b"), geometry = segments$model_point)
  )
}

test_that("order zero uses model points and source-specific half-widths", {
  inputs <- make_point_injection_inputs()
  segments <- isw:::.prepare_numeric_segments(inputs$segments)
  # Public targets may need transformation back to the analysis CRS.
  targets <- sf::st_transform(inputs$observations, 4326)
  operator <- isw:::.prepare_stream_response_operator(targets, segments, 0L)
  expect_s3_class(operator, "isw_point_response_operator")
  expect_equal(operator$distances_m, matrix(c(0, 60, 60, 0), 2), tolerance = 1e-7)
  expected_distances <- matrix(c(5, 60, 60, 10), 2)
  expect_equal(operator$effective_distances_m, expected_distances, tolerance = 1e-7)
  expect_null(operator$weights)
  expect_null(operator$segment_columns)

  # Independent Theis expression at known distances, in day/m^2.
  expected <- matrix(-expint::expint_E1(expected_distances^2 * 0.15 / (4 * 200 * 10)) /
                       (4 * pi * 200), 2)
  colnames(expected) <- segments$reach_segment_id
  response <- isw:::.evaluate_stream_response_operator(operator, 10, 20, 0.15, 10)
  expect_equal(response, expected, tolerance = 1e-10)
  expect_true(all(is.finite(response)))

  # Preserve matrix dimensions for single targets and single sources as well.
  for (target_rows in list(1L, 1:2)) {
    single_source <- isw:::.prepare_stream_response_operator(
      inputs$observations[target_rows, ], segments[1, ], 0L
    )
    expect_equal(dim(isw:::.evaluate_stream_response_operator(
      single_source, 10, 20, 0.15, 10
    )), c(length(target_rows), 1L))
  }
})

test_that("positive orders retain the existing finite-line response", {
  inputs <- make_point_injection_inputs()
  segments <- isw:::.prepare_numeric_segments(inputs$segments)
  for (order in c(1L, 2L, 4L, 16L)) {
    original <- isw:::.prepare_line_response_operator(
      inputs$observations, isw:::.prepare_line_elements(segments, order)
    )
    selected <- isw:::.prepare_stream_response_operator(inputs$observations, segments, order)
    expect_identical(selected, original)
    expect_identical(
      isw:::.evaluate_stream_response_operator(selected, 10, 20, 0.15, 10),
      isw:::.evaluate_line_response_operator(original, 10, 20, 0.15, 10)
    )
  }
})

test_that("point response caches retain operator ownership and reuse", {
  inputs <- make_point_injection_inputs()
  segments <- isw:::.prepare_numeric_segments(inputs$segments)
  operator <- isw:::.prepare_stream_response_operator(inputs$observations, segments, 0L)
  cache <- new.env(parent = emptyenv())
  first <- isw:::.get_cached_stream_response_matrix(operator, cache, 10, 20, 0.15, 10)
  expect_identical(first,
    isw:::.get_cached_stream_response_matrix(operator, cache, 10, 20, 0.15, 10))
  expect_length(ls(cache), 1L)
  line_operator <- isw:::.prepare_stream_response_operator(inputs$observations, segments, 4L)
  expect_error(isw:::.get_cached_stream_response_matrix(
    line_operator, cache, 10, 20, 0.15, 10
  ), "cannot be reused with a different response_operator")
})

test_that("point constant-head schedules reproduce endpoint heads without line processing", {
  inputs <- make_point_injection_inputs()
  reject_line_processing <- function(...) stop("unexpected line-source processing")
  local_mocked_bindings(
    .prepare_line_elements = reject_line_processing,
    .get_gauss_legendre_rule = reject_line_processing,
    .prepare_line_response_operator = reject_line_processing,
    .evaluate_line_response_operator = reject_line_processing,
    .package = "isw"
  )
  schedule <- generate_stream_injection_schedule(
    inputs$wells, inputs$schedules, inputs$segments, inputs$times,
    method = "constant_head", quadrature_order = 0L
  )
  expect_identical(attr(schedule, "isw_schedule_metadata")$quadrature_order, 0L)
  expect_lt(max(abs(as.numeric(schedule$boundary_residual))), 1e-10)
  expect_true(all(is.finite(schedule$matrix_condition_number)))
  reused <- model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments, inputs$times,
    stream_injection_schedule = schedule
  )
  internal <- model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments, inputs$times,
    injection_method = "constant_head", quadrature_order = 0L
  )
  expect_equal(reused, internal)
  expect_lt(max(abs(as.numeric(reused$water_level_change))), 1e-10)
})

test_that("point and order-one schedules warn on mismatched drawdown orders", {
  inputs <- make_point_injection_inputs()
  for (order in c(0L, 1L)) {
    schedule <- generate_stream_injection_schedule(
      inputs$wells, inputs$schedules, inputs$segments, inputs$times,
      method = "constant_head", quadrature_order = order
    )
    expect_warning(mismatched <- model_aquifer_water_level_change(
      inputs$wells, inputs$schedules, inputs$observations, inputs$segments, inputs$times,
      stream_injection_schedule = schedule, quadrature_order = 1L - order
    ), "differs from the value used to construct")
    expect_gt(max(abs(as.numeric(mismatched$water_level_change))), 1e-6)
  }
})

test_that("ADF point recovery matches direct well pulses and preserves segment rates", {
  inputs <- make_point_injection_inputs()
  apportionment <- prep_adf_stream_apportionment(
    inputs$wells, inputs$segments, units::set_units(15, "m")
  )
  schedule_at_order <- function(order) generate_stream_injection_schedule(
    inputs$wells, inputs$schedules, inputs$segments, inputs$times,
    stream_apportionment = apportionment, quadrature_order = order
  )
  schedule <- schedule_at_order(0L)
  expect_identical(schedule, schedule_at_order(1L))
  expect_identical(schedule, schedule_at_order(4L))
  result <- model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments, inputs$times,
    stream_apportionment = apportionment, quadrature_order = 0L
  )
  supplied <- model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments, inputs$times,
    stream_injection_schedule = schedule, quadrature_order = 0L
  )
  expect_equal(result, supplied)

  # Sum interval pulses directly, independently of event preparation and operators.
  coordinates <- sf::st_coordinates(inputs$segments$model_point)
  for (row in seq_len(nrow(result))) {
    target <- match(result$observation_id[[row]], inputs$observations$observation_id)
    time <- as.numeric(units::set_units(result$evaluation_time[[row]], "days"))
    expected <- 0
    for (interval in seq_len(nrow(schedule))) {
      source <- match(schedule$reach_segment_id[[interval]], inputs$segments$reach_segment_id)
      distance <- sqrt(sum((coordinates[target, ] - coordinates[source, ])^2))
      for (endpoint in c("interval_start", "interval_end")) {
        age <- time - as.numeric(units::set_units(schedule[[endpoint]][[interval]], "days"))
        if (age <= 0) next
        ratio <- calc_infinite_aquifer_drawdown_ratio(
          distance = units::set_units(distance, "m"), K = inputs$wells$K,
          D = inputs$wells$D, V = inputs$wells$V, t = units::set_units(age, "days"),
          well_diam = inputs$segments$stream_width[source]
        )
        # The Theis ratio uses positive pumping; injection has the opposite sign.
        sign <- if (endpoint == "interval_start") -1 else 1
        expected <- expected + sign * as.numeric(units::set_units(
          ratio * schedule$injection_rate[[interval]], "m"
        ))
      }
    }
    expect_equal(as.numeric(units::set_units(result$stream_recovery[[row]], "m")),
                 expected, tolerance = 1e-10)
  }
})

test_that("order one integrates edge midpoints on bent segments", {
  inputs <- make_point_injection_inputs()
  segments <- isw:::.prepare_numeric_segments(inputs$segments)
  operator <- isw:::.prepare_stream_response_operator(inputs$observations, segments, 1L)
  expect_s3_class(operator, "isw_line_response_operator")
  expect_equal(operator$weights, rep(c(3 / 7, 4 / 7), 2))
  expect_equal(lengths(operator$segment_columns), c(2L, 2L))

  # Known edge midpoints, evaluated independently and weighted by edge length.
  sources <- matrix(c(15, 0, 30, 20, 75, 0, 90, 20), ncol = 2, byrow = TRUE)
  targets <- sf::st_coordinates(inputs$observations)
  expected <- matrix(0, 2, 2, dimnames = list(NULL, segments$reach_segment_id))
  for (source in seq_len(nrow(sources))) {
    segment <- if (source <= 2L) 1L else 2L
    radius <- c(5, 10)[[segment]]
    distances <- pmax(sqrt(rowSums(sweep(targets, 2, sources[source, ], "-")^2)), radius)
    response <- -expint::expint_E1(distances^2 * 0.15 / (4 * 200 * 10)) / (4 * pi * 200)
    expected[, segment] <- expected[, segment] + c(3 / 7, 4 / 7)[[(source - 1L) %% 2L + 1L]] * response
  }
  actual <- isw:::.evaluate_stream_response_operator(operator, 10, 20, 0.15, 10)
  expect_equal(actual, expected, tolerance = 1e-12)
  point_operator <- isw:::.prepare_stream_response_operator(inputs$observations, segments, 0L)
  point_response <- isw:::.evaluate_stream_response_operator(point_operator, 10, 20, 0.15, 10)
  expect_gt(max(abs(actual - point_response)), 1e-6)
})

test_that("order-one quadrature schedules reproduce endpoint heads and reuse metadata", {
  inputs <- make_point_injection_inputs()
  schedule <- generate_stream_injection_schedule(
    inputs$wells, inputs$schedules, inputs$segments, inputs$times,
    method = "constant_head", quadrature_order = 1L
  )
  expect_identical(attr(schedule, "isw_schedule_metadata")$quadrature_order, 1L)
  expect_lt(max(abs(as.numeric(schedule$boundary_residual))), 1e-10)
  reused <- model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments, inputs$times,
    stream_injection_schedule = schedule
  )
  internal <- model_aquifer_water_level_change(
    inputs$wells, inputs$schedules, inputs$observations, inputs$segments, inputs$times,
    injection_method = "constant_head", quadrature_order = 1L
  )
  expect_equal(reused, internal)
  expect_lt(max(abs(as.numeric(reused$water_level_change))), 1e-10)
})
