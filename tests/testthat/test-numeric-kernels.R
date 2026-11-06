test_that("user-facing analytical functions are invariant to compatible input units", {
  distance <- units::set_units(c(10, 100, 1000), "m")
  K <- units::set_units(c(5, 10, 20), "m/day")
  D <- units::set_units(20, "m")
  t <- units::set_units(c(1, 10, 100), "day")
  well_diam <- units::set_units(0.2, "m")
  convert <- function(x, unit) units::set_units(x, unit, mode = "standard")

  metric <- calc_infinite_aquifer_drawdown_ratio(distance, K, D, 0.15, t, well_diam)
  imperial <- calc_infinite_aquifer_drawdown_ratio(
    convert(distance, "ft"), convert(K, "ft/s"), convert(D, "ft"),
    0.15, convert(t, "hour"), convert(well_diam, "inch")
  )
  expect_equal(metric, convert(imperial, "day/m^2"), tolerance = 1e-12)
  expect_identical(units::deparse_unit(imperial), "s ft-2")
  expect_equal(
    calc_straight_stream_depletion_fraction(distance, K, D, 0.15, t),
    calc_straight_stream_depletion_fraction(
      convert(distance, "ft"), convert(K, "ft/s"), convert(D, "ft"),
      0.15, convert(t, "hour")
    ), tolerance = 1e-12
  )
  metric_image <- calc_straight_stream_drawdown_ratio(
    distance, distance / 2, distance / 3, K, D, 0.15, t, well_diam
  )
  imperial_image <- calc_straight_stream_drawdown_ratio(
    convert(distance, "ft"), convert(distance / 2, "ft"),
    convert(distance / 3, "ft"), convert(K, "ft/s"), convert(D, "ft"),
    0.15, convert(t, "hour"), convert(well_diam, "inch")
  )
  expect_equal(metric_image, convert(imperial_image, "day/m^2"), tolerance = 1e-12)
})

test_that("straight-stream user-facing response is the real well minus its image", {
  K <- units::set_units(10, "m/day")
  D <- units::set_units(20, "m")
  t <- units::set_units(10, "day")
  expected <- calc_infinite_aquifer_drawdown_ratio(
    units::set_units(sqrt(60^2 + 30^2), "m"), K, D, 0.15, t
  ) - calc_infinite_aquifer_drawdown_ratio(
    units::set_units(sqrt(140^2 + 30^2), "m"), K, D, 0.15, t
  )
  expect_equal(calc_straight_stream_drawdown_ratio(
    units::set_units(100, "m"), units::set_units(40, "m"),
    units::set_units(30, "m"), K, D, 0.15, t
  ), expected)
  expect_equal(as.numeric(calc_straight_stream_drawdown_ratio(
    units::set_units(100, "m"), units::set_units(0, "m"),
    units::set_units(30, "m"), K, D, 0.15, t
  )), 0)
})

test_that("numeric Theis preserves radius regularization and scalar recycling", {
  response_day_per_m2 <- isw:::.theis_aquifer_drawdown_ratio(
    distance_m = c(0, 0.5, 1, 2), K_m_per_day = 10, D_m = 20,
    V = 0.15, t_day = c(1, 1, 1, 2), well_diam_m = 2
  )
  expect_identical(response_day_per_m2[1:3], rep(response_day_per_m2[1], 3))
  expect_type(response_day_per_m2, "double")
  expect_false(inherits(response_day_per_m2, "units"))
  expect_equal(response_day_per_m2, vapply(seq_len(4), function(i) {
    isw:::.theis_aquifer_drawdown_ratio(c(0, 0.5, 1, 2)[i], 10, 20,
                                       0.15, c(1, 1, 1, 2)[i], 2)
  }, numeric(1)))
})

test_that("numeric finite-line response matches adaptive spatial integration", {
  # Numeric reference inputs use meters/days; the integral is averaged over line length.
  reference_day_per_m2 <- integrate(function(source_x_m) {
    distance_m <- sqrt((20 - source_x_m)^2 + 30^2)
    u <- pmax(distance_m, 2.5)^2 / (4 * (10 * 20 / 0.15) * 10)
    -expint::expint_E1(u) / (4 * pi * 10 * 20)
  }, -50, 50, rel.tol = 1e-12)$value / 100
  response_day_per_m2 <- isw:::.line_sink_aquifer_drawdown_ratio(
    along_distance_m = 20, perpendicular_distance_m = 30, line_length_m = 100,
    K_m_per_day = 10, D_m = 20, V = 0.15, t_day = 10,
    stream_width_m = 5, quadrature_order = 64L
  )
  expect_equal(response_day_per_m2, reference_day_per_m2, tolerance = 1e-10)
  expect_false(inherits(response_day_per_m2, "units"))
})

test_that("numeric finite-line batching agrees with independent scalar evaluations", {
  response_day_per_m2 <- isw:::.line_sink_aquifer_drawdown_ratio(
    c(-20, 0, 20), c(3, 10, 30), 100, c(5, 10, 20), 20, 0.15,
    c(1, 10, 100), 5
  )
  expected_day_per_m2 <- vapply(seq_len(3), function(i) {
    isw:::.line_sink_aquifer_drawdown_ratio(
      c(-20, 0, 20)[i], c(3, 10, 30)[i], 100, c(5, 10, 20)[i], 20,
      0.15, c(1, 10, 100)[i], 5
    )
  }, numeric(1))
  expect_equal(response_day_per_m2, expected_day_per_m2)
})

test_that("numeric kernels never call the dimensionality validator", {
  local_mocked_bindings(check_dimensionality = function(...) {
    stop("unexpected dimensionality check")
  }, .package = "isw")
  expect_silent(isw:::.glover_stream_depletion_fraction(100, 10, 20, 0.15, 10))
  expect_silent(isw:::.theis_aquifer_drawdown_ratio(100, 10, 20, 0.15, 10, 0))
  expect_silent(isw:::.glover_aquifer_drawdown_ratio(100, 40, 30, 10, 20, 0.15, 10, 0))
  expect_silent(isw:::.line_sink_aquifer_drawdown_ratio(20, 30, 100, 10, 20, 0.15, 10, 5))
})
