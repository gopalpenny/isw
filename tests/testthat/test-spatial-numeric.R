make_spatial_numeric_reach <- function() {
  # Realistic UTM meter geometry permits meaningful transformations to feet and other CRSs.
  sf::st_sf(reach_id = "reach", geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(500000, 4100000, 500100, 4100000, 500100, 4100050),
                             ncol = 2, byrow = TRUE)), crs = 32610
  ))
}

test_that("non-UTM and feet-based inputs produce equivalent shared-UTM segments", {
  reach <- make_spatial_numeric_reach()
  feet_crs <- sf::st_crs("+proj=utm +zone=10 +datum=WGS84 +units=us-ft +no_defs")
  reference <- prep_stream_segments(reach, units::set_units(60, "m"))
  for (input_crs in list(feet_crs, sf::st_crs(3857), sf::st_crs(4326))) {
    result <- prep_stream_segments(sf::st_transform(reach, input_crs),
                                  units::set_units(60 / 0.3048, "ft"))
    expect_identical(sf::st_crs(result), sf::st_crs(reference))
    expect_equal(nrow(result), nrow(reference))
    expect_lt(max(as.numeric(sf::st_distance(result, reference, by_element = TRUE,
                                            which = "Hausdorff"))), 1e-6)
    expect_equal(result$represented_length, reference$represented_length, tolerance = 1e-7)
  }
  expect_error(prep_stream_segments(reach, units::set_units(60, "m"), analysis_crs = 3857),
               "UTM CRS in meters")
  expect_error(prep_stream_segments(reach, units::set_units(60, "m"), analysis_crs = feet_crs),
               "UTM CRS in meters")
})

test_that("secondary geometry transforms with segment lines and numeric measurements", {
  segments <- prep_stream_segments(make_spatial_numeric_reach(), units::set_units(60, "m"))
  feet_crs <- sf::st_crs("+proj=utm +zone=10 +datum=WGS84 +units=us-ft +no_defs")
  feet_segments <- isw:::.transform_spatial_geometry(segments, feet_crs)
  original <- feet_segments
  prepared <- isw:::.prepare_numeric_segments(feet_segments)
  expect_identical(sf::st_crs(prepared), sf::st_crs(32610))
  expect_identical(sf::st_crs(prepared$model_point), sf::st_crs(prepared))
  expect_lt(max(abs(sf::st_coordinates(prepared$model_point) -
                      sf::st_coordinates(segments$model_point))), 1e-6)
  expect_equal(prepared$represented_length_m, as.numeric(segments$represented_length), tolerance = 1e-7)
  expect_false(inherits(prepared$represented_length_m, "units"))
  expect_identical(feet_segments, original)
})

test_that("zone crossings are quiet and downstream preparation reuses the network CRS", {
  reach <- sf::st_sf(reach_id = "boundary", geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(-90.001, 45, -89.999, 45), ncol = 2, byrow = TRUE)), crs = 4326
  ))
  expect_silent(segments <- prep_stream_segments(reach, units::set_units(1000, "m")))
  expect_identical(sf::st_crs(segments)$epsg, 32616L)
  wells <- sf::st_sf(
    pump_id = "pump", K = units::set_units(10, "m/day"), D = units::set_units(20, "m"), V = 0.15,
    geometry = sf::st_sfc(sf::st_point(c(-90.002, 45.001)), crs = 4326)
  )
  wells <- sf::st_transform(wells, 32615)
  before <- wells
  apportioned <- prep_adf_stream_apportionment(wells, segments, units::set_units(50, "m"))
  expected_distance <- sf::st_distance(sf::st_transform(wells, sf::st_crs(segments)), segments)[1, ]
  expect_equal(apportioned$pump_to_reach_distance, expected_distance)
  expect_equal(sum(apportioned$apportionment_fraction), 1)
  expect_identical(wells, before)
  expect_identical(sf::st_crs(segments)$epsg, 32616L)
})

test_that("user-facing sampling and apportionment retain equivalent physical input units", {
  reach <- make_spatial_numeric_reach()
  meters <- prep_stream_segments(reach, units::set_units(60, "m"), stream_width = units::set_units(1, "m"))
  feet <- prep_stream_segments(reach, units::set_units(60 / 0.3048, "ft"),
                               stream_width = units::set_units(1 / 0.3048, "ft"))
  meter_samples <- prep_stream_sample_points(meters, units::set_units(20, "m"))
  feet_samples <- prep_stream_sample_points(feet, units::set_units(20 / 0.3048, "ft"))
  expect_equal(meter_samples, feet_samples)
  wells <- sf::st_sf(pump_id = "pump", K = units::set_units(10, "m/day"),
                     D = units::set_units(20, "m"), V = 0.15,
                     geometry = sf::st_sfc(sf::st_point(c(500050, 4100100)), crs = 32610))
  expect_equal(prep_adf_stream_apportionment(wells, meters, units::set_units(20, "m")),
               prep_adf_stream_apportionment(sf::st_transform(wells, 4326), feet,
                                              units::set_units(20 / 0.3048, "ft")), tolerance = 1e-8)
})

test_that("response operators retain numeric meter distances and sf source geometry", {
  segments <- prep_stream_segments(make_spatial_numeric_reach(), units::set_units(60, "m"))
  elements <- isw:::.prepare_line_elements(isw:::.prepare_numeric_segments(segments), 8L)
  targets <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(500050, 4100100)), crs = 32610))
  operator <- isw:::.prepare_line_response_operator(sf::st_transform(targets, 4326), elements)
  expect_s3_class(elements$quadrature_points, "sf")
  expect_identical(sf::st_crs(elements$quadrature_points), sf::st_crs(segments))
  expect_type(operator$distances_m, "double")
  expect_false(inherits(operator$distances_m, "units"))
  expect_equal(operator$distances_m, as.matrix(units::drop_units(
    sf::st_distance(targets, elements$quadrature_points))))
  before <- operator
  local_mocked_bindings(
    check_dimensionality = function(...) stop("unexpected repeated unit check"),
    .transform_spatial_geometry = function(...) stop("unexpected repeated spatial preparation"),
    .package = "isw"
  )
  expect_silent(early_day_per_m2 <- isw:::.evaluate_line_response_operator(operator, 10, 20, 0.15, 1))
  expect_silent(late_day_per_m2 <- isw:::.evaluate_line_response_operator(operator, 10, 20, 0.15, 10))
  expect_false(isTRUE(all.equal(early_day_per_m2, late_day_per_m2)))
  expect_identical(operator, before)
})
