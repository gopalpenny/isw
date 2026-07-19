make_usgs_test_point <- function() {
  sf::st_sf(
    site = "test",
    geometry = sf::st_sfc(sf::st_point(c(500000, 4900000)), crs = 26915)
  )
}

make_mock_3dhp_flowlines <- function() {
  sf::st_sf(
    id3dhp = c("flowline_1", "flowline_2"),
    gnisidlabel = c("Test Creek", NA_character_),
    streamorder = c(2L, 1L),
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(c(495000, 4900000, 505000, 4900000), ncol = 2, byrow = TRUE)
      ),
      sf::st_linestring(
        matrix(c(500000, 4895000, 500000, 4905000), ncol = 2, byrow = TRUE)
      ),
      crs = 26915
    )
  )
}

test_that("point queries require a length-units buffer", {
  point_aoi <- make_usgs_test_point()

  expect_error(
    get_usgs_stream_reaches(point_aoi),
    "buffer_distance is required"
  )

  expect_error(
    get_usgs_stream_reaches(point_aoi, buffer_distance = 1000),
    "scalar units object"
  )

  expect_error(
    get_usgs_stream_reaches(
      point_aoi,
      buffer_distance = units::set_units(1, "day")
    ),
    "converted to m"
  )

  expect_error(
    get_usgs_stream_reaches(
      point_aoi,
      buffer_distance = units::set_units(0, "m")
    ),
    "greater than zero"
  )
})

test_that("polygon queries cannot also specify a buffer", {
  polygon_aoi <- sf::st_sf(
    geometry = sf::st_as_sfc(
      sf::st_bbox(c(xmin = 499000, ymin = 4899000,
                    xmax = 501000, ymax = 4901000), crs = 26915)
    )
  )

  expect_error(
    get_usgs_stream_reaches(
      polygon_aoi,
      buffer_distance = units::set_units(1, "km")
    ),
    "must be NULL"
  )
})

test_that("point queries return validated and clipped stream reaches", {
  point_aoi <- make_usgs_test_point()
  mock_flowlines <- make_mock_3dhp_flowlines()

  testthat::local_mocked_bindings(
    .fetch_3dhp_flowlines = function(query_area) mock_flowlines,
    .package = "isw"
  )

  result <- get_usgs_stream_reaches(
    point_aoi,
    buffer_distance = units::set_units(1, "km")
  )

  expect_s3_class(result, "sf")
  expect_named(
    result,
    c("reach_id", "gnisidlabel", "streamorder", "geometry")
  )
  expect_identical(result$reach_id, c("flowline_1", "flowline_2"))
  expect_identical(sf::st_crs(result), sf::st_crs(point_aoi))
  expect_true(all(as.numeric(sf::st_length(result)) < 2100))
  expect_silent(isw:::.validate_stream_reaches(result))
})

test_that("USGS attributes can be omitted", {
  point_aoi <- make_usgs_test_point()
  mock_flowlines <- make_mock_3dhp_flowlines()

  testthat::local_mocked_bindings(
    .fetch_3dhp_flowlines = function(query_area) mock_flowlines,
    .package = "isw"
  )

  result <- get_usgs_stream_reaches(
    point_aoi,
    buffer_distance = units::set_units(1, "km"),
    clip = FALSE,
    keep_attributes = FALSE
  )

  expect_named(result, c("reach_id", "geometry"))
  expect_equal(as.numeric(sf::st_length(result)), c(10000, 10000))
})

test_that("USGS service responses must contain unique 3DHP identifiers", {
  point_aoi <- make_usgs_test_point()
  mock_flowlines <- make_mock_3dhp_flowlines()
  mock_flowlines$id3dhp <- rep("duplicate", nrow(mock_flowlines))

  testthat::local_mocked_bindings(
    .fetch_3dhp_flowlines = function(query_area) mock_flowlines,
    .package = "isw"
  )

  expect_error(
    get_usgs_stream_reaches(
      point_aoi,
      buffer_distance = units::set_units(1, "km"),
      clip = FALSE
    ),
    "duplicate id3dhp"
  )
})

test_that("only the current 3DHP source is accepted", {
  expect_error(
    get_usgs_stream_reaches(
      make_usgs_test_point(),
      buffer_distance = units::set_units(1, "km"),
      source = "nhdplus_hr"
    ),
    'currently be "3dhp"'
  )
})

test_that("the packaged Sixmile Creek network is a valid stream input", {
  data("sixmile_creek_stream_reaches", package = "isw")

  expect_s3_class(sixmile_creek_stream_reaches, "sf")
  expect_named(
    sixmile_creek_stream_reaches,
    c("reach_id", "stream_name", "geometry")
  )
  expect_setequal(
    unique(sixmile_creek_stream_reaches$stream_name),
    c("Sixmile Creek", "Dorn Creek")
  )
  expect_silent(
    isw:::.validate_stream_reaches(sixmile_creek_stream_reaches)
  )
})
