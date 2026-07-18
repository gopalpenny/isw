make_spatial_test_inputs <- function(longitude = -93.25, latitude = 44.95) {
  pumping_wells <- sf::st_as_sf(
    tibble::tibble(
      pump_id = "pump_1",
      x = longitude,
      y = latitude,
      K = units::set_units(10, "m/day"),
      D = units::set_units(20, "m"),
      V = 0.15
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  stream_reaches <- sf::st_sf(
    reach_id = "reach_1",
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(
          c(
            longitude - 0.05, latitude - 0.05,
            longitude + 0.05, latitude + 0.05
          ),
          ncol = 2,
          byrow = TRUE
        )
      ),
      crs = 4326
    )
  )

  list(
    pumping_wells = pumping_wells,
    stream_reaches = stream_reaches
  )
}

test_that("a northern UTM CRS is selected from the combined extent", {
  inputs <- make_spatial_test_inputs()

  selected_crs <- isw:::.select_analysis_crs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_s3_class(selected_crs, "crs")
  expect_identical(selected_crs$epsg, 32615L)
})

test_that("a southern UTM CRS is selected from the combined extent", {
  inputs <- make_spatial_test_inputs(longitude = 151.2, latitude = -33.9)

  selected_crs <- isw:::.select_analysis_crs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_identical(selected_crs$epsg, 32756L)
})

test_that("inputs may use different coordinate reference systems", {
  inputs <- make_spatial_test_inputs()
  inputs$stream_reaches <- sf::st_transform(inputs$stream_reaches, 3857)

  selected_crs <- isw:::.select_analysis_crs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_identical(selected_crs$epsg, 32615L)
})

test_that("observation wells are included when selecting the CRS", {
  inputs <- make_spatial_test_inputs()
  observation_wells <- sf::st_as_sf(
    tibble::tibble(
      observation_id = "obs_1",
      x = -93.15,
      y = 45.05
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  selected_crs <- isw:::.select_analysis_crs(
    inputs$pumping_wells,
    inputs$stream_reaches,
    observation_wells
  )

  expect_identical(selected_crs$epsg, 32615L)
})

test_that("a user-supplied projected CRS is returned", {
  inputs <- make_spatial_test_inputs()

  selected_crs <- isw:::.select_analysis_crs(
    inputs$pumping_wells,
    inputs$stream_reaches,
    analysis_crs = 26915
  )

  expect_identical(selected_crs, sf::st_crs(26915))
})

test_that("a user-supplied CRS must be valid and projected", {
  inputs <- make_spatial_test_inputs()

  expect_error(
    isw:::.select_analysis_crs(
      inputs$pumping_wells,
      inputs$stream_reaches,
      analysis_crs = "not-a-crs"
    ),
    "valid projected CRS"
  )

  expect_error(
    isw:::.select_analysis_crs(
      inputs$pumping_wells,
      inputs$stream_reaches,
      analysis_crs = 4326
    ),
    "must define a projected CRS"
  )

  expect_error(
    isw:::.select_analysis_crs(
      inputs$pumping_wells,
      inputs$stream_reaches,
      analysis_crs = 4978
    ),
    "must define a projected CRS"
  )
})

test_that("automatic selection warns when inputs span UTM zones", {
  inputs <- make_spatial_test_inputs(longitude = -90, latitude = 44.95)
  sf::st_geometry(inputs$stream_reaches) <- sf::st_sfc(
    sf::st_linestring(
      matrix(c(-90.1, 44.9, -89.9, 45.0), ncol = 2, byrow = TRUE)
    ),
    crs = 4326
  )

  expect_warning(
    isw:::.select_analysis_crs(
      inputs$pumping_wells,
      inputs$stream_reaches
    ),
    "span multiple UTM zones"
  )
})

test_that("automatic selection is limited to UTM coverage", {
  inputs <- make_spatial_test_inputs(longitude = 0, latitude = 85)

  expect_error(
    isw:::.select_analysis_crs(
      inputs$pumping_wells,
      inputs$stream_reaches
    ),
    "between 80 degrees south and 84 degrees north"
  )
})

test_that("CRS selection does not modify spatial inputs", {
  inputs <- make_spatial_test_inputs()
  original_pumping_wells <- inputs$pumping_wells
  original_stream_reaches <- inputs$stream_reaches

  isw:::.select_analysis_crs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_identical(inputs$pumping_wells, original_pumping_wells)
  expect_identical(inputs$stream_reaches, original_stream_reaches)
})

test_that("spatial inputs are transformed to the selected CRS", {
  inputs <- make_spatial_test_inputs()
  inputs$stream_reaches <- sf::st_transform(inputs$stream_reaches, 3857)
  observation_wells <- sf::st_as_sf(
    tibble::tibble(
      observation_id = "obs_1",
      x = -93.15,
      y = 45.05
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  expect_silent(
    prepared_inputs <- isw:::.prepare_spatial_inputs(
      inputs$pumping_wells,
      inputs$stream_reaches,
      observation_wells
    )
  )

  expect_identical(prepared_inputs$analysis_crs$epsg, 32615L)
  expect_identical(
    sf::st_crs(prepared_inputs$pumping_wells),
    prepared_inputs$analysis_crs
  )
  expect_identical(
    sf::st_crs(prepared_inputs$stream_reaches),
    prepared_inputs$analysis_crs
  )
  expect_identical(
    sf::st_crs(prepared_inputs$observation_wells),
    prepared_inputs$analysis_crs
  )
})

test_that("a supplied analysis CRS is used for every spatial input", {
  inputs <- make_spatial_test_inputs()

  prepared_inputs <- isw:::.prepare_spatial_inputs(
    inputs$pumping_wells,
    inputs$stream_reaches,
    analysis_crs = 26915
  )

  expect_identical(prepared_inputs$analysis_crs, sf::st_crs(26915))
  expect_identical(
    sf::st_crs(prepared_inputs$pumping_wells),
    sf::st_crs(26915)
  )
  expect_identical(
    sf::st_crs(prepared_inputs$stream_reaches),
    sf::st_crs(26915)
  )
})

test_that("missing well diameter defaults to zero meters", {
  inputs <- make_spatial_test_inputs()

  prepared_inputs <- isw:::.prepare_spatial_inputs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_equal(
    prepared_inputs$pumping_wells$well_diam,
    units::set_units(0, "m")
  )
})

test_that("an existing well diameter and its units are preserved", {
  inputs <- make_spatial_test_inputs()
  inputs$pumping_wells$well_diam <- units::set_units(1, "ft")

  prepared_inputs <- isw:::.prepare_spatial_inputs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_identical(
    prepared_inputs$pumping_wells$well_diam,
    inputs$pumping_wells$well_diam
  )
})

test_that("Z and M coordinates are dropped from prepared geometries", {
  inputs <- make_spatial_test_inputs()
  sf::st_geometry(inputs$pumping_wells) <- sf::st_sfc(
    sf::st_point(c(-93.25, 44.95, 250), dim = "XYZ"),
    crs = 4326
  )
  sf::st_geometry(inputs$stream_reaches) <- sf::st_sfc(
    sf::st_linestring(
      matrix(
        c(-93.30, 44.90, 250, -93.20, 45.00, 260),
        ncol = 3,
        byrow = TRUE
      ),
      dim = "XYZ"
    ),
    crs = 4326
  )
  observation_wells <- sf::st_sf(
    observation_id = "obs_1",
    geometry = sf::st_sfc(
      sf::st_point(c(-93.15, 45.05, 10), dim = "XYM"),
      crs = 4326
    )
  )

  expect_silent(
    prepared_inputs <- isw:::.prepare_spatial_inputs(
      inputs$pumping_wells,
      inputs$stream_reaches,
      observation_wells
    )
  )

  pumping_coordinates <- sf::st_coordinates(
    prepared_inputs$pumping_wells
  )
  stream_coordinates <- sf::st_coordinates(
    prepared_inputs$stream_reaches
  )
  observation_coordinates <- sf::st_coordinates(
    prepared_inputs$observation_wells
  )

  expect_false(any(c("Z", "M") %in% colnames(pumping_coordinates)))
  expect_false(any(c("Z", "M") %in% colnames(stream_coordinates)))
  expect_false(any(c("Z", "M") %in% colnames(observation_coordinates)))
})

test_that("NULL observation wells remain NULL", {
  inputs <- make_spatial_test_inputs()

  prepared_inputs <- isw:::.prepare_spatial_inputs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_null(prepared_inputs$observation_wells)
})

test_that("spatial preparation does not modify input objects", {
  inputs <- make_spatial_test_inputs()
  original_pumping_wells <- inputs$pumping_wells
  original_stream_reaches <- inputs$stream_reaches

  isw:::.prepare_spatial_inputs(
    inputs$pumping_wells,
    inputs$stream_reaches
  )

  expect_identical(inputs$pumping_wells, original_pumping_wells)
  expect_identical(inputs$stream_reaches, original_stream_reaches)
})
