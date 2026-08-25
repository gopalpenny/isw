test_that("packaged apportionment example inputs are valid", {
  data("example_pumping_wells", package = "isw")
  data("example_stream_reaches", package = "isw")
  data("example_observation_wells", package = "isw")

  expect_s3_class(example_pumping_wells, "sf")
  expect_s3_class(example_stream_reaches, "sf")
  expect_s3_class(example_observation_wells, "sf")

  expect_identical(nrow(example_pumping_wells), 2L)
  expect_identical(nrow(example_stream_reaches), 3L)
  expect_identical(nrow(example_observation_wells), 2L)

  expect_equal(
    example_pumping_wells$well_diam,
    units::set_units(c(0.3, 0.3), "m")
  )

  expect_silent(isw:::.validate_pumping_wells(example_pumping_wells))
  expect_silent(isw:::.validate_stream_reaches(example_stream_reaches))
  expect_silent(isw:::.validate_observation_wells(example_observation_wells))

  expect_identical(
    sf::st_crs(example_pumping_wells),
    sf::st_crs(example_stream_reaches)
  )
  expect_identical(
    sf::st_crs(example_pumping_wells),
    sf::st_crs(example_observation_wells)
  )

  stream_extent <- sf::st_bbox(example_stream_reaches)
  expect_equal(unname(stream_extent["xmax"] - stream_extent["xmin"]), 1580)
  expect_equal(unname(stream_extent["ymax"] - stream_extent["ymin"]), 1000)

  pumping_distances <- apply(
    sf::st_distance(example_pumping_wells, example_stream_reaches),
    1,
    min
  )
  expect_equal(as.numeric(pumping_distances[1]), 994 / sqrt(2))
  expect_equal(as.numeric(pumping_distances[2]), 710 / sqrt(10))

  observation_distances <- apply(
    sf::st_distance(example_observation_wells, example_stream_reaches),
    1,
    min
  )
  expect_equal(as.numeric(observation_distances[1]), sqrt(132500))
  expect_equal(as.numeric(observation_distances[2]), 710 / sqrt(10))

  pump_2_coordinates <- sf::st_coordinates(example_pumping_wells[2, ])
  downstream_observation_coordinates <- sf::st_coordinates(
    example_observation_wells[2, ]
  )
  expect_equal(
    unname((pump_2_coordinates + downstream_observation_coordinates) / 2),
    matrix(c(500279, 4979907), nrow = 1)
  )

  reach_segments <- isw:::.discretize_stream_reaches(
    example_stream_reaches,
    units::set_units(400, "m")
  )
  expect_equal(sum(reach_segments$reach_id == "downstream"), 3)
})
