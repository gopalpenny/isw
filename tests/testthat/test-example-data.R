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
})
