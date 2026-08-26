make_adf_test_inputs <- function(two_pumps = FALSE) {
  if (two_pumps) {
    pumping_wells <- sf::st_as_sf(
      tibble::tibble(
        pump_id = c("pump_1", "pump_2"),
        x = c(0, 50),
        y = c(0, 0),
        K = units::set_units(c(10, 12), "m/day"),
        D = units::set_units(c(20, 25), "m"),
        V = c(0.15, 0.20)
      ),
      coords = c("x", "y"),
      crs = 32615
    )
  } else {
    pumping_wells <- sf::st_as_sf(
      tibble::tibble(
        pump_id = "pump_1",
        x = 0,
        y = 0,
        K = units::set_units(10, "m/day"),
        D = units::set_units(20, "m"),
        V = 0.15
      ),
      coords = c("x", "y"),
      crs = 32615
    )
  }

  stream_reaches <- sf::st_sf(
    reach_id = c("short", "long"),
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(c(100, -50, 100, 50), ncol = 2, byrow = TRUE)
      ),
      sf::st_linestring(
        matrix(c(-100, -100, -100, 100), ncol = 2, byrow = TRUE)
      ),
      crs = 32615
    )
  )

  list(
    pumping_wells = pumping_wells,
    stream_reaches = stream_reaches
  )
}

test_that("reach segments are sampled at along-line interval centers", {
  stream_reaches <- sf::st_sf(
    reach_id = "reach_1",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
      crs = 32615
    )
  )
  reach_segments <- isw:::.discretize_stream_reaches(
    stream_reaches,
    units::set_units(100, "m")
  )

  sample_points <- generate_segment_sample_points(
    reach_segments,
    units::set_units(30, "m")
  )

  expect_s3_class(sample_points, "sf")
  expect_equal(nrow(sample_points), 4)
  expect_identical(
    sample_points$sample_point_id,
    paste0("reach_1_segment_1_point_", 1:4)
  )
  expect_equal(
    sample_points$sampled_length,
    units::set_units(rep(25, 4), "m")
  )
  expect_equal(
    sf::st_coordinates(sample_points)[, "X"],
    c(12.5, 37.5, 62.5, 87.5)
  )
  expect_equal(
    sum(sample_points$sampled_length),
    reach_segments$represented_length
  )
})

test_that("sampling retains bent reach geometry", {
  stream_reaches <- sf::st_sf(
    reach_id = "reach_1",
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(c(0, 0, 100, 0, 100, 100), ncol = 2, byrow = TRUE)
      ),
      crs = 32615
    )
  )
  reach_segments <- isw:::.discretize_stream_reaches(
    stream_reaches,
    units::set_units(250, "m")
  )
  sample_points <- generate_segment_sample_points(
    reach_segments,
    units::set_units(50, "m")
  )

  expect_equal(
    unname(sf::st_coordinates(sample_points)),
    matrix(c(25, 0, 75, 0, 100, 25, 100, 75), ncol = 2, byrow = TRUE)
  )
})

test_that("web apportionment is length weighted by reach segment", {
  inputs <- make_adf_test_inputs()

  stream_apportionment <- get_stream_reach_apportionment(
    inputs$pumping_wells,
    inputs$stream_reaches,
    reach_spacing = units::set_units(500, "m"),
    sample_spacing = units::set_units(500, "m"),
    method = "web",
    analysis_crs = 32615
  )

  expect_s3_class(stream_apportionment, "sf")
  expect_identical(
    stream_apportionment$reach_segment_id,
    c("short_segment_1", "long_segment_1")
  )
  expect_equal(
    stream_apportionment$pump_to_reach_distance,
    units::set_units(c(100, 100), "m")
  )
  expect_equal(
    stream_apportionment$apportionment_fraction,
    c(1 / 3, 2 / 3)
  )
  expect_equal(sum(stream_apportionment$apportionment_fraction), 1)
})

test_that("preferred ADF apportionment accepts prepared stream segments", {
  inputs <- make_adf_test_inputs()
  stream_segments <- get_stream_segments(
    inputs$stream_reaches,
    units::set_units(500, "m")
  )

  result <- get_adf_stream_apportionment(
    inputs$pumping_wells,
    stream_segments,
    sample_spacing = units::set_units(500, "m"),
    method = "web"
  )

  expect_equal(result$apportionment_fraction, c(1 / 3, 2 / 3))
  expect_equal(result$well_diam, result$represented_length / 2)
  expect_identical(sf::st_crs(result), sf::st_crs(stream_segments))
})

test_that("apportionment fractions sum to one for every pump", {
  inputs <- make_adf_test_inputs(two_pumps = TRUE)

  stream_apportionment <- get_stream_reach_apportionment(
    inputs$pumping_wells,
    inputs$stream_reaches,
    reach_spacing = units::set_units(75, "m"),
    sample_spacing = units::set_units(20, "m"),
    analysis_crs = 32615
  )

  fraction_sums <- vapply(
    inputs$pumping_wells$pump_id,
    function(pump_id) {
      sum(stream_apportionment$apportionment_fraction[
        stream_apportionment$pump_id == pump_id
      ])
    },
    numeric(1)
  )

  expect_equal(fraction_sums, c(pump_1 = 1, pump_2 = 1))
  expect_equal(
    as.integer(table(stream_apportionment$pump_id)),
    c(5, 5)
  )
})

test_that("maximum distance excludes remote sample points", {
  inputs <- make_adf_test_inputs()

  stream_apportionment <- get_stream_reach_apportionment(
    inputs$pumping_wells,
    inputs$stream_reaches,
    reach_spacing = units::set_units(500, "m"),
    sample_spacing = units::set_units(500, "m"),
    maximum_distance = units::set_units(110, "m"),
    analysis_crs = 32615
  )

  expect_equal(sum(stream_apportionment$apportionment_fraction), 1)

  expect_error(
    get_stream_reach_apportionment(
      inputs$pumping_wells,
      inputs$stream_reaches,
      reach_spacing = units::set_units(500, "m"),
      sample_spacing = units::set_units(500, "m"),
      maximum_distance = units::set_units(50, "m"),
      analysis_crs = 32615
    ),
    "No stream sample points"
  )
})

test_that("zero-distance sample points receive all apportionment", {
  inputs <- make_adf_test_inputs()
  sf::st_geometry(inputs$pumping_wells) <- sf::st_sfc(
    sf::st_point(c(100, 0)),
    crs = 32615
  )

  stream_apportionment <- get_stream_reach_apportionment(
    inputs$pumping_wells,
    inputs$stream_reaches,
    reach_spacing = units::set_units(500, "m"),
    sample_spacing = units::set_units(500, "m"),
    analysis_crs = 32615
  )

  expect_equal(
    stream_apportionment$apportionment_fraction,
    c(1, 0)
  )
})

test_that("fraction lookup evaluates unique elapsed times once per segment", {
  inputs <- make_adf_test_inputs()
  stream_apportionment <- get_stream_reach_apportionment(
    inputs$pumping_wells,
    inputs$stream_reaches,
    reach_spacing = units::set_units(500, "m"),
    sample_spacing = units::set_units(500, "m"),
    analysis_crs = 32615
  )
  pumping_response_times <- tibble::tibble(
    pump_id = rep("pump_1", 3),
    evaluation_time = as.Date(c("2025-02-01", "2025-03-01", "2025-04-01")),
    pumping_time = units::set_units(c(0, 31, 61), "days"),
    elapsed_time = units::set_units(c(30, 30, 60), "days"),
    pumping_rate_change = units::set_units(c(100, -20, 10), "m^3/day")
  )

  fraction_lookup <- isw:::.get_stream_depletion_fraction_lookup(
    inputs$pumping_wells,
    pumping_response_times,
    stream_apportionment
  )

  expect_equal(nrow(fraction_lookup), 4)
  expect_equal(
    sort(unique(as.numeric(fraction_lookup$elapsed_time))),
    c(30, 60)
  )
})

test_that("intermittent depletion uses elapsed-time fractions and superposition", {
  inputs <- make_adf_test_inputs()
  stream_apportionment <- get_stream_reach_apportionment(
    inputs$pumping_wells,
    inputs$stream_reaches,
    reach_spacing = units::set_units(500, "m"),
    sample_spacing = units::set_units(500, "m"),
    method = "web",
    analysis_crs = 32615
  )
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 10), "days"),
    pump_1 = units::set_units(c(100, 0), "m^3/day")
  )
  evaluation_times <- units::set_units(c(0, 10, 20), "days")

  stream_depletion <- get_apportioned_stream_depletion(
    inputs$pumping_wells,
    pumping_schedules,
    stream_apportionment,
    evaluation_times
  )

  segment_distances <- stream_apportionment$pump_to_reach_distance
  fractions_at_10 <- isw:::.glover_stream_depletion_fraction(
    segment_distances,
    inputs$pumping_wells$K,
    inputs$pumping_wells$D,
    inputs$pumping_wells$V,
    units::set_units(10, "days")
  )
  fractions_at_20 <- isw:::.glover_stream_depletion_fraction(
    segment_distances,
    inputs$pumping_wells$K,
    inputs$pumping_wells$D,
    inputs$pumping_wells$V,
    units::set_units(20, "days")
  )
  apportionment_fractions <-
    stream_apportionment$apportionment_fraction

  expect_equal(
    stream_depletion$stream_depletion_rate[1:2],
    units::set_units(c(0, 0), "m^3/day")
  )
  expect_equal(
    stream_depletion$stream_depletion_rate[3:4],
    units::set_units(
      100 * fractions_at_10 * apportionment_fractions,
      "m^3/day"
    )
  )
  expect_equal(
    stream_depletion$stream_depletion_rate[5:6],
    units::set_units(
      100 * (fractions_at_20 - fractions_at_10) *
        apportionment_fractions,
      "m^3/day"
    )
  )
})

test_that("preferred ADF depletion name preserves existing results", {
  inputs <- make_adf_test_inputs()
  stream_apportionment <- get_stream_reach_apportionment(
    inputs$pumping_wells,
    inputs$stream_reaches,
    reach_spacing = units::set_units(500, "m"),
    sample_spacing = units::set_units(500, "m"),
    analysis_crs = 32615
  )
  pumping_schedules <- tibble::tibble(
    t = units::set_units(c(0, 10), "days"),
    pump_1 = units::set_units(c(100, 0), "m^3/day")
  )
  evaluation_times <- units::set_units(c(0, 10, 20), "days")

  expect_equal(
    get_adf_stream_depletion(
      inputs$pumping_wells,
      pumping_schedules,
      stream_apportionment,
      evaluation_times
    ),
    get_apportioned_stream_depletion(
      inputs$pumping_wells,
      pumping_schedules,
      stream_apportionment,
      evaluation_times
    )
  )
})

test_that("sampling and apportionment inputs are validated", {
  inputs <- make_adf_test_inputs()
  reach_segments <- isw:::.discretize_stream_reaches(
    inputs$stream_reaches,
    units::set_units(500, "m")
  )

  expect_error(
    generate_segment_sample_points(reach_segments, 25),
    "not a units object"
  )
  expect_error(
    generate_segment_sample_points(
      reach_segments,
      units::set_units(0, "m")
    ),
    "finite, positive"
  )
  expect_error(
    get_stream_reach_apportionment(
      inputs$pumping_wells,
      inputs$stream_reaches,
      units::set_units(500, "m"),
      units::set_units(20, "m"),
      method = "unsupported",
      analysis_crs = 32615
    ),
    "should be one of"
  )
})
