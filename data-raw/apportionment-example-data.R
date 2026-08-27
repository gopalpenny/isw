# Generate the synthetic spatial inputs used throughout the stream-depletion
# apportionment documentation. Run this script from the isw package root when
# the example inputs need to be updated.

devtools::load_all()

example_pumping_wells <- sf::st_as_sf(
  tibble::tibble(
    pump_id = c("pump_1", "pump_2"),
    x = c(499006, 500350),
    y = c(4980000, 4980120),
    K = units::set_units(c(2e-5, 1.2e-5), "m/s"),
    D = units::set_units(c(110, 90), "m"),
    V = c(0.10, 0.12),
    well_diam = units::set_units(c(0.3, 0.3), "m")
  ),
  coords = c("x", "y"),
  crs = 32615
)

example_stream_reaches <- sf::st_sf(
  reach_id = c("upstream_1", "upstream_2", "downstream"),
  geometry = sf::st_sfc(
    sf::st_linestring(
      matrix(c(499500, 4980500, 500000, 4980000), ncol = 2, byrow = TRUE)
    ),
    sf::st_linestring(
      matrix(c(499500, 4979500, 500000, 4980000), ncol = 2, byrow = TRUE)
    ),
    sf::st_linestring(
      matrix(c(500000, 4980000, 501080, 4979640), ncol = 2, byrow = TRUE)
    ),
    crs = 32615
  )
)

example_observation_wells <- sf::st_as_sf(
  tibble::tibble(
    observation_id = c("obs_near", "obs_downstream"),
    x = c(499150, 500208),
    y = c(4980400, 4979694)
  ),
  coords = c("x", "y"),
  crs = 32615
)

.validate_pumping_wells(example_pumping_wells)
.validate_stream_reaches(example_stream_reaches)
.validate_observation_wells(example_observation_wells)

usethis::use_data(
  example_pumping_wells,
  overwrite = TRUE,
  compress = "xz"
)

usethis::use_data(
  example_stream_reaches,
  overwrite = TRUE,
  compress = "xz"
)

usethis::use_data(
  example_observation_wells,
  overwrite = TRUE,
  compress = "xz"
)
