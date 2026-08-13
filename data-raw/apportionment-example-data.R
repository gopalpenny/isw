# Generate the synthetic spatial inputs used throughout the stream-depletion
# apportionment documentation. Run this script from the isw package root when
# the example inputs need to be updated.

devtools::load_all()

example_pumping_wells <- sf::st_as_sf(
  tibble::tibble(
    pump_id = c("pump_1", "pump_2"),
    x = c(499800, 500450),
    y = c(4980050, 4980150),
    K = units::set_units(c(1e-5, 7.5e-6), "m/s"),
    D = units::set_units(c(50, 40), "m"),
    V = c(0.10, 0.12)
  ),
  coords = c("x", "y"),
  crs = 32615
)

example_stream_reaches <- sf::st_sf(
  reach_id = c("upstream_1", "upstream_2", "downstream"),
  geometry = sf::st_sfc(
    sf::st_linestring(
      matrix(c(500000, 4980200, 500150, 4980050), ncol = 2, byrow = TRUE)
    ),
    sf::st_linestring(
      matrix(c(500000, 4979900, 500150, 4980050), ncol = 2, byrow = TRUE)
    ),
    sf::st_linestring(
      matrix(c(500150, 4980050, 500350, 4979850), ncol = 2, byrow = TRUE)
    ),
    crs = 32615
  )
)

example_observation_wells <- sf::st_as_sf(
  tibble::tibble(
    observation_id = c("obs_near", "obs_downstream"),
    x = c(499900, 500240),
    y = c(4980100, 4979930)
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
