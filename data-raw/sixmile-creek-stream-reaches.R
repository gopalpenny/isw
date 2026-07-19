# Generate the Sixmile Creek example stream network from the USGS 3DHP
# service. Run this script from the isw package root when the packaged dataset
# needs to be refreshed.

devtools::load_all()

sixmile_example_well <- sf::st_sf(
  pump_id = "pump_1",
  geometry = sf::st_sfc(
    sf::st_point(c(295500, 4783200)),
    crs = 26916
  )
)

downloaded_stream_reaches <- get_usgs_stream_reaches(
  aoi = sixmile_example_well,
  buffer_distance = units::set_units(10, "km")
)

sixmile_creek_stream_reaches <- downloaded_stream_reaches[
  downloaded_stream_reaches$gnisidlabel %in%
    c("Sixmile Creek", "Dorn Creek"),
  c("reach_id", "gnisidlabel", "geometry")
]

names(sixmile_creek_stream_reaches)[
  names(sixmile_creek_stream_reaches) == "gnisidlabel"
] <- "stream_name"

rownames(sixmile_creek_stream_reaches) <- NULL

.validate_stream_reaches(sixmile_creek_stream_reaches)

usethis::use_data(
  sixmile_creek_stream_reaches,
  overwrite = TRUE,
  compress = "xz"
)
