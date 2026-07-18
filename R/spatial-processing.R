#' Select the projected CRS used for spatial analysis
#'
#' Select a local UTM coordinate reference system from the spatial extent of
#' the model inputs, or validate a projected CRS supplied by the user.
#'
#' @param pumping_wells A pumping-well object accepted by
#'   [`.validate_pumping_wells()`].
#' @param stream_reaches A stream-reach object accepted by
#'   [`.validate_stream_reaches()`].
#' @param observation_wells Either `NULL` or an observation-well object
#'   accepted by [`.validate_observation_wells()`].
#' @param analysis_crs Either `NULL` or a coordinate reference system accepted
#'   by [sf::st_crs()]. The supplied CRS must be projected. When `NULL`, a UTM
#'   CRS is selected automatically.
#'
#' @return An `st_crs` object containing the projected analysis CRS.
#'
#' @details
#' When `analysis_crs` is `NULL`, copies of the spatial inputs are temporarily
#' transformed to WGS 84 to determine their combined longitude and latitude
#' extent. The UTM zone containing the center of that extent is selected. The
#' northern-hemisphere WGS 84 UTM definition is used for study centers at or
#' north of the equator, and the southern-hemisphere definition is used for
#' study centers south of the equator.
#'
#' Automatic selection requires every input feature to fall between 80 degrees
#' south and 84 degrees north. A warning is issued when the combined extent
#' spans more than one UTM zone. In that case, the center zone is still
#' returned, but the user should consider supplying an appropriate projected
#' `analysis_crs`.
#'
#' The input objects are validated but are not transformed or otherwise
#' modified by this function.
#'
#' @examples
#' pumping_wells <- sf::st_as_sf(
#'   tibble::tibble(
#'     pump_id = "pump_1",
#'     x = -93.25,
#'     y = 44.95,
#'     K = units::set_units(10, "m/day"),
#'     D = units::set_units(20, "m"),
#'     V = 0.15
#'   ),
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' stream_reaches <- sf::st_sf(
#'   reach_id = "reach_1",
#'   geometry = sf::st_sfc(
#'     sf::st_linestring(
#'       matrix(c(-93.30, 44.90, -93.20, 45.00), ncol = 2, byrow = TRUE)
#'     ),
#'     crs = 4326
#'   )
#' )
#'
#' # Automatically select WGS 84 / UTM zone 15N.
#' automatic_crs <- isw:::.select_analysis_crs(
#'   pumping_wells,
#'   stream_reaches
#' )
#' automatic_crs$epsg
#'
#' # Alternatively, specify another projected CRS.
#' supplied_crs <- isw:::.select_analysis_crs(
#'   pumping_wells,
#'   stream_reaches,
#'   analysis_crs = 26915
#' )
#' supplied_crs$epsg
#'
#' @keywords internal
.select_analysis_crs <- function(
    pumping_wells,
    stream_reaches,
    observation_wells = NULL,
    analysis_crs = NULL) {

  .validate_pumping_wells(pumping_wells)
  .validate_stream_reaches(stream_reaches)
  .validate_observation_wells(observation_wells)

  if (!is.null(analysis_crs)) {
    selected_crs <- tryCatch(
      suppressWarnings(sf::st_crs(analysis_crs)),
      error = function(e) NULL
    )

    if (is.null(selected_crs) || is.na(selected_crs)) {
      stop("analysis_crs must define a valid projected CRS.")
    }

    if (!grepl("^PROJCRS\\[", selected_crs$wkt)) {
      stop("analysis_crs must define a projected CRS.")
    }

    return(selected_crs)
  }

  spatial_inputs <- list(pumping_wells, stream_reaches)

  if (!is.null(observation_wells)) {
    spatial_inputs <- c(spatial_inputs, list(observation_wells))
  }

  wgs84_extents <- lapply(
    spatial_inputs,
    function(x) sf::st_bbox(sf::st_transform(x, 4326))
  )

  longitude_range <- range(unlist(lapply(
    wgs84_extents,
    function(x) x[c("xmin", "xmax")]
  )))
  latitude_range <- range(unlist(lapply(
    wgs84_extents,
    function(x) x[c("ymin", "ymax")]
  )))

  if (diff(longitude_range) > 180) {
    stop(
      "Automatic UTM selection is not supported for study areas that cross ",
      "the antimeridian; supply analysis_crs."
    )
  }

  if (latitude_range[[1]] < -80 || latitude_range[[2]] > 84) {
    stop(
      "Automatic UTM selection requires spatial inputs between 80 degrees ",
      "south and 84 degrees north; supply analysis_crs."
    )
  }

  get_utm_zone <- function(longitude) {
    pmin(60, pmax(1, floor((longitude + 180) / 6) + 1))
  }

  center_longitude <- mean(longitude_range)
  center_latitude <- mean(latitude_range)
  utm_zone <- get_utm_zone(center_longitude)
  extent_zones <- unique(get_utm_zone(longitude_range))

  if (length(extent_zones) > 1) {
    warning(
      "The spatial inputs span multiple UTM zones; zone ",
      utm_zone,
      " was selected from the center of the combined extent. Consider ",
      "supplying analysis_crs.",
      call. = FALSE
    )
  }

  if (center_latitude >= 0) {
    utm_epsg <- 32600 + utm_zone
  } else {
    utm_epsg <- 32700 + utm_zone
  }

  sf::st_crs(utm_epsg)
}
