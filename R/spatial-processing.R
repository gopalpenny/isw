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

#' Prepare spatial inputs for analysis
#'
#' Transform pumping wells, stream reaches, and optional observation wells to
#' one projected coordinate reference system for distance-based modeling.
#'
#' @param pumping_wells A pumping-well object accepted by
#'   [`.validate_pumping_wells()`].
#' @param stream_reaches A stream-reach object accepted by
#'   [`.validate_stream_reaches()`].
#' @param observation_wells Either `NULL` or an observation-well object
#'   accepted by [`.validate_observation_wells()`].
#' @param analysis_crs Either `NULL` or a projected coordinate reference system
#'   accepted by [sf::st_crs()]. When `NULL`, [`.select_analysis_crs()`] selects
#'   a local UTM CRS automatically.
#'
#' @return A list containing:
#' \describe{
#'   \item{`pumping_wells`}{The prepared pumping-well `sf` object.}
#'   \item{`stream_reaches`}{The prepared stream-reach `sf` object.}
#'   \item{`observation_wells`}{The prepared observation-well `sf` object, or
#'     `NULL` when observation wells were not supplied.}
#'   \item{`analysis_crs`}{The projected `st_crs` object used for every prepared
#'     spatial input.}
#' }
#'
#' @details
#' All prepared geometries are transformed to the selected projected CRS and
#' reduced to their horizontal X and Y coordinates. Z and M coordinates are
#' dropped because the analytical models operate in two horizontal dimensions.
#'
#' If `pumping_wells` does not include `well_diam`, the prepared copy receives
#' a `well_diam` column containing zero meters for every pump. Existing aquifer
#' properties and their physical units are otherwise retained unchanged.
#'
#' The function returns transformed copies and does not modify any object
#' supplied by the user.
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
#' prepared_inputs <- isw:::.prepare_spatial_inputs(
#'   pumping_wells,
#'   stream_reaches
#' )
#'
#' prepared_inputs$analysis_crs$epsg
#' prepared_inputs$pumping_wells$well_diam
#' sf::st_crs(prepared_inputs$stream_reaches)$Name
#'
#' @keywords internal
.prepare_spatial_inputs <- function(
    pumping_wells,
    stream_reaches,
    observation_wells = NULL,
    analysis_crs = NULL) {

  selected_crs <- .select_analysis_crs(
    pumping_wells,
    stream_reaches,
    observation_wells,
    analysis_crs
  )

  prepare_geometry <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    sf::st_transform(x, selected_crs)
  }

  prepared_pumping_wells <- prepare_geometry(pumping_wells)
  prepared_stream_reaches <- prepare_geometry(stream_reaches)
  prepared_observation_wells <- prepare_geometry(observation_wells)

  if (!("well_diam" %in% names(prepared_pumping_wells))) {
    prepared_pumping_wells$well_diam <- units::set_units(
      rep(0, nrow(prepared_pumping_wells)),
      "m",
      mode = "standard"
    )
  }

  list(
    pumping_wells = prepared_pumping_wells,
    stream_reaches = prepared_stream_reaches,
    observation_wells = prepared_observation_wells,
    analysis_crs = selected_crs
  )
}
