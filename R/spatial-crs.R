.is_utm_crs <- function(crs) {
  # CRS metadata must explicitly describe UTM coordinates in meters.
  proj <- crs$proj4string
  !is.na(crs) && !is.null(proj) && !is.na(proj) &&
    grepl("\\+proj=utm( |$)", proj) && grepl("\\+units=m( |$)", proj)
}

.select_utm_crs <- function(spatial_inputs, analysis_crs = NULL, preferred_crs = NULL) {
  # Input CRS selection retains sf metadata; zone crossings are permitted.
  if (!is.null(analysis_crs)) {
    selected <- tryCatch(suppressWarnings(sf::st_crs(analysis_crs)), error = function(e) NULL)
    if (is.null(selected) || is.na(selected)) stop("analysis_crs must define a valid projected CRS.")
    if (!grepl("^PROJCRS\\[", selected$wkt)) stop("analysis_crs must define a projected CRS.")
    if (!.is_utm_crs(selected)) stop("analysis_crs must define a UTM CRS in meters.")
    return(selected)
  }
  if (!is.null(preferred_crs) && .is_utm_crs(preferred_crs)) return(preferred_crs)
  extents <- lapply(Filter(Negate(is.null), spatial_inputs), function(x) {
    sf::st_bbox(sf::st_transform(x, 4326))
  })
  longitude_range <- range(vapply(extents, function(x) x[["xmin"]], numeric(1)),
                           vapply(extents, function(x) x[["xmax"]], numeric(1)))
  latitude_range <- range(vapply(extents, function(x) x[["ymin"]], numeric(1)),
                          vapply(extents, function(x) x[["ymax"]], numeric(1)))
  if (diff(longitude_range) > 180) stop("Automatic UTM selection is not supported for study areas that cross the antimeridian.")
  if (latitude_range[[1]] < -80 || latitude_range[[2]] > 84) {
    stop("Automatic UTM selection requires spatial inputs between 80 degrees south and 84 degrees north.")
  }
  zone <- max(1, min(60, floor((mean(longitude_range) + 180) / 6) + 1))
  sf::st_crs(if (mean(latitude_range) >= 0) 32600 + zone else 32700 + zone)
}

#' Select the projected CRS used for spatial analysis
#'
#' Select a local UTM coordinate reference system from the spatial extent of
#' the model inputs, retain an existing UTM stream CRS, or validate a supplied UTM CRS.
#'
#' @param pumping_wells A pumping-well object accepted by
#'   `.validate_pumping_wells()`.
#' @param stream_reaches A stream-reach object accepted by
#'   `.validate_stream_reaches()`.
#' @param observation_wells Either `NULL` or an observation-well object
#'   accepted by `.validate_observation_wells()`.
#' @param analysis_crs Either `NULL` or a coordinate reference system accepted
#'   by [sf::st_crs()]. The supplied CRS must be UTM in meters. When `NULL`, a UTM
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
#' south and 84 degrees north. Crossing UTM zones is permitted without warning.
#' An existing UTM stream CRS is retained so subsequent workflow calls share it.
#'
#' The input objects are validated but are not transformed or otherwise
#' modified by this function.
#'
#' @examples
#' pumping_wells <- example_pumping_wells
#' stream_reaches <- example_stream_reaches
#'
#' # Automatically select WGS 84 / UTM zone 15N.
#' automatic_crs <- isw:::.select_analysis_crs(
#'   pumping_wells,
#'   stream_reaches
#' )
#' automatic_crs$epsg
#'
#' # Alternatively, specify another UTM CRS.
#' supplied_crs <- isw:::.select_analysis_crs(
#'   pumping_wells,
#'   stream_reaches,
#'   analysis_crs = 26915
#' )
#' supplied_crs$epsg
#'
#' @noRd
.select_analysis_crs <- function(pumping_wells, stream_reaches,
                                 observation_wells = NULL, analysis_crs = NULL) {
  # Validate raw spatial inputs at preparation entry; retain sf/CRS metadata.
  .validate_pumping_wells(pumping_wells)
  .validate_stream_reaches(stream_reaches)
  .validate_observation_wells(observation_wells)
  .select_utm_crs(list(pumping_wells, stream_reaches, observation_wells),
                  analysis_crs, sf::st_crs(stream_reaches))
}

.transform_spatial_geometry <- function(x, analysis_crs) {
  # Validated sf inputs retain CRS; transform all geometry columns, not just the active one.
  if (is.null(x)) return(NULL)
  geometry_columns <- names(x)[vapply(x, inherits, logical(1), "sfc")]
  for (column in geometry_columns) {
    x[[column]] <- sf::st_transform(sf::st_zm(x[[column]], drop = TRUE, what = "ZM"), analysis_crs)
  }
  x
}

#' Prepare spatial inputs for analysis
#'
#' Transform pumping wells, stream reaches, and optional observation wells to
#' one projected coordinate reference system for distance-based modeling.
#'
#' @param pumping_wells A pumping-well object accepted by
#'   `.validate_pumping_wells()`.
#' @param stream_reaches A stream-reach object accepted by
#'   `.validate_stream_reaches()`.
#' @param observation_wells Either `NULL` or an observation-well object
#'   accepted by `.validate_observation_wells()`.
#' @param analysis_crs Either `NULL` or a meter-based UTM coordinate reference system
#'   accepted by [sf::st_crs()]. When `NULL`, `.select_analysis_crs()` selects
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
#' pumping_wells <- example_pumping_wells
#' stream_reaches <- example_stream_reaches
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
#' @noRd
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

    .transform_spatial_geometry(x, selected_crs)
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

# Select the projected analysis CRS used to segment a stream network.
.select_stream_analysis_crs <- function(stream_reaches, analysis_crs = NULL) {
  # Input validation precedes shared UTM selection; retain existing UTM networks.
  .validate_stream_reaches(stream_reaches)
  .select_utm_crs(list(stream_reaches), analysis_crs, sf::st_crs(stream_reaches))
}
