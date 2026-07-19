# TODO: Before finalizing this public interface, review the source options,
# buffering and clipping defaults, retained USGS attributes, and the extent and
# field selection used for the packaged Sixmile Creek example.

#' Retrieve USGS stream reaches
#'
#' Retrieve stream-flowline geometry from the U.S. Geological Survey 3D
#' Hydrography Program (3DHP) for a user-supplied area of interest.
#'
#' @param aoi An `sf` object with a defined coordinate reference system.
#'   Polygon geometries define the query area directly. Point geometries may
#'   also be supplied when `buffer_distance` is specified.
#' @param buffer_distance Either `NULL` or a scalar `units` object with length
#'   dimensions. A positive buffer distance is required when `aoi` contains
#'   point geometry. It must be `NULL` when `aoi` contains polygon geometry.
#' @param source Character string specifying the USGS hydrography source.
#'   Currently, only `"3dhp"` is supported.
#' @param clip Logical. If `TRUE`, the default, returned flowlines are clipped
#'   to the query area. If `FALSE`, complete USGS flowline features that
#'   intersect the query area are returned.
#' @param keep_attributes Logical. If `TRUE`, the default, all attributes
#'   returned by the USGS service are retained. If `FALSE`, only `reach_id` and
#'   geometry are returned.
#'
#' @return An `sf` object in the coordinate reference system of `aoi`. The
#'   first column, `reach_id`, contains the character 3DHP identifier for each
#'   flowline. Remaining USGS attributes are retained when `keep_attributes`
#'   is `TRUE`.
#'
#' @details
#' This function uses [nhdplusTools::get_3dhp()] to query the 3DHP web service.
#' USGS is transitioning `nhdplusTools` to its new name, `hydrogeofetch`, while
#' retaining the same function interface. An internet connection is required.
#'
#' Point areas of interest are buffered before the web-service request. Buffer
#' construction is performed in a local UTM coordinate reference system and
#' the resulting polygon is transformed back to the input CRS. The downloaded
#' stream reaches remain in the input CRS; conversion to the projected model
#' CRS occurs later in the `isw` spatial-preparation workflow.
#'
#' Clipping may shorten flowlines at the query boundary. Features that only
#' touch the boundary at a point, along with empty or zero-length results, are
#' removed. The original 3DHP identifier is retained as `reach_id` for each
#' remaining feature.
#'
#' @examples
#' \dontrun{
#' pumping_wells <- sf::st_sf(
#'   pump_id = "pump_1",
#'   geometry = sf::st_sfc(
#'     sf::st_point(c(295500, 4783200)),
#'     crs = 26916
#'   )
#' )
#'
#' stream_reaches <- get_usgs_stream_reaches(
#'   aoi = pumping_wells,
#'   buffer_distance = units::set_units(10, "km")
#' )
#'
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = stream_reaches) +
#'   ggplot2::geom_sf(data = pumping_wells, color = "red", size = 3)
#' }
#'
#' @export
get_usgs_stream_reaches <- function(
    aoi,
    buffer_distance = NULL,
    source = "3dhp",
    clip = TRUE,
    keep_attributes = TRUE) {

  .validate_usgs_stream_aoi(aoi, buffer_distance)

  if (!is.character(source) || length(source) != 1 || is.na(source)) {
    stop("source must be a single character string.")
  }

  if (source != "3dhp") {
    stop('source must currently be "3dhp".')
  }

  if (!is.logical(clip) || length(clip) != 1 || is.na(clip)) {
    stop("clip must be TRUE or FALSE.")
  }

  if (
    !is.logical(keep_attributes) ||
      length(keep_attributes) != 1 ||
      is.na(keep_attributes)
  ) {
    stop("keep_attributes must be TRUE or FALSE.")
  }

  query_area <- .make_usgs_stream_query_area(aoi, buffer_distance)

  stream_reaches <- .fetch_3dhp_flowlines(query_area)

  if (is.null(stream_reaches) || nrow(stream_reaches) == 0) {
    stop("No USGS 3DHP flowlines were found in the query area.")
  }

  if (!inherits(stream_reaches, "sf")) {
    stop("The USGS 3DHP service did not return an sf object.")
  }

  if (is.na(sf::st_crs(stream_reaches))) {
    stop("The USGS 3DHP service returned flowlines without a defined CRS.")
  }

  if (!("id3dhp" %in% names(stream_reaches))) {
    stop("The USGS 3DHP response is missing the required id3dhp column.")
  }

  stream_reaches <- sf::st_transform(stream_reaches, sf::st_crs(aoi))

  if (clip) {
    stream_reaches <- suppressWarnings(
      sf::st_intersection(stream_reaches, sf::st_geometry(query_area))
    )

    stream_reaches <- suppressWarnings(
      sf::st_collection_extract(stream_reaches, "LINESTRING", warn = FALSE)
    )
  }

  stream_reaches <- stream_reaches[!sf::st_is_empty(stream_reaches), ]

  if (nrow(stream_reaches) == 0) {
    stop("No line geometry remained after clipping to the query area.")
  }

  reach_lengths <- suppressMessages(sf::st_length(stream_reaches))
  stream_reaches <- stream_reaches[
    is.finite(as.numeric(reach_lengths)) & as.numeric(reach_lengths) > 0,
  ]

  if (nrow(stream_reaches) == 0) {
    stop("No positive-length stream reaches remained in the query area.")
  }

  stream_reaches$id3dhp <- as.character(stream_reaches$id3dhp)

  if (anyDuplicated(stream_reaches$id3dhp) > 0) {
    stop("The USGS 3DHP response contains duplicate id3dhp values.")
  }

  names(stream_reaches)[names(stream_reaches) == "id3dhp"] <- "reach_id"

  geometry_column <- attr(stream_reaches, "sf_column")
  other_columns <- setdiff(
    names(stream_reaches),
    c("reach_id", geometry_column)
  )

  if (keep_attributes) {
    stream_reaches <- stream_reaches[
      c("reach_id", other_columns, geometry_column)
    ]
  } else {
    stream_reaches <- stream_reaches[c("reach_id", geometry_column)]
  }

  rownames(stream_reaches) <- NULL

  .validate_stream_reaches(stream_reaches)
}

#' Validate a USGS stream-query area
#'
#' @param aoi An object supplied as the stream-query area of interest.
#' @param buffer_distance Either `NULL` or a scalar length-units object.
#'
#' @return `aoi`, unchanged.
#'
#' @keywords internal
.validate_usgs_stream_aoi <- function(aoi, buffer_distance) {

  if (!inherits(aoi, "sf")) {
    stop("aoi must be an sf object.")
  }

  if (nrow(aoi) == 0) {
    stop("aoi must contain at least one feature.")
  }

  if (is.na(sf::st_crs(aoi))) {
    stop("aoi must have a defined CRS.")
  }

  if (any(sf::st_is_empty(aoi))) {
    stop("aoi cannot contain empty geometries.")
  }

  geometry_types <- as.character(sf::st_geometry_type(aoi, by_geometry = TRUE))
  point_types <- c("POINT", "MULTIPOINT")
  polygon_types <- c("POLYGON", "MULTIPOLYGON")

  if (all(geometry_types %in% point_types)) {
    if (is.null(buffer_distance)) {
      stop("buffer_distance is required when aoi contains point geometry.")
    }
  } else if (all(geometry_types %in% polygon_types)) {
    if (!is.null(buffer_distance)) {
      stop("buffer_distance must be NULL when aoi contains polygon geometry.")
    }
  } else {
    stop(
      "aoi geometries must be entirely point geometry or entirely polygon ",
      "geometry."
    )
  }

  if (!is.null(buffer_distance)) {
    if (!inherits(buffer_distance, "units") || length(buffer_distance) != 1) {
      stop("buffer_distance must be a scalar units object with length dimensions.")
    }

    check_dimensionality(
      buffer_distance,
      desired_units = "m",
      variable_name = "buffer_distance"
    )

    if (
      !is.finite(as.numeric(buffer_distance)) ||
        as.numeric(buffer_distance) <= 0
    ) {
      stop("buffer_distance must be finite and greater than zero.")
    }
  }

  coordinates <- sf::st_coordinates(aoi)

  if (any(!is.finite(coordinates[, 1:2, drop = FALSE]))) {
    stop("aoi cannot contain missing or nonfinite coordinates.")
  }

  aoi
}

#' Construct the polygon used for a USGS stream query
#'
#' @param aoi A validated point or polygon `sf` object.
#' @param buffer_distance Either `NULL` or a scalar length-units object.
#'
#' @return A one-feature `sf` polygon in the CRS of `aoi`.
#'
#' @keywords internal
.make_usgs_stream_query_area <- function(aoi, buffer_distance) {

  input_crs <- sf::st_crs(aoi)
  geometry_types <- as.character(sf::st_geometry_type(aoi, by_geometry = TRUE))

  if (all(geometry_types %in% c("POINT", "MULTIPOINT"))) {
    center <- suppressWarnings(
      sf::st_coordinates(
        sf::st_centroid(sf::st_union(sf::st_transform(aoi, 4326)))
      )
    )[1, 1:2]

    longitude <- center[[1]]
    latitude <- center[[2]]

    if (latitude < -80 || latitude > 84) {
      stop(
        "Point aoi must lie between 80 degrees south and 84 degrees north ",
        "for automatic UTM buffering."
      )
    }

    utm_zone <- floor((longitude + 180) / 6) + 1
    utm_zone <- max(1, min(60, utm_zone))
    utm_epsg <- if (latitude >= 0) 32600 + utm_zone else 32700 + utm_zone

    query_geometry <- aoi |>
      sf::st_transform(utm_epsg) |>
      sf::st_union() |>
      sf::st_buffer(dist = units::set_units(buffer_distance, "m")) |>
      sf::st_transform(input_crs)
  } else {
    query_geometry <- sf::st_union(aoi)
  }

  sf::st_sf(geometry = query_geometry)
}

#' Retrieve flowlines from the USGS 3DHP web service
#'
#' @param query_area A one-feature `sf` polygon.
#'
#' @return An `sf` object returned by the USGS `get_3dhp()` function.
#'
#' @keywords internal
.fetch_3dhp_flowlines <- function(query_area) {

  if (!requireNamespace("nhdplusTools", quietly = TRUE)) {
    stop(
      "Package 'nhdplusTools' is required to retrieve USGS stream data. ",
      "Install it from CRAN before calling ",
      "get_usgs_stream_reaches()."
    )
  }

  nhdplusTools::get_3dhp(
    AOI = query_area,
    type = "flowline",
    t_srs = sf::st_crs(query_area)$wkt
  )
}
