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

#' Discretize stream reaches for modeling
#'
#' Divide prepared stream geometries into approximately equal-length model
#' reaches while retaining their line geometry and parent identifiers.
#'
#' @param stream_reaches A projected stream-reach object accepted by
#'   [`.validate_stream_reaches()`], typically the `stream_reaches` element
#'   returned by [`.prepare_spatial_inputs()`].
#' @param reach_spacing A scalar `units` object with length dimensions giving
#'   the maximum target length of a model reach.
#'
#' @return An `sf` object with one `LINESTRING` feature per model reach. The
#'   following columns are added:
#' \describe{
#'   \item{`reach_part_id`}{Identifies the `LINESTRING` component of the
#'     original `reach_id`.}
#'   \item{`model_reach_id`}{Uniquely identifies the discretized model reach.}
#'   \item{`represented_length`}{The actual length represented by the model
#'     reach, retaining the linear units of the analysis CRS.}
#'   \item{`model_point`}{An `sfc_POINT` column containing the midpoint along
#'     the model-reach line. The sliced line remains the active geometry.}
#' }
#'
#' @details
#' Each `MULTILINESTRING` is first separated into its component
#' `LINESTRING`s. Components receive `reach_part_id` values within their
#' original `reach_id`. Ordinary `LINESTRING` features have one part.
#'
#' For each part, the number of model reaches is the part length divided by
#' `reach_spacing` and rounded up. The complete part is then divided into that
#' number of equal-length segments. Consequently, no model reach exceeds the
#' requested spacing, and `represented_length` may be smaller than
#' `reach_spacing` or differ among input parts.
#'
#' `model_point` is the midpoint measured along each sliced line, rather than
#' its geometric centroid, so the point is guaranteed to lie on the modeled
#' stream geometry. Additional input attributes are repeated for every model
#' reach derived from the source feature.
#'
#' The names `reach_part_id`, `model_reach_id`, `represented_length`, and
#' `model_point` are reserved for values created by this function. The input
#' object is not modified.
#'
#' @examples
#' stream_reaches <- sf::st_sf(
#'   reach_id = c("upstream_1", "upstream_2", "downstream"),
#'   geometry = sf::st_sfc(
#'     sf::st_linestring(
#'       matrix(c(500000, 4980200, 500150, 4980050), ncol = 2, byrow = TRUE)
#'     ),
#'     sf::st_linestring(
#'       matrix(c(500000, 4979900, 500150, 4980050), ncol = 2, byrow = TRUE)
#'     ),
#'     sf::st_linestring(
#'       matrix(c(500150, 4980050, 500350, 4979850), ncol = 2, byrow = TRUE)
#'     ),
#'     crs = 32615
#'   )
#' )
#'
#' model_reaches <- isw:::.discretize_stream_reaches(
#'   stream_reaches,
#'   reach_spacing = units::set_units(100, "m")
#' )
#'
#' model_reaches[c(
#'   "reach_id", "reach_part_id", "model_reach_id", "represented_length"
#' )]
#' sf::st_coordinates(model_reaches$model_point)
#'
#' plot_stream_discretization <- function(stream_reaches, model_reaches) {
#'   ggplot2::ggplot() +
#'     ggplot2::geom_sf(data = stream_reaches, color = "grey75", linewidth = 3) +
#'     ggplot2::geom_sf(
#'       data = model_reaches,
#'       ggplot2::aes(color = model_reach_id),
#'       linewidth = 1.5
#'     ) +
#'     ggplot2::geom_sf(
#'       data = model_reaches,
#'       ggplot2::aes(geometry = model_point),
#'       color = "black",
#'       fill = "white",
#'       shape = 21,
#'       size = 2.5
#'     ) +
#'     ggplot2::labs(color = "Model reach") +
#'     ggplot2::theme_minimal()
#' }
#'
#' plot_stream_discretization(stream_reaches, model_reaches)
#'
#' @keywords internal
.discretize_stream_reaches <- function(stream_reaches, reach_spacing) {

  .validate_stream_reaches(stream_reaches)

  if (!grepl("^PROJCRS\\[", sf::st_crs(stream_reaches)$wkt)) {
    stop(
      "stream_reaches must use a projected CRS before model discretization."
    )
  }

  check_dimensionality(
    reach_spacing,
    desired_units = "m",
    variable_name = "reach_spacing"
  )

  if (length(reach_spacing) != 1 ||
      !is.finite(as.numeric(reach_spacing)) ||
      as.numeric(reach_spacing) <= 0) {
    stop("reach_spacing must be a finite, positive scalar length.")
  }

  reserved_columns <- c(
    "reach_part_id",
    "model_reach_id",
    "represented_length",
    "model_point"
  )
  conflicting_columns <- intersect(reserved_columns, names(stream_reaches))

  if (length(conflicting_columns) > 0) {
    stop(
      "stream_reaches contains columns reserved for model discretization: ",
      paste(conflicting_columns, collapse = ", "),
      "."
    )
  }

  stream_reaches_xy <- sf::st_zm(
    stream_reaches,
    drop = TRUE,
    what = "ZM"
  )
  line_parts <- suppressWarnings(
    sf::st_cast(stream_reaches_xy, "LINESTRING")
  )

  part_number <- ave(
    seq_len(nrow(line_parts)),
    line_parts$reach_id,
    FUN = seq_along
  )
  line_parts$reach_part_id <- paste0(
    line_parts$reach_id,
    "_part_",
    part_number
  )

  part_lengths <- sf::st_length(line_parts)
  spacing_in_crs_units <- units::set_units(
    reach_spacing,
    units::deparse_unit(part_lengths),
    mode = "standard"
  )
  model_reaches_per_part <- ceiling(
    as.numeric(part_lengths / spacing_in_crs_units)
  )

  get_point_at_distance <- function(coordinates, cumulative_length, distance) {
    if (distance <= 0) {
      return(coordinates[1, ])
    }

    if (distance >= cumulative_length[[length(cumulative_length)]]) {
      return(coordinates[nrow(coordinates), ])
    }

    coordinate_indices <- seq_along(cumulative_length)
    segment_index <- max(which(
      cumulative_length <= distance &
        coordinate_indices < length(cumulative_length)
    ))
    segment_length <- cumulative_length[[segment_index + 1]] -
      cumulative_length[[segment_index]]
    fraction <- (distance - cumulative_length[[segment_index]]) /
      segment_length

    coordinates[segment_index, ] + fraction * (
      coordinates[segment_index + 1, ] - coordinates[segment_index, ]
    )
  }

  total_model_reaches <- sum(model_reaches_per_part)
  source_part_rows <- integer(total_model_reaches)
  segment_numbers <- integer(total_model_reaches)
  model_geometries <- vector("list", total_model_reaches)
  model_points <- vector("list", total_model_reaches)
  output_index <- 0L

  for (part_index in seq_len(nrow(line_parts))) {
    coordinates <- sf::st_coordinates(
      sf::st_geometry(line_parts)[[part_index]]
    )[, 1:2, drop = FALSE]
    coordinate_differences <- coordinates[-1, , drop = FALSE] -
      coordinates[-nrow(coordinates), , drop = FALSE]
    coordinate_lengths <- sqrt(rowSums(coordinate_differences^2))
    cumulative_length <- c(0, cumsum(coordinate_lengths))
    break_distances <- seq(
      0,
      cumulative_length[[length(cumulative_length)]],
      length.out = model_reaches_per_part[[part_index]] + 1
    )

    for (segment_number in seq_len(model_reaches_per_part[[part_index]])) {
      output_index <- output_index + 1L
      start_distance <- break_distances[[segment_number]]
      end_distance <- break_distances[[segment_number + 1]]
      midpoint_distance <- mean(c(start_distance, end_distance))

      start_point <- get_point_at_distance(
        coordinates,
        cumulative_length,
        start_distance
      )
      end_point <- get_point_at_distance(
        coordinates,
        cumulative_length,
        end_distance
      )
      interior_coordinates <- coordinates[
        cumulative_length > start_distance &
          cumulative_length < end_distance,
        ,
        drop = FALSE
      ]
      segment_coordinates <- rbind(
        start_point,
        interior_coordinates,
        end_point
      )

      source_part_rows[[output_index]] <- part_index
      segment_numbers[[output_index]] <- segment_number
      model_geometries[[output_index]] <- sf::st_linestring(
        segment_coordinates
      )
      model_points[[output_index]] <- sf::st_point(
        get_point_at_distance(
          coordinates,
          cumulative_length,
          midpoint_distance
        )
      )
    }
  }

  model_reaches <- line_parts[source_part_rows, , drop = FALSE]
  sf::st_geometry(model_reaches) <- sf::st_sfc(
    model_geometries,
    crs = sf::st_crs(line_parts)
  )
  model_reaches$model_reach_id <- paste0(
    model_reaches$reach_part_id,
    "_model_",
    segment_numbers
  )
  model_reaches$represented_length <- sf::st_length(model_reaches)
  model_reaches$model_point <- sf::st_sfc(
    model_points,
    crs = sf::st_crs(model_reaches)
  )

  geometry_column <- attr(model_reaches, "sf_column")
  identifying_columns <- c(
    "reach_id",
    "reach_part_id",
    "model_reach_id",
    "represented_length"
  )
  additional_columns <- setdiff(
    names(model_reaches),
    c(identifying_columns, "model_point", geometry_column)
  )

  model_reaches <- model_reaches[c(
    identifying_columns,
    additional_columns,
    "model_point",
    geometry_column
  )]

  row.names(model_reaches) <- NULL
  model_reaches
}
