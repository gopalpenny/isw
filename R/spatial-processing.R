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

# Select the projected analysis CRS used to segment a stream network.
.select_stream_analysis_crs <- function(stream_reaches, analysis_crs = NULL) {
  .validate_stream_reaches(stream_reaches)

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

  stream_crs <- sf::st_crs(stream_reaches)

  if (grepl("^PROJCRS\\[", stream_crs$wkt)) {
    if (identical(stream_crs$epsg, 3857L)) {
      warning(
        "Web Mercator is not recommended for hydraulic distance analysis; ",
        "consider supplying a suitable projected analysis_crs.",
        call. = FALSE
      )
    }
    return(stream_crs)
  }

  wgs84_extent <- sf::st_bbox(sf::st_transform(stream_reaches, 4326))
  longitude_range <- unname(wgs84_extent[c("xmin", "xmax")])
  latitude_range <- unname(wgs84_extent[c("ymin", "ymax")])

  if (diff(longitude_range) > 180) {
    stop(
      "Automatic UTM selection is not supported for streams that cross ",
      "the antimeridian; supply analysis_crs."
    )
  }

  if (latitude_range[[1]] < -80 || latitude_range[[2]] > 84) {
    stop(
      "Automatic UTM selection requires stream_reaches between 80 degrees ",
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
      "stream_reaches spans multiple UTM zones; zone ", utm_zone,
      " was selected from the center of its extent. Consider supplying ",
      "a suitable regional analysis_crs.",
      call. = FALSE
    )
  }

  sf::st_crs(if (center_latitude >= 0) 32600 + utm_zone else 32700 + utm_zone)
}

#' Create stream segments for analytical modeling
#'
#' Transform a stream network to a projected analysis CRS and divide it into
#' approximately equal-length model segments.
#'
#' @param stream_reaches An `sf` stream-reach object accepted by
#'   [`.validate_stream_reaches()`].
#' @param reach_spacing A scalar `units` length giving the maximum length of a
#'   model segment.
#' @param analysis_crs Either `NULL` or a projected coordinate reference
#'   system accepted by [sf::st_crs()]. When `NULL`, an existing projected
#'   stream CRS is retained; geographic streams are transformed to a local UTM
#'   CRS selected from their extent.
#'
#' @return A projected `sf` object with one line feature per stream segment.
#'   It contains `reach_id`, `reach_segment_id`, `represented_length`,
#'   `well_diam`, `model_point`, and the segment line geometry. `well_diam`
#'   defaults to half the actual represented segment length, so the discrete-
#'   well self-response is evaluated at one quarter of that length.
#'
#' @details
#' `model_point` is the along-line midpoint used as the discrete injection-well
#' and constant-head collocation location. Input objects are not modified.
#' Users may replace the positive `well_diam` values in the returned object
#' before constructing an injection schedule.
#'
#' @examples
#' stream_segments <- get_stream_segments(
#'   example_stream_reaches,
#'   reach_spacing = units::set_units(100, "m")
#' )
#' stream_segments[c(
#'   "reach_id", "reach_segment_id", "represented_length", "well_diam"
#' )]
#'
#' @export
get_stream_segments <- function(
    stream_reaches,
    reach_spacing,
    analysis_crs = NULL) {

  selected_crs <- .select_stream_analysis_crs(
    stream_reaches,
    analysis_crs
  )
  prepared_reaches <- sf::st_transform(
    sf::st_zm(stream_reaches, drop = TRUE, what = "ZM"),
    selected_crs
  )
  stream_segments <- .discretize_stream_reaches(
    prepared_reaches,
    reach_spacing
  )
  stream_segments$well_diam <- stream_segments$represented_length / 2
  geometry_column <- attr(stream_segments, "sf_column")
  key_columns <- c(
    "reach_id",
    "reach_segment_id",
    "represented_length",
    "well_diam"
  )
  additional_columns <- setdiff(
    names(stream_segments),
    c(key_columns, "model_point", geometry_column)
  )
  stream_segments[c(
    key_columns,
    additional_columns,
    "model_point",
    geometry_column
  )]
}

# Validate the neutral stream-segment representation.
.validate_stream_segments <- function(stream_segments) {
  if (!inherits(stream_segments, "sf") || nrow(stream_segments) == 0) {
    stop("stream_segments must be a nonempty sf object.")
  }

  required_columns <- c(
    "reach_id",
    "reach_segment_id",
    "represented_length",
    "well_diam",
    "model_point"
  )
  missing_columns <- setdiff(required_columns, names(stream_segments))

  if (length(missing_columns) > 0) {
    stop(
      "stream_segments is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  if (is.na(sf::st_crs(stream_segments)) ||
      !grepl("^PROJCRS\\[", sf::st_crs(stream_segments)$wkt)) {
    stop("stream_segments must use a projected CRS.")
  }

  if (!all(as.character(sf::st_geometry_type(
    stream_segments,
    by_geometry = TRUE
  )) == "LINESTRING")) {
    stop("Every stream_segments geometry must be a LINESTRING.")
  }

  if (!is.character(stream_segments$reach_id) ||
      !is.character(stream_segments$reach_segment_id) ||
      anyNA(stream_segments$reach_id) ||
      anyNA(stream_segments$reach_segment_id) ||
      any(trimws(stream_segments$reach_id) == "") ||
      any(trimws(stream_segments$reach_segment_id) == "") ||
      anyDuplicated(stream_segments$reach_segment_id) > 0) {
    stop(
      "stream_segments reach identifiers must be nonmissing, nonempty, ",
      "character values with unique reach_segment_id values."
    )
  }

  check_dimensionality(
    stream_segments$represented_length,
    "m",
    "stream_segments$represented_length"
  )
  check_dimensionality(
    stream_segments$well_diam,
    "m",
    "stream_segments$well_diam"
  )

  if (any(!is.finite(as.numeric(stream_segments$represented_length))) ||
      any(as.numeric(stream_segments$represented_length) <= 0) ||
      any(!is.finite(as.numeric(stream_segments$well_diam))) ||
      any(as.numeric(stream_segments$well_diam) <= 0)) {
    stop(
      "stream_segments represented_length and well_diam must contain ",
      "finite, positive values."
    )
  }

  if (!inherits(stream_segments$model_point, "sfc") ||
      length(stream_segments$model_point) != nrow(stream_segments) ||
      !all(as.character(sf::st_geometry_type(
        stream_segments$model_point
      )) == "POINT") ||
      any(sf::st_is_empty(stream_segments$model_point))) {
    stop(
      "stream_segments$model_point must contain one nonempty POINT per row."
    )
  }

  segment_lengths <- sf::st_length(stream_segments)

  if (!isTRUE(all.equal(
    segment_lengths,
    units::set_units(
      stream_segments$represented_length,
      units::deparse_unit(segment_lengths),
      mode = "standard"
    )
  ))) {
    stop(
      "stream_segments$represented_length must equal the active geometry ",
      "length."
    )
  }

  stream_segments
}

#' Discretize stream reaches for modeling
#'
#' Divide prepared stream geometries into approximately equal-length reach
#' segments while retaining their line geometry and parent identifiers.
#'
#' @param stream_reaches A projected stream-reach object accepted by
#'   [`.validate_stream_reaches()`], typically the `stream_reaches` element
#'   returned by [`.prepare_spatial_inputs()`].
#' @param reach_spacing A scalar `units` object with length dimensions giving
#'   the maximum target length of a reach segment.
#'
#' @return An `sf` object with one `LINESTRING` feature per reach segment. The
#'   following columns are added:
#' \describe{
#'   \item{`reach_segment_id`}{Uniquely identifies the discretized reach
#'     segment within its original `reach_id`.}
#'   \item{`represented_length`}{The actual length represented by the model
#'     reach segment, retaining the linear units of the analysis CRS.}
#'   \item{`model_point`}{An `sfc_POINT` column containing the midpoint along
#'     the reach-segment line. The sliced line remains the active geometry.}
#' }
#'
#' @details
#' Each `MULTILINESTRING` is first separated into its component
#' `LINESTRING`s so a reach segment is never created across a spatial gap.
#' Components are handled internally and do not receive a separate identifier.
#'
#' For each component, the number of reach segments is its length divided by
#' `reach_spacing` and rounded up. The complete part is then divided into that
#' number of equal-length segments. Consequently, no reach segment exceeds the
#' requested spacing, and `represented_length` may be smaller than
#' `reach_spacing` or differ among components. Segment numbers are sequential
#' within each original `reach_id`, including when it contains multiple
#' components.
#'
#' `model_point` is the midpoint measured along each sliced line, rather than
#' its geometric centroid, so the point is guaranteed to lie on the
#' stream geometry. Additional input attributes are repeated for every model
#' reach segment derived from the source feature.
#'
#' The names `reach_segment_id`, `represented_length`, and `model_point` are
#' reserved for values created by this function. The input object is not
#' modified.
#'
#' @examples
#' stream_reaches <- example_stream_reaches
#'
#' reach_segments <- isw:::.discretize_stream_reaches(
#'   stream_reaches,
#'   reach_spacing = units::set_units(100, "m")
#' )
#'
#' reach_segments[c(
#'   "reach_id", "reach_segment_id", "represented_length"
#' )]
#' sf::st_coordinates(reach_segments$model_point)
#'
#' plot_stream_discretization <- function(stream_reaches, reach_segments) {
#'   ggplot2::ggplot() +
#'     ggplot2::geom_sf(data = stream_reaches, color = "grey75", linewidth = 3) +
#'     ggplot2::geom_sf(
#'       data = reach_segments,
#'       ggplot2::aes(color = reach_segment_id),
#'       linewidth = 1.5
#'     ) +
#'     ggplot2::geom_sf(
#'       data = reach_segments,
#'       ggplot2::aes(geometry = model_point),
#'       color = "black",
#'       fill = "white",
#'       shape = 21,
#'       size = 2.5
#'     ) +
#'     ggplot2::labs(color = "Reach segment") +
#'     ggplot2::theme_minimal()
#' }
#'
#' plot_stream_discretization(stream_reaches, reach_segments)
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
    "reach_segment_id",
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

  part_lengths <- sf::st_length(line_parts)
  spacing_in_crs_units <- units::set_units(
    reach_spacing,
    units::deparse_unit(part_lengths),
    mode = "standard"
  )
  model_reaches_per_part <- ceiling(
    as.numeric(part_lengths / spacing_in_crs_units)
  )

  total_model_reaches <- sum(model_reaches_per_part)
  source_part_rows <- integer(total_model_reaches)
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

      start_point <- .point_along_linestring(
        coordinates,
        cumulative_length,
        start_distance
      )
      end_point <- .point_along_linestring(
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
      model_geometries[[output_index]] <- sf::st_linestring(
        segment_coordinates
      )
      model_points[[output_index]] <- sf::st_point(
        .point_along_linestring(
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
  reach_segment_numbers <- stats::ave(
    seq_len(nrow(model_reaches)),
    model_reaches$reach_id,
    FUN = seq_along
  )
  model_reaches$reach_segment_id <- paste0(
    model_reaches$reach_id,
    "_segment_",
    reach_segment_numbers
  )
  model_reaches$represented_length <- sf::st_length(model_reaches)
  model_reaches$model_point <- sf::st_sfc(
    model_points,
    crs = sf::st_crs(model_reaches)
  )

  geometry_column <- attr(model_reaches, "sf_column")
  identifying_columns <- c(
    "reach_id",
    "reach_segment_id",
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

# Return coordinates at a distance measured along a LINESTRING.
.point_along_linestring <- function(
    coordinates,
    cumulative_length,
    distance) {

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

#' Generate sample points within stream-reach segments
#'
#' Create regularly distributed points within each discretized reach segment
#' for web-based stream-depletion apportionment.
#'
#' @param reach_segments A projected `sf` object returned by
#'   [`.discretize_stream_reaches()`]. It must contain `reach_id`,
#'   `reach_segment_id`, and `represented_length` columns and one `LINESTRING`
#'   geometry per row.
#' @param sample_spacing A scalar `units` object with length dimensions giving
#'   the maximum stream length represented by an apportionment sample point.
#'
#' @return An `sf` point object with columns `reach_id`, `reach_segment_id`,
#'   `sample_point_id`, and `sampled_length`. Every reach segment receives at
#'   least one point. `sampled_length` retains the linear units of the analysis
#'   CRS.
#'
#' @details
#' The number of points in each reach segment is its length divided by
#' `sample_spacing` and rounded up. Points are placed at the along-line centers
#' of equal-length sampling intervals. This avoids duplicated points at shared
#' segment boundaries and assigns every point an explicit `sampled_length`.
#'
#' These points are distinct from the single `model_point` stored with each
#' reach segment. `model_point` represents the segment in the model, while the
#' finer sample points describe its geometry during web apportionment.
#'
#' @examples
#' stream_reaches <- example_stream_reaches
#'
#' reach_segments <- isw:::.discretize_stream_reaches(
#'   stream_reaches,
#'   reach_spacing = units::set_units(150, "m")
#' )
#'
#' sample_points <- generate_segment_sample_points(
#'   reach_segments,
#'   sample_spacing = units::set_units(40, "m")
#' )
#'
#' sample_points[c(
#'   "reach_id", "reach_segment_id", "sample_point_id", "sampled_length"
#' )]
#'
#' @export
generate_segment_sample_points <- function(reach_segments, sample_spacing) {

  if (!inherits(reach_segments, "sf")) {
    stop("reach_segments must be an sf object.")
  }

  required_columns <- c(
    "reach_id",
    "reach_segment_id",
    "represented_length"
  )
  missing_columns <- setdiff(required_columns, names(reach_segments))

  if (length(missing_columns) > 0) {
    stop(
      "reach_segments is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  if (nrow(reach_segments) == 0) {
    stop("reach_segments must contain at least one feature.")
  }

  if (is.na(sf::st_crs(reach_segments)) ||
      !grepl("^PROJCRS\\[", sf::st_crs(reach_segments)$wkt)) {
    stop("reach_segments must use a projected CRS.")
  }

  geometry_types <- as.character(
    sf::st_geometry_type(reach_segments, by_geometry = TRUE)
  )

  if (!all(geometry_types == "LINESTRING")) {
    stop("Every reach_segments geometry must be a LINESTRING.")
  }

  if (!is.character(reach_segments$reach_id) ||
      !is.character(reach_segments$reach_segment_id) ||
      anyNA(reach_segments$reach_id) ||
      anyNA(reach_segments$reach_segment_id) ||
      any(trimws(reach_segments$reach_id) == "") ||
      any(trimws(reach_segments$reach_segment_id) == "")) {
    stop(
      "reach_segments$reach_id and reach_segments$reach_segment_id must be ",
      "nonmissing, nonempty character vectors."
    )
  }

  if (anyDuplicated(reach_segments$reach_segment_id) > 0) {
    stop("reach_segments$reach_segment_id values must be unique.")
  }

  check_dimensionality(
    reach_segments$represented_length,
    desired_units = "m",
    variable_name = "reach_segments$represented_length"
  )
  check_dimensionality(
    sample_spacing,
    desired_units = "m",
    variable_name = "sample_spacing"
  )

  if (length(sample_spacing) != 1 ||
      !is.finite(as.numeric(sample_spacing)) ||
      as.numeric(sample_spacing) <= 0) {
    stop("sample_spacing must be a finite, positive scalar length.")
  }

  segment_lengths <- sf::st_length(reach_segments)

  if (!isTRUE(all.equal(
    segment_lengths,
    units::set_units(
      reach_segments$represented_length,
      units::deparse_unit(segment_lengths),
      mode = "standard"
    )
  ))) {
    stop(
      "reach_segments$represented_length must equal the active geometry ",
      "length."
    )
  }

  spacing_in_crs_units <- units::set_units(
    sample_spacing,
    units::deparse_unit(segment_lengths),
    mode = "standard"
  )
  points_per_segment <- ceiling(
    as.numeric(segment_lengths / spacing_in_crs_units)
  )
  total_points <- sum(points_per_segment)
  source_segment_rows <- integer(total_points)
  point_numbers <- integer(total_points)
  point_geometries <- vector("list", total_points)
  sampled_lengths <- numeric(total_points)
  output_index <- 0L

  for (segment_index in seq_len(nrow(reach_segments))) {
    coordinates <- sf::st_coordinates(
      sf::st_geometry(reach_segments)[[segment_index]]
    )[, 1:2, drop = FALSE]
    coordinate_differences <- coordinates[-1, , drop = FALSE] -
      coordinates[-nrow(coordinates), , drop = FALSE]
    cumulative_length <- c(
      0,
      cumsum(sqrt(rowSums(coordinate_differences^2)))
    )
    segment_length <- cumulative_length[[length(cumulative_length)]]
    number_of_points <- points_per_segment[[segment_index]]
    interval_length <- segment_length / number_of_points
    point_distances <- (seq_len(number_of_points) - 0.5) * interval_length

    for (point_number in seq_len(number_of_points)) {
      output_index <- output_index + 1L
      source_segment_rows[[output_index]] <- segment_index
      point_numbers[[output_index]] <- point_number
      sampled_lengths[[output_index]] <- interval_length
      point_geometries[[output_index]] <- sf::st_point(
        .point_along_linestring(
          coordinates,
          cumulative_length,
          point_distances[[point_number]]
        )
      )
    }
  }

  length_units <- units::deparse_unit(segment_lengths)

  sf::st_sf(
    reach_id = reach_segments$reach_id[source_segment_rows],
    reach_segment_id =
      reach_segments$reach_segment_id[source_segment_rows],
    sample_point_id = paste0(
      reach_segments$reach_segment_id[source_segment_rows],
      "_point_",
      point_numbers
    ),
    sampled_length = units::set_units(
      sampled_lengths,
      length_units,
      mode = "standard"
    ),
    geometry = sf::st_sfc(
      point_geometries,
      crs = sf::st_crs(reach_segments)
    )
  )
}
