.is_utm_crs <- function(crs) {
  # CRS metadata must explicitly describe UTM coordinates in meters.
  proj <- crs$proj4string
  !is.na(crs) && !is.null(proj) && !is.na(proj) &&
    grepl("\\+proj=utm( |$)", proj) && grepl("\\+units=m( |$)", proj)
}

.select_utm_crs <- function(spatial_inputs, analysis_crs = NULL, preferred_crs = NULL) {
  # Boundary CRS selection retains sf metadata; zone crossings are permitted.
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
#' @param analysis_crs Either `NULL` or a projected coordinate reference system
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
  # Boundary validation precedes shared UTM selection; retain existing UTM networks.
  .validate_stream_reaches(stream_reaches)
  .select_utm_crs(list(stream_reaches), analysis_crs, sf::st_crs(stream_reaches))
}

#' Create stream segments for analytical modeling
#'
#' Transform a stream network to a projected analysis CRS and divide it into
#' approximately equal-length model segments.
#'
#' @param stream_reaches A nonempty `sf` object with one positive-length
#'   `LINESTRING` or `MULTILINESTRING` feature per stream reach, a defined CRS,
#'   and a unique, nonmissing character `reach_id` column. An optional
#'   reach-level `stream_width` column must have length units.
#' @param reach_spacing A scalar `units` length giving the maximum length of a
#'   model segment.
#' @param analysis_crs Either `NULL` or a UTM coordinate reference system in
#'   meters accepted by [sf::st_crs()]. When `NULL`, an existing UTM stream CRS
#'   is retained; other input CRSs are transformed to a local UTM CRS selected
#'   from their extent. Crossing a zone boundary does not trigger a warning.
#' @param stream_width Either `NULL` or a positive scalar `units` length. A
#'   supplied value is applied to every reach. When `NULL`, an existing
#'   reach-level `stream_width` attribute is retained; if that attribute is
#'   absent, every reach uses a default width of 1 m.
#'
#' @return A projected `sf` object with one line feature per stream segment.
#'   It contains `reach_id`, `reach_segment_id`, `represented_length`,
#'   `stream_width`, `model_point`, and the segment line geometry.
#'
#' @details
#' `model_point` is the along-line midpoint used as the constant-head
#' collocation location. Injection is distributed uniformly along the active
#' line geometry. Stream validation requires it to be a finite point in the
#' segment CRS and on the active line, and warns if it differs from the
#' along-line midpoint. Input objects are not modified.
#' `stream_width` regularizes responses on and very near a line element using
#' an effective minimum radius of half the width.
#' `well_diam` is reserved for pumping-well objects and is not accepted on
#' stream-reach or stream-segment inputs.
#'
#' @examples
#' stream_segments <- prep_stream_segments(
#'   example_stream_reaches,
#'   reach_spacing = units::set_units(100, "m")
#' )
#' stream_segments[c(
#'   "reach_id", "reach_segment_id", "represented_length", "stream_width"
#' )]
#'
#' @export
prep_stream_segments <- function(
    stream_reaches,
    reach_spacing,
    analysis_crs = NULL,
    stream_width = NULL) {

  # Validate geometry and physical dimensions before stripping length units.
  selected_crs <- .select_stream_analysis_crs(stream_reaches, analysis_crs)
  .validate_positive_length(reach_spacing, "reach_spacing")
  reserved_columns <- intersect(
    c("reach_segment_id", "represented_length", "model_point"),
    names(stream_reaches)
  )
  if (length(reserved_columns)) {
    stop("stream_reaches contains columns reserved for model discretization: ",
         paste(reserved_columns, collapse = ", "), ".")
  }
  checked_reaches <- .set_stream_width(stream_reaches, stream_width)
  reach_spacing_m <- .numeric_in_standard_units(reach_spacing, "m", "reach_spacing")
  result <- .prep_stream_segments(
    .prepare_numeric_columns(checked_reaches, c(stream_width = "m")),
    reach_spacing_m, selected_crs, NULL
  )
  # Reattach public length/width units; geometry keeps its UTM CRS.
  .restore_spatial_columns(result, c(represented_length = "m", stream_width = "m"))
}

.prep_stream_segments <- function(stream_reaches, reach_spacing_m,
                                  analysis_crs, stream_width_m = NULL) {
  # Validated sf geometry retains CRS; stripped spacing/widths and derived lengths are meters.
  prepared_reaches <- .transform_spatial_geometry(stream_reaches, analysis_crs)
  .discretize_stream_reaches(prepared_reaches, reach_spacing_m)
}

# Attach and validate the physical width used by line-element responses.
.set_stream_width <- function(stream_segments, stream_width = NULL) {
  if ("well_diam" %in% names(stream_segments)) {
    stop(
      "stream segments cannot contain well_diam; use stream_width for ",
      "finite-line radius regularization."
    )
  }

  if (!is.null(stream_width)) {
    check_dimensionality(stream_width, "m", "stream_width")

    if (length(stream_width) != 1L) {
      stop("stream_width must be a scalar length.")
    }

    stream_segments$stream_width <- rep(
      stream_width,
      length.out = nrow(stream_segments)
    )
  } else if (!("stream_width" %in% names(stream_segments))) {
    stream_segments$stream_width <- units::set_units(
      rep(1, nrow(stream_segments)),
      "m"
    )
  }

  check_dimensionality(
    stream_segments$stream_width,
    "m",
    "stream_segments$stream_width"
  )

  width_values <- as.numeric(stream_segments$stream_width)
  if (any(!is.finite(width_values)) || any(width_values <= 0)) {
    stop("stream_width must contain finite, positive values.")
  }

  widths_by_reach <- split(width_values, stream_segments$reach_id)
  if (any(vapply(
    widths_by_reach,
    function(reach_widths) any(reach_widths != reach_widths[[1]]),
    logical(1)
  ))) {
    stop("stream_width must be constant within each reach_id.")
  }

  stream_segments
}

# Validate the neutral stream-segment representation.
.validate_stream_segments <- function(stream_segments) {
  if (!inherits(stream_segments, "sf") || nrow(stream_segments) == 0) {
    stop("stream_segments must be a nonempty sf object.")
  }

  if ("well_diam" %in% names(stream_segments)) {
    stop(
      "stream_segments cannot contain well_diam; use stream_width for ",
      "finite-line radius regularization."
    )
  }

  required_columns <- c(
    "reach_id",
    "reach_segment_id",
    "represented_length",
    "stream_width",
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

  if (any(sf::st_is_empty(stream_segments)) ||
      any(!is.finite(sf::st_coordinates(stream_segments)[, 1:2, drop = FALSE]))) {
    stop("stream_segments must contain nonempty geometries with finite coordinates.")
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
    stream_segments$stream_width,
    "m",
    "stream_segments$stream_width"
  )
  if (any(!is.finite(as.numeric(stream_segments$represented_length))) ||
      any(as.numeric(stream_segments$represented_length) <= 0) ||
      any(!is.finite(as.numeric(stream_segments$stream_width))) ||
      any(as.numeric(stream_segments$stream_width) <= 0)) {
    stop(
      "stream_segments represented_length and stream_width must contain ",
      "finite, positive values."
    )
  }

  width_values <- as.numeric(stream_segments$stream_width)
  widths_by_reach <- split(width_values, stream_segments$reach_id)
  if (any(vapply(
    widths_by_reach,
    function(reach_widths) any(reach_widths != reach_widths[[1]]),
    logical(1)
  ))) {
    stop("stream_segments$stream_width must be constant within each reach_id.")
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

  if (!identical(
    sf::st_crs(stream_segments$model_point),
    sf::st_crs(stream_segments)
  )) {
    stop(
      "stream_segments$model_point must use the same CRS as the active ",
      "segment geometry."
    )
  }

  model_point_coordinates <- sf::st_coordinates(
    stream_segments$model_point
  )[, 1:2, drop = FALSE]
  if (any(!is.finite(model_point_coordinates))) {
    stop("stream_segments$model_point must contain finite coordinates.")
  }

  model_point_distance <- sf::st_distance(
    stream_segments$model_point,
    sf::st_geometry(stream_segments),
    by_element = TRUE
  )
  on_line_tolerance <- units::set_units(
    1e-7,
    "m",
    mode = "standard"
  )
  if (any(model_point_distance > on_line_tolerance)) {
    stop(
      "Each stream_segments$model_point must lie on its active segment ",
      "geometry."
    )
  }

  # Normalize coordinate offsets to meters for the numeric interpolation helper.
  segment_lengths <- sf::st_length(stream_segments)
  coordinate_unit_m <- as.numeric(units::set_units(
    units::set_units(1, units::deparse_unit(segment_lengths), mode = "standard"), "m"
  ))
  segment_midpoints <- sf::st_sfc(
    lapply(seq_len(nrow(stream_segments)), function(segment_index) {
      segment_coordinates_m <- sf::st_coordinates(
        sf::st_geometry(stream_segments)[[segment_index]]
      )[, 1:2, drop = FALSE] * coordinate_unit_m
      coordinate_differences_m <-
        segment_coordinates_m[-1, , drop = FALSE] -
        segment_coordinates_m[-nrow(segment_coordinates_m), , drop = FALSE]
      cumulative_length_m <- c(
        0,
        cumsum(sqrt(rowSums(coordinate_differences_m^2)))
      )

      sf::st_point(.point_along_linestring(
        segment_coordinates_m,
        cumulative_length_m,
        cumulative_length_m[[length(cumulative_length_m)]] / 2
      ) / coordinate_unit_m)
    }),
    crs = sf::st_crs(stream_segments)
  )
  midpoint_distance <- sf::st_distance(
    stream_segments$model_point,
    segment_midpoints,
    by_element = TRUE
  )
  if (any(midpoint_distance > on_line_tolerance)) {
    warning(
      "stream_segments$model_point should be the along-line midpoint of ",
      "its active segment geometry."
    )
  }

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
#'   `.validate_stream_reaches()`, typically the `stream_reaches` element
#'   returned by `.prepare_spatial_inputs()`.
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
#' @noRd
.discretize_stream_reaches <- function(stream_reaches, reach_spacing_m) {
  # Validated sf geometry is in shared UTM meters; spacing/length columns are numeric m.
  stream_reaches_xy <- sf::st_zm(
    stream_reaches,
    drop = TRUE,
    what = "ZM"
  )
  line_parts <- suppressWarnings(
    sf::st_cast(stream_reaches_xy, "LINESTRING")
  )

  # sf measurements have explicit units; normalize and strip once for numeric spacing.
  part_length_m <- as.numeric(units::set_units(sf::st_length(line_parts), "m"))
  model_reaches_per_part <- ceiling(part_length_m / reach_spacing_m)

  total_model_reaches <- sum(model_reaches_per_part)
  source_part_rows <- integer(total_model_reaches)
  model_geometries <- vector("list", total_model_reaches)
  model_points <- vector("list", total_model_reaches)
  output_index <- 0L

  # Split each input linestring part into approximately equal-length model
  # segments while retaining its original path through bends.
  for (part_index in seq_len(nrow(line_parts))) {
    coordinates_m <- sf::st_coordinates(
      sf::st_geometry(line_parts)[[part_index]]
    )[, 1:2, drop = FALSE]
    coordinate_differences_m <- coordinates_m[-1, , drop = FALSE] -
      coordinates_m[-nrow(coordinates_m), , drop = FALSE]
    coordinate_lengths_m <- sqrt(rowSums(coordinate_differences_m^2))
    cumulative_length_m <- c(0, cumsum(coordinate_lengths_m))
    break_distances_m <- seq(
      0,
      cumulative_length_m[[length(cumulative_length_m)]],
      length.out = model_reaches_per_part[[part_index]] + 1
    )

    # Construct each segment geometry and its along-line midpoint.
    for (segment_number in seq_len(model_reaches_per_part[[part_index]])) {
      output_index <- output_index + 1L
      start_distance_m <- break_distances_m[[segment_number]]
      end_distance_m <- break_distances_m[[segment_number + 1]]
      midpoint_distance_m <- mean(c(start_distance_m, end_distance_m))

      start_point_m <- .point_along_linestring(
        coordinates_m,
        cumulative_length_m,
        start_distance_m
      )
      end_point_m <- .point_along_linestring(
        coordinates_m,
        cumulative_length_m,
        end_distance_m
      )
      interior_coordinates_m <- coordinates_m[
        cumulative_length_m > start_distance_m &
          cumulative_length_m < end_distance_m,
        ,
        drop = FALSE
      ]
      segment_coordinates_m <- rbind(
        start_point_m,
        interior_coordinates_m,
        end_point_m
      )

      source_part_rows[[output_index]] <- part_index
      model_geometries[[output_index]] <- sf::st_linestring(
        segment_coordinates_m
      )
      model_points[[output_index]] <- sf::st_point(
        .point_along_linestring(
          coordinates_m,
          cumulative_length_m,
          midpoint_distance_m
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
  model_reaches$represented_length_m <- as.numeric(units::set_units(sf::st_length(model_reaches), "m"))
  model_reaches$model_point <- sf::st_sfc(
    model_points,
    crs = sf::st_crs(model_reaches)
  )

  geometry_column <- attr(model_reaches, "sf_column")
  identifying_columns <- c(
    "reach_id",
    "reach_segment_id",
    "represented_length_m"
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
    coordinates_m,
    cumulative_length_m,
    distance_m) {
  # Prepared numeric coordinates and along-line distances are meters.

  if (distance_m <= 0) {
    return(coordinates_m[1, ])
  }

  if (distance_m >= cumulative_length_m[[length(cumulative_length_m)]]) {
    return(coordinates_m[nrow(coordinates_m), ])
  }

  coordinate_indices <- seq_along(cumulative_length_m)
  segment_index <- max(which(
    cumulative_length_m <= distance_m &
      coordinate_indices < length(cumulative_length_m)
  ))
  segment_length_m <- cumulative_length_m[[segment_index + 1]] -
    cumulative_length_m[[segment_index]]
  fraction <- (distance_m - cumulative_length_m[[segment_index]]) /
    segment_length_m

  coordinates_m[segment_index, ] + fraction * (
    coordinates_m[segment_index + 1, ] - coordinates_m[segment_index, ]
  )
}

#' Prepare sample points along stream segments
#'
#' Create regularly distributed points within each discretized reach segment
#' for web-based stream-depletion apportionment, spatial inspection, or
#' water-level evaluation between segment model points.
#'
#' @param stream_segments A projected stream-segment `sf` object, normally
#'   returned by [prep_stream_segments()]. It must
#'   contain `reach_id`,
#'   `reach_segment_id`, and `represented_length` columns and one `LINESTRING`
#'   geometry per row.
#' @param sample_spacing A scalar `units` object with length dimensions giving
#'   the maximum stream length represented by each sample point. Must be
#'   finite and positive.
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
#' This is the same sampling calculation used by [prep_adf_stream_apportionment()].
#' Sample points are also distinct from finite-line quadrature points.
#'
#' Generating points does not change the stream discretization or injection
#' schedules. The constant-head condition is imposed only at `model_point`
#' locations, so water-level changes at sample points need not be zero.
#' To use sample points as observation wells, create an `observation_id`
#' column from `sample_point_id`.
#'
#' @examples
#' stream_segments <- prep_stream_segments(
#'   example_stream_reaches,
#'   reach_spacing = units::set_units(400, "m")
#' )
#' sample_points <- prep_stream_sample_points(
#'   stream_segments,
#'   sample_spacing = units::set_units(100, "m")
#' )
#' sample_points$observation_id <- sample_points$sample_point_id
#'
#' @export
prep_stream_sample_points <- function(stream_segments, sample_spacing) {

  # Validate the sampling contract before converting lengths to numeric meters.
  .validate_stream_sampling_inputs(stream_segments, sample_spacing)
  sample_spacing_m <- .numeric_in_standard_units(sample_spacing, "m", "sample_spacing")
  result <- .prep_stream_sample_points(
    .prepare_sampling_segments(stream_segments), sample_spacing_m
  )
  # Reattach public sampled-length units; geometry retains its shared UTM CRS.
  .restore_spatial_columns(result, c(sampled_length = "m"))
}

.prep_stream_sample_points <- function(stream_segments, sample_spacing_m) {
  # Validated shared-UTM sf geometry retains CRS; lengths/spacing are numeric meters.
  points_per_segment <- ceiling(stream_segments$represented_length_m / sample_spacing_m)
  total_points <- sum(points_per_segment)
  source_segment_rows <- integer(total_points)
  point_numbers <- integer(total_points)
  point_geometries <- vector("list", total_points)
  sampled_length_m <- numeric(total_points)
  output_index <- 0L

  # Place evenly spaced sampling points along each modeled stream segment.
  for (segment_index in seq_len(nrow(stream_segments))) {
    coordinates_m <- sf::st_coordinates(
      sf::st_geometry(stream_segments)[[segment_index]]
    )[, 1:2, drop = FALSE]
    coordinate_differences_m <- coordinates_m[-1, , drop = FALSE] -
      coordinates_m[-nrow(coordinates_m), , drop = FALSE]
    cumulative_length_m <- c(
      0,
      cumsum(sqrt(rowSums(coordinate_differences_m^2)))
    )
    segment_length_m <- cumulative_length_m[[length(cumulative_length_m)]]
    number_of_points <- points_per_segment[[segment_index]]
    interval_length_m <- segment_length_m / number_of_points
    point_distances_m <- (seq_len(number_of_points) - 0.5) * interval_length_m

    # Record each sample point and the stream length that it represents.
    for (point_number in seq_len(number_of_points)) {
      output_index <- output_index + 1L
      source_segment_rows[[output_index]] <- segment_index
      point_numbers[[output_index]] <- point_number
      sampled_length_m[[output_index]] <- interval_length_m
      point_geometries[[output_index]] <- sf::st_point(
        .point_along_linestring(
          coordinates_m,
          cumulative_length_m,
          point_distances_m[[point_number]]
        )
      )
    }
  }

  sf::st_sf(
    reach_id = stream_segments$reach_id[source_segment_rows],
    reach_segment_id =
      stream_segments$reach_segment_id[source_segment_rows],
    sample_point_id = paste0(
      stream_segments$reach_segment_id[source_segment_rows],
      "_point_",
      point_numbers
    ),
    sampled_length_m = sampled_length_m,
    geometry = sf::st_sfc(
      point_geometries,
      crs = sf::st_crs(stream_segments)
    )
  )
}
