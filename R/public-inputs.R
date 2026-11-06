# Public-boundary validation and preparation for standard numeric internals.

.numeric_in_standard_units <- function(x, standard_unit, variable_name) {
  # Dimensions were checked by the boundary validator; normalize, then strip units.
  as.numeric(units::set_units(x, standard_unit, mode = "standard"))
}

.prepare_numeric_columns <- function(x, column_units) {
  # Units are checked before stripping; numeric column names include their units.
  suffixes <- c("m" = "m", "m/day" = "m_per_day",
                "m^3/day" = "m3_per_day", "days" = "day")
  output_units <- list()
  for (column in names(column_units)) {
    if (!column %in% names(x)) next
    standard_unit <- column_units[[column]]
    numeric_column <- paste0(column, "_", suffixes[[standard_unit]])
    if (numeric_column %in% names(x)) {
      stop("Input column conflicts with prepared numeric column: ", numeric_column)
    }
    output_units[[column]] <- units::deparse_unit(x[[column]])
    x[[numeric_column]] <- .numeric_in_standard_units(
      x[[column]], standard_unit, column
    )
    x[[column]] <- NULL
  }
  attr(x, "isw_input_units") <- output_units
  x
}

.prepare_numeric_pumping_wells <- function(pumping_wells, analysis_crs = NULL) {
  # Validate unit-bearing wells; keep CRS metadata and normalize physical columns.
  .validate_pumping_wells(pumping_wells)
  if (!is.null(analysis_crs)) {
    pumping_wells <- .transform_spatial_geometry(pumping_wells, analysis_crs)
  }
  if (!"well_diam" %in% names(pumping_wells)) {
    pumping_wells$well_diam <- units::set_units(0, "m")
  }
  .prepare_numeric_columns(
    pumping_wells, c(K = "m/day", D = "m", well_diam = "m")
  )
}

.prepare_numeric_segments <- function(stream_segments) {
  # Validate lengths/widths and geometry; coordinates retain explicit CRS units.
  .validate_stream_segments(stream_segments)
  .prepare_sampling_segments(stream_segments)
}

.prepare_sampling_segments <- function(stream_segments) {
  # Checked sf geometry retains all CRS columns; normalize measurements to meters.
  analysis_crs <- .select_utm_crs(list(stream_segments), preferred_crs = sf::st_crs(stream_segments))
  stream_segments <- .transform_spatial_geometry(stream_segments, analysis_crs)
  stream_segments <- .prepare_numeric_columns(
    stream_segments, c(represented_length = "m", stream_width = "m")
  )
  stream_segments$represented_length_m <- as.numeric(units::set_units(sf::st_length(stream_segments), "m"))
  stream_segments
}

.restore_spatial_columns <- function(x, column_units) {
  # Reattach physical units at public exits; spatial geometry and CRS remain intact.
  suffixes <- c("m" = "m")
  for (column in names(column_units)) {
    numeric_column <- paste0(column, "_", suffixes[[column_units[[column]]]])
    if (!numeric_column %in% names(x)) next
    x[[numeric_column]] <- units::set_units(x[[numeric_column]], column_units[[column]], mode = "standard")
    names(x)[match(numeric_column, names(x))] <- column
  }
  attr(x, "isw_input_units") <- NULL
  x
}

.prepare_numeric_apportionment <- function(stream_apportionment, pumping_wells) {
  # The public boundary checked this table; strip distances after meter conversion.
  .prepare_numeric_columns(
    stream_apportionment, c(pump_to_reach_distance = "m")
  )
}

.time_in_days <- function(x, time_origin = NULL) {
  # Convert checked Date/time inputs to numeric days; Date offsets share an origin.
  if (inherits(x, "Date")) return(as.numeric(x - time_origin))
  .numeric_in_standard_units(x, "days", "time")
}

.prepare_numeric_model_times <- function(pumping_schedules, evaluation_times,
                                         injection_times = NULL) {
  # Validate times, normalize/strip to numeric days, and retain output time metadata.
  .validate_evaluation_times(evaluation_times, pumping_schedules$t)
  .validate_injection_times(injection_times, pumping_schedules$t)
  normalized <- .normalize_time_inputs(pumping_schedules$t, evaluation_times)
  list(
    t_day = normalized$schedule_time_day,
    evaluation_time_day = normalized$target_time_day,
    injection_time_day = if (!is.null(injection_times)) {
      .time_in_days(injection_times, normalized$time_origin)
    } else NULL,
    time_origin = normalized$time_origin,
    output_schedule_times = pumping_schedules$t,
    output_evaluation_times = normalized$output_target_times
  )
}

.prepare_numeric_schedule <- function(pumping_schedules, pumping_wells, times) {
  # The public boundary checked rates/times; normalize to numeric m^3/day and days.
  pumping_schedules <- .prepare_numeric_columns(
    pumping_schedules,
    stats::setNames(rep("m^3/day", nrow(pumping_wells)), pumping_wells$pump_id)
  )
  if ("t_day" %in% names(pumping_schedules)) {
    stop("Input column conflicts with prepared numeric column: t_day")
  }
  pumping_schedules$t <- NULL
  pumping_schedules$t_day <- times$t_day
  attr(pumping_schedules, "isw_time_metadata") <- times[
    c("time_origin", "output_schedule_times", "output_evaluation_times")
  ]
  # Prepare reusable numeric events/pairs once, independently of output time metadata.
  events <- .get_pumping_rate_changes(pumping_schedules, pumping_wells$pump_id)
  attr(pumping_schedules, "isw_pumping_events") <- events
  attr(pumping_schedules, "isw_evaluation_time_day") <- times$evaluation_time_day
  attr(pumping_schedules, "isw_pumping_response_times") <- .get_pumping_response_times(
    events, times$evaluation_time_day
  )
  pumping_schedules
}

.restore_adf_output <- function(x, times, rate_units) {
  # Restore public times and rate units from numeric day and m^3/day columns.
  if (!all(c("evaluation_time_day", "stream_depletion_rate_m3_per_day") %in% names(x))) return(x)
  x$evaluation_time_day <- .restore_time_values(
    x$evaluation_time_day, times$output_evaluation_times, times$time_origin
  )
  names(x)[names(x) == "evaluation_time_day"] <- "evaluation_time"
  x$stream_depletion_rate_m3_per_day <- units::set_units(
    units::set_units(x$stream_depletion_rate_m3_per_day, "m^3/day"), rate_units, mode = "standard"
  )
  names(x)[names(x) == "stream_depletion_rate_m3_per_day"] <- "stream_depletion_rate"
  x
}

.restore_injection_output <- function(x, times, rate_units) {
  # Restore public interval times and rates; numeric internals use day and m^3/day.
  if (!"interval_start_day" %in% names(x)) return(x)
  missing_time_day <- attr(x, "isw_missing_time_day")
  if (length(missing_time_day)) {
    labels <- trimws(format(.restore_time_values(
      missing_time_day, times$output_schedule_times, times$time_origin
    )))
    warning("adf_stream_depletion is missing ", length(labels), " required schedule ",
            if (length(labels) == 1L) "boundary" else "boundaries", " (",
            paste(utils::head(labels, 5L), collapse = ", "),
            if (length(labels) > 5L) ", ..." else "",
            "). Depletion at missing times will be calculated internally.", call. = FALSE)
  }
  for (column in c("interval_start", "interval_end")) {
    numeric_column <- paste0(column, "_day")
    x[[numeric_column]] <- .restore_time_values(
      x[[numeric_column]], times$output_schedule_times, times$time_origin
    )
    names(x)[names(x) == numeric_column] <- column
  }
  x$injection_rate_m3_per_day <- units::set_units(
    units::set_units(x$injection_rate_m3_per_day, "m^3/day"), rate_units, mode = "standard"
  )
  names(x)[names(x) == "injection_rate_m3_per_day"] <- "injection_rate"
  if ("boundary_residual_m" %in% names(x)) {
    x$boundary_residual_m <- units::set_units(x$boundary_residual_m, "m")
    names(x)[names(x) == "boundary_residual_m"] <- "boundary_residual"
  }
  attr(x, "isw_missing_time_day") <- NULL
  x
}

.prepare_numeric_depletion <- function(stream_depletion, pumping_wells,
                                       pumping_schedules, stream_apportionment,
                                       time_origin) {
  # Validate supplied depletion before converting rates and evaluation times.
  .validate_apportioned_stream_depletion(
    stream_depletion, pumping_wells, pumping_schedules, stream_apportionment
  )
  stream_depletion <- .prepare_numeric_columns(
    stream_depletion, c(stream_depletion_rate = "m^3/day")
  )
  stream_depletion$evaluation_time_day <- .time_in_days(
    stream_depletion$evaluation_time, time_origin
  )
  stream_depletion$evaluation_time <- NULL
  stream_depletion
}

.prepare_numeric_injection <- function(stream_injection_schedule, pumping_wells,
                                       pumping_schedules, stream_segments,
                                       evaluation_times, time_origin) {
  # Validate supplied schedule coverage and units before stripping rates/times.
  .validate_stream_injection_schedule(
    stream_injection_schedule, pumping_wells, pumping_schedules,
    stream_segments, evaluation_times
  )
  stream_injection_schedule <- .prepare_numeric_columns(
    stream_injection_schedule,
    c(injection_rate = "m^3/day", boundary_residual = "m")
  )
  stream_injection_schedule$interval_start_day <- .time_in_days(
    stream_injection_schedule$interval_start, time_origin
  )
  stream_injection_schedule$interval_end_day <- .time_in_days(
    stream_injection_schedule$interval_end, time_origin
  )
  stream_injection_schedule$interval_start <- NULL
  stream_injection_schedule$interval_end <- NULL
  stream_injection_schedule
}

.validate_positive_length <- function(x, variable_name) {
  # Validate physical dimensions and scalar controls while units are explicit.
  check_dimensionality(x, "m", variable_name)
  if (length(x) != 1L || !is.finite(as.numeric(x)) || as.numeric(x) <= 0) {
    stop(variable_name, " must be a finite, positive scalar length.")
  }
}

.validate_quadrature_order <- function(quadrature_order) {
  # Dimensionless integer control, checked before calling numeric internals.
  if (!is.numeric(quadrature_order) || inherits(quadrature_order, "units") ||
      length(quadrature_order) != 1L ||
      !is.finite(quadrature_order) || quadrature_order < 1 ||
      quadrature_order != floor(quadrature_order)) {
    stop("quadrature_order must be a positive integer.")
  }
}

.prepare_analytical_inputs <- function(length_inputs, K, D, V, t, well_diam = NULL) {
  # Check dimensions/values before converting lengths to m, K to m/day, t to day.
  for (variable_name in names(length_inputs)) {
    check_dimensionality(length_inputs[[variable_name]], "m", variable_name)
  }
  check_dimensionality(K, "m/day", "K")
  check_dimensionality(D, "m", "D")
  check_dimensionality(t, "days", "t")
  if (!is.numeric(V) || inherits(V, "units") || length(V) == 0L ||
      any(!is.finite(V)) || any(V <= 0 | V > 1)) {
    stop("V must contain dimensionless values greater than 0 and at most 1.")
  }
  physical_inputs <- c(length_inputs, list(K = K, D = D, t = t))
  if (!is.null(well_diam)) {
    check_dimensionality(well_diam, "m", "well_diam")
    physical_inputs$well_diam <- well_diam
  }
  input_lengths <- lengths(c(physical_inputs, list(V = V)))
  if (any(input_lengths == 0L) ||
      any(!input_lengths %in% c(1L, max(input_lengths)))) {
    stop("Analytical inputs must have length one or a common length.")
  }
  if (any(vapply(physical_inputs, function(x) {
    any(!is.finite(as.numeric(x)))
  }, logical(1)))) stop("Analytical inputs must contain finite values.")
  if (any(as.numeric(K) <= 0) || any(as.numeric(D) <= 0) ||
      any(as.numeric(t) <= 0)) stop("K, D, and t must be positive.")
  if (!is.null(well_diam) && any(as.numeric(well_diam) < 0)) {
    stop("well_diam must be nonnegative.")
  }
  if ("distance" %in% names(length_inputs) && any(as.numeric(length_inputs$distance) < 0)) {
    stop("distance must be nonnegative.")
  }
  prepared <- stats::setNames(lapply(length_inputs, function(x) {
    .numeric_in_standard_units(x, "m", "distance")
  }), paste0(names(length_inputs), "_m"))
  prepared$K_m_per_day <- .numeric_in_standard_units(K, "m/day", "K")
  prepared$D_m <- .numeric_in_standard_units(D, "m", "D")
  prepared$V <- V
  prepared$t_day <- .numeric_in_standard_units(t, "days", "t")
  if (!is.null(well_diam)) {
    prepared$well_diam_m <- .numeric_in_standard_units(well_diam, "m", "well_diam")
  }
  # Preserve the existing public response units for reattachment at the boundary.
  prepared$output_response_units <- units::deparse_unit(1 / (K * D))
  prepared
}

.validate_stream_sampling_inputs <- function(stream_segments, sample_spacing) {
  # Boundary checks retain the existing sampling-specific geometry/unit contract.

  if (!inherits(stream_segments, "sf")) {
    stop("stream_segments must be an sf object.")
  }

  required_columns <- c(
    "reach_id",
    "reach_segment_id",
    "represented_length"
  )
  missing_columns <- setdiff(required_columns, names(stream_segments))

  if (length(missing_columns) > 0) {
    stop(
      "stream_segments is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  if (nrow(stream_segments) == 0) {
    stop("stream_segments must contain at least one feature.")
  }

  if (is.na(sf::st_crs(stream_segments)) ||
      !grepl("^PROJCRS\\[", sf::st_crs(stream_segments)$wkt)) {
    stop("stream_segments must use a projected CRS.")
  }

  geometry_types <- as.character(
    sf::st_geometry_type(stream_segments, by_geometry = TRUE)
  )

  if (!all(geometry_types == "LINESTRING")) {
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
      any(trimws(stream_segments$reach_segment_id) == "")) {
    stop(
      "stream_segments$reach_id and stream_segments$reach_segment_id must be ",
      "nonmissing, nonempty character vectors."
    )
  }

  if (anyDuplicated(stream_segments$reach_segment_id) > 0) {
    stop("stream_segments$reach_segment_id values must be unique.")
  }

  check_dimensionality(
    stream_segments$represented_length,
    desired_units = "m",
    variable_name = "stream_segments$represented_length"
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

  segment_lengths <- sf::st_length(stream_segments)

  if (any(!is.finite(as.numeric(segment_lengths))) ||
      any(as.numeric(segment_lengths) <= 0)) {
    stop("stream_segments must contain finite, positive geometry lengths.")
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

  invisible(stream_segments)
}

.restore_aquifer_output <- function(x, times, length_units) {
  # Reattach caller time/length units only after numeric m/day calculations finish.
  if (!"evaluation_time_day" %in% names(x)) return(x)
  x$evaluation_time_day <- .restore_time_values(
    x$evaluation_time_day, times$output_evaluation_times, times$time_origin
  )
  names(x)[names(x) == "evaluation_time_day"] <- "evaluation_time"
  for (column in c("pumping_drawdown", "stream_recovery", "water_level_change")) {
    numeric_column <- paste0(column, "_m")
    x[[numeric_column]] <- units::set_units(
      units::set_units(x[[numeric_column]], "m"), length_units, mode = "standard"
    )
    names(x)[names(x) == numeric_column] <- column
  }
  x
}
