# Read a valid quadrature order recorded on a constant-head schedule.
.get_metadata_quadrature_order <- function(stream_injection_schedule) {
  metadata <- attr(
    stream_injection_schedule,
    "isw_schedule_metadata",
    exact = TRUE
  )

  if (!is.list(metadata) ||
      !identical(metadata$injection_method, "constant_head")) {
    return(NULL)
  }

  quadrature_order <- metadata$quadrature_order
  if (length(quadrature_order) != 1L || is.na(quadrature_order) ||
      !is.finite(quadrature_order) || quadrature_order < 1 ||
      quadrature_order != as.integer(quadrature_order)) {
    return(NULL)
  }

  as.integer(quadrature_order)
}

# Reuse schedule quadrature metadata or report an explicit mismatch.
.resolve_schedule_quadrature_order <- function(
    stream_injection_schedule,
    quadrature_order,
    quadrature_order_supplied) {

  schedule_order <- .get_metadata_quadrature_order(
    stream_injection_schedule
  )
  if (is.null(schedule_order)) {
    return(quadrature_order)
  }

  if (!quadrature_order_supplied) {
    return(schedule_order)
  }

  supplied_order_is_valid <- length(quadrature_order) == 1L &&
    !is.na(quadrature_order) && is.finite(quadrature_order) &&
    quadrature_order >= 1 && quadrature_order == as.integer(quadrature_order)
  if (supplied_order_is_valid &&
      as.integer(quadrature_order) != schedule_order) {
    warning(
      "quadrature_order = ", as.integer(quadrature_order),
      " differs from the value used to construct the constant-head ",
      "stream_injection_schedule (", schedule_order, ").",
      call. = FALSE
    )
  }

  quadrature_order
}

# Validate apportioned stream-depletion results for drawdown calculations.
.validate_apportioned_stream_depletion <- function(
    stream_depletion,
    pumping_wells,
    pumping_schedules,
    stream_apportionment) {

  if (!is.data.frame(stream_depletion) || nrow(stream_depletion) == 0) {
    stop("stream_depletion must be a nonempty data frame or tibble.")
  }

  required_columns <- c(
    "pump_id",
    "evaluation_time",
    "reach_id",
    "reach_segment_id",
    "stream_depletion_rate"
  )
  missing_columns <- setdiff(required_columns, names(stream_depletion))

  if (length(missing_columns) > 0) {
    stop(
      "stream_depletion is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_depletion_apportionment(
    stream_apportionment,
    pumping_wells
  )

  evaluation_times <- sort(unique(stream_depletion$evaluation_time))
  .validate_evaluation_times(evaluation_times, pumping_schedules$t)

  if (!is.character(stream_depletion$pump_id) ||
      !is.character(stream_depletion$reach_id) ||
      !is.character(stream_depletion$reach_segment_id) ||
      anyNA(stream_depletion$pump_id) ||
      anyNA(stream_depletion$reach_id) ||
      anyNA(stream_depletion$reach_segment_id)) {
    stop(
      "stream_depletion identifiers must be nonmissing character vectors."
    )
  }

  check_dimensionality(
    stream_depletion$stream_depletion_rate,
    desired_units = "m^3/s",
    variable_name = "stream_depletion$stream_depletion_rate"
  )

  if (any(!is.finite(as.numeric(
    stream_depletion$stream_depletion_rate
  )))) {
    stop("stream_depletion$stream_depletion_rate must contain finite values.")
  }

  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  evaluation_day_values <- as.numeric(normalized_times$normalized_target_times)
  row_evaluation_days <- if (inherits(evaluation_times, "Date")) {
    as.numeric(stream_depletion$evaluation_time - pumping_schedules$t[[1]])
  } else {
    as.numeric(units::set_units(
      stream_depletion$evaluation_time,
      "days",
      mode = "standard"
    ))
  }

  row_keys <- paste(
    stream_depletion$pump_id,
    stream_depletion$reach_segment_id,
    row_evaluation_days,
    sep = "\r"
  )

  if (anyDuplicated(row_keys) > 0) {
    stop(
      "stream_depletion must contain one row per pump_id, ",
      "reach_segment_id, and evaluation_time combination."
    )
  }

  apportionment_table <- sf::st_drop_geometry(stream_apportionment)
  expected_keys <- character()
  expected_reaches <- character()

  # Build the complete set of pump, segment, and evaluation-time combinations
  # required in the supplied depletion table.
  for (pump_id in pumping_wells$pump_id) {
    pump_segments <- apportionment_table[
      apportionment_table$pump_id == pump_id,
      ,
      drop = FALSE
    ]

    for (evaluation_day in evaluation_day_values) {
      expected_keys <- c(
        expected_keys,
        paste(
          pump_id,
          pump_segments$reach_segment_id,
          evaluation_day,
          sep = "\r"
        )
      )
      expected_reaches <- c(expected_reaches, pump_segments$reach_id)
    }
  }

  matched_rows <- match(expected_keys, row_keys)

  if (anyNA(matched_rows) || length(row_keys) != length(expected_keys)) {
    stop(
      "stream_depletion must contain every pump, reach-segment, and ",
      "evaluation-time combination in stream_apportionment."
    )
  }

  if (!identical(
    stream_depletion$reach_id[matched_rows],
    expected_reaches
  )) {
    stop(
      "stream_depletion$reach_id must match each reach_segment_id in ",
      "stream_apportionment."
    )
  }

  stream_depletion
}

# Validate a user-supplied stream-injection schedule.
.validate_stream_injection_schedule <- function(
    stream_injection_schedule,
    pumping_wells,
    pumping_schedules,
    stream_segments,
    evaluation_times = NULL) {

  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_segments(stream_segments)
  .validate_evaluation_times(evaluation_times, pumping_schedules$t)

  if (!is.data.frame(stream_injection_schedule)) {
    stop("stream_injection_schedule must be a data frame or tibble.")
  }

  required_columns <- c(
    "pump_id",
    "reach_id",
    "reach_segment_id",
    "interval_start",
    "interval_end",
    "injection_rate"
  )
  missing_columns <- setdiff(
    required_columns,
    names(stream_injection_schedule)
  )

  if (length(missing_columns) > 0) {
    stop(
      "stream_injection_schedule is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  normalized_evaluations <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  schedule_start <- normalized_evaluations$normalized_schedule_times[[1]]
  final_evaluation <- max(normalized_evaluations$normalized_target_times)

  if (nrow(stream_injection_schedule) == 0) {
    if (final_evaluation > schedule_start) {
      stop(
        "stream_injection_schedule cannot be empty when evaluation times ",
        "extend beyond the first pumping-schedule time."
      )
    }
    return(stream_injection_schedule)
  }

  if (!is.character(stream_injection_schedule$pump_id) ||
      !is.character(stream_injection_schedule$reach_id) ||
      !is.character(stream_injection_schedule$reach_segment_id) ||
      anyNA(stream_injection_schedule$pump_id) ||
      anyNA(stream_injection_schedule$reach_id) ||
      anyNA(stream_injection_schedule$reach_segment_id)) {
    stop(
      "stream_injection_schedule identifiers must be nonmissing character ",
      "vectors."
    )
  }

  check_dimensionality(
    stream_injection_schedule$injection_rate,
    desired_units = "m^3/s",
    variable_name = "stream_injection_schedule$injection_rate"
  )

  if (any(!is.finite(as.numeric(
    stream_injection_schedule$injection_rate
  )))) {
    stop("stream_injection_schedule$injection_rate must be finite.")
  }

  schedule_is_date <- inherits(pumping_schedules$t, "Date")
  start_is_date <- inherits(stream_injection_schedule$interval_start, "Date")
  end_is_date <- inherits(stream_injection_schedule$interval_end, "Date")

  if (start_is_date != schedule_is_date || end_is_date != schedule_is_date) {
    stop(
      "stream_injection_schedule interval times and pumping_schedules$t ",
      "must both use Date values or both use units time values."
    )
  }

  if (!schedule_is_date &&
      (!inherits(stream_injection_schedule$interval_start, "units") ||
        !inherits(stream_injection_schedule$interval_end, "units"))) {
    stop(
      "stream_injection_schedule interval times must have time units."
    )
  }

  if (schedule_is_date) {
    interval_starts <- units::set_units(
      as.numeric(
        stream_injection_schedule$interval_start - pumping_schedules$t[[1]]
      ),
      "days",
      mode = "standard"
    )
    interval_ends <- units::set_units(
      as.numeric(
        stream_injection_schedule$interval_end - pumping_schedules$t[[1]]
      ),
      "days",
      mode = "standard"
    )
  } else {
    interval_starts <- units::set_units(
      stream_injection_schedule$interval_start,
      "days",
      mode = "standard"
    )
    interval_ends <- units::set_units(
      stream_injection_schedule$interval_end,
      "days",
      mode = "standard"
    )
  }

  if (anyNA(interval_starts) || anyNA(interval_ends) ||
      any(!is.finite(as.numeric(interval_starts))) ||
      any(!is.finite(as.numeric(interval_ends))) ||
      any(interval_starts >= interval_ends)) {
    stop(
      "stream_injection_schedule must contain finite intervals with ",
      "interval_start before interval_end."
    )
  }

  number_of_segments <- nrow(stream_segments)
  segment_rows <- rep(
    seq_len(number_of_segments),
    times = nrow(pumping_wells)
  )
  expected_keys <- paste(
    rep(pumping_wells$pump_id, each = number_of_segments),
    stream_segments$reach_segment_id[segment_rows],
    sep = "\r"
  )
  schedule_keys <- paste(
    stream_injection_schedule$pump_id,
    stream_injection_schedule$reach_segment_id,
    sep = "\r"
  )

  if (!setequal(unique(schedule_keys), unique(expected_keys))) {
    stop(
      "stream_injection_schedule must contain every pumping-well and stream-",
      "segment combination, with no additional combinations."
    )
  }

  expected_reaches <- stats::setNames(
    stream_segments$reach_id[segment_rows],
    expected_keys
  )

  # Validate continuous schedule coverage for every pump-segment pair.
  for (pair_key in unique(expected_keys)) {
    pair_rows <- which(schedule_keys == pair_key)
    pair_rows <- pair_rows[order(as.numeric(interval_starts[pair_rows]))]

    if (any(
      stream_injection_schedule$reach_id[pair_rows] !=
        expected_reaches[[pair_key]]
    )) {
      stop(
        "stream_injection_schedule$reach_id must match each reach_segment_id ",
        "in stream_segments."
      )
    }

    if (interval_starts[pair_rows[[1]]] != schedule_start ||
        interval_ends[pair_rows[[length(pair_rows)]]] < final_evaluation ||
        (length(pair_rows) > 1 && any(
          interval_starts[pair_rows[-1]] !=
            interval_ends[pair_rows[-length(pair_rows)]]
        ))) {
      stop(
        "stream_injection_schedule must provide continuous intervals from ",
        "the first pumping-schedule time through the final evaluation time ",
        "for every pump and reach segment."
      )
    }
  }

  stream_injection_schedule
}

# Build the internal stream-injection time grid.
.get_stream_injection_times <- function(
    pumping_schedules,
    evaluation_times = NULL,
    injection_times = NULL) {

  .validate_evaluation_times(evaluation_times, pumping_schedules$t)
  .validate_injection_times(injection_times, pumping_schedules$t)

  normalized_evaluations <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  evaluation_days <- normalized_evaluations$normalized_target_times
  pumping_days <- normalized_evaluations$normalized_schedule_times

  if (is.null(injection_times)) {
    refinement_days <- evaluation_days[0]
  } else {
    refinement_days <- .normalize_time_inputs(
      pumping_schedules$t,
      injection_times
    )$normalized_target_times
  }

  final_evaluation_day <- max(evaluation_days)
  boundary_day_values <- sort(unique(c(
    as.numeric(pumping_days[pumping_days <= final_evaluation_day]),
    as.numeric(evaluation_days),
    as.numeric(refinement_days[refinement_days <= final_evaluation_day])
  )))
  boundary_days <- units::set_units(
    boundary_day_values,
    "days",
    mode = "standard"
  )

  if (inherits(pumping_schedules$t, "Date")) {
    schedule_boundaries <- pumping_schedules$t[[1]] + boundary_day_values
  } else {
    schedule_boundaries <- units::set_units(
      boundary_days,
      units::deparse_unit(pumping_schedules$t),
      mode = "standard"
    )
  }

  list(
    schedule_boundaries = schedule_boundaries,
    boundary_days = boundary_days
  )
}

# Calculate interval-average stream-injection rates.
.get_interval_average_injection_schedule <- function(
    depletion_at_schedule_boundaries,
    pumping_schedules) {

  boundary_times <- sort(unique(
    depletion_at_schedule_boundaries$schedule_boundary
  ))
  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    boundary_times
  )
  schedule_start <- normalized_times$normalized_schedule_times[[1]]
  boundary_days <- normalized_times$normalized_target_times
  prepend_initial_zero <- boundary_days[[1]] > schedule_start

  if (prepend_initial_zero) {
    time_nodes <- c(schedule_start, boundary_days)
  } else {
    time_nodes <- boundary_days
  }

  result_pump_ids <- character()
  result_reach_ids <- character()
  result_segment_ids <- character()
  result_interval_starts <- time_nodes[0]
  result_interval_ends <- time_nodes[0]
  result_injection_rates <-
    depletion_at_schedule_boundaries$stream_depletion_rate[0]
  pair_keys <- unique(paste(
    depletion_at_schedule_boundaries$pump_id,
    depletion_at_schedule_boundaries$reach_segment_id,
    sep = "\r"
  ))

  # Convert boundary depletion rates into interval-average rates separately
  # for every pump-segment pair.
  for (pair_key in pair_keys) {
    key_parts <- strsplit(pair_key, "\r", fixed = TRUE)[[1]]
    pump_id <- key_parts[[1]]
    reach_segment_id <- key_parts[[2]]
    # Select the boundary rows for this pump-segment pair and place them in
    # chronological order.
    pair_rows <- which(
      depletion_at_schedule_boundaries$pump_id == pump_id &
        depletion_at_schedule_boundaries$reach_segment_id == reach_segment_id
    )
    pair_rows <- pair_rows[order(as.numeric(
      depletion_at_schedule_boundaries$schedule_boundary[pair_rows]
    ))]
    # Extract the depletion rate at each time boundary for this pump-segment
    # pair.
    endpoint_rates <-
      depletion_at_schedule_boundaries$stream_depletion_rate[pair_rows]

    if (prepend_initial_zero) {
      endpoint_rates <- c(endpoint_rates[[1]] * 0, endpoint_rates)
    }

    if (length(endpoint_rates) < 2) {
      next
    }

    # Average each pair of adjacent endpoint rates to obtain the constant rate
    # assigned to the interval between them.
    interval_rates <- (
      endpoint_rates[-length(endpoint_rates)] + endpoint_rates[-1]
    ) / 2
    number_of_intervals <- length(interval_rates)

    result_pump_ids <- c(
      result_pump_ids,
      rep(pump_id, number_of_intervals)
    )
    result_reach_ids <- c(
      result_reach_ids,
      rep(
        depletion_at_schedule_boundaries$reach_id[pair_rows[[1]]],
        number_of_intervals
      )
    )
    result_segment_ids <- c(
      result_segment_ids,
      rep(reach_segment_id, number_of_intervals)
    )
    result_interval_starts <- c(
      result_interval_starts,
      time_nodes[-length(time_nodes)]
    )
    result_interval_ends <- c(result_interval_ends, time_nodes[-1])
    result_injection_rates <- c(result_injection_rates, interval_rates)
  }

  tibble::tibble(
    pump_id = result_pump_ids,
    reach_id = result_reach_ids,
    reach_segment_id = result_segment_ids,
    interval_start = result_interval_starts,
    interval_end = result_interval_ends,
    injection_rate = result_injection_rates
  )
}

# Convert an interval injection schedule to nonzero rate-change events.
.get_injection_rate_changes <- function(injection_schedule) {

  event_pumps <- character()
  event_reaches <- character()
  event_segments <- character()
  event_times <- injection_schedule$interval_start[0]
  event_rate_changes <- injection_schedule$injection_rate[0]
  pair_keys <- unique(paste(
    injection_schedule$pump_id,
    injection_schedule$reach_segment_id,
    sep = "\r"
  ))

  # Convert each pump-segment interval schedule into nonzero rate-change events.
  for (pair_key in pair_keys) {
    key_parts <- strsplit(pair_key, "\r", fixed = TRUE)[[1]]
    pump_id <- key_parts[[1]]
    reach_segment_id <- key_parts[[2]]
    pair_rows <- which(
      injection_schedule$pump_id == pump_id &
        injection_schedule$reach_segment_id == reach_segment_id
    )
    pair_rows <- pair_rows[order(as.numeric(
      injection_schedule$interval_start[pair_rows]
    ))]
    # Public schedules use positive rates for water moving from the stream to
    # the aquifer. Response kernels use the pumping convention, so aquifer
    # injection enters the superposition calculation with the opposite sign.
    interval_rates <- -injection_schedule$injection_rate[pair_rows]
    previous_rates <- c(
      interval_rates[[1]] * 0,
      interval_rates[-length(interval_rates)]
    )
    rate_changes <- interval_rates - previous_rates
    nonzero_change <- as.numeric(rate_changes) != 0

    event_pumps <- c(event_pumps, rep(pump_id, sum(nonzero_change)))
    event_reaches <- c(
      event_reaches,
      rep(injection_schedule$reach_id[pair_rows[[1]]], sum(nonzero_change))
    )
    event_segments <- c(
      event_segments,
      rep(reach_segment_id, sum(nonzero_change))
    )
    event_times <- c(
      event_times,
      injection_schedule$interval_start[pair_rows][nonzero_change]
    )
    event_rate_changes <- c(
      event_rate_changes,
      rate_changes[nonzero_change]
    )
  }

  tibble::tibble(
    pump_id = event_pumps,
    reach_id = event_reaches,
    reach_segment_id = event_segments,
    injection_time = event_times,
    injection_rate_change = event_rate_changes
  )
}

#' Calculate interval-average stream-injection rate changes
#'
#' Convert stream-depletion rates evaluated at discrete times into piecewise
#' constant injection rates for aquifer-response superposition.
#'
#' @param stream_depletion Validated output from
#'   [model_adf_stream_depletion()].
#' @param pumping_schedules A validated pumping-schedule object.
#'
#' @return A tibble with one row per nonzero change in interval-average
#'   injection rate. Columns are `pump_id`, `reach_id`, `reach_segment_id`,
#'   `injection_time`, and `injection_rate_change`. Times use internal units of
#'   days and rates retain the stream-depletion rate units.
#'
#' @details
#' Stream depletion and public injection schedules are positive when water
#' enters the aquifer from a stream. This helper converts schedule rates to the
#' opposite sign so the same response kernel can be used for pumping and
#' injection.
#'
#' The first pumping-schedule time defines the initial time boundary. When the
#' first evaluation occurs later, stream depletion at that initial boundary is
#' assumed to be zero. The public injection rate in each interval is the
#' positive arithmetic mean of stream depletion at the beginning and end of
#' that interval. Consecutive rates are converted to signed aquifer-response
#' events; exact zero changes are omitted.
#'
#' @noRd
.get_interval_average_injection_rate_changes <- function(
    stream_depletion,
    pumping_schedules) {
  depletion_at_schedule_boundaries <- stream_depletion
  names(depletion_at_schedule_boundaries)[
    names(depletion_at_schedule_boundaries) == "evaluation_time"
  ] <- "schedule_boundary"

  .get_injection_rate_changes(
    .get_interval_average_injection_schedule(
      depletion_at_schedule_boundaries,
      pumping_schedules
    )
  )
}

#' Construct a stream-injection schedule
#'
#' Calculate piecewise-constant stream injection using ADF depletion or an
#' endpoint-collocated constant-head boundary.
#'
#' @param pumping_wells A nonempty `sf` object with one point per pumping well.
#'   It must contain unique character `pump_id` values, `K` and `D` with
#'   physical units, and dimensionless `V`. An optional `well_diam` length may
#'   be supplied.
#' @param pumping_schedules A wide data frame with a strictly increasing `t`
#'   column and one consistently unit-bearing pumping-rate column for every
#'   `pump_id`.
#' @param stream_segments A projected stream-segment object returned by
#'   [prep_stream_segments()].
#' @param evaluation_times Either `NULL`, a `Date` vector, or a `units` time
#'   vector giving the times that the schedule must span. When `NULL`, the
#'   pumping-schedule times are used.
#' @param injection_times Optional `Date` or `units` time values that refine the
#'   injection interval grid.
#' @param method Character string selecting `"adf"` or `"constant_head"`.
#'   `"adf"` is the default.
#' @param stream_apportionment An ADF apportionment returned by
#'   [prep_adf_stream_apportionment()]. Required for the ADF method.
#' @param allow_n_aquifer_parameter_sets Positive whole-number scalar giving
#'   the maximum number of unique aquifer parameter sets allowed in a
#'   constant-head calculation. The default is 5. Use `Inf` to impose no
#'   limit.
#' @param adf_stream_depletion Optional output from
#'   [model_adf_stream_depletion()]. When supplied with `method = "adf"`, rates
#'   at required schedule boundaries are reused and results at additional times
#'   are ignored. If required boundaries are missing, a warning is issued and
#'   depletion is calculated internally only for the missing times.
#' @param quadrature_order Positive integer number of Gauss--Legendre points
#'   used per straight edge of each finite line element. The default is 16.
#'
#' @return A tibble with one row per `pump_id`, `reach_segment_id`, and
#'   injection interval. `interval_start` and `interval_end` retain the time
#'   representation used by `pumping_schedules$t`. `injection_rate` is positive
#'   when water moves from the stream to the aquifer. Constant-head schedules
#'   additionally
#'   contain `aquifer_id`, `boundary_residual`, and
#'   `matrix_condition_number`.
#'   Generated schedules carry an `isw_schedule_metadata` attribute recording
#'   the injection method and, for constant-head schedules, quadrature order.
#'
#' @details
#' The default injection grid contains every pumping-schedule time through the
#' final evaluation time. Evaluation times are also included so the final
#' requested result is an interval boundary. Optional `injection_times` add
#' refinement boundaries; they never remove pumping-schedule boundaries.
#'
#' With `method = "adf"`, [model_adf_stream_depletion()] evaluates stream
#' depletion at every schedule boundary. The arithmetic mean of adjacent
#' endpoint rates is applied over each interval.
#' To precompute a complete `adf_stream_depletion` input, evaluate depletion at
#' every `evaluation_times` value and at every `pumping_schedules$t` and
#' `injection_times` value through the final evaluation time. A supplied table
#' that omits required boundaries is reused where possible, but generates a
#' warning while the missing values are calculated internally.
#'
#' With `method = "constant_head"`, segment model points are constant-head
#' collocation points and the segment lines are finite injection elements. One
#' coupled response matrix is factored for each aquifer parameter set and
#' interval duration.
#' Pump-specific right-hand sides preserve attribution while sharing that
#' factorization. Rates are solved so the signed boundary residual is zero at
#' each interval endpoint after accounting for all preceding injection.
#'
#' @examples
#' pumping_wells <- example_pumping_wells
#' stream_reaches <- example_stream_reaches
#' pumping_schedules <- tibble::tibble(
#'   t = units::set_units(c(0, 10, 20), "days"),
#'   pump_1 = units::set_units(c(100, 100, 0), "m^3/day"),
#'   pump_2 = units::set_units(c(0, 75, 0), "m^3/day")
#' )
#' stream_segments <- prep_stream_segments(
#'   stream_reaches,
#'   reach_spacing = units::set_units(100, "m"),
#'   analysis_crs = 32615
#' )
#' stream_apportionment <- prep_adf_stream_apportionment(
#'   pumping_wells,
#'   stream_segments,
#'   sample_spacing = units::set_units(25, "m")
#' )
#' generate_stream_injection_schedule(
#'   pumping_wells, pumping_schedules, stream_segments,
#'   evaluation_times = units::set_units(30, "days"),
#'   stream_apportionment = stream_apportionment
#' )
#'
#' @export
generate_stream_injection_schedule <- function(
    pumping_wells,
    pumping_schedules,
    stream_segments,
    evaluation_times = NULL,
    injection_times = NULL,
    method = c("adf", "constant_head"),
    stream_apportionment = NULL,
    allow_n_aquifer_parameter_sets = 5,
    adf_stream_depletion = NULL,
    quadrature_order = 16L) {

  method <- match.arg(method)
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_segments(stream_segments)

  # Combine pumping changes, requested evaluation times, and optional injection
  # times into boundaries for the piecewise-constant injection intervals.
  time_grid <- .get_stream_injection_times(
    pumping_schedules,
    evaluation_times,
    injection_times
  )

  if (method == "adf") {
    if (is.null(stream_apportionment)) {
      stop("stream_apportionment is required when method = \"adf\".")
    }

    .validate_stream_depletion_apportionment(
      stream_apportionment,
      pumping_wells
    )
    .validate_stream_apportionment_segments(
      stream_apportionment,
      stream_segments
    )
    if (is.null(adf_stream_depletion)) {
      internal_depletion <- model_adf_stream_depletion(
        pumping_wells,
        pumping_schedules,
        stream_apportionment,
        time_grid$schedule_boundaries
      )
    } else {
      internal_depletion <- .validate_apportioned_stream_depletion(
        adf_stream_depletion,
        pumping_wells,
        pumping_schedules,
        stream_apportionment
      )
      supplied_times <- sort(unique(internal_depletion$evaluation_time))
      required_times <- time_grid$schedule_boundaries
      supplied_day_values <- if (inherits(supplied_times, "Date")) {
        as.numeric(supplied_times)
      } else {
        as.numeric(units::set_units(
          supplied_times,
          "days",
          mode = "standard"
        ))
      }
      required_day_values <- if (inherits(required_times, "Date")) {
        as.numeric(required_times)
      } else {
        as.numeric(units::set_units(
          required_times,
          "days",
          mode = "standard"
        ))
      }
      missing_time_indices <- which(
        !(required_day_values %in% supplied_day_values)
      )
      if (length(missing_time_indices) > 0) {
        missing_times <- required_times[missing_time_indices]
        missing_time_labels <- trimws(format(missing_times))
        displayed_time_labels <- head(missing_time_labels, 5L)
        missing_time_summary <- paste(displayed_time_labels, collapse = ", ")
        if (length(missing_time_labels) > length(displayed_time_labels)) {
          missing_time_summary <- paste0(missing_time_summary, ", ...")
        }

        warning(
          "adf_stream_depletion is missing ",
          length(missing_times),
          " required schedule ",
          if (length(missing_times) == 1L) "boundary" else "boundaries",
          " (", missing_time_summary, "). Depletion at missing times will ",
          "be calculated internally.",
          call. = FALSE
        )

        missing_depletion <- model_adf_stream_depletion(
          pumping_wells,
          pumping_schedules,
          stream_apportionment,
          missing_times
        )
        internal_depletion <- rbind(
          internal_depletion,
          missing_depletion
        )
      }
      row_day_values <- if (inherits(
          internal_depletion$evaluation_time,
          "Date")) {
        as.numeric(internal_depletion$evaluation_time)
      } else {
        as.numeric(units::set_units(
          internal_depletion$evaluation_time,
          "days",
          mode = "standard"
        ))
      }
      internal_depletion <- internal_depletion[
        row_day_values %in% required_day_values,
      ]
    }
    depletion_at_schedule_boundaries <- internal_depletion
    names(depletion_at_schedule_boundaries)[
      names(depletion_at_schedule_boundaries) == "evaluation_time"
    ] <- "schedule_boundary"
    # For each pump-segment pair, average depletion rates at adjacent times to
    # assign one constant injection rate to the interval between them.
    injection_schedule <- .get_interval_average_injection_schedule(
      depletion_at_schedule_boundaries,
      pumping_schedules
    )
  } else if (method == "constant_head") {
    if (!is.null(adf_stream_depletion)) {
      stop(
        "adf_stream_depletion can only be supplied when method = \"adf\"."
      )
    }
    # Solve coupled segment injection rates for each interval to maintain zero
    # head change at stream model points while accounting for pumping and prior
    # injection.
    injection_schedule <- .get_constant_head_injection_schedule(
      pumping_wells,
      pumping_schedules,
      stream_segments,
      time_grid,
      allow_n_aquifer_parameter_sets = allow_n_aquifer_parameter_sets,
      quadrature_order = quadrature_order
    )
  }

  start_rows <- match(
    as.numeric(injection_schedule$interval_start),
    as.numeric(time_grid$boundary_days)
  )
  end_rows <- match(
    as.numeric(injection_schedule$interval_end),
    as.numeric(time_grid$boundary_days)
  )
  injection_schedule$interval_start <-
    time_grid$schedule_boundaries[start_rows]
  injection_schedule$interval_end <- time_grid$schedule_boundaries[end_rows]

  attr(injection_schedule, "isw_schedule_metadata") <- list(
    injection_method = method,
    quadrature_order = if (method == "constant_head") {
      as.integer(quadrature_order)
    } else {
      NULL
    }
  )

  injection_schedule
}

# Return one response ratio for each contributing segment injection-rate-change
# event at the current observation well and evaluation time.
#
# response_operator describes the fixed spatial problem. Evaluating it produces
# an n_observation_wells by n_stream_segments response matrix for a specified
# aquifer parameter set and elapsed time. Internally, the operator stores an
# n_observation_wells by n_quadrature_points distance matrix, together with one
# weight, stream width, and segment index for each quadrature point.
#
# elapsed_times and segment_columns are parallel vectors of length n_events.
# Each position represents one contributing injection event: elapsed_times
# gives that event's age at the current evaluation time, and segment_columns
# gives the response-matrix column of the segment where the event occurred.
# observation_well_index selects the single response-matrix row being modeled.
#
# The returned response_ratios vector also has length n_events and remains in
# the original event order. Each value is the response for one event's elapsed
# time, its stream segment, and the current observation well. It can therefore
# be multiplied element-by-element by the corresponding injection-rate changes.
.get_response_ratios_for_injection_events <- function(
    response_operator,
    response_cache,
    K,
    D,
    V,
    observation_well_index,
    elapsed_times,
    segment_columns) {

  # One elapsed time and one segment-column index are required for each event.
  if (length(elapsed_times) == 0L ||
      length(elapsed_times) != length(segment_columns)) {
    stop("elapsed_times and segment_columns must have equal, positive length.")
  }

  # Allocate the length-n_events output after obtaining its units from the
  # first set of calculated responses.
  response_ratios <- NULL

  # There may be fewer distinct elapsed times than events because simultaneous
  # rate changes on different stream segments have the same age.
  distinct_elapsed_times <- unique(elapsed_times)
  for (elapsed_time_index in seq_along(distinct_elapsed_times)) {
    elapsed_time <- distinct_elapsed_times[[elapsed_time_index]]

    # These positions in the length-n_events inputs identify all events having
    # the current elapsed time.
    injection_event_indices <- which(elapsed_times == elapsed_time)

    # This matrix has n_observation_wells rows and n_stream_segments columns
    # for the current elapsed time.
    response_matrix <- .get_cached_line_response_matrix(
      response_operator = response_operator,
      response_cache = response_cache,
      K = K,
      D = D,
      V = V,
      elapsed_time = elapsed_time
    )

    # The two-column index matrix contains one [observation row, segment column]
    # pair per event having this elapsed time. event_responses therefore has
    # length equal to length(injection_event_indices).
    event_responses <- response_matrix[cbind(
      rep(observation_well_index, length(injection_event_indices)),
      segment_columns[injection_event_indices]
    )]
    if (is.null(response_ratios)) {
      response_ratios <- rep(
        event_responses[0],
        length.out = length(elapsed_times)
      )
    }

    # Place these responses back into their original event positions.
    response_ratios[injection_event_indices] <- event_responses
  }

  # Return a length-n_events units vector aligned with the contributing
  # injection-event rows and their injection-rate changes.
  unname(response_ratios)
}

# Superimpose pumping and finite-line injection responses at observations.
.calculate_aquifer_water_level_change <- function(
    pumping_wells,
    pumping_schedules,
    observation_wells,
    evaluation_times = NULL,
    stream_injection_schedule,
    quadrature_order = 16L,
    stream_segments) {

  quadrature_order_supplied <- !missing(quadrature_order)
  .validate_observation_wells(observation_wells)
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_segments(stream_segments)

  if (missing(stream_injection_schedule) ||
      is.null(stream_injection_schedule)) {
    stop("stream_injection_schedule must be a completed schedule.")
  }

  quadrature_order <- .resolve_schedule_quadrature_order(
    stream_injection_schedule,
    quadrature_order,
    quadrature_order_supplied
  )
  analysis_crs <- sf::st_crs(stream_segments)

  prepare_points <- function(x) {
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    sf::st_transform(x, analysis_crs)
  }

  prepared_pumping_wells <- prepare_points(pumping_wells)
  prepared_observation_wells <- prepare_points(observation_wells)

  if (!("well_diam" %in% names(prepared_pumping_wells))) {
    prepared_pumping_wells$well_diam <- units::set_units(
      rep(0, nrow(prepared_pumping_wells)),
      "m",
      mode = "standard"
    )
  }

  # Prepare the quadrature representation used for responses at observation
  # wells; line elements prepared during schedule generation are not retained.
  line_elements <- .prepare_line_elements(
    stream_segments,
    quadrature_order
  )
  observation_response_operator <- .prepare_line_response_operator(
    prepared_observation_wells,
    line_elements
  )
  pump_distances <- sf::st_distance(
    prepared_pumping_wells,
    prepared_observation_wells
  )
  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  output_evaluation_times <- normalized_times$output_target_times
  evaluation_days <- normalized_times$normalized_target_times

  # Pair physical pumping-rate changes with requested evaluation times. Stream
  # injection events and their elapsed times are derived separately below.
  pumping_responses <- .get_pumping_response_times(
    pumping_schedules,
    pumping_wells,
    evaluation_times
  )

  # Validate the completed piecewise-constant segment injection schedule.
  injection_schedule <- .validate_stream_injection_schedule(
    stream_injection_schedule,
    pumping_wells,
    pumping_schedules,
    stream_segments,
    evaluation_times
  )
  injection_events <- .get_injection_rate_changes(injection_schedule)
  if (nrow(injection_events) > 0) {
    unique_injection_times <- sort(unique(injection_events$injection_time))
    normalized_injection_times <- .normalize_time_inputs(
      pumping_schedules$t,
      unique_injection_times
    )
    normalized_injection_days <-
      normalized_injection_times$normalized_target_times
    injection_events$injection_time <- normalized_injection_days[match(
        as.numeric(injection_events$injection_time),
        as.numeric(unique_injection_times)
      )]
  } else {
    injection_events$injection_time <- evaluation_days[0]
  }
  output_length_unit <- units::deparse_unit(pumping_wells$D)
  zero_drawdown <- units::set_units(
    0,
    output_length_unit,
    mode = "standard"
  )

  result_pump_ids <- character()
  result_observation_ids <- character()
  result_evaluation_times <- output_evaluation_times[0]
  result_pumping_drawdowns <- zero_drawdown[0]
  result_stream_recoveries <- zero_drawdown[0]
  injection_response_cache <- new.env(parent = emptyenv())

  # Evaluate every pump at every observation well and requested time,
  # separating direct pumping drawdown from recovery due to stream injection.
  for (pump_id in pumping_wells$pump_id) {
    pump_row <- match(pump_id, prepared_pumping_wells$pump_id)

    # At each observation well, combine direct drawdown from this pump with
    # recovery from the stream injection attributed to it.
    for (observation_well_index in seq_len(nrow(prepared_observation_wells))) {
      # Combine all contributing pumping and injection events at each output
      # time.
      for (evaluation_time_index in seq_along(output_evaluation_times)) {
        pumping_change <- zero_drawdown
        stream_change <- zero_drawdown
        physical_event_rows <- which(
          pumping_responses$pump_id == pump_id &
            as.numeric(pumping_responses$evaluation_time) ==
              as.numeric(output_evaluation_times[[evaluation_time_index]])
        )

        # Sum direct drawdown from this pump's contributing rate-change events.
        if (length(physical_event_rows) > 0) {
          response_ratio <- .theis_aquifer_drawdown_ratio(
            distance = pump_distances[pump_row, observation_well_index],
            K = prepared_pumping_wells$K[[pump_row]],
            D = prepared_pumping_wells$D[[pump_row]],
            V = prepared_pumping_wells$V[[pump_row]],
            t = pumping_responses$elapsed_time[physical_event_rows],
            well_diam = prepared_pumping_wells$well_diam[[pump_row]]
          )
          pumping_change <- units::set_units(
            sum(
              pumping_responses$pumping_rate_change[physical_event_rows] *
                response_ratio
            ),
            output_length_unit,
            mode = "standard"
          )
        }

        # Select earlier segment injection-rate changes attributed to this pump.
        contributing_injections <- which(
          injection_events$pump_id == pump_id &
            injection_events$injection_time <
              evaluation_days[[evaluation_time_index]]
        )

        if (length(contributing_injections) > 0) {
          matched_segment_columns <- match(
            injection_events$reach_segment_id[contributing_injections],
            stream_segments$reach_segment_id
          )
          injection_elapsed_times <-
            evaluation_days[[evaluation_time_index]] -
            injection_events$injection_time[contributing_injections]

          # At the current evaluation time, every earlier segment injection-
          # rate-change event still contributes to water-level recovery. Each
          # event has one current elapsed time: the evaluation time minus its
          # injection time. The same event is evaluated again at later output
          # times because its response evolves as its elapsed time increases.
          # Events that occurred together share an elapsed time, so the helper
          # evaluates one full response matrix for that distinct elapsed time
          # and extracts the applicable segment response for each event.
          injection_response_ratios <-
            .get_response_ratios_for_injection_events(
              response_operator = observation_response_operator,
              response_cache = injection_response_cache,
              K = prepared_pumping_wells$K[[pump_row]],
              D = prepared_pumping_wells$D[[pump_row]],
              V = prepared_pumping_wells$V[[pump_row]],
              observation_well_index = observation_well_index,
              elapsed_times = injection_elapsed_times,
              segment_columns = matched_segment_columns
            )

          # At this evaluation time, multiply every earlier injection-rate
          # change by its current event-specific response ratio, then sum the
          # resulting head changes to obtain total stream recovery for this
          # pump and observation well.
          stream_change <- units::set_units(
            sum(
              injection_events$injection_rate_change[
                contributing_injections
              ] * injection_response_ratios
            ),
            output_length_unit,
            mode = "standard"
          )
        }

        # Append one result row for this pump, observation, and evaluation time.
        result_pump_ids <- c(result_pump_ids, pump_id)
        result_observation_ids <- c(
          result_observation_ids,
          prepared_observation_wells$observation_id[[observation_well_index]]
        )
        result_evaluation_times <- c(
          result_evaluation_times,
          output_evaluation_times[[evaluation_time_index]]
        )
        result_pumping_drawdowns <- c(
          result_pumping_drawdowns,
          -pumping_change
        )
        result_stream_recoveries <- c(
          result_stream_recoveries,
          stream_change
        )
      }
    }
  }

  tibble::tibble(
    pump_id = result_pump_ids,
    observation_id = result_observation_ids,
    evaluation_time = result_evaluation_times,
    pumping_drawdown = result_pumping_drawdowns,
    stream_recovery = result_stream_recoveries,
    water_level_change = -result_pumping_drawdowns +
      result_stream_recoveries
  )
}

#' Model aquifer water-level change
#'
#' Superimpose physical pumping and stream-injection responses at observation
#' wells using a general stream-segment representation.
#'
#' @param pumping_wells A nonempty `sf` object with one point per pumping well.
#'   It must contain unique character `pump_id` values, `K` and `D` with
#'   physical units, dimensionless `V`, and optionally `well_diam` with length
#'   units.
#' @param pumping_schedules A wide data frame with a strictly increasing `t`
#'   column and one consistently unit-bearing pumping-rate column for every
#'   `pump_id`.
#' @param observation_wells A nonempty `sf` object with point geometry and a
#'   unique, nonmissing character `observation_id` column.
#' @param stream_segments A projected stream-segment object returned by
#'   [prep_stream_segments()].
#' @param evaluation_times Either `NULL`, a `Date` vector, or a `units` time
#'   vector giving the requested response times. When `NULL`, pumping-schedule
#'   times are used.
#' @param injection_times Optional `Date` or `units` time values refining the
#'   internally generated injection grid.
#' @param stream_injection_schedule Either `NULL` or a completed schedule from
#'   [generate_stream_injection_schedule()].
#' @param injection_method Character string selecting `"adf"` or
#'   `"constant_head"` when a schedule is not supplied. `"adf"` is the
#'   default.
#' @param stream_apportionment An object returned by
#'   [prep_adf_stream_apportionment()]. It is required when an ADF schedule is
#'   generated internally.
#' @param allow_n_aquifer_parameter_sets Positive whole-number scalar giving
#'   the maximum number of unique aquifer parameter sets allowed when a
#'   constant-head schedule is generated internally. The default is 5. Use
#'   `Inf` to impose no limit.
#' @param quadrature_order Positive integer number of Gauss--Legendre points
#'   used per straight edge of each finite line element. The default is 16.
#'   When a completed constant-head schedule is supplied and this argument is
#'   omitted, the order recorded in the schedule metadata is reused. Supplying
#'   a different order raises a warning and uses the supplied value.
#'
#' @return A tibble with one row for every `pump_id`, `observation_id`, and
#'   requested `evaluation_time`. `pumping_drawdown` is the positive decline
#'   caused by pumping, `stream_recovery` is the positive recovery caused by
#'   stream injection, and `water_level_change` is `-pumping_drawdown +
#'   stream_recovery`. All three response columns retain length units.
#'
#' @details
#' Results remain pump-specific so contributions can be inspected individually
#' or summed by observation and evaluation time. When no schedule is supplied,
#' [generate_stream_injection_schedule()] generates one using
#' `injection_method`. A supplied schedule reuses its stored method and
#' quadrature metadata; `injection_times` must then be `NULL`.
#'
#' The ADF path apportions each pump's analytical stream depletion among stream
#' segments and therefore requires `stream_apportionment`. The constant-head
#' path solves segment injection rates that keep segment model points as close
#' as practical to zero head change. Pumping and stream responses are then
#' superimposed at each observation well. Positive pumping rates produce
#' positive `pumping_drawdown`; injection produces positive `stream_recovery`.
#'
#' @seealso [generate_stream_injection_schedule()],
#'   [model_adf_stream_depletion()]
#'
#' @examples
#' stream_segments <- prep_stream_segments(
#'   example_stream_reaches,
#'   reach_spacing = units::set_units(100, "m")
#' )
#' stream_apportionment <- prep_adf_stream_apportionment(
#'   example_pumping_wells,
#'   stream_segments,
#'   sample_spacing = units::set_units(25, "m")
#' )
#' pumping_schedules <- tibble::tibble(
#'   t = units::set_units(c(0, 10, 20), "days"),
#'   pump_1 = units::set_units(c(100, 100, 0), "m^3/day"),
#'   pump_2 = units::set_units(c(0, 75, 0), "m^3/day")
#' )
#' model_aquifer_water_level_change(
#'   example_pumping_wells,
#'   pumping_schedules,
#'   example_observation_wells,
#'   stream_segments,
#'   stream_apportionment = stream_apportionment
#' )
#'
#' @export
model_aquifer_water_level_change <- function(
    pumping_wells,
    pumping_schedules,
    observation_wells,
    stream_segments,
    evaluation_times = NULL,
    injection_times = NULL,
    stream_injection_schedule = NULL,
    injection_method = c("adf", "constant_head"),
    stream_apportionment = NULL,
    allow_n_aquifer_parameter_sets = 5,
    quadrature_order = 16L) {

  injection_method <- match.arg(injection_method)
  .validate_stream_segments(stream_segments)

  if (!is.null(stream_injection_schedule) && !is.null(injection_times)) {
    stop(
      "injection_times must be NULL when a stream-injection schedule is ",
      "supplied."
    )
  }

  if (is.null(stream_injection_schedule)) {
    stream_injection_schedule <- generate_stream_injection_schedule(
      pumping_wells,
      pumping_schedules,
      stream_segments,
      evaluation_times,
      injection_times,
      method = injection_method,
      stream_apportionment = stream_apportionment,
      allow_n_aquifer_parameter_sets =
        allow_n_aquifer_parameter_sets,
      quadrature_order = quadrature_order
    )
  }

  response_quadrature_order <- if (missing(quadrature_order)) {
    schedule_order <- .get_metadata_quadrature_order(
      stream_injection_schedule
    )
    if (is.null(schedule_order)) 16L else schedule_order
  } else {
    quadrature_order
  }
  .calculate_aquifer_water_level_change(
    pumping_wells = pumping_wells,
    pumping_schedules = pumping_schedules,
    observation_wells = observation_wells,
    evaluation_times = evaluation_times,
    stream_injection_schedule = stream_injection_schedule,
    quadrature_order = response_quadrature_order,
    stream_segments = stream_segments
  )
}
