# TODO: Before finalizing apportioned aquifer drawdown, review the
# interval-average temporal approximation, finite line-element convergence
# with reach, quadrature, and evaluation-time spacing,
# and the pump-specific output structure.
# TODO: Review the optional stream_injection_schedule interface, including its
# continuous-coverage validation, whether externally supplied schedules should
# permit additional metadata, and opportunities to cache or reuse internally
# calculated stream-depletion responses.

# Read a valid quadrature order recorded on a constant-head schedule.
.get_schedule_quadrature_order <- function(stream_injection_schedule) {
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

  schedule_order <- .get_schedule_quadrature_order(
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
  evaluation_day_values <- as.numeric(normalized_times$evaluation_times)
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
    pumping_schedules,
    stream_apportionment,
    evaluation_times = NULL) {

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
  schedule_start <- normalized_evaluations$pumping_times[[1]]
  final_evaluation <- max(normalized_evaluations$evaluation_times)

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

  apportionment_table <- sf::st_drop_geometry(stream_apportionment)
  expected_keys <- paste(
    apportionment_table$pump_id,
    apportionment_table$reach_segment_id,
    sep = "\r"
  )
  schedule_keys <- paste(
    stream_injection_schedule$pump_id,
    stream_injection_schedule$reach_segment_id,
    sep = "\r"
  )

  if (!setequal(unique(schedule_keys), unique(expected_keys))) {
    stop(
      "stream_injection_schedule must contain every pump and reach-segment ",
      "combination in stream_apportionment, with no additional combinations."
    )
  }

  expected_reaches <- stats::setNames(
    apportionment_table$reach_id,
    expected_keys
  )

  for (pair_key in unique(expected_keys)) {
    pair_rows <- which(schedule_keys == pair_key)
    pair_rows <- pair_rows[order(as.numeric(interval_starts[pair_rows]))]

    if (any(
      stream_injection_schedule$reach_id[pair_rows] !=
        expected_reaches[[pair_key]]
    )) {
      stop(
        "stream_injection_schedule$reach_id must match each reach_segment_id ",
        "in stream_apportionment."
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
  output_evaluation_times <-
    normalized_evaluations$output_evaluation_times
  evaluation_days <- normalized_evaluations$evaluation_times
  pumping_days <- normalized_evaluations$pumping_times

  if (is.null(injection_times)) {
    refinement_days <- evaluation_days[0]
  } else {
    refinement_days <- .normalize_time_inputs(
      pumping_schedules$t,
      injection_times
    )$evaluation_times
  }

  final_evaluation_day <- max(evaluation_days)
  internal_day_values <- sort(unique(c(
    as.numeric(pumping_days[pumping_days <= final_evaluation_day]),
    as.numeric(evaluation_days),
    as.numeric(refinement_days[refinement_days <= final_evaluation_day])
  )))
  internal_days <- units::set_units(
    internal_day_values,
    "days",
    mode = "standard"
  )

  if (inherits(pumping_schedules$t, "Date")) {
    internal_times <- pumping_schedules$t[[1]] + internal_day_values
  } else {
    internal_times <- units::set_units(
      internal_days,
      units::deparse_unit(pumping_schedules$t),
      mode = "standard"
    )
  }

  list(
    evaluation_times = output_evaluation_times,
    injection_times = internal_times,
    injection_days = internal_days
  )
}

# Calculate interval-average stream-injection rates.
.get_interval_average_injection_schedule <- function(
    stream_depletion,
    pumping_schedules) {

  depletion_times <- sort(unique(stream_depletion$evaluation_time))
  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    depletion_times
  )
  schedule_start <- normalized_times$pumping_times[[1]]
  depletion_days <- normalized_times$evaluation_times
  prepend_initial_zero <- depletion_days[[1]] > schedule_start

  if (prepend_initial_zero) {
    time_nodes <- c(schedule_start, depletion_days)
  } else {
    time_nodes <- depletion_days
  }

  pump_output <- character()
  reach_output <- character()
  segment_output <- character()
  interval_start_output <- time_nodes[0]
  interval_end_output <- time_nodes[0]
  rate_output <- stream_depletion$stream_depletion_rate[0]
  pair_keys <- unique(paste(
    stream_depletion$pump_id,
    stream_depletion$reach_segment_id,
    sep = "\r"
  ))

  for (pair_key in pair_keys) {
    key_parts <- strsplit(pair_key, "\r", fixed = TRUE)[[1]]
    pump_id <- key_parts[[1]]
    reach_segment_id <- key_parts[[2]]
    pair_rows <- which(
      stream_depletion$pump_id == pump_id &
        stream_depletion$reach_segment_id == reach_segment_id
    )
    pair_rows <- pair_rows[order(as.numeric(
      stream_depletion$evaluation_time[pair_rows]
    ))]
    endpoint_rates <- stream_depletion$stream_depletion_rate[pair_rows]

    if (prepend_initial_zero) {
      endpoint_rates <- c(endpoint_rates[[1]] * 0, endpoint_rates)
    }

    if (length(endpoint_rates) < 2) {
      next
    }

    interval_rates <- -(
      endpoint_rates[-length(endpoint_rates)] + endpoint_rates[-1]
    ) / 2
    number_of_intervals <- length(interval_rates)

    pump_output <- c(pump_output, rep(pump_id, number_of_intervals))
    reach_output <- c(
      reach_output,
      rep(stream_depletion$reach_id[pair_rows[[1]]], number_of_intervals)
    )
    segment_output <- c(
      segment_output,
      rep(reach_segment_id, number_of_intervals)
    )
    interval_start_output <- c(
      interval_start_output,
      time_nodes[-length(time_nodes)]
    )
    interval_end_output <- c(interval_end_output, time_nodes[-1])
    rate_output <- c(rate_output, interval_rates)
  }

  tibble::tibble(
    pump_id = pump_output,
    reach_id = reach_output,
    reach_segment_id = segment_output,
    interval_start = interval_start_output,
    interval_end = interval_end_output,
    injection_rate = rate_output
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
    interval_rates <- injection_schedule$injection_rate[pair_rows]
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
#' Stream depletion is positive when water enters the aquifer from a stream.
#' Injection rates therefore use the opposite sign so the same well-response
#' kernel can be used for pumping and injection.
#'
#' The first pumping-schedule time defines the initial time boundary. When the
#' first evaluation occurs later, stream depletion at that initial boundary is
#' assumed to be zero. The injection rate in each interval is the negative
#' arithmetic mean of stream depletion at the beginning and end of that
#' interval. Consecutive interval rates are converted to signed rate changes;
#' exact zero changes are omitted.
#'
#' @keywords internal
.get_interval_average_injection_rate_changes <- function(
    stream_depletion,
    pumping_schedules) {
  .get_injection_rate_changes(
    .get_interval_average_injection_schedule(
      stream_depletion,
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
#' @param allow_many_aquifer_parameter_sets Logical. Constant-head calculations
#'   stop when more than ten unique aquifer parameter sets are found unless
#'   this is `TRUE`.
#' @param adf_stream_depletion Optional output from
#'   [model_adf_stream_depletion()] that includes every schedule boundary. When
#'   supplied with `method = "adf"`, these rates are reused instead of being
#'   recalculated; results at additional times are ignored.
#' @param quadrature_order Positive integer number of Gauss--Legendre points
#'   used per straight edge of each finite line element. The default is 16.
#'
#' @return A tibble with one row per `pump_id`, `reach_segment_id`, and
#'   injection interval. `interval_start` and `interval_end` retain the time
#'   representation used by `pumping_schedules$t`. `injection_rate` is negative
#'   when water enters the aquifer. Constant-head schedules additionally
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
    allow_many_aquifer_parameter_sets = FALSE,
    adf_stream_depletion = NULL,
    quadrature_order = 16L) {

  method <- match.arg(method)
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_segments(stream_segments)
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
        time_grid$injection_times
      )
    } else {
      internal_depletion <- .validate_apportioned_stream_depletion(
        adf_stream_depletion,
        pumping_wells,
        pumping_schedules,
        stream_apportionment
      )
      supplied_times <- sort(unique(internal_depletion$evaluation_time))
      required_times <- time_grid$injection_times
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
      if (!all(required_day_values %in% supplied_day_values)) {
        stop(
          "adf_stream_depletion must contain results at every stream-",
          "depletion schedule boundary."
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
    injection_schedule <- .get_interval_average_injection_schedule(
      internal_depletion,
      pumping_schedules
    )
  } else {
    if (!is.null(adf_stream_depletion)) {
      stop(
        "adf_stream_depletion can only be supplied when method = \"adf\"."
      )
    }
    injection_schedule <- .get_constant_head_injection_schedule(
      pumping_wells,
      pumping_schedules,
      stream_segments,
      time_grid,
      allow_many_aquifer_parameter_sets,
      quadrature_order
    )
  }

  start_rows <- match(
    as.numeric(injection_schedule$interval_start),
    as.numeric(time_grid$injection_days)
  )
  end_rows <- match(
    as.numeric(injection_schedule$interval_end),
    as.numeric(time_grid$injection_days)
  )
  injection_schedule$interval_start <-
    time_grid$injection_times[start_rows]
  injection_schedule$interval_end <- time_grid$injection_times[end_rows]

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

# Superimpose pumping and finite-line injection responses at observations.
.calculate_aquifer_water_level_change <- function(
    pumping_wells,
    pumping_schedules,
    observation_wells,
    stream_apportionment,
    evaluation_times = NULL,
    injection_times = NULL,
    stream_injection_schedule = NULL,
    quadrature_order = 16L,
    stream_segments) {

  quadrature_order_supplied <- !missing(quadrature_order)
  .validate_observation_wells(observation_wells)
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_depletion_apportionment(
    stream_apportionment,
    pumping_wells
  )
  .validate_stream_segments(stream_segments)

  if (!is.null(stream_injection_schedule) && !is.null(injection_times)) {
    stop(
      "injection_times must be NULL when stream_injection_schedule is ",
      "supplied."
    )
  }

  if (!is.null(stream_injection_schedule)) {
    quadrature_order <- .resolve_schedule_quadrature_order(
      stream_injection_schedule,
      quadrature_order,
      quadrature_order_supplied
    )
  }

  unique_stream_segments <- stream_segments
  .validate_stream_apportionment_segments(
    stream_apportionment,
    unique_stream_segments
  )
  analysis_crs <- sf::st_crs(unique_stream_segments)

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

  line_elements <- .prepare_line_elements(
    unique_stream_segments,
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
  output_evaluation_times <- normalized_times$output_evaluation_times
  evaluation_days <- normalized_times$evaluation_times
  pumping_responses <- .get_pumping_response_times(
    pumping_schedules,
    pumping_wells,
    evaluation_times
  )
  if (is.null(stream_injection_schedule)) {
    injection_schedule <- generate_stream_injection_schedule(
      pumping_wells = pumping_wells,
      pumping_schedules = pumping_schedules,
      stream_segments = unique_stream_segments,
      evaluation_times = evaluation_times,
      injection_times = injection_times,
      stream_apportionment = stream_apportionment
    )
  } else {
    injection_schedule <- .validate_stream_injection_schedule(
      stream_injection_schedule,
      pumping_schedules,
      stream_apportionment,
      evaluation_times
    )
  }
  injection_events <- .get_injection_rate_changes(injection_schedule)
  if (nrow(injection_events) > 0) {
    unique_injection_times <- sort(unique(injection_events$injection_time))
    normalized_injection_times <- .normalize_time_inputs(
      pumping_schedules$t,
      unique_injection_times
    )
    normalized_injection_days <- normalized_injection_times$evaluation_times
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

  pump_output <- character()
  observation_output <- character()
  evaluation_output <- output_evaluation_times[0]
  pumping_output <- zero_drawdown[0]
  recovery_output <- zero_drawdown[0]
  injection_response_cache <- new.env(parent = emptyenv())

  get_injection_response_matrix <- function(pump_row, elapsed_time) {
    elapsed_days <- as.numeric(units::set_units(
      elapsed_time,
      "days",
      mode = "standard"
    ))
    parameter_key <- paste(
      format(as.numeric(units::set_units(
        prepared_pumping_wells$K[[pump_row]], "m/day", mode = "standard"
      )), digits = 17),
      format(as.numeric(units::set_units(
        prepared_pumping_wells$D[[pump_row]], "m", mode = "standard"
      )), digits = 17),
      format(prepared_pumping_wells$V[[pump_row]], digits = 17),
      format(elapsed_days, digits = 17),
      sep = "\r"
    )

    if (!exists(parameter_key, envir = injection_response_cache,
        inherits = FALSE)) {
      assign(
        parameter_key,
        .evaluate_line_response_operator(
          observation_response_operator,
          K = prepared_pumping_wells$K[[pump_row]],
          D = prepared_pumping_wells$D[[pump_row]],
          V = prepared_pumping_wells$V[[pump_row]],
          elapsed_time = elapsed_time
        ),
        envir = injection_response_cache
      )
    }

    get(parameter_key, envir = injection_response_cache, inherits = FALSE)
  }

  for (pump_id in pumping_wells$pump_id) {
    pump_row <- match(pump_id, prepared_pumping_wells$pump_id)

    for (observation_index in seq_len(nrow(prepared_observation_wells))) {
      for (evaluation_index in seq_along(output_evaluation_times)) {
        pumping_change <- zero_drawdown
        stream_change <- zero_drawdown
        physical_event_rows <- which(
          pumping_responses$pump_id == pump_id &
            as.numeric(pumping_responses$evaluation_time) ==
              as.numeric(output_evaluation_times[[evaluation_index]])
        )

        if (length(physical_event_rows) > 0) {
          response_ratio <- .theis_aquifer_drawdown_ratio(
            distance = pump_distances[pump_row, observation_index],
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

        contributing_injections <- which(
          injection_events$pump_id == pump_id &
            injection_events$injection_time <
              evaluation_days[[evaluation_index]]
        )

        if (length(contributing_injections) > 0) {
          matched_segment_columns <- match(
            injection_events$reach_segment_id[contributing_injections],
            unique_stream_segments$reach_segment_id
          )
          elapsed_time <- evaluation_days[[evaluation_index]] -
            injection_events$injection_time[contributing_injections]
          response_ratio <- vapply(
            seq_along(contributing_injections),
            function(event_index) {
              response_matrix <- get_injection_response_matrix(
                pump_row,
                elapsed_time[[event_index]]
              )
              response_matrix[
                observation_index,
                matched_segment_columns[[event_index]]
              ]
            },
            numeric(1)
          )
          response_ratio <- units::set_units(
            response_ratio,
            "days/m^2",
            mode = "standard"
          )
          stream_change <- units::set_units(
            sum(
              injection_events$injection_rate_change[
                contributing_injections
              ] * response_ratio
            ),
            output_length_unit,
            mode = "standard"
          )
        }

        pump_output <- c(pump_output, pump_id)
        observation_output <- c(
          observation_output,
          prepared_observation_wells$observation_id[[observation_index]]
        )
        evaluation_output <- c(
          evaluation_output,
          output_evaluation_times[[evaluation_index]]
        )
        pumping_output <- c(pumping_output, -pumping_change)
        recovery_output <- c(recovery_output, stream_change)
      }
    }
  }

  tibble::tibble(
    pump_id = pump_output,
    observation_id = observation_output,
    evaluation_time = evaluation_output,
    pumping_drawdown = pumping_output,
    stream_recovery = recovery_output,
    water_level_change = -pumping_output + recovery_output
  )
}

# Expand neutral stream-segment identifiers to every pump for schedule mapping.
.expand_stream_segments_for_pumps <- function(
    stream_segments,
    pumping_wells) {

  .validate_stream_segments(stream_segments)
  .validate_pumping_wells(pumping_wells)
  number_of_segments <- nrow(stream_segments)
  segment_rows <- rep(
    seq_len(number_of_segments),
    times = nrow(pumping_wells)
  )
  tibble::tibble(
    pump_id = rep(pumping_wells$pump_id, each = number_of_segments),
    reach_id = stream_segments$reach_id[segment_rows],
    reach_segment_id = stream_segments$reach_segment_id[segment_rows],
    pump_to_reach_distance = units::set_units(
      rep(0, length(segment_rows)),
      "m"
    ),
    apportionment_fraction = rep(
      1 / number_of_segments,
      length(segment_rows)
    )
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
#' @param allow_many_aquifer_parameter_sets Passed to
#'   [generate_stream_injection_schedule()] for constant-head calculations.
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
    allow_many_aquifer_parameter_sets = FALSE,
    quadrature_order = 16L) {

  injection_method <- match.arg(injection_method)
  .validate_stream_segments(stream_segments)

  if (!is.null(stream_injection_schedule) && !is.null(injection_times)) {
    stop(
      "injection_times must be NULL when a stream-injection schedule is ",
      "supplied."
    )
  }

  constructing_adf_schedule <- is.null(stream_injection_schedule) &&
    injection_method == "adf"

  if (is.null(stream_injection_schedule)) {
    stream_injection_schedule <- generate_stream_injection_schedule(
      pumping_wells,
      pumping_schedules,
      stream_segments,
      evaluation_times,
      injection_times,
      method = injection_method,
      stream_apportionment = stream_apportionment,
      allow_many_aquifer_parameter_sets =
        allow_many_aquifer_parameter_sets,
      quadrature_order = quadrature_order
    )
  }

  response_apportionment <- if (constructing_adf_schedule) {
    stream_apportionment
  } else {
    .expand_stream_segments_for_pumps(stream_segments, pumping_wells)
  }
  response_quadrature_order <- if (missing(quadrature_order)) {
    schedule_order <- .get_schedule_quadrature_order(
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
    stream_apportionment = response_apportionment,
    evaluation_times = evaluation_times,
    stream_injection_schedule = stream_injection_schedule,
    quadrature_order = response_quadrature_order,
    stream_segments = stream_segments
  )
}
