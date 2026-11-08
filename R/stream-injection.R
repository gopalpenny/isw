# Validate a user-supplied stream-injection schedule.
.validate_stream_injection_schedule <- function(
    stream_injection_schedule,
    pumping_wells,
    pumping_schedules,
    stream_segments,
    evaluation_times = NULL) {

  # Context wells, schedule, segments and evaluation times were checked by the wrapper.

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

  if ("boundary_residual" %in% names(stream_injection_schedule)) {
    check_dimensionality(stream_injection_schedule$boundary_residual, "m",
                         "stream_injection_schedule$boundary_residual")
    if (any(!is.finite(as.numeric(stream_injection_schedule$boundary_residual)))) {
      stop("stream_injection_schedule$boundary_residual must be finite.")
    }
  }

  normalized_evaluations <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  schedule_start_day <- normalized_evaluations$schedule_time_day[[1]]
  final_evaluation_day <- max(normalized_evaluations$target_time_day)

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

  if (nrow(stream_injection_schedule) == 0) {
    # Empty horizons still require correctly typed physical columns.
    if (!schedule_is_date) {
      check_dimensionality(stream_injection_schedule$interval_start, "days", "interval_start")
      check_dimensionality(stream_injection_schedule$interval_end, "days", "interval_end")
    }
    if (final_evaluation_day > schedule_start_day) {
      stop(
        "stream_injection_schedule cannot be empty when evaluation times ",
        "extend beyond the first pumping-schedule time."
      )
    }
    return(stream_injection_schedule)
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

  # Interval dimensions were checked above; strip normalized days for coverage comparisons.
  interval_starts_day <- as.numeric(interval_starts)
  interval_ends_day <- as.numeric(interval_ends)
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
    pair_rows <- pair_rows[order(interval_starts_day[pair_rows])]

    if (any(
      stream_injection_schedule$reach_id[pair_rows] !=
        expected_reaches[[pair_key]]
    )) {
      stop(
        "stream_injection_schedule$reach_id must match each reach_segment_id ",
        "in stream_segments."
      )
    }

    if (interval_starts_day[pair_rows[[1]]] != schedule_start_day ||
        interval_ends_day[pair_rows[[length(pair_rows)]]] < final_evaluation_day ||
        (length(pair_rows) > 1 && any(
          interval_starts_day[pair_rows[-1]] !=
            interval_ends_day[pair_rows[-length(pair_rows)]]
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

# Merge prepared pumping, evaluation and refinement times into interval boundaries.
.get_stream_injection_times <- function(pumping_schedules, evaluation_time_day = NULL,
                                        injection_time_day = NULL) {
  # Already-validated numeric times are days; exclude boundaries after final evaluation.
  if (is.null(evaluation_time_day)) evaluation_time_day <- pumping_schedules$t_day
  final_evaluation_day <- max(evaluation_time_day)
  boundary_time_day <- sort(unique(c(
    pumping_schedules$t_day[pumping_schedules$t_day <= final_evaluation_day],
    evaluation_time_day, injection_time_day[injection_time_day <= final_evaluation_day]
  )))
  list(boundary_time_day = boundary_time_day,
       interval_start_day = utils::head(boundary_time_day, -1L),
       interval_end_day = utils::tail(boundary_time_day, -1L),
       interval_duration_day = diff(boundary_time_day))
}

.get_injection_rate_changes <- function(injection_schedule) {
  # Validated numeric rates use m^3/day and times day.
  template <- tibble::tibble(
    pump_id = character(),
    reach_id = character(),
    reach_segment_id = character(),
    injection_time_day = numeric(),
    injection_rate_change_m3_per_day = numeric()
  )
  keys <- unique(injection_schedule[c("pump_id", "reach_segment_id")])

  # Convert each pump-segment interval schedule into nonzero rate-change events.
  injection_events_by_pump_segment <- lapply(
    seq_len(nrow(keys)),
    .get_injection_events_for_pump_segment,
    keys = keys,
    injection_schedule = injection_schedule
  )

  do.call(rbind, c(list(template), injection_events_by_pump_segment))
}

# Construct signed rate-change events for one pump-segment pair.
.get_injection_events_for_pump_segment <- function(index, keys, injection_schedule) {
  # Validated numeric rates use m^3/day and times day.

  rows <- which(
    injection_schedule$pump_id == keys$pump_id[[index]] &
      injection_schedule$reach_segment_id == keys$reach_segment_id[[index]]
  )
  rows <- rows[order(injection_schedule$interval_start_day[rows])]

  # User-facing injection is positive from stream to aquifer; response kernels use
  # the pumping convention, so injection enters superposition with the opposite sign.
  change_m3_per_day <- diff(c(0, -injection_schedule$injection_rate_m3_per_day[rows]))
  changed <- which(change_m3_per_day != 0)

  tibble::tibble(
    pump_id = rep(keys$pump_id[[index]], length(changed)),
    reach_id = rep(injection_schedule$reach_id[rows[[1]]], length(changed)),
    reach_segment_id = rep(keys$reach_segment_id[[index]], length(changed)),
    injection_time_day = injection_schedule$interval_start_day[rows][changed],
    injection_rate_change_m3_per_day = change_m3_per_day[changed]
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
#' @param quadrature_order Nonnegative integer controlling stream injection sources.
#'   With `0`, each segment injects its total rate at its `model_point`, using
#'   `stream_width / 2` as the minimum response radius. Positive values give the
#'   number of Gauss--Legendre points per straight edge of each finite line
#'   element. The default is 4.
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
#' collocation points. Injection uses those points when `quadrature_order = 0`,
#' and uniform finite-line sources otherwise. One coupled response matrix is
#' factored for each aquifer parameter set and interval duration.
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
#' @importFrom utils head
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
    quadrature_order = 4L) {

  # Validate external tables/units before preparing numeric meters, days and rates.
  method <- match.arg(method)
  aquifer_groups <- NULL
  prepared_stream_segments <- .prepare_numeric_segments(stream_segments)
  prepared_pumping_wells <- .prepare_numeric_pumping_wells(pumping_wells, sf::st_crs(prepared_stream_segments))
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_quadrature_order(quadrature_order)
  prepared_model_times <- .prepare_numeric_model_times(
    pumping_schedules, evaluation_times, injection_times
  )
  if (method == "adf") {
    if (is.null(stream_apportionment)) {
      stop('stream_apportionment is required when method = "adf".')
    }
    .validate_stream_depletion_apportionment(stream_apportionment, pumping_wells)
    .validate_stream_apportionment_segments(stream_apportionment, stream_segments)
    prepared_stream_apportionment <- .prepare_numeric_apportionment(stream_apportionment, pumping_wells)
    depletion <- if (!is.null(adf_stream_depletion)) {
      .prepare_numeric_depletion(
        adf_stream_depletion, pumping_wells, pumping_schedules,
        stream_apportionment, prepared_model_times$time_origin
      )
    } else NULL
  } else {
    if (!is.null(adf_stream_depletion)) {
      stop('adf_stream_depletion can only be supplied when method = "adf".')
    }
    aquifer_groups <- .prepare_aquifer_groups(prepared_pumping_wells, allow_n_aquifer_parameter_sets)
    prepared_stream_apportionment <- NULL
    depletion <- NULL
  }
  attr(prepared_pumping_wells, "isw_aquifer_groups") <- aquifer_groups
  result <- .generate_stream_injection_schedule(
    prepared_pumping_wells,
    .prepare_numeric_schedule(pumping_schedules, pumping_wells, prepared_model_times),
    prepared_stream_segments,
    prepared_model_times$evaluation_time_day, prepared_model_times$injection_time_day, method,
    prepared_stream_apportionment, allow_n_aquifer_parameter_sets, depletion, quadrature_order
  )
  # Restore user-facing interval times/rates; warnings use caller-facing time labels.
  .restore_injection_output(result, prepared_model_times, units::deparse_unit(pumping_schedules[[pumping_wells$pump_id[[1]]]]))
}

.generate_stream_injection_schedule <- function(
    pumping_wells, pumping_schedules, stream_segments, evaluation_time_day = NULL,
    injection_time_day = NULL, method = "adf", stream_apportionment = NULL,
    allow_n_aquifer_parameter_sets = 5, adf_stream_depletion = NULL, quadrature_order = 4L) {
  # All physical inputs are prepared numeric meters/days; sf geometry retains its shared CRS.
  # Combine pumping changes, evaluations and optional refinement times into intervals.
  grid <- .get_stream_injection_times(pumping_schedules, evaluation_time_day, injection_time_day)
  missing_time_day <- numeric()
  if (method == "adf") {
    if (is.null(adf_stream_depletion)) {
      depletion <- .model_adf_stream_depletion(
        pumping_wells, pumping_schedules, stream_apportionment, grid$boundary_time_day
      )
    } else {
      adf_stream_depletion <- adf_stream_depletion[c(
        "pump_id", "evaluation_time_day", "reach_id", "reach_segment_id", "stream_depletion_rate_m3_per_day"
      )]
      missing_time_day <- grid$boundary_time_day[!grid$boundary_time_day %in% adf_stream_depletion$evaluation_time_day]
      depletion <- adf_stream_depletion[adf_stream_depletion$evaluation_time_day %in% grid$boundary_time_day, ]
      if (length(missing_time_day)) {
        depletion <- rbind(depletion, .model_adf_stream_depletion(
          pumping_wells, pumping_schedules, stream_apportionment, missing_time_day
        ))
      }
    }
    names(depletion)[names(depletion) == "evaluation_time_day"] <- "schedule_boundary_day"
    # For each pump-segment pair, average adjacent depletion rates to assign
    # one constant injection rate to the interval between them.
    stream_injection_schedule <- .get_interval_average_injection_schedule(depletion, pumping_schedules)
  } else {
    # Solve coupled segment rates to maintain zero head at stream model points,
    # accounting for pumping and all preceding injection intervals.
    stream_injection_schedule <- .generate_constant_head_injection_schedule(
      pumping_wells, pumping_schedules, stream_segments, grid,
      quadrature_order = quadrature_order
    )
  }
  attr(stream_injection_schedule, "isw_schedule_metadata") <- list(
    injection_method = method,
    quadrature_order = if (method == "constant_head") as.integer(quadrature_order) else NULL
  )
  attr(stream_injection_schedule, "isw_missing_time_day") <- missing_time_day
  stream_injection_schedule
}
