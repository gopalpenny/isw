# TODO: Before finalizing apportioned aquifer drawdown, review the
# interval-average temporal approximation, use of one model_point injection
# well per reach segment, zero injection-well diameter, convergence with reach
# and evaluation-time spacing, and the pump-specific output structure.

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

#' Calculate interval-average stream-injection rate changes
#'
#' Convert stream-depletion rates evaluated at discrete times into piecewise
#' constant injection rates for aquifer-response superposition.
#'
#' @param stream_depletion Validated output from
#'   [get_apportioned_stream_depletion()].
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

  evaluation_times <- sort(unique(stream_depletion$evaluation_time))
  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  schedule_start <- normalized_times$pumping_times[[1]]
  evaluation_days <- normalized_times$evaluation_times
  prepend_initial_zero <- evaluation_days[[1]] > schedule_start

  if (prepend_initial_zero) {
    time_nodes <- c(schedule_start, evaluation_days)
  } else {
    time_nodes <- evaluation_days
  }

  event_pumps <- character()
  event_reaches <- character()
  event_segments <- character()
  event_times <- time_nodes[0]
  event_rate_changes <- stream_depletion$stream_depletion_rate[0]
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
      endpoint_rates[-length(endpoint_rates)] +
        endpoint_rates[-1]
    ) / 2
    previous_rates <- c(
      interval_rates[[1]] * 0,
      interval_rates[-length(interval_rates)]
    )
    rate_changes <- interval_rates - previous_rates
    nonzero_change <- as.numeric(rate_changes) != 0

    event_pumps <- c(
      event_pumps,
      rep(pump_id, sum(nonzero_change))
    )
    event_reaches <- c(
      event_reaches,
      rep(stream_depletion$reach_id[pair_rows[[1]]], sum(nonzero_change))
    )
    event_segments <- c(
      event_segments,
      rep(reach_segment_id, sum(nonzero_change))
    )
    event_times <- c(
      event_times,
      time_nodes[-length(time_nodes)][nonzero_change]
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

#' Estimate aquifer drawdown with apportioned stream recovery
#'
#' Calculate observation-well drawdown from physical pumping wells and recovery
#' from stream-depletion injection wells using superposition.
#'
#' @param pumping_wells An `sf` pumping-well object accepted by
#'   [`.validate_pumping_wells()`].
#' @param pumping_schedules A shared, wide-format pumping schedule accepted by
#'   [`.validate_pumping_schedules()`].
#' @param observation_wells An `sf` observation-well object accepted by
#'   [`.validate_observation_wells()`].
#' @param stream_apportionment An `sf` object returned by
#'   [get_stream_depletion_apportionment()]. Its `model_point` column supplies
#'   one injection-well location per reach segment.
#' @param stream_depletion A tibble returned by
#'   [get_apportioned_stream_depletion()]. Its evaluation times define the
#'   requested drawdown times.
#'
#' @return A tibble with one row per `pump_id`, `observation_id`, and
#'   `evaluation_time`. `pumping_drawdown` is the positive decline caused by
#'   the physical pumping well, `stream_recovery` is the positive water-level
#'   recovery caused by its apportioned stream injection wells, and
#'   `aquifer_drawdown` is `pumping_drawdown - stream_recovery`. All three
#'   retain length units.
#'
#' @details
#' Physical pumping-well responses use the pumping schedule directly. Stream
#' depletion assigned to each reach segment is represented as injection at the
#' segment's along-line `model_point`. The injection schedule is constructed by
#' [`.get_interval_average_injection_rate_changes()`] and uses the aquifer
#' properties associated with the originating `pump_id`.
#'
#' Both pumping and injection responses use the infinite-aquifer
#' [get_aquifer_drawdown_ratio()] kernel. No image well is included because the
#' apportioned stream injection explicitly represents the stream contribution.
#' Results remain pump-specific so users can inspect individual contributions
#' or sum `aquifer_drawdown` across pumps by observation and evaluation time.
#'
#' This interval-average approach preserves the modeled stream-depletion rate
#' over each evaluation interval but approximates its continuously changing
#' timing. Sensitivity can be assessed by using more closely spaced evaluation
#' times.
#'
#' @examples
#' pumping_wells <- sf::st_as_sf(
#'   tibble::tibble(
#'     pump_id = "pump_1",
#'     x = 0,
#'     y = 0,
#'     K = units::set_units(10, "m/day"),
#'     D = units::set_units(20, "m"),
#'     V = 0.15
#'   ),
#'   coords = c("x", "y"),
#'   crs = 32615
#' )
#'
#' stream_reaches <- sf::st_sf(
#'   reach_id = "stream_1",
#'   geometry = sf::st_sfc(
#'     sf::st_linestring(matrix(c(100, -50, 100, 50), ncol = 2, byrow = TRUE)),
#'     crs = 32615
#'   )
#' )
#'
#' observation_wells <- sf::st_as_sf(
#'   tibble::tibble(observation_id = "obs_1", x = 50, y = 0),
#'   coords = c("x", "y"),
#'   crs = 32615
#' )
#'
#' pumping_schedules <- tibble::tibble(
#'   t = units::set_units(c(0, 10), "days"),
#'   pump_1 = units::set_units(c(100, 0), "m^3/day")
#' )
#' evaluation_times <- units::set_units(c(0, 10, 20), "days")
#'
#' stream_apportionment <- get_stream_depletion_apportionment(
#'   pumping_wells,
#'   stream_reaches,
#'   reach_spacing = units::set_units(100, "m"),
#'   sample_spacing = units::set_units(25, "m"),
#'   analysis_crs = 32615
#' )
#' stream_depletion <- get_apportioned_stream_depletion(
#'   pumping_wells,
#'   pumping_schedules,
#'   stream_apportionment,
#'   evaluation_times
#' )
#'
#' get_apportioned_aquifer_drawdown(
#'   pumping_wells,
#'   pumping_schedules,
#'   observation_wells,
#'   stream_apportionment,
#'   stream_depletion
#' )
#'
#' @export
get_apportioned_aquifer_drawdown <- function(
    pumping_wells,
    pumping_schedules,
    observation_wells,
    stream_apportionment,
    stream_depletion) {

  .validate_observation_wells(observation_wells)
  .validate_apportioned_stream_depletion(
    stream_depletion,
    pumping_wells,
    pumping_schedules,
    stream_apportionment
  )

  if (!("model_point" %in% names(stream_apportionment)) ||
      !inherits(stream_apportionment$model_point, "sfc") ||
      length(stream_apportionment$model_point) != nrow(stream_apportionment) ||
      !all(as.character(sf::st_geometry_type(
        stream_apportionment$model_point
      )) == "POINT") ||
      any(sf::st_is_empty(stream_apportionment$model_point))) {
    stop(
      "stream_apportionment$model_point must contain one nonempty POINT ",
      "geometry per pump--reach-segment row."
    )
  }

  analysis_crs <- sf::st_crs(stream_apportionment)

  if (is.na(analysis_crs) ||
      !grepl("^PROJCRS\\[", analysis_crs$wkt)) {
    stop("stream_apportionment must use a projected CRS.")
  }

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

  injection_points <- sf::st_sf(
    pump_id = stream_apportionment$pump_id,
    reach_segment_id = stream_apportionment$reach_segment_id,
    geometry = sf::st_transform(
      stream_apportionment$model_point,
      analysis_crs
    )
  )
  pump_distances <- sf::st_distance(
    prepared_pumping_wells,
    prepared_observation_wells
  )
  injection_distances <- sf::st_distance(
    injection_points,
    prepared_observation_wells
  )
  evaluation_times <- sort(unique(stream_depletion$evaluation_time))
  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  evaluation_days <- normalized_times$evaluation_times
  pumping_responses <- .get_pumping_response_times(
    pumping_schedules,
    pumping_wells,
    evaluation_times
  )
  injection_events <- .get_interval_average_injection_rate_changes(
    stream_depletion,
    pumping_schedules
  )
  output_length_unit <- units::deparse_unit(pumping_wells$D)
  zero_drawdown <- units::set_units(
    0,
    output_length_unit,
    mode = "standard"
  )

  pump_output <- character()
  observation_output <- character()
  evaluation_output <- evaluation_times[0]
  pumping_output <- zero_drawdown[0]
  recovery_output <- zero_drawdown[0]

  for (pump_id in pumping_wells$pump_id) {
    pump_row <- match(pump_id, prepared_pumping_wells$pump_id)
    injection_rows <- which(injection_points$pump_id == pump_id)

    for (observation_index in seq_len(nrow(prepared_observation_wells))) {
      for (evaluation_index in seq_along(evaluation_times)) {
        pumping_change <- zero_drawdown
        stream_change <- zero_drawdown
        physical_event_rows <- which(
          pumping_responses$pump_id == pump_id &
            as.numeric(pumping_responses$evaluation_time) ==
              as.numeric(evaluation_times[[evaluation_index]])
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
          matched_injection_rows <- injection_rows[match(
            injection_events$reach_segment_id[contributing_injections],
            injection_points$reach_segment_id[injection_rows]
          )]
          elapsed_time <- evaluation_days[[evaluation_index]] -
            injection_events$injection_time[contributing_injections]
          response_ratio <- .theis_aquifer_drawdown_ratio(
            distance = injection_distances[
              matched_injection_rows,
              observation_index
            ],
            K = prepared_pumping_wells$K[[pump_row]],
            D = prepared_pumping_wells$D[[pump_row]],
            V = prepared_pumping_wells$V[[pump_row]],
            t = elapsed_time,
            well_diam = units::set_units(0, "m")
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
          evaluation_times[[evaluation_index]]
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
    aquifer_drawdown = pumping_output - recovery_output
  )
}
