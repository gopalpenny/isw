#' Model ADF stream depletion
#'
#' Apply an ADF stream apportionment to intermittent pumping schedules using
#' Glover response fractions and superposition.
#'
#' @param pumping_wells A nonempty `sf` object with one point per pumping well.
#'   It must contain unique character `pump_id` values, hydraulic conductivity
#'   `K` with length-per-time units, aquifer thickness `D` with length units,
#'   and dimensionless drainable porosity or specific yield `V`.
#' @param pumping_schedules A wide data frame with a strictly increasing `t`
#'   column and one pumping-rate column for every `pump_id`. Times must be all
#'   `Date` values or all `units` time values. Pumping rates must have consistent
#'   volume-per-time units.
#' @param stream_apportionment A normalized relationship table returned by
#'   [prep_adf_stream_apportionment()].
#' @param evaluation_times Either `NULL`, a `Date` vector, or a `units` time
#'   vector. When `NULL`, `pumping_schedules$t` is used. When supplied, only
#'   these requested times are returned; pumping-schedule times are not added
#'   automatically.
#'
#' @return A tibble with one row for every requested `pump_id`,
#'   `evaluation_time`, and `reach_segment_id` combination. Columns are
#'   `pump_id`, `evaluation_time`, `reach_id`, `reach_segment_id`, and
#'   `stream_depletion_rate`. The rate is positive for depletion and retains
#'   the pumping-rate units.
#'
#' @details
#' Each pumping-rate change is evaluated with the Glover--Balmer response and
#' assigned to segments using `apportionment_fraction`. Responses are summed by
#' superposition. A change beginning exactly at an evaluation time does not
#' contribute until a later time. Every requested pump, time, and segment is
#' returned, including zero-response combinations.
#'
#' @seealso [prep_adf_stream_apportionment()],
#'   [generate_stream_injection_schedule()]
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
#' model_adf_stream_depletion(
#'   example_pumping_wells,
#'   pumping_schedules,
#'   stream_apportionment
#' )
#'
#' @export
model_adf_stream_depletion <- function(
    pumping_wells,
    pumping_schedules,
    stream_apportionment,
    evaluation_times = NULL) {
  
  # Validate original inputs, then strip units using the shared meter/day boundary.
  prepared_pumping_wells <- .prepare_numeric_pumping_wells(pumping_wells)
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_depletion_apportionment(stream_apportionment, pumping_wells)
  prepared_model_times <- .prepare_numeric_model_times(pumping_schedules, evaluation_times)
  result <- .model_adf_stream_depletion(
    prepared_pumping_wells,
    .prepare_numeric_schedule(pumping_schedules, pumping_wells, prepared_model_times),
    .prepare_numeric_apportionment(stream_apportionment, pumping_wells),
    prepared_model_times$evaluation_time_day
  )
  # Restore user-facing evaluation times and original pumping-rate units.
  .restore_adf_output(result, prepared_model_times, units::deparse_unit(pumping_schedules[[pumping_wells$pump_id[[1]]]]))
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
  
  # Context tables were checked at the user-facing boundary; supplied depletion is new.
  
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
  evaluation_times_day <- as.numeric(normalized_times$target_time_day)
  row_evaluation_time_day <- if (inherits(evaluation_times, "Date")) {
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
    row_evaluation_time_day,
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
    
    for (evaluation_time_day in evaluation_times_day) {
      expected_keys <- c(
        expected_keys,
        paste(
          pump_id,
          pump_segments$reach_segment_id,
          evaluation_time_day,
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

.model_adf_stream_depletion <- function(
    pumping_wells, pumping_schedules, stream_apportionment, evaluation_time_day) {
  # Prepared numeric rates are m^3/day, time day, hydraulic inputs m/day and m.
  n_evaluations <- length(evaluation_time_day)
  pumping_events <- attr(pumping_schedules, "isw_pumping_events")

  # Pair each nonzero rate change with later evaluations, reusing prepared pairs
  # when the requested time vector matches the user-facing boundary's preparation.
  pumping_response_times <- if (identical(
    evaluation_time_day,
    attr(pumping_schedules, "isw_evaluation_time_day")
  )) {
    attr(pumping_schedules, "isw_pumping_response_times")
  } else .get_pumping_response_times(pumping_events, evaluation_time_day)

  # Precalculate distinct temporal fractions before pump-specific superposition.
  depletion_fraction_lookup <- .get_stream_depletion_fraction_lookup(
    pumping_wells,
    pumping_response_times,
    stream_apportionment
  )

  stream_depletion_by_pump <- vector("list", nrow(pumping_wells))
  for (pump_row in seq_len(nrow(pumping_wells))) {
    pump_id <- pumping_wells$pump_id[[pump_row]]

    # Select the spatial weights, event ages and response fractions for this pump.
    pump_apportionment <- stream_apportionment[
      stream_apportionment$pump_id == pump_id,
    ]
    pump_responses <- pumping_response_times[
      pumping_response_times$pump_id == pump_id,
    ]
    pump_lookup <- depletion_fraction_lookup[
      depletion_fraction_lookup$pump_id == pump_id,
    ]
    elapsed_time_day <- unique(pump_lookup$elapsed_time_day)

    # Fraction rows match this pump's segments; columns identify distinct event ages.
    stream_depletion_fractions <- matrix(
      pump_lookup$stream_depletion_fraction,
      nrow = nrow(pump_apportionment)
    )
    elapsed_indices <- match(pump_responses$elapsed_time_day, elapsed_time_day)

    # Rate rows are segments; columns are requested evaluation times.
    stream_depletion_rates_m3_per_day <- matrix(
      0, nrow(pump_apportionment), length(evaluation_time_day)
    )
    for (event in seq_len(nrow(pump_responses))) {
      column <- pump_responses$evaluation_index[[event]]

      # Scale each event by its temporal response and static spatial apportionment,
      # adding one segment vector to the requested evaluation column.
      stream_depletion_rates_m3_per_day[, column] <- stream_depletion_rates_m3_per_day[, column] +
        pump_responses$pumping_rate_change_m3_per_day[[event]] *
        stream_depletion_fractions[, elapsed_indices[[event]]] *
        pump_apportionment$apportionment_fraction
    }

    # Emit one row per segment and evaluation for this pump, including zero responses.
    stream_depletion_by_pump[[pump_row]] <- tibble::tibble(
      pump_id = rep(pump_id, length(stream_depletion_rates_m3_per_day)),
      evaluation_time_day = rep(evaluation_time_day, each = nrow(pump_apportionment)),
      reach_id = rep(pump_apportionment$reach_id, times = n_evaluations),
      reach_segment_id = rep(pump_apportionment$reach_segment_id, times = n_evaluations),
      stream_depletion_rate_m3_per_day = as.numeric(stream_depletion_rates_m3_per_day)
    )
  }

  do.call(rbind, stream_depletion_by_pump)
}

# Build one numeric Glover response for each pump, segment and distinct event age.
.get_stream_depletion_fraction_lookup <- function(
    pumping_wells, pumping_response_times, stream_apportionment) {
  # Validated numeric distances/thickness are m, conductivity m/day and event ages day.
  
  # Build reusable fractions for every pump, segment and distinct elapsed response time.
  depletion_fraction_lookups_by_pump <- lapply(
    seq_len(nrow(pumping_wells)),
    .get_depletion_fraction_lookup_for_pump,
    pumping_wells = pumping_wells,
    pumping_response_times = pumping_response_times,
    stream_apportionment = stream_apportionment
  )
  
  do.call(rbind, depletion_fraction_lookups_by_pump)
}

# Build one pump's segment-by-age lookup without repeating boundary validation.
.get_depletion_fraction_lookup_for_pump <- function(
    pump_row, pumping_wells, pumping_response_times, stream_apportionment) {
  # Validated numeric distances/thickness are m, conductivity m/day and event ages day.

  pump_id <- pumping_wells$pump_id[[pump_row]]
  elapsed_time_day <- sort(unique(pumping_response_times$elapsed_time_day[
    pumping_response_times$pump_id == pump_id
  ]))
  pump_apportionment <- stream_apportionment[
    stream_apportionment$pump_id == pump_id,
  ]
  n_segments <- nrow(pump_apportionment)
  lookup_elapsed_time_day <- rep(elapsed_time_day, each = n_segments)

  # Batch all segment/elapsed-time combinations in one analytical call per pump.
  stream_depletion_fractions <- .glover_stream_depletion_fraction(
    rep(pump_apportionment$pump_to_reach_distance_m, times = length(elapsed_time_day)),
    pumping_wells$K_m_per_day[[pump_row]], pumping_wells$D_m[[pump_row]],
    pumping_wells$V[[pump_row]], lookup_elapsed_time_day
  )

  tibble::tibble(
    pump_id = rep(pump_id, length(lookup_elapsed_time_day)),
    reach_id = rep(pump_apportionment$reach_id, times = length(elapsed_time_day)),
    reach_segment_id = rep(pump_apportionment$reach_segment_id, times = length(elapsed_time_day)),
    elapsed_time_day = lookup_elapsed_time_day,
    stream_depletion_fraction = stream_depletion_fractions
  )
}


