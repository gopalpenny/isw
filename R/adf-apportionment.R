
#' Calculate ADF stream apportionment for existing stream segments
#'
#' Calculate static web or web-squared fractions assigning a pumping well's
#' analytical stream depletion to a prepared stream-segment network.
#'
#' @param pumping_wells A nonempty `sf` object with one point per pumping well.
#'   It must contain unique character `pump_id` values, hydraulic conductivity
#'   `K` with length-per-time units, aquifer thickness `D` with length units,
#'   and dimensionless drainable porosity or specific yield `V`. Geometry is
#'   transformed internally to the CRS of `stream_segments`.
#' @param stream_segments A projected `sf` object returned by
#'   [prep_stream_segments()].
#' @param sample_spacing A scalar `units` length giving the maximum stream
#'   length represented by an apportionment sample point.
#' @param method Character string selecting `"web_squared"` or `"web"`.
#' @param maximum_distance Either `NULL` or a positive scalar `units` length.
#'   Sample points farther than this distance from a pump receive zero weight.
#'
#' @return A tibble with one row per pump--stream-segment pair and columns
#'   `pump_id`, `reach_id`, `reach_segment_id`, `pump_to_reach_distance`, and
#'   `apportionment_fraction`. Physical segment geometry and attributes remain
#'   in `stream_segments` and are not duplicated in the returned table.
#'
#' @details
#' Apportionment is static: it describes where a pump's analytical depletion
#' is assigned, not how depletion changes through time. Use
#' [model_adf_stream_depletion()] to combine it with pumping schedules.
#'
#' Each stream segment is represented by points spaced no farther apart than
#' `sample_spacing`. A point representing length \eqn{L_i} at distance \eqn{d_i}
#' from a pump receives raw weight \eqn{L_i / d_i^p}, where \eqn{p = 2} for
#' `"web_squared"` and \eqn{p = 1} for `"web"`. Point weights are summed by
#' segment and normalized so each pump's segment fractions sum to one.
#' `maximum_distance`, when supplied, excludes more distant sample points before
#' normalization. If an eligible sample point is exactly colocated with a pump,
#' only the eligible zero-distance points receive weight. The reported
#' `pump_to_reach_distance` is the exact point-to-segment distance, not a sampled
#' distance.
#'
#' @seealso [prep_stream_segments()], [model_adf_stream_depletion()]
#'
#' @examples
#' stream_segments <- prep_stream_segments(
#'   example_stream_reaches,
#'   units::set_units(100, "m")
#' )
#' prep_adf_stream_apportionment(
#'   example_pumping_wells,
#'   stream_segments,
#'   sample_spacing = units::set_units(25, "m")
#' )
#'
#' @export
prep_adf_stream_apportionment <- function(
    pumping_wells,
    stream_segments,
    sample_spacing,
    method = c("web_squared", "web"),
    maximum_distance = NULL) {

  # Validate unit-bearing inputs before preparing numeric meter/day columns.
  method <- match.arg(method)
  segments <- .prepare_numeric_segments(stream_segments)
  wells <- .prepare_numeric_wells(pumping_wells, sf::st_crs(segments))
  .validate_positive_length(sample_spacing, "sample_spacing")
  if (!is.null(maximum_distance)) {
    .validate_positive_length(maximum_distance, "maximum_distance")
  }
  sample_spacing_m <- .numeric_in_standard_units(sample_spacing, "m", "sample_spacing")
  maximum_distance_m <- if (!is.null(maximum_distance)) {
    .numeric_in_standard_units(maximum_distance, "m", "maximum_distance")
  } else NULL
  result <- .prep_adf_stream_apportionment(
    wells,
    segments,
    sample_spacing_m, method, maximum_distance_m
  )
  # Restore public distance units after numeric apportionment.
  .restore_spatial_columns(result, c(pump_to_reach_distance = "m"))
}

.prep_adf_stream_apportionment <- function(
    pumping_wells, stream_segments, sample_spacing_m,
    method = "web_squared", maximum_distance_m = NULL) {
  # Validated sf inputs retain CRS; lengths/distances and sample spacing are numeric meters.
  pumping_wells <- .transform_spatial_geometry(pumping_wells, sf::st_crs(stream_segments))
  sample_points <- .prep_stream_sample_points(stream_segments, sample_spacing_m)
  # Measure all pumps once; strip sf distance units only after meter normalization.
  pump_to_sample_distance_m <- as.matrix(units::drop_units(units::set_units(
    sf::st_distance(pumping_wells, sample_points), "m"
  )))
  pump_to_segment_distance_m <- as.matrix(units::drop_units(units::set_units(
    sf::st_distance(pumping_wells, stream_segments), "m"
  )))
  distance_exponent <- if (method == "web_squared") 2 else 1
  number_of_pumps <- nrow(pumping_wells)
  number_of_segments <- nrow(stream_segments)
  segment_fractions <- matrix(0, nrow = number_of_pumps, ncol = number_of_segments)
  # For each pump, weight sampled stream lengths by distance, then aggregate
  # normalized point weights to the modeled segments.
  for (pump_index in seq_len(number_of_pumps)) {
    point_distance_m <- pump_to_sample_distance_m[pump_index, ]
    # All samples are eligible unless maximum_distance_m excludes distant points.
    eligible <- if (is.null(maximum_distance_m)) rep(TRUE, length(point_distance_m)) else {
      point_distance_m <= maximum_distance_m
    }
    if (!any(eligible)) {
      stop("No stream sample points are within maximum_distance for pump_id ",
           pumping_wells$pump_id[[pump_index]], ".")
    }
    raw_weights <- numeric(length(point_distance_m))
    zero_distance <- eligible & point_distance_m == 0
    # Each sample represents its surrounding stream length. Weight by length/distance^p;
    # if any eligible samples are colocated with the pump, only those receive weight.
    if (any(zero_distance)) {
      raw_weights[zero_distance] <- sample_points$sampled_length_m[zero_distance]
    } else {
      raw_weights[eligible] <- sample_points$sampled_length_m[eligible] /
        point_distance_m[eligible]^distance_exponent
    }
    point_fractions <- raw_weights / sum(raw_weights)
    # Sum each segment's normalized sample weights.
    for (segment_index in seq_len(number_of_segments)) {
      segment_fractions[pump_index, segment_index] <- sum(point_fractions[
        sample_points$reach_segment_id == stream_segments$reach_segment_id[[segment_index]]
      ])
    }
  }
  segment_rows <- rep(seq_len(number_of_segments), times = number_of_pumps)
  tibble::tibble(
    pump_id = rep(pumping_wells$pump_id, each = number_of_segments),
    reach_id = stream_segments$reach_id[segment_rows],
    reach_segment_id = stream_segments$reach_segment_id[segment_rows],
    pump_to_reach_distance_m = as.numeric(t(pump_to_segment_distance_m)),
    apportionment_fraction = as.vector(t(segment_fractions))
  )
}

# Validate a stream-depletion apportionment object.
.validate_stream_depletion_apportionment <- function(
    stream_apportionment,
    pumping_wells) {

  if (!is.data.frame(stream_apportionment) ||
      nrow(stream_apportionment) == 0) {
    stop("stream_apportionment must be a nonempty data frame or tibble.")
  }

  required_columns <- c(
    "pump_id",
    "reach_id",
    "reach_segment_id",
    "pump_to_reach_distance",
    "apportionment_fraction"
  )
  missing_columns <- setdiff(required_columns, names(stream_apportionment))

  if (length(missing_columns) > 0) {
    stop(
      "stream_apportionment is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  # Pumping wells were checked by the public boundary; validate this relationship only.

  if (!is.character(stream_apportionment$pump_id) ||
      !is.character(stream_apportionment$reach_id) ||
      !is.character(stream_apportionment$reach_segment_id) ||
      anyNA(stream_apportionment$pump_id) ||
      anyNA(stream_apportionment$reach_id) ||
      anyNA(stream_apportionment$reach_segment_id) ||
      any(trimws(stream_apportionment$pump_id) == "") ||
      any(trimws(stream_apportionment$reach_id) == "") ||
      any(trimws(stream_apportionment$reach_segment_id) == "")) {
    stop(
      "stream_apportionment identifiers must be nonmissing, nonempty ",
      "character values."
    )
  }

  if (!setequal(unique(stream_apportionment$pump_id), pumping_wells$pump_id)) {
    stop(
      "stream_apportionment$pump_id values must match ",
      "pumping_wells$pump_id values."
    )
  }

  pair_keys <- paste(
    stream_apportionment$pump_id,
    stream_apportionment$reach_segment_id,
    sep = "\r"
  )

  if (anyDuplicated(pair_keys) > 0) {
    stop(
      "stream_apportionment must contain one row per ",
      "pump_id and reach_segment_id pair."
    )
  }

  segment_reach_counts <- vapply(
    unique(stream_apportionment$reach_segment_id),
    function(segment_id) {
      length(unique(stream_apportionment$reach_id[
        stream_apportionment$reach_segment_id == segment_id
      ]))
    },
    integer(1)
  )
  if (any(segment_reach_counts != 1L)) {
    stop(
      "Each stream_apportionment$reach_segment_id must map to exactly one ",
      "reach_id."
    )
  }

  check_dimensionality(
    stream_apportionment$pump_to_reach_distance,
    desired_units = "m",
    variable_name = "stream_apportionment$pump_to_reach_distance"
  )

  if (any(!is.finite(as.numeric(
    stream_apportionment$pump_to_reach_distance
  ))) || any(as.numeric(
    stream_apportionment$pump_to_reach_distance
  ) < 0)) {
    stop(
      "stream_apportionment$pump_to_reach_distance must contain finite, ",
      "nonnegative values."
    )
  }

  fractions <- stream_apportionment$apportionment_fraction

  if (!is.numeric(fractions) || inherits(fractions, "units") ||
      any(!is.finite(fractions)) || any(fractions < 0 | fractions > 1)) {
    stop(
      "stream_apportionment$apportionment_fraction must contain finite ",
      "values from 0 to 1."
    )
  }

  fraction_sums <- vapply(
    pumping_wells$pump_id,
    function(pump_id) {
      sum(fractions[stream_apportionment$pump_id == pump_id])
    },
    numeric(1)
  )

  if (any(abs(fraction_sums - 1) > sqrt(.Machine$double.eps))) {
    stop(
      "stream_apportionment$apportionment_fraction must sum to 1 within ",
      "each pump_id."
    )
  }

  stream_apportionment
}

# Validate the foreign-key relationship between ADF apportionment and segments.
.validate_stream_apportionment_segments <- function(
    stream_apportionment,
    stream_segments) {

  segment_ids <- stream_segments$reach_segment_id
  apportionment_ids <- unique(stream_apportionment$reach_segment_id)

  if (!setequal(apportionment_ids, segment_ids)) {
    stop(
      "stream_apportionment$reach_segment_id values must match ",
      "stream_segments$reach_segment_id values."
    )
  }

  matched_segments <- match(
    stream_apportionment$reach_segment_id,
    segment_ids
  )
  if (!identical(
    stream_apportionment$reach_id,
    stream_segments$reach_id[matched_segments]
  )) {
    stop(
      "stream_apportionment$reach_id must match each reach_segment_id in ",
      "stream_segments."
    )
  }

  stream_apportionment
}

# Build one numeric Glover response for each pump, segment and distinct event age.
.get_stream_depletion_fraction_lookup <- function(
    pumping_wells, pumping_response_times, stream_apportionment) {
  # Validated numeric distances/thickness are m, conductivity m/day and event ages day.
  # Build reusable fractions for every pump, segment and distinct elapsed response time.
  parts <- lapply(seq_len(nrow(pumping_wells)), function(pump_row) {
    pump_id <- pumping_wells$pump_id[[pump_row]]
    elapsed_time_day <- sort(unique(pumping_response_times$elapsed_time_day[
      pumping_response_times$pump_id == pump_id
    ]))
    segments <- stream_apportionment[stream_apportionment$pump_id == pump_id, ]
    n_segments <- nrow(segments)
    lookup_elapsed_time_day <- rep(elapsed_time_day, each = n_segments)
    # Batch all segment/elapsed-time combinations in one analytical call per pump.
    fractions <- .glover_stream_depletion_fraction(
      rep(segments$pump_to_reach_distance_m, times = length(elapsed_time_day)),
      pumping_wells$K_m_per_day[[pump_row]], pumping_wells$D_m[[pump_row]],
      pumping_wells$V[[pump_row]], lookup_elapsed_time_day
    )
    tibble::tibble(
      pump_id = rep(pump_id, length(lookup_elapsed_time_day)),
      reach_id = rep(segments$reach_id, times = length(elapsed_time_day)),
      reach_segment_id = rep(segments$reach_segment_id, times = length(elapsed_time_day)),
      elapsed_time_day = lookup_elapsed_time_day,
      stream_depletion_fraction = fractions
    )
  })
  do.call(rbind, parts)
}

.calculate_adf_stream_depletion <- function(
    pumping_wells, pumping_schedules, stream_apportionment, evaluation_time_day) {
  # Prepared numeric rates are m^3/day, time day, hydraulic inputs m/day and m.
  n_evaluations <- length(evaluation_time_day)
  events <- attr(pumping_schedules, "isw_pumping_events")
  # Pair each nonzero rate change with later evaluations, reusing prepared pairs
  # when the requested time vector matches the public boundary's preparation.
  responses <- if (identical(evaluation_time_day, attr(pumping_schedules, "isw_evaluation_time_day"))) {
    attr(pumping_schedules, "isw_pumping_response_times")
  } else .get_pumping_response_times(events, evaluation_time_day)
  # Precalculate distinct temporal fractions before pump-specific superposition.
  lookup <- .get_stream_depletion_fraction_lookup(pumping_wells, responses, stream_apportionment)
  parts <- lapply(pumping_wells$pump_id, function(pump_id) {
    segments <- stream_apportionment[stream_apportionment$pump_id == pump_id, ]
    pump_responses <- responses[responses$pump_id == pump_id, ]
    pump_lookup <- lookup[lookup$pump_id == pump_id, ]
    elapsed_time_day <- unique(pump_lookup$elapsed_time_day)
    # Fraction rows match this pump's segments; columns identify distinct event ages.
    fractions <- matrix(pump_lookup$stream_depletion_fraction, nrow = nrow(segments))
    elapsed_indices <- match(pump_responses$elapsed_time_day, elapsed_time_day)
    rates_m3_per_day <- matrix(0, nrow(segments), length(evaluation_time_day))
    for (event in seq_len(nrow(pump_responses))) {
      column <- pump_responses$evaluation_index[[event]]
      # Scale each event by its temporal response and static spatial apportionment,
      # adding one segment vector to the requested evaluation column.
      rates_m3_per_day[, column] <- rates_m3_per_day[, column] +
        pump_responses$pumping_rate_change_m3_per_day[[event]] *
        fractions[, elapsed_indices[[event]]] * segments$apportionment_fraction
    }
    # Emit one row per segment and evaluation for this pump, including zero responses.
    tibble::tibble(
      pump_id = rep(pump_id, length(rates_m3_per_day)),
      evaluation_time_day = rep(evaluation_time_day, each = nrow(segments)),
      reach_id = rep(segments$reach_id, times = n_evaluations),
      reach_segment_id = rep(segments$reach_segment_id, times = n_evaluations),
      stream_depletion_rate_m3_per_day = as.numeric(rates_m3_per_day)
    )
  })
  do.call(rbind, parts)
}

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
  wells <- .prepare_numeric_wells(pumping_wells)
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_stream_depletion_apportionment(stream_apportionment, pumping_wells)
  times <- .prepare_numeric_model_times(pumping_schedules, evaluation_times)
  result <- .calculate_adf_stream_depletion(
    wells,
    .prepare_numeric_schedule(pumping_schedules, pumping_wells, times),
    .prepare_numeric_apportionment(stream_apportionment, pumping_wells),
    times$evaluation_time_day
  )
  # Restore public evaluation times and original pumping-rate units.
  .restore_adf_output(result, times, units::deparse_unit(pumping_schedules[[pumping_wells$pump_id[[1]]]]))
}
