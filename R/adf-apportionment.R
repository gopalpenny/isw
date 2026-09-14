
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

  method <- match.arg(method)
  .validate_pumping_wells(pumping_wells)
  .validate_stream_segments(stream_segments)

  if (!is.null(maximum_distance)) {
    check_dimensionality(maximum_distance, "m", "maximum_distance")

    if (length(maximum_distance) != 1 ||
        !is.finite(as.numeric(maximum_distance)) ||
        as.numeric(maximum_distance) <= 0) {
      stop("maximum_distance must be NULL or a finite, positive scalar length.")
    }
  }

  prepared_pumping_wells <- sf::st_transform(
    sf::st_zm(pumping_wells, drop = TRUE, what = "ZM"),
    sf::st_crs(stream_segments)
  )
  sample_points <- .generate_segment_sample_points(
    stream_segments,
    sample_spacing
  )
  distance_exponent <- if (method == "web_squared") 2 else 1
  number_of_pumps <- nrow(prepared_pumping_wells)
  number_of_segments <- nrow(stream_segments)
  segment_fractions <- matrix(
    0,
    nrow = number_of_pumps,
    ncol = number_of_segments
  )
  exact_distances <- vector("list", number_of_pumps)

  for (pump_index in seq_len(number_of_pumps)) {
    point_distances <- sf::st_distance(
      prepared_pumping_wells[pump_index, ],
      sample_points
    )[1, ]
    exact_distances[[pump_index]] <- sf::st_distance(
      prepared_pumping_wells[pump_index, ],
      stream_segments
    )[1, ]
    distance_units <- units::deparse_unit(point_distances)
    point_distance_values <- as.numeric(point_distances)
    sampled_length_values <- as.numeric(units::set_units(
      sample_points$sampled_length,
      distance_units,
      mode = "standard"
    ))
    eligible <- rep(TRUE, length(point_distances))

    if (!is.null(maximum_distance)) {
      eligible <- point_distance_values <= as.numeric(units::set_units(
        maximum_distance,
        distance_units,
        mode = "standard"
      ))
    }

    if (!any(eligible)) {
      stop(
        "No stream sample points are within maximum_distance for pump_id ",
        prepared_pumping_wells$pump_id[[pump_index]],
        "."
      )
    }

    raw_weights <- numeric(length(point_distances))
    zero_distance <- eligible & point_distance_values == 0

    if (any(zero_distance)) {
      raw_weights[zero_distance] <- sampled_length_values[zero_distance]
    } else {
      raw_weights[eligible] <- sampled_length_values[eligible] /
        point_distance_values[eligible]^distance_exponent
    }

    point_fractions <- raw_weights / sum(raw_weights)

    for (segment_index in seq_len(number_of_segments)) {
      segment_fractions[pump_index, segment_index] <- sum(
        point_fractions[
          sample_points$reach_segment_id ==
            stream_segments$reach_segment_id[[segment_index]]
        ]
      )
    }
  }

  segment_rows <- rep(seq_len(number_of_segments), times = number_of_pumps)
  stream_apportionment <- tibble::tibble(
    pump_id = rep(
      prepared_pumping_wells$pump_id,
      each = number_of_segments
    ),
    reach_id = stream_segments$reach_id[segment_rows],
    reach_segment_id = stream_segments$reach_segment_id[segment_rows],
    pump_to_reach_distance = do.call(c, exact_distances),
    apportionment_fraction = as.vector(t(segment_fractions))
  )
  stream_apportionment
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

  .validate_pumping_wells(pumping_wells)

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

#' Precalculate unique stream-depletion fractions
#'
#' Calculate the analytical stream-depletion fraction once for each unique
#' pump, reach segment, and elapsed pumping time used in a model run.
#'
#' @param pumping_wells A validated pumping-well object.
#' @param pumping_response_times An event table returned by
#'   [`.get_pumping_response_times()`].
#' @param stream_apportionment A validated stream-depletion apportionment
#'   object.
#'
#' @return A tibble with `pump_id`, `reach_id`, `reach_segment_id`,
#'   `elapsed_time`, and `stream_depletion_fraction`. Elapsed time retains
#'   internal units of days.
#'
#' @details
#' For fixed pump properties and pump-to-segment distance, the analytical
#' fraction depends on elapsed time but not on the pumping-rate magnitude or
#' the absolute pumping and evaluation dates. Deduplicating elapsed times
#' avoids repeating the numerical kernel for identical response periods.
#'
#' @noRd
.get_stream_depletion_fraction_lookup <- function(
    pumping_wells,
    pumping_response_times,
    stream_apportionment) {

  elapsed_output <- pumping_response_times$elapsed_time[0]
  pump_output <- character()
  reach_output <- character()
  segment_output <- character()
  fraction_output <- numeric()
  apportionment_table <- sf::st_drop_geometry(stream_apportionment)

  for (pump_id in pumping_wells$pump_id) {
    pump_response_rows <- pumping_response_times$pump_id == pump_id
    elapsed_values <- sort(unique(as.numeric(
      pumping_response_times$elapsed_time[pump_response_rows]
    )))

    if (length(elapsed_values) == 0) {
      next
    }

    segment_table <- apportionment_table[
      apportionment_table$pump_id == pump_id,
      ,
      drop = FALSE
    ]
    pump_row <- match(pump_id, pumping_wells$pump_id)

    for (elapsed_value in elapsed_values) {
      elapsed_time <- units::set_units(
        elapsed_value,
        "days",
        mode = "standard"
      )
      number_of_segments <- nrow(segment_table)
      pump_output <- c(pump_output, rep(pump_id, number_of_segments))
      reach_output <- c(reach_output, segment_table$reach_id)
      segment_output <- c(
        segment_output,
        segment_table$reach_segment_id
      )
      elapsed_output <- c(
        elapsed_output,
        rep(elapsed_time, number_of_segments)
      )
      fraction_output <- c(
        fraction_output,
        .glover_stream_depletion_fraction(
          x1 = segment_table$pump_to_reach_distance,
          K = pumping_wells$K[[pump_row]],
          D = pumping_wells$D[[pump_row]],
          V = pumping_wells$V[[pump_row]],
          t = elapsed_time
        )
      )
    }
  }

  tibble::tibble(
    pump_id = pump_output,
    reach_id = reach_output,
    reach_segment_id = segment_output,
    elapsed_time = elapsed_output,
    stream_depletion_fraction = fraction_output
  )
}

# Apply normalized ADF apportionment to pumping events and evaluation times.
.calculate_adf_stream_depletion <- function(
    pumping_wells,
    pumping_schedules,
    stream_apportionment,
    evaluation_times = NULL) {

  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  .validate_evaluation_times(evaluation_times, pumping_schedules$t)
  .validate_stream_depletion_apportionment(
    stream_apportionment,
    pumping_wells
  )

  pumping_response_times <- .get_pumping_response_times(
    pumping_schedules,
    pumping_wells,
    evaluation_times
  )
  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )
  output_evaluation_times <- normalized_times$output_evaluation_times
  fraction_lookup <- .get_stream_depletion_fraction_lookup(
    pumping_wells,
    pumping_response_times,
    stream_apportionment
  )
  apportionment_table <- sf::st_drop_geometry(stream_apportionment)

  pump_output <- character()
  evaluation_output <- output_evaluation_times[0]
  reach_output <- character()
  segment_output <- character()
  rate_output <- pumping_schedules[[pumping_wells$pump_id[[1]]]][0]

  for (pump_id in pumping_wells$pump_id) {
    segment_table <- apportionment_table[
      apportionment_table$pump_id == pump_id,
      ,
      drop = FALSE
    ]
    number_of_segments <- nrow(segment_table)
    zero_rates <- rep(
      pumping_schedules[[pump_id]][[1]] * 0,
      number_of_segments
    )

    for (evaluation_index in seq_along(output_evaluation_times)) {
      evaluation_time <- output_evaluation_times[evaluation_index]
      event_rows <- which(
        pumping_response_times$pump_id == pump_id &
          as.numeric(pumping_response_times$evaluation_time) ==
            as.numeric(evaluation_time)
      )
      segment_rates <- zero_rates

      for (event_row in event_rows) {
        elapsed_value <- as.numeric(
          pumping_response_times$elapsed_time[[event_row]]
        )
        lookup_rows <- which(
          fraction_lookup$pump_id == pump_id &
            as.numeric(fraction_lookup$elapsed_time) == elapsed_value
        )
        lookup_rows <- lookup_rows[match(
          segment_table$reach_segment_id,
          fraction_lookup$reach_segment_id[lookup_rows]
        )]
        segment_rates <- segment_rates +
          pumping_response_times$pumping_rate_change[[event_row]] *
          fraction_lookup$stream_depletion_fraction[lookup_rows] *
          segment_table$apportionment_fraction
      }

      pump_output <- c(pump_output, rep(pump_id, number_of_segments))
      evaluation_output <- c(
        evaluation_output,
        rep(evaluation_time, number_of_segments)
      )
      reach_output <- c(reach_output, segment_table$reach_id)
      segment_output <- c(
        segment_output,
        segment_table$reach_segment_id
      )
      rate_output <- c(rate_output, segment_rates)
    }
  }

  tibble::tibble(
    pump_id = pump_output,
    evaluation_time = evaluation_output,
    reach_id = reach_output,
    reach_segment_id = segment_output,
    stream_depletion_rate = rate_output
  )
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
#'   vector. When `NULL`, `pumping_schedules$t` is used.
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

  .calculate_adf_stream_depletion(
    pumping_wells,
    pumping_schedules,
    stream_apportionment,
    evaluation_times
  )
}
