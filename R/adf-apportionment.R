#' Calculate stream-reach apportionment
#'
#' Discretize a stream network and calculate the static fraction of a pumping
#' well's analytical stream depletion assigned to each reach segment.
#'
#' @param pumping_wells An `sf` pumping-well object accepted by
#'   [`.validate_pumping_wells()`].
#' @param stream_reaches An `sf` stream-reach object accepted by
#'   [`.validate_stream_reaches()`].
#' @param reach_spacing A scalar `units` length giving the maximum length of a
#'   modeled reach segment.
#' @param sample_spacing A scalar `units` length giving the maximum stream
#'   length represented by an apportionment sample point within a reach
#'   segment.
#' @param method Character string selecting `"web"` or `"web_squared"`.
#'   Web weighting uses inverse distance; web-squared weighting uses inverse
#'   distance squared.
#' @param maximum_distance Either `NULL` or a scalar `units` length. Sample
#'   points farther than this distance from a pump receive zero weight. When
#'   `NULL`, all sample points are included.
#' @param analysis_crs Either `NULL` or a projected coordinate reference system
#'   accepted by [sf::st_crs()]. When `NULL`, a local UTM CRS is selected
#'   automatically.
#'
#' @return An `sf` object with one row per pump--reach-segment pair. It contains
#'   `pump_id`, `reach_id`, `reach_segment_id`, `represented_length`,
#'   `pump_to_reach_distance`, `apportionment_fraction`, `model_point`, and the
#'   reach-segment line geometry. Additional stream attributes are retained.
#'
#' @details
#' Stream reaches are divided using [`.discretize_stream_reaches()`], and each
#' reach segment is sampled using [sample_reach_segments()]. For sample point
#' \eqn{j}, the unnormalized web weight is:
#'
#' \deqn{w_j = \frac{L_j}{d_j^p}}
#'
#' where \eqn{L_j} is `sampled_length`, \eqn{d_j} is the Euclidean distance
#' from the pump to the point, and \eqn{p} is 1 for `"web"` or 2 for
#' `"web_squared"`. Point weights are normalized within each pump and summed
#' by `reach_segment_id`. Consequently, `apportionment_fraction` sums to one
#' across the reach segments associated with each `pump_id`.
#'
#' `pump_to_reach_distance` is calculated separately as the shortest distance
#' from the pump point to the reach-segment line. It is therefore independent
#' of `sample_spacing` and is the distance supplied to the analytical
#' stream-depletion kernel.
#'
#' If a pump coincides with one or more sample points, only those zero-distance
#' points receive weight, divided in proportion to their `sampled_length`.
#'
#' @references
#' Zipper, S. C., Dallemagne, T., Gleeson, T., Boerman, T. C., and Hartmann, A.
#' (2018). Groundwater Pumping Impacts on Real Stream Networks: Testing the
#' Performance of Simple Management Tools. *Water Resources Research*, 54,
#' 5471--5486. \doi{10.1029/2018WR022707}
#'
#' @examples
#' pumping_wells <- example_pumping_wells
#' stream_reaches <- example_stream_reaches
#'
#' stream_apportionment <- get_stream_reach_apportionment(
#'   pumping_wells,
#'   stream_reaches,
#'   reach_spacing = units::set_units(100, "m"),
#'   sample_spacing = units::set_units(25, "m"),
#'   method = "web_squared"
#' )
#'
#' stream_apportionment[c(
#'   "pump_id", "reach_id", "reach_segment_id",
#'   "pump_to_reach_distance", "apportionment_fraction"
#' )]
#' tapply(
#'   stream_apportionment$apportionment_fraction,
#'   stream_apportionment$pump_id,
#'   sum
#' )
#'
#' @export
get_stream_reach_apportionment <- function(
    pumping_wells,
    stream_reaches,
    reach_spacing,
    sample_spacing,
    method = c("web_squared", "web"),
    maximum_distance = NULL,
    analysis_crs = NULL) {

  method <- match.arg(method)

  if (!is.null(maximum_distance)) {
    check_dimensionality(
      maximum_distance,
      desired_units = "m",
      variable_name = "maximum_distance"
    )

    if (length(maximum_distance) != 1 ||
        !is.finite(as.numeric(maximum_distance)) ||
        as.numeric(maximum_distance) <= 0) {
      stop("maximum_distance must be NULL or a finite, positive scalar length.")
    }
  }

  spatial_inputs <- .prepare_spatial_inputs(
    pumping_wells,
    stream_reaches,
    analysis_crs = analysis_crs
  )
  prepared_pumping_wells <- spatial_inputs$pumping_wells
  reach_segments <- .discretize_stream_reaches(
    spatial_inputs$stream_reaches,
    reach_spacing
  )
  sample_points <- sample_reach_segments(
    reach_segments,
    sample_spacing
  )

  distance_exponent <- if (method == "web_squared") 2 else 1
  number_of_pumps <- nrow(prepared_pumping_wells)
  number_of_segments <- nrow(reach_segments)
  segment_fractions <- matrix(
    0,
    nrow = number_of_pumps,
    ncol = number_of_segments
  )
  exact_distances <- vector("list", number_of_pumps)

  for (pump_index in seq_len(number_of_pumps)) {
    point_distance_matrix <- sf::st_distance(
      prepared_pumping_wells[pump_index, ],
      sample_points
    )
    point_distances <- point_distance_matrix[1, ]
    exact_distance_matrix <- sf::st_distance(
      prepared_pumping_wells[pump_index, ],
      reach_segments
    )
    exact_distances[[pump_index]] <- exact_distance_matrix[1, ]
    distance_units <- units::deparse_unit(point_distances)
    point_distance_values <- as.numeric(point_distances)
    sampled_length_values <- as.numeric(units::set_units(
      sample_points$sampled_length,
      distance_units,
      mode = "standard"
    ))

    if (is.null(maximum_distance)) {
      eligible <- rep(TRUE, length(point_distances))
    } else {
      maximum_distance_value <- as.numeric(units::set_units(
        maximum_distance,
        distance_units,
        mode = "standard"
      ))
      eligible <- point_distance_values <= maximum_distance_value
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
            reach_segments$reach_segment_id[[segment_index]]
        ]
      )
    }
  }

  segment_rows <- rep(seq_len(number_of_segments), times = number_of_pumps)
  stream_apportionment <- reach_segments[segment_rows, , drop = FALSE]
  stream_apportionment$pump_id <- rep(
    prepared_pumping_wells$pump_id,
    each = number_of_segments
  )
  stream_apportionment$pump_to_reach_distance <- do.call(
    c,
    exact_distances
  )
  stream_apportionment$apportionment_fraction <- as.vector(
    t(segment_fractions)
  )

  geometry_column <- attr(stream_apportionment, "sf_column")
  key_columns <- c(
    "pump_id",
    "reach_id",
    "reach_segment_id",
    "represented_length",
    "pump_to_reach_distance",
    "apportionment_fraction"
  )
  additional_columns <- setdiff(
    names(stream_apportionment),
    c(key_columns, "model_point", geometry_column)
  )
  stream_apportionment <- stream_apportionment[c(
    key_columns,
    additional_columns,
    "model_point",
    geometry_column
  )]
  row.names(stream_apportionment) <- NULL
  stream_apportionment
}

# Validate a stream-depletion apportionment object.
.validate_stream_depletion_apportionment <- function(
    stream_apportionment,
    pumping_wells) {

  if (!inherits(stream_apportionment, "sf") ||
      nrow(stream_apportionment) == 0) {
    stop("stream_apportionment must be a nonempty sf object.")
  }

  required_columns <- c(
    "pump_id",
    "reach_id",
    "reach_segment_id",
    "represented_length",
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

  check_dimensionality(
    stream_apportionment$represented_length,
    desired_units = "m",
    variable_name = "stream_apportionment$represented_length"
  )

  if (any(!is.finite(as.numeric(
    stream_apportionment$represented_length
  ))) || any(as.numeric(
    stream_apportionment$represented_length
  ) <= 0)) {
    stop(
      "stream_apportionment$represented_length must contain finite, ",
      "positive values."
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
#' @keywords internal
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

#' Estimate apportioned stream depletion from pumping schedules
#'
#' Apply static reach-segment apportionment and the time-dependent Glover
#' response to intermittent pumping schedules using superposition.
#'
#' @param pumping_wells An `sf` pumping-well object accepted by
#'   [`.validate_pumping_wells()`]. Its `pump_id` values must match the
#'   apportionment object and pumping-schedule columns.
#' @param pumping_schedules A shared, wide-format pumping schedule accepted by
#'   [`.validate_pumping_schedules()`].
#' @param stream_apportionment An `sf` object returned by
#'   [get_stream_reach_apportionment()].
#' @param evaluation_times Either `NULL`, a `Date` vector, or a `units` time
#'   vector. When `NULL`, `pumping_schedules$t` is used.
#'
#' @return A tibble with one row for every requested `pump_id`,
#'   `evaluation_time`, and `reach_segment_id` combination. Columns are
#'   `pump_id`, `evaluation_time`, `reach_id`, `reach_segment_id`, and
#'   `stream_depletion_rate`. The rate retains the units used in
#'   `pumping_schedules`.
#'
#' @details
#' A lookup table first calculates the Glover stream-depletion fraction for
#' every unique `pump_id`, `reach_segment_id`, and `elapsed_time`. For pumping
#' event \eqn{e}, pump \eqn{p}, and reach segment \eqn{s}, the event response
#' is:
#'
#' \deqn{\Delta q_{p,s,e} = \Delta Q_{p,e} F_{p,s}(t_e) A_{p,s}}
#'
#' where \eqn{\Delta Q} is the pumping-rate change, \eqn{F} is the
#' time-dependent analytical stream-depletion fraction, and \eqn{A} is the
#' static apportionment fraction. Event responses are summed by superposition.
#'
#' A pumping change beginning exactly at an evaluation time is excluded from
#' that evaluation. Every requested evaluation time and reach segment is
#' returned, with zero depletion when no earlier pumping event contributes.
#'
#' @examples
#' pumping_wells <- example_pumping_wells
#' stream_reaches <- example_stream_reaches
#'
#' stream_apportionment <- get_stream_reach_apportionment(
#'   pumping_wells,
#'   stream_reaches,
#'   reach_spacing = units::set_units(100, "m"),
#'   sample_spacing = units::set_units(25, "m")
#' )
#'
#' pumping_schedules <- tibble::tibble(
#'   t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
#'   pump_1 = units::set_units(c(500, 300, 0), "m^3/day"),
#'   pump_2 = units::set_units(c(0, 250, 0), "m^3/day")
#' )
#' evaluation_times <- seq.Date(
#'   from = pumping_schedules$t[2],
#'   by = "month",
#'   length.out = nrow(pumping_schedules)
#' )
#'
#' stream_depletion <- get_apportioned_stream_depletion(
#'   pumping_wells,
#'   pumping_schedules,
#'   stream_apportionment,
#'   evaluation_times
#' )
#'
#' stream_depletion
#'
#' @export
get_apportioned_stream_depletion <- function(
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
