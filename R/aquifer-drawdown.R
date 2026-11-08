# Return one response per contributing injection event at the current observation.
# The operator stores effective source distances in meters and, for lines,
# quadrature weights and segment indices. Its response matrix
# has one row per observation and one column per stream segment.
# elapsed_times_day and segment_columns are parallel event vectors: each event
# has an age and a source segment. observation_well_index selects the target row.
# Returned numeric day/m^2 responses retain event order for multiplication by
# the corresponding injection-rate changes in m^3/day.
.get_response_ratios_for_injection_events <- function(
    response_operator, response_cache, K_m_per_day, D_m, V,
    observation_well_index, elapsed_times_day, segment_columns,
    prepared_response_keys = NULL) {
  # Validated event indices and numeric m/day, m, day inputs; returns numeric day/m^2.
  response_ratios_day_per_m2 <- numeric(length(elapsed_times_day))

  # Simultaneous changes on different segments share an age and response matrix.
  for (elapsed_time_day in unique(elapsed_times_day)) {
    event_indices <- which(elapsed_times_day == elapsed_time_day)

    response_key <- if (!is.null(prepared_response_keys)) {
      prepared_response_keys$response_keys[[match(
        elapsed_time_day, prepared_response_keys$elapsed_times_day
      )]]
    } else NULL

    response_matrix_day_per_m2 <- .get_cached_stream_response_matrix(
      response_operator, response_cache, K_m_per_day, D_m, V, elapsed_time_day,
      response_key = response_key
    )

    # Index [observation row, segment column] for each event, placing responses
    # back into the original event positions.
    response_ratios_day_per_m2[event_indices] <- response_matrix_day_per_m2[cbind(
      rep(observation_well_index, length(event_indices)), segment_columns[event_indices]
    )]
  }
  unname(response_ratios_day_per_m2)
}

# Accumulate each response age across all observations using segment rate changes.
.calculate_stream_recovery_at_observations <- function(
    response_operator, response_cache, K_m_per_day, D_m, V,
    elapsed_times_day, segment_columns, injection_rate_change_m3_per_day,
    prepared_response_keys) {
  # Validated numeric inputs use m/day, m, day and m3/day; returns numeric meters.
  stream_recovery_m <- numeric(response_operator$number_of_targets)

  for (elapsed_time_day in unique(elapsed_times_day)) {
    event_indices <- which(elapsed_times_day == elapsed_time_day)

    # Reuse one observation-by-segment response matrix for all events of this age.
    response_matrix_day_per_m2 <- .get_cached_stream_response_matrix(
      response_operator, response_cache, K_m_per_day, D_m, V, elapsed_time_day,
      response_key = prepared_response_keys$response_keys[[match(
        elapsed_time_day, prepared_response_keys$elapsed_times_day
      )]]
    )

    # Sum simultaneous changes on the same segment before applying the full
    # observation-by-segment matrix; repeated segment indices must accumulate.
    segment_rate_changes_m3_per_day <- rowsum(
      injection_rate_change_m3_per_day[event_indices],
      segment_columns[event_indices], reorder = FALSE
    )

    # Expand the grouped rates to all source columns, leaving inactive segments zero.
    segment_injection_rate_changes_m3_per_day <- numeric(response_operator$number_of_segments)
    segment_injection_rate_changes_m3_per_day[as.integer(rownames(segment_rate_changes_m3_per_day))] <-
      segment_rate_changes_m3_per_day[, 1L]

    stream_recovery_m <- stream_recovery_m + as.numeric(
      response_matrix_day_per_m2 %*% segment_injection_rate_changes_m3_per_day
    )
  }

  stream_recovery_m
}

# Superimpose pumping and point or finite-line injection responses at observations.
.superimpose_aquifer_responses <- function(
    pumping_wells, pumping_schedules, observation_wells, evaluation_time_day,
    stream_injection_schedule, quadrature_order, stream_segments) {
  # Validated numeric hydraulic inputs use m, m/day, day and m3/day; sf retains CRS.

  # Prepare the selected source representation for observation responses once;
  # operators used during schedule generation are not retained here.
  observation_response_operator <- .prepare_stream_response_operator(
    observation_wells, stream_segments, quadrature_order
  )

  # Strip spatial distance units once; every subsequent head calculation uses meters.
  pump_to_observation_distances_m <- matrix(
    as.numeric(units::set_units(
      sf::st_distance(pumping_wells, observation_wells), "m"
    )),
    nrow = nrow(pumping_wells)
  )

  # Reuse pumping-rate changes paired with later evaluations. Injection events
  # come from a boundary-validated supplied schedule or a trusted private generator.
  pumping_responses <- attr(pumping_schedules, "isw_pumping_response_times", exact = TRUE)
  injection_events <- .get_injection_rate_changes(stream_injection_schedule)
  segment_columns <- match(injection_events$reach_segment_id, stream_segments$reach_segment_id)
  injection_response_cache <- new.env(parent = emptyenv())

  # Preallocate output vectors in pump, observation, then evaluation-time order.
  number_of_observations <- nrow(observation_wells)
  number_of_times <- length(evaluation_time_day)
  number_of_rows <- nrow(pumping_wells) * number_of_observations * number_of_times
  pumping_drawdown_m <- stream_recovery_m <- numeric(number_of_rows)

  # Evaluate every pump, observation and requested time, preserving attribution
  # of direct pumping drawdown and recovery from that pump's stream injection.
  for (pump_row in seq_len(nrow(pumping_wells))) {
    pump_id <- pumping_wells$pump_id[[pump_row]]

    # Reuse event selections across observations; boundary preparation owns validation.
    pumping_rows <- lapply(seq_len(number_of_times), function(index) {
      which(pumping_responses$pump_id == pump_id & pumping_responses$evaluation_index == index)
    })
    injection_rows <- lapply(evaluation_time_day, function(time_day) {
      which(injection_events$pump_id == pump_id & injection_events$injection_time_day < time_day)
    })

    # Prepare string keys before evaluations; event selection and response
    # matrices are shared across observations.
    injection_elapsed_times_day <- unlist(lapply(seq_len(number_of_times), function(index) {
      evaluation_time_day[[index]] - injection_events$injection_time_day[injection_rows[[index]]]
    }), use.names = FALSE)
    prepared_response_keys <- .prepare_line_response_keys(
      pumping_wells$K_m_per_day[[pump_row]], pumping_wells$D_m[[pump_row]],
      pumping_wells$V[[pump_row]], injection_elapsed_times_day
    )

    for (time_index in seq_len(number_of_times)) {
      pumping_response_rows <- pumping_rows[[time_index]]
      for (observation_index in seq_len(number_of_observations)) {
        result_index <- ((pump_row - 1L) * number_of_observations +
          observation_index - 1L) * number_of_times + time_index

        # Sum direct drawdown from all earlier pumping-rate changes.
        if (length(pumping_response_rows) > 0) {
          response_day_per_m2 <- .theis_aquifer_drawdown_ratio(
            distance_m = pump_to_observation_distances_m[pump_row, observation_index],
            K_m_per_day = pumping_wells$K_m_per_day[[pump_row]],
            D_m = pumping_wells$D_m[[pump_row]], V = pumping_wells$V[[pump_row]],
            t_day = pumping_responses$elapsed_time_day[pumping_response_rows],
            well_diam_m = pumping_wells$well_diam_m[[pump_row]]
          )
          pumping_drawdown_m[[result_index]] <- -sum(
            pumping_responses$pumping_rate_change_m3_per_day[pumping_response_rows] * response_day_per_m2
          )
        }
      }

      # Earlier injection events remain active as their elapsed ages increase.
      # Apply each age's matrix once for every observation, retaining pump attribution.
      injection_event_rows <- injection_rows[[time_index]]
      if (length(injection_event_rows) > 0) {
        result_indices <- ((pump_row - 1L) * number_of_observations +
          seq_len(number_of_observations) - 1L) * number_of_times + time_index
        stream_recovery_m[result_indices] <- .calculate_stream_recovery_at_observations(
          observation_response_operator, injection_response_cache,
          pumping_wells$K_m_per_day[[pump_row]], pumping_wells$D_m[[pump_row]],
          pumping_wells$V[[pump_row]],
          evaluation_time_day[[time_index]] - injection_events$injection_time_day[injection_event_rows],
          segment_columns[injection_event_rows],
          injection_events$injection_rate_change_m3_per_day[injection_event_rows],
          prepared_response_keys
        )
      }
    }
  }

  # One result row per pump, observation and evaluation, in that nesting order.
  tibble::tibble(
    pump_id = rep(pumping_wells$pump_id, each = number_of_observations * number_of_times),
    observation_id = rep(
      rep(observation_wells$observation_id, each = number_of_times),
      times = nrow(pumping_wells)
    ),
    evaluation_time_day = rep(evaluation_time_day, times = nrow(pumping_wells) * number_of_observations),
    pumping_drawdown_m = pumping_drawdown_m,
    stream_recovery_m = stream_recovery_m,
    water_level_change_m = -pumping_drawdown_m + stream_recovery_m
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
#' @param quadrature_order Nonnegative integer controlling stream injection sources.
#'   With `0`, each segment injects its total rate at its `model_point`, using
#'   `stream_width / 2` as the minimum response radius. Positive values give the
#'   number of Gauss--Legendre points per straight edge of each finite line
#'   element. The default is 4.
#'   When a completed constant-head schedule is supplied and this argument is
#'   omitted, the order recorded in the schedule metadata is reused. Supplying
#'   a different order raises a warning and uses the supplied value. This can
#'   change head responses, including the constant-head boundary residuals,
#'   while preserving each segment's total injection rate.
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
#' superimposed at each observation well. With `quadrature_order = 0`, the
#' entire segment rate is applied at its `model_point`; positive orders integrate
#' uniform injection along the segment. Both representations use a minimum
#' response radius of `stream_width / 2`. ADF injection rates are independent
#' of quadrature order. Positive pumping rates produce
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
    quadrature_order = 4L) {

  # Validate original inputs, including supplied intermediates, before stripping units.
  injection_method <- match.arg(injection_method)
  aquifer_groups <- NULL
  prepared_stream_segments <- .prepare_numeric_segments(stream_segments)
  prepared_pumping_wells <- .prepare_numeric_pumping_wells(pumping_wells, sf::st_crs(prepared_stream_segments))
  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  if (is.null(observation_wells)) {
    stop("observation_wells must be a nonempty sf object.")
  }
  .validate_observation_wells(observation_wells)
  .validate_quadrature_order(quadrature_order)
  if (!is.null(stream_injection_schedule) && !is.null(injection_times)) {
    stop("injection_times must be NULL when a stream-injection schedule is supplied.")
  }
  prepared_model_times <- .prepare_numeric_model_times(
    pumping_schedules, evaluation_times, injection_times
  )
  if (!is.null(stream_injection_schedule)) {
    quadrature_order <- .resolve_schedule_quadrature_order(
      stream_injection_schedule, quadrature_order, !missing(quadrature_order)
    )
    injection_schedule <- .prepare_numeric_injection(
      stream_injection_schedule, pumping_wells, pumping_schedules,
      stream_segments, evaluation_times, prepared_model_times$time_origin
    )
    prepared_stream_apportionment <- NULL
  } else if (injection_method == "adf") {
    if (is.null(stream_apportionment)) {
      stop('stream_apportionment is required when method = "adf".')
    }
    .validate_stream_depletion_apportionment(stream_apportionment, pumping_wells)
    .validate_stream_apportionment_segments(stream_apportionment, stream_segments)
    prepared_stream_apportionment <- .prepare_numeric_apportionment(stream_apportionment, pumping_wells)
    injection_schedule <- NULL
  } else {
    aquifer_groups <- .prepare_aquifer_groups(prepared_pumping_wells, allow_n_aquifer_parameter_sets)
    prepared_stream_apportionment <- NULL
    injection_schedule <- NULL
  }
  attr(prepared_pumping_wells, "isw_aquifer_groups") <- aquifer_groups
  result <- .model_aquifer_water_level_change(
    prepared_pumping_wells,
    .prepare_numeric_schedule(pumping_schedules, pumping_wells, prepared_model_times),
    .transform_spatial_geometry(observation_wells, sf::st_crs(prepared_stream_segments)),
    prepared_stream_segments,
    prepared_model_times$evaluation_time_day, prepared_model_times$injection_time_day,
    injection_schedule, injection_method, prepared_stream_apportionment,
    allow_n_aquifer_parameter_sets, quadrature_order
  )
  .restore_aquifer_output(result, prepared_model_times, units::deparse_unit(pumping_wells$D))
}

# Coordinate numeric schedule generation and observation superposition.
.model_aquifer_water_level_change <- function(
    pumping_wells, pumping_schedules, observation_wells, stream_segments,
    evaluation_time_day, injection_time_day = NULL,
    stream_injection_schedule = NULL, injection_method = "adf",
    stream_apportionment = NULL, allow_n_aquifer_parameter_sets = 5,
    quadrature_order = 4L) {
  # Prepared hydraulic quantities use numeric meters/days; spatial objects retain CRS.
  if (is.null(stream_injection_schedule)) {
    stream_injection_schedule <- .generate_stream_injection_schedule(
      pumping_wells, pumping_schedules, stream_segments, evaluation_time_day,
      injection_time_day, method = injection_method,
      stream_apportionment = stream_apportionment,
      allow_n_aquifer_parameter_sets = allow_n_aquifer_parameter_sets,
      quadrature_order = quadrature_order
    )
  }
  .superimpose_aquifer_responses(
    pumping_wells, pumping_schedules, observation_wells, evaluation_time_day,
    stream_injection_schedule, quadrature_order, stream_segments
  )
}
