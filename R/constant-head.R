# Validate user-facing aquifer-group controls and normalized hydraulic relationships.
.prepare_aquifer_groups <- function(pumping_wells, allow_n_aquifer_parameter_sets = 5) {
  # The boundary already prepared numeric m/day and m fields; check group controls/IDs.
  if (!is.numeric(allow_n_aquifer_parameter_sets) ||
      inherits(allow_n_aquifer_parameter_sets, "units") ||
      length(allow_n_aquifer_parameter_sets) != 1L ||
      is.na(allow_n_aquifer_parameter_sets) ||
      allow_n_aquifer_parameter_sets < 1 ||
      (!is.infinite(allow_n_aquifer_parameter_sets) &&
       allow_n_aquifer_parameter_sets !=
         floor(allow_n_aquifer_parameter_sets))) {
    stop(
      "allow_n_aquifer_parameter_sets must be a positive whole-number ",
      "scalar or Inf."
    )
  }

  groups <- .get_aquifer_groups(pumping_wells)
  if ("aquifer_id" %in% names(pumping_wells)) {
    aquifer_id <- pumping_wells$aquifer_id
    if (!is.character(aquifer_id) || anyNA(aquifer_id) || any(trimws(aquifer_id) == "")) {
      stop("pumping_wells$aquifer_id must contain nonmissing, nonempty character values.")
    }
    inconsistent_ids <- vapply(split(groups$parameter_key, aquifer_id), function(keys) {
      length(unique(keys)) != 1L
    }, logical(1))
    if (any(inconsistent_ids)) {
      stop("Pumps sharing an aquifer_id must have identical K, D, and V; inconsistent aquifer_id values: ",
           paste(names(inconsistent_ids)[inconsistent_ids], collapse = ", "), ".")
    }
  }
  number_of_parameter_sets <- length(unique(groups$aquifer_id))
  if (number_of_parameter_sets > allow_n_aquifer_parameter_sets) {
    stop(
      "Constant-head injection found ", number_of_parameter_sets,
      " aquifer parameter sets, exceeding ",
      "allow_n_aquifer_parameter_sets = ",
      allow_n_aquifer_parameter_sets, ". Increase the allowed number to ",
      "run this calculation."
    )
  }

  if (number_of_parameter_sets > 1) {
    warning(
      "Constant-head injection is using ", number_of_parameter_sets,
      " pump-specific aquifer parameter sets. Each set defines a separate ",
      "analytical response operator.",
      call. = FALSE
    )
  }

  groups
}

.get_aquifer_groups <- function(pumping_wells) {
  # Validated numeric K_m_per_day, D_m and dimensionless V; returns identifiers/keys.
  parameter_keys <- paste(format(pumping_wells$K_m_per_day, digits = 17),
                          format(pumping_wells$D_m, digits = 17),
                          format(pumping_wells$V, digits = 17), sep = "\r")
  aquifer_id <- if ("aquifer_id" %in% names(pumping_wells)) pumping_wells$aquifer_id else {
    paste0("aquifer_", match(parameter_keys, unique(parameter_keys)))
  }
  tibble::tibble(pump_id = pumping_wells$pump_id, aquifer_id = aquifer_id,
                 parameter_key = parameter_keys)
}

# Calculate pump-specific signed head change at stream model points.
.get_pumping_head_at_stream <- function(
    pumping_well, pumping_events, pump_to_stream_distances_m, evaluation_time_day) {
  # Validated numeric inputs use m, m/day, day and m3/day; returns signed head in m.
  contributing_rows <- which(pumping_events$pumping_time_day < evaluation_time_day)
  pumping_head_m <- numeric(length(pump_to_stream_distances_m))
  # Sum the signed head response from every earlier rate change for this pump.
  for (event_row in contributing_rows) {
    response_day_per_m2 <- .theis_aquifer_drawdown_ratio(
      distance_m = pump_to_stream_distances_m,
      K_m_per_day = pumping_well$K_m_per_day[[1]],
      D_m = pumping_well$D_m[[1]], V = pumping_well$V[[1]],
      t_day = evaluation_time_day - pumping_events$pumping_time_day[[event_row]],
      well_diam_m = pumping_well$well_diam_m[[1]]
    )
    pumping_head_m <- pumping_head_m +
      pumping_events$pumping_rate_change_m3_per_day[[event_row]] * response_day_per_m2
  }
  pumping_head_m
}

# Construct the endpoint-collocated constant-head injection schedule.
.get_constant_head_injection_schedule <- function(
    pumping_wells,
    pumping_schedules,
    stream_segments,
    time_grid,
    quadrature_order = 16L) {

  # Prepared sf geometry retains its shared meter UTM CRS; hydraulic inputs use
  # numeric m, m/day, day and m3/day. User-facing boundaries validate and restore units.
  aquifer_groups <- attr(pumping_wells, "isw_aquifer_groups", exact = TRUE)
  prepared_pumping_wells <- pumping_wells
  prepared_pumping_wells$aquifer_id <- aquifer_groups$aquifer_id
  analysis_crs <- sf::st_crs(stream_segments)
  # Use each segment's model point as the collocation location where the
  # constant-head solver enforces zero head change.
  stream_collocation_points <- sf::st_sf(
    reach_id = stream_segments$reach_id,
    reach_segment_id = stream_segments$reach_segment_id,
    geometry = sf::st_transform(stream_segments$model_point, analysis_crs)
  )
  # Represent each stream segment by weighted Gauss--Legendre source points
  # used to integrate its finite-line injection response.
  line_elements <- .prepare_line_elements(stream_segments, quadrature_order)
  # Precompute distances from every stream collocation point to those source
  # points so the geometry can be reused across response times and aquifers.
  boundary_response_operator <- .prepare_line_response_operator(
    stream_collocation_points,
    line_elements
  )
  # Convert spatial distances to bare meters once, before the interval loop.
  pump_to_stream_distances_m <- matrix(as.numeric(units::set_units(
    sf::st_distance(prepared_pumping_wells, stream_collocation_points), "m"
  )), nrow = nrow(prepared_pumping_wells))
  pumping_events <- attr(pumping_schedules, "isw_pumping_events", exact = TRUE)
  pump_events <- lapply(prepared_pumping_wells$pump_id, function(id) {
    pumping_events[pumping_events$pump_id == id, , drop = FALSE]
  })
  time_nodes_day <- time_grid$boundary_time_day
  number_of_intervals <- length(time_nodes_day) - 1L
  if (number_of_intervals <= 0) {
    return(tibble::tibble(
      pump_id = character(), aquifer_id = character(), reach_id = character(),
      reach_segment_id = character(), interval_start_day = numeric(),
      interval_end_day = numeric(), injection_rate_m3_per_day = numeric(),
      boundary_residual_m = numeric(), matrix_condition_number = numeric()
    ))
  }

  schedule_parts <- list()
  condition_diagnostics <- list()
  output_index <- 0L

  # Solve separately for each aquifer-parameter group so pumps with identical
  # K, D, and V can share response matrices and their matrix factorizations.
  for (aquifer_id in unique(aquifer_groups$aquifer_id)) {
    pump_rows <- which(prepared_pumping_wells$aquifer_id == aquifer_id)
    group_pumps <- prepared_pumping_wells[pump_rows, , drop = FALSE]
    number_of_pumps <- nrow(group_pumps)
    number_of_segments <- nrow(stream_collocation_points)
    solved_rates_m3_per_day <- array(
      0,
      dim = c(number_of_segments, number_of_pumps, number_of_intervals)
    )
    response_cache <- new.env(parent = emptyenv())
    factor_cache <- new.env(parent = emptyenv())

    get_response_matrix <- function(elapsed_time_day) {
      # Retrieve cached numeric day/m2 responses directly for the QR algebra.
      .get_cached_line_response_matrix(
        response_operator = boundary_response_operator,
        response_cache = response_cache,
        K_m_per_day = group_pumps$K_m_per_day[[1]],
        D_m = group_pumps$D_m[[1]], V = group_pumps$V[[1]],
        elapsed_time_day = elapsed_time_day
      )
    }

    # Step through the schedule intervals and solve the segment injection rates
    # needed to restore constant head at the end of each interval.
    for (interval_index in seq_len(number_of_intervals)) {
      interval_start_day <- time_nodes_day[[interval_index]]
      interval_end_day <- time_nodes_day[[interval_index + 1L]]
      interval_duration_day <- interval_end_day - interval_start_day
      pumping_head_m <- matrix(
        0,
        nrow = number_of_segments,
        ncol = number_of_pumps
      )
      historical_head_m <- pumping_head_m

      # Calculate direct pumping head at every stream collocation point for
      # each pump in this aquifer group.
      for (group_pump_index in seq_len(number_of_pumps)) {
        pump_row <- pump_rows[[group_pump_index]]
        pumping_head_m[, group_pump_index] <- .get_pumping_head_at_stream(
          group_pumps[group_pump_index, , drop = FALSE],
          pump_events[[pump_row]], pump_to_stream_distances_m[pump_row, ], interval_end_day
        )
      }

      if (interval_index > 1L) {
        # Accumulate the head response remaining from injection rates solved
        # for all preceding intervals.
        for (previous_index in seq_len(interval_index - 1L)) {
          pulse_response_day_per_m2 <-
            get_response_matrix(
              interval_end_day - time_nodes_day[[previous_index]]
            ) -
            get_response_matrix(
              interval_end_day - time_nodes_day[[previous_index + 1L]]
            )
          historical_head_m <- historical_head_m +
            pulse_response_day_per_m2 %*% matrix(
              solved_rates_m3_per_day[, , previous_index],
              nrow = number_of_segments, ncol = number_of_pumps
            )
        }
      }

      response_matrix_day_per_m2 <- get_response_matrix(interval_duration_day)

      # Key the QR factorization by normalized interval duration. Within this
      # aquifer group, equal durations have identical response matrices and can
      # reuse the same factorization and condition number.
      factor_key <- format(interval_duration_day, digits = 17)

      if (!exists(factor_key, envir = factor_cache, inherits = FALSE)) {
        matrix_qr <- qr(response_matrix_day_per_m2)
        condition_number <- kappa(matrix_qr, exact = FALSE)
        assign(
          factor_key,
          list(qr = matrix_qr, condition_number = condition_number),
          envir = factor_cache
        )
      }

      # Retrieve the cached QR decomposition and condition number. The same
      # decomposition solves the separate right-hand side for every pump.
      factorization <- get(
        factor_key,
        envir = factor_cache,
        inherits = FALSE
      )

      # Current injection must offset the combined head change from pumping
      # and injection during all preceding intervals.
      right_hand_side_m <- -(pumping_head_m + historical_head_m)
      # Use the QR decomposition to solve for the segment injection rates that
      # produce the required head correction at every collocation point.
      interval_rates_m3_per_day <- tryCatch(
        qr.coef(factorization$qr, right_hand_side_m),
        error = function(error) {
          stop(
            "The constant-head response matrix could not be solved for ",
            "aquifer_id ", aquifer_id, " and interval ending at ",
            paste(interval_end_day, "days"),
            ": ", conditionMessage(error),
            call. = FALSE
          )
        }
      )

      if (any(!is.finite(interval_rates_m3_per_day))) {
        stop(
          "The constant-head response matrix produced nonfinite rates for ",
          "aquifer_id ", aquifer_id, ".",
          call. = FALSE
        )
      }

      # Retain these rates for later intervals, calculate the remaining head
      # error after current injection, and record the matrix diagnostic.
      solved_rates_m3_per_day[, , interval_index] <- interval_rates_m3_per_day
      residual_m <- pumping_head_m + historical_head_m +
        response_matrix_day_per_m2 %*% interval_rates_m3_per_day
      condition_diagnostics[[length(condition_diagnostics) + 1L]] <-
        tibble::tibble(
          aquifer_id = aquifer_id,
          interval_end_day = interval_end_day,
          matrix_condition_number = factorization$condition_number
        )

      # Store the solved segment rates and boundary residuals for each pump.
      for (group_pump_index in seq_len(number_of_pumps)) {
        output_index <- output_index + 1L
        schedule_parts[[output_index]] <- tibble::tibble(
          pump_id = rep(
            group_pumps$pump_id[[group_pump_index]],
            number_of_segments
          ),
          aquifer_id = rep(aquifer_id, number_of_segments),
          reach_id = stream_collocation_points$reach_id,
          reach_segment_id = stream_collocation_points$reach_segment_id,
          interval_start_day = rep(
            interval_start_day,
            number_of_segments
          ),
          interval_end_day = rep(
            interval_end_day,
            number_of_segments
          ),
          injection_rate_m3_per_day = -interval_rates_m3_per_day[, group_pump_index],
          boundary_residual_m = residual_m[, group_pump_index],
          matrix_condition_number = rep(
            factorization$condition_number,
            number_of_segments
          )
        )
      }
    }
  }

  diagnostics <- do.call(rbind, condition_diagnostics)

  if (any(diagnostics$matrix_condition_number > 1e6)) {
    affected <- diagnostics[
      diagnostics$matrix_condition_number > 1e6,
      ,
      drop = FALSE
    ]
    warning(
      "Constant-head response-matrix condition number exceeded 1e6. ",
      "Maximum: ",
      format(max(affected$matrix_condition_number), digits = 5),
      "; affected aquifer_id values: ",
      paste(unique(affected$aquifer_id), collapse = ", "),
      ". Inspect stream spacing and stream_width values.",
      call. = FALSE
    )
  }

  do.call(rbind, schedule_parts)
}
