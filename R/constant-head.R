# Group pumping wells by aquifer-response parameters.
.get_aquifer_groups <- function(
    pumping_wells,
    allow_many_aquifer_parameter_sets = FALSE) {

  .validate_pumping_wells(pumping_wells)
  normalized_K <- as.numeric(units::set_units(
    pumping_wells$K,
    "m/day",
    mode = "standard"
  ))
  normalized_D <- as.numeric(units::set_units(
    pumping_wells$D,
    "m",
    mode = "standard"
  ))
  parameter_keys <- paste(
    format(normalized_K, digits = 17),
    format(normalized_D, digits = 17),
    format(pumping_wells$V, digits = 17),
    sep = "\r"
  )

  if ("aquifer_id" %in% names(pumping_wells)) {
    aquifer_id <- pumping_wells$aquifer_id

    if (!is.character(aquifer_id) || anyNA(aquifer_id) ||
        any(trimws(aquifer_id) == "")) {
      stop(
        "pumping_wells$aquifer_id must contain nonmissing, nonempty ",
        "character values."
      )
    }

    inconsistent_ids <- vapply(
      unique(aquifer_id),
      function(id) length(unique(parameter_keys[aquifer_id == id])) != 1,
      logical(1)
    )

    if (any(inconsistent_ids)) {
      stop(
        "Pumps sharing an aquifer_id must have identical K, D, and V; ",
        "inconsistent aquifer_id values: ",
        paste(unique(aquifer_id)[inconsistent_ids], collapse = ", "),
        "."
      )
    }
  } else {
    unique_keys <- unique(parameter_keys)
    aquifer_id <- paste0("aquifer_", match(parameter_keys, unique_keys))
  }

  number_of_parameter_sets <- length(unique(aquifer_id))

  if (number_of_parameter_sets > 10 &&
      !isTRUE(allow_many_aquifer_parameter_sets)) {
    stop(
      "Constant-head injection found ", number_of_parameter_sets,
      " aquifer parameter sets. More than 10 requires ",
      "allow_many_aquifer_parameter_sets = TRUE."
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

  tibble::tibble(
    pump_id = pumping_wells$pump_id,
    aquifer_id = aquifer_id,
    parameter_key = parameter_keys
  )
}

# Calculate a discrete-well response matrix between stream model points.
.get_stream_well_response_matrix <- function(
    stream_points,
    K,
    D,
    V,
    elapsed_time) {

  number_of_points <- nrow(stream_points)
  distance_matrix <- sf::st_distance(stream_points, stream_points)
  distance_vector <- units::set_units(
    as.numeric(distance_matrix),
    units::deparse_unit(distance_matrix),
    mode = "standard"
  )
  response <- .theis_aquifer_drawdown_ratio(
    distance = distance_vector,
    K = K,
    D = D,
    V = V,
    t = elapsed_time,
    well_diam = rep(stream_points$well_diam, each = number_of_points)
  )
  response <- units::set_units(
    response,
    "days/m^2",
    mode = "standard"
  )

  matrix(
    as.numeric(response),
    nrow = number_of_points,
    ncol = number_of_points
  )
}

# Calculate pump-specific signed head change at stream model points.
.get_pumping_head_at_stream <- function(
    pumping_well,
    pumping_events,
    stream_points,
    evaluation_day) {

  contributing_rows <- which(pumping_events$pumping_time < evaluation_day)
  pumping_head <- numeric(nrow(stream_points))

  if (length(contributing_rows) == 0) {
    return(pumping_head)
  }

  pump_distances <- sf::st_distance(pumping_well, stream_points)[1, ]

  for (event_row in contributing_rows) {
    response <- .theis_aquifer_drawdown_ratio(
      distance = pump_distances,
      K = pumping_well$K[[1]],
      D = pumping_well$D[[1]],
      V = pumping_well$V[[1]],
      t = evaluation_day - pumping_events$pumping_time[[event_row]],
      well_diam = pumping_well$well_diam[[1]]
    )
    response <- units::set_units(
      response,
      "days/m^2",
      mode = "standard"
    )
    rate_change <- units::set_units(
      pumping_events$pumping_rate_change[[event_row]],
      "m^3/day",
      mode = "standard"
    )
    pumping_head <- pumping_head +
      as.numeric(rate_change) * as.numeric(response)
  }

  pumping_head
}

# Construct the endpoint-collocated constant-head injection schedule.
.get_constant_head_injection_schedule <- function(
    pumping_wells,
    pumping_schedules,
    stream_segments,
    time_grid,
    allow_many_aquifer_parameter_sets = FALSE) {

  .validate_stream_segments(stream_segments)
  aquifer_groups <- .get_aquifer_groups(
    pumping_wells,
    allow_many_aquifer_parameter_sets
  )
  analysis_crs <- sf::st_crs(stream_segments)
  prepared_pumping_wells <- sf::st_transform(
    sf::st_zm(pumping_wells, drop = TRUE, what = "ZM"),
    analysis_crs
  )

  if (!("well_diam" %in% names(prepared_pumping_wells))) {
    prepared_pumping_wells$well_diam <- units::set_units(
      rep(0, nrow(prepared_pumping_wells)),
      "m",
      mode = "standard"
    )
  }

  prepared_pumping_wells$aquifer_id <- aquifer_groups$aquifer_id
  stream_points <- sf::st_sf(
    reach_id = stream_segments$reach_id,
    reach_segment_id = stream_segments$reach_segment_id,
    well_diam = stream_segments$well_diam,
    geometry = sf::st_transform(stream_segments$model_point, analysis_crs)
  )
  pumping_events <- .get_pumping_rate_changes(
    pumping_schedules,
    pumping_wells
  )
  time_nodes <- time_grid$injection_days
  number_of_intervals <- length(time_nodes) - 1L
  output_rate_unit <- units::deparse_unit(
    pumping_schedules[[pumping_wells$pump_id[[1]]]]
  )

  if (number_of_intervals <= 0) {
    return(tibble::tibble(
      pump_id = character(),
      aquifer_id = character(),
      reach_id = character(),
      reach_segment_id = character(),
      interval_start = time_grid$injection_times[0],
      interval_end = time_grid$injection_times[0],
      injection_rate = units::set_units(
        numeric(), output_rate_unit, mode = "standard"
      ),
      boundary_residual = units::set_units(numeric(), "m"),
      matrix_condition_number = numeric()
    ))
  }

  schedule_parts <- list()
  condition_diagnostics <- list()
  output_index <- 0L

  for (aquifer_id in unique(aquifer_groups$aquifer_id)) {
    pump_rows <- which(prepared_pumping_wells$aquifer_id == aquifer_id)
    group_pumps <- prepared_pumping_wells[pump_rows, , drop = FALSE]
    number_of_pumps <- nrow(group_pumps)
    number_of_segments <- nrow(stream_points)
    solved_rates <- array(
      0,
      dim = c(number_of_segments, number_of_pumps, number_of_intervals)
    )
    response_cache <- new.env(parent = emptyenv())
    factor_cache <- new.env(parent = emptyenv())

    get_response_matrix <- function(elapsed_time) {
      cache_key <- format(
        as.numeric(units::set_units(
          elapsed_time,
          "days",
          mode = "standard"
        )),
        digits = 17
      )

      if (!exists(cache_key, envir = response_cache, inherits = FALSE)) {
        assign(
          cache_key,
          .get_stream_well_response_matrix(
            stream_points,
            group_pumps$K[[1]],
            group_pumps$D[[1]],
            group_pumps$V[[1]],
            elapsed_time
          ),
          envir = response_cache
        )
      }

      get(cache_key, envir = response_cache, inherits = FALSE)
    }

    for (interval_index in seq_len(number_of_intervals)) {
      interval_start <- time_nodes[[interval_index]]
      interval_end <- time_nodes[[interval_index + 1L]]
      interval_duration <- interval_end - interval_start
      pumping_head <- matrix(
        0,
        nrow = number_of_segments,
        ncol = number_of_pumps
      )
      historical_head <- pumping_head

      for (group_pump_index in seq_len(number_of_pumps)) {
        pump_id <- group_pumps$pump_id[[group_pump_index]]
        pump_events <- pumping_events[
          pumping_events$pump_id == pump_id,
          ,
          drop = FALSE
        ]
        pumping_head[, group_pump_index] <- .get_pumping_head_at_stream(
          group_pumps[group_pump_index, , drop = FALSE],
          pump_events,
          stream_points,
          interval_end
        )
      }

      if (interval_index > 1L) {
        for (previous_index in seq_len(interval_index - 1L)) {
          pulse_response <-
            get_response_matrix(
              interval_end - time_nodes[[previous_index]]
            ) -
            get_response_matrix(
              interval_end - time_nodes[[previous_index + 1L]]
            )
          historical_head <- historical_head +
            pulse_response %*% solved_rates[, , previous_index]
        }
      }

      response_matrix <- get_response_matrix(interval_duration)
      factor_key <- format(
        as.numeric(units::set_units(
          interval_duration,
          "days",
          mode = "standard"
        )),
        digits = 17
      )

      if (!exists(factor_key, envir = factor_cache, inherits = FALSE)) {
        matrix_qr <- qr(response_matrix)
        condition_number <- kappa(matrix_qr, exact = FALSE)
        assign(
          factor_key,
          list(qr = matrix_qr, condition_number = condition_number),
          envir = factor_cache
        )
      }

      factorization <- get(
        factor_key,
        envir = factor_cache,
        inherits = FALSE
      )
      right_hand_side <- -(pumping_head + historical_head)
      interval_rates <- tryCatch(
        qr.coef(factorization$qr, right_hand_side),
        error = function(error) {
          stop(
            "The constant-head response matrix could not be solved for ",
            "aquifer_id ", aquifer_id, " and interval ending at ",
            as.character(time_grid$injection_times[[interval_index + 1L]]),
            ": ", conditionMessage(error),
            call. = FALSE
          )
        }
      )

      if (any(!is.finite(interval_rates))) {
        stop(
          "The constant-head response matrix produced nonfinite rates for ",
          "aquifer_id ", aquifer_id, ".",
          call. = FALSE
        )
      }

      solved_rates[, , interval_index] <- interval_rates
      residual <- pumping_head + historical_head +
        response_matrix %*% interval_rates
      condition_diagnostics[[length(condition_diagnostics) + 1L]] <-
        tibble::tibble(
          aquifer_id = aquifer_id,
          interval_end = time_grid$injection_times[[interval_index + 1L]],
          matrix_condition_number = factorization$condition_number
        )

      for (group_pump_index in seq_len(number_of_pumps)) {
        output_index <- output_index + 1L
        solved_rate_units <- units::set_units(
          interval_rates[, group_pump_index],
          "m^3/day",
          mode = "standard"
        )
        schedule_parts[[output_index]] <- tibble::tibble(
          pump_id = rep(
            group_pumps$pump_id[[group_pump_index]],
            number_of_segments
          ),
          aquifer_id = rep(aquifer_id, number_of_segments),
          reach_id = stream_points$reach_id,
          reach_segment_id = stream_points$reach_segment_id,
          interval_start = rep(
            interval_start,
            number_of_segments
          ),
          interval_end = rep(
            interval_end,
            number_of_segments
          ),
          injection_rate = units::set_units(
            solved_rate_units,
            output_rate_unit,
            mode = "standard"
          ),
          boundary_residual = units::set_units(
            residual[, group_pump_index],
            "m",
            mode = "standard"
          ),
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
      ". Inspect stream spacing and well_diam values.",
      call. = FALSE
    )
  }

  do.call(rbind, schedule_parts)
}
