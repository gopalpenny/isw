# Numerical response operators for transient point and finite-line sources.

# Select the stream representation before constructing any quadrature geometry.
.prepare_stream_response_operator <- function(
    target_points, stream_segments, quadrature_order = 4L) {
  # Inputs are input-validated, with shared meter UTM geometry and numeric widths.
  if (quadrature_order == 0L) {
    return(.prepare_point_response_operator(target_points, stream_segments))
  }
  line_elements <- .prepare_line_elements(stream_segments, quadrature_order)
  .prepare_line_response_operator(target_points, line_elements)
}

# Prepare one injection source per segment, located at its model point.
.prepare_point_response_operator <- function(target_points, stream_segments) {
  if (inherits(target_points, "sfc")) target_points <- sf::st_sf(geometry = target_points)
  prepared_targets <- .transform_spatial_geometry(target_points, sf::st_crs(stream_segments))
  distances_m <- as.matrix(units::drop_units(units::set_units(
    sf::st_distance(prepared_targets, stream_segments$model_point), "m"
  )))
  # Apply each source segment's half-width to its entire target column.
  effective_distances_m <- matrix(
    pmax(as.numeric(distances_m),
         rep(stream_segments$stream_width_m / 2, each = nrow(prepared_targets))),
    nrow = nrow(prepared_targets)
  )
  structure(list(
    distances_m = distances_m,
    effective_distances_m = effective_distances_m,
    segment_ids = stream_segments$reach_segment_id,
    number_of_targets = nrow(prepared_targets),
    number_of_segments = nrow(stream_segments)
  ), class = "isw_point_response_operator")
}

# Both representations return numeric day/m^2 matrices with one column per segment.
.evaluate_stream_response_operator <- function(operator, K_m_per_day, D_m, V, elapsed_time_day) {
  if (!inherits(operator, "isw_point_response_operator")) {
    return(.evaluate_line_response_operator(operator, K_m_per_day, D_m, V, elapsed_time_day))
  }
  response_day_per_m2 <- matrix(.theis_response_at_distance(
    as.numeric(operator$effective_distances_m), K_m_per_day, D_m, V, elapsed_time_day
  ), nrow = operator$number_of_targets, ncol = operator$number_of_segments)
  dimnames(response_day_per_m2) <- list(NULL, operator$segment_ids)
  response_day_per_m2
}

# Return Gauss--Legendre nodes and normalized weights on [-1/2, 1/2].
#' @importFrom statmod gauss.quad
#' @noRd
.get_gauss_legendre_rule <- function(order = 4L) {
  # Numeric dimensionless order is already validated; returns nodes and weights.
  order <- as.integer(order)

  rule <- gauss.quad(order, kind = "legendre")
  ordering <- order(rule$nodes)

  tibble::tibble(
    node = rule$nodes[ordering] / 2,
    weight = rule$weights[ordering] / 2
  )
}

#' Calculate a transient finite-line aquifer response
#'
#' Internal numerical kernel for the response to a uniform total rate along a
#' straight finite line element. Coordinates are local to the line midpoint.
#'
#' @param along_distance_m Signed observation-point distance parallel to the
#'   line. Numeric in meters.
#' @param perpendicular_distance_m Observation-point distance perpendicular to
#'   the line. Numeric in meters.
#' @param line_length_m Positive numeric length of the finite line, in meters.
#' @param K_m_per_day Numeric hydraulic conductivity in meters per day.
#' @param D_m Numeric aquifer thickness in meters.
#' @param V Drainable porosity or specific yield.
#' @param t_day Positive numeric elapsed time in days.
#' @param stream_width_m Positive numeric stream width in meters.
#' @param quadrature_order Positive number of Gauss--Legendre points. The
#'   default is 4.
#'
#' @return A numeric vector in day/m^2 giving water-level change divided by
#'   total line rate.
#'
#' @details
#' For a line of length \eqn{L} centered on the local origin, this evaluates
#' the normalized line average
#' \deqn{R_{line} = \int_{-1/2}^{1/2}
#' R_{Theis}(\sqrt{(x-L\xi)^2+y^2},t)\,d\xi.}
#' The effective distance at every quadrature point is bounded below by
#' `stream_width_m / 2`. Because the quadrature weights sum to one, the rate
#' multiplying the result is the total segment rate, not a rate per unit
#' length.
#'
#' @noRd
.line_sink_aquifer_drawdown_ratio <- function(
    along_distance_m, perpendicular_distance_m, line_length_m,
    K_m_per_day, D_m, V, t_day, stream_width_m, quadrature_order = 4L) {
  # Already-validated numeric inputs: m, m/day and day; returns numeric day/m^2.
  output_length <- max(lengths(list(
    along_distance_m, perpendicular_distance_m, line_length_m,
    K_m_per_day, D_m, V, t_day, stream_width_m
  )))
  rule <- .get_gauss_legendre_rule(quadrature_order)
  response_day_per_m2 <- numeric(output_length)
  recycle <- function(x) rep(x, length.out = output_length)
  along_distance_m <- recycle(along_distance_m)
  perpendicular_distance_m <- recycle(perpendicular_distance_m)
  line_length_m <- recycle(line_length_m)
  K_m_per_day <- recycle(K_m_per_day)
  D_m <- recycle(D_m)
  V <- recycle(V)
  t_day <- recycle(t_day)
  stream_width_m <- recycle(stream_width_m)

  for (index in seq_len(output_length)) {
    source_offset_m <- rule$node * line_length_m[[index]]
    distance_m <- sqrt(
      (along_distance_m[[index]] - source_offset_m)^2 +
        perpendicular_distance_m[[index]]^2
    )
    effective_distance_m <- pmax(distance_m, stream_width_m[[index]] / 2)
    point_response_day_per_m2 <- .theis_response_at_distance(
      effective_distance_m, K_m_per_day[[index]], D_m[[index]], V[[index]], t_day[[index]]
    )
    response_day_per_m2[[index]] <- sum(rule$weight * point_response_day_per_m2)
  }
  response_day_per_m2
}

# Expand finite line elements into reusable quadrature source points.
.prepare_line_elements <- function(stream_segments, quadrature_order = 4L) {
  # Validated shared-UTM sf geometry retains CRS; stream widths are numeric meters.
  rule <- .get_gauss_legendre_rule(quadrature_order)
  geometry <- sf::st_geometry(stream_segments)
  quadrature_coordinates_m <- list()
  quadrature_weights <- numeric()
  source_segment_indices <- integer()
  point_index <- 0L

  # A stream segment may contain multiple straight sections between vertices.
  # Place `quadrature_order` points along each positive-length straight section
  # and scale their weights by the section's share of the total segment length.
  for (segment_index in seq_len(nrow(stream_segments))) {
    coordinates_m <- sf::st_coordinates(geometry[[segment_index]])[, 1:2, drop = FALSE]
    edge_starts_m <- coordinates_m[-nrow(coordinates_m), , drop = FALSE]
    edge_ends_m <- coordinates_m[-1L, , drop = FALSE]
    edge_vectors_m <- edge_ends_m - edge_starts_m
    edge_lengths_m <- sqrt(rowSums(edge_vectors_m^2))
    positive_edges <- which(edge_lengths_m > 0)
    total_length_m <- sum(edge_lengths_m[positive_edges])

    if (!is.finite(total_length_m) || total_length_m <= 0) {
      stop("Every line element must contain at least one positive-length edge.")
    }

    # Place the quadrature rule along each edge and scale its weights by the
    # fraction of the segment's total length represented by that edge.
    for (edge_index in positive_edges) {
      edge_midpoint_m <- (edge_starts_m[edge_index, ] + edge_ends_m[edge_index, ]) / 2
      edge_points_m <- sweep(
        outer(rule$node, edge_vectors_m[edge_index, ]),
        2,
        edge_midpoint_m,
        "+"
      )

      # Store each coordinate as an sf point for later distance calculations.
      for (node_index in seq_len(nrow(edge_points_m))) {
        point_index <- point_index + 1L
        quadrature_coordinates_m[[point_index]] <- edge_points_m[node_index, ]
      }

      quadrature_weights <- c(
        quadrature_weights,
        rule$weight * edge_lengths_m[[edge_index]] / total_length_m
      )
      source_segment_indices <- c(
        source_segment_indices,
        rep(segment_index, nrow(edge_points_m))
      )
    }
  }

  # Convert the stored coordinates_m to point geometries, then create an `sf`
  # object linking each point to its parent segment, weight, and stream width.
  quadrature_geometry <- sf::st_sfc(
    lapply(quadrature_coordinates_m, sf::st_point),
    crs = sf::st_crs(stream_segments)
  )
  quadrature_points <- sf::st_sf(
    segment_index = source_segment_indices,
    weight = quadrature_weights,
    stream_width_m = stream_segments$stream_width_m[source_segment_indices],
    geometry = quadrature_geometry
  )

  structure(
    list(
      quadrature_points = quadrature_points,
      segment_ids = stream_segments$reach_segment_id,
      number_of_segments = nrow(stream_segments),
      quadrature_order = as.integer(quadrature_order),
      crs = sf::st_crs(stream_segments)
    ),
    class = "isw_prepared_line_elements"
  )
}

# Precompute target-to-quadrature distances for repeated hydraulic evaluation.
.prepare_line_response_operator <- function(target_points, line_elements) {
  # Validated sf targets retain CRS; prepare reusable numeric distances in meters.
  if (inherits(target_points, "sfc")) target_points <- sf::st_sf(geometry = target_points)
  prepared_targets <- .transform_spatial_geometry(target_points, line_elements$crs)
  # Normalize sf measurements before dropping units; preserve matrix dimensions.
  distances_m <- as.matrix(units::drop_units(units::set_units(
    sf::st_distance(prepared_targets, line_elements$quadrature_points), "m"
  )))
  stream_width_m <- line_elements$quadrature_points$stream_width_m
  effective_distances_m <- matrix(
    pmax(as.numeric(distances_m), rep(stream_width_m / 2, each = nrow(prepared_targets))),
    nrow = nrow(prepared_targets)
  )
  structure(list(
    distances_m = distances_m,
    effective_distances_m = effective_distances_m,
    segment_index = line_elements$quadrature_points$segment_index,
    weights = line_elements$quadrature_points$weight,
    stream_width_m = stream_width_m,
    segment_columns = lapply(seq_len(line_elements$number_of_segments), function(index) {
      which(line_elements$quadrature_points$segment_index == index)
    }),
    segment_ids = line_elements$segment_ids,
    number_of_targets = nrow(prepared_targets),
    number_of_segments = line_elements$number_of_segments
  ), class = "isw_line_response_operator")
}

.evaluate_line_response_operator <- function(operator, K_m_per_day, D_m, V, elapsed_time_day) {
  # Prepared numeric distances are m, conductivity m/day, time day; returns day/m^2.
  # Evaluate the source quadrature points, then sum their weighted responses
  # into one response column for each finite-line segment.
  point_response_day_per_m2 <- matrix(.theis_response_at_distance(
    as.numeric(operator$effective_distances_m), K_m_per_day, D_m, V, elapsed_time_day
  ), nrow = operator$number_of_targets)
  point_response_day_per_m2 <- sweep(point_response_day_per_m2, 2, operator$weights, "*")
  response_day_per_m2 <- matrix(0, operator$number_of_targets, operator$number_of_segments)
  for (segment in seq_len(operator$number_of_segments)) {
    response_day_per_m2[, segment] <- rowSums(
      point_response_day_per_m2[, operator$segment_columns[[segment]], drop = FALSE]
    )
  }
  dimnames(response_day_per_m2) <- list(NULL, operator$segment_ids)
  response_day_per_m2
}

# Prepare exact cache keys once for an aquifer and its distinct response ages.
.prepare_response_cache_keys <- function(K_m_per_day, D_m, V, elapsed_times_day) {
  # Validated numeric inputs use m/day, m and day; keys are dimensionless identifiers.
  elapsed_times_day <- unique(elapsed_times_day)
  aquifer_key <- paste(format(K_m_per_day, digits = 17), format(D_m, digits = 17),
                       format(V, digits = 17), sep = "\r")
  # Format ages individually to preserve the existing scalar key representation,
  # including full precision and independence from vector-wide formatting choices.
  age_keys <- vapply(elapsed_times_day, function(age_day) format(age_day, digits = 17), character(1))
  response_keys <- if (length(age_keys)) paste(aquifer_key, age_keys, sep = "\r") else character()
  list(elapsed_times_day = elapsed_times_day, response_keys = response_keys)
}

# Cache numeric day/m^2 matrices by prepared hydraulic parameters and elapsed days.
# The operator's effective distances and, for lines, weights and segment indices
# define the fixed spatial problem; matrices have target rows and segment
# columns. K_m_per_day, D_m and dimensionless V determine the hydraulic response.
# Bind each cache to its first operator so entries cannot be reused with different
# targets, source representations or quadrature rules.
.get_cached_stream_response_matrix <- function(
    response_operator, response_cache, K_m_per_day, D_m, V, elapsed_time_day,
    response_key = NULL) {
  # Prepared numeric inputs use m, m/day and day; retain cache-ownership diagnostics.
  cached_operator <- attr(response_cache, "isw_stream_response_operator", exact = TRUE)
  if (is.null(cached_operator)) {
    attr(response_cache, "isw_stream_response_operator") <- response_operator
  } else if (!identical(response_operator, cached_operator)) {
    stop("response_cache cannot be reused with a different response_operator.")
  }
  # Model coordinators supply prepared keys; independent private callers retain
  # the same scalar formatting fallback. Cache ownership checks still apply.
  if (is.null(response_key)) {
    response_key <- paste(format(K_m_per_day, digits = 17), format(D_m, digits = 17),
                          format(V, digits = 17), format(elapsed_time_day, digits = 17), sep = "\r")
  }
  if (!exists(response_key, envir = response_cache, inherits = FALSE)) {
    assign(response_key, .evaluate_stream_response_operator(
      response_operator, K_m_per_day, D_m, V, elapsed_time_day
    ), envir = response_cache)
  }
  get(response_key, envir = response_cache, inherits = FALSE)
}
