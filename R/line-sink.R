# Numerical response operators for transient finite line elements.

# Return Gauss--Legendre nodes and normalized weights on [-1/2, 1/2].
#' @importFrom statmod gauss.quad
#' @noRd
.get_gauss_legendre_rule <- function(order = 16L) {
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
#'   default is 16.
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
    K_m_per_day, D_m, V, t_day, stream_width_m, quadrature_order = 16L) {
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
.prepare_line_elements <- function(stream_segments, quadrature_order = 16L) {
  .validate_stream_segments(stream_segments)
  rule <- .get_gauss_legendre_rule(quadrature_order)
  geometry <- sf::st_geometry(stream_segments)
  quadrature_coordinates <- list()
  quadrature_weights <- numeric()
  segment_index <- integer()
  point_index <- 0L

  # A stream segment may contain multiple straight sections between vertices.
  # Place `quadrature_order` points along each positive-length straight section
  # and scale their weights by the section's share of the total segment length.
  for (index in seq_len(nrow(stream_segments))) {
    coordinates <- sf::st_coordinates(geometry[[index]])[, 1:2, drop = FALSE]
    starts <- coordinates[-nrow(coordinates), , drop = FALSE]
    ends <- coordinates[-1L, , drop = FALSE]
    edge_vectors <- ends - starts
    edge_lengths <- sqrt(rowSums(edge_vectors^2))
    positive_edges <- which(edge_lengths > 0)
    total_length <- sum(edge_lengths[positive_edges])

    if (!is.finite(total_length) || total_length <= 0) {
      stop("Every line element must contain at least one positive-length edge.")
    }

    # Place the quadrature rule along each edge and scale its weights by the
    # fraction of the segment's total length represented by that edge.
    for (edge_index in positive_edges) {
      edge_midpoint <- (starts[edge_index, ] + ends[edge_index, ]) / 2
      edge_points <- sweep(
        outer(rule$node, edge_vectors[edge_index, ]),
        2,
        edge_midpoint,
        "+"
      )

      # Store each coordinate as an sf point for later distance calculations.
      for (node_index in seq_len(nrow(edge_points))) {
        point_index <- point_index + 1L
        quadrature_coordinates[[point_index]] <- edge_points[node_index, ]
      }

      quadrature_weights <- c(
        quadrature_weights,
        rule$weight * edge_lengths[[edge_index]] / total_length
      )
      segment_index <- c(
        segment_index,
        rep(index, nrow(edge_points))
      )
    }
  }

  # Convert the stored coordinates to point geometries, then create an `sf`
  # object linking each point to its parent segment, weight, and stream width.
  quadrature_geometry <- sf::st_sfc(
    lapply(quadrature_coordinates, sf::st_point),
    crs = sf::st_crs(stream_segments)
  )
  width_unit <- units::deparse_unit(stream_segments$stream_width)
  quadrature_points <- sf::st_sf(
    segment_index = segment_index,
    weight = quadrature_weights,
    stream_width = units::set_units(
      stream_segments$stream_width[segment_index],
      width_unit,
      mode = "standard"
    ),
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
  if (!inherits(line_elements, "isw_prepared_line_elements")) {
    stop("line_elements must be returned by .prepare_line_elements().")
  }

  if (inherits(target_points, "sfc")) {
    target_points <- sf::st_sf(geometry = target_points)
  }
  .validate_point_sf(target_points, "target_points")

  prepared_targets <- sf::st_transform(
    sf::st_zm(target_points, drop = TRUE, what = "ZM"),
    line_elements$crs
  )

  structure(
    list(
      distances = sf::st_distance(
        prepared_targets,
        line_elements$quadrature_points
      ),
      segment_index = line_elements$quadrature_points$segment_index,
      weights = line_elements$quadrature_points$weight,
      stream_width = line_elements$quadrature_points$stream_width,
      segment_ids = line_elements$segment_ids,
      number_of_targets = nrow(prepared_targets),
      number_of_segments = line_elements$number_of_segments
    ),
    class = "isw_line_response_operator"
  )
}

# Evaluate a prepared target-by-line response matrix. Hydraulic inputs may use
# any compatible units; the returned matrix retains units normalized to
# days/m^2.
.evaluate_line_response_operator <- function(operator, K, D, V, elapsed_time) {
  if (!inherits(operator, "isw_line_response_operator")) {
    stop("operator must be returned by .prepare_line_response_operator().")
  }
  if (length(K) != 1 || length(D) != 1 || length(V) != 1 ||
      length(elapsed_time) != 1) {
    stop("K, D, V, and elapsed_time must be scalar for an operator evaluation.")
  }
  check_dimensionality(K, "m/s", "K")
  check_dimensionality(D, "m", "D")
  check_dimensionality(elapsed_time, "s", "elapsed_time")
  if (!is.finite(as.numeric(K)) || as.numeric(K) <= 0 ||
      !is.finite(as.numeric(D)) || as.numeric(D) <= 0 ||
      !is.finite(V) || V <= 0 ||
      !is.finite(as.numeric(elapsed_time)) || as.numeric(elapsed_time) <= 0) {
    stop("K, D, V, and elapsed_time must be finite and positive.")
  }

  number_of_targets <- operator$number_of_targets
  distance_unit <- units::deparse_unit(operator$distances)
  distances <- as.numeric(operator$distances)
  radii <- rep(
    as.numeric(units::set_units(
      operator$stream_width / 2,
      distance_unit,
      mode = "standard"
    )),
    each = number_of_targets
  )
  effective_distances <- units::set_units(
    pmax(distances, radii),
    distance_unit,
    mode = "standard"
  )
  point_response <- .theis_response_at_distance(
    effective_distances,
    K,
    D,
    V,
    elapsed_time
  )
  point_response <- matrix(
    as.numeric(units::set_units(
      point_response,
      "days/m^2",
      mode = "standard"
    )),
    nrow = number_of_targets
  )
  point_response <- sweep(point_response, 2, operator$weights, "*")
  response <- matrix(
    0,
    nrow = number_of_targets,
    ncol = operator$number_of_segments
  )

  # Combine the weighted quadrature-point responses into one response column
  # for each source segment.
  for (segment in seq_len(operator$number_of_segments)) {
    response[, segment] <- rowSums(
      point_response[, operator$segment_index == segment, drop = FALSE]
    )
  }

  dimnames(response) <- list(NULL, operator$segment_ids)
  units::set_units(response, "days/m^2", mode = "standard")
}

# Retrieve or calculate a units-bearing line-response matrix for one aquifer
# parameter set and elapsed time. response_operator is prepared once by
# .prepare_line_response_operator(). Its distances matrix has one row per
# target point and one column per stream quadrature point. The parallel
# weights and stream_width vectors describe those quadrature points, while
# segment_index maps them into the segment columns named by segment_ids;
# number_of_targets and number_of_segments define the output dimensions.
# K (hydraulic conductivity), D (aquifer thickness), and V (drainable porosity)
# determine the hydraulic response. Cache entries are indexed by K, D, V, and
# elapsed_time. The cache is also bound to its first operator so entries cannot
# be reused with another set of targets, stream elements, or quadrature rules.
.get_cached_line_response_matrix <- function(
    response_operator,
    response_cache,
    K,
    D,
    V,
    elapsed_time) {

  if (!is.environment(response_cache)) {
    stop("response_cache must be an environment.")
  }
  if (!inherits(response_operator, "isw_line_response_operator")) {
    stop(
      "response_operator must be returned by ",
      ".prepare_line_response_operator()."
    )
  }

  cached_operator <- attr(
    response_cache,
    "isw_line_response_operator",
    exact = TRUE
  )
  if (is.null(cached_operator)) {
    attr(response_cache, "isw_line_response_operator") <- response_operator
  } else if (!identical(response_operator, cached_operator)) {
    stop("response_cache cannot be reused with a different response_operator.")
  }

  parameter_key <- paste(
    format(as.numeric(units::set_units(
      K, "m/day", mode = "standard"
    )), digits = 17),
    format(as.numeric(units::set_units(
      D, "m", mode = "standard"
    )), digits = 17),
    format(V, digits = 17),
    format(as.numeric(units::set_units(
      elapsed_time, "days", mode = "standard"
    )), digits = 17),
    sep = "\r"
  )

  if (!exists(parameter_key, envir = response_cache, inherits = FALSE)) {
    assign(
      parameter_key,
      .evaluate_line_response_operator(
        response_operator,
        K = K,
        D = D,
        V = V,
        elapsed_time = elapsed_time
      ),
      envir = response_cache
    )
  }

  get(parameter_key, envir = response_cache, inherits = FALSE)
}
