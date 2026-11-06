make_numeric_line_elements <- function(stream_segments, quadrature_order = 16L) {
  isw:::.prepare_line_elements(isw:::.prepare_numeric_segments(stream_segments), quadrature_order)
}

make_line_sink_segments <- function(coordinates, stream_width = 10) {
  geometry <- sf::st_sfc(
    sf::st_linestring(matrix(coordinates, ncol = 2, byrow = TRUE)),
    crs = 32615
  )
  line <- sf::st_sf(reach_id = "reach", geometry = geometry)
  prep_stream_segments(
    line,
    reach_spacing = units::set_units(1000, "m"),
    stream_width = units::set_units(stream_width, "m")
  )
}

test_that("Gauss-Legendre rule is normalized and symmetric", {
  rule <- isw:::.get_gauss_legendre_rule(16)

  expect_equal(sum(rule$weight), 1, tolerance = 1e-14)
  expect_equal(rule$node, -rev(rule$node), tolerance = 1e-14)
  expect_equal(rule$weight, rev(rule$weight), tolerance = 1e-14)
})

test_that("Gauss-Legendre rule matches known low-order rules", {
  one_point_rule <- isw:::.get_gauss_legendre_rule(1)
  two_point_rule <- isw:::.get_gauss_legendre_rule(2)

  expect_equal(one_point_rule$node, 0)
  expect_equal(one_point_rule$weight, 1)
  expect_equal(
    two_point_rule$node,
    c(-1, 1) / (2 * sqrt(3)),
    tolerance = 1e-14
  )
  expect_equal(two_point_rule$weight, c(0.5, 0.5), tolerance = 1e-14)
})

test_that("Theis responses skip numerical underflow without changing values", {
  # Numeric inputs use meters/days; responses are day/m^2.
  K_m_per_day <- 10
  D_m <- 20
  V <- 0.15
  t_day <- 10
  diffusivity_m2_per_day <- K_m_per_day * D_m / V
  dimensionless_times <- c(1, 699, 700 - 1e-10, 701, 1000)
  distance_m <- sqrt(4 * diffusivity_m2_per_day * t_day * dimensionless_times)

  expect_silent(response_day_per_m2 <- isw:::.theis_response_at_distance(
    distance_m, K_m_per_day, D_m, V, t_day
  ))
  expected_at_one_day_per_m2 <- -expint::expint_E1(1) / (4 * pi * K_m_per_day * D_m)
  expect_equal(response_day_per_m2[[1]], expected_at_one_day_per_m2)
  expect_true(all(abs(response_day_per_m2[2:3]) > 0))
  expect_identical(response_day_per_m2[4:5], c(0, 0))
  # This input gives u = 700 exactly, without a square-root rounding ambiguity.
  expect_gt(abs(isw:::.theis_response_at_distance(700, 175, 1, 1, 1)), 0)
})

test_that("local line response converges with quadrature order", {
  # Already-prepared numeric inputs use meters and days.
  arguments <- list(
    along_distance_m = 20, perpendicular_distance_m = 3, line_length_m = 100,
    K_m_per_day = 10, D_m = 20, V = 0.15, t_day = 10, stream_width_m = 5
  )
  response_16 <- do.call(
    isw:::.line_sink_aquifer_drawdown_ratio,
    c(arguments, list(quadrature_order = 16))
  )
  response_32 <- do.call(
    isw:::.line_sink_aquifer_drawdown_ratio,
    c(arguments, list(quadrature_order = 32))
  )
  response_64 <- do.call(
    isw:::.line_sink_aquifer_drawdown_ratio,
    c(arguments, list(quadrature_order = 64))
  )

  expect_lt(abs(as.numeric(response_32 - response_64)), 1e-5)
  expect_lt(
    abs(as.numeric(response_32 - response_64)),
    abs(as.numeric(response_16 - response_64))
  )
})

test_that("prepared operator is rotation invariant", {
  horizontal <- make_line_sink_segments(c(-50, 0, 50, 0))
  vertical <- make_line_sink_segments(c(0, -50, 0, 50))
  horizontal_target <- sf::st_as_sf(
    data.frame(x = 20, y = 30), coords = c("x", "y"), crs = 32615
  )
  vertical_target <- sf::st_as_sf(
    data.frame(x = -30, y = 20), coords = c("x", "y"), crs = 32615
  )
  hydraulics <- list(
    K_m_per_day = 10,
    D_m = 20,
    V = 0.15,
    elapsed_time_day = 10
  )

  horizontal_operator <- isw:::.prepare_line_response_operator(
    horizontal_target,
    make_numeric_line_elements(horizontal)
  )
  vertical_operator <- isw:::.prepare_line_response_operator(
    vertical_target,
    make_numeric_line_elements(vertical)
  )

  expect_equal(
    do.call(isw:::.evaluate_line_response_operator,
      c(list(horizontal_operator), hydraulics)
    ),
    do.call(isw:::.evaluate_line_response_operator,
      c(list(vertical_operator), hydraulics)
    ),
    tolerance = 1e-12
  )
})

test_that("line responses are invariant to reversed vertex order", {
  targets <- sf::st_as_sf(
    tibble::tibble(
      target_id = c("target_1", "target_2", "target_3"),
      x = c(-20, 25, 140),
      y = c(15, 70, -35)
    ),
    coords = c("x", "y"),
    crs = 32615
  )
  hydraulics <- list(
    K_m_per_day = 10,
    D_m = 20,
    V = 0.15,
    elapsed_time_day = 10
  )
  coordinate_pairs <- list(
    list(
      forward = c(0, 0, 100, 30),
      reversed = c(100, 30, 0, 0)
    ),
    list(
      forward = c(0, 0, 30, 0, 30, 40, 80, 60),
      reversed = c(80, 60, 30, 40, 30, 0, 0, 0)
    )
  )

  for (coordinate_pair in coordinate_pairs) {
    forward_segments <- make_line_sink_segments(coordinate_pair$forward)
    reversed_segments <- make_line_sink_segments(coordinate_pair$reversed)

    forward_operator <- isw:::.prepare_line_response_operator(
      targets,
      make_numeric_line_elements(forward_segments)
    )
    reversed_operator <- isw:::.prepare_line_response_operator(
      targets,
      make_numeric_line_elements(reversed_segments)
    )
    forward_response <- do.call(
      isw:::.evaluate_line_response_operator,
      c(list(forward_operator), hydraulics)
    )
    reversed_response <- do.call(
      isw:::.evaluate_line_response_operator,
      c(list(reversed_operator), hydraulics)
    )

    expect_equal(
      unname(forward_response),
      unname(reversed_response),
      tolerance = 1e-12
    )
  }
})

test_that("prepared straight-line operator matches the local-coordinate kernel", {
  segments <- make_line_sink_segments(c(-50, 0, 50, 0))
  target <- sf::st_as_sf(
    data.frame(x = 20, y = 30), coords = c("x", "y"), crs = 32615
  )
  operator <- isw:::.prepare_line_response_operator(
    target,
    make_numeric_line_elements(segments, quadrature_order = 16)
  )
  operator_response <- isw:::.evaluate_line_response_operator(
    operator,
    10,
    20,
    0.15,
    10
  )
  local_response_day_per_m2 <- isw:::.line_sink_aquifer_drawdown_ratio(
    20, 30, 100, 10, 20, 0.15, 10, 10, quadrature_order = 16
  )

  expect_type(operator_response, "double")
  expect_false(inherits(operator_response, "units"))
  expect_equal(as.numeric(operator_response[[1]]), local_response_day_per_m2)
})

test_that("response matrix rows are targets and columns are source segments", {
  stream_reaches <- sf::st_sf(
    reach_id = c("horizontal", "diagonal"),
    stream_width = units::set_units(c(2, 20), "m"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(
        c(0, 0, 80, 0),
        ncol = 2,
        byrow = TRUE
      )),
      sf::st_linestring(matrix(
        c(180, 40, 240, 160),
        ncol = 2,
        byrow = TRUE
      )),
      crs = 32615
    )
  )
  stream_segments <- prep_stream_segments(
    stream_reaches,
    reach_spacing = units::set_units(1000, "m")
  )
  targets <- sf::st_as_sf(
    tibble::tibble(
      target_id = c("target_1", "target_2"),
      x = c(15, 260),
      y = c(30, 110)
    ),
    coords = c("x", "y"),
    crs = 32615
  )
  hydraulics <- list(
    K_m_per_day = 10,
    D_m = 20,
    V = 0.15,
    elapsed_time_day = 10
  )

  combined_operator <- isw:::.prepare_line_response_operator(
    targets,
    make_numeric_line_elements(stream_segments)
  )
  combined_response <- do.call(
    isw:::.evaluate_line_response_operator,
    c(list(combined_operator), hydraulics)
  )

  expect_identical(
    colnames(combined_response),
    stream_segments$reach_segment_id
  )
  expect_false(isTRUE(all.equal(
    unname(combined_response),
    unname(t(combined_response))
  )))

  for (target_index in seq_len(nrow(targets))) {
    for (source_index in seq_len(nrow(stream_segments))) {
      single_operator <- isw:::.prepare_line_response_operator(
        targets[target_index, , drop = FALSE],
        make_numeric_line_elements(
          stream_segments[source_index, , drop = FALSE]
        )
      )
      single_response <- do.call(
        isw:::.evaluate_line_response_operator,
        c(list(single_operator), hydraulics)
      )

      expect_equal(
        unname(combined_response[target_index, source_index]),
        single_response[[1]],
        tolerance = 1e-12
      )
    }
  }
})

test_that("prepared geometry and distances are reusable across evaluations", {
  segments <- make_line_sink_segments(c(-50, 0, 50, 0))
  targets <- sf::st_as_sf(
    data.frame(x = c(0, 25), y = c(0, 50)),
    coords = c("x", "y"), crs = 32615
  )
  elements <- make_numeric_line_elements(segments, quadrature_order = 8)
  operator <- isw:::.prepare_line_response_operator(targets, elements)

  early <- isw:::.evaluate_line_response_operator(
    operator,
    10,
    20,
    0.15,
    1
  )
  late <- isw:::.evaluate_line_response_operator(
    operator,
    10,
    20,
    0.15,
    100
  )

  expect_equal(dim(early), c(2, 1))
  expect_equal(dim(late), c(2, 1))
  expect_true(all(is.finite(early)))
  expect_true(all(is.finite(late)))
  expect_false(isTRUE(all.equal(early, late)))
  expect_equal(
    unname(vapply(split(elements$quadrature_points$weight,
      elements$quadrature_points$segment_index
    ), sum, numeric(1))),
    1
  )
})

test_that("cached line responses support injection-event extraction", {
  segments <- make_line_sink_segments(c(-50, 0, 50, 0))
  target <- sf::st_as_sf(
    data.frame(x = 20, y = 30), coords = c("x", "y"), crs = 32615
  )
  operator <- isw:::.prepare_line_response_operator(
    target,
    make_numeric_line_elements(segments, quadrature_order = 8)
  )
  response_cache <- new.env(parent = emptyenv())
  hydraulics <- list(
    K_m_per_day = 10,
    D_m = 20,
    V = 0.15
  )

  first_response <- isw:::.get_cached_line_response_matrix(
    response_operator = operator,
    response_cache = response_cache,
    K_m_per_day = hydraulics$K_m_per_day,
    D_m = hydraulics$D_m,
    V = hydraulics$V,
    elapsed_time_day = 1
  )
  repeated_response <- isw:::.get_cached_line_response_matrix(
    response_operator = operator,
    response_cache = response_cache,
    K_m_per_day = hydraulics$K_m_per_day,
    D_m = hydraulics$D_m,
    V = hydraulics$V,
    elapsed_time_day = 1
  )

  expect_equal(repeated_response, first_response)
  expect_length(ls(envir = response_cache, all.names = TRUE), 1)

  other_operator <- isw:::.prepare_line_response_operator(
    sf::st_as_sf(
      data.frame(x = 25, y = 30), coords = c("x", "y"), crs = 32615
    ),
    make_numeric_line_elements(segments, quadrature_order = 8)
  )
  expect_error(
    isw:::.get_cached_line_response_matrix(
      response_operator = other_operator,
      response_cache = response_cache,
      K_m_per_day = hydraulics$K_m_per_day,
      D_m = hydraulics$D_m,
      V = hydraulics$V,
      elapsed_time_day = 1
    ),
    "cannot be reused with a different response_operator"
  )

  response_ratios <- isw:::.get_response_ratios_for_injection_events(
    response_operator = operator,
    response_cache = response_cache,
    K_m_per_day = hydraulics$K_m_per_day,
    D_m = hydraulics$D_m,
    V = hydraulics$V,
    observation_well_index = 1L,
    elapsed_times_day = c(10, 1, 10),
    segment_columns = c(1L, 1L, 1L)
  )
  late_response <- isw:::.get_cached_line_response_matrix(
    response_operator = operator,
    response_cache = response_cache,
    K_m_per_day = hydraulics$K_m_per_day,
    D_m = hydraulics$D_m,
    V = hydraulics$V,
    elapsed_time_day = 10
  )

  expect_type(response_ratios, "double")
  expect_equal(
    response_ratios,
    c(late_response[[1]], first_response[[1]], late_response[[1]])
  )
  expect_length(ls(envir = response_cache, all.names = TRUE), 2)
})

test_that("target boundary validation rejects invalid geometries", {
  expect_error(isw:::.validate_point_sf(sf::st_sf(geometry = sf::st_sfc(sf::st_point(), crs=32615)),
                                      "target_points"), "cannot contain empty geometries")
  expect_error(isw:::.validate_point_sf(sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(Inf,0)), crs=32615)),
                                      "target_points"), "nonfinite coordinates")
  expect_error(isw:::.validate_point_sf(sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0,0)))),
                                      "target_points"), "defined CRS")
})

test_that("multi-vertex line weights include edge-length fractions", {
  segments <- make_line_sink_segments(c(0, 0, 30, 0, 30, 40))
  elements <- make_numeric_line_elements(segments, quadrature_order = 4)
  weights <- elements$quadrature_points$weight

  expect_equal(sum(weights[1:4]), 3 / 7)
  expect_equal(sum(weights[5:8]), 4 / 7)
  expect_equal(sum(weights), 1)
})
