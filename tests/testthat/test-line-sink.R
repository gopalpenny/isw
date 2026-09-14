make_line_sink_segments <- function(coordinates, stream_width = 10) {
  geometry <- sf::st_sfc(
    sf::st_linestring(matrix(coordinates, ncol = 2, byrow = TRUE)),
    crs = 32615
  )
  line <- sf::st_sf(reach_id = "reach", geometry = geometry)
  get_stream_segments(
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

test_that("Theis responses skip numerical underflow without changing values", {
  K <- units::set_units(10, "m/day")
  D <- units::set_units(20, "m")
  V <- 0.15
  elapsed_time <- units::set_units(10, "days")
  alpha <- K * D / V
  dimensionless_times <- c(1, 699, 701, 1000)
  distances <- sqrt(
    4 * alpha * elapsed_time * dimensionless_times
  )

  expect_silent(
    response <- isw:::.theis_response_at_distance(
      distances,
      K,
      D,
      V,
      elapsed_time
    )
  )
  expected_at_one <- 1 / (2 * pi * K * D) *
    (-0.5 * expint::expint(1))

  expect_equal(response[[1]], expected_at_one)
  expect_gt(abs(as.numeric(response[[2]])), 0)
  expect_identical(as.numeric(response[3:4]), c(0, 0))
})

test_that("local line response converges with quadrature order", {
  arguments <- list(
    along_distance = units::set_units(20, "m"),
    perpendicular_distance = units::set_units(3, "m"),
    line_length = units::set_units(100, "m"),
    K = units::set_units(10, "m/day"),
    D = units::set_units(20, "m"),
    V = 0.15,
    t = units::set_units(10, "days"),
    stream_width = units::set_units(5, "m")
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
    K = units::set_units(10, "m/day"),
    D = units::set_units(20, "m"),
    V = 0.15,
    elapsed_time = units::set_units(10, "days")
  )

  horizontal_operator <- isw:::.prepare_line_response_operator(
    horizontal_target,
    isw:::.prepare_line_elements(horizontal)
  )
  vertical_operator <- isw:::.prepare_line_response_operator(
    vertical_target,
    isw:::.prepare_line_elements(vertical)
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
    K = units::set_units(10, "m/day"),
    D = units::set_units(20, "m"),
    V = 0.15,
    elapsed_time = units::set_units(10, "days")
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
      isw:::.prepare_line_elements(forward_segments)
    )
    reversed_operator <- isw:::.prepare_line_response_operator(
      targets,
      isw:::.prepare_line_elements(reversed_segments)
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
    isw:::.prepare_line_elements(segments, quadrature_order = 16)
  )
  operator_response <- isw:::.evaluate_line_response_operator(
    operator,
    units::set_units(10, "m/day"),
    units::set_units(20, "m"),
    0.15,
    units::set_units(10, "days")
  )
  local_response <- isw:::.line_sink_aquifer_drawdown_ratio(
    units::set_units(20, "m"),
    units::set_units(30, "m"),
    units::set_units(100, "m"),
    units::set_units(10, "m/day"),
    units::set_units(20, "m"),
    0.15,
    units::set_units(10, "days"),
    units::set_units(10, "m"),
    quadrature_order = 16
  )

  expect_equal(operator_response[[1]], as.numeric(local_response))
})

test_that("finite-line responses are invariant to equivalent input units", {
  convert_units <- function(x, new_units) {
    units::set_units(x, new_units, mode = "standard")
  }
  metric_inputs <- list(
    along_distance = units::set_units(20, "m"),
    perpendicular_distance = units::set_units(3, "m"),
    line_length = units::set_units(100, "m"),
    K = units::set_units(10, "m/day"),
    D = units::set_units(20, "m"),
    V = 0.15,
    t = units::set_units(10, "days"),
    stream_width = units::set_units(5, "m"),
    quadrature_order = 16L
  )
  mixed_unit_inputs <- metric_inputs
  mixed_unit_inputs$along_distance <- convert_units(
    metric_inputs$along_distance,
    "ft"
  )
  mixed_unit_inputs$perpendicular_distance <- convert_units(
    metric_inputs$perpendicular_distance,
    "ft"
  )
  mixed_unit_inputs$line_length <- convert_units(
    metric_inputs$line_length,
    "ft"
  )
  mixed_unit_inputs$K <- convert_units(metric_inputs$K, "ft/hour")
  mixed_unit_inputs$D <- convert_units(metric_inputs$D, "ft")
  mixed_unit_inputs$t <- convert_units(metric_inputs$t, "hour")
  mixed_unit_inputs$stream_width <- convert_units(
    metric_inputs$stream_width,
    "ft"
  )

  metric_kernel_response <- do.call(
    isw:::.line_sink_aquifer_drawdown_ratio,
    metric_inputs
  )
  mixed_unit_kernel_response <- do.call(
    isw:::.line_sink_aquifer_drawdown_ratio,
    mixed_unit_inputs
  )
  expect_equal(
    convert_units(metric_kernel_response, "day/m^2"),
    convert_units(mixed_unit_kernel_response, "day/m^2"),
    tolerance = 1e-12
  )

  metric_segments <- make_line_sink_segments(
    c(-50, 0, 50, 0),
    stream_width = 5
  )
  mixed_unit_segments <- metric_segments
  mixed_unit_segments$stream_width <- convert_units(
    metric_segments$stream_width,
    "ft"
  )
  targets <- sf::st_as_sf(
    tibble::tibble(
      target_id = c("target_1", "target_2"),
      x = c(20, -35),
      y = c(3, 70)
    ),
    coords = c("x", "y"),
    crs = 32615
  )
  metric_operator <- isw:::.prepare_line_response_operator(
    targets,
    isw:::.prepare_line_elements(metric_segments)
  )
  mixed_unit_operator <- isw:::.prepare_line_response_operator(
    targets,
    isw:::.prepare_line_elements(mixed_unit_segments)
  )
  metric_operator_response <- isw:::.evaluate_line_response_operator(
    metric_operator,
    K = metric_inputs$K,
    D = metric_inputs$D,
    V = metric_inputs$V,
    elapsed_time = metric_inputs$t
  )
  mixed_unit_operator_response <- isw:::.evaluate_line_response_operator(
    mixed_unit_operator,
    K = mixed_unit_inputs$K,
    D = mixed_unit_inputs$D,
    V = mixed_unit_inputs$V,
    elapsed_time = mixed_unit_inputs$t
  )
  expect_equal(
    metric_operator_response,
    mixed_unit_operator_response,
    tolerance = 1e-12
  )
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
  stream_segments <- get_stream_segments(
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
    K = units::set_units(10, "m/day"),
    D = units::set_units(20, "m"),
    V = 0.15,
    elapsed_time = units::set_units(10, "days")
  )

  combined_operator <- isw:::.prepare_line_response_operator(
    targets,
    isw:::.prepare_line_elements(stream_segments)
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
        isw:::.prepare_line_elements(
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
  elements <- isw:::.prepare_line_elements(segments, quadrature_order = 8)
  operator <- isw:::.prepare_line_response_operator(targets, elements)

  early <- isw:::.evaluate_line_response_operator(
    operator,
    units::set_units(10, "m/day"),
    units::set_units(20, "m"),
    0.15,
    units::set_units(1, "day")
  )
  late <- isw:::.evaluate_line_response_operator(
    operator,
    units::set_units(10, "m/day"),
    units::set_units(20, "m"),
    0.15,
    units::set_units(100, "days")
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

test_that("response operators reject invalid target points", {
  segments <- make_line_sink_segments(c(-50, 0, 50, 0))
  elements <- isw:::.prepare_line_elements(segments)

  expect_error(
    isw:::.prepare_line_response_operator(
      sf::st_sfc(sf::st_point(), crs = 32615),
      elements
    ),
    "cannot contain empty geometries"
  )
  expect_error(
    isw:::.prepare_line_response_operator(
      sf::st_sfc(sf::st_point(c(Inf, 0)), crs = 32615),
      elements
    ),
    "nonfinite coordinates"
  )
  expect_error(
    isw:::.prepare_line_response_operator(
      sf::st_sfc(sf::st_point(c(0, 0))),
      elements
    ),
    "must have a defined CRS"
  )

  operator <- isw:::.prepare_line_response_operator(
    sf::st_sfc(sf::st_point(c(0, 0)), crs = 32615),
    elements
  )
  expect_s3_class(operator, "isw_line_response_operator")
})

test_that("multi-vertex line weights include edge-length fractions", {
  segments <- make_line_sink_segments(c(0, 0, 30, 0, 30, 40))
  elements <- isw:::.prepare_line_elements(segments, quadrature_order = 4)
  weights <- elements$quadrature_points$weight

  expect_equal(sum(weights[1:4]), 3 / 7)
  expect_equal(sum(weights[5:8]), 4 / 7)
  expect_equal(sum(weights), 1)
})
