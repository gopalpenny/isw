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
