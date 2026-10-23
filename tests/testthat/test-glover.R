# testing for glover model

suppressMessages(library(units))


x1 <- units::set_units(c(1, 5, 10) * 1e3, "ft")
D <- units::set_units(100, "ft")
K <- units::set_units(0.001, "ft/sec")
t <- units::set_units(5, "year")
V <- 0.2 # unitless
stream_depletion_fraction <- calc_straight_stream_depletion_fraction(x1 = x1, K = K, D = D, V = V, t = t) # % percentage

# For pasting results into expect_equal()
# paste0("c(",paste(round(stream_depletion_fraction, 5), collapse = ", "),")")

test_that("calc_straight_stream_depletion_fraction generates correct results for numeric/vector input",{
  expect_equal(round(stream_depletion_fraction, 5), c(0.93655, 0.69059, 0.42597))
})

distance <- set_units(c(1, 5, 10) * 1e3, "ft")
aquifer_drawdown_ratio <- calc_infinite_aquifer_drawdown_ratio(distance = distance, K = K, D = D, V = V, t = t)
# # For pasting results into expect_equal()
# paste0("c(",paste(round(aquifer_drawdown_ratio, 5), collapse = ", "),")")


test_that("calc_infinite_aquifer_drawdown_ratio generates correct results for numeric/vector input",{
  expect_equal(round(aquifer_drawdown_ratio, 5), set_units(c(-4.12237, -1.62017, -0.6887),"sec/ft^2"))
})


test_that("calc functions expose only explicit physical inputs", {
  calc_functions <- list(
    calc_straight_stream_depletion_fraction,
    calc_infinite_aquifer_drawdown_ratio,
    calc_straight_stream_drawdown_ratio
  )

  expect_false(any(vapply(
    calc_functions,
    function(calc_function) "df" %in% names(formals(calc_function)),
    logical(1)
  )))

  lapply(calc_functions, function(calc_function) {
    expect_error(
      do.call(calc_function, list(df = data.frame())),
      "unused argument"
    )
  })
})



# for radius < well_diam/2, drawdown does not increase.
distance <- units::set_units(c(0.5, 0.75, 1, 1.1, 2, 5, 10), "ft")
well_d <- units::set_units(2, "ft")
aquifer_drawdown_ratio <- calc_infinite_aquifer_drawdown_ratio(distance = distance, K = K, D = D, V = V, t = t, well_diam = well_d)
test_that("calc_infinite_aquifer_drawdown_ratio restrict drawdown inside well radius",{
  expect_equal(round(aquifer_drawdown_ratio,5),
               units::set_units(c(-15.11389, -15.11389, -15.11389, -14.96220, -14.01071, -12.55239, -11.44921),"s/ft^2"))
})

aquifer_drawdown_ratio <- calc_infinite_aquifer_drawdown_ratio(distance = distance, K = K, D = D, V = V, t = t, well_diam = rep(well_d, length(distance)))
test_that("calc_infinite_aquifer_drawdown_ratio restrict drawdown inside well radius, well_diam as vector",{
  expect_equal(round(aquifer_drawdown_ratio,5),
               units::set_units(c(-15.11389, -15.11389, -15.11389, -14.96220, -14.01071, -12.55239, -11.44921),"s/ft^2"))
})
