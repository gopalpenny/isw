
object <- units::set_units(1, "ft")
desired_units <- "m"

test_that("check_dimensionality receives object with wrong dimensionality", {
  expect_error(check_dimensionality(object, "m^2")) # error because object has wrong dimensionality
  expect_error(check_dimensionality(object, "hour","object"))
})
test_that("check_dimensionality receives a non-units object", {
  expect_error(check_dimensionality(1, "m")) # error because object not a units object
  expect_error(check_dimensionality(1, "m", "object"))
})


# Check that units errors work properly for get_aquifer_drawdown_ratio
D <- units::set_units(100, "ft")
K <- units::set_units(0.001, "ft/sec")
t <- units::set_units(5, "year")
V <- 0.2 # unitless
y <- units::set_units(1, "mi")
t <- units::set_units(5, "year")

val <- round(get_aquifer_drawdown_ratio(y = y, x1 = Inf, x2 = Inf, K = K, D = D, V = V, t = t),6)

test_that("get_aquifer_drawdown_ratio gives error with wrong input units", {
  expect_equal(val, units::set_units(-1.540413, "s/ft^2"))
  expect_error(get_aquifer_drawdown_ratio(y = 1, x1 = Inf, x2 = Inf, K = K, D = D, V = V, t = t))
  expect_error(get_aquifer_drawdown_ratio(y = y, x1 = Inf, x2 = Inf, K = 1, D = D, V = V, t = t))
  expect_error(get_aquifer_drawdown_ratio(y = y, x1 = Inf, x2 = Inf, K = K, D = 1, V = set_units(1,"ft"), t = t))
  expect_error(get_aquifer_drawdown_ratio(y = y, x1 = Inf, x2 = Inf, K = K, D = D, V = V, t = 1))
})

