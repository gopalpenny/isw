# testing for glover model

suppressMessages(library(units))
suppressMessages(library(tibble))


x1 <- units::set_units(c(1, 5, 10) * 1e3, "ft")
D <- units::set_units(100, "ft")
K <- units::set_units(0.001, "ft/sec")
t <- units::set_units(5, "year")
V <- 0.2 # unitless
stream_depletion_fraction <- get_stream_depletion_fraction(x1 = x1, K = K, D = D, V = V, t = t) # % percentage

# For pasting results into expect_equal()
# paste0("c(",paste(round(stream_depletion_fraction, 5), collapse = ", "),")")

test_that("get_stream_depletion_fraction generates correct results for numeric/vector input",{
  expect_equal(round(stream_depletion_fraction, 5), c(0.93655, 0.69059, 0.42597))
})

stream_depletion_fraction_df <- get_stream_depletion_fraction(tibble(x1 = x1, K = K, D = D, V = V, t = t)) # % percentage
test_that("get_stream_depletion_fraction generates correct results for data.frame input",{
  expect_equal(round(stream_depletion_fraction, 5), round(stream_depletion_fraction_df, 5))
})


r <- set_units(c(1, 5, 10) * 1e3, "ft")
aquifer_drawdown_ratio <- get_aquifer_drawdown_ratio(r = r, K = K, D = D, V = V, t = t)
# # For pasting results into expect_equal()
# paste0("c(",paste(round(aquifer_drawdown_ratio, 5), collapse = ", "),")")


test_that("get_aquifer_drawdown_ratio generates correct results for numeric/vector input",{
  expect_equal(round(aquifer_drawdown_ratio, 5), set_units(c(-4.12237, -1.62017, -0.6887),"sec/ft^2"))
})


aquifer_drawdown_ratio_df <- get_aquifer_drawdown_ratio(tibble(r = r, K = K, D = D, V = V, t = t))
test_that("get_aquifer_drawdown_ratio generates correct results for data.frame input",{
  expect_equal(round(aquifer_drawdown_ratio, 5), round(aquifer_drawdown_ratio_df, 5))
})



x1 <- set_units(c(1, 5, 10) * 1e3, "ft")
x2 <- set_units(1e3, "ft")
y <- set_units(1e3, "ft")
pumping_depletion <- get_depletion_from_pumping(x1 = x1, x2 = x2, y = y, K = K, D = D, V = V, t = t) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), function(x) round(as.numeric(x), 4)))
pumping_depletion$aquifer_drawdown_ratio <-
  set_units(pumping_depletion$aquifer_drawdown_ratio, "sec/ft^2")
# # For pasting results into expect_equal()
# pumping_depletion %>%
#   ggp::print_data_frame_for_entry()

pumping_depletion_prep <-
  data.frame(stream_depletion_fraction=c(0.9365, 0.6906, 0.426),
             aquifer_drawdown_ratio=set_units(c(-1.2707, -0.5705, -0.23),"sec/ft^2"))

test_that("get_depletion_from_pumping generates correct results for numeric/vector input",{
  expect_equal(pumping_depletion, pumping_depletion_prep)
})


pumping_depletion_df <- get_depletion_from_pumping(tibble(x1 = x1, x2 = x2, y = y, K = K, D = D, V = V, t = t)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), function(x) round(as.numeric(x), 4)))
pumping_depletion_df$aquifer_drawdown_ratio <-
  set_units(pumping_depletion_df$aquifer_drawdown_ratio, "sec/ft^2")
test_that("get_depletion_from_pumping generates correct results for data.frame input",{
  expect_equal(pumping_depletion, pumping_depletion_df)
})


# for radius < well_diam/2, drawdown does not increase.
r <- units::set_units(c(0.5, 0.75, 1, 1.1, 2, 5, 10), "ft")
aquifer_drawdown_ratio <- get_aquifer_drawdown_ratio(r = r, K = K, D = D, V = V, t = t, well_diam = units::set_units(2, "ft"))
test_that("get_aquifer_drawdown_ratio restrict drawdown inside well radius",{
  expect_equal(round(aquifer_drawdown_ratio,5),
               units::set_units(c(-15.11389, -15.11389, -15.11389, -14.96220, -14.01071, -12.55239, -11.44921),"s/ft^2"))
})


