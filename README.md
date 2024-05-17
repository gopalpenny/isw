
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isw

<!-- badges: start -->
<!-- badges: end -->

The goal of isw is to …

## Installation

You can install the development version of isw from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("gopalpenny/isw")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(isw)
## basic example code
```

``` r
library(units)
x1 <- set_units(c(1, 5, 10) * 1e3, "ft")
x2 <- set_units(1e3, "ft")
y <- set_units(1e3, "ft")
D <- set_units(100, "ft")
K <- set_units(0.001, "ft/sec")
t <- set_units(5, "year")
V <- 0.2 # unitless
get_depletion_from_pumping(x1, x2, y, K, D, V, t)
#>   stream_depletion_fraction aquifer_drawdown_ratio
#> 1                 0.9365474    -1.2707109 [s/ft^2]
#> 2                 0.6905933    -0.5705381 [s/ft^2]
#> 3                 0.4259739    -0.2299550 [s/ft^2]
```

For details of this function, check `?get_depletion_from_pumping`.
