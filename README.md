
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isw

<!-- badges: start -->
<!-- badges: end -->

The goal of isw is to enable modeling of stream depletion and aquifer
drawdown.

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
```

Consider the following configuration of stream, pumping well, and
observation well.

``` r
library(png)
well_config_img <- readPNG("fig/pumping_observation_wells.png")
# grid::grid.raster(well_config_img)

p_well_config <- ggplot2::ggplot() + ggplot2::annotation_custom(grid::rasterGrob(well_config_img,
                                                                width=ggplot2::unit(1,"npc"),
                                                                height=ggplot2::unit(1,"npc")),
                                               -Inf, Inf, -Inf, Inf) + ggplot2::coord_equal()
p_well_config
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Stream depletion and aquifer drawdown in this scenario can be modeled
using the function `get_depletion_from_pumping` in this package.

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