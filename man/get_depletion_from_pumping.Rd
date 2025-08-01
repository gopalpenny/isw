% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glover_model.R
\name{get_depletion_from_pumping}
\alias{get_depletion_from_pumping}
\title{Get stream depletion and changes in water level from pumping}
\usage{
get_depletion_from_pumping(
  df,
  x1 = NULL,
  x2 = NULL,
  y = NULL,
  K = NULL,
  D = NULL,
  V = NULL,
  t = NULL,
  well_diam = NULL
)
}
\arguments{
\item{df}{\code{data.frame} with columns specifying all parameters}

\item{x1}{Distance between well and river}

\item{x2}{Distance of observation well to stream}

\item{y}{Distance between pumping and observation well (parallel to stream)}

\item{K}{Saturated hydraulic conductivity}

\item{D}{Depth of aquifer}

\item{V}{Drainable porosity of aquifer}

\item{t}{Time from pumping onset at which to calculate stream depletion
fraction}

\item{well_diam}{Diameter of the well, inside which drawdown does not increase. Defaults to 0.}
}
\value{
A `data.frame` with two columns: `stream_depletion_fraction` and
  `aquifer_drawdown_fraction`. To calculate stream depletion and changes in
  water level, multiply these values by the pumping rate.
}
\description{
This function estimates stream depletion fraction (using
  `get_stream_depletion_fraction`) and changes in water level at an
  observation well (`get_aquifer_drawdown_ratio`) due to abstraction from a
  pumping well at time `t` after pumping initiates. Like
  `get_stream_depletion_fraction`, and unlike `get_aquifer_drawdown_ratio`,
  The function accounts for a the effect of the stream as a constant head
  boundary. See Glover (1954).
}
\details{
Get stream depletion and changes in water level from pumping
}
\examples{
library(units)
x1 <- set_units(c(1, 5, 10) * 1e3, "ft")
x2 <- set_units(1e3, "ft")
y <- set_units(1e3, "ft")
D <- set_units(100, "ft")
K <- set_units(0.001, "ft/sec")
t <- set_units(5, "year")
V <- 0.2 # unitless
depletion_from_pumping <- get_depletion_from_pumping(x1 = x1,
                                                     x2 = x2,
                                                     y = y,
                                                     K = K,
                                                     D = D,
                                                     V = V,
                                                     t = t)
depletion_from_pumping

# Specifying parameters as named data.frame columns
library(tibble) # simplifies specifying data.frames with units objects
df <- tibble(x1 = x1, x2 = x2, y = y, K = K, D = D, V = V, t = t)
depletion_from_pumping <- get_depletion_from_pumping(df)
depletion_from_pumping
}
