# glover_model.R

#' Probability integral
#' @param Z z value for estimating probability integral
#' @returns
#' Probability integral as (pnorm(Z \* sqrt(2)) - 0.5) \* 2. Necessary for glover model
#' @examples
#' \dontrun{
#' Z <- 0.5
#' prob_integral(Z)
#' }
prob_integral <- function(Z) {
  (pnorm(Z * sqrt(2)) - 0.5) * 2
}

#' Glover model of stream depletion fraction
#'
#' Glover model of stream depletion, including image well
#' @param x1 Distance between well and river
#' @param K Saturated hydraulic conductivity
#' @param D Depth of aquifer
#' @param V Drainable porosity of aquifer
#' @param t Time from pumping onset at which to calculate stream depletion fraction
#' @description
#' This function estimates stream depletion at time `t` as a fraction of pumping
#' from an individual pumping well. See Glover (1954).
#' @importFrom stats pnorm
#' @export
#' @examples
#' # Reproduce example from Glover
#' library(units)
#' x1 <- set_units(c(1, 5, 10) * 1e3, "ft")
#' D <- set_units(100, "ft")
#' K <- set_units(0.001, "ft/sec")
#' t <- set_units(5, "year")
#' V <- 0.2 # unitless
#' stream_depletion_fraction <- get_stream_depletion_fraction(x1, K, D, V, t) # % percentage
#' stream_depletion_fraction
get_stream_depletion_fraction <- function(x1, K, D, V, t) {
  alpha <- K * D / V
  x1_over_4_alpha_t <- x1 / sqrt(4 * alpha * t) # column 2 in Table 1 of glover
  if (length(units(x1_over_4_alpha_t)$numerator) != 0 | length(units(x1_over_4_alpha_t)$denominator) != 0) {
    stop("Units error resulting in dimensional value input to probability integral. ",
         "Numerator: ",units(x1_over_4_alpha_t)$numerator,", Denominator: ",units(x1_over_4_alpha_t)$denominator)
  } else{
    stream_depletion_fraction <- 1 - prob_integral(as.numeric(x1_over_4_alpha_t))
  }
  return(stream_depletion_fraction)
}

#' Drawdown for single well without boundaries
#'
#' Estimate drawdown at observation well due to pumping at another well
#'
#' @param r Distance between pumping and observation well
#' @param K Saturated hydraulic conductivity
#' @param D Depth of aquifer
#' @param V Drainable porosity of aquifer
#' @param t Time from pumping onset at which to calculate stream depletion fraction
#' @description
#' This function estimates the ratio of water level drawdown to pumping rate
#' at an observation well at time `t` after pumping initiates from an individual
#' pumping well. Note that this function does not account for the presence of any
#' aquifer boundaries. See Glover (1954).
#' @importFrom expint expint
#' @export
#' @examples
#' library(units)
#' r <- set_units(c(1, 5, 10) * 1e3, "ft")
#' D <- set_units(100, "ft")
#' K <- set_units(0.001, "ft/sec")
#' t <- set_units(5, "year")
#' V <- 0.2 # unitless
#' aquifer_drawdown_ratio <- get_aquifer_drawdown_ratio(r, K, D, V, t)
#'
#' # Drawdown per cusec pumping:
#' change_in_waterlevel_per_cusec <- aquifer_drawdown_ratio * set_units(1, "ft^3/sec")
#' change_in_waterlevel_per_cusec
get_aquifer_drawdown_ratio <- function(r, K, D, V, t) {
  alpha <- K * D / V
  radius_squared_over_4_alpha_t <- r^2/(4 * alpha * t)
  if (length(units(radius_squared_over_4_alpha_t)$numerator) != 0 | length(units(radius_squared_over_4_alpha_t)$denominator) != 0) {
    stop("Units error resulting in dimensional value input to expint. ",
         "Numerator: ",units(radius_squared_over_4_alpha_t)$numerator,", Denominator: ",units(radius_squared_over_4_alpha_t)$denominator)
  } else{
    exp_integral <- -0.5 * expint(as.numeric(radius_squared_over_4_alpha_t)) ### CHECK SIGN ON r^2 -- MAKES SMALL DIFFERENCE?
  }

  s_over_Q <- 1 / (2 * pi * K * D) * exp_integral
  return(s_over_Q)
}


#' Get stream depletion and changes in water level from pumping
#'
#' Get stream depletion and changes in water level from pumping
#'
#' @param x1 Distance of pumping well to stream
#' @param x2 Distance of observation well to stream
#' @param y Distance between pumping and observation well (parallel to stream)
#' @inheritParams get_stream_depletion_fraction
#' @export
#' @description
#' This function estimates stream depletion fraction (using
#' `get_stream_depletion_fraction`) and changes in water level at an observation
#' well (`get_aquifer_drawdown_ratio`) due to abstraction from a pumping
#' well at time `t` after pumping initiates. Like
#' `get_stream_depletion_fraction`, and unlike `get_aquifer_drawdown_ratio`,
#' The function accounts for a the effect of the stream as a constant head
#' boundary. See Glover (1954).
#' @returns
#' A `data.frame` with two columns: `stream_depletion_fraction` and
#' `aquifer_drawdown_fraction`. To calculate stream depletion and changes
#' in water level, multiply these values by the pumping rate.
#' @examples
#' library(units)
#' x1 <- set_units(c(1, 5, 10) * 1e3, "ft")
#' x2 <- set_units(1e3, "ft")
#' y <- set_units(1e3, "ft")
#' D <- set_units(100, "ft")
#' K <- set_units(0.001, "ft/sec")
#' t <- set_units(5, "year")
#' V <- 0.2 # unitless
#' get_depletion_from_pumping(x1, x2, y, K, D, V, t)
get_depletion_from_pumping <- function(x1, x2, y, K, D, V, t) {

  r_w <- sqrt(y^2 + (x2-x1)^2) # distance from observation well to pumping well
  r_wi <- sqrt(y^2 + (x2+x1)^2) # distance from observation well to pumping well (imaged across the stream)

  stream_depletion_fraction <- get_stream_depletion_fraction(x1, K, D, V, t) # %
  ds_w <- get_aquifer_drawdown_ratio(r_w, K, D, V, t) # ft / flowrate
  dw_wi <- -get_aquifer_drawdown_ratio(r_wi, K, D, V, t) # ft / flowrate
  depletion <- data.frame(stream_depletion_fraction = stream_depletion_fraction,
                          aquifer_drawdown_ratio = ds_w + dw_wi)
  return(depletion)
}
