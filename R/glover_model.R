# glover_model.R
# @param ... Named parameters that specify (or replace) columns from \code{df}

# TODO: Before finalizing the public drawdown interface, review the function
# names, signed drawdown convention, direct-distance argument, and compatibility
# of the separate straight-stream image-well wrapper with legacy examples.

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

#' Calculate the Glover stream-depletion fraction
#'
#' Internal numerical kernel for calculating stream depletion caused by a
#' pumping well near a fully penetrating stream with no streambed resistance.
#'
#' @param x1 Distance from the pumping well to the stream. Must have units of
#'   length.
#' @param K Saturated hydraulic conductivity. Must have units of length per
#'   time.
#' @param D Aquifer thickness. Must have units of length.
#' @param V Drainable porosity or specific yield. A dimensionless numeric value.
#' @param t Elapsed time since pumping began. Must have units of time.
#'
#' @return A dimensionless numeric vector containing the stream-depletion
#'   fraction.
#'
#' @details
#' This function contains the numerical implementation of the Glover and Balmer
#' analytical solution. Input preparation and higher-level pumping-schedule
#' processing are handled by other functions.
#'
#' Hydraulic diffusivity is calculated as:
#'
#' \deqn{\alpha = \frac{K D}{V}}
#'
#' The stream-depletion fraction is then calculated from the dimensionless
#' distance:
#'
#' \deqn{\frac{x_1}{\sqrt{4 \alpha t}}}
#'
#' @references
#' Glover, R. E., and Balmer, G. G. (1954). River Depletion Resulting from
#' Pumping a Well near a River. *Transactions, American Geophysical Union*,
#' 35(3), 468–470. \doi{10.1029/TR035i003p00468}
#'
#' @seealso [get_stream_depletion_fraction()]
#' @keywords internal
.glover_stream_depletion_fraction <- function(x1, K, D, V, t) {
  alpha <- K * D / V
  
  x1_over_4_alpha_t <- x1 / sqrt(4 * alpha * t)
  
  dimensionless <-
    length(units(x1_over_4_alpha_t)$numerator) == 0 &&
    length(units(x1_over_4_alpha_t)$denominator) == 0
  
  if (!dimensionless) {
    stop(
      "Units error resulting in dimensional value input ",
      "to probability integral. Numerator: ",
      units(x1_over_4_alpha_t)$numerator,
      ", Denominator: ",
      units(x1_over_4_alpha_t)$denominator
    )
  }
  
  1 - prob_integral(as.numeric(x1_over_4_alpha_t))
}

#' Glover model of stream depletion fraction
#'
#' Glover model of stream depletion, including image well
#' @param df \code{data.frame} with columns specifying all parameters
#' @param x1 Distance between well and river
#' @param K Saturated hydraulic conductivity
#' @param D Depth of aquifer
#' @param V Drainable porosity of aquifer
#' @param t Time from pumping onset at which to calculate stream depletion
#'   fraction
#' @description This function estimates stream depletion at time `t` as a
#'   fraction of pumping from an individual pumping well. See Glover (1954).
#'
#'   The function requires variables \code{x1}, \code{K}, \code{D}, \code{V},
#'   \code{t}. These variables can be specified as columns of \code{df}, or as
#'   named variables in the function call. If \code{df} is specified, the named
#'   variables are ignored.
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
#'
#' # Specifying parameters as numeric or vector inputs
#' stream_depletion_fraction <- get_stream_depletion_fraction(x1 = x1, K = K, D = D, V = V, t = t)
#' stream_depletion_fraction
#'
#' # Specifying parameters as named data.frame columns
#' library(tibble) # simplifies specifying data.frames with units objects
#' df <- tibble(x1 = x1, K = K, D = D, V = V, t = t)
#' stream_depletion_fraction <- get_stream_depletion_fraction(df)
#' stream_depletion_fraction
get_stream_depletion_fraction <- function(
    df,
    x1 = NULL,
    K = NULL,
    D = NULL,
    V = NULL,
    t = NULL) {

  if (!missing(df) && !is.null(df)) {
    if (!is.data.frame(df)) {
      stop("df must be a data.frame object")
    }

    x1 <- df[["x1"]]
    K <- df[["K"]]
    D <- df[["D"]]
    V <- df[["V"]]
    t <- df[["t"]]
  }

  .glover_stream_depletion_fraction(
    x1 = x1,
    K = K,
    D = D,
    V = V,
    t = t
  )
}

#' Calculate the infinite-aquifer drawdown ratio
#'
#' Internal numerical kernel for calculating drawdown from one pumping or
#' injection well without aquifer boundaries.
#'
#' @param distance Direct distance between the pumping or injection well and
#'   the observation location. Must have units of length.
#' @param K Saturated hydraulic conductivity. Must have units of length per
#'   time.
#' @param D Aquifer thickness. Must have units of length.
#' @param V Drainable porosity or specific yield. A dimensionless numeric value.
#' @param t Elapsed time since the rate change began. Must have units of time.
#' @param well_diam Well diameter. Drawdown does not increase within one well
#'   radius. Must have units of length.
#'
#' @return A units vector containing the ratio of water-level change to the
#'   well-rate change. The resulting dimensions are time divided by length
#'   squared. Positive pumping rates produce negative water-level changes;
#'   negative rates represent injection and produce positive changes.
#'
#' @details
#' This kernel evaluates the Theis well function using transmissivity
#' \eqn{K D} and hydraulic diffusivity \eqn{K D / V}. It contains no image
#' well or other boundary correction.
#'
#' @keywords internal
.theis_aquifer_drawdown_ratio <- function(
    distance,
    K,
    D,
    V,
    t,
    well_diam) {

  check_dimensionality(distance, "m", "distance")
  check_dimensionality(K, "m/s", "K")
  check_dimensionality(D, "m", "D")
  check_dimensionality(t, "s", "t")
  check_dimensionality(well_diam, "m", "well_diam")

  alpha <- K * D / V
  check_dimensionality(alpha, "m^2/s", "alpha")

  well_radius <- units::set_units(well_diam / 2, units(distance))
  response_distance <- dplyr::if_else(
    distance < well_radius,
    well_radius,
    distance
  )
  check_dimensionality(response_distance, "m", "response_distance")

  dimensionless_time <- response_distance^2 / (4 * alpha * t)
  dimensionless <-
    length(units(dimensionless_time)$numerator) == 0 &&
    length(units(dimensionless_time)$denominator) == 0

  if (!dimensionless) {
    stop(
      "Units error resulting in dimensional value input to expint. ",
      "Numerator: ",
      units(dimensionless_time)$numerator,
      ", Denominator: ",
      units(dimensionless_time)$denominator
    )
  }

  well_function <- -0.5 * expint(as.numeric(dimensionless_time))
  1 / (2 * pi * K * D) * well_function
}

#' Calculate the straight-stream aquifer-drawdown ratio
#'
#' Internal numerical kernel for calculating aquifer drawdown at an observation
#' well caused by pumping from another well near a single straight stream
#' represented as a constant-head boundary.
#'
#' @param x1 Perpendicular distance from the pumping well to the stream.
#' @param x2 Perpendicular distance from the observation well to the stream.
#' @param y_diff Distance between the pumping and observation wells parallel
#'   to the stream.
#' @param K Saturated hydraulic conductivity. Must have units of length per
#'   time.
#' @param D Aquifer thickness. Must have units of length.
#' @param V Drainable porosity or specific yield. A dimensionless numeric value.
#' @param t Elapsed time since pumping began. Must have units of time.
#' @param well_diam Pumping-well diameter. Drawdown does not increase within
#'   one well radius. Must have units of length.
#'
#' @return A units vector containing the ratio of aquifer drawdown to pumping
#'   rate. The resulting dimensions are time divided by length squared.
#'
#' @details
#' For a stream boundary, the function calculates drawdown from the pumping
#' well and subtracts the response associated with its image well. The pumping-
#' well and image-well distances from the observation well are:
#'
#' \deqn{r_w = \sqrt{(x_2-x_1)^2+y_{diff}^2}}
#'
#' \deqn{r_{wi} = \sqrt{(x_2+x_1)^2+y_{diff}^2}}
#'
#' @references
#' Glover, R. E., and Balmer, G. G. (1954). River Depletion Resulting from
#' Pumping a Well near a River. *Transactions, American Geophysical Union*,
#' 35(3), 468–470. \doi{10.1029/TR035i003p00468}
#'
#' @seealso [get_straight_stream_drawdown_ratio()]
#' @keywords internal
.glover_aquifer_drawdown_ratio <- function(
    x1,
    x2,
    y_diff,
    K,
    D,
    V,
    t,
    well_diam) {
  check_dimensionality(x1, "m", "x1")
  check_dimensionality(x2, "m", "x2")
  check_dimensionality(y_diff, "m", "y_diff")

  rw <- sqrt((x2 - x1)^2 + y_diff^2)
  rwi <- sqrt((x2 + x1)^2 + y_diff^2)
  
  sw_over_Q <- .theis_aquifer_drawdown_ratio(
    distance = rw,
    K = K,
    D = D,
    V = V,
    t = t,
    well_diam = well_diam
  )

  swi_over_Q <- .theis_aquifer_drawdown_ratio(
    distance = rwi,
    K = K,
    D = D,
    V = V,
    t = t,
    well_diam = well_diam
  )

  sw_over_Q - swi_over_Q
}

#' Drawdown from a single well without aquifer boundaries
#'
#' Estimate water-level change at an observation location due to pumping or
#' injection at one well in an infinite aquifer.
#'
#' @inheritParams get_stream_depletion_fraction
#' @param distance Direct distance between the well and observation location.
#' @param well_diam Diameter of the well, inside which drawdown does not increase. Defaults to 0.
#' @description
#' This function estimates the ratio of water level drawdown to pumping rate
#' at an observation well at time `t` after pumping initiates from an individual
#' pumping well. It does not include an image well or any other aquifer
#' boundary. See Theis (1935).
#' @importFrom expint expint
#' @importFrom units set_units
#' @export
#' @examples
#' library(units)
#' distance <- set_units(c(1, 5, 10) * 1e3, "ft")
#' D <- set_units(100, "ft")
#' K <- set_units(0.001, "ft/sec")
#' t <- set_units(5, "year")
#' V <- 0.2 # unitless
#' aquifer_drawdown_ratio <- get_aquifer_drawdown_ratio(distance = distance,
#'                                                      K = K,
#'                                                      D = D,
#'                                                      V = V,
#'                                                      t = t)
#'
#' # Drawdown per cusec pumping:
#' change_in_waterlevel_per_cusec <- aquifer_drawdown_ratio * set_units(1, "ft^3/sec")
#' change_in_waterlevel_per_cusec
#'
#'
#' # Specifying parameters as named data.frame columns
#' library(tibble) # simplifies specifying data.frames with units objects
#' df <- tibble(distance = distance, K = K, D = D, V = V, t = t)
#' aquifer_drawdown_ratio <- get_aquifer_drawdown_ratio(df)
#' aquifer_drawdown_ratio
#'
#' # for radius < well_diam/2, drawdown does not increase.
#' distance <- set_units(seq(0.25,2, by = 0.25), "ft")
#' well_diam <- set_units(2, "ft")
#' aquifer_drawdown_ratio <- get_aquifer_drawdown_ratio(distance = distance,
#'                                                      K = K, D = D,
#'                                                      V = V, t = t,
#'                                                      well_diam = well_diam)
#' aquifer_drawdown_ratio
get_aquifer_drawdown_ratio <- function(
    df,
    distance = NULL,
    K = NULL,
    D = NULL,
    V = NULL,
    t = NULL,
    well_diam = NULL) {
  
  if (!missing(df) && !is.null(df)) {
    if (!is.data.frame(df)) {
      stop("df must be a data.frame object")
    }
    
    distance <- df[["distance"]]
    K <- df[["K"]]
    D <- df[["D"]]
    V <- df[["V"]]
    t <- df[["t"]]
    well_diam <- df[["well_diam"]]
  }
  
  if (is.null(well_diam)) {
    well_diam <- units::set_units(0, "ft")
  }
  
  .theis_aquifer_drawdown_ratio(
    distance = distance,
    K = K,
    D = D,
    V = V,
    t = t,
    well_diam = well_diam
  )
}

#' Drawdown near a straight constant-head stream
#'
#' Estimate water-level change at an observation location using a pumping well
#' and its image across a straight, fully penetrating constant-head stream.
#'
#' @inheritParams get_stream_depletion_fraction
#' @param x1 Perpendicular distance from the pumping well to the stream.
#' @param x2 Perpendicular distance from the observation well to the stream.
#' @param y_diff Distance between the pumping and observation wells parallel to
#'   the stream.
#' @param well_diam Pumping-well diameter. Drawdown does not increase within
#'   one well radius. Defaults to zero.
#'
#' @return A units vector containing the ratio of water-level change to pumping
#'   rate, with dimensions of time divided by length squared.
#'
#' @details
#' This function subtracts the response of an equal-magnitude image injection
#' well from the pumping-well response. Use [get_aquifer_drawdown_ratio()] when
#' the stream is represented explicitly by distributed injection wells; using
#' both approaches would count the stream boundary twice.
#'
#' @examples
#' x1 <- units::set_units(1000, "m")
#' x2 <- units::set_units(500, "m")
#' y_diff <- units::set_units(250, "m")
#'
#' get_straight_stream_drawdown_ratio(
#'   x1 = x1,
#'   x2 = x2,
#'   y_diff = y_diff,
#'   K = units::set_units(1e-5, "m/s"),
#'   D = units::set_units(50, "m"),
#'   V = 0.15,
#'   t = units::set_units(1, "year")
#' )
#'
#' @export
get_straight_stream_drawdown_ratio <- function(
    df,
    x1 = NULL,
    x2 = NULL,
    y_diff = NULL,
    K = NULL,
    D = NULL,
    V = NULL,
    t = NULL,
    well_diam = NULL) {

  if (!missing(df) && !is.null(df)) {
    if (!is.data.frame(df)) {
      stop("df must be a data.frame object")
    }

    x1 <- df[["x1"]]
    x2 <- df[["x2"]]
    y_diff <- df[["y_diff"]]
    K <- df[["K"]]
    D <- df[["D"]]
    V <- df[["V"]]
    t <- df[["t"]]
    well_diam <- df[["well_diam"]]
  }

  if (is.null(well_diam)) {
    well_diam <- units::set_units(0, "m")
  }

  .glover_aquifer_drawdown_ratio(
    x1 = x1,
    x2 = x2,
    y_diff = y_diff,
    K = K,
    D = D,
    V = V,
    t = t,
    well_diam = well_diam
  )
}


#' Get stream depletion and changes in water level from pumping
#'
#' Get stream depletion and changes in water level from pumping
#'
#' @inheritParams get_stream_depletion_fraction
#' @inheritParams get_straight_stream_drawdown_ratio
#' @export
#' @description This function estimates stream depletion fraction (using
#'   `get_stream_depletion_fraction`) and changes in water level at an
#'   observation well (`get_straight_stream_drawdown_ratio`) due to abstraction from a
#'   pumping well at time `t` after pumping initiates. Like
#'   `get_stream_depletion_fraction`, the drawdown calculation accounts for
#'   the effect of a straight stream as a constant-head
#'   boundary. See Glover (1954).
#' @returns A `data.frame` with two columns: `stream_depletion_fraction` and
#'   `aquifer_drawdown_fraction`. To calculate stream depletion and changes in
#'   water level, multiply these values by the pumping rate.
#' @examples
#' library(units)
#' x1 <- set_units(c(1, 5, 10) * 1e3, "ft")
#' x2 <- set_units(1e3, "ft")
#' y_diff <- set_units(1e3, "ft")
#' D <- set_units(100, "ft")
#' K <- set_units(0.001, "ft/sec")
#' t <- set_units(5, "year")
#' V <- 0.2 # unitless
#' depletion_from_pumping <- get_depletion_from_pumping(x1 = x1,
#'                                                      x2 = x2,
#'                                                      y_diff = y_diff,
#'                                                      K = K,
#'                                                      D = D,
#'                                                      V = V,
#'                                                      t = t)
#' depletion_from_pumping
#'
#' # Specifying parameters as named data.frame columns
#' library(tibble) # simplifies specifying data.frames with units objects
#' df <- tibble(x1 = x1, x2 = x2, y_diff = y_diff,
#'              K = K, D = D, V = V, t = t)
#' depletion_from_pumping <- get_depletion_from_pumping(df)
#' depletion_from_pumping
get_depletion_from_pumping <- function(df, x1 = NULL, x2 = NULL,
                                       y_diff = NULL,
                                       K = NULL, D = NULL, V = NULL, t = NULL,
                                       well_diam = NULL) {
  if (!missing(df)) { # if df is specified, replace NULL parameters with df columns
    if (!is.null(df)) {
      if (!("data.frame" %in% class(df))) {
        stop("df must be a data.frame object")
      }
      for (var in c("x1", "x2", "y_diff", "K", "D", "V",
                    "t", "well_diam")) {
        assign(var, df[[var]])
      }
    }
  }

  if (is.null(well_diam)) {
    well_diam <- units::set_units(0, "ft")
  }

  stream_depletion_fraction <- get_stream_depletion_fraction(x1 = x1, K = K, D = D, V = V, t = t) # %
  ds_w <- get_straight_stream_drawdown_ratio(
    y_diff = y_diff, x1 = x1, x2 = x2,
    K = K, D = D, V = V, t = t, well_diam = well_diam
  ) # ft / flowrate
  depletion <- data.frame(stream_depletion_fraction = stream_depletion_fraction,
                          aquifer_drawdown_ratio = ds_w)
  return(depletion)
}
