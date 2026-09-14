# Calculate the probability integral used by the Glover--Balmer solution.
#
# @param Z Dimensionless numeric value.
# @return A dimensionless numeric value.
# @noRd
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
#' @seealso [calc_straight_stream_depletion_fraction()]
#' @noRd
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

#' Calculate straight-stream depletion fraction
#'
#' Evaluate the Glover--Balmer analytical stream-depletion fraction for one
#' pumping well beside a straight, fully penetrating stream.
#'
#' @param x1 Perpendicular distance from the pumping well to the stream. Must
#'   have units of length.
#' @param K Saturated hydraulic conductivity. Must have units of length per
#'   time.
#' @param D Aquifer thickness. Must have units of length.
#' @param V Drainable porosity or specific yield. Must be dimensionless.
#' @param t Positive elapsed time since pumping began. Must have units of time.
#'
#' @return A dimensionless numeric vector giving stream depletion as a
#'   fraction of a single well's constant pumping rate.
#'
#' @details
#' This is a specialized analytical calculation, not the scheduled,
#' multi-well package workflow. Vector inputs represent independent single-well
#' cases; they are not superimposed. The current implementation uses the
#' Glover--Balmer formulation and assumes no streambed resistance.
#'
#' Multiply the returned fraction by a pumping rate to obtain a stream-
#' depletion rate. Use [model_adf_stream_depletion()] for intermittent pumping,
#' multiple wells, and spatial apportionment over a stream network.
#'
#' @references
#' Glover, R. E., and Balmer, G. G. (1954). River Depletion Resulting from
#' Pumping a Well near a River. *Transactions, American Geophysical Union*,
#' 35(3), 468--470. \doi{10.1029/TR035i003p00468}
#'
#' @seealso [calc_infinite_aquifer_drawdown_ratio()],
#'   [calc_straight_stream_drawdown_ratio()]
#' @importFrom stats pnorm
#' @export
#' @examples
#' x1 <- units::set_units(c(1, 5, 10), "km")
#' D <- units::set_units(100, "ft")
#' K <- units::set_units(0.001, "ft/s")
#' t <- units::set_units(5, "year")
#' V <- 0.2 # unitless
#'
#' calc_straight_stream_depletion_fraction(
#'   x1 = x1, K = K, D = D, V = V, t = t
#' )
calc_straight_stream_depletion_fraction <- function(
    x1,
    K,
    D,
    V,
    t) {

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
#' @noRd
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

  .theis_response_at_distance(
    distance = response_distance,
    K = K,
    D = D,
    V = V,
    t = t
  )
}

# Evaluate the infinite-aquifer Theis response at an effective distance.
.theis_response_at_distance <- function(distance, K, D, V, t) {
  check_dimensionality(distance, "m", "distance")
  check_dimensionality(K, "m/s", "K")
  check_dimensionality(D, "m", "D")
  check_dimensionality(t, "s", "t")

  alpha <- K * D / V
  check_dimensionality(alpha, "m^2/s", "alpha")

  dimensionless_time <- distance^2 / (4 * alpha * t)
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

  dimensionless_time_values <- as.numeric(dimensionless_time)
  maximum_expint_argument <- 700
  evaluate_response <- dimensionless_time_values <= maximum_expint_argument
  well_function <- numeric(length(dimensionless_time_values))

  # E1(700) is approximately 1.4e-307. Larger arguments are effectively zero
  # for model outputs, and expint() begins emitting underflow warnings near
  # 701.9. Avoiding those calls also prevents warning handling from dominating
  # large response-matrix evaluations.
  well_function[evaluate_response] <- -0.5 * expint(
    dimensionless_time_values[evaluate_response]
  )

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
#' @seealso [calc_straight_stream_drawdown_ratio()]
#' @noRd
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

#' Calculate infinite-aquifer drawdown ratio
#'
#' Evaluate the Theis drawdown response to one pumping well in an infinite
#' aquifer without stream or other boundary effects.
#'
#' @param distance Direct distance between the pumping well and observation
#'   location. Must have units of length.
#' @param K Saturated hydraulic conductivity. Must have units of length per
#'   time.
#' @param D Aquifer thickness. Must have units of length.
#' @param V Drainable porosity or specific yield. Must be dimensionless.
#' @param t Positive elapsed time since pumping began. Must have units of time.
#' @param well_diam Optional pumping-well diameter with units of length.
#'   Drawdown is evaluated at one well radius when `distance` is smaller. The
#'   default is zero.
#'
#' @return A `units` vector giving signed water-level change divided by pumping
#'   rate, with dimensions of time per length squared. Values are negative for
#'   positive pumping rates.
#'
#' @details
#' This is a specialized single-well calculation. Vector inputs represent
#' independent cases and are not superimposed. Multiply the ratio by a positive
#' pumping rate to obtain the negative water-level change caused by pumping. Use
#' [model_aquifer_water_level_change()] for scheduled pumping, multiple wells,
#' and stream recovery.
#'
#' @references
#' Theis, C. V. (1935). The relation between the lowering of the piezometric
#' surface and the rate and duration of discharge of a well using groundwater
#' storage. *Transactions, American Geophysical Union*, 16, 519--524.
#'
#' @seealso [calc_straight_stream_depletion_fraction()],
#'   [calc_straight_stream_drawdown_ratio()]
#' @importFrom expint expint
#' @importFrom units set_units
#' @export
#' @examples
#' distance <- units::set_units(c(1, 5, 10), "km")
#' D <- units::set_units(100, "ft")
#' K <- units::set_units(0.001, "ft/s")
#' t <- units::set_units(5, "year")
#' V <- 0.2 # unitless
#'
#' calc_infinite_aquifer_drawdown_ratio(
#'   distance = distance, K = K, D = D, V = V, t = t
#' )
calc_infinite_aquifer_drawdown_ratio <- function(
    distance,
    K,
    D,
    V,
    t,
    well_diam = NULL) {

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
#' @param x1 Perpendicular distance from the pumping well to the stream. Must
#'   have units of length.
#' @param x2 Perpendicular distance from the observation location to the
#'   stream. Must have units of length.
#' @param y_diff Distance between the pumping and observation wells parallel to
#'   the stream. Must have units of length.
#' @param K Saturated hydraulic conductivity. Must have units of length per
#'   time.
#' @param D Aquifer thickness. Must have units of length.
#' @param V Drainable porosity or specific yield. Must be dimensionless.
#' @param t Positive elapsed time since pumping began. Must have units of time.
#' @param well_diam Pumping-well diameter. Drawdown does not increase within
#'   one well radius. Must have units of length and defaults to zero.
#'
#' @return A units vector containing the ratio of water-level change to pumping
#'   rate, with dimensions of time divided by length squared. Values are
#'   negative for positive pumping rates.
#'
#' @details
#' This function subtracts the response of an equal-magnitude image injection
#' well from the pumping-well response. Vector inputs represent independent cases rather than
#' interacting wells or pumping schedules. Multiply the returned ratio by a
#' positive pumping rate to obtain the negative water-level change caused by
#' pumping.
#'
#' Use [calc_infinite_aquifer_drawdown_ratio()] when the stream is represented
#' explicitly by distributed injection wells; using both approaches would
#' count the stream boundary twice. Use [model_aquifer_water_level_change()]
#' for the normal scheduled, multi-well workflow.
#'
#' @examples
#' x1 <- units::set_units(1000, "m")
#' x2 <- units::set_units(500, "m")
#' y_diff <- units::set_units(250, "m")
#'
#' calc_straight_stream_drawdown_ratio(
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
calc_straight_stream_drawdown_ratio <- function(
    x1,
    x2,
    y_diff,
    K,
    D,
    V,
    t,
    well_diam = NULL) {
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
