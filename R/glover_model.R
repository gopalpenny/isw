# Calculate the probability integral used by the Glover--Balmer solution.
#
# @param Z Dimensionless numeric value.
# @return A dimensionless numeric value.
# @noRd
prob_integral <- function(Z) {
  # Numeric dimensionless input and output.
  (pnorm(Z * sqrt(2)) - 0.5) * 2
}

#' Calculate the Glover stream-depletion fraction
#'
#' Internal numerical kernel for calculating stream depletion caused by a
#' pumping well near a fully penetrating stream with no streambed resistance.
#'
#' @param x1_m Numeric distance from the pumping well to the stream, in meters.
#' @param K_m_per_day Saturated hydraulic conductivity. Numeric in meters per
#'   day.
#' @param D_m Aquifer thickness. Numeric in meters.
#' @param V Drainable porosity or specific yield. A dimensionless numeric value.
#' @param t_day Numeric elapsed time since pumping began, in days.
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
.glover_stream_depletion_fraction <- function(x1_m, K_m_per_day, D_m, V, t_day) {
  # Already-validated numeric inputs in meters/days; returns dimensionless fractions.
  diffusivity_m2_per_day <- K_m_per_day * D_m / V
  dimensionless_distance <- x1_m / sqrt(4 * diffusivity_m2_per_day * t_day)
  1 - prob_integral(dimensionless_distance)
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

  # Validate physical inputs and strip units after normalization to meters/days.
  inputs <- .prepare_analytical_inputs(list(x1 = x1), K, D, V, t)
  .glover_stream_depletion_fraction(
    x1_m = inputs$x1_m,
    K_m_per_day = inputs$K_m_per_day,
    D_m = inputs$D_m,
    V = inputs$V,
    t_day = inputs$t_day
  )
}

#' Calculate the infinite-aquifer drawdown ratio
#'
#' Internal numerical kernel for calculating drawdown from one pumping or
#' injection well without aquifer boundaries.
#'
#' @param distance_m Direct distance between the pumping or injection well and
#'   the observation location. Numeric in meters.
#' @param K_m_per_day Saturated hydraulic conductivity. Numeric in meters per
#'   day.
#' @param D_m Aquifer thickness. Numeric in meters.
#' @param V Drainable porosity or specific yield. A dimensionless numeric value.
#' @param t_day Numeric elapsed time since the rate change began, in days.
#' @param well_diam_m Well diameter. Drawdown does not increase within one well
#'   radius. Numeric in meters.
#'
#' @return A numeric vector in day/m^2 containing the ratio of water-level change
#'   to the well-rate change. The resulting dimensions are time divided by length
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
    distance_m, K_m_per_day, D_m, V, t_day, well_diam_m) {
  # Already-validated numeric inputs: m, m/day and day; returns numeric day/m^2.
  effective_distance_m <- pmax(distance_m, well_diam_m / 2)
  .theis_response_at_distance(effective_distance_m, K_m_per_day, D_m, V, t_day)
}

# Evaluate the infinite-aquifer Theis response at an effective distance.
.theis_response_at_distance <- function(distance_m, K_m_per_day, D_m, V, t_day) {
  # Already-validated numeric inputs: m, m/day and day; returns numeric day/m^2.
  transmissivity_m2_per_day <- K_m_per_day * D_m
  diffusivity_m2_per_day <- transmissivity_m2_per_day / V
  dimensionless_time <- distance_m^2 / (4 * diffusivity_m2_per_day * t_day)
  evaluate_response <- dimensionless_time <= 700
  well_function <- numeric(length(dimensionless_time))

  # E1(700) is approximately 1.4e-307, effectively zero for model outputs.
  # expint() warns near 701.9; skipping larger arguments also avoids repeated
  # warning handling during large response-matrix evaluations.
  well_function[evaluate_response] <- -0.5 * expint(
    dimensionless_time[evaluate_response]
  )
  well_function / (2 * pi * transmissivity_m2_per_day)
}

#' Calculate the straight-stream aquifer-drawdown ratio
#'
#' Internal numerical kernel for calculating aquifer drawdown at an observation
#' well caused by pumping from another well near a single straight stream
#' represented as a constant-head boundary.
#'
#' @param x1_m Numeric perpendicular pump-to-stream distance, in meters.
#' @param x2_m Numeric perpendicular observation-to-stream distance, in meters.
#' @param y_diff_m Numeric separation parallel to the stream, in meters.
#' @param K_m_per_day Saturated hydraulic conductivity. Numeric in meters per
#'   day.
#' @param D_m Aquifer thickness. Numeric in meters.
#' @param V Drainable porosity or specific yield. A dimensionless numeric value.
#' @param t_day Numeric elapsed time since pumping began, in days.
#' @param well_diam_m Pumping-well diameter. Drawdown does not increase within
#'   one well radius. Numeric in meters.
#'
#' @return A numeric vector in day/m^2 containing the ratio of aquifer drawdown
#'   to pumping rate.
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
    x1_m, x2_m, y_diff_m, K_m_per_day, D_m, V, t_day, well_diam_m) {
  # Already-validated numeric inputs: m, m/day and day; returns numeric day/m^2.
  real_distance_m <- sqrt((x2_m - x1_m)^2 + y_diff_m^2)
  image_distance_m <- sqrt((x2_m + x1_m)^2 + y_diff_m^2)
  real_response_day_per_m2 <- .theis_aquifer_drawdown_ratio(
    real_distance_m, K_m_per_day, D_m, V, t_day, well_diam_m
  )
  image_response_day_per_m2 <- .theis_aquifer_drawdown_ratio(
    image_distance_m, K_m_per_day, D_m, V, t_day, well_diam_m
  )
  real_response_day_per_m2 - image_response_day_per_m2
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
  
  # Validate physical inputs and strip units after normalization to meters/days.
  inputs <- .prepare_analytical_inputs(
    list(distance = distance), K, D, V, t, well_diam
  )
  response_day_per_m2 <- .theis_aquifer_drawdown_ratio(
    distance_m = inputs$distance_m,
    K_m_per_day = inputs$K_m_per_day,
    D_m = inputs$D_m,
    V = inputs$V,
    t_day = inputs$t_day,
    well_diam_m = inputs$well_diam_m
  )
  # Restore the caller-facing response units after numeric calculation.
  units::set_units(
    units::set_units(response_day_per_m2, "day/m^2"),
    inputs$output_response_units, mode = "standard"
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

  # Validate physical inputs and strip units after normalization to meters/days.
  inputs <- .prepare_analytical_inputs(
    list(x1 = x1, x2 = x2, y_diff = y_diff), K, D, V, t, well_diam
  )
  response_day_per_m2 <- .glover_aquifer_drawdown_ratio(
    x1_m = inputs$x1_m,
    x2_m = inputs$x2_m,
    y_diff_m = inputs$y_diff_m,
    K_m_per_day = inputs$K_m_per_day,
    D_m = inputs$D_m,
    V = inputs$V,
    t_day = inputs$t_day,
    well_diam_m = inputs$well_diam_m
  )
  # Restore the caller-facing response units after numeric calculation.
  units::set_units(
    units::set_units(response_day_per_m2, "day/m^2"),
    inputs$output_response_units, mode = "standard"
  )
}
