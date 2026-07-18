#' Validate an sf point object
#'
#' Validate the spatial structure shared by pumping-well and observation-well
#' inputs.
#'
#' @param x An object to validate.
#' @param object_name A character string naming `x` for error messages.
#'
#' @return `x`, unchanged.
#'
#' @details
#' The object must be a nonempty `sf` object with a defined coordinate
#' reference system. Every feature must have nonempty `POINT` geometry with
#' finite horizontal coordinates.
#'
#' @keywords internal
.validate_point_sf <- function(x, object_name) {

  if (!inherits(x, "sf")) {
    stop(object_name, " must be an sf object.")
  }

  if (nrow(x) == 0) {
    stop(object_name, " must contain at least one feature.")
  }

  if (is.na(sf::st_crs(x))) {
    stop(object_name, " must have a defined CRS.")
  }

  geometry_types <- as.character(
    sf::st_geometry_type(x, by_geometry = TRUE)
  )

  if (!all(geometry_types == "POINT")) {
    stop("Every ", object_name, " geometry must be a POINT.")
  }

  if (any(sf::st_is_empty(x))) {
    stop(object_name, " cannot contain empty geometries.")
  }

  coordinates <- sf::st_coordinates(x)

  if (any(!is.finite(coordinates[, 1:2, drop = FALSE]))) {
    stop(object_name, " cannot contain missing or nonfinite coordinates.")
  }

  x
}

#' Validate pumping-well inputs
#'
#' Validate the structure, geometry, identifiers, and aquifer properties in a
#' pumping-well input object.
#'
#' @param pumping_wells An `sf` object containing one point feature per pumping
#'   well. Required columns are `pump_id`, `K`, `D`, and `V`. An optional
#'   `well_diam` column may also be supplied.
#'
#' @return The `pumping_wells` object, unchanged.
#'
#' @details
#' Each pumping well must have a unique identifier, nonempty point geometry,
#' and a defined coordinate reference system. Hydraulic conductivity, aquifer
#' thickness, and well diameter must retain their physical units. The identifier
#' `t` is reserved for the pumping-schedule time column and cannot be used as a
#' `pump_id`.
#'
#' This function validates inputs but does not transform geometry or add a
#' default `well_diam` column.
#'
#' @keywords internal
.validate_pumping_wells <- function(pumping_wells) {

  .validate_point_sf(pumping_wells, "pumping_wells")

  required_columns <- c("pump_id", "K", "D", "V")
  missing_columns <- setdiff(required_columns, names(pumping_wells))

  if (length(missing_columns) > 0) {
    stop(
      "pumping_wells is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  if (!is.character(pumping_wells$pump_id)) {
    stop("pumping_wells$pump_id must be a character vector.")
  }

  if (
    anyNA(pumping_wells$pump_id) ||
      any(trimws(pumping_wells$pump_id) == "")
  ) {
    stop("pumping_wells$pump_id cannot contain missing or empty values.")
  }

  if (anyDuplicated(pumping_wells$pump_id) > 0) {
    stop("pumping_wells$pump_id values must be unique.")
  }

  if ("t" %in% pumping_wells$pump_id) {
    stop(
      "pumping_wells$pump_id cannot be \"t\" because that name is reserved ",
      "for the pumping-schedule time column."
    )
  }

  check_dimensionality(
    pumping_wells$K,
    desired_units = "m/s",
    variable_name = "pumping_wells$K"
  )

  if (
    any(!is.finite(as.numeric(pumping_wells$K))) ||
      any(as.numeric(pumping_wells$K) <= 0)
  ) {
    stop("pumping_wells$K must contain finite, positive values.")
  }

  check_dimensionality(
    pumping_wells$D,
    desired_units = "m",
    variable_name = "pumping_wells$D"
  )

  if (
    any(!is.finite(as.numeric(pumping_wells$D))) ||
      any(as.numeric(pumping_wells$D) <= 0)
  ) {
    stop("pumping_wells$D must contain finite, positive values.")
  }

  if (
    !is.numeric(pumping_wells$V) ||
      inherits(pumping_wells$V, "units")
  ) {
    stop("pumping_wells$V must be a dimensionless numeric vector.")
  }

  if (
    any(!is.finite(pumping_wells$V)) ||
      any(pumping_wells$V <= 0 | pumping_wells$V > 1)
  ) {
    stop("pumping_wells$V must contain values greater than 0 and at most 1.")
  }

  if ("well_diam" %in% names(pumping_wells)) {
    check_dimensionality(
      pumping_wells$well_diam,
      desired_units = "m",
      variable_name = "pumping_wells$well_diam"
    )

    if (
      any(!is.finite(as.numeric(pumping_wells$well_diam))) ||
        any(as.numeric(pumping_wells$well_diam) < 0)
    ) {
      stop(
        "pumping_wells$well_diam must contain finite, nonnegative values."
      )
    }
  }

  pumping_wells
}

#' Validate observation-well inputs
#'
#' Validate the structure, geometry, and identifiers in an observation-well
#' input object.
#'
#' @param observation_wells Either `NULL` or an `sf` object containing one
#'   point feature per observation well and an `observation_id` column.
#'
#' @return `NULL` when `observation_wells` is `NULL`; otherwise, the
#'   `observation_wells` object, unchanged.
#'
#' @details
#' Observation wells are optional because stream depletion can be calculated
#' without evaluating aquifer drawdown at observation locations. When supplied,
#' each observation well must have a unique identifier, nonempty point
#' geometry, and a defined coordinate reference system.
#'
#' This function validates inputs but does not transform geometry.
#'
#' @keywords internal
.validate_observation_wells <- function(observation_wells) {

  if (is.null(observation_wells)) {
    return(NULL)
  }

  .validate_point_sf(observation_wells, "observation_wells")

  if (!("observation_id" %in% names(observation_wells))) {
    stop("observation_wells is missing required column: observation_id.")
  }

  if (!is.character(observation_wells$observation_id)) {
    stop("observation_wells$observation_id must be a character vector.")
  }

  if (
    anyNA(observation_wells$observation_id) ||
      any(trimws(observation_wells$observation_id) == "")
  ) {
    stop(
      "observation_wells$observation_id cannot contain missing or empty values."
    )
  }

  if (anyDuplicated(observation_wells$observation_id) > 0) {
    stop("observation_wells$observation_id values must be unique.")
  }

  observation_wells
}

#' Validate a time vector
#'
#' Validate the time representation shared by pumping schedules and evaluation
#' times.
#'
#' @param x A `Date` vector or a `units` vector with time dimensions.
#' @param object_name A character string naming `x` for error messages.
#'
#' @return `x`, unchanged.
#'
#' @details
#' Time values must be nonempty, finite, nonmissing, unique, and strictly
#' increasing. Character representations of dates are not accepted.
#'
#' This function validates inputs but does not convert dates or time units to
#' elapsed days.
#'
#' @keywords internal
.validate_time_vector <- function(x, object_name) {

  if (length(x) == 0) {
    stop(object_name, " must contain at least one time value.")
  }

  if (inherits(x, "Date")) {
    if (anyNA(x)) {
      stop(object_name, " cannot contain missing dates.")
    }
  } else if (inherits(x, "units")) {
    check_dimensionality(
      x,
      desired_units = "days",
      variable_name = object_name
    )

    if (any(!is.finite(as.numeric(x)))) {
      stop(object_name, " must contain finite time values.")
    }
  } else {
    stop(
      object_name,
      " must be a Date vector or a units vector with time dimensions."
    )
  }

  if (length(x) > 1 && any(diff(as.numeric(x)) <= 0)) {
    stop(object_name, " must be unique and strictly increasing.")
  }

  x
}

#' Validate pumping-schedule inputs
#'
#' Validate a shared, wide-format pumping schedule against the pumping wells
#' included in an analysis.
#'
#' @param pumping_schedules A data frame or tibble containing a time column
#'   named `t` and one pumping-rate column per pumping well. Pumping-rate column
#'   names must match `pumping_wells$pump_id` exactly.
#' @param pumping_wells A pumping-well object accepted by
#'   [`.validate_pumping_wells()`].
#'
#' @return The `pumping_schedules` object, unchanged.
#'
#' @details
#' The time column must contain either `Date` values or a `units` vector with
#' time dimensions. Times must be finite, nonmissing, unique, and strictly
#' increasing. Character representations of dates are not accepted.
#'
#' Each pumping-rate column must have units that are convertible to volume per
#' time and contain only finite values. All pumping-rate columns must use the
#' same units. Positive rates represent pumping; negative rates may be used to
#' represent injection. A rate begins at its corresponding time and remains
#' constant until the next row.
#'
#' This function validates inputs but does not normalize time units or convert
#' pumping rates to changes in pumping rate.
#'
#' @keywords internal
.validate_pumping_schedules <- function(pumping_schedules, pumping_wells) {

  .validate_pumping_wells(pumping_wells)

  if (!is.data.frame(pumping_schedules)) {
    stop("pumping_schedules must be a data frame or tibble.")
  }

  if (nrow(pumping_schedules) == 0) {
    stop("pumping_schedules must contain at least one row.")
  }

  if (anyDuplicated(names(pumping_schedules)) > 0) {
    stop("pumping_schedules cannot contain duplicate column names.")
  }

  pump_ids <- pumping_wells$pump_id
  expected_columns <- c("t", pump_ids)
  missing_columns <- setdiff(expected_columns, names(pumping_schedules))
  extra_columns <- setdiff(names(pumping_schedules), expected_columns)

  if (length(missing_columns) > 0) {
    stop(
      "pumping_schedules is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  if (length(extra_columns) > 0) {
    stop(
      "pumping_schedules contains columns that do not match pump_id values: ",
      paste(extra_columns, collapse = ", "),
      "."
    )
  }

  .validate_time_vector(pumping_schedules$t, "pumping_schedules$t")

  for (pump_id in pump_ids) {
    pumping_rate <- pumping_schedules[[pump_id]]
    variable_name <- paste0("pumping_schedules$", pump_id)

    check_dimensionality(
      pumping_rate,
      desired_units = "m^3/s",
      variable_name = variable_name
    )

    if (any(!is.finite(as.numeric(pumping_rate)))) {
      stop(variable_name, " must contain finite pumping rates.")
    }
  }

  pumping_units <- vapply(
    pumping_schedules[pump_ids],
    units::deparse_unit,
    character(1)
  )

  if (any(pumping_units != pumping_units[[1]])) {
    stop("All pumping-rate columns in pumping_schedules must use the same units.")
  }

  pumping_schedules
}

#' Validate evaluation-time inputs
#'
#' Validate optional model evaluation times against the pumping-schedule time
#' representation and starting time.
#'
#' @param evaluation_times Either `NULL`, a `Date` vector, or a `units` vector
#'   with time dimensions.
#' @param schedule_times The validated `t` vector from `pumping_schedules`.
#'
#' @return `NULL` when `evaluation_times` is `NULL`; otherwise, the
#'   `evaluation_times` vector, unchanged.
#'
#' @details
#' Evaluation times must use the same representation as `schedule_times`:
#' `Date` with `Date`, or unit-based time with unit-based time. The specific
#' units may differ when both vectors are unit based.
#'
#' Evaluation times may extend beyond the final pumping-schedule time but
#' cannot occur before the first pumping-schedule time. When
#' `evaluation_times` is `NULL`, a later preparation function will use
#' `schedule_times` as the default.
#'
#' This function validates inputs but does not apply that default or normalize
#' time values to elapsed days.
#'
#' @keywords internal
.validate_evaluation_times <- function(evaluation_times, schedule_times) {

  .validate_time_vector(schedule_times, "pumping_schedules$t")

  if (is.null(evaluation_times)) {
    return(NULL)
  }

  .validate_time_vector(evaluation_times, "evaluation_times")

  evaluation_is_date <- inherits(evaluation_times, "Date")
  schedule_is_date <- inherits(schedule_times, "Date")

  if (evaluation_is_date != schedule_is_date) {
    stop(
      "evaluation_times and pumping_schedules$t must both use Date values ",
      "or both use units time values."
    )
  }

  if (evaluation_is_date) {
    before_schedule <- evaluation_times < schedule_times[1]
  } else {
    evaluation_days <- units::set_units(evaluation_times, "days")
    schedule_start_days <- units::set_units(schedule_times[1], "days")
    before_schedule <- evaluation_days < schedule_start_days
  }

  if (any(before_schedule)) {
    stop(
      "evaluation_times cannot occur before the first pumping-schedule time."
    )
  }

  evaluation_times
}
