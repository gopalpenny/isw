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
#' thickness, and well diameter must retain their physical units.
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
