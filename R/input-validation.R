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

  if (!inherits(pumping_wells, "sf")) {
    stop("pumping_wells must be an sf object.")
  }

  if (nrow(pumping_wells) == 0) {
    stop("pumping_wells must contain at least one pumping well.")
  }

  if (is.na(sf::st_crs(pumping_wells))) {
    stop("pumping_wells must have a defined CRS.")
  }

  geometry_types <- as.character(
    sf::st_geometry_type(pumping_wells, by_geometry = TRUE)
  )

  if (!all(geometry_types == "POINT")) {
    stop("Every pumping_wells geometry must be a POINT.")
  }

  if (any(sf::st_is_empty(pumping_wells))) {
    stop("pumping_wells cannot contain empty geometries.")
  }

  coordinates <- sf::st_coordinates(pumping_wells)

  if (any(!is.finite(coordinates[, 1:2, drop = FALSE]))) {
    stop("pumping_wells cannot contain missing or nonfinite coordinates.")
  }

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
