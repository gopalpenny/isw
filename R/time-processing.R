#' Normalize schedule and target times
#'
#' Convert schedule and target times to a consistent internal time
#' representation while preserving the target times supplied by the caller.
#'
#' @param schedule_times A validated `Date` vector or `units` vector with time
#'   dimensions, typically `pumping_schedules$t`.
#' @param target_times Either `NULL`, a `Date` vector, or a `units` vector with
#'   time dimensions. When `NULL`, `schedule_times` are used.
#'
#' @return A list containing:
#' \describe{
#'   \item{`normalized_schedule_times`}{Schedule times as a `units` vector in
#'     days.}
#'   \item{`normalized_target_times`}{Target times as a `units` vector in
#'     days.}
#'   \item{`output_target_times`}{The original target-time values and
#'     representation to use in caller-facing results.}
#'   \item{`time_origin`}{The first pumping date for date-based inputs, or
#'     `NULL` for unit-based inputs.}
#' }
#'
#' @details
#' For date-based inputs, internal times are elapsed days from the first
#' pumping-schedule date. For unit-based inputs, values are converted to days
#' without shifting the time axis. Consequently, unit-based values retain the
#' same reference supplied by the user.
#'
#' The function does not modify either input object. Original target times are
#' retained so callers can preserve the expected representation even though
#' analytical calculations operate in days.
#'
#' @noRd
.normalize_time_inputs <- function(schedule_times, target_times = NULL) {

  .validate_time_vector(schedule_times, "pumping_schedules$t")
  .validate_target_times(target_times, schedule_times, "target_times")

  if (is.null(target_times)) {
    output_target_times <- schedule_times
  } else {
    output_target_times <- target_times
  }

  if (inherits(schedule_times, "Date")) {
    time_origin <- schedule_times[1]

    normalized_schedule_times <- units::set_units(
      as.numeric(schedule_times - time_origin),
      "days",
      mode = "standard"
    )

    normalized_target_times <- units::set_units(
      as.numeric(output_target_times - time_origin),
      "days",
      mode = "standard"
    )
  } else {
    time_origin <- NULL

    normalized_schedule_times <- units::set_units(
      schedule_times,
      "days",
      mode = "standard"
    )

    normalized_target_times <- units::set_units(
      output_target_times,
      "days",
      mode = "standard"
    )
  }

  list(
    normalized_schedule_times = normalized_schedule_times,
    normalized_target_times = normalized_target_times,
    output_target_times = output_target_times,
    time_origin = time_origin
  )
}
