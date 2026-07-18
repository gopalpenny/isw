#' Normalize pumping and evaluation times
#'
#' Convert pumping-schedule and evaluation times to a consistent internal time
#' representation while preserving the evaluation times supplied by the user.
#'
#' @param schedule_times A validated `Date` vector or `units` vector with time
#'   dimensions, typically `pumping_schedules$t`.
#' @param evaluation_times Either `NULL`, a `Date` vector, or a `units` vector
#'   with time dimensions. When `NULL`, `schedule_times` are used.
#'
#' @return A list containing:
#' \describe{
#'   \item{`pumping_times`}{Pumping-schedule times as a `units` vector in days.}
#'   \item{`evaluation_times`}{Evaluation times as a `units` vector in days.}
#'   \item{`output_evaluation_times`}{The original evaluation-time values and
#'     representation to use in user-facing results.}
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
#' The function does not modify either input object. Original evaluation times
#' are retained so model results can use the representation expected by the
#' user even though the analytical calculations operate in days.
#'
#' @keywords internal
.normalize_time_inputs <- function(schedule_times, evaluation_times = NULL) {

  .validate_time_vector(schedule_times, "pumping_schedules$t")
  .validate_evaluation_times(evaluation_times, schedule_times)

  if (is.null(evaluation_times)) {
    output_evaluation_times <- schedule_times
  } else {
    output_evaluation_times <- evaluation_times
  }

  if (inherits(schedule_times, "Date")) {
    time_origin <- schedule_times[1]

    pumping_times <- units::set_units(
      as.numeric(schedule_times - time_origin),
      "days",
      mode = "standard"
    )

    normalized_evaluation_times <- units::set_units(
      as.numeric(output_evaluation_times - time_origin),
      "days",
      mode = "standard"
    )
  } else {
    time_origin <- NULL

    pumping_times <- units::set_units(
      schedule_times,
      "days",
      mode = "standard"
    )

    normalized_evaluation_times <- units::set_units(
      output_evaluation_times,
      "days",
      mode = "standard"
    )
  }

  list(
    pumping_times = pumping_times,
    evaluation_times = normalized_evaluation_times,
    output_evaluation_times = output_evaluation_times,
    time_origin = time_origin
  )
}
