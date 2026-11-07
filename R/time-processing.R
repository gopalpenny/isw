# Normalize checked external times at the user-facing preparation boundary.
.normalize_time_inputs <- function(schedule_times, target_times = NULL) {
  # The boundary checked Date/time dimensions and ordering; strip to numeric days.
  output_target_times <- if (is.null(target_times)) schedule_times else target_times
  time_origin <- if (inherits(schedule_times, "Date")) schedule_times[1] else NULL
  schedule_time_day <- .time_in_days(schedule_times, time_origin)
  list(
    schedule_time_day = schedule_time_day,
    target_time_day = if (is.null(target_times)) schedule_time_day else {
      .time_in_days(target_times, time_origin)
    },
    output_target_times = output_target_times,
    time_origin = time_origin
  )
}

.restore_time_values <- function(time_day, output_times, time_origin = NULL) {
  # Reattach caller-facing dates or time units at user-facing output boundaries.
  if (inherits(output_times, "Date")) return(time_origin + time_day)
  units::set_units(units::set_units(time_day, "day"),
                   units::deparse_unit(output_times), mode = "standard")
}

.time_in_days <- function(x, time_origin = NULL) {
  # Convert checked Date/time inputs to numeric days; Date offsets share an origin.
  if (inherits(x, "Date")) return(as.numeric(x - time_origin))
  .numeric_in_standard_units(x, "days", "time")
}
