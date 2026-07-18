#' Get nonzero changes in pumping rate
#'
#' Convert a wide pumping schedule into a long table of pumping-rate changes
#' for use with analytical response functions.
#'
#' @param pumping_schedules A pumping-schedule object accepted by
#'   [`.validate_pumping_schedules()`].
#' @param pumping_wells A pumping-well object accepted by
#'   [`.validate_pumping_wells()`].
#'
#' @return A tibble with one row per nonzero change in pumping rate and columns
#'   `pump_id`, `pumping_time`, and `pumping_rate_change`. Pumping times and
#'   pumping-rate changes retain their units.
#'
#' @details
#' The pumping rate before the first schedule time is assumed to be zero. The
#' first change for each pump is therefore its rate in the first schedule row.
#' Subsequent changes are calculated by subtracting the preceding scheduled
#' rate from the new rate.
#'
#' Rows for which the change in pumping rate is exactly zero are omitted so
#' they do not trigger unnecessary analytical response calculations. Positive
#' and negative changes are retained. Pumps with no nonzero changes contribute
#' no rows to the returned table.
#'
#' @keywords internal
.get_pumping_rate_changes <- function(
    pumping_schedules,
    pumping_wells) {

  .validate_pumping_schedules(pumping_schedules, pumping_wells)
  normalized_times <- .normalize_time_inputs(pumping_schedules$t)
  pumping_times <- normalized_times$pumping_times

  pump_ids <- pumping_wells$pump_id
  event_pump_ids <- character()
  event_times <- pumping_times[0]
  event_rate_changes <- pumping_schedules[[pump_ids[[1]]]][0]

  for (pump_id in pump_ids) {
    pumping_rates <- pumping_schedules[[pump_id]]
    previous_rates <- c(
      pumping_rates[[1]] * 0,
      pumping_rates[-length(pumping_rates)]
    )
    pumping_rate_changes <- pumping_rates - previous_rates
    nonzero_change <- as.numeric(pumping_rate_changes) != 0

    event_pump_ids <- c(
      event_pump_ids,
      rep(pump_id, sum(nonzero_change))
    )
    event_times <- c(event_times, pumping_times[nonzero_change])
    event_rate_changes <- c(
      event_rate_changes,
      pumping_rate_changes[nonzero_change]
    )
  }

  tibble::tibble(
    pump_id = event_pump_ids,
    pumping_time = event_times,
    pumping_rate_change = event_rate_changes
  )
}
