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

#' Get pumping-event response times
#'
#' Pair nonzero changes in pumping rate with the evaluation times at which
#' those changes can affect the modeled response.
#'
#' @param pumping_schedules A pumping-schedule object accepted by
#'   [`.validate_pumping_schedules()`].
#' @param pumping_wells A pumping-well object accepted by
#'   [`.validate_pumping_wells()`].
#' @param evaluation_times Either `NULL`, a `Date` vector, or a `units` vector
#'   with time dimensions. When `NULL`, `pumping_schedules$t` is used.
#'
#' @return A tibble containing `pump_id`, `evaluation_time`, `pumping_time`,
#'   `elapsed_time`, and `pumping_rate_change`. Pumping and elapsed times use
#'   internal units of days. Evaluation times retain the representation
#'   supplied by the user, and pumping-rate changes retain their original
#'   units.
#'
#' @details
#' Pumping rates begin at the corresponding times in `pumping_schedules$t`.
#' Each nonzero change in pumping rate is paired only with evaluation times
#' strictly later than the pumping change. Consequently, every returned
#' `elapsed_time` is positive and pumping changes that begin exactly at an
#' evaluation time are not evaluated.
#'
#' A schedule timestamp can therefore represent both the end of one pumping
#' interval and the beginning of the next. An evaluation at that boundary
#' includes the cumulative response through the completed interval but not the
#' response to the new pumping rate. When schedule times are used as the
#' default evaluation times, the first schedule time has no preceding pumping
#' response and contributes no rows to this event-level table.
#'
#' The returned columns have the following roles:
#' \describe{
#'   \item{`pump_id`}{Identifies the pumping well responsible for the pumping
#'     event. It is used to associate the event with the correct aquifer and
#'     spatial properties.}
#'   \item{`evaluation_time`}{Identifies the user-requested time represented by
#'     the row and retains the user's original `Date` or time-unit
#'     representation. After calculating individual event responses, this
#'     column is used to group and sum those responses by evaluation time.}
#'   \item{`pumping_time`}{Records when the change in pumping rate began, using
#'     the internal time units of days. A single pumping event may appear in
#'     multiple rows when it affects multiple evaluation times.}
#'   \item{`elapsed_time`}{Gives the positive time between the pumping event and
#'     the evaluation, in days. This is the time supplied to the analytical
#'     response functions.}
#'   \item{`pumping_rate_change`}{Gives the signed change in pumping rate that
#'     drives the event response. It retains the pumping-rate units from
#'     `pumping_schedules`.}
#' }
#'
#' This is an event-level intermediate table rather than the final model
#' output. Evaluation times with multiple contributing pumping events appear
#' in multiple rows. Evaluation times with no applicable events do not appear
#' here; the final output assembly must restore all requested evaluation times
#' and assign zero response where appropriate. Rows are ordered by `pump_id`
#' and then `evaluation_time`.
#'
#' The function constructs only applicable event-evaluation pairs, avoiding
#' analytical calculations for zero changes, future pumping changes, and
#' changes occurring exactly at an evaluation time.
#'
#' @examples
#' pumping_wells <- example_pumping_wells
#'
#' pumping_schedules <- tibble::tibble(
#'   t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
#'   pump_1 = units::set_units(c(100, 80, 0), "m^3/day"),
#'   pump_2 = units::set_units(c(0, 50, 50), "m^3/day")
#' )
#' # Because pump_2 remains at 50 m^3/day in March, no new pumping event is
#' # needed for that month.
#'
#' # At February 1, the January pumping interval has elapsed, but the new
#' # February pumping rate has not yet affected the response.
#' isw:::.get_pumping_response_times(
#'   pumping_schedules,
#'   pumping_wells
#' )
#'
#' # Advance the evaluation times by one month so each value represents the
#' # end of the pumping interval that begins in the corresponding schedule row.
#' evaluation_times <- seq.Date(
#'   from = pumping_schedules$t[2],
#'   by = "month",
#'   length.out = nrow(pumping_schedules)
#' )
#'
#' isw:::.get_pumping_response_times(
#'   pumping_schedules,
#'   pumping_wells,
#'   evaluation_times = evaluation_times
#' )
#'
#' @keywords internal
.get_pumping_response_times <- function(
    pumping_schedules,
    pumping_wells,
    evaluation_times = NULL) {

  pumping_rate_changes <- .get_pumping_rate_changes(
    pumping_schedules,
    pumping_wells
  )

  normalized_times <- .normalize_time_inputs(
    pumping_schedules$t,
    evaluation_times
  )

  normalized_evaluation_times <- normalized_times$evaluation_times
  output_evaluation_times <- normalized_times$output_evaluation_times

  evaluation_indices_by_event <- lapply(
    seq_len(nrow(pumping_rate_changes)),
    function(event_index) {
      pumping_time <- pumping_rate_changes$pumping_time[event_index]
      which(normalized_evaluation_times > pumping_time)
    }
  )

  event_indices <- rep(
    seq_len(nrow(pumping_rate_changes)),
    lengths(evaluation_indices_by_event)
  )
  evaluation_indices <- as.integer(unlist(
    evaluation_indices_by_event,
    use.names = FALSE
  ))

  response_times <- tibble::tibble(
    pump_id = pumping_rate_changes$pump_id[event_indices],
    evaluation_time = output_evaluation_times[evaluation_indices],
    pumping_time = pumping_rate_changes$pumping_time[event_indices],
    elapsed_time = normalized_evaluation_times[evaluation_indices] -
      pumping_rate_changes$pumping_time[event_indices],
    pumping_rate_change =
      pumping_rate_changes$pumping_rate_change[event_indices]
  )

  response_order <- order(
    response_times$pump_id,
    as.numeric(response_times$evaluation_time)
  )

  response_times[response_order, ]
}
