# Construct nonzero rate-change events from a prepared numeric schedule.
.get_pumping_rate_changes <- function(pumping_schedules, pump_ids) {
  # Validated numeric schedule: t_day and pump columns suffixed _m3_per_day.
  # Convert each pump's scheduled rates into nonzero rate-change events.
  pumping_events_by_pump <- lapply(pump_ids, function(pump_id) {
    rate_m3_per_day <- pumping_schedules[[paste0(pump_id, "_m3_per_day")]]
    change_m3_per_day <- diff(c(0, rate_m3_per_day))
    changed <- which(change_m3_per_day != 0)
    tibble::tibble(
      pump_id = rep(pump_id, length(changed)),
      pumping_time_day = pumping_schedules$t_day[changed],
      pumping_rate_change_m3_per_day = change_m3_per_day[changed]
    )
  })
  do.call(rbind, pumping_events_by_pump)
}

# Pair already-prepared events with strictly later requested evaluation times.
.get_pumping_response_times <- function(pumping_events, evaluation_time_day) {
  # Validated numeric event times are days and rates m^3/day; indices are dimensionless.
  evaluation_indices_by_event <- lapply(pumping_events$pumping_time_day, function(time_day) {
    which(evaluation_time_day > time_day)
  })
  event_indices <- rep(seq_len(nrow(pumping_events)), lengths(evaluation_indices_by_event))
  evaluation_indices <- as.integer(unlist(evaluation_indices_by_event, use.names = FALSE))
  response_evaluation_time_day <- evaluation_time_day[evaluation_indices]
  response_elapsed_time_day <- response_evaluation_time_day - pumping_events$pumping_time_day[event_indices]
  result <- tibble::tibble(
    pump_id = pumping_events$pump_id[event_indices],
    evaluation_index = evaluation_indices,
    evaluation_time_day = response_evaluation_time_day,
    pumping_time_day = pumping_events$pumping_time_day[event_indices],
    elapsed_time_day = response_elapsed_time_day,
    pumping_rate_change_m3_per_day = pumping_events$pumping_rate_change_m3_per_day[event_indices]
  )
  result[order(result$pump_id, result$evaluation_index), ]
}
