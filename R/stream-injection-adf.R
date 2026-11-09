# Average endpoint depletion into piecewise-constant injection intervals.
.get_interval_average_injection_schedule <- function(
    depletion_at_schedule_boundaries, pumping_schedules) {
  # Validated numeric boundary times are days and depletion/injection rates m^3/day.

  # Include a zero-rate starting boundary if depletion begins after the schedule starts.
  boundary_time_day <- sort(unique(
    depletion_at_schedule_boundaries$schedule_boundary_day
  ))
  schedule_start_day <- pumping_schedules$t_day[[1]]
  prepend_zero <- length(boundary_time_day) > 0 && boundary_time_day[[1]] > schedule_start_day
  time_day <- if (prepend_zero) c(schedule_start_day, boundary_time_day) else boundary_time_day
  number_of_intervals <- length(time_day) - 1L

  empty_injection_schedule <- tibble::tibble(
    pump_id = character(),
    reach_id = character(),
    reach_segment_id = character(),
    interval_start_day = numeric(),
    interval_end_day = numeric(),
    injection_rate_m3_per_day = numeric()
  )
  pump_segment_pairs <- unique(depletion_at_schedule_boundaries[c("pump_id", "reach_segment_id")])

  # Convert boundary depletion to interval rates separately for each pump-segment pair.
  injection_schedules_by_pump_segment <- vector("list", nrow(pump_segment_pairs))
  for (pair_index in seq_len(nrow(pump_segment_pairs))) {
    pair_rows <- which(
      depletion_at_schedule_boundaries$pump_id == pump_segment_pairs$pump_id[[pair_index]] &
        depletion_at_schedule_boundaries$reach_segment_id == pump_segment_pairs$reach_segment_id[[pair_index]]
    )

    # Order endpoint rates chronologically before averaging adjacent boundaries.
    pair_rows <- pair_rows[order(depletion_at_schedule_boundaries$schedule_boundary_day[pair_rows])]
    endpoint_rate_m3_per_day <- depletion_at_schedule_boundaries$stream_depletion_rate_m3_per_day[pair_rows]
    if (prepend_zero) endpoint_rate_m3_per_day <- c(0, endpoint_rate_m3_per_day)
    if (length(endpoint_rate_m3_per_day) < 2) {
      injection_schedules_by_pump_segment[[pair_index]] <- empty_injection_schedule
      next
    }

    injection_schedules_by_pump_segment[[pair_index]] <- tibble::tibble(
      pump_id = rep(pump_segment_pairs$pump_id[[pair_index]], number_of_intervals),
      reach_id = rep(depletion_at_schedule_boundaries$reach_id[pair_rows[[1]]], number_of_intervals),
      reach_segment_id = rep(pump_segment_pairs$reach_segment_id[[pair_index]], number_of_intervals),
      interval_start_day = utils::head(time_day, -1L),
      interval_end_day = utils::tail(time_day, -1L),
      injection_rate_m3_per_day = (
        utils::head(endpoint_rate_m3_per_day, -1L) +
          utils::tail(endpoint_rate_m3_per_day, -1L)
      ) / 2
    )
  }

  do.call(rbind, c(list(empty_injection_schedule), injection_schedules_by_pump_segment))
}

.get_interval_average_injection_rate_changes <- function(stream_depletion, pumping_schedules) {
  # Prepared numeric depletion rates are m^3/day and evaluation/schedule times day.
  names(stream_depletion)[names(stream_depletion) == "evaluation_time_day"] <- "schedule_boundary_day"
  .get_injection_rate_changes(.get_interval_average_injection_schedule(
    stream_depletion, pumping_schedules
  ))
}
