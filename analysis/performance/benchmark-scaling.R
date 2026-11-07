# Source from the package root; generated measurements are ignored by Git.
pkgload::load_all(".", quiet = TRUE)

.make_scaling_workload <- function(scenario, method, evaluation_times = NULL,
                                   pumping_times = NULL, pumping_rates = NULL,
                                   quadrature_order = 16L, api = asNamespace("isw")) {
  # Synthetic sf geometry retains its meter CRS; hydraulic inputs carry units.
  # Keep a fixed 2,800-meter straight stream and footprint across all sweeps.
  reach_endpoints_m <- seq(499000, 501800, length.out = scenario$number_of_reaches + 1L)
  stream_reaches <- sf::st_sf(
    reach_id = paste0("reach_", seq_len(scenario$number_of_reaches)),
    geometry = sf::st_sfc(lapply(seq_len(scenario$number_of_reaches), function(index) {
      if (scenario$axis == "network_reaches") {
        # Add parallel reaches at fixed 100-meter segmentation; length and segment
        # count grow together here, unlike the reach-record-only sweep.
        sf::st_linestring(cbind(c(499000, 501800), 4980000 - (index - 1L) * 200))
      } else sf::st_linestring(cbind(reach_endpoints_m[c(index, index + 1L)], 4980000))
    }), crs = 32615)
  )
  pumping_wells <- sf::st_as_sf(data.frame(
    pump_id = paste0("pump_", seq_len(scenario$number_of_pumps)),
    x = seq(499200, 501600, length.out = scenario$number_of_pumps), y = 4980300
  ), coords = c("x", "y"), crs = 32615)
  pumping_wells$K <- units::set_units(
    if (scenario$distinct_aquifers) 1.5 * (1 + 0.02 * seq_len(scenario$number_of_pumps)) else
      rep(1.5, scenario$number_of_pumps), "m/day"
  )
  pumping_wells$D <- units::set_units(rep(100, scenario$number_of_pumps), "m")
  pumping_wells$V <- rep(0.1, scenario$number_of_pumps)
  pumping_wells$well_diam <- units::set_units(rep(0.3, scenario$number_of_pumps), "m")
  observation_wells <- sf::st_as_sf(data.frame(
    observation_id = paste0("observation_", seq_len(scenario$number_of_observations)),
    x = seq(499100, 501700, length.out = scenario$number_of_observations), y = 4980500
  ), coords = c("x", "y"), crs = 32615)
  if (is.null(pumping_times)) pumping_times <- units::set_units(c(0, 30, 60), "day")
  if (is.null(pumping_rates)) pumping_rates <- units::set_units(c(400, 250, 0), "m^3/day")
  pumping_schedules <- tibble::tibble(t = pumping_times)
  for (pump_id in pumping_wells$pump_id) {
    pumping_schedules[[pump_id]] <- pumping_rates
  }
  if (is.null(evaluation_times)) evaluation_times <- units::set_units(seq(0, 90, by = 5), "day")
  prepare_inputs <- function() {
    stream_segments <- api$prep_stream_segments(
      stream_reaches, units::set_units(
        if (scenario$axis == "network_reaches") 100 else 2800 / scenario$number_of_segments, "m"
      ),
      stream_width = units::set_units(10, "m")
    )
    stopifnot(nrow(stream_segments) == scenario$number_of_segments)
    stream_apportionment <- if (method == "adf") api$prep_adf_stream_apportionment(
      pumping_wells, stream_segments, units::set_units(50, "m")
    ) else NULL
    list(stream_segments = stream_segments, stream_apportionment = stream_apportionment)
  }
  prepared <- prepare_inputs()
  run_model <- function(include_preparation) {
    inputs <- if (include_preparation) prepare_inputs() else prepared
    api$model_aquifer_water_level_change(
      pumping_wells, pumping_schedules, observation_wells, inputs$stream_segments,
      evaluation_times, injection_method = method,
      stream_apportionment = inputs$stream_apportionment,
      allow_n_aquifer_parameter_sets = scenario$number_of_pumps,
      quadrature_order = quadrature_order
    )
  }
  list(run = run_model, number_of_evaluations = length(evaluation_times))
}

run_scaling_study <- function(repetitions = 5L,
                              output_directory = "analysis/profiles/scaling-study",
                              sweeps = c("observations", "reaches", "segments",
                                         "pumps_shared", "pumps_distinct", "network_reaches")) {
  # Counts are dimensionless; benchmark elapsed time is seconds, outside model units.
  stopifnot(length(repetitions) == 1L, repetitions >= 1L,
            is.finite(repetitions), repetitions == as.integer(repetitions))
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  make_sweep <- function(axis, levels) {
    scenario <- data.frame(axis = axis, level = levels,
      number_of_observations = 20L, number_of_reaches = 1L,
      number_of_segments = 28L, number_of_pumps = 2L, distinct_aquifers = FALSE)
    field <- switch(axis, observations = "number_of_observations",
      reaches = "number_of_reaches", network_reaches = "number_of_reaches", segments = "number_of_segments",
      pumps_shared = "number_of_pumps", pumps_distinct = "number_of_pumps")
    scenario[[field]] <- levels
    if (axis == "pumps_distinct") scenario$distinct_aquifers <- TRUE
    if (axis == "network_reaches") scenario$number_of_segments <- 28L * levels
    scenario
  }
  scenarios <- do.call(rbind, list(
    make_sweep("observations", c(1L, 5L, 20L, 100L, 500L)),
    make_sweep("reaches", c(1L, 2L, 4L, 7L, 14L, 28L)),
    make_sweep("segments", c(7L, 14L, 28L, 56L, 112L, 224L)),
    make_sweep("pumps_shared", c(1L, 2L, 5L, 10L, 20L)),
    make_sweep("pumps_distinct", c(1L, 2L, 5L, 10L, 20L)),
    make_sweep("network_reaches", c(1L, 2L, 4L, 8L))
  ))
  stopifnot(all(sweeps %in% scenarios$axis))
  scenarios <- scenarios[scenarios$axis %in% sweeps, ]
  utils::write.csv(scenarios, file.path(output_directory, "scenarios.csv"), row.names = FALSE)
  writeLines(c(paste("HEAD:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    paste("Repetitions:", repetitions),
    paste("Sweeps:", paste(unique(scenarios$axis), collapse = ", ")),
    "Current working-tree R sources; 19 evaluations, 90 days.",
    "Five fixed-footprint one-factor sweeps; network_reaches also grows length/segments.",
    "Shared aquifer unless pumps_distinct; synthetic workloads, not production predictions.",
    capture.output(sessionInfo())), file.path(output_directory, "session-info.txt"))
  writeLines(system2("git", c("diff", "HEAD", "--", "R"), stdout = TRUE),
             file.path(output_directory, "current-package.patch"))
  records <- list()
  modes <- expand.grid(method = c("adf", "constant_head"),
                       include_preparation = c(FALSE, TRUE), stringsAsFactors = FALSE)
  # Seed fixes scenario/mode order; workload construction itself is deterministic.
  set.seed(20261106)
  for (scenario_index in sample(seq_len(nrow(scenarios)))) {
    scenario <- scenarios[scenario_index, ]
    workloads <- lapply(c("adf", "constant_head"), function(method) {
      .make_scaling_workload(scenario, method)
    })
    names(workloads) <- c("adf", "constant_head")
    for (method in names(workloads)) {
      reused_result <- workloads[[method]]$run(FALSE)
      prepared_result <- workloads[[method]]$run(TRUE)
      stopifnot(isTRUE(all.equal(reused_result, prepared_result, tolerance = 1e-9)),
                nrow(reused_result) == scenario$number_of_pumps *
                  scenario$number_of_observations * 19L,
                all(is.finite(as.numeric(reused_result$water_level_change))))
    }
    for (repetition in seq_len(repetitions)) {
      for (mode_index in sample(seq_len(nrow(modes)))) {
        mode <- modes[mode_index, ]
        gc()
        elapsed_seconds <- unname(system.time(
          workloads[[mode$method]]$run(mode$include_preparation), gcFirst = FALSE
        )[["elapsed"]])
        records[[length(records) + 1L]] <- cbind(scenario,
          data.frame(method = mode$method, include_preparation = mode$include_preparation,
            repetition = repetition, elapsed_seconds = elapsed_seconds,
            number_of_evaluations = 19L,
            number_of_aquifers = if (scenario$distinct_aquifers) scenario$number_of_pumps else 1L))
      }
    }
    utils::write.csv(do.call(rbind, records), file.path(output_directory, "timings.csv"), row.names = FALSE)
    cat("Completed", scenario$axis, scenario$level, "with", repetitions,
        "repetitions per method/preparation mode\n")
    flush.console()
  }
  timings <- do.call(rbind, records)
  summary <- dplyr::summarise(dplyr::group_by(timings, axis, level, method,
    include_preparation, number_of_observations, number_of_reaches,
    number_of_segments, number_of_pumps, number_of_aquifers, number_of_evaluations),
    median_seconds = median(elapsed_seconds), min_seconds = min(elapsed_seconds),
    max_seconds = max(elapsed_seconds), iqr_seconds = stats::IQR(elapsed_seconds), .groups = "drop")
  utils::write.csv(summary, file.path(output_directory, "timing-summary.csv"), row.names = FALSE)
  invisible(list(timings = timings, summary = summary))
}

plot_scaling_study <- function(input_directories = "analysis/profiles/scaling-study",
                               output_directory = input_directories[[1L]]) {
  # Plot counts and elapsed seconds; these are benchmark rather than hydraulic units.
  summary <- do.call(rbind, lapply(input_directories, function(directory) {
    utils::read.csv(file.path(directory, "timing-summary.csv"))
  }))
  summary$axis <- factor(summary$axis,
    levels = c("observations", "reaches", "segments", "pumps_shared", "pumps_distinct", "network_reaches"),
    labels = c("Observation wells", "Reach records (28 segments)", "Stream segments",
      "Pumps: shared aquifer", "Pumps: distinct aquifers", "Growing network: reaches"))
  summary$method <- factor(summary$method, levels = c("adf", "constant_head"),
                           labels = c("ADF", "Constant head"))
  summary$preparation <- factor(summary$include_preparation, levels = c(FALSE, TRUE),
                                labels = c("Prepared stream inputs", "Including stream preparation"))
  chart <- ggplot2::ggplot(summary,
    ggplot2::aes(x = level, y = median_seconds, colour = method, linetype = preparation)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = min_seconds, ymax = max_seconds),
                             linewidth = 0.4, alpha = 0.35) +
    ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 1.7) +
    ggplot2::facet_wrap(~axis, scales = "free_x", ncol = 3) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::scale_colour_manual(values = c("#0072B2", "#D55E00")) +
    ggplot2::labs(title = "isw runtime as workload size increases",
      subtitle = "Five-run medians with min-max ranges; 19 evaluations over 90 days; observation-batched code",
      x = "Count (log scale)", y = "Elapsed seconds (log scale)", colour = NULL, linetype = NULL,
      caption = paste("Other inputs fixed at 20 observations, 28 segments, one reach and two pumps.",
        "Growing network adds 28 segments and 2,800 meters per reach. Synthetic geometry; measured locally.")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0))
  ggplot2::ggsave(file.path(output_directory, "runtime-scaling.png"), chart,
                   width = 13, height = 8, dpi = 160)
  ggplot2::ggsave(file.path(output_directory, "runtime-scaling.pdf"), chart,
                   width = 13, height = 8)
  invisible(chart)
}
