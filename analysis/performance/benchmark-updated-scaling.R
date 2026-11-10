# Run from the package root; generated reports are ignored by Git.
source("analysis/performance/benchmark-scaling.R")

run_updated_scaling_study <- function(
    repetitions = 5L,
    output_directory = "analysis/profiles/updated-scaling-study") {
  # Fixture objects retain units and CRS; counts and measured seconds are numeric.
  stopifnot(repetitions >= 1L, repetitions == as.integer(repetitions))
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  make_sweep <- function(axis, levels, field) {
    scenarios <- data.frame(
      axis = axis, level = levels,
      number_of_observations = 20L, number_of_reaches = 1L,
      number_of_segments = 28L, number_of_pumps = 2L,
      distinct_aquifers = axis == "pumps_distinct"
    )
    scenarios[[field]] <- levels
    if (axis == "network_reaches") {
      scenarios$number_of_segments <- 28L * levels
    }
    scenarios
  }

  scenarios <- rbind(
    make_sweep("observations", c(1L, 20L, 100L, 500L, 1000L, 2000L),
               "number_of_observations"),
    make_sweep("segments", c(7L, 28L, 56L, 112L, 224L, 448L),
               "number_of_segments"),
    make_sweep("pumps_shared", c(1L, 2L, 5L, 10L, 20L, 50L),
               "number_of_pumps"),
    make_sweep("pumps_distinct", c(1L, 2L, 5L, 10L, 20L, 50L),
               "number_of_pumps"),
    make_sweep("reaches", c(1L, 7L, 28L), "number_of_reaches"),
    make_sweep("network_reaches", c(1L, 2L, 4L, 8L), "number_of_reaches")
  )
  write.csv(scenarios, file.path(output_directory, "scenarios.csv"), row.names = FALSE)
  writeLines(c(
    paste("Commit:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    "Current working-tree sources; snapshot in current-package.patch.",
    "Orders 4 (line) and 0 (point); both ADF and constant head.",
    "19 evaluations over 90 days; full stream preparation included in every timed call.",
    "Other counts fixed at 20 observations, 28 segments, one reach, two pumps.",
    "Reach-record sweep holds total length/segments fixed; network sweep grows both.",
    "Warm-up and explicit pre-call GC excluded; GC during calls included.",
    paste("Repetitions per case:", repetitions),
    capture.output(sessionInfo())
  ), file.path(output_directory, "session-info.txt"))
  writeLines(system2("git", c("diff", "HEAD", "--", "R"), stdout = TRUE),
             file.path(output_directory, "current-package.patch"))

  records <- list()
  checks <- list()
  modes <- expand.grid(method = c("adf", "constant_head"),
                       quadrature_order = c(4L, 0L), stringsAsFactors = FALSE)
  set.seed(20261109)

  for (scenario_index in sample(seq_len(nrow(scenarios)))) {
    scenario <- scenarios[scenario_index, ]
    workloads <- expected <- vector("list", nrow(modes))
    for (mode_index in seq_len(nrow(modes))) {
      mode <- modes[mode_index, ]
      workloads[[mode_index]] <- .make_scaling_workload(
        scenario, mode$method, quadrature_order = mode$quadrature_order
      )
      expected[[mode_index]] <- workloads[[mode_index]]$run(TRUE)
      stopifnot(
        nrow(expected[[mode_index]]) ==
          scenario$number_of_pumps * scenario$number_of_observations * 19L,
        all(is.finite(as.numeric(expected[[mode_index]]$water_level_change)))
      )
    }

    for (repetition in seq_len(repetitions)) {
      for (mode_index in sample(seq_len(nrow(modes)))) {
        mode <- modes[mode_index, ]
        gc()
        elapsed_seconds <- unname(system.time(
          result <- workloads[[mode_index]]$run(TRUE), gcFirst = FALSE
        )[["elapsed"]])
        stopifnot(identical(result, expected[[mode_index]]))
        records[[length(records) + 1L]] <- cbind(scenario, data.frame(
          method = mode$method, quadrature_order = mode$quadrature_order,
          include_preparation = TRUE, repetition = repetition, elapsed_seconds,
          number_of_evaluations = 19L,
          number_of_aquifers = if (scenario$distinct_aquifers)
            scenario$number_of_pumps else 1L
        ))
      }
    }
    checks[[length(checks) + 1L]] <- cbind(scenario, data.frame(
      warm_outputs_finite = TRUE, timed_outputs_identical = TRUE
    ))
    write.csv(do.call(rbind, records), file.path(output_directory, "timings.csv"),
              row.names = FALSE)
    write.csv(do.call(rbind, checks), file.path(output_directory, "output-checks.csv"),
              row.names = FALSE)
    cat("Completed", scenario$axis, scenario$level, "for both methods and orders\n")
    flush.console()
  }

  timings <- do.call(rbind, records)
  summary <- dplyr::summarise(
    dplyr::group_by(timings, axis, level, method, quadrature_order,
      number_of_observations, number_of_reaches, number_of_segments,
      number_of_pumps, number_of_aquifers, number_of_evaluations),
    median_seconds = median(elapsed_seconds), min_seconds = min(elapsed_seconds),
    max_seconds = max(elapsed_seconds), iqr_seconds = stats::IQR(elapsed_seconds),
    .groups = "drop"
  )
  write.csv(summary, file.path(output_directory, "timing-summary.csv"), row.names = FALSE)
  invisible(summary)
}

plot_updated_scaling_study <- function(
    output_directory = "analysis/profiles/updated-scaling-study") {
  summary <- read.csv(file.path(output_directory, "timing-summary.csv"))
  summary$axis <- factor(summary$axis,
    levels = c("observations", "segments", "pumps_shared", "pumps_distinct",
               "reaches", "network_reaches"),
    labels = c("Observation wells", "Stream segments", "Pumps: shared aquifer",
               "Pumps: distinct aquifers", "Reach records: fixed 28 segments",
               "Growing network: reaches"))
  summary$method <- factor(summary$method, levels = c("adf", "constant_head"),
                           labels = c("ADF", "Constant head"))
  summary$source <- factor(summary$quadrature_order, levels = c(4L, 0L),
                           labels = c("Lines: order 4", "Points: order 0"))
  chart <- ggplot2::ggplot(summary, ggplot2::aes(
    x = level, y = median_seconds, colour = method, linetype = source
  )) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = min_seconds, ymax = max_seconds),
                            linewidth = 0.4, alpha = 0.25) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~axis, scales = "free_x", ncol = 3) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::scale_colour_manual(values = c("#0072B2", "#D55E00")) +
    ggplot2::labs(
      title = "isw scaling on updated code",
      subtitle = "Five-run medians and min-max ranges; full workflow; 19 evaluations over 90 days",
      x = "Count (log scale)", y = "Elapsed seconds (log scale)",
      colour = NULL, linetype = NULL,
      caption = paste(
        "Other inputs fixed at 20 observations, 28 segments, one reach and two pumps.",
        "Growing network adds 28 segments and 2,800 meters per reach.",
        "One factor varies at a time; synthetic geometry; measured locally."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0))
  ggplot2::ggsave(file.path(output_directory, "runtime-scaling.png"), chart,
                   width = 13, height = 8, dpi = 160)
  ggplot2::ggsave(file.path(output_directory, "runtime-scaling.pdf"), chart,
                   width = 13, height = 8)
  invisible(chart)
}
