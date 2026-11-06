# Profiling isw with profvis

Open `isw.Rproj` in RStudio, or start R in the package root. The profiling
tools are development tools; they do not need to be added to package Imports.
If needed, install them once:

```r
install.packages(c("profvis", "pkgload"))
```

Load the current checkout and profile the bundled example:

```r
source("analysis/performance/profile-isw.R", keep.source = TRUE)
p <- profile_isw()
p
```

Printing `p` opens the interactive profile in the RStudio Viewer or browser.
The script loads the package with `pkgload::load_all()` so package source lines
are available. It warms up the workload once before recording three runs at
10 ms sampling intervals. Loading packages and opening the viewer are outside
the recorded region.

By default, each recorded run prepares stream segments and ADF apportionment,
then calculates aquifer water-level change, including the stream injection
schedule. It uses two pumping wells, two observation wells, 400 m stream
spacing, and evaluation times from day 0 to day 90 at 30-day intervals.

To focus on modeling with inputs already prepared, or use the constant-head
method:

```r
p <- profile_isw(include_preparation = FALSE)
p

p_constant_head <- profile_isw(method = "constant_head")
p_constant_head
```

`include_preparation = FALSE` reuses the user-facing stream segments and apportionment.
The user-facing model call still validates inputs and prepares numeric hydraulic data
in meters and days. This option does not bypass those user-facing boundary checks.
Internal kernels consume numeric data, while `sf` geometry retains its CRS.

The constant-head example warns that the wells have two pump-specific aquifer
parameter sets. This is expected for the bundled data and also appears during
the warm-up run.

Increase repetitions for more samples. To investigate scaling, decrease stream
spacing or the evaluation interval; these change the problem size and may
substantially increase runtime, especially for the constant-head solver:

```r
p <- profile_isw(
  repetitions = 5,
  reach_spacing_m = 200,
  evaluation_step_days = 10
)
p
```

In the Flame Graph, look for wide blocks and click them to inspect source
lines. The Data tab helps identify functions accounting for the most time.
Parent and child times overlap, so do not add their totals. Time attributed
to a call into compiled code does not expose that library's internal lines.
Sampling results vary between runs; use this to locate bottlenecks, then
compare elapsed timings on the same inputs to assess an optimization.
Repeated runs in a profile are aggregated, not reported as individual timings.
The bundled example is a starting point; repeat with representative real inputs
before drawing conclusions about production performance.

Save a standalone report for sharing or viewing outside RStudio:

```r
dir.create("analysis/profiles", showWarnings = FALSE)
htmlwidgets::saveWidget(p, "analysis/profiles/adf.html", selfcontained = TRUE)
```

Self-contained HTML export requires Pandoc (included with RStudio). Without
Pandoc, use `selfcontained = FALSE` and keep the generated asset directory
alongside the HTML. Generated reports are ignored by Git.

For your own workload, load the source with the script above, prepare your
inputs, and wrap the calls you want to inspect:

```r
p <- profvis::profvis({
  # Your isw calls here; use a workload long enough to collect many samples.
}, interval = 0.01)
```

See the [profvis guide](https://profvis.r-lib.org/articles/profvis.html) and
[source-code profiling FAQ](https://profvis.r-lib.org/articles/faq.html).

## Compare the refactor with the original implementation

From the package root:

```r
source("analysis/performance/benchmark-isw.R", keep.source = TRUE)
study <- run_performance_study()
study$summary
```

The baseline is commit `9956c8c`, immediately before the performance refactor.
Its R functions are read into an isolated environment without switching branches;
both versions use the same installed dependencies and example input data. The
study checks complete output agreement before timing each case at tolerance
`1e-9`. Each version is warmed up, then five alternating pairs are measured
without a profiler. Garbage collection before each call is excluded, while
collections during a call remain part of its runtime.

Cases use 7, 14 and 28 stream segments, 4, 10 and 19 evaluation times, and 2,
2 and 20 observation wells respectively. All use the two example pumping wells,
their two aquifer parameter sets and a 90-day horizon. Each method is measured
with stream/apportionment preparation included and with those input tables
reused. These controlled examples are not a substitute for representative
production workloads.

Raw timings, median/range/IQR summaries, commit identifiers and session details
are written under `analysis/profiles/performance-study/`. Four current-code
profiles are then collected separately for model calls using existing stream
and apportionment tables: the example and the larger observation case, for each
method. Each produces an interactive HTML report, raw R profile and function
summaries by self and inclusive time. Inclusive percentages overlap across
parents and children; they must not be added.

Set `include_profiles = FALSE` to run timings alone. Increase `repetitions`
for a more stable timing distribution. The script measures performance and
does not alter package calculations.

To inspect allocation traffic and count helper calls in the larger case:

```r
source("analysis/performance/inspect-overhead.R", keep.source = TRUE)
overhead <- inspect_performance_overhead()
overhead$calls
overhead$memory
```

This supplemental inspection uses `testthat` for temporary function instrumentation.
Memory recording and call counting are separate from each other and from timing.
Recorded allocation bytes exclude unsized small-vector heap pages and native
allocations; they are not peak memory measurements. The instrumentation checks
output agreement and restores original functions on exit.

To compare prepared cache keys with the refactored checkpoint:

```r
run_performance_study(
  baseline_ref = "49d11ae",
  output_directory = "analysis/profiles/key-preparation-study"
)
```

If package R sources have uncommitted changes, the study saves their diff as
`current-package.patch` alongside the commit and session information.

See [PERFORMANCE-RESULTS.md](PERFORMANCE-RESULTS.md) for the initial measurements,
the cache-key comparison and remaining optimization proposals.
