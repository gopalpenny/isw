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
