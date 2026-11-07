# Current point injection versus the pre-line-sink implementation

Current order-one point injection is faster than the earlier point-source
implementation on all measured cases. This comparison includes all intervening
performance changes, not just the recent point-source fast path.

## Historical reference and matched physics

The baseline is `3d4dcb7`, the first parent of the line-sink merge `55402fb`:
the last main-branch version before line sinks. The current commit is
`04ae7a7` on `point-stream-injection`; its package R code contains the point
implementation introduced by `48a3b61`.

The old API used `get_stream_segments()`, `get_adf_stream_apportionment()` and
`get_aquifer_water_level_change()`. Benchmark adapters map those names and the
old `allow_many_aquifer_parameter_sets` argument without modifying the old
engine. Its old magrittr import is supplied in its isolated source environment.
The baseline has no quadrature-order argument; current modeling explicitly
uses order 1. Both run in the same R session with the same installed dependencies.

The old stream injection diameter defaults to half the represented segment
length, giving a minimum response radius of one quarter of segment length.
Current point mode uses half `stream_width` as its minimum radius. The benchmark
therefore sets current widths to half the segment lengths, preserving the old
radius rather than silently comparing different physical regularization.
This changes only the current benchmark inputs, not any package defaults.

## Workloads and timing method

All scenarios use the same bundled input data for both versions, two pumps
with two aquifer parameter sets, and a 90-day horizon:

| Case | Segments | Observations | Evaluation times |
|---|---:|---:|---:|
| Example | 7 | 2 | 4 |
| Finer grid | 14 | 2 | 10 |
| More observations | 28 | 20 | 19 |

Each case/method/preparation mode is warmed up before five alternating
baseline/current timing pairs. There are 120 timed calls across twelve cases.
No profiler or other benchmark is run concurrently. Explicit pre-call garbage
collection is excluded; collections during a call remain included. Expected
multi-aquifer informational warnings are muffled for both implementations.

Prepared-input calls still include validation, unit processing, injection
schedule generation, direct pumping drawdown, injection recovery and output
formatting. Preparation-included calls additionally rebuild stream segments
and, for ADF, apportionment during every call.

## Prepared stream inputs

| Case | Method | Pre-line-sink seconds | Current point seconds | Speedup |
|---|---|---:|---:|---:|
| 7 segments / 2 observations | ADF | 0.142 | 0.061 | 2.3x |
| 7 segments / 2 observations | Constant head | 0.231 | 0.074 | 3.1x |
| 14 segments / 2 observations | ADF | 0.365 | 0.087 | 4.2x |
| 14 segments / 2 observations | Constant head | 0.714 | 0.114 | 6.3x |
| 28 segments / 20 observations | ADF | 8.161 | 0.126 | 64.8x |
| 28 segments / 20 observations | Constant head | 9.969 | 0.175 | 57.0x |

## Including stream preparation

| Case | Method | Pre-line-sink seconds | Current point seconds | Speedup |
|---|---|---:|---:|---:|
| 7 segments / 2 observations | ADF | 0.200 | 0.119 | 1.7x |
| 7 segments / 2 observations | Constant head | 0.277 | 0.102 | 2.7x |
| 14 segments / 2 observations | ADF | 0.420 | 0.150 | 2.8x |
| 14 segments / 2 observations | Constant head | 0.728 | 0.133 | 5.5x |
| 28 segments / 20 observations | ADF | 8.371 | 0.196 | 42.7x |
| 28 segments / 20 observations | Constant head | 9.240 | 0.197 | 46.9x |

These are separately measured medians; small inversions between preparation
modes reflect variability and do not imply that preparation reduces work.
Raw minima, maxima and IQRs are in the timing summary. These numbers differ
from earlier baselines that already included line sinks. In particular,
`9956c8c` was pre-performance-refactor but was not pre-line-sink.

## Output checks and interpretation

All twelve complete warm-up table comparisons pass at tolerance `1e-9`.
The maximum absolute numeric difference over all quantities and cases is
6.94e-17 meters, consistent with floating-point rounding. Identifiers, dates,
row order, expected row counts and finite numeric values pass checks. All
120 timed results agree with their own warmed implementation outside timing.

The cumulative gains reflect numeric-unit internals, validation ownership,
prepared geometry/events, response caching, observation batching and output
allocation improvements since the original point-source version. This study
does not isolate each contribution. The separate comparison against
`c2ed2a4` isolates the more recent removal of order-one line overhead; see
[POINT-INJECTION-RESULTS.md](POINT-INJECTION-RESULTS.md).

The ten-year / 1,000-observation / 100-segment case was not run against this
historical baseline. No runtime is claimed for that configuration here.

## Reproduce

```r
source("analysis/performance/benchmark-pre-line-sink.R")
comparison <- run_pre_line_sink_study()
```

Generated, ignored files in `analysis/profiles/pre-line-sink-study/` include
`timings.csv`, `timing-summary.csv`, `output-comparison.csv` and `session-info.txt`.
The analysis script and notes form a reproducible benchmark checkpoint; package R
sources were not modified for the experiment. Detailed implementation review
remains pending.
