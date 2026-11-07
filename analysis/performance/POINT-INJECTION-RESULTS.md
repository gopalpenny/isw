# Point stream injection versus the earlier order-one path

The point representation is faster on the same ten-year monthly simulation,
with numerically identical modeled values for this straight-segment fixture.
Both implementations explicitly use `quadrature_order = 1`.

| Method | Earlier order-one path, seconds | Point injection, seconds | Time saved | Reduction |
|---|---:|---:|---:|---:|
| ADF | 8.381 | 7.538 | 0.843 s | 10.1% |
| Constant head | 9.559 | 8.262 | 1.297 s | 13.6% |

Values are medians of five unprofiled calls per implementation/method. This is
a fresh same-session comparison, rather than a comparison with the older
9.17 / 9.89 second measurements from the quadrature study. Use the fresh paired
comparison to estimate the improvement, since machine load and session state
can affect absolute times between studies.

## Versions and workload

* Baseline: `c2ed2a4`, immediately before point stream injection.
* Current: `48a3b61`, `point-stream-injection`.
* 1,000 observations, 100 straight segments on one 2,800-meter stream, and two
  pumps sharing K/D/V. Other geometry and physical inputs match SCALING-RESULTS.md.
* 121 calendar-month dates from January 2020 to January 2030: 120 intervals.
* Both pumps use the same monthly seasonal pumping rates, between 300 and
  500 m3/day. Each model call returns 242,000 rows.
* Stream segmentation and ADF apportionment are included in the timings, along
  with model validation, schedule generation, aquifer evaluation and outputs.
  R startup/package loading and explicit garbage collection before calls are
  excluded. Collections during calls remain included.

Baseline R functions are loaded from Git into an isolated environment without
changing the checkout. Both versions run in the same R session and use their
own preparation/modeling functions with the same dependencies. After warm-up,
five version pairs alternate baseline/current execution order. The methods run
sequentially, without other benchmark or profiler work running concurrently.

## Variability and output comparison

| Method | Version | Minimum seconds | Maximum seconds | IQR seconds |
|---|---|---:|---:|---:|
| ADF | Baseline | 8.264 | 10.574 | 0.133 |
| ADF | Point | 7.530 | 8.326 | 0.529 |
| Constant head | Baseline | 9.287 | 10.152 | 0.282 |
| Constant head | Point | 8.188 | 8.552 | 0.101 |

Identifiers, dates and row order match. Warm-up numeric comparisons have zero
maximum absolute difference and zero RMSE for pumping drawdown, stream recovery
and total water-level change in both methods. Full tables pass `all.equal()`
at tolerance `1e-9`. Every timed result is also compared with the warmed
baseline outside the measured interval; all twenty timed comparisons pass.
Expected row counts and finite water-level values also pass.

The earlier order-one path already has one midpoint source for each straight
edge. Here each segment is a single straight edge and its model point is the
midpoint, so the new path removes quadrature preparation/weighted aggregation
without changing the source locations. Remaining direct pumping, event/history
processing, cache access and output work limit the overall saving.

Curved/multi-edge segments are not included in this particular comparison.
Their new one-point representation can change both source counts and modeled
values; their runtime and approximation differences need a separate experiment.
No profiler or peak-memory measurement was collected for this comparison.

## Smaller bundled examples

The same commits were compared at order 1 on the earlier 90-day example
fixtures, with 15 alternating pairs per case. Both have two observations and
two pumps with distinct aquifer parameter sets (unlike the large monthly
fixture, which shares one set). The first case has seven segments and four
evaluation times; the finer case has fourteen segments and ten evaluations.
Both consist of single-edge segments. These are comparisons within each small
case, not a controlled change of segment/observation count alone from the
ten-year case.

### Prepared stream inputs

| Segments / observations / evaluation times | Method | Baseline ms | Point ms | Saved ms | Reduction |
|---|---|---:|---:|---:|---:|
| 7 / 2 / 4 | ADF | 67 | 64 | 3 | 4.5% |
| 7 / 2 / 4 | Constant head | 87 | 85 | 2 | 2.3% |
| 14 / 2 / 10 | ADF | 85 | 83 | 2 | 2.4% |
| 14 / 2 / 10 | Constant head | 115 | 107 | 8 | 7.0% |

### Including stream preparation

| Segments / observations / evaluation times | Method | Baseline ms | Point ms | Saved ms | Reduction |
|---|---|---:|---:|---:|---:|
| 7 / 2 / 4 | ADF | 130 | 127 | 3 | 2.3% |
| 7 / 2 / 4 | Constant head | 103 | 97 | 6 | 5.8% |
| 14 / 2 / 10 | ADF | 146 | 145 | 1 | 0.7% |
| 14 / 2 / 10 | Constant head | 133 | 126 | 7 | 5.3% |

The absolute gains are only one to eight milliseconds across these modes.
Percentages at this scale are sensitive to small timing fluctuations; use
the CSV ranges/IQRs rather than treating tenths of a percent as precise.
The smaller reductions are consistent with a larger share of fixed spatial
preparation, validation and output work and fewer response-matrix evaluations.
No new profiles were collected to partition those costs.

All eight warm-up complete-table comparisons pass at tolerance `1e-9`, and
all modeled quantities have zero maximum difference and zero RMSE. Identifiers,
dates, row order, expected sizes and finite numeric outputs pass checks. All
240 timed results agree with their own warmed implementation outside timing.
Expected multi-aquifer informational warnings are muffled for both versions.

Reproduce with `run_small_point_injection_study()` from the same comparison
script. Raw timings, summaries, output differences and session details are
retained in `analysis/profiles/small-point-injection-study/`.

## Reproduce and inspect

```r
source("analysis/performance/benchmark-point-injection.R")
comparison <- run_point_injection_study()
```

Generated, ignored files are in `analysis/profiles/point-injection-study/`:

* `timings.csv`: all twenty unprofiled measurements.
* `timing-summary.csv`: medians, minima, maxima and IQRs.
* `output-comparison.csv`: numeric differences and complete-table comparisons.
* `session-info.txt`: exact commits, fixture details and dependency versions.

Only the analysis helper, comparison script and documentation were changed for
this study; package R sources remain at the point-injection commit. These
analysis changes are uncommitted for review.
