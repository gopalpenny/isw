# Runtime scaling — November 6, 2026

This completed study uses the current observation-batched
working tree, after the cache-key checkpoint `b4b10cc`. It measures current-code
scaling rather than another before/after optimization comparison.

## Method and scope

Each case is warmed up before five unprofiled measurements per method and
preparation mode. Scenario order and method/mode order within each repetition
are seeded and shuffled. Explicit garbage collection before a call is excluded;
collections during a call count. Scenarios are not run concurrently.

The default fixture has 20 observation wells, two pumps sharing K/D/V, one
2,800-meter straight stream divided into 28 segments, and 19 evaluation times
from day 0 to day 90. K is 1.5 m/day, D is 100 m, V is 0.1 and stream width is
10 meters. Pumps and observations span fixed transects near the stream in UTM
zone 15N. All pumps change rates together: 400 m3/day at day 0, 250 m3/day at
day 30, then zero at day 60. Total pumping increases when pumps are added.

| Sweep | Levels | Other dimensions |
|---|---|---|
| Observation wells | 1, 5, 20, 100, 500 | Default fixture |
| Reach records | 1, 2, 4, 7, 14, 28 | Same total stream length and 28 segments |
| Stream segments | 7, 14, 28, 56, 112, 224 | Same total stream length and one reach |
| Pumps, shared aquifer | 1, 2, 5, 10, 20 | One common K/D/V set |
| Pumps, distinct aquifers | 1, 2, 5, 10, 20 | Each pump has a distinct K, with D/V fixed |
| Growing stream network | 1, 2, 4, 8 reaches | Each added parallel reach adds 2,800 meters and 28 segments |

The reach-record sweep subdivides the same line and isolates reach grouping
overhead at fixed segment count. The growing-network sweep increases both
reaches and segments, approximating the computational consequence of adding
stream coverage. It uses parallel straight streams, not a realistic branched
network. Counts alone do not capture geometry complexity or conditioning.

Both ADF and constant head are measured in two modes: reuse prepared stream
segments/apportionment, and include that preparation in every call. Both modes
still include user-facing model validation, internal numeric preparation,
injection schedule generation, aquifer evaluation and output formatting.
Equivalent finite outputs and expected output row counts are checked between
the two modes before timing. There is no expected output equality across
different workload sizes.

Unlike the earlier example-based benchmark, most cases use one shared aquifer
parameter set. Compare numbers within this study rather than interpreting
differences from earlier tables as optimization gains.

## Measured results

All 31 scenarios completed: 620 timed calls, plus warm-up and preparation-mode
output checks. Every check passed. Tables show median elapsed seconds; raw
min/max/IQR values are retained in the CSV summaries.

The clearest patterns are:

* Observations: 20 to 500 wells (25 times as many) raised model-only runtime
  from 0.158 to 0.597 seconds for ADF and 0.221 to 0.646 seconds for constant
  head. There is substantial fixed work, so total runtime does not multiply
  directly by observation count over this range.
* Segments: 28 to 224 raised ADF from 0.146 to 0.752 seconds and constant head
  from 0.196 to 1.561 seconds. The last doubling, 112 to 224, increased ADF
  about 1.9 times and constant head about 2.7 times.
* Reach records alone: no systematic runtime increase at fixed stream length
  and 28 segments. This does not imply new stream coverage is free.
* Pumps: runtime grows strongly with pump count. At 20 pumps, shared-aquifer
  times were 0.986 / 1.057 seconds (ADF / constant head); distinct-aquifer
  times were 1.155 / 1.758 seconds. Constant head was about 66% slower with
  distinct parameters, consistent with reduced response/factorization reuse.
* Growing the network from one to eight reaches also grows segments from
  28 to 224: ADF increased from 0.143 to 0.757 seconds, and constant head
  from 0.195 to 1.676 seconds.

The cached observation response matrices grow with observations times segments.
The constant-head boundary response matrix has segments times segments entries;
its dense QR work can grow more steeply as segments increase. This explains
why segment growth can become more costly for constant head, but these curves
do not establish a universal exponent or predict much larger networks.

![Runtime scaling curves](../profiles/scaling-study/runtime-scaling.png)

The PNG and PDF curves include five-run minima/maxima as vertical ranges.
Small differences and nonmonotonic points should not be interpreted as speedups.
Preparation modes were timed separately, so noise can put an inclusive median
slightly below a model-only median. Do not subtract them to estimate preparation
cost precisely.

### Observation wells

| Count | ADF: prepared inputs | Constant head: prepared inputs | ADF: including preparation | Constant head: including preparation |
|---:|---:|---:|---:|---:|
| 1 | 0.128 | 0.199 | 0.209 | 0.210 |
| 5 | 0.133 | 0.180 | 0.190 | 0.205 |
| 20 | 0.158 | 0.221 | 0.229 | 0.235 |
| 100 | 0.213 | 0.258 | 0.275 | 0.280 |
| 500 | 0.597 | 0.646 | 0.626 | 0.663 |

### Reach records, fixed at 28 segments

| Count | ADF: prepared inputs | Constant head: prepared inputs | ADF: including preparation | Constant head: including preparation |
|---:|---:|---:|---:|---:|
| 1 | 0.166 | 0.220 | 0.232 | 0.238 |
| 2 | 0.145 | 0.197 | 0.207 | 0.216 |
| 4 | 0.157 | 0.219 | 0.231 | 0.238 |
| 7 | 0.147 | 0.197 | 0.208 | 0.217 |
| 14 | 0.146 | 0.198 | 0.208 | 0.212 |
| 28 | 0.149 | 0.194 | 0.208 | 0.217 |

### Stream segments, fixed total length

| Count | ADF: prepared inputs | Constant head: prepared inputs | ADF: including preparation | Constant head: including preparation |
|---:|---:|---:|---:|---:|
| 7 | 0.086 | 0.143 | 0.139 | 0.157 |
| 14 | 0.107 | 0.155 | 0.164 | 0.174 |
| 28 | 0.146 | 0.196 | 0.210 | 0.209 |
| 56 | 0.227 | 0.285 | 0.296 | 0.308 |
| 112 | 0.393 | 0.568 | 0.477 | 0.560 |
| 224 | 0.752 | 1.561 | 0.864 | 1.701 |

### Pumping wells sharing one aquifer

| Count | ADF: prepared inputs | Constant head: prepared inputs | ADF: including preparation | Constant head: including preparation |
|---:|---:|---:|---:|---:|
| 1 | 0.104 | 0.154 | 0.170 | 0.171 |
| 2 | 0.173 | 0.218 | 0.231 | 0.238 |
| 5 | 0.267 | 0.335 | 0.325 | 0.353 |
| 10 | 0.492 | 0.557 | 0.548 | 0.571 |
| 20 | 0.986 | 1.057 | 1.035 | 1.067 |

### Pumping wells with distinct aquifer parameters

| Count | ADF: prepared inputs | Constant head: prepared inputs | ADF: including preparation | Constant head: including preparation |
|---:|---:|---:|---:|---:|
| 1 | 0.103 | 0.151 | 0.166 | 0.168 |
| 2 | 0.155 | 0.228 | 0.217 | 0.248 |
| 5 | 0.306 | 0.475 | 0.373 | 0.527 |
| 10 | 0.644 | 0.981 | 0.733 | 0.978 |
| 20 | 1.155 | 1.758 | 1.238 | 1.770 |

### Growing network: reaches and segments together

| Count | ADF: prepared inputs | Constant head: prepared inputs | ADF: including preparation | Constant head: including preparation |
|---:|---:|---:|---:|---:|
| 1 reaches / 28 segments | 0.143 | 0.195 | 0.206 | 0.213 |
| 2 reaches / 56 segments | 0.227 | 0.285 | 0.293 | 0.309 |
| 4 reaches / 112 segments | 0.397 | 0.547 | 0.488 | 0.573 |
| 8 reaches / 224 segments | 0.757 | 1.676 | 0.868 | 1.663 |

The distinct-aquifer constant-head cases emit the expected informational
warning about using pump-specific aquifer parameter sets. The initial sweep
reported 48 warnings, matching the four multi-aquifer levels and their twelve
constant-head warm-up/timed calls each. Warning generation remains inside the
user-facing timing path. The network-growth run completed without warnings.

## Ten-year monthly follow-up: 1,000 observations and 100 segments

Measured the requested larger case directly, using two pumps sharing one
aquifer parameter set and the same synthetic geometry/properties. Evaluation
dates run monthly from January 2020 to January 2030: 120 intervals and 121
evaluation nodes. Each call returns 242,000 rows. Stream segmentation and ADF
apportionment preparation are included; R startup/package loading are excluded.

| Pumping schedule | ADF median (range), seconds | Constant head median (range), seconds |
|---|---:|---:|
| Three changes: 400, 250, then 0 m3/day at the first three monthly dates | 36.1 (35.4–36.3) | 40.7 (40.0–41.8) |
| Monthly changes: seasonal rates between 300 and 500 m3/day | 36.6 (36.3–37.2) | 40.8 (39.7–41.3) |

Each case has three unprofiled timed repetitions following warm-up. All twelve
timed outputs agree with their warm-up result at tolerance `1e-9`; expected
row counts and finite values also pass. Both pumps use synchronized schedules.
This supports an estimate of about 40 seconds per complete model call for this
specific two-pump setup. Different pump counts, aquifer groups and geometry
can change the result; extrapolating the earlier short-run timings was avoided.

Actual calendar months have unequal lengths. This grid has 486 distinct positive
interval-end-to-earlier-boundary ages, versus 120 on a uniform 30-day grid.
Thus a nominally similar number of steps can require substantially different
response-cache work. This is an age-count calculation, not a separately timed
30-day-grid comparison. No profile was collected for this larger case.

See `benchmark-monthly.R`, [PROFILING.md](PROFILING.md) and the generated
`analysis/profiles/monthly-study/` raw timings, summary and session information.

## Quadrature orders 1 and 4 on the ten-year case

Repeated the monthly-changing-pumping case at orders 1 and 4, for both methods:
1,000 observations, 100 straight stream segments, two pumps sharing K/D/V,
121 calendar-monthly dates and stream preparation included. Each lower-order
case has three unprofiled timed repetitions after warm-up. Finite values,
242,000 output rows and repeatability against warm-up pass for every case.

| Quadrature order | ADF median (range), seconds | Constant head median (range), seconds |
|---|---:|---:|
| 1 | 9.17 (9.05–10.17) | 9.89 (9.61–9.99) |
| 4 | 15.46 (15.15–22.91) | 15.64 (15.28–17.96) |
| 16, earlier monthly study | 36.55 (36.29–37.15) | 40.83 (39.66–41.30) |

Order-16 timings are the earlier measurements, not interleaved measurements in
this follow-up. An additional untimed order-16 run for each method supplies
the numerical reference. This is not a convergence study establishing true error.

| Method | Order | Maximum absolute water-level difference from order 16 | RMSE |
|---|---:|---:|---:|
| ADF | 1 | 0.0302 mm | 0.0104 mm |
| ADF | 4 | About 1.4e-12 mm | About 1.5e-13 mm |
| Constant head | 1 | 2.448 mm | 0.867 mm |
| Constant head | 4 | 0.740 mm | 0.271 mm |

Stream-recovery absolute differences are the same because direct pumping
drawdown is unchanged. For constant head, the maximum differences are about
0.344% / 0.104% of the reference peak absolute water-level change for orders
1 / 4; these are not pointwise relative errors. The observation transect is
500 meters from the straight stream and segments are 28 meters long. Other
geometries and closer observations can have different quadrature sensitivity.
Constant-head quadrature also affects the collocation matrix and solved
injection schedule, so its differences need not match those for ADF.

Lower orders reduce response-evaluation work, while event processing, cache
access, direct pumping and output construction remain. Therefore total runtime
does not fall in direct proportion to integration-point count. Following review,
the package default was changed from 16 to 4; these historical benchmark fixtures
keep their explicit orders. See `benchmark-quadrature.R`, [PROFILING.md](PROFILING.md)
and `analysis/profiles/quadrature-study/` for raw timings and difference summaries.

## Reproduction and limitations

See [PROFILING.md](PROFILING.md) and `benchmark-scaling.R`. Initial five sweeps
are in `analysis/profiles/scaling-study/`; the supplemental network-growth sweep
is in `analysis/profiles/network-growth-study/`. Both retain raw timings,
summary medians/ranges/IQRs and session information. Curves combine the two sets.

The recorded runs can be reproduced separately without changing package code:

```r
source("analysis/performance/benchmark-scaling.R")
run_scaling_study(sweeps = c("observations", "reaches", "segments",
                            "pumps_shared", "pumps_distinct"))
run_scaling_study(sweeps = "network_reaches",
                  output_directory = "analysis/profiles/network-growth-study")
plot_scaling_study(c("analysis/profiles/scaling-study",
                      "analysis/profiles/network-growth-study"))
```

Results describe this machine, synthetic geometry and synchronized schedules.
More time intervals, staggered pump changes, more distinct response ages,
complex stream geometry or very different aquifer properties may change the
scaling. These timings do not measure peak memory and are not production
capacity guarantees. The implementation and analysis are being committed for the
user's later detailed diff review, which remains pending. The checkpoint baseline
is `b4b10cc`; the earlier cache-key commit has its own pending review noted in
PERFORMANCE-RESULTS.md.

## Planned order-one point representation (separate chat)

The user plans to make `quadrature_order = 1` use one point per stream segment,
bypassing line quadrature preparation and aggregation. This is not implemented
in this checkpoint. Current order one already uses one midpoint per straight
edge, so it is equivalent to one source point only for a straight, single-edge
segment. A multi-edge segment currently has multiple length-weighted sources.
Choose the point location explicitly (the existing `model_point` is the natural
candidate), preserve total segment rates and the `stream_width / 2` minimum
radius, and retain the sf geometry needed by spatial/apportionment operations.
Constant-head point mode still needs coupled segment responses, history
superposition and a matrix solve. Preserve schedule metadata order reuse and
operator-bound cache checks. Additional speedup should be measured rather than
assumed from eliminating the line-helper calls.
