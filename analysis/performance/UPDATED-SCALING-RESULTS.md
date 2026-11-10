# Scaling sweep on updated code

Measured commit: `733f34c8dd0aa744118c51501ebe57d8b491ddb1`. Package R sources
were unchanged during this study. The benchmark script and this report are
analysis files; generated measurements and plots are ignored under
`analysis/profiles/updated-scaling-study/`.

## Workload and checks

The study varies one count at a time across 31 scenarios, using ADF and constant
head with both finite lines (`quadrature_order = 4`) and points
(`quadrature_order = 0`). Five timed repetitions per combination give 620 timed
calls, in addition to 124 warm-up calls. Scenario and mode order are randomized
with a fixed seed. Every timed output is exactly identical to its warm-up output;
all warm-up outputs have the expected row counts and finite water-level changes.

Every timed call includes stream preparation, ADF apportionment where needed,
validation/unit processing, injection scheduling, aquifer drawdown/recovery and
output construction. Explicit garbage collection before each call is excluded;
garbage collection during calls is included. These are unprofiled elapsed times.

The baseline fixture has 20 observations, 28 stream segments, one reach, two
pumps sharing hydraulic parameters, and 19 evaluations over 90 days. Pumping
rates change at days 0, 30 and 60. Geometry is synthetic and straight, in a
meter-based UTM CRS. Stream length is 2,800 meters except in the growing-network
sweep. These are not ten-year monthly simulations.

## Default line mode: order 4

Five-run median full-workflow elapsed seconds:

| Count varied | Lower count | Higher count | ADF, lower → higher | Constant head, lower → higher |
|---|---:|---:|---:|---:|
| Observations | 20 | 2,000 | 0.182 → 0.444 | 0.178 → 0.444 |
| Stream segments | 28 | 448 | 0.180 → 1.510 | 0.179 → 2.146 |
| Pumps, shared aquifer | 2 | 50 | 0.182 → 2.561 | 0.183 → 2.619 |
| Pumps, distinct aquifers | 2 | 50 | 0.186 → 2.763 | 0.199 → 3.763 |
| Reach records, fixed total length/segments | 1 | 28 | 0.183 → 0.185 | 0.184 → 0.184 |
| Growing network reaches, 28 segments per reach | 1 | 8 | 0.181 → 0.756 | 0.176 → 0.730 |

Observation cost is modest in this short time grid: a 100-fold increase from 20
to 2,000 observations increases total runtime about 2.4–2.5 times. Fixed setup
cost matters at the smaller counts. This does not imply observation cost remains
small for long pumping histories: the separate ten-year profiles show response
evaluation dominating at 1,000 observations.

Pump cost grows approximately with pump count over much of the tested range.
Increasing from 20 to 50 shared-aquifer pumps raises ADF runtime from 0.910 to
2.561 seconds and constant-head runtime from 0.933 to 2.619 seconds. Distinct
aquifers cost more, especially for constant head: 50 distinct parameter sets
take 3.763 seconds versus 2.619 seconds for shared parameters. Each additional
pump also adds separately attributed output rows and event processing.

The most visible nonlinear increase is constant head at higher segment counts:
224 to 448 segments raises runtime from 0.735 to 2.146 seconds (2.9 times), while
ADF rises from 0.755 to 1.510 seconds (2.0 times). This is consistent with the
larger boundary matrices and factorizations required by constant head, although
these end-to-end timings do not isolate the factorization cost.

Reach-record count alone has little effect when total length and segment count
remain fixed. Growing the actual network adds segments and increases runtime.

## Point mode compared with lines

Five-run median full-workflow elapsed seconds at selected high counts:

| Scenario | ADF lines / points | Constant head lines / points |
|---|---:|---:|
| 2,000 observations | 0.444 / 0.278 | 0.444 / 0.265 |
| 448 segments | 1.510 / 1.403 | 2.146 / 1.108 |
| 50 pumps, shared aquifer | 2.561 / 2.546 | 2.619 / 2.598 |
| 50 pumps, distinct aquifers | 2.763 / 2.651 | 3.763 / 3.226 |

Point mode helps most here with large observation or segment counts. It barely
changes runtime in the shared-aquifer pump sweep, suggesting other per-pump
work dominates that fixture. These are different source representations;
cross-order numerical equivalence was not an acceptance condition for this
scaling study. Within each representation, every timed result matched its own
warm-up exactly.

## Limits and diagnostics

These sweeps do not measure the interaction of simultaneously large pump,
observation and segment counts. They also do not sweep schedule length, event
count, curved geometry, peak memory or quadrature orders above 4. The results
should not be multiplied together to predict a combined large workload.

The run emitted informational warnings for multiple aquifer parameter sets.
Targeted reruns at 50 distinct-aquifer pumps, 448 segments and the largest
growing network reproduced only the expected multi-aquifer message. See
`warning-audit.csv`. All runs completed successfully.

Min/max ranges and IQRs are saved with the medians. Small differences near the
fixed setup cost should not be interpreted as meaningful speedups.

## Reproduce and inspect

```r
source("analysis/performance/benchmark-updated-scaling.R")
run_updated_scaling_study()
plot_updated_scaling_study()
```

Generated artifacts are in `analysis/profiles/updated-scaling-study/`:

- `runtime-scaling.png` and `runtime-scaling.pdf`: six-panel scaling curves.
- `timing-summary.csv`: medians, ranges, IQRs and scenario counts.
- `timings.csv`: all 620 individual measurements.
- `scenarios.csv` and `output-checks.csv`: sweep design and agreement checks.
- `session-info.txt` and `current-package.patch`: code and runtime provenance.

The earlier ten-year monthly fixture remains the appropriate reference for
long-history absolute timings. A combined large-count/long-history benchmark
would be the next step for estimating a particular study's runtime.
