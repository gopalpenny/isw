# Initial performance results — November 6, 2026

The refactor is faster in all twelve measured comparisons, and complete aquifer
outputs agree with the original implementation at tolerance `1e-9`. The next
optimization target is repeated response-cache access across observations,
especially formatting numeric parameters into cache keys. Theis evaluation is
now a small fraction of sampled runtime in these examples.

## Method

Baseline: `9956c8c`, immediately before the performance refactor. Current package:
`49d11ae`. Both versions ran in the same R session with the same dependencies,
example data, UTM geometry and inputs. The baseline R functions were loaded into
an isolated environment; the checkout was not changed. Package R code was not
modified during this study.

Each case was warmed up and its outputs compared before five alternating
baseline/current timing pairs. Times below are medians of elapsed seconds,
without a profiler. Explicit garbage collection before each call is excluded;
collections during the call are included. Hardware/session details and raw
timings, ranges and IQRs are in `analysis/profiles/performance-study/`.

All cases have two pumps, two aquifer parameter sets and a 90-day horizon:

| Case | Stream segments | Evaluation times | Observation wells |
|---|---:|---:|---:|
| Example | 7 | 4 | 2 |
| Finer grid | 14 | 10 | 2 |
| More observations | 28 | 19 | 20 |

The larger observation set is a fixed transect between the two bundled example
observations. These are controlled scaling examples, not production-wide speed
guarantees. Five measurements provide an initial comparison; small cases show
noticeable timing variability. Different cases were not timed concurrently.

## Model calls with stream segments/apportionment reused

These calls still include user-facing input validation, numeric preparation,
injection-schedule generation and aquifer modeling.

| Case | Method | Baseline seconds | Current seconds | Speedup |
|---|---|---:|---:|---:|
| Example | ADF | 0.226 | 0.070 | 3.2× |
| Example | Constant head | 0.391 | 0.090 | 4.3× |
| Finer grid | ADF | 0.651 | 0.144 | 4.5× |
| Finer grid | Constant head | 1.587 | 0.137 | 11.6× |
| More observations | ADF | 11.107 | 0.443 | 25.1× |
| More observations | Constant head | 16.067 | 0.504 | 31.9× |

## Including stream segmentation and ADF apportionment

| Case | Method | Baseline seconds | Current seconds | Speedup |
|---|---|---:|---:|---:|
| Example | ADF | 0.373 | 0.177 | 2.1× |
| Example | Constant head | 0.444 | 0.179 | 2.5× |
| Finer grid | ADF | 0.614 | 0.157 | 3.9× |
| Finer grid | Constant head | 1.431 | 0.158 | 9.1× |
| More observations | ADF | 12.912 | 0.469 | 27.5× |
| More observations | Constant head | 17.117 | 0.522 | 32.8× |

The two preparation modes were measured separately. Noise can make an individual
baseline median with preparation slightly lower than one without preparation;
that does not mean preparation reduces computation.

## Remaining bottlenecks

Four `profvis` reports were recorded separately after timing. Percentages below
are inclusive percentages of sampled profile time, which also includes some
profiler/wrapper overhead. Parent and child percentages overlap and must not be
added. They locate work, rather than establish timing speedups.

| Larger case: function | ADF | Constant head |
|---|---:|---:|
| `.get_cached_line_response_matrix()` | 53% | 52% |
| `format.default()` (mostly under cache lookup) | 37% | 34% |
| `.theis_response_at_distance()` | 1.9% | 0.8% |

The cache already avoids repeating most hydraulic matrix evaluations. A separate
untimed, instrumented call confirms the difference between requesting and
calculating a response matrix:

| Larger case: calls per model run | ADF | Constant head |
|---|---:|---:|
| Cache requests | 4,980 | 5,628 |
| Actual line-response matrix evaluations | 30 | 66 |
| Observation-specific injection response extractions | 600 | 600 |
| Direct Theis ratio calls | 600 | 642 |

Constant-head totals include stream-boundary and observation response operators.
Every cache request currently formats K, D, V and elapsed time, even on hits.
The same full observation response matrix is requested repeatedly as the code
steps through observation wells. Thus many loops remain expensive because of
lookup and indexing work, rather than the exponential-integral calculation.

For the small example, CRS parameter parsing (`CPL_crs_parameters`) accounts for
about 31–35% of sampled time. Preparation/validation and constructing small
tibbles matter more there. QR solves did not appear prominently at these segment
counts; this does not establish their cost for much larger stream networks.

## Allocation inspection

Separate `Rprofmem()` runs recorded approximately 52.7 MiB of sized allocations
for ADF and 100.6 MiB for constant head in the larger case. The returned table
is only 0.042 MiB. These are cumulative recorded allocation sizes, not peak
resident memory: small-vector heap pages without byte sizes and native-library
allocations are not included in those totals. Instrumentation for call counting
was applied afterward and its results matched the uninstrumented output.

Dense operators and response caches still matter when scaling to many more
segments, observations and distinct elapsed times. Their matrix dimensions grow
with target-by-quadrature-point distances and target-by-segment responses. The
28-segment experiment does not establish production peak-memory requirements.

## Proposed next changes, for review

1. Accumulate injection recovery for all observations together. For each pump,
   evaluation and contributing age, multiply the full response matrix by the
   corresponding segment rate-change vector. This avoids repeating cache lookup
   and event extraction for every observation, without requiring one enormous
   observation-by-event-by-time array.
2. Prepare aquifer and elapsed-time cache identifiers once, preserving numeric
   distinctions and cache ownership checks. Avoid four numeric formatting calls
   for every cache hit.
3. For small workloads, inspect repeated CRS property parsing/transforms and
   small-tibble construction. Keep sf/CRS metadata and independent user-facing
   validation; reuse prepared metadata only within the validated call.

The cache-key preparation change below was implemented after review. Observation
batching and CRS/tibble changes remain proposals.

## Follow-up: prepare cache keys before repeated lookups

Compared `49d11ae` with the working-tree cache-key change using the same twelve
cases and five alternating timing pairs. All complete outputs agree at tolerance
`1e-9`. The exact package diff and session details are retained with the results
in `analysis/profiles/key-preparation-study/`; the checkout was not switched.

Aquifer parameters are formatted once per prepared key set, and distinct response
ages are formatted before the observation/history loops. The cache still checks
operator ownership and uses the same scalar, 17-digit key representation. The
response matrices, event selection, observation loops and hydraulic calculations
are unchanged. Independent private calls retain the original formatting fallback.

| Case | Method | Preparation included | Before seconds | After seconds | Runtime reduction |
|---|---|---|---:|---:|---:|
| Example | ADF | No | 0.075 | 0.073 | 2.7% |
| Example | Constant head | No | 0.091 | 0.092 | -1.1% |
| Finer grid | ADF | No | 0.100 | 0.095 | 5.0% |
| Finer grid | Constant head | No | 0.153 | 0.130 | 15.0% |
| More observations | ADF | No | 0.425 | 0.206 | 51.5% |
| More observations | Constant head | No | 0.560 | 0.285 | 49.1% |
| Example | ADF | Yes | 0.138 | 0.137 | 0.7% |
| Example | Constant head | Yes | 0.112 | 0.114 | -1.8% |
| Finer grid | ADF | Yes | 0.164 | 0.160 | 2.4% |
| Finer grid | Constant head | Yes | 0.165 | 0.150 | 9.1% |
| More observations | ADF | Yes | 0.495 | 0.270 | 45.5% |
| More observations | Constant head | Yes | 0.569 | 0.307 | 46.0% |

The earlier 20–30% estimate was conservative for the larger observation case:
the measured saving is about half the model runtime, or approximately twice the
speed. Small-case differences are within a few milliseconds and do not establish
a meaningful gain or regression. These are five-run medians from controlled
examples, rather than a guarantee for other workloads.

Separate profiles after this change put `format.default()` at 0.3% of ADF samples
and 0.8% of constant-head samples, compared with 37% and 34% in the initial
profiles. Cache-helper inclusive shares fell to 12% and 15%, respectively.
These overlapping, sampled percentages have different denominators after the
speedup; use the unprofiled timings above to quantify the saving.

Separate call counting confirms unchanged cache requests (4,980 ADF / 5,628
constant head) and actual matrix evaluations (30 / 66). Recorded sized
allocations are 53.6 / 101.7 MiB, slightly above the initial 52.7 / 100.6 MiB;
this change improves runtime rather than reducing recorded allocation traffic.
The allocation figures retain the exclusions and non-peak interpretation above.

Validation after the change: all 645 regression expectations pass, `R CMD build`
completes, and `R CMD check` reports `Status: OK`, including examples, tests,
vignette rebuilding and the PDF manual. Precision/collision and operator-ownership
regressions cover the new prepared-key path.

### Review checkpoint

The user requested a later review of the committed cache-key changes, including
a Git diff. That review is still pending. Preserve this as a separate commit
from any observation-batching implementation so the changes can be inspected
independently. The baseline for this checkpoint is `49d11ae`; the commit includes
the cache-key code, regression coverage, benchmark scripts and analysis notes.

Preparing keys introduces a list of distinct response ages and identifiers.
Constant-head preparation enumerates interval-to-earlier-boundary ages before
deduplicating them, so its temporary age vector can grow quadratically with the
number of intervals. This study does not establish memory behavior for very
long schedules. No observation-by-event-by-time response array is introduced.

## Reproduce and inspect

See [PROFILING.md](PROFILING.md) for commands. Interactive reports and raw data
are generated, ignored artifacts under `analysis/profiles/performance-study/`:

* `example-adf.html`, `example-constant_head.html`
* `more_observations-adf.html`, `more_observations-constant_head.html`
* `timings.csv`, `timing-summary.csv`, `session-info.txt`
* `*-by.self.csv`, `*-by.total.csv`, `*.Rprof`
* `allocation-summary.csv`, `call-counts.csv`, `*-allocations.Rprofmem`
