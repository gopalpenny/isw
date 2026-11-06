# Performance refactor: validation and internal units

Design document for branch `performance`. The tables below inventory the 61
named, top-level functions present before the refactor; proposed helpers are
identified separately.

## Implementation status: stages 1 through 9

The first implementation stage adds validated numeric preparation to all ten
user-facing entry points. Physical inputs are converted before stripping, numeric
columns carry unit suffixes, and CRS/original time and unit metadata are retained.
Analytical wrappers also define restoration of their user-facing response units.

The user-facing boundary checkpoint was committed as `56de729`. Per the user's
requested staging, there is no compatibility adapter: user-facing preparation
intentionally breaks paths whose internal calculations still expect units
objects and the original column names. Unmigrated private coordinators retain
the former user-facing calculation bodies and their checks.

Stage 2 migrates Glover, Theis, direct/image-well, local finite-line and quadrature
kernels to standard numeric inputs. Arguments/physical intermediates have unit
suffixes, with brief numeric-contract comments. Their repeated unit/input checks
are removed; independent user-facing analytical functions retain validation and
restore the caller-facing response units. Radius regularization, expint underflow
handling and quadrature normalization remain intact. Direct private-kernel test
and manual-analysis calls are updated to the numeric contract. Calls from the
unmigrated spatial/model coordinators will be updated in their respective stages.

Boundary tests use captured internal calls to verify numeric preparation without
depending on the not-yet-migrated calculations. The baseline full suite passed
before implementation. The latest boundary-only run passed 76 expectations.
The existing standalone analytical tests, new numeric-kernel regression tests,
and boundary tests pass. Stage 2 was committed as `431bfcd`.

Stage 3 restores segmentation, sampling and ADF apportionment with shared UTM
geometry, numeric physical columns internally and unit-bearing user-facing outputs.
Existing meter-based UTM stream CRSs are retained; other input CRSs are
transformed, and explicit analysis overrides must be UTM in meters. All sfc
columns are transformed, including secondary model points. Spatial objects keep
their sf class/CRS. Distance matrices, width regularization, line quadrature,
numeric response evaluation and response caches are prepared for reuse.
Injection-event extraction was updated to consume those numeric matrices.
Scheduled-model callers will adopt the new interfaces in subsequent stages.
Stage 3 was committed as `9710f3e`.

Stage 4 normalizes external dates/time quantities to numeric day vectors at the
boundary and retains original output times/origin as metadata. Prepared numeric
schedules carry reusable pumping-event and event/evaluation-pair tables. Events
use explicit day and m3_per_day column suffixes, preserve signed changes, omit
unchanged rates, and contribute only at strictly later evaluations. Numeric
injection grids merge pumping/evaluation/refinement boundaries. Aquifer grouping
uses normalized numeric properties; its relationship checks, limits and warnings
live in the boundary preparer, and prepared groups are retained for reuse.

The final time, event, aquifer-group and boundary runs passed 136 expectations.
Stage 4 was committed as `47f3a1c`.

Stage 5 restores user-facing ADF depletion and injection-schedule workflows. Numeric
Glover fractions are batched across segments/distinct elapsed times, rates are
accumulated into preallocated segment-by-evaluation arrays, and prepared pumping
events/pairs are reused. Interval averaging and injection-rate changes operate
on numeric days and m3_per_day columns, retaining the existing sign convention.
The private coordinator no longer calls user-facing validating functions. User-facing
formatters restore original time/rate units and Date values. Supplied depletion
is reused, extra times are ignored, and missing boundaries alone are calculated;
warnings format those missing times in the caller's representation.

Existing ADF tests and 29 new workflow expectations pass. Independent comparisons
against the pre-refactor implementation match depletion and injection schedules
for Date inputs and hour-based inputs with imperial rates. The stage 5 full suite
reports 531 passed expectations, zero failed expectations and 10 error blocks
in unmigrated constant-head/aquifer workflows.
These counts are a transition snapshot, not a passing package result. Vignette
explanations of completed model internals remain pending until end-to-end paths
work. Stage 5 was committed as `8eb515a`.

Stage 6 restores constant-head injection-schedule construction using prepared
numeric wells, events, aquifer groups and day grids. Pump-to-stream distances are
converted to bare meters once; pump event tables are split once before interval
loops. Cached response matrices use day/m2, solved rates use m3/day and head
accumulation uses meters. User-facing formatting restores dates, rate units and meter
residuals. Repeated input checks and unit conversion are removed from the solver;
user-facing schedule/model boundaries protect the private entry point. QR solves,
per-aquifer/duration factorization reuse, conditioning warnings and nonfinite
rate diagnostics remain. Explicit matrix dimensions support one segment with
multiple pumps during history accumulation.

Independent comparisons match pre-refactor schedules for dates and hours,
imperial rates, unequal interval lengths, and shared/separate aquifer groups.
Regression checks reconstruct endpoint head cancellation, cover single-segment
streams, zero pumping and empty horizons, and retain numerical diagnostics.
The stage 6 full suite reports 571 passed expectations, zero failed expectations
and 7 blocks with errors (six in aquifer workflow tests and one older direct
spatial-helper fixture). The aquifer test errors also include a supplied-schedule
fixture whose interval coverage must be corrected. This is still an intentionally
incomplete package result. Stage 6 was committed as `d36088b`.

Stage 7 restores aquifer observation superposition with numeric prepared wells,
events, injection schedules and evaluation days. The private coordinator calls
the private schedule generator; no internal user-facing boundary validation repeats.
Observation line operators and pump-to-observation distances are prepared once,
event selections are reused across observations, and output head arrays are
preallocated. Numeric meter heads and numeric day times are restored to the
caller's original length units and time representation at the user-facing exit.
Supplied schedule metadata/quadrature and coverage checks remain at that boundary.

Independent comparisons match the pre-refactor results for both injection methods,
dates/hours, imperial rates, shared/separate aquifer groups and generated/supplied
schedules. Added regressions cover mixed spatial CRSs, feet/hour inputs, Date/hour
outputs, collocated observations with well-diameter regularization and zero events.
Two older fixtures now use the proper coverage horizon and user-facing spatial entry.
The full stage 7 suite passes: 170 test blocks, 619 expectations, zero failures
and zero errors. This does not yet include R CMD check or vignette execution.
Stage 7 and the restored explanatory comments were committed as `231c2e8`.

Stage 8 audits all ten exported entry points. User-facing boundaries now check each
context object once; table-specific validators assume that related wells,
schedules and segments have already been checked. Numeric conversion helpers
trust those dimension checks, and time normalization trusts the checked time
axis/targets. Aquifer grouping consumes the already prepared numeric wells while
retaining boundary checks for group IDs and limits. Input checks remain for
newly supplied depletion/injection data, including coverage and metadata.

The audit also rejects nonfinite Date values, NULL model observations, invalid
optional residual columns, dimensioned quadrature controls, empty segment
geometries and zero-length sampling geometries. Empty supplied schedules still
require appropriate physical column types. Unit checking uses an explicit
conversion mode without changing the caller's global units options.

Call-count regressions cover seven user-facing routes and confirm one validation per
relevant context object. Prepared aquifer coordinators run with boundary
validators disabled in tests. All 638 expectations in 174 blocks pass, and the
independent pre-refactor comparisons still match both injection methods across
dates/hours, rates in imperial units, aquifer groups and supplied schedules.
Vignette/manual migration and package build checks are the next stage.
The validation/output audit was committed as `1c7cc03`.

The approved naming review preserves domain meaning in prepared boundary objects
(`prepared_pumping_wells`, `prepared_stream_segments`, `prepared_model_times`,
`prepared_stream_apportionment`) and in ADF event/fraction/rate calculations.
Pump-specific apportionment tables are named `pump_apportionment`, not `segments`.
Distance names identify stream or observation targets; observation operators,
injection caches and event row selections identify their roles. The private well
preparer is `.prepare_numeric_pumping_wells`. User-facing names, arguments, output
columns, unit suffixes and numerical calculations are unchanged.

Stage 9 updates the main vignette with a brief internal-units explanation and
the internals article with numeric preparation, event/grid/response interfaces,
UTM rules, Date origins and user-facing output restoration. Its reconstructed numeric
ADF injection schedule is checked column by column against user-facing output.
The finite-line manual report uses current operator fields and unit-suffixed
numeric reference/solve values. The segmentation-sensitivity analysis now uses
current user-facing function names and describes finite-line width regularization.
Plot rate columns use the consistent `m3_per_day` suffix. Geometry helper comments,
NEWS and the profiling guide reflect the new contracts.

Both vignettes, the finite-line manual report and both current scenario analyses
render successfully. The profiling helper runs for ADF and constant-head with
input preparation enabled and disabled. Source build and full R CMD check,
including tests, examples, vignette rebuilding and the PDF manual, pass; the
installed-package suite reports 638 expectations with no failures or warnings.
These checks do not measure speedup.

Stage 10 now has an initial reproducible timing/profile study, documented in
`PERFORMANCE-RESULTS.md`. Five alternating timing pairs per case compare the
original `9956c8c` implementation with package code at `49d11ae`; all twelve
model-output comparisons agree. Across the controlled cases, median speedups
range from 2.1x to 32.8x. Four separate current-code profiles and untimed
allocation/call-count inspections identify repeated response-cache key formatting
and observation-specific extraction as the next targets. No further package
optimization has been applied; proposed batching/cache changes await review.

### Boundary validation ownership after the audit

| Input/responsibility | Owner | Trusted downstream helpers |
|---|---|---|
| Pumping wells: geometry, IDs, K/D/V and optional diameter | `.prepare_numeric_pumping_wells` invokes `.validate_pumping_wells` once | Related schedule/apportionment validators and hydraulic kernels do not revalidate wells |
| Pumping schedule: columns, rate units/values and time axis | Each user-facing model/schedule wrapper invokes `.validate_pumping_schedules` once | `.prepare_numeric_schedule`, target-time checks and supplied-result validators trust the schedule |
| Full stream-segment contract | `.prepare_numeric_segments` invokes `.validate_stream_segments` once | Apportionment, supplied-schedule validators and line operators trust segments |
| Sampling-only segment contract | `prep_stream_sample_points` invokes `.validate_stream_sampling_inputs` | Numeric sampling trusts geometry, lengths and spacing; this route does not require model-point/width fields |
| Evaluation and refinement times | `.prepare_numeric_model_times` validates targets against the checked schedule | `.normalize_time_inputs`, time grids and event pairing do not revalidate times |
| Apportionment rows, distances, fractions and correspondence | User-facing ADF routes invoke `.validate_stream_depletion_apportionment`; schedule/model routes also check segment correspondence | Numeric apportionment conversion and supplied-depletion validation trust that context |
| Newly supplied depletion/injection tables | `.prepare_numeric_depletion` / `.prepare_numeric_injection` validate new data, identifiers, units, times and coverage | Numeric event helpers consume the resulting trusted schedule |
| Aquifer groups and quadrature controls/metadata | User-facing constant-head/model preparation using prepared wells | QR, line elements and response caches receive checked controls; numerical diagnostics remain active |
| Standalone analytical inputs | `.prepare_analytical_inputs` validates original values/dimensions | Numeric conversion and analytical kernels trust that contract; user-facing exits restore response units |
| Service AOI and downloaded geometry | `fetch_usgs_stream_reaches` validates the query input and returned reaches | Service adapters retain sf/CRS behavior |

## Internal contract

Use "user-facing" for exported functions and their contracts, and "internal" or "private" for implementation helpers.

User-facing functions accept the existing user-facing inputs, including compatible
physical units and Date/time representations. They validate and normalize those
inputs before invoking internal calculations. User-facing wrappers call internal
calculations directly rather than calling other user-facing functions and repeating
their preparation. There is one implementation of each calculation, with no
user-facing `validate` flag required.

| Quantity | Numeric internal convention |
|---|---|
| Length, distance, thickness, width, diameter, head | meters (`m`) |
| Time, elapsed time | days (`day`) |
| Hydraulic conductivity K | `m/day` |
| Pumping, injection, depletion rates | `m^3/day` |
| Transmissivity K D | `m^2/day` |
| Diffusivity K D / V | `m^2/day` |
| Response: head / volumetric rate | `day/m^2` |
| Porosity V, depletion fractions, weights | dimensionless |

### Naming numeric quantities

When physical units are otherwise implicit, append the internal unit to numeric
variable names, function arguments, list fields and table columns. Use consistent
singular suffixes: `_m`, `_day`, `_m_per_day`, `_m2_per_day`, `_m3_per_day`,
and `_day_per_m2`. Examples include `distance_m`, `elapsed_time_day`,
`K_m_per_day`, `diffusivity_m2_per_day`, `injection_rate_m3_per_day`, and
`response_matrix_day_per_m2`. For matrices and arrays the suffix describes the
units of their entries. Dimensionless quantities and index vectors need no
unit suffix.

Do not duplicate units in names when they are already explicitly represented,
such as unit-bearing user-facing inputs/outputs or coordinates whose units are
explicitly represented by their spatial object and CRS. If coordinates are
extracted into plain numeric values without that representation, use suffixes
such as `x_m` and `y_m`. User-facing API names remain unchanged.

### Minimal unit-contract comments

Every function must document its unit contract in the code. Keep comments brief
and avoid repeating a comment on each variable or arithmetic operation.
Preserve existing explanations of algorithms, matrix dimensions, event alignment
and sign conventions, updating them for the new interfaces. Move comments with
their responsibilities when validation or unit handling moves to a boundary;
remove comments only when their subject no longer applies or is redundant.

- At boundary validation, identify the block checking physical dimensions.
- At conversion/stripping, state that inputs are normalized to package-standard
  units before unit attributes are removed. One comment can cover the block.
- At output formatting, identify the block restoring units and, where relevant,
  caller-facing time representation.
- Near the top of each numeric-only function body, state that it consumes plain
  numeric inputs rather than `units` objects, and identify the applicable input
  and output units. Use specific derived units when meters/days alone would be
  ambiguous. State that inputs are already validated where applicable.
- For dimensionless/index-only helpers, a brief top-of-body comment identifying
  dimensionless values or indices satisfies the contract. Spatial helpers
  retaining CRS/unit metadata should briefly identify that representation.

Examples (wording should match each function's actual responsibility):

```r
# Validate physical dimensions before normalization.
# Normalize to meters/days and strip unit attributes for numeric calculations.
# Restore output units and caller-facing times.

# Already-validated plain numeric inputs: distances in m, K in m/day,
# thickness in m, elapsed time in day; returns numeric responses in day/m^2.
```

Use one appropriate comment per responsibility; do not add all of these to
every function. Unit suffixes remain the primary local cue, with comments
making each function's contract and boundary transitions explicit.

Convert units BEFORE stripping attributes. Retain original output-time vectors,
time origin, identifiers, CRS information, and output-unit metadata separately.
Preserve existing user-facing output shapes, sign conventions, and documented unit
behavior; inventory exact output-unit expectations in tests before coding.
Internal rate changes and constant-head solves must retain the current signs;
the current solver negates solved rates when constructing user-facing injection rates.

Spatial helpers retain `sf` geometry and its CRS throughout preparation and
internal spatial calculations. Do not strip CRS metadata or replace bent lines
with a custom numeric geometry representation. Numeric internals refers to
hydraulic calculations, not every private function. `sf` operations may produce
unit-bearing measurements; convert these to numeric meters before passing them
to hydraulic kernels.

Accept valid user input CRSs, including projected CRSs using feet, and transform
participating objects to one shared UTM analysis CRS. Zone-boundary crossings
alone produce neither an error nor a warning. A single exact CRS is required,
not just matching zone numbers; the datum and hemisphere must agree too. Retain
existing checks for unsupported automatic-selection extents such as polar or
antimeridian cases. Geographic coordinates used in selection/service queries
are not meter coordinates.

Choose the analysis CRS once for a prepared workflow. Segment preparation
retains an existing meter-based UTM CRS or selects one from the stream extent;
an enclosing preparation
step can choose it from the combined extent. Downstream user-facing functions reuse
the stream network's analysis CRS rather than independently selecting a new
zone when wells/observations are added. Both the active segment geometry and
the secondary `model_point` geometry must be transformed consistently. An
explicit `analysis_crs` override should follow the UTM contract; documenting
the change from arbitrary projected analysis overrides is part of migration.

Prepare spatial measurements/operators once per applicable user-facing call and
reuse them outside time-stepping loops. Preserve `sf` for segmentation,
sampling, point-to-line distances, quadrature geometry and spatial outputs.
Numeric distance matrices such as `pump_to_stream_distance_m` and
`target_to_quadrature_distance_m` need no attached CRS. Spatial objects retain
the CRS needed to interpret their coordinates and construct user-facing outputs.

## What protects internal functions?

1. Every exported entry point validates its own complete input contract,
   including cross-input relationships and optional arguments, through shared
   boundary validators/preparers.
2. Preparation creates private structures containing normalized numeric arrays,
   times, event indices, geometry operators, and output metadata. Nested
   calculations consume these structures, not arbitrary user-facing input tables.
3. Each internal helper documents required fields, dimensions, signs, and
   standard units. Call sites pass prepared data; a class label alone is not
   evidence that arbitrary external data is valid.
4. Validation is scoped to the current user-facing call. An object returned by a
   previous call can have been edited, so a later user-facing call validates it again.
   A wrapper can reuse prepared data within its own call.
5. New caller-supplied intermediates (injection schedules or precalculated ADF
   depletion) are validated at entry. Internally generated equivalents do not
   trigger revalidation of original inputs. Their algorithmic invariants remain
   covered by tests and numerical diagnostics.

"No internal validation" applies to calculation helpers, not to the private
functions whose explicit purpose is boundary validation. Keep solver failure,
rank/conditioning, nonfinite-result, boundary-residual, external-service, and
cache-ownership diagnostics where they are needed. Remove repeated checks of
unchanged user inputs. Mathematical branches such as zero-event handling and
the expint underflow cutoff remain calculations, not redundant validation.

## Exported functions: validation remains mandatory

| User-facing function | Current validation location | Proposed boundary responsibility | Calculation path / protection |
|---|---|---|---|
| `calc_straight_stream_depletion_fraction` | Mostly delegated to Glover kernel's dimensional-result check; no full explicit input boundary | Validate lengths, K, D, V, positive elapsed times, and supported vector lengths; convert to numeric standard units | Call numeric `.glover_stream_depletion_fraction`; attach/return dimensionless output |
| `calc_infinite_aquifer_drawdown_ratio` | Delegated to Theis helpers; supplies default diameter | Validate distances, hydraulic properties, times, diameter, and vector lengths; supply default and normalize once | Call numeric `.theis_aquifer_drawdown_ratio`; attach response units at exit |
| `calc_straight_stream_drawdown_ratio` | Delegated to Glover/Theis helpers; supplies default diameter | Validate coordinate distances and hydraulic/time inputs, diameter and vector lengths; normalize once | Call numeric `.glover_aquifer_drawdown_ratio`; attach response units at exit |
| `prep_stream_segments` | CRS selector, discretizer, width helper | Validate reaches, analysis CRS, spacing, width/width columns, reserved columns; prepare meter geometry and numeric widths/spacing | New private segmentation coordinator calls unchecked spatial helpers; user-facing exit restores sf/table units and CRS contract |
| `prep_stream_sample_points` | Inline geometry, identifier, length and spacing checks | Keep its specific sampling-input contract; normalize measured lengths and spacing | Extract private sampling implementation so apportionment does not call the validating user-facing function |
| `prep_adf_stream_apportionment` | Inline well/segment checks, method and cutoff checks; user-facing sampling call adds checks | Validate wells, segments, sampling controls and cutoff; prepare geometry/distances in meters | Extract private apportionment implementation calling private sampling; attach distance units at user-facing exit |
| `model_adf_stream_depletion` | Delegates to `.calculate_adf_stream_depletion`, which validates | Move complete well/schedule/apportionment/evaluation checks and normalization to boundary | Call unchecked `.calculate_adf_stream_depletion`; restore output time/rate units |
| `generate_stream_injection_schedule` | Inline checks plus validation repeated in time, ADF and constant-head helpers | Validate shared inputs, method-specific options, quadrature order, optional supplied depletion and time coverage; normalize once | Extract private schedule coordinator calling numeric ADF/constant-head helpers; attach schedule units/metadata at exit |
| `model_aquifer_water_level_change` | Checks segments, then calls validating schedule generator and validating drawdown helper | Validate complete model inputs, observations, quadrature controls, and any supplied injection schedule; prepare shared geometry, hydraulic and time data once | Call private schedule coordinator if needed, then `.calculate_aquifer_water_level_change`; format output once |
| `fetch_usgs_stream_reaches` | AOI validator, inline service/options/output checks | Keep AOI/options checks; normalize buffer to meters; retain validation of newly received remote data | Query helpers receive prepared AOI/buffer; service outputs cross a new external-data boundary |

Vector-length and finite/positive-value rules must be made explicit at these
boundaries, preserving physically valid signed coordinate distances and signed
rate changes. Move existing protections before removing them from kernels; do
not assume every user-facing function currently validates its full intended contract.

## Hydraulic kernels and line operators

| Existing internal function | Current checks / units | Proposed change | Protecting caller or contract |
|---|---|---|---|
| `prob_integral` | Numeric probability calculation | Remain numeric; dimensionless argument | Numeric Glover kernel supplies dimensionless values |
| `.glover_stream_depletion_fraction` | Unit arithmetic and dimensionless-result check | Numeric meter/day formula; remove units/result-dimensionality inspection | Prepared inputs from user-facing straight-stream calculation or ADF model coordinator |
| `.theis_aquifer_drawdown_ratio` | Repeated dimension checks, units arithmetic, radius handling | Numeric meter/day inputs; retain radius regularization; remove input checks | User-facing drawdown preparers or prepared pumping-model helpers |
| `.theis_response_at_distance` | Rechecks dimensions and diffusivity; unit arithmetic | Numeric formula returning `day/m^2`; retain expint cutoff | Numeric Theis/line helpers supply effective distances and positive ages |
| `.glover_aquifer_drawdown_ratio` | Checks coordinate dimensions, then calls checked Theis twice | Numeric direct/image distances and shared numeric Theis kernel | Prepared user-facing straight-stream drawdown inputs |
| `.get_gauss_legendre_rule` | Positive integer order validation | Generate numeric nodes/weights without checking order repeatedly | User-facing model boundary validates order; prepared line-element construction |
| `.line_sink_aquifer_drawdown_ratio` | Dimensional, length, finite and positivity checks; units attached to output | Numeric local geometry/quadrature and numeric output; remove input checks | Currently test/manual-only, with no production user-facing caller: migrate those callers to explicitly prepared standard-unit fixtures; document private contract |
| `.prepare_line_elements` | Revalidates segments; geometry and width unit handling | Consume validated shared-UTM sf geometry, numeric widths and validated order; retain sf quadrature geometry/CRS and numeric weights | Private model/schedule preparation reached from user-facing model/schedule boundaries |
| `.prepare_line_response_operator` | Class/point checks, transformation, units-bearing distances | Consume prepared target geometry/elements; geometry adapter converts distances once to numeric meters | User-facing model/schedule preparation validates points and creates elements; preserve CRS-aware adapter behavior |
| `.evaluate_line_response_operator` | Class, scalar, dimensional and finite/positive checks; units conversion | Numeric distances, widths, K, D, V, days; numeric response matrix; remove repeated input checks | Prepared operator and aquifer/time data from model/schedule preparation |
| `.get_cached_line_response_matrix` | Cache/operator checks; repeated units conversion and formatted keys | Prepared numeric parameters/ages and stable aquifer/time keys; numeric matrices | Private coordinator owns cache and operator; preserve cache ownership diagnostics or structurally bind cache to operator |

## Pumping, time, ADF, and injection calculations

| Existing internal function | Current checks / units | Proposed change | Protecting caller or contract |
|---|---|---|---|
| `.get_pumping_rate_changes` | Revalidates schedules; normalizes checked times; units-bearing rates | Numeric prepared times/rates; no schedule validation; reuse prepared events | Any of the three user-facing model/schedule boundaries prepares schedule first |
| `.get_pumping_response_times` | Revalidates evaluation times; rate-change helper revalidates schedules; normalizes again | Build event/evaluation indices and numeric positive ages from prepared events/times | User-facing ADF/model preparation; internal callers reuse prepared events |
| `.normalize_time_inputs` | Validates time vectors and target relationships; returns days with units | Boundary normalization helper: convert checked external dates/units once to numeric days plus origin/output metadata; remove duplicate validation inside conversion | Shared boundary preparer explicitly validates times before conversion; never called inside hydraulic loops |
| `.get_stream_depletion_fraction_lookup` | Unit-bearing elapsed times, distances and hydraulic values; no top-level validator | Use numeric aquifer parameters, distances and unique elapsed days | Unchecked ADF calculation receives prepared inputs from ADF/schedule/model user-facing boundary |
| `.calculate_adf_stream_depletion` | Checks schedules, evaluation times and apportionment; units arithmetic | Remove validators; numeric event superposition and numeric rate table | User-facing ADF preparer or private schedule coordinator using already-prepared data |
| `.get_stream_injection_times` | Checks evaluation/injection times; repeats time normalization | Numeric union of prepared pumping/evaluation/refinement times | User-facing schedule/model preparer validates time representations and relationships |
| `.get_interval_average_injection_schedule` | Re-normalizes time; uses units-bearing rate/time columns | Numeric averaging on prepared schedule boundaries | Private ADF schedule branch; supplied depletion validated/converted at entry |
| `.get_injection_rate_changes` | Units-bearing event construction | Numeric rates and days; preserve ordering and signs | Private schedule construction or validated/normalized supplied schedule |
| `.get_interval_average_injection_rate_changes` | Composes interval averaging and rate-change helpers | Compose numeric helpers; no validation or units conversion | Currently test-only; migrate test fixtures to prepared numeric tables or test via user-facing boundary |
| `.get_metadata_quadrature_order` | Reads/validates user-visible metadata | Keep metadata interpretation at boundary; carry resolved integer into prepared data | User-facing model boundary handling supplied schedule metadata |
| `.resolve_schedule_quadrature_order` | Resolves explicit order vs metadata consistency | Keep relationship validation at boundary; do once | User-facing model boundary before line-element construction |
| `.get_response_ratios_for_injection_events` | Checks event-vector lengths; units-bearing grouping/indexing and cached responses | Use prepared numeric event ages and segment indices; remove repeated input checks | Prepared event tables created by drawdown coordinator; vector alignment established during preparation |
| `.calculate_aquifer_water_level_change` | Revalidates observations, schedules, segments, supplied/generated schedule; prepares geometry/times repeatedly | Consume prepared inputs, numeric schedule and operators; numeric superposition; output metadata handled separately | User-facing model preparer validates originals and supplied intermediates; private schedule branch returns trusted numeric schedule |
| `.get_aquifer_groups` | Revalidates wells; converts/serializes parameters; checks/warns group count | Group normalized numeric K/D/V once; move allowed-count input validation to boundary; retain useful group-count diagnostic | User-facing constant-head schedule/model preparation |
| `.get_pumping_head_at_stream` | Recomputes spatial distances; calls checked Theis with units for every event | Consume precomputed numeric pump-to-stream distances and prepared events; numeric Theis | Prepared constant-head geometry, wells and pumping events |
| `.get_constant_head_injection_schedule` | Revalidates segments/wells/schedules through helpers; units conversion during intervals/cache accesses | Numeric prepared geometry/operators/events/time grid; QR/cache/history remain internal; no repeated input validation | Private schedule coordinator reached from validated user-facing schedule/model boundary; retain QR, conditioning and residual diagnostics |

## Spatial and service adapters

These functions are not all hydraulic kernels. Their CRS handling must remain
explicit even when numeric measurements use meters.

| Existing internal function | Current checks / units | Proposed change | Protecting caller or contract |
|---|---|---|---|
| `.select_analysis_crs` | Checks wells/reaches/observations and supplied CRS | Share one UTM-selection rule with stream selection; accept zone crossings without warning; retain selection feasibility checks | Currently only `.prepare_spatial_inputs` and direct tests/examples call it; no exported workflow calls that helper today |
| `.prepare_spatial_inputs` | Delegates validation to selector; transforms geometry; adds units-bearing default diameter | Treat as private spatial adapter; consume checked objects, produce geometry and numeric meter measurements/defaults where applicable | Currently test/example-only; migrate those callers explicitly or retire helper if unused; do not claim an existing user-facing guard |
| `.select_stream_analysis_crs` | Validates reaches plus supplied/automatic CRS | Select shared UTM rather than automatically retaining arbitrary projected input CRSs; downstream calls reuse that selection | `prep_stream_segments` boundary |
| `.set_stream_width` | Validates width, forbidden diameter field, positivity and reach consistency; attaches units | Split raw-input width preparation from numeric assignment; calculation helper receives checked numeric meter widths | `prep_stream_segments` boundary validates override and inherited width columns |
| `.discretize_stream_reaches` | Revalidates reaches, projected CRS, spacing, reserved columns; converts units | Consume prepared projected geometry and numeric meter spacing; numeric lengths; remove repeated raw-input checks | `prep_stream_segments` boundary and spatial adapter establish geometry/spacing contract |
| `.point_along_linestring` | Coordinate interpolation helper | Use consistently scaled projected/local meter coordinates, cumulative lengths and offsets; no units attributes | Prepared discretizer/sampler; boundary geometry validator may also use it with explicitly normalized coordinates |
| `.make_usgs_stream_query_area` | Geographic/UTM preparation, latitude feasibility, unit-bearing buffer | Consume checked AOI and numeric buffer meters; retain geographic feasibility diagnostics and CRS transformations | `fetch_usgs_stream_reaches` boundary; geographic longitude/latitude remain CRS coordinates, not meter distances |
| `.fetch_3dhp_flowlines` | Optional dependency check and external service call | Retain dependency/service diagnostics; no hydraulic unit processing | User-facing fetch coordinator supplies prepared query polygon; validates remote result afterward |

## Boundary validators: keep, compose without repeating shared checks

These private functions intentionally inspect raw units and external structures.
They are exempt from the numeric-calculation contract. Refactor broad validators
into shared object checks plus relationship checks where necessary, so a boundary
does not validate the same object through multiple nested validators.

| Existing function | Responsibility after refactor | Called from / protection provided |
|---|---|---|
| `check_dimensionality` | Boundary dimensional check/conversion support only; remove from all numeric kernels; avoid changing global units options repeatedly | User-facing preparers and explicit boundary validators |
| `.validate_point_sf` | Geometry/type/CRS/coordinate checks | Well, observation and spatial boundary checks |
| `.validate_pumping_wells` | Well IDs, geometry, hydraulic values/units, optional diameter | User-facing apportionment, ADF, schedule and aquifer preparation |
| `.validate_observation_wells` | Observation IDs/geometry | User-facing aquifer preparation; direct private spatial fixtures explicitly prepare |
| `.validate_stream_reaches` | Reach IDs/geometry | User-facing segmentation/fetch boundaries, including newly fetched data |
| `.validate_time_vector` | Date/time-units validity, finiteness and ordering as appropriate | Boundary time preparation only |
| `.validate_pumping_schedules` | Schedule shape/IDs/rates/units/times; split relationship checks from repeated well/time checks | ADF, schedule and aquifer user-facing preparation |
| `.validate_target_times` | Evaluation/refinement time representation and relationship to schedule; avoid rechecking schedule vector | Boundary time preparation |
| `.validate_evaluation_times` | Evaluation-specific boundary contract | ADF, schedule and aquifer user-facing preparation |
| `.validate_injection_times` | Refinement-time boundary contract | Schedule and aquifer user-facing preparation |
| `.validate_stream_segments` | Segment geometry, CRS, IDs, lengths, widths, collocation consistency | Sampling/apportionment/schedule/aquifer user-facing preparation; once per relevant user-facing call |
| `.validate_stream_depletion_apportionment` | Apportionment table, distances/weights and pump relationships; avoid rechecking wells | ADF/schedule/aquifer user-facing preparation |
| `.validate_stream_apportionment_segments` | Apportionment-to-segment ID/geometry relationships | ADF-based schedule/aquifer user-facing preparation |
| `.validate_apportioned_stream_depletion` | Caller-supplied depletion structure, units, pump/segment/time relationships; avoid rechecking originals | Schedule boundary when supplied depletion is used; preserve missing-time completion behavior |
| `.validate_stream_injection_schedule` | Caller-supplied schedule structure, coverage, units and relationships; avoid rechecking originals | Aquifer boundary when a schedule is supplied; generated schedules bypass this raw-input boundary |
| `.validate_usgs_stream_aoi` | Query AOI geometry and buffer validity/units | User-facing fetch boundary |

## Proposed new helpers and implementation order

### Package documentation

Update the vignettes alongside implementation, rather than describing the
planned numeric internals as existing behavior before the refactor is complete.

| Document | Planned addition |
|---|---|
| `vignettes/stream-depletion-and-drawdown.Rmd` (main workflow) | Briefly explain that user-facing functions accept compatible unit-bearing inputs, validate and convert them at entry, calculate internally using meters and days, and return unit-bearing results. Users do not need to convert inputs to internal units themselves. |
| `vignettes/articles/adf-modeling-internals.Rmd` (internals) | Add a shared internal-unit and validation-contract section, explicitly covering both ADF and constant-head modeling. Include the standard-unit table, numeric naming convention, user-facing boundary vs internal calculation responsibilities, output-unit restoration, Date handling, and the CRS-aware geometry boundary. |

Both explanations must preserve the distinction between internal numeric units
and the user-facing output-unit contract. Use American spelling ("meters"). The
internals discussion should also explain why validation/conversion occurs once
per user-facing call and how prepared inputs protect unchecked internal calculations.

Names below are provisional; these are responsibilities, not a second set of
numerical implementations.

| New responsibility | Shared use |
|---|---|
| Hydraulic input preparation | User-facing analytical functions; converts checked distances, K, D, V and times to numeric arrays |
| Model input preparation | User-facing ADF/schedule/aquifer functions; compose object/relationship validation and create shared numeric data and output metadata |
| Geometry/operator preparation | Spatial adapters; prepare meter measurements, quadrature elements and numeric distance operators once |
| Private stream sampling/apportionment calculations | User-facing prep functions and their internal composition |
| Private injection-schedule coordinator | User-facing schedule function and aquifer wrapper; returns a numeric schedule |
| User-facing output formatters | Restore unit-bearing tables/ratios, output dates and existing metadata at each user-facing exit |

### Implementation roadmap

| Stage | Work | Status / completion criterion |
|---|---|---|
| 0. Baseline and profiling | Preserve original behavior as a reference; establish reproducible ADF/constant-head profiles | Original full suite passed before changes; profiling setup exists. Add missing regression cases as each layer migrates. |
| 1. User-facing boundaries | Validate external inputs, normalize/strip physical quantities with suffixes, retain output metadata | Implemented and tested; final validation ownership audited in stage 8. User-facing paths work end to end. |
| 2. Analytical kernels | Migrate Glover, Theis, direct/image-well and local finite-line kernels to numeric meters/days; preserve regularization and underflow logic | Implemented and tested. Standalone user-facing analytical functions work with equivalent input units and unchanged output-unit behavior. |
| 3. Spatial preparation | Consolidate shared UTM selection, retain sf/CRS, migrate segmentation/sampling/apportionment, prepare reusable line operators and numeric meter distances | Implemented and tested. Spatial user-facing functions work independently; mixed CRSs and zone crossings work; secondary model-point geometry is transformed explicitly. Numeric cache/extraction interfaces migrated alongside operators. |
| 4. Time and pumping events | Numeric day/rate tables, event changes, evaluation indices, shared Date origin, aquifer groups | Implemented and tested. Prepared schedules retain reusable events/pairs; grouping uses normalized properties with boundary diagnostics. |
| 5. ADF workflow | Migrate fraction lookup, event superposition, interval averaging and private schedule coordinator | Implemented and tested. User-facing depletion/schedule outputs retain shapes, dates, units and missing-time completion behavior; match the pre-refactor implementation. |
| 6. Constant-head workflow | Numeric response/cache keys, precomputed pump-to-stream distances, numeric history accumulation; retain QR/factorization reuse | Implemented and tested. Matches baseline schedules; geometry and event preparation stay outside interval loops. Numeric units, QR reuse and solver diagnostics retained. |
| 7. Aquifer workflow | Numeric direct pumping and injection superposition; reuse prepared geometry, events and response matrices | Implemented and tested. Both injection methods and supplied schedules match baseline results; geometry/operators and event selections are reused, and user-facing output units/times are restored. |
| 8. Validation and output audit | Verify every independent user-facing route is protected; remove repeated input checks only from migrated helpers; complete output formatting | Implemented and tested. User-facing routes validate context once; new supplied data and numerical/service diagnostics retain their checks. |
| 9. Documentation and package checks | Update both vignettes, internal contracts/comments, manual analyses, user-facing-invalid-input tests and private numeric fixtures | Implemented and verified. Current workflow documents render, profiling helper works, and full R CMD check passes. |
| 10. Measure and optimize further | Repeat comparable unprofiled timings and profiles; inspect memory and larger workloads; consider further batching/chunking | Initial measurements and cache-key comparison complete (49–52% lower larger-case model runtime). Observation batching is implemented and output-equivalent; separate five-pair medians show another 15–21% reduction in the larger model-only cases. See PERFORMANCE-RESULTS.md for workload limits and profile details. |

For each migration: update its direct callers and unit-suffixed argument names,
add meaningful regression coverage, remove only now-redundant internal input
checks, and verify its user-facing outputs. Other not-yet-migrated workflows may
continue failing during the transition. Preserve the original reference rather
than changing expected results merely to make tests pass.

Coverage checks should enumerate all exports and their routes to prepared
calculations, including supplied schedules, supplied depletion, zero-event
cases, both injection methods, and independent analytical/preparation calls.
Calling a private helper with `isw:::` remains possible in R, but is outside the
user-facing input contract; protection comes from controlled production call paths,
documented contracts and regression tests, not access enforcement.
