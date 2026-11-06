# Performance refactor: validation and internal units

Design document for branch `performance`. The tables below inventory the 61
named, top-level functions present before the refactor; proposed helpers are
identified separately.

## Implementation status: public input boundaries

The first implementation stage adds validated numeric preparation to all ten
public entry points. Physical inputs are converted before stripping, numeric
columns carry unit suffixes, and CRS/original time and unit metadata are retained.
Analytical wrappers also define restoration of their public response units.

Per the user's requested staging, there is no compatibility adapter: existing
calculation bodies and all their internal unit checks remain in place. Public
preparation therefore intentionally breaks the paths whose internal calculations
still expect units objects and the original column names. The newly extracted
private coordinators retain the former public calculation bodies unchanged.
The next stage will migrate these internals individually, rather than bypassing
or removing their checks now.

Boundary tests use captured internal calls to verify numeric preparation without
depending on the not-yet-migrated calculations. The baseline full suite passed
before implementation. The latest boundary-only run passed 76 expectations.
After this stage, boundary tests pass; the full suite
reports 45 test blocks with errors from unmigrated paths (307 passed expectations,
zero failed expectations). These counts are a transition snapshot, not a passing
package result. Vignette explanations of completed numeric internals remain
pending until the end-to-end paths work.

## Internal contract

Public functions accept the existing user-facing inputs, including compatible
physical units and Date/time representations. They validate and normalize those
inputs before invoking internal calculations. Public wrappers call internal
calculations directly rather than calling other public functions and repeating
their preparation. There is one implementation of each calculation, with no
public `validate` flag required.

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
such as unit-bearing public inputs/outputs or coordinates whose units are
explicitly represented by their spatial object and CRS. If coordinates are
extracted into plain numeric values without that representation, use suffixes
such as `x_m` and `y_m`. Public API names remain unchanged.

### Minimal unit-contract comments

Every function must document its unit contract in the code. Keep comments brief
and avoid repeating a comment on each variable or arithmetic operation.

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
Preserve existing public output shapes, sign conventions, and documented unit
behavior; inventory exact output-unit expectations in tests before coding.
Internal rate changes and constant-head solves must retain the current signs;
the current solver negates solved rates when constructing public injection rates.

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
establishes it from the available stream extent, or an enclosing preparation
step can choose it from the combined extent. Downstream public functions reuse
the stream network's analysis CRS rather than independently selecting a new
zone when wells/observations are added. Both the active segment geometry and
the secondary `model_point` geometry must be transformed consistently. An
explicit `analysis_crs` override should follow the UTM contract; documenting
the change from arbitrary projected analysis overrides is part of migration.

Prepare spatial measurements/operators once per applicable public call and
reuse them outside time-stepping loops. Preserve `sf` for segmentation,
sampling, point-to-line distances, quadrature geometry and spatial outputs.
Numeric distance matrices such as `pump_to_stream_distance_m` and
`target_to_quadrature_distance_m` need no attached CRS. Spatial objects retain
the CRS needed to interpret their coordinates and construct public outputs.

## What protects internal functions?

1. Every exported entry point validates its own complete input contract,
   including cross-input relationships and optional arguments, through shared
   boundary validators/preparers.
2. Preparation creates private structures containing normalized numeric arrays,
   times, event indices, geometry operators, and output metadata. Nested
   calculations consume these structures, not arbitrary public input tables.
3. Each internal helper documents required fields, dimensions, signs, and
   standard units. Call sites pass prepared data; a class label alone is not
   evidence that arbitrary external data is valid.
4. Validation is scoped to the current public call. An object returned by a
   previous call can have been edited, so a later public call validates it again.
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

| Public function | Current validation location | Proposed boundary responsibility | Calculation path / protection |
|---|---|---|---|
| `calc_straight_stream_depletion_fraction` | Mostly delegated to Glover kernel's dimensional-result check; no full explicit input boundary | Validate lengths, K, D, V, positive elapsed times, and supported vector lengths; convert to numeric standard units | Call numeric `.glover_stream_depletion_fraction`; attach/return dimensionless output |
| `calc_infinite_aquifer_drawdown_ratio` | Delegated to Theis helpers; supplies default diameter | Validate distances, hydraulic properties, times, diameter, and vector lengths; supply default and normalize once | Call numeric `.theis_aquifer_drawdown_ratio`; attach response units at exit |
| `calc_straight_stream_drawdown_ratio` | Delegated to Glover/Theis helpers; supplies default diameter | Validate coordinate distances and hydraulic/time inputs, diameter and vector lengths; normalize once | Call numeric `.glover_aquifer_drawdown_ratio`; attach response units at exit |
| `prep_stream_segments` | CRS selector, discretizer, width helper | Validate reaches, analysis CRS, spacing, width/width columns, reserved columns; prepare meter geometry and numeric widths/spacing | New private segmentation coordinator calls unchecked spatial helpers; public exit restores sf/table units and CRS contract |
| `prep_stream_sample_points` | Inline geometry, identifier, length and spacing checks | Keep its specific sampling-input contract; normalize measured lengths and spacing | Extract private sampling implementation so apportionment does not call the validating public function |
| `prep_adf_stream_apportionment` | Inline well/segment checks, method and cutoff checks; public sampling call adds checks | Validate wells, segments, sampling controls and cutoff; prepare geometry/distances in meters | Extract private apportionment implementation calling private sampling; attach distance units at public exit |
| `model_adf_stream_depletion` | Delegates to `.calculate_adf_stream_depletion`, which validates | Move complete well/schedule/apportionment/evaluation checks and normalization to boundary | Call unchecked `.calculate_adf_stream_depletion`; restore output time/rate units |
| `generate_stream_injection_schedule` | Inline checks plus validation repeated in time, ADF and constant-head helpers | Validate shared inputs, method-specific options, quadrature order, optional supplied depletion and time coverage; normalize once | Extract private schedule coordinator calling numeric ADF/constant-head helpers; attach schedule units/metadata at exit |
| `model_aquifer_water_level_change` | Checks segments, then calls validating schedule generator and validating drawdown helper | Validate complete model inputs, observations, quadrature controls, and any supplied injection schedule; prepare shared geometry, hydraulic and time data once | Call private schedule coordinator if needed, then `.calculate_aquifer_water_level_change`; format output once |
| `fetch_usgs_stream_reaches` | AOI validator, inline service/options/output checks | Keep AOI/options checks; normalize buffer to meters; retain validation of newly received remote data | Query helpers receive prepared AOI/buffer; service outputs cross a new external-data boundary |

Vector-length and finite/positive-value rules must be made explicit at these
boundaries, preserving physically valid signed coordinate distances and signed
rate changes. Move existing protections before removing them from kernels; do
not assume every public function currently validates its full intended contract.

## Hydraulic kernels and line operators

| Existing internal function | Current checks / units | Proposed change | Protecting caller or contract |
|---|---|---|---|
| `prob_integral` | Numeric probability calculation | Remain numeric; dimensionless argument | Numeric Glover kernel supplies dimensionless values |
| `.glover_stream_depletion_fraction` | Unit arithmetic and dimensionless-result check | Numeric meter/day formula; remove units/result-dimensionality inspection | Prepared inputs from public straight-stream calculation or ADF model coordinator |
| `.theis_aquifer_drawdown_ratio` | Repeated dimension checks, units arithmetic, radius handling | Numeric meter/day inputs; retain radius regularization; remove input checks | Public drawdown preparers or prepared pumping-model helpers |
| `.theis_response_at_distance` | Rechecks dimensions and diffusivity; unit arithmetic | Numeric formula returning `day/m^2`; retain expint cutoff | Numeric Theis/line helpers supply effective distances and positive ages |
| `.glover_aquifer_drawdown_ratio` | Checks coordinate dimensions, then calls checked Theis twice | Numeric direct/image distances and shared numeric Theis kernel | Prepared public straight-stream drawdown inputs |
| `.get_gauss_legendre_rule` | Positive integer order validation | Generate numeric nodes/weights without checking order repeatedly | Public model boundary validates order; prepared line-element construction |
| `.line_sink_aquifer_drawdown_ratio` | Dimensional, length, finite and positivity checks; units attached to output | Numeric local geometry/quadrature and numeric output; remove input checks | Currently test/manual-only, with no production public caller: migrate those callers to explicitly prepared standard-unit fixtures; document private contract |
| `.prepare_line_elements` | Revalidates segments; geometry and width unit handling | Consume validated shared-UTM sf geometry, numeric widths and validated order; retain sf quadrature geometry/CRS and numeric weights | Private model/schedule preparation reached from public model/schedule boundaries |
| `.prepare_line_response_operator` | Class/point checks, transformation, units-bearing distances | Consume prepared target geometry/elements; geometry adapter converts distances once to numeric meters | Public model/schedule preparation validates points and creates elements; preserve CRS-aware adapter behavior |
| `.evaluate_line_response_operator` | Class, scalar, dimensional and finite/positive checks; units conversion | Numeric distances, widths, K, D, V, days; numeric response matrix; remove repeated input checks | Prepared operator and aquifer/time data from model/schedule preparation |
| `.get_cached_line_response_matrix` | Cache/operator checks; repeated units conversion and formatted keys | Prepared numeric parameters/ages and stable aquifer/time keys; numeric matrices | Private coordinator owns cache and operator; preserve cache ownership diagnostics or structurally bind cache to operator |

## Pumping, time, ADF, and injection calculations

| Existing internal function | Current checks / units | Proposed change | Protecting caller or contract |
|---|---|---|---|
| `.get_pumping_rate_changes` | Revalidates schedules; normalizes checked times; units-bearing rates | Numeric prepared times/rates; no schedule validation; reuse prepared events | Any of the three public model/schedule boundaries prepares schedule first |
| `.get_pumping_response_times` | Revalidates evaluation times; rate-change helper revalidates schedules; normalizes again | Build event/evaluation indices and numeric positive ages from prepared events/times | Public ADF/model preparation; internal callers reuse prepared events |
| `.normalize_time_inputs` | Validates time vectors and target relationships; returns days with units | Boundary normalization helper: convert checked external dates/units once to numeric days plus origin/output metadata; remove duplicate validation inside conversion | Shared boundary preparer explicitly validates times before conversion; never called inside hydraulic loops |
| `.get_stream_depletion_fraction_lookup` | Unit-bearing elapsed times, distances and hydraulic values; no top-level validator | Use numeric aquifer parameters, distances and unique elapsed days | Unchecked ADF calculation receives prepared inputs from ADF/schedule/model public boundary |
| `.calculate_adf_stream_depletion` | Checks schedules, evaluation times and apportionment; units arithmetic | Remove validators; numeric event superposition and numeric rate table | Public ADF preparer or private schedule coordinator using already-prepared data |
| `.get_stream_injection_times` | Checks evaluation/injection times; repeats time normalization | Numeric union of prepared pumping/evaluation/refinement times | Public schedule/model preparer validates time representations and relationships |
| `.get_interval_average_injection_schedule` | Re-normalizes time; uses units-bearing rate/time columns | Numeric averaging on prepared schedule boundaries | Private ADF schedule branch; supplied depletion validated/converted at entry |
| `.get_injection_rate_changes` | Units-bearing event construction | Numeric rates and days; preserve ordering and signs | Private schedule construction or validated/normalized supplied schedule |
| `.get_interval_average_injection_rate_changes` | Composes interval averaging and rate-change helpers | Compose numeric helpers; no validation or units conversion | Currently test-only; migrate test fixtures to prepared numeric tables or test via public boundary |
| `.get_metadata_quadrature_order` | Reads/validates user-visible metadata | Keep metadata interpretation at boundary; carry resolved integer into prepared data | Public model boundary handling supplied schedule metadata |
| `.resolve_schedule_quadrature_order` | Resolves explicit order vs metadata consistency | Keep relationship validation at boundary; do once | Public model boundary before line-element construction |
| `.get_response_ratios_for_injection_events` | Checks event-vector lengths; units-bearing grouping/indexing and cached responses | Use prepared numeric event ages and segment indices; remove repeated input checks | Prepared event tables created by drawdown coordinator; vector alignment established during preparation |
| `.calculate_aquifer_water_level_change` | Revalidates observations, schedules, segments, supplied/generated schedule; prepares geometry/times repeatedly | Consume prepared inputs, numeric schedule and operators; numeric superposition; output metadata handled separately | Public model preparer validates originals and supplied intermediates; private schedule branch returns trusted numeric schedule |
| `.get_aquifer_groups` | Revalidates wells; converts/serializes parameters; checks/warns group count | Group normalized numeric K/D/V once; move allowed-count input validation to boundary; retain useful group-count diagnostic | Public constant-head schedule/model preparation |
| `.get_pumping_head_at_stream` | Recomputes spatial distances; calls checked Theis with units for every event | Consume precomputed numeric pump-to-stream distances and prepared events; numeric Theis | Prepared constant-head geometry, wells and pumping events |
| `.get_constant_head_injection_schedule` | Revalidates segments/wells/schedules through helpers; units conversion during intervals/cache accesses | Numeric prepared geometry/operators/events/time grid; QR/cache/history remain internal; no repeated input validation | Private schedule coordinator reached from validated public schedule/model boundary; retain QR, conditioning and residual diagnostics |

## Spatial and service adapters

These functions are not all hydraulic kernels. Their CRS handling must remain
explicit even when numeric measurements use meters.

| Existing internal function | Current checks / units | Proposed change | Protecting caller or contract |
|---|---|---|---|
| `.select_analysis_crs` | Checks wells/reaches/observations and supplied CRS | Share one UTM-selection rule with stream selection; accept zone crossings without warning; retain selection feasibility checks | Currently only `.prepare_spatial_inputs` and direct tests/examples call it; no exported workflow calls that helper today |
| `.prepare_spatial_inputs` | Delegates validation to selector; transforms geometry; adds units-bearing default diameter | Treat as private spatial adapter; consume checked objects, produce geometry and numeric meter measurements/defaults where applicable | Currently test/example-only; migrate those callers explicitly or retire helper if unused; do not claim an existing public guard |
| `.select_stream_analysis_crs` | Validates reaches plus supplied/automatic CRS | Select shared UTM rather than automatically retaining arbitrary projected input CRSs; downstream calls reuse that selection | `prep_stream_segments` boundary |
| `.set_stream_width` | Validates width, forbidden diameter field, positivity and reach consistency; attaches units | Split raw-input width preparation from numeric assignment; calculation helper receives checked numeric meter widths | `prep_stream_segments` boundary validates override and inherited width columns |
| `.discretize_stream_reaches` | Revalidates reaches, projected CRS, spacing, reserved columns; converts units | Consume prepared projected geometry and numeric meter spacing; numeric lengths; remove repeated raw-input checks | `prep_stream_segments` boundary and spatial adapter establish geometry/spacing contract |
| `.point_along_linestring` | Coordinate interpolation helper | Use consistently scaled projected/local meter coordinates, cumulative lengths and offsets; no units attributes | Prepared discretizer/sampler; boundary geometry validator may also use it with explicitly normalized coordinates |
| `.make_usgs_stream_query_area` | Geographic/UTM preparation, latitude feasibility, unit-bearing buffer | Consume checked AOI and numeric buffer meters; retain geographic feasibility diagnostics and CRS transformations | `fetch_usgs_stream_reaches` boundary; geographic longitude/latitude remain CRS coordinates, not meter distances |
| `.fetch_3dhp_flowlines` | Optional dependency check and external service call | Retain dependency/service diagnostics; no hydraulic unit processing | Public fetch coordinator supplies prepared query polygon; validates remote result afterward |

## Boundary validators: keep, compose without repeating shared checks

These private functions intentionally inspect raw units and external structures.
They are exempt from the numeric-calculation contract. Refactor broad validators
into shared object checks plus relationship checks where necessary, so a boundary
does not validate the same object through multiple nested validators.

| Existing function | Responsibility after refactor | Called from / protection provided |
|---|---|---|
| `check_dimensionality` | Boundary dimensional check/conversion support only; remove from all numeric kernels; avoid changing global units options repeatedly | Public preparers and explicit boundary validators |
| `.validate_point_sf` | Geometry/type/CRS/coordinate checks | Well, observation and spatial boundary checks |
| `.validate_pumping_wells` | Well IDs, geometry, hydraulic values/units, optional diameter | Public apportionment, ADF, schedule and aquifer preparation |
| `.validate_observation_wells` | Observation IDs/geometry | Public aquifer preparation; direct private spatial fixtures explicitly prepare |
| `.validate_stream_reaches` | Reach IDs/geometry | Public segmentation/fetch boundaries, including newly fetched data |
| `.validate_time_vector` | Date/time-units validity, finiteness and ordering as appropriate | Boundary time preparation only |
| `.validate_pumping_schedules` | Schedule shape/IDs/rates/units/times; split relationship checks from repeated well/time checks | ADF, schedule and aquifer public preparation |
| `.validate_target_times` | Evaluation/refinement time representation and relationship to schedule; avoid rechecking schedule vector | Boundary time preparation |
| `.validate_evaluation_times` | Evaluation-specific boundary contract | ADF, schedule and aquifer public preparation |
| `.validate_injection_times` | Refinement-time boundary contract | Schedule and aquifer public preparation |
| `.validate_stream_segments` | Segment geometry, CRS, IDs, lengths, widths, collocation consistency | Sampling/apportionment/schedule/aquifer public preparation; once per relevant public call |
| `.validate_stream_depletion_apportionment` | Apportionment table, distances/weights and pump relationships; avoid rechecking wells | ADF/schedule/aquifer public preparation |
| `.validate_stream_apportionment_segments` | Apportionment-to-segment ID/geometry relationships | ADF-based schedule/aquifer public preparation |
| `.validate_apportioned_stream_depletion` | Caller-supplied depletion structure, units, pump/segment/time relationships; avoid rechecking originals | Schedule boundary when supplied depletion is used; preserve missing-time completion behavior |
| `.validate_stream_injection_schedule` | Caller-supplied schedule structure, coverage, units and relationships; avoid rechecking originals | Aquifer boundary when a schedule is supplied; generated schedules bypass this raw-input boundary |
| `.validate_usgs_stream_aoi` | Query AOI geometry and buffer validity/units | Public fetch boundary |

## Proposed new helpers and implementation order

### Package documentation

Update the vignettes alongside implementation, rather than describing the
planned numeric internals as existing behavior before the refactor is complete.

| Document | Planned addition |
|---|---|
| `vignettes/stream-depletion-and-drawdown.Rmd` (main workflow) | Briefly explain that public functions accept compatible unit-bearing inputs, validate and convert them at entry, calculate internally using meters and days, and return unit-bearing results. Users do not need to convert inputs to internal units themselves. |
| `vignettes/articles/adf-modeling-internals.Rmd` (internals) | Add a shared internal-unit and validation-contract section, explicitly covering both ADF and constant-head modeling. Include the standard-unit table, numeric naming convention, public boundary vs internal calculation responsibilities, output-unit restoration, Date handling, and the CRS-aware geometry boundary. |

Both explanations must preserve the distinction between internal numeric units
and the public output-unit contract. Use American spelling ("meters"). The
internals discussion should also explain why validation/conversion occurs once
per public call and how prepared inputs protect unchecked internal calculations.

Names below are provisional; these are responsibilities, not a second set of
numerical implementations.

| New responsibility | Shared use |
|---|---|
| Hydraulic input preparation | Public analytical functions; converts checked distances, K, D, V and times to numeric arrays |
| Model input preparation | Public ADF/schedule/aquifer functions; compose object/relationship validation and create shared numeric data and output metadata |
| Geometry/operator preparation | Spatial adapters; prepare meter measurements, quadrature elements and numeric distance operators once |
| Private stream sampling/apportionment calculations | Public prep functions and their internal composition |
| Private injection-schedule coordinator | Public schedule function and aquifer wrapper; returns a numeric schedule |
| Public output formatters | Restore unit-bearing tables/ratios, output dates and existing metadata at each public exit |

### Implementation roadmap

| Stage | Work | Status / completion criterion |
|---|---|---|
| 0. Baseline and profiling | Preserve original behavior as a reference; establish reproducible ADF/constant-head profiles | Original full suite passed before changes; profiling setup exists. Add missing regression cases as each layer migrates. |
| 1. Public boundaries | Validate external inputs, normalize/strip physical quantities with suffixes, retain output metadata; keep old internal checks | Implemented and boundary-tested. No adapter; branch intentionally has failing end-to-end paths. Audit these boundaries as later layers establish their exact contracts. |
| 2. Analytical kernels | Migrate Glover, Theis, direct/image-well and local finite-line kernels to numeric meters/days; preserve regularization and underflow logic | Next. Standalone public analytical functions work with equivalent meter/feet and day/second inputs and unchanged output-unit behavior. |
| 3. Spatial preparation | Consolidate shared UTM selection, retain sf/CRS, migrate segmentation/sampling/apportionment, prepare reusable line operators and numeric meter distances | Pending. Spatial public functions work independently; mixed CRSs and zone crossings work; secondary model-point geometry is handled explicitly. |
| 4. Time and pumping events | Numeric day/rate tables, event changes, evaluation indices, shared Date origin, aquifer groups | Pending. Helpers no longer renormalize inputs or reconstruct the same events repeatedly. |
| 5. ADF workflow | Migrate fraction lookup, event superposition, interval averaging and private schedule coordinator | Pending. Public depletion/schedule outputs retain shapes, dates, units and missing-time completion behavior. |
| 6. Constant-head workflow | Numeric response/cache keys, precomputed pump-to-stream distances, numeric history accumulation; retain QR/factorization reuse | Pending. Match baseline rates, signs and boundary residuals; retain numerical diagnostics. |
| 7. Aquifer workflow | Numeric direct pumping and injection superposition; reuse prepared geometry, events and response matrices | Pending. Full public wrapper and supplied-schedule paths work; observations no longer trigger repeated spatial preparation in time loops. |
| 8. Validation and output audit | Verify every independent public route is protected; remove repeated input checks only from migrated helpers; complete output formatting | Ongoing per stage, final audit pending. Keep checks of newly supplied data and numerical/service diagnostics. |
| 9. Documentation and package checks | Update both vignettes, internal contracts/comments, manual analyses, public-invalid-input tests and private numeric fixtures | Pending. Tests and R CMD check pass; rendered examples reflect actual behavior. |
| 10. Measure and optimize further | Repeat comparable unprofiled timings and profiles; inspect memory and larger workloads; consider further batching/chunking | Pending. Report measured gains separately from later algorithmic changes; no promised speedup or solver replacement. |

For each migration: update its direct callers and unit-suffixed argument names,
add meaningful regression coverage, remove only now-redundant internal input
checks, and verify its public outputs. Other not-yet-migrated workflows may
continue failing during the transition. Preserve the original reference rather
than changing expected results merely to make tests pass.

Coverage checks should enumerate all exports and their routes to prepared
calculations, including supplied schedules, supplied depletion, zero-event
cases, both injection methods, and independent analytical/preparation calls.
Calling a private helper with `isw:::` remains possible in R, but is outside the
public input contract; protection comes from controlled production call paths,
documented contracts and regression tests, not access enforcement.
