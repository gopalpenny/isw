# isw 0.1.0

## Numeric internal calculations

* User-facing functions validate inputs and normalize hydraulic calculations to
  numeric meters and days. Internal helpers use unit-suffixed numeric fields;
  user-facing outputs retain their documented physical units and time representations.
* Spatial objects retain `sf` geometry and CRS metadata. Analysis uses one
  meter-based UTM CRS, retaining an existing compatible stream CRS or selecting
  a local zone. Explicit `analysis_crs` values must be meter-based UTM; study
  areas may cross zone boundaries.
* Pumping events, spatial distances and response matrices are reused during
  modeling. Repeated context validation is removed from internal calculations;
  newly supplied intermediate results still receive boundary validation.
  Speed improvements will be assessed with comparable timings and profiles.

## Finite-line stream representation

* Point representation is now selected with `quadrature_order = 0` instead of
  `1`. All positive orders use Gauss--Legendre quadrature; order 1 places one
  source at each straight edge midpoint. The default remains 4.

* Setting `quadrature_order = 0` now applies each segment's entire injection
  rate at its `model_point`, bypassing line-source construction and quadrature
  integration. Point sources use `stream_width / 2` as the minimum response
  radius. This applies to constant-head schedule generation and to aquifer
  responses for either injection method. Positive orders retain finite-line
  integration.

* The default quadrature order is reduced from 16 to 4. Higher orders remain
  available through `quadrature_order`; supplied constant-head schedules still
  retain their recorded order when the modeling argument is omitted.

* Stream injection is now distributed uniformly along each stream segment using
  finite-line response calculations by default. Point injection at segment
  model points is available with `quadrature_order = 0`. The representation
  affects both constant-head injection rates and stream-induced aquifer
  water-level responses.
* Constant-head boundary conditions remain imposed at segment `model_point`
  locations at each injection interval endpoint.
* Stream segments now use `stream_width` instead of an effective injection-well
  `well_diam`. Width defaults to 1 m and regularizes near-stream responses using
  a minimum distance of half the width. Physical pumping wells continue to use
  `well_diam`.

## Function names and argument changes

* Renamed the main workflow functions to distinguish input preparation, schedule
  generation, and response modeling:

  | Previous function | Replacement | Argument changes |
  |---|---|---|
  | `get_stream_segments()` | `prep_stream_segments()` | Adds `stream_width = NULL`. |
  | `get_adf_stream_apportionment()` | `prep_adf_stream_apportionment()` | None. |
  | `get_adf_stream_depletion()` | `model_adf_stream_depletion()` | None. |
  | `get_stream_injection_schedule()` | `generate_stream_injection_schedule()` | Requires `stream_segments`; replaces `allow_many_aquifer_parameter_sets = FALSE` with `allow_n_aquifer_parameter_sets = 5`; adds `quadrature_order = 4L`. |
  | `get_aquifer_water_level_change()` | `model_aquifer_water_level_change()` | Replaces `allow_many_aquifer_parameter_sets = FALSE` with `allow_n_aquifer_parameter_sets = 5`; adds `quadrature_order = 4L`. |
  | `generate_segment_sample_points()` | `prep_stream_sample_points()` | None. |

* Reversed the user-facing `injection_rate` sign convention: positive values now
  represent water moving from the stream into the aquifer. Calculate total
  stream depletion with `sum(injection_rate)` rather than
  `-sum(injection_rate)`. The `water_level_change` sign convention remains
  unchanged: negative values indicate decline.
* In `prep_stream_segments()`, a supplied scalar `stream_width` applies to all
  reaches; otherwise reach-level widths are retained, defaulting to 1 m when
  absent. Stream-reach and stream-segment inputs no longer accept `well_diam`.
* In `generate_stream_injection_schedule()` and
  `model_aquifer_water_level_change()`, `quadrature_order` controls the number
  of Gauss--Legendre points per straight edge for positive orders; order 0
  selects one injection source per segment at its model point. The default is 4.
* The default constant-head limit changes from ten unique aquifer parameter
  sets to five. Set `allow_n_aquifer_parameter_sets = Inf` for unrestricted
  calculations, replacing the former
  `allow_many_aquifer_parameter_sets = TRUE`.
* For `method = "adf"`, `generate_stream_injection_schedule()` now requires
  stream geometry through `stream_segments`. Unlike the former
  `get_stream_injection_schedule()`, it does not accept an apportionment object
  as the third argument or recover geometry from `stream_apportionment`.
* `prep_adf_stream_apportionment()` returns a relationship tibble without
  duplicated stream geometry or segment attributes. Join results to
  `stream_segments` when spatial geometry is needed.

## Schedule reuse

* `generate_stream_injection_schedule(method = "adf")` now accepts partially
  precomputed `adf_stream_depletion`. Available ADF stream depletion rates at
  required injection times are reused, and rates at missing times are calculated
  internally with a warning.
* Generated schedules record their injection method and quadrature order. When
  a constant-head schedule is supplied to `model_aquifer_water_level_change()`,
  its quadrature order is reused unless explicitly overridden. A different
  supplied order produces a warning.

## Other API changes

* Renamed `get_usgs_stream_reaches()` to `fetch_usgs_stream_reaches()`.
* Renamed the specialized analytical functions:

  | Previous function | Replacement | Argument changes |
  |---|---|---|
  | `get_stream_depletion_fraction()` | `calc_straight_stream_depletion_fraction()` | Removes `df`; physical inputs must be supplied explicitly. |
  | `get_aquifer_drawdown_ratio()` | `calc_infinite_aquifer_drawdown_ratio()` | Removes `df`; physical inputs must be supplied explicitly. |
  | `get_straight_stream_drawdown_ratio()` | `calc_straight_stream_drawdown_ratio()` | Removes `df`; physical inputs must be supplied explicitly. |

* Removed `get_stream_reach_apportionment()`,
  `get_apportioned_stream_depletion()`, `get_apportioned_aquifer_drawdown()`,
  and `get_depletion_from_pumping()`. Use the preparation and modeling workflow
  above.
* Removed the re-exported `%>%` operator.
