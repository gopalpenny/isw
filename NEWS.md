# isw 0.1.0

## Finite-line stream representation

* Stream injection is now distributed uniformly along each stream segment using
  finite-line response calculations, replacing injection at discrete segment
  model points. This changes both constant-head injection rates and
  stream-induced aquifer water-level responses.
* Constant-head boundary conditions remain imposed at segment `model_point`
  locations at each injection interval endpoint.
* Stream segments now use `stream_width` instead of an effective injection-well
  `well_diam`. Width defaults to 1 m and regularizes near-stream responses using
  a minimum distance of half the width. Physical pumping wells continue to use
  `well_diam`.

## Breaking API changes

* Renamed the main workflow functions to distinguish input preparation, schedule
  generation, and response modeling:

  | Previous function | Replacement | Argument changes |
  |---|---|---|
  | `get_stream_segments()` | `prep_stream_segments()` | Adds `stream_width = NULL`. |
  | `get_adf_stream_apportionment()` | `prep_adf_stream_apportionment()` | None. |
  | `get_adf_stream_depletion()` | `model_adf_stream_depletion()` | None. |
  | `get_stream_injection_schedule()` | `generate_stream_injection_schedule()` | Requires `stream_segments`; replaces `allow_many_aquifer_parameter_sets = FALSE` with `allow_n_aquifer_parameter_sets = 5`; adds `quadrature_order = 16L`. |
  | `get_aquifer_water_level_change()` | `model_aquifer_water_level_change()` | Replaces `allow_many_aquifer_parameter_sets = FALSE` with `allow_n_aquifer_parameter_sets = 5`; adds `quadrature_order = 16L`. |
  | `generate_segment_sample_points()` | `prep_stream_sample_points()` | None. |

* Reversed the public `injection_rate` sign convention: positive values now
  represent water moving from the stream into the aquifer. Calculate total
  stream depletion with `sum(injection_rate)` rather than
  `-sum(injection_rate)`. The `water_level_change` sign convention remains
  unchanged: negative values indicate decline.
* In `prep_stream_segments()`, a supplied scalar `stream_width` applies to all
  reaches; otherwise reach-level widths are retained, defaulting to 1 m when
  absent. Stream-reach and stream-segment inputs no longer accept `well_diam`.
* In `generate_stream_injection_schedule()` and
  `model_aquifer_water_level_change()`, `quadrature_order` controls the number
  of Gauss--Legendre points per straight edge of each stream segment. The
  default is 16.
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
  precomputed `adf_stream_depletion`. Available boundary values are reused, and
  missing times are calculated internally with a warning.
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
