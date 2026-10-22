# isw 0.1.0.9000

## Stream injection and water-level response

* Clarified internal time terminology: user-requested `evaluation_times` and
  public `evaluation_time` outputs are unchanged, while the combined injection
  grid now uses `schedule_boundaries`, `boundary_days`, and an internal
  `schedule_boundary` column. The general time-normalization helper now uses
  `target_times` terminology.
* Exported the existing stream-sampling helper as `prep_stream_sample_points()`
  for spatial inspection and water-level evaluation along stream segments.
* Added `prep_stream_segments()` to separate projected stream discretization
  from ADF apportionment. Stream width is stored once per segment and defaults
  to 1 m when it is not supplied at the reach level.
* Added `generate_stream_injection_schedule()` with ADF and constant-head methods.
  It reports piecewise-constant injection rates that are positive when water
  moves from a stream to the aquifer. The
  constant-head method solves pump-specific finite-line injection rates that enforce
  zero signed water-level change at stream model points at interval endpoints.
  Precomputed output from `model_adf_stream_depletion()` can be reused when it
  includes every schedule boundary.
* Added boundary-residual and response-matrix condition-number diagnostics for
  constant-head schedules. Condition numbers above `1e6` produce a warning.
* Replaced the Boolean `allow_many_aquifer_parameter_sets` argument with the
  numeric `allow_n_aquifer_parameter_sets`. Constant-head calculations allow
  up to five unique aquifer parameter sets by default; callers can set a
  different positive whole-number limit or use `Inf`.
* Reorganized the public API around explicit verbs: `fetch_` retrieves external
  data, `prep_` constructs static model inputs, `generate_` constructs a stream-
  injection schedule, `model_` evaluates scheduled responses, and `calc_`
  exposes specialized single-pump analytical calculations.
* Removed the legacy `df` convenience argument from the three `calc_`
  functions. Their physical inputs must now be supplied explicitly.
* Removed the former combined apportionment, apportioned-depletion,
  apportioned-drawdown, and single-pump convenience interfaces. Stream geometry
  is now supplied through `stream_segments` rather than duplicated in or
  recovered from an apportionment object.
