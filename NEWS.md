# isw 0.1.0.9000

## Stream injection and water-level response

* Added `prep_stream_segments()` to separate projected stream discretization
  from ADF apportionment. Stream width is stored once per segment and defaults
  to 1 m when it is not supplied at the reach level.
* Added `generate_stream_injection_schedule()` with ADF and constant-head methods.
  It reports signed, piecewise-constant aquifer-injection rates. The
  constant-head method solves pump-specific discrete-well rates that enforce
  zero signed water-level change at stream model points at interval endpoints.
  Precomputed output from `model_adf_stream_depletion()` can be reused when it
  includes every schedule boundary.
* Added boundary-residual and response-matrix condition-number diagnostics for
  constant-head schedules. Condition numbers above `1e6` produce a warning.
* Reorganized the public API around explicit verbs: `fetch_` retrieves external
  data, `prep_` constructs static model inputs, `generate_` constructs a stream-
  injection schedule, `model_` evaluates scheduled responses, and `calc_`
  exposes specialized single-pump analytical calculations.
* Removed the former combined apportionment, apportioned-depletion,
  apportioned-drawdown, and single-pump convenience interfaces. Stream geometry
  is now supplied through `stream_segments` rather than duplicated in or
  recovered from an apportionment object.
