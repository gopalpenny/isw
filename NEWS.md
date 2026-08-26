# isw 0.1.0.9000

## Stream injection and water-level response

* Added `get_stream_segments()` to separate projected stream discretization
  from ADF apportionment. Discrete stream wells default to an effective
  diameter equal to half their represented segment length.
* Added `method = "constant_head"` to `get_stream_injection_schedule()`. The
  new method solves pump-specific discrete-well rates that enforce zero signed
  water-level change at stream model points at interval endpoints.
* Added boundary-residual and response-matrix condition-number diagnostics for
  constant-head schedules. Condition numbers above `1e6` produce a warning.
* Added `get_adf_stream_apportionment()`, `get_adf_stream_depletion()`, and
  `get_aquifer_water_level_change()` as preferred, method-specific or general
  interfaces. The existing `get_stream_reach_apportionment()`,
  `get_apportioned_stream_depletion()`, and
  `get_apportioned_aquifer_drawdown()` functions remain available for
  compatibility and are planned for deprecation in a future release.
