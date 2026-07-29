# LAGO 1.0.12

## New features

* `lago_optimization()` supports a standalone power goal, an outcome goal, or
  both together, taking the higher of the outcome goal and the power-implied
  outcome (#40).
* Added a power-calculation design-effect adjustment via the `icc` argument, so
  the power goal can account for within-center clustering (#29).
* Added a `quiet` argument to `lago_optimization()` that skips the progress
  messages and their paced delays, making programmatic and repeated calls
  substantially faster while returning identical results.
* `lago_optimization()` now returns an object of class `"lago"` with
  `print()`, `summary()`, and `plot()` methods. The result is still a list, so
  existing `$`-based access is unchanged.
* The overall intervention-effect test result is now returned in the result
  object rather than only printed (#49).
* `visualize_cost()` gained a "Copy to clipboard" button, a numeric cost
  summary, aligned cost and marginal-cost axes, and, on close, returns the cost
  list to the R session (assigned to `lago_cost_list`).
* `visualize_cost()` sliders now support per-component custom ranges (#34).
* Added non-fatal outcome-model fit diagnostics that warn about a questionable
  fit while the optimization continues (#36).
* Supplying `optimization_grid_search_step_size` now switches the optimization
  method to grid search automatically (#32).

## Bug fixes

* Fixed a crash when a single intervention component was used (#54).

## Documentation and infrastructure

* Added a pkgdown documentation site (#53).
* Added an automated test suite (testthat) and continuous integration (#45).
