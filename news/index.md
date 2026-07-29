# Changelog

## LAGO 1.0.12

### New features

- [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  supports a standalone power goal, an outcome goal, or both together,
  taking the higher of the outcome goal and the power-implied outcome
  ([\#40](https://github.com/correspondMerchant/LAGO-R-Package/issues/40)).
- Added a power-calculation design-effect adjustment via the `icc`
  argument, so the power goal can account for within-center clustering
  ([\#29](https://github.com/correspondMerchant/LAGO-R-Package/issues/29)).
- Added a `quiet` argument to
  [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  that skips the progress messages and their paced delays, making
  programmatic and repeated calls substantially faster while returning
  identical results.
- [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  now returns an object of class `"lago"` with
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods. The
  result is still a list, so existing `$`-based access is unchanged.
- The overall intervention-effect test result is now returned in the
  result object rather than only printed
  ([\#49](https://github.com/correspondMerchant/LAGO-R-Package/issues/49)).
- [`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
  gained a “Copy to clipboard” button, a numeric cost summary, aligned
  cost and marginal-cost axes, and, on close, returns the cost list to
  the R session (assigned to `lago_cost_list`).
- [`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
  sliders now support per-component custom ranges
  ([\#34](https://github.com/correspondMerchant/LAGO-R-Package/issues/34)).
- Added non-fatal outcome-model fit diagnostics that warn about a
  questionable fit while the optimization continues
  ([\#36](https://github.com/correspondMerchant/LAGO-R-Package/issues/36)).
- Supplying `optimization_grid_search_step_size` now switches the
  optimization method to grid search automatically
  ([\#32](https://github.com/correspondMerchant/LAGO-R-Package/issues/32)).

### Bug fixes

- Fixed a crash when a single intervention component was used
  ([\#54](https://github.com/correspondMerchant/LAGO-R-Package/issues/54)).

### Documentation and infrastructure

- Added a pkgdown documentation site
  ([\#53](https://github.com/correspondMerchant/LAGO-R-Package/issues/53)).
- Added an automated test suite (testthat) and continuous integration
  ([\#45](https://github.com/correspondMerchant/LAGO-R-Package/issues/45)).
