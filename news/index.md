# Changelog

## LAGO 1.0.12

- Added runnable `@examples` to every exported function that lacked
  them:
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  and the [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  `"lago"` objects. Every exported function now ships an example.
- Added
  [`lago_report()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_report.md),
  which renders a self-contained HTML report for a `"lago"` result. It
  knits a bundled R Markdown template that lays out the same sections as
  the console methods plus the confidence-set plot and a session-info
  footer. Requires the suggested `rmarkdown` and `knitr` packages.
- Restyled the [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) console output for
  `"lago"` results with boxed, colour-accented `cli` sections through a
  shared presentation formatter. The non-quiet in-run summary routes
  through the same formatter, so the output shown during a run is
  identical to [`print()`](https://rdrr.io/r/base/print.html) and the
  old double-render is gone. The console output shows the full picture,
  so results can be read without further calls: an inputs recap (data
  dimensions, outcome, intervention components, model family/link and
  fixed effects, goals, costs and bounds), the fitted outcome-model
  coefficient table, the overall intervention-effect test, the
  recommended intervention with its cost and the 95% CI for the
  estimated outcome, and the confidence set (size, cost IQR, and first
  rows). [`summary()`](https://rdrr.io/r/base/summary.html) renders the
  same output.
- The fitted outcome model is now carried on the result as `$model`, and
  the 95% confidence interval for the estimated outcome at the
  recommended intervention as `$est_outcome_ci`.
- [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  now returns an object of class `"lago"` with
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for a
  readable console summary and a confidence-set plot. The result is
  still a plain list, so existing `$`-based access is unchanged.
  ([\#61](https://github.com/correspondMerchant/LAGO-R-Package/issues/61))
- Added an “Optimizing an intervention with LAGO” vignette that walks
  through a full optimization on the bundled BetterBirth data, rendered
  as an Articles page on the documentation site.
  ([\#61](https://github.com/correspondMerchant/LAGO-R-Package/issues/61))
- Added a hex logo and refreshed the README header, badges plus a title
  logo, and removed the now-redundant banner.
  ([\#56](https://github.com/correspondMerchant/LAGO-R-Package/issues/56),
  [\#61](https://github.com/correspondMerchant/LAGO-R-Package/issues/61))
- Added a `quiet` argument to
  [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  that skips the progress messages and their paced delays. Programmatic
  and repeated calls run substantially faster while returning identical
  results.
  ([\#60](https://github.com/correspondMerchant/LAGO-R-Package/issues/60))
- [`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
  gained a “Copy to clipboard” button, a numeric cost summary, and
  aligned cost and marginal-cost axes. On close it now returns the
  assembled cost list to the R session, assigned to `lago_cost_list`.
  ([\#59](https://github.com/correspondMerchant/LAGO-R-Package/issues/59))
- Expanded the reference documentation for the bundled datasets with
  fuller variable descriptions.
  ([\#58](https://github.com/correspondMerchant/LAGO-R-Package/issues/58))
- Added a pkgdown documentation site published to GitHub Pages.
  ([\#53](https://github.com/correspondMerchant/LAGO-R-Package/issues/53))
- Added a GPL-3 `LICENSE` file and polished the README presentation.
  ([\#56](https://github.com/correspondMerchant/LAGO-R-Package/issues/56))
- Fixed a crash when a single intervention component was used.
  ([\#54](https://github.com/correspondMerchant/LAGO-R-Package/issues/54))
- Added an `icc` argument to the power calculation that applies a
  design-effect adjustment, so the power goal can account for
  within-center clustering.
  ([\#29](https://github.com/correspondMerchant/LAGO-R-Package/issues/29))
- The overall intervention-effect test result (statistic and p-value) is
  now returned in the result object rather than only printed.
  ([\#49](https://github.com/correspondMerchant/LAGO-R-Package/issues/49))
- Added an automated test suite (testthat) and continuous-integration
  checks.
  ([\#45](https://github.com/correspondMerchant/LAGO-R-Package/issues/45))
- [`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
  sliders now support per-component default and custom ranges.
  ([\#34](https://github.com/correspondMerchant/LAGO-R-Package/issues/34))
- Added non-fatal outcome-model fit diagnostics that warn about a
  questionable fit while the optimization continues.
  ([\#36](https://github.com/correspondMerchant/LAGO-R-Package/issues/36))
- Supplying `optimization_grid_search_step_size` now switches the
  optimization method to grid search automatically.
  ([\#32](https://github.com/correspondMerchant/LAGO-R-Package/issues/32))
- [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  now accepts a standalone power goal, an outcome goal, or both
  together, taking the higher of the outcome goal and the power-implied
  outcome.
  ([\#40](https://github.com/correspondMerchant/LAGO-R-Package/issues/40))
