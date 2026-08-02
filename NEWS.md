# LAGO 1.0.12

* Fixed the 95% confidence interval reported for the estimated outcome.
  `get_confidence_set()` prepends the recommended intervention to the
  confidence-set grid so that its interval is computed alongside the grid
  interventions, and the interval was then picked out of the result by
  position, as its first row. That row holds the recommended intervention only
  when its own interval covers the outcome goal, so whenever it did not,
  `$est_outcome_ci` reported the interval of an ordinary grid intervention
  instead. The interval is now always the one computed at the recommended
  intervention, and it is also reported when no grid intervention qualifies,
  where it used to be suppressed even though it was perfectly well defined.
* Fixed the confidence set losing one of its members. The prepended
  recommended intervention was removed from the returned set by position
  rather than by identity, so whenever the recommended intervention was not
  itself in the set, a genuine qualifying grid intervention was deleted in its
  place.
* Fixed `confidence_set_size_percentage`, which understated the size of the
  confidence set. It subtracted the prepended recommended intervention from
  both the count of qualifying interventions and the size of the grid
  unconditionally, including when that intervention had never been counted.
  The numerator and the denominator now both count grid interventions only.
* A confidence set containing exactly one grid intervention is no longer
  discarded and reported as empty with a size of 0.
* Fixed the confidence-interval bounds being attached to the wrong grid
  interventions. When the interval of a grid intervention could not be
  computed, the bounds of every later intervention in the confidence set were
  shifted relative to the coordinates they were reported against. This is
  reachable with two-way clustering, where the variance estimator is not
  guaranteed to be positive semi-definite and some intervals come out
  undefined.
* `get_confidence_set()` now returns a third field, `rec_int_ci`, the interval
  at the recommended intervention as a named `c(lower, upper)`, computed
  whether or not it covers the outcome goal, and `$cs` holds the qualifying
  grid interventions only. Its return shape has therefore changed for code
  that calls `get_confidence_set()` directly. The fields on the result of
  `lago_optimization()` keep the names and shapes they had.
* The confidence set, its size, and the estimated-outcome confidence interval
  can differ from earlier releases for configurations where the recommended
  intervention's own interval does not cover the outcome goal, or where no
  grid intervention qualifies. The values reported before were the wrong ones
  for those configurations, and the corrected values are what the fixes above
  produce. Where the recommended intervention's interval covers the goal and
  at least one grid intervention qualifies, which is the common case, every
  reported number is unchanged. The estimated-outcome interval additionally
  changes for most runs that include fixed time effects, since it was computed
  at the last period rather than the requested one, and the recommended
  intervention itself changes where a covariate's name was being read as a
  fixed effect.
* Fixed the prediction matrix in `get_confidence_set()` being paired with the
  outcome-model coefficients by position. Its columns were relabelled with the
  coefficient names rather than reordered to them, so a model whose terms were
  fitted in an order other than the order the matrix is assembled in had every
  column multiplied by the wrong coefficient, silently returning a different
  confidence set. The coefficients are now matched to the columns by name, so
  the order of the model's terms no longer matters, and a model that genuinely
  does not correspond to the predictors raises an error naming the
  coefficients and the predictors that failed to match instead of returning a
  plausible-looking answer. An additional covariate whose own name begins with
  "period" is also no longer counted as a fixed time effect, which used to make
  the prediction matrix one column too wide and fail with "non-conformable
  arguments".
* Fixed the estimated outcome being reported as 0 whenever a fixed time effect
  was included and the optimization asked for the reference period. The period
  was looked up among the model's coefficients, where the reference period has
  none, and the empty result collapsed the center-level effects to nothing.
  Periods are now resolved against the levels the model was fitted on.
* Fixed the interval for the estimated outcome being computed at the last time
  period while the estimate itself used the requested one. The two described
  different periods, far enough apart that the reported interval could exclude
  the estimate printed beside it. The interval now follows the requested
  period, so a recommendation reported for one period is no longer given
  another period's interval.
* Fixed an additional covariate or center characteristic whose name contains
  "center" being counted as a fixed center effect. The recommended
  intervention itself was corrupted through silent vector recycling, so the
  optimizer could report an intervention whose estimated outcome was not the
  one shown. Model terms are now resolved through the fitted model's own
  term-to-coefficient mapping rather than by matching names.
* Fixed a factor or character additional covariate or center characteristic
  raising an error or being paired with the wrong coefficient. The outcome
  model names such a coefficient after the factor level, not the column, so
  looking the column name up among the coefficients found nothing. A factor
  covariate is now held at its reference level, matching how numeric
  covariates are held at 0. More than one center characteristic per factor
  cannot be resolved this way, because exactly one optimization value is
  allowed per characteristic, and that combination now raises rather than
  choosing a level silently.
* Fixed the confidence set changing when the rows of `data` were reordered.
  The variance-covariance matrix built for continuous outcomes enumerated
  factor levels in order of first appearance while the model orders them by
  level, so the two were misaligned whenever a factor's levels first appeared
  in an order other than their level order, and the standard errors were then
  wrong. The matrix is now built with the model's own level order and paired
  with the prediction matrix by name. The columns of `predictors_data` may
  also be supplied in any order.
* Fixed passing more than one center characteristic, which either failed or
  silently added a single recycled column of values to the confidence set
  instead of one column per characteristic.
* Added runnable `@examples` to every exported function that lacked them:
  `get_confidence_set()` and the `print()`, `summary()`, and `plot()` methods
  for `"lago"` objects. Every exported function now ships an example.
* Added `lago_report()`, which renders a self-contained HTML report for a
  `"lago"` result. It knits a bundled R Markdown template that lays out the same
  sections as the console methods plus the confidence-set plot and a session-info
  footer. Requires the suggested `rmarkdown` and `knitr` packages.
* Restyled the `print()` and `summary()` console output for `"lago"` results
  with boxed, colour-accented `cli` sections through a shared presentation
  formatter. The non-quiet in-run summary routes through the same formatter, so
  the output shown during a run is identical to `print()` and the old
  double-render is gone. The console output shows the full picture, so results
  can be read without further calls: an inputs recap (data dimensions, outcome,
  intervention components, model family/link and fixed effects, goals, costs and
  bounds), the fitted outcome-model coefficient table, the overall
  intervention-effect test, the recommended intervention with its cost and the
  95% CI for the estimated outcome, and the confidence set (size, cost IQR, and
  first rows). `summary()` renders the same output.
* The fitted outcome model is now carried on the result as `$model`, and the
  95% confidence interval for the estimated outcome at the recommended
  intervention as `$est_outcome_ci`.
* `lago_optimization()` now returns an object of class `"lago"` with `print()`,
  `summary()`, and `plot()` methods for a readable console summary and a
  confidence-set plot. The result is still a plain list, so existing `$`-based
  access is unchanged. (#61)
* Added an "Optimizing an intervention with LAGO" vignette that walks through a
  full optimization on the bundled BetterBirth data, rendered as an Articles
  page on the documentation site. (#61)
* Added a hex logo and refreshed the README header, badges plus a title logo,
  and removed the now-redundant banner. (#56, #61)
* Added a `quiet` argument to `lago_optimization()` that skips the progress
  messages and their paced delays. Programmatic and repeated calls run
  substantially faster while returning identical results. (#60)
* `visualize_cost()` gained a "Copy to clipboard" button, a numeric cost
  summary, and aligned cost and marginal-cost axes. On close it now returns the
  assembled cost list to the R session, assigned to `lago_cost_list`. (#59)
* Expanded the reference documentation for the bundled datasets with fuller
  variable descriptions. (#58)
* Added a pkgdown documentation site published to GitHub Pages. (#53)
* Added a GPL-3 `LICENSE` file and polished the README presentation. (#56)
* Fixed a crash when a single intervention component was used. (#54)
* Added an `icc` argument to the power calculation that applies a design-effect
  adjustment, so the power goal can account for within-center clustering. (#29)
* The overall intervention-effect test result (statistic and p-value) is now
  returned in the result object rather than only printed. (#49)
* Added an automated test suite (testthat) and continuous-integration checks.
  (#45)
* `visualize_cost()` sliders now support per-component default and custom
  ranges. (#34)
* Added non-fatal outcome-model fit diagnostics that warn about a questionable
  fit while the optimization continues. (#36)
* Supplying `optimization_grid_search_step_size` now switches the optimization
  method to grid search automatically. (#32)
* `lago_optimization()` now accepts a standalone power goal, an outcome goal, or
  both together, taking the higher of the outcome goal and the power-implied
  outcome. (#40)
