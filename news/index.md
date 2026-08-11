# Changelog

## LAGO 1.1.0

- Fixed `outcome_goal_intention = "minimize"` ignoring the outcome goal.
  The minimize direction is implemented by negating the fitted
  coefficients and maximizing instead, and the result was converted back
  by negating it. For a logit link that is not the inverse of the
  transform: negating the coefficients maps a probability to one minus
  itself, not to minus itself. The estimated outcome was therefore
  reported as a negative probability, and, more seriously, the goal
  handed to the optimization was negated while the quantity it
  constrains stays between 0 and 1, so the constraint held for every
  candidate and the recommendation was driven by cost alone. On a
  constructed example the optimizer returned the do-nothing intervention
  at zero cost, with a true outcome probability of 0.94, and reported it
  as meeting a goal of 0.5 or below. An unachievable goal was also never
  detected, so the warning about shrinking towards the previous stage’s
  intervention could not appear. Recommendations, costs and confidence
  sets change for minimize runs with a logit link. Minimize with an
  identity link is unaffected, since negation was already the correct
  inverse there. This particular fix changes nothing for `maximize`,
  though the two fixes below do.
- Fixed the recommended intervention being allowed outside
  `intervention_lower_bounds` and `intervention_upper_bounds`. The
  interpolation the shrinking method performs was not confined to the
  bounds, so for an outcome goal it could not reach it returned a value
  below the lower bound, a value above the upper bound, or a value that
  was not finite: asking for an outcome of 1 returned `Inf`, and with
  lower bounds of 10 it returned a negative intervention of -23.3. Both
  are reachable in the maximize direction with no power goal, and a
  value below the lower bound is reachable on its own at ordinary goals
  such as 0.995. A recommendation is now brought onto the bounds
  supplied. The numerical optimizer can still stop a solver tolerance
  outside them, on the order of 1e-4, and that residue is projected back
  rather than reported.
- Fixed the numerical optimization returning the most expensive of its
  candidate solutions rather than the cheapest. The optimizer runs from
  several starting points and all of them satisfy the outcome goal, so
  the intended choice is the lowest-cost one. The recommendation and its
  cost change where the starting points converged to different local
  optima.
- Fixed the estimated outcome being wrong when more than one center
  characteristic was supplied. One term of the linear predictor was not
  summed, so it stayed a vector and the surrounding arithmetic recycled
  it. With a single center characteristic the term is a scalar and
  results are unchanged.
- `link = "probit"` and `link = "log"` are no longer accepted. They
  passed validation and were documented as supported, but the outcome
  calculation only ever implemented `logit` and `identity` and returned
  the linear predictor unchanged for the other two, reporting it as a
  probability or a mean. Asking for an outcome of 0.20 or below under
  `probit` returned an intervention reported as achieving exactly
  0.200000, where the probability it corresponds to is 0.579. Supporting
  either link needs inverse-link and variance paths the confidence set
  does not have, so they are rejected rather than silently wrong.
- Fixed the 95% confidence interval reported for the estimated outcome.
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  prepends the recommended intervention to the confidence-set grid so
  that its interval is computed alongside the grid interventions, and
  the interval was then picked out of the result by position, as its
  first row. That row holds the recommended intervention only when its
  own interval covers the outcome goal, so whenever it did not,
  `$est_outcome_ci` reported the interval of an ordinary grid
  intervention instead. The interval is now always the one computed at
  the recommended intervention, and it is also reported when no grid
  intervention qualifies, where it used to be suppressed even though it
  was perfectly well defined.
- Fixed the confidence set losing one of its members. The prepended
  recommended intervention was removed from the returned set by position
  rather than by identity, so whenever the recommended intervention was
  not itself in the set, a genuine qualifying grid intervention was
  deleted in its place.
- Fixed `confidence_set_size_percentage`, which understated the size of
  the confidence set. It subtracted the prepended recommended
  intervention from both the count of qualifying interventions and the
  size of the grid unconditionally, including when that intervention had
  never been counted. The numerator and the denominator now both count
  grid interventions only.
- A confidence set containing exactly one grid intervention is no longer
  discarded and reported as empty with a size of 0.
- Fixed the confidence-interval bounds being attached to the wrong grid
  interventions. When the interval of a grid intervention could not be
  computed, the bounds of every later intervention in the confidence set
  were shifted relative to the coordinates they were reported against.
  This is reachable with two-way clustering, where the variance
  estimator is not guaranteed to be positive semi-definite and some
  intervals come out undefined.
- [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  now returns a third field, `rec_int_ci`, the interval at the
  recommended intervention as a named `c(lower, upper)`, computed
  whether or not it covers the outcome goal, and `$cs` holds the
  qualifying grid interventions only. Its return shape has therefore
  changed for code that calls
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  directly. The fields on the result of
  [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  keep the names and shapes they had.
- The confidence set, its size, and the estimated-outcome confidence
  interval can differ from earlier releases for configurations where the
  recommended intervention’s own interval does not cover the outcome
  goal, or where no grid intervention qualifies. The values reported
  before were the wrong ones for those configurations, and the corrected
  values are what the fixes above produce. Where the recommended
  intervention’s interval covers the goal and at least one grid
  intervention qualifies, which is the common case, every reported
  number is unchanged. The estimated-outcome interval additionally
  changes for most runs that include fixed time effects, since it was
  computed at the last period rather than the requested one, and the
  recommended intervention itself changes where a covariate’s name was
  being read as a fixed effect.
- Fixed the prediction matrix in
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  being paired with the outcome-model coefficients by position. Its
  columns were relabelled with the coefficient names rather than
  reordered to them, so a model whose terms were fitted in an order
  other than the order the matrix is assembled in had every column
  multiplied by the wrong coefficient, silently returning a different
  confidence set. The coefficients are now matched to the columns by
  name, so the order of the model’s terms no longer matters, and a model
  that genuinely does not correspond to the predictors raises an error
  naming the coefficients and the predictors that failed to match
  instead of returning a plausible-looking answer. An additional
  covariate whose own name begins with “period” is also no longer
  counted as a fixed time effect, which used to make the prediction
  matrix one column too wide and fail with “non-conformable arguments”.
- Fixed the estimated outcome being reported as 0 whenever a fixed time
  effect was included and the optimization asked for the reference
  period. The period was looked up among the model’s coefficients, where
  the reference period has none, and the empty result collapsed the
  center-level effects to nothing. Periods are now resolved against the
  levels the model was fitted on.
- Fixed the interval for the estimated outcome being computed at the
  last time period while the estimate itself used the requested one, so
  a recommendation reported for one period was given another period’s
  interval. The interval now follows the requested period. This was
  largely masked before, because the interval was suppressed whenever no
  grid intervention qualified; now that it is always reported, an
  interval far enough from the estimate to exclude it would have become
  visible.
- Fixed an additional covariate or center characteristic whose name
  contains “center” being counted as a fixed center effect. The
  recommended intervention itself was corrupted through silent vector
  recycling, so the optimizer could report an intervention whose
  estimated outcome was not the one shown. Model terms are now resolved
  through the fitted model’s own term-to-coefficient mapping rather than
  by matching names.
- Fixed a factor or character additional covariate or center
  characteristic raising an error or being paired with the wrong
  coefficient. The outcome model names such a coefficient after the
  factor level, not the column, so looking the column name up among the
  coefficients found nothing. A factor covariate is now held at its
  reference level, matching how numeric covariates are held at 0. More
  than one center characteristic per factor cannot be resolved this way,
  because exactly one optimization value is allowed per characteristic,
  and that combination now raises rather than choosing a level silently.
- Fixed the confidence set changing when the rows of `data` were
  reordered. The variance-covariance matrix built for continuous
  outcomes enumerated factor levels in order of first appearance while
  the model orders them by level, so the two were misaligned whenever a
  factor’s levels first appeared in an order other than their level
  order, and the standard errors were then wrong. The matrix is now
  built with the model’s own level order and paired with the prediction
  matrix by name. The columns of `predictors_data` may also be supplied
  in any order.
- Fixed passing more than one center characteristic, which either failed
  or silently added a single recycled column of values to the confidence
  set instead of one column per characteristic.
- Fixed the estimated-outcome confidence interval and confidence set
  being computed on the logit scale for a binomial outcome model fitted
  with `link = "identity"`. The interval branch applied `expit()` and
  the logit delta-method factor from the outcome type alone, ignoring
  the link, so an identity-link binomial fit was reported on the wrong
  scale, with bounds that could exclude the point estimate and wrongly
  discard or populate the confidence set. The bounds are now built on
  the link the model was fitted on. Binary logit results are unchanged.
  ([\#74](https://github.com/correspondMerchant/LAGO-R-Package/issues/74))
- Fixed the confidence interval bounds for a binary outcome on the logit
  link being reported outside `[0, 1]`. The point estimate is `expit()`
  of the linear predictor and is a probability by construction, but the
  delta-method bounds are symmetric on the probability scale and could
  fall below 0 or above 1, so a set reported a lower bound of -0.106 or
  an upper bound of 1.049 for a probability. The bounds in `rec_int_ci`
  and in the confidence set are now confined to `[0, 1]`, while
  confidence-set membership is still decided from the unconfined
  interval, so an outcome goal of exactly 1 does not drop a qualifying
  intervention.
  ([\#75](https://github.com/correspondMerchant/LAGO-R-Package/issues/75))
- Fixed the numerical optimizer failing with the base-R error “argument
  is of length zero” when every restart of its
  maximum-achievable-outcome search failed. Only the cost search guarded
  against this, so an all-failed outcome search reached
  [`which.max()`](https://rdrr.io/r/base/which.min.html) over all-`NA`
  results and died before the package’s own message advising the
  `grid_search` method could appear. Both searches now raise that
  actionable message when no restart succeeds.
  ([\#73](https://github.com/correspondMerchant/LAGO-R-Package/issues/73))
- Fixed a rank-deficient outcome model being detected only after a full
  search over every intervention. When
  [`glm()`](https://rdrr.io/r/stats/glm.html) returns `NA` for a
  coefficient the optimization reads (the intercept, an intervention
  component or interaction term, a fixed center or time effect, or a
  center characteristic), the fit is now refused up front with an error
  naming the aliased terms, before any intervention is tried. Where such
  a fit is still reached, both the numerical and the grid-search paths
  raise that same error, instead of the numerical one advising
  `grid_search`, which fails on the same fit, and the grid one failing
  with “missing value where TRUE/FALSE needed”.
  ([\#74](https://github.com/correspondMerchant/LAGO-R-Package/issues/74),
  [\#76](https://github.com/correspondMerchant/LAGO-R-Package/issues/76))
- Fixed a factor, logical or ordered additional covariate or center
  characteristic whose name begins with “center” or “period” still being
  counted as a fixed center or time effect on the path a model passed
  directly to
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  can take, where the fitted model’s term-to-coefficient mapping is
  unavailable. The names held back from the fallback name search were
  column names, but a contrast-coded column’s coefficient is named after
  the level, so `center_grp` never held back `center_grpb`, which was
  then taken for a center dummy and recycled the center weights into a
  silently wrong outcome. The held-back names now come from the coding
  the model recorded, and a covariate named exactly “center” or
  “period”, whose dummies cannot be told from the genuine fixed-effect
  dummies, raises an error reporting the collision.
  ([\#73](https://github.com/correspondMerchant/LAGO-R-Package/issues/73))
- `center_weights_for_outcome_goal` must now be numeric, finite and
  non-negative at both entry points. Only their type, length and sum
  were checked before, so weights of -10 and 11 summed to 1 and were
  accepted, reporting an outcome of 10.95 for a binary outcome, and a
  missing weight surfaced as an opaque “missing value where TRUE/FALSE
  needed” error. A weight of exactly 0, which excludes that center from
  the average, is still allowed. The exported
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md),
  which does not pass through `validate_inputs()`, previously ran none
  of these checks and could report a confidence bound above 1 for a
  binary outcome.
  ([\#73](https://github.com/correspondMerchant/LAGO-R-Package/issues/73),
  [\#74](https://github.com/correspondMerchant/LAGO-R-Package/issues/74))
- Fixed the estimated outcome being scaled by center weights that did
  not sum to 1. Weights whose sum was within a thousandth of 1 passed
  validation and were multiplied into the per-center outcomes as
  supplied, so a set summing to 0.999 scaled every reported outcome,
  including the goal comparison the recommendation is chosen against.
  [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  now renormalises the weights it accepts to sum to 1, so the estimated
  outcome and the recommendation shift for accepted weights that did not
  already sum to exactly 1, and are unchanged for weights that did. The
  exported
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  instead refuses weights that do not sum to 1, since it is handed the
  weights an optimization ran with and renormalising them would move the
  interval away from the value it was computed at.
  ([\#72](https://github.com/correspondMerchant/LAGO-R-Package/issues/72),
  [\#75](https://github.com/correspondMerchant/LAGO-R-Package/issues/75))
- [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  now refuses an additional covariate whose column is entirely `NA`, in
  `validate_inputs()`, with an error naming the offending covariate(s).
  Such a column made [`glm()`](https://rdrr.io/r/stats/glm.html)’s
  internal `na.omit` drop every row, so the fit died with an opaque
  “nonempty numeric vector” error deep in model fitting that never named
  the covariate. The check catches all-`NA` numeric, factor and
  character columns, while a partially or fully observed covariate is
  untouched.
  ([\#77](https://github.com/correspondMerchant/LAGO-R-Package/issues/77))
- Added a warning when an additional covariate is dropped by
  [`glm()`](https://rdrr.io/r/stats/glm.html) as collinear. Its
  coefficient is `NA` but the optimization never reads it, so the run no
  longer stops or drops it silently: it warns naming the covariate and
  continues, returning the recommendation the fit without that covariate
  gives.
  ([\#76](https://github.com/correspondMerchant/LAGO-R-Package/issues/76))
- Added a warning from
  [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  when a binary outcome’s estimated outcome is reported outside
  `[0, 1]`, which happens when an identity-link linear probability model
  is extrapolated to an intervention beyond the range its components
  were fitted over. The estimate is not clamped, because it drives the
  optimizer and the recommendation, so the warning names the
  extrapolation and reports how many interval bounds are affected while
  every returned value is unchanged.
  ([\#75](https://github.com/correspondMerchant/LAGO-R-Package/issues/75))
- Added a warning when a numeric additional covariate whose observed
  range excludes 0 is held at 0 to compute the confidence set, naming
  each such covariate and its observed range, since the reported outcome
  and interval are then an extrapolation to a covariate value that never
  occurs in the data. The covariate is still held at 0, so no returned
  value changes.
  ([\#75](https://github.com/correspondMerchant/LAGO-R-Package/issues/75))
- The robust and clustered variance estimator for a logit outcome model
  is now computed by a compiled Rcpp kernel instead of two
  per-observation R loops. Only the accumulation of the bread and meat
  matrices moves to C++, so the matrix inversion stays in R and the
  returned variance is numerically identical to before, while the
  clustered confidence set is far faster on larger data. This is the
  package’s first compiled code, so `Rcpp` is now a dependency.
  ([\#79](https://github.com/correspondMerchant/LAGO-R-Package/issues/79))
- [`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
  now draws its total-cost and marginal-cost curves client-side with D3
  instead of as server-rendered images, so they redraw instantly as the
  sliders move. Each curve gains a hover read-out, the right endpoint of
  the total-cost curve is draggable to rescale all of a component’s
  coefficients at once (the sliders follow the drag), and the curves are
  tinted red when the coefficients are invalid. D3 is bundled with its
  license, so the app works offline.
  ([\#80](https://github.com/correspondMerchant/LAGO-R-Package/issues/80))
- Added a Python wrapper (in `python/`, importable as `lago`) that lets
  Python users call LAGO through `rpy2`. It wraps
  [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md),
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md),
  [`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
  and
  [`lago_report()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_report.md),
  converting a pandas data frame and Python lists to and from R, and
  calls the real R functions rather than reimplementing anything, so the
  results are exactly R’s. It embeds R, so R and the installed LAGO
  package are required.
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

### Known limitations

- The confidence set is the set of interventions whose confidence
  interval covers the outcome goal, which is a two sided test and does
  not depend on `outcome_goal_intention`. Under
  `outcome_goal_intention = "minimize"` it can therefore contain
  interventions whose estimated outcome is above the goal, and which
  cost more than the recommendation, because their interval still
  reaches the goal from above. Read the confidence set as the
  interventions the data cannot distinguish from the goal, not as the
  interventions that meet it. The estimated outcome and its interval,
  reported as `est_outcome_goal` and `est_outcome_ci`, are computed at
  the recommended intervention and are direction independent, so they
  are unaffected.
