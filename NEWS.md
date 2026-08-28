# LAGO 1.1.0

* Added a live in-browser demo to the documentation site (`live-demo.html`) that runs the real package client-side with webR (R compiled to WebAssembly), so anyone can try `lago_optimization()` with no installation. A GitHub Actions workflow builds the package to WebAssembly with the rwasm toolchain and publishes it as a small CRAN-like repository alongside the site.
* `lago_report()` now renders an interactive HTML dashboard: the confidence set is a hover-enabled D3 plot (a scatter for two components, a strip for one) with the recommended intervention highlighted, and each intervention component gets interactive total-cost and marginal-cost curves. The report stays a single self-contained offline file (D3 is inlined, no CDN or server) and its API is unchanged; rendering now also uses `jsonlite` (a new Suggests).
* Added an MCP (Model Context Protocol) server to the Python package (`python -m lago.mcp_server`) that exposes `optimize` and `sensitivity` as tools any MCP-aware AI agent can call, plus a `sensitivity()` function in the Python wrapper.
* Added `lago_sensitivity()`, which re-runs an optimization across a sweep of one input (an outcome or power goal, or a `"cost_multiplier"` that scales all costs) and reports how the recommended intervention, its cost, and the estimated outcome move, with `print()` and `plot()` methods.
* The package is now installed and loaded as `LAGOtrials` (call `library(LAGOtrials)`); the previous `LAGO` identifier clashed with an archived CRAN package. Function names, the `lago` result class, and the LAGO method name are unchanged.
* `visualize_cost()` now stores the returned cost list in the `lago_cost_list` option instead of assigning it to the global environment; retrieve it with `getOption("lago_cost_list")`.
* Added tests for the interaction-terms optimization path and the outcome-model fit warnings, and excluded the interactive `visualize_cost()` app from the coverage figure so it reflects the testable R code.
* Added a `CITATION.cff` so the repository can be cited, plus contributing guidelines, a code of conduct and issue/pull-request templates. (#86)
* Added test-coverage reporting, cross-platform (macOS, Windows, Linux) continuous integration, project-status and coverage badges, and a social-preview card. (#83, #84, #85)
* Added a Python wrapper (in `python/`, importable as `lago`) that calls LAGO through `rpy2`. (#81)
* `visualize_cost()` now draws its cost curves client-side with D3, with hover read-outs, a draggable curve endpoint, and invalid-state highlighting. (#80)
* The clustered variance estimator for logit outcomes is now computed by a compiled Rcpp kernel, so `Rcpp` is a new dependency. (#79)
* `lago_optimization()` now refuses an additional covariate whose column is entirely `NA`, naming it. (#77)
* A rank-deficient outcome model is now refused up front with an error naming the aliased terms. (#74, #76)
* Added a warning when `glm()` drops an additional covariate as collinear. (#76)
* Fixed confidence-interval bounds for a binary logit outcome being reported outside `[0, 1]`. (#75)
* Fixed the estimated outcome being scaled by center weights that did not sum to 1. (#72, #75)
* Added a warning when a binary outcome's estimated outcome is reported outside `[0, 1]`. (#75)
* Added a warning when a numeric additional covariate observed away from 0 is held at 0 for the confidence set. (#75)
* Fixed the confidence interval and set being computed on the logit scale for an identity-link binomial model. (#74)
* `center_weights_for_outcome_goal` must now be numeric, finite and non-negative, checked at both entry points. (#73, #74)
* Fixed the numerical optimizer failing with an opaque error when every restart failed, so it now gives an actionable message. (#73)
* Fixed a factor covariate named like "center" or "period" being miscounted as a fixed effect when `get_confidence_set()` is called directly. (#73)
* Fixed `outcome_goal_intention = "minimize"` ignoring the outcome goal on a logit link. (#70)
* Fixed the recommended intervention sometimes falling outside the intervention bounds. (#70)
* Fixed the numerical optimisation returning its most expensive candidate solution instead of the cheapest. (#70)
* Fixed the estimated outcome being wrong when more than one center characteristic was supplied. (#70)
* `link = "probit"` and `link = "log"` are no longer accepted, since only `logit` and `identity` were ever implemented. (#70)
* Fixed `$est_outcome_ci` sometimes reporting the wrong interval instead of the one at the recommended intervention. (#68)
* Fixed the confidence set occasionally dropping one of its qualifying interventions. (#68)
* Fixed `confidence_set_size_percentage` understating the size of the confidence set. (#68)
* A confidence set containing exactly one grid intervention is no longer discarded as empty. (#68)
* Fixed confidence-interval bounds being attached to the wrong interventions under two-way clustering. (#68)
* `get_confidence_set()` now returns `rec_int_ci`, the interval at the recommended intervention (its return shape changed for direct callers). (#68)
* The confidence set, its size, and the estimated-outcome interval can differ from earlier releases wherever the previous values were wrong. (#68)
* Fixed `get_confidence_set()` pairing the prediction matrix with the model coefficients by position instead of by name. (#68)
* Fixed the estimated outcome being reported as 0 for the reference period when fixed time effects were included. (#68)
* Fixed the estimated-outcome interval being computed at the wrong time period. (#68)
* Fixed an additional covariate or center characteristic named like "center" being miscounted as a fixed center effect. (#68)
* Fixed a factor or character additional covariate or center characteristic being paired with the wrong coefficient or erroring. (#68)
* Fixed the confidence set changing when the rows of `data` were reordered. (#68)
* Fixed passing more than one center characteristic to the confidence set failing or duplicating a column. (#68)
* Added runnable `@examples` to every exported function that lacked them. (#67)
* Restyled the `print()` and `summary()` console output with boxed `cli` sections. (#65)
* The fitted outcome model (`$model`) and the estimated-outcome interval (`$est_outcome_ci`) are now returned on the result. (#65)
* Added `lago_report()`, which renders a self-contained HTML report of a result. (#64)
* `lago_optimization()` now returns a `"lago"` object with `print()`, `summary()`, and `plot()` methods. (#61)
* Added an "Optimizing an intervention with LAGO" vignette. (#61)
* Added a hex logo and refreshed the README header. (#56, #61)
* Added a `quiet` argument to `lago_optimization()` that skips progress messages. (#60)
* `visualize_cost()` gained a copy-to-clipboard button, a numeric cost summary, and returns the cost list on close. (#59)
* Expanded the reference documentation for the bundled datasets. (#58)
* Added a GPL-3 `LICENSE` file. (#56)
* Fixed a crash when a single intervention component was used. (#54)
* Added a pkgdown documentation site published to GitHub Pages. (#53)
* The overall intervention-effect test result is now returned in the result object. (#49)
* Added an automated test suite (testthat) and continuous-integration checks. (#45)
* `lago_optimization()` now accepts a standalone power goal, an outcome goal, or both together. (#40)
* Added non-fatal outcome-model fit diagnostics. (#36)
* `visualize_cost()` sliders now support per-component default and custom ranges. (#34)
* Supplying `optimization_grid_search_step_size` now switches the optimization method to grid search automatically. (#32)
* Added an `icc` argument so the power goal can account for within-center clustering. (#29)

## Known limitations

* The confidence set is the set of interventions whose confidence interval
  covers the outcome goal, which is a two sided test and does not depend on
  `outcome_goal_intention`. Under `outcome_goal_intention = "minimize"` it can
  therefore contain interventions whose estimated outcome is above the goal, and
  which cost more than the recommendation, because their interval still reaches
  the goal from above. Read the confidence set as the interventions the data
  cannot distinguish from the goal, not as the interventions that meet it. The
  estimated outcome and its interval, reported as `est_outcome_goal` and
  `est_outcome_ci`, are computed at the recommended intervention and are
  direction independent, so they are unaffected.
