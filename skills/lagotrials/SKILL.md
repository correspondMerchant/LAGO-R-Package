---
name: lagotrials
description: Runs Learn-As-you-GO (LAGO) adaptive-trial optimization with the LAGOtrials R package. Fits the outcome model, computes the cost-optimal recommended intervention for the next stage to reach an outcome and/or power goal, builds confidence sets, sweeps an input, and finds the best outcome for a budget. Use when the task mentions LAGO, LAGOtrials, lago_optimization(), lago_sensitivity(), lago_budget(), lago_app(), adaptive/multi-stage trial optimization, recommending an intervention package to hit a target outcome or power, designing cost functions with visualize_cost(), or building a confidence set for an optimal intervention.
version: 2
---

# LAGOtrials

## Overview

LAGOtrials implements the Learn-As-you-GO (LAGO) method for adaptive trials, in
which a multi-component intervention is updated between stages using the data
collected so far. Given data from completed stages, it fits an outcome model and
returns the cheapest intervention package expected to reach a target mean
outcome and/or statistical power for the next stage, plus a confidence set for
the optimal intervention.

Package name is `LAGOtrials` (install/`library()`), but the function names use
the `lago_` prefix and the result object has class `"lago"`.

Install from CRAN (https://CRAN.R-project.org/package=LAGOtrials):
```r
install.packages("LAGOtrials")
library(LAGOtrials)
```
The development version, with fixes not yet on CRAN:
```r
# install.packages("devtools")
devtools::install_github("correspondMerchant/LAGO-R-Package")
```

Seven exported functions:
- `lago_optimization()` is the main entry point. It does everything below in one call.
- `lago_sensitivity()` sweeps one input and reports how the recommendation moves.
- `lago_budget()` finds the best outcome a budget can buy.
- `lago_app()` runs the whole analysis in a point and click Shiny app.
- `get_confidence_set()` is the lower level confidence set builder. It is
  usually reached through `lago_optimization(include_confidence_set = TRUE)`.
- `visualize_cost()` is an interactive Shiny app to design cost functions.
- `lago_report()` renders a shareable HTML report from a result.

`lago_app()`, `visualize_cost()` and `lago_report()` open a browser or write a
file, so do not call them from an unattended script. For a user who does not
write R, suggest `lago_app()`, or `lago_app(result)` to open a result's
settings in the app.

Before writing a call, read the help: `?lago_optimization`. It has many
arguments; the ones below are the essential ones.

## Core workflow

`lago_optimization()` requires: `data` (a data.frame), `outcome_name`,
`outcome_type` (`"continuous"` or `"binary"`), `intervention_components` (column
names), `intervention_lower_bounds`, `intervention_upper_bounds`, at least one
goal, and a cost specification.

```r
res <- lago_optimization(
  data = my_data,
  outcome_name = "Y",
  outcome_type = "continuous",
  glm_family = "gaussian",           # "default" picks gaussian/binomial by type
  link = "identity",                 # "default" picks identity/logit by family
  intervention_components = c("comp1", "comp2"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 10),
  cost_list_of_vectors = list(c(0, 4), c(4, 6)),
  outcome_goal = 40,
  outcome_goal_intention = "maximize",   # or "minimize"
  confidence_set_grid_step_size = c(1, 1)
)
res$rec_int          # recommended intervention (one value per component)
res$rec_int_cost     # its cost
res$est_outcome_goal # estimated outcome at the recommendation
res$est_outcome_ci   # CI for that estimate, 95% by default
res$cs               # confidence set (data.frame of qualifying grid points)
res                  # print()/summary() show the full formatted result
```

## Goal modes

Provide `outcome_goal`, `power_goal`, or both (at least one is required):
- Outcome goal alone: reach a target mean outcome.
- Power goal alone: reach a target power. Binary outcome only, and it needs a
  `group` column plus `num_centers_in_next_stage` and
  `patients_per_center_in_next_stage`.
- Both: the effective target is the higher of the outcome goal and the
  power-implied outcome (the "whichever is higher" rule).

`outcome_goal_intention` is `"maximize"` (default) or `"minimize"`. A power goal
cannot be combined with `"minimize"`.

For clustering, pass `icc` (a single value or `c(control, treatment)`) and
`power_goal_cluster_id`; the power calculation then inflates the variance by the
design effect.

## Cost functions

`cost_list_of_vectors` is a list with one numeric vector per intervention
component. Each vector is the polynomial coefficients of that component's cost:
`C(x) = coef[1] + coef[2]*x + coef[3]*x^2 + ...`. So `c(0, 4)` means `C(x)=4x`,
and `c(4, 6)` means `C(x)=4+6x`. Providing `cost_list_of_vectors` makes
`default_cost_fxn_type` ignored.

Alternatively give `unit_costs` + `default_cost_fxn_type` (`"linear"` or
`"cubic"`) and let the package build the coefficients. With the cubic cost,
also give `unit_cost_doses`, the dose each unit cost was measured at, so the
cost per unit at that dose is the unit cost. Without it a warning says the
cubic cost only matches each unit cost on average over the bounds.

To design cost functions interactively, run `visualize_cost()` — a Shiny app
with sliders and live curves. On close it returns the coefficient list (also in
`getOption("lago_cost_list")`), ready to pass as `cost_list_of_vectors`.

## Outcome model options

- `glm_family`: `"default"` (gaussian for continuous, binomial for binary). A
  binary outcome takes only `"binomial"`. A continuous outcome takes
  `"gaussian"`, or `"quasibinomial"` for a proportion between 0 and 1.
- `link`: only `"logit"` and `"identity"` are supported (the confidence-set and
  minimize machinery only implement those). `"default"` picks identity for
  gaussian, and logit for binomial and quasibinomial.
- `weights`: a numeric vector of glm() weights, one per row.
- Center level data: `input_data_structure = "center_level"` takes one row per
  center and period, with the outcome as a share in a column named
  `proportion` and each row's patients in `center_sample_size`. The outcome
  must be binary, `outcome_name` must be `"proportion"`, and center effects are
  on unless `include_center_effects = FALSE`. `weights` is ignored.
- Fixed effects: `include_center_effects = TRUE` (+ `center_effects_optimization_values`)
  and `include_time_effects = TRUE` (+ `time_effect_optimization_value`).
- `center_characteristics` (+ `center_characteristics_optimization_values`) and
  `additional_covariates` add model terms held at optimization/reference values.
- Interaction terms: set `include_interaction_terms = TRUE`, put the interaction
  in `intervention_components` as `"a:b"`, and list the mains in
  `main_components`, e.g. `intervention_components = c("a","b","a:b")`,
  `main_components = c("a","b")`.
- Optimizer: `optimization_method = "numerical"` (default) or `"grid_search"`.
  Supplying `optimization_grid_search_step_size` switches to grid search
  automatically.

## Confidence set

By default `include_confidence_set = TRUE`. The confidence set (`res$cs`) is the
set of grid interventions whose confidence interval covers the outcome goal.
`confidence_set_grid_step_size` controls grid resolution per component (coarser =
faster). A fine grid over 3+ components can be slow; increase the step size if it
drags. The interval at the recommendation itself is `res$est_outcome_ci` (a
field named `rec_int_ci` exists only on the object returned by
`get_confidence_set()` called directly).

Note: the confidence set is a two-sided test and does not depend on
`outcome_goal_intention`, so under `"minimize"` it can include interventions
whose estimate is above the goal. Read it as "interventions the data cannot
distinguish from the goal," not "interventions that meet it."

## Critical warnings

- Links are limited to `"logit"` and `"identity"`. `"probit"` and `"log"` are
  rejected (they were never fully implemented and would report the wrong scale).
- Binary outcome on `link = "identity"` is a linear probability model: the
  estimated outcome is the linear predictor and can fall outside `[0, 1]` when
  interventions extrapolate beyond the fitted range. The package warns and
  reports the value as computed (it does not clamp). Use `link = "logit"` for a
  probability by construction.
- An intervention component or additional covariate whose column is entirely
  `NA` is refused up front with a clear error.
- A rank-deficient outcome model (glm returns `NA` for a coefficient the
  optimizer reads, e.g. aliased fixed effects) is refused up front, naming the
  aliased terms. Switching to `grid_search` does not help — the fit is the
  problem; drop or combine the collinear predictors.
- `center_weights_for_outcome_goal` must be numeric, finite and non-negative,
  sum to 1, and follow the order of the centers: the levels of the `center`
  factor, or its sorted values. A center with no complete row must get 0.
- A `center_characteristics_optimization_values` value outside the range of
  that characteristic in the data gives a warning: the recommendation is then
  an extrapolation, and its interval can be very wide. Pass the warning on to
  the user, and suggest a value within the data.
- On the logit link, the interval of a binary outcome is the delta method
  interval, clamped to `[0, 1]`. A bound at exactly 0 or 1 means the interval
  went past it. An interval from 0 to 1 means the data cannot pin the outcome
  down there, which usually comes from an extrapolation or very few centers.
  On the identity link nothing is clamped (see the linear probability model
  above).

## Troubleshooting

- "The intervention component(s) ... are entirely NA" / "additional covariate(s)
  ... are entirely NA": that column has no observed values; remove it or supply
  data.
- Error naming aliased terms / "rank-deficient": collinear predictors; drop or
  combine them. Do not switch optimizer.
- "Numerical optimization failed to find a solution": try
  `optimization_method = "grid_search"` with
  `optimization_grid_search_step_size`; common with more than three components.
- A warning that the estimated outcome is outside `[0, 1]`: binary identity-link
  extrapolation (see Critical warnings); narrow the bounds or use logit.
- Confidence set is slow: increase `confidence_set_grid_step_size`, or set
  `include_confidence_set = FALSE` while iterating.
- A warning that the recommendation is for a center outside the centers in the
  data: the center characteristic value is outside the data. Use a value
  within it, or say the result is an extrapolation.
- An interval from 0 to 1 for a binary outcome: the model is very uncertain at
  the recommendation. Check for an extrapolation, too few centers, or a goal
  near the edge of what the data supports.

## Sensitivity and budget

Both take a fitted result and rerun its call, so the arguments need not be
typed again:
```r
sens <- lago_sensitivity(res, parameter = "outcome_goal", values = c(0.8, 0.85, 0.9))
plot(sens)                      # cost of the recommendation per value
lago_sensitivity(res, parameter = "cost_multiplier", values = c(0.8, 1, 1.2))
b <- lago_budget(res, budget = 30000)   # n_grid = 25 outcome goals by default
b$feasible; b$binding; b$rec_int; b$est_outcome
plot(b)                         # the cost and outcome of the goals searched
```
`parameter` is a scalar numeric argument of `lago_optimization()`, such as
`"outcome_goal"`, `"power_goal"`, `"icc"` or `"time_effect_optimization_value"`,
or `"cost_multiplier"`, which needs `cost_list_of_vectors` (a result keeps
it, so pass `cost_list_of_vectors = res$cost_list_of_vectors` for a result
fitted with unit costs). Neither computes the confidence set. A failed run is
a row with status `"error"`, not a stopped sweep.

## From an agent: the MCP server

`python/lago/mcp_server.py` in the repository serves the package over the
Model Context Protocol, with the tools `describe_data`, `optimize`,
`sensitivity` and `budget`. It needs R, LAGOtrials and Python with rpy2, as
`python/README.md` explains. Each result has a `warnings` list with the warnings of the
package. Report them with the numbers.

## The result object

`lago_optimization()` returns a `"lago"` object (a list, so `$` access works)
with `print()`, `summary()`, and `plot()` methods. Key fields: `rec_int`,
`rec_int_cost`, `est_outcome_goal`, `est_outcome_ci` (the CI at the
recommendation, 95% by default), `cs` (confidence set), and `model` (the fitted glm). `plot()` visualizes the confidence
set; `lago_report(res)` writes the full result to a shareable HTML file.

## Additional resources

- Vignette: "Optimizing an intervention with LAGO" (`browseVignettes("LAGOtrials")`).
- Help: `?lago_optimization`, `?lago_sensitivity`, `?lago_budget`, `?lago_app`,
  `?get_confidence_set`, `?visualize_cost`, `?lago_report`.
- Methods paper: Bing, Spiegelman, Nevo, Lok (2025), Biometrics,
  <doi:10.1093/biomtc/ujaf061>; foundational: Nevo, Lok, Spiegelman (2021),
  Annals of Statistics, <doi:10.1214/20-aos1978>.
