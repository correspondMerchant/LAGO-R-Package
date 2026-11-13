# Run a LAGO optimization in a point and click app

Opens a Shiny app in your web browser that runs the whole LAGO
optimization without writing R code. You load a dataset, choose the
outcome and the goal, choose the intervention components with their
bounds and costs, and say how the model accounts for centers and time.
The app then finds the recommended intervention with
[`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
and shows the result, the confidence set and the R code that reproduces
the run. From that result it can sweep one input with
[`lago_sensitivity()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_sensitivity.md),
find the best outcome for a budget with
[`lago_budget()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_budget.md),
and save a report with
[`lago_report()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_report.md).

## Usage

``` r
lago_app(data = NULL, launch.browser = interactive(), port = NULL)
```

## Arguments

- data:

  A data.frame to start with, or `NULL` to start with the bundled
  `BB_data` example.

- launch.browser:

  Whether to open the app in your web browser. By default it opens in an
  interactive session.

- port:

  The port to listen on, or `NULL` for a free one.

## Value

Invisibly, when you click **Done** or close the browser tab, a list with
the last `result` of
[`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
(or `NULL` if nothing was run), the last `sensitivity` of
[`lago_sensitivity()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_sensitivity.md)
and `budget` of
[`lago_budget()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_budget.md)
run from it (or `NULL`), the R `code` of all of them as a character
vector, and the `arguments` of the optimization. The same list is stored
in the `lago_app_result` option.

## Details

The app runs on your own computer and is reachable only from it, at
`127.0.0.1`. Nothing is sent over the network.

The app has these steps.

- **Data:** the `data` you pass, the bundled `BB_data`, R's `mtcars`, or
  a CSV file you upload. Each example opens on a setup that runs as is.

- **Outcome and goal:** `outcome_name`, `outcome_type`, `outcome_goal`,
  `outcome_goal_intention`, and for a binary outcome the power goal:
  `power_goal`, `num_centers_in_next_stage`,
  `patients_per_center_in_next_stage`, `power_goal_approach`, `icc` and
  `power_goal_cluster_id`. The app builds the `group` column from the
  treatment arm column you choose.

- **Intervention and costs:** `intervention_components`, their bounds,
  and the costs as `unit_costs` with `default_cost_fxn_type` and
  `unit_cost_doses`, or as `cost_list_of_vectors`. The "Shape the costs
  visually" button opens the cost designer of
  [`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
  in a dialog, which starts on the costs in use and puts the shaped
  costs in as cost coefficients. Interaction terms set
  `include_interaction_terms` and `main_components`, and the app builds
  each product column.

- **Centers, time and covariates:** `center_characteristics` with their
  values, or fixed center effects with `include_center_effects` and
  `center_effects_optimization_values`, fixed time effects with
  `include_time_effects` and `time_effect_optimization_value`, and
  `additional_covariates`. The app builds the `center` and `period`
  columns from the columns you choose.

A step that needs attention says what to change, and the run button
stays off until every step is ready. Warnings from the package are
listed with the result.

After a run, these tabs start from its result.

- **Results:** the recommendation, the confidence set with a CSV
  download, the outcome model, the intervention test, and the report
  download of
  [`lago_report()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_report.md),
  which needs the rmarkdown, knitr and jsonlite packages and pandoc.

- **Sensitivity:**
  [`lago_sensitivity()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_sensitivity.md)
  with `parameter` and `values`. It varies the outcome goal, a
  multiplier of all costs, and with a power goal the power goal, the
  centers and patients of the next stage, and the ICC. With time effects
  it varies the period. The values are even steps from a start to an
  end, or a typed list. A plot, a table and a CSV download show the
  result.

- **Budget:**
  [`lago_budget()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_budget.md)
  with `budget` and `n_grid`. The budget starts at the cost of the
  recommendation. The best outcome it buys can be put in as the outcome
  goal.

- **R code:** the code of the run, followed by the code of the last
  sweep and budget search of that result.

## See also

[`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
for the function the app runs,
[`lago_sensitivity()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_sensitivity.md),
[`lago_budget()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_budget.md)
and
[`lago_report()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_report.md)
for the tabs that start from its result, and
[`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)
for the cost designer.

## Examples

``` r
if (interactive()) {
  out <- lago_app()
  out$result
  cat(out$code, sep = "\n")
}
```
