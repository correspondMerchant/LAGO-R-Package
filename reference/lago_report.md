# Generate an HTML report for a LAGO optimization result

Renders a self-contained HTML report for the object returned by
\[lago_optimization()\]. The report knits a bundled parameterized R
Markdown template and lays out the same sections, in the same order and
with the same labels, as the console methods (\[print.lago()\] /
\[summary.lago()\]): the recommended intervention, its cost, the
estimated outcome (with its 95% CI) and goal, the power goal (when
present), the confidence set (size, cost IQR, and first rows), the
overall intervention-effect test (when present), the fitted
outcome-model coefficient table, and the confidence-set plot from
\[plot.lago()\] (when a confidence set is available). A
\`sessionInfo()\` footer records the environment the report was produced
in.

No new statistics are computed; every value is taken verbatim from an
existing field on \`x\` and formatted exactly as the console output
formats it.

Rendering requires the rmarkdown and knitr packages (listed under
Suggests). If they are not installed, an informative error is raised.

## Usage

``` r
lago_report(
  x,
  output_file = NULL,
  title = "LAGO optimization report",
  open = FALSE,
  ...
)
```

## Arguments

- x:

  A "lago" object returned by \[lago_optimization()\].

- output_file:

  A character string. The path to write the HTML report to. If \`NULL\`
  (the default), a temporary file with a \`.html\` extension is used.

- title:

  A character string. The report title. Default value without user
  specification: "LAGO optimization report".

- open:

  A boolean. If \`TRUE\`, opens the rendered report in a browser via
  \[utils::browseURL()\]. Default value without user specification:
  \`FALSE\`.

- ...:

  Additional arguments passed to \[rmarkdown::render()\].

## Value

The path to the rendered HTML file, invisibly.

## See also

\[lago_optimization()\]

Other LAGO functions:
[`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md),
[`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md),
[`lago_sensitivity()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_sensitivity.md),
[`visualize_cost()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/visualize_cost.md)

## Examples

``` r
# lago_report() renders an HTML file with rmarkdown, so it is wrapped in
# \dontrun: it writes to disk and invokes pandoc, and running a document
# render inside an in-process documentation build (e.g. pkgdown) can deadlock.
if (FALSE) { # \dontrun{
result <- lago_optimization(
  data = BB_data,
  outcome_name = "pp3_oxytocin_mother",
  outcome_type = "binary",
  glm_family = "binomial",
  intervention_components = c("coaching_updt", "launch_duration"),
  center_characteristics = c("birth_volume_100"),
  center_characteristics_optimization_values = 1.75,
  intervention_lower_bounds = c(1, 1),
  intervention_upper_bounds = c(40, 5),
  cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
  outcome_goal = 0.85,
  outcome_goal_intention = "maximize",
  confidence_set_grid_step_size = c(1, 1),
  quiet = TRUE
)
report_path <- lago_report(result)
} # }
```
