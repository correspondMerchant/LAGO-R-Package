# Print a budget-constrained LAGO result

Prints a short header with the budget and whether it binds, then the
recommended intervention, its cost and estimated outcome (or a note that
no intervention fits the budget).

## Usage

``` r
# S3 method for class 'lago_budget'
print(x, ...)
```

## Arguments

- x:

  A \`"lago_budget"\` object from \[lago_budget()\].

- ...:

  Ignored.

## Value

\`x\`, invisibly.

## Examples

``` r
# \donttest{
b <- lago_budget(
  data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
  glm_family = "gaussian", link = "identity",
  intervention_components = c("gear", "qsec"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 350),
  cost_list_of_vectors = list(c(0, 4), c(4, 6)),
  outcome_goal_intention = "maximize", budget = 100
)
print(b)
#> 
#> ── LAGO budget-constrained optimization ──
#> 
#> Budget: 100 (maximize).
#> Recommended intervention: gear = 10.000000, qsec = 9.307224
#> Cost: 99.8433458625911 (of 100).
#> Estimated outcome: 35.1239581327576.
# }
```
