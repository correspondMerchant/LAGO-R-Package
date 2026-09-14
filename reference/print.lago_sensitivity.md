# Print a LAGO sensitivity analysis

Prints a short header naming the varied parameter, the number of runs
and how many failed, then the sensitivity \`data.frame\`, then a
one-line summary of how \`rec_int_cost\` ranges over the sweep.

## Usage

``` r
# S3 method for class 'lago_sensitivity'
print(x, ...)
```

## Arguments

- x:

  A \`"lago_sensitivity"\` object from \[lago_sensitivity()\].

- ...:

  Ignored.

## Value

\`x\`, invisibly.

## Examples

``` r
# \donttest{
sens <- lago_sensitivity(
  data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
  glm_family = "gaussian", link = "identity",
  intervention_components = c("gear", "qsec"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 350),
  cost_list_of_vectors = list(c(0, 4), c(4, 6)),
  outcome_goal_intention = "maximize",
  parameter = "outcome_goal", values = c(30, 35, 40)
)
print(sens)
#> 
#> ── LAGO sensitivity analysis ──
#> 
#> Varied outcome_goal across 3 runs; 0 failed.
#>   value gear      qsec rec_int_cost est_outcome_goal status
#> 1    30   10  6.522269     83.13361               30     ok
#> 2    35   10  9.239851     99.43911               35     ok
#> 3    40   10 11.957433    115.74460               40     ok
#> rec_int_cost ranges from 83.13361 to 115.7446 as outcome_goal goes from 30 to
#> 40.
# }
```
