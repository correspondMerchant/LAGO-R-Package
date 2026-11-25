# Print a LAGO sensitivity analysis

Prints a short header naming the varied parameter, the number of runs
and how many failed, then the sensitivity `data.frame`, then a one-line
summary of how `rec_int_cost` ranges over the sweep, from the smallest
to the largest value, or from the first to the last value of a sweep of
period labels or dates. The table and the summary round the numbers to 7
significant digits and write them out without e notation, except a
number below 1e-6 or of 1e21 or more in size. The estimated outcome of a
run that reached its goal is rounded as
[`print.lago()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/print.lago.md)
rounds it, so a goal of 0.75 met by an estimate of 0.7499995 shows as
0.75. When the goal of some runs could not be reached, so the shrinking
method gave their recommendation, a line after the table lists their
values.

## Usage

``` r
# S3 method for class 'lago_sensitivity'
print(x, ...)
```

## Arguments

- x:

  A `"lago_sensitivity"` object from
  [`lago_sensitivity()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_sensitivity.md).

- ...:

  Ignored.

## Value

`x`, invisibly.

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
#>   value gear     qsec rec_int_cost est_outcome_goal status
#> 1    30   10 6.522269       83.134               30     ok
#> 2    35   10 9.239845       99.439               35     ok
#> 3    40   10 11.95743       115.74               40     ok
#> rec_int_cost is 83.134 at outcome_goal = 30 and 115.74 at outcome_goal = 40.
# }
```
