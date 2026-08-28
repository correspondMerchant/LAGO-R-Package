# Print a LAGO optimization result

Full console display of the object returned by \[lago_optimization()\],
rendered with boxed, colour-accented \[cli\]\[cli::cli\] sections: an
inputs recap (data dimensions, outcome, intervention components, model
family/link and fixed effects, goals, costs and bounds), the fitted
outcome-model coefficient table, the overall intervention-effect test,
the recommended intervention with its cost and the estimated outcome
(and its 95% confidence interval), and the confidence set (size, cost
IQR, and first rows). Everything is shown on the console so results can
be read without further calls. \[summary.lago()\] renders the same
output.

## Usage

``` r
# S3 method for class 'lago'
print(x, ...)
```

## Arguments

- x:

  A "lago" object returned by \[lago_optimization()\].

- ...:

  Ignored.

## Value

\`x\`, invisibly.

## Examples

``` r
# lago_optimization() already prints the result, so quiet = TRUE avoids
# rendering it twice here. The lower bounds start at 1 while the data also
# contains 0s, so the optimizer warns about that; the warning is expected.
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
  cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
  outcome_goal = 0.85,
  outcome_goal_intention = "maximize",
  include_confidence_set = TRUE,
  confidence_set_grid_step_size = c(1, 1),
  quiet = TRUE
)
#> Warning: The lower bound for the intervention component coaching_updt is greater than the minimum value in the data.
#> Warning: The lower bound for the intervention component launch_duration is greater than the minimum value in the data.

print(result)
#> 
#> ── LAGO optimization result ────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Input data dimensions: 6124 rows, 21 columns
#> Outcome name: pp3_oxytocin_mother
#> Outcome type: binary
#> 2 intervention component(s): coaching_updt, launch_duration
#> 1 center characteristic(s): birth_volume_100
#> Outcome model family: binomial
#> Outcome model link: logit
#> Fixed center effects: FALSE
#> Fixed time effects: FALSE
#> Outcome goal: 0.85
#> Power goal: not specified
#> Intervention component costs: c(0, 1700), c(0, 8000)
#> Intervention lower bounds: 1, 1
#> Intervention upper bounds: 40, 5
#> 
#> ── Outcome model fit 
#> 
#> Call:
#> glm(formula = formula, family = family_object, data = data, weights = weights)
#> 
#> Coefficients:
#>                   Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)      -2.299892   0.068371 -33.638  < 2e-16 ***
#> coaching_updt     0.025137   0.006112   4.113 3.91e-05 ***
#> launch_duration   1.024470   0.074135  13.819  < 2e-16 ***
#> birth_volume_100  0.664511   0.029627  22.429  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 8470.8  on 6123  degrees of freedom
#> Residual deviance: 5161.2  on 6120  degrees of freedom
#> AIC: 5169.2
#> 
#> Number of Fisher Scoring iterations: 6
#> 
#> 
#> ── Overall intervention-effect test 
#> To see the overall test results, include a 'group' column in the data with
#> values 'treatment' or 'control' (binary outcomes only).
#> 
#> ── Recommended intervention 
#> coaching_updt: 1
#> launch_duration: 2.7785
#> Cost: 23928
#> Estimated outcome: 0.85
#> 95% CI for the estimated outcome: 0.802 - 0.898
#> Outcome goal: 0.85
#> 
#> ── Confidence set 
#> 95% confidence set size: 9% of the grid
#> IQR of the cost within the 95% confidence set: 62325 - 76775
#> First rows of the confidence set (use $cs for all):
#>    coaching_updt launch_duration birth_volume_100 CI_lower_bound CI_upper_bound
#> 68            27               2             1.75          0.811          0.851
#> 69            28               2             1.75          0.814          0.855
#> 70            29               2             1.75          0.816          0.859
#> 71            30               2             1.75          0.819          0.863
#> 72            31               2             1.75          0.822          0.867
#> 73            32               2             1.75          0.824          0.871
#>     cost
#> 68 61900
#> 69 63600
#> 70 65300
#> 71 67000
#> 72 68700
#> 73 70400
```
