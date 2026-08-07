# get_confidence_set

Internal function that calculates the confidence set for the recommended
interventions

## Usage

``` r
get_confidence_set(
  predictors_data,
  include_center_effects = FALSE,
  center_weights_for_outcome_goal = 1,
  include_time_effects = FALSE,
  time_effect_optimization_value = NULL,
  additional_covariates = NULL,
  intervention_components,
  include_interaction_terms = FALSE,
  main_components = NULL,
  outcome_data,
  fitted_model,
  link,
  outcome_goal,
  outcome_type,
  intervention_lower_bounds,
  intervention_upper_bounds,
  confidence_set_grid_step_size,
  center_characteristics = NULL,
  center_characteristics_optimization_values = 0,
  confidence_set_alpha = 0.05,
  cluster_id = NULL,
  cost_list_of_vectors,
  rec_int
)
```

## Arguments

- predictors_data:

  A data.frame. The input data containing the intervention components
  and center characteristics.

- include_center_effects:

  A boolean. Specifies whether the fixed effects should be included in
  the outcome model.

- center_weights_for_outcome_goal:

  A numeric vector. Specifies the weights that will be used for
  calculating recommended interventions that satisfy the outcome goal
  for an (weighted) average center. The weights need to sum up to 1, and
  must all be non-negative and finite. A weight of 0 is allowed and
  excludes that center from the average. Only used, and only checked,
  when include_center_effects is TRUE. A vector whose sum is not 1 is
  refused here rather than renormalised: the interval is computed AT the
  weights the optimization ran with, so rescaling them would report an
  interval for a different weighting than the point estimate beside it.
  lago_optimization() refuses the same vectors, and normalises within
  the tolerance the ones it accepts, so weights arriving from it always
  sum to 1.

- include_time_effects:

  A boolean. Specifies whether the fixed time effects should be included
  in the outcome model.

- time_effect_optimization_value:

  The period the confidence set is computed at, as the value of the
  "period" column that identifies it. The recommended intervention and
  the estimated outcome reported alongside the confidence set are
  computed at this period, so the interval is computed at it too.
  Required when include_time_effects is TRUE.

- additional_covariates:

  A character vector. The names of the columns in the dataset that
  represent additional covariates that need to be included in the
  outcome model. This includes interaction terms or any other additional
  covariates.

- intervention_components:

  A character vector. The names of the columns in the dataset that
  represent the intervention components.

- include_interaction_terms:

  A boolean. Specifies whether there are interaction terms in the
  intervention components.

- main_components:

  A character vector. Specifies the main intervention components in the
  presence of interaction terms.

- outcome_data:

  A vector. The input data containing the outcome of interest.

- fitted_model:

  A glm(). The fitted glm() outcome model.

- link:

  A character string. The link function the interval is computed on,
  either "logit" or "identity". These are the only links the outcome
  machinery implements, see supported_outcome_links().

- outcome_goal:

  A numeric value. Specifies the outcome goal, a desired probability or
  mean value.

- outcome_type:

  A character string. Specifies the type of the outcome. Must be either
  "continuous" for continuous outcomes or "binary" for binary outcomes.

- intervention_lower_bounds:

  A numeric vector. Specifies the lower bounds of the intervention
  components.

- intervention_upper_bounds:

  A numeric vector. Specifies the upper bounds of the intervention
  components.

- confidence_set_grid_step_size:

  A numeric vector. Specifies the step size of the grid search algorithm
  used in the confidence set calculation.

- center_characteristics:

  A character vector. The names of the columns in the dataset that
  represent the center characteristics.

- center_characteristics_optimization_values:

  A numeric vector. The fixed values of the center characteristics at
  which the confidence set is computed, so the confidence set is
  specific to a center with these characteristic values. Must have the
  same length and order as center_characteristics.

- confidence_set_alpha:

  A numeric value. The type I error considered in the confidence set
  calculations.

- cluster_id:

  A list or NULL. Specifies the columns of data that will be used as
  clustering effects when the "outcome_type" is continuous.

- cost_list_of_vectors:

  A list of numeric vectors. The cost vectors used in the LAGO
  optimization.

- rec_int:

  A numeric vector, the recommended interventions calculated from the
  optimization step.

## Value

List( confidence_set_size_percentage = \<number, the size of the
confidence set as a fraction of the grid. Both the count of qualifying
interventions and the size of the grid count grid interventions only, so
rec_int is excluded from each\>, rec_int_ci = \<named numeric c(lower,
upper) rounded to 3 decimal places, the confidence interval at rec_int.
Computed whether or not it covers the outcome goal, so callers never
have to look for rec_int inside cs. For a binary outcome on the logit
link both bounds are confined to \[0, 1\], where the estimate is a
probability by construction and the interval around it therefore belongs
in that range; the interval is still the delta-method one and a bound at
exactly 0 or 1 is one that has been truncated to the range. Not confined
on the identity link, where the estimate is the linear predictor and is
itself unbounded, so confining the interval would report one that
excludes its own estimate. That estimate leaving \[0, 1\] is a defect in
its own right and not this one; lago_optimization() now warns when it
does, so it is flagged rather than silent, and the interval is still
reported as computed for the same reason as before. Not confined for a
continuous outcome either, whose range is not knowable here. NULL when
that interval is not computable\>, cs = \<data.frame of the grid
interventions whose confidence interval covers the outcome goal, with
their interval bounds and cost. rec_int is never one of its rows, and
need not be a grid intervention at all. NULL when no grid intervention
qualifies\> )

## Examples

``` r
# Normally reached through lago_optimization(include_confidence_set = TRUE).
# Called directly it needs the fitted outcome model and the recommended
# intervention from the optimization step, so both are taken from a run of
# the optimizer rather than refitting the model by hand. get_confidence_set()
# binds its prediction matrix to the coefficient vector by name, so a
# hand-fitted model may list its terms in any order. It must be fitted on
# exactly the predictors passed here, though: the intercept, the fixed
# center effects, the fixed time effects, the intervention components, the
# additional covariates and the center characteristics. Any other set of
# coefficients is an error naming what did not match.
# The lower bounds start at 1 while the data also contains 0s, so the
# optimizer warns about that; the warning is expected here.
opt <- lago_optimization(
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
  include_confidence_set = FALSE,
  quiet = TRUE
)
#> Warning: The lower bound for the intervention component coaching_updt is greater than the minimum value in the data.
#> Warning: The lower bound for the intervention component launch_duration is greater than the minimum value in the data.

intervention_components <- c("coaching_updt", "launch_duration")
predictors <- c(intervention_components, "birth_volume_100")

cs <- get_confidence_set(
  predictors_data = BB_data[, predictors, drop = FALSE],
  intervention_components = intervention_components,
  outcome_data = BB_data$pp3_oxytocin_mother,
  fitted_model = opt$model,
  link = "logit",
  outcome_goal = 0.85,
  outcome_type = "binary",
  intervention_lower_bounds = c(1, 1),
  intervention_upper_bounds = c(40, 5),
  confidence_set_grid_step_size = c(1, 1),
  center_characteristics = "birth_volume_100",
  center_characteristics_optimization_values = 1.75,
  cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
  rec_int = opt$rec_int
)

# Fraction of the grid inside the 95% confidence set: 18 of the 200 grid
# interventions qualify here (40 coaching values by 5 launch durations), so
# 0.09. print() shows the same number as a percentage.
cs$confidence_set_size_percentage
#> [1] 0.09

# The confidence interval at the recommended intervention, reported in its
# own field as c(lower, upper). It is computed whether or not it covers the
# outcome goal, so it is available even when no grid intervention qualifies.
# lago_optimization() reports this interval as $est_outcome_ci.
cs$rec_int_ci
#> lower upper 
#> 0.802 0.898 

# rec_int need not be one of the grid interventions, and here it is not:
# its launch_duration is about 2.78 while the grid steps through whole
# days. It is never a row of cs either, which holds the 18 qualifying grid
# interventions and nothing else.
opt$rec_int
#> [1] 1.000000 2.778472
head(cs$cs)
#>    coaching_updt launch_duration birth_volume_100 CI_lower_bound CI_upper_bound
#> 68            27               2             1.75          0.811          0.851
#> 69            28               2             1.75          0.814          0.855
#> 70            29               2             1.75          0.816          0.859
#> 71            30               2             1.75          0.819          0.863
#> 72            31               2             1.75          0.822          0.867
#> 73            32               2             1.75          0.824          0.871
#>    cost
#> 68 61.9
#> 69 63.6
#> 70 65.3
#> 71 67.0
#> 72 68.7
#> 73 70.4
```
