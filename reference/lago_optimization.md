# Run a LAGO optimization

Fits the outcome model, calculates the recommended interventions based
on an outcome goal and/or a power goal, calculates the confidence set
for the recommended interventions, and prints the output. This is the
main entry point of the package.

## Usage

``` r
lago_optimization(
  data,
  input_data_structure = "individual_level",
  outcome_name,
  outcome_type,
  intervention_components,
  intervention_lower_bounds,
  intervention_upper_bounds,
  outcome_goal = NULL,
  outcome_goal_intention = "maximize",
  power_goal = NULL,
  power_goal_approach = "unconditional",
  num_centers_in_next_stage = NULL,
  patients_per_center_in_next_stage = NULL,
  icc = NULL,
  power_goal_cluster_id = NULL,
  unit_costs = NULL,
  default_cost_fxn_type = "cubic",
  cost_list_of_vectors = NULL,
  glm_family = "default",
  link = "default",
  optimization_method = "numerical",
  confidence_set_alpha = 0.05,
  weights = NULL,
  center_characteristics = NULL,
  center_characteristics_optimization_values = NULL,
  main_components = NULL,
  time_effect_optimization_value = NULL,
  additional_covariates = NULL,
  center_weights_for_outcome_goal = NULL,
  optimization_grid_search_step_size = NULL,
  confidence_set_grid_step_size = NULL,
  include_confidence_set = TRUE,
  include_center_effects = FALSE,
  center_effects_optimization_values = NULL,
  include_time_effects = FALSE,
  include_interaction_terms = FALSE,
  prev_recommended_interventions = NULL,
  shrinkage_threshold = 0.25,
  quiet = FALSE
)
```

## Arguments

- data:

  A data.frame. The input dataset containing the variables of interest.

- input_data_structure:

  A character string. The data structure of the input data. Must be
  either "individual_level" or "center_level". Default value without
  user specification: "individual_level".

- outcome_name:

  A character string. The name of the column in the dataset that
  represents the outcome of interest.

- outcome_type:

  A character string. Specifies the type of the outcome. Must be either
  "continuous" for continuous outcomes or "binary" for binary outcomes.

- intervention_components:

  A character vector. The names of the columns in the dataset that
  represent the intervention components. For example: c("component1",
  "component2").

- intervention_lower_bounds:

  A numeric vector. Specifies the lower bounds of the intervention
  components. For example: for a two-component intervention package,
  lower bounds could be c(0,0).

- intervention_upper_bounds:

  A numeric vector. Specifies the upper bounds of the intervention
  components. For example: for a two-component intervention package,
  upper bounds could be c(10,20).

- outcome_goal:

  A numeric value. Specifies the outcome goal, a desired probability or
  mean value. Default value without user specification: NULL. At least
  one of outcome_goal or power_goal must be provided. If only a power
  goal is provided, the optimization target is the outcome level needed
  to achieve the power goal. If both are provided, the effective outcome
  goal is the higher of the two (see power_goal).

- outcome_goal_intention:

  A character string. Specifies the direction in which the interventions
  are expected to move the outcome, relative to the outcome without any
  intervention. Must be either "maximize" or "minimize". Set it to
  "maximize" when a larger outcome is desired and the interventions are
  meant to increase the outcome up to the outcome goal (for example,
  increasing the probability that a treatment is administered). Set it
  to "minimize" when a smaller outcome is desired and the interventions
  are meant to decrease the outcome down to the outcome goal (for
  example, reducing a mortality rate). This argument only controls the
  optimization direction; the outcome goal itself is always given as the
  target outcome value, regardless of the intention. Default value
  without user specification: "maximize". Note: "minimize" cannot be
  combined with a power goal, because achieving a power goal requires
  increasing the outcome.

- power_goal:

  A numeric value. Specifies the power goal, a desired power value
  (between 0 and 1). Only supported for binary outcomes, and requires a
  'group' column (values "treatment"/"control") along with
  num_centers_in_next_stage and patients_per_center_in_next_stage. At
  least one of outcome_goal or power_goal must be provided. The power
  goal is converted into the outcome level needed to achieve that power;
  the effective outcome goal used for optimization is max(power-implied
  outcome, outcome_goal). When only a power goal is provided, the
  power-implied outcome is used directly. Default value without user
  specification: NULL.

- power_goal_approach:

  A character string. Specifies the approach to achieve the power goal.
  Must be either "unconditional" or "conditional". Default value without
  user specification: "unconditional".

- num_centers_in_next_stage:

  A numeric value. Specifies the number of total centers (treatment +
  control) in the next stage of the trial. Default value without user
  specification: NULL.

- patients_per_center_in_next_stage:

  A numeric value. Specifies the number of patients per center in the
  next stage of the trial. Default value without user specification:
  NULL.

- icc:

  A numeric value in \[0, 1), or a length-2 numeric vector c(control,
  treatment). Specifies the intra-cluster correlation used to inflate
  the variance of the power-calculation test statistic by a design
  effect, so that the power goal accounts for within-center clustering.
  Only used when a power_goal is provided (ignored otherwise). When
  NULL, the power calculation assumes independent observations (the
  original behavior); icc = 0 gives the identical result. A larger icc
  raises the outcome level needed to meet the power goal, so it
  recommends a stronger intervention. A non-zero icc requires
  power_goal_cluster_id. Default value without user specification: NULL.

- power_goal_cluster_id:

  A character string. The name of a column in the data identifying the
  stage-1 centers (clusters), used to compute the stage-1 design effect
  for the power calculation. Required when icc is non-zero. The stage-1
  cluster size is the variance-appropriate (size-biased) mean sum(m_i^2)
  / sum(m_i) over that arm's centers, and each arm must have at least
  two centers. Default value without user specification: NULL.

- unit_costs:

  A numeric vector. Specifies the unit costs for each intervention
  component. Default value without user specification: NULL.

- default_cost_fxn_type:

  A character string. Specifies the shape of the default cost function.
  Must be either "linear" or "cubic". Default value without user
  specification: "cubic".

- cost_list_of_vectors:

  A list of numeric vectors. Specifies the cost functions for each
  intervention component. Each numeric vector in the list contains
  coefficients of the cost function for one intervention component. For
  example: list(c(1, 2, 3, 4), c(4, 6), c(5, 4, 3)) represents: - First
  component: cost = 1 + 2x_1 + 3x_1^2 + 4x_1^3 - Second component: cost
  = 4 + 6x_2 - Third component: cost = 5 + 4x_3 + 3x_3^2 list(c(0, 2),
  c(0, 6), c(0, 4)) represents: - First component: cost = 2x_1 - Second
  component: cost = 6x_2 - Third component: cost = 4x_3 Default value
  without user specification: NULL. Please note that either the
  unit_costs or cost_list_of_vectors must be specified. If both are
  specified, the cost_list_of_vectors will be used.

- glm_family:

  A character string. The family of the glm() outcome model. Default
  value without user specification: "gaussian" for continuous outcomes,
  "binomial" for binary outcomes.

- link:

  A character string. The link function of the glm() outcome model. Must
  be either "logit" or "identity": those are the links whose inverse the
  estimated outcome and the confidence set are computed with, so they
  are the only ones that can be reported on the outcome scale the goal
  is stated on. Default value without user specification: "identity" for
  continuous outcomes, "logit" for binary outcomes.

- optimization_method:

  A character string. Specifies the method used for LAGO optimization.
  Must be either "numerical" or "grid_search". Default value without
  user specification: "numerical". - Use "grid_search" if you want to
  exhaustively test every possible intervention package compositions in
  LAGO optimization. - Use "numerical" if you want to use gradient-based
  technique in LAGO optimization. Note: if
  optimization_grid_search_step_size is provided while this is
  "numerical", it is automatically switched to "grid_search" (with a
  message).

- confidence_set_alpha:

  A numeric value. The type I error considered in the confidence set
  calculations. Default value without user specification: 0.05.

- weights:

  A numeric vector. The weights argument of the glm() outcome model.

- center_characteristics:

  A character vector. The names of the columns in the dataset that
  represent the center characteristics. Note, include_center_effects and
  center_characteristics cannot be used together at the same time. For
  example: c("characteristic1")

- center_characteristics_optimization_values:

  A numeric vector. The fixed values of the center characteristics at
  which the recommended intervention is computed. Because the outcome
  model can depend on center characteristics, the recommendation is
  specific to a center with these characteristic values, i.e. the
  optimization answers "what is the optimal intervention for a center
  whose characteristics equal these values?". Must be provided when
  center_characteristics is used, must be numeric, and must have the
  same length and order as center_characteristics. For example, with
  center_characteristics = c("birth_volume_100"), setting
  center_characteristics_optimization_values = c(1.75) computes the
  recommended intervention for a center with birth_volume_100 = 1.75.

- main_components:

  A character vector. Specifies the main intervention components in the
  presence of interaction terms. For example: c("component1",
  "component2").

- time_effect_optimization_value:

  A numeric value. The value of the time effect that will be used for
  LAGO optimization.

- additional_covariates:

  A character vector. The names of the columns in the dataset that
  represent additional covaraites that need to be included in the
  outcome model. This includes interaction terms or any other additional
  covariates. For example: c("component2xcomponent4").

- center_weights_for_outcome_goal:

  A numeric vector. Specifies the weights that will be used for
  calculating recommended interventions that satisfy the outcome goal
  for an (weighted) average center. The weights need to sum up to 1, and
  must all be non-negative and finite. A weight of 0 is allowed and
  excludes that center from the average. Default value without user
  specification: For each center, calculate what percentage its sample
  size is of the total samples across all facilities - this percentage
  serves as that center's weight.

- optimization_grid_search_step_size:

  A numeric vector. Specifies the step size of the grid search algorithm
  used in LAGO optimization. This argument is only used by the
  "grid_search" method; if it is provided while optimization_method is
  "numerical", optimization_method is automatically switched to
  "grid_search" (with a message). Default value without user
  specification: 1/20 of the range for each intervention component.

- confidence_set_grid_step_size:

  A numeric vector. Specifies the step size of the grid search algorithm
  used in the confidence set calculation. Default value without user
  specification: 1/20 of the range for each intervention component.

- include_confidence_set:

  A boolean. Specifies whether the confidence set should be calculated
  along with the recommended interventions. Default value without user
  specification: TRUE.

- include_center_effects:

  A boolean. Specifies whether the fixed effects should be included in
  the outcome model. If set to TRUE, please make sure that the input
  data has a 'center' column to identify the centers. Note,
  include_center_effects and center_characteristics cannot be used
  together at the same time. Default value without user specification:
  FALSE for individual level data, TRUE for center level data.

- center_effects_optimization_values:

  A numeric vector. The center of interest that will be used for
  center-specific LAGO optimization. This is only used when
  include_center_effects is set to TRUE. If not specified, the LAGO
  optimization will be carried out for a weighted average of all
  centers.

- include_time_effects:

  A boolean. Specifies whether the fixed time effects should be included
  in the outcome model. If set to TRUE, please make sure that the input
  data has a 'period' column to identify the time periods. Default value
  without user specification: FALSE.

- include_interaction_terms:

  A boolean. Specifies whether there are interaction terms in the
  intervention components. Please make sure the interaciton terms are
  included in "intervention_components", and they follow the correct
  naming scheme: "component1:component3". Default value without user
  specification: FALSE.

- prev_recommended_interventions:

  A numeric vector. The recommended interventions from a previous stage
  of the LAGO optimization. If provided, this will be used in the
  shrinking method (see Section 5.1 of the Supplementary material of
  Nevo et al. (2021) for details).

- shrinkage_threshold:

  A numeric value. The threshold for the shrinkage method to kick in.
  This threshold represents the proportion of the distance between the
  estimated outcome without intervention, and the outcome goal. If the
  maximum reachable outcome is less than this value, the shrinkage
  method from Section 5.1 of the Supplementary Materials of Nevo et
  al. (2021) will be applied. If the maximum reachable outcome is
  greater than this value, then the maximum reachable outcome will be
  used as the outcome goal, and the shrinkage method will not be used.
  Default value without user specification: 0.25.

- quiet:

  A boolean. If TRUE, suppresses the progress messages, the paced delays
  between them, and the final printed output, which speeds up the call
  substantially (the default paced output adds about two seconds of
  artificial delay). The returned value is identical either way. Genuine
  warnings about the data or model fit are still shown. Useful for
  programmatic or repeated calls (for example, simulations). Default
  value without user specification: FALSE.

## Value

List( recommended interventions, associated cost for the interventions,
estimated outcome mean/probability for the intervention group, 95%
confidence set percentage, 95% confidence set, overall intervention test
results (test_results), a list with the test statistic and p-value; NULL
unless a valid 'group' column is supplied. The confidence set fields are
only present when include_confidence_set = TRUE.)

## Examples

``` r
# Basic case showing how to carry out the optimization with
# a built-in data set. The confidence set is skipped here to keep this
# example fast; the \donttest examples below show a full run with one.
lago_optimization(
  data = infert,
  outcome_name = "case",
  outcome_type = "binary",
  glm_family = "binomial",
  intervention_components = c("age", "parity"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(50, 10),
  cost_list_of_vectors = list(c(0, 4), c(0, 1)),
  outcome_goal = 0.5,
  outcome_goal_intention = "maximize",
  include_confidence_set = FALSE
)
#> ℹ Starting LAGO Optimization
#> ℹ Validating inputs...
#> [1] "When 'cost_list_of_vectors' is provided, 'default_cost_fxn_type' is ignored."
#> ✔ Done
#> ℹ Assessing the cost function...
#> ✔ Done
#> ℹ Fitting the outcome model...
#> Warning: The following intervention component(s) do not have a statistically significant association with the outcome (p > 0.05):
#>   - age
#>   - parity
#> The LAGO optimization will still run, but the recommendation for these component(s) is not well supported by the data.
#> ✔ Done
#> ℹ Calculating the recommended intervention...
#> Warning: The outcome goal is not estimated to be achievable. Since the maximum estimated achievable outcome
#>  is less than the outcome goal, we will shrink the recommended intervention towards the
#>  recommended intervention from the previous stage.
#> ✔ Done
#> → ♥ LAGO optimization complete ♥
#> ℹ Printing the output...
#> 
#> ── LAGO optimization result ────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Input data dimensions: 248 rows, 8 columns
#> Outcome name: case
#> Outcome type: binary
#> 2 intervention component(s): age, parity
#> Outcome model family: binomial
#> Outcome model link: logit
#> Fixed center effects: FALSE
#> Fixed time effects: FALSE
#> Outcome goal: 0.5
#> Power goal: not specified
#> Intervention component costs: c(0, 4), c(0, 1)
#> Intervention lower bounds: 0, 0
#> Intervention upper bounds: 50, 10
#> 
#> ── Outcome model fit 
#> 
#> Call:
#> glm(formula = formula, family = family_object, data = data, weights = weights)
#> 
#> Coefficients:
#>              Estimate Std. Error z value Pr(>|z|)
#> (Intercept) -0.753683   0.835836  -0.902    0.367
#> age          0.001137   0.025775   0.044    0.965
#> parity       0.014662   0.107680   0.136    0.892
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 316.17  on 247  degrees of freedom
#> Residual deviance: 316.15  on 245  degrees of freedom
#> AIC: 322.15
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> ── Overall intervention-effect test 
#> To see the overall test results, include a 'group' column in the data with
#> values 'treatment' or 'control' (binary outcomes only).
#> 
#> ── Recommended intervention 
#> age: 31.504
#> parity: 2.0927
#> Cost: 128.1089
#> Estimated outcome: 0.3347
#> 95% CI for the estimated outcome: not available (set include_confidence_set =
#> TRUE)
#> Outcome goal: 0.5
#> 
#> ── Confidence set 
#> Not computed (set include_confidence_set = TRUE to compute it).

# Fuller examples, including the confidence-set grid search, which is
# inherently slower. Wrapped in \donttest{} so they are still checkable
# but do not run on every automated check.
# \donttest{
lago_optimization(
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
  confidence_set_grid_step_size = c(1, 1)
)
#> ℹ Starting LAGO Optimization
#> ℹ Validating inputs...
#> Warning: The lower bound for the intervention component coaching_updt is greater than the minimum value in the data.
#> Warning: The lower bound for the intervention component launch_duration is greater than the minimum value in the data.
#> [1] "When 'cost_list_of_vectors' is provided, 'default_cost_fxn_type' is ignored."
#> ✔ Done
#> ℹ Assessing the cost function...
#> ✔ Done
#> ℹ Fitting the outcome model...
#> ✔ Done
#> ℹ Calculating the recommended intervention...
#> ✔ Done
#> ℹ Calculating the confidence set...
#> If the confidence set calculation takes a long time to run, please consider changing the confidence set step size. 
#> ✔ Done
#> → ♥ LAGO optimization complete ♥
#> ℹ Printing the output...
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
#> Intervention component costs: c(0, 1.7), c(0, 8)
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
#> Cost: 23.9278
#> Estimated outcome: 0.85
#> 95% CI for the estimated outcome: 0.802 - 0.898
#> Outcome goal: 0.85
#> 
#> ── Confidence set 
#> 95% confidence set size: 9% of the grid
#> IQR of the cost within the 95% confidence set: 62.32 - 76.78
#> First rows of the confidence set (use $cs for all):
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

lago_optimization(
  data = BB_proportions,
  outcome_name = "EBP_proportions",
  outcome_type = "continuous",
  glm_family = "quasibinomial",
  link = "logit",
  intervention_components = c("coaching_updt", "launch_duration"),
  center_characteristics = c("birth_volume_100"),
  center_characteristics_optimization_values = 1.75,
  intervention_lower_bounds = c(1, 1),
  intervention_upper_bounds = c(40, 5),
  cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
  outcome_goal = 0.85,
  outcome_goal_intention = "maximize",
  confidence_set_grid_step_size = c(1, 1)
)
#> ℹ Starting LAGO Optimization
#> ℹ Validating inputs...
#> Warning: The lower bound for the intervention component coaching_updt is greater than the minimum value in the data.
#> Warning: The lower bound for the intervention component launch_duration is greater than the minimum value in the data.
#> [1] "When 'cost_list_of_vectors' is provided, 'default_cost_fxn_type' is ignored."
#> ✔ Done
#> ℹ Assessing the cost function...
#> ✔ Done
#> ℹ Fitting the outcome model...
#> ✔ Done
#> ℹ Calculating the recommended intervention...
#> Warning: The outcome goal is not estimated to be achievable. Since the maximum estimated achievable outcome
#>  is less than the outcome goal, we will shrink the recommended intervention towards the
#>  recommended intervention from the previous stage.
#> ✔ Done
#> → ♥ LAGO optimization complete ♥
#> ℹ Printing the output...
#> 
#> ── LAGO optimization result ────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Input data dimensions: 7359 rows, 4 columns
#> Outcome name: EBP_proportions
#> Outcome type: continuous
#> 2 intervention component(s): coaching_updt, launch_duration
#> 1 center characteristic(s): birth_volume_100
#> Outcome model family: quasibinomial
#> Outcome model link: logit
#> Fixed center effects: FALSE
#> Fixed time effects: FALSE
#> Outcome goal: 0.85
#> Power goal: not specified
#> Intervention component costs: c(0, 1.7), c(0, 8)
#> Intervention lower bounds: 1, 1
#> Intervention upper bounds: 40, 5
#> 
#> ── Outcome model fit 
#> 
#> Call:
#> glm(formula = formula, family = family_object, data = data, weights = weights)
#> 
#> Coefficients:
#>                   Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)      -0.138303   0.011848  -11.67   <2e-16 ***
#> coaching_updt     0.034416   0.001382   24.90   <2e-16 ***
#> launch_duration   0.165681   0.015574   10.64   <2e-16 ***
#> birth_volume_100 -0.202380   0.003241  -62.45   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for quasibinomial family taken to be 0.08983316)
#> 
#>     Null deviance: 1486.60  on 7358  degrees of freedom
#> Residual deviance:  735.96  on 7355  degrees of freedom
#> AIC: NA
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> ── Overall intervention-effect test 
#> To see the overall test results, include a 'group' column in the data with
#> values 'treatment' or 'control' (binary outcomes only).
#> 
#> ── Recommended intervention 
#> coaching_updt: 38.9887
#> launch_duration: 4.793
#> Cost: 104.6248
#> Estimated outcome: 0.838
#> 95% CI for the estimated outcome: not available (set include_confidence_set =
#> TRUE)
#> Outcome goal: 0.85
#> 
#> ── Confidence set 
#> Not computed (set include_confidence_set = TRUE to compute it).
# }
```
