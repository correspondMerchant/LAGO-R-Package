
<p align="center">
  <img src="./images/banner.png" width="800" alt="Banner">
</p>

The LAGO R package bridges the gap between theoretical advances of LAGO and practical applications by providing a standardized solution for:
1) fitting the outcome models for both binary and continuous outcomes, including support for fixed center effects/center characteristics and fixed time effects,
2) calculating the recommended interventions based on various optimization criteria, including support for custom cost functions,
3) estimating the optimal intervention based on data from all stages,
4) calculating the 95\% confidence sets for the recommended interventions and the optimal interventions.

### How to install the package 
- Method 1 (directly using RStudio):
  ```
  install.packages("devtools")
  devtools::install_github("https://github.com/correspondMerchant/LAGO-R-Package")
  ```
- Method 2: Clone this repo into RStudio, you can follow the directions provided [in this video](https://www.youtube.com/watch?v=NInwldFZgwA&t=275s).

### The main functions 
THe LAGO R package has two user-facing functions `lago_otpimization()` and `visualize_cost()`. 

`lago_optimization()` carries out the LAGO optimizations, and `visualize_cost()` helps users to determine the cost function of intervention components.

### Basic use cases
For running LAGO optimizations:
```
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
  confidence_set_grid_step_size = c(1, 1)
)
```
Typical output:
```
ℹ Starting LAGO Optimization
ℹ Validating inputs...
[1] "When 'cost_list_of_vectors' is provided, 'default_cost_fxn_type' is ignored."
✔ Done
ℹ Assessing the cost function...
✔ Done
ℹ Fitting the outcome model...
✔ Done
ℹ Calculating the recommended intervention...
✔ Done
ℹ Calculating the confidence set...
If the confidence set calculation takes a long time to run, please consider changing the confidence set step size. 
✔ Done
→ ♥ LAGO optimization complete ♥
ℹ Printing the output...

==================================
============  Inputs  ============
==================================
Input data dimensions: 248 rows, and 8 columns 
Outcome name: case 
Outcome type: binary 
2 intervention package component(s): 
         age
         parity 
The outcome model: 
         family: binomial 
         link: logit 
         fixed center effects: FALSE 
         fixed time effects: FALSE 
Outcome goal: 0.5 
List of intervention component costs: c(0, 4), c(0, 1) 

=====================================
============  Model Fit  ============
=====================================

Call:
glm(formula = formula, family = family_object, data = data, weights = weights)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)
(Intercept) -0.753683   0.835836  -0.902    0.367
age          0.001137   0.025775   0.044    0.965
parity       0.014662   0.107680   0.136    0.892

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 316.17  on 247  degrees of freedom
Residual deviance: 316.15  on 245  degrees of freedom
AIC: 322.15

Number of Fisher Scoring iterations: 4


===================================================
===========  Recommended Interventions  ===========
===================================================
 component    value
       age 25.38815
    parity 10.00000

Cost for using the recommended interventions: 111.5526 
Estimated outcome goal using the recommended interventions: 0.3593491 

========================================
============ Confidence Set ============
========================================
Confidence set size percentage: 0.7647059 
Confidence set (only first few rows are shown): 
Please use $cs to get the full confidence set. 
  age parity CI_lower_bound CI_upper_bound cost
1   0      0         -0.036          0.677    0
2   1      0         -0.026          0.666    4
3   2      0         -0.015          0.656    8
4   3      0         -0.005          0.647   12
5   4      0          0.005          0.637   16
6   5      0          0.016          0.627   20
```

For visualizing cost functions:
```
visualize_cost(
  component_names = c("Component 1", "Component 2"),
  unit_costs = c(0.5, 1),
  default_cost_fxn_type = "linear",
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 10)
)
```
![screenshot of the cost function r shiny app basic example](./images/rshiny_screenshot.png)

### How to run additional examples
  All of the examples are located in the [manual tests](https://github.com/correspondMerchant/LAGO-R-Package/tree/main/manual_tests) folder as Rmd files.
  You can start with the simpler ones first, like the [identity link with built-in dataset](https://github.com/correspondMerchant/LAGO-R-Package/blob/main/manual_tests/test_rec_int_for_cts_identity.Rmd), or the [logistic link with the BetterBirth dataset](https://github.com/correspondMerchant/LAGO-R-Package/blob/main/manual_tests/test_rec_int_for_BB_data.Rmd) before moving on to other files.


### Citations 


### How to get help 
Reach out to [Ante Bing](mailto:abing@bu.edu) or [Minh Bui](mailto:minhb@bu.edu) if you have questions.




