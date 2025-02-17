
<p align="center">
  <img src="./images/banner.png" width="800" alt="Banner">
</p>

The LAGO R package bridges the gap between theoretical advances in Learn-As-you-GO (LAGO) and practical applications by providing a standardized solution for:
1) fitting the outcome models for both binary and continuous outcomes, including support for fixed center effects/center characteristics and fixed time effects,
2) calculating the recommended interventions based on various optimization criteria, including support for custom cost functions,
3) estimating the optimal intervention based on data from all stages,
4) calculating the 95\% confidence sets for the recommended interventions and the optimal interventions.

### Table of Contents
1. [How to install the R package](#how-to-install-the-r-package)
2. [The main functions](#the-main-functions)
3. [Basic use case](#basic-use-case)
4. [More advanced use case](#more-advanced-use-case)
5. [Additional examples in the R package](#how-to-run-additional-examples)
6. [Relevant LAGO papers](#relevant-lago-papers)
7. [How to get help](#how-to-get-help)


### How to install the R package 
- Method 1 (directly using RStudio):
  ```
  install.packages("devtools")
  devtools::install_github("https://github.com/correspondMerchant/LAGO-R-Package")
  ```
- Method 2: Clone this repo into RStudio, you can follow the directions provided [in this video](https://www.youtube.com/watch?v=NInwldFZgwA&t=275s).

### The main functions 
The LAGO R package has two user-facing functions `lago_optimization()` and `visualize_cost()`. 

`lago_optimization()` carries out the LAGO optimizations, and `visualize_cost()` helps users to determine the cost function of intervention components.

To get a better understanding of the input arguments, please take a look at the help files:
```
help(lago_optimization)
help(visualize_cost)
```

### Basic use case
We consider a hypothetical example based on a very well-known R built-in dataset: 'mtcars' to showcase how the LAGO package works. This example may not make practical sense, it is intended to be an example that describes how to run LAGO optimization for a real-world dataset.  

The 'mtcars' data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models).
Here, we only focus on the following variables: 'mpg' - miles per gallon, 'gear' - number of forward gears, and 'qsec' - quarter mile time in seconds.

Suppose that 'mpg' is our outcome of interest, and 'gear' and 'qsec' are the two intervention components. We are interested in estimating the optimal intervention package (values of 'gear' and 'qsec') that is expected to achieve an outcome goal of 40 miles per gallon. We expect that the estimated outcome without any intervention is going to be less than 40, and implementing the two intervention components can increase the value of the outcome (which corresponds to setting `outcome_goal_intention = "maximize"`). We are also interested in obtaining the 95% confidence set, which is a list of intervention package compositions that can be expected to include the optimal intervention in 95% of such trials. For the confidence set, we are only interested in the integer values of the intervention components, which corresponds to setting `confidence_set_grid_step_size = c(1, 1)`. 

Since 'mpg' is a continuous variable, we can fit a linear regression between the outcome 'mpg' and the predictors 'gear' and 'qsec'. Then, suppose that we know the lower and upper bounds of 'gear' and 'qsec' are: 0 <= 'gear' <= 10 and 0 <= 'qsec' <= 350, and the total monetary cost of implementing the 'gear' ($x_1$) and 'qsec' ($x_2$) are $C(x_1) = 4x_1$, and $C(x_2) = 4 + 6x_2$, respectively. 

For running LAGO optimizations:
```
results <- lago_optimization(
  data = mtcars,
  outcome_name = "mpg",
  outcome_type = "continuous",
  glm_family = "gaussian",
  link = "identity",
  intervention_components = c("gear", "qsec"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 350),
  cost_list_of_vectors = list(c(0, 4), c(4, 6)),
  outcome_goal = 40,
  outcome_goal_intention = "maximize",
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
Input data dimensions: 32 rows, and 13 columns 
Outcome name: mpg 
Outcome type: continuous 
2 intervention package component(s): 
         gear
         qsec 
The outcome model: 
         family: gaussian 
         link: identity 
         fixed center effects: FALSE 
         fixed time effects: FALSE 
Outcome goal: 40 
List of intervention component costs: c(0, 4), c(4, 6) 
Intervention lower bounds: 0 0 
Intervention upper bounds: 10 350 

=====================================
============  Model Fit  ============
=====================================

Call:
glm(formula = formula, family = family_object, data = data, weights = weights)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -30.7108     9.6702  -3.176 0.003530 ** 
gear          4.8711     1.0814   4.505 0.000100 ***
qsec          1.8399     0.4465   4.121 0.000288 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 18.84028)

    Null deviance: 1126.05  on 31  degrees of freedom
Residual deviance:  546.37  on 29  degrees of freedom
AIC: 189.61

Number of Fisher Scoring iterations: 2


===================================================
===========  Recommended Interventions  ===========
===================================================
 component    value
      gear 10.00000
      qsec 11.95743

Cost for using the recommended interventions: 115.7446 
Estimated outcome goal using the recommended interventions: 40 

========================================
============ Confidence Set ============
========================================
Confidence set size percentage: 0.04247604 
Confidence set (only first few rows are shown): 
Please use $cs to get the full confidence set. 
   gear qsec CI_lower_bound CI_upper_bound cost
44   10    3          6.902         40.137   62
55   10    4          9.261         41.458   68
66   10    5         11.589         42.810   74
77   10    6         13.881         44.197   80
88   10    7         16.136         45.622   86
98    9    8         15.118         40.577   88
```
From the output above, we see that both 'gear' and 'qsec' have positive effects on 'mpg', and the optimal intervention turns out to be 'gear' = 10, and 'qsec' = 11.96. 


You may not be satisfied with the current cost function $C(x_1) = 4x_1$, and $C(x_2) = 4 + 6x_2$. 
The LAGO R package offers an intuitive way to help you visualize and select cost functions:

The `visualize_cost` function creates a Shiny app that allows the user to adjust the coefficients of the cost functions for each intervention component and visualize the resulting total cost function and unit cost function. The initial coefficients are calculated based on the unit costs, the default cost function type (linear or cubic), and the lower and upper bounds. 

The user can adjust the coefficients using sliders and reset them to their initial values. The app also displays the current coefficient vector for each component. The user can copy the final coefficient list (at the bottom of the app) for use in the optimization function lago_optimization().

```
visualize_cost(
  component_names = c("Component 1", "Component 2"),
  unit_costs = c(0.5, 1),
  default_cost_fxn_type = "linear",
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 10)
)
```
The following gif shows the expected behavior of the R shiny app 

**(please wait a few seconds for the gif to load)**
![screenshot of the cost function r shiny app basic example](./images/shiny_2_14_2025.gif)


### More advanced use case


### How to run additional examples
  All of the examples are located in the [manual tests](https://github.com/correspondMerchant/LAGO-R-Package/tree/main/manual_tests) folder as Rmd files.
  You can start with the simpler ones first, like the [identity link with built-in dataset](https://github.com/correspondMerchant/LAGO-R-Package/blob/main/manual_tests/test_rec_int_for_cts_identity.Rmd), or the [logistic link with the BetterBirth dataset](https://github.com/correspondMerchant/LAGO-R-Package/blob/main/manual_tests/test_rec_int_for_BB_data.Rmd) before moving on to other files.


### Relevant LAGO papers
1. [Nevo D, Lok JJ, Spiegelman D. ANALYSIS OF "LEARN-AS-YOU-GO" (LAGO) STUDIES. Ann Stat. 2021 Apr;49(2):793-819. doi: 10.1214/20-aos1978. Epub 2021 Apr 2. PMID: 35510045; PMCID: PMC9067111.](https://pmc.ncbi.nlm.nih.gov/articles/PMC9067111/pdf/nihms-1761299.pdf)
2. [Bing A, Spiegelman D, Nevo D, Lok JJ. Learn-As-you-GO (LAGO) Trials: Optimizing Treatments and Preventing Trial Failure Through Ongoing Learning. arXiv preprint arXiv:2307.06552. 2023 Jul 13.](https://arxiv.org/pdf/2307.06552)

### How to get help 
Before reaching out for help, please carefully review this readme file, examine the descriptions of the arguments in R help files, run the Rmd files in the [manual tests](https://github.com/correspondMerchant/LAGO-R-Package/tree/main/manual_tests) folder, and read relevant LAGO papers. 

Reach out to [Ante Bing](mailto:abing@bu.edu) or [Minh Bui](mailto:minhb@bu.edu) if you still have questions.




