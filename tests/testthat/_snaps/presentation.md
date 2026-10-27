# print/summary snapshot: binary object with a confidence set

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 6124 rows, 21 columns
      Outcome name: pp3_oxytocin_mother
      Outcome type: binary
      2 intervention component(s): coaching_updt, launch_duration
      1 center characteristic(s): birth_volume_100
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 0.85
      Power goal: not specified
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
                       Estimate Std. Error
      (Intercept)      -2.29989   0.068371
      coaching_updt     0.02514   0.006112
      launch_duration   1.02447   0.074135
      birth_volume_100  0.66451   0.029627
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23928
      Estimated outcome: 0.85
      95% CI for the estimated outcome: 0.802 - 0.898
      Outcome goal: 0.85
      
      -- Confidence set 
      95% confidence set size: 10.56% of the grid
      IQR of the cost within the 95% confidence set: 31075 - 69975
      First rows of the confidence set (use $cs for all):
    Output
          coaching_updt launch_duration birth_volume_100 CI_lower_bound
      81             40             1.5             1.75          0.755
      108            27             2.0             1.75          0.811
      109            28             2.0             1.75          0.814
      110            29             2.0             1.75          0.816
      111            30             2.0             1.75          0.819
      112            31             2.0             1.75          0.822
          CI_upper_bound  cost
      81           0.851 80000
      108          0.851 61900
      109          0.855 63600
      110          0.859 65300
      111          0.863 67000
      112          0.867 68700

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 6124 rows, 21 columns
      Outcome name: pp3_oxytocin_mother
      Outcome type: binary
      2 intervention component(s): coaching_updt, launch_duration
      1 center characteristic(s): birth_volume_100
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 0.85
      Power goal: not specified
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
                       Estimate Std. Error
      (Intercept)      -2.29989   0.068371
      coaching_updt     0.02514   0.006112
      launch_duration   1.02447   0.074135
      birth_volume_100  0.66451   0.029627
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23928
      Estimated outcome: 0.85
      95% CI for the estimated outcome: 0.802 - 0.898
      Outcome goal: 0.85
      
      -- Confidence set 
      95% confidence set size: 10.56% of the grid
      IQR of the cost within the 95% confidence set: 31075 - 69975
      First rows of the confidence set (use $cs for all):
    Output
          coaching_updt launch_duration birth_volume_100 CI_lower_bound
      81             40             1.5             1.75          0.755
      108            27             2.0             1.75          0.811
      109            28             2.0             1.75          0.814
      110            29             2.0             1.75          0.816
      111            30             2.0             1.75          0.819
      112            31             2.0             1.75          0.822
          CI_upper_bound  cost
      81           0.851 80000
      108          0.851 61900
      109          0.855 63600
      110          0.859 65300
      111          0.863 67000
      112          0.867 68700

# print/summary snapshot: continuous object

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 32 rows, 11 columns
      Outcome name: mpg
      Outcome type: continuous
      2 intervention component(s): gear, qsec
      Outcome model family: gaussian
      Outcome model link: identity
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 40
      Power goal: not specified
      Intervention component costs: c(0, 4), c(4, 6)
      Intervention lower bounds: 0, 0
      Intervention upper bounds: 10, 350
      
      -- Outcome model fit 
    Output
                  Estimate Std. Error
      (Intercept)  -30.711     9.6702
      gear           4.871     1.0814
      qsec           1.840     0.4465
    Message
      Standard errors are model based, as the confidence set uses them.
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      gear: 10
      qsec: 11.9574
      Cost: 115.74
      Estimated outcome: 40
      95% CI for the estimated outcome: 26.64 - 53.36
      Outcome goal: 40
      
      -- Confidence set 
      95% confidence set size: 4.25% of the grid
      IQR of the cost within the 95% confidence set: 155.5 - 242.5
      First rows of the confidence set (use $cs for all):
    Output
         gear qsec CI_lower_bound CI_upper_bound cost
      45   10    3          6.902         40.137   62
      56   10    4          9.261         41.458   68
      67   10    5         11.589         42.810   74
      78   10    6         13.881         44.197   80
      89   10    7         16.136         45.622   86
      99    9    8         15.118         40.577   88

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 32 rows, 11 columns
      Outcome name: mpg
      Outcome type: continuous
      2 intervention component(s): gear, qsec
      Outcome model family: gaussian
      Outcome model link: identity
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 40
      Power goal: not specified
      Intervention component costs: c(0, 4), c(4, 6)
      Intervention lower bounds: 0, 0
      Intervention upper bounds: 10, 350
      
      -- Outcome model fit 
    Output
                  Estimate Std. Error
      (Intercept)  -30.711     9.6702
      gear           4.871     1.0814
      qsec           1.840     0.4465
    Message
      Standard errors are model based, as the confidence set uses them.
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      gear: 10
      qsec: 11.9574
      Cost: 115.74
      Estimated outcome: 40
      95% CI for the estimated outcome: 26.64 - 53.36
      Outcome goal: 40
      
      -- Confidence set 
      95% confidence set size: 4.25% of the grid
      IQR of the cost within the 95% confidence set: 155.5 - 242.5
      First rows of the confidence set (use $cs for all):
    Output
         gear qsec CI_lower_bound CI_upper_bound cost
      45   10    3          6.902         40.137   62
      56   10    4          9.261         41.458   68
      67   10    5         11.589         42.810   74
      78   10    6         13.881         44.197   80
      89   10    7         16.136         45.622   86
      99    9    8         15.118         40.577   88

# print/summary snapshot: power-goal object without a confidence set

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 6124 rows, 22 columns
      Outcome name: pp3_oxytocin_mother
      Outcome type: binary
      2 intervention component(s): coaching_updt, launch_duration
      1 center characteristic(s): birth_volume_100
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: not specified
      Power goal: 0.8
      Effective outcome goal (max of outcome goal and power-implied outcome): 0.2982
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
                       Estimate Std. Error
      (Intercept)      -2.29989   0.068371
      coaching_updt     0.02514   0.006112
      launch_duration   1.02447   0.074135
      birth_volume_100  0.66451   0.029627
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      Two-sample test for the difference in two proportions:
      Test statistic: 56.4253
      P-value: 0
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 1
      Cost: 9700
      Estimated outcome: 0.4782
      95% CI for the estimated outcome: not available (set include_confidence_set =
      TRUE)
      
      -- Confidence set 
      Not computed (set include_confidence_set = TRUE to compute it).

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 6124 rows, 22 columns
      Outcome name: pp3_oxytocin_mother
      Outcome type: binary
      2 intervention component(s): coaching_updt, launch_duration
      1 center characteristic(s): birth_volume_100
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: not specified
      Power goal: 0.8
      Effective outcome goal (max of outcome goal and power-implied outcome): 0.2982
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
                       Estimate Std. Error
      (Intercept)      -2.29989   0.068371
      coaching_updt     0.02514   0.006112
      launch_duration   1.02447   0.074135
      birth_volume_100  0.66451   0.029627
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      Two-sample test for the difference in two proportions:
      Test statistic: 56.4253
      P-value: 0
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 1
      Cost: 9700
      Estimated outcome: 0.4782
      95% CI for the estimated outcome: not available (set include_confidence_set =
      TRUE)
      
      -- Confidence set 
      Not computed (set include_confidence_set = TRUE to compute it).

# print/summary snapshot: single-component object

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 16 rows, 2 columns
      Outcome name: y
      Outcome type: binary
      1 intervention component(s): dose
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 0.6
      Power goal: not specified
      Intervention component costs: c(0, 1)
      Intervention lower bounds: 0
      Intervention upper bounds: 3
      
      -- Outcome model fit 
    Output
                  Estimate Std. Error
      (Intercept)   -4.120      2.103
      dose           2.747      1.285
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      dose: 1.6476
      Cost: 1.6476
      Estimated outcome: 0.6
      95% CI for the estimated outcome: 0.195 - 1
      Outcome goal: 0.6
      
      -- Confidence set 
      95% confidence set size: 25% of the grid
      IQR of the cost within the 95% confidence set: 2 - 2
      First rows of the confidence set (use $cs for all):
    Output
        dose CI_lower_bound CI_upper_bound cost
      4    2          0.464              1    2

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 16 rows, 2 columns
      Outcome name: y
      Outcome type: binary
      1 intervention component(s): dose
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 0.6
      Power goal: not specified
      Intervention component costs: c(0, 1)
      Intervention lower bounds: 0
      Intervention upper bounds: 3
      
      -- Outcome model fit 
    Output
                  Estimate Std. Error
      (Intercept)   -4.120      2.103
      dose           2.747      1.285
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      dose: 1.6476
      Cost: 1.6476
      Estimated outcome: 0.6
      95% CI for the estimated outcome: 0.195 - 1
      Outcome goal: 0.6
      
      -- Confidence set 
      95% confidence set size: 25% of the grid
      IQR of the cost within the 95% confidence set: 2 - 2
      First rows of the confidence set (use $cs for all):
    Output
        dose CI_lower_bound CI_upper_bound cost
      4    2          0.464              1    2

# print/summary snapshot: object without a confidence set

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 6124 rows, 21 columns
      Outcome name: pp3_oxytocin_mother
      Outcome type: binary
      2 intervention component(s): coaching_updt, launch_duration
      1 center characteristic(s): birth_volume_100
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 0.85
      Power goal: not specified
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
                       Estimate Std. Error
      (Intercept)      -2.29989   0.068371
      coaching_updt     0.02514   0.006112
      launch_duration   1.02447   0.074135
      birth_volume_100  0.66451   0.029627
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23928
      Estimated outcome: 0.85
      95% CI for the estimated outcome: not available (set include_confidence_set =
      TRUE)
      Outcome goal: 0.85
      
      -- Confidence set 
      Not computed (set include_confidence_set = TRUE to compute it).

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Inputs 
      Input data dimensions: 6124 rows, 21 columns
      Outcome name: pp3_oxytocin_mother
      Outcome type: binary
      2 intervention component(s): coaching_updt, launch_duration
      1 center characteristic(s): birth_volume_100
      Outcome model family: binomial
      Outcome model link: logit
      Fixed center effects: FALSE
      Fixed time effects: FALSE
      Outcome goal: 0.85
      Power goal: not specified
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
                       Estimate Std. Error
      (Intercept)      -2.29989   0.068371
      coaching_updt     0.02514   0.006112
      launch_duration   1.02447   0.074135
      birth_volume_100  0.66451   0.029627
    Message
      Standard errors are those of the fitted glm().
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23928
      Estimated outcome: 0.85
      95% CI for the estimated outcome: not available (set include_confidence_set =
      TRUE)
      Outcome goal: 0.85
      
      -- Confidence set 
      Not computed (set include_confidence_set = TRUE to compute it).

