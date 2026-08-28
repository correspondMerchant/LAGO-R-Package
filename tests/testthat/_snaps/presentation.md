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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
      (Intercept)      -2.299892   0.068371 -33.638  < 2e-16 ***
      coaching_updt     0.025137   0.006112   4.113 3.91e-05 ***
      launch_duration   1.024470   0.074135  13.819  < 2e-16 ***
      birth_volume_100  0.664511   0.029627  22.429  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 8470.8  on 6123  degrees of freedom
      Residual deviance: 5161.2  on 6120  degrees of freedom
      AIC: 5169.2
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23927.7723
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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
      (Intercept)      -2.299892   0.068371 -33.638  < 2e-16 ***
      coaching_updt     0.025137   0.006112   4.113 3.91e-05 ***
      launch_duration   1.024470   0.074135  13.819  < 2e-16 ***
      birth_volume_100  0.664511   0.029627  22.429  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 8470.8  on 6123  degrees of freedom
      Residual deviance: 5161.2  on 6120  degrees of freedom
      AIC: 5169.2
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23927.7723
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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept) -30.7108     9.6702  -3.176 0.003530 ** 
      gear          4.8711     1.0814   4.505 0.000100 ***
      qsec          1.8399     0.4465   4.121 0.000288 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for gaussian family taken to be 18.84028)
      
          Null deviance: 1126.05  on 31  degrees of freedom
      Residual deviance:  546.37  on 29  degrees of freedom
      AIC: 189.61
      
      Number of Fisher Scoring iterations: 2
      
    Message
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      gear: 10
      qsec: 11.9574
      Cost: 115.7446
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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept) -30.7108     9.6702  -3.176 0.003530 ** 
      gear          4.8711     1.0814   4.505 0.000100 ***
      qsec          1.8399     0.4465   4.121 0.000288 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for gaussian family taken to be 18.84028)
      
          Null deviance: 1126.05  on 31  degrees of freedom
      Residual deviance:  546.37  on 29  degrees of freedom
      AIC: 189.61
      
      Number of Fisher Scoring iterations: 2
      
    Message
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      gear: 10
      qsec: 11.9574
      Cost: 115.7446
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
      Effective outcome goal (max of outcome goal and power-implied outcome): 0.2986
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
      (Intercept)      -2.299892   0.068371 -33.638  < 2e-16 ***
      coaching_updt     0.025137   0.006112   4.113 3.91e-05 ***
      launch_duration   1.024470   0.074135  13.819  < 2e-16 ***
      birth_volume_100  0.664511   0.029627  22.429  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 8470.8  on 6123  degrees of freedom
      Residual deviance: 5161.2  on 6120  degrees of freedom
      AIC: 5169.2
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
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
      Effective outcome goal (max of outcome goal and power-implied outcome): 0.2986
      Intervention component costs: c(0, 1700), c(0, 8000)
      Intervention lower bounds: 1, 1
      Intervention upper bounds: 40, 5
      
      -- Outcome model fit 
    Output
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
      (Intercept)      -2.299892   0.068371 -33.638  < 2e-16 ***
      coaching_updt     0.025137   0.006112   4.113 3.91e-05 ***
      launch_duration   1.024470   0.074135  13.819  < 2e-16 ***
      birth_volume_100  0.664511   0.029627  22.429  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 8470.8  on 6123  degrees of freedom
      Residual deviance: 5161.2  on 6120  degrees of freedom
      AIC: 5169.2
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)  
      (Intercept)   -4.120      2.103  -1.959   0.0501 .
      dose           2.747      1.285   2.137   0.0326 *
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 22.1807  on 15  degrees of freedom
      Residual deviance:  9.3631  on 14  degrees of freedom
      AIC: 13.363
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)  
      (Intercept)   -4.120      2.103  -1.959   0.0501 .
      dose           2.747      1.285   2.137   0.0326 *
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 22.1807  on 15  degrees of freedom
      Residual deviance:  9.3631  on 14  degrees of freedom
      AIC: 13.363
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
      (Intercept)      -2.299892   0.068371 -33.638  < 2e-16 ***
      coaching_updt     0.025137   0.006112   4.113 3.91e-05 ***
      launch_duration   1.024470   0.074135  13.819  < 2e-16 ***
      birth_volume_100  0.664511   0.029627  22.429  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 8470.8  on 6123  degrees of freedom
      Residual deviance: 5161.2  on 6120  degrees of freedom
      AIC: 5169.2
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23927.7723
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
      
      Call:
      glm(formula = formula, family = family_object, data = data, weights = weights)
      
      Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
      (Intercept)      -2.299892   0.068371 -33.638  < 2e-16 ***
      coaching_updt     0.025137   0.006112   4.113 3.91e-05 ***
      launch_duration   1.024470   0.074135  13.819  < 2e-16 ***
      birth_volume_100  0.664511   0.029627  22.429  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 8470.8  on 6123  degrees of freedom
      Residual deviance: 5161.2  on 6120  degrees of freedom
      AIC: 5169.2
      
      Number of Fisher Scoring iterations: 6
      
    Message
      
      -- Overall intervention-effect test 
      To see the overall test results, include a 'group' column in the data with
      values 'treatment' or 'control' (binary outcomes only).
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23927.7723
      Estimated outcome: 0.85
      95% CI for the estimated outcome: not available (set include_confidence_set =
      TRUE)
      Outcome goal: 0.85
      
      -- Confidence set 
      Not computed (set include_confidence_set = TRUE to compute it).

