# print/summary snapshot: binary object with a confidence set

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23.9278
      Estimated outcome: 0.85
      Outcome goal: 0.85
      95% confidence set size: 10.56% of the grid
      Use summary() for the confidence set and test detail, plot() to visualize.

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23.9278
      Estimated outcome: 0.85
      Outcome goal: 0.85
      95% confidence set size: 10.56% of the grid
      
      -- Confidence set 
      Cost range in the 95% confidence set: 21.7 - 84
      First rows of the confidence set:
    Output
          coaching_updt launch_duration birth_volume_100 CI_lower_bound
      81             40             1.5             1.75          0.755
      108            27             2.0             1.75          0.811
      109            28             2.0             1.75          0.814
      110            29             2.0             1.75          0.816
      111            30             2.0             1.75          0.819
      112            31             2.0             1.75          0.822
          CI_upper_bound cost
      81           0.851 80.0
      108          0.851 61.9
      109          0.855 63.6
      110          0.859 65.3
      111          0.863 67.0
      112          0.867 68.7

# print/summary snapshot: continuous object

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      gear: 10
      qsec: 11.9574
      Cost: 115.7446
      Estimated outcome: 40
      Outcome goal: 40
      95% confidence set size: 4.25% of the grid
      Use summary() for the confidence set and test detail, plot() to visualize.

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      gear: 10
      qsec: 11.9574
      Cost: 115.7446
      Estimated outcome: 40
      Outcome goal: 40
      95% confidence set size: 4.25% of the grid
      
      -- Confidence set 
      Cost range in the 95% confidence set: 62 - 340
      First rows of the confidence set:
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
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 1
      Cost: 9.7
      Estimated outcome: 0.4782
      Power goal: 0.8
      Overall intervention test: p = 0
      Use summary() for the confidence set and test detail, plot() to visualize.

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 1
      Cost: 9.7
      Estimated outcome: 0.4782
      Power goal: 0.8
      
      -- Overall intervention-effect test 
      test statistic = 56.4253, p = 0

# print/summary snapshot: single-component object

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      dose: 1.6476
      Cost: 1.6476
      Estimated outcome: 0.6
      Outcome goal: 0.6
      95% confidence set size: 25% of the grid
      Use summary() for the confidence set and test detail, plot() to visualize.

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      dose: 1.6476
      Cost: 1.6476
      Estimated outcome: 0.6
      Outcome goal: 0.6
      95% confidence set size: 25% of the grid
      
      -- Confidence set 
      Cost range in the 95% confidence set: 2 - 2
      First rows of the confidence set:
    Output
        dose CI_lower_bound CI_upper_bound cost
      4    2          0.464          1.132    2

# print/summary snapshot: object without a confidence set

    Code
      print(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23.9278
      Estimated outcome: 0.85
      Outcome goal: 0.85
      Use summary() for the confidence set and test detail, plot() to visualize.

---

    Code
      summary(r)
    Message
      
      -- LAGO optimization result ----------------------------------------------------
      
      -- Recommended intervention 
      coaching_updt: 1
      launch_duration: 2.7785
      Cost: 23.9278
      Estimated outcome: 0.85
      Outcome goal: 0.85

