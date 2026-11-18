# A recommendation for a center outside the centers in the data is an
# extrapolation, and lago_optimization() says so.

extrapolation_call <- function(value, ...) {
  lago_optimization(
    data = BB_data[BB_data$pilot_phase == 1, ], outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"), intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5), center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = value, cost_list_of_vectors = list(c(0, 380, -24, 0.6), c(0, 1700, -950, 220)),
    outcome_goal = 0.85, outcome_goal_intention = "maximize", include_confidence_set = FALSE, quiet = TRUE, ...
  )
}

extrapolation_warnings <- function(expr) {
  w <- list()
  withCallingHandlers(expr, warning = function(x) {
    w[[length(w) + 1]] <<- x
    invokeRestart("muffleWarning")
  })
  Filter(function(x) grepl("outside the centers in the data", conditionMessage(x)), w)
}

test_that("a center characteristic outside the data gives one input warning that names it", {
  w <- extrapolation_warnings(extrapolation_call(2.86))
  expect_length(w, 1)
  expect_s3_class(w[[1]], "lago_input_warning")
  expect_match(conditionMessage(w[[1]]), "birth_volume_100 = 2.86, while the data has 0.95 to 1.3")
  expect_match(conditionMessage(w[[1]]), "an extrapolation of the model")
  # below the data too, and nothing within it or at its edge
  expect_length(extrapolation_warnings(extrapolation_call(0.5)), 1)
  expect_length(extrapolation_warnings(extrapolation_call(1.3)), 0)
  expect_length(extrapolation_warnings(extrapolation_call(1.1)), 0)
})

test_that("each characteristic outside the data is named", {
  d <- BB_data[BB_data$pilot_phase == 1, ]
  d$staff <- seq_len(nrow(d)) / 10
  w <- extrapolation_warnings(lago_optimization(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = "coaching_updt", intervention_lower_bounds = 1, intervention_upper_bounds = 40,
    center_characteristics = c("birth_volume_100", "staff"), center_characteristics_optimization_values = c(2, 100),
    unit_costs = 1, outcome_goal = 0.85, outcome_goal_intention = "maximize", include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_length(w, 1)
  expect_match(conditionMessage(w[[1]]), "birth_volume_100 = 2, while the data has 0.95 to 1.3, and staff = 100, while the data has 0.1 to 7.3")
})

test_that("a sweep gives the warning once, not once per run", {
  skip_on_cran()
  r <- suppressWarnings(extrapolation_call(2.86))
  w <- extrapolation_warnings(lago_sensitivity(r, parameter = "outcome_goal", values = c(0.8, 0.85, 0.9)))
  expect_length(w, 1)
})
