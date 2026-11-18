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
  expect_match(conditionMessage(w[[1]]), "birth_volume_100 = 2.86, while the data the model is fitted on has 0.95 to 1.3")
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
  expect_match(conditionMessage(w[[1]]), "birth_volume_100 = 2, while the data the model is fitted on has 0.95 to 1.3, and staff = 100, while the data the model is fitted on has 0.1 to 7.3")
})

test_that("a sweep gives the warning once, not once per run", {
  skip_on_cran()
  r <- suppressWarnings(extrapolation_call(2.86))
  w <- extrapolation_warnings(lago_sensitivity(r, parameter = "outcome_goal", values = c(0.8, 0.85, 0.9)))
  expect_length(w, 1)
})

test_that("the range is over the rows the model is fitted on", {
  d <- as.data.frame(BB_data)
  # the largest centers have no outcome, so the model never sees them
  d$pp3_oxytocin_mother[d$birth_volume_100 == 7.5] <- NA
  w <- extrapolation_warnings(lago_optimization(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"), intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5), center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 7.5, cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.85, include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_length(w, 1)
  expect_match(conditionMessage(w[[1]]), "birth_volume_100 = 7.5, while the data the model is fitted on has 0.6025 to 3.980833333")
  # a row with a weight of 0 is not fitted either
  d <- as.data.frame(BB_data)
  w <- extrapolation_warnings(lago_optimization(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"), intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5), center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 7.5, cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.85, include_confidence_set = FALSE, quiet = TRUE, weights = ifelse(d$birth_volume_100 == 7.5, 0, 1)
  ))
  expect_length(w, 1)
})

test_that("a value that names no center is refused", {
  for (v in list(Inf, -Inf, NA_real_, NaN)) {
    expect_error(extrapolation_call(v), "must be finite numbers, one per center characteristic")
  }
})

test_that("true or false and text characteristics are read as their 0 or 1 dummy", {
  d <- BB_data[BB_data$pilot_phase == 1, ]
  d$big <- d$birth_volume_100 > 1.2
  d$size <- ifelse(d$big, "large", "small")
  for (col in c("big", "size")) {
    run <- function(v) extrapolation_warnings(lago_optimization(
      data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
      intervention_components = "coaching_updt", intervention_lower_bounds = 1, intervention_upper_bounds = 40,
      center_characteristics = col, center_characteristics_optimization_values = v, unit_costs = 1,
      outcome_goal = 0.85, include_confidence_set = FALSE, quiet = TRUE
    ))
    w <- run(5)
    expect_length(w, 1)
    expect_match(conditionMessage(w[[1]]), sprintf("%s = 5, while the data the model is fitted on has 0 to 1", col))
    expect_length(run(1), 0)
  }
})

test_that("a value within floating point of an end does not warn, and the warning never hides an input error", {
  expect_length(extrapolation_warnings(extrapolation_call(1.3 + 1e-12)), 0)
  expect_length(extrapolation_warnings(extrapolation_call(0.95 - 1e-12)), 0)
  p <- as.data.frame(main_pulesa_data)
  p$proportion <- p$Success / p$Total_visit
  p$center_sample_size <- p$Total_visit
  p$center <- p$Clinic
  withr_warn <- options(warn = 2)
  on.exit(options(withr_warn), add = TRUE)
  expect_error(
    lago_optimization(
      data = p, input_data_structure = "center_level", outcome_name = "proportion", outcome_type = "binary",
      intervention_components = "DeliveryA", intervention_lower_bounds = 0, intervention_upper_bounds = 1,
      unit_costs = 1, center_characteristics = "MI_DeliveryB", center_characteristics_optimization_values = 100,
      include_center_effects = TRUE, outcome_goal = 0.6, quiet = TRUE
    ),
    "Fixed center effects and center characteristics cannot be both included"
  )
})

test_that("center level data is checked on its fitted rows, and a characteristic with no values is skipped", {
  p <- as.data.frame(main_pulesa_data)
  p$proportion <- p$Success / p$Total_visit
  p$center_sample_size <- p$Total_visit
  run <- function(data, v) extrapolation_warnings(lago_optimization(
    data = data, input_data_structure = "center_level", outcome_name = "proportion", outcome_type = "binary",
    intervention_components = "DeliveryA", intervention_lower_bounds = 0, intervention_upper_bounds = 1,
    unit_costs = 1, center_characteristics = "AccessBPMachines", center_characteristics_optimization_values = v,
    include_center_effects = FALSE, outcome_goal = 0.6, include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_length(run(p, 50), 1)
  expect_length(run(p, 0.6), 0)
  # a characteristic with no observed value has no range to check against, and
  # the model fit then says what is wrong
  p$AccessBPMachines <- NA_real_
  expect_error(run(p, 50))
  w <- tryCatch(run(p, 50), error = function(e) list())
  expect_length(w, 0)
})

test_that("the check skips a characteristic with no observed value on the fitted rows", {
  d <- data.frame(y = c(0, 1, 0, 1), x = 1:4, k = NA_real_)
  expect_silent(warn_center_extrapolation(d, "y", "x", "k", 9, NULL, FALSE, FALSE, "individual_level", NULL))
})

test_that("get_confidence_set() called directly warns once, from the rows its model was fitted on", {
  d <- BB_data[BB_data$pilot_phase == 1, ]
  r <- suppressWarnings(extrapolation_call(1.1))
  call_cs <- function(v) extrapolation_warnings(get_confidence_set(
    predictors_data = d[, c("coaching_updt", "launch_duration", "birth_volume_100")],
    intervention_components = c("coaching_updt", "launch_duration"), outcome_data = d$pp3_oxytocin_mother,
    fitted_model = r$model, link = "logit", outcome_goal = 0.85, outcome_type = "binary",
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5), confidence_set_grid_step_size = c(2, 1),
    center_characteristics = "birth_volume_100", center_characteristics_optimization_values = v,
    cost_list_of_vectors = list(c(0, 380, -24, 0.6), c(0, 1700, -950, 220)), rec_int = r$rec_int
  ))
  w <- call_cs(100)
  expect_length(w, 1)
  expect_match(conditionMessage(w[[1]]), "birth_volume_100 = 100, while the data the model is fitted on has 0.95 to 1.3")
  expect_length(call_cs(1.1), 0)
  # with the confidence set on, lago_optimization() still warns only once
  w <- extrapolation_warnings(lago_optimization(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"), intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5), center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 2.86, cost_list_of_vectors = list(c(0, 380, -24, 0.6), c(0, 1700, -950, 220)),
    outcome_goal = 0.85, confidence_set_grid_step_size = c(2, 1), quiet = TRUE
  ))
  expect_length(w, 1)
})

test_that("the shared check skips a value or a column it cannot read", {
  expect_silent(warn_outside_centers("k", Inf, list(1:3)))
  expect_silent(warn_outside_centers("k", 9, list(NULL)))
  expect_warning(warn_outside_centers(c("a", "k"), c(9, 9), list(1:3, NULL)), "a = 9, while")
})
