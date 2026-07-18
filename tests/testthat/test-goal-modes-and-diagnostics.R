# Tests for the three goal modes (#40) and the fit diagnostics (#36).

bb_grouped <- function() {
  d <- BB_data
  d$group <- ifelse(d$pre_post == 0, "control", "treatment")
  d
}

test_that("power goal alone works (no outcome goal)", {
  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = bb_grouped(),
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    power_goal = 0.8,
    num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 30,
    include_confidence_set = FALSE
  )))
  expect_length(res$rec_int, 2)
  # with no outcome goal, the target IS the power-implied outcome; pin it so
  # the power-to-outcome conversion actually has coverage (a broken
  # conversion would change this value).
  expect_equal(res$est_outcome_goal, 0.47817, tolerance = 1e-3)
})

test_that("both goals: the higher (outcome) goal binds", {
  args <- list(
    data = BB_data, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = 0.85, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE
  )
  outcome_only <- suppressWarnings(suppressMessages(do.call(lago_optimization, args)))
  both <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    utils::modifyList(args, list(
      data = bb_grouped(), power_goal = 0.8,
      num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 30
    ))
  )))
  # outcome goal 0.85 exceeds the power-implied outcome, so results match.
  expect_equal(both$rec_int, outcome_only$rec_int, tolerance = 1e-3)
})

test_that("power goal + minimize is rejected", {
  expect_error(
    suppressWarnings(lago_optimization(
      data = bb_grouped(),
      outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = "birth_volume_100",
      center_characteristics_optimization_values = 1.75,
      intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      outcome_goal = 0.85, outcome_goal_intention = "minimize",
      power_goal = 0.8, num_centers_in_next_stage = 10,
      patients_per_center_in_next_stage = 30
    )),
    "minimize"
  )
})

test_that("a non-significant intervention component triggers a warning", {
  set.seed(2)
  n <- 200
  # "noise" is non-negative (valid intervention) but has no true effect on y.
  d <- data.frame(dose = runif(n, 0, 10), noise = runif(n, 0, 10))
  d$y <- rbinom(n, 1, 1 / (1 + exp(-(-1 + 0.4 * d$dose))))
  # bounds cover the full data range so only the significance warning fires.
  expect_warning(
    suppressMessages(lago_optimization(
      data = d, outcome_name = "y", outcome_type = "binary",
      intervention_components = c("dose", "noise"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 10),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      outcome_goal = 0.7, outcome_goal_intention = "maximize",
      include_confidence_set = FALSE
    )),
    "significant"
  )
})

test_that("providing a grid step size switches method to grid_search", {
  expect_message(
    suppressWarnings(lago_optimization(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity",
      intervention_components = c("gear", "qsec"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
      cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = 40, outcome_goal_intention = "maximize",
      optimization_grid_search_step_size = c(1, 1),
      include_confidence_set = FALSE
    )),
    "grid_search"
  )
})
