# Tests that lago_optimization() rejects invalid inputs with clear errors.
# These call the public function so they exercise the real validation path.

# a minimal valid argument set; individual tests override one field to make
# it invalid.
base_args <- function(...) {
  utils::modifyList(
    list(
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
      include_confidence_set = FALSE
    ),
    list(...)
  )
}

test_that("outcome_type must be 'continuous' or 'binary'", {
  expect_error(
    suppressWarnings(do.call(lago_optimization, base_args(outcome_type = "count"))),
    "continuous|binary"
  )
})

test_that("outcome_name must be a column in the data", {
  expect_error(
    suppressWarnings(do.call(lago_optimization, base_args(outcome_name = "not_a_col"))),
    "outcome name"
  )
})

test_that("neither outcome_goal nor power_goal is an error", {
  expect_error(
    suppressWarnings(do.call(
      lago_optimization,
      base_args(outcome_goal = NULL, power_goal = NULL)
    )),
    "Both 'outcome_goal' and 'power_goal' are NULL"
  )
})

test_that("power goal with a continuous outcome is rejected", {
  expect_error(
    suppressWarnings(do.call(lago_optimization, base_args(power_goal = 0.8))),
    "binary"
  )
})

test_that("bounds must have matching lengths", {
  expect_error(
    suppressWarnings(do.call(
      lago_optimization,
      base_args(intervention_upper_bounds = c(10))
    )),
    "lengths of lower and upper bounds must be the same"
  )
})

test_that("neither unit_costs nor cost_list_of_vectors is an error", {
  expect_error(
    suppressWarnings(do.call(
      lago_optimization,
      base_args(cost_list_of_vectors = NULL, unit_costs = NULL)
    )),
    "Both 'unit_costs' and 'cost_list_of_vectors' are NULL"
  )
})
