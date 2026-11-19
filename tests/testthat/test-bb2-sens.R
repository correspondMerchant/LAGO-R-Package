# Tests for the fixes to lago_budget() and lago_sensitivity() from bug bash 2.

bb2_sens_base <- list(
  data = mtcars,
  outcome_name = "mpg",
  outcome_type = "continuous",
  glm_family = "gaussian",
  link = "identity",
  intervention_components = c("gear", "qsec"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 350),
  cost_list_of_vectors = list(c(0, 4), c(4, 6)),
  outcome_goal = 30,
  outcome_goal_intention = "maximize",
  include_confidence_set = FALSE,
  quiet = TRUE
)

bb2_budget <- function(..., budget, n_grid = 4L) {
  args <- utils::modifyList(bb2_sens_base, list(...))
  args$outcome_goal <- NULL
  suppressMessages(do.call(lago_budget, c(args, list(budget = budget, n_grid = n_grid))))
}

test_that("a small budget affords the cheap interventions below the goal grid", {
  skip_on_cran()
  # the lower bounds cost 4 and reach about -30.7, far below the grid, which
  # starts near -1.35 where the cheapest goal costs about 28.1
  b <- bb2_budget(budget = 4.5)
  expect_true(b$feasible)
  expect_lte(b$rec_int_cost, 4.5)
  expect_gt(b$rec_int_cost, 4.4)
  expect_lt(b$est_outcome, -29)
})

test_that("a budget below every intervention reports the true cheapest cost", {
  skip_on_cran()
  expect_warning(
    b <- bb2_budget(budget = 3),
    "cheapest reachable intervention \\(4\\)"
  )
  expect_false(b$feasible)
})

test_that("a small budget is feasible when minimizing an outcome above the grid", {
  skip_on_cran()
  mt <- mtcars
  mt$neg_mpg <- -mt$mpg
  b <- bb2_budget(
    data = mt, outcome_name = "neg_mpg", outcome_goal_intention = "minimize",
    budget = 4.5
  )
  expect_true(b$feasible)
  expect_lte(b$rec_int_cost, 4.5)
  expect_gt(b$est_outcome, 29)
})

test_that("a coarse grid still finds the best affordable outcome", {
  skip_on_cran()
  # n_grid = 2 used to stop at 14.32 for a cost of 40.98, while 19.82 was
  # affordable
  b <- bb2_budget(budget = 50, n_grid = 2L)
  expect_true(b$feasible)
  expect_true(b$binding)
  expect_lte(b$rec_int_cost, 50)
  expect_gt(b$rec_int_cost, 49.9)
  expect_gt(b$est_outcome, 19.8)
})

test_that("an unbinding budget reports the maximum reachable outcome", {
  skip_on_cran()
  mt <- mtcars
  mt$center <- factor(mt$cyl)
  mt$period <- mt$carb
  b <- suppressWarnings(bb2_budget(
    data = mt, include_center_effects = TRUE, include_time_effects = TRUE,
    time_effect_optimization_value = 2, center_effects_optimization_values = "6",
    budget = 60
  ))
  # gear at its upper bound of 10 with qsec at 0 is the maximum, about 45.29
  max_run <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    utils::modifyList(bb2_sens_base, list(
      data = mt, include_center_effects = TRUE, include_time_effects = TRUE,
      time_effect_optimization_value = 2, center_effects_optimization_values = "6",
      intervention_lower_bounds = c(10, 0), intervention_upper_bounds = c(10, 0),
      outcome_goal = 40
    ))
  )))
  expect_false(b$binding)
  expect_lt(abs(b$est_outcome - max_run$est_outcome_goal), 0.01)
})

test_that("print.lago_budget rounds the cost and the estimated outcome", {
  skip_on_cran()
  b <- bb2_budget(budget = 50, n_grid = 2L)
  out <- paste(cli::cli_fmt(print(b)), collapse = " ")
  expect_match(out, sprintf("Estimated outcome: %s", .lago_fmt_value(b$est_outcome)), fixed = TRUE)
  expect_match(out, sprintf("Cost: %s", .lago_fmt_cost(b$rec_int_cost)), fixed = TRUE)
  expect_no_match(out, "[0-9]\\.[0-9]{6}")
})

test_that("a cost_multiplier sweep scales the unit costs of a unit cost fit", {
  skip_on_cran()
  for (type in c("cubic", "linear")) {
    fit <- suppressWarnings(do.call(lago_optimization, utils::modifyList(
      bb2_sens_base,
      list(cost_list_of_vectors = NULL, unit_costs = c(4, 6), default_cost_fxn_type = type)
    )))
    s <- suppressWarnings(lago_sensitivity(fit, parameter = "cost_multiplier", values = c(1, 2)))
    expect_equal(s$status, c("ok", "ok"))
    expect_equal(s$rec_int_cost[1], fit$rec_int_cost, tolerance = 1e-8)
    expect_equal(s$rec_int_cost[2], 2 * fit$rec_int_cost, tolerance = 1e-6)
    expect_equal(s$gear[2], s$gear[1], tolerance = 1e-6)
    expect_equal(s$qsec[2], s$qsec[1], tolerance = 1e-6)
  }
})

test_that("a cost_multiplier sweep without any costs names both cost inputs", {
  fit <- structure(list(), class = "lago", lago_call_args = utils::modifyList(
    bb2_sens_base, list(cost_list_of_vectors = NULL)
  ))
  expect_error(
    lago_sensitivity(fit, parameter = "cost_multiplier", values = 1),
    "requires `cost_list_of_vectors` or\\s+`unit_costs`"
  )
})

test_that("print.lago_sensitivity with one successful run names it once", {
  s <- structure(
    data.frame(value = 30, gear = 10, qsec = 6.5, rec_int_cost = 83.1336,
               est_outcome_goal = 30, status = "ok"),
    parameter = "outcome_goal", component_names = c("gear", "qsec"),
    class = c("lago_sensitivity", "data.frame")
  )
  out <- paste(cli::cli_fmt(print(s)), collapse = " ")
  expect_match(out, "rec_int_cost is 83.1336 at outcome_goal = 30.", fixed = TRUE)
  expect_no_match(out, " and ")
})
