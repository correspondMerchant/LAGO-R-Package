# Paths added by the bug hunts that the other tests do not reach.

cov_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

cov_shrunk <- function() {
  set.seed(4)
  d <- data.frame(x = stats::runif(300, 0, 4))
  d$y <- 1 + 0.4 * d$x + stats::rnorm(300)
  cov_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 4,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 10, quiet = TRUE
  ))
}

test_that("print and plot say why a shrunk result has no confidence set", {
  r <- cov_shrunk()
  expect_true(r$shrinking_method_used)
  utils::capture.output(lines <- cli::cli_fmt(print(r)))
  expect_true(any(grepl("the shrinking method was used", lines)))
  expect_message(plot(r), "the shrinking method was used")
})

test_that("plot says when no grid point is in the confidence set", {
  r <- cov_shrunk()
  r$shrinking_method_used <- FALSE
  r$confidence_set_size_percentage <- 0
  expect_message(plot(r), "no grid point is in the confidence set")
})

test_that("the stage-1 intervention falls back to the plain mean without usable weights", {
  set.seed(4)
  d <- data.frame(x = stats::runif(50, 0, 4))
  # a goal far out of reach shrinks all the way to the stage-1 intervention
  rec <- cov_quiet(LAGOtrials:::get_recommended_interventions(
    data = d, intervention_components_coeff = c(1, 0.4), include_interaction_terms = FALSE,
    main_components = NULL, intervention_components = "x",
    all_center_lvl_effects = 0, center_weights_for_outcome_goal = 1,
    cost_list_of_vectors = list(c(0, 1)), intervention_lower_bounds = 0,
    intervention_upper_bounds = 4, outcome_goal = 100, optimization_method = "numerical",
    optimization_grid_search_step_size = NULL, link = "identity", lower_outcome_goal = FALSE,
    prev_recommended_interventions = NULL, shrinkage_threshold = NULL, power_goal = NULL,
    power_goal_approach = NULL, num_centers_in_next_stage = NULL,
    patients_per_center_in_next_stage = NULL, outcome_name = "y",
    row_weights = rep(0, 50)
  ))
  expect_equal(unname(rec$est_rec_int), mean(d$x))
})

test_that("grid search shrinks an unreachable power goal to the stage-1 intervention", {
  d <- do.call(rbind, c(
    lapply(1:6, function(j) data.frame(group = "control", x = 0, y = rep(1:0, c(3, 12)))),
    lapply(1:6, function(j) data.frame(
      group = "treatment", x = c(1, 1.5, 2, 2.5, 3, 3.5)[j], y = rep(1:0, c(j, 15 - j))
    ))
  ))
  r <- cov_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
    link = "identity", intervention_lower_bounds = 0, intervention_upper_bounds = 40,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.3, power_goal = 0.8,
    num_centers_in_next_stage = 2, patients_per_center_in_next_stage = 2,
    power_goal_approach = "conditional", optimization_method = "grid_search",
    optimization_grid_search_step_size = 1, include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_true(r$shrinking_method_used)
  expect_equal(r$rec_int, mean(d$x), tolerance = 1e-8)
})

test_that("lago_budget() searches the main components of an interaction model", {
  set.seed(8)
  d <- data.frame(a = stats::runif(400, 0, 4), b = stats::runif(400, 0, 4))
  d$y <- 1 + 0.5 * d$a + 0.3 * d$b + 0.1 * d$a * d$b + stats::rnorm(400)
  d[["a:b"]] <- d$a * d$b
  b <- cov_quiet(lago_budget(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("a", "b", "a:b"), main_components = c("a", "b"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4), cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    budget = 3, n_grid = 5
  ))
  expect_true(b$feasible)
  expect_named(b$rec_int, c("a", "b"))
  expect_lte(b$rec_int_cost, 3 + 1e-6)
})

test_that("a binary lago_budget() extends its grid towards 1", {
  set.seed(2)
  d <- data.frame(x = stats::runif(2000, 0, 5))
  d$y <- stats::rbinom(2000, 1, stats::plogis(-1 + 1.5 * d$x))
  b <- cov_quiet(lago_budget(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 5,
    cost_list_of_vectors = list(c(0, 1)), budget = 100, n_grid = 5
  ))
  expect_true(b$feasible)
  # goals above the 0.98 top of the first grid were tried
  expect_gt(max(b$frontier$outcome_goal), 0.98)
})

test_that("lago_budget() says when no goal is reachable without a power goal", {
  set.seed(6)
  d <- data.frame(x = stats::runif(300, 0, 2), cc = stats::runif(300, 0, 1))
  d$y <- 5 + 0.5 * d$x + 4 * d$cc + stats::rnorm(300, sd = 0.2)
  run <- function() lago_budget(
    data = d, outcome_name = "y", outcome_type = "continuous", intervention_components = "x",
    center_characteristics = "cc", center_characteristics_optimization_values = -20,
    intervention_lower_bounds = 0, intervention_upper_bounds = 2,
    cost_list_of_vectors = list(c(0, 1)), budget = 10, n_grid = 5
  )
  expect_warning(b <- suppressMessages(run()), "Check the model, bounds and outcome type")
  expect_false(b$feasible)
  utils::capture.output(lines <- cli::cli_fmt(print(b)))
  expect_true(any(grepl("no budget would help", lines)))
})

test_that("print and plot say how to get a confidence set that was not requested", {
  set.seed(4)
  d <- data.frame(x = stats::runif(300, 0, 4))
  d$y <- 1 + 0.4 * d$x + stats::rnorm(300)
  r <- cov_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 4,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 1.5,
    include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_false(r$shrinking_method_used)
  utils::capture.output(lines <- cli::cli_fmt(print(r)))
  expect_true(any(grepl("set include_confidence_set = TRUE to compute it", lines)))
  expect_message(plot(r), "include_confidence_set = TRUE")
})
