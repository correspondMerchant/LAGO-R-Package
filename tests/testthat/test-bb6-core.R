# Round 5 bug bash: core fixes

test_that("shrinking with interactions never recommends a point worse than the stage 1 intervention", {
  set.seed(5)
  d <- data.frame(x1 = stats::runif(300), x2 = stats::runif(300))
  d$y <- -d$x1 - d$x2 + 4 * d$x1 * d$x2 + stats::rnorm(300, 0, 0.1)
  d[["x1:x2"]] <- d$x1 * d$x2
  for (goal in c(2.5, 13, 30)) {
    for (method in c("grid_search", "numerical")) {
      r <- suppressWarnings(lago_optimization(
        data = d, outcome_name = "y", outcome_type = "continuous",
        glm_family = "gaussian", link = "identity",
        intervention_components = c("x1", "x2", "x1:x2"), main_components = c("x1", "x2"),
        include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
        intervention_upper_bounds = c(1, 1), cost_list_of_vectors = list(c(0, 1), c(0, 1)),
        prev_recommended_interventions = c(0.1, 0.1), outcome_goal = goal,
        optimization_method = method,
        optimization_grid_search_step_size = if (method == "grid_search") c(0.05, 0.05),
        include_confidence_set = FALSE, quiet = TRUE
      ))
      b <- stats::coef(r$model)
      at <- function(x) b[[1]] + b[[2]] * x[1] + b[[3]] * x[2] + b[[4]] * x[1] * x[2]
      expect_true(r$shrinking_method_used)
      expect_gte(at(r$rec_int), at(c(0.1, 0.1)) - 1e-9)
    }
  }
})

test_that("the numerical cost search does not stop early when one component has a wide range", {
  set.seed(7)
  n <- 600
  d <- data.frame(x1 = stats::runif(n, 0, 10), x2 = stats::runif(n, 0, 10))
  d$y <- stats::rbinom(n, 1, stats::plogis(-2 + 0.5 * d$x1 + 0.3 * d$x2))
  run <- function(s) {
    dd <- d
    dd$x2 <- d$x2 * s
    suppressWarnings(lago_optimization(
      data = dd, outcome_name = "y", outcome_type = "binary",
      intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 10 * s), outcome_goal = 0.5,
      cost_list_of_vectors = list(c(0, 1), c(0, 1e5 / s)),
      include_confidence_set = FALSE, quiet = TRUE
    ))
  }
  near <- run(1)
  far <- run(1e4)
  expect_equal(far$rec_int_cost, near$rec_int_cost, tolerance = 1e-4)
  expect_equal(far$est_outcome_goal, 0.5, tolerance = 1e-4)
})

test_that("the optimizer grid keeps its upper bound for a component in small units", {
  m <- stats::glm(mpg ~ gear + qsec, data = mtcars)
  gmax <- unname(stats::predict(m, data.frame(gear = 5, qsec = 23)))
  run <- function(sc) {
    d <- mtcars
    d$gearn <- d$gear * sc
    suppressWarnings(lago_optimization(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity", intervention_components = c("gearn", "qsec"),
      intervention_lower_bounds = c(3 * sc, 14), intervention_upper_bounds = c(5 * sc, 23),
      cost_list_of_vectors = list(c(0, 4 / sc), c(4, 6)), outcome_goal = gmax - 1e-9,
      optimization_method = "grid_search", optimization_grid_search_step_size = c(0.7 * sc, 1),
      include_confidence_set = FALSE, quiet = TRUE
    ))
  }
  r <- run(1e-9)
  expect_false(r$shrinking_method_used)
  expect_equal(r$rec_int_cost, run(1)$rec_int_cost)
})
