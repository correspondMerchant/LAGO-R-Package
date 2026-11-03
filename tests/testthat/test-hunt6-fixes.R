# Results checked against independent calculations, from a sixth bug hunt.

hunt6_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

hunt6_warnings <- function(expr) {
  warnings <- character(0)
  value <- withCallingHandlers(suppressMessages(expr), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

test_that("the shrunk recommendation does not depend on the outcome's units", {
  set.seed(4)
  d <- data.frame(x1 = stats::runif(300, 0, 4), x2 = stats::runif(300, 0, 3))
  d$y <- 1 + 0.4 * d$x1 + 0.3 * d$x2 + stats::rnorm(300)
  b <- stats::coef(stats::glm(y ~ x1 + x2, data = d))
  up <- c(4, 3)
  # just past the reachable outcome, so the shrinking method is used
  goal <- b[[1]] + sum(b[2:3] * up) + 0.3
  # Nevo's shrinking formula by hand, each component with the other at its bound
  s1 <- colMeans(d[, c("x1", "x2")])
  hand <- vapply(1:2, function(k) {
    other <- 3 - k
    bmax <- (goal - b[[1]] - b[[1 + other]] * up[other]) / up[k]
    s1[[k]] + (up[k] - s1[[k]]) * (b[[1 + k]] - bmax / 2) / (bmax / 2)
  }, numeric(1))
  for (k in c(1, 1e-6, 1e-8)) {
    dd <- d
    dd$y <- d$y * k
    r <- hunt6_quiet(lago_optimization(
      data = dd, outcome_name = "y", outcome_type = "continuous",
      intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = up, cost_list_of_vectors = list(c(0, 1), c(0, 2)),
      outcome_goal = goal * k, include_confidence_set = FALSE, quiet = TRUE
    ))
    expect_true(r$shrinking_method_used)
    expect_equal(unname(r$rec_int), hand, tolerance = 1e-4)
  }
})

test_that("the default cubic cost is accurate for wide bounds and far from 0", {
  coefs <- LAGOtrials:::cost_fxn_calculator(0, 5000, 3, "cubic")[[1]]
  x <- seq(1, 5000, length.out = 2001)
  total <- vapply(x, function(v) sum(coefs * v^(0:4)), numeric(1))
  expect_true(all(diff(total) >= 0))
  expect_equal(mean(total / x), 3, tolerance = 1e-3)
  # narrow bounds far from 0 keep the average unit cost, and a result the
  # plain solve gave accurately is unchanged
  unit_mean <- function(lo, hi) {
    k <- LAGOtrials:::cost_fxn_calculator(lo, hi, 3, "cubic")[[1]]
    x <- seq(lo, hi, length.out = 201)
    mean(vapply(x, function(v) sum(k * v^(0:4)), numeric(1)) / x)
  }
  expect_equal(unit_mean(10, 10.0001), 3, tolerance = 1e-4)
  expect_equal(unit_mean(18.5, 18.50048), 3, tolerance = 1e-4)
  expect_equal(unit_mean(200, 200.1), 3, tolerance = 1e-4)
  # and so do narrow bounds near 1
  expect_equal(unit_mean(1, 1.00001), 3, tolerance = 1e-4)
  expect_equal(unit_mean(0.5, 0.500005), 3, tolerance = 1e-4)
})

test_that("the numerical optimizer tries every start before refusing", {
  set.seed(5)
  d <- data.frame(x1 = stats::runif(1500, 0, 10), x2 = stats::runif(1500, 0, 10), x3 = stats::runif(1500, 0, 10))
  d$y <- stats::rbinom(1500, 1, stats::plogis(-1.5 + 0.15 * d$x1 - 0.05 * d$x2 - 0.03 * d$x3))
  b <- stats::coef(stats::glm(y ~ x1 + x2 + x3, stats::binomial, d))
  goal <- round(stats::plogis(b[[1]] + 10 * b[[2]]) - 0.005, 4)
  r <- hunt6_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = c("x1", "x2", "x3"), intervention_lower_bounds = c(0, 0, 0),
    intervention_upper_bounds = c(10, 10, 10), outcome_goal = goal,
    cost_list_of_vectors = list(c(0, 1), c(0, 1), c(0, 1)),
    include_confidence_set = FALSE, quiet = TRUE
  ))
  # the cheapest point spends on x1 alone, by hand
  expect_equal(r$rec_int[1], (stats::qlogis(goal) - b[[1]]) / b[[2]], tolerance = 1e-4)
})

test_that("an unreachable power goal is shrunk for on the identity link too", {
  d <- do.call(rbind, c(
    lapply(1:6, function(j) data.frame(group = "control", x = 0, y = rep(1:0, c(3, 12)))),
    lapply(1:6, function(j) data.frame(
      group = "treatment", x = c(1, 1.5, 2, 2.5, 3, 3.5)[j], y = rep(1:0, c(j, 15 - j))
    ))
  ))
  run <- hunt6_warnings(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
    link = "identity", intervention_lower_bounds = 0, intervention_upper_bounds = 40,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.3, power_goal = 0.8,
    num_centers_in_next_stage = 2, patients_per_center_in_next_stage = 2,
    power_goal_approach = "conditional", quiet = TRUE
  ))
  r <- run$value
  expect_true(r$shrinking_method_used)
  # the cause is the power goal, which the model can reach at the bound
  expect_true(any(grepl("shrunk to the stage-1 intervention", run$warnings)))
  expect_false(any(grepl("maximum estimated achievable outcome", run$warnings)))
  # all the way to the stage-1 intervention, as on the logit link
  expect_equal(r$rec_int, mean(d$x), tolerance = 1e-8)
  # and lago_budget() does not count it as reaching the power goal
  b <- hunt6_quiet(lago_budget(r, budget = 40))
  expect_false(b$feasible)
})

test_that("a covariate glm() cannot estimate is not reported as held at 0", {
  set.seed(9)
  d <- data.frame(center = factor(sample(paste0("c", 1:5), 1500, TRUE)), x1 = stats::runif(1500, 0, 4))
  d$ccov <- 10 + c(c1 = 0, c2 = 1, c3 = 0, c4 = 2, c5 = 1)[as.character(d$center)]
  d$y <- stats::rbinom(1500, 1, stats::plogis(-1 + 0.4 * d$x1 + c(0, 0.3, -0.2, 0.4, 0.1)[as.numeric(d$center)]))
  run <- hunt6_warnings(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x1",
    include_center_effects = TRUE, additional_covariates = "ccov",
    intervention_lower_bounds = 0, intervention_upper_bounds = 4,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.5,
    confidence_set_grid_step_size = 0.25, quiet = TRUE
  ))
  expect_true(any(grepl("ccov could not be estimated", run$warnings)))
  expect_false(any(grepl("held at 0", run$warnings)))
})

test_that("a factor covariate with only some levels aliased is not called dropped", {
  set.seed(2)
  d <- data.frame(x = stats::runif(600, 0, 5), region = sample(c("east", "north", "south"), 600, TRUE))
  d$urban <- as.numeric(d$region == "south")
  d$y <- 2 + 0.4 * d$x + (d$region == "north") + stats::rnorm(600)
  run <- hunt6_warnings(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous", intervention_components = "x",
    additional_covariates = c("urban", "region"), intervention_lower_bounds = 0,
    intervention_upper_bounds = 5, cost_list_of_vectors = list(c(0, 1)), outcome_goal = 3.5,
    include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_true(any(grepl("Some levels of the additional covariate\\(s\\) region", run$warnings)))
  expect_false(any(grepl("region could not be estimated by glm\\(\\) \\(their coefficient\\(s\\) are NA\\), so they were dropped", run$warnings)))
})

test_that("get_confidence_set() refuses a model with an NA center coefficient", {
  set.seed(9)
  d <- data.frame(center = factor(sample(paste0("c", 1:5), 1500, TRUE)), x1 = stats::runif(1500, 0, 4))
  d$ccov <- c(c1 = 0, c2 = 1, c3 = 0, c4 = 2, c5 = 1)[as.character(d$center)]
  d$y <- stats::rbinom(1500, 1, stats::plogis(-1 + 0.4 * d$x1))
  # glm() puts the NA on a center dummy when the covariate comes first
  fit <- stats::glm(y ~ ccov + center + x1, stats::binomial, d)
  expect_error(
    get_confidence_set(
      predictors_data = d[, c("center", "x1", "ccov")], include_center_effects = TRUE,
      center_weights_for_outcome_goal = as.numeric(table(d$center)) / 1500,
      additional_covariates = "ccov", intervention_components = "x1", outcome_data = d$y,
      fitted_model = fit, link = "logit", outcome_goal = 0.5, outcome_type = "binary",
      intervention_lower_bounds = 0, intervention_upper_bounds = 4,
      confidence_set_grid_step_size = 0.25, cost_list_of_vectors = list(c(0, 1)), rec_int = 1.5
    ),
    "could not be estimated by glm"
  )
})

test_that("a binary budget for a rare outcome reaches goals below the grid", {
  set.seed(1)
  d <- data.frame(x1 = stats::runif(20000, 0, 3), x2 = stats::runif(20000, 0, 3))
  d$y <- stats::rbinom(20000, 1, stats::plogis(-6 + 0.6 * d$x1 + 0.4 * d$x2))
  b <- hunt6_quiet(lago_budget(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(3, 3), cost_list_of_vectors = list(c(0, 3), c(0, 5)),
    outcome_goal_intention = "maximize", budget = 6
  ))
  expect_true(b$feasible)
  # the best affordable outcome by brute force over the fitted model
  fit <- stats::glm(y ~ x1 + x2, stats::binomial, d)
  g <- expand.grid(x1 = seq(0, 3, 0.01), x2 = seq(0, 3, 0.01))
  g <- g[3 * g$x1 + 5 * g$x2 <= 6, ]
  best <- max(stats::predict(fit, g, type = "response"))
  expect_lt(abs(b$est_outcome - best) / best, 0.02)
})

test_that("print keeps 4 significant digits for a rare outcome", {
  set.seed(9)
  d <- data.frame(x = stats::runif(40000, 0, 10))
  d$y <- stats::rbinom(40000, 1, stats::plogis(-6.5 + 0.05 * d$x))
  r <- hunt6_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 10,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.0023,
    confidence_set_grid_step_size = 0.25, quiet = TRUE
  ))
  utils::capture.output(lines <- cli::cli_fmt(print(r)))
  ci_line <- grep("CI for the estimated outcome", lines, value = TRUE)
  expect_match(ci_line, format(r$est_outcome_ci[["lower"]]), fixed = TRUE)
})

test_that("solver noise around 0 still prints as 0", {
  fmt <- getFromNamespace(".lago_fmt_value", "LAGOtrials")
  expect_equal(fmt(c(3e-9, -2e-8, 0.00123456, 0.5)), c(0, 0, 0.001235, 0.5))
})

test_that("a binary budget below the cheapest cost does not keep filling", {
  set.seed(1)
  d <- data.frame(x1 = stats::runif(2000, 0, 3), x2 = stats::runif(2000, 0, 3))
  d$y <- stats::rbinom(2000, 1, stats::plogis(-1 + 0.6 * d$x1 + 0.4 * d$x2))
  b <- hunt6_quiet(lago_budget(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(3, 3), cost_list_of_vectors = list(c(2, 3), c(0, 5)),
    outcome_goal_intention = "maximize", budget = 1, n_grid = 5
  ))
  expect_false(b$feasible)
  # the grid, one fill pass that finds nothing cheaper, and nothing more
  expect_lte(nrow(b$frontier), 10)
})

test_that("a binary budget fills below the grid when no grid goal is reached", {
  set.seed(3)
  d <- data.frame(x1 = stats::runif(40000, 0, 3))
  d$y <- stats::rbinom(40000, 1, stats::plogis(-6 + 0.3 * d$x1))
  b <- hunt6_quiet(lago_budget(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = "x1", intervention_lower_bounds = 0,
    intervention_upper_bounds = 3, cost_list_of_vectors = list(c(0, 1)),
    outcome_goal_intention = "maximize", budget = 1.5
  ))
  expect_true(b$feasible)
  # the best affordable outcome is at x1 = 1.5, by the fitted model
  fit <- stats::glm(y ~ x1, stats::binomial, d)
  best <- stats::predict(fit, data.frame(x1 = 1.5), type = "response")
  expect_lt(abs(b$est_outcome - best) / best, 0.02)
})
