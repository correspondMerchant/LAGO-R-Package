# Results checked against independent calculations, from a third bug hunt.

hunt3_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# ---- a center with only zero weights ------------------------------------------

test_that("a center whose rows all have zero weight is left out of the fit", {
  set.seed(1)
  d <- data.frame(center = factor(rep(paste0("c", 1:5), each = 40)), x = stats::runif(200, 0, 10))
  d$y <- stats::rbinom(200, 1, stats::plogis(-1 + 0.2 * d$x + c(-0.5, 0, 0.3, 0.6, -0.2)[as.integer(d$center)]))
  run <- function(data, weights = NULL) {
    lago_optimization(
      data = data, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
      include_center_effects = TRUE, weights = weights, intervention_lower_bounds = 0,
      intervention_upper_bounds = 10, cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.6,
      confidence_set_grid_step_size = 1, quiet = TRUE
    )
  }
  expect_warning(
    zero <- suppressMessages(run(d, ifelse(d$center == "c3", 0, 1))),
    "no usable row with a positive glm weight"
  )
  # the same data without that center at all
  without <- hunt3_quiet(run(droplevels(d[d$center != "c3", ])))
  expect_equal(zero$rec_int, without$rec_int, tolerance = 1e-8)
  # a positive weight on a row the model cannot use does not count
  incomplete <- d
  first <- which(d$center == "c3")[1]
  incomplete$x[first] <- NA
  weights <- ifelse(d$center == "c3", 0, 1)
  weights[first] <- 1
  expect_equal(hunt3_quiet(run(incomplete, weights))$rec_int, without$rec_int, tolerance = 1e-8)
  expect_equal(zero$est_outcome_ci, without$est_outcome_ci)
  expect_equal(zero$confidence_set_size_percentage, without$confidence_set_size_percentage)
})

# ---- the default cubic cost -----------------------------------------------------

test_that("the default cubic cost does not depend on the cost unit", {
  costs <- function(scale) {
    LAGOtrials:::cost_fxn_calculator(c(0, 0), c(10, 4), c(1, 2) * scale, "cubic")
  }
  base <- costs(1)
  for (scale in c(0.01, 1000)) {
    expect_equal(lapply(costs(scale), function(v) v / scale), base, tolerance = 1e-8)
  }
  # a unit cost of 0 is the zero curve, at once
  expect_equal(LAGOtrials:::cost_fxn_calculator(0, 4, 0, "cubic")[[1]], rep(0, 5))
})

test_that("a negative unit cost is refused", {
  expect_error(
    lago_optimization(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 350), unit_costs = c(1, -1), outcome_goal = 40,
      quiet = TRUE
    ),
    "unit_costs must be finite and not negative"
  )
})

test_that("the confidence set keeps the precision of a cost in small units", {
  r <- hunt3_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 0.001), c(0, 0.002)), outcome_goal = 40,
    confidence_set_grid_step_size = c(1, 1), quiet = TRUE
  ))
  hand <- 0.001 * r$cs$gear + 0.002 * r$cs$qsec
  expect_equal(r$cs$cost, hand, tolerance = 1e-6)
  # a large cost keeps its 2 decimals
  big <- hunt3_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4321.123), c(0, 3456.789)), outcome_goal = 40,
    confidence_set_grid_step_size = c(1, 1), quiet = TRUE
  ))
  expect_equal(big$cs$cost, round(4321.123 * big$cs$gear + 3456.789 * big$cs$qsec, 2))
})

# ---- the numerical optimizer ------------------------------------------------------

test_that("the numerical optimizer does not return a point short of the goal", {
  set.seed(1)
  d <- expand.grid(x1 = 0:10, x2 = 0:10)
  d <- d[rep(seq_len(nrow(d)), 3), ]
  d[["x1:x2"]] <- d$x1 * d$x2
  d$y <- d$x1 + 0.98 * d$x2 - 0.2 * d$x1 * d$x2 + stats::rnorm(nrow(d), sd = 0.01)
  r <- hunt3_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("x1", "x2", "x1:x2"), main_components = c("x1", "x2"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 10), outcome_goal = 9.5,
    cost_list_of_vectors = list(c(0, 1), c(0, 2)), include_confidence_set = FALSE, quiet = TRUE
  ))
  # the cheapest point that reaches the goal, by brute force
  fit <- stats::glm(y ~ x1 + x2 + x1:x2, data = d)
  g <- expand.grid(x1 = seq(0, 10, 0.01), x2 = seq(0, 10, 0.01))
  ok <- stats::predict(fit, g) >= 9.5
  expect_gte(r$est_outcome_goal, 9.5 - 1e-4)
  expect_equal(r$rec_int_cost, min((g$x1 + 2 * g$x2)[ok]), tolerance = 1e-3)
})

# the optimizer on given coefficients, with x1, x2 and their interaction
hunt3_optimize <- function(beta, cost, goal, link) {
  suppressWarnings(LAGOtrials:::get_recommended_interventions(
    data = NULL, intervention_components_coeff = beta, include_interaction_terms = TRUE,
    main_components = c("x1", "x2"), intervention_components = c("x1", "x2", "x1:x2"),
    all_center_lvl_effects = 0, center_weights_for_outcome_goal = 1,
    cost_list_of_vectors = cost, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 10), outcome_goal = goal, optimization_method = "numerical",
    optimization_grid_search_step_size = NULL, link = link, lower_outcome_goal = FALSE,
    prev_recommended_interventions = c(5, 5), shrinkage_threshold = NULL, power_goal = NULL,
    power_goal_approach = NULL, num_centers_in_next_stage = NULL,
    patients_per_center_in_next_stage = NULL, outcome_name = "y"
  ))
}

# the cheapest grid point that reaches the goal
hunt3_brute <- function(beta, cost, goal, link) {
  g <- expand.grid(x1 = seq(0, 10, 0.005), x2 = seq(0, 10, 0.005))
  eta <- beta[1] + beta[2] * g$x1 + beta[3] * g$x2 + beta[4] * g$x1 * g$x2
  outcome <- if (link == "logit") stats::plogis(eta) else eta
  total <- cost[[1]][2] * g$x1 + cost[[1]][3] * g$x1^2 + cost[[2]][2] * g$x2 + cost[[2]][3] * g$x2^2
  min(total[outcome >= goal])
}

test_that("restarts that stop short of the goal are left out", {
  cases <- list(
    list(beta = c(-1.5, 0.46, 0.42, -0.097), cost = list(c(0, 1, 0), c(0, 1, 0)), goal = 0.85, link = "logit"),
    list(beta = c(0, 1, 0.98, -0.2), cost = list(c(0, 0, 1), c(0, 0, 2)), goal = 9.5, link = "identity")
  )
  for (case in cases) {
    r <- do.call(hunt3_optimize, case)
    expect_false(r$shrinking_method_used)
    expect_gte(r$est_reachable_outcome, case$goal - 1e-6)
    expect_equal(r$rec_int_cost, do.call(hunt3_brute, case), tolerance = 2e-3)
  }
})

test_that("the highest outcome is not taken from a saddle point on the diagonal", {
  # equal effects that cancel through the interaction: the diagonal restarts
  # stop at about 0.62, while x2 = 0 and x1 = 10 reaches 0.95
  r <- hunt3_optimize(c(-2, 0.5, 0.5, -0.1), list(c(0, 1), c(0, 1)), 0.9, "logit")
  expect_false(r$shrinking_method_used)
  expect_equal(r$est_rec_int, c((stats::qlogis(0.9) + 2) / 0.5, 0), tolerance = 1e-4)
})

test_that("the numerical optimizer looks off the diagonal of the bounds", {
  set.seed(1)
  d <- expand.grid(x1 = 0:10, x2 = 0:10)
  d <- d[rep(seq_len(nrow(d)), 2), ]
  d$y <- d$x1 + d$x2 + stats::rnorm(nrow(d), sd = 0.5)
  r <- hunt3_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 10), outcome_goal = 8,
    cost_list_of_vectors = list(c(0, 4, -0.2), c(0, 4, -0.2)),
    include_confidence_set = FALSE, quiet = TRUE
  ))
  # with a cost that flattens out, the cheapest point spends on one component
  fit <- stats::glm(y ~ x1 + x2, data = d)
  g <- expand.grid(x1 = seq(0, 10, 0.01), x2 = seq(0, 10, 0.01))
  ok <- stats::predict(fit, g) >= 8
  cost <- 4 * g$x1 - 0.2 * g$x1^2 + 4 * g$x2 - 0.2 * g$x2^2
  expect_equal(r$rec_int_cost, min(cost[ok]), tolerance = 1e-3)
})

# ---- the overall intervention test on center level data ---------------------------

test_that("the overall test on center level data counts each row's patients", {
  set.seed(1)
  cl <- data.frame(
    center = rep(paste0("c", 1:6), times = 2), x = round(stats::runif(12, 0, 5), 2),
    center_sample_size = c(400, 20, 30, 25, 500, 15, 20, 450, 30, 25, 20, 380),
    group = rep(c("treatment", "control"), each = 6)
  )
  events <- stats::rbinom(12, cl$center_sample_size,
    stats::plogis(-1 + 0.3 * cl$x + ifelse(cl$group == "treatment", 0.4, 0))
  )
  cl$proportion <- events / cl$center_sample_size
  r <- hunt3_quiet(lago_optimization(
    data = cl, input_data_structure = "center_level", outcome_name = "proportion",
    outcome_type = "binary", intervention_components = "x", intervention_lower_bounds = 0,
    intervention_upper_bounds = 10, cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.6,
    include_center_effects = TRUE, include_confidence_set = FALSE, quiet = TRUE
  ))
  # the same test on the patients, one row each
  y <- unlist(lapply(seq_len(12), function(i) rep(1:0, c(events[i], cl$center_sample_size[i] - events[i]))))
  arm <- rep(cl$group, cl$center_sample_size)
  tr <- y[arm == "treatment"]
  ct <- y[arm == "control"]
  t_stat <- (mean(tr) - mean(ct)) / sqrt(stats::var(tr) / length(tr) + stats::var(ct) / length(ct))
  expect_equal(r$test_results$test_stat, t_stat, tolerance = 1e-10)
  expect_equal(r$test_results$p_val, 2 * stats::pt(-abs(t_stat), length(y) - 2), tolerance = 1e-10)
})

# ---- a covariate glm() cannot estimate ----------------------------------------------

test_that("a covariate glm() cannot estimate leaves the confidence set as without it", {
  set.seed(8)
  d <- data.frame(center = sample(LETTERS[1:6], 3000, TRUE), x = stats::runif(3000, 0, 10))
  # constant within each center, so the center effects already hold it
  d$region <- ifelse(d$center %in% c("A", "B", "C"), "north", "south")
  shift <- c(A = 0, B = 0.2, C = -0.2, D = 0.3, E = 0.1, F = -0.1)[d$center]
  d$yb <- stats::rbinom(3000, 1, stats::plogis(-2 + 0.25 * d$x + shift))
  d$yc <- 3 + 0.5 * d$x + shift + stats::rnorm(3000)
  run <- function(type, covariate) {
    hunt3_quiet(lago_optimization(
      data = d, outcome_name = if (type == "binary") "yb" else "yc", outcome_type = type,
      intervention_components = "x", include_center_effects = TRUE,
      center_weights_for_outcome_goal = c(0, 0, 1, 0, 0, 0), additional_covariates = covariate,
      intervention_lower_bounds = 0, intervention_upper_bounds = 10,
      cost_list_of_vectors = list(c(0, 1)), outcome_goal = if (type == "binary") 0.5 else 6,
      confidence_set_grid_step_size = 0.5, quiet = TRUE
    ))
  }
  for (type in c("binary", "continuous")) {
    with_region <- run(type, "region")
    without <- run(type, NULL)
    expect_true(is.na(stats::coef(with_region$model)[["regionsouth"]]))
    expect_equal(with_region$est_outcome_ci, without$est_outcome_ci)
    expect_equal(with_region$cs, without$cs)
  }
  # an intervention component glm() cannot estimate is not dropped, so the
  # confidence set stays empty rather than treating it as having no effect
  set.seed(3)
  dd <- data.frame(x1 = stats::runif(400, 0, 10))
  dd$x2 <- 2 * dd$x1
  dd$y <- stats::rbinom(400, 1, stats::plogis(-2 + 0.3 * dd$x1))
  fit <- stats::glm(y ~ x1 + x2, data = dd, family = stats::binomial)
  cs <- suppressWarnings(get_confidence_set(
    predictors_data = dd[, c("x1", "x2")], intervention_components = c("x1", "x2"),
    outcome_data = dd$y, fitted_model = fit, link = "logit", outcome_goal = 0.5,
    outcome_type = "binary", intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 20), confidence_set_grid_step_size = c(1, 5),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = c(6.7, 0)
  ))
  expect_equal(cs$confidence_set_size_percentage, 0)
  expect_null(cs$cs)
})

# ---- column types ---------------------------------------------------------------------

test_that("a factor outcome and a logical component are refused clearly", {
  set.seed(2)
  d <- data.frame(x1 = stats::runif(300, 0, 10), x2 = stats::runif(300) > 0.5)
  d$y <- stats::rbinom(300, 1, stats::plogis(-2.5 + 0.3 * d$x1 + d$x2))
  args <- list(
    outcome_name = "y", outcome_type = "binary", intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 1),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.6,
    include_confidence_set = FALSE, quiet = TRUE
  )
  factor_outcome <- d
  factor_outcome$y <- factor(ifelse(d$y == 1, "yes", "no"))
  factor_outcome$x2 <- as.numeric(d$x2)
  expect_error(
    do.call(lago_optimization, c(list(data = factor_outcome), args)),
    "The outcome column 'y' must be numeric"
  )
  expect_error(
    do.call(lago_optimization, c(list(data = d), args)),
    "'x2' must be numeric columns"
  )
})
