# Results checked against independent calculations, from a fourth bug hunt.

hunt4_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

hunt4_warnings <- function(expr) {
  warnings <- character(0)
  value <- withCallingHandlers(suppressMessages(expr), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

# ---- the power goal ------------------------------------------------------------

# stage 1 where the treatment arm starts lower than control
hunt4_power_data <- function() {
  data.frame(
    group = rep(c("control", "treatment"), each = 200),
    y = c(rep(1:0, c(100, 100)), rep(1:0, c(40, 160)))
  )
}

test_that("the unconditional power goal asks for power to show benefit", {
  d <- hunt4_power_data()
  # the next stage of 100 centers of 10 can show benefit
  goal <- LAGOtrials:::get_power_desired_outcome(
    d, 0, 0.8, "unconditional", 100, 10, "y", control_rate = 0.5
  )
  # the signed z of the two arms at the goal, by hand
  n2 <- 500
  p1 <- (40 + n2 * goal) / (200 + n2)
  p0 <- (100 + n2 * 0.5) / (200 + n2)
  z <- (p1 - p0) / sqrt(p1 * (1 - p1) / (200 + n2) + p0 * (1 - p0) / (200 + n2))
  ncp <- stats::uniroot(function(x) 1 - stats::pchisq(stats::qchisq(0.95, 1), 1, x) - 0.8, c(0, 100))$root
  expect_gt(z, 0)
  # the goal is the first point of a grid of 1000 that has the power
  expect_equal(z, sqrt(ncp), tolerance = 1e-2)
  # a next stage too small to show benefit is reported, not a harmful goal
  expect_warning(
    small <- LAGOtrials:::get_power_desired_outcome(
      d, 0, 0.8, "unconditional", 10, 10, "y", control_rate = 0.5
    ),
    "power goal cannot be reached"
  )
  expect_equal(small, 1)
})

test_that("an unreachable conditional power goal is reported and not set to 0", {
  d <- hunt4_power_data()
  expect_warning(
    goal <- LAGOtrials:::get_power_desired_outcome(
      d, 0, 0.8, "conditional", 10, 10, "y", control_rate = 0.5
    ),
    "power goal cannot be reached"
  )
  expect_equal(goal, 1)
})

# ---- shrinking --------------------------------------------------------------------

test_that("the shrunk recommendation does not depend on a component's units", {
  set.seed(1)
  d <- data.frame(x1 = stats::runif(1200, 0, 4), x2 = stats::runif(1200, 0, 2))
  d$y <- stats::rbinom(1200, 1, stats::plogis(-1 + 0.3 * d$x1 + 0.5 * d$x2))
  goal <- 0.75
  # Nevo's shrinking formula by hand for x1, with x2 at its upper bound
  b <- stats::coef(stats::glm(y ~ x1 + x2, stats::binomial, d))
  b1max <- (stats::qlogis(goal) - b[[1]] - b[[3]] * 2) / 4
  b1min <- b1max / 2
  x1_hand <- mean(d$x1) + (4 - mean(d$x1)) * (b[[2]] - b1min) / (b1max - b1min)
  for (k in c(1, 1e5, 1e7)) {
    dk <- d
    dk$x1 <- d$x1 * k
    r <- hunt4_quiet(lago_optimization(
      data = dk, outcome_name = "y", outcome_type = "binary",
      intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(4 * k, 2), cost_list_of_vectors = list(c(0, 3 / k, 0.5 / k^2), c(0, 5)),
      outcome_goal = goal, include_confidence_set = FALSE, quiet = TRUE
    ))
    expect_true(r$shrinking_method_used)
    expect_equal(r$rec_int[1] / k, x1_hand, tolerance = 1e-5)
  }
})

# ---- confidence sets ---------------------------------------------------------------

test_that("a rare outcome keeps the digits of its interval", {
  set.seed(9)
  d <- data.frame(x = stats::runif(40000, 0, 10))
  d$y <- stats::rbinom(40000, 1, stats::plogis(-6.5 + 0.05 * d$x))
  r <- hunt4_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 10,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.0023,
    confidence_set_grid_step_size = 0.25, quiet = TRUE
  ))
  # the delta method interval at the recommendation, by hand
  fit <- r$model
  x <- c(1, r$rec_int)
  eta <- sum(x * stats::coef(fit))
  p <- stats::plogis(eta)
  se <- sqrt(drop(t(x) %*% stats::vcov(fit) %*% x)) * p * (1 - p)
  expected <- p + c(-1, 1) * stats::qnorm(0.975) * se
  expect_equal(unname(unlist(r$est_outcome_ci)), signif(expected, 4), tolerance = 1e-6)
  # every row shown in the confidence set holds the goal in its shown interval
  expect_true(all(r$cs$CI_lower_bound <= 0.0023 & 0.0023 <= r$cs$CI_upper_bound))
})

test_that("a binary confidence set with four components does not run out of memory", {
  set.seed(1)
  d <- as.data.frame(matrix(stats::runif(2000, 0, 2), ncol = 4))
  names(d) <- paste0("x", 1:4)
  d$y <- stats::rbinom(500, 1, stats::plogis(-2 + rowSums(d) * 0.3))
  r <- hunt4_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = paste0("x", 1:4), intervention_lower_bounds = rep(0, 4),
    intervention_upper_bounds = rep(2, 4), cost_list_of_vectors = rep(list(c(0, 1)), 4),
    # the default confidence set grid has 21^4 + 1 rows
    outcome_goal = 0.5, quiet = TRUE
  ))
  expect_true(is.numeric(r$confidence_set_size_percentage))
})

test_that("get_confidence_set() takes a single clustering vector and refuses three", {
  set.seed(2)
  d <- data.frame(site = factor(rep(1:8, each = 25)), x = stats::runif(200, 0, 4))
  d$y <- 1 + 0.5 * d$x + stats::rnorm(8)[d$site] + stats::rnorm(200)
  fit <- stats::glm(y ~ x, data = d)
  call_cs <- function(cluster_id) {
    get_confidence_set(
      predictors_data = d[, "x", drop = FALSE], intervention_components = "x",
      outcome_data = d$y, fitted_model = fit, link = "identity", outcome_goal = 2.5,
      outcome_type = "continuous", intervention_lower_bounds = 0, intervention_upper_bounds = 4,
      confidence_set_grid_step_size = 0.5, cost_list_of_vectors = list(c(0, 1)),
      cluster_id = cluster_id, rec_int = 3
    )
  }
  expect_equal(call_cs(d$site), call_cs(list(d$site)))
  expect_error(call_cs(list(d$site, d$site, d$site)), "at most two clustering vectors")
})

# ---- model and inputs ----------------------------------------------------------------

test_that("the quasibinomial family takes the logit link by default", {
  d <- as.data.frame(BB_proportions)
  args <- list(
    data = d, outcome_name = "EBP_proportions", outcome_type = "continuous",
    glm_family = "quasibinomial", intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = 0.7,
    include_confidence_set = FALSE, quiet = TRUE
  )
  default_link <- hunt4_quiet(do.call(lago_optimization, args))
  logit <- hunt4_quiet(do.call(lago_optimization, c(args, list(link = "logit"))))
  expect_equal(default_link$rec_int, logit$rec_int)
})

test_that("a column name that is not a valid R name is refused clearly", {
  d <- mtcars
  names(d)[names(d) == "gear"] <- "gear count"
  expect_error(
    lago_optimization(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("gear count", "qsec"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = 30, quiet = TRUE
    ),
    "not valid R names"
  )
})

hunt4_center_data <- function() {
  set.seed(6)
  d <- data.frame(center = factor(rep(paste0("c", 1:6), each = 60)), x = stats::runif(360, 0, 5))
  d$y <- stats::rbinom(360, 1, stats::plogis(-1.5 + 0.4 * d$x + c(-1, -0.5, 0, 0.5, 1, 0.2)[as.integer(d$center)]))
  d
}

test_that("a named center given with center weights is reported as ignored", {
  run <- hunt4_warnings(lago_optimization(
    data = hunt4_center_data(), outcome_name = "y", outcome_type = "binary",
    intervention_components = "x", include_center_effects = TRUE,
    center_effects_optimization_values = "c4", center_weights_for_outcome_goal = rep(1 / 6, 6),
    intervention_lower_bounds = 0, intervention_upper_bounds = 5,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.6,
    include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_true(any(grepl("named center is ignored", run$warnings)))
})

test_that("weights given with center level data are reported as ignored", {
  set.seed(7)
  cl <- data.frame(center = rep(paste0("c", 1:8), 3), x = stats::runif(24, 0, 5), center_sample_size = 50)
  cl$proportion <- stats::rbinom(24, 50, stats::plogis(-1 + 0.3 * cl$x)) / 50
  run <- hunt4_warnings(lago_optimization(
    data = cl, input_data_structure = "center_level", outcome_name = "proportion",
    outcome_type = "binary", intervention_components = "x", weights = rep(2, 24),
    intervention_lower_bounds = 0, intervention_upper_bounds = 5,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.6,
    include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_true(any(grepl("'weights' is ignored for center level data", run$warnings)))
})

test_that("the goal direction check weighs center level rows by their patients", {
  # one large center near 0.2 and three small ones near 0.9
  cl <- data.frame(
    center = rep(paste0("c", 1:4), 2), x = c(1, 2, 3, 4, 2, 3, 4, 5),
    center_sample_size = rep(c(400, 10, 10, 10), 2),
    proportion = c(0.2, 0.9, 0.9, 0.9, 0.25, 0.8, 0.9, 1)
  )
  run <- hunt4_warnings(lago_optimization(
    data = cl, input_data_structure = "center_level", outcome_name = "proportion",
    outcome_type = "binary", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 5,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.5,
    include_confidence_set = FALSE, quiet = TRUE
  ))
  # the patient mean is about 0.25, below the goal, so no direction warning
  expect_false(any(grepl("observed mean of the outcome", run$warnings)))
})

# ---- sweeps and printing ------------------------------------------------------------

hunt4_mtcars <- function(..., include_confidence_set = FALSE) {
  hunt4_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    include_confidence_set = include_confidence_set, quiet = TRUE, ...
  ))
}

test_that("a component named like a sweep result column is refused", {
  d <- mtcars
  names(d)[names(d) == "gear"] <- "value"
  expect_error(
    lago_sensitivity(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("value", "qsec"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = 30, parameter = "outcome_goal", values = c(30, 35), quiet = TRUE
    ),
    "name of a column"
  )
})

test_that("the sweep summary pairs each cost with its own value", {
  r <- hunt4_mtcars(outcome_goal = 30)
  s <- hunt4_quiet(lago_sensitivity(r, parameter = "outcome_goal", values = c(40, 35, 30)))
  printed <- paste(cli::cli_fmt(utils::capture.output(print(s))), collapse = " ")
  printed <- paste(c(printed, cli::cli_fmt(print(s))), collapse = " ")
  low <- format(s$rec_int_cost[s$value == 30])
  high <- format(s$rec_int_cost[s$value == 40])
  expect_match(printed, paste0(low, " at outcome_goal = 30"), fixed = TRUE)
  expect_match(printed, paste0(high, " at outcome_goal = 40"), fixed = TRUE)
})

test_that("the sweep baseline is found for a value built with seq()", {
  r <- hunt4_mtcars(outcome_goal = 30)
  # seq() gives 30.000000000000004, not 30
  values <- seq(0.1, 0.5, by = 0.1) * 100
  expect_false(30 %in% values)
  s <- hunt4_quiet(lago_sensitivity(r, parameter = "outcome_goal", values = values))
  expect_false(is.null(attr(s, "baseline")))
})

test_that("an unreachable goal says the shrinking method left out the confidence set", {
  r <- hunt4_mtcars(outcome_goal = 1000, include_confidence_set = TRUE)
  expect_true(r$shrinking_method_used)
  utils::capture.output(lines <- cli::cli_fmt(print(r)))
  printed <- paste(lines, collapse = " ")
  expect_match(printed, "shrinking method was used", fixed = TRUE)
  expect_no_match(printed, "set include_confidence_set = TRUE", fixed = TRUE)
})

test_that("a minimize budget that does not bind says minimum", {
  b <- hunt4_quiet(lago_budget(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(6, 25), cost_list_of_vectors = list(c(0, 4), c(0, 2)),
    outcome_goal_intention = "minimize", budget = 1000, n_grid = 5
  ))
  skip_if(b$binding)
  printed <- paste(cli::cli_fmt(print(b)), collapse = " ")
  expect_match(printed, "minimum reachable outcome", fixed = TRUE)
})

test_that("lago_budget() does not count a run shrunk below its power goal as reached", {
  d <- hunt4_power_data()
  d$x <- ifelse(d$group == "treatment", stats::runif(400, 0, 2), 0)
  run <- hunt4_warnings(lago_budget(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = "x", intervention_lower_bounds = 0,
    intervention_upper_bounds = 2, cost_list_of_vectors = list(c(0, 1)),
    power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 10, budget = 100, n_grid = 5
  ))
  expect_false(run$value$feasible)
  expect_true(any(grepl("power goal", run$warnings)))
})

# ---- the pinned cubic cost and the dose units --------------------------------------------

test_that("a cubic cost pinned at a measured dose does not depend on the dose units", {
  total <- function(coefs, x) sum(coefs * x^(0:4))
  base <- LAGOtrials:::cost_fxn_calculator(0, 5, 2, "cubic", unit_cost_doses = 2)[[1]]
  for (s in c(0.01, 60)) {
    scaled <- LAGOtrials:::cost_fxn_calculator(0, 5 * s, 2 / s, "cubic", unit_cost_doses = 2 * s)[[1]]
    for (x in c(1, 3, 5)) expect_equal(total(scaled, x * s), total(base, x), tolerance = 1e-8)
  }
})

test_that("a budget below the cheapest reachable cost says to increase it", {
  b <- hunt4_quiet(lago_budget(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(6, 25), cost_list_of_vectors = list(c(5, 4), c(5, 2)),
    outcome_goal_intention = "maximize", budget = 1, n_grid = 5
  ))
  expect_false(b$feasible)
  expect_gt(nrow(b$frontier), 0)
  printed <- paste(cli::cli_fmt(print(b)), collapse = " ")
  expect_match(printed, "increase the budget", fixed = TRUE)
})

test_that("a component named like a budget search column is refused", {
  d <- mtcars
  names(d)[names(d) == "qsec"] <- "est_outcome"
  expect_error(
    lago_budget(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("gear", "est_outcome"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal_intention = "maximize", budget = 60
    ),
    "name of a column lago_budget\\(\\) uses in its search"
  )
})
