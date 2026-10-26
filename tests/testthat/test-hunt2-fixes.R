# Results checked against independent calculations, from a second bug hunt.

hunt2_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# ---- the interval keeps its names on the averaged center path ---------------

test_that("the weighted average center's interval is named lower and upper", {
  d <- as.data.frame(BB_data)
  d$center <- d$site_name
  r <- hunt2_quiet(lago_optimization(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    glm_family = "binomial", intervention_components = c("coaching_updt", "launch_duration"),
    include_center_effects = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 5), cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.7, confidence_set_grid_step_size = c(4, 1), quiet = TRUE
  ))
  expect_identical(names(r$est_outcome_ci), c("lower", "upper"))
  expect_no_error(utils::capture.output(print(r), summary(r)))
})

# ---- the printed confidence level -------------------------------------------

test_that("print() and summary() label the interval with its confidence level", {
  r <- hunt2_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 40,
    confidence_set_alpha = 0.2, confidence_set_grid_step_size = c(1, 1), quiet = TRUE
  ))
  printed <- paste(cli::cli_fmt(print(r)), collapse = "\n")
  expect_match(printed, "80% CI for the estimated outcome", fixed = TRUE)
  expect_match(printed, "80% confidence set size", fixed = TRUE)
  expect_no_match(printed, "95%", fixed = TRUE)
  expect_identical(LAGOtrials:::.lago_confidence_label(r), "80%")
  small <- structure(list(), lago_call_args = list(confidence_set_alpha = 1e-5))
  expect_identical(LAGOtrials:::.lago_confidence_label(small), "99.999%")
})

# ---- lago_budget() ------------------------------------------------------------

budget_mtcars <- function(upper, budget, ...) {
  hunt2_quiet(lago_budget(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = upper,
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal_intention = "maximize",
    budget = budget, ...
  ))
}

# the best outcome with cost 4 * gear + 4 + 6 * qsec <= budget, by brute force
budget_mtcars_oracle <- function(upper, budget) {
  b <- stats::coef(stats::glm(mpg ~ gear + qsec, data = mtcars))
  grid <- expand.grid(
    gear = seq(0, upper[1], length.out = 201),
    qsec = seq(0, upper[2], length.out = 2001)
  )
  grid <- grid[4 * grid$gear + 4 + 6 * grid$qsec <= budget, ]
  max(b[[1]] + b[[2]] * grid$gear + b[[3]] * grid$qsec)
}

test_that("lago_budget() refines the right gap when goals tie on the outcome", {
  d <- as.data.frame(BB_data)
  r <- hunt2_quiet(lago_budget(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    glm_family = "binomial", intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100", center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal_intention = "maximize",
    budget = 10300
  ))
  # with linear costs the best spend is coaching at 1 and the rest on launch_duration
  fit <- stats::glm(pp3_oxytocin_mother ~ coaching_updt + launch_duration + birth_volume_100,
    family = stats::binomial, data = d
  )
  oracle <- unname(stats::predict(fit,
    data.frame(coaching_updt = 1, launch_duration = (10300 - 1700) / 8000, birth_volume_100 = 1.75),
    type = "response"
  ))
  expect_lte(r$rec_int_cost, 10300)
  expect_gt(r$est_outcome, oracle - 0.005)
  expect_lt(abs(r$outcome_goal - r$est_outcome), 0.04)
})

test_that("lago_budget() extends its goal grid far enough to bracket the boundary", {
  r <- budget_mtcars(c(10, 3500), 1e6)
  expect_equal(r$est_outcome, budget_mtcars_oracle(c(10, 3500), 1e6), tolerance = 0.01)
})

test_that("lago_budget() still refines with n_grid = 2", {
  r <- budget_mtcars(c(10, 350), 100, n_grid = 2)
  # two goals span the observed range padded by half a span on each side, and
  # the refinement puts two more inside the gap, a third of that span apart
  span <- 2 * diff(range(mtcars$mpg))
  expect_lt(abs(r$est_outcome - budget_mtcars_oracle(c(10, 350), 100)), span / 3)
  expect_gt(nrow(r$frontier), 2)
  expect_false(anyDuplicated(r$frontier$outcome_goal) > 0)
})

# ---- center level default weights --------------------------------------------

test_that("center level default weights count all of a center's patients", {
  p <- main_pulesa_data
  p$center <- p$Clinic
  p$period <- p$Period
  p$proportion <- p$Proportions
  p$center_sample_size <- p$Total_visit
  run <- function(data) {
    hunt2_quiet(lago_optimization(
      data = data, input_data_structure = "center_level", outcome_name = "proportion",
      outcome_type = "binary", intervention_components = c("AccessMedicines", "HypertensionTraining"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(8, 8),
      cost_list_of_vectors = list(c(0, 1), c(0, 2)), outcome_goal = 0.6,
      include_center_effects = TRUE, include_time_effects = TRUE,
      time_effect_optimization_value = 13, include_confidence_set = FALSE, quiet = TRUE
    ))
  }
  original <- run(p)
  reversed <- run(p[rev(seq_len(nrow(p))), ])
  # the numerical optimizer differs by about 1e-8 across platforms
  expect_equal(original$rec_int, reversed$rec_int, tolerance = 1e-6)
  # the weights the estimate uses are each center's share of all its visits
  fit <- original$model
  centers <- fit$xlevels$center
  w <- as.numeric(tapply(p$Total_visit, factor(p$center, levels = centers), sum, na.rm = TRUE))
  w <- w / sum(w)
  rows <- data.frame(
    center = factor(centers, levels = centers),
    period = factor("13", levels = fit$xlevels$period),
    AccessMedicines = original$rec_int[1], HypertensionTraining = original$rec_int[2]
  )
  estimate <- sum(w * stats::predict(fit, rows, type = "response"))
  expect_equal(estimate, original$est_outcome_goal, tolerance = 1e-6)
})

# ---- grid search includes the upper bound -------------------------------------

test_that("grid search tries the upper bound when the step does not divide the range", {
  d <- as.data.frame(BB_data)
  run <- function(goal) {
    hunt2_quiet(lago_optimization(
      data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = "birth_volume_100", center_characteristics_optimization_values = 1.75,
      intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = goal,
      outcome_goal_intention = "maximize", optimization_method = "grid_search",
      include_confidence_set = FALSE, quiet = TRUE
    ))
  }
  # the cheapest default-step grid point reaching 0.982, with the bounds added
  fit <- stats::glm(pp3_oxytocin_mother ~ coaching_updt + launch_duration + birth_volume_100,
    family = stats::binomial, data = d
  )
  steps <- c(diff(range(d$coaching_updt)), diff(range(d$launch_duration))) / 20
  grid <- expand.grid(
    coaching_updt = unique(c(seq(1, 40, by = steps[1]), 40)),
    launch_duration = unique(c(seq(1, 5, by = steps[2]), 5))
  )
  grid$birth_volume_100 <- 1.75
  grid$p <- stats::predict(fit, grid, type = "response")
  grid$cost <- 1700 * grid$coaching_updt + 8000 * grid$launch_duration
  ok <- grid[grid$p >= 0.982, ]
  r <- run(0.982)
  expect_equal(r$rec_int_cost, min(ok$cost), tolerance = 1e-8)
  expect_gte(r$est_outcome_goal, 0.982)
})
