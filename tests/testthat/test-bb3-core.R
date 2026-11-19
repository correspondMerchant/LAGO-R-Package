# Tests for the fixes of the third bug bash round. Each fails before its fix.

bb3_vi <- function(...) {
  args <- list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    outcome_goal = 40, outcome_goal_intention = "maximize", power_goal = NULL,
    power_goal_approach = "unconditional",
    cost_list_of_vectors = list(c(0, 4), c(4, 6))
  )
  dots <- list(...)
  args[names(dots)] <- dots
  suppressWarnings(suppressMessages(do.call(LAGOtrials:::validate_inputs, args)))
}

bb3_clustered <- function() {
  set.seed(9)
  d <- expand.grid(i = 1:8, center = paste0("c", 1:8), stringsAsFactors = FALSE)
  d$group <- ifelse(d$center %in% paste0("c", 1:4), "control", "treatment")
  d$dose <- ifelse(d$group == "treatment", 1, 0)
  d$y <- stats::rbinom(nrow(d), 1, ifelse(d$group == "treatment", 0.5, 0.3))
  d
}

test_that("a clustered power goal works on individual level data", {
  d <- bb3_clustered()
  r <- suppressWarnings(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = "dose", intervention_lower_bounds = 0,
    intervention_upper_bounds = 1, cost_list_of_vectors = list(c(0, 1)),
    power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 20, icc = 0.05,
    power_goal_cluster_id = "center", include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_true(is.finite(r$effective_outcome_goal))
})

test_that("center level power checks count arms and centers with patients", {
  d <- bb3_clustered()
  cl <- stats::aggregate(y ~ center + group + dose, d, mean)
  names(cl)[names(cl) == "y"] <- "proportion"
  cl$center_sample_size <- 8
  base <- list(
    input_data_structure = "center_level", outcome_name = "proportion",
    outcome_type = "binary", intervention_components = "dose",
    intervention_lower_bounds = 0, intervention_upper_bounds = 1,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = NULL, power_goal = 0.8,
    num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 20,
    include_center_effects = FALSE
  )
  no_patients <- cl
  no_patients$center_sample_size[no_patients$group == "treatment"] <- 0
  expect_error(
    do.call(bb3_vi, c(base, list(data = no_patients))),
    "'treatment' arm of the 'group' column has no rows with an observed outcome"
  )
  one_size <- cl
  one_size$center_sample_size[one_size$center %in% c("c2", "c3", "c4")] <- NA
  expect_error(
    do.call(bb3_vi, c(base, list(
      data = one_size, icc = 0.05, power_goal_cluster_id = "center"
    ))),
    "with an observed outcome and patients"
  )
})

test_that("the power control rate is held within 0 and 1", {
  d <- make_small_clustered()
  goal <- function(rate, approach) {
    suppressWarnings(get_power_desired_outcome(
      data = d, intervention_components_coeff = small_coeff(d),
      power_goal = 0.8, power_goal_approach = approach,
      num_centers_in_next_stage = 20, patients_per_center_in_next_stage = 20,
      outcome_name = "y", control_rate = rate
    ))
  }
  for (approach in c("unconditional", "conditional")) {
    expect_equal(goal(-0.05, approach), goal(0, approach))
    expect_gte(goal(-0.05, approach), 0)
  }
})

test_that("the numerical recommendation does not depend on the outcome's units", {
  set.seed(2)
  d <- data.frame(x1 = stats::runif(300, 0, 10), x2 = stats::runif(300, 0, 5))
  d$y <- 1 + 0.3 * d$x1 + 0.4 * d$x2 + stats::rnorm(300)
  rec <- function(s) {
    dd <- d
    dd$y <- d$y * s
    suppressWarnings(lago_optimization(
      data = dd, outcome_name = "y", outcome_type = "continuous",
      intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 5), cost_list_of_vectors = list(c(0, 1), c(0, 2)),
      outcome_goal = 6 * s, include_confidence_set = FALSE, quiet = TRUE
    ))$rec_int
  }
  base <- rec(1)
  for (s in c(1e-9, 1e9)) expect_equal(rec(s), base, tolerance = 1e-4)
})

test_that("prev_recommended_interventions with NA is refused clearly", {
  expect_error(
    bb3_vi(prev_recommended_interventions = c(NA, 2)),
    "prev_recommended_interventions must be finite numbers"
  )
  expect_error(
    bb3_vi(prev_recommended_interventions = c(20, 2)),
    "The prev_recommended_interventions\\[ 1 \\]"
  )
})

test_that("weights, goals and flags are checked for NA and length", {
  w <- rep(1, nrow(mtcars))
  expect_error(bb3_vi(weights = replace(w, 1, -1)), "must not be negative or infinite")
  expect_error(bb3_vi(outcome_goal = NA_real_), "a single finite number")
  expect_error(bb3_vi(outcome_goal = c(30, 40)), "a single finite number")
  expect_error(bb3_vi(confidence_set_alpha = c(0.05, 0.1)), "a single number")
  expect_error(bb3_vi(include_confidence_set = NA), "must be a boolean")
})

test_that("all zero weights are refused with a clear message without effects", {
  expect_error(
    suppressWarnings(lago_optimization(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(1.5, 15),
      intervention_upper_bounds = c(5, 22), outcome_goal = 22,
      cost_list_of_vectors = list(c(0, 4), c(0, 6)), weights = rep(0, nrow(mtcars)),
      include_confidence_set = FALSE, quiet = TRUE
    )),
    "No row has a positive glm weight \\('weights'\\)"
  )
  cl <- data.frame(proportion = c(0.2, 0.4, 0.5), center_sample_size = 0, x = 1:3)
  expect_error(
    suppressWarnings(lago_optimization(
      data = cl, input_data_structure = "center_level", outcome_name = "proportion",
      outcome_type = "binary", intervention_components = "x",
      intervention_lower_bounds = 1, intervention_upper_bounds = 3, outcome_goal = 0.5,
      cost_list_of_vectors = list(c(0, 1)), include_confidence_set = FALSE,
      include_center_effects = FALSE, quiet = TRUE
    )),
    "No row has a positive glm weight \\(center_sample_size\\)"
  )
})

test_that("named center weights work with an explicit NA center", {
  d <- mtcars
  d$center <- addNA(factor(ifelse(seq_len(nrow(d)) <= 8, NA, d$cyl)))
  fit <- function(w) {
    suppressWarnings(lago_optimization(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(1.5, 15),
      intervention_upper_bounds = c(5, 22), outcome_goal = 22,
      cost_list_of_vectors = list(c(0, 4), c(0, 6)), include_center_effects = TRUE,
      center_weights_for_outcome_goal = w, include_confidence_set = FALSE, quiet = TRUE
    ))$est_outcome
  }
  plain <- fit(c(0.1, 0.2, 0.3, 0.4))
  named <- c(0.4, 0.3, 0.2, 0.1)
  names(named) <- c("NA", "8", "6", "4")
  expect_equal(fit(named), plain)
  names(named)[1] <- NA
  expect_equal(fit(named), plain)
})

test_that("a date-time period value is matched by its label", {
  d <- mtcars
  d$period <- as.POSIXct(ifelse(seq_len(nrow(d)) %% 2 == 0, "2024-01-01", "2024-04-01"), tz = "UTC")
  fit <- function(v) {
    suppressWarnings(lago_optimization(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(1.5, 15),
      intervention_upper_bounds = c(5, 22), outcome_goal = 22,
      cost_list_of_vectors = list(c(0, 4), c(0, 6)), include_time_effects = TRUE,
      time_effect_optimization_value = v, include_confidence_set = FALSE, quiet = TRUE
    ))$est_outcome
  }
  expect_equal(fit(as.POSIXct("2024-04-01", tz = "UTC")), fit("2024-04-01"))
})

test_that("the cost type note does not come back in a sweep from a result", {
  r <- suppressWarnings(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(1.5, 15),
    intervention_upper_bounds = c(5, 22), outcome_goal = 22,
    cost_list_of_vectors = list(c(0, 4), c(0, 6)), include_confidence_set = FALSE,
    quiet = TRUE
  ))
  expect_null(attr(r, "lago_call_args")$default_cost_fxn_type)
  found <- character(0)
  withCallingHandlers(
    suppressWarnings(lago_sensitivity(r, parameter = "outcome_goal", values = c(21, 22), quiet = FALSE)),
    message = function(m) {
      found <<- c(found, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("default_cost_fxn_type", found, fixed = TRUE)))
})

test_that("a direct get_confidence_set() call checks names, costs, bounds and steps", {
  set.seed(3)
  d <- data.frame(x1 = stats::runif(200, 0, 10), x2 = stats::runif(200, 0, 5))
  d$yb <- stats::rbinom(200, 1, stats::plogis(-2 + 0.2 * d$x1 + 0.2 * d$x2))
  m <- stats::glm(yb ~ x1 + x2, binomial, d)
  cs <- function(...) {
    args <- list(
      predictors_data = d[c("x1", "x2")], intervention_components = c("x1", "x2"),
      outcome_data = d$yb, fitted_model = m, link = "logit",
      outcome_goal = 0.6, outcome_type = "binary",
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 5),
      confidence_set_grid_step_size = c(5, 5),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = c(4.3, 2.1)
    )
    dots <- list(...)
    args[names(dots)] <- dots
    do.call(get_confidence_set, args)
  }
  plain <- cs()$rec_int_ci
  expect_equal(cs(rec_int = c(x2 = 2.1, x1 = 4.3))$rec_int_ci, plain)
  expect_error(cs(rec_int = c(a = 1, b = 2)), "names of rec_int must be")
  expect_error(cs(cost_list_of_vectors = list(c(0, 1))), "one cost vector per component")
  expect_error(cs(intervention_upper_bounds = c(10, NA)), "intervention_upper_bounds must be 2")
  expect_error(cs(confidence_set_grid_step_size = 1), "confidence_set_grid_step_size must be 2")
  expect_error(cs(intervention_lower_bounds = c(11, 0)), "must not be above")
  expect_error(cs(confidence_set_grid_step_size = c(0, 1)), "must be above 0")
  # an error inside names the arguments, not their values
  err <- tryCatch(cs(outcome_type = "other"), error = function(e) e)
  expect_lt(nchar(paste(deparse(conditionCall(err)), collapse = "")), 2000)
})

test_that("a goal of 0 is optimized on the scale of 1", {
  set.seed(5)
  d <- data.frame(x = stats::runif(200, 0, 4))
  d$y <- -1 + 0.5 * d$x + stats::rnorm(200, sd = 0.2)
  r <- suppressWarnings(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = "x", intervention_lower_bounds = 0,
    intervention_upper_bounds = 4, cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = 0, include_confidence_set = FALSE, quiet = TRUE
  ))
  b <- stats::coef(r$model)
  expect_equal(r$rec_int, unname(-b[1] / b[2]), tolerance = 1e-5)
})

test_that("names of a direct rec_int are checked against the main components", {
  set.seed(3)
  d <- data.frame(x1 = stats::runif(200, 0, 10), x2 = stats::runif(200, 0, 5))
  d$`x1:x2` <- d$x1 * d$x2
  d$yb <- stats::rbinom(200, 1, stats::plogis(-2 + 0.2 * d$x1 + 0.2 * d$x2))
  m <- stats::glm(yb ~ x1 + x2 + `x1:x2`, binomial, d)
  expect_error(
    get_confidence_set(
      predictors_data = d[c("x1", "x2", "x1:x2")], intervention_components = c("x1", "x2", "x1:x2"),
      include_interaction_terms = TRUE, main_components = c("x1", "x2"),
      outcome_data = d$yb, fitted_model = m, link = "logit", outcome_goal = 0.6,
      outcome_type = "binary", intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(5, 5),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = c(a = 1, b = 2)
    ),
    "names of rec_int must be the main components"
  )
})
