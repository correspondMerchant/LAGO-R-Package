# Tests for the fixes of the second package-wide bug bash. Each test fails on
# the code before its fix.

bb2_cars <- function() {
  d <- mtcars
  d$center <- factor(d$cyl)
  d
}

# validate_inputs() on a small valid mtcars set, with one argument changed
bb2_vi <- function(...) {
  args <- list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    outcome_goal = 40, outcome_goal_intention = "maximize", power_goal = NULL,
    power_goal_approach = "unconditional",
    cost_list_of_vectors = list(c(0, 4), c(4, 6))
  )
  # replaced whole, as modifyList() would merge two data frames column by column
  dots <- list(...)
  args[names(dots)] <- dots
  suppressWarnings(suppressMessages(do.call(LAGOtrials:::validate_inputs, args)))
}

bb2_fit <- function(...) {
  suppressWarnings(suppressMessages(lago_optimization(
    data = bb2_cars(), outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", intervention_components = c("wt", "qsec"),
    intervention_lower_bounds = c(1.5, 15), intervention_upper_bounds = c(5, 22),
    outcome_goal = 22, cost_list_of_vectors = list(c(0, 4), c(0, 6)),
    include_confidence_set = FALSE, quiet = TRUE, ...
  )))
}

test_that("named center weights are matched to the centers by name", {
  by_order <- bb2_fit(
    include_center_effects = TRUE,
    center_weights_for_outcome_goal = c(0.2, 0.3, 0.5)
  )
  by_name <- bb2_fit(
    include_center_effects = TRUE,
    center_weights_for_outcome_goal = c("8" = 0.5, "4" = 0.2, "6" = 0.3)
  )
  expect_equal(by_name$rec_int, by_order$rec_int)
  expect_equal(by_name$est_outcome, by_order$est_outcome)
  expect_error(
    bb2_fit(
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = c(a = 0.2, b = 0.3, c = 0.5)
    ),
    "names of center_weights_for_outcome_goal must be the centers"
  )
})

test_that("a text or Date period can be chosen by its label", {
  d <- bb2_cars()
  d$center <- NULL
  labels <- c("2024-Q1", "2024-Q2")
  d$period <- labels[(seq_len(nrow(d)) %% 2) + 1]
  fit <- function(value, data = d) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = data, outcome_name = "mpg", outcome_type = "continuous",
      glm_family = "gaussian", intervention_components = c("wt", "qsec"),
      intervention_lower_bounds = c(1.5, 15),
      intervention_upper_bounds = c(5, 22), outcome_goal = 22,
      cost_list_of_vectors = list(c(0, 4), c(0, 6)),
      include_confidence_set = FALSE, include_time_effects = TRUE,
      time_effect_optimization_value = value, quiet = TRUE
    )))
  }
  q1 <- fit("2024-Q1")
  q2 <- fit("2024-Q2")
  expect_false(isTRUE(all.equal(q1$est_outcome, q2$est_outcome)))
  dd <- d
  dd$period <- as.Date(ifelse(d$period == "2024-Q1", "2024-01-01", "2024-04-01"))
  expect_equal(fit(as.Date("2024-04-01"), dd)$est_outcome, q2$est_outcome)
  # numeric labels written as "01" and "02" are found by their number
  dn <- d
  dn$period <- ifelse(d$period == "2024-Q1", "01", "02")
  expect_equal(fit(2, dn)$est_outcome, q2$est_outcome)
  expect_equal(fit(factor("2024-Q2"))$est_outcome, q2$est_outcome)
  expect_error(fit(NA), "must be one period")
})

test_that("include_time_effects = NA is refused with a clear message", {
  expect_error(bb2_fit(include_time_effects = NA), "must be TRUE or FALSE")
})

test_that("a Date characteristic is checked on its own scale", {
  rows <- data.frame(d = as.Date("2024-01-01") + c(0, 10, 20))
  expect_warning(
    LAGOtrials:::warn_outside_centers("d", as.numeric(as.Date("2025-01-01")), list(rows$d)),
    "outside the centers in the data"
  )
  expect_no_warning(
    LAGOtrials:::warn_outside_centers("d", as.numeric(as.Date("2024-01-05")), list(rows$d))
  )
})

test_that("center level extrapolation uses the rows with a positive sample size", {
  d <- data.frame(
    proportion = c(0.2, 0.4, 0.5, 0.7), center_sample_size = c(10, 10, 10, 0),
    x = c(1, 2, 3, 4), size = c(1, 2, 3, 100)
  )
  # the row with size 100 has no patients, so 50 is outside 1 to 3
  expect_warning(
    LAGOtrials:::warn_center_extrapolation(
      d, "proportion", "x", "size", 50, NULL, FALSE, FALSE, "center_level",
      weights = NULL
    ),
    "size = 50, while the data the model is fitted on has 1 to 3"
  )
})

test_that("weights, sizes and outcomes out of range are refused clearly", {
  w <- rep(1, nrow(mtcars))
  expect_error(bb2_vi(weights = replace(w, 1, Inf)), "must be finite and not negative")
  expect_error(bb2_vi(weights = replace(w, 1, -1)), "must be finite and not negative")
  d <- mtcars
  d$am2 <- d$am
  d$am2[1] <- 2
  expect_error(
    bb2_vi(data = d, outcome_name = "am2", outcome_type = "binary", outcome_goal = 0.5),
    "values outside 0 to 1, such as 2. A binary outcome must be 0 or 1."
  )
  cl <- data.frame(
    proportion = c(0.2, 1.4), center_sample_size = c(10, 10), gear = 1:2, qsec = 1:2
  )
  base <- list(
    data = cl, input_data_structure = "center_level", outcome_name = "proportion",
    outcome_type = "binary", outcome_goal = 0.5
  )
  expect_error(do.call(bb2_vi, base), "proportion of events in each row")
  base$data$proportion <- c(0.2, 0.4)
  base$data$center_sample_size <- c("10", "10")
  expect_error(do.call(bb2_vi, base), "'center_sample_size' column must hold numbers")
})

test_that("a missing arm is named in the power goal check and left out of the test", {
  d <- mtcars
  d$group <- ifelse(d$am == 1, "treatment", "control")
  d$group[1] <- NA
  expect_error(
    bb2_vi(
      data = d, outcome_name = "vs", outcome_type = "binary", outcome_goal = 0.5,
      power_goal = 0.8, num_centers_in_next_stage = 4,
      patients_per_center_in_next_stage = 10
    ),
    "1 row\\(s\\) have a missing value"
  )
  res <- LAGOtrials:::test_processor(d, "binary", "vs")
  expect_named(res, c("test_stat", "p_val"))
  d$group <- c("treatment", rep("control", nrow(d) - 1))
  expect_warning(
    expect_null(LAGOtrials:::test_processor(d, "binary", "vs")),
    "needs at least two rows"
  )
})

bb2_power <- function(power_goal, approach = "unconditional", j = 20, n = 20, ...) {
  d <- make_small_clustered()
  suppressWarnings(get_power_desired_outcome(
    data = d, intervention_components_coeff = small_coeff(d),
    power_goal = power_goal, power_goal_approach = approach,
    num_centers_in_next_stage = j, patients_per_center_in_next_stage = n,
    outcome_name = "y", ...
  ))
}

test_that("a power goal at or below alpha, or very close to 1, does not crash", {
  low <- bb2_power(0.03)
  expect_true(is.numeric(low) && is.finite(low))
  expect_no_error(bb2_power(1 - 2^-52))
})

test_that("the power goal is not rounded up to the next grid point", {
  d <- make_small_clustered()
  ctl <- stats::plogis(small_coeff(d)[[1]])
  grid <- seq(ctl, 1, length.out = 1000)
  for (approach in c("unconditional", "conditional")) {
    big <- bb2_power(0.8, approach, j = 1e4, n = 1e3)
    # between grid points, not on the first one that meets the goal
    expect_gt(min(abs(grid - big)), 1e-9)
    expect_gt(bb2_power(0.8, approach, j = 1e4, n = 1e3, icc = 0.01,
      power_goal_cluster_id = "center"), big)
  }
})

test_that("power inputs are checked whatever the icc", {
  d <- make_small_clustered()
  d$dose <- ifelse(d$group == "treatment", 1, 0)
  pv <- function(...) {
    bb2_vi(
      data = d, outcome_name = "y", outcome_type = "binary",
      intervention_components = "dose", intervention_lower_bounds = 0,
      intervention_upper_bounds = 1, outcome_goal = 0.5,
      cost_list_of_vectors = list(c(0, 1)), power_goal = 0.8,
      num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 10, ...
    )
  }
  expect_error(pv(power_goal_cluster_id = "nope"), "'nope' was not found")
  expect_error(pv(icc = 0, power_goal_cluster_id = "nope"), "'nope' was not found")
  expect_error(pv(power_goal_cluster_id = NA_character_), "single column name")
  expect_error(pv(power_goal = NA_real_), "a single number")
  expect_error(pv(power_goal = c(0.8, 0.9)), "a single number")
  expect_error(pv(num_centers_in_next_stage = Inf), "one whole, finite number")
  expect_error(pv(num_centers_in_next_stage = 2.5), "one whole, finite number")
  expect_error(pv(patients_per_center_in_next_stage = c(5, 6)), "one whole, finite number")
  expect_error(pv(icc = NA_real_), "icc must be in")
  expect_error(pv(shrinkage_threshold = NA_real_), "a single number")
  expect_error(
    pv(power_goal_approach = "other"),
    "^The power_goal_approach must be one of the supported approaches: 'unconditional' or 'conditional'\\.$"
  )
})

test_that("a direct get_confidence_set() call checks rec_int, alpha and the goal", {
  set.seed(3)
  d <- data.frame(x1 = runif(200, 0, 10), x2 = runif(200, 0, 5))
  d$yb <- stats::rbinom(200, 1, stats::plogis(-2 + 0.2 * d$x1 + 0.2 * d$x2))
  m <- stats::glm(yb ~ x1 + x2, binomial, d)
  cs <- function(...) {
    args <- list(
      predictors_data = d[c("x1", "x2")], intervention_components = c("x1", "x2"),
      outcome_data = d$yb, fitted_model = m, link = "logit",
      outcome_goal = 0.6, outcome_type = "binary",
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 5),
      confidence_set_grid_step_size = c(5, 5),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = c(8, 0)
    )
    dots <- list(...)
    args[names(dots)] <- dots
    do.call(get_confidence_set, args)
  }
  expect_length(cs()$rec_int_ci, 2)
  expect_error(cs(rec_int = NULL), "rec_int must be the recommended intervention, 2")
  expect_error(cs(rec_int = 8), "rec_int must be the recommended intervention, 2")
  expect_error(
    cs(
      include_interaction_terms = TRUE, main_components = c("x1", "x2"),
      intervention_components = c("x1", "x2", "x1:x2"), rec_int = 8
    ),
    "2 finite number\\(s\\), one per main component"
  )
  expect_error(cs(confidence_set_alpha = 0), "between 0 and 1")
  expect_error(cs(confidence_set_alpha = 1.5), "between 0 and 1")
  expect_error(cs(confidence_set_alpha = c(0.05, 0.5)), "between 0 and 1")
  expect_error(cs(outcome_goal = c(0.5, 0.7)), "single finite number")
})

test_that("a goal within round off of the best outcome is reached, not shrunk", {
  set.seed(83)
  d <- data.frame(x1 = stats::runif(300, 0.5, 4))
  d$y <- stats::rbinom(300, 1, stats::plogis(0.27 - 1.05 * d$x1))
  b <- stats::coef(stats::glm(y ~ x1, binomial, d))
  at <- function(x) unname(stats::plogis(b[1] + b[2] * x))
  # the goal is 1e-12 past the best outcome, which is at one bound
  cases <- list(
    list(intention = "maximize", goal = at(0.7) + 1e-12, best = 0.7),
    list(intention = "minimize", goal = at(3.7) - 1e-12, best = 3.7)
  )
  for (case in cases) {
    for (method in c("numerical", "grid_search")) {
      r <- suppressWarnings(lago_optimization(
        data = d, outcome_name = "y", outcome_type = "binary",
        intervention_components = "x1", intervention_lower_bounds = 0.7,
        intervention_upper_bounds = 3.7, outcome_goal = case$goal,
        outcome_goal_intention = case$intention,
        cost_list_of_vectors = list(c(0, 1)), optimization_method = method,
        include_confidence_set = FALSE, quiet = TRUE
      ))
      expect_false(r$shrinking_method_used)
      expect_equal(r$rec_int, case$best, tolerance = 1e-6)
    }
  }
})

test_that("the cost type note is shown only when the caller set the type", {
  notes <- function(...) {
    found <- character(0)
    withCallingHandlers(
      suppressWarnings(lago_optimization(
        data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
        intervention_components = c("wt", "qsec"),
        intervention_lower_bounds = c(1.5, 15), intervention_upper_bounds = c(5, 22),
        outcome_goal = 22, cost_list_of_vectors = list(c(0, 4), c(0, 6)),
        include_confidence_set = FALSE, ...
      )),
      message = function(m) {
        found <<- c(found, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    any(grepl("'default_cost_fxn_type' is ignored", found, fixed = TRUE))
  }
  expect_false(notes())
  expect_true(notes(default_cost_fxn_type = "linear"))
})

test_that("the bound warnings are input warnings, so a sweep shows them once", {
  args <- list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(4, 0), intervention_upper_bounds = c(10, 350),
    outcome_goal = 40, outcome_goal_intention = "maximize", power_goal = NULL,
    power_goal_approach = "unconditional",
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), quiet = TRUE
  )
  w <- tryCatch(do.call(LAGOtrials:::validate_inputs, args), warning = function(w) w)
  expect_s3_class(w, "lago_input_warning")
  expect_match(conditionMessage(w), "lower bound for the intervention component gear")
})
