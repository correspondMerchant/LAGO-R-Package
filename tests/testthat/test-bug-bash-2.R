# Tests for the fixes of the second package-wide bug bash. Each test fails on
# the code before its fix.

bb2_cars <- function() {
  d <- mtcars
  d$center <- factor(d$cyl)
  d
}

# validate_inputs() on a small valid mtcars set, with one argument changed
bb2_vi <- function(...) {
  args <- utils::modifyList(list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    outcome_goal = 40, outcome_goal_intention = "maximize", power_goal = NULL,
    power_goal_approach = "unconditional",
    cost_list_of_vectors = list(c(0, 4), c(4, 6))
  ), list(...))
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
  expect_error(bb2_vi(weights = w * 0), "at least one weight above 0")
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
