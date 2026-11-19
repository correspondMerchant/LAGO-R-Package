# Tests for the fixes of the second package-wide bug bash. Each test fails on
# the code before its fix.

bb2_cars <- function() {
  d <- mtcars
  d$center <- factor(d$cyl)
  d
}

bb2_fit <- function(...) {
  suppressWarnings(suppressMessages(lago_optimization(
    data = bb2_cars(), outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", intervention_components = c("wt", "qsec"),
    intervention_lower_bounds = c(1.5, 15), intervention_upper_bounds = c(5, 22),
    outcome_goal = 22, cost_list_of_vectors = list(c(0, 4), c(0, 6)),
    include_confidence_set = FALSE, ...
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
      time_effect_optimization_value = value
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
