# Round 4 bug bash fixes for lago_sensitivity() and lago_budget().

bb4_sens_mt_args <- function(...) {
  utils::modifyList(list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 30,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE,
    quiet = TRUE
  ), list(...))
}

bb4_sens_mt_fit <- function(...) {
  suppressWarnings(suppressMessages(
    do.call(lago_optimization, bb4_sens_mt_args(...))
  ))
}

# a sensitivity result built by hand, to test print() and plot() alone
bb4_sens_fake <- function(value, cost, gear = seq_along(value),
                          qsec = seq_along(value), parameter = "outcome_goal") {
  df <- data.frame(
    value = value, gear = gear, qsec = qsec, rec_int_cost = cost,
    est_outcome_goal = rep(1, length(value)),
    status = ifelse(is.na(cost), "error", "ok"), stringsAsFactors = FALSE
  )
  attr(df, "parameter") <- parameter
  attr(df, "component_names") <- c("gear", "qsec")
  class(df) <- c("lago_sensitivity", "data.frame")
  df
}

# the cli lines of print(), with the data frame body kept off the test output
bb4_sens_printed <- function(s) {
  out <- NULL
  utils::capture.output(out <- cli::cli_fmt(print(s)))
  paste(out, collapse = " ")
}

test_that("lago_sensitivity() refuses sweeps that cannot change the recommendation", {
  fit <- bb4_sens_mt_fit()
  expect_error(
    lago_sensitivity(fit, parameter = "shrinkage_threshold", values = c(0.1, 0.5)),
    "\"shrinkage_threshold\" is not used by lago_optimization\\(\\)"
  )
  expect_error(
    lago_sensitivity(fit, parameter = "num_centers_in_next_stage", values = c(5, 10)),
    "\"num_centers_in_next_stage\" only affects the recommendation through the power goal"
  )
  expect_error(
    lago_sensitivity(fit, parameter = "patients_per_center_in_next_stage", values = c(5, 10)),
    "\"patients_per_center_in_next_stage\" only affects the recommendation through the power goal"
  )
  expect_error(
    lago_sensitivity(fit, parameter = "icc", values = c(0, 0.1)),
    "\"icc\" only affects the recommendation through the power goal"
  )
})

test_that("an unnamed argument next to `object` is refused, not dropped", {
  fit <- bb4_sens_mt_fit()
  expect_error(
    lago_sensitivity(fit, 1e4, parameter = "outcome_goal", values = 30),
    "Every argument in `...` must be named when `object` is given"
  )
  expect_error(
    lago_budget(fit, mtcars[1:10, ], budget = 100),
    "Every argument in `...` must be named when `object` is given"
  )
  # named overrides still work
  merged <- merge_call_args(list(a = 1, b = 2), list(b = 3))
  expect_identical(merged, list(a = 1, b = 3))
  expect_identical(merge_call_args(list(a = 1), list()), list(a = 1))
})

test_that("a data frame given first is refused with a note to name the data", {
  args <- bb4_sens_mt_args()
  expect_error(
    do.call(lago_sensitivity, c(list(mtcars), args[-1],
      list(parameter = "outcome_goal", values = 30))),
    "Name the data, as in data = my_data"
  )
  expect_error(
    do.call(lago_budget, c(list(mtcars), args[-1], list(budget = 100))),
    "Name the data, as in data = my_data"
  )
  expect_error(
    lago_sensitivity(list(1), parameter = "outcome_goal", values = 30),
    "`object` must be a `lago` result"
  )
})

test_that("a period sweep takes factor and date-time values as labels", {
  mt <- mtcars
  mt$period <- rep(c("a", "b"), 16)
  fit <- bb4_sens_mt_fit(
    data = mt, include_time_effects = TRUE, time_effect_optimization_value = "a"
  )
  s <- suppressWarnings(suppressMessages(lago_sensitivity(
    fit, parameter = "time_effect_optimization_value", values = factor(c("b", "a"))
  )))
  expect_identical(s$value, c("b", "a"))
  expect_identical(s$status, c("ok", "ok"))
  expect_equal(attr(s, "baseline")$rec_int, fit$rec_int)
  # the summary keeps the order of the sweep, as the plot does
  printed <- bb4_sens_printed(s)
  expect_match(printed, "time_effect_optimization_value = b and .* time_effect_optimization_value = a")

  times <- as.POSIXct(c("2024-01-01 10:00:00", "2024-02-01 10:00:00"), tz = "UTC")
  mt$period <- rep(format(times), 16)
  fit <- bb4_sens_mt_fit(
    data = mt, include_time_effects = TRUE,
    time_effect_optimization_value = format(times[1])
  )
  s <- suppressWarnings(suppressMessages(lago_sensitivity(
    fit, parameter = "time_effect_optimization_value", values = rev(times)
  )))
  expect_identical(s$value, rev(format(times)))
  expect_identical(s$status, c("ok", "ok"))
  expect_error(
    lago_sensitivity(fit, parameter = "time_effect_optimization_value", values = factor(NA)),
    "must not contain NA"
  )
  expect_error(
    lago_sensitivity(fit, parameter = "time_effect_optimization_value", values = list(1)),
    "or a factor or date-time vector"
  )
})

test_that("the printed summary writes numbers in full", {
  s <- bb4_sens_fake(value = c(2e5, 1e5), cost = c(3e5, 1e5))
  printed <- bb4_sens_printed(s)
  expect_match(printed, "rec_int_cost is 100000 at outcome_goal = 100000 and 300000 at outcome_goal = 200000\\.")
  expect_false(grepl("e+05", printed, fixed = TRUE))
  # one successful run has one end
  s <- bb4_sens_fake(value = c(1e5, 2e5), cost = c(1e5, NA))
  printed <- bb4_sens_printed(s)
  expect_match(printed, "rec_int_cost is 100000 at outcome_goal = 100000\\.")
})

test_that("a one-row sweep plots its point without a line", {
  s <- bb4_sens_fake(value = 1, cost = 10)
  p <- plot(s)
  expect_length(p$layers, 1L)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  p <- plot(s, show = "components")
  expect_length(p$layers, 1L)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  # two rows get the line
  s <- bb4_sens_fake(value = c(1, 2), cost = c(10, 20))
  expect_length(plot(s)$layers, 2L)
  expect_length(plot(s, show = "components")$layers, 2L)
})

test_that("the components plot counts dropped rows, not points", {
  s <- bb4_sens_fake(
    value = c(1, 2, 3), cost = c(10, NA, 30), gear = c(1, NA, 3), qsec = c(1, NA, 3)
  )
  expect_message(p <- plot(s, show = "components"), "^1 row\\(s\\) with NA component value")
  expect_equal(nrow(p$data), 4L)
})

test_that("lago_budget() refuses a fractional or text n_grid", {
  fit <- bb4_sens_mt_fit()
  expect_error(lago_budget(fit, budget = 100, n_grid = 2.5), "single integer >= 2, a whole number")
  expect_error(lago_budget(fit, budget = 100, n_grid = "5"), "single integer >= 2, a whole number")
  expect_error(lago_budget(fit, budget = 100, n_grid = 1), "single integer >= 2, a whole number")
  expect_error(lago_budget(fit, budget = 100, n_grid = NA_real_), "single integer >= 2, a whole number")
  expect_error(lago_budget(fit, budget = 100, n_grid = c(2, 3)), "single integer >= 2, a whole number")
  expect_error(lago_budget(fit, budget = 100, n_grid = 3e9), "single integer >= 2, a whole number")
})

test_that("lago_budget() binding does not read round off on a large outcome as binding", {
  skip_on_cran()
  # the best outcome, about 232667 at the upper bounds, costs 640004
  fit <- bb4_sens_mt_fit(outcome_goal = 25, intervention_upper_bounds = c(1e4, 1e5))
  b <- suppressWarnings(suppressMessages(lago_budget(fit, budget = 1e6)))
  expect_true(b$feasible)
  expect_equal(b$rec_int_cost, 640004, tolerance = 1e-6)
  expect_false(b$binding)
})

test_that("lago_budget() binds when the costlier maximum gains less than round off", {
  skip_on_cran()
  # with no center term the logit is flat near the upper bounds (40, 5), which
  # cost 108000 and gain less than 1e-9 over the best intervention that fits
  # a budget of 107999
  fit <- suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    glm_family = "binomial", intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = 0.5,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE, quiet = TRUE
  )))
  b <- suppressWarnings(suppressMessages(lago_budget(fit, budget = 107999)))
  expect_true(b$binding)
  expect_lte(b$rec_int_cost, 107999)
  b <- suppressWarnings(suppressMessages(lago_budget(fit, budget = 108000)))
  expect_false(b$binding)
})
