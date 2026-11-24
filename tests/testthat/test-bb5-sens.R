# Round 5 bug bash fixes for lago_sensitivity() and lago_budget().

bb5_sens_args <- function(...) {
  utils::modifyList(list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 30,
    outcome_goal_intention = "maximize"
  ), list(...))
}

bb5_sens_run <- function(...) {
  suppressWarnings(suppressMessages(
    do.call(lago_sensitivity, bb5_sens_args(...))
  ))
}

test_that("a date-time period sweep is labeled in the time zone of the period column", {
  mt <- mtcars
  t_ny <- as.POSIXct(c("2024-01-01 10:00:00", "2024-02-01 10:00:00"), tz = "America/New_York")
  mt$period <- rep(t_ny, 16)
  vals <- t_ny
  attr(vals, "tzone") <- "UTC"
  s <- bb5_sens_run(
    data = mt, include_time_effects = TRUE,
    time_effect_optimization_value = vals[2],
    parameter = "time_effect_optimization_value", values = vals
  )
  expect_identical(s$status, c("ok", "ok"))
  expect_identical(s$value, c("2024-01-01 10:00:00", "2024-02-01 10:00:00"))
  expect_false(is.null(attr(s, "baseline")))
  expect_equal(attr(s, "baseline")$rec_int_cost, s$rec_int_cost[2])
})

test_that("power_goal is not matched by part of power_goal_approach", {
  expect_error(
    lago_sensitivity(
      data = mtcars, outcome_name = "mpg", power_goal_approach = "conditional",
      parameter = "icc", values = c(0, 0.1)
    ),
    "without a power_goal"
  )
})

test_that("NA selects the explicit NA period level in a period sweep", {
  mt <- mtcars
  mt$period <- addNA(factor(rep(c("p1", "p2", NA), length.out = 32)))
  s <- bb5_sens_run(
    data = mt, include_time_effects = TRUE,
    time_effect_optimization_value = NA,
    parameter = "time_effect_optimization_value", values = c("p1", NA, "p2")
  )
  expect_identical(s$status, c("ok", "ok", "ok"))
  direct <- suppressWarnings(suppressMessages(do.call(lago_optimization, bb5_sens_args(
    data = mt, include_time_effects = TRUE,
    time_effect_optimization_value = NA_character_,
    include_confidence_set = FALSE, quiet = TRUE
  ))))
  expect_equal(s$rec_int_cost[2], direct$rec_int_cost)
  # the NA run is the baseline
  expect_equal(attr(s, "baseline")$rec_int_cost, direct$rec_int_cost)
  # the plot keeps the NA row
  expect_no_message(p <- plot(s))
  expect_identical(nrow(p$data), 3L)
  expect_true(anyNA(levels(p$data$value)))
  expect_identical(nrow(ggplot2::ggplot_build(p)$data[[2]]), 3L)
  expect_identical(nrow(plot(s, show = "components")$data), 6L)

  f <- bb5_sens_run(
    data = mt, include_time_effects = TRUE, time_effect_optimization_value = "p1",
    parameter = "time_effect_optimization_value",
    values = factor(c("p1", NA), exclude = NULL)
  )
  expect_identical(f$status, c("ok", "ok"))
  expect_equal(f$rec_int_cost[2], direct$rec_int_cost)

  # without an NA level, NA is still refused
  mt$period <- factor(rep(c("p1", "p2"), 16))
  expect_error(
    lago_sensitivity(
      data = mt, include_time_effects = TRUE, time_effect_optimization_value = "p1",
      parameter = "time_effect_optimization_value", values = c("p1", NA)
    ),
    "unless the 'period' column is a factor with an explicit NA level"
  )
})

test_that("missing values in a period sweep gives the values message", {
  mt <- mtcars
  mt$period <- factor(rep(c("p1", "p2"), 16))
  expect_error(
    lago_sensitivity(
      data = mt, include_time_effects = TRUE,
      parameter = "time_effect_optimization_value"
    ),
    "`values` must be a non-empty numeric, character, or Date vector",
    fixed = TRUE
  )
  expect_error(
    lago_sensitivity(data = mtcars, parameter = "outcome_goal"),
    "`values` must be a non-empty numeric vector.",
    fixed = TRUE
  )
})

test_that("print() writes the table numbers in full", {
  df <- data.frame(
    value = c(30, -1e9, 1e9), gear = c(10, NA, 1e-7),
    rec_int_cost = c(83.808404942, NA, 4e9), est_outcome_goal = c(30, NA, 2),
    status = c("ok", "error", "ok"), stringsAsFactors = FALSE
  )
  attr(df, "parameter") <- "outcome_goal"
  attr(df, "component_names") <- "gear"
  class(df) <- c("lago_sensitivity", "data.frame")
  out <- utils::capture.output(suppressMessages(print(df)))
  expect_true(any(grepl("-1000000000", out, fixed = TRUE)))
  expect_true(any(grepl("4000000000", out, fixed = TRUE)))
  expect_true(any(grepl("83.8084 ", out, fixed = TRUE)))
  # a number below 1e-6 stays in e notation
  expect_true(any(grepl("1e-7", out, fixed = TRUE)))
  expect_false(any(grepl("e+", out, fixed = TRUE)))
  expect_false(any(grepl("<NA>", out, fixed = TRUE)))
  expect_true(any(grepl(" NA ", out, fixed = TRUE)))
})

test_that("an infeasible budget is written in full and print() gives the cheapest cost", {
  args <- bb5_sens_args(budget = 3.9999999, outcome_goal_intention = "minimize")
  args$outcome_goal <- NULL
  w <- character(0)
  b <- withCallingHandlers(
    suppressMessages(do.call(lago_budget, args)),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(b$feasible)
  expect_true(any(grepl(
    "The budget (3.9999999) is below the cost of the cheapest reachable intervention (4)",
    w, fixed = TRUE
  )))
  out <- cli::cli_fmt(print(b))
  expect_true(any(grepl("The cheapest reachable intervention costs 4.", out, fixed = TRUE)))
})
