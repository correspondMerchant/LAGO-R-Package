# Round 6 bug bash: lago_sensitivity() and lago_budget().

sens6_base <- function(...) {
  utils::modifyList(list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 30,
    outcome_goal_intention = "maximize"
  ), list(...))
}

sens6_run <- function(args, parameter, values) {
  suppressWarnings(do.call(
    lago_sensitivity, c(args, list(parameter = parameter, values = values))
  ))
}

# the cli lines a print method writes, without its plain table
cli_text_of <- function(x) cli::cli_fmt(utils::capture.output(print(x)))

baseline_goal <- function(s) attr(attr(s, "baseline"), "lago_call_args")$outcome_goal

test_that("the baseline of tiny values is matched relative to the baseline", {
  d <- mtcars
  d$mpg <- mtcars$mpg * 1e-9
  s <- sens6_run(
    sens6_base(data = d, outcome_goal = 3.5e-8), "outcome_goal",
    c(2.5e-8, 3e-8, 3.5e-8)
  )
  expect_equal(baseline_goal(s), 3.5e-8)
  expect_equal(attr(s, "baseline")$rec_int_cost, s$rec_int_cost[3])
})

test_that("the closest value within the tolerance is the baseline", {
  s <- sens6_run(
    sens6_base(outcome_goal = 30), "outcome_goal", c(30 * (1 + 1e-9), 30)
  )
  expect_identical(baseline_goal(s), 30)
})

test_that("a baseline of 0 matches only 0", {
  d <- mtcars
  d$mpg <- mtcars$mpg - 25
  s <- sens6_run(
    sens6_base(data = d, outcome_goal = 0), "outcome_goal", c(1e-10, 0)
  )
  expect_identical(baseline_goal(s), 0)
})

test_that("a numeric period sweep takes NA when the period has an NA level", {
  mt <- mtcars
  mt$period <- addNA(factor(rep(c(1, 2, NA), length.out = 32)))
  args <- sens6_base(
    data = mt, include_time_effects = TRUE, time_effect_optimization_value = 1
  )
  s <- sens6_run(args, "time_effect_optimization_value", c(1, 2, NA))
  expect_identical(s$status, rep("ok", 3))
  expect_true(is.na(s$value[3]))
  direct <- suppressWarnings(do.call(lago_optimization, utils::modifyList(
    args, list(time_effect_optimization_value = NA_real_,
      include_confidence_set = FALSE, quiet = TRUE)
  )))
  expect_equal(s$rec_int_cost[3], direct$rec_int_cost)

  # a lone logical NA, which is also the baseline here
  args$time_effect_optimization_value <- NA
  s1 <- sens6_run(args, "time_effect_optimization_value", NA)
  expect_identical(s1$status, "ok")
  expect_equal(s1$rec_int_cost, direct$rec_int_cost)
  expect_false(is.null(attr(s1, "baseline")))

  # NaN and Inf are still refused
  expect_error(
    sens6_run(args, "time_effect_optimization_value", c(1, NaN)),
    "must be all finite"
  )
  expect_error(
    sens6_run(args, "time_effect_optimization_value", c(NA, Inf)),
    "must be all finite"
  )
})

test_that("a numeric period sweep refuses NA without an NA level", {
  mt <- mtcars
  mt$period <- rep(c(1, 2), length.out = 32)
  args <- sens6_base(
    data = mt, include_time_effects = TRUE, time_effect_optimization_value = 1
  )
  expect_error(
    sens6_run(args, "time_effect_optimization_value", c(1, NA)),
    "explicit NA level"
  )
  expect_error(
    sens6_run(args, "time_effect_optimization_value", NA),
    "explicit NA level"
  )
})

shrunk_sweep <- function() {
  sens6_run(sens6_base(), "outcome_goal", c(30, 35, 1000, 5000))
}

test_that("print() lists the runs that used the shrinking method", {
  s <- shrunk_sweep()
  expect_identical(attr(s, "shrinking_method_used"), c(FALSE, FALSE, TRUE, TRUE))
  out <- paste(cli_text_of(s), collapse = " ")
  expect_match(
    out,
    "The goal could not be reached at outcome_goal = 1000, 5000, so the shrinking method gave the recommendation there.",
    fixed = TRUE
  )

  # none is listed without a shrunk run, or for a result that does not record it
  ok <- s[1:2, ]
  attr(ok, "shrinking_method_used") <- c(FALSE, FALSE)
  expect_no_match(paste(cli_text_of(ok), collapse = " "), "could not be reached")
  attr(s, "shrinking_method_used") <- NULL
  expect_no_match(paste(cli_text_of(s), collapse = " "), "could not be reached")
})

test_that("print() lists shrunk period labels as text", {
  s <- data.frame(
    value = c("a", "b"), x = c(1, 2), rec_int_cost = c(1, 2),
    est_outcome_goal = c(1, 2), status = "ok", stringsAsFactors = FALSE
  )
  attr(s, "parameter") <- "time_effect_optimization_value"
  attr(s, "component_names") <- "x"
  attr(s, "shrinking_method_used") <- c(FALSE, TRUE)
  class(s) <- c("lago_sensitivity", "data.frame")
  out <- paste(cli_text_of(s), collapse = " ")
  expect_match(out, "time_effect_optimization_value = b, so", fixed = TRUE)
})

test_that("plot() marks the runs that used the shrinking method", {
  s <- shrunk_sweep()
  p <- plot(s)
  expect_match(p$labels$caption, "Triangles mark runs whose goal could not be reached")
  expect_identical(
    p$data$kind,
    c("goal reached", "goal reached", "goal out of reach", "goal out of reach")
  )
  built <- ggplot2::ggplot_build(p)
  shapes <- built$data[[2]]$shape
  expect_identical(shapes, c(16, 16, 17, 17))

  pc <- plot(s, show = "components")
  expect_match(pc$labels$caption, "Triangles mark")
  expect_identical(sort(unique(ggplot2::ggplot_build(pc)$data[[2]]$shape)), c(16, 17))

  # without a shrunk run the plot has no shapes or note
  ok <- s[1:2, ]
  attr(ok, "shrinking_method_used") <- c(FALSE, FALSE)
  expect_null(plot(ok)$labels$caption)
  expect_null(plot(ok, show = "components")$labels$caption)
})

test_that("print() rounds the estimate of a reached goal as print.lago() does", {
  s <- shrunk_sweep()
  s$est_outcome_goal[1] <- 0.7499995
  s$est_outcome_goal[3] <- 0.7499995
  out <- utils::capture.output(suppressMessages(print(s)))
  row1 <- grep("^1 ", out, value = TRUE)
  row3 <- grep("^3 ", out, value = TRUE)
  expect_match(row1, " 0.75 ", fixed = TRUE)
  expect_no_match(row1, "0.7499995", fixed = TRUE)
  # a shrunk run did not reach its goal, so it keeps its 7 digits
  expect_match(row3, "0.7499995", fixed = TRUE)
})

budget6_args <- function(...) {
  utils::modifyList(list(
    outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), data = mtcars
  ), list(...))
}

test_that("print.lago_budget rounds the estimate as print.lago() does", {
  b <- suppressWarnings(do.call(lago_budget, c(budget6_args(), list(budget = 100))))
  b$est_outcome <- 0.12345
  out <- paste(cli_text_of(b), collapse = " ")
  expect_match(out, "Estimated outcome: 0.123.", fixed = TRUE)
})

test_that("an outcome with no finite value reports the error of lago_optimization()", {
  d <- mtcars
  d$mpg <- NA_real_
  expect_warning(
    b <- do.call(lago_budget, c(budget6_args(data = d), list(budget = 10))),
    "has no observed values"
  )
  expect_false(b$feasible)
  expect_match(b$run_error, "has no observed values")
  d$mpg <- Inf
  expect_warning(
    b <- do.call(lago_budget, c(budget6_args(data = d), list(budget = 10))),
    "hold Inf or -Inf"
  )
})

test_that("a narrow reachable range is searched closely", {
  b <- suppressWarnings(do.call(lago_budget, c(budget6_args(
    intervention_lower_bounds = c(3, 17),
    intervention_upper_bounds = c(3.001, 17.001)
  ), list(budget = 118.005, n_grid = 10))))
  # the best outcome within this budget is about 15.185351
  expect_gt(b$est_outcome, 15.18534)
  expect_lte(b$rec_int_cost, 118.005)
})

test_that("with a power goal the infeasible note names the power implied outcome", {
  set.seed(3)
  n <- 48
  d <- data.frame(center = rep(paste0("c", 1:8), each = 6))
  d$group <- ifelse(as.integer(sub("c", "", d$center)) <= 4, "control", "treatment")
  d$dose <- ifelse(d$group == "treatment", runif(n, 0, 4), 0)
  d$dose2 <- ifelse(d$group == "treatment", runif(n, 0, 4), 0)
  d$y <- rbinom(n, 1, stats::plogis(-1 + 0.3 * d$dose + 0.15 * d$dose2))
  args <- list(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = c("dose", "dose2"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(6, 6),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    power_goal = 0.8, num_centers_in_next_stage = 2,
    patients_per_center_in_next_stage = 3
  )
  ws <- character()
  b <- withCallingHandlers(
    do.call(lago_budget, c(args, list(budget = 1, n_grid = 8))),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(b$feasible)
  expect_true(attr(b, "power_goal_used"))
  expect_true(any(grepl(
    "below the cost of the cheapest intervention that reaches the outcome the power goal implies",
    ws, fixed = TRUE
  )))
  out <- paste(cli_text_of(b), collapse = " ")
  expect_match(
    out, "The cheapest intervention that reaches the outcome the power goal implies costs",
    fixed = TRUE
  )

  # without a power goal the note names the cheapest reachable intervention
  attr(b, "power_goal_used") <- FALSE
  out <- paste(cli_text_of(b), collapse = " ")
  expect_match(out, "The cheapest reachable intervention costs", fixed = TRUE)
})
