# Round 3 bug bash fixes for lago_sensitivity() and lago_budget().

bb3_sens_bb_fit <- function() {
  suppressWarnings(lago_optimization(
    data = BB_data, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    glm_family = "binomial", intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100", center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = 0.85,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE, quiet = TRUE
  ))
}

# a small data set whose single component is not significant, so every fit
# warns about it
bb3_sens_small_args <- function() {
  set.seed(1)
  d <- do.call(rbind, lapply(1:8, function(j) {
    x <- if (j <= 4) 0 else stats::runif(1, 0.5, 3)
    data.frame(
      center = paste0("c", j), x = x, group = ifelse(x == 0, "control", "treatment"),
      y = stats::rbinom(8, 1, stats::plogis(-1 + 0.3 * x))
    )
  }))
  list(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 6,
    cost_list_of_vectors = list(c(1, 1)), outcome_goal = 0.4,
    include_confidence_set = FALSE, quiet = TRUE
  )
}

bb3_sens_period_fit <- function(period) {
  mt <- mtcars
  mt$period <- period
  suppressWarnings(suppressMessages(lago_optimization(
    data = mt, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 30,
    outcome_goal_intention = "maximize", include_time_effects = TRUE,
    time_effect_optimization_value = period[1],
    include_confidence_set = FALSE, quiet = TRUE
  )))
}

collect_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = seen)
}

test_that("lago_budget() reports binding when the maximum costs more than the budget", {
  skip_on_cran()
  opt <- bb3_sens_bb_fit()
  # the maximum is at the upper bounds (40, 5), which cost 108000
  b <- suppressWarnings(lago_budget(opt, budget = 107999))
  expect_true(b$binding)
  expect_lte(b$rec_int_cost, 107999)
  expect_gt(b$rec_int_cost, 107999 - 1e-4 * 107999)
  printed <- paste(cli::cli_fmt(print(b)), collapse = " ")
  expect_false(grepl("did not bind", printed))
  # the cost is shown with 2 decimals (bug bash 6), and not over the budget
  expect_match(printed, "Cost: 10799[0-9](\\.[0-9]{1,2})? \\(of 107999\\)")
  shown <- as.numeric(sub(".*Cost: ([0-9.]+) .*", "\\1", printed))
  expect_lte(shown, 107999)
  # a budget that covers the maximum does not bind and recommends it
  b <- suppressWarnings(lago_budget(opt, budget = 108000))
  expect_false(b$binding)
  expect_equal(unname(b$rec_int), c(40, 5), tolerance = 1e-4)
  expect_equal(b$rec_int_cost, 108000, tolerance = 1e-5)
  # the frontier is pinned down on a cost gap too, below the maximum
  b <- suppressWarnings(lago_budget(opt, budget = 107700))
  expect_true(b$binding)
  expect_gt(b$rec_int_cost, 107700 - 1e-4 * 107700)
})

test_that("lago_budget() pins down the cost when minimizing", {
  mt <- mtcars
  mt$neg_mpg <- -mt$mpg
  b <- suppressWarnings(lago_budget(
    data = mt, outcome_name = "neg_mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal_intention = "minimize", budget = 50, n_grid = 4
  ))
  expect_true(b$feasible)
  expect_true(b$binding)
  expect_lte(b$rec_int_cost, 50)
  expect_gt(b$rec_int_cost, 50 - 1e-4 * 50)
})

test_that("a goal the optimizer meets within its own tolerance counts as reached", {
  # the numerical optimizer stops about 3e-6 short of the goal -1.35 here,
  # which the search used to read as out of reach, so it stopped near a cost of
  # 25 for a budget of 50
  b <- suppressWarnings(lago_budget(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal_intention = "maximize", budget = 50, n_grid = 2
  ))
  expect_true(b$binding)
  expect_lte(b$rec_int_cost, 50)
  expect_gt(b$rec_int_cost, 49.9)
})

test_that("a printed budget cost is rounded down only when rounding passes the budget", {
  b <- structure(list(
    budget = 107999, feasible = TRUE, binding = TRUE, rec_int = c(a = 1),
    rec_int_cost = 107997.4, est_outcome = 0.5, outcome_goal = 0.5,
    frontier = data.frame(), component_names = "a", intention = "maximize",
    run_error = NULL
  ), class = "lago_budget")
  # a cost of 1e4 or more shows 2 decimals (bug bash 6), which stay under
  # the budget here
  expect_match(paste(cli::cli_fmt(print(b)), collapse = " "), "Cost: 107997.4 ")
  b$rec_int_cost <- 107900.4
  expect_match(paste(cli::cli_fmt(print(b)), collapse = " "), "Cost: 107900.4 ")
  # a cost that rounds up past the budget is shown under it
  b$budget <- 107998.999
  b$rec_int_cost <- 107998.996
  printed <- paste(cli::cli_fmt(print(b)), collapse = " ")
  expect_lte(as.numeric(sub(".*Cost: ([0-9.]+) .*", "\\1", printed)), 107998.999)
})

test_that("a sweep shows each model fit warning once", {
  args <- bb3_sens_small_args()
  out <- collect_warnings(do.call(lago_sensitivity, c(
    args,
    list(parameter = "outcome_goal", values = c(0.3, 0.35, 0.4))
  )))
  fit <- grepl("statistically significant association", out$warnings)
  expect_identical(sum(fit), 1L)
  args$outcome_goal <- NULL
  out <- collect_warnings(do.call(lago_budget, c(args, list(budget = 3, n_grid = 4))))
  expect_identical(sum(grepl("statistically significant association", out$warnings)), 1L)
})

test_that("only a repeat of the same model fit warning is hidden", {
  handler <- LAGOtrials:::show_each_input_note_once()
  # the handler knows the fit by the name of the function that runs it
  cached_outcome_model_fitting <- function(text) warning(text, call. = FALSE)
  shown <- character(0)
  withCallingHandlers(
    withCallingHandlers(
      {
        for (text in c("a", "b", "a")) cached_outcome_model_fitting(text)
        for (text in c("run", "run")) warning(text, call. = FALSE)
      },
      warning = handler
    ),
    warning = function(w) {
      shown <<- c(shown, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(shown, c("a", "b", "run", "run"))
})

test_that("sweeps that cannot change the recommendation are refused", {
  args <- bb3_sens_small_args()
  expect_error(
    do.call(lago_sensitivity, c(args, list(parameter = "time_effect_optimization_value", values = c(1, 2)))),
    "include_time_effects = TRUE"
  )
  expect_error(
    do.call(lago_sensitivity, c(args, list(parameter = "icc", values = c(0, 0.05)))),
    "without a power_goal"
  )
})

test_that("a sweep of period labels runs, prints and plots", {
  period <- rep_len(c("2024-Q1", "2024-Q2"), nrow(mtcars))
  fit <- bb3_sens_period_fit(period)
  s <- suppressWarnings(suppressMessages(lago_sensitivity(
    fit, parameter = "time_effect_optimization_value", values = c("2024-Q1", "2024-Q2")
  )))
  expect_identical(s$value, c("2024-Q1", "2024-Q2"))
  expect_true(all(s$status == "ok"))
  # the stored period is one of the swept labels, so it is the baseline
  expect_false(is.null(attr(s, "baseline")))
  utils::capture.output(lines <- cli::cli_fmt(print(s)))
  printed <- paste(lines, collapse = " ")
  expect_match(printed, "time_effect_optimization_value = 2024-Q2")
  p <- plot(s)
  expect_s3_class(ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]], "ScaleDiscrete")
  expect_identical(levels(p$data$value), c("2024-Q1", "2024-Q2"))
  p <- plot(s, show = "components")
  expect_s3_class(ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]], "ScaleDiscrete")
  # a row without a cost is dropped from the discrete plot too
  s$rec_int_cost[2] <- NA
  expect_message(p <- plot(s), "1 row")
  expect_identical(as.character(p$data$value), "2024-Q1")
  # without the stored period among the labels there is no baseline
  s <- suppressWarnings(suppressMessages(lago_sensitivity(
    fit, parameter = "time_effect_optimization_value", values = "2024-Q2"
  )))
  expect_null(attr(s, "baseline"))
  # a numeric sweep keeps its continuous axis
  s <- suppressWarnings(suppressMessages(lago_sensitivity(
    fit, parameter = "outcome_goal", values = c(30, 32)
  )))
  p <- plot(s)
  expect_s3_class(ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]], "ScaleContinuous")
  p <- plot(s, show = "components")
  expect_s3_class(ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]], "ScaleContinuous")
})

test_that("a sweep of dates runs and plots on a discrete axis", {
  days <- as.Date(c("2024-01-01", "2024-04-01"))
  fit <- bb3_sens_period_fit(rep_len(days, nrow(mtcars)))
  s <- suppressWarnings(suppressMessages(lago_sensitivity(
    fit, parameter = "time_effect_optimization_value", values = days
  )))
  expect_s3_class(s$value, "Date")
  expect_true(all(s$status == "ok"))
  p <- plot(s)
  expect_s3_class(ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]], "ScaleDiscrete")
})

test_that("period values are checked", {
  fit <- bb3_sens_period_fit(rep_len(c("2024-Q1", "2024-Q2"), nrow(mtcars)))
  expect_error(
    lago_sensitivity(fit, parameter = "time_effect_optimization_value", values = c("2024-Q1", NA)),
    "must not contain NA"
  )
  expect_error(
    lago_sensitivity(fit, parameter = "time_effect_optimization_value", values = TRUE),
    "numeric, character, or Date vector"
  )
  # text is still refused for every other parameter
  expect_error(
    lago_sensitivity(fit, parameter = "outcome_goal", values = c("30", "35")),
    "non-empty numeric vector"
  )
})

test_that("the lago_sensitivity() help lists every attribute it sets", {
  rd <- test_path("..", "..", "man", "lago_sensitivity.Rd")
  skip_if_not(file.exists(rd))
  text <- paste(readLines(rd), collapse = " ")
  expect_match(text, "effective_outcome_goal")
  expect_match(text, "shrinking_method_used")
})
