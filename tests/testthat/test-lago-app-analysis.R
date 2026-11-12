# lago_app(): the Sensitivity and Budget tabs and the report download, which
# start from the result of a run. The app itself is never launched here.

analysis_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# mtcars set up in a test server, ready to run
analysis_setup <- function(session, col_id, goal_mode = "outcome", cost_mode = "unit") {
  session$setInputs(source = "mtcars", sep = ",", dec = ".")
  session$setInputs(
    outcome = "mpg", outcome_type = "continuous", goal_mode = goal_mode, goal = 25,
    intention = "maximize", components = c("gear", "qsec"), cost_mode = cost_mode,
    cost_type = "linear", center_mode = "none", include_cs = FALSE, approach = "unconditional"
  )
  do.call(session$setInputs, stats::setNames(
    list(3, 5, 2, 14, 20, 1),
    c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))
  ))
}

analysis_spec <- function(...) {
  s <- .app_defaults(datasets::mtcars, "mtcars")
  utils::modifyList(s, list(...))
}

test_that("a sweep offers only the inputs the run uses", {
  s <- analysis_spec()
  expect_identical(unname(.app_sweep_choices(s)), c("outcome_goal", "cost_multiplier"))
  s <- analysis_spec(outcome_type = "binary", goal_mode = "power")
  expect_identical(
    unname(.app_sweep_choices(s)),
    c("cost_multiplier", "power_goal", "num_centers_in_next_stage", "patients_per_center_in_next_stage")
  )
  s$cluster_col <- "center"
  s$time_on <- TRUE
  expect_true(all(c("icc", "time_effect_optimization_value") %in% .app_sweep_choices(s)))
  expect_identical(.app_sweep_label("icc"), "ICC")
  expect_identical(.app_sweep_label("other"), "other")
})

test_that("a sweep starts on values around the current settings", {
  d <- datasets::mtcars
  s <- analysis_spec(goal = 25)
  span <- diff(range(d$mpg))
  expect_equal(.app_sweep_defaults("outcome_goal", s, d)[c("from", "to")], list(from = signif(25 - 0.15 * span, 3), to = signif(25 + 0.15 * span, 3)))
  # a binary goal stays inside 0 and 1
  b <- .app_sweep_defaults("outcome_goal", analysis_spec(outcome_type = "binary", goal = 0.95), d)
  expect_equal(c(b$from, b$to), c(0.8, 0.99))
  expect_equal(.app_sweep_defaults("cost_multiplier", s, d)[c("from", "to")], list(from = 0.5, to = 1.5))
  expect_equal(.app_sweep_defaults("power_goal", analysis_spec(power_goal = 0.95), d)$to, 0.99)
  centers <- .app_sweep_defaults("num_centers_in_next_stage", analysis_spec(n_centers = 10), d)
  expect_equal(c(centers$from, centers$to), c(5, 15))
  patients <- .app_sweep_defaults("patients_per_center_in_next_stage", analysis_spec(n_patients = 1), d)
  expect_equal(c(patients$from, patients$to), c(1, 5))
  expect_equal(.app_sweep_defaults("icc", analysis_spec(icc = c(0.04, 0.08)), d)$to, 0.16)
  expect_equal(.app_sweep_defaults("icc", analysis_spec(), d)$to, 0.1)
  # the periods are typed, as only they make sense
  dp <- data.frame(period = c("Q1", "Q2", "Q3"))
  expect_identical(.app_sweep_defaults("time_effect_optimization_value", analysis_spec(period_col = "period"), dp)$typed, "1, 2, 3")
  dn <- data.frame(period = c(2019, 2020))
  expect_identical(.app_sweep_defaults("time_effect_optimization_value", analysis_spec(period_col = "period"), dn)$typed, "2019, 2020")
})

test_that("sweep values come from even steps or a typed list, and say what to fix", {
  v <- .app_sweep_values("outcome_goal", "steps", 20, 30, 3, "", "continuous")
  expect_equal(v$values, c(20, 25, 30))
  expect_null(v$error)
  expect_equal(.app_sweep_values("num_centers_in_next_stage", "steps", 4, 6, 5, "")$values, c(4, 5, 6))
  expect_equal(.app_sweep_values("power_goal", "typed", NA, NA, NA, "0.7, 0.8, 0.8")$values, c(0.7, 0.8))
  err <- function(...) .app_sweep_values(...)$error
  expect_match(err("outcome_goal", "typed", NA, NA, NA, "a, b"), "separated by commas")
  expect_match(err("outcome_goal", "steps", NA, 2, 5, ""), "From and a To")
  expect_match(err("outcome_goal", "steps", 2, 2, 5, ""), "must differ")
  expect_match(err("outcome_goal", "steps", 1, 2, 30, ""), "from 2 to 25")
  expect_match(err("outcome_goal", "steps", 1, 2, 2.5, ""), "from 2 to 25")
  expect_match(err("outcome_goal", "typed", NA, NA, NA, paste(1:26, collapse = ",")), "at most 25")
  expect_match(err("outcome_goal", "typed", NA, NA, NA, "3"), "at least two")
  expect_match(err("outcome_goal", "steps", 0.5, 1, 3, "", "binary"), "between 0 and 1")
  expect_null(err("outcome_goal", "steps", 10, 40, 3, "", "continuous"))
  expect_match(err("cost_multiplier", "steps", 0, 1, 3, ""), "above 0")
  expect_match(err("power_goal", "steps", 0.5, 1, 3, ""), "between 0 and 1")
  expect_match(err("num_centers_in_next_stage", "typed", NA, NA, NA, "1, 2"), "2 or more")
  expect_match(err("patients_per_center_in_next_stage", "typed", NA, NA, NA, "1.5, 2"), "whole number")
  expect_match(err("icc", "typed", NA, NA, NA, "0, 1"), "below 1")
})

test_that("a sweep adds the cost list and the center column it needs, in the run and the code", {
  r <- list(cost_list_of_vectors = list(c(0, 2), c(0, 1)))
  s <- analysis_spec(cost_mode = "unit", cluster_col = "cyl")
  expect_identical(.app_sweep_extra("cost_multiplier", s, r), list(cost_list_of_vectors = r$cost_list_of_vectors))
  expect_identical(.app_sweep_extra("cost_multiplier", utils::modifyList(s, list(cost_mode = "coef")), r), list())
  expect_identical(.app_sweep_extra("icc", s, r), list(power_goal_cluster_id = "cyl"))
  code <- paste(.app_sweep_code("cost_multiplier", c(0.5, 1), s), collapse = "\n")
  expect_match(code, "lago_sensitivity\\(\n  result,\n  parameter = \"cost_multiplier\",\n  values = c\\(0.5, 1\\),\n  cost_list_of_vectors = result\\$cost_list_of_vectors\n\\)")
  expect_match(paste(.app_sweep_code("icc", c(0, 0.1), s), collapse = "\n"), "power_goal_cluster_id = \"cyl\"")
  expect_identical(.app_budget_code(120, 8), "budget_result <- lago_budget(result, budget = 120, n_grid = 8L)")
})

test_that("a budget search is explained in plain sentences", {
  base <- list(budget = 100, component_names = c("gear", "qsec"), intention = "maximize", run_error = NULL)
  b <- utils::modifyList(base, list(feasible = TRUE, binding = TRUE, rec_int = c(4, 16), rec_int_cost = 98, est_outcome = 27))
  expect_match(.app_budget_sentence(b), "With a budget of 100, the best estimated outcome is 27. It sets gear at 4, qsec at 16, for a cost of 98. A better outcome")
  b$binding <- FALSE
  expect_match(.app_budget_sentence(b), "budget is not the limit")
  b$intention <- "minimize"
  expect_match(.app_budget_sentence(b), "the lowest estimated outcome")
  none <- utils::modifyList(base, list(feasible = FALSE, frontier = data.frame(rec_int_cost = c(150, 200))))
  expect_match(.app_budget_sentence(none), "costs 100 or less. The cheapest one found costs 150")
  none$frontier <- data.frame(rec_int_cost = numeric(0))
  expect_match(.app_budget_sentence(none), "Widen the bounds")
  failed <- utils::modifyList(base, list(feasible = FALSE, run_error = "the model cannot be fitted"))
  expect_match(.app_budget_sentence(failed), "every optimization failed: the model cannot be fitted")
})

test_that("the report says what to install when it cannot be made", {
  expect_null(.app_report_missing(function(p) TRUE, function() TRUE))
  expect_match(
    .app_report_missing(function(p) p == "knitr", function() TRUE),
    "needs the rmarkdown, jsonlite packages. Install with install.packages\\(c\\(\"rmarkdown\", \"jsonlite\"\\)\\)"
  )
  expect_match(.app_report_missing(function(p) p != "knitr", function() TRUE), "the knitr package\\.")
  expect_match(.app_report_missing(function(p) TRUE, function() FALSE), "needs pandoc")
})

test_that("a sweep table names failed, shrunk and raised runs", {
  s <- data.frame(value = c(20, 25, 30), gear = c(3, 4, NA), rec_int_cost = c(10, 12, NA), est_outcome_goal = c(21, 25, NA), status = c("ok", "ok", "error"))
  attr(s, "parameter") <- "outcome_goal"
  attr(s, "component_names") <- "gear"
  attr(s, "effective_outcome_goal") <- c(21, 25, NA)
  attr(s, "shrinking_method_used") <- c(FALSE, TRUE, NA)
  attr(s, "error_messages") <- c(`30` = "no fit")
  tab <- .app_sweep_table(s)
  expect_identical(names(tab), c("Outcome goal", "gear", "Cost", "Estimated outcome", "Note"))
  expect_identical(tab$Note, c("The power goal raised the goal to 21.", "The goal is out of reach, so the shrinking method was used.", "This run failed: no fit"))
  expect_identical(.app_sweep_sentence(s), "The cost of the recommendation is 10 at the lowest value, 20, and 12 at the highest value, 25. 1 of 3 runs failed, as the table shows.")
  s$status <- "error"
  expect_match(.app_sweep_sentence(s), "Every run of the sweep failed")
})

test_that("the sweep and budget tabs wait for a result of the current settings", {
  shiny::testServer(.build_lago_app(), {
    analysis_setup(session, col_id)
    expect_match(as.character(output$sweep_gate$html), "Press Find recommendation first. The sweep")
    expect_match(as.character(output$budget_gate$html), "The search starts")
    expect_null(current_result())
    session$setInputs(sweep_param = "outcome_goal", sweep_how = "steps", sweep_from = NA, sweep_to = 30, sweep_steps = 3)
    expect_match(as.character(output$sweep_preview$html), "From and a To")
    session$setInputs(budget = NA, budget_grid = 8)
    expect_match(as.character(output$budget_hint$html), "budget above 0")
    session$setInputs(budget = 10, budget_grid = 40)
    expect_match(as.character(output$budget_hint$html), "from 2 to 25")
    expect_identical(unname(sweep_choices()), c("outcome_goal", "cost_multiplier"))
  })
})

test_that("the app sweeps, searches a budget, and returns both with their code", {
  skip_on_cran()
  returned <- NULL
  local_mocked_bindings(stopApp = function(returnValue = invisible()) returned <<- returnValue, .package = "shiny")
  shiny::testServer(.build_lago_app(), {
    analysis_setup(session, col_id)
    session$setInputs(run = 1)
    r <- run_state()$run$value
    expect_s3_class(r, "lago")
    expect_identical(current_result(), r)
    expect_null(output$sweep_gate$html)
    expect_equal(budget_prefill(), signif(r$rec_int_cost, 4))
    expect_match(as.character(output$results$html), "Report and next steps")

    session$setInputs(sweep_param = "outcome_goal", sweep_how = "steps", sweep_from = 20, sweep_to = 30, sweep_steps = 3)
    expect_match(as.character(output$sweep_preview$html), "Runs 3 optimizations, at 20, 25, 30")
    session$setInputs(run_sweep = 1)
    sw <- sweep_state()$run
    expect_null(sw$error)
    expect_s3_class(sw$value, "lago_sensitivity")
    expect_equal(sw$value$value, c(20, 25, 30))
    expect_match(as.character(output$sweep_results$html), "The cost of the recommendation is")
    session$setInputs(sweep_show = "components")
    expect_type(output$sweep_plot, "list")
    expect_match(output$sweep_table, "Outcome goal")
    csv <- utils::read.csv(output$download_sweep)
    expect_identical(names(csv), c("value", "gear", "qsec", "rec_int_cost", "est_outcome_goal", "status", "note"))

    session$setInputs(budget = signif(r$rec_int_cost, 4), budget_grid = 4)
    session$setInputs(run_budget = 1)
    bu <- budget_state()$run
    expect_null(bu$error)
    expect_s3_class(bu$value, "lago_budget")
    html <- as.character(output$budget_results$html)
    expect_match(html, "With a budget of")
    expect_match(html, "Use this outcome as the goal")
    expect_type(output$budget_plot, "list")
    expect_match(output$budget_table, "Within budget")
    expect_true(file.exists(output$download_budget))

    code <- paste(code_lines(), collapse = "\n")
    expect_match(code, "sensitivity <- lago_sensitivity\\(\n  result,\n  parameter = \"outcome_goal\"")
    expect_match(code, "budget_result <- lago_budget\\(result, budget = ")
    session$setInputs(use_budget_goal = 1)

    # new settings mark both as old and drop them from the code
    session$setInputs(goal = 26)
    expect_match(as.character(output$sweep_gate$html), "settings changed since the last run")
    expect_match(as.character(output$sweep_results$html), "from an earlier result")
    expect_match(as.character(output$budget_results$html), "from an earlier result")
    expect_false(grepl("lago_sensitivity", paste(code_lines(), collapse = "\n")))
    session$setInputs(done = 1)
  })
  expect_s3_class(returned$sensitivity, "lago_sensitivity")
  expect_s3_class(returned$budget, "lago_budget")
  expect_true(any(grepl("lago_budget", returned$code)))
})

test_that("the sweep code reproduces the sweep of the app", {
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    analysis_setup(session, col_id)
    session$setInputs(run = 1)
    session$setInputs(sweep_param = "cost_multiplier", sweep_how = "typed", sweep_typed = "0.5, 2")
    session$setInputs(run_sweep = 1)
    sw <- sweep_state()$run$value
    expect_equal(sw$rec_int_cost[2], 4 * sw$rec_int_cost[1], tolerance = 1e-6)
    env <- new.env()
    assign("mtcars", datasets::mtcars, envir = env)
    analysis_quiet(utils::capture.output(eval(parse(text = code_lines()), envir = env)))
    expect_equal(as.data.frame(env$sensitivity)$rec_int_cost, sw$rec_int_cost, tolerance = 1e-5)
  })
})

test_that("a sweep or search that fails shows the package error", {
  skip_on_cran()
  local_mocked_bindings(
    lago_sensitivity = function(...) stop("the sweep broke"),
    lago_budget = function(...) stop("the search broke")
  )
  shiny::testServer(.build_lago_app(), {
    analysis_setup(session, col_id)
    session$setInputs(run = 1)
    session$setInputs(sweep_param = "outcome_goal", sweep_how = "steps", sweep_from = 20, sweep_to = 30, sweep_steps = 3)
    session$setInputs(run_sweep = 1)
    expect_match(as.character(output$sweep_results$html), "could not run with these values")
    expect_match(as.character(output$sweep_results$html), "the sweep broke")
    session$setInputs(budget = 10, budget_grid = 4)
    session$setInputs(run_budget = 1)
    expect_match(as.character(output$budget_results$html), "the search broke")
    expect_false(grepl("lago_budget|lago_sensitivity", paste(code_lines(), collapse = "\n")))
  })
})

test_that("the report downloads, or says what to install", {
  skip_on_cran()
  skip_if(!is.null(.app_report_missing()), "the report needs rmarkdown, knitr, jsonlite and pandoc")
  shiny::testServer(.build_lago_app(), {
    analysis_setup(session, col_id)
    session$setInputs(run = 1, report_title = "Trial plan")
    path <- output$download_report
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(html, "Trial plan")
  })
  local_mocked_bindings(.app_report_missing = function(...) "Install the report packages.")
  shiny::testServer(.build_lago_app(), {
    analysis_setup(session, col_id)
    session$setInputs(run = 1)
    expect_match(as.character(output$results$html), "Install the report packages.")
  })
})
