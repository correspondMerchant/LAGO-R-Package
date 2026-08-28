# Rendering a report needs pandoc plus the Suggests that drive it: rmarkdown +
# knitr knit the template and jsonlite serializes the data the interactive D3
# plots read. Skip on CRAN and whenever any of those are unavailable.
skip_report_deps <- function() {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

# small builders used across the render tests
fit_two_comp <- function() {
  suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1, 0.5),
    quiet = TRUE
  )))
}

fit_one_comp <- function() {
  suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt"),
    intervention_lower_bounds = c(1),
    intervention_upper_bounds = c(40),
    cost_list_of_vectors = list(c(0, 1700)),
    outcome_goal = 0.7,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1),
    quiet = TRUE
  )))
}

fit_three_comp <- function() {
  suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c(
      "coaching_updt", "launch_duration", "leadership_updt"
    ),
    intervention_lower_bounds = c(1, 1, 0),
    intervention_upper_bounds = c(40, 5, 1),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000), c(0, 3000)),
    outcome_goal = 0.7,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(5, 1, 0.5),
    quiet = TRUE
  )))
}

# an interaction-terms fit (mirrors tests/testthat/test-interaction-terms.R): the
# interaction column "a:b" is a real product column, main_components are the two
# knobs, and cost_list_of_vectors / bounds are per MAIN component. The cost
# curves must be labelled by the main components, not by x$intervention_components
# (which carries the extra, backticked "`a:b`" entry).
fit_interaction <- function() {
  set.seed(11)
  grid <- expand.grid(a = 0:4, b = 0:4)
  a <- rep(grid$a, times = 6)
  b <- rep(grid$b, times = 6)
  y <- 1 + 0.5 * a + 0.3 * b + 0.2 * a * b + rnorm(length(a), 0, 0.4)
  d <- data.frame(y = y, a = a, b = b)
  d[["a:b"]] <- d$a * d$b
  suppressWarnings(suppressMessages(lago_optimization(
    data = d,
    outcome_name = "y",
    outcome_type = "continuous",
    glm_family = "gaussian",
    link = "identity",
    intervention_components = c("a", "b", "a:b"),
    main_components = c("a", "b"),
    include_interaction_terms = TRUE,
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = 3,
    outcome_goal_intention = "maximize",
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(1, 1),
    confidence_set_grid_step_size = c(1, 1),
    quiet = TRUE
  )))
}

read_html <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")

test_that("lago_report() renders a non-empty HTML file with the key sections", {
  skip_report_deps()

  r <- fit_two_comp()

  f <- lago_report(r)
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)

  html <- read_html(f)
  expect_true(grepl("Inputs", html))
  expect_true(grepl("Recommended", html))
  expect_true(grepl("0.85", html))
  expect_true(grepl("Confidence set plot", html))
  expect_true(grepl("Outcome model fit", html))
})

test_that("the report wires the interactive D3 pieces and embeds the data", {
  skip_report_deps()

  r <- fit_two_comp()
  f <- lago_report(r)
  html <- read_html(f)

  # the vendored D3 v7 library is inlined (its ISC copyright header is a stable
  # marker that only appears when the library body itself is present).
  expect_true(grepl("Mike Bostock", html, fixed = TRUE))

  # the report's own D3 code is inlined and both render entry points are called
  # against their target divs.
  expect_true(grepl("LAGOReport.renderConfidenceSet", html, fixed = TRUE))
  expect_true(grepl("LAGOReport.renderCostCurves", html, fixed = TRUE))

  # the interactive container divs exist.
  expect_true(grepl("id=\"lago-cs-plot\"", html, fixed = TRUE))
  expect_true(grepl("id=\"lago-cost-curves\"", html, fixed = TRUE))

  # the JSON payload the plots read is embedded, including confidence-set data
  # (a component column and the CI/cost columns) so the plots have real values.
  expect_true(grepl("var LAGO_REPORT_DATA =", html, fixed = TRUE))
  expect_true(grepl("coaching_updt", html, fixed = TRUE))
  expect_true(grepl("CI_lower_bound", html, fixed = TRUE))
})

test_that("the rendered report is fully self-contained (no external assets)", {
  skip_report_deps()

  r <- fit_two_comp()
  f <- lago_report(r)
  html <- read_html(f)

  # nothing is loaded from the network: no external <script src>, <link href>,
  # or generic http(s) asset references. Everything is inlined.
  expect_false(grepl("src=\"http", html))
  expect_false(grepl("src='http", html))
  expect_false(grepl("<script src=\"http", html))
  expect_false(grepl("href=\"http[^\"]*\\.css", html))
})

test_that("a single-component result renders the interactive views", {
  skip_report_deps()

  r <- fit_one_comp()
  expect_length(r$display_components, 1)

  f <- lago_report(r)
  expect_true(file.exists(f))
  html <- read_html(f)
  # a 1-component confidence set still renders interactively (the strip view)
  expect_true(grepl("Confidence set plot", html))
  expect_true(grepl("LAGOReport.renderConfidenceSet", html, fixed = TRUE))
  expect_true(grepl("Cost functions", html))
})

test_that("a 3-component result renders via the fallback path without error", {
  skip_report_deps()

  r <- fit_three_comp()
  expect_length(r$display_components, 3)

  # must not error even though the interactive confidence-set view does not
  # apply to 3+ components.
  expect_error(f <- lago_report(r), NA)
  expect_true(file.exists(f))
  html <- read_html(f)
  # no interactive confidence-set plot is wired for 3+ components (plot.lago()
  # draws nothing there), but the cost curves still render.
  expect_false(grepl(
    "LAGOReport.renderConfidenceSet(\"lago-cs-plot\"", html,
    fixed = TRUE
  ))
  expect_true(grepl("LAGOReport.renderCostCurves", html, fixed = TRUE))
})

test_that("lago_report() renders without a plot section when there is no CS", {
  skip_report_deps()

  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    include_confidence_set = FALSE,
    quiet = TRUE
  )))

  f <- lago_report(r)
  expect_true(file.exists(f))
  html <- read_html(f)
  expect_true(grepl("Recommended", html))
  expect_false(grepl("Confidence set plot", html))
  # the outcome-model fit section is independent of the confidence set
  expect_true(grepl("Outcome model fit", html))
  # cost curves do not depend on the confidence set, so they still render
  expect_true(grepl("Cost functions", html))
})

test_that("cost curves are labelled by the main components under interactions", {
  skip_report_deps()

  r <- fit_interaction()
  # the cost inputs are per main component, so both are length 2
  expect_length(r$display_components, 2)
  expect_length(r$cost_list_of_vectors, 2)
  # x$intervention_components carries the extra, backticked interaction entry
  expect_true(any(grepl("`a:b`", r$intervention_components, fixed = TRUE)))

  f <- lago_report(r)
  html <- read_html(f)

  # the serialized cost payload labels the curves with the main components only
  # (["a","b"]), not intervention_components (["a","b","`a:b`"]). This is the
  # exact regression a revert to x$intervention_components would reintroduce,
  # and it fails even under the conventional interaction-last ordering where the
  # rendered titles alone would still look correct by luck.
  expect_true(grepl('"cost":{"components":["a","b"]', html, fixed = TRUE))
  # and the backticked interaction name never reaches the report
  expect_false(grepl("`a:b`", html, fixed = TRUE))
  # both components' cost curves are still wired
  expect_true(grepl("Cost functions", html))
  expect_true(grepl("LAGOReport.renderCostCurves", html, fixed = TRUE))
})

test_that("lago_report() rejects non-lago objects", {
  expect_error(lago_report(list(1, 2)), "lago")
})
