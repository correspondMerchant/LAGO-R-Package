test_that("lago_report() renders a non-empty HTML file with the key sections", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1, 0.5),
    quiet = TRUE
  )))

  f <- lago_report(r)
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)

  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("Inputs", html))
  expect_true(grepl("Recommended", html))
  expect_true(grepl("0.85", html))
  expect_true(grepl("Confidence set plot", html))
  expect_true(grepl("Outcome model fit", html))
})

test_that("lago_report() renders without a plot section when there is no CS", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    include_confidence_set = FALSE,
    quiet = TRUE
  )))

  f <- lago_report(r)
  expect_true(file.exists(f))
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("Recommended", html))
  expect_false(grepl("Confidence set plot", html))
  # the outcome-model fit section is independent of the confidence set
  expect_true(grepl("Outcome model fit", html))
})

test_that("lago_report() rejects non-lago objects", {
  expect_error(lago_report(list(1, 2)), "lago")
})
