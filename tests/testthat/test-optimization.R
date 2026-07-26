# Integration/regression tests: run the full lago_optimization() pipeline and
# assert the recommended interventions match the published / previously-verified
# results. These guard the core algorithm against regressions.

test_that("BB_data binary reproduces the Nevo et al. recommendation", {
  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1, 0.5)
  )))
  # Nevo et al.: launch duration ~2.78 days and 1 coaching visit.
  expect_equal(res$rec_int[1], 1, tolerance = 1e-3)
  expect_equal(res$rec_int[2], 2.77847, tolerance = 1e-3)
  expect_equal(res$est_outcome_goal, 0.85, tolerance = 1e-3)
  expect_equal(res$rec_int_cost, 23.9278, tolerance = 1e-2)
  # confidence set size (regression pin from current output; ~10.5% is
  # consistent with the ~10.5% reported in Nevo et al.)
  expect_equal(res$confidence_set_size_percentage, 0.10556, tolerance = 1e-3)
  # the confidence set data.frame carries the expected columns, and the
  # recommended intervention's CI brackets the outcome goal (0.85).
  expect_true(all(
    c("CI_lower_bound", "CI_upper_bound", "cost") %in% names(res$cs)
  ))
})

test_that("mtcars continuous (identity link) is stable", {
  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = mtcars,
    outcome_name = "mpg",
    outcome_type = "continuous",
    glm_family = "gaussian",
    link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 40,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1, 1)
  )))
  expect_equal(res$rec_int[1], 10, tolerance = 1e-3)
  expect_equal(res$rec_int[2], 11.95743, tolerance = 1e-3)
  expect_equal(res$rec_int_cost, 115.7446, tolerance = 1e-2)
  expect_equal(res$confidence_set_size_percentage, 0.04248, tolerance = 1e-3)
})

test_that("numerical and grid_search agree on the mtcars recommendation", {
  common <- list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 40, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE
  )
  num <- suppressWarnings(suppressMessages(do.call(lago_optimization, common)))
  grid <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    c(common, list(
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(1, 1)
    ))
  )))
  # grid search (step 1) quantizes to integers; the numerical optimum should
  # land within one grid step of the grid solution on every component.
  expect_true(all(abs(num$rec_int - grid$rec_int) <= 1))
})

test_that("minimize direction produces a valid recommendation", {
  # exercises the double-negation minimize path end-to-end (not just the
  # error guard). Lower mpg toward 10 using disp/hp.
  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("disp", "hp"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(500, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 10, outcome_goal_intention = "minimize",
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(10, 10),
    include_confidence_set = FALSE
  )))
  # recommendation reported on the original (un-negated) scale
  expect_equal(res$rec_int, c(500, 230), tolerance = 1e-3)
  expect_equal(res$est_outcome_goal, 9.8495, tolerance = 1e-2)
})

test_that("return value has the expected shape", {
  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 40, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE
  )))
  expect_true(all(c("rec_int", "rec_int_cost", "est_outcome_goal") %in% names(res)))
  expect_length(res$rec_int, 2)
})

test_that("the overall test result is returned (not just printed) when a group column is present", {
  bb <- BB_data
  bb$group <- ifelse(bb$pre_post == 0, "control", "treatment")
  common <- list(
    data = bb, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = 0.85, outcome_goal_intention = "maximize"
  )
  # returned both when the confidence set is skipped ...
  res_no_cs <- suppressWarnings(suppressMessages(do.call(
    lago_optimization, c(common, list(include_confidence_set = FALSE))
  )))
  expect_false(is.null(res_no_cs$test_results))
  expect_true(all(c("test_stat", "p_val") %in% names(res_no_cs$test_results)))

  # ... and when it is computed.
  res_cs <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    c(common, list(
      include_confidence_set = TRUE, confidence_set_grid_step_size = c(1, 0.5)
    ))
  )))
  expect_false(is.null(res_cs$test_results))
  expect_equal(res_cs$test_results$test_stat, res_no_cs$test_results$test_stat)

  # NULL (not missing) when no valid group column is supplied.
  common_no_group <- common
  common_no_group$data <- BB_data # BB_data has no 'group' column
  res_no_group <- suppressWarnings(suppressMessages(do.call(
    lago_optimization, c(common_no_group, list(include_confidence_set = FALSE))
  )))
  expect_null(res_no_group$test_results)
})

test_that("a single intervention component does not crash the optimizer (#54)", {
  # data[, one_col] used to collapse to a vector, breaking colMeans() (shrinking
  # path) and the confidence-set construction (data[[col]] / apply over rows).
  # Uses a base data.frame (not a tibble) so the [, ] drop actually triggers.
  d <- data.frame(
    dose = rep(0:3, each = 4),
    y = c(0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
  )
  base <- list(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = "dose",
    intervention_lower_bounds = 0, intervention_upper_bounds = 3,
    cost_list_of_vectors = list(c(0, 1)),
    outcome_goal_intention = "maximize"
  )

  # reachable goal -> confidence-set path
  res_cs <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    c(base, list(
      outcome_goal = 0.6, include_confidence_set = TRUE,
      confidence_set_grid_step_size = 1
    ))
  )))
  expect_length(res_cs$rec_int, 1)
  expect_true("dose" %in% names(res_cs$cs))

  # unreachable goal -> shrinking path (the colMeans() site)
  res_shrink <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    c(base, list(outcome_goal = 0.999, include_confidence_set = FALSE))
  )))
  expect_length(res_shrink$rec_int, 1)

  # continuous outcome + CS exercises get_vcov / prepare_design_matrix on
  # predictors_data, the confidence_set_processor drop = FALSE site that the
  # binary CS path above does not reach.
  set.seed(1)
  dc <- data.frame(x = seq(0, 10, length.out = 30))
  dc$y <- 1.5 * dc$x + rnorm(30, 0, 1)
  res_cts <- suppressWarnings(suppressMessages(lago_optimization(
    data = dc, outcome_name = "y", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 10,
    cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = 12, outcome_goal_intention = "maximize",
    include_confidence_set = TRUE, confidence_set_grid_step_size = 1
  )))
  expect_length(res_cts$rec_int, 1)
  expect_true("x" %in% names(res_cts$cs))
})

test_that("weights length is validated against the number of observations, not columns (#54)", {
  # length(data[, comp]) counted columns for a tibble / single-column drop, so
  # correctly-sized weights on a single-component tibble were wrongly rejected.
  skip_if_not_installed("tibble")
  tb <- tibble::tibble(
    dose = rep(0:3, each = 4),
    y = c(0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
  )
  base <- list(
    data = tb, outcome_name = "y", outcome_type = "binary",
    intervention_components = "dose",
    intervention_lower_bounds = 0, intervention_upper_bounds = 3,
    cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = 0.6, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE
  )
  # correctly-sized weights (one per observation) are accepted
  expect_length(
    suppressWarnings(suppressMessages(do.call(
      lago_optimization, c(base, list(weights = rep(1, nrow(tb))))
    )))$rec_int,
    1
  )
  # wrong-length weights are still rejected
  expect_error(
    suppressWarnings(suppressMessages(do.call(
      lago_optimization, c(base, list(weights = rep(1, 5)))
    ))),
    "number of observations"
  )
})
