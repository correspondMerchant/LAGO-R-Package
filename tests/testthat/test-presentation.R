# Snapshot tests for the restyled console output. These pin the rendered
# print()/summary() text for a representative set of `lago` objects so the
# shared presentation formatter (lago_blocks() + .lago_render()) cannot drift.
#
# The underlying optimizations are the same deterministic runs exercised in
# test-optimization.R / test-goal-modes-and-diagnostics.R, so the recommended
# interventions, costs, and confidence sets are reproducible. expect_snapshot()
# renders with local_reproducible_output() (fixed width, colours off), so the
# captured cli output is stable across machines.

bb_grouped <- function() {
  d <- BB_data
  d$group <- ifelse(d$pre_post == 0, "control", "treatment")
  d
}

# --- no double-render on an unassigned top-level call -------------------------

test_that("a non-quiet unassigned call renders the result box exactly once", {
  # An unassigned top-level call auto-prints its return value only when that
  # value is *visible*. lago_optimization() also prints the in-run summary when
  # !quiet, so a visible return would render the box twice. The function must
  # return invisibly. withVisible() on the call itself reports the visibility a
  # top-level autoprint would see.
  vis <- NULL
  captured <- utils::capture.output(
    vis <- suppressWarnings(suppressMessages(withVisible(lago_optimization(
      data = BB_data,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = "birth_volume_100",
      center_characteristics_optimization_values = 1.75,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      outcome_goal_intention = "maximize",
      confidence_set_grid_step_size = c(1, 0.5),
      quiet = FALSE
    ))))
  )
  captured <- paste(captured, collapse = "\n")
  # Invisible return: a top-level autoprint would add no second render.
  expect_false(vis$visible)
  expect_s3_class(vis$value, "lago")
  # The in-run print produced exactly one result box.
  expect_equal(
    length(gregexpr("LAGO optimization result", captured, fixed = TRUE)[[1]]),
    1L
  )
})

# --- binary + confidence set --------------------------------------------------

test_that("print/summary snapshot: binary object with a confidence set", {
  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1, 0.5),
    quiet = TRUE
  )))
  expect_snapshot(print(r))
  expect_snapshot(summary(r))
})

# --- continuous ---------------------------------------------------------------

test_that("print/summary snapshot: continuous object", {
  r <- suppressWarnings(suppressMessages(lago_optimization(
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
    confidence_set_grid_step_size = c(1, 1),
    quiet = TRUE
  )))
  expect_snapshot(print(r))
  expect_snapshot(summary(r))
})

# --- power goal (no confidence set) -------------------------------------------

test_that("print/summary snapshot: power-goal object without a confidence set", {
  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = bb_grouped(),
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    power_goal = 0.8,
    num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 30,
    include_confidence_set = FALSE,
    quiet = TRUE
  )))
  expect_snapshot(print(r))
  expect_snapshot(summary(r))
})

# --- single component ---------------------------------------------------------

test_that("print/summary snapshot: single-component object", {
  d <- data.frame(
    dose = rep(0:3, each = 4),
    y = c(0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
  )
  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = d,
    outcome_name = "y",
    outcome_type = "binary",
    intervention_components = "dose",
    intervention_lower_bounds = 0,
    intervention_upper_bounds = 3,
    cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = 0.6,
    outcome_goal_intention = "maximize",
    include_confidence_set = TRUE,
    confidence_set_grid_step_size = 1,
    quiet = TRUE
  )))
  expect_snapshot(print(r))
  expect_snapshot(summary(r))
})

# --- no confidence set --------------------------------------------------------

test_that("print/summary snapshot: object without a confidence set", {
  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    include_confidence_set = FALSE,
    quiet = TRUE
  )))
  expect_snapshot(print(r))
  expect_snapshot(summary(r))
})
