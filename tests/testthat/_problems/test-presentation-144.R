# Extracted from test-presentation.R:144

# prequel ----------------------------------------------------------------------
bb_grouped <- function() {
  d <- BB_data
  d$group <- ifelse(d$pre_post == 0, "control", "treatment")
  d
}

# test -------------------------------------------------------------------------
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
