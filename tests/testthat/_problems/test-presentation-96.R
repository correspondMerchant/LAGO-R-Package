# Extracted from test-presentation.R:96

# prequel ----------------------------------------------------------------------
bb_grouped <- function() {
  d <- BB_data
  d$group <- ifelse(d$pre_post == 0, "control", "treatment")
  d
}

# test -------------------------------------------------------------------------
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
