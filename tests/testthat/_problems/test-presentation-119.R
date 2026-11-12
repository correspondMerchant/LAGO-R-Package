# Extracted from test-presentation.R:119

# prequel ----------------------------------------------------------------------
bb_grouped <- function() {
  d <- BB_data
  d$group <- ifelse(d$pre_post == 0, "control", "treatment")
  d
}

# test -------------------------------------------------------------------------
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
