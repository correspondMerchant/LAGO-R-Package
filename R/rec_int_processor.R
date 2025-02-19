rec_int_processor <- function(
    data,
    model,
    center_characteristics,
    include_center_effects,
    include_time_effects,
    include_interaction_terms,
    main_components,
    intervention_components,
    optimization_method,
    optimization_grid_search_step_size,
    link,
    center_weights_for_outcome_goal,
    cost_list_of_vectors,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal,
    center_characteristics_optimization_values,
    time_effect_optimization_value,
    lower_outcome_goal,
    prev_recommended_interventions,
    shrinkage_threshold,
    power_goal,
    power_goal_approach,
    num_centers_in_next_stage,
    patients_per_center_in_next_stage,
    outcome_name) {
  # get coefficients for the intervention components
  intervention_components_coeff <-
    model$coefficients[c("(Intercept)", intervention_components)]

  # get coefficients for the center characteristics
  if (!is.null(center_characteristics)) {
    center_characteristics_coeff <- model$coefficients[center_characteristics]
  }

  # set the values of center_cha_coeff_vec and
  # center_characteristics_optimization_values based on if
  # center_characteristics is defined.
  if (!is.null(center_characteristics)) {
    center_cha_coeff_vec <- center_characteristics_coeff
  } else {
    center_cha_coeff_vec <- 0
    center_characteristics_optimization_values <- 0
  }

  # get coefficients for facilities, which includes both the fixed
  # center effects (if specified) and fixed time effects (if specified)
  if (include_center_effects) {
    all_coefs <- coef(model)
    intercept_only <- all_coefs["(Intercept)"]
    fixed_center_coefs <- all_coefs[
      grep("center", names(all_coefs), ignore.case = TRUE)
    ]
    # add intercept to each center coefficient to get "true" effects
    true_center_effects <- fixed_center_coefs + intercept_only
    all_center_lvl_effects <- c(intercept_only, true_center_effects)
  } else {
    all_center_lvl_effects <- 0
  }

  # add fixed time effect (if specified)
  if (include_time_effects) {
    all_center_lvl_effects <- all_center_lvl_effects +
      all_coefs[
        grep(
          paste0("period", time_effect_optimization_value, "$"),
          names(all_coefs),
          ignore.case = TRUE
        )
      ]
  }

  # should be deleted later, after cost for PULESA part is done
  # print("intervention coefficients: ")
  # print(intervention_components_coeff)
  # print("center level effects:")
  # print(all_center_lvl_effects)
  # print("optimization method:")
  # print(optimization_method)
  # print("intervention components:")
  # print(intervention_components)

  # calculate default step size
  # 1/20th of the range for each intervention component
  components_for_step_size <- if (include_interaction_terms) {
    main_components
  } else {
    intervention_components
  }
  step_size_results <- numeric(length(components_for_step_size))
  for (i in seq_along(components_for_step_size)) {
    current_intervention <- gsub("`", "", components_for_step_size[i])
    current_range <- range(data[[current_intervention]], na.rm = TRUE)
    step_size_results[i] <- (current_range[2] - current_range[1]) * (1 / 20)
  }
  if (optimization_method == "grid_search" &&
    is.null(optimization_grid_search_step_size)) {
    optimization_grid_search_step_size <- step_size_results
  }

  # calculate recommended interventions
  rec_int_results <- get_recommended_interventions(
    data = data,
    link = link,
    intervention_components_coeff = intervention_components_coeff,
    include_interaction_terms = include_interaction_terms,
    main_components = main_components,
    intervention_components = intervention_components,
    all_center_lvl_effects = all_center_lvl_effects,
    center_weights_for_outcome_goal = center_weights_for_outcome_goal,
    cost_list_of_vectors = cost_list_of_vectors,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    outcome_goal = outcome_goal,
    optimization_method = optimization_method,
    optimization_grid_search_step_size = optimization_grid_search_step_size,
    center_cha_coeff_vec = center_cha_coeff_vec,
    center_characteristics_optimization_values =
      center_characteristics_optimization_values,
    lower_outcome_goal = lower_outcome_goal,
    prev_recommended_interventions = prev_recommended_interventions,
    shrinkage_threshold = shrinkage_threshold,
    power_goal = power_goal,
    power_goal_approach = power_goal_approach,
    num_centers_in_next_stage = num_centers_in_next_stage,
    patients_per_center_in_next_stage = patients_per_center_in_next_stage,
    outcome_name = outcome_name
  )

  list(
    rec_int = rec_int_results$est_rec_int,
    rec_int_cost = rec_int_results$rec_int_cost,
    est_outcome_goal = rec_int_results$est_reachable_outcome,
    step_size_results = step_size_results,
    shrinking_method_used = rec_int_results$shrinking_method_used
  )
}
