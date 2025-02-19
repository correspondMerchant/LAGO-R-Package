confidence_set_processor <- function(
    data,
    confidence_set_grid_step_size,
    step_size_results,
    include_center_effects,
    include_time_effects,
    intervention_components,
    additional_covariates,
    center_characteristics,
    center_weights_for_outcome_goal,
    include_interaction_terms,
    main_components,
    outcome_name,
    model,
    family_object,
    outcome_goal,
    outcome_type,
    intervention_lower_bounds,
    intervention_upper_bounds,
    center_characteristics_optimization_values,
    confidence_set_alpha,
    cost_list_of_vectors,
    rec_int) {
  # assign confidence set step size if not specified
  if (is.null(confidence_set_grid_step_size)) {
    confidence_set_grid_step_size <- step_size_results
  }

  # calculate the confidence set for the recommended interventions
  cat(paste(
    "If the confidence set calculation takes a long time to run,",
    "please consider changing the confidence set step size. \n"
  ))
  predictors_list <- c(
    if (include_center_effects) "center",
    if (include_time_effects) "period",
    intervention_components,
    if (!is.null(additional_covariates)) additional_covariates,
    if (!is.null(center_characteristics)) center_characteristics
  )
  predictors_list <- gsub("`", "", predictors_list)

  # convert columns of the fixed effects to factor type
  if (include_center_effects || include_time_effects) {
    cluster_id <- list()
    if (include_center_effects) {
      data$center <- factor(data$center)
      cluster_id <- append(cluster_id, list(data$center))
    }
    if (include_time_effects) {
      data$period <- factor(data$period)
      cluster_id <- append(cluster_id, list(data$period))
    }
  } else {
    cluster_id <- NULL
  }

  cs <- get_confidence_set(
    predictors_data = data[, predictors_list],
    include_center_effects,
    center_weights_for_outcome_goal,
    include_time_effects,
    additional_covariates,
    intervention_components,
    include_interaction_terms,
    main_components,
    outcome_data = data[, outcome_name],
    fitted_model = model,
    link = family_object$link,
    outcome_goal = outcome_goal,
    outcome_type = outcome_type,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    confidence_set_grid_step_size = confidence_set_grid_step_size,
    center_characteristics = center_characteristics,
    center_characteristics_optimization_values =
      center_characteristics_optimization_values,
    confidence_set_alpha = confidence_set_alpha,
    cluster_id = cluster_id,
    cost_list_of_vectors,
    rec_int = rec_int
  )

  list(
    cs = cs
  )
}
