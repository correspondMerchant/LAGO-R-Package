rec_int_processor <- function(
    data,
    model,
    center_characteristics,
    additional_covariates = NULL,
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
    outcome_name,
    icc = NULL,
    power_goal_cluster_id = NULL) {
  # the coefficient names glm() gave each term of the model, so every
  # coefficient below is picked out by the term it came from rather than by
  # what its name happens to look like. A term's own name is in general NOT one
  # of its coefficient names: glm() expands a factor or character term into one
  # dummy per non-reference level and names each dummy after its level, so a
  # column vol_hi with levels lo/hi is a coefficient named vol_hihi.
  coef_mapping <- term_coef_names(model)
  all_coefs <- coef(model)

  # get coefficients for the intervention components
  intervention_components_coeff <-
    model$coefficients[c("(Intercept)", intervention_components)]

  # get coefficients for the center characteristics. Looked up through the term
  # mapping: indexing model$coefficients by the raw column name yielded NA for
  # a factor or character characteristic, since its coefficient is named after
  # its level, and the NA then propagated through the estimated outcome and
  # broke the optimization.
  if (!is.null(center_characteristics)) {
    center_characteristics_coeff <- all_coefs[
      center_characteristic_coef_names(center_characteristics, coef_mapping)
    ]
    if (anyNA(center_characteristics_coeff)) {
      stop(paste0(
        "The center characteristic(s) ",
        paste(
          center_characteristics[is.na(center_characteristics_coeff)],
          collapse = ", "
        ),
        " have no coefficient in the outcome model, so the recommended ",
        "intervention cannot be computed at the values supplied for them."
      ))
    }
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
  # center effects (if specified) and fixed time effects (if specified).
  # The fixed center and time effect dummies are identified by the term they
  # came from rather than by what their names happen to look like. A covariate
  # whose own name starts with "center", e.g. center_size, came from its own
  # term and so is never taken for a center dummy. Adding it to the
  # center-level effects would shift every predicted outcome by its
  # coefficient, and the first-element lookups downstream would then read a
  # coefficient that is not a center effect at all.
  # The fallback for a model whose term mapping could not be rebuilt, which
  # outcome_model_fitting() does not produce, is the name search this used to
  # do, anchored so that only a name beginning with the term is matched, and
  # restricted to the coefficients no other block below claims for itself.
  #
  # That exclusion list is the point of named_predictors, and it has to be the
  # names THIS function looks up on its own account, exactly as
  # get_confidence_set() passes its own: the intervention components, the
  # additional covariates, the center characteristics and the intercept. Passing
  # character(0) excluded nothing, so on the fallback a covariate or
  # characteristic named center_size was claimed as a center dummy and
  # period_flag as a period dummy. Reading center_size as a center dummy
  # adds its coefficient to all_center_lvl_effects and makes that vector one
  # longer than center_weights_for_outcome_goal, which shifts every predicted
  # outcome and recycles the weights: the #68 defect, one layer down.
  named_predictors <- gsub("`", "", c(
    "(Intercept)", intervention_components, additional_covariates,
    center_characteristics
  ))
  fixed_effect_coefs <- function(term) {
    fixed_effect_coef_names(
      term, coef_mapping, names(all_coefs), named_predictors
    )
  }

  if (include_center_effects) {
    intercept_only <- all_coefs["(Intercept)"]
    fixed_center_coefs <- all_coefs[fixed_effect_coefs("center")]
    # add intercept to each center coefficient to get "true" effects
    true_center_effects <- fixed_center_coefs + intercept_only
    all_center_lvl_effects <- c(intercept_only, true_center_effects)
  } else {
    all_center_lvl_effects <- 0
  }

  # add the fixed time effect of the requested period (if specified). The
  # period is resolved against the period levels the model was fitted on and
  # its dummy is matched by exact name, so a value that is not a period raises
  # instead of contributing nothing. Matching by pattern let the requested
  # period miss every dummy and silently return a zero-length coefficient,
  # which collapsed all_center_lvl_effects to length zero and made the
  # estimated outcome 0. The reference period contributes 0, since glm() leaves
  # it out of the dummies and it is already carried by the intercept.
  if (include_time_effects) {
    period_coefs <- fixed_effect_coefs("period")
    if (length(period_coefs) == 0) {
      stop(paste0(
        "'include_time_effects' is TRUE but no fixed time effect coefficient ",
        "could be identified in the outcome model. It must be fitted with a ",
        "'period' term, as outcome_model_fitting() builds it."
      ))
    }
    period_indicators <- time_effect_indicator(
      model, period_coefs, time_effect_optimization_value
    )
    all_center_lvl_effects <- all_center_lvl_effects +
      sum(period_indicators * all_coefs[period_coefs])
  }

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
    outcome_name = outcome_name,
    icc = icc,
    power_goal_cluster_id = power_goal_cluster_id
  )

  list(
    rec_int = rec_int_results$est_rec_int,
    rec_int_cost = rec_int_results$rec_int_cost,
    est_outcome_goal = rec_int_results$est_reachable_outcome,
    step_size_results = step_size_results,
    shrinking_method_used = rec_int_results$shrinking_method_used,
    effective_outcome_goal = rec_int_results$effective_outcome_goal
  )
}
