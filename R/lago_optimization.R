#' lago_optimization
#'
#' @description Fitting the outcome model, calculates the recommended
#' interventions based on an outcome goal and/or a power goal, calculates
#' the confidence set for the recommended interventions, and prints the output.
#'
#' @param data A data.frame. The input dataset containing the variables of
#' interest.
#' @param outcome_name A character string. The name of the column in the dataset
#' that represents the outcome of interest.
#' @param outcome_type A character string. Specifies the type of the outcome.
#' Must be either "continuous" for continuous outcomes or "binary" for binary
#' outcomes.
#' @param intervention_components A character vector. The names of the columns
#' in the dataset that represent the intervention components.
#' For example: c("component1", "component2").
#' @param intervention_lower_bounds A numeric vector. Specifies the lower bounds
#' of the intervention components.
#' For example: for a two-component intervention package, lower bounds could be
#' c(0,0).
#' @param intervention_upper_bounds A numeric vector. Specifies the upper bounds
#' of the intervention components.
#' For example: for a two-component intervention package, upper bounds could be
#' c(10,20).
#' @param outcome_goal A numeric value. Specifies the outcome goal, a desired
#' probability or mean value.
#' @param outcome_goal_intention A character string. Specifies the intention of
#' the outcome goal. Must be either "maximize" or "minimize". If the goal is to
#' increase the outcome, set it to "maximize". If the goal is to decrease the
#' outcome, set it to "minimize".
#'
#' @param unit_costs A numeric vector. Specifies the unit costs for each
#' intervention component.
#' Default value without user specification: NULL.
#' @param default_cost_fxn_type A character string. Specifies the shape of the
#' default cost function. Must be either "linear" or "cubic".
#' Default value without user specification: "cubic".
#' @param cost_list_of_vectors A list of numeric vectors. Specifies the cost
#' functions for each intervention component. Each numeric vector in the list
#' contains coefficients of the cost function for one intervention component.
#' For example:
#' list(c(1, 2, 3, 4), c(4, 6), c(5, 4, 3)) represents:
#' - First component: cost = 1 + 2x_1 + 3x_1^2 + 4x_1^3
#' - Second component: cost = 4 + 6x_2
#' - Third component: cost = 5 + 4x_3 + 3x_3^2
#' list(c(0, 2), c(0, 6), c(0, 4)) represents:
#' - First component: cost = 2x_1
#' - Second component: cost = 6x_2
#' - Third component: cost = 4x_3
#' Default value without user specification: NULL.
#' Please note that either the unit_costs or cost_list_of_vectors must be
#' specified. If both are specified, the cost_list_of_vectors will be used.
#' @param input_data_structure A character string. The data structure of the
#' input data. Must be either "individual_level" or "center_level".
#' Default value without user specification: "individual_level".
#' @param glm_family A character string. The family of the glm() outcome model.
#' Default value without user specification:
#' "gaussian" for continuous outcomes, "binomial" for binary outcomes.
#' @param link A character string. The link function of the glm() outcome model.
#' Default value without user specification:
#' "identity" for continuous outcomes, "logit" for binary outcomes.
#' @param weights A numeric vector. The weights argument of the glm()
#' outcome model.
#' @param center_characteristics A character vector. The names of the columns in
#' the dataset that represent the center characteristics.
#' Note, include_center_effects and center_characteristics cannot be used
#' together at the same time.
#' For example: c("characteristic1")
#' @param center_characteristics_optimization_values A numeric vector. The
#' values of the center characteristics that will be used for LAGO optimization.
#' For example: c(1.74)
#' @param include_center_effects A boolean. Specifies whether the fixed effects
#' should be included in the outcome model. If set to TRUE, please make sure
#' that the input data has a 'center' column to identify the centers.
#' Note, include_center_effects and center_characteristics cannot be used
#' together at the same time.
#' Default value without user specification: FALSE for individual level data,
#' TRUE for center level data.
#' @param center_effects_optimization_values A numeric vector. The center of
#' interest that will be used for center-specific LAGO optimization. This is
#' only used when include_center_effects is set to TRUE. If not specified,
#' the LAGO optimization will be carried out for a weighted average of all
#' centers.
#' @param include_time_effects A boolean. Specifies whether the fixed time
#' effects should be included in the outcome model. If set to TRUE, please
#' make sure that the input data has a 'period' column to identify the time
#' periods.
#' Default value without user specification: FALSE.
#' @param include_interaction_terms A boolean. Specifies whether there are
#' interaction terms in the intervention components. Please make sure the
#' interaciton terms are included in "intervention_components", and they
#' follow the correct naming scheme: "component1:component3".
#' Default value without user specification: FALSE.
#' @param main_components A character vector. Specifies the main intervention
#' components in the presence of interaction terms.
#' For example: c("component1", "component2").
#' @param time_effect_optimization_value A numeric value. The value of the time
#' effect that will be used for LAGO optimization.
#' @param  center_weights_for_outcome_goal A numeric vector. Specifies the
#' weights that will be used for calculating recommended interventions that
#' satisfy the outcome goal for an (weighted) average center.
#' The weights need to sum up to 1.
#' Default value without user specification:
#' For each center, calculate what percentage its sample size is of the total
#' samples across all facilities - this percentage serves as that
#' center's weight.
#' @param additional_covariates A character vector. The names of the columns in
#' the dataset that represent additional covaraites that need to be included
#' in the outcome model. This includes interaction terms or any other additional
#' covariates.
#' For example: c("component2xcomponent4").
#' @param optimization_method A character string. Specifies the method used for
#' LAGO optimization. Must be either "numerical" or "grid_search".
#' Default value without user specification: "numerical".
#' - Use "grid_search" if you want to exhaustively test every possible
#' intervention package compositions in LAGO optimization.
#' - Use "numerical" if you want to use gradient-based technique in LAGO
#' optimization.
#' @param optimization_grid_search_step_size A numeric vector. Specifies the
#' step size of the grid search algorithm used in LAGO optimization.
#' Default value without user specification:
#' 1/20 of the range for each intervention component.
#' @param include_confidence_set A boolean. Specifies whether the confidence set
#' should be calculated along with the recommended interventions.
#' Default value without user specification: TRUE.
#' @param confidence_set_grid_step_size A numeric vector. Specifies the step
#' size of the grid search algorithm used in the confidence set calculation.
#' Default value without user specification:
#' 1/20 of the range for each intervention component.
#' @param confidence_set_alpha A numeric value. The type I error considered in
#' the confidence set calculations.
#' Default value without user specification: 0.05.
#' @param prev_recommended_interventions A numeric vector. The recommended
#' interventions from a previous stage of the LAGO optimization. If provided,
#' this will be used in the shrinking method (see Section 5.1 of the
#' Supplementary material of Nevo et al. (2021) for details).
#' @param shrinkage_threshold A numeric value. The threshold for the shrinkage
#' method to kick in. This threshold represents the proportion of the distance
#' between the estimated outcome without intervention, and the outcome goal. If
#' the maximum reachable outcome is less than this value, the shrinkage method
#' from Section 5.1 of the Supplementary Materials of Nevo et al. (2021)
#' will be applied. If the maximum reachable outcome is greater than this value,
#' then the maximum reachable outcome will be used as the outcome goal, and the
#' shrinkage method will not be used.
#' Default value without user specification: 0.25.
#' @param power_goal A numeric value. Specifies the power goal, a desired power
#' value.
#' Default value without user specification: NULL.
#' @param power_goal_approach A character string. Specifies the approach to
#' achieve the power goal. Must be either "unconditional" or "conditional".
#' Default value without user specification: "unconditional".
#' @param num_centers_in_next_stage A numeric value. Specifies the number of
#' total centers (treatment + control) in the next stage of the trial.
#' Default value without user specification: NULL.
#' @param patients_per_center_in_next_stage A numeric value. Specifies
#' the number of patients per center in the next stage of the trial.
#' Default value without user specification: NULL.
#'
#' @return List(
#' recommended interventions,
#' associated cost for the interventions,
#' estimated outcome mean/probability for the intervention group,
#' 95% confidence set percentage,
#' 95% confidence set)
#'
#' @examples
#' # Basic case showing how to carry out the optimization with
#' # a built-in data set.
#' lago_optimization(
#'   data = infert,
#'   outcome_name = "case",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   intervention_components = c("age", "parity"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(50, 10),
#'   cost_list_of_vectors = list(c(0, 4), c(0, 1)),
#'   outcome_goal = 0.5,
#'   outcome_goal_intention = "maximize",
#'   confidence_set_grid_step_size = c(1, 1)
#' )
#'
#' lago_optimization(
#'   data = BB_data,
#'   outcome_name = "pp3_oxytocin_mother",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   intervention_components = c("coaching_updt", "launch_duration"),
#'   center_characteristics = c("birth_volume_100"),
#'   center_characteristics_optimization_values = 1.75,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
#'   outcome_goal = 0.85,
#'   outcome_goal_intention = "maximize",
#'   confidence_set_grid_step_size = c(1, 1)
#' )
#'
#' lago_optimization(
#'   data = BB_proportions,
#'   outcome_name = "EBP_proportions",
#'   outcome_type = "continuous",
#'   glm_family = "quasibinomial",
#'   link = "logit",
#'   intervention_components = c("coaching_updt", "launch_duration"),
#'   center_characteristics = c("birth_volume_100"),
#'   center_characteristics_optimization_values = 1.75,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
#'   outcome_goal = 0.85,
#'   outcome_goal_intention = "maximize",
#'   confidence_set_grid_step_size = c(1, 1)
#' )
#'
#' @export
#' @importFrom utils head
#' @import cli
#'
lago_optimization <- function(
    data,
    input_data_structure = "individual_level",
    outcome_name,
    outcome_type,
    intervention_components,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal,
    outcome_goal_intention,
    power_goal = NULL,
    power_goal_approach = "unconditional",
    num_centers_in_next_stage = NULL,
    patients_per_center_in_next_stage = NULL,
    unit_costs = NULL,
    default_cost_fxn_type = "cubic",
    cost_list_of_vectors = NULL,
    glm_family = "default",
    link = "default",
    optimization_method = "numerical",
    confidence_set_alpha = 0.05,
    weights = NULL,
    center_characteristics = NULL,
    center_characteristics_optimization_values = NULL,
    main_components = NULL,
    time_effect_optimization_value = NULL,
    additional_covariates = NULL,
    center_weights_for_outcome_goal = NULL,
    optimization_grid_search_step_size = NULL,
    confidence_set_grid_step_size = NULL,
    include_confidence_set = TRUE,
    include_center_effects = FALSE,
    center_effects_optimization_values = NULL,
    include_time_effects = FALSE,
    include_interaction_terms = FALSE,
    prev_recommended_interventions = NULL,
    shrinkage_threshold = 0.25) {
  cli::cli_alert_info("Starting LAGO Optimization")

  cli::cli_alert_info("Validating inputs...")
  # validate and preapre inputs
  inputs <- do.call(
    validate_inputs,
    as.list(environment())
  )
  Sys.sleep(0.25)
  cli::cli_alert_success("Done")

  # unpack the inputs to the environment
  list2env(inputs, envir = environment())

  cli::cli_alert_info("Assessing the cost function...")
  # if the user did not specify cost_list_of_vectors, we
  # calculate it based on the unit_costs and default_cost_fxn_type
  if (is.null(cost_list_of_vectors)) {
    cost_list_of_vectors <- cost_fxn_calculator(
      intervention_lower_bounds = intervention_lower_bounds,
      intervention_upper_bounds = intervention_upper_bounds,
      unit_costs = unit_costs,
      default_cost_fxn_type = default_cost_fxn_type
    )
  }
  Sys.sleep(0.25)
  cli::cli_alert_success("Done")

  cli::cli_alert_info("Fitting the outcome model...")
  # fit the outcome model
  outcome_model <- outcome_model_fitting(
    data = data,
    input_data_structure = input_data_structure,
    outcome_name = outcome_name,
    family_object = family_object,
    intervention_components = intervention_components,
    weights = weights,
    center_characteristics = center_characteristics,
    additional_covariates = additional_covariates,
    include_center_effects = include_center_effects,
    include_time_effects = include_time_effects,
    include_interaction_terms = include_interaction_terms
  )
  Sys.sleep(0.25)
  cli::cli_alert_success("Done")

  # unpack the outcome model to the environment
  list2env(outcome_model, envir = environment())

  cli::cli_alert_info("Calculating the recommended intervention...")
  if (lower_outcome_goal) {
    new_model <- model
    new_model$coefficients <- -1 * (model$coefficients)
  }
  # calculate the recommended interventions
  rec_int_results <- rec_int_processor(
    data = data,
    model = if (lower_outcome_goal) new_model else model,
    center_characteristics = center_characteristics,
    include_center_effects = include_center_effects,
    include_time_effects = include_time_effects,
    include_interaction_terms = include_interaction_terms,
    main_components = main_components,
    intervention_components = intervention_components,
    optimization_method = optimization_method,
    optimization_grid_search_step_size = optimization_grid_search_step_size,
    link = link,
    center_weights_for_outcome_goal = center_weights_for_outcome_goal,
    cost_list_of_vectors = cost_list_of_vectors,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    outcome_goal = if (lower_outcome_goal) -1 * outcome_goal else outcome_goal,
    center_characteristics_optimization_values =
      center_characteristics_optimization_values,
    time_effect_optimization_value = time_effect_optimization_value,
    lower_outcome_goal = lower_outcome_goal,
    prev_recommended_interventions = prev_recommended_interventions,
    shrinkage_threshold = shrinkage_threshold,
    power_goal = power_goal,
    power_goal_approach = power_goal_approach,
    num_centers_in_next_stage = num_centers_in_next_stage,
    patients_per_center_in_next_stage = patients_per_center_in_next_stage,
    outcome_name = outcome_name
  )
  Sys.sleep(0.25)
  cli::cli_alert_success("Done")

  # unpack the recommended intervention results to the environment
  list2env(rec_int_results, envir = environment())

  if (rec_int_results$shrinking_method_used) {
    include_confidence_set <- FALSE
  }
  # calculate the confidence set
  if (include_confidence_set) {
    cli::cli_alert_info("Calculating the confidence set...")
    cs_results <- confidence_set_processor(
      data = data,
      confidence_set_grid_step_size = confidence_set_grid_step_size,
      step_size_results = step_size_results,
      include_center_effects = include_center_effects,
      include_time_effects = include_time_effects,
      intervention_components = intervention_components,
      additional_covariates = additional_covariates,
      center_characteristics = center_characteristics,
      center_weights_for_outcome_goal = center_weights_for_outcome_goal,
      include_interaction_terms = include_interaction_terms,
      main_components = main_components,
      outcome_name = outcome_name,
      model = model,
      family_object = family_object,
      outcome_goal = outcome_goal,
      outcome_type = outcome_type,
      intervention_lower_bounds = intervention_lower_bounds,
      intervention_upper_bounds = intervention_upper_bounds,
      center_characteristics_optimization_values =
        center_characteristics_optimization_values,
      confidence_set_alpha = confidence_set_alpha,
      cost_list_of_vectors = cost_list_of_vectors,
      rec_int = rec_int
    )

    # unpack the confidence set results to the environment
    list2env(cs_results, envir = environment())
    Sys.sleep(0.25)
    cli::cli_alert_success("Done")
  }

  # carry out the overall intervention test
  if ("group" %in% names(data)) {
    test_results <- test_processor(
      data = data,
      outcome_type = outcome_type,
      outcome_name = outcome_name
    )
  } else {
    test_results <- NULL
  }

  cli::cli_alert("{symbol$heart} LAGO optimization complete {symbol$heart}")
  Sys.sleep(0.5)
  cli::cli_alert_info("Printing the output...")

  Sys.sleep(0.25)
  # print the output, including some user inputs
  print_output(
    data = data,
    outcome_name = outcome_name,
    outcome_type = outcome_type,
    intervention_components = intervention_components,
    include_interaction_terms = include_interaction_terms,
    main_components = main_components,
    center_characteristics = center_characteristics,
    family_object = family_object,
    include_center_effects = include_center_effects,
    include_time_effects = include_time_effects,
    outcome_goal = outcome_goal,
    cost_list_of_vectors = cost_list_of_vectors,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    model = model,
    rec_int = rec_int,
    rec_int_cost = rec_int_cost,
    est_outcome_goal = est_outcome_goal,
    include_confidence_set = include_confidence_set,
    cs = if (include_confidence_set) cs else NULL,
    test_results = test_results
  )

  return(
    if (!include_confidence_set) {
      list(
        rec_int = rec_int,
        rec_int_cost = rec_int_cost,
        est_outcome_goal = est_outcome_goal
      )
    } else {
      list(
        rec_int = rec_int,
        rec_int_cost = rec_int_cost,
        est_outcome_goal = est_outcome_goal,
        confidence_set_size_percentage = cs$confidence_set_size_percentage,
        cs = cs$cs[-1, ]
      )
    }
  )
}
