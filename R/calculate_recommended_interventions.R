#' calculate_recommended_interventions
#'
#' @description Calculates the LAGO recommended interventions based on an
#' outcome goal and/or a power goal.
#'
#' ### required arguments:
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
#'
#' ### optional arguments:
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
#' @param outcome_goal A numeric value. Specifies the outcome goal, a desired
#' probability or mean value.
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
#' calculate_recommended_interventions(
#'   data = infert,
#'   outcome_name = "case",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   intervention_components = c("age", "parity"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(50, 10),
#'   cost_list_of_vectors = list(c(0, 4), c(0, 1)),
#'   outcome_goal = 0.5,
#'   confidence_set_grid_step_size = c(1, 1)
#' )
#'
#' calculate_recommended_interventions(
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
#'   confidence_set_grid_step_size = c(1, 1)
#' )
#'
#' calculate_recommended_interventions(
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
#'   confidence_set_grid_step_size = c(1, 1)
#' )
#'
#' @export
#' @importFrom utils head
#'
calculate_recommended_interventions <- function(
    data,
    input_data_structure = "individual_level",
    outcome_name,
    outcome_type,
    intervention_components,
    intervention_lower_bounds,
    intervention_upper_bounds,
    cost_list_of_vectors,
    outcome_goal,
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
    include_time_effects = FALSE,
    include_interaction_terms = FALSE) {
  # validate and preapre inputs
  inputs <- do.call(
    validate_inputs,
    as.list(environment())
  )

  # unpack the inputs to the environment
  list2env(inputs, envir = environment())

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

  # unpack the outcome model to the environment
  list2env(outcome_model, envir = environment())

  # calculate the recommended interventions
  rec_int_results <- rec_int_processor(
    data = data,
    model = model,
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
    outcome_goal = outcome_goal,
    center_characteristics_optimization_values =
      center_characteristics_optimization_values,
    time_effect_optimization_value = time_effect_optimization_value
  )

  # unpack the recommended intervention results to the environment
  list2env(rec_int_results, envir = environment())

  # calculate the confidence set
  if (include_confidence_set) {
    cs_results <- confidence_set_processor(
      data = data,
      include_confidence_set = include_confidence_set,
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
      cost_list_of_vectors = cost_list_of_vectors
    )

    # unpack the confidence set results to the environment
    list2env(cs_results, envir = environment())
  }

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
    outcome_goal = outcome_goal,
    cost_list_of_vectors = cost_list_of_vectors,
    model = model,
    rec_int = rec_int,
    rec_int_cost = rec_int_cost,
    est_outcome_goal = est_outcome_goal,
    include_confidence_set = include_confidence_set,
    cs = cs
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
        cs = cs$cs
      )
    }
  )
}
