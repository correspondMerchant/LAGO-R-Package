#' calculate_recommended_interventions
#'
#' @description Calculates the LAGO recommended interventions based on an outcome
#' goal and/or a power goal.
#'
#'
#' ## required arguments:
#' @param data A data.frame. The input dataset containing the variables of interest.
#' @param outcome_name A character string. The name of the column in the dataset
#' that represents the outcome of interest.
#' @param outcome_type A character string. Specifies the type of the outcome.
#' Must be either "continuous" for continuous outcomes or "binary" for binary
#' outcomes.
#' @param intervention_components A character vector. The names of the columns in
#' the dataset that represent the intervention components.
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
#' ## optional arguments:
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
#' For example: c("characteristic1")
#' @param center_characteristics_optimization_values A numeric vector. The
#' values of the center characteristics that will be used for LAGO optimization.
#' For example: c(1.74)
#' @param outcome_goal A numeric value. Specifies the outcome goal, a desired
#' probability or mean value.
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
#' @return List(recommended interventions,
#' associated cost for the interventions,
#' estimated outcome mean/probability for the intervention group in the next stage,
#' 95% confidence set percentage,
#' 95% confidence set)
#'
#' @examples
#' # Basic case showing how to carry out the optimization with a built-in data set.
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
#'
calculate_recommended_interventions <- function(data,
                                                outcome_name,
                                                outcome_type,
                                                intervention_components,
                                                intervention_lower_bounds,
                                                intervention_upper_bounds,
                                                cost_list_of_vectors,
                                                outcome_goal, # later, make this optional once power goal is included
                                                glm_family = "default",
                                                link = "default",
                                                weights = NULL,
                                                center_characteristics = NULL,
                                                center_characteristics_optimization_values = NULL,
                                                optimization_method = "numerical",
                                                optimization_grid_search_step_size = NULL,
                                                include_confidence_set = TRUE,
                                                confidence_set_grid_step_size = NULL,
                                                confidence_set_alpha = 0.05) {
  # check if the data frame is null
  if (is.null(data)) {
    stop("The data frame 'data' is NULL.")
  }
  # check if the data frame is a data frame type
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }
  # check if the data frame is empty
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("The data frame 'data' is empty.")
  }
  # check if the data frame has all the required columns
  # for now, we are not requiring any columns.
  # TODO: for adding a power goal or adding fixed center effects in model fitting,
  # we would need to specify a few required columns: groups, center.
  required_columns <- c()
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(sprintf(
      "The following required columns are missing: %s",
      paste(missing_columns, collapse = ", ")
    ))
  }

  # check if the outcome name is a character type
  if (!is.character(outcome_name)) {
    stop("The outcome name is not a character type.")
  }
  # check if the outcome name is empty
  if (nchar(outcome_name) == 0) {
    stop("The outcome name is empty.")
  }
  # check if the outcome name is one of the columns in the data frame
  if (!(outcome_name %in% names(data))) {
    stop("The outcome name is not present in the provided data frame.")
  }

  # check if the outcome type is a character type
  if (!is.character(outcome_type)) {
    stop("The outcome type is not a character type.")
  }
  # check if the outcome type is either continuous or binary
  allowed_outcome_types <- c("continuous", "binary")
  if (!(outcome_type %in% allowed_outcome_types)) {
    stop(paste("Outcome type", outcome_type, "is not 'continuous' or 'binary'."))
  }

  # check if the glm family is a character type
  if (!is.character(glm_family)) {
    stop("The glm family is not a character type.")
  }
  # assign default glm_family based on the outcome type
  if (glm_family == "default") {
    if (outcome_type == "continuous") {
      glm_family <- "gaussian"
    } else if (outcome_type == "binary") {
      glm_family <- "binomial"
    }
  }

  # check if the link option is a character type
  if (!is.character(link)) {
    stop("The link option is not a character type.")
  }
  # check if the provided link option is supported
  supported_link_options <- c("logit", "probit", "identity", "log", "default")
  if (!(link %in% supported_link_options)) {
    stop(paste("link=", link, ".", "The link option has to be one of the following: logit, probit, identity, and log."))
  }
  # assign link based on the glm_family
  if (link == "default") {
    if (glm_family == "binomial") {
      link <- "logit"
    } else if (glm_family == "gaussian") {
      link <- "identity"
    }
  }

  # check if the provided weights option is numeric
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      stop("The weights option is not numeric.")
    }
    # check if the provided weights option has the same length as the number of
    # observations
    if (length(weights) != length(data[, intervention_components[1]])) {
      stop("The length of the weights is not the same as the number of observations in the provided data frame.")
    }
  }

  # check if intervention_components is a character vector type
  if (!(is.vector(intervention_components) && is.character(intervention_components))) {
    stop("Interventions list must be a character vector.")
  }
  # check if intervention_components are all columns in the data frame
  if (!all(intervention_components %in% names(data))) {
    stop("All elements in intervention_components must be columns in the data frame.")
  }

  # check if center_characteristics is a character vector type
  if (!is.null(center_characteristics)) {
    if (!(is.vector(center_characteristics) && is.character(center_characteristics))) {
      stop("center_characteristics must be a character vector.")
    }
    # check if intervention_components are all columns in the data frame
    if (!all(center_characteristics %in% names(data))) {
      stop("All elements in center_characteristics must be columns in the data frame.")
    }
    # check if center_characteristics_optimization_values is a numeric vector type
    if (!(is.vector(center_characteristics_optimization_values) && is.numeric(center_characteristics_optimization_values))) {
      stop("center_characteristics_optimization_values must be a numeric vector.")
    }
    # check if center_characteristics_optimization_values is not null
    if (is.null(center_characteristics_optimization_values)) {
      stop("center_characteristics_optimization_values is NULL. You decided to
           include center characteristics in the model, please either provide
           values of the center characteristics for LAGO optimization, or consider
           dropping the center characteristics.")
    }
    # check if length of center_characteristics_optimization_values is the same
    # as the length of center_characteristics
    if (length(center_characteristics) != length(center_characteristics_optimization_values)) {
      stop("The length of center_characteristics does not equal to the length of
           center_characteristics_optimization_values.")
    }
  }

  # check if intervention_lower_bounds and intervention_upper_bounds are both
  # numerical vectors
  if (!(is.vector(intervention_lower_bounds) && is.numeric(intervention_lower_bounds))) {
    stop("intervention_lower_bounds is not a numeric vector.")
  }
  if (!(is.vector(intervention_upper_bounds) && is.numeric(intervention_upper_bounds))) {
    stop("intervention_upper_bounds is not a numeric vector.")
  }

  # check if lower bounds list and upper bounds list have the same length,
  # and if the lower bounds are <= upper bounds respectively
  if (length(intervention_lower_bounds) != length(intervention_upper_bounds)) {
    stop("The lengths of lower and upper bounds do not match.")
  }
  if (any(intervention_lower_bounds < 0)) {
    stop("The intervention must have non-negative values only.")
  }
  invalid_indices <- which(intervention_upper_bounds < intervention_lower_bounds)
  if (length(invalid_indices) > 0) {
    stop(paste(
      "Invalid bounds at position(s):",
      paste(invalid_indices, collapse = ", "),
      "\nUpper bounds for the interventions must be greater than or equal to the lower bounds."
    ))
  }

  # check if cost_list_of_vectors is a list, and each sublist is a numeric vector
  if (!is.list(cost_list_of_vectors)) {
    stop("cost_list_of_vectors must be a list.")
  }
  # Check if all elements of sublists in cost_list_of_vectors are numeric
  all_numeric <- all(sapply(cost_list_of_vectors, function(sublist) {
    all(sapply(sublist, is.numeric))
  }))
  if (!all_numeric) {
    stop("All elements in the sublists of cost_list_of_vectors must be numeric.")
  }
  # check if the dimension of cost_list_of_vectors matches the dimension of intervention_components
  if (length(cost_list_of_vectors) != length(intervention_components)) {
    stop("The lengths of cost_list_of_vectors and intervention_components do not match.")
  }

  # check if the outcome goal optimization method is a character type
  if (!is.character(optimization_method)) {
    stop("The optimization_method is not a character type.")
  }
  # check if the outcome goal optimization method is one of the defined methods
  optimization_method_list <- c("numerical", "grid_search")
  if (!(optimization_method %in% optimization_method_list)) {
    stop(paste("optimization_method is not one of the supported methods. The supported methods are", optimization_method_list, "."))
  }

  # when the optimization_method is grid_search, check the number of intervention components
  if (optimization_method == "grid_search") {
    if (length(intervention_components) > 3) {
      warning(paste0(
        "There are more than 3 intervention components, and the grid search ",
        "algorithm may take significant amount of time to run. Please consider ",
        "adjusting the step size, or switch to the numerical optimization ",
        "method."
      ))
    }
  }

  # if optimization_grid_search_step_size is provided, it needs to be a numeric vector
  if (!is.null(optimization_grid_search_step_size)) {
    if (!(is.vector(optimization_grid_search_step_size) && is.numeric(optimization_grid_search_step_size))) {
      stop("optimization_grid_search_step_size is not a numeric vector.")
    }
    # if optimization_grid_search_step_size is provided, it needs to have the same length as the
    # number of intervention components
    if (length(optimization_grid_search_step_size) != length(intervention_components)) {
      stop("The number of step sizes provided for the grid search algorithm does not equal to the number of intervention components. Please check the length of optimization_grid_search_step_size.")
    }
  }

  # check if the outcome goal is a numeric number
  if (!is.numeric(outcome_goal)) {
    stop("The outcome goal is not numeric.")
  }
  # check if the outcome goal >= outcome that we already observed
  if (outcome_goal <= mean(data[[outcome_name]])) {
    stop("The specified outcome goal is below the observed mean of the intervention group. Please increase the goal.")
  }

  # check whether the include_confidence_set indicator is boolean
  if (!is.logical(include_confidence_set)) {
    stop("The include_confidence_set indicator is not a boolean.")
  }

  # check if confidence set step size is provided
  if (include_confidence_set && !is.null(confidence_set_grid_step_size)) {
    # if confidence_set_grid_step_size is provided, it needs to be a numeric vector
    if (!(is.vector(confidence_set_grid_step_size) && is.numeric(confidence_set_grid_step_size))) {
      stop("confidence_set_grid_step_size is not a numeric vector.")
    }
    # if confidence_set_grid_step_size is provided, it needs to have the same length as the
    # number of intervention components
    if (length(confidence_set_grid_step_size) != length(intervention_components)) {
      stop("The number of step sizes provided for the grid search algorithm in the confidence set calculation does not equal to the number of intervention components. Please check the length of confidence_set_grid_step_size.")
    }
  }

  # check if confidence_set_alpha is a numeric type
  if (!is.numeric(confidence_set_alpha)) {
    stop("confidence_set_alpha is not numeric.")
  }
  # check if confidence_set_alpha is between 0 and 1
  if (confidence_set_alpha <= 0 || confidence_set_alpha >= 1) {
    stop("confidence_set_alpha must be between 0 and 1.")
  }

  # check if the specified family matches the outcome type
  valid_families <- list(
    "binary" = c("binomial"),
    "continuous" = c("gaussian", "quasibinomial")
  )
  if (!glm_family %in% valid_families[[outcome_type]]) {
    stop(paste("The specified family '", glm_family,
      "' is not valid for the outcome type '", outcome_type, "'.",
      " Please select a compatible family.",
      sep = ""
    ))
  }

  # Convert glm family strings to glm family objects
  family_object <- switch(glm_family,
    "binomial" = binomial(link = link),
    "gaussian" = gaussian(link = link),
    "quasibinomial" = quasibinomial(link = link),
  )

  # fit the outcome model
  # TODO: add the option to fit the outcome model with fixed center effects,
  # and/or fixed time effects, and/or interaction terms.
  if (is.null(center_characteristics)) {
    formula <- as.formula(paste(outcome_name, "~", paste(intervention_components, collapse = " + ")))
    model <- glm(formula, data = data, family = family_object, weights = weights)
  } else {
    formula_with_center_characteristics <- as.formula(paste(outcome_name, "~", paste(intervention_components, collapse = " + "), " + ", paste(center_characteristics, collapse = " + ")))
    model <- glm(formula_with_center_characteristics, data = data, family = family_object, weights = weights)
  }

  # if model did not converge, stop the function
  if (!model$converged) {
    stop("Model did not converge. Please check your data and model specifications.")
  }

  # get coefficients for the intervention components
  int_coeff <- model$coefficients[c("(Intercept)", intervention_components)]

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

  # calculate default step size
  # 1/20th of the range for each intervention component
  step_size_results <- numeric(length(intervention_components))
  for (i in seq_along(intervention_components)) {
    current_intervention <- intervention_components[i]
    current_range <- range(data[[current_intervention]], na.rm = TRUE)
    step_size_results[i] <- (current_range[2] - current_range[1]) * (1 / 20)
  }

  # assign grid search step size
  if (optimization_method == "grid_search" && is.null(optimization_grid_search_step_size)) {
    optimization_grid_search_step_size <- step_size_results
  }

  message("Calculating LAGO recommended interventions...")
  # calculate recommended interventions
  rec_int_results <- get_recommended_interventions(
    beta_vector = int_coeff,
    cost_list_of_vectors = cost_list_of_vectors,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    outcome_goal = outcome_goal,
    optimization_method = optimization_method,
    optimization_grid_search_step_size = optimization_grid_search_step_size,
    center_cha_coeff_vec = center_cha_coeff_vec,
    center_characteristics_optimization_values = center_characteristics_optimization_values
  )
  message("Done")

  rec_int <- rec_int_results$est_rec_int
  rec_int_cost <- rec_int_results$rec_int_cost
  est_outcome_goal <- rec_int_results$est_reachable_outcome


  # assign confidence set step size
  if (include_confidence_set && is.null(confidence_set_grid_step_size)) {
    confidence_set_grid_step_size <- step_size_results
  }

  # calculate the confidence set for the recommended interventions
  if (include_confidence_set) {
    cat("If the confidence set calculation takes a long time to run, please consider increasing the confidence set step size. \n")
    message("Calculating the confidence set...")
    if (!is.null(center_characteristics)) {
      predictors_list <- c(intervention_components, center_characteristics)
    } else {
      predictors_list <- intervention_components
    }

    cs <- get_confidence_set(
      predictors_data = data[, predictors_list],
      outcome_data = data[, outcome_name],
      fitted_model = model,
      outcome_goal = outcome_goal,
      outcome_type = outcome_type,
      intervention_lower_bounds = intervention_lower_bounds,
      intervention_upper_bounds = intervention_upper_bounds,
      confidence_set_grid_step_size = confidence_set_grid_step_size,
      center_characteristics = center_characteristics,
      center_characteristics_optimization_values = center_characteristics_optimization_values,
      confidence_set_alpha = confidence_set_alpha
    )
    message("Done")
  }

  # input errors are very common, we show some of the key user inputs so
  # users can double check their inputs
  cat("\n==================================")
  cat("\n============  Inputs  ============\n")
  cat("==================================\n")
  cat(
    "Input data dimensions:", dim(data)[1], "rows, and",
    dim(data)[2], "columns \n"
  )
  cat("Outcome name:", outcome_name, "\n")
  cat("Outcome type:", outcome_type, "\n")
  cat(
    length(intervention_components),
    "intervention package component(s):", intervention_components, "\n"
  )
  if (!is.null(center_characteristics)) {
    cat(
      length(center_characteristics),
      "center characteristic(s):", center_characteristics, "\n"
    )
  }
  cat("The outcome model: \n")
  cat("\t family:", family_object$family, "\n")
  cat("\t link:", family_object$link, "\n")
  cat("Outcome goal:", outcome_goal, "\n")
  cat("List of intervention component costs:", toString(cost_list_of_vectors), "\n")

  cat("\n===================================================")
  cat("\n===========  Recommended Interventions  ===========\n")
  cat("===================================================\n")
  rec_int_df <- data.frame(
    component = intervention_components,
    value = rec_int
  )
  print(rec_int_df, row.names = FALSE)
  cat("\nCost for using the recommended interventions:", rec_int_cost, "\n")
  cat(
    "Estimated outcome goal using the recommended interventions:",
    est_outcome_goal, "\n"
  )

  if (include_confidence_set) {
    cat("\n========================================")
    cat("\n============ Confidence Set ============\n")
    cat("========================================\n")
    cat("Confidence set size percentage:", cs$confidence_set_size_percentage, "\n")
    cat("Confidence set: \n")
    print(cs$cs)
  }

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
