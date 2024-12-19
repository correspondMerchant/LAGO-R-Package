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
  # check if the input data is null
  if (is.null(data)) {
    stop("The argument 'data' is NULL.")
  }
  # check if the input data is a data frame type
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }
  # check if the input data is empty
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("The argument 'data' is empty.")
  }

  # check if the input data structure is a character type
  if (!is.character(input_data_structure)) {
    stop("The input_data_structure must be a character string.")
  }
  # check if the input data structure is supported
  if (!(input_data_structure %in% c("individual_level", "center_level"))) {
    stop(paste(
      "\ninput_data_structure:", input_data_structure,
      "\ninput_data_structure must be either 'individual_level'",
      "or 'center_level'."
    ))
  }

  # check if the outcome name is a character type
  if (!is.character(outcome_name)) {
    stop("The outcome name must be a character string.")
  }
  # check if the outcome name is empty
  if (nchar(outcome_name) == 0) {
    stop("The outcome name is empty.")
  }
  # check if the outcome name is one of the columns in the data frame
  if (!(outcome_name %in% names(data))) {
    stop("The outcome name must be presented in the provided input data.")
  }

  # check if the outcome type is a character type
  if (!is.character(outcome_type)) {
    stop("The outcome type must be a character string.")
  }
  # check if the outcome type is either continuous or binary
  allowed_outcome_types <- c("continuous", "binary")
  if (!(outcome_type %in% allowed_outcome_types)) {
    stop(paste(
      "Outcome type", outcome_type,
      "must be either 'continuous' or 'binary'."
    ))
  }
  # check if the input data structure is center_level when
  # the outcome type is continuous.
  if (input_data_structure == "center_level") {
    if (outcome_type == "continuous") {
      stop(paste(
        "For continuous outcomes, LAGO requires individual",
        "level data for the optimization step."
      ))
    }
  }

  # check if the input data has all the required columns
  required_columns <- c()
  if (input_data_structure == "center_level") {
    required_columns <- c(
      required_columns,
      "proportion",
      "center_sample_size"
    )
  }

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(sprintf(
      "The following required columns are missing: %s",
      paste(missing_columns, collapse = ", ")
    ))
  }

  # check if the glm family is a character type
  if (!is.character(glm_family)) {
    stop("The glm family must be a character string.")
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
    stop("The link option must be a character string.")
  }
  # check if the provided link option is supported
  supported_link_options <- c("logit", "probit", "identity", "log", "default")
  if (!(link %in% supported_link_options)) {
    stop(paste(
      "link=", link, ".",
      paste0(
        "The link option has to be one of ",
        "the following: logit, probit, identity, and log."
      )
    ))
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
      stop("The 'weights' option must be a numeric vector.")
    }
    # check if the provided weights option has the same length as the number of
    # observations
    if (length(weights) != length(data[, intervention_components[1]])) {
      stop(paste0(
        "The length of the weights must be the same as the ",
        "number of observations in the provided input data."
      ))
    }
  }

  # check if intervention_components is a character vector type
  if (!(is.vector(intervention_components) &&
    is.character(intervention_components))) {
    stop("Interventions list must be a character vector.")
  }
  # check if intervention_components are all columns in the data frame
  if (!all(intervention_components %in% names(data))) {
    stop(paste0(
      "All elements in intervention_components ",
      "must be columns in the data frame."
    ))
  }

  # check if center_characteristics is a character vector type
  if (length(center_characteristics) > 0) {
    if (!(is.vector(center_characteristics) &&
      is.character(center_characteristics))) {
      stop("center_characteristics must be a character vector.")
    }
    # check if center_characteristics are all columns in the data frame
    if (!all(center_characteristics %in% names(data))) {
      stop(paste0(
        "All elements in center_characteristics ",
        "must be columns in the data frame."
      ))
    }
    # check if center_characteristics_optimization_values
    # is a numeric vector type
    if (!(is.vector(center_characteristics_optimization_values) &&
      is.numeric(center_characteristics_optimization_values))) {
      stop(paste0(
        "center_characteristics_optimization_values ",
        "must be a numeric vector."
      ))
    }
    # check if center_characteristics_optimization_values is not null
    if (is.null(center_characteristics_optimization_values)) {
      stop(paste0(
        "center_characteristics_optimization_values is NULL. ",
        "You decided to include center characteristics in ",
        "the model, please either provide values of the ",
        "center characteristics for LAGO optimization, ",
        "or consider dropping the center characteristics."
      ))
    }
    # check if length of center_characteristics_optimization_values is the same
    # as the length of center_characteristics
    if (length(center_characteristics) !=
      length(center_characteristics_optimization_values)) {
      stop(paste(
        "The length of center_characteristics must be the",
        "same as the length of",
        "center_characteristics_optimization_values."
      ))
    }
  }

  # check whether the include_center_effects indicator is boolean
  if (!is.logical(include_center_effects)) {
    stop("The include_center_effects indicator must be a boolean.")
  }
  # if include_center_effects is not specified, set the default value
  if (input_data_structure == "center_level") {
    include_center_effects <- TRUE
  } else {
    include_center_effects <- FALSE
  }

  # check whether center_characteristics and center effects are both included
  if (include_center_effects && !is.null(center_characteristics)) {
    stop(paste(
      "Fixed center effects and center characteristics cannot be",
      "both included in the outcome model. Please",
      "pick one to use."
    ))
  }

  # check if the input data has the "center" column if
  # include_center_effects is TRUE
  if (include_center_effects) {
    if (!"center" %in% names(data)) {
      stop(paste(
        "To include fixed center effects in the outcome model",
        "please make sure that the input data has a 'center'",
        "column to identify facilities."
      ))
    }
    # if "center" column of the input data is not a factor,
    # change it to factor and print a warning message
    if (!is.factor(data$center)) {
      data$center <- as.factor(data$center)
      message(paste(
        "'center' column is not a factor type. To ensure",
        "the correct model fit, it has been converted",
        "to the factor type."
      ))
    }
  }

  # preliminary check for center_weights_for_outcome_goal
  if (!is.null(center_weights_for_outcome_goal)) {
    # check if include center effects is set to FALSE
    if (!include_center_effects) {
      stop(paste(
        "'include_center_effects' must be set to TRUE,",
        "for values of 'center_weights_for_outcome_goal'",
        "to be used."
      ))
    }
    # check if center_weights_for_outcome_goal is a numeric vector
    if (!is.numeric(center_weights_for_outcome_goal)) {
      stop(paste("center_weights_for_outcome_goal must be a numeric vector."))
    }
    # check if center_weights_for_outcome_goal has the same length as the
    # number of facilities provided in the input data
    n_facilities <- length(unique(data$center))
    if (length(center_weights_for_outcome_goal) != n_facilities) {
      stop(paste(
        "The length of center_weights_for_outcome_goal must match",
        "the number of unique facilities in the input data."
      ))
    }
  } else {
    # if center_weights_for_outcome_goal is not specified, but
    # include_center_effects is set to TRUE, we calculate its default values
    if (include_center_effects) {
      if (input_data_structure == "center_level") {
        center_sizes <- tapply(
          data$center_sample_size,
          data$center,
          function(x) x[1]
        )
        total_sample_size <- sum(center_sizes)
      } else if (input_data_structure == "individual_level") {
        total_sample_size <- sum(as.numeric(table(data$center)))
        center_sizes <- as.numeric(table(data$center))
      }
      center_weights_for_outcome_goal <- center_sizes / total_sample_size
    } else {
      center_weights_for_outcome_goal <- 1
    }
  }

  # check if values of center_weights_for_outcome_goal sum up to 1
  if (!is.null(center_weights_for_outcome_goal)) {
    if (abs(sum(center_weights_for_outcome_goal - 1)) >= 0.001) {
      stop(paste(
        "values in center_weights_for_outcome_goal must",
        "sum up to 1."
      ))
    }
  }

  # check if include_time_effects is logical
  if (!is.logical(include_time_effects)) {
    stop(paste("'include_time_effects' must be a boolean."))
  }

  # if include_time_effects is set to TRUE
  if (include_time_effects) {
    # the input data must have a 'period' column
    if (!"period" %in% names(data)) {
      stop(paste(
        "To include fixed time effects in the outcome model,",
        "please make sure that the input data has a 'period'",
        "column to identify time period."
      ))
    }
    # the time_effect_optimization_value must be provided, one numeric number
    if (is.null(time_effect_optimization_value) ||
      !is.numeric(time_effect_optimization_value) ||
      length(time_effect_optimization_value) != 1) {
      stop(paste(
        "'include_time_effects' is set to TRUE,",
        "'time_effect_optimization_value' must be provided,",
        "and it must be a numeric value."
      ))
    }
    # if "period" column of the input data is not a factor,
    # change it to factor and print a warning message
    if (!is.factor(data$period)) {
      data$period <- as.factor(data$period)
      message(paste(
        "'period' column is not a factor type. To ensure",
        "the correct model fit, it has been converted",
        "to the factor type."
      ))
    }
  }

  # if include_interaction_terms is set to TRUE
  if (include_interaction_terms) {
    # check if interaction terms are included in intervention_components
    # and follow the correct naming scheme
    if (!any(grepl(":", intervention_components, fixed = TRUE))) {
      stop(paste(
        "Please make sure the interaction terms are included",
        "as part of the 'intervention_components', and the interaciton terms",
        "need to have the format of 'X1:X5' "
      ))
    }

    # if so, check if main_components is null
    if (is.null(main_components)) {
      stop(paste(
        "'main_components' must be defined as a character vector",
        "if interaction terms are",
        "included in 'intervention_components'."
      ))
    }
    # check if main_components is a character vector
    if (!is.character(main_components)) {
      stop(paste("'main_components' must be a character vector."))
    }

    # if not null, check if main_components have the correct naming scheme
    if (any(grepl(":", main_components, fixed = TRUE))) {
      stop(paste(
        "Interaction terms should be included as part of the",
        "'intervention_components', not part of the",
        "'main_components'."
      ))
    }

    # check if main_components are all columns in the data frame
    # need this to calculate the step size if the step size is not specified.
    if (optimization_method == "grid_search" &&
      is.null(optimization_grid_search_step_size)) {
      if (!all(main_components %in% names(data))) {
        stop(paste(
          "When 'optimization_method' is set to 'grid_search',",
          "and optimization_grid_search_step_size is not specified,",
          "all elements in the main_components",
          "must be columns in the data frame for auto calculating",
          "the step size."
        ))
      }
    }

    # check if interaction terms can be found in the main_components
    check_term <- function(term) {
      if (grepl(":", term, fixed = TRUE)) {
        parts <- unlist(strsplit(term, ":", fixed = TRUE))
        return(all(parts %in% main_components))
      } else {
        return(term %in% main_components)
      }
    }
    interaction_check_results <- sapply(intervention_components, check_term)
    if (!all(interaction_check_results)) {
      stop(paste(
        "Each component of 'intervention_components' including",
        "interaction terms (separated by ':')",
        "must be present in 'main_components'."
      ))
    }

    # add backticks to interactions terms in intervention components
    # for glm model fitting
    needs_backticks <- grepl(":", intervention_components)
    intervention_components[needs_backticks] <- paste0(
      "`", intervention_components[needs_backticks], "`"
    )
  }

  # check if additional_covariates is a character vector
  if (length(additional_covariates) > 0) {
    if (!(is.vector(additional_covariates) &&
      is.character(additional_covariates))) {
      stop("additional_covariates must be a character vector.")
    }
    # check if additional_covariates are all columns in the data frame
    if (!all(additional_covariates %in% names(data))) {
      stop(paste0(
        "All elements in additional_covariates ",
        "must be columns in the data frame."
      ))
    }
  }

  # check if intervention_lower_bounds and intervention_upper_bounds are both
  # numerical vectors
  if (!(is.vector(intervention_lower_bounds) &&
    is.numeric(intervention_lower_bounds))) {
    stop("intervention_lower_bounds must be a numeric vector.")
  }
  if (!(is.vector(intervention_upper_bounds) &&
    is.numeric(intervention_upper_bounds))) {
    stop("intervention_upper_bounds must be a numeric vector.")
  }

  # check if the dimension of intervention_lower_bounds matches
  # the dimension of intervention_components or the dimension
  # of main_components
  if (
    (!include_interaction_terms &&
      (length(intervention_lower_bounds) != length(intervention_components))) ||
      (include_interaction_terms &&
        (length(intervention_lower_bounds) != length(main_components)))
  ) {
    stop(paste(
      "Without interaction terms,",
      "the lengths of 'intervention_lower_bounds' and",
      "'intervention_components' must be the same.",
      "With interaction terms,",
      "the lengths of 'intervention_lower_bounds' and",
      "'main_components' must be the same."
    ))
  }

  # check if lower bounds list and upper bounds list have the same length,
  # and if the lower bounds are <= upper bounds respectively
  if (length(intervention_lower_bounds) != length(intervention_upper_bounds)) {
    stop("The lengths of lower and upper bounds must be the same.")
  }
  if (any(intervention_lower_bounds < 0)) {
    stop("The intervention must have non-negative values only.")
  }
  invalid_indices <- which(intervention_upper_bounds <
    intervention_lower_bounds)
  if (length(invalid_indices) > 0) {
    stop(paste(
      "Invalid bounds at position(s):",
      paste(invalid_indices, collapse = ", "),
      paste0(
        "\nUpper bounds for the interventions must be greater ",
        "than or equal to the lower bounds."
      )
    ))
  }

  # check if cost_list_of_vectors is a list,
  # and each sublist is a numeric vector
  if (!is.list(cost_list_of_vectors)) {
    stop("cost_list_of_vectors must be a list.")
  }
  # Check if all elements of sublists in cost_list_of_vectors are numeric
  all_numeric <- all(sapply(cost_list_of_vectors, function(sublist) {
    all(sapply(sublist, is.numeric))
  }))
  if (!all_numeric) {
    stop(paste(
      "All elements in the sublists of cost_list_of_vectors",
      "must be numeric."
    ))
  }
  # check if the dimension of cost_list_of_vectors matches
  # the dimension of intervention_components or the dimension
  # of main_components
  if (
    (!include_interaction_terms &&
      (length(cost_list_of_vectors) != length(intervention_components))) ||
      (include_interaction_terms &&
        (length(cost_list_of_vectors) != length(main_components)))
  ) {
    stop(paste(
      "Without interaction terms,",
      "the lengths of 'cost_list_of_vectors' and",
      "'intervention_components' must be the same.",
      "With interaction terms,",
      "the lengths of 'cost_list_of_vectors' and",
      "'main_components' must be the same."
    ))
  }

  # check if the outcome goal optimization method is a character type
  if (!is.character(optimization_method)) {
    stop("The optimization_method must be a character string.")
  }
  # check if the outcome goal optimization method is one of the defined methods
  optimization_method_list <- c("numerical", "grid_search")
  if (!(optimization_method %in% optimization_method_list)) {
    stop(paste(
      "optimization_method must be one of the supported methods.",
      "The supported methods are:", optimization_method_list, "."
    ))
  }

  # when the optimization_method is grid_search,
  # check the number of intervention components
  if (optimization_method == "grid_search") {
    if (length(intervention_components) > 3) {
      message(paste0(
        "There are more than 3 intervention components, and the grid search ",
        "algorithm may take significant amount of time to run. ",
        "Please consider adjusting the step size, or switch to the ",
        "numerical optimization method."
      ))
    }
  }

  # if optimization_grid_search_step_size is provided,
  # it needs to be a numeric vector
  if (!is.null(optimization_grid_search_step_size)) {
    if (!(is.vector(optimization_grid_search_step_size) &&
      is.numeric(optimization_grid_search_step_size))) {
      stop("optimization_grid_search_step_size must be a numeric vector.")
    }
    # if optimization_grid_search_step_size is provided,
    # it needs to have the same length as the number of intervention components
    if ((!include_interaction_terms &&
      (length(optimization_grid_search_step_size) !=
        length(intervention_components))) ||
      (include_interaction_terms &&
        (length(optimization_grid_search_step_size) !=
          length(main_components)))
    ) {
      stop(paste(
        "Without interaction terms,",
        "The number of step sizes provided for the grid search",
        "algorithm must be the same as the number of intervention",
        "components. Please check the length of",
        "'optimization_grid_search_step_size'.",
        "With interaciton terms,",
        "The number of step sizes provided for the grid search",
        "algorithm must be the same as the length of 'main_components'."
      ))
    }
  }

  # check if the outcome goal is a numeric number
  if (!is.numeric(outcome_goal)) {
    stop("The outcome goal must be a numeric value.")
  }
  # check if the outcome goal >= outcome that we already observed
  if (outcome_goal <= mean(data[[outcome_name]])) {
    stop(paste(
      "The specified outcome goal is below the observed mean of the",
      "intervention group. Please increase the goal."
    ))
  }

  # check whether the include_confidence_set indicator is boolean
  if (!is.logical(include_confidence_set)) {
    stop("The include_confidence_set indicator must be a boolean.")
  }

  # check if confidence set step size is provided
  if (include_confidence_set && !is.null(confidence_set_grid_step_size)) {
    # if confidence_set_grid_step_size is provided,
    # it needs to be a numeric vector
    if (!(is.vector(confidence_set_grid_step_size) &&
      is.numeric(confidence_set_grid_step_size))) {
      stop("confidence_set_grid_step_size must be a numeric vector.")
    }
    # if confidence_set_grid_step_size is provided,
    # it needs to have the same length as the number of intervention components
    if ((!include_interaction_terms &&
      (length(confidence_set_grid_step_size) !=
        length(intervention_components))) ||
      (include_interaction_terms &&
        (length(confidence_set_grid_step_size) !=
          length(main_components)))
    ) {
      stop(paste(
        "Without interaction terms,",
        "the number of step sizes provided for the grid",
        "search algorithm in the confidence set calculation",
        "must be the same as the length of 'intervention_components'.",
        "Please check the length of 'confidence_set_grid_step_size'.",
        "With interaction terms,",
        "the number of step sizes provided for the grid",
        "search algorithm in the confidence set calculation",
        "must be the same as the length of 'main_components'."
      ))
    }
  }

  # check if confidence_set_alpha is a numeric type
  if (!is.numeric(confidence_set_alpha)) {
    stop("confidence_set_alpha must be a numeric value.")
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
  if (input_data_structure == "center_level") {
    outcome_name <- "proportion"
    weights <- data$center_sample_size
  }
  covariates <- c(
    if (include_center_effects) "center",
    if (include_time_effects) "period",
    intervention_components,
    if (!is.null(additional_covariates)) additional_covariates,
    if (!is.null(center_characteristics)) center_characteristics
  )
  formula <- as.formula(
    paste(outcome_name, "~", paste(covariates, collapse = " + "))
  )
  tryCatch(
    {
      model <- glm(
        formula,
        data = data,
        family = family_object,
        weights = weights
      )
    },
    error = function(e) {
      stop(paste("Error occurred during model fitting step:\n", e))
    }
  )

  # if model did not converge, stop the function
  if (!model$converged) {
    stop(paste(
      "Model did not converge. Please check",
      "your input data and model specifications."
    ))
  }

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
  } else {
    all_center_lvl_effects <- 0
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

  # should be deleted later, after cost for PULESA part is done
  print("intervention coefficients: ")
  print(intervention_components_coeff)
  print("center level effects:")
  print(all_center_lvl_effects)

  message("Calculating LAGO recommended interventions...")
  # calculate recommended interventions
  rec_int_results <- get_recommended_interventions(
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
      center_characteristics_optimization_values
  )
  message("Done")

  rec_int <- rec_int_results$est_rec_int
  rec_int_cost <- rec_int_results$rec_int_cost
  est_outcome_goal <- rec_int_results$est_reachable_outcome


  # assign confidence set step size if not specified
  if (include_confidence_set && is.null(confidence_set_grid_step_size)) {
    confidence_set_grid_step_size <- step_size_results
  }

  # calculate the confidence set for the recommended interventions
  if (include_confidence_set) {
    cat(paste(
      "If the confidence set calculation takes a long time to run,",
      "please consider increasing the confidence set step size. \n"
    ))
    message("Calculating the confidence set...")
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
      outcome_goal = outcome_goal,
      outcome_type = outcome_type,
      intervention_lower_bounds = intervention_lower_bounds,
      intervention_upper_bounds = intervention_upper_bounds,
      confidence_set_grid_step_size = confidence_set_grid_step_size,
      center_characteristics = center_characteristics,
      center_characteristics_optimization_values =
        center_characteristics_optimization_values,
      confidence_set_alpha = confidence_set_alpha,
      cluster_id = cluster_id
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
    "intervention package component(s): \n",
    paste("\t", intervention_components, collapse = "\n"),
    "\n"
  )
  if (include_interaction_terms) {
    cat(
      length(main_components),
      "main effect component(s): \n",
      paste("\t", main_components, collapse = "\n"),
      "\n"
    )
  }
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
  cat(
    "List of intervention component costs:",
    toString(cost_list_of_vectors), "\n"
  )

  cat("\n=====================================")
  cat("\n============  Model Fit  ============\n")
  cat("=====================================\n")
  print(summary(model))

  cat("\n===================================================")
  cat("\n===========  Recommended Interventions  ===========\n")
  cat("===================================================\n")
  rec_int_df <- data.frame(
    component = if (include_interaction_terms) {
      main_components
    } else {
      intervention_components
    },
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
    cat(
      "Confidence set size percentage:",
      cs$confidence_set_size_percentage, "\n"
    )

    if (cs$confidence_set_size_percentage > 0) {
      cat("Confidence set (only first few rows are shown): \n")
      cat("Please use $cs to get the full confidence set. \n")
      print(head(cs$cs))
    } else {
      cat("No confidense set was found. \n")
    }
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
