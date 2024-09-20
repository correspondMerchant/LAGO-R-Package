#' calculate_recommended_interventions
#'
#' @description Returns the LAGO recommended interventions for the next stage.
#' The recommended interventions that aim to satisfy the outcome goal and/or the
#' power goal.
#'
#' @param df data.frame, Input data
#' @param outcome_name character, Outcome name
#' @param outcome_type character, Outcome type, either "continuous" or "binary"
#' @param glm_family character, Family for the GLM model
#' For example: "binomial"
#' For v1.1, we are only considering binomial family, and quasibinomial(link="logit")
#' @param link character, the link option when glm_family is set to quasibinomial.
#' It has to be one of the following:
#' logit (default), probit, cauchit, log and cloglog.
#' @param include_intercept boolean, Whether the intercept should be included in the
#' fitted model.
#' @param interventions_list character vector, Names of the intervention components
#' For example: c("component1", "component2")
#' @param center_characteristic_list character vector, Names of the center characteristics
#' For example: c("characteristic1")
#' @param center_characteristic_list_for_optimization numeric vector, Given values of
#' the center characteristics
#' For example: c(1.74)
#' @param intervention_lower_bounds numeric vector, Lower bounds for the intervention components
#' For example: c(0,0)
#' @param intervention_upper_bounds numeric vector, Upper bounds for the intervention components
#' For example: c(10,20)
#' @param cost_list_of_lists list, A nested list structure defining cost functions for the intervention components.
#' Each sublist represents a component and contains coefficients for its cost function.
#' The position of each coefficient in the sublist corresponds to the power of x in the polynomial cost function.
#' For example:
#' list(list(1, 2), list(4), list(5)) represents:
#' - First component: cost = 1x + 2x^2
#' - Second component: cost = 4x
#' - Third component: cost = 5x
#' Empty sublists are not allowed. Each component must have at least one coefficient.
#' For v1, we are only considering linear cost functions, more details below.
#' @param outcome_goal numeric, The outcome goal
#' @param outcome_goal_optimization character, Method used behind the scenes for
#' calculating the recommended interventions. Either "numerical" or "grid_search"
#' For v1, this does not matter since we work with linear cost functions and
#' we only have one way of solving for the recommended interventions
#'
#' @return List(recommended interventions, associated cost for the interventions,
#' estimated outcome mean/probability for the intervention group in the next stage)
#'
#' @examples
#' calculate_recommended_interventions(
#'   df = infert,
#'   outcome_name = "case",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   interventions_list = c("age", "parity"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(50, 10),
#'   cost_list_of_lists = list(list(4), list(1)),
#'   outcome_goal = 0.5
#' )
#'
#' @export
#'
calculate_recommended_interventions <- function(df,
                                                outcome_name,
                                                outcome_type,
                                                glm_family,
                                                link = "logit",
                                                include_intercept = TRUE,
                                                interventions_list,
                                                center_characteristic_list = NULL,
                                                center_characteristic_list_for_optimization = NULL,
                                                intervention_lower_bounds,
                                                intervention_upper_bounds,
                                                cost_list_of_lists,
                                                outcome_goal,
                                                outcome_goal_optimization = "numerical") {
  # initial checks, very important
  # check if the data frame is not null
  if (is.null(df)) {
    stop("The data frame 'df' is NULL.")
  }
  # check if the data frame is a data frame type
  if (!is.data.frame(df)) {
    stop("The argument 'df' must be a data frame.")
  }
  # check if the data frame is empty
  if (nrow(df) == 0 || ncol(df) == 0) {
    stop("The data frame 'df' is empty.")
  }
  # check if the data frame has all the required columns
  # for v1, we are not requiring any columns.
  # TODO: specify the required_columns for later versions. We would need these
  # columns for power calculations, and/or adding fixed center effects, etc.
  # required_columns <- c("group", "center")
  required_columns <- c()
  missing_columns <- setdiff(required_columns, names(df))
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
  # check if outcome name is empty
  if (nchar(outcome_name) == 0) {
    stop("The outcome name is empty.")
  }
  # check if outcome name is one of the columns in the data frame
  if (!(outcome_name %in% names(df))) {
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
  # TODO: for later versions, check if the user provided glm_family is one of
  # the supported family names. For v1, we are only considering binomial family.

  # if glm_family is quasibinomial, check if the link option is a character type
  if (!is.character(link)) {
    stop("The link option for quasibinomial family is not a character type.")
  }
  # check if the provided link option is supported
  supported_link_options <- c("logit", "probit", "cauchit", "log", "cloglog")
  if (!(link %in% supported_link_options)) {
    stop("The link option for quasibinomial family has to be one of the following: logit, probit, cauchit, log, and cloglog.")
  }
  
  # check if interventions_list is a character vector type
  if (!(is.vector(interventions_list) && is.character(interventions_list))) {
    stop("Interventions list must be a character vector.")
  }
  # check if interventions_list are all columns in the data frame
  if (!all(interventions_list %in% names(df))) {
    stop("All elements in interventions_list must be columns in the data frame.")
  }

  # check if center_characteristic_list is a character vector type
  if (!is.null(center_characteristic_list)) {
    if (!(is.vector(center_characteristic_list) && is.character(center_characteristic_list))) {
      stop("center_characteristic_list must be a character vector.")
    }
    # check if interventions_list are all columns in the data frame
    if (!all(center_characteristic_list %in% names(df))) {
      stop("All elements in center_characteristic_list must be columns in the data frame.")
    }
    # check if center_characteristic_list_for_optimization is a numeric vector type
    if (!(is.vector(center_characteristic_list_for_optimization) && is.numeric(center_characteristic_list_for_optimization))) {
      stop("center_characteristic_list_for_optimization must be a numeric vector.")
    }
    # check if center_characteristic_list_for_optimization is not null
    if (is.null(center_characteristic_list_for_optimization)) {
      stop("center_characteristic_list_for_optimization is NULL. You decided to
           include center characteristics in the model, please either provide
           values of the center characteristics for LAGO optimization, or consider
           dropping the center characteristics.")
    }
    # check if length of center_characteristic_list_for_optimization is the same
    # as the length of center_characteristic_list
    if (length(center_characteristic_list) != length(center_characteristic_list_for_optimization)) {
      stop("The length of center_characteristic_list does not equal to the length of
           center_characteristic_list_for_optimization.")
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

  # check if cost_list_of_lists is a list, and each list within the list is a numeric vector
  if (!is.list(cost_list_of_lists)) {
    stop("cost_list_of_lists must be a list.")
  }
  # Check if all elements of cost_list_of_lists are lists
  if (!all(sapply(cost_list_of_lists, is.list))) {
    stop("All elements of the cost_list_of_lists must be lists.")
  }
  # Check if all sub-elements of cost_list_of_lists are numeric
  all_numeric <- all(sapply(cost_list_of_lists, function(sublist) {
    all(sapply(sublist, is.numeric))
  }))
  if (!all_numeric) {
    stop("All elements in the sublists of cost_list_of_lists must be numeric.")
  }
  # check if the dimension of cost_list_of_lists matches the dimension of interventions_list
  if (length(cost_list_of_lists) != length(interventions_list)) {
    stop("The lengths of cost_list_of_lists and interventions_list do not match.")
  }

  # check if the outcome goal optimization method is a character type
  if (!is.character(outcome_goal_optimization)) {
    stop("The outcome_goal_optimization is not a character type.")
  }
  # V1 does not consider the value of outcome_goal_optimization, because we are
  # only working with linear cost functions for now. For linear cost functions,
  # we have a very robust and fast way of solving for recommended interventions,
  # which maximizes the most cost-effective intervention component first, if the
  # outcome goal is not met, we max out the second most cost-effective intervention
  # component, and so on, until the outcome goal is achieved.
  #
  # See get_recommended_intervention_linear_cost.R for details.
  #
  # TODO: future versions will consider both 'numerical' and 'grid_search'
  # Previously, we only had grid search method for working with cubic cost
  # function. For PULESA analysis, Jingyu added the numerical solution. Check
  # the dropbox folder 'LAGO r package paper' for Jingyu's code.

  # check if the outcome goal is a numeric number
  if (!is.numeric(outcome_goal)) {
    stop("The outcome goal is not numeric.")
  }
  # check if the outcome goal >= outcome that we already observed
  if (outcome_goal <= mean(df[[outcome_name]])) {
    stop("The specified outcome goal is below the observed mean of the intervention group. Please increase the goal.")
  }



  # Convert glm family strings to actual glm family objects
  # need this step before fitting the models.
  # TODO: figure out which ones do we want to support?
  # for v1, we are only working with binomial family.
  if (glm_family != "quasibinomial") {
    family_object <- switch(glm_family,
                            "binomial" = binomial(),
                            "poisson" = poisson(),
                            "gaussian" = gaussian(),
                            "Gamma" = Gamma(),
                            "inverse.gaussian" = inverse.gaussian(),
                            "quasi" = quasi(),
                            "quasibinomial" = quasibinomial(),
                            "quasipoisson" = quasipoisson()
                            )
  } else {
    family_object <- switch(link,
                            "logit" = quasibinomial(link="logit"),
                            "probit" = quasibinomial(link="probit"),
                            "cauchit" = quasibinomial(link="cauchit"),
                            "log" = quasibinomial(link="log"),
                            "cloglog" = quasibinomial(link="cloglog")
                            )
  }

  # MINH----------------------------------------------------------------------------------
  valid_families <- list(
    "binary" = c("binomial", "quasibinomial"),
    "continuous" = c("gaussian", "Gamma", "inverse.gaussian", "quasi", "quasibinomial")
  )
  
  # check if the specified family matches the outcome type
  if (!glm_family %in% valid_families[[outcome_type]]) {
    stop(paste("The specified family '", glm_family, 
               "' is not valid for the outcome type '", outcome_type, "'.", 
               " Please select a compatible family.", sep = ""))
  }
  # MINH----------------------------------------------------------------------------------
  # fit the model
  # depending on the whether the user wants to include center characteristics
  # we fit the model differently.
  if (is.null(center_characteristic_list)) {
    formula <- as.formula(paste(outcome_name, "~", paste(interventions_list, collapse = " + ")))
    model <- glm(formula, data = df, family = family_object)
  } else {
    formula_with_center_characteristics <- as.formula(paste(outcome_name, "~", paste(interventions_list, collapse = " + "), " + ", paste(center_characteristic_list, collapse = " + ")))
    model <- glm(formula_with_center_characteristics, data = df, family = family_object)
  }

  # if model did not converge, no need to continue to calculate the recommended
  # interventions.
  if (!model$converged) {
    stop("Model did not converge. Please check your data and model specifications.")
  }

  # get coefficients for the intervention components
  if (include_intercept == TRUE) {
    int_coeff <- model$coefficients[c("(Intercept)", interventions_list)]
  } else {
    int_coeff <- model$coefficients[interventions_list]
  }

  # get coefficients for the center characteristics
  if (!is.null(center_characteristic_list)) {
    center_characteristics_coeff <- model$coefficients[center_characteristic_list]
  }

  # TODO: add the option for non linear cost functions
  # for v1, we are only supporting linear cost functions, so below, I'm using
  # x[[1]], but this will need to be updated for other types of cost functions.
  cost_coef <- sapply(cost_list_of_lists, function(x) x[[1]])

  # set the values of center_cha_coeff_vec and center_cha based on if
  # center_characteristic_list is defined. See parameter definitions in function
  # get_recommended_interventions_linear_cost for details.
  if (!is.null(center_characteristic_list)) {
    center_cha_coeff_vec <- center_characteristics_coeff
    center_cha <- center_characteristic_list_for_optimization
  } else {
    center_cha_coeff_vec <- 0
    center_cha <- 0
  }

  # get the recommended interventions that satisfy the outcome goal while
  # minimizing the total cost.
  # TODO: add support for the "phase" option, the phase option is used to indicate
  # if we are calculating the recommended interventions for the next stage, or
  # calculating the optimal interventions at the end of the LAGO trial. For the
  # optimal intervention at the end of the LAGO trial, using linear cost
  # functions, see web appendix section 5.1 of Nevo et al for details.
  rec_int_results <- get_recommended_interventions_linear_cost(
    beta_vec = int_coeff,
    center_cha_coeff_vec = center_cha_coeff_vec,
    cost_coef = cost_coef,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    outcome_goal = outcome_goal,
    center_cha = center_cha,
    intercept = include_intercept
  )
  rec_int <- rec_int_results$est_rec_int
  rec_int_cost <- rec_int %*% cost_coef
  est_outcome_goal <- rec_int_results$est_reachable_outcome

  # TODO: add confidence set calculations for the recommended interventions
  # for v1, we are only returning the following list:
  return(list(
    rec_int = rec_int,
    rec_int_cost = rec_int_cost,
    est_outcome_goal = est_outcome_goal
  ))
}






################################
# TEST (should be deleted later)
################################
# coeff <- c(0.1, 0.3, 0.15)
# cost_list_of_lists <- list(list(1),list(4))
# cost_coef <- sapply(cost_list_of_lists, function(x) x[[1]])
# intervention_lower_bounds <- c(0,0)
# intervention_upper_bounds <- c(10,20)
# outcome_goal <- 0.8
#
#
# coeff <- c(0.05, 0.1, 0.15, 0.2)
# cost_list_of_lists <- list(list(1),list(4), list(2.2))
# cost_coef <- sapply(cost_list_of_lists, function(x) x[[1]])
# intervention_lower_bounds <- c(0,0,0)
# intervention_upper_bounds <- c(3,20,5)
# outcome_goal <- 0.9
#
#
# calculate_recommended_interventions(
#   df = infert,
#   outcome_name = "case",
#   outcome_type = "binary",
#   glm_family = "binomial",
#   interventions_list = c("age", "parity"),
#   intervention_lower_bounds = c(0, 0),
#   intervention_upper_bounds = c(50, 10),
#   cost_list_of_lists = list(list(4), list(1)),
#   outcome_goal = 0.5
# )
