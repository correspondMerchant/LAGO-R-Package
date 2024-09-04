#' calculate_recommended_interventions
#'
#' @description Returns the LAGO recommended interventions for the next stage.
#' The recommended interventions that aim to satisfy the outcome goal and/or the
#' power goal.
#'
#' @param df data.frame, Input data
#' @param outcome_type character, Outcome type, either "continuous" or "binary"
#' @param glm_family character, Family for the GLM model
#' For example: "binomial"
#' @param interventions_list list, Names of the intervention components
#' For example: list("component1", "component2")
#' @param center_characteristic_list list, Names of the center characteristics
#' For example: list("characteristic1")
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
#' @param outcome_goal_optimization character, Method used behind the scenes for
#' calculating the recommended interventions. Either "numerical" or "grid_search"
#' @param outcome_goal numeric, The outcome goal
#'
#' @return List(recommended interventions, associated cost for the interventions,
#' estimated outcome mean/probability for the intervention group in the next stage)
#'
#' @examples
#' calculate_recommended_interventions(df = data_frame,
#'                                     outcome_type = "binary",
#'                                     glm_family = "binomial",
#'                                     interventions_list = list("component1", "component2"),
#'                                     intervention_lower_bounds = c(0, 0),
#'                                     intervention_upper_bounds = c(10, 20),
#'                                     cost_list_of_lists = list(list(1), list(4)),
#'                                     outcome_goal = 0.8)
#'
#' @export
#'
calculate_recommended_interventions <- function(df,
                                                outcome_type,
                                                glm_family,
                                                interventions_list,
                                                center_characteristic_list = NULL,
                                                intervention_lower_bounds,
                                                intervention_upper_bounds,
                                                cost_list_of_lists,
                                                outcome_goal_optimization = "numerical",
                                                outcome_goal) {
  # TODO: future versions of this package will use 'outcome_goal_optimization'
  # For now, we are only working with linear cost, so no need for numerical solutions yet.

  # initial checks
  # check if the data frame is not null, is a data frame type, and not empty
  if (is.null(df)) {
    stop("The data frame 'df' is NULL.")
  }
  if (!is.data.frame(df)) {
    stop("The argument 'df' must be a data frame.")
  }
  if (nrow(df) == 0 || ncol(df) == 0) {
    stop("The data frame 'df' is empty.")
  }

  allowed_outcome_types <- c("continuous", "binary")
  if (!(outcome_type %in% allowed_outcome_types)) {
    stop(paste("Outcome type", outcome_type, "is not 'continuous' or 'binary'."))
  }

  # check if the data frame has all the required columns
  # TODO: specify the required_columns
  required_columns <- c("outcome", "group", "center")
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(sprintf(
      "The following required columns are missing: %s",
      paste(missing_columns, collapse = ", ")
    ))
  }

  # check if interventions_list is empty, and is made up with a list of strings
  if (!is.list(interventions_list) || !all(sapply(
    interventions_list,
    is.character
  ))) {
    stop("interventions_list must be a list of strings")
  }

  # check if interventions_list are all columns in the data frame
  if (!all(unlist(interventions_list) %in% names(df))) {
    stop("All elements in the interventions_list must be column names in the provided data frame.")
  }

  # If center characteristic is provided, check if it is in the data frame
  if (!is.null(center_characteristic_list)) {
    if (!is.list(center_characteristic_list) || !all(sapply(
      center_characteristic_list,
      is.character
    ))) {
      stop("center_characteristic_list must be a list of strings")
    }
    if (!all(unlist(center_characteristic_list) %in% names(df))) {
      stop("All elements in the center_characteristic_list must be column names in the provided data frame.")
    }
  }

  # check if lower bounds list and upper bounds list have the same length,
  # and if the lower bounds are <= upper bounds respectively
  if (length(intervention_lower_bounds) != length(intervention_upper_bounds)) {
    stop("The lengths of lower and upper bounds do not match.")
  }
  if (any(intervention_lower_bounds) < 0) {
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
  for (i in seq_along(cost_list_of_lists)) {
    if (!is.numeric(cost_list_of_lists[[i]])) {
      stop(paste("Element", i, "of cost_list_of_lists must be a numeric vector."))
    }
  }

  # check if the dimension of cost_list_of_lists matches the dimension of interventions_list
  if (length(cost_list_of_lists) != length(interventions_list)) {
    stop("The lengths of cost_list_of_lists and interventions_list do not match.")
  }

  # check if the outcome goal >= intervention group average values
  int_group_mean <- mean(df$outcome[df$group == 1], na.rm = TRUE)
  if (outcome_goal <= int_group_mean) {
    stop("The outcome goal is smaller than the intervention group mean, please increase the outcome goal.")
  }



  # Convert glm family string to actual glm family object
  # TODO: figure out which ones do we want to support?
  # for v1, only binomial.
  family_object <- switch(glm_family,
    "binomial" = binomial(),
    "poisson" = poisson(),
    "gaussian" = gaussian(),
    "Gamma" = Gamma(),
    "inverse.gaussian" = inverse.gaussian(),
    "quasi" = quasi(),
    "quasibinomial" = quasibinomial(),
    "quasipoisson" = quasipoisson(),
    stop("Unsupported family. Please use one of the supported family types.")
  )

  # fit the model
  if (is.null(center_characteristic_list)) {
    formula <- as.formula(paste("outcome", "~", paste(interventions_list, collapse = " + ")))
    model <- glm(formula, data = df, family = family_object)
  } else {
    formula_with_center_characteristics <- as.formula(paste("outcome", "~", paste(interventions_list, collapse = " + "), paste(center_characteristic_list, collapse = " + ")))
    model <- glm(formula_with_center_characteristics, data = df, family = family_object)
  }

  if (!model$converged) {
    stop("Model did not converge. Please check your data and model specifications.")
  }

  # TODO: add the option for center characteristics in the model
  coeff <- model$coefficients

  # TODO: add the option for non linear cost functions
  # TODO: add another helper function to deal with non-linear cost functions
  cost_coeff <- sapply(cost_list_of_lists, function(x) x[[1]])

  # get the recommended interventions that satisfy the outcome goal while minimizing the cost
  # TODO: add a programming check for whether the model is fitted with an intercept
  # TODO: add "phase" option, to indicate if we are calculating the recommended interventions
  # for the next stage, or calculating the optimal interventions for future studies. For the
  # optimal intervention, for linear cost functions, see web appendix section 5.1 of Nevo et al.
  rec_int_results <- get_recommended_interventions_linear_cost(coeff,
                                                               center_cha_coeff_vec = 0,
                                                               cost_coeff,
                                                               intervention_lower_bounds,
                                                               intervention_upper_bounds,
                                                               outcome_goal,
                                                               center_cha = 0,
                                                               intercept = T)
  rec_int <- rec_int_results$est_rec_int
  rec_int_cost <- rec_int %*% cost_coeff
  est_outcome_goal <- rec_int_results$est_reachable_goal

  # TODO: add confidence set for the recommended interventions

  return(list(
    rec_int,
    rec_int_cost,
    est_outcome_goal
  ))
}






############
# TEST
############
# coeff <- c(0.1, 0.3, 0.15)
# cost_list_of_lists <- list(list(1),list(4))
# cost_coeff <- sapply(cost_list_of_lists, function(x) x[[1]])
# intervention_lower_bounds <- c(0,0)
# intervention_upper_bounds <- c(10,20)
# outcome_goal <- 0.8
#
#
# coeff <- c(0.05, 0.1, 0.15, 0.2)
# cost_list_of_lists <- list(list(1),list(4), list(2.2))
# cost_coeff <- sapply(cost_list_of_lists, function(x) x[[1]])
# intervention_lower_bounds <- c(0,0,0)
# intervention_upper_bounds <- c(3,20,5)
# outcome_goal <- 0.9
