#' calculate_recommended_interventions
#'
#' @description Calculate the LAGO recommended interventions for the next stage.
#' The recommended interventions aim to satisfy the outcome goal and/or the
#' power goal.
#'
#' @param df Description of the first parameter
#' @param outcome_type Description of the second parameter
#' @param interventions_list
#' @param intervention_lower_bounds
#' @param intervention_upper_bounds
#' @param cost_list_of_lists
#' @param outcome_goal_optimization
#' @param outcome_goal
#'
#' @return Description of what the function returns
#'
#' @examples
#' example_function(1, 2)
#'
#' @importFrom package function
#' @importFrom package function
#' @importFrom package function
#' @export
#'
calculate_recommended_interventions <- function(df,
                                                outcome_type,
                                                interventions_list,
                                                intervention_lower_bounds,
                                                intervention_upper_bounds,
                                                cost_list_of_lists,
                                                outcome_goal_optimization = "numerical",
                                                outcome_goal) {
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
  # TODO: complete the required_columns
  required_columns <- c("outcome", "group")
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
    stop("All elements in the interventions_list must be column names in the provided data frame")
  }

  # check if lower bounds list and upper bounds list have the same length,
  # and if the lower bounds are <= upper bounds respectively
  if (length(intervention_lower_bounds) != length(intervention_upper_bounds)) {
    stop("The lengths of lower and upper bounds do not match.")
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


  #
}
