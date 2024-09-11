#' get_recommended_interventions_higher_order_cost
#'
#' @description Calculates the LAGO recommended interventions
#' for the next stage based on a general cost function (not necessarily linear)
#' and given parameters.
#' The recommended interventions aim to satisfy the outcome goal and/or
#' the power goal.
#'
#' @param beta_vec numeric vector, Coefficient estimates for the intervention
#' components, including the intercept term, but not including any coefficient
#' estimates for the center characteristics.
#' For example: c(0.1, 0.3, 0.15).
#' If intercept == T, then the first element 0.1 is the estimate for the intercept.
#' @param cost_list_of_lists A nested list structure defining cost functions for
#' the intervention components. Each sublist represents a component and contains
#' coefficients for its cost function.
#' @param intervention_lower_bounds numeric vector, Lower bounds for the
#' intervention components.
#' For example: c(0,0).
#' @param intervention_upper_bounds numeric vector, Upper bounds for the
#' intervention components.
#' For example: c(10,20).
#' @param outcome_goal numeric, The outcome goal.
#' @param outcome_goal_optimization character, Method used behind the scenes for
#' calculating the recommended interventions. Either "numerical" or "grid_search".
#' @param cost_function_weights numeric vector, A numeric vector defining the weights for the
#' cost functions of the intervention components.
#' For example, if cost_list_of_lists = list(c(1, 2, 3, 4), c(4, 6), c(5, 4, 3)),
#' and cost_function_weights = c(0.2, 1.4, 1.6) then the total cost function is:
#' (1 + 2x + 3x^2 + 4x^3)\*0.2 + (4 + 6x)\*1.4 + (5 + 4x + 3x^2)\*1.6.
#' @param grid_search_step_size numeric, step size for the grid search algorithm,
#' default value is 0.01 for each intervention component.
#' @param center_cha_coeff_vec numeric vector, Coefficients estimates for the
#' center characteristics.
#' For example: c(0.2, 0.4).
#' @param center_cha numeric vector, Given values of the center characteristics.
#' @param intercept boolean, Whether the intercept was included in the fitted model.
#' @param phase character, Either "Planning" or "Optimal". "Planning" means
#' we are calculating the recommended interventions for the next stage. When the
#' outcome goal cannot be reached, we aim to bring the estimated outcome mean/probability
#' for the intervention group in the next stage to be as close to the outcome goal
#' as possible. "Optimal" means we are calculating the optimal interventions at the
#' end of the stage, using the idea from web appendix section 5.1 of Nevo et al.
#'
#' For now, this defaults to "planning". we can discuss whether we need
#' both "Planning" and "Optimal".
#'
#' @return List(recommended interventions, outcome goal, and estimated outcome mean/probability
#' for the intervention group in the next stage )
#'
#' @examples
#' # Example for using the grid search
#' get_recommended_interventions_higher_order_cost(
#'   beta_vec = c(0.1, 0.3, 0.15),
#'   cost_list_of_lists = list(c(1, 2), c(4)),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 20),
#'   outcome_goal = 0.8,
#'   outcome_goal_optimization = "grid_search",
#' )
#'
#' # Example for using the numerical method
#' get_recommended_interventions_higher_order_cost(
#'   beta_vec = c(0.1, 0.3, 0.15),
#'   cost_list_of_lists = list(c(1, 2), c(4)),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 20),
#'   outcome_goal = 0.8,
#'   outcome_goal_optimization = "numerical"
#' )
#'
#' # Example for using the grid search method with cost function weights
#' get_recommended_interventions_higher_order_cost(
#'   beta_vec = c(0.1, 0.3, 0.15),
#'   cost_list_of_lists = list(c(0, 1, 2), c(0, 4, 4, 4)),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 20),
#'   outcome_goal = 0.8,
#'   outcome_goal_optimization = "grid_search",
#'   cost_function_weights = c(0.4, 1.6)
#' )
#'
#' # Example for using the numerical method with cost function weights
#' get_recommended_interventions_higher_order_cost(
#'   beta_vec = c(0.1, 0.3, 0.15),
#'   cost_list_of_lists = list(c(0, 1, 2), c(0, 4, 4, 4)),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 20),
#'   outcome_goal = 0.8,
#'   outcome_goal_optimization = "numerical",
#'   cost_function_weights = c(0.4, 1.6)
#' )
#'
#' @importFrom rje expit logit
#' @import stats
#' @importFrom NlcOptim solnl
#'
#' @export
#'
#'
get_recommended_interventions_higher_order_cost <- function(beta_vec,
                                                            cost_list_of_lists,
                                                            intervention_lower_bounds,
                                                            intervention_upper_bounds,
                                                            outcome_goal,
                                                            outcome_goal_optimization = "numerical",
                                                            cost_function_weights = NULL,
                                                            grid_search_step_size = 0.01,
                                                            center_cha_coeff_vec = 0,
                                                            center_cha = 0,
                                                            intercept = TRUE,
                                                            phase = "planning") {
  # check if center_cha and center_cha_coeff_vec have the same length
  if (length(center_cha) != length(center_cha_coeff_vec)) {
    stop("coefficients for the center charactersitics and the number of given center characteristics are not at the same length.")
  }
  # Function to create a cost function based on coefficients in cost_list_of_lists
  # TODO: for now, there is no fixed cost associated with each component.
  # we may need to change the following to x^(i-1) later to give user the option
  # to add fixed costs.
  create_cost_function <- function(coeffs) {
    function(x) {
        sum(sapply(seq_along(coeffs), function(i) coeffs[i] * x^(i-1)))
    }
  }

  # implement the grid_search solution
  if (outcome_goal_optimization == "grid_search") {
    # main optimization function using grid search
    optimize_cost_grid_search <- function(cost_params, lo, up, beta, outcome_goal,
                                          center_cha_coeff_vec, center_cha,
                                          step_size) {
      # create sequence grids for each intervention component
      grids <- lapply(seq_along(cost_params), function(i) {
        seq(lo[i], up[i], by = step_size)
      })
      # create the full grid
      full_grid <- do.call(expand.grid, grids)
      # create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)

      # optimization function
      f_combined <- function(int) {
        # define intervention vector based on whether the fitted model has intercept
        int_vector <- if (intercept) c(1, int) else int
        # calculate the outcome for this intervention
        outcome <- expit(sum(beta * int_vector) + center_cha_coeff_vec * center_cha)
        # calculate the cost for this intervention
        if (is.null(cost_function_weights)) {
          cost <- sum(mapply(function(f, x) f(x), cost_functions, int))
        } else {
          cost <- sum(mapply(function(f, x, w) w*f(x), cost_functions, int, cost_function_weights))
        }

        return(list(outcome = outcome, cost = cost))
      }

      # apply f_combined to all grid points
      all_results <- apply(full_grid, 1, f_combined)

      # extract outcomes and costs
      all_outcomes <- sapply(all_results, function(x) x$outcome)
      all_costs <- sapply(all_results, function(x) x$cost)

      # find the maximum outcome
      max_outcome <- max(all_outcomes)

      if (max_outcome >= outcome_goal) {
        # if the outcome goal is achievable, find the minimum cost solution that meets the goal
        valid_indices <- which(all_outcomes >= outcome_goal)
        best_index <- valid_indices[which.min(all_costs[valid_indices])]

        est_rec_int <- as.numeric(full_grid[best_index, ])
        est_reachable_outcome <- outcome_goal
        rec_int_cost <- all_costs[best_index]
      } else {
        # if the outcome goal is not achievable, find the solution with the maximum outcome
        best_index <- which.max(all_outcomes)

        est_rec_int <- as.numeric(full_grid[best_index, ])
        est_reachable_outcome <- max_outcome
        rec_int_cost <- all_costs[best_index]
      }

      return(list(
        est_rec_int = est_rec_int,
        rec_int_cost = rec_int_cost,
        est_reachable_outcome = est_reachable_outcome
      ))
    }

    # calls the grid search optimization function
    opt_results <- optimize_cost_grid_search(
      cost_list_of_lists, intervention_lower_bounds,
      intervention_upper_bounds, beta_vec, outcome_goal,
      center_cha_coeff_vec, center_cha, grid_search_step_size
    )
  } else if (outcome_goal_optimization == "numerical") {
    # we use the solnl() function from the NlcOptim library (Jingyu's idea)
    # the objective function is the function that we are trying to minimize
    # so in our case, the total cost, sum of cost for each intervention component.

    # Main optimization function using solnl()
    optimize_cost_nlcoptim <- function(cost_params, lo, up, beta, outcome_goal,
                                       center_cha_coeff_vec, center_cha, intercept = TRUE) {
      # Create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)

      # Objective function to maximize outcome
      # TODO: for now, the objective function is just expit, we will need to
      # make it work with other link functions when fitting GLM
      obj_fun_for_max_outcome <- function(int) {
        int_vector <- if (intercept) c(1, int) else int
        # negative because NlcOptim minimizes this objective function by default
        -expit(sum(beta * int_vector) + center_cha_coeff_vec * center_cha)
      }

      # get the max achievable outcome
      result_max <- solnl(X = (lo + up) / 2, objfun = obj_fun_for_max_outcome, lb = lo, ub = up)
      max_achievable_outcome <- -result_max$fn


      # objective function for the total cost
      cost_obj_fun <- function(x) {
        if (is.null(cost_function_weights)) {
          return(sum(mapply(function(f, x) f(x), cost_functions, x)))
        } else {
          return(sum(mapply(function(f, x, w) w*f(x), cost_functions, x, cost_function_weights)))
        }
      }
      print(cost_obj_fun)

      # check if the max achievable outcome is larger than the outcome goal
      if (max_achievable_outcome >= outcome_goal) {
        # If the goal is achievable, get the recommended intervention that minimizes
        # the total cost function

        # a vector of constraint equations (see helper doc of NlcOptim package for details)
        constraint_fun <- function(x) {
          f <- NULL
          int_vector <- if (intercept) c(1, x) else x
          f <- rbind(f, outcome_goal - expit(sum(beta * int_vector) + center_cha_coeff_vec * center_cha))
          return(list(ceq = NULL, c = f))
        }

        # run optimization
        result <- solnl(
          X = (lo + up) / 2,
          objfun = cost_obj_fun,
          confun = constraint_fun,
          lb = lo,
          ub = up
        )

        est_rec_int <- result$par
        est_reachable_outcome <- outcome_goal
        rec_int_cost <- result$fn
      } else {
        # If the goal is not achievable, return the max achievable outcome
        est_rec_int <- result_max$par
        est_reachable_outcome <- max_achievable_outcome
        rec_int_cost <- cost_obj_fun(est_rec_int)
      }

      return(list(
        est_rec_int = est_rec_int,
        rec_int_cost = rec_int_cost,
        est_reachable_outcome = est_reachable_outcome,
        max_achievable_outcome = max_achievable_outcome
      ))
    }


    opt_results <- optimize_cost_nlcoptim(
      cost_list_of_lists, intervention_lower_bounds,
      intervention_upper_bounds, beta_vec, outcome_goal,
      center_cha_coeff_vec, center_cha
    )
  }

  return(list(
    est_rec_int = opt_results$est_rec_int,
    rec_int_cost = opt_results$rec_int_cost,
    est_reachable_outcome = opt_results$est_reachable_outcome
  ))
}







### TEST ####
# delete later #
# result <- get_recommended_interventions_higher_order_cost(beta_vec = c(-0.13830, 0.03442, 0.16568),
#                                                     cost_list_of_lists = list(c(380, -24, 0.6), c(1700, -950, 220)),
#                                                     intervention_lower_bounds = c(1, 1),
#                                                     intervention_upper_bounds = c(40, 5),
#                                                     outcome_goal = 0.8,
#                                                     outcome_goal_optimization = "grid_search",
#                                                     grid_search_step_size = 0.01,
#                                                     center_cha_coeff_vec = -0.20238,
#                                                     center_cha = 1.753519
#                                                     )
# print(result)
# should be 3.97, 35.5
#
# result <- get_recommended_interventions_higher_order_cost(beta_vec = c(-0.13830, 0.03442, 0.16568),
#                                                     cost_list_of_lists = list(c(380, -24, 0.6), c(1700, -950, 220)),
#                                                     intervention_lower_bounds = c(1, 1),
#                                                     intervention_upper_bounds = c(40, 5),
#                                                     outcome_goal = 0.9,
#                                                     outcome_goal_optimization = "numerical",
#                                                     grid_search_step_size = 0.01,
#                                                     center_cha_coeff_vec = -0.20238,
#                                                     center_cha = 1.753519
#                                                     )
# print(result)
#
#
# get_recommended_interventions_higher_order_cost(
#    beta_vec = c(0.1, 0.3, 0.15),
#    cost_list_of_lists = list(c(0, 1, 2), c(0, 4, 4, 4)),
#    intervention_lower_bounds = c(1, 1),
#    intervention_upper_bounds = c(10, 20),
#    outcome_goal = 0.8,
#    outcome_goal_optimization = "grid_search",
#    grid_search_step_size = 0.1,
#    cost_function_weights = c(0.4, 1.6)
#  )
