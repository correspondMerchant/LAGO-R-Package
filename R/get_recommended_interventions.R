#' get_recommended_interventions
#'
#' @description Internal function that calculates the LAGO recommended
#' interventions based on an outcome goal and/or a power goal.
#'
#' @param beta_vector A numeric vector. The coefficient estimates for the
#' intervention components including the intercept term
#' For example: c(0.1, 0.3, 0.15).
#' @param center_cha_coeff_vec A numeric vector. The coefficients estimates for
#' the center characteristics.
#' For example: c(-0.4).
#' @param center_characteristics_optimization_values A numeric vector. The
#' values of the center characteristics that will be used for LAGO optimization.
#' For example: c(1.74)
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
#'
#' @return List(
#' recommended interventions,
#' outcome goal,
#' estimated outcome mean/probability for the intervention
#' group in the next stage )
#'
#' @examples
#' # Example for using the grid search
#' get_recommended_interventions(
#'   beta_vector = c(0.1, 0.3, 0.15),
#'   cost_list_of_vectors = list(c(0, 1, 2), c(0, 4)),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 20),
#'   optimization_grid_search_step_size = c(2, 5),
#'   outcome_goal = 0.8,
#'   optimization_method = "grid_search",
#' )
#'
#' # Example for using the numerical method
#' get_recommended_interventions(
#'   beta_vector = c(0.1, 0.3, 0.15),
#'   cost_list_of_vectors = list(c(0, 1, 2), c(0, 4)),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 20),
#'   optimization_grid_search_step_size = c(2, 5),
#'   outcome_goal = 0.8,
#'   optimization_method = "numerical"
#' )
#'
#' @importFrom rje expit logit
#' @import stats
#' @importFrom NlcOptim solnl
#'
#' @export
#'
get_recommended_interventions <- function(
    beta_vector,
    cost_list_of_vectors,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal,
    optimization_method,
    optimization_grid_search_step_size,
    center_cha_coeff_vec = 0,
    center_characteristics_optimization_values = 0) {
  # Function to create a cost function based on coefficients
  # in cost_list_of_vectors
  create_cost_function <- function(coeffs) {
    function(x) {
      sum(sapply(seq_along(coeffs), function(i) coeffs[i] * x^(i - 1)))
    }
  }

  # implement the grid_search solution
  if (optimization_method == "grid_search") {
    # main optimization function using grid search
    optimize_cost_grid_search <- function(cost_params,
                                          lo,
                                          up,
                                          beta,
                                          outcome_goal,
                                          center_cha_coeff_vec,
                                          center_cha,
                                          step_size) {
      # create sequence grids for each intervention component
      grids <- lapply(seq_along(cost_params), function(i) {
        seq(lo[i], up[i], by = step_size[i])
      })
      # create the full grid
      full_grid <- do.call(expand.grid, grids)
      # create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)

      # optimization function
      f_combined <- function(int) {
        int_vector <- c(1, int)
        # calculate the outcome for this intervention
        outcome <- expit(
          sum(beta * int_vector) + center_cha_coeff_vec * center_cha
        )
        # calculate the cost for this intervention
        cost <- sum(mapply(function(f, x) f(x), cost_functions, int))

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
        # if the outcome goal is achievable,
        # find the minimum cost solution that meets the goal
        valid_indices <- which(all_outcomes >= outcome_goal)
        best_index <- valid_indices[which.min(all_costs[valid_indices])]

        est_rec_int <- as.numeric(full_grid[best_index, ])
        est_reachable_outcome <- outcome_goal
        rec_int_cost <- all_costs[best_index]
      } else {
        # if the outcome goal is not achievable,
        # find the solution with the maximum outcome
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
      cost_list_of_vectors, intervention_lower_bounds,
      intervention_upper_bounds, beta_vector, outcome_goal,
      center_cha_coeff_vec, center_characteristics_optimization_values,
      optimization_grid_search_step_size
    )
  } else if (optimization_method == "numerical") {
    # we use the solnl() function from the NlcOptim library (Jingyu's idea)
    # the objective function is the function that we are trying to minimize:
    # total cost, or the sum of cost for each intervention component.

    # Main optimization function using solnl()
    optimize_cost_nlcoptim <- function(cost_params,
                                       lo,
                                       up,
                                       beta,
                                       outcome_goal,
                                       center_cha_coeff_vec,
                                       center_cha) {
      # Create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)

      # Objective function to maximize outcome
      # TODO: for now, the objective function is just expit, we will need to
      # make it work with other link functions when fitting GLM
      obj_fun_for_max_outcome <- function(int) {
        int_vector <- c(1, int)
        # negative because NlcOptim minimizes this objective function by default
        -expit(sum(beta * int_vector) + center_cha_coeff_vec * center_cha)
      }

      # get the max achievable outcome
      result_max <- solnl(
        X = (lo + up) / 2,
        objfun = obj_fun_for_max_outcome,
        lb = lo,
        ub = up
      )
      max_achievable_outcome <- -result_max$fn


      # objective function for the total cost
      cost_obj_fun <- function(x) {
        return(sum(mapply(function(f, x) f(x), cost_functions, x)))
      }

      # check if the max achievable outcome is larger than the outcome goal
      if (max_achievable_outcome >= outcome_goal) {
        # If the goal is achievable, get the recommended intervention that
        # minimizes the total cost function

        # a vector of constraint equations
        # (see helper doc of NlcOptim package for details)
        constraint_fun <- function(x) {
          f <- NULL
          int_vector <- c(1, x)
          f <- rbind(
            f,
            outcome_goal - expit(sum(beta * int_vector) + center_cha_coeff_vec * center_cha)
          )
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
      cost_list_of_vectors, intervention_lower_bounds,
      intervention_upper_bounds, beta_vector, outcome_goal,
      center_cha_coeff_vec, center_characteristics_optimization_values
    )
  }

  return(list(
    est_rec_int = opt_results$est_rec_int,
    rec_int_cost = opt_results$rec_int_cost,
    est_reachable_outcome = opt_results$est_reachable_outcome
  ))
}
