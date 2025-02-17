#' get_recommended_interventions
#'
#' @description Internal function that calculates the LAGO recommended
#' interventions based on an outcome goal and/or a power goal.
#'
#' @param data A data frame. The dataset that contains the intervention
#' components and the outcome variable.
#' @param intervention_components_coeff A numeric vector.
#' The coefficient estimates for the intervention components
#' including the intercept term. For example: c(0.1, 0.3, 0.15).
#' @param include_interaction_terms A boolean. Specifies whether there are
#' interaction terms in the intervention components.
#' @param main_components A character vector. Specifies the main intervention
#' components in the presence of interaction terms.
#' @param intervention_components A character vector. The names of the columns
#' in the dataset that represent the intervention components.
#' @param all_center_lvl_effects A numeric vector.
#' The coefficient estimates for the facilities, which includes the fixed
#' center effects and the fixed time effects.
#' @param center_weights_for_outcome_goal A numeric vector. Specifies the
#' weights of all facilities. The weights should sum up to 1.
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
#' @param link A character string. Specifies the link function used when fitting
#' the outcome model.
#' @param lower_outcome_goal A boolean value. Specifies whether the outcome goal
#' is intended to be lower or higher than the average outcome.
#' @param prev_recommended_interventions A numeric vector. Specifies the
#' recommended interventions from the previous stage.
#' @param shrinkage_threshold A numeric value. Specifies the threshold for
#' shrinking the recommended intervention towards the recommended intervention
#' from the previous stage.
#'
#' @return List(
#' recommended interventions,
#' outcome goal,
#' estimated outcome mean/probability for the intervention
#' group in the next stage )
#'
#'
#' @importFrom rje expit logit
#' @import stats
#' @importFrom NlcOptim solnl
#'
#' @export
#'
get_recommended_interventions <- function(
    data,
    intervention_components_coeff,
    include_interaction_terms,
    main_components,
    intervention_components,
    all_center_lvl_effects,
    center_weights_for_outcome_goal,
    cost_list_of_vectors,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal,
    optimization_method,
    optimization_grid_search_step_size,
    center_cha_coeff_vec = 0,
    center_characteristics_optimization_values = 0,
    link = "identity",
    lower_outcome_goal = FALSE,
    prev_recommended_interventions,
    shrinkage_threshold) {
  # shrinking flag
  shrinking_method_used <- FALSE

  # Function to create a cost function based on coefficients
  # in cost_list_of_vectors
  create_cost_function <- function(coeffs) {
    function(x) {
      sum(sapply(seq_along(coeffs), function(i) coeffs[i] * x^(i - 1)))
    }
  }

  # check if prev rec int is avaliable
  # if not, use the mean of the actual intervention values
  if (is.null(prev_recommended_interventions)) {
    if (include_interaction_terms) {
      all_components <- main_components
    } else {
      all_components <- intervention_components
    }
    shrink_to_int_values <- colMeans(
      data[, all_components],
      na.rm = TRUE
    )
  } else {
    shrink_to_int_values <- prev_recommended_interventions
  }

  # implement the grid_search solution
  if (optimization_method == "grid_search") {
    # main optimization function using grid search
    optimize_cost_grid_search <- function(cost_params,
                                          lo,
                                          up,
                                          beta,
                                          outcome_goal,
                                          include_interaction_terms,
                                          main_components,
                                          intervention_components,
                                          all_center_lvl_effects,
                                          center_weights_for_outcome_goal,
                                          center_cha_coeff_vec,
                                          center_cha,
                                          step_size,
                                          link,
                                          shrinkage_threshold) {
      # create sequence grids for each intervention component
      grids <- lapply(seq_along(cost_params), function(i) {
        seq(lo[i], up[i], by = step_size[i])
      })
      # create the full grid
      full_grid <- do.call(expand.grid, grids)

      # create a new grid based on the full grid if there are
      # interaction terms. The full grid should only include
      # main effects, and the new grid should account for
      # the interaction terms properly.
      if (include_interaction_terms) {
        colnames(full_grid) <- main_components

        new_grid <- data.frame(
          matrix(
            nrow = nrow(full_grid),
            ncol = length(intervention_components)
          )
        )
        colnames(new_grid) <- intervention_components

        # Fill in the new grid
        for (i in seq_along(intervention_components)) {
          # Split the component string by ":"
          components <- strsplit(
            gsub("`", "", intervention_components[i]), ":"
          )[[1]]

          if (length(components) == 1) {
            # Single component - just copy the column
            new_grid[, i] <- full_grid[, (components)]
          } else {
            # Multiple components - multiply the corresponding columns
            # Initialize with first component
            result <- full_grid[, (components[1])]
            # Multiply by remaining components
            for (j in 2:length(components)) {
              result <- result * full_grid[, (components[j])]
            }
            new_grid[, i] <- result
          }
        }
      } else {
        new_grid <- full_grid
      }

      # create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)

      # optimization function
      f_combined <- function(int, main_effects_int, link) {
        int_vector <- as.numeric(c(1, int))
        # calculate the outcome for this intervention
        if (link == "logit") {
          outcome <- sum(
            center_weights_for_outcome_goal *
              expit(
                all_center_lvl_effects +
                  sum(beta * int_vector) +
                  center_cha_coeff_vec * center_cha +
                  ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
              )
          )
        } else {
          outcome <- sum(
            center_weights_for_outcome_goal *
              (
                all_center_lvl_effects +
                  sum(beta * int_vector) +
                  center_cha_coeff_vec * center_cha +
                  ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
              )
          )
        }

        # calculate the cost for this intervention
        cost <- sum(mapply(
          function(f, x) f(x),
          cost_functions,
          as.numeric(main_effects_int)
        ))

        return(list(outcome = outcome, cost = cost))
      }

      # apply f_combined to all grid points
      all_results <- mapply(
        function(i) f_combined(new_grid[i, ], full_grid[i, ], link),
        1:nrow(new_grid),
        SIMPLIFY = FALSE # so it rerturns a list
      )

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
        rec_int_cost <- all_costs[best_index]
      } else {
        # check if the largest achievable outcome is larger than
        # 1/4 of the difference between the outcome goal and the
        # estimated outcome without intervention
        no_int_vector <- if (include_interaction_terms) {
          rep(0, length(main_components))
        } else {
          rep(0, length(intervention_components))
        }
        est_outcome_wo_int <- get_est_reachable_outcome(
          x = no_int_vector,
          include_interaction_terms = include_interaction_terms,
          intervention_components = intervention_components,
          main_components = main_components,
          link = link,
          outcome_goal = outcome_goal,
          center_weights_for_outcome_goal = center_weights_for_outcome_goal,
          all_center_lvl_effects = all_center_lvl_effects,
          beta = beta,
          center_cha_coeff_vec = center_cha_coeff_vec,
          center_cha = center_cha
        )

        quarter_point <- est_outcome_wo_int +
          (outcome_goal - est_outcome_wo_int) * shrinkage_threshold
        if (max_outcome >= quarter_point) {
          warning(paste(
            "The outcome goal is not achievable.",
            "However, since the maximum estimated achievable outcome\n",
            "is larger than", shrinkage_threshold,
            "of the difference between the outcome goal",
            "and the estimated outcome\n without intervention, we will use",
            "the largest achievable outcome as the outcome goal.",
            collapse = " "
          ))
          # if the largest achievable outcome is larger than 1/4 of the
          # difference between the outcome goal and the estimated outcome
          # without intervention, we use the largest achievable outcome
          # as the estimated outcome with intervention
          best_index <- which.max(all_outcomes)
          est_rec_int <- as.numeric(full_grid[best_index, ])
          rec_int_cost <- all_costs[best_index]
        } else {
          # shrinking method is used
          # see Section 5.1 of Supplementary Materials of Nevo et al. (2021)
          warning(paste(
            "The outcome goal is not achievable.",
            "Since the maximum estimated achievable outcome\n",
            "is less than 1/4 of the difference between the outcome goal",
            "and the estimated outcome\n without intervention, we will shrink",
            "the recommended intervention towards the\n",
            "recommended intervention from the previous stage.",
            "If you want to avoid the shrinking method,\n",
            "please consider lower the 'shrinkage_threshold' parameter,",
            "the current/default value is 0.25.",
            collapse = " "
          ))
          shrinking_results <- shrinking_method(
            lo = lo,
            up = up,
            beta = beta,
            outcome_goal = outcome_goal,
            include_interaction_terms = include_interaction_terms,
            intervention_components = intervention_components,
            main_components = main_components,
            all_center_lvl_effects = all_center_lvl_effects,
            center_weights_for_outcome_goal = center_weights_for_outcome_goal,
            center_cha_coeff_vec = center_cha_coeff_vec,
            center_cha = center_cha,
            link = link,
            stage_1_intervention = shrink_to_int_values
          )
          shrinking_method_used <- TRUE
          est_rec_int <- as.numeric(shrinking_results)

          cost_functions <- lapply(cost_params, create_cost_function)
          cost <- sum(mapply(
            function(f, x) f(x),
            cost_functions,
            as.numeric(est_rec_int)
          ))
          rec_int_cost <- cost
        }
      }

      est_reachable_outcome <- get_est_reachable_outcome(
        x = est_rec_int,
        include_interaction_terms = include_interaction_terms,
        intervention_components = intervention_components,
        main_components = main_components,
        link = link,
        outcome_goal = outcome_goal,
        center_weights_for_outcome_goal = center_weights_for_outcome_goal,
        all_center_lvl_effects = all_center_lvl_effects,
        beta = beta,
        center_cha_coeff_vec = center_cha_coeff_vec,
        center_cha = center_cha
      )

      return(list(
        est_rec_int = est_rec_int,
        rec_int_cost = rec_int_cost,
        est_reachable_outcome = ifelse(
          lower_outcome_goal,
          -1 * est_reachable_outcome,
          est_reachable_outcome
        ),
        shrinking_method_used = shrinking_method_used
      ))
    }

    # calls the grid search optimization function
    opt_results <- optimize_cost_grid_search(
      cost_list_of_vectors,
      intervention_lower_bounds,
      intervention_upper_bounds,
      intervention_components_coeff,
      outcome_goal,
      include_interaction_terms,
      main_components,
      intervention_components,
      all_center_lvl_effects,
      center_weights_for_outcome_goal,
      center_cha_coeff_vec,
      center_characteristics_optimization_values,
      optimization_grid_search_step_size,
      link,
      shrinkage_threshold
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
                                       all_center_lvl_effects,
                                       center_weights_for_outcome_goal,
                                       center_cha_coeff_vec,
                                       center_cha,
                                       link) {
      # Objective function to maximize outcome
      obj_fun_for_max_outcome <- function(int) {
        # In the presence of interaction terms
        if (include_interaction_terms) {
          int_vector <- numeric(length(intervention_components))
          for (i in seq_along(intervention_components)) {
            components <- strsplit(
              gsub("`", "", intervention_components[i]), ":"
            )[[1]]
            if (length(components) == 1) {
              # For single components, use the value directly
              idx <- which(main_components == components[1])
              int_vector[i] <- int[idx]
            } else {
              # For interaction terms, multiply the corresponding values
              prod_result <- 1
              for (comp in components) {
                # identify which component in the main_components corresponds
                # to "comp".
                idx <- which(main_components == comp)
                prod_result <- prod_result * int[idx]
              }
              int_vector[i] <- prod_result
            }
          }
          int_vector <- c(1, int_vector)
        } else {
          # no interaction term
          int_vector <- c(1, int)
        }
        # negative because NlcOptim minimizes this objective function by default
        return(
          if (link == "logit") {
            -sum(
              center_weights_for_outcome_goal *
                expit(
                  all_center_lvl_effects +
                    sum(beta * int_vector) +
                    center_cha_coeff_vec * center_cha +
                    ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                )
            )
          } else {
            -sum(
              center_weights_for_outcome_goal *
                (
                  all_center_lvl_effects +
                    sum(beta * int_vector) +
                    center_cha_coeff_vec * center_cha +
                    ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                )
            )
          }
        )
      }

      # get the max achievable outcome
      quantile_points <- 1:10 / 10
      results <- numeric(length(quantile_points))
      results_int_components <- matrix(
        0,
        nrow = length(lo),
        ncol = length(quantile_points)
      )
      for (i in seq_along(quantile_points)) {
        start_points <- lo + quantile_points[i] * (up - lo)
        result <- tryCatch(
          {
            solnl(
              X = start_points,
              objfun = obj_fun_for_max_outcome,
              lb = lo,
              ub = up
            )
          },
          error = function(e) {
            return(NULL) # Return NULL if an error occurs
          }
        )

        if (!is.null(result)) {
          results[i] <- -result$fn
          results_int_components[, i] <- result$par
        } else {
          results[i] <- NA # Assign NA if the optimization failed
        }
      }
      max_position <- which.max(results)
      max_achievable_outcome <- results[max_position]
      max_achi_outcome_int_comp <- results_int_components[, max_position]

      # Create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)
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
          if (include_interaction_terms) {
            int_vector <- numeric(length(intervention_components))
            for (i in seq_along(intervention_components)) {
              components <- strsplit(
                gsub("`", "", intervention_components[i]), ":"
              )[[1]]

              if (length(components) == 1) {
                # For single components, use the value directly
                idx <- which(main_components == components[1])
                int_vector[i] <- x[idx]
              } else {
                # For interaction terms, multiply the corresponding values
                prod_result <- 1
                for (comp in components) {
                  # identify which component in the main_components corresponds
                  # to "comp".
                  idx <- which(main_components == comp)
                  prod_result <- prod_result * x[idx]
                }
                int_vector[i] <- prod_result
              }
            }
            int_vector <- c(1, int_vector)
          } else {
            int_vector <- c(1, x)
          }
          if (link == "logit") {
            f <- rbind(
              f,
              outcome_goal -
                sum(
                  center_weights_for_outcome_goal *
                    expit(
                      all_center_lvl_effects +
                        sum(beta * int_vector) +
                        center_cha_coeff_vec * center_cha +
                        ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                    )
                )
            )
          } else {
            f <- rbind(
              f,
              outcome_goal -
                sum(
                  center_weights_for_outcome_goal *
                    (
                      all_center_lvl_effects +
                        sum(beta * int_vector) +
                        center_cha_coeff_vec * center_cha +
                        ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                    )
                )
            )
          }

          return(list(ceq = NULL, c = f))
        }

        cost_results <- numeric(length(quantile_points))

        for (i in seq_along(quantile_points)) {
          start_points <- lo + quantile_points[i] * (up - lo)
          result <- tryCatch(
            {
              solnl(
                X = start_points, # Use start_points instead of a fixed midpoint
                objfun = cost_obj_fun,
                confun = constraint_fun,
                lb = lo,
                ub = up
              )
            },
            error = function(e) {
              return(NULL) # Return NULL if an error occurs
            }
          )

          if (!is.null(result)) {
            cost_results[i] <- result$fn
            results_int_components[, i] <- result$par
          } else {
            cost_results[i] <- NA # Assign NA if the optimization failed
          }
        }

        valid_indices <- which(!is.na(cost_results))
        min_position <- valid_indices[which.max(cost_results[valid_indices])]
        rec_int_cost <- cost_results[min_position]
        int_components <- results_int_components[, min_position]

        est_rec_int <- int_components

        est_reachable_outcome <- get_est_reachable_outcome(
          x = est_rec_int,
          include_interaction_terms = include_interaction_terms,
          intervention_components = intervention_components,
          main_components = main_components,
          link = link,
          outcome_goal = outcome_goal,
          center_weights_for_outcome_goal = center_weights_for_outcome_goal,
          all_center_lvl_effects = all_center_lvl_effects,
          beta = beta,
          center_cha_coeff_vec = center_cha_coeff_vec,
          center_cha = center_cha
        )
      } else {
        # max achievable outcome is less than the outcome goal
        no_int_vector <- if (include_interaction_terms) {
          rep(0, length(main_components))
        } else {
          rep(0, length(intervention_components))
        }
        est_outcome_wo_int <- get_est_reachable_outcome(
          x = no_int_vector,
          include_interaction_terms = include_interaction_terms,
          intervention_components = intervention_components,
          main_components = main_components,
          link = link,
          outcome_goal = outcome_goal,
          center_weights_for_outcome_goal = center_weights_for_outcome_goal,
          all_center_lvl_effects = all_center_lvl_effects,
          beta = beta,
          center_cha_coeff_vec = center_cha_coeff_vec,
          center_cha = center_cha
        )

        quarter_point <- est_outcome_wo_int +
          (outcome_goal - est_outcome_wo_int) * shrinkage_threshold
        if (max_achievable_outcome >= quarter_point) {
          # use the largest achievable outcome as the outcome goal
          warning(paste(
            "The outcome goal is not achievable.",
            "However, since the maximum estimated achievable outcome\n",
            "is larger than", shrinkage_threshold,
            "of the difference between the outcome goal",
            "and the estimated outcome\n without intervention, we will use",
            "the largest achievable outcome as the outcome goal.",
            collapse = " "
          ))

          est_rec_int <- max_achi_outcome_int_comp

          rec_int_cost <- cost_obj_fun(est_rec_int)

          est_reachable_outcome <- get_est_reachable_outcome(
            x = est_rec_int,
            include_interaction_terms = include_interaction_terms,
            intervention_components = intervention_components,
            main_components = main_components,
            link = link,
            outcome_goal = outcome_goal,
            center_weights_for_outcome_goal = center_weights_for_outcome_goal,
            all_center_lvl_effects = all_center_lvl_effects,
            beta = beta,
            center_cha_coeff_vec = center_cha_coeff_vec,
            center_cha = center_cha
          )
        } else {
          # shrinking method is used
          # see Section 5.1 of Supplementary Materials of Nevo et al. (2021)
          warning(paste(
            "The outcome goal is not achievable.",
            "Since the maximum estimated achievable outcome\n",
            "is less than 1/4 of the difference between the outcome goal",
            "and the estimated outcome\n without intervention, we will shrink",
            "the recommended intervention towards the\n",
            "recommended intervention from the previous stage.",
            "If you want to avoid the shrinking method,\n",
            "please consider lower the 'shrinkage_threshold' parameter,",
            "the current/default value is 0.25.",
            collapse = " "
          ))

          shrinking_results <- shrinking_method(
            lo = lo,
            up = up,
            beta = beta,
            outcome_goal = outcome_goal,
            include_interaction_terms = include_interaction_terms,
            intervention_components = intervention_components,
            main_components = main_components,
            all_center_lvl_effects = all_center_lvl_effects,
            center_weights_for_outcome_goal = center_weights_for_outcome_goal,
            center_cha_coeff_vec = center_cha_coeff_vec,
            center_cha = center_cha,
            link = link,
            stage_1_intervention = shrink_to_int_values
          )
          shrinking_method_used <- TRUE
          est_rec_int <- as.numeric(shrinking_results)

          est_reachable_outcome <- get_est_reachable_outcome(
            x = est_rec_int,
            include_interaction_terms = include_interaction_terms,
            intervention_components = intervention_components,
            main_components = main_components,
            link = link,
            outcome_goal = outcome_goal,
            center_weights_for_outcome_goal = center_weights_for_outcome_goal,
            all_center_lvl_effects = all_center_lvl_effects,
            beta = beta,
            center_cha_coeff_vec = center_cha_coeff_vec,
            center_cha = center_cha
          )

          rec_int_cost <- cost_obj_fun(est_rec_int)
        }
      }

      return(list(
        est_rec_int = est_rec_int,
        rec_int_cost = rec_int_cost,
        est_reachable_outcome = ifelse(
          lower_outcome_goal,
          -1 * est_reachable_outcome,
          est_reachable_outcome
        ),
        max_achievable_outcome = max_achievable_outcome,
        shrinking_method_used = shrinking_method_used
      ))
    }

    opt_results <- optimize_cost_nlcoptim(
      cost_list_of_vectors,
      intervention_lower_bounds,
      intervention_upper_bounds,
      intervention_components_coeff,
      outcome_goal,
      all_center_lvl_effects,
      center_weights_for_outcome_goal,
      center_cha_coeff_vec,
      center_characteristics_optimization_values,
      link = link
    )
  }

  return(list(
    est_rec_int = opt_results$est_rec_int,
    rec_int_cost = opt_results$rec_int_cost,
    est_reachable_outcome = opt_results$est_reachable_outcome,
    shrinking_method_used = opt_results$shrinking_method_used
  ))
}
