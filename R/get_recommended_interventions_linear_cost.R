#' get_recommended_interventions_linear_cost
#'
#' @description Calculates the LAGO recommended interventions
#' for the next stage based on a linear cost function and given parameters.
#' The recommended interventions aim to satisfy the outcome goal and/or
#' the power goal.
#'
#' @param beta_vec numeric vector, Coefficient estimates for the intervention
#' components, including the intercept term, but not including any coefficient
#' estimates for the center characteristics.
#' For example: c(0.1, 0.3, 0.15).
#' If intercept == T, then the first element 0.1 is the estimate for the intercept.
#' @param cost_coef numeric vector, Coefficients for the cost functions.
#' For linear cost functions, one element per intervention component is required.
#' @param intervention_lower_bounds numeric vector, Lower bounds for the
#' intervention components.
#' For example: c(0,0).
#' @param intervention_upper_bounds numeric vector, Upper bounds for the
#' intervention components.
#' For example: c(10,20).
#' @param outcome_goal numeric, The outcome goal.
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
#' For v1, this defaults to "planning". we can discuss whether we want to keep
#' both "Planning" and "Optimal".
#'
#' @return List(recommended interventions, outcome goal, and estimated outcome mean/probability
#' for the intervention group in the next stage )
#'
#' @examples
#' get_recommended_interventions_linear_cost(
#'   beta_vec = c(0.1, 0.3, 0.15),
#'   cost_coef = c(1, 4),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 20),
#'   outcome_goal = 0.8
#' )
#'
#' @importFrom rje expit logit
#' @import stats
#'
#' @export
#
#
get_recommended_interventions_linear_cost <- function(beta_vec,
                                                      cost_coef,
                                                      intervention_lower_bounds,
                                                      intervention_upper_bounds,
                                                      outcome_goal,
                                                      center_cha_coeff_vec = 0,
                                                      center_cha = 0,
                                                      intercept = TRUE,
                                                      phase = "planning") {
  # check if center_cha and center_cha_coeff_vec have the same length
  if (length(center_cha) != length(center_cha_coeff_vec)) {
    stop("coefficients for the center charactersitics and the number of given center characteristics are not at the same length.")
  }

  # handle intercept
  if (intercept == T) {
    beta0 <- beta_vec[1]
    beta_vec <- beta_vec[-1]
  } else {
    beta0 <- 0
  }

  # Initialize estimated recommended intervention to minimum values
  est_rec_int <- intervention_lower_bounds
  # Calculate cost-effectiveness of each intervention
  cost_effects <- beta_vec / cost_coef

  # If all effects are non-positive, recommend minimum intervention
  if (all(cost_effects <= 0)) {
    # TODO: notice the "expit", need to generalize to other inverse of link functions.
    est_reachable_outcome <- expit(beta0 + sum(center_cha_coeff_vec * center_cha) + sum(beta_vec * intervention_lower_bounds))
    return(list(
      est_rec_int = intervention_lower_bounds,
      rec_int_cost = cost_coef %*% intervention_lower_bounds,
      est_reachable_outcome = est_reachable_outcome
    ))
  }

  # Separate positive and negative effects
  neg_eff <- cost_effects <= 0
  pos_eff <- cost_effects > 0

  # Calculate baseline logit probability of success
  beta0.center <- beta0 + sum(center_cha_coeff_vec * center_cha) + sum(beta_vec[neg_eff] * intervention_lower_bounds[neg_eff])

  # Check if minimum intervention already reaches the outcome_goal
  if (expit(beta0.center + sum(beta_vec[pos_eff] * intervention_lower_bounds[pos_eff])) >= outcome_goal) {
    return(list(
      est_rec_int = est_rec_int,
      rec_int_cost = cost_coef %*% intervention_lower_bounds,
      est_reachable_outcome = outcome_goal
    ))
  }

  # Order positive effects by cost-effectiveness
  order.effect <- order(cost_effects[pos_eff], decreasing = T)

  # Check if maximum intervention can reach the outcome_goal
  max.can.reach <- as.numeric(expit(beta0.center + sum(beta_vec[pos_eff] * intervention_upper_bounds[pos_eff])))

  if (max.can.reach < outcome_goal) {
    # cannot reach the outcome_goal. check if phase = planning, if so, we try to get the
    # reachable outcome_goal to be as close as possible to the actual outcome_goal.
    if (phase == "planning") {
      est_rec_int[pos_eff] <- intervention_upper_bounds[pos_eff]
    } else {
      # Calculate intervention values for each component based on
      # Nevo et al web appendix section 5.1
      # TODO: generalize this to work with more than 2 intervention components
      for (c in 1:2) {
        if (as.numeric(beta_vec[-c]) < 0) {
          the.other.int <- intervention_lower_bounds[-c]
        } else {
          the.other.int <- intervention_upper_bounds[-c]
        }

        beta.max <- (logit(outcome_goal) - (sum(the.other.int * as.numeric(beta_vec[-c]) + beta0))) / intervention_upper_bounds[c]
        beta.min <- beta.max / 2

        if (as.numeric(beta_vec[c]) < beta.min) {
          est_rec_int[c] <- median(intervention_lower_bounds[c], intervention_upper_bounds[c])
        } else {
          slope <- (intervention_upper_bounds[c] - median(intervention_lower_bounds[c], intervention_upper_bounds[c])) / (beta.max - beta.min)
          est_rec_int[c] <- median(intervention_lower_bounds[c], intervention_upper_bounds[c]) + slope * (as.numeric(beta_vec[c]) - beta.min)
        }
      }
    }
    est_reachable_outcome <- max.can.reach
  } else {
    # can reach the outcome_goal, find the recommended interventions
    est_reachable_outcome <- outcome_goal

    # Iterate through interventions in order of cost-effectiveness
    # It checks if the most cost-effective intervention alone can reach the outcome_goal
    # If not, it adds the next most cost-effective intervention to the mix
    # This process continues until it finds a combination that can reach the outcome_goal
    for (i in 1:max(order.effect)) {
      beta.temp <- beta_vec[pos_eff][order.effect][1:i]
      intervention_upper_bounds.temp <- intervention_upper_bounds[pos_eff][order.effect][1:i]

      if (i < length(order.effect)) {
        beta.other.temp <- beta_vec[pos_eff][order.effect][(i + 1):length(order.effect)]
        x.other.temp <- intervention_lower_bounds[pos_eff][order.effect][(i + 1):length(order.effect)]
      } else {
        # if we are considering the last intervention component
        beta.other.temp <- x.other.temp <- 0
      }

      # Check if current set of interventions can reach the outcome_goal
      if (expit(beta0.center + sum(beta.temp * intervention_upper_bounds.temp) + sum(beta.other.temp * x.other.temp)) < outcome_goal) {
        next
      }

      # If the outcome_goal is exactly reached with all considered interventions at their maximum
      if (expit(beta0.center + sum(beta.temp * intervention_upper_bounds.temp) + sum(beta.other.temp * x.other.temp)) == outcome_goal) {
        est_rec_int[pos_eff][order.effect][1:i] <- intervention_upper_bounds.temp
        est_rec_int[pos_eff][order.effect][1:i] <- intervention_upper_bounds.temp
        break
      }

      # The outcome_goal can be reached, we calculate the recommended intervention levels
      # If only one intervention is needed
      if (i == 1) {
        est_rec_int[pos_eff][order.effect][i] <- (logit(outcome_goal) - beta0.center - sum(x.other.temp * beta.other.temp)) / beta.temp[i]
        break
      } else {
        # multiple interventions are needed
        est_rec_int[pos_eff][order.effect][1:(i - 1)] <- intervention_upper_bounds.temp[-i]
        # the top i-1 gets their maximal values
        est_rec_int[pos_eff][order.effect][i] <- (logit(outcome_goal) - sum(beta.temp[1:(i - 1)] * intervention_upper_bounds.temp[1:(i - 1)]) - beta0.center - sum(x.other.temp * beta.other.temp)) / beta.temp[i]
        break
      }
    }
  }

  return(list(
    est_rec_int = est_rec_int,
    rec_int_cost = cost_coef %*% est_rec_int,
    est_reachable_outcome = est_reachable_outcome
  ))
}

#
# get_recommended_interventions_linear_cost(beta_vec = c(0.1, 0.3, 0.15, 0.2),
#                                           cost_coef = c(1,2,3),
#                                           intervention_lower_bounds = c(0,0,0),
#                                           intervention_upper_bounds = c(2,3,10),
#                                           outcome_goal = 0.8,
#                                           center_cha_coeff_vec = 0,
#                                           center_cha = 0,
#                                           intercept = TRUE,
#                                           phase = "planning")
