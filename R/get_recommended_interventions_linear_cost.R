#' get_recommended_interventions_linear_cost
#'
#' @description Function to actually calculate the LAGO recommended interventions
#' for the next stage. The recommended interventions aim to satisfy the
#' outcome goal and/or the power goal.
#'
#' @param beta_vec Description of the first parameter
#' @param center_cha_coeff_vec Description of the second parameter
#' @param cost_coef
#' @param x_min
#' @param x_max
#' @param goal
#' @param center_cha
#' @param intercept
#' @param phase
#'
#' @return Description of what the function returns
#'
#' @examples
#' example_function(1, 2)
#'
#' @importFrom package function
#' @importFrom package function
#' @importFrom package function
#'
#' Note: this is not a user facing function
#'
get_recommended_interventions_linear_cost <- function(beta_vec,
                                                      center_cha_coeff_vec = 0,
                                                      cost_coef,
                                                      x_min,
                                                      x_max,
                                                      goal,
                                                      center_cha = 0,
                                                      intercept = T,
                                                      phase) {
  if (length(center_cha) != length(center_cha_coeff_vec)) {
    stop("center_cha_coeff_vec and center_cha are not at the same length")
  }
  if (intercept == T) {
    beta0 <- beta_vec[1]
    beta_vec <- beta_vec[-1]
  } else {
    beta0 <- 0
  }

  est_rec_int <- x_min
  cost_effects <- beta_vec / cost_coef

  # If all betas are non-positive the recommended intervention is x_min
  if (all(cost_effects <= 0)) {
    # TODO: notice the "expit", need to generalize to other inverse of link functions.
    est_reachable_goal <- expit( beta0 + sum(center_cha_coeff_vec * center_cha) + sum(beta_vec * x_min) )
    return(list(
      est_rec_int = x_min,
      goal = goal,
      est_reachable_goal = est_reachable_goal
    ))
  }

  # If some betas are negative set their value in the
  # estimated recommended intervention to x_min
  neg_eff <- cost_effects <= 0
  pos_eff <- cost_effects > 0

  # Setting the baseline logit prob success by summing the intercept,
  # the effect of center_cha, and the minimal values of the intervention
  beta0.center <- beta0 + sum(center_cha_coeff_vec * center_cha) + sum(beta_vec[neg_eff] * x_min[neg_eff])

  if (expit(beta0.center + sum(beta_vec[pos_eff] * x_min[pos_eff])) >= goal) {
    return(list(est_rec_int = est_rec_int,
                goal = goal,
                est_reachable_goal = goal))
  }

  order.effect <- order(cost_effects[pos_eff], decreasing = T)
  # Check if setting all positive effects to x_max can reach the goal
  max.can.reach <- expit(beta0.center + sum(beta_vec[pos_eff] * x_max[pos_eff]))

  if (max.can.reach < goal) {
    # cannot reach the goal. check if phase = planning, if so, we try to get the
    # reachable goal to be as close as possible to the actual goal.
    if (phase == "planning") {
      est_rec_int[pos_eff] <- x_max[pos_eff]
    } else {
      # Nevo et al web appendix section 5.1
      # TODO: make this work for more than 2 intervention components?
      for (c in 1:2) {
        if (as.numeric(beta_vec[-c]) < 0) {
          the.other.int <- x_min[-c]
        } else {
          the.other.int <- x_max[-c]
        }

        beta.max <- (logit(goal) - (sum(the.other.int * as.numeric(beta_vec[-c]) + beta0))) / x_max[c]
        beta.min <- beta.max / 2

        if (as.numeric(beta_vec[c]) < beta.min) {
          est_rec_int[c] <- median(x_min[c], x_max[c])
        } else {
          slope <- (x_max[c] - median(x_min[c], x_max[c])) / (beta.max - beta.min)
          est_rec_int[c] <- median(x_min[c], x_max[c]) + slope * (as.numeric(beta_vec[c]) - beta.min)
        }
      }
    }
    est_reachable_goal <- max.can.reach
  } else {
    # can reach the goal, so lets find the recommended interventions
    est_reachable_goal <- goal

    for (i in 1:max(order.effect)) {
      # identify the 1 to i--th most cost effective betas
      beta.temp <- beta_vec[pos_eff][order.effect][1:i]
      # find upper bounds
      x_max.temp <- x_max[pos_eff][order.effect][1:i]

      if (i < length(order.effect)) {
        beta.other.temp <- beta_vec[pos_eff][order.effect][(i + 1):length(order.effect)]
        x.other.temp <- x_min[pos_eff][order.effect][(i + 1):length(order.effect)]
      } else {
        # if we are considering the last intervention component
        beta.other.temp <- x.other.temp <- 0
      }

      # cannot reach the goal yet, move on to the next intervention component
      if (expit(beta0.center + sum(beta.temp * x_max.temp) + sum(beta.other.temp * x.other.temp)) < goal) {
        next
      }

      if (expit(beta0.center + sum(beta.temp * x_max.temp) + sum(beta.other.temp * x.other.temp)) == goal) {
        est_rec_int[pos_eff][order.effect][1:i] <- x_max.temp
        est_rec_int[pos_eff][order.effect][1:i] <- x_max.temp
        break
      }

      if (i == 1) {
        est_rec_int[pos_eff][order.effect][i] <- (logit(goal) - beta0.center - sum(x.other.temp * beta.other.temp)) / beta.temp[i]
        break
      } else {
        est_rec_int[pos_eff][order.effect][1:(i - 1)] <- x_max.temp[-i]
        # the top i-1 gets their maximal values
        est_rec_int[pos_eff][order.effect][i] <- (logit(goal) - sum(beta.temp[1:(i - 1)] * x_max.temp[1:(i - 1)]) - beta0.center - sum(x.other.temp * beta.other.temp)) / beta.temp[i]
        break
      }
    }
  }

  return(list(est_rec_int = est_rec_int, goal = goal, est_reachable_goal = est_reachable_goal))
}
