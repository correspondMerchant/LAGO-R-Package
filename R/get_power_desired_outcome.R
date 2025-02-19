get_power_desired_outcome <- function(
    data,
    intervention_components_coeff,
    power_goal,
    power_goal_approach,
    num_centers_in_next_stage,
    patients_per_center_in_next_stage,
    outcome_name) {
  ##################################
  ## unconditional power approach ##
  ##################################
  if (power_goal_approach == "unconditional") {
    # helper function to get the power difference
    get_power_diff <- function(ncp, alpha, df, power_goal) {
      critical_value <- qchisq(1 - alpha, df)
      theoretical_power <- 1 - pchisq(
        critical_value,
        df,
        ncp,
        lower.tail = TRUE
      )
      return(theoretical_power - power_goal)
    }

    # helper function to get the minimum required ncp
    get_min_req_ncp <- function(alpha, df, power_goal) {
      root <- uniroot(get_power_diff, c(0, 100),
        alpha = alpha, df = df,
        power_goal = power_goal
      )
      return(root$root)
    }

    # helper function to get the ncp difference
    get_ncp_diff <- function(expit_part,
                             desired_ncp,
                             j, # number of centers in the next stage
                             n2j, # number of patients per center
                             ctl_data_stage1,
                             int_data_stage1,
                             beta0) {
      n0_2 <- j / 2 * n2j
      n1_2 <- j / 2 * n2j
      n0_1 <- nrow(ctl_data_stage1)
      n1_1 <- nrow(int_data_stage1)
      S0_1 <- sum(ctl_data_stage1[[outcome_name]])
      S1_1 <- sum(int_data_stage1[[outcome_name]])
      N0 <- n0_1 + n0_2
      N1 <- n1_1 + n1_2

      S1_2 <- n1_2 * expit_part
      S0_2 <- n0_2 * rje::expit(beta0)

      top <- (S1_1 + S1_2) / N1 - (S0_1 + S0_2) / N0
      bottom_part1 <- ((S1_1 + S1_2) / N1) * (1 - ((S1_1 + S1_2) / N1)) / N1
      bottom_part2 <- ((S0_1 + S0_2) / N0) * (1 - ((S0_1 + S0_2) / N0)) / N0

      if (bottom_part1 + bottom_part2 < 0) {
        # there is no point taking the sqrt of a negative number
        return(-1)
      }
      bottom <- sqrt(bottom_part1 + bottom_part2)
      calculated_ncp <- (top / bottom)^2
      return(calculated_ncp - desired_ncp)
    }

    # unconditional power approach
    uncond_get_desired_outcome <- function(desired_ncp,
                                           j,
                                           n2j,
                                           ctl_data_stage1,
                                           int_data_stage1,
                                           lower = 0,
                                           upper = 1.5,
                                           n_points = 1000,
                                           beta0) {
      grid_for_expit_part <- seq(lower, upper, length.out = n_points)

      ncp_diffs <- sapply(grid_for_expit_part, function(expit_part) {
        get_ncp_diff(
          expit_part,
          desired_ncp,
          j,
          n2j,
          ctl_data_stage1,
          int_data_stage1,
          beta0
        )
      })

      # Find the index of the minimum positive difference
      # (calculated_ncp - desired_ncp)
      pos_diff_idx <- which(ncp_diffs >= 0)

      if (length(pos_diff_idx) == 0) {
        message("No non-negative NCP differences found in grid search.")
        return(1)
      }

      ctl_percentage <- sum(ctl_data_stage1[[outcome_name]]) / nrow(ctl_data_stage1)
      pos_grid_for_expit_part <- grid_for_expit_part[pos_diff_idx]

      return((pos_grid_for_expit_part[pos_grid_for_expit_part > ctl_percentage])[1])
    }

    min_ncp_solution <- get_min_req_ncp(
      alpha = 0.05,
      df = 1,
      power_goal
    )

    desired_outcome <- uncond_get_desired_outcome(
      min_ncp_solution,
      num_centers_in_next_stage,
      patients_per_center_in_next_stage,
      data[data$group == "control", ],
      data[data$group == "treatment", ],
      lower = rje::expit(intervention_components_coeff[1]),
      upper = 1,
      n_points = 1000,
      intervention_components_coeff[1]
    )
  } else if (power_goal_approach == "conditional") {
    #################################
    ## conditional power approach  ##
    #################################
    cond_get_desired_outcome <- function(j,
                                         n2j,
                                         ctl_data_stage1,
                                         int_data_stage1,
                                         power_goal,
                                         lower = 0,
                                         upper = 1.5,
                                         n_points = 1000,
                                         beta0) {
      n0_2 <- j / 2 * n2j
      n1_2 <- j / 2 * n2j
      n0_1 <- nrow(ctl_data_stage1)
      n1_1 <- nrow(int_data_stage1)
      S0_1 <- sum(ctl_data_stage1[[outcome_name]])
      S1_1 <- sum(int_data_stage1[[outcome_name]])
      N0 <- n0_1 + n0_2
      N1 <- n1_1 + n1_2


      z_alpha_over_2 <- qnorm(1 - 0.05 / 2)
      minus_z_pi <- qnorm(power_goal)

      grid_for_expit_part <- seq(lower, upper, length.out = n_points)

      conditional_power_function <- function(expit_part) {
        S1_2 <- n1_2 * expit_part
        S0_2 <- n0_2 * expit(beta0)

        sqrt_part1 <- (S1_1 + S1_2) / N1 * (1 - (S1_1 + S1_2) / N1) / N1
        sqrt_part2 <- (S0_1 + S0_2) / N0 * (1 - (S0_1 + S0_2) / N0) / N0
        z_alpha_sqrt_multiply_part <- z_alpha_over_2 * sqrt(sqrt_part1 + sqrt_part2)

        mu_hat_x_2 <- n1_2 * expit_part / N1 - n0_2 * expit(beta0) / N0
        sigma_hat_x_2 <- sqrt(n0_2 * expit(beta0) * (1 - expit(beta0)) / N0^2 + n1_2 * expit_part * (1 - expit_part) / N1^2)

        equation_result <- z_alpha_sqrt_multiply_part - S1_1 / N1 + S0_1 / N0 - mu_hat_x_2 + minus_z_pi * sigma_hat_x_2
        return(equation_result)
      }

      suppressWarnings({
        pwr_equation_values <- na.omit(
          sapply(grid_for_expit_part, conditional_power_function)
        )
      })

      all_possible_expit_part_values <- grid_for_expit_part[pwr_equation_values <= 0]

      if (length(all_possible_expit_part_values) > 0) {
        final_expit_value <- all_possible_expit_part_values[1]
      } else {
        final_expit_value <- 0
      }
      return(final_expit_value)
    }

    desired_outcome <- cond_get_desired_outcome(
      num_centers_in_next_stage,
      patients_per_center_in_next_stage,
      data[data$group == "control", ],
      data[data$group == "treatment", ],
      power_goal,
      lower = rje::expit(intervention_components_coeff[1]),
      upper = 1,
      n_points = 1000,
      intervention_components_coeff[1]
    )
  }

  return(desired_outcome)
}
