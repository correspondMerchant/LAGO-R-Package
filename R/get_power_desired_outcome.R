get_power_desired_outcome <- function(
    data,
    intervention_components_coeff,
    power_goal,
    power_goal_approach,
    num_centers_in_next_stage,
    patients_per_center_in_next_stage,
    outcome_name,
    icc = NULL,
    power_goal_cluster_id = NULL) {
  # ---------------------------------------------------------------------------
  # Design effects (issue #29). When icc is NULL the design effects are all 1
  # and every variance below reduces bit-for-bit to the independent-binomial
  # p(1-p)/N form, preserving the pre-icc behavior.
  #
  # Each arm's variance uses stage-specific design effects:
  #   DE1 = 1 + (m1 - 1) * icc   (stage-1 cluster size m1, size-biased mean)
  #   DE2 = 1 + (n2j - 1) * icc  (planned next-stage cluster size n2j)
  # applied per the LAGO power paper (arXiv 2509.11479):
  #   - unconditional (Theorem 1, stage-1 random): both stages are clustered,
  #     Var_a = p_a(1-p_a) * (DE1_a*n_a1 + DE2_a*n_a2) / N_a^2
  #   - conditional (Theorem 2, stage-1 fixed): the stage-2 prediction variance
  #     (sigma_hat_x_2) clusters stage-2 only, while the rejection-threshold
  #     term uses the final pooled (both-stage) proportion SE.
  # ---------------------------------------------------------------------------
  n2j <- patients_per_center_in_next_stage

  # icc may be a scalar (shared) or length-2 c(control, treatment).
  if (is.null(icc)) {
    icc_ctl <- 0
    icc_int <- 0
  } else if (length(icc) == 1) {
    icc_ctl <- icc
    icc_int <- icc
  } else {
    icc_ctl <- icc[1]
    icc_int <- icc[2]
  }

  # Size-biased (variance-appropriate) mean stage-1 cluster size for one arm:
  # m1 = sum(m_i^2) / sum(m_i). Returns NA for a degenerate single-center arm.
  size_biased_cluster_size <- function(arm_data) {
    if (is.null(power_goal_cluster_id)) {
      return(NA_real_)
    }
    m <- as.numeric(table(arm_data[[power_goal_cluster_id]]))
    m <- m[m > 0]
    if (length(m) < 2) {
      return(NA_real_)
    }
    sum(m^2) / sum(m)
  }

  ctl_stage1_all <- data[data$group == "control", ]
  int_stage1_all <- data[data$group == "treatment", ]
  m1_ctl <- size_biased_cluster_size(ctl_stage1_all)
  m1_int <- size_biased_cluster_size(int_stage1_all)

  # When a design effect is requested we need a stage-1 cluster size. This is a
  # hard dependency for the unconditional path (stage-1 is random there) and for
  # the conditional rejection threshold (both-stage pooled SE). Fail loudly
  # rather than silently under-clustering stage-1.
  needs_stage1_de <- !is.null(icc) && (icc_ctl > 0 || icc_int > 0)
  if (needs_stage1_de && (is.na(m1_ctl) || is.na(m1_int))) {
    stop(paste(
      "A non-zero 'icc' requires a valid 'power_goal_cluster_id' column that",
      "identifies stage-1 centers, with at least two centers per arm, so the",
      "stage-1 design effect can be computed. Please provide it, or set",
      "icc = NULL / icc = 0."
    ))
  }

  # Design effects per arm. m1 is only used when a design effect is requested;
  # guard the NA case (icc == 0 / NULL) so DE1 stays 1.
  de1_ctl <- if (is.na(m1_ctl)) 1 else 1 + (m1_ctl - 1) * icc_ctl
  de1_int <- if (is.na(m1_int)) 1 else 1 + (m1_int - 1) * icc_int
  de2_ctl <- 1 + (n2j - 1) * icc_ctl
  de2_int <- 1 + (n2j - 1) * icc_int

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

      p1 <- (S1_1 + S1_2) / N1
      p0 <- (S0_1 + S0_2) / N0
      top <- p1 - p0
      # both-stage clustered variance (unconditional, Theorem 1). At icc = 0
      # DE1 = DE2 = 1 and each term collapses to p(1-p)/N.
      bottom_part1 <- p1 * (1 - p1) * (de1_int * n1_1 + de2_int * n1_2) / N1^2
      bottom_part2 <- p0 * (1 - p0) * (de1_ctl * n0_1 + de2_ctl * n0_2) / N0^2

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
        # no grid point reaches the required ncp: the power goal is infeasible
        # at these inputs. When a design effect is in force, say so explicitly
        # rather than silently returning an unreachable outcome.
        if (!is.null(icc)) {
          warning(paste(
            "The design effect implied by 'icc' makes the power goal",
            "infeasible for the given next-stage size: no attainable outcome",
            "reaches the required power. Returning an unreachable outcome",
            "(1); consider a larger next-stage sample, a lower power goal, or",
            "reviewing the icc."
          ))
        } else {
          message("No non-negative NCP differences found in grid search.")
        }
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

        p1 <- (S1_1 + S1_2) / N1
        p0 <- (S0_1 + S0_2) / N0
        # rejection-threshold SE: uses the final pooled proportion (eq 5), so
        # both stages are clustered here (both-stage form). DE applied inside
        # the sqrt.
        sqrt_part1 <- p1 * (1 - p1) * (de1_int * n1_1 + de2_int * n1_2) / N1^2
        sqrt_part2 <- p0 * (1 - p0) * (de1_ctl * n0_1 + de2_ctl * n0_2) / N0^2
        z_alpha_sqrt_multiply_part <- z_alpha_over_2 * sqrt(sqrt_part1 + sqrt_part2)

        mu_hat_x_2 <- n1_2 * expit_part / N1 - n0_2 * expit(beta0) / N0
        # stage-2 prediction variance: stage-1 is conditioned on (fixed), so
        # only the stage-2 increment is clustered here. DE2 applied inside the
        # sqrt (guarding against a DE^2 error on this sd-form term).
        sigma_hat_x_2 <- sqrt(
          de2_ctl * n0_2 * expit(beta0) * (1 - expit(beta0)) / N0^2 +
            de2_int * n1_2 * expit_part * (1 - expit_part) / N1^2
        )

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
        # no attainable outcome satisfies the conditional-power inequality: the
        # goal is infeasible at these inputs. Pre-icc this returned 0 silently,
        # which drops the power goal downstream via max(0, outcome_goal). When a
        # design effect is in force, warn explicitly instead of failing quietly.
        if (!is.null(icc)) {
          warning(paste(
            "The design effect implied by 'icc' makes the power goal",
            "infeasible for the given next-stage size under the conditional",
            "approach: no attainable outcome satisfies the required power.",
            "Returning 0, which does not raise the outcome goal; consider a",
            "larger next-stage sample, a lower power goal, or reviewing the",
            "icc."
          ))
        }
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
