# if stage 2 goal cannot be reached, needs to use Nevo web appendix
# section 5.1 to calculate the recommended intervention, as a way to keep
# it as a continuous function of the stage 1 estimated betas.

shrinking_method <- function(
    lo,
    up,
    beta,
    outcome_goal,
    include_interaction_terms,
    intervention_components,
    main_components,
    all_center_lvl_effects,
    center_weights_for_outcome_goal,
    center_cha_coeff_vec,
    center_cha,
    link,
    stage_1_intervention) {
    beta_vec <- beta[-1]
    n_components <- ifelse(
        include_interaction_terms,
        length(main_components),
        length(intervention_components)
    )
    recommended_values <- rep(0, n_components)

    # Helper function to create interaction vector
    create_interaction_vector <- function(x) {
        if (!include_interaction_terms) {
            return(c(1, x))
        }

        int_vector <- numeric(length(intervention_components))
        for (i in seq_along(intervention_components)) {
            components <- strsplit(gsub("`", "", intervention_components[i]), ":")[[1]]
            if (length(components) == 1) {
                idx <- which(main_components == components[1])
                int_vector[i] <- x[idx]
            } else {
                prod_result <- 1
                for (comp in components) {
                    idx <- which(main_components == comp)
                    prod_result <- prod_result * x[idx]
                }
                int_vector[i] <- prod_result
            }
        }
        c(1, int_vector)
    }

    # Helper function to calculate effect
    calculate_effect <- function(beta_value, X_value, current_component,
                                 other_boundaries, other_betas) {
        x <- numeric(n_components)
        x[current_component] <- X_value
        x[setdiff(1:n_components, current_component)] <- other_boundaries

        int_vector <- create_interaction_vector(x)
        beta_modified <- beta
        beta_modified[current_component + 1] <- beta_value

        if (link == "logit") {
            return(sum(
                center_weights_for_outcome_goal *
                    expit(
                        all_center_lvl_effects +
                            sum(beta_modified * int_vector) +
                            center_cha_coeff_vec * center_cha +
                            ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                    )
            ))
        } else {
            return(sum(
                center_weights_for_outcome_goal *
                    (
                        all_center_lvl_effects +
                            sum(beta_modified * int_vector) +
                            center_cha_coeff_vec * center_cha +
                            ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                    )
            ))
        }
    }

    # Main optimization loop
    for (c in 1:n_components) {
        other_indices <- setdiff(1:n_components, c)
        other_boundaries <- ifelse(beta_vec[other_indices] < 0,
            lo[other_indices],
            up[other_indices]
        )

        # Fix X at upper boundary
        X_fixed <- up[c]

        # Binary search for beta_max
        left <- -100 # reasonable starting range for beta
        right <- 100 # reasonable starting range for beta

        while (abs(right - left) > 1e-6) {
            mid <- (left + right) / 2
            effect <- calculate_effect(
                mid, X_fixed, c,
                other_boundaries, beta_vec[other_indices]
            )

            if (abs(effect - outcome_goal) < 1e-6) {
                beta_max <- mid
                break
            } else if (effect < outcome_goal) {
                left <- mid
            } else {
                right <- mid
            }
        }

        beta_max <- left

        # Calculate beta min and recommended value
        beta_min <- beta_max / 2
        if (beta_vec[c] <= beta_min) {
            recommended_values[c] <- stage_1_intervention[c]
        } else {
            slope <- (up[c] - stage_1_intervention[c]) / (beta_max - beta_min)
            recommended_values[c] <- stage_1_intervention[c] +
                slope * (beta_vec[c] - beta_min)
        }
    }

    return(recommended_values)
}
