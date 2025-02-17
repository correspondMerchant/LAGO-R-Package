get_est_reachable_outcome <- function(
    x,
    include_interaction_terms,
    intervention_components,
    main_components,
    link,
    outcome_goal,
    center_weights_for_outcome_goal,
    all_center_lvl_effects,
    beta,
    center_cha_coeff_vec,
    center_cha) {
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
        est_outcome <- sum(
            center_weights_for_outcome_goal *
                expit(
                    all_center_lvl_effects +
                        sum(beta * int_vector) +
                        center_cha_coeff_vec * center_cha +
                        ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                )
        )
    } else {
        est_outcome <- sum(
            center_weights_for_outcome_goal *
                (
                    all_center_lvl_effects +
                        sum(beta * int_vector) +
                        center_cha_coeff_vec * center_cha +
                        ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
                )
        )
    }

    return(est_outcome)
}
