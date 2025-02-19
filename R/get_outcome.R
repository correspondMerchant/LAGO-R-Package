get_outcome <- function(
    center_weights_for_outcome_goal,
    all_center_lvl_effects,
    beta,
    int_vector,
    center_cha_coeff_vec,
    center_cha,
    link) {
  if (link == "logit") {
    outcome <- sum(
      center_weights_for_outcome_goal *
        rje::expit(
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

  return(outcome)
}
