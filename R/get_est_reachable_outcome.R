get_est_reachable_outcome <- function(
    x,
    include_interaction_terms,
    intervention_components,
    main_components,
    link,
    center_weights_for_outcome_goal,
    all_center_lvl_effects,
    beta,
    center_cha_coeff_vec,
    center_cha) {
  int_vector <- get_int_vector(
    include_interaction_terms,
    intervention_components,
    main_components,
    x
  )

  est_outcome <- get_outcome(
    center_weights_for_outcome_goal,
    all_center_lvl_effects,
    beta,
    int_vector,
    center_cha_coeff_vec,
    center_cha,
    link
  )

  return(est_outcome)
}
