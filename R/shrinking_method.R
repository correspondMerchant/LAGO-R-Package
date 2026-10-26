# use Nevo web appendix section 5.1 to calculate the recommended
# intervention, as a way to keep it as a continuous function of
# the stage 1 estimated betas.

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
  n_components <- ifelse(
    include_interaction_terms,
    length(main_components),
    length(intervention_components)
  )
  # the position in beta of each main component's own coefficient, since beta
  # follows intervention_components, which may list an interaction term first
  coef_index <- if (include_interaction_terms) {
    1 + match(gsub("`", "", main_components), gsub("`", "", intervention_components))
  } else {
    1 + seq_len(n_components)
  }
  # a main component only inside an interaction term has no coefficient of its own
  beta_vec <- beta[coef_index]
  beta_vec[is.na(coef_index)] <- 0
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
    beta_modified[coef_index[current_component]] <- beta_value

    return(get_outcome(
      center_weights_for_outcome_goal,
      all_center_lvl_effects,
      beta_modified,
      int_vector,
      center_cha_coeff_vec,
      center_cha,
      link
    ))
  }

  # Main optimization loop
  for (c in 1:n_components) {
    # with a coefficient of 0 the shrinking formula keeps the stage-1 value
    if (is.na(coef_index[c])) {
      recommended_values[c] <- stage_1_intervention[c]
      next
    }
    other_indices <- setdiff(1:n_components, c)
    other_boundaries <- ifelse(beta_vec[other_indices] < 0,
      lo[other_indices],
      up[other_indices]
    )

    # Fix X at upper boundary
    X_fixed <- up[c]

    beta_max <- find_needed_coefficient(function(b) {
      calculate_effect(b, X_fixed, c, other_boundaries, beta_vec[other_indices])
    }, outcome_goal)

    # beta_max is the coefficient this component would need, at its upper
    # bound, to reach the outcome goal on its own. The recommendation is an
    # interpolation between the two ENDPOINTS below, indexed by how far
    # beta_vec[c] has travelled along [beta_min, beta_max]:
    #
    #   beta_vec[c] == beta_min  ->  stage_1_intervention[c]
    #   beta_vec[c] == beta_max  ->  up[c]
    #
    # (substitute either into the interpolation and it returns exactly that
    # endpoint). Both endpoints are inside [lo, up]: up[c] is a bound, and the
    # stage-1 intervention is one too, since a user-supplied
    # prev_recommended_interventions is validated against the bounds and the
    # colMeans() default it falls back to is projected onto them. So the
    # interpolation stays inside the bounds exactly as long as its fraction
    # stays in [0, 1], i.e. as long as beta_vec[c] stays inside the bracket,
    # and that is what the three branches below enforce.
    #
    # beta_min = beta_max / 2 is "half as effective as needed", which only
    # brackets beta_max from below while beta_max > 0. Under the "minimize"
    # direction the fitted coefficients are negated (see lago_optimization()),
    # so beta_max is routinely <= 0: halving it then moves it AWAY from zero's
    # side and the bracket is empty or inverted, beta_max - beta_min <= 0. The
    # interpolation has no meaning on an empty bracket -- with a negative width
    # it runs backwards, away from up[c] and out through lo[c], and with a zero
    # width it divides by zero and returns a non-finite intervention. There is
    # no fraction to compute in that case, so the fallback the caller warned
    # about, the stage-1 intervention itself, is what is returned. This is
    # decided from the bracket the binary search produced rather than from the
    # sign of any coefficient, so it does not depend on the optimization
    # direction: an inverted bracket is refused the same way whichever
    # direction produced it.
    beta_min <- beta_max / 2
    if (beta_max - beta_min <= 0 || beta_vec[c] <= beta_min) {
      # at or below the bottom of the bracket, or no usable bracket at all
      recommended_values[c] <- stage_1_intervention[c]
    } else if (beta_vec[c] >= beta_max) {
      # at or above the top of the bracket. The interpolation's value at
      # beta_max is up[c], and extrapolating past it would recommend more of
      # the component than the upper bound allows.
      recommended_values[c] <- up[c]
    } else {
      slope <- (up[c] - stage_1_intervention[c]) / (beta_max - beta_min)
      recommended_values[c] <- stage_1_intervention[c] +
        slope * (beta_vec[c] - beta_min)
    }
  }

  return(recommended_values)
}

# The coefficient at which effect_at(), which rises with it, reaches the goal,
# by bisection. The search starts in [-100, 100]. A needed coefficient above
# 100, which depends on the outcome's units, doubles the top of the bracket
# until it holds it. A needed coefficient below -100 leaves no usable bracket
# either way. Returns the midpoint that reaches the goal, or else the lower end
# of the final bracket.
find_needed_coefficient <- function(effect_at, outcome_goal) {
  left <- -100
  right <- 100
  for (k in 1:60) {
    if (effect_at(right) >= outcome_goal) break
    right <- right * 2
  }
  while (abs(right - left) > 1e-6) {
    mid <- (left + right) / 2
    # at a very large scale the bracket can stop shrinking before 1e-6
    if (mid <= left || mid >= right) break
    effect <- effect_at(mid)
    if (abs(effect - outcome_goal) < 1e-6) {
      return(mid)
    } else if (effect < outcome_goal) {
      left <- mid
    } else {
      right <- mid
    }
  }
  left
}
