get_outcome <- function(
    center_weights_for_outcome_goal,
    all_center_lvl_effects,
    beta,
    int_vector,
    center_cha_coeff_vec,
    center_cha,
    link) {
  # The center-characteristic contribution is a single number: the inner
  # product of the characteristics' coefficients with the values the
  # recommendation is computed at. It is summed for the same reason
  # sum(beta * int_vector) is. Left as the elementwise product it stayed a
  # VECTOR as soon as there were two or more center characteristics, and it
  # then recycled against all_center_lvl_effects (which is per center-level
  # effect, not per characteristic), silently producing a wrong outcome. With
  # exactly one characteristic the product is already length one, so sum() is
  # a no-op there and that common case is unchanged.
  center_cha_effect <- sum(center_cha_coeff_vec * center_cha)

  if (link == "logit") {
    outcome <- sum(
      center_weights_for_outcome_goal *
        rje::expit(
          all_center_lvl_effects +
            sum(beta * int_vector) +
            center_cha_effect +
            ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
        )
    )
  } else {
    outcome <- sum(
      center_weights_for_outcome_goal *
        (
          all_center_lvl_effects +
            sum(beta * int_vector) +
            center_cha_effect +
            ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)
        )
    )
  }

  return(outcome)
}


# The "minimize" direction is implemented by negating the fitted coefficients
# (see lago_optimization()), which turns "reach an outcome at most as large as
# the goal" into the maximization problem every optimizer here already solves.
# This is the map that negation induces on the OUTCOME scale, and it is the
# one place that map is written down.
#
# It is NOT always a negation, because get_outcome() applies the inverse link:
#   - identity (and any non-logit link, which get_outcome treats linearly):
#     the outcome is the linear predictor, so negating the coefficients
#     negates the outcome.        flip(y) = -y
#   - logit: the outcome is a weighted mean of expit(eta) with weights summing
#     to 1, and expit(-eta) = 1 - expit(eta), so negating the coefficients
#     REFLECTS the probability about 1/2, it does not negate it.
#                                 flip(p) = 1 - p
#
# Either way flip() is an order-reversing involution on the outcome scale, so
# maximizing the flipped outcome is exactly minimizing the original one, and
# "flipped outcome >= flip(goal)" is exactly "original outcome <= goal".
# Applying it to the goal on the way in and to the estimated outcome on the
# way out is what keeps the two comparable.
flip_outcome_scale <- function(value, link) {
  if (link == "logit") {
    1 - value
  } else {
    -1 * value
  }
}
