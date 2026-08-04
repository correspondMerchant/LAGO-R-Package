# The link functions the outcome machinery actually implements.
#
# A link belongs here only when EVERY step that consumes one handles it:
# get_outcome() below has to apply its inverse, flip_outcome_scale() below has
# to invert the "minimize" coefficient negation on its outcome scale, and
# get_confidence_set() has to build an interval on that same scale. Only
# "logit" and "identity" are handled throughout, so only those two are
# supported, and this one vector is what validate_inputs() accepts and what
# both functions below branch on. Keeping the set in a single place is what
# stops the three from drifting apart again.
#
# "probit" and "log" used to be accepted by validate_inputs() and are not here.
# Nothing implemented their inverse links, so they fell through to the identity
# branch of get_outcome() and the linear predictor was reported as though it
# were the probability or the mean. For "log" the gap is not merely unwritten
# code: with two or more center-level effects the "minimize" flip below cannot
# be written at all (see the note on flip_outcome_scale()).
supported_outcome_links <- function() {
  c("logit", "identity")
}


# The message for a link none of the outcome machinery implements. Shared by
# validate_inputs() and by the two functions below so the supported set is
# written down once and every refusal names the same set.
unsupported_link_message <- function(link) {
  paste0(
    "link=", link, ". The link option has to be one of the following: ",
    paste(supported_outcome_links(), collapse = ", "), ". Only these links ",
    "are implemented by the outcome and confidence set calculations, so any ",
    "other link would be reported on the wrong scale rather than computed."
  )
}


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

  # the linear predictor, one value per center-level effect. Computed once and
  # then mapped onto the outcome scale by the inverse link below, so the two
  # link branches can differ ONLY in that inverse and cannot come to disagree
  # about the predictor itself.
  linear_predictor <- all_center_lvl_effects +
    sum(beta * int_vector) +
    center_cha_effect +
    ifelse(length(all_center_lvl_effects) > 1, -beta[1], 0)

  # the reported outcome is a weighted mean of the inverse link of the linear
  # predictor, with weights summing to 1. An unhandled link has no inverse to
  # apply here, so it is refused rather than passed through: returning the
  # linear predictor would report it as if it were the probability or the mean
  # the caller asked for, which is a wrong number and not an error.
  if (link == "logit") {
    outcome <- sum(
      center_weights_for_outcome_goal * rje::expit(linear_predictor)
    )
  } else if (link == "identity") {
    outcome <- sum(
      center_weights_for_outcome_goal * linear_predictor
    )
  } else {
    stop(unsupported_link_message(link))
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
#   - identity: the outcome is the linear predictor, so negating the
#     coefficients negates the outcome.        flip(y) = -y
#   - logit: the outcome is a weighted mean of expit(eta) with weights summing
#     to 1, and expit(-eta) = 1 - expit(eta), so negating the coefficients
#     REFLECTS the probability about 1/2, it does not negate it.
#                                             flip(p) = 1 - p
#
# Either way flip() is an order-reversing involution on the outcome scale, so
# maximizing the flipped outcome is exactly minimizing the original one, and
# "flipped outcome >= flip(goal)" is exactly "original outcome <= goal".
# Applying it to the goal on the way in and to the estimated outcome on the
# way out is what keeps the two comparable.
#
# Every link needs its own entry here, and it is not a formality: for a "log"
# link the map cannot be written down at all once there is more than one
# center-level effect. The outcome is sum(w_i * exp(eta_i)) and the flipped
# outcome is sum(w_i * exp(-eta_i)), and two different eta vectors with the
# same outcome can have different flipped outcomes, so there is no function of
# the outcome alone to write. That is why the supported set above is a property
# of this function as much as of get_outcome(), and why an unhandled link is
# refused in both rather than defaulted to the linear case.
flip_outcome_scale <- function(value, link) {
  if (link == "logit") {
    1 - value
  } else if (link == "identity") {
    -1 * value
  } else {
    stop(unsupported_link_message(link))
  }
}
