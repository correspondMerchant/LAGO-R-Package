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


#' warn_if_outcome_outside_range
#'
#' @description Non-fatal check that the estimated outcome REPORTED for a
#' binary outcome is a probability, i.e. that it lies in [0, 1]. Warns, never
#' raises, and never alters a value.
#'
#' @details A binary outcome's estimate is a probability, and on the "logit"
#' link it is expit() of the linear predictor and is inside [0, 1] by
#' construction. On the "identity" link the model is a linear probability model
#' and the estimate IS the linear predictor, so it is not confined to anything.
#' A fit whose every fitted value on the DATA is a probability still
#' extrapolates outside [0, 1] at an intervention outside the range its
#' components were fitted over, which is exactly what intervention bounds
#' reaching beyond that range ask for. glm() does not object, because it only
#' ever sees the data. So "Estimated outcome: 1.5351" was reported for a
#' binary outcome with no error and no warning.
#'
#' WARN, rather than refuse or clamp, and the two rejected options are worth
#' recording because neither is harmless.
#'
#' Refusing would reject a fit that is legitimate over its own data range: a
#' linear probability model is a defensible choice, lago_optimization()
#' accepts it deliberately, and the estimate is only out of range for
#' interventions outside the observed support.
#'
#' Clamping the estimate would be worse than either. get_outcome() is what
#' every optimizer and the goal comparison are driven by, so clamping it there
#' would change WHICH intervention is recommended and would flatten the
#' objective above the boundary, turning a reporting problem into an
#' optimization one. Clamping only the reported copy would make the reported
#' outcome disagree with the value the recommendation was chosen by, i.e. two
#' wrong numbers instead of one. Nothing here changes a value.
#'
#' NOT placed inside get_outcome(), which is the single place the estimate is
#' produced, for two reasons that are each sufficient. First, outcome_type does
#' not reach it and neither do its callers carry one, so it cannot know that
#' [0, 1] is the right range and a guard there would fire on a continuous
#' outcome, whose range is not knowable. Second, and decisively, get_outcome()
#' is evaluated on the FLIPPED outcome scale under
#' outcome_goal_intention = "minimize", where the identity-link flip is a
#' negation: on a "minimize" run reporting a perfectly valid 0.0422, all
#' eleven values get_outcome() returned were negative. A guard there would have
#' to be threaded outcome_type AND lower_outcome_goal to say anything true, and
#' it is called once per grid point besides, so it could not warn once per run.
#' The check therefore belongs where the reported value exists on the caller's
#' own outcome scale and outcome_type is already in scope, which is
#' lago_optimization(), and it is called there once.
#'
#' The reported interval is mentioned but does not itself trigger the warning.
#' On the identity link get_confidence_set() deliberately does not confine the
#' interval, because the estimate it belongs to is unconfined and confining
#' only the interval would report an interval excluding its own estimate. That
#' decision and this warning are the same statement from two sides: neither
#' alters a number, and the user is told that what is reported is not a
#' probability. Triggering on a bound alone would re-open a decision already
#' taken, so the trigger is the estimate and the bounds are counted only to say
#' how far the report is affected.
#'
#' @param est_outcome A numeric value, the estimated outcome as reported, on
#' the caller's own outcome scale.
#' @param outcome_type A character string, "binary" or "continuous". Only a
#' binary outcome has a knowable range, so a continuous one returns at once.
#' @param link A character string, the link the outcome model was fitted on.
#' Used to name the mechanism, not to decide the condition: the condition is
#' whether the reported value is a probability, which is the property that is
#' violated.
#' @param reported_ci A numeric vector of the reported interval bounds at the
#' recommended intervention, or NULL when no confidence set was requested.
#' @param cs_rows The reported confidence set as a data.frame with
#' CI_lower_bound and CI_upper_bound columns, or NULL when there is none.
#'
#' @return Invisibly NULL. Called for its side effect of issuing one warning.
#'
#' @noRd
warn_if_outcome_outside_range <- function(est_outcome,
                                         outcome_type,
                                         link,
                                         reported_ci = NULL,
                                         cs_rows = NULL) {
  # a continuous outcome's range is not knowable here, which is the same reason
  # get_confidence_set() does not confine its interval.
  if (!identical(outcome_type, "binary")) {
    return(invisible(NULL))
  }
  # a non-finite value is not a range violation and is reported, or refused,
  # elsewhere. NULL and length-zero inputs fall out of this as FALSE.
  outside_unit_range <- function(values) {
    is.finite(values) & (values < 0 | values > 1)
  }
  if (!any(outside_unit_range(est_outcome))) {
    return(invisible(NULL))
  }

  # how much of the rest of the report is affected, which is cheap to say and
  # tells the user whether the headline number is the only one. Counted over
  # the bounds as REPORTED, i.e. after rounding, so the count matches what the
  # user can see rather than an unrounded value they cannot.
  reported_bounds <- c(
    reported_ci,
    if (!is.null(cs_rows)) {
      c(cs_rows$CI_lower_bound, cs_rows$CI_upper_bound)
    }
  )
  n_bounds_outside <- sum(outside_unit_range(reported_bounds))
  bounds_sentence <- if (n_bounds_outside > 0) {
    paste0(
      " ", n_bounds_outside, " reported confidence interval bound(s) are ",
      "outside [0, 1] as well, and are likewise reported as computed: the ",
      "interval is not confined on this link, because confining it around an ",
      "unconfined estimate would report an interval excluding its own ",
      "estimate."
    )
  } else {
    ""
  }

  # the mechanism, named from the link rather than assumed, so the sentence is
  # true for whichever link produced the value. Only "identity" can reach here
  # in practice, since expit() cannot leave [0, 1], but a caller passing its
  # own link should not be told about a model it did not fit.
  mechanism <- if (link == "identity") {
    paste0(
      "The outcome model was fitted with link = \"identity\", so it is a ",
      "linear probability model whose estimated outcome is the linear ",
      "predictor itself and is not confined to [0, 1]. A fit whose every ",
      "fitted value on the data is a probability still extrapolates outside ",
      "[0, 1] at an intervention beyond the range its components were fitted ",
      "over, which is what intervention bounds reaching past that range ask ",
      "for."
    )
  } else {
    paste0(
      "The outcome model was fitted with link = \"", link, "\", whose inverse ",
      "did not confine the estimated outcome to [0, 1]."
    )
  }

  # format() to enough digits that the printed value cannot round to a number
  # inside [0, 1]: signif(1.0000004, 6) is 1, which would read "the estimated
  # outcome is 1, which is outside [0, 1]", contradicting itself. The value is
  # only just outside the range in that case, but the message must not say a
  # thing and its negation.
  warning(paste0(
    "The estimated outcome is ", format(est_outcome, digits = 15),
    ", which is outside ",
    "[0, 1] and so is not a probability, while the outcome is binary. ",
    mechanism,
    bounds_sentence,
    "\nThe LAGO optimization still ran and the recommended intervention is ",
    "the one the fitted model implies, so no reported value has been altered ",
    "to fit the range. Please do not read the estimated outcome as a ",
    "probability. Consider narrowing the intervention bounds to the range ",
    "the data covers, or fitting the outcome model with link = \"logit\", ",
    "whose estimated outcome is a probability by construction."
  ))

  invisible(NULL)
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
