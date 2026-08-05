# Unit tests for the internal guards of get_outcome(), flip_outcome_scale() and
# the fixed-effect name resolution, all of which are UNREACHABLE from
# lago_optimization().
#
# Every one of these is a live guard that no end-to-end call can exercise:
#
#   - get_outcome() and flip_outcome_scale() each refuse an unimplemented link,
#     but lago_optimization() refuses the same links earlier in
#     validate_inputs(), so the two inner refusals can be deleted with the
#     public API still green. They are not redundant: both functions are
#     reachable independently (get_outcome() is called by shrinking_method(),
#     and get_confidence_set() is exported), and passing an unhandled link
#     through reports the LINEAR PREDICTOR as though it were the probability,
#     which is #70's probit defect exactly.
#   - get_outcome() sums the weighted inverse link. mean() instead of sum()
#     divides the reported outcome by the number of center-level effects, and
#     the suite's fixed-center-effect fixtures do not surface it as a failure.
#     This is the sibling one line down from the center-characteristic sum()
#     that #70 fixed.
#   - time_effect_indicator() matches a period dummy by its EXACT name. Every
#     other fixture in the suite has at most 4 periods, so no dummy name is a
#     prefix of another and a prefix match is indistinguishable from an exact
#     one. 11 periods separate them.
#   - fixed_effect_coef_names() has a prefix-search fallback for a model whose
#     term-to-coefficient mapping could not be rebuilt, with two guards of its
#     own: the search is anchored, and it skips the names the assembly can
#     supply for itself. The mapping is available for every model the suite
#     fits, so the fallback never runs and neither guard is tested end to end.
#
# getFromNamespace() is how test-helpers.R reaches the other internals, and is
# used here for the same reason.


test_that("get_outcome() and flip_outcome_scale() refuse an unimplemented link (#70)", {
  # both are reachable without passing through validate_inputs(), which is why
  # each carries its own refusal rather than trusting its caller. Falling
  # through to the identity branch instead reports the linear predictor as the
  # probability: for the arguments below get_outcome() would return 0.9, which
  # is what a caller asking for a probability gets handed.
  get_outcome <- getFromNamespace("get_outcome", "LAGO")
  flip_outcome_scale <- getFromNamespace("flip_outcome_scale", "LAGO")

  # "cloglog" and "sqrt" are here alongside the two links #70 removed because
  # the guard is about the SET, not about those two names: any future link
  # added to glm()'s side without an inverse here has to be refused too.
  for (bad in c("probit", "log", "cloglog", "sqrt")) {
    expect_error(
      get_outcome(1, 0.2, c(0.1, 0.3), c(1, 2), 0, 0, bad),
      paste0("link=", bad),
      fixed = TRUE
    )
    expect_error(flip_outcome_scale(0.7, bad), paste0("link=", bad),
      fixed = TRUE
    )
    # the message names the links that ARE implemented, so the refusal says
    # what to use instead
    expect_error(
      get_outcome(1, 0.2, c(0.1, 0.3), c(1, 2), 0, 0, bad),
      "has to be one of the following: logit, identity"
    )
    expect_error(
      flip_outcome_scale(0.7, bad),
      "has to be one of the following: logit, identity"
    )
  }

  # the two supported links are unaffected, so this is a refusal and not a
  # blanket failure. Both values are computed by hand: the linear predictor
  # here is 0 + 0.1 * 1 + 0.3 * 2 = 0.7.
  expect_equal(
    get_outcome(1, 0, c(0.1, 0.3), c(1, 2), 0, 0, "logit"),
    rje::expit(0.7)
  )
  expect_equal(
    get_outcome(1, 0, c(0.1, 0.3), c(1, 2), 0, 0, "identity"),
    0.7
  )
  # flip() on a probability reflects it about 1/2; on the identity scale it
  # negates. Deleting the refusal would make every other link do the latter.
  expect_equal(flip_outcome_scale(0.7, "logit"), 0.3)
  expect_equal(flip_outcome_scale(0.7, "identity"), -0.7)
})


test_that("get_outcome() SUMS the weighted inverse link over center-level effects", {
  # the reported outcome is a weighted mean of the inverse link of the linear
  # predictor, with weights that sum to 1, so it must lie between the smallest
  # and the largest inverse-link value. mean() in place of sum() divides it by
  # the number of center-level effects, which for the three centers below sends
  # 0.673 to 0.224: a number outside the range of the quantities it averages,
  # and a wrong reported probability rather than an error.
  #
  # Only visible with MORE THAN ONE center-level effect, which is why the
  # single-center fixtures elsewhere cannot see it. This is the same defect one
  # line down from the center-characteristic product that #70 fixed.
  get_outcome <- getFromNamespace("get_outcome", "LAGO")

  weights <- c(0.5, 0.3, 0.2)
  center_effects <- c(0.1, 0.4, -0.2)
  beta <- c(0.1, 0.3)
  int_vector <- c(1, 2)
  # the per-center linear predictors, built the way get_outcome() builds them:
  # the center-level effect, plus sum(beta * int_vector), plus the
  # center-characteristic term (0 here), minus beta[1] because there is more
  # than one center-level effect and the intercept is already inside each one.
  eta <- center_effects + sum(beta * int_vector) - beta[1]

  logit_out <- get_outcome(
    weights, center_effects, beta, int_vector, 0, 0, "logit"
  )
  expect_equal(logit_out, sum(weights * rje::expit(eta)), tolerance = 1e-12)
  # the invariant, which is what catches a future reduction that is wrong in
  # some other way than a division: a weighted mean with weights summing to 1
  # is bracketed by the values it averages.
  expect_equal(sum(weights), 1)
  expect_gte(logit_out, min(rje::expit(eta)))
  expect_lte(logit_out, max(rje::expit(eta)))

  # the identity branch reduces the same way over the same predictors, so it is
  # held to the same requirement
  identity_out <- get_outcome(
    weights, center_effects, beta, int_vector, 0, 0, "identity"
  )
  expect_equal(identity_out, sum(weights * eta), tolerance = 1e-12)
  expect_gte(identity_out, min(eta))
  expect_lte(identity_out, max(eta))

  # a SINGLE center-level effect is unchanged, since sum() and mean() agree on
  # one element. That is the common case, and it is why this is invisible from
  # the outside without a multi-center fixture.
  expect_equal(
    get_outcome(1, 0.1, beta, int_vector, 0, 0, "logit"),
    rje::expit(0.1 + sum(beta * int_vector))
  )
})


test_that("a period dummy is matched exactly, so period 1 is not period 10", {
  # glm() names a period dummy with the term name followed by the level, so the
  # dummy of level "1" is exactly "period1". With fewer than 10 periods no
  # dummy name is a prefix of another and a prefix match behaves identically to
  # an exact one, which is why every fixture in the suite (at most 4 periods)
  # is blind to the difference. With 11 periods "period1" is a prefix of
  # "period10" and of nothing else, so a prefix search returns two hits, the
  # length(hits) == 1 guard rejects, and requesting period 1 fails outright on
  # a study that is perfectly well specified.
  time_effect_indicator <- getFromNamespace("time_effect_indicator", "LAGO")

  d <- data.frame(
    y = rep(0:1, 110),
    x = seq_len(220),
    period = factor(rep(0:10, each = 20))
  )
  model <- suppressWarnings(glm(y ~ x + period, data = d, family = binomial()))
  dummies <- grep("^period", names(coef(model)), value = TRUE)
  # the fixture has to contain a dummy name that is a prefix of another one, or
  # there is nothing for an inexact match to trip over
  expect_length(dummies, 10)
  expect_true("period1" %in% dummies && "period10" %in% dummies)

  # exactly one 1, on the requested period's own dummy
  expect_equal(time_effect_indicator(model, dummies, 1), c(1, rep(0, 9)))
  expect_equal(time_effect_indicator(model, dummies, 10), c(rep(0, 9), 1))
  expect_equal(time_effect_indicator(model, dummies, 2), c(0, 1, rep(0, 8)))
  # the reference period is level 0, which glm() left out of the dummies on
  # purpose, so it is all zeros rather than a failure to match
  expect_equal(time_effect_indicator(model, dummies, 0), rep(0, 10))

  # every requested period selects exactly one dummy, or none for the reference
  for (p in 0:10) {
    indicator <- time_effect_indicator(model, dummies, p)
    expect_equal(sum(indicator), if (p == 0) 0 else 1, info = paste("period", p))
    expect_true(all(indicator %in% c(0, 1)), info = paste("period", p))
  }

  # a value that is not one of the model's periods raises rather than silently
  # standing for the reference period
  expect_error(
    time_effect_indicator(model, dummies, 11),
    "is not one of the .*periods the outcome model was fitted on"
  )
})


test_that("the fixed-effect fallback is anchored and skips the named predictors", {
  # fixed_effect_coef_names() resolves the "center" and "period" terms through
  # the model's term-to-coefficient mapping, and falls back to a prefix search
  # when there is no mapping. The fallback has two guards, and both are dead
  # code from the outside because every model the suite fits yields a mapping:
  # coef_mapping = NULL below is what takes the fallback directly.
  #
  # This is the #68 center_size defect one layer down. Unanchored, the search
  # claims any coefficient with "center" or "period" ANYWHERE in its name; and
  # without the named_predictors exclusion it claims a covariate or center
  # characteristic whose own name merely starts that way, which is the case
  # that recommended an intervention whose true outcome was 0.428 while
  # reporting 0.6.
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )

  # center_size and period_flag are named predictors: the assembly supplies
  # their columns itself, so the fallback must not also claim them as dummies.
  # my_center_effect and the_period_thing contain the term name but do not
  # start with it, so only an unanchored search claims them.
  model_coef_names <- c(
    "(Intercept)", "coaching_updt", "center_size", "period_flag",
    "center2", "center3", "period1", "period2",
    "my_center_effect", "the_period_thing"
  )
  named_predictors <- c(
    "(Intercept)", "coaching_updt", "center_size", "period_flag"
  )

  expect_equal(
    fixed_effect_coef_names(
      "center", NULL, model_coef_names, named_predictors
    ),
    c("center2", "center3")
  )
  expect_equal(
    fixed_effect_coef_names(
      "period", NULL, model_coef_names, named_predictors
    ),
    c("period1", "period2")
  )

  # the mapping, when there is one, wins outright: the fallback is not consulted
  # at all, so a coefficient the mapping does not list is not picked up by the
  # prefix search behind its back.
  mapping <- list(
    center = c("center2", "center3"), period = c("period1", "period2")
  )
  expect_equal(
    fixed_effect_coef_names(
      "center", mapping, model_coef_names, named_predictors
    ),
    c("center2", "center3")
  )
  # a model with no fixed effects at all resolves to nothing, which is what
  # get_confidence_set() turns into its "no fixed time effect coefficient could
  # be identified" refusal rather than a silent empty block.
  expect_length(
    fixed_effect_coef_names(
      "period", NULL, c("(Intercept)", "coaching_updt"),
      c("(Intercept)", "coaching_updt")
    ),
    0
  )
})
