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
#   - select_restart_within_bounds() projects the chosen restart onto the
#     intervention bounds and recomputes its cost there. Both only do anything
#     when EVERY restart left the box, which needs solnl() to stop a tolerance
#     outside every one of its bounds on every restart. The three-component
#     integration fixture in test-minimize-and-bounds.R does reach it, but only
#     because most of its restarts miss the box by around 1e-16, so a solver
#     that converged more tightly would disarm it silently. Called directly with
#     a hand-built restart matrix there is no solver to depend on.
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


test_that("both callers pass the names they have already claimed", {
  # The guard above is a PARAMETER, so it has two halves: the helper filtering
  # against the list, and the caller building a list to filter against. The
  # helper half is covered above. This is the caller half, which
  # rec_int_processor() used to defeat by passing character(0): a list that
  # excludes nothing makes the helper's exclusion a no-op, so the two are not
  # independent guards and covering one does not cover the other.
  #
  # The exclusion list each caller owes is the names it looks up on its own
  # account: the intercept, the intervention components, the additional
  # covariates and the center characteristics. Built here the way both callers
  # build it, over the coefficient names glm() actually produces for a fit with
  # real center and period dummies alongside a center_size covariate and a
  # period_flag covariate.
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")

  bb <- as.data.frame(BB_data)
  bb$center <- factor(rep_len(paste0("c", 1:3), nrow(bb)))
  bb$period <- factor(rep_len(1:3, nrow(bb)))
  bb$center_size <- bb$staff_nurse
  bb$period_flag <- bb$distance_10
  model <- suppressWarnings(glm(
    pp3_oxytocin_mother ~ center + period + coaching_updt +
      launch_duration + center_size + period_flag,
    data = bb, family = binomial()
  ))
  model_coef_names <- names(coef(model))
  # the fixture has to contain the confusable names, or there is nothing for an
  # empty exclusion list to wrongly claim
  expect_true(all(c("centerc2", "centerc3", "period2", "period3",
    "center_size", "period_flag") %in% model_coef_names))

  # the list, built exactly as both call sites build it
  named_predictors <- gsub("`", "", c(
    "(Intercept)", c("coaching_updt", "launch_duration"),
    c("center_size", "period_flag"), NULL
  ))

  # with the list the callers owe, only the real dummies are claimed
  expect_equal(
    fixed_effect_coef_names(
      "center", NULL, model_coef_names, named_predictors
    ),
    c("centerc2", "centerc3")
  )
  expect_equal(
    fixed_effect_coef_names(
      "period", NULL, model_coef_names, named_predictors
    ),
    c("period2", "period3")
  )

  # and EMPTIED, which is what rec_int_processor() used to pass, the covariates
  # are claimed as dummies. This is the assertion that fails if either call site
  # regresses to character(0).
  expect_equal(
    fixed_effect_coef_names(
      "center", NULL, model_coef_names, character(0)
    ),
    c("centerc2", "centerc3", "center_size")
  )
  expect_equal(
    fixed_effect_coef_names(
      "period", NULL, model_coef_names, character(0)
    ),
    c("period2", "period3", "period_flag")
  )
  # the two disagree, so the argument is load-bearing rather than incidental
  expect_false(identical(
    fixed_effect_coef_names(
      "center", NULL, model_coef_names, named_predictors
    ),
    fixed_effect_coef_names("center", NULL, model_coef_names, character(0))
  ))

  # WHY it matters, spelled out in the units rec_int_processor() works in.
  # all_center_lvl_effects is the intercept followed by one entry per center
  # dummy, and it is averaged against center_weights_for_outcome_goal, which has
  # one entry per center. An extra claimed coefficient makes it one longer than
  # the weights, so the weights recycle and every predicted outcome shifts.
  all_coefs <- coef(model)
  effects_of <- function(exclusions) {
    dummies <- fixed_effect_coef_names(
      "center", NULL, model_coef_names, exclusions
    )
    intercept <- all_coefs[["(Intercept)"]]
    c(intercept, all_coefs[dummies] + intercept)
  }
  n_centers <- length(levels(bb$center))
  expect_length(effects_of(named_predictors), n_centers)
  expect_length(effects_of(character(0)), n_centers + 1)

  # a center characteristic is excluded on the same footing as a covariate, so
  # the list covers both ways center_size can enter the model
  as_characteristic <- gsub("`", "", c(
    "(Intercept)", c("coaching_updt", "launch_duration"), NULL, "center_size"
  ))
  expect_equal(
    fixed_effect_coef_names(
      "center", NULL, model_coef_names, as_characteristic
    ),
    c("centerc2", "centerc3")
  )

  # For the record on reachability, which is why this is a
  # documentation-of-intent guard rather than a live defect: the fallback runs
  # only when term_coef_names() returns NULL, and it does not for this model or
  # for any model outcome_model_fitting() builds. The mapping resolves the
  # dummies exactly and the fallback is never consulted.
  mapping <- term_coef_names(model)
  expect_false(is.null(mapping))
  expect_equal(mapping$center, c("centerc2", "centerc3"))
  expect_equal(
    fixed_effect_coef_names(
      "center", mapping, model_coef_names, character(0)
    ),
    c("centerc2", "centerc3")
  )
})


test_that("a covariate named exactly like a fixed effect keeps the term's dummies", {
  # The exclusion list is built by appending each column's levels to its own
  # name, which is a string, and for a covariate named exactly "center" those
  # strings ARE the names of the real center dummies. Holding them back takes
  # every genuine dummy out of the center effects and leaves them empty, which
  # is worse than the over-claiming the list prevents, and once the term mapping
  # is gone nothing distinguishes the two. Such a column is therefore left out
  # of the list.
  #
  # Every other fixture in this file names its covariate with a PREFIX of the
  # term (center_size, center_grp, center_flag), which the list must still hold
  # back. Only an exact match is dropped, so both assertions are made here: an
  # earlier attempt at this fix filtered on startsWith() and reintroduced the
  # defect the prefixed fixtures cover.
  claimed_coef_names <- getFromNamespace("claimed_coef_names", "LAGO")

  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  model <- glm(
    Proportions ~ center + AccessMedicines,
    data = pulesa, family = gaussian()
  )
  real_dummies <- grep("^center", names(coef(model)), value = TRUE)
  expect_length(real_dummies, length(levels(pulesa$Clinic)) - 1)

  # a covariate named exactly "center": none of the real dummies may be held
  # back, or the center effects come back empty
  exact <- claimed_coef_names(model, NULL, c(
    "(Intercept)", "AccessMedicines", "center"
  ))
  expect_equal(sum(real_dummies %in% exact), 0)

  # a covariate named with the term as a PREFIX is still held back, which is the
  # case the exclusion list exists for
  pulesa$center_grp <- factor(rep_len(c("a", "b"), nrow(pulesa)))
  prefixed_model <- glm(
    Proportions ~ center + AccessMedicines + center_grp,
    data = pulesa, family = gaussian()
  )
  prefixed <- claimed_coef_names(prefixed_model, NULL, c(
    "(Intercept)", "AccessMedicines", "center_grp"
  ))
  expect_true("center_grpb" %in% prefixed)
  expect_equal(
    sum(grep("^center", names(coef(prefixed_model)), value = TRUE) %in%
      prefixed),
    1
  )
})


test_that("rec_int_processor() itself excludes the covariates it names", {
  # The test above builds the exclusion list the way the callers build it, which
  # pins what the list must BE but cannot observe a caller that stops passing
  # one: it never runs the caller. This does, through rec_int_processor(), on a
  # model whose term mapping has been removed so the fallback is the code path
  # actually taken.
  #
  # The pulesa data has 16 clinics, so a correct run resolves 15 center dummies
  # and all_center_lvl_effects has 16 entries, matching the 16 weights. With the
  # exclusion list emptied the center_size covariate is claimed as a 16th dummy,
  # all_center_lvl_effects becomes 17 long, the weights recycle against it and
  # the estimated outcome changes. That difference is what this asserts.
  rec_int_processor <- getFromNamespace("rec_int_processor", "LAGO")
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")

  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  # a covariate whose own name begins with "center", i.e. the #68 shape
  pulesa$center_size <- 5 + 0.01 * seq_len(nrow(pulesa))
  model <- glm(
    Proportions ~ center + AccessMedicines + AccessBPMachines + center_size,
    data = pulesa, family = gaussian()
  )
  n_centers <- length(levels(pulesa$Clinic))
  expect_equal(n_centers, 16)

  # the fallback is only reached with no mapping, and the mapping is available
  # for this model as fitted, so it is removed on purpose here. Emptying
  # term.labels is what makes term_coef_names() return NULL.
  no_mapping <- model
  attr(no_mapping$terms, "term.labels") <- character(0)
  expect_false(is.null(term_coef_names(model)))
  expect_true(is.null(term_coef_names(no_mapping)))

  run <- function(fitted) {
    suppressWarnings(suppressMessages(rec_int_processor(
      data = pulesa,
      model = fitted,
      center_characteristics = NULL,
      additional_covariates = "center_size",
      include_center_effects = TRUE,
      include_time_effects = FALSE,
      include_interaction_terms = FALSE,
      main_components = NULL,
      intervention_components = c("AccessMedicines", "AccessBPMachines"),
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(5, 0.5),
      link = "identity",
      center_weights_for_outcome_goal = rep(1 / n_centers, n_centers),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 1),
      outcome_goal = 0.6,
      center_characteristics_optimization_values = NULL,
      time_effect_optimization_value = NULL,
      lower_outcome_goal = FALSE,
      prev_recommended_interventions = NULL,
      shrinkage_threshold = 0.25,
      power_goal = NULL,
      power_goal_approach = "unconditional",
      num_centers_in_next_stage = NULL,
      patients_per_center_in_next_stage = NULL,
      outcome_name = "Proportions"
    )))
  }

  # taking the fallback must give the SAME answer as resolving through the
  # mapping: the fallback is a reconstruction of the mapping's result, so the
  # two agreeing is the whole requirement on it. They agree only while the
  # exclusion list is passed. Emptied, the fallback claims center_size as a 16th
  # dummy and the two diverge.
  via_mapping <- run(model)
  via_fallback <- run(no_mapping)
  expect_identical(
    via_fallback$est_outcome_goal, via_mapping$est_outcome_goal
  )
  expect_identical(via_fallback$rec_int, via_mapping$rec_int)
  expect_identical(via_fallback$rec_int_cost, via_mapping$rec_int_cost)

  # and the value itself, so this is not two wrong numbers agreeing. It is a
  # fixture, recorded from the fixed tree rather than derived here: the two
  # assertions above are what pin the behaviour, and this one only holds them to
  # a number that was checked once.
  expect_equal(via_mapping$est_outcome_goal, -1.28046742730091,
    tolerance = 1e-12
  )

  # the OTHER call site, get_confidence_set(), on the same model and the same
  # fallback. It assembles its prediction columns from the resolved names and
  # then checks them against the model's coefficients, so an emptied exclusion
  # list there does not report a wrong number: the extra claimed dummy makes the
  # block one column too wide and the coefficient check refuses the model. That
  # this SUCCEEDS is therefore the assertion, and it is what fails if that call
  # site stops passing its list.
  cs <- suppressWarnings(suppressMessages(get_confidence_set(
    predictors_data = pulesa[, c(
      "center", "AccessMedicines", "AccessBPMachines", "center_size"
    ), drop = FALSE],
    include_center_effects = TRUE,
    center_weights_for_outcome_goal = rep(1 / n_centers, n_centers),
    additional_covariates = "center_size",
    intervention_components = c("AccessMedicines", "AccessBPMachines"),
    outcome_data = pulesa$Proportions,
    fitted_model = no_mapping,
    link = "identity",
    outcome_goal = 0.6,
    outcome_type = "continuous",
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 1),
    confidence_set_grid_step_size = c(5, 0.5),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    rec_int = c(5, 0.5)
  )))
  expect_equal(
    as.numeric(cs$rec_int_ci),
    c(-2.414, -0.262),
    tolerance = 1e-3
  )
})


test_that("a FACTOR covariate's coefficient is excluded, not just its column", {
  # The test above uses a NUMERIC covariate, center_size, whose single
  # coefficient IS named after its column, so passing the column name held it
  # back. A factor or character covariate is one coefficient per non-reference
  # level, each named after the LEVEL: a column center_grp with levels a/b is a
  # coefficient named center_grpb. So a list of column names held back nothing
  # for it, the anchored search claimed center_grpb as a 16th center dummy, and
  # the numeric case passing was not evidence the factor case did.
  #
  # This is #68's defect surviving on the factor case alone, and it is a wrong
  # NUMBER rather than an error: all_center_lvl_effects came back 17 long
  # against 16 weights, the weights recycled, and the reported outcome was off
  # by 5% with nothing said. The exclusion list is therefore built from the
  # coefficient names the predictors account for, which claimed_coef_names()
  # resolves from the model.
  rec_int_processor <- getFromNamespace("rec_int_processor", "LAGO")
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )
  claimed_coef_names <- getFromNamespace("claimed_coef_names", "LAGO")

  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  # a FACTOR covariate whose own name begins with "center", i.e. the #68 shape
  # one level down
  pulesa$center_grp <- factor(rep_len(c("a", "b"), nrow(pulesa)))
  model <- glm(
    Proportions ~ center + AccessMedicines + AccessBPMachines + center_grp,
    data = pulesa, family = gaussian()
  )
  n_centers <- length(levels(pulesa$Clinic))
  expect_equal(n_centers, 16)

  # the fixture has to contain the confusable name, or there is nothing to be
  # wrongly claimed. The coefficient is named after the LEVEL, not the column:
  # that asymmetry is the whole defect.
  model_coef_names <- names(coef(model))
  expect_true("center_grpb" %in% model_coef_names)
  expect_false("center_grp" %in% model_coef_names)

  no_mapping <- model
  attr(no_mapping$terms, "term.labels") <- character(0)
  expect_true(is.null(term_coef_names(no_mapping)))

  # The levels come from model$xlevels rather than from the model frame, and the
  # reason is a fit made with model = FALSE whose data has gone out of scope:
  # that fit has no $model to read levels from. Reading them from there instead
  # would pass every assertion below, because this fixture still carries its
  # model frame, so the case that justifies the choice is asserted here on its
  # own. Both conditions are needed to reach the fallback: model = FALSE alone
  # leaves the term mapping intact.
  detached <- local({
    scoped <- pulesa
    glm(
      Proportions ~ center + AccessMedicines + AccessBPMachines + center_grp,
      data = scoped, family = gaussian(), model = FALSE
    )
  })
  expect_null(detached$model)
  expect_false(is.null(detached$xlevels))
  attr(detached$terms, "term.labels") <- character(0)
  expect_true(is.null(term_coef_names(detached)))

  # with no model frame to fall back on, the level-named coefficient is still
  # held back and center still resolves to its 15 dummies. Reading levels from
  # the absent frame gives 16, which is the recycling shape of the original
  # defect.
  detached_excluded <- claimed_coef_names(
    detached, term_coef_names(detached),
    c("(Intercept)", "AccessMedicines", "AccessBPMachines", "center_grp")
  )
  expect_true("center_grpb" %in% detached_excluded)
  expect_length(
    fixed_effect_coef_names(
      "center", term_coef_names(detached), names(coef(detached)),
      detached_excluded
    ),
    n_centers - 1
  )

  # THROUGH THE CALLER, which is where the wrong number appeared. Taking the
  # fallback must give the same answer as resolving through the mapping: the
  # fallback is a reconstruction of the mapping's result, so the two agreeing is
  # the whole requirement on it.
  run <- function(fitted) {
    suppressWarnings(suppressMessages(rec_int_processor(
      data = pulesa,
      model = fitted,
      center_characteristics = NULL,
      additional_covariates = "center_grp",
      include_center_effects = TRUE,
      include_time_effects = FALSE,
      include_interaction_terms = FALSE,
      main_components = NULL,
      intervention_components = c("AccessMedicines", "AccessBPMachines"),
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(5, 0.5),
      link = "identity",
      center_weights_for_outcome_goal = rep(1 / n_centers, n_centers),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 1),
      outcome_goal = 0.6,
      center_characteristics_optimization_values = NULL,
      time_effect_optimization_value = NULL,
      lower_outcome_goal = FALSE,
      prev_recommended_interventions = NULL,
      shrinkage_threshold = 0.25,
      power_goal = NULL,
      power_goal_approach = "unconditional",
      num_centers_in_next_stage = NULL,
      patients_per_center_in_next_stage = NULL,
      outcome_name = "Proportions"
    )))
  }
  via_mapping <- run(model)
  via_fallback <- run(no_mapping)
  expect_identical(
    via_fallback$est_outcome_goal, via_mapping$est_outcome_goal
  )
  expect_identical(via_fallback$rec_int, via_mapping$rec_int)
  expect_identical(via_fallback$rec_int_cost, via_mapping$rec_int_cost)

  # and the value itself, so this is not two wrong numbers agreeing. The
  # unfixed fallback reported 0.745 against this 0.708, a 5.2% error, which is
  # the magnitude the recycling produces here.
  expect_equal(via_mapping$est_outcome_goal, 0.708333856066007,
    tolerance = 1e-12
  )

  # and the same thing one layer down, in the names themselves. This is placed
  # AFTER the end-to-end assertions on purpose: the defect is a wrong NUMBER, so
  # what must fail on an unfixed tree is the comparison above, not a lookup of a
  # helper that tree does not have.
  # the list the caller owes now names the coefficient, and the model's own
  # xlevels is where the level comes from, so no name is guessed from its shape
  columns <- c(
    "(Intercept)", "AccessMedicines", "AccessBPMachines", "center_grp"
  )
  claimed <- getFromNamespace("claimed_coef_names", "LAGO")(
    no_mapping, NULL, columns
  )
  expect_true("center_grpb" %in% claimed)
  # the column name is kept too: a numeric column IS its own coefficient name
  expect_true(all(columns %in% claimed))

  # with it, only the 15 real dummies are claimed
  expect_length(
    fixed_effect_coef_names("center", NULL, model_coef_names, claimed),
    n_centers - 1
  )
  expect_false("center_grpb" %in%
    fixed_effect_coef_names("center", NULL, model_coef_names, claimed))
  # and with the raw COLUMN names, which is what used to be passed, the factor
  # coefficient is claimed as a 16th. This is the assertion that fails if the
  # list regresses to column names.
  expect_true("center_grpb" %in%
    fixed_effect_coef_names("center", NULL, model_coef_names, columns))
  expect_length(
    fixed_effect_coef_names("center", NULL, model_coef_names, columns),
    n_centers
  )


  # the OTHER call site, get_confidence_set(), on the same fallback model. It
  # cross-checks its assembled columns against the model's coefficients, so the
  # extra claimed dummy there is refused rather than reported as a number: its
  # symptom was an error, not a wrong answer. The fix does not make this model
  # work -- a factor covariate held at its reference level is still one column
  # against two coefficient names -- but it does make the refusal name the one
  # coefficient that is genuinely unmatched instead of all 16.
  cs_error <- tryCatch(
    suppressWarnings(suppressMessages(get_confidence_set(
      predictors_data = pulesa[, c(
        "center", "AccessMedicines", "AccessBPMachines", "center_grp"
      ), drop = FALSE],
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = rep(1 / n_centers, n_centers),
      additional_covariates = "center_grp",
      intervention_components = c("AccessMedicines", "AccessBPMachines"),
      outcome_data = pulesa$Proportions,
      fitted_model = no_mapping,
      link = "identity",
      outcome_goal = 0.6,
      outcome_type = "continuous",
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 1),
      confidence_set_grid_step_size = c(5, 0.5),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      rec_int = c(5, 0.5)
    ))),
    error = function(e) conditionMessage(e)
  )
  expect_match(cs_error, "do not match the predictors")
  # the unmatched coefficient is the factor dummy and NOTHING else: the 15 real
  # center dummies are resolved. Unfixed, all 15 were listed here too, because
  # the block was one column too wide and every name shifted.
  unmatched <- sub(
    ".*coefficient\\(s\\) with no matching predictor: ([^\n]*).*", "\\1",
    cs_error
  )
  expect_identical(trimws(unmatched), "center_grpb")
})


# the fixture the four tests below share: a 16-center pulesa fit with one
# covariate column named center_cov, whose coefficient the anchored ^center
# search will claim unless it is held back. The covariate's TYPE and CODING are
# what vary. Returning the pieces rather than the model keeps each test's own
# assertions about its coding local to it.
contrast_fixture <- function(column, contrasts_arg = NULL) {
  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  pulesa$center_cov <- column(nrow(pulesa))
  model <- if (is.null(contrasts_arg)) {
    glm(Proportions ~ center + AccessMedicines + center_cov,
      data = pulesa, family = gaussian()
    )
  } else {
    glm(Proportions ~ center + AccessMedicines + center_cov,
      data = pulesa, family = gaussian(),
      contrasts = list(center_cov = contrasts_arg)
    )
  }
  list(
    data = pulesa,
    model = model,
    n_centers = length(levels(pulesa$Clinic)),
    columns = c("(Intercept)", "AccessMedicines", "center_cov")
  )
}


test_that("a LOGICAL covariate's coefficient is excluded (it has no xlevels)", {
  # A logical column is contrast-coded exactly as a factor is -- glm() codes it
  # as factor(x, c(FALSE, TRUE)) and its coefficient is center_covTRUE -- but it
  # gets NO $xlevels entry at all. So an expansion driven off $xlevels saw fewer
  # than two levels, held nothing back, and the anchored search claimed
  # center_covTRUE as a 16th center dummy: #68's recycling shape on a column
  # type the fix was meant to cover. The expansion is driven off $contrasts
  # instead, which does list the column.
  fx <- contrast_fixture(function(k) rep_len(c(TRUE, FALSE), k))
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )
  claimed_coef_names <- getFromNamespace("claimed_coef_names", "LAGO")

  # the fixture's own preconditions: this is the asymmetry that is the defect
  model_coef_names <- names(coef(fx$model))
  expect_true("center_covTRUE" %in% model_coef_names)
  expect_false("center_cov" %in% model_coef_names)
  expect_true("center_cov" %in% names(fx$model$contrasts))
  expect_false("center_cov" %in% names(fx$model$xlevels))

  claimed <- claimed_coef_names(fx$model, NULL, fx$columns)
  expect_true("center_covTRUE" %in% claimed)
  # and the center block is its 15 real dummies, not 16
  dummies <- fixed_effect_coef_names(
    "center", NULL, model_coef_names, claimed
  )
  expect_length(dummies, fx$n_centers - 1)
  expect_false("center_covTRUE" %in% dummies)
  # the mapping path is the reference answer, and the fallback must equal it
  mapping <- term_coef_names(fx$model)
  expect_false(is.null(mapping))
  expect_identical(
    sort(dummies),
    sort(fixed_effect_coef_names(
      "center", mapping, model_coef_names,
      claimed_coef_names(fx$model, mapping, fx$columns)
    ))
  )
})


test_that("an ORDERED covariate's coefficients are excluded (contr.poly)", {
  # An ordered factor defaults to contr.poly, so its coefficients are
  # center_cov.L and center_cov.Q -- polynomial contrasts, NOT one dummy per
  # level. paste0(column, levels[-1]) built center_covmid and center_covhi,
  # which are not coefficients of this model at all, so nothing real was held
  # back and the anchored search claimed BOTH real ones as center dummies: 17
  # against 16 weights. That is a larger silent error than the two-level factor
  # case, because two extra effects enter rather than one.
  fx <- contrast_fixture(function(k) {
    factor(rep_len(c("lo", "mid", "hi"), k),
      levels = c("lo", "mid", "hi"), ordered = TRUE
    )
  })
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )
  claimed_coef_names <- getFromNamespace("claimed_coef_names", "LAGO")

  # the coefficients really are polynomial, and the level-named strings the old
  # expansion built really are absent: without both halves there is no defect
  model_coef_names <- names(coef(fx$model))
  expect_true(all(c("center_cov.L", "center_cov.Q") %in% model_coef_names))
  expect_false(any(c("center_covmid", "center_covhi") %in% model_coef_names))
  expect_identical(fx$model$contrasts[["center_cov"]], "contr.poly")

  claimed <- claimed_coef_names(fx$model, NULL, fx$columns)
  expect_true(all(c("center_cov.L", "center_cov.Q") %in% claimed))
  dummies <- fixed_effect_coef_names(
    "center", NULL, model_coef_names, claimed
  )
  expect_length(dummies, fx$n_centers - 1)
  expect_false(any(c("center_cov.L", "center_cov.Q") %in% dummies))
  mapping <- term_coef_names(fx$model)
  expect_false(is.null(mapping))
  expect_identical(
    sort(dummies),
    sort(fixed_effect_coef_names(
      "center", mapping, model_coef_names,
      claimed_coef_names(fx$model, mapping, fx$columns)
    ))
  )
})


test_that("a NON-DEFAULT contrast's coefficients are excluded", {
  # A caller may set any coding, through options(contrasts=) or the per-column
  # contrasts= argument of glm(). contr.helmert names its dummies by POSITION,
  # center_cov1 and center_cov2, so a levels-driven expansion built
  # center_covb / center_covc -- names this model does not have -- and both real
  # ones were claimed as center dummies. The suffixes come from the contrast
  # matrix's colnames() instead, which is what glm() itself appends.
  fx <- contrast_fixture(
    function(k) factor(rep_len(c("a", "b", "c"), k)), "contr.helmert"
  )
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )
  claimed_coef_names <- getFromNamespace("claimed_coef_names", "LAGO")

  model_coef_names <- names(coef(fx$model))
  expect_true(all(c("center_cov1", "center_cov2") %in% model_coef_names))
  expect_false(any(c("center_covb", "center_covc") %in% model_coef_names))
  # contr.helmert's own matrix has NO column names, so the suffixes are the
  # column positions. Deriving them from colnames() alone would give nothing.
  expect_null(colnames(contr.helmert(3)))

  claimed <- claimed_coef_names(fx$model, NULL, fx$columns)
  expect_true(all(c("center_cov1", "center_cov2") %in% claimed))
  dummies <- fixed_effect_coef_names(
    "center", NULL, model_coef_names, claimed
  )
  expect_length(dummies, fx$n_centers - 1)
  mapping <- term_coef_names(fx$model)
  expect_false(is.null(mapping))
  expect_identical(
    sort(dummies),
    sort(fixed_effect_coef_names(
      "center", mapping, model_coef_names,
      claimed_coef_names(fx$model, mapping, fx$columns)
    ))
  )

  # the same defect through options(contrasts=), which is set globally and so
  # reaches a caller who never touched glm()'s contrasts argument. The coding is
  # read off the FIT, so restoring the option must not change the answer.
  previous <- options(contrasts = c("contr.sum", "contr.poly"))
  summed <- contrast_fixture(function(k) factor(rep_len(c("a", "b", "c"), k)))
  options(previous)
  expect_identical(summed$model$contrasts[["center_cov"]], "contr.sum")
  expect_identical(getOption("contrasts")[[1]], "contr.treatment")
  summed_names <- names(coef(summed$model))
  expect_true(all(c("center_cov1", "center_cov2") %in% summed_names))
  expect_length(
    fixed_effect_coef_names(
      "center", NULL, summed_names,
      claimed_coef_names(summed$model, NULL, summed$columns)
    ),
    summed$n_centers - 1
  )
})


test_that("the fallback equals the mapping for every column type it handles", {
  # The fallback is a reconstruction of what the term mapping gives, so the one
  # requirement on it is that it AGREES with the mapping wherever the mapping
  # exists. That is the strongest check available, because it does not depend on
  # anybody's reasoning about what glm() names things: the mapping reads the
  # names off the model matrix.
  #
  # Asserted per column type rather than on one model, because each type is
  # coded differently and a fix for one need not fix another -- which is how a
  # logical column and an ordered factor stayed broken while the two-level
  # factor case was fixed.
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )
  claimed_coef_names <- getFromNamespace("claimed_coef_names", "LAGO")

  unnamed_matrix <- contr.treatment(3)
  colnames(unnamed_matrix) <- NULL
  cases <- list(
    numeric = list(function(k) seq_len(k) / k, NULL),
    `factor 2 levels` = list(
      function(k) factor(rep_len(c("a", "b"), k)), NULL
    ),
    `factor 4 levels` = list(
      function(k) factor(rep_len(c("a", "b", "c", "d"), k)), NULL
    ),
    character = list(function(k) rep_len(c("a", "b"), k), NULL),
    logical = list(function(k) rep_len(c(TRUE, FALSE), k), NULL),
    ordered = list(function(k) {
      factor(rep_len(c("lo", "mid", "hi"), k),
        levels = c("lo", "mid", "hi"), ordered = TRUE
      )
    }, NULL),
    `unused level` = list(function(k) {
      factor(rep_len(c("a", "b"), k), levels = c("a", "b", "z"))
    }, NULL),
    # levels in an order factor() would not choose for itself. Every other
    # factor case here is alphabetical, so rebuilding the column WITHOUT
    # carrying its levels over would still produce the right suffixes and this
    # would all pass. Under contr.treatment the suffixes ARE the levels, so a
    # rebuild that re-sorted them holds back a name the model does not have and
    # misses one it does.
    `unsorted levels` = list(function(k) {
      factor(rep_len(c("mid", "lo", "hi"), k),
        levels = c("mid", "lo", "hi")
      )
    }, NULL),
    `contr.sum` = list(
      function(k) factor(rep_len(c("a", "b", "c"), k)), "contr.sum"
    ),
    `contr.SAS` = list(
      function(k) factor(rep_len(c("a", "b", "c"), k)), "contr.SAS"
    ),
    `unnamed contrast matrix` = list(
      function(k) factor(rep_len(c("a", "b", "c"), k)), unnamed_matrix
    )
  )

  for (case in names(cases)) {
    fx <- contrast_fixture(cases[[case]][[1]], cases[[case]][[2]])
    mapping <- term_coef_names(fx$model)
    # the mapping has to be available, or the comparison is vacuous
    expect_false(is.null(mapping), info = case)
    model_coef_names <- names(coef(fx$model))

    # the invariant: the same center dummies either way
    expect_identical(
      sort(fixed_effect_coef_names(
        "center", NULL, model_coef_names,
        claimed_coef_names(fx$model, NULL, fx$columns)
      )),
      sort(fixed_effect_coef_names(
        "center", mapping, model_coef_names,
        claimed_coef_names(fx$model, mapping, fx$columns)
      )),
      info = case
    )
    # and it is the right SIZE, so the two agreeing is not two wrong answers
    expect_length(
      fixed_effect_coef_names(
        "center", NULL, model_coef_names,
        claimed_coef_names(fx$model, NULL, fx$columns)
      ),
      fx$n_centers - 1
    )
    # every name the fallback holds back for the covariate is a coefficient the
    # model really has. The levels-driven expansion built center_covmid for an
    # ordered factor, which is nobody's coefficient, and so held back nothing.
    derived <- setdiff(
      claimed_coef_names(fx$model, NULL, fx$columns), fx$columns
    )
    expect_true(all(derived %in% model_coef_names), info = case)
    # and the covariate's own coefficients are exactly what the mapping says
    expect_identical(
      sort(unique(c(derived, "center_cov"))),
      sort(unique(c(mapping[["center_cov"]], "center_cov"))),
      info = case
    )
  }
})


test_that("the fallback equals the mapping on a fit with no model frame", {
  # The route the fallback is written for: a model = FALSE fit whose data= name
  # has left scope. That fit has no $model to read a level or a coding from, so
  # the derivation must come off $contrasts and $xlevels, which both survive it.
  # model = FALSE alone is not enough -- model.matrix() re-derives the frame
  # from the data, which is still reachable -- so the term labels are cleared
  # too, which is what makes term_coef_names() return NULL.
  term_coef_names <- getFromNamespace("term_coef_names", "LAGO")
  fixed_effect_coef_names <- getFromNamespace(
    "fixed_effect_coef_names", "LAGO"
  )
  claimed_coef_names <- getFromNamespace("claimed_coef_names", "LAGO")

  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  n_centers <- length(levels(pulesa$Clinic))
  # a logical and an ordered column together, i.e. the two types with no
  # $xlevels entry and no per-level naming
  pulesa$center_flag <- rep_len(c(TRUE, FALSE), nrow(pulesa))
  pulesa$center_ord <- factor(
    rep_len(c("lo", "mid", "hi"), nrow(pulesa)),
    levels = c("lo", "mid", "hi"), ordered = TRUE
  )
  formula <- Proportions ~ center + AccessMedicines + center_flag + center_ord
  columns <- c(
    "(Intercept)", "AccessMedicines", "center_flag", "center_ord"
  )

  model <- glm(formula, data = pulesa, family = gaussian())
  mapping <- term_coef_names(model)
  expect_false(is.null(mapping))
  reference <- sort(fixed_effect_coef_names(
    "center", mapping, names(coef(model)),
    claimed_coef_names(model, mapping, columns)
  ))
  expect_length(reference, n_centers - 1)

  # model = FALSE ALONE leaves the mapping intact: the frame is gone, but the
  # name given as data= is still reachable from the formula's environment, so
  # model.matrix() re-derives one. This is the half the comment used to omit.
  still_reachable <- local({
    scoped <- pulesa
    glm(Proportions ~ center + AccessMedicines + center_flag + center_ord,
      data = scoped, family = gaussian(), model = FALSE
    )
  })
  expect_null(still_reachable$model)
  expect_false(is.null(term_coef_names(still_reachable)))

  # with the name out of scope as well, there is neither a frame to read nor
  # data to rebuild one from, and the mapping really is NULL. No term label is
  # cleared here: this is the route as a caller reaches it.
  detached <- local({
    scoped <- pulesa
    glm(formula, data = scoped, family = gaussian(), model = FALSE)
  })
  expect_null(detached$model)
  expect_true(is.null(term_coef_names(detached)))
  # and what the derivation needs did survive. $contrasts lists the logical
  # column even though $xlevels does not, which is why it is the source used.
  expect_false(is.null(detached$contrasts))
  expect_true("center_flag" %in% names(detached$contrasts))
  expect_false("center_flag" %in% names(detached$xlevels))
  expect_true("center_ord" %in% names(detached$xlevels))

  claimed <- claimed_coef_names(detached, NULL, columns)
  expect_true("center_flagTRUE" %in% claimed)
  expect_true(all(c("center_ord.L", "center_ord.Q") %in% claimed))
  expect_identical(
    sort(fixed_effect_coef_names(
      "center", NULL, names(coef(detached)), claimed
    )),
    reference
  )

  # THROUGH THE CALLER, which is where the wrong number appeared: two extra
  # effects entered all_center_lvl_effects, the weights recycled against it, and
  # the reported outcome was off by 10.5% with nothing said. Held back, the
  # fallback reports what the mapping does.
  rec_int_processor <- getFromNamespace("rec_int_processor", "LAGO")
  run <- function(fitted) {
    suppressWarnings(suppressMessages(rec_int_processor(
      data = pulesa,
      model = fitted,
      center_characteristics = NULL,
      additional_covariates = c("center_flag", "center_ord"),
      include_center_effects = TRUE,
      include_time_effects = FALSE,
      include_interaction_terms = FALSE,
      main_components = NULL,
      intervention_components = "AccessMedicines",
      optimization_method = "grid_search",
      optimization_grid_search_step_size = 5,
      link = "identity",
      center_weights_for_outcome_goal = rep(1 / n_centers, n_centers),
      cost_list_of_vectors = list(c(0, 1)),
      intervention_lower_bounds = 0,
      intervention_upper_bounds = 10,
      outcome_goal = 0.6,
      center_characteristics_optimization_values = NULL,
      time_effect_optimization_value = NULL,
      lower_outcome_goal = FALSE,
      prev_recommended_interventions = NULL,
      shrinkage_threshold = 0.25,
      power_goal = NULL,
      power_goal_approach = "unconditional",
      num_centers_in_next_stage = NULL,
      patients_per_center_in_next_stage = NULL,
      outcome_name = "Proportions"
    )))
  }
  via_mapping <- run(model)
  via_fallback <- run(detached)
  expect_identical(
    via_fallback$est_outcome_goal, via_mapping$est_outcome_goal
  )
  expect_identical(via_fallback$rec_int, via_mapping$rec_int)
})


test_that("lago_optimization() passes additional_covariates on to the processor", {
  # The test above runs rec_int_processor() directly, so it pins what the callee
  # does with the argument but not that its caller supplies it. Deleting the one
  # line in lago_optimization() that forwards additional_covariates leaves that
  # test green, because nothing else drives the caller.
  #
  # The fallback the argument protects needs a model whose term mapping is
  # missing, which lago_optimization() never fits, so this cannot be reached
  # end to end by running an optimization. What can be checked is the forwarding
  # itself: rec_int_processor() is replaced for the duration of one call and
  # asked what it received.
  seen <- new.env(parent = emptyenv())
  real <- getFromNamespace("rec_int_processor", "LAGO")
  spy <- function(...) {
    args <- list(...)
    seen$additional_covariates <- args$additional_covariates
    do.call(real, args)
  }

  testthat::with_mocked_bindings(
    {
      suppressWarnings(suppressMessages(lago_optimization(
        data = BB_data,
        outcome_name = "pp3_oxytocin_mother",
        outcome_type = "binary",
        glm_family = "binomial",
        intervention_components = c("coaching_updt", "launch_duration"),
        additional_covariates = "birth_volume_100",
        intervention_lower_bounds = c(1, 1),
        intervention_upper_bounds = c(40, 5),
        cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
        outcome_goal = 0.85,
        include_confidence_set = FALSE,
        quiet = TRUE
      )))
    },
    rec_int_processor = spy,
    .package = "LAGO"
  )

  expect_identical(seen$additional_covariates, "birth_volume_100")
})


# ---------------------------------------------------------------------------
# select_restart_within_bounds(): the numerical optimizer's restart selection
# ---------------------------------------------------------------------------

# a linear total cost, the shape create_cost_function() builds from
# cost_list_of_vectors = list(c(0, 2), c(0, 5)). Linear and increasing, so
# projecting a component UP to a lower bound raises the cost and projecting one
# DOWN to an upper bound lowers it. The recomputed cost is therefore not on a
# predictable side of the solver's, which is why it is recomputed rather than
# adjusted.
srb_cost <- function(x) sum(c(2, 5) * x)
srb_lower <- c(1, 1)
srb_upper <- c(10, 5)

# the cost function get_recommended_interventions() actually assembles: one
# closure per component from create_cost_function(), reduced with mapply(). Same
# values as srb_cost() on a two-component intervention, but it ERRORS on a
# zero-length intervention instead of returning 0, which is used below where
# that distinction is the point.
srb_cost_as_assembled <- local({
  create_cost_function <- function(coeffs) {
    function(x) {
      sum(sapply(seq_along(coeffs), function(i) coeffs[i] * x^(i - 1)))
    }
  }
  cost_functions <- lapply(list(c(0, 2), c(0, 5)), create_cost_function)
  function(x) sum(mapply(function(f, x) f(x), cost_functions, x))
})

# restart columns and their costs together, so a test cannot accidentally pair a
# cost with the wrong restart: the cost of every restart IS srb_cost() at it,
# which is what solnl() converges to and reports in cost_results.
srb_restarts <- function(...) {
  points <- cbind(...)
  list(points = points, costs = apply(points, 2, srb_cost))
}


test_that("out-of-box restarts are dropped and the cheapest is taken", {
  # The first three steps, on restarts that need no projection. solnl() steps a
  # little outside the box to buy a lower objective, so the cheapest restart is
  # systematically the one furthest outside the bounds: choosing on cost alone
  # chooses the violation. That is why the in-box filter runs FIRST and the
  # comparison is only over the survivors.
  select_restart_within_bounds <- getFromNamespace(
    "select_restart_within_bounds", "LAGO"
  )

  # ALL in box. The filter keeps everything, so the answer is just the cheapest,
  # and the projection has nothing to move: the chosen point comes back
  # unchanged, bit for bit.
  all_in <- srb_restarts(c(2, 2), c(3, 1), c(5, 4))
  expect_equal(all_in$costs, c(14, 11, 30))
  chosen <- select_restart_within_bounds(
    all_in$points, all_in$costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))
  expect_identical(chosen$rec_int_cost, 11)

  # SOME in box, and the cheapest restart overall is one of the ones that left
  # it: column 1 costs 6, which is cheaper than every survivor, and it is
  # discarded because its first component is 0.5 against a lower bound of 1.
  # Without the filter it would win, and its projection to c(1, 1) would then be
  # recommended at a cost of 7 rather than the genuine in-box optimum of 11.
  some_in <- srb_restarts(c(0.5, 1), c(3, 1), c(4, 2))
  expect_equal(some_in$costs, c(6, 11, 18))
  chosen <- select_restart_within_bounds(
    some_in$points, some_in$costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))
  expect_identical(chosen$rec_int_cost, 11)
  # and it really did pass over a cheaper number
  expect_lt(min(some_in$costs), chosen$rec_int_cost)

  # an UPPER-bound violation is dropped just the same as a lower-bound one, so
  # the filter is not one-sided
  upper_out <- srb_restarts(c(3, 1), c(10, 5.5), c(10.5, 1))
  chosen <- select_restart_within_bounds(
    upper_out$points, upper_out$costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))

  # a SINGLE restart, in box: the degenerate case of the same three steps, and
  # the one that a which.min() over an empty selection would break on
  single <- srb_restarts(c(3, 1))
  chosen <- select_restart_within_bounds(
    single$points, single$costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))
  expect_identical(chosen$rec_int_cost, 11)
})


test_that("a tie in restart cost resolves to the first such restart", {
  # which.min() takes the first minimum, so two restarts of equal cost resolve
  # to the earlier column. This is not arbitrary: the restarts are ordered by
  # their start point along the box diagonal, so the tie-break is deterministic
  # and reproducible rather than dependent on the order NlcOptim happened to
  # return. Pinning it is what makes a future change of reduction (e.g. to
  # which() plus sample(), or to the LAST minimum) visible.
  select_restart_within_bounds <- getFromNamespace(
    "select_restart_within_bounds", "LAGO"
  )

  # c(3, 1) and c(1, 1.8) both cost 11, exactly, and both are in the box
  tie <- srb_restarts(c(3, 1), c(1, 1.8), c(5, 4))
  expect_identical(tie$costs[[1]], tie$costs[[2]])
  expect_identical(tie$costs, c(11, 11, 30))
  chosen <- select_restart_within_bounds(
    tie$points, tie$costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))
  expect_identical(chosen$rec_int_cost, 11)

  # the tie-break happens AFTER the filter, not before it: put an equally cheap
  # restart outside the box in front of the winner and the winner is unchanged,
  # because the out-of-box one is not in the comparison at all
  tie_out <- srb_restarts(c(0.5, 2), c(3, 1), c(5, 4))
  expect_identical(tie_out$costs, c(11, 11, 30))
  chosen <- select_restart_within_bounds(
    tie_out$points, tie_out$costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))
})


test_that("a restart whose optimization failed is not selected", {
  # solnl() is wrapped in tryCatch() and a restart that errors leaves NA in
  # cost_results with its column of the restart matrix left at whatever it was
  # initialised to, i.e. all zeros. An NA cost must therefore exclude the
  # restart outright: which.min() ignores NA, but `!is.na(costs)` is what stops
  # the all-zeros column from being read as a legitimate in-box point of unknown
  # cost.
  select_restart_within_bounds <- getFromNamespace(
    "select_restart_within_bounds", "LAGO"
  )

  # column 1 is the zeros a failed restart leaves behind. It is OUT of the box
  # (0 < lower bound 1) and its cost is NA.
  with_na <- srb_restarts(c(0, 0), c(3, 1), c(5, 4))
  costs <- c(NA_real_, with_na$costs[[2]], with_na$costs[[3]])
  chosen <- select_restart_within_bounds(
    with_na$points, costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))
  expect_identical(chosen$rec_int_cost, 11)

  # the harder version: the NA is on a restart that IS in the box, and would be
  # the cheapest if its cost were read as 0. The in-box filter alone does not
  # exclude it, so this is the NA guard on its own.
  na_in_box <- srb_restarts(c(1, 1), c(3, 1), c(5, 4))
  chosen <- select_restart_within_bounds(
    na_in_box$points, c(NA_real_, 11, 30), srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(3, 1))
  expect_identical(chosen$rec_int_cost, 11)

  # and when the ONLY in-box restart is the failed one, the fallback keeps the
  # out-of-box ones rather than returning nothing, and the cheapest of THOSE is
  # projected. c(0.5, 1) costs 6 unprojected and c(1, 1) costs 7.
  only_na_in_box <- srb_restarts(c(2, 2), c(0.5, 1), c(11, 2))
  expect_identical(only_na_in_box$costs, c(14, 6, 32))
  chosen <- select_restart_within_bounds(
    only_na_in_box$points, c(NA_real_, 6, 32),
    srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(1, 1))
  expect_identical(chosen$rec_int_cost, 7)

  # EVERY restart failing selects nothing at all: both the filter and the
  # fallback are `which()` over an all-NA condition, so there is no column to
  # index and no intervention to project. get_recommended_interventions()
  # refuses that case with its own "Numerical optimization failed to find a
  # solution" message BEFORE calling this, which is why there is no stop() here.
  # This pins that the refusal upstream is load-bearing: reached anyway, the
  # cost function the optimizer assembles errors out, so a deleted upstream
  # refusal surfaces as an opaque failure rather than as a recommendation.
  all_failed <- srb_restarts(c(2, 2), c(3, 1), c(5, 4))
  expect_error(
    select_restart_within_bounds(
      all_failed$points, rep(NA_real_, 3), srb_lower, srb_upper,
      srb_cost_as_assembled
    ),
    "invalid 'type'"
  )
  # the selection itself is what is empty, independently of the cost function:
  # with a cost function that tolerates a zero-length argument, the returned
  # intervention has no components at all.
  chosen <- select_restart_within_bounds(
    all_failed$points, rep(NA_real_, 3), srb_lower, srb_upper, srb_cost
  )
  expect_length(chosen$int_components, 0)
  # and the two cost functions agree on every non-degenerate case, so that
  # substitution is not smuggling in different arithmetic
  expect_identical(srb_cost(c(3, 1)), srb_cost_as_assembled(c(3, 1)))
  expect_identical(srb_cost(c(4, 1)), srb_cost_as_assembled(c(4, 1)))
})


test_that("an all-failed restart set is refused by one guard, for both loops", {
  # The refusal the test above relies on being upstream. Both restart loops of
  # the numerical optimizer record a failed solnl() as NA and carry on, and both
  # have to refuse the case where every restart failed, because there is then
  # nothing to select. The cost loop always did; the max-achievable-outcome loop
  # did not, and reached which.max() over an all-NA vector, which is integer(0),
  # so the outcome it indexed was numeric(0) and the goal comparison two
  # statements later failed with the base-R error "argument is of length zero"
  # rather than telling the caller what to do instead.
  #
  # The condition and the wording are now one function both loops call, so this
  # tests the guard itself rather than one loop's copy of it.
  refuse_if_all_restarts_failed <- getFromNamespace(
    "refuse_if_all_restarts_failed", "LAGO"
  )

  # all failed on a FULL-RANK model: refused, and by the message that names the
  # way out. That the model is of full rank is what makes recommending the other
  # optimizer sensible, and it is stated by passing no aliased coefficient
  # names -- see the test below for the other branch.
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11)),
    "Numerical optimization failed to find a solution"
  )
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11)),
    "'grid_search'"
  )
  # passing an EMPTY set of aliased names is the same as passing none, so the
  # argument's default cannot drift away from what the callers rely on
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), character(0)),
    "Numerical optimization failed to find a solution"
  )
  # a single restart, failed, is still every restart
  expect_error(
    refuse_if_all_restarts_failed(NA_real_),
    "Numerical optimization failed to find a solution"
  )

  # SOME failing is not this case and must pass through untouched, which is the
  # half that a guard written as any(is.na()) would break: one bad starting
  # point out of eleven has to leave the optimization running.
  expect_silent(refuse_if_all_restarts_failed(c(NA_real_, 0.5, NA_real_)))
  expect_null(refuse_if_all_restarts_failed(c(NA_real_, 0.5)))
  expect_silent(refuse_if_all_restarts_failed(c(0.1, 0.2, 0.3)))

  # and what the surviving restarts then resolve to, which is the reason the
  # partial case is safe to pass through: which.max() skips NAs, so it returns
  # the position of the best SURVIVOR in the original indexing. Both the value
  # and the restart's converged point are indexed by that same position, so they
  # stay the pair one restart produced rather than being read off different
  # restarts.
  results <- c(NA_real_, 0.4, NA_real_, 0.9, 0.2)
  points <- matrix(
    c(0, 0, 4, 1, 0, 0, 9, 2, 1, 3),
    nrow = 2
  )
  max_position <- which.max(results)
  expect_identical(max_position, 4L)
  expect_identical(results[max_position], 0.9)
  expect_identical(points[, max_position], c(9, 2))
  # not the NA-bearing first column, which an is.na()-blind max would take
  expect_false(identical(points[, 1], points[, max_position]))
})


test_that("a rank-deficient fit is not answered by recommending grid_search", {
  # The refusal has one CONDITION and two CAUSES, and the causes want opposite
  # advice. Recommending the other optimization method is only sensible when the
  # model could be estimated: an aliased term's coefficient is NA, every outcome
  # computed from it is NA, and no search over interventions can recover from
  # that -- the grid search fails on the same fit, with its own unguarded
  # comparison. So the message that names grid_search must NOT be what a
  # rank-deficient fit gets, which is what a single message for both did.
  refuse_if_all_restarts_failed <- getFromNamespace(
    "refuse_if_all_restarts_failed", "LAGO"
  )
  refuse_if_no_grid_outcome <- getFromNamespace(
    "refuse_if_no_grid_outcome", "LAGO"
  )

  aliased <- c("period2", "period3")

  # ONE aliased term takes the rank-deficient branch too, which is the boundary
  # the branch is chosen at. Asserting only the two-term case leaves a test that
  # a condition of "more than one aliased term" would satisfy, and a single
  # aliased coefficient is what the message's own example produces: two
  # intervention components that are rescalings of one another alias exactly
  # one. Getting the boundary wrong restores the whole defect for that case,
  # grid search recommended and more than three components blamed.
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), "launch_duration"),
    "rank-deficient"
  )
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), "launch_duration"),
    "launch_duration"
  )
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), "launch_duration"),
    "^(?!.*grid_search).*$",
    perl = TRUE
  )

  # the rank-deficient branch: the cause, the terms, and what to do about them
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), aliased),
    "rank-deficient"
  )
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), aliased),
    "period2, period3"
  )
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), aliased),
    "[Dd]rop or combine"
  )
  # and NOT the advice that cannot help. This is the assertion the shared
  # message fails: it recommended grid_search whatever the cause.
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), aliased),
    "^(?!.*grid_search).*$",
    perl = TRUE
  )
  # nor the cause it used to blame, which the aliased fit does not have: two
  # intervention components, not more than three
  expect_error(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), aliased),
    "^(?!.*more than\\s+three intervention components).*$",
    perl = TRUE
  )

  # the branch is on the ALIASED NAMES and not on the restarts, so a full-rank
  # all-failed set still gets the wording a caller in that position needs. Both
  # branches from the same call, so neither can be reached by accident.
  full_rank <- tryCatch(
    refuse_if_all_restarts_failed(rep(NA_real_, 11)),
    error = conditionMessage
  )
  deficient <- tryCatch(
    refuse_if_all_restarts_failed(rep(NA_real_, 11), aliased),
    error = conditionMessage
  )
  expect_false(identical(full_rank, deficient))
  expect_match(full_rank, "grid_search")
  expect_no_match(deficient, "grid_search")

  # a SOME-failed set still passes through, whatever the model: the branch must
  # not turn the guard into one that fires on any NA. A restart is a starting
  # point, and the survivors are a legitimate answer.
  expect_silent(refuse_if_all_restarts_failed(c(NA_real_, 0.5), aliased))
  expect_null(refuse_if_all_restarts_failed(c(NA_real_, 0.5), aliased))

  # the grid search's own guard, which is the second half of the same defect:
  # an NA grid outcome reached max() >= goal and failed with the base-R
  # "missing value where TRUE/FALSE needed". It says the same thing about the
  # same cause, since the cause is the model.
  expect_error(refuse_if_no_grid_outcome(c(0.4, NA, 0.6), aliased),
    "rank-deficient")
  expect_error(refuse_if_no_grid_outcome(c(0.4, NA, 0.6), aliased),
    "period2, period3")
  expect_error(refuse_if_no_grid_outcome(c(NA_real_, NA_real_), aliased),
    "rank-deficient")
  # the same boundary as above: one aliased term takes this branch too
  expect_error(refuse_if_no_grid_outcome(c(0.4, NA, 0.6), "launch_duration"),
    "rank-deficient")
  expect_error(refuse_if_no_grid_outcome(c(0.4, NA, 0.6), "launch_duration"),
    "^(?!.*grid_search).*$", perl = TRUE)
  expect_identical(
    tryCatch(refuse_if_no_grid_outcome(rep(NA_real_, 3), aliased),
      error = conditionMessage),
    deficient
  )
  # and on a full-rank model it names the outcome rather than the model, and
  # still does not recommend a method
  full_rank_grid <- tryCatch(
    refuse_if_no_grid_outcome(c(0.4, NA, 0.6)),
    error = conditionMessage
  )
  expect_match(full_rank_grid, "could not be computed at every intervention")
  expect_no_match(full_rank_grid, "rank-deficient")

  # it fires on ANY NA, unlike the restart guard, and that is exactly the
  # condition that already failed: max() of a vector holding one NA is NA, so
  # "NA >= goal" was already the R error. Refusing here narrows nothing.
  expect_true(is.na(max(c(0.4, NA, 0.6))))
  expect_error(
    if (max(c(0.4, NA, 0.6)) >= 0.5) TRUE else FALSE,
    "missing value where TRUE/FALSE needed"
  )
  # a grid with every outcome a number passes through untouched
  expect_silent(refuse_if_no_grid_outcome(c(0.4, 0.5, 0.6)))
  expect_null(refuse_if_no_grid_outcome(c(0.4, 0.5, 0.6), aliased))
  expect_silent(refuse_if_no_grid_outcome(numeric(0), aliased))
})


test_that("the optimizers are told which coefficients could not be estimated", {
  # The guards above can only distinguish the two causes if something reads the
  # aliased names off the FITTED MODEL and hands them over.
  # get_recommended_interventions() is given coefficient vectors, not the model,
  # and by then an NA has been summed into the center-level effects and carries
  # THEIR names -- so the terms could not be named from there even though the
  # NA is visible. rec_int_processor() has the model, which is why the read
  # happens there.
  bbp <- as.data.frame(BB_proportions)
  bbp$center <- factor(rep_len(paste0("s", 1:6), nrow(bbp)))
  bbp$period <- factor(rep_len(1:3, nrow(bbp)))
  model <- suppressWarnings(glm(
    EBP_proportions ~ center + period + coaching_updt + launch_duration,
    data = bbp, family = quasibinomial(link = "logit")
  ))
  all_coefs <- coef(model)
  # the precondition, asserted rather than assumed
  expect_true(anyNA(all_coefs))
  expect_identical(names(all_coefs)[is.na(all_coefs)], c("period2", "period3"))

  # what the NA looks like one step downstream, which is why the read cannot be
  # deferred: it is still there, but it now spells the center-level effects
  coef_mapping <- getFromNamespace("term_coef_names", "LAGO")(model)
  named_predictors <- getFromNamespace("claimed_coef_names", "LAGO")(
    model, coef_mapping,
    c("(Intercept)", "coaching_updt", "launch_duration")
  )
  fecn <- getFromNamespace("fixed_effect_coef_names", "LAGO")
  center_coefs <- fecn("center", coef_mapping, names(all_coefs),
    named_predictors)
  period_coefs <- fecn("period", coef_mapping, names(all_coefs),
    named_predictors)
  intercept <- all_coefs["(Intercept)"]
  center_level <- c(intercept, all_coefs[center_coefs] + intercept)
  indicators <- getFromNamespace("time_effect_indicator", "LAGO")(
    model, period_coefs, 1
  )
  center_level <- center_level + sum(indicators * all_coefs[period_coefs])
  expect_true(anyNA(center_level))
  # every entry is NA and each is named after a CENTER, so "period2, period3"
  # is unrecoverable from here
  expect_true(all(is.na(center_level)))
  expect_false(any(c("period2", "period3") %in% names(center_level)))

  # and the intervention coefficients, which is the other vector the optimizer
  # gets, are not NA at all: nothing there says the fit is rank-deficient
  expect_false(anyNA(all_coefs[c("(Intercept)", "coaching_updt",
    "launch_duration")]))

  # so the aliased names are passed in, and the refusal names them. Through the
  # processor rather than the optimizer, which is the wiring under test.
  err <- tryCatch(
    suppressWarnings(suppressMessages(getFromNamespace(
      "rec_int_processor", "LAGO"
    )(
      data = bbp,
      model = model,
      center_characteristics = NULL,
      additional_covariates = NULL,
      include_center_effects = TRUE,
      include_time_effects = TRUE,
      include_interaction_terms = FALSE,
      main_components = NULL,
      intervention_components = c("coaching_updt", "launch_duration"),
      optimization_method = "numerical",
      optimization_grid_search_step_size = NULL,
      link = "logit",
      center_weights_for_outcome_goal = rep(1 / 6, 6),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      outcome_goal = 0.85,
      center_characteristics_optimization_values = NULL,
      time_effect_optimization_value = 1,
      lower_outcome_goal = FALSE,
      prev_recommended_interventions = NULL,
      shrinkage_threshold = 0.25,
      power_goal = NULL,
      power_goal_approach = "unconditional",
      num_centers_in_next_stage = NULL,
      patients_per_center_in_next_stage = NULL,
      outcome_name = "EBP_proportions"
    ))),
    error = conditionMessage
  )
  expect_match(err, "rank-deficient")
  expect_match(err, "period2, period3")
  expect_no_match(err, "grid_search")
})


test_that("with no restart in the box the winner is projected and recosted", {
  # THE POINT OF THE EXTRACTION. Both the projection and the cost recomputation
  # are only reachable when every restart left the box, which from the outside
  # needs solnl() to stop a tolerance outside every bound on every restart. Here
  # it is just an argument.
  #
  # A recommendation the user's own bounds forbid is not a recommendation, so
  # the winner is brought back onto the box; and the cost has to be that of the
  # point being recommended, not of the point the solver stopped at. Which way
  # the cost then moves depends on which bound was crossed, so the solver's
  # number is not usable as an estimate of it: the test below projects a
  # component down to an upper bound and the cost falls, from 35.5 to 27.
  select_restart_within_bounds <- getFromNamespace(
    "select_restart_within_bounds", "LAGO"
  )

  # none of the three is in the box: column 1 is below the first lower bound,
  # column 2 below the second, column 3 above the first upper bound.
  none_in <- srb_restarts(c(0.9, 2), c(4, 0.5), c(11, 2))
  expect_identical(none_in$costs, c(11.8, 10.5, 32))
  # the precondition this test is about, asserted rather than assumed
  expect_false(any(apply(
    none_in$points, 2,
    function(x) all(x >= srb_lower) && all(x <= srb_upper)
  )))

  chosen <- select_restart_within_bounds(
    none_in$points, none_in$costs, srb_lower, srb_upper, srb_cost
  )

  # the fallback kept them all, so the cheapest of the three wins: column 2, at
  # 10.5. Its second component is 0.5, below its lower bound of 1, and the
  # PROJECTION is what puts it on the bound. Exactly on it, not near it.
  expect_identical(chosen$int_components, c(4, 1))
  expect_identical(chosen$int_components[[2]], srb_lower[[2]])

  # the RECOMPUTATION: the reported cost is srb_cost() at the projected point,
  # 2*4 + 5*1 = 13, and NOT the 10.5 the solver stopped at. The two differ by
  # 2.5, so reporting the solver's cost is a 19% understatement of what the
  # recommendation actually costs.
  expect_identical(chosen$rec_int_cost, 13)
  expect_identical(chosen$rec_int_cost, srb_cost(chosen$int_components))
  expect_false(isTRUE(all.equal(chosen$rec_int_cost, none_in$costs[[2]])))
  expect_gt(chosen$rec_int_cost, none_in$costs[[2]])

  # the same on a SINGLE restart that violates both bounds in opposite
  # directions, so the projection is a pmax on one component and a pmin on the
  # other in the same call. c(0.25, 7) projects to c(1, 5) and costs 27, against
  # the 35.5 the solver reported.
  both_ways <- srb_restarts(c(0.25, 7))
  expect_identical(both_ways$costs, 35.5)
  chosen <- select_restart_within_bounds(
    both_ways$points, both_ways$costs, srb_lower, srb_upper, srb_cost
  )
  expect_identical(chosen$int_components, c(1, 5))
  expect_identical(chosen$rec_int_cost, 27)
  expect_identical(chosen$rec_int_cost, srb_cost(chosen$int_components))
  # here the projection LOWERS the cost, since the binding violation is on the
  # upper bound. The requirement is not a direction, it is that the cost belongs
  # to the returned point.
  expect_lt(chosen$rec_int_cost, both_ways$costs)

  # the returned intervention is inside the box in every one of these, which is
  # the invariant lago_optimization() reports to the user
  for (case in list(none_in, both_ways)) {
    result <- select_restart_within_bounds(
      case$points, case$costs, srb_lower, srb_upper, srb_cost
    )
    expect_true(all(result$int_components >= srb_lower))
    expect_true(all(result$int_components <= srb_upper))
    expect_identical(result$rec_int_cost, srb_cost(result$int_components))
  }
})


# ---------------------------------------------------------------------------
# validate_inputs(): the center weights are renormalised, not merely checked
# ---------------------------------------------------------------------------

test_that("center weights are renormalised exactly when they need it", {
  # The tolerance says the input was MEANT to be a set of weights; it does not
  # make it one. The weights multiply the per-center outcomes and are summed, so
  # a set summing to 1 - d scales EVERY reported outcome by 1 - d: the relative
  # bias is exactly sum(w) - 1, on the estimated outcome, on the goal comparison
  # the recommendation is chosen against, and on the confidence set.
  #
  # Renormalising here rather than tightening the tolerance keeps documented
  # input accepted. The requirement is two-sided: a no-op for a compliant
  # caller, and unbiased for everyone else.
  validate_inputs <- getFromNamespace("validate_inputs", "LAGO")
  get_outcome <- getFromNamespace("get_outcome", "LAGO")

  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  vi <- function(weights) {
    suppressWarnings(suppressMessages(validate_inputs(
      data = pulesa,
      outcome_name = "Proportions",
      outcome_type = "continuous",
      intervention_components = c("AccessMedicines", "AccessBPMachines"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 1),
      outcome_goal = 0.6,
      outcome_goal_intention = "maximize",
      power_goal = NULL,
      power_goal_approach = "unconditional",
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = weights
    )))$center_weights_for_outcome_goal
  }

  # COMPLIANT: bit-identical, not merely equal. x / 1 is exact in floating
  # point, so a caller already summing to 1 is unaffected by construction.
  n_centers <- length(levels(pulesa$Clinic))
  compliant <- rep(1 / n_centers, n_centers)
  expect_identical(sum(compliant), 1)
  expect_identical(vi(compliant), compliant)

  # NON-COMPLIANT but inside the tolerance: 15 weights of 0.0624 and one of
  # 0.0632 sum to 0.9992, which the 0.001 check accepts. It comes back
  # renormalised, and to the same value as dividing by the sum by hand.
  raw <- c(rep(0.0624, n_centers - 1), 0.0632)
  expect_equal(sum(raw), 0.9992)
  expect_lt(abs(sum(raw) - 1), 0.001)
  corrected <- vi(raw)
  expect_identical(corrected, raw / sum(raw))
  expect_identical(sum(corrected), 1)
  expect_false(identical(corrected, raw))

  # the bias this removes, on the estimated outcome, computed by hand. The
  # identity link makes get_outcome() a plain weighted sum of the per-center
  # linear predictors, so scaling the weights scales the result: the relative
  # error is exactly sum(w) - 1 and nothing else.
  center_effects <- c(0.30, 0.45, 0.55, 0.40, 0.52, 0.35, 0.48, 0.42,
                      0.38, 0.50, 0.44, 0.36, 0.46, 0.41, 0.53, 0.39)
  beta <- c(0.05, 0.02, 0.10)
  int_vector <- c(1, 4, 0.8)
  eta <- center_effects + sum(beta * int_vector) - beta[1]
  biased <- get_outcome(raw, center_effects, beta, int_vector, 0, 0, "identity")
  unbiased <- get_outcome(
    corrected, center_effects, beta, int_vector, 0, 0, "identity"
  )
  # both hand derivations, independent of the package
  expect_equal(biased, sum(raw * eta), tolerance = 1e-14)
  expect_equal(unbiased, sum((raw / sum(raw)) * eta), tolerance = 1e-14)
  # the relative bias IS sum(w) - 1, i.e. -8e-4 here
  expect_equal((biased - unbiased) / unbiased, sum(raw) - 1, tolerance = 1e-9)
  expect_equal((biased - unbiased) / unbiased, -8e-4, tolerance = 1e-9)
  # and the unbiased value is the one a weighted mean must give: bracketed by
  # the values it averages, which the biased one is free to leave
  expect_gte(unbiased, min(eta))
  expect_lte(unbiased, max(eta))

  # the tolerance is still the check that the input was MEANT to be weights, so
  # something that is not is refused rather than silently rescaled. 16 weights
  # of 0.05 sum to 0.8.
  expect_error(
    vi(rep(0.05, n_centers)),
    "must sum up to 1"
  )

  # the correction is SILENT: no message and no warning. It is at most 0.1% of a
  # weight and is what the caller already asked for by passing something the
  # tolerance accepts. Warning would fire on rounded input the documentation
  # invites, e.g. three weights written as c(0.333, 0.333, 0.334).
  messages <- character(0)
  warnings <- character(0)
  withCallingHandlers(
    suppressWarnings(suppressMessages(vi(raw))),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(grep("weight", messages, ignore.case = TRUE), 0)
  expect_length(grep("weight", warnings, ignore.case = TRUE), 0)

  # the DEFAULT weights, which validate_inputs() derives from the center sample
  # sizes rather than taking from the caller, already sum to 1 and so are also
  # unaffected. The renormalisation sits after every branch that can produce
  # them, so it covers the caller's weights, the sample-size default and the
  # single-named-center indicator alike.
  expect_identical(sum(vi(NULL)), 1)
})
