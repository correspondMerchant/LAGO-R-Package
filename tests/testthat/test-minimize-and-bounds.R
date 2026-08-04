# Regression tests for three defects that survived the whole suite before they
# were fixed. Each test here is written to FAIL if its fix is reverted:
#
#   1. the "minimize" direction used the wrong inverse on a logit link, which
#      made est_outcome_goal negative and made the outcome goal INERT (every
#      goal returned the same recommendation).
#   2. the recommendation could leave [lower_bounds, upper_bounds] three
#      different ways: below the lower bound, above the upper bound, and
#      non-finite.
#   3. the numerical optimizer selected the WRONG restart, with which.max on a
#      cost it minimizes, and then selected for out-of-box restarts once that
#      was corrected.
#
# The existing minimize coverage (test-optimization.R) uses link = "identity"
# with grid_search, a combination that structurally cannot see a logit-specific
# inverse bug or a solver restart bug, which is why these are separate.
#
# Runtime matters here: every call uses quiet = TRUE and skips the confidence
# set unless the confidence interval is what is being asserted.


# The cost model is a per-component polynomial in that component's value:
# cost = sum_c sum_k coef[[c]][k] * x[c]^(k-1). Recomputing it in the test from
# cost_list_of_vectors is how "the reported cost belongs to the RETURNED
# intervention" is checked, which is exactly what breaks if the solver's own
# cost is reported instead of the cost at the point being recommended.
cost_at <- function(rec_int, cost_list_of_vectors) {
  sum(mapply(
    function(x, coefs) sum(coefs * x^(seq_along(coefs) - 1)),
    rec_int,
    cost_list_of_vectors
  ))
}

# The published BB_data configuration (same model as test-optimization.R's
# first test), parameterised over the pieces these tests vary. Binary outcome,
# logit link, so the minimize flip is the logit one (p -> 1 - p) rather than a
# negation.
bb_config <- function(outcome_goal,
                      outcome_goal_intention,
                      intervention_lower_bounds = c(1, 1),
                      intervention_upper_bounds = c(40, 5),
                      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
                      optimization_method = "numerical",
                      optimization_grid_search_step_size = NULL,
                      include_confidence_set = FALSE,
                      confidence_set_grid_step_size = NULL) {
  args <- list(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    cost_list_of_vectors = cost_list_of_vectors,
    outcome_goal = outcome_goal,
    outcome_goal_intention = outcome_goal_intention,
    optimization_method = optimization_method,
    include_confidence_set = include_confidence_set,
    quiet = TRUE
  )
  if (!is.null(optimization_grid_search_step_size)) {
    args$optimization_grid_search_step_size <- optimization_grid_search_step_size
  }
  if (!is.null(confidence_set_grid_step_size)) {
    args$confidence_set_grid_step_size <- confidence_set_grid_step_size
  }
  args
}

# A THREE-component numerical configuration, shared by the bounds test and the
# rec_int_cost test below because it is the only one either of them has that
# reaches the numerical optimizer's "every restart left the box" fallback.
#
# With two components solnl() always brings at least one restart back inside the
# bounds, so the fallback is dead code and the projection that follows it has
# nothing to move: instrumented over every numerical call the rest of the suite
# makes, the projection displaced the chosen point by exactly 0 and the fallback
# fired 0 times. With three components and a goal that is only just reachable,
# all eleven restarts stop a solver tolerance outside the box, the fallback keeps
# them all, and the winner is 2.0e-07 BELOW its lower bound before the projection
# repairs it. That is what makes the projection, and the cost recomputation at
# the projected point, checkable from outside.
#
# birth_volume_100 is an intervention component here rather than a center
# characteristic, which is what makes this three components and not two; it has
# no center_characteristics argument for that reason and so cannot use
# bb_config().
bb_three_component_config <- function() {
  list(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c(
      "coaching_updt", "launch_duration", "birth_volume_100"
    ),
    intervention_lower_bounds = c(1, 1, 1),
    intervention_upper_bounds = c(41, 6, 11),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8), c(0, 2)),
    outcome_goal = 0.95,
    outcome_goal_intention = "maximize",
    optimization_method = "numerical",
    include_confidence_set = FALSE,
    quiet = TRUE
  )
}

run_lago <- function(args) {
  suppressWarnings(suppressMessages(do.call(lago_optimization, args)))
}


# ---------------------------------------------------------------------------
# 1. the minimize transform's inverse, on a logit link
# ---------------------------------------------------------------------------

test_that("minimize on a logit link reports est_outcome_goal as a probability", {
  # "minimize" is implemented by negating the fitted coefficients and
  # maximizing. On a logit link the reported outcome is a weighted mean of
  # expit(eta), and expit(-eta) = 1 - expit(eta), so negating the coefficients
  # REFLECTS the probability about 1/2. Undoing it with -1 * instead of 1 -
  # produced a negative "probability": -0.5218337 in place of 0.4781663, which
  # is exactly 1 - 0.5218337. That is the sign error, so it is pinned tightly.
  res <- run_lago(bb_config(
    outcome_goal = 0.55, outcome_goal_intention = "minimize",
    include_confidence_set = TRUE, confidence_set_grid_step_size = c(1, 1)
  ))

  # the value itself: the outcome at rec_int = c(1, 1), which is the smallest
  # probability the box allows since both fitted component effects are positive.
  expect_equal(res$est_outcome_goal, 0.4781663, tolerance = 1e-6)
  expect_equal(res$rec_int, c(1, 1), tolerance = 1e-6)

  # the invariants the value has to satisfy, which is what catches a future
  # inverse that is wrong in some other way than a sign. A binary outcome is a
  # probability, and an estimate has to lie inside its own interval.
  expect_gte(res$est_outcome_goal, 0)
  expect_lte(res$est_outcome_goal, 1)
  expect_equal(res$est_outcome_ci, c(lower = 0.444, upper = 0.512),
    tolerance = 1e-6
  )
  expect_gte(res$est_outcome_goal, res$est_outcome_ci[["lower"]])
  expect_lte(res$est_outcome_goal, res$est_outcome_ci[["upper"]])
})

test_that("minimize on a logit link actually applies the outcome goal", {
  # The most valuable thing here. The goal handed to the optimization was
  # negated with -1 * as well, so on a logit link the constraint read
  # "p >= -0.55" -- true for every candidate, i.e. the goal was INERT. Outcome
  # goals of 0.55, 0.30 and 0.10 all returned byte-identical recommendations.
  # 0.55 is above the whole reachable range so the cheapest corner c(1, 1)
  # satisfies it; anything below the reachable minimum cannot be met and the
  # recommendation moves, so a live goal must give a DIFFERENT answer.
  loose <- run_lago(bb_config(0.55, "minimize"))
  tight <- run_lago(bb_config(0.45, "minimize"))
  tighter <- run_lago(bb_config(0.10, "minimize"))

  expect_false(isTRUE(all.equal(loose$rec_int, tight$rec_int)))
  expect_false(isTRUE(all.equal(loose$est_outcome_goal, tight$est_outcome_goal)))
  # the two unreachable goals agree with each other (both fall back the same
  # way) but not with the reachable one, which is the point: the goal is read.
  expect_equal(tight$rec_int, tighter$rec_int, tolerance = 1e-6)

  # and the same holds under grid_search, so this is a property of the flip and
  # not of one optimizer.
  loose_grid <- run_lago(bb_config(0.55, "minimize",
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(2, 1)
  ))
  tight_grid <- run_lago(bb_config(0.45, "minimize",
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(2, 1)
  ))
  expect_false(isTRUE(all.equal(loose_grid$rec_int, tight_grid$rec_int)))
})

test_that("a feasible minimize goal is met in the requested direction", {
  # BB_data's fitted component effects are both positive, so its reachable
  # minimum is at the lower corner and no interior minimize goal binds there.
  # This fits a logit model whose component effects are NEGATIVE, so lowering
  # the outcome costs money and the goal genuinely binds: the returned outcome
  # has to be AT MOST the goal, and a tighter goal has to cost more. Under the
  # -1 * inverse every goal returned the lower corner with a negative
  # est_outcome_goal, so no goal was met in the stated direction at all.
  set.seed(11)
  n <- 400
  d <- data.frame(dose = runif(n, 0, 10), visits = runif(n, 0, 10))
  d$y <- rbinom(n, 1, 1 / (1 + exp(-(2 - 0.45 * d$dose - 0.25 * d$visits))))

  min_config <- function(outcome_goal, optimization_method = "numerical") {
    args <- list(
      data = d, outcome_name = "y", outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("dose", "visits"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 10),
      cost_list_of_vectors = list(c(0, 1), c(0, 3)),
      outcome_goal = outcome_goal, outcome_goal_intention = "minimize",
      optimization_method = optimization_method,
      include_confidence_set = FALSE, quiet = TRUE
    )
    if (optimization_method == "grid_search") {
      args$optimization_grid_search_step_size <- c(0.5, 0.5)
    }
    args
  }

  for (method in c("numerical", "grid_search")) {
    goals <- c(0.60, 0.40, 0.20)
    results <- lapply(goals, function(g) run_lago(min_config(g, method)))

    for (i in seq_along(goals)) {
      # met in the requested direction: at most the goal, not at least it.
      expect_lte(results[[i]]$est_outcome_goal, goals[i] + 1e-6)
      # still a probability, and non-negative, which the sign bug violated.
      expect_gte(results[[i]]$est_outcome_goal, 0)
    }
    # a stricter goal costs strictly more: the goal is doing work at every
    # level, not just at one.
    costs <- vapply(results, function(r) r$rec_int_cost, numeric(1))
    expect_true(all(diff(costs) > 0))
  }
})

test_that("minimize on an identity link is unchanged", {
  # On an identity link the outcome IS the linear predictor, so negating the
  # coefficients negates the outcome and -1 * is the CORRECT inverse there.
  # This pins the identity path to the value it has always had, so a future
  # change cannot "fix" identity by analogy with the logit fix.
  # Same configuration as test-optimization.R's minimize test.
  res <- run_lago(list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("disp", "hp"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(500, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 10, outcome_goal_intention = "minimize",
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(10, 10),
    include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_equal(res$rec_int, c(500, 230), tolerance = 1e-6)
  expect_equal(res$est_outcome_goal, 9.849547, tolerance = 1e-5)

  # and the identity goal binds too: tighter goals give different, costlier
  # recommendations, all meeting the goal from below.
  identity_config <- function(outcome_goal) {
    list(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity",
      intervention_components = c("disp", "hp"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(500, 350),
      cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = outcome_goal, outcome_goal_intention = "minimize",
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(10, 10),
      include_confidence_set = FALSE, quiet = TRUE
    )
  }
  goals <- c(20, 15, 12)
  results <- lapply(goals, function(g) run_lago(identity_config(g)))
  for (i in seq_along(goals)) {
    expect_lte(results[[i]]$est_outcome_goal, goals[i] + 1e-6)
  }
  costs <- vapply(results, function(r) r$rec_int_cost, numeric(1))
  expect_true(all(diff(costs) > 0))
})


# ---------------------------------------------------------------------------
# 2. the bounds invariant
# ---------------------------------------------------------------------------

test_that("the recommendation never leaves the intervention bounds", {
  # THE assertion that was missing. The shrinking method's interpolation was
  # unbounded, so an unachievable outcome goal could return an intervention
  # outside the box the user gave, in three distinct ways. All three are
  # reachable from the public API with no power goal:
  #
  #   NON-FINITE  outcome_goal = 1 on BB_data maximize returned c(Inf, Inf):
  #               the bracket collapses and the interpolation divides by zero.
  #   BELOW       an unachievable goal with lower bounds above the observed
  #               column means returned the unprojected column means, e.g.
  #               c(8.36, 1.25) under lower bounds c(10, 2).
  #   ABOVE       the interpolation extrapolated past its top endpoint, e.g.
  #               c(26.05, 5.03) under upper bounds c(25, 5).
  #
  # The cases below span both goal intentions, both optimizers, feasible and
  # infeasible goals, and both outcome types, and every one of them is checked
  # against the SAME three-part invariant. This is deliberately broad and
  # cheap: it is the assertion that fails loudly on any of the three modes.
  mtcars_config <- function(components, lower, upper, goal, intention,
                            method = "numerical", step = NULL) {
    args <- list(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity",
      intervention_components = components,
      intervention_lower_bounds = lower, intervention_upper_bounds = upper,
      cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = goal, outcome_goal_intention = intention,
      optimization_method = method,
      include_confidence_set = FALSE, quiet = TRUE
    )
    if (!is.null(step)) args$optimization_grid_search_step_size <- step
    args
  }

  cases <- list(
    # --- non-finite mode: goal 1 is unreachable for any probability model ---
    "binary maximize infeasible goal=1, numerical" =
      bb_config(1, "maximize"),
    "binary maximize infeasible goal=1, grid_search" =
      bb_config(1, "maximize",
        optimization_method = "grid_search",
        optimization_grid_search_step_size = c(10, 2)
      ),
    # --- below-lower-bound mode: lower bounds above the observed means ---
    "binary maximize infeasible, lower bounds above data means, numerical" =
      bb_config(0.9999, "maximize", intervention_lower_bounds = c(10, 2)),
    "binary maximize infeasible, lower bounds above data means, grid_search" =
      bb_config(0.9999, "maximize",
        intervention_lower_bounds = c(10, 2),
        optimization_method = "grid_search",
        optimization_grid_search_step_size = c(10, 1)
      ),
    "continuous minimize infeasible, lower bounds above data means, numerical" =
      mtcars_config(c("gear", "qsec"), c(5, 15), c(10, 30), 10, "minimize"),
    "continuous minimize infeasible, lower bounds above data means, grid" =
      mtcars_config(c("gear", "qsec"), c(5, 15), c(10, 30), 10, "minimize",
        method = "grid_search", step = c(1, 5)
      ),
    # --- above-upper-bound mode: the interpolation overshot its endpoint ---
    "binary maximize infeasible, tight upper bounds, grid_search" =
      bb_config(0.99, "maximize",
        intervention_upper_bounds = c(25, 5),
        optimization_method = "grid_search",
        optimization_grid_search_step_size = c(5, 1)
      ),
    "continuous maximize infeasible, tight upper bounds, grid_search" =
      mtcars_config(c("gear", "qsec"), c(5, 15), c(6, 25), 40, "maximize",
        method = "grid_search", step = c(2, 5)
      ),
    # --- feasible goals: the optimizer paths, not the shrinking fallback ---
    "binary maximize feasible, numerical" =
      bb_config(0.85, "maximize"),
    "binary maximize feasible, grid_search" =
      bb_config(0.85, "maximize",
        optimization_method = "grid_search",
        optimization_grid_search_step_size = c(10, 1)
      ),
    "binary minimize feasible, numerical" =
      bb_config(0.55, "minimize"),
    "binary minimize infeasible, grid_search" =
      bb_config(0.30, "minimize",
        optimization_method = "grid_search",
        optimization_grid_search_step_size = c(10, 1)
      ),
    "continuous maximize feasible, numerical" =
      mtcars_config(c("gear", "qsec"), c(0, 0), c(10, 350), 40, "maximize"),
    "continuous minimize feasible, numerical" =
      mtcars_config(c("disp", "hp"), c(0, 0), c(500, 350), 15, "minimize"),
    "continuous minimize infeasible, grid_search" =
      mtcars_config(c("disp", "hp"), c(0, 0), c(500, 350), 5, "minimize",
        method = "grid_search", step = c(50, 50)
      ),
    # --- the numerical fallback where EVERY restart left the box -------------
    # The only case in this file that reaches it, and the only reason the
    # projection onto the bounds is checked at all: with two components some
    # restart always lands inside, so the projection never has to move
    # anything. Here all eleven restarts stop outside, the winner is 2.0e-07
    # below its lower bound of 1, and only the projection brings it back. See
    # bb_three_component_config().
    "binary maximize feasible, THREE components, numerical" =
      bb_three_component_config()
  )

  for (label in names(cases)) {
    args <- cases[[label]]
    res <- run_lago(args)
    lower <- args$intervention_lower_bounds
    upper <- args$intervention_upper_bounds

    # the three-part invariant. Compared exactly, with no tolerance: a
    # recommendation the user cannot implement is not "close enough", and the
    # numerical path now projects onto the box so equality at a bound is exact.
    expect_true(all(is.finite(res$rec_int)), info = label)
    expect_true(all(res$rec_int >= lower), info = label)
    expect_true(all(res$rec_int <= upper), info = label)
  }
})

test_that("the shrinking fallback returns the projected previous intervention", {
  # The specific value behind the below-lower-bound mode. shrink_to_int_values
  # defaults to the observed column means, which are a property of the DATA and
  # need not respect the bounds; BB_data's means are c(8.365, 0.810). With
  # lower bounds c(10, 2) the unprojected means were returned as the
  # recommendation, an intervention the user's own bounds forbid. The fix
  # projects them onto the box first, so the recommendation is at worst the
  # projected point c(10, 2), never the raw means.
  observed_means <- colMeans(BB_data[, c("coaching_updt", "launch_duration")])
  expect_true(observed_means[["coaching_updt"]] < 10)
  expect_true(observed_means[["launch_duration"]] < 2)

  res <- run_lago(bb_config(0.9999, "maximize",
    intervention_lower_bounds = c(10, 2)
  ))
  expect_true(all(res$rec_int >= c(10, 2)))
  expect_false(isTRUE(all.equal(as.numeric(res$rec_int),
    as.numeric(observed_means),
    tolerance = 1e-6
  )))
})

test_that("an empty shrink bracket falls back to stage 1, not to the upper corner", {
  # The other half of the shrinking method's bracket guard, and the only defect
  # in this file that the bounds invariant structurally cannot see: BOTH answers
  # are inside the box, so its VALUE is the signal and is pinned here on purpose.
  #
  # The interpolation runs over [beta_min, beta_max] with beta_min = beta_max/2,
  # which brackets beta_max from below only while beta_max > 0. When it does not
  # -- beta_max - beta_min <= 0 -- there is no bracket, no fraction to compute,
  # and the answer is the stage-1 intervention the caller's warning promised.
  #
  # outcome_goal = 1 is unreachable under a logit link, expit() being strictly
  # below 1, so this is the shrinking path. It also makes the bracket EMPTY in
  # the zero-width way rather than the negative-width way: the binary search
  # accepts mid = 0 as its lower end, then the next probe is within 1e-6 of the
  # goal and breaks out, and `beta_max <- left` keeps that 0. Both components
  # come back with beta_max == beta_min == 0 exactly (measured, not inferred).
  #
  # With the guard removed, beta_vec[c] > 0 == beta_max sends both components to
  # the NEXT branch, the >= beta_max clamp, so the recommendation is up[c] for
  # every component at once: the maximum-everything corner, legal and expensive
  # and nothing to do with stage 1. The two guards are therefore not
  # independent -- the lower one is what stops a degenerate bracket from being
  # read as "already past the top" -- which is why deleting it alone still
  # returns an in-box answer and the bounds invariant stays green.
  observed_means <- colMeans(BB_data[, c("coaching_updt", "launch_duration")])
  # the stage-1 intervention here is the observed column means projected onto
  # the bounds, since no prev_recommended_interventions was supplied
  stage_1 <- pmin(pmax(observed_means, c(1, 1)), c(40, 5))

  for (method in c("numerical", "grid_search")) {
    args <- bb_config(1, "maximize", optimization_method = method)
    if (method == "grid_search") {
      args$optimization_grid_search_step_size <- c(10, 2)
    }
    res <- run_lago(args)

    # the stage-1 intervention itself, to the last bit: it is not computed, it
    # is returned unchanged, so there is no solver tolerance to allow for.
    expect_equal(as.numeric(res$rec_int), as.numeric(stage_1),
      tolerance = 1e-12, info = method
    )
    expect_equal(res$rec_int_cost,
      cost_at(stage_1, args$cost_list_of_vectors),
      tolerance = 1e-12, info = method
    )
    # and NOT the corner. Stated separately because it is the actual failure
    # mode and it is worth naming: c(40, 5) costs 108, nearly 5x the 22.22 the
    # stage-1 fallback costs, and both are inside the bounds.
    expect_false(
      isTRUE(all.equal(as.numeric(res$rec_int), c(40, 5), tolerance = 1e-6)),
      info = method
    )
    expect_lt(res$rec_int_cost, 30)
  }

  # The same guard at the unit level, where the three cases are pure and cheap
  # to separate -- and in its other degenerate mode, which is why these are not
  # a restatement of the run above. beta_max is the coefficient the component
  # would need at its upper bound to reach the goal alone; with the other
  # component already pinning the outcome above the goal it comes back
  # NEGATIVE, so beta_min = beta_max / 2 sits ABOVE beta_max and the bracket is
  # INVERTED rather than merely zero-width. Both modes have to reach the
  # stage-1 fallback, and only the "<= 0" comparison covers both.
  shrinking_method <- getFromNamespace("shrinking_method", "LAGO")
  shrink <- function(beta, goal = 0.85, stage_1 = c(8.365, 0.81)) {
    shrinking_method(
      lo = c(1, 1), up = c(40, 5), beta = beta, outcome_goal = goal,
      include_interaction_terms = FALSE,
      intervention_components = c("a", "b"), main_components = NULL,
      all_center_lvl_effects = 0, center_weights_for_outcome_goal = 1,
      center_cha_coeff_vec = 0, center_cha = 0, link = "logit",
      stage_1_intervention = stage_1
    )
  }
  # both components effective enough that neither needs a positive beta_max:
  # both brackets are empty and both components fall back to stage 1
  expect_equal(shrink(c(0.1, 5, 5)), c(8.365, 0.81), tolerance = 1e-12)
  # only the first component is effective, so only the second one's bracket is
  # empty. The first is above a usable beta_max and correctly clamps to up[1],
  # which is the neighbouring branch and must keep working.
  expect_equal(shrink(c(0.1, 5, -0.5)), c(40, 0.81), tolerance = 1e-12)
  # neither is effective: beta_max is positive and beta_vec is below beta_min,
  # so this is the ordinary below-the-bracket fallback, same answer by a
  # different branch. Included so the pin above is not read as "stage 1 always".
  expect_equal(shrink(c(0.1, 0.01, 0.01)), c(8.365, 0.81), tolerance = 1e-12)

  # an identity link with negated coefficients, which is how the "minimize"
  # direction reaches this branch: beta_max is routinely <= 0 there, so the
  # guard is not a binary-outcome curiosity.
  expect_equal(
    shrinking_method(
      lo = c(0, 0), up = c(10, 10), beta = c(-2, 0.45, 0.25),
      outcome_goal = -0.2, include_interaction_terms = FALSE,
      intervention_components = c("a", "b"), main_components = NULL,
      all_center_lvl_effects = 0, center_weights_for_outcome_goal = 1,
      center_cha_coeff_vec = 0, center_cha = 0, link = "identity",
      stage_1_intervention = c(3, 4)
    ),
    c(3, 4),
    tolerance = 1e-12
  )
})

test_that("an unachievable minimize goal warns about the right extreme", {
  # The warning text is direction-specific: under minimize the goal is BELOW
  # everything reachable, so the sentence has to name the minimum estimated
  # achievable outcome. Only the maximize sentence used to exist, so under
  # minimize the message described a comparison that had not happened. This was
  # unreachable until the minimize goal constraint started binding, since a
  # vacuous constraint is never unachievable.
  # The lower bounds sit above the smallest observed values, so validation also
  # warns about each component. Those warnings are not what this test is about
  # and expect_warning() only consumes the first one it matches, so they are
  # suppressed here to keep them out of the reporter's warning list.
  expect_warning(
    withCallingHandlers(
      suppressMessages(do.call(
        lago_optimization,
        bb_config(0.10, "minimize")
      )),
      warning = function(w) {
        if (grepl("greater than the minimum value", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    "minimum estimated achievable outcome"
  )
  # the maximize direction still names the maximum.
  expect_warning(
    withCallingHandlers(
      suppressMessages(do.call(
        lago_optimization,
        bb_config(1, "maximize")
      )),
      warning = function(w) {
        if (grepl("greater than the minimum value", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    "maximum estimated achievable outcome"
  )
})


# ---------------------------------------------------------------------------
# 3. the numerical optimizer's restart selection
# ---------------------------------------------------------------------------

test_that("the numerical optimizer picks the cheapest in-box restart", {
  # solnl() is run from eleven starting points and one is selected. The
  # objective is COST and it is minimized, so the best restart is the cheapest
  # one -- but the selection used which.max, returning the most EXPENSIVE
  # candidate that satisfied the goal.
  #
  # With a linear cost every restart converges to the same point, which is why
  # the existing tests never saw this. A cubic cost (the package's own default
  # cost shape, built by cost_fxn_calculator()) makes the restarts land in
  # genuinely different places, and the spread is large: the cheapest restart
  # costs 20.05 and the most expensive 39.16. Selecting the wrong end is then
  # unmistakable.
  cost_fxn_calculator <- getFromNamespace("cost_fxn_calculator", "LAGO")
  cubic_costs <- cost_fxn_calculator(
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 5),
    unit_costs = c(1.7, 8),
    default_cost_fxn_type = "cubic"
  )

  args <- bb_config(0.85, "maximize",
    intervention_lower_bounds = c(0, 0),
    cost_list_of_vectors = cubic_costs
  )
  res <- run_lago(args)

  # the goal is still met, so this is a comparison among feasible candidates.
  expect_equal(res$est_outcome_goal, 0.85, tolerance = 1e-6)
  # the cheap solution, not the expensive one. which.max returned
  # c(18.597, 2.347) at cost 39.157 for this exact configuration.
  expect_equal(res$rec_int, c(0, 2.803008), tolerance = 1e-5)
  expect_equal(res$rec_int_cost, 20.047258, tolerance = 1e-5)

  # a grid search over the same problem cannot beat the numerical optimum by
  # more than one grid step's worth of cost. This is the optimizer-independent
  # form of the same claim: under which.max the numerical answer was almost
  # twice the cost of a coarse grid's answer, which no correct optimizer can be.
  grid_args <- args
  grid_args$optimization_method <- "grid_search"
  grid_args$optimization_grid_search_step_size <- c(4, 1)
  grid_res <- run_lago(grid_args)
  expect_lte(res$rec_int_cost, grid_res$rec_int_cost)
})

test_that("rec_int_cost is the cost of the returned rec_int", {
  # What this pins is the in-box filter: restarts that left the box are dropped
  # before the cheapest is chosen, because solnl() treats the box as soft and
  # the cheapest restart is cheapest precisely by sitting outside the bounds.
  #
  # It also pins the projection onto the bounds that follows, and the
  # recomputation of the cost at the projected point, but ONLY through the
  # three-component case at the end of the list. Every two-component
  # configuration leaves at least one restart inside the box, so the filter's
  # survivor is already implementable and the projection has nothing to move --
  # measured over every numerical call this suite makes, it displaced the chosen
  # point by exactly 0. The three-component case is the one where all eleven
  # restarts stop outside, the fallback keeps them all, and the projection moves
  # the winner by 2.0e-07: without it the recommendation is below its lower
  # bound, and with the solver's own cost reported instead of the recomputed one
  # the reported cost is 3.4e-07 away from the cost of the point returned. The
  # tolerance of 1e-10 below is tight enough to see that.
  #
  # Recomputing the cost polynomial in the test is what makes the filter
  # checkable from the outside: the reported cost has to agree with the
  # returned intervention.
  cost_fxn_calculator <- getFromNamespace("cost_fxn_calculator", "LAGO")
  cubic_costs <- cost_fxn_calculator(
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 5),
    unit_costs = c(1.7, 8),
    default_cost_fxn_type = "cubic"
  )

  cases <- list(
    "linear cost, maximize" = bb_config(0.85, "maximize"),
    "linear cost, maximize, goal near the lower corner" =
      bb_config(0.60, "maximize"),
    "linear cost, minimize" = bb_config(0.55, "minimize"),
    "cubic cost, maximize" = bb_config(0.85, "maximize",
      intervention_lower_bounds = c(0, 0),
      cost_list_of_vectors = cubic_costs
    ),
    "cubic cost, maximize, low goal" = bb_config(0.50, "maximize",
      intervention_lower_bounds = c(0, 0),
      cost_list_of_vectors = cubic_costs
    ),
    # the shrinking fallback reports a cost too, computed at the shrunk
    # intervention rather than by an optimizer, so it is held to the same
    # consistency requirement. This case is also where the unbounded
    # interpolation returned a non-finite intervention.
    "linear cost, maximize, unachievable goal (shrinking fallback)" =
      bb_config(1, "maximize"),
    "linear cost, minimize, unachievable goal (shrinking fallback)" =
      bb_config(0.10, "minimize"),
    # the one case that reaches the all-restarts-out-of-box fallback, so the
    # one case where the projection and the cost recomputation are live. See
    # the note above and bb_three_component_config().
    "linear cost, maximize, THREE components (projection fallback)" =
      bb_three_component_config()
  )

  for (label in names(cases)) {
    args <- cases[[label]]
    res <- run_lago(args)

    # in the box, exactly: the projection makes a bound-valued component land
    # on the bound rather than a solver tolerance outside it.
    expect_true(all(res$rec_int >= args$intervention_lower_bounds), info = label)
    expect_true(all(res$rec_int <= args$intervention_upper_bounds), info = label)
    # and the reported cost belongs to the point being recommended.
    expect_equal(
      res$rec_int_cost,
      cost_at(res$rec_int, args$cost_list_of_vectors),
      tolerance = 1e-10,
      info = label
    )
  }
})

test_that("unit_costs and the equivalent cost_list_of_vectors agree", {
  # The cubic restart case above is reachable through the ordinary public
  # entry point too: unit_costs + default_cost_fxn_type = "cubic" is the
  # documented way to get a cubic cost, and it goes through the same restart
  # selection. Pinning the two forms to the same answer is what stops the
  # regression above from being an artefact of hand-written coefficients.
  cost_fxn_calculator <- getFromNamespace("cost_fxn_calculator", "LAGO")
  cubic_costs <- cost_fxn_calculator(
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 5),
    unit_costs = c(1.7, 8),
    default_cost_fxn_type = "cubic"
  )

  explicit <- run_lago(bb_config(0.85, "maximize",
    intervention_lower_bounds = c(0, 0),
    cost_list_of_vectors = cubic_costs
  ))

  from_unit_costs_args <- bb_config(0.85, "maximize",
    intervention_lower_bounds = c(0, 0)
  )
  from_unit_costs_args$cost_list_of_vectors <- NULL
  from_unit_costs_args$unit_costs <- c(1.7, 8)
  from_unit_costs_args$default_cost_fxn_type <- "cubic"
  from_unit_costs <- run_lago(from_unit_costs_args)

  expect_equal(from_unit_costs$rec_int, explicit$rec_int, tolerance = 1e-8)
  expect_equal(from_unit_costs$rec_int_cost, explicit$rec_int_cost,
    tolerance = 1e-8
  )
  # and the cheap end, not the expensive end, on this path as well.
  expect_equal(from_unit_costs$rec_int_cost, 20.047258, tolerance = 1e-5)
})
