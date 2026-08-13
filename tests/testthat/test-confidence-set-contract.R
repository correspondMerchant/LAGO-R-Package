# Regression tests for the CONTRACT of the confidence set (PR #68).
#
# get_confidence_set() prepends rec_int to the grid so its interval is computed
# alongside the grid's, and the four defects pinned here all came from treating
# that prepended row as if it were part of the confidence set, or from indexing
# the interval matrix by anything other than the grid row number:
#
#   1. rec_int was identified BY GRID POSITION (row 1). Row 1 is the
#      recommendation only when its own interval covers the goal; otherwise it
#      is an ordinary grid point, and reading it as the recommendation reported
#      the wrong interval, deleted a genuine member, mis-sized the set, and
#      threw away a one-point set as empty. The contract now: rec_int_ci is its
#      own field, $cs holds qualifying GRID points only, and the size is
#      qualifying grid points over grid points.
#   2. Coordinates and intervals were MISPAIRED once complete.cases() dropped a
#      row: the interval matrix was indexed by post-filter row numbers and the
#      grid by original ones.
#   3. The hand-built variance matrix enumerated factor levels by FIRST
#      APPEARANCE while glm() uses level order, so a row permutation of the
#      same data changed the answer.
#   4. The interval was computed at the hardcoded LAST period while the point
#      estimate used the requested one, so the two could describe different
#      periods and the interval could exclude the estimate.
#
# The tests prefer invariants (nrow == size * n_grid, an interval recomputed at
# the row's own coordinates, a permutation giving an identical object) over
# copied output, so they keep their meaning if the numbers legitimately move.

# How close the confidence set's boundary runs to the outcome goal, in relative
# terms. Membership is decided by whether an intervention's interval brackets
# the goal, so an interval endpoint sitting a hair from the goal means a tiny
# coefficient change moves that intervention across the boundary and changes
# the row count with nothing being wrong. Row-count fixtures are guarded with
# this so a boundary shift reports itself as a fixture to update rather than as
# a broken confidence set.
#
# The margin is computed over the WHOLE grid, not over the returned rows: the
# tightest case is an intervention just OUTSIDE the set, which is exactly the
# one about to move in. It also recomputes the intervals unrounded, because the
# returned bounds are rounded to three decimals and would hide any margin
# finer than that. The recomputation is the delta method the package documents,
# fitted with glm() here rather than taken from the result, so it does not
# depend on the code being guarded.
cs_boundary_margin <- function(args, outcome_goal) {
  model <- glm(
    stats::as.formula(paste(
      args$outcome_name, "~",
      paste(c(
        args$intervention_components, args$center_characteristics
      ), collapse = " + ")
    )),
    data = args$data, family = args$glm_family
  )
  grid <- expand.grid(
    seq(
      args$intervention_lower_bounds[1], args$intervention_upper_bounds[1],
      by = args$confidence_set_grid_step_size[1]
    ),
    seq(
      args$intervention_lower_bounds[2], args$intervention_upper_bounds[2],
      by = args$confidence_set_grid_step_size[2]
    )
  )
  new_data <- cbind(
    1, as.matrix(grid), args$center_characteristics_optimization_values
  )
  eta <- as.numeric(new_data %*% coef(model))
  se <- sqrt(diag(new_data %*% vcov(model) %*% t(new_data)))
  p <- rje::expit(eta)
  half <- stats::qnorm(0.975) * se * p * (1 - p)
  min(abs(c(p - half, p + half) - outcome_goal)) / outcome_goal
}

# The published BB_data configuration, as arguments for lago_optimization().
# 40 coaching values by 5 launch durations at step c(1, 1) is a 200-point grid,
# which is the denominator every size assertion below is checked against.
bb_cs_args <- function(outcome_goal, outcome_goal_intention, step = c(1, 1)) {
  list(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = outcome_goal,
    outcome_goal_intention = outcome_goal_intention,
    include_confidence_set = TRUE,
    confidence_set_grid_step_size = step,
    quiet = TRUE
  )
}

bb_optimize <- function(...) {
  suppressWarnings(suppressMessages(do.call(lago_optimization, bb_cs_args(...))))
}

# the number of grid interventions the confidence set is reported over, i.e.
# the denominator of confidence_set_size_percentage. Computed the same way
# get_confidence_set() builds the grid, so the invariant checks below do not
# hardcode a count.
n_grid_points <- function(lower, upper, step) {
  prod(vapply(
    seq_along(lower),
    function(i) length(seq(lower[i], upper[i], by = step[i])),
    numeric(1)
  ))
}

# The 95% interval for a binary/logit outcome at one prediction row, computed
# straight from coef() and vcov() of the fitted model. This is the formula
# get_confidence_set()'s binary branch uses (delta method on the probability
# scale), written out independently so the reported interval is checked against
# a hand computation and not against itself.
logit_ci_at <- function(model, x, alpha = 0.05) {
  b <- coef(model)
  x <- x[names(b)]
  p <- rje::expit(sum(x * b))
  se <- sqrt(as.numeric(t(x) %*% vcov(model)[names(b), names(b)] %*% x)) *
    p * (1 - p)
  round(c(lower = p - qnorm(1 - alpha / 2) * se,
          upper = p + qnorm(1 - alpha / 2) * se), 3)
}

# the prediction row of the BB_data configuration at one intervention, named
# with the coefficients it multiplies
bb_pred_row <- function(rec_int) {
  c(
    "(Intercept)" = 1,
    coaching_updt = rec_int[1],
    launch_duration = rec_int[2],
    birth_volume_100 = 1.75
  )
}

# A direct get_confidence_set() call on the BB_data configuration, so the
# recommendation's interval can be read from its own rec_int_ci field rather
# than from anything lago_optimization() reports about it.
bb_get_confidence_set <- function(model, rec_int, outcome_goal, step = c(1, 1)) {
  predictors <- c("coaching_updt", "launch_duration", "birth_volume_100")
  suppressWarnings(get_confidence_set(
    # BB_data is a tibble; predictors_data is indexed by column name, so a
    # plain data.frame keeps that indexing simple
    predictors_data = as.data.frame(BB_data)[, predictors, drop = FALSE],
    intervention_components = c("coaching_updt", "launch_duration"),
    outcome_data = BB_data$pp3_oxytocin_mother,
    fitted_model = model,
    link = "logit",
    outcome_goal = outcome_goal,
    outcome_type = "binary",
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    confidence_set_grid_step_size = step,
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    rec_int = rec_int
  ))
}

test_that("the confidence set holds qualifying GRID points and is sized over the grid", {
  # rec_int is prepended to the grid to get its own interval computed, and used
  # to be returned as row 1 of the set and subtracted from the count. Both are
  # wrong, in two different ways that the two configurations here cover between
  # them:
  #   minimize 0.55: rec_int IS a grid point and its interval does NOT cover
  #     the goal, so row 1 is an ordinary grid point. Deleting it dropped a
  #     genuine member (7 reported as 6) and subtracting it under-sized the set.
  #   maximize 0.85: rec_int's interval DOES cover the goal (the recommendation
  #     is chosen to reach it) but rec_int is NOT a grid point: its
  #     launch_duration is about 2.78 while the grid steps through whole days.
  #     So the prepended row qualified and was returned as a member of a set
  #     that is supposed to hold grid points only.
  grid_n <- n_grid_points(c(1, 1), c(40, 5), c(1, 1))
  expect_equal(grid_n, 200)

  for (cfg in list(
    list(
      goal = 0.55, intention = "minimize", size = 0.035, rows = 7,
      rec_int_covers_goal = FALSE
    ),
    list(
      goal = 0.85, intention = "maximize", size = 0.09, rows = 18,
      rec_int_covers_goal = TRUE
    )
  )) {
    res <- bb_optimize(cfg$goal, cfg$intention)

    # THE invariant: the size is the number of qualifying grid points over the
    # number of grid points, so the two determine each other exactly. The old
    # code subtracted the prepended row from a count that never included it and
    # divided by a grid it had widened by one.
    expect_equal(nrow(res$cs), res$confidence_set_size_percentage * grid_n)

    # The reference row count is a fixture, not an invariant, and it sits on a
    # narrow margin: at the maximize goal of 0.85 the nearest excluded grid
    # intervention, c(5, 3), has a lower confidence bound of 0.85014, only
    # 1.4e-4 above the goal. A coefficient perturbation of that relative size,
    # from another BLAS or a new glm, would move it into the set and change the
    # count without anything being wrong. The margin is therefore asserted
    # first, so that case reports itself as a fixture that needs updating
    # rather than as a broken confidence set.
    expect_gt(
      cs_boundary_margin(bb_cs_args(cfg$goal, cfg$intention), cfg$goal), 1e-4
    )
    expect_equal(res$confidence_set_size_percentage, cfg$size, tolerance = 1e-6)
    expect_equal(nrow(res$cs), cfg$rows)

    # whether rec_int's own interval covers the goal is asserted, not assumed:
    # it is what decides which of the two failure modes above the configuration
    # exercises, so a fixture that quietly changed sides would still pass the
    # invariants while pinning something else.
    expect_equal(
      res$est_outcome_ci[["lower"]] <= cfg$goal &&
        cfg$goal <= res$est_outcome_ci[["upper"]],
      cfg$rec_int_covers_goal
    )
    # either way rec_int is not a row of $cs
    expect_false(any(
      abs(res$cs$coaching_updt - res$rec_int[1]) < 1e-9 &
        abs(res$cs$launch_duration - res$rec_int[2]) < 1e-9
    ))

    # every returned row IS a grid point, and every row's interval covers the
    # goal (the definition of the set, checked on the rounded bounds it reports)
    expect_true(all(res$cs$coaching_updt %in% seq(1, 40, by = 1)))
    expect_true(all(res$cs$launch_duration %in% seq(1, 5, by = 1)))
    expect_true(all(
      res$cs$CI_lower_bound <= cfg$goal & cfg$goal <= res$cs$CI_upper_bound
    ))
  }
})

test_that("get_confidence_set() itself returns grid points only, with rec_int_ci apart", {
  # the same contract asserted on the exported function directly, which is
  # where it is implemented. lago_optimization() used to paper over half of it
  # by stripping row 1 positionally on the way out, so a caller of
  # get_confidence_set() got the prepended row as a member of the set and no
  # interval for the recommendation at all.
  grid_n <- n_grid_points(c(1, 1), c(40, 5), c(1, 1))

  for (cfg in list(
    list(goal = 0.55, intention = "minimize", size = 0.035, rows = 7),
    list(goal = 0.85, intention = "maximize", size = 0.09, rows = 18)
  )) {
    opt <- bb_optimize(cfg$goal, cfg$intention)
    direct <- bb_get_confidence_set(opt$model, opt$rec_int, cfg$goal)

    expect_equal(nrow(direct$cs), direct$confidence_set_size_percentage * grid_n)
    expect_equal(nrow(direct$cs), cfg$rows)
    expect_equal(
      direct$confidence_set_size_percentage, cfg$size, tolerance = 1e-6
    )

    # rec_int's interval is its own field, always computed, and matches a hand
    # computation from coef()/vcov() at rec_int's coordinates
    expect_false(is.null(direct$rec_int_ci))
    expect_equal(
      direct$rec_int_ci,
      logit_ci_at(opt$model, bb_pred_row(opt$rec_int)),
      tolerance = 1e-6
    )
    # ... and rec_int is not among the returned rows
    expect_false(any(
      abs(direct$cs$coaching_updt - opt$rec_int[1]) < 1e-9 &
        abs(direct$cs$launch_duration - opt$rec_int[2]) < 1e-9
    ))
    # the object lago_optimization() reports is this one, untrimmed
    expect_equal(opt$cs, direct$cs)
    expect_equal(opt$est_outcome_ci, direct$rec_int_ci, tolerance = 1e-6)
  }
})

test_that("est_outcome_ci is the interval AT rec_int, not the interval of grid row 1", {
  # est_outcome_ci used to be read off row 1 of the returned confidence set,
  # which is the recommendation's interval only when that interval covers the
  # goal. Here it does not, so row 1 is an ordinary grid point and its interval
  # was reported as the recommendation's.
  res <- bb_optimize(0.55, "minimize")

  # the recommendation is c(1, 1) and its interval is computed at c(1, 1) ...
  expect_equal(res$rec_int, c(1, 1), tolerance = 1e-6)
  # ... which a hand computation from coef()/vcov() reproduces exactly
  expect_equal(
    res$est_outcome_ci,
    logit_ci_at(res$model, bb_pred_row(res$rec_int)),
    tolerance = 1e-6
  )
  # ... and which get_confidence_set() reports in its own rec_int_ci field
  direct <- bb_get_confidence_set(res$model, res$rec_int, 0.55)
  expect_equal(res$est_outcome_ci, direct$rec_int_ci, tolerance = 1e-6)
  # the two calls agree on the set itself as well, so nothing is trimmed on the
  # way out of lago_optimization()
  expect_equal(res$cs, direct$cs)
  expect_equal(
    res$confidence_set_size_percentage, direct$confidence_set_size_percentage
  )

  # and it is NOT row 1 of the set: a positional read would have reported the
  # first qualifying grid point's interval instead, which is a different
  # interval of a different intervention.
  expect_false(isTRUE(all.equal(
    unname(res$est_outcome_ci),
    c(res$cs$CI_lower_bound[1], res$cs$CI_upper_bound[1])
  )))
  # the interval belongs to rec_int and the estimated outcome is inside it
  expect_true(
    res$est_outcome_ci[["lower"]] <= res$est_outcome_goal &&
      res$est_outcome_goal <= res$est_outcome_ci[["upper"]]
  )
})

test_that("a confidence set of exactly one grid point is reported, not discarded", {
  # the old code returned NULL and size 0 whenever at most one row qualified,
  # on the reasoning that the single row could only be the prepended rec_int.
  # It can just as well be a single qualifying GRID point, which is a confidence
  # set of one and was silently reported as empty.
  grid_n <- n_grid_points(c(1, 1), c(40, 5), c(5, 1))
  res <- bb_optimize(0.52, "minimize", step = c(5, 1))

  expect_equal(nrow(res$cs), 1)
  expect_equal(res$confidence_set_size_percentage, 1 / grid_n, tolerance = 1e-9)
  # the one member is a grid point whose interval covers the goal, and is not
  # the recommendation c(1, 1)
  expect_equal(res$cs$coaching_updt, 6)
  expect_equal(res$cs$launch_duration, 1)
  expect_true(res$cs$CI_lower_bound <= 0.52 && 0.52 <= res$cs$CI_upper_bound)
})

test_that("a grid point that attains the goal EXACTLY qualifies, and is the answer", {
  # The inequality that decides which grid points qualify is >=, not >, and no
  # other test can tell the two apart: every fixture's goal falls strictly
  # between two grid points' outcomes, so the boundary case never arises.
  #
  # Landing on it exactly cannot be arranged by choosing a round-looking goal and
  # hoping. It is arranged the other way round: the model is fitted first and the
  # goal is DERIVED from a grid point's own fitted outcome, so the two are the
  # same double by construction and the comparison is exact in floating point.
  # The fixture below is deterministic (a fixed sinusoidal wobble rather than
  # rnorm) and the coefficients LAGO fits are identical() to the ones the goal is
  # derived from, which is asserted rather than assumed.
  #
  # This lives in the contract file rather than the optimizer file because the
  # same inequality governs confidence-set membership: the interval at the
  # exactly-attaining point has to cover the goal too, and it is asserted below.
  d <- data.frame(x1 = rep(0:4, each = 8))
  d$y <- 1 + 2 * d$x1 + 0.01 * sin(seq_len(nrow(d)))
  model <- glm(y ~ x1, data = d, family = gaussian())
  # the fitted outcome AT the grid point x1 == 2, which is what the goal is set
  # to. Every grid point below x1 == 2 falls short of it and every one above
  # exceeds it, so x1 == 2 qualifies on equality alone.
  exact_goal <- as.numeric(
    coef(model)[["(Intercept)"]] + coef(model)[["x1"]] * 2
  )

  args <- list(
    data = d, outcome_name = "y", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = "x1",
    intervention_lower_bounds = 0, intervention_upper_bounds = 4,
    cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = exact_goal, outcome_goal_intention = "maximize",
    optimization_method = "grid_search",
    optimization_grid_search_step_size = 1,
    include_confidence_set = TRUE, confidence_set_grid_step_size = 1,
    quiet = TRUE
  )
  res <- suppressWarnings(suppressMessages(do.call(lago_optimization, args)))

  # the fixture's whole point: the goal is the fitted outcome of a grid point,
  # bit for bit, so ">= goal" and "> goal" disagree on exactly that point.
  expect_identical(unname(coef(res$model)), unname(coef(model)))
  expect_identical(res$est_outcome_goal, exact_goal)

  # x1 == 2 attains the goal and costs 2. Under ">" it is skipped and the next
  # grid point up, x1 == 3, is recommended instead: a 50% cost overrun for an
  # outcome the user never asked for.
  expect_equal(res$rec_int, 2)
  expect_equal(res$rec_int_cost, 2)
  expect_false(isTRUE(all.equal(as.numeric(res$rec_int), 3)))

  # and the confidence set agrees: the exactly-attaining point is in it, with an
  # interval that brackets the goal.
  expect_equal(nrow(res$cs), 1)
  expect_equal(res$cs$x1, 2)
  expect_true(
    res$cs$CI_lower_bound <= exact_goal & exact_goal <= res$cs$CI_upper_bound
  )
  expect_equal(res$confidence_set_size_percentage, 1 / 5)
})

test_that("rec_int_ci is reported even when no grid point qualifies", {
  # the recommendation's interval does not depend on the confidence set being
  # non-empty, so it is returned either way. It used to be read out of the set,
  # so an empty set left the caller with no interval at all.
  res <- bb_optimize(0.52, "minimize", step = c(10, 1))

  expect_null(res$cs)
  expect_equal(res$confidence_set_size_percentage, 0)
  expect_equal(
    res$est_outcome_ci,
    logit_ci_at(res$model, bb_pred_row(res$rec_int)),
    tolerance = 1e-6
  )
})

# A deterministic continuous data set with both a center and a period column.
# Fixed center effects plus fixed time effects on a continuous outcome is what
# routes the variance through the hand-rolled TWO-WAY clustered estimator
# (Cameron-Gelbach-Miller), which is not positive semi-definite here, so some
# grid rows get a NaN standard error and are dropped. That drop is what the
# index mispairing needed. mtcars is used as the source of the two components
# so the fixture carries no RNG dependence.
two_way_data <- function(n_centers = 4, n_periods = 3) {
  d <- mtcars[, c("mpg", "gear", "qsec")]
  # centers and periods assigned round-robin. Neither column's order of FIRST
  # APPEARANCE is its level order, which is what the row-permutation test below
  # relies on and asserts.
  d$center <- factor(paste0("c", (seq_len(nrow(d)) %% n_centers) + 1))
  d$period <- factor((seq_len(nrow(d)) %% n_periods) + 1)
  d
}

two_way_optimize <- function(data,
                             outcome_goal = 30,
                             period = 2,
                             center = "c2") {
  suppressWarnings(suppressMessages(lago_optimization(
    data = data,
    outcome_name = "mpg",
    outcome_type = "continuous",
    glm_family = "gaussian",
    link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14),
    intervention_upper_bounds = c(5, 22),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = outcome_goal,
    outcome_goal_intention = "maximize",
    include_center_effects = TRUE,
    center_effects_optimization_values = center,
    include_time_effects = TRUE,
    time_effect_optimization_value = period,
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(1, 2),
    # a coarse confidence-set grid (3 x 5 = 15 points): the two-way estimator is
    # rebuilt per call, and the size of the set is not what this fixture is for
    confidence_set_grid_step_size = c(1, 2),
    include_confidence_set = TRUE,
    quiet = TRUE
  )))
}

# --- independent reimplementation of the continuous/identity variance ---------
# Built here from scratch so the reported bounds are checked against a
# calculation that shares no code with the one under test. glm()'s dummy coding
# (drop the FIRST LEVEL, number the rest in LEVEL order) is what is replicated,
# which is the coding the fix adopted.
ref_design_matrix <- function(data) {
  x <- matrix(1, nrow = nrow(data), ncol = 1)
  colnames(x) <- "(Intercept)"
  for (col in names(data)) {
    v <- data[[col]]
    if (is.factor(v) || is.character(v)) {
      for (lev in levels(droplevels(factor(v)))[-1]) {
        x <- cbind(x, as.numeric(v == lev))
        colnames(x)[ncol(x)] <- paste0(col, lev)
      }
    } else {
      x <- cbind(x, v)
      colnames(x)[ncol(x)] <- col
    }
  }
  x
}

# cluster-robust sandwich for a linear model: (X'X)^-1 sum_c S_c S_c' (X'X)^-1
ref_cluster_vcov <- function(x, cluster, outcome, fitted) {
  resid <- outcome - fitted
  bread <- solve(t(x) %*% x)
  meat <- matrix(0, ncol(x), ncol(x))
  for (cl in unique(cluster)) {
    idx <- which(cluster == cl)
    score <- t(x[idx, , drop = FALSE]) %*% resid[idx]
    meat <- meat + score %*% t(score)
  }
  bread %*% meat %*% bread
}

# Cameron-Gelbach-Miller two-way estimator: V1 + V2 - V12
ref_two_way_vcov <- function(data, model, predictors) {
  x <- ref_design_matrix(data[, predictors, drop = FALSE])
  v <- ref_cluster_vcov(x, data$center, data$mpg, model$fitted.values) +
    ref_cluster_vcov(x, data$period, data$mpg, model$fitted.values) -
    ref_cluster_vcov(
      x, paste(data$center, data$period, sep = "_"), data$mpg,
      model$fitted.values
    )
  dimnames(v) <- list(colnames(x), colnames(x))
  v
}

# the 95% interval of the continuous/identity branch at ONE intervention, at
# center "c2" and the requested period
ref_two_way_ci_at <- function(data, model, gear, qsec, period = 2) {
  b <- coef(model)
  centers <- levels(data$center)
  periods <- levels(data$period)
  row <- c(
    "(Intercept)" = 1,
    setNames(as.numeric(centers[-1] == "c2"), paste0("center", centers[-1])),
    setNames(as.numeric(periods[-1] == period), paste0("period", periods[-1])),
    gear = gear, qsec = qsec
  )[names(b)]
  v <- ref_two_way_vcov(data, model, c("center", "period", "gear", "qsec"))
  point <- sum(row * b)
  se <- suppressWarnings(
    sqrt(as.numeric(t(row) %*% v[names(b), names(b)] %*% row))
  )
  round(c(lower = point - qnorm(0.975) * se,
          upper = point + qnorm(0.975) * se), 3)
}

test_that("two-way clustering pairs every row's interval with its OWN coordinates", {
  # The interval matrix was indexed by post-complete.cases() row numbers while
  # the grid was indexed by original ones, so every row after a dropped one
  # carried an earlier row's bounds. Reaching it needs rows to actually be
  # dropped, which the two-way estimator supplies: it is not positive
  # semi-definite here, so some rows get a NaN standard error.
  d <- two_way_data()
  res <- two_way_optimize(d)

  # The whole grid's intervals, computed independently at each point's own
  # coordinates. The set is then the points whose interval covers the goal, so
  # the coordinates, the bounds AND the membership are all derived from one
  # calculation that never sees a row number.
  grid <- expand.grid(gear = seq(3, 5, by = 1), qsec = seq(14, 22, by = 2))
  reference <- cbind(grid, t(apply(grid, 1, function(r) {
    ref_two_way_ci_at(d, res$model, r[["gear"]], r[["qsec"]])
  })))
  # the fixture must really drop rows, or there is nothing to mispair: the
  # two-way estimator is not positive semi-definite here, so some rows get a
  # NaN standard error and complete.cases() removes them.
  expect_true(any(is.nan(reference$lower)))
  expect_true(any(!is.nan(reference$lower)))

  qualifies <- !is.nan(reference$lower) &
    reference$lower <= 30 & 30 <= reference$upper
  expected <- reference[qualifies, ]
  # the independently derived set is non-empty and comes from BOTH sides of a
  # dropped row, which is exactly the case a shifted index gets wrong
  expect_gt(nrow(expected), 0)
  expect_true(any(which(qualifies) > min(which(is.nan(reference$lower)))))

  # the reported set is that set: same membership, in the same grid order,
  # each row carrying the bounds computed at its own coordinates
  expect_false(is.null(res$cs))
  expect_equal(nrow(res$cs), nrow(expected))
  expect_equal(res$cs$gear, expected$gear)
  expect_equal(res$cs$qsec, expected$qsec)
  expect_equal(res$cs$CI_lower_bound, unname(expected$lower), tolerance = 1e-6)
  expect_equal(res$cs$CI_upper_bound, unname(expected$upper), tolerance = 1e-6)
  # and the size counts those rows over the 15 grid points
  expect_equal(
    res$confidence_set_size_percentage,
    nrow(expected) / n_grid_points(c(3, 14), c(5, 22), c(1, 2))
  )
  # the recommendation's interval is likewise the interval at rec_int
  expect_equal(
    res$est_outcome_ci,
    ref_two_way_ci_at(d, res$model, res$rec_int[1], res$rec_int[2]),
    tolerance = 1e-6
  )
})

test_that("a row permutation of data leaves the confidence set identical", {
  # The design matrix the variance is built from enumerated a factor's levels
  # by FIRST APPEARANCE while glm() numbers them in LEVEL order, so its dummies
  # could stand for different columns than the model's coefficients. Nothing
  # about the fit changes under a pure row permutation, so the confidence set
  # must not change either.
  d <- two_way_data()
  perm <- c(8:nrow(d), 1:7)
  dp <- d[perm, ]

  # the permutation has to be one that MOVES a factor's first appearance, and
  # first appearance has to differ from level order in the first place -
  # otherwise enumerating by either gives the same dummies and the fixture
  # proves nothing.
  for (col in c("center", "period")) {
    expect_false(identical(
      as.character(unique(d[[col]])), levels(d[[col]])
    ))
    expect_false(identical(
      as.character(unique(d[[col]])), as.character(unique(dp[[col]]))
    ))
  }
  # the permutation is a permutation: same rows, same factor levels
  expect_equal(nrow(dp), nrow(d))
  expect_equal(sort(dp$mpg), sort(d$mpg))
  expect_equal(levels(dp$center), levels(d$center))
  expect_equal(levels(dp$period), levels(d$period))

  res <- two_way_optimize(d)
  res_perm <- two_way_optimize(dp)

  expect_identical(res_perm$cs, res$cs)
  expect_identical(
    res_perm$confidence_set_size_percentage, res$confidence_set_size_percentage
  )
  expect_identical(res_perm$est_outcome_ci, res$est_outcome_ci)
})

test_that("predictors_data is matched by column NAME, and an extra column is an error", {
  # the variance matrix is built from predictors_data, not from the fitted
  # model, so its rows and columns are in whatever order the caller supplied.
  # They are paired with the assembled prediction columns by name; pairing by
  # position would multiply each column by another column's variance, and an
  # unmatched extra column would widen the matrix and shift every variance,
  # both without a symptom.
  # Fixed center effects only, so the confidence set is reachable through the
  # exported get_confidence_set() with the same arguments regardless of how the
  # fixed time effects are specified. The variance is still the hand-rolled
  # clustered one built from predictors_data, which is the code under test.
  d <- two_way_data()
  d$period <- NULL
  opt <- suppressWarnings(suppressMessages(lago_optimization(
    data = d,
    outcome_name = "mpg",
    outcome_type = "continuous",
    glm_family = "gaussian",
    link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14),
    intervention_upper_bounds = c(5, 22),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 30,
    outcome_goal_intention = "maximize",
    include_center_effects = TRUE,
    center_effects_optimization_values = "c2",
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(1, 2),
    confidence_set_grid_step_size = c(1, 2),
    include_confidence_set = TRUE,
    quiet = TRUE
  )))
  predictors <- c("center", "gear", "qsec")

  call_cs <- function(predictors_data) {
    suppressWarnings(get_confidence_set(
      predictors_data = predictors_data,
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = as.numeric(levels(d$center) == "c2"),
      intervention_components = c("gear", "qsec"),
      outcome_data = d$mpg,
      fitted_model = opt$model,
      link = "identity",
      outcome_goal = 30,
      outcome_type = "continuous",
      intervention_lower_bounds = c(3, 14),
      intervention_upper_bounds = c(5, 22),
      confidence_set_grid_step_size = c(1, 2),
      cluster_id = list(d$center),
      cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      rec_int = opt$rec_int
    ))
  }

  in_order <- call_cs(d[, predictors, drop = FALSE])
  reversed <- call_cs(d[, rev(predictors), drop = FALSE])
  # the reversed order really is a different order (guards the fixture)
  expect_false(identical(rev(predictors), predictors))
  expect_identical(reversed, in_order)
  # and it is the same answer lago_optimization() reports, so the column order
  # confidence_set_processor() happens to pass is not special
  expect_equal(in_order$cs, opt$cs)

  # an extra column matches no predictor, so it is refused rather than silently
  # dropped after having widened the variance matrix
  expect_error(
    call_cs(cbind(d[, predictors, drop = FALSE], spare = d$mpg)),
    "do not match the predictors"
  )
})

test_that("the estimated outcome lies inside its interval at every requested period", {
  # The interval was computed at the hardcoded LAST period while the point
  # estimate used the requested one, so the two described different periods.
  # The reference period is included: it has no dummy of its own and is all-zero
  # columns, which is a legitimate answer and not a failure to match.
  d <- two_way_data(n_centers = 3, n_periods = 4)
  periods <- levels(d$period)
  expect_gt(length(periods), 2) # several periods, or there is nothing to mix up

  results <- lapply(periods, function(p) {
    two_way_optimize(
      d, outcome_goal = 24, period = as.numeric(p), center = "c1"
    )
  })

  for (i in seq_along(periods)) {
    res <- results[[i]]
    ci <- res$est_outcome_ci
    expect_false(is.null(ci))
    expect_true(
      ci[["lower"]] <= res$est_outcome_goal &&
        res$est_outcome_goal <= ci[["upper"]]
    )
  }

  # the interval actually MOVES with the requested period: identical intervals
  # everywhere would satisfy the containment above while still ignoring the
  # request, which is what computing at a fixed period looks like.
  intervals <- unique(lapply(results, function(r) unname(r$est_outcome_ci)))
  expect_equal(length(intervals), length(periods))
})


test_that("the exported get_confidence_set() refuses a bad center weight", {
  # get_confidence_set() is EXPORTED and does not go through validate_inputs(),
  # so the weight guards lago_optimization() has do not cover a direct caller.
  # The weights average the per-center outcomes, so a negative one puts the
  # interval outside the range of the intervals it averages: c(8.5, -8, 0.5)
  # sums to exactly 1, passed every check this function made, and reported a
  # CI_upper_bound of 1.014 for a BINARY outcome -- a "probability" above 1,
  # silently. This function already validates its own link and its own
  # coefficient-to-predictor match for exactly this reason (it cannot trust its
  # caller), and the weights are the same kind of argument.
  d <- as.data.frame(BB_data)
  d$center <- factor(rep_len(paste0("c", 1:3), nrow(d)))
  components <- c("coaching_updt", "launch_duration")
  model <- glm(
    pp3_oxytocin_mother ~ center + coaching_updt + launch_duration,
    data = d, family = binomial(link = "logit")
  )

  call_cs <- function(w, include_center_effects = TRUE) {
    suppressWarnings(get_confidence_set(
      predictors_data = d[, c("center", components), drop = FALSE],
      include_center_effects = include_center_effects,
      center_weights_for_outcome_goal = w,
      intervention_components = components,
      outcome_data = d$pp3_oxytocin_mother,
      fitted_model = model,
      link = "logit",
      outcome_goal = 0.85,
      outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(8, 1),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      rec_int = c(20, 3)
    ))
  }

  # a negative weight is refused, in the words validate_inputs() uses, so a
  # caller who moves between the two entry points is told the same thing
  expect_error(call_cs(c(8.5, -8, 0.5)), "must be non-negative")
  expect_error(call_cs(c(0.75, -0.5, 0.75)), "must be non-negative")
  expect_error(call_cs(c(20.5, -20, 0.5)), "must be non-negative")
  # summing to 1 is what let it through, so the fixture asserts that it does
  expect_identical(sum(c(8.5, -8, 0.5)), 1)
  expect_identical(sum(c(0.75, -0.5, 0.75)), 1)

  # the boundary, so the tolerance cannot be widened without a test failing.
  # It admits floating-point noise from a residual weight and nothing larger.
  expect_error(call_cs(c(0.5, -1e-9, 0.5)), "must be non-negative")
  expect_error(call_cs(c(0.5, -1e-12, 0.5)), "must be non-negative")

  # non-finite weights are named rather than reaching the interval, where they
  # made every bound NA and rec_int_ci NULL with nothing said about why
  for (bad in list(
    c(0.5, NA_real_, 0.5), c(0.5, NaN, 0.5),
    c(0.5, Inf, 0.5), c(0.5, 0.5, -Inf)
  )) {
    expect_error(call_cs(bad), "must all be finite")
  }

  # a weight of exactly 0 is still ALLOWED, and so is a residual weight a hair
  # below zero: the guard must not narrow what the function accepts. The
  # single-named-center path the package itself builds is exactly a vector of
  # one 1 and the rest 0.
  residual <- -.Machine$double.eps / 2
  expect_lt(residual, 0)
  for (good in list(
    c(1, 1, 1) / 3, c(0.5, 0, 0.5), c(1, 0, 0), c(0, 0, 1),
    c(0.5, residual, 0.5)
  )) {
    res <- call_cs(good)
    expect_false(is.null(res$rec_int_ci))
    expect_true(all(res$rec_int_ci >= 0 & res$rec_int_ci <= 1))
  }

  # and the numbers a compliant vector produces are unchanged, so the guard
  # only removed the refused cases
  uniform <- call_cs(c(1, 1, 1) / 3)
  expect_true(all(uniform$cs$CI_upper_bound <= 1))
  expect_true(all(uniform$cs$CI_lower_bound >= 0))

  # the weights are only USED when the fixed center effects are included, so
  # they are only checked then: a caller who is not asking for center effects
  # passes the default 1 and must not be refused for a vector nobody reads.
  expect_error(
    suppressWarnings(get_confidence_set(
      predictors_data = d[, components, drop = FALSE],
      center_weights_for_outcome_goal = c(-1, 2),
      intervention_components = components,
      outcome_data = d$pp3_oxytocin_mother,
      fitted_model = glm(
        pp3_oxytocin_mother ~ coaching_updt + launch_duration,
        data = d, family = binomial(link = "logit")
      ),
      link = "logit",
      outcome_goal = 0.85,
      outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(8, 1),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      rec_int = c(20, 3)
    )),
    NA
  )

  # the same weights through the guarded primary path are refused there too, so
  # the two entry points agree rather than one being stricter
  primary <- function() {
    suppressWarnings(suppressMessages(lago_optimization(
      data = d,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = components,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      outcome_goal = 0.85,
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = c(8.5, -8, 0.5),
      include_confidence_set = FALSE,
      quiet = TRUE
    )))
  }
  expect_error(primary(), "must be non-negative")
  expect_identical(
    tryCatch(primary(), error = conditionMessage),
    tryCatch(call_cs(c(8.5, -8, 0.5)), error = conditionMessage)
  )
})


test_that("a binary outcome's interval is built on the link it was fitted on", {
  # The binary branch applied expit() and the logit delta-method factor
  # p * (1 - p) UNCONDITIONALLY, keyed on outcome_type and ignoring link. A
  # binomial model fitted with link = "identity" -- which lago_optimization()
  # accepts -- therefore got its interval on the logit scale: 0.636 to 0.655
  # where the identity-scale interval is 0.558 to 0.642, an interval that does
  # not even contain the point estimate the same run reported. The scale an
  # interval belongs on is a property of the LINK, not of the outcome's type,
  # which is what get_outcome() has keyed on all along -- hence the point
  # estimate being right while the interval was not.
  #
  # The fixture converges cleanly (3 IRLS iterations, no glm warning), because a
  # binomial identity fit that does not converge would leave it open whether the
  # interval or the fit was the problem. It keeps the fitted probabilities well
  # inside (0, 1): a linear probability model on a grid, replicated ten rows per
  # cell so the design is balanced.
  cells <- expand.grid(x1 = seq(0, 9, length.out = 40), x2 = 1:5)
  p <- 0.30 + 0.02 * cells$x1 + 0.04 * cells$x2
  expect_true(all(p > 0.05 & p < 0.95))
  d <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    ones <- round(p[i] * 10)
    data.frame(
      x1 = cells$x1[i], x2 = cells$x2[i],
      y = c(rep(1L, ones), rep(0L, 10 - ones))
    )
  }))

  # the fixture's own precondition: the fit converges and glm() says nothing
  fit_warnings <- character(0)
  model <- withCallingHandlers(
    glm(y ~ x1 + x2, data = d, family = binomial(link = "identity")),
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(model$converged)
  expect_length(fit_warnings, 0)
  expect_false(anyNA(coef(model)))

  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = d,
    outcome_name = "y",
    outcome_type = "binary",
    glm_family = "binomial",
    link = "identity",
    intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(0, 1),
    intervention_upper_bounds = c(9, 5),
    cost_list_of_vectors = list(c(0, 1), c(0, 2)),
    outcome_goal = 0.6,
    confidence_set_grid_step_size = c(1, 1),
    quiet = TRUE
  )))

  # THE assertion. The interval is recomputed by hand from glm() itself -- the
  # linear predictor and its standard error, with no link transformation,
  # because on the identity link the outcome IS the linear predictor and the
  # delta-method factor is the derivative of the identity map, i.e. 1. This
  # oracle does not go through the package.
  critical_value <- qnorm(0.975)
  identity_interval <- function(x) {
    row <- c(1, x)
    point <- as.numeric(row %*% coef(model))
    std_error <- sqrt(as.numeric(t(row) %*% vcov(model) %*% row))
    c(
      round(point - critical_value * std_error, 3),
      round(point + critical_value * std_error, 3)
    )
  }
  expect_identical(
    unname(res$est_outcome_ci),
    identity_interval(res$rec_int)
  )

  # and what the defect produced instead, so the test says which scale is wrong
  # rather than only that the number changed: expit() of the point with the
  # logit delta factor accounts for the OLD interval exactly, and it is a
  # different interval from the one now reported.
  row <- c(1, res$rec_int)
  point <- as.numeric(row %*% coef(model))
  std_error <- sqrt(as.numeric(t(row) %*% vcov(model) %*% row))
  logit_point <- rje::expit(point)
  logit_std_error <- std_error * logit_point * (1 - logit_point)
  logit_interval <- c(
    round(logit_point - critical_value * logit_std_error, 3),
    round(logit_point + critical_value * logit_std_error, 3)
  )
  expect_false(identical(logit_interval, identity_interval(res$rec_int)))
  expect_false(identical(unname(res$est_outcome_ci), logit_interval))

  # the interval contains the point estimate, which the logit-scale one did not:
  # the estimated outcome was 0.6 and the reported interval 0.636 to 0.655
  expect_true(
    res$est_outcome_ci[["lower"]] <= res$est_outcome_goal &&
      res$est_outcome_goal <= res$est_outcome_ci[["upper"]]
  )

  # every confidence-set row, not only rec_int, and each against its own
  # coordinates. A set whose intervals are on the wrong scale can still be
  # non-empty, so containment of the goal alone would not catch this.
  expect_gt(nrow(res$cs), 0)
  for (i in seq_len(nrow(res$cs))) {
    expect_identical(
      c(res$cs$CI_lower_bound[i], res$cs$CI_upper_bound[i]),
      identity_interval(c(res$cs$x1[i], res$cs$x2[i]))
    )
  }
  # and they cover the goal, as membership claims, on the corrected scale
  expect_true(all(
    res$cs$CI_lower_bound <= 0.6 & res$cs$CI_upper_bound >= 0.6
  ))

  # the LOGIT case is untouched, which is what confines the change to the link
  # that was wrong. Same oracle, the delta-method interval glm()'s own binary
  # machinery reports, on the same package's default binary configuration.
  logit_res <- suppressWarnings(suppressMessages(lago_optimization(
    data = as.data.frame(BB_data),
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = 0.85,
    confidence_set_grid_step_size = c(8, 1),
    quiet = TRUE
  )))
  logit_model <- glm(
    pp3_oxytocin_mother ~ coaching_updt + launch_duration,
    data = as.data.frame(BB_data), family = binomial(link = "logit")
  )
  logit_row <- c(1, logit_res$rec_int)
  logit_eta <- as.numeric(logit_row %*% coef(logit_model))
  logit_p <- rje::expit(logit_eta)
  logit_se <- sqrt(
    as.numeric(t(logit_row) %*% vcov(logit_model) %*% logit_row)
  ) * logit_p * (1 - logit_p)
  expect_identical(
    unname(logit_res$est_outcome_ci),
    c(
      round(logit_p - critical_value * logit_se, 3),
      round(logit_p + critical_value * logit_se, 3)
    )
  )
})


test_that("the exported get_confidence_set() refuses a non-unit weight sum", {
  # The companion to the negative-weight refusal above, reaching the same wrong
  # number by the other door. Every weight here is non-negative and finite, so
  # refuse_invalid_center_weights() does not apply, but the vector is still not
  # a set of weights TAKEN TOGETHER: the reported outcome is
  # sum(weight_i * outcome_i), a weighted mean only when the weights sum to 1,
  # so a set summing to 12 scales the intervention contribution of every
  # reported outcome by 12. On a proportion outcome that reported a confidence
  # interval of 1.406 to 2.147.
  #
  # get_confidence_set() is EXPORTED and does not go through validate_inputs(),
  # which is where the sum check lived, so a direct caller had none at all.
  # It is REFUSED here rather than renormalised, unlike validate_inputs():
  # this function is handed the weights an optimization has already run with and
  # reports the interval AT them, so rescaling them would report an interval for
  # a different weighting than the point estimate printed beside it.
  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  components <- c("AccessMedicines", "AccessBPMachines")
  model <- glm(
    Proportions ~ center + AccessMedicines + AccessBPMachines,
    data = pulesa, family = gaussian(link = "identity")
  )
  n_centers <- length(levels(pulesa$Clinic))
  expect_equal(n_centers, 16)

  call_cs <- function(w) {
    suppressWarnings(get_confidence_set(
      predictors_data = pulesa[, c("center", components), drop = FALSE],
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = w,
      intervention_components = components,
      outcome_data = pulesa$Proportions,
      fitted_model = model,
      link = "identity",
      outcome_goal = 0.5,
      outcome_type = "continuous",
      intervention_lower_bounds = c(1, 0.5),
      intervention_upper_bounds = c(5, 1),
      confidence_set_grid_step_size = c(1, 0.25),
      cluster_id = list(pulesa$center),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      rec_int = c(3, 0.75)
    ))
  }

  # the fixture's own precondition: these weights pass every check the function
  # made before, so the sum is the only thing that refuses them
  over <- rep(12 / n_centers, n_centers)
  expect_true(all(over >= 0 & is.finite(over)))
  expect_equal(sum(over), 12)

  # a sum far from 1 is refused, in the words validate_inputs() uses
  expect_error(call_cs(over), "sum up to 1")
  expect_error(call_cs(rep(200 / n_centers, n_centers)), "sum up to 1")
  expect_error(call_cs(rep(0.5 / n_centers, n_centers)), "sum up to 1")
  # all-zero weights are a sum of 0, so they are refused here too
  expect_error(call_cs(rep(0, n_centers)), "sum up to 1")

  # compliant weights are UNCHANGED, so the guard only removed the refused
  # cases. This is the number the scaled vectors were a multiple of.
  unit <- call_cs(rep(1 / n_centers, n_centers))
  expect_false(is.null(unit$rec_int_ci))
  expect_identical(
    unname(unit$rec_int_ci),
    c(0.557, 0.628)
  )

  # the tolerance is validate_inputs()' own 0.001 and is not narrowed: a
  # residual sum a hair off 1, which is what renormalised weights can be, is
  # still accepted, and so is rounded input the documentation invites
  expect_error(call_cs(rep(1 / n_centers, n_centers)), NA)
  nudged <- rep(1 / n_centers, n_centers)
  nudged[1] <- nudged[1] + 0.0005
  expect_lt(abs(sum(nudged) - 1), 0.001)
  expect_error(call_cs(nudged), NA)

  # and the two entry points refuse the same vector in the SAME words, which is
  # the property that makes refusing here consistent rather than divergent
  primary <- function() {
    suppressWarnings(suppressMessages(lago_optimization(
      data = pulesa,
      outcome_name = "Proportions",
      outcome_type = "continuous",
      intervention_components = components,
      intervention_lower_bounds = c(1, 0.5),
      intervention_upper_bounds = c(5, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      outcome_goal = 0.5,
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = over,
      include_confidence_set = FALSE,
      quiet = TRUE
    )))
  }
  expect_error(primary(), "sum up to 1")
  expect_identical(
    tryCatch(primary(), error = conditionMessage),
    tryCatch(call_cs(over), error = conditionMessage)
  )

  # the weights are only USED when the fixed center effects are included, so a
  # caller who is not asking for them passes the scalar default 1 and must not
  # be refused for a vector nobody reads. That default sums to 1 anyway, so the
  # gate is asserted with a vector that does NOT.
  expect_error(
    suppressWarnings(get_confidence_set(
      predictors_data = pulesa[, components, drop = FALSE],
      include_center_effects = FALSE,
      center_weights_for_outcome_goal = c(6, 6),
      intervention_components = components,
      outcome_data = pulesa$Proportions,
      fitted_model = glm(
        Proportions ~ AccessMedicines + AccessBPMachines,
        data = pulesa, family = gaussian(link = "identity")
      ),
      link = "identity",
      outcome_goal = 0.5,
      outcome_type = "continuous",
      intervention_lower_bounds = c(1, 0.5),
      intervention_upper_bounds = c(5, 1),
      confidence_set_grid_step_size = c(1, 0.25),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      rec_int = c(3, 0.75)
    )),
    NA
  )
})


test_that("a binary outcome's reported interval is confined to [0, 1]", {
  # A binary outcome is a probability, so a reported bound of -0.106 or 1.049 is
  # not one. The binary branch builds pred +- z*se on the probability scale,
  # which is symmetric there and so free to leave [0, 1] on either side, and it
  # did on BOTH links: this fixture reported CI_lower_bound -0.106 and
  # CI_upper_bound 1.049 in the same returned set.
  #
  # The fix is a CLAMP, so the assertions below are written against the clamped
  # DELTA-METHOD interval and not against a logit-scale one: the interval is
  # still the delta-method interval this package has always reported, and
  # clamping only bounds what is reported of it.
  set.seed(11)
  d <- data.frame(a = rep(1:3, each = 6), b = rep_len(c(1, 2), 18))
  d$y <- rbinom(18, 1, 0.5)
  components <- c("a", "b")

  call_cs <- function(link, goal = 0.5) {
    model <- glm(
      y ~ a + b, data = d, family = binomial(link = link)
    )
    list(model = model, res = suppressWarnings(get_confidence_set(
      predictors_data = d[, components, drop = FALSE],
      intervention_components = components,
      outcome_data = d$y,
      fitted_model = model,
      link = link,
      outcome_goal = goal,
      outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(3, 2),
      confidence_set_grid_step_size = c(1, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      rec_int = c(2, 1.5)
    )))
  }

  critical_value <- qnorm(0.975)
  # the hand oracle: the delta-method interval from glm() directly, then
  # clamped. It does not go through the package.
  hand <- function(model, x, link) {
    row <- c(1, x)
    eta <- as.numeric(row %*% coef(model))
    se_eta <- sqrt(as.numeric(t(row) %*% vcov(model) %*% row))
    if (link == "identity") {
      point <- eta
      se_point <- se_eta
    } else {
      point <- rje::expit(eta)
      se_point <- se_eta * point * (1 - point)
    }
    bounds <- c(point - critical_value * se_point,
      point + critical_value * se_point)
    # confined on the logit link only, matching the code: there the point
    # estimate is expit(eta) and inside [0, 1], so confining the interval brings
    # it into agreement with the estimate. On the identity link the estimate is
    # the linear predictor and is itself unbounded, so confining the interval
    # around it would report an interval excluding its own estimate. An oracle
    # that clamped on both links would agree with any implementation that did.
    if (link == "logit") {
      bounds <- pmin(pmax(bounds, 0), 1)
    }
    round(bounds, 3)
  }

  for (link in c("logit", "identity")) {
    fit <- call_cs(link)
    model <- fit$model
    res <- fit$res

    # THE assertion, on the logit link: no reported bound is outside [0, 1].
    # On the identity link the opposite is required, since the estimate itself
    # is not confined there and an interval that excluded its own estimate would
    # be worse than one leaving the range. Both are asserted, so neither arm
    # passes by agreeing with whatever the code happens to do.
    expect_gt(nrow(res$cs), 0)
    if (link == "logit") {
      expect_true(all(res$cs$CI_lower_bound >= 0))
      expect_true(all(res$cs$CI_upper_bound <= 1))
      expect_true(all(res$rec_int_ci >= 0 & res$rec_int_ci <= 1))
    } else {
      # every reported interval contains the estimate it belongs to, which is
      # the property confining them on this link would break.
      #
      # On THIS fixture no estimate leaves [0, 1] -- the largest is 0.594 -- so
      # these hold whether or not the interval is confined, and are not what
      # pins the choice. The hand oracle below is: it follows the link, so a
      # confined bound disagrees with it. These are kept because they state the
      # property the choice exists to protect, and they bite on a fixture that
      # extrapolates far enough for the estimate itself to leave the range.
      for (i in seq_len(nrow(res$cs))) {
        point <- as.numeric(
          c(1, res$cs$a[i], res$cs$b[i]) %*% coef(model)
        )
        expect_gte(round(point, 3), res$cs$CI_lower_bound[i])
        expect_lte(round(point, 3), res$cs$CI_upper_bound[i])
      }
    }

    # the fixture's own precondition: the UNCLAMPED delta-method interval really
    # does leave [0, 1] here, on both sides, so the assertion above has
    # something to bite on rather than passing vacuously
    unclamped <- function(x) {
      row <- c(1, x)
      eta <- as.numeric(row %*% coef(model))
      se_eta <- sqrt(as.numeric(t(row) %*% vcov(model) %*% row))
      if (link == "identity") {
        point <- eta
        se_point <- se_eta
      } else {
        point <- rje::expit(eta)
        se_point <- se_eta * point * (1 - point)
      }
      round(c(point - critical_value * se_point,
        point + critical_value * se_point), 3)
    }
    raw <- vapply(
      seq_len(nrow(res$cs)),
      function(i) unclamped(c(res$cs$a[i], res$cs$b[i])),
      numeric(2)
    )
    expect_true(any(raw[1, ] < 0))
    expect_true(any(raw[2, ] > 1))

    # every row agrees with the hand oracle, at its OWN coordinates, so the
    # bounds are the clamped delta-method interval and not some other interval
    for (i in seq_len(nrow(res$cs))) {
      expect_identical(
        unname(c(res$cs$CI_lower_bound[i], res$cs$CI_upper_bound[i])),
        hand(model, c(res$cs$a[i], res$cs$b[i]), link)
      )
    }
    expect_identical(
      unname(res$rec_int_ci), hand(model, c(2, 1.5), link)
    )

    # rec_int_ci at a recommendation whose UNCLAMPED interval leaves the range.
    # The recommendation above is inside it, so that assertion cannot observe
    # the clamp on this field at all, and rec_int_ci is row 1 of the same matrix
    # and is what the printed interval for the estimated outcome comes from: it
    # has to be confined too, and only a stored snapshot was holding that.
    out_of_range <- suppressWarnings(get_confidence_set(
      predictors_data = d[, c("a", "b"), drop = FALSE],
      intervention_components = c("a", "b"),
      outcome_data = d$y, fitted_model = model, link = link,
      outcome_goal = 0.5, outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(3, 2),
      confidence_set_grid_step_size = c(1, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      rec_int = c(1, 1)
    ))
    expect_true(unclamped(c(1, 1))[1] < 0)
    expect_identical(unname(out_of_range$rec_int_ci), hand(model, c(1, 1), link))
    if (link == "logit") {
      expect_true(all(
        out_of_range$rec_int_ci >= 0 & out_of_range$rec_int_ci <= 1
      ))
    } else {
      # not confined here, so it is the interval as computed
      expect_identical(
        unname(out_of_range$rec_int_ci), unclamped(c(1, 1))
      )
    }

    # the interval is NOT the logit-scale one transformed back, which is the
    # other way to bound it and would have moved every binary number. Asserted
    # so the test says which interval is reported, not merely that it is in
    # range.
    logit_scale <- function(x) {
      row <- c(1, x)
      eta <- as.numeric(row %*% coef(model))
      se_eta <- sqrt(as.numeric(t(row) %*% vcov(model) %*% row))
      round(rje::expit(c(eta - critical_value * se_eta,
        eta + critical_value * se_eta)), 3)
    }
    expect_false(identical(
      unname(res$rec_int_ci), logit_scale(c(2, 1.5))
    ))

    # a clamped bound sits at exactly the boundary, which is what distinguishes
    # truncation from an interval that happens to end there. Exactly 0 and
    # exactly 1, not nearly: findInterval() treats the interval as [lower,
    # upper), so a bound confined to 1e-6 rather than 0 would report a
    # probability that is not one AND drop every row from a goal of exactly 0.
    if (link == "logit") {
      for (i in seq_len(nrow(res$cs))) {
        bounds <- unclamped(c(res$cs$a[i], res$cs$b[i]))
        if (bounds[1] < 0) {
          expect_identical(res$cs$CI_lower_bound[i], 0)
        }
        if (bounds[2] > 1) {
          expect_identical(res$cs$CI_upper_bound[i], 1)
        }
      }
      expect_true(any(
        res$cs$CI_lower_bound == 0 | res$cs$CI_upper_bound == 1
      ))
    } else {
      # on the identity link nothing is confined, so a bound that left the range
      # is reported as computed. Asserted so this arm cannot pass under a design
      # that clamps here.
      out <- vapply(
        seq_len(nrow(res$cs)),
        function(i) {
          b <- unclamped(c(res$cs$a[i], res$cs$b[i]))
          b[1] < 0 || b[2] > 1
        },
        logical(1)
      )
      if (any(out)) {
        i <- which(out)[1]
        expect_identical(
          unname(c(res$cs$CI_lower_bound[i], res$cs$CI_upper_bound[i])),
          unclamped(c(res$cs$a[i], res$cs$b[i]))
        )
      }
    }

    # MEMBERSHIP is unchanged. The goal is a probability, so it lies in the
    # range the bounds are clamped to, and clamping a bound to the range it is
    # compared against cannot move it across the goal. Asserted directly:
    # every returned row still brackets the goal, and every row NOT returned
    # still fails to, judged on the UNCLAMPED bounds.
    expect_true(all(
      res$cs$CI_lower_bound <= 0.5 & res$cs$CI_upper_bound >= 0.5
    ))
    grid <- expand.grid(a = 1:3, b = 1:2)
    covers_unclamped <- vapply(seq_len(nrow(grid)), function(i) {
      bounds <- unclamped(c(grid$a[i], grid$b[i]))
      bounds[1] <= 0.5 && 0.5 <= bounds[2]
    }, logical(1))
    returned <- paste(res$cs$a, res$cs$b)
    expect_setequal(
      returned,
      paste(grid$a, grid$b)[covers_unclamped]
    )

    # AT A GOAL OF EXACTLY 1, which is where confining a bound and deciding
    # membership from it come apart. findInterval() treats the interval as
    # [lower, upper), so a bound brought down from 1.049 to 1 puts the goal at
    # the closed end and the row stops qualifying: an intervention whose
    # computed interval covers the goal would drop out because of how its bound
    # is reported. Membership therefore reads the interval as computed, and this
    # is what fails if the two are ever recombined. The goal 0.5 case above
    # cannot see it, since no bound is confined near an interior goal.
    at_one <- call_cs(link, goal = 1)$res
    covers_one <- vapply(seq_len(nrow(grid)), function(i) {
      bounds <- unclamped(c(grid$a[i], grid$b[i]))
      bounds[1] <= 1 && 1 <= bounds[2]
    }, logical(1))
    if (any(covers_one)) {
      expect_false(is.null(at_one$cs))
      expect_setequal(
        paste(at_one$cs$a, at_one$cs$b),
        paste(grid$a, grid$b)[covers_one]
      )
    }
  }
})


test_that("a continuous outcome's interval is NOT clamped to [0, 1]", {
  # The counterpart to the test above, pinning the deliberate asymmetry. The
  # binary branch can confine its bounds because "binary" fixes the range at
  # [0, 1]; a continuous outcome is a mean on whatever scale the caller's data
  # is on, and nothing here knows what that is. "continuous" with a proportion
  # is supported, and so is a count or a duration, and the outcome goal is only
  # required to be numeric -- so an out-of-[0, 1] bound is not knowably wrong
  # for a continuous outcome and clamping it would corrupt every outcome that is
  # not a proportion.
  #
  # This asserts the range is left alone on a fixture whose outcome is far
  # outside [0, 1], which a clamp would silently destroy.
  set.seed(5)
  d <- data.frame(x1 = rep(1:6, each = 4), x2 = rep_len(c(1, 2), 24))
  d$y <- 40 + 3 * d$x1 + 2 * d$x2 + rnorm(24, 0, 1.5)
  expect_true(all(d$y > 1))

  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = d,
    outcome_name = "y",
    outcome_type = "continuous",
    intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(6, 2),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = 55,
    confidence_set_grid_step_size = c(1, 1),
    quiet = TRUE
  )))

  # the bounds are on the outcome's own scale, well outside [0, 1], and are
  # reported as they are
  expect_gt(nrow(res$cs), 0)
  expect_true(all(res$cs$CI_lower_bound > 1))
  expect_true(all(res$cs$CI_upper_bound > 1))
  expect_true(all(res$est_outcome_ci > 1))
  # a clamp would have collapsed every one of them to exactly 1
  expect_false(any(res$cs$CI_upper_bound == 1))
})

# --- the LOGIT sandwich variance is now accumulated in a compiled kernel ------
# The per-observation loops that build the "bread" (J) and "meat" (V) matrices
# of the robust/clustered logit sandwich moved from R into src/sandwich_vcov.cpp
# (get_single_cluster_vcov and the non-clustered HC0 branch of get_vcov). Only
# the accumulation moved: solve() and bread %*% V %*% t(bread) are still R, so
# the inversion is byte-for-byte R's. These tests pin the result of that path
# against a sandwich formula written out from scratch here, so they check the
# kernel against an independent hand computation and not against itself. The
# logit sandwich is reached with outcome_type = "continuous" and link = "logit"
# (the "binary" branch takes vcov(fitted_model) directly and never enters it).

# design matrix of numeric predictors, built here so the reference shares no
# code with prepare_design_matrix() inside get_vcov()
ref_logit_design <- function(data, predictors) {
  x <- matrix(1, nrow = nrow(data), ncol = 1)
  colnames(x) <- "(Intercept)"
  for (col in predictors) {
    x <- cbind(x, data[[col]])
    colnames(x)[ncol(x)] <- col
  }
  x
}

# clustered logit sandwich: per cluster c, hessian H_c = sum_i ddb_i ddb_i' and
# score s_c = sum_i ddb_i (y_i - p_i) with ddb_i = p_i(1-p_i) x_i; then
# J = sum_c H_c / n_clusters, V = sum_c s_c s_c' / n_clusters, and the returned
# variance is J^-1 V J^-1 / n_clusters. Written from the formula, not the code.
ref_logit_cluster_vcov <- function(x, cluster, fitted, outcome) {
  np <- ncol(x)
  matrix_j <- matrix(0, np, np)
  matrix_v <- matrix(0, np, np)
  clusters <- unique(cluster)
  n_clusters <- length(clusters)
  for (cl in clusters) {
    idx <- which(cluster == cl)
    hess <- matrix(0, np, np)
    score <- matrix(0, np, 1)
    for (i in idx) {
      x_i <- as.matrix(x[i, ])
      p_i <- fitted[i]
      ddb <- (p_i * (1 - p_i)) * x_i
      hess <- hess + ddb %*% t(ddb)
      score <- score + ddb * (outcome[i] - p_i)
    }
    matrix_j <- matrix_j + hess / n_clusters
    matrix_v <- matrix_v + (score %*% t(score)) / n_clusters
  }
  bread <- solve(matrix_j)
  bread %*% matrix_v %*% t(bread) / n_clusters
}

# non-clustered HC0 logit sandwich: J = sum_i ddb_i ddb_i' / n and the meat is
# the per-observation form V = sum_i ddb_i (y_i - p_i)^2 ddb_i' / n (NOT a
# clustered score), with the returned variance J^-1 V J^-1 / n.
ref_logit_hc0_vcov <- function(x, fitted, outcome) {
  n <- nrow(x)
  np <- ncol(x)
  matrix_j <- matrix(0, np, np)
  matrix_v <- matrix(0, np, np)
  for (i in 1:n) {
    x_i <- as.matrix(x[i, ])
    p_i <- fitted[i]
    ddb <- (p_i * (1 - p_i)) * x_i
    matrix_j <- matrix_j + (ddb %*% t(ddb)) / n
    matrix_v <- matrix_v + (ddb %*% ((outcome[i] - p_i)^2) %*% t(ddb)) / n
  }
  bread <- solve(matrix_j)
  (bread %*% matrix_v %*% t(bread)) / n
}

# the interval the continuous + logit branch reports at one prediction row: the
# bounds are formed on the linear-predictor scale (eta +/- z*se), expit()ed, and
# rounded to 3 decimals. se uses the sandwich vcov built above.
ref_logit_ci_at <- function(vcov, coefs, row, alpha = 0.05) {
  row <- row[names(coefs)]
  v <- vcov[names(coefs), names(coefs)]
  eta <- sum(row * coefs)
  se <- sqrt(as.numeric(t(row) %*% v %*% row))
  z <- qnorm(1 - alpha / 2)
  round(rje::expit(c(lower = eta - z * se, upper = eta + z * se)), 3)
}

# the prediction row of the logit-sandwich fixture at one intervention, named
# with the coefficients it multiplies (intercept, the two components, and the
# center characteristic z held at its optimization value 1.75)
bb_pred_row_z <- function(x1, x2) {
  c("(Intercept)" = 1, x1 = x1, x2 = x2, z = 1.75)
}

# a binary fixture with enough rows, clusters and periods that the clustered
# path is genuinely exercised (see the vacuity guards in the tests below). Built
# from a fixed seed so the reference and the package see identical input.
logit_sandwich_fixture <- function() {
  set.seed(424242)
  n <- 300
  d <- data.frame(
    x1 = runif(n, 1, 40), x2 = runif(n, 1, 5), z = rnorm(n, 1.75, 0.5)
  )
  eta <- -2 + 0.03 * d$x1 + 0.1 * d$x2 + 0.2 * d$z
  d$y <- rbinom(n, 1, 1 / (1 + exp(-eta)))
  d$center <- factor(paste0("c", (seq_len(n) %% 12) + 1))
  d$period <- factor((seq_len(n) %% 5) + 1)
  d
}

# a direct get_confidence_set() call on that fixture at a fixed clustering mode
logit_sandwich_gcs <- function(d, model, cluster_id) {
  suppressWarnings(get_confidence_set(
    predictors_data = d[, c("x1", "x2", "z"), drop = FALSE],
    intervention_components = c("x1", "x2"),
    outcome_data = d$y,
    fitted_model = model,
    link = "logit",
    outcome_goal = 0.5,
    outcome_type = "continuous",
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    confidence_set_grid_step_size = c(10, 2),
    center_characteristics = "z",
    center_characteristics_optimization_values = 1.75,
    cluster_id = cluster_id,
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    rec_int = c(20, 3)
  ))
}

test_that("the non-clustered logit HC0 vcov matches an independent recomputation", {
  d <- logit_sandwich_fixture()
  model <- glm(y ~ x1 + x2 + z, data = d, family = binomial())
  res <- logit_sandwich_gcs(d, model, NULL)

  x <- ref_logit_design(d, c("x1", "x2", "z"))
  vcov_ref <- ref_logit_hc0_vcov(x, model$fitted.values, d$y)
  ci_ref <- ref_logit_ci_at(vcov_ref, coef(model), bb_pred_row_z(20, 3))

  # the recommendation's interval, from the compiled HC0 accumulation, equals
  # the interval from the hand-written HC0 sandwich
  expect_equal(unname(res$rec_int_ci), unname(ci_ref), tolerance = 1e-6)
})

test_that("one-way clustered logit vcov matches an independent recomputation", {
  d <- logit_sandwich_fixture()
  model <- glm(y ~ x1 + x2 + z, data = d, family = binomial())

  # vacuity guard: the clustered path must really see MORE THAN ONE cluster,
  # otherwise it collapses to a single-cluster degenerate case
  expect_gt(length(unique(d$center)), 1)

  res <- logit_sandwich_gcs(d, model, list(d$center))
  x <- ref_logit_design(d, c("x1", "x2", "z"))
  vcov_ref <- ref_logit_cluster_vcov(x, d$center, model$fitted.values, d$y)
  ci_ref <- ref_logit_ci_at(vcov_ref, coef(model), bb_pred_row_z(20, 3))

  expect_equal(unname(res$rec_int_ci), unname(ci_ref), tolerance = 1e-6)

  # and the whole reported set carries bounds from the same clustered vcov, at
  # each grid point's own coordinates
  grid <- expand.grid(x1 = seq(1, 40, by = 10), x2 = seq(1, 5, by = 2))
  reference <- t(apply(grid, 1, function(r) {
    ref_logit_ci_at(vcov_ref, coef(model), bb_pred_row_z(r[["x1"]], r[["x2"]]))
  }))
  qualifies <- reference[, "lower"] <= 0.5 & 0.5 <= reference[, "upper"]
  expect_gt(sum(qualifies), 0)
  expect_equal(nrow(res$cs), sum(qualifies))
  expect_equal(res$cs$CI_lower_bound, unname(reference[qualifies, "lower"]),
    tolerance = 1e-6
  )
  expect_equal(res$cs$CI_upper_bound, unname(reference[qualifies, "upper"]),
    tolerance = 1e-6
  )
})

test_that("two-way clustered logit vcov matches an independent CGM recomputation", {
  d <- logit_sandwich_fixture()
  model <- glm(y ~ x1 + x2 + z, data = d, family = binomial())

  # vacuity guard: BOTH clustering dimensions are non-degenerate, so the
  # Cameron-Gelbach-Miller assembly V1 + V2 - V12 exercises all three terms
  expect_gt(length(unique(d$center)), 1)
  expect_gt(length(unique(d$period)), 1)

  res <- logit_sandwich_gcs(d, model, list(d$center, d$period))
  x <- ref_logit_design(d, c("x1", "x2", "z"))
  fv <- model$fitted.values
  # V1 + V2 - V12, each an independently recomputed clustered sandwich
  vcov_ref <- ref_logit_cluster_vcov(x, d$center, fv, d$y) +
    ref_logit_cluster_vcov(x, d$period, fv, d$y) -
    ref_logit_cluster_vcov(x, paste(d$center, d$period, sep = "_"), fv, d$y)
  ci_ref <- ref_logit_ci_at(vcov_ref, coef(model), bb_pred_row_z(20, 3))

  expect_equal(unname(res$rec_int_ci), unname(ci_ref), tolerance = 1e-6)

  # the two-way set and its bounds also come from the CGM vcov, per grid point
  grid <- expand.grid(x1 = seq(1, 40, by = 10), x2 = seq(1, 5, by = 2))
  reference <- t(apply(grid, 1, function(r) {
    ref_logit_ci_at(vcov_ref, coef(model), bb_pred_row_z(r[["x1"]], r[["x2"]]))
  }))
  qualifies <- reference[, "lower"] <= 0.5 & 0.5 <= reference[, "upper"]
  expect_gt(sum(qualifies), 0)
  expect_equal(nrow(res$cs), sum(qualifies))
  expect_equal(res$cs$CI_lower_bound, unname(reference[qualifies, "lower"]),
    tolerance = 1e-6
  )
  expect_equal(res$cs$CI_upper_bound, unname(reference[qualifies, "upper"]),
    tolerance = 1e-6
  )
})

# --- the compiled kernels and the R side refuse malformed input LOUDLY --------
# The two kernels index fitted_values[i], outcome[i] and (clustered)
# cluster_index[i] over i in [0, nrow(X)) with no bounds check, and the
# clustered one uses cluster_index[i] to index its accumulators, so a length
# mismatch is an out-of-bounds READ and an out-of-range cluster index an
# out-of-bounds WRITE -- undefined behavior that used to produce a warning and
# a NaN rather than an error. A length mismatch is reachable in practice: a
# predictor column with sporadic NAs makes glm() na.omit those rows, so
# model$fitted.values comes back shorter than the design matrix. These pin that
# the guards now error, and that a well-formed call in the same test still
# succeeds so the guard is not simply erroring on everything.

test_that("the logit kernels refuse fitted_values/outcome shorter than nrow(X)", {
  set.seed(101)
  n <- 24
  x <- cbind(1, matrix(rnorm(n * 2), n, 2))
  fitted <- runif(n, 0.1, 0.9)
  outcome <- rbinom(n, 1, 0.5)
  cluster_index <- as.integer(rep_len(0:3, n))
  n_clusters <- 4L

  # the precondition the guard exists for: the vectors really are shorter than
  # the design matrix, which used to warn + return NaN instead of erroring
  short_fitted <- fitted[seq_len(n - 3)]
  short_outcome <- outcome[seq_len(n - 3)]
  expect_length(short_fitted, n - 3)
  expect_false(length(short_fitted) == nrow(x))

  # HC0 (non-clustered) kernel
  expect_error(
    LAGOtrials:::sandwich_hc0_logit_accumulate(x, short_fitted, short_outcome),
    "must have length nrow\\(X\\)"
  )
  # clustered kernel, short fitted/outcome
  expect_error(
    LAGOtrials:::sandwich_cluster_logit_accumulate(
      x, cluster_index, n_clusters, short_fitted, short_outcome
    ),
    "must have length nrow\\(X\\)"
  )
  # clustered kernel, short cluster_index
  short_index <- cluster_index[seq_len(n - 2)]
  expect_false(length(short_index) == nrow(x))
  expect_error(
    LAGOtrials:::sandwich_cluster_logit_accumulate(
      x, short_index, n_clusters, fitted, outcome
    ),
    "cluster_index must have length nrow\\(X\\)"
  )

  # NON-VACUOUS: the same well-formed inputs succeed and return the J/V pair, so
  # the guard is firing on the malformation and not on the call shape
  ok_hc0 <- LAGOtrials:::sandwich_hc0_logit_accumulate(x, fitted, outcome)
  expect_named(ok_hc0, c("J", "V"))
  expect_equal(dim(ok_hc0$J), c(3, 3))
  ok_cl <- LAGOtrials:::sandwich_cluster_logit_accumulate(
    x, cluster_index, n_clusters, fitted, outcome
  )
  expect_named(ok_cl, c("J", "V"))
  expect_equal(dim(ok_cl$V), c(3, 3))
})

test_that("the clustered kernel refuses an out-of-range cluster index", {
  set.seed(202)
  n <- 20
  x <- cbind(1, matrix(rnorm(n * 2), n, 2))
  fitted <- runif(n, 0.1, 0.9)
  outcome <- rbinom(n, 1, 0.5)
  cluster_index <- as.integer(rep_len(0:3, n))
  n_clusters <- 4L

  # an index == n_clusters is one past the last accumulator slot: an
  # out-of-bounds WRITE, not a legal cluster
  bad_high <- cluster_index
  bad_high[5] <- n_clusters
  expect_true(any(bad_high >= n_clusters))
  expect_error(
    LAGOtrials:::sandwich_cluster_logit_accumulate(
      x, bad_high, n_clusters, fitted, outcome
    ),
    "out of range"
  )

  # a negative index is likewise out of [0, n_clusters)
  bad_neg <- cluster_index
  bad_neg[7] <- -1L
  expect_true(any(bad_neg < 0))
  expect_error(
    LAGOtrials:::sandwich_cluster_logit_accumulate(
      x, bad_neg, n_clusters, fitted, outcome
    ),
    "out of range"
  )

  # n_clusters < 1 sizes the accumulators empty
  expect_error(
    LAGOtrials:::sandwich_cluster_logit_accumulate(
      x, cluster_index, 0L, fitted, outcome
    ),
    "n_clusters must be >= 1"
  )

  # NON-VACUOUS: the same setup with every index in range succeeds
  ok <- LAGOtrials:::sandwich_cluster_logit_accumulate(
    x, cluster_index, n_clusters, fitted, outcome
  )
  expect_named(ok, c("J", "V"))
})

test_that("a clustered confidence set refuses an NA cluster id", {
  # An NA cluster id has no defined cluster to fold its rows into. The logit
  # path builds match(cluster_id, unique(cluster_id)) - 1L, and unique() makes
  # NA its OWN level, so an NA-cluster row would be folded into a real cluster
  # of the compiled kernel -- a wrong number -- rather than dropped. It is
  # refused up front, on the ORIGINAL cluster vectors, so the two-way case
  # (whose intersection is built by paste(), and paste(NA, x) is the STRING
  # "NA_x", not NA) is caught before the missingness is hidden.
  set.seed(303)
  n <- 90
  d <- data.frame(
    x1 = runif(n, 1, 40), x2 = runif(n, 1, 5), z = rnorm(n, 1.75, 0.5)
  )
  eta <- -2 + 0.03 * d$x1 + 0.1 * d$x2 + 0.2 * d$z
  d$y <- rbinom(n, 1, 1 / (1 + exp(-eta)))
  d$center <- factor(paste0("c", (seq_len(n) %% 8) + 1))
  d$period <- factor((seq_len(n) %% 4) + 1)
  model <- glm(y ~ x1 + x2 + z, data = d, family = binomial())

  gcs <- function(cluster_id) {
    suppressWarnings(get_confidence_set(
      predictors_data = d[, c("x1", "x2", "z"), drop = FALSE],
      intervention_components = c("x1", "x2"), outcome_data = d$y,
      fitted_model = model, link = "logit", outcome_goal = 0.5,
      outcome_type = "continuous", intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(10, 2),
      center_characteristics = "z",
      center_characteristics_optimization_values = 1.75,
      cluster_id = cluster_id, cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      rec_int = c(20, 3)
    ))
  }

  # one-way: an NA in the single cluster vector
  cid_na <- as.character(d$center)
  cid_na[3] <- NA
  expect_true(anyNA(cid_na))
  expect_error(gcs(list(cid_na)), "cluster_id contains NA")

  # two-way: the NA is in the SECOND dimension, where paste(cluster_id1,
  # cluster_id2) would hide it as the string "NA_x". The guard reads the
  # original vectors, so it still fires.
  cid1 <- as.character(d$center)
  cid2 <- as.character(d$period)
  cid2[10] <- NA
  expect_true(anyNA(cid2))
  expect_identical(paste(NA, "x", sep = "_"), "NA_x") # the paste() hides it
  expect_error(gcs(list(cid1, cid2)), "cluster_id contains NA")

  # NON-VACUOUS: the same well-formed clustered runs, with no NA, still succeed
  # (one-way and two-way), so the guard is not erroring on every clustered call
  ok_one <- gcs(list(as.character(d$center)))
  expect_false(is.null(ok_one$rec_int_ci))
  ok_two <- gcs(list(as.character(d$center), as.character(d$period)))
  expect_false(is.null(ok_two$rec_int_ci))
})
