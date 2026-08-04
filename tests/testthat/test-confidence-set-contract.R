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
    # reference values verified against the current tree
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
