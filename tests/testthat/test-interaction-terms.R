# End-to-end tests for the interaction-terms path
# (include_interaction_terms = TRUE). These exercise the a:b component
# multiplication in three places no other test reaches:
#   - the grid_search interaction grid build in get_recommended_interventions()
#   - the same build in the confidence-set grid in get_confidence_set()
#   - the int_vector builder in shrinking_method() (unachievable goal)

# Build data whose continuous outcome truly depends on a, b AND a*b, on the
# identity link so the estimated outcome is the linear predictor and can be
# pinned against an independent hand fit. The interaction column must be a
# real column named "a:b" (its product), which is what the package fits.
interaction_data <- function() {
  set.seed(11)
  grid <- expand.grid(a = 0:4, b = 0:4)
  a <- rep(grid$a, times = 6)
  b <- rep(grid$b, times = 6)
  # true dependence on a, b, and the a*b interaction, plus small noise
  y <- 1 + 0.5 * a + 0.3 * b + 0.2 * a * b + rnorm(length(a), 0, 0.4)
  d <- data.frame(y = y, a = a, b = b)
  d[["a:b"]] <- d$a * d$b
  d
}

interaction_run <- function(...) {
  args <- list(
    data = interaction_data(),
    outcome_name = "y",
    outcome_type = "continuous",
    glm_family = "gaussian",
    link = "identity",
    intervention_components = c("a", "b", "a:b"),
    main_components = c("a", "b"),
    include_interaction_terms = TRUE,
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(1, 1),
    confidence_set_grid_step_size = c(1, 1),
    include_confidence_set = TRUE,
    quiet = TRUE
  )
  ov <- list(...)
  for (nm in names(ov)) args[nm] <- list(ov[[nm]])
  do.call(lago_optimization, args)
}

# the fitted design the package uses. For two numeric regressors R's a:b term
# is the product, so glm(y ~ a * b) is the same design the package fits from
# the "a:b" column, and its coefficients are the ones the optimizer reads.
# Held against the run's own estimated outcome below, this pins the a*b
# multiplication rather than letting the package agree with itself.
hand_interaction_outcome <- function(rec_int) {
  d <- interaction_data()
  m <- glm(y ~ a * b, data = d, family = gaussian(link = "identity"))
  a_rec <- rec_int[1]
  b_rec <- rec_int[2]
  as.numeric(c(1, a_rec, b_rec, a_rec * b_rec) %*% coef(m))
}

test_that("interaction-terms run recommends over the MAIN components and the estimated outcome matches an independent a*b fit", {
  # goal 3 is reachable on these bounds and leaves a non-empty confidence set,
  # so both the recommendation grid and the confidence-set grid build the
  # interaction columns and there is a real data.frame to assert on.
  res <- suppressWarnings(suppressMessages(interaction_run(
    outcome_goal = 3, outcome_goal_intention = "maximize"
  )))

  # the recommendation is over the two MAIN components, not the three
  # intervention components (the interaction column is not a knob).
  expect_length(res$rec_int, 2)
  expect_true(all(res$rec_int >= c(0, 0)))
  expect_true(all(res$rec_int <= c(4, 4)))
  expect_true(is.finite(res$est_outcome_goal))

  # INDEPENDENT hand computation: the estimated outcome at the recommendation
  # is the linear predictor c(1, a, b, a*b) %*% coef of a fit computed here,
  # not by the package.
  expect_equal(
    res$est_outcome_goal, hand_interaction_outcome(res$rec_int),
    tolerance = 1e-8
  )

  # the confidence set carries the expected columns and its knob columns are
  # the MAIN components (the interaction column is not reported as a knob).
  expect_s3_class(res$cs, "data.frame")
  expect_true(all(
    c("CI_lower_bound", "CI_upper_bound", "cost") %in% names(res$cs)
  ))
  expect_true(all(c("a", "b") %in% names(res$cs)))
  expect_false("a:b" %in% names(res$cs))
  expect_gt(nrow(res$cs), 0)
})

test_that("an unachievable goal drives the interaction shrinking path", {
  # a goal above the maximum achievable outcome on the bounds sends the run
  # into shrinking_method(), whose create_interaction_vector() multiplies the
  # a*b components (shrinking_method.R lines ~33-48). The goal-unreachable
  # warning is the mechanism that reaches the path, so it is captured, not
  # forced: we assert it fired AND keep the result for the value assertions.
  ws <- character(0)
  res <- withCallingHandlers(
    suppressMessages(interaction_run(
      outcome_goal = 100, outcome_goal_intention = "maximize",
      include_confidence_set = FALSE
    )),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # the unreachable-goal warning fired, which is what routed the run through
  # the shrinking method.
  expect_true(any(grepl("goal|reach|achiev", ws)))

  expect_length(res$rec_int, 2)
  expect_true(all(res$rec_int >= c(0, 0)))
  expect_true(all(res$rec_int <= c(4, 4)))

  # the shrinking-path estimated outcome is the linear predictor at the
  # returned recommendation, held against the same independent a*b fit.
  expect_equal(
    res$est_outcome_goal, hand_interaction_outcome(res$rec_int),
    tolerance = 1e-8
  )
})
