# A goal within round off above the best reachable outcome counts as reached.
# No point meets it exactly, so the optimizer is aimed at the best outcome,
# rather than running every restart to its last iteration. Under OpenBLAS those
# iterations made such a run about 20 times slower still.

test_that("a goal just above the best outcome is reached without a long search", {
  orig <- NlcOptim::solnl
  evals <- 0
  local_mocked_bindings(
    solnl = function(X, objfun, confun = NULL, ...) {
      if (is.null(confun)) {
        return(orig(X = X, objfun = objfun, ...))
      }
      counted <- function(x) {
        evals <<- evals + 1
        confun(x)
      }
      orig(X = X, objfun = objfun, confun = counted, ...)
    },
    .package = "NlcOptim"
  )
  b <- stats::coef(stats::glm(mpg ~ gear + qsec, data = mtcars))
  best <- sum(b * c(1, 10, 3500))
  r <- suppressWarnings(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 3500), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = best * (1 + 5e-11), include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_false(r$shrinking_method_used)
  expect_equal(r$rec_int, c(10, 3500))
  expect_lt(evals, 2000)
})

test_that("lago_budget() extends a continuous goal grid one goal at a time", {
  sizes <- integer(0)
  real <- lago_sensitivity
  local_mocked_bindings(lago_sensitivity = function(..., values) {
    sizes <<- c(sizes, length(values))
    real(..., values = values)
  })
  suppressWarnings(suppressMessages(lago_budget(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity", intervention_components = "gear",
    intervention_lower_bounds = 0, intervention_upper_bounds = 100,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal_intention = "maximize",
    budget = 1e4, n_grid = 5
  )))
  # the first sweep has n_grid goals, and each extension after it one
  expect_equal(sizes[1], 5L)
  expect_true(sizes[2] == 1L)
})

test_that("every search for the best outcome caps its iterations", {
  orig <- NlcOptim::solnl
  caps <- c()
  local_mocked_bindings(
    solnl = function(X, objfun, confun = NULL, ..., maxIter = 4000) {
      # the search for the best outcome has no constraint
      if (is.null(confun)) caps <<- c(caps, maxIter)
      if (is.null(confun)) orig(X = X, objfun = objfun, ..., maxIter = maxIter) else
        orig(X = X, objfun = objfun, confun = confun, ..., maxIter = maxIter)
    },
    .package = "NlcOptim"
  )
  suppressWarnings(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 30, include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_gt(length(caps), 10)
  expect_true(all(caps <= 100))
})
