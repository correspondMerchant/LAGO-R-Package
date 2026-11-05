# NlcOptim::solnl() can return points outside the bounds. On the ubuntu-26.04
# runners, with OpenBLAS 0.3.32, it returned a component of 7474.9 for a bound
# of 4, and a component of -9.5e-05 for a bound of 0. These tests wrap solnl()
# so it does the same, and check that the recommendation does not change.

real_solnl <- NlcOptim::solnl

solver_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

solver_case_shrinking <- function() {
  set.seed(2)
  d <- data.frame(a = stats::runif(200, 0, 4), b = stats::runif(200, 0, 4))
  d$y <- 1 + 0.8 * d$a + 0.2 * d$a * d$b + stats::rnorm(200, 0, 0.5)
  d[["a:b"]] <- d$a * d$b
  list(data = d, run = function() solver_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("a", "a:b"), main_components = c("a", "b"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4), cost_list_of_vectors = list(c(0, 1), c(0, 2)),
    outcome_goal = 20, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE, quiet = TRUE
  )))
}

test_that("a highest outcome restart far outside the bounds does not make an unreachable goal look reachable", {
  case <- solver_case_shrinking()
  clean <- case$run()
  expect_true(clean$shrinking_method_used)
  first <- TRUE
  local_mocked_bindings(
    solnl = function(X = NULL, objfun = NULL, confun = NULL, ...) {
      r <- real_solnl(X = X, objfun = objfun, confun = confun, ...)
      # the highest outcome search has no constraint function
      if (is.null(confun) && first) {
        first <<- FALSE
        r$par <- c(4, 7474.9)
        r$fn <- objfun(r$par)
      }
      r
    },
    .package = "NlcOptim"
  )
  off <- case$run()
  expect_true(off$shrinking_method_used)
  expect_equal(off$rec_int, clean$rec_int, tolerance = 1e-8)
  expect_equal(off$rec_int[2], mean(case$data$b), tolerance = 1e-8)
})

test_that("a cost restart outside the bounds does not hide a cheaper one inside them", {
  set.seed(3)
  up <- c(1500, 6000, 1, 10)
  u <- sapply(1:4, function(i) stats::runif(1600))
  b <- stats::runif(4, 0.3, 0.8) * 2
  y <- stats::rbinom(1600, 1, stats::plogis(-1 + u %*% b))
  slope <- stats::runif(4, 1, 5)
  d <- as.data.frame(sweep(u, 2, up, `*`))
  names(d) <- paste0("x", 1:4)
  d$y <- y
  run <- function() solver_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = paste0("x", 1:4), intervention_lower_bounds = rep(0, 4),
    intervention_upper_bounds = up, outcome_goal = 0.7045,
    cost_list_of_vectors = lapply(1:4, function(i) c(1, slope[i] / up[i])),
    include_confidence_set = FALSE, quiet = TRUE
  ))
  clean <- run()
  beta <- stats::coef(clean$model)
  k <- 0
  local_mocked_bindings(
    solnl = function(X = NULL, objfun = NULL, confun = NULL, ...) {
      r <- real_solnl(X = X, objfun = objfun, confun = confun, ...)
      if (!is.null(confun)) {
        k <<- k + 1
        if (k == 1) {
          # the best point with x4 pushed below its bound of 0, and x3 raised
          # to keep the same outcome, so it costs less than any point inside
          p <- clean$rec_int
          p[4] <- -0.5
          p[3] <- p[3] + 0.5 * beta[["x4"]] / beta[["x3"]]
          r$par <- p
        } else if (k <= 11) {
          # the other diagonal restarts stop at a costly point that reaches the goal
          r$par <- up
        }
      }
      r
    },
    .package = "NlcOptim"
  )
  off <- run()
  expect_equal(off$rec_int_cost, clean$rec_int_cost, tolerance = 1e-6)
  expect_true(all(off$rec_int >= 0 & off$rec_int <= up))
})

test_that("an extra start that stops short of the goal is skipped", {
  case <- solver_case_shrinking()
  d <- case$data
  run <- function() solver_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("a", "a:b"), main_components = c("a", "b"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4), cost_list_of_vectors = list(c(0, 1), c(0, 2)),
    outcome_goal = 4, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE, quiet = TRUE
  ))
  clean <- run()
  expect_false(clean$shrinking_method_used)
  k <- 0
  local_mocked_bindings(
    solnl = function(X = NULL, objfun = NULL, confun = NULL, lb = NULL, ...) {
      r <- real_solnl(X = X, objfun = objfun, confun = confun, lb = lb, ...)
      if (!is.null(confun)) {
        k <<- k + 1
        # after the diagonal restarts, one start, and the run that finishes
        # it, stop a hair short of the goal for a saving under 0.1%. The
        # others stop at the lower bounds, which cost the least and do not
        # reach the goal.
        if (k %in% 12:13) r$par <- clean$rec_int * (1 - 1e-7)
        if (k > 13) r$par <- lb
      }
      r
    },
    .package = "NlcOptim"
  )
  off <- run()
  expect_gt(k, 11)
  expect_equal(off$rec_int_cost, clean$rec_int_cost, tolerance = 1e-6)
  expect_gte(off$est_outcome_goal, 4 - 1e-6)
})
