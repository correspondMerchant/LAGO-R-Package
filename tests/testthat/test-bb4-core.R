# Tests for the core fixes of the third bug bash round. Each fails before its fix.

bb4_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("every start of the cost search caps its iterations", {
  orig <- NlcOptim::solnl
  caps <- c()
  local_mocked_bindings(
    solnl = function(X, objfun, confun = NULL, ..., maxIter = 4000) {
      if (!is.null(confun)) caps <<- c(caps, maxIter)
      if (is.null(confun)) orig(X = X, objfun = objfun, ..., maxIter = maxIter) else
        orig(X = X, objfun = objfun, confun = confun, ..., maxIter = maxIter)
    },
    .package = "NlcOptim"
  )
  bb4_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 30, include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_gt(length(caps), 10)
  expect_true(all(caps <= 500))
})

bb4_interaction <- function(goal, ...) {
  set.seed(1)
  d <- expand.grid(a = 0:4, b = 0:4, rep = 1:8)
  d$y <- 1 + 0.5 * d$a - 0.3 * d$b + 0.2 * d$a * d$b + stats::rnorm(nrow(d), sd = 0.1)
  d[["a:b"]] <- d$a * d$b
  bb4_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("a", "b", "a:b"), main_components = c("a", "b"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4), cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = goal, include_confidence_set = FALSE, quiet = TRUE, ...
  ))
}

test_that("with interactions a goal just out of reach shrinks near the best corner", {
  best <- bb4_interaction(1)
  b <- stats::coef(best$model)
  top <- b[[1]] + 4 * b[[2]] + 4 * b[[3]] + 16 * b[[4]]
  r <- bb4_interaction(top + 0.001)
  expect_true(r$shrinking_method_used)
  # it stays at the corner that reaches the best outcome, not the stage 1 means
  expect_equal(r$rec_int, c(4, 4), tolerance = 1e-3)
  r2 <- bb4_interaction(top * 1.02)
  expect_true(all(r2$rec_int > 3))
})

test_that("the confidence set does not depend on how far a component is from 0", {
  shifted <- mtcars
  shifted$qsec <- shifted$qsec + 1e5
  fit <- function(d, lo, up) bb4_quiet(lago_optimization(
    data = d, outcome_name = "mpg", outcome_type = "continuous", glm_family = "gaussian",
    link = "identity", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = lo, intervention_upper_bounds = up, outcome_goal = 25,
    quiet = TRUE, cost_list_of_vectors = list(c(0, 4), c(0, 6)),
    confidence_set_grid_step_size = c(0.5, 0.5)
  ))
  a <- fit(shifted, c(3, 1e5 + 14), c(5, 1e5 + 23))
  b <- fit(mtcars, c(3, 14), c(5, 23))
  expect_equal(a$est_outcome_ci, b$est_outcome_ci)
  expect_equal(a$confidence_set_size_percentage, b$confidence_set_size_percentage)
  # the same inverse as solve() on an ordinary design, with weights too
  X <- cbind(1, mtcars$wt, mtcars$qsec)
  w <- mtcars$carb
  expect_equal(LAGOtrials:::xtx_inverse(X), solve(crossprod(X)), ignore_attr = TRUE)
  expect_equal(LAGOtrials:::xtx_inverse(X, w), solve(t(X) %*% (w * X)), ignore_attr = TRUE)
  expect_error(LAGOtrials:::xtx_inverse(cbind(1, 1:3, 2 * (1:3))), "computationally singular")
})

test_that("a cubic that round off would swamp falls back to the linear cost", {
  expect_warning(
    k <- LAGOtrials:::cost_fxn_calculator(1e5, 1e5 + 1, 3, "cubic")[[1]],
    "linear cost \\(3 per unit\\) is used"
  )
  expect_equal(k, c(0, 3, 0, 0, 0))
  # ordinary bounds keep the cubic, with no warning
  expect_no_warning(k <- LAGOtrials:::cost_fxn_calculator(100, 110, 3, "cubic")[[1]])
  expect_true(any(k[3:5] != 0))
})

test_that("a direct get_confidence_set() call checks center weights and reads number columns as factors", {
  set.seed(4)
  d <- data.frame(
    center = rep(1:4, each = 60), period = rep(1:3, length.out = 240),
    x1 = stats::runif(240, 0, 10), x2 = stats::runif(240, 0, 5)
  )
  d$y <- 1 + 0.3 * d$x1 + 0.2 * d$x2 + 0.1 * d$center + stats::rnorm(240)
  r <- bb4_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 5), cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = 4, include_center_effects = TRUE, include_time_effects = TRUE,
    time_effect_optimization_value = 2, confidence_set_grid_step_size = c(2, 1),
    quiet = TRUE
  ))
  cs <- function(...) {
    args <- list(
      predictors_data = d[c("center", "period", "x1", "x2")],
      include_center_effects = TRUE, include_time_effects = TRUE,
      time_effect_optimization_value = 2, intervention_components = c("x1", "x2"),
      outcome_data = d$y, fitted_model = r$model, link = "identity", outcome_goal = 4,
      outcome_type = "continuous", intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(2, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = r$rec_int
    )
    dots <- list(...)
    args[names(dots)] <- dots
    bb4_quiet(do.call(get_confidence_set, args))
  }
  expect_error(cs(), "one weight per center of 'fitted_model', 4 in all")
  expect_error(cs(center_weights_for_outcome_goal = c(0.5, 0.5)), "but it has 2")
  # integer center and period columns are read as the model's factors
  out <- cs(center_weights_for_outcome_goal = rep(0.25, 4))
  expect_length(out$rec_int_ci, 2)
})

test_that("an explicit NA period level can be chosen", {
  d <- mtcars
  d$period <- addNA(factor(ifelse(seq_len(nrow(d)) %% 3 == 0, NA, seq_len(nrow(d)) %% 2)))
  m <- stats::glm(mpg ~ period + wt, data = d)
  names_p <- grep("^period", names(stats::coef(m)), value = TRUE)
  ind <- LAGOtrials:::time_effect_indicator(m, names_p, NA_character_)
  expect_equal(sum(ind), 1)
  expect_equal(names_p[ind == 1], "periodNA")
  # the reference period still has no indicator
  expect_equal(sum(LAGOtrials:::time_effect_indicator(m, names_p, "0")), 0)
})

test_that("the separation warning is for binary models only", {
  # a normal outcome in dollars, with a covariate z of no effect: its standard
  # error is large in dollars, but the fit is healthy
  set.seed(2)
  d <- data.frame(x = stats::runif(200, 0, 5), z = stats::rnorm(200))
  d$y <- 50000 + 20000 * d$x + stats::rnorm(200, sd = 40000)
  z_coef <- stats::coef(stats::lm(y ~ x + z, d))[["z"]]
  d$y <- d$y - (z_coef - 10) * d$z
  m <- stats::glm(y ~ x + z, data = d)
  expect_gt(stats::coef(summary(m))["z", 2], 1e3)
  expect_no_warning(LAGOtrials:::diagnose_model_fit(m, "x"))
})

test_that("a center with zero weights is warned about once", {
  d <- mtcars
  d$center <- factor(rep(c("A", "B", "C", "D"), length.out = nrow(d)))
  found <- character(0)
  withCallingHandlers(
    suppressMessages(lago_optimization(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(1.5, 15),
      intervention_upper_bounds = c(5, 22), outcome_goal = 22,
      cost_list_of_vectors = list(c(0, 4), c(0, 6)), include_center_effects = TRUE,
      weights = ifelse(d$center == "C", 0, 1), include_confidence_set = FALSE, quiet = TRUE
    )),
    warning = function(w) {
      found <<- c(found, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(sum(grepl("'C'", found, fixed = TRUE)), 1)
})
