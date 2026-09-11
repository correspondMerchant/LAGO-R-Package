# Tests for lago_budget(): the budget-constrained (reverse) optimization. It
# layers on lago_sensitivity()/lago_optimization(), sweeping the outcome goal to
# find the best outcome reachable within a cost budget. The mtcars continuous
# example is small, fast and deterministic (see test-optimization.R), and the
# confidence set is off throughout, so each search runs quickly. A small n_grid
# keeps the tests fast.

mtcars_base <- list(
  data = mtcars,
  outcome_name = "mpg",
  outcome_type = "continuous",
  glm_family = "gaussian",
  link = "identity",
  intervention_components = c("gear", "qsec"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(10, 350),
  cost_list_of_vectors = list(c(0, 4), c(4, 6)),
  outcome_goal_intention = "maximize"
)

run_budget <- function(budget, ...) {
  suppressWarnings(suppressMessages(do.call(
    lago_budget,
    c(mtcars_base, list(budget = budget, n_grid = 12L, quiet = TRUE), list(...))
  )))
}

test_that("a feasible budget returns a well-formed lago_budget within budget", {
  b <- run_budget(60)

  expect_s3_class(b, "lago_budget")
  expect_true(b$feasible)
  expect_true(b$binding)

  # recommendation is named by the components and is finite
  expect_equal(names(b$rec_int), c("gear", "qsec"))
  expect_false(anyNA(b$rec_int))

  # the whole point: cost does not exceed the budget, and the outcome is finite
  expect_lte(b$rec_int_cost, 60 + 1e-6)
  expect_true(is.finite(b$est_outcome))
  # for maximize the achieved outcome equals the goal that produced it
  expect_equal(b$est_outcome, b$outcome_goal, tolerance = 1e-6)

  # frontier shape
  expect_s3_class(b$frontier, "data.frame")
  expect_true(all(
    c("outcome_goal", "rec_int_cost", "est_outcome", "affordable", "status")
    %in% names(b$frontier)
  ))
  expect_true(any(b$frontier$affordable))
})

test_that("more budget buys at least as good an outcome (monotone)", {
  outs <- vapply(c(40, 60, 100, 200), function(bg) run_budget(bg)$est_outcome,
    numeric(1))
  expect_false(anyNA(outs))
  expect_true(all(diff(outs) >= -1e-6))
})

test_that("every affordable frontier point respects the budget", {
  b <- run_budget(80)
  aff <- b$frontier[b$frontier$affordable, , drop = FALSE]
  expect_true(nrow(aff) > 0)
  expect_true(all(aff$rec_int_cost <= 80 + 1e-6))
  # and the chosen outcome is the best among affordable points
  expect_equal(b$est_outcome, max(aff$est_outcome), tolerance = 1e-6)
})

test_that("a very large budget does not bind (returns the max reachable)", {
  big <- run_budget(1e7)
  expect_true(big$feasible)
  expect_false(big$binding)
  # it should be at least as good as any smaller (binding) budget's outcome
  expect_gte(big$est_outcome, run_budget(100)$est_outcome - 1e-6)
})

test_that("a budget below the cheapest reachable is infeasible and warns", {
  expect_warning(
    b <- suppressMessages(do.call(
      lago_budget,
      c(mtcars_base, list(budget = 1e-4, n_grid = 12L, quiet = TRUE))
    )),
    regexp = "budget|reachable"
  )
  expect_false(b$feasible)
  expect_true(is.na(b$rec_int_cost))
  expect_true(all(is.na(b$rec_int)))
})

test_that("the object-reuse form matches passing the arguments directly", {
  opt <- suppressMessages(do.call(
    lago_optimization,
    c(mtcars_base, list(
      outcome_goal = 30, include_confidence_set = FALSE, quiet = TRUE
    ))
  ))
  from_obj <- suppressWarnings(lago_budget(opt, budget = 80, n_grid = 12L))
  direct <- run_budget(80)
  expect_equal(from_obj$rec_int_cost, direct$rec_int_cost, tolerance = 1e-6)
  expect_equal(from_obj$est_outcome, direct$est_outcome, tolerance = 1e-6)
})

test_that("minimize direction picks the lowest reachable outcome within budget", {
  min_base <- modifyList(mtcars_base, list(outcome_goal_intention = "minimize"))
  run_min <- function(bg) {
    suppressWarnings(suppressMessages(do.call(
      lago_budget, c(min_base, list(budget = bg, n_grid = 12L, quiet = TRUE))
    )))
  }
  b <- run_min(60)
  expect_true(b$feasible)
  expect_lte(b$rec_int_cost, 60 + 1e-6)
  # among affordable points the chosen outcome is the SMALLEST (best to minimize)
  aff <- b$frontier[b$frontier$affordable, , drop = FALSE]
  expect_equal(b$est_outcome, min(aff$est_outcome), tolerance = 1e-6)

  # more budget never yields a worse (higher) minimized outcome
  outs <- vapply(c(40, 60, 100), function(x) run_min(x)$est_outcome, numeric(1))
  expect_true(all(diff(outs) <= 1e-6))

  # `binding` is direction-aware: expensive HIGH-outcome goals are worse, not
  # better, so if the best (lowest) outcome is already affordable the budget
  # must not be reported as binding just because those exist.
  expect_false(run_min(1e7)$binding)
})

test_that("input validation rejects bad budget / n_grid / object", {
  expect_error(run_budget(-1), "positive")
  expect_error(run_budget(c(1, 2)), "single")
  expect_error(run_budget(Inf), "finite")
  # call directly (not via run_budget, which hardcodes n_grid) so this reaches
  # lago_budget()'s own n_grid check; "single integer" is unique to that message.
  expect_error(
    do.call(lago_budget, c(mtcars_base, list(budget = 60, n_grid = 1L))),
    "single integer"
  )
  expect_error(lago_budget(object = 42, budget = 60), "lago")
})

test_that("print returns its argument invisibly (both feasible and not)", {
  b <- run_budget(60)
  # cli routes its output through its own sink, so assert the invisible-return
  # contract rather than capturing text.
  expect_invisible(print(b))
  expect_identical(withVisible(print(b))$value, b)

  infeasible <- suppressWarnings(suppressMessages(run_budget(1e-4)))
  expect_invisible(print(infeasible))
})

test_that("plot returns a ggplot for a feasible result", {
  skip_if_not_installed("ggplot2")
  b <- run_budget(60)
  p <- plot(b)
  expect_s3_class(p, "ggplot")
})

test_that("works for a binary outcome (goals span (0, 1))", {
  # binary path builds the goal grid over (0, 1); assert it runs and stays
  # within budget when feasible.
  b <- suppressWarnings(suppressMessages(lago_budget(
    data = infert, outcome_name = "case", outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("age", "parity"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(50, 10),
    cost_list_of_vectors = list(c(0, 4), c(0, 1)),
    outcome_goal_intention = "maximize", budget = 150, n_grid = 10L, quiet = TRUE
  )))
  expect_s3_class(b, "lago_budget")
  if (isTRUE(b$feasible)) {
    expect_lte(b$rec_int_cost, 150 + 1e-6)
    expect_true(b$est_outcome >= 0 && b$est_outcome <= 1 + 1e-6)
  }
})

test_that("outcome_goal_intention defaults to maximize when omitted", {
  args <- mtcars_base
  args$outcome_goal_intention <- NULL
  b <- suppressWarnings(suppressMessages(do.call(
    lago_budget, c(args, list(budget = 100, n_grid = 10L, quiet = TRUE))
  )))
  expect_equal(b$intention, "maximize")
})

test_that("the object-reuse form accepts `...` overrides of the stored call", {
  opt <- suppressMessages(do.call(lago_optimization, c(
    mtcars_base,
    list(outcome_goal = 30, include_confidence_set = FALSE, quiet = TRUE)
  )))
  # override a stored argument through `...`; just needs to run and stay valid.
  b <- suppressWarnings(lago_budget(
    opt, budget = 200, intervention_upper_bounds = c(6, 300), n_grid = 10L
  ))
  expect_s3_class(b, "lago_budget")
})

test_that("missing data/outcome args and a bad outcome_name error clearly", {
  no_data <- mtcars_base
  no_data$data <- NULL
  expect_error(
    do.call(lago_budget, c(no_data, list(budget = 60))), "must be available"
  )
  bad_name <- mtcars_base
  bad_name$outcome_name <- "not_a_column"
  expect_error(
    do.call(lago_budget, c(bad_name, list(budget = 60))), "not a column"
  )
  # a lago object that predates call-arg recording
  fake <- structure(list(rec_int = 1), class = "lago")
  expect_error(lago_budget(fake, budget = 60), "call arguments")
})

test_that("an unfittable model yields feasible = FALSE and an empty frontier", {
  # BB_data's pre_post is a pre/post period flag, perfectly separable from the
  # coaching components, so the outcome model never converges: every swept goal
  # errors, nothing is reachable.
  bad <- list(
    data = BB_data, outcome_name = "pre_post", outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "datafeedback"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal_intention = "maximize"
  )
  expect_warning(
    b <- suppressMessages(do.call(
      lago_budget, c(bad, list(budget = 50, n_grid = 8L, quiet = TRUE))
    )),
    regexp = "reachable|Check the model"
  )
  expect_false(b$feasible)
  expect_equal(nrow(b$frontier), 0L)

  skip_if_not_installed("ggplot2")
  expect_message(p <- plot(b), "No reachable")
  expect_null(p)
})

test_that("print notes when the budget does not bind", {
  big <- suppressWarnings(run_budget(1e7))
  expect_false(big$binding)
  expect_invisible(print(big))
})
