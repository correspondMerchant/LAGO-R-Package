# Tests for lago_sensitivity(): the robustness sweep around a LAGO
# recommendation. The mtcars continuous example is small, fast, and
# deterministic (see test-optimization.R), and lago_sensitivity() forces the
# confidence set off internally, so each sweep runs quickly.

# Baseline mtcars arguments shared across the behavioral tests. `parameter`,
# `values`, and any per-test overrides are added at the call site.
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

test_that("outcome_goal sweep returns a tidy lago_sensitivity data.frame", {
  vals <- c(25, 30, 35, 40)
  sens <- suppressWarnings(suppressMessages(do.call(
    lago_sensitivity,
    c(mtcars_base, list(parameter = "outcome_goal", values = vals))
  )))

  # class and shape
  expect_s3_class(sens, "lago_sensitivity")
  expect_s3_class(sens, "data.frame")
  expect_equal(nrow(sens), length(vals))

  # expected columns: value, one per component, cost, outcome, status
  expect_true(all(
    c("value", "gear", "qsec", "rec_int_cost", "est_outcome_goal", "status")
    %in% names(sens)
  ))
  expect_equal(sens$value, vals)

  # attributes
  expect_equal(attr(sens, "parameter"), "outcome_goal")
  expect_equal(attr(sens, "component_names"), c("gear", "qsec"))

  # all runs succeeded
  expect_true(all(sens$status == "ok"))
  expect_false(anyNA(sens$rec_int_cost))
  expect_false(anyNA(sens$est_outcome_goal))

  # non-vacuous: the goals are all above the observed mean (so they are the
  # stringent, maximize direction) and the cost actually varies.
  expect_true(all(vals > mean(mtcars$mpg)))
  expect_true(max(sens$rec_int_cost) > min(sens$rec_int_cost))

  # cost-of-stringency: a stricter maximize goal costs at least as much.
  cost <- sens$rec_int_cost
  expect_true(all(diff(cost) >= -1e-6))
})

test_that("cost_multiplier sweep is argmin-invariant (cost scales by f)", {
  # grid_search makes the argmin exact: scaling every cost coefficient by a
  # positive f cannot change which grid point is cheapest, so the recommended
  # intervention is identical across multipliers and the cost is baseline * f.
  mult <- c(0.5, 1, 2)
  sens <- suppressWarnings(suppressMessages(do.call(
    lago_sensitivity,
    c(
      mtcars_base,
      list(
        outcome_goal = 40,
        optimization_method = "grid_search",
        optimization_grid_search_step_size = c(1, 35),
        parameter = "cost_multiplier",
        values = mult
      )
    )
  )))

  expect_true(all(sens$status == "ok"))

  # non-vacuous preconditions
  expect_true(length(unique(mult)) > 1L)
  base_row <- which(sens$value == 1)
  expect_length(base_row, 1L)
  base_cost <- sens$rec_int_cost[base_row]
  expect_true(base_cost > 0)

  # recommended intervention identical across every multiplier
  for (cn in attr(sens, "component_names")) {
    expect_equal(sens[[cn]], rep(sens[[cn]][base_row], nrow(sens)))
  }

  # cost is exactly the baseline cost times the multiplier
  expect_equal(sens$rec_int_cost, base_cost * sens$value, tolerance = 1e-6)
})

test_that("a single failing run does not abort the sweep", {
  # shrinkage_threshold must be in (0, 1); 1.5 makes exactly one run stop in
  # validation while the others (0.1, 0.25) succeed. Capture warnings so the
  # single sweep warning can be asserted without other warnings interfering.
  warns <- character(0)
  sens <- withCallingHandlers(
    suppressMessages(do.call(
      lago_sensitivity,
      c(
        mtcars_base,
        list(
          outcome_goal = 40,
          parameter = "shrinkage_threshold",
          values = c(0.1, 0.25, 1.5)
        )
      )
    )),
    warning = function(cond) {
      warns <<- c(warns, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )

  # sweep completed with all rows present
  expect_equal(nrow(sens), 3L)

  # the failing row (1.5) has NA outputs and a non-"ok" status
  bad <- which(sens$value == 1.5)
  expect_length(bad, 1L)
  expect_true(is.na(sens$rec_int_cost[bad]))
  expect_true(is.na(sens$est_outcome_goal[bad]))
  expect_true(is.na(sens$gear[bad]))
  expect_true(is.na(sens$qsec[bad]))
  expect_false(sens$status[bad] == "ok")

  # the other rows are fine
  good <- which(sens$value != 1.5)
  expect_true(all(sens$status[good] == "ok"))
  expect_false(anyNA(sens$rec_int_cost[good]))

  # exactly the failure warning was emitted, naming the failed value
  expect_true(any(grepl("run\\(s\\) failed", warns)))
  expect_true(any(grepl("1.5", warns)))

  # the error message is retained on the object
  em <- attr(sens, "error_messages")
  expect_true(!is.null(em))
  expect_match(em[["1.5"]], "shrinkage_threshold")
})

test_that("input validation rejects bad parameter / values", {
  # a vector-valued formal is not sweepable
  expect_error(
    lago_sensitivity(
      parameter = "intervention_lower_bounds", values = c(0, 1)
    ),
    "scalar numeric"
  )
  # a name that is not a formal at all
  expect_error(
    lago_sensitivity(parameter = "not_a_real_arg", values = 1),
    "not a formal"
  )
  # a confidence-set-only scalar is rejected (the confidence set is not computed
  # during a sweep, so sweeping it would return identical rows). confirm the
  # message names the confidence set rather than the generic scalar message.
  expect_error(
    lago_sensitivity(
      parameter = "confidence_set_alpha", values = c(0.01, 0.05, 0.1)
    ),
    "confidence set"
  )
  # parameter must be a single character string
  expect_error(
    lago_sensitivity(parameter = c("a", "b"), values = 1),
    "single"
  )
  expect_error(
    lago_sensitivity(parameter = 123, values = 1),
    "character"
  )
  # values must be a non-empty numeric vector
  expect_error(
    lago_sensitivity(parameter = "outcome_goal", values = "a"),
    "numeric"
  )
  expect_error(
    lago_sensitivity(parameter = "outcome_goal", values = numeric(0)),
    "non-empty"
  )
  # values must be finite
  expect_error(
    lago_sensitivity(parameter = "outcome_goal", values = c(1, Inf)),
    "finite"
  )
  # cost_multiplier requires cost_list_of_vectors
  expect_error(
    lago_sensitivity(
      data = mtcars, parameter = "cost_multiplier", values = c(0.8, 1.2)
    ),
    "cost_list_of_vectors"
  )
  # a non-positive cost multiplier is invalid
  expect_error(
    lago_sensitivity(
      cost_list_of_vectors = list(c(0, 4)),
      parameter = "cost_multiplier", values = c(-1, 1)
    ),
    "positive"
  )
})

# Baseline lago_optimization() call used by the object-form tests below. The
# confidence set is off and quiet is on so the initial fit is fast; the sweep
# forces both regardless. outcome_goal is supplied so the fit is valid.
mtcars_opt_args <- c(
  mtcars_base,
  list(outcome_goal = 30, include_confidence_set = FALSE, quiet = TRUE)
)

test_that("lago result carries its call arguments as an attribute", {
  opt <- suppressWarnings(suppressMessages(
    do.call(lago_optimization, mtcars_opt_args)
  ))
  ca <- attr(opt, "lago_call_args")

  # named list of the evaluated formals, with defaults filled in
  expect_type(ca, "list")
  expect_false(is.null(names(ca)))
  expect_true(all(
    c(
      "data", "outcome_name", "intervention_components",
      "cost_list_of_vectors", "outcome_goal"
    ) %in% names(ca)
  ))
  # the values are the ones the user effectively called with
  expect_s3_class(ca$data, "data.frame")
  expect_identical(ca$outcome_name, "mpg")
  expect_identical(ca$intervention_components, c("gear", "qsec"))
  expect_identical(ca$cost_list_of_vectors, list(c(0, 4), c(4, 6)))
  expect_identical(ca$outcome_goal, 30)

  # the attribute does not leak into the documented $-accessible fields or the
  # printed list membership.
  expect_false("lago_call_args" %in% names(opt))
})

test_that("object form and args form produce the same sweep", {
  vals <- c(30, 35, 40)
  opt <- suppressWarnings(suppressMessages(
    do.call(lago_optimization, mtcars_opt_args)
  ))

  s_obj <- suppressWarnings(suppressMessages(
    lago_sensitivity(opt, parameter = "outcome_goal", values = vals)
  ))
  s_args <- suppressWarnings(suppressMessages(do.call(
    lago_sensitivity,
    c(mtcars_base, list(
      outcome_goal = 30, parameter = "outcome_goal", values = vals
    ))
  )))

  # the two calling conventions must agree column for column
  cols <- c(
    "value", "gear", "qsec", "rec_int_cost", "est_outcome_goal", "status"
  )
  for (cn in cols) {
    expect_equal(s_obj[[cn]], s_args[[cn]])
  }
  # comparing as data.frames (dropping the class) also matches
  expect_equal(as.data.frame(s_obj), as.data.frame(s_args))

  # non-vacuous: the sweep actually moved the cost, so an equal comparison is
  # not trivially satisfied by a flat curve.
  expect_true(all(s_obj$status == "ok"))
  expect_true(max(s_obj$rec_int_cost) > min(s_obj$rec_int_cost))
})

test_that("... overrides the arguments stored on the object", {
  vals <- c(30, 35, 40)
  opt <- suppressWarnings(suppressMessages(
    do.call(lago_optimization, mtcars_opt_args)
  ))

  s_noover <- suppressWarnings(suppressMessages(
    lago_sensitivity(opt, parameter = "outcome_goal", values = vals)
  ))
  # a very different cost function must change the reported cost.
  s_over <- suppressWarnings(suppressMessages(lago_sensitivity(
    opt,
    cost_list_of_vectors = list(c(0, 40), c(4, 6)),
    parameter = "outcome_goal", values = vals
  )))

  expect_true(all(s_over$status == "ok"))
  # the override took effect: the costs differ from the un-overridden sweep.
  expect_false(isTRUE(all.equal(
    s_over$rec_int_cost, s_noover$rec_int_cost
  )))
})

test_that("an object without stored call args is a clear error", {
  opt <- suppressWarnings(suppressMessages(
    do.call(lago_optimization, mtcars_opt_args)
  ))
  attr(opt, "lago_call_args") <- NULL
  expect_error(
    lago_sensitivity(opt, parameter = "outcome_goal", values = c(30, 35)),
    "does not carry"
  )
})

test_that("object dispatch does not swallow a by-name data argument", {
  # a non-lago object passed positionally as `object` is rejected, not treated
  # as an argument.
  expect_error(
    lago_sensitivity(mtcars, parameter = "outcome_goal", values = c(30, 35)),
    "must be a `lago` result"
  )

  # data passed by name lands in `...`, so `object` stays NULL and the direct
  # (no-object) form still works exactly as before.
  s_direct <- suppressWarnings(suppressMessages(lago_sensitivity(
    data = mtcars,
    outcome_name = "mpg",
    outcome_type = "continuous",
    glm_family = "gaussian",
    link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal_intention = "maximize",
    parameter = "outcome_goal", values = c(30, 35)
  )))
  expect_s3_class(s_direct, "lago_sensitivity")
  expect_equal(nrow(s_direct), 2L)
})

test_that("print() and plot() methods work on a result", {
  sens <- suppressWarnings(suppressMessages(do.call(
    lago_sensitivity,
    c(mtcars_base, list(parameter = "outcome_goal", values = c(30, 35, 40)))
  )))

  expect_no_error(suppressMessages(print(sens)))

  p <- suppressMessages(plot(sens))
  expect_s3_class(p, "ggplot")

  p2 <- suppressMessages(plot(sens, show = "components"))
  expect_s3_class(p2, "ggplot")
})
