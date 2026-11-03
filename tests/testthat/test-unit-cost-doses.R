# The cubic cost can be pinned at the dose each unit cost was measured at.

dose_costs <- function(lower, upper, unit_costs, doses = NULL) {
  LAGOtrials:::cost_fxn_calculator(lower, upper, unit_costs, "cubic", unit_cost_doses = doses)
}

# the total cost polynomial at x, from its coefficients
dose_total <- function(coefs, x) sum(coefs * x^(seq_along(coefs) - 1))

test_that("the cubic cost per unit at the measured dose is the unit cost", {
  cases <- list(
    list(lower = 0, upper = 40, unit_cost = 1700, dose = 20),
    list(lower = 1, upper = 5, unit_cost = 8000, dose = 1.5),
    list(lower = 0, upper = 10, unit_cost = 0.5, dose = 10),
    list(lower = 2, upper = 6, unit_cost = 3, dose = 2.5)
  )
  for (case in cases) {
    coefs <- dose_costs(case$lower, case$upper, case$unit_cost, case$dose)[[1]]
    expect_equal(dose_total(coefs, case$dose) / case$dose, case$unit_cost, tolerance = 1e-8)
    # still a valid cost: never negative and never decreasing over the bounds
    x <- seq(case$lower, case$upper, length.out = 201)
    total <- vapply(x, function(v) dose_total(coefs, v), numeric(1))
    expect_true(all(total >= -1e-8))
    expect_true(all(diff(total) >= -1e-8 * max(abs(total))))
  }
  # restating the costs in other units only rescales the pinned curve
  base <- dose_costs(c(0, 0), c(40, 5), c(1700, 8000), c(20, 2))
  thousands <- dose_costs(c(0, 0), c(40, 5), c(1.7, 8), c(20, 2))
  expect_equal(lapply(thousands, function(v) v * 1000), base, tolerance = 1e-8)
  # each component is pinned at its own dose
  for (i in 1:2) {
    expect_equal(dose_total(base[[i]], c(20, 2)[i]) / c(20, 2)[i], c(1700, 8000)[i], tolerance = 1e-8)
  }
  # a unit cost of 0 is still the zero curve
  expect_equal(dose_costs(0, 4, 0, 2)[[1]], rep(0, 5))
})

dose_run <- function(...) {
  lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), outcome_goal = 40,
    include_confidence_set = FALSE, quiet = TRUE, ...
  )
}

test_that("the cubic cost warns when the measured doses are not given", {
  # slow, so it runs in CI and coverage but not in the CRAN check
  skip_on_cran()
  expect_warning(
    suppressMessages(dose_run(unit_costs = c(4, 6))),
    "'unit_cost_doses' is not given"
  )
  no_dose_warning <- function(...) {
    warnings <- character(0)
    withCallingHandlers(suppressMessages(dose_run(...)), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    expect_false(any(grepl("unit_cost_doses", warnings)))
  }
  no_dose_warning(unit_costs = c(4, 6), unit_cost_doses = c(4, 18))
  no_dose_warning(unit_costs = c(4, 6), default_cost_fxn_type = "linear")
  no_dose_warning(cost_list_of_vectors = list(c(0, 4), c(4, 6)))
  no_dose_warning(unit_costs = c(4, 6), cost_list_of_vectors = list(c(0, 4), c(4, 6)))
})

test_that("a sweep shows the cubic cost warning once", {
  # a budget sweep of many runs
  skip_on_cran()
  r <- suppressWarnings(suppressMessages(dose_run(unit_costs = c(4, 6))))
  count <- function(expr) {
    n <- 0
    withCallingHandlers(expr,
      warning = function(w) {
        if (grepl("unit_cost_doses", conditionMessage(w))) n <<- n + 1
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    )
    n
  }
  expect_equal(count(lago_sensitivity(r, parameter = "outcome_goal", values = c(30, 35, 40))), 1)
  expect_equal(count(lago_budget(r, budget = 300, n_grid = 5)), 1)
})

test_that("lago_optimization() uses the pinned cubic cost", {
  # slow, so it runs in CI and coverage but not in the CRAN check
  skip_on_cran()
  r <- suppressWarnings(suppressMessages(dose_run(
    unit_costs = c(4, 6), unit_cost_doses = c(4, 18), optimization_method = "grid_search"
  )))
  pinned <- dose_costs(c(0, 0), c(10, 350), c(4, 6), c(4, 18))
  expected <- dose_total(pinned[[1]], r$rec_int[1]) + dose_total(pinned[[2]], r$rec_int[2])
  expect_equal(r$rec_int_cost, expected, tolerance = 1e-8)
  # a sweep from the result keeps the pinned cost
  sweep <- suppressWarnings(suppressMessages(lago_sensitivity(
    r, parameter = "outcome_goal", values = 40
  )))
  expect_equal(sweep$rec_int_cost, r$rec_int_cost, tolerance = 1e-8)
})

test_that("unit_cost_doses is checked", {
  expect_error(
    suppressMessages(dose_run(unit_costs = c(4, 6), unit_cost_doses = 4)),
    "one dose for each unit cost"
  )
  expect_error(
    suppressMessages(dose_run(unit_costs = c(4, 6), unit_cost_doses = c(4, 400))),
    "within the lower and upper"
  )
  expect_error(
    suppressMessages(dose_run(unit_costs = c(4, 6), unit_cost_doses = c(-1, 18))),
    "within the lower and upper"
  )
  expect_error(
    suppressMessages(dose_run(unit_costs = c(4, 6), unit_cost_doses = c(NA, 18))),
    "one dose for each unit cost"
  )
})
