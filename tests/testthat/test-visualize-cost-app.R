# visualize_cost() was split so the app object is built separately from being
# launched (.build_visualize_cost_app), letting the same UI/server run locally
# and in the browser (shinylive). The interactive server cannot be driven
# headlessly, but we can check the wrapper still validates its inputs and that
# the builder returns a runnable Shiny app object.

test_that("visualize_cost validates its inputs before launching", {
  expect_error(
    visualize_cost(
      component_names = 1,
      unit_costs = 1,
      default_cost_fxn_type = "linear",
      intervention_lower_bounds = 0,
      intervention_upper_bounds = 1
    ),
    "character vector"
  )
  expect_error(
    visualize_cost(
      component_names = c("A", "B"),
      unit_costs = c(1, 2),
      default_cost_fxn_type = "quadratic",
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 10)
    ),
    "'linear' or 'cubic'"
  )
  expect_error(
    visualize_cost(
      component_names = c("A", "B"),
      unit_costs = c(1, 2),
      default_cost_fxn_type = "linear",
      intervention_lower_bounds = c(0, 5),
      intervention_upper_bounds = c(10, 5)
    ),
    "less than intervention upper bounds"
  )
})

test_that(".build_visualize_cost_app returns a runnable Shiny app object", {
  builder <- getFromNamespace(".build_visualize_cost_app", "LAGOtrials")
  # Best-effort cleanup of the client-asset resource path. It may not be
  # registered at all (the app now registers it lazily when the UI/server build),
  # so also swallow removeResourcePath's "not found" warning.
  on.exit(suppressWarnings(
    try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE)
  ))
  # bslib::navbarPage emits a benign construction warning about non-nav children
  # (the app passes useShinyjs() alongside its nav panels); it predates this
  # refactor and is unrelated to the app object being valid, so keep it out of
  # the way of the class check.
  app <- suppressWarnings(builder(
    component_names = c("Coaching", "Launch"),
    unit_costs = c(1700, 8000),
    default_cost_fxn_type = "linear",
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5)
  ))
  expect_s3_class(app, "shiny.appobj")
})

test_that(".parse_cost_query reads a valid config and falls back on anything else", {
  pq <- getFromNamespace(".parse_cost_query", "LAGOtrials")
  d <- list(
    component_names = "A", unit_costs = 1, default_cost_fxn_type = "linear",
    intervention_lower_bounds = 0, intervention_upper_bounds = 5
  )
  # missing / empty query -> defaults
  expect_identical(pq("", d), d)
  expect_identical(pq(NULL, d), d)
  # a valid query is parsed into the component configuration
  v <- pq("components=x,y&lower=1,1&upper=40,5&costs=1700,8000&form=cubic", d)
  expect_equal(v$component_names, c("x", "y"))
  expect_equal(v$unit_costs, c(1700, 8000))
  expect_equal(v$intervention_lower_bounds, c(1, 1))
  expect_equal(v$intervention_upper_bounds, c(40, 5))
  expect_equal(v$default_cost_fxn_type, "cubic")
  # malformed queries fall back to the defaults rather than erroring
  expect_identical(pq("components=x,y&lower=1&upper=5&costs=2", d), d) # length mismatch
  expect_identical(pq("components=x&lower=5&upper=5&costs=2", d), d) # lower !< upper
  expect_identical(pq("components=x&lower=1&upper=5&costs=-2", d), d) # negative cost
  expect_identical(pq("components=x&lower=1&upper=5&costs=nope", d), d) # non-numeric
  # an unknown form defaults to linear
  expect_equal(
    pq("components=x&lower=1&upper=5&costs=2&form=quartic", d)$default_cost_fxn_type,
    "linear"
  )
})

test_that("the Linear/Cubic toggle switches the coefficient-vector length", {
  builder <- getFromNamespace(".build_visualize_cost_app", "LAGOtrials")
  # testServer runs the app's onStop, which already removes the resource path, so
  # this best-effort cleanup must also swallow removeResourcePath's "not found"
  # warning (try() only catches errors).
  on.exit(suppressWarnings(
    try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE)
  ))
  app <- suppressWarnings(builder(
    component_names = "A",
    unit_costs = 1700,
    default_cost_fxn_type = "linear",
    intervention_lower_bounds = 1,
    intervention_upper_bounds = 40
  ))
  shiny::testServer(app, {
    # count the coefficients in a rendered "c(a, b, ...)" vector text
    ncoef <- function(txt) length(strsplit(gsub("[c() ]", "", txt), ",")[[1]])
    # linear: a 2-coefficient vector (intercept + slope)
    session$setInputs(cost_type = "linear", coef_1_0 = 0, coef_1_1 = 1700)
    expect_equal(ncoef(output$coefficient_text_1), 2L)
    # cubic: a 5-coefficient vector (degree-4 total cost)
    session$setInputs(
      cost_type = "cubic",
      coef_1_0 = 0, coef_1_1 = 1705, coef_1_2 = 0.1,
      coef_1_3 = -0.07, coef_1_4 = 0.002
    )
    expect_equal(ncoef(output$coefficient_text_1), 5L)
    # and back to linear reads 2 again
    session$setInputs(cost_type = "linear")
    expect_equal(ncoef(output$coefficient_text_1), 2L)
  })
})
