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
  # The builder registers a resource path for its client-side assets; clean it
  # up so constructing the app in a test does not leak the path process-globally.
  on.exit(try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE))
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
