# Fixes from the third bug bash for the cost designer (visualize_cost()).

# the shinyjs show and hide calls of a test session, as "show id" or "hide id"
capture_shinyjs <- function(session) {
  log <- character(0)
  session$sendCustomMessage <- function(type, message) {
    if (grepl("shinyjs-(show|hide)", type)) {
      log <<- c(log, paste(sub("shinyjs-", "", type), message$id))
    }
  }
  function() unique(log)
}

# the slider and numeric input updates of a test session

test_that(".shift_coefs re-expands a cost in the scaled position on the bounds", {
  # 1 + 2x + 3x^2 at x = 1 + 2t is 6 + 16t + 12t^2
  sh <- .shift_coefs(c(1, 2, 3), 1, 3)
  expect_equal(sh$d, c(6, 16, 12))
  expect_true(all(sh$err >= 0))
  expect_true(all(sh$err < 1e-12))
})

test_that("a cubic far from 0 that really falls is flagged", {
  # the designer's own cubic still passes on narrow bounds far from 0 (it is
  # flagged as not accurate there, which test-bb4-designer.R checks)
  for (b in list(c(1000, 1001), c(5000, 5010), c(1e4, 1e4 + 1))) {
    v <- .cost_validity(.designer_cubic_coefs(b[1], b[2], 1), b[1], b[2])
    v$accurate <- NULL
    expect_true(all(unlist(v)), info = paste(b, collapse = ", "))
  }
  # the same cubic tilted down by 50 per unit falls by 49 over the bounds
  tilted <- function(lb) {
    co <- .designer_cubic_coefs(lb, lb + 1, 1)
    co[1] <- co[1] + 50 * lb
    co[2] <- co[2] - 50
    co
  }
  v <- .cost_validity(tilted(1000), 1000, 1001)
  expect_false(v$non_decreasing)
  expect_false(v$positive_marginal)
  v <- .cost_validity(tilted(1e5), 1e5, 1e5 + 1)
  expect_false(v$non_decreasing)
  expect_false(v$positive_marginal)
})

test_that("a falling total with a small unit cost is flagged", {
  v <- .cost_validity(c(1e-6, -2e-8), 0, 1)
  expect_false(v$non_decreasing)
  expect_false(v$positive_marginal)
  expect_true(v$non_negative_total)
})

test_that("the app shows the non-decreasing warning for a small falling cost", {
  app <- suppressWarnings(.build_visualize_cost_app("A", 1e-6, "linear", 0, 1))
  on.exit(suppressWarnings(
    try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE)
  ))
  shiny::testServer(app, {
    log <- capture_shinyjs(session)
    session$setInputs(cost_type = "linear", coef_1_0 = 1e-6, coef_1_1 = -2e-8)
    expect_true("show plot_warning_1" %in% log())
    expect_true("show coef_warning_1" %in% log())
  })
})

test_that("the marginal warning by the coefficients follows the marginal check", {
  # a check that finds only a negative marginal shows the marginal warnings and
  # hides the non-decreasing one
  local_mocked_bindings(.cost_validity = function(...) {
    list(non_decreasing = TRUE, positive_marginal = FALSE, non_negative_total = TRUE)
  })
  p <- .build_cost_parts("A", 1, "linear", 0, 1)
  shiny::testServer(p$server, {
    log <- capture_shinyjs(session)
    session$setInputs(cost_type = "linear", coef_1_0 = 0, coef_1_1 = 1)
    expect_true("show coef_warning_1" %in% log())
    expect_true("show negative_cost_warning_1" %in% log())
    expect_true("hide plot_warning_1" %in% log())
  })
  # a valid cost hides it
  local_mocked_bindings(.cost_validity = function(...) {
    list(non_decreasing = TRUE, positive_marginal = TRUE, non_negative_total = TRUE)
  })
  p <- .build_cost_parts("A", 1, "linear", 0, 1)
  shiny::testServer(p$server, {
    log <- capture_shinyjs(session)
    session$setInputs(cost_type = "linear", coef_1_0 = 0, coef_1_1 = 1)
    expect_true("hide coef_warning_1" %in% log())
  })
})

test_that("a drag widens the slider in proportion to the value", {
  app <- suppressWarnings(.build_visualize_cost_app("A", 1e-6, "linear", 0, 1))
  on.exit(suppressWarnings(
    try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE)
  ))
  ui <- .build_cost_parts("A", 1e-6, "linear", 0, 1)$ui
  s0 <- designer_row(ui, "coef_1_0")$scale
  s1 <- designer_row(ui, "coef_1_1")$scale
  shiny::testServer(app, {
    upd <- capture_updates(session)
    # the slider and its range inputs are in the row's units
    session$setInputs(
      cost_type = "linear", coef_1_0 = 0, coef_1_1 = 1e-6 * s1,
      range_min_1_0 = -1e-4 * s0, range_max_1_0 = 1e-4 * s0,
      range_min_1_1 = -9.9e-5 * s1, range_max_1_1 = 0.000101 * s1
    )
    session$setInputs(dragged_coefs_1 = list(coefs = list(0, 3e-4), nonce = 1))
    s <- upd("coef_1_1")
    expect_equal(s$value, 3e-4 * s1)
    expect_equal(s$min, -9.9e-5 * s1)
    expect_true(s$max > 3e-4 * s1 && s$max < 1e-2 * s1)
    expect_equal(s$step, slider_step(s$min, s$max))
    expect_equal(upd("range_max_1_1")$value, s$max)
    # a drag below the range widens the lower end the same way
    session$setInputs(dragged_coefs_1 = list(coefs = list(-5e-4, 1e-6), nonce = 2))
    s <- upd("coef_1_0")
    expect_true(s$min < -5e-4 * s0 && s$min > -1e-2 * s0)
    expect_equal(s$max, 1e-4 * s0)
  })
})

test_that("slider ranges scale with the bounds and steps with the value", {
  # on [0, 0.001] the x^2 coefficient must move by about 1000 to bend the curve
  r <- compute_slider_range(0, 1, power = 2, lb = 0, ub = 1e-3)
  expect_equal(c(r$min, r$max), c(-1000, 1000))
  r <- compute_slider_range(0, 1, power = 4, lb = 0, ub = 1e-3)
  expect_equal(c(r$min, r$max), c(-1e9, 1e9))
  # the swing is capped so the range stays finite
  r <- compute_slider_range(0, 1, power = 4, lb = 0, ub = 1e-90)
  expect_true(is.finite(r$min) && is.finite(r$max))
  # the designer opens the x^2 slider on that range
  p <- .build_cost_parts("A", 1, "cubic", 0, 1e-3)
  # the tag tree, without rendering its dependencies
  tags <- htmltools::tagQuery(p$ui)$find("#range_max_1_2")$selectedTags()
  expect_equal(htmltools::tagGetAttribute(tags[[1]], "value"), "1000")
  # the step of a wide range grows with it, and a huge value still moves
  expect_equal(slider_step(-4e11, 6e11), 1e6)
  step <- slider_step(1e12, 1e12 + 1)
  expect_true(1e12 + step > 1e12)
  # the usual steps are unchanged
  expect_equal(slider_step(-10.864, 16.296), 1e-5)
  expect_equal(slider_step(-1e-4, 1e-4), 1e-6)
})

test_that("the cost summary keeps small costs and narrow bounds", {
  expect_equal(format_coef(1e-6), "1e-06")
  expect_equal(format_coef(-0.000123456), "-0.000123")
  expect_equal(format_coef(1234.56789), "1234.568")
  expect_equal(format_coef(Inf), "Inf")
  on.exit(suppressWarnings(
    try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE)
  ))
  app <- suppressWarnings(.build_visualize_cost_app("A", 1e-6, "linear", 0, 1))
  shiny::testServer(app, {
    session$setInputs(cost_type = "linear")
    expect_match(output$cost_summary_1, "Total cost at A = 1: 1e-06", fixed = TRUE)
  })
  app <- suppressWarnings(.build_visualize_cost_app("A", 1, "linear", 0, 1e-6))
  shiny::testServer(app, {
    session$setInputs(cost_type = "linear")
    expect_match(output$cost_summary_1, "Total cost at A = 1e-06: 1e-06", fixed = TRUE)
  })
})

test_that("visualize_cost refuses a form vector and missing or empty names", {
  testthat::local_mocked_bindings(
    runApp = function(...) stop("the app was launched"),
    .package = "shiny"
  )
  expect_error(
    visualize_cost("A", 1, c("linear", "cubic"), 0, 1),
    "must be a single character string"
  )
  expect_error(
    visualize_cost(character(0), numeric(0), "linear", numeric(0), numeric(0)),
    "at least one component name"
  )
  expect_error(
    visualize_cost(NA_character_, 1, "linear", 0, 1),
    "must not be missing or empty"
  )
  expect_error(
    visualize_cost("", 1, "linear", 0, 1),
    "must not be missing or empty"
  )
})
