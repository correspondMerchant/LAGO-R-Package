# Fixes from the second bug bash for the cost designer (visualize_cost()).

test_that("a drag that the slider cannot show still reaches the cost list", {
  # unit cost 1e-6 on [0, 10]: a drag to c(0, 3e-6) moves the slope by less
  # than half a slider step from its reading, so the browser sends no new
  # slider value. The coefficient text, the cost list and Apply must still
  # use the dragged value.
  got <- NULL
  p <- .build_cost_parts("A", 1e-6, "linear", 0, 10,
    ns = shiny::NS("d"), embedded = TRUE,
    on_apply = function(cl, form) got <<- cl
  )
  shiny::testServer(function(id) shiny::moduleServer(id, p$server),
    args = list(id = "d"), {
      session$setInputs(cost_type = "linear", coef_1_0 = 0, coef_1_1 = 1e-6)
      expect_equal(current_cost_list(), list(c(0, 1e-6)))
      session$setInputs(dragged_coefs_1 = list(coefs = list(0, 1.4e-6), nonce = 1))
      expect_equal(output$coefficient_text_1, "c(0, 1.4e-06)")
      expect_equal(current_cost_list(), list(c(0, 1.4e-6)))
      session$setInputs(apply = 1)
      expect_equal(got, list(c(0, 1.4e-6)))
    }
  )
})

test_that("the server checks the marginal cost and the total cost separately", {
  # a line that starts below 0 has a positive marginal but a negative total
  v <- .cost_validity(c(-5, 1), 0, 10)
  expect_true(v$non_decreasing)
  expect_true(v$positive_marginal)
  expect_false(v$non_negative_total)
  # a falling line has a negative marginal but stays non-negative
  v <- .cost_validity(c(10, -0.5), 0, 10)
  expect_false(v$non_decreasing)
  expect_false(v$positive_marginal)
  expect_true(v$non_negative_total)
  # the page has a separate warning for a negative total cost
  p <- .build_cost_parts("A", 2, "linear", 0, 10)
  # the text of the page, without rendering its dependencies
  html <- paste(unlist(p$ui), collapse = " ")
  expect_match(html, "negative_total_warning_1", fixed = TRUE)
  expect_match(html, "Total cost should never be negative", fixed = TRUE)
})

test_that("the designer's cubic is valid on narrow bounds far from 0", {
  for (b in list(c(1000, 1001), c(5000, 5010), c(1e4, 1e4 + 1))) {
    v <- .cost_validity(.designer_cubic_coefs(b[1], b[2], 1), b[1], b[2])
    expect_true(v$non_decreasing, info = paste(b, collapse = ", "))
    expect_true(v$positive_marginal, info = paste(b, collapse = ", "))
    expect_true(v$non_negative_total, info = paste(b, collapse = ", "))
  }
  # a curve that truly falls on those bounds is still flagged
  v <- .cost_validity(c(2000, -1), 1000, 1001)
  expect_false(v$non_decreasing)
  expect_false(v$positive_marginal)
})

test_that("slider ranges and steps scale with a small unit cost", {
  r <- compute_slider_range(0, 1e-6)
  expect_equal(r$min, -1e-4)
  expect_equal(r$max, 1e-4)
  expect_equal(r$step, 1e-6)
  # the linear slope 1e-6 sits on a step of its own slider
  r <- compute_slider_range(1e-6, 1e-6)
  expect_true(r$min < 1e-6 && 1e-6 < r$max)
  expect_equal(r$step, 1e-6)
  expect_equal(((1e-6 - r$min) / r$step) %% 1, 0, tolerance = 1e-8)
  # a unit cost below 1 is no longer widened to a range of 1
  r <- compute_slider_range(0, 0.5)
  expect_equal(c(r$min, r$max), c(-0.5, 0.5))
  # a zero unit cost still falls back to a range of 1
  r <- compute_slider_range(0, 0)
  expect_equal(c(r$min, r$max), c(-1, 1))
  # the usual ranges are unchanged
  r <- compute_slider_range(2.716, 2)
  expect_equal(c(r$min, r$max, r$step), c(-10.864, 16.296, 1e-5))
})

test_that("visualize_cost refuses infinite bounds and negative unit costs", {
  # never launch the app here, even when the validation lets a call through
  testthat::local_mocked_bindings(
    runApp = function(...) stop("the app was launched"),
    .package = "shiny"
  )
  call_vc <- function(uc = 1, lb = 0, ub = 10) {
    visualize_cost("A", uc, "linear", lb, ub)
  }
  expect_error(call_vc(ub = Inf), "upper bounds must all be finite")
  expect_error(call_vc(lb = -Inf), "lower bounds must all be finite")
  expect_error(call_vc(lb = NA_real_), "lower bounds must all be finite")
  expect_error(call_vc(uc = -3), "unit costs must not be negative")
})
