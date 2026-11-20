# The cost designer shows a coefficient whose range is very small or very
# large in a unit of its own size, so its slider can be used.

test_that("a slider row is scaled only when its range is far from ordinary", {
  expect_equal(.slider_scale(list(min = -10, max = 16)), 1)
  expect_equal(.slider_scale(list(min = -1e-3, max = 1e-3)), 1)
  expect_equal(.slider_scale(list(min = -1e6, max = 1e6)), 1)
  expect_equal(.slider_scale(list(min = -2e-18, max = 2e-18)), 1e18)
  expect_equal(.slider_scale(list(min = -5e11, max = 5e11)), 1e-12)
  expect_equal(.slider_scale(list(min = 0, max = 0)), 1)
  expect_equal(.scale_label(1), "")
  expect_equal(.scale_label(1e18), " (shown in units of 1e-18)")
})

test_that("on wide bounds the high order sliders can bend the curve", {
  p <- .build_cost_parts("A", 1, "cubic", 0, 1e6)
  # the x^4 coefficient of a cost of about 1e6 over [0, 1e6] is about 1e-18
  r <- designer_row(p$ui, "coef_1_4")
  expect_equal(r$scale, 1e18)
  tag <- htmltools::tagQuery(p$ui)$find("#coef_1_4")$selectedTags()[[1]]
  rng <- as.numeric(c(
    htmltools::tagGetAttribute(tag, "data-min"),
    htmltools::tagGetAttribute(tag, "data-max")
  ))
  # the slider runs over about -1 to 1, in steps a slider can make
  expect_true(all(abs(rng) <= 10) && all(abs(rng) >= 0.1))
  expect_gte(r$step, 1e-6)
  # one step changes the cost at the upper bound by a small part of it
  expect_lt(r$step / r$scale * 1e6^4, 1e4)
  label <- paste(unlist(htmltools::tagQuery(p$ui)$find("label")$selectedTags()), collapse = " ")
  expect_match(label, "A^4 coefficient (shown in units of 1e-18)", fixed = TRUE)
  # ordinary rows keep their plain slider
  expect_equal(designer_row(p$ui, "coef_1_1")$scale, 1)
})

test_that("the server reads a scaled slider as the coefficient", {
  p <- .build_cost_parts("A", 1, "cubic", 0, 1e6, ns = shiny::NS("d"), embedded = TRUE)
  shiny::testServer(function(id) shiny::moduleServer(id, p$server), args = list(id = "d"), {
    session$setInputs(cost_type = "cubic", coef_1_4 = 0.5)
    expect_equal(current_cost_list()[[1]][5], 0.5e-18)
    # Reset puts the coefficient back, read through the exact value
    session$setInputs(reset_1 = 1)
    start <- .designer_cubic_coefs(0, 1e6, 1)[5]
    expect_equal(current_cost_list()[[1]][5], start)
    # a typed range is in the slider's units, and keeps the exact value
    session$setInputs(range_min_1_4 = -2, range_max_1_4 = 2)
    expect_equal(current_cost_list()[[1]][5], start)
  })
})
