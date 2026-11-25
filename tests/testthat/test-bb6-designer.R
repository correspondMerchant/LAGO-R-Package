# Round 6 fixes to the cost designer: named unit costs and bounds in
# visualize_cost(), slider ranges on multiples of their steps, and half step
# ties.

# whether x is a whole multiple of step, up to the round-off of x / step
on_step <- function(x, step) {
  k <- x / step
  abs(k - round(k)) <= 1e-3
}

test_that("visualize_cost matches named unit costs and bounds by name", {
  old <- options(lago_cost_list = NULL)
  on.exit(options(old), add = TRUE)
  on.exit(suppressWarnings(
    try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE)
  ), add = TRUE)
  # the app closes with the button at once, and runApp() returns the list
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      shiny::testServer(appDir, {
        session$setInputs(cost_type = form)
        try(session$setInputs(quit_button = 1), silent = TRUE)
      })
      getOption("lago_cost_list")
    },
    .package = "shiny"
  )
  run <- function(...) suppressMessages(visualize_cost(c("a", "b"), ...))
  form <- "linear"
  got <- run(c(b = 10, a = 1), form, c(b = 5, a = 0), c(a = 1, b = 6))
  # names do not show up on the coefficients
  expect_identical(got, list(c(0, 1), c(0, 10)))
  form <- "cubic"
  got <- run(c(b = 10, a = 1), form, c(b = 5, a = 0), c(a = 1, b = 6))
  expect_equal(got, list(
    .designer_cubic_coefs(0, 1, 1), .designer_cubic_coefs(5, 6, 10)
  ))
  expect_null(names(got[[2]]))
  form <- "linear"
  # unnamed values still go by position
  expect_identical(run(c(1, 10), form, c(0, 5), c(1, 6)), list(c(0, 1), c(0, 10)))
  # a name that is not a component, among all named values, is refused
  expect_error(
    visualize_cost(c("a", "b"), c(a = 1, z = 10), "linear", c(0, 5), c(1, 6)),
    "The names of unit_costs must be the component names, each once: 'a', 'b'"
  )
  expect_error(
    visualize_cost(c("a", "b"), c(1, 10), "linear", c(a = 0, a = 5), c(1, 6)),
    "intervention_lower_bounds"
  )
  # the bounds are compared after the match
  expect_error(
    visualize_cost(c("a", "b"), c(1, 10), "linear", c(a = 0, b = 5), c(b = 4, a = 1)),
    "lower bounds must be less than intervention upper bounds"
  )
})

test_that(".snap_slider_range widens a range to multiples of its step", {
  r <- .snap_slider_range(-2000000.00167, -1999999.9983)
  expect_equal(r$step, 1e-5)
  expect_true(on_step(r$min, r$step) && on_step(r$max, r$step))
  expect_true(r$min <= -2000000.00167 && r$max >= -1999999.9983)
  # rounded to the step's decimals
  expect_identical(r$min, round(r$min, 5))
  expect_identical(r$max, round(r$max, 5))
  # a range already on its step keeps its ends, round-off included
  r <- .snap_slider_range(0.1 + 0.2, 1.7)
  expect_identical(r$min, 0.3)
  expect_identical(r$max, 1.7)
  # a step of 1 or more rounds to whole numbers
  r <- .snap_slider_range(-1.5e9 - 0.4, 3e9 + 0.6)
  expect_true(r$step >= 1)
  expect_true(on_step(r$min, r$step) && on_step(r$max, r$step))
  expect_true(r$min <= -1.5e9 - 0.4 && r$max >= 3e9 + 0.6)
  # widening can raise the step, which is then used to snap again
  expect_equal(slider_step(0.05, 1000000.03), 0.1)
  r <- .snap_slider_range(0.05, 1000000.03)
  expect_identical(r, list(min = 0, max = 1000001, step = 1))
  expect_equal(r$step, slider_step(r$min, r$max))
})

test_that("a half step tie reads as the exact value", {
  # the slider reads 0.0911 for 0.091105 on a step of 1e-5, a hair more than
  # half a step away
  step <- 1.0000000000000001e-05
  expect_gt(abs(0.0911 - 0.091105), step / 2)
  expect_true(.within_half_step(0.0911, 0.091105, step))
  expect_false(.within_half_step(0.09109, 0.091105, step))

  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  co <- c(0, 1882.5, 0, 0, 0.091105)
  p <- .build_cost_parts("A", 3765, "cubic", 0, 28.4453, initial_coefs = list(co))
  expect_equal(designer_row(p$ui, "coef_1_4")$step, 1e-5)
  shiny::testServer(p$server, {
    session$setInputs(coef_1_4 = 0.0911)
    expect_identical(current_cost_list()[[1]][5], 0.091105)
    # a range edit does not count the tie reading as a move
    session$setInputs(range_min_1_4 = -0.4, range_max_1_4 = 0.6)
    expect_identical(current_cost_list()[[1]][5], 0.091105)
    # a full step away is a move
    session$setInputs(coef_1_4 = 0.09109)
    expect_identical(current_cost_list()[[1]][5], 0.09109)
  })
})

test_that("the designer puts every slider range on multiples of its step", {
  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  # on [1000, 1001] the default ranges of the cubic start between steps
  p <- .build_cost_parts(c("A", "B"), c(1000, 1), "cubic", c(1000, 0), c(1001, 10))
  for (i in 0:4) {
    tag <- htmltools::tagQuery(p$ui)$find(paste0("#coef_1_", i))$selectedTags()[[1]]
    step <- as.numeric(htmltools::tagGetAttribute(tag, "data-step"))
    for (end in c("data-min", "data-max")) {
      expect_true(on_step(as.numeric(htmltools::tagGetAttribute(tag, end)), step))
    }
  }
  shiny::testServer(p$server, {
    upd <- capture_updates(session)
    # a label or type in a message is not a number
    sent <- function(id) suppressWarnings(upd(id))
    # the first flush starts the observers
    session$setInputs(cost_type = "cubic")
    # a typed range is widened for the slider
    session$setInputs(range_min_1_1 = 0.00123, range_max_1_1 = 1.0000017)
    m <- sent("coef_1_1")
    expect_equal(m$step, 1e-5)
    expect_equal(m$min, 0.00123)
    expect_equal(m$max, 1.00001)
    # a drag past the typed range widens its max, and the slider gets both
    # ends on multiples of the step
    session$setInputs(range_min_2_1 = 0.1234567, range_max_2_1 = 1.5)
    session$setInputs(dragged_coefs_2 = list(coefs = c(0, 2, 0, 0, 0.0005)))
    m <- sent("coef_2_1")
    expect_equal(m$value, 2)
    expect_equal(m$min, 0.12345)
    expect_equal(m$max, 7)
    expect_equal(sent("range_max_2_1")$value, 7)
    expect_null(sent("range_min_2_1"))
    expect_identical(current_cost_list()[[2]][2], 2)
    # a form switch and a reset use the widened default ranges
    session$setInputs(cost_type = "linear")
    for (step in list(
      function() session$setInputs(cost_type = "cubic"),
      function() session$setInputs(reset_1 = 1)
    )) {
      step()
      m <- sent("coef_1_2")
      expect_true(on_step(m$min, m$step) && on_step(m$max, m$step))
      expect_equal(sent("range_min_1_2")$value, m$min)
      expect_equal(sent("range_max_1_2")$value, m$max)
    }
  })
})
