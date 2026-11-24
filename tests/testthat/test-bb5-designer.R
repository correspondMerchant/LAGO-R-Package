# Round 5 fixes to the cost designer: the linear start on bounds below 0 and
# starting coefficients for each form.

test_that("the linear form starts at a total cost of 0 on bounds below 0", {
  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  p <- .build_cost_parts(c("A", "B", "C"), c(1, 2, 3), "linear", c(-5, -10, 2), c(5, -1, 4))
  shiny::testServer(p$server, {
    session$setInputs(cost_type = "linear")
    cl <- current_cost_list()
    # the cost at each lower bound is 0 below 0 and unit cost * bound above 0
    expect_equal(cl, list(c(5, 1), c(20, 2), c(0, 3)))
    lbs <- c(-5, -10, 2)
    at_lb <- vapply(1:3, function(ci) cl[[ci]][1] + cl[[ci]][2] * lbs[ci], numeric(1))
    expect_equal(at_lb, c(0, 0, 6))
    v <- .cost_validity(cl[[1]], -5, 5)
    expect_true(v$non_negative_total)
    v <- .cost_validity(cl[[2]], -10, -1)
    expect_true(v$non_negative_total)
    # a switch to the cubic and back keeps the start at 0
    session$setInputs(cost_type = "cubic")
    session$setInputs(cost_type = "linear")
    expect_equal(current_cost_list()[[1]], c(5, 1))
  })
})

test_that("visualize_cost opens the linear form at a cost of 0 below 0", {
  app <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      app <<- appDir
      NULL
    },
    .package = "shiny"
  )
  expect_null(visualize_cost("A", 2, "linear", -3, 3))
  # the page the launched app serves starts the intercept slider at 6
  expect_s3_class(app, "shiny.appobj")
  page <- environment(app$httpHandler)$ui(list(QUERY_STRING = ""))
  tag <- htmltools::tagQuery(page)$find("#coef_1_0")$selectedTags()[[1]]
  expect_equal(as.numeric(htmltools::tagGetAttribute(tag, "data-from")), 6)
})

test_that("initial_coefs as one list still starts only the starting form", {
  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  p <- .build_cost_parts(c("A", "B"), c(1, 1), "linear", c(0, 0), c(10, 10),
    initial_coefs = list(c(3, 2), c(1, 2, 3))
  )
  shiny::testServer(p$server, {
    session$setInputs(cost_type = "linear")
    # the second vector does not fit the linear form, so it keeps the default
    expect_equal(current_cost_list(), list(c(3, 2), c(0, 1)))
    session$setInputs(cost_type = "cubic")
    expect_equal(current_cost_list()[[1]], .designer_cubic_coefs(0, 10, 1))
  })
})

test_that("initial_coefs can start each form on its own coefficients", {
  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  cubic <- c(1, 0.5, 0, 0, 0.01)
  p <- .build_cost_parts(c("A", "B"), c(1, 1), "cubic", c(0, 0), c(10, 10),
    initial_coefs = list(
      linear = list(c(4, 3), c(Inf, 1), c(9, 9)),
      cubic = list(cubic)
    )
  )
  shiny::testServer(p$server, {
    session$setInputs(cost_type = "cubic")
    expect_equal(current_cost_list(), list(cubic, .designer_cubic_coefs(0, 10, 1)))
    session$setInputs(cost_type = "linear")
    # an infinite vector and one past the last component are left out
    expect_equal(current_cost_list(), list(c(4, 3), c(0, 1)))
    # the reset goes back to the given coefficients
    session$setInputs(reset_1 = 1)
    expect_equal(current_cost_list()[[1]], c(4, 3))
  })
  # a form that is left out keeps its defaults
  p <- .build_cost_parts("A", 1, "linear", 0, 10, initial_coefs = list(cubic = list(cubic)))
  shiny::testServer(p$server, {
    session$setInputs(cost_type = "linear")
    expect_equal(current_cost_list()[[1]], c(0, 1))
    session$setInputs(cost_type = "cubic")
    expect_equal(current_cost_list()[[1]], cubic)
  })
})
