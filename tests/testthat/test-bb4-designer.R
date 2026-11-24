# Round 4 fixes to the cost designer: per form slider scales, slider ranges from
# the effect of each term, the accuracy warning, refused deep links and the
# cubic start on bounds below 0.

# the scale, range and label slider `id` has in the built UI
ui_row <- function(ui, id) {
  tag <- htmltools::tagQuery(ui)$find(paste0("#", id))$selectedTags()[[1]]
  num <- function(a) as.numeric(htmltools::tagGetAttribute(tag, a))
  list(scale = num("data-lago-scale"), min = num("data-min"), max = num("data-max"))
}

test_that("on [1000, 1001] the linear sliders move the coefficients", {
  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  p <- .build_cost_parts("A", 1, "linear", 1000, 1001, embedded = TRUE)
  r0 <- ui_row(p$ui, "coef_1_0")
  r1 <- ui_row(p$ui, "coef_1_1")
  # the linear rows have their own plain scale, not the cubic one
  expect_equal(c(r0$scale, r1$scale), c(1, 1))
  expect_equal(c(r1$min, r1$max), c(-4, 6))
  shiny::testServer(p$server, {
    session$setInputs(cost_type = "linear", coef_1_0 = 0, coef_1_1 = r1$max)
    expect_equal(current_cost_list()[[1]], c(0, 6))
    session$setInputs(coef_1_1 = r1$min)
    expect_equal(current_cost_list()[[1]], c(0, -4))
    session$setInputs(coef_1_0 = 0.5)
    expect_equal(current_cost_list()[[1]], c(0.5, -4))
  })
})

test_that("a form switch gives each row the new form's scale and unit", {
  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  # with a unit cost of 1e-6 the slope row has another scale in each form
  p <- .build_cost_parts("A", 1e-6, "cubic", 0, 1, embedded = TRUE)
  expect_equal(ui_row(p$ui, "coef_1_1")$scale, 1e6)
  expect_equal(ui_row(.build_cost_parts("A", 1e-6, "linear", 0, 1)$ui, "coef_1_1")$scale, 1e5)
  shiny::testServer(p$server, {
    sent <- list()
    session$sendInputMessage <- function(inputId, message) {
      sent[[inputId]] <<- message
    }
    scales <- list()
    session$sendCustomMessage <- function(type, message) {
      if (identical(type, "lago-exact-coef") && !is.null(message$scale)) {
        # the mock session puts its own prefix on the id
        scales[[sub("^.*-", "", message$id)]] <<- message$scale
      }
    }
    start <- .designer_cubic_coefs(0, 1, 1e-6)
    session$setInputs(cost_type = "cubic", coef_1_0 = 0, coef_1_1 = 0.5)
    expect_equal(current_cost_list()[[1]], start)
    session$setInputs(cost_type = "linear")
    # the browser is told the slider's new scale, and the label its unit
    expect_equal(scales[["coef_1_1"]], 1e5)
    u <- sent[["coef_1_1"]]
    expect_equal(as.numeric(c(u$min, u$max, u$value)), c(-0.4, 0.6, 0.1))
    expect_equal(as.character(u$label$html), "A^1 coefficient (shown in units of 1e-05)")
    # while the slider still shows its cubic reading, the coefficient is the
    # linear start
    expect_equal(current_cost_list()[[1]], c(0, 1e-6))
    # the slider's new readings are in the linear unit
    session$setInputs(coef_1_1 = 0.1)
    expect_equal(current_cost_list()[[1]], c(0, 1e-6))
    session$setInputs(coef_1_1 = 0.25)
    expect_equal(current_cost_list()[[1]], c(0, 2.5e-6))
    # a typed range is in the linear unit too
    session$setInputs(range_min_1_1 = 0, range_max_1_1 = 1)
    expect_equal(as.numeric(sent[["coef_1_1"]]$max), 1)
    session$setInputs(coef_1_1 = 0.7)
    expect_equal(current_cost_list()[[1]], c(0, 7e-6))
    # Reset and a drag use the linear unit
    session$setInputs(reset_1 = 1)
    expect_equal(as.numeric(sent[["coef_1_1"]]$value), 0.1)
    session$setInputs(coef_1_1 = 0.1)
    expect_equal(current_cost_list()[[1]], c(0, 1e-6))
    session$setInputs(dragged_coefs_1 = list(coefs = list(2e-7, 3e-6), nonce = 1))
    expect_equal(as.numeric(sent[["coef_1_0"]]$value), 0.2)
    expect_equal(as.numeric(sent[["coef_1_1"]]$value), 0.3)
    session$setInputs(coef_1_0 = 0.2, coef_1_1 = 0.3)
    expect_equal(current_cost_list()[[1]], c(2e-7, 3e-6))
    # back to cubic, the rows get the cubic scale again
    session$setInputs(cost_type = "cubic")
    expect_equal(scales[["coef_1_1"]], 1e6)
    expect_equal(as.character(sent[["coef_1_1"]]$label$html), "A^1 coefficient (shown in units of 1e-06)")
    # the slope row's old reading is stale, and reads as the cubic start
    expect_equal(current_cost_list()[[1]][2], start[2])
    # the browser moves the sliders to the cubic start
    session$setInputs(coef_1_0 = 0, coef_1_1 = 0.5)
    expect_equal(current_cost_list()[[1]], start)
  })
})

test_that("the slider label shows the unit of the starting form", {
  p <- .build_cost_parts("A", 1, "cubic", 0, 1e6)
  labels <- vapply(
    htmltools::tagQuery(p$ui)$find("label")$selectedTags(),
    function(t) paste(unlist(t$children), collapse = ""), character(1)
  )
  expect_true("A^4 coefficient (shown in units of 1e-18)" %in% labels)
  # a linear start has plain rows
  p <- .build_cost_parts("A", 1, "linear", 1000, 1001)
  labels <- vapply(
    htmltools::tagQuery(p$ui)$find("label")$selectedTags(),
    function(t) paste(unlist(t$children), collapse = ""), character(1)
  )
  expect_true("A^1 coefficient" %in% labels)
})

test_that("slider ranges come from the effect of each term on the curve", {
  # on [1000, 1001] a full swing of the slope changes the rise by 5 times the
  # cost range, not by 5 times the slope at the far bound
  r <- compute_slider_range(1, 1, power = 1, lb = 1000, ub = 1001)
  expect_equal(c(r$min, r$max), c(-4, 6))
  # a large coefficient that nearly cancels gets a narrow range around it
  co <- .designer_cubic_coefs(100, 110, 1)
  r <- compute_slider_range(co[3], 1, power = 2, lb = 100, ub = 110)
  hw <- (r$max - r$min) / 2
  expect_lt(hw * (110^2 - 100^2), 60)
  expect_gt(hw * (110^2 - 100^2), 5)
  # a narrow range around a large value is shown in the unit of its width
  expect_equal(.slider_scale(list(min = 0.5 - 1e-10, max = 0.5 + 1e-10)), 1e10)
  # even a cancelling coefficient far from 0 keeps a range a slider can step
  co <- .designer_cubic_coefs(1e4, 1e4 + 1, 1)
  r <- compute_slider_range(co[1], 1, power = 0, lb = 1e4, ub = 1e4 + 1)
  expect_gte((r$max - r$min) / r$step, 10)
  # the variation of x^p over the bounds
  expect_equal(.power_variation(0, 2, 3), 1)
  expect_equal(.power_variation(2, 2, 3), 5)
  expect_equal(.power_variation(2, -3, -2), 5)
  expect_equal(.power_variation(3, -2, 3), 35)
  expect_equal(.power_variation(2, -2, 3), 9)
  expect_equal(.power_variation(1, 1000, 1001), 1)
  # an overflowing variation still gives a finite range
  r <- compute_slider_range(0, 1, power = 4, lb = 0, ub = 1e100)
  expect_true(is.finite(r$min) && is.finite(r$max) && r$min < r$max)
})

test_that("on [100, 110] one step of every cubic slider changes the cost little", {
  p <- .build_cost_parts("A", 1, "cubic", 100, 110)
  for (i in 0:4) {
    r <- designer_row(p$ui, paste0("coef_1_", i))
    expect_lt(r$step / r$scale * 110^i, 0.2)
  }
})

test_that("a curve that is not accurate on its bounds is flagged", {
  # the designer cubic on narrow bounds far from 0 nearly cancels
  v <- .cost_validity(.designer_cubic_coefs(1000, 1001, 1), 1000, 1001)
  expect_false(v$accurate)
  # a fit through points on [1e4, 1e4 + 1] misses its points
  xs <- seq(1e4, 1e4 + 1, length.out = 5)
  ys <- c(10, 11, 12.5, 14, 20)
  fit <- solve(outer(xs - 1e4, 0:4, "^"), ys)
  # expand sum_k fit_k (x - 1e4)^k into powers of x, as the browser does
  co <- vapply(0:4, function(j) {
    k <- j:4
    sum(fit[k + 1] * choose(k, j) * (-1e4)^(k - j))
  }, numeric(1))
  v <- .cost_validity(co, 1e4, 1e4 + 1)
  expect_true(v$non_decreasing)
  expect_false(v$accurate)
  # ordinary curves are accurate
  expect_true(.cost_validity(.designer_cubic_coefs(10, 20, 1), 10, 20)$accurate)
  expect_true(.cost_validity(c(0, 1), 1000, 1001)$accurate)
  expect_true(.cost_validity(c(5, 0, 0, 0, 0), 0, 10)$accurate)
  expect_true(.cost_validity(c(0, 0), 0, 10)$accurate)
})

test_that("the designer warns when the curve is not accurate", {
  testthat::local_mocked_bindings(runApp = function(...) stop("no app"), .package = "shiny")
  p <- .build_cost_parts("A", 1, "cubic", 1000, 1001, embedded = TRUE)
  warn <- htmltools::tagQuery(p$ui)$find("#inaccurate_warning_1")$selectedTags()
  expect_length(warn, 1)
  expect_match(paste(unlist(warn[[1]]$children), collapse = ""), "Use the linear form instead")
  js <- character(0)
  local_mocked_bindings(
    show = function(id, ...) js <<- c(js, paste("show", id)),
    hide = function(id, ...) js <<- c(js, paste("hide", id)),
    .package = "shinyjs"
  )
  shiny::testServer(p$server, {
    session$setInputs(cost_type = "cubic")
    session$flushReact()
    expect_true("show inaccurate_warning_1" %in% js)
    js <<- character(0)
    session$setInputs(cost_type = "linear")
    session$flushReact()
    expect_true("hide inaccurate_warning_1" %in% js)
  })
})

test_that("a deep link with more than 10 components is refused with a note", {
  d <- list(
    component_names = "A", unit_costs = 1, default_cost_fxn_type = "linear",
    intervention_lower_bounds = 0, intervention_upper_bounds = 5
  )
  n <- 11
  q <- paste0(
    paste0("components=c", seq_len(n), collapse = "&"),
    "&lower=", paste(rep(0, n), collapse = ","),
    "&upper=", paste(rep(1, n), collapse = ","),
    "&costs=", paste(rep(1, n), collapse = ",")
  )
  v <- .parse_cost_query(q, d)
  expect_equal(v[names(d)], d)
  expect_match(v$notice, "11 components")
  expect_match(v$notice, "at most 10")
  # the designer shows the note
  p <- do.call(.build_cost_parts, v)
  note <- htmltools::tagQuery(p$ui)$find("#designer_notice")$selectedTags()
  expect_length(note, 1)
  expect_match(paste(unlist(note[[1]]$children), collapse = ""), "11 components")
  # a link with 10 components is still read
  q10 <- paste0(
    paste0("components=c", 1:10, collapse = "&"),
    "&lower=", paste(rep(0, 10), collapse = ","),
    "&upper=", paste(rep(1, 10), collapse = ","),
    "&costs=", paste(rep(1, 10), collapse = ",")
  )
  v10 <- .parse_cost_query(q10, d)
  expect_length(v10$component_names, 10)
  expect_null(v10$notice)
  # no note without one
  p <- .build_cost_parts("A", 1, "linear", 0, 5)
  expect_length(htmltools::tagQuery(p$ui)$find("#designer_notice")$selectedTags(), 0)
})

test_that("the cubic start is never negative on bounds below 0", {
  for (b in list(c(-10, -5), c(-3, 4), c(0, 2), c(2, 7))) {
    co <- .designer_cubic_coefs(b[1], b[2], 2)
    x <- seq(b[1], b[2], length.out = 50)
    cost <- vapply(x, function(v) sum(co * v^(0:4)), numeric(1))
    expect_gte(min(cost), -1e-9)
    # it rises by the cost range over the bounds
    expect_equal(cost[50] - cost[1], 2 * (b[2] - b[1]), tolerance = 1e-9)
  }
  # on bounds below 0 it starts at a cost of 0
  co <- .designer_cubic_coefs(-10, -5, 1)
  expect_equal(sum(co * (-10)^(0:4)), 0, tolerance = 1e-9)
  # on bounds from 0 up it keeps the linear cost's endpoints
  co <- .designer_cubic_coefs(2, 7, 1)
  expect_equal(sum(co * 2^(0:4)), 2)
  expect_equal(sum(co * 7^(0:4)), 7)
})
