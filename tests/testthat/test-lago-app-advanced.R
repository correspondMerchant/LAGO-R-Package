# lago_app(): the advanced settings of the outcome model, the optimizer, the
# confidence set and the previous stage. The app itself is never launched here.

advanced_data <- function() {
  d <- datasets::mtcars
  d$share <- d$mpg / 40
  d$n <- rep(c(1, 2), 16)
  d$neg <- c(-1, rep(1, 31))
  d$label <- letters[(seq_len(32) - 1) %% 4 + 1]
  d
}

advanced_spec <- function(...) {
  s <- .app_defaults(advanced_data(), "d")
  s$outcome <- "mpg"
  s$outcome_type <- "continuous"
  s$goal <- 25
  s$components <- c("gear", "qsec")
  s$lower <- c(3, 14)
  s$upper <- c(5, 20)
  s$unit_costs <- c(2, 1)
  s$include_cs <- FALSE
  utils::modifyList(s, list(...))
}

test_that("the advanced settings leave the package defaults alone until they are changed", {
  b <- .app_build(advanced_spec(), advanced_data())
  expect_false(any(c(
    "glm_family", "link", "weights", "optimization_method", "optimization_grid_search_step_size",
    "confidence_set_alpha", "prev_recommended_interventions"
  ) %in% names(b$args)))
  # a binary outcome never gets a family, as the package has only one for it
  bin <- .app_build(advanced_spec(outcome = "am", outcome_type = "binary", goal = 0.5, glm_family = "quasibinomial"), advanced_data())
  expect_null(bin$args$glm_family)
})

test_that("each advanced setting reaches the arguments and the code", {
  d <- advanced_data()
  s <- advanced_spec(
    outcome = "share", goal = 0.6, glm_family = "quasibinomial", link = "logit", weights_col = "n",
    method = "grid_search", grid_steps = c(0.5, NA), include_cs = TRUE, cs_level = 0.9, cs_steps = c(NA, 1),
    prev_on = TRUE, prev = c(4, 15)
  )
  b <- .app_build(s, d)
  expect_identical(b$args$glm_family, "quasibinomial")
  expect_identical(b$args$link, "logit")
  expect_identical(b$args$weights, d$n)
  expect_identical(b$args$optimization_method, "grid_search")
  # an empty step takes 1/20 of the range
  expect_equal(b$args$optimization_grid_search_step_size, c(0.5, 0.3))
  expect_equal(b$args$confidence_set_grid_step_size, c(0.1, 1))
  expect_equal(b$args$confidence_set_alpha, 0.1)
  expect_equal(b$args$prev_recommended_interventions, c(4, 15))
  code <- paste(b$code, collapse = "\n")
  expect_match(code, "weights = data[[\"n\"]]", fixed = TRUE)
  expect_match(code, "confidence_set_alpha = 0.1", fixed = TRUE)
  expect_match(code, "optimization_method = \"grid_search\"", fixed = TRUE)
  expect_false(grepl("c(1, 2, 1, 2", code, fixed = TRUE))
  expect_identical(.app_steps(c(1, 2, 3), c(0, 0), c(1, 1)), c(0.05, 0.05))
})

test_that("each advanced setting that cannot run says what to change", {
  d <- advanced_data()
  msgs <- function(...) paste(.app_problems(advanced_spec(...), d)$message, collapse = "\n")
  expect_match(msgs(glm_family = "quasibinomial"), "proportion model needs an outcome between 0 and 1")
  expect_false(grepl("proportion model", msgs(outcome = "share", goal = 0.6, glm_family = "quasibinomial")))
  expect_match(msgs(link = "logit"), "logit link needs an outcome between 0 and 1, and 'mpg' goes outside it")
  expect_false(grepl("logit link", msgs(outcome = "share", goal = 0.6, link = "logit")))
  expect_match(msgs(outcome = "share", goal = 60, glm_family = "quasibinomial"), "must be between 0 and 1, such as 0.6 for 60%")
  expect_match(msgs(outcome = "share", goal = 60, link = "logit"), "such as 0.6 for 60%")
  expect_false(grepl("0.6 for 60%", msgs(goal = 60)))
  expect_match(msgs(weights_col = "label"), "weights column must be numeric")
  expect_match(msgs(weights_col = "gear"), "cannot also be the outcome or a component")
  expect_match(msgs(weights_col = "neg"), "must be finite numbers of 0 or more, with at least one above 0")
  d$zero <- 0
  d$inf <- c(Inf, rep(1, 31))
  expect_match(paste(.app_problems(advanced_spec(weights_col = "zero"), d)$message, collapse = "\n"), "at least one above 0")
  expect_match(paste(.app_problems(advanced_spec(weights_col = "inf"), d)$message, collapse = "\n"), "must be finite numbers")
  expect_match(msgs(method = "grid_search", grid_steps = c(0, NA)), "grid search step of gear must be above 0")
  expect_match(msgs(include_cs = TRUE, cs_steps = c(NA, -1)), "confidence set step of qsec must be above 0")
  expect_match(msgs(method = "grid_search", grid_steps = c(1e-4, 1e-4)), "grid search would try")
  expect_match(msgs(include_cs = TRUE, cs_steps = c(1e-4, 1e-4)), "confidence set grid would have")
  expect_match(msgs(prev_on = TRUE, prev = c(4, NA)), "previous stage's level of qsec, or turn off")
  expect_match(msgs(prev_on = TRUE, prev = c(9, 15)), "previous stage's level of gear \\(9\\) must be within its bounds")
  expect_equal(nrow(.app_problems(advanced_spec(prev_on = TRUE, prev = c(4, 15), method = "grid_search", grid_steps = c(0.5, 1)), d)), 0)
})

test_that("the code reproduces a run with the advanced settings", {
  skip_on_cran()
  d <- advanced_data()
  s <- advanced_spec(
    outcome = "share", goal = 0.6, glm_family = "quasibinomial", weights_col = "n",
    method = "grid_search", grid_steps = c(0.5, 0.5), prev_on = TRUE, prev = c(4, 15)
  )
  b <- .app_build(s, d)
  run <- suppressWarnings(suppressMessages(do.call(lago_optimization, c(list(data = b$data), b$args, list(quiet = TRUE)))))
  expect_identical(run$family, "quasibinomial")
  env <- new.env()
  assign("d", d, envir = env)
  suppressWarnings(suppressMessages(utils::capture.output(eval(parse(text = b$code), envir = env))))
  expect_equal(env$result$rec_int, run$rec_int, tolerance = 1e-5)
})

test_that("the server reads the advanced settings and fills the previous stage", {
  sent <- list()
  local_mocked_bindings(
    updateNumericInput = function(session, inputId, label = NULL, value = NULL, ...) sent[[length(sent) + 1]] <<- list(id = inputId, value = value),
    .package = "shiny"
  )
  shiny::testServer(.build_lago_app(data = advanced_data(), data_name = "d"), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    session$setInputs(
      outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 25, intention = "maximize",
      components = c("gear", "qsec"), cost_mode = "unit", cost_type = "linear", center_mode = "none",
      include_cs = TRUE, approach = "unconditional"
    )
    do.call(session$setInputs, stats::setNames(list(3, 5, 2, 14, 20, 1), c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))))
    session$setInputs(glm_family = "default", link = "identity", weights_col = "n", method = "grid_search", cs_level = "0.99", prev_on = TRUE)
    do.call(session$setInputs, stats::setNames(list(0.5, 1, 4, 15), c(col_id("gstep", "gear"), col_id("csstep", "qsec"), col_id("prev", "gear"), col_id("prev", "qsec"))))
    s <- spec()
    expect_identical(s$link, "identity")
    expect_identical(s$weights_col, "n")
    expect_identical(s$method, "grid_search")
    expect_equal(s$cs_level, 0.99)
    expect_equal(s$grid_steps, c(0.5, NA))
    expect_equal(s$cs_steps, c(NA, 1))
    expect_equal(s$prev, c(4, 15))
    expect_match(as.character(output$grid_steps_ui$html), "Grid search step of gear")
    expect_match(as.character(output$cs_steps_note$html), "Leave a step empty for 1/20 of the range: gear 0.1, qsec 0.3")
    expect_match(as.character(output$prev_ui$html), "Previous stage level of qsec")
    expect_match(paste(code_lines(), collapse = "\n"), "confidence_set_alpha = 0.01", fixed = TRUE)
    # without a result there is nothing to fill in
    session$setInputs(use_rec_as_prev = 1)
    expect_length(Filter(function(m) grepl("^prev_", m$id), sent), 0)
    session$setInputs(components = character(0))
    expect_match(as.character(output$prev_ui$html), "Choose at least one component")
    expect_null(output$cs_steps_note$html)
  })
})

test_that("the current recommendation fills the previous stage", {
  skip_on_cran()
  sent <- list()
  local_mocked_bindings(
    updateNumericInput = function(session, inputId, label = NULL, value = NULL, ...) sent[[length(sent) + 1]] <<- list(id = inputId, value = value),
    .package = "shiny"
  )
  shiny::testServer(.build_lago_app(data = advanced_data(), data_name = "d"), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    session$setInputs(
      outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 25, intention = "maximize",
      components = c("gear", "qsec"), cost_mode = "unit", cost_type = "linear", center_mode = "none",
      include_cs = FALSE, approach = "unconditional"
    )
    do.call(session$setInputs, stats::setNames(list(3, 5, 2, 14, 20, 1), c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))))
    session$setInputs(include_cs = TRUE, cs_level = "0.9", run = 1)
    r <- run_state()$run$value
    # the interval is labelled with the level it was computed at
    expect_match(as.character(output$results$html), "90% CI")
    session$setInputs(use_rec_as_prev = 1)
    filled <- Filter(function(m) grepl("^prev_", m$id), sent)
    expect_length(filled, 2)
    expect_equal(vapply(filled, `[[`, numeric(1), "value"), signif(as.numeric(r$rec_int), 6))
    # a result for other components is not used, even with as many of them
    session$setInputs(components = c("gear", "carb"))
    do.call(session$setInputs, stats::setNames(list(3, 5, 2, 1, 8, 1), c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "carb"), col_id("ub", "carb"), col_id("cost", "carb"))))
    session$setInputs(use_rec_as_prev = 2)
    expect_length(Filter(function(m) grepl("^prev_", m$id), sent), 2)
  })
})
