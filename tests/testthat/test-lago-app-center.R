# lago_app(): center level data, the PULESA example and the center weights.
# The app itself is never launched here.

center_pulesa <- function() as.data.frame(main_pulesa_data)

center_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("the PULESA example opens on the package's center level model and runs as it is", {
  d <- center_pulesa()
  s <- .app_defaults(d, "main_pulesa_data")
  expect_identical(s$structure, "center")
  expect_identical(c(s$cl_value, s$cl_total), c("Success", "Total_visit"))
  expect_identical(c(s$center_mode, s$center_col, s$period_col, s$period_value), c("effects", "Clinic", "Period", "13"))
  expect_false(s$include_cs)
  expect_equal(nrow(.app_problems(s, d)), 0)
  b <- .app_build(s, d)
  expect_identical(b$args$input_data_structure, "center_level")
  expect_identical(b$args$outcome_name, "proportion")
  expect_identical(b$args$outcome_type, "binary")
  expect_true(b$args$include_center_effects)
  expect_identical(b$prep[1:2], c(
    "data$proportion <- data[[\"Success\"]] / data[[\"Total_visit\"]]",
    "data$center_sample_size <- data[[\"Total_visit\"]]"
  ))
  expect_equal(b$data$proportion, d$Success / d$Total_visit)
  ch <- .app_choices(d, s)
  expect_identical(c(ch$center_col, ch$period_col, ch$period), c("Clinic", "Period", "13"))
})

test_that("the R code of the PULESA example reproduces its run", {
  skip_on_cran()
  d <- center_pulesa()
  b <- .app_build(.app_defaults(d, "main_pulesa_data"), d)
  run <- center_quiet(do.call(lago_optimization, c(list(data = b$data), b$args, list(quiet = TRUE))))
  expect_equal(run$est_outcome_goal, 0.8, tolerance = 1e-4)
  env <- new.env()
  assign("main_pulesa_data", d, envir = env)
  center_quiet(utils::capture.output(eval(parse(text = b$code), envir = env)))
  expect_equal(env$result$rec_int, run$rec_int, tolerance = 1e-5)
})

test_that("center level columns are built from successes or from a proportion", {
  d <- data.frame(s = c(2, 5), n = c(10, 20), proportion = c(0.2, 0.25), center_sample_size = c(10, 20), x = 1:2)
  spec <- utils::modifyList(.app_empty_spec(), list(structure = "center", cl_kind = "proportion", cl_value = "proportion", cl_total = "center_sample_size"))
  # columns that already have the names the package reads are used as they are
  expect_length(.app_center_columns(spec, d)$code, 0)
  spec$cl_total <- "n"
  out <- .app_center_columns(spec, d)
  expect_identical(out$code, "data$center_sample_size <- data[[\"n\"]]")
  spec$cl_kind <- "successes"
  spec$cl_value <- "s"
  out <- .app_center_columns(spec, d)
  expect_equal(out$data$proportion, c(0.2, 0.25))
  # nothing is built until both columns are chosen and numeric
  spec$cl_total <- ""
  expect_identical(.app_center_columns(spec, d)$data, d)
  expect_identical(.app_resolve(spec)[c("outcome", "outcome_type")], list(outcome = "proportion", outcome_type = "binary"))
  expect_identical(.app_resolve(.app_empty_spec())$outcome, "")
})

test_that("center level settings that cannot run say what to change", {
  d <- center_pulesa()
  d$text <- "a"
  d$neg <- -d$Total_visit
  d$share <- d$Success / d$Total_visit
  base <- .app_defaults(d, "main_pulesa_data")
  msgs <- function(...) paste(.app_problems(utils::modifyList(base, list(...)), d)$message, collapse = "\n")
  expect_match(msgs(cl_value = ""), "Choose the column with the successes of each row")
  expect_match(msgs(cl_kind = "proportion", cl_value = ""), "Choose the column with the proportion of each row")
  expect_match(msgs(cl_total = ""), "Choose the column with the total")
  expect_match(msgs(cl_value = "text"), "successes column 'text' must be numeric")
  expect_match(msgs(cl_total = "text"), "total column 'text' must be numeric")
  expect_match(msgs(cl_total = "neg"), "Every total in 'neg' must be a number above 0")
  expect_match(msgs(cl_value = "Total_visit", cl_total = "Success"), "at most the total of their row")
  expect_match(msgs(cl_kind = "proportion", cl_value = "Success"), "proportions in 'Success' must be between 0 and 1")
  expect_false(grepl("between 0 and 1", msgs(cl_kind = "proportion", cl_value = "share")))
  expect_match(msgs(components = c("Total_visit", "DeliveryA")), "cannot also be an intervention component")
  expect_match(msgs(weights_col = "Total_visit"), "does not take weights")
  # a proportion column is not 0/1 and that is fine
  expect_false(grepl("not 0/1", msgs()))
})

test_that("center level data without center effects says so to the package", {
  d <- center_pulesa()
  s <- utils::modifyList(.app_defaults(d, "main_pulesa_data"), list(center_mode = "none"))
  expect_false(.app_build(s, d)$args$include_center_effects)
  s$center_mode <- "chars"
  s$center_chars <- "MI_DeliveryB"
  s$center_char_values <- 0
  s$covariates <- character(0)
  expect_false(.app_build(s, d)$args$include_center_effects)
})

test_that("the app says when it replaces the proportion or total columns", {
  d <- center_pulesa()
  d$proportion <- 0.5
  d$center_sample_size <- 1
  notes <- .app_notes(.app_defaults(d, "main_pulesa_data"), d)
  expect_true(any(grepl("column named proportion. The app replaces it with one built from Success and Total_visit", notes)))
  expect_true(any(grepl("column named center_sample_size. The app replaces it with one built from Total_visit", notes)))
  s <- utils::modifyList(.app_defaults(d, "main_pulesa_data"), list(cl_kind = "proportion", cl_value = "Proportions"))
  expect_true(any(grepl("built from Proportions,", .app_notes(s, d))))
})

test_that("center weights follow the package's order of centers and start at each center's share", {
  d <- data.frame(site = factor(c("b", "a", "b", "b"), levels = c("c", "b", "a")), n = c(10, 30, 20, 40), y = 1)
  expect_identical(.app_center_levels(d, "site"), c("b", "a"))
  expect_identical(.app_center_levels(data.frame(k = c(10, 2, 10)), "k"), c("2", "10"))
  expect_identical(.app_center_levels(d, "none"), character(0))
  s <- utils::modifyList(.app_empty_spec(), list(center_col = "site"))
  expect_equal(unname(.app_center_weights_default(s, d)), c(0.75, 0.25))
  s$structure <- "center"
  s$cl_total <- "n"
  expect_equal(unname(.app_center_weights_default(s, d)), c(70, 30) / 100)
  d$n <- 0
  expect_equal(unname(.app_center_weights_default(s, d)), c(0.5, 0.5))
  expect_length(.app_center_weights_default(utils::modifyList(s, list(center_col = "none")), d), 0)
})

test_that("center weights reach the arguments and are checked", {
  d <- center_pulesa()
  base <- .app_defaults(d, "main_pulesa_data")
  w <- unname(.app_center_weights_default(base, d))
  s <- utils::modifyList(base, list(cw_on = TRUE, cw = w))
  expect_equal(nrow(.app_problems(s, d)), 0)
  expect_equal(.app_build(s, d)$args$center_weights_for_outcome_goal, w)
  # one center to plan for replaces the weights
  s$center_for <- "Mengo Hospital"
  b <- .app_build(s, d)
  expect_null(b$args$center_weights_for_outcome_goal)
  expect_identical(b$args$center_effects_optimization_values, "Mengo Hospital")
  msgs <- function(...) paste(.app_problems(utils::modifyList(base, list(cw_on = TRUE, ...)), d)$message, collapse = "\n")
  expect_match(msgs(cw = w[-1]), "Enter a weight of 0 or more for every center")
  expect_match(msgs(cw = c(-0.1, w[-1] + 0.1 / 15)), "Enter a weight of 0 or more for every center")
  expect_match(msgs(cw = w * 2), "The center weights add up to 2. They must add up to 1")
  # a center with no complete row cannot get a weight
  d$DeliveryA[d$Clinic == "Mengo Hospital"] <- NA
  expect_match(
    paste(.app_problems(utils::modifyList(base, list(cw_on = TRUE, cw = w)), d)$message, collapse = "\n"),
    "The center Mengo Hospital has no row where every model column is observed, so its weight must be 0"
  )
})

test_that("the server reads center level data and the center weights", {
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "main_pulesa_data", sep = ",", dec = ".")
    d <- current_data()
    expect_identical(nrow(d), 192L)
    comps <- .app_presets()$main_pulesa_data$components
    session$setInputs(
      structure = "center", cl_kind = "successes", cl_value = "Success", cl_total = "Total_visit",
      outcome = "Success", outcome_type = "continuous", goal_mode = "outcome", goal = 0.8, intention = "maximize",
      components = comps, cost_mode = "unit", cost_type = "linear", include_cs = FALSE, approach = "unconditional",
      center_mode = "effects", center_col = "Clinic", center_for = "", time_on = TRUE, period_col = "Period", period_value = "13",
      covariates = c("MI_DeliveryB", "MI_RemoteMonitoring", "MI_PerformanceImprovement"), cw_on = TRUE
    )
    s <- spec()
    # center level data always has a binary outcome, read from the built proportion
    expect_identical(c(s$outcome, s$outcome_type), c("proportion", "binary"))
    html <- as.character(output$cw_ui$html)
    expect_match(html, "Butabika NRH")
    expect_match(html, "Each center starts at its share of the patients")
    w <- unname(.app_center_weights_default(s, d))
    do.call(session$setInputs, stats::setNames(as.list(signif(w, 4)), vapply(seq_along(w), function(k) cw_id("Clinic", k), character(1))))
    expect_match(as.character(output$cw_sum$html), "The weights add up to 1.")
    expect_equal(spec()$cw, signif(w, 4))
  })
})
