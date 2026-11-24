# Fixes to lago_app() from the fifth bug bash.

bb5_quiet <- function(expr) suppressWarnings(suppressMessages(expr))
bb5_in_c_locale <- function(f) {
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")))) skip("no C locale")
  f()
}
bb5_file <- function(...) {
  path <- tempfile(fileext = ".csv")
  writeBin(c(...), path)
  path
}
bb5_text <- function(s) bb5_file(charToRaw(enc2utf8(s)))
# the arguments a result stores, for a call on mtcars
bb5_args <- function(...) {
  a <- list(
    data = datasets::mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 20),
    unit_costs = c(2, 1), default_cost_fxn_type = "linear",
    outcome_goal = 25, outcome_goal_intention = "maximize", link = "identity",
    include_confidence_set = FALSE, confidence_set_alpha = 0.05
  )
  utils::modifyList(a, list(...))
}
bb5_mtcars <- function(session, col_id, goal = 25, intention = "maximize") {
  session$setInputs(source = "mtcars", sep = ",", dec = ".")
  session$setInputs(
    outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = goal,
    intention = intention, components = c("gear", "qsec"), cost_mode = "unit",
    cost_type = "linear", center_mode = "none", include_cs = FALSE, approach = "unconditional"
  )
  do.call(session$setInputs, stats::setNames(
    list(3, 5, 2, 14, 20, 1),
    c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))
  ))
}

test_that("a result with named per component inputs in another order opens on the same settings", {
  skip_on_cran()
  r <- bb5_quiet(lago_optimization(
    data = datasets::mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(qsec = 14, gear = 3), intervention_upper_bounds = c(qsec = 20, gear = 5),
    cost_list_of_vectors = list(qsec = c(4, 6), gear = c(0, 4)),
    outcome_goal = 25, include_confidence_set = FALSE, quiet = TRUE
  ))
  s <- .app_spec_from_args(attr(r, "lago_call_args"))$spec
  expect_identical(s$lower, c(3, 14))
  expect_identical(s$upper, c(5, 20))
  expect_identical(s$coefs, list(c(0, 4), c(4, 6)))
  b <- .app_build(s, datasets::mtcars)
  again <- bb5_quiet(do.call(lago_optimization, c(list(data = b$data), b$args, list(quiet = TRUE))))
  expect_equal(again$rec_int, r$rec_int)
  expect_equal(again$rec_int_cost, r$rec_int_cost)
})

test_that("every named per component input of a call is put in the order of the components", {
  a <- bb5_args(
    unit_costs = c(qsec = 6, gear = 1), default_cost_fxn_type = "cubic", unit_cost_doses = c(qsec = 15, gear = 4),
    optimization_method = "grid_search", optimization_grid_search_step_size = c(qsec = 0.5, gear = 1),
    include_confidence_set = TRUE, confidence_set_grid_step_size = c(qsec = 0.25, gear = 0.5),
    prev_recommended_interventions = c(qsec = 16, gear = 4),
    center_characteristics = c("wt", "hp"), center_characteristics_optimization_values = c(hp = 100, wt = 3)
  )
  s <- .app_spec_from_args(a)$spec
  expect_identical(s$unit_costs, c(1, 6))
  expect_identical(s$doses, c(4, 15))
  expect_identical(s$grid_steps, c(1, 0.5))
  expect_identical(s$cs_steps, c(0.5, 0.25))
  expect_identical(s$prev, c(4, 16))
  expect_identical(s$center_char_values, c(3, 100))
  # values without names, or with names that are not the components, keep
  # their order, as the package reads them by position
  expect_identical(.app_by_name(c(5, 6), c("gear", "qsec")), c(5, 6))
  expect_identical(.app_by_name(c(a = 5, b = 6), c("gear", "qsec")), c(5, 6))
  expect_identical(.app_by_name(array(c(qsec = 6, gear = 5)), c("gear", "qsec")), c(6, 5))
  expect_identical(.app_by_name(array(c(6, 5), dimnames = list(c("qsec", "gear"))), c("gear", "qsec")), c(5, 6))
})

test_that("a named icc pair of a result is shown in control, treatment order", {
  base <- list(power_goal = 0.8, num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 30, power_goal_approach = "unconditional")
  s <- .app_spec_from_args(do.call(bb5_args, c(base, list(icc = c(treatment = 0.01, control = 0.3)))))$spec
  expect_identical(s$icc, c(0.3, 0.01))
  expect_identical(.app_parse_icc(paste(format(s$icc, trim = TRUE), collapse = ", ")), c(0.3, 0.01))
  expect_identical(.app_spec_from_args(do.call(bb5_args, c(base, list(icc = c(0.2, 0.1)))))$spec$icc, c(0.2, 0.1))
  expect_identical(.app_spec_from_args(do.call(bb5_args, c(base, list(icc = c(control = 0.05)))))$spec$icc, 0.05)
  expect_identical(.app_spec_from_args(do.call(bb5_args, base))$spec$icc, numeric(0))
})

test_that("a numeric period of a result names the period whose label is that number", {
  d <- as.data.frame(BB_data)
  d$period <- factor(sprintf("%02d", d$pilot_phase))
  a <- list(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 5), unit_costs = c(1, 8),
    default_cost_fxn_type = "linear", outcome_goal = 0.85, outcome_goal_intention = "maximize", link = "logit",
    include_confidence_set = FALSE, confidence_set_alpha = 0.05, include_time_effects = TRUE,
    time_effect_optimization_value = 2
  )
  s <- .app_spec_from_args(a)$spec
  expect_identical(s$period_value, "02")
  expect_identical(.app_choices(d, s)$period, "02")
  expect_identical(.app_build(s, d)$args$time_effect_optimization_value, 2)
  # a period typed as its number is no problem, and is not said to be missing
  s$period_value <- "4"
  expect_false(any(grepl("period", .app_problems(s, d)$message %||% character(0))))
  expect_length(.app_spec_misfits(s, d), 0)
  expect_identical(.app_choices(d, s)$period, "04")
  s$period_value <- "3"
  expect_match(.app_spec_misfits(s, d), "the period 3")
  expect_true(any(grepl("The period 3 has no row", .app_problems(s, d)$message)))
  # text periods are matched by their label, or by its number
  d$when <- ifelse(d$pilot_phase == 4, "late", sprintf("%02d", d$pilot_phase))
  s$period_col <- "when"
  s$period_value <- "late"
  expect_identical(.app_build(s, d)$args$time_effect_optimization_value, 3L)
  s$period_value <- "2"
  expect_identical(.app_build(s, d)$args$time_effect_optimization_value, 2L)
  # a value not stored as a number is kept as its text
  a$time_effect_optimization_value <- "04"
  expect_identical(.app_spec_from_args(a)$spec$period_value, "04")
  expect_identical(.app_period_label(NA, c("1", "2")), NA_character_)
  expect_identical(.app_period_label("x", c("1", "2")), "x")
  expect_identical(.app_period_label("2", c("2.0", "02")), "2")
})

test_that("uploaded names of letters with no ASCII form get readable, unique and valid names", {
  expect_identical(.app_translit("abc"), "abc")
  local_mocked_bindings(.app_translit = function(x) rep("?", length(x)))
  for (h in list(c("\u4e2d", "\u4e2d"), c("\u4e2d", "\u4e2d", "\u4e2d"), c("\u4e2d\u6587", "y"), c("\u4e2d", "X1"))) {
    got <- .app_csv_names(h)
    expect_false(anyDuplicated(got) > 0)
    expect_identical(make.names(got), got)
    expect_false(any(.app_unreadable_name(got)))
  }
  expect_identical(.app_csv_names(c("\u4e2d", "\u4e2d")), c("X1", "X2"))
  expect_identical(.app_csv_names(c("\u4e2d", "X1")), c("X1.1", "X1"))
  expect_identical(.app_csv_names(c("\u4e2dif", "y")), c(".if", "y"))
  local_mocked_bindings(.app_translit = function(x) "zhong")
  expect_identical(.app_csv_names(c("\u4e2d", "y")), c("zhong", "y"))
  expect_true(.app_unreadable_name("..1") && .app_unreadable_name("...") && .app_unreadable_name("."))
  expect_false(.app_unreadable_name(".if"))
})

test_that("columns named ., ..1 or ... are refused before the run", {
  d <- datasets::mtcars
  names(d)[names(d) == "wt"] <- "."
  names(d)[names(d) == "hp"] <- "..1"
  s <- .app_defaults(d, "d")
  s$outcome <- "mpg"
  s$outcome_type <- "continuous"
  s$components <- c("gear", "qsec")
  s <- .app_set_component_defaults(s, d)
  s$include_cs <- FALSE
  s$covariates <- c(".", "..1")
  msg <- paste(.app_problems(s, d)$message, collapse = " ")
  expect_match(msg, "'\\.', '\\.\\.1' are not valid R names here, or are names R reserves.*'X\\.', 'X\\.\\.1'")
})

test_that("the thousands hint needs a first group that is not 0 and says when the mark is unclear", {
  hint <- function(...) .app_csv_hint(data.frame(dose = c(...), y = 1, stringsAsFactors = FALSE), ";", ".")
  h <- hint("0,125", "0,250")
  expect_match(h, "decimal comma. Choose Comma as the decimal mark.$")
  expect_false(grepl("thousands", h))
  h <- hint("1,250", "2,500")
  expect_match(h, "decimal comma.*If the comma is between the thousands instead")
  h <- .app_csv_hint(data.frame(dose = c("1.250", "2.500"), y = 1, stringsAsFactors = FALSE), ";", ",")
  expect_match(h, "decimal point.*If the point is between the thousands instead")
  expect_match(hint("1,250,000", "2,500"), "comma between the thousands, such as 1,250,000")
  expect_match(hint("1,250.5", "2"), "comma between the thousands")
  expect_match(.app_csv_hint(data.frame(dose = c("0.125", "0.25"), y = 1), ";", ","), "decimal point. Choose Point as the decimal mark.$")
})

test_that("a blank or byte order mark only file is empty, and UTF-32 text is named as such", {
  blank <- bb5_text("\n \n\n")
  bom <- bb5_file(as.raw(c(0xEF, 0xBB, 0xBF)))
  u32 <- bb5_file(iconv("a,b\n1,2\n", "UTF-8", "UTF-32LE", toRaw = TRUE)[[1]])
  u32bom <- bb5_file(as.raw(c(0xFF, 0xFE, 0, 0)), iconv("a\n1\n", "UTF-8", "UTF-32LE", toRaw = TRUE)[[1]])
  u32be <- bb5_file(as.raw(c(0, 0, 0xFE, 0xFF)), iconv("a\n1\n", "UTF-8", "UTF-32BE", toRaw = TRUE)[[1]])
  u16 <- bb5_file(iconv("a,b\n1,2\n", "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]])
  for (f in c(u32, u32bom, u32be)) expect_identical(.app_file_encoding(f), "UTF-32")
  expect_identical(.app_file_encoding(u16), "UTF-16")
  expect_error(.app_read_csv(u32), "UTF-32 text")
  expect_true(.app_csv_blank(blank))
  expect_true(.app_csv_blank(bom))
  expect_true(.app_csv_blank(bb5_file(raw(0))))
  expect_false(.app_csv_blank(u16))
  expect_false(.app_csv_blank(bb5_text("a,b\n1,2\n")))
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    for (f in list(list(blank, "The file is empty"), list(bom, "The file is empty"), list(u32, "saved as UTF-32 text"))) {
      session$setInputs(source = "upload", sep = ",", dec = ".", file = list(datapath = f[[1]], name = "f.csv"))
      expect_match(upload_error(), f[[2]])
    }
  })
})

test_that("without a UTF-8 locale a header that keeps its name keeps it", {
  bb5_in_c_locale(function() {
    expect_false(isTRUE(l10n_info()[["UTF-8"]]))
    expect_identical(.app_make_names(c("\u00e9", "e"), utf8_locales = "nope"), c("e.1", "e"))
    expect_identical(.app_make_names(c("e", "\u00e9", "\u00e9"), utf8_locales = "nope"), c("e", "e.1", "e.2"))
  })
})

test_that("a failed run leaves no result for Done", {
  skip_on_cran()
  returned <- "unset"
  local_mocked_bindings(stopApp = function(returnValue = invisible()) returned <<- returnValue, .package = "shiny")
  shiny::testServer(.build_lago_app(), {
    bb5_mtcars(session, col_id)
    session$setInputs(run = 1)
    expect_s3_class(shared$result$result, "lago")
    session$setInputs(goal = 26)
    with_mocked_bindings(session$setInputs(run = 2), lago_optimization = function(...) stop("boom"))
    expect_match(run_state()$run$error, "boom")
    session$setInputs(done = 1)
    expect_null(returned$result)
    expect_null(returned$code)
  })
})

test_that("the budget and the goal from a budget keep ten digits on the safe side", {
  x <- 10027.0422272345
  up <- .app_round_safe(x, "up")
  expect_gte(up, x)
  expect_lt(up - x, 1e-5)
  down <- .app_round_safe(1024.99716412345, "down")
  expect_lte(down, 1024.99716412345)
  expect_gt(down, 1024.99716412345 - 1e-6)
})

test_that("a new dataset or a recommendation that costs nothing starts the budget again", {
  skip_on_cran()
  set_budget <- list()
  typed <- character(0)
  real_num <- shiny::updateNumericInput
  real_text <- shiny::updateTextInput
  local_mocked_bindings(
    updateNumericInput = function(session, inputId, label = NULL, value = NULL, ...) {
      if (identical(inputId, "budget")) set_budget[[length(set_budget) + 1L]] <<- value
      real_num(session, inputId, label = label, value = value, ...)
    },
    updateTextInput = function(session, inputId, label = NULL, value = NULL, ...) {
      if (identical(inputId, "sweep_typed")) typed <<- c(typed, value)
      real_text(session, inputId, label = label, value = value, ...)
    },
    .package = "shiny"
  )
  csv <- tempfile(fileext = ".csv")
  d <- datasets::mtcars
  d$mpg <- d$mpg + 100
  utils::write.csv(d, csv, row.names = FALSE)
  shiny::testServer(.build_lago_app(), {
    bb5_mtcars(session, col_id)
    session$setInputs(run = 1)
    session$setInputs(budget = 30, budget_grid = 4)
    session$setInputs(sweep_param = "outcome_goal", sweep_how = "typed", sweep_typed = "24, 26")
    # a budget typed for this data is kept by the next run on it
    session$setInputs(goal = 26)
    session$setInputs(run = 2)
    expect_identical(input$budget, 30)
    n_typed <- length(typed)
    session$setInputs(source = "upload", file = list(datapath = csv, name = "d.csv"))
    # the typed sweep values were for the other data
    expect_identical(typed[length(typed)], "")
    expect_gt(length(typed), n_typed)
    bb5_mtcars_upload <- function() {
      session$setInputs(
        outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 125,
        intention = "maximize", components = c("gear", "qsec"), cost_mode = "unit",
        cost_type = "linear", center_mode = "none", include_cs = FALSE, approach = "unconditional"
      )
      do.call(session$setInputs, stats::setNames(
        list(3, 5, 200, 14, 20, 100),
        c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))
      ))
    }
    bb5_mtcars_upload()
    session$setInputs(run = 3)
    cost <- run_state()$run$value$rec_int_cost
    expect_identical(budget_prefill(), .app_round_safe(cost, "up"))
    expect_identical(set_budget[[length(set_budget)]], budget_prefill())
    # the browser shows the budget it was sent
    session$setInputs(budget = budget_prefill())
    # a recommendation that costs nothing leaves the budget empty
    session$setInputs(intention = "minimize", goal = 130)
    do.call(session$setInputs, stats::setNames(list(0, 0), c(col_id("lb", "gear"), col_id("lb", "qsec"))))
    session$setInputs(run = 4)
    expect_identical(run_state()$run$value$rec_int_cost, 0)
    expect_identical(budget_prefill(), NA_real_)
    expect_identical(set_budget[[length(set_budget)]], NA_real_)
  })
})

test_that("the cost designer keeps the costs in use when its form is switched", {
  spec <- .app_empty_spec()
  spec$components <- c("gear", "qsec")
  spec$lower <- c(3, 14)
  spec$upper <- c(5, 20)
  spec$cost_mode <- "coef"
  spec$coefs <- list(c(1, 2), c(0, 1, 0, 0, 0))
  st <- .app_designer_start(spec)
  expect_identical(st$by_form$linear, list(c(1, 2), c(0, 1)))
  expect_identical(st$by_form$cubic, list(c(1, 2, 0, 0, 0), c(0, 1, 0, 0, 0)))
  spec$coefs <- list(c(0, 1, 0, 0, 0.5), c(0, 1, 1), c(0, 1, NA, 0, 0))
  st <- .app_designer_start(spec)
  expect_identical(st$by_form$linear, list(NULL, NULL, NULL))
  expect_identical(st$by_form$cubic, list(c(0, 1, 0, 0, 0.5), NULL, c(0, 1, NA, 0, 0)))
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    bb5_mtcars(session, col_id)
    in_use <- cost_fxn_calculator(c(3, 14), c(5, 20), c(2, 1), "linear")
    session$setInputs(open_designer = 1)
    session$setInputs(`designer_1-cost_type` = "cubic")
    session$setInputs(`designer_1-apply` = 1)
    expect_equal(unname(designer_costs()), lapply(in_use, function(v) c(v, 0, 0, 0)))
  })
})
