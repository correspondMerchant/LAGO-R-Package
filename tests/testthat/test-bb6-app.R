# Fixes to lago_app() from the sixth bug bash.

bb6_quiet <- function(expr) suppressWarnings(suppressMessages(expr))
bb6_file <- function(...) {
  path <- tempfile(fileext = ".csv")
  writeBin(c(...), path)
  path
}
# the arguments a result stores, for a call on mtcars
bb6_args <- function(...) {
  a <- list(
    data = datasets::mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 20),
    unit_costs = c(2, 1), default_cost_fxn_type = "linear",
    outcome_goal = 25, outcome_goal_intention = "maximize", link = "identity",
    include_confidence_set = FALSE, confidence_set_alpha = 0.05
  )
  # each argument replaces its default whole, as modifyList() would merge the
  # columns of a new data frame into the old one
  extra <- list(...)
  a[names(extra)] <- extra
  a
}
bb6_spec <- function(...) {
  s <- .app_defaults(datasets::mtcars, "mtcars")
  s$components <- c("gear", "qsec")
  s$lower <- c(3, 14)
  s$upper <- c(5, 20)
  utils::modifyList(s, list(...))
}

test_that("text periods with month names or dates are in time order", {
  expect_identical(.app_periods(paste(month.abb, 2021))$labels, paste(month.abb, 2021))
  expect_identical(.app_periods(c("Jan 2021", "Dec 2020", "Feb 2021"))$labels, c("Dec 2020", "Jan 2021", "Feb 2021"))
  expect_identical(.app_periods(c("Mar-21", "Jan-21", "Feb-21"))$labels, c("Jan-21", "Feb-21", "Mar-21"))
  expect_identical(.app_periods(c("15 Jan 2021", "3 January 2021", "1 Dec 2020"))$labels, c("1 Dec 2020", "3 January 2021", "15 Jan 2021"))
  expect_identical(.app_periods(c("Mar", "jan", "Dec"))$labels, c("jan", "Mar", "Dec"))
  # a month name with no year in some labels leaves the order to the numbers
  expect_identical(.app_periods(c("May", "June 2"))$labels, c("May", "June 2"))
  # dates in the one order every label reads in
  expect_identical(.app_periods(c("15/01/2021", "01/02/2021", "20/02/2021"))$labels, c("15/01/2021", "01/02/2021", "20/02/2021"))
  expect_identical(.app_periods(c("02/01/2021", "01/15/2021"))$labels, c("01/15/2021", "02/01/2021"))
  expect_identical(.app_periods(c("2021-02-01", "2021-01-15"))$labels, c("2021-01-15", "2021-02-01"))
  expect_identical(.app_periods(c("2021.12.01", "2021.2.1"))$labels, c("2021.2.1", "2021.12.01"))
  # dates that read as days or months, and no dates, keep the number order
  expect_null(.app_label_dates(c("03/01/2021", "01/02/2021", "02/01/2021")))
  expect_identical(.app_periods(c("03/01/2021", "01/02/2021", "02/01/2021"))$labels, c("01/02/2021", "02/01/2021", "03/01/2021"))
  expect_null(.app_label_dates(c("31/02/2021", "01/01/2021")))
  expect_null(.app_label_dates(c("01/02/21", "03/04/21")))
  expect_null(.app_label_dates(c("Q1 2021", "2021-01-01")))
  expect_null(.app_label_dates(character(0)))
  # a label with no number comes first
  expect_identical(.app_periods(c("follow-up 1", "baseline", "follow-up 2"))$labels, c("baseline", "follow-up 1", "follow-up 2"))
})

test_that("the default period of month labels is the latest month", {
  set.seed(1)
  d <- data.frame(a = runif(48, 0, 10), b = runif(48, 0, 5), mon = rep(paste(month.abb, 2021), each = 4))
  d$y <- rbinom(48, 1, 0.5)
  spec <- utils::modifyList(.app_defaults(d, "d"), list(time_on = TRUE, period_col = "mon"))
  expect_identical(.app_choices(d, spec)$period, "Dec 2021")
})

test_that("a logical outcome column is offered as a binary outcome", {
  d <- data.frame(y = rep(c(TRUE, FALSE), 10), a = 1:20, b = (20:1) / 2, empty = NA)
  expect_true(.app_is_binary(d$y))
  expect_identical(.app_outcome_cols(d), c("y", "a", "b"))
  s <- .app_defaults(d, "d")
  expect_identical(c(s$outcome, s$outcome_type), c("y", "binary"))
  expect_identical(s$components, c("a", "b"))
  expect_true("y" %in% .app_choices(d, s)$outcomes)
  expect_false(any(grepl("at least two numeric columns", .app_problems(s, d)$message)))
  # one logical column and one numeric one are enough, a logical one alone is not
  expect_false(any(grepl("at least two numeric columns", .app_problems(s, d[c("y", "a")])$message)))
  expect_true(any(grepl("at least two numeric columns", .app_problems(s, d["y"])$message)))
  expect_true(any(grepl("at least two numeric columns", .app_problems(s, data.frame(y = d$y, z = !d$y))$message)))
  # with no 0/1 column the first numeric one is the outcome
  expect_identical(.app_defaults(data.frame(u = c(1.5, 2), v = 1:2, w = 3:4), "d")$outcome, "u")
})

test_that("a file with a NUL byte is refused with a clear message", {
  nul <- bb6_file(charToRaw("y,a,b\n1,2,3\n0,4"), as.raw(0), charToRaw("4,5\n1,6,7\n"))
  plain <- bb6_file(charToRaw("y,a,b\n1,2,3\n0,4,5\n"))
  expect_true(.app_has_nul(nul))
  expect_false(.app_has_nul(plain))
  expect_error(.app_read_csv(nul), "NUL byte")
  expect_identical(nrow(.app_read_csv(plain)), 2L)
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "upload", sep = ",", dec = ".", file = list(datapath = nul, name = "nul.csv"))
    expect_match(upload_error(), "The file has a NUL byte")
    expect_null(current_data())
  })
})

test_that("the decimal mark hint reads numbers with a power of ten", {
  d <- data.frame(a = c("2,5e-3", "1,5", "3"), b = 1:3, stringsAsFactors = FALSE)
  expect_match(.app_csv_hint(d, ",", "."), "decimal comma. Choose Comma")
  d2 <- data.frame(a = c("2.5E+3", "1.5", "3e2"), b = 1:3, stringsAsFactors = FALSE)
  expect_match(.app_csv_hint(d2, ";", ","), "decimal point. Choose Point")
})

test_that("a period time in another time zone opens on its period, and an unknown period is named", {
  d <- datasets::mtcars
  d$period <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + 86400 * ((seq_len(32) - 1) %% 4)
  a <- bb6_args(data = d, include_time_effects = TRUE, time_effect_optimization_value = as.POSIXct("2024-01-03 07:00:00", tz = "America/New_York"))
  x <- .app_spec_from_args(a)
  expect_identical(x$spec$period_value, "2024-01-03 12:00:00")
  expect_length(x$left, 0)
  # a column with no time zone is read in the local zone
  d$period <- as.POSIXct(format(d$period, tz = "UTC"))
  attr(d$period, "tzone") <- NULL
  a <- bb6_args(data = d, include_time_effects = TRUE, time_effect_optimization_value = as.POSIXct("2024-01-03 12:00:00"))
  expect_identical(.app_spec_from_args(a)$spec$period_value, "2024-01-03 12:00:00")
  # a period the data does not have is named in what is left out
  a$time_effect_optimization_value <- "2030-01-01"
  x <- .app_spec_from_args(a)
  expect_match(x$left, "the period 2030-01-01, which this data does not have, so the latest period with rows the model can use is used", fixed = TRUE)
  # a center the data does not have is named, and the average center is used
  d$center <- rep(c("a", "b"), 16)
  a <- bb6_args(data = d, include_center_effects = TRUE, center_effects_optimization_values = "z")
  x <- .app_spec_from_args(a)
  expect_identical(x$spec$center_for, "")
  expect_match(x$left, "the center z to optimize for")
})

test_that("weights that equal a component open as a column the app offers", {
  d <- datasets::mtcars
  d$wcopy <- d$gear
  x <- .app_spec_from_args(bb6_args(data = d, weights = d$gear))
  expect_identical(x$spec$weights_col, "wcopy")
  expect_true("wcopy" %in% .app_choices(d, x$spec)$weights)
  expect_length(x$left, 0)
  # with no other such column the weights are named in what is left out
  x <- .app_spec_from_args(bb6_args(weights = datasets::mtcars$qsec))
  expect_identical(x$spec$weights_col, "")
  expect_match(x$left, "the weights, which are not a column of the data that the app offers as weights")
})

test_that("the weights of a center level result are left out, as the package does not use them", {
  p <- as.data.frame(main_pulesa_data)
  p$proportion <- p$Success / p$Total_visit
  p$center_sample_size <- p$Total_visit
  p$center <- p$Clinic
  a <- bb6_args(
    data = p, input_data_structure = "center_level", outcome_name = "proportion", outcome_type = "binary",
    intervention_components = c("AccessMedicines", "AccessBPMachines"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(8, 2), outcome_goal = 0.8, link = "default", weights = p$Total_visit
  )
  x <- .app_spec_from_args(a)
  expect_identical(x$spec$weights_col, "")
  expect_length(x$left, 0)
  expect_false(any(grepl("weights", .app_problems(x$spec, p)$message)))
})

test_that("an ICC of a result opens with all its digits", {
  sent <- NULL
  local_mocked_bindings(
    updateTextInput = function(session, inputId, label = NULL, value = NULL, ...) if (identical(inputId, "icc")) sent <<- value,
    .package = "shiny"
  )
  start <- .app_spec_from_args(bb6_args(
    outcome_name = "am", outcome_type = "binary", outcome_goal = 0.6, power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 20, power_goal_approach = "unconditional", icc = c(control = 1 / 7, treatment = 1 / 30)
  ))
  shiny::testServer(.build_lago_app(data = datasets::mtcars, data_name = "mtcars", start = start), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
  })
  expect_equal(.app_parse_icc(sent), c(1 / 7, 1 / 30), tolerance = 1e-14)
})

test_that("center weights are kept, checked and loaded by the names of their centers", {
  d <- as.data.frame(BB_data)
  d$center <- d$site_name
  centers <- .app_center_levels(d, "center")
  w <- seq_along(centers) / sum(seq_along(centers))
  a <- bb6_args(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary", outcome_goal = 0.8, link = "default",
    intervention_components = "coaching_updt", intervention_lower_bounds = 0, intervention_upper_bounds = 40, unit_costs = 1,
    include_center_effects = TRUE, center_weights_for_outcome_goal = w
  )
  s <- .app_spec_from_args(a)$spec
  expect_identical(names(s$cw), centers)
  # the package gets the weights in the order of the centers
  expect_identical(.app_build(s, d)$args$center_weights_for_outcome_goal, w)
  # weights for other centers are named, though there are as many
  s2 <- s
  names(s2$cw) <- paste0(centers, "x")
  expect_match(.app_spec_misfits(s2, d), "the center weights, which are for other centers than this data has")
  expect_length(.app_spec_misfits(s, d), 0)
  # weights for a number of centers the data does not have keep no names
  a$center_weights_for_outcome_goal <- c(0.5, 0.5)
  x <- .app_spec_from_args(a)
  expect_null(names(x$spec$cw))
  expect_match(x$left, "for 2 centers while this data has")
  skip_on_cran()
  shiny::testServer(.build_lago_app(data = d, data_name = "d"), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    # weights saved in another order of the centers are loaded by name
    saved <- utils::modifyList(s, list(cw = rev(s$cw)))
    saved$cw <- rev(s$cw)
    f <- tempfile(fileext = ".rds")
    saveRDS(list(lago_app_settings = 1L, data_name = "d", spec = saved), f)
    session$setInputs(load_settings = list(datapath = f, name = "s.rds"))
    session$setInputs(center_mode = "effects", center_col = "center", center_for = "", cw_on = TRUE)
    html <- as.character(output$cw_ui$html)
    expect_match(html, "weight in the settings")
    first <- regmatches(html, regexpr("value=\"[^\"]*\"", html))
    expect_identical(first, sprintf("value=\"%s\"", format(w[1], digits = 15)))
    # the weights in the app are named by their centers
    expect_identical(names(spec()$cw), centers)
    # weights saved for other centers start at the shares of the patients
    other <- saved
    names(other$cw) <- paste0(names(other$cw), "x")
    saveRDS(list(lago_app_settings = 1L, data_name = "d", spec = other), f)
    session$setInputs(load_settings = list(datapath = f, name = "s2.rds"))
    session$setInputs(center_mode = "effects", center_col = "center", center_for = "", cw_on = TRUE)
    expect_match(as.character(output$cw_ui$html), "Each center starts at its share of the patients")
    # weights saved without names, as by older versions, are read in order
    old <- saved
    old$cw <- unname(s$cw)
    saveRDS(list(lago_app_settings = 1L, data_name = "d", spec = old), f)
    session$setInputs(load_settings = list(datapath = f, name = "s3.rds"))
    session$setInputs(center_mode = "effects", center_col = "center", center_for = "", cw_on = TRUE)
    html <- as.character(output$cw_ui$html)
    expect_match(html, "weight in the settings")
    expect_identical(regmatches(html, regexpr("value=\"[^\"]*\"", html)), sprintf("value=\"%s\"", format(w[1], digits = 15)))
  })
})

test_that("a period column with another role, and a covariate that is a center column, are refused", {
  msgs <- function(...) .app_problems(bb6_spec(...), datasets::mtcars)$message
  role <- "cannot also be the outcome, a component, a covariate, a center characteristic or the center column"
  expect_true(any(grepl(role, msgs(time_on = TRUE, period_col = "gear", period_value = "4"))))
  expect_true(any(grepl(role, msgs(time_on = TRUE, period_col = "mpg", period_value = "21"))))
  expect_true(any(grepl(role, msgs(time_on = TRUE, period_col = "cyl", period_value = "4", covariates = "cyl"))))
  expect_true(any(grepl(role, msgs(time_on = TRUE, period_col = "cyl", period_value = "4", center_mode = "chars", center_chars = "cyl", center_char_values = 4))))
  expect_true(any(grepl(role, msgs(time_on = TRUE, period_col = "cyl", period_value = "4", center_mode = "effects", center_col = "cyl"))))
  expect_false(any(grepl(role, msgs(time_on = TRUE, period_col = "cyl", period_value = "4"))))
  cov <- "The covariate(s) %s cannot also be the center column or a center characteristic"
  expect_true(any(grepl(sprintf(cov, "carb"), msgs(covariates = c("wt", "carb"), center_mode = "effects", center_col = "carb"), fixed = TRUE)))
  expect_true(any(grepl(sprintf(cov, "hp"), msgs(covariates = "hp", center_mode = "chars", center_chars = "hp", center_char_values = 100), fixed = TRUE)))
  expect_false(any(grepl("cannot also be the center column", msgs(covariates = "hp", center_mode = "none", center_col = "hp"))))
})

test_that("a cost multiplier sweep of unit costs scales them without a cost list", {
  expect_identical(.app_sweep_extra("cost_multiplier", bb6_spec(cost_mode = "unit"), list(cost_list_of_vectors = list(1))), list())
  expect_false(any(grepl("cost_list_of_vectors", .app_sweep_code("cost_multiplier", c(0.5, 1), bb6_spec(cost_mode = "unit")))))
  expect_true(any(grepl("power_goal_cluster_id = \"cyl\"", .app_sweep_code("icc", c(0, 0.1), bb6_spec(cluster_col = "cyl")), fixed = TRUE)))
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "mtcars", sep = ",", dec = ".")
    session$setInputs(
      outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 22, intention = "maximize",
      components = c("wt", "qsec"), cost_mode = "unit", cost_type = "cubic", center_mode = "none",
      include_cs = FALSE, approach = "unconditional"
    )
    ids <- c(col_id("lb", "wt"), col_id("ub", "wt"), col_id("cost", "wt"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))
    do.call(session$setInputs, stats::setNames(list(1.5, 5, 3.7, 14, 20, 1.3), ids))
    do.call(session$setInputs, stats::setNames(list(3, 17), c(col_id("dose", "wt"), col_id("dose", "qsec"))))
    session$setInputs(run = 1)
    session$setInputs(sweep_param = "cost_multiplier", sweep_how = "typed", sweep_typed = "0.5, 1, 2")
    session$setInputs(run_sweep = 1)
    st <- sweep_state()$run
    expect_null(st$error)
    expect_false(any(grepl("unit_cost_doses", st$warnings)))
    r <- current_result()
    old <- bb6_quiet(lago_sensitivity(r, parameter = "cost_multiplier", values = c(0.5, 1, 2), cost_list_of_vectors = r$cost_list_of_vectors))
    expect_equal(st$value$rec_int_cost, old$rec_int_cost)
    expect_equal(st$value$value, old$value)
    expect_equal(st$value$rec_int_cost[2], r$rec_int_cost)
  })
})

test_that("the cost designer opens on cost coefficients of any length up to 5", {
  spec <- bb6_spec(cost_mode = "coef")
  spec$coefs <- list(2, c(0, 1, 0.5, 0.1))
  st <- .app_designer_start(spec)
  expect_identical(st$form, "cubic")
  expect_identical(st$coefs, list(c(2, 0, 0, 0, 0), c(0, 1, 0.5, 0.1, 0)))
  expect_length(st$unfit, 0)
  expect_identical(st$by_form$linear, list(c(2, 0), NULL))
  # a cubic of degree 1 opens in the linear form
  spec$coefs <- list(2, c(0, 1))
  st <- .app_designer_start(spec)
  expect_identical(st$form, "linear")
  expect_identical(st$coefs, list(c(2, 0), c(0, 1)))
  spec$coefs <- list(c(0, 1, 0), c(1, 2))
  st <- .app_designer_start(spec)
  expect_identical(st$form, "cubic")
  expect_identical(st$by_form$linear, list(c(0, 1), c(1, 2)))
  # more than 5 coefficients, or none, fit no form
  spec$coefs <- list(c(0, 1, 0, 0, 0, 1), c(0, 2))
  st <- .app_designer_start(spec)
  expect_identical(st$form, "cubic")
  expect_identical(st$unfit, "gear")
  expect_null(st$by_form$cubic[[1]])
  spec$coefs <- list(NULL, c(0, 2))
  st <- .app_designer_start(spec)
  expect_identical(st$form, "linear")
  expect_identical(st$unfit, "gear")
})
