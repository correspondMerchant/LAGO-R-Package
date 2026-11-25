# Bug bash 5 fixes in the presentation layer: the outcome goal rounded like
# the estimate, rounding in one step, the checks on the arguments and the title
# of lago_report(), and the precision of the report data.

bb5_skip_report <- function() {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

bb5_fit <- function(...) {
  args <- list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 23),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 25,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1, 1), quiet = TRUE
  )
  args[names(list(...))] <- list(...)
  suppressWarnings(suppressMessages(do.call(lago_optimization, args)))
}

bb5_html <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("the outcome goal is rounded like the estimate, so a met goal does not look missed", {
  r <- bb5_fit(include_confidence_set = FALSE)
  r$outcome_goal <- 0.85049
  r$est_outcome_goal <- 0.850491
  b <- lago_blocks(r)
  expect_identical(b$outcome$rows[1], "Estimated outcome: 0.85")
  expect_identical(b$outcome$rows[3], "Outcome goal: 0.85")
  expect_true("Outcome goal: 0.85 (maximize: at least this value)" %in% b$inputs$rows)
  # a nonzero goal closer to 0 than 1e-6 does not show as 0
  r$outcome_goal <- 5e-7
  b <- lago_blocks(r)
  expect_identical(b$outcome$rows[3], "Outcome goal: 5e-7")
  expect_true("Outcome goal: 5e-7 (maximize: at least this value)" %in% b$inputs$rows)
  # the effective goal of a power goal uses the same rule
  r$power_goal <- 0.8
  r$effective_outcome_goal <- 0.85049
  b <- lago_blocks(r)
  expect_true(
    "Effective outcome goal (max of outcome goal and power-implied outcome): 0.85" %in% b$inputs$rows
  )
})

test_that("values are rounded in one step from their 17 digits, not rounded twice", {
  v <- c(404461827.45464969, -2555838594.7868457)
  expect_identical(.lago_num_text(.lago_fmt_value(v)), c("404461827.4546", "-2555838594.7868"))
  expect_identical(.lago_num_text(.lago_fmt_interval(v)), c("404461827.455", "-2555838594.787"))
  # a value whose 15 digits read back as the same double still rounds a cut
  # digit of 5 up, as it is typed
  expect_identical(.lago_num_text(.lago_fmt_value(c(2.30495, 1.00005))), c("2.305", "1.0001"))
  # 15 digits at most are kept, and a 5 in the 16th digit rounds up
  expect_identical(.lago_round_dec(1.0000000000000049, 15, sig = TRUE), 1)
  expect_identical(.lago_round_dec(0.12345678901234567, 20, sig = TRUE), 0.123456789012346)
  p <- .lago_dec_digits(c(2.30495, 404461827.45464969))
  expect_identical(p$cut_digits, c("23049500000000000", "40446182745464969"))
  expect_identical(p$cut_exp, c(0L, 8L))
})

test_that("lago_report() refuses argument names that are only the start of a render() argument", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  x <- bb5_fit(include_confidence_set = FALSE)
  out <- tempfile(fileext = ".html")
  for (arg in c("run_p", "output_form", "runt", "output_opt", "qui", "output_")) {
    dots <- stats::setNames(list("x"), arg)
    expect_error(
      do.call(lago_report, c(list(x, output_file = out), dots)),
      "only the start of an argument name",
      fixed = TRUE
    )
  }
  expect_error(
    lago_report(x, output_file = out, run_p = FALSE, qui = TRUE),
    "only the start of argument names",
    fixed = TRUE
  )
  expect_false(file.exists(out))
})

test_that("the report data holds 17 significant digits, the double R formats", {
  bb5_skip_report()
  x <- bb5_fit(include_confidence_set = FALSE)
  x$rec_int_cost <- 404461827.45464969
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  lago_report(x, output_file = out)
  html <- bb5_html(out)
  expect_true(grepl('"rec_int_cost":404461827.45464969', html, fixed = TRUE))
  # a cost of 1e4 or more shows 2 decimals (bug bash 6)
  expect_true(grepl("Cost: 404461827.45", html, fixed = TRUE))
})

test_that("Unicode noncharacters of a report title become entities", {
  for (code in c(64976L, 65007L, 65534L, 65535L, 131070L, 131071L, 1114110L, 1114111L)) {
    expect_identical(.lago_report_title_yaml(paste0("a", intToUtf8(code), "b")), paste0("a&#", code, ";b"))
  }
  # the code points next to them stay as they are
  for (code in c(64975L, 65008L, 65533L, 65536L)) {
    ch <- intToUtf8(code)
    expect_identical(.lago_report_title_yaml(paste0("a", ch, "b")), paste0("a", ch, "b"))
  }
})

test_that("a report title with U+FFFE and U+FFFF renders", {
  bb5_skip_report()
  x <- bb5_fit(include_confidence_set = FALSE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  lago_report(x, output_file = out, title = "a\uFFFEb\uFFFFc")
  expect_true(file.exists(out))
  expect_true(grepl("<h1 class=\"title", bb5_html(out), fixed = TRUE))
})

test_that("a report title that is not UTF-8 gets a message that names the title", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  x <- bb5_fit(include_confidence_set = FALSE)
  out <- tempfile(fileext = ".html")
  expect_error(lago_report(x, output_file = out, title = "\xe9t\xe9"), "`title` is not valid UTF-8 text", fixed = TRUE)
  expect_error(lago_report(x, output_file = out, title = c("ok", "a\xed\xa0\x80b")), "`title` is not valid UTF-8 text", fixed = TRUE)
  expect_false(file.exists(out))
})

test_that("a report title marked as latin1 is converted to UTF-8", {
  bb5_skip_report()
  x <- bb5_fit(include_confidence_set = FALSE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  lat <- "\xe9t\xe9"
  Encoding(lat) <- "latin1"
  lago_report(x, output_file = out, title = lat)
  expect_true(grepl("\u00e9t\u00e9</h1>", bb5_html(out), fixed = TRUE))
})
