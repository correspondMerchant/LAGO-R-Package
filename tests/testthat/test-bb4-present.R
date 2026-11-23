# Bug bash 4 fixes in the presentation layer: rounding that agrees with the JS
# of the report, the estimate inside its printed interval, p-values, the size
# of the confidence set, the confidence label, the reason there is no overall
# test, and the checks on the arguments and the title of lago_report().

bb4_skip_report <- function() {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

bb4_fit <- function(...) {
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

bb4_print <- function(x) {
  utils::capture.output(lines <- cli::cli_fmt(print(x)))
  gsub("\\s+", " ", paste(lines, collapse = " "))
}

bb4_binary_data <- function() {
  set.seed(1)
  n <- 600
  d <- data.frame(center = rep(paste0("c", 1:6), each = 100))
  d$a <- stats::runif(n, 0, 4)
  d$b <- stats::runif(n, 0, 3)
  d$y <- stats::rbinom(n, 1, stats::plogis(-1 + 0.3 * d$a + 0.4 * d$b))
  d
}

bb4_binary_fit <- function(data) {
  suppressWarnings(suppressMessages(lago_optimization(
    data = data, outcome_name = "y", outcome_type = "binary",
    intervention_components = c("a", "b"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(4, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 2)), outcome_goal = 0.6,
    include_confidence_set = FALSE, quiet = TRUE
  )))
}

# The same values and the same expected text as tests/js/test-report-math.js,
# so R and the JS of the report write each value the same way. A cut digit of
# 5 in the decimal digits rounds up, away from 0.
bb4_ties <- data.frame(
  v = c(2.30495, 3487.45, 0.10005, 12.3455, 60.5405, 5784.9499999999998, -2.30495, 0.0123455, 1.00005, 0.6789),
  value = c("2.305", "3487.45", "0.1001", "12.3455", "60.5405", "5784.95", "-2.305", "0.01235", "1.0001", "0.6789"),
  cost = c("2.305", "3487.5", "0.10005", "12.346", "60.541", "5785", "-2.305", "0.012346", "1.0001", "0.6789"),
  interval = c("2.305", "3487.45", "0.1", "12.346", "60.541", "5784.95", "-2.305", "0.01235", "1", "0.679"),
  stringsAsFactors = FALSE
)

test_that("values with a cut digit of 5 round the same way as in the JS of the report", {
  expect_identical(.lago_num_text(.lago_fmt_value(bb4_ties$v)), bb4_ties$value)
  expect_identical(.lago_num_text(.lago_fmt_cost(bb4_ties$v)), bb4_ties$cost)
  expect_identical(.lago_num_text(.lago_fmt_interval(bb4_ties$v)), bb4_ties$interval)
  # the values stay numbers for callers that format them further
  expect_identical(.lago_fmt_value(c(0.10005, NA, Inf, 0, -5e-7)), c(0.1001, NA, Inf, 0, 0))
  expect_identical(.lago_fmt_cost_range(c(0.125, 2.675, 0, -0.0012345)), c(0.125, 2.68, 0, -0.00123))
  expect_identical(.lago_round_dec(c(0.004, 0.005, 0.0049), 2, sig = FALSE), c(0, 0.01, 0))
})

test_that("large and small numbers are written without floating point noise, as JS writes them", {
  expect_identical(
    .lago_num_text(c(1e16 + 2, 123456789012.34567, 1e25, 1.5e-7, 0.000001, -0, 1.5e21, -123456.5, NA, Inf)),
    c("10000000000000000", "123456789012.346", "1e+25", "1.5e-7", "0.000001", "0", "1.5e+21", "-123456.5", NA, "Inf")
  )
  # an exact tie in the 16th digit is settled to even, as jsonlite writes it in
  # the report data and as the JS decDigits() reads it
  expect_identical(
    .lago_num_text(c(100000000000000.5, 100000000000001.5, 999999999999999.5)),
    c("100000000000000", "100000000000002", "1000000000000000")
  )
  expect_identical(.lago_num_text(numeric(0)), character(0))
})

test_that("the estimate is printed with the rounding of its interval, never outside it", {
  r <- bb4_fit()
  r$est_outcome_goal <- 0.6788999952
  r$est_outcome_ci <- c(lower = 0.679, upper = 0.679)
  rows <- lago_blocks(r)$outcome$rows
  expect_identical(rows[1], "Estimated outcome: 0.679")
  expect_identical(rows[2], "95% CI for the estimated outcome: 0.679 - 0.679")
  r$est_outcome_goal <- 0.0123455
  r$est_outcome_ci <- c(lower = 0.01, upper = 0.01469)
  rows <- lago_blocks(r)$outcome$rows
  expect_identical(rows[1:2], c("Estimated outcome: 0.01235", "95% CI for the estimated outcome: 0.01 - 0.01469"))
})

test_that("p-values are written without R e notation, and a missing test value says so", {
  expect_identical(.lago_fmt_pval(c(1e-5, 1.234e-10, 0.03, 0, NA)), c("0.00001", "1.234e-10", "0.03", "< 2.2e-16", NA))
  r <- bb4_fit(include_confidence_set = FALSE)
  r$test_results <- list(test_stat = 2.5, p_val = 1e-5)
  expect_identical(lago_blocks(r)$test$rows, c("Test statistic: 2.5", "P-value: 0.00001"))
  r$test_results <- list(test_stat = 2.5)
  expect_identical(lago_blocks(r)$test$rows, c("Test statistic: 2.5", "P-value: not available"))
  r$test_results <- list(test_stat = NA_real_, p_val = NA_real_)
  expect_identical(lago_blocks(r)$test$rows, c("Test statistic: not available", "P-value: not available"))
})

test_that("the size of the confidence set shows 0.01% at 0.005% and not 100% for a set short of the grid", {
  r <- bb4_fit()
  size_line <- function(s) {
    r$confidence_set_size_percentage <- s
    lago_blocks(r)$confidence_set$rows[1]
  }
  expect_identical(size_line(0.00005), "95% confidence set size: 0.01% of the grid")
  expect_identical(size_line(0.00004), "95% confidence set size: < 0.01% of the grid")
  expect_identical(size_line(0.99996), "95% confidence set size: > 99.99% of the grid")
  expect_identical(size_line(1), "95% confidence set size: 100% of the grid")
  expect_identical(size_line(0), "95% confidence set size: 0% of the grid")
})

test_that("a group column that the test could not use gives the reason, not a request for the column", {
  d <- bb4_binary_data()
  ask <- "include a 'group' column in the data"
  expect_match(bb4_print(bb4_binary_fit(d)), ask, fixed = TRUE)
  d$group <- "treatment"
  out <- bb4_print(bb4_binary_fit(d))
  expect_match(out, "each arm of the 'group' column needs at least two patients", fixed = TRUE)
  expect_no_match(out, ask, fixed = TRUE)
  d$group <- ifelse(d$a > 1, "Treatment", "Control")
  r <- bb4_binary_fit(d)
  out <- bb4_print(r)
  expect_match(out, "the 'group' column has values other than 'treatment' and 'control'", fixed = TRUE)
  expect_no_match(out, ask, fixed = TRUE)
  # an object that did not record its data asks for the column
  attr(r, "lago_call_args") <- NULL
  expect_match(bb4_print(r), ask, fixed = TRUE)
})

test_that("the recap shows interaction components without backticks", {
  r <- bb4_fit(include_confidence_set = FALSE)
  r$intervention_components <- c("gear", "qsec", "`gear:qsec`")
  rows <- lago_blocks(r)$inputs$rows
  expect_true("3 intervention component(s): gear, qsec, gear:qsec" %in% rows)
})

test_that("a missing outcome_goal field does not pick up outcome_goal_intention", {
  r <- bb4_fit(include_confidence_set = FALSE)
  r$outcome_goal <- NULL
  expect_false(is.null(r$outcome_goal_intention))
  blocks <- lago_blocks(r)
  expect_true("Outcome goal: not specified" %in% blocks$inputs$rows)
  expect_false(any(grepl("^Outcome goal", blocks$outcome$rows)))
})

test_that("the confidence label of an alpha close to 1 is not 0%", {
  lab <- function(a) .lago_confidence_label(structure(list(), lago_call_args = list(confidence_set_alpha = a)))
  expect_identical(lab(0.99999), "0.001%")
  expect_identical(lab(0.75), "25%")
  expect_identical(lab(0.5), "50%")
  expect_identical(lab(1 / 3), "66.67%")
  expect_identical(lab(1e-12), "99.9999999999%")
})

test_that("a huge cost coefficient shows no made up digits", {
  expect_identical(.lago_fmt_cost_coefs(c(0, -1.23456e-8, 1e25, 123456.7, -5, NA)), "0, -1.2346e-8, 1e+25, 123460, -5, NA")
})

test_that("every control character of a report title becomes an entity", {
  codes <- c(1:9, 11:12, 14:31, 127:159, 8232L, 8233L)
  for (code in codes) {
    expect_identical(.lago_report_title_yaml(paste0("a", intToUtf8(code), "b")), paste0("a&#", code, ";b"))
  }
  # a line break stays the YAML escape it was
  expect_identical(.lago_report_title_yaml("one\r\ntwo\rthree"), "one\\ntwo\\nthree")
})

test_that("a report title with control characters renders", {
  bb4_skip_report()
  x <- bb4_fit(include_confidence_set = FALSE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  lago_report(x, output_file = out, title = paste0("a", intToUtf8(1), "b\u0085c\td"))
  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl(paste0("a", intToUtf8(1), "b\u0085c\td</h1>"), html, fixed = TRUE))
})

test_that("lago_report() refuses arguments that change the kind of output", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  x <- bb4_fit(include_confidence_set = FALSE)
  out <- tempfile(fileext = ".html")
  for (arg in c("output_format", "output_options", "output_yaml", "run_pandoc", "runtime")) {
    dots <- stats::setNames(list("x"), arg)
    expect_error(do.call(lago_report, c(list(x, output_file = out), dots)), "does not accept", fixed = TRUE)
  }
  expect_error(lago_report(x, out, "Title", FALSE, "extra"), "must be named", fixed = TRUE)
  expect_error(lago_report(x, output_file = out, quiet = NA), "must be `TRUE` or `FALSE`", fixed = TRUE)
  expect_false(file.exists(out))
})

test_that("lago_report() says what is wrong with the output path", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  x <- bb4_fit(include_confidence_set = FALSE)
  base <- tempfile("bb4_")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE))
  expect_error(lago_report(x, output_file = file.path(base, "nodir/")), "does not exist", fixed = TRUE)
  afile <- file.path(base, "afile")
  file.create(afile)
  expect_error(lago_report(x, output_file = file.path(afile, "x.html")), "is a file and not a folder", fixed = TRUE)
  expect_error(lago_report(x, output_file = file.path(base, "x.pdf")), "or '.htm'", fixed = TRUE)
})
