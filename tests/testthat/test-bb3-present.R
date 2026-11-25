# Bug bash 3 fixes in the presentation layer: the report title, numbers in e
# notation, the size of a tiny confidence set, the outcome goal and its
# direction, and the checks on the output file and the title of lago_report().

bb3_skip_report <- function() {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

bb3_fit <- function(...) {
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

bb3_html <- function(x, ...) {
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  lago_report(x, output_file = out, ...)
  readLines(out, warn = FALSE)
}

# The text of the <title> element and of the <h1> title heading.
bb3_titles <- function(html) {
  one <- paste(html, collapse = "\n")
  page <- regmatches(one, regexpr("<title>[^<]*</title>", one))
  head <- regmatches(one, regexpr("<h1 class=\"title[^>]*>[^<]*</h1>", one))
  # pandoc may wrap a long heading over lines
  gsub("\\s+", " ", c(
    page = sub("^<title>(.*)</title>$", "\\1", page),
    h1 = sub("^<h1[^>]*>(.*)</h1>$", "\\1", head)
  ))
}

# A title as pandoc writes it in HTML text: & < > " and ' escaped.
bb3_html_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub("\"", "&quot;", s, fixed = TRUE)
  gsub("'", "&#39;", s, fixed = TRUE)
}

test_that("the report title escapes punctuation as entities, not backslashes", {
  expect_identical(.lago_report_title_yaml("Plain title 2"), "Plain title 2")
  expect_identical(.lago_report_title_yaml("a (b) [c]"), "a &#40;b&#41; &#91;c&#93;")
  expect_identical(.lago_report_title_yaml('"\\'), "&#34;&#92;")
  expect_identical(.lago_report_title_yaml("one\ntwo"), "one two")
  expect_identical(.lago_report_title_yaml("été"), "été")
})

test_that("every character of a report title shows literally in the heading and page title", {
  bb3_skip_report()
  r <- bb3_fit()
  titles <- c(
    "Cost (USD) and [notes]",
    "BetterBirth report (2024)",
    "\\(x\\) and \\[y\\]",
    "{a} {.b}",
    "$x^2$ cost $5",
    "[link](http://x) and <http://y>",
    'Trial "A" \'B\'',
    "C:\\data\\new *not bold* _u_ ~s~ ^p^",
    "<b>bold</b> & @ref # h --- ..."
  )
  for (t in titles) {
    html <- bb3_html(r, title = t)
    expect_identical(unname(bb3_titles(html)), rep(bb3_html_escape(t), 2), label = t)

  }
})

test_that("numbers in the text are written in full, as the JS report writes them", {
  expect_identical(
    .lago_num_text(c(100000, 2e6, 1e-5, 0.000012346, 1e21, 1e-7, 0, -0, NA, Inf, -1234.5)),
    c("100000", "2000000", "0.00001", "0.000012346", "1e+21", "1e-7", "0", "0", NA, "Inf", "-1234.5")
  )
  r <- bb3_fit(
    intervention_components = "qsec", intervention_lower_bounds = 14,
    intervention_upper_bounds = 23, cost_list_of_vectors = list(c(0, 100000)),
    confidence_set_grid_step_size = 1
  )
  b <- lago_blocks(r)
  expect_no_match(unlist(b), "e[+-][0-9]")
  # a cost of 1e4 or more shows 2 decimals (bug bash 6)
  expect_identical(b$cost$rows, paste0("Cost: ", format(round(r$rec_int_cost, 2), scientific = FALSE, nsmall = 0, digits = 15)))
  expect_match(b$confidence_set$rows[2], "IQR of the cost within the 95% confidence set: [0-9]+ - [0-9]+$")

  # bounds, the recommendation, the estimate and its interval, and the test
  big <- r
  big$intervention_lower_bounds <- 1e5
  big$intervention_upper_bounds <- 2e6
  big$rec_int <- 1e5
  big$est_outcome_goal <- 1e-5
  big$est_outcome_ci <- c(lower = 1e-5, upper = 3e5)
  big$test_results <- list(test_stat = 1e5, p_val = 0.5)
  b <- lago_blocks(big)
  expect_true("Intervention lower bounds: 100000" %in% b$inputs$rows)
  expect_true("Intervention upper bounds: 2000000" %in% b$inputs$rows)
  expect_identical(b$recommendation$rows$value, "100000")
  expect_identical(b$outcome$rows[1:2], c("Estimated outcome: 0.00001", "95% CI for the estimated outcome: 0.00001 - 300000"))
  expect_true("Test statistic: 100000" %in% b$test$rows)
  big$display_components <- NULL
  big$intervention_components <- NULL
  expect_identical(lago_blocks(big)$recommendation$rows, data.frame(value = "100000", stringsAsFactors = FALSE))
  big$power_goal <- 1e5
  big$effective_outcome_goal <- 2e5
  b <- lago_blocks(big)
  expect_true("Power goal: 100000" %in% b$inputs$rows)
  expect_identical(b$power$rows, "Power goal: 100000")
  expect_match(b$inputs$rows, "power-implied outcome\\): 200000$", all = FALSE)
})

test_that("a tiny confidence set does not show as 0% of the grid", {
  r <- bb3_fit()
  r$confidence_set_size_percentage <- 1e-6
  expect_match(lago_blocks(r)$confidence_set$rows[1], "size: < 0.01% of the grid", fixed = TRUE)
  r$confidence_set_size_percentage <- 0.0000501
  expect_match(lago_blocks(r)$confidence_set$rows[1], "size: 0.01% of the grid", fixed = TRUE)
  r$confidence_set_size_percentage <- 0.1234
  expect_match(lago_blocks(r)$confidence_set$rows[1], "size: 12.34% of the grid", fixed = TRUE)
  r$confidence_set_size_percentage <- 0
  r$cs <- NULL
  expect_match(lago_blocks(r)$confidence_set$rows[1], "size: 0% of the grid", fixed = TRUE)
})

test_that("the outcome goal is rounded like the estimate and shows its direction", {
  r <- bb3_fit(outcome_goal = 70 / 3)
  expect_identical(r$outcome_goal_intention, "maximize")
  b <- lago_blocks(r)
  expect_true("Outcome goal: 23.333 (maximize: at least this value)" %in% b$inputs$rows)
  expect_true("Outcome goal: 23.333" %in% b$outcome$rows)

  r <- bb3_fit(outcome_goal = 18, outcome_goal_intention = "minimize")
  expect_true("Outcome goal: 18 (minimize: at most this value)" %in% lago_blocks(r)$inputs$rows)

  # an object made before the direction was stored uses its recorded
  # arguments, and shows no direction without them
  r$outcome_goal_intention <- NULL
  expect_true("Outcome goal: 18 (minimize: at most this value)" %in% lago_blocks(r)$inputs$rows)
  attr(r, "lago_call_args") <- NULL
  expect_true("Outcome goal: 18" %in% lago_blocks(r)$inputs$rows)
  r$outcome_goal <- NULL
  expect_true("Outcome goal: not specified" %in% lago_blocks(r)$inputs$rows)
})

test_that("lago_report() refuses an output file that is not HTML", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  r <- bb3_fit()
  dir <- tempfile("bb3-report-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  rmd <- file.path(dir, "x.Rmd")
  writeLines("keep me", rmd)
  expect_error(lago_report(r, output_file = rmd), "must end in", fixed = TRUE)
  expect_identical(readLines(rmd), "keep me")
  expect_error(lago_report(r, output_file = file.path(dir, "rep.pdf")), "rep.pdf", fixed = TRUE)
  expect_error(lago_report(r, output_file = file.path(dir, "rep")), "must end in", fixed = TRUE)
  expect_false(file.exists(file.path(dir, "rep.pdf")))
})

test_that("lago_report() accepts .htm and .HTML, and uses the default title for an empty one", {
  bb3_skip_report()
  r <- bb3_fit()
  dir <- tempfile("bb3-report-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (f in c("a.htm", "b.HTML")) {
    out <- lago_report(r, output_file = file.path(dir, f))
    expect_true(file.exists(out))
  }
  default <- c(page = "LAGO optimization report", h1 = "LAGO optimization report")
  for (t in list("", NULL, NA, c("", " "))) {
    expect_identical(bb3_titles(bb3_html(r, title = t)), default)
  }
  expect_identical(bb3_titles(bb3_html(r, title = c("a", NA, "b")))[["page"]], "a b")
})
