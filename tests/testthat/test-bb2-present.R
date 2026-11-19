# Bug bash 2 fixes in the presentation layer: the report title, the confidence
# set section, the cost recap, p-values, the test message for a continuous
# outcome and the confidence label.

bb2_skip_report <- function() {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

bb2_fit <- function(...) {
  args <- list(
    data = BB_data,
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.85,
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(1, 1),
    quiet = TRUE
  )
  args[names(list(...))] <- list(...)
  suppressWarnings(suppressMessages(do.call(lago_optimization, args)))
}

bb2_print <- function(x) {
  utils::capture.output(lines <- cli::cli_fmt(print(x)))
  paste(lines, collapse = "\n")
}

bb2_report <- function(x, ...) {
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  lago_report(x, output_file = out, ...)
  paste(readLines(out, warn = FALSE), collapse = "\n")
}

test_that("a report title with quotes, backslashes or markdown shows literally", {
  expect_identical(.lago_report_title_yaml("Plain title"), "Plain title")
  expect_identical(.lago_report_title_yaml('a "b"'), 'a \\\\\\"b\\\\\\"')
  expect_identical(.lago_report_title_yaml("one\ntwo"), "one\\ntwo")
  bb2_skip_report()
  r <- bb2_fit()
  html <- bb2_report(r, title = 'Trial "A" report')
  expect_match(html, "<title>Trial &quot;A&quot; report</title>", fixed = TRUE)
  html <- bb2_report(r, title = "C:\\\\data *not bold* <b>")
  expect_match(html, "<title>C:\\\\data *not bold* &lt;b&gt;</title>", fixed = TRUE)
  expect_no_match(html, "<em>not bold</em>", fixed = TRUE)
})

test_that("the confidence set section explains a missing set in print and report", {
  r <- bb2_fit(include_confidence_set = FALSE)
  why <- "Not computed (set include_confidence_set = TRUE to compute it)."
  expect_identical(lago_blocks(r)$confidence_set$rows, why)
  expect_match(bb2_print(r), why, fixed = TRUE)

  full <- bb2_fit()
  empty <- full
  empty$cs <- NULL
  empty_rows <- lago_blocks(empty)$confidence_set$rows
  expect_match(empty_rows[2], "No confidence set was found for the current outcome goal.", fixed = TRUE)
  expect_no_match(bb2_print(empty), "First rows of the confidence set", fixed = TRUE)

  # a set without a cost column shows only its size
  nocost <- full
  nocost$cs$cost <- NULL
  expect_length(lago_blocks(nocost)$confidence_set$rows, 1)

  shrink <- empty
  shrink$confidence_set_size_percentage <- NULL
  shrink$shrinking_method_used <- TRUE
  expect_match(lago_blocks(shrink)$confidence_set$rows, "shrinking method was used", fixed = TRUE)
  expect_match(bb2_print(shrink), "Not computed: the goal could not be reached", fixed = TRUE)

  bb2_skip_report()
  expect_match(bb2_report(r), why, fixed = TRUE)
  html <- bb2_report(empty)
  expect_match(html, "No confidence set was found for the current outcome goal.", fixed = TRUE)
  expect_no_match(html, "First rows of the confidence set", fixed = TRUE)
  expect_match(bb2_report(shrink), "Not computed: the goal could not be reached", fixed = TRUE)
  expect_match(bb2_report(full), "First rows of the confidence set", fixed = TRUE)
})

test_that("the cost recap shows rounded coefficients per component", {
  r <- bb2_fit(include_confidence_set = FALSE)
  r$cost_list_of_vectors <- list(
    c(0, 2269.90684959357, 14.2386748093757, -7.41111352783079),
    c(4, 1e5, 1.7e-6)
  )
  line <- grep("component costs", lago_blocks(r)$inputs$rows, value = TRUE)
  expect_identical(
    line,
    paste0(
      "Intervention component costs: coaching_updt (0, 2269.9, 14.239, -7.4111), ",
      "launch_duration (4, 100000, 0.0000017)"
    )
  )
  # without labels that line up, the coefficients still show
  r$display_components <- NULL
  r$intervention_components <- "only_one"
  line <- grep("component costs", lago_blocks(r)$inputs$rows, value = TRUE)
  expect_identical(
    line,
    "Intervention component costs: (0, 2269.9, 14.239, -7.4111), (4, 100000, 0.0000017)"
  )
})

test_that("a p-value that underflows prints as < 2.2e-16 in print and report", {
  expect_identical(.lago_fmt_pval(c(0, 1e-300, 0.0123456, NA)), c("< 2.2e-16", "< 2.2e-16", "0.01235", NA))
  r <- bb2_fit(include_confidence_set = FALSE)
  r$test_results <- list(test_stat = 56.4253, p_val = 0)
  expect_match(bb2_print(r), "P-value: < 2.2e-16", fixed = TRUE)
  bb2_skip_report()
  expect_match(bb2_report(r), "P-value: &lt; 2.2e-16", fixed = TRUE)
})

test_that("a continuous outcome is told the test needs a binary outcome", {
  g <- within(BB_data, group <- ifelse(pre_post == 0, "control", "treatment"))
  r <- bb2_fit(
    data = g, glm_family = "gaussian", outcome_type = "continuous",
    include_confidence_set = FALSE
  )
  printed <- bb2_print(r)
  expect_match(printed, "The overall test is only available for binary outcomes.", fixed = TRUE)
  expect_no_match(printed, "include a 'group' column", fixed = TRUE)
})

test_that("an odd alpha gives a short confidence label", {
  lab <- function(a) .lago_confidence_label(structure(list(), lago_call_args = list(confidence_set_alpha = a)))
  expect_identical(lab(1 / 3), "66.67%")
  expect_identical(lab(0.05), "95%")
  expect_identical(lab(1e-5), "99.999%")
})
