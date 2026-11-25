# Bug bash 6 fixes in the presentation layer: an estimate that reached a goal
# on a rounding tie, large costs, the power settings in the inputs recap, the
# data text and the title of the report, the output folder checks of
# lago_report(), and percent signs in the help pages.

bb6_skip_report <- function() {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

bb6_bb_fit <- function(...) {
  bb <- BB_data
  args <- list(
    data = bb, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    outcome_goal = 0.8125, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE, quiet = TRUE
  )
  args[names(list(...))] <- list(...)
  suppressWarnings(suppressMessages(do.call(lago_optimization, args)))
}

bb6_line <- function(rows, start) rows[startsWith(rows, start)]

test_that("an estimate that reached a goal on a rounding tie shows as the goal", {
  r <- bb6_bb_fit()
  # the estimate is a hair below the goal, so on its own it rounds below it
  expect_false(isTRUE(r$shrinking_method_used))
  expect_lt(.lago_fmt_interval(r$est_outcome_goal), .lago_fmt_interval(0.8125))
  rows <- lago_blocks(r)$outcome$rows
  expect_identical(bb6_line(rows, "Estimated outcome: "), "Estimated outcome: 0.813")
  expect_identical(bb6_line(rows, "Outcome goal: "), "Outcome goal: 0.813")
  # a goal that was not reached keeps its own estimate
  r$shrinking_method_used <- TRUE
  rows <- lago_blocks(r)$outcome$rows
  expect_identical(bb6_line(rows, "Estimated outcome: "), "Estimated outcome: 0.812")
})

test_that(".lago_fmt_estimate() moves only an estimate on the missed side of a reached goal", {
  est <- 0.81249998723151962
  expect_identical(.lago_fmt_estimate(est, 0.8125, "maximize", TRUE), 0.813)
  expect_identical(.lago_fmt_estimate(est, 0.8125, "maximize", FALSE), 0.812)
  expect_identical(.lago_fmt_estimate(est, 0.8125, "minimize", TRUE), 0.812)
  expect_identical(.lago_fmt_estimate(0.6125001, 0.6124999, "minimize", TRUE), 0.612)
  expect_identical(.lago_fmt_estimate(0.9, 0.8125, "maximize", TRUE), 0.9)
  # an estimate farther from the goal than the tolerance keeps its own text
  expect_identical(.lago_fmt_estimate(0.7, 0.8125, "maximize", TRUE), 0.7)
  expect_identical(.lago_fmt_estimate(0.8124, 0.8125, "maximize", TRUE), 0.812)
  expect_identical(.lago_fmt_estimate(est, NULL, "maximize", TRUE), 0.812)
  expect_identical(.lago_fmt_estimate(est, NA_real_, "maximize", TRUE), 0.812)
  expect_identical(.lago_fmt_estimate(NA_real_, 0.8125, "maximize", TRUE), NA_real_)
  expect_identical(.lago_fmt_estimate(est, 0.8125, NULL, TRUE), 0.812)
})

test_that("the estimate is checked against the effective outcome goal too", {
  r <- bb6_bb_fit(outcome_goal = 0.5)
  # an object whose power goal raised the goal to a tie that the estimate
  # reached a hair short of
  r$power_goal <- 0.8
  r$effective_outcome_goal <- 0.8125
  r$est_outcome_goal <- 0.81249998723151962
  rows <- lago_blocks(r)$outcome$rows
  expect_identical(bb6_line(rows, "Estimated outcome: "), "Estimated outcome: 0.813")
})

test_that("a cost of 1e4 or more shows 2 decimals, and a smaller one 5 significant digits", {
  expect_identical(.lago_fmt_cost(1000012.3456), 1000012.35)
  expect_identical(.lago_fmt_cost(-1000012.3456), -1000012.35)
  expect_identical(.lago_fmt_cost(23927.77), 23927.77)
  expect_identical(.lago_fmt_cost(9999.77), 9999.8)
  expect_identical(.lago_fmt_cost(1234.5678), 1234.6)
  expect_identical(.lago_fmt_cost(c(NA, Inf, 0)), c(NA, Inf, 0))
  # the cost polynomials of the inputs recap keep 5 significant digits
  expect_identical(.lago_fmt_cost_coefs(c(1000012.3456, 12.3456)), "1000000, 12.346")
  # a large fixed cost no longer hides the part that changes with the dose
  r <- bb6_bb_fit(cost_list_of_vectors = list(c(1e6, 1700), c(0, 8000)), outcome_goal = 0.85)
  expect_identical(
    lago_blocks(r)$cost$rows,
    paste0("Cost: ", .lago_num_text(.lago_round_dec(r$rec_int_cost, 2, sig = FALSE)))
  )
  expect_false(lago_blocks(r)$cost$rows %in% paste0("Cost: ", .lago_num_text(signif(r$rec_int_cost, 5))))
})

test_that(".lago_fmt_cost_within() never shows a cost that fits above the budget", {
  expect_identical(.lago_fmt_cost_within(1234.56, 1234.57), 1234.5)
  expect_identical(.lago_fmt_cost_within(12345.678, 12345.679), 12345.67)
  expect_identical(.lago_fmt_cost_within(0.123456, 0.1234565), 0.12345)
  expect_identical(.lago_fmt_cost_within(1234.54, 1234.57), 1234.5)
  expect_identical(.lago_fmt_cost_within(1234.58, 1234.57), 1234.6)
  expect_identical(.lago_fmt_cost_within(1234.56, NA), 1234.6)
  expect_identical(.lago_fmt_cost_within(c(0, 1234.56), 1234.57), c(0, 1234.5))
})

test_that("the inputs recap shows the power settings of a power goal", {
  r <- bb6_bb_fit(outcome_goal = 0.5)
  expect_length(bb6_line(lago_blocks(r)$inputs$rows, "Power goal approach"), 0)
  r$power_goal <- 0.8
  args <- attr(r, "lago_call_args")
  args$power_goal <- 0.8
  args$num_centers_in_next_stage <- 10
  args$patients_per_center_in_next_stage <- 50
  args$power_goal_approach <- "conditional"
  attr(r, "lago_call_args") <- args
  rows <- lago_blocks(r)$inputs$rows
  expect_identical(bb6_line(rows, "Power goal approach: "), "Power goal approach: conditional")
  expect_identical(bb6_line(rows, "Next stage size: "), "Next stage size: 10 centers, 50 patients per center")
  expect_identical(
    bb6_line(rows, "ICC: "),
    "ICC: not specified (independent observations)"
  )
  # the lines follow the power goal line
  expect_identical(which(startsWith(rows, "Power goal approach")), which(startsWith(rows, "Power goal: ")) + 1L)
})

test_that(".lago_power_rows() writes each kind of icc and skips what was not recorded", {
  base <- list(power_goal_approach = "unconditional", num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 50)
  expect_identical(.lago_power_rows(c(base, list(icc = 0.05)))[3], "ICC: 0.05")
  expect_identical(
    .lago_power_rows(c(base, list(icc = c(0.05, 0.1))))[3],
    "ICC: 0.05 (control), 0.1 (treatment)"
  )
  expect_identical(
    .lago_power_rows(c(base, list(icc = c(treatment = 0.1, control = 0.05))))[3],
    "ICC: 0.05 (control), 0.1 (treatment)"
  )
  expect_identical(.lago_power_rows(NULL), character(0))
  expect_identical(
    .lago_power_rows(list(icc = 0.05)),
    "ICC: 0.05"
  )
  expect_identical(.lago_power_rows(list(icc = c(0.1, 0.2, 0.3))), character(0))
})

test_that(".lago_md_escape() shows data text as typed in markdown", {
  expect_identical(.lago_md_escape("*a*"), "&#42;a&#42;")
  expect_identical(.lago_md_escape("A|B"), "A&#124;B")
  expect_identical(.lago_md_escape("<b>x</b>"), "&#60;b&#62;x&#60;&#47;b&#62;")
  expect_identical(.lago_md_escape("a\nb"), "a b")
  expect_identical(.lago_md_escape(c("plain", NA, "")), c("plain", NA, ""))
  df <- data.frame(`a*b` = c("$1$", "x"), n = c(1.5, 2), check.names = FALSE,
    row.names = c("r_1", "r 2"))
  out <- .lago_md_escape_df(df)
  expect_identical(names(out), c("a&#42;b", "n"))
  expect_identical(out[[1]], c("&#36;1&#36;", "x"))
  expect_identical(out$n, c(1.5, 2))
  expect_identical(rownames(out), c("r&#95;1", "r 2"))
  # automatic row names stay automatic
  expect_identical(.row_names_info(.lago_md_escape_df(data.frame(a = factor("<i>")))), -1L)
})

test_that("the report shows the center names of the coefficient table as typed", {
  bb6_skip_report()
  set.seed(3)
  cn <- c("A|B", "*star*", "<b>bold</b>", "$1$ and $2$", "plain")
  d <- data.frame(center = rep(cn, each = 60), a = stats::runif(300, 0, 4), b = stats::runif(300, 0, 4))
  d$y <- 1 + 0.5 * d$a + 0.3 * d$b + as.numeric(factor(d$center)) * 0.1 + stats::rnorm(300, sd = 0.2)
  r <- suppressWarnings(suppressMessages(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("a", "b"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4), include_center_effects = TRUE,
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 3,
    confidence_set_grid_step_size = c(0.5, 0.5), quiet = TRUE
  )))
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  lago_report(r, out)
  h <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # the first center in the sort order of the locale is the reference level,
  # with no row of its own
  centers <- grep("^center", rownames(lago_coef_table(r)), value = TRUE)
  expect_length(centers, 4)
  html <- gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", centers, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  for (cell in html) expect_match(h, paste0("<td align=\"left\">", cell, "</td>"), fixed = TRUE)
  expect_false(grepl("<td align=\"left\">center<b>bold</b>", h, fixed = TRUE))
  expect_false(grepl("<em>star</em>", h, fixed = TRUE))
  expect_false(grepl("<math", h, fixed = TRUE))
  # the text lines still read as before
  expect_match(h, "<li>Outcome goal: 3 (maximize: at least this value)</li>", fixed = TRUE)
})

test_that("a title's line breaks become one space and leading spaces are not code", {
  expect_identical(.lago_report_title_yaml("Trial A\n\n    phase 2"), "Trial A phase 2")
  expect_identical(.lago_report_title_yaml("a  \nb"), "a b")
  expect_identical(.lago_report_title_yaml("one\r\ntwo\rthree"), "one two three")
  expect_identical(.lago_report_title_yaml("    code"), "code")
  expect_identical(.lago_report_title_yaml("line1\n"), "line1")
  expect_identical(.lago_report_title_yaml("a\tb"), "a&#9;b")
})

test_that("the report title over several lines renders as one heading", {
  bb6_skip_report()
  r <- bb6_bb_fit(outcome_goal = 0.85)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  lago_report(r, out, title = "Trial A\n\n    phase 2")
  h <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(h, "<title>Trial A phase 2</title>", fixed = TRUE)
  expect_match(h, ">Trial A phase 2</h1>", fixed = TRUE)
  expect_false(grepl("<code>phase 2", h, fixed = TRUE))
})

test_that("lago_report() names a file or an unwritable folder in the way of output_file", {
  r <- bb6_bb_fit(outcome_goal = 0.85)
  base <- tempfile("bb6_paths_")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  afile <- file.path(base, "afile.txt")
  file.create(afile)
  expect_error(
    lago_report(r, file.path(afile, "sub", "deeper", "r.html")),
    "because it is a file and not a folder"
  )
  ro <- file.path(base, "ro")
  dir.create(ro)
  Sys.chmod(ro, "555")
  on.exit(Sys.chmod(ro, "755"), add = TRUE, after = FALSE)
  skip_if(file.access(ro, 2) == 0, "the folder stays writable for this user")
  target <- file.path(ro, "new", "r.html")
  err <- tryCatch(lago_report(r, target), error = function(e) cli::ansi_strip(conditionMessage(e)))
  err <- gsub("\\s+", " ", err)
  expect_match(err, "cannot be written", fixed = TRUE)
  expect_match(err, basename(target), fixed = TRUE)
  expect_match(err, "cannot be written in", fixed = TRUE)
})

test_that("no help page has a doubled backslash before a percent sign", {
  man <- test_path("..", "..", "man")
  skip_if_not(dir.exists(man), "the man folder is not available")
  rd <- list.files(man, pattern = "[.]Rd$", full.names = TRUE)
  bad <- unlist(lapply(rd, function(f) {
    lines <- readLines(f, warn = FALSE)
    if (any(grepl("\\\\%", lines, fixed = TRUE))) basename(f)
  }))
  expect_identical(bad, NULL)
})
