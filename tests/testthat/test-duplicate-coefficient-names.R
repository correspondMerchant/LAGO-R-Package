# Two coefficients with the same name cannot be told apart, as when a column has
# both a level labeled NA and an explicit NA level, or a column is named like
# another column and its level.

dup_names_base <- function() as.data.frame(BB_data)

dup_names_run <- function(d, ...) {
  tryCatch(
    {
      suppressWarnings(suppressMessages(lago_optimization(
        data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
        intervention_components = c("coaching_updt", "launch_duration"),
        intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
        cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.85,
        outcome_goal_intention = "maximize", quiet = TRUE, ...
      )))
      "no error"
    },
    error = conditionMessage
  )
}

# centers with one labeled "NA" and one an explicit NA level
dup_names_centers <- function(literal = "NA") {
  d <- dup_names_base()
  sites <- sort(unique(d$site_name))
  label <- as.character(d$site_name)
  label[label == sites[1]] <- literal
  label[label == sites[2]] <- NA
  d$center <- factor(label, levels = c(sort(unique(label[!is.na(label)])), NA), exclude = NULL)
  d
}

dup_names_message <- function(name, columns) {
  paste0(
    "The outcome model gives more than one coefficient the ",
    if (length(name) == 1) "name " else "names ", paste(name, collapse = ", "),
    ", from the ", if (length(columns) == 1) "column " else "columns ",
    paste(columns, collapse = ", "),
    ". glm() names a factor coefficient by joining the column name and the ",
    "level, so the package cannot tell these coefficients apart. This happens ",
    "when a column has both a level labeled with the text NA and an explicit NA ",
    "level (as from addNA()), or when a column name is another column's name ",
    "followed by one of its levels. Rename one of those levels or columns."
  )
}

test_that("a center column with both NA levels is refused", {
  d <- dup_names_centers()
  expected <- dup_names_message("centerNA", "center")
  expect_identical(
    dup_names_run(d, include_center_effects = TRUE, include_confidence_set = FALSE),
    expected
  )
  expect_identical(
    dup_names_run(d,
      include_center_effects = TRUE, include_confidence_set = FALSE,
      center_effects_optimization_values = NA_character_
    ),
    expected
  )
  expect_identical(dup_names_run(d, include_center_effects = TRUE), expected)
})

test_that("a period column with both NA levels is refused", {
  d <- dup_names_base()
  set.seed(1)
  period <- sample(c("1", "2", "NA", NA), nrow(d), TRUE)
  d$period <- factor(period, levels = c("1", "2", "NA", NA), exclude = NULL)
  expect_identical(
    dup_names_run(d, include_time_effects = TRUE, time_effect_optimization_value = 1),
    dup_names_message("periodNA", "period")
  )
})

test_that("a covariate with both NA levels is refused", {
  d <- dup_names_base()
  set.seed(2)
  group <- sample(c("a", "NA", NA), nrow(d), TRUE)
  d$grp <- factor(group, levels = c("a", "NA", NA), exclude = NULL)
  expect_identical(
    dup_names_run(d, additional_covariates = "grp", include_confidence_set = FALSE),
    dup_names_message("grpNA", "grp")
  )
})

test_that("a column named like another column and its level is refused", {
  d <- dup_names_base()
  set.seed(4)
  d$visit <- factor(sample(c("1", "2"), nrow(d), TRUE))
  d$visit2 <- stats::rnorm(nrow(d))
  expect_identical(
    dup_names_run(d, additional_covariates = c("visit", "visit2"), include_confidence_set = FALSE),
    dup_names_message("visit2", c("visit", "visit2"))
  )
})

test_that("every repeated name is listed", {
  d <- dup_names_base()
  set.seed(5)
  for (column in c("g", "h")) {
    d[[column]] <- factor(sample(c("a", "NA", NA), nrow(d), TRUE),
      levels = c("a", "NA", NA), exclude = NULL
    )
  }
  expect_identical(
    dup_names_run(d, additional_covariates = c("g", "h"), include_confidence_set = FALSE),
    dup_names_message(c("gNA", "hNA"), c("g", "h"))
  )
})

test_that("lago_budget() reports the refusal rather than an unreachable budget", {
  expected <- dup_names_message("centerNA", "center")
  args <- list(
    data = dup_names_centers(), outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary", intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal_intention = "maximize",
    include_center_effects = TRUE, budget = 30, n_grid = 4
  )
  warned <- character(0)
  result <- withCallingHandlers(
    suppressMessages(do.call(lago_budget, args)),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(result$feasible)
  expect_identical(result$run_error, expected)
  expect_identical(warned, paste(
    "No outcome goal in the searched range was reachable, because every",
    "run of lago_optimization() failed. The first failed with:", expected
  ))
  # cli wraps long lines, so search the joined text
  printed <- paste(cli::cli_fmt(print(result)), collapse = " ")
  expect_true(grepl("Every run of lago_optimization() failed", printed, fixed = TRUE))
  expect_true(grepl("the name centerNA, from the column center", printed, fixed = TRUE))
  expect_false(grepl("increase the budget", printed, fixed = TRUE))
})

test_that("a center labeled NA or an explicit NA level alone still runs", {
  expect_identical(
    dup_names_run(dup_names_centers("Other"), include_center_effects = TRUE, include_confidence_set = FALSE),
    "no error"
  )
  # the literal "NA" label without the explicit NA level
  d <- dup_names_centers()
  # is.na() is FALSE on an explicit NA level, so test the labels
  d <- d[!is.na(as.character(d$center)), ]
  d$center <- droplevels(d$center)
  expect_false(anyNA(levels(d$center)))
  expect_identical(
    dup_names_run(d, include_center_effects = TRUE, include_confidence_set = FALSE),
    "no error"
  )
})
