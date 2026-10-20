# Clear refusals for a center name that is not a center and for a global
# na.action that keeps or stops on rows with a missing value.

small_checks_run <- function(d, ...) {
  tryCatch(
    {
      suppressWarnings(suppressMessages(lago_optimization(
        data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
        intervention_components = c("coaching_updt", "launch_duration"),
        intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
        cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.85,
        outcome_goal_intention = "maximize", include_confidence_set = FALSE,
        quiet = TRUE, ...
      )))
      "no error"
    },
    error = conditionMessage
  )
}

# BB_data with the rows of one site given a missing center
small_checks_missing_center <- function() {
  d <- as.data.frame(BB_data)
  label <- as.character(d$site_name)
  label[label == sort(unique(label))[1]] <- NA
  d$center <- label
  d
}

test_that("NA_character_ is refused when no center is an explicit NA level", {
  expected <- paste0(
    "center_effects_optimization_values (NA_character_) must be one of the ",
    "centers in the 'center' column. To select the rows with a missing center, ",
    "make them an explicit level with addNA()."
  )
  d <- small_checks_missing_center()
  expect_identical(
    small_checks_run(d, include_center_effects = TRUE, center_effects_optimization_values = NA_character_),
    expected
  )
  d$center <- factor(d$center)
  expect_identical(
    small_checks_run(d, include_center_effects = TRUE, center_effects_optimization_values = NA_character_),
    expected
  )
})

test_that("NA_character_ still selects an explicit NA level", {
  d <- small_checks_missing_center()
  d$center <- addNA(factor(d$center))
  expect_identical(
    small_checks_run(d, include_center_effects = TRUE, center_effects_optimization_values = NA_character_),
    "no error"
  )
})

test_that("the center name refusals read as one sentence", {
  d <- as.data.frame(BB_data)
  d$center <- d$site_name
  expect_identical(
    small_checks_run(d, include_center_effects = TRUE, center_effects_optimization_values = 5),
    "center_effects_optimization_values must be a character string naming one center."
  )
  expect_identical(
    small_checks_run(d,
      include_center_effects = TRUE,
      center_effects_optimization_values = sort(unique(d$center))[1:2]
    ),
    "center_effects_optimization_values must be a single center name."
  )
  expect_identical(
    small_checks_run(d, include_center_effects = TRUE, center_effects_optimization_values = "Nowhere"),
    paste0(
      "center_effects_optimization_values (Nowhere) must be one of the ",
      "centers in the 'center' column."
    )
  )
})

small_checks_na_action <- function(action, d) {
  old <- options(na.action = action)
  on.exit(options(old), add = TRUE)
  small_checks_run(d)
}

na_action_message <- function(action, rows) {
  paste0(
    "The global option na.action is ", action, ", but ", rows,
    if (rows == 1) " row has" else " rows have", " a missing value in a model ",
    "variable or its glm weight. The outcome model needs ",
    if (rows == 1) "that row" else "those rows",
    " dropped, so set options(na.action = \"na.omit\") or ",
    "options(na.action = \"na.exclude\"), or fill in the missing values."
  )
}

test_that("na.pass or na.fail with a missing model value gets a clear refusal", {
  d <- as.data.frame(BB_data)
  # three missing values in two rows, so rows are counted, not values
  d$coaching_updt[c(5, 9)] <- NA
  d$launch_duration[5] <- NA
  for (action in c("na.pass", "na.fail")) {
    expect_identical(small_checks_na_action(action, d), na_action_message(action, 2))
  }
  expect_identical(small_checks_na_action(stats::na.pass, d), na_action_message("na.pass", 2))
  expect_identical(small_checks_na_action(stats::na.fail, d), na_action_message("na.fail", 2))
  one <- as.data.frame(BB_data)
  one$coaching_updt[5] <- NA
  expect_identical(small_checks_na_action("na.fail", one), na_action_message("na.fail", 1))
})

test_that("na.pass or na.fail with complete data, and na.exclude with missing data, still run", {
  for (action in c("na.pass", "na.fail")) {
    expect_identical(small_checks_na_action(action, as.data.frame(BB_data)), "no error")
  }
  d <- as.data.frame(BB_data)
  d$coaching_updt[5] <- NA
  expect_identical(small_checks_na_action("na.exclude", d), "no error")
})
