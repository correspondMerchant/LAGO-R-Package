# Fixed center and time effects need levels with observed, positively weighted rows.

row_checks_run <- function(data, ...) {
  tryCatch(
    {
      suppressWarnings(suppressMessages(lago_optimization(
        data = data,
        outcome_name = "pp3_oxytocin_mother",
        outcome_type = "binary",
        intervention_components = c("coaching_updt", "launch_duration"),
        intervention_lower_bounds = c(0, 0),
        intervention_upper_bounds = c(40, 3),
        cost_list_of_vectors = list(c(0, 1), c(0, 1)),
        outcome_goal = 0.85,
        outcome_goal_intention = "maximize",
        include_confidence_set = FALSE,
        quiet = TRUE,
        ...
      )))
      "no error"
    },
    error = conditionMessage
  )
}

row_checks_data <- function() {
  d <- as.data.frame(BB_data)
  d$center <- d$site_name
  d
}

no_center_message <- paste0(
  "Fixed center effects need at least two centers, but the data has no ",
  "observed center. Fill in the center column, or set ",
  "include_center_effects = FALSE."
)

test_that("an all NA center column gets a clear refusal", {
  d <- row_checks_data()
  for (empty in list(NA, NA_character_, factor(NA, levels = c("x", "y")))) {
    d$center <- empty
    expect_identical(
      row_checks_run(d, include_center_effects = TRUE),
      no_center_message
    )
  }
})

test_that("an all NA center column is refused before given center weights are checked", {
  d <- row_checks_data()
  d$center <- NA
  expect_identical(
    row_checks_run(d,
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = c(0.5, 0.5)
    ),
    no_center_message
  )
})

test_that("center level data with an all NA center column gets a clear refusal", {
  cl <- data.frame(
    center = NA, coaching_updt = c(0, 5, 10, 15),
    launch_duration = c(1, 3, 2, 3), proportion = c(0.2, 0.3, 0.5, 0.6),
    center_sample_size = rep(20, 4)
  )
  err <- tryCatch(
    suppressWarnings(suppressMessages(lago_optimization(
      data = cl, input_data_structure = "center_level",
      outcome_name = "proportion", outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.5,
      quiet = TRUE
    ))),
    error = conditionMessage
  )
  expect_identical(err, paste0(
    "Fixed center effects need at least two centers, but the data has no ",
    "observed center. Fill in the center column."
  ))
})

positive_weight_message <- function(term, kept) {
  paste0(
    "Fixed ", if (term == "center") "center" else "time", " effects need at ",
    "least two ", term, "s with a row that has a positive glm weight ",
    "(center_sample_size for center level data), but ",
    if (length(kept) == 0) {
      paste0("no row has one. Give the rows of at least two ", term, "s")
    } else {
      paste0("only the ", term, " '", kept, "' has one. Give the rows of another ", term)
    },
    " a positive weight."
  )
}

test_that("a center whose rows all have zero weight does not count as usable", {
  d <- row_checks_data()
  centers <- sort(unique(d$center))[1:2]
  d <- d[d$center %in% centers, ]
  w <- ifelse(d$center == centers[1], 0, 1)
  expect_identical(
    row_checks_run(d, include_center_effects = TRUE, weights = w),
    positive_weight_message("center", centers[2])
  )
  expect_identical(
    row_checks_run(d, include_center_effects = TRUE, weights = rep(0, nrow(d))),
    positive_weight_message("center", character(0))
  )
})

test_that("a center level center with a zero sample size does not count as usable", {
  cl <- data.frame(
    center = rep(c("a", "b"), each = 4), coaching_updt = rep(c(0, 5, 10, 15), 2),
    launch_duration = rep(c(1, 3, 2, 3), 2),
    proportion = c(0.2, 0.3, 0.5, 0.6, 0.25, 0.35, 0.45, 0.7),
    center_sample_size = rep(c(0, 20), each = 4)
  )
  err <- tryCatch(
    suppressWarnings(suppressMessages(lago_optimization(
      data = cl, input_data_structure = "center_level",
      outcome_name = "proportion", outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.5,
      quiet = TRUE
    ))),
    error = conditionMessage
  )
  expect_identical(err, positive_weight_message("center", "b"))
})

test_that("a period whose rows all have zero weight does not count as usable", {
  d <- row_checks_data()
  d$period <- ifelse(seq_len(nrow(d)) %% 2 == 0, 1, 2)
  w <- ifelse(d$period == 2, 0, 1)
  expect_identical(
    row_checks_run(d,
      include_time_effects = TRUE, time_effect_optimization_value = 1,
      weights = w
    ),
    positive_weight_message("period", "1")
  )
})

test_that("zero weight rows next to positive ones in the same center are fine", {
  d <- row_checks_data()
  w <- rep(1, nrow(d))
  w[seq(1, nrow(d), 7)] <- 0
  expect_identical(
    row_checks_run(d, include_center_effects = TRUE, weights = w),
    "no error"
  )
})
