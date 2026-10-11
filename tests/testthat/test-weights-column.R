# A data column named weights must not replace the weights the outcome model uses.

weights_column_fixture <- function() {
  set.seed(512)
  n <- 60L
  d <- data.frame(x = seq(0, 3, length.out = n), z = rnorm(n))
  d$y <- 0.4 + 0.2 * d$x + 0.1 * d$z + rnorm(n, sd = 0.1)
  d$w <- rep(c(0.5, 2, 1), length.out = n)
  d$center <- factor(rep_len(c("a", "b", "c"), n))
  d
}

weights_column_fit <- function(d, weights = NULL, ...) {
  LAGOtrials:::outcome_model_fitting(
    data = d, outcome_name = "y", family_object = gaussian(),
    intervention_components = "x", weights = weights,
    additional_covariates = "z", center_characteristics = NULL, ...
  )$model
}

test_that("the weights argument is used when the data has a weights column", {
  d <- weights_column_fixture()
  expected <- weights_column_fit(d, d$w)
  d$weights <- rep_len(c(9, 0.1, 4, 0.3), nrow(d))
  fit <- weights_column_fit(d, d$w)
  expect_equal(unname(fit$prior.weights), d$w)
  expect_equal(coef(fit), coef(expected))
})

test_that("no weights argument fits unweighted when the data has a weights column", {
  d <- weights_column_fixture()
  expected <- weights_column_fit(d)
  d$weights <- rep_len(c(9, 0.1, 4, 0.3), nrow(d))
  fit <- weights_column_fit(d)
  expect_equal(unname(fit$prior.weights), rep(1, nrow(d)))
  expect_equal(coef(fit), coef(expected))
})

test_that("the weights stay aligned with the fitted rows when a row is dropped", {
  d <- weights_column_fixture()
  d$z[c(3, 17)] <- NA
  d$weights <- rep_len(c(9, 0.1, 4, 0.3), nrow(d))
  fit <- weights_column_fit(d, d$w)
  expect_equal(unname(fit$prior.weights), d$w[-c(3, 17)])
})

test_that("center level data is weighted by center_sample_size despite a weights column", {
  cl <- data.frame(
    center = factor(rep(c("c1", "c2", "c3"), each = 4)),
    x = rep(c(0, 5, 10, 15), 3),
    proportion = c(0.2, 0.3, 0.5, 0.6, 0.25, 0.3, 0.45, 0.7, 0.1, 0.35, 0.4, 0.65),
    center_sample_size = rep(c(20, 50, 35), each = 4)
  )
  cl$weights <- rep_len(c(1, 7, 2), nrow(cl))
  fit <- suppressWarnings(LAGOtrials:::outcome_model_fitting(
    data = cl, input_data_structure = "center_level", outcome_name = "proportion",
    family_object = quasibinomial(), intervention_components = "x",
    weights = NULL, additional_covariates = NULL, center_characteristics = NULL,
    include_center_effects = TRUE
  )$model)
  expect_equal(unname(fit$prior.weights), cl$center_sample_size)
})

test_that("a weights column with missing values does not drop rows from the fit", {
  d <- weights_column_fixture()
  expected <- weights_column_fit(d, d$w, include_center_effects = TRUE)
  d$weights <- NA_real_
  fit <- weights_column_fit(d, d$w, include_center_effects = TRUE)
  expect_equal(unname(fit$prior.weights), d$w)
  expect_equal(coef(fit), coef(expected))
})

test_that("a weights column does not change the advice when effects lack rows", {
  d <- weights_column_fixture()
  n <- nrow(d)
  d$period <- factor(rep_len(c(1, 2), n))
  d$center[seq(1, n, 2)] <- NA
  d$period[seq(2, n, 2)] <- NA
  fit <- function(data) {
    tryCatch(
      weights_column_fit(data, rep(1, n),
        include_center_effects = TRUE, include_time_effects = TRUE
      ),
      error = conditionMessage
    )
  }
  d$weights <- NA_real_
  expect_identical(fit(d), paste0(
    "Fixed time effects need at least two periods, but the data has only ",
    "the period '1'. Add data from another period, or set ",
    "include_time_effects = FALSE."
  ))
  d$period <- factor(rep_len(c(1, 2), n))
  d$center[] <- NA
  d$center[1:2] <- c("a", "b")
  d$period[1:2] <- NA
  expect_identical(fit(d), paste0(
    "No row has every model variable and its glm weight (center_sample_size ",
    "for center level data) observed, so the outcome model cannot be fitted. ",
    "Fill in the missing values, or set include_center_effects = FALSE, or ",
    "set include_time_effects = FALSE."
  ))
})

test_that("a fitting error names the weights rather than printing them", {
  d <- weights_column_fixture()
  w <- d$w
  w[3] <- -1
  err <- tryCatch(weights_column_fit(d, w), error = conditionMessage)
  expect_match(err, "weights = weights, : negative weights not allowed\\n$")
  expect_no_match(err, "weights = c(", fixed = TRUE)
})

test_that("a weights column can still be an additional covariate", {
  d <- weights_column_fixture()
  d$weights <- d$z
  fit <- LAGOtrials:::outcome_model_fitting(
    data = d, outcome_name = "y", family_object = gaussian(),
    intervention_components = "x", weights = d$w,
    additional_covariates = "weights", center_characteristics = NULL
  )$model
  expect_equal(unname(fit$prior.weights), d$w)
  expect_true("weights" %in% names(coef(fit)))
})

test_that("the printed call still names the weights argument", {
  d <- weights_column_fixture()
  fit <- weights_column_fit(d, d$w)
  expect_identical(fit$call$weights, quote(weights))
})

weights_column_run <- function(data, ...) {
  suppressWarnings(suppressMessages(lago_optimization(
    data = data,
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    confidence_set_grid_step_size = c(2, 0.15),
    outcome_goal_intention = "maximize",
    quiet = TRUE,
    ...
  )))
}

weights_column_key <- function(res) {
  list(
    rec_int = res$rec_int, est = res$est_outcome_goal,
    ci = res$est_outcome_ci, cs = res$cs
  )
}

test_that("lago_optimization() gives the same result with or without a weights column", {
  d <- as.data.frame(BB_data)
  set.seed(29)
  w <- runif(nrow(d), 0.5, 2)
  shadowed <- d
  shadowed$weights <- rev(w)
  for (type in c("binary", "continuous")) {
    outcome <- if (type == "binary") "pp3_oxytocin_mother" else "leadership_updt"
    goal <- if (type == "binary") 0.85 else 0.93
    for (weights in list(NULL, w)) {
      expected <- weights_column_run(d, outcome_name = outcome,
        outcome_type = type, outcome_goal = goal, weights = weights)
      result <- weights_column_run(shadowed, outcome_name = outcome,
        outcome_type = type, outcome_goal = goal, weights = weights)
      expect_equal(weights_column_key(result), weights_column_key(expected))
    }
  }
})
