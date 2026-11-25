# Round 6 input checks: the interaction column tolerance, the goal at the mean
# warning in a goal sweep, POSIXlt columns, center effects with center
# characteristics, numeric and factor center ids, NaN ids, the
# optimization_method and interaction hint messages, and the fit errors.

bb6_data <- function(n = 200) {
  set.seed(1)
  d <- data.frame(
    center = rep(1:10, each = n / 10), period = rep(1:4, n / 4),
    A = runif(n, 0, 2), B = runif(n, 0, 3)
  )
  d$y <- rbinom(n, 1, stats::plogis(-1 + 0.5 * d$A + 0.3 * d$B))
  d$yc <- 1 + d$A + d$B + rnorm(n)
  d
}

bb6_run <- function(..., data = bb6_data()) {
  args <- list(
    outcome_name = "yc", outcome_type = "continuous",
    intervention_components = c("A", "B"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(2, 3), unit_costs = c(1, 1),
    default_cost_fxn_type = "linear", outcome_goal = 4,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE,
    quiet = TRUE
  )
  given <- list(...)
  args[names(given)] <- given
  args$data <- data
  do.call("lago_optimization", args)
}

bb6_quiet <- function(...) suppressWarnings(suppressMessages(bb6_run(...)))

bb6_interaction <- function(data, ...) {
  bb6_quiet(
    data = data, intervention_components = c("A", "B", "A:B"),
    main_components = c("A", "B"), include_interaction_terms = TRUE, ...
  )
}

# the messages of the conditions an expression gives
bb6_conditions <- function(expr) {
  seen <- list(warnings = character(0), messages = character(0))
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      seen$warnings <<- c(seen$warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      seen$messages <<- c(seen$messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  c(list(value = value), seen)
}

test_that("a few large products do not let a wrong small interaction row pass", {
  d <- bb6_data()
  d$A[2] <- 1000
  d$B[2] <- 1000
  d$`A:B` <- d$A * d$B
  d$`A:B`[3] <- d$`A:B`[3] + 3
  expect_error(
    bb6_interaction(d, intervention_upper_bounds = c(2, 3)),
    "in row 3"
  )
  set.seed(2)
  u <- bb6_data()
  u$A <- runif(200, 0, 1000)
  u$B <- runif(200, 0, 1000)
  u$`A:B` <- u$A * u$B
  k <- which.min(u$`A:B`)
  u$`A:B`[k] <- u$`A:B`[k] + 200
  expect_error(
    bb6_interaction(u, intervention_upper_bounds = c(1000, 1000)),
    paste0("in row ", k, " ")
  )
})

test_that("an interaction column saved with rounding is still accepted", {
  d <- bb6_data()
  d$A[2] <- 1000
  d$B[2] <- 1000
  d$`A:B` <- signif(d$A * d$B, 4)
  expect_s3_class(bb6_interaction(d), "lago")
  # 3 decimals, when the median product is 0.5 or more
  d <- bb6_data()
  d$A <- d$A + 1
  d$`A:B` <- round(d$A * d$B, 3)
  expect_s3_class(
    bb6_interaction(d, intervention_upper_bounds = c(3, 3)),
    "lago"
  )
})

test_that("the goal at the mean warning appears once in a goal sweep", {
  d <- bb6_data()
  out <- bb6_conditions(lago_sensitivity(
    data = d, outcome_name = "yc", outcome_type = "continuous",
    intervention_components = c("A", "B"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(2, 3), unit_costs = c(1, 1),
    default_cost_fxn_type = "linear", outcome_goal = 1,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE,
    quiet = TRUE, parameter = "outcome_goal", values = c(1, 1.5, 2)
  ))
  expect_equal(sum(grepl("is to maximize the outcome", out$warnings)), 1)
  expect_match(
    out$warnings, "the outcome goal is at or below the observed mean",
    all = FALSE
  )
  out <- bb6_conditions(bb6_run(
    data = d, outcome_goal = 8, outcome_goal_intention = "minimize"
  ))
  expect_match(
    out$warnings,
    "to minimize the outcome, but the outcome goal is at or above the observed mean",
    all = FALSE
  )
})

test_that("a POSIXlt period column is read as POSIXct", {
  d <- bb6_data()
  times <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 3600 * (d$period - 1)
  d$period <- times
  ct <- bb6_quiet(
    data = d, include_time_effects = TRUE,
    time_effect_optimization_value = times[3]
  )
  d$period <- as.POSIXlt(times)
  out <- bb6_conditions(bb6_run(
    data = d, include_time_effects = TRUE,
    time_effect_optimization_value = times[3]
  ))
  expect_match(out$messages, "'period' hold POSIXlt date-times", all = FALSE)
  expect_equal(out$value$rec_int, ct$rec_int)
  expect_equal(out$value$est_outcome_goal, ct$est_outcome_goal)
})

bb6_center_level <- function() {
  cl <- data.frame(
    center = 1:10, A = seq(0, 2, length.out = 10),
    B = seq(0, 3, length.out = 10)[c(3, 1, 7, 2, 9, 4, 10, 5, 8, 6)],
    size = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
  )
  cl$proportion <- stats::plogis(-1 + 0.5 * cl$A + 0.2 * cl$B + 0.05 * cl$size)
  cl$center_sample_size <- 50
  cl
}

bb6_center_level_run <- function(...) {
  bb6_run(
    data = bb6_center_level(), input_data_structure = "center_level",
    outcome_name = "proportion", outcome_type = "binary", outcome_goal = 0.6,
    center_characteristics = "size",
    center_characteristics_optimization_values = 4, ...
  )
}

test_that("center level data with center characteristics leaves center effects out", {
  out <- bb6_conditions(bb6_center_level_run())
  expect_false(out$value$include_center_effects)
  expect_match(out$messages, "center effects are left out", all = FALSE)
  expect_error(
    suppressWarnings(suppressMessages(
      bb6_center_level_run(include_center_effects = TRUE)
    )),
    "set include_center_effects = FALSE to use the center characteristics",
    fixed = TRUE
  )
})

test_that("a factor names the center to optimize for", {
  d <- bb6_data()
  d$center <- paste0("c", d$center)
  by_text <- bb6_quiet(
    data = d, include_center_effects = TRUE,
    center_effects_optimization_values = "c3"
  )
  by_factor <- bb6_quiet(
    data = d, include_center_effects = TRUE,
    center_effects_optimization_values = factor("c3")
  )
  expect_equal(by_factor$rec_int, by_text$rec_int)
  expect_equal(by_factor$est_outcome_goal, by_text$est_outcome_goal)
})

test_that("numeric center ids are labeled in full", {
  d <- bb6_data()
  d$center <- d$center * 100000
  ids <- format(sort(unique(d$center)), scientific = FALSE, trim = TRUE)
  w <- stats::setNames(rep(0.1, 10), rev(ids))
  r <- bb6_quiet(
    data = d, include_center_effects = TRUE, center_weights_for_outcome_goal = w
  )
  expect_identical(r$model$xlevels$center, ids)
  r <- bb6_quiet(
    data = d, include_center_effects = TRUE,
    center_effects_optimization_values = "300000"
  )
  by_weight <- bb6_quiet(
    data = d, include_center_effects = TRUE,
    center_weights_for_outcome_goal = as.numeric(ids == "300000")
  )
  expect_equal(r$rec_int, by_weight$rec_int)
  expect_equal(r$est_outcome_goal, by_weight$est_outcome_goal)
  # the message lists the centers
  expect_error(
    bb6_quiet(
      data = d, include_center_effects = TRUE,
      center_effects_optimization_values = "3e5"
    ),
    "must be one of the centers in the 'center' column: '100000', '200000'"
  )
  # ids that print alike keep the labels as.factor() gives
  x <- c(1, 1 + 1e-15, 2)
  expect_identical(as_label_factor(x), as.factor(x))
  expect_identical(as_label_factor(c("b", "a")), as.factor(c("b", "a")))
})

test_that("NaN in a numeric center or period column is missing, not a level", {
  d <- bb6_data()
  d$center[1:20] <- NaN
  d$period[21:30] <- NaN
  r <- bb6_quiet(
    data = d, include_center_effects = TRUE, include_time_effects = TRUE,
    time_effect_optimization_value = 2
  )
  expect_false("NaN" %in% r$model$xlevels$center)
  expect_false("NaN" %in% r$model$xlevels$period)
  expect_equal(stats::nobs(r$model), 170)
})

test_that("an unknown optimization_method names the methods", {
  expect_error(
    bb6_quiet(optimization_method = "grid"),
    paste0(
      "optimization_method must be one of the supported methods, ",
      "\"numerical\" or \"grid_search\", not \"grid\"."
    ),
    fixed = TRUE
  )
})

test_that("the interaction hint gives code that runs for any column name", {
  d <- bb6_data()
  names(d)[names(d) == "A"] <- "dose a"
  args <- list(
    intervention_components = c("dose a", "B", "dose a:B"),
    main_components = c("dose a", "B"), include_interaction_terms = TRUE
  )
  missing <- tryCatch(
    do.call(bb6_quiet, c(args, list(data = d))),
    error = conditionMessage
  )
  code <- "data[[\"dose a:B\"]] <- data[[\"dose a\"]] * data$B"
  expect_match(missing, code, fixed = TRUE)
  data <- d
  eval(parse(text = code))
  expect_equal(data$`dose a:B`, d$`dose a` * d$B)
})

bb6_fit <- function(data, family, outcome = "y", ...) {
  outcome_model_fitting(
    data = data, outcome_name = outcome, family_object = family,
    intervention_components = c("x1", "x2"), weights = NULL,
    center_characteristics = NULL, additional_covariates = NULL, ...
  )
}

bb6_fit_data <- function(seed, n = 60) {
  set.seed(seed)
  d <- data.frame(x1 = runif(n, 0, 10), x2 = runif(n, 0, 10))
  d$y <- rbinom(n, 1, pmin(0.99, pmax(0.01, 0.05 + 0.09 * d$x1 + rnorm(n, 0, 0.05))))
  d
}

test_that("a fit that does not converge gives glm's warnings and a likely cause", {
  set.seed(1)
  d <- data.frame(x1 = runif(60, 0, 10), x2 = runif(60, 0, 5))
  d$y <- as.numeric(d$x1 > 5)
  err <- tryCatch(bb6_fit(d, binomial()), error = conditionMessage)
  expect_match(err, "Model did not converge", fixed = TRUE)
  expect_match(err, "  - glm.fit: algorithm did not converge", fixed = TRUE)
  expect_match(err, "most likely separation", fixed = TRUE)
  # an identity link that does not converge gets the identity hint
  err <- tryCatch(bb6_fit(bb6_fit_data(3), binomial("identity")), error = conditionMessage)
  expect_match(err, "Model did not converge", fixed = TRUE)
  expect_match(err, "With the identity link", fixed = TRUE)
  expect_no_match(err, "separation")
  # a gaussian fit names no binomial cause
  set.seed(52)
  g <- data.frame(x1 = runif(40, 0, 10), x2 = runif(40, 0, 10))
  g$y <- pmin(0.999, pmax(0.001, stats::plogis(30 * (g$x1 - 5)) + rnorm(40, 0, 0.02)))
  err <- tryCatch(bb6_fit(g, gaussian("logit")), error = conditionMessage)
  expect_match(err, "Model did not converge", fixed = TRUE)
  expect_no_match(err, "separation|identity link")
})

test_that("the identity link hint fires when glm cannot correct the step size", {
  err <- tryCatch(bb6_fit(bb6_fit_data(61), binomial("identity")), error = conditionMessage)
  # how glm() fails here depends on the BLAS, so any of its failures counts
  if (!is.character(err)) skip("glm() fitted this data on this platform")
  expect_match(err, "cannot correct step size|no valid set of coefficients|did not converge")
  expect_match(err, "With the identity link", fixed = TRUE)
})

test_that("a zero weight center is left out under na.fail and na.pass", {
  d <- bb6_data()
  w <- ifelse(d$center == 10, 0, 1)
  fit <- function() {
    suppressWarnings(outcome_model_fitting(
      data = d, outcome_name = "y", family_object = binomial(),
      intervention_components = c("A", "B"), weights = w,
      center_characteristics = NULL, additional_covariates = NULL,
      include_center_effects = TRUE
    ))$model
  }
  d$center <- factor(d$center)
  omitted <- fit()
  for (action in c("na.fail", "na.pass")) {
    old <- options(na.action = action)
    model <- fit()
    options(old)
    expect_equal(coef(model), coef(omitted))
    expect_identical(attr(model, "lago_zero_weight_levels"), list(center = "10"))
  }
})

test_that("a global na.action that is another function has no name", {
  old <- options(na.action = function(object, ...) object)
  on.exit(options(old))
  expect_identical(LAGOtrials:::global_na_action_name(), "")
})
