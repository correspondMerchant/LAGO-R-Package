# Tests that validate_inputs() rejects invalid inputs with clear errors, and
# emits the expected warnings/messages for the non-fatal branches.
# These call the internal validator directly so they exercise every branch.
# Grouped by validation domain; see each section header.

# minimal valid argument set for validate_inputs(); individual tests override
# one field to trigger a specific branch.
vi_args <- function(...) {
  utils::modifyList(
    list(
      data = mtcars,
      outcome_name = "mpg",
      outcome_type = "continuous",
      intervention_components = c("gear", "qsec"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 350),
      outcome_goal = 40,
      outcome_goal_intention = "maximize",
      power_goal = NULL,
      power_goal_approach = "unconditional",
      cost_list_of_vectors = list(c(0, 4), c(4, 6))
    ),
    list(...)
  )
}

# call the validator with the merged args, silencing the informational
# messages (individual message/warning tests call it directly instead).
call_vi <- function(...) {
  suppressMessages(do.call(LAGOtrials:::validate_inputs, vi_args(...)))
}


# --- Group 1: data, outcome, family, link -----------------------------------

test_that("NULL data is rejected", {
  args <- vi_args()
  args["data"] <- list(NULL)
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, args)),
               "The argument 'data' is NULL")
})

test_that("non-data.frame data is rejected", {
  expect_error(call_vi(data = 1:10), "The argument 'data' must be a data frame")
})

test_that("empty data (0 rows/cols) is rejected", {
  args <- vi_args()
  args$data <- data.frame()
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, args)),
               "The argument 'data' is empty")
})

test_that("non-character input_data_structure is rejected", {
  expect_error(call_vi(input_data_structure = 1), "input_data_structure must be a character string")
})

test_that("unsupported input_data_structure is rejected", {
  expect_error(call_vi(input_data_structure = "foo"), "must be either 'individual_level'")
})

test_that("non-character outcome_name is rejected", {
  expect_error(call_vi(outcome_name = 1), "The outcome name must be a character string")
})

test_that("empty outcome_name is rejected", {
  expect_error(call_vi(outcome_name = ""), "The outcome name is empty")
})

test_that("outcome_name not a column is rejected", {
  expect_error(call_vi(outcome_name = "not_a_col"), "must be presented in the provided input data")
})

test_that("non-character outcome_type is rejected", {
  expect_error(call_vi(outcome_type = 1), "The outcome type must be a character string")
})

test_that("unsupported outcome_type is rejected", {
  expect_error(call_vi(outcome_type = "count"), "must be either 'continuous' or 'binary'")
})

test_that("center_level with continuous outcome is rejected", {
  expect_error(
    call_vi(input_data_structure = "center_level"),
    "LAGO requires individual"
  )
})

test_that("center_level missing required columns is rejected", {
  # assign $data on the built arg list: passing a non-superset frame through
  # call_vi()/modifyList() would merge it column-wise into mtcars rather than
  # replace it.
  a <- vi_args(outcome_name = "y", outcome_type = "binary",
    input_data_structure = "center_level")
  a$data <- data.frame(
    y = c(0, 1, 1, 0), gear = c(3, 4, 3, 5), qsec = c(16, 17, 18, 19)
  )
  expect_error(
    suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "The following required columns are missing"
  )
})

test_that("non-character glm_family is rejected", {
  expect_error(call_vi(glm_family = 1), "The glm family must be a character string")
})

test_that("non-character link is rejected", {
  expect_error(call_vi(link = 1), "The link option must be a character string")
})

test_that("unsupported link value is rejected", {
  expect_error(call_vi(link = "cloglog"), "The link option has to be one of")
})


# --- Group 2: weights, center characteristics, center effects ---------------

test_that("weights must be numeric", {
  expect_error(call_vi(weights = "a"),
    "The 'weights' option must be a numeric vector")
})

test_that("weights length must match number of observations", {
  expect_error(call_vi(weights = c(1, 2, 3)),
    "length of the weights must be the same")
})

test_that("center_characteristics must be a character vector", {
  expect_error(call_vi(center_characteristics = c(1, 2)),
    "center_characteristics must be a character vector")
})

test_that("center_characteristics must all be columns in the data", {
  expect_error(call_vi(center_characteristics = "not_a_col"),
    "All elements in center_characteristics")
})

test_that("center_characteristics_optimization_values must be numeric", {
  expect_error(
    call_vi(center_characteristics = "hp",
      center_characteristics_optimization_values = "x"),
    "center_characteristics_optimization_values")
})

test_that("center_characteristics and its optimization values must match length", {
  expect_error(
    call_vi(center_characteristics = c("hp", "wt"),
      center_characteristics_optimization_values = c(1)),
    "length of center_characteristics must be the")
})

test_that("include_center_effects must be a boolean", {
  expect_error(call_vi(include_center_effects = "yes"),
    "include_center_effects indicator must be a boolean")
})

test_that("center effects and center characteristics cannot both be set", {
  expect_error(
    call_vi(include_center_effects = TRUE,
      center_characteristics = "hp",
      center_characteristics_optimization_values = 1),
    "Fixed center effects and center characteristics cannot be")
})

test_that("include_center_effects TRUE requires a 'center' column", {
  expect_error(call_vi(include_center_effects = TRUE),
    "'center'")
})

test_that("non-factor 'center' column triggers a conversion message", {
  # 'center' is numeric (not a factor) here; modifyList would merge data frames,
  # so set $data on the built arg list directly.
  a <- vi_args(include_center_effects = TRUE)
  a$data <- data.frame(
    mpg = c(21, 22, 23, 24, 25, 26),
    gear = c(3, 4, 3, 4, 3, 4),
    qsec = c(16, 17, 18, 19, 20, 21),
    center = c(1, 1, 2, 2, 3, 3)
  )
  expect_message(do.call(LAGOtrials:::validate_inputs, a),
    "'center' column is not a factor type")
})

test_that("center_weights_for_outcome_goal requires include_center_effects TRUE", {
  expect_error(
    call_vi(center_weights_for_outcome_goal = c(0.5, 0.5)),
    "'include_center_effects' must be set to TRUE")
})

test_that("center_weights_for_outcome_goal must be numeric", {
  a <- vi_args(include_center_effects = TRUE,
    center_weights_for_outcome_goal = "a")
  a$data <- data.frame(
    mpg = c(21, 22, 23, 24, 25, 26),
    gear = c(3, 4, 3, 4, 3, 4),
    qsec = c(16, 17, 18, 19, 20, 21),
    center = factor(c(1, 1, 2, 2, 3, 3))
  )
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "center_weights_for_outcome_goal must be a numeric vector")
})

test_that("center_weights_for_outcome_goal length must match facility count", {
  a <- vi_args(include_center_effects = TRUE,
    center_weights_for_outcome_goal = c(0.5, 0.5))
  a$data <- data.frame(
    mpg = c(21, 22, 23, 24, 25, 26),
    gear = c(3, 4, 3, 4, 3, 4),
    qsec = c(16, 17, 18, 19, 20, 21),
    center = factor(c(1, 1, 2, 2, 3, 3))
  )
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "center_weights_for_outcome_goal must match")
})

test_that("center_effects_optimization_values must be a character", {
  a <- vi_args(include_center_effects = TRUE,
    center_effects_optimization_values = 1)
  a$data <- data.frame(
    mpg = c(21, 22, 23, 24, 25, 26),
    gear = c(3, 4, 3, 4, 3, 4),
    qsec = c(16, 17, 18, 19, 20, 21),
    center = factor(c(1, 1, 2, 2, 3, 3))
  )
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "center_effects_optimization_values")
})

test_that("center_effects_optimization_values must be a single value", {
  a <- vi_args(include_center_effects = TRUE,
    center_effects_optimization_values = c("1", "2"))
  a$data <- data.frame(
    mpg = c(21, 22, 23, 24, 25, 26),
    gear = c(3, 4, 3, 4, 3, 4),
    qsec = c(16, 17, 18, 19, 20, 21),
    center = factor(c(1, 1, 2, 2, 3, 3))
  )
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "must be a single value")
})

test_that("center_effects_optimization_values must be an existing center", {
  a <- vi_args(include_center_effects = TRUE,
    center_effects_optimization_values = "99")
  a$data <- data.frame(
    mpg = c(21, 22, 23, 24, 25, 26),
    gear = c(3, 4, 3, 4, 3, 4),
    qsec = c(16, 17, 18, 19, 20, 21),
    center = factor(c(1, 1, 2, 2, 3, 3))
  )
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "must be one of the centers")
})

test_that("center_weights_for_outcome_goal must sum to 1", {
  a <- vi_args(include_center_effects = TRUE,
    center_weights_for_outcome_goal = c(0.5, 0.5, 0.5))
  a$data <- data.frame(
    mpg = c(21, 22, 23, 24, 25, 26),
    gear = c(3, 4, 3, 4, 3, 4),
    qsec = c(16, 17, 18, 19, 20, 21),
    center = factor(c(1, 1, 2, 2, 3, 3))
  )
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "sum up to 1")
})

test_that("center_weights_for_outcome_goal must be non-negative and finite", {
  # Summing to 1 does not make a vector a set of weights. The weights are a
  # convex combination over the centers -- the reported outcome is
  # sum(weight_i * outcome_i), a weighted MEAN of the per-center outcomes -- so
  # a negative weight lets the result leave the range of the values it averages.
  # c(-10, 11, 0) sums to exactly 1, passed every check, and produced a logit
  # "probability" of 10.95 for a binary outcome: a wrong number, silently.
  cw <- function(w) {
    a <- vi_args(include_center_effects = TRUE,
      center_weights_for_outcome_goal = w)
    a$data <- data.frame(
      mpg = c(21, 22, 23, 24, 25, 26),
      gear = c(3, 4, 3, 4, 3, 4),
      qsec = c(16, 17, 18, 19, 20, 21),
      center = factor(c(1, 1, 2, 2, 3, 3))
    )
    suppressMessages(do.call(LAGOtrials:::validate_inputs, a))
  }

  # the wild case, summing to EXACTLY 1 so the sum check cannot catch it
  expect_identical(sum(c(-10, 11, 0)), 1)
  expect_error(cw(c(-10, 11, 0)), "must be non-negative")
  # and a mild one, also summing to 1
  expect_error(cw(c(-0.05, 0.55, 0.5)), "must be non-negative")

  # a residual weight, written the way a caller writes it, is ALLOWED even
  # though it lands a few floating-point units below zero. R evaluates
  # 1 - .3 - .3 - .4 left to right and the result is -5.55e-17, while
  # 1 - sum(c(.3, .3, .4)) folds the addends first and gives exactly 0, so only
  # the sequential form reaches this at all. The vector still sums to exactly 1.
  # Refusing it would contradict allowing a weight of exactly 0 two assertions
  # below, which is why the check compares against a tolerance.
  # The fixture above has three centers. Which literals produce a negative
  # residual depends on how R folds them, so the value is stated outright
  # rather than computed here: one unit in the last place below zero, which is
  # what a caller's subtraction can land on.
  residual <- -.Machine$double.eps / 2
  expect_lt(residual, 0)
  # accepted rather than returned unchanged: the vector sums to a hair under 1,
  # so the renormalisation rescales it, which is its job. What this asserts is
  # that the sign check does not refuse it.
  expect_equal(
    cw(c(0.5, 0.5, residual))$center_weights_for_outcome_goal,
    c(0.5, 0.5, residual),
    tolerance = 1e-12
  )
  # a weight negative by more than rounding is still refused, so the tolerance
  # admits floating-point noise and not a weight the caller meant
  expect_error(cw(c(0.5, 0.51, -0.01)), "must be non-negative")

  # and the boundary itself, so the gate cannot be widened without a test
  # failing. Asserting only the two cases above leaves every size in between
  # unconstrained: a gate of -1e-3 would satisfy them both while admitting
  # weights that are negative by far more than any subtraction explains.
  # Anything past a few units in the last place of 1 is refused.
  expect_error(cw(c(0.5, 0.5, -1e-9)), "must be non-negative")
  expect_error(cw(c(0.5, 0.5, -1e-12)), "must be non-negative")

  # a weight of exactly 0 is ALLOWED, deliberately: it means a center the
  # recommendation is not being computed for, which is what the package itself
  # builds from center_effects_optimization_values (the named center gets 1 and
  # every other center 0). Refusing 0 would refuse that documented path.
  expect_identical(
    cw(c(0.5, 0.5, 0))$center_weights_for_outcome_goal,
    c(0.5, 0.5, 0)
  )
  expect_identical(
    cw(c(1, 0, 0))$center_weights_for_outcome_goal,
    c(1, 0, 0)
  )

  # NA, NaN and Inf are named rather than reaching the sum comparison, where
  # any(NA < 0) is not FALSE but the base-R error "missing value where
  # TRUE/FALSE needed" -- which said nothing about weights.
  for (bad in list(c(NA_real_, 0.5, 0.5), c(NaN, 0.5, 0.5),
    c(Inf, 0.5, 0.5), c(Inf, -Inf, 1))) {
    expect_error(cw(bad), "must all be finite")
  }
  expect_error(cw(c(NA_real_, 0.5, 0.5)), "not weights")

  # A factor whose levels look like numbers reaches neither of those checks:
  # is.finite() is TRUE for every level, so it passes, and the comparison that
  # follows gives NA and produces the same opaque base-R error this guard
  # exists to replace. It is refused for what it is instead.
  expect_true(all(is.finite(factor(c("0.5", "0.5")))))
  expect_error(
    cw(factor(c("0.5", "0.25", "0.25"))), "must be a numeric vector"
  )
  expect_error(cw(c("0.5", "0.25", "0.25")), "must be a numeric vector")

  # all-zero weights are refused by the sum check, which is also what keeps the
  # renormalisation from dividing by zero: a vector summing to 0 is 1 away from
  # 1, not within the 0.001 tolerance of it.
  expect_error(cw(c(0, 0, 0)), "sum up to 1")

  # a compliant vector is still returned untouched, so none of the above
  # narrowed what is accepted
  expect_identical(
    cw(c(0.25, 0.25, 0.5))$center_weights_for_outcome_goal,
    c(0.25, 0.25, 0.5)
  )
})

test_that("the weight guard is one function, shared with the confidence set", {
  # The finiteness and non-negativity checks are a helper both entry points
  # call, not a copy in each. That matters because the exported
  # get_confidence_set() does not go through validate_inputs() and had NO weight
  # checks at all: a negative weight reached its interval and reported a
  # "probability" above 1. Two copies would let the two doors drift apart, which
  # is the failure mode this pins -- the same vector must be refused in the same
  # words wherever it is passed.
  refuse_invalid_center_weights <- getFromNamespace(
    "refuse_invalid_center_weights", "LAGOtrials"
  )

  # what it refuses, and what it deliberately does not
  expect_error(refuse_invalid_center_weights(c(-10, 11, 0)),
    "must be non-negative")
  expect_error(refuse_invalid_center_weights(c(0.5, -0.01, 0.51)),
    "must be non-negative")
  expect_error(refuse_invalid_center_weights(c(0.5, -1e-9, 0.5)),
    "must be non-negative")
  expect_error(refuse_invalid_center_weights(c(NA_real_, 0.5, 0.5)),
    "must all be finite")
  expect_error(refuse_invalid_center_weights(c(Inf, -Inf, 1)),
    "must all be finite")
  expect_silent(refuse_invalid_center_weights(c(0.5, 0.5, 0)))
  expect_null(refuse_invalid_center_weights(c(1, 0, 0)))
  expect_silent(refuse_invalid_center_weights(c(0.5, -.Machine$double.eps / 2,
    0.5)))

  # the SUM check is NOT in the helper, deliberately: validate_inputs() both
  # refuses a sum far from 1 and renormalises what it accepts, and the
  # renormalisation is what makes that tolerance safe. get_confidence_set() has
  # no such step, so requiring a unit sum of it would refuse the vector its own
  # caller already normalised. All-zero weights are therefore accepted by the
  # helper and refused by validate_inputs().
  expect_silent(refuse_invalid_center_weights(c(0, 0, 0)))
  expect_silent(refuse_invalid_center_weights(c(0.3, 0.3, 0.3)))

  # and validate_inputs() really routes through it rather than carrying its own
  # copy: the messages are identical strings, not merely both matching a regex.
  cw <- function(w) {
    a <- vi_args(include_center_effects = TRUE,
      center_weights_for_outcome_goal = w)
    a$data <- data.frame(
      mpg = c(21, 22, 23, 24, 25, 26),
      gear = c(3, 4, 3, 4, 3, 4),
      qsec = c(16, 17, 18, 19, 20, 21),
      center = factor(c(1, 1, 2, 2, 3, 3))
    )
    suppressMessages(do.call(LAGOtrials:::validate_inputs, a))
  }
  for (bad in list(c(-10, 11, 0), c(NA_real_, 0.5, 0.5))) {
    expect_identical(
      tryCatch(cw(bad), error = conditionMessage),
      tryCatch(refuse_invalid_center_weights(bad), error = conditionMessage)
    )
  }
})

test_that("the weight checks cover the DEFAULT weights, not just supplied", {
  # The checks are placed after every branch that can produce the weights, not
  # inside the one that reads them from the caller, so they cover the
  # center-size default and the single-named-center indicator as well. That
  # matters: the default is center_sizes / total_sample_size, which with every
  # center_sample_size zero is 0/0, i.e. all NaN, and it then reached the sum
  # comparison as the opaque "missing value where TRUE/FALSE needed".
  a <- vi_args(include_center_effects = TRUE, outcome_type = "binary",
    glm_family = "binomial", input_data_structure = "center_level",
    outcome_name = "proportion", intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 10),
    outcome_goal = 0.6)
  a$data <- data.frame(
    proportion = c(0.4, 0.5, 0.6, 0.7),
    center_sample_size = c(0, 0, 0, 0),
    center = factor(c("a", "b", "c", "d")),
    x1 = c(1, 2, 3, 4), x2 = c(2, 3, 4, 5)
  )
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "must all be finite")
  # and the message says where an unsupplied set of weights came from, since the
  # caller passed none and would otherwise have nothing to go on
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "center_sample_size")

  # with real sample sizes the same call is accepted and the default weights are
  # the sample-size shares, so the guard did not break the path it guards.
  # Compared as values: on the center_level path the default comes from tapply()
  # and so carries that function's 1-d array shape, which is pre-existing and
  # not what this test is about.
  a$data$center_sample_size <- c(10, 20, 30, 40)
  expect_identical(
    as.numeric(suppressMessages(do.call(LAGOtrials:::validate_inputs,
      a))$center_weights_for_outcome_goal),
    c(0.1, 0.2, 0.3, 0.4)
  )
})


# --- Group 3: time effects, interaction terms, additional covariates --------

test_that("include_time_effects must be logical", {
  expect_error(call_vi(include_time_effects = "yes"),
    "'include_time_effects' must be a boolean")
})

test_that("unsupported glm_family reaches the switch default", {
  expect_error(call_vi(glm_family = "poisson"),
    "Unsupported glm_family: poisson")
})

test_that("include_time_effects TRUE requires a 'period' column", {
  expect_error(call_vi(include_time_effects = TRUE),
    "input data has a 'period'")
})

test_that("time_effect_optimization_value must be a single numeric when time effects on", {
  dt <- mtcars
  dt$period <- rep(1:2, length.out = nrow(mtcars))
  expect_error(
    call_vi(data = dt, include_time_effects = TRUE,
            time_effect_optimization_value = NULL),
    "'time_effect_optimization_value' must be provided")
  expect_error(
    call_vi(data = dt, include_time_effects = TRUE,
            time_effect_optimization_value = "a"),
    "'time_effect_optimization_value' must be provided")
  expect_error(
    call_vi(data = dt, include_time_effects = TRUE,
            time_effect_optimization_value = c(1, 2)),
    "'time_effect_optimization_value' must be provided")
})

test_that("non-factor 'period' column triggers a conversion message", {
  dt <- mtcars
  dt$period <- rep(1:2, length.out = nrow(mtcars))
  expect_message(
    do.call(LAGOtrials:::validate_inputs, vi_args(
      data = dt, include_time_effects = TRUE,
      time_effect_optimization_value = 1)),
    "'period' column is not a factor type")
})

test_that("interaction terms on but no ':' in intervention_components", {
  expect_error(call_vi(include_interaction_terms = TRUE),
    "interaction terms are included as part of")
})

test_that("main_components must be defined when interaction terms included", {
  di <- mtcars
  di[["gear:qsec"]] <- di$gear * di$qsec
  expect_error(
    call_vi(data = di, include_interaction_terms = TRUE,
            intervention_components = c("gear","qsec","gear:qsec"),
            main_components = NULL),
    "'main_components' must be defined as a character vector")
})

test_that("main_components must be a character vector", {
  di <- mtcars
  di[["gear:qsec"]] <- di$gear * di$qsec
  expect_error(
    call_vi(data = di, include_interaction_terms = TRUE,
            intervention_components = c("gear","qsec","gear:qsec"),
            main_components = c(1, 2)),
    "'main_components' must be a character vector")
})

test_that("main_components must not contain ':'", {
  di <- mtcars
  di[["gear:qsec"]] <- di$gear * di$qsec
  expect_error(
    call_vi(data = di, include_interaction_terms = TRUE,
            intervention_components = c("gear","qsec","gear:qsec"),
            main_components = c("gear","gear:qsec")),
    "Interaction terms should be included as part of")
})

test_that("grid_search without step size requires main_components to be columns", {
  di <- mtcars
  di[["gear:qsec"]] <- di$gear * di$qsec
  expect_error(
    call_vi(data = di, include_interaction_terms = TRUE,
            optimization_method = "grid_search",
            intervention_components = c("gear","qsec","gear:qsec"),
            main_components = c("gear","qsec","foobar")),
    "all elements in the main_components")
})

test_that("interaction parts must be present in main_components", {
  di <- mtcars
  di[["gear:drat"]] <- di$gear * di$drat
  expect_error(
    call_vi(data = di, include_interaction_terms = TRUE,
            intervention_components = c("gear","qsec","gear:drat"),
            main_components = c("gear","qsec")),
    "must be present in 'main_components'")
})

test_that("':' in intervention_components while interaction terms off is rejected", {
  di <- mtcars
  di[["gear:qsec"]] <- di$gear * di$qsec
  expect_error(
    call_vi(data = di, include_interaction_terms = FALSE,
            intervention_components = c("gear","gear:qsec")),
    "':' is found in the intervention_components")
})

test_that("additional_covariates must be a character vector", {
  expect_error(call_vi(additional_covariates = c(1, 2)),
    "additional_covariates must be a character vector")
})

test_that("additional_covariates must all be columns in the data", {
  expect_error(call_vi(additional_covariates = c("foobar")),
    "All elements in additional_covariates")
})

# An additional covariate whose column is entirely NA cannot enter the model:
# glm()'s internal na.omit drops every row and the fit dies with an opaque
# error that never names the column. This is always a mistake -- unlike a
# collinear covariate, which glm() can drop and still fit -- so it is refused
# up front here, naming the offender(s). The tests below pin that the refusal
# fires and names the covariate, catches an all-NA column of any type, and
# stays silent on a partially-observed or fully-observed covariate.

test_that("an all-NA additional covariate is refused, naming it", {
  di <- mtcars
  di$allna <- NA_real_
  # precondition: the column really is entirely NA, so the test is not vacuous
  expect_true(all(is.na(di$allna)))
  expect_error(call_vi(data = di, additional_covariates = "allna"),
    "allna")
  expect_error(call_vi(data = di, additional_covariates = "allna"),
    "entirely NA")
})

test_that("two all-NA additional covariates are both named in one error", {
  di <- mtcars
  di$allna1 <- NA_real_
  di$allna2 <- NA_real_
  # precondition: both columns are entirely NA
  expect_true(all(is.na(di$allna1)) && all(is.na(di$allna2)))
  # one stop() listing both offenders together, alongside a valid covariate
  err <- tryCatch(
    call_vi(data = di, additional_covariates = c("allna1", "cyl", "allna2")),
    error = conditionMessage)
  expect_match(err, "allna1")
  expect_match(err, "allna2")
})

test_that("an all-NA factor or character additional covariate is refused", {
  # is.na() works on factor and character columns too, so the check catches an
  # all-NA column of any type and does not itself error on the non-numeric one
  di <- mtcars
  di$allna_fac <- factor(rep(NA_character_, nrow(di)), levels = c("a", "b"))
  expect_true(all(is.na(di$allna_fac)))
  expect_error(call_vi(data = di, additional_covariates = "allna_fac"),
    "allna_fac")

  di2 <- mtcars
  di2$allna_chr <- NA_character_
  expect_true(all(is.na(di2$allna_chr)))
  expect_error(call_vi(data = di2, additional_covariates = "allna_chr"),
    "allna_chr")
})

test_that("a covariate with some but not all NA does not trip the refusal", {
  # negative control: the refusal is narrow. A covariate with SOME NAs is a
  # legitimate input glm() can still fit, so this must pass the validator.
  di <- mtcars
  di$some_na <- di$cyl
  di$some_na[1] <- NA
  # precondition: some NA, but not all
  expect_true(anyNA(di$some_na) && !all(is.na(di$some_na)))
  expect_no_error(call_vi(data = di, additional_covariates = "some_na"))
})

test_that("a fully-observed additional covariate does not trip the refusal", {
  # negative control: a covariate with no NA at all passes untouched
  expect_false(anyNA(mtcars$cyl))
  expect_no_error(call_vi(data = mtcars, additional_covariates = "cyl"))
})

test_that("lago_optimization refuses an all-NA covariate with a clear message", {
  # END-TO-END: the same all-NA covariate that dies with an opaque
  # "nonempty numeric vector" error deep in model fitting on the unfixed
  # source is now refused up front by the input validator, so the caller sees
  # a message that names the covariate and says why. Fails on the unfixed
  # source, where the run reaches glm() and returns the cryptic error instead.
  bbp <- as.data.frame(BB_proportions)
  bbp$allna <- NA_real_
  # precondition: the covariate really is entirely NA
  expect_true(all(is.na(bbp$allna)))
  run <- function() {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bbp,
      outcome_name = "EBP_proportions",
      outcome_type = "continuous",
      glm_family = "quasibinomial",
      link = "logit",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      additional_covariates = "allna",
      include_confidence_set = FALSE,
      quiet = TRUE
    )))
  }
  # the new message: names the covariate and says why it cannot enter the model
  expect_error(run(), "allna")
  expect_error(run(), "entirely NA")
  # and NOT the opaque model-fitting errors the missing guard produced. Both
  # assertions fail on the unfixed source, where run() returns exactly these.
  expect_error(run(), "^(?!.*nonempty numeric).*$", perl = TRUE)
  expect_error(run(), "^(?!.*model fitting step).*$", perl = TRUE)
})


# An intervention component whose column is entirely NA has the same problem as
# an all-NA additional covariate: glm()'s internal na.omit drops every row and
# the fit dies with the opaque "Argument mu must be a nonempty numeric vector"
# error that never names the component. It is refused up front here, naming the
# offender(s), by the same mechanism as the covariate refusal above. The tests
# below pin that the refusal fires and names the component, catches an all-NA
# column of any type, and stays silent on a partially-observed component.

test_that("an all-NA intervention component is refused, naming it", {
  di <- mtcars
  di$allna <- NA_real_
  # precondition: the column really is entirely NA, so the test is not vacuous
  expect_true(all(is.na(di$allna)))
  expect_error(
    call_vi(data = di, intervention_components = c("gear", "allna")),
    "allna")
  expect_error(
    call_vi(data = di, intervention_components = c("gear", "allna")),
    "entirely NA")
})

test_that("two all-NA intervention components are both named in one error", {
  di <- mtcars
  di$allna1 <- NA_real_
  di$allna2 <- NA_real_
  # precondition: both columns are entirely NA
  expect_true(all(is.na(di$allna1)) && all(is.na(di$allna2)))
  # one stop() listing both offenders together, alongside a valid component
  err <- tryCatch(
    call_vi(data = di,
            intervention_components = c("allna1", "gear", "allna2"),
            intervention_lower_bounds = c(0, 0, 0),
            intervention_upper_bounds = c(10, 10, 10),
            cost_list_of_vectors = list(c(0, 4), c(4, 6), c(0, 1))),
    error = conditionMessage)
  expect_match(err, "allna1")
  expect_match(err, "allna2")
})

test_that("an all-NA factor or character intervention component is refused", {
  # is.na() works on factor and character columns too, so the check catches an
  # all-NA column of any type and does not itself error on the non-numeric one
  di <- mtcars
  di$allna_fac <- factor(rep(NA_character_, nrow(di)), levels = c("a", "b"))
  expect_true(all(is.na(di$allna_fac)))
  expect_error(
    call_vi(data = di, intervention_components = c("gear", "allna_fac")),
    "allna_fac")

  di2 <- mtcars
  di2$allna_chr <- NA_character_
  expect_true(all(is.na(di2$allna_chr)))
  expect_error(
    call_vi(data = di2, intervention_components = c("gear", "allna_chr")),
    "allna_chr")
})

test_that("an intervention component with some but not all NA is not refused", {
  # negative control: the refusal is narrow. A component with SOME NAs is a
  # legitimate input glm() can still fit, so this must pass the validator.
  di <- mtcars
  di$some_na <- di$gear
  di$some_na[1] <- NA
  # precondition: some NA, but not all
  expect_true(anyNA(di$some_na) && !all(is.na(di$some_na)))
  expect_no_error(
    call_vi(data = di, intervention_components = c("gear", "some_na")))
})

test_that("lago_optimization refuses an all-NA intervention component clearly", {
  # END-TO-END: the same all-NA intervention component that dies with an opaque
  # "Argument mu must be a nonempty numeric vector" error deep in model fitting
  # on the unfixed source is now refused up front by the input validator, so
  # the caller sees a message that names the component and says why. Fails on
  # the unfixed source, where the run reaches glm() and returns the cryptic
  # error instead.
  bbp <- as.data.frame(BB_proportions)
  bbp$allna <- NA_real_
  # precondition: the component really is entirely NA
  expect_true(all(is.na(bbp$allna)))
  run <- function() {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bbp,
      outcome_name = "EBP_proportions",
      outcome_type = "continuous",
      glm_family = "quasibinomial",
      link = "logit",
      intervention_components = c("coaching_updt", "allna"),
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      include_confidence_set = FALSE,
      quiet = TRUE
    )))
  }
  # the new message: names the component and says why it cannot enter the model
  expect_error(run(), "allna")
  expect_error(run(), "entirely NA")
  # and NOT the opaque model-fitting errors the missing guard produced. Both
  # assertions fail on the unfixed source, where run() returns exactly these.
  expect_error(run(), "^(?!.*nonempty numeric).*$", perl = TRUE)
  expect_error(run(), "^(?!.*model fitting step).*$", perl = TRUE)
})


# --- Group 4: bounds, costs, optimization method, grid step -----------------

test_that("intervention_lower_bounds must be numeric", {
  expect_error(call_vi(intervention_lower_bounds = "a"),
    "intervention_lower_bounds must be a numeric vector")
})

test_that("intervention_upper_bounds must be numeric", {
  expect_error(call_vi(intervention_upper_bounds = "a"),
    "intervention_upper_bounds must be a numeric vector")
})

test_that("lower bounds length must match number of components", {
  expect_error(call_vi(intervention_lower_bounds = c(0)),
    "lengths of 'intervention_lower_bounds' and")
})

test_that("lower and upper bounds must have the same length", {
  expect_error(call_vi(intervention_lower_bounds = c(0, 0),
                       intervention_upper_bounds = c(10)),
    "lengths of lower and upper bounds must be the same")
})

test_that("intervention bounds must be non-negative", {
  expect_error(call_vi(intervention_lower_bounds = c(-1, 0)),
    "non-negative values only")
})

test_that("upper bound must be >= lower bound", {
  expect_error(call_vi(intervention_lower_bounds = c(5, 5),
                       intervention_upper_bounds = c(3, 350)),
    "Invalid bounds at position")
})

test_that("lower bound above data minimum warns", {
  expect_warning(
    do.call(LAGOtrials:::validate_inputs, vi_args(glm_family = "gaussian", quiet = TRUE,
      intervention_lower_bounds = c(4, 0),
      intervention_upper_bounds = c(10, 350))),
    "greater than the minimum value in the data")
})

test_that("upper bound below data maximum warns", {
  expect_warning(
    do.call(LAGOtrials:::validate_inputs, vi_args(glm_family = "gaussian", quiet = TRUE,
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 20))),
    "less than the maximum value in the data")
})

test_that("unit_costs must be numeric", {
  expect_error(call_vi(unit_costs = "a"),
    "unit_costs must be a numeric vector")
})

test_that("unit_costs length must match number of components", {
  expect_error(call_vi(unit_costs = c(1, 1, 1)),
    "lengths of 'unit_costs' and")
})

test_that("default_cost_fxn_type must be a character string", {
  expect_error(call_vi(unit_costs = c(1, 1), default_cost_fxn_type = 42),
    "default_cost_fxn_type must be a character string")
})

test_that("default_cost_fxn_type must be a supported type", {
  expect_error(call_vi(unit_costs = c(1, 1), default_cost_fxn_type = "quadratic"),
    "default_cost_fxn_type must be one of the supported types")
})

test_that("cost_list_of_vectors must be a list", {
  expect_error(call_vi(cost_list_of_vectors = c(1, 2)),
    "cost_list_of_vectors must be a list")
})

test_that("cost_list_of_vectors sublists must all be numeric", {
  args <- vi_args()
  args$cost_list_of_vectors <- list(c(0, 4), c("a", "b"))
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, args)),
    "sublists of cost_list_of_vectors")
})

test_that("cost_list_of_vectors length must match number of components", {
  args <- vi_args()
  args$cost_list_of_vectors <- list(c(0, 4))
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, args)),
    "lengths of 'cost_list_of_vectors' and")
})

test_that("optimization_method must be a character string", {
  expect_error(call_vi(optimization_method = 5),
    "optimization_method must be a character string")
})

test_that("optimization_method must be a supported method", {
  expect_error(call_vi(optimization_method = "foo"),
    "optimization_method must be one of the supported methods")
})

test_that("optimization_grid_search_step_size must be numeric", {
  expect_error(call_vi(optimization_grid_search_step_size = "a"),
    "optimization_grid_search_step_size must be a numeric vector")
})

test_that("grid search step size length must match number of components", {
  expect_error(call_vi(optimization_grid_search_step_size = c(1)),
    "number of step sizes provided for the grid search")
})

test_that("grid search step sizes must be finite and positive", {
  expect_error(call_vi(optimization_grid_search_step_size = c(1, -1)),
    "finite and greater than 0")
})

test_that("providing step size switches numerical to grid_search", {
  expect_message(
    do.call(LAGOtrials:::validate_inputs, vi_args(glm_family = "gaussian", quiet = TRUE,
      optimization_grid_search_step_size = c(1, 1))),
    "has been switched to 'grid_search'")
})

test_that("grid_search with more than 3 components warns via message", {
  args <- vi_args(glm_family = "gaussian", quiet = TRUE,
                  optimization_method = "grid_search",
                  intervention_components = c("gear", "qsec", "cyl", "hp"),
                  intervention_lower_bounds = c(0, 0, 0, 0),
                  intervention_upper_bounds = c(10, 350, 10, 400))
  args$cost_list_of_vectors <- list(c(0, 4), c(4, 6), c(0, 4), c(4, 6))
  expect_message(do.call(LAGOtrials:::validate_inputs, args),
    "more than 3 intervention components")
})


# --- Group 5: goals, confidence set, power goal, icc, approach --------------

# group-local helpers for power-goal / icc cases: build a binary data frame with
# a 'group' column and a cluster-id column with >=2 centers per arm. These do NOT
# use the shared vi_args() because modifyList() recurses into a data frame (which
# is a list) and would MERGE columns rather than replace the whole frame.
g5_pwr_data <- function() data.frame(
  y = rep(c(0, 1), 6),
  x1 = rep(c(1, 2, 3, 4, 5, 6), 2),
  x2 = rep(c(2, 3, 4, 5, 6, 7), 2),
  group = rep(c("treatment", "control"), 6),
  cid = rep(c("a", "b", "c", "d"), 3)
)
g5_pwr_call <- function(...) {
  overrides <- list(...)
  # pull data out first: modifyList recurses into data frames (they are lists)
  # and would merge columns rather than replace the frame.
  new_data <- overrides$data
  overrides$data <- NULL
  args <- utils::modifyList(list(
    data = if (is.null(new_data)) g5_pwr_data() else new_data,
    outcome_name = "y", outcome_type = "binary",
    glm_family = "binomial", intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 10),
    outcome_goal = NULL, outcome_goal_intention = "maximize",
    power_goal = 0.8, power_goal_approach = "unconditional",
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 20),
    overrides)
  suppressMessages(do.call(LAGOtrials:::validate_inputs, args))
}

test_that("outcome_goal must be numeric", {
  expect_error(call_vi(outcome_goal = "high"),
    "outcome goal must be a numeric value")
})

test_that("outcome_goal_intention must be a character string", {
  expect_error(call_vi(outcome_goal_intention = 5),
    "outcome_goal_intention must be a character string")
})

test_that("outcome_goal_intention must be a supported intention", {
  expect_error(call_vi(outcome_goal_intention = "foo"),
    "of the supported intentions")
})

test_that("minimize intention with goal above the mean warns", {
  expect_warning(call_vi(outcome_goal_intention = "minimize", outcome_goal = 40),
    "to minimize the outcome")
})

test_that("maximize intention with goal at or below the mean warns", {
  expect_warning(call_vi(outcome_goal_intention = "maximize", outcome_goal = 10),
    "to maximize the outcome")
})

test_that("include_confidence_set must be a boolean", {
  expect_error(call_vi(include_confidence_set = "yes"),
    "include_confidence_set indicator must be a boolean")
})

test_that("confidence_set_grid_step_size must be numeric", {
  expect_error(
    call_vi(include_confidence_set = TRUE, confidence_set_grid_step_size = "x"),
    "confidence_set_grid_step_size must be a numeric vector")
})

test_that("confidence_set_grid_step_size length must match components", {
  expect_error(
    call_vi(include_confidence_set = TRUE, confidence_set_grid_step_size = c(1)),
    "confidence set calculation")
})

test_that("confidence_set_alpha must be numeric", {
  expect_error(call_vi(confidence_set_alpha = "x"),
    "confidence_set_alpha must be a numeric value")
})

test_that("confidence_set_alpha must be between 0 and 1", {
  expect_error(call_vi(confidence_set_alpha = 1.5),
    "confidence_set_alpha must be between 0 and 1")
})

test_that("glm_family must match the outcome type", {
  expect_error(call_vi(outcome_type = "binary", glm_family = "gaussian"),
    "is not valid for the outcome type")
})

test_that("prev_recommended_interventions must be numeric", {
  expect_error(call_vi(prev_recommended_interventions = "x"),
    "prev_recommended_interventions must be a numeric vector")
})

test_that("prev_recommended_interventions length must match components", {
  expect_error(call_vi(prev_recommended_interventions = c(1)),
    "length of prev_recommended_interventions")
})

test_that("prev_recommended_interventions must be within the bounds", {
  expect_error(call_vi(prev_recommended_interventions = c(100, 5)),
    "outside the bounds of the intervention component")
})

test_that("shrinkage_threshold must be numeric", {
  expect_error(call_vi(shrinkage_threshold = "x"),
    "shrinkage_threshold must be a numeric value")
})

test_that("shrinkage_threshold must be between 0 and 1", {
  expect_error(call_vi(shrinkage_threshold = 2),
    "shrinkage_threshold must be between 0 and 1")
})

test_that("power_goal must be numeric", {
  expect_error(call_vi(power_goal = "x"),
    "power goal must be a numeric value")
})

test_that("power_goal must be between 0 and 1", {
  expect_error(call_vi(power_goal = 1.5),
    "power goal must be between 0 and 1")
})

test_that("power_goal requires a binary outcome", {
  expect_error(call_vi(power_goal = 0.8),
    "power goal only works with binary outcomes")
})

test_that("power_goal cannot be combined with minimize", {
  expect_error(
    g5_pwr_call(outcome_goal_intention = "minimize"),
    "power goal cannot be combined")
})

test_that("power_goal requires a 'group' column", {
  df <- data.frame(y = c(0, 1, 0, 1), x1 = c(1, 2, 3, 4), x2 = c(2, 3, 4, 5))
  expect_error(g5_pwr_call(data = df), "there is a 'group' column")
})

test_that("power_goal 'group' column must be treatment or control", {
  df <- data.frame(y = c(0, 1, 0, 1), x1 = c(1, 2, 3, 4), x2 = c(2, 3, 4, 5),
    group = c("treatment", "control", "placebo", "control"))
  expect_error(g5_pwr_call(data = df), "must have values 'treatment'")
})

test_that("power_goal requires num_centers and patients", {
  expect_error(
    g5_pwr_call(num_centers_in_next_stage = NULL,
      patients_per_center_in_next_stage = NULL),
    "for the power goal to work")
})

test_that("num_centers_in_next_stage must be numeric", {
  expect_error(g5_pwr_call(num_centers_in_next_stage = "x"),
    "num_centers_in_next_stage must be a numeric value")
})

test_that("num_centers_in_next_stage must be positive", {
  expect_error(g5_pwr_call(num_centers_in_next_stage = 0),
    "num_centers_in_next_stage must be greater than 0")
})

test_that("patients_per_center_in_next_stage must be numeric", {
  expect_error(g5_pwr_call(patients_per_center_in_next_stage = "x"),
    "patients_per_center_in_next_stage must be a numeric value")
})

test_that("patients_per_center_in_next_stage must be positive", {
  expect_error(g5_pwr_call(patients_per_center_in_next_stage = 0),
    "patients_per_center_in_next_stage must be greater than 0")
})

test_that("icc must be numeric", {
  expect_error(g5_pwr_call(icc = "x"),
    "icc must be a numeric value")
})

test_that("icc must have length 1 or 2", {
  expect_error(g5_pwr_call(icc = c(0.1, 0.2, 0.3)),
    "icc must have length 1")
})

test_that("icc must be in the range 0 to 1", {
  expect_error(g5_pwr_call(icc = 1.2), "icc must be in")
})

test_that("non-zero icc requires power_goal_cluster_id", {
  expect_error(g5_pwr_call(icc = 0.1),
    "requires 'power_goal_cluster_id'")
})

test_that("power_goal_cluster_id must be a single column name", {
  expect_error(g5_pwr_call(icc = 0.1, power_goal_cluster_id = c("a", "b")),
    "power_goal_cluster_id must be a single column name")
})

test_that("power_goal_cluster_id must be a column in the data", {
  expect_error(g5_pwr_call(icc = 0.1, power_goal_cluster_id = "nope"),
    "was not found in the data")
})

test_that("each arm needs at least two distinct centers", {
  df <- data.frame(
    y = c(0, 1, 0, 1, 0, 1),
    x1 = c(1, 2, 3, 4, 5, 6),
    x2 = c(2, 3, 4, 5, 6, 7),
    group = c("treatment", "treatment", "treatment", "control", "control", "control"),
    cid = c("a", "a", "a", "b", "c", "d"))
  expect_error(g5_pwr_call(data = df, icc = 0.1, power_goal_cluster_id = "cid"),
    "fewer than two distinct")
})

test_that("icc without a power_goal emits an ignored message", {
  expect_message(do.call(LAGOtrials:::validate_inputs, vi_args(icc = 0.1)),
    "icc is ignored")
})

test_that("power_goal_approach must be a character string", {
  expect_error(call_vi(power_goal_approach = 5),
    "power_goal_approach must be a character string")
})

test_that("power_goal_approach must be a supported approach", {
  expect_error(call_vi(power_goal_approach = "foo"),
    "supported approaches")
})

# --- Group 6: remaining early-exit rules (both-NULL pairs need the
# args[[.]] <- list(NULL) idiom because modifyList() drops NULL overrides) ----

test_that("intervention_components must be a character vector", {
  expect_error(call_vi(intervention_components = 1:2),
    "Interventions list must be a character vector")
})

test_that("intervention_components must all be columns in the data", {
  expect_error(call_vi(intervention_components = c("gear", "not_a_col")),
    "All elements in intervention_components")
})

test_that("both unit_costs and cost_list_of_vectors NULL is rejected", {
  a <- vi_args()
  a["cost_list_of_vectors"] <- list(NULL)
  a["unit_costs"] <- list(NULL)
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "Both 'unit_costs' and 'cost_list_of_vectors' are NULL")
})

test_that("both outcome_goal and power_goal NULL is rejected", {
  a <- vi_args()
  a["outcome_goal"] <- list(NULL)
  a["power_goal"] <- list(NULL)
  expect_error(suppressMessages(do.call(LAGOtrials:::validate_inputs, a)),
    "Both 'outcome_goal' and 'power_goal' are NULL")
})
