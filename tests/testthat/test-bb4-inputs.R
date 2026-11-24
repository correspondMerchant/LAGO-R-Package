# Round 4 input checks: names on per component vectors, repeated and shared
# columns, string and flag arguments, unusable data and interaction columns.

# the defaults with the given arguments in place, NULL included
bb4_args <- function(defaults, given) {
  defaults[names(given)] <- given
  defaults
}

bb4_run <- function(..., data = mtcars) {
  args <- bb4_args(list(
    outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 23),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 25,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE,
    quiet = TRUE
  ), list(...))
  args$data <- data
  suppressMessages(do.call("lago_optimization", args))
}

bb4_quiet_run <- function(...) suppressWarnings(bb4_run(...))

# validate_inputs() directly, with the arguments lago_optimization() passes
bb4_validate <- function(...) {
  args <- bb4_args(list(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 23),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)), outcome_goal = 25,
    outcome_goal_intention = "maximize", power_goal = NULL,
    power_goal_approach = "conditional", include_confidence_set = FALSE,
    quiet = TRUE
  ), list(...))
  do.call("validate_inputs", args)
}

bb4_binary_data <- function() {
  set.seed(1)
  n <- 600
  d <- data.frame(
    center = rep(paste0("c", 1:6), each = 100),
    period = rep(1:4, length.out = n),
    x1 = stats::runif(n, 0, 10), x2 = stats::runif(n, 0, 5)
  )
  d$vol <- rep(1:6, each = 100)
  d$size <- rep(c(2, 5, 3, 6, 4, 1), each = 100)
  lp <- -1 + 0.15 * d$x1 + 0.2 * d$x2 + 0.1 * d$vol
  d$y <- stats::rbinom(n, 1, stats::plogis(lp))
  d$group <- ifelse(d$center %in% c("c1", "c2", "c3"), "control", "treatment")
  d
}

bb4_binary <- function(..., data = bb4_binary_data()) {
  args <- bb4_args(list(
    outcome_name = "y", outcome_type = "binary",
    intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 5),
    unit_costs = c(1, 2), unit_cost_doses = c(5, 2), outcome_goal = 0.6,
    include_confidence_set = FALSE, quiet = TRUE
  ), list(...))
  args$data <- data
  suppressMessages(suppressWarnings(do.call("lago_optimization", args)))
}

test_that("named bounds are matched to the components by name", {
  plain <- bb4_quiet_run()
  named <- bb4_quiet_run(
    intervention_lower_bounds = c(qsec = 14, gear = 3),
    intervention_upper_bounds = c(qsec = 23, gear = 5)
  )
  expect_equal(named$rec_int, plain$rec_int)
  expect_equal(named$intervention_lower_bounds, c(3, 14))
  expect_error(
    bb4_quiet_run(intervention_lower_bounds = c(a = 3, b = 14)),
    "names of intervention_lower_bounds must be the intervention components, each once: 'gear', 'qsec'"
  )
  expect_error(
    bb4_quiet_run(intervention_upper_bounds = c(gear = 5, gear = 23)),
    "names of intervention_upper_bounds must be"
  )
  expect_error(
    bb4_quiet_run(intervention_upper_bounds = stats::setNames(c(5, 23), c("gear", NA))),
    "names of intervention_upper_bounds must be"
  )
})

test_that("a one dimensional array of bounds is read as a vector", {
  lower <- tapply(c(3, 14), c("gear", "qsec"), sum)
  expect_equal(bb4_quiet_run(intervention_lower_bounds = lower)$rec_int, bb4_quiet_run()$rec_int)
})

test_that("named costs are matched to the components by name", {
  plain <- bb4_quiet_run(
    cost_list_of_vectors = NULL, unit_costs = c(4, 6),
    default_cost_fxn_type = "linear"
  )
  named <- bb4_quiet_run(
    cost_list_of_vectors = NULL, unit_costs = c(qsec = 6, gear = 4),
    default_cost_fxn_type = "linear"
  )
  expect_equal(named$rec_int_cost, plain$rec_int_cost)
  expect_equal(named$rec_int, plain$rec_int)
  listed <- bb4_quiet_run(cost_list_of_vectors = list(qsec = c(4, 6), gear = c(0, 4)))
  expect_equal(listed$rec_int, bb4_quiet_run()$rec_int)
  expect_equal(listed$cost_list_of_vectors, list(c(0, 4), c(4, 6)))
  expect_error(
    bb4_quiet_run(cost_list_of_vectors = list(a = c(0, 4), b = c(4, 6))),
    "names of cost_list_of_vectors must be the intervention components"
  )
  doses <- bb4_quiet_run(
    cost_list_of_vectors = NULL, unit_costs = c(4, 6),
    unit_cost_doses = c(qsec = 20, gear = 4)
  )
  in_order <- bb4_quiet_run(
    cost_list_of_vectors = NULL, unit_costs = c(4, 6),
    unit_cost_doses = c(4, 20)
  )
  expect_equal(doses$cost_list_of_vectors, in_order$cost_list_of_vectors)
})

test_that("named step sizes and previous interventions are matched by name", {
  plain <- bb4_quiet_run(optimization_grid_search_step_size = c(1, 0.5))
  named <- bb4_quiet_run(optimization_grid_search_step_size = c(qsec = 0.5, gear = 1))
  expect_equal(named$rec_int, plain$rec_int)
  expect_error(
    bb4_quiet_run(optimization_grid_search_step_size = c(a = 1, b = 0.5)),
    "names of optimization_grid_search_step_size must be"
  )
  cs_plain <- bb4_validate(
    include_confidence_set = TRUE, confidence_set_grid_step_size = c(1, 0.5)
  )
  cs_named <- bb4_validate(
    include_confidence_set = TRUE,
    confidence_set_grid_step_size = c(qsec = 0.5, gear = 1)
  )
  expect_equal(cs_named$confidence_set_grid_step_size, c(1, 0.5))
  expect_equal(cs_plain$confidence_set_grid_step_size, c(1, 0.5))
  prev <- suppressWarnings(bb4_validate(prev_recommended_interventions = c(qsec = 20, gear = 4)))
  expect_equal(prev$prev_recommended_interventions, c(4, 20))
  expect_error(
    suppressWarnings(bb4_validate(prev_recommended_interventions = c(a = 4, b = 20))),
    "names of prev_recommended_interventions must be"
  )
})

test_that("named center characteristic values are matched by name", {
  plain <- bb4_binary(
    center_characteristics = c("vol", "size"),
    center_characteristics_optimization_values = c(2, 5)
  )
  named <- bb4_binary(
    center_characteristics = c("vol", "size"),
    center_characteristics_optimization_values = c(size = 5, vol = 2)
  )
  expect_equal(named$rec_int, plain$rec_int)
  expect_error(
    bb4_binary(
      center_characteristics = c("vol", "size"),
      center_characteristics_optimization_values = c(a = 5, vol = 2)
    ),
    "names of center_characteristics_optimization_values must be the center characteristics"
  )
})

test_that("a named icc pair is matched to the arms by name", {
  power <- list(
    outcome_goal = NULL, power_goal = 0.8, num_centers_in_next_stage = 6,
    patients_per_center_in_next_stage = 100, power_goal_cluster_id = "center"
  )
  plain <- do.call(bb4_binary, c(power, list(icc = c(0.02, 0.1))))
  named <- do.call(bb4_binary, c(power, list(icc = c(treatment = 0.1, control = 0.02))))
  reversed <- do.call(bb4_binary, c(power, list(icc = c(0.1, 0.02))))
  expect_equal(named$effective_outcome_goal, plain$effective_outcome_goal)
  expect_false(isTRUE(all.equal(reversed$effective_outcome_goal, plain$effective_outcome_goal)))
  expect_error(
    do.call(bb4_binary, c(power, list(icc = c(a = 0.1, control = 0.02)))),
    "names of icc must be the arms, each once: 'control', 'treatment'"
  )
})

test_that("repeated columns and columns in two roles are refused", {
  expect_error(
    bb4_quiet_run(intervention_components = c("gear", "gear")),
    "intervention_components names the column\\(s\\) 'gear' more than once"
  )
  expect_error(
    bb4_binary(
      center_characteristics = c("vol", "vol"),
      center_characteristics_optimization_values = c(2, 2)
    ),
    "center_characteristics names the column\\(s\\) 'vol' more than once"
  )
  expect_error(
    bb4_binary(additional_covariates = c("vol", "vol")),
    "additional_covariates names the column\\(s\\) 'vol' more than once"
  )
  expect_error(
    bb4_binary(
      center_characteristics = "x1", center_characteristics_optimization_values = 2
    ),
    "'x1' is an intervention component and a center characteristic"
  )
  expect_error(
    bb4_binary(
      center_characteristics = "vol", center_characteristics_optimization_values = 2,
      additional_covariates = c("y", "vol")
    ),
    "'y' is the outcome and an additional covariate, and 'vol' is a center characteristic and an additional covariate"
  )
  d <- cbind(mtcars, gear = mtcars$carb)
  expect_error(bb4_quiet_run(data = d), "The data has more than one column named 'gear'")
})

test_that("bounds that are not finite numbers are refused", {
  for (bad in list(c(NA, 14), c(NaN, 14), c(-Inf, 14))) {
    expect_error(
      bb4_quiet_run(intervention_lower_bounds = bad),
      "must be finite numbers, one per component"
    )
  }
  expect_error(
    bb4_quiet_run(intervention_upper_bounds = c(5, Inf)),
    "NA, NaN, Inf and -Inf are not bounds"
  )
})

test_that("data with no usable row is refused before the fit", {
  d <- mtcars
  d$gear[1:16] <- NA
  d$qsec[17:32] <- NA
  expect_error(
    bb4_quiet_run(data = d),
    "No row has every model variable and its glm weight observed.*'gear', 'qsec'\\. Fill"
  )
  w <- rep(1, 32)
  w[!is.na(mtcars$gear)] <- NA
  expect_error(
    bb4_quiet_run(weights = w),
    "values with missing entries are 'weights'\\. Fill"
  )
})

test_that("rows the fit cannot use do not hide the later refusals", {
  # every center is on a row with a missing outcome, so the center effects fail
  d <- bb4_binary_data()
  d$center[!is.na(d$y)] <- NA
  d$center[1:2] <- c("c1", "c2")
  d$y[1:2] <- NA
  expect_error(
    bb4_binary(data = d, include_center_effects = TRUE),
    "No row has every model variable"
  )
  # with center effects the fit names the rows, and the effects
  d <- bb4_binary_data()
  odd <- seq_len(nrow(d)) %% 2 == 1
  d$cov <- ifelse(odd, NA, 1)
  d$cov2 <- ifelse(odd, 1, NA)
  expect_error(
    bb4_binary(
      data = d, include_center_effects = TRUE,
      additional_covariates = c("cov", "cov2")
    ),
    "No row has every model variable and its glm weight \\(center_sample_size"
  )
  expect_error(
    bb4_quiet_run(weights = rep(0, 32)),
    "No row has a positive glm weight"
  )
})

test_that("string arguments must be one string", {
  args <- c(
    "outcome_name", "outcome_type", "glm_family", "link", "input_data_structure",
    "optimization_method", "outcome_goal_intention"
  )
  for (arg in args) {
    for (bad in list(character(0), c("a", "b"), NA_character_)) {
      call_args <- stats::setNames(list(bad), arg)
      expect_error(
        do.call(bb4_quiet_run, call_args),
        paste0("'", arg, "' must be one character string"),
        info = arg
      )
    }
  }
  expect_error(
    bb4_quiet_run(
      cost_list_of_vectors = NULL, unit_costs = c(4, 6),
      default_cost_fxn_type = c("linear", "cubic")
    ),
    "'default_cost_fxn_type' must be one character string, but it is a vector of length 2"
  )
})

test_that("quiet and include_interaction_terms must be TRUE or FALSE", {
  for (bad in list(NA, c(TRUE, FALSE), "yes")) {
    expect_error(bb4_quiet_run(quiet = bad), "'quiet' must be TRUE or FALSE")
    expect_error(
      bb4_quiet_run(include_interaction_terms = bad),
      "'include_interaction_terms' must be TRUE or FALSE"
    )
  }
})

test_that("Inf in a model column is refused", {
  for (column in c("qsec", "mpg")) {
    d <- mtcars
    d[[column]][3] <- Inf
    expect_error(bb4_quiet_run(data = d), paste0("'", column, "' hold Inf or -Inf"))
  }
  d <- mtcars
  d$wt[2] <- -Inf
  expect_error(
    bb4_quiet_run(data = d, additional_covariates = "wt"),
    "'wt' hold Inf or -Inf"
  )
})

test_that("the outcome range is checked for quasibinomial and a continuous logit", {
  d <- mtcars
  d$share <- d$mpg / 40
  args <- list(data = d, outcome_name = "share", outcome_goal = 0.6)
  expect_no_error(do.call(bb4_quiet_run, c(args, glm_family = "quasibinomial")))
  d$share[1] <- 1.5
  args$data <- d
  expect_error(
    do.call(bb4_quiet_run, c(args, glm_family = "quasibinomial")),
    "values outside 0 to 1, such as 1.5. The quasibinomial family"
  )
  d$share[1] <- 0
  args$data <- d
  expect_error(
    do.call(bb4_quiet_run, c(args, link = "logit")),
    "not strictly between 0 and 1, such as 0. A logit link"
  )
  expect_no_error(do.call(bb4_quiet_run, c(args, glm_family = "quasibinomial")))
})

test_that("reserved names are refused as model columns", {
  d <- mtcars
  names(d)[names(d) == "qsec"] <- "..1"
  expect_error(
    bb4_quiet_run(data = d, intervention_components = c("gear", "..1")),
    "'..1' are reserved in R.*'X..1'"
  )
})

test_that("the bound warnings use the rows the model can use", {
  d <- mtcars
  # the only row with gear 3 below the bound has no outcome
  d$gear[1] <- 2
  d$mpg[1] <- NA
  warnings <- character(0)
  withCallingHandlers(
    bb4_run(data = d),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("lower bound for the intervention component gear", warnings)))
  # with interaction terms a main component outside the model is skipped
  d <- mtcars
  d$x1 <- NA_real_
  d[["x1:gear"]] <- 0
  expect_no_error(suppressWarnings(bb4_validate(
    data = d, intervention_components = c("gear", "x1:gear"),
    main_components = c("x1", "gear"), include_interaction_terms = TRUE,
    intervention_lower_bounds = c(0, 3), intervention_upper_bounds = c(1, 5)
  )))
})

test_that("a goal at the observed mean warns for both intentions", {
  goal <- mean(mtcars$mpg)
  expect_warning(bb4_run(outcome_goal = goal), "intention is to maximize")
  expect_warning(
    bb4_run(outcome_goal = goal, outcome_goal_intention = "minimize"),
    "intention is to minimize"
  )
})

test_that("values for effects that are not in the model are ignored with a warning", {
  expect_warning(
    bb4_run(center_effects_optimization_values = "c1"),
    "'center_effects_optimization_values' is ignored, because include_center_effects is FALSE"
  )
  expect_warning(
    bb4_run(time_effect_optimization_value = 1),
    "'time_effect_optimization_value' is ignored, because include_time_effects is FALSE"
  )
  expect_warning(
    bb4_run(center_characteristics_optimization_values = 1),
    "'center_characteristics_optimization_values' is ignored, because no center_characteristics"
  )
})

test_that("an explicit NA period level is selected by NA", {
  d <- bb4_binary_data()
  d$period[d$period == 4] <- NA
  d$period <- addNA(factor(d$period))
  args <- list(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 5),
    unit_costs = c(1, 2), unit_cost_doses = c(5, 2), outcome_goal = 0.6,
    outcome_goal_intention = "maximize", power_goal = NULL,
    power_goal_approach = "conditional", include_time_effects = TRUE,
    include_confidence_set = FALSE, quiet = TRUE
  )
  for (value in list(NA_character_, NA)) {
    out <- suppressMessages(suppressWarnings(
      do.call("validate_inputs", c(args, list(time_effect_optimization_value = value)))
    ))
    expect_identical(out$time_effect_optimization_value, NA_character_)
  }
  d$period <- as.numeric(as.character(d$period))
  args$data <- d
  expect_error(
    suppressMessages(do.call(
      "validate_inputs", c(args, list(time_effect_optimization_value = NA_character_))
    )),
    "NA selects the rows with a missing period only when they are an explicit level"
  )
})

test_that("a date-time period in another time zone names the same period", {
  d <- bb4_binary_data()
  d$period <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") +
    3600 * 24 * 90 * (d$period - 1)
  same <- bb4_binary(
    data = d, include_time_effects = TRUE,
    time_effect_optimization_value = as.POSIXct("2024-06-29 12:00:00", tz = "UTC")
  )
  other <- bb4_binary(
    data = d, include_time_effects = TRUE,
    time_effect_optimization_value = as.POSIXct("2024-06-29 08:00:00", tz = "America/New_York")
  )
  expect_equal(other$rec_int, same$rec_int)
})

test_that("a text, factor or logical column with one value is refused", {
  d <- bb4_binary_data()
  d$one <- "a"
  d$flag <- TRUE
  expect_error(
    bb4_binary(data = d, additional_covariates = "one"),
    "additional covariate 'one' has only the value 'a'.*Remove it from additional_covariates"
  )
  expect_error(
    bb4_binary(
      data = d, center_characteristics = "flag",
      center_characteristics_optimization_values = 1
    ),
    "center characteristic 'flag' has only the value 'TRUE'.*center_characteristics,"
  )
  # a second value only on rows the model cannot use does not count
  d$one[1] <- "b"
  d$y[1] <- NA
  expect_error(
    bb4_binary(data = d, additional_covariates = "one"),
    "has only the value 'a'"
  )
})

test_that("grid notes and messages count the main components", {
  d <- mtcars
  d[["gear:qsec"]] <- d$gear * d$qsec
  d[["gear:wt"]] <- d$gear * d$wt
  notes <- character(0)
  withCallingHandlers(
    suppressWarnings(bb4_validate(
      data = d, intervention_components = c("gear", "qsec", "wt", "gear:qsec"),
      main_components = c("gear", "qsec", "wt"), include_interaction_terms = TRUE,
      intervention_lower_bounds = c(3, 14, 1), intervention_upper_bounds = c(5, 23, 6),
      cost_list_of_vectors = list(c(0, 4), c(4, 6), c(1, 1)),
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(1, 1, 1)
    )),
    message = function(m) {
      notes <<- c(notes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("more than 3 intervention components", notes)))
  expect_error(
    suppressWarnings(bb4_validate(
      data = d, intervention_components = c("gear", "qsec", "gear:qsec"),
      main_components = c("gear", "qsec"), include_interaction_terms = TRUE,
      prev_recommended_interventions = c(4, 20, 80)
    )),
    "must be the same as the number of main components, 2"
  )
})

test_that("cost coefficients must be finite and present", {
  for (bad in list(list(c(0, NA), c(4, 6)), list(numeric(0), c(4, 6)), list(c(0, 4), c(Inf, 6)))) {
    expect_error(
      bb4_quiet_run(cost_list_of_vectors = bad),
      "Each vector in cost_list_of_vectors must hold at least one coefficient"
    )
  }
  expect_error(
    bb4_quiet_run(cost_list_of_vectors = list(list(0, 4), c(4, 6))),
    "must be numeric"
  )
})

test_that("unit_cost_doses that are not used are ignored with a warning", {
  expect_warning(
    bb4_run(unit_cost_doses = c(100, 100)),
    "'unit_cost_doses' is ignored, because the cost functions are given in 'cost_list_of_vectors'"
  )
  expect_warning(
    bb4_run(
      cost_list_of_vectors = NULL, unit_costs = c(4, 6),
      default_cost_fxn_type = "linear", unit_cost_doses = c(100, 100)
    ),
    "'unit_cost_doses' is ignored, because the linear cost"
  )
})

test_that("an interaction column must be the product of its components", {
  d <- mtcars
  d[["gear:qsec"]] <- d$gear * d$qsec
  args <- list(
    data = d, intervention_components = c("gear", "qsec", "gear:qsec"),
    main_components = c("gear", "qsec"), include_interaction_terms = TRUE
  )
  expect_no_error(do.call(bb4_quiet_run, args))
  d[["gear:qsec"]][5] <- d[["gear:qsec"]][5] + 1
  args$data <- d
  expect_error(
    do.call(bb4_quiet_run, args),
    "interaction column 'gear:qsec' must hold the product gear \\* qsec, but in row 5"
  )
  # a difference on a row the model does not use is not a problem
  d$mpg[5] <- NA
  args$data <- d
  expect_no_error(do.call(bb4_quiet_run, args))
  # a product of zero must be zero
  d <- mtcars
  d$zero <- 0
  d[["gear:zero"]] <- 0
  d[["gear:zero"]][3] <- 1e-12
  expect_error(
    bb4_quiet_run(
      data = d, intervention_components = c("gear", "zero", "gear:zero"),
      main_components = c("gear", "zero"), include_interaction_terms = TRUE,
      intervention_lower_bounds = c(3, 0), intervention_upper_bounds = c(5, 1)
    ),
    "must hold the product gear \\* zero, but in row 3"
  )
})

test_that("missing interaction and main component columns are named", {
  expect_error(
    bb4_quiet_run(
      intervention_components = c("gear", "qsec", "gear:qsec"),
      main_components = c("gear", "qsec"), include_interaction_terms = TRUE
    ),
    "interaction term\\(s\\) 'gear:qsec' are not columns.*data\\[\\[\"gear:qsec\"\\]\\] <- data\\$gear \\* data\\$qsec"
  )
  d <- mtcars
  d[["gear:x9"]] <- d$gear
  expect_error(
    bb4_quiet_run(
      data = d, intervention_components = c("gear", "gear:x9"),
      main_components = c("gear", "x9"), include_interaction_terms = TRUE
    ),
    "all elements in the main_components must be columns in the data frame, but 'x9' are not"
  )
  d$x9 <- "a"
  expect_error(
    bb4_quiet_run(
      data = d, intervention_components = c("gear", "gear:x9"),
      main_components = c("gear", "x9"), include_interaction_terms = TRUE
    ),
    "main component\\(s\\) 'x9' must be numeric columns"
  )
  expect_error(
    bb4_quiet_run(
      data = d, intervention_components = c("gear", "gear:x9"),
      main_components = c("gear", "gear"), include_interaction_terms = TRUE
    ),
    "main_components names the column\\(s\\) 'gear' more than once"
  )
  expect_error(
    bb4_quiet_run(intervention_components = c("gear", "x9")),
    "must be columns in the data frame\\.$"
  )
})
