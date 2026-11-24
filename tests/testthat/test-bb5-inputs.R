# Round 5 input checks: names that relate to no component, a named scalar icc,
# an odd number of next stage centers, names the confidence set uses, rounded
# interaction columns, the warning for a goal at the mean, and checks over the
# rows the model fits.

bb5_args <- function(defaults, given) {
  defaults[names(given)] <- given
  defaults
}

bb5_run <- function(..., data = mtcars) {
  args <- bb5_args(list(
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

bb5_quiet_run <- function(...) suppressWarnings(bb5_run(...))

# the messages of the warnings an expression gives, and its value
bb5_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = seen)
}

bb5_ab_data <- function(n = 200) {
  set.seed(1)
  d <- data.frame(a = stats::runif(n, 0, 2), b = stats::runif(n, 0, 2))
  d$y <- 1 + d$a + d$b + 0.5 * d$a * d$b + stats::rnorm(n)
  d
}

bb5_ab <- function(..., data = bb5_ab_data()) {
  bb5_run(
    data = data, outcome_name = "y", intervention_components = c("a", "b"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(2, 2),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 5, ...
  )
}

bb5_interaction <- function(data, ...) {
  bb5_quiet_run(
    data = data, outcome_name = "y",
    intervention_components = c("a", "b", "a:b"), main_components = c("a", "b"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(2, 2), cost_list_of_vectors = NULL,
    unit_costs = c(1, 1), default_cost_fxn_type = "linear", outcome_goal = 5,
    ...
  )
}

bb5_power_data <- function() {
  set.seed(1)
  n <- 400
  d <- data.frame(center = rep(1:20, each = 20), x = stats::runif(n, 0, 10))
  d$group <- rep(c("control", "treatment"), length.out = n)
  d$y <- stats::rbinom(n, 1, stats::plogis(-2 + 0.2 * d$x))
  d$cl <- d$center
  d
}

bb5_power <- function(...) {
  bb5_run(
    data = bb5_power_data(), outcome_name = "y", outcome_type = "binary",
    intervention_components = "x", intervention_lower_bounds = 0,
    intervention_upper_bounds = 10, cost_list_of_vectors = NULL,
    unit_costs = 1, default_cost_fxn_type = "linear", outcome_goal = NULL,
    power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 50, power_goal_cluster_id = "cl", ...
  )
}

test_that("names that relate to no component are ignored, and values are matched by position", {
  plain <- bb5_quiet_run(
    data = mtcars, intervention_components = "qsec", intervention_lower_bounds = 14,
    intervention_upper_bounds = 22, cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = 21
  )
  # quantile() names its one value "95%"
  upper <- stats::setNames(22, "95%")
  from_quantile <- bb5_quiet_run(
    data = mtcars, intervention_components = "qsec", intervention_lower_bounds = 14,
    intervention_upper_bounds = upper, cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = 21
  )
  expect_equal(from_quantile$rec_int, plain$rec_int)
  # a vector where only some values have names, none of them a component
  partly <- bb5_quiet_run(intervention_lower_bounds = c(3, foo = 14))
  expect_equal(partly$rec_int, bb5_quiet_run()$rec_int)
  # a center characteristic value named by quantile()
  set.seed(2)
  d <- mtcars
  d$center <- rep(1:8, each = 4)
  cc <- bb5_quiet_run(
    data = d, center_characteristics = "wt",
    center_characteristics_optimization_values = stats::setNames(3, "50%")
  )
  cc_plain <- bb5_quiet_run(
    data = d, center_characteristics = "wt",
    center_characteristics_optimization_values = 3
  )
  expect_equal(cc$rec_int, cc_plain$rec_int)
})

test_that("names that do refer to the components are still read by name", {
  reversed <- bb5_quiet_run(
    intervention_lower_bounds = c(qsec = 14, gear = 3),
    intervention_upper_bounds = c(qsec = 23, gear = 5)
  )
  expect_equal(reversed$rec_int, bb5_quiet_run()$rec_int)
  # a partial name and a misspelled pair are refused
  expect_error(
    bb5_quiet_run(intervention_lower_bounds = c(gear = 3, 14)),
    "The names of intervention_lower_bounds must be"
  )
  expect_error(
    bb5_quiet_run(intervention_lower_bounds = c(Gear = 3, Qsec = 14)),
    "The names of intervention_lower_bounds must be"
  )
  expect_error(
    bb5_quiet_run(intervention_lower_bounds = c(gear = 3, gear = 14)),
    "each once"
  )
})

test_that("a named icc with one value is refused, as it is shared by both arms", {
  expect_error(
    suppressWarnings(bb5_power(icc = c(treatment = 0.1))),
    "icc has one value, which is shared by both arms, so it cannot be named, but it is named 'treatment'"
  )
  expect_error(
    suppressWarnings(bb5_power(icc = stats::setNames(0.1, NA))),
    "it is named 'NA'"
  )
  shared <- suppressWarnings(bb5_power(icc = 0.1))
  empty_name <- suppressWarnings(bb5_power(icc = stats::setNames(0.1, "")))
  expect_equal(empty_name$rec_int, shared$rec_int)
})

test_that("an odd number of next stage centers is warned about, and 1 is refused", {
  odd <- bb5_warnings(bb5_power(num_centers_in_next_stage = 5))
  expect_true(any(grepl(
    "num_centers_in_next_stage is 5, an odd number. The power calculation splits the next stage evenly between the two arms, so it plans 2.5 centers per arm.",
    odd$warnings,
    fixed = TRUE
  )))
  even <- bb5_warnings(bb5_power(num_centers_in_next_stage = 6))
  expect_false(any(grepl("odd number", even$warnings)))
  expect_error(
    suppressWarnings(bb5_power(num_centers_in_next_stage = 1)),
    "num_centers_in_next_stage must be at least 2"
  )
})

test_that("components and center characteristics cannot take the names of confidence set columns", {
  d <- mtcars
  d$cost <- d$gear
  expect_error(
    bb5_quiet_run(data = d, intervention_components = c("cost", "qsec")),
    "The column name\\(s\\) 'cost' cannot be used for an intervention component"
  )
  d <- bb5_ab_data()
  names(d)[names(d) == "a"] <- "CI_lower_bound"
  d[["CI_lower_bound:b"]] <- d$CI_lower_bound * d$b
  expect_error(
    bb5_quiet_run(
      data = d, outcome_name = "y",
      intervention_components = c("b", "CI_lower_bound:b"),
      main_components = c("CI_lower_bound", "b"),
      include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(2, 2), cost_list_of_vectors = NULL,
      unit_costs = c(1, 1), outcome_goal = 5
    ),
    "'CI_lower_bound' cannot be used"
  )
  d <- mtcars
  d$CI_upper_bound <- d$wt
  expect_error(
    bb5_quiet_run(
      data = d, center_characteristics = "CI_upper_bound",
      center_characteristics_optimization_values = 3
    ),
    "Rename the column\\(s\\) in the data, for example to 'CI_upper_bound_value'"
  )
})

test_that("an interaction column saved with rounding still counts as the product", {
  d <- bb5_ab_data()
  d$a <- round(d$a, 3)
  d$b <- round(d$b, 3)
  d[["a:b"]] <- round(d$a * d$b, 3)
  expect_s3_class(bb5_interaction(d), "lago")
  d[["a:b"]] <- signif(d$a * d$b, 4)
  expect_s3_class(bb5_interaction(d), "lago")
  # a centered product is still refused
  d[["a:b"]] <- (d$a - mean(d$a)) * (d$b - mean(d$b))
  expect_error(bb5_interaction(d), "must hold the product a \\* b")
  # a small product is judged on its own size when the products are large
  d <- bb5_ab_data()
  d$a <- d$a * 1000
  d[["a:b"]] <- d$a * d$b
  d[["a:b"]][which.min(d[["a:b"]])] <- 0.5 * min(d[["a:b"]]) + 1
  expect_error(
    bb5_interaction(d, intervention_upper_bounds = c(2000, 2)),
    "must hold the product a \\* b"
  )
})

test_that("an interaction product of integer columns does not overflow", {
  set.seed(3)
  n <- 200
  d <- data.frame(
    a = sample(40000:60000, n, TRUE), b = sample(40000:60000, n, TRUE)
  )
  d$y <- stats::rnorm(n)
  d[["a:b"]] <- as.numeric(d$a) * as.numeric(d$b)
  out <- bb5_warnings(bb5_interaction(
    d, intervention_upper_bounds = c(70000, 70000), outcome_goal = 1
  ))
  expect_false(any(grepl("overflow", out$warnings)))
  d[["a:b"]][1] <- 5
  expect_error(
    bb5_interaction(d, intervention_upper_bounds = c(70000, 70000), outcome_goal = 1),
    "in row 1 it is 5 while the product is"
  )
})

test_that("the goal at the mean warning is an input warning a sweep shows once", {
  d <- bb5_ab_data()
  goal <- mean(d$y) - 0.1
  out <- bb5_warnings(lago_sensitivity(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("a", "b"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(2, 2), unit_costs = c(1, 1),
    default_cost_fxn_type = "linear", outcome_goal = goal,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE,
    quiet = TRUE, parameter = "cost_multiplier", values = c(1, 2, 3)
  ))
  expect_equal(sum(grepl("is to maximize the outcome", out$warnings)), 1)
  cond <- tryCatch(
    bb5_ab(outcome_goal = goal, outcome_goal_intention = "maximize"),
    warning = function(w) w
  )
  expect_s3_class(cond, "lago_input_warning")
  cond <- tryCatch(
    bb5_ab(outcome_goal = mean(d$y) + 0.1, outcome_goal_intention = "minimize"),
    warning = function(w) w
  )
  expect_s3_class(cond, "lago_input_warning")
  expect_match(conditionMessage(cond), "to minimize the outcome")
  # the warning is still headed by validate_inputs()
  expect_match(paste(deparse(conditionCall(cond)), collapse = ""), "^validate_inputs\\(")
})

test_that("the goal at the mean tolerance is relative to the outcome's size", {
  d <- bb5_ab_data()
  d$y <- d$y * 1e-12
  # twice the mean is far above it, whatever the unit
  out <- bb5_warnings(bb5_ab(data = d, outcome_goal = 2 * mean(d$y)))
  expect_false(any(grepl("specified outcome goal intention", out$warnings)))
  out <- bb5_warnings(bb5_ab(
    data = d, outcome_goal = 0.5 * mean(d$y), outcome_goal_intention = "minimize"
  ))
  expect_false(any(grepl("specified outcome goal intention", out$warnings)))
  # a goal equal to the mean up to round off still counts as at the mean
  out <- bb5_warnings(bb5_ab(data = d, outcome_goal = mean(d$y) * (1 + 1e-12)))
  expect_true(any(grepl("specified outcome goal intention", out$warnings)))
})

test_that("the observed mean is taken over the rows the model fits", {
  set.seed(3)
  n <- 200
  d <- data.frame(a = sample(0:5, n, TRUE), b = sample(0:5, n, TRUE))
  d$y <- 1 + d$a + d$b + stats::rnorm(n)
  d$a[1:40] <- NA
  d$y[1:40] <- 30
  # 7 is above the mean of the fitted rows, about 6, and below the mean of all
  # rows, about 10.7
  out <- bb5_warnings(bb5_run(
    data = d, outcome_name = "y", intervention_components = c("a", "b"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(5, 5),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 7
  ))
  expect_false(any(grepl("specified outcome goal intention", out$warnings)))
  # a row with weight 0 is not fitted, so it does not count either
  d <- bb5_ab_data()
  d$y[1:20] <- 100
  w <- rep(1, nrow(d))
  w[1:20] <- 0
  out <- bb5_warnings(bb5_ab(data = d, weights = w, outcome_goal = 5))
  expect_false(any(grepl("specified outcome goal intention", out$warnings)))
})

test_that("outcome values on rows the model does not fit are not range checked", {
  d <- bb5_ab_data()
  d$yb <- as.numeric(d$y > stats::median(d$y))
  d$yb[1] <- 2
  d$a[1] <- NA
  expect_s3_class(
    bb5_quiet_run(
      data = d, outcome_name = "yb", outcome_type = "binary",
      intervention_components = c("a", "b"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(2, 2),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.6
    ),
    "lago"
  )
  # nor on a row with weight 0
  d$a[1] <- 1
  w <- rep(1, nrow(d))
  w[1] <- 0
  expect_s3_class(
    bb5_quiet_run(
      data = d, outcome_name = "yb", outcome_type = "binary", weights = w,
      intervention_components = c("a", "b"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(2, 2),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.6
    ),
    "lago"
  )
  # on a fitted row it is still refused
  expect_error(
    bb5_quiet_run(
      data = d, outcome_name = "yb", outcome_type = "binary",
      intervention_components = c("a", "b"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(2, 2),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.6
    ),
    "has values outside 0 to 1, such as 2"
  )
  d <- bb5_ab_data()
  d$yp <- stats::plogis(scale(d$y)[, 1])
  d$yp[1] <- 1.5
  d$a[1] <- NA
  expect_s3_class(
    bb5_ab(data = d, outcome_name = "yp", glm_family = "quasibinomial", outcome_goal = 0.6),
    "lago"
  )
  # a gaussian logit outcome of 0 on a row with weight 0
  d <- bb5_ab_data()
  d$yp <- stats::plogis(scale(d$y)[, 1])
  d$yp[1] <- 0
  w <- rep(1, nrow(d))
  w[1] <- 0
  expect_s3_class(
    suppressWarnings(bb5_ab(
      data = d, outcome_name = "yp", link = "logit", weights = w, outcome_goal = 0.7
    )),
    "lago"
  )
  w[1] <- 1
  expect_error(
    suppressWarnings(bb5_ab(
      data = d, outcome_name = "yp", link = "logit", weights = w, outcome_goal = 0.7
    )),
    "not strictly between 0 and 1, such as 0"
  )
})

test_that("Inf is refused only on rows the model keeps", {
  d <- bb5_ab_data()
  d$y[1] <- Inf
  d$a[1] <- NA
  expect_s3_class(bb5_ab(data = d), "lago")
  d <- bb5_ab_data()
  d$b[1] <- Inf
  d$a[1] <- NA
  expect_s3_class(bb5_ab(data = d), "lago")
  # glm() cannot fit a row with Inf even when its weight is 0
  d <- bb5_ab_data()
  d$y[1] <- Inf
  w <- rep(1, nrow(d))
  w[1] <- 0
  expect_error(
    bb5_ab(data = d, weights = w),
    "The column\\(s\\) 'y' hold Inf or -Inf on rows the outcome model uses"
  )
})
