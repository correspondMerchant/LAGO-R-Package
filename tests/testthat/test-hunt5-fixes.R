# Results checked against independent calculations, from a fifth bug hunt.

hunt5_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# the same patients, one row each and aggregated to center level rows
hunt5_center_data <- function() {
  set.seed(5)
  rows <- expand.grid(period = 1:3, center = paste0("c", 1:6), stringsAsFactors = FALSE)
  rows$x1 <- round(stats::runif(nrow(rows), 0, 4), 1)
  rows$x2 <- round(stats::runif(nrow(rows), 0, 3), 1)
  rows$center_sample_size <- sample(c(10, 30, 120), nrow(rows), replace = TRUE)
  rows$events <- stats::rbinom(nrow(rows), rows$center_sample_size,
    stats::plogis(-2 + 0.2 * rows$x1 + 0.3 * rows$x2)
  )
  rows$proportion <- rows$events / rows$center_sample_size
  patients <- rows[rep(seq_len(nrow(rows)), rows$center_sample_size), c("center", "x1", "x2")]
  patients$y <- unlist(lapply(seq_len(nrow(rows)), function(i) {
    rep(1:0, c(rows$events[i], rows$center_sample_size[i] - rows$events[i]))
  }))
  list(rows = rows, patients = patients)
}

hunt5_center_args <- list(
  outcome_type = "binary", intervention_components = c("x1", "x2"),
  intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(4, 3),
  cost_list_of_vectors = list(c(0, 1), c(0, 2)), include_confidence_set = FALSE,
  quiet = TRUE
)

test_that("center level data keeps include_center_effects = FALSE", {
  d <- hunt5_center_data()
  r <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$rows, input_data_structure = "center_level", outcome_name = "proportion",
    include_center_effects = FALSE, outcome_goal = 0.3
  ), hunt5_center_args)))
  expect_false("center" %in% all.vars(stats::formula(r$model)))
  # the same as the patients without center effects
  p <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$patients, outcome_name = "y", outcome_goal = 0.3
  ), hunt5_center_args)))
  expect_equal(r$rec_int, p$rec_int, tolerance = 1e-6)
})

test_that("center level data refuses an outcome other than proportion", {
  d <- hunt5_center_data()
  expect_error(
    do.call(lago_optimization, c(list(
      data = d$rows, input_data_structure = "center_level", outcome_name = "events",
      outcome_goal = 0.3
    ), hunt5_center_args)),
    "outcome_name must be \"proportion\""
  )
})

test_that("the shrinking target weighs center level rows by their patients", {
  d <- hunt5_center_data()
  # an unreachable goal, so the shrinking method shrinks towards stage 1
  rows <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$rows, input_data_structure = "center_level", outcome_name = "proportion",
    include_center_effects = TRUE, outcome_goal = 0.95
  ), hunt5_center_args)))
  patients <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$patients, outcome_name = "y", include_center_effects = TRUE, outcome_goal = 0.95
  ), hunt5_center_args)))
  expect_true(rows$shrinking_method_used)
  expect_equal(rows$rec_int, patients$rec_int, tolerance = 1e-6)
})

# ---- confidence sets ----------------------------------------------------------

test_that("the confidence set grid includes each upper bound", {
  set.seed(3)
  d <- data.frame(x = stats::runif(300, 0.3, 9.4))
  d$y <- 2 + 0.5 * d$x + stats::rnorm(300)
  fit <- stats::glm(y ~ x, data = d)
  goal <- unname(stats::predict(fit, data.frame(x = 10))) - 1e-6
  r <- hunt5_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 10,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = goal,
    confidence_set_grid_step_size = 0.3, quiet = TRUE
  ))
  expect_true(10 %in% r$cs$x)
})

test_that("confidence_set_grid_step_size must be finite and positive", {
  for (step in list(0, -1, NA_real_, Inf)) {
    expect_error(
      lago_optimization(
        data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
        intervention_components = "qsec", intervention_lower_bounds = 0,
        intervention_upper_bounds = 30, cost_list_of_vectors = list(c(0, 1)),
        outcome_goal = 25, confidence_set_grid_step_size = step, quiet = TRUE
      ),
      "finite and positive"
    )
  }
})

test_that("a non gaussian family on the identity link uses glm()'s variance", {
  set.seed(8)
  d <- data.frame(x = stats::runif(800, 0, 10))
  d$y <- stats::rbinom(800, 20, 0.05 + 0.08 * d$x) / 20
  r <- hunt5_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous", glm_family = "quasibinomial",
    link = "identity", intervention_components = "x", intervention_lower_bounds = 0,
    intervention_upper_bounds = 10, cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.5,
    confidence_set_grid_step_size = 0.5, quiet = TRUE
  ))
  table <- LAGOtrials:::lago_coef_table(r)
  expect_equal(table[["Std. Error"]], unname(sqrt(diag(stats::vcov(r$model)))), tolerance = 1e-10)
})

# ---- the optimizer and the power goal --------------------------------------------

test_that("the numerical optimizer does not depend on a component's units", {
  set.seed(3)
  up <- c(1500, 6000, 1, 10)
  u <- sapply(1:4, function(i) stats::runif(1600))
  b <- stats::runif(4, 0.3, 0.8) * 2
  y <- stats::rbinom(1600, 1, stats::plogis(-1 + u %*% b))
  slope <- stats::runif(4, 1, 5)
  run <- function(scale) {
    d <- as.data.frame(sweep(u, 2, scale, `*`))
    names(d) <- paste0("x", 1:4)
    d$y <- y
    hunt5_quiet(lago_optimization(
      data = d, outcome_name = "y", outcome_type = "binary",
      intervention_components = paste0("x", 1:4), intervention_lower_bounds = rep(0, 4),
      intervention_upper_bounds = scale, outcome_goal = 0.7045,
      cost_list_of_vectors = lapply(1:4, function(i) c(1, slope[i] / scale[i])),
      include_confidence_set = FALSE, quiet = TRUE
    ))
  }
  native <- run(up)
  fraction <- run(rep(1, 4))
  expect_equal(native$rec_int_cost, fraction$rec_int_cost, tolerance = 1e-6)
  expect_equal(native$rec_int / up, fraction$rec_int, tolerance = 1e-4)
})

test_that("the unconditional power goal may lie below the stage-1 control rate", {
  d <- data.frame(
    group = rep(c("control", "treatment"), each = 200),
    y = c(rep(1:0, c(60, 140)), rep(1:0, c(70, 130)))
  )
  goal <- LAGOtrials:::get_power_desired_outcome(
    d, 0, 0.8, "unconditional", 100, 10, "y", control_rate = 0.15
  )
  # the power by hand, with the next stage control arm at the planned rate
  n2 <- 500
  power_at <- function(e) {
    p1 <- (70 + n2 * e) / (200 + n2)
    p0 <- (60 + n2 * 0.15) / (200 + n2)
    ncp <- (p1 - p0)^2 / (p1 * (1 - p1) / (200 + n2) + p0 * (1 - p0) / (200 + n2))
    1 - stats::pchisq(stats::qchisq(0.95, 1), 1, ncp = ncp)
  }
  oracle <- stats::uniroot(function(e) power_at(e) - 0.8, c(0.2, 1))$root
  # below the stage-1 control rate of 0.3, within the grid's resolution
  expect_lt(goal, 0.3)
  expect_equal(goal, oracle, tolerance = 2e-3)
})

# ---- sweeps ---------------------------------------------------------------------

test_that("overriding unit_costs in a sweep replaces the stored cost list", {
  opt <- hunt5_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 30, include_confidence_set = FALSE, quiet = TRUE
  ))
  s <- hunt5_quiet(lago_sensitivity(opt,
    unit_costs = c(40, 60), default_cost_fxn_type = "linear",
    parameter = "outcome_goal", values = 35
  ))
  direct <- hunt5_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), unit_costs = c(40, 60),
    default_cost_fxn_type = "linear", outcome_goal = 35,
    include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_equal(s$rec_int_cost, direct$rec_int_cost, tolerance = 1e-5)
})

test_that("a binary budget reaches a rare outcome near 0", {
  set.seed(11)
  d <- data.frame(x = stats::runif(4000, 0, 2))
  d$y <- stats::rbinom(4000, 1, stats::plogis(-4.2 - 0.6 * d$x))
  b <- hunt5_quiet(lago_budget(
    data = d, outcome_name = "y", outcome_type = "binary", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 3,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal_intention = "minimize",
    budget = 3, n_grid = 5
  ))
  # the whole budget buys x = 3, the lowest outcome
  fit <- stats::glm(y ~ x, stats::binomial, d)
  best <- unname(stats::predict(fit, data.frame(x = 3), type = "response"))
  # a relative error, since expect_equal() compares a value this small absolutely
  expect_lt(abs(b$est_outcome - best) / best, 0.05)
})

# ---- costs and printing ------------------------------------------------------------

test_that("the cubic cost handles equal bounds and narrow bounds far from 0", {
  expect_equal(LAGOtrials:::cost_fxn_calculator(2, 2, 3, "cubic")[[1]], c(0, 3, 0, 0, 0))
  coefs <- LAGOtrials:::cost_fxn_calculator(1000, 1100, 5, "cubic")[[1]]
  x <- seq(1000, 1100, length.out = 2001)
  total <- vapply(x, function(v) sum(coefs * v^(0:4)), numeric(1))
  # a valid cost whose cost per unit averages the unit cost over the bounds
  expect_true(all(diff(total) >= 0))
  expect_equal(mean(total / x), 5, tolerance = 1e-3)
})

test_that("the printed cost IQR keeps a cost in small units", {
  r <- hunt5_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 0.0001), c(0, 0.0002)),
    outcome_goal = 40, confidence_set_grid_step_size = c(1, 1), quiet = TRUE
  ))
  utils::capture.output(lines <- cli::cli_fmt(print(r)))
  iqr <- grep("IQR of the cost", lines, value = TRUE)
  expect_length(iqr, 1)
  expect_no_match(iqr, ": 0 - 0", fixed = TRUE)
})

test_that("a sweep keeps the center effects an older center level result used", {
  d <- hunt5_center_data()
  r <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$rows, input_data_structure = "center_level", outcome_name = "proportion",
    outcome_goal = 0.3
  ), hunt5_center_args)))
  # version 1.1.0 stored FALSE for center level data it fitted with center
  # effects, and had no unit_cost_doses argument
  old <- r
  args <- attr(old, "lago_call_args")
  args$include_center_effects <- FALSE
  args$unit_cost_doses <- NULL
  attr(old, "lago_call_args") <- args
  fresh <- hunt5_quiet(lago_sensitivity(r, parameter = "outcome_goal", values = 0.3))
  replay <- hunt5_quiet(lago_sensitivity(old, parameter = "outcome_goal", values = 0.3))
  expect_equal(replay$rec_int_cost, fresh$rec_int_cost, tolerance = 1e-5)
  # and on the same center level data passed again
  same_data <- hunt5_quiet(lago_sensitivity(old,
    data = d$rows, parameter = "outcome_goal", values = 0.3
  ))
  expect_equal(same_data$rec_int_cost, fresh$rec_int_cost, tolerance = 1e-5)
  # an older individual level result swept onto center level data gets the
  # center effects center level data takes by default
  ind <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$patients, outcome_name = "y", outcome_goal = 0.3
  ), hunt5_center_args)))
  args <- attr(ind, "lago_call_args")
  args$include_center_effects <- FALSE
  args$unit_cost_doses <- NULL
  attr(ind, "lago_call_args") <- args
  onto_rows <- hunt5_quiet(lago_sensitivity(ind,
    data = d$rows, input_data_structure = "center_level", outcome_name = "proportion",
    parameter = "outcome_goal", values = 0.3
  ))
  expect_equal(onto_rows$rec_int_cost, fresh$rec_int_cost, tolerance = 1e-5)
})

test_that("center level data with no observed center can turn center effects off", {
  d <- hunt5_center_data()
  rows <- d$rows
  rows$center <- NA
  expect_error(
    do.call(lago_optimization, c(list(
      data = rows, input_data_structure = "center_level", outcome_name = "proportion",
      outcome_goal = 0.3
    ), hunt5_center_args)),
    "or set include_center_effects = FALSE"
  )
})

test_that("the default cubic cost refuses bounds too far from 0 for their width", {
  expect_error(
    LAGOtrials:::cost_fxn_calculator(1e6, 1e6 + 0.01, 3, "cubic"),
    "cannot be computed for bounds this far from 0"
  )
})

test_that("a sweep on other data resolves the center effects default again", {
  d <- hunt5_center_data()
  r <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$rows, input_data_structure = "center_level", outcome_name = "proportion",
    outcome_goal = 0.3
  ), hunt5_center_args)))
  swept <- hunt5_quiet(lago_sensitivity(r,
    data = d$patients, input_data_structure = "individual_level", outcome_name = "y",
    parameter = "outcome_goal", values = 0.3
  ))
  # individual level data takes no center effects by default
  direct <- hunt5_quiet(do.call(lago_optimization, c(list(
    data = d$patients, outcome_name = "y", outcome_goal = 0.3
  ), hunt5_center_args)))
  expect_equal(swept$rec_int_cost, direct$rec_int_cost, tolerance = 1e-5)
})

test_that("a binary budget on the identity link stops at 0 without a false warning", {
  skip_on_cran()
  set.seed(3)
  d <- data.frame(x1 = stats::runif(2000, 0, 4), x2 = stats::runif(2000, 0, 3))
  d$y <- stats::rbinom(2000, 1, 0.7 - 0.1 * d$x1 - 0.05 * d$x2)
  warnings <- character(0)
  b <- withCallingHandlers(
    suppressMessages(lago_budget(
      data = d, outcome_name = "y", outcome_type = "binary", glm_family = "binomial",
      link = "identity", outcome_goal_intention = "minimize",
      intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(6, 5), cost_list_of_vectors = list(c(0, 1), c(0, 2)),
      budget = 100
    )),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("extended 30 times", warnings)))
  expect_lt(b$est_outcome, 1e-3)
})
