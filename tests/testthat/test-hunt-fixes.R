# Results that must not depend on how the same model is written down, checked
# against independent calculations.

hunt_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# ---- power goal: the next-stage control rate --------------------------------

power_planning_data <- function() {
  set.seed(20)
  centers <- do.call(rbind, lapply(seq_len(12), function(j) {
    data.frame(
      center = paste0("c", j), group = if (j <= 6) "control" else "treatment",
      beds = round(stats::runif(1, 2, 6), 1)
    )
  }))
  d <- centers[rep(seq_len(12), each = 25), ]
  d$dose <- ifelse(d$group == "control", 0, round(stats::runif(nrow(d), 1, 8)))
  d$y <- stats::rbinom(nrow(d), 1, stats::plogis(-2.6 + 0.12 * d$dose + 0.35 * d$beds))
  d
}

power_planning_run <- function(d, value, approach, ...) {
  hunt_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "binary",
    intervention_components = "dose", intervention_lower_bounds = 0,
    intervention_upper_bounds = 20, cost_list_of_vectors = list(c(0, 100)),
    power_goal = 0.8, power_goal_approach = approach,
    num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 20,
    include_confidence_set = FALSE, quiet = TRUE, ...
  ))
}

test_that("the power goal plans at the center characteristic values, not at 0", {
  d <- power_planning_data()
  value <- mean(unique(d[, c("center", "beds")])$beds)
  centered <- d
  centered$beds <- d$beds - value
  for (approach in c("unconditional", "conditional")) {
    raw <- power_planning_run(d, approach = approach,
      center_characteristics = "beds", center_characteristics_optimization_values = value
    )
    shifted <- power_planning_run(centered, approach = approach,
      center_characteristics = "beds", center_characteristics_optimization_values = 0
    )
    expect_equal(raw$effective_outcome_goal, shifted$effective_outcome_goal, tolerance = 1e-8)
    expect_equal(raw$rec_int, shifted$rec_int, tolerance = 1e-6)
  }
  # the unconditional answer is the first grid point with enough power
  fit <- stats::glm(y ~ dose + beds, family = stats::binomial, data = d)
  q0 <- unname(stats::predict(fit, data.frame(dose = 0, beds = value), type = "response"))
  n2 <- 10 / 2 * 20
  ctl <- d$group == "control"
  S1 <- sum(d$y[!ctl]); n1 <- sum(!ctl); S0 <- sum(d$y[ctl]); n0 <- sum(ctl)
  power_at <- function(e) {
    p1 <- (S1 + n2 * e) / (n1 + n2)
    p0 <- (S0 + n2 * q0) / (n0 + n2)
    ncp <- (p1 - p0)^2 / (p1 * (1 - p1) / (n1 + n2) + p0 * (1 - p0) / (n0 + n2))
    1 - stats::pchisq(stats::qchisq(0.95, 1), 1, ncp = ncp)
  }
  oracle <- stats::uniroot(function(e) power_at(e) - 0.8, c(max(q0, S0 / n0), 1))$root
  raw <- power_planning_run(d, approach = "unconditional",
    center_characteristics = "beds", center_characteristics_optimization_values = value
  )
  expect_equal(raw$effective_outcome_goal, oracle, tolerance = 1e-3)
})

test_that("the power goal does not depend on which center is the reference", {
  d <- power_planning_data()
  goals <- vapply(c("c1", "c5", "c9"), function(reference) {
    dd <- d
    dd$center <- stats::relevel(factor(dd$center), ref = reference)
    power_planning_run(dd, approach = "unconditional", include_center_effects = TRUE)$effective_outcome_goal
  }, numeric(1))
  expect_equal(unname(goals), rep(goals[[1]], 3), tolerance = 1e-8)
})

test_that("the conditional power goal skips grid points it cannot evaluate in place", {
  d <- make_small_clustered()
  d$dose <- ifelse(d$group == "treatment", 1, 0)
  # a control rate below 0, possible on the identity link, makes some points NaN
  control_rate <- -0.02
  result <- suppressWarnings(get_power_desired_outcome(
    data = d, intervention_components_coeff = c("(Intercept)" = control_rate, dose = 0.1),
    power_goal = 0.8, power_goal_approach = "conditional",
    num_centers_in_next_stage = 20, patients_per_center_in_next_stage = 20,
    outcome_name = "y", control_rate = control_rate
  ))
  # the same conditional power inequality, evaluated on the same grid
  ctl <- d$group == "control"
  n0_1 <- sum(ctl); n1_1 <- sum(!ctl); S0_1 <- sum(d$y[ctl]); S1_1 <- sum(d$y[!ctl])
  n2 <- 20 / 2 * 20; N0 <- n0_1 + n2; N1 <- n1_1 + n2
  value <- function(e) {
    p1 <- (S1_1 + n2 * e) / N1
    p0 <- (S0_1 + n2 * control_rate) / N0
    threshold <- stats::qnorm(0.975) * sqrt(p1 * (1 - p1) / N1 + p0 * (1 - p0) / N0)
    mu <- n2 * e / N1 - n2 * control_rate / N0
    sigma <- sqrt(n2 * control_rate * (1 - control_rate) / N0^2 + n2 * e * (1 - e) / N1^2)
    threshold - S1_1 / N1 + S0_1 / N0 - mu + stats::qnorm(0.8) * sigma
  }
  grid <- seq(control_rate, 1, length.out = 1000)
  values <- suppressWarnings(vapply(grid, value, numeric(1)))
  expect_true(anyNA(values))
  expect_identical(result, grid[!is.na(values) & values <= 0][1])
})

# ---- power goal: center level data -------------------------------------------

test_that("the power goal on center level data counts patients, not rows", {
  set.seed(11)
  cl <- expand.grid(period = 1:4, center = paste0("c", 1:12), stringsAsFactors = FALSE)
  cl$group <- ifelse(as.integer(sub("c", "", cl$center)) <= 6, "control", "treatment")
  cl$dose <- ifelse(cl$group == "control", 0, cl$period + round(stats::runif(nrow(cl), 0, 3)))
  cl$center_sample_size <- 15
  cl$events <- stats::rbinom(nrow(cl), 15, stats::plogis(-1.4 + 0.12 * cl$dose))
  cl$proportion <- cl$events / 15
  patients <- cl[rep(seq_len(nrow(cl)), cl$center_sample_size), c("center", "group", "dose")]
  patients$y <- unlist(lapply(seq_len(nrow(cl)), function(i) {
    rep(1:0, c(cl$events[i], 15 - cl$events[i]))
  }))
  common <- list(
    outcome_type = "binary", intervention_components = "dose",
    include_center_effects = TRUE, intervention_lower_bounds = 0,
    intervention_upper_bounds = 30, cost_list_of_vectors = list(c(0, 100)),
    power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 20, include_confidence_set = FALSE,
    quiet = TRUE
  )
  for (approach in c("unconditional", "conditional")) {
    aggregated <- hunt_quiet(do.call(lago_optimization, c(common, list(
      data = cl, input_data_structure = "center_level",
      outcome_name = "proportion", power_goal_approach = approach
    ))))
    expanded <- hunt_quiet(do.call(lago_optimization, c(common, list(
      data = patients, outcome_name = "y", power_goal_approach = approach
    ))))
    expect_equal(aggregated$effective_outcome_goal, expanded$effective_outcome_goal, tolerance = 1e-8)
    expect_equal(aggregated$rec_int, expanded$rec_int, tolerance = 1e-6)
  }
})

test_that("a center level row without a sample size is left out of the power goal", {
  set.seed(11)
  cl <- expand.grid(period = 1:4, center = paste0("c", 1:12), stringsAsFactors = FALSE)
  cl$group <- ifelse(as.integer(sub("c", "", cl$center)) <= 6, "control", "treatment")
  cl$dose <- ifelse(cl$group == "control", 0, cl$period + round(stats::runif(nrow(cl), 0, 3)))
  cl$center_sample_size <- 15
  cl$events <- stats::rbinom(nrow(cl), 15, stats::plogis(-1.4 + 0.12 * cl$dose))
  cl$proportion <- cl$events / 15
  # one control and one treatment row lose their sample size
  missing <- c(which(cl$center == "c2" & cl$period == 3), which(cl$center == "c8" & cl$period == 2))
  kept <- cl[-missing, ]
  cl$center_sample_size[missing] <- NA
  patients <- kept[rep(seq_len(nrow(kept)), kept$center_sample_size), c("center", "group", "dose")]
  patients$y <- unlist(lapply(seq_len(nrow(kept)), function(i) {
    rep(1:0, c(kept$events[i], 15 - kept$events[i]))
  }))
  common <- list(
    outcome_type = "binary", intervention_components = "dose",
    include_center_effects = TRUE, intervention_lower_bounds = 0,
    intervention_upper_bounds = 30, cost_list_of_vectors = list(c(0, 100)),
    power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 20, include_confidence_set = FALSE,
    # the default weights size a center differently in the two layouts
    center_weights_for_outcome_goal = rep(1 / 12, 12), quiet = TRUE
  )
  for (approach in c("unconditional", "conditional")) {
    aggregated <- hunt_quiet(do.call(lago_optimization, c(common, list(
      data = cl, input_data_structure = "center_level",
      outcome_name = "proportion", power_goal_approach = approach
    ))))
    expanded <- hunt_quiet(do.call(lago_optimization, c(common, list(
      data = patients, outcome_name = "y", power_goal_approach = approach
    ))))
    expect_equal(aggregated$effective_outcome_goal, expanded$effective_outcome_goal, tolerance = 1e-8)
  }
})

# ---- the interval of the weighted average center on the logit link ----------

weighted_center_data <- function() {
  set.seed(11)
  cl <- data.frame(
    center = factor(rep(paste0("c", 1:8), each = 3)),
    coaching_updt = rep(c(0, 6, 12), 8) + rep(0:7, each = 3) %% 3,
    launch_duration = rep(c(1, 3, 2), 8),
    center_sample_size = rep(c(40, 60, 80, 100, 120, 150, 200, 250), each = 3)
  )
  effect <- rep(c(-2.0, -1.2, -0.5, 0, 0.5, 1.0, 1.6, 2.2), each = 3)
  p <- stats::plogis(-1 + 0.12 * cl$coaching_updt + 0.3 * cl$launch_duration + effect)
  cl$proportion <- stats::rbinom(nrow(cl), cl$center_sample_size, p) / cl$center_sample_size
  cl
}

test_that("the weighted average center's interval is for the reported estimate", {
  cl <- weighted_center_data()
  res <- hunt_quiet(lago_optimization(
    data = cl, input_data_structure = "center_level", outcome_name = "proportion",
    outcome_type = "binary", glm_family = "binomial",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 1), intervention_upper_bounds = c(20, 4),
    cost_list_of_vectors = list(c(0, 10), c(0, 50)), outcome_goal = 0.75,
    outcome_goal_intention = "maximize", confidence_set_grid_step_size = c(1, 1),
    quiet = TRUE
  ))
  fit <- stats::glm(proportion ~ center + coaching_updt + launch_duration,
    family = stats::binomial, weights = center_sample_size, data = cl
  )
  w <- as.numeric(tapply(cl$center_sample_size, cl$center, `[`, 1))
  w <- w / sum(w)
  # the delta method interval of sum(w * p) over the centers
  oracle <- function(x1, x2) {
    rows <- data.frame(
      center = factor(levels(cl$center), levels = levels(cl$center)),
      coaching_updt = x1, launch_duration = x2
    )
    X <- stats::model.matrix(~ center + coaching_updt + launch_duration, rows)
    p <- as.numeric(stats::plogis(X %*% stats::coef(fit)))
    gradient <- colSums(w * p * (1 - p) * X)
    se <- sqrt(drop(t(gradient) %*% stats::vcov(fit) %*% gradient))
    estimate <- sum(w * p)
    c(estimate - stats::qnorm(0.975) * se, estimate + stats::qnorm(0.975) * se)
  }
  expected <- oracle(res$rec_int[1], res$rec_int[2])
  expect_equal(unname(res$est_outcome_ci), round(expected, 3), tolerance = 1e-8)
  expect_true(res$est_outcome_ci[1] <= res$est_outcome_goal && res$est_outcome_goal <= res$est_outcome_ci[2])
  grid <- expand.grid(coaching_updt = 0:20, launch_duration = 1:4)
  bounds <- t(mapply(oracle, grid$coaching_updt, grid$launch_duration))
  members <- grid[bounds[, 1] <= 0.75 & bounds[, 2] >= 0.75, ]
  expect_identical(
    sort(paste(res$cs$coaching_updt, res$cs$launch_duration)),
    sort(paste(members$coaching_updt, members$launch_duration))
  )
})

test_that("a continuous logit interval for the weighted average center is centred on the estimate", {
  set.seed(4)
  d <- data.frame(center = factor(rep(paste0("c", 1:4), each = 150)), x = stats::runif(600, 0, 10))
  base <- c(-2.5, -1, 0.5, 2.5)[as.integer(d$center)]
  d$y <- stats::plogis(base + 0.15 * d$x + stats::rnorm(600, 0, 0.3))
  res <- hunt_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    glm_family = "quasibinomial", link = "logit", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 10,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = 0.5,
    outcome_goal_intention = "maximize", include_center_effects = TRUE,
    confidence_set_grid_step_size = 0.5, quiet = TRUE
  ))
  expect_equal(mean(res$est_outcome_ci), res$est_outcome_goal, tolerance = 1e-3)
  expect_true(res$est_outcome_ci[1] <= res$est_outcome_goal && res$est_outcome_goal <= res$est_outcome_ci[2])
})

# ---- shrinking ----------------------------------------------------------------

test_that("the shrunk recommendation does not depend on the outcome's units", {
  set.seed(3)
  d <- data.frame(x = stats::runif(120, 0, 1))
  d$y <- 3000 + 200 * d$x + stats::rnorm(120, 0, 50)
  run <- function(scale, method) {
    dd <- d
    dd$y <- dd$y / scale
    hunt_quiet(lago_optimization(
      data = dd, outcome_name = "y", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity", intervention_components = "x",
      intervention_lower_bounds = 0, intervention_upper_bounds = 1,
      cost_list_of_vectors = list(c(0, 10)), outcome_goal = 3300 / scale,
      outcome_goal_intention = "maximize", optimization_method = method,
      include_confidence_set = FALSE, quiet = TRUE
    ))$rec_int
  }
  # Nevo's shrinking formula by hand
  b <- stats::coef(stats::glm(y ~ x, data = d))
  beta_max <- 3300 - b[[1]]
  beta_min <- beta_max / 2
  oracle <- mean(d$x) + (1 - mean(d$x)) * (b[[2]] - beta_min) / (beta_max - beta_min)
  # the search stops at a coefficient step of 1e-6, so allow 1e-4 here
  for (method in c("numerical", "grid_search")) {
    expect_equal(unname(run(1, method)), oracle, tolerance = 1e-4)
    expect_equal(unname(run(1000, method)), oracle, tolerance = 1e-4)
  }
})

test_that("shrinking finishes for a needed coefficient far above 100", {
  set.seed(3)
  d <- data.frame(x = stats::runif(120, 0, 1))
  d$y <- (3000 + 200 * d$x + stats::rnorm(120, 0, 50)) * 1e8
  started <- proc.time()[["elapsed"]]
  rec <- hunt_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity", intervention_components = "x",
    intervention_lower_bounds = 0, intervention_upper_bounds = 1,
    cost_list_of_vectors = list(c(0, 10)), outcome_goal = 3300 * 1e8,
    outcome_goal_intention = "maximize", include_confidence_set = FALSE,
    quiet = TRUE
  ))$rec_int
  expect_lt(proc.time()[["elapsed"]] - started, 30)
  b <- stats::coef(stats::glm(y ~ x, data = d))
  beta_max <- 3300 * 1e8 - b[[1]]
  oracle <- mean(d$x) + (1 - mean(d$x)) * (b[[2]] - beta_max / 2) / (beta_max / 2)
  expect_equal(unname(rec), oracle, tolerance = 1e-4)
})

test_that("shrinking keeps the stage-1 value of a component with no term of its own", {
  set.seed(2)
  d <- data.frame(a = stats::runif(200, 0, 4), b = stats::runif(200, 0, 4))
  d$y <- 1 + 0.8 * d$a + 0.2 * d$a * d$b + stats::rnorm(200, 0, 0.5)
  d[["a:b"]] <- d$a * d$b
  res <- hunt_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = c("a", "a:b"), main_components = c("a", "b"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4), cost_list_of_vectors = list(c(0, 1), c(0, 2)),
    outcome_goal = 20, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_equal(res$rec_int[2], mean(d$b), tolerance = 1e-8)
  expect_true(res$rec_int[1] >= 0 && res$rec_int[1] <= 4)
})

test_that("the shrunk recommendation does not depend on the order of the terms", {
  set.seed(2)
  d <- data.frame(a = stats::runif(200, 0, 4), b = stats::runif(200, 0, 4))
  d$y <- 1 + 0.8 * d$a + 0.3 * d$b + 0.2 * d$a * d$b + stats::rnorm(200, 0, 0.5)
  d[["a:b"]] <- d$a * d$b
  run <- function(components, mains) {
    res <- hunt_quiet(lago_optimization(
      data = d, outcome_name = "y", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity",
      intervention_components = components, main_components = mains,
      include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(4, 4),
      cost_list_of_vectors = if (identical(mains, c("a", "b"))) list(c(0, 1), c(0, 2)) else list(c(0, 2), c(0, 1)),
      outcome_goal = 9, outcome_goal_intention = "maximize",
      include_confidence_set = FALSE, quiet = TRUE
    ))
    stats::setNames(res$rec_int, mains)[c("a", "b")]
  }
  # Nevo's shrinking formula by hand, each component with its own coefficient
  b <- stats::coef(stats::glm(y ~ a + b + a:b, data = d))
  s1 <- colMeans(d[, c("a", "b")])
  oracle <- vapply(1:2, function(k) {
    other <- 3 - k
    rest <- b[[1]] + b[[1 + other]] * 4 + b[[4]] * 16
    beta_max <- (9 - rest) / 4
    beta_min <- beta_max / 2
    s1[[k]] + (4 - s1[[k]]) * (b[[1 + k]] - beta_min) / (beta_max - beta_min)
  }, numeric(1))
  expect_equal(unname(run(c("a", "b", "a:b"), c("a", "b"))), oracle, tolerance = 1e-5)
  expect_equal(unname(run(c("a:b", "a", "b"), c("a", "b"))), oracle, tolerance = 1e-5)
  expect_equal(unname(run(c("a", "b", "a:b"), c("b", "a"))), oracle, tolerance = 1e-5)
})
