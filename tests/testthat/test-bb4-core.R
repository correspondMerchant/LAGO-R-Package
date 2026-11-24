# Tests for the core fixes of the third bug bash round. Each fails before its fix.

bb4_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("every start of the cost search caps its iterations", {
  orig <- NlcOptim::solnl
  caps <- c()
  local_mocked_bindings(
    solnl = function(X, objfun, confun = NULL, ..., maxIter = 4000) {
      if (!is.null(confun)) caps <<- c(caps, maxIter)
      if (is.null(confun)) orig(X = X, objfun = objfun, ..., maxIter = maxIter) else
        orig(X = X, objfun = objfun, confun = confun, ..., maxIter = maxIter)
    },
    .package = "NlcOptim"
  )
  bb4_quiet(lago_optimization(
    data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 30, include_confidence_set = FALSE, quiet = TRUE
  ))
  expect_gt(length(caps), 10)
  expect_true(all(caps <= 500))
})

bb4_interaction <- function(goal, ...) {
  set.seed(1)
  d <- expand.grid(a = 0:4, b = 0:4, rep = 1:8)
  d$y <- 1 + 0.5 * d$a - 0.3 * d$b + 0.2 * d$a * d$b + stats::rnorm(nrow(d), sd = 0.1)
  d[["a:b"]] <- d$a * d$b
  bb4_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("a", "b", "a:b"), main_components = c("a", "b"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(4, 4), cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = goal, include_confidence_set = FALSE, quiet = TRUE, ...
  ))
}

test_that("with interactions a goal just out of reach shrinks near the best corner", {
  best <- bb4_interaction(1)
  b <- stats::coef(best$model)
  top <- b[[1]] + 4 * b[[2]] + 4 * b[[3]] + 16 * b[[4]]
  r <- bb4_interaction(top + 0.001)
  expect_true(r$shrinking_method_used)
  # it stays at the corner that reaches the best outcome, not the stage 1 means
  expect_equal(r$rec_int, c(4, 4), tolerance = 1e-3)
  r2 <- bb4_interaction(top * 1.02)
  expect_true(all(r2$rec_int > 3))
})

test_that("the confidence set does not depend on how far a component is from 0", {
  shifted <- mtcars
  shifted$qsec <- shifted$qsec + 1e5
  fit <- function(d, lo, up) bb4_quiet(lago_optimization(
    data = d, outcome_name = "mpg", outcome_type = "continuous", glm_family = "gaussian",
    link = "identity", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = lo, intervention_upper_bounds = up, outcome_goal = 25,
    quiet = TRUE, cost_list_of_vectors = list(c(0, 4), c(0, 6)),
    confidence_set_grid_step_size = c(0.5, 0.5)
  ))
  a <- fit(shifted, c(3, 1e5 + 14), c(5, 1e5 + 23))
  b <- fit(mtcars, c(3, 14), c(5, 23))
  expect_equal(a$est_outcome_ci, b$est_outcome_ci)
  expect_equal(a$confidence_set_size_percentage, b$confidence_set_size_percentage)
  # the same inverse as solve() on an ordinary design, with weights too
  X <- cbind(1, mtcars$wt, mtcars$qsec)
  w <- mtcars$carb
  expect_equal(LAGOtrials:::xtx_inverse(X), solve(crossprod(X)), ignore_attr = TRUE)
  expect_equal(LAGOtrials:::xtx_inverse(X, w), solve(t(X) %*% (w * X)), ignore_attr = TRUE)
  expect_error(LAGOtrials:::xtx_inverse(cbind(1, 1:3, 2 * (1:3))), "computationally singular")
})

test_that("a cubic that round off would swamp falls back to the linear cost", {
  expect_warning(
    k <- LAGOtrials:::cost_fxn_calculator(1e5, 1e5 + 1, 3, "cubic")[[1]],
    "linear cost \\(3 per unit\\) is used"
  )
  expect_equal(k, c(0, 3, 0, 0, 0))
  # ordinary bounds keep the cubic, with no warning
  expect_no_warning(k <- LAGOtrials:::cost_fxn_calculator(100, 110, 3, "cubic")[[1]])
  expect_true(any(k[3:5] != 0))
})

test_that("a direct get_confidence_set() call checks center weights and reads number columns as factors", {
  set.seed(4)
  d <- data.frame(
    center = rep(1:4, each = 60), period = rep(1:3, length.out = 240),
    x1 = stats::runif(240, 0, 10), x2 = stats::runif(240, 0, 5)
  )
  d$y <- 1 + 0.3 * d$x1 + 0.2 * d$x2 + 0.1 * d$center + stats::rnorm(240)
  r <- bb4_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("x1", "x2"), intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 5), cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = 4, include_center_effects = TRUE, include_time_effects = TRUE,
    time_effect_optimization_value = 2, confidence_set_grid_step_size = c(2, 1),
    quiet = TRUE
  ))
  cs <- function(...) {
    args <- list(
      predictors_data = d[c("center", "period", "x1", "x2")],
      include_center_effects = TRUE, include_time_effects = TRUE,
      time_effect_optimization_value = 2, intervention_components = c("x1", "x2"),
      outcome_data = d$y, fitted_model = r$model, link = "identity", outcome_goal = 4,
      outcome_type = "continuous", intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(2, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = r$rec_int
    )
    dots <- list(...)
    args[names(dots)] <- dots
    bb4_quiet(do.call(get_confidence_set, args))
  }
  expect_error(cs(), "one weight per center of 'fitted_model', 4 in all")
  expect_error(cs(center_weights_for_outcome_goal = c(0.5, 0.5)), "but it has 2")
  # integer center and period columns are read as the model's factors
  out <- cs(center_weights_for_outcome_goal = rep(0.25, 4))
  expect_length(out$rec_int_ci, 2)
})

test_that("an explicit NA period level can be chosen", {
  d <- mtcars
  d$period <- addNA(factor(ifelse(seq_len(nrow(d)) %% 3 == 0, NA, seq_len(nrow(d)) %% 2)))
  m <- stats::glm(mpg ~ period + wt, data = d)
  names_p <- grep("^period", names(stats::coef(m)), value = TRUE)
  ind <- LAGOtrials:::time_effect_indicator(m, names_p, NA_character_)
  expect_equal(sum(ind), 1)
  expect_equal(names_p[ind == 1], "periodNA")
  # the reference period still has no indicator
  expect_equal(sum(LAGOtrials:::time_effect_indicator(m, names_p, "0")), 0)
})

test_that("the separation warning is for binary models only", {
  # a normal outcome in dollars, with a covariate z of no effect: its standard
  # error is large in dollars, but the fit is healthy
  set.seed(2)
  d <- data.frame(x = stats::runif(200, 0, 5), z = stats::rnorm(200))
  d$y <- 50000 + 20000 * d$x + stats::rnorm(200, sd = 40000)
  z_coef <- stats::coef(stats::lm(y ~ x + z, d))[["z"]]
  d$y <- d$y - (z_coef - 10) * d$z
  m <- stats::glm(y ~ x + z, data = d)
  expect_gt(stats::coef(summary(m))["z", 2], 1e3)
  expect_no_warning(LAGOtrials:::diagnose_model_fit(m, "x"))
})

test_that("a center with zero weights is warned about once", {
  d <- mtcars
  d$center <- factor(rep(c("A", "B", "C", "D"), length.out = nrow(d)))
  found <- character(0)
  withCallingHandlers(
    suppressMessages(lago_optimization(
      data = d, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(1.5, 15),
      intervention_upper_bounds = c(5, 22), outcome_goal = 22,
      cost_list_of_vectors = list(c(0, 4), c(0, 6)), include_center_effects = TRUE,
      weights = ifelse(d$center == "C", 0, 1), include_confidence_set = FALSE, quiet = TRUE
    )),
    warning = function(w) {
      found <<- c(found, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(sum(grepl("'C'", found, fixed = TRUE)), 1)
})

test_that("a warning from the input checks names validate_inputs, not the whole function", {
  w <- tryCatch(
    lago_optimization(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("gear", "qsec"), intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 350), cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = 15, include_confidence_set = FALSE, quiet = TRUE
    ),
    warning = function(w) w
  )
  # the goal below the observed mean warning, which carries its call
  expect_match(conditionMessage(w), "observed mean")
  call_text <- paste(deparse(conditionCall(w)), collapse = "")
  expect_match(call_text, "^validate_inputs\\(")
  expect_false(grepl("function (data", call_text, fixed = TRUE))
})

test_that("lago_optimization() runs at an explicit NA period level", {
  d <- mtcars
  d$period <- addNA(factor(ifelse(seq_len(nrow(d)) %% 3 == 0, NA, seq_len(nrow(d)) %% 2)))
  run <- function(v) bb4_quiet(lago_optimization(
    data = d, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("wt", "qsec"), intervention_lower_bounds = c(1.5, 15),
    intervention_upper_bounds = c(5, 22), outcome_goal = 22,
    cost_list_of_vectors = list(c(0, 4), c(0, 6)), include_time_effects = TRUE,
    time_effect_optimization_value = v, include_confidence_set = FALSE, quiet = TRUE
  ))
  r <- run(NA_character_)
  b <- stats::coef(r$model)
  expect_equal(r$est_outcome, b[["(Intercept)"]] + b[["periodNA"]] + sum(b[c("wt", "qsec")] * r$rec_int), tolerance = 1e-6)
})

bb5_interaction <- function(goal, f = function(x1, x2) x1 + x2 - 3 * x1 * x2, ...) {
  set.seed(5)
  d <- data.frame(x1 = stats::runif(400), x2 = stats::runif(400))
  d$y <- f(d$x1, d$x2) + stats::rnorm(400, sd = 0.05)
  d[["x1:x2"]] <- d$x1 * d$x2
  bb4_quiet(lago_optimization(
    data = d, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("x1", "x2", "x1:x2"), main_components = c("x1", "x2"),
    include_interaction_terms = TRUE, intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(1, 1), cost_list_of_vectors = list(c(0, 1), c(0, 2)),
    outcome_goal = goal, include_confidence_set = FALSE, quiet = TRUE, ...
  ))
}

test_that("with opposing interactions the shrunk recommendation stays near the best point", {
  top <- bb5_interaction(0.5)
  b <- stats::coef(top$model)
  best <- max(b[[1]] + b[[2]], b[[1]] + b[[3]])
  for (method in c("numerical", "grid_search")) {
    r <- bb5_interaction(best + 0.002, optimization_method = method,
      optimization_grid_search_step_size = if (method == "grid_search") c(0.05, 0.05))
    expect_true(r$shrinking_method_used)
    # the estimate stays close to the best outcome, not at the worst corner
    expect_gt(r$est_outcome, best - 0.05)
  }
})

bb5_mtcars_fit <- function(d, lo, up, step = c(0.5, 0.5), ...) {
  suppressWarnings(lago_optimization(
    data = d, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = lo, intervention_upper_bounds = up,
    quiet = TRUE, cost_list_of_vectors = list(c(0, 4), c(0, 6)),
    confidence_set_grid_step_size = step, ...
  ))
}

test_that("a clustered confidence set does not change when a component is shifted far from 0", {
  set.seed(846)
  n <- 90L
  d <- data.frame(x = seq(-1, 1, length.out = n), z = rnorm(n))
  d$y <- 0.5 + 0.16 * d$x + 0.04 * d$z + rnorm(n, sd = 0.055)
  d$c1 <- rep_len(c("b", "a", "c", "d", "e"), n)
  run <- function(s) {
    dd <- d
    dd$x <- dd$x + s
    m <- glm(y ~ x + z, data = dd)
    suppressWarnings(get_confidence_set(
      predictors_data = dd[c("x", "z")], additional_covariates = "z",
      intervention_components = "x", outcome_data = dd$y, fitted_model = m,
      link = "identity", outcome_type = "continuous", outcome_goal = 0.5,
      intervention_lower_bounds = s - 1, intervention_upper_bounds = s + 1,
      confidence_set_grid_step_size = 0.05, cost_list_of_vectors = list(c(0, 1)),
      rec_int = s + 0.4, cluster_id = list(dd$c1)
    ))
  }
  near <- run(0)
  far <- run(1e6)
  expect_equal(far$rec_int_ci, near$rec_int_ci)
  expect_equal(far$confidence_set_size_percentage, near$confidence_set_size_percentage)
})

test_that("a quasibinomial logit fit with a component near 100000 keeps its interval", {
  near <- mtcars
  near$mpg <- plogis((mtcars$mpg - 20) / 6)
  far <- near
  far$qsec <- far$qsec + 1e5
  args <- list(glm_family = "quasibinomial", link = "logit", outcome_goal = 0.6)
  r0 <- do.call(bb5_mtcars_fit, c(list(near, c(3, 14), c(5, 23)), args))
  r1 <- do.call(bb5_mtcars_fit, c(list(far, c(3, 1e5 + 14), c(5, 1e5 + 23)), args))
  expect_equal(r1$est_outcome_ci, r0$est_outcome_ci)
  expect_equal(r1$confidence_set_size_percentage, r0$confidence_set_size_percentage)
})

test_that("an identity fit with a component near 1e7 is not called rank deficient", {
  d <- mtcars
  d$qsec <- mtcars$qsec + 1e7
  r <- bb5_mtcars_fit(d, c(3, 1e7 + 14), c(5, 1e7 + 23),
    glm_family = "gaussian", link = "identity", outcome_goal = 25
  )
  r0 <- bb5_mtcars_fit(mtcars, c(3, 14), c(5, 23),
    glm_family = "gaussian", link = "identity", outcome_goal = 25
  )
  expect_equal(r$est_outcome_ci, r0$est_outcome_ci)
  d$qsec <- mtcars$qsec / 10 + 1e7
  expect_no_error(bb5_mtcars_fit(d, c(3, 1e7 + 1.4), c(5, 1e7 + 2.3),
    glm_family = "gaussian", link = "identity", outcome_goal = 25,
    step = c(0.5, 0.05)
  ))
})

test_that("a default cubic cost whose total falls as evaluated is replaced by the linear cost", {
  cases <- list(
    c("0x1.52e3125ebd31ap+18", "0x1.52e33da4a2966p+18", "0x1.602e5f49aacedp+16", "0x1.52e33140d0b29p+18"),
    c("0x1.3907aaebd3b99p+19", "0x1.3907c18b4f36p+19", "0x1.7977e7b9ce83ap+10", "0x1.3907c18b4f36p+19"),
    c("0x1.1d9e653e51f0ep+4", "0x1.1d9f2c6d15397p+4", "0x1.3b76bed56931fp+13", NA)
  )
  for (case in cases) {
    v <- as.numeric(case)
    k <- suppressWarnings(LAGOtrials:::cost_fxn_calculator(v[1], v[2], v[3], "cubic", v[4])[[1]])
    x <- seq(v[1], v[2], length.out = 2001)
    total <- vapply(x, function(z) sum(k * z^(0:4)), numeric(1))
    rise <- total[length(total)] - total[1]
    expect_gt(rise, 0)
    expect_gte(min(diff(total)), -1e-6 * rise)
  }
})

bb5_na_period_data <- function() {
  set.seed(7)
  n <- 400
  d <- data.frame(
    period = addNA(factor(sample(c(1:3, NA), n, TRUE))),
    x1 = runif(n, 0, 10), x2 = runif(n, 0, 5)
  )
  d$y <- rbinom(n, 1, plogis(-1 + 0.1 * d$x1 + 0.2 * d$x2))
  d$yc <- -1 + 0.1 * d$x1 + 0.2 * d$x2 + rnorm(n)
  d
}

test_that("a direct continuous confidence set refuses an explicit NA level clearly", {
  d <- bb5_na_period_data()
  m <- glm(yc ~ x1 + x2 + period, data = d)
  expect_error(
    get_confidence_set(
      predictors_data = d[, c("period", "x1", "x2")], include_time_effects = TRUE,
      time_effect_optimization_value = 2, intervention_components = c("x1", "x2"),
      outcome_data = d$yc, fitted_model = m, link = "identity", outcome_goal = 0.6,
      outcome_type = "continuous", intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(5, 2.5),
      cost_list_of_vectors = list(c(0, 1), c(0, 2)), rec_int = c(5, 2)
    ),
    "cannot use an explicit NA level .* in the column\\(s\\) 'period'"
  )
})

test_that("the text NA for an explicit NA period level points to NA_character_", {
  d <- bb5_na_period_data()
  expect_error(
    suppressWarnings(lago_optimization(
      data = d, outcome_name = "y", outcome_type = "binary", glm_family = "binomial",
      intervention_components = c("x1", "x2"), include_time_effects = TRUE,
      time_effect_optimization_value = "NA", intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 5), cost_list_of_vectors = list(c(0, 1), c(0, 2)),
      outcome_goal = 0.6, quiet = TRUE
    )),
    "NA \\(an explicit NA level, chosen with NA_character_\\)"
  )
})

test_that("a goal far smaller than the outcomes still gives the cheapest intervention", {
  run <- function(goal) {
    suppressWarnings(LAGOtrials:::get_recommended_interventions(
      data = NULL, intervention_components_coeff = c(-50, 60, 40),
      include_interaction_terms = FALSE, main_components = c("x1", "x2"),
      intervention_components = c("x1", "x2"), all_center_lvl_effects = 0,
      center_weights_for_outcome_goal = 1,
      cost_list_of_vectors = list(c(0, 1, 0, 2), c(0, 3, 0, 1)),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(1, 1),
      outcome_goal = goal, optimization_method = "numerical",
      optimization_grid_search_step_size = NULL, center_cha_coeff_vec = 0,
      center_characteristics_optimization_values = 0, link = "identity",
      lower_outcome_goal = FALSE, prev_recommended_interventions = c(0.1, 0.1),
      shrinkage_threshold = 0.25, power_goal = NULL,
      power_goal_approach = "unconditional", num_centers_in_next_stage = NULL,
      patients_per_center_in_next_stage = NULL, outcome_name = "y"
    ))
  }
  expect_equal(run(1e-200)$rec_int_cost, run(1e-9)$rec_int_cost, tolerance = 1e-6)
})

test_that("a binary fit on the identity link that glm() cannot start explains why", {
  expect_error(
    suppressWarnings(lago_optimization(
      data = BB_data, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
      link = "identity", intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = 0.99,
      include_confidence_set = FALSE, quiet = TRUE
    )),
    "With the identity link, glm\\(\\) needs fitted probabilities between 0 and 1"
  )
})

test_that("conditioning a design scales a constant column and leaves a zero column", {
  X <- cbind("(Intercept)" = 1, a = c(1, 2, 3, 4) + 1e6, b = 5, z = 0)
  cd <- LAGOtrials:::conditioned_design(X)
  expect_equal(cd$Z %*% solve(cd$M), X, ignore_attr = TRUE)
  expect_equal(diag(cd$M)[3:4], c(1 / 5, 1))
  expect_equal(unname(cd$Z[, "a"]), as.numeric(scale(X[, "a"])))
})

test_that("shrinking with interactions keeps the stage 1 intervention when the best point is no better", {
  shrunk <- LAGOtrials:::shrinking_method(
    lo = c(0, 0), up = c(1, 1), beta = c(0, 1, 1, -3), outcome_goal = 2,
    include_interaction_terms = TRUE, intervention_components = c("x1", "x2", "x1:x2"),
    main_components = c("x1", "x2"), all_center_lvl_effects = 0,
    center_weights_for_outcome_goal = 1, center_cha_coeff_vec = 0, center_cha = 0,
    link = "identity", stage_1_intervention = c(0.5, 0.2), best_point = c(0, 0)
  )
  expect_equal(shrunk, c(0.5, 0.2))
})
