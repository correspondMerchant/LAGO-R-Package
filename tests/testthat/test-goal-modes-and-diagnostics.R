# Tests for the three goal modes (#40) and the fit diagnostics (#36).

bb_grouped <- function() {
  d <- BB_data
  d$group <- ifelse(d$pre_post == 0, "control", "treatment")
  d
}

test_that("power goal alone works (no outcome goal)", {
  res <- suppressWarnings(suppressMessages(lago_optimization(
    data = bb_grouped(),
    outcome_name = "pp3_oxytocin_mother",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    power_goal = 0.8,
    num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 30,
    include_confidence_set = FALSE
  )))
  expect_length(res$rec_int, 2)
  # with no outcome goal, the target IS the power-implied outcome; pin it so
  # the power-to-outcome conversion actually has coverage (a broken
  # conversion would change this value).
  expect_equal(res$est_outcome_goal, 0.47817, tolerance = 1e-3)
})

test_that("both goals: the higher (outcome) goal binds", {
  args <- list(
    data = BB_data, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    outcome_goal = 0.85, outcome_goal_intention = "maximize",
    include_confidence_set = FALSE
  )
  outcome_only <- suppressWarnings(suppressMessages(do.call(lago_optimization, args)))
  both <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    utils::modifyList(args, list(
      data = bb_grouped(), power_goal = 0.8,
      num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 30
    ))
  )))
  # outcome goal 0.85 exceeds the power-implied outcome, so results match.
  expect_equal(both$rec_int, outcome_only$rec_int, tolerance = 1e-3)
})

test_that("power goal + minimize is rejected", {
  expect_error(
    suppressWarnings(lago_optimization(
      data = bb_grouped(),
      outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = "birth_volume_100",
      center_characteristics_optimization_values = 1.75,
      intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      outcome_goal = 0.85, outcome_goal_intention = "minimize",
      power_goal = 0.8, num_centers_in_next_stage = 10,
      patients_per_center_in_next_stage = 30
    )),
    "minimize"
  )
})

test_that("a non-significant intervention component triggers a warning", {
  set.seed(2)
  n <- 200
  # "noise" is non-negative (valid intervention) but has no true effect on y.
  d <- data.frame(dose = runif(n, 0, 10), noise = runif(n, 0, 10))
  d$y <- rbinom(n, 1, 1 / (1 + exp(-(-1 + 0.4 * d$dose))))
  # bounds cover the full data range so only the significance warning fires.
  expect_warning(
    suppressMessages(lago_optimization(
      data = d, outcome_name = "y", outcome_type = "binary",
      intervention_components = c("dose", "noise"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 10),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      outcome_goal = 0.7, outcome_goal_intention = "maximize",
      include_confidence_set = FALSE
    )),
    "significant"
  )
})

test_that("providing a grid step size switches method to grid_search", {
  expect_message(
    suppressWarnings(lago_optimization(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity",
      intervention_components = c("gear", "qsec"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
      cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = 40, outcome_goal_intention = "maximize",
      optimization_grid_search_step_size = c(1, 1),
      include_confidence_set = FALSE
    )),
    "grid_search"
  )
})


# A binary outcome's ESTIMATE reported outside [0, 1]. On the identity link the
# outcome model is a linear probability model, so the estimate is the linear
# predictor and is not confined to anything: intervention bounds reaching past
# the range the components were fitted over extrapolate it out of the range,
# and glm() does not object because every fitted value ON THE DATA is a
# probability. It was reported with no error and no warning.
#
# The fixture is built so the mechanism is the ONLY thing being tested: x1 is
# fitted over 0..4 and the upper bound is 9, so nothing but the extrapolation
# puts the estimate out of range. max(fitted()) < 1 is asserted, so a run that
# reproduces this cannot be dismissed as a fit glm() would already have
# complained about.
# Both components are significant on this seed, deliberately: the fit
# diagnostics warn about a non-significant intervention component, and a
# fixture that tripped that too would emit a second warning that expect_warning
# lets escape and the suite then counts as a stray. The seed is chosen so the
# range warning is the ONLY warning the run emits.
out_of_range_fixture <- function() {
  set.seed(152)
  n <- 60
  x1 <- rep(0:4, each = 12)
  x2 <- rep(1:3, times = 20)
  p <- pmin(pmax(0.10 + 0.11 * x1 + 0.10 * x2, 0), 1)
  data.frame(y = rbinom(n, 1, p), x1 = x1, x2 = x2)
}

out_of_range_run <- function(...) {
  args <- list(
    data = out_of_range_fixture(),
    outcome_name = "y",
    outcome_type = "binary",
    glm_family = "binomial",
    link = "identity",
    intervention_components = c("x1", "x2"),
    intervention_lower_bounds = c(0, 1),
    # 9 is well beyond the 0..4 x1 was fitted over, which is what extrapolates
    intervention_upper_bounds = c(9, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = 1.3,
    optimization_method = "grid_search",
    optimization_grid_search_step_size = c(1, 1),
    confidence_set_grid_step_size = c(1, 1),
    quiet = TRUE
  )
  ov <- list(...)
  for (nm in names(ov)) args[nm] <- list(ov[[nm]])
  do.call(lago_optimization, args)
}

test_that("a binary estimate outside [0, 1] warns and is not altered", {
  d <- out_of_range_fixture()
  model <- suppressWarnings(glm(
    y ~ x1 + x2, data = d, family = binomial(link = "identity")
  ))
  # the preconditions, so this cannot go vacuous: glm() converged and every
  # fitted value on the DATA is a probability, so the fit is one glm() accepts
  # without complaint and only the extrapolated prediction leaves the range.
  expect_true(model$converged)
  expect_lt(max(fitted(model)), 1)

  expect_warning(
    res <- out_of_range_run(),
    "outside \\[0, 1\\] and so is not a probability"
  )

  # the estimate is out of range, which is the defect, and it is reported AS
  # COMPUTED: nothing is clamped. Held against the model's own linear
  # predictor at the recommended intervention, computed here from coef()
  # alone, so this is not the package agreeing with itself.
  expect_gt(res$est_outcome_goal, 1)
  hand <- as.numeric(c(1, res$rec_int) %*% coef(model))
  expect_equal(res$est_outcome_goal, hand, tolerance = 1e-12)
})

# every warning one run emits, so a test can pick out the one it is about. The
# fixture also trips the pre-existing significance warning on x2, and that one
# comes FIRST: tryCatch(warning=) would hand back that message instead, which
# is what made the assertions below look like failures against a warning they
# were never about.
out_of_range_warnings <- function(...) {
  ws <- character(0)
  withCallingHandlers(
    out_of_range_run(...),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  ws
}

test_that("the range warning names the range, the cause and the bounds", {
  # actionable, not merely present: it has to say what is out of range, what
  # the range is, why it happened, and that nothing was altered. A warning
  # that only says "out of range" leaves the user with no next step.
  ws <- out_of_range_warnings()
  w <- ws[grepl("not a probability", ws)]
  expect_length(w, 1)
  # the value is printed at full precision, not rounded: a value only just
  # outside the range, say 1.0000004, would round to 1 and read "the estimated
  # outcome is 1, which is outside [0, 1]", contradicting itself. So the printed
  # value is asserted to begin with the true digits and not to be a rounded one.
  expect_match(w, "estimated outcome is 1\\.4407272")
  expect_match(w, "outside \\[0, 1\\]")
  expect_match(w, "binary")
  expect_match(w, "linear probability model")
  expect_match(w, "extrapolates outside")
  expect_match(w, "link = \"identity\"")
  # the mechanism, named as extrapolation beyond the fitted range
  expect_match(w, "beyond the range its components were fitted")
  # that nothing was altered, which is the whole reason this warns
  expect_match(w, "no reported value has been altered")
  # and the actionable alternatives
  expect_match(w, "narrowing the intervention bounds")
  expect_match(w, "link = \"logit\"")
  # it counts the affected reported bounds rather than staying silent on them
  expect_match(w, "reported confidence interval bound\\(s\\) are outside")
})

test_that("the range warning does not round a barely-out value to look in range", {
  # an estimate just past the boundary, 1 + 4e-7, must not print as "1", which
  # would make the message say "the estimated outcome is 1, which is outside
  # [0, 1]" -- a statement and its negation. The message builder is called
  # directly, since driving the optimizer to land an estimate exactly there is
  # not controllable.
  warn_fn <- getFromNamespace("warn_if_outcome_outside_range", "LAGO")
  msg <- tryCatch(
    warn_fn(1.0000004, "binary", "identity"),
    warning = conditionMessage
  )
  expect_true(is.character(msg))
  expect_no_match(msg, "outcome is 1, which is outside")
  expect_match(msg, "1.0000004", fixed = TRUE)
})

test_that("an estimate at exactly 0 or 1 is a probability and does not warn", {
  # the boundary is closed: 0 and 1 are probabilities, so an estimate landing on
  # either is in range and must be silent. The range test is strict (< 0, > 1),
  # and loosening it to <= or >= would warn on a legitimate probability, which a
  # saturated fit can report. Called directly, since landing the estimate
  # exactly on a boundary through the optimizer is not controllable.
  warn_fn <- getFromNamespace("warn_if_outcome_outside_range", "LAGO")
  for (boundary in c(0, 1)) {
    expect_no_warning(warn_fn(boundary, "binary", "identity"))
    expect_no_warning(warn_fn(boundary, "binary", "logit"))
  }
  # just outside each boundary does warn, so the silence above is the closed
  # boundary and not a dead check
  expect_warning(warn_fn(-1e-6, "binary", "identity"), "outside")
  expect_warning(warn_fn(1 + 1e-6, "binary", "identity"), "outside")
})

test_that("the range warning fires once per run, not once per grid point", {
  # the grid here is 10 x 3 = 30 interventions and the confidence set grid is
  # the same size, so a per-point warning would flood. Counting them is the
  # assertion.
  ws <- out_of_range_warnings()
  expect_identical(sum(grepl("not a probability", ws)), 1L)
})

test_that("the range warning does not fire where the estimate is in range", {
  # the same fixture with bounds INSIDE the fitted range: the estimate is a
  # probability, so there is nothing to warn about and the run must be silent
  # on this account.
  res <- suppressWarnings(out_of_range_run(
    intervention_upper_bounds = c(4, 3), outcome_goal = 0.6
  ))
  ws <- out_of_range_warnings(
    intervention_upper_bounds = c(4, 3), outcome_goal = 0.6
  )
  expect_lte(res$est_outcome_goal, 1)
  expect_gte(res$est_outcome_goal, 0)
  expect_identical(sum(grepl("not a probability", ws)), 0L)
})

test_that("a minimize run whose estimate is a probability does not warn", {
  # THE case that decides where this check belongs. get_outcome() is evaluated
  # on the FLIPPED outcome scale under "minimize", where the identity-link flip
  # is a negation, so every value it returns on such a run is negative even
  # when the reported estimate is a perfectly good probability. A guard placed
  # inside get_outcome() would fire on all of them. This asserts the check sees
  # the value as REPORTED, on the caller's own scale.
  min_args <- list(
    intervention_upper_bounds = c(4, 3),
    outcome_goal = 0.2,
    outcome_goal_intention = "minimize",
    include_confidence_set = FALSE
  )
  res <- suppressWarnings(do.call(out_of_range_run, min_args))
  ws <- do.call(out_of_range_warnings, min_args)
  expect_gte(res$est_outcome_goal, 0)
  expect_lte(res$est_outcome_goal, 1)
  expect_identical(sum(grepl("not a probability", ws)), 0L)
})

test_that("a continuous outcome outside [0, 1] does not warn", {
  # a continuous outcome's range is not knowable here, which is the same reason
  # get_confidence_set() does not confine its interval. mpg is far outside
  # [0, 1] and must pass without this warning, which is what a check keyed on
  # the link alone, or one placed where outcome_type does not reach, would get
  # wrong.
  ws <- character(0)
  res <- withCallingHandlers(
    suppressMessages(lago_optimization(
      data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
      glm_family = "gaussian", link = "identity",
      intervention_components = c("gear", "qsec"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 350),
      cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      outcome_goal = 40, outcome_goal_intention = "maximize",
      include_confidence_set = FALSE, quiet = TRUE
    )),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_gt(res$est_outcome_goal, 1)
  expect_identical(sum(grepl("not a probability", ws)), 0L)
})

test_that("warning or not, the recommendation is the one the model implies", {
  # the reason this WARNS rather than clamps. get_outcome() drives every
  # optimizer and the goal comparison, so clamping the estimate would change
  # which intervention is recommended. Here the recommendation is held against
  # the cheapest grid intervention whose own linear predictor meets the goal,
  # computed from coef() alone: that is what the optimizer is supposed to
  # return, and a clamped objective could not, since every intervention above
  # the boundary would tie at 1.
  d <- out_of_range_fixture()
  model <- suppressWarnings(glm(
    y ~ x1 + x2, data = d, family = binomial(link = "identity")
  ))
  res <- suppressWarnings(out_of_range_run())

  grid <- expand.grid(x1 = seq(0, 9, by = 1), x2 = seq(1, 3, by = 1))
  grid$est <- apply(grid, 1, function(r) {
    as.numeric(c(1, r[["x1"]], r[["x2"]]) %*% coef(model))
  })
  grid$cost <- grid$x1 + grid$x2
  ok <- grid[grid$est >= 1.3, ]
  expect_gt(nrow(ok), 1)
  best <- ok[which.min(ok$cost), ]
  expect_identical(res$rec_int, c(best$x1, best$x2))
  # and the objective is NOT flat above the boundary, which is what clamping
  # would have made it: the qualifying interventions have distinct estimates.
  expect_gt(length(unique(ok$est)), 1)
})
