# Fixed center and period effects need an unordered factor, two usable levels.

factor_run <- function(data, ...) {
  suppressWarnings(lago_optimization(
    data = data,
    outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal = 0.85, include_center_effects = TRUE,
    confidence_set_grid_step_size = c(2, 0.15), quiet = TRUE,
    ...
  ))
}

factor_key <- function(res) {
  list(
    rec_int = res$rec_int, est = res$est_outcome_goal,
    ci = res$est_outcome_ci, cs = res$cs
  )
}

# reversed, so the reference level is not the alphabetical first
center_levels <- rev(sort(unique(BB_data$site_name)))

test_that("an ordered center is fitted with one effect per center", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name, levels = center_levels)
  plain <- suppressMessages(factor_run(d))
  d$center <- factor(d$site_name, levels = center_levels, ordered = TRUE)
  expect_message(
    ordered <- factor_run(d), "'center' column is an ordered factor"
  )
  expect_false(is.null(ordered$cs))
  expect_equal(factor_key(ordered), factor_key(plain))
  # the printed Call shows the coding the model was fitted with
  expect_identical(
    ordered$model$call$contrasts, list(center = "contr.treatment")
  )
})

test_that("an ordered period is fitted with one effect per period", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  periods <- (seq_len(nrow(d)) %% 3) + 1
  args <- list(include_time_effects = TRUE, time_effect_optimization_value = 2)
  # period 3 is the first level, where main returned a wrong result silently
  for (value in c(2, 3)) {
    args$time_effect_optimization_value <- value
    d$period <- factor(periods, levels = c(3, 1, 2))
    plain <- suppressMessages(do.call(factor_run, c(list(d), args)))
    d$period <- factor(periods, levels = c(3, 1, 2), ordered = TRUE)
    expect_message(
      ordered <- do.call(factor_run, c(list(d), args)),
      "'period' column is an ordered factor"
    )
    expect_false(is.null(ordered$cs))
    expect_equal(factor_key(ordered), factor_key(plain))
  }
})

test_that("a global contrasts option does not change the center coding", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  d$period <- factor((seq_len(nrow(d)) %% 3) + 1)
  args <- list(include_time_effects = TRUE, time_effect_optimization_value = 2)
  plain <- suppressMessages(do.call(factor_run, c(list(d), args)))
  previous <- options(contrasts = c("contr.sum", "contr.poly"))
  summed <- tryCatch(
    {
      res <- suppressMessages(do.call(factor_run, c(list(d), args)))
      # the run leaves the caller's own option as it found it
      left <- getOption("contrasts")[[1]]
      res
    },
    finally = options(previous)
  )
  expect_identical(left, "contr.sum")
  expect_equal(factor_key(summed), factor_key(plain))
  expect_identical(
    summed$model$call$contrasts,
    list(center = "contr.treatment", period = "contr.treatment")
  )
})

test_that("fixed center effects with one usable center are refused clearly", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  set.seed(1)
  d$cov <- stats::rnorm(nrow(d))
  d$cov[d$center != "Auras"] <- NA
  expect_error(
    suppressMessages(factor_run(d, additional_covariates = "cov")),
    "at least two centers .* only the center 'Auras' has one"
  )
  # every row misses one of two covariates, though neither is entirely NA
  odd <- seq_len(nrow(d)) %% 2 == 1
  d$cov <- ifelse(odd, NA, 1)
  d$cov2 <- ifelse(odd, 1, NA)
  err <- tryCatch(
    suppressMessages(factor_run(d, additional_covariates = c("cov", "cov2"))),
    error = conditionMessage
  )
  expect_match(err, "No row has every model variable")
  # turning center effects off leaves no usable row here either
  expect_no_match(err, "_effects = FALSE")
  expect_match(err, "Fill in the missing values\\.$")
  # with a single center as well, the message asks for both
  err <- tryCatch(
    suppressMessages(factor_run(
      d[d$center == "Auras", ], additional_covariates = c("cov", "cov2")
    )),
    error = conditionMessage
  )
  expect_match(err, "the data has only the center 'Auras'")
  expect_match(err, "fill those in too")
  # a period with no observed value names the column, whatever the na.action
  d$period <- NA_real_
  err <- tryCatch(
    suppressMessages(factor_run(
      d,
      include_time_effects = TRUE, time_effect_optimization_value = 1
    )),
    error = conditionMessage
  )
  expect_match(err, "the data has no observed period. Fill in the period column")
  # nothing else is missing, so the message stops at the turn-off advice
  expect_match(err, "include_time_effects = FALSE\\.$")
  # a period observed only where a covariate is missing: turning it off still fits
  d$period <- ifelse(seq_len(nrow(d)) %% 2 == 1, 1, NA)
  d$cov <- ifelse(seq_len(nrow(d)) %% 2 == 1, NA, 1)
  err <- tryCatch(
    suppressMessages(factor_run(
      d,
      additional_covariates = "cov", include_time_effects = TRUE,
      time_effect_optimization_value = 1
    )),
    error = conditionMessage
  )
  expect_match(err, "the data has only the period '1'")
  expect_no_match(err, "fill those in too")
  # two periods, but each observed only where the covariate is missing
  d$period <- ifelse(seq_len(nrow(d)) %% 2 == 1, seq_len(nrow(d)) %% 4, NA)
  err <- tryCatch(
    suppressMessages(factor_run(
      d,
      additional_covariates = "cov", include_time_effects = TRUE,
      time_effect_optimization_value = 1
    )),
    error = conditionMessage
  )
  expect_match(err, "values, or set include_time_effects = FALSE\\.$")
  # not offered when the other effect would then have only one usable level
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  auras <- d$center == "Auras"
  d$cov <- ifelse(auras, 1, NA)
  d$period <- ifelse(auras, NA, (seq_len(nrow(d)) %% 2) + 1)
  err <- tryCatch(
    suppressMessages(factor_run(
      d,
      additional_covariates = "cov", include_time_effects = TRUE,
      time_effect_optimization_value = 1
    )),
    error = conditionMessage
  )
  expect_match(err, "Fill in the missing values\\.$")
})

test_that("data with a single center is refused clearly", {
  d <- as.data.frame(BB_data)
  d <- d[d$site_name == "Auras", ]
  d$center <- factor(d$site_name)
  expect_error(
    suppressMessages(factor_run(d)),
    "the data has only the center 'Auras'. .*include_center_effects = FALSE"
  )
  # center level data always fits center effects, so that is not offered
  cl <- data.frame(
    center = factor(rep("c1", 4)), coaching_updt = c(0, 5, 10, 15),
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
      include_center_effects = TRUE, quiet = TRUE
    ))),
    error = conditionMessage
  )
  expect_match(err, "the data has only the center 'c1'")
  expect_no_match(err, "include_center_effects = FALSE")
})

test_that("an na.action that keeps missing rows still gets a clear refusal", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  previous <- options(na.action = "na.pass")
  on_pass <- function(data, ...) {
    tryCatch(
      suppressMessages(factor_run(data, ...)),
      error = conditionMessage,
      finally = options(previous)
    )
  }
  # a true NA kept in the frame is not counted as a second center
  one <- d[d$site_name == "Auras", ]
  one$center[1:3] <- NA
  expect_match(on_pass(one), "the data has only the center 'Auras'")
  options(na.action = "na.pass")
  d$period <- NA_real_
  err <- on_pass(
    d,
    include_time_effects = TRUE, time_effect_optimization_value = 1
  )
  expect_match(err, "the data has no observed period. Fill in the period")
  expect_identical(getOption("na.action"), previous$na.action)
  # the same message as under the default na.action
  expect_identical(err, tryCatch(
    suppressMessages(factor_run(
      d,
      include_time_effects = TRUE, time_effect_optimization_value = 1
    )),
    error = conditionMessage
  ))
})

test_that("fixed time effects with one usable period are refused clearly", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  d$period <- factor((seq_len(nrow(d)) %% 3) + 1)
  set.seed(1)
  d$cov <- stats::rnorm(nrow(d))
  d$cov[d$period != "2"] <- NA
  expect_error(
    suppressMessages(factor_run(
      d,
      additional_covariates = "cov", include_time_effects = TRUE,
      time_effect_optimization_value = 2
    )),
    "at least two periods .* only the period '2' has one"
  )
})

test_that("the usable-level check does not repeat the fit's warnings", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  d$period <- factor((seq_len(nrow(d)) %% 3) + 1)
  stats::contrasts(d$period) <- stats::contr.sum(3)
  set.seed(1)
  d$cov <- stats::rnorm(nrow(d))
  # period 3 has no usable row, so model.frame() drops its level and warns
  d$cov[d$period == "3"] <- NA
  seen <- character()
  withCallingHandlers(
    suppressMessages(lago_optimization(
      data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.85,
      additional_covariates = "cov", include_center_effects = TRUE,
      include_time_effects = TRUE, time_effect_optimization_value = 2,
      include_confidence_set = FALSE, quiet = TRUE
    )),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # once, inside the fit diagnostic, and not again from the check
  expect_equal(sum(grepl("contrasts dropped", seen)), 1)
})
