# get_confidence_set() reads every categorical predictor as treatment coded with
# the first level as the reference and each coefficient named after its level,
# so a model fitted any other way is refused.

coding_data <- function() {
  d <- mtcars[, c("mpg", "gear", "qsec")]
  d$center <- factor(paste0("c", (seq_len(nrow(d)) %% 4) + 1))
  d$period <- factor(c("a", "b", "c")[(seq_len(nrow(d)) %% 3) + 1])
  d$grp <- factor(c("a", "b", "b", "a", "b", "a", "a", "b")[(seq_len(nrow(d)) %% 8) + 1])
  d$y <- as.numeric(d$mpg > 20)
  d
}

coding_fit <- function(d, formula, family = binomial(), ...) {
  suppressWarnings(glm(formula, data = d, family = family, ...))
}

coding_cs <- function(d, model, center = TRUE, period = NULL, cov = NULL,
                      cha = NULL, type = "binary") {
  predictors <- c(
    if (center) "center", if (!is.null(period)) "period", "gear", "qsec",
    cov, cha
  )
  suppressWarnings(get_confidence_set(
    predictors_data = d[, predictors, drop = FALSE],
    include_center_effects = center,
    center_weights_for_outcome_goal = if (center) {
      as.numeric(levels(d$center) == "c2")
    } else {
      1
    },
    include_time_effects = !is.null(period),
    time_effect_optimization_value = period,
    additional_covariates = cov,
    center_characteristics = cha,
    center_characteristics_optimization_values = if (is.null(cha)) 0 else 1,
    intervention_components = c("gear", "qsec"),
    outcome_data = if (type == "binary") d$y else d$mpg,
    fitted_model = model,
    link = if (type == "binary") "logit" else "identity",
    outcome_goal = if (type == "binary") 0.5 else 30,
    outcome_type = type,
    intervention_lower_bounds = c(3, 14),
    intervention_upper_bounds = c(5, 22),
    confidence_set_grid_step_size = c(1, 2),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    rec_int = c(4, 18)
  ))
}

coding_message <- function(columns, codings, hint_names = columns) {
  paste0(
    "The fitted model does not code ",
    paste0(columns, " (", codings, ")", collapse = ", "),
    " the way get_confidence_set() reads every factor, character and logical ",
    "predictor, which is treatment contrasts with the first level as the ",
    "reference and each coefficient named after its level, so its result ",
    "would be wrong. Refit the model with ",
    "contrasts = list(",
    paste0(hint_names, ' = "contr.treatment"', collapse = ", "),
    "), as lago_optimization() does."
  )
}

coding_error <- function(expr) {
  tryCatch(
    {
      expr
      "no error"
    },
    error = conditionMessage
  )
}

test_that("a center effect coded another way is refused", {
  d <- coding_data()
  f <- y ~ center + gear + qsec
  for (coding in c("contr.sum", "contr.SAS", "contr.helmert")) {
    model <- coding_fit(d, f, contrasts = list(center = coding))
    expect_identical(
      coding_error(coding_cs(d, model)),
      coding_message("center", coding)
    )
  }
  model <- coding_fit(d, f, contrasts = list(center = contr.treatment(4, base = 3)))
  expect_identical(
    coding_error(coding_cs(d, model)),
    coding_message("center", "a custom contrast matrix")
  )
})

test_that("a treatment matrix whose columns are not named after the levels is refused", {
  d <- coding_data()
  d$period <- factor((seq_len(nrow(d)) %% 3))
  model <- coding_fit(d, y ~ center + period + gear + qsec,
    contrasts = list(period = contr.treatment(3))
  )
  expect_identical(
    coding_error(coding_cs(d, model, period = "2")),
    coding_message(
      "period", "a treatment matrix whose columns are not named after the levels"
    )
  )
  model <- coding_fit(d, y ~ center + gear + qsec,
    contrasts = list(center = contr.treatment(4))
  )
  expect_identical(
    coding_error(coding_cs(d, model)),
    coding_message(
      "center", "a treatment matrix whose columns are not named after the levels"
    )
  )
})

test_that("an ordered center fitted with its default coding is refused", {
  d <- coding_data()
  d$center <- factor(d$center, ordered = TRUE)
  model <- coding_fit(d, y ~ center + gear + qsec)
  expect_identical(
    coding_error(coding_cs(d, model)),
    coding_message("center", "contr.poly")
  )
})

test_that("a global sum coding is refused", {
  d <- coding_data()
  old <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old), add = TRUE)
  model <- coding_fit(d, y ~ center + gear + qsec)
  expect_identical(
    coding_error(coding_cs(d, model)),
    coding_message("center", "contr.sum")
  )
})

test_that("a period, covariate or center characteristic coded another way is refused", {
  d <- coding_data()
  model <- coding_fit(d, y ~ center + period + gear + qsec,
    contrasts = list(period = "contr.sum")
  )
  expect_identical(
    coding_error(coding_cs(d, model, period = "a")),
    coding_message("period", "contr.sum")
  )
  model <- coding_fit(d, y ~ gear + qsec + grp, contrasts = list(grp = "contr.sum"))
  expect_identical(
    coding_error(coding_cs(d, model, center = FALSE, cov = "grp")),
    coding_message("grp", "contr.sum")
  )
  expect_identical(
    coding_error(coding_cs(d, model, center = FALSE, cha = "grp")),
    coding_message("grp", "contr.sum")
  )
})

test_that("a logical predictor coded another way is refused", {
  d <- coding_data()
  d$flag <- rep_len(c(TRUE, FALSE, FALSE), nrow(d))
  model <- coding_fit(d, y ~ center + gear + qsec + flag,
    contrasts = list(flag = "contr.sum")
  )
  expect_identical(
    coding_error(coding_cs(d, model, cov = "flag")),
    coding_message("flag", "contr.sum")
  )
})

test_that("a continuous outcome with a 0 and 1 factor coded another way is refused", {
  d <- coding_data()
  d$grp <- factor(as.integer(d$grp == "b"))
  model <- coding_fit(d, mpg ~ center + gear + qsec + grp,
    family = gaussian(), contrasts = list(grp = "contr.sum")
  )
  expect_identical(
    coding_error(coding_cs(d, model, cov = "grp", type = "continuous")),
    coding_message("grp", "contr.sum")
  )
})

test_that("a factor the confidence set does not read is left to the coefficient check", {
  d <- coding_data()
  model <- coding_fit(d, y ~ center + gear + qsec + grp,
    contrasts = list(grp = "contr.sum")
  )
  err <- coding_error(coding_cs(d, model))
  expect_match(err, "^The coefficients of 'fitted_model' do not match")
})

test_that("the refit hint quotes a column name that is not syntactic", {
  d <- coding_data()
  d$`my grp` <- d$grp
  model <- coding_fit(d, y ~ center + gear + qsec + `my grp`,
    contrasts = list(`my grp` = "contr.sum")
  )
  err <- coding_error(coding_cs(d, model, cov = "my grp"))
  expect_identical(err, coding_message("my grp", "contr.sum", "`my grp`"))
  hint <- sub(".*Refit the model with (.*), as lago_optimization\\(\\) does\\.$", "\\1", err)
  expect_no_error(parse(text = hint))
})

test_that("every column coded another way is named", {
  d <- coding_data()
  model <- coding_fit(d, y ~ center + gear + qsec + grp,
    contrasts = list(center = "contr.sum", grp = "contr.helmert")
  )
  expect_identical(
    coding_error(coding_cs(d, model, cov = "grp")),
    coding_message(c("center", "grp"), c("contr.sum", "contr.helmert"))
  )
})

test_that("a continuous outcome gets the same refusal", {
  d <- coding_data()
  model <- coding_fit(d, mpg ~ center + gear + qsec,
    family = gaussian(), contrasts = list(center = "contr.sum")
  )
  expect_identical(
    coding_error(coding_cs(d, model, type = "continuous")),
    coding_message("center", "contr.sum")
  )
})

test_that("treatment coding with the first level as the reference is accepted", {
  d <- coding_data()
  f <- y ~ center + period + gear + qsec + grp
  expected <- coding_cs(d, coding_fit(d, f), period = "b", cov = "grp")
  named <- coding_fit(d, f, contrasts = list(
    center = "contr.treatment", period = "contr.treatment", grp = "contr.treatment"
  ))
  expect_identical(coding_cs(d, named, period = "b", cov = "grp"), expected)
  named_matrix <- coding_fit(d, f, contrasts = list(center = contr.treatment(levels(d$center))))
  expect_identical(coding_cs(d, named_matrix, period = "b", cov = "grp"), expected)
  d$flag <- rep_len(c(TRUE, FALSE, FALSE), nrow(d))
  logical_model <- coding_fit(d, y ~ center + gear + qsec + flag)
  expect_no_error(coding_cs(d, logical_model, cov = "flag"))
})

test_that("an explicit NA center level is accepted", {
  d <- coding_data()
  d$center[c(2, 9, 17, 30)] <- NA
  d$center <- addNA(d$center)
  f <- y ~ center + gear + qsec
  call_cs <- function(model) {
    suppressWarnings(get_confidence_set(
      predictors_data = d[, c("center", "gear", "qsec")],
      include_center_effects = TRUE,
      center_weights_for_outcome_goal = c(0, 1, 0, 0, 0),
      intervention_components = c("gear", "qsec"), outcome_data = d$y,
      fitted_model = model, link = "logit", outcome_goal = 0.5,
      outcome_type = "binary", intervention_lower_bounds = c(3, 14),
      intervention_upper_bounds = c(5, 22), confidence_set_grid_step_size = c(1, 2),
      cost_list_of_vectors = list(c(0, 4), c(4, 6)), rec_int = c(4, 18)
    ))
  }
  expected <- call_cs(coding_fit(d, f, contrasts = list(center = "contr.treatment")))
  expect_identical(call_cs(coding_fit(d, f)), expected)
  expect_true(any(is.na(levels(d$center))))
})
