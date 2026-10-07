covariate_contrast_data <- function(levels = c("a", "b")) {
  d <- expand.grid(x = 0:10, grp = levels, repeat_id = 1:4)
  d$grp <- factor(d$grp, levels = levels)
  effects <- seq_along(levels) * 2 - 2
  d$y <- 1 + 2 * d$x + effects[as.integer(d$grp)] +
    c(-0.15, -0.05, 0.05, 0.15)[d$repeat_id]
  d
}

covariate_contrast_run <- function(data, ...) {
  suppressWarnings(lago_optimization(
    data = data, outcome_name = "y", outcome_type = "continuous",
    glm_family = "gaussian", link = "identity",
    intervention_components = "x", intervention_lower_bounds = 0,
    intervention_upper_bounds = 10, cost_list_of_vectors = list(c(0, 1)),
    outcome_goal = 10.5, optimization_method = "grid_search",
    optimization_grid_search_step_size = 0.5,
    confidence_set_grid_step_size = 0.25, quiet = TRUE, ...
  ))
}

covariate_contrast_key <- function(res) {
  list(rec_int = res$rec_int, cost = res$rec_int_cost,
       est = res$est_outcome_goal, ci = res$est_outcome_ci, cs = res$cs)
}

test_that("categorical covariates keep reference predictions under global contrasts", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  d <- covariate_contrast_data()
  plain <- covariate_contrast_run(d, additional_covariates = "grp",
    include_confidence_set = FALSE)
  options(contrasts = c("contr.sum", "contr.helmert"))
  summed <- covariate_contrast_run(d, additional_covariates = "grp",
    include_confidence_set = FALSE)
  expect_identical(getOption("contrasts"), c("contr.sum", "contr.helmert"))
  expect_equal(fitted(summed$model), fitted(plain$model))
  expect_equal(covariate_contrast_key(summed), covariate_contrast_key(plain))
  expect_equal(as.numeric(plain$rec_int), 5)
  expect_identical(summed$model$call$contrasts, list(grp = "contr.treatment"))
})

test_that("categorical covariates have coding invariant continuous confidence output", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  for (lv in list(c("b", "a"), c("c", "a", "b"))) {
    d <- covariate_contrast_data(lv)
    plain <- covariate_contrast_run(d, additional_covariates = "grp")
    expect_false(is.null(plain$cs))
    expect_length(plain$est_outcome_ci, 2)
    for (coding in c("contr.sum", "contr.helmert")) {
      options(contrasts = c(coding, "contr.poly"))
      for (kind in c("factor", "ordered", "character")) {
        changed <- d
        if (kind == "ordered") changed$grp <- ordered(d$grp, levels = lv)
        if (kind == "character") {
          changed$grp <- as.character(d$grp)
          reference <- d
          reference$grp <- factor(as.character(d$grp))
          options(contrasts = c("contr.treatment", "contr.poly"))
          baseline <- covariate_contrast_run(reference, additional_covariates = "grp")
          options(contrasts = c(coding, "contr.poly"))
        } else {
          baseline <- plain
        }
        result <- covariate_contrast_run(changed, additional_covariates = "grp")
        expect_equal(covariate_contrast_key(result), covariate_contrast_key(baseline))
        expect_equal(coef(result$model), coef(baseline$model))
        expect_identical(result$model$contrasts, list(grp = "contr.treatment"))
        expect_identical(getOption("contrasts"), c(coding, "contr.poly"))
      }
    }
    options(contrasts = c("contr.treatment", "contr.poly"))
  }
})

test_that("two level characteristics retain treatment indicator semantics", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  d <- covariate_contrast_data()
  for (value in c(0, 1)) {
    plain <- covariate_contrast_run(d, center_characteristics = "grp",
      center_characteristics_optimization_values = value)
    expect_false(is.null(plain$cs))
    for (kind in c("factor", "ordered", "character", "logical")) {
      changed <- d
      if (kind == "ordered") changed$grp <- ordered(d$grp)
      if (kind == "character") changed$grp <- as.character(d$grp)
      if (kind == "logical") changed$grp <- d$grp == "b"
      options(contrasts = c("contr.sum", "contr.poly"))
      result <- covariate_contrast_run(changed, center_characteristics = "grp",
        center_characteristics_optimization_values = value)
      expect_equal(covariate_contrast_key(result), covariate_contrast_key(plain))
      expect_identical(result$model$contrasts, list(grp = "contr.treatment"))
      expect_identical(getOption("contrasts"), c("contr.sum", "contr.poly"))
      at <- changed[1, , drop = FALSE]
      at$x <- as.numeric(result$rec_int)
      at$grp <- if (kind == "logical") value == 1 else {
        factor(if (value == 1) "b" else "a", levels = levels(d$grp),
          ordered = kind == "ordered")
      }
      expect_equal(as.numeric(predict(result$model, at, type = "response")),
        as.numeric(result$est_outcome_goal))
    }
    options(contrasts = c("contr.treatment", "contr.poly"))
  }
  expect_error(covariate_contrast_run(covariate_contrast_data(c("a", "b", "c")),
    center_characteristics = "grp", center_characteristics_optimization_values = 1),
    "Center characteristics with more than two levels are not supported")
})

test_that("explicit contrast attributes are overridden without changing input data", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  d <- covariate_contrast_data(c("c", "a", "b"))
  plain <- covariate_contrast_run(d, additional_covariates = "grp")
  for (ordered in c(FALSE, TRUE)) {
    changed <- d
    changed$grp <- factor(d$grp, levels = levels(d$grp), ordered = ordered)
    contrasts(changed$grp) <- contr.sum(3)
    before <- changed
    result <- covariate_contrast_run(changed, additional_covariates = "grp")
    expect_identical(changed, before)
    expect_equal(covariate_contrast_key(result), covariate_contrast_key(plain))
    expect_identical(result$model$contrasts, list(grp = "contr.treatment"))
  }
})

test_that("only model predictors receive contrasts and numeric controls are unchanged", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  d <- covariate_contrast_data()
  d$grp <- as.integer(d$grp) - 1
  d$unused <- ordered(rep(c("a", "b"), length.out = nrow(d)))
  plain <- covariate_contrast_run(d, additional_covariates = "grp")
  options(contrasts = c("contr.sum", "contr.poly"))
  result <- covariate_contrast_run(d, additional_covariates = "grp")
  expect_null(result$model$contrasts)
  expect_null(result$model$call$contrasts)
  expect_equal(covariate_contrast_key(result), covariate_contrast_key(plain))
  expect_equal(coef(result$model), c("(Intercept)" = 1, x = 2, grp = 2))
  d$grp <- d$grp == 1
  logical_result <- covariate_contrast_run(d, additional_covariates = "grp")
  expect_equal(covariate_contrast_key(logical_result), covariate_contrast_key(plain))
  expect_identical(logical_result$model$call$contrasts, list(grp = "contr.treatment"))
})

test_that("multiple categorical covariates share the reference level convention", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  d <- expand.grid(x = 0:10, grp = c("c", "a", "b"), flag = c(FALSE, TRUE),
    repeat_id = 1:4)
  d$grp <- factor(d$grp, levels = c("c", "a", "b"))
  d$y <- 1 + 2 * d$x + 2 * (as.integer(d$grp) - 1) + 3 * d$flag +
    c(-0.15, -0.05, 0.05, 0.15)[d$repeat_id]
  plain <- covariate_contrast_run(d, additional_covariates = c("grp", "flag"))
  options(contrasts = c("contr.helmert", "contr.poly"))
  d$grp <- ordered(d$grp, levels = levels(d$grp))
  result <- covariate_contrast_run(d, additional_covariates = c("grp", "flag"))
  expect_equal(covariate_contrast_key(result), covariate_contrast_key(plain))
  expect_equal(coef(result$model), coef(plain$model))
  expect_identical(result$model$contrasts,
    list(grp = "contr.treatment", flag = "contr.treatment"))
})

test_that("numeric interaction columns stay numeric beside categorical covariates", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  d <- expand.grid(x = 0:4, z = 0:2, grp = c("a", "b"), repeat_id = 1:4)
  d$grp <- factor(d$grp)
  d[["x:z"]] <- d$x * d$z
  d$y <- 1 + 2 * d$x + d$z + 0.5 * d[["x:z"]] + 2 * (d$grp == "b") +
    c(-0.15, -0.05, 0.05, 0.15)[d$repeat_id]
  run <- function(data) suppressWarnings(lago_optimization(
    data = data, outcome_name = "y", outcome_type = "continuous",
    intervention_components = c("x", "z", "x:z"),
    main_components = c("x", "z"), include_interaction_terms = TRUE,
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(4, 2),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 10.5,
    additional_covariates = "grp", optimization_method = "grid_search",
    optimization_grid_search_step_size = c(0.5, 0.5),
    confidence_set_grid_step_size = c(0.25, 0.25), quiet = TRUE
  ))
  plain <- run(d)
  options(contrasts = c("contr.sum", "contr.poly"))
  d$grp <- ordered(d$grp)
  result <- run(d)
  expect_equal(covariate_contrast_key(result), covariate_contrast_key(plain))
  expect_identical(result$model$call$contrasts, list(grp = "contr.treatment"))
  expect_equal(unname(coef(result$model)), c(1, 2, 1, 0.5, 2))
  expect_true(is.numeric(result$model$model[["x:z"]]))
})

test_that("binary confidence output is invariant for ordered additional covariates", {
  previous <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(previous))
  d <- as.data.frame(BB_data)
  d$site <- factor(rep(c("a", "b"), length.out = nrow(d)))
  run <- function(data) suppressWarnings(lago_optimization(
    data = data, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.85,
    additional_covariates = "site", confidence_set_grid_step_size = c(2, 0.15),
    quiet = TRUE
  ))
  plain <- run(d)
  options(contrasts = c("contr.helmert", "contr.poly"))
  d$site <- ordered(d$site)
  result <- run(d)
  expect_false(is.null(plain$cs))
  expect_equal(covariate_contrast_key(result), covariate_contrast_key(plain))
  expect_equal(fitted(result$model), fitted(plain$model))
  expect_identical(result$model$contrasts, list(site = "contr.treatment"))
})
