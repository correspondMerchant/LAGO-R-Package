# A sweep fits the outcome model once and reuses it, with the same results.

fit_cache_args <- function() {
  d <- as.data.frame(BB_data)
  d$center <- d$site_name
  list(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.85,
    outcome_goal_intention = "maximize", include_center_effects = TRUE
  )
}

# Count the outcome model fits while the real fitting still runs.
count_fits <- function(env = parent.frame()) {
  fits <- new.env()
  fits$n <- 0
  real <- LAGOtrials:::outcome_model_fitting
  local_mocked_bindings(
    outcome_model_fitting = function(...) {
      fits$n <- fits$n + 1
      real(...)
    },
    .env = env
  )
  fits
}

fit_cache_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("a sensitivity sweep fits the outcome model once", {
  fits <- count_fits()
  fit_cache_quiet(do.call(lago_sensitivity, c(
    fit_cache_args(),
    list(parameter = "outcome_goal", values = c(0.8, 0.84, 0.88, 0.92))
  )))
  expect_identical(fits$n, 1)
})

test_that("a budget search fits the outcome model once across its sweeps", {
  fits <- count_fits()
  args <- fit_cache_args()
  args$outcome_goal <- NULL
  fit_cache_quiet(do.call(lago_budget, c(args, list(budget = 20, n_grid = 4))))
  expect_identical(fits$n, 1)
})

test_that("a cost sweep fits the outcome model once", {
  fits <- count_fits()
  fit_cache_quiet(do.call(lago_sensitivity, c(
    fit_cache_args(),
    list(parameter = "cost_multiplier", values = c(0.5, 1, 2))
  )))
  expect_identical(fits$n, 1)
})

test_that("lago_optimization() alone fits every time", {
  fits <- count_fits()
  fit_cache_quiet(do.call(lago_optimization, c(fit_cache_args(), list(quiet = TRUE))))
  fit_cache_quiet(do.call(lago_optimization, c(fit_cache_args(), list(quiet = TRUE))))
  expect_identical(fits$n, 2)
})

test_that("each sweep row matches a single lago_optimization() run", {
  values <- c(0.8, 0.86, 0.92)
  sens <- fit_cache_quiet(do.call(lago_sensitivity, c(
    fit_cache_args(),
    list(parameter = "outcome_goal", values = values)
  )))
  for (i in seq_along(values)) {
    args <- fit_cache_args()
    args$outcome_goal <- values[i]
    single <- fit_cache_quiet(do.call(lago_optimization, c(
      args,
      list(include_confidence_set = FALSE, quiet = TRUE)
    )))
    expect_equal(sens$est_outcome_goal[i], single$est_outcome_goal)
    expect_equal(sens$rec_int_cost[i], single$rec_int_cost)
  }
})

test_that("fit warnings are still raised for every run of a sweep", {
  args <- fit_cache_args()
  # a copy of a covariate cannot be estimated, so every fit warns about it
  args$data$birth_copy <- args$data$birth_volume_100
  args$additional_covariates <- c("birth_volume_100", "birth_copy")
  warned <- 0
  withCallingHandlers(
    suppressMessages(do.call(lago_sensitivity, c(
      args,
      list(parameter = "outcome_goal", values = c(0.8, 0.85, 0.9))
    ))),
    warning = function(w) {
      if (grepl("could not be estimated by glm()", conditionMessage(w), fixed = TRUE)) {
        warned <<- warned + 1
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(warned, 3)
})

test_that("the cache is only active during a sweep", {
  fit_cache_quiet(do.call(lago_sensitivity, c(
    fit_cache_args(),
    list(parameter = "outcome_goal", values = c(0.8, 0.85))
  )))
  expect_false(isTRUE(LAGOtrials:::.lago_fit_cache$active))
  expect_length(LAGOtrials:::.lago_fit_cache$entries, 0)
})

test_that("the cache tells apart fits with different inputs", {
  d <- fit_cache_args()$data
  fit <- function(w) {
    LAGOtrials:::cached_outcome_model_fitting(
      data = d, outcome_name = "pp3_oxytocin_mother", family_object = binomial(),
      intervention_components = c("coaching_updt", "launch_duration"),
      weights = w, center_characteristics = NULL, additional_covariates = NULL
    )$model
  }
  fits <- count_fits()
  LAGOtrials:::with_fit_cache({
    a <- suppressWarnings(fit(NULL))
    b <- suppressWarnings(fit(NULL))
    c <- suppressWarnings(fit(rep(2, nrow(d))))
    old <- options(contrasts = c("contr.sum", "contr.poly"))
    on.exit(options(old), add = TRUE)
    e <- suppressWarnings(fit(NULL))
  })
  expect_identical(fits$n, 3)
  expect_identical(a, b)
})
