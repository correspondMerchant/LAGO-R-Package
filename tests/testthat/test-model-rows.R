# The confidence set and center weights must use what glm() fitted.

bb_center <- function() {
  d <- BB_data
  d$center <- d$site_name
  d
}

rows_run <- function(data, ...) {
  suppressMessages(lago_optimization(
    data = data,
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal_intention = "maximize",
    confidence_set_grid_step_size = c(2, 0.15),
    quiet = TRUE,
    ...
  ))
}

# the parts of a result that depend on which rows are used
rows_key <- function(res) {
  list(
    rec_int = res$rec_int, est = res$est_outcome_goal,
    ci = res$est_outcome_ci, cs = res$cs
  )
}

# a within-center covariate, NA for every row of one center, so glm() drops it
with_lost <- function(lost = "Purwa") {
  d <- bb_center()
  set.seed(11)
  d$cov <- stats::rnorm(nrow(d), 5, 1)
  d$cov[d$center == lost] <- NA
  d
}
lost_center <- function(d) as.character(unique(d$center[is.na(d$cov)]))

# renormalizing the kept weights rounds at 1e-17, which nudges the optimum
opt_tol <- 1e-4

binary_lost <- list(
  outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
  outcome_goal = 0.85, additional_covariates = "cov",
  include_center_effects = TRUE
)

test_that("a continuous outcome's confidence set uses only the fitted rows", {
  d <- bb_center()
  # pp2_hand_hygiene is missing on some rows, so glm() drops them
  expect_true(anyNA(d$pp2_hand_hygiene))
  args <- list(
    outcome_name = "leadership_updt", outcome_type = "continuous",
    outcome_goal = 0.93, additional_covariates = "pp2_hand_hygiene"
  )
  with_na <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  complete <- suppressWarnings(do.call(
    rows_run, c(list(d[!is.na(d$pp2_hand_hygiene), ]), args)
  ))
  expect_equal(rows_key(with_na), rows_key(complete))
})

test_that("a logit-link continuous confidence set uses only the fitted rows", {
  bbp <- as.data.frame(BB_proportions)
  n <- nrow(bbp)
  set.seed(2)
  bbp$cov <- stats::rnorm(n)
  bbp$cov[sample(n, n %/% 10)] <- NA
  bbp$center <- factor(rep_len(paste0("s", 1:6), n))
  bbp$period <- factor(sample(1:3, n, TRUE))
  logit_run <- function(data, ...) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = data, outcome_name = "EBP_proportions",
      outcome_type = "continuous", glm_family = "quasibinomial", link = "logit",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = 0.5,
      additional_covariates = "cov", quiet = TRUE, ...
    )))
  }
  complete <- bbp[!is.na(bbp$cov), ]
  # the compiled sandwich kernels unclustered, then clustered two ways
  clustered <- list(
    include_center_effects = TRUE, center_effects_optimization_values = "s2",
    include_time_effects = TRUE, time_effect_optimization_value = 2
  )
  for (extra in list(list(), clustered)) {
    with_na <- do.call(logit_run, c(list(bbp), extra))
    expect_false(is.null(with_na$cs))
    expect_equal(
      rows_key(with_na), rows_key(do.call(logit_run, c(list(complete), extra)))
    )
  }
})

test_that("center and time clustering use only the fitted rows", {
  d <- bb_center()
  d$period <- factor((seq_len(nrow(d)) %% 3) + 1)
  # a named center has weights that do not depend on the dropped rows
  args <- list(
    outcome_name = "leadership_updt", outcome_type = "continuous",
    outcome_goal = 0.93, additional_covariates = "pp2_hand_hygiene",
    include_center_effects = TRUE, center_effects_optimization_values = "Auras",
    include_time_effects = TRUE, time_effect_optimization_value = 2
  )
  with_na <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  complete <- suppressWarnings(do.call(
    rows_run, c(list(d[!is.na(d$pp2_hand_hygiene), ]), args)
  ))
  expect_true(all(is.finite(with_na$est_outcome_ci)))
  expect_equal(rows_key(with_na), rows_key(complete))
})

test_that("a center with no complete row is left out of the weights", {
  d <- with_lost()
  lost <- lost_center(d)
  expect_length(lost, 1)
  seen <- character()
  with_na <- withCallingHandlers(
    suppressMessages(do.call(rows_run, c(list(d), binary_lost))),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("left out of the average center", seen)))
  without <- suppressWarnings(do.call(
    rows_run, c(list(d[d$center != lost, ]), binary_lost)
  ))
  expect_false(is.null(with_na$cs))
  expect_equal(rows_key(with_na), rows_key(without), tolerance = opt_tol)
})

test_that("a continuous outcome with a lost center uses the fitted centers", {
  d <- with_lost()
  lost <- lost_center(d)
  d$period <- factor((seq_len(nrow(d)) %% 3) + 1)
  args <- list(
    outcome_name = "leadership_updt", outcome_type = "continuous",
    outcome_goal = 0.93, additional_covariates = "cov",
    include_center_effects = TRUE
  )
  # one-way center clustering, then two-way with the period
  by_time <- list(
    include_time_effects = TRUE, time_effect_optimization_value = 2
  )
  for (extra in list(list(), by_time)) {
    with_na <- suppressWarnings(do.call(rows_run, c(list(d), args, extra)))
    without <- suppressWarnings(
      do.call(rows_run, c(list(d[d$center != lost, ]), args, extra))
    )
    expect_false(is.null(with_na$cs))
    expect_equal(rows_key(with_na), rows_key(without), tolerance = opt_tol)
  }
})

test_that("a lost first-level center is handled like any other", {
  first <- sort(unique(BB_data$site_name))[1]
  d <- with_lost(first)
  with_na <- suppressWarnings(do.call(rows_run, c(list(d), binary_lost)))
  without <- suppressWarnings(do.call(
    rows_run, c(list(d[d$center != first, ]), binary_lost)
  ))
  expect_false(is.null(with_na$cs))
  expect_equal(rows_key(with_na), rows_key(without), tolerance = opt_tol)
})

test_that("unused center levels do not shift the weights", {
  d <- with_lost()
  d$center <- factor(d$center)
  # a level with no rows left, as after subsetting a factor
  d <- d[d$center != "Auras", ]
  expect_true("Auras" %in% levels(d$center))
  dropped <- d
  dropped$center <- droplevels(dropped$center)
  seen <- character()
  with_unused <- withCallingHandlers(
    do.call(rows_run, c(list(d), binary_lost)),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # the unused level is not a center, so it is not reported as left out
  expect_false(any(grepl("Auras", seen)))
  expect_false(is.null(with_unused$cs))
  expect_equal(
    rows_key(with_unused),
    rows_key(suppressWarnings(
      do.call(rows_run, c(list(dropped), binary_lost))
    )),
    tolerance = opt_tol
  )
  # user weights count only the centers present
  centers <- levels(dropped$center)
  raw <- ifelse(centers == lost_center(d), 0, seq_along(centers))
  w <- list(center_weights_for_outcome_goal = raw / sum(raw))
  expect_equal(
    rows_key(suppressWarnings(do.call(rows_run, c(list(d), binary_lost, w)))),
    rows_key(suppressWarnings(
      do.call(rows_run, c(list(dropped), binary_lost, w))
    ))
  )
})

test_that("the sweep weights a center with no complete row correctly", {
  d <- with_lost()
  sweep <- function(data) {
    suppressMessages(lago_sensitivity(
      data = data, outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.8,
      outcome_goal_intention = "maximize", additional_covariates = "cov",
      include_center_effects = TRUE, quiet = TRUE,
      parameter = "outcome_goal", values = c(0.8, 0.85)
    ))
  }
  seen <- character()
  with_na <- withCallingHandlers(sweep(d), warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_false(any(grepl("longer object length", seen)))
  without <- suppressWarnings(sweep(d[d$center != lost_center(d), ]))
  expect_equal(with_na$rec_int_cost, without$rec_int_cost, tolerance = opt_tol)
  expect_equal(
    with_na$est_outcome_goal, without$est_outcome_goal, tolerance = opt_tol
  )
})

test_that("a named center with no complete row is refused clearly", {
  d <- with_lost()
  expect_error(
    suppressWarnings(do.call(rows_run, c(
      list(d), binary_lost,
      list(center_effects_optimization_values = lost_center(d))
    ))),
    "has no row the outcome model could use"
  )
})

test_that("a named center is optimized for when another center is lost", {
  d <- with_lost()
  # it sorts right after the lost Purwa, so a shifted indicator picks another
  args <- c(
    binary_lost, list(center_effects_optimization_values = "Ramiya Behar")
  )
  with_na <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  without <- suppressWarnings(do.call(
    rows_run, c(list(d[d$center != lost_center(d), ]), args)
  ))
  expect_false(is.null(with_na$cs))
  expect_equal(rows_key(with_na), rows_key(without))
})

test_that("the default weights size each fitted center by all of its rows", {
  d <- with_lost()
  centers <- sort(unique(as.character(d$center)))
  lost <- lost_center(d)
  # half of a fitted center's rows are dropped too, so the two sizes differ
  auras <- which(d$center == "Auras")
  d$cov[auras[seq_len(length(auras) %/% 2)]] <- NA
  sizes <- as.numeric(table(d$center)[centers])
  sizes[centers == lost] <- 0
  default <- suppressWarnings(do.call(rows_run, c(list(d), binary_lost)))
  by_all_rows <- suppressWarnings(do.call(rows_run, c(
    list(d), binary_lost,
    list(center_weights_for_outcome_goal = sizes / sum(sizes))
  )))
  expect_false(is.null(default$cs))
  expect_equal(rows_key(default), rows_key(by_all_rows), tolerance = opt_tol)
})

test_that("user weights on a center with no complete row must be 0", {
  d <- with_lost()
  centers <- sort(unique(as.character(d$center)))
  lost <- lost_center(d)
  even <- rep(1 / length(centers), length(centers))
  expect_error(
    suppressWarnings(do.call(rows_run, c(
      list(d), binary_lost, list(center_weights_for_outcome_goal = even)
    ))),
    "non-zero weight"
  )
  # unequal weights, so a helper that put them on the wrong centers would differ
  raw <- ifelse(centers == lost, 0, seq_along(centers))
  zero <- raw / sum(raw)
  with_na <- suppressWarnings(do.call(rows_run, c(
    list(d), binary_lost, list(center_weights_for_outcome_goal = zero)
  )))
  kept <- zero[centers != lost]
  without <- suppressWarnings(do.call(rows_run, c(
    list(d[d$center != lost, ]), binary_lost,
    list(center_weights_for_outcome_goal = kept / sum(kept))
  )))
  expect_false(is.null(with_na$cs))
  expect_equal(rows_key(with_na), rows_key(without), tolerance = opt_tol)
})

test_that("user weights win over a named center, as in validation", {
  d <- with_lost()
  centers <- sort(unique(as.character(d$center)))
  lost <- lost_center(d)
  raw <- ifelse(centers == lost, 0, 1)
  w <- raw / sum(raw)
  both <- suppressWarnings(do.call(rows_run, c(
    list(d), binary_lost,
    list(center_weights_for_outcome_goal = w,
         center_effects_optimization_values = "Auras")
  )))
  weights_only <- suppressWarnings(do.call(rows_run, c(
    list(d), binary_lost, list(center_weights_for_outcome_goal = w)
  )))
  expect_equal(rows_key(both), rows_key(weights_only))
})

test_that("a missing center id is not counted as a facility", {
  d <- bb_center()
  d$center[d$center == "Auras"][1:5] <- NA
  n_centers <- length(unique(stats::na.omit(d$center)))
  args <- list(
    outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    outcome_goal = 0.85, include_center_effects = TRUE,
    center_weights_for_outcome_goal = rep(1 / n_centers, n_centers)
  )
  res <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  without <- suppressWarnings(do.call(
    rows_run, c(list(d[!is.na(d$center), ]), args)
  ))
  expect_equal(rows_key(res), rows_key(without))
})

test_that("an explicit NA center level is kept when another center is lost", {
  d <- with_lost()
  lost <- lost_center(d)
  d$center[d$center == "Auras"] <- NA
  # addNA() makes NA a real level that glm() fits as a center
  d$center <- addNA(factor(d$center))
  without <- d[!(d$center %in% lost), ]
  by_name <- list(center_effects_optimization_values = "Ramiya Behar")
  for (extra in list(list(), by_name)) {
    with_na <- suppressWarnings(
      do.call(rows_run, c(list(d), binary_lost, extra))
    )
    removed <- suppressWarnings(
      do.call(rows_run, c(list(without), binary_lost, extra))
    )
    expect_false(is.null(with_na$cs))
    expect_equal(rows_key(with_na), rows_key(removed), tolerance = opt_tol)
  }
})

test_that("a continuous confidence set refuses an explicit NA center level", {
  d <- bb_center()
  d$center[d$center == "Auras"] <- NA
  d$center <- addNA(factor(d$center))
  expect_error(
    suppressWarnings(rows_run(
      d,
      outcome_name = "leadership_updt", outcome_type = "continuous",
      outcome_goal = 0.93, include_center_effects = TRUE
    )),
    "cannot use an explicit NA level"
  )
  # a factor covariate's NA level is refused too, not left as an empty set
  d <- bb_center()
  set.seed(5)
  d$grp <- addNA(factor(sample(c("a", "b", NA), nrow(d), TRUE)))
  expect_error(
    suppressWarnings(rows_run(
      d,
      outcome_name = "leadership_updt", outcome_type = "continuous",
      outcome_goal = 0.93, additional_covariates = "grp"
    )),
    "explicit NA level .* 'grp'"
  )
  # as the reference level it adds no dummy, so it works like any other name
  first <- function(x) factor(x, levels = c(NA, "a", "b"), exclude = NULL)
  args <- list(
    outcome_name = "leadership_updt", outcome_type = "continuous",
    outcome_goal = 0.93, additional_covariates = "grp"
  )
  g <- as.character(d$grp)
  d$grp <- first(g)
  as_ref <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  d$grp <- factor(ifelse(is.na(g), "0", g))
  named <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  expect_false(is.null(as_ref$cs))
  expect_equal(rows_key(as_ref), rows_key(named))
})

test_that("an unused explicit NA level does not stop a continuous run", {
  d <- as.data.frame(bb_center())
  d$period <- factor((seq_len(nrow(d)) %% 3) + 1)
  args <- list(
    outcome_name = "leadership_updt", outcome_type = "continuous",
    outcome_goal = 0.93, include_center_effects = TRUE,
    include_time_effects = TRUE, time_effect_optimization_value = 2
  )
  plain <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  # addNA() adds an NA level even when no row is NA
  d$period <- addNA(d$period)
  expect_true(anyNA(levels(d$period)))
  with_level <- suppressWarnings(do.call(rows_run, c(list(d), args)))
  expect_equal(rows_key(with_level), rows_key(plain))
  # an NA center level whose rows glm() all drops is unused in the fit too
  lost <- with_lost()
  lost$center[lost$center == lost_center(lost)] <- NA
  lost$center <- addNA(factor(lost$center))
  args <- c(args[1:4], additional_covariates = "cov")
  with_na <- suppressWarnings(do.call(rows_run, c(list(lost), args)))
  without <- suppressWarnings(
    do.call(rows_run, c(list(lost[!is.na(lost$cov), ]), args))
  )
  expect_equal(rows_key(with_na), rows_key(without), tolerance = opt_tol)
})

test_that("a tibble works for a continuous outcome with center effects", {
  # BB_data is a tibble, whose data[, name] is a one-column tibble, not a vector
  expect_s3_class(BB_data, "tbl_df")
  res <- suppressWarnings(rows_run(
    bb_center(),
    outcome_name = "leadership_updt", outcome_type = "continuous",
    outcome_goal = 0.93, include_center_effects = TRUE
  ))
  expect_true(all(is.finite(res$est_outcome_ci)))
})

test_that("a direct confidence-set call with unfitted rows is refused", {
  d <- as.data.frame(bb_center())
  opt <- suppressWarnings(rows_run(
    d,
    outcome_name = "leadership_updt", outcome_type = "continuous",
    outcome_goal = 0.93, additional_covariates = "pp2_hand_hygiene",
    include_confidence_set = FALSE
  ))
  preds <- c("coaching_updt", "launch_duration", "pp2_hand_hygiene")
  expect_error(
    get_confidence_set(
      predictors_data = d[, preds], outcome_data = d$leadership_updt,
      fitted_model = opt$model, link = "identity", outcome_goal = 0.93,
      outcome_type = "continuous",
      intervention_components = c("coaching_updt", "launch_duration"),
      additional_covariates = "pp2_hand_hygiene",
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
      confidence_set_grid_step_size = c(10, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = opt$rec_int
    ),
    "rows the model was fitted on"
  )
})

# center level data whose components vary independently within each center
center_level <- function() {
  set.seed(3)
  cl <- data.frame(
    center = factor(rep(paste0("c", 1:6), each = 4)),
    coaching_updt = rep(c(0, 5, 10, 15), 6),
    launch_duration = rep(c(1, 3, 2, 3), 6),
    center_sample_size = rep(c(20, 30, 40, 50, 60, 70), each = 4)
  )
  cl$proportion <- stats::plogis(
    -1 + 0.05 * cl$coaching_updt + 0.2 * cl$launch_duration +
      stats::rnorm(nrow(cl), 0, 0.3)
  )
  cl
}

center_level_run <- function(cl) {
  suppressWarnings(rows_run(
    cl,
    input_data_structure = "center_level", outcome_name = "proportion",
    outcome_type = "binary", outcome_goal = 0.6, include_center_effects = TRUE
  ))
}

test_that("a center-level center with no sample size is refused clearly", {
  cl <- center_level()
  cl$center_sample_size[cl$center == "c3"] <- NA
  expect_error(
    center_level_run(cl),
    "center_sample_size is missing for every row of the center"
  )
})

test_that("a center-level center is sized by its first observed sample size", {
  cl <- center_level()
  first <- which(cl$center == "c3")[1]
  cl$center_sample_size[first] <- NA
  partial <- center_level_run(cl)
  expect_false(is.null(partial$cs))
  # glm() drops that row too, so the result is the run without it
  expect_equal(rows_key(partial), rows_key(center_level_run(cl[-first, ])))
})
