# Round 6 fixes in get_confidence_set(), called directly.

gcs_data <- function() {
  set.seed(4)
  d <- data.frame(x1 = runif(240, 0, 10), x2 = runif(240, 0, 5))
  d$y <- 1 + 0.3 * d$x1 + 0.2 * d$x2 + rnorm(240)
  d
}

gcs_call <- function(...) {
  d <- gcs_data()
  m <- glm(y ~ x1 + x2, data = d)
  a <- list(
    predictors_data = d[c("x1", "x2")], intervention_components = c("x1", "x2"),
    outcome_data = d$y, fitted_model = m, link = "identity", outcome_goal = 3,
    outcome_type = "continuous", intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(1, 0.5),
    cost_list_of_vectors = list(c(0, 1), c(0, 10)), rec_int = c(5, 2)
  )
  g <- list(...)
  a[names(g)] <- g
  do.call(get_confidence_set, a)
}

test_that("named bounds, grid steps and cost list are matched by name", {
  base <- gcs_call()
  expect_identical(
    gcs_call(
      intervention_lower_bounds = c(x2 = 0, x1 = 0),
      intervention_upper_bounds = c(x2 = 5, x1 = 10)
    ),
    base
  )
  expect_identical(gcs_call(confidence_set_grid_step_size = c(x2 = 0.5, x1 = 1)), base)
  expect_identical(gcs_call(cost_list_of_vectors = list(x2 = c(0, 10), x1 = c(0, 1))), base)
  expect_identical(gcs_call(rec_int = c(x2 = 2, x1 = 5)), base)
  expect_error(
    gcs_call(intervention_upper_bounds = c(a = 10, b = 5)),
    "The names of intervention_upper_bounds must be the intervention components"
  )
  expect_error(
    gcs_call(rec_int = c(a = 5, b = 2)),
    "The names of rec_int must be the intervention components"
  )
})

test_that("a one value rec_int named by quantile() is accepted", {
  d <- gcs_data()
  m1 <- glm(y ~ x1, data = d)
  one <- function(r) {
    get_confidence_set(
      predictors_data = d["x1"], intervention_components = "x1",
      outcome_data = d$y, fitted_model = m1, link = "identity", outcome_goal = 3,
      outcome_type = "continuous", intervention_lower_bounds = 0,
      intervention_upper_bounds = 10, confidence_set_grid_step_size = 1,
      cost_list_of_vectors = list(c(0, 1)), rec_int = r
    )
  }
  r <- stats::quantile(d$x1, 0.5)
  expect_identical(names(r), "50%")
  expect_identical(one(r), one(unname(r)))
})

test_that("cost vectors must be finite numbers", {
  expect_error(
    gcs_call(cost_list_of_vectors = list(c(0, 1), c("0", "10"))),
    "position\\(s\\) 2 do not"
  )
  expect_error(
    gcs_call(cost_list_of_vectors = list(c(0, NA), c(0, 10))),
    "position\\(s\\) 1 do not"
  )
  expect_error(
    gcs_call(cost_list_of_vectors = list(numeric(0), c(0, 10))),
    "position\\(s\\) 1 do not"
  )
})

cc_call <- function(values, ...) {
  d <- gcs_data()
  d$cc1 <- rep(seq(1, 4, length.out = 8), each = 30)
  d$cc2 <- rep(c(10, 40, 20, 30, 15, 35, 25, 12), each = 30)
  d$y <- d$y + 0.1 * d$cc1 + 0.05 * d$cc2
  m <- glm(y ~ x1 + x2 + cc1 + cc2, data = d)
  get_confidence_set(
    predictors_data = d[c("x1", "x2", "cc1", "cc2")],
    intervention_components = c("x1", "x2"),
    outcome_data = d$y, fitted_model = m, link = "identity", outcome_goal = 5,
    outcome_type = "continuous", intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(1, 0.5),
    cost_list_of_vectors = list(c(0, 1), c(0, 10)), rec_int = c(5, 2),
    center_characteristics = c("cc1", "cc2"),
    center_characteristics_optimization_values = values, ...
  )
}

test_that("center characteristic values are checked and matched by name", {
  base <- cc_call(c(2, 30))
  expect_identical(cc_call(c(cc2 = 30, cc1 = 2)), base)
  expect_identical(cc_call(tapply(c(30, 2), c("cc2", "cc1"), identity)), base)
  expect_error(cc_call(c(a = 2, b = 30)), "The names of center_characteristics_optimization_values")
  expect_error(cc_call(c(2, NA)), "must be finite numbers")
  expect_error(cc_call(c("2", "30")), "must be a numeric vector")
  expect_error(cc_call(list(cc1 = 2, cc2 = 30)), "must be a numeric vector")
  expect_error(cc_call(2), "one per center characteristic")
})

center_call <- function(weights) {
  set.seed(11)
  n <- 300
  d <- data.frame(
    center = factor(sample(c("b", "a", "c"), n, TRUE), levels = c("b", "a", "c")),
    x1 = runif(n, 0, 10), x2 = runif(n, 0, 5)
  )
  d$y <- 1 + 0.3 * d$x1 + 0.2 * d$x2 + c(b = 0, a = 2, c = -2)[as.character(d$center)] +
    rnorm(n)
  m <- glm(y ~ center + x1 + x2, data = d)
  get_confidence_set(
    predictors_data = d[c("center", "x1", "x2")], include_center_effects = TRUE,
    center_weights_for_outcome_goal = weights,
    intervention_components = c("x1", "x2"),
    outcome_data = d$y, fitted_model = m, link = "identity", outcome_goal = 3,
    outcome_type = "continuous", intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(1, 0.5),
    cost_list_of_vectors = list(c(0, 1), c(0, 10)), rec_int = c(5, 2)
  )
}

test_that("named center weights are matched to the model's centers by name", {
  base <- center_call(c(0.2, 0.7, 0.1))
  expect_identical(center_call(c(c = 0.1, b = 0.2, a = 0.7)), base)
  expect_false(identical(center_call(c(0.1, 0.2, 0.7)), base))
  expect_error(
    center_call(c(b = 0.2, a = 0.7, zz = 0.1)),
    "The names of center_weights_for_outcome_goal must be the centers"
  )
})

two_way_data <- function() {
  set.seed(4)
  n <- 400
  d <- data.frame(x1 = runif(n, 0, 10), x2 = runif(n, 1, 5))
  d$site <- sample(c("A", "A_1", "B", "B_1", paste0("s", 1:10)), n, TRUE)
  d$period <- sample(c("1_x", "x", "2_y", "y", paste0("p", 1:10)), n, TRUE)
  d$y <- 0.2 + 0.03 * d$x1 + 0.05 * d$x2 + rnorm(n, sd = 0.05) +
    (d$site %in% c("A", "A_1")) * 0.03 + (d$period %in% c("x", "1_x")) * 0.03
  # in units where 3 decimals show the variance to many digits
  d$y <- 1000 * d$y
  d
}

cluster_call <- function(d, m, cl, link = "identity", goal = 600) {
  get_confidence_set(
    predictors_data = d[c("x1", "x2")], intervention_components = c("x1", "x2"),
    outcome_data = d$y, fitted_model = m, link = link, outcome_type = "continuous",
    outcome_goal = goal, intervention_lower_bounds = c(0, 1),
    intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(1, 1),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), rec_int = c(5, 3), cluster_id = cl
  )
}

# The CR0 sandwich variance at rec_int from its definition, for the checks
# below. w are the weights of the bread and the score.
oracle_ci <- function(m, d, cl, w) {
  X <- stats::model.matrix(m)
  e <- d$y - m$fitted.values
  S <- X * (w * e)
  meat <- function(g) crossprod(rowsum(S, as.character(g)))
  M <- if (length(cl) == 1) {
    meat(cl[[1]])
  } else {
    meat(cl[[1]]) + meat(cl[[2]]) -
      meat(interaction(cl[[1]], cl[[2]], drop = TRUE))
  }
  A <- solve(crossprod(X * sqrt(w)))
  V <- A %*% M %*% A
  r <- c(1, 5, 3)
  est <- sum(r * coef(m))
  se <- sqrt(drop(r %*% V %*% r))
  .lago_fmt_interval(est + c(-1, 1) * stats::qnorm(0.975) * se)
}

test_that("the two-way intersection does not merge cells whose labels paste alike", {
  d <- two_way_data()
  m <- glm(y ~ x1 + x2, data = d)
  cl <- list(d$site, d$period)
  r1 <- expect_no_warning(cluster_call(d, m, cl))
  r2 <- cluster_call(d, m, list(sub("_", ".", d$site), sub("_", ".", d$period)))
  expect_identical(r1, r2)
  expect_equal(unname(r1$rec_int_ci), oracle_ci(m, d, cl, rep(1, nrow(d))))
})

test_that("a two-way variance that is not positive semidefinite is clipped with a warning", {
  found <- NULL
  for (seed in 1:200) {
    set.seed(seed)
    n <- 60
    d <- data.frame(x1 = runif(n, 0, 10), x2 = runif(n, 1, 5))
    d$g1 <- sample(1:3, n, TRUE)
    d$g2 <- sample(1:3, n, TRUE)
    d$y <- 0.2 + 0.03 * d$x1 + 0.05 * d$x2 + rnorm(n, sd = 0.05)
    m <- glm(y ~ x1 + x2, data = d)
    X <- stats::model.matrix(m)
    S <- X * (d$y - m$fitted.values)
    meat <- function(g) crossprod(rowsum(S, as.character(g)))
    M <- meat(d$g1) + meat(d$g2) - meat(paste(d$g1, d$g2))
    r <- c(1, 5, 3)
    A <- solve(crossprod(X))
    if (drop(r %*% A %*% M %*% A %*% r) < 0) {
      found <- list(d = d, m = m)
      break
    }
  }
  expect_false(is.null(found))
  warned <- character(0)
  res <- withCallingHandlers(
    cluster_call(found$d, found$m, list(found$d$g1, found$d$g2)),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("two-way clustered variance", warned)))
  expect_false(any(grepl("High uncertainty", warned)))
  expect_true(all(is.finite(res$rec_int_ci)))
  expect_lte(res$rec_int_ci[["lower"]], res$rec_int_ci[["upper"]])
})

test_that("a positive semidefinite two-way variance gives no warning", {
  d <- two_way_data()
  m <- glm(y ~ x1 + x2, data = d)
  site <- rep(1:16, length.out = nrow(d))
  region <- (site - 1) %/% 4
  expect_no_warning(cluster_call(d, m, list(site, region)))
})

test_that("identity link with a non-gaussian family clusters with the working weights", {
  set.seed(7)
  n <- 140
  d <- data.frame(x1 = runif(n, 0, 10), x2 = runif(n, 1, 5))
  d$y <- stats::rpois(n, 5 + 3 * d$x1)
  d$g <- sample(1:20, n, TRUE)
  m <- glm(y ~ x1 + x2,
    data = d, family = stats::quasipoisson(link = "identity"),
    start = c(5, 1, 0)
  )
  res <- cluster_call(d, m, list(d$g), goal = 20)
  expect_equal(unname(res$rec_int_ci), oracle_ci(m, d, list(d$g), m$weights))
  m$weights[1] <- NA
  expect_error(cluster_call(d, m, list(d$g), goal = 20), "the working weights of the fit")
})

test_that("cluster_id = list() is the same as no clusters", {
  d <- two_way_data()
  m <- glm(y ~ x1 + x2, data = d)
  expect_identical(cluster_call(d, m, list()), cluster_call(d, m, NULL))
})

test_that("the characteristic check uses the prior weights in fitted row order", {
  set.seed(3)
  n <- 80
  d <- data.frame(x1 = runif(n, 0, 10), x2 = runif(n, 0, 5))
  d$cc <- seq(1, 5, length.out = n)
  d$y <- 1 + 0.3 * d$x1 + 0.2 * d$x2 + 0.1 * d$cc + rnorm(n)
  d$w <- 1
  d$w[n] <- 0
  d$x2[1] <- NA
  m <- glm(y ~ x1 + x2 + cc, data = d, weights = w, na.action = stats::na.exclude)
  dk <- d[-1, ]
  expect_warning(
    get_confidence_set(
      predictors_data = dk[c("x1", "x2", "cc")], intervention_components = c("x1", "x2"),
      outcome_data = dk$y, fitted_model = m, link = "identity", outcome_goal = 3,
      outcome_type = "continuous", intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 5), confidence_set_grid_step_size = c(1, 0.5),
      cost_list_of_vectors = list(c(0, 1), c(0, 10)), rec_int = c(5, 2),
      center_characteristics = "cc", center_characteristics_optimization_values = 5
    ),
    "outside the centers in the data"
  )
})

test_that("interval bounds use the package rounding rule", {
  expect_identical(round_interval_bound(1.0005), .lago_fmt_interval(1.0005))
  expect_identical(round_interval_bound(1.0005), 1.001)
  expect_identical(round_interval_bound(0.012345), 0.01235)
  expect_identical(round_interval_bound(c(NA, Inf)), c(NA, Inf))
})

test_that("the grid adds its upper bound by a gap relative to the step", {
  set.seed(5)
  n <- 100
  d <- data.frame(x1 = 1e6 + runif(n, 0, 12))
  d$y <- rnorm(n, sd = 5)
  m <- glm(y ~ x1, data = d)
  up <- 1e6 + 10.005
  res <- get_confidence_set(
    predictors_data = d["x1"], intervention_components = "x1",
    outcome_data = d$y, fitted_model = m, link = "identity", outcome_goal = mean(d$y),
    outcome_type = "continuous", intervention_lower_bounds = 1e6,
    intervention_upper_bounds = up, confidence_set_grid_step_size = 1,
    cost_list_of_vectors = list(c(0, 1)), rec_int = 1e6 + 5
  )
  expect_identical(nrow(res$cs), 12L)
  expect_equal(max(res$cs$x1), up)
})
