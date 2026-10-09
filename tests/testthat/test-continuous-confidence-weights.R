weighted_confidence_fixture <- function() {
  set.seed(846)
  n <- 90L
  d <- data.frame(x = seq(-1, 1, length.out = n), z = rnorm(n))
  d$y <- 0.5 + 0.16 * d$x + 0.04 * d$z + rnorm(n, sd = 0.055)
  d$w <- rep(c(0.3, 1, 4), length.out = n)
  d$c1 <- rep_len(c("b", "a", "c", "d", "e"), n)
  d$c2 <- rep_len(c("late", "early", "mid"), n)
  d
}

weighted_confidence_call <- function(d, model, link = "identity", clusters = NULL,
                                     predictors = c("x", "z"), type = "continuous") {
  suppressWarnings(get_confidence_set(
    predictors_data = d[predictors], additional_covariates = setdiff(predictors, "x"),
    intervention_components = "x", outcome_data = d$y, fitted_model = model,
    link = link, outcome_type = type, outcome_goal = 0.5,
    intervention_lower_bounds = -1, intervention_upper_bounds = 1,
    confidence_set_grid_step_size = 0.05, cost_list_of_vectors = list(c(0, 1)),
    rec_int = 0.4, cluster_id = clusters
  ))
}

weighted_confidence_ci <- function(model, covariance, x, link = "identity") {
  row <- setNames(rep(0, length(coef(model))), names(coef(model)))
  row["(Intercept)"] <- 1
  row["x"] <- x
  eta <- sum(row * coef(model))
  se <- sqrt(drop(row %*% covariance[names(row), names(row)] %*% row))
  bounds <- eta + c(-1, 1) * qnorm(0.975) * se
  if (link == "logit") bounds <- plogis(bounds)
  bounds
}

weighted_confidence_oracle <- function(model, d, link, clusters = NULL) {
  X <- model.matrix(model)
  w <- model$prior.weights
  A <- matrix(0, ncol(X), ncol(X))
  scores <- matrix(0, nrow(X), ncol(X))
  scale <- 0
  for (i in which(w > 0)) {
    gradient <- X[i, ]
    if (link == "logit") gradient <- gradient * model$fitted.values[i] *
      (1 - model$fitted.values[i])
    residual <- d$y[i] - model$fitted.values[i]
    A <- A + w[i] * outer(gradient, gradient)
    scores[i, ] <- w[i] * gradient * residual
    scale <- scale + w[i] * residual^2
  }
  B <- solve(A)
  if (is.null(clusters) && link == "identity") {
    V <- B * scale / (sum(w > 0) - ncol(X))
  } else {
    cluster_meat <- function(ids) {
      M <- matrix(0, ncol(X), ncol(X))
      for (id in unique(ids)) {
        score <- colSums(scores[ids == id, , drop = FALSE])
        M <- M + outer(score, score)
      }
      M
    }
    if (is.null(clusters)) {
      M <- cluster_meat(seq_len(nrow(X)))
    } else {
      M <- cluster_meat(clusters[[1]])
      if (length(clusters) == 2) M <- M + cluster_meat(clusters[[2]]) -
        cluster_meat(paste(clusters[[1]], clusters[[2]], sep = "_"))
    }
    V <- B %*% M %*% t(B)
  }
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

weighted_confidence_expect <- function(result, model, covariance, link) {
  expect_equal(unname(result$rec_int_ci),
    round(weighted_confidence_ci(model, covariance, 0.4, link), 3))
  grid <- seq(-1, 1, by = 0.05)
  bounds <- suppressWarnings(t(vapply(grid, function(x)
    weighted_confidence_ci(model, covariance, x, link), numeric(2))))
  selected <- is.finite(bounds[, 1]) & bounds[, 1] <= 0.5 & 0.5 < bounds[, 2]
  expect_equal(result$confidence_set_size_percentage, mean(selected))
  if (any(selected)) {
    expect_equal(result$cs$x, grid[selected])
    expect_equal(result$cs$CI_lower_bound, round(bounds[selected, 1], 3))
    expect_equal(result$cs$CI_upper_bound, round(bounds[selected, 2], 3))
  } else {
    expect_null(result$cs)
  }
}

test_that("public weighted identity intervals use fitted prior weights", {
  d <- weighted_confidence_fixture()
  model <- glm(y ~ x + z, data = d, weights = w)
  result <- weighted_confidence_call(d, model)
  expected <- round(weighted_confidence_ci(model, vcov(model), 0.4), 3)
  expect_equal(unname(result$rec_int_ci), expected)
})

test_that("weighted logit intervals use legacy weighted gradient scores", {
  d <- weighted_confidence_fixture()
  for (family in list(gaussian("logit"), quasibinomial("logit"))) {
    model <- glm(y ~ x + z, data = d, weights = w, family = family)
    expect_gt(max(abs(model$prior.weights - model$weights)), 0.1)
    for (clusters in list(NULL, list(d$c1), list(d$c2), list(d$c1, d$c2))) {
      covariance <- weighted_confidence_oracle(model, d, "logit", clusters)
      weighted_confidence_expect(weighted_confidence_call(d, model, "logit", clusters),
        model, covariance, "logit")
    }
  }
})

test_that("logit kernels accumulate weighted bread and score outer products", {
  X <- cbind(1, c(-0.9, 0.2, 1.3, -0.4, 0.8))
  fitted <- c(0.2, 0.3, 0.6, 0.4, 0.7)
  y <- c(0.1, 0.5, 0.8, 0.3, 0.9)
  w <- c(0.3, 2, 0, 1.1, 4)
  ids <- c(0L, 1L, 2L, 0L, 1L)
  g <- X * (fitted * (1 - fitted))
  scores <- g * (w * (y - fitted))
  hc0 <- LAGOtrials:::sandwich_hc0_logit_accumulate
  cluster <- LAGOtrials:::sandwich_cluster_logit_accumulate
  for (weights in list(w, 13 * w, rep(1, 5))) {
    scores <- g * (weights * (y - fitted))
    result <- hc0(X, fitted, y, weights)
    expect_equal(result$J, unname(crossprod(g, weights * g)) / 5, tolerance = 1e-14)
    expect_equal(result$V, unname(crossprod(scores)) / 5, tolerance = 1e-14)
    result <- cluster(X, ids, 3L, fitted, y, weights)
    summed <- rowsum(scores, ids, reorder = FALSE)
    expect_equal(result$J, unname(crossprod(g, weights * g)) / 3, tolerance = 1e-14)
    expect_equal(result$V, unname(crossprod(summed)) / 3, tolerance = 1e-14)
  }
  expect_identical(hc0(X, fitted, y), hc0(X, fitted, y, NULL))
  expect_identical(cluster(X, ids, 3L, fitted, y), cluster(X, ids, 3L, fitted, y, NULL))
  bad <- list(numeric(), rep(1, 4), rep(1, 6), c(-1, rep(1, 4)),
    c(NA_real_, rep(1, 4)), c(NaN, rep(1, 4)), c(Inf, rep(1, 4)))
  for (weights in bad) {
    message <- if (length(weights) != 5) "prior_weights must have length" else
      "prior_weights must be finite and nonnegative\\.$"
    expect_error(hc0(X, fitted, y, weights), message)
    expect_error(cluster(X, ids, 3L, fitted, y, weights), message)
  }
})

test_that("continuous confidence sets refuse corrupt fitted prior weights", {
  d <- weighted_confidence_fixture()
  bad <- list(numeric(), rep(1, nrow(d) - 1), rep(1, nrow(d) + 1),
    rep("one", nrow(d)), rep(TRUE, nrow(d)), rep(1 + 0i, nrow(d)),
    matrix(1, nrow(d), 1), c(-1, rep(1, nrow(d) - 1)),
    c(NA_real_, rep(1, nrow(d) - 1)), c(NaN, rep(1, nrow(d) - 1)),
    c(Inf, rep(1, nrow(d) - 1)))
  for (link in c("identity", "logit")) {
    model <- glm(y ~ x + z, data = d, family = gaussian(link))
    for (weights in bad) {
      corrupt <- model
      corrupt$prior.weights <- weights
      expect_error(weighted_confidence_call(d, corrupt, link),
        "fitted_model\\$prior.weights must be a finite nonnegative numeric vector with one entry per fitted row\\.$")
    }
  }
})

test_that("only unclustered identity requires positive residual degrees of freedom", {
  d <- data.frame(x = c(-1, 1, 0, 0.5), z = c(0, 0, 0, 0), y = c(0.3, 0.7, 0.4, 0.6))
  model <- glm(y ~ x, data = d, weights = c(1, 1, 0, 0))
  expect_equal(model$df.residual, 0)
  expect_error(weighted_confidence_call(d, model, predictors = "x"),
    "Continuous identity covariance requires positive residual degrees of freedom among positive weight rows\\.$")
  unit <- glm(y ~ x, data = d[1:2, ])
  expect_error(weighted_confidence_call(d[1:2, ], unit, predictors = "x"),
    "Continuous identity covariance requires positive residual degrees of freedom among positive weight rows\\.$")
  expect_no_error(weighted_confidence_call(d, model, clusters = list(1:4), predictors = "x"))
  model <- glm(y ~ x, data = d, weights = c(1, 1, 0, 0), family = gaussian("logit"))
  expect_no_error(weighted_confidence_call(d, model, "logit", predictors = "x"))
})

test_that("fractional and zero relative weights match all continuous covariance paths", {
  for (link in c("identity", "logit")) {
    for (family in list(gaussian(link), quasibinomial(link))) {
      for (zeros in c(FALSE, TRUE)) {
        d <- weighted_confidence_fixture()
        if (zeros) {
          d$w[c(4, 19)] <- 0
          d$c1[c(4, 19)] <- "zero_only"
        }
        model <- suppressWarnings(glm(y ~ x + z, data = d, weights = w, family = family))
        for (clusters in list(NULL, list(d$c1), list(d$c2), list(d$c1, d$c2))) {
          covariance <- weighted_confidence_oracle(model, d, link, clusters)
          result <- weighted_confidence_call(d, model, link, clusters)
          weighted_confidence_expect(result, model, covariance, link)
          scaled <- model
          scaled$prior.weights <- 13 * model$prior.weights
          expect_equal(weighted_confidence_call(d, scaled, link, clusters), result)
          refitted <- suppressWarnings(glm(y ~ x + z, data = d, weights = 13 * w, family = family))
          expect_equal(weighted_confidence_call(d, refitted, link, clusters), result)
          if (zeros) {
            positive <- d$w > 0
            retained <- d[positive, ]
            subset_model <- suppressWarnings(glm(y ~ x + z, data = retained, weights = w, family = family))
            retained_clusters <- lapply(clusters, function(cid) cid[positive])
            if (is.null(clusters)) retained_clusters <- NULL
            expect_equal(weighted_confidence_call(retained, subset_model, link, retained_clusters), result)
          }
        }
        if (link == "identity" && family$family == "gaussian") {
          covariance <- weighted_confidence_oracle(model, d, link)
          expect_equal(covariance, suppressWarnings(vcov(model)), tolerance = 1e-12)
          expect_equal(model$df.residual, sum(d$w > 0) - length(coef(model)))
          if (zeros) expect_false(isTRUE(all.equal(covariance,
            covariance * model$df.residual / (nrow(d) - length(coef(model))))))
        }
      }
    }
  }
})

test_that("fitted missing rows and na.exclude do not subset prior weights twice", {
  for (na_action in c("na.omit", "na.exclude")) {
    for (link in c("identity", "logit")) {
      d <- weighted_confidence_fixture()
      d$q <- factor(rep_len(c("third", "first", "second"), nrow(d)),
        levels = c("second", "third", "first"))
      d$y[3] <- NA
      d$z[8] <- NA
      d$w[10] <- NA
      d$q[13] <- NA
      d$w[17] <- NaN
      model <- glm(y ~ z + q + x, data = d, weights = w,
        family = gaussian(link), na.action = na_action,
        contrasts = list(q = "contr.treatment"))
      keep <- complete.cases(d[c("y", "z", "w", "q")])
      retained <- d[keep, ]
      expect_equal(unname(model$prior.weights), retained$w)
      expect_length(model$fitted.values, nrow(retained))
      expect_length(fitted(model), if (na_action == "na.exclude") nrow(d) else nrow(retained))
      for (clusters in list(NULL, list(retained$c1), list(retained$c1, retained$c2))) {
        covariance <- weighted_confidence_oracle(model, retained, link, clusters)
        result <- weighted_confidence_call(retained, model, link, clusters, c("q", "x", "z"))
        weighted_confidence_expect(result, model, covariance, link)
        refitted <- glm(y ~ z + q + x, data = retained, weights = w,
          family = gaussian(link), contrasts = list(q = "contr.treatment"))
        expect_equal(weighted_confidence_call(retained, refitted, link, clusters,
          c("z", "q", "x")), result)
      }
      expect_error(weighted_confidence_call(d, model, link, predictors = c("q", "x", "z")),
        "rows the model was fitted on")
    }
  }
})

test_that("zero weight observations skip nonfinite residual arithmetic", {
  d <- weighted_confidence_fixture()
  d$w[c(4, 19)] <- 0
  for (link in c("identity", "logit")) {
    model <- glm(y ~ x + z, data = d, weights = w, family = gaussian(link))
    changed <- model
    changed$fitted.values[c(4, 19)] <- c(NA_real_, Inf)
    changed_data <- d
    changed_data$y[c(4, 19)] <- c(Inf, NA_real_)
    for (clusters in list(NULL, list(d$c1), list(d$c1, d$c2))) {
      expect_identical(weighted_confidence_call(changed_data, changed, link, clusters),
        weighted_confidence_call(d, model, link, clusters))
    }
    bad_cluster <- d$c1
    bad_cluster[4] <- NA
    expect_error(weighted_confidence_call(d, model, link, list(bad_cluster)),
      "cluster_id contains NA")
    expect_error(weighted_confidence_call(d, model, link, list(d$c1[-4])),
      "cluster_id must have one entry per fitted row")
  }
  X <- cbind(1, c(-1, 0, 1))
  fitted <- c(0.2, 0.5, 0.7)
  y <- c(0.1, 0.6, 0.8)
  weights <- c(1, 0, 2)
  hc0 <- LAGOtrials:::sandwich_hc0_logit_accumulate
  cluster <- LAGOtrials:::sandwich_cluster_logit_accumulate
  expected_hc0 <- hc0(X, fitted, y, weights)
  expected_cluster <- cluster(X, 0:2, 3L, fitted, y, weights)
  X[2, ] <- NA_real_
  fitted[2] <- Inf
  y[2] <- NA_real_
  expect_identical(hc0(X, fitted, y, weights), expected_hc0)
  expect_identical(cluster(X, 0:2, 3L, fitted, y, weights), expected_cluster)
})

test_that("unit weights and absent prior fields preserve original public outputs", {
  d <- weighted_confidence_fixture()
  for (link in c("identity", "logit")) {
    for (family in list(gaussian(link), quasibinomial(link))) {
      model <- glm(y ~ x + z, data = d, family = family)
      unit <- glm(y ~ x + z, data = d, family = family, weights = rep(1, nrow(d)))
      absent <- model
      absent$prior.weights <- NULL
      constant <- model
      constant$prior.weights <- rep(2.5, nrow(d))
      for (clusters in list(NULL, list(d$c1), list(d$c1, d$c2))) {
        expected <- weighted_confidence_call(d, model, link, clusters)
        expect_identical(weighted_confidence_call(d, unit, link, clusters), expected)
        expect_identical(weighted_confidence_call(d, absent, link, clusters), expected)
        expect_equal(weighted_confidence_call(d, constant, link, clusters), expected)
      }
    }
  }
})

test_that("weighted bread is not regularized when positive rows lose rank", {
  for (link in c("identity", "logit")) {
    d <- weighted_confidence_fixture()
    model <- glm(y ~ x + z, data = d, family = gaussian(link))
    model$prior.weights <- rep(0, nrow(d))
    for (clusters in list(list(d$c1), list(d$c1, d$c2))) {
      expect_error(weighted_confidence_call(d, model, link, clusters), "singular")
    }
    if (link == "logit") expect_error(weighted_confidence_call(d, model, link), "singular")
    if (link == "identity") expect_error(weighted_confidence_call(d, model, link),
      "positive residual degrees of freedom")
    d$z <- as.numeric(seq_len(nrow(d)) <= 2)
    model <- glm(y ~ x + z, data = d, family = gaussian(link))
    model$prior.weights <- d$w
    model$prior.weights[1:2] <- 0
    expect_error(weighted_confidence_call(d, model, link), "singular")
  }
})

test_that("confidence covariance follows the actual fit in a weights column collision", {
  d <- weighted_confidence_fixture()
  d$weights <- rep_len(c(0.7, 3, 1.2, 0.4), nrow(d))
  for (link in c("identity", "logit")) {
    fit <- LAGOtrials:::outcome_model_fitting(data = d, outcome_name = "y",
      family_object = gaussian(link), intervention_components = "x", weights = d$w,
      additional_covariates = "z", center_characteristics = NULL)$model
    expect_equal(unname(fit$prior.weights), d$weights)
    expect_false(isTRUE(all.equal(unname(fit$prior.weights), d$w)))
    for (clusters in list(NULL, list(d$c1), list(d$c1, d$c2))) {
      covariance <- weighted_confidence_oracle(fit, d, link, clusters)
      weighted_confidence_expect(weighted_confidence_call(d, fit, link, clusters),
        fit, covariance, link)
    }
  }
})

test_that("weighted binary intervals retain model covariance without continuous guards", {
  d <- weighted_confidence_fixture()
  set.seed(83)
  d$y <- rbinom(nrow(d), 1, 0.5)
  for (link in c("identity", "logit")) {
    model <- suppressWarnings(glm(y ~ x + z, data = d, weights = w,
      family = binomial(link), start = if (link == "identity") c(0.5, 0, 0) else NULL))
    row <- c(1, 0.4, 0)
    eta <- drop(row %*% coef(model))
    se <- sqrt(drop(row %*% vcov(model) %*% row))
    if (link == "logit") {
      mean <- plogis(eta)
      expected <- pmin(1, pmax(0, mean + c(-1, 1) * qnorm(0.975) * mean * (1 - mean) * se))
    } else expected <- eta + c(-1, 1) * qnorm(0.975) * se
    original <- weighted_confidence_call(d, model, link, type = "binary")
    expect_equal(unname(original$rec_int_ci), round(expected, 3))
    corrupt <- model
    corrupt$prior.weights <- "ignored"
    expect_identical(weighted_confidence_call(d, corrupt, link, list(d$c1, d$c2),
      type = "binary"), original)
  }
})

test_that("public optimization aligns weighted covariance with fitted fixed effects", {
  old <- options(na.action = "na.exclude")
  on.exit(options(old), add = TRUE)
  for (link in c("identity", "logit")) {
    d <- weighted_confidence_fixture()
    d$center <- factor(d$c1, levels = c("b", "e", "a", "d", "c"))
    d$x <- d$x + 1
    d$period <- factor(match(d$c2, c("late", "mid", "early")), levels = c(2, 1, 3))
    d$q <- factor(rep_len(c("other", "ref"), nrow(d)), levels = c("ref", "other"))
    d$w[c(4, 19)] <- 0
    d$y[3] <- NA
    d$z[8] <- NA
    d$w[10] <- NA
    d$q[13] <- NA
    keep <- complete.cases(d[c("y", "z", "w", "q")])
    retained <- d[keep, ]
    for (mode in 0:3) {
      center <- mode %in% c(1, 3)
      period <- mode %in% c(2, 3)
      run <- function(dat) suppressWarnings(suppressMessages(lago_optimization(
        data = dat, outcome_name = "y", outcome_type = "continuous",
        glm_family = "gaussian", link = link, weights = dat$w,
        intervention_components = "x", additional_covariates = c("z", "q"),
        intervention_lower_bounds = 0, intervention_upper_bounds = 2,
        confidence_set_grid_step_size = 0.05, cost_list_of_vectors = list(c(0, 1)),
        outcome_goal = 0.5, outcome_goal_intention = "maximize", quiet = TRUE,
        include_center_effects = center, center_effects_optimization_values = if (center) "b" else NULL,
        include_time_effects = period, time_effect_optimization_value = if (period) 2 else NULL
      )))
      result <- run(d)
      filtered <- run(retained)
      expect_equal(result$rec_int, filtered$rec_int)
      expect_equal(result$est_outcome_ci, filtered$est_outcome_ci)
      expect_equal(result$cs, filtered$cs)
      model <- result$model
      expect_equal(unname(model$prior.weights), retained$w)
      expect_length(fitted(model), nrow(d))
      clusters <- c(if (center) list(retained$center), if (period) list(retained$period))
      if (!length(clusters)) clusters <- NULL
      covariance <- weighted_confidence_oracle(model, retained, link, clusters)
      expect_equal(unname(result$est_outcome_ci),
        round(weighted_confidence_ci(model, covariance, result$rec_int, link), 3))
      grid <- seq(0, 2, 0.05)
      bounds <- suppressWarnings(t(vapply(grid, function(x)
        weighted_confidence_ci(model, covariance, x, link), numeric(2))))
      selected <- is.finite(bounds[, 1]) & bounds[, 1] <= 0.5 & 0.5 < bounds[, 2]
      expect_equal(result$confidence_set_size_percentage, mean(selected))
      if (any(selected)) {
        expect_equal(result$cs$x, grid[selected])
        expect_equal(result$cs$CI_lower_bound, round(bounds[selected, 1], 3))
        expect_equal(result$cs$CI_upper_bound, round(bounds[selected, 2], 3))
      } else expect_null(result$cs)
    }
  }
})

test_that("joint row and numeric predictor permutations preserve covariance coordinates", {
  d <- weighted_confidence_fixture()
  permutation <- c(seq(2, nrow(d), 2), seq(1, nrow(d), 2))
  for (link in c("identity", "logit")) {
    model <- glm(y ~ z + x, data = d, weights = w, family = gaussian(link))
    shuffled <- d[permutation, ]
    refitted <- glm(y ~ z + x, data = shuffled, weights = w, family = gaussian(link))
    for (mode in 0:2) {
      clusters <- switch(mode + 1, NULL, list(d$c1), list(d$c1, d$c2))
      shuffled_clusters <- lapply(clusters, function(ids) ids[permutation])
      if (is.null(clusters)) shuffled_clusters <- NULL
      result <- weighted_confidence_call(d, model, link, clusters, c("z", "x"))
      weighted_confidence_expect(result, model,
        weighted_confidence_oracle(model, d, link, clusters), link)
      expect_equal(weighted_confidence_call(shuffled, refitted, link, shuffled_clusters), result)
    }
  }
})

test_that("weighted identity clustering uses weighted residual scores", {
  d <- weighted_confidence_fixture()
  model <- glm(y ~ x + z, data = d, weights = w)
  for (clusters in list(list(d$c1), list(d$c2), list(d$c1, d$c2))) {
    covariance <- weighted_confidence_oracle(model, d, "identity", clusters)
    weighted_confidence_expect(weighted_confidence_call(d, model, clusters = clusters),
      model, covariance, "identity")
  }
})
