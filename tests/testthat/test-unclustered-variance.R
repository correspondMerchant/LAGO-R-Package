# The confidence set of a continuous outcome with fixed effects uses the same
# variance as without them, and the coefficient table shows that variance.

unclustered_data <- function() {
  set.seed(4)
  d <- data.frame(
    center = factor(rep(paste0("c", 1:6), each = 60)),
    period = factor(rep(1:3, 120)), x = stats::runif(360, 0, 10)
  )
  base <- c(-2.5, -1, 0.5, 2.5, 1, 0)[as.integer(d$center)]
  d$y <- stats::plogis(base + 0.15 * d$x + stats::rnorm(360, 0, 0.3))
  d$yl <- 2 + base + 0.3 * d$x + stats::rnorm(360)
  d
}

unclustered_run <- function(d, link, time_effects) {
  suppressWarnings(suppressMessages(lago_optimization(
    data = d, outcome_name = if (link == "logit") "y" else "yl",
    outcome_type = "continuous",
    glm_family = if (link == "logit") "quasibinomial" else "gaussian", link = link,
    intervention_components = "x", include_center_effects = TRUE,
    center_effects_optimization_values = "c3",
    include_time_effects = time_effects,
    time_effect_optimization_value = if (time_effects) 2 else NULL,
    intervention_lower_bounds = 0, intervention_upper_bounds = 10,
    cost_list_of_vectors = list(c(0, 1)), outcome_goal = if (link == "logit") 0.75 else 4,
    confidence_set_grid_step_size = 0.5, quiet = TRUE
  )))
}

# the variance by hand: the model based one on the identity link, and on the
# logit link the sandwich with d_i = p_i (1 - p_i) x_i as the package defines it
unclustered_oracle <- function(fit, link) {
  if (link == "identity") return(stats::vcov(fit))
  X <- stats::model.matrix(fit)
  p <- fit$fitted.values
  D <- X * (p * (1 - p))
  bread <- solve(crossprod(D))
  bread %*% crossprod(D * (fit$y - p)) %*% bread
}

unclustered_ci <- function(fit, vcov_matrix, row, link) {
  eta <- sum(row * stats::coef(fit))
  se <- sqrt(drop(t(row) %*% vcov_matrix %*% row))
  bounds <- eta + c(-1, 1) * stats::qnorm(0.975) * se
  round(if (link == "logit") stats::plogis(bounds) else bounds, 3)
}

test_that("fixed effects do not cluster the continuous confidence set", {
  d <- unclustered_data()
  for (link in c("logit", "identity")) {
    for (time_effects in c(FALSE, TRUE)) {
      r <- unclustered_run(d, link, time_effects)
      fit <- r$model
      names_b <- names(stats::coef(fit))
      row <- stats::setNames(numeric(length(names_b)), names_b)
      row[c("(Intercept)", "centerc3", "x")] <- c(1, 1, r$rec_int)
      if (time_effects) row["period2"] <- 1
      # an interior recommendation, so the x terms enter the interval
      expect_true(r$rec_int > 0 && r$rec_int < 10)
      expected <- unclustered_ci(fit, unclustered_oracle(fit, link), row, link)
      expect_equal(unname(unlist(r$est_outcome_ci)), expected, tolerance = 1e-8)
      # the clustered variance made this set empty or the interval missing
      expect_gt(r$confidence_set_size_percentage, 0)
    }
  }
})

test_that("the coefficient table shows the standard errors the intervals use", {
  d <- unclustered_data()
  for (link in c("logit", "identity")) {
    r <- unclustered_run(d, link, TRUE)
    table <- LAGOtrials:::lago_coef_table(r)
    expect_identical(colnames(table), c("Estimate", "Std. Error"))
    expect_equal(table$Estimate, unname(stats::coef(r$model)))
    expect_equal(table[["Std. Error"]], unname(sqrt(diag(unclustered_oracle(r$model, link)))),
      tolerance = 1e-8
    )
    expect_match(LAGOtrials:::lago_se_note(r), if (link == "logit") "HC0" else "model based")
  }
  binary <- suppressWarnings(suppressMessages(lago_optimization(
    data = as.data.frame(BB_data), outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = 0.85,
    include_confidence_set = FALSE, quiet = TRUE
  )))
  expect_equal(LAGOtrials:::lago_coef_table(binary)[["Std. Error"]],
    unname(sqrt(diag(stats::vcov(binary$model))))
  )
  expect_match(LAGOtrials:::lago_se_note(binary), "fitted glm")
  printed <- utils::capture.output(lines <- cli::cli_fmt(print(binary)))
  printed <- c(printed, lines)
  expect_true(any(grepl("Std. Error", printed, fixed = TRUE)))
  expect_false(any(grepl("Pr(>|z|)", printed, fixed = TRUE)))
})
