# Tests for the fit diagnostics in diagnose_model_fit(): the glm fit-warning
# surfacing (separation signal captured during fitting) and the large-SE /
# separation check. Both are exercised through a (quasi-)separation logistic
# fixture, so the preconditions the checks key on are real and asserted here
# rather than assumed.

# A perfectly separated logistic design: y is 0 below a cutoff and 1 above it,
# so a single predictor separates the classes completely. glm() then warns
# "fitted probabilities numerically 0 or 1 occurred" during fitting and drives
# the coefficient standard errors to enormous values, which is what both
# diagnostics detect.
separation_fixture <- function() {
  x <- seq(-5, 5, length.out = 40)
  y <- as.integer(x > 0)
  data.frame(x = x, y = y)
}

# fit the fixture while capturing the warnings glm() emits, exactly as
# outcome_model_fitting() does, so the captured vector is the fit_warnings the
# diagnostic is fed.
fit_with_captured_warnings <- function(d) {
  fit_warnings <- character(0)
  model <- withCallingHandlers(
    glm(y ~ x, data = d, family = binomial(link = "logit")),
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(model = model, fit_warnings = fit_warnings)
}

test_that("diagnose_model_fit surfaces glm fit warnings and flags large standard errors under separation", {
  diagnose <- getFromNamespace("diagnose_model_fit", "LAGO")
  d <- separation_fixture()
  fit <- fit_with_captured_warnings(d)

  # PRECONDITION 1: glm actually warned about separation during fitting, so
  # the fit-warning surfacing has something real to surface.
  expect_true(length(fit$fit_warnings) > 0)
  expect_true(any(grepl(
    "numerically 0 or 1", fit$fit_warnings, fixed = TRUE
  )))

  # PRECONDITION 2: the fit really does have an extremely large standard error
  # (both large in absolute terms and relative to the estimate), so the
  # large-SE check is not firing on a healthy fit.
  se <- coef(summary(fit$model))[, 2]
  est <- coef(summary(fit$model))[, 1]
  large <- is.finite(se) & se > 1e3 & se > 100 * abs(est)
  expect_true(any(large))

  # capture every warning diagnose_model_fit() emits so the specific ones can
  # be picked out (the fixture may also trip the non-significance warning).
  ws <- character(0)
  withCallingHandlers(
    diagnose(
      model = fit$model,
      intervention_components = "x",
      fit_warnings = fit$fit_warnings
    ),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # the surfaced glm fit warning (lines ~176-182): names separation and echoes
  # the captured glm message.
  surfaced <- ws[grepl("may indicate an unreliable fit", ws)]
  expect_length(surfaced, 1)
  expect_match(surfaced, "separation")
  expect_match(surfaced, "numerically 0 or 1")

  # the large-SE / near-singular warning (lines ~214-221): names the affected
  # coefficient (x).
  large_se_warn <- ws[grepl("extremely large standard errors", ws)]
  expect_length(large_se_warn, 1)
  expect_match(large_se_warn, "separation or a near-singular fit")
  expect_match(large_se_warn, "x")
})
