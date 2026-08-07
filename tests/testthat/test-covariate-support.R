# Regression tests for the additional-covariate support warning.
#
# get_confidence_set() holds every additional covariate at 0 to build the
# prediction grid. For a NUMERIC covariate whose observed values never reach 0
# -- a clinic size in [5, 6], say -- 0 is a value that never occurs in the
# data, so the reported estimated outcome and its interval are read at an
# unobserved covariate value: an extrapolation. The function now warns about
# that, naming the covariate and its observed range.
#
# The warning is DIAGNOSTIC ONLY. It changes no returned value: the covariate
# is still held at 0 either way, so the interval it warns about is exactly the
# interval it would have returned in silence. The tests below pin both halves:
# that it fires in the one case it should, stays silent in the cases it should
# not, and that the returned result is identical whether or not it fires.


# A numeric covariate observed in [5, 6], attached to the bundled BB_data. The
# range is built by rescaling an existing column into [5, 6] deterministically,
# so it never touches 0 and does not depend on a random seed.
bb_with_offset_covariate <- function() {
  bb <- as.data.frame(BB_data)
  x <- bb$coaching_updt
  bb$clinic_size <- 5 + (x - min(x)) / (max(x) - min(x))
  bb
}

# get_confidence_set() over BB_data with one additional covariate, at a coarse
# grid to keep it cheap. The model is fitted on exactly the predictors passed.
covariate_confidence_set <- function(bb, covariate) {
  model <- suppressWarnings(glm(
    as.formula(paste(
      "pp3_oxytocin_mother ~ coaching_updt + launch_duration +", covariate
    )),
    data = bb, family = binomial()
  ))
  get_confidence_set(
    predictors_data = bb[
      , c("coaching_updt", "launch_duration", covariate),
      drop = FALSE
    ],
    additional_covariates = covariate,
    intervention_components = c("coaching_updt", "launch_duration"),
    outcome_data = bb$pp3_oxytocin_mother,
    fitted_model = model,
    link = "logit",
    outcome_goal = 0.85,
    outcome_type = "binary",
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    confidence_set_grid_step_size = c(10, 2),
    cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
    rec_int = c(1, 2.77847)
  )
}


test_that("a numeric covariate observed away from 0 warns it is extrapolated", {
  bb <- bb_with_offset_covariate()
  # precondition: the covariate really is observed in [5, 6], excluding 0
  expect_gt(min(bb$clinic_size), 0)
  expect_equal(range(bb$clinic_size), c(5, 6))

  # the warning names the covariate and its observed range, and says the report
  # is an extrapolation because the covariate is held at 0 off its support
  expect_warning(
    covariate_confidence_set(bb, "clinic_size"),
    "clinic_size.*\\[5, 6\\].*held at 0.*extrapolation"
  )
})


test_that("the warning fires exactly once, listing every offending covariate", {
  bb <- bb_with_offset_covariate()
  # a second numeric covariate also observed away from 0, in a different range
  x <- bb$launch_duration
  bb$staff_count <- 20 + (x - min(x)) / (max(x) - min(x)) * 10
  expect_gt(min(bb$staff_count), 0)
  # a THIRD numeric covariate observed spanning 0, which must NOT be named: it
  # is not held off its support. Included so the test pins that the message
  # lists the offenders and ONLY the offenders, not every covariate.
  bb$balance <- (x - mean(x))
  expect_lt(min(bb$balance), 0)
  expect_gt(max(bb$balance), 0)

  model <- suppressWarnings(glm(
    pp3_oxytocin_mother ~ coaching_updt + launch_duration +
      clinic_size + staff_count + balance,
    data = bb, family = binomial()
  ))
  run <- function() {
    get_confidence_set(
      predictors_data = bb[
        , c(
          "coaching_updt", "launch_duration", "clinic_size", "staff_count",
          "balance"
        ),
        drop = FALSE
      ],
      additional_covariates = c("clinic_size", "staff_count", "balance"),
      intervention_components = c("coaching_updt", "launch_duration"),
      outcome_data = bb$pp3_oxytocin_mother,
      fitted_model = model,
      link = "logit",
      outcome_goal = 0.85,
      outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(10, 2),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      rec_int = c(1, 2.77847)
    )
  }
  # the grid has many rows; the warning is fired once for the call, not per row
  warnings <- character(0)
  withCallingHandlers(
    run(),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  support_warnings <- grep(
    "held at 0 to compute the confidence set", warnings,
    value = TRUE
  )
  expect_length(support_warnings, 1)
  # and the single warning names both offenders and ONLY them: a message that
  # listed every covariate would pass an offenders-are-named check while telling
  # the user a covariate held on its support is off it.
  expect_match(support_warnings, "clinic_size")
  expect_match(support_warnings, "staff_count")
  expect_no_match(support_warnings, "balance")
})


test_that("a numeric covariate whose range includes 0 does not warn", {
  bb <- bb_with_offset_covariate()
  # centre the covariate so its observed range straddles 0; 0 is then a value
  # it takes in the data, so holding it there is not an extrapolation
  bb$clinic_size <- bb$clinic_size - 5.5
  expect_lt(min(bb$clinic_size), 0)
  expect_gt(max(bb$clinic_size), 0)

  expect_no_warning(covariate_confidence_set(bb, "clinic_size"))
})


test_that("a numeric covariate whose range touches 0 does not warn", {
  bb <- bb_with_offset_covariate()
  # 0 sitting at the boundary of the observed range is still an observed value,
  # so the boundary case does not warn either
  bb$clinic_size <- bb$clinic_size - 5
  expect_equal(min(bb$clinic_size), 0)

  expect_no_warning(covariate_confidence_set(bb, "clinic_size"))
})


test_that("a factor additional covariate does not warn", {
  bb <- as.data.frame(BB_data)
  # held at 0, a factor covariate sits at its reference level, an OBSERVED
  # level, so it is not an extrapolation and must not warn
  bb$arm <- factor(
    ifelse(bb$pre_post == 0, "pre", "post"),
    levels = c("post", "pre")
  )
  expect_no_warning(covariate_confidence_set(bb, "arm"))
})


test_that("a character additional covariate does not warn", {
  bb <- as.data.frame(BB_data)
  # a character covariate is contrast-coded like a factor: held at 0 it is its
  # reference level, so it is not extrapolated
  bb$arm <- as.character(ifelse(bb$pre_post == 0, "pre", "post"))
  expect_no_warning(covariate_confidence_set(bb, "arm"))
})


test_that("no additional covariates means no warning", {
  bb <- as.data.frame(BB_data)
  model <- suppressWarnings(glm(
    pp3_oxytocin_mother ~ coaching_updt + launch_duration,
    data = bb, family = binomial()
  ))
  run <- function() {
    get_confidence_set(
      predictors_data = bb[
        , c("coaching_updt", "launch_duration"),
        drop = FALSE
      ],
      intervention_components = c("coaching_updt", "launch_duration"),
      outcome_data = bb$pp3_oxytocin_mother,
      fitted_model = model,
      link = "logit",
      outcome_goal = 0.85,
      outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(10, 2),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      rec_int = c(1, 2.77847)
    )
  }
  expect_no_warning(run())
})


test_that("the warning changes no returned value (it is diagnostic only)", {
  # the warning reads predictors_data only to decide whether to fire. The
  # binary branch's interval is built from vcov(fitted_model), NOT from
  # predictors_data, so the SAME fitted model with a predictors_data whose
  # covariate range straddles 0 rather than excluding it toggles the warning
  # off while returning the identical object. That isolates the warning as a
  # pure side effect: the only thing that changed is whether the condition was
  # signalled, and the returned result is byte-for-byte the same.
  bb <- bb_with_offset_covariate()
  model <- suppressWarnings(glm(
    pp3_oxytocin_mother ~ coaching_updt + launch_duration + clinic_size,
    data = bb, family = binomial()
  ))
  run <- function(predictors_data) {
    get_confidence_set(
      predictors_data = predictors_data,
      additional_covariates = "clinic_size",
      intervention_components = c("coaching_updt", "launch_duration"),
      outcome_data = bb$pp3_oxytocin_mother,
      fitted_model = model,
      link = "logit",
      outcome_goal = 0.85,
      outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(10, 2),
      cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
      rec_int = c(1, 2.77847)
    )
  }
  pd_excludes_zero <- bb[
    , c("coaching_updt", "launch_duration", "clinic_size"),
    drop = FALSE
  ]
  pd_includes_zero <- pd_excludes_zero
  pd_includes_zero$clinic_size <- pd_includes_zero$clinic_size - 5.5

  with_warning <- suppressWarnings(run(pd_excludes_zero))
  no_warning <- run(pd_includes_zero)

  # the second call really is the one that does not warn
  expect_no_warning(run(pd_includes_zero))
  # and the returned object is identical whether or not the warning fired
  expect_identical(with_warning, no_warning)
})
