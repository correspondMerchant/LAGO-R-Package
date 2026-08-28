# Regression tests for the model-term and link handling fixed in #68 and #70.
#
# The defects these pin all reported a plausible number rather than failing:
# the prediction matrix was paired with the coefficient vector BY POSITION, a
# factor term's coefficient is named after its LEVEL and so was never found by
# the column name, an unanchored grep("center"/"period") claimed a covariate
# whose name merely started that way, "probit" and "log" were accepted and the
# LINEAR PREDICTOR was then reported as the probability, and one
# linear-predictor term was left unsummed so it recycled once there were two
# center characteristics.
#
# Wherever it is possible, these assert an INVARIANT rather than an output:
# permuting the terms of the same model must not change the answer, renaming a
# covariate must not change the answer, and a factor covariate must give what
# its own explicit dummy columns give. An invariant cannot be satisfied by a
# stale hardcoded number, and it is the exact property each defect broke.


# The model the confidence set below is computed over, fitted with its terms in
# a given order. Every permutation is the SAME model: glm() lists the
# coefficients in term order, so only the coefficient ORDER differs, not the
# fit.
fit_bb_model <- function(predictors) {
  suppressWarnings(glm(
    as.formula(paste(
      "pp3_oxytocin_mother ~", paste(predictors, collapse = " + ")
    )),
    data = as.data.frame(BB_data),
    family = binomial()
  ))
}

# get_confidence_set() over the bundled BB_data at a coarse grid (40 x 5 in
# steps of 5 and 1, so 40 grid interventions), which is what keeps these cheap.
# Arguments passed through ... REPLACE the defaults below rather than being
# passed alongside them, so a test can drop the center characteristic or ask for
# another link without get_confidence_set() seeing the argument twice.
bb_confidence_set <- function(model,
                              predictors_data_cols = c(
                                "coaching_updt", "launch_duration",
                                "birth_volume_100"
                              ),
                              ...) {
  bb <- as.data.frame(BB_data)
  args <- list(
    predictors_data = bb[, predictors_data_cols, drop = FALSE],
    intervention_components = c("coaching_updt", "launch_duration"),
    outcome_data = bb$pp3_oxytocin_mother,
    fitted_model = model,
    link = "logit",
    outcome_goal = 0.85,
    outcome_type = "binary",
    intervention_lower_bounds = c(1, 1),
    intervention_upper_bounds = c(40, 5),
    confidence_set_grid_step_size = c(5, 1),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    rec_int = c(1, 2.77847)
  )
  overrides <- list(...)
  # a NULL override has to REMOVE the default, which modifyList() would not do
  for (name in names(overrides)) args[[name]] <- overrides[[name]]
  for (name in names(overrides)) {
    if (is.null(overrides[[name]])) args[name] <- list(NULL)
  }
  do.call(get_confidence_set, args)
}


test_that("get_confidence_set() pairs coefficients with predictors by name, not position (#68)", {
  predictors <- c("coaching_updt", "launch_duration", "birth_volume_100")
  # all six orderings of the same three terms. The columns are assembled in one
  # fixed order and used to be renamed with names(coef(model)), which renames
  # but cannot reorder, so five of these six multiplied every column by another
  # term's coefficient and reported a different confidence set for the same
  # model. identical() is the whole point: the six results are the same
  # quantity computed from the same fit, so they must agree exactly, not just
  # to a tolerance.
  results <- lapply(
    list(
      c(1, 2, 3), c(1, 3, 2), c(2, 1, 3),
      c(2, 3, 1), c(3, 1, 2), c(3, 2, 1)
    ),
    function(perm) bb_confidence_set(fit_bb_model(predictors[perm]))
  )
  # Compared one permutation at a time so a failure names the permutation that
  # disagreed, rather than reporting that some element of a logical vector was
  # not TRUE.
  perm_labels <- c(
    "1,2,3", "1,3,2", "2,1,3", "2,3,1", "3,1,2", "3,2,1"
  )
  for (i in seq_along(results)[-1]) {
    expect_identical(
      results[[i]], results[[1]],
      info = paste0(
        "term order ", perm_labels[i], " disagreed with ", perm_labels[1]
      )
    )
  }
  # and the shared answer is a real confidence set, not six copies of a
  # degenerate one: 3 of the 40 grid interventions qualify, and the interval at
  # the recommended intervention covers the outcome goal of 0.85.
  # The row count is asserted alongside the size because the size alone cannot
  # tell the two formulas apart here: the old (n - 1) / (N - 1) over 4
  # qualifying rows of a grid it had widened to 41 also gives 0.075, from a
  # genuinely different set. The count separates 3 from 4.
  expect_equal(results[[1]]$confidence_set_size_percentage, 3 / 40)
  expect_equal(nrow(results[[1]]$cs), 3)
  expect_equal(
    results[[1]]$rec_int_ci,
    c(lower = 0.802, upper = 0.898),
    tolerance = 1e-3
  )

  # the same invariance holds when one of the terms is a FACTOR, whose
  # coefficient is named after its level ("armpre" for a column "arm") and so
  # is not even findable by the column name.
  bb <- as.data.frame(BB_data)
  bb$arm <- factor(
    ifelse(bb$pre_post == 0, "pre", "post"),
    levels = c("post", "pre")
  )
  arm_cs <- function(predictors) {
    model <- suppressWarnings(glm(
      as.formula(paste(
        "pp3_oxytocin_mother ~", paste(predictors, collapse = " + ")
      )),
      data = bb, family = binomial()
    ))
    get_confidence_set(
      predictors_data = bb[
        , c("coaching_updt", "launch_duration", "arm"),
        drop = FALSE
      ],
      additional_covariates = "arm",
      intervention_components = c("coaching_updt", "launch_duration"),
      outcome_data = bb$pp3_oxytocin_mother,
      fitted_model = model,
      link = "logit",
      outcome_goal = 0.85,
      outcome_type = "binary",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(5, 1),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      rec_int = c(1, 2.77847)
    )
  }
  expect_identical(
    arm_cs(c("coaching_updt", "launch_duration", "arm")),
    arm_cs(c("arm", "launch_duration", "coaching_updt"))
  )

  # predictors_data is paired by name too. On the continuous path the
  # var-cov matrix is built from predictors_data rather than from the model, so
  # its rows are in the column order of predictors_data, which the caller need
  # not supply in the assembly order. Pairing those by position took every
  # variance from another column's fit.
  bp <- as.data.frame(BB_proportions)
  cts_model <- glm(
    EBP_proportions ~ coaching_updt + launch_duration + birth_volume_100,
    data = bp, family = gaussian()
  )
  cts_cs <- function(cols) {
    get_confidence_set(
      predictors_data = bp[, cols, drop = FALSE],
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = "birth_volume_100",
      center_characteristics_optimization_values = 1.75,
      outcome_data = bp$EBP_proportions,
      fitted_model = cts_model,
      link = "identity",
      outcome_goal = 0.5,
      outcome_type = "continuous",
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      confidence_set_grid_step_size = c(10, 2),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      rec_int = c(20, 2)
    )
  }
  expect_identical(
    cts_cs(c("coaching_updt", "launch_duration", "birth_volume_100")),
    cts_cs(c("birth_volume_100", "launch_duration", "coaching_updt"))
  )
})


test_that("get_confidence_set() refuses a model that does not correspond to the predictors (#68)", {
  # a model fitted on leadership_updt where the confidence set is computed over
  # launch_duration. The coefficient count happens to match, so the positional
  # pairing accepted it and returned a plausible number computed from the wrong
  # coefficient. It must name BOTH sides of what did not match.
  wrong_model <- fit_bb_model(c("coaching_updt", "leadership_updt"))
  expect_error(
    bb_confidence_set(
      wrong_model,
      predictors_data_cols = c("coaching_updt", "launch_duration"),
      center_characteristics = NULL,
      center_characteristics_optimization_values = 0
    ),
    "coefficient\\(s\\) with no matching predictor: leadership_updt"
  )
  expect_error(
    bb_confidence_set(
      wrong_model,
      predictors_data_cols = c("coaching_updt", "launch_duration"),
      center_characteristics = NULL,
      center_characteristics_optimization_values = 0
    ),
    "predictor\\(s\\) with no matching coefficient: launch_duration"
  )

  # An OVER-SPECIFIED model, which is the other half of the check and the half
  # the case above cannot reach. wrong_model has the SAME number of
  # coefficients as there are columns, so anyNA(coef_positions) alone catches
  # it and the length comparison is never exercised. Here every column does
  # match a coefficient and a coefficient is left OVER, so anyNA() sees nothing
  # and only the count separates them. Weakened to that one direction the extra
  # coefficient is silently dropped and a plausible confidence set is returned.
  extra_model <- fit_bb_model(c(
    "coaching_updt", "launch_duration", "birth_volume_100", "leadership_updt"
  ))
  # the fixture really is over-specified in one direction only: 5 coefficients
  # for the 4 columns the confidence set is assembled over, and every column
  # has a coefficient.
  expect_length(coef(extra_model), 5)
  expect_true(all(
    c("(Intercept)", "coaching_updt", "launch_duration", "birth_volume_100") %in%
      names(coef(extra_model))
  ))
  expect_error(
    bb_confidence_set(extra_model),
    "coefficient\\(s\\) with no matching predictor: leadership_updt"
  )
  # the count is named, so the message says which side has the surplus
  expect_error(
    bb_confidence_set(extra_model),
    "5 coefficient\\(s\\), 4 predictor\\(s\\)"
  )
})


test_that("two-level character and factor additional_covariates are found by coefficient name (#68)", {
  bb <- as.data.frame(BB_data)
  bb$arm_factor <- factor(
    ifelse(bb$pre_post == 0, "pre", "post"),
    levels = c("post", "pre")
  )
  bb$arm_character <- as.character(bb$arm_factor)
  # the explicit numeric dummy glm() builds for arm_factor. The factor and this
  # column give the same fit, so the whole optimization must agree: it is the
  # coefficient LOOKUP that differs, since the factor's coefficient is named
  # "arm_factorpre" and the dummy's is named after the column.
  bb$arm_factorpre <- as.numeric(bb$arm_factor == "pre")

  run <- function(covariate) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bb,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("coaching_updt", "launch_duration"),
      additional_covariates = covariate,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(10, 2),
      quiet = TRUE
    )))
  }
  as_factor <- run("arm_factor")
  as_character <- run("arm_character")
  as_dummy <- run("arm_factorpre")

  # the coefficient really is named after the LEVEL, not the column, which is
  # what the name-based lookup had to learn to handle
  expect_true("arm_factorpre" %in% names(coef(as_factor$model)))
  expect_false("arm_factor" %in% names(coef(as_factor$model)))

  # verified by hand: the covariate is held at 0, i.e. at its reference level
  # "post", so the estimated outcome is the inverse logit of the intercept plus
  # the two intervention terms at the recommendation and nothing else.
  beta <- coef(as_factor$model)
  expect_equal(
    as_factor$est_outcome_goal,
    rje::expit(
      beta[["(Intercept)"]] +
        beta[["coaching_updt"]] * as_factor$rec_int[1] +
        beta[["launch_duration"]] * as_factor$rec_int[2]
    ),
    tolerance = 1e-10
  )
  # the goal of 0.85 is already exceeded at the lower bounds, so the cheapest
  # recommendation is the lower bound itself
  expect_equal(as_factor$rec_int, c(1, 1), tolerance = 1e-6)
  expect_equal(as_factor$est_outcome_goal, 0.894019, tolerance = 1e-5)

  # character, factor and explicit dummy are three spellings of one model, so
  # every reported quantity must agree
  for (other in list(as_character, as_dummy)) {
    expect_equal(other$rec_int, as_factor$rec_int)
    expect_equal(other$est_outcome_goal, as_factor$est_outcome_goal)
    expect_equal(other$est_outcome_ci, as_factor$est_outcome_ci)
    expect_equal(
      other$confidence_set_size_percentage,
      as_factor$confidence_set_size_percentage
    )
    expect_equal(other$cs, as_factor$cs)
  }

  # the continuous path builds its own design matrix from predictors_data rather
  # than reading the model's, and it used to take the FIRST VALUE IT MET as the
  # reference level via unique() instead of the factor's first level, which is
  # what glm() drops. So the rows are ordered here to put the NON-reference
  # level first: the dummy was then built for the wrong level, under a name that
  # matched no coefficient, and every variance came from the wrong fit. Both
  # spellings of the same model must give the same interval.
  bp <- as.data.frame(BB_proportions)
  bp$arm_factor <- factor(
    ifelse(bp$coaching_updt > 3, "pre", "post"),
    levels = c("post", "pre")
  )
  bp$arm_factorpre <- as.numeric(bp$arm_factor == "pre")
  # "pre" is the second level, so ordering it first is what unique() trips over
  bp <- bp[order(bp$arm_factor, decreasing = TRUE), ]
  expect_equal(as.character(bp$arm_factor[1]), "pre")

  run_continuous <- function(covariate) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bp,
      outcome_name = "EBP_proportions",
      outcome_type = "continuous",
      glm_family = "gaussian",
      link = "identity",
      intervention_components = c("coaching_updt", "launch_duration"),
      additional_covariates = covariate,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.5,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(5, 1),
      quiet = TRUE
    )))
  }
  cts_factor <- run_continuous("arm_factor")
  cts_dummy <- run_continuous("arm_factorpre")
  expect_equal(cts_factor$rec_int, cts_dummy$rec_int)
  expect_equal(cts_factor$est_outcome_goal, cts_dummy$est_outcome_goal)
  # the interval and the confidence set are what the wrong dummy corrupted
  expect_equal(cts_factor$est_outcome_ci, cts_dummy$est_outcome_ci)
  expect_equal(
    cts_factor$confidence_set_size_percentage,
    cts_dummy$confidence_set_size_percentage
  )
  expect_equal(cts_factor$cs, cts_dummy$cs)
  # verified by hand: the covariate is again held at its reference level, so it
  # contributes nothing to the linear predictor at the recommendation
  cts_beta <- coef(cts_factor$model)
  expect_equal(
    cts_factor$est_outcome_goal,
    cts_beta[["(Intercept)"]] +
      cts_beta[["coaching_updt"]] * cts_factor$rec_int[1] +
      cts_beta[["launch_duration"]] * cts_factor$rec_int[2],
    tolerance = 1e-10
  )
  # 7 of the 40 grid interventions qualify. The wrong dummy reported 4.
  expect_equal(cts_factor$confidence_set_size_percentage, 7 / 40)
  expect_equal(
    cts_factor$est_outcome_ci, c(lower = 0.484, upper = 0.516),
    tolerance = 1e-3
  )
})


test_that("a factor additional_covariate with more than two levels is assembled at its reference level (#68)", {
  # a three-level factor expands to TWO coefficients, so the covariate block
  # needs two columns, not one. Assembling one column per covariate made the
  # block too narrow and the names could not be assigned to it at all.
  bb <- as.data.frame(BB_data)
  bb$phase <- factor(
    ifelse(bb$pilot_phase <= 1, "a", ifelse(bb$pilot_phase <= 3, "b", "c")),
    levels = c("a", "b", "c")
  )
  # the two dummies glm() builds for phase, as explicit numeric columns
  bb$phaseb <- as.numeric(bb$phase == "b")
  bb$phasec <- as.numeric(bb$phase == "c")

  run <- function(covariates) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bb,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("coaching_updt", "launch_duration"),
      additional_covariates = covariates,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(10, 2),
      quiet = TRUE
    )))
  }
  as_factor <- run("phase")
  as_dummies <- run(c("phaseb", "phasec"))

  expect_true(
    all(c("phaseb", "phasec") %in% names(coef(as_factor$model)))
  )
  # both dummies are held at 0, which is the reference level "a", so the
  # estimated outcome is the intercept plus the intervention terms. Verified by
  # hand from the fitted coefficients.
  beta <- coef(as_factor$model)
  expect_equal(
    as_factor$est_outcome_goal,
    rje::expit(
      beta[["(Intercept)"]] +
        beta[["coaching_updt"]] * as_factor$rec_int[1] +
        beta[["launch_duration"]] * as_factor$rec_int[2]
    ),
    tolerance = 1e-10
  )
  # here the goal binds, so the recommendation sits where the estimated outcome
  # meets it exactly
  expect_equal(as_factor$est_outcome_goal, 0.85, tolerance = 1e-8)
  expect_equal(as_factor$rec_int, c(1, 2.944728), tolerance = 1e-5)
  expect_equal(as_factor$confidence_set_size_percentage, 0.25)

  # the factor and its own two dummy columns are the same model
  expect_equal(as_dummies$rec_int, as_factor$rec_int)
  expect_equal(as_dummies$est_outcome_goal, as_factor$est_outcome_goal)
  expect_equal(as_dummies$est_outcome_ci, as_factor$est_outcome_ci)
  expect_equal(as_dummies$cs, as_factor$cs)
})


test_that("a factor center_characteristic is found by its level-named coefficient (#68)", {
  # the optimizer read model$coefficients["vol_hi"] where glm() wrote
  # "vol_hihi", so the coefficient came back NA and the NA propagated into the
  # estimated outcome and broke the optimization outright.
  bb <- as.data.frame(BB_data)
  bb$vol_hi <- factor(
    ifelse(bb$birth_volume_100 > 1.5, "hi", "lo"),
    levels = c("lo", "hi")
  )
  bb$vol_hi_dummy <- as.numeric(bb$vol_hi == "hi")

  run <- function(characteristic, goal) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bb,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = characteristic,
      # 1 is the non-reference level "hi", the only value a two-level
      # characteristic can be held at other than its reference level
      center_characteristics_optimization_values = 1,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = goal,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(10, 2),
      quiet = TRUE
    )))
  }
  as_factor <- run("vol_hi", 0.85)
  expect_true("vol_hihi" %in% names(coef(as_factor$model)))

  # verified by hand: the characteristic is held at 1, so its coefficient
  # enters the linear predictor once, in full.
  beta <- coef(as_factor$model)
  expect_equal(
    as_factor$est_outcome_goal,
    rje::expit(
      beta[["(Intercept)"]] +
        beta[["coaching_updt"]] * as_factor$rec_int[1] +
        beta[["launch_duration"]] * as_factor$rec_int[2] +
        beta[["vol_hihi"]] * 1
    ),
    tolerance = 1e-10
  )
  expect_equal(as_factor$est_outcome_goal, 0.85, tolerance = 1e-8)
  # A loose tolerance on purpose, and do NOT tighten it. The subject here is
  # the coefficient LOOKUP. On the tree that predates the restart-selection fix
  # this same call returns 1.00038 rather than 1, because the cheapest restart
  # was chosen without checking it had stayed inside the bounds. That is a
  # different defect, pinned tightly by its own test in
  # test-minimize-and-bounds.R, and tightening this tolerance would make this
  # test fail for that reason instead of for the lookup it is about.
  expect_equal(as_factor$rec_int, c(1, 1.747970), tolerance = 1e-3)

  # the factor and its explicit dummy are the same model
  as_dummy <- run("vol_hi_dummy", 0.85)
  expect_equal(as_dummy$rec_int, as_factor$rec_int)
  expect_equal(as_dummy$est_outcome_goal, as_factor$est_outcome_goal)

  # the confidence set path names its column after the CHARACTERISTIC, not
  # after the coefficient. A lower goal is used so the set is non-empty and
  # there are rows for that column to be checked on.
  reachable <- run("vol_hi", 0.7)
  expect_true("vol_hi" %in% names(reachable$cs))
  # the confidence set is computed for ONE center, so every row carries the
  # same characteristic value
  expect_equal(unique(reachable$cs$vol_hi), 1)
  expect_equal(reachable$confidence_set_size_percentage, 0.25)
})


test_that("a center characteristic with more than two levels is refused, not resolved to one level (#68)", {
  # a three-level factor characteristic expands to two coefficients, and only
  # ONE value per characteristic is supplied, so nothing says which level that
  # value belongs to. Picking one silently would report the optimization at a
  # level the user never asked for.
  bb <- as.data.frame(BB_data)
  bb$vol3 <- factor(
    ifelse(
      bb$birth_volume_100 > 2, "hi",
      ifelse(bb$birth_volume_100 > 1.4, "mid", "lo")
    ),
    levels = c("lo", "mid", "hi")
  )
  expect_error(
    suppressWarnings(suppressMessages(lago_optimization(
      data = bb,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = "vol3",
      center_characteristics_optimization_values = 1,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      outcome_goal_intention = "maximize",
      include_confidence_set = FALSE,
      quiet = TRUE
    ))),
    "vol3 expand to more than one coefficient each"
  )

  # and on the exported entry point, which can be reached without passing
  # through the optimizer at all
  model <- suppressWarnings(glm(
    pp3_oxytocin_mother ~ coaching_updt + launch_duration + vol3,
    data = bb, family = binomial()
  ))
  expect_error(
    get_confidence_set(
      predictors_data = bb[
        , c("coaching_updt", "launch_duration", "vol3"),
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
      center_characteristics = "vol3",
      center_characteristics_optimization_values = 1,
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      rec_int = c(1, 2.8)
    ),
    "vol3 expand to more than one coefficient each"
  )
})


test_that("a covariate named like a fixed effect prefix does not disturb the result (#68)", {
  # an unanchored grep("center", ...) / grep("period", ...) counted a covariate
  # named center_size or period_flag as a fixed-effect dummy. The reference
  # test is the SAME model with the covariate renamed to something innocuous,
  # which is by construction the same fit and so must give the same answer.
  bb <- as.data.frame(BB_data)
  run_bb <- function(name) {
    d <- bb
    d[[name]] <- d$birth_volume_100
    suppressWarnings(suppressMessages(lago_optimization(
      data = d,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("coaching_updt", "launch_duration"),
      additional_covariates = name,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(10, 2),
      quiet = TRUE
    )))
  }
  # only the reported quantities are compared, since the column name itself
  # legitimately differs between the two runs (it names a column of cs when the
  # covariate is a center characteristic). The names are dropped from cs for the
  # same reason; NULL passes through unchanged, which is a legitimate empty
  # confidence set both runs must agree on.
  unnamed_cs <- function(cs) if (is.null(cs)) NULL else unname(as.matrix(cs))
  same_answer <- function(a, b) {
    expect_equal(a$rec_int, b$rec_int)
    expect_equal(a$est_outcome_goal, b$est_outcome_goal)
    expect_equal(a$est_outcome_ci, b$est_outcome_ci)
    expect_equal(
      a$confidence_set_size_percentage, b$confidence_set_size_percentage
    )
    expect_equal(unnamed_cs(a$cs), unnamed_cs(b$cs))
  }
  reference <- run_bb("plain_covariate")
  same_answer(run_bb("center_size"), reference)
  same_answer(run_bb("period_flag"), reference)

  # the same names again, this time with the fixed center AND time effects
  # actually present, which is when the grep had real dummies to be confused
  # with. The pulesa data is one row per clinic per period, which is what those
  # effects are built from.
  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  pulesa$period <- as.numeric(as.character(pulesa$Period))
  run_pulesa <- function(name, period_value) {
    d <- pulesa
    d[[name]] <- d$HypertensionTraining
    suppressWarnings(suppressMessages(lago_optimization(
      data = d,
      outcome_name = "Proportions",
      outcome_type = "continuous",
      glm_family = "gaussian",
      link = "identity",
      intervention_components = c("AccessMedicines", "AccessBPMachines"),
      additional_covariates = name,
      include_center_effects = TRUE,
      include_time_effects = TRUE,
      time_effect_optimization_value = period_value,
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      outcome_goal = 0.6,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(5, 0.5),
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(2, 0.25),
      quiet = TRUE
    )))
  }
  # period 5 is a period with a dummy of its own, and period 2 is the reference
  # period, which has none. The "period" twin of the bug was worse on the
  # reference period, so both are checked.
  for (period_value in c(2, 5)) {
    pulesa_reference <- run_pulesa("plain_covariate", period_value)
    same_answer(run_pulesa("center_size", period_value), pulesa_reference)
    same_answer(run_pulesa("period_flag", period_value), pulesa_reference)
    # a real answer, not a degenerate one both runs agree on
    expect_true(pulesa_reference$est_outcome_goal > 0.6)
    expect_equal(pulesa_reference$confidence_set_size_percentage, 1 / 9)
  }

  # and as a CENTER CHARACTERISTIC rather than an additional covariate, which
  # is looked up through a different code path
  run_characteristic <- function(name) {
    d <- pulesa
    d[[name]] <- d$HypertensionTraining
    suppressWarnings(suppressMessages(lago_optimization(
      data = d,
      outcome_name = "Proportions",
      outcome_type = "continuous",
      glm_family = "gaussian",
      link = "identity",
      intervention_components = c("AccessMedicines", "AccessBPMachines"),
      center_characteristics = name,
      center_characteristics_optimization_values = 3,
      include_time_effects = TRUE,
      time_effect_optimization_value = 5,
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      # a goal the data can reach, so the confidence set is non-empty and its
      # rows are there to be compared
      outcome_goal = 0.74,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(5, 0.5),
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(2, 0.25),
      quiet = TRUE
    )))
  }
  characteristic_reference <- run_characteristic("plain_characteristic")
  same_answer(run_characteristic("center_size"), characteristic_reference)
  same_answer(run_characteristic("period_flag"), characteristic_reference)
  expect_true(characteristic_reference$est_outcome_goal > 0.7)
  expect_equal(nrow(characteristic_reference$cs), 3)
})


test_that("est_outcome_goal is not 0 on the reference period with fixed time effects (#68)", {
  # the reference period matches no period dummy, so the pattern-matched lookup
  # returned a zero-length coefficient, all_center_lvl_effects collapsed to
  # length zero, and the estimated outcome was reported as literally 0 -- for
  # data whose outcome is a proportion around 0.5.
  pulesa <- as.data.frame(main_pulesa_data)
  pulesa$center <- pulesa$Clinic
  pulesa$period <- as.numeric(as.character(pulesa$Period))
  # the first level of Period, i.e. the one glm() leaves out of the dummies
  reference_period <- as.numeric(levels(main_pulesa_data$Period)[1])
  expect_equal(reference_period, 2)

  run <- function(period_value, include_center_effects) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = pulesa,
      outcome_name = "Proportions",
      outcome_type = "continuous",
      glm_family = "gaussian",
      link = "identity",
      intervention_components = c("AccessMedicines", "AccessBPMachines"),
      include_center_effects = include_center_effects,
      include_time_effects = TRUE,
      time_effect_optimization_value = period_value,
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(10, 1),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      outcome_goal = 0.5,
      outcome_goal_intention = "maximize",
      include_confidence_set = FALSE,
      optimization_method = "grid_search",
      optimization_grid_search_step_size = c(2, 0.25),
      quiet = TRUE
    )))
  }
  at_reference <- run(reference_period, FALSE)
  # the true outcome is nowhere near 0, so 0 can only be the collapsed lookup
  expect_true(at_reference$est_outcome_goal > 0.4)

  # verified by hand: the reference period has no dummy, so the linear
  # predictor at the recommendation is the intercept plus the two intervention
  # terms and no time term at all.
  beta <- coef(at_reference$model)
  expect_equal(
    at_reference$est_outcome_goal,
    beta[["(Intercept)"]] +
      beta[["AccessMedicines"]] * at_reference$rec_int[1] +
      beta[["AccessBPMachines"]] * at_reference$rec_int[2],
    tolerance = 1e-10
  )
  expect_equal(at_reference$est_outcome_goal, 0.542068, tolerance = 1e-5)

  # a non-reference period adds exactly its own dummy, and no other
  at_period_5 <- run(5, FALSE)
  expect_equal(
    at_period_5$est_outcome_goal,
    beta[["(Intercept)"]] + beta[["period5"]] +
      beta[["AccessMedicines"]] * at_period_5$rec_int[1] +
      beta[["AccessBPMachines"]] * at_period_5$rec_int[2],
    tolerance = 1e-10
  )
  # the two periods really do differ, so the period term is being used
  expect_false(isTRUE(all.equal(
    at_reference$est_outcome_goal, at_period_5$est_outcome_goal
  )))

  # and with the fixed center effects included as well, which is the
  # combination the collapse was found on
  with_centers <- run(reference_period, TRUE)
  expect_true(with_centers$est_outcome_goal > 0.4)
  expect_equal(with_centers$est_outcome_goal, 0.516574, tolerance = 1e-5)
})


test_that("lago_optimization() refuses the probit and log links (#70)", {
  # both used to pass validation and were advertised as supported, but only
  # "logit" and "identity" have their inverse link implemented, so the LINEAR
  # PREDICTOR was reported as the PROBABILITY. This exact call, asked for an
  # outcome of 0.20 under a probit fit, reported reaching exactly 0.2000000000
  # while the probability at the recommendation was pnorm(0.20) = 0.5793: the
  # goal was echoed back as the linear predictor that attains it.
  bb <- as.data.frame(BB_data)
  binary_run <- function(link) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bb,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      link = link,
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.2,
      outcome_goal_intention = "maximize",
      include_confidence_set = FALSE,
      quiet = TRUE
    )))
  }
  # the message names the links that ARE supported, so the refusal says what to
  # use instead
  expect_error(binary_run("probit"), "logit, identity")
  expect_error(binary_run("probit"), "link=probit")

  # "log" is refused on a quasibinomial fit, which glm() converges on, so this
  # is the package refusing the link and not glm() failing to fit it
  bp <- as.data.frame(BB_proportions)
  expect_error(
    suppressWarnings(suppressMessages(lago_optimization(
      data = bp,
      outcome_name = "EBP_proportions",
      outcome_type = "continuous",
      glm_family = "quasibinomial",
      link = "log",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.6,
      outcome_goal_intention = "maximize",
      include_confidence_set = FALSE,
      quiet = TRUE
    ))),
    "link=log"
  )

  # the two supported links, and the default, still work and report an outcome
  # on the scale the goal is stated on
  logit_run <- binary_run("logit")
  default_run <- binary_run("default")
  # the goal is a probability, so the reported outcome has to be one too. Under
  # logit it is: the goal of 0.20 is already exceeded with no intervention at
  # all, so the recommendation is the zero intervention and the reported outcome
  # is the baseline probability, 0.2986. This is the assertion probit could not
  # satisfy, since it reported the linear predictor instead.
  expect_equal(logit_run$rec_int, c(0, 0), tolerance = 1e-6)
  expect_equal(logit_run$est_outcome_goal, 0.298636, tolerance = 1e-5)
  expect_equal(
    logit_run$est_outcome_goal,
    rje::expit(coef(logit_run$model)[["(Intercept)"]]),
    tolerance = 1e-10
  )
  expect_equal(default_run$est_outcome_goal, logit_run$est_outcome_goal)
  expect_equal(default_run$rec_int, logit_run$rec_int)

  identity_run <- suppressWarnings(suppressMessages(lago_optimization(
    data = mtcars,
    outcome_name = "mpg",
    outcome_type = "continuous",
    glm_family = "gaussian",
    link = "identity",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(10, 350),
    cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    outcome_goal = 40,
    outcome_goal_intention = "maximize",
    include_confidence_set = FALSE,
    quiet = TRUE
  )))
  expect_equal(identity_run$est_outcome_goal, 40, tolerance = 1e-6)
})


test_that("the exported get_confidence_set() refuses the probit and log links (#70)", {
  # get_confidence_set() is exported, so it is reachable with any link at all
  # without passing through validate_inputs(). Both of its branches assume the
  # interval is built on one of the two implemented links -- the binary branch
  # applies expit() unconditionally -- so any other link silently returned a
  # logit-scale interval whatever was asked for.
  model <- fit_bb_model(c("coaching_updt", "launch_duration"))
  for (link in c("probit", "log")) {
    expect_error(
      bb_confidence_set(
        model,
        predictors_data_cols = c("coaching_updt", "launch_duration"),
        center_characteristics = NULL,
        center_characteristics_optimization_values = 0,
        link = link
      ),
      "logit, identity"
    )
  }
  # and the supported link still computes an interval
  supported <- bb_confidence_set(
    model,
    predictors_data_cols = c("coaching_updt", "launch_duration"),
    center_characteristics = NULL,
    center_characteristics_optimization_values = 0
  )
  expect_length(supported$rec_int_ci, 2)
  expect_true(all(supported$rec_int_ci >= 0 & supported$rec_int_ci <= 1))
})


test_that("multiple center characteristics are summed, not recycled (#68, #70)", {
  # get_outcome() left the center-characteristic term as an elementwise
  # PRODUCT, which stayed a vector as soon as there were two characteristics
  # and then recycled against the center-level effects. get_confidence_set()
  # separately cbind()ed the value vector itself, adding ONE recycled column
  # instead of one column per characteristic.
  bb <- as.data.frame(BB_data)
  run <- function(characteristics, values) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = bb,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      glm_family = "binomial",
      intervention_components = c("coaching_updt", "launch_duration"),
      center_characteristics = characteristics,
      center_characteristics_optimization_values = values,
      intervention_lower_bounds = c(1, 1),
      intervention_upper_bounds = c(40, 5),
      cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
      outcome_goal = 0.85,
      outcome_goal_intention = "maximize",
      include_confidence_set = TRUE,
      confidence_set_grid_step_size = c(10, 2),
      quiet = TRUE
    )))
  }
  two <- run(c("birth_volume_100", "distance_10"), c(1.75, 5))

  # the estimated outcome, computed independently: the characteristics enter
  # the linear predictor as an inner product, one term per characteristic at
  # its own value. Recycling the product instead produced 0.8775 here against a
  # true 0.3622 at the same recommendation.
  beta <- coef(two$model)
  expect_equal(
    two$est_outcome_goal,
    rje::expit(
      beta[["(Intercept)"]] +
        beta[["coaching_updt"]] * two$rec_int[1] +
        beta[["launch_duration"]] * two$rec_int[2] +
        beta[["birth_volume_100"]] * 1.75 +
        beta[["distance_10"]] * 5
    ),
    tolerance = 1e-10
  )
  # the goal binds, so the recommendation meets it exactly
  expect_equal(two$est_outcome_goal, 0.85, tolerance = 1e-8)
  expect_equal(two$rec_int, c(1, 3.347542), tolerance = 1e-5)

  # ONE column per characteristic, named after it, and in the order the
  # characteristics were given
  expect_equal(
    names(two$cs),
    c(
      "coaching_updt", "launch_duration", "birth_volume_100", "distance_10",
      "CI_lower_bound", "CI_upper_bound", "cost"
    )
  )
  # the confidence set is computed for ONE center, so each column holds that
  # characteristic's own value down every row. A single recycled column instead
  # cycled the values down the rows.
  expect_equal(two$cs$birth_volume_100, rep(1.75, nrow(two$cs)))
  expect_equal(two$cs$distance_10, rep(5, nrow(two$cs)))
  expect_gt(nrow(two$cs), 1)

  # three characteristics, so the row count of the confidence set is not a
  # multiple of the number of characteristics for either 2 or 3 and the
  # recycling could not have coincidentally lined up
  three <- run(
    c("birth_volume_100", "distance_10", "staff_nurse"), c(1.75, 5, 5)
  )
  beta3 <- coef(three$model)
  expect_equal(
    three$est_outcome_goal,
    rje::expit(
      beta3[["(Intercept)"]] +
        beta3[["coaching_updt"]] * three$rec_int[1] +
        beta3[["launch_duration"]] * three$rec_int[2] +
        beta3[["birth_volume_100"]] * 1.75 +
        beta3[["distance_10"]] * 5 +
        beta3[["staff_nurse"]] * 5
    ),
    tolerance = 1e-10
  )
  expect_equal(
    names(three$cs),
    c(
      "coaching_updt", "launch_duration", "birth_volume_100", "distance_10",
      "staff_nurse", "CI_lower_bound", "CI_upper_bound", "cost"
    )
  )

  # a SINGLE characteristic is unchanged: the product is already length one
  # there, so sum() is a no-op and the published Nevo et al. recommendation
  # still comes out.
  one <- run("birth_volume_100", 1.75)
  expect_equal(one$rec_int, c(1, 2.778472), tolerance = 1e-5)
  expect_equal(one$est_outcome_goal, 0.85, tolerance = 1e-8)
  expect_equal(names(one$cs), c(
    "coaching_updt", "launch_duration", "birth_volume_100",
    "CI_lower_bound", "CI_upper_bound", "cost"
  ))
})
