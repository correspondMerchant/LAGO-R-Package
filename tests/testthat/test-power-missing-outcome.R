# The power-goal conversion (get_power_desired_outcome) must use only rows with
# an observed outcome, the rows the final two-arm test uses. A missing outcome
# used to count toward its arm's size but make the arm's sum NA, which crashed
# the unconditional approach and made the conditional approach silently return
# 0.

power_run <- function(data, approach) {
  suppressWarnings(suppressMessages(lago_optimization(
    data = data,
    outcome_name = "pp2_hand_hygiene",
    outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0),
    intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)),
    outcome_goal_intention = "maximize",
    power_goal = 0.8,
    power_goal_approach = approach,
    num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 30,
    include_confidence_set = FALSE,
    quiet = TRUE
  )))
}

bb_grouped_na <- function() {
  d <- BB_data
  d$group <- ifelse(d$pre_post == 0, "control", "treatment")
  d
}

test_that("on binding data, a missing outcome gives the complete-case target", {
  # BB_data's stage 1 already powers the comparison, so its target sits on a
  # grid floor that barely depends on the stage-1 sums. On this small dataset
  # the ncp constraint binds, so a biased rule (e.g. sum(na.rm = TRUE) while
  # still counting the missing rows) would give a different answer.
  d <- make_small_clustered()
  coeff <- small_coeff(d)
  d_na <- d
  d_na$y[c(1, 11, 45, 55, 65)] <- NA
  expect_true(all(c("control", "treatment") %in% d_na$group[is.na(d_na$y)]))
  complete <- d_na[!is.na(d_na$y), ]
  for (approach in c("unconditional", "conditional")) {
    for (icc in list(NULL, 0.03)) {
      # n2j = 10 as well as 20: at 20 the answer does not depend on the stage-1
      # design effect, so only the smaller next stage covers m1 being computed
      # from the rows with an observed outcome
      for (n2j in c(20, 10)) {
        args <- list(
          intervention_components_coeff = coeff, power_goal = 0.8,
          power_goal_approach = approach, num_centers_in_next_stage = 20,
          patients_per_center_in_next_stage = n2j, outcome_name = "y",
          icc = icc,
          power_goal_cluster_id = if (is.null(icc)) NULL else "center"
        )
        target <- function(data) {
          suppressWarnings(do.call(
            get_power_desired_outcome, c(list(data = data), args)
          ))
        }
        with_na <- target(d_na)
        expect_equal(
          with_na, target(complete),
          info = paste(approach, format(icc), n2j)
        )
        # a binding target, not a grid floor
        expect_gt(with_na, stats::plogis(coeff[[1]]) + 0.01)
        # a direct caller's missing group rows are dropped the same way
        d_group_na <- d
        d_group_na$group[c(3, 50)] <- NA
        expect_equal(
          target(d_group_na), target(d[-c(3, 50), ]),
          info = paste("NA group", approach, format(icc), n2j)
        )
      }
    }
  }
})

test_that("BB_data with missing outcomes gives the complete-case goal", {
  d <- bb_grouped_na()
  # pp2_hand_hygiene has missing values in the bundled data
  expect_true(anyNA(d$pp2_hand_hygiene))
  complete <- d[!is.na(d$pp2_hand_hygiene), ]
  for (approach in c("unconditional", "conditional")) {
    with_na <- power_run(d, approach)
    expect_equal(
      with_na$effective_outcome_goal,
      power_run(complete, approach)$effective_outcome_goal,
      info = approach
    )
    # the conditional approach used to return 0 here, silently dropping the goal
    expect_gt(with_na$effective_outcome_goal, 0)
  }
})

test_that("a power goal with only one arm in the group column is refused", {
  d <- bb_grouped_na()
  d$group <- "control"
  expect_error(
    suppressWarnings(suppressMessages(lago_optimization(
      data = d,
      outcome_name = "pp3_oxytocin_mother",
      outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(40, 3),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      outcome_goal_intention = "maximize",
      power_goal = 0.8,
      num_centers_in_next_stage = 10,
      patients_per_center_in_next_stage = 30,
      include_confidence_set = FALSE,
      quiet = TRUE
    ))),
    "both arms"
  )
})

test_that("an outcome goal works when the outcome has missing values", {
  # the goal-direction check compared the goal against mean(outcome) with no
  # na.rm, so any NA made validate_inputs() stop inside if()
  d <- bb_grouped_na()
  run <- function(data, ...) {
    suppressWarnings(suppressMessages(lago_optimization(
      data = data,
      outcome_name = "pp2_hand_hygiene",
      outcome_type = "binary",
      intervention_components = c("coaching_updt", "launch_duration"),
      intervention_lower_bounds = c(0, 0),
      intervention_upper_bounds = c(40, 3),
      cost_list_of_vectors = list(c(0, 1), c(0, 1)),
      outcome_goal = 0.5,
      outcome_goal_intention = "maximize",
      include_confidence_set = FALSE,
      quiet = TRUE,
      ...
    )))
  }
  complete <- d[!is.na(d$pp2_hand_hygiene), ]
  expect_equal(run(d)$rec_int, run(complete)$rec_int)
  both <- list(
    power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 30
  )
  expect_equal(
    do.call(run, c(list(d), both))$rec_int,
    do.call(run, c(list(complete), both))$rec_int
  )
})

test_that("an arm with rows but no observed outcome is refused", {
  # the arm is present in 'group', so only a check on observed rows catches it
  d <- bb_grouped_na()
  d$pp2_hand_hygiene[d$group == "treatment"] <- NA
  expect_error(power_run(d, "unconditional"), "both arms")
  # the function-level guard, for callers that skip validate_inputs()
  small <- make_small_clustered()
  coeff <- small_coeff(small)
  small$y[small$group == "treatment"] <- NA
  expect_error(
    get_power_desired_outcome(
      data = small, intervention_components_coeff = coeff, power_goal = 0.8,
      power_goal_approach = "unconditional", num_centers_in_next_stage = 20,
      patients_per_center_in_next_stage = 20, outcome_name = "y"
    ),
    "both arms"
  )
})

test_that("the icc center check counts only centers with an observed outcome", {
  # two treatment centers, one of them with every outcome missing: validation
  # must say so up front rather than fail later with a misleading message
  small <- make_small_clustered()
  small$center[small$center %in% c("c5", "c6")] <- "t1"
  small$center[small$center %in% c("c7", "c8")] <- "t2"
  small$y[small$center == "t2"] <- NA
  small$dose <- rep(0:4, length.out = nrow(small))
  expect_error(
    suppressWarnings(suppressMessages(lago_optimization(
      data = small, outcome_name = "y", outcome_type = "binary",
      intervention_components = "dose", intervention_lower_bounds = 0,
      intervention_upper_bounds = 4, cost_list_of_vectors = list(c(0, 1)),
      outcome_goal_intention = "maximize", power_goal = 0.8,
      num_centers_in_next_stage = 20, patients_per_center_in_next_stage = 20,
      icc = 0.03, power_goal_cluster_id = "center",
      include_confidence_set = FALSE, quiet = TRUE
    ))),
    "fewer than two distinct power_goal_cluster_id centers with an observed"
  )
})

test_that("an outcome with no observed value is refused up front", {
  d <- bb_grouped_na()
  d$pp2_hand_hygiene <- NA_real_
  expect_error(power_run(d, "unconditional"), "has no observed values")
})

test_that("the icc center check does not count a missing center id", {
  # one real treatment center plus rows whose center id is missing
  small <- make_small_clustered()
  small$center[small$center %in% c("c6", "c7", "c8")] <- NA
  small$dose <- rep(0:4, length.out = nrow(small))
  expect_error(
    suppressWarnings(suppressMessages(lago_optimization(
      data = small, outcome_name = "y", outcome_type = "binary",
      intervention_components = "dose", intervention_lower_bounds = 0,
      intervention_upper_bounds = 4, cost_list_of_vectors = list(c(0, 1)),
      outcome_goal_intention = "maximize", power_goal = 0.8,
      num_centers_in_next_stage = 20, patients_per_center_in_next_stage = 20,
      icc = 0.03, power_goal_cluster_id = "center",
      include_confidence_set = FALSE, quiet = TRUE
    ))),
    "fewer than two distinct power_goal_cluster_id centers with an observed"
  )
})
