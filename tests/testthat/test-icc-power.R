# Tests for the ICC / design-effect option in the power calculation (issue #29).
#
# The design effect only bites when the ncp constraint binds, which needs a
# small sample (on large data the control-rate floor binds first and icc cannot
# move the answer). We build a small clustered synthetic dataset for the
# direct-function tests, and use BB_data for the backward-compat path.

make_small_clustered <- function() {
  # deterministic small two-arm clustered dataset. J centers per label, m each.
  J <- 8
  m <- 10
  rows <- list()
  # fixed 0/1 pattern per center to avoid any RNG dependence across R versions
  for (j in seq_len(J)) {
    grp <- if (j <= J / 2) "control" else "treatment"
    rate <- if (grp == "treatment") 0.45 else 0.30
    y <- as.integer(seq_len(m) / m <= rate)
    rows[[j]] <- data.frame(
      center = paste0("c", j), group = grp, y = y
    )
  }
  do.call(rbind, rows)
}

small_coeff <- function(d) {
  c(
    "(Intercept)" = stats::qlogis(mean(d$y[d$group == "control"])),
    "dose" = 0.6
  )
}

test_that("icc = NULL and icc = 0 give identical power-implied outcomes", {
  d <- make_small_clustered()
  coeff <- small_coeff(d)
  common <- list(
    data = d, intervention_components_coeff = coeff, power_goal = 0.8,
    power_goal_approach = "unconditional", num_centers_in_next_stage = 20,
    patients_per_center_in_next_stage = 20, outcome_name = "y"
  )
  null_res <- suppressWarnings(do.call(get_power_desired_outcome, common))
  zero_res <- suppressWarnings(do.call(
    get_power_desired_outcome,
    c(common, list(icc = 0, power_goal_cluster_id = "center"))
  ))
  expect_equal(null_res, zero_res)
})

test_that("a larger icc raises the power-implied outcome (both approaches)", {
  d <- make_small_clustered()
  coeff <- small_coeff(d)
  call_icc <- function(icc, approach) {
    suppressWarnings(get_power_desired_outcome(
      data = d, intervention_components_coeff = coeff, power_goal = 0.8,
      power_goal_approach = approach, num_centers_in_next_stage = 20,
      patients_per_center_in_next_stage = 20, outcome_name = "y",
      icc = icc,
      power_goal_cluster_id = if (is.null(icc) || all(icc == 0)) NULL else "center"
    ))
  }
  for (approach in c("unconditional", "conditional")) {
    r0 <- call_icc(0, approach)
    r_small <- call_icc(0.01, approach)
    r_big <- call_icc(0.03, approach)
    # monotone non-decreasing, and strictly higher somewhere (not saturated flat)
    expect_true(r0 <= r_small)
    expect_true(r_small <= r_big)
    expect_true(r_big > r0)
  }
})

test_that("the shipped both-stage formula matches an independent reference solver", {
  # Pin the ACTUAL get_power_desired_outcome() output against an independent
  # re-implementation of the unconditional both-stage variance. This calls the
  # real function (not just its own formula), so a DE1/DE2 swap, a
  # whole-variance multiply, or a stage-2-only form in the code would fail here
  # even though it might pass a monotonicity test.
  d <- make_small_clustered()
  coeff <- small_coeff(d)
  icc <- 0.02
  j <- 20
  n2j <- 20

  # independent reference: replicate get_power_desired_outcome's grid search
  # with the both-stage variance built here from scratch.
  ref_required_outcome <- function() {
    required_ncp <- stats::uniroot(function(ncp) {
      (1 - stats::pchisq(stats::qchisq(0.95, 1), 1, ncp)) - 0.8
    }, c(0, 100))$root
    de2 <- 1 + (n2j - 1) * icc
    de1_arm <- function(grp) {
      m <- as.numeric(table(d[d$group == grp, "center"]))
      m1 <- sum(m^2) / sum(m)
      1 + (m1 - 1) * icc
    }
    de1_int <- de1_arm("treatment")
    de1_ctl <- de1_arm("control")
    n1_1 <- sum(d$group == "treatment")
    n0_1 <- sum(d$group == "control")
    n1_2 <- n0_2 <- j / 2 * n2j
    N1 <- n1_1 + n1_2
    N0 <- n0_1 + n0_2
    S1_1 <- sum(d$y[d$group == "treatment"])
    S0_1 <- sum(d$y[d$group == "control"])
    beta0 <- coeff[["(Intercept)"]]
    grid <- seq(rje::expit(beta0), 1, length.out = 1000)
    ncp <- vapply(grid, function(ep) {
      S1_2 <- n1_2 * ep
      S0_2 <- n0_2 * rje::expit(beta0)
      p1 <- (S1_1 + S1_2) / N1
      p0 <- (S0_1 + S0_2) / N0
      v1 <- p1 * (1 - p1) * (de1_int * n1_1 + de2 * n1_2) / N1^2
      v0 <- p0 * (1 - p0) * (de1_ctl * n0_1 + de2 * n0_2) / N0^2
      ((p1 - p0) / sqrt(v1 + v0))^2
    }, numeric(1))
    idx <- which(ncp >= required_ncp)
    ctl_pct <- S0_1 / n0_1
    pos <- grid[idx]
    pos[pos > ctl_pct][1]
  }

  shipped <- suppressWarnings(get_power_desired_outcome(
    data = d, intervention_components_coeff = coeff, power_goal = 0.8,
    power_goal_approach = "unconditional", num_centers_in_next_stage = j,
    patients_per_center_in_next_stage = n2j, outcome_name = "y",
    icc = icc, power_goal_cluster_id = "center"
  ))
  expect_equal(shipped, ref_required_outcome(), tolerance = 1e-6)
})

test_that("the conditional path matches an independent reference (DE inside sqrt, not DE^2)", {
  # Pin the real conditional get_power_desired_outcome() against an independent
  # reimplementation. de2_power = 1 is the correct DE-inside-sqrt form; a DE^2
  # bug on the sd-form sigma_hat_x_2 (multiplying the sd by DE instead of the
  # variance inside the sqrt) corresponds to de2_power = 2 and is caught by the
  # tight (1e-6) pin below.
  d <- make_small_clustered()
  coeff <- small_coeff(d)
  icc <- 0.02
  j <- 20
  n2j <- 20

  ref_conditional <- function(de2_power) {
    z <- stats::qnorm(1 - 0.05 / 2)
    z_pi <- stats::qnorm(0.8)
    beta0 <- coeff[["(Intercept)"]]
    de2 <- 1 + (n2j - 1) * icc
    de1_arm <- function(grp) {
      mm <- as.numeric(table(d[d$group == grp, "center"]))
      1 + (sum(mm^2) / sum(mm) - 1) * icc
    }
    de1_int <- de1_arm("treatment")
    de1_ctl <- de1_arm("control")
    n1_1 <- sum(d$group == "treatment")
    n0_1 <- sum(d$group == "control")
    n1_2 <- n0_2 <- j / 2 * n2j
    N1 <- n1_1 + n1_2
    N0 <- n0_1 + n0_2
    S1_1 <- sum(d$y[d$group == "treatment"])
    S0_1 <- sum(d$y[d$group == "control"])
    grid <- seq(rje::expit(beta0), 1, length.out = 1000)
    vals <- vapply(grid, function(ep) {
      S1_2 <- n1_2 * ep
      S0_2 <- n0_2 * rje::expit(beta0)
      p1 <- (S1_1 + S1_2) / N1
      p0 <- (S0_1 + S0_2) / N0
      sp1 <- p1 * (1 - p1) * (de1_int * n1_1 + de2 * n1_2) / N1^2
      sp2 <- p0 * (1 - p0) * (de1_ctl * n0_1 + de2 * n0_2) / N0^2
      z_part <- z * sqrt(sp1 + sp2)
      mu <- n1_2 * ep / N1 - n0_2 * rje::expit(beta0) / N0
      # de2_power = 1: DE inside the sqrt (correct). de2_power = 2: the DE^2 bug.
      sig <- sqrt(
        (de2^de2_power) * n0_2 * rje::expit(beta0) * (1 - rje::expit(beta0)) / N0^2 +
          (de2^de2_power) * n1_2 * ep * (1 - ep) / N1^2
      )
      z_part - S1_1 / N1 + S0_1 / N0 - mu + z_pi * sig
    }, numeric(1))
    poss <- grid[vals <= 0]
    if (length(poss) > 0) poss[1] else 0
  }

  shipped <- suppressWarnings(get_power_desired_outcome(
    data = d, intervention_components_coeff = coeff, power_goal = 0.8,
    power_goal_approach = "conditional", num_centers_in_next_stage = j,
    patients_per_center_in_next_stage = n2j, outcome_name = "y",
    icc = icc, power_goal_cluster_id = "center"
  ))
  # matches the correct (DE-inside-sqrt) reference ...
  expect_equal(shipped, ref_conditional(1), tolerance = 1e-6)
  # ... and is distinguishable from the DE^2 form (guards the sd-vs-var trap).
  expect_false(isTRUE(all.equal(shipped, ref_conditional(2), tolerance = 1e-6)))
})

test_that("icc validation rejects bad inputs", {
  d <- make_small_clustered()
  base <- list(
    data = transform(d, dose = ifelse(group == "treatment", 1, 0)),
    outcome_name = "y", outcome_type = "binary",
    intervention_components = "dose",
    intervention_lower_bounds = 0, intervention_upper_bounds = 1,
    cost_list_of_vectors = list(c(0, 1)),
    power_goal = 0.8, num_centers_in_next_stage = 4,
    patients_per_center_in_next_stage = 8, include_confidence_set = FALSE
  )
  # out of range
  expect_error(
    suppressWarnings(suppressMessages(do.call(
      lago_optimization, c(base, list(icc = 1.2, power_goal_cluster_id = "center"))
    ))),
    "\\[0, 1\\)"
  )
  expect_error(
    suppressWarnings(suppressMessages(do.call(
      lago_optimization, c(base, list(icc = -0.1, power_goal_cluster_id = "center"))
    ))),
    "\\[0, 1\\)"
  )
  # wrong length
  expect_error(
    suppressWarnings(suppressMessages(do.call(
      lago_optimization,
      c(base, list(icc = c(0.1, 0.1, 0.1), power_goal_cluster_id = "center"))
    ))),
    "length 1"
  )
  # non-zero icc without a cluster id
  expect_error(
    suppressWarnings(suppressMessages(do.call(
      lago_optimization, c(base, list(icc = 0.05))
    ))),
    "power_goal_cluster_id"
  )
  # cluster id column absent
  expect_error(
    suppressWarnings(suppressMessages(do.call(
      lago_optimization, c(base, list(icc = 0.05, power_goal_cluster_id = "nope"))
    ))),
    "not found"
  )
})

test_that("icc without a power goal is ignored with a message", {
  # assert directly on validate_inputs(), which is where the message is emitted.
  # Going through the full lago_optimization() here is fragile: these small
  # fixtures can trip unrelated pre-existing issues in the optimizer, which
  # would mask (or crash before) this assertion.
  d <- transform(make_small_clustered(), dose = ifelse(group == "treatment", 1, 0))
  expect_message(
    validate_inputs(
      data = d, outcome_name = "y", outcome_type = "binary",
      intervention_components = "dose",
      intervention_lower_bounds = 0, intervention_upper_bounds = 1,
      outcome_goal = 0.6, outcome_goal_intention = "maximize",
      power_goal = NULL, power_goal_approach = "unconditional",
      cost_list_of_vectors = list(c(0, 1)), icc = 0.05
    ),
    "icc is ignored"
  )
})

test_that("an infeasible design effect warns instead of silently dropping the goal", {
  d <- make_small_clustered()
  coeff <- small_coeff(d)
  # large icc + tiny next stage -> required outcome unreachable
  expect_warning(
    get_power_desired_outcome(
      data = d, intervention_components_coeff = coeff, power_goal = 0.8,
      power_goal_approach = "unconditional", num_centers_in_next_stage = 4,
      patients_per_center_in_next_stage = 8, outcome_name = "y",
      icc = 0.3, power_goal_cluster_id = "center"
    ),
    "infeasible"
  )
  expect_warning(
    get_power_desired_outcome(
      data = d, intervention_components_coeff = coeff, power_goal = 0.99,
      power_goal_approach = "conditional", num_centers_in_next_stage = 4,
      patients_per_center_in_next_stage = 8, outcome_name = "y",
      icc = 0.5, power_goal_cluster_id = "center"
    ),
    "infeasible"
  )
})

test_that("BB_data backward compatibility: icc = NULL matches the pre-icc result", {
  bb <- BB_data
  bb$group <- ifelse(bb$pre_post == 0, "control", "treatment")
  args <- list(
    data = bb, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    center_characteristics = "birth_volume_100",
    center_characteristics_optimization_values = 1.75,
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5),
    cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
    power_goal = 0.8, num_centers_in_next_stage = 10,
    patients_per_center_in_next_stage = 30, include_confidence_set = FALSE
  )
  null_res <- suppressWarnings(suppressMessages(do.call(lago_optimization, args)))
  zero_res <- suppressWarnings(suppressMessages(do.call(
    lago_optimization,
    c(args, list(icc = 0, power_goal_cluster_id = "site_name"))
  )))
  expect_equal(null_res$est_outcome_goal, zero_res$est_outcome_goal)
})
