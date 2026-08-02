# Shared presentation formatter for `lago` objects.
#
# `lago_blocks()` turns a `lago` object into an ordered list of labeled,
# pre-formatted "blocks". Both the console S3 methods and the report generator
# render from these blocks, so they can never drift apart.
#
# This layer performs NO statistics: every quantity is pulled verbatim from an
# existing field and formatted with the SAME wording and rounding the current
# print.lago / summary.lago methods use. The `%||%` null-coalescing helper is
# defined once in R/lago_methods.R and is visible package-wide.

# --- small formatting helpers -------------------------------------------------
# Rounding kept consistent with print.lago / summary.lago:
#   - recommendation values, cost, estimated outcome: round(., 4)
#   - confidence-set cost IQR: round(., 2)
#   - p-values: signif(., 4)

.lago_fmt_value <- function(v) round(v, 4)

.lago_fmt_cost_range <- function(v) round(v, 2)

.lago_fmt_pval <- function(p, digits = 4) signif(p, digits)

# --- the shared formatter -----------------------------------------------------

#' Build presentation blocks for a LAGO result
#'
#' Internal helper that turns a `lago` object into an ordered list of labeled,
#' pre-formatted blocks shared by the console methods and the report generator.
#' Each block is `list(title = <string>, rows = <data.frame or character
#' vector>)`. Blocks that do not apply to the object are `NULL`
#' (`power` when there is no power goal, `confidence_set` when there is no
#' confidence set, `test` when there are no test results).
#'
#' No new statistics are computed here; every value is taken verbatim from an
#' existing field and formatted with the same wording and rounding used by
#' [print.lago()] and [summary.lago()].
#'
#' @param x A "lago" object returned by [lago_optimization()].
#'
#' @return A named list with elements `inputs`, `recommendation`, `cost`,
#'   `outcome`, `power`, `confidence_set`, and `test`, in that order.
#'
#' @keywords internal
#' @noRd
lago_blocks <- function(x) {
  # --- inputs recap: echoes the key user inputs so the console output can be
  # read on its own, the way the old output did. NULL when the fields were not
  # carried (older objects). Each entry is a pre-formatted "label: value" line.
  inputs <- NULL
  if (!is.null(x$outcome_name) || !is.null(x$input_nrow)) {
    input_rows <- character(0)
    if (!is.null(x$input_nrow)) {
      input_rows <- c(
        input_rows,
        paste0(
          "Input data dimensions: ", x$input_nrow, " rows, ",
          x$input_ncol, " columns"
        )
      )
    }
    if (!is.null(x$outcome_name)) {
      input_rows <- c(input_rows, paste0("Outcome name: ", x$outcome_name))
    }
    if (!is.null(x$outcome_type)) {
      input_rows <- c(input_rows, paste0("Outcome type: ", x$outcome_type))
    }
    comps_in <- x$intervention_components
    if (!is.null(comps_in)) {
      input_rows <- c(
        input_rows,
        paste0(
          length(comps_in), " intervention component(s): ",
          paste(comps_in, collapse = ", ")
        )
      )
    }
    if (isTRUE(x$include_interaction_terms) && !is.null(x$main_components)) {
      input_rows <- c(
        input_rows,
        paste0(
          length(x$main_components), " main effect component(s): ",
          paste(x$main_components, collapse = ", ")
        )
      )
    }
    if (!is.null(x$center_characteristics)) {
      input_rows <- c(
        input_rows,
        paste0(
          length(x$center_characteristics), " center characteristic(s): ",
          paste(x$center_characteristics, collapse = ", ")
        )
      )
    }
    if (!is.null(x$family)) {
      input_rows <- c(
        input_rows,
        paste0("Outcome model family: ", x$family),
        paste0("Outcome model link: ", x$link),
        paste0("Fixed center effects: ", isTRUE(x$include_center_effects)),
        paste0("Fixed time effects: ", isTRUE(x$include_time_effects))
      )
    }
    # Both goal lines are always shown (with "not specified" when NULL), as the
    # old output did, so the recap is unambiguous about which goal was set.
    input_rows <- c(
      input_rows,
      paste0(
        "Outcome goal: ",
        if (is.null(x$outcome_goal)) "not specified" else x$outcome_goal
      ),
      paste0(
        "Power goal: ",
        if (is.null(x$power_goal)) "not specified" else x$power_goal
      )
    )
    if (!is.null(x$power_goal) && !is.null(x$effective_outcome_goal)) {
      input_rows <- c(
        input_rows,
        paste0(
          "Effective outcome goal (max of outcome goal and ",
          "power-implied outcome): ",
          .lago_fmt_value(x$effective_outcome_goal)
        )
      )
    }
    if (!is.null(x$cost_list_of_vectors)) {
      input_rows <- c(
        input_rows,
        paste0(
          "Intervention component costs: ",
          toString(x$cost_list_of_vectors)
        )
      )
    }
    if (!is.null(x$intervention_lower_bounds)) {
      input_rows <- c(
        input_rows,
        paste0(
          "Intervention lower bounds: ",
          paste(x$intervention_lower_bounds, collapse = ", ")
        ),
        paste0(
          "Intervention upper bounds: ",
          paste(x$intervention_upper_bounds, collapse = ", ")
        )
      )
    }
    if (length(input_rows) > 0) {
      inputs <- list(title = "Inputs", rows = input_rows)
    }
  }

  # --- recommendation: zip rec_int with its component labels, exactly like
  # print.lago does. display_components lines up with rec_int; fall back to
  # intervention_components for robustness.
  comps <- x$display_components %||% x$intervention_components
  vals <- x$rec_int
  if (!is.null(comps) && length(comps) == length(vals)) {
    rec_rows <- data.frame(
      component = comps,
      value = .lago_fmt_value(vals),
      stringsAsFactors = FALSE
    )
  } else {
    rec_rows <- data.frame(value = .lago_fmt_value(vals))
  }
  recommendation <- list(
    title = "Recommended intervention",
    rows = rec_rows
  )

  # --- cost
  cost <- list(
    title = "Cost",
    rows = paste0("Cost: ", .lago_fmt_value(x$rec_int_cost))
  )

  # --- outcome (estimated outcome, its 95% CI when available, and the outcome
  # goal when present). The CI is the interval for the estimated outcome at the
  # recommended intervention, which get_confidence_set() reports in its own
  # rec_int_ci field, carried on the object as est_outcome_ci.
  outcome_rows <- paste0(
    "Estimated outcome: ", .lago_fmt_value(x$est_outcome_goal)
  )
  # Always show a CI line, with the old fallback wording when the interval is
  # not available. The interval no longer depends on the confidence set being
  # non-empty, so with the set requested it is missing only when the interval
  # at the recommended intervention itself could not be computed.
  ci_line <- if (!is.null(x$est_outcome_ci)) {
    paste0(
      "95% CI for the estimated outcome: ",
      .lago_fmt_value(x$est_outcome_ci[["lower"]]),
      " - ",
      .lago_fmt_value(x$est_outcome_ci[["upper"]])
    )
  } else if (!is.null(x$confidence_set_size_percentage)) {
    paste0(
      "95% CI for the estimated outcome: not available (the interval at the ",
      "recommended intervention could not be computed)"
    )
  } else {
    "95% CI for the estimated outcome: not available (set include_confidence_set = TRUE)"
  }
  outcome_rows <- c(outcome_rows, ci_line)
  if (!is.null(x$outcome_goal)) {
    outcome_rows <- c(
      outcome_rows,
      paste0("Outcome goal: ", x$outcome_goal)
    )
  }
  outcome <- list(
    title = "Outcome",
    rows = outcome_rows
  )

  # --- power (NULL when no power goal)
  power <- NULL
  if (!is.null(x$power_goal)) {
    power <- list(
      title = "Power",
      rows = paste0("Power goal: ", x$power_goal)
    )
  }

  # --- confidence set (NULL when no confidence set)
  confidence_set <- NULL
  if (!is.null(x$cs)) {
    cs_rows <- character(0)
    if (!is.null(x$confidence_set_size_percentage)) {
      cs_rows <- c(
        cs_rows,
        paste0(
          "95% confidence set size: ",
          round(100 * x$confidence_set_size_percentage, 2),
          "% of the grid"
        )
      )
    }
    if (!is.null(x$cs$cost)) {
      # IQR (25th-75th percentile) of the cost within the confidence set, as the
      # old console output reported it.
      qs <- stats::quantile(x$cs$cost, probs = c(0.25, 0.75), names = FALSE)
      cs_rows <- c(
        cs_rows,
        paste0(
          "IQR of the cost within the 95% confidence set: ",
          .lago_fmt_cost_range(qs[1]),
          " - ",
          .lago_fmt_cost_range(qs[2])
        )
      )
    }
    confidence_set <- list(
      title = "Confidence set",
      rows = cs_rows
    )
  }

  # --- overall intervention-effect test (NULL when no test results). Binary
  # outcomes use the two-sample test for a difference in proportions; the label
  # names the test, as the old output did.
  test <- NULL
  if (!is.null(x$test_results)) {
    test_rows <- character(0)
    if (identical(x$outcome_type, "binary")) {
      test_rows <- c(
        test_rows,
        "Two-sample test for the difference in two proportions:"
      )
    }
    test_rows <- c(
      test_rows,
      paste0("Test statistic: ", .lago_fmt_value(x$test_results$test_stat)),
      paste0("P-value: ", .lago_fmt_pval(x$test_results$p_val, 4))
    )
    test <- list(
      title = "Overall intervention-effect test",
      rows = test_rows
    )
  }

  list(
    inputs = inputs,
    recommendation = recommendation,
    cost = cost,
    outcome = outcome,
    power = power,
    confidence_set = confidence_set,
    test = test
  )
}
