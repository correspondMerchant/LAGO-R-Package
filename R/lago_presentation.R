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
#   - confidence-set cost range: round(., 2)
#   - p-values: signif(., 3) in the compact line, signif(., 4) in the full line

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
#' @return A named list with elements `recommendation`, `cost`, `outcome`,
#'   `power`, `confidence_set`, and `test`, in that order.
#'
#' @keywords internal
#' @noRd
lago_blocks <- function(x) {
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

  # --- outcome (estimated outcome, plus the outcome goal when present)
  outcome_rows <- paste0(
    "Estimated outcome: ", .lago_fmt_value(x$est_outcome_goal)
  )
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
      cs_rows <- c(
        cs_rows,
        paste0(
          "Cost range in the 95% confidence set: ",
          .lago_fmt_cost_range(min(x$cs$cost)),
          " - ",
          .lago_fmt_cost_range(max(x$cs$cost))
        )
      )
    }
    confidence_set <- list(
      title = "Confidence set",
      rows = cs_rows
    )
  }

  # --- overall intervention-effect test (NULL when no test results)
  test <- NULL
  if (!is.null(x$test_results)) {
    test <- list(
      title = "Overall intervention-effect test",
      rows = paste0(
        "test statistic = ",
        .lago_fmt_value(x$test_results$test_stat),
        ", p = ",
        .lago_fmt_pval(x$test_results$p_val, 4)
      )
    )
  }

  list(
    recommendation = recommendation,
    cost = cost,
    outcome = outcome,
    power = power,
    confidence_set = confidence_set,
    test = test
  )
}
