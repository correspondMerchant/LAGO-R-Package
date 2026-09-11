#' Budget-constrained LAGO optimization
#'
#' @description Answers the reverse of the usual LAGO question. Instead of "what
#' is the least costly intervention that reaches an outcome goal?"
#' ([lago_optimization()]), `lago_budget()` asks "given a fixed budget, what is
#' the best outcome I can reach, and with which intervention?".
#'
#' The best-outcome-within-budget intervention always lies on the same
#' least-cost frontier that [lago_optimization()] traces out (for any target
#' outcome, the cheapest way to reach it is what the optimizer already finds), so
#' `lago_budget()` sweeps the outcome goal across the outcome's range with
#' [lago_sensitivity()], reads the recommended cost and estimated outcome at each
#' goal, and returns the reachable goal whose recommended cost is highest without
#' exceeding the budget (for `outcome_goal_intention = "maximize"`; the lowest
#' reachable outcome within budget for `"minimize"`). It does not touch the
#' optimizer internals, so it inherits the same model, cost functions and bounds.
#'
#' @param object An optional `lago` result from [lago_optimization()]. When
#' supplied, the baseline optimization arguments are read from the call it
#' carries, so the whole call need not be retyped, and anything in `...`
#' overrides those stored values. When `NULL` (the default), the baseline
#' arguments come from `...`. Passing a non-`lago` object, or a `lago` result
#' from a version that did not record its call arguments, is an error.
#' @param ... The baseline [lago_optimization()] arguments (the user's own
#' optimization call), forwarded unchanged to every run. `outcome_goal` is
#' swept and so need not be supplied; `include_confidence_set` and `quiet` are
#' overridden (the confidence set is never computed during the search).
#' @param budget A single positive, finite numeric value. The maximum total cost
#' allowed, in the same units as the supplied `cost_list_of_vectors` (or
#' `unit_costs`).
#' @param n_grid A single integer (>= 2) giving how many outcome goals to try
#' across the outcome's range. Larger values give a finer, more precise answer
#' at the cost of more optimization runs. A second refinement pass is run around
#' the affordability boundary, so the effective resolution is finer than
#' `n_grid` alone. Default 25.
#' @param quiet A boolean forwarded to [lago_optimization()] via
#' [lago_sensitivity()]. Defaults to `TRUE` so the search is not noisy.
#'
#' @details The candidate outcome goals span the outcome's range: `(0, 1)` for a
#' binary outcome, or the observed range of the outcome column (padded by half a
#' span on each side) for a continuous one. Each goal is one
#' [lago_optimization()] run with the confidence set off. A goal beyond the
#' reachable range does not error: the optimizer's shrinking method returns a
#' recommendation whose estimated outcome falls short of the goal, so such a goal
#' is not counted as genuinely reached and does not become a candidate (only
#' goals the recommendation actually delivers are considered). Among the reached
#' goals whose recommended cost is at most `budget`, the one with the best
#' estimated outcome is chosen (largest for `"maximize"`, smallest for
#' `"minimize"`). The grid is extended in the improving direction until goals
#' stop being reached or affordable, and a refinement sweep is then run near the
#' affordability boundary to tighten the estimate.
#'
#' If no reachable goal is affordable (the budget is below the cost of even the
#' cheapest reachable intervention), the result has `feasible = FALSE` and `NA`
#' recommendation fields. If every reachable goal is affordable, the budget does
#' not bind (`binding = FALSE`) and the maximum reachable outcome is returned.
#'
#' @return An object of class `"lago_budget"`, a list with:
#' \describe{
#'   \item{budget}{The supplied budget.}
#'   \item{feasible}{`TRUE` if at least one reachable intervention was
#'   affordable.}
#'   \item{binding}{`TRUE` if the budget constrained the choice: a strictly
#'   better outcome was reachable but cost more than the budget. `FALSE` if the
#'   best reachable outcome was already affordable.}
#'   \item{rec_int}{The recommended intervention (one value per component), or
#'   `NA` when not `feasible`.}
#'   \item{rec_int_cost}{Its cost (at most `budget`), or `NA`.}
#'   \item{est_outcome}{Its estimated outcome, or `NA`.}
#'   \item{outcome_goal}{The outcome goal that produced the recommendation
#'   (equal to `est_outcome` up to the grid resolution), or `NA`.}
#'   \item{frontier}{A `data.frame` of the reachable goals searched (the
#'   cost/outcome tradeoff curve): `outcome_goal`, `rec_int_cost`, `est_outcome`,
#'   `affordable`, `status`.}
#'   \item{component_names}{The intervention component names.}
#'   \item{intention}{The `outcome_goal_intention` used.}
#' }
#'
#' @examples
#' \donttest{
#' # How good an outcome can a budget of 100 buy? (mtcars, continuous.)
#' b <- lago_budget(
#'   data = mtcars,
#'   outcome_name = "mpg",
#'   outcome_type = "continuous",
#'   glm_family = "gaussian",
#'   link = "identity",
#'   intervention_components = c("gear", "qsec"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 350),
#'   cost_list_of_vectors = list(c(0, 4), c(4, 6)),
#'   outcome_goal_intention = "maximize",
#'   budget = 100
#' )
#' b
#' plot(b)
#'
#' # Reuse a fitted result's call instead of retyping it.
#' opt <- lago_optimization(
#'   data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
#'   glm_family = "gaussian", link = "identity",
#'   intervention_components = c("gear", "qsec"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 350),
#'   cost_list_of_vectors = list(c(0, 4), c(4, 6)),
#'   outcome_goal = 30, outcome_goal_intention = "maximize",
#'   include_confidence_set = FALSE, quiet = TRUE
#' )
#' lago_budget(opt, budget = 100)
#' }
#'
#' @family LAGO functions
#' @seealso [lago_optimization()], [lago_sensitivity()]
#' @export
lago_budget <- function(object = NULL, ..., budget, n_grid = 25L,
                        quiet = TRUE) {
  dots <- list(...)

  # Resolve the baseline optimization arguments the same way lago_sensitivity()
  # does: from a fitted result's stored call (with `...` overriding), or from
  # `...` directly. We only need a few of them here to build the goal grid; the
  # full set is forwarded to lago_sensitivity() below.
  if (!is.null(object)) {
    if (!inherits(object, "lago")) {
      stop(paste(
        "`object` must be a `lago` result from lago_optimization(). Pass the",
        "lago_optimization() arguments directly if you do not have one."
      ))
    }
    stored <- attr(object, "lago_call_args")
    if (is.null(stored)) {
      stop(paste(
        "`object` does not carry its call arguments (it was created by an",
        "older version of lago_optimization()). Refit with the current version",
        "of the package, or pass the lago_optimization() arguments directly."
      ))
    }
    base <- stored
    for (nm in names(dots)) base[[nm]] <- dots[[nm]]
  } else {
    base <- dots
  }

  # --- validate budget / n_grid --------------------------------------------
  if (missing(budget) || !is.numeric(budget) || length(budget) != 1L ||
    !is.finite(budget) || budget <= 0) {
    stop("`budget` must be a single positive, finite numeric value.")
  }
  n_grid <- suppressWarnings(as.integer(n_grid))
  if (length(n_grid) != 1L || is.na(n_grid) || n_grid < 2L) {
    stop("`n_grid` must be a single integer >= 2.")
  }

  data <- base$data
  outcome_name <- base$outcome_name
  outcome_type <- base$outcome_type
  intention <- base$outcome_goal_intention
  if (is.null(intention)) intention <- "maximize"
  maximize <- !identical(intention, "minimize")
  if (is.null(data) || is.null(outcome_name) || is.null(outcome_type)) {
    stop(paste(
      "`data`, `outcome_name` and `outcome_type` must be available (via",
      "`object` or `...`) to run a budget search."
    ))
  }
  if (!outcome_name %in% names(data)) {
    stop(sprintf("`outcome_name` \"%s\" is not a column of `data`.", outcome_name))
  }

  # --- candidate outcome goals across the outcome's range -------------------
  goal_grid <- function(n) {
    if (identical(outcome_type, "binary")) {
      seq(0.02, 0.98, length.out = n)
    } else {
      y <- suppressWarnings(as.numeric(data[[outcome_name]]))
      rng <- range(y[is.finite(y)])
      span <- diff(rng)
      if (!is.finite(span) || span <= 0) span <- abs(rng[1]) + 1
      seq(rng[1] - span / 2, rng[2] + span / 2, length.out = n)
    }
  }

  # One sensitivity sweep over a set of goals -> a tidy data.frame with the
  # recommended cost, estimated outcome and per-run status. lago_sensitivity()
  # warns when some runs fail; unreachable goals are expected here, so that
  # warning is muffled (genuine data/model warnings from each fit still show).
  sweep <- function(goals) {
    sens_args <- c(
      list(object = object), dots,
      list(parameter = "outcome_goal", values = goals, quiet = quiet)
    )
    sens <- withCallingHandlers(
      do.call(lago_sensitivity, sens_args),
      warning = function(w) {
        # muffle warnings that are artifacts of scanning the goal range, not of
        # the user's chosen configuration: lago_sensitivity()'s "N run(s) failed"
        # summary; lago_optimization()'s "the specified outcome goal is <below/at
        # the observed mean>" note (fires for every low goal); and its "not
        # estimated to be achievable" note (fires for every goal beyond the
        # reachable range, which the shrinking method then pulls back rather than
        # erroring, so these arrive as ordinary warnings). Genuine data/model-fit
        # warnings (rank deficiency, dropped covariates, non-convergence) still
        # surface.
        m <- conditionMessage(w)
        if (grepl("run\\(s\\) failed", m) ||
          grepl("specified outcome goal intention", m) ||
          grepl("not estimated to be achievable", m)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    comps <- attr(sens, "component_names")
    ok <- sens$status == "ok" &
      is.finite(sens$rec_int_cost) & is.finite(sens$est_outcome_goal)
    # A goal beyond the reachable range does not error: the shrinking method
    # returns a recommendation whose estimated outcome falls short of the goal.
    # Such a point is not a genuine point on the cost/outcome frontier, so mark
    # only goals the recommendation actually meets as `reached` (est reaches the
    # goal, up to a tolerance; overshoot from the grid counts as reached).
    tol <- 1e-6 * pmax(1, abs(sens$value))
    reached <- ok & (if (maximize) {
      sens$est_outcome_goal >= sens$value - tol
    } else {
      sens$est_outcome_goal <= sens$value + tol
    })
    data.frame(
      outcome_goal = sens$value,
      rec_int_cost = sens$rec_int_cost,
      est_outcome = sens$est_outcome_goal,
      status = sens$status,
      reached = reached,
      affordable = ok & sens$rec_int_cost <= budget,
      stringsAsFactors = FALSE
    ) -> df
    attr(df, "component_names") <- comps
    # keep the per-component recommended doses alongside, by component name
    for (cn in comps) df[[cn]] <- sens[[cn]]
    df
  }

  fr <- sweep(goal_grid(n_grid))
  comps <- attr(fr, "component_names")

  # The goal grid spans the outcome's observed range, but interventions near
  # their bounds can push the outcome beyond it, so the true affordable maximum
  # may sit above the initial grid. If the improving-most goal tried is still
  # reachable AND affordable, the answer lies further out: extend the grid in the
  # improving direction (up for maximize, down for minimize) until that extreme
  # goal is either unreachable or unaffordable (so the boundary is bracketed), or
  # a small cap is hit to bound the work.
  extreme_ok_afford <- function(df) {
    g <- if (maximize) max(df$outcome_goal) else min(df$outcome_goal)
    row <- df[df$outcome_goal == g, , drop = FALSE][1, ]
    # extend only while the improving-most goal is genuinely reached (not pulled
    # back by shrinking) AND affordable; once it is unreachable or unaffordable
    # the boundary is bracketed, which also stops the shrinking region from
    # extending the grid to its cap for a large budget.
    isTRUE(row$reached && row$affordable)
  }
  extensions <- 0L
  while (extreme_ok_afford(fr) && extensions < 6L) {
    span <- diff(range(fr$outcome_goal))
    if (!is.finite(span) || span <= 0) span <- abs(fr$outcome_goal[1]) + 1
    if (maximize) {
      g0 <- max(fr$outcome_goal)
      new_goals <- seq(g0, g0 + span, length.out = n_grid + 1L)[-1]
    } else {
      g0 <- min(fr$outcome_goal)
      new_goals <- seq(g0 - span, g0, length.out = n_grid + 1L)[-(n_grid + 1L)]
    }
    fr <- rbind(fr, sweep(new_goals))
    fr <- fr[order(fr$outcome_goal), , drop = FALSE]
    rownames(fr) <- NULL
    extensions <- extensions + 1L
  }

  # index of the best affordable, reachable goal (best = largest outcome for
  # maximize, smallest for minimize).
  best_idx <- function(df) {
    # only genuinely reached goals are real candidates: a shrunk (unreachable)
    # point does not actually deliver its goal, so it must not be recommended
    # even if its shrunk cost happens to fit the budget.
    aff <- which(df$reached & df$affordable)
    if (!length(aff)) {
      return(NA_integer_)
    }
    aff[if (maximize) which.max(df$est_outcome[aff]) else which.min(df$est_outcome[aff])]
  }

  bi <- best_idx(fr)

  # --- refinement: tighten the estimate near the affordability boundary -----
  # The true best-affordable outcome sits between the chosen goal and the
  # adjacent goal that was unaffordable or unreachable (in the improving
  # direction). Sweep a finer grid across that gap and re-pick.
  if (!is.na(bi)) {
    ord <- order(fr$outcome_goal)
    pos <- match(bi, ord)
    neighbor <- if (maximize) {
      if (pos < length(ord)) ord[pos + 1L] else NA_integer_
    } else {
      if (pos > 1L) ord[pos - 1L] else NA_integer_
    }
    if (!is.na(neighbor)) {
      lo <- min(fr$outcome_goal[bi], fr$outcome_goal[neighbor])
      hi <- max(fr$outcome_goal[bi], fr$outcome_goal[neighbor])
      if (hi > lo) {
        fr2 <- sweep(seq(lo, hi, length.out = n_grid))
        fr <- rbind(fr, fr2)
        fr <- fr[order(fr$outcome_goal), , drop = FALSE]
        rownames(fr) <- NULL
        bi <- best_idx(fr)
      }
    }
  }

  # "reachable" here means genuinely reached (the recommendation delivers the
  # goal), not merely non-erroring: unreachable goals return via shrinking.
  reachable <- fr$reached
  n_reachable <- sum(reachable)
  feasible <- !is.na(bi)
  # the budget binds if a strictly BETTER outcome is reachable but was not
  # affordable (better = higher for maximize, lower for minimize). Since the
  # choice is the best affordable outcome, any reachable better outcome must be
  # over budget, so its existence means the budget constrained the choice. This
  # is direction-aware: for minimize, expensive high-outcome goals are worse, not
  # better, and must not count as the budget binding.
  chosen_out <- if (feasible) fr$est_outcome[bi] else NA_real_
  binding <- feasible && any(reachable & (
    if (maximize) fr$est_outcome > chosen_out + 1e-9 else
      fr$est_outcome < chosen_out - 1e-9))

  if (feasible) {
    rec_int <- if (length(comps)) as.numeric(fr[bi, comps]) else numeric(0)
    names(rec_int) <- comps
    result <- list(
      budget = budget,
      feasible = TRUE,
      binding = binding,
      rec_int = rec_int,
      rec_int_cost = fr$rec_int_cost[bi],
      est_outcome = fr$est_outcome[bi],
      outcome_goal = fr$outcome_goal[bi]
    )
  } else {
    cheapest <- if (n_reachable > 0L) {
      min(fr$rec_int_cost[reachable])
    } else {
      NA_real_
    }
    if (n_reachable == 0L) {
      warning(paste(
        "No outcome goal in the searched range was reachable, so no",
        "budget-constrained recommendation could be made. Check the model,",
        "bounds and outcome type."
      ), call. = FALSE)
    } else {
      warning(sprintf(paste(
        "The budget (%s) is below the cost of the cheapest reachable",
        "intervention (%s), so no recommendation fits it."
      ), format(budget), format(cheapest)), call. = FALSE)
    }
    result <- list(
      budget = budget,
      feasible = FALSE,
      binding = TRUE,
      rec_int = stats::setNames(rep(NA_real_, length(comps)), comps),
      rec_int_cost = NA_real_,
      est_outcome = NA_real_,
      outcome_goal = NA_real_
    )
  }

  # report only genuinely reached goals as the cost/outcome frontier, so the
  # frontier (and its plot) is the real tradeoff curve, not the shrunk points
  # that beyond-reachable goals collapse to.
  result$frontier <- fr[fr$reached,
    c("outcome_goal", "rec_int_cost", "est_outcome", "affordable", "status")]
  rownames(result$frontier) <- NULL
  result$component_names <- comps
  result$intention <- intention
  class(result) <- "lago_budget"
  result
}

#' Print a budget-constrained LAGO result
#'
#' @description Prints a short header with the budget and whether it binds, then
#' the recommended intervention, its cost and estimated outcome (or a note that
#' no intervention fits the budget).
#'
#' @param x A `"lago_budget"` object from [lago_budget()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \donttest{
#' b <- lago_budget(
#'   data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
#'   glm_family = "gaussian", link = "identity",
#'   intervention_components = c("gear", "qsec"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 350),
#'   cost_list_of_vectors = list(c(0, 4), c(4, 6)),
#'   outcome_goal_intention = "maximize", budget = 100
#' )
#' print(b)
#' }
#'
#' @exportS3Method print lago_budget
print.lago_budget <- function(x, ...) {
  cli::cli_h2("LAGO budget-constrained optimization")
  cli::cli_text("Budget: {.val {x$budget}} ({x$intention}).")
  if (!x$feasible) {
    cli::cli_alert_warning(
      "No reachable intervention fits this budget; increase the budget."
    )
    return(invisible(x))
  }

  comps <- x$component_names
  rec <- x$rec_int
  rec_txt <- if (length(comps)) {
    paste(paste0(comps, " = ", format(as.numeric(rec))), collapse = ", ")
  } else {
    "(none)"
  }
  cli::cli_text("Recommended intervention: {rec_txt}")
  cli::cli_text("Cost: {.val {x$rec_int_cost}} (of {.val {x$budget}}).")
  cli::cli_text("Estimated outcome: {.val {x$est_outcome}}.")
  if (!x$binding) {
    cli::cli_alert_info(paste(
      "The budget did not bind: the whole reachable outcome range fits it,",
      "so this is the maximum reachable outcome."
    ))
  }
  invisible(x)
}

#' Plot a budget-constrained LAGO result
#'
#' @description Draws the cost/outcome frontier the search traced out (estimated
#' outcome against recommended cost across the swept goals), with a vertical line
#' at the budget and the chosen recommendation highlighted.
#'
#' @param x A `"lago_budget"` object from [lago_budget()].
#' @param ... Ignored.
#'
#' @return A [ggplot2][ggplot2::ggplot] object, or `NULL` invisibly when there is
#' nothing reachable to plot.
#'
#' @examples
#' \donttest{
#' b <- lago_budget(
#'   data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
#'   glm_family = "gaussian", link = "identity",
#'   intervention_components = c("gear", "qsec"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 350),
#'   cost_list_of_vectors = list(c(0, 4), c(4, 6)),
#'   outcome_goal_intention = "maximize", budget = 100
#' )
#' plot(b)
#' }
#'
#' @import ggplot2
#' @importFrom ggplot2 .data
#' @exportS3Method plot lago_budget
plot.lago_budget <- function(x, ...) {
  fr <- x$frontier
  keep <- fr$status == "ok" & is.finite(fr$rec_int_cost) &
    is.finite(fr$est_outcome)
  if (!any(keep)) {
    message("No reachable points to plot.")
    return(invisible(NULL))
  }
  d <- fr[keep, , drop = FALSE]
  d <- d[order(d$rec_int_cost), , drop = FALSE]

  p <- ggplot2::ggplot(
    d, ggplot2::aes(x = .data[["rec_int_cost"]], y = .data[["est_outcome"]])
  ) +
    ggplot2::geom_line(color = "#0066cc") +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[["affordable"]]), size = 2
    ) +
    ggplot2::scale_color_manual(
      values = c(`TRUE` = "#0066cc", `FALSE` = "#b0b7c0"),
      labels = c(`TRUE` = "within budget", `FALSE` = "over budget"),
      name = NULL
    ) +
    ggplot2::geom_vline(
      xintercept = x$budget, linetype = "dashed", color = "#cc3300"
    ) +
    ggplot2::labs(
      title = "LAGO budget frontier: best outcome per cost",
      x = "recommended cost", y = "estimated outcome"
    ) +
    ggplot2::theme_minimal(base_size = 14)

  if (isTRUE(x$feasible)) {
    p <- p + ggplot2::annotate(
      "point",
      x = x$rec_int_cost, y = x$est_outcome,
      color = "#cc3300", size = 3.5, shape = 18
    )
  }
  p
}
