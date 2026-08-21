# Formals of lago_optimization() that cannot be swept as a scalar numeric,
# because they are the data, a character option, a boolean flag, a list, or a
# vector-valued argument. Anything NOT in this set (e.g. outcome_goal,
# power_goal, shrinkage_threshold, icc, time_effect_optimization_value,
# num_centers_in_next_stage, patients_per_center_in_next_stage) is a scalar
# numeric that a run can override with one element of `values`. Kept as an
# explicit denylist so a new scalar numeric formal is sweepable without editing
# this file, while the vector/character/boolean formals are still rejected with
# a clear message. Confidence-set-only scalars are rejected separately (see
# .lago_confidence_set_only_formals).
.lago_non_sweepable_formals <- function() {
  c(
    "data", "input_data_structure", "outcome_name", "outcome_type",
    "intervention_components", "intervention_lower_bounds",
    "intervention_upper_bounds", "outcome_goal_intention",
    "power_goal_approach", "power_goal_cluster_id", "unit_costs",
    "default_cost_fxn_type", "cost_list_of_vectors", "glm_family", "link",
    "optimization_method", "weights", "center_characteristics",
    "center_characteristics_optimization_values", "main_components",
    "additional_covariates", "center_weights_for_outcome_goal",
    "optimization_grid_search_step_size", "confidence_set_grid_step_size",
    "include_confidence_set", "include_center_effects",
    "center_effects_optimization_values", "include_time_effects",
    "include_interaction_terms", "prev_recommended_interventions", "quiet"
  )
}

# Scalar numeric formals that only affect the confidence set. lago_sensitivity()
# forces include_confidence_set = FALSE, and none of the reported columns
# (rec_int, rec_int_cost, est_outcome_goal) depend on the confidence set, so
# sweeping one of these would return identical rows. They are rejected with a
# tailored message rather than silently producing a flat, meaningless curve.
.lago_confidence_set_only_formals <- function() {
  c("confidence_set_alpha")
}

#' Sensitivity analysis for a LAGO recommendation
#'
#' @description Trial designers often are unsure about some of the inputs to
#' [lago_optimization()], chiefly the outcome goal and the assumed intervention
#' costs. `lago_sensitivity()` answers "how much does the recommendation move
#' if that input is different?" by re-running [lago_optimization()] across a
#' sweep of one input and reporting how the recommended intervention, its cost,
#' and the estimated outcome change. This turns a single point recommendation
#' into a robustness picture: a stringency curve for the outcome goal, or a
#' straight line for a uniform cost rescaling.
#'
#' The confidence set is the slow part of [lago_optimization()] and is not
#' needed to see how a recommendation moves, so every run is forced to
#' `include_confidence_set = FALSE` for speed. A single run failing does not
#' abort the sweep: its outputs are recorded as `NA` and a single warning names
#' the failed values afterwards.
#'
#' @param ... The baseline [lago_optimization()] arguments (the user's own
#' optimization call). They are forwarded unchanged to every run. Typically the
#' user copies their `lago_optimization(...)` call in and simply adds
#' `parameter` and `values`. `include_confidence_set` and `quiet` supplied here
#' are overridden (see Details).
#' @param parameter A single character string naming what to vary. Two modes:
#' \enumerate{
#'   \item The name of a scalar numeric argument of [lago_optimization()] that
#'   affects the recommendation (for example `"outcome_goal"`, `"power_goal"`,
#'   or `"shrinkage_threshold"`). Each run overrides that argument with one
#'   element of `values`. Vector-valued arguments such as
#'   `intervention_lower_bounds`, and confidence-set-only arguments such as
#'   `confidence_set_alpha` (the confidence set is not computed during a sweep),
#'   are rejected.
#'   \item The special string `"cost_multiplier"`: each run multiplies every
#'   coefficient of every vector in the baseline `cost_list_of_vectors` by one
#'   element of `values`, so `values = c(0.8, 1, 1.2)` evaluates the costs at
#'   80\%, 100\%, and 120\%. This mode requires `cost_list_of_vectors` in
#'   `...` and all `values` must be positive.
#' }
#' @param values A non-empty numeric vector, all finite. One run per element.
#' @param quiet A boolean forwarded to [lago_optimization()]. Defaults to
#' `TRUE` so the sweep is not noisy. Genuine warnings about the data or model
#' fit from each run are still shown.
#'
#' @details Each run builds the modified argument list from `...`, forces
#' `include_confidence_set = FALSE`, sets `quiet = quiet`, and calls
#' [lago_optimization()] inside `tryCatch()`. A run that errors contributes a
#' row of `NA` outputs with a non-`"ok"` status instead of stopping the sweep.
#'
#' @return An object of class `"lago_sensitivity"`, which is a `data.frame`
#' with one row per element of `values` and columns:
#' \describe{
#'   \item{value}{The swept value for that run.}
#'   \item{<component>}{One numeric column per intervention component, named by
#'   the component, holding its recommended value for that run.}
#'   \item{rec_int_cost}{The cost of the recommended intervention.}
#'   \item{est_outcome_goal}{The estimated outcome at the recommendation.}
#'   \item{status}{`"ok"` for a successful run, otherwise `"error"`.}
#' }
#' The object carries attributes `parameter` (the swept string),
#' `component_names` (the component column names), `baseline` (the full
#' [lago_optimization()] result at the baseline value, i.e. multiplier 1 for
#' `"cost_multiplier"` or the value supplied in `...` for a named parameter, or
#' `NULL` if not present), and, when any run failed, `error_messages` (the
#' error text named by the failed values).
#'
#' @examples
#' \donttest{
#' # How sensitive is the recommendation to the outcome goal? Each run is a
#' # separate optimization, so this is wrapped in \donttest to keep automated
#' # checks fast; the confidence set is off internally so it still runs quickly.
#' sens <- lago_sensitivity(
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
#'   parameter = "outcome_goal",
#'   values = c(30, 35, 40)
#' )
#' sens
#' plot(sens)
#'
#' # How sensitive is it to the assumed costs? A uniform rescaling never changes
#' # which intervention is cheapest, so the recommendation is unchanged and the
#' # cost scales linearly with the multiplier.
#' cost_sens <- lago_sensitivity(
#'   data = mtcars,
#'   outcome_name = "mpg",
#'   outcome_type = "continuous",
#'   glm_family = "gaussian",
#'   link = "identity",
#'   intervention_components = c("gear", "qsec"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 350),
#'   cost_list_of_vectors = list(c(0, 4), c(4, 6)),
#'   outcome_goal = 40,
#'   outcome_goal_intention = "maximize",
#'   parameter = "cost_multiplier",
#'   values = c(0.8, 1, 1.2)
#' )
#' cost_sens
#' }
#'
#' @family LAGO functions
#' @seealso [lago_optimization()]
#' @export
lago_sensitivity <- function(..., parameter, values, quiet = TRUE) {
  dots <- list(...)

  # --- validate `parameter` -------------------------------------------------
  if (missing(parameter) || !is.character(parameter) ||
    length(parameter) != 1L || is.na(parameter)) {
    stop("`parameter` must be a single, non-NA character string.")
  }

  cost_mode <- identical(parameter, "cost_multiplier")

  if (!cost_mode) {
    formal_names <- names(formals(lago_optimization))
    if (!parameter %in% formal_names) {
      stop(sprintf(
        paste0(
          "`parameter` must be \"cost_multiplier\" or the name of a scalar ",
          "numeric argument of lago_optimization(); \"%s\" is not a formal ",
          "argument."
        ),
        parameter
      ))
    }
    if (parameter %in% .lago_non_sweepable_formals()) {
      stop(sprintf(
        paste0(
          "`parameter` \"%s\" is not a scalar numeric argument and cannot be ",
          "swept. Sweep a scalar numeric argument such as \"outcome_goal\", ",
          "\"power_goal\", or \"shrinkage_threshold\", or use ",
          "\"cost_multiplier\"."
        ),
        parameter
      ))
    }
    if (parameter %in% .lago_confidence_set_only_formals()) {
      stop(sprintf(
        paste0(
          "`parameter` \"%s\" only affects the confidence set, which ",
          "lago_sensitivity() does not compute (it forces ",
          "include_confidence_set = FALSE for speed), so sweeping it would ",
          "return identical rows. Sweep a parameter that affects the ",
          "recommendation, such as \"outcome_goal\" or \"power_goal\", or use ",
          "\"cost_multiplier\"."
        ),
        parameter
      ))
    }
  }

  # --- validate `values` ----------------------------------------------------
  if (missing(values) || !is.numeric(values) || length(values) == 0L) {
    stop("`values` must be a non-empty numeric vector.")
  }
  if (any(!is.finite(values))) {
    stop("`values` must be all finite (no NA, NaN, or Inf).")
  }

  # --- cost_multiplier-specific validation ----------------------------------
  if (cost_mode) {
    if (!"cost_list_of_vectors" %in% names(dots) ||
      is.null(dots$cost_list_of_vectors)) {
      stop(paste(
        "`parameter = \"cost_multiplier\"` requires `cost_list_of_vectors`",
        "in `...` (the baseline cost functions to scale)."
      ))
    }
    if (any(values <= 0)) {
      stop(paste(
        "For `parameter = \"cost_multiplier\"`, all `values` must be",
        "positive; a non-positive cost multiplier is not meaningful."
      ))
    }
  }

  # component names to fall back on if every run fails (so the columns are
  # still meaningful). rec_int / display_components hold main effects only, so
  # main_components is preferred when interaction terms are used.
  guess_components <- if (isTRUE(dots$include_interaction_terms) &&
    !is.null(dots$main_components)) {
    dots$main_components
  } else {
    dots$intervention_components
  }

  # --- run the sweep --------------------------------------------------------
  n <- length(values)
  runs <- vector("list", n)
  errors <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    args <- dots
    if (cost_mode) {
      args$cost_list_of_vectors <- lapply(
        dots$cost_list_of_vectors, function(v) v * values[i]
      )
    } else {
      args[[parameter]] <- values[i]
    }
    # the confidence set is the slow part and is not needed to see how a
    # recommendation moves, so it is forced off; quiet is honored.
    args$include_confidence_set <- FALSE
    args$quiet <- quiet

    res <- tryCatch(
      do.call(lago_optimization, args),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      errors[i] <- conditionMessage(res)
      # leave runs[[i]] as its pre-allocated NULL (do NOT assign NULL, which
      # would delete the element and shift the list).
    } else {
      runs[[i]] <- res
    }
  }

  ok <- !vapply(runs, is.null, logical(1))

  # --- resolve the component column names -----------------------------------
  component_names <- NULL
  first_ok <- which(ok)
  if (length(first_ok) >= 1L) {
    r <- runs[[first_ok[1]]]
    component_names <- r$display_components %||% r$intervention_components
  }
  if (is.null(component_names)) component_names <- guess_components
  if (is.null(component_names)) component_names <- character(0)
  n_comp <- length(component_names)

  # --- assemble the per-run outputs -----------------------------------------
  comp_mat <- matrix(NA_real_, nrow = n, ncol = n_comp)
  cost <- rep(NA_real_, n)
  est <- rep(NA_real_, n)
  status <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    if (ok[i]) {
      r <- runs[[i]]
      ri <- as.numeric(r$rec_int)
      if (n_comp > 0L) {
        k <- min(n_comp, length(ri))
        if (k > 0L) comp_mat[i, seq_len(k)] <- ri[seq_len(k)]
      }
      cost[i] <- as.numeric(r$rec_int_cost)
      est[i] <- as.numeric(r$est_outcome_goal)
      status[i] <- "ok"
    } else {
      status[i] <- "error"
    }
  }

  df <- data.frame(value = values, stringsAsFactors = FALSE)
  if (n_comp > 0L) {
    comp_df <- as.data.frame(comp_mat, stringsAsFactors = FALSE)
    names(comp_df) <- component_names
    df <- cbind(df, comp_df)
  }
  df$rec_int_cost <- cost
  df$est_outcome_goal <- est
  df$status <- status
  rownames(df) <- NULL

  # --- baseline run (multiplier 1, or the value supplied in `...`) ----------
  baseline <- NULL
  baseline_value <- if (cost_mode) 1 else dots[[parameter]]
  if (!is.null(baseline_value) && is.numeric(baseline_value) &&
    length(baseline_value) == 1L) {
    idx <- which(values == baseline_value & ok)
    if (length(idx) >= 1L) baseline <- runs[[idx[1]]]
  }

  attr(df, "parameter") <- parameter
  attr(df, "component_names") <- component_names
  attr(df, "baseline") <- baseline
  if (any(!ok)) {
    attr(df, "error_messages") <- stats::setNames(
      errors[!ok], as.character(values[!ok])
    )
  }
  class(df) <- c("lago_sensitivity", "data.frame")

  # --- one warning naming the failed values ---------------------------------
  if (any(!ok)) {
    warning(
      sprintf(
        paste0(
          "%d of %d %s run(s) failed (values: %s); their outputs are NA in ",
          "the result. See attr(x, \"error_messages\") for the messages."
        ),
        sum(!ok), n,
        if (cost_mode) "cost_multiplier" else parameter,
        paste(values[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df
}

#' Print a LAGO sensitivity analysis
#'
#' @description Prints a short header naming the varied parameter, the number
#' of runs and how many failed, then the sensitivity `data.frame`, then a
#' one-line summary of how `rec_int_cost` ranges over the sweep.
#'
#' @param x A `"lago_sensitivity"` object from [lago_sensitivity()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \donttest{
#' sens <- lago_sensitivity(
#'   data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
#'   glm_family = "gaussian", link = "identity",
#'   intervention_components = c("gear", "qsec"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 350),
#'   cost_list_of_vectors = list(c(0, 4), c(4, 6)),
#'   outcome_goal_intention = "maximize",
#'   parameter = "outcome_goal", values = c(30, 35, 40)
#' )
#' print(sens)
#' }
#'
#' @exportS3Method print lago_sensitivity
print.lago_sensitivity <- function(x, ...) {
  param <- attr(x, "parameter")
  n <- nrow(x)
  n_fail <- sum(x$status != "ok")

  cli::cli_h2("LAGO sensitivity analysis")
  cli::cli_text(
    "Varied {.field {param}} across {n} run{?s}; {n_fail} failed."
  )

  # print the plain data.frame body (not this method, to avoid recursion).
  print.data.frame(x)

  ok <- x$status == "ok" & is.finite(x$rec_int_cost)
  if (any(ok)) {
    cost <- x$rec_int_cost[ok]
    vals <- x$value[ok]
    # pre-format so there are no glue braces in the interpolated string.
    msg <- sprintf(
      "rec_int_cost ranges from %s to %s as %s goes from %s to %s.",
      format(min(cost)), format(max(cost)), param,
      format(vals[1]), format(vals[length(vals)])
    )
    cli::cli_text(msg)
  }

  invisible(x)
}

#' Plot a LAGO sensitivity analysis
#'
#' @description Draws the sensitivity curve with [ggplot2][ggplot2::ggplot].
#' By default (`show = "cost"`) it plots the recommended cost against the swept
#' value as a line with points: for an `"outcome_goal"` sweep this is the
#' cost-of-stringency curve, and for a `"cost_multiplier"` sweep it is a
#' straight line. With `show = "components"` it instead plots the recommended
#' value of each intervention component against the swept value, one coloured
#' line per component. Rows with `NA` outputs are dropped with a message.
#'
#' @param x A `"lago_sensitivity"` object from [lago_sensitivity()].
#' @param show One of `"cost"` (default, plot `rec_int_cost`) or
#' `"components"` (plot the per-component recommended values).
#' @param ... Ignored.
#'
#' @return A [ggplot2][ggplot2::ggplot] object, or `NULL` invisibly when
#' `show = "components"` and no components are recorded.
#'
#' @examples
#' \donttest{
#' sens <- lago_sensitivity(
#'   data = mtcars, outcome_name = "mpg", outcome_type = "continuous",
#'   glm_family = "gaussian", link = "identity",
#'   intervention_components = c("gear", "qsec"),
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 350),
#'   cost_list_of_vectors = list(c(0, 4), c(4, 6)),
#'   outcome_goal_intention = "maximize",
#'   parameter = "outcome_goal", values = c(30, 35, 40)
#' )
#' plot(sens)
#' plot(sens, show = "components")
#' }
#'
#' @import ggplot2
#' @importFrom ggplot2 .data
#' @exportS3Method plot lago_sensitivity
plot.lago_sensitivity <- function(x, show = c("cost", "components"), ...) {
  show <- match.arg(show)
  param <- attr(x, "parameter")
  comps <- attr(x, "component_names")

  if (identical(show, "cost")) {
    keep <- is.finite(x$value) & is.finite(x$rec_int_cost)
    n_drop <- sum(!keep)
    if (n_drop > 0L) {
      message(n_drop, " row(s) with NA cost dropped from the plot.")
    }
    d <- x[keep, , drop = FALSE]
    p <- ggplot2::ggplot(
      d, ggplot2::aes(x = .data[["value"]], y = .data[["rec_int_cost"]])
    ) +
      ggplot2::geom_line(color = "#0066cc") +
      ggplot2::geom_point(color = "#0066cc", size = 2) +
      ggplot2::labs(
        title = "LAGO sensitivity: recommended cost",
        x = param, y = "recommended cost"
      ) +
      ggplot2::theme_minimal(base_size = 14)
    return(p)
  }

  # show == "components"
  if (length(comps) == 0L) {
    message("No intervention components recorded; nothing to plot.")
    return(invisible(NULL))
  }
  # build the long data.frame with base R (tidyr is not a dependency).
  long <- do.call(rbind, lapply(comps, function(cn) {
    data.frame(
      value = x$value,
      component = cn,
      rec_value = x[[cn]],
      stringsAsFactors = FALSE
    )
  }))
  keep <- is.finite(long$value) & is.finite(long$rec_value)
  n_drop <- sum(!keep)
  if (n_drop > 0L) {
    message(n_drop, " row(s) with NA component value dropped from the plot.")
  }
  long <- long[keep, , drop = FALSE]

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data[["value"]], y = .data[["rec_value"]],
      color = .data[["component"]]
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "LAGO sensitivity: recommended intervention",
      x = param, y = "recommended value", color = "component"
    ) +
    ggplot2::theme_minimal(base_size = 14)
  p
}
