# Null-coalescing helper (base R gained %||% only in 4.4.0; DESCRIPTION allows
# older R, so define a local one).
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Print a LAGO optimization result
#'
#' @description Concise console display of the object returned by
#' [lago_optimization()]: the recommended intervention (component and value),
#' its cost, the estimated outcome, and, when available, the confidence-set size
#' and whether a power goal or overall test was used.
#'
#' @param x A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @exportS3Method print lago
print.lago <- function(x, ...) {
  cli::cli_h1("LAGO optimization result")

  # recommended intervention: rec_int is unnamed, so zip it with the stored
  # component labels for a readable table. display_components lines up with
  # rec_int (main components when interaction terms are used); fall back to
  # intervention_components for robustness.
  comps <- x$display_components %||% x$intervention_components
  vals <- x$rec_int
  cli::cli_h3("Recommended intervention")
  if (!is.null(comps) && length(comps) == length(vals)) {
    for (i in seq_along(vals)) {
      cli::cli_li("{comps[i]}: {round(vals[i], 4)}")
    }
  } else {
    cli::cli_li("{round(vals, 4)}")
  }

  cli::cli_text("Cost: {round(x$rec_int_cost, 4)}")
  cli::cli_text("Estimated outcome: {round(x$est_outcome_goal, 4)}")
  if (!is.null(x$outcome_goal)) {
    cli::cli_text("Outcome goal: {x$outcome_goal}")
  }
  if (!is.null(x$power_goal)) {
    cli::cli_text("Power goal: {x$power_goal}")
  }
  if (!is.null(x$confidence_set_size_percentage)) {
    cli::cli_text(
      "95% confidence set size: {round(100 * x$confidence_set_size_percentage, 2)}% of the grid"
    )
  }
  if (!is.null(x$test_results)) {
    cli::cli_text(
      "Overall intervention test: p = {signif(x$test_results$p_val, 3)}"
    )
  }
  cli::cli_text(
    "Use summary() for the confidence set and test detail, plot() to visualize."
  )
  invisible(x)
}

#' Summarize a LAGO optimization result
#'
#' @description Fuller display than [print.lago()]: adds the confidence-set cost
#' range and first rows, and the overall-test statistic and p-value when present.
#'
#' @param object A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary lago
summary.lago <- function(object, ...) {
  print(object)

  if (!is.null(object$cs)) {
    cli::cli_h3("Confidence set")
    if (!is.null(object$cs$cost)) {
      cli::cli_text(
        "Cost range in the 95% confidence set: {round(min(object$cs$cost), 2)} - {round(max(object$cs$cost), 2)}"
      )
    }
    cli::cli_text("First rows of the confidence set:")
    print(utils::head(object$cs))
  }

  if (!is.null(object$test_results)) {
    cli::cli_h3("Overall intervention-effect test")
    cli::cli_text(
      "test statistic = {round(object$test_results$test_stat, 4)}, p = {signif(object$test_results$p_val, 4)}"
    )
  }
  invisible(object)
}

#' Plot a LAGO optimization result
#'
#' @description Visualizes the 95% confidence set. For a two-component
#' intervention it plots the grid points in the confidence set with the
#' recommended intervention highlighted; for a single component it plots the
#' confidence interval bounds against the dose. Requires a confidence set
#' (`include_confidence_set = TRUE` in [lago_optimization()]); otherwise it
#' returns invisibly with a message rather than erroring.
#'
#' @param x A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly) when a plot is produced, otherwise `NULL`
#' invisibly.
#' @importFrom ggplot2 .data
#' @exportS3Method plot lago
plot.lago <- function(x, ...) {
  if (is.null(x$cs)) {
    message(
      "No confidence set to plot. Re-run lago_optimization() with ",
      "include_confidence_set = TRUE."
    )
    return(invisible(NULL))
  }

  # number of intervention components comes from the stored labels, NOT from
  # ncol(cs): the confidence set gains extra columns (center characteristics,
  # CI bounds, cost), so counting cs columns would miscount the components.
  # display_components matches the cs main-effect columns (main components when
  # interaction terms are used); fall back to intervention_components.
  comps <- x$display_components %||% x$intervention_components
  n_comp <- length(comps)

  if (n_comp == 2) {
    cs <- x$cs
    p <- ggplot2::ggplot(
      cs,
      ggplot2::aes(x = .data[[comps[1]]], y = .data[[comps[2]]])
    ) +
      ggplot2::geom_point(color = "#0066cc", alpha = 0.6) +
      ggplot2::annotate(
        "point",
        x = x$rec_int[1], y = x$rec_int[2],
        color = "#cc3300", size = 4, shape = 18
      ) +
      ggplot2::labs(
        title = "95% confidence set",
        subtitle = "Red diamond: recommended intervention",
        x = comps[1], y = comps[2]
      ) +
      ggplot2::theme_minimal(base_size = 14)
    print(p)
    return(invisible(p))
  }

  if (n_comp == 1) {
    cs <- x$cs
    p <- ggplot2::ggplot(
      cs,
      ggplot2::aes(x = .data[[comps[1]]])
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data[["CI_lower_bound"]], ymax = .data[["CI_upper_bound"]]
        ),
        fill = "#0066cc", alpha = 0.2
      ) +
      ggplot2::geom_vline(
        xintercept = x$rec_int[1], color = "#cc3300", linetype = "dashed"
      ) +
      ggplot2::labs(
        title = "95% confidence set",
        subtitle = "Dashed line: recommended intervention",
        x = comps[1], y = "Outcome CI"
      ) +
      ggplot2::theme_minimal(base_size = 14)
    print(p)
    return(invisible(p))
  }

  message(
    "plot() supports 1- or 2-component interventions; ",
    "this result has ", n_comp, " components. ",
    "Use $cs to inspect the confidence set directly."
  )
  invisible(NULL)
}
