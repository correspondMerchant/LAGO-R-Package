# Null-coalescing helper (base R gained %||% only in 4.4.0; DESCRIPTION allows
# older R, so define a local one).
`%||%` <- function(a, b) if (is.null(a)) b else a

# LAGO brand accent, matched to the hex logo blue also used in plot.lago
# (#0066cc). `make_ansi_style` degrades to identity on terminals without
# colour support, so this is safe everywhere.
.lago_accent <- function() cli::make_ansi_style("#0066cc")

# Shared console renderer for a `lago` object. Both print.lago (compact) and
# summary.lago (full) render through this so their output can never drift, and
# the non-quiet in-run summary in lago_optimization() calls print() on the
# assembled result, giving byte-identical output. This layer performs NO
# statistics: every quantity comes from lago_blocks(x) or verbatim from an
# existing field, with the same wording/rounding the methods have always used.
.lago_render <- function(x, full = FALSE) {
  blocks <- lago_blocks(x)
  accent <- .lago_accent()

  cli::cli_h1("{accent('LAGO optimization result')}")

  # --- recommended intervention: aligned component | value table.
  cli::cli_h3("{accent(blocks$recommendation$title)}")
  rec_rows <- blocks$recommendation$rows
  if (!is.null(rec_rows$component)) {
    dl <- stats::setNames(
      as.character(rec_rows$value), rec_rows$component
    )
    cli::cli_dl(dl)
  } else {
    for (v in rec_rows$value) cli::cli_li("{v}")
  }

  # --- cost, outcome, power: pre-formatted character rows from lago_blocks.
  for (row in blocks$cost$rows) cli::cli_text(row)
  for (row in blocks$outcome$rows) cli::cli_text(row)
  if (!is.null(blocks$power)) {
    for (row in blocks$power$rows) cli::cli_text(row)
  }

  # --- confidence set size: shown in both modes, gated on the size field so it
  # still appears when the field is present but no confidence set was found
  # (cs NULL), preserving the historical print.lago behaviour.
  if (!is.null(x$confidence_set_size_percentage)) {
    cli::cli_text(
      "95% confidence set size: {round(100 * x$confidence_set_size_percentage, 2)}% of the grid"
    )
  }

  if (!full) {
    # compact: overall-test p-value (signif 3) + a single hint line.
    if (!is.null(x$test_results)) {
      cli::cli_text(
        "Overall intervention test: p = {signif(x$test_results$p_val, 3)}"
      )
    }
    cli::cli_text(
      "Use summary() for the confidence set and test detail, plot() to visualize."
    )
    return(invisible(x))
  }

  # --- full: confidence-set cost range + first rows.
  if (!is.null(blocks$confidence_set)) {
    cli::cli_h3("{accent(blocks$confidence_set$title)}")
    # the size line is already shown above; render the remaining rows (cost
    # range) verbatim from the shared block.
    cost_rows <- grep(
      "^95% confidence set size", blocks$confidence_set$rows,
      value = TRUE, invert = TRUE
    )
    for (row in cost_rows) cli::cli_text(row)
    cli::cli_text("First rows of the confidence set:")
    print(utils::head(x$cs))
  }

  # --- full: overall-test statistic + p-value (signif 4).
  if (!is.null(blocks$test)) {
    cli::cli_h3("{accent(blocks$test$title)}")
    for (row in blocks$test$rows) cli::cli_text(row)
  }

  invisible(x)
}

#' Print a LAGO optimization result
#'
#' @description Concise console display of the object returned by
#' [lago_optimization()]: the recommended intervention (component and value),
#' its cost, the estimated outcome, and, when available, the confidence-set size
#' and whether a power goal or overall test was used. Rendered with boxed,
#' colour-accented [cli][cli::cli] sections through the shared presentation
#' formatter so it never drifts from [summary.lago()] or the in-run summary.
#'
#' @param x A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @exportS3Method print lago
print.lago <- function(x, ...) {
  .lago_render(x, full = FALSE)
}

#' Summarize a LAGO optimization result
#'
#' @description Fuller display than [print.lago()]: adds the confidence-set cost
#' range and first rows, and the overall-test statistic and p-value when
#' present. Renders through the same shared formatter as [print.lago()].
#'
#' @param object A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary lago
summary.lago <- function(object, ...) {
  .lago_render(object, full = TRUE)
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
