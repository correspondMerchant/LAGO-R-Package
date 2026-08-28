# Null-coalescing helper (base R gained %||% only in 4.4.0; DESCRIPTION allows
# older R, so define a local one).
`%||%` <- function(a, b) if (is.null(a)) b else a

# LAGO brand accent, matched to the hex logo blue also used in plot.lago
# (#0066cc). `make_ansi_style` degrades to identity on terminals without
# colour support, so this is safe everywhere.
.lago_accent <- function() cli::make_ansi_style("#0066cc")

# Shared console renderer for a `lago` object. print.lago and summary.lago both
# render through this so their output can never drift, and the non-quiet in-run
# summary in lago_optimization() calls print() on the assembled result, giving
# byte-identical output. It shows the FULL picture (inputs recap, model fit and
# coefficient table, overall test, recommendation with the estimated-outcome CI,
# and the confidence set) so R users can read everything straight from the
# console. This layer performs NO statistics: every quantity comes from
# lago_blocks(x) or verbatim from an existing field, with the same wording and
# rounding the package has always used. `full` currently renders identically to
# the default; it is kept so summary.lago() can extend the output later without
# changing the print.lago() contract.
.lago_render <- function(x, full = FALSE) {
  blocks <- lago_blocks(x)
  accent <- .lago_accent()

  cli::cli_h1("{accent('LAGO optimization result')}")

  # --- inputs recap (echoes the key user inputs).
  if (!is.null(blocks$inputs)) {
    cli::cli_h3("{accent(blocks$inputs$title)}")
    for (row in blocks$inputs$rows) cli::cli_text(row)
  }

  # --- outcome model fit: family/link/effects appear in the inputs recap; here
  # we print the fitted-model coefficient table the way the old output did
  # (print(summary(model))). Carried on the object as $model.
  if (!is.null(x$model)) {
    cli::cli_h3("{accent('Outcome model fit')}")
    print(summary(x$model))
  }

  # --- overall intervention-effect test (or the guidance shown when no valid
  # 'group' column was supplied, matching the old output).
  cli::cli_h3("{accent('Overall intervention-effect test')}")
  if (!is.null(blocks$test)) {
    for (row in blocks$test$rows) cli::cli_text(row)
  } else {
    cli::cli_text(
      paste(
        "To see the overall test results, include a 'group' column in the",
        "data with values 'treatment' or 'control' (binary outcomes only)."
      )
    )
  }

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
  for (row in blocks$cost$rows) cli::cli_text(row)
  for (row in blocks$outcome$rows) cli::cli_text(row)

  # --- confidence set: size, cost IQR, and first rows. The size line is shown
  # here (gated on the field) so it still appears when the field is present but
  # no confidence set was found (cs NULL).
  cli::cli_h3("{accent('Confidence set')}")
  if (is.null(x$confidence_set_size_percentage)) {
    # confidence set was not requested (include_confidence_set = FALSE).
    cli::cli_text(
      "Not computed (set include_confidence_set = TRUE to compute it)."
    )
  } else {
    cli::cli_text(
      "95% confidence set size: {round(100 * x$confidence_set_size_percentage, 2)}% of the grid"
    )
    if (!is.null(blocks$confidence_set) && !is.null(x$cs)) {
      # render the non-size rows (cost IQR) verbatim from the shared block.
      iqr_rows <- grep(
        "^95% confidence set size", blocks$confidence_set$rows,
        value = TRUE, invert = TRUE
      )
      for (row in iqr_rows) cli::cli_text(row)
      cli::cli_text("First rows of the confidence set (use $cs for all):")
      print(utils::head(x$cs))
    } else {
      # size field present but no set found for the current goal.
      cli::cli_text("No confidence set was found for the current outcome goal.")
    }
  }

  invisible(x)
}

#' Print a LAGO optimization result
#'
#' @description Full console display of the object returned by
#' [lago_optimization()], rendered with boxed, colour-accented [cli][cli::cli]
#' sections: an inputs recap (data dimensions, outcome, intervention components,
#' model family/link and fixed effects, goals, costs and bounds), the fitted
#' outcome-model coefficient table, the overall intervention-effect test, the
#' recommended intervention with its cost and the estimated outcome (and its
#' 95\% confidence interval), and the confidence set (size, cost IQR, and first
#' rows). Everything is shown on the console so results can be read without
#' further calls. [summary.lago()] renders the same output.
#'
#' @param x A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' # lago_optimization() already prints the result, so quiet = TRUE avoids
#' # rendering it twice here. The lower bounds start at 1 while the data also
#' # contains 0s, so the optimizer warns about that; the warning is expected.
#' result <- lago_optimization(
#'   data = BB_data,
#'   outcome_name = "pp3_oxytocin_mother",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   intervention_components = c("coaching_updt", "launch_duration"),
#'   center_characteristics = c("birth_volume_100"),
#'   center_characteristics_optimization_values = 1.75,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
#'   outcome_goal = 0.85,
#'   outcome_goal_intention = "maximize",
#'   include_confidence_set = TRUE,
#'   confidence_set_grid_step_size = c(1, 1),
#'   quiet = TRUE
#' )
#'
#' print(result)
#'
#' @exportS3Method print lago
print.lago <- function(x, ...) {
  .lago_render(x, full = FALSE)
}

#' Summarize a LAGO optimization result
#'
#' @description Renders the same full console display as [print.lago()]: the
#' inputs recap, outcome-model coefficient table, overall intervention-effect
#' test, recommended intervention (with cost and the estimated-outcome CI), and
#' the confidence set. Provided so `summary()` works as expected on a "lago"
#' object.
#'
#' @param object A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return `object`, invisibly.
#'
#' @examples
#' # summary() currently renders exactly the same output as print(). The lower
#' # bounds start at 1 while the data also contains 0s, so the optimizer warns
#' # about that; the warning is expected here.
#' result <- lago_optimization(
#'   data = BB_data,
#'   outcome_name = "pp3_oxytocin_mother",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   intervention_components = c("coaching_updt", "launch_duration"),
#'   center_characteristics = c("birth_volume_100"),
#'   center_characteristics_optimization_values = 1.75,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
#'   outcome_goal = 0.85,
#'   outcome_goal_intention = "maximize",
#'   include_confidence_set = TRUE,
#'   confidence_set_grid_step_size = c(1, 1),
#'   quiet = TRUE
#' )
#'
#' summary(result)
#'
#' @exportS3Method summary lago
summary.lago <- function(object, ...) {
  .lago_render(object, full = TRUE)
}

#' Plot a LAGO optimization result
#'
#' @description Visualizes the 95\% confidence set. For a two-component
#' intervention it plots the grid points in the confidence set with the
#' recommended intervention highlighted; for a single component it plots the
#' confidence interval bounds against the dose. A non-empty confidence set is
#' required. `result$cs` can be NULL even with `include_confidence_set = TRUE`
#' (its default), when no confidence set was found for the outcome goal or the
#' shrinking method was used, and then plot() returns invisibly with a message
#' rather than erroring.
#'
#' @param x A "lago" object returned by [lago_optimization()].
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly) when a plot is produced, otherwise `NULL`
#' invisibly.
#'
#' @examples
#' # A plot needs a non-empty confidence set: plot() returns invisibly with a
#' # message when result$cs is NULL, which can happen even with
#' # include_confidence_set = TRUE (its default) if no confidence set was found
#' # for the outcome goal, or if the shrinking method was used.
#' # The lower bounds start at 1 while the data also contains 0s, so the
#' # optimizer warns about that; the warning is expected here.
#' result <- lago_optimization(
#'   data = BB_data,
#'   outcome_name = "pp3_oxytocin_mother",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   intervention_components = c("coaching_updt", "launch_duration"),
#'   center_characteristics = c("birth_volume_100"),
#'   center_characteristics_optimization_values = 1.75,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   cost_list_of_vectors = list(c(0, 1700), c(0, 8000)),
#'   outcome_goal = 0.85,
#'   outcome_goal_intention = "maximize",
#'   include_confidence_set = TRUE,
#'   confidence_set_grid_step_size = c(1, 1),
#'   quiet = TRUE
#' )
#'
#' # Two components: the confidence set grid with the recommended
#' # intervention marked.
#' plot(result)
#'
#' @import ggplot2
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
