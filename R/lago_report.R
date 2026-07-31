#' Generate an HTML report for a LAGO optimization result
#'
#' @description Renders a self-contained HTML report for the object returned by
#' [lago_optimization()]. The report knits a bundled parameterized R Markdown
#' template and lays out the same sections, in the same order and with the same
#' labels, as the console methods ([print.lago()] / [summary.lago()]): the
#' recommended intervention, its cost, the estimated outcome and goal, the power
#' goal (when present), the confidence set (size, cost range, and first rows),
#' the overall intervention-effect test (when present), and the confidence-set
#' plot from [plot.lago()] (when a confidence set is available). A
#' `sessionInfo()` footer records the environment the report was produced in.
#'
#' No new statistics are computed; every value is taken verbatim from an
#' existing field on `x` and formatted exactly as the console output formats it.
#'
#' Rendering requires the \pkg{rmarkdown} and \pkg{knitr} packages (listed under
#' Suggests). If they are not installed, an informative error is raised.
#'
#' @param x A "lago" object returned by [lago_optimization()].
#' @param output_file A character string. The path to write the HTML report to.
#' If `NULL` (the default), a temporary file with a `.html` extension is used.
#' @param title A character string. The report title.
#' Default value without user specification: "LAGO optimization report".
#' @param open A boolean. If `TRUE`, opens the rendered report in a browser via
#' [utils::browseURL()]. Default value without user specification: `FALSE`.
#' @param ... Additional arguments passed to [rmarkdown::render()].
#'
#' @return The path to the rendered HTML file, invisibly.
#'
#' @examples
#' \donttest{
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
#'   cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
#'   outcome_goal = 0.85,
#'   outcome_goal_intention = "maximize",
#'   confidence_set_grid_step_size = c(1, 1),
#'   quiet = TRUE
#' )
#' report_path <- lago_report(result)
#' }
#'
#' @export
lago_report <- function(x,
                        output_file = NULL,
                        title = "LAGO optimization report",
                        open = FALSE,
                        ...) {
  if (!inherits(x, "lago")) {
    cli::cli_abort(c(
      "{.arg x} must be a {.cls lago} object.",
      "i" = "Pass the value returned by {.fn lago_optimization}."
    ))
  }

  # rmarkdown + knitr are Suggests-only, so guard before touching them.
  missing_pkgs <- c("rmarkdown", "knitr")[
    !vapply(
      c("rmarkdown", "knitr"),
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  if (length(missing_pkgs) > 0) {
    cli::cli_abort(c(
      "{.fn lago_report} needs the {.pkg {missing_pkgs}} package{?s}.",
      "i" = "Install {?it/them} with {.code install.packages(c({missing_pkgs}))}."
    ))
  }

  template <- system.file(
    "rmarkdown", "lago_report.Rmd",
    package = "LAGO"
  )
  if (!nzchar(template) || !file.exists(template)) {
    cli::cli_abort(
      "Could not find the bundled report template {.file lago_report.Rmd}."
    )
  }

  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".html")
  }
  output_file <- normalizePath(output_file, mustWork = FALSE)

  # render() renders in place next to the template unless we point it away, so
  # split the requested path into directory + file for output_dir/output_file.
  rendered <- rmarkdown::render(
    input = template,
    output_file = basename(output_file),
    output_dir = dirname(output_file),
    params = list(x = x, title = title),
    quiet = TRUE,
    envir = new.env(parent = globalenv()),
    ...
  )

  if (isTRUE(open)) {
    utils::browseURL(rendered)
  }

  invisible(rendered)
}
