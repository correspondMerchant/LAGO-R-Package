#' Generate an HTML report for a LAGO optimization result
#'
#' @description Renders a self-contained, interactive HTML report for the object
#' returned by [lago_optimization()]. The report knits a bundled parameterized R
#' Markdown template and lays out the same sections, in the same order and with
#' the same labels, as the console methods ([print.lago()] / [summary.lago()]):
#' the recommended intervention, its cost, the estimated outcome (with its 95\%
#' CI) and goal, the power goal (when present), the confidence set (size, cost
#' IQR, and first rows), the overall intervention-effect test (when present), and
#' the fitted outcome-model coefficient table. A `sessionInfo()` footer records
#' the environment the report was produced in.
#'
#' No new statistics are computed; every value is taken verbatim from an
#' existing field on `x` and formatted exactly as the console output formats it.
#'
#' @details The report is interactive and drawn entirely client-side with the
#' bundled D3 v7 library (`inst/js/d3.v7.min.js`) plus `inst/js/lago-report.js`,
#' both inlined into the HTML so the output is a single self-contained,
#' offline file with no external asset references (no CDN, no Shiny, no
#' additional widget frameworks). It adds two interactive views:
#'
#' \itemize{
#'   \item an interactive confidence-set plot: a hover-enabled scatter for a
#'     two-component intervention (with the recommended intervention
#'     highlighted) or a one-dimensional strip for a single component. For 3+
#'     components, or when the interactive view is not applicable, it falls back
#'     to the static [plot.lago()] figure; when no confidence set is available
#'     the section is omitted.
#'   \item interactive cost curves: for each intervention component, its
#'     total-cost curve and marginal (per-unit) cost curve, each with a hover
#'     read-out, drawn from the result's cost functions and bounds.
#' }
#'
#' Rendering requires the \pkg{rmarkdown}, \pkg{knitr}, and \pkg{jsonlite}
#' packages (all listed under Suggests). If they are not installed, an
#' informative error is raised.
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
#' # lago_report() renders an HTML file with rmarkdown, so it is wrapped in
#' # \dontrun: it writes to disk and invokes pandoc, and running a document
#' # render inside an in-process documentation build (e.g. pkgdown) can deadlock.
#' \dontrun{
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
#'   confidence_set_grid_step_size = c(1, 1),
#'   quiet = TRUE
#' )
#' report_path <- lago_report(result)
#' }
#'
#' @family LAGO functions
#' @seealso [lago_optimization()]
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

  # rmarkdown + knitr render the template; jsonlite serializes the data the
  # interactive D3 plots read. All three are Suggests-only, so guard before
  # touching them.
  needed_pkgs <- c("rmarkdown", "knitr", "jsonlite")
  missing_pkgs <- needed_pkgs[
    !vapply(
      needed_pkgs,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  if (length(missing_pkgs) > 0) {
    # Build a copy-pasteable, valid install command with quoted package names.
    # Interpolating the bare vector inside {.code} would collapse to
    # "rmarkdown and knitr" (unquoted), which does not parse.
    install_hint <- paste0(
      "install.packages(c(",
      paste(encodeString(missing_pkgs, quote = "\""), collapse = ", "),
      "))"
    )
    cli::cli_abort(c(
      "{.fn lago_report} needs the {.pkg {missing_pkgs}} package{?s}.",
      "i" = "Install {?it/them} with {.code {install_hint}}."
    ))
  }

  template <- system.file(
    "rmarkdown", "lago_report.Rmd",
    package = "LAGOtrials"
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
