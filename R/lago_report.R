#' Generate an HTML report for a LAGO optimization result
#'
#' @description Renders a self-contained, interactive HTML report for the object
#' returned by [lago_optimization()]. The report knits a bundled parameterized R
#' Markdown template. Its text sections use the same labels and the same
#' formatted lines as the console methods ([print.lago()] / [summary.lago()]),
#' but the report orders them differently and adds plots. It shows the inputs
#' recap, the recommended intervention, its cost, the estimated outcome (with
#' its confidence interval, 95\% by default) and goal, the power goal (when
#' present), the confidence set (size, cost IQR, and first rows, or the reason
#' there is no set), the overall intervention-effect test (when present), and
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
#'     highlighted) or a one-dimensional strip for a single component. The plot
#'     is drawn for one or two components only, as [plot.lago()] is. With three
#'     or more components, or when there is no confidence set, the plot section
#'     is left out, and the confidence set section still gives its size or the
#'     reason there is no set.
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
#' If it is an existing folder, the report is written in it as
#' `lago_report.html`. A file path must end in `.html` or `.htm`, since the
#' report is always an HTML file.
#' @param title A character string. The report title. Every character shows
#' as typed, so markdown, HTML and math are not interpreted. An empty or
#' missing title (`""`, `NA` or `NULL`) uses the default title.
#' Default value without user specification: "LAGO optimization report".
#' @param open A boolean. If `TRUE`, opens the rendered report in a browser via
#' [utils::browseURL()]. Default value without user specification: `FALSE`.
#' @param ... Additional arguments passed to [rmarkdown::render()], such as
#' `quiet = FALSE` to see the rendering, or `envir`. The input, the output
#' folder and the parameters are set by `lago_report()`, so `input`,
#' `output_dir` and `params` are refused. Use `output_file` and `title`
#' instead.
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

  template <- .lago_report_template()
  if (!nzchar(template) || !file.exists(template)) {
    cli::cli_abort(
      "Could not find the bundled report template {.file lago_report.Rmd}."
    )
  }

  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".html")
  }
  if (!is.character(output_file) || length(output_file) != 1 || is.na(output_file) || !nzchar(output_file)) {
    cli::cli_abort("{.arg output_file} must be one path, or {.code NULL} for a temporary file.")
  }
  # a folder gets the report inside it, not a file named after it next to it
  if (dir.exists(output_file)) output_file <- file.path(output_file, "lago_report.html")
  # The report is always HTML. Another extension would make pandoc fail (.pdf)
  # or write HTML into a file of another kind, such as a user's .Rmd file.
  if (!tolower(tools::file_ext(output_file)) %in% c("html", "htm")) {
    cli::cli_abort(c(
      "{.arg output_file} must end in {.file .html}, not {.file {basename(output_file)}}.",
      "i" = "{.fn lago_report} writes an HTML file only."
    ))
  }
  # A missing or empty title would leave the page with the name of a working
  # file as its title, so the default title is used instead.
  title <- as.character(title)
  title <- title[!is.na(title) & nzchar(trimws(title))]
  if (length(title) == 0) title <- "LAGO optimization report"
  output_file <- normalizePath(output_file, mustWork = FALSE)

  # quiet and envir have defaults the caller may change. The rest is how the
  # report is built, so passing it would clash with the call below.
  dots <- list(...)
  fixed <- intersect(names(dots), c("input", "output_dir", "params"))
  if (length(fixed) > 0) {
    cli::cli_abort(c(
      "{.fn lago_report} sets {.arg {fixed}} itself.",
      "i" = "Choose where the report goes with {.arg output_file}, and its title with {.arg title}."
    ))
  }
  render_args <- utils::modifyList(
    list(quiet = TRUE, envir = new.env(parent = globalenv())),
    dots
  )

  # render() writes its working files next to its input, so it renders a copy
  # of the template in a folder of its own. The installed package folder may be
  # read only, and two reports made at the same time must not share files.
  work <- tempfile("lago_report_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  input <- file.path(work, basename(template))
  file.copy(template, input)
  # split the requested path into directory + file for output_dir/output_file
  rendered <- do.call(rmarkdown::render, c(list(
    input = input,
    output_file = basename(output_file),
    output_dir = dirname(output_file),
    params = list(x = x, title = title)
  ), render_args))

  if (isTRUE(open)) {
    utils::browseURL(rendered)
  }

  invisible(rendered)
}

# The report title as the inside of a YAML double quoted string. Every ASCII
# punctuation character becomes a numeric HTML entity, such as &#40; for "(".
# pandoc reads an entity as the plain character, so the title shows literally
# in the heading and in the page title: no emphasis, links, raw HTML or math.
# A backslash escape is not enough, because pandoc reads \( and \[ as the start
# of math. The entities hold no backslash or double quote, so only line breaks
# need escaping for YAML.
.lago_report_title_yaml <- function(title) {
  title <- paste(as.character(title), collapse = " ")
  chars <- strsplit(title, "", fixed = TRUE)[[1]]
  punct <- grepl("^[!-/:-@\\[-`{-~]$", chars, perl = TRUE)
  chars[punct] <- paste0("&#", vapply(chars[punct], utf8ToInt, integer(1)), ";")
  gsub("\r?\n|\r", "\\\\n", paste(chars, collapse = ""))
}

# The bundled report template, in a function of its own so a test can point it
# at a copy.
.lago_report_template <- function() {
  system.file("rmarkdown", "lago_report.Rmd", package = "LAGOtrials")
}
