#' Run a LAGO optimization in a point and click app
#'
#' Opens a Shiny app in your web browser that runs the whole LAGO optimization
#' without writing R code. You load a dataset, choose the outcome and the goal,
#' choose the intervention components with their bounds and costs, and say how
#' the model accounts for centers and time. The app then finds the recommended
#' intervention with [lago_optimization()] and shows the result, the confidence
#' set and the R code that reproduces the run.
#'
#' The app runs on your own computer and is reachable only from it, at
#' `127.0.0.1`. Nothing is sent over the network.
#'
#' @details The app has these steps.
#'
#' * **Data:** the `data` you pass, the bundled `BB_data`, R's `mtcars`, or a
#'   CSV file you upload. Each example opens on a setup that runs as is.
#' * **Outcome and goal:** `outcome_name`, `outcome_type`, `outcome_goal`,
#'   `outcome_goal_intention`, and for a binary outcome the power goal:
#'   `power_goal`, `num_centers_in_next_stage`,
#'   `patients_per_center_in_next_stage`, `power_goal_approach`, `icc` and
#'   `power_goal_cluster_id`. The app builds the `group` column from the
#'   treatment arm column you choose.
#' * **Intervention and costs:** `intervention_components`, their bounds, and
#'   the costs as `unit_costs` with `default_cost_fxn_type` and
#'   `unit_cost_doses`, or as `cost_list_of_vectors`. The "Shape the costs
#'   visually" button opens the cost designer of [visualize_cost()] in a dialog,
#'   which starts on the costs in use and puts the shaped costs in as cost
#'   coefficients. Interaction terms set
#'   `include_interaction_terms` and `main_components`, and the app builds each
#'   product column.
#' * **Centers, time and covariates:** `center_characteristics` with their
#'   values, or fixed center effects with `include_center_effects` and
#'   `center_effects_optimization_values`, fixed time effects with
#'   `include_time_effects` and `time_effect_optimization_value`, and
#'   `additional_covariates`. The app builds the `center` and `period` columns
#'   from the columns you choose.
#'
#' A step that needs attention says what to change, and the run button stays
#' off until every step is ready. Warnings from the package are listed with
#' the result.
#'
#' @param data A data.frame to start with, or `NULL` to start with the
#'   bundled `BB_data` example.
#' @param launch.browser Whether to open the app in your web browser. By default
#'   it opens in an interactive session.
#' @param port The port to listen on, or `NULL` for a free one.
#'
#' @return Invisibly, when you click **Done** or close the browser tab, a list
#'   with the last `result` of [lago_optimization()] (or `NULL` if nothing was
#'   run), its R `code` as a character vector, and the `arguments` it was
#'   called with. The same list is stored in the `lago_app_result` option.
#'
#' @seealso [lago_optimization()] for the function the app runs, and
#'   [visualize_cost()] for the cost designer.
#'
#' @examples
#' if (interactive()) {
#'   out <- lago_app()
#'   out$result
#'   cat(out$code, sep = "\n")
#' }
#'
#' @export
lago_app <- function(data = NULL,
                     launch.browser = interactive(),
                     port = NULL) {
  data_name <- if (is.null(data)) NULL else deparse1(substitute(data))
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data.frame, or NULL to start with the BB_data example.", call. = FALSE)
  }
  app <- .build_lago_app(data = data, data_name = data_name)
  on.exit(try(shiny::removeResourcePath("lago_cost_assets"), silent = TRUE), add = TRUE)
  old <- options(shiny.maxRequestSize = 100 * 1024^2)
  on.exit(options(old), add = TRUE)
  out <- .lago_run_app(app, port = port, launch.browser = launch.browser)
  options(lago_app_result = out)
  invisible(out)
}

# Runs the app on the loopback interface only, so no other machine can reach
# it. There is no way to widen the bind.
.lago_run_app <- function(app, port = NULL, launch.browser = interactive()) {
  shiny::runApp(app, host = "127.0.0.1", port = port, launch.browser = launch.browser)
}

# A label with a "?" that opens a short explanation.
.app_help <- function(label, text) {
  htmltools::tags$span(
    label, " ",
    bslib::popover(
      htmltools::tags$span(
        class = "lago-help", tabindex = "0", role = "button",
        `aria-label` = paste("Help:", label), "?"
      ),
      text
    )
  )
}

.app_next <- function(id, label) {
  htmltools::div(class = "lago-next", shiny::actionButton(id, label, class = "btn-outline-primary btn-sm"))
}

.app_css <- "
.lago-help { display:inline-block; width:1.25em; height:1.25em; line-height:1.25em;
  border-radius:50%; background:#e7eef7; color:#1f4e79; text-align:center;
  font-size:.8em; font-weight:600; cursor:pointer; }
.lago-help:focus { outline:2px solid #1f4e79; }
.lago-next { margin-top:1rem; text-align:right; }
.lago-step { display:flex; gap:.5rem; align-items:flex-start; margin-bottom:.5rem; }
.lago-step .lago-mark { width:1.4em; flex:none; text-align:center; font-weight:700; }
.lago-step.ok .lago-mark { color:#1a7f37; }
.lago-step.todo .lago-mark { color:#b35900; }
.lago-step .msg { font-size:.85em; color:#555; }
.lago-stale { opacity:.55; }
.lago-comp { border:1px solid #dee2e6; border-radius:.5rem; padding:.6rem .8rem; margin-bottom:.6rem; }
.lago-comp h6 { margin:0 0 .4rem 0; }
.lago-intro { color:#444; }
.lago-notes li { white-space:pre-line; margin-bottom:.4rem; }
"

.build_lago_app <- function(data = NULL, data_name = NULL) {
  # the cost designer's D3 curves, served from the package, with no CDN
  shiny::addResourcePath("lago_cost_assets", system.file("js", package = "LAGOtrials"))
  sources <- c(
    "BetterBirth example (BB_data)" = "BB_data",
    "mtcars, an R example with a continuous outcome" = "mtcars",
    "Upload a CSV file" = "upload"
  )
  if (!is.null(data)) {
    sources <- c(stats::setNames("passed", sprintf("Your data (%s)", data_name)), sources)
  }

  ui <- bslib::page_sidebar(
    title = "LAGO trial optimizer",
    theme = bslib::bs_theme(version = 5),
    sidebar = bslib::sidebar(
      width = 330,
      shinyjs::useShinyjs(),
      htmltools::tags$head(
        htmltools::tags$style(htmltools::HTML(.app_css)),
        htmltools::tags$script(src = "lago_cost_assets/d3.v7.min.js"),
        htmltools::tags$script(src = "lago_cost_assets/cost-curves.js")
      ),
      htmltools::tags$strong("Ready to run"),
      shiny::uiOutput("checklist"),
      bslib::input_task_button("run", "Find recommendation", label_busy = "Running..."),
      shiny::uiOutput("run_hint"),
      htmltools::hr(),
      shiny::actionButton("reset", "Reset this dataset's settings", class = "btn-outline-secondary btn-sm"),
      shiny::actionButton("done", "Done: return to R", class = "btn-success"),
      htmltools::tags$small(class = "text-muted", "Done closes the app and returns the last result to R.")
    ),
    bslib::navset_card_tab(
      id = "steps",
      bslib::nav_panel(
        "1. Data",
        value = "data",
        htmltools::p(class = "lago-intro", "Choose the data from the first stage of the trial. Each example opens on a setup that runs as it is, so you can press Find recommendation right away."),
        shiny::radioButtons("source", .app_help("Data", "The first stage data, with one row per patient. The outcome and the components must be numeric columns."), choices = sources, selected = if (is.null(data)) "BB_data" else "passed"),
        shiny::conditionalPanel(
          "input.source == 'upload'",
          shiny::fileInput("file", .app_help("CSV file", "A text file with a header row and one row per patient. It stays on your computer."), accept = c(".csv", "text/csv")),
          bslib::layout_columns(
            shiny::selectInput("sep", .app_help("Separator", "The character between the values of a row. Many European files use a semicolon."), c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
            shiny::selectInput("dec", .app_help("Decimal mark", "The character before the decimals, such as the point in 2.5 or the comma in 2,5."), c("Point" = ".", "Comma" = ","))
          )
        ),
        shiny::uiOutput("data_info"),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel("Columns", shiny::tableOutput("data_columns")),
          bslib::accordion_panel("First rows", shiny::tableOutput("data_head"))
        ),
        .app_next("next_data", "Next: outcome and goal")
      ),
      bslib::nav_panel(
        "2. Outcome and goal",
        value = "outcome",
        htmltools::p(class = "lago-intro", "Say what the trial measures and what it should reach."),
        bslib::layout_columns(
          shiny::selectInput("outcome", .app_help("Outcome column", "The column that holds the outcome of each patient, or of each row."), choices = NULL),
          htmltools::div(
            shiny::radioButtons("outcome_type", .app_help("Outcome type", "Binary for an outcome that is 0 or 1 for each patient, such as whether a practice was followed. Continuous for a measured value."), c("Binary (0/1)" = "binary", "Continuous" = "continuous"), inline = TRUE),
            shiny::uiOutput("outcome_type_note")
          )
        ),
        shiny::radioButtons(
          "goal_mode", .app_help("Goal", "An outcome goal is the outcome level the next stage should reach. A power goal asks for enough statistical power in the next stage to show a benefit of the intervention, and works only for a binary outcome. With both, the higher of the two outcome levels is used."),
          c("Outcome goal" = "outcome", "Power goal" = "power", "Both (the higher one is used)" = "both"),
          inline = TRUE
        ),
        bslib::layout_columns(
          shiny::conditionalPanel(
            "input.goal_mode != 'power'",
            shiny::numericInput("goal", .app_help("Outcome goal", "The outcome level to reach. For a binary outcome it is a probability between 0 and 1."), value = 0.85, step = 0.01)
          ),
          shiny::radioButtons(
            "intention", .app_help("Direction", "Maximize when a higher outcome is better, such as adherence. Minimize when a lower outcome is better, such as a complication rate. The goal is the level to reach either way."),
            c("Maximize" = "maximize", "Minimize" = "minimize"), inline = TRUE
          )
        ),
        shiny::conditionalPanel(
          "input.goal_mode != 'outcome'",
          bslib::card(
            bslib::card_header("Power goal"),
            bslib::layout_columns(
              shiny::selectInput("arm_col", .app_help("Treatment arm column", "The column that says which patients got the intervention in the first stage."), choices = NULL),
              shiny::selectInput("arm_value", .app_help("Value that means treatment", "The value of the treatment arm column for patients who got the intervention."), choices = NULL)
            ),
            bslib::layout_columns(
              shiny::numericInput("power_goal", .app_help("Power goal", "The statistical power the next stage should have, such as 0.8."), value = 0.8, min = 0.05, max = 0.99, step = 0.05),
              shiny::numericInput("n_centers", .app_help("Centers in the next stage", "How many centers the next stage of the trial will have."), value = 10, min = 1, step = 1),
              shiny::numericInput("n_patients", .app_help("Patients per center", "How many patients each center of the next stage will enroll."), value = 30, min = 1, step = 1)
            ),
            bslib::accordion(
              open = FALSE,
              bslib::accordion_panel(
                "More power options",
                shiny::radioButtons("approach", .app_help("Approach", "Unconditional plans the power of the next stage alone. Conditional also counts the first stage data."), c("Unconditional" = "unconditional", "Conditional" = "conditional"), inline = TRUE),
                bslib::layout_columns(
                  shiny::textInput("icc", .app_help("ICC (optional)", "The intraclass correlation of the outcome within a center, such as 0.05. Give two values, such as 0.04, 0.06, for the control and the treatment arm. Leave it empty to ignore clustering."), value = ""),
                  shiny::selectInput("cluster_col", .app_help("Center column for the ICC", "The column that says which center each patient was in. Only columns with at least two centers that have an observed outcome in each arm are listed."), choices = NULL, selectize = FALSE)
                )
              )
            )
          )
        ),
        shiny::uiOutput("notes_outcome"),
        .app_next("next_outcome", "Next: intervention and costs")
      ),
      bslib::nav_panel(
        "3. Intervention and costs",
        value = "intervention",
        htmltools::p(class = "lago-intro", "Choose the parts of the intervention to optimize, the range each may take, and what each costs."),
        shiny::selectizeInput("components", .app_help("Intervention components", "The parts of the intervention whose levels LAGO recommends, such as the number of coaching visits."), choices = NULL, multiple = TRUE),
        shiny::radioButtons(
          "cost_mode", .app_help("Costs", "A cost per unit gives a total cost of unit cost times the level. Cost coefficients give any polynomial: c0 + c1 x + c2 x^2 and so on."),
          c("Cost per unit" = "unit", "Cost coefficients" = "coef"), inline = TRUE
        ),
        shiny::conditionalPanel(
          "input.cost_mode == 'unit'",
          shiny::radioButtons(
            "cost_type", .app_help("Cost shape", "Linear keeps the same cost for each unit. Cubic lets the cost per unit change with the level, with the unit cost as its average, or as its value at the dose where it was measured."),
            c("Linear" = "linear", "Cubic" = "cubic"), inline = TRUE
          )
        ),
        htmltools::div(
          class = "mb-3",
          shiny::actionButton("open_designer", "Shape the costs visually", icon = shiny::icon("chart-line"), class = "btn-outline-primary btn-sm"),
          " ",
          .app_help("", "Opens the cost designer, which draws the total and marginal cost of each component and lets you shape them with sliders or by dragging the curve. Use these costs then puts them in as cost coefficients.")
        ),
        shiny::uiOutput("component_rows"),
        shiny::uiOutput("notes_intervention"),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            "Interactions (optional)",
            htmltools::p("Model the product of two components when the effect of one depends on the other. The recommendation is still given for the components themselves."),
            shiny::uiOutput("interaction_ui")
          ),
          bslib::accordion_panel(
            "Confidence set",
            shiny::checkboxInput("include_cs", "Compute the confidence set (slower)", value = TRUE),
            htmltools::p(class = "text-muted", "The confidence set holds the interventions whose outcome may reach the goal, given the uncertainty of the first stage. The grid has about 20 steps across each range.")
          )
        ),
        .app_next("next_intervention", "Next: centers, time and covariates")
      ),
      bslib::nav_panel(
        "4. Centers, time and covariates",
        value = "centers",
        htmltools::p(class = "lago-intro", "Optional. Say how the model accounts for differences between centers and over time."),
        shiny::radioButtons(
          "center_mode", .app_help("Centers", "Center characteristics, such as size, are held at the values you set, so the recommendation is for a center like that. A fixed effect per center fits each center its own level, and the recommendation is for the average center or for the center you choose."),
          c("None" = "none", "By center characteristics" = "chars", "Fixed effect per center" = "effects"),
          inline = TRUE
        ),
        shiny::conditionalPanel(
          "input.center_mode == 'chars'",
          shiny::selectizeInput("center_chars", .app_help("Center characteristics", "Numeric columns that describe a center, such as its size. A column with two text values can be used after recoding it as 0 and 1."), choices = NULL, multiple = TRUE),
          shiny::uiOutput("center_char_values")
        ),
        shiny::conditionalPanel(
          "input.center_mode == 'effects'",
          bslib::layout_columns(
            shiny::selectInput("center_col", .app_help("Column that identifies the centers", "The column that says which center each patient was in."), choices = NULL, selectize = FALSE),
            shiny::selectInput("center_for", .app_help("Optimize for", "The weighted average center weighs each center by its number of patients. Or choose one center to plan for."), choices = NULL, selectize = FALSE)
          )
        ),
        shiny::checkboxInput("time_on", .app_help("Fixed time effects", "Fit each time period its own level, and recommend for the period you choose."), value = FALSE),
        shiny::conditionalPanel(
          "input.time_on",
          bslib::layout_columns(
            shiny::selectInput("period_col", .app_help("Column that holds the period", "The column that says in which time period each patient was seen."), choices = NULL, selectize = FALSE),
            shiny::selectInput("period_value", .app_help("Period to optimize for", "The period whose effect the recommendation assumes, often the latest one."), choices = NULL, selectize = FALSE)
          )
        ),
        shiny::uiOutput("notes_centers"),
        shiny::selectizeInput("covariates", .app_help("Additional covariates (optional)", "Other columns the outcome model adjusts for. When recommending, a numeric covariate is held at 0 and a factor at its first level."), choices = NULL, multiple = TRUE),
        .app_next("next_centers", "Find recommendation")
      ),
      bslib::nav_panel("Results", value = "results", shiny::uiOutput("results")),
      bslib::nav_panel(
        "R code",
        value = "code",
        htmltools::p(class = "lago-intro", "This code reproduces the current settings. Copy it into an R script to rerun or change the analysis."),
        shiny::verbatimTextOutput("code"),
        shiny::downloadButton("download_code", "Download .R file", class = "btn-outline-primary btn-sm")
      )
    )
  )

  # Shared by every browser session of this app: the last result, so a page
  # refresh keeps it, and the open sessions, so the app stops only when the
  # last tab is closed and not on a refresh.
  shared <- new.env(parent = emptyenv())
  shared$result <- list(result = NULL, code = NULL, arguments = NULL)
  shared$sessions <- 0L

  server <- function(input, output, session) {
    shared$sessions <- shared$sessions + 1L
    loaded <- shiny::reactiveVal(NULL)
    upload_error <- shiny::reactiveVal(NULL)
    run_state <- shiny::reactiveVal(NULL)
    # bumped on every load and reset, so the inputs of the component rows get
    # new ids and a value typed for another dataset is never reused
    generation <- shiny::reactiveVal(0L)
    last_outcome <- shiny::reactiveVal(NULL)
    # TRUE during the update cycle of a load or reset. The observers below then
    # leave every choice to apply_defaults(), as they would otherwise send the
    # old selections back. In the next cycle the browser has the new values.
    applying_defaults <- shiny::reactiveVal(FALSE)

    load_data <- function(source) {
      switch(source,
        passed = list(data = data, name = data_name),
        BB_data = list(data = as.data.frame(LAGOtrials::BB_data), name = "BB_data"),
        mtcars = list(data = datasets::mtcars, name = "mtcars"),
        upload = {
          f <- input$file
          if (is.null(f)) {
            NULL
          } else {
            d <- tryCatch(.app_read_csv(f$datapath, input$sep, input$dec), error = function(e) e)
            if (inherits(d, "error")) {
              list(error = sprintf(
                "The file could not be read with these settings (%s). Check that the separator and the decimal mark match the file.",
                conditionMessage(d)
              ))
            } else {
              list(
                data = d, name = .app_read_csv_code(f$name, input$sep, input$dec),
                hint = .app_csv_hint(d, input$sep, input$dec), renamed = .app_renamed(f$datapath, input$sep, d)
              )
            }
          }
        }
      )
    }

    apply_defaults <- function(d, name) {
      spec <- .app_defaults(d, name)
      generation(generation() + 1L)
      last_outcome(spec$outcome)
      applying_defaults(TRUE)
      session$onFlushed(function() applying_defaults(FALSE), once = TRUE)
      ch <- .app_choices(d, spec)
      shiny::updateSelectInput(session, "outcome", choices = ch$outcomes, selected = spec$outcome)
      shiny::updateRadioButtons(session, "outcome_type", selected = spec$outcome_type)
      shiny::updateNumericInput(session, "goal", value = spec$goal)
      shiny::updateRadioButtons(session, "goal_mode", selected = "outcome")
      shiny::updateRadioButtons(session, "intention", selected = "maximize")
      shiny::updateSelectizeInput(session, "components", choices = ch$components, selected = spec$components)
      shiny::updateRadioButtons(session, "cost_mode", selected = "unit")
      shiny::updateRadioButtons(session, "cost_type", selected = "linear")
      shiny::updateCheckboxInput(session, "include_cs", value = TRUE)
      shiny::updateCheckboxGroupInput(session, "interactions", selected = character(0))
      shiny::updateSelectInput(session, "arm_col", choices = ch$arms, selected = ch$arm)
      shiny::updateSelectInput(session, "arm_value", choices = ch$arm_values, selected = ch$arm_value)
      shiny::updateNumericInput(session, "power_goal", value = 0.8)
      shiny::updateNumericInput(session, "n_centers", value = 10)
      shiny::updateNumericInput(session, "n_patients", value = 30)
      shiny::updateRadioButtons(session, "approach", selected = "unconditional")
      shiny::updateTextInput(session, "icc", value = "")
      shiny::updateSelectInput(session, "cluster_col", choices = c("Choose a column" = "", ch$clusters), selected = "")
      shiny::updateRadioButtons(session, "center_mode", selected = "none")
      shiny::updateSelectizeInput(session, "center_chars", choices = ch$center_chars, selected = character(0))
      shiny::updateSelectInput(session, "center_col", choices = c("Choose a column" = "", ch$others), selected = ch$center_col)
      shiny::updateSelectInput(session, "center_for", choices = c("The weighted average center" = "", ch$centers), selected = "")
      shiny::updateCheckboxInput(session, "time_on", value = FALSE)
      shiny::updateSelectInput(session, "period_col", choices = c("Choose a column" = "", ch$others), selected = ch$period_col)
      shiny::updateSelectInput(session, "period_value", choices = ch$periods, selected = ch$period)
      shiny::updateSelectizeInput(session, "covariates", choices = ch$covariates, selected = character(0))
      designer_costs(list())
      run_state(NULL)
    }

    shiny::observeEvent(list(input$source, input$file, input$sep, input$dec), {
      got <- load_data(input$source)
      upload_error(got$error)
      if (!is.null(got$error)) got <- NULL
      loaded(got)
      if (!is.null(got)) apply_defaults(got$data, got$name)
    })
    shiny::observeEvent(input$reset, {
      got <- loaded()
      if (!is.null(got)) apply_defaults(got$data, got$name)
    })

    current_data <- shiny::reactive(loaded()$data)
    col_id <- function(prefix, col) paste0(prefix, "_", generation(), "_", match(col, names(current_data())))
    # costs applied from the cost designer, by component. The coefficient
    # inputs carry the designer version in their ids, so an apply redraws them
    # with the new values and keeps the bounds.
    designer_costs <- shiny::reactiveVal(list())
    designer_version <- shiny::reactiveVal(0L)
    designer_count <- 0L
    coef_id <- function(col) paste0(col_id("coef", col), "_", designer_version())
    designer_text <- function(col) {
      v <- designer_costs()[[col]]
      if (is.null(v)) NULL else paste(format_coef_code(v), collapse = ", ")
    }

    # Choices that depend on the outcome and the components.
    shiny::observeEvent(list(current_data(), input$outcome), {
      d <- current_data()
      shiny::req(d, input$outcome %in% names(d))
      if (shiny::isolate(applying_defaults())) {
        return()
      }
      numeric_cols <- .app_numeric_cols(d)
      auto <- if (.app_is_binary(d[[input$outcome]])) "binary" else "continuous"
      shiny::updateRadioButtons(session, "outcome_type", selected = auto)
      # a new outcome gets a goal on its own scale
      if (!identical(shiny::isolate(last_outcome()), input$outcome)) {
        last_outcome(input$outcome)
        shiny::updateNumericInput(session, "goal", value = .app_default_goal(d, input$outcome, auto))
      }
      kept <- intersect(shiny::isolate(input$components), setdiff(numeric_cols, input$outcome))
      shiny::updateSelectizeInput(session, "components", choices = setdiff(numeric_cols, input$outcome), selected = kept)
      two_level <- names(d)[vapply(d, function(v) length(unique(stats::na.omit(v))) == 2, logical(1))]
      arm_choices <- setdiff(two_level, input$outcome)
      arm_kept <- intersect(shiny::isolate(input$arm_col), arm_choices)
      shiny::updateSelectInput(session, "arm_col", choices = arm_choices, selected = if (length(arm_kept)) arm_kept else if ("group" %in% arm_choices) "group" else arm_choices[1])
      others <- setdiff(names(d), input$outcome)
      keep_col <- function(id, name) {
        kept <- intersect(shiny::isolate(input[[id]]), others)
        if (length(kept)) kept else if (name %in% others) name else ""
      }
      shiny::updateSelectInput(session, "center_col", choices = c("Choose a column" = "", others), selected = keep_col("center_col", "center"))
      shiny::updateSelectInput(session, "period_col", choices = c("Choose a column" = "", others), selected = keep_col("period_col", "period"))
    }, ignoreNULL = FALSE)

    output$outcome_type_note <- shiny::renderUI({
      d <- current_data()
      shiny::req(d, input$outcome %in% names(d))
      auto <- if (.app_is_binary(d[[input$outcome]])) "binary" else "continuous"
      htmltools::tags$small(class = "text-muted", if (identical(input$outcome_type, auto)) "Detected from the data." else "Set by you.")
    })

    shiny::observeEvent(list(current_data(), input$arm_col), {
      d <- current_data()
      if (shiny::isolate(applying_defaults())) {
        return()
      }
      shiny::req(d, input$arm_col %in% names(d))
      values <- sort(unique(as.character(stats::na.omit(d[[input$arm_col]]))))
      pick <- if ("treatment" %in% values) "treatment" else if ("1" %in% values) "1" else values[length(values)]
      shiny::updateSelectInput(session, "arm_value", choices = values, selected = pick)
    })

    shiny::observeEvent(list(current_data(), input$outcome, input$arm_col, input$arm_value, input$components), {
      d <- current_data()
      if (shiny::isolate(applying_defaults())) {
        return()
      }
      shiny::req(d)
      choices <- .app_cluster_choices(d, input$outcome, input$arm_col, input$arm_value, exclude = input$components)
      kept <- intersect(shiny::isolate(input$cluster_col), choices)
      shiny::updateSelectInput(session, "cluster_col", choices = c("Choose a column" = "", choices), selected = if (length(kept)) kept else "")
    }, ignoreNULL = FALSE)

    shiny::observeEvent(list(current_data(), input$components, input$outcome), {
      d <- current_data()
      if (shiny::isolate(applying_defaults())) {
        return()
      }
      shiny::req(d)
      numeric_cols <- .app_numeric_cols(d)
      chars <- setdiff(numeric_cols, c(input$outcome, input$components))
      shiny::updateSelectizeInput(session, "center_chars", choices = chars, selected = intersect(shiny::isolate(input$center_chars), chars))
      covs <- setdiff(names(d), c(input$outcome, input$components))
      shiny::updateSelectizeInput(session, "covariates", choices = covs, selected = intersect(shiny::isolate(input$covariates), covs))
    }, ignoreNULL = FALSE)

    shiny::observeEvent(list(current_data(), input$center_col), {
      d <- current_data()
      if (shiny::isolate(applying_defaults())) {
        return()
      }
      shiny::req(d)
      centers <- if (isTRUE(input$center_col %in% names(d))) sort(unique(as.character(stats::na.omit(d[[input$center_col]])))) else character(0)
      shiny::updateSelectInput(session, "center_for", choices = c("The weighted average center" = "", centers), selected = "")
    })

    shiny::observeEvent(list(current_data(), input$period_col), {
      d <- current_data()
      if (shiny::isolate(applying_defaults())) {
        return()
      }
      shiny::req(d)
      periods <- if (isTRUE(input$period_col %in% names(d))) .app_periods(d[[input$period_col]])$labels else character(0)
      shiny::updateSelectInput(session, "period_value", choices = periods, selected = if (length(periods)) periods[length(periods)] else character(0))
    })

    output$data_info <- shiny::renderUI({
      got <- loaded()
      if (!is.null(upload_error())) {
        return(htmltools::div(class = "alert alert-danger", role = "alert", upload_error()))
      }
      if (is.null(got)) {
        return(htmltools::p(class = "text-muted", "Choose a CSV file. It needs a header row, and numeric columns for the outcome and the components."))
      }
      d <- got$data
      htmltools::tagList(
        htmltools::p(sprintf("%d rows and %d columns, %d of them numeric.", nrow(d), ncol(d), length(.app_numeric_cols(d)))),
        if (!is.null(got$hint)) htmltools::div(class = "alert alert-warning", role = "status", got$hint),
        if (length(got$renamed)) {
          htmltools::div(
            class = "alert alert-info", role = "status",
            paste0("These column names were changed to names R can use: ", paste(got$renamed, collapse = ", "), ".")
          )
        }
      )
    })
    output$data_columns <- shiny::renderTable({
      d <- current_data()
      shiny::req(d)
      data.frame(
        Column = names(d),
        Type = vapply(d, function(v) class(v)[1], character(1)),
        Missing = vapply(d, function(v) sum(is.na(v)), integer(1)),
        Min = vapply(d, function(v) if (is.numeric(v) && any(!is.na(v))) format(min(v, na.rm = TRUE), digits = 4) else "", character(1)),
        Max = vapply(d, function(v) if (is.numeric(v) && any(!is.na(v))) format(max(v, na.rm = TRUE), digits = 4) else "", character(1))
      )
    })
    output$data_head <- shiny::renderTable(utils::head(current_data(), 8))

    # One row per component with its bounds and cost. A value the user already
    # typed is kept when the row is redrawn.
    output$component_rows <- shiny::renderUI({
      d <- current_data()
      shiny::req(d)
      comps <- intersect(input$components, names(d))
      if (!length(comps)) {
        return(htmltools::p(class = "text-muted", "Choose at least one component."))
      }
      cubic <- identical(input$cost_mode, "unit") && identical(input$cost_type, "cubic")
      coef_mode <- identical(input$cost_mode, "coef")
      keep <- function(id, default) {
        v <- shiny::isolate(input[[id]])
        if (is.null(v)) default else v
      }
      rows <- lapply(comps, function(cc) {
        rng <- range(d[[cc]], na.rm = TRUE)
        cost_ui <- if (coef_mode) {
          from_designer <- designer_text(cc)
          shiny::textInput(coef_id(cc), .app_help("Cost coefficients c0, c1, ...", "The total cost at level x is c0 + c1 x + c2 x^2 and so on. Enter the numbers separated by commas, such as 0, 2."), value = keep(coef_id(cc), if (is.null(from_designer)) "0, 1" else from_designer))
        } else {
          shiny::numericInput(col_id("cost", cc), .app_help("Unit cost", "The cost of one unit of this component, such as the cost of one coaching visit. The defaults of 1 are placeholders: enter your own costs."), value = keep(col_id("cost", cc), 1), min = 0)
        }
        htmltools::div(
          class = "lago-comp",
          htmltools::h6(cc),
          bslib::layout_columns(
            shiny::numericInput(col_id("lb", cc), .app_help("Lower bound", "The lowest level the next stage may use. It must be 0 or more."), value = keep(col_id("lb", cc), max(0, rng[1])), min = 0),
            shiny::numericInput(col_id("ub", cc), .app_help("Upper bound", "The highest level the next stage may use."), value = keep(col_id("ub", cc), rng[2])),
            cost_ui,
            if (cubic) shiny::numericInput(col_id("dose", cc), .app_help("Dose where the unit cost was measured (optional)", "With this dose the cubic cost equals the unit cost there. Without it, the unit cost is the average over the range."), value = keep(col_id("dose", cc), NA))
          ),
          htmltools::tags$small(class = "text-muted", sprintf("Observed in the data: %s to %s.", format(rng[1], digits = 4), format(rng[2], digits = 4)))
        )
      })
      htmltools::tagList(rows)
    })

    output$interaction_ui <- shiny::renderUI({
      comps <- input$components
      if (length(comps) < 2) {
        return(htmltools::p(class = "text-muted", "Choose at least two components to model an interaction."))
      }
      pairs <- utils::combn(comps, 2, simplify = FALSE)
      terms <- vapply(pairs, paste, character(1), collapse = ":")
      labels <- vapply(pairs, paste, character(1), collapse = " \u00d7 ")
      shiny::checkboxGroupInput("interactions", NULL, choices = stats::setNames(terms, labels), selected = intersect(shiny::isolate(input$interactions), terms))
    })

    output$center_char_values <- shiny::renderUI({
      d <- current_data()
      shiny::req(d)
      chars <- input$center_chars
      if (!length(chars)) {
        return(NULL)
      }
      do.call(bslib::layout_columns, lapply(chars, function(cc) {
        v <- shiny::isolate(input[[col_id("cc", cc)]])
        shiny::numericInput(col_id("cc", cc), sprintf("Optimize at %s =", cc), value = if (is.null(v)) signif(stats::median(d[[cc]], na.rm = TRUE), 4) else v)
      }))
    })

    # the inputs of a hidden step still have to exist, so the checks and the run
    # see them before the user opens that step
    for (id in c("component_rows", "interaction_ui", "center_char_values")) {
      shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
    }

    num <- function(x) {
      if (is.null(x) || !length(x)) NA_real_ else suppressWarnings(as.numeric(x[[1]]))
    }
    spec <- shiny::reactive({
      got <- loaded()
      shiny::req(got)
      d <- got$data
      s <- .app_empty_spec()
      # an input the browser has not sent yet keeps the default of the spec
      pick <- function(x, default) if (is.null(x) || !length(x)) default else x
      chr <- function(x) pick(x, "")
      comps <- intersect(pick(input$components, character(0)), names(d))
      s$data_name <- got$name
      s$outcome <- chr(input$outcome)
      s$outcome_type <- pick(input$outcome_type, s$outcome_type)
      s$goal_mode <- pick(input$goal_mode, s$goal_mode)
      s$goal <- num(input$goal)
      s$intention <- pick(input$intention, s$intention)
      s$components <- comps
      s$lower <- vapply(comps, function(cc) num(input[[col_id("lb", cc)]]), numeric(1), USE.NAMES = FALSE)
      s$upper <- vapply(comps, function(cc) num(input[[col_id("ub", cc)]]), numeric(1), USE.NAMES = FALSE)
      s$cost_mode <- pick(input$cost_mode, s$cost_mode)
      s$cost_type <- pick(input$cost_type, s$cost_type)
      s$unit_costs <- vapply(comps, function(cc) num(input[[col_id("cost", cc)]]), numeric(1), USE.NAMES = FALSE)
      s$doses <- vapply(comps, function(cc) num(input[[col_id("dose", cc)]]), numeric(1), USE.NAMES = FALSE)
      s$coefs <- lapply(comps, function(cc) {
        text <- input[[coef_id(cc)]]
        # until the redrawn input reports, the designer's costs hold
        if (is.null(text)) text <- designer_text(cc)
        .app_parse_coefs(text)
      })
      pairs <- if (length(comps) >= 2) vapply(utils::combn(comps, 2, simplify = FALSE), paste, character(1), collapse = ":") else character(0)
      s$interactions <- intersect(pick(input$interactions, character(0)), pairs)
      s$include_cs <- !isFALSE(input$include_cs)
      s$center_mode <- pick(input$center_mode, s$center_mode)
      s$center_chars <- intersect(pick(input$center_chars, character(0)), names(d))
      s$center_char_values <- vapply(s$center_chars, function(cc) num(input[[col_id("cc", cc)]]), numeric(1), USE.NAMES = FALSE)
      s$center_col <- chr(input$center_col)
      s$center_for <- chr(input$center_for)
      s$time_on <- isTRUE(input$time_on)
      s$period_col <- chr(input$period_col)
      s$period_value <- chr(input$period_value)
      s$covariates <- intersect(pick(input$covariates, character(0)), names(d))
      s$arm_col <- chr(input$arm_col)
      s$arm_value <- chr(input$arm_value)
      s$power_goal <- num(input$power_goal)
      s$n_centers <- num(input$n_centers)
      s$n_patients <- num(input$n_patients)
      s$approach <- pick(input$approach, s$approach)
      s$icc <- .app_parse_icc(input$icc)
      s$cluster_col <- chr(input$cluster_col)
      s
    })

    note_box <- function(msgs) {
      if (!length(msgs)) {
        return(NULL)
      }
      htmltools::div(class = "alert alert-info", role = "status", htmltools::tags$ul(class = "mb-0", lapply(msgs, htmltools::tags$li)))
    }
    notes <- shiny::reactive({
      got <- loaded()
      if (is.null(got)) character(0) else .app_notes(spec(), got$data)
    })
    output$notes_outcome <- shiny::renderUI(note_box(grep("named group", notes(), value = TRUE)))
    output$notes_centers <- shiny::renderUI(note_box(grep("named group", notes(), value = TRUE, invert = TRUE)))
    output$notes_intervention <- shiny::renderUI({
      s <- spec()
      if (identical(s$cost_mode, "unit") && length(s$unit_costs) && all(s$unit_costs %in% 1)) {
        note_box("The unit costs are still the placeholder of 1. Enter the real cost of one unit of each component to get a cost you can use.")
      }
    })

    problems <- shiny::reactive({
      if (is.null(loaded())) {
        msg <- if (is.null(upload_error())) "Choose a CSV file to upload." else "The file could not be read. Step 1 says what to change."
        return(data.frame(step = "data", message = msg))
      }
      .app_problems(spec(), current_data())
    })

    steps <- c(data = "1. Data", outcome = "2. Outcome and goal", intervention = "3. Intervention and costs", centers = "4. Centers, time and covariates")
    output$checklist <- shiny::renderUI({
      p <- problems()
      htmltools::tagList(lapply(names(steps), function(st) {
        msgs <- p$message[p$step == st]
        if (is.null(loaded()) && st != "data") msgs <- "Load the data first."
        ok <- !length(msgs)
        htmltools::div(
          class = paste("lago-step", if (ok) "ok" else "todo"),
          htmltools::span(class = "lago-mark", `aria-hidden` = "true", if (ok) "\u2713" else "!"),
          htmltools::span(class = "visually-hidden", if (ok) "Ready:" else "Needs attention:"),
          htmltools::div(
            shiny::actionLink(paste0("go_", st), steps[[st]]),
            if (!ok) htmltools::div(class = "msg", msgs[1], if (length(msgs) > 1) sprintf(" (and %d more)", length(msgs) - 1))
          )
        )
      }))
    })
    for (st in names(steps)) {
      local({
        target <- st
        shiny::observeEvent(input[[paste0("go_", target)]], bslib::nav_select("steps", target))
      })
    }
    # The cost designer in a dialog, started on the costs now in use. It runs as
    # a module under a new id each time, so no value carries over from an
    # earlier opening.
    shiny::observeEvent(input$open_designer, {
      s <- spec()
      comps <- s$components
      ok <- length(comps) && all(is.finite(c(s$lower, s$upper))) && all(s$lower < s$upper)
      if (!ok) {
        shiny::showNotification("Choose the components and give each a lower bound below its upper bound first.", type = "warning")
        return()
      }
      start <- .app_designer_start(s)
      designer_count <<- designer_count + 1L
      id <- paste0("designer_", designer_count)
      parts <- .build_cost_parts(
        component_names = comps,
        unit_costs = start$unit_costs,
        default_cost_fxn_type = start$form,
        intervention_lower_bounds = s$lower,
        intervention_upper_bounds = s$upper,
        ns = shiny::NS(id),
        embedded = TRUE,
        initial_coefs = start$coefs,
        on_apply = function(cost_list, form) {
          designer_costs(stats::setNames(cost_list, comps))
          shiny::updateRadioButtons(session, "cost_mode", selected = "coef")
          designer_version(designer_version() + 1L)
          shiny::removeModal(session)
          shiny::showNotification("The costs from the designer are now in use, as cost coefficients.", type = "message")
        }
      )
      shiny::showModal(shiny::modalDialog(
        title = "Shape the costs",
        parts$ui,
        size = "xl", easyClose = TRUE,
        footer = shiny::modalButton("Cancel")
      ), session = session)
      shiny::moduleServer(id, parts$server)
    })

    shiny::observeEvent(input$next_data, bslib::nav_select("steps", "outcome"))
    shiny::observeEvent(input$next_outcome, bslib::nav_select("steps", "intervention"))
    shiny::observeEvent(input$next_intervention, bslib::nav_select("steps", "centers"))

    shiny::observe({
      shinyjs::toggleState("run", condition = !nrow(problems()))
      shinyjs::toggleState("next_centers", condition = !nrow(problems()))
    })
    output$run_hint <- shiny::renderUI({
      p <- problems()
      if (!nrow(p)) {
        return(NULL)
      }
      htmltools::tags$small(class = "text-muted", role = "status", "Fix the steps marked ! to run.")
    })

    built <- shiny::reactive({
      shiny::req(!nrow(problems()))
      .app_build(spec(), current_data())
    })

    do_run <- function() {
      b <- built()
      shiny::withProgress(message = "Finding the recommended intervention", value = 0.3, {
        res <- .app_run(function() do.call(lago_optimization, c(list(data = b$data), b$args, list(quiet = TRUE))))
        shiny::setProgress(1)
      })
      run_state(list(run = res, build = b))
      if (is.null(res$error)) {
        shared$result <- list(result = res$value, code = b$code, arguments = b$args)
      }
      bslib::nav_select("steps", "results")
    }
    shiny::observeEvent(input$run, do_run())
    shiny::observeEvent(input$next_centers, do_run())

    stale <- shiny::reactive({
      st <- run_state()
      if (is.null(st)) {
        return(FALSE)
      }
      now <- tryCatch(built(), error = function(e) NULL)
      is.null(now) || !identical(now$args, st$build$args) || !identical(now$prep, st$build$prep) ||
        !identical(now$code[2], st$build$code[2])
    })

    output$results <- shiny::renderUI({
      st <- run_state()
      if (is.null(st)) {
        return(htmltools::p(class = "lago-intro", "Press Find recommendation to see the result here."))
      }
      run <- st$run
      notes <- c(run$warnings, run$messages)
      notes_card <- if (length(notes)) {
        bslib::card(
          bslib::card_header(sprintf("Things to check (%d)", length(notes))),
          htmltools::tags$ul(class = "lago-notes", lapply(notes, htmltools::tags$li))
        )
      }
      if (!is.null(run$error)) {
        return(htmltools::tagList(
          bslib::card(
            class = "border-danger",
            bslib::card_header("The optimization could not run with these settings"),
            htmltools::p(run$error),
            htmltools::p("Things that often help:"),
            htmltools::tags$ul(lapply(.app_error_tips(), htmltools::tags$li)),
            htmltools::p(class = "text-muted", "Your settings are kept. Change them and run again.")
          ),
          notes_card
        ))
      }
      r <- run$value
      comps <- r$display_components
      if (is.null(comps)) comps <- r$intervention_components
      fmt <- function(v) vapply(.lago_fmt_value(v), format, character(1), big.mark = ",")
      rec <- paste(sprintf("%s = %s", comps, fmt(r$rec_int)), collapse = ", ")
      goal_used <- if (!is.null(r$effective_outcome_goal)) r$effective_outcome_goal else r$outcome_goal
      ci <- r$est_outcome_ci
      htmltools::div(
        class = if (stale()) "lago-stale",
        if (stale()) {
          htmltools::div(class = "alert alert-warning", role = "status", "These results are from earlier settings. Press Find recommendation to update them.")
        },
        htmltools::p(class = "lead", .app_summary_sentence(r, comps)),
        if (isTRUE(r$shrinking_method_used)) {
          htmltools::div(
            class = "alert alert-info",
            htmltools::p("The goal cannot be reached within the bounds, so the recommendation was found with the shrinking method. It moves from the first stage intervention toward the bounds by how far the goal is from reach. Lower the goal or widen the bounds to get a recommendation that reaches it."),
            shiny::actionButton("fix_goal", "Lower the goal", class = "btn-outline-primary btn-sm"),
            " ",
            shiny::actionButton("fix_bounds", "Widen the bounds", class = "btn-outline-primary btn-sm")
          )
        },
        bslib::layout_column_wrap(
          width = 1 / 3,
          bslib::value_box("Recommended intervention", htmltools::span(style = "font-size:1.1rem", rec)),
          bslib::value_box("Cost of the recommendation", fmt(r$rec_int_cost)),
          bslib::value_box(
            "Estimated outcome",
            fmt(r$est_outcome_goal),
            if (length(ci) == 2 && all(is.finite(ci))) htmltools::p(sprintf("95%% CI %s to %s", fmt(ci[1]), fmt(ci[2]))),
            if (!is.null(goal_used)) htmltools::p(sprintf("Goal used: %s%s", fmt(goal_used), .app_goal_source(r)))
          )
        ),
        notes_card,
        bslib::navset_card_underline(
          bslib::nav_panel(
            "Confidence set",
            shiny::uiOutput("cs_ui")
          ),
          bslib::nav_panel("Outcome model", shiny::uiOutput("model_ui")),
          if (!is.null(r$test_results)) bslib::nav_panel("Intervention test", shiny::uiOutput("test_ui")),
          bslib::nav_panel("Full printout", shiny::verbatimTextOutput("printout"))
        )
      )
    })

    output$cs_ui <- shiny::renderUI({
      st <- run_state()
      r <- st$run$value
      shiny::req(r)
      if (is.null(r$cs)) {
        why <- if (isTRUE(r$shrinking_method_used)) {
          "There is no confidence set because the shrinking method was used."
        } else if (is.null(r$confidence_set_size_percentage)) {
          "The confidence set was not computed. Turn it on in step 3."
        } else {
          "No intervention in the grid is in the confidence set."
        }
        return(htmltools::p(why))
      }
      htmltools::tagList(
        htmltools::p(sprintf("%s of the grid is in the confidence set.", paste0(format(100 * r$confidence_set_size_percentage, digits = 3), "%"))),
        if (length(r$rec_int) <= 2) {
          shiny::plotOutput("cs_plot", height = "420px")
        } else {
          htmltools::tagList(
            htmltools::p(if (nrow(r$cs) > 200) {
              sprintf("With three or more components the set is listed as a table. The 200 cheapest of its %s interventions are shown. Download the file for all of them.", format(nrow(r$cs), big.mark = ","))
            } else {
              "With three or more components the set is listed as a table, cheapest first."
            }),
            shiny::tableOutput("cs_table")
          )
        },
        shiny::downloadButton("download_cs", "Download the confidence set (.csv)", class = "btn-outline-primary btn-sm")
      )
    })
    output$cs_plot <- shiny::renderPlot({
      r <- run_state()$run$value
      shiny::req(r, r$cs)
      plot(r)
    })
    output$cs_table <- shiny::renderTable({
      cs <- run_state()$run$value$cs
      shiny::req(cs)
      if ("cost" %in% names(cs)) cs <- cs[order(cs$cost), , drop = FALSE]
      utils::head(cs, 200)
    })
    output$model_ui <- shiny::renderUI({
      r <- run_state()$run$value
      shiny::req(r, r$model)
      htmltools::tagList(
        htmltools::p(sprintf("The %s model fitted to the first stage data, with the %s link.", r$family, r$link)),
        shiny::tableOutput("model_table")
      )
    })
    output$model_table <- shiny::renderTable({
      r <- run_state()$run$value
      shiny::req(r, r$model)
      co <- stats::coef(summary(r$model))
      data.frame(Term = rownames(co), signif(as.data.frame(co), 4), check.names = FALSE, row.names = NULL)
    }, digits = 4)
    output$test_ui <- shiny::renderUI({
      r <- run_state()$run$value
      shiny::req(r)
      b <- lago_blocks(r)$test
      shiny::req(b)
      htmltools::tagList(
        htmltools::p("Whether the first stage shows an effect of the intervention as a whole."),
        htmltools::tags$ul(lapply(b$rows, htmltools::tags$li))
      )
    })
    shiny::observeEvent(input$fix_goal, bslib::nav_select("steps", "outcome"))
    shiny::observeEvent(input$fix_bounds, bslib::nav_select("steps", "intervention"))
    output$download_cs <- shiny::downloadHandler(
      filename = function() "lago_confidence_set.csv",
      content = function(file) utils::write.csv(run_state()$run$value$cs, file, row.names = FALSE)
    )
    output$printout <- shiny::renderText({
      r <- run_state()$run$value
      shiny::req(r)
      paste(.app_printout(r), collapse = "\n")
    })

    code_lines <- shiny::reactive({
      b <- tryCatch(built(), error = function(e) NULL)
      if (is.null(b)) "# Fix the steps marked ! to see the code." else b$code
    })
    output$code <- shiny::renderText(paste(code_lines(), collapse = "\n"))
    output$download_code <- shiny::downloadHandler(
      filename = function() "lago_analysis.R",
      content = function(file) writeLines(code_lines(), file)
    )

    shiny::observeEvent(input$done, shiny::stopApp(shared$result))
    # closing the last tab ends the app too, after a pause so that a page
    # refresh, which opens a new session, does not
    session$onSessionEnded(function() {
      shared$sessions <- shared$sessions - 1L
      if (!shiny::isRunning()) {
        return()
      }
      later::later(function() {
        if (shared$sessions == 0L && shiny::isRunning()) shiny::stopApp(shared$result)
      }, delay = 5)
    })
  }

  shiny::shinyApp(ui, server)
}
