#' Visualize and choose intervention cost functions
#'
#' @description Visualize the cost functions for the intervention components.
#' This function creates a Shiny app that allows the user to adjust the
#' coefficients of the cost functions for each intervention component and
#' visualize the resulting total cost function and its derivative.
#' The initial coefficients are calculated based on the unit costs, the
#' cost function type (linear or cubic), and the lower and upper bounds. A
#' "Cost function form" toggle at the top switches all components between the
#' linear (2 coefficients) and cubic (a degree-4 total cost, 5 coefficients)
#' forms, resetting each to that form's initial coefficients.
#' The user can adjust the coefficients using sliders and reset them to their
#' initial values. Each slider has a default range scaled to its coefficient's
#' magnitude (derived from the unit costs and bounds), and the user can set a
#' custom range for any slider using its "Range min" / "Range max" inputs.
#' The total cost curve can also be reshaped directly: it carries a draggable
#' handle at each of several points along the curve, and dragging one refits the
#' cost function through the new set of points and updates the sliders.
#' The app also displays the current coefficient vector for each component.
#' The user can copy the final coefficient list for use in the optimization
#' function lago_optimization().
#'
#' @param component_names A character vector of the names of the
#' intervention components.
#' @param unit_costs A numeric vector of the unit costs for each
#' intervention component.
#' @param default_cost_fxn_type A character string specifying the cost function
#' type the app opens with. Must be either "linear" or "cubic". The user can
#' switch forms in the app with the "Cost function form" toggle.
#' @param intervention_lower_bounds A numeric vector of the lower bounds for
#' each intervention component.
#' @param intervention_upper_bounds A numeric vector of the upper bounds for
#' each intervention component.
#'
#' @family LAGO functions
#' @seealso [lago_optimization()]
#' @export
#' @import bslib shiny
#' @importFrom shinyjs useShinyjs show hide runjs
#'
#' @examples
#' \dontrun{
#' visualize_cost(
#'   component_names = c("Component 1", "Component 2"),
#'   unit_costs = c(0.5, 1),
#'   default_cost_fxn_type = "linear",
#'   intervention_lower_bounds = c(0, 0),
#'   intervention_upper_bounds = c(10, 10)
#' )
#' }
#' @return Invisibly, the cost-function coefficient list
#' (a list of numeric vectors, one per intervention component) as it stood
#' when the app was closed, suitable for passing to
#' \code{lago_optimization(cost_list_of_vectors = ...)}. The same list can be
#' copied from within the app.
#'
#' @details When the app is closed with the "Return list to R & close" button,
#' the coefficient list is also stored in the \code{lago_cost_list} option
#' (overwriting any existing value) and a message reports this, so the list is
#' available even when the app was launched with a bare
#' \code{visualize_cost(...)} call rather than \code{cost_list <-
#' visualize_cost(...)}. Retrieve it with
#' \code{getOption("lago_cost_list")}. Closing the browser tab instead of using
#' the button does not save the list.
#'
# nocov start
# visualize_cost() validates its inputs and launches the app; the interactive
# UI + server live in .build_visualize_cost_app() below. Both only run inside a
# live browser session, which the coverage instrumentation cannot drive, so
# counting their lines only depresses the coverage figure without measuring
# anything. The curve/drag math they rely on is tested separately by the
# standalone JavaScript test (tests/js/test-cost-math.js, run in its own CI job),
# and the pure R helpers below (compute_slider_range, format_coef) are outside
# this block and are covered by tests/testthat/test-helpers.R.
visualize_cost <- function(
    component_names,
    unit_costs,
    default_cost_fxn_type,
    intervention_lower_bounds,
    intervention_upper_bounds) {
  # input validation
  stopifnot(
    "component names must be a character vector." =
      is.character(component_names),
    "give at least one component name." =
      length(component_names) >= 1,
    "component names must not be missing or empty." =
      !anyNA(component_names) && all(nzchar(component_names)),
    "unit costs must be a numeric vector." =
      is.numeric(unit_costs),
    "unit costs must all be finite." =
      all(is.finite(unit_costs)),
    "unit costs must not be negative." =
      all(unit_costs >= 0),
    "unit costs must have the same length as component names." =
      length(unit_costs) == length(component_names),
    "default cost function type must be a single character string." =
      is.character(default_cost_fxn_type) && length(default_cost_fxn_type) == 1,
    "default cost function type must be either 'linear' or 'cubic'." =
      default_cost_fxn_type %in% c("linear", "cubic"),
    "intervention lower bounds must be a numeric vector." =
      is.numeric(intervention_lower_bounds),
    "intervention lower bounds must have the same length as component names." =
      length(intervention_lower_bounds) == length(component_names),
    "intervention lower bounds must all be finite." =
      all(is.finite(intervention_lower_bounds)),
    "intervention upper bounds must be a numeric vector." =
      is.numeric(intervention_upper_bounds),
    "intervention upper bounds must have the same length as component names." =
      length(intervention_upper_bounds) == length(component_names),
    "intervention upper bounds must all be finite." =
      all(is.finite(intervention_upper_bounds)),
    "intervention lower bounds must be less than intervention upper bounds." =
      all(intervention_lower_bounds < intervention_upper_bounds)
  )

  # Build and launch the app. The quit button's observer stores the current cost
  # list in the `lago_cost_list` option and calls stopApp(cost_list), so
  # runApp() returns that list on a button-close and NULL otherwise (closing the
  # browser tab leaves the app running; an error propagates). Report the save
  # only on that button-close path.
  cost_list <- shiny::runApp(
    .build_visualize_cost_app(
      component_names = component_names,
      unit_costs = unit_costs,
      default_cost_fxn_type = default_cost_fxn_type,
      intervention_lower_bounds = intervention_lower_bounds,
      intervention_upper_bounds = intervention_upper_bounds
    ),
    # only this computer can reach the app
    host = "127.0.0.1"
  )
  if (!is.null(cost_list)) {
    message(
      "Your cost list has been saved to the `lago_cost_list` option.\n",
      "Retrieve it with: cost_list <- getOption(\"lago_cost_list\")\n",
      "Use it with: ",
      "lago_optimization(..., cost_list_of_vectors = ",
      "getOption(\"lago_cost_list\"))"
    )
  }
  invisible(cost_list)
}

# Build the UI tags and server function of the visualize_cost() app for one fixed
# component configuration, returned as list(ui, server). Kept separate from both
# visualize_cost() (which runs the app locally) and .build_visualize_cost_app()
# (which wraps these into a request-driven shinyApp), so a single source of truth
# builds the UI and server for a config.
#
# The same parts also run inside lago_app(), as a module: `ns` namespaces every
# id, `embedded = TRUE` leaves out the page frame and the buttons that only the
# stand alone app has, and `on_apply(cost_list, form)` is called when the user
# applies the costs. `initial_coefs` starts the designer on given coefficients,
# one vector per component of the starting form's length, instead of the
# defaults. The server works both as a plain server and in moduleServer(),
# since it reads its namespace from the session.
.build_cost_parts <- function(
    component_names,
    unit_costs,
    default_cost_fxn_type,
    intervention_lower_bounds,
    intervention_upper_bounds,
    ns = shiny::NS(NULL),
    embedded = FALSE,
    initial_coefs = NULL,
    on_apply = NULL) {
  # Precompute the initial coefficients and per-coefficient slider ranges for
  # BOTH cost-function forms, so the app can switch between them at runtime with
  # a Linear/Cubic toggle. A linear total cost has 2 coefficients, a cubic
  # (degree-4 total) has 5. Each coefficient gets its own slider range scaled to
  # its magnitude (see compute_slider_range) so a slider is usable regardless of
  # the coefficient's scale, and the user can still override any range with the
  # per-slider min/max inputs. The UI renders the maximum number of slider rows
  # per component and shows/hides the higher-order rows as the form changes; the
  # server assembles the coefficient vector at the currently selected form's
  # length.
  cost_types <- c("linear", "cubic")
  coefs_by_type <- list(
    linear = cost_fxn_calculator(
      intervention_lower_bounds = intervention_lower_bounds,
      intervention_upper_bounds = intervention_upper_bounds,
      unit_costs = unit_costs,
      default_cost_fxn_type = "linear"
    ),
    # The designer opens the cubic form on its own visibly curved but still valid
    # (non-negative, non-decreasing) demo cost, which the user then shapes,
    # rather than on cost_fxn_calculator's cubic; see .designer_cubic_coefs.
    cubic = lapply(seq_along(unit_costs), function(ci) {
      .designer_cubic_coefs(
        intervention_lower_bounds[ci],
        intervention_upper_bounds[ci],
        unit_costs[ci]
      )
    })
  )
  # the starting form opens on the given coefficients where they fit it
  if (!is.null(initial_coefs)) {
    for (ci in seq_along(initial_coefs)) {
      v <- initial_coefs[[ci]]
      if (length(v) == length(coefs_by_type[[default_cost_fxn_type]][[ci]]) && all(is.finite(v))) {
        coefs_by_type[[default_cost_fxn_type]][[ci]] <- as.numeric(v)
      }
    }
  }
  ranges_by_type <- stats::setNames(lapply(cost_types, function(t) {
    lapply(seq_along(coefs_by_type[[t]]), function(ci) {
      lapply(seq_along(coefs_by_type[[t]][[ci]]), function(i) {
        compute_slider_range(
          coefs_by_type[[t]][[ci]][i], unit_costs[ci],
          power = i - 1,
          lb = intervention_lower_bounds[ci],
          ub = intervention_upper_bounds[ci]
        )
      })
    })
  }), cost_types)
  n_components <- length(component_names)
  # Each slider row shows its coefficient times a fixed power of 10, so a row
  # whose coefficient is very small or very large still has a usable slider:
  # a slider cannot step below about 1e-6, and steps far below a value's size
  # move nothing. The scale comes from the cubic form's range, which has every
  # row, and is 1 for ordinary ranges, where the slider shows the coefficient.
  row_scales <- lapply(seq_len(n_components), function(ci) {
    vapply(ranges_by_type$cubic[[ci]], .slider_scale, numeric(1))
  })
  sc <- function(ci, i) row_scales[[ci]][i]
  # a range in true units, as the slider of row i shows it
  disp <- function(rng, ci, i) {
    s <- sc(ci, i)
    list(min = rng$min * s, max = rng$max * s, step = slider_step(rng$min * s, rng$max * s))
  }
  # coefficient count for a component under a given form
  n_coef <- function(t, ci) length(coefs_by_type[[t]][[ci]])
  # maximum slider rows to render per component (linear 2, cubic 5)
  max_ncoef <- vapply(seq_len(n_components), function(ci) {
    max(vapply(cost_types, function(t) n_coef(t, ci), integer(1)))
  }, integer(1))
  # value + range to seed slider row i0 (0-based) of component ci at build time:
  # the chosen starting form's value when that row is part of the form, else the
  # cubic value (a sensible default for when a form switch later reveals it).
  seed_row <- function(ci, i0) {
    t <- if (i0 < n_coef(default_cost_fxn_type, ci)) {
      default_cost_fxn_type
    } else {
      "cubic"
    }
    list(
      value = coefs_by_type[[t]][[ci]][i0 + 1] * sc(ci, i0 + 1),
      rng = disp(ranges_by_type[[t]][[ci]][[i0 + 1]], ci, i0 + 1)
    )
  }

  # Serve the vendored client-side assets (D3 v7 + the cost-curve binding) from
  # the installed package's inst/js directory under a URL prefix. This keeps the
  # curves client-side and offline (no CDN), as required for CRAN. Using
  # addResourcePath + tags$script avoids a new hard dependency on htmltools
  # (htmlDependency is not re-exported by shiny); addResourcePath and tags are
  # both provided by shiny, which is already imported.
  if (!embedded) {
    js_dir <- system.file("js", package = "LAGOtrials")
    addResourcePath("lago_cost_assets", js_dir)
  }

  form_toggle <- div(
    style = paste(
      "padding: 10px 16px; background-color: #f8f9fa;",
      "border-bottom: 1px solid #dee2e6;"
    ),
    radioButtons(
      inputId = ns("cost_type"),
      label = "Cost function form",
      choices = c("Linear" = "linear", "Cubic" = "cubic"),
      selected = default_cost_fxn_type,
      inline = TRUE
    )
  )

  # One nav panel per component
  panels <- lapply(seq_len(n_components), function(component_idx) {
    nav_panel(
      paste("Component", component_idx, ":", component_names[component_idx]),
      fluidRow(
        column(
          5,
          card(
            card_header("Adjust coefficients using sliders"),
            card_body(
              # Create sliders for the component's coefficients. Every form's
              # rows are rendered (up to the cubic's 5); rows beyond the
              # starting form's length are hidden and revealed when the user
              # switches form. Each slider has its own default range
              # (compute_slider_range) plus two numeric inputs for a custom
              # range. Each row is wrapped in an id'd div so the cost_type
              # observer can show or hide it.
              lapply(
                seq_len(max_ncoef[component_idx]),
                function(i) {
                  seed <- seed_row(component_idx, i - 1)
                  rng <- seed$rng
                  hidden <- (i - 1) >= n_coef(default_cost_fxn_type, component_idx)
                  div(
                    id = ns(paste0("coef_row_", component_idx, "_", i - 1)),
                    style = if (hidden) "display: none;" else NULL,
                    div(
                      style = paste(
                        "display: flex; gap: 10px; align-items: flex-end;",
                        "margin-bottom: 5px;"
                      ),
                      numericInput(
                        inputId = ns(paste0(
                          "range_min_", component_idx, "_", i - 1
                        )),
                        label = "Range min",
                        value = rng$min,
                        width = "120px"
                      ),
                      numericInput(
                        inputId = ns(paste0(
                          "range_max_", component_idx, "_", i - 1
                        )),
                        label = "Range max",
                        value = rng$max,
                        width = "120px"
                      )
                    ),
                    .scaled_slider(
                      sliderInput(
                        inputId = ns(paste0("coef_", component_idx, "_", i - 1)),
                        label = paste0(
                          component_names[component_idx], "^", i - 1,
                          " coefficient",
                          .scale_label(sc(component_idx, i))
                        ),
                        min = rng$min,
                        max = rng$max,
                        value = seed$value,
                        step = rng$step,
                        width = "100%"
                      ),
                      sc(component_idx, i)
                    )
                  )
                }
              ),
              hr(),
              # Add reset button
              actionButton(
                inputId = ns(paste0("reset_", component_idx)),
                label = "Reset Coefficients",
                class = "btn-warning"
              ),
              hr(),
              h4(paste("Coefficient vector for", component_names[component_idx], ":")),
              # Add warning message below coefficient vector
              div(
                id = ns(paste0("coef_warning_", component_idx)),
                style = "color: red; margin-top: 10px; display: none;",
                "Warning: Current coefficients may lead to negative marginal costs!"
              ),
              verbatimTextOutput(ns(paste0("coefficient_text_", component_idx)))
            )
          )
        ),
        column(
          7,
          card(
            card_header("Visualization"),
            card_body(
              # Add warning messages above plot
              div(
                id = ns(paste0("plot_warning_", component_idx)),
                style = "color: red; margin-bottom: 5px; display: none;",
                "Warning: Total cost function should be non-decreasing!"
              ),
              div(
                id = ns(paste0("negative_cost_warning_", component_idx)),
                style = "color: red; margin-bottom: 10px; display: none;",
                "Warning: Marginal cost function should always be positive!"
              ),
              div(
                id = ns(paste0("negative_total_warning_", component_idx)),
                style = "color: red; margin-bottom: 10px; display: none;",
                "Warning: Total cost should never be negative!"
              ),
              # Client-side D3 (v7) target. cost-curves.js reads the coefs
              # straight from this component's sliders and the bounds / unit
              # cost from these data-* attributes, then renders both SVGs and
              # redraws instantly as the sliders move (no server round-trip).
              # The right endpoint of the total-cost curve is draggable; the
              # drag writes rescaled coefficients back via
              # input$dragged_coefs_<component> (see the observeEvent below).
              # data-ns is the namespace prefix of the slider and drag ids.
              div(
                id = ns(paste0("cost_curves_", component_idx)),
                class = "lago-cost-curves",
                `data-component` = component_idx,
                `data-ns` = ns(""),
                # data-ncoef is the currently selected form's coefficient count
                # (the number of coefficients the curve reads); it is updated
                # when the form changes. data-maxncoef is the number of slider
                # rows rendered (so the JS binds them all up front, including
                # rows that a later form switch reveals).
                `data-ncoef` = n_coef(default_cost_fxn_type, component_idx),
                `data-maxncoef` = max_ncoef[component_idx],
                `data-lb` = intervention_lower_bounds[component_idx],
                `data-ub` = intervention_upper_bounds[component_idx],
                `data-unit-cost` = unit_costs[component_idx],
                `data-name` = component_names[component_idx]
              ),
              hr(),
              # Key numeric values over the intervention range, so the user
              # can target a known cost rather than only eyeballing the curve.
              verbatimTextOutput(ns(paste0("cost_summary_", component_idx)))
            )
          )
        )
      )
    )
  })

  if (embedded) {
    # inside lago_app(): the app loads the scripts, and the user applies the
    # costs to the app instead of copying them or returning them to R
    ui <- tagList(
      form_toggle,
      do.call(bslib::navset_card_tab, panels),
      div(
        style = "padding: 12px 16px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
        h5("Cost coefficients for the app"),
        verbatimTextOutput(ns("complete_coef_list"))
      )
    )
    # the app places this button in the dialog's footer, next to Cancel
    apply_ui <- actionButton(
      inputId = ns("apply"),
      label = "Use these costs",
      class = "btn-primary",
      icon = icon("circle-check")
    )
  } else {
    # bslib warns that navbarPage's direct children should all be nav panels; this
    # app deliberately also passes useShinyjs() and the form-toggle header, so the
    # warning is expected and benign. Suppress it so it does not clutter the
    # console on every launch.
    ui <- suppressWarnings(navbarPage(
      title = "Cost Functions Visualization",
      theme = bs_theme(version = 5, bootswatch = "flatly"),

      # Load the vendored D3 first, then the cost-curve binding (tags$head content
      # is hoisted to <head>). The Linear/Cubic form toggle sits alongside it in
      # the header so it shows above every component tab and switches all
      # components at once.
      header = tagList(
        tags$head(
          # shinylive runs this app in a same-origin iframe whose URL does NOT
          # carry the parent tab's ?components=... deep link (that query lives on
          # the top /visualize-cost/ tab). Without it the app would always fall
          # back to the example, so the designer never receives the components the
          # playground handed off. Adopt the parent's query into this iframe's own
          # URL (a one-time reload) so the normal request/clientData query parsing
          # below picks it up. Guarded: only when we are in an iframe, the parent
          # has a query, and this frame does not yet (prevents a reload loop); a
          # cross-origin parent throws and is ignored. No-op outside an iframe
          # (e.g. a local visualize_cost() launch), where window.top === window.
          tags$script(shiny::HTML(paste0(
            "(function(){try{",
            "if (window.top === window) return;",           # not in an iframe (local launch)
            "var q = window.top.location.search;",           # parent tab's query, same-origin
            "if (!q || q.indexOf('components=') === -1) return;", # only a cost-designer deep link
            "if (window.location.search) return;",           # this frame already carries a query
            "var seen = false;",
            "try { seen = !!sessionStorage.getItem('lago_cost_adopted');",
            "  if (!seen) sessionStorage.setItem('lago_cost_adopted', '1'); } catch (e) {}",
            "if (!seen) window.location.replace(window.location.pathname + q);", # adopt once
            "}catch(e){}})();"
          ))),
          tags$script(src = "lago_cost_assets/d3.v7.min.js"),
          tags$script(src = "lago_cost_assets/cost-curves.js")
        ),
        form_toggle
      ),

      # Include shinyjs
      useShinyjs(),

      footer = div(
        style = "padding: 20px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
        fluidRow(
          column(
            8, # Takes up 8/12 of the width for the coefficient list
            h4("Using these cost functions in optimization"),
            p("If you are satisfied with the cost functions for all intervention components, use the following coefficient list when running lago_optimization():"),
            verbatimTextOutput("complete_coef_list"),
            div(
              style = "margin-bottom: 10px;",
              actionButton(
                inputId = "copy_button",
                label = "Copy to clipboard",
                class = "btn-primary btn-sm",
                icon = icon("clipboard")
              ),
              # Send the current cost functions back to the playground tab that
              # opened this designer (see the postMessage in the observer below).
              actionButton(
                inputId = "send_to_playground",
                label = "Use these costs in the playground",
                class = "btn-primary btn-sm",
                icon = icon("arrow-left")
              ),
              span(
                id = "copy_confirmation",
                style = "color: #198754; margin-left: 10px; display: none;",
                "Copied!"
              ),
              span(
                id = "send_confirmation",
                style = "color: #198754; margin-left: 10px; display: none;",
                "Sent to the playground."
              ),
              span(
                id = "send_error",
                style = "color: #7a2020; margin-left: 10px; display: none;",
                "Open the cost designer from the playground to send costs back."
              )
            ),
            p("Example usage: lago_optimization(..., cost_list_of_vectors = cost_list)")
          ),
          column(
            4, # Takes up 4/12 of the width for the finish button
            div(
              style = "text-align: right; padding-top: 20px;",
              actionButton(
                inputId = "quit_button",
                label = "Return list to R & close",
                class = "btn-danger",
                icon = icon("circle-check")
              )
            )
          )
        )
      ),

      # Add nav panels for each component
      !!!panels
    ))
  }

  calculate_cost <- function(coefficients, x) {
    degree <- length(coefficients) - 1
    result <- sapply(x, function(x_val) {
      sum(coefficients * x_val^(0:degree))
    })
    return(result)
  }

  calculate_derivative <- function(coefficients, x) {
    degree <- length(coefficients) - 1
    if (degree == 0) {
      return(rep(0, length(x)))
    }
    derivative_coeffs <- coefficients[-1] * (1:degree)
    result <- sapply(x, function(x_val) {
      sum(derivative_coeffs * x_val^(0:(degree - 1)))
    })
    return(result)
  }

  server <- function(input, output, session) {
    # A slider reports its value rounded to its step, which loses the small
    # high order coefficients of a cubic. The server keeps the exact value it
    # set, and a slider value within half a step of it is read as that value.
    exact <- new.env(parent = emptyenv())
    # The environment is not reactive, so every change to it bumps this
    # counter. Everything that reads an exact value depends on the counter, so
    # it updates even when the slider reports no change, for example after a
    # drag to a value within half a step of the slider's old reading.
    exact_rev <- reactiveVal(0)
    bump_exact <- function() exact_rev(isolate(exact_rev()) + 1)
    # the drawn curves read the same exact values, see preferExact() in
    # cost-curves.js
    # stale is the slider's reading when only its range changed: while the
    # slider still reads that, it has not been moved and the exact value holds
    set_coef <- function(id, value, step, stale = NULL) {
      assign(id, list(value = value, step = step, stale = stale), envir = exact)
      bump_exact()
      session$sendCustomMessage("lago-exact-coef", list(id = session$ns(id), value = value, step = step, stale = stale))
    }
    drop_coef <- function(id) {
      if (exists(id, envir = exact, inherits = FALSE)) rm(list = id, envir = exact)
      bump_exact()
      session$sendCustomMessage("lago-exact-coef", list(id = session$ns(id), value = NULL))
    }
    coef_input <- function(component_idx, i) {
      id <- paste0("coef_", component_idx, "_", i - 1)
      # the slider shows the coefficient times its row's scale
      v <- input[[id]]
      if (!is.null(v)) v <- v / sc(component_idx, i)
      exact_rev()
      e <- if (exists(id, envir = exact, inherits = FALSE)) get(id, envir = exact)
      # a slider that has not reported yet, as in a dialog that just opened,
      # reads as the value the server set
      if (is.null(v)) {
        return(if (is.null(e)) NA_real_ else e$value)
      }
      # the stale reading lasts until the slider reports anything else
      if (!is.null(v) && !is.null(e$stale) && !isTRUE(v == e$stale)) {
        e$stale <- NULL
        assign(id, e, envir = exact)
      }
      if (!is.null(v) && !is.null(e) &&
        (abs(v - e$value) <= e$step / 2 || isTRUE(v == e$stale))) {
        e$value
      } else {
        v
      }
    }
    for (component_idx in seq_len(n_components)) {
      for (i in seq_along(coefs_by_type[[default_cost_fxn_type]][[component_idx]])) {
        set_coef(
          paste0("coef_", component_idx, "_", i - 1),
          coefs_by_type[[default_cost_fxn_type]][[component_idx]][i],
          disp(ranges_by_type[[default_cost_fxn_type]][[component_idx]][[i]], component_idx, i)$step /
            sc(component_idx, i)
        )
      }
    }

    # Remove the resource-path prefix registered above when the app stops, so
    # relaunching does not leak "lago_cost_assets" process-globally across
    # launches. The addResourcePath above re-runs on the next launch.
    if (!embedded) onStop(function() removeResourcePath("lago_cost_assets"))

    # The currently selected cost-function form (linear/cubic); defaults to the
    # form the app was launched with until the user toggles it.
    cur_type <- reactive({
      if (is.null(input$cost_type)) default_cost_fxn_type else input$cost_type
    })

    # Current coefficients for all components, as a list of numeric vectors, at
    # the selected form's length (2 for linear, 5 for cubic). Shared by the
    # coefficient-list text, the copy button, and the value returned to R when
    # the app closes.
    current_cost_list <- reactive({
      lapply(seq_len(n_components), function(component_idx) {
        sapply(
          seq_len(n_coef(cur_type(), component_idx)),
          function(i) {
            coef_input(component_idx, i)
          }
        )
      })
    })

    # The `cost_list <- list(...)` snippet, ready to paste into R.
    cost_list_code <- reactive({
      coef_strings <- sapply(current_cost_list(), function(coefs) {
        paste0("c(", paste(format_coef_code(coefs), collapse = ", "), ")")
      })
      paste0(
        "cost_list <- list(\n    ",
        paste(coef_strings, collapse = ",\n    "),
        "\n)"
      )
    })

    # Create output for complete coefficient list
    output$complete_coef_list <- renderText({
      cost_list_code()
    })

    # Create reactive expressions and outputs for each component
    lapply(seq_len(n_components), function(component_idx) {
      # Add observer for reset button: reset each slider to the CURRENT form's
      # initial value and default range (so Reset in cubic mode restores the
      # cubic defaults, not the linear ones).
      observeEvent(input[[paste0("reset_", component_idx)]], {
        t <- cur_type()
        lapply(
          seq_len(n_coef(t, component_idx)),
          function(i) {
            rng <- disp(ranges_by_type[[t]][[component_idx]][[i]], component_idx, i)
            updateNumericInput(
              session,
              inputId = paste0("range_min_", component_idx, "_", i - 1),
              value = rng$min
            )
            updateNumericInput(
              session,
              inputId = paste0("range_max_", component_idx, "_", i - 1),
              value = rng$max
            )
            set_coef(
              paste0("coef_", component_idx, "_", i - 1),
              coefs_by_type[[t]][[component_idx]][i], rng$step / sc(component_idx, i)
            )
            updateSliderInput(
              session,
              inputId = paste0("coef_", component_idx, "_", i - 1),
              value = coefs_by_type[[t]][[component_idx]][i] * sc(component_idx, i),
              min = rng$min,
              max = rng$max,
              step = rng$step
            )
          }
        )
      })

      # Observers for the custom range inputs: when the user edits a slider's
      # min or max, update that slider's range. Only apply valid ranges
      # (both finite, min < max) so a partially-typed value does not break
      # the slider. Set up for every rendered row (including higher-order rows a
      # form switch reveals), not just the starting form's rows.
      lapply(
        seq_len(max_ncoef[component_idx]),
        function(i) {
          observeEvent(
            {
              input[[paste0("range_min_", component_idx, "_", i - 1)]]
              input[[paste0("range_max_", component_idx, "_", i - 1)]]
            },
            {
              rmin <- input[[paste0("range_min_", component_idx, "_", i - 1)]]
              rmax <- input[[paste0("range_max_", component_idx, "_", i - 1)]]
              if (is.null(rmin) || is.null(rmax) ||
                !is.finite(rmin) || !is.finite(rmax) || rmin >= rmax) {
                return()
              }
              # the stored exact value keeps the slider's new step, or is
              # dropped when the new range leaves it out or the user has moved
              # the slider away from it
              id <- paste0("coef_", component_idx, "_", i - 1)
              # the range inputs are in the slider's units, the exact value in
              # the coefficient's
              s <- sc(component_idx, i)
              kept <- NULL
              if (exists(id, envir = exact, inherits = FALSE)) {
                e <- get(id, envir = exact)
                kept <- e$value
                cur <- isolate(input[[id]])
                if (!is.null(cur)) cur <- cur / s
                unmoved <- is.null(cur) || abs(cur - e$value) <= e$step / 2 ||
                  isTRUE(cur == e$stale)
                if (unmoved && kept * s >= rmin && kept * s <= rmax) {
                  set_coef(id, kept, slider_step(rmin, rmax) / s, stale = cur)
                } else {
                  drop_coef(id)
                  kept <- NULL
                }
              }
              # the slider moves to the exact value, so at its new finer step it
              # still reads as that value
              if (is.null(kept)) {
                updateSliderInput(
                  session,
                  inputId = id,
                  min = rmin,
                  max = rmax,
                  step = slider_step(rmin, rmax)
                )
              } else {
                updateSliderInput(
                  session,
                  inputId = id,
                  value = kept * s,
                  min = rmin,
                  max = rmax,
                  step = slider_step(rmin, rmax)
                )
              }
            },
            ignoreInit = TRUE
          )
        }
      )

      # Coefficient text output
      output[[paste0("coefficient_text_", component_idx)]] <- renderText({
        current_coefs <- sapply(
          seq_len(n_coef(cur_type(), component_idx)),
          function(i) {
            coef_input(component_idx, i)
          }
        )
        req(all(is.finite(current_coefs)))
        paste0("c(", paste(format_coef_code(current_coefs), collapse = ", "), ")")
      })

      # Add reactive expression for cost function validation
      observe({
        current_coefs <- sapply(
          seq_len(n_coef(cur_type(), component_idx)),
          function(i) {
            coef_input(component_idx, i)
          }
        )
        req(all(is.finite(current_coefs)))

        # the total must not fall, the marginal (the derivative) must not be
        # negative, and the total must not be negative
        validity <- .cost_validity(
          current_coefs,
          intervention_lower_bounds[component_idx],
          intervention_upper_bounds[component_idx]
        )
        is_non_decreasing_valid <- validity$non_decreasing
        is_positive_valid <- validity$positive_marginal
        is_non_negative_valid <- validity$non_negative_total

        # Update warnings based on validation results
        if (!is_non_decreasing_valid) {
          shinyjs::show(paste0("plot_warning_", component_idx))
        } else {
          shinyjs::hide(paste0("plot_warning_", component_idx))
        }

        # the warning below the coefficients is about the marginal cost, so it
        # follows the marginal check
        if (!is_positive_valid) {
          shinyjs::show(paste0("negative_cost_warning_", component_idx))
          shinyjs::show(paste0("coef_warning_", component_idx))
        } else {
          shinyjs::hide(paste0("negative_cost_warning_", component_idx))
          shinyjs::hide(paste0("coef_warning_", component_idx))
        }

        if (!is_non_negative_valid) {
          shinyjs::show(paste0("negative_total_warning_", component_idx))
        } else {
          shinyjs::hide(paste0("negative_total_warning_", component_idx))
        }

        # Briefly outline this component's visualization card in red when a
        # coefficient choice is invalid. This replaces a full-page background
        # flash, which fired the whole body on every reactive tick while
        # dragging through an invalid region and was jarring; the per-card
        # outline plus the inline warnings convey the problem without it.
        if (!is_non_decreasing_valid || !is_positive_valid ||
          !is_non_negative_valid) {
          runjs(sprintf(
            'var w = document.getElementById("%1$s");
             var card = w ? w.closest(".card") : null;
             if (card) {
               card.style.transition = "box-shadow 0.4s";
               card.style.boxShadow = "0 0 0 3px rgba(220,53,69,0.6)";
               setTimeout(function(){ card.style.boxShadow = "none"; }, 600);
             }',
            session$ns(paste0("plot_warning_", component_idx))
          ))
        }
      })

      # The total-cost and marginal-cost curves are now drawn client-side by
      # inst/js/cost-curves.js (D3 v7 SVG), which reads this component's
      # coefficients directly from its sliders and redraws instantly on every
      # slider tick with no server round-trip. The former renderPlot() ->
      # PNG round-trip has been removed. The server still owns the
      # authoritative validation (the observe() above) and the numeric summary
      # below, and it handles the drag writeback (the observeEvent below).

      # Drag writeback. When the user drags the right endpoint of the total-cost
      # curve, cost-curves.js rescales all coefficients and sends the new vector
      # here as an event. We update each slider to the rescaled value, first
      # widening its min/max if the new value would fall outside the current
      # slider range (a slider silently clamps out-of-range values, which would
      # otherwise break the round-trip and the copy snippet).
      #
      # No feedback oscillation: this is the ONLY path that reacts to the drag
      # input, and updating the sliders here does NOT call Shiny.setInputValue.
      # The slider updates fire the sliders' change events, which the JS turns
      # into a normal client-side redraw (never a new drag event). ignoreInit
      # keeps it from firing on app start, and priority:"event" on the JS side
      # means a repeated target still registers as a fresh, single event.
      observeEvent(input[[paste0("dragged_coefs_", component_idx)]],
        {
          msg <- input[[paste0("dragged_coefs_", component_idx)]]
          new_coefs <- as.numeric(unlist(msg$coefs))
          if (length(new_coefs) != n_coef(cur_type(), component_idx) ||
            any(!is.finite(new_coefs))) {
            return()
          }
          lapply(seq_along(new_coefs), function(i) {
            val <- new_coefs[i]
            # the range inputs and the slider are in the slider's units
            s <- sc(component_idx, i)
            cur_min <- input[[paste0("range_min_", component_idx, "_", i - 1)]]
            cur_max <- input[[paste0("range_max_", component_idx, "_", i - 1)]]
            # Widen the range on the side the new value would be clamped, to
            # the default range of a coefficient of that value, so the margin
            # scales with the value and the bounds. Reuse the range-input
            # mechanism so the displayed Range min / Range max stay consistent
            # with the slider.
            wide <- compute_slider_range(
              val, unit_costs[component_idx],
              power = i - 1,
              lb = intervention_lower_bounds[component_idx],
              ub = intervention_upper_bounds[component_idx]
            )
            new_min <- cur_min
            new_max <- cur_max
            if (is.null(new_min) || !is.finite(new_min) || val * s < new_min) {
              new_min <- wide$min * s
            }
            if (is.null(new_max) || !is.finite(new_max) || val * s > new_max) {
              new_max <- wide$max * s
            }
            if (!identical(new_min, cur_min)) {
              updateNumericInput(
                session,
                inputId = paste0("range_min_", component_idx, "_", i - 1),
                value = new_min
              )
            }
            if (!identical(new_max, cur_max)) {
              updateNumericInput(
                session,
                inputId = paste0("range_max_", component_idx, "_", i - 1),
                value = new_max
              )
            }
            set_coef(
              paste0("coef_", component_idx, "_", i - 1), val,
              slider_step(new_min, new_max) / s
            )
            updateSliderInput(
              session,
              inputId = paste0("coef_", component_idx, "_", i - 1),
              value = val * s,
              min = new_min,
              max = new_max,
              step = slider_step(new_min, new_max)
            )
          })
        },
        ignoreInit = TRUE
      )

      # Numeric summary of the current cost function over the intervention
      # range: total cost at the lower and upper bounds, and the average
      # marginal (per-unit) cost across the range.
      output[[paste0("cost_summary_", component_idx)]] <- renderText({
        current_coefs <- sapply(
          seq_len(n_coef(cur_type(), component_idx)),
          function(i) {
            coef_input(component_idx, i)
          }
        )
        req(all(is.finite(current_coefs)))
        lb <- intervention_lower_bounds[component_idx]
        ub <- intervention_upper_bounds[component_idx]
        cost_lb <- calculate_cost(current_coefs, lb)
        cost_ub <- calculate_cost(current_coefs, ub)
        x_vals <- seq(lb, ub, length.out = 2000)
        mean_marginal <- mean(calculate_derivative(current_coefs, x_vals))
        paste0(
          "Total cost at ", component_names[component_idx], " = ",
          format_coef(lb), ": ", format_coef(cost_lb), "\n",
          "Total cost at ", component_names[component_idx], " = ",
          format_coef(ub), ": ", format_coef(cost_ub), "\n",
          "Average marginal cost over the range: ",
          format_coef(mean_marginal)
        )
      })
    })

    # Switch every component between the linear and cubic cost-function forms.
    # All slider rows are rendered up front; here we reconfigure the rows the new
    # form uses (value + range), show or hide the higher-order rows, tell the
    # client how many coefficients each curve should now read (data-ncoef), and
    # redraw. ignoreInit skips the initial value so the app opens in the form it
    # was launched with without a needless rebuild.
    observeEvent(input$cost_type, {
      t <- input$cost_type
      lapply(seq_len(n_components), function(component_idx) {
        n <- n_coef(t, component_idx)
        lapply(seq_len(max_ncoef[component_idx]), function(i) {
          row_id <- paste0("coef_row_", component_idx, "_", i - 1)
          if (i <= n) {
            rng <- disp(ranges_by_type[[t]][[component_idx]][[i]], component_idx, i)
            updateNumericInput(
              session,
              inputId = paste0("range_min_", component_idx, "_", i - 1),
              value = rng$min
            )
            updateNumericInput(
              session,
              inputId = paste0("range_max_", component_idx, "_", i - 1),
              value = rng$max
            )
            set_coef(
              paste0("coef_", component_idx, "_", i - 1),
              coefs_by_type[[t]][[component_idx]][i], rng$step / sc(component_idx, i)
            )
            updateSliderInput(
              session,
              inputId = paste0("coef_", component_idx, "_", i - 1),
              value = coefs_by_type[[t]][[component_idx]][i] * sc(component_idx, i),
              min = rng$min,
              max = rng$max,
              step = rng$step
            )
            shinyjs::show(row_id)
          } else {
            shinyjs::hide(row_id)
          }
        })
      })
      # The coefficient count is uniform across components for a given form, so a
      # single data-ncoef update covers every curve; then redraw them all.
      runjs(sprintf(
        paste0(
          "document.querySelectorAll('.lago-cost-curves[data-ns=\"%s\"]')",
          ".forEach(function(c){ c.setAttribute('data-ncoef', %d); });",
          " if (window.LAGOCostCurves && LAGOCostCurves.redraw) {",
          " LAGOCostCurves.redraw(); }"
        ),
        session$ns(""), n_coef(t, 1)
      ))
    }, ignoreInit = TRUE)

    if (embedded) {
      # inside lago_app(): hand the costs and the form to the app
      observeEvent(input$apply, {
        if (is.function(on_apply)) on_apply(current_cost_list(), cur_type())
      })
    } else {
      # Copy the coefficient-list snippet to the clipboard, with a brief
      # "Copied!" confirmation. Uses the async Clipboard API when available and
      # falls back to a hidden textarea + execCommand for older/insecure
      # (non-HTTPS) contexts.
      observeEvent(input$copy_button, {
        code <- cost_list_code()
        # encode the snippet as a JS string literal without adding a jsonlite
        # dependency: escape backslashes, quotes, and newlines, then wrap in
        # double quotes.
        esc <- gsub("\\\\", "\\\\\\\\", code)
        esc <- gsub("\"", "\\\\\"", esc)
        esc <- gsub("\n", "\\\\n", esc)
        code_js <- paste0("\"", esc, "\"")
        runjs(sprintf(
          '(function() {
            var text = %s;
            var done = function() {
              var el = document.getElementById("copy_confirmation");
              if (el) { el.style.display = "inline";
                setTimeout(function(){ el.style.display = "none"; }, 2000); }
            };
            if (navigator.clipboard && window.isSecureContext) {
              navigator.clipboard.writeText(text).then(done, done);
            } else {
              var ta = document.createElement("textarea");
              ta.value = text; ta.style.position = "fixed"; ta.style.opacity = "0";
              document.body.appendChild(ta); ta.focus(); ta.select();
              try { document.execCommand("copy"); } catch (e) {}
              document.body.removeChild(ta); done();
            }
          })();',
          code_js
        ))
      })

      # Send the current cost functions back to the playground tab that opened this
      # designer. window.opener is that tab (the designer is opened with a plain
      # window.open, no noopener), so postMessage hands over the component names and
      # their coefficient vectors same-origin; the playground uses them as
      # cost_list_of_vectors. If there is no opener (the designer was opened
      # directly), show a hint instead of failing silently.
      observeEvent(input$send_to_playground, {
        cl <- current_cost_list()
        # escape names for a JS string literal (mirrors the copy-button path):
        # backslash, double quote, and newline/carriage return, which would
        # otherwise break the literal.
        esc <- gsub("\\\\", "\\\\\\\\", component_names)
        esc <- gsub("\"", "\\\\\"", esc)
        esc <- gsub("\n", "\\\\n", esc)
        esc <- gsub("\r", "\\\\r", esc)
        comp_json <- paste0(
          "[", paste0("\"", esc, "\"", collapse = ","), "]"
        )
        costs_json <- paste0(
          "[",
          paste(
            vapply(
              cl,
              function(v) paste0("[", paste(v, collapse = ","), "]"),
              character(1)
            ),
            collapse = ","
          ),
          "]"
        )
        # Deliver to the playground. shinylive runs this app inside a same-origin
        # iframe, so window.opener here is null (the opener lives on the top tab);
        # a same-origin BroadcastChannel reaches the playground tab regardless of
        # the frame/opener relationship. The confirmation is gated on an opener
        # existing (window.top.opener, else window.opener) rather than on the
        # broadcast, since the broadcast cannot detect a missing listener: opened
        # from the playground shows "sent", a standalone launch shows the hint.
        runjs(sprintf(
          'var msg = {type: "lago-cost-designer", components: %s, costs: %s};
           /* Best-effort broadcast: reaches the playground tab across the
              same-origin shinylive iframe. BroadcastChannel cannot confirm
              receipt (postMessage succeeds even with no listener), so it does not
              drive the confirmation. */
           try {
             var bc = new BroadcastChannel("lago-cost-designer");
             bc.postMessage(msg); bc.close();
           } catch (e) {}
           /* Confirm only when this designer was launched from a tab (the
              playground) via an opener; also deliver through it, idempotently with
              the broadcast. A standalone launch has no opener, so show the hint. */
           var launched = false;
           try {
             var op = (window.top && window.top.opener) || window.opener;
             if (op && !op.closed) {
               op.postMessage(msg, window.location.origin); launched = true;
             }
           } catch (e) {}
           var el = document.getElementById(launched ? "send_confirmation" : "send_error");
           if (el) { el.style.display = "inline";
             setTimeout(function(){ el.style.display = "none"; }, launched ? 2500 : 4000); }',
          comp_json, costs_json
        ))
      })

      # Closing the app returns the current cost list to R, so the result can be
      # captured (e.g. cost_list <- visualize_cost(...)) instead of only copied.
      observeEvent(input$quit_button, {
        cl <- current_cost_list()
        # Store the list in the `lago_cost_list` option so it is available even
        # when the app was launched with a bare visualize_cost(...) call (no
        # assignment). Using options() rather than assigning into the global
        # environment keeps this CRAN-clean (package code may not write to
        # globalenv()) while preserving the convenience: the caller can retrieve
        # it with getOption("lago_cost_list"). This overwrites any existing value.
        # Returning it via stopApp() is what lets visualize_cost() report the save
        # (runApp() returns this list only on a button-close). In the browser
        # (shinylive) there is no R session to return to: options() is harmless and
        # stopApp() simply ends the app.
        options(lago_cost_list = cl)
        stopApp(cl)
      })
      }
  }

  list(ui = ui, server = server, apply_ui = if (embedded) apply_ui)
}

# Assemble the visualize_cost() shinyApp. The component configuration can come
# from the page's URL query (?components=a&components=b&lower=..&upper=..&costs=..&form=..),
# which is how the playground hands off the components the user set up; when the
# query is absent or invalid the explicit arguments are used, so a plain
# visualize_cost(...) / local launch is unchanged. Both the UI (request-driven)
# and the server (reading the same query at connect via isolate) resolve the
# same configuration independently and hand it to .build_cost_parts().
.build_visualize_cost_app <- function(
    component_names,
    unit_costs,
    default_cost_fxn_type,
    intervention_lower_bounds,
    intervention_upper_bounds) {
  defaults <- list(
    component_names = component_names,
    unit_costs = unit_costs,
    default_cost_fxn_type = default_cost_fxn_type,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds
  )
  ui <- function(request) {
    do.call(.build_cost_parts, .parse_cost_query(request$QUERY_STRING, defaults))$ui
  }
  server <- function(input, output, session) {
    args <- .parse_cost_query(
      shiny::isolate(session$clientData$url_search), defaults
    )
    do.call(.build_cost_parts, args)$server(input, output, session)
  }
  shinyApp(ui, server)
}
# nocov end

# Resolve the component configuration for the cost designer from a URL query
# string, falling back to `defaults` (a list of the .build_cost_parts arguments)
# whenever the query is missing or does not fully and validly specify a
# configuration. The query carries `components` (one repeated param per name), `lower`,
# `upper` and `costs` (comma-separated numbers, one per component) and an
# optional `form` ("linear"/"cubic"). Everything must agree in length and be
# finite with lower < upper, or the defaults are used, so a malformed link
# degrades to the standard example rather than erroring.
.parse_cost_query <- function(query_string, defaults) {
  if (is.null(query_string) || !nzchar(query_string)) {
    return(defaults)
  }
  q <- shiny::parseQueryString(query_string)
  # Component names come as repeated `components` params (components=a&components=b),
  # not a comma-joined string, so a name may itself contain a comma without being
  # split apart. The numeric params stay comma-joined (numbers never contain a
  # comma).
  comp_names <- unlist(q[names(q) == "components"], use.names = FALSE)
  if (length(comp_names) == 0L || any(!nzchar(comp_names))) {
    return(defaults)
  }
  as_num <- function(s) {
    if (is.null(s)) {
      return(numeric(0))
    }
    suppressWarnings(as.numeric(strsplit(s, ",", fixed = TRUE)[[1]]))
  }
  lower <- as_num(q[["lower"]])
  upper <- as_num(q[["upper"]])
  costs <- as_num(q[["costs"]])
  n <- length(comp_names)
  valid <- n >= 1 && n <= 10 &&
    length(lower) == n && length(upper) == n && length(costs) == n &&
    all(is.finite(lower)) && all(is.finite(upper)) && all(is.finite(costs)) &&
    all(costs >= 0) && all(lower < upper)
  if (!valid) {
    return(defaults)
  }
  form <- q[["form"]]
  if (is.null(form) || !form %in% c("linear", "cubic")) {
    form <- "linear"
  }
  list(
    component_names = comp_names,
    unit_costs = costs,
    default_cost_fxn_type = form,
    intervention_lower_bounds = lower,
    intervention_upper_bounds = upper
  )
}

# A visibly curved starting cost for the cubic form of the designer, returned as
# the 5 ascending-power coefficients (x^0..x^4) of the total cost. It is the
# convex increasing cost C(x) = unit_cost*lb + (unit_cost/2)(x-lb) +
# a4*(x-lb)^4, with a4 = unit_cost/(2*(ub-lb)^3). By construction it starts at
# C(lb) = unit_cost*lb and reaches C(ub) = unit_cost*ub (the same endpoints as
# the linear cost), stays non-negative and non-decreasing on [lb, ub], and puts
# half of its rise into the quartic term so the curve is clearly bent rather than
# within ~1% of a line. Used only as the designer's cubic starting point (the
# user reshapes it); it does not change the cost model used by lago_optimization.
.designer_cubic_coefs <- function(lower_bound, upper_bound, unit_cost) {
  span <- upper_bound - lower_bound
  a4 <- unit_cost / (2 * span^3)
  # Expand unit_cost*lb + (unit_cost/2)(x-lb) + a4*(x-lb)^4 into powers of x.
  c(
    a4 * lower_bound^4 + (unit_cost / 2) * lower_bound,
    -4 * a4 * lower_bound^3 + unit_cost / 2,
    6 * a4 * lower_bound^2,
    -4 * a4 * lower_bound,
    a4
  )
}

# Compute a default slider range for a single cost-function coefficient, the
# coefficient of x^power. The coefficients span very different scales, so a
# single fixed slider range is not usable for all of them. This helper returns a
# range centered on the coefficient's initial value, with a half width of k
# times its magnitude. The half width is at least the unit cost (1 when the unit
# cost is 0), so a zero-valued coefficient is still adjustable, and at least
# 1e-4, since the slider cannot show a step below 1e-6 (see slider_step()). It is
# also at least the swing that changes the total cost at the bound farthest
# from 0 by the cost range unit_cost * (ub - lb), so a high order coefficient on
# bounds near 0, such as [0, 0.001], can still bend the curve. Returns a
# list(min, max, step).
compute_slider_range <- function(init, unit_cost, k = 5, power = 1, lb = 0, ub = 1) {
  u <- if (unit_cost == 0) 1 else abs(unit_cost)
  far <- max(abs(lb), abs(ub))
  # the swing of this coefficient that moves the cost at the far bound by
  # about the unit cost times the width of the bounds
  natural <- u * (ub - lb) / far^power
  # a very large swing is capped, so the range stays finite. A very small one
  # is kept, as the designer shows such a row in a unit of its own size (see
  # .slider_scale), and otherwise the floor keeps a range wide enough to use.
  floor_hw <- if (is.finite(natural) && natural > 0 && natural < 1e-4) {
    natural
  } else {
    min(max(u, 1e-4, natural), 1e300)
  }
  half_width <- max(k * abs(init), floor_hw)
  # round the bounds for display: this keeps the auto-generated slider tick
  # labels and the pre-filled range inputs readable (e.g. -10.863 instead of
  # -10.8629818181818). Round outward so the initial value stays within range.
  # Round to 3 decimals, or to 3 significant digits of the half width when it
  # is small, so a small range is not rounded away.
  digits <- max(3, 2 - floor(log10(half_width)))
  unit <- 10^-digits
  lower <- round(floor(round((init - half_width) / unit, 6)) * unit, digits)
  upper <- round(ceiling(round((init + half_width) / unit, 6)) * unit, digits)
  list(min = lower, max = upper, step = slider_step(lower, upper))
}

# Re-expand a cost function sum_k c_k x^k on [lb, ub] in t = (x - lb) / (ub -
# lb), which runs from 0 to 1 over the bounds: the result d holds the
# coefficients of sum_j d_j t^j. Far from 0 the terms c_k x^k are large and
# nearly cancel, so evaluating the curve in x loses most of its digits. In t the
# terms are about as large as the cost itself, so the curve is evaluated with
# little round-off. err bounds the round-off in each d_j, a few units of the
# machine epsilon times the sum of the sizes of the terms that make up d_j. The
# browser uses the same steps, see shiftCoefs() in cost-curves.js.
.shift_coefs <- function(coefs, lb, ub) {
  n <- length(coefs)
  span <- ub - lb
  gam <- 8 * .Machine$double.eps
  # lb^0, lb^1, ... and span^0, span^1, ... by repeated products
  lb_pow <- cumprod(c(1, rep(lb, n - 1)))
  span_pow <- cumprod(c(1, rep(span, n - 1)))
  d <- numeric(n)
  err <- numeric(n)
  for (j in seq_len(n) - 1) {
    k <- j:(n - 1)
    terms <- choose(k, j) * coefs[k + 1] * lb_pow[k - j + 1]
    d[j + 1] <- sum(terms) * span_pow[j + 1]
    err[j + 1] <- gam * sum(abs(terms)) * span_pow[j + 1]
  }
  list(d = d, err = err)
}

# Check a cost function on [lb, ub] at n points. Returns a list of three flags:
# non_decreasing (the total cost never falls below its highest value so far),
# positive_marginal (the derivative is never negative) and non_negative_total
# (the total cost is never negative). The curve is evaluated in the form of
# .shift_coefs(), and each check allows only the round-off of that evaluation,
# so a fall that adds up over many small steps, or a fall of a small cost, is
# still caught. The browser runs the same checks, see validate() in
# cost-curves.js.
.cost_validity <- function(coefs, lb, ub, n = 2000) {
  sh <- .shift_coefs(coefs, lb, ub)
  d <- sh$d
  j <- seq_along(d) - 1
  t <- seq(0, 1, length.out = n)
  pw <- outer(t, j, "^")
  gam <- 8 * .Machine$double.eps
  # the rise of the total above its value at lb, and a bound on its error at
  # any point: the errors of d move every point by at most sum(err), and each
  # evaluation adds at most gam * sum(abs(d))
  rise <- drop(pw[, -1, drop = FALSE] %*% d[-1])
  coef_err <- sum(sh$err[-1])
  eval_err <- gam * sum(abs(d[-1]))
  total <- d[1] + rise
  total_err <- sh$err[1] + gam * abs(d[1]) + coef_err + eval_err
  # the derivative in t is the marginal cost times (ub - lb)
  dd <- (d * j)[-1]
  marg <- drop(pw[, seq_along(dd), drop = FALSE] %*% dd)
  marg_err <- sum((sh$err * j)[-1]) + gam * sum(abs(dd))
  list(
    non_decreasing = all(rise >= cummax(rise) - (coef_err + 2 * eval_err)),
    positive_marginal = all(marg >= -marg_err),
    non_negative_total = all(total >= -total_err)
  )
}

# The slider step for a range: 0.00001, a finer power of 10 for a narrow
# range, and a power of 10 near a millionth of the range for a wide range, so
# the step grows with the range. It is also at least about 1e-12 times the
# largest value in the range, so the arrow keys still move a slider whose
# values are so large that a smaller step is lost in rounding. It is never
# below 1e-6, since the slider reads a smaller step written as 1e-07 as a step
# of whole numbers.
slider_step <- function(lower, upper) {
  by_width <- 10^floor(log10((upper - lower) / 1e6))
  by_size <- 10^ceiling(log10(max(abs(lower), abs(upper)) * 1e-12))
  by_narrow <- 10^floor(log10(min((upper - lower) / 1000, 0.00001)))
  max(by_narrow, by_width, by_size, 1e-6)
}

# Format a coefficient for the code the user copies or reads back into R, at
# full precision (15 significant digits). A cubic's terms can nearly cancel when
# the bounds are far from 0, so fewer digits can change the cost a lot.
format_coef_code <- function(x) {
  vapply(x, function(v) as.character(v), character(1))
}

# Format a number for display in the app: 3 decimal places for values of 1 or
# more, and 3 significant digits for smaller values, so a small cost or a
# narrow bound does not read as 0. Trailing zeros are dropped. This keeps the
# cost summary readable instead of showing values like -10.8629818181818. The
# copied coefficient list uses format_coef_code() instead.
format_coef <- function(x) {
  vapply(x, function(v) {
    if (!is.finite(v) || abs(v) < 1) {
      return(as.character(signif(v, 3)))
    }
    formatC(round(v, 3), format = "f", digits = 3, drop0trailing = TRUE)
  }, character(1))
}


# The power of 10 a slider row shows its coefficient in: 1 for an ordinary
# range, and otherwise the one that brings the range to about -1 to 1. A slider
# cannot step below about 1e-6, and on a range of 1e12 a step of 1e-5 moves
# nothing, so such a coefficient is shown in a unit of its own size.
.slider_scale <- function(rng) {
  half <- max(abs(rng$min), abs(rng$max))
  if (!is.finite(half) || half <= 0 || (half >= 1e-3 && half <= 1e6)) {
    return(1)
  }
  10^(-round(log10(half)))
}

# The note a slider label gets when its row is shown in a unit other than 1.
.scale_label <- function(scale) {
  if (scale == 1) {
    return("")
  }
  paste0(" (shown in units of ", format(1 / scale, digits = 1), ")")
}

# A slider with its row's scale, which the drawn curves read it through.
.scaled_slider <- function(slider, scale) {
  htmltools::tagQuery(slider)$find("input")$addAttrs(`data-lago-scale` = scale)$allTags()
}
