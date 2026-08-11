#' Visualize and choose intervention cost functions
#'
#' @description Visualize the cost functions for the intervention components.
#' This function creates a Shiny app that allows the user to adjust the
#' coefficients of the cost functions for each intervention component and
#' visualize the resulting total cost function and its derivative.
#' The initial coefficients are calculated based on the unit costs, the
#' default cost function type (linear or cubic), and the lower and upper bounds.
#' The user can adjust the coefficients using sliders and reset them to their
#' initial values. Each slider has a default range scaled to its coefficient's
#' magnitude (derived from the unit costs and bounds), and the user can set a
#' custom range for any slider using its "Range min" / "Range max" inputs.
#' The app also displays the current coefficient vector for each component.
#' The user can copy the final coefficient list for use in the optimization
#' function lago_optimization().
#'
#' @param component_names A character vector of the names of the
#' intervention components.
#' @param unit_costs A numeric vector of the unit costs for each
#' intervention component.
#' @param default_cost_fxn_type A character string specifying the default
#' cost function type. Must be either "linear" or "cubic".
#' @param intervention_lower_bounds A numeric vector of the lower bounds for
#' each intervention component.
#' @param intervention_upper_bounds A numeric vector of the upper bounds for
#' each intervention component.
#'
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
#' the coefficient list is also assigned to \code{lago_cost_list} in the global
#' environment (overwriting any existing object of that name) and a message
#' reports this, so the list is available even when the app was launched with a
#' bare \code{visualize_cost(...)} call rather than \code{cost_list <-
#' visualize_cost(...)}. Closing the browser tab instead of using the button
#' does not save the list.
#'
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
    "unit costs must be a numeric vector." =
      is.numeric(unit_costs),
    "unit costs must all be finite." =
      all(is.finite(unit_costs)),
    "unit costs must have the same length as component names." =
      length(unit_costs) == length(component_names),
    "default cost function type must be a character." =
      is.character(default_cost_fxn_type),
    "default cost function type must be either 'linear' or 'cubic'." =
      default_cost_fxn_type %in% c("linear", "cubic"),
    "intervention lower bounds must be a numeric vector." =
      is.numeric(intervention_lower_bounds),
    "intervention lower bounds must have the same length as component names." =
      length(intervention_lower_bounds) == length(component_names),
    "intervention upper bounds must be a numeric vector." =
      is.numeric(intervention_upper_bounds),
    "intervention upper bounds must have the same length as component names." =
      length(intervention_upper_bounds) == length(component_names),
    "intervention lower bounds must be less than intervention upper bounds." =
      all(intervention_lower_bounds < intervention_upper_bounds)
  )

  # When the user closes the app with the "Return list to R & close" button, the
  # quit observer saves the current cost list to `lago_cost_list` in the global
  # environment and sets `saved` to TRUE. This on.exit then tells the user where
  # it is. The flag (initialized locally here) is required so the message only
  # prints on that button-close path: it must NOT fire on an early error or on a
  # browser tab-close / Esc, where no cost list was produced.
  saved <- FALSE
  on.exit(
    if (saved) {
      message(
        "Your cost list has been saved to `lago_cost_list` in your global ",
        "environment.\n",
        "Use it with: ",
        "lago_optimization(..., cost_list_of_vectors = lago_cost_list)"
      )
    },
    add = TRUE
  )

  # Calculate the initial coefficients for the cost function
  initial_coefficients_list <- cost_fxn_calculator(
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    unit_costs = unit_costs,
    default_cost_fxn_type = default_cost_fxn_type
  )

  # Calculate the default slider range for every coefficient. Each coefficient
  # gets its own range scaled to its magnitude (see compute_slider_range), so
  # sliders are usable regardless of the coefficient's scale, instead of the
  # previous fixed -15..15 range. The range is derived from the initial
  # coefficients (which come from unit_costs, the bounds, and the cost function
  # type) and the component's unit cost. Users can override any slider's range
  # with the per-slider min/max inputs in the app.
  slider_ranges_list <- lapply(
    seq_along(initial_coefficients_list),
    function(component_idx) {
      lapply(
        initial_coefficients_list[[component_idx]],
        function(init) {
          compute_slider_range(init, unit_costs[component_idx])
        }
      )
    }
  )

  # Serve the vendored client-side assets (D3 v7 + the cost-curve binding) from
  # the installed package's inst/js directory under a URL prefix. This keeps the
  # curves client-side and offline (no CDN), as required for CRAN. Using
  # addResourcePath + tags$script avoids a new hard dependency on htmltools
  # (htmlDependency is not re-exported by shiny); addResourcePath and tags are
  # both provided by shiny, which is already imported.
  js_dir <- system.file("js", package = "LAGO")
  addResourcePath("lago_cost_assets", js_dir)

  ui <- navbarPage(
    title = "Cost Functions Visualization",
    theme = bs_theme(version = 5, bootswatch = "flatly"),

    # Load the vendored D3 first, then the cost-curve binding. Placed in the
    # document head via header = tags$head(...); tags$head content is hoisted to
    # <head> regardless of where it appears in the UI.
    header = tags$head(
      tags$script(src = "lago_cost_assets/d3.v7.min.js"),
      tags$script(src = "lago_cost_assets/cost-curves.js")
    ),

    # Include shinyjs
    useShinyjs(),

    # Add footer panel for cost function list
    # footer = div(
    #     style = "padding: 20px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
    #     h4("Using these cost functions in optimization"),
    #     p("If you are satisfied with the cost functions for all intervention components, use the following coefficient list when running lago_optimization():"),
    #     verbatimTextOutput("complete_coef_list"),
    #     p("Example usage: lago_optimization(..., cost_list_of_vectors = cost_list)")
    # ),
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
            span(
              id = "copy_confirmation",
              style = "color: #198754; margin-left: 10px; display: none;",
              "Copied!"
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
    !!!lapply(seq_along(initial_coefficients_list), function(component_idx) {
      nav_panel(
        paste("Component", component_idx, ":", component_names[component_idx]),
        fluidRow(
          column(
            5,
            card(
              card_header("Adjust coefficients using sliders"),
              card_body(
                # Create sliders for the current component's coefficients.
                # Each slider has its own default range (compute_slider_range)
                # plus two numeric inputs so the user can set a custom range.
                lapply(
                  seq_along(initial_coefficients_list[[component_idx]]),
                  function(i) {
                    rng <- slider_ranges_list[[component_idx]][[i]]
                    tagList(
                      div(
                        style = paste(
                          "display: flex; gap: 10px; align-items: flex-end;",
                          "margin-bottom: 5px;"
                        ),
                        numericInput(
                          inputId = paste0(
                            "range_min_", component_idx, "_", i - 1
                          ),
                          label = "Range min",
                          value = rng$min,
                          width = "120px"
                        ),
                        numericInput(
                          inputId = paste0(
                            "range_max_", component_idx, "_", i - 1
                          ),
                          label = "Range max",
                          value = rng$max,
                          width = "120px"
                        )
                      ),
                      sliderInput(
                        inputId = paste0("coef_", component_idx, "_", i - 1),
                        label = paste0(
                          component_names[component_idx], "^", i - 1,
                          " coefficient"
                        ),
                        min = rng$min,
                        max = rng$max,
                        value = initial_coefficients_list[[component_idx]][i],
                        step = rng$step,
                        width = "100%"
                      )
                    )
                  }
                ),
                hr(),
                # Add reset button
                actionButton(
                  inputId = paste0("reset_", component_idx),
                  label = "Reset Coefficients",
                  class = "btn-warning"
                ),
                hr(),
                h4(paste("Coefficient vector for", component_names[component_idx], ":")),
                # Add warning message below coefficient vector
                div(
                  id = paste0("coef_warning_", component_idx),
                  style = "color: red; margin-top: 10px; display: none;",
                  "Warning: Current coefficients may lead to negative marginal costs!"
                ),
                verbatimTextOutput(paste0("coefficient_text_", component_idx))
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
                  id = paste0("plot_warning_", component_idx),
                  style = "color: red; margin-bottom: 5px; display: none;",
                  "Warning: Total cost function should be non-decreasing!"
                ),
                div(
                  id = paste0("negative_cost_warning_", component_idx),
                  style = "color: red; margin-bottom: 10px; display: none;",
                  "Warning: Marginal cost function should always be positive!"
                ),
                # Client-side D3 (v7) target. cost-curves.js reads the coefs
                # straight from this component's sliders and the bounds / unit
                # cost from these data-* attributes, then renders both SVGs and
                # redraws instantly as the sliders move (no server round-trip).
                # The right endpoint of the total-cost curve is draggable; the
                # drag writes rescaled coefficients back via
                # input$dragged_coefs_<component> (see the observeEvent below).
                div(
                  id = paste0("cost_curves_", component_idx),
                  class = "lago-cost-curves",
                  `data-component` = component_idx,
                  `data-ncoef` =
                    length(initial_coefficients_list[[component_idx]]),
                  `data-lb` = intervention_lower_bounds[component_idx],
                  `data-ub` = intervention_upper_bounds[component_idx],
                  `data-unit-cost` = unit_costs[component_idx],
                  `data-name` = component_names[component_idx]
                ),
                hr(),
                # Key numeric values over the intervention range, so the user
                # can target a known cost rather than only eyeballing the curve.
                verbatimTextOutput(paste0("cost_summary_", component_idx))
              )
            )
          )
        )
      )
    })
  )

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
    # Remove the resource-path prefix registered above when the app stops, so
    # relaunching does not leak "lago_cost_assets" process-globally across
    # launches. The addResourcePath above re-runs on the next launch.
    onStop(function() removeResourcePath("lago_cost_assets"))

    # Add function to check if cost function is non-decreasing
    is_non_decreasing <- function(x_vals, y_vals) {
      all(diff(y_vals) >= -1e-10) # Using small tolerance for numerical stability
    }

    # Create a reactiveValues object to store the initial coefficients
    rv <- reactiveValues(
      initial_coefs = initial_coefficients_list
    )

    # Current coefficients for all components, as a list of numeric vectors.
    # Shared by the coefficient-list text, the copy button, and the value
    # returned to R when the app closes.
    current_cost_list <- reactive({
      lapply(seq_along(initial_coefficients_list), function(component_idx) {
        sapply(
          seq_along(initial_coefficients_list[[component_idx]]),
          function(i) {
            input[[paste0("coef_", component_idx, "_", i - 1)]]
          }
        )
      })
    })

    # The `cost_list <- list(...)` snippet, ready to paste into R.
    cost_list_code <- reactive({
      coef_strings <- sapply(current_cost_list(), function(coefs) {
        paste0("c(", paste(format_coef(coefs), collapse = ", "), ")")
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

    # Create reactive expressions and outputs for each component
    lapply(seq_along(initial_coefficients_list), function(component_idx) {
      # Add observer for reset button
      observeEvent(input[[paste0("reset_", component_idx)]], {
        # Reset each slider to its initial value AND its default range from rv
        lapply(
          seq_along(rv$initial_coefs[[component_idx]]),
          function(i) {
            rng <- slider_ranges_list[[component_idx]][[i]]
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
            updateSliderInput(
              session,
              inputId = paste0("coef_", component_idx, "_", i - 1),
              value = rv$initial_coefs[[component_idx]][i],
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
      # the slider.
      lapply(
        seq_along(initial_coefficients_list[[component_idx]]),
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
              updateSliderInput(
                session,
                inputId = paste0("coef_", component_idx, "_", i - 1),
                min = rmin,
                max = rmax,
                step = min((rmax - rmin) / 1000, 0.00001)
              )
            },
            ignoreInit = TRUE
          )
        }
      )

      # Coefficient text output
      output[[paste0("coefficient_text_", component_idx)]] <- renderText({
        current_coefs <- sapply(
          seq_along(initial_coefficients_list[[component_idx]]),
          function(i) {
            input[[paste0("coef_", component_idx, "_", i - 1)]]
          }
        )
        paste0("c(", paste(format_coef(current_coefs), collapse = ", "), ")")
      })

      # Add reactive expression for cost function validation
      observe({
        current_coefs <- sapply(
          seq_along(initial_coefficients_list[[component_idx]]),
          function(i) {
            input[[paste0("coef_", component_idx, "_", i - 1)]]
          }
        )

        x_vals <- seq(
          intervention_lower_bounds[component_idx],
          intervention_upper_bounds[component_idx],
          length.out = 2000
        )
        y_vals <- calculate_cost(current_coefs, x_vals)

        # Check if cost function is non-decreasing
        is_non_decreasing_valid <- is_non_decreasing(x_vals, y_vals)

        # Check if cost function is always positive
        is_positive_valid <- all(y_vals >= -1e-10) # Using small tolerance for numerical stability

        # Update warnings based on validation results
        if (!is_non_decreasing_valid) {
          shinyjs::show(paste0("plot_warning_", component_idx))
          shinyjs::show(paste0("coef_warning_", component_idx))
        } else {
          shinyjs::hide(paste0("plot_warning_", component_idx))
          shinyjs::hide(paste0("coef_warning_", component_idx))
        }

        if (!is_positive_valid) {
          shinyjs::show(paste0("negative_cost_warning_", component_idx))
        } else {
          shinyjs::hide(paste0("negative_cost_warning_", component_idx))
        }

        # Briefly outline this component's visualization card in red when a
        # coefficient choice is invalid. This replaces a full-page background
        # flash, which fired the whole body on every reactive tick while
        # dragging through an invalid region and was jarring; the per-card
        # outline plus the inline warnings convey the problem without it.
        if (!is_non_decreasing_valid || !is_positive_valid) {
          runjs(sprintf(
            'var w = document.getElementById("plot_warning_%1$s");
             var card = w ? w.closest(".card") : null;
             if (card) {
               card.style.transition = "box-shadow 0.4s";
               card.style.boxShadow = "0 0 0 3px rgba(220,53,69,0.6)";
               setTimeout(function(){ card.style.boxShadow = "none"; }, 600);
             }',
            component_idx
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
          if (length(new_coefs) !=
            length(initial_coefficients_list[[component_idx]]) ||
            any(!is.finite(new_coefs))) {
            return()
          }
          lapply(seq_along(new_coefs), function(i) {
            val <- new_coefs[i]
            cur_min <- input[[paste0("range_min_", component_idx, "_", i - 1)]]
            cur_max <- input[[paste0("range_max_", component_idx, "_", i - 1)]]
            # Expand the range symmetrically with a small margin if the new
            # value would be clamped. Reuse the range-input mechanism so the
            # displayed Range min / Range max stay consistent with the slider.
            new_min <- cur_min
            new_max <- cur_max
            if (is.null(new_min) || !is.finite(new_min) || val < new_min) {
              new_min <- floor((val - abs(val) * 0.5 - 1) * 1000) / 1000
            }
            if (is.null(new_max) || !is.finite(new_max) || val > new_max) {
              new_max <- ceiling((val + abs(val) * 0.5 + 1) * 1000) / 1000
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
            updateSliderInput(
              session,
              inputId = paste0("coef_", component_idx, "_", i - 1),
              value = val,
              min = new_min,
              max = new_max,
              step = min((new_max - new_min) / 1000, 0.00001)
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
          seq_along(initial_coefficients_list[[component_idx]]),
          function(i) {
            input[[paste0("coef_", component_idx, "_", i - 1)]]
          }
        )
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

    # Closing the app returns the current cost list to R, so the result can be
    # captured (e.g. cost_list <- visualize_cost(...)) instead of only copied.
    observeEvent(input$quit_button, {
      cl <- current_cost_list()
      # save into the global environment so the list is available even when the
      # app was launched with a bare visualize_cost(...) call (no assignment).
      # This overwrites any existing `lago_cost_list`; the on.exit message
      # announces it. `saved` gates that message (see the on.exit near the top).
      assign("lago_cost_list", cl, envir = globalenv())
      saved <<- TRUE
      stopApp(cl)
    })
  }

  # Run the app rather than only returning the app object, so that calling
  # visualize_cost(...) launches it AND the value passed to stopApp() (the
  # current cost list) is returned to the caller, i.e.
  # cost_list <- visualize_cost(...). invisible() keeps a bare call from
  # auto-printing the whole list to the console on close.
  invisible(shiny::runApp(shinyApp(ui, server)))
}

# Compute a default slider range for a single cost-function coefficient.
# The cost-function coefficients span very different scales (for a cubic cost
# the leading coefficient may be ~1 while the highest-order term is ~0.005),
# so a single fixed slider range is not usable for all of them. This helper
# returns a range centered on the coefficient's initial value and scaled to
# its magnitude, with a floor derived from the component's unit cost so that a
# zero-valued coefficient is still adjustable. Returns a list(min, max, step).
compute_slider_range <- function(init, unit_cost, k = 5) {
  # half-width is k times the coefficient magnitude, but never smaller than a
  # floor based on the unit cost (falling back to 1 when the unit cost is 0),
  # so a coefficient whose initial value is 0 still has room to move.
  floor_hw <- max(abs(unit_cost), 1)
  half_width <- max(k * abs(init), floor_hw)
  # round the bounds for display: this keeps the auto-generated slider tick
  # labels and the pre-filled range inputs readable (e.g. -10.863 instead of
  # -10.8629818181818). Round outward so the initial value stays within range.
  lower <- floor((init - half_width) * 1000) / 1000
  upper <- ceiling((init + half_width) * 1000) / 1000
  # step: keep the historical fine 0.00001 step for the usual (wide) ranges,
  # and only go finer than that for a very narrow range (width < 0.01), so the
  # slider can still resolve values within it.
  step <- min((upper - lower) / 1000, 0.00001)
  list(min = lower, max = upper, step = step)
}

# Format a number for display in the app: 3 decimal places by default, but
# more places (up to 5) for small-magnitude values so that roughly 3
# significant figures are preserved. Trailing zeros are dropped. This keeps
# slider labels, range inputs, and the coefficient list readable instead of
# showing values like -10.8629818181818 or 0.000000000.
format_coef <- function(x) {
  vapply(x, function(v) {
    if (!is.finite(v)) {
      return(as.character(v))
    }
    if (v == 0) {
      return("0")
    }
    # for |v| >= 1, use 3 decimals; for smaller values, add places so ~3
    # significant figures survive, capped at 5 decimals.
    mag <- floor(log10(abs(v)))
    digits <- min(max(3, 2 - mag), 5)
    formatC(round(v, digits),
      format = "f", digits = digits, drop0trailing = TRUE
    )
  }, character(1))
}
