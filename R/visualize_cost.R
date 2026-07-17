#' visualize_cost
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
#' @import bslib shiny ggplot2
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
#' @return NULL
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

  ui <- navbarPage(
    title = "Cost Functions Visualization",
    theme = bs_theme(version = 5, bootswatch = "flatly"),

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
          p("Example usage: lago_optimization(..., cost_list_of_vectors = cost_list)")
        ),
        column(
          4, # Takes up 4/12 of the width for the quit button
          div(
            style = "text-align: right; padding-top: 20px;",
            actionButton(
              inputId = "quit_button",
              label = "Quit App",
              class = "btn-danger",
              icon = icon("times-circle")
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
                plotOutput(paste0("costPlot_", component_idx)),
                plotOutput(paste0("derivativePlot_", component_idx))
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
    # Add function to check if cost function is non-decreasing
    is_non_decreasing <- function(x_vals, y_vals) {
      all(diff(y_vals) >= -1e-10) # Using small tolerance for numerical stability
    }

    # Create a reactiveValues object to store the initial coefficients
    rv <- reactiveValues(
      initial_coefs = initial_coefficients_list
    )

    # Create output for complete coefficient list
    output$complete_coef_list <- renderText({
      # Get current coefficients for all components
      current_coefs_all <- lapply(seq_along(initial_coefficients_list), function(component_idx) {
        coefs <- sapply(
          seq_along(initial_coefficients_list[[component_idx]]),
          function(i) {
            input[[paste0("coef_", component_idx, "_", i - 1)]]
          }
        )
        return(coefs)
      })

      # Format as R list
      coef_strings <- sapply(current_coefs_all, function(coefs) {
        paste0("c(", paste(format_coef(coefs), collapse = ", "), ")")
      })

      paste0(
        "cost_list <- list(\n    ",
        paste(coef_strings, collapse = ",\n    "),
        "\n)"
      )
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

        # Flash screen red if either condition is invalid
        if (!is_non_decreasing_valid || !is_positive_valid) {
          runjs(sprintf('
                        document.body.style.transition = "background-color 0.5s";
                        document.body.style.backgroundColor = "rgba(255,0,0,0.1)";
                        setTimeout(function() {
                            document.body.style.backgroundColor = "white";
                        }, 500);
                    '))
        }
      })

      observeEvent(input$quit_button, {
        stopApp()
      })

      output[[paste0("costPlot_", component_idx)]] <- renderPlot({
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

        ggplot(data.frame(x = x_vals, y = y_vals), aes(x = x, y = y)) +
          geom_line(color = "#0066cc", size = 1) +
          theme_minimal() +
          labs(
            title = paste("Total Cost Function -", component_names[component_idx]),
            x = component_names[component_idx],
            y = "Total Cost"
          ) +
          theme(text = element_text(size = 14))
      })

      output[[paste0("derivativePlot_", component_idx)]] <- renderPlot({
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
        y_vals <- calculate_derivative(current_coefs, x_vals)

        ggplot(data.frame(x = x_vals, y = y_vals), aes(x = x, y = y)) +
          geom_line(color = "#cc3300", size = 1) +
          # Add horizontal reference line for unit cost
          geom_hline(
            yintercept = unit_costs[component_idx],
            linetype = "dashed",
            color = "black",
            size = 0.8
          ) +
          theme_minimal() +
          labs(
            title = paste("Derivative of the Total Cost Function (Marginal Cost) -", component_names[component_idx]),
            x = component_names[component_idx],
            y = "Marginal Cost"
          ) +
          # Add annotation for the reference line
          annotate("text",
            x = max(x_vals),
            y = unit_costs[component_idx],
            label = sprintf("Unit Cost: %.2f", unit_costs[component_idx]),
            hjust = 1,
            vjust = -0.5
          ) +
          theme(text = element_text(size = 14))
      })
    })
  }

  shinyApp(ui, server)
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
