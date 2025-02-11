#' visualize_cost
#'
#' @description Visualize the cost functions for the intervention components.
#' This function creates a Shiny app that allows the user to adjust the
#' coefficients of the cost functions for each intervention component and
#' visualize the resulting total cost function and its derivative.
#' The initial coefficients are calculated based on the unit costs, the
#' default cost function type (linear or cubic), and the lower and upper bounds.
#' The user can adjust the coefficients using sliders and reset them to their
#' initial values. The app also displays the current coefficient vector for
#' each component.
#' The user can copy the final coefficient list for use in the optimization
#' function lago_optimization().
#'
#' ### required arguments:
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
#'
#' @examples
#' \dontrun{
#' visualize_cost(
#'     component_names = c("Component 1", "Component 2"),
#'     unit_costs = c(0.5, 1),
#'     default_cost_fxn_type = "linear",
#'     intervention_lower_bounds = c(0, 0),
#'     intervention_upper_bounds = c(10, 10)
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

    ui <- navbarPage(
        title = "Cost Functions Visualization",
        theme = bs_theme(version = 5, bootswatch = "flatly"),

        # Add footer panel for cost function list
        footer = div(
            style = "padding: 20px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
            h4("Using these cost functions in optimization"),
            p("If you are satisfied with the cost functions for all intervention components, use the following coefficient list when running lago_optimization():"),
            verbatimTextOutput("complete_coef_list"),
            p("Example usage: lago_optimization(..., cost_list_of_vectors = cost_list)")
        ),

        # Create a nav_panel for each component
        !!!lapply(seq_along(initial_coefficients_list), function(component_idx) {
            nav_panel(
                paste("Component", component_idx, ":", component_names[component_idx]),
                fluidRow(
                    column(
                        5,
                        card(
                            card_header("Adjust Coefficients"),
                            card_body(
                                # Create sliders for the current component's coefficients
                                h4("Adjust coefficients using sliders:"),
                                lapply(
                                    seq_along(initial_coefficients_list[[component_idx]]),
                                    function(i) {
                                        sliderInput(
                                            inputId = paste0("coef_", component_idx, "_", i - 1),
                                            label = paste0(component_names[component_idx], "^", i - 1, " coefficient"),
                                            min = -15,
                                            max = 15,
                                            value = initial_coefficients_list[[component_idx]][i],
                                            step = 0.00001,
                                            width = "100%"
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
                                h4("Coefficient vector for x:"),
                                verbatimTextOutput(paste0("coefficient_text_", component_idx))
                            )
                        )
                    ),
                    column(
                        7,
                        card(
                            card_header("Visualization"),
                            card_body(
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
                paste0("c(", paste(format(coefs, digits = 8), collapse = ", "), ")")
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
                # Reset each slider to its initial value from rv
                lapply(
                    seq_along(rv$initial_coefs[[component_idx]]),
                    function(i) {
                        updateSliderInput(
                            session,
                            inputId = paste0("coef_", component_idx, "_", i - 1),
                            value = rv$initial_coefs[[component_idx]][i]
                        )
                    }
                )
            })

            # Coefficient text output
            output[[paste0("coefficient_text_", component_idx)]] <- renderText({
                current_coefs <- sapply(
                    seq_along(initial_coefficients_list[[component_idx]]),
                    function(i) {
                        input[[paste0("coef_", component_idx, "_", i - 1)]]
                    }
                )
                paste0("c(", paste(current_coefs, collapse = ", "), ")")
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
                        title = paste("Derivative of Cost Function (Unit Cost Function) -", component_names[component_idx]),
                        x = component_names[component_idx],
                        y = "Unit Cost"
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
