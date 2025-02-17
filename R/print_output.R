print_output <- function(
    data,
    outcome_name,
    outcome_type,
    intervention_components,
    include_interaction_terms,
    main_components,
    center_characteristics,
    family_object,
    include_center_effects,
    include_time_effects,
    outcome_goal,
    cost_list_of_vectors,
    intervention_lower_bounds,
    intervention_upper_bounds,
    model,
    rec_int,
    rec_int_cost,
    est_outcome_goal,
    include_confidence_set,
    cs,
    test_results) {
    # input errors are very common, we show some of the key user inputs so
    # users can double check their inputs
    cat("\n==================================")
    cat("\n============  Inputs  ============\n")
    cat("==================================\n")
    cat(
        "Input data dimensions:", dim(data)[1], "rows, and",
        dim(data)[2], "columns \n"
    )
    cat("Outcome name:", outcome_name, "\n")
    cat("Outcome type:", outcome_type, "\n")
    cat(
        length(intervention_components),
        "intervention package component(s): \n",
        paste("\t", intervention_components, collapse = "\n"),
        "\n"
    )
    if (include_interaction_terms) {
        cat(
            length(main_components),
            "main effect component(s): \n",
            paste("\t", main_components, collapse = "\n"),
            "\n"
        )
    }
    if (!is.null(center_characteristics)) {
        cat(
            length(center_characteristics),
            "center characteristic(s):", center_characteristics, "\n"
        )
    }
    cat("The outcome model: \n")
    cat("\t family:", family_object$family, "\n")
    cat("\t link:", family_object$link, "\n")
    cat("\t fixed center effects:", include_center_effects, "\n")
    cat("\t fixed time effects:", include_time_effects, "\n")
    cat("Outcome goal:", outcome_goal, "\n")
    cat(
        "List of intervention component costs:",
        toString(cost_list_of_vectors), "\n"
    )
    cat(
        "Intervention lower bounds:",
        intervention_lower_bounds, "\n"
    )
    cat(
        "Intervention upper bounds:",
        intervention_upper_bounds, "\n"
    )

    cat("\n=====================================")
    cat("\n============  Model Fit  ============\n")
    cat("=====================================\n")
    print(summary(model))
    if (!is.null(test_results)) {
        cat("\n")
        cat("Overall intervention test results: \n")
        if (outcome_type == "binary") {
            cat(
                "Two-sample T-test for the difference in two proportions: \n",
                "Test statistic:", test_results$test_stat, "\n",
                "P-value:", test_results$p_val, "\n"
            )
        }
    } else {
        cat(
            "\nTo see the overall test results,",
            "please include a 'group' column in the data,\n",
            "and make sure the values of the 'group' column are either",
            "'treatment' or 'control'.\n",
            "(Only binary outcomes are supported for now)\n"
        )
    }

    cat("\n===================================================")
    cat("\n===========  Recommended Interventions  ===========\n")
    cat("===================================================\n")
    rec_int_df <- data.frame(
        component = if (include_interaction_terms) {
            main_components
        } else {
            intervention_components
        },
        value = rec_int
    )
    print(rec_int_df, row.names = FALSE)
    cat(
        "Estimated outcome goal using the recommended interventions:",
        est_outcome_goal, "\n"
    )
    cat(
        "95% confidence interval for the estimated outcome goal:",
        if (include_confidence_set) {
            paste(cs$cs[1, ]$CI_lower_bound, "-", cs$cs[1, ]$CI_upper_bound)
        } else {
            "Not available, please set include_confidence_set = TRUE"
        }, "\n"
    )
    cat("\nCost for using the recommended interventions:", rec_int_cost, "\n")

    if (include_confidence_set) {
        cat("\n========================================")
        cat("\n============ Confidence Set ============\n")
        cat("========================================\n")
        if (round(cs$confidence_set_size_percentage, 2) == 0) {
            cat(
                "\nNo confidence set was found for the current outcome goal.\n"
            )
        } else {
            cat(
                "Confidence set size percentage:",
                cs$confidence_set_size_percentage, "\n"
            )
            lo_percentile <- quantile(cs$cs[-1, ]$cost, 0.25)
            hi_percentile <- quantile(cs$cs[-1, ]$cost, 0.75)
            cat(
                "IQR of the cost within the 95% confidence set:",
                lo_percentile, "-", hi_percentile, "\n\n"
            )

            if (cs$confidence_set_size_percentage > 0) {
                cat("Confidence set (only first few rows are shown): \n")
                cat("Please use $cs to get the full confidence set. \n")
                print(head(cs$cs[-1, ]))
            } else {
                cat("No confidense set was found. \n")
            }
        }
    }
}
