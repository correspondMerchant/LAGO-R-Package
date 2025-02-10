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
    model,
    rec_int,
    rec_int_cost,
    est_outcome_goal,
    include_confidence_set,
    cs) {
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

    cat("\n=====================================")
    cat("\n============  Model Fit  ============\n")
    cat("=====================================\n")
    print(summary(model))

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
    cat("\nCost for using the recommended interventions:", rec_int_cost, "\n")
    cat(
        "Estimated outcome goal using the recommended interventions:",
        est_outcome_goal, "\n"
    )

    if (include_confidence_set) {
        cat("\n========================================")
        cat("\n============ Confidence Set ============\n")
        cat("========================================\n")
        cat(
            "Confidence set size percentage:",
            cs$confidence_set_size_percentage, "\n"
        )

        if (cs$confidence_set_size_percentage > 0) {
            cat("Confidence set (only first few rows are shown): \n")
            cat("Please use $cs to get the full confidence set. \n")
            print(head(cs$cs))
        } else {
            cat("No confidense set was found. \n")
        }
    }
}
