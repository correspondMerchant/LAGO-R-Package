validate_inputs <- function(
    data,
    input_data_structure = "individual_level",
    outcome_name,
    outcome_type,
    intervention_components,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal,
    unit_costs = NULL,
    default_cost_fxn_type = "cubic",
    cost_list_of_vectors = NULL,
    glm_family = "default",
    link = "default",
    optimization_method = "numerical",
    confidence_set_alpha = 0.05,
    weights = NULL,
    center_characteristics = NULL,
    center_characteristics_optimization_values = NULL,
    main_components = NULL,
    time_effect_optimization_value = NULL,
    additional_covariates = NULL,
    center_weights_for_outcome_goal = NULL,
    optimization_grid_search_step_size = NULL,
    confidence_set_grid_step_size = NULL,
    include_confidence_set = TRUE,
    include_center_effects = FALSE,
    center_effects_optimization_values = NULL,
    include_time_effects = FALSE,
    include_interaction_terms = FALSE) {
    # check if the input data is null
    if (is.null(data)) {
        stop("The argument 'data' is NULL.")
    }
    # check if the input data is a data frame type
    if (!is.data.frame(data)) {
        stop("The argument 'data' must be a data frame.")
    }
    # check if the input data is empty
    if (nrow(data) == 0 || ncol(data) == 0) {
        stop("The argument 'data' is empty.")
    }

    # check if the input data structure is a character type
    if (!is.character(input_data_structure)) {
        stop("The input_data_structure must be a character string.")
    }
    # check if the input data structure is supported
    if (!(input_data_structure %in% c("individual_level", "center_level"))) {
        stop(paste(
            "\ninput_data_structure:", input_data_structure,
            "\ninput_data_structure must be either 'individual_level'",
            "or 'center_level'."
        ))
    }

    # check if the outcome name is a character type
    if (!is.character(outcome_name)) {
        stop("The outcome name must be a character string.")
    }
    # check if the outcome name is empty
    if (nchar(outcome_name) == 0) {
        stop("The outcome name is empty.")
    }
    # check if the outcome name is one of the columns in the data frame
    if (!(outcome_name %in% names(data))) {
        stop("The outcome name must be presented in the provided input data.")
    }

    # check if the outcome type is a character type
    if (!is.character(outcome_type)) {
        stop("The outcome type must be a character string.")
    }
    # check if the outcome type is either continuous or binary
    allowed_outcome_types <- c("continuous", "binary")
    if (!(outcome_type %in% allowed_outcome_types)) {
        stop(paste(
            "Outcome type", outcome_type,
            "must be either 'continuous' or 'binary'."
        ))
    }
    # check if the input data structure is center_level when
    # the outcome type is continuous.
    if (input_data_structure == "center_level") {
        if (outcome_type == "continuous") {
            stop(paste(
                "For continuous outcomes, LAGO requires individual",
                "level data for the optimization step."
            ))
        }
    }

    # check if the input data has all the required columns
    required_columns <- c()
    if (input_data_structure == "center_level") {
        required_columns <- c(
            required_columns,
            "proportion",
            "center_sample_size"
        )
    }

    missing_columns <- setdiff(required_columns, names(data))
    if (length(missing_columns) > 0) {
        stop(sprintf(
            "The following required columns are missing: %s",
            paste(missing_columns, collapse = ", ")
        ))
    }

    # check if the glm family is a character type
    if (!is.character(glm_family)) {
        stop("The glm family must be a character string.")
    }
    # assign default glm_family based on the outcome type
    if (glm_family == "default") {
        if (outcome_type == "continuous") {
            glm_family <- "gaussian"
        } else if (outcome_type == "binary") {
            glm_family <- "binomial"
        }
    }

    # check if the link option is a character type
    if (!is.character(link)) {
        stop("The link option must be a character string.")
    }
    # check if the provided link option is supported
    supported_link_options <- c("logit", "probit", "identity", "log", "default")
    if (!(link %in% supported_link_options)) {
        stop(paste(
            "link=", link, ".",
            paste0(
                "The link option has to be one of ",
                "the following: logit, probit, identity, and log."
            )
        ))
    }
    # assign link based on the glm_family
    if (link == "default") {
        if (glm_family == "binomial") {
            link <- "logit"
        } else if (glm_family == "gaussian") {
            link <- "identity"
        }
    }

    # check if the provided weights option is numeric
    if (!is.null(weights)) {
        if (!is.numeric(weights)) {
            stop("The 'weights' option must be a numeric vector.")
        }
        # check if the provided weights option has the same length as the number of
        # observations
        if (length(weights) != length(data[, intervention_components[1]])) {
            stop(paste0(
                "The length of the weights must be the same as the ",
                "number of observations in the provided input data."
            ))
        }
    }

    # check if intervention_components is a character vector type
    if (!(is.vector(intervention_components) &&
        is.character(intervention_components))) {
        stop("Interventions list must be a character vector.")
    }
    # check if intervention_components are all columns in the data frame
    if (!all(intervention_components %in% names(data))) {
        stop(paste0(
            "All elements in intervention_components ",
            "must be columns in the data frame."
        ))
    }

    # check if center_characteristics is a character vector type
    if (length(center_characteristics) > 0) {
        if (!(is.vector(center_characteristics) &&
            is.character(center_characteristics))) {
            stop("center_characteristics must be a character vector.")
        }
        # check if center_characteristics are all columns in the data frame
        if (!all(center_characteristics %in% names(data))) {
            stop(paste0(
                "All elements in center_characteristics ",
                "must be columns in the data frame."
            ))
        }
        # check if center_characteristics_optimization_values
        # is a numeric vector type
        if (!(is.vector(center_characteristics_optimization_values) &&
            is.numeric(center_characteristics_optimization_values))) {
            stop(paste0(
                "center_characteristics_optimization_values ",
                "must be a numeric vector."
            ))
        }
        # check if center_characteristics_optimization_values is not null
        if (is.null(center_characteristics_optimization_values)) {
            stop(paste0(
                "center_characteristics_optimization_values is NULL. ",
                "You decided to include center characteristics in ",
                "the model, please either provide values of the ",
                "center characteristics for LAGO optimization, ",
                "or consider dropping the center characteristics."
            ))
        }
        # check if length of center_characteristics_optimization_values is the same
        # as the length of center_characteristics
        if (length(center_characteristics) !=
            length(center_characteristics_optimization_values)) {
            stop(paste(
                "The length of center_characteristics must be the",
                "same as the length of",
                "center_characteristics_optimization_values."
            ))
        }
    }

    # check whether the include_center_effects indicator is boolean
    if (!is.logical(include_center_effects)) {
        stop("The include_center_effects indicator must be a boolean.")
    }
    # if include_center_effects is not specified, set the default value
    if (input_data_structure == "center_level" && !include_center_effects) {
        include_center_effects <- TRUE
    }

    # check whether center_characteristics and center effects are both included
    if (include_center_effects && !is.null(center_characteristics)) {
        stop(paste(
            "Fixed center effects and center characteristics cannot be",
            "both included in the outcome model. Please",
            "pick one to use."
        ))
    }

    # check if the input data has the "center" column if
    # include_center_effects is TRUE
    if (include_center_effects) {
        if (!"center" %in% names(data)) {
            stop(paste(
                "To include fixed center effects in the outcome model",
                "please make sure that the input data has a 'center'",
                "column to identify facilities."
            ))
        }
        # if "center" column of the input data is not a factor,
        # change it to factor and print a warning message
        if (!is.factor(data$center)) {
            data$center <- as.factor(data$center)
            message(paste(
                "'center' column is not a factor type. To ensure",
                "the correct model fit, it has been converted",
                "to the factor type."
            ))
        }
    }

    # preliminary check for center_weights_for_outcome_goal
    if (!is.null(center_weights_for_outcome_goal)) {
        # check if include center effects is set to FALSE
        if (!include_center_effects) {
            stop(paste(
                "'include_center_effects' must be set to TRUE,",
                "for values of 'center_weights_for_outcome_goal'",
                "to be used."
            ))
        }
        # check if center_weights_for_outcome_goal is a numeric vector
        if (!is.numeric(center_weights_for_outcome_goal)) {
            stop(paste("center_weights_for_outcome_goal must be a numeric vector."))
        }
        # check if center_weights_for_outcome_goal has the same length as the
        # number of facilities provided in the input data
        n_facilities <- length(unique(data$center))
        if (length(center_weights_for_outcome_goal) != n_facilities) {
            stop(paste(
                "The length of center_weights_for_outcome_goal must match",
                "the number of unique facilities in the input data."
            ))
        }
    } else {
        # if center_weights_for_outcome_goal is not specified, but
        # include_center_effects is set to TRUE, we calculate its default values
        if (include_center_effects) {
            # if not specifing center_effects_optimization_values,
            # then we calculate the default values for center weights
            if (is.null(center_effects_optimization_values)) {
                if (input_data_structure == "center_level") {
                    center_sizes <- tapply(
                        data$center_sample_size,
                        data$center,
                        function(x) x[1]
                    )
                    total_sample_size <- sum(center_sizes)
                } else if (input_data_structure == "individual_level") {
                    total_sample_size <- sum(as.numeric(table(data$center)))
                    center_sizes <- as.numeric(table(data$center))
                }
                center_weights_for_outcome_goal <- center_sizes / total_sample_size
            } else {
                # if center_effects_optimization_values is specified,
                # we set the weight for that specific center to 1, rest to 0
                if (!is.character(center_effects_optimization_values)) {
                    stop(paste(
                        "center_effects_optimization_values
                        must be a character."
                    ))
                }
                if (length(center_effects_optimization_values) != 1) {
                    stop(paste(
                        "center_effects_optimization_values
                        must be a single value."
                    ))
                }
                if (!center_effects_optimization_values %in%
                    unique(data$center)) {
                    stop(paste(
                        "center_effects_optimization_values
                        must be one of the centers."
                    ))
                }
                center_weights_for_outcome_goal <- ifelse(sort(unique(data$center)) ==
                    center_effects_optimization_values, 1, 0)
            }
        } else {
            center_weights_for_outcome_goal <- 1
        }
    }

    # check if values of center_weights_for_outcome_goal sum up to 1
    if (!is.null(center_weights_for_outcome_goal)) {
        if (abs(sum(center_weights_for_outcome_goal) - 1) >= 0.001) {
            stop(paste(
                "values in center_weights_for_outcome_goal must",
                "sum up to 1."
            ))
        }
    }

    # check if include_time_effects is logical
    if (!is.logical(include_time_effects)) {
        stop(paste("'include_time_effects' must be a boolean."))
    }
    family_object <- switch(glm_family,
        "binomial" = binomial(link = link),
        "gaussian" = gaussian(link = link),
        "quasibinomial" = quasibinomial(link = link),
        stop(paste("Unsupported glm_family:", glm_family))
    )


    # if include_time_effects is set to TRUE
    if (include_time_effects) {
        # the input data must have a 'period' column
        if (!"period" %in% names(data)) {
            stop(paste(
                "To include fixed time effects in the outcome model,",
                "please make sure that the input data has a 'period'",
                "column to identify time period."
            ))
        }
        # the time_effect_optimization_value must be provided, one numeric number
        if (is.null(time_effect_optimization_value) ||
            !is.numeric(time_effect_optimization_value) ||
            length(time_effect_optimization_value) != 1) {
            stop(paste(
                "'include_time_effects' is set to TRUE,",
                "'time_effect_optimization_value' must be provided,",
                "and it must be a numeric value."
            ))
        }
        # if "period" column of the input data is not a factor,
        # change it to factor and print a warning message
        if (!is.factor(data$period)) {
            data$period <- as.factor(data$period)
            message(paste(
                "'period' column is not a factor type. To ensure",
                "the correct model fit, it has been converted",
                "to the factor type."
            ))
        }
    }

    # if include_interaction_terms is set to TRUE
    if (include_interaction_terms) {
        # check if interaction terms are included in intervention_components
        # and follow the correct naming scheme
        if (!any(grepl(":", intervention_components, fixed = TRUE))) {
            stop(paste(
                "Please make sure the interaction terms are included",
                "as part of the 'intervention_components', and the interaciton terms",
                "need to have the format of 'X1:X5' "
            ))
        }

        # if so, check if main_components is null
        if (is.null(main_components)) {
            stop(paste(
                "'main_components' must be defined as a character vector",
                "if interaction terms are",
                "included in 'intervention_components'."
            ))
        }
        # check if main_components is a character vector
        if (!is.character(main_components)) {
            stop(paste("'main_components' must be a character vector."))
        }

        # if not null, check if main_components have the correct naming scheme
        if (any(grepl(":", main_components, fixed = TRUE))) {
            stop(paste(
                "Interaction terms should be included as part of the",
                "'intervention_components', not part of the",
                "'main_components'."
            ))
        }

        # check if main_components are all columns in the data frame
        # need this to calculate the step size if the step size is not specified.
        if (optimization_method == "grid_search" &&
            is.null(optimization_grid_search_step_size)) {
            if (!all(main_components %in% names(data))) {
                stop(paste(
                    "When 'optimization_method' is set to 'grid_search',",
                    "and optimization_grid_search_step_size is not specified,",
                    "all elements in the main_components",
                    "must be columns in the data frame for auto calculating",
                    "the step size."
                ))
            }
        }

        # check if interaction terms can be found in the main_components
        check_term <- function(term) {
            if (grepl(":", term, fixed = TRUE)) {
                parts <- unlist(strsplit(term, ":", fixed = TRUE))
                return(all(parts %in% main_components))
            } else {
                return(term %in% main_components)
            }
        }
        interaction_check_results <- sapply(intervention_components, check_term)
        if (!all(interaction_check_results)) {
            stop(paste(
                "Each component of 'intervention_components' including",
                "interaction terms (separated by ':')",
                "must be present in 'main_components'."
            ))
        }

        # add backticks to interactions terms in intervention components
        # for glm model fitting
        needs_backticks <- grepl(":", intervention_components)
        intervention_components[needs_backticks] <- paste0(
            "`", intervention_components[needs_backticks], "`"
        )
    }

    # check if additional_covariates is a character vector
    if (length(additional_covariates) > 0) {
        if (!(is.vector(additional_covariates) &&
            is.character(additional_covariates))) {
            stop("additional_covariates must be a character vector.")
        }
        # check if additional_covariates are all columns in the data frame
        if (!all(additional_covariates %in% names(data))) {
            stop(paste0(
                "All elements in additional_covariates ",
                "must be columns in the data frame."
            ))
        }
    }

    # check if intervention_lower_bounds and intervention_upper_bounds are both
    # numerical vectors
    if (!(is.vector(intervention_lower_bounds) &&
        is.numeric(intervention_lower_bounds))) {
        stop("intervention_lower_bounds must be a numeric vector.")
    }
    if (!(is.vector(intervention_upper_bounds) &&
        is.numeric(intervention_upper_bounds))) {
        stop("intervention_upper_bounds must be a numeric vector.")
    }

    # check if the dimension of intervention_lower_bounds matches
    # the dimension of intervention_components or the dimension
    # of main_components
    if (
        (!include_interaction_terms &&
            (length(intervention_lower_bounds) != length(intervention_components))) ||
            (include_interaction_terms &&
                (length(intervention_lower_bounds) != length(main_components)))
    ) {
        stop(paste(
            "Without interaction terms,",
            "the lengths of 'intervention_lower_bounds' and",
            "'intervention_components' must be the same.",
            "With interaction terms,",
            "the lengths of 'intervention_lower_bounds' and",
            "'main_components' must be the same."
        ))
    }

    # check if lower bounds list and upper bounds list have the same length,
    # and if the lower bounds are <= upper bounds respectively
    if (length(intervention_lower_bounds) != length(intervention_upper_bounds)) {
        stop("The lengths of lower and upper bounds must be the same.")
    }
    if (any(intervention_lower_bounds < 0)) {
        stop("The intervention must have non-negative values only.")
    }
    invalid_indices <- which(intervention_upper_bounds <
        intervention_lower_bounds)
    if (length(invalid_indices) > 0) {
        stop(paste(
            "Invalid bounds at position(s):",
            paste(invalid_indices, collapse = ", "),
            paste0(
                "\nUpper bounds for the interventions must be greater ",
                "than or equal to the lower bounds."
            )
        ))
    }

    # check if unit_costs is valid
    if (!is.null(unit_costs)) {
        # check if unit_costs is a numeric vector
        if (!is.numeric(unit_costs)) {
            stop("unit_costs must be a numeric vector.")
        }
        # check if the dimension of unit_costs matches
        # the dimension of intervention_components or the dimension
        # of main_components
        if (
            (!include_interaction_terms &&
                (length(unit_costs) != length(intervention_components))) ||
                (include_interaction_terms &&
                    (length(unit_costs) != length(main_components)))
        ) {
            stop(paste(
                "Without interaction terms,",
                "the lengths of 'unit_costs' and",
                "'intervention_components' must be the same.",
                "With interaction terms,",
                "the lengths of 'unit_costs' and",
                "'main_components' must be the same."
            ))
        }
        # check if default_cost_fxn_type is valid
        if (!is.character(default_cost_fxn_type)) {
            stop("default_cost_fxn_type must be a character string.")
        }
        # check if default_cost_fxn_type is one of the supported types
        supported_cost_fxn_types <- c("linear", "cubic")
        if (!(default_cost_fxn_type %in% supported_cost_fxn_types)) {
            stop(sprintf(
                "default_cost_fxn_type must be one of the supported types. The supported types are: %s.",
                paste(supported_cost_fxn_types, collapse = ", ")
            ))
        }
    }

    # check if cost_list_of_vectors is valid
    if (!is.null(cost_list_of_vectors)) {
        # check if cost_list_of_vectors is a list,
        # and each sublist is a numeric vector
        if (!is.list(cost_list_of_vectors)) {
            stop("cost_list_of_vectors must be a list.")
        }
        # Check if all elements of sublists in cost_list_of_vectors are numeric
        all_numeric <- all(sapply(cost_list_of_vectors, function(sublist) {
            all(sapply(sublist, is.numeric))
        }))
        if (!all_numeric) {
            stop(paste(
                "All elements in the sublists of cost_list_of_vectors",
                "must be numeric."
            ))
        }
        # check if the dimension of cost_list_of_vectors matches
        # the dimension of intervention_components or the dimension
        # of main_components
        if (
            (!include_interaction_terms &&
                (length(cost_list_of_vectors) != length(intervention_components))) ||
                (include_interaction_terms &&
                    (length(cost_list_of_vectors) != length(main_components)))
        ) {
            stop(paste(
                "Without interaction terms,",
                "the lengths of 'cost_list_of_vectors' and",
                "'intervention_components' must be the same.",
                "With interaction terms,",
                "the lengths of 'cost_list_of_vectors' and",
                "'main_components' must be the same."
            ))
        }
        # check if default_cost_fxn_type is also provided
        if (!is.null(default_cost_fxn_type)) {
            print(paste(
                "When 'cost_list_of_vectors' is provided,",
                "'default_cost_fxn_type' is ignored."
            ))
        }
    }

    # check if unit_costs and cost_list_of_vectors are both null
    if (is.null(unit_costs) && is.null(cost_list_of_vectors)) {
        stop(paste(
            "Both 'unit_costs' and 'cost_list_of_vectors' are NULL.",
            "Please provide at least one of them."
        ))
    }

    # check if the outcome goal optimization method is a character type
    if (!is.character(optimization_method)) {
        stop("The optimization_method must be a character string.")
    }
    # check if the outcome goal optimization method is one of the defined methods
    optimization_method_list <- c("numerical", "grid_search")
    if (!(optimization_method %in% optimization_method_list)) {
        stop(paste(
            "optimization_method must be one of the supported methods.",
            "The supported methods are:", optimization_method_list, "."
        ))
    }

    # when the optimization_method is grid_search,
    # check the number of intervention components
    if (optimization_method == "grid_search") {
        if (length(intervention_components) > 3) {
            message(paste0(
                "There are more than 3 intervention components, and the grid search ",
                "algorithm may take significant amount of time to run. ",
                "Please consider adjusting the step size, or switch to the ",
                "numerical optimization method."
            ))
        }
    }

    # if optimization_grid_search_step_size is provided,
    # it needs to be a numeric vector
    if (!is.null(optimization_grid_search_step_size)) {
        if (!(is.vector(optimization_grid_search_step_size) &&
            is.numeric(optimization_grid_search_step_size))) {
            stop("optimization_grid_search_step_size must be a numeric vector.")
        }
        # if optimization_grid_search_step_size is provided,
        # it needs to have the same length as the number of intervention components
        if ((!include_interaction_terms &&
            (length(optimization_grid_search_step_size) !=
                length(intervention_components))) ||
            (include_interaction_terms &&
                (length(optimization_grid_search_step_size) !=
                    length(main_components)))
        ) {
            stop(paste(
                "Without interaction terms,",
                "The number of step sizes provided for the grid search",
                "algorithm must be the same as the number of intervention",
                "components. Please check the length of",
                "'optimization_grid_search_step_size'.",
                "With interaciton terms,",
                "The number of step sizes provided for the grid search",
                "algorithm must be the same as the length of 'main_components'."
            ))
        }
    }

    # check if the outcome goal is a numeric number
    if (!is.numeric(outcome_goal)) {
        stop("The outcome goal must be a numeric value.")
    }
    # check if the outcome goal >= outcome that we already observed
    if (outcome_goal <= mean(data[[outcome_name]])) {
        lower_outcome_goal <- TRUE
        # stop(paste(
        #     "The specified outcome goal is below the observed mean of the",
        #     "intervention group. Please increase the goal."
        # ))
        print(paste(
            "The specified outcome goal is less than the observed mean of the",
            "outcome. If the intended behavior is to reduce the outcome to a",
            "prespecified threshold, please ignore this."
        ))
    } else {
        lower_outcome_goal <- FALSE
    }

    # check whether the include_confidence_set indicator is boolean
    if (!is.logical(include_confidence_set)) {
        stop("The include_confidence_set indicator must be a boolean.")
    }

    # check if confidence set step size is provided
    if (include_confidence_set && !is.null(confidence_set_grid_step_size)) {
        # if confidence_set_grid_step_size is provided,
        # it needs to be a numeric vector
        if (!(is.vector(confidence_set_grid_step_size) &&
            is.numeric(confidence_set_grid_step_size))) {
            stop("confidence_set_grid_step_size must be a numeric vector.")
        }
        # if confidence_set_grid_step_size is provided,
        # it needs to have the same length as the number of intervention components
        if ((!include_interaction_terms &&
            (length(confidence_set_grid_step_size) !=
                length(intervention_components))) ||
            (include_interaction_terms &&
                (length(confidence_set_grid_step_size) !=
                    length(main_components)))
        ) {
            stop(paste(
                "Without interaction terms,",
                "the number of step sizes provided for the grid",
                "search algorithm in the confidence set calculation",
                "must be the same as the length of 'intervention_components'.",
                "Please check the length of 'confidence_set_grid_step_size'.",
                "With interaction terms,",
                "the number of step sizes provided for the grid",
                "search algorithm in the confidence set calculation",
                "must be the same as the length of 'main_components'."
            ))
        }
    }

    # check if confidence_set_alpha is a numeric type
    if (!is.numeric(confidence_set_alpha)) {
        stop("confidence_set_alpha must be a numeric value.")
    }
    # check if confidence_set_alpha is between 0 and 1
    if (confidence_set_alpha <= 0 || confidence_set_alpha >= 1) {
        stop("confidence_set_alpha must be between 0 and 1.")
    }

    # check if the specified family matches the outcome type
    valid_families <- list(
        "binary" = c("binomial"),
        "continuous" = c("gaussian", "quasibinomial")
    )
    if (!glm_family %in% valid_families[[outcome_type]]) {
        stop(paste("The specified family '", glm_family,
            "' is not valid for the outcome type '", outcome_type, "'.",
            " Please select a compatible family.",
            sep = ""
        ))
    }

    list(
        data = data,
        input_data_structure = input_data_structure,
        outcome_name = outcome_name,
        outcome_type = outcome_type,
        intervention_components = intervention_components,
        intervention_lower_bounds = intervention_lower_bounds,
        intervention_upper_bounds = intervention_upper_bounds,
        outcome_goal = outcome_goal,
        unit_costs = unit_costs,
        default_cost_fxn_type = default_cost_fxn_type,
        cost_list_of_vectors = cost_list_of_vectors,
        glm_family = glm_family,
        link = link,
        optimization_method = optimization_method,
        confidence_set_alpha = confidence_set_alpha,
        weights = weights,
        center_characteristics = center_characteristics,
        center_characteristics_optimization_values = center_characteristics_optimization_values,
        main_components = main_components,
        time_effect_optimization_value = time_effect_optimization_value,
        additional_covariates = additional_covariates,
        center_weights_for_outcome_goal = center_weights_for_outcome_goal,
        optimization_grid_search_step_size = optimization_grid_search_step_size,
        confidence_set_grid_step_size = confidence_set_grid_step_size,
        include_confidence_set = include_confidence_set,
        include_center_effects = include_center_effects,
        include_time_effects = include_time_effects,
        include_interaction_terms = include_interaction_terms,
        family_object = family_object,
        link = link,
        lower_outcome_goal = lower_outcome_goal
    )
}
