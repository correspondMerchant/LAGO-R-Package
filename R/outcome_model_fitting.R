outcome_model_fitting <- function(
    data,
    input_data_structure = "individual_level",
    outcome_name,
    family_object,
    intervention_components,
    weights,
    center_characteristics,
    additional_covariates,
    include_center_effects = FALSE,
    include_time_effects = FALSE,
    include_interaction_terms = FALSE) {
  # fit the outcome model
  if (input_data_structure == "center_level") {
    outcome_name <- "proportion"
    weights <- data$center_sample_size
  }
  covariates <- c(
    if (include_center_effects) "center",
    if (include_time_effects) "period",
    intervention_components,
    if (!is.null(additional_covariates)) additional_covariates,
    if (!is.null(center_characteristics)) center_characteristics
  )
  formula <- as.formula(
    paste(outcome_name, "~", paste(covariates, collapse = " + "))
  )
  tryCatch(
    {
      model <- glm(
        formula,
        data = data,
        family = family_object,
        weights = weights
      )
    },
    error = function(e) {
      stop(paste("Error occurred during model fitting step:\n", e))
    }
  )

  # if model did not converge, stop the function
  if (!model$converged) {
    stop(paste(
      "Model did not converge. Please check",
      "your input data and model specifications."
    ))
  }

  list(
    model = model
  )
}
