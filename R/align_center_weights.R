# Align the center weights with the centers glm() fitted (it drops any it cannot use).
align_center_weights_to_fit <- function(center_weights,
                                        data,
                                        model,
                                        user_weights = NULL,
                                        center_name = NULL) {
  # the validated factor's own levels, which can include an explicit NA level
  all_centers <- levels(data$center)
  fitted_centers <- levels(droplevels(stats::model.frame(model)$center))
  kept <- all_centers %in% fitted_centers
  if (all(kept)) {
    return(center_weights)
  }
  lost <- all_centers[!kept]
  if (length(center_weights) != length(all_centers)) {
    stop(paste0(
      "Internal error: ", length(center_weights), " center weights for ",
      length(all_centers), " centers, so they cannot be matched to the centers."
    ))
  }
  why <- paste0(
    "no row the outcome model could use (one with every model variable and its ",
    "glm weight, which is center_sample_size for center level data, observed)"
  )
  # user weights win over a named center, as in validate_inputs()
  if (!is.null(center_name) && is.null(user_weights)) {
    if (center_name %in% lost) {
      stop(paste0(
        "The center '", center_name, "' named in ",
        "'center_effects_optimization_values' has ", why, ", so the model ",
        "cannot fit its effect. Pick another center, or fill in the missing ",
        "values."
      ))
    }
    return(as.numeric(fitted_centers %in% center_name))
  }
  if (!is.null(user_weights)) {
    weighted_lost <- lost[center_weights[!kept] != 0]
    if (length(weighted_lost) > 0) {
      stop(paste0(
        "The center(s) ", paste0("'", weighted_lost, "'", collapse = ", "),
        " have ", why, ", so the model cannot fit their effects, but ",
        "'center_weights_for_outcome_goal' gives them a non-zero weight. Set ",
        "their weights to 0 and rescale the others to sum to 1, or fill in the ",
        "missing values."
      ))
    }
  } else {
    warning(paste0(
      "The center(s) ", paste0("'", lost, "'", collapse = ", "), " have ", why,
      ", so the model could not fit them and they are left out of the average ",
      "center."
    ))
  }
  # match() pairs an NA level with NA, where indexing by name would not
  weights <- center_weights[match(fitted_centers, all_centers)]
  unname(weights / sum(weights))
}
