test_processor <- function(data, outcome_type, outcome_name) {
  # check if the 'group' column is either 'treatment' or 'control'
  if (!all(data$group %in% c("treatment", "control"))) {
    warning(paste(
      "To show the overall test results,",
      "the 'group' column must be either 'treatment'",
      "or 'control'. Please check the data."
    ))
    return(NULL)
  }

  # Remove NA values only from relevant columns
  complete_cases <- complete.cases(data[, c("group", outcome_name)])
  data <- data[complete_cases, ]

  # if outcome_type is "binary", use two-sample test
  if (outcome_type == "binary") {
    # split the data into treatment and control groups
    treatment <- as.numeric(
      data[data$group == "treatment",
        outcome_name,
        drop = TRUE
      ]
    )
    control <- as.numeric(
      data[data$group == "control",
        outcome_name,
        drop = TRUE
      ]
    )

    # manually write test for two-sample test
    # with unpooled variance
    t_stat <- (mean(treatment) - mean(control)) / sqrt(
      var(treatment) / length(treatment) + var(control) / length(control)
    )
    p_val <- 2 * pt(
      -abs(t_stat),
      df = length(treatment) + length(control) - 2
    )

    # add pooled variance once the proofs are done
    results <- list(test_stat = t_stat, p_val = p_val)
  } else {
    # for now, only binary outcomes are supported
    results <- NULL
  }

  return(results)
}
