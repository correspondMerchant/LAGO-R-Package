test_processor <- function(data, outcome_type, outcome_name, row_sizes = NULL) {
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
  if (!is.null(row_sizes)) {
    # a row without a size is left out, as glm() leaves it out
    complete_cases <- complete_cases & !is.na(row_sizes)
    row_sizes <- row_sizes[complete_cases]
  }
  data <- data[complete_cases, ]

  # if outcome_type is "binary", use two-sample test
  if (outcome_type == "binary") {
    # the mean, variance and size of each arm's patients. A center level row
    # stands for row_sizes patients with the row's proportion of events, so it
    # gives the same test as those patients one row each.
    arm_summary <- function(arm) {
      y <- as.numeric(data[data$group == arm, outcome_name, drop = TRUE])
      if (is.null(row_sizes)) {
        return(c(mean = mean(y), var = var(y), n = length(y)))
      }
      size <- row_sizes[data$group == arm]
      n <- sum(size)
      events <- sum(y * size)
      c(mean = events / n, var = (events - events^2 / n) / (n - 1), n = n)
    }
    treatment <- arm_summary("treatment")
    control <- arm_summary("control")

    # manually write test for two-sample test
    # with unpooled variance
    t_stat <- (treatment[["mean"]] - control[["mean"]]) / sqrt(
      treatment[["var"]] / treatment[["n"]] + control[["var"]] / control[["n"]]
    )
    p_val <- 2 * pt(
      -abs(t_stat),
      df = treatment[["n"]] + control[["n"]] - 2
    )

    # add pooled variance once the proofs are done
    results <- list(test_stat = t_stat, p_val = p_val)
  } else {
    # for now, only binary outcomes are supported
    results <- NULL
  }

  return(results)
}
