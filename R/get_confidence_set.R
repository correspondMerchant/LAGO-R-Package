#' get_confidence_set
#'
#' @description Internal function that calculates the confidence set
#' for the recommended interventions
#'
#' @param predictors_data A data.frame. The input data containing
#' the intervention components and center characteristics.
#' @param outcome_data A data.frame. The input data containing the outcome
#' of interest.
#' @param fitted_model A glm(). The fitted glm() outcome model.
#' @param outcome_goal A numeric value. Specifies the outcome goal, a desired
#' probability or mean value.
#' @param outcome_type A character string. Specifies the type of the outcome.
#' Must be either "continuous" for continuous outcomes or "binary" for binary
#' outcomes.
#' @param intervention_lower_bounds A numeric vector. Specifies the lower bounds
#' of the intervention components.
#' For example: for a two-component intervention package, lower bounds could be
#' c(0,0).
#' @param intervention_upper_bounds A numeric vector. Specifies the upper bounds
#' of the intervention components.
#' For example: for a two-component intervention package, upper bounds could be
#' c(10,20).
#' @param confidence_set_grid_step_size A numeric vector. Specifies the step
#' size of the grid search algorithm used in the confidence set calculation.
#' Default value without user specification:
#' 1/20 of the range for each intervention component.
#' @param center_characteristics A character vector. The names of the columns in
#' the dataset that represent the center characteristics.
#' For example: c("characteristic1")
#' @param center_characteristics_optimization_values A numeric vector. The
#' values of the center characteristics that will be used for LAGO optimization.
#' For example: c(1.74)
#' @param confidence_set_alpha A numeric value. The type I error considered in
#' the confidence set calculations.
#' Default value without user specification: 0.05.
#'
#' @return List(
#' percentage of interventions that are included in the confidence set,
#' confidence set)
#'
#' @examples
#' # fit the model
#' model <- glm(
#'   EBP_proportions ~ coaching_updt
#'     + launch_duration + birth_volume_100,
#'   data = BB_proportions, family = quasibinomial(link = "logit")
#' )
#' # get the confidence set
#' cs_results <- get_confidence_set(
#'   predictors_data = cbind(
#'     BB_proportions$coaching_updt, BB_proportions$launch_duration,
#'     BB_proportions$birth_volume_100
#'   ),
#'   fitted_model = model,
#'   outcome_goal = 0.8,
#'   outcome_type = "continuous",
#'   center_characteristics = "birth_volume_100",
#'   center_characteristics_optimization_values = 1.74,
#'   outcome_data = BB_proportions$EBP_proportions,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   confidence_set_grid_step_size = c(0.1, 0.1)
#' )
#'
#' @import stats
#'
#' @export
#'
get_confidence_set <- function(predictors_data,
                               outcome_data,
                               fitted_model,
                               outcome_goal,
                               outcome_type,
                               intervention_lower_bounds,
                               intervention_upper_bounds,
                               confidence_set_grid_step_size,
                               center_characteristics = NULL,
                               center_characteristics_optimization_values = 0,
                               confidence_set_alpha = 0.05) {
  # Create a list to store sequences for each component
  sequences <- list()
  # Generate sequences for each intervention component
  for (i in 1:length(intervention_lower_bounds)) {
    sequences[[i]] <- seq(
      from = intervention_lower_bounds[i],
      to = intervention_upper_bounds[i],
      by = confidence_set_grid_step_size[i]
    )
  }
  # expand grid
  grid_x <- expand.grid(sequences)
  n_rows <- nrow(grid_x)

  # repeat the center characteristics to be used later for creating the data
  # set for predictions
  if (!is.null(center_characteristics)) {
    n_center_cha <- length(center_characteristics_optimization_values)
    repeated_center_cha <- rep(
      center_characteristics_optimization_values,
      length.out = n_rows * n_center_cha
    )
    repeated_center_cha_mat <- matrix(repeated_center_cha,
      nrow = n_rows,
      ncol = n_center_cha, byrow = T
    )
    # create new data set for predictions
    new_data <- as.data.frame(cbind(grid_x, repeated_center_cha_mat))
  } else {
    new_data <- as.data.frame(grid_x)
  }

  # give column names of the new data set based on the fitted model
  colnames(new_data) <- names(fitted_model$coefficients)[-1]

  # get critical value based on the given alpha value
  critical_value <- qnorm(1 - confidence_set_alpha / 2)

  if (outcome_type == "binary") {
    # predict outcomes using the fitted model
    pred_all <- predict(
      fitted_model,
      newdata = new_data,
      se.fit = TRUE,
      type = "response"
    )

    # lower and upper bounds of predictions
    lb_prob_all <- pred_all$fit - critical_value * pred_all$se.fit
    ub_prob_all <- pred_all$fit + critical_value * pred_all$se.fit
    ci_prob_all <- cbind(lb_prob_all, ub_prob_all)

    # calculate percentage of interventions
    # that are included in the confidence set
    set_size_intervals <- sum(apply(
      ci_prob_all,
      1,
      function(y) findInterval(x = outcome_goal, vec = y)
    ) == 1)
    confidence_set_size_percentage <- set_size_intervals / n_rows

    # calculate the confidence set
    cs <- new_data[(apply(
      ci_prob_all,
      1,
      function(y) findInterval(x = outcome_goal, vec = y)
    ) == 1) == 1, ]
  } else if (outcome_type == "continuous") {
    # define the function to manually calculate var-cov matrix according to the
    # formulas in Bing et al.
    # TODO: for other link functions, we may need separate functions to manually
    # calculate their vcov matrix. this is not straightforward.
    get_vcov <- function(predictors_data, model, outcome_data) {
      matrix_j <- 0
      matrix_v <- 0
      n <- length(predictors_data[, 1])
      probs <- model$fitted.values

      for (i in 1:n) {
        x_i <- as.matrix(c(1, as.numeric(predictors_data[i, ])))
        p_i <- probs[i]
        ddbeta_i <- (p_i * (1 - p_i)) * x_i

        j_i <- ddbeta_i %*% t(ddbeta_i)
        matrix_j <- matrix_j + j_i / n

        v_i <- (ddbeta_i) %*% ((outcome_data[i] - p_i)^2) %*% t(ddbeta_i)
        matrix_v <- matrix_v + v_i / n
      }

      return(solve(matrix_j) %*% matrix_v %*% t(solve(matrix_j)) / n)
    }

    # calculate the var-cov matrix
    vcov_matrix <- get_vcov(predictors_data, fitted_model, outcome_data)

    # get predicted values
    pred_all <- predict(fitted_model, newdata = new_data)

    # Create design matrix
    C <- cbind(1, as.matrix(new_data))

    # Calculate standard errors for all rows
    std_er_all <- sqrt(rowSums((C %*% vcov_matrix) * C))

    # Calculate lower and upper bounds for predicted values
    lb_prob_all <- pred_all - critical_value * std_er_all
    ub_prob_all <- pred_all + critical_value * std_er_all

    # Combine and apply expit function
    ci_prob_all <- expit(cbind(lb_prob_all, ub_prob_all))

    # calculate percentage of interventions that are
    # included in the confidence set
    set_size_intervals <- sum(apply(
      ci_prob_all,
      1,
      function(y) findInterval(x = outcome_goal, vec = y)
    ) == 1)
    confidence_set_size_percentage <- set_size_intervals / n_rows

    # calculate the confidence set
    cs <- new_data[(apply(
      ci_prob_all,
      1,
      function(y) findInterval(x = outcome_goal, vec = y)
    ) == 1) == 1, ]
  }

  return(list(
    confidence_set_size_percentage = confidence_set_size_percentage,
    cs = cs
  ))
}
