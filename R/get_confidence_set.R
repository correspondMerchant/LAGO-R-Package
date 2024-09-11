#' get_confidence_set
#'
#' @description Calculates the confidence set for the recommended intervention
#'
#' @param predictors df, All predictors used in the fitted model.
#' @param fitted_model glm(), The fitted model.
#' @param outcome_goal numeric, The outcome goal.
#' @param outcome_type character, Outcome type, either "continuous" or "binary".
#' @param outcome df, Outcome for the fitted model.
#' @param intervention_lower_bounds numeric vector, Lower bounds for the
#' intervention components.
#' For example: c(0,0).
#' @param intervention_upper_bounds numeric vector, Upper bounds for the
#' intervention components.
#' For example: c(10,20).
#' @param confidence_set_step_size numeric vector, step sizes of the grid for the
#' intervention components.
#' For example: c(1, 0.5)
#' @param center_characteristic_list character vector, Names of the center characteristics
#' For example: c("characteristic1")
#' @param center_cha numeric vector, Given values of the center characteristics.
#' @param include_intercept boolean, Whether the intercept was included in the fitted model.
#' @param confidence_set_alpha numeric, Type I error for the confidence interval.
#'
#' @return List(percentage of interventions that are included in the confidence set,
#' confidence set)
#'
#' @examples
#' # fit the model
#' model <- glm(EBP_proportions ~ coaching_updt + launch_duration + birth_volume_100,
#'   data = BB_proportions, family = quasibinomial(link = "logit")
#' )
#' # get the confidence set
#' cs_results <- get_confidence_set(
#'   predictors = cbind(BB_proportions$coaching_updt, BB_proportions$launch_duration,
#'   BB_proportions$birth_volume_100),
#'   fitted_model = model,
#'   outcome_goal = 0.8,
#'   outcome_type = "continuous",
#'   center_characteristic_list = "birth_volume_100",
#'   center_cha = 1.74,
#'   outcome = BB_proportions$EBP_proportions,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   confidence_set_step_size = c(0.1, 0.1)
#' )
#'
#' @import stats
#'
#' @export
#
#
get_confidence_set <- function(predictors,
                               fitted_model,
                               outcome_goal,
                               outcome_type,
                               outcome,
                               intervention_lower_bounds,
                               intervention_upper_bounds,
                               confidence_set_step_size,
                               center_characteristic_list = NULL,
                               center_cha = 0,
                               include_intercept = TRUE,
                               confidence_set_alpha = 0.05) {
  # Create a list to store sequences for each component
  sequences <- list()
  # Generate sequences for each intervention component
  for (i in 1:length(intervention_lower_bounds)) {
    sequences[[i]] <- seq(
      from = intervention_lower_bounds[i],
      to = intervention_upper_bounds[i],
      by = confidence_set_step_size[i]
    )
  }
  # expand grid
  grid_x <- expand.grid(sequences)
  n_rows <- nrow(grid_x)

  # repeat the center characteristics to be used later for creating the data
  # set for predictions
  if (!is.null(center_characteristic_list)) {
    n_center_cha <- length(center_cha)
    repeated_center_cha <- rep(center_cha, length.out = n_rows * n_center_cha)
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
  if (include_intercept) {
    colnames(new_data) <- names(fitted_model$coefficients)[-1]
  } else {
    colnames(new_data) <- names(fitted_model$coefficients)
  }

  # get critical value based on the given alpha value
  critical_value <- qnorm(1 - confidence_set_alpha / 2)

  if (outcome_type == "binary") {
    # predict outcomes using the fitted model
    pred_all <- predict(fitted_model, newdata = new_data, se.fit = T, type = "response")

    # lower and upper bounds of predictions
    lb_prob_all <- pred_all$fit - critical_value * pred_all$se.fit
    ub_prob_all <- pred_all$fit + critical_value * pred_all$se.fit
    ci_prob_all <- cbind(lb_prob_all, ub_prob_all)

    # calculate percentage of interventions that are included in the confidence set
    set_size_intervals <- sum(apply(ci_prob_all, 1, function(y) findInterval(x = outcome_goal, vec = y)) == 1)
    confidence_set_size_percentage <- set_size_intervals / n_rows

    # calculate the confidence set
    cs <- new_data[(apply(ci_prob_all, 1, function(y) findInterval(x = outcome_goal, vec = y)) == 1) == 1, ]
  } else if (outcome_type == "continuous") {
    # define the function to manually calculate var-cov matrix according to the
    # formulas in Bing et al.
    # TODO: for other link functions, we may need separate functions to manually
    # calculate their vcov matrix. this is not straightforward.
    get_vcov <- function(predictors, model, outcome) {
      matrix.J <- 0
      matrix.V <- 0
      n <- length(predictors[, 1])
      probs <- model$fitted.values

      for (i in 1:n) {
        if (include_intercept) {
          x.i <- as.matrix(c(1, as.numeric(predictors[i, ])))
        } else {
          x.i <- as.matrix(as.numeric(predictors[i, ]))
        }

        p.i <- probs[i]
        ddbeta.i <- (p.i * (1 - p.i)) * x.i

        J.i <- ddbeta.i %*% t(ddbeta.i)
        matrix.J <- matrix.J + J.i / n

        V.i <- (ddbeta.i) %*% ((outcome[i] - p.i)^2) %*% t(ddbeta.i)
        matrix.V <- matrix.V + V.i / n
      }

      return(solve(matrix.J) %*% matrix.V %*% t(solve(matrix.J)) / n)
    }

    # calculate the var-cov matrix
    print(predictors)
    print(fitted_model)

    vcov_matrix <- get_vcov(predictors, fitted_model, outcome)

    # get predicted values
    pred_all <- predict(fitted_model, newdata = new_data)

    # Create design matrix
    if (include_intercept) {
      C <- cbind(1, as.matrix(new_data))
    } else {
      C <- as.matrix(new_data)
    }

    # Calculate standard errors for all rows
    std_er_all <- sqrt(rowSums((C %*% vcov_matrix) * C))

    # Calculate lower and upper bounds for predicted values
    lb_prob_all <- pred_all - critical_value * std_er_all
    ub_prob_all <- pred_all + critical_value * std_er_all

    # Combine and apply expit function
    ci_prob_all <- expit(cbind(lb_prob_all, ub_prob_all))

    # calculate percentage of interventions that are included in the confidence set
    set_size_intervals <- sum(apply(ci_prob_all, 1, function(y) findInterval(x = outcome_goal, vec = y)) == 1)
    confidence_set_size_percentage <- set_size_intervals / n_rows

    # calculate the confidence set
    cs <- new_data[(apply(ci_prob_all, 1, function(y) findInterval(x = outcome_goal, vec = y)) == 1) == 1, ]
  }

  return(list(
    confidence_set_size_percentage = confidence_set_size_percentage,
    cs = cs
  ))
}





# TEST #
# delete later#
#
# ## fit the model
# eg_model <- glm(case ~ age + parity, data = infert, family = binomial())
#
# ## get the confidence set
# get_confidence_set(
#   data = infert,
#   fitted_model = eg_model,
#   outcome_goal = 0.5,
#   outcome_type = "binary",
#   outcome_name = "case",
#   intervention_lower_bounds = c(0, 0),
#   intervention_upper_bounds = c(50, 10),
#   confidence_set_step_size = c(5, 1)
# )
## fit the model
# load(BB_proportions)
# model <- glm(EBP_proportions ~ coaching_updt + launch_duration + birth_volume_100,
#              data = BB_proportions, family = quasibinomial(link="logit") )
# summary(model)
# #
# ## get the confidence set
# cs_results <- get_confidence_set(
#  predictors = cbind(BB_proportions$coaching_updt, BB_proportions$launch_duration,
# BB_proportions$birth_volume_100),
#  fitted_model = model,
#  outcome_goal = 0.8,
#  outcome_type = "continuous",
#  center_characteristic_list = "birth_volume_100",
#  center_cha = 1.74,
#  outcome = BB_proportions$EBP_proportions,
#  intervention_lower_bounds = c(1, 1),
#  intervention_upper_bounds = c(40, 5),
#  confidence_set_step_size = c(0.1, 0.1)
# )
