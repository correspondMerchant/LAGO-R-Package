#' get_confidence_set
#'
#' @description Internal function that calculates the confidence set
#' for the recommended interventions
#'
#' @param predictors_data A data.frame. The input data containing
#' the intervention components and center characteristics.
#' @param include_center_effects A boolean. Specifies whether the fixed effects
#' should be included in the outcome model.
#' @param center_weights_for_outcome_goal A numeric vector. Specifies the
#' weights that will be used for calculating recommended interventions that
#' satisfy the outcome goal for an (weighted) average center.
#' The weights need to sum up to 1.
#' @param include_time_effects A boolean. Specifies whether the fixed time
#' effects should be included in the outcome model.
#' @param additional_covariates A character vector. The names of the columns in
#' the dataset that represent additional covaraites that need to be included
#' in the outcome model. This includes interaction terms or any other additional
#' covariates.
#' @param intervention_components A character vector. The names of the columns
#' in the dataset that represent the intervention components.
#' @param include_interaction_terms A boolean. Specifies whether there are
#' interaction terms in the intervention components.
#' @param main_components A character vector. Specifies the main intervention
#' components in the presence of interaction terms.
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
#' @param cluster_id A list. Specifies the columns of data that will be used
#' as clustering effects when the "outcome_type" is continuous.
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
#'   predictors_data = BB_proportions[, c(
#'     "coaching_updt",
#'     "launch_duration",
#'     "birth_volume_100"
#'   )],
#'   intervention_components = c("coaching_updt", "launch_duration"),
#'   fitted_model = model,
#'   outcome_goal = 0.8,
#'   outcome_type = "continuous",
#'   center_characteristics = "birth_volume_100",
#'   center_characteristics_optimization_values = 1.74,
#'   outcome_data = BB_proportions$EBP_proportions,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   confidence_set_grid_step_size = c(1, 0.5)
#' )
#'
#' @import stats
#' @importFrom rje expit logit
#'
#' @export
#'
get_confidence_set <- function(
    predictors_data,
    include_center_effects = FALSE,
    center_weights_for_outcome_goal = 1,
    include_time_effects = FALSE,
    additional_covariates = NULL,
    intervention_components,
    include_interaction_terms = FALSE,
    main_components = NULL,
    outcome_data,
    fitted_model,
    outcome_goal,
    outcome_type,
    intervention_lower_bounds,
    intervention_upper_bounds,
    confidence_set_grid_step_size,
    center_characteristics = NULL,
    center_characteristics_optimization_values = 0,
    confidence_set_alpha = 0.05,
    cluster_id = NULL) {
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

  # create a new grid based on the full grid if there are
  # interaction terms. The full grid should only include
  # main effects, and the new grid should account for
  # the interaction terms properly.
  if (include_interaction_terms) {
    colnames(grid_x) <- main_components

    new_grid <- data.frame(
      matrix(
        nrow = n_rows,
        ncol = length(intervention_components)
      )
    )
    colnames(new_grid) <- intervention_components

    # Fill in the new grid
    for (i in seq_along(intervention_components)) {
      # Split the component string by ":"
      components <- strsplit(
        gsub("`", "", intervention_components[i]), ":"
      )[[1]]

      if (length(components) == 1) {
        # Single component - just copy the column
        new_grid[, i] <- grid_x[, (components)]
      } else {
        # Multiple components - multiply the corresponding columns
        # Initialize with first component
        result <- grid_x[, (components[1])]
        # Multiply by remaining components
        for (j in 2:length(components)) {
          result <- result * grid_x[, (components[j])]
        }
        new_grid[, i] <- result
      }
    }
  } else {
    colnames(grid_x) <- intervention_components
    new_grid <- grid_x
  }

  # add center effects (if specified) to the data that
  # will be used for predictions
  if (include_center_effects) {
    n_centers <- length(center_weights_for_outcome_goal) - 1
    repeated_center_effects <- rep(
      center_weights_for_outcome_goal[-1],
      length.out = n_rows * n_centers
    )
    repeated_center_effects_mat <- matrix(
      repeated_center_effects,
      nrow = n_rows,
      ncol = n_centers,
      byrow = TRUE
    )
  }

  # add time effects (if specified) to the data that
  # will be used for predictions
  if (include_time_effects) {
    n_periods <- length(
      grep("period", names(coef(fitted_model)), ignore.case = TRUE)
    )
    repeated_time_effects <- rep(
      c(rep(0, n_periods - 1), 1), # assuming we want the last period
      length.out = n_rows * n_periods
    )
    repeated_time_effects_mat <- matrix(
      repeated_time_effects,
      nrow = n_rows,
      ncol = n_periods,
      byrow = TRUE
    )
  }

  # add additional covariates (if specified) to the data that
  # will be used for predictions
  if (length(additional_covariates) > 0) {
    n_additional <- length(additional_covariates)
    repeated_additional <- rep(
      0,
      length.out = n_rows * n_additional
    )
    repeated_additional_mat <- matrix(
      repeated_additional,
      nrow = n_rows,
      ncol = n_additional,
      byrow = TRUE
    )
  }

  # add center characteristics (if specified) to the data that
  # will be used for predictions
  if (length(center_characteristics) > 0) {
    n_center_cha <- length(center_characteristics_optimization_values)
    repeated_center_cha <- rep(
      center_characteristics_optimization_values,
      length.out = n_rows * n_center_cha
    )
    repeated_center_cha_mat <- matrix(
      repeated_center_cha,
      nrow = n_rows,
      ncol = n_center_cha,
      byrow = TRUE
    )
  }

  # assemble the new data for prediction
  components <- list()
  if (include_center_effects) {
    components$center_effects <- repeated_center_effects_mat
  }
  if (include_time_effects) {
    components$time_effects <- repeated_time_effects_mat
  }
  components$new_grid <- new_grid
  if (length(additional_covariates) > 0) {
    components$additional <- repeated_additional_mat
  }
  if (length(center_characteristics) > 0) {
    components$center_cha <- repeated_center_cha_mat
  }
  new_data <- as.data.frame(do.call(cbind, components))
  new_data <- cbind(Intercept = 1, new_data)
  colnames(new_data) <- names(fitted_model$coefficients)

  # get critical value based on the given alpha value
  critical_value <- qnorm(1 - confidence_set_alpha / 2)
  # ----------------------------------------------------------------------------
  if (outcome_type == "binary") {
    # TODO: this part needs to handle more than logit link.
    new_data <- as.matrix(new_data)
    pred_all <- expit(new_data %*% coef(fitted_model))
    se_pred_all <- sqrt(
      diag((new_data) %*% vcov(fitted_model) %*% t(new_data))
    ) * pred_all * (1 - pred_all)

    # lower and upper bounds of predictions
    lb_prob_all <- pred_all - critical_value * se_pred_all
    ub_prob_all <- pred_all + critical_value * se_pred_all
    ci_prob_all <- cbind(lb_prob_all, ub_prob_all)

    # calculate percentage of interventions
    # that are included in the confidence set
    set_size_intervals <- sum(apply(
      ci_prob_all,
      1,
      function(y) findInterval(x = outcome_goal, vec = y)
    ) == 1)
    confidence_set_size_percentage <- set_size_intervals / n_rows
    # ----------------------------------------------------------------------------
  } else if (outcome_type == "continuous") {
    # link is either "binary" or "identity"
    # If link == "binary", use the old logic (logistic-like) for variance
    # If link == "identity", use the identity link logic.

    # define the function to manually calculate var-cov matrix
    get_vcov <- function(predictors_data,
                         model,
                         outcome_data,
                         cluster_ids = NULL) {
      # First prepare the full design matrix
      # (including fixed effects dummies if any)
      prepare_design_matrix <- function(data) {
        # Start with intercept column
        X <- matrix(1, nrow = nrow(data), ncol = 1)
        colnames(X) <- "(Intercept)"

        # For each column in predictors_data
        for (col in names(data)) {
          if (is.factor(data[[col]]) || is.character(data[[col]])) {
            # Create dummies for categorical variables
            # (using first level as reference)
            levels <- unique(data[[col]])
            for (lev in levels[-1]) {
              dummy <- as.numeric(data[[col]] == lev)
              X <- cbind(X, dummy)
              colnames(X)[ncol(X)] <- paste0(col, lev)
            }
          } else {
            # Numeric columns added as is
            X <- cbind(X, data[[col]])
            colnames(X)[ncol(X)] <- col
          }
        }
        return(X)
      }

      # Original single cluster vcov (logistic-like)
      get_single_cluster_vcov <- function(X,
                                          cluster_id,
                                          n_params,
                                          fitted_values,
                                          outcome) {
        matrix_j <- matrix(0, nrow = n_params, ncol = n_params)
        matrix_v <- matrix(0, nrow = n_params, ncol = n_params)

        clusters <- unique(cluster_id)
        n_clusters <- length(clusters)

        for (c in clusters) {
          cluster_idx <- which(cluster_id == c)

          cluster_score <- matrix(0, nrow = n_params, ncol = 1)
          cluster_hessian <- matrix(0, nrow = n_params, ncol = n_params)

          for (i in cluster_idx) {
            x_i <- as.matrix(X[i, ])
            p_i <- fitted_values[i]
            ddbeta_i <- (p_i * (1 - p_i)) * x_i

            j_i <- ddbeta_i %*% t(ddbeta_i)
            cluster_hessian <- cluster_hessian + j_i

            score_i <- ddbeta_i * (outcome[i] - p_i)
            cluster_score <- cluster_score + score_i
          }

          matrix_j <- matrix_j + cluster_hessian / n_clusters
          matrix_v <- matrix_v + (cluster_score %*% t(cluster_score)) / n_clusters
        }

        bread <- solve(matrix_j)
        return(bread %*% matrix_v %*% t(bread) / n_clusters)
      }

      # Identity link single cluster vcov
      get_single_cluster_vcov_identity <- function(X,
                                                   cluster_id,
                                                   outcome,
                                                   fitted_values) {
        residuals <- outcome - fitted_values
        # Bread matrix for linear model: (X'X)^(-1)
        bread <- solve(t(X) %*% X)

        # Initialize cluster sum
        n_params <- ncol(X)
        cluster_sum <- matrix(0, nrow = n_params, ncol = n_params)

        clusters <- unique(cluster_id)

        for (c in clusters) {
          cluster_idx <- which(cluster_id == c)
          X_c <- X[cluster_idx, , drop = FALSE]
          e_c <- residuals[cluster_idx]

          # Cluster score S_c = X_c'e_c
          cluster_score <- t(X_c) %*% e_c

          # Add S_c S_c' to cluster_sum
          cluster_sum <- cluster_sum + (cluster_score %*% t(cluster_score))
        }
        return(bread %*% cluster_sum %*% bread)
      }

      print(names(predictors_data))
      X <- prepare_design_matrix(predictors_data)
      n <- nrow(X)
      n_params <- ncol(X)
      fitted_values <- model$fitted.values

      if (is.null(cluster_ids)) {
        # Non-clustered case
        if (link == "identity") {
          # For identity link (linear model):
          # sigma^2 = sum of squared residuals / (n - n_params)
          residuals <- outcome_data - fitted_values
          sigma2 <- sum(residuals^2) / (n - n_params)
          bread <- solve(t(X) %*% X)
          vcov_matrix <- bread * sigma2
        } else {
          matrix_j <- matrix(0, nrow = n_params, ncol = n_params)
          matrix_v <- matrix(0, nrow = n_params, ncol = n_params)

          for (i in 1:n) {
            x_i <- as.matrix(X[i, ])
            p_i <- fitted_values[i]
            ddbeta_i <- (p_i * (1 - p_i)) * x_i

            j_i <- ddbeta_i %*% t(ddbeta_i)
            matrix_j <- matrix_j + j_i / n

            v_i <- (ddbeta_i) %*% ((outcome_data[i] - p_i)^2) %*% t(ddbeta_i)
            matrix_v <- matrix_v + v_i / n
          }

          bread <- solve(matrix_j)
          vcov_matrix <- (bread %*% matrix_v %*% t(bread)) / n
        }
      } else if (length(cluster_ids) == 1) {
        # Single clustering
        cluster_id <- if (is.list(cluster_ids)) cluster_ids[[1]] else cluster_ids

        if (link == "identity") {
          vcov_matrix <- get_single_cluster_vcov_identity(
            X,
            cluster_id,
            outcome_data,
            fitted_values
          )
        } else {
          vcov_matrix <- get_single_cluster_vcov(
            X,
            cluster_id,
            n_params,
            fitted_values,
            outcome_data
          )
        }
      } else if (length(cluster_ids) == 2) {
        # Two-way clustering
        cluster_id1 <- cluster_ids[[1]]
        cluster_id2 <- cluster_ids[[2]]

        if (link == "identity") {
          vcov1 <- get_single_cluster_vcov_identity(
            X,
            cluster_id1,
            outcome_data,
            fitted_values
          )
          vcov2 <- get_single_cluster_vcov_identity(
            X,
            cluster_id2,
            outcome_data,
            fitted_values
          )
          intersection_id <- paste(cluster_id1, cluster_id2, sep = "_")
          vcov12 <- get_single_cluster_vcov_identity(
            X,
            intersection_id,
            outcome_data,
            fitted_values
          )
        } else {
          vcov1 <- get_single_cluster_vcov(
            X,
            cluster_id1,
            n_params,
            fitted_values,
            outcome_data
          )
          vcov2 <- get_single_cluster_vcov(
            X,
            cluster_id2,
            n_params,
            fitted_values,
            outcome_data
          )
          intersection_id <- paste(cluster_id1, cluster_id2, sep = "_")
          vcov12 <- get_single_cluster_vcov(
            X,
            intersection_id,
            n_params,
            fitted_values,
            outcome_data
          )
        }

        # Cameron-Gelbach-Miller (2011) two-way clustering
        vcov_matrix <- vcov1 + vcov2 - vcov12
      } else if (length(cluster_ids) == 1) {
        # Single clustering (duplicate else condition can be removed,
        # this was in original code, just keep one version above)
        cluster_id <- cluster_ids[[1]]
        if (link == "identity") {
          vcov_matrix <- get_single_cluster_vcov_identity(
            X,
            cluster_id,
            outcome_data,
            fitted_values
          )
        } else {
          vcov_matrix <- get_single_cluster_vcov(
            X,
            cluster_id,
            n_params,
            fitted_values,
            outcome_data
          )
        }
      }

      rownames(vcov_matrix) <- colnames(X)
      colnames(vcov_matrix) <- colnames(X)

      return(vcov_matrix)
    }

    # calculate the var-cov matrix
    vcov_matrix <- get_vcov(
      predictors_data,
      fitted_model,
      outcome_data,
      cluster_id
    )

    # get predicted values
    new_data <- as.matrix(new_data)
    pred_all <- (new_data %*% as.matrix(coef(fitted_model)))

    # Calculate standard errors for all rows
    std_er_all <- sqrt(rowSums((new_data %*% vcov_matrix) * new_data))

    # Calculate lower and upper bounds for predicted values
    lb_prob_all <- pred_all - critical_value * std_er_all
    ub_prob_all <- pred_all + critical_value * std_er_all

    if (link == "identity") {
      # For identity link, predictions are on the correct scale already.
      ci_prob_all <- cbind(lb_prob_all, ub_prob_all)
    } else {
      # For binary link (or the original logic), use expit.
      ci_prob_all <- expit(cbind(lb_prob_all, ub_prob_all))
    }

    # calculate percentage of interventions that are included in the confidence set
    set_size_intervals <- sum(apply(
      ci_prob_all,
      1,
      function(y) findInterval(x = outcome_goal, vec = y)
    ) == 1)
    confidence_set_size_percentage <- set_size_intervals / n_rows
  }

  # obtain the confidence set
  cs_row_indices <- which((apply(
    ci_prob_all,
    1,
    function(y) findInterval(x = outcome_goal, vec = y)
  ) == 1) == 1)

  if (length(cs_row_indices) == 0) {
    return(list(
      confidence_set_size_percentage = 0,
      cs = NULL
    ))
  }

  cs <- grid_x[cs_row_indices, c(
    if (include_interaction_terms) {
      main_components
    } else {
      intervention_components
    }
  )]

  if (length(center_characteristics) > 0) {
    original_cs_name <- names(cs)
    cs <- cbind(cs, center_characteristics_optimization_values)
    names(cs) <- c(original_cs_name, center_characteristics)
  }

  return(list(
    confidence_set_size_percentage = confidence_set_size_percentage,
    cs = cs
  ))
}
