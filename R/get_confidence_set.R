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
#' the dataset that represent additional covariates that need to be included
#' in the outcome model. This includes interaction terms or any other additional
#' covariates.
#' @param intervention_components A character vector. The names of the columns
#' in the dataset that represent the intervention components.
#' @param include_interaction_terms A boolean. Specifies whether there are
#' interaction terms in the intervention components.
#' @param main_components A character vector. Specifies the main intervention
#' components in the presence of interaction terms.
#' @param outcome_data A vector. The input data containing the outcome
#' of interest.
#' @param fitted_model A glm(). The fitted glm() outcome model.
#' @param link A character string. The link function (e.g. "logit", "identity").
#' @param outcome_goal A numeric value. Specifies the outcome goal, a desired
#' probability or mean value.
#' @param outcome_type A character string. Specifies the type of the outcome.
#' Must be either "continuous" for continuous outcomes or "binary" for binary
#' outcomes.
#' @param intervention_lower_bounds A numeric vector. Specifies the lower bounds
#' of the intervention components.
#' @param intervention_upper_bounds A numeric vector. Specifies the upper bounds
#' of the intervention components.
#' @param confidence_set_grid_step_size A numeric vector. Specifies the step
#' size of the grid search algorithm used in the confidence set calculation.
#' @param center_characteristics A character vector. The names of the columns in
#' the dataset that represent the center characteristics.
#' @param center_characteristics_optimization_values A numeric vector. The
#' fixed values of the center characteristics at which the confidence set is
#' computed, so the confidence set is specific to a center with these
#' characteristic values. Must have the same length and order as
#' center_characteristics.
#' @param confidence_set_alpha A numeric value. The type I error considered in
#' the confidence set calculations.
#' @param cluster_id A list or NULL. Specifies the columns of data that will be
#' used as clustering effects when the "outcome_type" is continuous.
#' @param cost_list_of_vectors A list of numeric vectors. The cost vectors
#' used in the LAGO optimization.
#' @param rec_int A numeric vector, the recommended interventions calculated
#' from the optimization step.
#'
#' @return List(
#'   confidence_set_size_percentage = <number, the size of the confidence set
#'     as a fraction of the grid. Both the count of qualifying interventions
#'     and the size of the grid count grid interventions only, so rec_int is
#'     excluded from each>,
#'   rec_int_ci = <named numeric c(lower, upper) rounded to 3 decimal places,
#'     the confidence interval at rec_int. Computed whether or not it covers
#'     the outcome goal, so callers never have to look for rec_int inside cs.
#'     NULL when that interval is not computable>,
#'   cs = <data.frame of the grid interventions whose confidence interval
#'     covers the outcome goal, with their interval bounds and cost. rec_int is
#'     never one of its rows, and need not be a grid intervention at all.
#'     NULL when no grid intervention qualifies>
#' )
#'
#' @import stats
#' @importFrom rje expit logit
#'
#' @examples
#' # Normally reached through lago_optimization(include_confidence_set = TRUE).
#' # Called directly it needs the fitted outcome model and the recommended
#' # intervention from the optimization step, so both are taken from a run of
#' # the optimizer rather than refitting the model by hand. get_confidence_set()
#' # binds its prediction matrix to the coefficient vector by name, so a
#' # hand-fitted model may list its terms in any order. It must be fitted on
#' # exactly the predictors passed here, though: the intercept, the fixed
#' # center effects, the fixed time effects, the intervention components, the
#' # additional covariates and the center characteristics. Any other set of
#' # coefficients is an error naming what did not match.
#' # The lower bounds start at 1 while the data also contains 0s, so the
#' # optimizer warns about that; the warning is expected here.
#' opt <- lago_optimization(
#'   data = BB_data,
#'   outcome_name = "pp3_oxytocin_mother",
#'   outcome_type = "binary",
#'   glm_family = "binomial",
#'   intervention_components = c("coaching_updt", "launch_duration"),
#'   center_characteristics = c("birth_volume_100"),
#'   center_characteristics_optimization_values = 1.75,
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
#'   outcome_goal = 0.85,
#'   outcome_goal_intention = "maximize",
#'   include_confidence_set = FALSE,
#'   quiet = TRUE
#' )
#'
#' intervention_components <- c("coaching_updt", "launch_duration")
#' predictors <- c(intervention_components, "birth_volume_100")
#'
#' cs <- get_confidence_set(
#'   predictors_data = BB_data[, predictors, drop = FALSE],
#'   intervention_components = intervention_components,
#'   outcome_data = BB_data$pp3_oxytocin_mother,
#'   fitted_model = opt$model,
#'   link = "logit",
#'   outcome_goal = 0.85,
#'   outcome_type = "binary",
#'   intervention_lower_bounds = c(1, 1),
#'   intervention_upper_bounds = c(40, 5),
#'   confidence_set_grid_step_size = c(1, 1),
#'   center_characteristics = "birth_volume_100",
#'   center_characteristics_optimization_values = 1.75,
#'   cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
#'   rec_int = opt$rec_int
#' )
#'
#' # Fraction of the grid inside the 95% confidence set: 18 of the 200 grid
#' # interventions qualify here (40 coaching values by 5 launch durations), so
#' # 0.09. print() shows the same number as a percentage.
#' cs$confidence_set_size_percentage
#'
#' # The confidence interval at the recommended intervention, reported in its
#' # own field as c(lower, upper). It is computed whether or not it covers the
#' # outcome goal, so it is available even when no grid intervention qualifies.
#' # lago_optimization() reports this interval as $est_outcome_ci.
#' cs$rec_int_ci
#'
#' # rec_int need not be one of the grid interventions, and here it is not:
#' # its launch_duration is about 2.78 while the grid steps through whole
#' # days. It is never a row of cs either, which holds the 18 qualifying grid
#' # interventions and nothing else.
#' opt$rec_int
#' head(cs$cs)
#'
#' @keywords internal
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
    link,
    outcome_goal,
    outcome_type,
    intervention_lower_bounds,
    intervention_upper_bounds,
    confidence_set_grid_step_size,
    center_characteristics = NULL,
    center_characteristics_optimization_values = 0,
    confidence_set_alpha = 0.05,
    cluster_id = NULL,
    cost_list_of_vectors,
    rec_int) {
  # Create a list to store sequences for each component
  sequences <- list()
  # Generate sequences for each intervention component
  for (i in seq_along(intervention_lower_bounds)) {
    sequences[[i]] <- seq(
      from = intervention_lower_bounds[i],
      to = intervention_upper_bounds[i],
      by = confidence_set_grid_step_size[i]
    )
  }
  # expand grid
  grid_x <- expand.grid(sequences)
  # the number of grid interventions, before the recommended intervention is
  # prepended. The confidence set and its size are reported over these
  # interventions only, so this is the denominator of the percentage.
  n_grid_rows <- nrow(grid_x)
  # add the rec_int values to the grid so that
  # the CI can be calculated for the recommended interventions.
  # rec_int is row 1 from here on, and the grid interventions are rows
  # 2 to n_rows. rec_int need not be one of the grid interventions.
  grid_x <- rbind(rec_int, grid_x)
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

  # the coefficient names glm gave the fixed-effect dummy columns. Every name
  # the assembly below can supply for itself is set aside first, so a covariate
  # whose own name begins with "center" or "period" is not mistaken for a
  # dummy. glm expands a factor into one dummy per non-reference level, in
  # level order, which is also the order the dummy columns are built in below.
  named_predictors <- gsub("`", "", c(
    "(Intercept)", intervention_components, additional_covariates,
    center_characteristics
  ))
  fixed_effect_coefs <- names(coef(fitted_model))[
    !gsub("`", "", names(coef(fitted_model))) %in% named_predictors
  ]

  # the names the assembled columns of one block carry. A block whose width and
  # number of coefficient names disagree cannot be matched at all, so it is
  # named unmatchably and reported by the coefficient check below instead of
  # failing here.
  block_names <- function(coef_names, n_cols, label) {
    if (length(coef_names) == n_cols) {
      coef_names
    } else {
      paste0(label, seq_len(n_cols))
    }
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
    # center_weights_for_outcome_goal is in center level order, so column i
    # above is the i-th non-reference center, which is the i-th center dummy.
    center_effect_names <- block_names(
      grep("^center", fixed_effect_coefs, value = TRUE, ignore.case = TRUE),
      n_centers,
      "unmatched center effect "
    )
  }

  # add time effects (if specified) to the data that
  # will be used for predictions
  if (include_time_effects) {
    # one column per time dummy, in period level order, so the last column is
    # the last period, which is the one set to 1 below
    time_effect_names <- grep(
      "^period", fixed_effect_coefs,
      value = TRUE, ignore.case = TRUE
    )
    n_periods <- length(time_effect_names)
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

  # assemble the new data for prediction. Each block is labelled with the
  # coefficient names its columns actually stand for, so the coefficients can
  # be matched to the columns by name below.
  components <- list()
  assembled_names <- character(0)
  if (include_center_effects) {
    components$center_effects <- repeated_center_effects_mat
    assembled_names <- c(assembled_names, center_effect_names)
  }
  if (include_time_effects) {
    components$time_effects <- repeated_time_effects_mat
    assembled_names <- c(assembled_names, time_effect_names)
  }
  components$new_grid <- new_grid
  assembled_names <- c(assembled_names, colnames(new_grid))
  if (length(additional_covariates) > 0) {
    # n_additional is length(additional_covariates), so the covariate names
    # already line up with the columns one for one
    components$additional <- repeated_additional_mat
    assembled_names <- c(assembled_names, additional_covariates)
  }
  if (length(center_characteristics) > 0) {
    # this block is as wide as center_characteristics_optimization_values,
    # which validate_inputs() requires to match center_characteristics in
    # length and order
    components$center_cha <- repeated_center_cha_mat
    assembled_names <- c(assembled_names, block_names(
      center_characteristics, n_center_cha,
      "unmatched center characteristic "
    ))
  }
  new_data <- as.data.frame(do.call(cbind, components))
  new_data <- cbind(Intercept = 1, new_data)
  # "(Intercept)" is the name glm() gives the intercept coefficient
  colnames(new_data) <- c("(Intercept)", assembled_names)

  # pair the coefficients with the columns they belong to BY NAME. The columns
  # above are assembled in the order outcome_model_fitting() builds the model
  # formula in, so a model fitted with its terms in some other order has its
  # coefficients in that other order too, and pairing them by position would
  # silently multiply every column by the wrong coefficient. Reordering the
  # coefficients rather than the columns also keeps new_data lined up with the
  # var-cov matrix the continuous branch builds from predictors_data.
  # Backticks are stripped from both sides before matching, since an
  # interaction term is named `a:b` in the model formula and glm keeps the
  # backticks in the coefficient name, while a caller may pass it either way.
  model_coefs <- coef(fitted_model)
  coef_keys <- gsub("`", "", names(model_coefs))
  column_keys <- gsub("`", "", colnames(new_data))
  coef_positions <- match(column_keys, coef_keys)
  # the match has to be one to one in both directions. A coefficient with no
  # column would otherwise be dropped silently, and a duplicated name on
  # either side would make the pairing ambiguous.
  if (anyNA(coef_positions) ||
    length(coef_keys) != length(column_keys) ||
    anyDuplicated(coef_keys) > 0 || anyDuplicated(column_keys) > 0) {
    describe <- function(x) {
      if (length(x) > 0) paste(x, collapse = ", ") else "none"
    }
    stop(paste0(
      "The coefficients of 'fitted_model' do not match the predictors the ",
      "confidence set is computed over.\n",
      "  ", length(model_coefs), " coefficient(s), ", ncol(new_data),
      " predictor(s)\n",
      "  coefficient(s) with no matching predictor: ",
      describe(names(model_coefs)[!coef_keys %in% column_keys]), "\n",
      "  predictor(s) with no matching coefficient: ",
      describe(colnames(new_data)[!column_keys %in% coef_keys]), "\n",
      "  predictor(s) named more than once: ",
      describe(unique(column_keys[duplicated(column_keys)])), "\n",
      "'fitted_model' must be fitted on exactly the intercept, the fixed ",
      "center and time effects, the intervention components, the additional ",
      "covariates and the center characteristics passed here. The order of ",
      "its terms does not matter."
    ))
  }
  model_coefs <- model_coefs[coef_positions]

  # get critical value based on the given alpha value
  critical_value <- qnorm(1 - confidence_set_alpha / 2)

  if (outcome_type == "binary") {
    # vcov() is indexed by the same coefficient names, so reorder it the same
    # way the coefficients were
    model_vcov <- vcov(fitted_model)[
      coef_positions, coef_positions,
      drop = FALSE
    ]
    new_data <- as.matrix(new_data)
    pred_all <- expit(new_data %*% model_coefs)
    se_pred_all <- sqrt(
      diag((new_data) %*% model_vcov %*% t(new_data))
    ) * pred_all * (1 - pred_all)

    # lower and upper bounds of predictions
    lb_prob_all <- pred_all - critical_value * se_pred_all
    ub_prob_all <- pred_all + critical_value * se_pred_all
    # the shared code below turns these bounds into the confidence set and
    # its size, for both outcome types
    ci_prob_all <- cbind(lb_prob_all, ub_prob_all)
  } else if (outcome_type == "continuous") {
    # link is either "logit" or "identity" in your usage
    # If link == "logit", use the logistic-like approach
    # If link == "identity", use linear approach.

    # define the function to manually calculate var-cov matrix
    get_vcov <- function(predictors_data,
                         model,
                         outcome_data,
                         cluster_ids = NULL,
                         link) {
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

      # logistic link single cluster vcov helper function
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

      # Identity link single cluster vcov helper function
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

      X <- prepare_design_matrix(predictors_data)
      n <- nrow(X)
      n_params <- ncol(X)
      fitted_values <- model$fitted.values

      if (is.null(cluster_ids)) {
        # Non-clustered case
        # no fixed center effects or fixed time effects
        if (link == "identity") {
          # For identity link (linear model):
          # sigma^2 = sum of squared residuals / (n - n_params)
          residuals <- outcome_data - fitted_values
          sigma2 <- sum(residuals^2) / (n - n_params)
          bread <- solve(t(X) %*% X)
          vcov_matrix <- bread * sigma2
        } else {
          # logistic-like approach
          # no fixed center effects or fixed time effects
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
      cluster_id,
      link = link
    )

    # get predicted values. vcov_matrix is not built from the fitted model's
    # coefficients but from predictors_data, which is in the assembly order,
    # so it is used as it comes and is not reordered with the coefficients.
    new_data <- as.matrix(new_data)
    pred_all <- (new_data %*% as.matrix(model_coefs))

    # Calculate standard errors for all rows
    std_er_all <- suppressWarnings({
      sqrt(rowSums((new_data %*% vcov_matrix) * new_data))
    })
    if (any(is.nan(std_er_all))) {
      warning(paste0(
        "Warning: High uncertainty for the confidence set,",
        " please consider checking for multicollinearity or",
        " dropping predictors."
      ))
    }

    # Calculate lower and upper bounds for predicted values
    lb_prob_all <- pred_all - critical_value * std_er_all
    ub_prob_all <- pred_all + critical_value * std_er_all

    if (link == "identity") {
      # For identity link, predictions are on the correct scale already.
      ci_prob_all <- cbind(lb_prob_all, ub_prob_all)
    } else {
      # For "logit" (or other logistic-like) link, apply expit.
      ci_prob_all <- expit(cbind(lb_prob_all, ub_prob_all))
    }
  }

  # rows whose interval could not be computed (an NA bound) cannot be tested
  # against the outcome goal, so they are dropped. Rows are identified by their
  # position in grid_x throughout, so keep the original row numbers rather than
  # renumbering the kept rows: ci_prob_all keeps its full length and only the
  # dropped rows are marked as not covering the goal.
  valid_rows <- complete.cases(ci_prob_all) # Identify rows without NA values

  # rows whose interval covers the outcome goal, i.e. lower <= goal <= upper.
  # findInterval() returns 1 exactly for that case. Indices are grid_x row
  # numbers, so row 1 is rec_int and rows 2+ are the grid interventions.
  covers_goal <- logical(n_rows)
  covers_goal[valid_rows] <- apply(
    ci_prob_all[valid_rows, , drop = FALSE],
    1,
    function(y) findInterval(x = outcome_goal, vec = y)
  ) == 1

  # the confidence interval at the recommended intervention, reported whether
  # or not it covers the outcome goal, so callers do not have to look for
  # rec_int inside the confidence set. NULL when it could not be computed.
  rec_int_ci <- if (valid_rows[1]) {
    c(
      lower = round(ci_prob_all[1, 1], 3),
      upper = round(ci_prob_all[1, 2], 3)
    )
  } else {
    NULL
  }

  # the confidence set is the set of GRID interventions whose interval covers
  # the outcome goal. Row 1 (rec_int) is excluded: it is reported through
  # rec_int_ci, and it is not necessarily a grid intervention.
  cs_row_indices <- which(covers_goal[-1]) + 1

  # the size of the confidence set as a fraction of the grid. Both the
  # numerator and the denominator count grid interventions only.
  confidence_set_size_percentage <- length(cs_row_indices) / n_grid_rows

  # no grid intervention's confidence interval covers the outcome goal, so
  # there is no confidence set to report. rec_int_ci is still returned, since
  # it does not depend on the confidence set being non-empty.
  if (length(cs_row_indices) == 0) {
    return(list(
      confidence_set_size_percentage = confidence_set_size_percentage,
      rec_int_ci = rec_int_ci,
      cs = NULL
    ))
  }

  cs <- grid_x[cs_row_indices, c(
    if (include_interaction_terms) {
      main_components
    } else {
      intervention_components
    }
  ), drop = FALSE]

  create_cost_function <- function(coeffs) {
    function(x) {
      sum(sapply(seq_along(coeffs), function(i) coeffs[i] * x^(i - 1)))
    }
  }
  cost_functions <- lapply(cost_list_of_vectors, create_cost_function)
  row_costs <- data.frame(
    row.names = rownames(cs),
    total_cost = apply(cs, 1, function(row) {
      sum(mapply(
        function(cost_fn, value) cost_fn(value),
        cost_functions,
        row
      ))
    })
  )

  if (length(center_characteristics) > 0) {
    original_cs_name <- names(cs)
    cs <- cbind(cs, center_characteristics_optimization_values)
    names(cs) <- c(original_cs_name, center_characteristics)
  }

  # cs_row_indices are grid_x row numbers, and ci_prob_all is indexed by the
  # same row numbers, so the bounds line up with the cs rows they belong to.
  cs_output_names <- names(cs)
  ci_lower_bound <- round(ci_prob_all[cs_row_indices, 1], 3)
  ci_upper_bound <- round(ci_prob_all[cs_row_indices, 2], 3)

  cs <- cbind(
    cs,
    ci_lower_bound,
    ci_upper_bound
  )
  cs <- cbind(
    cs,
    round(row_costs, 2)
  )
  names(cs) <- c(
    cs_output_names,
    "CI_lower_bound",
    "CI_upper_bound",
    "cost"
  )

  return(list(
    confidence_set_size_percentage = confidence_set_size_percentage,
    rec_int_ci = rec_int_ci,
    cs = cs
  ))
}
