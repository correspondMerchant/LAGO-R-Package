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
#' @param time_effect_optimization_value The period the confidence set is
#' computed at, as the value of the "period" column that identifies it. The
#' recommended intervention and the estimated outcome reported alongside the
#' confidence set are computed at this period, so the interval is computed at
#' it too. Required when include_time_effects is TRUE.
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
#' @param link A character string. The link function the interval is computed
#' on, either "logit" or "identity". These are the only links the outcome
#' machinery implements, see supported_outcome_links().
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
    time_effect_optimization_value = NULL,
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
  # the interval is built on the outcome scale of ONE of the links the package
  # implements, and both branches below assume it is one of those two: the
  # continuous branch applies expit() to anything that is not "identity", and
  # the binary branch applies it unconditionally. So any other link silently
  # produced a logit-scale interval regardless of what was asked for. This
  # function is exported and so reachable with any link at all, not only
  # through lago_optimization(), which is why the link is checked here as well
  # as in validate_inputs().
  if (!link %in% supported_outcome_links()) {
    stop(unsupported_link_message(link))
  }
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

  # the coefficient names glm gave each term of the fitted model, so every
  # block below can be labelled with the coefficients it actually stands for.
  # glm expands a factor term into one dummy coefficient per non-reference
  # level and names the dummies after the levels, so a term's own name is in
  # general not one of its coefficient names: a column 'arm' with levels
  # post/pre is a coefficient named 'armpre'. NULL when the mapping cannot be
  # established, in which case every term is taken to carry a coefficient of
  # its own name, which is what the checks below then report as unmatched.
  coef_mapping <- term_coef_names(fitted_model)
  model_coef_names <- names(coef(fitted_model))
  # every COEFFICIENT name the assembly below can supply for itself, set aside
  # so the fallback in fixed_effect_coef_names() does not take a covariate
  # whose own name begins with "center" or "period" for a fixed-effect dummy.
  # Coefficient names and not column names: a factor covariate's coefficient is
  # named after its level, so center_grp on its own never held back
  # center_grpb. See claimed_coef_names().
  named_predictors <- claimed_coef_names(fitted_model, coef_mapping, c(
    "(Intercept)", intervention_components, additional_covariates,
    center_characteristics
  ))

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
      fixed_effect_coef_names(
        "center", coef_mapping, model_coef_names, named_predictors
      ),
      n_centers,
      "unmatched center effect "
    )
  }

  # add time effects (if specified) to the data that
  # will be used for predictions
  if (include_time_effects) {
    # one column per time dummy, in period level order
    time_effect_names <- fixed_effect_coef_names(
      "period", coef_mapping, model_coef_names, named_predictors
    )
    n_periods <- length(time_effect_names)
    if (n_periods == 0) {
      stop(paste0(
        "'include_time_effects' is TRUE but no fixed time effect coefficient ",
        "could be identified in 'fitted_model'. The model must be fitted with ",
        "a 'period' term, as outcome_model_fitting() builds it, so the ",
        "confidence set can be computed at a single period."
      ))
    }
    # the confidence set is computed at ONE period, the same one the
    # recommended intervention and the reported estimated outcome are computed
    # at, so the interval and the point estimate refer to the same quantity.
    # It used to be hardcoded to the last period, so with any other requested
    # period the reported interval was the interval of a different period than
    # the reported point estimate, and could exclude it outright. The reference
    # period has no dummy of its own and is all-zero columns.
    indicators <- time_effect_indicator(
      fitted_model, time_effect_names, time_effect_optimization_value
    )
    repeated_time_effects <- rep(
      indicators,
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
    # one column per coefficient a covariate expands into, not one column per
    # covariate: a factor or character covariate is one coefficient per
    # non-reference level, so it needs that many columns. Every column is 0,
    # which is the value a numeric covariate has always been held at and, for
    # a factor, is its reference level, so the same "held at 0" convention
    # carries over one level down without any choice of level being made here.
    additional_names <- unlist(
      lapply(additional_covariates, predictor_coef_names, coef_mapping),
      use.names = FALSE
    )
    n_additional <- length(additional_names)
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
    # n_additional is length(additional_names), so the coefficient names the
    # covariates expand into already line up with the columns one for one
    components$additional <- repeated_additional_mat
    assembled_names <- c(assembled_names, additional_names)
  }
  if (length(center_characteristics) > 0) {
    # this block is as wide as center_characteristics_optimization_values,
    # which validate_inputs() requires to match center_characteristics in
    # length and order, so one value per characteristic. A characteristic that
    # is a factor with more than two levels expands to more coefficients than
    # that, and which of its dummies the single value belongs to is not
    # knowable, so the helper raises rather than guessing.
    center_cha_names <- center_characteristic_coef_names(
      center_characteristics, coef_mapping
    )
    components$center_cha <- repeated_center_cha_mat
    assembled_names <- c(assembled_names, block_names(
      center_cha_names,
      n_center_cha,
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
    # only the causes that actually apply are listed, so the message always
    # names what went wrong instead of reporting empty lists for the causes
    # that did not. A duplicated name on either side is a cause of its own and
    # is reported as such, since it can be the sole trigger.
    reason <- function(label, x) {
      if (length(x) > 0) {
        paste0("  ", label, ": ", paste(x, collapse = ", "), "\n")
      } else {
        ""
      }
    }
    reasons <- paste0(
      reason(
        "coefficient(s) with no matching predictor",
        names(model_coefs)[!coef_keys %in% column_keys]
      ),
      reason(
        "predictor(s) with no matching coefficient",
        colnames(new_data)[!column_keys %in% coef_keys]
      ),
      reason(
        "coefficient(s) named more than once",
        unique(coef_keys[duplicated(coef_keys)])
      ),
      reason(
        "predictor(s) named more than once",
        unique(column_keys[duplicated(column_keys)])
      )
    )
    stop(paste0(
      "The coefficients of 'fitted_model' do not match the predictors the ",
      "confidence set is computed over.\n",
      "  ", length(model_coefs), " coefficient(s), ", ncol(new_data),
      " predictor(s)\n",
      reasons,
      "'fitted_model' must be fitted on exactly the intercept, the fixed ",
      "center and time effects, the intervention components, the additional ",
      "covariates and the center characteristics passed here. Its factor and ",
      "character terms may expand to several coefficients each. The order of ",
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
    # link is either "logit" or "identity", the only links the outcome
    # machinery implements, see supported_outcome_links().
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
            # Create dummies for categorical variables, using the same
            # reference level and the same dummy order glm() uses, so this
            # matrix's columns pair with the model's coefficients. glm() drops
            # the FIRST LEVEL, not the first value it happens to meet, and
            # numbers the rest in level order, so unique() is wrong on both
            # counts: it can pick a different reference level and order the
            # dummies by first appearance. That made the variance estimate
            # depend on the row order of the input data.
            levels <- model_factor_levels(data[[col]])
            for (lev in levels[-1]) {
              dummy <- as.numeric(data[[col]] == lev)
              X <- cbind(X, dummy)
              colnames(X)[ncol(X)] <- paste0(col, lev)
            }
          } else if (is.logical(data[[col]])) {
            # glm() treats a logical column as a two-level factor whose only
            # dummy is TRUE, and names the coefficient accordingly
            X <- cbind(X, as.numeric(data[[col]]))
            colnames(X)[ncol(X)] <- paste0(col, "TRUE")
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

    # vcov_matrix is not built from the fitted model but from predictors_data,
    # so its rows and columns are in the column order of predictors_data, which
    # a caller need not supply in the assembly order. Pair them with the
    # assembled columns BY NAME, the same way the coefficients are, rather than
    # trusting the two orders to agree: a mismatch would multiply every column
    # by another column's variance and there would be nothing to show it.
    vcov_keys <- gsub("`", "", colnames(vcov_matrix))
    vcov_positions <- match(column_keys, vcov_keys)
    # The pairing must be one to one in BOTH directions. Matching only the
    # assembled columns would let an EXTRA column of predictors_data through:
    # it widens the matrix built from them, the extra row and column are
    # dropped by the subset below, and every remaining variance is silently
    # taken from the wrong fit.
    extra_vcov_keys <- setdiff(vcov_keys, column_keys)
    if (anyNA(vcov_positions) || anyDuplicated(vcov_keys) > 0 ||
      length(extra_vcov_keys) > 0) {
      stop(paste0(
        "The columns of 'predictors_data' do not match the predictors the ",
        "confidence set is computed over, so the variance-covariance matrix ",
        "built from them cannot be paired with them.\n",
        "  predictor(s) with no matching column of 'predictors_data': ",
        paste(
          if (anyNA(vcov_positions)) {
            colnames(new_data)[is.na(vcov_positions)]
          } else {
            "none"
          },
          collapse = ", "
        ), "\n",
        "  column(s) of 'predictors_data' matching no predictor: ",
        paste(
          if (length(extra_vcov_keys) > 0) extra_vcov_keys else "none",
          collapse = ", "
        ), "\n",
        "'predictors_data' must hold exactly the columns the model was ",
        "fitted on, i.e. 'center' and 'period' where the fixed effects are ",
        "included, the intervention components, the additional covariates and ",
        "the center characteristics. Their order does not matter."
      ))
    }
    vcov_matrix <- vcov_matrix[vcov_positions, vcov_positions, drop = FALSE]

    # get predicted values
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
  } else {
    # ci_prob_all is assigned in the two branches above and read by the shared
    # code below, so an unrecognised outcome type would reach that code with it
    # undefined and fail on "object 'ci_prob_all' not found" instead of saying
    # what is wrong. lago_optimization() validates outcome_type, so this is
    # reachable only through a direct call.
    stop(paste0(
      "'outcome_type' must be either \"binary\" or \"continuous\", not \"",
      outcome_type, "\"."
    ))
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
    # one column per center characteristic, each holding that
    # characteristic's own optimization value down every row: the confidence
    # set is computed for a single center, so every row of it shares the same
    # characteristic values. cbind()ing the value vector itself added ONE
    # recycled column rather than one per characteristic, which errored on the
    # name assignment below, or, when nrow(cs) was a multiple of the number of
    # characteristics, silently produced a single column cycling through the
    # values down the rows. validate_inputs() requires one value per
    # characteristic, in the same order, so the two line up here.
    original_cs_name <- names(cs)
    cs <- cbind(cs, matrix(
      rep(center_characteristics_optimization_values, each = nrow(cs)),
      nrow = nrow(cs)
    ))
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

# the coefficient names glm() gave each term of a fitted model, as a list whose
# names are the term labels of the model formula and whose elements are the
# coefficient names that term expands into. A term's own name is in general
# NOT one of its coefficient names: glm expands a factor term into one dummy
# per non-reference level and names each dummy after its level, so a column
# 'arm' with levels post/pre becomes a coefficient named 'armpre'.
# The mapping is read off the model matrix's "assign" attribute, which records
# the term each of its columns came from and is what R itself pairs
# coefficients with terms by. No name prefixes are compared, so a covariate
# whose name is a prefix of another term's name, e.g. dose beside dose2, can
# never be resolved to the other term's coefficients.
# Returns NULL when the mapping cannot be rebuilt or does not account for
# every coefficient, e.g. the data the model was fitted on is gone. A partial
# mapping is discarded rather than used to label some blocks and not others.
term_coef_names <- function(model) {
  mapping <- tryCatch(
    {
      design <- model.matrix(model)
      term_labels <- attr(terms(model), "term.labels")
      assign_idx <- attr(design, "assign")
      if (is.null(assign_idx) || length(term_labels) == 0) {
        NULL
      } else {
        setNames(
          lapply(
            seq_along(term_labels),
            function(i) colnames(design)[assign_idx == i]
          ),
          gsub("`", "", term_labels)
        )
      }
    },
    error = function(e) NULL
  )
  if (!is.null(mapping)) {
    covered <- c("(Intercept)", unlist(mapping, use.names = FALSE))
    if (!all(names(coef(model)) %in% covered)) {
      mapping <- NULL
    }
  }
  mapping
}

# the coefficient names one named predictor column stands for in the fitted
# model. Looked up in the term-to-coefficient mapping, which is exact. A
# predictor the mapping does not know, including the case of no mapping at
# all, is taken to carry a single coefficient of its own name, so it is
# reported as unmatched by the check in get_confidence_set() rather than
# paired with a coefficient that merely looks like it.
predictor_coef_names <- function(predictor, coef_mapping) {
  matched <- coef_mapping[[gsub("`", "", predictor)]]
  if (length(matched) == 0) predictor else matched
}

# the coefficient names of the fixed center or time effects, whose term is
# "center" or "period": outcome_model_fitting() always puts them in the model
# formula under those names, so the mapping names their dummies exactly.
# The fallback for a model whose mapping could not be rebuilt is the prefix
# search this used to do, restricted to the coefficients no other block can
# claim so that a covariate named like center_size or period_flag is not
# taken for a dummy.
#
# named_predictors is the COEFFICIENT names the callers claim, not the column
# names, which is what claimed_coef_names() builds for them. Excluding by
# column name only excluded a numeric predictor, whose single coefficient is
# named after the column; a factor or character predictor is one coefficient
# per non-reference level, named after the LEVEL, so center_grp in the list
# never excluded its coefficient center_grpb and the anchored search below
# claimed it as a center dummy.
fixed_effect_coef_names <- function(term,
                                    coef_mapping,
                                    model_coef_names,
                                    named_predictors) {
  mapped <- coef_mapping[[term]]
  if (length(mapped) > 0) {
    return(mapped)
  }
  unclaimed <- model_coef_names[
    !gsub("`", "", model_coef_names) %in% named_predictors
  ]
  grep(paste0("^", term), unclaimed, value = TRUE, ignore.case = TRUE)
}

# every coefficient name the caller's own named predictors account for, which
# is what fixed_effect_coef_names() must not claim as a fixed center or time
# effect. Both callers hold the same thing back: the intercept, the
# intervention components, the additional covariates and the center
# characteristics.
#
# A predictor's coefficient names are NOT in general its column name. A column
# the model contrast-codes carries one coefficient per contrast column, named
# after that contrast column and not after the term, so a column center_grp
# with levels a/b is a coefficient named center_grpb. Passing the column names
# alone therefore held back nothing for such a column, and its coefficient was
# claimed as a center dummy: all_center_lvl_effects came back one entry too
# long, the weights recycled against it, and the reported outcome was wrong by
# 5% with no indication. That is #68's defect on a factor covariate, and it is
# the reason this expansion exists rather than the raw names being passed.
#
# The expansion is done in two ways because the two apply in disjoint cases.
# predictor_coef_names() is exact and is used whenever the term mapping is
# available. It cannot help when the mapping is NULL, since it then returns the
# column name unchanged -- and NULL is exactly when fixed_effect_coef_names()
# consults this list at all. So for that case the coding the model was fitted
# under is used instead, via contrast_coef_names() below, which reads it off
# model$contrasts and model$xlevels. Both survive a fit made with
# model = FALSE whose data= name has since left scope, which is one way a
# mapping goes missing: model = FALSE alone leaves the mapping intact, since
# model.matrix() re-derives the frame from the data that is still reachable.
#
# Both expansions are unioned with the raw column names rather than replacing
# them: a numeric column IS its own coefficient name, so it is not
# contrast-coded and has no $contrasts entry to expand.
claimed_coef_names <- function(model, coef_mapping, named_predictors,
                               fixed_effect_terms = c("center", "period")) {
  columns <- gsub("`", "", named_predictors)
  # A column named exactly like a fixed-effect term is dropped rather than
  # resolved. Its dummies would be named paste0("center", level), which is how
  # the real center dummies are named, so nothing distinguishes them once the
  # term mapping is gone: holding them back would take every genuine dummy with
  # them and leave the fixed effects empty, which is worse than the
  # over-claiming this list prevents. Leaving such a column out means the search
  # below over-claims by exactly its coefficients, which the caller's
  # coefficient count check then refuses, so the collision is reported instead
  # of silently resolved either way.
  columns <- setdiff(columns, fixed_effect_terms)
  contrast_names <- unlist(
    lapply(columns, contrast_coef_names, model),
    use.names = FALSE
  )
  mapped_names <- unlist(
    lapply(columns, predictor_coef_names, coef_mapping),
    use.names = FALSE
  )
  unique(c(columns, contrast_names, mapped_names))
}

# the coefficient names one column of the fitted model was contrast-coded into,
# derived from the coding the model itself records rather than from a naming
# convention. Empty for a column the model did not contrast-code at all, e.g. a
# numeric one, whose single coefficient is its own column name.
#
# The names are not reconstructed from the levels. glm() names a dummy
# paste0(column, level) only under contr.treatment, and three supported column
# types are coded some other way:
#   - an ordered factor defaults to contr.poly, giving center_ord.L and
#     center_ord.Q rather than one dummy per level;
#   - a logical column is contrast-coded as a two-level factor FALSE/TRUE
#     (center_flagTRUE) but gets NO $xlevels entry, so a levels-driven
#     expansion saw fewer than two levels and held nothing back at all;
#   - a caller may set any coding through options(contrasts=) or the per-column
#     contrasts= argument of glm(), and contr.sum, contr.helmert and an unnamed
#     contrast matrix all name their dummies by POSITION: center_grp1,
#     center_grp2.
# Each of those was a name the model has and this function did not report, so
# the anchored search in fixed_effect_coef_names() claimed it as a center or
# period dummy: the #68 recycling shape, silently wrong by 10.5% on the
# ordered case.
#
# model$contrasts is the slot R records the coding in. It names every
# contrast-coded column, logicals included, and holds either the name of the
# contrast function or the contrast matrix itself. stats::contrasts() resolves
# either to the matrix, and its colnames() are the suffixes glm() appends --
# NULL colnames meaning the columns are numbered from 1, which is what
# model.matrix() does with them. The factor is rebuilt from the model's own
# levels with the recorded coding attached, so the answer depends on the fit
# and not on the caller's current options(contrasts=). The levels have to be
# carried over rather than left to factor(), which would sort them: under the
# default coding the suffixes ARE the levels, so a re-sorted rebuild names a
# dummy the model does not have.
#
# What this cannot do is tell whose coefficient a name belongs to. It appends a
# suffix to a column name, and the resulting string can be spelled the same as
# a coefficient of another term: a column "cent" with a level "erKakiri HC III"
# gives "centerKakiri HC III", the name of a real center dummy, and on the
# fallback that dummy is then held back from the center effects. The case that
# is actually reachable, a column named exactly like a fixed-effect term, is
# handled by claimed_coef_names() above. The rest is the residual ambiguity of
# looking coefficients up by name at all, which is why this path exists only
# for models whose own term mapping is gone.
contrast_coef_names <- function(column, model) {
  contrast <- model$contrasts[[column]]
  if (is.null(contrast)) {
    return(character(0))
  }
  # a logical column is coded as the two-level factor factor(x, c(FALSE, TRUE))
  # and so has no $xlevels entry of its own to read
  levels <- model$xlevels[[column]]
  if (is.null(levels)) {
    levels <- c("FALSE", "TRUE")
  }
  matrix <- tryCatch(
    {
      rebuilt <- factor(levels, levels = levels)
      attr(rebuilt, "contrasts") <- contrast
      stats::contrasts(rebuilt)
    },
    error = function(e) NULL
  )
  if (!is.matrix(matrix) || ncol(matrix) == 0) {
    return(character(0))
  }
  suffixes <- colnames(matrix)
  if (is.null(suffixes)) {
    suffixes <- as.character(seq_len(ncol(matrix)))
  }
  paste0(column, suffixes)
}

# the coefficient name each center characteristic stands for, one per
# characteristic and in the order they were given. A characteristic that is a
# factor or character column with more than two levels expands to more than one
# coefficient, and only one value per characteristic is supplied in
# center_characteristics_optimization_values, so nothing says which of its
# levels that value belongs to: say so rather than pick one, which would ignore
# the value the caller supplied or hold the characteristic at a level it did not
# ask for.
center_characteristic_coef_names <- function(center_characteristics,
                                            coef_mapping) {
  resolved <- lapply(
    center_characteristics, predictor_coef_names, coef_mapping
  )
  ambiguous <- center_characteristics[lengths(resolved) > 1]
  if (length(ambiguous) > 0) {
    stop(paste0(
      "The center characteristic(s) ", paste(ambiguous, collapse = ", "),
      " expand to more than one coefficient each in the outcome model, which ",
      "means they are factor or character columns with more than two ",
      "levels. Only one value per center characteristic is supplied, in ",
      "'center_characteristics_optimization_values', so there is no way to ",
      "say which of the levels the optimization should be computed at. ",
      "Center characteristics with more than two levels are not supported: ",
      "please recode them as numeric columns, or as one two-level column per ",
      "level, and pass a value for each."
    ))
  }
  unlist(resolved, use.names = FALSE)
}

# the levels of a column in the order glm() builds its dummies from, i.e. with
# the reference level first. glm() uses the factor's levels, dropping any that
# no row takes, and orders an unclassed character column's levels by sorting it,
# which is what factor() does.
model_factor_levels <- function(x) {
  if (is.factor(x)) {
    levels(droplevels(x))
  } else {
    levels(factor(x))
  }
}

# a 0/1 indicator over the fixed time effect dummies of the model that selects
# ONE period: 1 on the dummy of the requested period, 0 on the rest. The
# reference period has no dummy of its own, so it is all zeros, which is a
# legitimate answer rather than a failure to match.
# The period the caller asked for is resolved against the period levels the
# model was fitted on, which is the authoritative set, so it is never confused
# with a coefficient that merely looks like it and a value that is not a period
# at all raises rather than silently standing for the reference period.
# The names are matched exactly, not by prefix: glm() names a period dummy with
# the term name followed by the level, so the dummy of level "1" is exactly
# "period1" and never "period10".
time_effect_indicator <- function(model, time_effect_names, period_value) {
  if (length(period_value) != 1 || is.na(period_value)) {
    stop(paste0(
      "'time_effect_optimization_value' must be a single non-missing value ",
      "identifying the period to compute at when 'include_time_effects' is ",
      "TRUE."
    ))
  }
  period_levels <- model$xlevels[["period"]]
  wanted <- as.character(period_value)
  if (length(period_levels) > 0 && !wanted %in% period_levels) {
    stop(paste0(
      "'time_effect_optimization_value' (", wanted, ") is not one of the ",
      "periods the outcome model was fitted on, which are ",
      paste(period_levels, collapse = ", "),
      ". It must be one of the values of the 'period' column."
    ))
  }
  indicator <- rep(0, length(time_effect_names))
  hits <- which(time_effect_names == paste0("period", wanted))
  if (length(hits) == 1) {
    indicator[hits] <- 1
    return(indicator)
  }
  if (length(hits) == 0 && length(period_levels) > 0 &&
    wanted == period_levels[1]) {
    # the reference period, which glm() left out of the dummies on purpose
    return(indicator)
  }
  stop(paste0(
    "'time_effect_optimization_value' (", wanted, ") does not identify ",
    "exactly one fixed time effect of the outcome model. Its time effect ",
    "coefficient(s) are ", paste(time_effect_names, collapse = ", "),
    ", plus the reference period, which has no coefficient of its own."
  ))
}
