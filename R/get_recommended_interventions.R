# The warning for an outcome goal that no intervention inside the bounds is
# estimated to reach, so that the recommendation falls back to shrinking
# towards the previous stage's intervention.
#
# Which extreme is out of reach, and in which direction, depends on the
# optimization direction: under "maximize" the goal is above everything
# reachable, and under "minimize" it is below everything reachable. Only the
# "maximize" sentence was ever written, and under "minimize" it named the wrong
# extreme and the wrong inequality, i.e. it described a comparison that had not
# happened. This was unreachable until the "minimize" goal constraint started
# binding, since a vacuous constraint is never unachievable.
unachievable_goal_message <- function(lower_outcome_goal) {
  reason <- if (lower_outcome_goal) {
    paste(
      "Since the minimum estimated achievable outcome\n",
      "is greater than the outcome goal,"
    )
  } else {
    paste(
      "Since the maximum estimated achievable outcome\n",
      "is less than the outcome goal,"
    )
  }
  paste(
    "The outcome goal is not estimated to be achievable.",
    reason,
    "we will shrink the recommended intervention towards the\n",
    "recommended intervention from the previous stage."
  )
}


#' rank_deficient_outcome_message
#'
#' @description The message for an optimization that had nothing to compare
#' because the OUTCOME MODEL could not be estimated, i.e. glm() returned NA for
#' at least one coefficient.
#'
#' @details This is the cause of the no-computable-outcome situation, as
#' distinct from the optimizer struggling with a problem it could in principle
#' solve, and the two need opposite advice. An aliased term's coefficient is
#' NA, the outcome computed from it is NA, and nothing that searches over
#' interventions can recover from that: every point it tries is NA whatever the
#' search. So this branch must NOT send the caller to the other optimization
#' method, which is what the single message here used to do for every cause. On
#' the only configuration known to reach the all-failed path at all, following
#' that advice reached the grid search's own goal comparison as the base-R error
#' "missing value where TRUE/FALSE needed", i.e. a second opaque failure after
#' being told to expect a solution.
#'
#' What the caller can act on is the model, so the message names the terms that
#' could not be estimated and says to drop or combine them.
#'
#' The condition is read off the fitted coefficients rather than off the NA
#' outcomes, so the terms can be NAMED: by the time an outcome is NA the NA has
#' been summed into the center-level effects and carries their names, not the
#' aliased term's.
#'
#' @param aliased_coef_names A character vector, the names of the coefficients
#' glm() could not estimate. Non-empty: the caller branches on that.
#'
#' @return A character string, the message to raise.
#'
#' @noRd
rank_deficient_outcome_message <- function(aliased_coef_names) {
  paste(
    "No outcome could be estimated at any of the interventions tried,",
    "because the outcome model is rank-deficient: the coefficient(s)",
    paste(aliased_coef_names, collapse = ", "),
    "could not be estimated (glm() returned NA for them), which happens when",
    "those predictors are collinear with others in the model. An outcome",
    "computed from an NA coefficient is NA, so no optimization method can",
    "proceed on this fit and changing the 'optimization_method' will not help.",
    "Drop or combine the collinear predictor(s) and fit again. Common causes",
    "are two intervention components that are rescalings of one another, and",
    "fixed center and time effects whose assignments are confounded rather",
    "than crossed. A model can also be saturated rather than collinear, with",
    "as many coefficients as it has rows to fit them from, which is what",
    "center-level data with one row per center gives once center effects are",
    "included: there dropping a predictor still leaves nothing to estimate the",
    "rest from, and what helps is more rows per center or fitting without the",
    "center effects."
  )
}


#' refuse_if_all_restarts_failed
#'
#' @description Internal guard for a multi-start numerical optimization in
#' which EVERY restart failed, so there is no result among them to choose
#' from.
#'
#' @details Both restart loops of the numerical optimizer record a failed
#' NlcOptim::solnl() call as NA and carry on, so that one bad starting point
#' does not lose the whole optimization. All of them failing is a different
#' situation: there is nothing to select, and the only thing to tell the
#' caller is what stopped the optimization and what to do about it.
#'
#' WHICH of those to say depends on the cause, and the two causes want opposite
#' advice, so the message branches on aliased_coef_names. A rank-deficient
#' outcome model gets rank_deficient_outcome_message(), which does not
#' recommend another optimizer, because none can help: every outcome is NA
#' before any search begins. The wording below is kept for the other case, a
#' genuine numerical-optimization failure on an estimable model, which is what
#' a caller in that position still needs. One message for both blamed a cause
#' ("more than three intervention components") that no reachable configuration
#' demonstrated, and recommended a method that fails on the same input.
#'
#' The condition and the message live here, once, rather than at each loop.
#' The max-achievable-outcome loop had no such check at all while the cost loop
#' had one, and the difference was not visible as a difference: an all-failed
#' max-outcome loop reached `which.max()` over all NAs, which is `integer(0)`,
#' so the outcome it indexed was `numeric(0)` and the goal comparison two
#' statements later failed with the R error "argument is of length zero". One
#' guard both loops call is what stops a third loop from being written without
#' one, and what keeps a single wording for a single situation.
#'
#' @param results A numeric vector, one entry per restart, holding the value
#' that restart converged to. NA marks a restart whose optimization failed.
#' @param aliased_coef_names A character vector, the names of the outcome
#' model's coefficients glm() could not estimate. Empty (the default) means the
#' model is of full rank, so an all-failed restart set is the optimizer's own
#' failure. Defaulted so that a call site with no model to hand still gets the
#' guard, with the wording it had.
#'
#' @return Invisibly NULL when at least one restart succeeded. Raises
#' otherwise, so the callers below can treat returning as "there is something
#' to choose from".
#'
#' @noRd
refuse_if_all_restarts_failed <- function(results,
                                          aliased_coef_names = character(0)) {
  if (all(is.na(results))) {
    if (length(aliased_coef_names) > 0) {
      stop(rank_deficient_outcome_message(aliased_coef_names))
    }
    stop(paste(
      "Numerical optimization failed to find a solution.",
      "Please consider using the 'grid_search' method by",
      "setting the 'optimization_method' parameter to",
      "'grid_search', and provide proper values for the",
      "'optimization_grid_search_step_size' parameter.",
      "This problem usually occurs when you have more than",
      "three intervention components."
    ))
  }
  invisible(NULL)
}


#' refuse_if_no_grid_outcome
#'
#' @description Internal guard for a grid search none of whose grid points has
#' a computable outcome, so there is nothing to compare against the goal.
#'
#' @details The grid search's counterpart to
#' refuse_if_all_restarts_failed(). The numerical optimizer records a failed
#' restart as NA and refuses the all-failed case by name; the grid search had no
#' such check, and an NA outcome flowed into `max(all_outcomes) >=
#' new_outcome_goal`, which is not FALSE for an NA but the R error "missing
#' value where TRUE/FALSE needed".
#'
#' Both guards exist for the same reason and both distinguish the same two
#' causes, so the wording is shared: a rank-deficient outcome model must not be
#' answered by recommending the other optimization method, because the other
#' method fails on the same fit.
#'
#' It refuses on ANY NA rather than on all of them, unlike the restart guard,
#' because the two situations differ. A failed restart is one starting point out
#' of eleven and the survivors are a legitimate answer, so only all-failed is
#' refused. A grid outcome is a function of the model, not of a starting point:
#' one NA there means the model cannot produce an outcome and every other grid
#' point's value is NA too. Refusing on any is also exactly the condition that
#' already failed, since max() propagates a single NA, so this narrows nothing.
#'
#' @param all_outcomes A numeric vector, the estimated outcome at each grid
#' intervention.
#' @param aliased_coef_names A character vector, the names of the outcome
#' model's coefficients glm() could not estimate. Empty means full rank; see
#' refuse_if_all_restarts_failed().
#'
#' @return Invisibly NULL when every grid outcome is a number. Raises
#' otherwise.
#'
#' @noRd
refuse_if_no_grid_outcome <- function(all_outcomes,
                                     aliased_coef_names = character(0)) {
  if (anyNA(all_outcomes)) {
    if (length(aliased_coef_names) > 0) {
      stop(rank_deficient_outcome_message(aliased_coef_names))
    }
    stop(paste(
      "The estimated outcome could not be computed at every intervention on",
      "the grid, so none of them can be compared against the outcome goal.",
      "This means the outcome model produced a missing estimated outcome.",
      "Please check the fitted outcome model and the values supplied for the",
      "center weights, the center characteristics and the time effect."
    ))
  }
  invisible(NULL)
}


#' select_restart_within_bounds
#'
#' @description Internal function that picks the recommended intervention out
#' of the points a multi-start numerical optimizer converged to, and returns it
#' together with the cost of the point being recommended.
#'
#' @details The filter, the fallback, the choice, the projection and the cost
#' recomputation are one decision and are deliberately kept together: each
#' step's correctness depends on the one before it.
#'
#' costs holds the cost each restart converged to, so the best restart is the
#' cheapest one. Every restart satisfies the outcome constraint, which
#' NlcOptim::solnl() enforces through confun, so cost is the only thing to
#' choose on among the ones that are actually implementable.
#'
#' solnl() treats the box as a soft constraint and will step a little outside it
#' to buy a lower objective, so the cheapest restart is systematically the one
#' furthest outside the bounds: selecting on cost alone selects for the
#' violation. Restarts that left the box are therefore dropped before the
#' comparison. If every restart left it they are all kept, so a recommendation
#' is still returned, and the survivor is brought back onto the box.
#'
#' The chosen restart can still sit a solver tolerance outside the box, when
#' every restart did. A recommendation has to be implementable, so it is
#' projected onto the bounds and its cost recomputed at the value actually being
#' recommended rather than reported from the point the solver stopped at. The
#' recomputed cost can be either side of the one the solver reported, since
#' projection moves a component up to a lower bound or down to an upper one, so
#' the solver's cost is not a bound on the recommendation's cost in either
#' direction.
#'
#' @param restart_points A numeric matrix with one column per restart and one
#' row per intervention component, holding the point each restart converged to.
#' @param costs A numeric vector, one entry per column of restart_points, with
#' the cost that restart converged to. NA marks a restart whose optimization
#' failed. Not every entry may be NA: the caller refuses that case with its own
#' message before calling this.
#' @param lower_bounds A numeric vector. The lower bounds of the intervention
#' components.
#' @param upper_bounds A numeric vector. The upper bounds of the intervention
#' components.
#' @param cost_fun A function of one numeric vector returning the total cost of
#' that intervention.
#'
#' @return A list with:
#' - int_components: the chosen intervention, projected onto the bounds.
#' - rec_int_cost: cost_fun() evaluated at int_components.
#'
#' @keywords internal
select_restart_within_bounds <- function(restart_points,
                                         costs,
                                         lower_bounds,
                                         upper_bounds,
                                         cost_fun) {
  in_box <- apply(
    restart_points, 2,
    function(x) {
      all(x >= lower_bounds) &&
        all(x <= upper_bounds)
    }
  )
  valid_indices <- which(!is.na(costs) & in_box)
  if (length(valid_indices) == 0) {
    valid_indices <- which(!is.na(costs))
  }
  min_position <- valid_indices[which.min(costs[valid_indices])]
  int_components <- restart_points[, min_position]

  int_components <- pmin(
    pmax(int_components, lower_bounds),
    upper_bounds
  )

  list(
    int_components = int_components,
    rec_int_cost = cost_fun(int_components)
  )
}


#' get_recommended_interventions
#'
#' @description Internal function that calculates the LAGO recommended
#' interventions based on an outcome goal and/or a power goal.
#'
#' @param data A data frame. The dataset that contains the intervention
#' components and the outcome variable.
#' @param intervention_components_coeff A numeric vector.
#' The coefficient estimates for the intervention components
#' including the intercept term. For example: c(0.1, 0.3, 0.15).
#' @param include_interaction_terms A boolean. Specifies whether there are
#' interaction terms in the intervention components.
#' @param main_components A character vector. Specifies the main intervention
#' components in the presence of interaction terms.
#' @param intervention_components A character vector. The names of the columns
#' in the dataset that represent the intervention components.
#' @param all_center_lvl_effects A numeric vector.
#' The coefficient estimates for the facilities, which includes the fixed
#' center effects and the fixed time effects.
#' @param center_weights_for_outcome_goal A numeric vector. Specifies the
#' weights of all facilities. The weights should sum up to 1.
#' @param center_cha_coeff_vec A numeric vector. The coefficients estimates for
#' the center characteristics.
#' For example: c(-0.4).
#' @param center_characteristics_optimization_values A numeric vector. The
#' fixed values of the center characteristics at which the recommended
#' intervention is computed, so the recommendation is specific to a center with
#' these characteristic values. Must have the same length and order as
#' center_characteristics.
#' For example: c(1.75)
#' @param cost_list_of_vectors A list of numeric vectors. Specifies the cost
#' functions for each intervention component. Each numeric vector in the list
#' contains coefficients of the cost function for one intervention component.
#' For example:
#' list(c(1, 2, 3, 4), c(4, 6), c(5, 4, 3)) represents:
#' - First component: cost = 1 + 2x_1 + 3x_1^2 + 4x_1^3
#' - Second component: cost = 4 + 6x_2
#' - Third component: cost = 5 + 4x_3 + 3x_3^2
#' list(c(0, 2), c(0, 6), c(0, 4)) represents:
#' - First component: cost = 2x_1
#' - Second component: cost = 6x_2
#' - Third component: cost = 4x_3
#' @param intervention_lower_bounds A numeric vector. Specifies the lower bounds
#' of the intervention components.
#' For example: for a two-component intervention package, lower bounds could be
#' c(0,0).
#' @param intervention_upper_bounds A numeric vector. Specifies the upper bounds
#' of the intervention components.
#' For example: for a two-component intervention package, upper bounds could be
#' c(10,20).
#' @param outcome_goal A numeric value. Specifies the outcome goal, a desired
#' probability or mean value.
#' @param optimization_method A character string. Specifies the method used for
#' LAGO optimization. Must be either "numerical" or "grid_search".
#' Default value without user specification: "numerical".
#' - Use "grid_search" if you want to exhaustively test every possible
#' intervention package compositions in LAGO optimization.
#' - Use "numerical" if you want to use gradient-based technique in LAGO
#' optimization.
#' @param optimization_grid_search_step_size A numeric vector. Specifies the
#' step size of the grid search algorithm used in LAGO optimization.
#' Default value without user specification:
#' 1/20 of the range for each intervention component.
#' @param link A character string. Specifies the link function used when fitting
#' the outcome model, either "logit" or "identity". These are the only links the
#' outcome machinery implements, see supported_outcome_links().
#' @param lower_outcome_goal A boolean value. Specifies whether the outcome goal
#' is intended to be lower or higher than the average outcome.
#' @param prev_recommended_interventions A numeric vector. Specifies the
#' recommended interventions from the previous stage.
#' @param shrinkage_threshold A numeric value. Specifies the threshold for
#' shrinking the recommended intervention towards the recommended intervention
#' from the previous stage.
#' @param power_goal A numeric value. Specifies the power goal, a desired
#' power value between 0 and 1.
#' @param power_goal_approach A character string. Specifies the approach used
#' to achieve the power goal. Must be either "unconditional" or "conditional".
#' @param num_centers_in_next_stage A numeric value. Specifies the number of
#' centers in the next stage.
#' @param patients_per_center_in_next_stage A numeric value.
#' Specifies the number of patients per center in the next stage.
#' @param outcome_name A character string. Specifies the name of the outcome
#' variable in the dataset.
#' @param icc A numeric value in [0, 1), or a length-2 numeric vector
#' c(control, treatment). Intra-cluster correlation used to inflate the
#' power-calculation variance by a design effect. NULL (default) reproduces the
#' original independent-observation behavior. Passed through to
#' get_power_desired_outcome.
#' @param power_goal_cluster_id A character string. The name of a column in the
#' data identifying the stage-1 centers, used to compute the stage-1 design
#' effect when icc is non-zero. Default NULL.
#' @param aliased_coef_names A character vector. The names of the outcome
#' model's coefficients glm() could not estimate, which the caller reads off
#' the fitted model. Empty (the default) means the model is of full rank. This
#' function is given coefficient VECTORS rather than the model, so it cannot
#' establish this for itself: an NA it sees has already been summed into the
#' center-level effects and no longer names the term it came from. Used only to
#' say which of the two causes stopped an optimization that could compute no
#' outcome at all, since the two need opposite advice.
#'
#' @return List(
#' est_rec_int = recommended interventions,
#' rec_int_cost = associated cost of the recommended interventions,
#' est_reachable_outcome = estimated outcome mean/probability for the
#' intervention group in the next stage,
#' shrinking_method_used = whether the shrinking method was applied,
#' effective_outcome_goal = the outcome goal actually used for optimization,
#' i.e. max(power-implied outcome, outcome_goal) )
#'
#' @keywords internal
#'
#' @importFrom rje expit logit
#' @import stats
#' @importFrom NlcOptim solnl
#'
get_recommended_interventions <- function(
    data,
    intervention_components_coeff,
    include_interaction_terms,
    main_components,
    intervention_components,
    all_center_lvl_effects,
    center_weights_for_outcome_goal,
    cost_list_of_vectors,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal,
    optimization_method,
    optimization_grid_search_step_size,
    center_cha_coeff_vec = 0,
    center_characteristics_optimization_values = 0,
    link = "identity",
    lower_outcome_goal = FALSE,
    prev_recommended_interventions,
    shrinkage_threshold,
    power_goal,
    power_goal_approach,
    num_centers_in_next_stage,
    patients_per_center_in_next_stage,
    outcome_name,
    icc = NULL,
    power_goal_cluster_id = NULL,
    aliased_coef_names = character(0)) {
  # check if power goal is null, if not, calculate the desired outcome
  # value needed to achieve the power goal
  if (!is.null(power_goal)) {
    power_desired_outcome <- get_power_desired_outcome(
      data,
      intervention_components_coeff,
      power_goal,
      power_goal_approach,
      num_centers_in_next_stage,
      patients_per_center_in_next_stage,
      outcome_name,
      icc = icc,
      power_goal_cluster_id = power_goal_cluster_id
    )

    effective_outcome_goal <- max(power_desired_outcome, outcome_goal)
  } else {
    effective_outcome_goal <- outcome_goal
  }

  # THE scale boundary on the way IN. outcome_goal arrives on the outcome scale
  # the caller stated it on, and everything below optimizes on the flipped
  # scale when lower_outcome_goal is TRUE (the "minimize" direction is
  # implemented by negating the fitted coefficients, see lago_optimization()).
  # So the flip is applied here, once, to the one goal the optimizers compare
  # against, using the same flip_outcome_scale() that the un-flip below
  # inverts. Doing it here rather than in the caller is what lets the original
  # goal be REPORTED as the caller supplied it: it is carried through
  # untouched in effective_outcome_goal instead of being recovered by flipping
  # a flipped copy. The two flips would not compose to the identity in floating
  # point on a logit link -- 1 - (1 - g) is off by up to an ulp of g -- so
  # round-tripping would hand the confidence set a goal that differs from the
  # user's in its last bits.
  new_outcome_goal <- if (lower_outcome_goal) {
    flip_outcome_scale(effective_outcome_goal, link)
  } else {
    effective_outcome_goal
  }

  # Function to create a cost function based on coefficients
  # in cost_list_of_vectors
  create_cost_function <- function(coeffs) {
    function(x) {
      sum(sapply(seq_along(coeffs), function(i) coeffs[i] * x^(i - 1)))
    }
  }

  # check if prev rec int is avaliable
  # if not, use the mean of the actual intervention values
  if (is.null(prev_recommended_interventions)) {
    if (include_interaction_terms) {
      all_components <- main_components
    } else {
      all_components <- intervention_components
    }
    # drop = FALSE keeps a one-column selection as a data.frame; without it,
    # data[, single_col] collapses to a vector and colMeans() errors.
    observed_mean_int_values <- colMeans(
      data[, all_components, drop = FALSE],
      na.rm = TRUE
    )
    # the shrinking method shrinks TOWARDS this vector and can return it
    # unchanged, so it is a candidate recommendation and has to be one the user
    # could actually implement, i.e. inside the bounds they gave. The observed
    # column means are a property of the DATA and need not be: the bounds
    # describe what the next stage may do, and validate_inputs() only warns
    # when they exclude values the data contains. Left unprojected, an
    # intervention the user's own bounds forbid was returned as the
    # recommendation. Projecting onto the box is the nearest intervention to
    # the observed mean that the bounds allow, which is what "shrink towards
    # what was done before" can mean when what was done before is off-limits
    # now. A user-supplied prev_recommended_interventions needs no projection:
    # validate_inputs() already rejects one outside the bounds.
    shrink_to_int_values <- pmin(
      pmax(observed_mean_int_values, intervention_lower_bounds),
      intervention_upper_bounds
    )
  } else {
    shrink_to_int_values <- prev_recommended_interventions
  }

  # implement the grid_search solution
  if (optimization_method == "grid_search") {
    # main optimization function using grid search
    optimize_cost_grid_search <- function(cost_params,
                                          lo,
                                          up,
                                          beta,
                                          outcome_goal,
                                          include_interaction_terms,
                                          main_components,
                                          intervention_components,
                                          all_center_lvl_effects,
                                          center_weights_for_outcome_goal,
                                          center_cha_coeff_vec,
                                          center_cha,
                                          step_size,
                                          link,
                                          shrinkage_threshold) {
      shrinking_method_used <- FALSE
      # create sequence grids for each intervention component
      grids <- lapply(seq_along(cost_params), function(i) {
        seq(lo[i], up[i], by = step_size[i])
      })
      # create the full grid
      full_grid <- do.call(expand.grid, grids)

      # create a new grid based on the full grid if there are
      # interaction terms. The full grid should only include
      # main effects, and the new grid should account for
      # the interaction terms properly.
      if (include_interaction_terms) {
        colnames(full_grid) <- main_components

        new_grid <- data.frame(
          matrix(
            nrow = nrow(full_grid),
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
            new_grid[, i] <- full_grid[, (components)]
          } else {
            # Multiple components - multiply the corresponding columns
            # Initialize with first component
            result <- full_grid[, (components[1])]
            # Multiply by remaining components
            for (j in 2:length(components)) {
              result <- result * full_grid[, (components[j])]
            }
            new_grid[, i] <- result
          }
        }
      } else {
        new_grid <- full_grid
      }

      # create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)

      # optimization function
      f_combined <- function(int, main_effects_int, link) {
        int_vector <- as.numeric(c(1, int))
        outcome <- get_outcome(
          center_weights_for_outcome_goal,
          all_center_lvl_effects,
          beta,
          int_vector,
          center_cha_coeff_vec,
          center_cha,
          link
        )

        # calculate the cost for this intervention
        cost <- sum(mapply(
          function(f, x) f(x),
          cost_functions,
          as.numeric(main_effects_int)
        ))

        return(list(outcome = outcome, cost = cost))
      }

      # apply f_combined to all grid points
      all_results <- mapply(
        function(i) f_combined(new_grid[i, ], full_grid[i, ], link),
        seq_len(nrow(new_grid)),
        SIMPLIFY = FALSE # so it rerturns a list
      )

      # extract outcomes and costs
      all_outcomes <- sapply(all_results, function(x) x$outcome)
      all_costs <- sapply(all_results, function(x) x$cost)

      # an NA among the grid outcomes is refused here, before it reaches the
      # goal comparison below. max() of anything holding an NA or NaN is NA, so
      # "NA >= goal" is not FALSE but the R error "missing value where
      # TRUE/FALSE needed", which is what this used to fail with -- no mention
      # of the outcome, the model, or what to do. This is a GUARD and not a
      # change of behaviour: anyNA(all_outcomes) holds exactly when
      # is.na(max(all_outcomes)) does, i.e. exactly on the inputs that already
      # failed, so no grid search that used to return one still raises.
      # The numerical optimizer reaches the same situation as an all-failed
      # restart set, since solnl() cannot optimize an NA objective either, and
      # refuse_if_all_restarts_failed() names it there.
      refuse_if_no_grid_outcome(all_outcomes, aliased_coef_names)

      # find the maximum outcome
      max_outcome <- max(all_outcomes)

      if (max_outcome >= new_outcome_goal) {
        # 1) if the outcome goal is achievable
        valid_indices <- which(all_outcomes >= new_outcome_goal)
        best_index <- valid_indices[which.min(all_costs[valid_indices])]

        est_rec_int <- as.numeric(full_grid[best_index, ])
        rec_int_cost <- all_costs[best_index]
      } else {
        warning(unachievable_goal_message(lower_outcome_goal))
        shrinking_results <- shrinking_method(
          lo = lo,
          up = up,
          beta = beta,
          outcome_goal = new_outcome_goal,
          include_interaction_terms = include_interaction_terms,
          intervention_components = intervention_components,
          main_components = main_components,
          all_center_lvl_effects = all_center_lvl_effects,
          center_weights_for_outcome_goal = center_weights_for_outcome_goal,
          center_cha_coeff_vec = center_cha_coeff_vec,
          center_cha = center_cha,
          link = link,
          stage_1_intervention = shrink_to_int_values
        )

        shrinking_method_used <- TRUE
        est_rec_int <- as.numeric(shrinking_results)

        cost_functions <- lapply(cost_params, create_cost_function)
        cost <- sum(mapply(
          function(f, x) f(x),
          cost_functions,
          as.numeric(est_rec_int)
        ))
        rec_int_cost <- cost
      }

      est_reachable_outcome <- get_est_reachable_outcome(
        x = est_rec_int,
        include_interaction_terms = include_interaction_terms,
        intervention_components = intervention_components,
        main_components = main_components,
        link = link,
        center_weights_for_outcome_goal = center_weights_for_outcome_goal,
        all_center_lvl_effects = all_center_lvl_effects,
        beta = beta,
        center_cha_coeff_vec = center_cha_coeff_vec,
        center_cha = center_cha
      )

      return(list(
        est_rec_int = est_rec_int,
        rec_int_cost = rec_int_cost,
        # left on the flipped scale, like everything else this optimizer
        # computes. It is put back on the original outcome scale once, at the
        # single un-flip in this function's return value below.
        est_reachable_outcome = est_reachable_outcome,
        shrinking_method_used = shrinking_method_used
      ))
    }

    # calls the grid search optimization function
    opt_results <- optimize_cost_grid_search(
      cost_list_of_vectors,
      intervention_lower_bounds,
      intervention_upper_bounds,
      intervention_components_coeff,
      outcome_goal,
      include_interaction_terms,
      main_components,
      intervention_components,
      all_center_lvl_effects,
      center_weights_for_outcome_goal,
      center_cha_coeff_vec,
      center_characteristics_optimization_values,
      optimization_grid_search_step_size,
      link,
      shrinkage_threshold
    )
  } else if (optimization_method == "numerical") {
    # we use the solnl() function from the NlcOptim library (Jingyu's idea)
    # the objective function is the function that we are trying to minimize:
    # total cost, or the sum of cost for each intervention component.

    # Main optimization function using solnl()
    optimize_cost_nlcoptim <- function(cost_params,
                                       lo,
                                       up,
                                       beta,
                                       outcome_goal,
                                       all_center_lvl_effects,
                                       center_weights_for_outcome_goal,
                                       center_cha_coeff_vec,
                                       center_cha,
                                       link) {
      shrinking_method_used <- FALSE
      # Objective function to maximize outcome
      obj_fun_for_max_outcome <- function(int) {
        int_vector <- get_int_vector(
          include_interaction_terms,
          intervention_components,
          main_components,
          int
        )

        # negative because NlcOptim minimizes this objective function by default
        return(
          -get_outcome(
            center_weights_for_outcome_goal,
            all_center_lvl_effects,
            beta,
            int_vector,
            center_cha_coeff_vec,
            center_cha,
            link
          )
        )
      }

      # get the max achievable outcome
      quantile_points <- 0:10 / 10
      results <- numeric(length(quantile_points))
      results_int_components <- matrix(
        0,
        nrow = length(lo),
        ncol = length(quantile_points)
      )
      for (i in seq_along(quantile_points)) {
        start_points <- lo + quantile_points[i] * (up - lo)
        result <- tryCatch(
          {
            NlcOptim::solnl(
              X = start_points,
              objfun = obj_fun_for_max_outcome,
              lb = lo,
              ub = up
            )
          },
          error = function(e) {
            return(NULL) # Return NULL if an error occurs
          }
        )

        if (!is.null(result)) {
          results[i] <- -result$fn
          results_int_components[, i] <- result$par
        } else {
          results[i] <- NA # Assign NA if the optimization failed
        }
      }
      # every restart having failed is refused here, before anything is read
      # out of results. which.max() over an all-NA vector is integer(0), so
      # the outcome below would be numeric(0) and the goal comparison further
      # down would fail with "argument is of length zero" instead of saying
      # what went wrong. Some restarts failing is not this case and is handled
      # by which.max() itself, which skips NAs: the surviving restarts are
      # compared and the winner's column of results_int_components is the
      # column that same restart wrote, since both are indexed by restart.
      # The aliased coefficient names are what lets the refusal say WHY, since
      # a rank-deficient model and a hard optimization both land here and want
      # opposite advice.
      refuse_if_all_restarts_failed(results, aliased_coef_names)
      max_position <- which.max(results)
      max_achievable_outcome <- results[max_position]

      # Create cost functions
      cost_functions <- lapply(cost_params, create_cost_function)
      # objective function for the total cost
      cost_obj_fun <- function(x) {
        return(sum(mapply(function(f, x) f(x), cost_functions, x)))
      }

      # 1) if the max achievable outcome is larger than the new outcome goal
      if (max_achievable_outcome >= new_outcome_goal) {
        # If the goal is achievable, get the recommended intervention that
        # minimizes the total cost function

        # a vector of constraint equations
        # (see helper doc of NlcOptim package for details)
        constraint_fun <- function(x) {
          f <- NULL

          int_vector <- get_int_vector(
            include_interaction_terms,
            intervention_components,
            main_components,
            x
          )

          f <- rbind(
            f,
            new_outcome_goal -
              get_outcome(
                center_weights_for_outcome_goal,
                all_center_lvl_effects,
                beta,
                int_vector,
                center_cha_coeff_vec,
                center_cha,
                link
              )
          )

          return(list(ceq = NULL, c = f))
        }

        cost_results <- numeric(length(quantile_points))

        for (i in seq_along(quantile_points)) {
          start_points <- lo + quantile_points[i] * (up - lo)
          result <- tryCatch(
            {
              NlcOptim::solnl(
                X = start_points, # Use start_points instead of a fixed midpoint
                objfun = cost_obj_fun,
                confun = constraint_fun,
                lb = lo,
                ub = up
              )
            },
            error = function(e) {
              return(NULL) # Return NULL if an error occurs
            }
          )

          if (!is.null(result)) {
            cost_results[i] <- result$fn
            results_int_components[, i] <- result$par
          } else {
            cost_results[i] <- NA # Assign NA if the optimization failed
          }
        }

        # if numerical solution fails to find a solution. The same refusal the
        # max-outcome loop above makes, from the same place and with the same
        # cause, so the two loops cannot come to describe the same situation
        # differently.
        refuse_if_all_restarts_failed(cost_results, aliased_coef_names)

        # Choosing among the restarts, and making the winner implementable, is
        # one decision and lives in select_restart_within_bounds(): the in-box
        # filter, the keep-everything fallback, the cheapest-survivor choice,
        # the projection onto the bounds and the cost recomputation at the
        # projected point. It is a package-level internal rather than inline
        # here so that each of those steps can be tested with a hand-built
        # restart matrix, with no solver, model or data in between. The last two
        # steps only do anything when EVERY restart left the box, which is
        # exceptional from the outside, so they are not otherwise reachable from
        # a test.
        selected <- select_restart_within_bounds(
          restart_points = results_int_components,
          costs = cost_results,
          lower_bounds = intervention_lower_bounds,
          upper_bounds = intervention_upper_bounds,
          cost_fun = cost_obj_fun
        )
        rec_int_cost <- selected$rec_int_cost

        est_rec_int <- selected$int_components

        est_reachable_outcome <- get_est_reachable_outcome(
          x = est_rec_int,
          include_interaction_terms = include_interaction_terms,
          intervention_components = intervention_components,
          main_components = main_components,
          link = link,
          center_weights_for_outcome_goal = center_weights_for_outcome_goal,
          all_center_lvl_effects = all_center_lvl_effects,
          beta = beta,
          center_cha_coeff_vec = center_cha_coeff_vec,
          center_cha = center_cha
        )
      } else {
        warning(unachievable_goal_message(lower_outcome_goal))

        shrinking_results <- shrinking_method(
          lo = lo,
          up = up,
          beta = beta,
          outcome_goal = new_outcome_goal,
          include_interaction_terms = include_interaction_terms,
          intervention_components = intervention_components,
          main_components = main_components,
          all_center_lvl_effects = all_center_lvl_effects,
          center_weights_for_outcome_goal = center_weights_for_outcome_goal,
          center_cha_coeff_vec = center_cha_coeff_vec,
          center_cha = center_cha,
          link = link,
          stage_1_intervention = shrink_to_int_values
        )
        shrinking_method_used <- TRUE
        est_rec_int <- as.numeric(shrinking_results)

        est_reachable_outcome <- get_est_reachable_outcome(
          x = est_rec_int,
          include_interaction_terms = include_interaction_terms,
          intervention_components = intervention_components,
          main_components = main_components,
          link = link,
          center_weights_for_outcome_goal = center_weights_for_outcome_goal,
          all_center_lvl_effects = all_center_lvl_effects,
          beta = beta,
          center_cha_coeff_vec = center_cha_coeff_vec,
          center_cha = center_cha
        )

        rec_int_cost <- cost_obj_fun(est_rec_int)
      }

      return(list(
        est_rec_int = est_rec_int,
        rec_int_cost = rec_int_cost,
        # left on the flipped scale, like max_achievable_outcome and everything
        # else this optimizer computes. It is put back on the original outcome
        # scale once, at the single un-flip in this function's return below.
        est_reachable_outcome = est_reachable_outcome,
        max_achievable_outcome = max_achievable_outcome,
        shrinking_method_used = shrinking_method_used
      ))
    }

    opt_results <- optimize_cost_nlcoptim(
      cost_list_of_vectors,
      intervention_lower_bounds,
      intervention_upper_bounds,
      intervention_components_coeff,
      outcome_goal,
      all_center_lvl_effects,
      center_weights_for_outcome_goal,
      center_cha_coeff_vec,
      center_characteristics_optimization_values,
      link = link
    )
  }

  # THE scale boundary. Everything above works on the flipped outcome scale
  # when lower_outcome_goal is TRUE (the "minimize" direction is implemented by
  # negating the fitted coefficients, see lago_optimization()), and every
  # caller below expects the original outcome scale. So the flip is undone
  # here, once, for every outcome-valued field and on every path, rather than
  # inside each optimizer. Each optimizer previously undid it for itself, with
  # its own copy of the inverse, which is how the two came to disagree with the
  # flip they were inverting.
  #
  # Only outcome-valued fields are un-flipped. est_rec_int and rec_int_cost are
  # an intervention and its cost: they live on the intervention scale, which
  # the flip never touched, so they pass through untouched.
  un_flip <- function(value) {
    if (lower_outcome_goal) flip_outcome_scale(value, link) else value
  }

  return(list(
    est_rec_int = opt_results$est_rec_int,
    rec_int_cost = opt_results$rec_int_cost,
    est_reachable_outcome = un_flip(opt_results$est_reachable_outcome),
    shrinking_method_used = opt_results$shrinking_method_used,
    # the effective outcome goal actually used for optimization:
    # max(power-implied outcome, outcome_goal). Equals outcome_goal when
    # no power goal is set, and the power-implied outcome when only a
    # power goal is set. Already on the original outcome scale: it is the value
    # the flip above was applied TO, so it is reported exactly as the caller
    # stated it rather than un-flipped back, and downstream consumers such as
    # the confidence set and the printout can use it directly.
    effective_outcome_goal = effective_outcome_goal
  ))
}
