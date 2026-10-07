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
  # glm() needs two centers (or periods) with a usable row, else it fails on contrasts
  effects <- c(
    if (include_center_effects) "center",
    if (include_time_effects) "period"
  )
  # quiet, since the real fit below builds the same frame and reports its warnings
  frame <- if (length(effects) > 0) {
    tryCatch(
      suppressWarnings(glm(formula,
        data = data, family = family_object, weights = weights,
        method = "model.frame"
      )),
      error = function(e) NULL
    )
  }
  effects_need <- function(term) {
    paste0(
      "Fixed ", if (term == "center") "center" else "time", " effects need at ",
      "least two ", term, "s"
    )
  }
  # center level data always fits center effects, so turning them off is no remedy
  turn_off <- function(term) {
    if (term == "center" && input_data_structure == "center_level") {
      ""
    } else {
      paste0(
        ", or set include_", if (term == "center") "center" else "time",
        "_effects = FALSE"
      )
    }
  }
  no_usable_row <- paste0(
    "No row has every model variable and its glm weight (center_sample_size ",
    "for center level data) observed"
  )
  unusable <- !is.null(frame) && nrow(frame) == 0
  # the rows glm() would keep with one effect left out (only on the refusal paths)
  frame_without <- function(term) {
    tryCatch(
      suppressWarnings(glm(stats::update(formula, paste(". ~ . -", term)),
        data = data, family = family_object, weights = weights,
        method = "model.frame"
      )),
      error = function(e) NULL
    )
  }
  # too few levels in the data at all, whatever the na.action keeps
  for (term in if (is.null(frame)) character(0) else effects) {
    in_data <- levels(droplevels(as.factor(data[[term]])))
    if (length(in_data) >= 2) next
    # whether the other variables still have no usable row once this term is out
    rest <- if (unusable) frame_without(term)
    stop(paste0(
      effects_need(term),
      if (length(in_data) == 0) {
        paste0(
          ", but the data has no observed ", term, ". Fill in the ", term,
          " column"
        )
      } else {
        paste0(
          ", but the data has only the ", term, " '", in_data,
          "'. Add data from another ", term
        )
      },
      turn_off(term), ".",
      if (!is.null(rest) && nrow(rest) == 0) {
        paste0(" ", no_usable_row, " either, so fill those in too.")
      } else {
        ""
      }
    ))
  }
  if (unusable) {
    # an effect can be turned off instead when that leaves a fit the checks below pass
    helps <- Filter(function(term) {
      rest <- if (nzchar(turn_off(term))) frame_without(term)
      !is.null(rest) && nrow(rest) > 0 && all(vapply(
        setdiff(effects, term),
        function(other) length(levels(as.factor(rest[[other]]))) >= 2,
        logical(1)
      ))
    }, effects)
    stop(paste0(
      no_usable_row, ", so the outcome model cannot be fitted. Fill in the ",
      "missing values", paste0(vapply(helps, turn_off, ""), collapse = ""), "."
    ))
  }
  # the levels glm() would code, so a true NA kept by na.pass is not a level
  for (term in if (is.null(frame)) character(0) else effects) {
    used <- levels(as.factor(frame[[term]]))
    if (length(used) >= 2) next
    stop(paste0(
      effects_need(term), " with a row the outcome model can use (one with ",
      "every model variable and its glm weight, which is center_sample_size ",
      "for center level data, observed), but only the ", term, " '", used,
      "' has one. Fill in the missing values", turn_off(term), "."
    ))
  }
  # Categorical predictors use the first level as their reference.
  predictors <- all.vars(stats::delete.response(stats::terms(formula)))
  categorical_predictors <- Filter(function(term) {
    column <- data[[term]]
    is.factor(column) || is.character(column) || is.logical(column)
  }, predictors)
  effect_contrasts <- if (length(categorical_predictors) > 0) {
    stats::setNames(
      rep(list("contr.treatment"), length(categorical_predictors)),
      categorical_predictors
    )
  }
  # capture any warnings glm() emits during fitting (e.g. "fitted
  # probabilities numerically 0 or 1 occurred", which signals separation) so
  # they can be surfaced as fit diagnostics instead of being swallowed. The
  # fit still proceeds; glm warnings are not fatal.
  fit_warnings <- character(0)
  model <- withCallingHandlers(
    tryCatch(
      {
        glm(
          formula,
          data = data,
          family = family_object,
          weights = weights,
          contrasts = effect_contrasts
        )
      },
      error = function(e) {
        stop(paste("Error occurred during model fitting step:\n", e))
      }
    ),
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # if model did not converge, stop the function
  if (!model$converged) {
    stop(paste(
      "Model did not converge. Please check",
      "your input data and model specifications."
    ))
  }

  # Record the actual model formula in the fitted model's call. glm() captured
  # the `formula` symbol from this call site, so print()/summary() showed the
  # uninformative "glm(formula = formula, ...)", and how that symbol deparsed
  # changed across R versions (newer R substitutes the formula itself), which
  # made the print/summary snapshots version-dependent. Storing the formula
  # object makes the printed Call show the real model and be identical across R
  # versions.
  model$call$formula <- formula
  # Record the fitted coding, or none when every predictor is numeric.
  model$call$contrasts <- effect_contrasts

  # refuse a rank-deficient fit up front, but only when the aliasing lands on a
  # coefficient the optimization actually reads. glm() returns NA for a
  # coefficient it could not estimate -- two predictors carrying the same
  # information, or a saturated fit. An NA in a coefficient the optimizer uses
  # makes every predicted outcome NA and no optimization can then proceed, so
  # it is fatal and raised here rather than left to fail downstream. But an NA
  # in an ADDITIONAL COVARIATE is not fatal: rec_int_processor() never reads
  # those into any coefficient vector the optimizer sees, so the recommendation
  # is exactly the one the drop-that-covariate fit gives. Refusing on any NA
  # turned that usable run into an error, so the set below is exactly the
  # coefficients the optimizer will read: the intercept and intervention
  # components (interaction terms included, since they are part of
  # intervention_components), the fixed center effects, the fixed time effects,
  # and the center characteristics. It is built with the same
  # term-to-coefficient machinery rec_int_processor() uses to read them, so the
  # two cannot drift: a factor's coefficient is named after its level, not its
  # column, and is resolved through the model's term mapping rather than by
  # matching names. The names are also the only place the aliased TERMS can
  # still be named -- by the time an outcome is NA the NA has been summed into
  # the center-level effects and carries their names instead.
  all_coefs <- coef(model)
  coef_mapping <- term_coef_names(model)
  named_predictors <- claimed_coef_names(model, coef_mapping, c(
    "(Intercept)", intervention_components, additional_covariates,
    center_characteristics
  ))
  optimizer_coef_names <- c(
    "(Intercept)",
    intervention_components,
    if (include_center_effects) {
      fixed_effect_coef_names(
        "center", coef_mapping, names(all_coefs), named_predictors
      )
    },
    if (include_time_effects) {
      fixed_effect_coef_names(
        "period", coef_mapping, names(all_coefs), named_predictors
      )
    },
    if (!is.null(center_characteristics)) {
      unlist(
        lapply(center_characteristics, predictor_coef_names, coef_mapping),
        use.names = FALSE
      )
    }
  )
  optimizer_coefs <- all_coefs[optimizer_coef_names]
  aliased_coef_names <- names(optimizer_coefs)[is.na(optimizer_coefs)]
  if (length(aliased_coef_names) > 0) {
    stop(rank_deficient_outcome_message(aliased_coef_names))
  }

  # an aliased ADDITIONAL COVARIATE is not fatal -- the optimizer never reads
  # it, so the recommendation is unchanged from the fit without it -- but glm()
  # dropping it means it contributed nothing, which the caller may not have
  # intended. Warn, naming the covariate, and let the optimization continue, in
  # keeping with the other non-fatal fit diagnostics below.
  if (!is.null(additional_covariates)) {
    dropped_covariates <- Filter(function(covariate) {
      covariate_coefs <- predictor_coef_names(covariate, coef_mapping)
      any(is.na(all_coefs[covariate_coefs]))
    }, additional_covariates)
    if (length(dropped_covariates) > 0) {
      warning(paste0(
        "The additional covariate(s) ",
        paste(dropped_covariates, collapse = ", "),
        " could not be estimated by glm() (their coefficient(s) are NA), so ",
        "they were dropped from the fit and contribute nothing to the ",
        "recommended intervention. This usually means they are collinear with ",
        "other predictors. The recommendation is the one the fit without them ",
        "gives. If that is not intended, drop or combine the covariate(s)."
      ))
    }
  }

  # run non-fatal fit diagnostics. These only warn; LAGO optimization always
  # continues so the user still gets a recommended intervention, but is told
  # when the outcome model fit is questionable and the recommendation should
  # be interpreted with caution.
  diagnose_model_fit(
    model = model,
    intervention_components = intervention_components,
    fit_warnings = fit_warnings
  )

  list(
    model = model
  )
}

#' Non-fatal diagnostics for the fitted outcome model
#'
#' Checks the fitted glm for signs that the fit is unreliable and issues
#' warnings (never errors) so LAGO optimization can continue. Covers three
#' Tier-1 checks:
#'   1. glm fit warnings captured during fitting (separation signal).
#'   2. separation / near-non-identifiability, detected via extremely large
#'      coefficient standard errors relative to the estimates.
#'   3. intervention-component effects that are not statistically significant,
#'      which make the corresponding part of the recommendation unreliable.
#'
#' @param model A fitted glm object.
#' @param intervention_components A character vector of intervention component
#'   names (may include backticked interaction terms).
#' @param fit_warnings A character vector of warning messages emitted by glm()
#'   during fitting.
#' @return Invisibly NULL. Called for its side effect of issuing warnings.
#' @keywords internal
diagnose_model_fit <- function(model,
                               intervention_components,
                               fit_warnings = character(0)) {
  # 1. surface any warnings glm() emitted during fitting. The classic one is
  #    "fitted probabilities numerically 0 or 1 occurred", which indicates
  #    (quasi-)separation in a logistic fit.
  if (length(fit_warnings) > 0) {
    warning(paste0(
      "The outcome model fitting produced the following warning(s), which ",
      "may indicate an unreliable fit (e.g. separation):\n",
      paste0("  - ", unique(fit_warnings), collapse = "\n"),
      "\nThe LAGO optimization will still run, but please interpret the ",
      "recommended intervention with caution."
    ))
  }

  # pull the coefficient table; guard against models where it cannot be built.
  coef_summary <- tryCatch(
    stats::coef(summary(model)),
    error = function(e) NULL
  )
  if (is.null(coef_summary) || nrow(coef_summary) == 0) {
    return(invisible(NULL))
  }
  estimates <- coef_summary[, 1]
  std_errors <- coef_summary[, 2]

  # glm coefficient rownames keep the backticks that interaction terms are
  # wrapped in (e.g. `component1:component2`), so strip backticks from the
  # rownames to compare against the (also stripped) intervention component
  # names below.
  stripped_rownames <- gsub("`", "", rownames(coef_summary))

  # 2. separation / near-non-identifiability check: a hallmark of separation
  #    (even when glm reports convergence) is a coefficient whose standard
  #    error is huge both in absolute terms AND relative to its estimate.
  #    Both conditions are required: a large absolute SE alone can occur for a
  #    well-identified predictor on a very small scale (its natural
  #    coefficient is large), and a large SE-to-estimate ratio alone can occur
  #    for a legitimately near-null coefficient with an ordinary SE. Requiring
  #    both avoids flagging those healthy fits.
  large_se <- is.finite(std_errors) & is.finite(estimates) &
    std_errors > 1e3 &
    std_errors > 100 * abs(estimates)
  if (any(large_se)) {
    warning(paste0(
      "The outcome model has coefficient(s) with extremely large standard ",
      "errors, which often indicates separation or a near-singular fit:\n",
      paste0("  - ", stripped_rownames[large_se], collapse = "\n"),
      "\nThe LAGO optimization will still run, but the recommended ",
      "intervention may be unreliable. Consider checking for separation, ",
      "collinearity, or dropping predictors."
    ))
  }

  # 3. intervention-component significance check: if an intervention
  #    component's effect is not statistically significant, the optimization
  #    will still use its point estimate, but the recommendation for that
  #    component is not well supported by the data.
  # strip backticks used for interaction terms so names match the coef table.
  comp_names <- gsub("`", "", intervention_components)
  p_col <- if (ncol(coef_summary) >= 4) 4 else NULL
  if (!is.null(p_col)) {
    p_values <- coef_summary[, p_col]
    # match against backtick-stripped rownames so interaction components
    # (whose rownames retain backticks) are found.
    present_idx <- which(stripped_rownames %in% comp_names)
    nonsig_idx <- present_idx[
      is.finite(p_values[present_idx]) & p_values[present_idx] > 0.05
    ]
    if (length(nonsig_idx) > 0) {
      warning(paste0(
        "The following intervention component(s) do not have a statistically ",
        "significant association with the outcome (p > 0.05):\n",
        paste0("  - ", stripped_rownames[nonsig_idx], collapse = "\n"),
        "\nThe LAGO optimization will still run, but the recommendation for ",
        "these component(s) is not well supported by the data."
      ))
    }
  }

  invisible(NULL)
}
