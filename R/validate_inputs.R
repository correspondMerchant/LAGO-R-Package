validate_inputs <- function(
    data,
    input_data_structure = "individual_level",
    outcome_name,
    outcome_type,
    intervention_components,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal,
    outcome_goal_intention,
    power_goal,
    power_goal_approach,
    num_centers_in_next_stage = NULL,
    patients_per_center_in_next_stage = NULL,
    icc = NULL,
    power_goal_cluster_id = NULL,
    unit_costs = NULL,
    unit_cost_doses = NULL,
    default_cost_fxn_type = "cubic",
    cost_type_given = TRUE,
    cost_list_of_vectors = NULL,
    glm_family = "default",
    link = "default",
    optimization_method = "numerical",
    confidence_set_alpha = 0.05,
    weights = NULL,
    center_characteristics = NULL,
    center_characteristics_optimization_values = NULL,
    main_components = NULL,
    time_effect_optimization_value = NULL,
    additional_covariates = NULL,
    center_weights_for_outcome_goal = NULL,
    optimization_grid_search_step_size = NULL,
    confidence_set_grid_step_size = NULL,
    include_confidence_set = TRUE,
    include_center_effects = NULL,
    center_effects_optimization_values = NULL,
    include_time_effects = FALSE,
    include_interaction_terms = FALSE,
    prev_recommended_interventions = NULL,
    shrinkage_threshold = 0.25,
    quiet = FALSE) {
  # check if the input data is null
  if (is.null(data)) {
    stop("The argument 'data' is NULL.")
  }
  # check if the input data is a data frame type
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }
  # check if the input data is empty
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("The argument 'data' is empty.")
  }

  # check if the input data structure is a character type
  if (!is.character(input_data_structure)) {
    stop("The input_data_structure must be a character string.")
  }
  refuse_not_one_string(input_data_structure, "input_data_structure")
  # check if the input data structure is supported
  if (!(input_data_structure %in% c("individual_level", "center_level"))) {
    stop(paste(
      "\ninput_data_structure:", input_data_structure,
      "\ninput_data_structure must be either 'individual_level'",
      "or 'center_level'."
    ))
  }

  # check if the outcome name is a character type
  if (!is.character(outcome_name)) {
    stop("The outcome name must be a character string.")
  }
  refuse_not_one_string(outcome_name, "outcome_name")
  # check if the outcome name is empty
  if (nchar(outcome_name) == 0) {
    stop("The outcome name is empty.")
  }
  # check if the outcome name is one of the columns in the data frame
  if (!(outcome_name %in% names(data))) {
    stop("The outcome name must be presented in the provided input data.")
  }
  # an outcome with no observed value leaves nothing to fit or to compare a
  # goal against (its mean with na.rm = TRUE would be NaN)
  if (all(is.na(data[[outcome_name]]))) {
    stop(paste0(
      "The outcome column '", outcome_name, "' has no observed values."
    ))
  }

  # check if the outcome type is a character type
  if (!is.character(outcome_type)) {
    stop("The outcome type must be a character string.")
  }
  refuse_not_one_string(outcome_type, "outcome_type")
  # check if the outcome type is either continuous or binary
  allowed_outcome_types <- c("continuous", "binary")
  if (!(outcome_type %in% allowed_outcome_types)) {
    stop(paste(
      "Outcome type", outcome_type,
      "must be either 'continuous' or 'binary'."
    ))
  }
  # a factor or character outcome has no mean to compare the goal against
  outcome_column <- data[[outcome_name]]
  if (!is.numeric(outcome_column) && !is.logical(outcome_column)) {
    stop(paste0(
      "The outcome column '", outcome_name, "' must be numeric",
      if (outcome_type == "binary") ", with 0 and 1 for a binary outcome",
      ", but it is a ", class(outcome_column)[1], " column. Recode it, for ",
      "example with ",
      if (outcome_type == "binary") {
        paste0("as.numeric(data$", outcome_name, " == \"yes\") for yes and no values, or ")
      },
      "as.numeric(as.character(data$", outcome_name, ")) for values written as numbers."
    ))
  }
  # check if the input data structure is center_level when
  # the outcome type is continuous.
  if (input_data_structure == "center_level") {
    if (outcome_type == "continuous") {
      stop(paste(
        "For continuous outcomes, LAGO requires individual",
        "level data for the optimization step."
      ))
    }
  }

  # check if the input data has all the required columns
  required_columns <- c()
  if (input_data_structure == "center_level") {
    required_columns <- c(
      required_columns,
      "proportion",
      "center_sample_size"
    )
  }

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(sprintf(
      "The following required columns are missing: %s",
      paste(missing_columns, collapse = ", ")
    ))
  }
  # center level data is fitted on its proportion column, so the outcome must be it
  if (input_data_structure == "center_level" && !identical(outcome_name, "proportion")) {
    stop(paste0(
      "For center level data the outcome is the 'proportion' column, so ",
      "outcome_name must be \"proportion\", not \"", outcome_name, "\"."
    ))
  }
  # a binary outcome, or the proportion of center level data, is between 0 and
  # 1 on the rows the model fits. The other columns are checked later, so the
  # rows are judged here on the model columns the data has.
  binary_rows <- rows_model_fits(
    data,
    c(
      outcome_name, intervention_components, center_characteristics,
      additional_covariates, if (isTRUE(include_center_effects)) "center",
      if (isTRUE(include_time_effects)) "period"
    ),
    if (input_data_structure == "center_level") data$center_sample_size else weights
  )
  binary_outcome <- outcome_column[binary_rows & !is.na(outcome_column)]
  outside_binary <- binary_outcome[binary_outcome < 0 | binary_outcome > 1]
  if (outcome_type == "binary" && length(outside_binary) > 0) {
    stop(paste0(
      "The outcome column '", outcome_name, "' has values outside 0 to 1, ",
      "such as ", format(outside_binary[1], digits = 10), ". ",
      if (input_data_structure == "center_level") {
        "For center level data it must be the proportion of events in each row."
      } else {
        "A binary outcome must be between 0 and 1, and is usually 0 or 1."
      }
    ))
  }
  if (input_data_structure == "center_level" && "center_sample_size" %in% names(data)) {
    sizes <- data$center_sample_size
    if (!is.numeric(sizes) || any(!is.finite(sizes) & !is.na(sizes)) ||
      any(sizes < 0, na.rm = TRUE)) {
      stop(paste(
        "The 'center_sample_size' column must hold numbers that are finite",
        "and not negative, the number of patients in each row."
      ))
    }
  }

  # check if the glm family is a character type
  if (!is.character(glm_family)) {
    stop("The glm family must be a character string.")
  }
  refuse_not_one_string(glm_family, "glm_family")
  # assign default glm_family based on the outcome type
  if (glm_family == "default") {
    if (outcome_type == "continuous") {
      glm_family <- "gaussian"
    } else if (outcome_type == "binary") {
      glm_family <- "binomial"
    }
  }

  # check if the link option is a character type
  if (!is.character(link)) {
    stop("The link option must be a character string.")
  }
  refuse_not_one_string(link, "link")
  # check if the provided link option is supported. The set is
  # supported_outcome_links(), the one place the links the outcome machinery
  # implements are written down, plus "default", which is resolved to one of
  # them from the glm family just below.
  # "probit" and "log" used to be accepted here and are not any more. Nothing
  # implemented their inverse links, so get_outcome() fell through to its
  # identity branch and reported the LINEAR PREDICTOR as the estimated
  # probability or mean: a probit fit asked to hold the outcome at most 0.20
  # reported 0.200 as reached while the actual probability was pnorm(0.20) =
  # 0.579, and get_confidence_set() built its interval on the wrong scale too.
  # Accepting a link only some of the machinery handles reports wrong numbers
  # with no sign anything is wrong, so the link is refused up front instead.
  supported_link_options <- c(supported_outcome_links(), "default")
  if (!(link %in% supported_link_options)) {
    stop(unsupported_link_message(link))
  }
  # assign link based on the glm_family
  if (link == "default") {
    # a quasibinomial outcome is a proportion, so it takes the logit link too
    if (glm_family %in% c("binomial", "quasibinomial")) {
      link <- "logit"
    } else if (glm_family == "gaussian") {
      link <- "identity"
    }
  }

  # check if the outcome goal optimization method is a character type
  if (!is.character(optimization_method)) {
    stop("The optimization_method must be a character string.")
  }
  refuse_not_one_string(optimization_method, "optimization_method")
  # check if the outcome goal optimization method is one of the defined methods
  optimization_method_list <- c("numerical", "grid_search")
  if (!(optimization_method %in% optimization_method_list)) {
    stop(paste(
      "optimization_method must be one of the supported methods.",
      "The supported methods are:", optimization_method_list, "."
    ))
  }

  # center level rows are weighted by their center_sample_size
  if (!is.null(weights) && input_data_structure == "center_level") {
    input_warning(paste(
      "'weights' is ignored for center level data, whose rows are weighted",
      "by their center_sample_size."
    ))
  }
  # check if the provided weights option is numeric
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      stop("The 'weights' option must be a numeric vector.")
    }
    # check if the provided weights option has the same length as the number of
    # observations. Use nrow(data) rather than length(data[, comp]): the latter
    # returns the observation count for a base data.frame but the column count
    # for a tibble or a single-column drop, so it mis-validates weights.
    if (length(weights) != nrow(data)) {
      stop(paste0(
        "The length of the weights must be the same as the ",
        "number of observations in the provided input data."
      ))
    }
    # glm() refuses negative weights. All zero weights are named later, with
    # the rows the model can use.
    # a row with an NA weight is left out of the fit, as glm() leaves it out
    if (any(is.infinite(weights)) || any(weights < 0, na.rm = TRUE)) {
      stop("The 'weights' must not be negative or infinite.")
    }
  }

  # check if intervention_components is a character vector type
  if (!(is.vector(intervention_components) &&
    is.character(intervention_components))) {
    stop("Interventions list must be a character vector.")
  }
  # check if intervention_components are all columns in the data frame
  if (!all(intervention_components %in% names(data))) {
    missing_interactions <- setdiff(
      intervention_components[grepl(":", intervention_components, fixed = TRUE)],
      names(data)
    )
    stop(paste0(
      "All elements in intervention_components ",
      "must be columns in the data frame.",
      if (length(missing_interactions) > 0) {
        example <- strsplit(missing_interactions[1], ":", fixed = TRUE)[[1]]
        paste0(
          " The interaction term(s) ",
          paste0("'", missing_interactions, "'", collapse = ", "),
          " are not columns. An interaction term needs a column with that ",
          "name that holds the product of its components, for example ",
          "data[[\"", missing_interactions[1], "\"]] <- ",
          paste0("data$", example, collapse = " * "), "."
        )
      }
    ))
  }
  # a column given twice would be counted as two components
  refuse_repeated_columns(intervention_components, "intervention_components")
  # refuse any intervention component whose column is entirely NA. glm()'s
  # internal na.omit drops every row in that case, and the fit dies with an
  # opaque error ("Argument mu must be a nonempty numeric vector") that never
  # names the component. This runs after the column-existence check above so
  # data[[component]] is always a real column, and before the interaction-term
  # backticks are added below so the names still match the data columns.
  # all(is.na(col)) treats numeric, factor and character columns alike, so an
  # all-NA component of any type is caught here rather than left to fail
  # cryptically inside model fitting.
  all_na_components <- intervention_components[vapply(
    intervention_components,
    function(component) all(is.na(data[[component]])),
    logical(1)
  )]
  if (length(all_na_components) > 0) {
    stop(paste0(
      "The intervention component(s) ",
      paste(all_na_components, collapse = ", "),
      " are entirely NA, so glm() would drop every row and they ",
      "cannot enter the model. Remove them from intervention_components ",
      "or supply observed values."
    ))
  }
  # a component is a dose, and glm() would code a non-numeric one as levels
  non_numeric_components <- intervention_components[!vapply(
    intervention_components,
    function(component) is.numeric(data[[component]]),
    logical(1)
  )]
  if (length(non_numeric_components) > 0) {
    stop(paste0(
      "The intervention component(s) ",
      paste0("'", non_numeric_components, "'", collapse = ", "),
      " must be numeric columns. Recode a yes or no component as 0 and 1, ",
      "for example with as.numeric()."
    ))
  }

  # check if center_characteristics is a character vector type
  if (length(center_characteristics) > 0) {
    if (!(is.vector(center_characteristics) &&
      is.character(center_characteristics))) {
      stop("center_characteristics must be a character vector.")
    }
    # check if center_characteristics are all columns in the data frame
    if (!all(center_characteristics %in% names(data))) {
      stop(paste0(
        "All elements in center_characteristics ",
        "must be columns in the data frame."
      ))
    }
    # a characteristic given twice would be fitted and set twice
    refuse_repeated_columns(center_characteristics, "center_characteristics")
    center_characteristics_optimization_values <-
      drop_1d_array(center_characteristics_optimization_values)
    # check if center_characteristics_optimization_values
    # is a numeric vector type
    if (!(is.vector(center_characteristics_optimization_values) &&
      is.numeric(center_characteristics_optimization_values))) {
      stop(paste0(
        "center_characteristics_optimization_values ",
        "must be a numeric vector."
      ))
    }
    # check if center_characteristics_optimization_values is not null
    if (is.null(center_characteristics_optimization_values)) {
      stop(paste0(
        "center_characteristics_optimization_values is NULL. ",
        "You decided to include center characteristics in ",
        "the model, please either provide values of the ",
        "center characteristics for LAGO optimization, ",
        "or consider dropping the center characteristics."
      ))
    }
    # check if length of center_characteristics_optimization_values is the same
    # as the length of center_characteristics
    if (length(center_characteristics) !=
      length(center_characteristics_optimization_values)) {
      stop(paste(
        "The length of center_characteristics must be the",
        "same as the length of",
        "center_characteristics_optimization_values."
      ))
    }
    # a missing or infinite value names no center, and glm() cannot use it
    if (any(!is.finite(center_characteristics_optimization_values))) {
      stop(paste(
        "center_characteristics_optimization_values must be finite numbers,",
        "one per center characteristic. NA, NaN, Inf and -Inf name no center."
      ))
    }
    center_characteristics_optimization_values <- match_by_names(
      center_characteristics_optimization_values, center_characteristics,
      "center_characteristics_optimization_values", "center characteristics"
    )
  }

  # if include_center_effects is not specified, set the default value: TRUE
  # for center level data, FALSE otherwise. A value the user gave is kept.
  if (is.null(include_center_effects)) {
    include_center_effects <- input_data_structure == "center_level"
  }
  # check whether the include_center_effects indicator is boolean
  if (!is.logical(include_center_effects) || length(include_center_effects) != 1 ||
    is.na(include_center_effects)) {
    stop("The include_center_effects indicator must be a boolean.")
  }

  # check whether center_characteristics and center effects are both included
  if (include_center_effects && !is.null(center_characteristics)) {
    stop(paste(
      "Fixed center effects and center characteristics cannot be",
      "both included in the outcome model. Please",
      "pick one to use."
    ))
  }

  # check if the input data has the "center" column if
  # include_center_effects is TRUE
  if (include_center_effects) {
    if (!"center" %in% names(data)) {
      stop(paste(
        "To include fixed center effects in the outcome model",
        "please make sure that the input data has a 'center'",
        "column to identify facilities."
      ))
    }
    # refused here, as the center weights below would otherwise fail with an unclear message
    if (all(is.na(data$center))) {
      stop(paste0(
        "Fixed center effects need at least two centers, but the data has no ",
        "observed center. Fill in the center column, or set ",
        "include_center_effects = FALSE."
      ))
    }
    # if "center" column of the input data is not a factor,
    # change it to factor and print a warning message
    if (!is.factor(data$center)) {
      data$center <- as.factor(data$center)
      input_note(paste(
        "'center' column is not a factor type. To ensure",
        "the correct model fit, it has been converted",
        "to the factor type."
      ))
    }
    # an unordered factor, so it is treated as center labels and not as a scale
    if (is.ordered(data$center)) {
      class(data$center) <- "factor"
      input_note(paste(
        "'center' column is an ordered factor. It has been converted to an",
        "unordered factor with the same levels, so the model fits one effect",
        "per center."
      ))
    }
    # one level per center present, as glm() drops unused levels (align handles lost ones)
    data$center <- droplevels(data$center)
  }

  # preliminary check for center_weights_for_outcome_goal
  if (!is.null(center_weights_for_outcome_goal)) {
    # check if include center effects is set to FALSE
    if (!include_center_effects) {
      stop(paste(
        "'include_center_effects' must be set to TRUE,",
        "for values of 'center_weights_for_outcome_goal'",
        "to be used."
      ))
    }
    # check if center_weights_for_outcome_goal is a numeric vector
    if (!is.numeric(center_weights_for_outcome_goal)) {
      stop(paste("center_weights_for_outcome_goal must be a numeric vector."))
    }
    # check if center_weights_for_outcome_goal has the same length as the
    # number of facilities provided in the input data
    # the centers present (a missing center id is not a facility)
    n_facilities <- nlevels(data$center)
    if (length(center_weights_for_outcome_goal) != n_facilities) {
      stop(paste(
        "The length of center_weights_for_outcome_goal must match",
        "the number of unique facilities in the input data."
      ))
    }
    # named weights are matched to the centers by name, not by position
    weight_names <- names(center_weights_for_outcome_goal)
    if (!is.null(weight_names)) {
      center_levels <- levels(data$center)
      # an explicit NA level is named "NA", or NA
      level_keys <- ifelse(is.na(center_levels), "NA", center_levels)
      name_keys <- ifelse(is.na(weight_names), "NA", weight_names)
      if (anyDuplicated(name_keys) > 0 || !setequal(name_keys, level_keys)) {
        stop(paste0(
          "The names of center_weights_for_outcome_goal must be the centers ",
          "in the 'center' column, each once: ",
          paste0("'", center_levels, "'", collapse = ", "),
          ". Fix the names, or remove them with unname() to match the ",
          "weights to the centers in this order."
        ))
      }
      center_weights_for_outcome_goal <-
        unname(center_weights_for_outcome_goal[match(level_keys, name_keys)])
    }
    if (!is.null(center_effects_optimization_values)) {
      input_warning(paste(
        "Both 'center_weights_for_outcome_goal' and",
        "'center_effects_optimization_values' are given. The center weights",
        "are used and the named center is ignored. Give only one of them."
      ))
    }
  } else {
    # if center_weights_for_outcome_goal is not specified, but
    # include_center_effects is set to TRUE, we calculate its default values
    if (include_center_effects) {
      # if not specifing center_effects_optimization_values,
      # then we calculate the default values for center weights
      if (is.null(center_effects_optimization_values)) {
        if (input_data_structure == "center_level") {
          # each center's size is its patients, the sum over its rows, as glm() weights them
          center_sizes <- tapply(
            data$center_sample_size,
            data$center,
            function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
          )
          no_size <- names(center_sizes)[is.na(center_sizes)]
          if (length(no_size) > 0) {
            stop(paste0(
              "center_sample_size is missing for every row of the center(s) ",
              paste0("'", no_size, "'", collapse = ", "), ", so they have no ",
              "size to weight them by. Fill in their sample size, or drop them."
            ))
          }
          total_sample_size <- sum(center_sizes)
        } else if (input_data_structure == "individual_level") {
          total_sample_size <- sum(as.numeric(table(data$center)))
          center_sizes <- as.numeric(table(data$center))
        }
        center_weights_for_outcome_goal <- center_sizes / total_sample_size
      } else {
        # if center_effects_optimization_values is specified,
        # we set the weight for that specific center to 1, rest to 0
        if (!is.character(center_effects_optimization_values)) {
          stop(paste(
            "center_effects_optimization_values must be a character string",
            "naming one center."
          ))
        }
        if (length(center_effects_optimization_values) != 1) {
          stop("center_effects_optimization_values must be a single center name.")
        }
        # the levels, so a missing center without an explicit NA level is not a center
        if (!center_effects_optimization_values %in% levels(data$center)) {
          missing_name <- is.na(center_effects_optimization_values)
          stop(paste0(
            "center_effects_optimization_values (",
            if (missing_name) "NA_character_" else center_effects_optimization_values,
            ") must be one of the centers in the 'center' column.",
            if (missing_name) {
              paste(
                " To select the rows with a missing center, make them an",
                "explicit level with addNA()."
              )
            }
          ))
        }
        # by level, so an explicit NA level keeps its place among the centers
        center_weights_for_outcome_goal <- as.numeric(
          levels(data$center) %in% center_effects_optimization_values
        )
      }
    } else {
      center_weights_for_outcome_goal <- 1
    }
  }

  # check the values of center_weights_for_outcome_goal. Here rather than in the
  # branch above that reads them from the caller, so that the checks cover every
  # way the vector can be arrived at: the caller's weights, the center-size
  # default and the single-named-center indicator alike. The center-size default
  # is not automatically a set of weights either -- with every
  # center_sample_size zero it is 0/0, i.e. all NaN.
  if (!is.null(center_weights_for_outcome_goal)) {
    # what a set of weights has to be at all, i.e. finite and non-negative,
    # before the sum check below can be made on it. Shared with the exported
    # get_confidence_set(), so the two entry points cannot come to refuse
    # different things: see refuse_invalid_center_weights() for why each check
    # is there.
    refuse_invalid_center_weights(center_weights_for_outcome_goal)
    # and whether the vector is a set of weights at all as a SET, i.e. sums to
    # 1. Shared with the exported get_confidence_set() for the same reason the
    # checks above are: see refuse_non_unit_weight_sum().
    refuse_non_unit_weight_sum(center_weights_for_outcome_goal)
    weights_sum <- sum(center_weights_for_outcome_goal)
    # The tolerance above says the input was MEANT to be a set of weights. It
    # does not make it one: the weights multiply the per-center outcomes and are
    # then summed, so a set summing to 0.999 scales every reported outcome by
    # 0.999 and one summing to 1.001 by 1.001. On the documented example weights
    # that is a bias of 6.7e-4 on the reported outcome, in the direction of the
    # error and on every outcome the run reports, including the goal comparison
    # the recommendation is chosen against.
    #
    # Renormalising here rather than tightening the tolerance: the tolerance is
    # documented and callers rely on it, so tightening it would turn accepted
    # input into a hard error. Renormalising leaves weights that sum to exactly
    # 1 bit for bit unchanged, since dividing by 1 is exact, and removes the
    # bias for everyone else. It is not quite a no-op on the defaults computed
    # below: a vector of center sizes divided by its own total need not sum to
    # exactly 1, and about one such vector in four hundred sums to one unit in
    # the last place away from it, so those runs move by the same amount. That
    # is the correction doing its job on a genuinely non-unit sum, not an error
    # introduced here.
    #
    # Done once, here, where the weights are validated, so nothing downstream
    # needs to know: this is where lago_optimization() obtains them, whether
    # from the caller, from the center sample sizes, or from a single named
    # center. After the fit, align_center_weights_to_fit() keeps only the
    # centers glm() fitted (renormalizing when it drops one), and that vector
    # is what every optimizer, the shrinking method and the confidence set get.
    #
    # Silently, deliberately. The correction is at most 0.1% of a weight and is
    # what the caller already asked for by passing something the tolerance
    # accepts as a set of weights. Warning on it would fire on rounded input
    # that the documentation invites -- c(0.333, 0.333, 0.334) is the natural
    # way to write three equal weights -- so it would be noise on correct
    # usage while the actual behaviour is now right. A caller who did not intend
    # weights is still refused, by the check above.
    center_weights_for_outcome_goal <-
      center_weights_for_outcome_goal / weights_sum
  }

  # check if include_time_effects is logical
  if (!is.logical(include_time_effects) || length(include_time_effects) != 1 ||
    is.na(include_time_effects)) {
    stop(paste("'include_time_effects' must be TRUE or FALSE."))
  }
  family_object <- switch(glm_family,
    "binomial" = binomial(link = link),
    "gaussian" = gaussian(link = link),
    "quasibinomial" = quasibinomial(link = link),
    stop(paste("Unsupported glm_family:", glm_family))
  )


  # if include_time_effects is set to TRUE
  if (include_time_effects) {
    # the input data must have a 'period' column
    if (!"period" %in% names(data)) {
      stop(paste(
        "To include fixed time effects in the outcome model,",
        "please make sure that the input data has a 'period'",
        "column to identify time period."
      ))
    }
    # an explicit NA level, as from addNA(), is a period that NA selects
    has_na_period <- is.factor(data$period) && anyNA(levels(data$period))
    value_is_na <- length(time_effect_optimization_value) == 1 &&
      is.na(time_effect_optimization_value)
    # one period: a number, or the label of a text, factor or Date period
    if (is.null(time_effect_optimization_value) ||
      !(is.numeric(time_effect_optimization_value) ||
        is.character(time_effect_optimization_value) ||
        is.factor(time_effect_optimization_value) ||
        inherits(time_effect_optimization_value, c("Date", "POSIXt")) ||
        (value_is_na && is.logical(time_effect_optimization_value))) ||
      length(time_effect_optimization_value) != 1 ||
      (value_is_na && !has_na_period)) {
      stop(paste(
        "'include_time_effects' is set to TRUE,",
        "'time_effect_optimization_value' must be provided,",
        "and it must be one period: a number, or the label of a period",
        "in the 'period' column.",
        if (value_is_na) {
          paste(
            "NA selects the rows with a missing period only when they are an",
            "explicit level of a factor 'period' column, made with addNA()."
          )
        }
      ))
    }
    # a date-time is labeled in the time zone of the period column, as its
    # levels are, so the same instant in another time zone is the same period
    if (inherits(time_effect_optimization_value, "POSIXt") &&
      inherits(data$period, "POSIXt")) {
      time_effect_optimization_value <- as.POSIXct(time_effect_optimization_value)
      attr(time_effect_optimization_value, "tzone") <- attr(data$period, "tzone")
    }
    # a factor or date-time is matched by its label, as the period levels are,
    # and NA by the explicit NA level
    if (is.factor(time_effect_optimization_value) ||
      inherits(time_effect_optimization_value, "POSIXt") || value_is_na) {
      time_effect_optimization_value <- as.character(time_effect_optimization_value)
    }
    # if "period" column of the input data is not a factor,
    # change it to factor and print a warning message
    if (!is.factor(data$period)) {
      data$period <- as.factor(data$period)
      input_note(paste(
        "'period' column is not a factor type. To ensure",
        "the correct model fit, it has been converted",
        "to the factor type."
      ))
    }
    # an unordered factor, so it is treated as period labels and not as a scale
    if (is.ordered(data$period)) {
      class(data$period) <- "factor"
      input_note(paste(
        "'period' column is an ordered factor. It has been converted to an",
        "unordered factor with the same levels, so the model fits one effect",
        "per period."
      ))
    }
  }

  if (!is.logical(include_interaction_terms) ||
    length(include_interaction_terms) != 1 || is.na(include_interaction_terms)) {
    stop("'include_interaction_terms' must be TRUE or FALSE.")
  }
  # the component columns as they are named in the data, before an interaction
  # term is quoted for the model formula below
  component_columns <- intervention_components
  # if include_interaction_terms is set to TRUE
  if (include_interaction_terms) {
    # check if interaction terms are included in intervention_components
    # and follow the correct naming scheme
    if (!any(grepl(":", intervention_components, fixed = TRUE))) {
      stop(paste(
        "Please make sure the interaction terms are included",
        "as part of the 'intervention_components', and the interaction terms",
        "need to have the format of 'X1:X5' "
      ))
    }

    # if so, check if main_components is null
    if (is.null(main_components)) {
      stop(paste(
        "'main_components' must be defined as a character vector",
        "if interaction terms are",
        "included in 'intervention_components'."
      ))
    }
    # check if main_components is a character vector
    if (!is.character(main_components)) {
      stop(paste("'main_components' must be a character vector."))
    }

    # if not null, check if main_components have the correct naming scheme
    if (any(grepl(":", main_components, fixed = TRUE))) {
      stop(paste(
        "Interaction terms should be included as part of the",
        "'intervention_components', not part of the",
        "'main_components'."
      ))
    }

    refuse_repeated_columns(main_components, "main_components")
    # the bounds are checked against the main components, and each interaction
    # column against their product, so they must be numeric columns
    missing_main <- main_components[!main_components %in% names(data)]
    if (length(missing_main) > 0) {
      stop(paste0(
        "With interaction terms, all elements in the main_components must be ",
        "columns in the data frame, but ",
        paste0("'", missing_main, "'", collapse = ", "), " are not."
      ))
    }
    non_numeric_main <- main_components[!vapply(
      main_components, function(component) is.numeric(data[[component]]),
      logical(1)
    )]
    if (length(non_numeric_main) > 0) {
      stop(paste0(
        "The main component(s) ",
        paste0("'", non_numeric_main, "'", collapse = ", "),
        " must be numeric columns."
      ))
    }

    # check if interaction terms can be found in the main_components
    check_term <- function(term) {
      if (grepl(":", term, fixed = TRUE)) {
        parts <- unlist(strsplit(term, ":", fixed = TRUE))
        return(all(parts %in% main_components))
      } else {
        return(term %in% main_components)
      }
    }
    interaction_check_results <- sapply(intervention_components, check_term)
    if (!all(interaction_check_results)) {
      stop(paste(
        "Each component of 'intervention_components' including",
        "interaction terms (separated by ':')",
        "must be present in 'main_components'."
      ))
    }

    # add backticks to interactions terms in intervention components
    # for glm model fitting
    needs_backticks <- grepl(":", intervention_components)
    intervention_components[needs_backticks] <- paste0(
      "`", intervention_components[needs_backticks], "`"
    )
  } else {
    # if interaction terms are not included, then the intervention
    # components shouldn't have ":" in their names
    if (any(grepl(":", intervention_components, fixed = TRUE))) {
      stop(paste(
        "':' is found in the intervention_components,",
        "did you mean to set include_interaction_terms to TRUE?"
      ))
    }
  }

  # check if additional_covariates is a character vector
  if (length(additional_covariates) > 0) {
    if (!(is.vector(additional_covariates) &&
      is.character(additional_covariates))) {
      stop("additional_covariates must be a character vector.")
    }
    # check if additional_covariates are all columns in the data frame
    if (!all(additional_covariates %in% names(data))) {
      stop(paste0(
        "All elements in additional_covariates ",
        "must be columns in the data frame."
      ))
    }
    refuse_repeated_columns(additional_covariates, "additional_covariates")
    # refuse any additional covariate whose column is entirely NA. glm()'s
    # internal na.omit drops every row in that case, and the fit dies with an
    # opaque error that never names the column. This runs after the
    # column-existence check above so data[[covariate]] is always a real
    # column. all(is.na(col)) treats numeric, factor and character columns
    # alike, so an all-NA covariate of any type is caught here rather than
    # left to fail cryptically inside model fitting.
    all_na_covariates <- additional_covariates[vapply(
      additional_covariates,
      function(covariate) all(is.na(data[[covariate]])),
      logical(1)
    )]
    if (length(all_na_covariates) > 0) {
      stop(paste0(
        "The additional covariate(s) ",
        paste(all_na_covariates, collapse = ", "),
        " are entirely NA, so glm() would drop every row and they ",
        "cannot enter the model. Remove them from additional_covariates ",
        "or supply observed values."
      ))
    }
  }

  # the model formula is built from these names, so each must be a valid R
  # name. An interaction term such as a:b is quoted where it is used.
  model_names <- unique(c(
    outcome_name, intervention_components, main_components,
    additional_covariates, center_characteristics
  ))
  model_names <- model_names[!grepl(":", model_names, fixed = TRUE)]
  # ..1 and ... pass make.names() but are reserved, so a formula cannot use them
  reserved_names <- model_names[grepl("^[.][.]([.]|[0-9]+)$", model_names)]
  if (length(reserved_names) > 0) {
    stop(paste0(
      "The column name(s) ", paste0("'", reserved_names, "'", collapse = ", "),
      " are reserved in R, so they cannot be used in the outcome model ",
      "formula. Rename them, for example to ",
      paste0("'X", reserved_names, "'", collapse = ", "), "."
    ))
  }
  bad_names <- model_names[make.names(model_names) != model_names]
  if (length(bad_names) > 0) {
    stop(paste0(
      "The column name(s) ", paste0("'", bad_names, "'", collapse = ", "),
      " are not valid R names, so they cannot be used in the outcome model ",
      "formula. Rename them, for example with make.names(), which gives ",
      paste0("'", make.names(bad_names), "'", collapse = ", "), "."
    ))
  }
  # the confidence set has a column for each component and center
  # characteristic, next to its own columns with these names
  confidence_set_columns <- c("cost", "CI_lower_bound", "CI_upper_bound")
  clashing_names <- intersect(
    c(intervention_components, main_components, center_characteristics),
    confidence_set_columns
  )
  if (length(clashing_names) > 0) {
    stop(paste0(
      "The column name(s) ", paste0("'", clashing_names, "'", collapse = ", "),
      " cannot be used for an intervention component or a center ",
      "characteristic, because the confidence set has its own columns named ",
      paste0("'", confidence_set_columns, "'", collapse = ", "),
      ". Rename the column(s) in the data, for example to ",
      paste0("'", clashing_names, "_value'", collapse = ", "), "."
    ))
  }

  # A column can have one role in the model. A characteristic that is also a
  # component, for example, would be fitted once and then set twice.
  roles <- list(
    "the outcome" = outcome_name,
    "an intervention component" = unique(c(component_columns, main_components)),
    "a center characteristic" = center_characteristics,
    "an additional covariate" = additional_covariates
  )
  role_columns <- unlist(lapply(roles, unique), use.names = FALSE)
  shared_columns <- unique(role_columns[duplicated(role_columns)])
  if (length(shared_columns) > 0) {
    stop(paste0(
      "Each column can have only one role in the outcome model, but ",
      paste(vapply(shared_columns, function(column) {
        in_roles <- names(roles)[vapply(roles, function(r) column %in% r, logical(1))]
        paste0("'", column, "' is ", paste(in_roles, collapse = " and "))
      }, character(1)), collapse = ", and "),
      ". Remove it from all but one of them."
    ))
  }

  # the columns the outcome model is fitted on, and the glm weights of its rows
  fit_weights <- if (input_data_structure == "center_level") {
    data$center_sample_size
  } else {
    weights
  }
  base_columns <- unique(c(
    outcome_name, component_columns, center_characteristics,
    additional_covariates,
    if (input_data_structure == "center_level") "center_sample_size"
  ))
  effect_columns <- c(
    if (include_center_effects) "center",
    if (include_time_effects) "period"
  )
  # a second column with the same name would be read in place of the first
  repeated_data_columns <- intersect(
    c(base_columns, main_components, effect_columns),
    names(data)[duplicated(names(data))]
  )
  if (length(repeated_data_columns) > 0) {
    stop(paste0(
      "The data has more than one column named ",
      paste0("'", repeated_data_columns, "'", collapse = ", "),
      ". Rename or remove the extra columns, so the model uses the one you mean."
    ))
  }
  observed_weight <- if (is.null(fit_weights)) TRUE else !is.na(fit_weights)
  base_rows <- stats::complete.cases(data[base_columns]) & observed_weight
  # with center or time effects the fit refuses this, and says whether turning
  # an effect off would help
  if (!any(base_rows) && length(effect_columns) == 0) {
    with_missing <- c(
      sprintf("'%s'", base_columns[vapply(
        base_columns, function(column) anyNA(data[[column]]), logical(1)
      )]),
      if (input_data_structure != "center_level" && anyNA(weights)) "'weights'"
    )
    stop(paste0(
      "No row has every model variable and its glm weight observed, so the ",
      "outcome model cannot be fitted. The values with missing entries are ",
      paste(with_missing, collapse = ", "), ". Fill in the missing values, or ",
      "remove a column from the model."
    ))
  }
  # the rows glm() keeps, and among them the rows it fits, with a positive weight
  complete_rows <- base_rows
  if (length(effect_columns) > 0) {
    complete_rows <- complete_rows & stats::complete.cases(data[effect_columns])
  }
  if (!any(complete_rows)) {
    # the fit refuses this with a message about the center or period effects
    complete_rows <- base_rows
  }
  fitted_rows <- complete_rows
  if (!is.null(fit_weights)) fitted_rows <- fitted_rows & fit_weights > 0
  if (!any(fitted_rows)) fitted_rows <- complete_rows

  # glm() cannot use an infinite value on a row it keeps, even one with weight
  # 0, and the observed mean would be infinite. A row it leaves out does not
  # matter.
  infinite_columns <- Filter(function(column) {
    is.numeric(data[[column]]) && any(is.infinite(data[[column]][complete_rows]))
  }, unique(c(base_columns, main_components)))
  if (length(infinite_columns) > 0) {
    stop(paste0(
      "The column(s) ", paste0("'", infinite_columns, "'", collapse = ", "),
      " hold Inf or -Inf on rows the outcome model uses, which it cannot do. ",
      "Replace those values with finite numbers, or with NA to leave their ",
      "rows out."
    ))
  }
  # the outcome values the model fits, on the rows with a positive weight
  fitted_outcome <- outcome_column[fitted_rows & !is.na(outcome_column)]

  # a text, factor or logical column with one value has no effect to estimate
  single_level <- list(
    "additional covariate" = additional_covariates,
    "center characteristic" = center_characteristics
  )
  # only judged when some row can be used, as the fit refuses the rest
  for (role in if (any(complete_rows)) names(single_level)) {
    for (column in single_level[[role]]) {
      x <- data[[column]]
      if (!(is.factor(x) || is.character(x) || is.logical(x))) next
      values <- unique(as.character(x[complete_rows]))
      if (length(values) < 2) {
        stop(paste0(
          "The ", role, " '", column, "' has only the value '", values,
          "' on the rows the outcome model can use, so the model cannot ",
          "estimate its effect. Remove it from ",
          if (role == "additional covariate") "additional_covariates" else "center_characteristics",
          ", or add data with another value."
        ))
      }
    }
  }

  # The model reads an interaction column as the product of its components,
  # and the recommendation computes that product, so the two must agree.
  for (term in component_columns[grepl(":", component_columns, fixed = TRUE)]) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    # in double precision, as a product of integer columns can overflow. The
    # parts are main components, which are numeric.
    product <- Reduce(`*`, lapply(parts, function(part) as.numeric(data[[part]])))
    rows <- which(fitted_rows & !is.na(product))
    difference <- abs(data[[term]][rows] - product[rows])
    # a column saved with 4 significant digits, or with 3 decimals when the
    # products are about 1 or more, still agrees. Each row is judged against
    # the larger of its own product and the mean product, so a small product
    # does not need more decimals than the column was saved with.
    tolerance <- 1e-3 * pmax(abs(product[rows]), mean(abs(product[rows])))
    mismatch <- rows[difference > tolerance]
    if (length(mismatch) > 0) {
      row <- mismatch[1]
      stop(paste0(
        "The interaction column '", term, "' must hold the product ",
        paste(parts, collapse = " * "), ", but in row ", row, " it is ",
        format(data[[term]][row], digits = 10), " while the product is ",
        format(product[row], digits = 10), ". The model reads the column as ",
        "that product, so the recommendation would be wrong. Set it with ",
        "data[[\"", term, "\"]] <- ", paste0("data$", parts, collapse = " * "),
        "."
      ))
    }
  }

  intervention_lower_bounds <- drop_1d_array(intervention_lower_bounds)
  intervention_upper_bounds <- drop_1d_array(intervention_upper_bounds)
  # the components each per component vector is given for
  per_component <- if (include_interaction_terms) main_components else component_columns
  per_component_what <- if (include_interaction_terms) {
    "main components"
  } else {
    "intervention components"
  }

  # check if intervention_lower_bounds and intervention_upper_bounds are both
  # numerical vectors
  if (!(is.vector(intervention_lower_bounds) &&
    is.numeric(intervention_lower_bounds))) {
    stop("intervention_lower_bounds must be a numeric vector.")
  }
  if (!(is.vector(intervention_upper_bounds) &&
    is.numeric(intervention_upper_bounds))) {
    stop("intervention_upper_bounds must be a numeric vector.")
  }

  # check if the dimension of intervention_lower_bounds matches
  # the dimension of intervention_components or the dimension
  # of main_components
  if (
    (!include_interaction_terms &&
      (length(intervention_lower_bounds) != length(intervention_components))) ||
      (include_interaction_terms &&
        (length(intervention_lower_bounds) != length(main_components)))
  ) {
    stop(paste(
      "Without interaction terms,",
      "the lengths of 'intervention_lower_bounds' and",
      "'intervention_components' must be the same.",
      "With interaction terms,",
      "the lengths of 'intervention_lower_bounds' and",
      "'main_components' must be the same."
    ))
  }

  # check if lower bounds list and upper bounds list have the same length,
  # and if the lower bounds are <= upper bounds respectively
  if (length(intervention_lower_bounds) != length(intervention_upper_bounds)) {
    stop("The lengths of lower and upper bounds must be the same.")
  }
  if (!all(is.finite(intervention_lower_bounds)) ||
    !all(is.finite(intervention_upper_bounds))) {
    stop(paste(
      "intervention_lower_bounds and intervention_upper_bounds must be finite",
      "numbers, one per component. NA, NaN, Inf and -Inf are not bounds."
    ))
  }
  intervention_lower_bounds <- match_by_names(
    intervention_lower_bounds, per_component, "intervention_lower_bounds",
    per_component_what
  )
  intervention_upper_bounds <- match_by_names(
    intervention_upper_bounds, per_component, "intervention_upper_bounds",
    per_component_what
  )
  if (any(intervention_lower_bounds < 0)) {
    stop("The intervention must have non-negative values only.")
  }
  invalid_indices <- which(intervention_upper_bounds <
    intervention_lower_bounds)
  if (length(invalid_indices) > 0) {
    stop(paste(
      "Invalid bounds at position(s):",
      paste(invalid_indices, collapse = ", "),
      paste0(
        "\nUpper bounds for the interventions must be greater ",
        "than or equal to the lower bounds."
      )
    ))
  }

  # check if lower bounds and upper bounds make sense by checking the min and
  # max values of the components on the rows the model is fitted on
  all_components <- per_component
  for (i in seq_along(all_components)) {
    fitted_values <- data[[all_components[i]]][fitted_rows]
    fitted_values <- fitted_values[!is.na(fitted_values)]
    if (length(fitted_values) == 0) next
    min_val <- min(fitted_values)
    max_val <- max(fitted_values)
    if (intervention_lower_bounds[i] > min_val) {
      input_warning(paste(
        "The lower bound for the intervention component",
        all_components[i],
        "is greater than the minimum value in the data."
      ))
    }
    if (intervention_upper_bounds[i] < max_val) {
      input_warning(paste(
        "The upper bound for the intervention component",
        all_components[i],
        "is less than the maximum value in the data."
      ))
    }
  }

  # check if unit_costs is valid
  if (!is.null(unit_costs)) {
    # check if unit_costs is a numeric vector
    if (!is.numeric(unit_costs)) {
      stop("unit_costs must be a numeric vector.")
    }
    if (any(!is.finite(unit_costs)) || any(unit_costs < 0)) {
      stop("unit_costs must be finite and not negative.")
    }
    # check if the dimension of unit_costs matches
    # the dimension of intervention_components or the dimension
    # of main_components
    if (
      (!include_interaction_terms &&
        (length(unit_costs) != length(intervention_components))) ||
        (include_interaction_terms &&
          (length(unit_costs) != length(main_components)))
    ) {
      stop(paste(
        "Without interaction terms,",
        "the lengths of 'unit_costs' and",
        "'intervention_components' must be the same.",
        "With interaction terms,",
        "the lengths of 'unit_costs' and",
        "'main_components' must be the same."
      ))
    }
    unit_costs <- match_by_names(
      drop_1d_array(unit_costs), per_component, "unit_costs", per_component_what
    )
    # check if default_cost_fxn_type is valid
    if (!is.character(default_cost_fxn_type)) {
      stop("default_cost_fxn_type must be a character string.")
    }
    refuse_not_one_string(default_cost_fxn_type, "default_cost_fxn_type")
    # check if default_cost_fxn_type is one of the supported types
    supported_cost_fxn_types <- c("linear", "cubic")
    if (!(default_cost_fxn_type %in% supported_cost_fxn_types)) {
      stop(sprintf(
        "default_cost_fxn_type must be one of the supported types. The supported types are: %s.",
        paste(supported_cost_fxn_types, collapse = ", ")
      ))
    }
    # the dose each unit cost was measured at pins the cubic cost there. With a
    # cost list the doses are ignored, as said below.
    if (!is.null(unit_cost_doses) && is.null(cost_list_of_vectors) &&
      default_cost_fxn_type == "linear") {
      input_warning(paste(
        "'unit_cost_doses' is ignored, because the linear cost is the unit",
        "cost at every dose."
      ))
      unit_cost_doses <- NULL
    } else if (!is.null(unit_cost_doses) && is.null(cost_list_of_vectors)) {
      unit_cost_doses <- drop_1d_array(unit_cost_doses)
      if (!is.numeric(unit_cost_doses) || length(unit_cost_doses) != length(unit_costs) ||
        any(!is.finite(unit_cost_doses))) {
        stop(paste(
          "unit_cost_doses must be a finite numeric vector with one dose for",
          "each unit cost."
        ))
      }
      unit_cost_doses <- match_by_names(
        unit_cost_doses, per_component, "unit_cost_doses", per_component_what
      )
      if (any(unit_cost_doses < intervention_lower_bounds) ||
        any(unit_cost_doses > intervention_upper_bounds)) {
        stop(paste(
          "Each value of unit_cost_doses must lie within the lower and upper",
          "bounds of its intervention component."
        ))
      }
    } else if (is.null(unit_cost_doses) && is.null(cost_list_of_vectors) &&
      default_cost_fxn_type == "cubic") {
      input_warning(paste(
        "'unit_cost_doses' is not given, so the cubic cost only matches each",
        "unit cost on average over the bounds, not at the dose it was measured",
        "at. Give the dose each unit cost was measured at to pin the cost there."
      ))
    }
  }

  # check if cost_list_of_vectors is valid
  if (!is.null(cost_list_of_vectors)) {
    # check if cost_list_of_vectors is a list,
    # and each sublist is a numeric vector
    if (!is.list(cost_list_of_vectors)) {
      stop("cost_list_of_vectors must be a list.")
    }
    # Check if every vector in cost_list_of_vectors is numeric
    all_numeric <- all(vapply(cost_list_of_vectors, is.numeric, logical(1)))
    if (!all_numeric) {
      stop(paste(
        "All elements in the sublists of cost_list_of_vectors",
        "must be numeric."
      ))
    }
    # a missing or infinite coefficient gives no cost, and an empty vector no
    # cost function at all
    usable_costs <- vapply(cost_list_of_vectors, function(coefficients) {
      length(coefficients) > 0 && all(is.finite(coefficients))
    }, logical(1))
    if (!all(usable_costs)) {
      stop(paste0(
        "Each vector in cost_list_of_vectors must hold at least one ",
        "coefficient, and each coefficient must be a finite number, but the ",
        "vector(s) at position(s) ", paste(which(!usable_costs), collapse = ", "),
        " do not. NA, NaN, Inf and -Inf are not coefficients."
      ))
    }
    # check if the dimension of cost_list_of_vectors matches
    # the dimension of intervention_components or the dimension
    # of main_components
    if (
      (!include_interaction_terms &&
        (length(cost_list_of_vectors) != length(intervention_components))) ||
        (include_interaction_terms &&
          (length(cost_list_of_vectors) != length(main_components)))
    ) {
      stop(paste(
        "Without interaction terms,",
        "the lengths of 'cost_list_of_vectors' and",
        "'intervention_components' must be the same.",
        "With interaction terms,",
        "the lengths of 'cost_list_of_vectors' and",
        "'main_components' must be the same."
      ))
    }
    cost_list_of_vectors <- match_by_names(
      cost_list_of_vectors, per_component, "cost_list_of_vectors",
      per_component_what
    )
    if (!is.null(unit_cost_doses)) {
      input_warning(paste(
        "'unit_cost_doses' is ignored, because the cost functions are given",
        "in 'cost_list_of_vectors'."
      ))
      unit_cost_doses <- NULL
    }
    # check if default_cost_fxn_type is also provided
    if (isTRUE(cost_type_given) && !is.null(default_cost_fxn_type) && !quiet) {
      input_note(paste(
        "When 'cost_list_of_vectors' is provided,",
        "'default_cost_fxn_type' is ignored."
      ))
    }
  }

  # check if unit_costs and cost_list_of_vectors are both null
  if (is.null(unit_costs) && is.null(cost_list_of_vectors)) {
    stop(paste(
      "Both 'unit_costs' and 'cost_list_of_vectors' are NULL.",
      "Please provide at least one of them."
    ))
  }

  # if optimization_grid_search_step_size is provided, validate it before it
  # can influence optimization_method below, so that an invalid step size is
  # rejected up front rather than after a "switched to grid_search" message.
  if (!is.null(optimization_grid_search_step_size)) {
    optimization_grid_search_step_size <-
      drop_1d_array(optimization_grid_search_step_size)
    # it needs to be a numeric vector
    if (!(is.vector(optimization_grid_search_step_size) &&
      is.numeric(optimization_grid_search_step_size))) {
      stop("optimization_grid_search_step_size must be a numeric vector.")
    }
    # it needs to have the same length as the number of intervention components
    if ((!include_interaction_terms &&
      (length(optimization_grid_search_step_size) !=
        length(intervention_components))) ||
      (include_interaction_terms &&
        (length(optimization_grid_search_step_size) !=
          length(main_components)))
    ) {
      stop(paste(
        "Without interaction terms,",
        "The number of step sizes provided for the grid search",
        "algorithm must be the same as the number of intervention",
        "components. Please check the length of",
        "'optimization_grid_search_step_size'.",
        "With interaction terms,",
        "The number of step sizes provided for the grid search",
        "algorithm must be the same as the length of 'main_components'."
      ))
    }
    # each step size must be finite and strictly positive, otherwise the grid
    # search seq() call would error or produce a degenerate grid.
    if (!all(is.finite(optimization_grid_search_step_size)) ||
      any(optimization_grid_search_step_size <= 0)) {
      stop(paste(
        "All elements of 'optimization_grid_search_step_size' must be",
        "finite and greater than 0."
      ))
    }
    optimization_grid_search_step_size <- match_by_names(
      optimization_grid_search_step_size, per_component,
      "optimization_grid_search_step_size", per_component_what
    )
  }

  # optimization_grid_search_step_size is only used by the grid search
  # method. If it is provided while the method is "numerical", the user
  # clearly intends to use grid search, so switch the method (with a
  # message) instead of silently ignoring the step size.
  if (!is.null(optimization_grid_search_step_size) &&
    optimization_method == "numerical") {
    optimization_method <- "grid_search"
    input_note(paste(
      "'optimization_grid_search_step_size' was provided, so",
      "'optimization_method' has been switched to 'grid_search'.",
      "To use the numerical method instead, remove",
      "'optimization_grid_search_step_size'."
    ))
  }

  # when the optimization_method is grid_search,
  # check the number of intervention components
  if (optimization_method == "grid_search") {
    if (length(per_component) > 3) {
      input_note(paste0(
        "There are more than 3 intervention components, and the grid search ",
        "algorithm may take significant amount of time to run. ",
        "Please consider adjusting the step size, or switch to the ",
        "numerical optimization method."
      ))
    }
  }

  # at least one of outcome_goal or power_goal must be provided. LAGO
  # optimization supports three modes: outcome goal alone, power goal alone,
  # or both together (in which case the effective goal is the higher of the
  # two, see get_recommended_interventions()).
  if (is.null(outcome_goal) && is.null(power_goal)) {
    stop(paste(
      "Both 'outcome_goal' and 'power_goal' are NULL.",
      "Please provide at least one of them."
    ))
  }

  # check if the outcome goal is a numeric number (only when provided)
  if (!is.null(outcome_goal) && (!is.numeric(outcome_goal) ||
    length(outcome_goal) != 1 || !is.finite(outcome_goal))) {
    stop("The outcome goal must be a numeric value, a single finite number.")
  }

  # check if the outcome_goal_intention is a character type
  if (!is.character(outcome_goal_intention)) {
    stop("The outcome_goal_intention must be a character string.")
  }
  refuse_not_one_string(outcome_goal_intention, "outcome_goal_intention")
  # check if the outcome_goal_intention is one of the defined intentions
  allowed_outcome_goal_intentions <- c("minimize", "maximize")
  if (!(outcome_goal_intention %in% allowed_outcome_goal_intentions)) {
    stop(paste(
      "The outcome_goal_intention must be one",
      "of the supported intentions.",
      "The supported intentions are:",
      allowed_outcome_goal_intentions, "."
    ))
  }

  # set the lower_outcome_goal flag accordingly
  # check the average outcome and print warning messages.
  # when only a power goal is provided (outcome_goal is NULL), the goal is
  # always to increase the outcome to reach the desired power, so
  # lower_outcome_goal is FALSE and the intention checks are skipped.
  # the observed mean the goal is compared with, weighted as glm() weights the
  # rows, so center level data is averaged over patients rather than rows
  # over the rows the model fits, with a positive weight
  observed_mean <- local({
    y <- outcome_column
    w <- if (is.null(fit_weights)) rep(1, length(y)) else fit_weights
    ok <- fitted_rows & !is.na(y) & !is.na(w)
    m <- if (any(ok) && sum(w[ok]) > 0) stats::weighted.mean(y[ok], w[ok]) else NaN
    if (is.finite(m)) m else mean(y, na.rm = TRUE)
  })
  # a goal equal to the mean up to round off counts as equal, as the mean is
  # summed in an order that differs from platform to platform. The tolerance
  # is relative, so it suits an outcome in any unit.
  at_mean_tol <- if (is.null(outcome_goal)) 0 else 1e-9 * abs(observed_mean)
  if (is.null(outcome_goal)) {
    lower_outcome_goal <- FALSE
  } else if (outcome_goal_intention == "minimize") {
    lower_outcome_goal <- TRUE
    # a goal at the observed mean asks for no change, in either direction
    if (outcome_goal >= observed_mean - at_mean_tol) {
      input_warning(paste(
        "The specified outcome goal intention is",
        "to minimize the outcome.",
        "The observed mean of the outcome is",
        observed_mean,
        "and the specified outcome goal is", outcome_goal, "."
      ), call = sys.call())
    }
  } else {
    lower_outcome_goal <- FALSE
    if (outcome_goal <= observed_mean + at_mean_tol) {
      input_warning(paste(
        "The specified outcome goal intention",
        "is to maximize the outcome.",
        "The observed mean of the outcome is",
        observed_mean,
        "and the specified outcome goal is", outcome_goal, "."
      ), call = sys.call())
    }
  }

  # check whether the include_confidence_set indicator is boolean
  if (!is.logical(include_confidence_set) || length(include_confidence_set) != 1 ||
    is.na(include_confidence_set)) {
    stop("The include_confidence_set indicator must be a boolean.")
  }

  # check if confidence set step size is provided
  if (include_confidence_set && !is.null(confidence_set_grid_step_size)) {
    confidence_set_grid_step_size <- drop_1d_array(confidence_set_grid_step_size)
    # if confidence_set_grid_step_size is provided,
    # it needs to be a numeric vector
    if (!(is.vector(confidence_set_grid_step_size) &&
      is.numeric(confidence_set_grid_step_size))) {
      stop("confidence_set_grid_step_size must be a numeric vector.")
    }
    # if confidence_set_grid_step_size is provided,
    # it needs to have the same length as the number of intervention components
    if ((!include_interaction_terms &&
      (length(confidence_set_grid_step_size) !=
        length(intervention_components))) ||
      (include_interaction_terms &&
        (length(confidence_set_grid_step_size) !=
          length(main_components)))
    ) {
      stop(paste(
        "Without interaction terms,",
        "the number of step sizes provided for the grid",
        "search algorithm in the confidence set calculation",
        "must be the same as the length of 'intervention_components'.",
        "Please check the length of 'confidence_set_grid_step_size'.",
        "With interaction terms,",
        "the number of step sizes provided for the grid",
        "search algorithm in the confidence set calculation",
        "must be the same as the length of 'main_components'."
      ))
    }
    if (any(!is.finite(confidence_set_grid_step_size)) ||
      any(confidence_set_grid_step_size <= 0)) {
      stop("confidence_set_grid_step_size must be finite and positive.")
    }
    confidence_set_grid_step_size <- match_by_names(
      confidence_set_grid_step_size, per_component,
      "confidence_set_grid_step_size", per_component_what
    )
  }

  # check if confidence_set_alpha is a numeric type
  if (!is.numeric(confidence_set_alpha) || length(confidence_set_alpha) != 1 ||
    is.na(confidence_set_alpha)) {
    stop("confidence_set_alpha must be a numeric value, a single number.")
  }
  # check if confidence_set_alpha is between 0 and 1
  if (confidence_set_alpha <= 0 || confidence_set_alpha >= 1) {
    stop("confidence_set_alpha must be between 0 and 1.")
  }

  # check if the specified family matches the outcome type
  valid_families <- list(
    "binary" = c("binomial"),
    "continuous" = c("gaussian", "quasibinomial")
  )
  if (!glm_family %in% valid_families[[outcome_type]]) {
    stop(paste("The specified family '", glm_family,
      "' is not valid for the outcome type '", outcome_type, "'.",
      " Please select a compatible family.",
      sep = ""
    ))
  }
  # a quasibinomial outcome is a proportion, and a logit link on a continuous
  # outcome needs values strictly between 0 and 1, where the logit is finite
  # only the rows the model fits matter, as for a binary outcome
  outside_range <- if (glm_family == "quasibinomial") {
    fitted_outcome[fitted_outcome < 0 | fitted_outcome > 1]
  } else if (glm_family == "gaussian" && link == "logit") {
    fitted_outcome[fitted_outcome <= 0 | fitted_outcome >= 1]
  } else {
    numeric(0)
  }
  if (length(outside_range) > 0) {
    stop(paste0(
      "The outcome column '", outcome_name, "' has values ",
      if (glm_family == "quasibinomial") {
        "outside 0 to 1"
      } else {
        "that are not strictly between 0 and 1"
      },
      ", such as ", format(outside_range[1], digits = 10), ". ",
      if (glm_family == "quasibinomial") {
        "The quasibinomial family models a proportion, which is from 0 to 1."
      } else {
        paste(
          "A logit link on a continuous outcome needs values above 0 and",
          "below 1. Use the identity link, or rescale the outcome."
        )
      }
    ))
  }

  # check if the prev_recommended_interventions is numeric and
  # has the same length as the number of intervention components
  if (!is.null(prev_recommended_interventions)) {
    prev_recommended_interventions <- drop_1d_array(prev_recommended_interventions)
    if (!is.numeric(prev_recommended_interventions)) {
      stop("prev_recommended_interventions must be a numeric vector.")
    }
    all_components <- per_component
    if (length(prev_recommended_interventions) != length(all_components)) {
      stop(paste0(
        "The length of prev_recommended_interventions must be the same as ",
        "the number of ", per_component_what, ", ", length(all_components), "."
      ))
    }

    if (!all(is.finite(prev_recommended_interventions))) {
      stop(paste(
        "prev_recommended_interventions must be finite numbers, one per",
        "intervention component, with no NA."
      ))
    }
    prev_recommended_interventions <- match_by_names(
      prev_recommended_interventions, per_component,
      "prev_recommended_interventions", per_component_what
    )
    # prev_recommended_interventions must be within the bounds
    for (i in 1:length(all_components)) {
      if (prev_recommended_interventions[i] <
        intervention_lower_bounds[i] ||
        prev_recommended_interventions[i] >
          intervention_upper_bounds[i]) {
        stop(paste(
          "The prev_recommended_interventions[", i, "]",
          "is outside the bounds of the intervention component",
          all_components[i], "."
        ))
      }
    }
  }

  # check if shrinkage_threshold is a numeric value
  # and is between 0 and 1
  if (!is.numeric(shrinkage_threshold) || length(shrinkage_threshold) != 1 ||
    is.na(shrinkage_threshold)) {
    stop("shrinkage_threshold must be a numeric value, a single number.")
  }
  if (shrinkage_threshold < 0 || shrinkage_threshold > 1) {
    stop("shrinkage_threshold must be between 0 and 1.")
  }

  # check if power_goal is a numeric number if not null
  if (!is.null(power_goal)) {
    if (!is.numeric(power_goal) || length(power_goal) != 1 || is.na(power_goal)) {
      stop("The power goal must be a numeric value, a single number.")
    }
    # check if power_goal is between 0 and 1
    if (power_goal <= 0 || power_goal >= 1) {
      stop("The power goal must be between 0 and 1.")
    }
    # check if the outcome_type is binary
    if (outcome_type != "binary") {
      stop("For now, the power goal only works with binary outcomes.")
    }
    # a power goal requires increasing the outcome to reach the desired
    # power, so it is incompatible with minimizing the outcome. This holds
    # whether or not an outcome goal is also provided.
    if (outcome_goal_intention == "minimize") {
      stop(paste(
        "A power goal cannot be combined with",
        "outcome_goal_intention = 'minimize', because achieving a power",
        "goal requires increasing the outcome. Please set",
        "outcome_goal_intention = 'maximize', or remove the power goal."
      ))
    }
    # check if there is a 'group' column in the data
    # and it is either "treatment" or "control"
    if (!"group" %in% names(data)) {
      stop(paste(
        "To include a power goal, please make sure that",
        "there is a 'group' column in the input data."
      ))
    }
    if (!all(data$group %in% c("treatment", "control"))) {
      stop(paste0(
        "The 'group' column must have values 'treatment' or 'control'",
        if (anyNA(data$group)) {
          paste0(
            ", but ", sum(is.na(data$group)), " row(s) have a missing value. ",
            "Fill in their arm, or drop those rows"
          )
        },
        "."
      ))
    }
    # the power calculation uses stage-1 rows with an observed outcome, so
    # each arm needs at least one (a missing outcome is not counted)
    # for center level data a row counts only with patients in it
    has_patients <- if (input_data_structure == "center_level") {
      !is.na(data$center_sample_size) & data$center_sample_size > 0
    } else {
      TRUE
    }
    observed_group <- data$group[!is.na(data[[outcome_name]]) & has_patients]
    for (grp in c("control", "treatment")) {
      if (!any(observed_group == grp)) {
        stop(paste0(
          "A power goal needs stage-1 data in both arms, but the '", grp,
          "' arm of the 'group' column has no rows with an observed outcome."
        ))
      }
    }
    # num_centers_in_next_stage and patients_per_center_in_next_stage
    # cannot be null for power goal to work
    if (is.null(num_centers_in_next_stage) ||
      is.null(patients_per_center_in_next_stage)) {
      stop(paste(
        "num_centers_in_next_stage and",
        "patients_per_center_in_next_stage must be provided",
        "for the power goal to work."
      ))
    }
    # if num_center_in_next_stage is not null, it must be a numeric value
    # each is one count: a whole, finite number above 0
    next_stage <- list(
      num_centers_in_next_stage = num_centers_in_next_stage,
      patients_per_center_in_next_stage = patients_per_center_in_next_stage
    )
    for (size_name in names(next_stage)) {
      size <- next_stage[[size_name]]
      if (!is.numeric(size)) {
        stop(paste0(size_name, " must be a numeric value."))
      }
      if (length(size) != 1 || !is.finite(size) || size != round(size)) {
        stop(paste0(size_name, " must be one whole, finite number."))
      }
      if (size <= 0) {
        stop(paste0(size_name, " must be greater than 0."))
      }
    }
    # the next stage is split evenly between the two arms
    if (num_centers_in_next_stage < 2) {
      stop(paste(
        "num_centers_in_next_stage must be at least 2, because the next stage",
        "has a control arm and a treatment arm, each with at least one center."
      ))
    }
    if (num_centers_in_next_stage %% 2 == 1) {
      input_warning(paste0(
        "num_centers_in_next_stage is ", num_centers_in_next_stage, ", an odd ",
        "number. The power calculation splits the next stage evenly between ",
        "the two arms, so it plans ", num_centers_in_next_stage / 2,
        " centers per arm. Give an even number for whole centers in each arm."
      ))
    }
    # a cluster id that is given must name a column, whatever the icc
    if (!is.null(power_goal_cluster_id)) {
      if (!is.character(power_goal_cluster_id) ||
        length(power_goal_cluster_id) != 1 || is.na(power_goal_cluster_id)) {
        stop("power_goal_cluster_id must be a single column name.")
      }
      if (!power_goal_cluster_id %in% names(data)) {
        stop(paste0(
          "power_goal_cluster_id column '", power_goal_cluster_id,
          "' was not found in the data."
        ))
      }
    }
    # icc (issue #29): optional intra-cluster correlation used to inflate the
    # power-calculation variance by a design effect. Scalar (shared across arms)
    # or length-2 c(control, treatment). Each value must be in [0, 1).
    if (!is.null(icc)) {
      if (!is.numeric(icc)) {
        stop("icc must be a numeric value (or a length-2 numeric vector).")
      }
      if (length(icc) != 1 && length(icc) != 2) {
        stop("icc must have length 1 (shared) or 2 (control, treatment).")
      }
      if (!all(is.finite(icc)) || any(icc < 0) || any(icc >= 1)) {
        stop("icc must be in [0, 1).")
      }
      # one value is shared by both arms, so a name such as "treatment" would
      # wrongly suggest it is for that arm only
      if (length(icc) == 1 && !is.null(names(icc)) &&
        (is.na(names(icc)) || nzchar(names(icc)))) {
        stop(paste0(
          "icc has one value, which is shared by both arms, so it cannot be ",
          "named, but it is named '", names(icc), "'. Give ",
          "c(control = , treatment = ) for a value per arm, or remove the name ",
          "with unname()."
        ))
      }
      # a pair is read as c(control, treatment), and named values by name
      if (length(icc) == 2) {
        icc <- match_by_names(icc, c("control", "treatment"), "icc", "arms")
      }
      # a non-zero icc needs a stage-1 cluster id to compute the stage-1 design
      # effect; validate the column exists and is non-degenerate per arm.
      if (any(icc > 0)) {
        if (is.null(power_goal_cluster_id)) {
          stop(paste(
            "A non-zero 'icc' requires 'power_goal_cluster_id', the name of a",
            "column identifying stage-1 centers, so the stage-1 design effect",
            "can be computed."
          ))
        }
        # need at least two centers per arm to identify between-cluster
        # variance, counted on the rows the power calculation uses (an observed
        # outcome) and ignoring a missing center id, as its table() does
        for (grp in c("control", "treatment")) {
          ids <- data[
            data$group == grp & !is.na(data[[outcome_name]]) & has_patients,
            power_goal_cluster_id,
            drop = TRUE
          ]
          n_centers <- length(unique(ids[!is.na(ids)]))
          if (n_centers < 2) {
            stop(paste0(
              "The '", grp, "' arm has fewer than two distinct ",
              "power_goal_cluster_id centers with an observed outcome",
              if (input_data_structure == "center_level") " and patients",
              ", so ",
              "the stage-1 design effect is not identifiable. Provide data ",
              "with at least two such centers per arm, or set icc = NULL."
            ))
          }
        }
      }
    }
  } else if (!is.null(icc)) {
    # icc only affects the power calculation; without a power goal it is a no-op.
    input_note("icc is ignored because no power_goal was provided.")
  }

  # check if power_goal_approach is a character type
  # and is one of the defined approaches
  if (!is.character(power_goal_approach)) {
    stop("The power_goal_approach must be a character string.")
  }
  allowed_power_goal_approaches <- c("unconditional", "conditional")
  if (length(power_goal_approach) != 1 ||
    !(power_goal_approach %in% allowed_power_goal_approaches)) {
    stop(paste0(
      "The power_goal_approach must be one of the supported approaches: ",
      paste0("'", allowed_power_goal_approaches, "'", collapse = " or "), "."
    ))
  }


  # A value for an effect that is not in the model is not used. Said here,
  # after the input errors, as the warning below is.
  ignored_values <- c(
    if (!include_center_effects && !is.null(center_effects_optimization_values)) {
      "'center_effects_optimization_values' is ignored, because include_center_effects is FALSE."
    },
    if (!include_time_effects && !is.null(time_effect_optimization_value)) {
      "'time_effect_optimization_value' is ignored, because include_time_effects is FALSE."
    },
    if (length(center_characteristics) == 0 &&
      !is.null(center_characteristics_optimization_values)) {
      paste(
        "'center_characteristics_optimization_values' is ignored, because no",
        "center_characteristics are given."
      )
    }
  )
  for (text in ignored_values) input_warning(text)

  # A value outside the centers the model is fitted on asks it about a center
  # it has not seen. The estimate and its interval are then an extrapolation,
  # and with few centers the interval can cover every probability. It is
  # checked last, so that a warning never hides an input error, also when
  # warnings are errors.
  warn_center_extrapolation(
    data, outcome_name, intervention_components, center_characteristics,
    center_characteristics_optimization_values, additional_covariates,
    include_center_effects, include_time_effects, input_data_structure, weights
  )

  list(
    data = data,
    input_data_structure = input_data_structure,
    outcome_name = outcome_name,
    outcome_type = outcome_type,
    intervention_components = intervention_components,
    intervention_lower_bounds = intervention_lower_bounds,
    intervention_upper_bounds = intervention_upper_bounds,
    outcome_goal = outcome_goal,
    unit_costs = unit_costs,
    unit_cost_doses = unit_cost_doses,
    default_cost_fxn_type = default_cost_fxn_type,
    cost_list_of_vectors = cost_list_of_vectors,
    glm_family = glm_family,
    link = link,
    optimization_method = optimization_method,
    confidence_set_alpha = confidence_set_alpha,
    weights = weights,
    center_characteristics = center_characteristics,
    center_characteristics_optimization_values = center_characteristics_optimization_values,
    main_components = main_components,
    time_effect_optimization_value = time_effect_optimization_value,
    additional_covariates = additional_covariates,
    center_weights_for_outcome_goal = center_weights_for_outcome_goal,
    optimization_grid_search_step_size = optimization_grid_search_step_size,
    confidence_set_grid_step_size = confidence_set_grid_step_size,
    include_confidence_set = include_confidence_set,
    include_center_effects = include_center_effects,
    include_time_effects = include_time_effects,
    include_interaction_terms = include_interaction_terms,
    family_object = family_object,
    link = link,
    lower_outcome_goal = lower_outcome_goal,
    prev_recommended_interventions = prev_recommended_interventions,
    icc = icc
  )
}


#' refuse_invalid_center_weights
#'
#' @description Internal guard for the center weights: refuses a vector that
#' is not a set of weights at all, i.e. one holding a non-finite or a negative
#' value.
#'
#' @details Both checks are here rather than at one caller because
#' validate_inputs() is not the only entry point the weights arrive through.
#' The exported get_confidence_set() takes them directly and does not go
#' through validate_inputs(), so a negative weight passed to it reached the
#' interval unchecked and reported a "probability" above 1 for a binary
#' outcome. That is the same wrong number validate_inputs() already refused,
#' from the other door, and the two must refuse it in the same words: a caller
#' who moves between the two entry points should not be told two different
#' things about the same vector.
#'
#' Finiteness first, because neither of the comparisons that follow can be made
#' at all otherwise: any(NA < 0) is not FALSE, it is the R error "missing value
#' where TRUE/FALSE needed", and that is what a missing weight used to surface
#' as at the sum check in validate_inputs(). An Inf was refused there, but for
#' summing to Inf rather than for being one, and an Inf beside a -Inf sums to
#' NaN and hit the same opaque comparison. None of these is a weight, so each
#' is named here instead.
#'
#' Then non-negativity. The weights are a convex combination over the centers:
#' the reported outcome is sum(weight_i * outcome_i) over the center-level
#' effects, so it is a weighted MEAN of the per-center outcomes and has to lie
#' between the smallest and the largest of them. A negative weight breaks
#' exactly that, and summing to 1 does not rule one out: weights of -10 and 11
#' sum to 1 and gave 10.95 for a logit outcome, i.e. a reported "probability"
#' outside \[0, 1\] for a binary outcome. That is a wrong number rather than an
#' error, so it is refused and not clamped: a caller who passed a negative
#' weight did not mean a weight of 0, and quietly substituting one would answer
#' a question they did not ask.
#'
#' A weight of exactly 0 is allowed, deliberately. It means a center the
#' recommendation is not being computed for, which is a meaningful thing to ask
#' for and is what the package itself builds from
#' center_effects_optimization_values: the named center gets weight 1 and every
#' other center 0. Refusing 0 would refuse that documented path. All weights
#' being 0 is a different matter, and it is refused by validate_inputs()' sum
#' check, which is what keeps its renormalisation from dividing by zero: a
#' vector summing to 0 is 1 away from 1, not within 0.001 of it.
#'
#' Compared against a small negative tolerance rather than against 0. A weight
#' a caller computes as a residual, one minus the others, can land a few
#' floating-point units below zero while the vector still sums to 1, and
#' refusing that would contradict allowing a weight of exactly 0. Anything
#' further below zero than rounding explains is a weight the caller meant to be
#' negative.
#'
#' The SUM check is deliberately NOT here. validate_inputs() both refuses a sum
#' far from 1 and renormalises what it accepts, and the renormalisation is what
#' makes the tolerance safe; get_confidence_set() has no such step and is
#' documented as taking the weights the optimization ran with, so requiring a
#' unit sum of it would refuse the vector its own caller has already
#' normalised for it.
#'
#' @param center_weights_for_outcome_goal A numeric vector of center weights.
#'
#' @return Invisibly NULL when every weight is finite and non-negative. Raises
#' otherwise.
#'
#' @noRd
refuse_invalid_center_weights <- function(center_weights_for_outcome_goal) {
  # Numeric first, because the two checks below cannot see anything else.
  # is.finite() is TRUE for every level of a factor, so a factor passes it, and
  # the comparison that follows then gives NA and the caller fails on the
  # opaque "missing value where TRUE/FALSE needed" that this function exists to
  # replace. A character vector fails the comparison the same way.
  if (!is.numeric(center_weights_for_outcome_goal)) {
    stop(paste(
      "center_weights_for_outcome_goal must be a numeric vector. A factor or",
      "character vector is not weights, even when its levels look like",
      "numbers: use as.numeric() on the values themselves."
    ))
  }
  if (!all(is.finite(center_weights_for_outcome_goal))) {
    stop(paste(
      "values in center_weights_for_outcome_goal must all be finite",
      "numbers. NA, NaN, Inf and -Inf are not weights. If the weights were",
      "not supplied, they were derived from the 'center_sample_size'",
      "column, which cannot be zero for every center."
    ))
  }
  if (any(center_weights_for_outcome_goal < -8 * .Machine$double.eps)) {
    stop(paste(
      "values in center_weights_for_outcome_goal must be non-negative.",
      "The weights average the per-center outcomes, so a negative weight",
      "makes the reported outcome fall outside the range of the outcomes it",
      "averages, and for a binary outcome outside [0, 1]. A weight of 0 is",
      "allowed and excludes that center from the average."
    ))
  }
  invisible(NULL)
}


#' refuse_non_unit_weight_sum
#'
#' @description Internal guard for the center weights as a SET: refuses a
#' vector whose sum is not 1, within the tolerance validate_inputs() has always
#' used.
#'
#' @details This is the other way to the wrong number
#' refuse_invalid_center_weights() refuses. That guard rules out a vector no
#' single element of which is a weight; this one rules out a vector every
#' element of which could be, but which is not a set of weights taken together.
#' The reported outcome is sum(weight_i * outcome_i) over the center-level
#' effects, so it is a weighted mean only when the weights sum to 1: a set
#' summing to 12 scales the intervention and center-characteristic contribution
#' of every reported outcome by 12, and 16 weights of 12/16 gave a
#' confidence interval of 1.406 to 2.147 for an outcome that is a proportion.
#' Every weight there is non-negative and finite, so neither of the checks in
#' refuse_invalid_center_weights() applies, and the symptom is the same one it
#' exists to prevent.
#'
#' Shared with the exported get_confidence_set() rather than left at
#' validate_inputs(), which is the same reason the checks in
#' refuse_invalid_center_weights() are shared: get_confidence_set() does not go
#' through validate_inputs(), so a caller reaching it directly had no sum check
#' at all, and the @param text saying the weights must sum to 1 was true of one
#' entry point and not the other.
#'
#' REFUSED rather than renormalised at get_confidence_set(), while
#' validate_inputs() both refuses this and renormalises what it accepts. The two
#' are not inconsistent: both entry points refuse exactly the same vectors, in
#' the same words. What differs is what happens to a vector inside the
#' tolerance, and there validate_inputs() can do more because it OWNS the
#' weights -- it is where they are derived and normalised, and lago_optimization()
#' only narrows them afterwards to the centers glm() fitted, through
#' align_center_weights_to_fit(), which renormalises when it drops one.
#' get_confidence_set() is handed weights an optimization has
#' already run with, and is documented as computing the interval at those
#' weights, so renormalising them there would report an interval for a
#' different weighting than the point estimate it is printed beside. That is the
#' same class of defect as reporting the interval of one period beside the
#' estimate of another.
#'
#' Renormalising there would also not be a no-op on correct input, which is what
#' settles it. validate_inputs() stores center_sizes / total_sample_size, and
#' about one such vector in six hundred does not sum to exactly 1 in floating
#' point; dividing by that sum again perturbs every weight by up to one unit in
#' the last place and moves the interval. So renormalising in the name of
#' matching the other entry point would change the numbers of runs that are
#' already correct, and refusing changes none of them: every internal path
#' arrives with a sum of 1 to within rounding, because validate_inputs() has
#' already normalised it and align_center_weights_to_fit() renormalises it when
#' it drops a center.
#'
#' The tolerance is validate_inputs()' own 0.001 rather than a tighter one.
#' It is documented, callers rely on it, and it is what says the input was MEANT
#' to be a set of weights; tightening it here would turn input the package has
#' always accepted into a hard error, and the residual scaling it admits is at
#' most 0.1%, which is the tolerance's own documented meaning. What this closes
#' is the unbounded case.
#'
#' A vector summing to 0, including one that is all zeros, is refused here: it
#' is 1 away from 1. That is also what keeps validate_inputs()'
#' renormalisation from dividing by zero.
#'
#' @param center_weights_for_outcome_goal A numeric vector of center weights,
#' already known to be finite and non-negative.
#'
#' @return Invisibly NULL when the weights sum to 1 within the tolerance.
#' Raises otherwise.
#'
#' @noRd
refuse_non_unit_weight_sum <- function(center_weights_for_outcome_goal) {
  if (abs(sum(center_weights_for_outcome_goal) - 1) >= 0.001) {
    stop(paste(
      "values in center_weights_for_outcome_goal must",
      "sum up to 1."
    ))
  }
  invisible(NULL)
}

# The rows the outcome model fits: those with every model column the data has
# observed and a positive glm weight, when there are weights.
rows_model_fits <- function(data, columns, weights) {
  columns <- intersect(unlist(columns), names(data))
  rows <- stats::complete.cases(data[columns])
  if (is.numeric(weights) && length(weights) == nrow(data)) {
    rows <- rows & !is.na(weights) & weights > 0
  }
  rows
}

# Signal an input note, a message about how the inputs were read. A sweep shows
# each one once rather than once per run. See show_each_input_note_once().
input_note <- function(text) {
  message(structure(
    class = c("lago_input_note", "message", "condition"),
    list(message = paste0(text, "\n"), call = NULL)
  ))
}

# Signal an input warning, a warning about how the inputs were read. A sweep
# shows each one once rather than once per run, as it does an input note. A
# call given heads the warning, as it would head one from warning().
input_warning <- function(text, call = NULL) {
  warning(structure(
    class = c("lago_input_warning", "warning", "condition"),
    list(message = text, call = call)
  ))
}

# Warns when a center characteristic is optimized at a value outside the range
# the model is fitted on. That range is over the rows glm() keeps, which are
# the rows where every model column is observed, so a center whose rows all
# lack the outcome does not count. A logical characteristic is read as 0 and 1,
# and a factor or text one as its 0 or 1 dummy, as the model fits them. Values
# within floating point of an end are not outside it.
warn_center_extrapolation <- function(data, outcome_name, intervention_components,
                                      center_characteristics, values,
                                      additional_covariates, include_center_effects,
                                      include_time_effects, input_data_structure,
                                      weights) {
  if (is.null(center_characteristics) || is.null(values)) {
    return(invisible(NULL))
  }
  model_cols <- intersect(c(
    outcome_name, intervention_components, center_characteristics,
    additional_covariates,
    if (isTRUE(include_time_effects)) "period",
    if (identical(input_data_structure, "center_level")) "center_sample_size"
  ), names(data))
  used <- stats::complete.cases(data[model_cols])
  # glm() fits no row with a missing or zero weight. For center level data the
  # weights are center_sample_size, and the user's weights are not used.
  if (identical(input_data_structure, "center_level")) {
    weights <- data$center_sample_size
  }
  if (is.numeric(weights) && length(weights) == nrow(data)) used <- used & !is.na(weights) & weights > 0
  warn_outside_centers(
    center_characteristics, values,
    lapply(center_characteristics, function(cc) data[[cc]][used])
  )
  invisible(NULL)
}

# Warns when a center characteristic value is outside its fitted values. A
# logical characteristic is read as 0 and 1, and a factor or text one as its 0
# or 1 dummy, as the model fits them. Values within floating point of an end
# are not outside it.
warn_outside_centers <- function(center_characteristics, values, columns) {
  outside <- character(0)
  for (i in seq_along(center_characteristics)) {
    x <- columns[[i]]
    v <- values[i]
    if (is.null(x) || !is.finite(v)) next
    # a Date or date-time is fitted as its number, as glm() reads it
    if (is.logical(x) || inherits(x, c("Date", "POSIXt"))) x <- as.numeric(x)
    dummy <- !is.numeric(x)
    rng <- if (dummy) {
      c(0, 1)
    } else {
      if (!any(is.finite(x))) next
      range(x[is.finite(x)])
    }
    tol <- sqrt(.Machine$double.eps) * max(1, abs(rng))
    if (v < rng[1] - tol || v > rng[2] + tol) {
      outside <- c(outside, sprintf(
        "%s = %s, while the data the model is fitted on has %s to %s",
        center_characteristics[i], format(v, digits = 10),
        format(rng[1], digits = 10), format(rng[2], digits = 10)
      ))
    }
  }
  if (length(outside) > 0) {
    input_warning(paste0(
      "The recommendation is for a center outside the centers in the data: ",
      paste(outside, collapse = ", and "), ". The estimated outcome and its ",
      "confidence interval are then an extrapolation of the model, so the ",
      "interval can be very wide. Choose a value within the data, or make ",
      "sure the model holds for such a center."
    ))
  }
  invisible(NULL)
}

# Refuses a string argument that is not one value, such as character(0), two
# strings or NA_character_, which would otherwise fail on an unclear message.
refuse_not_one_string <- function(x, arg) {
  if (length(x) != 1 || is.na(x)) {
    stop(paste0(
      "'", arg, "' must be one character string, but it is ",
      if (length(x) != 1) paste0("a vector of length ", length(x)) else "NA",
      "."
    ))
  }
  invisible(NULL)
}

# A one dimensional array, as tapply() returns, is read as the plain vector it
# holds, with its names kept.
drop_1d_array <- function(x) {
  if (is.array(x) && length(dim(x)) == 1) {
    x <- stats::setNames(as.vector(x), names(x))
  }
  x
}

# Matches a named vector or list to the expected names, and returns it in their
# order without names. The names are read when one of them is an expected name,
# or when there are two or more values and each has a name. Names of another
# kind, such as the "95%" that quantile() gives one value, are ignored, and the
# values are kept in their order, as a vector without names is. Names that are
# read must be exactly the expected ones, each once.
match_by_names <- function(x, expected, arg, what) {
  x_names <- names(x)
  if (is.null(x_names)) {
    return(x)
  }
  every_value_named <- length(x) > 1 && !anyNA(x_names) && all(nzchar(x_names))
  if (!any(x_names %in% expected) && !every_value_named) {
    return(unname(x))
  }
  if (anyNA(x_names) || anyDuplicated(x_names) > 0 ||
    !setequal(x_names, expected)) {
    stop(paste0(
      "The names of ", arg, " must be the ", what, ", each once: ",
      paste0("'", expected, "'", collapse = ", "),
      ". Fix the names, or remove them with unname() to match the values to ",
      "the ", what, " in this order."
    ))
  }
  unname(x[match(expected, x_names)])
}

# Refuses a column list that names a column more than once.
refuse_repeated_columns <- function(columns, arg) {
  repeated <- unique(columns[duplicated(columns)])
  if (length(repeated) > 0) {
    stop(paste0(
      arg, " names the column(s) ", paste0("'", repeated, "'", collapse = ", "),
      " more than once. Give each column once."
    ))
  }
  invisible(NULL)
}
