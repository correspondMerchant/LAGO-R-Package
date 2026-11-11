# Helpers behind lago_app(). They take the app's settings as a plain list (a
# "spec") and do not depend on Shiny, so they can be tested directly. The run
# and the R code shown in the app both come from .app_build(), so the code
# always reproduces the run.

# A spec with every field the app reads, at its default.
.app_empty_spec <- function() {
  list(
    data_name = "data",
    outcome = "", outcome_type = "binary",
    goal_mode = "outcome", goal = NA_real_, intention = "maximize",
    components = character(0), lower = numeric(0), upper = numeric(0),
    cost_mode = "unit", unit_costs = numeric(0), cost_type = "linear",
    doses = numeric(0), coefs = list(),
    interactions = character(0),
    center_mode = "none", center_chars = character(0), center_char_values = numeric(0),
    center_col = "", center_for = "",
    time_on = FALSE, period_col = "", period_value = "",
    covariates = character(0),
    arm_col = "", arm_value = "", power_goal = 0.8, n_centers = 10, n_patients = 30,
    approach = "unconditional", icc = numeric(0), cluster_col = "",
    include_cs = TRUE
  )
}

# The bundled examples, set up so the first run works without any change.
.app_presets <- function() {
  list(
    BB_data = list(
      outcome = "pp3_oxytocin_mother", outcome_type = "binary",
      components = c("coaching_updt", "launch_duration"), goal = 0.85
    ),
    mtcars = list(
      outcome = "mpg", outcome_type = "continuous",
      components = c("gear", "qsec"), goal = 25
    )
  )
}

.app_numeric_cols <- function(data) {
  names(data)[vapply(data, is.numeric, logical(1))]
}

.app_is_binary <- function(x) {
  x <- x[!is.na(x)]
  is.numeric(x) && length(x) > 0 && all(x %in% c(0, 1))
}

# A starting spec for a dataset: its preset when it has one, otherwise the
# first 0/1 column as the outcome and the next two numeric columns as the
# components.
.app_defaults <- function(data, data_name = "data") {
  spec <- .app_empty_spec()
  spec$data_name <- data_name
  numeric_cols <- .app_numeric_cols(data)
  preset <- .app_presets()[[data_name]]
  if (!is.null(preset) && all(c(preset$outcome, preset$components) %in% names(data))) {
    spec$outcome <- preset$outcome
    spec$outcome_type <- preset$outcome_type
    spec$components <- preset$components
  } else if (length(numeric_cols)) {
    binary <- numeric_cols[vapply(data[numeric_cols], .app_is_binary, logical(1))]
    spec$outcome <- if (length(binary)) binary[1] else numeric_cols[1]
    spec$outcome_type <- if (.app_is_binary(data[[spec$outcome]])) "binary" else "continuous"
    spec$components <- utils::head(setdiff(numeric_cols, spec$outcome), 2)
  }
  spec <- .app_set_component_defaults(spec, data)
  spec$goal <- if (!is.null(preset$goal) && identical(spec$outcome, preset$outcome)) {
    preset$goal
  } else {
    .app_default_goal(data, spec$outcome, spec$outcome_type)
  }
  spec
}

# A goal in the middle of the outcome's range, or 0.85 for a binary outcome.
.app_default_goal <- function(data, outcome, outcome_type) {
  if (identical(outcome_type, "binary") || !outcome %in% names(data)) {
    return(0.85)
  }
  y <- data[[outcome]]
  if (!is.numeric(y) || all(is.na(y))) {
    return(NA_real_)
  }
  signif(mean(range(y, na.rm = TRUE)), 4)
}

# Bounds from each component's observed range and a unit cost of 1.
.app_set_component_defaults <- function(spec, data) {
  comps <- spec$components
  rng <- lapply(comps, function(cc) range(data[[cc]], na.rm = TRUE))
  # the package needs bounds of 0 or more
  spec$lower <- pmax(0, vapply(rng, `[`, numeric(1), 1))
  spec$upper <- vapply(rng, `[`, numeric(1), 2)
  spec$unit_costs <- rep(1, length(comps))
  spec$doses <- rep(NA_real_, length(comps))
  spec$coefs <- rep(list(c(0, 1)), length(comps))
  spec
}

# The grid step of the confidence set: about 20 steps across each range.
.app_cs_steps <- function(lower, upper) {
  span <- upper - lower
  ifelse(span > 0, pmax(span / 20, 1e-6), 1)
}

# The number of confidence set grid points for these bounds and steps.
.app_cs_grid_size <- function(lower, upper, steps) {
  prod(floor((upper - lower) / steps + 1e-9) + 1)
}

# Parse a typed coefficient list such as "0, 2, 0.5" into numbers, or NULL.
.app_parse_coefs <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(NULL)
  }
  parts <- strsplit(trimws(text), "[,; ]+")[[1]]
  values <- suppressWarnings(as.numeric(parts))
  if (!length(values) || anyNA(values)) NULL else values
}

# The R code that reads an uploaded CSV the same way the app did.
.app_read_csv_code <- function(name, sep = ",", dec = ".") {
  extra <- c(
    if (!identical(sep, ",")) sprintf("sep = %s", deparse1(sep)),
    if (!identical(dec, ".")) sprintf("dec = %s", deparse1(dec))
  )
  sprintf(
    "read.csv(%s)",
    paste(c(deparse1(name), extra, "na.strings = c(\"\", \"NA\")", "fileEncoding = \"UTF-8-BOM\""), collapse = ", ")
  )
}

# Parse the ICC box: empty, one value, or a control and a treatment value.
.app_parse_icc <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(numeric(0))
  }
  values <- .app_parse_coefs(text)
  if (is.null(values) || length(values) > 2) NA_real_ else values
}

# Every choice list of the app for a dataset, with the default selections, so
# a load or reset can set them all at once.
.app_choices <- function(data, spec) {
  cols <- names(data)
  numeric_cols <- .app_numeric_cols(data)
  others <- setdiff(cols, spec$outcome)
  two_level <- cols[vapply(data, function(v) length(unique(stats::na.omit(v))) == 2, logical(1))]
  arms <- setdiff(two_level, spec$outcome)
  arm <- if ("group" %in% arms) "group" else if (length(arms)) arms[1] else character(0)
  arm_values <- if (length(arm)) sort(unique(as.character(stats::na.omit(data[[arm]])))) else character(0)
  arm_value <- if ("treatment" %in% arm_values) "treatment" else if ("1" %in% arm_values) "1" else utils::tail(arm_values, 1)
  center_col <- if ("center" %in% others) "center" else ""
  period_col <- if ("period" %in% others) "period" else ""
  periods <- if (nzchar(period_col)) .app_periods(data[[period_col]])$labels else character(0)
  list(
    outcomes = numeric_cols,
    components = setdiff(numeric_cols, spec$outcome),
    arms = arms, arm = arm, arm_values = arm_values, arm_value = arm_value,
    clusters = if (length(arm)) .app_cluster_choices(data, spec$outcome, arm, arm_value, exclude = spec$components) else character(0),
    others = others,
    center_chars = setdiff(numeric_cols, c(spec$outcome, spec$components)),
    center_col = center_col,
    centers = if (nzchar(center_col)) sort(unique(as.character(stats::na.omit(data[[center_col]])))) else character(0),
    period_col = period_col,
    periods = periods, period = utils::tail(periods, 1),
    covariates = setdiff(cols, c(spec$outcome, spec$components))
  )
}

# The period labels of a column in time order, and whether they are numbers.
# A factor keeps its level order. Text labels are ordered by the first number
# in them, so Q2 comes before Q10, and then by their text.
.app_periods <- function(x) {
  labels <- if (is.factor(x)) levels(droplevels(x)) else unique(as.character(stats::na.omit(x)))
  as_num <- suppressWarnings(as.numeric(labels))
  if (length(labels) && !anyNA(as_num)) {
    return(list(labels = labels[order(as_num)], numeric = TRUE))
  }
  if (is.factor(x)) {
    return(list(labels = labels, numeric = FALSE))
  }
  # month names, such as Jan or January, in calendar order
  months <- match(tolower(substr(labels, 1, 3)), tolower(month.abb))
  if (!anyNA(months) && all(tolower(labels) %in% tolower(c(month.abb, month.name)))) {
    return(list(labels = labels[order(months)], numeric = FALSE))
  }
  # otherwise by the numbers in each label, the largest first, so a year
  # comes before the quarter in labels such as Q1 2021
  numbers <- lapply(regmatches(labels, gregexpr("[0-9]+(\\.[0-9]+)?", labels)), as.numeric)
  largest <- vapply(numbers, function(x) if (length(x)) max(x) else NA_real_, numeric(1))
  first <- vapply(numbers, function(x) if (length(x)) x[1] else NA_real_, numeric(1))
  list(labels = labels[order(largest, first, labels, na.last = TRUE)], numeric = FALSE)
}

# Read an uploaded CSV with the separator and decimal mark the user chose.
.app_read_csv <- function(path, sep = ",", dec = ".") {
  utils::read.csv(
    path,
    sep = sep, dec = dec, na.strings = c("", "NA"),
    check.names = TRUE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
  )
}

# The prepared data, the lago_optimization() arguments and the R code for a
# spec. `data` is the dataset as loaded. Columns the package reads by a fixed
# name (group, center and period) and the interaction columns are built on a
# copy, and the same lines are added to the code.
.app_build <- function(spec, data) {
  d <- data
  prep <- character(0)
  add <- function(line, value_fun) {
    prep[[length(prep) + 1L]] <<- line
    value_fun()
  }
  rstr <- function(x) deparse1(x)

  for (term in spec$interactions) {
    ab <- strsplit(term, ":", fixed = TRUE)[[1]]
    add(
      sprintf("data[[%s]] <- data[[%s]] * data[[%s]]", rstr(term), rstr(ab[1]), rstr(ab[2])),
      function() d[[term]] <<- d[[ab[1]]] * d[[ab[2]]]
    )
  }
  power_on <- .app_power_on(spec)
  if (power_on && .app_needs_group(spec, data)) {
    add(
      sprintf(
        "data$group <- ifelse(as.character(data[[%s]]) == %s, \"treatment\", \"control\")",
        rstr(spec$arm_col), rstr(as.character(spec$arm_value))
      ),
      function() {
        d$group <<- ifelse(as.character(d[[spec$arm_col]]) == as.character(spec$arm_value), "treatment", "control")
      }
    )
  }
  if (identical(spec$center_mode, "effects") && nzchar(spec$center_col) && spec$center_col != "center") {
    add(
      sprintf("data$center <- factor(data[[%s]])", rstr(spec$center_col)),
      function() d$center <<- factor(d[[spec$center_col]])
    )
  }
  period_value <- NA_real_
  if (isTRUE(spec$time_on) && nzchar(spec$period_col) && spec$period_col %in% names(data)) {
    periods <- .app_periods(data[[spec$period_col]])
    # the package reads the period as a factor whose labels are numbers
    if (periods$numeric) {
      period_value <- as.numeric(spec$period_value)
      if (spec$period_col != "period" || !is.factor(data[[spec$period_col]])) {
        add(
          sprintf("data$period <- factor(data[[%s]])", rstr(spec$period_col)),
          function() d$period <<- factor(d[[spec$period_col]])
        )
      }
    } else {
      # text labels become 1, 2, ... in time order
      period_value <- match(spec$period_value, periods$labels)
      add(
        sprintf(
          "data$period <- factor(match(as.character(data[[%s]]), %s))",
          rstr(spec$period_col), paste(deparse(periods$labels, width.cutoff = 500L), collapse = " ")
        ),
        function() d$period <<- factor(match(as.character(d[[spec$period_col]]), periods$labels))
      )
    }
  }

  comps <- spec$components
  inter <- spec$interactions
  args <- list(
    outcome_name = spec$outcome,
    outcome_type = spec$outcome_type,
    intervention_components = c(comps, inter),
    intervention_lower_bounds = spec$lower,
    intervention_upper_bounds = spec$upper
  )
  if (!identical(spec$goal_mode, "power")) args$outcome_goal <- spec$goal
  args$outcome_goal_intention <- spec$intention
  if (identical(spec$cost_mode, "coef")) {
    args$cost_list_of_vectors <- lapply(spec$coefs, as.numeric)
  } else {
    args$unit_costs <- spec$unit_costs
    args$default_cost_fxn_type <- spec$cost_type
    if (identical(spec$cost_type, "cubic") && any(!is.na(spec$doses))) {
      args$unit_cost_doses <- spec$doses
    }
  }
  if (length(inter)) {
    args$include_interaction_terms <- TRUE
    args$main_components <- comps
  }
  if (identical(spec$center_mode, "chars") && length(spec$center_chars)) {
    args$center_characteristics <- spec$center_chars
    args$center_characteristics_optimization_values <- spec$center_char_values
  }
  if (identical(spec$center_mode, "effects")) {
    args$include_center_effects <- TRUE
    if (nzchar(spec$center_for)) args$center_effects_optimization_values <- spec$center_for
  }
  if (length(spec$covariates)) args$additional_covariates <- spec$covariates
  if (isTRUE(spec$time_on)) {
    args$include_time_effects <- TRUE
    args$time_effect_optimization_value <- period_value
  }
  if (power_on) {
    args$power_goal <- spec$power_goal
    args$num_centers_in_next_stage <- spec$n_centers
    args$patients_per_center_in_next_stage <- spec$n_patients
    if (!identical(spec$approach, "unconditional")) args$power_goal_approach <- spec$approach
    if (length(spec$icc) && !anyNA(spec$icc)) {
      args$icc <- spec$icc
      if (any(spec$icc > 0)) args$power_goal_cluster_id <- spec$cluster_col
    }
  }
  if (isTRUE(spec$include_cs)) {
    args$confidence_set_grid_step_size <- signif(.app_cs_steps(spec$lower, spec$upper), 4)
  } else {
    args$include_confidence_set <- FALSE
  }

  list(data = d, args = args, prep = prep, code = .app_code(spec$data_name, prep, args))
}

.app_power_on <- function(spec) {
  identical(spec$outcome_type, "binary") && spec$goal_mode %in% c("power", "both")
}

# A group column is built unless the data already has a usable one picked as is.
.app_needs_group <- function(spec, data) {
  !(identical(spec$arm_col, "group") && identical(as.character(spec$arm_value), "treatment") &&
    .app_group_ok(data))
}

.app_group_ok <- function(data) {
  "group" %in% names(data) && all(data$group %in% c("treatment", "control")) &&
    all(c("treatment", "control") %in% data$group)
}

# The R code that reproduces a run: load the data, prepare it, call
# lago_optimization() with the same arguments.
.app_code <- function(data_name, prep, args, call = "lago_optimization", result = "result") {
  arg_lines <- vapply(names(args), function(nm) {
    sprintf("  %s = %s", nm, paste(deparse(args[[nm]], width.cutoff = 500L), collapse = " "))
  }, character(1))
  c(
    "library(LAGOtrials)",
    sprintf("data <- %s", data_name),
    prep,
    sprintf("%s <- %s(", result, call),
    paste0(c("  data = data", arg_lines), collapse = ",\n"),
    ")"
  )
}

# Problems that stop a run, each with the step it belongs to and a message that
# says what to change. An empty data frame means the settings are ready.
.app_problems <- function(spec, data) {
  out <- list()
  note <- function(step, msg) out[[length(out) + 1L]] <<- data.frame(step = step, message = msg)
  fmt <- function(x) format(x, digits = 4)

  if (is.null(data) || !nrow(data)) {
    note("data", "Load a dataset or upload a CSV file.")
    return(do.call(rbind, out))
  }
  numeric_cols <- .app_numeric_cols(data)
  if (length(numeric_cols) < 2) {
    note("data", "The data needs at least two numeric columns: one outcome and one intervention component.")
  }

  y <- if (nzchar(spec$outcome) && spec$outcome %in% names(data)) data[[spec$outcome]]
  if (is.null(y)) {
    note("outcome", "Choose the outcome column.")
  } else if (identical(spec$outcome_type, "binary") && !.app_is_binary(y)) {
    note("outcome", sprintf("The outcome '%s' is not 0/1, so it cannot be binary. Choose Continuous or another column.", spec$outcome))
  }
  power_on <- .app_power_on(spec)
  if (!identical(spec$goal_mode, "power")) {
    if (!is.finite(spec$goal)) {
      note("outcome", "Enter the outcome goal.")
    } else if (identical(spec$outcome_type, "binary") && (spec$goal <= 0 || spec$goal >= 1)) {
      note("outcome", "A binary outcome goal is a probability, so it must be between 0 and 1.")
    }
  }

  comps <- spec$components
  if (!length(comps)) {
    note("intervention", "Choose at least one intervention component.")
  } else {
    if (spec$outcome %in% comps) note("intervention", "The outcome cannot also be an intervention component.")
    for (k in seq_along(comps)) {
      lo <- spec$lower[k]
      up <- spec$upper[k]
      if (!is.finite(lo) || !is.finite(up)) {
        note("intervention", sprintf("Enter both bounds for %s.", comps[k]))
      } else if (lo < 0) {
        note("intervention", sprintf("The lower bound of %s must be 0 or more.", comps[k]))
      } else if (lo > up) {
        note("intervention", sprintf("The upper bound of %s (%s) must not be below its lower bound (%s).", comps[k], fmt(up), fmt(lo)))
      }
    }
    if (identical(spec$cost_mode, "coef")) {
      for (k in seq_along(comps)) {
        if (is.null(spec$coefs[[k]])) {
          note("intervention", sprintf("Enter the cost coefficients of %s as numbers separated by commas, such as 0, 2.", comps[k]))
        }
      }
    } else {
      bad <- comps[!is.finite(spec$unit_costs) | spec$unit_costs < 0]
      if (length(bad)) note("intervention", sprintf("Enter a unit cost of 0 or more for %s.", paste(bad, collapse = ", ")))
      if (identical(spec$cost_type, "cubic")) {
        if (any(!is.na(spec$doses)) && anyNA(spec$doses)) {
          note("intervention", "Enter the dose where the unit cost was measured for every component, or leave all of them empty.")
        }
        out_of <- which(!is.na(spec$doses) & (spec$doses < spec$lower | spec$doses > spec$upper))
        for (k in out_of) {
          note("intervention", sprintf("The dose where the unit cost of %s was measured must be within its bounds.", comps[k]))
        }
      }
    }
    if (isTRUE(spec$include_cs) && all(is.finite(spec$lower), is.finite(spec$upper))) {
      size <- .app_cs_grid_size(spec$lower, spec$upper, .app_cs_steps(spec$lower, spec$upper))
      if (size > 1e5) {
        note("intervention", sprintf("The confidence set grid would have %s points. Use fewer components or turn off the confidence set.", format(size, big.mark = ",")))
      }
    }
  }

  role_cols <- c(spec$outcome, comps)
  for (cv in spec$covariates) {
    if (cv %in% names(data) && length(unique(stats::na.omit(data[[cv]]))) < 2) {
      note("centers", sprintf("The covariate %s has only one value, so the model cannot adjust for it. Remove it.", cv))
    }
  }
  if (identical(spec$center_mode, "chars")) {
    if (!length(spec$center_chars)) note("centers", "Choose at least one center characteristic, or choose None.")
    bad <- spec$center_chars[!is.finite(spec$center_char_values)]
    if (length(bad)) note("centers", sprintf("Enter the value to optimize at for %s.", paste(bad, collapse = ", ")))
  }
  if (identical(spec$center_mode, "effects")) {
    if (!nzchar(spec$center_col)) {
      note("centers", "Choose the column that identifies the centers.")
    } else if (spec$center_col %in% role_cols) {
      note("centers", "The center column cannot also be the outcome or a component.")
    } else {
      # glm() leaves out a row with a missing value in a model column, so a
      # center counts only when one of its rows is complete
      used <- intersect(c(spec$outcome, comps, spec$covariates, if (isTRUE(spec$time_on)) spec$period_col), names(data))
      complete <- stats::complete.cases(data[used])
      centers <- as.character(data[[spec$center_col]])
      fitted <- unique(centers[complete & !is.na(centers)])
      if (length(fitted) < 2) {
        note("centers", "Fixed center effects need at least two centers with a row where every model column is observed.")
      } else if (nzchar(spec$center_for) && !spec$center_for %in% fitted) {
        note("centers", sprintf("The center %s has no row where every model column is observed, so it cannot be optimized for. Choose another center.", spec$center_for))
      }
    }
  }
  if (isTRUE(spec$time_on)) {
    if (!nzchar(spec$period_col)) {
      note("centers", "Choose the column that holds the time period.")
    } else if (!nzchar(spec$period_value)) {
      note("centers", "Choose the period to optimize for.")
    }
  }

  if (spec$goal_mode %in% c("power", "both") && !identical(spec$outcome_type, "binary")) {
    note("outcome", "A power goal works only for a binary outcome. Choose Outcome goal.")
  }
  if (power_on) {
    if (!identical(spec$intention, "maximize")) note("outcome", "A power goal needs the direction Maximize.")
    if (!nzchar(spec$arm_col)) {
      note("outcome", "Choose the column that says which patients were in the treatment arm.")
    } else if (spec$arm_col %in% names(data)) {
      arm <- as.character(data[[spec$arm_col]])
      if (anyNA(arm)) {
        note("outcome", sprintf("The treatment arm column '%s' has missing values.", spec$arm_col))
      } else if (!any(arm == as.character(spec$arm_value)) || all(arm == as.character(spec$arm_value))) {
        note("outcome", "The treatment arm column needs both treated and control patients.")
      }
    }
    if (!is.finite(spec$power_goal) || spec$power_goal < 0.05 || spec$power_goal >= 1) {
      note("outcome", "The power goal must be at least 0.05 and below 1.")
    }
    if (!is.finite(spec$n_centers) || spec$n_centers <= 0 || !is.finite(spec$n_patients) || spec$n_patients <= 0) {
      note("outcome", "Enter the number of centers and patients per center in the next stage.")
    }
    if (length(spec$icc)) {
      if (anyNA(spec$icc) || any(spec$icc < 0 | spec$icc >= 1)) {
        note("outcome", "Enter the ICC as one value, or as a control and a treatment value, each at least 0 and below 1.")
      } else if (any(spec$icc > 0) && !nzchar(spec$cluster_col)) {
        note("outcome", "An ICC above 0 needs the column that identifies the centers.")
      }
    }
  }

  if (!length(out)) {
    return(data.frame(step = character(0), message = character(0)))
  }
  do.call(rbind, out)
}

# Run a package call and collect what it reports. Warnings and messages are
# kept, without the console colors, so the app can show them. Printed output
# is captured, so nothing goes to the R console behind the app.
.app_run <- function(fun) {
  warnings <- character(0)
  messages <- character(0)
  clean <- function(x) trimws(cli::ansi_strip(x))
  value <- NULL
  error <- NULL
  output <- utils::capture.output(
    value <- tryCatch(
      withCallingHandlers(
        fun(),
        warning = function(w) {
          warnings <<- c(warnings, clean(conditionMessage(w)))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          messages <<- c(messages, clean(conditionMessage(m)))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        error <<- clean(conditionMessage(e))
        NULL
      }
    )
  )
  list(
    value = value, error = error,
    warnings = unique(warnings), messages = unique(messages[nzchar(messages)]),
    output = output
  )
}

# The printed result as lines of text, in the order print() writes them. The
# printout uses both the output (tables) and the message stream (cli text), so
# both are captured on one connection.
.app_printout <- function(x) {
  lines <- character(0)
  con <- textConnection("lines", "w", local = TRUE)
  sink(con)
  sink(con, type = "message")
  on.exit({
    sink(type = "message")
    sink()
    close(con)
  })
  print(x)
  cli::ansi_strip(lines)
}

# Which goal the result was optimized for, as a short note after its value.
.app_goal_source <- function(r) {
  if (is.null(r$power_goal) || is.null(r$effective_outcome_goal)) {
    return("")
  }
  if (is.null(r$outcome_goal)) {
    return(" (from the power goal)")
  }
  if (r$effective_outcome_goal > r$outcome_goal + 1e-9) {
    " (from the power goal, which needs more than the outcome goal)"
  } else {
    " (the outcome goal, since the power goal needs no more)"
  }
}

# Notes that do not stop a run but that the user should know about, such as a
# column the app replaces.
.app_notes <- function(spec, data) {
  notes <- character(0)
  replaced <- function(name, from) {
    if (name %in% names(data) && !identical(from, name)) {
      notes[[length(notes) + 1L]] <<- sprintf(
        "Your data already has a column named %s. The app replaces it with one built from %s, as the package reads that name.",
        name, from
      )
    }
  }
  if (.app_power_on(spec) && nzchar(spec$arm_col) && .app_needs_group(spec, data)) replaced("group", spec$arm_col)
  if (identical(spec$center_mode, "effects") && nzchar(spec$center_col)) replaced("center", spec$center_col)
  if (isTRUE(spec$time_on) && nzchar(spec$period_col) && spec$period_col %in% names(data)) {
    periods <- .app_periods(data[[spec$period_col]])
    replaced("period", spec$period_col)
    if (!periods$numeric) {
      notes[[length(notes) + 1L]] <- sprintf(
        "The periods of %s are text, so the model numbers them: %s.",
        spec$period_col, paste(sprintf("%s = %d", periods$labels, seq_along(periods$labels)), collapse = ", ")
      )
    }
  }
  notes
}

# Columns that can identify the centers for the ICC: at least two centers with
# an observed outcome in each arm.
.app_cluster_choices <- function(data, outcome, arm_col, arm_value, exclude = character(0)) {
  if (length(outcome) != 1 || length(arm_col) != 1 || length(arm_value) != 1 ||
    !all(c(outcome, arm_col) %in% names(data))) {
    return(character(0))
  }
  treated <- as.character(data[[arm_col]]) == as.character(arm_value)
  seen <- !is.na(data[[outcome]]) & !is.na(treated)
  candidates <- setdiff(names(data), c(outcome, arm_col, exclude))
  ok <- vapply(candidates, function(cc) {
    v <- data[[cc]]
    in_arm <- function(arm) length(unique(stats::na.omit(v[seen & treated == arm])))
    in_arm(TRUE) >= 2 && in_arm(FALSE) >= 2 && length(unique(stats::na.omit(v))) < nrow(data)
  }, logical(1))
  candidates[ok]
}

# A message for a CSV that was read but probably with the wrong settings.
.app_csv_hint <- function(d, sep, dec) {
  if (ncol(d) == 1) {
    return("The file was read as one column. Choose the separator the file uses, such as Semicolon.")
  }
  text_cols <- names(d)[vapply(d, is.character, logical(1))]
  looks_comma <- vapply(text_cols, function(cc) {
    v <- stats::na.omit(d[[cc]])
    length(v) > 0 && all(grepl("^-?[0-9]+,[0-9]+$|^-?[0-9]+$", v)) && any(grepl(",", v))
  }, logical(1))
  if (identical(dec, ".") && any(looks_comma)) {
    return(sprintf(
      "The column(s) %s look like numbers with a decimal comma. Choose Comma as the decimal mark.",
      paste(text_cols[looks_comma], collapse = ", ")
    ))
  }
  NULL
}

# Header names the reader changed to valid R names, as "old (now new)".
.app_renamed <- function(path, sep, d) {
  if (ncol(d) < 2) {
    return(character(0))
  }
  raw <- tryCatch(
    names(utils::read.csv(path, sep = sep, nrows = 1, check.names = FALSE, fileEncoding = "UTF-8-BOM")),
    error = function(e) names(d)
  )
  if (length(raw) != length(names(d))) {
    return(character(0))
  }
  changed <- raw != names(d)
  sprintf("%s (now %s)", raw[changed], names(d)[changed])
}

# One plain sentence that sums up a result.
.app_summary_sentence <- function(r, comps) {
  fmt <- function(v) vapply(.lago_fmt_value(v), format, character(1), big.mark = ",")
  levels <- paste(sprintf("%s at %s", comps, fmt(r$rec_int)), collapse = ", ")
  if (isTRUE(r$shrinking_method_used)) {
    return(sprintf(
      "The goal is out of reach within the bounds. The shrinking method recommends %s, for a cost of %s and an estimated outcome of %s.",
      levels, fmt(r$rec_int_cost), fmt(r$est_outcome_goal)
    ))
  }
  sprintf(
    "The cheapest intervention that is estimated to reach the goal sets %s. It costs %s and its estimated outcome is %s.",
    levels, fmt(r$rec_int_cost), fmt(r$est_outcome_goal)
  )
}

# Steps a user can take when the package refuses a run.
.app_error_tips <- function() {
  c(
    "Check that the outcome column and its type are the ones you meant.",
    "Check that the components have an effect on the outcome in the first stage data.",
    "Remove covariates or center settings one at a time to see which one the model cannot fit.",
    "Narrow or widen the bounds, or lower the goal."
  )
}

# Where the cost designer starts: the costs now in use, in the form that holds
# them. Cost coefficients of length 2 or 5 open in the linear or cubic form,
# and unit costs open on the curve the package computes from them.
.app_designer_start <- function(spec) {
  n <- length(spec$components)
  if (identical(spec$cost_mode, "coef")) {
    lens <- vapply(spec$coefs, length, integer(1))
    form <- if (n && all(lens == 5L)) "cubic" else "linear"
    want <- if (form == "cubic") 5L else 2L
    coefs <- lapply(spec$coefs, function(v) if (length(v) == want) v else NULL)
    unit_costs <- vapply(spec$coefs, function(v) {
      if (length(v) >= 2 && is.finite(v[2]) && v[2] > 0) v[2] else 1
    }, numeric(1))
  } else {
    form <- spec$cost_type
    unit_costs <- ifelse(is.finite(spec$unit_costs) & spec$unit_costs >= 0, spec$unit_costs, 1)
    doses <- if (identical(form, "cubic") && any(!is.na(spec$doses)) && !anyNA(spec$doses)) spec$doses else NULL
    coefs <- tryCatch(
      cost_fxn_calculator(spec$lower, spec$upper, unit_costs, form, unit_cost_doses = doses),
      error = function(e) NULL
    )
  }
  list(form = form, unit_costs = unit_costs, coefs = coefs)
}
