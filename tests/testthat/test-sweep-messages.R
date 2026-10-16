# A sweep shows each distinct input note once, not once per run.

sweep_messages_args <- function() {
  d <- as.data.frame(BB_data)
  d$center <- d$site_name
  list(
    data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3),
    cost_list_of_vectors = list(c(0, 1), c(0, 1)), outcome_goal = 0.85,
    outcome_goal_intention = "maximize", include_center_effects = TRUE,
    optimization_grid_search_step_size = c(5, 1)
  )
}

# Collect every message the call emits and count the cli progress lines apart.
sweep_messages_collect <- function(expr) {
  plain <- character(0)
  progress <- 0
  value <- NULL
  # the printed results are not under test
  utils::capture.output(value <- withCallingHandlers(
    suppressWarnings(expr),
    message = function(m) {
      if (inherits(m, "cliMessage")) {
        progress <<- progress + 1
      } else {
        plain <<- c(plain, sub("\n$", "", conditionMessage(m)))
      }
      invokeRestart("muffleMessage")
    }
  ))
  list(value = value, plain = plain, progress = progress)
}

sweep_messages_expected <- c(
  paste(
    "'center' column is not a factor type. To ensure",
    "the correct model fit, it has been converted",
    "to the factor type."
  ),
  paste(
    "'optimization_grid_search_step_size' was provided, so",
    "'optimization_method' has been switched to 'grid_search'.",
    "To use the numerical method instead, remove",
    "'optimization_grid_search_step_size'."
  )
)

test_that("lago_sensitivity() shows each input note once", {
  out <- sweep_messages_collect(do.call(lago_sensitivity, c(
    sweep_messages_args(),
    list(parameter = "outcome_goal", values = c(0.8, 0.85, 0.9))
  )))
  expect_identical(out$plain, sweep_messages_expected)
  expect_true(all(out$value$results$status == "ok"))
})

test_that("lago_budget() shows each input note once across its sweeps", {
  args <- sweep_messages_args()
  args$outcome_goal <- NULL
  out <- sweep_messages_collect(do.call(lago_budget, c(
    args,
    list(budget = 20, n_grid = 4)
  )))
  expect_identical(out$plain, sweep_messages_expected)
})

test_that("only a repeat of the same input note is hidden", {
  handler <- LAGOtrials:::show_each_input_note_once()
  shown <- character(0)
  withCallingHandlers(
    withCallingHandlers(
      {
        for (text in c("a", "b", "a", "c", "b")) LAGOtrials:::input_note(text)
        message("plain")
        message("plain")
      },
      message = handler
    ),
    message = function(m) {
      shown <<- c(shown, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(shown, c("a", "b", "c", "plain", "plain"))
})

test_that("a sweep shows a repeated message that is not an input note for every run", {
  stub <- LAGOtrials::lago_optimization
  body(stub) <- quote({
    LAGOtrials:::input_note("input")
    message("run")
    stop("stub")
  })
  local_mocked_bindings(lago_optimization = stub)
  out <- sweep_messages_collect(do.call(lago_sensitivity, c(
    sweep_messages_args(),
    list(parameter = "outcome_goal", values = c(0.8, 0.85, 0.9))
  )))
  expect_identical(out$plain, c("input", "run", "run", "run"))
})

test_that("progress messages are still shown for every run when quiet is FALSE", {
  args <- c(sweep_messages_args(), list(quiet = FALSE))
  # a sweep turns the confidence set off, so the single run does too
  single <- sweep_messages_collect(do.call(lago_optimization, c(
    args,
    list(include_confidence_set = FALSE)
  )))
  out <- sweep_messages_collect(do.call(lago_sensitivity, c(
    args,
    list(parameter = "outcome_goal", values = c(0.8, 0.85, 0.9))
  )))
  expect_gt(single$progress, 0)
  expect_identical(out$progress, 3 * single$progress)
  # with quiet = FALSE the cost note is shown too, once, after the center note
  expect_identical(out$plain, c(
    sweep_messages_expected[1],
    "When 'cost_list_of_vectors' is provided, 'default_cost_fxn_type' is ignored.",
    sweep_messages_expected[2]
  ))
})

test_that("a single lago_optimization() run still shows its messages", {
  out <- sweep_messages_collect(do.call(lago_optimization, c(
    sweep_messages_args(),
    list(quiet = TRUE)
  )))
  expect_identical(out$plain, sweep_messages_expected)
})

test_that("every input note from validate_inputs() is an input note", {
  base <- mtcars
  base$center <- rep_len(c("a", "b", "c"), nrow(base))
  base$period <- rep_len(c(1, 2), nrow(base))
  notes <- function(...) {
    found <- list()
    args <- list(
      data = base, outcome_name = "mpg", outcome_type = "continuous",
      intervention_components = c("gear", "qsec"),
      intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
      outcome_goal = 40, outcome_goal_intention = "maximize", power_goal = NULL,
      power_goal_approach = "unconditional", cost_list_of_vectors = list(c(0, 4), c(4, 6)),
      quiet = TRUE
    )
    # Replace each argument whole, since modifyList() would merge the cost lists.
    overrides <- list(...)
    args[names(overrides)] <- overrides
    withCallingHandlers(
      suppressWarnings(do.call(LAGOtrials:::validate_inputs, args)),
      message = function(m) {
        found[[length(found) + 1]] <<- m
        invokeRestart("muffleMessage")
      }
    )
    found
  }
  expect_note <- function(found, pattern) {
    hit <- Filter(function(m) grepl(pattern, conditionMessage(m), fixed = TRUE), found)
    expect_length(hit, 1)
    expect_s3_class(hit[[1]], "lago_input_note")
  }
  expect_note(notes(include_center_effects = TRUE), "'center' column is not a factor type")
  ordered_data <- base
  ordered_data$center <- factor(ordered_data$center, ordered = TRUE)
  ordered_data$period <- factor(ordered_data$period, ordered = TRUE)
  expect_note(notes(data = ordered_data, include_center_effects = TRUE), "'center' column is an ordered factor")
  expect_note(
    notes(include_time_effects = TRUE, time_effect_optimization_value = 1),
    "'period' column is not a factor type"
  )
  expect_note(
    notes(data = ordered_data, include_time_effects = TRUE, time_effect_optimization_value = 1),
    "'period' column is an ordered factor"
  )
  expect_note(
    notes(optimization_grid_search_step_size = c(1, 50)),
    "'optimization_method' has been switched to 'grid_search'"
  )
  expect_note(
    notes(
      intervention_components = c("gear", "qsec", "wt", "hp"),
      intervention_lower_bounds = c(0, 0, 0, 0), intervention_upper_bounds = c(10, 350, 10, 400),
      cost_list_of_vectors = list(c(0, 4), c(4, 6), c(0, 1), c(0, 1)),
      optimization_method = "grid_search", optimization_grid_search_step_size = c(5, 175, 5, 200)
    ),
    "There are more than 3 intervention components"
  )
  expect_note(notes(icc = 0.1), "icc is ignored because no power_goal was provided.")
  expect_note(
    notes(quiet = FALSE),
    "When 'cost_list_of_vectors' is provided, 'default_cost_fxn_type' is ignored."
  )
  expect_length(notes(), 0)
})

test_that("the cost note is a message, not printed output", {
  base <- mtcars
  printed <- utils::capture.output(invisible(suppressMessages(LAGOtrials:::validate_inputs(
    data = base, outcome_name = "mpg", outcome_type = "continuous",
    intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(10, 350),
    outcome_goal = 40, outcome_goal_intention = "maximize", power_goal = NULL,
    power_goal_approach = "unconditional", cost_list_of_vectors = list(c(0, 4), c(4, 6)),
    quiet = FALSE
  ))))
  expect_identical(printed, character(0))
})
