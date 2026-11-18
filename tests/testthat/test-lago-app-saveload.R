# lago_app(): saving and loading settings, and starting from a lago result.
# The app itself is never launched here.

saveload_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# a lago result with no confidence set, from lago_optimization() arguments
saveload_fit <- function(...) {
  saveload_quiet(lago_optimization(..., include_confidence_set = FALSE, quiet = TRUE))
}

saveload_args <- function(data, ...) {
  a <- formals(lago_optimization)
  a <- lapply(a[setdiff(names(a), c("data", "..."))], function(x) if (is.symbol(x) && !nzchar(as.character(x))) NULL else eval(x))
  utils::modifyList(a, c(list(data = data), list(...)), keep.null = TRUE)
}

test_that("a call becomes the spec of the app, with what it cannot show", {
  d <- datasets::mtcars
  a <- saveload_args(d,
    outcome_name = "mpg", outcome_type = "continuous", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 20), unit_costs = c(2, 1),
    unit_cost_doses = c(4, 15), outcome_goal = 25, outcome_goal_intention = "minimize"
  )
  s <- .app_spec_from_args(a)
  expect_length(s$left, 0)
  expect_identical(s$spec$cost_type, "cubic")
  expect_equal(s$spec$doses, c(4, 15))
  expect_identical(s$spec$intention, "minimize")
  expect_identical(s$spec$goal_mode, "outcome")

  a2 <- utils::modifyList(a, list(
    cost_list_of_vectors = list(c(0, 2), c(1, 1)), glm_family = "quasibinomial", link = "identity",
    weights = d$carb, optimization_method = "grid_search", optimization_grid_search_step_size = c(0.5, 1),
    confidence_set_alpha = 0.01, confidence_set_grid_step_size = c(1, 2), prev_recommended_interventions = c(4, 15),
    additional_covariates = "wt", center_characteristics = "hp", center_characteristics_optimization_values = 100,
    include_confidence_set = TRUE
  ))
  s2 <- .app_spec_from_args(a2)$spec
  expect_identical(s2$cost_mode, "coef")
  expect_identical(s2$coefs, list(c(0, 2), c(1, 1)))
  expect_identical(c(s2$glm_family, s2$link, s2$weights_col, s2$method), c("quasibinomial", "identity", "carb", "grid_search"))
  expect_equal(c(s2$grid_steps, s2$cs_level, s2$cs_steps, s2$prev), c(0.5, 1, 0.99, 1, 2, 4, 15))
  expect_true(s2$prev_on)
  expect_identical(c(s2$center_mode, s2$center_chars, s2$covariates), c("chars", "hp", "wt"))
  expect_equal(s2$center_char_values, 100)

  # what the app cannot show is named
  a3 <- utils::modifyList(a, list(weights = seq_len(32) / 7, confidence_set_alpha = 0.2))
  left <- .app_spec_from_args(a3)$left
  expect_match(left[1], "the weights, which are not a column of the data")
  expect_match(left[2], "the confidence level of 80%")
})

test_that("interaction terms, center effects, time effects and a power goal come back", {
  d <- as.data.frame(BB_data)
  a <- saveload_args(d,
    outcome_name = "pp3_oxytocin_mother", outcome_type = "binary",
    intervention_components = c("coaching_updt", "launch_duration", "launch_duration:coaching_updt", "a:b:c"),
    main_components = c("coaching_updt", "launch_duration"), include_interaction_terms = TRUE,
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(40, 3), unit_costs = c(1, 1),
    outcome_goal = NULL, power_goal = 0.8, num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 30,
    icc = c(0.04, 0.06), power_goal_cluster_id = "site_name", include_center_effects = TRUE,
    center_effects_optimization_values = "A", include_time_effects = TRUE, time_effect_optimization_value = 3
  )
  x <- .app_spec_from_args(a)
  s <- x$spec
  # a reversed pair is put in the order the app lists it
  expect_identical(s$interactions, "coaching_updt:launch_duration")
  expect_match(x$left, "the interaction term a:b:c, which is not the product of two components")
  expect_identical(c(s$goal_mode, s$arm_col, s$arm_value, s$cluster_col), c("power", "group", "treatment", "site_name"))
  expect_equal(s$icc, c(0.04, 0.06))
  expect_identical(c(s$center_mode, s$center_col, s$center_for), c("effects", "center", "A"))
  expect_identical(c(s$period_col, s$period_value), c("period", "3"))
  a$center_effects_optimization_values <- NULL
  a$center_weights_for_outcome_goal <- c(0.5, 0.5)
  s <- .app_spec_from_args(a)$spec
  expect_true(s$cw_on)
  expect_equal(s$cw, c(0.5, 0.5))
  # center level data has center effects unless the call turned them off
  p <- as.data.frame(main_pulesa_data)
  cl <- .app_spec_from_args(saveload_args(p, input_data_structure = "center_level", outcome_name = "proportion", outcome_type = "binary", intervention_components = "DeliveryA", intervention_lower_bounds = 0, intervention_upper_bounds = 1, unit_costs = 1, outcome_goal = 0.6))$spec
  expect_identical(c(cl$structure, cl$cl_kind, cl$cl_value, cl$cl_total, cl$center_mode), c("center", "proportion", "proportion", "center_sample_size", "effects"))
})

test_that("a result started in the app runs to the same recommendation", {
  skip_on_cran()
  d <- transform(datasets::mtcars, `gear:qsec` = gear * qsec, check.names = FALSE)
  r <- saveload_fit(
    data = d, outcome_name = "mpg", outcome_type = "continuous", intervention_components = c("gear", "qsec", "gear:qsec"),
    main_components = c("gear", "qsec"), include_interaction_terms = TRUE, intervention_lower_bounds = c(3, 14),
    intervention_upper_bounds = c(5, 20), unit_costs = c(2, 1), outcome_goal = 25, outcome_goal_intention = "maximize",
    optimization_method = "grid_search", optimization_grid_search_step_size = c(0.5, 1), weights = d$carb
  )
  a <- attr(r, "lago_call_args")
  s <- .app_spec_from_args(a)$spec
  s$data_name <- "d"
  expect_equal(nrow(.app_problems(s, a$data)), 0)
  b <- .app_build(s, a$data)
  again <- saveload_quiet(do.call(lago_optimization, c(list(data = b$data), b$args, list(quiet = TRUE))))
  expect_equal(again$rec_int, r$rec_int, tolerance = 1e-5)
})

test_that("lago_app() takes a lago result and refuses one without its data", {
  r <- saveload_fit(
    data = datasets::mtcars, outcome_name = "mpg", outcome_type = "continuous", intervention_components = c("gear", "qsec"),
    intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 20), unit_costs = c(2, 1),
    outcome_goal = 25, outcome_goal_intention = "maximize"
  )
  local_mocked_bindings(.lago_run_app = function(app, ...) app)
  app <- lago_app(r, launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
  shiny::testServer(app, {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    expect_identical(loaded()$name, "attr(r, \"lago_call_args\")$data")
    expect_identical(start_spec()$cost_type, "cubic")
    expect_match(as.character(output$data_info$html), "Every setting of it is shown")
    # the inputs start on the call's values
    session$setInputs(components = c("gear", "qsec"))
    expect_match(as.character(output$component_rows$html), "value=\"14\"")
    session$setInputs(reset = 1)
    expect_identical(start_spec()$cost_type, "cubic")
  })
  bare <- r
  attr(bare, "lago_call_args") <- NULL
  expect_error(lago_app(bare), "does not carry the data of its call")
  expect_error(lago_app(1:3), "must be a data.frame or a `lago` result")
})

test_that("a result whose call the app cannot show in full says what is left out", {
  d <- as.data.frame(BB_data)
  start <- .app_spec_from_args(saveload_args(d,
    outcome_name = "pp3_oxytocin_mother", outcome_type = "binary", intervention_components = "coaching_updt",
    intervention_lower_bounds = 0, intervention_upper_bounds = 40, unit_costs = 1, outcome_goal = 0.85,
    confidence_set_alpha = 0.2
  ))
  shiny::testServer(.build_lago_app(data = d, data_name = "d", start = start), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    html <- as.character(output$data_info$html)
    expect_match(html, "The app cannot show these settings of it, so they are left out")
    expect_match(html, "the confidence level of 80%")
  })
})

test_that("settings are saved and loaded back, and refused when they do not fit", {
  said <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) said <<- c(said, as.character(ui)), .package = "shiny")
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "mtcars", sep = ",", dec = ".")
    session$setInputs(
      outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 27, intention = "maximize",
      components = c("gear", "qsec"), cost_mode = "unit", cost_type = "linear", center_mode = "none",
      include_cs = FALSE, approach = "unconditional", method = "grid_search"
    )
    do.call(session$setInputs, stats::setNames(list(3, 5, 4, 14, 20, 1), c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))))
    saved <- readRDS(output$save_settings)
    expect_identical(saved$lago_app_settings, 1L)
    expect_identical(saved$data_label, "the mtcars example")
    expect_equal(saved$spec$goal, 27)
    f <- tempfile(fileext = ".rds")
    saveRDS(saved, f)
    session$setInputs(goal = 20, load_settings = list(datapath = f, name = "s.rds"))
    s <- start_spec()
    expect_equal(s$goal, 27)
    expect_identical(s$method, "grid_search")
    expect_equal(s$unit_costs, c(4, 1))
    expect_match(said[length(said)], "The settings are loaded")

    # a file that is not a settings file
    other <- tempfile(fileext = ".rds")
    saveRDS(list(a = 1), other)
    session$setInputs(load_settings = list(datapath = other, name = "x.rds"))
    expect_match(said[length(said)], "not a settings file of this app")
    # settings for columns this data does not have
    saved$spec$components <- c("gear", "coaching_updt")
    saveRDS(saved, f)
    session$setInputs(load_settings = list(datapath = f, name = "s2.rds"))
    expect_match(said[length(said)], "does not have: coaching_updt. Load the mtcars example first, then load the settings again.", fixed = TRUE)
    # no data yet
    session$setInputs(source = "upload")
    session$setInputs(load_settings = list(datapath = f, name = "s3.rds"))
    expect_match(said[length(said)], "Load the data first, then the settings")
  })
})

test_that("the columns that settings need are listed", {
  s <- utils::modifyList(.app_empty_spec(), list(
    outcome = "y", components = c("a", "b"), covariates = "w", center_mode = "effects", center_col = "site",
    time_on = TRUE, period_col = "p", weights_col = "n"
  ))
  expect_identical(.app_spec_columns(s), c("y", "a", "b", "w", "site", "p", "n"))
  s <- utils::modifyList(s, list(structure = "center", cl_value = "s", cl_total = "t", center_mode = "chars", center_chars = "k", time_on = FALSE, weights_col = ""))
  expect_identical(.app_spec_columns(s), c("s", "t", "a", "b", "w", "k"))
  s <- utils::modifyList(.app_empty_spec(), list(outcome = "y", outcome_type = "binary", goal_mode = "power", arm_col = "g", cluster_col = "c"))
  expect_identical(.app_spec_columns(s), c("y", "g", "c"))
})

test_that("the treatment arm comes from the settings, else from the usual names", {
  d <- data.frame(y = c(0, 1, 0, 1), x = 1:4, arm = c("a", "b", "a", "b"), g = c(0, 1, 0, 1), k = c("p", "q", "p", "q"))
  spec <- function(...) utils::modifyList(.app_defaults(d, "d"), list(...))
  ch <- .app_choices(d, spec(arm_col = "arm", arm_value = "a"))
  expect_identical(c(ch$arm, ch$arm_value), c("arm", "a"))
  # a value the column does not have falls back to 1, then to the last value
  expect_identical(.app_choices(d, spec(arm_col = "g", arm_value = "z"))$arm_value, "1")
  expect_identical(.app_choices(d, spec(arm_col = "k", arm_value = "z"))$arm_value, "q")
  d$group <- c("control", "treatment", "control", "treatment")
  expect_identical(.app_choices(d, spec(arm_col = "none"))$arm, "group")
})

test_that("a power goal with an outcome goal comes back as both goals", {
  a <- saveload_args(as.data.frame(BB_data),
    outcome_name = "pp3_oxytocin_mother", outcome_type = "binary", intervention_components = "coaching_updt",
    intervention_lower_bounds = 0, intervention_upper_bounds = 40, unit_costs = 1, outcome_goal = 0.85,
    power_goal = 0.8, num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 30
  )
  expect_identical(.app_spec_from_args(a)$spec$goal_mode, "both")
})

test_that("loaded settings start every input they hold, and typed values are kept when inputs are drawn again", {
  d <- as.data.frame(main_pulesa_data)
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "main_pulesa_data", sep = ",", dec = ".")
    s <- utils::modifyList(.app_defaults(d, "main_pulesa_data"), list(
      cost_type = "cubic", doses = c(1, rep(NA, 6)), cw_on = TRUE, cw = rep(1 / 16, 16),
      center_mode = "chars", center_chars = "MI_DeliveryB", center_char_values = 1
    ))
    f <- tempfile(fileext = ".rds")
    # a file saved before the data label was kept
    saveRDS(list(lago_app_settings = 1L, data_name = "main_pulesa_data", spec = s), f)
    session$setInputs(load_settings = list(datapath = f, name = "s.rds"))
    session$setInputs(components = s$components, cost_mode = "unit", cost_type = "cubic", center_mode = "chars", center_chars = "MI_DeliveryB")
    expect_match(as.character(output$component_rows$html), "Dose where the unit cost was measured")
    expect_match(as.character(output$center_char_values$html), "value=\"1\"")
    # center weights from the file, for the same centers
    session$setInputs(center_mode = "effects", center_col = "Clinic", center_for = "", cw_on = TRUE, structure = "center", cl_total = "Total_visit")
    expect_match(as.character(output$cw_ui$html), "value=\"0.0625\"")
    # a value typed in is kept when the inputs are drawn again
    do.call(session$setInputs, stats::setNames(list(2, 3), c(col_id("cc", "MI_DeliveryB"), col_id("gstep", s$components[1]))))
    session$setInputs(center_chars = c("MI_DeliveryB", "MI_RemoteMonitoring"), components = s$components[1:2])
    expect_match(as.character(output$center_char_values$html), "value=\"2\"")
    expect_match(as.character(output$grid_steps_ui$html), "value=\"3\"")
  })
})

test_that("a settings file without a data label names no data in its message", {
  said <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) said <<- c(said, as.character(ui)), .package = "shiny")
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "mtcars", sep = ",", dec = ".")
    f <- tempfile(fileext = ".rds")
    saveRDS(list(lago_app_settings = 1L, spec = utils::modifyList(.app_empty_spec(), list(outcome = "mpg", components = "missing_column"))), f)
    session$setInputs(load_settings = list(datapath = f, name = "s.rds"))
  })
  expect_match(said[length(said)], "Load the data they were saved with first, then load the settings again.", fixed = TRUE)
})

test_that("the interactions of a result or of loaded settings are checked once their components show", {
  d <- transform(datasets::mtcars, `gear:qsec` = gear * qsec, check.names = FALSE)
  start <- .app_spec_from_args(saveload_args(d,
    outcome_name = "mpg", outcome_type = "continuous", intervention_components = c("gear", "qsec", "gear:qsec"),
    main_components = c("gear", "qsec"), include_interaction_terms = TRUE, intervention_lower_bounds = c(3, 14),
    intervention_upper_bounds = c(5, 20), unit_costs = c(2, 1), outcome_goal = 25
  ))
  shiny::testServer(.build_lago_app(data = d, data_name = "d", start = start), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    # before the components arrive, there is nothing to check
    expect_match(as.character(output$interaction_ui$html), "Choose at least two components")
    session$setInputs(components = c("gear", "qsec"))
    expect_match(as.character(output$interaction_ui$html), "value=\"gear:qsec\" checked")
    # later the user's own choice holds
    session$setInputs(interactions = character(0), components = c("qsec", "gear"))
    expect_false(grepl("checked", as.character(output$interaction_ui$html)))
  })
})

test_that("loading names the values that do not fit the data, and the kind of a wrong file", {
  said <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) said <<- c(said, paste(as.character(ui), collapse = " ")), .package = "shiny")
  d <- as.data.frame(main_pulesa_data)
  s <- utils::modifyList(.app_defaults(d, "main_pulesa_data"), list(center_for = "Nowhere", period_value = "99"))
  expect_match(.app_spec_misfits(s, d)[1], "the center Nowhere to optimize for, which this data does not have")
  expect_match(.app_spec_misfits(s, d)[2], "the period 99, which this data does not have, so the last period is used")
  s2 <- utils::modifyList(.app_defaults(d, "main_pulesa_data"), list(cw_on = TRUE, cw = c(0.5, 0.5)))
  expect_match(.app_spec_misfits(s2, d), "for 2 centers while this data has 16")
  bb <- as.data.frame(BB_data)
  s3 <- utils::modifyList(.app_defaults(bb, "BB_data"), list(goal_mode = "power", arm_col = "pre_post", arm_value = "7"))
  expect_match(.app_spec_misfits(s3, bb), "the treatment value 7, which the column pre_post does not have")
  expect_length(.app_spec_misfits(.app_defaults(d, "main_pulesa_data"), d), 0)
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "main_pulesa_data", sep = ",", dec = ".")
    f <- tempfile(fileext = ".rds")
    saveRDS(list(lago_app_settings = 1L, spec = s), f)
    session$setInputs(load_settings = list(datapath = f, name = "a.rds"))
    expect_match(said[length(said)], "do not fit this data and were changed")
    expect_identical(start_spec()$center_for, "")
    saveRDS(list(lago_app_settings = 2L, spec = s), f)
    session$setInputs(load_settings = list(datapath = f, name = "b.rds"))
    expect_match(said[length(said)], "saved by a newer version of LAGOtrials")
    saveRDS(structure(list(rec_int = 1), class = "lago"), f)
    session$setInputs(load_settings = list(datapath = f, name = "c.rds"))
    expect_match(said[length(said)], "holds a result of lago_optimization\\(\\), not settings")
    # saved from an upload, the message names the file
    saved <- readRDS(output$save_settings)
    expect_identical(saved$data_label, "the main_pulesa_data example")
  })
})

test_that("weights from loaded settings say where they start", {
  d <- as.data.frame(main_pulesa_data)
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "main_pulesa_data", sep = ",", dec = ".")
    s <- utils::modifyList(.app_defaults(d, "main_pulesa_data"), list(cw_on = TRUE, cw = rep(1 / 16, 16)))
    f <- tempfile(fileext = ".rds")
    saveRDS(list(lago_app_settings = 1L, spec = s), f)
    session$setInputs(load_settings = list(datapath = f, name = "a.rds"))
    session$setInputs(center_mode = "effects", center_col = "Clinic", center_for = "", cw_on = TRUE, structure = "center", cl_total = "Total_visit")
    expect_match(as.character(output$cw_ui$html), "Each center starts at its weight in the settings it was opened with")
  })
})

test_that("saving names an upload or a result by how to get the data back", {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(datasets::mtcars, f, row.names = FALSE)
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "upload", sep = ",", dec = ".", file = data.frame(name = "trial.csv", size = 1, type = "text/csv", datapath = f))
    expect_identical(readRDS(output$save_settings)$data_label, "the file trial.csv")
  })
  start <- .app_spec_from_args(saveload_args(datasets::mtcars, outcome_name = "mpg", outcome_type = "continuous", intervention_components = "gear", intervention_lower_bounds = 3, intervention_upper_bounds = 5, unit_costs = 1, outcome_goal = 25))
  shiny::testServer(.build_lago_app(data = datasets::mtcars, data_name = "r", start = start), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    expect_match(readRDS(output$save_settings)$data_label, "the data of the result the app was started from")
  })
  shiny::testServer(.build_lago_app(data = datasets::mtcars, data_name = "cars"), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    expect_identical(readRDS(output$save_settings)$data_label, "your data cars")
  })
})

test_that("Done returns no result from before a reset or a new dataset, but a new page keeps it", {
  skip_on_cran()
  returned <- list()
  local_mocked_bindings(stopApp = function(returnValue = invisible()) returned[[length(returned) + 1]] <<- returnValue, .package = "shiny")
  app <- .build_lago_app()
  setup <- function(session, col_id) {
    session$setInputs(
      outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 25, intention = "maximize",
      components = c("gear", "qsec"), cost_mode = "unit", cost_type = "linear", center_mode = "none",
      include_cs = FALSE, approach = "unconditional"
    )
    do.call(session$setInputs, stats::setNames(list(3, 5, 2, 14, 20, 1), c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))))
  }
  shiny::testServer(app, {
    session$setInputs(source = "mtcars", sep = ",", dec = ".")
    setup(session, col_id)
    session$setInputs(run = 1)
    session$setInputs(done = 1)
    session$setInputs(reset = 1)
    session$setInputs(done = 2)
    setup(session, col_id)
    session$setInputs(run = 2)
    session$setInputs(source = "BB_data")
    session$setInputs(done = 3)
    session$setInputs(source = "mtcars")
    setup(session, col_id)
    session$setInputs(run = 3)
  })
  expect_s3_class(returned[[1]]$result, "lago")
  expect_null(returned[[2]]$result)
  expect_null(returned[[3]]$result)
  # a new browser session, as after a page refresh, still returns the last run
  shiny::testServer(app, {
    session$setInputs(source = "mtcars", sep = ",", dec = ".")
    session$setInputs(done = 1)
  })
  expect_s3_class(returned[[4]]$result, "lago")
})
