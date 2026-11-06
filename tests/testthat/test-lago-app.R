# lago_app(): the settings it builds, its checks, and its server. The app
# itself is never launched here.

app_bb <- function() as.data.frame(BB_data)

app_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

app_run_code <- function(code, data_name, data) {
  env <- new.env()
  assign(data_name, data, envir = env)
  app_quiet(utils::capture.output(eval(parse(text = code), envir = env)))
  env$result
}

test_that("lago_app() serves on the loopback interface only", {
  seen <- NULL
  local_mocked_bindings(
    runApp = function(appDir, port = NULL, launch.browser = FALSE, host = NULL, ...) {
      seen <<- list(host = host, app = appDir)
      NULL
    },
    .package = "shiny"
  )
  old <- options(shiny.host = "0.0.0.0")
  on.exit(options(old), add = TRUE)
  out <- lago_app(launch.browser = FALSE)
  expect_identical(seen$host, "127.0.0.1")
  expect_s3_class(seen$app, "shiny.appobj")
  expect_null(out)
})

test_that("lago_app() refuses data that is not a data frame", {
  expect_error(lago_app(1:3, launch.browser = FALSE), "must be a data.frame")
})

test_that("the bundled examples start on settings with no problems", {
  for (nm in c("BB_data", "mtcars")) {
    d <- if (nm == "BB_data") app_bb() else datasets::mtcars
    spec <- .app_defaults(d, nm)
    expect_equal(nrow(.app_problems(spec, d)), 0, info = nm)
    expect_length(spec$components, 2)
  }
  spec <- .app_defaults(app_bb(), "BB_data")
  expect_identical(spec$outcome, "pp3_oxytocin_mother")
  expect_identical(spec$outcome_type, "binary")
  expect_equal(spec$goal, 0.85)
})

test_that("a dataset with no preset gets a 0/1 outcome and two components", {
  d <- data.frame(a = c(1.5, 2, 3), y = c(0, 1, 1), b = 4:6, c = 7:9)
  spec <- .app_defaults(d, "my_data")
  expect_identical(spec$outcome, "y")
  expect_identical(spec$components, c("a", "b"))
  expect_equal(spec$lower, c(1.5, 4))
  expect_equal(spec$upper, c(3, 6))
})

test_that("the R code calls lago_optimization() with the same arguments as the run", {
  d <- app_bb()
  spec <- .app_defaults(d, "BB_data")
  spec$include_cs <- FALSE
  b <- .app_build(spec, d)
  expect_identical(b$args$intervention_components, c("coaching_updt", "launch_duration"))
  expect_false(b$args$include_confidence_set)
  parsed <- parse(text = b$code)
  call <- parsed[[length(parsed)]][[3]]
  expect_identical(as.character(call[[1]]), "lago_optimization")
  args_in_code <- lapply(as.list(call)[-1], eval)
  expect_equal(args_in_code[names(b$args)], b$args)
})

test_that("the R code reproduces the run", {
  skip_on_cran()
  d <- app_bb()
  spec <- .app_defaults(d, "BB_data")
  spec$include_cs <- FALSE
  b <- .app_build(spec, d)
  run <- .app_run(function() do.call(lago_optimization, c(list(data = b$data), b$args, list(quiet = TRUE))))
  expect_null(run$error)
  expect_equal(app_run_code(b$code, "BB_data", d)$rec_int, run$value$rec_int)
})

test_that("interaction, arm, center and period columns are built in the run and the code", {
  d <- data.frame(
    y = c(0, 1, 0, 1, 1, 0), a = 1:6, b = 6:1, arm = c("x", "t", "x", "t", "t", "x"),
    site = c("s1", "s1", "s2", "s2", "s3", "s3"), wave = c(1, 2, 1, 2, 1, 2)
  )
  spec <- .app_defaults(d, "d")
  spec$components <- c("a", "b")
  spec <- .app_set_component_defaults(spec, d)
  spec$interactions <- "a:b"
  spec$goal_mode <- "both"
  spec$arm_col <- "arm"
  spec$arm_value <- "t"
  spec$center_mode <- "effects"
  spec$center_col <- "site"
  spec$center_for <- "s2"
  spec$time_on <- TRUE
  spec$period_col <- "wave"
  spec$period_value <- 2
  b <- .app_build(spec, d)
  expect_equal(b$data[["a:b"]], d$a * d$b)
  expect_identical(b$data$group, ifelse(d$arm == "t", "treatment", "control"))
  expect_equal(b$data$center, factor(d$site))
  expect_equal(b$data$period, factor(d$wave))
  expect_identical(b$args$intervention_components, c("a", "b", "a:b"))
  expect_identical(b$args$main_components, c("a", "b"))
  expect_true(b$args$include_interaction_terms)
  expect_identical(b$args$center_effects_optimization_values, "s2")
  expect_equal(b$args$time_effect_optimization_value, 2)
  expect_equal(b$args$power_goal, 0.8)
  # the code builds the same columns from the loaded data
  env <- new.env()
  env$d <- d
  eval(parse(text = paste(b$code[2:(length(b$code) - 3)], collapse = "\n")), envir = env)
  expect_equal(env$data, b$data)
})

test_that("an existing group column is used as it is", {
  d <- data.frame(y = c(0, 1, 0, 1), a = 1:4, group = c("control", "treatment", "control", "treatment"))
  spec <- .app_defaults(d, "d")
  spec$goal_mode <- "power"
  spec$arm_col <- "group"
  spec$arm_value <- "treatment"
  b <- .app_build(spec, d)
  expect_length(b$prep, 0)
  expect_null(b$args[["outcome_goal"]])
})

test_that("the cost settings reach the arguments", {
  d <- datasets::mtcars
  spec <- .app_defaults(d, "mtcars")
  spec$cost_type <- "cubic"
  spec$doses <- c(4, NA)
  b <- .app_build(spec, d)
  expect_identical(b$args$default_cost_fxn_type, "cubic")
  expect_equal(b$args$unit_cost_doses, c(4, NA))
  spec$cost_mode <- "coef"
  spec$coefs <- list(c(0, 2), c(1, 0, 3))
  b <- .app_build(spec, d)
  expect_equal(b$args$cost_list_of_vectors, list(c(0, 2), c(1, 0, 3)))
  expect_null(b$args$unit_costs)
  expect_equal(.app_parse_coefs("0, 2; 0.5"), c(0, 2, 0.5))
  expect_null(.app_parse_coefs("0, two"))
})

test_that("each problem names the step and says what to change", {
  d <- app_bb()
  spec <- .app_defaults(d, "BB_data")
  check <- function(change, step, pattern) {
    s <- spec
    s[names(change)] <- change
    p <- .app_problems(s, d)
    expect_true(any(p$step == step & grepl(pattern, p$message)), info = pattern)
  }
  check(list(components = character(0), lower = numeric(0), upper = numeric(0)), "intervention", "at least one intervention component")
  check(list(lower = c(5, 0), upper = c(2, 3)), "intervention", "must not be below its lower bound")
  check(list(lower = c(-1, 0)), "intervention", "must be 0 or more")
  check(list(lower = c(NA, 0)), "intervention", "Enter both bounds")
  check(list(cost_type = "cubic", doses = c(10, NA)), "intervention", "for every component")
  check(list(cost_type = "cubic", doses = c(50, 1)), "intervention", "within its bounds")
  check(list(goal = NA_real_), "outcome", "Enter the outcome goal")
  check(list(outcome = ""), "outcome", "Choose the outcome column")
  check(list(goal_mode = "power", arm_col = "pre_post", arm_value = "1", power_goal = 1), "outcome", "at least 0.05")
  check(list(goal_mode = "power", arm_col = "pre_post", arm_value = "1", n_centers = 0), "outcome", "centers and patients")
  check(list(goal_mode = "power", arm_col = "pre_post", arm_value = "1", icc = c(0.1, 1.2)), "outcome", "each at least 0")
  check(list(goal_mode = "power", arm_col = "pre_post", arm_value = "9"), "outcome", "both treated and control")
  check(list(center_mode = "effects", center_col = "coaching_updt"), "centers", "cannot also be the outcome")
  check(list(center_mode = "chars", center_chars = character(0)), "centers", "at least one center characteristic")
  check(list(time_on = TRUE, period_col = "pilot_phase", period_value = ""), "centers", "Choose the period")
  check(list(goal = 1.2), "outcome", "between 0 and 1")
  check(list(outcome = "coaching_updt"), "outcome", "not 0/1")
  check(list(unit_costs = c(-1, 1)), "intervention", "unit cost of 0 or more")
  check(list(cost_mode = "coef", coefs = list(NULL, c(0, 1))), "intervention", "cost coefficients")
  check(list(goal_mode = "power", intention = "minimize", arm_col = "pre_post", arm_value = "1"), "outcome", "Maximize")
  check(list(goal_mode = "power", arm_col = ""), "outcome", "treatment arm")
  check(list(goal_mode = "power", arm_col = "pre_post", arm_value = "1", icc = 0.05, cluster_col = ""), "outcome", "ICC above 0")
  d1 <- transform(d, one = 1)
  p <- .app_problems(modifyList(spec, list(covariates = "one")), d1)
  expect_true(any(grepl("only one value", p$message)))
  check(list(center_mode = "effects", center_col = ""), "centers", "identifies the centers")
  check(list(center_mode = "chars", center_chars = "birth_volume_100", center_char_values = NA_real_), "centers", "value to optimize at")
  check(list(time_on = TRUE, period_col = ""), "centers", "time period")
  check(list(outcome_type = "continuous", goal_mode = "power"), "outcome", "only for a binary outcome")
  expect_identical(.app_problems(spec, NULL)$step, "data")
})

test_that("the confidence set grid size is checked before a run", {
  d <- datasets::mtcars
  spec <- .app_defaults(d, "mtcars")
  spec$components <- c("gear", "qsec", "wt", "hp")
  spec <- .app_set_component_defaults(spec, d)
  expect_gt(.app_cs_grid_size(spec$lower, spec$upper, .app_cs_steps(spec$lower, spec$upper)), 1e5)
  expect_true(any(grepl("confidence set grid", .app_problems(spec, d)$message)))
  spec$include_cs <- FALSE
  expect_false(any(grepl("confidence set grid", .app_problems(spec, d)$message)))
})

test_that("a run keeps its warnings, messages, error and printed output", {
  r <- .app_run(function() {
    cat("printed\n")
    message("\033[1mnote\033[22m")
    warning("careful")
    42
  })
  expect_equal(r$value, 42)
  expect_identical(r$warnings, "careful")
  expect_identical(r$messages, "note")
  expect_identical(r$output, "printed")
  r <- .app_run(function() stop("bad input"))
  expect_null(r$value)
  expect_identical(r$error, "bad input")
})

test_that("an uploaded file is read with the chosen separator and decimal mark", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("y;dose level", "1;2,5", "0;3,5"), path)
  d <- .app_read_csv(path, sep = ";", dec = ",")
  expect_identical(names(d), c("y", "dose.level"))
  expect_equal(d$dose.level, c(2.5, 3.5))
})

# Set the row inputs as the browser would, through the server's own col_id().
app_server_inputs <- function(session, col_id, comps, bounds, costs = rep(1, length(comps))) {
  vals <- list()
  for (k in seq_along(comps)) {
    vals[[col_id("lb", comps[k])]] <- bounds[[k]][1]
    vals[[col_id("ub", comps[k])]] <- bounds[[k]][2]
    vals[[col_id("cost", comps[k])]] <- costs[k]
  }
  do.call(session$setInputs, vals)
}

test_that("the server checks the steps and blocks a run until they are ready", {
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "BB_data", sep = ",", dec = ".")
    session$setInputs(
      outcome = "pp3_oxytocin_mother", outcome_type = "binary", goal_mode = "outcome", goal = 0.85,
      intention = "maximize", components = character(0), cost_mode = "unit", cost_type = "linear",
      center_mode = "none", include_cs = FALSE, approach = "unconditional"
    )
    expect_true(any(grepl("at least one intervention component", problems()$message)))
    expect_match(paste(code_lines(), collapse = "\n"), "Fix the steps")
    session$setInputs(components = c("coaching_updt", "launch_duration"))
    app_server_inputs(session, col_id, c("coaching_updt", "launch_duration"), list(c(0, 40), c(0, 3)))
    expect_equal(nrow(problems()), 0)
    expect_match(paste(code_lines(), collapse = "\n"), "lago_optimization\\(")
  })
})

test_that("the server runs, marks old results and returns the result on Done", {
  skip_on_cran()
  returned <- NULL
  local_mocked_bindings(stopApp = function(returnValue = invisible()) returned <<- returnValue, .package = "shiny")
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "BB_data", sep = ",", dec = ".")
    session$setInputs(
      outcome = "pp3_oxytocin_mother", outcome_type = "binary", goal_mode = "outcome", goal = 0.85,
      intention = "maximize", components = c("coaching_updt", "launch_duration"), cost_mode = "unit",
      cost_type = "linear", center_mode = "none", include_cs = FALSE, approach = "unconditional"
    )
    app_server_inputs(session, col_id, c("coaching_updt", "launch_duration"), list(c(0, 40), c(0, 3)))
    session$setInputs(run = 1)
    st <- run_state()
    expect_null(st$run$error)
    expect_length(st$run$value$rec_int, 2)
    expect_false(stale())
    expect_match(as.character(output$results$html), "Recommended intervention")
    session$setInputs(goal = 0.8)
    expect_true(stale())
    session$setInputs(done = 1)
  })
  expect_s3_class(returned$result, "lago")
  expect_identical(returned$code[3], "result <- lago_optimization(")
})

test_that("the server shows the package error and keeps the settings", {
  skip_on_cran()
  d <- transform(datasets::mtcars, k = 1)
  shiny::testServer(.build_lago_app(data = d, data_name = "d"), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    session$setInputs(
      outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 25,
      intention = "maximize", components = c("gear", "k"), cost_mode = "unit", cost_type = "linear",
      center_mode = "none", include_cs = FALSE, approach = "unconditional"
    )
    app_server_inputs(session, col_id, c("gear", "k"), list(c(3, 5), c(0, 2)))
    expect_equal(nrow(problems()), 0)
    session$setInputs(run = 1)
    expect_match(run_state()$run$error, "rank-deficient")
    html <- as.character(output$results$html)
    expect_match(html, "could not run with these settings")
    expect_match(html, "Your settings are kept")
    expect_identical(input$components, c("gear", "k"))
  })
})

test_that("the result says which goal it was optimized for", {
  expect_identical(.app_goal_source(list(outcome_goal = 0.8)), "")
  expect_match(.app_goal_source(list(power_goal = 0.8, effective_outcome_goal = 0.7)), "from the power goal\\)")
  expect_match(.app_goal_source(list(power_goal = 0.8, outcome_goal = 0.6, effective_outcome_goal = 0.7)), "needs more")
  expect_match(.app_goal_source(list(power_goal = 0.8, outcome_goal = 0.6, effective_outcome_goal = 0.6)), "needs no more")
})

test_that("the code reads an uploaded file with the same settings", {
  tail_args <- ", na.strings = c(\"\", \"NA\"), fileEncoding = \"UTF-8-BOM\")"
  expect_identical(.app_read_csv_code("a.csv"), paste0("read.csv(\"a.csv\"", tail_args))
  expect_identical(.app_read_csv_code("a.csv", ";", ","), paste0("read.csv(\"a.csv\", sep = \";\", dec = \",\"", tail_args))
})

test_that("a center with no complete row is caught before the run", {
  d <- data.frame(y = c(0, 1, 0, 1, 1, 0), a = c(1, 2, 3, 4, NA, NA), site = c("s1", "s1", "s2", "s2", "s3", "s3"))
  spec <- .app_defaults(d, "d")
  spec$components <- "a"
  spec <- .app_set_component_defaults(spec, d)
  spec$center_mode <- "effects"
  spec$center_col <- "site"
  spec$center_for <- "s3"
  expect_true(any(grepl("center s3 has no row", .app_problems(spec, d)$message)))
  spec$center_for <- ""
  expect_equal(nrow(.app_problems(spec, d)), 0)
})

test_that("default lower bounds are never below 0", {
  d <- transform(datasets::mtcars, qsec_c = qsec - mean(qsec))
  spec <- .app_defaults(d[, c("mpg", "gear", "qsec_c")], "d")
  expect_true(all(spec$lower >= 0))
  expect_equal(nrow(.app_problems(spec, d)), 0)
})

test_that("periods with number or text labels can be optimized for", {
  d <- data.frame(y = c(0, 1, 0, 1), a = 1:4, wave = factor(c("2", "10", "2", "10")), q = c("Q2", "Q1", "Q2", "Q1"))
  spec <- .app_defaults(d, "d")
  spec$components <- "a"
  spec <- .app_set_component_defaults(spec, d)
  spec$time_on <- TRUE
  spec$period_col <- "wave"
  spec$period_value <- "10"
  expect_identical(.app_periods(d$wave)$labels, c("2", "10"))
  b <- .app_build(spec, d)
  expect_equal(b$args$time_effect_optimization_value, 10)
  spec$period_col <- "q"
  spec$period_value <- "Q2"
  b <- .app_build(spec, d)
  expect_equal(b$args$time_effect_optimization_value, 2)
  expect_equal(b$data$period, factor(c(2, 1, 2, 1)))
  expect_true(any(grepl("Q1 = 1, Q2 = 2", .app_notes(spec, d))))
})

test_that("the app says when it replaces a column of the data", {
  d <- data.frame(y = c(0, 1, 0, 1), a = 1:4, center = c(1, 1, 2, 2), hosp = c("h1", "h1", "h2", "h2"))
  spec <- .app_defaults(d, "d")
  spec$center_mode <- "effects"
  spec$center_col <- "hosp"
  expect_true(any(grepl("already has a column named center", .app_notes(spec, d))))
  spec$center_col <- "center"
  expect_length(.app_notes(spec, d), 0)
})

test_that("the ICC box takes one or two values", {
  expect_equal(.app_parse_icc(""), numeric(0))
  expect_equal(.app_parse_icc("0.05"), 0.05)
  expect_equal(.app_parse_icc("0.04, 0.06"), c(0.04, 0.06))
  expect_true(is.na(.app_parse_icc("0.1, 0.2, 0.3")))
  d <- data.frame(y = rep(c(0, 1), 8), arm = rep(c(0, 1), each = 8), site = rep(c("a", "b", "c", "d"), each = 4), x = 1:16)
  expect_identical(.app_cluster_choices(d, "y", "arm", "1"), "site")
})

test_that("upload settings that do not match the file get a hint", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("y;dose level", "1;2,5", "0;3,5"), path)
  d <- .app_read_csv(path, sep = ",", dec = ".")
  expect_match(.app_csv_hint(d, ",", "."), "Semicolon")
  d <- .app_read_csv(path, sep = ";", dec = ".")
  expect_match(.app_csv_hint(d, ";", "."), "decimal comma")
  d <- .app_read_csv(path, sep = ";", dec = ",")
  expect_null(.app_csv_hint(d, ";", ","))
  expect_identical(.app_renamed(path, ";", d), "dose level (now dose.level)")
})

test_that("the code for an uploaded file reads blank cells as the app does", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("y,a,make", "1,2,x", "0,3,", "1,4,z"), path)
  in_app <- .app_read_csv(path)
  code <- .app_read_csv_code(path)
  expect_identical(eval(parse(text = code)), in_app)
  expect_true(is.na(in_app$make[2]))
})

test_that("a new dataset or a reset never reuses the bounds typed before", {
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "BB_data", sep = ",", dec = ".")
    session$setInputs(outcome = "pp3_oxytocin_mother", components = c("coaching_updt", "launch_duration"), cost_mode = "unit")
    app_server_inputs(session, col_id, c("coaching_updt", "launch_duration"), list(c(0, 5), c(0, 3)), costs = c(7, 1))
    expect_equal(spec()$upper, c(5, 3))
    old_ids <- c(col_id("ub", "coaching_updt"), col_id("cost", "coaching_updt"))
    session$setInputs(reset = 1)
    expect_false(col_id("ub", "coaching_updt") %in% old_ids)
    expect_true(all(is.na(spec()$upper)))
    session$setInputs(source = "mtcars")
    session$setInputs(outcome = "mpg", components = c("gear", "hp"))
    # launch_duration and hp are both column 4, but the ids differ by load
    expect_true(all(is.na(spec()$upper)))
  })
})

test_that("the summary sentence states the recommendation", {
  r <- list(rec_int = c(2, 0.5), rec_int_cost = 10, est_outcome_goal = 0.85, shrinking_method_used = FALSE)
  expect_match(.app_summary_sentence(r, c("a", "b")), "sets a at 2, b at 0.5. It costs 10")
  r$shrinking_method_used <- TRUE
  expect_match(.app_summary_sentence(r, c("a", "b")), "out of reach")
})

test_that("text periods are put in time order", {
  expect_identical(.app_periods(c("Q1", "Q10", "Q2"))$labels, c("Q1", "Q2", "Q10"))
  expect_identical(.app_periods(c("Week 10", "Week 2"))$labels, c("Week 2", "Week 10"))
  expect_identical(.app_periods(factor(c("late", "early"), levels = c("early", "late")))$labels, c("early", "late"))
  expect_identical(.app_periods(c("Spring", "Autumn"))$labels, c("Autumn", "Spring"))
})

test_that("a load or reset sets every choice and then lets the user change them", {
  d2 <- as.data.frame(BB_data)
  shiny::testServer(.build_lago_app(data = d2, data_name = "d2"), {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    # the defaults cycle has ended, so the next change is the user's
    expect_false(applying_defaults())
    session$setInputs(outcome = "pre_post", components = c("coaching_updt", "datafeedback"))
    session$setInputs(source = "BB_data")
    expect_false(applying_defaults())
    session$setInputs(reset = 1)
    expect_false(applying_defaults())
    # loading the same data twice does not block the next outcome change
    session$setInputs(source = "passed")
    session$setInputs(source = "passed")
    session$setInputs(outcome = "pp2_hand_hygiene")
    expect_false(applying_defaults())
    expect_identical(last_outcome(), "pp2_hand_hygiene")
  })
})

test_that("the default choices of a dataset include the treatment value and the period", {
  d <- data.frame(y = c(0, 1, 0, 1), a = 1:4, arm = c("c", "treatment", "c", "treatment"), period = c("Q2", "Q10", "Q1", "Q2"))
  spec <- .app_defaults(d, "d")
  ch <- .app_choices(d, spec)
  expect_identical(ch$arm, "arm")
  expect_identical(ch$arm_value, "treatment")
  expect_identical(ch$period_col, "period")
  expect_identical(ch$periods, c("Q1", "Q2", "Q10"))
  expect_identical(ch$period, "Q10")
  expect_false("a" %in% ch$covariates)
})

test_that("month names and year quarters are put in time order", {
  expect_identical(.app_periods(c("Mar", "Jan", "Dec", "Feb"))$labels, c("Jan", "Feb", "Mar", "Dec"))
  expect_identical(.app_periods(c("Q1 2021", "Q2 2020", "Q1 2020"))$labels, c("Q1 2020", "Q2 2020", "Q1 2021"))
})

test_that("a failed upload says so in every step", {
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "upload", sep = ",", dec = ".")
    upload_error("bad file")
    loaded(NULL)
    expect_match(problems()$message, "could not be read")
    expect_match(as.character(output$checklist$html), "Load the data first")
  })
})

test_that("the arguments carry center characteristics, covariates and every power setting", {
  d <- as.data.frame(BB_data)
  spec <- .app_defaults(d, "BB_data")
  spec$center_mode <- "chars"
  spec$center_chars <- "birth_volume_100"
  spec$center_char_values <- 1.6
  spec$covariates <- "staff_nurse"
  spec$goal_mode <- "both"
  spec$arm_col <- "pre_post"
  spec$arm_value <- "1"
  spec$approach <- "conditional"
  spec$icc <- c(0.04, 0.06)
  spec$cluster_col <- "site_name"
  b <- .app_build(spec, d)
  expect_identical(b$args$center_characteristics, "birth_volume_100")
  expect_equal(b$args$center_characteristics_optimization_values, 1.6)
  expect_identical(b$args$additional_covariates, "staff_nurse")
  expect_identical(b$args$power_goal_approach, "conditional")
  expect_equal(b$args$icc, c(0.04, 0.06))
  expect_identical(b$args$power_goal_cluster_id, "site_name")
  expect_equal(nrow(.app_problems(spec, d)), 0)
})

test_that("the remaining checks and notes say what to change", {
  expect_true(is.na(.app_default_goal(data.frame(y = c(NA_real_, NA_real_)), "y", "continuous")))
  d1 <- data.frame(y = c(0, 1), txt = c("a", "b"))
  expect_true(any(grepl("at least two numeric columns", .app_problems(.app_defaults(d1, "d1"), d1)$message)))
  d <- data.frame(y = c(0, 1, 0, 1), a = 1:4, site = c("s1", "s1", "s1", "s1"), arm = c("t", NA, "c", "t"),
    group = c("x", "x", "x", "x"))
  spec <- .app_defaults(d, "d")
  spec$center_mode <- "effects"
  spec$center_col <- "site"
  expect_true(any(grepl("at least two centers", .app_problems(spec, d)$message)))
  spec$center_mode <- "none"
  spec$goal_mode <- "power"
  spec$arm_col <- "arm"
  spec$arm_value <- "t"
  expect_true(any(grepl("has missing values", .app_problems(spec, d)$message)))
  expect_true(any(grepl("named group", .app_notes(spec, d))))
  one <- data.frame(abc = 1:3)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("a,b", "1,2"), path)
  expect_length(.app_renamed(path, ",", one), 0)
  expect_length(.app_renamed(path, ";", data.frame(a = 1, b = 2, c = 3)), 0)
})

app_upload <- function(session, path, name = basename(path)) {
  session$setInputs(file = data.frame(name = name, size = file.size(path), type = "text/csv", datapath = path, stringsAsFactors = FALSE))
}

test_that("the server reads an upload, lists renamed columns and reports a file it cannot read", {
  good <- tempfile(fileext = ".csv")
  bad <- tempfile(fileext = ".csv")
  on.exit(unlink(c(good, bad)), add = TRUE)
  writeLines(c("y,dose level,visits", "1,2.5,3", "0,1,1", "1,3,4", "0,0.5,2"), good)
  writeLines(c("a,b", "1,2,3,4"), bad)
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "upload", sep = ",", dec = ".")
    app_upload(session, good, "trial.csv")
    expect_identical(names(current_data()), c("y", "dose.level", "visits"))
    expect_match(loaded()$name, "read.csv\\(\"trial.csv\"")
    html <- as.character(output$data_info$html)
    expect_match(html, "dose level \\(now dose.level\\)")
    app_upload(session, bad, "bad.csv")
    expect_null(loaded())
    expect_match(as.character(output$data_info$html), "could not be read with these settings")
  })
})

test_that("the server fills the choices that follow a column the user picks", {
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "BB_data", sep = ",", dec = ".")
    session$setInputs(outcome = "pp3_oxytocin_mother", components = c("coaching_updt", "launch_duration"))
    session$setInputs(goal_mode = "power", arm_col = "pre_post", arm_value = "1")
    session$setInputs(center_mode = "effects", center_col = "site_name", center_for = "Auras")
    session$setInputs(time_on = TRUE, period_col = "pilot_phase", period_value = "2")
    session$setInputs(outcome = "pp2_hand_hygiene")
    expect_false(applying_defaults())
    expect_identical(spec()$center_col, "site_name")
    session$setInputs(cost_mode = "coef", center_mode = "chars", center_chars = "birth_volume_100")
    rows <- as.character(output$component_rows$html)
    expect_match(rows, "Cost coefficients")
    expect_match(as.character(output$center_char_values$html), "Optimize at birth_volume_100")
    expect_false(stale())
  })
})

test_that("the results show the shrinking method and its actions", {
  skip_on_cran()
  d <- as.data.frame(BB_data)
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "BB_data", sep = ",", dec = ".")
    session$setInputs(
      outcome = "pp3_oxytocin_mother", outcome_type = "binary", goal_mode = "outcome", goal = 0.99,
      intention = "maximize", components = c("coaching_updt", "launch_duration"), cost_mode = "unit",
      cost_type = "linear", center_mode = "none", include_cs = TRUE, approach = "unconditional"
    )
    app_server_inputs(session, col_id, c("coaching_updt", "launch_duration"), list(c(0, 5), c(0, 1)))
    session$setInputs(run = 1)
    r <- run_state()$run$value
    expect_true(r$shrinking_method_used)
    html <- as.character(output$results$html)
    expect_match(html, "Widen the bounds")
    expect_match(as.character(output$cs_ui$html), "shrinking method was used")
    session$setInputs(fix_goal = 1)
    session$setInputs(fix_bounds = 1)
  })
})

test_that("the results show the confidence set, its table and the intervention test", {
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "BB_data", sep = ",", dec = ".")
    session$setInputs(
      outcome = "pp3_oxytocin_mother", outcome_type = "binary", goal_mode = "both", goal = 0.85,
      intention = "maximize", components = c("coaching_updt", "launch_duration"), cost_mode = "unit",
      cost_type = "linear", center_mode = "none", include_cs = TRUE, approach = "unconditional",
      arm_col = "pre_post", arm_value = "1", power_goal = 0.8, n_centers = 10, n_patients = 30
    )
    app_server_inputs(session, col_id, c("coaching_updt", "launch_duration"), list(c(0, 40), c(0, 3)))
    session$setInputs(run = 1)
    r <- run_state()$run$value
    expect_false(is.null(r$cs))
    expect_match(as.character(output$cs_ui$html), "of the grid is in the confidence set")
    expect_type(output$cs_plot, "list")
    expect_match(as.character(output$test_ui$html), "effect of the intervention as a whole")
    expect_match(output$cs_table, "coaching_updt")
    # three components list the set as a table
    session$setInputs(components = c("coaching_updt", "launch_duration", "staff_nurse"))
    app_server_inputs(session, col_id, c("coaching_updt", "launch_duration", "staff_nurse"), list(c(0, 40), c(0, 3), c(0, 10)))
    session$setInputs(run = 2)
    expect_match(as.character(output$cs_ui$html), "listed as a table")
  })
})
