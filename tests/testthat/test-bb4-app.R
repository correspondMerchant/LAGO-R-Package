# Fixes to lago_app() from the fourth bug bash.

# Run f with LC_CTYPE set to the C locale or a UTF-8 one, and set it back.
bb4_in_locale <- function(kind, f) {
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  tries <- if (identical(kind, "C")) "C" else c("C.UTF-8", "en_US.UTF-8", "UTF-8")
  set <- ""
  for (loc in tries) {
    set <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
    if (nzchar(set)) break
  }
  if (!nzchar(set)) skip(sprintf("no %s locale", kind))
  f()
}
bb4_file <- function(...) {
  path <- tempfile(fileext = ".csv")
  writeBin(c(...), path)
  path
}
bb4_text <- function(s) bb4_file(charToRaw(enc2utf8(s)))
bb4_codes <- function(x) vapply(x, function(s) paste(utf8ToInt(s), collapse = " "), character(1), USE.NAMES = FALSE)
bb4_quiet <- function(expr) suppressWarnings(suppressMessages(expr))
# read a file as the app does and with the code it shows, in a locale
bb4_read <- function(path, kind, sep = ",", dec = ".") {
  bb4_in_locale(kind, function() {
    enc <- .app_file_encoding(path)
    d <- .app_read_csv(path, sep, dec, enc)
    code <- .app_read_csv_code(path, sep, dec, enc, .app_csv_header(path, sep, enc))
    list(d = d, code = code, from_code = eval(parse(text = code)), renamed = .app_renamed(path, sep, d, enc))
  })
}
bb4_mtcars <- function(session, col_id, goal = 25, intention = "maximize", ub = 20) {
  session$setInputs(source = "mtcars", sep = ",", dec = ".")
  session$setInputs(
    outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = goal,
    intention = intention, components = c("gear", "qsec"), cost_mode = "unit",
    cost_type = "linear", center_mode = "none", include_cs = FALSE, approach = "unconditional"
  )
  do.call(session$setInputs, stats::setNames(
    list(3, 5, 2, 14, ub, 1),
    c(col_id("lb", "gear"), col_id("ub", "gear"), col_id("cost", "gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), col_id("cost", "qsec"))
  ))
}

test_that("column names get the same suffixes as make.names(unique = TRUE)", {
  cases <- list(c("a b", "a.b"), c("x.1", "x", "x"), c("if", "if."), c("a", "a", "", "a b", "a.b"))
  for (x in cases) {
    for (kind in c("C", "UTF-8")) {
      expect_identical(bb4_in_locale(kind, function() .app_make_names(x)), make.names(x, unique = TRUE))
    }
  }
  # the shown code reads the same column as the app
  path <- bb4_text("y,dose x,dose.x\n1,10,1\n0,20,2\n1,30,3\n")
  for (kind in c("C", "UTF-8")) {
    got <- bb4_read(path, kind)
    expect_identical(names(got$d), c("y", "dose.x.1", "dose.x"))
    expect_identical(got$d$dose.x, 1:3)
    expect_identical(got$from_code, got$d)
    expect_identical(got$renamed, "dose x (now dose.x.1)")
  }
})

test_that("names that are not ASCII match make.names() of a UTF-8 locale, and the app turns them into ASCII", {
  x <- c(
    "\u00aa", "\u00b2x", "x\u00b2", "\u2163", "\u00bd", "\u00e1", "\u03c0r", "_\u00e9", "\u00e9_", ".1\u00e9",
    "TRUE", "NA\u00e9", "a \u00e9", "a.\u00e9"
  )
  want <- bb4_in_locale("UTF-8", function() bb4_codes(make.names(x, unique = TRUE)))
  for (kind in c("C", "UTF-8")) {
    expect_identical(bb4_in_locale(kind, function() bb4_codes(.app_make_names(x))), want)
  }
  # with no UTF-8 locale on the system, the names are made from ASCII
  bb4_in_locale("C", function() {
    expect_identical(.app_make_names(c("\u00e9t\u00e9", "a"), utf8_locales = "no_such_locale"), c("ete", "a"))
  })
  expect_identical(
    .app_ascii_fold(c("\u00c6sop \u00df \u0152uvre \u0141\u00f3d\u017a \u00fe", "\u03c0r")),
    c("AEsop ss OEuvre Lodz th", ".r")
  )
  expect_identical(.app_csv_names(c("\u00e9t\u00e9", "ete", "a")), c("ete.1", "ete", "a"))

  # an uploaded file with such names runs in any locale, and its code reads it
  set.seed(2)
  n <- 120
  body <- paste(sprintf("%d,%.2f,%.2f,%.2f", rbinom(n, 1, 0.5), runif(n, 0, 5), runif(n, 0, 5), runif(n, 0, 5)), collapse = "\n")
  path <- bb4_text(paste0("y,a,area_m\u00b2,\u00e9t\u00e9\n", body, "\n"))
  for (kind in c("C", "UTF-8")) {
    got <- bb4_read(path, kind)
    expect_identical(names(got$d), c("y", "a", "area_m.", "ete"))
    expect_identical(got$from_code, got$d)
    expect_length(got$renamed, 2)
    bb4_in_locale(kind, function() {
      s <- modifyList(.app_defaults(got$d, "d"), list(
        outcome = "y", outcome_type = "binary", components = c("area_m.", "ete"),
        lower = c(0, 0), upper = c(5, 5), unit_costs = c(1, 1), goal = 0.5, include_cs = FALSE
      ))
      expect_identical(nrow(.app_problems(s, got$d)), 0L)
    })
  }
  # the code shown in one locale gives the same data in the other
  code <- bb4_read(path, "UTF-8")$code
  expect_identical(bb4_in_locale("C", function() eval(parse(text = code))), bb4_read(path, "C")$d)

  # data passed to the app with such a name gets advice that works in any locale
  d <- data.frame(y = c(0, 1, 0, 1), a = 1:4, b = 4:1)
  names(d)[3] <- "\u00e9t\u00e9"
  s <- modifyList(.app_defaults(d, "d"), list(components = c("a", "\u00e9t\u00e9"), lower = c(0, 0), upper = c(4, 4), unit_costs = c(1, 1)))
  msg <- bb4_in_locale("C", function() .app_problems(s, d)$message)
  expect_true(any(grepl("such as 'ete'", msg, fixed = TRUE)))
})

test_that("the CSV hint knows thousands separators, a decimal point, and a dot for a missing value", {
  read <- function(text, sep, dec) .app_read_csv(bb4_text(text), sep, dec)
  d <- read("y;cost;z\n1;1,200;3\n0;12,500;5\n1;2,000;3\n", ";", ".")
  hint <- .app_csv_hint(d, ";", ".")
  expect_match(hint, "comma between the thousands, such as 1,200")
  expect_false(grepl("decimal comma", hint))
  d <- read("y;cost;z\n1;1.200,5;3\n0;12.500,2;5\n1;2,5;3\n", ";", ",")
  expect_match(.app_csv_hint(d, ";", ","), "point between the thousands, such as 1.200,5")
  # Comma chosen for a file with a decimal point
  d <- read("y;x;z\n1;2.5;3\n0;4.5;5\n", ";", ",")
  expect_match(.app_csv_hint(d, ";", ","), "x look like numbers with a decimal point. Choose Point")
  # Point chosen for a file with a decimal comma, as before
  d <- read("y;x;z\n1;2,5;3\n0;4,5;5\n", ";", ".")
  expect_match(.app_csv_hint(d, ";", "."), "x look like numbers with a decimal comma. Choose Comma")
  d <- read("y,x,z\n1,.,3\n0,4.5,5\n1,2,.\n0,3,4\n", ",", ".")
  expect_match(.app_csv_hint(d, ",", "."), "x, z use a dot for a missing value")
  d <- read("y;x\n1;.\n0;4,5\n", ";", ",")
  expect_match(.app_csv_hint(d, ";", ","), "x use a dot for a missing value")
  # two hints for two columns
  d <- read("y;x;z\n1;2,5;.\n0;4,5;3.5\n", ";", ".")
  expect_match(.app_csv_hint(d, ";", "."), "decimal comma.*dot for a missing value")
  d <- read("y,x,s\n1,2.5,a\n0,4,b\n", ",", ".")
  expect_null(.app_csv_hint(d, ",", "."))
})

test_that("an empty file, a file with only a header, and UTF-16 without a byte order mark say so", {
  utf16 <- bb4_file(as.vector(rbind(charToRaw("y,x\n1,2\n0,3\n"), as.raw(0))))
  expect_identical(.app_file_encoding(utf16), "UTF-16")
  expect_identical(.app_file_encoding(bb4_file(as.vector(rbind(as.raw(0), charToRaw("y,x\n1,2\n"))))), "UTF-16")
  # one stray zero byte is not UTF-16
  expect_identical(.app_file_encoding(bb4_file(charToRaw("y,x\n1,2"), as.raw(0), charToRaw("\n0,4\n"))), "UTF-8-BOM")
  files <- list(
    list(path = utf16, msg = "saved as UTF-16 text"),
    list(path = bb4_file(raw(0)), msg = "The file is empty"),
    list(path = bb4_text("y,x,z\n"), msg = "header row but no rows of data")
  )
  shiny::testServer(.build_lago_app(), {
    for (f in files) {
      session$setInputs(source = "upload", sep = ",", dec = ".", file = list(datapath = f$path, name = "f.csv"))
      expect_match(upload_error(), f$msg)
      expect_null(loaded())
    }
  })
})

test_that("rounding to four digits stays on the safe side", {
  expect_identical(.app_round_safe(12.3449, "up"), 12.35)
  expect_identical(.app_round_safe(12.3451, "up"), 12.35)
  expect_identical(.app_round_safe(12.3451, "down"), 12.34)
  expect_identical(.app_round_safe(12.3449, "down"), 12.34)
  expect_identical(.app_round_safe(-0.123456, "down"), -0.1235)
  expect_identical(.app_round_safe(0.99996, "down"), 0.9999)
  expect_identical(.app_round_safe(0.7, "down"), 0.7)
  expect_identical(.app_round_safe(0, "up"), 0)
  expect_identical(.app_round_safe(NA_real_, "up"), NA_real_)
})

test_that("the budget starts at or above the cost of the recommendation", {
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    bb4_mtcars(session, col_id, goal = 10)
    do.call(session$setInputs, stats::setNames(list(3.33, 14.1234), c(col_id("lb", "gear"), col_id("lb", "qsec"))))
    session$setInputs(run = 1)
    cost <- run_state()$run$value$rec_int_cost
    # signif() would round this cost down, below what the recommendation costs
    expect_lt(signif(cost, 4), cost)
    expect_gte(budget_prefill(), cost)
    expect_identical(budget_prefill(), .app_round_safe(cost, "up"))
  })
})

test_that("Use this outcome as the goal rounds toward the outcome the budget reaches", {
  skip_on_cran()
  set_goal <- NULL
  real_update <- shiny::updateNumericInput
  local_mocked_bindings(
    updateNumericInput = function(session, inputId, label = NULL, value = NULL, ...) {
      if (identical(inputId, "goal")) set_goal <<- value
      real_update(session, inputId, label = label, value = value, ...)
    },
    .package = "shiny"
  )
  for (case in list(list(intention = "maximize", goal = 20, want = 20.12), list(intention = "minimize", goal = 15, want = 20.13))) {
    local_mocked_bindings(lago_budget = function(...) {
      structure(list(feasible = TRUE, est_outcome = 20.1234567, intention = case$intention, budget = 30), class = "lago_budget")
    })
    shiny::testServer(.build_lago_app(), {
      bb4_mtcars(session, col_id, goal = case$goal, intention = case$intention)
      session$setInputs(run = 1)
      expect_null(run_state()$run$error)
      session$setInputs(budget = 30, budget_grid = 4)
      session$setInputs(run_budget = 1)
      session$setInputs(use_budget_goal = 1)
      expect_identical(set_goal, case$want)
    })
  }
})

test_that("the goal from a budget can be reached again when the bounds make it the best outcome", {
  skip_on_cran()
  # an upper bound where signif() of the best outcome is above that outcome
  m <- stats::lm(mpg ~ gear + qsec, datasets::mtcars)
  ubs <- seq(19.5, 20.5, by = 0.01)
  p <- stats::predict(m, data.frame(gear = 5, qsec = ubs))
  ub <- ubs[which(signif(p, 4) > p + 1e-6)[1]]
  set_goal <- NULL
  real_update <- shiny::updateNumericInput
  local_mocked_bindings(
    updateNumericInput = function(session, inputId, label = NULL, value = NULL, ...) {
      if (identical(inputId, "goal")) set_goal <<- value
      real_update(session, inputId, label = label, value = value, ...)
    },
    .package = "shiny"
  )
  shiny::testServer(.build_lago_app(), {
    bb4_mtcars(session, col_id, ub = ub)
    session$setInputs(run = 1)
    session$setInputs(budget = 1000, budget_grid = 6)
    session$setInputs(run_budget = 1)
    b <- budget_state()$run$value
    expect_gt(signif(b$est_outcome, 4), b$est_outcome)
    session$setInputs(use_budget_goal = 1)
    expect_lte(set_goal, b$est_outcome)
    session$setInputs(goal = set_goal)
    session$setInputs(run = 2)
    expect_false(isTRUE(run_state()$run$value$shrinking_method_used))
  })
})

test_that("Done returns no sweep or budget after a later run of that tab fails", {
  skip_on_cran()
  returned <- NULL
  local_mocked_bindings(stopApp = function(returnValue = invisible()) returned <<- returnValue, .package = "shiny")
  fail <- FALSE
  real_budget <- lago_budget
  real_sensitivity <- lago_sensitivity
  local_mocked_bindings(
    lago_budget = function(...) if (fail) stop("search broke") else real_budget(...),
    lago_sensitivity = function(...) if (fail) stop("sweep broke") else real_sensitivity(...)
  )
  shiny::testServer(.build_lago_app(), {
    bb4_mtcars(session, col_id)
    session$setInputs(run = 1)
    session$setInputs(budget = 28, budget_grid = 4)
    session$setInputs(run_budget = 1)
    session$setInputs(sweep_param = "cost_multiplier", sweep_how = "typed", sweep_typed = "0.5, 2")
    session$setInputs(run_sweep = 1)
    session$setInputs(done = 1)
    expect_s3_class(returned$budget, "lago_budget")
    expect_s3_class(returned$sensitivity, "lago_sensitivity")
    fail <<- TRUE
    session$setInputs(budget = 29)
    session$setInputs(run_budget = 2)
    expect_match(budget_state()$run$error, "search broke")
    session$setInputs(done = 2)
    expect_null(returned$budget)
    expect_false(any(grepl("lago_budget", returned$code)))
    expect_s3_class(returned$sensitivity, "lago_sensitivity")
    expect_identical(names(returned), c("result", "sensitivity", "budget", "code", "arguments"))
    session$setInputs(sweep_typed = "0.5, 3")
    session$setInputs(run_sweep = 2)
    session$setInputs(done = 3)
    expect_null(returned$sensitivity)
    expect_false(any(grepl("lago_sensitivity", returned$code)))
    expect_s3_class(returned$result, "lago")
  })
})

test_that("switching to an upload with no file or a file that fails clears the result", {
  skip_on_cran()
  returned <- NULL
  local_mocked_bindings(stopApp = function(returnValue = invisible()) returned <<- returnValue, .package = "shiny")
  bad <- bb4_file(as.raw(c(0xFF, 0xFE)), as.vector(rbind(charToRaw("a,b\n1,2\n"), as.raw(0))))
  shiny::testServer(.build_lago_app(), {
    bb4_mtcars(session, col_id)
    session$setInputs(run = 1)
    session$setInputs(source = "upload")
    expect_null(run_state())
    expect_match(as.character(output$results$html), "Press Find recommendation")
    session$setInputs(done = 1)
    expect_null(returned$result)
    expect_null(returned$code)
    session$setInputs(source = "mtcars")
    bb4_mtcars(session, col_id)
    session$setInputs(run = 2)
    session$setInputs(source = "upload", file = list(datapath = bad, name = "u.csv"))
    expect_null(run_state())
    session$setInputs(done = 2)
    expect_null(returned$result)
  })
})

test_that("Load settings keeps the cost coefficients", {
  skip_on_cran()
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "mtcars", sep = ",", dec = ".")
    session$setInputs(
      outcome = "mpg", outcome_type = "continuous", goal_mode = "outcome", goal = 27, intention = "maximize",
      components = c("gear", "qsec"), cost_mode = "coef", cost_type = "linear", center_mode = "none",
      include_cs = FALSE, approach = "unconditional", method = "numerical"
    )
    do.call(session$setInputs, stats::setNames(
      list(3, 5, "0, 7", 14, 20, "1, 2, 0.5"),
      c(col_id("lb", "gear"), col_id("ub", "gear"), coef_id("gear"), col_id("lb", "qsec"), col_id("ub", "qsec"), coef_id("qsec"))
    ))
    saved <- readRDS(output$save_settings)
    # a field saved as NULL keeps its default
    saved$spec["cluster_col"] <- list(NULL)
    f <- tempfile(fileext = ".rds")
    saveRDS(saved, f)
    session$setInputs(load_settings = list(datapath = f, name = "s.rds"))
    expect_identical(start_spec()$coefs, list(c(0, 7), c(1, 2, 0.5)))
    expect_identical(start_spec()$cluster_col, "")
    html <- as.character(output$component_rows$html)
    expect_match(html, "value=\"0, 7\"", fixed = TRUE)
    expect_match(html, "value=\"1, 2, 0.5\"", fixed = TRUE)
  })
})

test_that("a column the app reads is never one it replaced first", {
  set.seed(3)
  n <- 300
  d <- data.frame(group = sample(c("g1", "g2", "g3"), n, TRUE), trt = sample(0:1, n, TRUE), a = runif(n, 0, 5), b = runif(n, 0, 3))
  d$y <- rbinom(n, 1, stats::plogis(-1 + 0.3 * d$a))
  s <- modifyList(.app_defaults(d, "d"), list(
    outcome = "y", components = c("a", "b"), goal_mode = "both", goal = 0.6, arm_col = "trt", arm_value = "1",
    center_mode = "effects", center_col = "group", include_cs = FALSE
  ))
  s <- .app_set_component_defaults(s, d)
  s$goal <- 0.6
  p <- .app_problems(s, d)
  expect_true(any(p$step == "centers" & grepl("The column group is chosen as the center column, but the app first builds", p$message)))
  # the period column, read after the center column is built
  d$center <- sample(c("c1", "c2"), n, TRUE)
  d$site <- sample(c("s1", "s2", "s3"), n, TRUE)
  s2 <- modifyList(s, list(goal_mode = "outcome", center_col = "site", time_on = TRUE, period_col = "center", period_value = "c2"))
  expect_true(any(grepl("The column center is chosen as the period column", .app_problems(s2, d)$message)))
  # a column read before anything replaces it is fine
  s3 <- modifyList(s, list(arm_col = "center", arm_value = "c1", center_col = "site"))
  expect_false(any(grepl("first builds", .app_problems(s3, d)$message)))
  # the total column of center level data, read after the proportion is built
  cl <- data.frame(site = rep(c("s1", "s2", "s3"), 4), a = runif(12, 0, 5), b = runif(12, 0, 3), succ = rpois(12, 5) + 1)
  cl$proportion <- cl$succ + rpois(12, 10)
  s4 <- modifyList(.app_empty_spec(), list(
    structure = "center", cl_kind = "successes", cl_value = "succ", cl_total = "proportion",
    components = c("a", "b"), goal = 0.3, include_cs = FALSE
  ))
  s4 <- .app_set_component_defaults(s4, cl)
  expect_true(any(grepl("The column proportion is chosen as the total column", .app_problems(.app_resolve(s4), cl)$message)))
})

test_that("a result opens with the grid steps the package used for it", {
  set.seed(1)
  n <- 1600
  d <- data.frame(a = runif(n, 0, 10), b = runif(n, 0, 6), k = 1)
  d$y <- rbinom(n, 1, stats::plogis(-1 + 0.15 * d$a + 0.1 * d$b))
  r <- bb4_quiet(lago_optimization(d,
    outcome_name = "y", outcome_type = "binary", intervention_components = c("a", "b"),
    intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(4, 3), outcome_goal = 0.42, unit_costs = c(1, 3),
    optimization_method = "grid_search", quiet = TRUE
  ))
  a <- attr(r, "lago_call_args")
  s <- .app_spec_from_args(a)$spec
  steps <- c(diff(range(d$a)), diff(range(d$b))) / 20
  expect_identical(s$grid_steps, steps)
  expect_identical(s$cs_steps, steps)
  b <- .app_build(s, a$data)
  expect_identical(b$args$optimization_grid_search_step_size, steps)
  expect_identical(b$args$confidence_set_grid_step_size, steps)
  r2 <- bb4_quiet(do.call(lago_optimization, c(list(data = b$data), b$args, quiet = TRUE)))
  expect_equal(r2$rec_int, r$rec_int)
  expect_equal(r2$confidence_set_size_percentage, r$confidence_set_size_percentage)
  expect_equal(nrow(r2$cs), nrow(r$cs))
  # steps the call gave are kept, and a constant component leaves its step to the app
  a2 <- a
  a2$confidence_set_grid_step_size <- c(0.5, 0.25)
  a2$intervention_components <- c("a", "k")
  expect_identical(.app_spec_from_args(a2)$spec$cs_steps, c(0.5, 0.25))
  a2$confidence_set_grid_step_size <- NULL
  a2$include_confidence_set <- FALSE
  a2$optimization_method <- "numerical"
  expect_identical(.app_spec_from_args(a2)$spec$cs_steps, numeric(0))
  a2$include_confidence_set <- TRUE
  expect_identical(.app_spec_from_args(a2)$spec$cs_steps, c(steps[1], NA))
  # the app shows them in the advanced settings
  skip_on_cran()
  want <- steps
  app <- .build_lago_app(data = a$data, data_name = "attr(r, \"lago_call_args\")$data", start = .app_spec_from_args(a))
  shiny::testServer(app, {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    session$setInputs(components = c("a", "b"))
    expect_match(as.character(output$grid_steps_ui$html), sprintf("value=\"%s\"", format(want[1], digits = 15)), fixed = TRUE)
    expect_match(as.character(output$cs_steps_ui$html), sprintf("value=\"%s\"", format(want[2], digits = 15)), fixed = TRUE)
  })
})

test_that("the checks catch sizes that are not whole, a single usable period, and an arm with no outcome", {
  set.seed(1)
  n <- 400
  d <- data.frame(per = sample(1:4, n, TRUE), a = runif(n, 0, 5), b = runif(n, 0, 3), trt = sample(0:1, n, TRUE))
  d$y <- rbinom(n, 1, stats::plogis(-1 + 0.3 * d$a + 0.2 * d$b))
  s <- .app_defaults(d, "d")
  s <- modifyList(s, list(outcome = "y", outcome_type = "binary", components = c("a", "b"), include_cs = FALSE))
  s <- .app_set_component_defaults(s, d)
  s$goal <- 0.7
  msgs <- function(change, data = d) .app_problems(modifyList(s, change), data)$message
  power <- list(goal_mode = "power", arm_col = "trt", arm_value = "1")
  expect_true(any(grepl("must be whole numbers", msgs(c(power, n_centers = 10.5)))))
  expect_true(any(grepl("must be whole numbers", msgs(c(power, n_patients = 0.5)))))
  expect_false(any(grepl("whole numbers", msgs(power))))
  d1 <- d
  d1$one <- 1
  expect_true(any(grepl("at least two periods", msgs(list(time_on = TRUE, period_col = "one", period_value = "1"), d1))))
  d1$only4 <- ifelse(d1$per == 4, 1, NA)
  d1$cv <- ifelse(d1$per == 4, d1$a, NA)
  expect_true(any(grepl("at least two periods", msgs(list(time_on = TRUE, period_col = "per", period_value = "4", covariates = "cv"), d1))))
  expect_false(any(grepl("at least two periods", msgs(list(time_on = TRUE, period_col = "per", period_value = "4"), d1))))
  d3 <- d
  d3$y[d3$trt == 1] <- NA
  expect_true(any(grepl("The treatment arm has no row with an observed outcome", msgs(power, d3))))
  d3 <- d
  d3$y[d3$trt == 0] <- NA
  expect_true(any(grepl("The control arm has no row with an observed outcome", msgs(power, d3))))
  # center level data counts only rows with patients
  cl <- data.frame(site = rep(sprintf("s%d", 1:6), 2), trt = rep(0:1, 6), a = runif(12, 0, 5), b = runif(12, 0, 3))
  cl$tot <- ifelse(cl$trt == 1, 0, 20)
  cl$succ <- 5
  sc <- modifyList(.app_empty_spec(), list(
    structure = "center", cl_kind = "successes", cl_value = "succ", cl_total = "tot",
    components = c("a", "b"), goal = 0.3, include_cs = FALSE, goal_mode = "power", arm_col = "trt", arm_value = "1"
  ))
  sc <- .app_resolve(.app_set_component_defaults(sc, cl))
  expect_true(any(grepl("The treatment arm has no row", .app_problems(sc, cl)$message)))
  cl$tot <- 20
  expect_false(any(grepl("arm has no row", .app_problems(sc, cl)$message)))
})

test_that("center weights of a result open exactly as the result has them", {
  skip_on_cran()
  set.seed(1)
  n <- 600
  d <- data.frame(center = sample(sprintf("s%d", 1:5), n, TRUE), a = runif(n, 0, 5), b = runif(n, 0, 3))
  d$y <- rbinom(n, 1, stats::plogis(-1 + 0.3 * d$a + 0.2 * d$b))
  w <- (1:5) / 15
  names(w) <- sprintf("s%d", 1:5)
  r <- bb4_quiet(lago_optimization(d,
    outcome_name = "y", outcome_type = "binary", intervention_components = c("a", "b"),
    intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(5, 3), outcome_goal = 0.6, unit_costs = c(1, 1),
    include_center_effects = TRUE, center_weights_for_outcome_goal = w, include_confidence_set = FALSE, quiet = TRUE
  ))
  a <- attr(r, "lago_call_args")
  app <- .build_lago_app(data = a$data, data_name = "d", start = .app_spec_from_args(a))
  shiny::testServer(app, {
    session$setInputs(source = "passed", sep = ",", dec = ".")
    session$setInputs(center_mode = "effects", center_col = "center", center_for = "", cw_on = TRUE, structure = "individual")
    html <- as.character(output$cw_ui$html)
    expect_match(html, sprintf("value=\"%s\"", format(w[[1]], digits = 15)), fixed = TRUE)
  })
})

test_that("the ICC center column is offered for one row per center, counting only centers with patients", {
  set.seed(5)
  nc <- 12
  cl <- data.frame(center = paste0("c", 1:nc))
  cl$group <- ifelse(1:nc %% 2 == 0, "treatment", "control")
  cl$dose <- ifelse(cl$group == "treatment", runif(nc, 0.3, 1), 0)
  cl$center_sample_size <- sample(20:60, nc, TRUE)
  cl$proportion <- stats::rbinom(nc, cl$center_sample_size, stats::plogis(-1 + 1.2 * cl$dose)) / cl$center_sample_size
  expect_identical(.app_cluster_choices(cl, "proportion", "group", "treatment", exclude = "dose", center_level = TRUE), "center")
  # one row per patient, a column with a new value on every row names patients
  expect_identical(.app_cluster_choices(cl, "proportion", "group", "treatment", exclude = "dose"), character(0))
  cl2 <- rbind(transform(cl, period = 1), transform(cl, period = 2))
  cl2$center_sample_size[cl2$group == "treatment" & cl2$center != "c12"] <- 0
  expect_false("center" %in% .app_cluster_choices(cl2, "proportion", "group", "treatment", exclude = "dose", center_level = TRUE))
  # the app lists it for center level settings
  cl$succ <- round(cl$proportion * cl$center_sample_size)
  cl$tot <- cl$center_sample_size
  cl$center_sample_size <- NULL
  cl$proportion <- NULL
  s <- modifyList(.app_empty_spec(), list(
    structure = "center", cl_kind = "successes", cl_value = "succ", cl_total = "tot",
    components = "dose", goal_mode = "power", arm_col = "group", arm_value = "treatment"
  ))
  expect_identical(.app_choices(cl, s)$clusters, "center")
  r <- bb4_quiet(lago_optimization(
    data = transform(cl, proportion = succ / tot, center_sample_size = tot), input_data_structure = "center_level",
    outcome_name = "proportion", outcome_type = "binary", intervention_components = "dose", intervention_lower_bounds = 0,
    intervention_upper_bounds = 1, cost_list_of_vectors = list(c(0, 1)), include_confidence_set = FALSE, quiet = TRUE,
    include_center_effects = FALSE, power_goal = 0.8, num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 20,
    icc = 0.05, power_goal_cluster_id = "center"
  ))
  expect_s3_class(r, "lago")
})

test_that("a power goal may be any value between 0 and 1, and the input allows it", {
  d <- as.data.frame(BB_data)
  s <- modifyList(.app_defaults(d, "BB_data"), list(goal_mode = "power", arm_col = "pre_post", arm_value = "1"))
  expect_false(any(grepl("power goal must", .app_problems(modifyList(s, list(power_goal = 0.02)), d)$message)))
  for (bad in c(0, 1, NA)) {
    expect_true(any(grepl("above 0 and below 1", .app_problems(modifyList(s, list(power_goal = bad)), d)$message)))
  }
  ui <- environment(.build_lago_app()$httpHandler)$ui
  tag <- htmltools::tagQuery(ui)$find("#power_goal")$selectedTags()[[1]]
  expect_identical(c(tag$attribs$min, tag$attribs$max), c(0, 1))
})
