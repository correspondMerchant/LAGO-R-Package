# Fixes to lago_app() from the second bug bash.

bb2_spec <- function(data, name = "data", ...) {
  s <- .app_defaults(data, name)
  s[names(list(...))] <- list(...)
  s
}
bb2_msgs <- function(s, data) paste(.app_problems(s, data)$message, collapse = "\n")

bb2_latin1_csv <- function(path) {
  set.seed(1)
  site <- rep(c("Abc", "Cl\u00ednica", "Def"), each = 20)
  lines <- c("site;y;x1", sprintf("%s;%d;%s", site, rbinom(60, 1, 0.5), sub(".", ",", format(runif(60, 0, 5)), fixed = TRUE)))
  writeBin(iconv(paste0(paste(lines, collapse = "\n"), "\n"), "UTF-8", "latin1", toRaw = TRUE)[[1]], path)
}

test_that("a CSV that is not UTF-8 is read in full as Latin-1", {
  path <- tempfile(fileext = ".csv")
  bb2_latin1_csv(path)
  expect_identical(.app_file_encoding(path), "latin1")
  d <- .app_read_csv(path, sep = ";", dec = ",")
  expect_equal(nrow(d), 60)
  expect_equal(sum(d$site == "Cl\u00ednica"), 20)
  expect_identical(.app_renamed(path, ";", d, "latin1"), character(0))
  expect_match(.app_read_csv_code("a.csv", ";", ",", "latin1"), "fileEncoding = \"latin1\"", fixed = TRUE)
  utf8 <- tempfile(fileext = ".csv")
  writeLines(enc2utf8(c("site,y", "Cl\u00ednica,1")), utf8, useBytes = TRUE)
  expect_identical(.app_file_encoding(utf8), "UTF-8-BOM")
})

test_that("the app tells the user when an upload was read as Latin-1", {
  path <- tempfile(fileext = ".csv")
  bb2_latin1_csv(path)
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "upload", sep = ";", dec = ",", file = list(datapath = path, name = "l.csv"))
    expect_equal(nrow(current_data()), 60)
    expect_match(loaded()$name, "fileEncoding = \"latin1\"", fixed = TRUE)
    expect_match(as.character(output$data_info$html), "read as Latin-1")
  })
})

test_that("a period with no row the model can use is refused and is not the default", {
  d <- as.data.frame(BB_data)
  d$pp3_oxytocin_mother[d$pilot_phase == 4] <- NA
  s <- bb2_spec(d, "BB_data", time_on = TRUE, period_col = "pilot_phase", period_value = "4", include_cs = FALSE)
  expect_match(bb2_msgs(s, d), "The period 4 has no row the model can use")
  expect_identical(.app_default_period(s, d, "pilot_phase"), "2")
  expect_identical(.app_choices(d, modifyList(s, list(period_value = "")))$period, "2")
  s$period_value <- "2"
  expect_equal(nrow(.app_problems(s, d)), 0)
  # center effects and center characteristics count as model columns too
  d$site_name[d$pilot_phase == 2] <- NA
  expect_identical(.app_default_period(modifyList(s, list(center_mode = "effects", center_col = "site_name")), d, "pilot_phase"), "1")
  d$birth_volume_100[d$pilot_phase == 1] <- NA
  expect_identical(.app_default_period(modifyList(s, list(center_mode = "chars", center_chars = "birth_volume_100")), d, "pilot_phase"), "2")
  # with no usable period the latest one is the default, and no column gives none
  d$pp3_oxytocin_mother <- NA
  expect_identical(.app_default_period(s, d, "pilot_phase"), "4")
  expect_identical(.app_default_period(s, d, "nope"), character(0))
})

test_that("a chosen column named like a column the app builds is refused", {
  d <- as.data.frame(BB_data)
  d$center <- d$birth_volume_100
  d$group <- d$launch_duration
  d$period <- d$launch_duration
  s <- bb2_spec(d, "BB_data", center_mode = "effects", center_col = "site_name", covariates = "center", include_cs = FALSE)
  expect_match(bb2_msgs(s, d), "The column center is chosen as a covariate, but the app builds its own column named center")
  s <- bb2_spec(d, "BB_data", components = c("coaching_updt", "group"), goal_mode = "both", arm_col = "data_flag", arm_value = "rct", include_cs = FALSE)
  s <- .app_set_component_defaults(s, d)
  expect_match(bb2_msgs(s, d), "The column group is chosen as an intervention component")
  s <- bb2_spec(d, "BB_data", time_on = TRUE, period_col = "pilot_phase", period_value = "4", weights_col = "period", include_cs = FALSE)
  expect_match(bb2_msgs(s, d), "The column period is chosen as the weights")
  s <- bb2_spec(d, "BB_data", center_mode = "chars", center_chars = "period", center_char_values = 1, time_on = TRUE, period_col = "pilot_phase", period_value = "4", include_cs = FALSE)
  expect_match(bb2_msgs(s, d), "The column period is chosen as a center characteristic")
  d$period <- NULL
  s <- bb2_spec(d, "BB_data", outcome = "group", outcome_type = "continuous", goal = 2, goal_mode = "outcome", include_cs = FALSE)
  expect_false(grepl("chosen as the outcome", bb2_msgs(s, d)))
  # a column that is not built is fine
  expect_false(grepl("the app builds", bb2_msgs(bb2_spec(d, "BB_data", covariates = "center", include_cs = FALSE), d)))

  # center level data builds proportion and center_sample_size
  set.seed(2)
  cd <- data.frame(clinic = rep(paste0("c", 1:8), each = 10), succ = rpois(80, 5), tot = 20, x1 = runif(80, 0, 4), center_sample_size = runif(80))
  s <- .app_defaults(cd, "cd")
  s$structure <- "center"
  s$cl_kind <- "successes"
  s$cl_value <- "succ"
  s$cl_total <- "tot"
  s$components <- c("x1", "center_sample_size")
  s <- .app_set_component_defaults(s, cd)
  s$goal <- 0.3
  expect_match(bb2_msgs(s, cd), "The column center_sample_size is chosen as an intervention component")
  expect_identical(.app_built_columns(s, cd), c("proportion", "center_sample_size"))
  s$cl_kind <- "proportion"
  s$cl_value <- "proportion"
  s$cl_total <- "center_sample_size"
  expect_identical(.app_built_columns(s, cd), character(0))
  # an outcome named like a built column, with individual data
  b <- as.data.frame(BB_data)
  b$group <- b$pp3_oxytocin_mother
  s <- bb2_spec(b, "BB_data", outcome = "group", goal_mode = "both", arm_col = "data_flag", arm_value = "rct", include_cs = FALSE)
  expect_match(bb2_msgs(s, b), "The column group is chosen as the outcome")
})

test_that("a continuous outcome of exactly 0 or 1 with the logit link is refused", {
  m <- mtcars
  m$share <- (m$mpg - min(m$mpg)) / diff(range(m$mpg))
  s <- bb2_spec(m, "mtcars", outcome = "share", outcome_type = "continuous", link = "logit", goal = 0.6, include_cs = FALSE)
  expect_match(bb2_msgs(s, m), "has values of exactly 0 or 1. Choose the proportion model")
  s$glm_family <- "quasibinomial"
  expect_false(grepl("exactly 0 or 1", bb2_msgs(s, m)))
  m$share <- pmin(pmax(m$share, 0.01), 0.99)
  s$glm_family <- "default"
  expect_false(grepl("exactly 0 or 1", bb2_msgs(s, m)))
})

test_that("zero padded numeric period labels run", {
  d <- as.data.frame(BB_data)
  d$per <- sprintf("%02d", d$pilot_phase)
  s <- bb2_spec(d, "BB_data", time_on = TRUE, period_col = "per", period_value = "04", include_cs = FALSE)
  expect_equal(nrow(.app_problems(s, d)), 0)
  b <- .app_build(s, d)
  r <- .app_run(function() do.call(lago_optimization, c(list(data = b$data), b$args, list(quiet = TRUE))))
  expect_null(r$error)
  expect_true(all(is.finite(r$value$rec_int)))
})

test_that("passed data with column names that are not valid R names is refused before the run", {
  d <- as.data.frame(BB_data)
  names(d)[names(d) == "launch_duration"] <- "launch duration"
  s <- bb2_spec(d, "d", components = c("coaching_updt", "launch duration"), include_cs = FALSE)
  s <- .app_set_component_defaults(s, d)
  expect_match(bb2_msgs(s, d), "'launch duration' are not valid R names.*'launch.duration'")
  expect_false(grepl("valid R names", bb2_msgs(bb2_spec(d, "d", include_cs = FALSE), d)))
})

test_that("a binary outcome that cannot be fitted with the identity link says to use logit", {
  raw <- "Error occurred during model fitting step:\n Error: no valid set of coefficients has been found: please supply starting values"
  expect_match(.app_explain_error(raw, "identity"), "Choose the logit link")
  expect_identical(.app_explain_error(raw, NULL), raw)
  expect_identical(.app_explain_error("other", "identity"), "other")
  expect_null(.app_explain_error(NULL, "identity"))
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "BB_data", sep = ",", dec = ".")
    session$setInputs(
      outcome = "pp3_oxytocin_mother", outcome_type = "binary", goal_mode = "outcome", goal = 0.85, intention = "maximize",
      components = c("coaching_updt", "launch_duration"), cost_mode = "unit", cost_type = "linear", center_mode = "none",
      include_cs = FALSE, approach = "unconditional", method = "numerical", link = "identity"
    )
    do.call(session$setInputs, stats::setNames(list(0, 40, 1, 0, 3, 1), c(col_id("lb", "coaching_updt"), col_id("ub", "coaching_updt"), col_id("cost", "coaching_updt"), col_id("lb", "launch_duration"), col_id("ub", "launch_duration"), col_id("cost", "launch_duration"))))
    session$setInputs(run = 1)
    expect_match(run_state()$run$error, "could not be fitted with the identity link")
  })
})
