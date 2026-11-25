# Fixes to lago_app() from the third bug bash.

# Run f with LC_CTYPE set to the C locale or a UTF-8 one, and set it back.
bb3_in_locale <- function(kind, f) {
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
bb3_file <- function(...) {
  path <- tempfile(fileext = ".csv")
  writeBin(c(...), path)
  path
}
bb3_codes <- function(x) vapply(x, function(s) paste(utf8ToInt(s), collapse = " "), character(1), USE.NAMES = FALSE)
# read a file as the app does and with the code it shows, in a locale
bb3_read <- function(path, kind, sep = ",") {
  bb3_in_locale(kind, function() {
    enc <- .app_file_encoding(path)
    d <- .app_read_csv(path, sep, ".", enc)
    code <- .app_read_csv_code(path, sep, ".", enc, .app_csv_header(path, sep, enc))
    list(enc = enc, d = d, code = code, from_code = eval(parse(text = code)), renamed = .app_renamed(path, sep, d, enc))
  })
}

test_that("a Windows-1252 file whose text starts with an accent or a curly quote is read in any locale", {
  path <- bb3_file(
    charToRaw("y,x,site\n1,2,"), as.raw(0xC9), charToRaw("vora\n0,3,"), as.raw(c(0x93, 0x41, 0x94)),
    charToRaw("\n1,4,"), as.raw(0x80), charToRaw("5\n0,5,b\n")
  )
  for (kind in c("C", "UTF-8")) {
    got <- bb3_read(path, kind)
    expect_identical(got$enc, "CP1252")
    expect_equal(nrow(got$d), 4)
    expect_identical(bb3_codes(got$d$site), bb3_codes(c("\u00c9vora", "\u201cA\u201d", "\u20ac5", "b")))
    expect_identical(got$from_code, got$d)
    expect_match(got$code, "iconv(readLines(", fixed = TRUE)
  }
  # a byte that Windows-1252 leaves out is read as Latin-1
  path <- bb3_file(charToRaw("y,site\n1,a"), as.raw(c(0x81, 0xE9)), charToRaw("\n0,b\n"))
  got <- bb3_read(path, "C")
  expect_identical(got$enc, "latin1")
  expect_identical(bb3_codes(got$d$site), bb3_codes(c("a\u0081\u00e9", "b")))
  expect_identical(got$from_code, got$d)
})

test_that("a UTF-8 file keeps every row in the C locale, and the shown code does too", {
  path <- bb3_file(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("y,x,site\n1,2,caf\xc3\xa9\n0,3,b\n1,4,c\n"))
  for (kind in c("C", "UTF-8")) {
    got <- bb3_read(path, kind)
    expect_identical(got$enc, "UTF-8")
    expect_identical(names(got$d), c("y", "x", "site"))
    expect_identical(bb3_codes(got$d$site), bb3_codes(c("caf\u00e9", "b", "c")))
    expect_identical(got$from_code, got$d)
  }
  # ASCII text, with or without a byte order mark, keeps the plain code
  path <- bb3_file(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("y,x\n1,2\n"))
  got <- bb3_read(path, "C")
  expect_identical(got$enc, "UTF-8-BOM")
  expect_match(got$code, "fileEncoding = \"UTF-8-BOM\"", fixed = TRUE)
  expect_identical(got$from_code, got$d)
})

test_that("a header with accents gives the same ASCII name in any locale", {
  path <- bb3_file(charToRaw("y,x,"), as.raw(0xE2), charToRaw("ge\n1,2,3\n0,3,4\n"))
  for (kind in c("C", "UTF-8")) {
    got <- bb3_read(path, kind)
    expect_identical(names(got$d), c("y", "x", "age"))
    expect_identical(got$from_code, got$d)
    expect_match(got$code, "check.names = FALSE", fixed = TRUE)
    expect_length(got$renamed, 1)
  }
  # names that must change are set by the shown code
  path <- bb3_file(
    charToRaw("y,"), as.raw(0xE2), charToRaw(" ge,"), as.raw(c(0x93, 0x71, 0x94)), charToRaw(",x-1\n1,2,3,4\n")
  )
  for (kind in c("C", "UTF-8")) {
    got <- bb3_read(path, kind)
    expect_identical(bb3_codes(names(got$d)), bb3_codes(c("y", "a.ge", "X.q.", "x.1")))
    expect_identical(got$from_code, got$d)
    expect_match(got$code, "stats::setNames(", fixed = TRUE)
    expect_length(got$renamed, 3)
  }
  x <- c("\u0661x", "a", "a", "if")
  expect_identical(bb3_codes(.app_make_names(x)), bb3_in_locale("UTF-8", function() bb3_codes(make.names(x, unique = TRUE))))
  expect_identical(.app_ascii_string("\u00e2\"b"), "\"\\u{e2}\\\"b\"")
})

test_that("a UTF-16 file gets a clear message", {
  path <- bb3_file(as.raw(c(0xFF, 0xFE)), as.vector(rbind(charToRaw("y,x\r\n1,2\r\n"), as.raw(0))))
  expect_identical(.app_file_encoding(path), "UTF-16")
  expect_error(.app_read_csv(path), "UTF-16")
  big <- bb3_file(as.raw(c(0xFE, 0xFF)), as.vector(rbind(as.raw(0), charToRaw("y,x\n"))))
  expect_identical(.app_file_encoding(big), "UTF-16")
  shiny::testServer(.build_lago_app(), {
    session$setInputs(source = "upload", sep = ",", dec = ".", file = list(datapath = path, name = "u.csv"))
    expect_match(upload_error(), "saved as UTF-16 text")
    expect_false(grepl("separator", upload_error()))
  })
})

test_that("starting from a result keeps named center weights with their centers", {
  d <- as.data.frame(BB_data)
  d$center <- factor(d$site_name)
  lv <- levels(d$center)
  w <- seq_along(lv) / sum(seq_along(lv))
  names(w) <- lv
  a <- list(
    data = d, outcome_name = "y", outcome_type = "binary", outcome_goal = 0.8, outcome_goal_intention = "maximize",
    intervention_components = "coaching_updt", intervention_lower_bounds = 0, intervention_upper_bounds = 1,
    unit_costs = 1, default_cost_fxn_type = "linear", include_center_effects = TRUE,
    link = "default", confidence_set_alpha = 0.05, center_weights_for_outcome_goal = rev(w)
  )
  expect_equal(.app_spec_from_args(a)$spec$cw, w)
  a$center_weights_for_outcome_goal <- unname(w)
  expect_equal(.app_spec_from_args(a)$spec$cw, w)
})

test_that("a cluster column named like a column the app builds is refused", {
  d <- as.data.frame(BB_data)
  d$center <- d$distance_10
  s <- modifyList(.app_defaults(d, "d"), list(
    outcome = "pp3_oxytocin_mother", outcome_type = "binary", goal = 0.85, intention = "maximize",
    components = c("coaching_updt", "launch_duration"), lower = c(0, 0), upper = c(40, 10), unit_costs = c(1, 8),
    doses = c(NA_real_, NA_real_), include_cs = FALSE, goal_mode = "both", arm_col = "data_flag", arm_value = "rct",
    power_goal = 0.8, n_centers = 10, n_patients = 50, icc = 0.05, cluster_col = "center",
    center_mode = "effects", center_col = "site_name", center_for = ""
  ))
  msgs <- paste(.app_problems(s, d)$message, collapse = "\n")
  expect_match(msgs, "The column center is chosen as the cluster column")
  s$cluster_col <- "distance_10"
  expect_false(grepl("the app builds", paste(.app_problems(s, d)$message, collapse = "\n")))
})

test_that("the logit 0/1 check counts only the rows glm() fits", {
  set.seed(1)
  d <- datasets::mtcars
  d$y <- runif(32, 0.1, 0.9)
  d$y[1] <- 1
  d$gear[1] <- NA
  s <- modifyList(.app_defaults(d, "d"), list(
    outcome = "y", outcome_type = "continuous", link = "logit", glm_family = "default", goal = 0.6,
    components = c("gear", "qsec"), lower = c(3, 14), upper = c(5, 20), unit_costs = c(1, 1), include_cs = FALSE
  ))
  msgs <- function() paste(.app_problems(s, d)$message, collapse = "\n")
  expect_false(grepl("exactly 0 or 1", msgs()))
  d$gear[1] <- 4
  expect_match(msgs(), "exactly 0 or 1")
})
