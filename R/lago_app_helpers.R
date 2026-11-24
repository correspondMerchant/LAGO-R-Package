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
    include_cs = TRUE,
    # advanced settings: "default" and empty values leave the package default
    glm_family = "default", link = "default", weights_col = "",
    method = "numerical", grid_steps = numeric(0),
    cs_level = 0.95, cs_steps = numeric(0),
    prev_on = FALSE, prev = numeric(0),
    # center level data: one row per center and period, with the successes or
    # the proportion of a row and its total
    structure = "individual", cl_kind = "successes", cl_value = "", cl_total = "",
    cw_on = FALSE, cw = numeric(0)
  )
}

# The bundled examples, set up so the first run works without any change.
.app_presets <- function() {
  list(
    BB_data = list(
      outcome = "pp3_oxytocin_mother", outcome_type = "binary",
      components = c("coaching_updt", "launch_duration"), goal = 0.85
    ),
    main_pulesa_data = list(
      outcome = "Success", outcome_type = "binary",
      components = c(
        "AccessMedicines", "AccessBPMachines", "HypertensionTraining", "DeliveryA",
        "DeliveryB", "RemoteMonitoring", "PerformanceImprovement"
      ),
      goal = 0.8,
      # the model of the package's PULESA example, without its confidence set,
      # whose grid would be far too large for seven components
      settings = list(
        structure = "center", cl_kind = "successes", cl_value = "Success", cl_total = "Total_visit",
        covariates = c("MI_DeliveryB", "MI_RemoteMonitoring", "MI_PerformanceImprovement"),
        center_mode = "effects", center_col = "Clinic",
        time_on = TRUE, period_col = "Period", period_value = "13", include_cs = FALSE
      )
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
  if (!is.null(preset) && identical(spec$outcome, preset$outcome)) {
    for (nm in names(preset$settings)) spec[[nm]] <- preset$settings[[nm]]
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

# The R code that reads an uploaded CSV the same way the app did, in any
# locale. A plain ASCII file is read directly. Other text is read as lines,
# turned into UTF-8 and then parsed, as read.csv() with a fileEncoding turns
# the text into the native encoding and loses rows in a locale such as C.
# header is the header of the file as it was written, which says whether the
# code must set the column names itself.
.app_read_csv_code <- function(name, sep = ",", dec = ".", encoding = "UTF-8-BOM", header = character(0)) {
  extra <- c(
    if (!identical(sep, ",")) sprintf("sep = %s", deparse1(sep)),
    if (!identical(dec, ".")) sprintf("dec = %s", deparse1(dec))
  )
  if (identical(encoding, "UTF-8-BOM")) {
    return(sprintf(
      "read.csv(%s)",
      paste(c(deparse1(name), extra, "na.strings = c(\"\", \"NA\")", "fileEncoding = \"UTF-8-BOM\""), collapse = ", ")
    ))
  }
  text <- if (identical(encoding, "UTF-8")) {
    sprintf("sub(\"^\\ufeff\", \"\", readLines(%s, warn = FALSE, encoding = \"UTF-8\"))", deparse1(name))
  } else {
    sprintf("iconv(readLines(%s, warn = FALSE), %s, \"UTF-8\")", deparse1(name), deparse1(encoding))
  }
  # make.names() keeps an accented letter only in a locale that has it, so
  # names with such letters are set by the code
  wide <- any(utf8ToInt(paste(header, collapse = "")) > 127)
  fixed <- .app_csv_names(header)
  code <- sprintf(
    "read.csv(%s)",
    paste(c(sprintf("text = %s", text), extra, "na.strings = c(\"\", \"NA\")", "encoding = \"UTF-8\"", if (wide) "check.names = FALSE"), collapse = ", ")
  )
  if (wide && !identical(fixed, header)) {
    code <- sprintf("stats::setNames(%s, c(%s))", code, paste(vapply(fixed, .app_ascii_string, character(1)), collapse = ", "))
  }
  code
}

# A string as R code in ASCII, with \u escapes for other characters, so the
# code means the same in every locale.
.app_ascii_string <- function(x) {
  chars <- vapply(utf8ToInt(x), function(cp) {
    if (cp > 127) sprintf("\\u{%x}", cp) else substr(deparse1(intToUtf8(cp)), 2, nchar(deparse1(intToUtf8(cp))) - 1)
  }, character(1))
  paste0("\"", paste(chars, collapse = ""), "\"")
}

# make.names(unique = TRUE) as a UTF-8 locale gives it, in any locale. In
# another locale the names are made in a UTF-8 locale for a moment, as which
# letters a name may keep depends on the locale. Where the system has no
# UTF-8 locale, the letters that are not ASCII are first turned into ASCII.
.app_make_names <- function(x, utf8_locales = c("C.UTF-8", "en_US.UTF-8", "UTF-8")) {
  x <- enc2utf8(as.character(x))
  if (isTRUE(l10n_info()[["UTF-8"]]) || all(utf8ToInt(paste(x, collapse = "")) < 128)) {
    return(make.names(x, unique = TRUE))
  }
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  for (loc in utf8_locales) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) {
      out <- make.names(x, unique = TRUE)
      Encoding(out) <- "UTF-8"
      return(out)
    }
  }
  .app_unique_names(make.names(.app_ascii_fold(x)), x)
}

# Names made unique as make.names(unique = TRUE) makes them, where a name that
# is the same as its header keeps it and the changed names get a number.
.app_unique_names <- function(made, header) {
  o <- order(made != header)
  made[o] <- make.unique(made[o])
  made
}

# Whether a name has no letter or digit, apart from a number that makes it
# unique, such as ".", "..1" or "...". R reserves some of them, and the others
# do not say which column they are.
.app_unreadable_name <- function(x) grepl("^[.]*([.][0-9]+)?$", x)

# Letters with accents and other Latin letters as their plain ASCII letters,
# and any other character that is not ASCII as a dot.
.app_ascii_fold <- function(x) {
  from <- c(0xAA, 0xB5, 0xBA, 0xC0:0xD6, 0xD8:0xF6, 0xF8:0x17F)
  to <- strsplit(paste(
    "a u o A A A A A A AE C E E E E I I I I D N O O O O O",
    "O U U U U Y TH ss a a a a a a ae c e e e e i i i i d n o o o o o",
    "o u u u u y th y",
    "A a A a A a C c C c C c C c D d D d E e E e E e E e E e G g G g",
    "G g G g H h H h I i I i I i I i I i IJ ij J j K k k L l L l L l L",
    "l L l N n N n N n n N n O o O o O o OE oe R r R r R r S s S s S s",
    "S s T t T t T t U u U u U u U u U u U u W w Y y Y Z z Z z Z z s"
  ), " ")[[1]]
  vapply(enc2utf8(as.character(x)), function(s) {
    cp <- utf8ToInt(s)
    chars <- vapply(cp, function(k) if (k < 128) intToUtf8(k) else if (k %in% from) to[match(k, from)] else ".", character(1))
    paste(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# The names the app gives the columns of an uploaded file: the names of
# make.names(unique = TRUE) in a UTF-8 locale, with any name that is not ASCII
# then turned into ASCII. The C locale cannot read a name that is not ASCII,
# so these names work in the model and in the shown code in every locale.
.app_csv_names <- function(header) {
  header <- enc2utf8(as.character(header))
  made <- .app_make_names(header)
  wide <- .app_has_wide(made)
  if (!any(wide) && !any(.app_has_wide(header))) {
    return(made)
  }
  made[wide] <- make.names(.app_ascii_fold(made[wide]))
  # a name of letters with no ASCII form, such as Chinese or Greek, is
  # transliterated where the system can, or else named by its column
  folded <- .app_has_wide(header) & .app_unreadable_name(made)
  made[folded] <- vapply(which(folded), function(k) {
    tr <- .app_translit(header[k])
    if (isTRUE(grepl("[A-Za-z0-9]", tr) && !grepl("?", tr, fixed = TRUE))) make.names(tr) else paste0("X", k)
  }, character(1))
  # as make.names() does, a name that did not change keeps its name
  .app_unique_names(made, header)
}

# Whether each text has a character that is not ASCII.
.app_has_wide <- function(x) vapply(x, function(s) any(utf8ToInt(s) > 127), logical(1), USE.NAMES = FALSE)

# A text in ASCII where the system can transliterate it, with "?" for each
# character it cannot.
.app_translit <- function(x) suppressWarnings(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "?"))

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
  arm <- if (spec$arm_col %in% arms) spec$arm_col else if ("group" %in% arms) "group" else if (length(arms)) arms[1] else character(0)
  arm_values <- if (length(arm)) sort(unique(as.character(stats::na.omit(data[[arm]])))) else character(0)
  arm_value <- if (as.character(spec$arm_value) %in% arm_values) as.character(spec$arm_value) else if ("treatment" %in% arm_values) "treatment" else if ("1" %in% arm_values) "1" else utils::tail(arm_values, 1)
  # a preset names its center and period columns, else the usual names are used
  center_col <- if (spec$center_col %in% others) spec$center_col else if ("center" %in% others) "center" else ""
  period_col <- if (spec$period_col %in% others) spec$period_col else if ("period" %in% others) "period" else ""
  periods <- if (nzchar(period_col)) .app_periods(data[[period_col]])$labels else character(0)
  list(
    outcomes = numeric_cols,
    components = setdiff(numeric_cols, spec$outcome),
    arms = arms, arm = arm, arm_values = arm_values, arm_value = arm_value,
    clusters = if (length(arm)) .app_cluster_choices(.app_center_columns(spec, data)$data, .app_resolve(spec)$outcome, arm, arm_value, exclude = c(spec$components, if (.app_center_level(spec)) c(spec$cl_value, spec$cl_total)), center_level = .app_center_level(spec)) else character(0),
    others = others,
    center_chars = setdiff(numeric_cols, c(spec$outcome, spec$components)),
    center_col = center_col,
    centers = if (nzchar(center_col)) sort(unique(as.character(stats::na.omit(data[[center_col]])))) else character(0),
    period_col = period_col,
    periods = periods, period = if (.app_period_in(spec$period_value, periods)) .app_period_label(spec$period_value, periods) else .app_default_period(spec, data, period_col),
    covariates = setdiff(cols, c(spec$outcome, spec$components, if (.app_center_level(spec)) c(spec$cl_value, spec$cl_total))),
    weights = setdiff(numeric_cols, c(spec$outcome, spec$components)),
    counts = numeric_cols
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

# The encoding of a file: "UTF-8-BOM" for ASCII text, which may start with a
# byte order mark, "UTF-8" for other UTF-8 text, "UTF-16" or "UTF-32" for
# UTF-16 or UTF-32 text, with or without a byte order mark, and otherwise Windows-1252, which has
# every printable Latin-1 character. A file with a byte that Windows-1252
# leaves out is read as Latin-1.
.app_file_encoding <- function(path) {
  bytes <- readBin(path, "raw", n = file.size(path))
  # UTF-32 text starts with a byte order mark of four bytes, or has three zero
  # bytes in each group of four, as each ASCII character takes four bytes
  if (length(bytes) >= 4 && (identical(bytes[1:4], as.raw(c(0xFF, 0xFE, 0, 0))) || identical(bytes[1:4], as.raw(c(0, 0, 0xFE, 0xFF))))) {
    return("UTF-32")
  }
  if (length(bytes) >= 8) {
    zero4 <- vapply(1:4, function(k) mean(bytes[seq(k, length(bytes), by = 4)] == as.raw(0)), numeric(1))
    if (sum(zero4 > 0.5) >= 3) {
      return("UTF-32")
    }
  }
  if (length(bytes) >= 2 && (identical(bytes[1:2], as.raw(c(0xFF, 0xFE))) || identical(bytes[1:2], as.raw(c(0xFE, 0xFF))))) {
    return("UTF-16")
  }
  # UTF-16 text without a byte order mark has a zero byte in most of its odd
  # or most of its even places, as each ASCII character takes two bytes
  if (length(bytes) >= 4) {
    zero <- bytes == as.raw(0)
    odd <- seq(1, length(bytes), by = 2)
    if (mean(zero[odd]) > 0.5 || mean(zero[-odd]) > 0.5) {
      return("UTF-16")
    }
  }
  bytes <- bytes[bytes != as.raw(0)]
  if (validUTF8(rawToChar(bytes))) {
    if (length(bytes) >= 3 && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) bytes <- bytes[-(1:3)]
    return(if (all(as.integer(bytes) < 128)) "UTF-8-BOM" else "UTF-8")
  }
  if (any(bytes %in% as.raw(c(0x81, 0x8D, 0x8F, 0x90, 0x9D)))) "latin1" else "CP1252"
}

# The lines of a text file in UTF-8, without a byte order mark.
.app_csv_lines <- function(path, encoding = .app_file_encoding(path)) {
  if (encoding %in% c("CP1252", "latin1")) {
    return(iconv(readLines(path, warn = FALSE), encoding, "UTF-8"))
  }
  sub("^\ufeff", "", readLines(path, warn = FALSE, encoding = "UTF-8"))
}

# Whether a file has no text but spaces, empty lines and a byte order mark.
.app_csv_blank <- function(path, encoding = .app_file_encoding(path)) {
  !isTRUE(file.size(path) > 0) || (!encoding %in% c("UTF-16", "UTF-32") && !any(nzchar(trimws(.app_csv_lines(path, encoding)))))
}

# The column names in the header of a file, as they were written.
.app_csv_header <- function(path, sep = ",", encoding = .app_file_encoding(path)) {
  tryCatch(
    names(utils::read.csv(text = .app_csv_lines(path, encoding), sep = sep, nrows = 1, check.names = FALSE, encoding = "UTF-8")),
    error = function(e) character(0)
  )
}

# The periods of a column with a row the model can use: every model column
# observed and any weight above 0.
.app_usable_periods <- function(spec, data, period_col) {
  spec <- .app_resolve(spec)
  data <- .app_center_columns(spec, data)$data
  model <- c(
    spec$outcome, spec$components, spec$covariates, period_col,
    if (identical(spec$center_mode, "effects")) spec$center_col,
    if (identical(spec$center_mode, "chars")) spec$center_chars
  )
  rows <- .app_fitted_rows(spec, data, model)
  unique(as.character(data[[period_col]])[rows & !is.na(data[[period_col]])])
}

# The period to optimize for by default: the latest one with a row the model
# can use, or the latest one when none has.
.app_default_period <- function(spec, data, period_col) {
  if (!isTRUE(period_col %in% names(data))) {
    return(character(0))
  }
  periods <- .app_periods(data[[period_col]])$labels
  usable <- intersect(periods, .app_usable_periods(spec, data, period_col))
  utils::tail(if (length(usable)) usable else periods, 1)
}

# Read an uploaded CSV with the separator and decimal mark the user chose.
# The text is turned into UTF-8 before it is parsed, so that no locale, such as
# C, stops the read at an accented letter.
.app_read_csv <- function(path, sep = ",", dec = ".", encoding = .app_file_encoding(path)) {
  if (encoding %in% c("UTF-16", "UTF-32")) {
    stop(sprintf("The file is %s text. Save it as UTF-8 and read it again.", encoding), call. = FALSE)
  }
  d <- utils::read.csv(
    text = .app_csv_lines(path, encoding),
    sep = sep, dec = dec, na.strings = c("", "NA"),
    check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE
  )
  names(d) <- .app_csv_names(names(d))
  d
}

.app_center_level <- function(spec) identical(spec$structure, "center")

# Center level data has a binary outcome, read from the proportion column the
# app builds.
.app_resolve <- function(spec) {
  if (.app_center_level(spec)) {
    spec$outcome <- "proportion"
    spec$outcome_type <- "binary"
  }
  spec
}

# The outcome of center level data as the package reads it: a column named
# proportion, and the total of each row in a column named center_sample_size.
# Returns the code lines and the data with both columns, when the columns are
# chosen and numeric.
.app_center_columns <- function(spec, data) {
  if (!.app_center_level(spec) || !nzchar(spec$cl_value) || !nzchar(spec$cl_total) ||
    !all(c(spec$cl_value, spec$cl_total) %in% names(data)) ||
    !is.numeric(data[[spec$cl_value]]) || !is.numeric(data[[spec$cl_total]])) {
    return(list(data = data, code = character(0)))
  }
  rstr <- function(x) deparse1(x)
  code <- character(0)
  if (identical(spec$cl_kind, "successes")) {
    data$proportion <- data[[spec$cl_value]] / data[[spec$cl_total]]
    code <- c(code, sprintf("data$proportion <- data[[%s]] / data[[%s]]", rstr(spec$cl_value), rstr(spec$cl_total)))
  } else if (spec$cl_value != "proportion") {
    data$proportion <- data[[spec$cl_value]]
    code <- c(code, sprintf("data$proportion <- data[[%s]]", rstr(spec$cl_value)))
  }
  if (spec$cl_total != "center_sample_size") {
    data$center_sample_size <- data[[spec$cl_total]]
    code <- c(code, sprintf("data$center_sample_size <- data[[%s]]", rstr(spec$cl_total)))
  }
  list(data = data, code = code)
}

# The rows glm() fits: every model column observed, and for weights or center
# level totals a weight above 0, as a row with a weight of 0 or none is left out.
.app_fitted_rows <- function(spec, data, model_cols) {
  rows <- stats::complete.cases(data[intersect(model_cols, names(data))])
  w <- if (nzchar(spec$weights_col) && spec$weights_col %in% names(data)) data[[spec$weights_col]]
  if (is.numeric(w)) rows <- rows & !is.na(w) & w > 0
  if (.app_center_level(spec) && is.numeric(data$center_sample_size)) rows <- rows & data$center_sample_size > 0
  rows
}

# The centers in the order the package gives their weights: the levels of the
# center factor once unused levels are dropped.
.app_center_levels <- function(data, center_col) {
  if (!isTRUE(center_col %in% names(data))) {
    return(character(0))
  }
  x <- data[[center_col]]
  levels(droplevels(if (is.factor(x)) x else factor(x)))
}

# The weight of each center as the package sets it by default: its share of
# the patients, which for center level data is the sum of its totals.
.app_center_weights_default <- function(spec, data) {
  centers <- .app_center_levels(data, spec$center_col)
  if (!length(centers)) {
    return(numeric(0))
  }
  f <- factor(as.character(data[[spec$center_col]]), levels = centers)
  size <- if (.app_center_level(spec) && isTRUE(spec$cl_total %in% names(data))) {
    tapply(data[[spec$cl_total]], f, sum, na.rm = TRUE)
  } else {
    as.numeric(table(f))
  }
  size[!is.finite(size)] <- 0
  # a center whose rows glm() cannot fit gets 0, as the package requires
  resolved <- .app_resolve(spec)
  built <- .app_center_columns(spec, data)$data
  model <- c(resolved$outcome, spec$components, spec$covariates, if (isTRUE(spec$time_on)) spec$period_col)
  fits <- .app_fitted_rows(resolved, built, model)
  size[!centers %in% unique(as.character(data[[spec$center_col]])[fits])] <- 0
  if (sum(size) <= 0) {
    return(rep(1 / length(centers), length(centers)))
  }
  stats::setNames(as.numeric(size) / sum(size), centers)
}

# The prepared data, the lago_optimization() arguments and the R code for a
# spec. `data` is the dataset as loaded. Columns the package reads by a fixed
# name (group, center and period) and the interaction columns are built on a
# copy, and the same lines are added to the code.
.app_build <- function(spec, data) {
  spec <- .app_resolve(spec)
  d <- data
  prep <- character(0)
  add <- function(line, value_fun) {
    prep[[length(prep) + 1L]] <<- line
    value_fun()
  }
  rstr <- function(x) deparse1(x)

  cl <- .app_center_columns(spec, data)
  for (line in cl$code) add(line, function() NULL)
  d <- cl$data

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
      period_value <- match(.app_period_label(spec$period_value, periods$labels), periods$labels)
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
  if (.app_center_level(spec)) {
    args <- c(list(input_data_structure = "center_level"), args)
    # the package fits center effects to center level data unless told not to
    if (!identical(spec$center_mode, "effects")) args$include_center_effects <- FALSE
  }
  if (identical(spec$center_mode, "effects")) {
    args$include_center_effects <- TRUE
    if (nzchar(spec$center_for)) {
      args$center_effects_optimization_values <- spec$center_for
    } else if (isTRUE(spec$cw_on)) {
      args$center_weights_for_outcome_goal <- spec$cw
    }
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
  # the code reads the weights from the data, as the run does
  exprs <- list()
  if (identical(spec$outcome_type, "continuous") && !identical(spec$glm_family, "default")) args$glm_family <- spec$glm_family
  if (!identical(spec$link, "default")) args$link <- spec$link
  if (nzchar(spec$weights_col)) {
    args$weights <- d[[spec$weights_col]]
    exprs$weights <- sprintf("data[[%s]]", rstr(spec$weights_col))
  }
  if (identical(spec$method, "grid_search")) {
    args$optimization_method <- "grid_search"
    args$optimization_grid_search_step_size <- .app_steps(spec$grid_steps, spec$lower, spec$upper)
  }
  if (isTRUE(spec$include_cs)) {
    args$confidence_set_grid_step_size <- .app_steps(spec$cs_steps, spec$lower, spec$upper)
    if (!isTRUE(all.equal(spec$cs_level, 0.95))) args$confidence_set_alpha <- signif(1 - spec$cs_level, 6)
  } else {
    args$include_confidence_set <- FALSE
  }
  if (isTRUE(spec$prev_on)) args$prev_recommended_interventions <- spec$prev

  list(data = d, args = args, prep = prep, code = .app_code(spec$data_name, prep, args, exprs = exprs))
}

# The grid step of each component: the one typed, or 1/20 of its range.
.app_steps <- function(typed, lower, upper) {
  steps <- signif(.app_cs_steps(lower, upper), 4)
  if (length(typed) == length(steps)) steps[is.finite(typed)] <- typed[is.finite(typed)]
  steps
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
.app_code <- function(data_name, prep, args, call = "lago_optimization", result = "result", exprs = list()) {
  # `exprs` gives the code for an argument whose value comes from the data,
  # such as data[["n"]] for the weights, instead of the values themselves
  arg_lines <- vapply(names(args), function(nm) {
    value <- if (!is.null(exprs[[nm]])) exprs[[nm]] else paste(deparse(args[[nm]], width.cutoff = 500L), collapse = " ")
    sprintf("  %s = %s", nm, value)
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

# The columns the app builds for the package, which reads them by these names,
# so a column of the data with such a name is replaced.
.app_built_columns <- function(spec, data) {
  built <- character(0)
  if (.app_center_level(spec)) {
    if (identical(spec$cl_kind, "successes") || !identical(spec$cl_value, "proportion")) built <- c(built, "proportion")
    if (!identical(spec$cl_total, "center_sample_size")) built <- c(built, "center_sample_size")
  }
  if (.app_power_on(spec) && .app_needs_group(spec, data)) built <- c(built, "group")
  if (identical(spec$center_mode, "effects") && nzchar(spec$center_col) && spec$center_col != "center") built <- c(built, "center")
  if (isTRUE(spec$time_on) && nzchar(spec$period_col) && spec$period_col != "period") built <- c(built, "period")
  built
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
  if (.app_center_level(spec)) {
    value_name <- if (identical(spec$cl_kind, "successes")) "successes" else "proportion"
    v <- if (nzchar(spec$cl_value)) data[[spec$cl_value]]
    n <- if (nzchar(spec$cl_total)) data[[spec$cl_total]]
    if (is.null(v)) {
      note("outcome", sprintf("Choose the column with the %s of each row.", value_name))
    } else if (!is.numeric(v)) {
      note("outcome", sprintf("The %s column '%s' must be numeric.", value_name, spec$cl_value))
    }
    if (is.null(n)) {
      note("outcome", "Choose the column with the total, the number of patients each row stands for.")
    } else if (!is.numeric(n)) {
      note("outcome", sprintf("The total column '%s' must be numeric.", spec$cl_total))
    } else if (any(is.infinite(n)) || any(n <= 0, na.rm = TRUE)) {
      note("outcome", sprintf("Every total in '%s' must be a number above 0.", spec$cl_total))
    }
    if (is.numeric(v) && is.numeric(n)) {
      if (identical(spec$cl_kind, "successes") && (any(v < 0, na.rm = TRUE) || any(v > n, na.rm = TRUE))) {
        note("outcome", sprintf("The successes in '%s' must be 0 or more and at most the total of their row.", spec$cl_value))
      } else if (identical(spec$cl_kind, "proportion") && any(v < 0 | v > 1, na.rm = TRUE)) {
        note("outcome", sprintf("The proportions in '%s' must be between 0 and 1.", spec$cl_value))
      }
    }
    if (any(c(spec$cl_value, spec$cl_total) %in% spec$components)) {
      note("intervention", "The successes, proportion or total column cannot also be an intervention component.")
    }
    if (any(c(spec$cl_value, spec$cl_total) %in% c(spec$covariates, spec$center_chars))) {
      note("centers", "The successes, proportion or total column cannot also be a covariate or a center characteristic.")
    }
    # the package weighs the average center by its totals, so each center needs one
    if (is.numeric(n) && identical(spec$center_mode, "effects") && !nzchar(spec$center_for) && !isTRUE(spec$cw_on) &&
      isTRUE(spec$center_col %in% names(data))) {
      centers <- as.character(data[[spec$center_col]])
      no_total <- sort(unique(centers[!is.na(centers)][tapply(is.na(n), centers, all)[centers[!is.na(centers)]]]))
      if (length(no_total)) {
        note("outcome", sprintf(
          "The center %s has no total on any row, so it has no size to weigh it by. Fill in its totals, or set the weight of each center in step 4.",
          paste(no_total, collapse = ", ")
        ))
      }
    }
    if (nzchar(spec$weights_col)) {
      note("outcome", "Center level data does not take weights, as each row is weighted by its total. Choose No weights.")
    }
  }
  # a column the app builds would replace a chosen column of the same name
  built <- .app_built_columns(spec, data)
  roles <- list(
    "the outcome" = if (!.app_center_level(spec)) spec$outcome,
    "an intervention component" = spec$components, "a covariate" = spec$covariates,
    "a center characteristic" = spec$center_chars, "the weights" = spec$weights_col,
    "the cluster column" = if (.app_power_on(spec)) spec$cluster_col
  )
  for (role in names(roles)) {
    for (nm in intersect(roles[[role]], built)) {
      note(if (role %in% c("the outcome", "the weights", "the cluster column")) "outcome" else if (role == "an intervention component") "intervention" else "centers", sprintf(
        "The column %s is chosen as %s, but the app builds its own column named %s for the package, which reads that name. Rename the column in your data, or choose another one.",
        nm, role, nm
      ))
    }
  }
  # the app builds its columns in the order of `built`, so a column that one
  # of them reads must not be one that an earlier one replaced
  sources <- list(
    center_sample_size = list(col = spec$cl_total, role = "the total column", step = "outcome"),
    group = list(col = spec$arm_col, role = "the treatment arm column", step = "outcome"),
    center = list(col = spec$center_col, role = "the center column", step = "centers"),
    period = list(col = spec$period_col, role = "the period column", step = "centers")
  )
  for (target in intersect(names(sources), built)) {
    src <- sources[[target]]
    earlier <- built[seq_len(match(target, built) - 1L)]
    if (src$col %in% earlier) {
      note(src$step, sprintf(
        "The column %s is chosen as %s, but the app first builds its own column named %s for the package, which reads that name. Rename the column in your data, or choose another one.",
        src$col, src$role, src$col
      ))
    }
  }
  # the package puts these columns in the model formula, which needs valid R
  # names other than the ones R reserves, such as ..1 and ..., and a formula
  # reads a column named . as all the other columns
  formula_cols <- c(if (!.app_center_level(spec)) spec$outcome, spec$components, spec$covariates, spec$center_chars)
  reserved <- formula_cols == "." | grepl("^[.][.]([.]|[0-9]+)$", formula_cols)
  bad_names <- unique(formula_cols[nzchar(formula_cols) & (formula_cols != make.names(formula_cols) | reserved)])
  if (length(bad_names)) {
    better <- .app_csv_names(bad_names)
    better[.app_unreadable_name(better)] <- paste0("X", better[.app_unreadable_name(better)])
    note("data", sprintf(
      "The column name(s) %s are not valid R names here, or are names R reserves, so the model cannot use them. Rename them in R to names of ASCII letters, digits, dots and underscores, such as %s.",
      paste0("'", bad_names, "'", collapse = ", "), paste0("'", better, "'", collapse = ", ")
    ))
  }

  spec <- .app_resolve(spec)
  data <- .app_center_columns(spec, data)$data

  y <- if (nzchar(spec$outcome) && spec$outcome %in% names(data)) data[[spec$outcome]]
  if (is.null(y)) {
    # for center level data the columns above say what to choose
    if (!.app_center_level(spec)) note("outcome", "Choose the outcome column.")
  } else if (identical(spec$outcome_type, "binary") && !.app_center_level(spec) && !.app_is_binary(y)) {
    note("outcome", sprintf("The outcome '%s' is not 0/1, so it cannot be binary. Choose Continuous or another column.", spec$outcome))
  }
  # the proportion model and the logit link both read the outcome as a share
  proportion <- identical(spec$outcome_type, "continuous") && identical(spec$glm_family, "quasibinomial")
  logit <- identical(spec$outcome_type, "continuous") && identical(spec$link, "logit")
  if (!is.null(y) && (proportion || logit) && any(y < 0 | y > 1, na.rm = TRUE)) {
    note("outcome", if (proportion) {
      sprintf("The proportion model needs an outcome between 0 and 1, and '%s' goes outside it. Choose the normal model.", spec$outcome)
    } else {
      sprintf("The logit link needs an outcome between 0 and 1, and '%s' goes outside it. Choose the usual link or identity.", spec$outcome)
    })
  }
  # only the rows glm() fits count, as it leaves out a row with a missing value
  fit_cols <- c(
    spec$outcome, spec$components, spec$covariates, if (isTRUE(spec$time_on)) spec$period_col,
    if (identical(spec$center_mode, "effects")) spec$center_col,
    if (identical(spec$center_mode, "chars")) spec$center_chars
  )
  if (!is.null(y) && logit && !proportion && !any(y < 0 | y > 1, na.rm = TRUE) &&
    any(y[.app_fitted_rows(spec, data, fit_cols)] %in% c(0, 1))) {
    note("outcome", sprintf(
      "The logit link with the normal model needs an outcome strictly between 0 and 1, and '%s' has values of exactly 0 or 1. Choose the proportion model (quasibinomial), which takes 0 and 1.",
      spec$outcome
    ))
  }
  if ((proportion || logit) && !identical(spec$goal_mode, "power") && is.finite(spec$goal) && (spec$goal <= 0 || spec$goal >= 1)) {
    note("outcome", "A goal for an outcome between 0 and 1 must be between 0 and 1, such as 0.6 for 60%.")
  }
  if (nzchar(spec$weights_col)) {
    w <- data[[spec$weights_col]]
    if (is.null(w) || !is.numeric(w)) {
      note("outcome", "The weights column must be numeric.")
    } else if (spec$weights_col %in% c(spec$outcome, spec$components)) {
      note("outcome", "The weights column cannot also be the outcome or a component.")
    } else if (any(is.infinite(w)) || any(w < 0, na.rm = TRUE) || !any(w > 0, na.rm = TRUE)) {
      note("outcome", sprintf("The weights in '%s' must be finite numbers of 0 or more, with at least one above 0.", spec$weights_col))
    }
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
    bounds_ok <- all(is.finite(spec$lower), is.finite(spec$upper)) && all(spec$lower <= spec$upper)
    check_steps <- function(typed, what) {
      # an empty step is fine, as it takes 1/20 of the range
      typed <- c(typed, rep(NA_real_, length(comps)))[seq_along(comps)]
      bad <- comps[!is.na(typed) & !(is.finite(typed) & typed > 0)]
      if (length(bad)) note("intervention", sprintf("The %s step of %s must be above 0. Leave it empty for 1/20 of the range.", what, paste(bad, collapse = ", ")))
      !length(bad)
    }
    if (identical(spec$method, "grid_search") && check_steps(spec$grid_steps, "grid search") && bounds_ok) {
      size <- .app_cs_grid_size(spec$lower, spec$upper, .app_steps(spec$grid_steps, spec$lower, spec$upper))
      if (size > 1e5) {
        note("intervention", sprintf("The grid search would try %s interventions. Use larger steps, fewer components, or the numerical optimizer.", format(size, big.mark = ",")))
      }
    }
    if (isTRUE(spec$include_cs) && check_steps(spec$cs_steps, "confidence set") && bounds_ok) {
      size <- .app_cs_grid_size(spec$lower, spec$upper, .app_steps(spec$cs_steps, spec$lower, spec$upper))
      if (size > 1e5) {
        note("intervention", sprintf("The confidence set grid would have %s points. Use larger steps, fewer components, or turn off the confidence set.", format(size, big.mark = ",")))
      }
    }
    if (isTRUE(spec$prev_on)) {
      for (k in seq_along(comps)) {
        v <- if (k <= length(spec$prev)) spec$prev[k] else NA_real_
        if (!is.finite(v)) {
          note("intervention", sprintf("Enter the previous stage's level of %s, or turn off the previous stage.", comps[k]))
        } else if (is.finite(spec$lower[k]) && is.finite(spec$upper[k]) && (v < spec$lower[k] || v > spec$upper[k])) {
          note("intervention", sprintf("The previous stage's level of %s (%s) must be within its bounds.", comps[k], fmt(v)))
        }
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
      used <- intersect(c(
        spec$outcome, comps, spec$covariates, if (isTRUE(spec$time_on)) spec$period_col,
        if (.app_center_level(spec)) "center_sample_size"
      ), names(data))
      complete <- .app_fitted_rows(spec, data, used)
      centers <- as.character(data[[spec$center_col]])
      fitted <- unique(centers[complete & !is.na(centers)])
      if (length(fitted) < 2) {
        note("centers", "Fixed center effects need at least two centers with a row the model can use, where every model column is observed and any weight is above 0.")
      } else if (nzchar(spec$center_for) && !spec$center_for %in% fitted) {
        note("centers", sprintf("The center %s has no row the model can use, where every model column is observed and any weight is above 0, so it cannot be optimized for. Choose another center.", spec$center_for))
      } else if (!nzchar(spec$center_for) && isTRUE(spec$cw_on)) {
        centers_all <- .app_center_levels(data, spec$center_col)
        w <- spec$cw
        if (length(w) != length(centers_all) || any(!is.finite(w)) || any(w < 0)) {
          note("centers", "Enter a weight of 0 or more for every center.")
        } else if (abs(sum(w) - 1) >= 0.001) {
          note("centers", sprintf("The center weights add up to %s. They must add up to 1.", format(signif(sum(w), 4))))
        } else if (any(w > 0 & !centers_all %in% fitted)) {
          note("centers", sprintf(
            "The center %s has no row the model can use, where every model column is observed and any weight is above 0, so its weight must be 0.",
            paste(centers_all[w > 0 & !centers_all %in% fitted], collapse = ", ")
          ))
        }
      }
    }
  }
  if (isTRUE(spec$time_on)) {
    if (!nzchar(spec$period_col)) {
      note("centers", "Choose the column that holds the time period.")
    } else if (!nzchar(spec$period_value)) {
      note("centers", "Choose the period to optimize for.")
    } else if (spec$period_col %in% names(data) && length(.app_usable_periods(spec, data, spec$period_col)) < 2) {
      note("centers", sprintf(
        "Fixed time effects need at least two periods with a row the model can use, where every model column is observed and any weight is above 0, and %s has fewer. Turn off the time effects, or choose another period column.",
        spec$period_col
      ))
    } else if (spec$period_col %in% names(data) && !.app_period_in(spec$period_value, .app_usable_periods(spec, data, spec$period_col))) {
      note("centers", sprintf(
        "The period %s has no row the model can use, where every model column is observed and any weight is above 0, so it cannot be optimized for. Choose another period.",
        spec$period_value
      ))
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
      } else if (!is.null(y)) {
        # the power is computed from the rows with an observed outcome, and for
        # center level data only from rows with patients
        seen <- !is.na(y)
        if (.app_center_level(spec) && is.numeric(data$center_sample_size)) {
          seen <- seen & !is.na(data$center_sample_size) & data$center_sample_size > 0
        }
        treated <- arm == as.character(spec$arm_value)
        empty <- c(treatment = !any(seen & treated), control = !any(seen & !treated))
        for (side in names(empty)[empty]) {
          note("outcome", sprintf(
            "The %s arm has no row with an observed outcome, and the power goal needs one in each arm.", side
          ))
        }
      }
    }
    if (!is.finite(spec$power_goal) || spec$power_goal <= 0 || spec$power_goal >= 1) {
      note("outcome", "The power goal must be above 0 and below 1, such as 0.8.")
    }
    if (!is.finite(spec$n_centers) || spec$n_centers <= 0 || !is.finite(spec$n_patients) || spec$n_patients <= 0) {
      note("outcome", "Enter the number of centers and patients per center in the next stage.")
    } else if (spec$n_centers != round(spec$n_centers) || spec$n_patients != round(spec$n_patients)) {
      note("outcome", "The number of centers and the patients per center in the next stage must be whole numbers.")
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
  spec <- .app_resolve(spec)
  notes <- character(0)
  replaced <- function(name, from) {
    if (name %in% names(data) && !identical(from, name)) {
      notes[[length(notes) + 1L]] <<- sprintf(
        "Your data already has a column named %s. The app replaces it with one built from %s, as the package reads that name.",
        name, from
      )
    }
  }
  if (.app_center_level(spec)) {
    if (identical(spec$cl_kind, "successes") && nzchar(spec$cl_value) && nzchar(spec$cl_total)) {
      replaced("proportion", paste(spec$cl_value, "and", spec$cl_total))
    } else if (nzchar(spec$cl_value)) {
      replaced("proportion", spec$cl_value)
    }
    if (nzchar(spec$cl_total)) replaced("center_sample_size", spec$cl_total)
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
# an observed outcome in each arm. For one row per patient, a column with a
# different value on every row names the patients and not the centers. For
# center level data a row stands for a center, and counts only with patients
# in it.
.app_cluster_choices <- function(data, outcome, arm_col, arm_value, exclude = character(0), center_level = FALSE) {
  if (length(outcome) != 1 || length(arm_col) != 1 || length(arm_value) != 1 ||
    !all(c(outcome, arm_col) %in% names(data))) {
    return(character(0))
  }
  treated <- as.character(data[[arm_col]]) == as.character(arm_value)
  seen <- !is.na(data[[outcome]]) & !is.na(treated)
  size <- data$center_sample_size
  if (center_level && is.numeric(size)) seen <- seen & !is.na(size) & size > 0
  candidates <- setdiff(names(data), c(outcome, arm_col, exclude, if (center_level) "center_sample_size"))
  ok <- vapply(candidates, function(cc) {
    v <- data[[cc]]
    in_arm <- function(arm) length(unique(stats::na.omit(v[seen & treated == arm])))
    in_arm(TRUE) >= 2 && in_arm(FALSE) >= 2 && (center_level || length(unique(stats::na.omit(v))) < nrow(data))
  }, logical(1))
  candidates[ok]
}

# A message for a CSV that was read but probably with the wrong settings.
.app_csv_hint <- function(d, sep, dec) {
  if (ncol(d) == 1) {
    return("The file was read as one column. Choose the separator the file uses, such as Semicolon.")
  }
  text_cols <- names(d)[vapply(d, is.character, logical(1))]
  values <- lapply(d[text_cols], function(v) unique(trimws(stats::na.omit(v))))
  esc <- function(mark) if (identical(mark, ".")) "\\." else mark
  other <- if (identical(dec, ".")) "," else "."
  # a whole number, and a number with the decimal mark chosen or the other one
  whole <- "^-?[0-9]+$"
  number <- sprintf("^-?([0-9]+(%s[0-9]*)?|%s[0-9]+)$", esc(dec), esc(dec))
  number_other <- sprintf("^-?[0-9]*%s[0-9]+$", esc(other))
  # the other mark between groups of three digits, as in 1,200 or 1.200,5,
  # where the first group is not 0, as 0,125 is no number of thousands
  thousands <- sprintf("^-?[1-9][0-9]{0,2}(%s[0-9]{3})+(%s[0-9]+)?$", esc(other), esc(dec))
  # a whole number or one with three decimals after the other mark, such as
  # 1,250, which may be a decimal number as well as a number of thousands
  three_decimals <- sprintf("^-?[0-9]+(%s[0-9]{3})?$", esc(other))
  # every value fits one of the patterns, and some value fits the last one
  looks <- function(patterns) {
    vapply(values, function(v) {
      length(v) > 0 && all(Reduce(`|`, lapply(patterns, grepl, x = v))) && any(grepl(patterns[[length(patterns)]], v))
    }, logical(1))
  }
  ambiguous <- vapply(values, function(v) all(grepl(three_decimals, v)), logical(1))
  thousands_like <- looks(list(whole, number, thousands))
  as_thousands <- thousands_like & !ambiguous
  as_other <- looks(list(whole, number_other)) & !as_thousands
  as_dot_na <- looks(list(number, "^\\.$")) & !as_thousands & !as_other
  cols <- function(hit) paste(text_cols[hit], collapse = ", ")
  hints <- c(
    if (any(as_thousands)) {
      example <- grep(thousands, unlist(values[as_thousands]), value = TRUE)[1]
      sprintf(
        "The column(s) %s look like numbers with a %s between the thousands, such as %s, which the app cannot read as numbers. Save the file without separators between the thousands and upload it again.",
        cols(as_thousands), if (identical(other, ",")) "comma" else "point", example
      )
    },
    if (any(as_other)) {
      sprintf(
        "The column(s) %s look like numbers with a decimal %s. Choose %s as the decimal mark.%s",
        cols(as_other), if (identical(other, ",")) "comma" else "point", if (identical(other, ",")) "Comma" else "Point",
        if (any(as_other & ambiguous & thousands_like)) {
          sprintf(" If the %s is between the thousands instead, save the file without separators between the thousands and upload it again.", if (identical(other, ",")) "comma" else "point")
        } else {
          ""
        }
      )
    },
    if (any(as_dot_na)) {
      sprintf(
        "The column(s) %s use a dot for a missing value, so they were read as text. Leave those cells empty or write NA in them, and upload the file again.",
        cols(as_dot_na)
      )
    }
  )
  if (length(hints)) paste(hints, collapse = " ") else NULL
}

# Header names the reader changed to valid R names, as "old (now new)".
.app_renamed <- function(path, sep, d, encoding = .app_file_encoding(path)) {
  if (ncol(d) < 2) {
    return(character(0))
  }
  raw <- .app_csv_header(path, sep, encoding)
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

# The error of a run, with a hint when the identity link could not be fitted.
.app_explain_error <- function(error, link) {
  if (!is.null(error) && identical(link, "identity") && grepl("no valid set of coefficients", error, fixed = TRUE)) {
    error <- paste(
      "The model could not be fitted with the identity link, which can give a probability below 0 or above 1.",
      "Choose the logit link, or the usual link, in the outcome model settings of step 2.",
      sprintf("The package said: %s", error)
    )
  }
  error
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
# them. Cost coefficients open in the cubic form when any component has 5 of
# them, where a linear cost of 2 is the same curve padded with zeros, and in
# the linear form otherwise. A component whose coefficients fit neither form
# opens on the designer's own curve, and is named in `unfit`. Unit costs open
# on the curve the package computes from them. `by_form` has the costs in use
# for each form where they fit it, so a switch of the form keeps them: a
# linear cost is a cubic one padded with zeros, and a cubic cost of degree 1
# is a linear one.
.app_designer_start <- function(spec) {
  comps <- spec$components
  if (identical(spec$cost_mode, "coef")) {
    lens <- vapply(spec$coefs, length, integer(1))
    form <- if (any(lens == 5L)) "cubic" else "linear"
    want <- if (form == "cubic") 5L else 2L
    coefs <- lapply(spec$coefs, function(v) {
      if (length(v) == want) v else if (want == 5L && length(v) == 2L) c(v, 0, 0, 0) else NULL
    })
    fits <- !vapply(coefs, is.null, logical(1))
    # the average cost per unit over the bounds, for the designer's ranges
    unit_costs <- vapply(seq_along(comps), function(k) {
      v <- spec$coefs[[k]]
      lo <- spec$lower[k]
      up <- spec$upper[k]
      if (is.null(v) || !is.finite(lo) || !is.finite(up) || up <= lo) {
        return(1)
      }
      total <- function(x) sum(v * x^(seq_along(v) - 1))
      per_unit <- (total(up) - total(lo)) / (up - lo)
      if (is.finite(per_unit) && per_unit > 0) per_unit else 1
    }, numeric(1))
    unfit <- comps[!fits]
  } else {
    form <- spec$cost_type
    unit_costs <- ifelse(is.finite(spec$unit_costs) & spec$unit_costs >= 0, spec$unit_costs, 1)
    doses <- if (identical(form, "cubic") && any(!is.na(spec$doses)) && !anyNA(spec$doses)) spec$doses else NULL
    coefs <- tryCatch(
      cost_fxn_calculator(spec$lower, spec$upper, unit_costs, form, unit_cost_doses = doses),
      error = function(e) NULL
    )
    unfit <- character(0)
  }
  in_use <- if (identical(spec$cost_mode, "coef")) spec$coefs else coefs
  by_form <- list(
    linear = lapply(in_use, function(v) {
      if (length(v) == 2L) v else if (length(v) == 5L && isTRUE(all(v[3:5] == 0))) v[1:2]
    }),
    cubic = lapply(in_use, function(v) if (length(v) == 5L) v else if (length(v) == 2L) c(v, 0, 0, 0))
  )
  list(form = form, unit_costs = unit_costs, coefs = coefs, unfit = unfit, by_form = by_form)
}

# The inputs a sensitivity sweep can vary for these settings, as label =
# argument. Each is offered only when the run uses it.
.app_sweep_choices <- function(spec) {
  out <- character(0)
  if (!identical(spec$goal_mode, "power")) out["Outcome goal"] <- "outcome_goal"
  out["All costs, by a multiplier"] <- "cost_multiplier"
  if (.app_power_on(spec)) {
    out["Power goal"] <- "power_goal"
    out["Centers in the next stage"] <- "num_centers_in_next_stage"
    out["Patients per center"] <- "patients_per_center_in_next_stage"
    # an ICC above 0 needs the center column of the power goal
    if (nzchar(spec$cluster_col)) out["ICC"] <- "icc"
  }
  if (isTRUE(spec$time_on)) out["Period to optimize for"] <- "time_effect_optimization_value"
  out
}

.app_sweep_label <- function(param) {
  labels <- c(
    outcome_goal = "Outcome goal", cost_multiplier = "Cost multiplier", power_goal = "Power goal",
    num_centers_in_next_stage = "Centers in the next stage", patients_per_center_in_next_stage = "Patients per center",
    icc = "ICC", time_effect_optimization_value = "Period"
  )
  if (param %in% names(labels)) labels[[param]] else param
}

# Where a sweep of `param` starts: From, To and Steps around the current
# value, or for the period the periods to choose from.
.app_sweep_defaults <- function(param, spec, data) {
  clamp <- function(x, lo, hi) min(max(x, lo), hi)
  around <- function(x, by, lo, hi) list(from = signif(clamp(x - by, lo, hi), 3), to = signif(clamp(x + by, lo, hi), 3), steps = 5L)
  binary <- identical(spec$outcome_type, "binary")
  switch(param,
    outcome_goal = {
      y <- suppressWarnings(as.numeric(data[[spec$outcome]]))
      span <- if (binary) 1 else diff(range(y[is.finite(y)]))
      goal <- if (is.finite(spec$goal)) spec$goal else 0
      if (!is.finite(span) || span <= 0) span <- max(abs(goal), 1)
      if (binary) around(goal, 0.15, 0.01, 0.99) else around(goal, 0.15 * span, -Inf, Inf)
    },
    cost_multiplier = list(from = 0.5, to = 1.5, steps = 5L),
    power_goal = around(if (is.finite(spec$power_goal)) spec$power_goal else 0.8, 0.1, 0.05, 0.99),
    num_centers_in_next_stage = ,
    patients_per_center_in_next_stage = {
      n <- if (param == "num_centers_in_next_stage") spec$n_centers else spec$n_patients
      if (!is.finite(n)) n <- 10
      lo <- max(if (param == "num_centers_in_next_stage") 2 else 1, round(n / 2))
      list(from = lo, to = max(round(n * 1.5), lo + 4), steps = 5L)
    },
    icc = list(from = 0, to = signif(max(0.1, 2 * suppressWarnings(max(spec$icc, na.rm = TRUE))), 2), steps = 5L),
    time_effect_optimization_value = list(periods = .app_period_values(data, spec$period_col)$labels)
  )
}

# The periods of a column, and the numbers the model knows them by: the period
# itself when it is a number, else its place in time order.
.app_period_values <- function(data, period_col) {
  if (!isTRUE(period_col %in% names(data))) {
    return(list(labels = character(0), values = numeric(0)))
  }
  p <- .app_periods(data[[period_col]])
  list(labels = p$labels, values = if (p$numeric) as.numeric(p$labels) else seq_along(p$labels))
}

# The values of a period sweep, from the periods chosen by name.
.app_sweep_period_values <- function(chosen, data, period_col) {
  p <- .app_period_values(data, period_col)
  keep <- p$labels %in% chosen
  if (sum(keep) < 2) {
    return(list(values = NULL, labels = NULL, error = "Choose at least two periods."))
  }
  list(values = p$values[keep], labels = p$labels[keep], error = NULL)
}

# The values of a sweep, from even steps or a typed list, with the problem to
# fix when they do not work for `param`.
.app_sweep_values <- function(param, how, from, to, steps, typed, outcome_type = "binary") {
  bad <- function(msg) list(values = NULL, error = msg)
  if (identical(how, "typed")) {
    values <- .app_parse_coefs(typed)
    if (is.null(values)) {
      return(bad("Type the values as numbers separated by commas, such as 0.7, 0.8, 0.9."))
    }
    values <- unique(values)
  } else {
    if (!is.finite(from) || !is.finite(to)) {
      return(bad("Give a From and a To value."))
    }
    if (from == to) {
      return(bad("From and To must differ."))
    }
    if (!is.finite(steps) || steps != round(steps) || steps < 2 || steps > 25) {
      return(bad("Steps must be a whole number from 2 to 25."))
    }
    values <- seq(from, to, length.out = steps)
    if (param %in% c("num_centers_in_next_stage", "patients_per_center_in_next_stage")) values <- unique(round(values))
    values <- signif(values, 6)
  }
  if (length(values) > 25) {
    return(bad("A sweep takes at most 25 values."))
  }
  if (length(values) < 2) {
    return(bad("A sweep needs at least two different values."))
  }
  msg <- switch(param,
    outcome_goal = if (identical(outcome_type, "binary") && any(values <= 0 | values >= 1)) "A goal for a binary outcome must be between 0 and 1.",
    cost_multiplier = if (any(values <= 0)) "Cost multipliers must be above 0.",
    power_goal = if (any(values <= 0 | values >= 1)) "A power goal must be between 0 and 1.",
    num_centers_in_next_stage = if (any(values < 2 | values != round(values))) "The number of centers must be a whole number of 2 or more.",
    patients_per_center_in_next_stage = if (any(values < 1 | values != round(values))) "The number of patients must be a whole number of 1 or more.",
    icc = if (any(values < 0 | values >= 1)) "An ICC must be 0 or more and below 1.",
    NULL
  )
  if (!is.null(msg)) bad(msg) else list(values = values, error = NULL)
}

# The arguments a sweep adds to the stored call of the result. A cost
# multiplier scales the cost coefficients, which the result holds even when it
# was run with unit costs. An ICC above 0 needs the center column.
.app_sweep_extra <- function(param, spec, result) {
  extra <- list()
  if (identical(param, "cost_multiplier") && identical(spec$cost_mode, "unit")) {
    extra$cost_list_of_vectors <- result$cost_list_of_vectors
  }
  if (identical(param, "icc")) extra$power_goal_cluster_id <- spec$cluster_col
  extra
}

# The R code of a sweep and a budget search, run on `result` from the code above.
.app_sweep_code <- function(param, values, spec) {
  extra <- c(
    if (identical(param, "cost_multiplier") && identical(spec$cost_mode, "unit")) "  cost_list_of_vectors = result$cost_list_of_vectors",
    if (identical(param, "icc")) sprintf("  power_goal_cluster_id = %s", deparse1(spec$cluster_col))
  )
  c(
    "sensitivity <- lago_sensitivity(",
    paste0(c("  result", sprintf("  parameter = %s", deparse1(param)), sprintf("  values = %s", paste(deparse(values, width.cutoff = 500L), collapse = " ")), extra), collapse = ",\n"),
    ")"
  )
}

# x to `digits` significant digits, rounded up or down so the rounded value
# is never on the wrong side of x. Ten digits keep the value close at any
# scale, such as a cost of 10,027.04 or an outcome of 1,030.44.
.app_round_safe <- function(x, direction = c("up", "down"), digits = 10) {
  direction <- match.arg(direction)
  if (!is.finite(x) || x == 0) {
    return(x)
  }
  near <- signif(x, digits)
  if (if (direction == "up") near >= x else near <= x) {
    return(near)
  }
  # one unit of the last digit kept, toward the safe side
  unit <- 10^(floor(log10(abs(x))) - digits + 1)
  signif(near + if (direction == "up") unit else -unit, digits)
}

.app_budget_code <- function(budget, n_grid) {
  sprintf("budget_result <- lago_budget(result, budget = %s, n_grid = %s)", deparse1(budget), deparse1(as.integer(n_grid)))
}

# What a budget search found, in plain sentences.
.app_budget_sentence <- function(b) {
  fmt <- function(v) vapply(.lago_fmt_value(v), format, character(1), big.mark = ",")
  if (!is.null(b$run_error)) {
    return(paste("The search could not run, because every optimization failed:", b$run_error))
  }
  if (!isTRUE(b$feasible)) {
    if (!NROW(b$frontier)) {
      return("No outcome goal in the searched range could be reached within the bounds, so no budget would help. Widen the bounds in step 3.")
    }
    return(sprintf(
      "No intervention that reaches a goal costs %s or less. The cheapest one found costs %s.",
      fmt(b$budget), fmt(min(b$frontier$rec_int_cost, na.rm = TRUE))
    ))
  }
  levels <- paste(sprintf("%s at %s", b$component_names, fmt(b$rec_int)), collapse = ", ")
  best <- if (identical(b$intention, "minimize")) "lowest" else "best"
  first <- sprintf(
    "With a budget of %s, the %s estimated outcome is %s. It sets %s, for a cost of %s.",
    fmt(b$budget), best, fmt(b$est_outcome), levels, fmt(b$rec_int_cost)
  )
  second <- if (isTRUE(b$binding)) {
    "A better outcome can be reached, but it costs more than the budget."
  } else {
    "The budget is not the limit: this is the best outcome the bounds allow, and it costs less than the budget."
  }
  paste(first, second)
}

# NULL when the report can be made, else what to install.
.app_report_missing <- function(has_pkg = function(p) requireNamespace(p, quietly = TRUE),
                                has_pandoc = function() rmarkdown::pandoc_available()) {
  pkgs <- c("rmarkdown", "knitr", "jsonlite")
  missing <- pkgs[!vapply(pkgs, has_pkg, logical(1))]
  if (length(missing)) {
    return(sprintf(
      "The report needs the %s package%s. Install with install.packages(c(%s)) and restart the app.",
      paste(missing, collapse = ", "), if (length(missing) > 1) "s" else "",
      paste0("\"", missing, "\"", collapse = ", ")
    ))
  }
  if (!isTRUE(has_pandoc())) {
    return("The report needs pandoc, which comes with RStudio. Install pandoc from pandoc.org or run the app from RStudio.")
  }
  NULL
}

# A sweep as the app shows it: the swept value, the recommendation, its cost
# and outcome, and a note for a run that failed, was shrunk, or had its goal
# raised by the power goal.
.app_sweep_table <- function(s, labels = NULL) {
  param <- attr(s, "parameter")
  out <- data.frame(value = if (length(labels) == nrow(s)) labels else s$value)
  names(out) <- .app_sweep_label(param)
  for (cc in attr(s, "component_names")) out[[cc]] <- s[[cc]]
  out$Cost <- s$rec_int_cost
  out[["Estimated outcome"]] <- s$est_outcome_goal
  out$Note <- .app_sweep_notes(s)
  out
}

.app_sweep_notes <- function(s) {
  note <- rep("", nrow(s))
  eff <- attr(s, "effective_outcome_goal")
  if (identical(attr(s, "parameter"), "outcome_goal") && length(eff) == nrow(s)) {
    raised <- is.finite(eff) & eff > s$value + 1e-9
    note[raised] <- sprintf("The power goal raised the goal to %s.", format(signif(eff[raised], 4)))
  }
  shrunk <- attr(s, "shrinking_method_used")
  if (length(shrunk) == nrow(s)) note[shrunk %in% TRUE] <- "The goal is out of reach, so the shrinking method was used."
  errs <- attr(s, "error_messages")
  if (length(errs)) {
    at <- match(names(errs), as.character(s$value))
    note[at[!is.na(at)]] <- paste("This run failed:", errs[!is.na(at)])
  }
  note
}

# One plain sentence on how the cost moves across a sweep. `labels` name the
# values, such as the periods.
.app_sweep_sentence <- function(s, labels = NULL) {
  fmt <- function(v) vapply(.lago_fmt_value(v), format, character(1), big.mark = ",")
  ok <- s$status == "ok" & is.finite(s$rec_int_cost)
  if (!any(ok)) {
    return("Every run of the sweep failed. The table says why.")
  }
  ord <- order(s$value[ok])
  cost <- s$rec_int_cost[ok][ord]
  vals <- if (length(labels) == nrow(s)) labels[ok][ord] else fmt(s$value[ok][ord])
  out <- sprintf(
    "The cost of the recommendation is %s at the lowest value, %s, and %s at the highest value, %s.",
    fmt(cost[1]), vals[1], fmt(cost[length(cost)]), vals[length(vals)]
  )
  if (any(!ok)) out <- paste(out, sprintf("%d of %d runs failed, as the table shows.", sum(!ok), nrow(s)))
  out
}

# A per component input of a call in the order of `expected`. The package
# matches named values to the components by name, so values whose names are
# the expected ones, each once, are put in that order. Other values keep their
# order, as the package reads them by position.
.app_by_name <- function(x, expected) {
  if (is.array(x) && length(dim(x)) == 1) x <- stats::setNames(as.vector(x), names(x))
  nms <- names(x)
  if (!is.null(nms) && !anyNA(nms) && !anyDuplicated(nms) && length(nms) == length(expected) && setequal(nms, expected)) {
    x <- x[match(expected, nms)]
  }
  unname(x)
}

# The label of a period that a value stands for. A value that is a number
# also names the period whose label is that number, such as 2 for "02", as
# the package reads it, when exactly one label is.
.app_period_label <- function(value, labels) {
  value <- as.character(value)
  if (length(value) != 1 || is.na(value) || value %in% labels) {
    return(value)
  }
  number <- suppressWarnings(as.numeric(value))
  same <- which(suppressWarnings(as.numeric(labels)) == number)
  if (is.finite(number) && length(same) == 1) labels[same] else value
}

# Whether a value names one of the periods.
.app_period_in <- function(value, labels) .app_period_label(value, labels) %in% labels

# The spec of a lago_optimization() call, from the arguments a result stores,
# and the settings the app cannot show, in plain words. The data is the one
# the call used, which already has the center, period and group columns the
# package reads by name.
.app_spec_from_args <- function(a) {
  s <- .app_empty_spec()
  left <- character(0)
  d <- a$data
  s$outcome <- a$outcome_name
  s$outcome_type <- a$outcome_type
  if (identical(a$input_data_structure, "center_level")) {
    s$structure <- "center"
    s$cl_kind <- "proportion"
    s$cl_value <- "proportion"
    s$cl_total <- "center_sample_size"
  }
  if (!is.null(a$power_goal)) {
    s$goal_mode <- if (is.null(a$outcome_goal)) "power" else "both"
    s$power_goal <- a$power_goal
    s$n_centers <- a$num_centers_in_next_stage
    s$n_patients <- a$patients_per_center_in_next_stage
    s$approach <- a$power_goal_approach
    # a pair is shown as control, treatment, which named values give by name
    if (!is.null(a$icc)) s$icc <- if (length(a$icc) == 2) as.numeric(.app_by_name(a$icc, c("control", "treatment"))) else as.numeric(a$icc)
    if (!is.null(a$power_goal_cluster_id)) s$cluster_col <- a$power_goal_cluster_id
    s$arm_col <- "group"
    s$arm_value <- "treatment"
  }
  if (!is.null(a$outcome_goal)) s$goal <- a$outcome_goal
  s$intention <- a$outcome_goal_intention

  inter <- isTRUE(a$include_interaction_terms) && !is.null(a$main_components)
  comps <- if (inter) a$main_components else a$intervention_components
  s$components <- comps
  pairs <- if (length(comps) >= 2) vapply(utils::combn(comps, 2, simplify = FALSE), paste, character(1), collapse = ":") else character(0)
  for (term in setdiff(a$intervention_components, comps)) {
    ab <- strsplit(gsub("`", "", term), ":", fixed = TRUE)[[1]]
    pair <- if (length(ab) == 2) intersect(c(paste(ab, collapse = ":"), paste(rev(ab), collapse = ":")), pairs) else character(0)
    if (length(pair)) {
      s$interactions <- c(s$interactions, pair[1])
    } else {
      left <- c(left, sprintf("the interaction term %s, which is not the product of two components", term))
    }
  }
  # the app reads each per component input in the order of the components
  by_comp <- function(x) as.numeric(.app_by_name(x, comps))
  s$lower <- by_comp(a$intervention_lower_bounds)
  s$upper <- by_comp(a$intervention_upper_bounds)
  if (!is.null(a$cost_list_of_vectors)) {
    s$cost_mode <- "coef"
    s$coefs <- lapply(.app_by_name(a$cost_list_of_vectors, comps), as.numeric)
    s$unit_costs <- rep(1, length(comps))
  } else {
    s$unit_costs <- by_comp(a$unit_costs)
    s$cost_type <- a$default_cost_fxn_type
    s$doses <- if (is.null(a$unit_cost_doses)) rep(NA_real_, length(comps)) else by_comp(a$unit_cost_doses)
    s$coefs <- rep(list(c(0, 1)), length(comps))
  }

  if (!is.null(a$center_characteristics)) {
    s$center_mode <- "chars"
    s$center_chars <- a$center_characteristics
    s$center_char_values <- as.numeric(.app_by_name(a$center_characteristics_optimization_values, a$center_characteristics))
  }
  effects <- if (is.null(a$include_center_effects)) identical(s$structure, "center") else isTRUE(a$include_center_effects)
  if (effects) {
    s$center_mode <- "effects"
    s$center_col <- "center"
    if (!is.null(a$center_weights_for_outcome_goal)) {
      s$cw_on <- TRUE
      # the app gives the weights in the order of the centers, and the
      # package matches named weights to the centers by name
      w <- a$center_weights_for_outcome_goal
      centers <- .app_center_levels(d, "center")
      if (!is.null(names(w)) && length(centers) && setequal(names(w), centers)) w <- w[centers]
      s$cw <- as.numeric(w)
    } else if (!is.null(a$center_effects_optimization_values)) {
      s$center_for <- as.character(a$center_effects_optimization_values)
    }
  }
  if (isTRUE(a$include_time_effects)) {
    s$time_on <- TRUE
    s$period_col <- "period"
    v <- a$time_effect_optimization_value
    s$period_value <- if (is.numeric(v) && "period" %in% names(d)) .app_period_label(v, .app_periods(d$period)$labels) else as.character(v)
  }
  if (!is.null(a$additional_covariates)) s$covariates <- a$additional_covariates

  if (identical(a$glm_family, "quasibinomial")) s$glm_family <- "quasibinomial"
  if (a$link %in% c("logit", "identity")) s$link <- a$link
  if (!is.null(a$weights)) {
    same <- names(d)[vapply(d, function(v) is.numeric(v) && length(v) == length(a$weights) && isTRUE(all.equal(as.numeric(v), as.numeric(a$weights))), logical(1))]
    if (length(same)) {
      s$weights_col <- same[1]
    } else {
      left <- c(left, "the weights, which are not a column of the data")
    }
  }
  # a step the call left out is 1/20 of the component's range in the data, as
  # the package sets it, and not of the bounds, as the app would
  data_steps <- vapply(comps, function(cc) diff(range(d[[gsub("`", "", cc)]], na.rm = TRUE)) / 20, numeric(1), USE.NAMES = FALSE)
  # a component with one value leaves its step to the app
  data_steps[!(data_steps > 0)] <- NA_real_
  if (identical(a$optimization_method, "grid_search") || !is.null(a$optimization_grid_search_step_size)) {
    s$method <- "grid_search"
    s$grid_steps <- if (is.null(a$optimization_grid_search_step_size)) data_steps else by_comp(a$optimization_grid_search_step_size)
  }
  s$include_cs <- !isFALSE(a$include_confidence_set)
  level <- 1 - a$confidence_set_alpha
  known <- c(0.9, 0.95, 0.99)
  if (any(abs(level - known) < 1e-9)) {
    s$cs_level <- known[abs(level - known) < 1e-9]
  } else {
    left <- c(left, sprintf("the confidence level of %s%%, as the app offers 90%%, 95%% and 99%%", format(100 * level)))
  }
  if (!is.null(a$confidence_set_grid_step_size)) {
    s$cs_steps <- by_comp(a$confidence_set_grid_step_size)
  } else if (s$include_cs) {
    s$cs_steps <- data_steps
  }
  if (!is.null(a$prev_recommended_interventions)) {
    s$prev_on <- TRUE
    s$prev <- by_comp(a$prev_recommended_interventions)
  }
  list(spec = s, left = left)
}

# The columns of the data that a spec names, which a dataset must have before
# saved settings can be put into the app.
.app_spec_columns <- function(spec) {
  cols <- c(
    if (.app_center_level(spec)) c(spec$cl_value, spec$cl_total) else spec$outcome,
    spec$components, spec$covariates,
    if (identical(spec$center_mode, "chars")) spec$center_chars,
    if (identical(spec$center_mode, "effects")) spec$center_col,
    if (isTRUE(spec$time_on)) spec$period_col,
    if (.app_power_on(spec)) c(spec$arm_col, spec$cluster_col),
    spec$weights_col
  )
  unique(cols[nzchar(cols)])
}

# Values of loaded settings that this data does not have, and what the app
# uses instead, in plain words. The columns are checked before this.
.app_spec_misfits <- function(spec, data) {
  out <- character(0)
  if (identical(spec$center_mode, "effects") && isTRUE(spec$center_col %in% names(data))) {
    centers <- .app_center_levels(data, spec$center_col)
    if (nzchar(spec$center_for) && !spec$center_for %in% centers) {
      out <- c(out, sprintf("the center %s to optimize for, which this data does not have, so the weighted average center is used", spec$center_for))
    } else if (!nzchar(spec$center_for) && isTRUE(spec$cw_on) && length(spec$cw) != length(centers)) {
      out <- c(out, sprintf(
        "the center weights, which are for %d centers while this data has %d, so each center starts at its share of the patients",
        length(spec$cw), length(centers)
      ))
    }
  }
  if (isTRUE(spec$time_on) && isTRUE(spec$period_col %in% names(data))) {
    periods <- .app_periods(data[[spec$period_col]])$labels
    if (!.app_period_in(spec$period_value, periods)) {
      out <- c(out, sprintf("the period %s, which this data does not have, so the last period is used", spec$period_value))
    }
  }
  if (.app_power_on(spec) && isTRUE(spec$arm_col %in% names(data))) {
    values <- unique(as.character(stats::na.omit(data[[spec$arm_col]])))
    if (!as.character(spec$arm_value) %in% values) {
      out <- c(out, sprintf("the treatment value %s, which the column %s does not have", spec$arm_value, spec$arm_col))
    }
  }
  out
}
