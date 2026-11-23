# Shared presentation formatter for `lago` objects.
#
# `lago_blocks()` turns a `lago` object into an ordered list of labeled,
# pre-formatted "blocks". Both the console S3 methods and the report generator
# render from these blocks, so they can never drift apart.
#
# This layer performs NO statistics: every quantity is pulled verbatim from an
# existing field and formatted with the SAME wording and rounding the current
# print.lago / summary.lago methods use. The `%||%` null-coalescing helper is
# defined once in R/lago_methods.R and is visible package-wide.

# --- small formatting helpers -------------------------------------------------
# Rounding kept consistent with print.lago / summary.lago and with the JS
# fmtValue(), fmtInterval() and fmtCost() of the report
# (inst/js/lago-report.js):
#   - recommendation values: .lago_fmt_value()
#   - estimated outcome and its interval: .lago_fmt_interval()
#   - recommended cost: 5 significant digits (.lago_fmt_cost())
#   - confidence-set cost IQR: .lago_fmt_cost_range()
#   - p-values: 4 significant digits, or "< 2.2e-16" when the p-value is below
#     the machine precision (it then underflows to 0, which is not a true
#     p-value)
#
# Every rounding here goes through .lago_round_dec(), which works on the
# decimal digits of the number and rounds a cut digit of 5 up, away from 0.
# R's round() and signif() and the toFixed() and toPrecision() of JS each
# settle a cut digit of 5 their own way, so a value such as 0.10005 could show
# as 0.1 in the text and as 0.1001 in a hover read-out. The JS code of the
# report uses the same digits and the same rule, so both show the same text.

# The decimal digits of finite, nonzero numbers, as 15 significant digits
# (all a double holds without noise), with the sign and the power of 10 of the
# first digit. sprintf() rounds correctly and breaks an exact tie to even, as
# jsonlite does when it writes the report data, and as the JS decDigits() does.
.lago_dec_digits <- function(v) {
  s <- sprintf("%.14e", abs(v))
  list(
    neg = v < 0,
    digits = paste0(substr(s, 1, 1), substr(s, 3, 16)),
    exp = as.integer(substring(s, 18))
  )
}

# v rounded to n significant digits (sig = TRUE) or to n decimals
# (sig = FALSE), from the decimal digits above, with a cut digit of 5 rounded
# up, away from 0. A value that is not finite, or is 0, is returned as it is.
.lago_round_dec <- function(v, n, sig) {
  v <- as.numeric(v)
  out <- v
  ok <- is.finite(v) & v != 0
  if (!any(ok)) return(out)
  p <- .lago_dec_digits(v[ok])
  # the number of digits kept, at most the 15 there are
  keep <- pmin(n + (!sig) * (p$exp + 1L), 15L)
  next_digit <- suppressWarnings(as.integer(substr(p$digits, keep + 1L, keep + 1L)))
  next_digit[keep < 0 | is.na(next_digit)] <- 0L
  kept <- suppressWarnings(as.numeric(substr(p$digits, 1L, pmax(keep, 0L))))
  kept[is.na(kept)] <- 0
  kept <- kept + (next_digit >= 5L)
  res <- as.numeric(paste0(ifelse(p$neg, "-", ""), sprintf("%.0f", kept), "e", p$exp + 1L - keep))
  # a value rounded to 0 is 0, not -0
  res[res == 0] <- 0
  out[ok] <- res
  out
}

# 4 decimals, or 4 significant digits for values closer to 0 than 0.1, so a
# rare outcome keeps its digits. Values closer to 0 than 1e-6 are solver noise
# and stay at 4 decimals, so they print as 0.
.lago_fmt_value <- function(v) {
  v <- as.numeric(v)
  ifelse(
    abs(v) >= 0.1 | abs(v) < 1e-6 | !is.finite(v),
    .lago_round_dec(v, 4, sig = FALSE),
    .lago_round_dec(v, 4, sig = TRUE)
  )
}

# The estimated outcome and the bounds of its interval: 3 decimals, or 4
# significant digits below 0.1, the way get_confidence_set() stores the
# bounds. The estimate uses the same rule as its bounds, and one rule keeps the
# order of the three values, so the estimate never shows outside its interval
# (an estimate of 0.6789 with stored bounds 0.679 and 0.679 shows as 0.679).
.lago_fmt_interval <- function(v) {
  v <- as.numeric(v)
  ifelse(
    abs(v) >= 0.1 | !is.finite(v),
    .lago_round_dec(v, 3, sig = FALSE),
    .lago_round_dec(v, 4, sig = TRUE)
  )
}

# The recommended cost is produced by the numerical optimizer, whose trailing
# digits differ across platforms and BLAS builds. At 4 decimals those
# platform-specific digits become visible once costs are in the thousands
# (e.g. 23927.7721 vs 23927.7723), which breaks reproducible snapshots and
# reports. Showing 5 significant figures keeps the value stable regardless of
# the magnitude of the caller's cost units while staying precise enough for a
# cost. The confidence-set costs are evaluated at exact grid points, so their
# IQR stays on 2 decimals, with 3 significant digits for a cost below 1 so a
# cost in small units does not show as 0.
.lago_fmt_cost <- function(v) .lago_round_dec(v, 5, sig = TRUE)

.lago_fmt_cost_range <- function(v) {
  v <- as.numeric(v)
  ifelse(abs(v) >= 1 | v == 0, .lago_round_dec(v, 2, sig = FALSE), .lago_round_dec(v, 3, sig = TRUE))
}

# A number as text, written in full the way the JS numText() of the report
# writes it: e notation is used only for values from 1e21 up or closer to 0
# than 1e-6, as JS does, and then as JS writes it, such as 1e-7 and 1.5e+21.
# The text holds at most 15 significant digits, so a large number does not
# show floating point noise in its last digits. Pass the value rounded first.
.lago_num_text <- function(v) {
  v <- as.numeric(v)
  # NA, NaN, Inf and -Inf as R writes them
  out <- as.character(v)
  ok <- is.finite(v) & v != 0
  if (!any(ok)) return(out)
  p <- .lago_dec_digits(v[ok])
  d <- sub("0+$", "", p$digits)
  k <- nchar(d)
  e <- p$exp
  plain <- ifelse(
    e >= k - 1L,
    paste0(d, strrep("0", pmax(e - k + 1L, 0L))),
    ifelse(
      e < 0L,
      paste0("0.", strrep("0", pmax(-e - 1L, 0L)), d),
      paste0(substr(d, 1L, e + 1L), ".", substring(d, e + 2L))
    )
  )
  mantissa <- ifelse(k > 1L, paste0(substr(d, 1L, 1L), ".", substring(d, 2L)), d)
  sci <- paste0(mantissa, "e", ifelse(e >= 0L, "+", "-"), abs(e))
  out[ok] <- paste0(ifelse(p$neg, "-", ""), ifelse(e >= -6L & e <= 20L, plain, sci))
  out[!is.na(v) & v == 0] <- "0"
  out
}

# A p-value as text: 4 significant digits, written like the other numbers, or
# "< 2.2e-16" below the machine precision. NA stays NA.
.lago_fmt_pval <- function(p, digits = 4) {
  p <- as.numeric(p)
  eps <- .Machine$double.eps
  ifelse(
    !is.na(p) & p < eps,
    paste0("< ", format(eps, digits = 2)),
    .lago_num_text(.lago_round_dec(p, digits, sig = TRUE))
  )
}

# A test statistic or p-value as text with fmt, or "not available" when the
# value is missing (NULL or NA), as in an object made by an older version.
.lago_fmt_test_value <- function(v, fmt) {
  v <- as.numeric(v)
  if (length(v) == 0) v <- NA_real_
  out <- fmt(v)
  out[is.na(v)] <- "not available"
  out
}

# The coefficients of one cost polynomial, rounded to 5 significant figures and
# written like the other numbers, such as "0, 2269.9, 14.239". A coefficient
# from 1e21 up or closer to 0 than 1e-6 uses e notation, such as 1e+25, so it
# shows no made up digits.
.lago_fmt_cost_coefs <- function(v) {
  paste(.lago_num_text(.lago_fmt_cost(v)), collapse = ", ")
}

# --- the shared formatter -----------------------------------------------------

# The confidence level of a result, such as "95%", from the confidence_set_alpha
# it was computed with. 95% for an object that did not record its arguments.
# The smaller of alpha and 1 - alpha is rounded to 4 significant digits first,
# so an alpha such as 1/3 gives a short label ("66.67%"), a small alpha such as
# 1e-5 keeps its digits ("99.999%"), and an alpha close to 1 such as 0.99999
# does not show as 0% ("0.001%").
.lago_confidence_label <- function(x) {
  alpha <- attr(x, "lago_call_args")$confidence_set_alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha)) alpha <- 0.05
  level <- ifelse(
    alpha <= 0.5,
    1 - .lago_round_dec(alpha, 4, sig = TRUE),
    .lago_round_dec(1 - alpha, 4, sig = TRUE)
  )
  # 12 significant digits drop the noise of 100 * level
  paste0(.lago_num_text(.lago_round_dec(100 * level, 12, sig = TRUE)), "%")
}

#' Build presentation blocks for a LAGO result
#'
#' Internal helper that turns a `lago` object into an ordered list of labeled,
#' pre-formatted blocks shared by the console methods and the report generator.
#' Each block is `list(title = <string>, rows = <data.frame or character
#' vector>)`. Blocks that do not apply to the object are `NULL`
#' (`power` when there is no power goal, `test` when there are no test
#' results). The `confidence_set` block is always present. When there is no
#' set, its rows say why.
#'
#' No new statistics are computed here; every value is taken verbatim from an
#' existing field and formatted with the same wording and rounding used by
#' [print.lago()] and [summary.lago()].
#'
#' @param x A "lago" object returned by [lago_optimization()].
#'
#' @return A named list with elements `inputs`, `recommendation`, `cost`,
#'   `outcome`, `power`, `confidence_set`, and `test`, in that order.
#'
#' @keywords internal
#' @noRd
lago_blocks <- function(x) {
  # --- inputs recap: echoes the key user inputs so the console output can be
  # read on its own, the way the old output did. NULL when the fields were not
  # carried (older objects). Each entry is a pre-formatted "label: value" line.
  inputs <- NULL
  if (!is.null(x[["outcome_name"]]) || !is.null(x[["input_nrow"]])) {
    input_rows <- character(0)
    if (!is.null(x[["input_nrow"]])) {
      input_rows <- c(
        input_rows,
        paste0(
          "Input data dimensions: ", x[["input_nrow"]], " rows, ",
          x[["input_ncol"]], " columns"
        )
      )
    }
    if (!is.null(x[["outcome_name"]])) {
      input_rows <- c(input_rows, paste0("Outcome name: ", x[["outcome_name"]]))
    }
    if (!is.null(x[["outcome_type"]])) {
      input_rows <- c(input_rows, paste0("Outcome type: ", x[["outcome_type"]]))
    }
    # an interaction term such as `x1:x2` is stored with backticks for the
    # model formula, and is shown without them as the caller typed it
    comps_in <- gsub("`", "", x[["intervention_components"]], fixed = TRUE)
    if (length(comps_in) > 0) {
      input_rows <- c(
        input_rows,
        paste0(
          length(comps_in), " intervention component(s): ",
          paste(comps_in, collapse = ", ")
        )
      )
    }
    if (isTRUE(x[["include_interaction_terms"]]) && !is.null(x[["main_components"]])) {
      input_rows <- c(
        input_rows,
        paste0(
          length(x[["main_components"]]), " main effect component(s): ",
          paste(x[["main_components"]], collapse = ", ")
        )
      )
    }
    if (!is.null(x[["center_characteristics"]])) {
      input_rows <- c(
        input_rows,
        paste0(
          length(x[["center_characteristics"]]), " center characteristic(s): ",
          paste(x[["center_characteristics"]], collapse = ", ")
        )
      )
    }
    if (!is.null(x[["family"]])) {
      input_rows <- c(
        input_rows,
        paste0("Outcome model family: ", x[["family"]]),
        paste0("Outcome model link: ", x[["link"]]),
        paste0("Fixed center effects: ", isTRUE(x[["include_center_effects"]])),
        paste0("Fixed time effects: ", isTRUE(x[["include_time_effects"]]))
      )
    }
    # The direction of the goal. An object made before the direction was
    # stored falls back on the arguments it recorded, and shows no direction
    # when it has neither.
    intention <- x[["outcome_goal_intention"]] %||% attr(x, "lago_call_args")$outcome_goal_intention
    goal_direction <- ""
    if (!is.null(x[["outcome_goal"]]) && identical(intention, "maximize")) {
      goal_direction <- " (maximize: at least this value)"
    } else if (!is.null(x[["outcome_goal"]]) && identical(intention, "minimize")) {
      goal_direction <- " (minimize: at most this value)"
    }
    # Both goal lines are always shown (with "not specified" when NULL), as the
    # old output did, so the recap is unambiguous about which goal was set.
    input_rows <- c(
      input_rows,
      paste0(
        "Outcome goal: ",
        if (is.null(x[["outcome_goal"]])) "not specified" else .lago_num_text(.lago_fmt_value(x[["outcome_goal"]])),
        goal_direction
      ),
      paste0(
        "Power goal: ",
        if (is.null(x[["power_goal"]])) "not specified" else .lago_num_text(x[["power_goal"]])
      )
    )
    if (!is.null(x[["power_goal"]]) && !is.null(x[["effective_outcome_goal"]])) {
      input_rows <- c(
        input_rows,
        paste0(
          "Effective outcome goal (max of outcome goal and ",
          "power-implied outcome): ",
          .lago_num_text(.lago_fmt_value(x[["effective_outcome_goal"]]))
        )
      )
    }
    if (!is.null(x[["cost_list_of_vectors"]])) {
      # one "component: coefficients" entry per cost polynomial, named by the
      # components the cost vectors line up with (the main components with
      # interaction terms), and rounded so the line stays readable.
      cost_vecs <- x[["cost_list_of_vectors"]]
      cost_labels <- x[["display_components"]] %||% x[["intervention_components"]]
      cost_text <- vapply(cost_vecs, .lago_fmt_cost_coefs, character(1))
      if (length(cost_labels) == length(cost_vecs)) {
        cost_text <- paste0(cost_labels, " (", cost_text, ")")
      } else {
        cost_text <- paste0("(", cost_text, ")")
      }
      input_rows <- c(
        input_rows,
        paste0(
          "Intervention component costs: ",
          paste(cost_text, collapse = ", ")
        )
      )
    }
    if (!is.null(x[["intervention_lower_bounds"]])) {
      input_rows <- c(
        input_rows,
        paste0(
          "Intervention lower bounds: ",
          paste(.lago_num_text(x[["intervention_lower_bounds"]]), collapse = ", ")
        ),
        paste0(
          "Intervention upper bounds: ",
          paste(.lago_num_text(x[["intervention_upper_bounds"]]), collapse = ", ")
        )
      )
    }
    if (length(input_rows) > 0) {
      inputs <- list(title = "Inputs", rows = input_rows)
    }
  }

  # --- recommendation: zip rec_int with its component labels, exactly like
  # print.lago does. display_components lines up with rec_int; fall back to
  # intervention_components for robustness.
  comps <- x[["display_components"]] %||% x[["intervention_components"]]
  vals <- x[["rec_int"]]
  if (!is.null(comps) && length(comps) == length(vals)) {
    rec_rows <- data.frame(
      component = comps,
      value = .lago_num_text(.lago_fmt_value(vals)),
      stringsAsFactors = FALSE
    )
  } else {
    rec_rows <- data.frame(value = .lago_num_text(.lago_fmt_value(vals)), stringsAsFactors = FALSE)
  }
  recommendation <- list(
    title = "Recommended intervention",
    rows = rec_rows
  )

  # --- cost
  cost <- list(
    title = "Cost",
    rows = paste0("Cost: ", .lago_num_text(.lago_fmt_cost(x[["rec_int_cost"]])))
  )

  # --- outcome (estimated outcome, its CI when available, and the outcome
  # goal when present). The CI is the interval for the estimated outcome at the
  # recommended intervention, which get_confidence_set() reports in its own
  # rec_int_ci field, carried on the object as est_outcome_ci.
  # The estimate is shown with the rounding of its interval, so it never
  # shows outside the interval (see .lago_fmt_interval()).
  outcome_rows <- paste0(
    "Estimated outcome: ", .lago_num_text(.lago_fmt_interval(x[["est_outcome_goal"]]))
  )
  # Always show a CI line, with the old fallback wording when the interval is
  # not available. The interval no longer depends on the confidence set being
  # non-empty, so with the set requested it is missing only when the interval
  # at the recommended intervention itself could not be computed.
  level <- .lago_confidence_label(x)
  ci_line <- if (!is.null(x[["est_outcome_ci"]])) {
    paste0(
      level, " CI for the estimated outcome: ",
      .lago_num_text(.lago_fmt_interval(x[["est_outcome_ci"]][["lower"]])),
      " - ",
      .lago_num_text(.lago_fmt_interval(x[["est_outcome_ci"]][["upper"]]))
    )
  } else if (!is.null(x[["confidence_set_size_percentage"]])) {
    paste0(
      level, " CI for the estimated outcome: not available (the interval at the ",
      "recommended intervention could not be computed)"
    )
  } else if (isTRUE(x[["shrinking_method_used"]])) {
    paste0(level, " CI for the estimated outcome: not available (the goal could not be reached, so the shrinking method was used and no confidence set is computed)")
  } else {
    paste0(level, " CI for the estimated outcome: not available (set include_confidence_set = TRUE)")
  }
  outcome_rows <- c(outcome_rows, ci_line)
  if (!is.null(x[["outcome_goal"]])) {
    outcome_rows <- c(
      outcome_rows,
      paste0("Outcome goal: ", .lago_num_text(.lago_fmt_value(x[["outcome_goal"]])))
    )
  }
  outcome <- list(
    title = "Outcome",
    rows = outcome_rows
  )

  # --- power (NULL when no power goal)
  power <- NULL
  if (!is.null(x[["power_goal"]])) {
    power <- list(
      title = "Power",
      rows = paste0("Power goal: ", .lago_num_text(x[["power_goal"]]))
    )
  }

  # --- confidence set. Always present, so the console and the report both
  # explain why there is no set when it was not computed or is empty.
  if (is.null(x[["confidence_set_size_percentage"]]) && isTRUE(x[["shrinking_method_used"]])) {
    cs_rows <- "Not computed: the goal could not be reached, so the shrinking method was used and no confidence set is computed."
  } else if (is.null(x[["confidence_set_size_percentage"]])) {
    # confidence set was not requested (include_confidence_set = FALSE).
    cs_rows <- "Not computed (set include_confidence_set = TRUE to compute it)."
  } else {
    # 2 decimals, but a set too small for that shows as "< 0.01%" and not
    # as 0%, which would read as an empty set, and a set just short of the
    # whole grid shows as "> 99.99%" and not as 100%.
    size <- 100 * x[["confidence_set_size_percentage"]]
    size_round <- .lago_round_dec(size, 2, sig = FALSE)
    size_text <- .lago_num_text(size_round)
    if (size > 0 && size_round == 0) size_text <- "< 0.01"
    if (size < 100 && size_round == 100) size_text <- "> 99.99"
    cs_rows <- paste0(level, " confidence set size: ", size_text, "% of the grid")
    if (is.null(x[["cs"]])) {
      # size field present but no set found for the current goal.
      cs_rows <- c(cs_rows, "No confidence set was found for the current outcome goal.")
    } else if (!is.null(x[["cs"]]$cost)) {
      # IQR (25th-75th percentile) of the cost within the confidence set, as the
      # old console output reported it.
      qs <- stats::quantile(x[["cs"]]$cost, probs = c(0.25, 0.75), names = FALSE)
      cs_rows <- c(
        cs_rows,
        paste0(
          "IQR of the cost within the ", level, " confidence set: ",
          .lago_num_text(.lago_fmt_cost_range(qs[1])),
          " - ",
          .lago_num_text(.lago_fmt_cost_range(qs[2]))
        )
      )
    }
  }
  confidence_set <- list(
    title = "Confidence set",
    rows = cs_rows
  )

  # --- overall intervention-effect test (NULL when no test results). Binary
  # outcomes use the two-sample test for a difference in proportions; the label
  # names the test, as the old output did.
  test <- NULL
  if (!is.null(x[["test_results"]])) {
    test_rows <- character(0)
    if (identical(x[["outcome_type"]], "binary")) {
      test_rows <- c(
        test_rows,
        "Two-sample test for the difference in two proportions:"
      )
    }
    test_rows <- c(
      test_rows,
      paste0(
        "Test statistic: ",
        .lago_fmt_test_value(x[["test_results"]][["test_stat"]], function(v) .lago_num_text(.lago_fmt_value(v)))
      ),
      paste0("P-value: ", .lago_fmt_test_value(x[["test_results"]][["p_val"]], .lago_fmt_pval))
    )
    test <- list(
      title = "Overall intervention-effect test",
      rows = test_rows
    )
  }

  list(
    inputs = inputs,
    recommendation = recommendation,
    cost = cost,
    outcome = outcome,
    power = power,
    confidence_set = confidence_set,
    test = test
  )
}
