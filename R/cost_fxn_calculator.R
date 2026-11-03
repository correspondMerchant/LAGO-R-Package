cost_fxn_calculator <- function(
    intervention_lower_bounds,
    intervention_upper_bounds,
    unit_costs,
    default_cost_fxn_type,
    unit_cost_doses = NULL) {
  # The idea for calculating the cost function comes from Jingyu.
  #
  # Assuming we want a 3rd degree polynomial:
  # it has the form of f(x) = ax^3 + bx^2 + cx + d
  # A, b, c, and d are all unknowns.
  # To solve a, b, and c, we find 3 points on the graph lets
  # call them x1, x2, and x3, where they have desired derivatives
  # e.g. f’(x1) = 0, f’(x2) = -delta, f’(x3) = 0.
  # x1, x2, and x3 are at the 0th, 33th, and 66th percentile of the range.
  # If you would like to control where the cost function start to go up,
  # then you can move x2, instead of using it at the 25th percentile,
  # we could use at say 80 or 90th percentile.
  #
  # For the last unknown d,
  # we want integral over the range f(x)dx / range = 1
  # or, when the dose a unit cost was measured at is given, f(dose) = unit cost.
  # f(x) is the cost per unit at dose x, since the total cost is x f(x), so the
  # total cost at that dose is then the measured unit cost times the dose.
  #
  # please note that this method implies that the unit cost function
  # is a polynomial of degree 3, and the total cost function is a polynomial
  # of degree 4.
  #
  # also note that there are 2 more added constraints
  # (compared to Jingyu's code):
  # 1. the cubic function of the unit cost is always positive
  # 2. the total cost function is non-decreasing.
  #
  if (default_cost_fxn_type == "linear") {
    results <- lapply(unit_costs, function(x) c(0, x))
  } else if (default_cost_fxn_type == "cubic") {
    compute_abcd <- function(range_min, range_max, unit_cost, dose) {
      # a component held at one dose has the linear cost there
      if (range_max == range_min) {
        return(c(0, unit_cost, 0, 0, 0))
      }
      # Percentiles for derivative constraints
      x1 <- range_min + 0 * (range_max - range_min)
      x2 <- range_min + 0.33 * (range_max - range_min) # adjust as needed
      x3 <- range_min + 0.6 * (range_max - range_min) # adjust as needed

      # Initial delta value (negative slope at x2), in the units of the unit
      # cost, so restating the costs in other units only rescales the curve.
      # A unit cost of 0 then gives the zero curve at once.
      delta <- -0.8 * unit_cost # Adjustable
      # with a measured dose the slope is also per dose unit, so the pinned
      # curve does not depend on the units of the dose either
      if (!is.na(dose)) delta <- delta / (range_max - range_min)

      # Define function to compute coefficients. The system is solved in x and
      # in u = (x - range_min) / width, which is well conditioned. The solve in
      # x is used when its curve matches the curve in u, and otherwise the
      # solve in u converted back to x. When neither matches, as for bounds far
      # from 0 compared with their width, it returns NULL and a flatter curve
      # is tried.
      solve_coefficients <- function(delta) {
        plain <- tryCatch(solve_in(0, 1, delta), error = function(e) NULL)
        # the same curve, solved in u = (x - range_min) / width
        m <- range_min
        w <- range_max - range_min
        scaled <- solve_in(m, w, delta)
        # back to the powers of (x - m), then of x
        A3 <- scaled[1] / w^3
        B2 <- scaled[2] / w^2
        C1 <- scaled[3] / w
        D0 <- scaled[4]
        coeff <- c(
          A3,
          B2 - 3 * A3 * m,
          C1 - 2 * B2 * m + 3 * A3 * m^2,
          D0 - C1 * m + B2 * m^2 - A3 * m^3
        )
        # a curve in x must match the curve in u to 0.1%, which fails when
        # the bounds are far from 0 compared with their width. The plain solve
        # is checked first, so a result it gave before is kept when accurate.
        x <- seq(range_min, range_max, length.out = 25)
        u <- (x - m) / w
        curve_in_x <- function(k) k[1] * x^3 + k[2] * x^2 + k[3] * x + k[4]
        in_u <- scaled[1] * u^3 + scaled[2] * u^2 + scaled[3] * u + scaled[4]
        accurate <- function(k) {
          all(is.finite(k)) &&
            max(abs(curve_in_x(k) - in_u)) <= 1e-3 * max(abs(in_u), abs(unit_cost), 1e-12)
        }
        if (!is.null(plain) && accurate(as.numeric(plain))) {
          return(as.numeric(plain))
        }
        if (accurate(coeff)) {
          return(coeff)
        }
        # no accurate curve at this slope, so the loop below tries a smaller one
        NULL
      }
      # the coefficients of a u^3 + b u^2 + c u + d, where u = (x - shift) / scale
      solve_in <- function(shift, scale, delta) {
        u1 <- (x1 - shift) / scale
        u2 <- (x2 - shift) / scale
        u3 <- (x3 - shift) / scale
        lo <- (range_min - shift) / scale
        hi <- (range_max - shift) / scale
        last_row <- if (is.na(dose)) {
          c((hi^4 - lo^4) / 4, (hi^3 - lo^3) / 3, (hi^2 - lo^2) / 2, (hi - lo))
        } else {
          u0 <- (dose - shift) / scale
          c(u0^3, u0^2, u0, 1)
        }
        A <- matrix(c(
          3 * u1^2, 2 * u1, 1, 0,
          3 * u2^2, 2 * u2, 1, 0,
          3 * u3^2, 2 * u3, 1, 0,
          last_row
        ), nrow = 4, byrow = TRUE)
        # the slope condition and the average are taken in u
        last_value <- if (is.na(dose)) (range_max - range_min) * unit_cost / scale else unit_cost
        b_vec <- c(0, delta * scale, 0, last_value)
        solve(A, b_vec)
      }

      # Function to evaluate f(x)
      f <- function(x, coeff) {
        return(coeff[1] * x^3 + coeff[2] * x^2 + coeff[3] * x + coeff[4])
      }

      # Function to evaluate g'(x) = 4ax^3 + 3bx^2 + 2cx + d
      g_prime <- function(x, coeff) {
        return(4 * coeff[1] * x^3 + 3 * coeff[2] * x^2 + 2 * coeff[3] * x + coeff[4])
      }

      # Iteratively reduce |delta| if necessary
      first_delta <- delta
      while (TRUE) {
        coeff <- solve_coefficients(delta)
        if (is.null(coeff)) {
          # a flatter curve has smaller coefficients, so it may be accurate
          if (abs(delta) < 1e-8 * abs(first_delta)) {
            stop(paste(
              "The default cubic cost cannot be computed for bounds this far",
              "from 0 compared with their width. Give the costs with",
              "cost_list_of_vectors, or use default_cost_fxn_type = \"linear\"."
            ), call. = FALSE)
          }
          delta <- delta * 0.9
          next
        }

        # Check if f(x) is negative anywhere in the range
        x_vals <- seq(range_min, range_max, length.out = 100)
        min_f_x <- min(sapply(x_vals, function(x) f(x, coeff)))

        # Check if g'(x) is always non-negative
        min_g_prime_x <- min(sapply(x_vals, function(x) g_prime(x, coeff)))

        if (min_f_x >= 0 && min_g_prime_x >= 0) {
          # If both conditions hold, return coefficients
          rev_coeff <- rev(coeff)
          final_coeff <- c(0, rev_coeff)
          return(final_coeff)
        } else {
          # If conditions fail, reduce |delta| (make slope less steep)
          delta <- delta * 0.9 # Reduce by 10%
        }
      }
    }

    if (is.null(unit_cost_doses)) unit_cost_doses <- rep(NA_real_, length(unit_costs))
    results <- mapply(
      compute_abcd,
      intervention_lower_bounds,
      intervention_upper_bounds,
      unit_costs,
      unit_cost_doses,
      SIMPLIFY = FALSE
    )
  } else {
    stop("Invalid default_cost_fxn_type")
  }

  return(results)
}
