cost_fxn_calculator <- function(
    intervention_lower_bounds,
    intervention_upper_bounds,
    unit_costs,
    default_cost_fxn_type) {
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
        compute_abcd <- function(range_min, range_max, unit_cost) {
            # Percentiles for derivative constraints
            x1 <- range_min + 0 * (range_max - range_min)
            x2 <- range_min + 0.33 * (range_max - range_min) # adjust as needed
            x3 <- range_min + 0.6 * (range_max - range_min) # adjust as needed

            # Initial delta value (negative slope at x2)
            delta <- -0.8 # Adjustable

            # Define function to compute coefficients
            solve_coefficients <- function(delta) {
                A <- matrix(c(
                    3 * x1^2, 2 * x1, 1, 0,
                    3 * x2^2, 2 * x2, 1, 0,
                    3 * x3^2, 2 * x3, 1, 0,
                    (range_max^4 - range_min^4) / 4,
                    (range_max^3 - range_min^3) / 3,
                    (range_max^2 - range_min^2) / 2,
                    (range_max - range_min)
                ), nrow = 4, byrow = TRUE)

                b_vec <- c(0, delta, 0, (range_max - range_min) * unit_cost)
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
            while (TRUE) {
                coeff <- solve_coefficients(delta)

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

        results <- mapply(
            compute_abcd,
            intervention_lower_bounds,
            intervention_upper_bounds,
            unit_costs,
            SIMPLIFY = FALSE
        )
    } else {
        stop("Invalid default_cost_fxn_type")
    }

    return(results)
}
