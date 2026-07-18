# Unit tests for the internal pure helper functions.

test_that("cost_fxn_calculator builds linear cost coefficients", {
  cost_fxn_calculator <- getFromNamespace("cost_fxn_calculator", "LAGO")
  res <- cost_fxn_calculator(
    intervention_lower_bounds = c(0),
    intervention_upper_bounds = c(10),
    unit_costs = c(2),
    default_cost_fxn_type = "linear"
  )
  # linear cost for unit cost u is c(0, u)
  expect_equal(res[[1]], c(0, 2))
})

test_that("cost_fxn_calculator cubic cost is non-negative and non-decreasing", {
  cost_fxn_calculator <- getFromNamespace("cost_fxn_calculator", "LAGO")
  res <- cost_fxn_calculator(
    intervention_lower_bounds = c(0),
    intervention_upper_bounds = c(10),
    unit_costs = c(2),
    default_cost_fxn_type = "cubic"
  )
  coeffs <- res[[1]]
  # total cost is a degree-4 polynomial: coeffs are c(c0, c1, c2, c3, c4)
  x <- seq(0, 10, length.out = 500)
  total <- sapply(x, function(v) sum(coeffs * v^(seq_along(coeffs) - 1)))
  expect_true(all(total >= -1e-8)) # non-negative
  expect_true(all(diff(total) >= -1e-8)) # non-decreasing
})

test_that("compute_slider_range centers on init and stays finite/positive", {
  csr <- getFromNamespace("compute_slider_range", "LAGO")
  # large coefficient
  r <- csr(2.716, 2)
  expect_true(r$min < 2.716 && 2.716 < r$max)
  expect_true(r$max > r$min)
  expect_true(r$step > 0)
  # zero-init coefficient falls back to a unit-cost-derived floor
  r0 <- csr(0, 5)
  expect_equal(r0$min, -5)
  expect_equal(r0$max, 5)
  # zero unit cost still yields a usable range (floor of 1)
  rz <- csr(0, 0)
  expect_equal(rz$min, -1)
  expect_equal(rz$max, 1)
})

test_that("format_coef uses 3 decimals, more for small values, drops zeros", {
  fmt <- getFromNamespace("format_coef", "LAGO")
  expect_equal(fmt(11.95743), "11.957")
  expect_equal(fmt(-10.8629818), "-10.863")
  expect_equal(fmt(0), "0")
  expect_equal(fmt(0.0055), "0.0055") # small: extra places kept
  expect_equal(fmt(0.000029), "0.00003") # capped at 5 decimals
})

test_that("get_int_vector prepends 1 and expands interaction products", {
  giv <- getFromNamespace("get_int_vector", "LAGO")
  # no interaction: c(1, x)
  expect_equal(
    giv(FALSE, c("a", "b"), NULL, c(3, 4)),
    c(1, 3, 4)
  )
  # interaction a:b -> product of the two main components
  expect_equal(
    giv(TRUE, c("a", "b", "a:b"), c("a", "b"), c(3, 4)),
    c(1, 3, 4, 12)
  )
})
