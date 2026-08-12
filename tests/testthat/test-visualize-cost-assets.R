# The client-side cost curves in visualize_cost() depend on vendored JS assets
# (D3 v7 + the cost-curve binding) being installed with the package and
# resolvable via system.file(). A broken install (assets missing) would make
# the app render blank curves, so guard the asset paths here.

test_that("vendored JS assets are installed and resolvable via system.file", {
  js <- system.file("js/cost-curves.js", package = "LAGO")
  d3 <- system.file("js/d3.v7.min.js", package = "LAGO")
  lic <- system.file("js/d3-LICENSE", package = "LAGO")

  expect_true(nzchar(js) && file.exists(js))
  expect_true(nzchar(d3) && file.exists(d3))
  expect_true(nzchar(lic) && file.exists(lic))
})

test_that("vendored D3 is v7 and its license carries the copyright", {
  d3 <- system.file("js/d3.v7.min.js", package = "LAGO")
  lic <- system.file("js/d3-LICENSE", package = "LAGO")
  skip_if(!nzchar(d3) || !nzchar(lic), "assets not installed")

  head_line <- readLines(d3, n = 1L, warn = FALSE)
  expect_match(head_line, "d3js\\.org v7", perl = TRUE)

  lic_txt <- paste(readLines(lic, warn = FALSE), collapse = "\n")
  expect_match(lic_txt, "Copyright", ignore.case = TRUE)
  expect_match(lic_txt, "Mike Bostock")
})

test_that("cost-curves.js exposes the pure math contract used by the app", {
  js <- system.file("js/cost-curves.js", package = "LAGO")
  skip_if(!nzchar(js), "asset not installed")
  txt <- paste(readLines(js, warn = FALSE), collapse = "\n")
  # the functions the binding + node test rely on
  expect_match(txt, "function costAt")
  expect_match(txt, "function marginalAt")
  expect_match(txt, "function dragScale")
  # the drag writeback input id must match the R observeEvent
  expect_match(txt, "dragged_coefs_")
})

# The JS math itself (tests/js/test-cost-math.js) is run by its own GitHub
# Actions job (js-test.yaml), not from here: driving a node subprocess out of
# the R suite is fragile across environments (node's require() resolves
# tests/js -> ../../inst/js, which does not hold in the copied tree R CMD check
# and covr build), and the R suite should not depend on a node interpreter.
