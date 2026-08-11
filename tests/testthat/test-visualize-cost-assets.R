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

test_that("node JS math test passes (R<->JS numeric parity)", {
  # Run the standalone node test (tests/js/test-cost-math.js) so the R<->JS
  # curve-math parity is exercised in automation. Skipped cleanly where node is
  # absent (e.g. CRAN) or where the JS test file is not present (it is excluded
  # from the built tarball via .Rbuildignore, so this only runs against the
  # source tree, e.g. devtools::test()).
  #
  # Resolve node with Sys.which so a full path is used: the interpreter may live
  # outside the minimal PATH that system2() would otherwise see (e.g. a mise /
  # conda install), and Sys.which honours the PATH R was launched with.
  node <- Sys.which("node")
  skip_if(nchar(node) == 0, "node not available")

  # Locate the JS test relative to the testthat working directory (tests/testthat
  # during devtools::test() / R CMD check).
  js_test <- testthat::test_path("..", "js", "test-cost-math.js")
  skip_if(!file.exists(js_test), "tests/js/test-cost-math.js not present")

  status <- suppressWarnings(system2(
    node,
    args = shQuote(normalizePath(js_test)),
    stdout = FALSE,
    stderr = FALSE
  ))
  expect_equal(status, 0L)
})
