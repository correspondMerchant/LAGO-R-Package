# Standalone Shiny app that runs LAGOtrials' visualize_cost() interface entirely
# in the browser via shinylive (Shiny on webR / WebAssembly). It is exported to
# static files and deployed to the documentation site under /visualize-cost/ by
# .github/workflows/shinylive.yaml, so the interactive cost-function designer
# runs with no server and no local R install.
#
# Packaging model:
#   * shinylive bundles shiny, bslib and shinyjs from repo.r-wasm.org at export
#     time (they have WebAssembly binaries there).
#   * LAGOtrials is NOT on repo.r-wasm.org, so it is installed at runtime from
#     the small CRAN-like wasm repository published alongside this site (built
#     against the SAME webR 0.6.0 ABI that shinylive bundles; see
#     .github/workflows/webr-repo.yaml). The webR versions on the two sides MUST
#     match or the binary will not load.
#   * LAGOtrials is loaded dynamically (a variable name + character.only, and the
#     app builder is reached with asNamespace) so shinylive's static dependency
#     scan (renv::dependencies) never tries, and fails, to bundle it at export
#     time.

pkg <- "LAGOtrials"
webr::install(pkg, repos = c(
  "https://correspondmerchant.github.io/LAGO-R-Package/webr-repo",
  "https://repo.r-wasm.org"
))
library(pkg, character.only = TRUE)

library(shiny)
library(bslib)
library(shinyjs)

# Open on a representative BetterBirth-style configuration (the same two
# components and unit costs the live demo and playground use), so the app lands
# on a live, meaningful example instead of an empty form.
build_app <- get(".build_visualize_cost_app", asNamespace(pkg))
build_app(
  component_names = c("Coaching visits", "Launch duration (months)"),
  unit_costs = c(1700, 8000),
  default_cost_fxn_type = "linear",
  intervention_lower_bounds = c(1, 1),
  intervention_upper_bounds = c(40, 5)
)
