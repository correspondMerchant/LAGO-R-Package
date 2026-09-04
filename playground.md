Design a LAGO optimization interactively, in your browser, with no
install. Pick a bundled dataset or upload your own CSV, choose the
outcome and intervention components, set the bounds, per-unit costs and
outcome goal, then press **Run**. The recommendation is drawn with the
package's own interactive
[webR](https://docs.r-wasm.org/webr/latest/)-powered charts.

Starting webR…

First load downloads R and the package (tens of MB) once and can take up
to a minute; after that, runs are quick.

## 1. Data

Bundled dataset BB_data (BetterBirth, binary) mtcars (continuous)

… or upload a CSV

Drop a .csv here, or click to choose

Comma-separated with a header row; numeric columns become the outcome
and component choices.

## 2. Model

Outcome column

Outcome type

Binary

Continuous

Outcome goal: 0.85

Direction

Maximize

Minimize

Intervention components

Run optimization

Reset

## R code for this configuration

Copy

Paste this into R (after `library(LAGOtrials)`) to reproduce the run.

``` out
# choose an outcome and at least one intervention component
```

## Result

``` out
```

webR runs a WebAssembly build of R (4.6.0) entirely client-side; nothing
you load or type leaves your browser. The confidence-set plot is drawn
for one- or two-component interventions (three or more still get the
cost curves and the full console summary). This playground exposes the
common options; for the rest — a power goal, center characteristics and
fixed effects, clustering (`icc`), a custom GLM family/link, and more —
call `lago_optimization()` in R (copy the snippet above as a starting
point). To shape the per-component cost functions themselves (including
non-linear ones), use the interactive [cost
designer](https://correspondmerchant.github.io/LAGO-R-Package/visualize-cost/).
See the [package
documentation](https://correspondmerchant.github.io/LAGO-R-Package/index.md)
for the complete API.
