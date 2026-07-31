# Design: package polish (vignette, NEWS, hex logo, S3 methods)

Four "make the package feel finished" improvements in one PR. Ranked by risk:
the S3 methods (4) touch the return value and are the only real-risk item; the
rest are additive docs/assets.

## 1. A vignette

Add one narrative article rendered as the pkgdown "Articles" tab and shipped as
a package vignette. Content: a walkthrough of a LAGO optimization on the
built-in BetterBirth data (fit -> recommended intervention -> confidence set),
plus a short "adding a power goal" section. Most of the prose/code already
exists in `tests/manual_tests/test_rec_int_for_BB_data.Rmd` and
`test_binary_power_goal.Rmd`; adapt, do not duplicate wholesale.

Mechanics:
- `vignettes/lago-optimization.Rmd` with the standard
  `output: rmarkdown::html_vignette` + `%\VignetteIndexEntry{...}` +
  `%\VignetteEngine{knitr::rmarkdown}` header.
- DESCRIPTION: add `knitr`, `rmarkdown` to Suggests and `VignetteBuilder: knitr`.
- The vignette MUST use `library(LAGO)`, NOT `devtools::load_all()` (the source
  manual-test `.Rmd`s start with `devtools::load_all()`, but devtools is not a
  package dependency, so R CMD build on a clean machine would fail).
- Use `quiet = TRUE` in the vignette calls (the new argument) so the rendered
  output is clean, and keep runtime small (coarse `confidence_set_grid_step_size`).
- Reproducible: use only the shipped `BB_data` (no network, no confidential
  PULESA data). Gate any heavy chunk with `eval` if build time is a concern.

## 2. NEWS.md

Add a `NEWS.md` changelog. pkgdown renders it as a "Changelog" tab, matching the
top heading to the DESCRIPTION `Version`. DESCRIPTION `Version` is currently
`1.0.12` (NOT a development version), so the heading is `# LAGO 1.0.12` — do not
use "development version" (it would not match). List the work already shipped
under it (from git log / merged PRs):
- power goal standalone (#40), grid-step auto-switch (#32), fit diagnostics
  (#36), per-slider cost ranges (#34), testthat + CI (#45), return
  test_results (#49), ICC in the power calculation (#29), single-component
  crash fix (#54), pkgdown site (#53), the visualize_cost improvements +
  return-to-session (#59), and the `quiet` argument (#60).
Do not invent version numbers beyond the existing 1.0.12.

## 3. Hex logo

The standard "fancy R package" badge. ALL-OR-NOTHING: wire the README `<img>`
tag and the pkgdown logo ONLY together with the actual image file — an `<img>`
referencing a missing `man/figures/logo.png` renders as a broken image on
GitHub and the pkgdown site (worse than no logo).

- Convention: `man/figures/logo.png` (pkgdown + `usethis::use_logo()` pick it up
  automatically for the navbar; referenced in the README as
  `# LAGO <img src="man/figures/logo.png" align="right" height="139" />`).
- DECISION FOR THE MAINTAINER: the hex image is a brand choice this design does
  not auto-pick. Options: (a) the maintainer provides/approves a designed logo;
  (b) generate one with the `hexSticker` package (NOT currently installed) from
  an agreed motif/colors. Until an image exists, this PR ships NEITHER the file
  NOR the `<img>` tag (no broken link). If the maintainer supplies/approves an
  image during this PR, add the file AND the tag together; otherwise defer item
  3 to a follow-up and ship items 1, 2, 4.

## 4. S3 print / summary / plot methods (the real-risk item)

Give `lago_optimization()`'s result an S3 class so it prints cleanly and can be
plotted, instead of being a bare list dumped by `print_output`.

Design:
- Add `class(result) <- "lago"` to the returned list in BOTH return branches.
  Backward-compatible: still a list, so `res$rec_int`, `res$cs`, `names(res)`,
  and the existing tests (`... %in% names(res)`) keep working (verified live by
  both reviewers: `$`/`[[`/`names`/`%in%`/`is.list`/`saveRDS` all unchanged;
  `identical(unclass(new), old)` is the compat check). Because the return is a
  two-branch `return(if ... )`, capture the list in a variable, set the class,
  then return it.
- REQUIRED carrier fields (additive, compat-safe): the current return does NOT
  contain the metadata the methods need. `rec_int` is an UNNAMED numeric
  (`as.numeric(full_grid[...])` strips names, get_recommended_interventions.R),
  and `intervention_components`, `outcome_type`, `outcome_goal`, `power_goal`
  are not stored. Add these to BOTH branches so the methods can render:
  `intervention_components` (the labels), `outcome_type`, `outcome_goal`,
  `power_goal`. These are all in scope at the return (they are function
  arguments). This is the fix for the design's earlier gap; the "no return
  changes" scope note is corrected — field ADDITIONS are in scope, field
  renames/removals are not.
- `print.lago(x, ...)`: concise summary — recommended intervention as a
  component = value table (built by zipping `intervention_components` with
  `rec_int`, since `rec_int` itself is unnamed), cost, estimated outcome goal,
  confidence-set size % (when present), and whether a power goal / overall test
  was used (from the new carrier fields + `test_results`). Use `cli`.
- `summary.lago(object, ...)`: the above plus the confidence-set IQR / first
  rows and the overall-test statistic/p-value when present.
- `plot.lago(x, ...)`: a ggplot showing the confidence set. Guard order:
  (a) if `cs` is absent/NULL (the default `include_confidence_set = FALSE`
  branch has NO `cs` field at all), message that a plot needs
  `include_confidence_set = TRUE` and return invisibly — do NOT error;
  (b) determine the number of intervention components from the stored
  `intervention_components`, NOT from `ncol(cs)` (the `cs` gains an extra
  column when `center_characteristics` is used, so counting cs columns
  miscounts the flagship 2-component BB example); (c) for 2 components, plot the
  in-set grid points with the recommended intervention highlighted; for 1
  component, plot cost (or CI bounds) vs the single dose; for >2, message that
  plotting is supported for <=2 components. Never error.
- ggplot2 "no visible binding for global variable" NOTE: `aes()` with bare
  column names triggers an R CMD check NOTE. Use the `.data$col` pronoun (or
  extend the existing `globalVariables` in zzz.R, which already lists x/y). The
  design's earlier "no new NOTE" claim only holds with this.

Interaction with existing `print_output`: `print_output` is the verbose
console dump called inside `lago_optimization` (now gated by `quiet`). It stays
as-is for the in-run progress printing. The new `print.lago` is what fires when
the user later prints the returned object. These are separate and both fine;
the design should note they are not redundant (one is the run-time report, one
is the stored-object display).

Backward-compat requirement (hard): every existing test must still pass, and
`res$<field>` access is unchanged. The class is additive.

## Scope

In scope: the vignette; NEWS.md (`# LAGO 1.0.12`); the S3 class + ADDITIVE
carrier fields (intervention_components, outcome_type, outcome_goal,
power_goal) in both return branches; print/summary/plot.lago + roxygen +
`@export` (which emits the S3method entries); the DESCRIPTION
Suggests(knitr, rmarkdown) + VignetteBuilder. Hex logo ONLY if an image is
supplied this PR (else deferred).

Out of scope: designing/auto-generating the hex image; CRAN submission; changing
any computation; renaming/removing existing return fields (only additions).

## Verification plan

- S3 backward-compat: full testthat suite still passes (current suite is 32
  test_that / 72 expect_* per review; do not assert "75/75"); `res$rec_int`
  etc. and `names(res)` unchanged; `identical(unclass(res_new_without_carrier),
  res_old)` conceptually (note the carrier fields are new, so the exact check
  is: existing fields byte-identical + new fields present + class attached).
- Carrier fields: confirm intervention_components, outcome_type, outcome_goal,
  power_goal are present in BOTH return branches and correct.
- print.lago / summary.lago: render without error for binary + continuous, with
  and without confidence set / power goal / group column; the component=value
  table is built by zipping intervention_components with the unnamed rec_int.
- plot.lago: cs-absent -> graceful message (no error); 2 components (incl. the
  BB example WITH center_characteristics, where cs has an extra column) ->
  correct ggplot using the stored component count, not ncol(cs); 1 component ->
  cost-vs-dose plot; >2 -> graceful message. Uses `.data$` in aes (no NOTE).
- Vignette: builds via R CMD build / `devtools::build_vignettes()` without
  error, uses `library(LAGO)` + quiet=TRUE + coarse grid, only BB_data.
- NEWS.md heading matches Version 1.0.12; pkgdown build succeeds, Changelog +
  Articles tabs render.
- R CMD check: no new NOTE/WARNING (vignette VignetteBuilder set, S3method
  entries, DESCRIPTION changes, and the `.data$` pronoun avoiding the ggplot
  global-variable NOTE).

## Resolved by review

1. S3 class is backward-compat-safe (verified live by both reviewers). Confirmed.
2. The return MUST gain additive carrier fields (intervention_components,
   outcome_type, outcome_goal, power_goal) — the methods cannot render without
   them (rec_int is unnamed; labels/goals not otherwise stored). Corrected the
   scope to allow field additions.
3. plot.lago: guard cs-presence FIRST (no cs in the default no-CS branch);
   count components from stored intervention_components, NOT ncol(cs)
   (center_characteristics adds a cs column and would miscount).
4. Use `.data$col` in ggplot `aes()` to avoid the "no visible binding" NOTE.
5. Logo is all-or-nothing (no broken `<img>`); NEWS heading is `# LAGO 1.0.12`;
   vignette uses `library(LAGO)` not devtools.

## Open questions for the maintainer

1. Class name: `lago` (proposed) vs `lago_optimization` vs `lago_result`?
2. Hex logo: provide/approve an image so item 3 ships in this PR, or defer it to
   a follow-up (ship items 1, 2, 4 now)? No silently auto-generated logo.
3. print.lago vs print_output: keep the split (run-time report vs stored-object
   display — both reviewers agree the split is sound and print_output cannot be
   reused since it needs model/inputs the object does not store)? Confirmed
   sound; flagging only for awareness.
