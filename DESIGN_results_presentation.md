# Design: LAGO results presentation layer (console + report)

Combines menu items #1 (report generator) and #5 (console glow-up). Both are
the *presentation layer* for a `lago` object, so they share one implementation:
same labels, same field->text mapping, same section order, same palette.

## Non-goal / safety boundary

No statistics. Every displayed quantity is pulled verbatim from an existing
named field on the object and labeled with the SAME wording the current
`print`/`summary`/`print_output` already use. No re-derivation, no
re-interpretation. If a field's meaning is ambiguous, flag it, don't guess.

## The object contract (verified in lago_optimization.R)

Fields available on a `lago` object:
- `rec_int` (unnamed numeric) + `display_components` (labels lining up with it)
- `rec_int_cost` (numeric)
- `est_outcome_goal` (numeric, estimated outcome at rec_int)
- `outcome_goal` (numeric or NULL)
- `power_goal` (numeric or NULL)
- `confidence_set_size_percentage` (numeric or NULL)
- `cs` (data.frame or NULL) — grid points in the 95% CS, with CI bounds + cost
- `test_results` (list `{test_stat, p_val}` or NULL)
- `outcome_type`, `intervention_components`

## Architecture: one shared formatter

New internal file `R/lago_presentation.R` with a single helper that turns a
`lago` object into an ordered list of labeled, pre-formatted "blocks"
(title + rows). BOTH the console methods and the report render from these
blocks, so they can never drift apart.

    lago_blocks(x) -> list(
      recommendation = list(title=..., rows=data.frame(component, value)),
      cost           = ...,
      outcome        = ...,
      power          = ...,   # NULL if no power_goal
      confidence_set = ...,   # NULL if no cs
      test           = ...    # NULL if no test_results
    )

Section order (matches current print_output): Recommendation -> Cost ->
Outcome -> Power -> Confidence set -> Overall test.

## Part A - Console glow-up (restyle print.lago / summary.lago)

- Boxed, colored `cli` sections (cli_h1/h2/rule) with a consistent accent color
  matched to the hex logo blue (#0066cc, already used in plot.lago).
- Aligned two-column tables for the recommendation (component | value) and the
  CS cost range, via `cli::cli_dl` / formatted `format()`.
- `print.lago`: compact — recommendation, cost, est outcome, goals, CS size %,
  test p-value, one hint line.
- `summary.lago`: fuller — adds CS cost range + head(cs) + full test stat/df.
- A one-line `format(x)` / compact summary string usable in logs.
- All still return the object invisibly. No dependency changes (cli is Imports).

## Part B - Report generator

New exported `lago_report(x, output_file, output_format, title, ...)`:
- Bundled parameterized R Markdown template under `inst/rmarkdown/` (or
  `inst/report/`), knit via `rmarkdown::render()`.
- Sections from `lago_blocks(x)` + the `plot.lago(x)` confidence-set figure +
  a session-info footer.
- Guards: `rlang::check_installed`/`requireNamespace` for rmarkdown; clear
  `cli_abort` if the object lacks a CS and a plot was requested; returns the
  output path invisibly.
- rmarkdown + knitr already in Suggests; no new hard dependency.

## Open decisions (need sign-off before unattended run)

D1. **Report output format.**
    (a) HTML only via rmarkdown — zero external tooling, always works. [safe]
    (b) HTML + PDF — PDF needs a LaTeX engine (tinytex); adds a soft dep and a
        failure mode on machines without LaTeX.
    (c) Quarto — needs the external `quarto` CLI installed; nicer output, more
        setup.

D2. **In-run output reconciliation (the double-render).**
    (a) Retire the old ASCII `print_output()` and route the non-quiet in-run
        summary through the new shared formatter, so the in-run output and
        `print.lago` are identical. Fixes the double-render. [rec]
    (b) Leave `print_output()` untouched; only restyle the S3 methods. Keeps the
        double, but lower blast radius.

D3. **Export name.** `lago_report()` (rec) vs `render_lago_report()` vs a
    `report()` generic + `report.lago` method.

## Verification plan

- Console: snapshot tests (`testthat::expect_snapshot`) of print/summary for
  binary, continuous, power-goal, single-component, and no-CS objects.
- Report: render to a tempfile in a test; assert the file exists, is non-empty,
  and contains the rec-intervention value + goal labels. `skip_on_cran` +
  `skip_if_not_installed("rmarkdown")`.
- Full `R CMD check`; pkgdown builds; new function documented + in _pkgdown.yml
  reference index + NEWS entry.

## Process

Design -> 3 build agents (formatter / console / report) -> each output gets 2
adversarial Sr-SDE reviewers -> reconcile -> R CMD check green -> push branch
`feature/results-presentation`. PR left for user to open.
