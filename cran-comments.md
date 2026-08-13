# cran-comments

## Submission

This is a new submission.

The package was previously developed under the identifier `LAGO`. That name
clashes (case-insensitively) with the archived CRAN package `lago`, so the
package is submitted as `LAGOtrials`. The exported functions
(`lago_optimization()`, `get_confidence_set()`, `visualize_cost()`,
`lago_report()`) and the "Learn-As-you-GO" (LAGO) method name are unchanged;
only the installable/loadable package identifier differs, so users now call
`library(LAGOtrials)`.

## Test environments

* Local: R 4.5.3 on x86_64 Linux (conda toolchain).
* GitHub Actions: macOS (release), Windows (release), and Linux
  (release, oldrel-1) via the standard `r-lib/actions` R-CMD-check workflow.
* win-builder (release, devel) and mac-builder to be run by the maintainer as
  part of the submission.

## R CMD check results

`R CMD check --as-cran` gives 0 errors, 1 warning, and 2 notes.

* NOTE (CRAN incoming feasibility): flags this as a "New submission" (expected).
  It also reports:
  - A title-case suggestion ("Learn-as-You-Go" rather than "Learn-As-You-Go").
    "Learn-As-you-GO" is the established capitalization of the method name
    (see the cited papers), so the title keeps that form intentionally.
  - A "possibly invalid URL" (HTTP 403) for a New England Journal of Medicine
    DOI link in `man/BB_data.Rd`. The URL is valid; nejm.org returns 403 to
    automated head requests but resolves in a browser.
* NOTE (compilation flags): "Compilation used the following non-portable
  flag(s): -march=nocona". This flag is injected by the local conda R
  toolchain's compiler configuration, not by the package. The package's own
  `src/` sets no compiler flags, and CRAN's build machines use their own
  toolchain, so this note does not reproduce there.
* WARNING (manual/PDF): the local environment is missing `qpdf`, so the
  PDF-manual size check cannot run locally. This is a missing local tool, not
  a package defect; it does not occur on CRAN, and the manual builds from the
  same Rd sources. Locally the package is checked with `--no-manual` to skip
  this step.

No errors are present, and no notes or warnings other than the environment
artifacts above.
