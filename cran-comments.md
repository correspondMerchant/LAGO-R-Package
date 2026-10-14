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

* Local: R 4.5.3 on x86_64 Linux (Amazon Linux 2023).
* GitHub Actions: macOS (release), Windows (release), and Linux
  (devel, release, oldrel-1) via the standard `r-lib/actions` R-CMD-check
  workflow. All five report `Status: OK`.
* win-builder: R-release (4.6.1) and R-devel (2026-10-10 r90659).
* mac-builder: two R-release arm64 runs stopped at the dependency check
  because none of the CRAN dependencies were available on the builder, so they
  did not check the package. The GitHub Actions macOS (arm64) check above
  passes.

## R CMD check results

There were no ERRORs or WARNINGs.

win-builder (release and devel) gives 1 NOTE:

* "New submission".
* "Possibly misspelled words in DESCRIPTION": Bing, LAGO, Lok, Nevo and
  Spiegelman. These are the authors of the cited papers and the name of the
  Learn-As-you-GO (LAGO) method, and are spelled correctly.

The local check (`R CMD check --as-cran --no-manual`) gives 2 NOTEs, both from
the local environment:

* CRAN incoming feasibility: "New submission". It also lists
  `https://www.gnu.org/licenses/gpl-3.0` (from README.md) as possibly invalid
  because the connection timed out. That host is unreachable from the local
  network only, and the URL is valid.
* Compilation flags: "Compilation used the following non-portable flag(s):
  -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS
  -march=x86-64-v2". These flags come from the local system R's compiler
  configuration, not from the package. The package's own `src/` sets no
  compiler flags, so this note does not reproduce on CRAN.

The local host has no LaTeX installation, so the PDF manual is not built
locally. win-builder builds and checks it.
