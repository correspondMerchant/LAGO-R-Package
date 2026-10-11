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
  workflow.
* win-builder (release, devel) and mac-builder to be run by the maintainer as
  part of the submission.

## R CMD check results

`R CMD check --as-cran --no-manual` gives 0 errors, 0 warnings, and 2 notes.
The local host has no LaTeX installation, so the PDF manual is not built
locally.

* NOTE (CRAN incoming feasibility): flags this as a "New submission", which is
  expected. Locally the same note also lists
  `https://www.gnu.org/licenses/gpl-3.0` (from README.md) as possibly invalid
  because the connection timed out. That host is unreachable from the local
  network only, and the URL is valid.
* NOTE (compilation flags): "Compilation used the following non-portable
  flag(s): -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2
  -Wp,-D_GLIBCXX_ASSERTIONS -march=x86-64-v2". These flags are injected by the
  local system R's compiler configuration, not by the package. The package's
  own `src/` sets no compiler flags, and CRAN's build machines use their own
  toolchain, so this note does not reproduce there.

No errors or warnings are present, and no notes other than the environment
artifacts above.
