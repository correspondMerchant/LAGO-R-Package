# cran-comments

## Submission

This is an update of LAGOtrials 1.1.0, which was published on CRAN on
2026-10-24.

It fixes the test failures in the MKL additional check
(https://www.stats.ox.ac.uk/pub/bdr/Rblas/MKL/LAGOtrials.out). Six tests
compared the results of two model fits exactly, and under MKL these differ by
about 1e-7. Those comparisons now use a tolerance of 1e-5, which is far below
the changes the tests check for.

It also fixes several results that were silently wrong in 1.1.0, among them
the power goal's control rate, the confidence interval of the weighted average
center on the logit link, and the shrinking method used when the outcome goal
cannot be reached. The changes are listed in NEWS.md.

## Test environments

* Local: R 4.5.3 on x86_64 Linux (Amazon Linux 2023).
* GitHub Actions: macOS (release), Windows (release), and Linux
  (devel, release, oldrel-1) via the standard `r-lib/actions` R-CMD-check
  workflow.
* R-hub containers, run locally with docker: `mkl` (Fedora 44, R-devel
  2026-11-02 r90827, Intel MKL 2025.3) and `atlas` (Fedora, R-devel, ATLAS).
  Both give `Status: OK`.
* win-builder: R-devel and R-release.

## R CMD check results

There were no ERRORs or WARNINGs.

The local check (`R CMD check --as-cran`) gives 3 NOTEs, all from the local
environment:

* Future file timestamps: "unable to verify current time", because the time
  server is unreachable from the local network.
* HTML version of manual: "Skipping checking math rendering: package 'V8'
  unavailable", because V8 is not installed locally.
* Compilation flags: "Compilation used the following non-portable flag(s):
  -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS
  -march=x86-64-v2". These flags come from the local system R's compiler
  configuration, not from the package. The package's own `src/` sets no
  compiler flags, so this note does not reproduce on CRAN.

## Reverse dependencies

There are no reverse dependencies.
