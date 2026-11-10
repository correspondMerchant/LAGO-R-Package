# cran-comments

## Submission

This is an update of LAGOtrials 1.1.1, which was published on CRAN on
2026-11-04.

It fixes the remaining test failure in the MKL additional check
(https://www.stats.ox.ac.uk/pub/bdr/Rblas/MKL/LAGOtrials.out). One test
compared a sweep with a direct run at a tolerance of 1e-8, and under MKL the
two costs differ by about 2.6e-8 relative. To find every comparison of this
kind at once, we reran the test suite with a relative noise of 1e-8 on the
fitted coefficients, about the size of a BLAS difference. That found 23
comparisons of two separate fits or runs, which now use a tolerance of 1e-5.

It also fixes a bug in the numerical optimizer that showed on Ubuntu 26.04
with OpenBLAS 0.3.32, where the solver returned points outside the
intervention bounds, and it adds `lago_app()`, a Shiny app that runs on the
loopback interface only. The changes are listed in NEWS.md.

## Test environments

* Local: R 4.5.3 on x86_64 Linux (Amazon Linux 2023).
* GitHub Actions: macOS (release), Windows (release), and Linux
  (devel, release, oldrel-1) via the standard `r-lib/actions` R-CMD-check
  workflow, on both the Ubuntu 24.04 and the Ubuntu 26.04 runners. All pass.
* R-hub containers, run locally with docker: `mkl` (Fedora 44, R-devel,
  Intel MKL 2025.3) and `atlas` (Fedora, R-devel, ATLAS). Both give
  `Status: OK`.
* win-builder: R-devel (2026-11-07 r90850) and R-release (4.6.2). Both give
  1 NOTE, explained below.

## R CMD check results

There were no ERRORs or WARNINGs.

win-builder (R-devel and R-release) gives 1 NOTE:

* CRAN incoming feasibility: "Days since last update: 6". This update is
  sent early because it fixes the MKL additional check failure that CRAN
  asked us to fix by 2026-11-21, see above.

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
