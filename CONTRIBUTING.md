# Contributing to LAGOtrials

Thanks for your interest in LAGOtrials. Contributions of all kinds are
welcome, from bug reports to documentation fixes to new features.

## Reporting a bug

Open an issue using the bug-report template. A good report includes:

- what you ran (a minimal
  [`lago_optimization()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/lago_optimization.md)
  /
  [`get_confidence_set()`](https://correspondmerchant.github.io/LAGO-R-Package/reference/get_confidence_set.md)
  call),
- what you expected and what happened instead,
- the output of
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) and the
  LAGOtrials version.

A small reproducible example is the single most helpful thing you can
provide.

## Suggesting a feature

Open an issue using the feature-request template and describe the use
case, not just the mechanism: what analysis are you trying to run that
LAGOtrials does not yet support?

## Making a change

1.  Fork the repository and create a branch off `main`.

2.  Make your change. Keep the existing style: run `styler::style_pkg()`
    and `lintr::lint_package()` if you have them.

3.  Add or update tests under `tests/testthat/`. New behaviour needs a
    test that fails without your change.

4.  Run the checks locally:

    ``` r

    devtools::document()   # if you changed roxygen comments
    devtools::test()       # NOT_CRAN=true runs the full suite
    devtools::check()
    ```

5.  Open a pull request against `main` and fill in the template.

Continuous integration runs `R CMD check` on Linux, macOS and Windows,
reports test coverage to Codecov, and runs the JavaScript math test for
the cost curves. Please make sure the checks pass.

## Code of Conduct

By participating in this project you agree to abide by the [Code of
Conduct](https://correspondmerchant.github.io/LAGO-R-Package/CODE_OF_CONDUCT.md).
