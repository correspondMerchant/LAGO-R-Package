# Non-fatal diagnostics for the fitted outcome model

Checks the fitted glm for signs that the fit is unreliable and issues
warnings (never errors) so LAGO optimization can continue. Covers three
Tier-1 checks: 1. glm fit warnings captured during fitting (separation
signal). 2. separation / near-non-identifiability, detected via
extremely large coefficient standard errors relative to the estimates.
3. intervention-component effects that are not statistically
significant, which make the corresponding part of the recommendation
unreliable.

## Usage

``` r
diagnose_model_fit(model, intervention_components, fit_warnings = character(0))
```

## Arguments

- model:

  A fitted glm object.

- intervention_components:

  A character vector of intervention component names (may include
  backticked interaction terms).

- fit_warnings:

  A character vector of warning messages emitted by glm() during
  fitting.

## Value

Invisibly NULL. Called for its side effect of issuing warnings.
