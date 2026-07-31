# Summarize a LAGO optimization result

Renders the same full console display as \[print.lago()\]: the inputs
recap, outcome-model coefficient table, overall intervention-effect
test, recommended intervention (with cost and the estimated-outcome CI),
and the confidence set. Provided so \`summary()\` works as expected on a
"lago" object.

## Usage

``` r
# S3 method for class 'lago'
summary(object, ...)
```

## Arguments

- object:

  A "lago" object returned by \[lago_optimization()\].

- ...:

  Ignored.

## Value

\`object\`, invisibly.
