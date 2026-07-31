# Summarize a LAGO optimization result

Fuller display than \[print.lago()\]: adds the confidence-set cost range
and first rows, and the overall-test statistic and p-value when present.
Renders through the same shared formatter as \[print.lago()\].

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
