# Print a LAGO optimization result

Concise console display of the object returned by
\[lago_optimization()\]: the recommended intervention (component and
value), its cost, the estimated outcome, and, when available, the
confidence-set size and whether a power goal or overall test was used.
Rendered with boxed, colour-accented \[cli\]\[cli::cli\] sections
through the shared presentation formatter so it never drifts from
\[summary.lago()\] or the in-run summary.

## Usage

``` r
# S3 method for class 'lago'
print(x, ...)
```

## Arguments

- x:

  A "lago" object returned by \[lago_optimization()\].

- ...:

  Ignored.

## Value

\`x\`, invisibly.
