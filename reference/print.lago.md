# Print a LAGO optimization result

Full console display of the object returned by \[lago_optimization()\],
rendered with boxed, colour-accented \[cli\]\[cli::cli\] sections: an
inputs recap (data dimensions, outcome, intervention components, model
family/link and fixed effects, goals, costs and bounds), the fitted
outcome-model coefficient table, the overall intervention-effect test,
the recommended intervention with its cost and the estimated outcome
(and its 95 confidence interval), and the confidence set (size, cost
IQR, and first rows). Everything is shown on the console so results can
be read without further calls. \[summary.lago()\] renders the same
output.

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
