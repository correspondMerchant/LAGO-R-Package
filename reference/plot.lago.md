# Plot a LAGO optimization result

Visualizes the 95 intervention it plots the grid points in the
confidence set with the recommended intervention highlighted; for a
single component it plots the confidence interval bounds against the
dose. Requires a confidence set (\`include_confidence_set = TRUE\` in
\[lago_optimization()\]); otherwise it returns invisibly with a message
rather than erroring.

## Usage

``` r
# S3 method for class 'lago'
plot(x, ...)
```

## Arguments

- x:

  A "lago" object returned by \[lago_optimization()\].

- ...:

  Ignored.

## Value

A ggplot object (invisibly) when a plot is produced, otherwise \`NULL\`
invisibly.
