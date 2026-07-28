# BetterBirth Data (for continuous outcome analysis, using EBP proportions as a continuous outcome).

This is the cleaned version of the BetterBirth dataset used in Bing et
al.

## Usage

``` r
BB_proportions
```

## Format

A data frame with 7359 rows and 4 variables:

- coaching_updt:

  Number of coaching visits accrued (at time of birth)

- launch_duration:

  Duration of checklist launch

- birth_volume_100:

  Estimated monthly birth volume (divided by 100)

- EBP_proportions:

  Essential birth practices performed out of all possible birth
  practices measured. We consider this as a continuous outcome.

## Source

<https://arxiv.org/pdf/2307.06552>

## Examples

``` r
data(BB_proportions)
head(BB_proportions)
#>   coaching_updt launch_duration birth_volume_100 EBP_proportions
#> 1             0               0              1.3       0.6000000
#> 2             0               0              1.3       0.2142857
#> 3             0               0              1.3       0.1000000
#> 4             0               0              1.3       0.3000000
#> 5             0               0              1.3       0.2142857
#> 6             0               0              1.3       0.3000000
```
