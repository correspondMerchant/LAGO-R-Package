# BetterBirth Data (for binary outcome analysis)

This is the cleaned version of the BetterBirth dataset used in Nevo et
al.

## Usage

``` r
BB_data
```

## Format

A data frame with 6124 rows and 21 variables:

- coaching_updt:

  Number of coaching visits accrued (at time of birth)

- data_flag:

  Indicator of original dataset

- datafeedback:

  Data feedback loop

- launch_duration:

  Duration of checklist launch

- leadership_ITT:

  Leadership engagement, ITT

- leadership_updt:

  Intensity of leadership engagement (at time of birth)

- pre_post:

  Indicator for visits after initial coaching visit

- pilot_phase:

  Phase of the pilot studies

- site_name:

  Site name

- always_checklist:

  Checklist was used in all phases

- sometimes_checklist:

  Checklist was only in some phases

- birth_volume:

  Estimated monthly birth volume

- birth_volume_100:

  Estimated monthly birth volume (divided by 100)

- distance:

  Estimated distance to nearby hospital (km)

- distance_10:

  Estimated distance to nearby hospital (divided by 10)

- staff_nurse:

  Number of ANM, staff nurses, and lady health visitors

- pp2_hand_hygiene:

  fada_op2_handwashing

- pp3_oxytocin_mother:

  fada_op3_oxytocin_administered

- center_type:

  Phase of the pilot studies

- coach5:

  Number of coaching vissts accrued divided by 5

- coach3:

  Number of coaching vissts accrued divided by 3

## Source

[doi:10.1186/s13012-015-0309-y](https://doi.org/10.1186/s13012-015-0309-y)

[doi:10.1056/NEJMoa1701075](https://doi.org/10.1056/NEJMoa1701075)

## Examples

``` r
data(BB_data)
head(BB_data)
#> # A tibble: 6 × 21
#>   coaching_updt data_flag datafeedback launch_duration leadership_ITT
#>           <dbl> <chr>            <dbl>           <dbl>          <dbl>
#> 1             0 rct                  0               0              0
#> 2             0 rct                  0               0              0
#> 3             0 rct                  0               0              0
#> 4             0 rct                  0               0              0
#> 5             0 rct                  0               0              0
#> 6             0 rct                  0               0              0
#> # ℹ 16 more variables: leadership_updt <dbl>, pre_post <dbl>,
#> #   pilot_phase <dbl>, site_name <chr>, always_checklist <dbl>,
#> #   sometimes_checklist <dbl>, birth_volume <dbl>, birth_volume_100 <dbl>,
#> #   distance <dbl>, distance_10 <dbl>, staff_nurse <dbl>,
#> #   pp2_hand_hygiene <dbl>, pp3_oxytocin_mother <dbl>, center_type <chr>,
#> #   coach5 <dbl>, coach3 <dbl>
```
