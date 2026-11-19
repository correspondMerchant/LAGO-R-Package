# PULESA hypertension-care data (new model)

Clinic-level data from the PULESA study, a cluster-randomized trial of a
multi-component intervention to improve hypertension care. Each row is
one clinic in one time period. This is the version used to fit the "new"
LAGO outcome model, which uses the same clinics and periods as
[`main_pulesa_data`](https://correspondmerchant.github.io/LAGO-R-Package/reference/main_pulesa_data.md)
but a different specification of the intervention (the `W1` to `W8`
components and the missing value indicators `MI_W5`, `MI_W7` and
`MI_W8`).

## Usage

``` r
new_pulesa_data
```

## Format

A data frame with 192 rows and 22 variables:

- Clinic:

  Clinic (cluster) identifier

- Period:

  Study time period

- AccessBPMachines:

  Intervention component: access to blood-pressure machines

- AccessMedicines:

  Intervention component: access to medicines

- DeliveryA:

  Intervention component: care delivery mode A

- DeliveryB:

  Intervention component: care delivery mode B. Contains `NA` in 24 rows

- HypertensionTraining:

  Intervention component: hypertension training

- MI_W5:

  Missing value indicator for `W5`: 1 when `DeliveryB` is `NA` and `W5`
  holds the constant fill value 0.4800 (24 rows), 0 otherwise

- MI_W7:

  Missing value indicator for `W7`: 1 when `RemoteMonitoring` is `NA`
  and `W7` holds the constant fill value 0.2416 (11 rows), 0 otherwise

- MI_W8:

  Missing value indicator for `W8`: 1 when `PerformanceImprovement` is
  `NA` and `W8` holds the constant fill value 0.4348 (17 rows), 0
  otherwise

- PerformanceImprovement:

  Intervention component: performance improvement. Contains `NA` in 17
  rows

- RemoteMonitoring:

  Intervention component: remote monitoring. Contains `NA` in 11 rows

- Success:

  Outcome: number of successes at the clinic-period

- Total_visit:

  Number of visits at the clinic-period

- W1:

  Intervention component W1

- W2:

  Intervention component W2

- W3:

  Intervention component W3

- W4:

  Intervention component W4

- W5:

  Intervention component W5

- W6:

  Intervention component W6

- W7:

  Intervention component W7

- W8:

  Intervention component W8

## Details

Unlike in
[`main_pulesa_data`](https://correspondmerchant.github.io/LAGO-R-Package/reference/main_pulesa_data.md),
the missing values of `DeliveryB` (24 rows), `RemoteMonitoring` (11
rows) and `PerformanceImprovement` (17 rows) are kept as `NA` here. The
matching components `W5`, `W7` and `W8` have no missing values: in those
rows they hold a constant fill value, and the `MI_W5`, `MI_W7` and
`MI_W8` columns flag them.

## Examples

``` r
data(new_pulesa_data)
head(new_pulesa_data)
#>         Clinic Period       W1        W2      W3       W4        W5         W6
#> 1 Butabika NRH      2 1.887500 0.5103734 0.00000 1.651563 0.0000000 0.00000000
#> 2 Butabika NRH      3 2.970289 0.5257143 3.01875 2.192356 0.4800151 0.28930302
#> 3 Butabika NRH      4 3.563636 0.5892857 3.17500 2.375758 0.4800151 0.28930302
#> 4 Butabika NRH      5 4.611636 0.7842640 3.55000 3.001223 0.1464011 0.09527693
#> 5 Butabika NRH      6 3.841538 0.8128655 4.55000 2.594286 0.4800151 0.28930302
#> 6 Butabika NRH      7 3.688525 0.7904762 4.67125 1.975995 0.4800151 0.28930302
#>          W7        W8 MI_W5 MI_W7 MI_W8 AccessMedicines AccessBPMachines
#> 1 0.0000000 0.0000000     0     0     0        1.887500        0.5103734
#> 2 0.0000000 0.4348131     1     0     1        2.970289        0.5257143
#> 3 0.0000000 1.0000000     1     0     0        3.563636        0.5892857
#> 4 0.1960660 1.0000000     0     0     0        4.611636        0.7842640
#> 5 0.2032164 0.0000000     1     0     0        3.841538        0.8128655
#> 6 0.1976190 1.0000000     1     0     0        3.688525        0.7904762
#>   HypertensionTraining DeliveryA  DeliveryB RemoteMonitoring
#> 1              0.00000 0.8750000 0.00000000             0.00
#> 2              3.01875 0.7380952         NA             0.00
#> 3              3.17500 0.6666667         NA             0.00
#> 4              3.55000 0.6507937 0.03174603             0.25
#> 5              4.55000 0.6753247         NA             0.25
#> 6              4.67125 0.5357143         NA             0.25
#>   PerformanceImprovement Success Total_visit
#> 1                      0      76         249
#> 2                     NA     131         401
#> 3                      1     159         394
#> 4                      1     249         466
#> 5                      0     216         393
#> 6                      1     188         360
```
