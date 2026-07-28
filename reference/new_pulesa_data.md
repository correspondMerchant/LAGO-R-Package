# PULESA data for running the "new" model (need to be deleted before publishing the R pacakge).

PULESA data for running the "new" model (need to be deleted before
publishing the R pacakge).

## Usage

``` r
new_pulesa_data
```

## Format

A data frame with 192 rows and 15 variables:

- Clinic:
- Period:
- AccessBPMachines:
- AccessMedicines:
- DeliveryA:
- DeliveryB:
- HypertensionTraining:
- MI_W5:
- MI_W7:
- MI_W8:
- PerformanceImprovement:
- RemoteMonitoring:
- Success:
- Total_visit:
- W1:
- W2:
- W3:
- W4:
- W5:
- W6:
- W7:
- W8:

## Examples

``` r
data(main_pulesa_data)
head(main_pulesa_data)
#>         Clinic Period AccessMedicines AccessBPMachines HypertensionTraining
#> 1 Butabika NRH      2        1.887500        0.5103734              0.00000
#> 2 Butabika NRH      3        2.970289        0.5257143              3.01875
#> 3 Butabika NRH      4        3.563636        0.5892857              3.17500
#> 4 Butabika NRH      5        4.611636        0.7842640              3.55000
#> 5 Butabika NRH      6        3.841538        0.8128655              4.55000
#> 6 Butabika NRH      7        3.688525        0.7904762              4.67125
#>   DeliveryA  DeliveryB MI_DeliveryB RemoteMonitoring MI_RemoteMonitoring
#> 1 0.8750000 0.00000000            0             0.00                   0
#> 2 0.7380952 0.29270018            1             0.00                   0
#> 3 0.6666667 0.29270018            1             0.00                   0
#> 4 0.6507937 0.03174603            0             0.25                   0
#> 5 0.6753247 0.29270018            1             0.25                   0
#> 6 0.5357143 0.29270018            1             0.25                   0
#>   PerformanceImprovement MI_PerformanceImprovement Success Total_visit
#> 1              0.0000000                         0      76         249
#> 2              0.4348131                         1     131         401
#> 3              1.0000000                         0     159         394
#> 4              1.0000000                         0     249         466
#> 5              0.0000000                         0     216         393
#> 6              1.0000000                         0     188         360
#>   Proportions
#> 1   0.3052209
#> 2   0.3266833
#> 3   0.4035533
#> 4   0.5343348
#> 5   0.5496183
#> 6   0.5222222
```
