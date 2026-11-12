# Define data availability scenarios

Define data availability scenarios

## Usage

``` r
define_scenarios(
  seq_lag = 0:3,
  seq_samples = seq(1, by = -0.25, length.out = 4),
  voc_scale = list(c(0, 0.5))
)
```

## Arguments

- seq_lag:

  The number of weeks that sequences lag the date. Default is to test 0
  to 3 weeks of lag.

- seq_samples:

  Fraction of samples to include (deterministic scaling). The default is
  to test all samples down to 25% of samples by 25% increments.

- voc_scale:

  A list of mean and standard deviations to use to inform the prior for
  additional transmissibility of the VOC variant. The default a
  uninformed no prior knowledge prior (0, 0.5). adjusted growth (0.74,
  0.1).

## Value

A data frame of scenario definitions with ids

## See also

Functions to define and create data scenarios
[`generate_obs_scenario()`](epiforecasts.io/forecast.vocs/reference/generate_obs_scenario.md),
[`update_obs_availability()`](epiforecasts.io/forecast.vocs/reference/update_obs_availability.md)

## Examples

``` r
define_scenarios()
#>        id seq_lag seq_samples voc_scale
#>     <int>   <int>       <num>    <list>
#>  1:     1       0        1.00   0.0,0.5
#>  2:     2       0        0.75   0.0,0.5
#>  3:     3       0        0.50   0.0,0.5
#>  4:     4       0        0.25   0.0,0.5
#>  5:     5       1        1.00   0.0,0.5
#>  6:     6       1        0.75   0.0,0.5
#>  7:     7       1        0.50   0.0,0.5
#>  8:     8       1        0.25   0.0,0.5
#>  9:     9       2        1.00   0.0,0.5
#> 10:    10       2        0.75   0.0,0.5
#> 11:    11       2        0.50   0.0,0.5
#> 12:    12       2        0.25   0.0,0.5
#> 13:    13       3        1.00   0.0,0.5
#> 14:    14       3        0.75   0.0,0.5
#> 15:    15       3        0.50   0.0,0.5
#> 16:    16       3        0.25   0.0,0.5
```
