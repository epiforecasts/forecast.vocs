# Define observed data for a scenario

Define observed data for a scenario

## Usage

``` r
generate_obs_scenario(obs, seq_lag, seq_samples)
```

## Arguments

- obs:

  A dataframe of observations as returned by
  [`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md)
  or similar.

- seq_lag:

  Number, number of weeks to lag sequence data behind date of
  observation.

- seq_samples:

  Fraction of sequence samples to include.

## Value

A `data.frame` of scenario definitions with ids

## See also

Functions to define and create data scenarios
[`define_scenarios()`](epiforecasts.io/forecast.vocs/reference/define_scenarios.md),
[`update_obs_availability()`](epiforecasts.io/forecast.vocs/reference/update_obs_availability.md)

## Examples

``` r
generate_obs_scenario(latest_obs(germany_covid19_delta_obs), 4, 0.1)
#>           date location_name location  cases cases_available seq_total seq_voc
#>         <IDat>        <char>   <char>  <int>          <IDat>     <num>   <num>
#>  1: 2021-03-20       Germany       DE  87328      2021-03-20        NA      NA
#>  2: 2021-03-27       Germany       DE 109442      2021-03-27        NA      NA
#>  3: 2021-04-03       Germany       DE 117965      2021-04-03        NA      NA
#>  4: 2021-04-10       Germany       DE 107223      2021-04-10        NA      NA
#>  5: 2021-04-17       Germany       DE 142664      2021-04-17       411       1
#>  6: 2021-04-24       Germany       DE 145568      2021-04-24       454       3
#>  7: 2021-05-01       Germany       DE 131887      2021-05-01       367       5
#>  8: 2021-05-08       Germany       DE 107141      2021-05-08       455       9
#>  9: 2021-05-15       Germany       DE  77261      2021-05-15       369      10
#> 10: 2021-05-22       Germany       DE  57310      2021-05-22       382      12
#> 11: 2021-05-29       Germany       DE  33052      2021-05-29       278      10
#> 12: 2021-06-05       Germany       DE  22631      2021-06-05       220      18
#> 13: 2021-06-12       Germany       DE  15553      2021-06-12       157      31
#> 14: 2021-06-19       Germany       DE   7659      2021-06-19        99      41
#> 15: 2021-06-26       Germany       DE   5033      2021-06-26        71      45
#> 16: 2021-07-03       Germany       DE   4181      2021-07-03        80      63
#> 17: 2021-07-10       Germany       DE   5067      2021-07-10        74      64
#> 18: 2021-07-17       Germany       DE   7969      2021-07-17        56      52
#> 19: 2021-07-24       Germany       DE  11122      2021-07-24        NA      NA
#> 20: 2021-07-31       Germany       DE  14654      2021-07-31        NA      NA
#> 21: 2021-08-07       Germany       DE  18474      2021-08-07        NA      NA
#> 22: 2021-08-14       Germany       DE  28646      2021-08-14        NA      NA
#> 23: 2021-08-21       Germany       DE  44862      2021-08-21        NA      NA
#> 24: 2021-08-28       Germany       DE  62984      2021-08-28        NA      NA
#> 25: 2021-09-04       Germany       DE  71057      2021-09-04        NA      NA
#> 26: 2021-09-11       Germany       DE  75107      2021-09-11        NA      NA
#>           date location_name location  cases cases_available seq_total seq_voc
#>         <IDat>        <char>   <char>  <int>          <IDat>     <num>   <num>
#>       share_voc seq_available
#>           <num>        <IDat>
#>  1:          NA          <NA>
#>  2:          NA          <NA>
#>  3:          NA          <NA>
#>  4:          NA          <NA>
#>  5: 0.001460565    2021-05-15
#>  6: 0.007490637    2021-05-22
#>  7: 0.014725934    2021-05-29
#>  8: 0.019573345    2021-06-05
#>  9: 0.027386117    2021-06-12
#> 10: 0.031176316    2021-06-19
#> 11: 0.036677454    2021-06-26
#> 12: 0.082385071    2021-07-03
#> 13: 0.197318008    2021-07-10
#> 14: 0.417759839    2021-07-17
#> 15: 0.638810198    2021-07-24
#> 16: 0.785803238    2021-07-31
#> 17: 0.874829932    2021-08-07
#> 18: 0.923758865    2021-08-14
#> 19:          NA          <NA>
#> 20:          NA          <NA>
#> 21:          NA          <NA>
#> 22:          NA          <NA>
#> 23:          NA          <NA>
#> 24:          NA          <NA>
#> 25:          NA          <NA>
#> 26:          NA          <NA>
#>       share_voc seq_available
#>           <num>        <IDat>
```
