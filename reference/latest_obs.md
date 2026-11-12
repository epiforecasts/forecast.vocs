# Filter for latest observations of all types

Filter for latest observations of all types

## Usage

``` r
latest_obs(obs)
```

## Arguments

- obs:

  A `data.frame` with the following variables: `date`, `cases`,
  `seq_voc`, and `seq_total`, `cases_available`, and `seq_available`.
  `seq_available` and `case_available` must be uniquely define data rows
  but other rows can be duplicated based on data availability. This data
  format allows for multiple versions of case and sequence data for a
  given date with different reporting dates. This is important when
  using the package in evaluation settings or in real-time where data
  sources are liable to be updated as new data becomes available. See
  [germany_covid19_delta_obs](epiforecasts.io/forecast.vocs/reference/germany_covid19_delta_obs.md)
  for an example of a supported data set.

## Value

A `data.frame` of observations filtered for the latest available data.

## See also

Preprocessing functions
[`filter_by_availability()`](epiforecasts.io/forecast.vocs/reference/filter_by_availability.md),
[`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md),
[`piecewise_steps()`](epiforecasts.io/forecast.vocs/reference/piecewise_steps.md)

## Examples

``` r
dt <- rbind(
  update_obs_availability(germany_covid19_delta_obs, seq_lag = 3),
  update_obs_availability(germany_covid19_delta_obs, seq_lag = 1)
)
latest_obs(dt)
#>           date location_name location  cases cases_available seq_total seq_voc
#>         <IDat>        <char>   <char>  <int>          <IDat>     <int>   <int>
#>  1: 2021-03-20       Germany       DE  87328      2021-03-20        NA      NA
#>  2: 2021-03-27       Germany       DE 109442      2021-03-27        NA      NA
#>  3: 2021-04-03       Germany       DE 117965      2021-04-03        NA      NA
#>  4: 2021-04-10       Germany       DE 107223      2021-04-10        NA      NA
#>  5: 2021-04-17       Germany       DE 142664      2021-04-17      3757       3
#>  6: 2021-04-24       Germany       DE 145568      2021-04-24      3957      15
#>  7: 2021-05-01       Germany       DE 131887      2021-05-01      1913      19
#>  8: 2021-05-08       Germany       DE 107141      2021-05-08      2161      37
#>  9: 2021-05-15       Germany       DE  77261      2021-05-15      1676      40
#> 10: 2021-05-22       Germany       DE  57310      2021-05-22      1387      30
#> 11: 2021-05-29       Germany       DE  33052      2021-05-29      1328      34
#> 12: 2021-06-05       Germany       DE  22631      2021-06-05      1136      75
#> 13: 2021-06-12       Germany       DE  15553      2021-06-12       696     116
#> 14: 2021-06-19       Germany       DE   7659      2021-06-19       576     226
#> 15: 2021-06-26       Germany       DE   5033      2021-06-26       452     287
#> 16: 2021-07-03       Germany       DE   4181      2021-07-03       534     407
#> 17: 2021-07-10       Germany       DE   5067      2021-07-10       468     399
#> 18: 2021-07-17       Germany       DE   7969      2021-07-17       564     521
#> 19: 2021-07-24       Germany       DE  11122      2021-07-24        NA      NA
#> 20: 2021-07-31       Germany       DE  14654      2021-07-31        NA      NA
#> 21: 2021-08-07       Germany       DE  18474      2021-08-07        NA      NA
#> 22: 2021-08-14       Germany       DE  28646      2021-08-14        NA      NA
#> 23: 2021-08-21       Germany       DE  44862      2021-08-21        NA      NA
#> 24: 2021-08-28       Germany       DE  62984      2021-08-28        NA      NA
#> 25: 2021-09-04       Germany       DE  71057      2021-09-04        NA      NA
#> 26: 2021-09-11       Germany       DE  75107      2021-09-11        NA      NA
#>           date location_name location  cases cases_available seq_total seq_voc
#>         <IDat>        <char>   <char>  <int>          <IDat>     <int>   <int>
#>        share_voc seq_available
#>            <num>        <IDat>
#>  1:           NA          <NA>
#>  2:           NA          <NA>
#>  3:           NA          <NA>
#>  4:           NA          <NA>
#>  5: 0.0007985094    2021-05-08
#>  6: 0.0037907506    2021-05-15
#>  7: 0.0099320439    2021-05-22
#>  8: 0.0171217029    2021-05-29
#>  9: 0.0238663484    2021-06-05
#> 10: 0.0216294160    2021-06-12
#> 11: 0.0256024096    2021-06-19
#> 12: 0.0660211268    2021-06-26
#> 13: 0.1666666667    2021-07-03
#> 14: 0.3923611111    2021-07-10
#> 15: 0.6349557522    2021-07-17
#> 16: 0.7621722846    2021-07-24
#> 17: 0.8525641026    2021-07-31
#> 18: 0.9237588652    2021-08-07
#> 19:           NA          <NA>
#> 20:           NA          <NA>
#> 21:           NA          <NA>
#> 22:           NA          <NA>
#> 23:           NA          <NA>
#> 24:           NA          <NA>
#> 25:           NA          <NA>
#> 26:           NA          <NA>
#>        share_voc seq_available
#>            <num>        <IDat>
```
