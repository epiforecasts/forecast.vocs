# Update observations based on availability

Update observations based on availability

## Usage

``` r
update_obs_availability(obs, cases_lag, seq_lag)
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

- cases_lag:

  Number of weeks that case data takes to be reported. Defaults to not
  alter the input data.

- seq_lag:

  Number of weeks that sequence data takes to be reported. Defaults to
  not alter the input data.

## Value

A `data.frame` of observations with updated case and sequence
availability dates.

## See also

Functions to define and create data scenarios
[`define_scenarios()`](epiforecasts.io/forecast.vocs/reference/define_scenarios.md),
[`generate_obs_scenario()`](epiforecasts.io/forecast.vocs/reference/generate_obs_scenario.md)

## Examples

``` r
update_obs_availability(
  germany_covid19_delta_obs,
  cases_lag = 2, seq_lag = 3
)
#>            date location_name location  cases cases_available seq_total seq_voc
#>          <IDat>        <char>   <char>  <int>          <IDat>     <int>   <int>
#>   1: 2021-03-20       Germany       DE  87328      2021-04-03        NA      NA
#>   2: 2021-03-27       Germany       DE 109442      2021-04-10        NA      NA
#>   3: 2021-04-03       Germany       DE 117965      2021-04-17        NA      NA
#>   4: 2021-04-10       Germany       DE 107223      2021-04-24        NA      NA
#>   5: 2021-04-17       Germany       DE 142664      2021-05-01      3757       3
#>  ---                                                                           
#> 110: 2021-08-14       Germany       DE  28646      2021-08-28        NA      NA
#> 111: 2021-08-21       Germany       DE  44862      2021-09-04        NA      NA
#> 112: 2021-08-28       Germany       DE  62984      2021-09-11        NA      NA
#> 113: 2021-09-04       Germany       DE  71057      2021-09-18        NA      NA
#> 114: 2021-09-11       Germany       DE  75107      2021-09-25        NA      NA
#>         share_voc seq_available
#>             <num>        <IDat>
#>   1:           NA          <NA>
#>   2:           NA          <NA>
#>   3:           NA          <NA>
#>   4:           NA          <NA>
#>   5: 0.0007985094    2021-05-08
#>  ---                           
#> 110:           NA          <NA>
#> 111:           NA          <NA>
#> 112:           NA          <NA>
#> 113:           NA          <NA>
#> 114:           NA          <NA>
```
