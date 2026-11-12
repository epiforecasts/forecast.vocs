# Filter data based on availability and forecast date

Filter data based on availability and forecast date

## Usage

``` r
filter_by_availability(
  obs,
  date = max(obs$date),
  seq_date = date,
  case_date = date
)
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

- date:

  Date at which to filter. Defaults to the maximum date in `obs`.

- seq_date:

  Date from which to use available sequence data. Defaults to the
  `date`.

- case_date:

  Date from which to use available case data. Defaults to the `date`.

## Value

A `data.frame` of observations filter for the latest available data for
the specified dates of interest.

## See also

Preprocessing functions
[`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md),
[`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md),
[`piecewise_steps()`](epiforecasts.io/forecast.vocs/reference/piecewise_steps.md)

## Examples

``` r
options(mc.cores = 4)
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = as.Date("2021-06-12"),
)
dt <- rbind(
  update_obs_availability(obs, seq_lag = 3),
  update_obs_availability(obs, seq_lag = 1)
)
# filter out duplicates and up to the present date
filter_by_availability(dt)
#>           date location_name location  cases cases_available seq_total seq_voc
#>         <IDat>        <char>   <char>  <int>          <IDat>     <int>   <int>
#>  1: 2021-03-20       Germany       DE  87328      2021-03-20        NA      NA
#>  2: 2021-03-27       Germany       DE 109442      2021-03-27        NA      NA
#>  3: 2021-04-03       Germany       DE 117965      2021-04-03        NA      NA
#>  4: 2021-04-10       Germany       DE 107223      2021-04-10        NA      NA
#>  5: 2021-04-17       Germany       DE 142664      2021-04-17      4066       5
#>  6: 2021-04-24       Germany       DE 145568      2021-04-24      4494      31
#>  7: 2021-05-01       Germany       DE 131887      2021-05-01      3615      55
#>  8: 2021-05-08       Germany       DE 107141      2021-05-08      4479      86
#>  9: 2021-05-15       Germany       DE  77261      2021-05-15      3399      93
#> 10: 2021-05-22       Germany       DE  57310      2021-05-22      3275     108
#> 11: 2021-05-29       Germany       DE  33052      2021-05-29      1328      34
#> 12: 2021-06-05       Germany       DE  22631      2021-06-05        NA      NA
#> 13: 2021-06-12       Germany       DE  15553      2021-06-12        NA      NA
#>       share_voc seq_available
#>           <num>        <IDat>
#>  1:          NA          <NA>
#>  2:          NA          <NA>
#>  3:          NA          <NA>
#>  4:          NA          <NA>
#>  5: 0.001229710    2021-05-08
#>  6: 0.006898086    2021-05-15
#>  7: 0.015214385    2021-05-22
#>  8: 0.019200714    2021-05-29
#>  9: 0.027360989    2021-06-05
#> 10: 0.032977099    2021-06-12
#> 11: 0.025602410    2021-06-05
#> 12:          NA    2021-06-26
#> 13:          NA    2021-07-03

# filter to only use sequence data up the the 12th of June
filter_by_availability(dt, seq_date = "2021-06-12")
#>           date location_name location  cases cases_available seq_total seq_voc
#>         <IDat>        <char>   <char>  <int>          <IDat>     <int>   <int>
#>  1: 2021-03-20       Germany       DE  87328      2021-03-20        NA      NA
#>  2: 2021-03-27       Germany       DE 109442      2021-03-27        NA      NA
#>  3: 2021-04-03       Germany       DE 117965      2021-04-03        NA      NA
#>  4: 2021-04-10       Germany       DE 107223      2021-04-10        NA      NA
#>  5: 2021-04-17       Germany       DE 142664      2021-04-17      4066       5
#>  6: 2021-04-24       Germany       DE 145568      2021-04-24      4494      31
#>  7: 2021-05-01       Germany       DE 131887      2021-05-01      3615      55
#>  8: 2021-05-08       Germany       DE 107141      2021-05-08      4479      86
#>  9: 2021-05-15       Germany       DE  77261      2021-05-15      3399      93
#> 10: 2021-05-22       Germany       DE  57310      2021-05-22      3275     108
#> 11: 2021-05-29       Germany       DE  33052      2021-05-29      1328      34
#> 12: 2021-06-05       Germany       DE  22631      2021-06-05        NA      NA
#> 13: 2021-06-12       Germany       DE  15553      2021-06-12        NA      NA
#>       share_voc seq_available
#>           <num>        <IDat>
#>  1:          NA          <NA>
#>  2:          NA          <NA>
#>  3:          NA          <NA>
#>  4:          NA          <NA>
#>  5: 0.001229710    2021-05-08
#>  6: 0.006898086    2021-05-15
#>  7: 0.015214385    2021-05-22
#>  8: 0.019200714    2021-05-29
#>  9: 0.027360989    2021-06-05
#> 10: 0.032977099    2021-06-12
#> 11: 0.025602410    2021-06-05
#> 12:          NA    2021-06-26
#> 13:          NA    2021-07-03

# as above but only use
filter_by_availability(dt,
  seq_date = "2021-06-12",
  case_date = "2021-07-01"
)
#>           date location_name location  cases cases_available seq_total seq_voc
#>         <IDat>        <char>   <char>  <int>          <IDat>     <int>   <int>
#>  1: 2021-03-20       Germany       DE  87328      2021-03-20        NA      NA
#>  2: 2021-03-27       Germany       DE 109442      2021-03-27        NA      NA
#>  3: 2021-04-03       Germany       DE 117965      2021-04-03        NA      NA
#>  4: 2021-04-10       Germany       DE 107223      2021-04-10        NA      NA
#>  5: 2021-04-17       Germany       DE 142664      2021-04-17      4066       5
#>  6: 2021-04-24       Germany       DE 145568      2021-04-24      4494      31
#>  7: 2021-05-01       Germany       DE 131887      2021-05-01      3615      55
#>  8: 2021-05-08       Germany       DE 107141      2021-05-08      4479      86
#>  9: 2021-05-15       Germany       DE  77261      2021-05-15      3399      93
#> 10: 2021-05-22       Germany       DE  57310      2021-05-22      3275     108
#> 11: 2021-05-29       Germany       DE  33052      2021-05-29      1328      34
#> 12: 2021-06-05       Germany       DE  22631      2021-06-05        NA      NA
#> 13: 2021-06-12       Germany       DE  15553      2021-06-12        NA      NA
#>       share_voc seq_available
#>           <num>        <IDat>
#>  1:          NA          <NA>
#>  2:          NA          <NA>
#>  3:          NA          <NA>
#>  4:          NA          <NA>
#>  5: 0.001229710    2021-05-08
#>  6: 0.006898086    2021-05-15
#>  7: 0.015214385    2021-05-22
#>  8: 0.019200714    2021-05-29
#>  9: 0.027360989    2021-06-05
#> 10: 0.032977099    2021-06-12
#> 11: 0.025602410    2021-06-05
#> 12:          NA    2021-06-26
#> 13:          NA    2021-07-03
```
