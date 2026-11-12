# Check observations are in the correct format

Check observations are in the correct format

## Usage

``` r
check_observations(obs)
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

## See also

Functions used for checking inputs
[`check_dataframe()`](epiforecasts.io/forecast.vocs/reference/check_dataframe.md),
[`check_param()`](epiforecasts.io/forecast.vocs/reference/check_param.md),
[`check_quantiles()`](epiforecasts.io/forecast.vocs/reference/check_quantiles.md)

## Examples

``` r
obs <- latest_obs(germany_covid19_delta_obs)
check_observations(obs)
```
