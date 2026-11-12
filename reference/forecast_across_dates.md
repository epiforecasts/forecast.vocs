# Forecast across multiple dates

Forecast across multiple dates

## Usage

``` r
forecast_across_dates(
  obs,
  forecast_dates = unique(obs[!is.na(seq_available)])$date[-c(1:3)],
  ...
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

- forecast_dates:

  A list of dates to forecast at.

- ...:

  Additional parameters passed to
  [`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md).

## Value

A `data.table` each row containing the output from running
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) on a
single forecast date.

## See also

Functions used for forecasting across models, dates, and scenarios
[`forecast_across_scenarios()`](epiforecasts.io/forecast.vocs/reference/forecast_across_scenarios.md),
[`forecast_n_strain()`](epiforecasts.io/forecast.vocs/reference/forecast_n_strain.md),
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`summary.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/summary.fv_forecast.md),
[`unnest_posterior()`](epiforecasts.io/forecast.vocs/reference/unnest_posterior.md)

## Examples

``` r
if (FALSE) { # interactive()
library(ggplot2)
options(mc.cores = 4)

forecasts <- forecast_across_dates(
  germany_covid19_delta_obs,
  forecast_dates = c(as.Date("2021-05-01"), as.Date("2021-06-12")),
  horizon = 4,
  strains = 2,
  adapt_delta = 0.99,
  max_treedepth = 15,
  variant_relationship = "scaled"
)

# inspect forecasts
forecasts

# unnest posteriors
posteriors <- unnest_posterior(forecasts)

# plot case posterior predictions
plot_cases(posteriors, log = TRUE) +
  facet_grid(vars(forecast_date), vars(voc_scale))
}
```
