# Forecast across multiple scenarios and dates

Forecast across multiple scenarios and dates

## Usage

``` r
forecast_across_scenarios(obs, scenarios, ...)
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

- scenarios:

  A `data.frame` of scenarios as produced by
  [`define_scenarios()`](epiforecasts.io/forecast.vocs/reference/define_scenarios.md).
  If an `obs` variable is present this is used as the scenario data but
  otherwise
  [`generate_obs_scenario()`](epiforecasts.io/forecast.vocs/reference/generate_obs_scenario.md)
  is used to generate this data from the other variables in `scenarios`.

- ...:

  Additional parameters passed to
  [`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md).

## Value

A data table each rows containing the output from running
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) on a
single scenario for a single forecast date.

## See also

Functions used for forecasting across models, dates, and scenarios
[`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md),
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

scenarios <- define_scenarios(
  voc_scale = list(c(0, 0.5), c(0.5, 0.25)),
  seq_lag = 1,
  seq_samples = 1
)
scenarios

forecasts <- forecast_across_scenarios(
  germany_covid19_delta_obs,
  scenarios,
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
  facet_grid(vars(forecast_date))
}
```
