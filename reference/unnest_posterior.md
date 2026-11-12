# Unnest posterior estimates from a forecast data.frame

Unnest posterior predictions and forecasts from output produced by
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) (or
multiple combined calls) dropping diagnostic and fitting variables in
the process.

## Usage

``` r
unnest_posterior(forecasts, target = "posterior")
```

## Arguments

- forecasts:

  A data frame of forecasts as produced by
  [`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md).

- target:

  A character string indicating the list of outputs to unnest.

## Value

An unnested `data.frame` of posterior estimates and other variables
produced by
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md).

## See also

Functions used for forecasting across models, dates, and scenarios
[`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md),
[`forecast_across_scenarios()`](epiforecasts.io/forecast.vocs/reference/forecast_across_scenarios.md),
[`forecast_n_strain()`](epiforecasts.io/forecast.vocs/reference/forecast_n_strain.md),
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`summary.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/summary.fv_forecast.md)

## Examples

``` r
if (FALSE) { # interactive()
library(data.table)
options(mc.cores = 4)
dt <- forecast(
  germany_covid19_delta_obs,
  forecast_date = as.Date("2021-06-12"),
  max_treedepth = 15, adapt_delta = 0.99
)

# unnest posterior predictions
posterior <- unnest_posterior(dt)
posterior

# unnest forecasts
forecasts <- unnest_posterior(dt, target = "forecast")
forecasts
}
```
