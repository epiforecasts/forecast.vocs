# Summary method for forecast

`summary` method for class "fv_forecast".

## Usage

``` r
# S3 method for class 'fv_forecast'
summary(
  object,
  target = "posterior",
  type = "model",
  as_dt = FALSE,
  forecast = FALSE,
  ...
)
```

## Arguments

- object:

  A `data.table` output from
  [`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) of
  class "fv_forecast".

- target:

  A character string indicating the target object within the
  [`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) to
  summarise. Current options are: posterior predictions ("posterior"),
  posterior forecasts ("forecast"), the model fit ("fit"), and the model
  diagnostics ("diagnostics"). When "posterior" or "forecast" are used
  then
  [`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md)
  is called on the nested posterior or forecast.

- type:

  A character string used to filter the summarised output and defaulting
  to "model". Current options are: "model" which returns a summary of
  key model parameters, "cases" which returns summarised cases,
  "voc_frac" which returns summarised estimates of the fraction of cases
  that have the variant of concern, "voc_advantage" that returns
  summarised estimates of the the transmission advantage of the variant
  of concern, "growth" which returns summarised variant specific and
  overall growth rates, "rt" which returns summarised variant specific
  and overall reproduction number estimates, "raw" which returns a raw
  posterior summary, and "all" which returns all tidied posterior
  estimates.

- as_dt:

  Logical defaults to `FALSE`. Once any filtering has been applied
  should [`summary()`](https://rdrr.io/r/base/summary.html) fall back to
  using the default `data.table` method.

- forecast:

  Logical defaults to `FALSE`. Should
  [`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md)
  be used to return only forecasts rather than complete posterior.

- ...:

  Additional summary arguments.

## Value

A summary `data.table`.

## See also

summary.fv_posterior forecast unnest_posterior

Functions used for forecasting across models, dates, and scenarios
[`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md),
[`forecast_across_scenarios()`](epiforecasts.io/forecast.vocs/reference/forecast_across_scenarios.md),
[`forecast_n_strain()`](epiforecasts.io/forecast.vocs/reference/forecast_n_strain.md),
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`unnest_posterior()`](epiforecasts.io/forecast.vocs/reference/unnest_posterior.md)

## Examples

``` r
if (FALSE) { # interactive()
options(mc.cores = 4)

forecasts <- forecast(
  germany_covid19_delta_obs,
  forecast_date = as.Date("2021-06-12"),
  horizon = 4,
  strains = c(1, 2),
  adapt_delta = 0.99,
  max_treedepth = 15,
  variant_relationship = "scaled"
)
# inspect forecasts
forecasts

# extract the model summary
summary(forecasts, type = "model")

# extract the fit object
summary(forecasts, target = "fit")

# extract the case forecast
summary(forecasts, type = "cases", forecast = TRUE)
}
```
