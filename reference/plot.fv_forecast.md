# Plot method for forecast

`plot` method for class "fv_forecast". The type of plot produced can be
controlled using the `target` and `type` arguments with the latter only
being functional when `target` is set to "posterior" or "forecast".

## Usage

``` r
# S3 method for class 'fv_forecast'
plot(x, obs = NULL, target = "posterior", type = "cases", ...)
```

## Arguments

- x:

  A `data.table` of output as produced by
  [`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) of
  class "fv_forecast".

- obs:

  A data frame of observed data as produced by
  [`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md).

- target:

  A character string indicating the target object within the
  [`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) to
  produce plots for. Current options are: posterior predictions
  ("posterior"), posterior forecasts ("forecast"), and the model fit
  ("fit"). When "posterior" or "forecast" are used then
  [`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md)
  is called whereas when "fit" is used
  [`plot_pairs()`](epiforecasts.io/forecast.vocs/reference/plot_pairs.md)
  is used.

- type:

  A character string indicating the type of plot required, defaulting to
  "cases". Current options are: "cases" which calls
  [`plot_cases()`](epiforecasts.io/forecast.vocs/reference/plot_cases.md),
  "voc_frac" which calls
  [`plot_voc_frac()`](epiforecasts.io/forecast.vocs/reference/plot_voc_frac.md),
  "voc_advantage" which calls
  [`plot_voc_advantage()`](epiforecasts.io/forecast.vocs/reference/plot_voc_advantage.md),
  "growth" which calls
  [`plot_growth()`](epiforecasts.io/forecast.vocs/reference/plot_growth.md),
  "rt" which calls
  [`plot_rt()`](epiforecasts.io/forecast.vocs/reference/plot_rt.md), and
  "all" which produces a list of all plots by call
  [`plot_posterior()`](epiforecasts.io/forecast.vocs/reference/plot_posterior.md).

- ...:

  Pass additional arguments to lower level plot functions.

## Value

`ggplot2` object

## See also

plot.fv_posterior

Functions used for forecasting across models, dates, and scenarios
[`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md),
[`forecast_across_scenarios()`](epiforecasts.io/forecast.vocs/reference/forecast_across_scenarios.md),
[`forecast_n_strain()`](epiforecasts.io/forecast.vocs/reference/forecast_n_strain.md),
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md),
[`summary.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/summary.fv_forecast.md),
[`unnest_posterior()`](epiforecasts.io/forecast.vocs/reference/unnest_posterior.md)

Plotting functions
[`add_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/add_forecast_dates.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`plot_cases()`](epiforecasts.io/forecast.vocs/reference/plot_cases.md),
[`plot_default()`](epiforecasts.io/forecast.vocs/reference/plot_default.md),
[`plot_growth()`](epiforecasts.io/forecast.vocs/reference/plot_growth.md),
[`plot_pairs()`](epiforecasts.io/forecast.vocs/reference/plot_pairs.md),
[`plot_posterior()`](epiforecasts.io/forecast.vocs/reference/plot_posterior.md),
[`plot_rt()`](epiforecasts.io/forecast.vocs/reference/plot_rt.md),
[`plot_theme()`](epiforecasts.io/forecast.vocs/reference/plot_theme.md),
[`plot_voc_advantage()`](epiforecasts.io/forecast.vocs/reference/plot_voc_advantage.md),
[`plot_voc_frac()`](epiforecasts.io/forecast.vocs/reference/plot_voc_frac.md),
[`save_plots()`](epiforecasts.io/forecast.vocs/reference/save_plots.md)

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

# plot case posterior predictions
plot(forecasts, log = TRUE)

# plot case posterior predictions with central estimates
plot(forecasts, log = TRUE, central = TRUE)

# plot voc posterior predictions
plot(forecasts, type = "voc_frac")
}
```
