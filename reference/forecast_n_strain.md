# Forecast for a single model and summarise

Forecast for a single model and summarise

## Usage

``` r
forecast_n_strain(
  data,
  model = NULL,
  inits = forecast.vocs::fv_inits,
  fit = forecast.vocs::fv_sample,
  posterior = forecast.vocs::fv_tidy_posterior,
  extract_forecast = forecast.vocs::fv_extract_forecast,
  strains = 2,
  voc_label = "VOC",
  probs = c(0.05, 0.2, 0.8, 0.95),
  digits = 3,
  scale_r = 1,
  timespan = 7,
  ...
)
```

## Arguments

- data:

  A list of data as produced by
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md).

- model:

  A `cmdstanr` model object as loaded by
  [`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md).

- inits:

  A function that returns a function to samples initial conditions with
  the same arguments as
  [`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md).
  If not supplied the package default
  [`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md) is
  used.

- fit:

  A function that fits the supplied model with the same arguments and
  return values as
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).
  If not supplied the package default
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md)
  is used which performs MCMC sampling using cmdstanr.

- posterior:

  A function that summarises the output from the supplied fitting
  function with the same arguments and return values (depending on the
  requirement for downstream package functionality to function) as
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md).
  If not supplied the package default
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md)
  is used.

- extract_forecast:

  A function that extracts the forecast from the summarised `posterior`.
  If not supplied the package default
  [`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md)
  is used.

- strains:

  Integer number of strains. Defaults to 2. Current maximum is 2.

- voc_label:

  A character string, default to "VOC". Defines the label to assign to
  variant of concern specific parameters. Example usage is to rename
  parameters to use variant specific terminology.

- probs:

  A vector of numeric probabilities to produce quantile summaries for.
  By default these are the 5%, 20%, 80%, and 95% quantiles which are
  also the minimum set required for plotting functions to work (such as
  [`plot_cases()`](epiforecasts.io/forecast.vocs/reference/plot_cases.md),
  [`plot_rt()`](epiforecasts.io/forecast.vocs/reference/plot_rt.md), and
  [`plot_voc_frac()`](epiforecasts.io/forecast.vocs/reference/plot_voc_frac.md)).

- digits:

  Numeric, defaults to 3. Number of digits to round summary statistics
  to.

- scale_r:

  Numeric, defaults to 1. Rescale the timespan over which the growth
  rate and reproduction number is calculated. An example use case is
  rescaling the growth rate from weekly to be scaled by the mean of the
  generation time (for COVID-19 for example this would be 5.5 / 7.

- timespan:

  Integer, defaults to 7. Indicates the number of days between each
  observation. Defaults to a week.

- ...:

  Additional parameters passed to
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).

## See also

Functions used for forecasting across models, dates, and scenarios
[`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md),
[`forecast_across_scenarios()`](epiforecasts.io/forecast.vocs/reference/forecast_across_scenarios.md),
[`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`summary.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/summary.fv_forecast.md),
[`unnest_posterior()`](epiforecasts.io/forecast.vocs/reference/unnest_posterior.md)
