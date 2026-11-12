# Forecast using branching processes at a target date

Forecast using branching processes at a target date

## Usage

``` r
forecast(
  obs,
  forecast_date = max(obs$date),
  seq_date = forecast_date,
  case_date = forecast_date,
  data_list = forecast.vocs::fv_as_data_list,
  inits = forecast.vocs::fv_inits,
  fit = forecast.vocs::fv_sample,
  posterior = forecast.vocs::fv_tidy_posterior,
  extract_forecast = forecast.vocs::fv_extract_forecast,
  horizon = 4,
  r_init = c(0, 0.25),
  r_step = 1,
  r_forecast = TRUE,
  beta = c(0, 0.1),
  lkj = 0.5,
  period = NULL,
  special_periods = c(),
  voc_scale = c(0, 0.2),
  voc_label = "VOC",
  strains = 2,
  variant_relationship = "correlated",
  overdispersion = TRUE,
  models = NULL,
  likelihood = TRUE,
  output_loglik = FALSE,
  debug = FALSE,
  keep_fit = TRUE,
  scale_r = 1,
  digits = 3,
  timespan = 7,
  probs = c(0.05, 0.2, 0.8, 0.95),
  id = 0,
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

- forecast_date:

  Date at which to forecast. Defaults to the maximum date in `obs`.

- seq_date:

  Date from which to use available sequence data. Defaults to the
  `date`.

- case_date:

  Date from which to use available case data. Defaults to the `date`.

- data_list:

  A function that returns a list of data as ingested by the `inits` and
  `fit` function. Must use arguments as defined in
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md).
  If not supplied the package default
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md)
  is used.

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

- horizon:

  Integer forecast horizon. Defaults to 4.

- r_init:

  Numeric vector of length 2. Mean and standard deviation for the normal
  prior on the initial log growth rate.

- r_step:

  Integer, defaults to 1. The number of observations between each change
  in the growth rate.

- r_forecast:

  Logical, defaults `TRUE`. Should the growth rate be forecast beyond
  the data horizon.

- beta:

  Numeric vector, defaults to c(0, 0.5). Represents the mean and
  standard deviation of the normal prior (truncated at 1 and -1) on the
  weighting in the differenced AR process of the previous difference.
  Placing a tight prior around zero effectively reduces the AR process
  to a random walk on the growth rate.

- lkj:

  Numeric defaults to 0.5. The assumed prior covariance between variants
  growth rates when using the "correlated" model. This sets the shape
  parameter for the Lewandowski-Kurowicka-Joe (LKJ) prior distribution.
  If set to 1 assigns a uniform prior for all correlations, values less
  than 1 indicate increased belief in strong correlations and values
  greater than 1 indicate increased belief weaker correlations. Our
  default setting places increased weight on some correlation between
  strains.

- period:

  Logical defaults to `NULL`. If specified should be a function that
  accepts a vector of dates. This can be used to assign periodic effects
  to dates which will then be adjusted for in the case model. An example
  is adjusting for day of the week effects for which the
  [`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md)
  can be used.

- special_periods:

  A vector of dates to pass to the `period` function argument with the
  same name to be treated as "special" for example holidays being
  treated as sundays in
  [`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md).

- voc_scale:

  Numeric vector of length 2. Prior mean and standard deviation for the
  initial growth rate modifier due to the variant of concern.

- voc_label:

  A character string, default to "VOC". Defines the label to assign to
  variant of concern specific parameters. Example usage is to rename
  parameters to use variant specific terminology.

- strains:

  Integer number of strains to use. Defaults to 2. Current maximum is 2.
  A numeric vector can be passed if forecasts from multiple strain
  models are desired.

- variant_relationship:

  Character string, defaulting to "correlated". Controls the
  relationship of strains with options being "correlated" (strains
  growth rates are correlated over time), "scaled" (a fixed scaling
  between strains), and "independent" (fully independent strains after
  initial scaling).

- overdispersion:

  Logical, defaults to `TRUE`. Should the observations used include
  overdispersion.

- models:

  A model as supplied by
  [`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md).
  If not supplied the default for that strain is used. If multiple
  strain models are being forecast then `models` should be a list
  models.

- likelihood:

  Logical, defaults to `TRUE`. Should the likelihood be included in the
  model

- output_loglik:

  Logical, defaults to `FALSE`. Should the log-likelihood be output.
  Disabling this will speed up fitting if evaluating the model fit is
  not required.

- debug:

  Logical, defaults to `FALSE`. Should within model debug information be
  returned.

- keep_fit:

  Logical, defaults to `TRUE`. Should the stan model fit be kept and
  returned. Dropping this can substantially reduce memory usage in
  situations where multiple models are being fit.

- scale_r:

  Numeric, defaults to 1. Rescale the timespan over which the growth
  rate and reproduction number is calculated. An example use case is
  rescaling the growth rate from weekly to be scaled by the mean of the
  generation time (for COVID-19 for example this would be 5.5 / 7.

- digits:

  Numeric, defaults to 3. Number of digits to round summary statistics
  to.

- timespan:

  Integer, defaults to 7. Indicates the number of days between each
  observation. Defaults to a week.

- probs:

  A vector of numeric probabilities to produce quantile summaries for.
  By default these are the 5%, 20%, 80%, and 95% quantiles which are
  also the minimum set required for plotting functions to work (such as
  [`plot_cases()`](epiforecasts.io/forecast.vocs/reference/plot_cases.md),
  [`plot_rt()`](epiforecasts.io/forecast.vocs/reference/plot_rt.md), and
  [`plot_voc_frac()`](epiforecasts.io/forecast.vocs/reference/plot_voc_frac.md)).

- id:

  ID to assign to this forecast. Defaults to 0.

- ...:

  Additional parameters passed to
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).

## Value

A `data.frame` containing the output of
[`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md) in
each row as well as the summarised posterior, forecast and information
about the parameters specified.

## See also

Functions used for forecasting across models, dates, and scenarios
[`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md),
[`forecast_across_scenarios()`](epiforecasts.io/forecast.vocs/reference/forecast_across_scenarios.md),
[`forecast_n_strain()`](epiforecasts.io/forecast.vocs/reference/forecast_n_strain.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`summary.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/summary.fv_forecast.md),
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

# plot case posterior predictions
plot(forecasts, log = TRUE)

# plot voc posterior predictions
plot(forecasts, type = "voc_frac")

# extract the case forecast
summary(forecasts, type = "cases", forecast = TRUE)
}
```
