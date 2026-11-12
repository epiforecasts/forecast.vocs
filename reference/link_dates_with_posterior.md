# Link dates by time for posterior parameter estimates

Link dates by time for posterior parameter estimates

## Usage

``` r
link_dates_with_posterior(posterior, data, timespan = 7, mod_end = 0)
```

## Arguments

- posterior:

  A data frame of summarised posterior estimates as returned by
  [`cmdstanr::summary()`](https://mc-stan.org/cmdstanr/reference/fit-method-summary.html)
  with an additional type variable which contains the following
  character string options: "non-VOC", "VOC", "Combined", "Overall".

- data:

  A list of data as returned in the "data" entry of the output returned
  by
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).

- timespan:

  Integer, defaults to 7. Indicates the number of days between each
  observation. Defaults to a week.

- mod_end:

  Integer, defaults to 0. Amount to shift the end date of estimates.

## Value

A posterior `data.frame` with an additional data column.

## See also

Functions used for postprocessing of model fits
[`convert_to_stanfit()`](epiforecasts.io/forecast.vocs/reference/convert_to_stanfit.md),
[`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md),
[`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md),
[`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md),
[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
[`link_obs_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_obs_with_posterior.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`print.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/print.fv_posterior.md),
[`quantiles_to_long()`](epiforecasts.io/forecast.vocs/reference/quantiles_to_long.md),
[`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md),
[`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)
