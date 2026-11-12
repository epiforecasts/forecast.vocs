# Link posterior estimates with observed data

Link posterior estimates with observed data and flag if values are
observed or not.

## Usage

``` r
link_obs_with_posterior(posterior, obs, horizon, target_types)
```

## Arguments

- posterior:

  A data frame of summarised posterior estimates as returned by
  [`cmdstanr::summary()`](https://mc-stan.org/cmdstanr/reference/fit-method-summary.html)
  with an additional type variable which contains the following
  character string options: "non-VOC", "VOC", "Combined", "Overall".

- obs:

  Numeric vector of observed data to link to posterior estimates.

- horizon:

  Integer indicating the horizon of unobserved forecasts. If not
  specified will be inferred from `obs`.

- target_types:

  A character vector of types (as specified in the `type` variable) to
  modify.

## Value

The input `data.frame` combined with `obs` and `observed` variables.

## See also

Functions used for postprocessing of model fits
[`convert_to_stanfit()`](epiforecasts.io/forecast.vocs/reference/convert_to_stanfit.md),
[`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md),
[`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md),
[`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md),
[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
[`link_dates_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_dates_with_posterior.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`print.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/print.fv_posterior.md),
[`quantiles_to_long()`](epiforecasts.io/forecast.vocs/reference/quantiles_to_long.md),
[`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md),
[`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)
