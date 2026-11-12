# Convert to stanfit object

Convert to stanfit object

## Usage

``` r
convert_to_stanfit(fit)
```

## Arguments

- fit:

  List of output as returned by
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).

## Value

The model fit as a `stanfit` object

## See also

Functions used for postprocessing of model fits
[`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md),
[`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md),
[`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md),
[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
[`link_dates_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_dates_with_posterior.md),
[`link_obs_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_obs_with_posterior.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`print.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/print.fv_posterior.md),
[`quantiles_to_long()`](epiforecasts.io/forecast.vocs/reference/quantiles_to_long.md),
[`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md),
[`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)

## Examples

``` r
if (FALSE) { # interactive()
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = as.Date("2021-06-12"),
)
dt <- fv_as_data_list(obs)
inits <- fv_inits(dt)
fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
convert_to_stanfit(fit)
}
```
