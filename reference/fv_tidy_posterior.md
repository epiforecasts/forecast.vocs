# Summarise the posterior tidily

A very opinionated wrapper around
[`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)
with cleaning and tidying including linking to observed data, tidying
parameter names, and transforming parameters for interpretability. See
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md)
for a more generic solution.

## Usage

``` r
fv_tidy_posterior(
  fit,
  probs = c(0.05, 0.2, 0.8, 0.95),
  digits = 3,
  voc_label = "VOC",
  scale_r = 1,
  timespan = 7
)
```

## Arguments

- fit:

  List of output as returned by
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).

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

- voc_label:

  A character string, default to "VOC". Defines the label to assign to
  variant of concern specific parameters. Example usage is to rename
  parameters to use variant specific terminology.

- scale_r:

  Numeric, defaults to 1. Rescale the timespan over which the growth
  rate and reproduction number is calculated. An example use case is
  rescaling the growth rate from weekly to be scaled by the mean of the
  generation time (for COVID-19 for example this would be 5.5 / 7.

- timespan:

  Integer, defaults to 7. Indicates the number of days between each
  observation. Defaults to a week.

## Value

A dataframe summarising the model posterior. Output is stratified by
`value_type` with posterior summaries by case, voc, voc advantage vs
non-voc over time, rt, growth, model, and the raw posterior summary.

## See also

Functions used for postprocessing of model fits
[`convert_to_stanfit()`](epiforecasts.io/forecast.vocs/reference/convert_to_stanfit.md),
[`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md),
[`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md),
[`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md),
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
options(mc.cores = 4)
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = as.Date("2021-06-12"),
)
dt <- fv_as_data_list(obs)
inits <- fv_inits(dt)
fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
fv_tidy_posterior(fit)
}
```
