# Plot method for fv_tidy_posterior

`plot` method for class "fv_posterior". This function wraps all lower
level plot functions.

## Usage

``` r
# S3 method for class 'fv_posterior'
plot(
  x,
  obs = NULL,
  type = "cases",
  forecast_dates = NULL,
  central = FALSE,
  all_obs = FALSE,
  voc_label = "variant of concern",
  ...
)
```

## Arguments

- x:

  A `data.table` of output as produced by
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md).

- obs:

  A data frame of observed data as produced by
  [`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md).

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

- forecast_dates:

  A data.frame in the format produced by
  [`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md)
  (with at least a date variable and a Data unavailable variable)).
  Specifies when date availability should be add to plots. May contain
  faceting variables.

- central:

  Logical, defaults to FALSE. Should the mean and median central
  estimates be plot as dashed and solid lines respectively. Requires
  `mean` and `median` variables to be present in the input.

- all_obs:

  Logical, defaults to `FALSE`. Should all observations be plot or just
  those in the date range of the estimates being plot.

- voc_label:

  Character string giving the name to assign to the variant of concern.
  Defaults to "variant of concern".

- ...:

  Pass additional arguments to lower level plot functions.

## Value

`ggplot2` object

## See also

Functions used for postprocessing of model fits
[`convert_to_stanfit()`](epiforecasts.io/forecast.vocs/reference/convert_to_stanfit.md),
[`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md),
[`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md),
[`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md),
[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
[`link_dates_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_dates_with_posterior.md),
[`link_obs_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_obs_with_posterior.md),
[`print.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/print.fv_posterior.md),
[`quantiles_to_long()`](epiforecasts.io/forecast.vocs/reference/quantiles_to_long.md),
[`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md),
[`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)

Plotting functions
[`add_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/add_forecast_dates.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
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
posterior <- fv_example(strains = 2, type = "posterior")

# plot cases on the log scale
plot(posterior, type = "cases", log = TRUE)


# plot cases with central estimates
plot(posterior, type = "cases", log = FALSE, central = TRUE)


# plot fraction that have the variant of concern
plot(posterior, type = "voc_frac")


# plot the transmission advantage for the the variant of concern
plot(posterior, type = "voc_advantage")


# plot the growth rates for both voc and non-voc cases
plot(posterior, type = "growth")


# plot the reproduction number estimates
plot(posterior, type = "rt")
```
