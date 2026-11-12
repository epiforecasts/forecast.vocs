# Plot the posterior prediction for cases

Plot the posterior prediction for cases

## Usage

``` r
plot_cases(
  posterior,
  obs = NULL,
  forecast_dates = NULL,
  all_obs = FALSE,
  central = FALSE,
  col = NULL,
  log = TRUE
)
```

## Arguments

- posterior:

  A dataframe of posterior output as produced by
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md).
  For forecast dates to be extracted data with `value_type == "cases"`
  must be present.

- obs:

  A data frame of observed data as produced by
  [`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md).

- forecast_dates:

  A data.frame in the format produced by
  [`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md)
  (with at least a date variable and a Data unavailable variable)).
  Specifies when date availability should be add to plots. May contain
  faceting variables.

- all_obs:

  Logical, defaults to `FALSE`. Should all observations be plot or just
  those in the date range of the estimates being plot.

- central:

  Logical, defaults to FALSE. Should the mean and median central
  estimates be plot as dashed and solid lines respectively. Requires
  `mean` and `median` variables to be present in the input.

- col:

  A character string denoting the variable to use to stratify the ribbon
  plot. Defaults to "type" which indicates the data stream.

- log:

  Logical, defaults to `TRUE`. Should cases be plot on the log 2 scale?

## Value

A `ggplot2` plot.

## See also

Plotting functions
[`add_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/add_forecast_dates.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
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

# default with log transform
plot_cases(posterior)


# without log transform
plot_cases(posterior, log = FALSE)
```
