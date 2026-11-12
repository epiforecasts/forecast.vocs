# Save plots by name

Save plots by name

## Usage

``` r
save_plots(plots, save_path = NULL, type = "png", ...)
```

## Arguments

- plots:

  A named list of `ggplot2` plots.

- save_path:

  A character string indicating where to save plots if required.

- type:

  A character string indicating the format to use to save plots.

- ...:

  Additional arguments passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## See also

Plotting functions
[`add_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/add_forecast_dates.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`plot_cases()`](epiforecasts.io/forecast.vocs/reference/plot_cases.md),
[`plot_default()`](epiforecasts.io/forecast.vocs/reference/plot_default.md),
[`plot_growth()`](epiforecasts.io/forecast.vocs/reference/plot_growth.md),
[`plot_pairs()`](epiforecasts.io/forecast.vocs/reference/plot_pairs.md),
[`plot_posterior()`](epiforecasts.io/forecast.vocs/reference/plot_posterior.md),
[`plot_rt()`](epiforecasts.io/forecast.vocs/reference/plot_rt.md),
[`plot_theme()`](epiforecasts.io/forecast.vocs/reference/plot_theme.md),
[`plot_voc_advantage()`](epiforecasts.io/forecast.vocs/reference/plot_voc_advantage.md),
[`plot_voc_frac()`](epiforecasts.io/forecast.vocs/reference/plot_voc_frac.md)

## Examples

``` r
posterior <- fv_example(strains = 2, type = "posterior")
p <- plot(posterior, type = "all")
save_plots(p, save_path = tempdir())
#> Saving 6.67 x 6.67 in image
#> Saving 6.67 x 6.67 in image
#> Saving 6.67 x 6.67 in image
#> Saving 6.67 x 6.67 in image
#> Saving 6.67 x 6.67 in image
#> Saving 6.67 x 6.67 in image
```
