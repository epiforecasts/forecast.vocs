# Pairs plot of parameters of interest and fitting diagnostics

Pairs plot of parameters of interest and fitting diagnostics

## Usage

``` r
plot_pairs(
  fit,
  pars = c("r_init", "r_scale", "beta", "voc_beta", "voc_scale[1]", "init_cases[1]",
    "init_cases[2]", "eta[1]", "voc_eta[1]", "sqrt_phi[1]", "sqrt_phi[2]", "sqrt_phi"),
  diagnostics = TRUE,
  ...
)
```

## Arguments

- fit:

  List of output as returned by
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).

- pars:

  Character vector of parameters to try and include in the plot. Will
  only be included if present in the fitted model.

- diagnostics:

  Logical, defaults to `TRUE`. Should fitting diagnostics be returned as
  a `data.frame`.

- ...:

  Additional parameters passed to
  [`bayesplot::mcmc_pairs()`](https://mc-stan.org/bayesplot/reference/MCMC-scatterplots.html).

## Value

A `ggplot2` based pairs plot of parameters of interest

## See also

Plotting functions
[`add_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/add_forecast_dates.md),
[`plot.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`plot_cases()`](epiforecasts.io/forecast.vocs/reference/plot_cases.md),
[`plot_default()`](epiforecasts.io/forecast.vocs/reference/plot_default.md),
[`plot_growth()`](epiforecasts.io/forecast.vocs/reference/plot_growth.md),
[`plot_posterior()`](epiforecasts.io/forecast.vocs/reference/plot_posterior.md),
[`plot_rt()`](epiforecasts.io/forecast.vocs/reference/plot_rt.md),
[`plot_theme()`](epiforecasts.io/forecast.vocs/reference/plot_theme.md),
[`plot_voc_advantage()`](epiforecasts.io/forecast.vocs/reference/plot_voc_advantage.md),
[`plot_voc_frac()`](epiforecasts.io/forecast.vocs/reference/plot_voc_frac.md),
[`save_plots()`](epiforecasts.io/forecast.vocs/reference/save_plots.md)

Functions to explore and validate models
[`bp_launch_shinystan()`](epiforecasts.io/forecast.vocs/reference/bp_launch_shinystan.md),
[`fv_score_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_score_forecast.md)

## Examples

``` r
if (FALSE) { # interactive()
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = as.Date("2021-06-12"),
)
dt <- fv_as_data_list(obs)
inits <- fv_inits(dt)
fit <- fv_sample(dt, init = inits, adapt_voc = 0.99, max_treedepth = 15)
plot_pairs(fit)
}
```
