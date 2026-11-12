# Launch shinystan

Launch shinystan an interactive tool for stan model evaluation

## Usage

``` r
bp_launch_shinystan(fit)
```

## Arguments

- fit:

  List of output as returned by
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).

## See also

Functions to explore and validate models
[`fv_score_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_score_forecast.md),
[`plot_pairs()`](epiforecasts.io/forecast.vocs/reference/plot_pairs.md)

## Examples

``` r
if (FALSE) { # interactive()
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = as.Date("2021-06-12")
)

dt <- fv_as_data_list(obs)
inits <- fv_inits(dt)
fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
bp_launch_shinystan(fit)
}
```
