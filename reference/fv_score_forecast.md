# Evaluate forecasts using proper scoring rules

Acts as a wrapper to
[`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html).
In particular, handling filtering the output for various
[forecast.vocs](epiforecasts.io/forecast.vocs/reference/forecast.vocs-package.md)
functions and linking this output to observed data. See the
documentation for the `scoringutils` package for more on forecast
scoring and the documentation and examples below for simple examples in
the context of `forecast.vocs`. Internally name clashes between
`scoringutils` variables and `forecast.vocs` variables are handled.

## Usage

``` r
fv_score_forecast(forecast, obs, log = FALSE, check = TRUE, round_to = 3, ...)
```

## Arguments

- forecast:

  A posterior forecast or posterior prediction as returned by
  [`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md),
  [`summary.fv_forecast()`](epiforecasts.io/forecast.vocs/reference/summary.fv_forecast.md)
  or
  [`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md).
  Internally case forecasts are filtered for using the `value_type`
  variable if present as are only overall or combined case counts (i.e
  as returned) by the 1 and 2 strain models. If looking for more complex
  scoring it may be wise to implement a custom wrapper.

- obs:

  A data frame of observed data as produced by
  [`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md).

- log:

  Logical, defaults to FALSE. Should scores be calculated on the log
  scale (with a 0.01 shift) for both observations and forecasts. Scoring
  in this way can be thought of as a relative score vs the more usual
  absolute measure. It may be useful when targets are on very different
  scales or when the forecaster is more interested in good all round
  performance versus good performance for targets with large values.

- check:

  Logical, defaults to FALSE. Should `scoringutils::check_forecasts()`
  be used to check input forecasts.

- round_to:

  Integer defaults to 3. Number of digits to round scoring output to.

- ...:

  Additional arguments passed to
  [`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html).

## Value

A `data.table` as returned by
[`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html).

## See also

Functions to explore and validate models
[`bp_launch_shinystan()`](epiforecasts.io/forecast.vocs/reference/bp_launch_shinystan.md),
[`plot_pairs()`](epiforecasts.io/forecast.vocs/reference/plot_pairs.md)

## Examples

``` r
if (FALSE) { # interactive()
options(mc.cores = 4)
library(data.table)
library(scoringutils)

# Fit and forecast using both the one and two strain models
forecasts <- forecast(
  germany_covid19_delta_obs,
  forecast_date = as.Date("2021-06-12"),
  horizon = 4,
  strains = c(1, 2),
  adapt_delta = 0.99,
  max_treedepth = 15,
  variant_relationship = "scaled"
)

# Extract forecasts
forecasts <- summary(forecasts, target = "forecast", type = "cases")

# Filter for the latest available observations
obs <- latest_obs(germany_covid19_delta_obs)

# score on the absolute scale
scores <- fv_score_forecast(forecasts, obs)
summarise_scores(scores, by = "strains")

# score overall on a log scale
log_scores <- fv_score_forecast(forecasts, obs, log = TRUE)
summarise_scores(log_scores, by = "strains")

# score by horizon
summarise_scores(scores, by = c("strains", "horizon"))

# score by horizon on a log scale
summarise_scores(log_scores, by = c("strains", "horizon"))
}
```
